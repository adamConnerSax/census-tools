{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
module Census.API
  ( module Census.API
  , CF.ResultKey
  )
where

import qualified Census.ComputedFields         as CF
import qualified Census.Fields                 as CF

import           Servant
import           Servant.API.Generic
import           Servant.Client
import           Servant.Client.Generic

import           Control.Exception.Safe         ( throw )
import qualified Control.Foldl                 as FL
import           Control.Lens                   ( (^.)
                                                , (^..)
                                                , (^?)
                                                )
import qualified Control.Lens                  as L
import           Control.Monad                  ( forM_
                                                , join
                                                , mapM_
                                                , sequence
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import qualified Control.Monad.Except          as X
import qualified Data.Aeson                    as A
import qualified Data.Aeson.Types              as A
import qualified Data.Aeson.Lens               as L
import           Data.ByteString.Lazy           ( fromStrict )
import qualified Data.Foldable                 as F
import qualified Data.HashMap.Strict           as HM
import qualified Data.List                     as List
import qualified Data.List.Split               as LS
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe )
import           Data.Monoid                    ( (<>) )
import           Data.Proxy                     ( Proxy() )
import qualified Data.Set                      as S
import           Data.Text                      ( Text
                                                , intercalate
                                                , unpack
                                                , pack
                                                )
import qualified Text.Read                     as TR
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as Vec
import           Data.Vinyl                    as V
import           Data.Vinyl.Functor            as V
import           Data.Vinyl.TypeLevel          as V
import           Frames                        as F
import           Frames                         ( (:->)
                                                , (&:)
                                                )
import           Frames.CSV                    as F
import           Frames.InCore                 as FI
import qualified Pipes                         as P

stateFIPSAndNamesCSV = "conversion-data/states.csv"

-- state table is small so we can load it into memory.  No need to stream
getStateFIPSFrame :: IO (F.Frame CF.StateFIPSAndNames)
getStateFIPSFrame = F.inCoreAoS $ F.readTable stateFIPSAndNamesCSV

baseUrl = BaseUrl Https "api.census.gov" 443 "data"

data Census_Routes route = Census_Routes
  {
    _ACS :: route :- Capture "Year" CF.YearT :> "acs" :> Capture "span" Text :> QueryParam "get" Text :> QueryParam "for" Text :> QueryParam "in" Text :> QueryParam "key" Text :> Get '[JSON] A.Value
  , _SAIPE :: route :- "timeseries" :> "poverty" :> "saipe" :>  QueryParam "get" Text :> QueryParam "for" Text :> QueryParam "in" Text :> QueryParam "time" CF.YearT :> QueryParam "key" Text :> Get '[JSON] A.Value
  }
  deriving (Generic)

censusClients :: Census_Routes (AsClientT ClientM)
censusClients = genericClient

-- Census specific constants

type ApiKey = Text
censusApiKey :: ApiKey
censusApiKey = "2a9aeb4cae45db2d5e7ce1c9d0622caf68e8de15"


-- https://api.census.gov/data/2017/acs/acs5/variables
data ACS_Span = ACS1 | ACS3 | ACS5 deriving (Show,Enum,Eq,Ord,Bounded)
acsSpanToText :: ACS_Span -> Text
acsSpanToText ACS1 = "acs1"
acsSpanToText ACS3 = "acs3"
acsSpanToText ACS5 = "acs5"


getACSData :: CF.YearT -> ACS_Span -> CF.GeoCode a -> [Text] -> ClientM A.Value
getACSData year span geoCode codes =
  let (forM, inM) = CF.geoCodeToQuery geoCode
      getQ        = Just $ intercalate "," codes
  in  (_ACS censusClients) year
                           (acsSpanToText span)
                           getQ
                           forM
                           inM
                           (Just censusApiKey)


type ACSKey = "ACSKey" F.:-> Text
type ACSCount = "ACSCount" F.:-> Int

type ACSKeyedCount = '[ACSKey, ACSCount]

maxFieldsPerQuery = 50

getACSCountsLong
  :: forall f gs
   . ( ColumnHeaders gs
     , Ord (F.Record gs)
     , RecVec (gs ++ ACSKeyedCount)
     , Foldable f
     )
  => CF.RequestDictionary
  -> f CF.ResultKey
  -> CF.YearT
  -> ACS_Span
  -> CF.GeoCode gs
  -> Int
  -> X.ExceptT
       Text
       ClientM
       (F.FrameRec (gs ++ ACSKeyedCount))
getACSCountsLong rDict countKeys year span geoCode maxCodesPerQuery = do
  let gColHeaders = fmap pack $ F.columnHeaders (Proxy :: Proxy (F.Record gs))
      fColHeaders =
        fmap pack $ F.columnHeaders (Proxy :: Proxy (F.Record ACSKeyedCount))
      keySet       = S.fromList (gColHeaders ++ F.toList countKeys)
      dictWithGeos = CF.addGeoCodeRequests geoCode rDict -- geoCode cols need to be added
  requests <- either X.throwError return $ CF.getRequests dictWithGeos keySet
--  liftIO $ putStrLn $ show $ CF.printSortedRequests requests
  -- given the keys and an array of returned values 
  let
    compute :: Vec.Vector Text -> A.Array -> Either Text A.Object
    compute keys =
      CF.processRequests requests . HM.fromList . Vec.toList . Vec.zip keys
    queryChunks =
      LS.chunksOf maxCodesPerQuery $ S.toList $ CF.getFieldsToQuery requests
    queryToObject
      :: Monad m => A.Value -> X.ExceptT Text m (M.Map (F.Record gs) A.Object)
    queryToObject v = do
      let headersM :: Maybe (Vec.Vector Text)
          headersM =
            join $ traverse (traverse (^? L._String)) (v ^? L.nth 0 . L._Array)
          rowList :: [Vec.Vector A.Value]
          rowList = tail (v ^.. L.values . L._Array) -- tail here to get rid of header row
      headers <- maybe
        (throwError $ "problem extracting headers from " <> (pack $ show v))
        return
        headersM
      X.when (length rowList == 0) $ throwError "rowList returned no rows"
--    X.ExceptT m (Vec.Vector (F.Record gs, M.Map Text A.Value))
      rowMaps <- traverse
        (CF.geoDecode geoCode . HM.fromList . Vec.toList . Vec.zip headers)
        rowList
      return $ M.fromList rowMaps -- at this point, per query, we have only one line per geocode
    doOneQuery queryCodes =
      (X.lift (getACSData year span geoCode queryCodes) >>= queryToObject)
  liftIO $ putStrLn $ show $ queryChunks
  queryRes <- traverse doOneQuery queryChunks -- [Map (F.Record gs) A.Object]
  let queryResMap = M.unionsWith (<>) queryRes
  processedQueryRes <- traverse
    (CF.eitherToX id . CF.processRequests requests)
    queryResMap -- Map (F.Record gs) A.Object
  let
    toValueRecord
      :: Monad m => (Text, A.Value) -> X.ExceptT Text m (F.Record ACSKeyedCount)
    toValueRecord (key, aValue) = do
      let r2X :: Monad m => A.Result a -> X.ExceptT Text m a
          r2X r = case r of
            A.Success a -> return a
            A.Error   s -> throwError $ "AesonError: " <> (pack s)
      datAsString <- r2X (A.fromJSON aValue)
      val         <-
        maybe
            (  throwError
            $  "Failed to parse \""
            <> (pack datAsString)
            <> "\" as Int"
            )
            return
          $ TR.readMaybe datAsString
      return $ key F.&: val F.&: V.RNil
    objectToRequestedList
      :: (Foldable f, Monad m)
      => f CF.ResultKey
      -> A.Object
      -> X.ExceptT Text m [(Text, A.Value)]
    objectToRequestedList requestKeys o =
      traverse
          (\k ->
            maybe (throwError $ "Lookup failed for key=" <> k)
                  (\x -> return (k, x))
              $ HM.lookup k o
          )
        $ FL.fold FL.list requestKeys
  mapOfRequested <- traverse (objectToRequestedList countKeys) processedQueryRes
  mapOfFrames    <- traverse (traverse toValueRecord) mapOfRequested -- Map (F.Record gs) [F.Record ACSKeyedCount]
  return
    $ F.toFrame
    $ concat
    $ fmap (\(keyRec, dataRecs) -> fmap (keyRec `V.rappend`) dataRecs)
    $ M.toList mapOfFrames


mergeEithers (Right x) = x
mergeEithers (Left  y) = Left y

getACSDataFrame'
  :: forall fs gs
   . ( ColumnHeaders fs
     , ColumnHeaders gs
     , RecVec (gs ++ fs)
     , ReadRec (gs ++ fs)
     , RMap (gs ++ fs)
     , ReifyConstraint Show ElField (gs ++ fs)
     , RecordToList (gs ++ fs)
     )
  => CF.RequestDictionary
  -> CF.YearT
  -> ACS_Span
  -> CF.GeoCode gs
  -> X.ExceptT Text ClientM (F.FrameRec (gs V.++ fs))
getACSDataFrame' dict year span geoCode = do
  let gColHeaders  = fmap pack $ F.columnHeaders (Proxy :: Proxy (F.Record gs))
      fColHeaders  = fmap pack $ F.columnHeaders (Proxy :: Proxy (F.Record fs))
      keySet       = S.fromList (fColHeaders ++ gColHeaders)
      dictWithGeos = CF.addGeoCodeRequests geoCode dict -- geoCode cols need to be added
  requests <- either X.throwError return $ CF.getRequests dictWithGeos keySet
--  X.lift $ liftIO $ putStrLn $ unpack $ CF.printSortedRequests requests
  let computeAndReduce :: Vec.Vector Text -> A.Array -> Either Text A.Array
      computeAndReduce keys =
        mergeEithers
          . fmap (CF.objectToArray (gColHeaders ++ fColHeaders))
          . CF.processRequests requests
          . HM.fromList
          . Vec.toList
          . Vec.zip keys

  queried <- X.lift
    $ getACSData year span geoCode (S.toList $ CF.getFieldsToQuery requests)
  let headersM :: Maybe (Vec.Vector Text) = join
        $ traverse (traverse (^? L._String)) (queried ^? L.nth 0 . L._Array)
  headers <- maybe (throwError "No rows or no headers") return headersM
  --X.lift . liftIO $ putStrLn $ show queried
  let rowsValE =
        fmap (A.Array . Vec.fromList)
          $ traverse (fmap A.Array . computeAndReduce headers)
          $ tail (queried ^.. L.values . L._Array) -- tail here to get rid of header row
  rowsVal <- either throwError return rowsValE
  X.lift
    $     liftIO
    $     FI.inCoreAoS
    $     jsonArraysToRecordPipe (return rowsVal)
    P.>-> mapEither


type MyServantClientM = X.ExceptT Text ClientM
runX :: ClientEnv -> MyServantClientM a -> IO (Either Text a)
runX env a =
  let handleResult a = case a of
        Left  ce -> Left . pack $ show ce
        Right x  -> x
  in  fmap handleResult $ flip runClientM env $ X.runExceptT a


jsonTextArrayToList :: A.Value -> [Text]
jsonTextArrayToList x = x ^.. L.values . L._String

jsonArraysOfTextToPipe
  :: (MonadIO m, Monad m) => m A.Value -> P.Producer Text m ()
jsonArraysOfTextToPipe mv = do
  v <- P.lift mv
--  liftIO $ putStrLn $ show v
  let vs = v ^.. L.values
  P.yield "" -- pipeTableEither eats a row, presuming they are headers
  forM_ vs $ \v -> do
    let csv = intercalate "," $ jsonTextArrayToList v
--    liftIO $ putStr $ show csv ++ ": "
    P.yield csv
  return ()

jsonArraysToRecordPipe
  :: (F.ReadRec rs, Monad m, MonadIO m)
  => m A.Value
  -> P.Producer (F.Rec (Either Text :. F.ElField) rs) m ()
jsonArraysToRecordPipe mv = jsonArraysOfTextToPipe mv P.>-> F.pipeTableEither

mapEither
  :: ( Monad m
     , MonadIO m
     , RMap rs
     , V.ReifyConstraint Show V.ElField rs
     , V.RecordToList rs
     )
  => P.Pipe (F.Rec (Either Text :. F.ElField) rs) (F.Record rs) m ()
mapEither = do
  eRec <- P.await
--  liftIO $ eitherRecordPrint eRec
  case (F.rtraverse V.getCompose eRec) of
    Left  _ -> mapEither -- skip it
    Right r -> P.yield r >> mapEither

-- for debugging
eitherRecordPrint
  :: (V.RMap rs, V.ReifyConstraint Show V.ElField rs, V.RecordToList rs)
  => F.Rec (Either Text :. F.ElField) rs
  -> IO ()
eitherRecordPrint eRec = case (F.rtraverse V.getCompose eRec) of
  Left  err -> liftIO $ putStrLn $ "Error: " ++ (unpack err)
  Right r   -> liftIO $ print r



