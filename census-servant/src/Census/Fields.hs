{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE Rank2Types                #-}
module Census.Fields where

import qualified Census.ComputedFields         as CF

import qualified Data.Aeson                    as A
import qualified Data.Foldable                 as Fold
import           Data.List                      ( nub
                                                , concat
                                                ) -- I know this is inefficient.  Short lists.
import           Data.Proxy                     ( Proxy(..) )
import qualified Data.Map                      as M
import qualified Data.HashMap.Lazy             as HML
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

import qualified Data.Set                      as S
import qualified Data.Vinyl                    as V
import qualified Data.Vinyl.Functor            as V
import qualified Data.Vinyl.TypeLevel          as V
import qualified Frames                        as F
import qualified Frames                         ( (:->)
                                                , (&:)
                                                )

import           Data.Kind                      ( Type )
import           GHC.TypeLits                   ( Symbol
                                                , KnownSymbol(..)
                                                )

-- state FIPS datatypes
F.tableTypes "StateFIPSAndNames" "../conversion-data/states.csv"  -- declares StateName, StateFIPS, StateAbbreviation

F.declareColumn "CountyFIPS" ''Int -- = "countyFIPS" :-> Int
F.declareColumn "CongressionalDistrict" ''Int -- = "countyFIPS" :-> Int

type Year = Int
F.declareColumn "YearF" ''Year

F.declareColumn "Population" ''Int
F.declareColumn "YoungMale" ''Int
F.declareColumn "YoungWhiteMalePct" ''Double
F.declareColumn "OldWhiteMalePct" ''Double
F.declareColumn "YoungWhiteFemalePct" ''Double
F.declareColumn "OldWhiteFemalePct" ''Double
F.declareColumn "YoungNonWhiteMalePct" ''Double
F.declareColumn "OldNonWhiteMalePct" ''Double
F.declareColumn "YoungNonWhiteFemalePct" ''Double
F.declareColumn "OldNonWhiteFemalePct" ''Double


F.declareColumn "YoungMalePctPop" ''Double
F.declareColumn "YoungMalePctTot" ''Double



data GeoCode a where
  AllStatesAndCounties :: GeoCode '[StateFIPS, CountyFIPS]
  AllStatesAndDistricts :: GeoCode '[StateFIPS, CongressionalDistrict]

geoCodeToQuery :: GeoCode a -> (Maybe Text, Maybe Text)
--geoCodeToQuery (GeoCodeRawFor forText) = (Just forText, Nothing)
--geoCodeToQuery (GeoCodeRawForIn forText inText) = (Just forText, Just inText)
geoCodeToQuery AllStatesAndCounties = (Just "county:*", Just "state:*")
geoCodeToQuery AllStatesAndDistricts =
  (Just "congressional district:*", Just "state:*")

addGeoCodeRequests :: GeoCode a -> CF.RequestDictionary -> CF.RequestDictionary
addGeoCodeRequests x (CF.RequestDictionary dict) =
  CF.RequestDictionary $ M.union dict $ case x of
    AllStatesAndCounties -> M.fromList
      [CF.inQuery "county" "CountyFIPS", CF.inQuery "state" "StateFIPS"]
    AllStatesAndDistricts -> M.fromList
      [ CF.inQuery "congressional district" "CongressionalDistrict"
      , CF.inQuery "state" "StateFIPS"
      ]

acsRequestDictionary = CF.RequestDictionary $ M.fromList $ fmap
  (\r@(CF.Request k _ _) -> (k, r))
  (acsQueryRequests ++ acsComputedRequests)


acsQueryRequests :: [CF.Request]
acsQueryRequests =
  concat $ fmap (\(censusV, key) -> CF.query censusV key) codes
{-    $  [ ("B01003_001E" , "Population")
       , ("B01001_001E" , "Total")
       , ("B01001_002E" , "TotalMale")
       , ("B01001A_001E", "TotalWhite")
       , ("B01001A_002E", "TotalWhiteMale")
       , ("B01001B_001E", "TotalBlack")
       , ("B01001B_002E", "TotalBlackMale")
       ]
    ++ youngMaleCodes
-}

acsComputedRequests :: [CF.Request]
acsComputedRequests =
  [ youngMaleR
  , oldMaleR
  , youngMalePctPopR
  , youngMalePctTotR
  , youngWhiteMaleR
  , youngWhiteMalePctR
  , oldWhiteMaleR
  , oldWhiteMalePctR
  , youngFemaleR
  , oldFemaleR
  , oldFemaleR
  , youngWhiteFemaleR
  , youngWhiteFemalePctR
  , oldWhiteFemaleR
  , oldWhiteFemalePctR
  , youngNonWhiteMaleR
  , youngNonWhiteMalePctR
  , oldNonWhiteMaleR
  , oldNonWhiteMalePctR
  , youngNonWhiteFemaleR
  , youngNonWhiteFemalePctR
  , oldNonWhiteFemaleR
  , oldNonWhiteFemalePctR
  ]

youngMaleCodes :: [(Text, Text)]
youngMaleCodes =
  [ ("B01001_003E", "MaleUnder5")
  , ("B01001_004E", "Male5To9")
  , ("B01001_005E", "Male10To14")
  , ("B01001_006E", "Male15To17")
  , ("B01001_007E", "Male18To19")
  , ("B01001_008E", "Male20")
  , ("B01001_009E", "Male21")
  , ("B01001_010E", "Male22To24")
  , ("B01001_011E", "Male25To29")
  , ("B01001_012E", "Male30To34")
  , ("B01001_013E", "Male35To39")
  , ("B01001_014E", "Male40To44")
  ]

youngFemaleCodes :: [(Text, Text)]
youngFemaleCodes =
  [ ("B01001_027E", "FemaleUnder5")
  , ("B01001_028E", "Female5To9")
  , ("B01001_029E", "Female10To14")
  , ("B01001_030E", "Female15To17")
  , ("B01001_031E", "Female18To19")
  , ("B01001_032E", "Female20")
  , ("B01001_033E", "Female21")
  , ("B01001_034E", "Female22To24")
  , ("B01001_035E", "Female25To29")
  , ("B01001_036E", "Female30To34")
  , ("B01001_037E", "Female35To39")
  , ("B01001_038E", "Female40To44")
  ]

youngWhiteMaleCodes :: [(Text, Text)]
youngWhiteMaleCodes =
  [ ("B01001A_003E", "WhiteMaleUnder5")
  , ("B01001A_004E", "WhiteMale5To9")
  , ("B01001A_005E", "WhiteMale10To14")
  , ("B01001A_006E", "WhiteMale15To17")
  , ("B01001A_007E", "WhiteMale18To19")
  , ("B01001A_008E", "WhiteMale20To24")
  , ("B01001A_009E", "WhiteMale25To29")
  , ("B01001A_010E", "WhiteMale30To34")
  , ("B01001A_011E", "WhiteMale35To44")
  ]

youngWhiteFemaleCodes :: [(Text, Text)]
youngWhiteFemaleCodes =
  [ ("B01001A_018E", "WhiteFemaleUnder5")
  , ("B01001A_019E", "WhiteFemale5To9")
  , ("B01001A_020E", "WhiteFemale10To14")
  , ("B01001A_021E", "WhiteFemale15To17")
  , ("B01001A_022E", "WhiteFemale18To19")
  , ("B01001A_023E", "WhiteFemale20To24")
  , ("B01001A_024E", "WhiteFemale25To29")
  , ("B01001A_025E", "WhiteFemale30To34")
  , ("B01001A_026E", "WhiteFemale35To44")
  ]

youngBlackMaleCodes :: [(Text, Text)]
youngBlackMaleCodes =
  [ ("B01001B_003E", "BlackMaleUnder5")
  , ("B01001B_004E", "BlackMale5To9")
  , ("B01001B_005E", "BlackMale10To14")
  , ("B01001B_006E", "BlackMale15To17")
  , ("B01001B_007E", "BlackMale18To19")
  , ("B01001B_008E", "BlackMale20To24")
  , ("B01001B_009E", "BlackMale25To29")
  , ("B01001B_010E", "BlackMale30To34")
  , ("B01001B_011E", "BlackMale35To44")
  ]

youngBlackFemaleCodes :: [(Text, Text)]
youngBlackFemaleCodes =
  [ ("B01001B_018E", "BlackFemaleUnder5")
  , ("B01001B_019E", "BlackFemale5To9")
  , ("B01001B_020E", "BlackFemale10To14")
  , ("B01001B_021E", "BlackFemale15To17")
  , ("B01001B_022E", "BlackFemale18To19")
  , ("B01001B_023E", "BlackFemale20To24")
  , ("B01001B_024E", "BlackFemale25To29")
  , ("B01001B_025E", "BlackFemale30To34")
  , ("B01001B_026E", "BlackFemale35To44")
  ]


codes :: [(Text, Text)]
codes =
  [ ("B01003_001E", "Population")
    , ("B01001_001E", "Total")
    , ("B01001_002E", "TotalMale")
    ]
    ++ youngMaleCodes
    ++ [("B01001_026E", "TotalFemale")]
    ++ youngFemaleCodes
    ++ [("B01001A_001E", "TotalWhite"), ("B01001A_002E", "TotalWhiteMale")]
    ++ youngWhiteMaleCodes
    ++ [("B01001A_017E", "TotalWhiteFemale")]
    ++ youngWhiteFemaleCodes
    ++ [("B01001B_001E", "TotalBlack"), ("B01001B_002E", "TotalBlackMale")]
    ++ youngBlackMaleCodes
    ++ [("B01001B_017E", "TotalBlackFemale")]
    ++ youngBlackFemaleCodes

youngMaleKeys :: S.Set Text
youngMaleKeys = S.fromList $ fmap snd youngMaleCodes

youngWhiteMaleKeys :: S.Set Text
youngWhiteMaleKeys = S.fromList $ fmap snd youngWhiteMaleCodes

youngFemaleKeys :: S.Set Text
youngFemaleKeys = S.fromList $ fmap snd youngFemaleCodes

youngWhiteFemaleKeys :: S.Set Text
youngWhiteFemaleKeys = S.fromList $ fmap snd youngWhiteFemaleCodes

youngMaleR =
  CF.Request "YoungMale" youngMaleKeys (CF.Compute $ CF.addAll youngMaleKeys)

oldMaleR = CF.Request "OldMale"
                      (S.fromList ["TotalMale", "YoungMale"])
                      (CF.Compute $ CF.difference "TotalMale" "YoungMale")

youngWhiteMaleR = CF.Request "YoungWhiteMale"
                             youngWhiteMaleKeys
                             (CF.Compute $ CF.addAll youngWhiteMaleKeys)
youngWhiteMalePctR = CF.Request
  "YoungWhiteMalePct"
  (S.fromList ["YoungWhiteMale", "Total"])
  (CF.Compute $ CF.ratio "YoungWhiteMale" "Total")

oldWhiteMaleR = CF.Request
  "OldWhiteMale"
  (S.fromList ["TotalWhiteMale", "YoungWhiteMale"])
  (CF.Compute $ CF.difference "TotalWhiteMale" "YoungWhiteMale")

oldWhiteMalePctR = CF.Request "OldWhiteMalePct"
                              (S.fromList ["OldWhiteMale", "Total"])
                              (CF.Compute $ CF.ratio "OldWhiteMale" "Total")


youngNonWhiteMaleR = CF.Request
  "YoungNonWhiteMale"
  (S.fromList ["YoungMale", "YoungWhiteMale"])
  (CF.Compute $ CF.difference "YoungMale" "YoungWhiteMale")
youngNonWhiteMalePctR = CF.Request
  "YoungNonWhiteMalePct"
  (S.fromList ["YoungNonWhiteMale", "Total"])
  (CF.Compute $ CF.ratio "YoungNonWhiteMale" "Total")

oldNonWhiteMaleR = CF.Request
  "OldNonWhiteMale"
  (S.fromList ["OldMale", "OldWhiteMale"])
  (CF.Compute $ CF.difference "OldMale" "OldWhiteMale")
oldNonWhiteMalePctR = CF.Request
  "OldNonWhiteMalePct"
  (S.fromList ["OldNonWhiteMale", "Total"])
  (CF.Compute $ CF.ratio "OldNonWhiteMale" "Total")


youngFemaleR = CF.Request "YoungFemale"
                          youngFemaleKeys
                          (CF.Compute $ CF.addAll youngFemaleKeys)

oldFemaleR = CF.Request
  "OldFemale"
  (S.fromList ["TotalFemale", "YoungFemale"])
  (CF.Compute $ CF.difference "TotalFemale" "YoungFemale")

youngWhiteFemaleR = CF.Request "YoungWhiteFemale"
                               youngWhiteFemaleKeys
                               (CF.Compute $ CF.addAll youngWhiteFemaleKeys)

youngWhiteFemalePctR = CF.Request
  "YoungWhiteFemalePct"
  (S.fromList ["YoungWhiteFemale", "Total"])
  (CF.Compute $ CF.ratio "YoungWhiteFemale" "Total")

oldWhiteFemaleR = CF.Request
  "OldWhiteFemale"
  (S.fromList ["TotalWhiteFemale", "YoungWhiteFemale"])
  (CF.Compute $ CF.difference "TotalWhiteFemale" "YoungWhiteFemale")

oldWhiteFemalePctR = CF.Request
  "OldWhiteFemalePct"
  (S.fromList ["OldWhiteFemale", "Total"])
  (CF.Compute $ CF.ratio "OldWhiteFemale" "Total")


youngNonWhiteFemaleR = CF.Request
  "YoungNonWhiteFemale"
  (S.fromList ["YoungFemale", "YoungWhiteFemale"])
  (CF.Compute $ CF.difference "YoungFemale" "YoungWhiteFemale")
youngNonWhiteFemalePctR = CF.Request
  "YoungNonWhiteFemalePct"
  (S.fromList ["YoungNonWhiteFemale", "Total"])
  (CF.Compute $ CF.ratio "YoungNonWhiteFemale" "Total")

oldNonWhiteFemaleR = CF.Request
  "OldNonWhiteFemale"
  (S.fromList ["OldFemale", "OldWhiteFemale"])
  (CF.Compute $ CF.difference "OldFemale" "OldWhiteFemale")
oldNonWhiteFemalePctR = CF.Request
  "OldNonWhiteFemalePct"
  (S.fromList ["OldNonWhiteFemale", "Total"])
  (CF.Compute $ CF.ratio "OldNonWhiteFemale" "Total")


youngMalePctPopR = CF.Request
  "YoungMalePctPop"
  (S.fromList ["YoungMale", "Population"])
  (CF.Compute $ CF.ratio "YoungMale" "Population")

youngMalePctTotR = CF.Request "YoungMalePctTot"
                              (S.fromList ["YoungMale", "Total"])
                              (CF.Compute $ CF.ratio "YoungMale" "Total")





{-
type B01003_001E = "B01003_001E" F.:-> Int
instance ComputedField ACS Population where
  type FieldNeeds ACS Population = '[B01003_001E]
  makeField = F.rgetField @B01003_001E

F.declareColumn "MedianHouseholdIncome" ''Double
type B19013_001E = "B19013_001E" F.:-> Int
instance ComputedField ACS MedianHouseholdIncome where
  type FieldNeeds ACS MedianHouseholdIncome = '[B19013_001E]
  makeField = realToFrac . F.rgetField @B19013_001E

type SAEMHI_PT = "SAEMHI_PT" F.:-> Double
instance ComputedField SAIPE MedianHouseholdIncome where
  type FieldNeeds SAIPE MedianHouseholdIncome = '[SAEMHI_PT]
  makeField = F.rgetField @SAEMHI_PT

F.declareColumn "MedianAge" ''Double
type B01002_001E = "B01002_001E" F.:-> Double
instance ComputedField ACS MedianAge where
  type FieldNeeds ACS MedianAge = '[B01002_001E]
  makeField = F.rgetField @B01002_001E


F.declareColumn "HSGradPct" ''Double
type B06009_001E = "B06009_001E" F.:-> Int
type B06009_003E = "B06009_003E" F.:-> Int
instance ComputedField ACS HSGradPct where
  type FieldNeeds ACS HSGradPct = '[B06009_001E,B06009_003E]
  makeField r =
    let tot = realToFrac $ F.rgetField @B06009_001E r
        hsGrads = realToFrac $ F.rgetField @B06009_003E r
    in hsGrads/tot

F.declareColumn "NonHSGradPct" ''Double
type B06009_002E = "B06009_002E" F.:-> Int
instance ComputedField ACS NonHSGradPct where
  type FieldNeeds ACS NonHSGradPct = '[B06009_001E,B06009_002E]
  makeField r =
    let tot = realToFrac $ F.rgetField @B06009_001E r
        nonHSGrads = realToFrac $ F.rgetField @B06009_002E r
    in nonHSGrads/tot

F.declareColumn "CollegeGradPct" ''Double
type B06009_005E = "B06009_005E" F.:-> Int
instance ComputedField ACS CollegeGradPct where
  type FieldNeeds ACS CollegeGradPct = '[B06009_001E,B06009_005E]
  makeField r =
    let tot = realToFrac $ F.rgetField @B06009_001E r
        collegeGrads = realToFrac $ F.rgetField @B06009_005E r
    in collegeGrads/tot

F.declareColumn "GradSchoolPct" ''Double
type B06009_006E = "B06009_006E" F.:-> Int
instance ComputedField ACS GradSchoolPct where
  type FieldNeeds ACS GradSchoolPct = '[B06009_001E,B06009_006E]
  makeField r =
    let tot = realToFrac $ F.rgetField @B06009_001E r
        gradSchool = realToFrac $ F.rgetField @B06009_006E r
    in gradSchool/tot

F.declareColumn "MedianHouseholdIncomeMOE" ''Int
type SAEMHI_MOE = "SAEMHI_MOE" F.:-> Int
instance ComputedField SAIPE MedianHouseholdIncomeMOE where
  type FieldNeeds SAIPE MedianHouseholdIncomeMOE = '[SAEMHI_MOE]
  makeField = F.rgetField @SAEMHI_MOE

F.declareColumn "PovertyRate" ''Double -- PovertyR = "povertyR" :-> Double
type B17001_001E = "B17001_001E" F.:-> Int
type B17001_002E = "B17001_002E" F.:-> Int
instance ComputedField ACS PovertyRate where
  type FieldNeeds ACS PovertyRate = '[B17001_001E,B17001_002E]
  makeField r =
    let tot = realToFrac $ F.rgetField @B17001_001E r
        below = realToFrac $ F.rgetField @B17001_002E r
    in below/tot

type SAEPOVRTALL_PT = "SAEPOVRTALL_PT" F.:-> Double
instance ComputedField SAIPE PovertyRate where
  type FieldNeeds SAIPE PovertyRate = '[SAEPOVRTALL_PT]
  makeField = F.rgetField @SAEPOVRTALL_PT

F.declareColumn "AverageHouseholdSize" ''Double
type B25010_001E = "B25010_001E" F.:-> Double
instance ComputedField ACS AverageHouseholdSize where
  type FieldNeeds ACS AverageHouseholdSize = '[B25010_001E] -- this is avg household size *of occuped housing units* which is not quite what we want
  makeField = F.rgetField @B25010_001E


--
{-
The next bunch are for a trial demographic election result model.
Race x Sex x Age
Race is one of W(hite), B(lack), L(atino), A(sian), O(ther)
Sex is one of M(ale), F(emale)
Age is one of Y(oung) (<45) or O(old) (>= 45)

I am aware that these categories are exclusive and othering.  That sucks.  I've got two problems.  I need to shrink the number of categories to
do anything meaningful and the census only tracks so many things. Still.
-}

type B01001_001E = "B01001_001E" F.:-> Int -- total unweighted count, our denominator

type AllCount = "AllCount" F.:-> Int
instance ComputedField ACS AllCount where
  type FieldNeeds ACS AllCount = '[B01001_001E]
  makeField = F.rgetField @B01001_001E


type B01001_002E = "B01001_002E" F.:-> Int -- total male
type B01001_003E = "B01001_003E" F.:-> Int -- M < 5
type B01001_004E = "B01001_004E" F.:-> Int -- M 5-9
type B01001_005E = "B01001_005E" F.:-> Int -- M 10-14
type B01001_006E = "B01001_006E" F.:-> Int -- M 15-17
type B01001_007E = "B01001_007E" F.:-> Int -- M 18-19
type B01001_008E = "B01001_008E" F.:-> Int -- M 20
type B01001_009E = "B01001_009E" F.:-> Int -- M 21
type B01001_010E = "B01001_010E" F.:-> Int -- M 22-24
type B01001_011E = "B01001_011E" F.:-> Int -- M 25-29
type B01001_012E = "B01001_012E" F.:-> Int -- M 30-34
type B01001_013E = "B01001_013E" F.:-> Int -- M 35-39
type B01001_014E = "B01001_014E" F.:-> Int -- M 40-44
type MYCodes = [B01001_003E, B01001_004E, B01001_005E, B01001_006E, B01001_007E, B01001_008E, B01001_009E, B01001_010E, B01001_011E, B01001_012E, B01001_013E, B01001_014E]

type B01001A_001E = "B01001A_001E" F.:-> Int -- W
type B01001A_002E = "B01001A_002E" F.:-> Int -- WM
F.declareColumn "WMY" '' Double
F.declareColumn "WMO" '' Double
F.declareColumn "NWMY" '' Double
F.declareColumn "NWMO" '' Double
type B01001A_003E = "B01001A_003E" F.:-> Int -- WM < 5
type B01001A_004E = "B01001A_004E" F.:-> Int -- WM 5-9
type B01001A_005E = "B01001A_005E" F.:-> Int -- WM 10-14
type B01001A_006E = "B01001A_006E" F.:-> Int -- WM 15-17
type B01001A_007E = "B01001A_007E" F.:-> Int -- WM 18-19
type B01001A_008E = "B01001A_008E" F.:-> Int -- WM 20-24
type B01001A_009E = "B01001A_009E" F.:-> Int -- WM 25-29
type B01001A_010E = "B01001A_010E" F.:-> Int -- WM 30-34
type B01001A_011E = "B01001A_011E" F.:-> Int -- WM 35-44
type WMYCodes = [B01001A_003E,B01001A_004E,B01001A_005E,B01001A_006E,B01001A_007E,B01001A_008E,B01001A_009E,B01001A_010E,B01001A_011E]

instance ComputedField ACS WMY where
  type FieldNeeds ACS WMY = (B01001_001E ': WMYCodes)
  makeField r =
    let tot = F.rgetField @B01001_001E r
        wmy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @WMYCodes r)
    in realToFrac wmy/realToFrac tot

instance ComputedField ACS WMO where
  type FieldNeeds ACS WMO =  (B01001_001E ': B01001A_002E ': WMYCodes)
  makeField r =
    let tot = F.rgetField @B01001_001E r
        wm = F.rgetField @B01001A_002E r
        wmy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @WMYCodes r)
    in realToFrac (wm - wmy)/realToFrac tot

instance ComputedField ACS NWMY where
  type FieldNeeds ACS NWMY = (B01001_001E ': (WMYCodes V.++ MYCodes))
  makeField r =
    let tot = F.rgetField @B01001_001E r
        my = Fold.foldl' (+) 0 (F.recToList $ F.rcast @MYCodes r)
        wmy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @WMYCodes r)
    in realToFrac (my - wmy)/realToFrac tot

instance ComputedField ACS NWMO where
  type FieldNeeds ACS NWMO =  (B01001_001E ': B01001_002E ': B01001A_002E ': (WMYCodes V.++ MYCodes))
  makeField r =
    let tot = F.rgetField @B01001_001E r
        m = F.rgetField @B01001_002E r
        wm = F.rgetField @B01001A_002E r
        my = Fold.foldl' (+) 0 (F.recToList $ F.rcast @MYCodes r)
        wmy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @WMYCodes r)
        mo = m - my
        wmo = wm - wmy
    in realToFrac (mo - wmo)/realToFrac tot

type B01001_026E = "B01001_026E" F.:-> Int -- total female
type B01001_027E = "B01001_027E" F.:-> Int -- F < 5
type B01001_028E = "B01001_028E" F.:-> Int -- F 5-9
type B01001_029E = "B01001_029E" F.:-> Int -- F 10-14
type B01001_030E = "B01001_030E" F.:-> Int -- F 15-17
type B01001_031E = "B01001_031E" F.:-> Int -- F 18-19
type B01001_032E = "B01001_032E" F.:-> Int -- F 20
type B01001_033E = "B01001_033E" F.:-> Int -- F 21
type B01001_034E = "B01001_034E" F.:-> Int -- F 22-24
type B01001_035E = "B01001_035E" F.:-> Int -- F 25-29
type B01001_036E = "B01001_036E" F.:-> Int -- F 30-34
type B01001_037E = "B01001_037E" F.:-> Int -- F 35-39
type B01001_038E = "B01001_038E" F.:-> Int -- F 40-44

type FYCodes =[B01001_027E, B01001_028E, B01001_029E, B01001_030E, B01001_031E, B01001_032E, B01001_033E, B01001_034E, B01001_035E, B01001_036E, B01001_037E, B01001_038E]


type B01001A_017E = "B01001A_017E" F.:-> Int -- WF
F.declareColumn "WFY" '' Double
F.declareColumn "WFO" '' Double
F.declareColumn "NWFY" '' Double
F.declareColumn "NWFO" '' Double
type B01001A_018E = "B01001A_018E" F.:-> Int -- WF < 5
type B01001A_019E = "B01001A_019E" F.:-> Int -- WF 5-9
type B01001A_020E = "B01001A_020E" F.:-> Int -- WF 10-14
type B01001A_021E = "B01001A_021E" F.:-> Int -- WF 15-17
type B01001A_022E = "B01001A_022E" F.:-> Int -- WF 18-19
type B01001A_023E = "B01001A_023E" F.:-> Int -- WF 20-24
type B01001A_024E = "B01001A_024E" F.:-> Int -- WF 25-29
type B01001A_025E = "B01001A_025E" F.:-> Int -- WF 30-34
type B01001A_026E = "B01001A_026E" F.:-> Int -- WF 35-44
type WFYCodes = [B01001A_018E,B01001A_019E,B01001A_020E,B01001A_021E,B01001A_022E,B01001A_023E,B01001A_024E,B01001A_025E,B01001A_026E]

instance ComputedField ACS WFY where
  type FieldNeeds ACS WFY = (B01001_001E ': WFYCodes)
  makeField r =
    let tot = F.rgetField @B01001_001E r
        wfy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @WFYCodes r)
    in realToFrac wfy/realToFrac tot

instance ComputedField ACS WFO where
  type FieldNeeds ACS WFO =  (B01001_001E ': B01001A_017E ': WFYCodes)
  makeField r =
    let tot = F.rgetField @B01001_001E r
        wf = F.rgetField @B01001A_017E r
        wfy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @WFYCodes r)
    in realToFrac (wf - wfy)/realToFrac tot


instance ComputedField ACS NWFY where
  type FieldNeeds ACS NWFY = (B01001_001E ': (WFYCodes V.++ FYCodes))
  makeField r =
    let tot = F.rgetField @B01001_001E r
        fy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @FYCodes r)
        wfy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @WFYCodes r)
    in realToFrac (fy - wfy)/realToFrac tot

instance ComputedField ACS NWFO where
  type FieldNeeds ACS NWFO =  (B01001_001E ': B01001_026E ': B01001A_017E ': (WFYCodes V.++ FYCodes))
  makeField r =
    let tot = F.rgetField @B01001_001E r
        f = F.rgetField @B01001_026E r
        wf = F.rgetField @B01001A_017E r
        fy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @FYCodes r)
        wfy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @WFYCodes r)
        fo = f - fy
        wfo = wf - wfy
    in realToFrac (fo - wfo)/realToFrac tot


type B01001B_002E = "B01001B_002E" F.:-> Int -- BM
F.declareColumn "BMY" '' Double
F.declareColumn "BMO" '' Double
type B01001B_003E = "B01001B_003E" F.:-> Int -- BM < 5
type B01001B_004E = "B01001B_004E" F.:-> Int -- BM 5-9
type B01001B_005E = "B01001B_005E" F.:-> Int -- BM 10-14
type B01001B_006E = "B01001B_006E" F.:-> Int -- BM 15-17
type B01001B_007E = "B01001B_007E" F.:-> Int -- BM 18-19
type B01001B_008E = "B01001B_008E" F.:-> Int -- BM 20-24
type B01001B_009E = "B01001B_009E" F.:-> Int -- BM 25-29
type B01001B_010E = "B01001B_010E" F.:-> Int -- BM 30-34
type B01001B_011E = "B01001B_011E" F.:-> Int -- BM 35-44

type BMYCodes = [B01001B_003E,B01001B_004E,B01001B_005E,B01001B_006E,B01001B_007E,B01001B_008E,B01001B_009E,B01001B_010E,B01001B_011E]
instance ComputedField ACS BMY where
  type FieldNeeds ACS BMY = (B01001_001E ': BMYCodes)
  makeField r =
    let tot = F.rgetField @B01001_001E r
        bmy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @BMYCodes r)
    in realToFrac bmy/realToFrac tot

instance ComputedField ACS BMO where
  type FieldNeeds ACS BMO =  (B01001_001E ': B01001B_002E ': BMYCodes)
  makeField r =
    let tot = F.rgetField @B01001_001E r
        bm = F.rgetField @B01001B_002E r
        bmy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @BMYCodes r)
    in realToFrac (bm - bmy)/realToFrac tot

type B01001B_017E = "B01001B_017E" F.:-> Int -- BF
F.declareColumn "BFY" '' Double
F.declareColumn "BFO" '' Double
type B01001B_018E = "B01001B_018E" F.:-> Int -- BF < 5
type B01001B_019E = "B01001B_019E" F.:-> Int -- BF 5-9
type B01001B_020E = "B01001B_020E" F.:-> Int -- BF 10-14
type B01001B_021E = "B01001B_021E" F.:-> Int -- BF 15-17
type B01001B_022E = "B01001B_022E" F.:-> Int -- BF 18-19
type B01001B_023E = "B01001B_023E" F.:-> Int -- BF 20-24
type B01001B_024E = "B01001B_024E" F.:-> Int -- BF 25-29
type B01001B_025E = "B01001B_025E" F.:-> Int -- BF 30-34
type B01001B_026E = "B01001B_026E" F.:-> Int -- BF 35-44
type BFYCodes = [B01001B_018E,B01001B_019E,B01001B_020E,B01001B_021E,B01001B_022E,B01001B_023E,B01001B_024E,B01001B_025E,B01001B_026E]

instance ComputedField ACS BFY where
  type FieldNeeds ACS BFY = (B01001_001E ': BFYCodes)
  makeField r =
    let tot = F.rgetField @B01001_001E r
        bfy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @BFYCodes r)
    in realToFrac bfy/realToFrac tot

instance ComputedField ACS BFO where
  type FieldNeeds ACS BFO =  (B01001_001E ': B01001B_017E ': BFYCodes)
  makeField r =
    let tot = F.rgetField @B01001_001E r
        bf = F.rgetField @B01001B_017E r
        bfy = Fold.foldl' (+) 0 (F.recToList $ F.rcast @BFYCodes r)
    in realToFrac (bf - bfy)/realToFrac tot





---
data SAIPEDataCode = MedianHouseholdIncome |
                     MedianHouseholdIncomeMOE |
                     Poverty0to17Rate |
                     Poverty0to17Count |
                     Poverty0to17MOE |
                     Poverty0to4Rate |
                     Poverty0to4Count |
                     Poverty0to4MOE |
                     PovertyRate |
                     PovertyCount |
                     PovertyMOE

saipeDataCodeToText MedianHouseholdIncome    = "SAEMHI_PT"
saipeDataCodeToText MedianHouseholdIncomeMOE = "SAEMHI_MOE"
saipeDataCodeToText Poverty0to17Rate         = "SAEPOVRTO_17_PT"
saipeDataCodeToText Poverty0to17Count        = "SAEPOV0_17_PT"
saipeDataCodeToText Poverty0to17MOE          = "SAEPOV0_17_MOE"
saipeDataCodeToText Poverty0to4Rate          = "SAEPOVRT0_4_PT"
saipeDataCodeToText Poverty0to4Count         = "SAEPOVT0_4_PT"
saipeDataCodeToText Poverty0to4MOE           = "SAEPOV0_4_MOE"
saipeDataCodeToText PovertyRate              = "SAEPOVRTALL_PT"
saipeDataCodeToText PovertyCount             = "SAEPOVALL_PT"
saipeDataCodeToText PovertyMOE               = "SAEPOVTALL_MOE"



-}
