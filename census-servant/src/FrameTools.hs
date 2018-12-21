{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module FrameTools where

import           Control.Applicative     (liftA2)
import           Control.Arrow           (second)
import qualified Control.Foldl           as FL
import           Control.Lens            ((^.))
import qualified Control.Lens            as L
import qualified Data.Foldable           as Fold
import qualified Data.List               as L
import qualified Data.Map                as M
import           Data.Maybe              (fromJust, fromMaybe, isJust)
import           Data.Proxy              (Proxy (..))
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Vector             as V
import qualified Data.Vinyl              as V
import qualified Data.Vinyl.Class.Method as V
import qualified Data.Vinyl.Core         as V
import qualified Data.Vinyl.Curry        as V
import qualified Data.Vinyl.Functor      as V
import qualified Data.Vinyl.TypeLevel    as V
import qualified Data.Vinyl.XRec         as V
import qualified Dhall                   as D
import           Frames                  ((:.), (&:))
import qualified Frames                  as F
import qualified Frames.CSV              as F
import qualified Frames.InCore           as F
import qualified Frames.ShowCSV          as F
import qualified Pipes                   as P
import qualified Pipes.Prelude           as P

import           Servant
import           Servant.API.Generic
import           Servant.Client
import           Servant.Client.Generic

-- I want to decode a query, possibly multi-part, into an inCore frame.  That way we can edit before saving.
-- First step.  Decode each row into a Vinyl Record.

-- I guess this amounts to a json -> frame
