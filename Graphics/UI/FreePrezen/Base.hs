{-# LANGUAGE DeriveFunctor, TypeSynonymInstances, FlexibleInstances #-}

module Graphics.UI.FreePrezen.Base (
  PageContents,
  PageContent (..),
  PageConfig (..),
  move,
  image,
  text,
  box,
  wait,
  
  Prezen,
  PrezenF (..),
  PrezenConfig (..),
  page
  ) where

import Control.Monad.Free
import Control.Monad.Trans
import Data.Default
import qualified Graphics.UI.FreeGame as FG

-- Pages

type PageContents = Free PageContent

data PageContent a
  = Move (FG.V2 Float) (PageContents ()) a
  | Image FG.Bitmap a
  | Text Float FG.Color String a
  | Box (FG.V2 Float) (FG.Color, FG.Color) a
  | Wait a deriving (Functor)

data PageConfig = PageConfig {
  pageBackground :: Either (FG.Color, FG.Color) FG.Bitmap,
  pageFont :: FG.Font }

move :: FG.V2 Float -> PageContents () -> PageContents ()
move cood content = liftF $ Move cood content ()

image :: FG.Bitmap -> PageContents ()
image bitmap = liftF $ Image bitmap ()

text :: Float -> FG.Color -> String -> PageContents ()
text size color txt = liftF $ Text size color txt ()

box :: FG.V2 Float -> (FG.Color, FG.Color) -> PageContents ()
box bx color = liftF $ Box bx color ()

wait :: PageContents ()
wait = liftF $ Wait ()

-- Presentations

type Prezen = Free PrezenF

data PrezenF a
  = Page PageConfig (PageContents a) a
  | LiftIO (IO a) deriving (Functor)

data PrezenConfig = PrezenConfig {
  prezenSize :: FG.V2 Float,
  prezenIsFullscreen :: Bool }

instance Default PrezenConfig where
  def = PrezenConfig {
    prezenSize = FG.V2 800 600,
    prezenIsFullscreen = False }

page :: PageConfig -> PageContents () -> Prezen ()
page config content = liftF $ Page config content ()

instance MonadIO Prezen where
  liftIO io = liftF $ LiftIO io

