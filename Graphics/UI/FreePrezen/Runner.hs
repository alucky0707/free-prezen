module Graphics.UI.FreePrezen.Runner (runPrezen) where

import Graphics.UI.FreePrezen.Base

import Control.Applicative
import Control.Lens
import Control.Monad.Trans
import Control.Monad.Free
import Control.Monad.State
import Data.Array
import Data.Tuple
import qualified Graphics.UI.FreeGame as FG

type RunResult a = Either a a

runResult :: RunResult a -> a
runResult (Right x) = x
runResult (Left  x) = x

mapResult :: (a -> b) -> RunResult a -> RunResult b
mapResult f (Right x) = Right $ f x
mapResult f (Left  x) = Left  $ f x

consResult :: a -> RunResult [a] -> RunResult [a]
consResult x xs = mapResult ((:) x) xs

appendResult :: RunResult [a] -> RunResult [a] -> RunResult [a]
appendResult (Right xs) (Right ys) = Right $ xs ++ ys
appendResult (Right xs) (Left  ys) = Left  $ xs ++ ys
appendResult (Left  xs) _          = Left xs

runPageContents :: (FG.FromFinalizer m, FG.Picture2D m, FG.Figure2D m, Monad m, Applicative m) => Int -> FG.V2 Float -> PageConfig -> PageContents a -> m ()
runPageContents step size cfg page = drawBackground *> sequence (runResult $ flip evalState 0 $ runPageContents' step cfg 0 page) *> pure ()
  where
  drawBackground = case pageBackground cfg of
    Left color -> drawBox size color
    Right bitmap -> do
      let cood = size / 2
      FG.translate cood $ FG.fromBitmap bitmap
  font = pageFont cfg

drawBox :: (FG.Picture2D p, FG.Figure2D p, Monad p) => FG.V2 Float -> (FG.Color, FG.Color) -> p ()
drawBox size (background, border) = do
  FG.colored background $ FG.polygon [pure 0, size & FG._y .~ 0, size, size & FG._x .~ 0]
  FG.colored border $ FG.polygonOutline [pure 0, size & FG._y .~ 0, size, size & FG._x .~ 0]

runPageContents' :: (FG.FromFinalizer m, FG.Picture2D m, FG.Figure2D m, Monad m) => Int -> PageConfig -> Float -> PageContents a -> State Int (RunResult [m ()])
runPageContents' step cfg line (Free (Move cood content cont)) = appendResult <$> ((mapResult $ map $ FG.translate cood) <$> runPageContents' step cfg line content) <*> runPageContents' step cfg line cont
runPageContents' step cfg line (Free (Image bitmap cont)) = consResult (FG.translate (FG.V2 0 (line +  h / 2)) $ FG.fromBitmap bitmap) <$> runPageContents' step cfg line' cont
  where
  (w, h) = FG.bitmapSize bitmap & both %~ realToFrac
  line' = line + h
runPageContents' step cfg line (Free (Text size color txt cont)) = consResult (FG.translate (FG.V2 0 line') $ FG.colored color $ FG.text font size txt) <$> runPageContents' step cfg line' cont
  where
  font = pageFont cfg
  line' = line + size
runPageContents' step cfg line (Free (Box box color cont)) = consResult (FG.translate (FG.V2 0 line) $ drawBox box color) <$> runPageContents' step cfg line cont
runPageContents' step cfg line (Free (Wait cont)) = do
  waited <- get
  if waited < step
    then modify (+ 1) *> runPageContents' step cfg line cont
    else return $ Left []
runPageContents' _    _   _   (Pure _) = return $ Right []

calcStep :: PageContents a -> Int
calcStep (Free (Move _ content cont)) = calcStep content + calcStep cont
calcStep (Free (Image _ cont))        = calcStep cont
calcStep (Free (Text _ _ _ cont))     = calcStep cont
calcStep (Free (Box _ _ cont))        = calcStep cont
calcStep (Free (Wait cont))           = 1 + calcStep cont
calcStep (Pure _)                     = 0

runPrezen :: PrezenConfig -> Prezen a -> IO ()
runPrezen cfg prezen = do
  FG.runGame prm $ runPrezen' 0 0 cfg $ prezenToArray prezen
  return ()
  where
  prm = FG.def {
    FG._windowTitle = "free-prezen",
    FG._windowRegion = let FG.V2 x y = prezenSize cfg in FG.BoundingBox 0 0 x y,
    FG._windowed = not $ prezenIsFullscreen cfg }

fromList :: [a] -> Array Int a
fromList xs = array (0, length xs - 1) $ zip [0..] xs

arrLength :: Array Int a -> Int
arrLength = uncurry (-) . swap . bounds

prezenToArray :: Prezen a -> (Array Int (PageConfig, PageContents (Prezen a)), Maybe (IO (Prezen a)))
prezenToArray prezen = (fromList $ prezenToList prezen, prezenToIO prezen)
  where
  prezenToList (Free (Page cfg page cont)) = (cfg, page) : prezenToList cont
  prezenToList (Free (LiftIO cont)) = []
  prezenToList (Pure _) = []
  prezenToIO (Free (Page _ _ cont)) =prezenToIO cont
  prezenToIO (Free (LiftIO cont)) = Just cont
  prezenToIO (Pure _) = Nothing

runPrezen' :: Int -> Int -> PrezenConfig -> (Array Int (PageConfig, PageContents (Prezen a)), Maybe (IO (Prezen a))) -> FG.Game ()
runPrezen' permStepBase idx cfg (pages, Nothing) = flip fix permStepBase $ \loop permStep-> do
  runPageContents permStep size cfg' page *> FG.tick
  
  next <- any id <$> mapM FG.keySpecial [FG.KeyEnter, FG.KeyRight]
  pred <- FG.keySpecial FG.KeyLeft
  terminate <- FG.keySpecial FG.KeyEsc
  if next
    then if permStep == permStepMax
      then if idx' /= len
        then nextPage permStep *> runPrezen' 0 (idx' + 1) cfg (pages, Nothing) -- go to next page
        else unless terminate $ loop permStep
      else waitPage 10 (permStep + 1) *> loop (permStep + 1)
    else if pred && idx' /= 0
      then if permStep == 0
        then predPage permStep *> runPrezen' predPermStepMax (idx' - 1) cfg (pages, Nothing) -- back to the page
        else waitPage 10 (permStep - 1) *> loop (permStep - 1)
      else unless terminate $ loop permStep
  where
  size = fmap realToFrac $ prezenSize cfg
  FG.V2 w h = size
  len = arrLength pages
  fixing idx = max 0 $ min idx $ len
  idx' = fixing idx -- fixing page index
  page'@(cfg', page) = pages ! idx'
  permStepMax = calcStep page
  predPermStepMax = calcStep $ snd $ pages ! fixing (idx' - 1)
  movePage time stepB stepF back forward = mapM (\t-> uncurry (runPageContents stepB size)  back *> (move t $ uncurry (runPageContents stepF size) forward) *> FG.tick) time *> pure ()
    where
    l = length time
    move t = FG.translate (FG.V2 (t * w / realToFrac l) 0)
  nextPage permStep = movePage [1..30] 0 permStep (pages ! fixing (idx' + 1)) page'
  predPage permStep = movePage [30,29..1] permStep predPermStepMax page' (pages ! fixing (idx' - 1))
  waitPage time permStep = replicateM time (runPageContents permStep size cfg' page *> FG.tick) *> pure ()
runPrezen' permStepBase idx cfg (pages, Just contIO) = flip fix permStepBase $ \loop permStep-> do
  runPageContents permStep size cfg' page *> FG.tick
  
  next <- any id <$> mapM FG.keySpecial [FG.KeyEnter, FG.KeyRight]
  pred <- FG.keySpecial FG.KeyLeft
  terminate <- FG.keySpecial FG.KeyEsc
  if next
    then if permStep == permStepMax
      then if idx' /= len
        then nextPage permStep *> runPrezen' 0 (idx' + 1) cfg (pages, Just contIO) -- go to next page
        else nextIO permStep
      else waitPage 10 (permStep + 1) *> loop (permStep + 1)
    else if pred && idx' /= 0
      then if permStep == 0
        then predPage permStep *> runPrezen' predPermStepMax (idx' - 1) cfg (pages, Just contIO) -- back to the page
        else waitPage 10 (permStep - 1) *> loop (permStep - 1)
      else unless terminate $ loop permStep
  where
  size = fmap realToFrac $ prezenSize cfg
  FG.V2 w h = size
  len = arrLength pages
  fixing idx = max 0 $ min idx $ len
  idx' = fixing idx -- fixing page index
  page'@(cfg', page) = pages ! idx'
  permStepMax = calcStep page
  predPermStepMax = calcStep $ snd $ pages ! fixing (idx' - 1)
  movePage time stepB stepF back forward = mapM (\t-> uncurry (runPageContents stepB size)  back *> (move t $ uncurry (runPageContents stepF size) forward) *> FG.tick) time *> pure ()
    where
    l = length time
    move t = FG.translate (FG.V2 (t * w / realToFrac l) 0)
  nextPage permStep = movePage [1..30] 0 permStep (pages ! fixing (idx' + 1)) page'
  predPage permStep = movePage [30,29..1] permStep predPermStepMax page' (pages ! fixing (idx' - 1))
  waitPage time permStep = replicateM time (runPageContents permStep size cfg' page *> FG.tick) *> pure ()
  nextIO permStep = do
    cont <- FG.embedIO contIO
    let (pages2, contIO2) = prezenToArray cont
        pages3 = listArray (0, len + arrLength pages2 + 1) $ elems pages ++ elems pages2
    runPrezen' permStep idx' cfg (pages3, contIO2)

