{-# LANGUAGE ImplicitParams #-}
import Control.Monad
import Graphics.UI.FreePrezen
import System.IO (hFlush, stdout)

toppage txt m = page ?cfgTop $ do
  move (V2 50 100) $ text 100 white txt
  move (V2 100 300) $ m

title txt m = page ?cfgPage $ do
  move (V2 50 50) $ do
    move (V2 (-50) 0) $ box (V2 800 70) (white, white)
    move (V2 0 2) $ text 60 black txt
  move (V2 100 150) m

txt = text 40 white

main = do
  font <- loadFont "VL-PGothic-Regular.ttf"
  topbg <- loadBitmapFromFile "toppage.png"
  img <- loadBitmapFromFile "image.png"
  let ?cfgTop = PageConfig {
    pageFont = font,
    pageBackground = Right topbg }
  let ?cfgPage = PageConfig {
    pageFont = font,
    pageBackground = Left (black, white) }
  runPrezen def $ do
    toppage "ぷれぜん" $ do
      txt  "なんか説明"
    title "一ページ目" $ do
      txt "だからどうした"
      wait
      move (V2 80 5) $ image img
    title "二ページ目" $ do
      txt "これで"
      txt "おしまい"
    title "三ページ目" $ do
      txt "IOのテスト"
      txt "コンソールで真偽値を答えてみて"
      txt "ください"
    ans <- liftIO $ putStr "continue? " >> hFlush stdout >> fmap read getLine
    when ans $ do
      title "三ページ目" $ do
        wait
        txt "と、"
        wait
        txt "思ったら"
        wait
        txt "続いた"
      title "四ページ目" $ do
        wait
        txt "おいおい"
        wait
        txt "まだ"
        wait
        txt "続くのかよ"
