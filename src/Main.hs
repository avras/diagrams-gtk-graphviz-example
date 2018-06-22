{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}


import            Data.GraphViz
import            Data.GraphViz.Attributes.Complete hiding (blue)
import            Data.GraphViz.Commands

import            Control.Monad.Trans.Reader (ReaderT(..))
import            GI.Gtk hiding (main, noArrow)
import qualified  GI.Gtk as Gtk
import qualified  Graphics.Rendering.Cairo as GRC
import qualified  Graphics.Rendering.Cairo.Types as CT (Cairo(..))
import            Graphics.Rendering.Cairo.Internal (Render(..))
import qualified  GI.Cairo as GIC
import            Foreign.Ptr (castPtr)

import            Diagrams.Prelude hiding (Render)
import            Diagrams.Backend.Cairo
import            Diagrams.Backend.Cairo.Internal (Options(..))
import            Diagrams.TwoD.GraphViz

hex = mkGraph [0..19]
        (   [ (v, (v+1)`mod`6, ()) | v <- [0..5] ]
         ++ [ (v, v+k, ()) | v <- [0..5], k <- [6,12] ]
         ++ [ (2,18,()), (2,19,()), (15,18,()), (15,19,()), (18,3,()), (19,3,()) ]
        )

padFactor = 1.2 :: Double

main = do
  let params :: GraphvizParams Int v e () v
      params = defaultDiaParams { fmtEdge = const [arrowTo noArrow] }
  hex' <- layoutGraph' params Neato hex
  let hexDrawing :: Diagram B
      hexDrawing = drawGraph
                     (const $ place (circle 19))
                     (\_ _ _ _ _ p -> stroke p)
                     hex'

  
  Gtk.init Nothing

  win <- windowNew WindowTypeToplevel
  da <- drawingAreaNew

  onWidgetDraw da $ \(GIC.Context fp) -> withManagedPtr fp $ \p -> (`runReaderT` CT.Cairo (castPtr p)) $ runRender $ do
    GRC.liftIO $ putStrLn "Drawing widget"
    w <- GRC.liftIO $ fromIntegral <$> widgetGetAllocatedWidth da
    h <- GRC.liftIO $ fromIntegral <$> widgetGetAllocatedHeight da
    GRC.scale w h
    diagram2render $ hexDrawing
    return True

  setWindowTitle win "Diagram"
  setWindowDefaultWidth win 512
  setWindowDefaultHeight win 512
  onWidgetDestroy win mainQuit
  containerAdd win da
  widgetShowAll win
  Gtk.main

diagram2render :: Diagram B -> Render ()
diagram2render d =
  snd $ renderDia Cairo (CairoOptions "" (dims2D 1.0 1.0) RenderOnly False) $ d # lw 0.005 # padX padFactor # padY padFactor
