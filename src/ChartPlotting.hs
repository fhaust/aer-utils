
{-# LANGUAGE ViewPatterns #-}

module ChartPlotting where



import           Graphics.Rendering.Chart.Easy
import           Graphics.Rendering.Chart.Backend.Cairo

import           Data.Colour.Palette.BrewerSet

import           Linear
import qualified Data.Vector.Storable as S

import           Data.List
import           Data.Foldable

import           Control.Monad

-------------------------------------------------
--- V I S U A L I Z E   S P I K E   T R A I N ---
-------------------------------------------------

type Events = S.Vector (V3 Double)

eventTo4dPoint :: V3 Double -> (Double,Double,Double,Double)
eventTo4dPoint (V3 x y t) = (x,y,0.1,t)
eventsTo4dPoints es = map eventTo4dPoint $ S.toList es

plotEvents :: FilePath -> Events -> IO ()
plotEvents fn es = toFile def fn $ plotEvents' es 

plotEvents' :: Events -> EC (Layout Double Double) ()
plotEvents' es = do
    {-layout_title      .= "stuff"-}
    layout_background .= solidFillStyle (opaque white)
    layout_foreground .= opaque black
    {-layout_left_axis_visibility . axis_show_ticks .= False-}
    layout_x_axis . laxis_generate .= scaledAxis def (0,5)
    layout_y_axis . laxis_generate .= scaledAxis def (0,5)

    plot $ liftEC $ do
      {-area_spots_4d_title .= "title"-}
      area_spots_4d_max_radius .= 50
      area_spots_4d_values .= eventsTo4dPoints es
      area_spots_4d_palette .= brewerSet YlGn 9
      area_spots_4d_opacity .= 0.5

---------------------------------------------------
--- V I S U A L I Z E   S P I K E   T R A I N S ---
---------------------------------------------------

plotMoreEventsHelper es = layout
  where plot = def & area_spots_4d_max_radius .~ 50
                   & area_spots_4d_values     .~ eventsTo4dPoints es
                   & area_spots_4d_palette    .~ brewerSet YlGn 9
                   & area_spots_4d_opacity    .~ 0.5
        layout = def & layout_plots .~ [toPlot plot]
                     & layout_background .~ solidFillStyle (opaque white)
                     & layout_foreground .~ opaque black
                     & layout_x_axis . laxis_generate .~ scaledAxis def (0,5)
                     & layout_y_axis . laxis_generate .~ scaledAxis def (0,5)

plotMoreEvents fn es = void $ renderableToFile (def & fo_size .~ (800,length es * 600)) fn (plotMoreEvents' es)

plotMoreEvents' es = renderStackedLayouts sls
  where sls = def & slayouts_layouts         .~ map (StackedLayout . plotMoreEventsHelper) (toList es)
                  & slayouts_compress_legend .~ True

    {-let sls = map (StackedLayout . foobar) es-}

    {-slayouts_layouts .= sls-}
    {-slayouts_compress_legend .= True-}




-----------------------------------------
--- V I S U A L I Z E   P A T C H E S ---
-----------------------------------------

{-plotPatches' ps = -}

-----------------------------
--- E R R O R   P L O T S ---
-----------------------------

plotReconstructionError fn before after = toFile def fn $ do
    layout_title      .= "reconstruction error after iteration"
    layout_background .= solidFillStyle (opaque white)
    layout_foreground .= opaque black
    layout_x_axis . laxis_title .= "iterations"
    layout_y_axis . laxis_title .= "reconstruction error"

    let brew = map opaque $ brewerSet Paired 4


    plot $ liftEC $ do
      plot_fillbetween_style  .= solidFillStyle (brew !! 2)
      plot_fillbetween_values .= imap (\x (stats -> (lo,_,_,_,hi)) -> (x,(lo,hi))) after


    plot $ liftEC $ do
      plot_lines_style . line_color .= brew !! 3
      plot_lines_values .= [ imap (\x (stats -> (_,_,y,_,_)) -> (x,y)) after ]
      {-plot_lines_title  .= "error after iteration"-}

    {-plot $ liftEC $ do-}
    {-  plot_fillbetween_style  .= solidFillStyle (brew !! 0)-}
    {-  plot_fillbetween_values .= imap (\x (stats -> (lo,_,_,_,hi)) -> (x,(lo,hi))) before-}

    {-plot $ liftEC $ do-}
    {-  plot_lines_style . line_color .= brew !! 1-}
    {-  plot_lines_values .= [ imap (\x (stats -> (_,_,y,_,_)) -> (x,y)) before  ]-}
    {-  plot_lines_title  .= "error before iteration"-}


    {-plot $ liftEC $ do-}
    {-  plot_candle_values .= imap (\i (stats -> (a,b,c,d,e)) -> Candle i a b c d e) before-}
    {-  plot_candle_line_style . line_color .= c0-}
    {-  [>plot_candle_fill .= True<]-}
    {-  plot_candle_rise_fill_style .= solidFillStyle c0-}
    {-  plot_candle_fall_fill_style .= solidFillStyle c0-}
    {-  plot_candle_centre .= 1-}

    {-plot $ liftEC $ do-}
    {-  plot_candle_values .= imap (\i (stats -> (a,b,c,d,e)) -> Candle i a b c d e) after-}
    {-  plot_candle_line_style . line_color .= c1-}
    {-  [>plot_candle_fill .= True<]-}
    {-  plot_candle_rise_fill_style .= solidFillStyle c1-}
    {-  plot_candle_fall_fill_style .= solidFillStyle c1-}
    {-  plot_candle_centre .= 1-}


stats es = (minimum es, ses !! f, mean es, ses !! (3*f), maximum es)
    where ses = sort es
          f   = length es `div` 4
          mean xs = sum xs / fromIntegral (length xs)
