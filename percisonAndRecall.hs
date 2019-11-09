import System.Environment(getArgs)
import Data.Colour.Names
import Data.Colour
import Control.Lens
import Data.Default.Class
import Data.Time.LocalTime
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo


prices' :: [(Int, Int, Int)]
prices' = [(a, a, a) | a <- [1..]]

chart = toRenderable layout 
  where

    price1 = plot_lines_style . line_color .~ opaque blue
           $ plot_lines_values .~ [ [ (d,v) | (d,v,_) <- prices'] ]
           $ plot_lines_title .~ "price 1"
           $ def

    layout = layoutlr_title .~"Price History"
           $ layoutlr_left_axis . laxis_override .~ axisGridHide
           $ layoutlr_right_axis . laxis_override .~ axisGridHide
           $ layoutlr_x_axis . laxis_override .~ axisGridHide
           $ layoutlr_plots .~ [Left (toPlot price1)]
           $ layoutlr_grid_last .~ False
           $ def

main = renderableToFile def "example2_big.png" chart