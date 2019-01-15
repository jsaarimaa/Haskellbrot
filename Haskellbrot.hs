module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Interface.Pure.Display
import Control.Parallel.Strategies
import Data.Complex
import Data.Word
import Data.Maybe
import Data.ByteString (ByteString, pack)

data ComputationState = Computation Image_width Image_height TopLeft BottomRight
                          (Maybe Picture) MousePos DrawMode

type Image_width  = Int
type Image_height = Int
type TopLeft      = Complex Double
type BottomRight  = Complex Double
type MousePos     = Complex Double
data DrawMode     = NoDrawing | Draw (Float,Float) (Float,Float)

max_iter              = 256 :: Int
image_width           = 1500 :: Int
image_height          = 1000 :: Int
initial_top_left      = ((-2) :+ 1) :: Complex Double
initial_bottom_right  = (1 :+ (-1)) :: Complex Double

chunkSize             = image_width

initial_plane = generateComplexPlane
                  initial_top_left initial_bottom_right image_width image_height

initialComputationState :: ComputationState
initialComputationState = Computation image_width image_height
                            initial_top_left initial_bottom_right Nothing (0 :+ 0) NoDrawing

computeIterations :: Complex Double -> Int
computeIterations p = length . takeWhile (\z -> magnitude z <= 2) .
                            take max_iter $ iterate (\z -> z^2 + p) 0

getColor :: Int -> [Word8]
getColor i  | i == max_iter     = [0, 0, 0, 255]
            | i < upper_limit   = [c1, 0, c1, 255]
            | otherwise         = [225, div c2 2, 225, 255]
            where 
                    upper_limit = 64
                    c1 = fromIntegral (div (i*255) upper_limit)
                    c2 = fromIntegral (div (i*255) max_iter)

generateComplexPlane :: TopLeft -> BottomRight -> Image_width -> Image_height -> [Complex Double]
generateComplexPlane tl br iw ih = [(r :+ i)
                    | y <- [0..(ih-1)], let i = (imagPart tl) - ((fromIntegral y)*((abs((imagPart tl)-(imagPart br)))/(fromIntegral (ih - 1)))),
                      x <- [0..(iw-1)], let r = (realPart tl) + ((fromIntegral x)*((abs((realPart tl)-(realPart br)))/(fromIntegral (iw - 1))))]

compute :: Float -> (ComputationState -> ComputationState)
compute dt (Computation iw ih tl br img mp dm)
  = if isNothing img then Computation iw ih tl br new_img mp dm
                     else Computation iw ih tl br img mp dm
        where
            new_cp = generateComplexPlane tl br iw ih
            new_img = Just $ bitmapOfByteString iw ih (BitmapFormat TopToBottom PxRGBA)
                             (toByteString new_cp) True

toByteString :: [Complex Double] -> ByteString
toByteString complex_list = pack.concat $ chunkedParMap (getColor.computeIterations) complex_list

drawComputationState :: ComputationState -> Picture
drawComputationState (Computation iw ih tl br img mp NoDrawing)
    | isNothing img = blank
    | otherwise     = pictures [(fromJust img), (mouse_pos_text mp)]

drawComputationState (Computation iw ih _ _ img mp (Draw (cx,cy) (mx,my)))
    = pictures [fromJust img, mouse_pos_text mp, dragBox] 
        where 
            dragBox = color white boxInRatio
            -- The "zoom box" needs to be in same aspect ratio as the widow
            boxInRatio
                | ((w >= h) && (q1 || q2)) = line [(cx,cy), (mx,cy), (mx,cy+w/ar), (cx,cy+w/ar), (cx,cy)]
                | (w >= h) = line [(cx,cy), (mx,cy), (mx,cy-w/ar), (cx,cy-w/ar), (cx,cy)]
                | (q1 || q4) = line [(cx,cy), (cx-h*ar,cy), (cx-h*ar,my), (cx,my), (cx,cy)]
                | otherwise  = line [(cx,cy), (cx+h*ar,cy), (cx+h*ar,my), (cx,my), (cx,cy)]
                    where 
                        q1 = (cx > mx) && (cy < my)
                        q2 = (cx < mx) && (cy < my)
                        q3 = (cx < mx) && (cy > my)
                        q4 = (cx > mx) && (cy > my)
                        -- aspect ratio
                        ar = fromIntegral iw / fromIntegral ih
                        w  = abs (cx - mx)
                        h  = abs (cy - my)

handleEvents :: Event -> ComputationState -> ComputationState
-- LMB Down
handleEvents (EventKey (MouseButton LeftButton) Down _ m_pos) cs
  = Computation iw ih mp br img mp (Draw m_pos m_pos)
    where (Computation iw ih tl br img mp dm) = updateMPos m_pos cs

-- LMB Up
handleEvents (EventKey (MouseButton LeftButton) Up _ m_pos) cs
    = adjust $ Computation iw ih tl mp Nothing mp NoDrawing
      where (Computation iw ih tl br img mp dm) = updateMPos m_pos cs

handleEvents (EventMotion m_pos) (Computation iw ih tl br img mp (Draw click_p _))
    = updateMPos m_pos (Computation iw ih tl br img mp (Draw click_p m_pos))

-- Mouse moved without drawing mode
handleEvents (EventMotion m_pos) cs
    = updateMPos m_pos cs

-- Other events do nothing
handleEvents _ st = st

mouse_pos_text :: Complex Double -> Picture
mouse_pos_text (mpr:+mpi) = scale 0.2 0.2 
                          . translate (-3500) (-2400) 
                          . color white 
                          . text 
                          $ ((show mpr) ++ " : " ++ (show mpi))
              
updateMPos :: (Float, Float) -> ComputationState -> ComputationState
updateMPos (mx,my) (Computation iw ih tl br img mcc dm)
    = Computation iw ih tl br img new_mcc dm
      where
        new_mcc = mrc :+ mic 
        mrc = ((realToFrac mx) * (realDist tl br / fromIntegral iw)) + ar
        mic = ((realToFrac my) * (imagDist tl br / fromIntegral ih)) + ai
        ar = (realPart tl + realPart br) / 2
        ai = (imagPart tl + imagPart br) / 2

adjust :: ComputationState -> ComputationState
adjust (Computation iw ih (tlr :+ tli) (brr :+ bri) img mp dm)
    | tlr > brr = adjust $ Computation iw ih (brr :+ tli) (tlr :+ bri) img mp dm
    | tli < bri = adjust $ Computation iw ih (tlr :+ bri) (brr :+ tli) img mp dm
    | complexW >= complexH = Computation iw ih (tlr :+ tli) (brr :+ (tli - complexW / aspectRatio)) img mp dm
    | otherwise = Computation iw ih (tlr :+ tli) ((tlr + complexH * aspectRatio) :+ bri) img mp dm
        where aspectRatio = fromIntegral iw / fromIntegral ih
              complexH = abs (tli - bri)
              complexW = abs (tlr - brr)

chunkedParMap :: NFData b => (a -> b) -> [a] -> [b]
chunkedParMap f = withStrategy (parListChunk chunkSize rdeepseq) . map f

imagDist :: Num a => Complex a -> Complex a -> a
imagDist z1 z2 = abs (imagPart z1 - imagPart z2)

realDist :: Num a => Complex a -> Complex a -> a
realDist z1 z2 = abs (realPart z1 - realPart z2)

main :: IO ()
main = play
        (InWindow "Haskellbrot" (1500, 1000) (100,100))
        black
        25
        initialComputationState
        drawComputationState
        handleEvents
        compute