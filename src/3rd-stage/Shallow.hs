{-# LANGUAGE TupleSections #-}
module Shallow where

import Data.List hiding (union)
import qualified Data.Set as S
import Debug.Trace


type Point = (Float, Float)

type Pointed a = Point -> a

type Region = Pointed Bool

type Transformation = Point -> Point


inside :: Point -> Region -> Bool
inside = flip ($)

fromPoints :: [Point] -> Region
fromPoints = flip $ any . (==)

rectangle :: Float -> Float -> Region
rectangle width height = \(x, y) -> ((<= width / 2) $ abs(x)) && ((<= height / 2) $ abs(y))

circle :: Float -> Region
circle radius = \(x, y) -> (<= radius ** 2) $ x ** 2 + y ** 2

plot :: Int -> Int -> Region -> String
plot width height region = 
    let
     line y 
      | y < -height || y > height = ""
      | otherwise = let chars = [if region (fromIntegral x, fromIntegral y) then '*' else '.' | x <- [-width .. width]]
                    in chars
    in intercalate "\n" [line y | y <- [height, height - 1 .. -height]]

printPlot :: Int -> Int -> Region -> IO ()
printPlot width height region = putStrLn $ plot width height region

promoteUnary :: (a -> b) -> Pointed a -> Pointed b
promoteUnary = (.)

promoteBinary :: (a -> b -> c) -> Pointed a -> Pointed b -> Pointed c
promoteBinary f pointed1 pointed2 point = f (pointed1 point) (pointed2 point)

complement :: Region -> Region
complement = promoteUnary not

union :: Region -> Region -> Region
union = promoteBinary (||)

intersection :: Region -> Region -> Region
intersection = promoteBinary (&&)

translation :: Float -> Float -> Transformation
translation tx ty = \(x, y) -> (x - tx, y - ty)

scaling :: Float -> Transformation
scaling factor = \(x, y) -> (x / factor, y / factor)

applyTransformation :: Transformation -> Region -> Region
applyTransformation = flip (.)

combineTransformations :: [Transformation] -> Transformation
combineTransformations = foldl (.) id . reverse

circles :: Int -> Region
circles n
    | n <= 0    = const False
    | otherwise = union (circle 2)
                        (applyTransformation (translation 6 0)
                                             (circles (n - 1)))

infiniteCircles :: Region
infiniteCircles = union (circle 2)
                        (applyTransformation (translation 6 0)
                                             infiniteCircles)
