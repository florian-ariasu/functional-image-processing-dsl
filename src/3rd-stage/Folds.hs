{-# OPTIONS_GHC -Wno-missing-methods #-}
module Folds where

import Shallow (Point, Region, Transformation)
import qualified Shallow as S

data RegionShape a
    = FromPoints [Point]
    | Rectangle Float Float
    | Circle Float
    | Complement a
    | Union a a
    | Intersection a a
    | Transform TransformationAST a
    deriving (Show, Eq)

data TransformationShape a
    = Translation Float Float
    | Scaling Float
    | Combine [a]
    deriving (Show, Eq)

newtype RegionAST
    = R (RegionShape RegionAST)
    deriving (Eq)

newtype TransformationAST
    = T (TransformationShape TransformationAST)
    deriving (Eq)

fromPoints :: [Point] -> RegionAST
fromPoints = R . FromPoints

rectangle :: Float -> Float -> RegionAST
rectangle width height = R $ Rectangle width height

circle :: Float -> RegionAST
circle = R . Circle

complement :: RegionAST -> RegionAST
complement = R . Complement

union :: RegionAST -> RegionAST -> RegionAST
union region1 region2 = R $ Union region1 region2

intersection :: RegionAST -> RegionAST -> RegionAST
intersection region1 region2 = R $ Intersection region1 region2

translation :: Float -> Float -> TransformationAST
translation tx ty = T $ Translation tx ty

scaling :: Float -> TransformationAST
scaling = T . Scaling

combineTransformations :: [TransformationAST] -> TransformationAST
combineTransformations [transformation] = transformation
combineTransformations transformations = T $ Combine transformations

applyTransformation :: TransformationAST -> RegionAST -> RegionAST
applyTransformation (T (Combine [])) region = region
applyTransformation transformation region = R $ Transform transformation region

instance Show TransformationAST where
    show :: TransformationAST -> String
    show (T transformation)
        | Translation tx ty <- transformation = "+(" ++ show tx ++ "," ++ show ty ++ ")"
        | Scaling f <- transformation = "*<" ++ show f ++ ">"
        | Combine ts <- transformation = show ts

instance Show RegionAST where
    show :: RegionAST -> String
    show = showRegion 0
        where
            indent n = replicate (2 * n) ' '
            showRegion level (R region)
                | FromPoints points <- region =
                    indent level ++ "FromPoints " ++ show points
                | Rectangle width height <- region =
                    indent level ++ "Rectangle " ++ show width ++ " " ++ show height
                | Circle radius <- region =
                    indent level ++ "Circle " ++ show radius
                | Complement subRegion <- region =
                    indent level ++ "~\n" ++ showRegion (level + 1) subRegion
                | Union region1 region2 <- region =
                    indent level ++ "+\n" ++ showRegion (level + 1) region1 ++ "\n" ++ showRegion (level + 1) region2
                | Intersection region1 region2 <- region =
                    indent level ++ "*\n" ++ showRegion (level + 1) region1 ++ "\n" ++ showRegion (level + 1) region2
                | Transform transformation subRegion <- region =
                    indent level ++ show transformation ++ "\n" ++ showRegion (level + 1) subRegion

instance Num RegionAST where
    fromInteger :: Integer -> RegionAST
    fromInteger n = fromPoints [(fromInteger n, fromInteger n)]
    
    negate :: RegionAST -> RegionAST
    negate = complement

    (+) :: RegionAST -> RegionAST -> RegionAST
    (+) = union

    (*) :: RegionAST -> RegionAST -> RegionAST
    (*) = intersection

    (-) :: RegionAST -> RegionAST -> RegionAST
    region1 - region2 = region1 * (negate region2)

instance Functor TransformationShape where
    -- fmap :: (a -> b) -> TransformationShape a -> TransformationShape b
    fmap :: (a -> b) -> TransformationShape a -> TransformationShape b
    fmap f transformation
        | Combine transformations <- transformation = Combine (f <$> transformations)
        | Translation tx ty <- transformation = Translation tx ty
        | Scaling factor <- transformation = Scaling factor

instance Functor RegionShape where
    -- fmap :: (a -> b) -> RegionShape a -> RegionShape b
    fmap :: (a -> b) -> RegionShape a -> RegionShape b
    fmap f region
        | FromPoints points <- region =  FromPoints points
        | Rectangle width height <- region = Rectangle width height
        | Circle radius <- region = Circle radius
        | Complement r <- region = Complement $ f r
        | Union region1 region2 <- region = Union (f region1) (f region2)
        | Intersection region1 region2 <- region = Intersection (f region1) (f region2)
        | Transform transformation r <- region = Transform transformation $ f r

type TransformationCombiner a = TransformationShape a -> a
type RegionCombiner a = RegionShape a -> a

foldTransformationAST :: TransformationCombiner a -> TransformationAST -> a
foldTransformationAST f (T transformation) = f (fmap (foldTransformationAST f) transformation)

foldRegionAST :: RegionCombiner a -> RegionAST -> a
foldRegionAST f (R region) = f (fmap (foldRegionAST f) region)

toTransformation :: TransformationAST -> Transformation
toTransformation = foldTransformationAST combiner
  where
    combiner :: TransformationCombiner Transformation
    combiner transformation
        | Translation tx ty <- transformation = S.translation tx ty
        | Scaling factor <- transformation = S.scaling factor
        | Combine transformations <- transformation = S.combineTransformations transformations

basicTransformationCount :: TransformationAST -> Int
basicTransformationCount = foldTransformationAST combiner
  where
    combiner :: TransformationCombiner Int
    combiner transformation
        | Translation _ _ <- transformation = 1
        | Scaling _ <- transformation = 1
        | Combine transformations <- transformation = sum transformations

basicEntityCount :: RegionAST -> (Int, Int)
basicEntityCount = foldRegionAST combiner
  where
    combiner :: RegionCombiner (Int, Int)
    combiner region
        | FromPoints _ <- region = (1, 0)
        | Rectangle _ _ <- region = (1, 0)
        | Circle _ <- region = (1, 0)
        | Complement (r1, t1) <- region = (r1, t1)
        | Union (r1, t1) (r2, t2) <- region = (r1 + r2, t1 + t2)
        | Intersection (r1, t1) (r2, t2) <- region =  (r1 + r2, t1 + t2)
        | Transform transformation (r, t) <- region = (r, t + basicTransformationCount transformation)

showFoldFlag :: Bool
showFoldFlag = False
