module Deep where

import Shallow (Point, Region, Transformation)
import qualified Shallow as S
import Data.Maybe (fromMaybe)


data RegionAST
    = FromPoints [Point]
    | Rectangle Float Float
    | Circle Float
    | Complement RegionAST
    | Union RegionAST RegionAST
    | Intersection RegionAST RegionAST
    | Transform TransformationAST RegionAST
    deriving (Show, Eq)

data TransformationAST
    = Translation Float Float
    | Scaling Float
    | Combine [TransformationAST]
    deriving (Show, Eq)

fromPoints :: [Point] -> RegionAST
fromPoints = FromPoints

rectangle :: Float -> Float -> RegionAST
rectangle = Rectangle

circle :: Float -> RegionAST
circle = Circle

complement :: RegionAST -> RegionAST
complement = Complement

union :: RegionAST -> RegionAST -> RegionAST
union = Union

intersection :: RegionAST -> RegionAST -> RegionAST
intersection = Intersection

translation :: Float -> Float -> TransformationAST
translation = Translation

scaling :: Float -> TransformationAST
scaling = Scaling

combineTransformations :: [TransformationAST] -> TransformationAST
combineTransformations [transformation] = transformation
combineTransformations transformations = Combine transformations

applyTransformation :: TransformationAST -> RegionAST -> RegionAST
applyTransformation (Combine []) region = region
applyTransformation transformation region = Transform transformation region

toTransformation :: TransformationAST -> Transformation
toTransformation transformation
    | isTranslation transformation = S.translation tx ty
    | isScaling transformation = S.scaling factor
    | otherwise = S.combineTransformations (map toTransformation transScale)

    where 
        isTranslation (Translation _ _) = True
        isTranslation _ = False

        isScaling (Scaling _) = True
        isScaling _ = False

        transScale = fromMaybe [] $ case transformation of
            Combine lst -> Just lst
            _ -> Nothing

        (tx, ty) = fromMaybe (0, 0) $ case transformation of 
            Translation x y -> Just (x, y)
            _ -> Nothing

        factor = fromMaybe 1.0  $ case transformation of 
            Scaling factor -> Just factor
            _ -> Nothing

toRegion :: RegionAST -> Region
toRegion region 
    | isFromPoints region = S.fromPoints points
    | isRectangle region = S.rectangle width height
    | isCircle region = S.circle radius
    | isComplement region = S.complement (toRegion complementedRegion)
    | isUnion region = S.union (toRegion region1) (toRegion region2)
    | isIntersection region = S.intersection (toRegion region3) (toRegion region4)
    | isTransformation region = S.applyTransformation (toTransformation t) (toRegion r)

    where
        isFromPoints (FromPoints _) = True
        isFromPoints _ = False

        isRectangle (Rectangle _ _) = True
        isRectangle _ = False

        isCircle (Circle _) = True
        isCircle _ = False

        isComplement (Complement _) = True
        isComplement _ = False

        isUnion (Union _ _) = True
        isUnion _ = False

        isIntersection (Intersection _ _) = True
        isIntersection _ = False

        isTransformation (Transform _ _) = True
        isTransformation _ = False

        (width, height) = fromMaybe (0, 0) $ case region of 
            Rectangle width height -> Just (width, height)
            _ -> Nothing

        radius = fromMaybe 0.0  $ case region of 
            Circle radius -> Just radius
            _ -> Nothing

        points = fromMaybe [] $ case region of
            FromPoints points -> Just points
            _ -> Nothing

        complementedRegion = fromMaybe (FromPoints []) $ case region of
            Complement region -> Just region
            _ -> Nothing

        (region1, region2) = fromMaybe (FromPoints [], FromPoints []) $ case region of
            Union r1 r2 -> Just (r1, r2)
            _ -> Nothing

        (region3, region4) = fromMaybe (FromPoints [], FromPoints []) $ case region of
            Intersection r3 r4 -> Just (r3, r4)
            _ -> Nothing

        (t, r) = case region of
            Transform transformation region -> (transformation, region)
            _ -> error "Not a Transform region"

inside :: Point -> RegionAST -> Bool
inside = flip toRegion

decomposeTransformation :: TransformationAST -> [TransformationAST]
decomposeTransformation transformation
    | isTranslation transformation = [Translation tx ty]
    | isScaling transformation = [Scaling factor]
    | otherwise = concatMap decomposeTransformation transScale

    where 
        isTranslation (Translation _ _) = True
        isTranslation _ = False

        isScaling (Scaling _) = True
        isScaling _ = False

        transScale = fromMaybe [] $ case transformation of
            Combine lst -> Just lst
            _ -> Nothing

        (tx, ty) = fromMaybe (0, 0) $ case transformation of 
            Translation x y -> Just (x, y)
            _ -> Nothing

        factor = fromMaybe 1.0  $ case transformation of 
            Scaling factor -> Just factor
            _ -> Nothing

fuseTransformations :: [TransformationAST] -> [TransformationAST]
fuseTransformations [] = []
fuseTransformations [t] = [t]
fuseTransformations (t1 : t2 : rest)
    | isTranslation t1 && isTranslation t2 = 
        let (x1, y1) = case t1 of
                            Translation x y -> (x, y)
                            _ -> error "Not a Translation transformation"

            (x2, y2) = case t2 of
                            Translation x y -> (x, y)
                            _ -> error "Not a Translation transformation"
        in fuseTransformations (Translation (x1 + x2) (y1 + y2) : rest)

    | isScaling t1 && isScaling t2 =    
        let factor1 = case t1 of
                            Scaling factor1 -> factor1
                            _ -> error "Not a Scaling transformation"

            factor2 = case t2 of
                            Scaling factor2 -> factor2
                            _ -> error "Not a Scaling transformation"
        in fuseTransformations (Scaling (factor1 * factor2) : rest)

    | otherwise = 
        t1 : fuseTransformations (t2 : rest)

    where
        isTranslation (Translation _ _) = True
        isTranslation _ = False

        isScaling (Scaling _) = True
        isScaling _ = False

optimizeTransformations :: RegionAST -> RegionAST
optimizeTransformations region
    | isPrimary region = region
    | isComplement region = optimizeComplement region
    | isBinaryOperation region = optimizeBinaryOperation region
    | isTransform region = optimizeTransform region
    where
        isPrimary (FromPoints _) = True
        isPrimary (Rectangle _ _) = True
        isPrimary (Circle _) = True
        isPrimary _ = False

        isComplement (Complement _) = True
        isComplement _ = False

        isBinaryOperation (Union _ _) = True
        isBinaryOperation (Intersection _ _) = True
        isBinaryOperation _ = False

        isTransform (Transform _ _) = True
        isTransform _ = False

optimizeComplement :: RegionAST -> RegionAST
optimizeComplement (Complement r)
    | Transform t r' <- optimizeTransformations r = Transform t (Complement r')
    | otherwise = Complement (optimizeTransformations r)

optimizeBinaryOperation :: RegionAST -> RegionAST
optimizeBinaryOperation (Union r1 r2)
    | (Transform t1 r1', Transform t2 r2') <- (optimizeTransformations r1, optimizeTransformations r2), t1 == t2 =
        Transform t1 (Union r1' r2')
    | otherwise = Union (optimizeTransformations r1) (optimizeTransformations r2)

optimizeBinaryOperation (Intersection r1 r2)
    | (Transform t1 r1', Transform t2 r2') <- (optimizeTransformations r1, optimizeTransformations r2), t1 == t2 =
        Transform t1 (Intersection r1' r2')
    | otherwise = Intersection (optimizeTransformations r1) (optimizeTransformations r2)

optimizeTransform :: RegionAST -> RegionAST
optimizeTransform (Transform t r)
    | Transform t' r' <- optimizeTransformations r =
        let combinedTransformations = fuseTransformations (decomposeTransformation t ++ decomposeTransformation t')
        in Transform (combineTransformations combinedTransformations) r'
    | otherwise = Transform (combineTransformations (fuseTransformations (decomposeTransformation t))) (optimizeTransformations r)
