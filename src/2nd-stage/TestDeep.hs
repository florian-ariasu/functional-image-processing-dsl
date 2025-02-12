module TestDeep where

import Deep
import TestPP
import qualified Shallow as S

testToTransformation :: TestData
testToTransformation =
    tests
        1
        10
        [ testValApprox "toTransformation translation example" val1 $ (toTransformation $ translation 1 2) (1, 2)
        , testValApprox "toTransformation translation fractional" val2 $ (toTransformation $ translation 1.45 1.95) (2.57, -8.96)
        , testValApprox "toTransformation translation simple structure" val3 $ (toTransformation $ translation 1.45 const1)  (-7.4, 4.5)
        , testValApprox "toTransformation translation complex structure" val4 $ (toTransformation $ translation const2 3.57)  (-log (3.5), log $ cos $ log $ pi/5)
        , testValApprox "toTransformation translation large1" val5 $ (toTransformation $ translation (func_eval 4.5) 9000) (-656, 943)
        , testValApprox "toTransformation translation large2" val6 $ (toTransformation $ translation (func_eval 4.5) 15000) (435.43, 243.435)
        , testValApprox "toTransformation scaling example" val7 $ (toTransformation $ scaling 2) (2, 2)
        , testValApprox "toTransformation scaling fractional" val8 $ (toTransformation $ scaling 1.95) (2.57, -8.96)
        , testValApprox "toTransformation scaling simple structure" val9 $ (toTransformation $ scaling const1) (-7.4, 4.5)
        , testValApprox "toTransformation scaling complex structure" val10 $ (toTransformation $ scaling const2) (-log (2.5e-4), log $ cos $ log $ pi / (tan pi/4))
        , testValApprox "toTransformation scaling large1" val11 $ (toTransformation $ scaling (func_eval 2.56)) (-656, 9000)
        , testValApprox "toTransformation scaling large2" val12 $ (toTransformation $ scaling (func_eval 5.65)) (-15000, 567.435)
        , testValApprox "toTransformation combineTransformations example" val13 $ (toTransformation $ combineTransformations [scaling 0.5, translation 1 0]) (2.57, -8.96)
        , testValApprox "toTransformation combineTransformations simple1" val14 $ (toTransformation $ combineTransformations [scaling 0.5, translation 1 0, scaling 2.7]) (-7.4, 4.5)
        , testValApprox "toTransformation combineTransformations simple2" val15 $ (toTransformation $ combineTransformations [scaling 0.5, translation 5 0, scaling 2.2, translation 1 2, scaling 4.5]) (-log (3.5), log $ cos $ log $ pi/5)
        , testValApprox "toTransformation combineTransformations strip" val16 $ (toTransformation $ combineTransformations [scaling 0.2, translation 1.5 0, scaling 4.5, translation 3 2, scaling 1.5]) (-656, 943)
        , testValApprox "toTransformation combineTransformations circle" val17 $ (toTransformation $ combineTransformations [scaling 0.8, translation 1.4 1.1, scaling 5, translation 1.8 2, scaling 2.5]) (435.43, 243.435)
        , testValApprox "toTransformation combineTransformations fractional" val18 $ (toTransformation $ combineTransformations [scaling 0.5, translation 1 0, scaling 2.7, translation 1 2, scaling 4.5, scaling 1.7, translation 1.4 2]) (2, 2)
        , testValApprox "toTransformation  combineTransformations large2" val19 $ (toTransformation $ combineTransformations [scaling 0.5, translation 1 0, scaling 2.7, translation 1 2, scaling 4.5]) (-log (2.5e-4), log $ cos $ log $ pi / (tan pi/4))
        ]
        where func_eval = sqrt . sum . map (sin . exp . sqrt . exp) . enumFromTo 1
              const1 = sin 3 * pi/5
              const2 = cos 7 * pi/11
              val1 = S.translation 1 2 (1, 2)
              val2 = S.translation 1.45 1.95 (2.57, -8.96)
              val3 = S.translation 1.45 const1 (-7.4, 4.5)
              val4 = S.translation const2 3.57 (-log (3.5), log $ cos $ log $ pi/5)
              val5 = S.translation (func_eval 4.5) 9000 (-656, 943)
              val6 = S.translation (func_eval 4.5) 15000 (435.43, 243.435)
              val7 = S.scaling 2 (2, 2)
              val8 = S.scaling 1.95 (2.57, -8.96)
              val9 = S.scaling const1 (-7.4, 4.5)
              val10 = S.scaling const2 (-log (2.5e-4), log $ cos $ log $ pi / (tan pi/4))
              val11 = S.scaling (func_eval 2.56) (-656, 9000)
              val12 = S.scaling (func_eval 5.65) (-15000, 567.435)
              val13 = (S.combineTransformations [S.scaling 0.5, S.translation 1 0]) (2.57, -8.96)
              val14 = (S.combineTransformations [S.scaling 0.5, S.translation 1 0, S.scaling 2.7]) (-7.4, 4.5)
              val15 = (S.combineTransformations [S.scaling 0.5, S.translation 5 0, S.scaling 2.2, S.translation 1 2, S.scaling 4.5]) (-log (3.5), log $ cos $ log $ pi/5)
              val16 = (S.combineTransformations [S.scaling 0.2, S.translation 1.5 0, S.scaling 4.5, S.translation 3 2, S.scaling 1.5]) (-656, 943)
              val17 = (S.combineTransformations [S.scaling 0.8, S.translation 1.4 1.1, S.scaling 5, S.translation 1.8 2, S.scaling 2.5]) (435.43, 243.435)
              val18 = (S.combineTransformations [S.scaling 0.5, S.translation 1 0, S.scaling 2.7, S.translation 1 2, S.scaling 4.5, S.scaling 1.7, S.translation 1.4 2]) (2, 2)
              val19 = (S.combineTransformations [S.scaling 0.5, S.translation 1 0, S.scaling 2.7, S.translation 1 2, S.scaling 4.5]) (-log (2.5e-4), log $ cos $ log $ pi / (tan pi/4))

testToRegion :: TestData
testToRegion =
    tests
        2
        20
        [ testVal "toRegion ex1" str1 $ S.plot 2 1 $ toRegion $ fromPoints [(0, 0), (1, 1)]
        , testVal "toRegion ex2" str2 $ S.plot 2 2 $ toRegion $ rectangle 2 2
        , testVal "toRegion ex3" str3 $ S.plot 2 2 $ toRegion $ circle 2
        , testVal "toRegion strip" str4 $ S.plot 5 0 $ toRegion $ rectangle 4 10
        , testVal "toRegion circle" str5 $ S.plot 4 3 $ toRegion $ circle 3
        , testVal "toRegion fatCircle" str6 $ S.plot 4 3 $ toRegion $ circle 4
        , testVal "toRegion fractional" str7 $ S.plot 0 3 $ toRegion $ circle 0.9
        , testVal "toRegion complex" str9 $ S.plot 4 4 $ toRegion complexASTRegion
        , testVal "toRegion complement reversed circle" str10 $ S.plot 3 3 $ toRegion $ complement $ circle 2
        , testVal "toRegion complement missing points" str11 $ S.plot 3 3 $ toRegion $ complement $ fromPoints [(0, 0), (1, 0), (0, 1), (1, 1)]
        , testVal "toRegion complement small window" str12 $ S.plot 3 3 $ toRegion $ complement $ rectangle 2 3
        , testVal "toRegion complement star" str13 $ S.plot 5 5 $ toRegion $ complement $ fromPoints [(-3, 0), (-2, 0), (-1, 0), (0, 0), (1, 0), (2, 0), (3, 0), (0, 1), (0, 2), (0, 3), (0, -1), (0, -2), (0, -3), (1, 1), (2, 2), (1, -1), (2, -2), (-1, 1), (-2, 2), (-1, -1), (-2, -2)]
        , testVal "toRegion complement cross" str14 $ S.plot 3 3 $ toRegion $ complement $ fromPoints [(0, 0), (0, 1), (0, 2), (0, -1), (-1, 1), (1, 1)]
        , testVal "toRegion complement medium circle" str15 $ S.plot 5 5 $ toRegion $ complement $ applyTransformation (translation 2 2) $ circle 3
        , testVal "toRegion complement big circle" str16 $ S.plot 7 7 $ toRegion $ complement $ applyTransformation (scaling 2) $ circle 2
        , testVal "toRegion union example" str17 $ S.plot 2 2 $ toRegion $ union (circle 1) (fromPoints [(0, 0), (-2, 2), (2, -2)])
        , testVal "toRegion union overlap" str18 $ S.plot 4 4 $ toRegion $ union (applyTransformation (translation (-1) 0) (circle 2)) (applyTransformation (translation 1 0) (circle 2))
        , testVal "toRegion union no overlap" str19 $ S.plot 5 5 $ toRegion $ union (applyTransformation (translation (-2) 0) (circle 1)) (applyTransformation (translation 2 0) (circle 1))
        , testVal "toRegion union includes" str20 $ S.plot 4 4 $ toRegion $ union (rectangle 2 2) (rectangle 3 3)
        , testVal "toRegion union overlap shape" str21 $ S.plot 4 4 $ toRegion $ union (circle 2) (rectangle 6 2)
        , testVal "toRegion union cross" str22 $ S.plot 4 4 $ toRegion $ union (rectangle 6 1) (rectangle 1 6)
        , testVal "toRegion union letterT" str23 $ S.plot 5 5 $ toRegion $ union (applyTransformation (translation 0 5) (rectangle 7 2)) (rectangle 2 10)
        , testVal "toRegion union orthogonal" str24 $ S.plot 5 5 $ toRegion $ union (applyTransformation (translation 0 (-5)) (rectangle 10 2)) (rectangle 2 10)
        , testVal "toRegion union complement" str25 $ S.plot 5 5 $ toRegion $ union (fromPoints [(0, 0)]) (complement $ fromPoints [(0, 0)])
        , testVal "toRegion union perfect overlap" str26 $ S.plot 4 4 $ toRegion $ union (circle 3) (circle 3)
        , testVal "toRegion intersection example" str27 $ S.plot 3 3 $ toRegion $ intersection (circle 1) (fromPoints [(0, 0), (-2, 2), (2, -2)])
        , testVal "toRegion intersection overlap" str28 $ S.plot 4 4 $ toRegion $ intersection (applyTransformation (translation (-1) 0) (circle 2)) (applyTransformation (translation 1 0) (circle 2))
        , testVal "toRegion intersection difference" str29 $ S.plot 4 4 $ toRegion $ intersection (complement $ circle 2) (circle 3)
        , testVal "toRegion intersection includes" str30 $ S.plot 4 4 $ toRegion $ intersection (rectangle 2 2) (rectangle 3 3)
        , testVal "toRegion intersection complement" str31 $ S.plot 4 4 $ toRegion $ intersection (circle 2) (complement $ circle 2)
        , testVal "toRegion intersection perfect overlap" str32 $ S.plot 4 4 $ toRegion $ intersection (circle 3) (circle 3)
        , testVal "toRegion intersection slice" str33 $ S.plot 4 4 $ toRegion $ intersection (circle 2) (rectangle 6 2)
        , testVal "toRegion intersection square" str34 $ S.plot 4 4 $ toRegion $ intersection (rectangle 6 2) (rectangle 2 6)
        , testVal "toRegion intersection double intersect" str35 $ S.plot 5 5 $ toRegion $ intersection (circle 3) $ intersection (applyTransformation (translation 1 1) (circle 3)) (applyTransformation (translation 0 1) (circle 3))
        , testVal "toRegion intersection set intersect" str36 $ S.plot 5 5 $ toRegion $ intersection (fromPoints [(0, 0), (0, 1), (1, 1), (2, 2), (-1, 0)]) $ fromPoints [(0, 0), (1, 3), (2, 2), (5, 3), (1, 8)]
        ]
  where
    str1 = S.plot 2 1 $ S.fromPoints [(0, 0), (1, 1)]
    str2 = S.plot 2 2 $ S.rectangle 2 2
    str3 = S.plot 2 2 $ S.circle 2
    str4 = S.plot 5 0 $ S.rectangle 4 10
    str5 = S.plot 4 3 $ S.circle 3
    str6 = S.plot 4 3 $ S.circle 4
    str7 = S.plot 0 3 $ S.circle 0.9
    str8 = S.plot 10 2 $ (\(x, y) -> abs (y - sin x) < 0.5)
    str9 = S.plot 4 4 $ complexRegion
    complexASTRegion = rectangle 3.2 3.1 `union` fromPoints complexPoints
    complexRegion p = S.rectangle 3.2 3.1 p || S.fromPoints complexPoints p
    complexPoints = [(-3, 3), (4, 2), (5, 10), (-3, -2), (3, -2), (-2, -3), (-1, -3), (0, -3), (1, -3), (2, -3)]
    str10 = S.plot 3 3 $ S.complement $ S.circle 2
    str11 = S.plot 3 3 $ S.complement $ S.fromPoints [(0, 0), (1, 0), (0, 1), (1, 1)]
    str12 = S.plot 3 3 $ S.complement $ S.rectangle 2 3
    str13 = S.plot 5 5 $ S.complement $ S.fromPoints [(-3, 0), (-2, 0), (-1, 0), (0, 0), (1, 0), (2, 0), (3, 0), (0, 1), (0, 2), (0, 3), (0, -1), (0, -2), (0, -3), (1, 1), (2, 2), (1, -1), (2, -2), (-1, 1), (-2, 2), (-1, -1), (-2, -2)]
    str14 = S.plot 3 3 $ S.complement $ S.fromPoints [(0, 0), (0, 1), (0, 2), (0, -1), (-1, 1), (1, 1)]
    str15 = S.plot 5 5 $ S.complement $ S.applyTransformation (S.translation 2 2) $ S.circle 3
    str16 = S.plot 7 7 $  S.complement $ S.applyTransformation (S.scaling 2) $ S.circle 2
    str17 = S.plot 2 2 $ S.union (S.circle 1) (S.fromPoints [(0, 0), (-2, 2), (2, -2)])
    str18 = S.plot 4 4 $ S.union (S.applyTransformation (S.translation (-1) 0) (S.circle 2)) (S.applyTransformation (S.translation 1 0) (S.circle 2))
    str19 = S.plot 5 5 $ S.union (S.applyTransformation (S.translation (-2) 0) (S.circle 1)) (S.applyTransformation (S.translation 2 0) (S.circle 1))
    str20 = S.plot 4 4 $ S.union (S.rectangle 2 2) (S.rectangle 3 3)
    str21 = S.plot 4 4 $ S.union (S.circle 2) (S.rectangle 6 2)
    str22 = S.plot 4 4 $ S.union (S.rectangle 6 1) (S.rectangle 1 6)
    str23 = S.plot 5 5 $ S.union (S.applyTransformation (S.translation 0 5) (S.rectangle 7 2)) (S.rectangle 2 10)
    str24 = S.plot 5 5 $ S.union (S.applyTransformation (S.translation 0 (-5)) (S.rectangle 10 2)) (S.rectangle 2 10)
    str25 = S.plot 5 5 $ S.union (S.fromPoints [(0, 0)]) (S.complement $ S.fromPoints [(0, 0)])
    str26 = S.plot 4 4 $ S.union (S.circle 3) (S.circle 3)
    str27 = S.plot 3 3 $ S.intersection (S.circle 1) (S.fromPoints [(0, 0), (-2, 2), (2, -2)])
    str28 = S.plot 4 4 $ S.intersection (S.applyTransformation (S.translation (-1) 0) (S.circle 2)) (S.applyTransformation (S.translation 1 0) (S.circle 2))
    str29 = S.plot 4 4 $ S.intersection (S.complement $ S.circle 2) (S.circle 3)
    str30 = S.plot 4 4 $ S.intersection (S.rectangle 2 2) (S.rectangle 3 3)
    str31 = S.plot 4 4 $ S.intersection (S.circle 2) (S.complement $ S.circle 2)
    str32 = S.plot 4 4 $ S.intersection (S.circle 3) (S.circle 3)
    str33 = S.plot 4 4 $ S.intersection (S.circle 2) (S.rectangle 6 2)
    str34 = S.plot 4 4 $ S.intersection (S.rectangle 6 2) (S.rectangle 2 6)
    str35 = S.plot 5 5 $ S.intersection (S.circle 3) $ S.intersection (S.applyTransformation (S.translation 1 1) (S.circle 3)) (S.applyTransformation (S.translation 0 1) (S.circle 3))
    str36 = S.plot 5 5 $ S.intersection (S.fromPoints [(0, 0), (0, 1), (1, 1), (2, 2), (-1, 0)]) $ S.fromPoints [(0, 0), (1, 3), (2, 2), (5, 3), (1, 8)]

testDecomposeTransformation :: TestData
testDecomposeTransformation =
    tests
        3
        10
        [ testVal "decomposeTransformation ex1" [Translation 1 2] $ decomposeTransformation (Translation 1 2)
        , testVal "decomposeTransformation ex2" [Scaling 2] $ decomposeTransformation (Scaling 2)
        , testVal "decomposeTransformation ex3" ex3Flat $ decomposeTransformation ex3Nest
        , testVal "decomposeTransformation oneLevel" oneLevelFlat $ decomposeTransformation oneLevelNest
        , testVal "decomposeTransformation deep" deepFlat $ decomposeTransformation deepNest
        , testVal "decomposeTransformation emptyChildren" emptyChildrenFlat $ decomposeTransformation emptyChildrenNest
        , testVal "decomposeTransformation matrix" matrixFlat $ decomposeTransformation matrixNest
        ]
  where
    ex3Nest = Combine [Translation 1 2, Combine [Translation 3 4, Scaling 2], Scaling 3]
    ex3Flat = [Translation 1 2, Translation 3 4, Scaling 2, Scaling 3]

    -- Un singur nivel în adâncime.
    oneLevelNest = Combine [Scaling 1, Translation 2 3, Scaling 4]
    oneLevelFlat = [Scaling 1, Translation 2 3, Scaling 4]

    -- Test mai adânc.
    deepNest =
        Combine
            [ Translation 1 2
            , Scaling 3
            , Combine [Combine [Scaling 4, Translation 5 6], Translation 7 8, Combine [Combine [Combine [Scaling 9]]]]
            ]
    deepFlat = [Translation 1 2, Scaling 3, Scaling 4, Translation 5 6, Translation 7 8, Scaling 9]

    -- Unele liste pot fi goale.
    emptyChildrenNest = Combine [Combine [], Scaling 1, Combine [Translation 2 3, Combine []]]
    emptyChildrenFlat = [Scaling 1, Translation 2 3]

    -- Liste în formă de matrice.
    matrixNest = Combine [Combine [Scaling 1, Translation 2 3], Combine [Translation 4 5, Translation 6 7], Combine [Scaling 8, Scaling 9]]
    matrixFlat = [Scaling 1, Translation 2 3, Translation 4 5, Translation 6 7, Scaling 8, Scaling 9]

testFuseTransformations :: TestData
testFuseTransformations =
    tests
        4
        20
        [ testVal "fuseTransformations ex1" [Translation 1 2] $ fuseTransformations [Translation 1 2]
        , testVal "fuseTransformations ex2" [Scaling 6] $ fuseTransformations [Scaling 2, Scaling 3]
        , testVal "fuseTransformations ex3" ex3Fused $ fuseTransformations ex3Apart
        , testVal "fuseTransformations trans" transFused $ fuseTransformations transApart
        , testVal "fuseTransformations scale" scaleFused $ fuseTransformations scaleApart
        , testVal "fuseTransformations empty" emptyFused $ fuseTransformations emptyApart
        , testVal "fuseTransformations skip" skipFused $ fuseTransformations skipApart
        , testVal "fuseTransformations big" bigFused $ fuseTransformations bigApart
        ]
  where
    ex3Apart = [Translation 1 2, Translation 3 4, Scaling 2, Scaling 3, Translation 5 6]
    ex3Fused = [Translation 4 6, Scaling 6, Translation 5 6]

    -- Combină mai multe translații.
    transApart = [Translation 1 0, Translation 0 (-1), Translation 4 3]
    transFused = [Translation 5 2]

    -- Combină mai multe scalări.
    scaleApart = [Scaling 4, Scaling 0.5, Scaling 1, Scaling 3]
    scaleFused = [Scaling 6]

    -- Nu dăm erori dacă primim o listă goală.
    emptyApart = []
    emptyFused = emptyApart

    -- Putem combina operații doar în interiorul listei, nu și la capete.
    skipApart = [Scaling 1, Translation 2 3, Scaling 2, Scaling 2, Scaling 4, Translation 4 1, Translation 1 1, Scaling 4]
    skipFused = [Scaling 1, Translation 2 3, Scaling 16, Translation 5 2, Scaling 4]

    -- Test mai mare.
    bigApart = replicate 3 (Translation 1 (-3)) ++ replicate 2 (Scaling 3) ++ replicate 10 (Translation 0 2) ++ replicate 4 (Scaling 2)
    bigFused = [Translation 3 (-9), Scaling 9, Translation 0 20, Scaling 16]

testOptimizeTransformations :: TestData
testOptimizeTransformations =
    tests
        5
        60
        [
            testValW "optimizeTransformations ex1" (30/4) ref1 out1,
            testValW "optimizeTransformations ex2" (30/4) ref2 out2,
            testValW "optimizeTransformations ex3" (30/4) ref3 out3,
            testValW "optimizeTransformations ex4" (30/4) ref4 out4,
            testValW "optimizeTransformations ex5" (10/7) ref5 out5,
            testValW "optimizeTransformations unionNoPrefix" (10/7) ref7 out7,
            testValW "optimizeTransformations intersectionNoPrefix" (10/7) ref8 out8,
            testValW "optimizeTransformations unionNoPrefix2" (10/7) ref9 out9,
            testValW "optimizeTransformations intersectionNoPrefix2" (10/7) ref10 out10,
            testValW "optimizeTransformations doubleUnionNoPrefix" (10/7) ref11 out11,
            testValW "optimizeTransformations mixedNoPrefix" (10/7) ref12 out12,
            testValW "optimizeTransformations ex6" (20/6) ref6 out6,
            testValW "optimizeTransformations doubleUnion" (20/6) ref13 out13,
            testValW "optimizeTransformations doubleIntersection" (20/6) ref14 out14,
            testValW "optimizeTransformations complex" (20/6) ref15 out15,
            testValW "optimizeTransformations reverseComplex" (20/6) ref16 out16,
            testValW "optimizeTransformations supreme" (20/6) ref17 out17
        ]
    where
        out1 = optimizeTransformations $ Transform (Combine [ Translation 1 2, Combine [ Translation 3 4, Scaling 2], Scaling 3]) (Circle 5)
        ref1 = Transform (Combine [Translation 4.0 6.0,Scaling 6.0]) (Circle 5.0)
        out2 = optimizeTransformations $ Transform (Combine [ Translation 1 2, Combine [ Translation 3 4, Scaling 2], Scaling 3]) (Transform (Scaling 4) (Transform (Scaling 2) (Circle 5)))
        ref2 = Transform (Combine [Translation 4.0 6.0,Scaling 48.0]) (Circle 5.0)
        out3 = optimizeTransformations $ Complement (Transform (Scaling 4) (Transform (Scaling 2) (Circle 5)))
        ref3 = Transform (Scaling 8.0) (Complement (Circle 5.0))
        out4 = optimizeTransformations $ Transform (Combine [ Translation 1 2, Combine [ Translation 3 4, Scaling 2], Scaling 3]) (Complement (Transform (Scaling 4) (Transform (Scaling 2) (Circle 5))))
        ref4 = Transform (Combine [Translation 4.0 6.0,Scaling 48.0]) (Complement (Circle 5.0))
        out5 = optimizeTransformations $ Union (Complement (Transform (Scaling 4) (Transform (Scaling 2) (Circle 5)))) (Rectangle 6 7)
        ref5 = Union (Transform (Scaling 8.0) (Complement (Circle 5.0))) (Rectangle 6.0 7.0)
        out6 = optimizeTransformations $ Union (Transform (Combine [ Translation 1 2, Combine [ Translation 3 4, Scaling 2], Scaling 3]) (Complement (Transform (Scaling 4) (Transform (Scaling 2) (Circle 5))))) (Transform (Translation 4 6) (Rectangle 6 7))
        ref6 = Transform (Translation 4.0 6.0) (Union (Transform (Scaling 48.0) (Complement (Circle 5.0))) (Rectangle 6.0 7.0))
        out7 = optimizeTransformations $ Union (Complement (Transform (Scaling 3) (Circle 3))) $ Transform (Translation 1 2) $ Transform (Translation 1 0) $ Circle 2
        ref7 = Union (Transform (Scaling 3.0) (Complement (Circle 3.0))) (Transform (Translation 2.0 2.0) (Circle 2.0))
        out8 = optimizeTransformations $ Intersection (Complement (Transform (Scaling 3) (Circle 3))) $ Transform (Translation 1 2) $ Transform (Translation 1 0) $ Circle 2
        ref8 = Intersection (Transform (Scaling 3.0) (Complement (Circle 3.0))) (Transform (Translation 2.0 2.0) (Circle 2.0))
        out9 = optimizeTransformations $ Union (Transform (Scaling 3) (Circle 3)) $ Complement $ Transform (Combine [Scaling 2, Scaling 3, Combine [Scaling 2, Scaling 3]]) $ Circle 1
        ref9 = Union (Transform (Scaling 3) (Circle 3)) $ Transform (Scaling 36) $ Complement $ Circle 1
        out10 = optimizeTransformations $ Intersection (Transform (Scaling 3) (Circle 3)) $ Complement $ Transform (Combine [Scaling 2, Scaling 3, Combine [Scaling 2, Scaling 3]]) $ Circle 1
        ref10 = Intersection (Transform (Scaling 3) (Circle 3)) $ Transform (Scaling 36) $ Complement $ Circle 1
        out11 = optimizeTransformations $ Union (Transform (Combine [Scaling 2, Scaling 3]) (Circle 2)) $ Union (Rectangle 2 3) $ Complement $ Transform (Scaling 2) $ Circle 3
        ref11 = Union (Transform (Scaling 6) (Circle 2)) $ Union (Rectangle 2 3) $ Transform (Scaling 2) $ Complement $ Circle 3     
        out12 = optimizeTransformations $ Intersection (Transform (Combine [Scaling 2, Scaling 3]) (Circle 2)) $ Union (Rectangle 2 3) $ Complement $ Transform (Scaling 2) $ Circle 3
        ref12 = Intersection (Transform (Scaling 6) (Circle 2)) $ Union (Rectangle 2 3) $ Transform (Scaling 2) $ Complement $ Circle 3     
        out13 = optimizeTransformations $ Union (Transform (Scaling 2) (Transform (Scaling 4) (Rectangle 4 4))) (Union (Transform (Combine [Scaling 4, Scaling 2]) (Circle 3)) (Complement (Transform (Scaling 8) (Circle 7))))
        ref13 = Transform (Scaling 8.0) (Union (Rectangle 4.0 4.0) (Union (Circle 3.0) (Complement (Circle 7.0))))
        out14 = optimizeTransformations $ Intersection (Transform (Scaling 2) (Transform (Scaling 4) (Rectangle 4 4))) (Intersection (Transform (Combine [Scaling 4, Scaling 2]) (Circle 3)) (Complement (Transform (Scaling 8) (Circle 7))))
        ref14 = Transform (Scaling 8.0) (Intersection (Rectangle 4.0 4.0) (Intersection (Circle 3.0) (Complement (Circle 7.0))))
        out15 = optimizeTransformations $ Transform (Combine [Scaling 2, Translation 1 1, Scaling 2, Scaling 4]) $ Union (Transform (Combine [Translation 0 1, Scaling 4, Scaling 3, Translation 2 3]) (Rectangle 3 1)) $ Complement $ Transform (Combine [Translation 0 1, Scaling 2, Scaling 6, Translation 3 4]) $ Rectangle 2 2
        ref15 = Transform (Combine [Scaling 2.0,Translation 1.0 1.0,Scaling 8.0,Translation 0.0 1.0,Scaling 12.0]) (Union (Transform (Translation 2.0 3.0) (Rectangle 3.0 1.0)) (Transform (Translation 3.0 4.0) (Complement (Rectangle 2.0 2.0))))
        out16 = optimizeTransformations $ Transform (Combine [Scaling 2, Translation 1 1, Scaling 2, Scaling 4]) $ Intersection (Transform (Combine [Translation 0 1, Scaling 4, Scaling 3, Translation 2 3]) (Rectangle 3 1)) $ Complement $ Transform (Combine [Translation 0 1, Scaling 2, Scaling 6, Translation 3 4]) $ Rectangle 2 2
        ref16 = Transform (Combine [Scaling 2.0,Translation 1.0 1.0,Scaling 8.0,Translation 0.0 1.0,Scaling 12.0]) (Intersection (Transform (Translation 2.0 3.0) (Rectangle 3.0 1.0)) (Transform (Translation 3.0 4.0) (Complement (Rectangle 2.0 2.0))))
        out17 = optimizeTransformations $ Transform (Scaling 6) $ Union (Union (Complement (Transform (Combine [Scaling 2, Translation 3 4, Scaling 7]) (Circle 4))) (Transform (Combine [Scaling 2, Combine [Translation 3 4], Scaling 5]) (Circle 5))) $ Complement $ Transform (Combine [Scaling 2, Translation 3 4, Scaling 10]) $ Circle 3
        ref17 = Transform (Combine [Scaling 12.0,Translation 3.0 4.0]) (Union (Union (Transform (Scaling 7.0) (Complement (Circle 4.0))) (Transform (Scaling 5.0) (Circle 5.0))) (Transform (Scaling 10.0) (Complement (Circle 3.0))))
              
main :: IO ()
main =
    vmCheck
        [ testToTransformation
        , testToRegion
        , testDecomposeTransformation
        , testFuseTransformations
        , testOptimizeTransformations
        ]