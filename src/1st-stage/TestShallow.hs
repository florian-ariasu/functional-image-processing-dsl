module TestShallow where

import Shallow
import TestPP

testInside :: TestData
testInside =
    tests
        1
        5
        [ testCond "inside ex1" $ inside (0, 0) (== (0, 0))
        , testCond "inside ex2" $ not $ inside (1, 1) (== (0, 0))
        , testCond "inside list1" $ inside (2, 2) (`elem` [(1, 1), (2, 2), (0, 0)])
        , testCond "inside list2" $ not $ inside (-1, 3) (`elem` [(1, 1), (2, 2), (0, 0)])
        , testCond "inside formula1" $ inside (-1, 3) (\(_, y) -> y > 0)
        , testCond "inside formula2" $ not $ inside (1, 5) (\(x, y) -> x > 0 && y < 0)
        , testCond "inside line" $ inside (2.7, 9.1) (\(x, y) -> y == 3 * x + 1)
        ]

testFromPoints :: TestData
testFromPoints =
    tests
        2
        5
        [ testCond "fromPoints ex1" $ inside (0, 0) $ fromPoints [(1, 1), (0, 0)]
        , testCond "fromPoints ex2" $ not $ inside (0, 1) $ fromPoints [(1, 1), (0, 0)]
        , testCond "fromPoints allPoints" $ all (fromPoints fewPoints) fewPoints
        , testCond "fromPoints noPoints" $ not $ any (fromPoints []) [(i, i) | i <- [0 .. 10]]
        ]
  where
    fewPoints = [(2, 3), (1, -2), (5, 0)]

testRectangle :: TestData
testRectangle =
    tests
        3
        10
        [ testCond "rectangle ex1" $ rectangle 2 2 (0, 0)
        , testCond "rectangle ex2" $ rectangle 2 2 (-1, 1)
        , testCond "rectangle ex3" $ rectangle 2 2 (1, -1)
        , testCond "rectangle ex4" $ not $ rectangle 2 2 (2, 2)
        , testCond "rectangle one" $ rectangle 1 1 (0.5, -0.5)
        , testCond "rectangle notSquare" $ rectangle 5 2 (2.5, 0)
        , testCond "rectangle close" $ not $ rectangle 3 4 (1.5, 2.1)
        , testCond "rectangle fraction" $ not $ rectangle 0.1 0.4 (0, 1)
        , testCond "rectangle large1" $ rectangle 9000 9000 (121, 144)
        , testCond "rectangle large2" $ not $ rectangle 10000 10000 (5000.5, 30)
        ]

testCircle :: TestData
testCircle =
    tests
        4
        10
        [ testCond "circle ex1" $ circle 1 (0, 0)
        , testCond "circle ex2" $ circle 1 (1, 0)
        , testCond "circle ex3" $ circle 1 (0, 1)
        , testCond "circle ex4" $ not $ circle 1 (1, 1)
        , testCond "circle onEdge" $ circle 2.5 (-2, -1.5)
        , testCond "circle small" $ not $ circle 0.2 (0, 1)
        , testCond "circle zero" $ circle 0.01 (0, 0)
        , testCond "circle large1" $ not $ circle 100 (20, 98)
        , testCond "circle large2" $ circle 100 (-30, 30)
        ]

testPlot :: TestData
testPlot =
    tests
        5
        15
        [ testVal "plot ex1" str1 $ plot 2 1 $ fromPoints [(0, 0), (1, 1)]
        , testVal "plot ex2" str2 $ plot 2 2 $ rectangle 2 2
        , testVal "plot ex3" str3 $ plot 2 2 $ circle 2
        , testVal "plot strip" str4 $ plot 5 0 $ rectangle 4 10
        , testVal "plot circle" str5 $ plot 4 3 $ circle 3
        , testVal "plot fatCircle" str6 $ plot 4 3 $ circle 4
        , testVal "plot fractional" str7 $ plot 0 3 $ circle 0.9
        , testVal "plot sin" str8 $ plot 10 2 (\(x, y) -> abs (y - sin x) < 0.5)
        , testVal "plot complex" str9 $ plot 4 4 complexRegion
        ]
  where
    str1 = "...*.\n..*..\n....."
    str2 = ".....\n.***.\n.***.\n.***.\n....."
    str3 = "..*..\n.***.\n*****\n.***.\n..*.."
    str4 = "...*****..."
    str5 = "....*....\n..*****..\n..*****..\n.*******.\n..*****..\n..*****..\n....*...."
    str6 = "..*****..\n.*******.\n.*******.\n*********\n.*******.\n.*******.\n..*****.."
    str7 = ".\n.\n.\n*\n.\n.\n."
    str8 = ".....................\n*....**....**....**..\n.*..*..*..*..*..*..*.\n..**....**....**....*\n....................."
    str9 = ".........\n.*.......\n........*\n...***...\n...***...\n...***...\n.*.....*.\n..*****..\n........."
    complexRegion p = rectangle 3.2 3.1 p || fromPoints complexPoints p
    complexPoints = [(-3, 3), (4, 2), (5, 10), (-3, -2), (3, -2), (-2, -3), (-1, -3), (0, -3), (1, -3), (2, -3)]

testPromoteUnary :: TestData
testPromoteUnary =
    tests
    6
    5
    [
        testVal "promoteUnary example" 4 $ promoteUnary (+ 1) (\(x, _) -> x) (3, 2),
        testVal "promoteUnary squared distance" 25 $ promoteUnary (\(x, y) -> x ^ 2 + y ^ 2) (\(x, y) -> (x, -y)) (3, 4),
        testVal "promoteUnary xsign" True $ promoteUnary (>= 0) fst (3, 4),
        testVal "promoteUnary manhattan distance" 7 $ promoteUnary (\(x, y) -> abs x + abs y) (\(x, y) -> (x, -y)) (3, 4),
        testVal "promoteUnary list" [4, 5, 6] $ promoteUnary (map (+1)) (\(x, y) -> [x..y]) (3, 5),
        testVal "promoteUnary list comprehension" [0, 0, 0, 1, 1, 1] $ promoteUnary (map fst) (\(x, y) -> [(e1, e2) | e1 <- [0..x], e2 <- [0..y]]) (1, 2),
        testVal "promoteUnary string" "24" $ promoteUnary (\(x, y) -> show (round x) ++ show (round y)) (\(x, y) -> (y, x)) (4, 2),
        testVal "promoteUnary iterate" 5 $ length $ promoteUnary (takeWhile ((<= 5) . snd)) (iterate $ fmap (+1)) (0, 1),
        testVal "promoteUnary map" 2 $ promoteUnary (head . map (*2)) ((:[]) . snd) (0, 1),
        testVal "promoteUnary filter" 2 $ head $ promoteUnary (filter even . map round) (\(x, y) -> [x, y]) (1, 2)
    ]

testPromoteBinary :: TestData
testPromoteBinary =
    tests
    7
    10
    [
        testVal "promoteBinary example" 5 $ promoteBinary (+) fst snd (3, 2),
        testVal "promoteBinary funcs" 6 $ promoteBinary (\f g -> f undefined * g undefined) (\(x, _) -> const x) (\(_, y) -> const y) (3, 2),
        testVal "promoteBinary lists" [0, 1, 2, 0, 1, 2, 3] $ promoteBinary (++) (\(x, _) -> [0..x]) (\(_, y) -> [0..y]) (2, 3),
        testVal "promoteBinary member" True $ promoteBinary elem fst (\(x, y) -> [y/2 .. y]) (5, 8),
        testVal "promoteBinary strings" "585858" $ concat $ take 6 $ promoteBinary alternate (repeat . show . round . fst) (repeat . show . round . snd) (5, 8),
        testVal "promoteBinary infinite" [0, 1, 2, 4] $ take 4 $ promoteBinary (++) (\(x, _) -> [0..x]) (\(_, y) -> [y..]) (2, 4),
        testVal "promoteBinary apply" 2 $ head $ promoteBinary (<*>) (\(_, y) -> [const y]) ((:[]) . fst) (4, 2),
        testVal "promoteBinary compose" 20 $ promoteBinary (.) (\(x, _) -> (*x)) (\(_, y) -> (*y)) (2, 5)  2,
        testVal "promoteBinary zip" [(0, 0), (1, 1), (2, 2), (3, 3), (4, 4)] $ promoteBinary zip (\(x, y) -> [0..x]) (\(x, y) -> [0..y]) (4, 5),
        testVal "promoteBinary fold" 14 $ foldl (+) 0 $ promoteBinary map (\(x, y) -> (+x)) (\(x, y) -> [0..y]) (2, 3)
    ]
    where
        alternate l = concatMap (\(x, y) -> [x, y]) . zip l

testComplement :: TestData
testComplement =
    tests
    8
    5
    [
        testVal "complement empty" smallEmptyImage $ plot 2 2 $ complement $ const True,
        testVal "complement full" smallFullImage $ plot 2 2 $ complement $ const False,
        testVal "complement reversed circle" smallCircleImage $ plot 3 3 $ complement $ circle 2,
        testVal "complement missing points" missingPoints $ plot 3 3 $ complement $ fromPoints [(0, 0), (1, 0), (0, 1), (1, 1)],
        testVal "complement double complement" smallEmptyImage $ plot 2 2 $ complement $ complement $ const False,
        testVal "complement small window" smallRectangle $ plot 3 3 $ complement $ rectangle 2 3,
        testVal "complement star" starImage $ plot 5 5 $ complement $ fromPoints [(-3, 0), (-2, 0), (-1, 0), (0, 0), (1, 0), (2, 0), (3, 0), (0, 1), (0, 2), (0, 3), (0, -1), (0, -2), (0, -3), (1, 1), (2, 2), (1, -1), (2, -2), (-1, 1), (-2, 2), (-1, -1), (-2, -2)],
        testVal "complement cross" crossImage $ plot 3 3 $ complement $ fromPoints [(0, 0), (0, 1), (0, 2), (0, -1), (-1, 1), (1, 1)],
        testVal "complement medium circle" circleImage $ plot 5 5 $ complement $ applyTransformation (translation 2 2) $ circle 3,
        testVal "complement big circle" bigCircleImage $ plot 7 7 $ complement $ applyTransformation (scaling 2) $ circle 2
    ]
    where
        smallEmptyImage = ".....\n.....\n.....\n.....\n....."
        smallFullImage = "*****\n*****\n*****\n*****\n*****"
        smallCircleImage = "*******\n***.***\n**...**\n*.....*\n**...**\n***.***\n*******"
        missingPoints = "*******\n*******\n***..**\n***..**\n*******\n*******\n*******"
        smallRectangle = "*******\n*******\n**...**\n**...**\n**...**\n*******\n*******"
        starImage = "***********\n***********\n*****.*****\n***.*.*.***\n****...****\n**.......**\n****...****\n***.*.*.***\n*****.*****\n***********\n***********"
        crossImage = "*******\n***.***\n**...**\n***.***\n***.***\n*******\n*******"
        circleImage = "*******.***\n*****.....*\n*****.....*\n****.......\n*****.....*\n*****.....*\n*******.***\n***********\n***********\n***********\n***********"
        bigCircleImage = "***************\n***************\n***************\n*******.*******\n*****.....*****\n****.......****\n****.......****\n***.........***\n****.......****\n****.......****\n*****.....*****\n*******.*******\n***************\n***************\n***************"

testUnion :: TestData
testUnion =
    tests
    9
    5
    [
        testVal "union example" exampleImage $ plot 2 2 $ union (circle 1) (fromPoints [(0, 0), (-2, 2), (2, -2)]),
        testVal "union overlap" overlappingCircles $ plot 4 4 $ union (applyTransformation (translation (-1) 0) (circle 2)) (applyTransformation (translation 1 0) (circle 2)),
        testVal "union no overlap"  noOverlap $ plot 5 5 $ union (applyTransformation (translation (-2) 0) (circle 1)) (applyTransformation (translation 2 0) (circle 1)),
        testVal "union includes" rectangles $ plot 4 4 $ union (rectangle 2 2) (rectangle 3 3),
        testVal "union overlap shape" rectangleCircle $ plot 4 4 $ union (circle 2) (rectangle 6 2),
        testVal "union cross" crossImage $ plot 4 4 $ union (rectangle 6 1) (rectangle 1 6),
        testVal "union letterT" letterTImage $ plot 5 5 $ union (applyTransformation (translation 0 5) (rectangle 7 2)) (rectangle 2 10),
        testVal "union orthogonal" orthogonalImage $ plot 5 5 $ union (applyTransformation (translation 0 (-5)) (rectangle 10 2)) (rectangle 2 10),
        testVal "union complement" fullImage $ plot 5 5 $ union (fromPoints [(0, 0)]) (complement $ fromPoints [(0, 0)]),
        testVal "union perfect overlap" circleImage $ plot 4 4 $ union (circle 3) (circle 3)
    ]
    where
        exampleImage = "*....\n..*..\n.***.\n..*..\n....*"
        overlappingCircles = ".........\n.........\n...*.*...\n..*****..\n.*******.\n..*****..\n...*.*...\n.........\n........."
        noOverlap = "...........\n...........\n...........\n...........\n...*...*...\n..***.***..\n...*...*...\n...........\n...........\n...........\n..........."
        rectangles = ".........\n.........\n.........\n...***...\n...***...\n...***...\n.........\n.........\n........."
        rectangleCircle = ".........\n.........\n....*....\n.*******.\n.*******.\n.*******.\n....*....\n.........\n........."
        crossImage = ".........\n....*....\n....*....\n....*....\n.*******.\n....*....\n....*....\n....*....\n........."
        letterTImage = "..*******..\n..*******..\n....***....\n....***....\n....***....\n....***....\n....***....\n....***....\n....***....\n....***....\n....***...."
        orthogonalImage = "....***....\n....***....\n....***....\n....***....\n....***....\n....***....\n....***....\n....***....\n....***....\n***********\n***********"
        fullImage = "***********\n***********\n***********\n***********\n***********\n***********\n***********\n***********\n***********\n***********\n***********"
        circleImage = ".........\n....*....\n..*****..\n..*****..\n.*******.\n..*****..\n..*****..\n....*....\n........."

testIntersection :: TestData
testIntersection =
    tests
    10
    5
    [
        testVal "intersection example" exampleImage $ plot 3 3 $ intersection (circle 1) (fromPoints [(0, 0), (-2, 2), (2, -2)]),
        testVal "intersection overlap" overlappingCircles $ plot 4 4 $ intersection (applyTransformation (translation (-1) 0) (circle 2)) (applyTransformation (translation 1 0) (circle 2)),
        testVal "intersection difference" bigOletter $ plot 4 4 $ intersection (complement $ circle 2) (circle 3),
        testVal "intersection includes" rectangles $ plot 4 4 $ intersection (rectangle 2 2) (rectangle 3 3),
        testVal "intersection complement" emptyImage $ plot 4 4 $ intersection (circle 2) (complement $ circle 2),
        testVal "intersection perfect overlap" circleImage $ plot 4 4 $ intersection (circle 3) (circle 3),
        testVal "intersection slice" rectangleCircle $ plot 4 4 $ intersection (circle 2) (rectangle 6 2),
        testVal "intersection square" squareImage $ plot 4 4 $ intersection (rectangle 6 2) (rectangle 2 6),
        testVal "intersection double intersect" threeCircles $ plot 5 5 $ intersection (circle 3) $ intersection (applyTransformation (translation 1 1) (circle 3)) (applyTransformation (translation 0 1) (circle 3)),
        testVal "intersection set intersect" pointSetIntersect $ plot 5 5 $ intersection (fromPoints [(0, 0), (0, 1), (1, 1), (2, 2), (-1, 0)]) $ fromPoints [(0, 0), (1, 3), (2, 2), (5, 3), (1, 8)]
    ]
    where
        exampleImage = ".......\n.......\n.......\n...*...\n.......\n.......\n......."
        overlappingCircles = ".........\n.........\n.........\n....*....\n...***...\n....*....\n.........\n.........\n........."
        bigOletter = ".........\n....*....\n..**.**..\n..*...*..\n.*.....*.\n..*...*..\n..**.**..\n....*....\n........."
        rectangles = ".........\n.........\n.........\n...***...\n...***...\n...***...\n.........\n.........\n........."
        emptyImage = ".........\n.........\n.........\n.........\n.........\n.........\n.........\n.........\n........."
        circleImage = ".........\n....*....\n..*****..\n..*****..\n.*******.\n..*****..\n..*****..\n....*....\n........."
        rectangleCircle = ".........\n.........\n.........\n...***...\n..*****..\n...***...\n.........\n.........\n........."
        squareImage = ".........\n.........\n.........\n...***...\n...***...\n...***...\n.........\n.........\n........."
        threeCircles = "...........\n...........\n.....*.....\n....****...\n...*****...\n....****...\n....****...\n...........\n...........\n...........\n..........."
        pointSetIntersect = "...........\n...........\n...........\n.......*...\n...........\n.....*.....\n...........\n...........\n...........\n...........\n..........."

testTranslation :: TestData
testTranslation =
    tests
        11
        5
        [ testValApprox "translation example" (0.0,0.0) $ translation 1 2 (1, 2)
        , testValApprox "translation fractional" (1.1199999,-10.91) $ translation 1.45 1.95 (2.57, -8.96)
        , testValApprox "translation simple structure" (-8.85,4.4113317) $ translation 1.45 const1  (-7.4, 4.5)
        , testValApprox "translation complex structure" (-1.468077,-3.6821027) $ translation const2 3.57  (-log (3.5), log $ cos $ log $ pi/5)
        , testValApprox "translation large1" (-656.8906,-8057.0) $ translation (func_eval 4.5) 9000 (-656, 943)
        , testValApprox "translation large2" (434.53937,-14756.565) $ translation (func_eval 4.5) 15000 (435.43, 243.435)
        ]
        where func_eval = sqrt . sum . map (sin . exp . sqrt . exp) . enumFromTo 1
              const1 = sin 3 * pi/5
              const2 = cos 7 * pi/11

testScaling :: TestData
testScaling =
    tests
        12
        5
        [ testValApprox "scaling example" (1.0,1.0) $ scaling 2 (2, 2)
        , testValApprox "scaling fractional" (1.3179487,-4.5948715) $ scaling 1.95 (2.57, -8.96)
        , testValApprox "scaling simple structure" (17.194738,-10.45626) $ scaling const1 (-7.4, 4.5)
        , testValApprox "scaling complex structure" (83.76407,-2.2026427e-2) $ scaling const2 (-log (2.5e-4), log $ cos $ log $ pi / (tan pi/4))
        , testValApprox "scaling large1" (-421.8851,5788.058) $ scaling (func_eval 2.56) (-656, 9000)
        , testValApprox "scaling large2" (-7093.0063,268.32132) $ scaling (func_eval 5.65) (-15000, 567.435)
        ]
        where func_eval = sqrt . sum . map (cos . sin . exp . sqrt . exp) . enumFromTo 1
              const1 = sin 5 * pi/7
              const2 = cos 5 * pi/9

testApplyTransformation :: TestData
testApplyTransformation =
    tests
        13
        5
        [ testVal "applyTransformation example" str1 $ plot 2 2 $ applyTransformation (translation 1 0) (circle 2)
        , testVal "applyTransformation simple1" str2 $ plot 2 2 $ applyTransformation (translation 2 0) (rectangle 3 5)
        , testVal "applyTransformation simple2" str3 $ plot 3 5 $ applyTransformation (translation 2 1) (circle 3)
        , testVal "applyTransformation strip" str4 $ plot 5 3 $ applyTransformation (translation 1 1) (rectangle 3 9)
        , testVal "applyTransformation circle" str5 $ plot 4 3 $ applyTransformation (translation 3 2) (circle 5.6)
        , testVal "applyTransformation cos" str6 $ plot 2 4 $ applyTransformation (translation 2 1) (\(x, y) -> abs (log y - cos x) < pi / 2)
        , testVal "applyTransformation fractional" str7 $ plot 0 3 $ applyTransformation (translation 2 1) (circle 2.5)
        , testVal "applyTransformation large1" str8 $ plot 10 4 $ applyTransformation (translation 2 3) (\(x, y) -> abs (func_eval y - exp x) < log 7)
        , testVal "applyTransformation large2" str9 $ plot 8 4 $ applyTransformation (translation 2 3) complexRegion
        ]
  where
    str1 = "...*.\n..***\n.****\n..***\n...*."
    str2 = "...**\n...**\n...**\n...**\n...**"
    str3 = ".......\n.....*.\n...****\n...****\n..*****\n...****\n...****\n.....*.\n.......\n.......\n......."
    str4 = ".....***...\n.....***...\n.....***...\n.....***...\n.....***...\n.....***...\n.....***..."
    str5 = "..*******\n..*******\n..*******\n..*******\n...******\n....*****\n.....****"
    str6 = "..***\n*.***\n*****\n.....\n.....\n.....\n.....\n.....\n....."
    str7 = ".\n*\n*\n*\n.\n.\n."
    str8 = "*************........\n*************........\n*************........\n*************........\n*************........\n*************........\n*************........\n*************........\n*************........"
    str9 = ".........***.....\n.........***.....\n.........***.....\n.......*.........\n........*.*.*....\n.................\n.................\n.................\n................."
    func_eval = sqrt . sum . map (cos . sin . exp . sqrt . exp . exp) . enumFromTo 1
    complexRegion p = rectangle 3.7 3.5 p || fromPoints complexPoints p
    complexPoints = [(-3, 3.4), (4, 2), (5, 10), (-3, -2), (3, -2.9), (-2, -3), (-1, -3.4), (0, -3), (1.8, -3), (2, -3)]

testCombineTransformations :: TestData
testCombineTransformations =
    tests
        14
        10
        [ testVal "combineTransformations example" str1 $ plot 2 2 $ applyTransformation
            (combineTransformations [scaling 0.5, translation 1 0]) (circle 2)
        , testVal "combineTransformations simple1" str2 $ plot 2 2 $ applyTransformation
            (combineTransformations [scaling 0.5, translation 1 0, scaling 2.7]) (rectangle 3 5)
        , testVal "combineTransformations simple2" str3 $ plot 10 4 $ applyTransformation
            (combineTransformations [scaling 0.5, translation 5 0, scaling 2.2, translation 1 2, scaling 4.5]) (circle 1)
        , testVal "combineTransformations strip" str4 $ plot 5 3 $ applyTransformation
            (combineTransformations [scaling 0.2, translation 1.5 0, scaling 4.5, translation 3 2, scaling 1.5]) (rectangle 3 9)
        , testVal "combineTransformations circle" str5 $ plot 4 3 $ applyTransformation
            (combineTransformations [scaling 0.8, translation 1.4 1.1, scaling 5, translation 1.8 2, scaling 2.5]) (circle 5.6)
        , testVal "combineTransformations sin-cos" str6 $ plot 10 4 $ applyTransformation
            (combineTransformations [scaling 0.5, translation 1 0, scaling 2.7, translation 1 2, scaling 4.5, scaling 2.7, translation 1 2]) (\(x, y) -> abs (sin y - cos x) < pi / 2)
        , testVal "combineTransformations fractional" str7 $ plot 0 3 $ applyTransformation
            (combineTransformations [scaling 0.5, translation 1 0, scaling 2.7, translation 1 2, scaling 4.5, scaling 1.7, translation 1.4 2]) (circle 2.5)
        , testVal "combineTransformations large1" str8 $ plot 10 4 $ applyTransformation
            (combineTransformations [scaling 0.5, translation (-1) 2, scaling 1.7, translation 1 2, scaling 2.5]) (\(x, y) -> abs (func_eval y - exp x) < log 7)
        , testVal "combineTransformations large2" str9 $ plot 8 4 $ applyTransformation
            (combineTransformations [scaling 0.5, translation 1 0, scaling 2.7, translation 1 2, scaling 4.5]) complexRegion
        ]
  where
    str1 = ".....\n.....\n..**.\n.....\n....."
    str2 = ".****\n.****\n.****\n.****\n.****"
    str3 = ".........**********..\n.........**********..\n.........**********..\n.........**********..\n..........*********..\n..........********...\n...........******....\n.....................\n....................."
    str4 = "......*****\n......*****\n......*****\n......*****\n......*****\n......*****\n......*****"
    str5 = "*********\n*********\n*********\n*********\n*********\n*********\n*********"
    str6 = "**************.......\n***************......\n***************......\n****************.....\n*****************....\n******************...\n*******************..\n********************.\n*********************"
    str7 = ".\n.\n.\n.\n.\n.\n."
    str8 = "*************........\n************.........\n************.........\n************.........\n************.........\n************.........\n************.........\n************.........\n************........."
    str9 = "*****************\n*****************\n*****************\n*****************\n*****************\n*****************\n*****************\n*****************\n*****************"
    func_eval = sqrt . sum . map (cos . sin . exp . sqrt . exp . exp . sqrt) . enumFromTo 1
    complexRegion p = rectangle 3.7 3.5 p || fromPoints complexPoints p
    complexPoints = [(-3, 3.4), (4, 2), (5, 10), (-3, -2), (3, -2.9), (-2, -3), (-1, -3.4), (0, -3), (1.8, -3), (2, -3)]

testBfs :: TestData
testBfs =
    tests
    15
    10
    [
        testVal "bfs node 1 tree" bfsNode1Tree (take 10 $ bfs 1 expandTree),
        testVal "bfs node 5 tree" bfsNode5Tree (take 10 $ bfs 5 expandTree),
        testVal "bfs node 1 graph" bfsNode1Graph (take 15 $ bfs 1 expandGraph),
        testVal "bfs node 5 graph" bfsNode5Graph (take 15 $ bfs 5 expandGraph),
        testVal "bfs node 15 graph" bfsNode15Graph (take 15 $ bfs 15 expandGraph)
    ]
    where
        adjacentNodes :: Eq a => a -> [(a, a)] -> [a]
        adjacentNodes x edges = [y | (a, b) <- edges, (a == x && b /= x) || (b == x && a /= x), y <- [a, b], y /= x]
        tree = [(1, 2), (1, 3), (2, 4), (2, 5), (3, 6), (3, 7), (4, 8), (5, 9), (6, 10)]
        expandTree = flip adjacentNodes tree
        bfsNode1Tree = [(1, 0), (2, 1), (3, 1), (4, 2), (5, 2), (6, 2), (7, 2), (8, 3), (9, 3), (10, 3)]
        bfsNode5Tree = [(5, 0), (2, 1), (9, 1), (1, 2), (4, 2), (3, 3), (8, 3), (6, 4), (7, 4), (10, 5)]
        graph = [(1, 2), (2, 3), (3, 4), (4, 1),   -- Cycle 1
                (5, 6), (6, 7), (7, 8), (8, 5), -- Cycle 2
                (9, 10), (10, 11), (11, 12), (12, 9), -- Cycle 3
                (1, 5), (3, 9), (13, 14), (14, 15), (15, 13), -- Additional edges
                (2, 6), (7, 11), (11, 13)] -- Additional edges for connectivity
        expandGraph = flip adjacentNodes graph
        bfsNode1Graph = [(1, 0), (2, 1), (4, 1), (5, 1), (3, 2), (6, 2), (8, 2), (9, 3), (7, 3), (10, 4), (12, 4), (11, 4), (13, 5), (14, 6), (15, 6)]
        bfsNode5Graph = [(5, 0), (6, 1), (8, 1), (1, 1), (7, 2), (2, 2), (4, 2), (11, 3), (3, 3), (10, 4), (12, 4), (13, 4), (9, 4), (14, 5), (15, 5)]
        bfsNode15Graph = [(15, 0), (14, 1), (13, 1), (11, 2), (10, 3), (12, 3), (7, 3), (9, 4), (6, 4), (8, 4), (3, 5), (5, 5), (2, 5), (4, 6), (1, 6)]

testRegionAvoidingBfs :: TestData
testRegionAvoidingBfs =
    tests
    16
    10
    [
        testVal "regionAvoidingBfs circle" circleValue (lookup (-5, 1) $ regionAvoidingBfs (4, 2) $ circle 4),
        testVal "regionAvoidingBfs circles" circlesValue (lookup (3, 0) $ regionAvoidingBfs (-3, 0) $ circles 3),
        testVal "regionAvoidingBfs square" squareValue (lookup (-6, -3) $ regionAvoidingBfs (6, 3) $ rectangle 4 4),
        testVal "regionAvoidingBfs rectangle" rectangleValue (lookup (-7, 2) $ regionAvoidingBfs (7, 2) $ rectangle 5 3),
        testVal "regionAvoidingBfs rectangles" rectanglesValue (lookup (-9, 0) $ regionAvoidingBfs (9, 0) $ rectangles 5)
    ]
    where
        circleValue = Just 16
        circlesValue = Just 12
        squareValue = Just 18
        rectangleValue = Just 14
        rectanglesValue = Just 22
        rectangles n
            | n <= 0    = const False
            | otherwise = rectangle 2 2 `union` applyTransformation (translation 6 0) (rectangles (n - 1))

main :: IO ()
main =
    vmCheck
        [ testInside
        , testFromPoints
        , testRectangle
        , testCircle
        , testPlot
        , testPromoteUnary
        , testPromoteBinary
        , testComplement
        , testUnion
        , testIntersection
        , testTranslation
        , testScaling
        , testApplyTransformation
        , testCombineTransformations
        , testBfs
        , testRegionAvoidingBfs
        ]