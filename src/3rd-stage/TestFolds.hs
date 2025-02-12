module TestFolds where

import Folds
    ( TransformationAST(T),
      RegionAST(..),
      TransformationShape(Combine, Translation, Scaling),
      RegionShape(Union, Circle, Intersection, Rectangle, Complement,
                  Transform, FromPoints),
      fromPoints,
      rectangle,
      circle,
      complement,
      union,
      intersection,
      translation,
      scaling,
      combineTransformations,
      applyTransformation,
      foldTransformationAST,
      foldRegionAST,
      showFoldFlag, TransformationCombiner, RegionCombiner, toTransformation, basicTransformationCount, basicEntityCount )
import TestPP ( testCond, testVal, testValApprox, tests, vmCheck, TestData )
import qualified Shallow as S

-- Helpers functions below.

labelFolder :: TransformationCombiner String
labelFolder (Translation _ _) = "t"
labelFolder (Scaling _) = "s"
labelFolder (Combine labels) = concat labels

regionPrintFolder :: RegionCombiner String
regionPrintFolder (FromPoints _) = "p"
regionPrintFolder (Rectangle _ _) = "r"
regionPrintFolder (Circle _) = "c"
regionPrintFolder (Complement s) = "~" ++ s
regionPrintFolder (Union a b) = a ++ "U" ++ b
regionPrintFolder (Intersection a b) = a ++ "&" ++ b
regionPrintFolder (Transform _ a) = a ++ "@"

depthFolder :: RegionCombiner Int
depthFolder (Complement d) = 1 + d
depthFolder (Union a b) = 1 + max a b
depthFolder (Intersection a b) = 1 + max a b
depthFolder (Transform _ d) = 1 + d
depthFolder _ = 1

testShowTransformationAST :: TestData
testShowTransformationAST =
    tests
        1
        10
        [ testVal "showTransformationAST ex" str0 $ show $ combineTransformations ex_transf
        ,testVal "showTransformationAST simple1" str1 $ show $ combineTransformations simple1_transf
        ,testVal "showTransformationAST simple2" str2 $ show $ combineTransformations simple2_transf
        ,testVal "showTransformationAST simple3" str3 $ show $ combineTransformations simple3_transf
        ,testVal "showTransformationAST complex1" str4 $ show $ combineTransformations complex1_transf
        ,testVal "showTransformationAST complex2" str5 $ show $ combineTransformations complex2_transf
        ,testVal "showTransformationAST complex3" str6 $ show $ combineTransformations complex3_transf
        ]
        where
            ex_transf = [translation 1 2, scaling 3, combineTransformations [scaling 4, translation 5 6]]
            str0 = "[+(1.0,2.0),*<3.0>,[*<4.0>,+(5.0,6.0)]]"

            simple1_transf = [translation 1 2, scaling 3, combineTransformations [scaling 4, translation 5 6, combineTransformations [translation 7 8, scaling 9, combineTransformations [scaling 10, translation 11 12]]]]
            str1 = "[+(1.0,2.0),*<3.0>,[*<4.0>,+(5.0,6.0),[+(7.0,8.0),*<9.0>,[*<10.0>,+(11.0,12.0)]]]]"

            simple2_transf = [translation 1.5 2.7, scaling 3.2, combineTransformations [scaling 4.6, translation 5.1 6.3, combineTransformations [translation 7.9 8.2, scaling 9.5, combineTransformations [scaling 10.7, translation 11.4 12.8, combineTransformations [translation 13.6 14.1, scaling 15.3, combineTransformations [scaling 16.9, translation 17.2 18.7]]]]]]
            str2 = "[+(1.5,2.7),*<3.2>,[*<4.6>,+(5.1,6.3),[+(7.9,8.2),*<9.5>,[*<10.7>,+(11.4,12.8),[+(13.6,14.1),*<15.3>,[*<16.9>,+(17.2,18.7)]]]]]]"

            simple3_transf = [translation 19.1 20.2, scaling 21.3, combineTransformations [scaling 22.4, translation 23.5 24.6, combineTransformations [translation 25.7 26.8, scaling 27.9, combineTransformations [scaling 28.1, translation 29.2 30.3, combineTransformations [translation 31.4 32.5, scaling 33.6, combineTransformations [scaling 34.7, translation 35.8 36.9]]]]]]
            str3 = "[+(19.1,20.2),*<21.3>,[*<22.4>,+(23.5,24.6),[+(25.7,26.8),*<27.9>,[*<28.1>,+(29.2,30.3),[+(31.4,32.5),*<33.6>,[*<34.7>,+(35.8,36.9)]]]]]]"

            complex1_transf = [combineTransformations [translation 1.1 2.2, scaling 3.3, combineTransformations [scaling 4.4, translation 5.5 6.6, combineTransformations [translation 7.7 8.8, scaling 9.9, combineTransformations [scaling 10.1, translation 11.2 12.3, combineTransformations [translation 13.4 14.5, scaling 15.6, combineTransformations [scaling 16.7, translation 17.8 18.9]]]]]]]
            str4 = "[+(1.1,2.2),*<3.3>,[*<4.4>,+(5.5,6.6),[+(7.7,8.8),*<9.9>,[*<10.1>,+(11.2,12.3),[+(13.4,14.5),*<15.6>,[*<16.7>,+(17.8,18.9)]]]]]]"

            complex2_transf = [combineTransformations [translation 19.1 20.2, scaling 21.3, combineTransformations [scaling 22.4, translation 23.5 24.6, combineTransformations [translation 25.7 26.8, scaling 27.9, combineTransformations [scaling 28.1, translation 29.2 30.3, combineTransformations [translation 31.4 32.5, scaling 33.6, combineTransformations [scaling 34.7, translation 35.8 36.9, combineTransformations [translation 37.1 38.2, scaling 39.3, combineTransformations [scaling 40.4, translation 41.5 42.6, combineTransformations [translation 43.7 44.8, scaling 45.9]]]]]]]]]]
            str5 = "[+(19.1,20.2),*<21.3>,[*<22.4>,+(23.5,24.6),[+(25.7,26.8),*<27.9>,[*<28.1>,+(29.2,30.3),[+(31.4,32.5),*<33.6>,[*<34.7>,+(35.8,36.9),[+(37.1,38.2),*<39.3>,[*<40.4>,+(41.5,42.6),[+(43.7,44.8),*<45.9>]]]]]]]]]"

            complex3_transf = [combineTransformations [translation 1.1 2.2, scaling 3.3, combineTransformations [scaling 4.4, translation 5.5 6.6, combineTransformations [translation 7.7 8.8, scaling 9.9, combineTransformations [scaling 10.1, translation 11.2 12.3, combineTransformations [translation 13.4 14.5, scaling 15.6, combineTransformations [scaling 16.7, translation 17.8 18.9, combineTransformations [translation 19.1 20.2, scaling 21.3, combineTransformations [scaling 22.4, translation 23.5 24.6, combineTransformations [translation 25.7 26.8, scaling 27.9, combineTransformations [scaling 28.1, translation 29.2 30.3, combineTransformations [translation 31.4 32.5, scaling 33.6, combineTransformations [scaling 34.7, translation 35.8 36.9, combineTransformations [translation 37.1 38.2, scaling 39.3, combineTransformations [scaling 40.4, translation 41.5 42.6, combineTransformations [translation 43.7 44.8, scaling 45.9, combineTransformations [scaling 46.1, translation 47.2 48.3, combineTransformations [translation 49.4 50.5, scaling 51.6, combineTransformations [scaling 52.7, translation 53.8 54.9, combineTransformations [translation 55.0 56.1, scaling 57.2]]]]]]]]]]]]]]]]]]]]
            str6 = "[+(1.1,2.2),*<3.3>,[*<4.4>,+(5.5,6.6),[+(7.7,8.8),*<9.9>,[*<10.1>,+(11.2,12.3),[+(13.4,14.5),*<15.6>,[*<16.7>,+(17.8,18.9),[+(19.1,20.2),*<21.3>,[*<22.4>,+(23.5,24.6),[+(25.7,26.8),*<27.9>,[*<28.1>,+(29.2,30.3),[+(31.4,32.5),*<33.6>,[*<34.7>,+(35.8,36.9),[+(37.1,38.2),*<39.3>,[*<40.4>,+(41.5,42.6),[+(43.7,44.8),*<45.9>,[*<46.1>,+(47.2,48.3),[+(49.4,50.5),*<51.6>,[*<52.7>,+(53.8,54.9),[+(55.0,56.1),*<57.2>]]]]]]]]]]]]]]]]]]]"

testShowRegionAST :: TestData
testShowRegionAST =
    tests
        2
        20
        [ testVal "showRegionAST ex" str0 $ show $ ex_reg
        ,testVal "showRegionAST simple1" str1 $ show $ simple1_reg
        ,testVal "showRegionAST simple2" str2 $ show $ simple2_reg
        ,testVal "showRegionAST simple3" str3 $ show $ simple3_reg
        ,testVal "showRegionAST complex1" str4 $ show $ complex1_reg
        ,testVal "showRegionAST complex2" str5 $ show $ complex2_reg
        ,testVal "showRegionAST complex3" str6 $ show $ complex3_reg
        ,testVal "showRegionAST complex4" str7 $ show $ complex4_reg
        ,testVal "showRegionAST complex5" str8 $ show $ complex5_reg
        ]
        where
            ex_reg = union (complement (fromPoints [(1, 1)])) (intersection (applyTransformation (translation 1 2) (circle 3)) (rectangle 4 5))
            str0 = "+\n  ~\n    FromPoints [(1.0,1.0)]\n  *\n    +(1.0,2.0)\n      Circle 3.0\n    Rectangle 4.0 5.0"

            simple1_reg = union (complement (fromPoints [(2.2, 3.3)])) (intersection (applyTransformation (scaling 2.5) (circle 4.4)) (rectangle 5.5 6.6))
            str1 = "+\n  ~\n    FromPoints [(2.2,3.3)]\n  *\n    *<2.5>\n      Circle 4.4\n    Rectangle 5.5 6.6"

            simple2_reg = union (complement (fromPoints [(3.3, 4.4)])) (intersection (applyTransformation (combineTransformations [translation 3.5 4.5, scaling 5.5]) (circle 6.6)) (rectangle 7.7 8.8))
            str2 = "+\n  ~\n    FromPoints [(3.3,4.4)]\n  *\n    [+(3.5,4.5),*<5.5>]\n      Circle 6.6\n    Rectangle 7.7 8.8"

            simple3_reg = union (complement (fromPoints [(4.4, 5.5)])) (intersection (applyTransformation (combineTransformations [scaling 4.5, translation 5.5 6.5]) (circle 7.7)) (rectangle 8.8 9.9))
            str3 = "+\n  ~\n    FromPoints [(4.4,5.5)]\n  *\n    [*<4.5>,+(5.5,6.5)]\n      Circle 7.7\n    Rectangle 8.8 9.9"

            complex1_reg = union (complement (fromPoints [(1.1, 2.2)])) (intersection (applyTransformation (combineTransformations [scaling 1.1, translation 2.2 3.3, scaling 4.4, translation 5.5 6.6, scaling 7.7, translation 8.8 9.9, scaling 10.1, translation 11.1 12.1, scaling 13.1, translation 14.1 15.1]) (circle 16.1)) (rectangle 17.1 18.1))
            str4 = "+\n  ~\n    FromPoints [(1.1,2.2)]\n  *\n    [*<1.1>,+(2.2,3.3),*<4.4>,+(5.5,6.6),*<7.7>,+(8.8,9.9),*<10.1>,+(11.1,12.1),*<13.1>,+(14.1,15.1)]\n      Circle 16.1\n    Rectangle 17.1 18.1"

            complex2_reg = union (complement (fromPoints [(2.2, 3.3)])) (intersection (applyTransformation (combineTransformations [scaling 2.2, translation 3.3 4.4, scaling 5.5, combineTransformations [translation 6.6 7.7, scaling 8.8, combineTransformations [translation 9.9 10.1, scaling 11.1, translation 12.1 13.1], scaling 14.1], translation 15.1 16.1]) (circle 17.1)) (rectangle 18.1 19.1))
            str5 = "+\n  ~\n    FromPoints [(2.2,3.3)]\n  *\n    [*<2.2>,+(3.3,4.4),*<5.5>,[+(6.6,7.7),*<8.8>,[+(9.9,10.1),*<11.1>,+(12.1,13.1)],*<14.1>],+(15.1,16.1)]\n      Circle 17.1\n    Rectangle 18.1 19.1"

            complex3_reg = union (complement (fromPoints [(3.3, 4.4)])) (intersection (applyTransformation (combineTransformations [scaling 3.3, translation 4.4 5.5, scaling 6.6, combineTransformations [translation 7.7 8.8, scaling 9.9, combineTransformations [translation 10.1 11.1, scaling 12.1, combineTransformations [translation 13.1 14.1, scaling 15.1, combineTransformations [translation 16.1 17.1, scaling 18.1, combineTransformations [translation 19.1 20.1, scaling 21.1, translation 22.1 23.1]]]]]]) (applyTransformation (scaling 9.9) (circle 10.1))) (rectangle 25.1 26.1))
            str6 = "+\n  ~\n    FromPoints [(3.3,4.4)]\n  *\n    [*<3.3>,+(4.4,5.5),*<6.6>,[+(7.7,8.8),*<9.9>,[+(10.1,11.1),*<12.1>,[+(13.1,14.1),*<15.1>,[+(16.1,17.1),*<18.1>,[+(19.1,20.1),*<21.1>,+(22.1,23.1)]]]]]]\n      *<9.9>\n        Circle 10.1\n    Rectangle 25.1 26.1"

            complex4_reg = intersection (intersection (intersection (union (applyTransformation (combineTransformations [scaling 1.1, translation 2.2 3.3, scaling 4.4, combineTransformations [translation 5.5 6.6, scaling 7.7, combineTransformations [translation 8.8 9.9, scaling 10.1, combineTransformations [translation 11.1 12.1, scaling 13.1, combineTransformations [translation 14.1 15.1, scaling 16.1, translation 17.1 18.1]]]]]) (circle 19.1)) (rectangle 20.1 21.1)) (applyTransformation (scaling 22.1) (circle 23.1))) (rectangle 24.1 25.1)) (applyTransformation (scaling 26.1) (circle 27.1))
            str7 = "*\n  *\n    *\n      +\n        [*<1.1>,+(2.2,3.3),*<4.4>,[+(5.5,6.6),*<7.7>,[+(8.8,9.9),*<10.1>,[+(11.1,12.1),*<13.1>,[+(14.1,15.1),*<16.1>,+(17.1,18.1)]]]]]\n          Circle 19.1\n        Rectangle 20.1 21.1\n      *<22.1>\n        Circle 23.1\n    Rectangle 24.1 25.1\n  *<26.1>\n    Circle 27.1"

            complex5_reg = complement (complement (complement (applyTransformation (combineTransformations [scaling 1.1, translation 2.2 3.3, scaling 4.4, combineTransformations [translation 5.5 6.6, scaling 7.7, combineTransformations [translation 8.8 9.9, scaling 10.1, combineTransformations [translation 11.1 12.1, scaling 13.1, combineTransformations [translation 14.1 15.1, scaling 16.1, translation 17.1 18.1, combineTransformations [translation 19.1 20.1, scaling 21.1, translation 22.1 23.1, combineTransformations [translation 24.1 25.1, scaling 26.1, translation 27.1 28.1]]]]]]]) (union (applyTransformation (scaling 29.1) (circle 30.1)) (applyTransformation (translation 31.1 32.1) (rectangle 33.1 34.1))))))
            str8 = "~\n  ~\n    ~\n      [*<1.1>,+(2.2,3.3),*<4.4>,[+(5.5,6.6),*<7.7>,[+(8.8,9.9),*<10.1>,[+(11.1,12.1),*<13.1>,[+(14.1,15.1),*<16.1>,+(17.1,18.1),[+(19.1,20.1),*<21.1>,+(22.1,23.1),[+(24.1,25.1),*<26.1>,+(27.1,28.1)]]]]]]]\n        +\n          *<29.1>\n            Circle 30.1\n          +(31.1,32.1)\n            Rectangle 33.1 34.1"

testNumRegionAST :: TestData
testNumRegionAST =
    tests
        3
        10
        [ testVal "numRegionAST ex1" str0 $ show (fromInteger 1 :: RegionAST)
        ,testVal "numRegionAST ex2" str1 $ show (1 :: RegionAST)
        ,testVal "numRegionAST ex3" str2 $ show (fromInteger (-1) :: RegionAST)
        ,testVal "numRegionAST ex4" str3 $ show (-1 :: RegionAST)
        ,testVal "numRegionAST ex5" str4 $ show (circle 3 - rectangle 4 5)
        ,testVal "numRegionAST ex6" str5 $ show (-1 + applyTransformation (translation 1 2) (circle 3) * rectangle 4 5)
        ,testVal "numRegionAST simple1" str6 $ show (-1 + ((-) (fromInteger 1)) ((*) (applyTransformation (translation 1 2) (circle 3)) (rectangle 4 5)))
        ,testVal "numRegionAST simple2" str7 $ show (-2 + ((-) (fromInteger (-2))) ((*) (applyTransformation (combineTransformations [translation 3.5 4.5, scaling 5.5]) (circle 6.6)) (rectangle 7.7 8.8)))
        ,testVal "numRegionAST simple3" str8 $ show (-3 + ((-) (fromInteger 3)) ((*) (applyTransformation (combineTransformations [scaling 4.5, translation 5.5 6.5]) (circle 7.7)) (rectangle 8.8 9.9)))
        ,testVal "numRegionAST simple4" str9 $ show (-4 + ((-) (fromInteger (-4))) ((*) (applyTransformation (combineTransformations [scaling 1.1, translation 2.2 3.3, scaling 4.4, translation 5.5 6.6, scaling 7.7, translation 8.8 9.9, scaling 10.1, translation 11.1 12.1, scaling 13.1, translation 14.1 15.1]) (circle 16.1)) (rectangle 17.1 18.1)))
        ,testVal "numRegionAST complex1" str10 $ show (- (- (- (applyTransformation (combineTransformations [scaling 1.1, translation 2.2 3.3, scaling 4.4, combineTransformations [translation 5.5 6.6, scaling 7.7, combineTransformations [translation 8.8 9.9, scaling 10.1, combineTransformations [translation 11.1 12.1, scaling 13.1, combineTransformations [translation 14.1 15.1, scaling 16.1, translation 17.1 18.1, combineTransformations [translation 19.1 20.1, scaling 21.1, translation 22.1 23.1, combineTransformations [translation 24.1 25.1, scaling 26.1, translation 27.1 28.1]]]]]]]) (applyTransformation (scaling 29.1) (circle 30.1) + applyTransformation (translation 31.1 32.1) (rectangle 33.1 34.1))))))
        ,testVal "numRegionAST complex2" str11 $ show ((applyTransformation (combineTransformations [scaling 1.1, translation 2.2 3.3, scaling 4.4, combineTransformations [translation 5.5 6.6, scaling 7.7, combineTransformations [translation 8.8 9.9, scaling 10.1, combineTransformations [translation 11.1 12.1, scaling 13.1, combineTransformations [translation 14.1 15.1, scaling 16.1, translation 17.1 18.1]]]]]) (circle 19.1) + rectangle 20.1 21.1) * applyTransformation (scaling 22.1) (circle 23.1) * rectangle 24.1 25.1 * applyTransformation (scaling 26.1) (circle 27.1))
        ,testVal "numRegionAST complex3" str12 $ show (-(fromPoints [(3.3, 4.4)]) + (applyTransformation (combineTransformations [scaling 3.3, translation 4.4 5.5, scaling 6.6, combineTransformations [translation 7.7 8.8, scaling 9.9, combineTransformations [translation 10.1 11.1, scaling 12.1, combineTransformations [translation 13.1 14.1, scaling 15.1, combineTransformations [translation 16.1 17.1, scaling 18.1, combineTransformations [translation 19.1 20.1, scaling 21.1, translation 22.1 23.1]]]]]]) (applyTransformation (scaling 9.9) (circle 10.1)) * rectangle 25.1 26.1))
        ,testVal "numRegionAST complex4" str13 $ show (-(fromPoints [(2.2, 3.3)]) + (applyTransformation (combineTransformations [scaling 2.2, translation 3.3 4.4, scaling 5.5, combineTransformations [translation 6.6 7.7, scaling 8.8, combineTransformations [translation 9.9 10.1, scaling 11.1, translation 12.1 13.1], scaling 14.1], translation 15.1 16.1]) (circle 17.1) * rectangle 18.1 19.1))
        ]
        where
            str0 = "FromPoints [(1.0,1.0)]"
            str1 = "FromPoints [(1.0,1.0)]"
            str2 = "FromPoints [(-1.0,-1.0)]"
            str3 = "~\n  FromPoints [(1.0,1.0)]"
            str4 = "*\n  Circle 3.0\n  ~\n    Rectangle 4.0 5.0"
            str5 = "+\n  ~\n    FromPoints [(1.0,1.0)]\n  *\n    +(1.0,2.0)\n      Circle 3.0\n    Rectangle 4.0 5.0"
            str6 = "+\n  ~\n    FromPoints [(1.0,1.0)]\n  *\n    FromPoints [(1.0,1.0)]\n    ~\n      *\n        +(1.0,2.0)\n          Circle 3.0\n        Rectangle 4.0 5.0"
            str7 = "+\n  ~\n    FromPoints [(2.0,2.0)]\n  *\n    FromPoints [(-2.0,-2.0)]\n    ~\n      *\n        [+(3.5,4.5),*<5.5>]\n          Circle 6.6\n        Rectangle 7.7 8.8"
            str8 = "+\n  ~\n    FromPoints [(3.0,3.0)]\n  *\n    FromPoints [(3.0,3.0)]\n    ~\n      *\n        [*<4.5>,+(5.5,6.5)]\n          Circle 7.7\n        Rectangle 8.8 9.9"
            str9 = "+\n  ~\n    FromPoints [(4.0,4.0)]\n  *\n    FromPoints [(-4.0,-4.0)]\n    ~\n      *\n        [*<1.1>,+(2.2,3.3),*<4.4>,+(5.5,6.6),*<7.7>,+(8.8,9.9),*<10.1>,+(11.1,12.1),*<13.1>,+(14.1,15.1)]\n          Circle 16.1\n        Rectangle 17.1 18.1"
            str10 = "~\n  ~\n    ~\n      [*<1.1>,+(2.2,3.3),*<4.4>,[+(5.5,6.6),*<7.7>,[+(8.8,9.9),*<10.1>,[+(11.1,12.1),*<13.1>,[+(14.1,15.1),*<16.1>,+(17.1,18.1),[+(19.1,20.1),*<21.1>,+(22.1,23.1),[+(24.1,25.1),*<26.1>,+(27.1,28.1)]]]]]]]\n        +\n          *<29.1>\n            Circle 30.1\n          +(31.1,32.1)\n            Rectangle 33.1 34.1"
            str11 = "*\n  *\n    *\n      +\n        [*<1.1>,+(2.2,3.3),*<4.4>,[+(5.5,6.6),*<7.7>,[+(8.8,9.9),*<10.1>,[+(11.1,12.1),*<13.1>,[+(14.1,15.1),*<16.1>,+(17.1,18.1)]]]]]\n          Circle 19.1\n        Rectangle 20.1 21.1\n      *<22.1>\n        Circle 23.1\n    Rectangle 24.1 25.1\n  *<26.1>\n    Circle 27.1"
            str12 = "+\n  ~\n    FromPoints [(3.3,4.4)]\n  *\n    [*<3.3>,+(4.4,5.5),*<6.6>,[+(7.7,8.8),*<9.9>,[+(10.1,11.1),*<12.1>,[+(13.1,14.1),*<15.1>,[+(16.1,17.1),*<18.1>,[+(19.1,20.1),*<21.1>,+(22.1,23.1)]]]]]]\n      *<9.9>\n        Circle 10.1\n    Rectangle 25.1 26.1"
            str13 = "+\n  ~\n    FromPoints [(2.2,3.3)]\n  *\n    [*<2.2>,+(3.3,4.4),*<5.5>,[+(6.6,7.7),*<8.8>,[+(9.9,10.1),*<11.1>,+(12.1,13.1)],*<14.1>],+(15.1,16.1)]\n      Circle 17.1\n    Rectangle 18.1 19.1"

testFunctorTransformationShape :: TestData
testFunctorTransformationShape =
    tests
        4
        10
        [
            testVal "Functor TransformationShape ex1" (Translation 1 2) ((+1) <$> Translation 1 2),
            testVal "Functor TransformationShape ex2" (Scaling 3) ((+1) <$> Scaling 3),
            testVal "Functor TransformationShape ex3" (Combine [2, 3, 4]) ((+1) <$> Combine [1, 2, 3]),
            testVal "Functor TransformationShape basic" (Combine [scaling 3]) (combineTransformations . (:[]) <$> Combine [scaling 3]),
            testVal "Functor TransformationShape strings" (Combine ["aa", "bb"]) (concatMap head . replicate 2 <$> Combine [["a"], ["b"]]),
            testVal "Functor TransformationShape medium" (Combine [T $ Combine [scaling 3, scaling 2]]) (combineTransformations . (:[scaling 2]) <$> Combine [scaling 3]),
            testVal "Functor transformationShape compose" (Combine [3, 5, 7]) ((+1) . (*2) <$> Combine [1, 2, 3]),
            testVal "Functor TransformationShape equational" (Combine [3, 5, 7]) ((+1) <$> (*2) <$> Combine [1, 2, 3]),
            testVal "Functor TransformationShape types" (Combine ["1", "2", "3"]) (show <$> Combine [1, 2, 3]),
            testVal "Functor TransformationShape complex" (Combine [T $ Combine [scaling 6, scaling 4]]) (Combine $ (:[]) $ T (scaling <$> Combine [6, 4]))
        ]

testFunctorRegionShape :: TestData
testFunctorRegionShape =
    tests
        5
        10
        [
            testVal "Functor RegionShape ex1" (Circle 1) ((+1) <$> Circle 1),
            testVal "Functor RegionShape ex2" (Union 2 3) ((+1) <$> Union 1 2),
            testVal "Functor RegionShape basicIntersect" (Intersection 2 3) ((+1) <$> Intersection 1 2),
            testVal "Functor RegionShape points" (FromPoints [(0, 0)]) ((*100) <$> FromPoints [(0, 0)]),
            testVal "Functor RegionShape rectangle" (Rectangle 2 3) ((+1) <$> Rectangle 2 3),
            testVal "Functor RegionShape complement" (Complement 4) ((+1) <$> Complement 3),
            testVal "Functor RegionShape transform" (Transform (scaling 2) 6) ((*2) <$> Transform (scaling 2) 3),
            testVal "Functor RegionShape morePoints" (FromPoints [(0, 0), (1, 1), (2, 2)]) ((+4) <$> (FromPoints [(0, 0), (1, 1), (2, 2)])),
            testVal "Functor RegionShape equational" (Union 3 5) ((+1) <$> (*2) <$> Union 1 2),
            testVal "Functor RegionShape complex" ref (func <$> out)
        ]
        where
            ref = Union (R $ Complement $ R $ Circle 1) $ R $ Intersection (R $ Rectangle 2 3) $ R $ Complement $ R $ Circle 1
            out = Union (R $ Complement $ R $ Circle 1) $ R $ Intersection (R $ Rectangle 2 3) $ R $ Complement $ R $ Circle 1

            func :: RegionAST -> RegionAST
            func original@(R content) = case content of
                Circle radius -> R $ Circle $ 2 * radius
                Rectangle width height -> R $ Rectangle (3 * width) (4 * height)
                _ -> original

testFoldTransformationAST :: TestData
testFoldTransformationAST =
    tests
        6
        15
        [ testVal "foldTransformationAST ex      " exStr $ foldTransformationAST labelFolder exAst
        , testVal "foldTransformationAST empty   " "" $ foldTransformationAST labelFolder $ combineTransformations []
        , testVal "foldTransformationAST repeated" repeatedStr $ foldTransformationAST labelFolder repeatedAst
        ]
  where
    exAst = combineTransformations [translation 1 2, scaling 3, combineTransformations [scaling 4, translation 5 6]]
    exStr = "tsst"

    repeatedAst =
        combineTransformations
            [ scaling 1
            , combineTransformations [scaling 2, translation 3 4, combineTransformations [translation 5 6]]
            , combineTransformations [scaling 7]
            , translation 8 9
            , scaling 10
            ]
    repeatedStr = "ssttsts"

testFoldRegionAST :: TestData
testFoldRegionAST =
    tests
        7
        15
        [ testVal "foldRegionAST simple " "r" $ foldRegionAST regionPrintFolder $ rectangle 16 9
        , testVal "foldRegionAST union  " unionStr $ foldRegionAST regionPrintFolder unionReg
        , testVal "foldRegionAST nested " nestedStr $ foldRegionAST regionPrintFolder nestedReg
        , testVal "foldRegionAST shallow" 2 $ foldRegionAST depthFolder $ complement (circle 3.14)
        , testVal "foldRegionAST deep   " deepInt $ foldRegionAST depthFolder deepReg
        ]
  where
    unionReg = fromPoints [(0, 0)] `union` applyTransformation (translation 1 2) (circle 10)
    unionStr = "pUc@"

    nestedReg =
        complement
            ( intersection
                (applyTransformation (scaling 4) (circle 1))
                (fromPoints [] `union` applyTransformation (combineTransformations [scaling 2, translation 1 0]) (rectangle 2 3))
            )
    nestedStr = "~c@&pUr@"

    deepReg = nestedReg
    deepInt = 5

testToTransformation :: TestData
testToTransformation =
    tests
        8
        5
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
              val13 = S.combineTransformations [S.scaling 0.5, S.translation 1 0] (2.57, -8.96)
              val14 = S.combineTransformations [S.scaling 0.5, S.translation 1 0, S.scaling 2.7] (-7.4, 4.5)
              val15 = S.combineTransformations [S.scaling 0.5, S.translation 5 0, S.scaling 2.2, S.translation 1 2, S.scaling 4.5] (-log (3.5), log $ cos $ log $ pi/5)
              val16 = S.combineTransformations [S.scaling 0.2, S.translation 1.5 0, S.scaling 4.5, S.translation 3 2, S.scaling 1.5] (-656, 943)
              val17 = S.combineTransformations [S.scaling 0.8, S.translation 1.4 1.1, S.scaling 5, S.translation 1.8 2, S.scaling 2.5] (435.43, 243.435)
              val18 = S.combineTransformations [S.scaling 0.5, S.translation 1 0, S.scaling 2.7, S.translation 1 2, S.scaling 4.5, S.scaling 1.7, S.translation 1.4 2] (2, 2)
              val19 = S.combineTransformations [S.scaling 0.5, S.translation 1 0, S.scaling 2.7, S.translation 1 2, S.scaling 4.5] (-log (2.5e-4), log $ cos $ log $ pi / (tan pi/4))

testBasicTransformationCount :: TestData
testBasicTransformationCount =
    tests
        9
        5
        [ testVal "basicTransformationCount one translation" 1 $ basicTransformationCount $ translation 1 2
        , testVal "basicTransformationCount one scaling" 1 $ basicTransformationCount $ scaling 3
        , testVal "basicTransformationCount 4 transformations imbricated" 4 $ basicTransformationCount $
            combineTransformations [ translation 1 2, combineTransformations [ translation 3 4, scaling 2], scaling 3]
        , testVal "basicTransformationCount 4 transformations simple" 4 $ basicTransformationCount $
           combineTransformations [ translation 1 2, translation 3 4, translation 5 6, scaling 3]
        , testVal "basicTransformationCount 5 transformations" 5 $ basicTransformationCount $
            combineTransformations [ translation 1 2, combineTransformations [ translation 3 4, scaling 2], scaling 3, translation 5 6]
        , testVal "basicTransformationCount 6 transformations" 6 $ basicTransformationCount $
            combineTransformations [ translation 1 2, combineTransformations [ translation 3 4, scaling 2], combineTransformations [ translation 5 6, scaling 3], scaling 4]
        , testVal "basicTransformationCount 8 transformations" 8 $ basicTransformationCount $
            combineTransformations [ translation 1 2, combineTransformations [ translation 3 4, scaling 2], scaling 3, scaling 4, combineTransformations [ translation 7 8, scaling 8], scaling 5]
        , testVal "basicTransformationCount 10 transformations" 10 $ basicTransformationCount $
            combineTransformations [ translation 1 2, translation 3 4, translation 5 6, scaling 3, translation 7 8, translation 9 10, translation 11 12, translation 13 14, translation 15 16, translation 17 18]
        , testVal "basicTransformationCount 16 transformations" 16 $ basicTransformationCount $
            combineTransformations [ translation 1 2, combineTransformations [ translation 3 4, scaling 2], translation 5 6, translation 7 8, translation 9 10, translation 11 12, translation 13 14, translation 15 16, translation 17 18, translation 19 20, translation 21 22, translation 23 24, translation 25 26, translation 27 28, translation 29 30]
        , testVal "basicTransformationCount 20 transformations" 20 $ basicTransformationCount $
            combineTransformations [ translation 1 2, combineTransformations [ translation 3 4, scaling 2], scaling 3, translation 5 6, translation 7 8, translation 9 10, translation 11 12, translation 13 14, translation 15 16, translation 17 18, translation 19 20, translation 21 22, translation 23 24, translation 25 26, translation 27 28, translation 29 30, translation 31 32, translation 33 34, translation 35 36]
        ]

testBasicEntityCount :: TestData
testBasicEntityCount =
    tests
        10
        10
        [ testVal "basicEntityCount 2r + 7t" (2, 7) $ basicEntityCount $
            union (applyTransformation
                    (combineTransformations
                        [ translation 1 2
                        , combineTransformations [ translation 3 4
                                                , scaling 2
                                                ]  
                        , scaling 3
                        ])
                    (complement
                        (applyTransformation
                            (scaling 4)
                            (applyTransformation (scaling 2) (circle 5)))))
                  (applyTransformation (translation 4 6) (rectangle 6 7))
        , testVal "basicEntityCount 2r + 2t" (2, 2) $ basicEntityCount $
            union (applyTransformation (scaling 2) (circle 5))
                  (applyTransformation (translation 4 6) (rectangle 6 7))
        , testVal "basicEntityCount 3r + 3t union" (3, 3) $ basicEntityCount $
            union (applyTransformation (scaling 2) (circle 5))
                  (union (applyTransformation (scaling 2) (circle 3))
                         (applyTransformation (translation 4 6) (rectangle 6 7)))
        , testVal "basicEntityCount 3r + 3t intersection" (3, 3) $ basicEntityCount $
            union (applyTransformation (scaling 2) (circle 5))
                  (intersection (applyTransformation (scaling 2) (rectangle 3 4))
                                 (applyTransformation (translation 4 6) (rectangle 6 7)))
        , testVal "basicEntityCount 4r + 4t" (4, 4) $ basicEntityCount $
            union (applyTransformation (scaling 2) (circle 5))
                  (intersection (applyTransformation (scaling 2) (rectangle 3 4))
                                 (union (applyTransformation (translation 4 6) (rectangle 6 7))
                                        (applyTransformation (scaling 2) (rectangle 8 9))))
        , testVal "basicEntityCount 5r + 5t" (5, 5) $ basicEntityCount $
            union (applyTransformation (scaling 2) (circle 5))
                  (union (applyTransformation (scaling 2) (circle 3))
                         (union (applyTransformation (translation 4 6) (rectangle 6 7))
                                (union (applyTransformation (translation 1 2) (circle 10))
                                       (applyTransformation (scaling 3) (rectangle 8 9)))))
        , testVal "basicEntityCount 4r + 5t" (4, 5) $ basicEntityCount $
            union (applyTransformation (scaling 2) (circle 5))
                (union (applyTransformation (scaling 2) (circle 3))
                        (union (applyTransformation (translation 4 6) (rectangle 6 7))
                                (applyTransformation (combineTransformations [scaling 3, translation 1 2]) (rectangle 10 11))))
        , testVal "basicEntityCount 4r + 6t" (4, 6) $ basicEntityCount $
            union (applyTransformation (scaling 2) (circle 5))
                (intersection (applyTransformation (scaling 2) (rectangle 3 4))
                                (union (applyTransformation (translation 4 6) (rectangle 6 7))
                                    ((applyTransformation (combineTransformations [scaling 3, translation 1 2, translation 3 4]) (rectangle 10 11)))))
        , testVal "basicEntityCount 8r + 16t" (8, 16) $ basicEntityCount $
            union (union (applyTransformation (scaling 2) (circle 5))
                (intersection (applyTransformation (scaling 2) (rectangle 3 4))
                                (union (applyTransformation (translation 4 6) (rectangle 6 7))
                                    (applyTransformation (combineTransformations [scaling 3, scaling 2, translation 1 2, translation 2 4, scaling 1.5])
                                        (circle 7)))))
            (union (applyTransformation (scaling 2) (circle 5))
                (intersection (applyTransformation (scaling 2) (rectangle 3 4))
                                (union (applyTransformation (translation 4 6) (rectangle 6 7))
                                    (applyTransformation (combineTransformations [scaling 3, scaling 2, translation 1 2, translation 2 4, scaling 1.5])
                                        (circle 7)))))
        , testVal "basicEntityCount 10r + 18t" (10, 18) $ basicEntityCount $
            union (union (union (applyTransformation (scaling 2) (circle 5))
                (intersection (applyTransformation (scaling 2) (rectangle 3 4))
                                (union (applyTransformation (translation 4 6) (rectangle 6 7))
                                    (applyTransformation (combineTransformations [ scaling 3, translation 1 2, translation 2 4, scaling 1.5, translation 3 5])
                                        (rectangle 14 15)))))
                (applyTransformation (scaling 10) (circle 20)))
            (union (union (applyTransformation (scaling 5) (circle 7))
                (intersection (applyTransformation (scaling 4) (rectangle 9 10))
                                (union (applyTransformation (translation 14 16) (rectangle 16 17))
                                    (applyTransformation (combineTransformations [ scaling 5, translation 6 7, translation 8 16, scaling 2.5, translation 6 10])
                                        (rectangle 60 75)))))
                (applyTransformation (scaling 100) (circle 200)))
        ]

testShowFold :: TestData
testShowFold =
    tests
        11
        10
        [ testCond "show as fold" showFoldFlag
        ]

main :: IO ()
main =
    vmCheck
        [ testShowTransformationAST
        , testShowRegionAST
        , testNumRegionAST
        , testFunctorTransformationShape
        , testFunctorRegionShape
        , testFoldTransformationAST
        , testFoldRegionAST
        , testToTransformation
        , testBasicTransformationCount
        , testBasicEntityCount
        , testShowFold
        ]