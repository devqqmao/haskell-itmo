module TestSpec where

import MyLib
import Test.Hspec
import Prelude hiding (or, and, not, fst, snd, pred)
spec :: Spec
spec = do
    describe "or" $ do
        it "tru_tru" $ do
            let func = or tru tru
            func 1 2 `shouldBe` 1
        it "tru_fls" $ do
            let func = or tru fls 
            func 1 2 `shouldBe` 1
        it "fls_tru" $ do
            let func = or fls tru 
            func 1 2 `shouldBe` 1
        it "fls_fls" $ do
            let func = or fls fls 
            func 1 2 `shouldBe` 2
    describe "and" $ do
        it "tru_tru" $ do
            let func = and tru tru
            func 1 2 `shouldBe` 1
        it "tru_fls" $ do
            let func = and tru fls 
            func 1 2 `shouldBe` 2
        it "fls_tru" $ do
            let func = and fls tru 
            func 1 2 `shouldBe` 2
        it "fls_fls" $ do
            let func = and fls fls 
            func 1 2 `shouldBe` 2
    describe "or" $ do
        it "tru" $ do
            let func = not tru
            func 1 2 `shouldBe` 2
        it "fls" $ do
            let func = not fls 
            func 1 2 `shouldBe` 1
    describe "isZero" $ do
        it "test_0" $ do
            (isZero ch0) 0 1 `shouldBe` 0 
            (isZero ch1) 0 1 `shouldBe` 1 
            (isZero ch2) 0 1 `shouldBe` 1 
    describe "church" $ do
        it "test_0" $ do
            ch0 ('s':) "z" `shouldBe` "z"
            ch1 ('s':) "z" `shouldBe` "sz"
            ch2 ('s':) "z" `shouldBe` "ssz"
    describe "suc" $ do
        it "test_0" $ do
            (suc ch0) ('s':) "z" `shouldBe` "sz"
            (suc ch1) ('s':) "z" `shouldBe` "ssz"
            (suc (suc ch2)) ('s':) "z" `shouldBe` "ssssz"
    describe "plus" $ do
        it "test_0" $ do
            (ch0 `plus` ch0) ('s':) "z" `shouldBe` "z"
            (ch0 `plus` ch1) ('s':) "z" `shouldBe` "sz"
            (ch1 `plus` ch0) ('s':) "z" `shouldBe` "sz"
            (ch1 `plus` ch1) ('s':) "z" `shouldBe` "ssz"
            (ch2 `plus` ch2) ('s':) "z" `shouldBe` "ssssz"
            (ch3 `plus` ch2) ('s':) "z" `shouldBe` "sssssz"
    describe "mult" $ do
        it "test_0" $ do
            (ch0 `mult` ch0) ('s':) "z" `shouldBe` "z"
            (ch0 `mult` ch1) ('s':) "z" `shouldBe` "z"
            (ch1 `mult` ch0) ('s':) "z" `shouldBe` "z"
            (ch1 `mult` ch1) ('s':) "z" `shouldBe` "sz"
            (ch2 `mult` ch2) ('s':) "z" `shouldBe` "ssssz"
            (ch3 `mult` ch2) ('s':) "z" `shouldBe` "ssssssz"
            (ch3 `mult` ch4) ('s':) "z" `shouldBe` "ssssssssssssz"
    describe "pow" $ do
        it "test_0" $ do
            -- (ch0 `pow` ch0) ('s':) "z" `shouldBe`"z"
            -- (ch0 `pow` ch1) ('s':) "z" `shouldBe` "z"
            -- (ch1 `pow` ch0) ('s':) "z" `shouldBe` "z"
            (ch1 `pow` ch1) ('s':) "z" `shouldBe` "sz"
            (ch2 `pow` ch2) ('s':) "z" `shouldBe` "ssssz"
            (ch3 `pow` ch2) ('s':) "z" `shouldBe` "sssssssssz"
            (ch4 `pow` ch2) ('s':) "z" `shouldBe` "ssssssssssssssssz"
    describe "divides3" $ do
        it "ini" $ do
            (ini fst) 0 1 `shouldBe` 0
            (ini snd) 0 1 `shouldBe` 0
        it "is_tru_fls" $ do
            -- tru fls -> tru
            -- other -> fls
            (is_tru_fls (pair tru fls)) 0 1 `shouldBe` 0
            (is_tru_fls (pair tru tru)) 0 1 `shouldBe` 1
            (is_tru_fls (pair fls fls)) 0 1 `shouldBe` 1
            (is_tru_fls (pair fls tru)) 0 1 `shouldBe` 1
        it "swap" $ do
            1 `shouldBe` 1
            -- pair -> pair
            -- pair tru tru -> pair tru fls
            -- pair tru fls -> fls tru
            -- pair fls tru -> tru tru
            {-
            let t1 = (swap (pair tru tru)) 
            let t2 = (swap (pair tru fls)) 
            let t3 = (swap (pair fls tru)) 
            
            (get_fst t1) 0 1 `shouldBe` 0
            (get_snd t1) 0 1 `shouldBe` 1
            -}

        it "divides3" $ do
            tru 0 1 `shouldBe` 0 
            {-
            (divides3 ch1) 0 1 `shouldBe` 1 
            (divides3 ch2) 0 1 `shouldBe` 1 
            (divides3 ch3) 0 1 `shouldBe` 0 
            (divides3 ch4) 0 1 `shouldBe` 1 
            (divides3 ch5) 0 1 `shouldBe` 1 
            (divides3 ch6) 0 1 `shouldBe` 0 
            -}
    describe "pred" $ do
        it "test_0" $ do
            -- (pred ch0) ('s':) "z" `shouldBe` "z"
            (pred ch1) ('s':) "z" `shouldBe` "z"
            (pred ch2) ('s':) "z" `shouldBe` "sz"
            (pred ch3) ('s':) "z" `shouldBe` "ssz"
