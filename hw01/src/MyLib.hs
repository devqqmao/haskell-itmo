module MyLib where

import Prelude hiding (or, and, not, fst, snd, pred)

k= \x y -> x
k_ = \x y -> y

tru = k
fls = k_

or = \b1 b2 -> b1 tru b2

pair = \x y f -> f x y
fst = k
snd = k_

and = \b1 b2 -> b1 b2 fls
not = \b -> b fls tru

ch0 = \s z -> z
ch1 = \s z -> s z
ch2 = \s z -> s (s z)
ch3 = \s z -> s (s (s z))
ch4 = \s z -> s (s (s (s z)))
ch5 = \s z -> s (s (s (s (s z))))
ch6 = \s z -> s (s (s (s (s (s z)))))

suc = \n s z -> s (n s z)
plus = \n m -> n suc m
mult = \n m -> n (plus m) ch0
-- pow = \n m -> m (mult n) ch1 ???
pow = \n m -> m n 

isZero = \n -> n (\x -> fls) tru

get_fst = \p -> p fst
get_snd = \p -> p snd 

ini = pair tru tru
is_tru_fls = \p -> and (and (get_fst p) tru) (not(or (get_snd p) fls))

{-
First idea: that doesn't work
-- Проблема: как запустить вычисление функции во второй скобке?
swap без матчинга???
swap = \p -> p (\x y -> let z = (y (pair tru fls) (pair fls tru)) in (x z (pair tru tru)))
divides3 = \n -> and (n swap ini) 
-}


-- Идея с вычитанием


ini0 = pair ch0 ch0
next = \p f -> f (get_snd p) (suc (get_snd p))
pred = \n -> get_fst (n next ini0)

