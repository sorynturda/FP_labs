module Rezolvari where

a = fact 100000

fact 0 = 1 
fact n = factAcc n 1
factAcc n acc = 
    if n == 0 then
        acc 
    else
        factAcc (n-1) (n * acc)