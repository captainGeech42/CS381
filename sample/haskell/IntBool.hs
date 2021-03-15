-- | A simple expression language with two types.
module IntBool where

import Prelude hiding (not,and,or)


--  Syntax of the "core" IntBool language
--
--  int  ::=  (any integer)
--
--  exp  ::=  int                integer literal
--        |   exp + exp          integer addition
--        |   exp * exp          integer multiplication
--        |   exp = exp          check whether two expressions are equal
--        |   exp ? exp : exp    conditional expressions


-- 1. Define the abstract syntax as a Haskell data type.

data Exp
   = Lit Int
   | Add Exp Exp
   | Mul Exp Exp
   | Equ Exp Exp
   | If  Exp Exp Exp
  deriving (Eq,Show)


-- Here are some example expressions:
--  * encode the abstract syntax tree as a Haskell value
--  * what should the result be?

-- | 2 * (3 + 4)  =>  14
ex1 :: Exp
ex1 = Mul (Lit 2) (Add (Lit 3) (Lit 4))

-- | 2 * (3 + 4) = 10  =>  false
ex2 :: Exp
ex2 = Equ ex1 (Lit 10)

-- | 2 * (3 + 4) ? 5 : 6  =>  type error!
ex3 :: Exp
ex3 = If ex1 (Lit 5) (Lit 6)

-- | 2 * (3 + 4) = 10 ? 5 : 6  =>  6
ex4 :: Exp
ex4 = If ex2 (Lit 5) (Lit 6)


-- 2. Identify/define the semantic domain for this language.
--   * what types of values can we have?
--   * how can we express this in Haskell?

-- Types of values we can have:
--  * Int
--  * Bool
--  * Error

data Value
   = I Int
   | B Bool
   | Error
  deriving (Eq,Show)


-- Alternative semantics domain using Maybe and Either:
--
--   data Maybe a = Nothing | Just a
--   data Either a b = Left a | Right b
--
--   type Value = Maybe (Either Int Bool)
--
-- Example semantic values in both representations:
-- 
--   I 5     <=>  Just (Left 5)
--   B True  <=>  Just (Right True)
--   Error   <=>  Nothing
--   
-- Isomorphic to the above:
--   type Value = Maybe (Either Bool Int)
--   type Value = Either (Maybe Int) Bool
--   type Value = Either Int (Maybe Bool)
--   type Value = Either (Either Int Bool) ()
--
-- Not isomorphic to the above
--   type Value = Either (Maybe Int) (Maybe Bool)
--     (reason: two different Nothings! Left Nothing and Right Nothing)
--


-- 3. Define the semantic function.
eval :: Exp -> Value
eval (Lit i)    = I i
eval (Add l r)  = case (eval l, eval r) of
                    (I i, I j) -> I (i + j)
                    _          -> Error
eval (Mul l r)  = case (eval l, eval r) of
                    (I i, I j) -> I (i * j)
                    _          -> Error
eval (Equ l r)  = case (eval l, eval r) of
                    (I i, I j) -> B (i == j)
                    (B a, B b) -> B (a == b)
                    _          -> Error
eval (If c t e) = case eval c of
                    B True  -> eval t
                    B False -> eval e
                    _       -> Error


-- 4. Syntactic sugar.
--
-- Goal: extend the syntax of our language with the following operations:
--
--      * boolean literals
--      * integer negation
--      * boolean negation (not)
--      * conjunction (and)
--      * disjunction (or)
-- 
-- How do we do this? Can we do it without changing the abstract syntax
-- or the semantics?

true :: Exp
true = Equ (Lit 1) (Lit 1)

false :: Exp
false = Equ (Lit 0) (Lit 1)

neg :: Exp -> Exp
neg e = Mul (Lit (-1)) e

not :: Exp -> Exp
not e = If e false true
-- not e = Equ false e

and :: Exp -> Exp -> Exp
and l r = If l (If r true false) false

or :: Exp -> Exp -> Exp
or l r = If l true (If r true false)


-- | Example program that uses our syntactic sugar.
--     not true || 3 = -3 && (true || false)  ->  false
ex5 :: Exp
ex5 = or (not true) (and (Equ (Lit 3) (neg (Lit 3))) (or true false))



--
-- * Statically typed variant (later!)
--


-- 1. Define the syntax of types.



-- 2. Define the typing relation.
typeOf = undefined


-- 3. Define the semantics of type-correct programs.
evalChecked = undefined


-- | Helper function to evaluate an Exp to an Int.
evalInt :: Exp -> Int
evalInt e = case evalChecked e of
              Left i -> i
              _ -> error "internal error: expected Int, got something else!"

-- | Helper function to evaluate an Exp to an Bool.
evalBool :: Exp -> Bool
evalBool e = case evalChecked e of
               Right b -> b
               _ -> error "internal error: expected Bool, got something else!"


-- 4. Define our interpreter.
checkAndEval = undefined
