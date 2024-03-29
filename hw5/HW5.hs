-- Group members:
--  * (me) Alexander Nead-Work, 933190259
--
-- Grading notes: 15pts total
--  * 2pts checkExpr
--  * 4pts checkCmd
--  * 1pt checkBlock
--  * 1pt checkDef
--  * 2pts expr
--  * 5pts cmd
--  * + extra credit (typically 1-5pts)
--
-- Supporting files:
--
--  * MiniLogo.hs -- Defines the syntax of MiniLogo, pretty printing functions,
--    and the environment types that you'll need to implement the semantics.
--    Also includes functions to generate example MiniLogo programs to test with.
--
--  * Render.hs -- Defines the Point and Line types used in the semantics, plus
--    code for rendering the semantics of MiniLogo programs in HTML5.
--
module HW5 (draw) where

import MiniLogo
import Render


-- Note: in this file, we're placing the AST argument as the *last* argument in
-- each function, rather than the first argument. Although this makes the
-- semantic domains less obvious, it's good FP style since it promotes partial
-- application and can make some of your definitions a bit simpler.


-- Some functions from the Prelude that may be helpful on this assignment:
--
--   all :: (a -> Bool) -> [a] -> Bool
--     Returns true if the function returns true for every element in the list.
--
--   elem :: Eq a => a -> [a] -> Bool
--     Returns true if the first argument is contained as an element in the list.


-- This comment is defining some MiniLogo expressions used in doctests.
--
-- $setup
-- >>> let exprXY = Mul (Ref "y") (Add (Lit 3) (Ref "x"))
-- >>> let exprXZ = Add (Mul (Ref "x") (Lit 4)) (Ref "z")


--
-- * Static checker
--


-- | Statically check that a MiniLogo program is well formed. This function
--   builds up the map 'ms' that contains the defined macros and their arity
--   (i.e. the number of arguments they require), then checks all of the macro
--   definitions, and finally checks the main macro.
--
--   >>> check (xboxes 5 3)
--   True
--
--   >>> check (Program [line,xbox] [])
--   False
--
check :: Prog -> Bool
check (Program defs main) =
    all (checkDef arities) defs && checkBlock arities [] main
  where 
    entry (Define m ps _) = (m, length ps)
    arities = map entry defs



-- | Statically check that an expression is well formed by checking that there
--   are no unbound variables. This function receives as an argument a list of
--   all of the variables that are declared in the scope of the expression.
--
--   >>> checkExpr ["x"] (Ref "x")
--   True
--
--   >>> checkExpr ["y"] (Ref "x")
--   False
--
--   >>> checkExpr ["x","y"] exprXY
--   True
--
--   >>> checkExpr ["x","y"] exprXZ
--   False
--
checkExpr :: [Var] -> Expr -> Bool
checkExpr _ (Lit _)      = True
checkExpr _ (Color _)    = True
checkExpr vars (Ref v)   = elem v vars
checkExpr vars (Add l r) = checkExpr vars l && checkExpr vars r
checkExpr vars (Sub l r) = checkExpr vars l && checkExpr vars r
checkExpr vars (Mul l r) = checkExpr vars l && checkExpr vars r
checkExpr vars (Div l r) = checkExpr vars l && checkExpr vars r



-- | Statically check that a command is well formed by: (1) checking whether
--   all expressions it contains are well formed, (2) checking whether every
--   macro that is called has been defined, and (3) checking whether every
--   macro is called with the correct number of arguments. The first argument
--   to this function is a map containing the declared macros as keys and the
--   number of arguments they expect (their "arity") as values.
--
--   >>> checkCmd [] ["x","y"] (Move exprXY exprXZ)
--   False
--
--   >>> checkCmd [] ["x","y","z"] (Move exprXY exprXZ)
--   True
--
--   >>> checkCmd [] [] (Call "foo" [Lit 2, Lit 3])
--   False
--
--   >>> checkCmd [("f",2)] [] (Call "f" [Lit 2, Lit 3])
--   True
--
--   >>> checkCmd [("f",2)] [] (Call "f" [Lit 2, Lit 3, Lit 4])
--   False
--
--   >>> checkCmd [("f",2)] ["x","y","z"] (Call "f" [exprXZ, exprXY])
--   True
--
--   >>> checkCmd [("f",2)] ["x","y"] (Call "f" [exprXZ, exprXY])
--   False
--
--   >>> checkCmd [] [] (For "z" (Lit 1) (Lit 100) [Move (Ref "z") (Ref "z")])
--   True
--
--   >>> checkCmd [] [] (For "z" (Lit 1) (Lit 100) [Pen Up, Call "f" [Ref "z"]])
--   False
--
--   >>> checkCmd [("f",1)] [] (For "z" (Lit 1) (Lit 100) [Pen Up, Call "f" [Ref "z"]])
--   True
--
--   >>> checkCmd [("f",1)] [] (For "z" (Lit 1) (Lit 100) [Pen Up, Call "f" [exprXZ]])
--   False
--
--   >>> checkCmd [("f",1)] ["x"] (For "z" (Lit 1) (Lit 100) [Pen Up, Call "f" [exprXZ]])
--   True
--
--   >>> checkCmd [("f",1)] ["x"] (For "z" (Lit 1) (Lit 100) [Pen Up, Call "f" [exprXY]])
--   False
--
--   >>> checkCmd [("f",1)] ["x"] (For "z" exprXY (Lit 100) [Pen Up, Call "f" [exprXZ]])
--   False
--
--   >>> checkCmd [("f",1)] ["x"] (For "z" (Lit 1) exprXY [Pen Up, Call "f" [exprXZ]])
--   False
--
--   >>> checkCmd [("f",1)] ["x","y"] (For "z" exprXY exprXY [Pen Up, Call "f" [exprXZ]])
--   True
--
checkCmd :: Map Macro Int -> [Var] -> Cmd -> Bool
checkCmd macros vars (Pen _)                                                                                                                   = True
checkCmd macros vars (SetColor _)                                                                                                              = True
checkCmd macros vars (Move x y)                                                                                                                = checkExpr vars x && checkExpr vars y
checkCmd macros vars (Call m a) | get m macros == Just (length a) && all (checkExpr vars) a                                                    = True
checkCmd macros vars (For v start stop b) | checkExpr (vars ++ [v]) start && checkExpr (vars ++ [v]) stop && checkBlock macros (vars ++ [v]) b = True
checkCmd _ _ _ = False



-- | Statically check whether all of the commands in a block are well formed.
--
--   >>> checkBlock [] [] []
--   True
--
--   >>> checkBlock [] ["x","y"] [Pen Up, Move exprXY exprXZ, Pen Down]
--   False
--
--   >>> checkBlock [] ["x","y","z"] [Pen Up, Move exprXY exprXZ, Pen Down]
--   True

--   >>> checkBlock [] ["x","y"] [Pen Up, Call "f" [exprXY], Pen Down]
--   False
--
--   >>> checkBlock [("f",2)] ["x","y"] [Pen Up, Call "f" [exprXY], Pen Down]
--   False
--
--   >>> checkBlock [("f",2)] ["x","y"] [Pen Up, Call "f" [exprXY,exprXZ], Pen Down]
--   False
--
--   >>> checkBlock [("f",2)] ["x","y","z"] [Pen Up, Call "f" [exprXY,exprXZ], Pen Down]
--   True
--
checkBlock :: Map Macro Int -> [Var] -> Block -> Bool
checkBlock macros vars = all (checkCmd macros vars)



-- | Check whether a macro definition is well formed.
--
--   >>> checkDef [] (Define "f" [] [Pen Down, Move (Lit 2) (Lit 3), Pen Up])
--   True
--
--   >>> checkDef [] (Define "f" [] [Pen Down, Move exprXY exprXZ, Pen Up])
--   False
--
--   >>> checkDef [] (Define "f" ["x","y","z"] [Pen Down, Move exprXY exprXZ, Pen Up])
--   True
--
--   >>> checkDef [] (Define "f" ["x","y","z"] [Pen Down, Call "g" [exprXY,exprXZ], Pen Up])
--   False
--
--   >>> checkDef [("g",3)] (Define "f" ["x","y","z"] [Pen Down, Call "g" [exprXY,exprXZ], Pen Up])
--   False
--
--   >>> checkDef [("g",3)] (Define "f" ["x","y","z"] [Pen Down, Call "g" [exprXY,exprXZ,exprXY], Pen Up])
--   True
--
checkDef :: Map Macro Int -> Def -> Bool
checkDef macros (Define m p b) = checkBlock (set m (length p) macros) p b



--
-- * Semantics of MiniLogo
--

-- | The state of the pen, which includes whether it is up or down and its
--   current location.
type State = (Mode, Point, Expr) -- Expr==Color

-- | The initial state of the pen.
initPen :: State
initPen = (Up, (0,0), Color 0xff0000)


-- | Run a MiniLogo program by:
--    1. Statically checking for errors. If it fails the check, stop here.
--    2. Run the semantics to produce a bunch of lines.
--    3. Pass those lines to the renderer.
--
--   Once your checker and semantic function are working, you should be
--   able to apply 'draw' to a MiniLogo program, then load the file
--   MiniLogo.html in your browswer to view the rendered image.
draw :: Prog -> IO ()
draw p | check p   = toHTML (prog p)
       | otherwise = putStrLn "failed static check :-("


-- ** Semantic functions

-- In this section, we're assuming we already ran the static checker. This
-- means that you can use the 'getOrFail' function to lookup entries in either
-- the variable environment or macro environment, and you can use the 'setAll'
-- function to add arguments to an environment without first checking that you
-- have the correct number.
--
-- Remember that in this assignment we're making the AST type the last argument
-- in each function to support a nicer functional programming style, even
-- though this obfuscates the semantic domains a bit. The semantic domain of
-- each syntactic category is listed below for reference and then briefly
-- explained.
--
-- Semantic domains:
--   * Expr:  Env -> Int
--   * Cmd:   Macros -> Env -> State -> (State, [Line])
--   * Block: Macros -> Env -> State -> (State, [Line])
--   * Prog:  [Line]
--
-- The semantics of expressions requires only the variable environment, and
-- this environment is read-only since expressions cannot change the values of
-- variables.

-- In addition to the variable environment, the semantics of commands and
-- blocks also takes a macro environment (Macros), which stores macro
-- definitions. The macro environment maps macro names to a pair
-- '(Pars,Block)', where the first element of the pair is the list of
-- parameters that the macro declares, and second element is the body of the
-- macro. You will have to use the macro environment in the semantics of
-- commands to implement macro calls.
--
-- Unlike in Homework 4, commands may now produce an arbitrary number of lines
-- (not just 0 or 1) because of macro calls and for loops. So, the semantic
-- domain of commands now produces a list of lines rather than a 'Maybe Line'.
--
-- Programs are run on initially empty environments and a well-defined initial
-- pen state (Up,(0,0)), so the semantic function doesn't take any arguments
-- besides the program itself. We also return only the lines drawn by the
-- program as a result since we no longer care about the pen state once the
-- program has completely finished running.



-- | Semantics of expressions.
--
--   >>> let env = [("x",3),("y",4)]
--
--   >>> expr env (Mul (Ref "y") (Add (Lit 5) (Ref "x")))
--   32
--
--   >>> expr env (Add (Mul (Ref "x") (Lit 5)) (Mul (Lit 6) (Ref "y")))
--   39
--
--   >>> expr env (Div (Lit 6) (Lit 3))
--   2
--
--   >>> expr env (Sub (Lit 6) (Lit 3))
--   3
--
expr :: Env -> Expr -> Int
expr _ (Lit i)     = i
expr _ (Color i)   = i
expr env (Ref v)   = getOrFail v env
expr env (Add l r) = expr env l + expr env r
expr env (Sub l r) = expr env l - expr env r
expr env (Mul l r) = expr env l * expr env r
expr env (Div l r) = expr env l `div` expr env r



-- | Semantics of commands.
--
--   >>> let vs = [("x",3),("y",4)]
--   >>> let Define _ ps b = line
--   >>> let ms = [("m1",([],[])),("line",(ps,b)),("m2",([],[]))]
--   
--   >>> cmd [] [] (Up,(2,3),Color 0xff0000) (Pen Down)
--   ((Down,(2,3),Color 16711680),[])
--
--   >>> cmd [] [] (Down,(2,3),Color 0xff0000) (Pen Up)
--   ((Up,(2,3),Color 16711680),[])
--
--   >>> cmd [] [] (Down,(2,3),Color 0xff0000) (SetColor (Color 0x0000ff))
--   ((Down,(2,3),Color 255),[])
--
--   >>> cmd [] vs (Up,(2,3),Color 0xff0000) (Move (Ref "y") (Add (Ref "x") (Lit 2)))
--   ((Up,(4,5),Color 16711680),[])
--
--   >>> cmd [] vs (Down,(2,3),Color 0xff0000) (Move (Ref "y") (Add (Ref "x") (Lit 2)))
--   ((Down,(4,5),Color 16711680),[((2,3),(4,5),16711680)])
--
--   >>> cmd ms vs (Up,(0,0),Color 0xff0000) (Call "m1" [])
--   ((Up,(0,0),Color 16711680),[])
--
--   >>> cmd ms vs (Down,(0,0),Color 0xff0000) (Call "line" [Ref "x", Ref "y", Add (Ref "x") (Lit 2), Add (Ref "y") (Lit 3), Color 0xff0000])
--   ((Down,(5,7),Color 16711680),[((3,4),(5,7),16711680)])
--
--   >>> cmd [] vs (Down,(0,0),Color 0xff0000) (For "i" (Lit 1) (Ref "x") [])
--   ((Down,(0,0),Color 16711680),[])
--
--   >>> cmd ms vs (Down,(0,0),Color 0xff0000) (For "i" (Lit 1) (Ref "y") [Move (Ref "i") (Ref "i")])
--   ((Down,(4,4),Color 16711680),[((0,0),(1,1),16711680),((1,1),(2,2),16711680),((2,2),(3,3),16711680),((3,3),(4,4),16711680)])
--
--   >>> cmd ms vs (Down,(0,0),Color 0xff0000) (For "i" (Ref "x") (Lit 1) [Call "line" [Ref "i", Ref "i", Mul (Ref "x") (Ref "i"), Mul (Ref "y") (Ref "i")]])
--   ((Down,(3,4),Color 16711680),[((3,3),(9,12),16711680),((2,2),(6,8),16711680),((1,1),(3,4),16711680)])
--
cmd :: Macros -> Env -> State -> Cmd -> (State, [Line])
cmd defs env state@(pen,pos,color) c = case c of

    Pen Up   -> ((Up, pos, color), [])
    Pen Down -> ((Down, pos, color), [])

    SetColor color' -> ((pen, pos, color'), [])

    Move xExp yExp -> case pen of
                         Up   -> ((pen, (expr env xExp, expr env yExp), color), [])
                         Down -> ((pen, (expr env xExp, expr env yExp), color), [(pos, (expr env xExp, expr env yExp), expr env color)])

    Call macro args ->

      let (ps, c') = getOrFail macro defs 
           
          -- This helper function takes the parameters to the function
          -- and evaluates/adds them to the program environment
          extendEnv :: Env -> [(Var, Expr)] -> Map Var Int
          extendEnv _ []           = []
          extendEnv env ((v, e):t) = (v, expr env e) : extendEnv env t
    
      in block defs (env ++ extendEnv env (zip ps args)) (pen,pos,color) c'

    For v fromExp toExp body ->

      let from = expr env fromExp
          to   = expr env toExp
          ixs  = if from <= to then [from .. to] else reverse [to .. from]

          -- This helper function runs the body of the loop once, with the loop
          -- index set to the given integer. You just need to study the code
          -- and fill in the undefined part that runs the body of the loop.
          loopStep :: (State, [Line]) -> Int -> (State, [Line])
          loopStep (s, ls) i =
            let (s', ls') = block defs (env ++ [(v, i)]) s body
            in (s', ls ++ ls')

      in foldl loopStep (state, []) ixs



-- | Semantics of blocks.
--
--   >>> block [] [] (Down,(0,0),Color 0xff0000) []
--   ((Down,(0,0),Color 16711680),[])
--
--   >>> block [] [] (Down,(0,0),Color 0xff0000) [Pen Down, Pen Up, Pen Up, Move (Lit 2) (Lit 3)]
--   ((Up,(2,3),Color 16711680),[])
--
--   >>> block [] [] (Down,(0,0),Color 0xff0000) [Pen Up, Move (Lit 2) (Lit 3), Pen Down, Move (Lit 4) (Lit 5), Move (Lit 6) (Lit 7)]
--   ((Down,(6,7),Color 16711680),[((2,3),(4,5),16711680),((4,5),(6,7),16711680)])
-- 
block :: Macros -> Env -> State -> Block -> (State, [Line])
block defs env state = go state []
  where
    go s ls []     = (s,ls)
    go s ls (c:cs) = let (s',ls') = cmd defs env s c
                     in go s' (ls ++ ls') cs



-- | Semantic function for programs.
prog :: Prog -> [Line]
prog (Program defs main) = snd $ block (map entry defs) [] initPen main
  where
    entry (Define m ps b) = (m, (ps,b))


--
-- * Amazing picture (extra credit)
--
-- Custom functionality I implemented:
--  * Support for custom colors
--  * Division and subtraction operations
--  * Changed the scale factor in Render.hs to 1, and updated MiniLogo.hs to accurately draw shebang
--

filledBox :: Def
filledBox = Define "filledBox" ["x","y","w","h"]
    [ Call "box" [Ref "x", Ref "y", Ref "w", Ref "h"]
    , For "i" (Lit 0) (Ref "w")
      [ Pen Up
      , Move (Add (Ref "x") (Ref "i")) (Ref "y")
      , Pen Down
      , Move (Add (Ref "x") (Ref "i")) (Add (Ref "y") (Ref "h"))
      ]
    ]

-- | A MiniLogo program that draws your amazing picture.
amazing :: Prog
amazing = Program [line, box, filledBox]
    [ SetColor (Color 0xfad61d)
    , Call "filledBox" [Lit 150, Lit 0, Lit 200, Lit 310]

    , SetColor (Color 0x000000)
    -- left edge
    , Call "line" [Lit 150, Lit 0, Lit 160, Lit 30]
    , Call "line" [Lit 160, Lit 30, Lit 140, Lit 100]
    , Call "line" [Lit 140, Lit 100, Lit 180, Lit 220]

    -- start of left ear
    , Call "line" [Lit 180, Lit 220, Lit 190, Lit 230]
    , Call "line" [Lit 180, Lit 220, Lit 130, Lit 300]

    -- down left ear
    , Call "line" [Lit 130, Lit 300, Lit 230, Lit 250]

    -- connector
    , Call "line" [Lit 230, Lit 250, Lit 300, Lit 250]

    -- right ear
    , Call "line" [Lit 300, Lit 250, Lit 400, Lit 280]
    , Call "line" [Lit 400, Lit 280, Lit 330, Lit 210]
    
    -- little notch up thingy
    , Call "line" [Lit 330, Lit 210, Lit 320, Lit 220]

    -- right edge
    , Call "line" [Lit 330, Lit 210, Lit 350, Lit 0]

    -- cheeks
    , SetColor (Color 0xc26915)
    , Call "filledBox" [Lit 160, Lit 85, Lit 35, Lit 25]
    , Call "filledBox" [Lit 290, Lit 85, Lit 35, Lit 25]
    
    -- mouth
    , SetColor (Color 0xff5e5e)
    , Call "filledBox" [Lit 215, Lit 70, Lit 35, Lit 25]
    , SetColor (Color 0x6e2222)
    , Call "box" [Lit 215, Lit 70, Lit 35, Lit 25]

    -- left eye
    , SetColor (Color 0x61331e)
    , Call "filledBox" [Lit 200, Lit 140, Lit 20, Lit 25]
    , SetColor (Color 0xffffff)
    , Call "filledBox" [Lit 202, Lit 154, Lit 6, Lit 9]
    
    -- right eye
    , SetColor (Color 0x61331e)
    , Call "filledBox" [Lit 260, Lit 140, Lit 20, Lit 25]
    , SetColor (Color 0xffffff)
    , Call "filledBox" [Lit 262, Lit 154, Lit 6, Lit 9]

    -- nose
    , SetColor (Color 0x61331e)
    , Call "filledBox" [Lit 235, Lit 120, Lit 8, Lit 4]
    ]