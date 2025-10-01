module Solver (satSolve) where

import Data.Array.Unboxed (indices)

import Language.CNF.Parse.ParseDIMACS

data SatResult 
    = SAT
    | UNSAT
  deriving (Eq, Ord, Show, Read)

satSolve :: CNF -> SatResult
satSolve cnf =
    let cls = clauses cnf
    in solveClauses cls

solveClauses :: [Clause] -> SatResult

-- (1) if S = empty => return Sat
solveClauses [] = SAT

-- (2) [] ∈ S => return Unsat
solveClauses cls
    | any (null . indices) cls = UNSAT
    | otherwise                = SAT

-- TODO 
-- # (3) Если нашли unit клозу, то
            -- # 1 - Выпишем контрарную проп переменную из всех остальных клоз
            -- # 2 - Подставим в проп переменную зн-е 1(чтобы эту клозу удовлетворить) 

-- # (4) Если нашли чистый литерал который входит в клозы без контрарной пары в других


-- # 1 - Убираем клозы с чистыми литералами
        --   # 2 - Подставим в чистый литерал зн-е 1(чтобы эти клозы удовлетворить)


