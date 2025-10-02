module Solver (satSolve) where

import Data.Array.Unboxed (listArray, elems)
-- import Debug.Trace (traceShow, traceShowId)

import Language.CNF.Parse.ParseDIMACS

import Control.Monad.State
import qualified Data.Map.Strict as Map

type Model = Map.Map Int Bool
type Solver a = State Model a

data SatResult 
    = SAT
    | UNSAT
  deriving (Eq, Ord, Show, Read)

satSolve :: CNF -> (SatResult, Model)
satSolve cnf =
    let cls = clauses cnf
    in runState (solveClauses cls 0) Map.empty

solveClauses :: [Clause] -> Int -> Solver SatResult
solveClauses cls lit
    | null cls = return SAT
    | any (null . elems) cls = return UNSAT
    | otherwise = do
        -- 0) назначаем литерал
        if lit /= 0 then assignLiteral lit else return ()

        -- 1) Обрабатываем unit clauses
        let lits = unitClauses cls
        cls' <- unitStep cls lits

        -- 2) Находим чистые литералы
        let allLits = concatMap elems cls'
            litCounts = Map.fromListWith (+) [(l, 1 :: Int) | l <- allLits]
            pureLits = [ l | l <- Map.keys litCounts, Map.notMember (-l) litCounts ]

        -- 3) Подставляем чистые литералы
        cls'' <- elimStep cls' pureLits

        -- 4) Выбираем следующий литерал
        let remainingLits = concatMap elems cls''
            nextLiteral = case remainingLits of
                []    -> 0
                (x:_) -> abs x

        -- 5) След литерал
        res <- solveClauses cls'' nextLiteral
        case res of
            SAT   -> return SAT
            UNSAT -> solveClauses cls'' (-nextLiteral)


-- eliminatePureLiteral S l
-- Фильтруем набор клоз для дальнейшего разбора алгоритмом
--      Если клоза содержит литерал, то считаем её истинной и вычеркиваем из рассматриваемых
eliminatePureLiteral :: [Clause] -> Int -> [Clause]

-- # Iterate over all clauses and delete with pure_prop
eliminatePureLiteral cls prop =
    [ cl | cl <- cls, not (prop `elem` elems cl) ]

-- unitPropagate S prop
-- Продвигаем prop в каждую клозу:
--      Если клоза не содержит prop, то надо почистить !prop, тк он ложен
unitPropagate :: [Clause] -> Int -> [Clause]

-- # Iterate over all clauses and delete not(prop_cl)
unitPropagate cls prop =
    [ listArray (0, length newProps - 1) newProps
    | cl <- cls
    , let props    = elems cl
    , not (prop `elem` props)                -- удалить клоузы с prop
    , let newProps = filter (/= (-prop)) props
    ]

unitClauses :: [Clause] -> [Int]
unitClauses cls =
    [ x
    | cl <- cls
    , let lits = elems cl
    , [x] <- [lits]
    ]

assignLiteral :: Int -> Solver ()
assignLiteral lit =
    let var = abs lit
        val = lit > 0
    in 
        modify (Map.insert var val)

-- Последовательный обход всех unit-клоуз
unitStep :: [Clause] -> [Int] -> Solver [Clause]
unitStep cls [] = return cls
unitStep cls (l:ls) = do
    -- 1) продвигаем через unitPropagate
    let cls' = unitPropagate cls l
    -- 2) записываем литерал в модель
    assignLiteral l
    -- продолжаем с оставшимися unit-клозами
    unitStep cls' ls

-- Последовательный обход всех unit-клоуз
elimStep :: [Clause] -> [Int] -> Solver [Clause]
elimStep cls [] = return cls
elimStep cls (l:ls) = do
    -- 1) продвигаем через eliminatePureLiteral
    let cls' = eliminatePureLiteral cls l
    -- 2) записываем литерал в модель
    assignLiteral l
    -- продолжаем с оставшимися unit-клозами
    elimStep cls' ls
