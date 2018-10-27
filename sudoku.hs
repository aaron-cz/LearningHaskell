import Data.Set

data Unit = Set Int | Singleton Int | Nil 


type Units = [Unit]
data Sudoku = Sudoku Int Units | Solution Int [Int] | Failure

solveSudoku :: Sudoku -> Sudoku
solveSudoku (Sudoku size u) = propogate (Sudoku size u) (fromList [0..(size * size-1)])


propogate :: Sudoku -> Set Int -> Sudoku
propogate Failure _               = Failure
propogate s       empty           = s
propogate (Sudoku size units) xs  = 
    propogate s' constraints
    where x   = findMin xs
          xs' = delete x xs
          ux  = units!!x
          (s', constraints) = propUnit (Sudoku size units) ux (x, xs')



-- if (ux == Singleton _) then propogate s' constraints
-- -- update xs
-- else propogate (Sudoku size units) xs'
propUnit :: Sudoku -> Unit -> (Int, Set Int) -> (Sudoku, Set Int)
propUnit s _ (i, si) = (s, si)
