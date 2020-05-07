import Data.List 

data System = System {
	x  :: [State],
	x0 :: [State],
	u  :: [Input],
	tr :: State -> Input -> State,
	y  :: [Outputs],
	h  :: State -> Output
}

-- | Now defining the following specifically for quicksort

-- | TYPES | --

type Pivot = Int
type State = [Int]

type Output = String

type Input = String

-- | GENERATING THE SETS FROM THE TYPES | -- 

-- pivot is always first element here
partition :: State -> Input -> State
partition (x:xs) next =
	let 
		smallerSorted = [a | a <- xs, a <= x]  
    	biggerSorted = [a | a <- xs, a > x]  
    in  
    	smallerSorted ++ [x] ++ biggerSorted


-- generate all possible permutations of a list
genPermut :: [Int] -> [[Int]]
genPermut ls = permutations ls

isSorted :: (Ord a) => [a] -> Bool
isSorted []       = True
isSorted [x]      = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

mkQsortSystem :: System
mkQsortSystem ls = 
	System states init_state "next" trans_func outputs output_map
	where
		n = length ls
		states = genPermut ls
		init_state = (ls\\\\\\\\\\\\
		trans_func = partition
		outputs = [True, False]
		output_map =  



