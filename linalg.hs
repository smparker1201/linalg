--A small library of basic linear algebra functions 

dotProd :: [Float] -> [Float] -> Float
dotProd a b = 
	if length a /= length b
		then error "Dimension Mismatch" 
		else foldr (+) 0  $ zipWith (*) a b 

vectorSum :: [Float] -> [Float] -> [Float]  
vectorSum a b = 
	if length a /= length b 
		then error "Dimension Mismatch" 
		else zipWith (+) a b


vectorSubtract ::[Float] -> [Float] -> [Float] 
vectorSubtract a b = 
	if length a /= length b
		then error "Dimension Mismatch" 
		else zipWith (-) a b


isOrthogonal ::[Float] -> [Float] -> Bool
isOrthogonal a b = dotProd a b == 0

magnitude ::[Float] -> Float
magnitude a = dotProd a a ** (1/2) 

vectorNorm ::[Float] -> [Float] 
vectorNorm a = map (/ magnitude a) a 

constProd :: [Float] -> Float -> [Float] 
constProd list n = map (*n) list

isSquare ::[[Float]] -> Bool
isSquare a = all (\x -> length x == length a) a

transpose ::[[Float]] -> [[Float]]
transpose ([]:_) = []
transpose x = 
	if isValid x 
		then (map head x):transpose (map tail x) 
		else error "Invalid Input" 
	


multiply :: [[Float]] -> [[Float]] -> [[Float]]
multiply a b =
	if isValid a && isValid b
		then multHelper a (transpose b)
		else error "Invalid Input" 
	where 
		multHelper [] _ = []
		multHelper a b = [dotProd (head a) y | y <- b]: multHelper (tail a) b 

isValid :: [[Float]] -> Bool
isValid [] = True
isValid	(x:[]) = True
isValid all@(x:y:_) = (length x == length y) && isValid (tail all)

removeRow :: [[Float]] -> Int -> [[Float]] 
removeRow matrix n = (take n matrix)++(drop (n+1) matrix)

insertRow :: [[Float]] -> [Float] -> Int -> [[Float]]
insertRow matrix row n = (take n matrix)++(row:(drop n matrix)) 

rowReduce :: [[Float]] -> [[Float]]
rowReduce matrix =
	if isValid matrix
		then internalHelper matrix  0 0
		else error "Invalid Input" 
	where 
		internalHelper mat pivPos rowPos
			| pivPos == minimum [length(head mat),length mat] = mat
			| otherwise =
				let newMat =(take rowPos mat)++(swap (drop rowPos mat) pivPos)
				    pivotRow = createPivotRow newMat pivPos rowPos
				    pivot = fst pivotRow
				    newRow = snd pivotRow
				in internalHelper (reduce newMat pivot pivPos rowPos) (pivPos+1) (newRow+1) 
	
nullSpace :: [[Float]] -> [[Float]]
nullSpace matrix = matrix

columnSpace :: [[Float]] -> [[Float]]
columnSpace matrix = matrix

rowSpace :: [[Float]] -> [[Float]]
rowSpace matrix = matrix
-----------------------------Internal Helper Functions---------------------------------------
reduce :: [[Float]] -> [Float] -> Int -> Int-> [[Float]]
reduce matrix pivotRow p r = insertRow (map (applyPivotRow matrix pivotRow p) (removeRow matrix r)) pivotRow r

swap :: [[Float]] -> Int -> [[Float]]
swap (x:[]) p  = [x]
swap matrix p 
	| (head matrix)!!p == 0 = (swap (tail matrix) p)++[head matrix]
	| otherwise = matrix

createPivotRow :: [[Float]] -> Int -> Int -> ([Float],Int)
createPivotRow matrix p r
	| matrix!!r!!p /= 0 = (constProd (matrix!!r) (1/(matrix!!r!!p)),r)
	| otherwise = (matrix!!r,(r-1))

applyPivotRow :: [[Float]]-> [Float]-> Int -> [Float] -> [Float]
applyPivotRow matrix pivotRow p row = vectorSubtract row (constProd pivotRow $row!!p)

