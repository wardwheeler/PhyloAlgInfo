{- |
Module      :  Matrix functions for Algorithmic (Kolmogorov) complexity
Description :  Functions to generate(algorithmic) complexity of GTR-type character change models
Copyright   :  (c) 2019-2023 Ward C. Wheeler, Division of Invertebrate Zoology, AMNH. All rights reserved.
License     :

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

The views and conclusions contained in the software and documentation are those
of the authors and should not be interpreted as representing official policies,
either expressed or implied, of the FreeBSD Project.

Maintainer  :  Ward Wheeler <wheeler@amnh.org>
Stability   :  unstable
Portability :  portable (I hope)

Many functions not used in Complexity code when moved from matrix Eigen calculation
to QR decomposition.  THese codes also models for the code strings for
generated code

-}


module Complexity.MatrixUtilities where

import           Complexity.Constants
import           Complexity.MathUtilities
import           Complexity.Types
import           Complexity.Utilities
import           Data.List
import           Data.Maybe
import           Debug.Trace
-- import Data.Ord
-- import Control.Applicative



-- | removeColumn delets index-th (from 1) column from a matrix to form a new matrix
removeColumn :: Int -> [[a]] -> [[a]]
removeColumn index inMatrix =
  if null inMatrix then []
  else
    let row = head inMatrix
        newRow = take (index -1) row ++ drop index row
    in
    newRow : removeColumn index (tail inMatrix)

-- | moveListElement moves teh index element of a list to the end of 
-- a new list
moveListElement :: Int -> [a] -> [a]
moveListElement index inList =
  if null inList then []
  else 
      (take index inList) <> (drop (index + 1) inList) <> [inList !! index]


-- | moveRowAndColumnToEnd takes a mtrix ([[a]]) and moves
-- the inout index row and columns to last.  This used to
-- reorder matrices by putting the indel row and column
-- last as is assumed by POY/PhyG tcm format
moveRowAndColumnToEnd :: (Show a) => Maybe a -> Int -> [[a]] -> [[a]]
moveRowAndColumnToEnd setNNValue0 index inMatrix =
  
  if null inMatrix then []
  else
      let -- delted row to add back at end
          deletedRow' = inMatrix !! index

          -- row with identity cell moved to end
          -- setNNValue0 is Maybe a to set  gap tp gap value at end to 0.0 or whatever
          finalValue = if isNothing setNNValue0 then deletedRow' !! index
                       else fromJust setNNValue0
          deleteRow = (take index deletedRow') <> (drop (index + 1) deletedRow') <> [finalValue]
          deleteRowLL = fmap (:[]) deleteRow

          -- matric wihtout row and column to be moved (+1 since indexed from one for some reason)
          remainderMatrix = removeRowAndColumn (index + 1) (index + 1) inMatrix

          -- add cells from delted row to end of remainderMatrix rows
          newMatrix1 = zipWith (<>) remainderMatrix deleteRowLL

          -- add deleteRow as last row to new matrix
          newMatrix2 = newMatrix1 <> [deleteRow]
      in
      -- trace ("MRCE: " <> (show index) <> "\n" <> (show remainderMatrix) <> "\n" <> (show inMatrix) <> "\n" <> (show newMatrix2) <> "\n") $ 
      newMatrix2

-- | removeRowAndColumn counting from 1 used in general cofactor
-- for matrix inversion
removeRowAndColumn :: Int -> Int -> [[a]] -> [[a]]
removeRowAndColumn row column inMatrix =
  if null inMatrix then []
  else
    --remove row
    let firstMatrix = take (row -1) inMatrix ++ drop row inMatrix
        secondMatrix = removeColumn column firstMatrix
    in
    secondMatrix

-- | getCofactors returns the cofactors of a matrix for detetminant
-- calculation
getCofactor1 :: Int -> Matrix -> Double
getCofactor1 jIndex inMatrix
  | null inMatrix = error "Null matric in getCofactor"
  | jIndex > length (head inMatrix) = 0
  | otherwise =
      let firstRow = head inMatrix
          aij = (firstRow !! (jIndex - 1))
          cij = power (-1) (1 + jIndex) 0
          mij = removeColumn jIndex (tail inMatrix)
      in
      (aij * cij * determinantNumerical mij) + getCofactor1 (jIndex + 1) inMatrix


-- | dummy determinant function to set up file
-- assumes it is square (check earlier)
determinantNumerical :: Matrix -> Double
determinantNumerical inMatrix
  | length (head inMatrix) < 2 = error ("Matrix of order " ++ show (length $ head inMatrix))
  | length (head inMatrix) == 2 =
      let [a,b] = head inMatrix
          [c,d] = last inMatrix
      in
      a*d - b*c
  | otherwise = --use cofactors
      getCofactor1 1 inMatrix


-- | matrixMultiplyScalar multiplies a matrix by a scalar value
matrixMultiplyScalar :: Double -> Matrix -> Matrix
matrixMultiplyScalar a b =
  if null b then []
  else
    let first = head b
        newRow = fmap (a *) first
    in
    newRow : matrixMultiplyScalar a (tail b)

-- | getRow creates row of inverted matrix
getRow :: Int -> Int -> Matrix -> [Double]
getRow row column aMatrix =
  if column > length (head aMatrix) then []
  else
    let invAIJ = power (-1) (column + row) 0 * determinantNumerical (removeRowAndColumn column row aMatrix)
    in
    invAIJ : getRow row (column + 1) aMatrix

-- | invertMatrix inverts matrix by cofactors
-- A^{-1} = C^T / detA
-- A^{-1}_{ij} =  C_{ji}/detA
-- initialize with i=row=1 j=column=1
invertMatrix :: Matrix -> Matrix
invertMatrix aMatrix = invertMatrixExt aMatrix
  {-
  let cMatrix = cofactorTMatrix aMatrix 1
      detA = determinantNumerical aMatrix --determinant aMatrix
      detARecip = 1 /  detA
  in
  if detA == 0 then error "Matrix is singular and cannot be inverted"
  else if abs detA < Complexity.Constants.epsilon then error ("Matrix is nearly singular (det = " ++ show detA ++ ") and may blow up")
  else matrixMultiplyScalar detARecip cMatrix
  -}


-- | gets Transpose cofacrtor matrix C^T for matrix inversion
cofactorTMatrix :: Matrix -> Int -> Matrix
cofactorTMatrix aMatrix row =
  if row > length (head aMatrix) then []
  else
    let cofARow = getRow row 1 aMatrix
    in
    cofARow : cofactorTMatrix aMatrix (row + 1)


-- | central calcualtion of pr_{i,j}(t)
--counts from 0, 0, x
--Key the COLUMNS correspond to eigenvectors
-- Striomer and vonHessler lambda i is column vector i
--for (column, row) indexing whereas usually (row, column)
getPij :: [Double] -> Matrix -> Matrix -> Double -> Int -> Int -> Int -> Int -> Double
getPij eigenVectorList uMatrix uInvMatrix time mLambda iRow jColumn iterations =
  if mLambda == length uMatrix then 0
  else
    let lambda = eigenVectorList !! mLambda
        umi = (uMatrix !! iRow) !! mLambda
        uInvjm = (uInvMatrix !! mLambda) !! jColumn
        result = (expE (lambda * time) * umi * uInvjm)
    in
    result + getPij eigenVectorList uMatrix uInvMatrix time (mLambda + 1) iRow jColumn iterations

-- | adjustSym taks a matrix and row number and makes new symmetric row
-- makes small negative values pos epsilon
adjustSym :: Matrix -> Int -> Int -> [Double]
adjustSym inMatrix row column
  | row == length inMatrix = []
  | column == length inMatrix = adjustSym inMatrix (row + 1) 0
  | column == row = ((inMatrix !! row) !! column) : adjustSym inMatrix row (column + 1)
  | otherwise =
      let first = (inMatrix !! row) !! column
          second = (inMatrix !! column) !! row
          newValue = (first + second) / 2
      in
      if newValue > 0 then newValue : adjustSym inMatrix row (column + 1)
      else Complexity.Constants.epsilon : adjustSym inMatrix row (column + 1)


-- | adjust probabiliteis makes some adjustments for numerical issues
-- Pij = Pji enforced by average
-- small negative become epsilon positive
-- starts at 0
adjustProbs :: Matrix -> Int -> Matrix
adjustProbs inMatrix row =
  if row == length inMatrix then []
  else
    let newRow = adjustSym inMatrix row 0
    in
    newRow : adjustProbs inMatrix (row + 1)


-- | adjustDiag adjusts diagnoal values to sum to 1.0
-- start at 0
adjustDiag :: Matrix -> Matrix -> Int -> Matrix
adjustDiag inMatrix fullMatrix row =
  if row == length fullMatrix then []
  else
    let firstRow = head inMatrix
        first = take row firstRow
        second = sum firstRow - ((fullMatrix !! row) !! row)
        third = drop (row + 1) firstRow
    in
    (first ++ [1 - second] ++ third) : adjustDiag (tail inMatrix) fullMatrix (row + 1)

-- |trapezoidIntegration uses trapezoid rule to integrate the product of
-- 2 functions 1) from matrix solutions to Markov and 2) probability distribution
-- function on time.
-- assumes starting t  [0, maxValue]
-- the initial funtion is from the matrix stuff.
-- the second function is getUniforPDF or getExponentialPDF type
-- signatures
-- give it row and column of Prob(t)
-- kMultiplier is for rate class calculation---only used in the model prob function--not in branch
-- should branch not have kWeight?
trapezoidIntegration :: ([Double] -> [[Double]] -> [[Double]] -> Double -> Int -> Int -> Int -> Int -> Double) -> (Double -> Int -> Double -> Double) -> [Double] -> [[Double]] -> [[Double]]-> Int -> Int -> Double -> Double -> Int -> Int -> Double -> Double
trapezoidIntegration pijFun distFun eigenValueList uMatrix uInvMatrix iRow jColumn probDistParam maxValue iterations counter kWeight =
  if counter >= iterations then 0
  else
    let interval = maxValue / fromIntegral iterations
        time = fromIntegral counter * interval
        pij = pijFun eigenValueList uMatrix uInvMatrix (time * kWeight) 0 iRow jColumn iterations
        pij' = pijFun eigenValueList uMatrix uInvMatrix ((time * kWeight)  + interval) 0 iRow jColumn iterations
        probTime = distFun probDistParam iterations time
        probTime' = distFun probDistParam iterations (time + interval)
        value = interval * ((pij * probTime) + (pij' * probTime')) / 2
    in
    if value < 0 then
      0 + trapezoidIntegration pijFun distFun eigenValueList uMatrix uInvMatrix iRow jColumn probDistParam maxValue iterations (counter + 1) kWeight
      -- Hope this is OK
      -- if abs value < epsilon then 0 + trapezoidIntegration pijFun distFun eigenValueList uMatrix uInvMatrix iRow jColumn probDistParam maxValue iterations (counter + 1) kWeight
      -- else error ("Negative probability in trapezoid " ++ show uMatrix ++ "\n" ++ show uInvMatrix ++ "\n" ++ show iRow ++ " " ++ show jColumn ++ " " ++ show counter ++ " " ++ show interval ++ " " ++ show time ++ " " ++ " " ++ " " ++ show value)
    else
      value + trapezoidIntegration pijFun distFun eigenValueList uMatrix uInvMatrix iRow jColumn probDistParam maxValue iterations (counter + 1) kWeight


-- | makeGTRMatrixInternal makes GTR matrix using local functions
-- and returning eigen values, vectors and vector matrix inversion
-- the eigenvalues are sorted so largest is last and set to 0.
makeGTRMatrixLocal :: Int -> [[Double]] -> [Double]-> ([Double], [[Double]], [[Double]])
makeGTRMatrixLocal alphabetSize rMatrixIn piVectorIn =
    let newR = split2Matrix alphabetSize $ isPosR rMatrixIn alphabetSize 0 0
        newR2 = regularizeR newR
        qMatrix = split2Matrix alphabetSize $ makeQ newR2 piVectorIn alphabetSize 0 0
        qMatrix2 = split2Matrix alphabetSize $ addDiagValues qMatrix alphabetSize 0 0
        isSymmQ = checkSymmetryQ qMatrix2 piVectorIn alphabetSize 0 0
    in
    if not isSymmQ then error ("Q matrix not time reversible : " ++ show qMatrix)
    else
        let (qFacMatrix, rFacMatrix, eigenVectors) = qrFactorization qMatrix2 (makeDiagMatrix (length qMatrix2) 1) 0
            rqMatrix = matrixMultiply rFacMatrix qFacMatrix
            eigenVals = getDiagValues rqMatrix 0
            eigenVectorsInverse = invertMatrix eigenVectors
        in
        --trace ("MGTRL: " <> (show eigenVals)) $
        (eigenVals, eigenVectors, eigenVectorsInverse)

-- | isPosR take input matrix (ignores diagonals) and checks that all >0,
isPosR :: [[Double]] -> Int -> Int -> Int -> [Double]
isPosR inR alphSize row column
  | null inR = error "Null R Matrix in isPosR"
  | row == alphSize = []
  | column == alphSize = isPosR inR alphSize (row + 1) 0
  | row == column = 0 : isPosR inR alphSize row (column + 1)
  | ((inR !! row) !! column) < 0 = error ("Element (" ++ show row ++ "," ++ show column ++ ") in R matrix <0 : " ++ show inR)
  | otherwise = ((inR !! row) !! column) : isPosR inR alphSize row (column + 1)

-- | checkSymmetryQ checks Qij=Qji
checkSymmetryQ :: [[Double]] -> [Double] -> Int -> Int -> Int -> Bool
checkSymmetryQ qMatrix piVector alphSize row column
  | row == alphSize = True
  | column == alphSize = checkSymmetryQ qMatrix piVector alphSize (row + 1) 0
  | row == column = checkSymmetryQ qMatrix piVector alphSize row (column + 1)
  | abs (((piVector !! row) * ((qMatrix !! row) !! column)) - ((piVector !! column) * ((qMatrix !! column) !! row))) > Complexity.Constants.epsilon = error ("Element (" ++ show row ++ "," ++ show column ++ ") in Q matrix not symmetrical : " ++ show qMatrix)
  | otherwise = checkSymmetryQ qMatrix piVector alphSize row (column + 1)

-- | regularizePi take input Pi vector and checks that all >0,
-- sums to 1.
regularizePi :: [Double] -> [Double]
regularizePi inPi =
    if null inPi then error "Null Pi Vector in regularizePi"
    else
        let allPos = foldl' (&&) True $ fmap (>0) inPi
            total = sum inPi
            newPi = fmap (/total) inPi
        in
        if allPos then newPi
        else error ("Element in pi vector <0 : " ++ show inPi)


-- | regularizeR takes R matrix and makes param values average 1.0
regularizeR :: [[Double]] -> [[Double]]
regularizeR rMatrix =
    if null rMatrix then error "Error null input makeQ"
    else
        let alphabetSize = fromIntegral $ length rMatrix
            factor = ((alphabetSize * alphabetSize) - alphabetSize)/2
            transitions = fmap (takeWhile (> 0)) rMatrix
            sumTransitions = sum $ fmap sum transitions
        in
        fmap (fmap (* (factor/sumTransitions))) rMatrix

-- | makeQ take rMatrix and piVector (regularized) and creates Q matrix
makeQ :: [[Double]] -> [Double] -> Int -> Int -> Int -> [Double]
makeQ rMatrix piVector alphSize row column
  | null rMatrix || null piVector = error "Null input to makeQ"
  | row == alphSize = []
  | column == alphSize = makeQ rMatrix piVector alphSize (row + 1) 0
  | row == column = 0 : makeQ rMatrix piVector  alphSize row (column + 1)
  | otherwise =
      let newValue = ((rMatrix !! row) !! column) * (piVector !! column)
      in
      newValue : makeQ rMatrix piVector alphSize row (column + 1)

-- | addDiagValues take Q Matrix and adds in diagnoal values to make sum row =0
addDiagValues :: [[Double]] -> Int -> Int -> Int -> [Double]
addDiagValues qMatrix alphSize row column
  | null qMatrix = error "Null input to addDiagValues"
  | row == alphSize = []
  | column == alphSize = addDiagValues qMatrix alphSize (row + 1) 0
  | row /= column = ((qMatrix !! row) !! column) : addDiagValues qMatrix alphSize row (column + 1)
  | otherwise =
      let newValue = sum (qMatrix !! row)
      in
      (-1 * newValue) : addDiagValues qMatrix  alphSize row (column + 1)

-- | getElement get an individual i,j element
getElement :: Int -> Int -> Int -> [[Double]] -> [[Double]] -> [Double]
getElement aRow counter bCols aMat bMat =
  if counter == bCols then []
  else
    let aMatRow = aMat !! aRow
        bMatColumn = fmap (!! counter) bMat
        cList = zipWith (*) aMatRow bMatColumn
    in
    sum cList :  getElement aRow (counter + 1) bCols aMat bMat

-- | getRows gets each row of new matrix recursively
getRows :: Int -> Int -> Int -> [[Double]] -> [[Double]] -> [[Double]]
getRows counter aRows bCols aMat bMat =
  if counter == aRows then []
  else
    getElement counter 0 bCols aMat bMat :  getRows (counter + 1) aRows bCols aMat bMat

-- | matrixMultiply multiplies two matrices naively
-- c_{i,j} = \Sum_{k=0}^{m-1} a_{i,k} b_{k,j}
matrixMultiply :: [[Double]] -> [[Double]] -> [[Double]]
matrixMultiply aMat bMat
  | null aMat = error "Null matrix (first arg) in matrixMultiply"
  | null bMat = error "Null matrix (first arg) in matrixMultiply"
  | otherwise =
      let aRows = length aMat
          aCols = length $ head aMat
          bRows = length bMat
          bCols = length $ head bMat
      in
      if aCols /= bRows then error ("Columns of first " ++ show aCols ++ " do not equal rows of second " ++ show bRows)
      else
        getRows 0 aRows bCols aMat bMat

-- | normalizeVectorWithSign multiplies values by sqrt(sum i^2) for all i's in vector
-- sign change used for eigen vectors
normalizeVectorWithSign :: [Double]-> [Double]
normalizeVectorWithSign a =
  if null a then error "Null vector in normalize"
  else
    let sumSquares = sqrt $ sum $ zipWith (*) a a
        minVal = minimum a
        maxVal = maximum a
    in
    if maxVal >= abs minVal then fmap (/sumSquares) a
    else fmap (/(-1 *sumSquares)) a

-- | normalizeVector multiplies values by sqrt(sum i^2) for all i's in vector
normalizeVector :: [Double]-> [Double]
normalizeVector a =
  if null a then error "Null vector in normalize"
  else
    let sumSquares = sqrt $ sum $ zipWith (*) a a
    in
    fmap (/sumSquares) a

-- | normalizeColumnVector takes list of list and normalizes
normalizeColumnVector :: [[Double]] -> [[Double]]
normalizeColumnVector colVect =
  if length (head colVect) /= 1 then error "Column vector order > 1"
  else
    let cVect = concat  colVect
        normVect = normalizeVector cVect
    in
    fmap (:[]) normVect

-- | makeDiagRow makes an individual row of a diagnonal matrix
makeDiagRow :: Int -> Double -> Int -> [[Double]]
makeDiagRow dimension value counter =
  if counter == dimension then []
  else
    let firstPart = replicate counter 0
        thirdPart = replicate (dimension - counter - 1) 0
    in
    (firstPart ++ [value] ++ thirdPart) : makeDiagRow dimension value (counter + 1)

-- | makeDiagMatrix makes diagnonal matrix of input value
makeDiagMatrix :: Int -> Double -> [[Double]]
makeDiagMatrix dimension value =
  if dimension == 0 then error "Can't make order 0 idenity matrix"
  else makeDiagRow dimension value 0

-- | subtractMatrices subtracts matrices
subtractMatrices :: [[Double]] -> [[Double]] -> [[Double]]
subtractMatrices a b
  | null a && null b = []
  | null a || null b = error ("Unequal size matrix(ces) in subtractMatrices" ++ show a ++ " " ++ show b)
  | otherwise =
      let a0 = head a
          b0 = head b
          c0 = zipWith (-) a0 b0
      in
      c0 : subtractMatrices (tail a) (tail b)


-- | transposeMatrix exchanges row and column values
-- ued to convert eigenvectors as rows matrix to eigenvectors as columns.
transposeMatrix :: [[Double]] -> [[Double]]
transposeMatrix a =
  if null $ head a then []
  else
    let row = fmap head a
    in
    row : transposeMatrix (fmap tail a)


-- | makeMatrixMinor removes rows and columns to create minor
-- indexed from 0,0
makeMatrixMinor :: Int -> Int -> [[Double]] -> [[Double]]
makeMatrixMinor row column aMatrix =
  if null aMatrix then error "Null matrix in makeMatrixMinor"
  else
    let aRows = length aMatrix
        aColumns = length $ head aMatrix
    in
    if row >= aRows then error "Minor too small (rows) in makeMatrixMinor"
    else if column >= aColumns then error "Minor too small (columns) in makeMatrixMinor"
    else
      let fewerRows = drop (row +1) aMatrix
      in
      fmap (drop (column + 1)) fewerRows

-- | addMatrices adds matrices
addMatrices :: [[Double]] -> [[Double]] -> [[Double]]
addMatrices a b
  | null a && null b = []
  | null a || null b = error ("Unequal size matrix(ces) in addMatrices" ++ show a ++ " " ++ show b)
  | otherwise =
      let a0 = head a
          b0 = head b
          c0 = zipWith (+) a0 b0
      in
      c0 : addMatrices (tail a) (tail b)

-- | euclidNorm sum square of vector elements
euclidNorm :: [Double] -> Double
euclidNorm inVector =
  if null inVector then error "Null vector in euclidNorm"
  else
    sqrt (sum $ zipWith (*) inVector inVector)


-- | makeEVector make vector of [0....,1,..0]
makeEVector :: Int -> Int -> Double -> [Double]
makeEVector dimension position value =
  if dimension == 0 then error "Null matrix requested in makeEVector"
  else
    let firstPart = replicate position 0
        thirdPart = replicate (dimension - position - 1) 0
    in
    (firstPart ++ [value] ++ thirdPart)

-- getColumnVector gets column i (0-indexed) as
-- column vector (matrix of order nx1) from matrix
getColumnVector :: Int -> [[Double]] -> [[Double]]
getColumnVector column aMatrix =
  if null aMatrix then error "Null matrix in getColumnVector"
  else
    let numCols = length $ head aMatrix
    in
    if column >= numCols then error "Column number larger than matrix in getColumnVector"
    else
      let columnVals = fmap ((:[]) . (!! column)) aMatrix
      in
      columnVals

-- | getHouseHolder takes a matrix and column number to create Housholder matrix
-- for QR
getHouseholder :: [[Double]]-> [[Double]]
getHouseholder aMatrix  =
  if null aMatrix then error "Null matrix in getHouseHolder"
  else
    let e1 = transposeMatrix [makeEVector (length aMatrix) 0 1]
        a1 = getColumnVector 0 aMatrix
        alpha = euclidNorm (concat a1)
        ae1 = matrixMultiplyScalar alpha e1
        uVect = subtractMatrices a1 ae1
        vVect = normalizeColumnVector uVect
        idenityMatrix = makeDiagMatrix (length aMatrix) 1
        twoXvvT = matrixMultiplyScalar 2 $ matrixMultiply vVect (transposeMatrix vVect)
        qMatrix = subtractMatrices idenityMatrix twoXvvT
    in
    qMatrix

-- | padOutMinor adds 1 on diagnoal and elsewhere 0's
-- to pad out a minor matrix into full size (dimension x dimension)
-- for Householder
-- assumes square
padOutMinor :: [[Double]] ->Int -> [[Double]]
padOutMinor inMinor dimension =
  if null inMinor then makeDiagMatrix dimension 1 --identity matrix
  else
    let minorDim = length inMinor
    in
    if minorDim == dimension then inMinor -- nothing to do
    else if minorDim > dimension then error "Minor already larger than dimension"
    else
      let nRows = dimension - minorDim
          identityMatrix = makeDiagMatrix dimension 1
          newRows = take nRows identityMatrix
          newColumns = take nRows <$> drop nRows identityMatrix
          fullRows = zipWith (++) newColumns inMinor
          fullMatrix = newRows ++ fullRows
      in
      fullMatrix

-- | getHouseholderList takes a matrix and recurseively generates the list of Q, Q', Q'' etc
-- dimension-1 times (assumes square here)
getHouseholderList :: [[Double]] -> [[Double]] -> Int -> Int -> [[[Double]]]
getHouseholderList inMatrix aMatrix fullDimension counter =
  if null inMatrix then error "Null matrix in getHousehlderList"
  else
    let thisQ' = getHouseholder inMatrix
        thisQ = padOutMinor thisQ' fullDimension
        q1AMatrix = matrixMultiply thisQ aMatrix
    in
    if length thisQ' == 2 then [thisQ]
    else thisQ : getHouseholderList (makeMatrixMinor counter counter q1AMatrix) aMatrix fullDimension (counter + 1)

-- qrDecomposition performes QR decomposition using Householder reflections
-- assumes square here though not a general restriction of the method
qrDecomposition :: [[Double]] -> ([[Double]], [[Double]])
qrDecomposition aMatrix =
  if null aMatrix then error "Null matrix in qrDecomposition"
  else
    let qMatrixList = getHouseholderList aMatrix aMatrix (length aMatrix) 0
        qTList = fmap transposeMatrix qMatrixList
        qMatrix = foldl' matrixMultiply (head qTList) (tail qTList)
        rMatrix = matrixMultiply (transposeMatrix qMatrix) aMatrix
    in
    (qMatrix, rMatrix)

-- qrFactorization repeated calls qrDecomposition untril maxIteratrions or convergance
qrFactorization :: [[Double]] -> [[Double]] -> Int -> ([[Double]], [[Double]], [[Double]])
qrFactorization aMatrix uMatrix counter =
  if null aMatrix then error "Null matrix in qrFactorization"
  else
    let (qMatrix, rMatrix) = qrDecomposition aMatrix
        newA = matrixMultiply rMatrix qMatrix
        newU = matrixMultiply uMatrix qMatrix
        linAMatrix = concat aMatrix
        linNewAMatrix = concat newA
        diffVal = sum $ abs <$> zipWith (-) linAMatrix linNewAMatrix
    in
    --trace ("qrFact: " <> (show (counter, maxIterations))) $ -- <> " " <> (show (qMatrix, rMatrix)) <> (show $ length (filter (== nan) $ concat qMatrix))) $
    if length (filter (== "NaN") $ fmap show $ concat qMatrix) /= 0 then error "\n\tqrFactorization error--NaN values"
    else if (diffVal/ fromIntegral (length aMatrix * length aMatrix)) < Complexity.Constants.epsilon then (qMatrix, rMatrix, newU)
    else if counter > maxIterations then 
      trace ("Warning: Max iterations exceeded in qrFactorization")
      (qMatrix, rMatrix, newU) -- error "Max iterations exceeded in qrFactorization" -- (qMatrix, rMatrix, uMatrix)
    else qrFactorization newA newU (counter + 1)
    --)

-- | getDiagValues returns the diagonal values of a matrix
getDiagValues :: [[Double]] -> Int -> [Double]
getDiagValues inMatrix counter
  | null inMatrix = error "Null matrix in getDiagValues"
  | counter == length inMatrix = []
  | otherwise = ((inMatrix !! counter) !! counter) : getDiagValues inMatrix (counter + 1)
