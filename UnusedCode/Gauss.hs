{- |
Module      :  Gauss 
Description :  Gaussian elimination functions 
Copyright   :  (c) 2019 Ward C. Wheeler, Division of Invertebrate Zoology, AMNH. All rights reserved.
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

-}
module Complexity.Gauss where

import Data.List
import Debug.Trace
import Complexity.Types
import Complexity.Constants
import qualified Numeric.LinearAlgebra as NLA
import Data.Maybe

-- |getEigenVector Gets Eigenvector vis Gaussian elimination
--with last step modified to have last element =1
--and rest ratios
-- Modified from https://haskellicious.wordpress.com/2012/11/26/the-gauss-algorithm-in-haskell/
-- seems tohave some numerical issues
getEigenVector :: Matrix -> Vector
getEigenVector a = 
  if (length $ head a) == 2 then -- 2x2
    [-1 * (last $ head a) / (head $ head a), 1]
  else
    --trace ("Bigger than 2x2")
    reverse . resubstitute . reverse . fmap reverse $ triangularEigen $ fmap (++ [0]) a

triangularEigen :: Matrix -> Matrix
triangularEigen [] = []
triangularEigen m  = 
    --row:(triangularEigen rows')
    --where
    let (row:rows) = rotatePivot m    -- see discussion below
        rows' = fmap f rows
        f bs
          | (head bs) <= epsilon = drop 1 bs  --floating point zero
          | otherwise      = drop 1 $ zipWith (-) (fmap (*c) bs) row
          where 
          c = (head row)/(head bs)  
    in 
    row:(triangularEigen rows')

rotatePivot :: Matrix -> Matrix
rotatePivot (row:rows) =
    --trace ("pivot") (
    if (abs $ head row) > 0 then (row:rows) -- watch for 0 pivot
    else rotatePivot (rows ++ [row])
    --)

-- | this only for eigenvectors--makes last 1 so rest are ratios
resubstitute :: Matrix -> [Double]
resubstitute [] = []
resubstitute (row:rows) = 
    --trace ("Subsitute " ++ show row ++ " " ++ show rows) (
    if head row == 0 then -- looks for first entry 0
      --trace ("In") (
      let x = 1 -- last row
          rows' = fmap substituteUnknown rows
          substituteUnknown (a1:(a2:as')) = ((a1-x*a2):as')
      in
      x:(resubstitute rows')
      --)
    else 
      --trace ("out") (
      let x     = (head row)/(last row)
          rows' = fmap substituteUnknown rows
          substituteUnknown (a1:(a2:as')) = ((a1-x*a2):as')
      in
      x:(resubstitute rows')
      --)
      --) 

-- from https://luckytoilet.wordpress.com/2010/02/21/solving-systems-of-linear-equations-in-haskell/
gaussianReduce :: Matrix -> Matrix
gaussianReduce matrix = fixlastrow $ foldl reduceRow matrix [0..(length matrix) -1]

-- | swaps element at position a with element at position b
swap :: Matrix -> Int -> Int -> Matrix 
swap xs a b =
  if a > b then swap xs b a
  else if a == b then xs
  else --a < b  
      let (p1,p2) = splitAt a xs
          (p3,p4) = splitAt (b-a-1) (tail p2)
      in 
      p1 ++ [xs!!b] ++ p3 ++ [xs!!a] ++ (tail p4)
 
reduceRow :: Matrix -> Int -> Matrix
reduceRow matrix1 r = 
    --first non-zero element on or below (r,r).
    let firstnonzero = head $ filter (\x -> matrix1 !! x !! r /= 0) [r..length matrix1-1]
        --matrix with row swapped (if needed)
        matrix2 = swap matrix1 r firstnonzero
        --row we're working with
        row = matrix2 !! r
        --make it have 1 as the leading coefficient
        row1 = map (\x -> x / (row !! r)) row
        --subtract nr from row1 while multiplying
        subrow nr = let k = nr!!r in zipWith (\a b -> k*a - b) row1 nr
        --apply subrow to all rows below
        nextrows = map subrow $ drop (r+1) matrix2
        --concat the lists and repeat
    in 
    take r matrix2 ++ [row1] ++ nextrows
 
fixlastrow :: Matrix -> Matrix
fixlastrow matrix' = 
  let a = init matrix'; row = last matrix'; z = last row; nz = last (init row)
  in 
  a ++ [init (init row) ++ [1, z / nz]]

 --Solve a matrix (must already be in REF form) by back substitution.
substitute :: Matrix -> Vector
substitute matrix = foldr next [last (last matrix)] (init matrix) where
  next row found = 
   let subpart = init $ drop (length matrix - length found) row
       solution = last row - sum (zipWith (*) found subpart)
   in 
   solution : found

-- infinite to 1 is there to make the last vaiable 1 since the vectors are not
-- unique
solveEigen :: Matrix -> Vector
solveEigen = substitute . (fmap (fmap infiniteTo1)) . gaussianReduce

-- | infinite21 converts infinity values to 1.0
-- used in Gaussian elimination since underdetermined
infiniteTo1 :: Double -> Double
infiniteTo1 a = 
  if isInfinite a then 1.0
  else a

{-
 -- | gaussJordanFinish takes REF form augmented matrix and completes subsitution 
 gaussJordanFinish :: Matrix -> Matrix 
 gaussJordanFinish a =
    if null a then errro "Null matrix in gaussJordanFinish"
    else 
  -}    

-- | makeSquare takes a row of matrix and multiplies last element by (-1)
-- used for eigenvector solver
makeSquare :: [Double] -> [Double]
makeSquare inRow =
  if null inRow then error "Null row in makeSquare"
  else (init inRow) ++ [-1 * (last inRow)] 

-- | solveEigenExt uses external linear system solver to get
-- eigenvectors
-- makes matrix square by reversing sign of last column.  This
-- sets teh last vector element to 1 and solves the  first n-1 
-- eigenvector values, then 1 is appended to the end to complete the vector
-- before normalization
-- solutionVector usually equals [0...] but for repeated eigenvalues
-- can be sert to previous eigenvector solution
solveEigenExt :: [Double] -> [[Double]] -> [Double]
solveEigenExt prevEigenVector inMatrix =
  trace ("InMatrix " ++ show inMatrix) (
  if null inMatrix then error "Null matrix in solveEigenExt"
  else 
    -- multiple last column by (-1)
    let alphabetSize = length inMatrix
        squareMatrix = take (alphabetSize -1) $ fmap init inMatrix
        -- take into account solutionVector
        resultMatrix = take (alphabetSize -1) $ zipWith (-) prevEigenVector (fmap last inMatrix)
        firstPart = NLA.matrix (alphabetSize -1) $ concat squareMatrix
        secondPart = NLA.matrix 1  resultMatrix
        eigenVector = NLA.linearSolveSVD firstPart secondPart
        eigenVectorList  =  ((concat $ NLA.toLists eigenVector) ++ [1])
        {-t 
        firstPart = NLA.matrix alphabetSize $ concat inMatrix
        secondPart = NLA.matrix 1  $ replicate alphabetSize 0
        eigenVector = NLA.linearSolveSVD firstPart secondPart
        -}
        
    in
    --if isNothing eigenVector then error "Singular matrix in solveEigen"
    --else 
      trace ("first " ++ show firstPart ++ "\nsecond " ++ show secondPart ++ "\nEV " ++ show eigenVectorList)-- ((head $ NLA.toLists eigenVector))) 
      eigenVectorList
    
    )
 
 -- | solveEigenVectorsWithRepeat solves for a list of eigenvalues and vectors and 
 -- accomodates repeat eigenvalues is egenvector determination
solveEigenVectorsWithRepeat :: Double -> [Double] -> [Double] -> [[[Double]]] -> [[Double]]
solveEigenVectorsWithRepeat prevEigenValue prevEigenVector eigenValueList inMatrixList =
  if null eigenValueList then []
  else 
    let curEigenValue = head eigenValueList
        curMatrix = head inMatrixList
        zeroVector = replicate (length curMatrix) 0
    in
    trace ("SEVWR " ++ show prevEigenValue ++ " " ++ show prevEigenVector ++ " " ++ show curEigenValue ++ " " ++ show ((abs $ (abs prevEigenValue) -  (abs curEigenValue)) > (0.1))) (
    if (abs $ (abs prevEigenValue) -  (abs curEigenValue)) > (0.1) then 
      let curEigenVector = solveEigenExt zeroVector curMatrix
      in
      curEigenVector : solveEigenVectorsWithRepeat curEigenValue curEigenVector (tail eigenValueList) (tail inMatrixList) 
    else 
      let newMatrix = NLA.matrix (length curMatrix) $ concat curMatrix
          newMatrixSquared = newMatrix NLA.<> newMatrix
          curMatrixSquared =  NLA.toLists newMatrixSquared
          curEigenVector = solveEigenExt zeroVector curMatrixSquared
      in
      trace ("CEV " ++ show curEigenVector)
      curEigenVector : solveEigenVectorsWithRepeat curEigenValue curEigenVector (tail eigenValueList) (tail inMatrixList) 
      )

