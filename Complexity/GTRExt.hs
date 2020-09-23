{- |
Module      :  GTRExt
Description :  GTR functions using external numerical libraries
Copyright   :  (c) 2019-2020 Ward C. Wheeler, Division of Invertebrate Zoology, AMNH. All rights reserved.
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

module Complexity.GTRExt
  (  makeGTRMatrixExt
  ) where

import           Data.List
import           Numeric.LinearAlgebra
import           Complexity.Utilities
import           Complexity.MatrixUtilities
import           Prelude                    hiding ((<>))
-- import Debug.Trace

-- | makeGTRMatrixExt makes GTR matricx using external LAPACK/BLAS libraries
-- and returning eigen values, vectors and vector matrix inversion
makeGTRMatrixExt :: Int -> [[Double]] -> [Double]-> ([Double], [[Double]], [[Double]])
makeGTRMatrixExt alphabetSize rMatrixIn piVectorIn =
    let newR = split2Matrix alphabetSize $ isPosR rMatrixIn alphabetSize 0 0
        newR2 = regularizeR newR
        qMatrix = split2Matrix alphabetSize $ makeQ newR2 piVectorIn alphabetSize 0 0
        qMatrix2 = split2Matrix alphabetSize $ addDiagValues qMatrix alphabetSize 0 0
        isSymmQ = checkSymmetryQ qMatrix2 piVectorIn alphabetSize 0 0
    in
    if not isSymmQ then error ("Q matrix not time reversible : " ++ show qMatrix)
    else --put in Numeric.LinearAlgebra type
        let qMatLA = matrix alphabetSize $ concat qMatrix2
            (qrMatrix,rMatrix) = qr qMatLA
            aMatrix = rMatrix <> qrMatrix
            eigenVectorsRealLists = toLists qrMatrix
            eigenVectorsInverseLists = toLists $ inv qrMatrix
            eigenValsRealList = getDiagValues (toLists aMatrix) 0
        in
        (eigenValsRealList, eigenVectorsRealLists, eigenVectorsInverseLists)
    
