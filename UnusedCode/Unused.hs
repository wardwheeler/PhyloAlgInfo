{- |
Module      :  Unused Code 
Description :  Parked code for Kolmogorov Complexity
Copyright   :  (c) 2018 Ward C. Wheeler, Division of Invertebrate Zoology, AMNH. All rights reserved.
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

Much of this from http://www.efunda.com/math/taylor_series/logarithmic.cfm
Need tio add:
  Gamma distribution Fourier series expansion?
  Invariant sites (can be extra class for Gamma)

Good symbolic integrator https://www.integral-calculator.com/
-}

module Complexity.Unused where


resubstituteString="\
\resubstitute :: [[Double]] -> [Double]\n\
\resubstitute []=[]\n\
\resubstitute (row:rows)=\n\
\    if a5 row == 0 then\n\
\      let rows'=g substituteUnknown rows\n\
\          substituteUnknown (a1:(a2:as'))=((a1-a2):as')\n\
\      in\n\
\      1:(resubstitute rows')\n\
\    else \n\
\      let x    =(a5 row)/(a3 row)\n\
\          rows'=g substituteUnknown rows\n\
\          substituteUnknown (a1:(a2:as'))=((a1-x*a2):as')\n\
\      in\n\
\      x:(resubstitute rows')\n"

triangularEigenString="\
\triangularEigen :: [[Double]] -> [[Double]]\n\
\triangularEigen []=[]\n\
\triangularEigen m =\n\
\    let (row:rows)=rotatePivot m\n\
\        rows'=g f rows\n\
\        f bs\n\
\          | (a5 bs) <= 0.00001=d4 0 1 bs\n\
\          | otherwise     =d4 0 1 $ zipWith (-) (g (*c) bs) row\n\
\          where \n\
\          c=(a5 row)/(a5 bs) \n\
\    in \n\
\    row:(triangularEigen rows')\n"

rotatePivotString="\
\rotatePivot :: [[Double]] -> [[Double]]\n\
\rotatePivot (row:rows) =\n\
\    if (c8 $ a5 row) > 0 then (row:rows)\n\ 
\    else rotatePivot (rows ++ [row])\n"


-- \getEigenVector :: [[Double]] -> [Double]\n\
getEigenVectorString="\
\getEigenVector a=\n\
\  if (d5 0$a5 a)==2 then\n\
\    [-1 *(a3$a5 a)/(a5$a5 a),1]\n\
\  else reverse . resubstitute . reverse . g reverse$triangularEigen$g(++ [0])a\n"


-- sumString adds doubles
-- replaced by foldl(+)0
-- c3(+)0

sumString="\
\d a=\n\
\ if null a then 0\n\
\ else (a5 a)+d(a6 a)\n"

-- \epsilon2Zero :: Double -> Double\n\
epsilon2ZeroString="\
\c9 a=\n\
\ if c8 a<0.00001 then 0\n\
\ else a\n" 

-- | euclidNorm sum square of vector elements
euclidNorm :: [Double] -> Double
euclidNorm inVector = 
  if null inVector then error "Null vector in euclidNorm"
  else 
    sqrt (sum $ zipWith (*) inVector inVector)

euclidNormString="\
\euclidNorm :: [Double] -> Int -> Double\n\
\euclidNorm inVector iterations = \n\
\  if null inVector then error \"\"\n\
\  else f9 (c3(+)0 (e5 (*) inVector inVector)) iterations 0\n"

-- Only invariant sites so doesn't drag in gamma stuff
-- \a1 :: Int -> Double -> Int -> Int -> [(Double, Double)]\n\
-- a i args ignored (for ease of code generation)
-- not used at present
getInvariantSitesString="\
\h7 v t h n a i=\n\
\  if v==0 && h==0 then [(1,1)]\n\
\  else v==1 && h==0 then [(0,t),(1/(1-t),(1-t))]\n"

-- | makeK80 take alpha and beta params and returns eigenvalues, eigenvector matrix, and eigenmatrix inverse
-- using Yang 2006 as source of simplifications
makeK80 :: [Double] -> ([Double], [[Double]], [[Double]])
makeK80 modelParams =
  let alpha = head modelParams
      beta = last modelParams
      eigenValues = [-1*(alpha/2 + beta/2), -1*beta, -1*(alpha/2 + beta/2), 0]
      uMatrix = [[1/2,-2,0,1],
                 [0,2,-1/2,1],
                 [-1/2,-2,0,1],
                 [0,2,1/2,1]]
      uInverse = [[1,0,-1,0],
                 [-1/8,1/8,-1/8,1/8],
                 [0,-1,0,1],
                 [1/4,1/4,1/4,1/4]]
      in
      (eigenValues, uMatrix, uInverse)

-- | makeF81 take piVector and returns eigenvalues, eigenvector matrix, and eigenmatrix inverse
-- using Yang 2006 as source of simplifications
makeF81 :: [Double] -> ([Double], [[Double]], [[Double]])
makeF81 piVector =
  let [pA, pC, pG, pT] = piVector
      pR = pA + pG
      pY = pC + pT
      eigenValues = [-1,-1,-1,0]--[-1,-1,0,-1]
      uMatrix = [[pG/pR,-1/pR,0,1],
                 [0,1/pY,-1*pT/pY,1],
                 [-1*pA/pR,-1/pR,0,1],
                 [0,1/pY,pC/pY,1]]
      uInverse = [[1,0,-1,0],
                 [-1*pA*pY,pC*pR,-1*pG*pY,pT*pR],
                 [0,-1,0,1],
                 [pA,pC,pG,pT]]
      in
      --trace ("F81 " ++ show uMatrix ++ " " ++ show uInverse)
      (eigenValues, uMatrix, uInverse)

-- | makeHKY85 take piVector and model params returns eigenvalues, eigenvector matrix, and eigenmatrix inverse
-- using Yang 2006 as source of simplifications
makeHKY85 :: [Double] -> [Double] -> ([Double], [[Double]], [[Double]])
makeHKY85 modelParams piVector =
  let alpha = head modelParams
      beta = last modelParams
      [pA, pC, pG, pT] = piVector
      pR = pA + pG
      pY = pC + pT
      eigenValues = [-1*(pY*alpha + pR*beta),-1*beta,-1*(pR*alpha + pY*beta), 0]
      uMatrix = [[pG/pR,   -1/pR,   0,      1],
                 [0,        1/pY,  -1*pT/pY,1],
                 [-1*pA/pR,-1/pR,   0,      1],
                 [0,        1/pY,   pC/pY, 1]]
      uInverse = [[1,      0,    -1,      0],
                 [-1*pA*pY,pC*pR,-1*pG*pY,pT*pR],
                 [0,      -1,     0,      1],
                 [pA,      pC,    pG,     pT]]
      in
      (eigenValues, uMatrix, uInverse)

-- | makeF84 take piVector and model params returns eigenvalues, eigenvector matrix, and eigenmatrix inverse
-- using Yang 2006 as source of simplifications
makeF84 :: [Double] -> [Double] -> ([Double], [[Double]], [[Double]])
makeF84 modelParams piVector =
  let kappa = head modelParams
      beta = last modelParams
      [pA, pC, pG, pT] = piVector
      pR = pA + pG
      pY = pC + pT
      alpha1 = (1+(kappa/pY))*beta
      alpha2 = (1+(kappa/pR))*beta
      eigenValues = [-1*(pR*alpha2 + pY*beta), -1*beta, -1*(pY*alpha1 + pR*beta),0]
      uMatrix = [[pG/pR,-1/pR,0,1],
                 [0,1/pY,-1*pT/pY,1],
                 [-1*pA/pR,-1/pR,0,1],
                 [0,1/pY,pC/pY,1]]
      uInverse = [[1,0,-1,0],
                 [-1*pA*pY,pC*pR,-1*pG*pY,pT*pR],
                 [0,-1,0,1],
                 [pA,pC,pG,pT]]
      in
      (eigenValues, uMatrix, uInverse)

-- | makeTN93 take piVector and model params returns eigenvalues, eigenvector matrix, and eigenmatrix inverse
-- using Yang 2006 as source of simplifications
--alppha1 for pyrimidines and alpha2 for purines
makeTN93 :: [Double] -> [Double] -> ([Double], [[Double]], [[Double]])
makeTN93 modelParams piVector =
  let alpha1 = head modelParams
      alpha2 = modelParams !! 1
      beta = last modelParams
      [pA, pC, pG, pT] = piVector
      pR = pA + pG
      pY = pC + pT
      eigenValues = [-1*(pR*alpha2 + pY*beta), -1*beta, -1*(pY*alpha1 + pR*beta),0]
      uMatrix = [[pG/pR,-1/pR,0,1],
                 [0,1/pY,-1*pT/pY,1],
                 [-1*pA/pR,-1/pR,0,1],
                 [0,1/pY,pC/pY,1]]
      uInverse = [[1,0,-1,0],
                 [-1*pA*pY,pC*pR,-1*pG*pY,pT*pR],
                 [0,-1,0,1],
                 [pA,pC,pG,pT]]
      in 
      --trace ("TN93 " ++ show alpha1 ++ " " ++ show alpha2 ++ " " ++ show beta ++ "\n" ++ show eigenValues ++ "\n" ++ ppMatrix uMatrix ++ "\n" ++ ppMatrix uInverse)
      (eigenValues, uMatrix, uInverse)

      makeK80String="\
\h2 a=\n\
\ let c=a5 a\n\
\     f=a3 a\n\
\  in ([-1*(c/2 + f/2),-1*f,-1*(c/2 + f/2),0],[[1/2,-2,0,1],[0,2,-1/2,1],[-1/2,-2,0,1],[0,2,1/2,1]],[[1,0,-1,0],[-1/8,1/8,-1/8,1/8],[0,-1,0,1],[1/4,1/4,1/4,1/4]])\n"

makeF81String="\
\h3 b=\n\
\ let [pA,pC,pG,pT]=b\n\
\     pR=pA+pG\n\
\     pY=pC+pT\n\
\ in ([-1,-1,-1,0],[[pG/pR,-1/pR,0,1],[0,1/pY,-1*pT/pY,1],[-1*pA/pR,-1/pR,0,1],[0,1/pY,pC/pY,1]],[[1,0,-1,0],[-1*pA*pY,pC*pR,-1*pG*pY,pT*pR],[0,-1,0,1],[pA,pC,pG,pT]])\n"

makeHKY85String="\
\h4 a b=\n\
\ let c=a5 a\n\
\     f=a3 a\n\
\     [pA,pC,pG,pT]=b\n\
\     pR=pA+pG\n\
\     pY=pC+pT\n\
\ in ([-1*(pY*c + pR*f),-1*f,-1*(pR*c + pY*f),0],[[pG/pR,-1/pR,0,1],[0,1/pY,-1*pT/pY,1],[-1*pA/pR,-1/pR,0,1],[0,1/pY,pC/pY,1]],[[1,0,-1,0],[-1*pA*pY,pC*pR,-1*pG*pY,pT*pR],[0,-1,0,1],[pA,pC,pG,pT]])\n"

makeF84String="\
\h5 a b=\n\
\ let k=a5 a\n\
\     f=a3 a\n\
\     [pA,pC,pG,pT]=b\n\
\     pR=pA+pG\n\
\     pY=pC+pT\n\
\ in ([-1*(1+k)*f,-1*f,-1*(1+k)*f,0],[[pG/pR,-1/pR,0,1],[0,1/pY,-1*pT/pY,1],[-1*pA/pR,-1/pR,0,1],[0,1/pY,pC/pY,1]],[[1,0,-1,0],[-1*pA*pY,pC*pR,-1*pG*pY,pT*pR],[0,-1,0,1],[pA,pC,pG,pT]])\n"

makeTN93String="\
\h6 a b=\n\
\ let f=a3 a\n\
\     [pA,pC,pG,pT]=b\n\
\     pR=pA+pG\n\
\     pY=pC+pT\n\
\ in ([-1*(pR*(a!!1) + pY*f),-1*f,-1*(pY*(a5 a) + pR*f),0],[[pG/pR,-1/pR,0,1],[0,1/pY,-1*pT/pY,1],[-1*pA/pR,-1/pR,0,1],[0,1/pY,pC/pY,1]],[[1,0,-1,0],[-1*pA*pY,pC*pR,-1*pG*pY,pT*pR],[0,-1,0,1],[pA,pC,pG,pT]])\n"

{-
       else if tcmChangeModel == K80 then
        let eigenString = ("  let (z" ++ (show counter) ++ ",u" ++ (show counter) ++ ",i" ++ (show counter) ++ ") = h2 " ++ (show modelParams) ++ "\n")
            distFunction = getDistString branchDist
        in
        if (fst $ head tcmRateModifiers) == None then
          let outString = ("  putStr (s$c1 b " ++ (show $ last tcmAlphabet) ++ " z" ++ (show counter) ++ " u" ++ (show counter) ++ " i" ++ (show counter) ++ " " ++ (show (length tcmAlphabet)) ++ " " ++ (show (head branchParams)) ++ " " ++ (show maximumTime) ++ " " ++ (show tcmPrecision) ++ " " ++ distFunction ++ " [(1,1)]" ++ ")\n")
          in
          eigenString ++ outString ++ makeCharacterModelsString (tail charModelList) (counter + 1) areRateModifiers areBothDistributions
        else 
          let letString = ("  let w" ++ (show counter) ++ "=a1 " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ " " ++ (show d) ++ " " ++ (show e) ++ " " ++ (show tcmPrecision) ++ "\n")
              outString = ("  putStr (s$c1 b " ++ (show $ last tcmAlphabet) ++ " z" ++ (show counter) ++ " u" ++ (show counter) ++ " i" ++ (show counter) ++ " " ++ (show (length tcmAlphabet)) ++ " " ++ (show (head branchParams)) ++ " " ++ (show maximumTime) ++ " " ++ (show tcmPrecision) ++ " " ++ distFunction ++ " w" ++ (show counter) ++ ")\n")
          in
          letString ++ eigenString ++ outString ++ makeCharacterModelsString (tail charModelList) (counter + 1) areRateModifiers areBothDistributions
       else if tcmChangeModel == F81 then
        let eigenString = ("  let (z" ++ (show counter) ++ ",u" ++ (show counter) ++ ",i" ++ (show counter) ++ ") = h3 " ++ (show tcmP) ++ "\n")
            distFunction = getDistString branchDist
        in
        if (fst $ head tcmRateModifiers) == None then
          let outString = ("  putStr (s$c1 b " ++ (show $ last tcmAlphabet) ++ " z" ++ (show counter) ++ " u" ++ (show counter) ++ " i" ++ (show counter) ++ " " ++ (show (length tcmAlphabet)) ++ " " ++ (show (head branchParams)) ++ " " ++ (show maximumTime) ++ " " ++ (show tcmPrecision) ++ " " ++ distFunction ++ " [(1,1)]" ++ ")\n")
          in
          eigenString ++ outString ++ makeCharacterModelsString (tail charModelList) (counter + 1) areRateModifiers areBothDistributions
        else 
          let letString = ("  let w" ++ (show counter) ++ "=a1 " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ " " ++ (show d) ++ " " ++ (show e) ++ " " ++ (show tcmPrecision) ++ "\n")
              outString = ("  putStr (s$c1 b " ++ (show $ last tcmAlphabet) ++ " z" ++ (show counter) ++ " u" ++ (show counter) ++ " i" ++ (show counter) ++ " " ++ (show (length tcmAlphabet)) ++ " " ++ (show (head branchParams)) ++ " " ++ (show maximumTime) ++ " " ++ (show tcmPrecision) ++ " " ++ distFunction ++ " w" ++ (show counter) ++ ")\n")
          in
          letString ++ eigenString ++ outString ++ makeCharacterModelsString (tail charModelList) (counter + 1) areRateModifiers areBothDistributions
       else if tcmChangeModel == HKY85 then
        let eigenString = ("  let (z" ++ (show counter) ++ ",u" ++ (show counter) ++ ",i" ++ (show counter) ++ ") = h4 " ++ (show modelParams) ++ " " ++ (show tcmP) ++ "\n")
            distFunction = getDistString branchDist
        in
        if (fst $ head tcmRateModifiers) == None then
          let outString = ("  putStr (s$c1 b " ++ (show $ last tcmAlphabet) ++ " z" ++ (show counter) ++ " u" ++ (show counter) ++ " i" ++ (show counter) ++ " " ++ (show (length tcmAlphabet)) ++ " " ++ (show (head branchParams)) ++ " " ++ (show maximumTime) ++ " " ++ (show tcmPrecision) ++ " " ++ distFunction ++ " [(1,1)]" ++ ")\n")
          in
          eigenString ++ outString ++ makeCharacterModelsString (tail charModelList) (counter + 1) areRateModifiers areBothDistributions
        else 
          let letString = ("  let w" ++ (show counter) ++ "=a1 " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ " " ++ (show d) ++ " " ++ (show e) ++ " " ++ (show tcmPrecision) ++ "\n")
              outString = ("  putStr (s$c1 b " ++ (show $ last tcmAlphabet) ++ " z" ++ (show counter) ++ " u" ++ (show counter) ++ " i" ++ (show counter) ++ " " ++ (show (length tcmAlphabet)) ++ " " ++ (show (head branchParams)) ++ " " ++ (show maximumTime) ++ " " ++ (show tcmPrecision) ++ " " ++ distFunction ++ " w" ++ (show counter) ++ ")\n")
          in
          letString ++ eigenString ++ outString ++ makeCharacterModelsString (tail charModelList) (counter + 1) areRateModifiers areBothDistributions
       else if tcmChangeModel == F84 then
        let eigenString = ("  let (z" ++ (show counter) ++ ",u" ++ (show counter) ++ ",i" ++ (show counter) ++ ") = h5 " ++ (show modelParams) ++ " " ++ (show tcmP) ++ "\n")
            distFunction = getDistString branchDist
        in
        if (fst $ head tcmRateModifiers) == None then
          let outString = ("  putStr (s$c1 b " ++ (show $ last tcmAlphabet) ++ " z" ++ (show counter) ++ " u" ++ (show counter) ++ " i" ++ (show counter) ++ " " ++ (show (length tcmAlphabet)) ++ " " ++ (show (head branchParams)) ++ " " ++ (show maximumTime) ++ " " ++ (show tcmPrecision) ++ " " ++ distFunction ++ " [(1,1)]" ++ ")\n")
          in
          eigenString ++ outString ++ makeCharacterModelsString (tail charModelList) (counter + 1) areRateModifiers areBothDistributions
        else 
          let letString = ("  let w" ++ (show counter) ++ "=a1 " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ " " ++ (show d) ++ " " ++ (show e) ++ " " ++ (show tcmPrecision) ++ "\n")
              outString = ("  putStr (s$c1 b " ++ (show $ last tcmAlphabet) ++ " z" ++ (show counter) ++ " u" ++ (show counter) ++ " i" ++ (show counter) ++ " " ++ (show (length tcmAlphabet)) ++ " " ++ (show (head branchParams)) ++ " " ++ (show maximumTime) ++ " " ++ (show tcmPrecision) ++ " " ++ distFunction ++ " w" ++ (show counter) ++ ")\n")
          in
          letString ++ eigenString ++ outString ++ makeCharacterModelsString (tail charModelList) (counter + 1) areRateModifiers areBothDistributions
       else if tcmChangeModel == TN93 then
        let eigenString = ("  let (z" ++ (show counter) ++ ",u" ++ (show counter) ++ ",i" ++ (show counter) ++ ") = h6 " ++ (show modelParams) ++ " " ++ (show tcmP) ++ "\n")
            distFunction = getDistString branchDist
        in
        if (fst $ head tcmRateModifiers) == None then
          let outString = ("  putStr (s$c1 b " ++ (show $ last tcmAlphabet) ++ " z" ++ (show counter) ++ " u" ++ (show counter) ++ " i" ++ (show counter) ++ " " ++ (show (length tcmAlphabet)) ++ " " ++ (show (head branchParams)) ++ " " ++ (show maximumTime) ++ " " ++ (show tcmPrecision) ++ " " ++ distFunction ++ " [(1,1)]" ++ ")\n")
          in
          eigenString ++ outString ++ makeCharacterModelsString (tail charModelList) (counter + 1) areRateModifiers areBothDistributions
        else 
          let letString = ("  let w" ++ (show counter) ++ "=a1 " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ " " ++ (show d) ++ " " ++ (show e) ++ " " ++ (show tcmPrecision) ++ "\n")
              outString = ("  putStr (s$c1 b " ++ (show $ last tcmAlphabet) ++ " z" ++ (show counter) ++ " u" ++ (show counter) ++ " i" ++ (show counter) ++ " " ++ (show (length tcmAlphabet)) ++ " " ++ (show (head branchParams)) ++ " " ++ (show maximumTime) ++ " " ++ (show tcmPrecision) ++ " " ++ distFunction ++ " w" ++ (show counter) ++ ")\n")
          in
          letString ++ eigenString ++ outString ++ makeCharacterModelsString (tail charModelList) (counter + 1) areRateModifiers areBothDistributions
      -}

      
defaultMachineModel = MachineModel  { machineName = "DefaultMachine"
                                    , graphSpecification = defaultGraph
                                    , characterModelList = [(defaultCharModel, 20),(defaultCharModel, 30)]
                                    }

defaultGraph = GraphModel { graphName = "DefaultGraph"
                          , numLeaves = 20
                          , numRoots = 4
                          , numSingletons = 1
                          , numNetworkEdges = 2
                          }

defaultCharModel = CharacterModel { characterName = "DefaultCharacterModel"
                                  , alphabet = ["0","1"]
                                  , branchLength = (Uniform,[])
                                  , rateModifiers = [(None,[])]
                                  , changeModel = (Neyman, [[]], [], [])
                                  , precision = 100
                                  , charLength = 1
                                  }


-- \f4 :: [Double] -> Int -> Double -> Double\n\
polyEvalString="\
\f4 a b x=\n\
\ if null a then 0\n\
\ else (a5 a)*(p x b 0) +(f4(a6 a)(b+1)x)\n"

-- \f5 :: [Double] -> [Double]\n\
polyPrimeString="\
\f5 a =\n\
\ if d5 0 a==1 then []\n\ 
\ else(f5(f6 a))++[(a3 a)*(fromIntegral ((d5 0 a)-1))]\n"

-- \e0 :: [Double] -> [Double] -> [Double]\n\
polyMinusString="\
\e0 a b=\n\
\ if null a && (not$null b) then g((-1)*)b\n\
\ else if null b && (not$null a) then a\n\
\ else if null a && (null b) then []\n\
\ else (a5 a)-(a5 b):e0(a6 a)(a6 b)\n"

-- \e4 :: [Double] -> [Double] -> [Double]\n\
polyDeflateString="\
\e4 a b=\n\
\ if d5 0 a<2 then []\n\
\ else\n\
\  let c=(a3 a)/(a3 b)\n\
\  in (e4(f6$e0 a$e1 b((r((d5 0 a)-2)0 0)++[c])0)b)++[c]\n"

-- \f1 :: [Double] -> [Double] -> [Double]\n\
polyAddString="\
\f1 a b=\n\
\ if null a && (not$null b) then b\n\
\ else if null b && (not$null a) then a\n\
\ else if null a && (null b) then []\n\
\ else (a5 a)+(a5 b):f1(a6 a)(a6 b)\n"

-- \e1 :: [Double] -> [Double] -> Int -> [Double]\n\
polyMultiplyString="\
\e1 a b c=\n\
\ if null a then []\n\
\ else if c==d5 0 a then []\n\
\ else f1(g((a!!c)*)((r c 0 0)++b))(e1 a b(c+1))\n"

-- \f6 :: [a] -> [a]\n\
initString="\
\f6 a=\n\
\ if null a then []\n\
\ else if d5 0 a==1 then []\n\
\ else (a5 a):f6(a6 a)\n"

-- \g4 ::  [[Double]] -> Int -> Int -> [[Double]]\n\
swapString="\
\g4 x a b=\n\
\ if a>b then g4 x b a\n\
\ else if a==b then x\n\
\ else\n\ 
\  let (p1,p2)=g9 a x\n\
\      (p3,p4)=g9(b-a-1)(a6 p2)\n\
\  in p1++[x!!b]++p3++[x!!a]++(a6 p4)\n"


-- | addLambda att [-1] to each diagnoal list
addLambda :: Int -> PolyMatrix -> PolyMatrix
addLambda row inMatrix =
  if null inMatrix then []
  else
    let thisRow = head inMatrix
        first = take row thisRow
        second = (thisRow !! row) ++ [-1]
        rest = drop (row +1) thisRow
    in
    (first ++ [second] ++ rest) : addLambda (row + 1) (tail inMatrix)

-- | makePolyMatrix take a matrix an converts to poly matrix with -1 added
--- on diagnosls for A - \lambda I
makePolyMatrix :: Matrix -> Int -> PolyMatrix
makePolyMatrix inMatrix row =
  if null inMatrix then []
  else
    let firstPoly = fmap (fmap (:[])) inMatrix
    in
    addLambda 0 firstPoly

-- | getEigenValues takes matrix, concverts to poly matrix with -1 on diagonals,
-- gets characteristic equation, solves characteristic equation and returns
-- a list of the eigenvalues
getEigenValues :: Matrix -> [Double]
getEigenValues inMatrix =
  if null inMatrix then error "Null matrix in getEigenValues"
  else
    let polyMatrix = makePolyMatrix inMatrix 0-- A - \lambda I
        characteristcEq = determinantPoly polyMatrix
    in
    -- erverse and sor not necessary, just for comparison to external library functions
    sortOn Data.Ord.Down (getAllRoots characteristcEq)


-- | getEigenMatrix take matrix and subtracts eigenvalue from diagonals
-- start with 0 for rowCounter
getEigenMatrix :: Matrix -> Int -> Double -> Matrix
getEigenMatrix inMatrix rowCounter eigenValue =
  if null inMatrix then []
  else
    let row = head inMatrix
        firstPart = take rowCounter row
        diagValue = (row !! rowCounter) - eigenValue
        lastPart = (tail $ drop rowCounter row)
    in
    (firstPart ++ [diagValue] ++ lastPart) : getEigenMatrix (tail inMatrix) (rowCounter + 1) eigenValue

-- getInitialGuess searches for sign changes in an interval 
getInitialGuess :: PolynomialList -> Double -> Double -> Double -> Double -> Double
getInitialGuess a value previous upper increment =
  if previous > upper then (-0.5) -- error ("Sign change not found in getInitialGuess " ++ (show a) ++ " " ++ show previous ++ " > " ++ show upper)
  else
    let newValue = polyEval a 0 (previous + increment)
    in
    --trace (show previous ++ " : " ++ (show value) ++ " " ++ show newValue) (
    if abs value < epsilon then value
    else if abs newValue < epsilon then newValue
    else if (value < 0) && (newValue > 0) || (value > 0) && (newValue < 0) then previous + (increment /2)
    else getInitialGuess a newValue (previous + increment) upper increment
    --)

-- | getAllRoots uses Newton-Rapheson root finder and deflation to get 'all' roots
-- to a polynomial with all real coefficients
getAllRoots :: PolynomialList -> [Double]
getAllRoots a
  | null a = error "Null polynomial in getAllRoots"
  | length a == 2 =  -- can't happen with Markov
      let a0 = head a
          a1 = a !! 1
      in
      [(-1) * a0 / a1]
  | otherwise = --not sure initial guess needed ni clase of Eigenvalues of continuous time -1.0 might be fine
      let initialGuess = -0.5 --getInitialGuess a (polyEval a 0 (-10)) (-10) 10 0.5 
          newRoot = newtonRoot a initialGuess 0
          deflatePoly = [-1 * newRoot, 1]
          newPoly = polyDeflate a deflatePoly
      in
      newRoot : getAllRoots newPoly

-- | newtonRoot gets root of polynomial via Newton-Rapheson method
newtonRoot :: PolynomialList -> Double -> Int -> Double
newtonRoot a x0 counter =
  if null a then error "Null polynomial in newtonRoot"
  else
    let y = polyEval a 0 x0
        yPrime = polyEval (polyPrime a) 0 x0
    in
    if abs yPrime < epsilon then x0 -- then error ("yPrime too small in newtonRoot" ++ (show yPrime) ++ " < " ++ show epsilon)
    else
      let x1 = x0 - (y / yPrime)
          foundSolution = abs (x1 - x0) < epsilon
      in
      if foundSolution then x1
      else if counter > maxIterations then
        if abs x1 < epsilon then 0
        else x1
        --else error ("Exceeded maximum iterations in newtonRoot for poly " ++ show a)
      else newtonRoot a x1 (counter + 1)

-- | polyEval takes arguemnt and returns value of function
polyEval :: PolynomialList -> Int -> Double -> Double
polyEval a exponent x=
  if null a then 0
  else
    let first = head a
        value =  first * power x exponent 0 --starts with exponent at zero
    in
    value + polyEval (tail a) (exponent + 1) x

-- | polyPrime differentiates a polynomial
polyPrime :: PolynomialList -> PolynomialList
polyPrime a =
  if length a == 1 then [] -- constant
  else
    let lastA = last a
        exponent = length a - 1
    in
    polyPrime (init a) ++ [lastA * fromIntegral exponent]

-- | polyMinus subtraction function for polynomial list
-- a - b
polyMinus :: PolynomialList -> PolynomialList -> PolynomialList
polyMinus a b
  | null a && not (null b) = fmap ((-1) *) b
  | null b && not (null a) = a
  | null a && null b = []
  | otherwise =
      let a1 = head a
          b1 = head b
          c1 = a1 - b1
      in
      c1 : polyMinus (tail a) (tail b)

-- | polyDeflate divides a polynomial by a root factor b=(x-r) 
-- to reduce the degree of polynomial by 1 in root finding 
-- there should be zero remainder if root correct (< epsilon)
-- reduces degree of polynomial by 1 each iteration
-- a/b
polyDeflate :: PolynomialList -> PolynomialList -> PolynomialList
polyDeflate a b
  | null b = error "Dividing by null polynomial"
  | length a < 2 = []
  | otherwise =
      let lastA = last a
          lastB = last b
          newLast = lastA / lastB
          toSubtract = polyMultiply b (replicate (length a - 2) 0 ++ [newLast]) 0
          reducedPoly = init $ polyMinus a toSubtract
      in
      -- create new polynomial in revers order
      --trace ((show a) ++ " " ++ (show b) ++ " " ++ (show toSubtract) ++ " " ++ (show reducedPoly)) 
      polyDeflate reducedPoly b ++ [newLast]


-- | polyAdd addition function for PolynomialList type
polyAdd :: PolynomialList -> PolynomialList -> PolynomialList
polyAdd a b
  | null a && not (null b) = b
  | null b && not (null a) = a
  | null a && null b = []
  | otherwise =
      let a1 = head a
          b1 = head b
          c1 = a1 + b1
      in
      --trace ("Add a1 " ++ (show a1) ++ " b1 " ++ (show b1) ++ " c1 " ++ (show c1))
      c1 : polyAdd (tail a) (tail b)

-- | polyMultiplymultiply function for PolynomialList 
-- requires increasing arity of list as higher order terms are created
-- to make (length a -1) + (length b -1) polynomial with list length of
-- (length a) + (length b) - 1
-- start at index 0 and go over list a
polyMultiply :: PolynomialList -> PolynomialList -> Int -> PolynomialList
polyMultiply a b index
  | null a = []
  | index == length a = []
  | otherwise =
      let shiftB = replicate index 0 ++ b
          newB = fmap ((a !! index) *) shiftB
      in
      --trace ("shiftB " ++ (show shiftB) ++ " newB " ++ (show newB)) 
      polyAdd newB (polyMultiply a b (index + 1))

-- | eulersConstant  iterations starts at 1
eulersConstant :: Int -> Int -> Double -> Double
eulersConstant iterations counter curValue =
  if counter > iterations then curValue
  else eulersConstant iterations (counter + 1) (curValue + (1/ fromIntegral counter) - logE (1 + (1/ fromIntegral counter)) iterations 0 0)

-- | divideListN splits a list in to N equal lists
divideListN :: Int -> [a] -> [[a]]
divideListN size inList =
  if null inList then []
  else
      take size inList : divideListN size (drop size inList)


-- | getCofactorsPoly returns the cofactors of a matrix for detetminant 
-- calculation for polynomial
getCofactorsPoly :: Int -> [[PolynomialList]] -> PolynomialList
getCofactorsPoly jIndex inMatrix
  | null inMatrix = error "Null matric in getCofactorPoly"
  | jIndex > length (head inMatrix) = []
  | otherwise =
      let firstRow = head inMatrix
          aij = firstRow !! (jIndex - 1)
          cij = power (-1) (1 + jIndex) 0
          mij = removeColumn jIndex (tail inMatrix)
      in
      polyAdd (polyMultiply (polyMultiply [cij] aij 0) (determinantPoly mij) 0) (getCofactorsPoly (jIndex + 1) inMatrix)

-- | determinant function for polynomial
-- assumes it is square (check earlier)
determinantPoly :: [[PolynomialList]] -> PolynomialList
determinantPoly inMatrix
  | length (head inMatrix) < 2 = error ("Matrix of order " ++ show (length $ head inMatrix))
  | length (head inMatrix) == 2 =
      let [a,b] = head inMatrix
          [c,d] = last inMatrix
      in
      polyMinus (polyMultiply a d 0) (polyMultiply b c 0)
  | otherwise = --use cofactors
      getCofactorsPoly 1 inMatrix

-- | epsilon2Zero converts a small value to 0 
epsilon2Zero :: Double -> Double
epsilon2Zero a =
 if abs a < epsilon then 0.0
 else a

-- \b5 :: [[Double]] -> [Double]\n\
getEigenValuesString="\
\b5 a=d2$d1$d0 a\n"

-- \b6 :: [[Double]] -> Int -> Double -> [[Double]]\n\
getEigenMatrixString="\
\b6 a b c=\n\
\  if null a then []\n\
\  else\n\
\   let r=a5 a\n\
\   in((d3 0 b r)++[(r!!b)-c]++(a6$d4 0 b r)):b6(a6 a)(b+1)c\n"

-- \d0 :: [[Double]] -> [[[Double]]]\n\
makePolyMatrixString="\
\d0 a=\n\
\ if null a then []\n\
\ else d9 0$g(g(:[]))a\n"

-- \d1 :: [[[Double]]] -> [Double]\n\
determinantPolyString="\
\d1 m=\n\
\ if (d5 0$a5 m)==2 then\n\
\  let [a,b]=a5 m\n\
\      [c,d]=a3 m\n\
\  in e0(e1 a d 0)(e1 b c 0)\n\
\ else e2 1 m\n"

-- \d2 :: [Double] -> [Double]\n\
getAllRootsString="\
\d2 a=\n\
\ if d5 0 a==2 then [(-1)*(a5 a)/(a!!1)]\n\
\ else (e3 a(-0.5)0):d2(e4 a[-1*(e3 a (-0.5) 0),1])\n"

-- \e2 :: Int -> [[[Double]]] -> [Double]\n\
getCofactorsPolyString="\
\e2 a b=\n\
\ if (a>(d5 0$a5 b)) then []\n\
\ else f1(e1(e1[p(-1)(1+a)0]((a5 b)!!(a-1))0)(d1$e9 a(a6 b))0)(e2(a+1)b)\n"

-- \d9 :: Int -> [[[Double]]] -> [[[Double]]]\n\
addLambdaString="\
\d9 a b=\n\
\ if null b then []\n\
\ else\n\
\  let c=a5 b\n\
\  in ((d3 0 a c)++[(c!!a)++[-1]]++(d4 0(a+1)c)):d9(a+1)(a6 b)\n"

-- \e3 :: [Double] -> Double -> Int -> Double\n\
newtonRootString="\
\e3 a b c=\n\
\ let y=f4 a 0 b\n\
\     p=f4(f5 a)0 b\n\
\ in\n\
\ if c8 p<" ++ epsilonString ++ " then b\n\
\ else\n\
\  let x1=(b-(y/p))\n\
\  in\n\
\  if c8(x1-b)<" ++ epsilonString ++ " then x1\n\
\  else if c>" ++ maxIterationsString ++ " then error \"0\"\n\
\  else e3 a x1(c+1)\n"

-- \b9 :: [[Double]] -> [Double]\n\
solveEigenString="\
\b9=g6.(g(g g8)).g7\n"

-- \infiniteTo1 :: Double -> Double\n\
infiniteTo1String="\
\g8 a=\n\
\  if isInfinite a then 1\n\
\  else a\n"

-- \g7 :: [[Double]] -> [[Double]]\n\
gaussianReduceString="\
\g7 a=g5$c3 g3 a[0..(d5 0 a)-1]\n"