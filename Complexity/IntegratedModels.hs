{- |
Module      :  IntegratedModels 
Description :  Analytically integrated models -functions for Algorithmic (Kolmogorov) complexity 
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
either expressed or implied, of the FreeBSD Project

Maintainer  :  Ward Wheeler <wheeler@amnh.org>
Stability   :  unstable
Portability :  portable (I hope)

Used symbolic integrator https://www.integral-calculator.com/

-}
module Complexity.IntegratedModels where

import Debug.Trace
import Complexity.Types
import Complexity.MathUtilities
import Complexity.MatrixUtilities
import Complexity.Constants


-- | neymanUniformWithK returns the probabilities if change and no change Pii and Pij
-- for Neyman models characters with Uniform branch length distribution and rate classes with 'k' weights
-- after Wheeler Cladistics 30 (2014) 282–290
-- takes alphabetsize and uniform parameter [0.alpha] and iterations for expE function
-- the pair time weights and percent weight fro Invairnat and Gamma adjustments
neymanUniformWithK :: Int -> Double -> Int -> (Double, Double) -> (Double, Double)
neymanUniformWithK r a iterations (kWeight, kFraction) =
    if kWeight < epsilon  then (kFraction, 0.0)
    else
        let r1 = fromIntegral r
            eak = expE (-1 * a * kWeight) iterations 0
            --pii throws some NaN fir invarants
            pii = ((((eak + 1) * (r1 -1)) + (a * kWeight))/(a * kWeight * r1))
            pij = ((eak + (a * kWeight) - 1)/(a * kWeight * r1))
            pii2 = 1 - ((r1 -1) * pij)
            --pij2 = (1 - pii) / (r1 -1)
        in
        --trace ("U:" ++ (show kFraction) ++ " " ++ (show kWeight) ++ " " ++ (show pii2) ++ " " ++ (show pij))
        (kFraction * pii2, kFraction * pij)

-- | neymanExponential returns the probabilities if change and no change Pii and Pij
-- for Neyman models characters with Exponential branch length distribution and rate classes with 'k' weights
-- after Wheeler Cladistics 30 (2014) 282–290
-- takes alphabetsize and exponential parameter alpha
-- the parameter of pair time weights and percent weight fro Invairnat and Gamma adjustments
neymanExponentialWithK :: Int -> Double -> (Double, Double) -> (Double, Double)
neymanExponentialWithK r a (kWeight, kFraction) =
    if kWeight < epsilon  then (kFraction, 0.0)
    else
        let r1 = fromIntegral r
            pii = ((a *r1) + kWeight) / (r1 * (a + kWeight))
            pij = (kWeight / (r1 * (kWeight + a)))
            pii2 = 1 - ((r1 -1) * pij)
        in
        --trace ("E:" ++ (show kFraction) ++ " " ++ (show kWeight) ++ " " ++ (show pii) ++ " " ++ (show pij))
        (kFraction * pii2, kFraction * pij)

-- | neymanGeneralWithK is Neyman for both Expinential and Uniform with wights list
-- if weight list is [(1,1)] then is simple Neyman
neymanGeneralWithK :: Distribution -> Int -> Double -> Int -> (Double, Double) -> (Double, Double)
neymanGeneralWithK distribution r a iterations (kWeight, kFraction)
  | kWeight < epsilon = (kFraction, 0.0)
  | distribution == Uniform =
        let r1 = fromIntegral r
            eak = expE (-1 * a * kWeight) iterations 0
            --pii throws some NaN fir invarants
            pij = (eak + (a * kWeight) - 1)/(a * kWeight * r1)
            pii = 1 - ((r1 -1) * pij)
        in
        (kFraction * pii, kFraction * pij)
  | distribution == Exponential =
        let r1 = fromIntegral r
            pij = kWeight / (r1 * (kWeight + a))
            pii = 1 - ((r1 -1) * pij)
        in
        (kFraction * pii, kFraction * pij)
  | otherwise = error ("Distibution " ++ show distribution ++ " not implemented)")

-- | neymanUniform returns the probabilities if change and no change Pii and Pij
-- for Neyman models characters with Uniform branch length distribution
-- after Wheeler Cladistics 30 (2014) 282–290
-- takes alphabetsize and uniform parameter [0.alpha] and iterations for expE function
neymanUniform :: Int -> Double -> Int -> (Double, Double)
neymanUniform alphabetSize alpha iterations =
    let pii = (alpha - ((fromIntegral alphabetSize - 1) * (expE (-1 * alpha) iterations 0) - 1)) / (fromIntegral alphabetSize * alpha)
        pij = (alpha - 1 + expE (-1 * alpha) iterations 0) / (fromIntegral alphabetSize * alpha)
    in
    (pii, pij)

-- | neymanExponential returns the probabilities if change and no change Pii and Pij
-- for Neyman models characters with Exponential branch length distribution
-- after Wheeler Cladistics 30 (2014) 282–290
-- takes alphabetsize and exponential parameter alpha
neymanExponential :: Int -> Double -> (Double, Double)
neymanExponential alphabetSize alpha =
    let pii = (1 + (alpha * fromIntegral alphabetSize)) / ((alpha + 1) * fromIntegral alphabetSize)
        pij = 1 / ((alpha + 1) * fromIntegral alphabetSize)
    in
    (pii, pij)

-- | TN93 model with exponential branch length distributions following Yang (2005) rates alpha1 for T<>C, alpha2 A<>G, and Beta for transversion
-- matrices reorderd (and eigenvalues) 
-- other 4-states models are simplifications
-- JC69 defualts to Neyman with r=4
-- this is exponential branch length model with 'k' for raet factor (1, 1) if all same etc)
tn93ExponentialWithK :: [Double] -> [Double] -> Double -> Int -> (Double, Double) -> [[Double]]
tn93ExponentialWithK [alpha1, alpha2, beta] [pA, pC, pG, pT] expParam iterations (kWeight, kFraction) =
  if kWeight < epsilon then [[kFraction,0,0,0],[0,kFraction,0,0],[0,0,kFraction,0],[0,0,0,kFraction]]
  else
    let pR = pA + pG
        pY = pC + pT
        f1 = (pY*alpha1 + pR*beta)
        f2 = (pR*alpha2 + pY*beta)
        c0Column = pA*pY/pR
        c1Column = pC*pR/pY
        c2Column = pG*pY/pR
        c3Column = pT*pR/pY
        d0Row = pG/pR
        d1Row = pT/pY
        d2Row = pA/pR
        d3Row = pC/pY
        m2 = expParam * expParam
        km = expParam * kWeight
        k2 = kWeight * kWeight
        p00 = (((d0Row + c0Column + pA) * m2) + ((((c0Column + pA) * f1)+(beta * d0Row)+(pA *beta)) * km) + (pA * beta * f1 * k2)) / ((expParam + (beta*kWeight)) * (expParam + (f1*kWeight)))
        p01 = pC * beta * kWeight / (expParam + (beta*kWeight))
        p02 = -1 * (((d0Row - c2Column - pG) * m2) + (((((-1*c2Column) - pG) * f1)+(beta * d0Row)-(pG *beta)) * km) - (pG * beta * f1 * k2)) / ((expParam + (beta*kWeight)) * (expParam + (f1*kWeight)))
        p03 = pT * beta * kWeight / (expParam + (beta*kWeight))
        p10 = pA * beta * kWeight / (expParam + (beta*kWeight))
        p11 = (((d1Row + c1Column + pC) * m2) + ((((c1Column + pC) * f2)+(beta * d1Row)+(pC *beta)) * km) + (pC * beta * f2 * k2)) / ((expParam + (beta*kWeight)) * (expParam + (f2*kWeight)))
        p12 = pG * beta * kWeight / (expParam + (beta*kWeight))
        p13 = -1 * (((d1Row - c3Column - pT) * m2) + (((((-1*c3Column) - pT) * f2)+(beta * d1Row)-(pT *beta)) * km) - (pT * beta * f2 * k2)) / ((expParam + (beta*kWeight)) * (expParam + (f2*kWeight)))
        p20 = -1 * (((d2Row - c0Column - pA) * m2) + (((((-1*c0Column) - pA) * f1)+(beta * d2Row)-(pA *beta)) * km) - (pA * beta * f1 * k2)) / ((expParam + (beta*kWeight)) * (expParam + (f1*kWeight)))
        p21 = pC * beta * kWeight / (expParam + (beta*kWeight))
        p22 = (((d2Row + c2Column + pG) * m2) + ((((c2Column + pG) * f1)+(beta * d2Row)+(pG *beta)) * km) + (pG * beta * f1 * k2)) / ((expParam + (beta*kWeight)) * (expParam + (f1*kWeight)))
        p23 = pT * beta * kWeight / (expParam + (beta*kWeight))
        p30 = pA * beta * kWeight / (expParam + (beta*kWeight))
        p31 = -1 * (((d3Row - c1Column - pC) * m2) + (((((-1*c1Column) - pC) * f2)+(beta * d3Row)-(pC *beta)) * km) - (pC * beta * f2 * k2)) / ((expParam + (beta*kWeight)) * (expParam + (f2*kWeight)))
        p32 = pG * beta * kWeight / (expParam + (beta*kWeight))
        p33 = (((d3Row + c3Column + pT) * m2) + ((((c3Column + pT) * f2)+(beta * d3Row)+(pT *beta)) * km) + (pT * beta * f2 * k2)) / ((expParam + (beta*kWeight)) * (expParam + (f2*kWeight)))
        probIntegratesMatrix = [[p00,p01,p02,p03],[p10,p11,p12,p13],[p20,p21,p22,p23],[p30,p31,p32,p33]]
    in
    --trace ("Prob Matrix:" ++ ppMatrix probIntegratesMatrix)
    matrixMultiplyScalar kFraction probIntegratesMatrix

-- | TN93 model with exponential branch length distributions following Yang (2005) rates alpha1 for T<>C, alpha2 A<>G, and Beta for transversion
-- matrices reorderd (and eigenvalues) 
-- other 4-states models are simplifications
-- JC69 defualts to Neyman with r=4
-- this is exponential branch length model with 'k' for raet factor (1, 1) if all same etc)
tn93UniformWithK :: [Double] -> [Double] -> Double -> Int -> (Double, Double) -> [[Double]]
tn93UniformWithK [alpha1, alpha2, beta] [pA, pC, pG, pT] expParam iterations (kWeight, kFraction) =
  if kWeight < epsilon then [[kFraction,0,0,0],[0,kFraction,0,0],[0,0,kFraction,0],[0,0,0,kFraction]]
  else
    let pR = pA + pG
        pY = pC + pT
        f1 = (pY*alpha1 + pR*beta)
        f2 = (pR*alpha2 + pY*beta)
        c0Column = pA*pY/pR
        c1Column = pC*pR/pY
        c2Column = pG*pY/pR
        c3Column = pT*pR/pY
        d0Row = pG/pR
        d1Row = pT/pY
        d2Row = pA/pR
        d3Row = pC/pY
        ebkm = expE (beta * kWeight * expParam) iterations 0
        enbkm = expE (-1 * beta * kWeight * expParam) iterations 0
        enf1km = expE (-1 * f1 * kWeight * expParam) iterations 0
        enf2km = expE (-1 * f2 * kWeight * expParam) iterations 0
        bkmfactor = (enbkm + ((beta * kWeight * expParam) - 1)) / (beta * kWeight * expParam)
        p00 = ((enbkm * ((ebkm * ((pA * beta * f1 * kWeight * expParam) + (c0Column * f1) + (beta * d0Row))) - (c0Column * f1))) - (beta * d0Row * enf1km)) / (beta * f1 * kWeight * expParam)
        p01 = pC * bkmfactor
        p02 = ((enbkm * ((ebkm * ((pG * beta * f1 * kWeight * expParam) + (c2Column * f1) - (beta * d0Row))) - (c2Column * f1))) + (beta * d0Row * enf1km)) / (beta * f1 * kWeight * expParam)
        p03 = pT * bkmfactor
        p10 = pA * bkmfactor
        p11 = ((enbkm * ((ebkm * ((pC * beta * f2 * kWeight * expParam) + (c1Column * f2) + (beta * d1Row))) - (c1Column * f2))) - (beta * d1Row * enf2km)) / (beta * f2 * kWeight * expParam)
        p12 = pG * bkmfactor
        p13 = ((enbkm * ((ebkm * ((pT * beta * f2 * kWeight * expParam) + (c3Column * f2) - (beta * d1Row))) - (c3Column * f2))) + (beta * d1Row * enf2km)) / (beta * f2 * kWeight * expParam)
        p20 = ((enbkm * ((ebkm * ((pA * beta * f1 * kWeight * expParam) + (c0Column * f1) - (beta * d2Row))) - (c0Column * f1))) + (beta * d2Row * enf1km)) / (beta * f1 * kWeight * expParam)
        p21 = pC * bkmfactor
        p22 = ((enbkm * ((ebkm * ((pG * beta * f1 * kWeight * expParam) + (c2Column * f1) + (beta * d2Row))) - (c2Column * f1))) - (beta * d2Row * enf1km)) / (beta * f1 * kWeight * expParam)
        p23 = pG * bkmfactor
        p30 = pA * bkmfactor
        p31 = ((enbkm * ((ebkm * ((pC * beta * f2 * kWeight * expParam) + (c1Column * f2) - (beta * d3Row))) - (c1Column * f2))) + (beta * d3Row * enf2km)) / (beta * f2 * kWeight * expParam)
        p32 = pG * bkmfactor
        p33 = ((enbkm * ((ebkm * ((pT * beta * f2 * kWeight * expParam) + (c3Column * f2) + (beta * d3Row))) - (c3Column * f2))) - (beta * d3Row * enf2km)) / (beta * f2 * kWeight * expParam)
        probIntegratesMatrix = [[p00,p01,p02,p03],[p10,p11,p12,p13],[p20,p21,p22,p23],[p30,p31,p32,p33]]
    in
    --trace ("Prob Matrix :\n" ++ ppMatrix probIntegratesMatrix)
    matrixMultiplyScalar kFraction probIntegratesMatrix

-- | F84 model with exponential branch length distributions following Yang (2005) rates alpha1 for T<>C, alpha2 A<>G, and Beta for transversion
-- matrices reorderd (and eigenvalues) 
-- convwerted from TN93 via kappa -> alphas
f84ExponentialWithK :: [Double] -> [Double] -> Double -> Int -> (Double, Double) -> [[Double]]
f84ExponentialWithK [kappa, beta] [pA, pC, pG, pT] expParam iterations (kWeight, kFraction) =
  if kWeight < epsilon then [[kFraction,0,0,0],[0,kFraction,0,0],[0,0,kFraction,0],[0,0,0,kFraction]]
  else
    let pR = pA + pG
        pY = pC + pT
        alpha1 = (1+(kappa/pY))*beta
        alpha2 = (1+(kappa/pR))*beta
        f1 = (pY*alpha1 + pR*beta)
        f2 = (pR*alpha2 + pY*beta)
        c0Column = pA*pY/pR
        c1Column = pC*pR/pY
        c2Column = pG*pY/pR
        c3Column = pT*pR/pY
        d0Row = pG/pR
        d1Row = pT/pY
        d2Row = pA/pR
        d3Row = pC/pY
        m2 = expParam * expParam
        km = expParam * kWeight
        k2 = kWeight * kWeight
        p00 = (((d0Row + c0Column + pA) * m2) + ((((c0Column + pA) * f1)+(beta * d0Row)+(pA *beta)) * km) + (pA * beta * f1 * k2)) / ((expParam + (beta*kWeight)) * (expParam + (f1*kWeight)))
        p01 = pC * beta * kWeight / (expParam + (beta*kWeight))
        p02 = -1 * (((d0Row - c2Column - pG) * m2) + (((((-1*c2Column) - pG) * f1)+(beta * d0Row)-(pG *beta)) * km) - (pG * beta * f1 * k2)) / ((expParam + (beta*kWeight)) * (expParam + (f1*kWeight)))
        p03 = pT * beta * kWeight / (expParam + (beta*kWeight))
        p10 = pA * beta * kWeight / (expParam + (beta*kWeight))
        p11 = (((d1Row + c1Column + pC) * m2) + ((((c1Column + pC) * f2)+(beta * d1Row)+(pC *beta)) * km) + (pC * beta * f2 * k2)) / ((expParam + (beta*kWeight)) * (expParam + (f2*kWeight)))
        p12 = pG * beta * kWeight / (expParam + (beta*kWeight))
        p13 = -1 * (((d1Row - c3Column - pT) * m2) + (((((-1*c3Column) - pT) * f2)+(beta * d1Row)-(pT *beta)) * km) - (pT * beta * f2 * k2)) / ((expParam + (beta*kWeight)) * (expParam + (f2*kWeight)))
        p20 = -1 * (((d2Row - c0Column - pA) * m2) + (((((-1*c0Column) - pA) * f1)+(beta * d2Row)-(pA *beta)) * km) - (pA * beta * f1 * k2)) / ((expParam + (beta*kWeight)) * (expParam + (f1*kWeight)))
        p21 = pC * beta * kWeight / (expParam + (beta*kWeight))
        p22 = (((d2Row + c2Column + pG) * m2) + ((((c2Column + pG) * f1)+(beta * d2Row)+(pG *beta)) * km) + (pG * beta * f1 * k2)) / ((expParam + (beta*kWeight)) * (expParam + (f1*kWeight)))
        p23 = pT * beta * kWeight / (expParam + (beta*kWeight))
        p30 = pA * beta * kWeight / (expParam + (beta*kWeight))
        p31 = -1 * (((d3Row - c1Column - pC) * m2) + (((((-1*c1Column) - pC) * f2)+(beta * d3Row)-(pC *beta)) * km) - (pC * beta * f2 * k2)) / ((expParam + (beta*kWeight)) * (expParam + (f2*kWeight)))
        p32 = pG * beta * kWeight / (expParam + (beta*kWeight))
        p33 = (((d3Row + c3Column + pT) * m2) + ((((c3Column + pT) * f2)+(beta * d3Row)+(pT *beta)) * km) + (pT * beta * f2 * k2)) / ((expParam + (beta*kWeight)) * (expParam + (f2*kWeight)))
        probIntegratesMatrix = [[p00,p01,p02,p03],[p10,p11,p12,p13],[p20,p21,p22,p23],[p30,p31,p32,p33]]
    in
    --trace ("Prob Matrix:" ++ ppMatrix probIntegratesMatrix)
    matrixMultiplyScalar kFraction probIntegratesMatrix

-- | F84 model with exponential branch length distributions following Yang (2005) rates alphpC for T<>C, alphpG A<>G, and Beta for transversion
-- matrices reorderd (and eigenvalues) 
-- other 4-states models are simplifications
-- JC69 defualts to Neyman with r=4
-- this is exponential branch length model with 'k' for raet factor (1, 1) if all same etc)
f84UniformWithK :: [Double] -> [Double] -> Double -> Int -> (Double, Double) -> [[Double]]
f84UniformWithK [kappa, beta] [pA, pC, pG, pT] expParam iterations (kWeight, kFraction) =
  if kWeight < epsilon then [[kFraction,0,0,0],[0,kFraction,0,0],[0,0,kFraction,0],[0,0,0,kFraction]]
  else
    let pR = pA + pG
        pY = pC + pT
        alpha1 = (1+(kappa/pY))*beta
        alpha2 = (1+(kappa/pR))*beta
        f1 = (pY*alpha1 + pR*beta)
        f2 = (pR*alpha2 + pY*beta)
        c0Column = pA*pY/pR
        c1Column = pC*pR/pY
        c2Column = pG*pY/pR
        c3Column = pT*pR/pY
        d0Row = pG/pR
        d1Row = pT/pY
        d2Row = pA/pR
        d3Row = pC/pY
        ebkm = expE (beta * kWeight * expParam) iterations 0
        enbkm = expE (-1 * beta * kWeight * expParam) iterations 0
        enf1km = expE (-1 * f1 * kWeight * expParam) iterations 0
        enf2km = expE (-1 * f2 * kWeight * expParam) iterations 0
        bkmfactor = (enbkm + ((beta * kWeight * expParam) - 1)) / (beta * kWeight * expParam)
        p00 = ((enbkm * ((ebkm * ((pA * beta * f1 * kWeight * expParam) + (c0Column * f1) + (beta * d0Row))) - (c0Column * f1))) - (beta * d0Row * enf1km)) / (beta * f1 * kWeight * expParam)
        p01 = pC * bkmfactor
        p02 = ((enbkm * ((ebkm * ((pG * beta * f1 * kWeight * expParam) + (c2Column * f1) - (beta * d0Row))) - (c2Column * f1))) + (beta * d0Row * enf1km)) / (beta * f1 * kWeight * expParam)
        p03 = pT * bkmfactor
        p10 = pA * bkmfactor
        p11 = ((enbkm * ((ebkm * ((pC * beta * f2 * kWeight * expParam) + (c1Column * f2) + (beta * d1Row))) - (c1Column * f2))) - (beta * d1Row * enf2km)) / (beta * f2 * kWeight * expParam)
        p12 = pG * bkmfactor
        p13 = ((enbkm * ((ebkm * ((pT * beta * f2 * kWeight * expParam) + (c3Column * f2) - (beta * d1Row))) - (c3Column * f2))) + (beta * d1Row * enf2km)) / (beta * f2 * kWeight * expParam)
        p20 = ((enbkm * ((ebkm * ((pA * beta * f1 * kWeight * expParam) + (c0Column * f1) - (beta * d2Row))) - (c0Column * f1))) + (beta * d2Row * enf1km)) / (beta * f1 * kWeight * expParam)
        p21 = pC * bkmfactor
        p22 = ((enbkm * ((ebkm * ((pG * beta * f1 * kWeight * expParam) + (c2Column * f1) + (beta * d2Row))) - (c2Column * f1))) - (beta * d2Row * enf1km)) / (beta * f1 * kWeight * expParam)
        p23 = pG * bkmfactor
        p30 = pA * bkmfactor
        p31 = ((enbkm * ((ebkm * ((pC * beta * f2 * kWeight * expParam) + (c1Column * f2) - (beta * d3Row))) - (c1Column * f2))) + (beta * d3Row * enf2km)) / (beta * f2 * kWeight * expParam)
        p32 = pG * bkmfactor
        p33 = ((enbkm * ((ebkm * ((pT * beta * f2 * kWeight * expParam) + (c3Column * f2) + (beta * d3Row))) - (c3Column * f2))) - (beta * d3Row * enf2km)) / (beta * f2 * kWeight * expParam)
        probIntegratesMatrix = [[p00,p01,p02,p03],[p10,p11,p12,p13],[p20,p21,p22,p23],[p30,p31,p32,p33]]
    in
    --trace ("Prob Matrix :\n" ++ ppMatrix probIntegratesMatrix)
    matrixMultiplyScalar kFraction probIntegratesMatrix

-- | HKY85 model with exponential branch length distributions following Yang (2005) rates alpha transition and Beta for transversion
-- matrices reorderd (and eigenvalues) 
-- convwerted from TN93 via kappa -> alphas
hky85ExponentialWithK :: [Double] -> [Double] -> Double -> Int -> (Double, Double) -> [[Double]]
hky85ExponentialWithK [alpha, beta] [pA, pC, pG, pT] expParam iterations (kWeight, kFraction) =
  if kWeight < epsilon then [[kFraction,0,0,0],[0,kFraction,0,0],[0,0,kFraction,0],[0,0,0,kFraction]]
  else
    let pR = pA + pG
        pY = pC + pT
        f1 = (pY*alpha + pR*beta)
        f2 = (pR*alpha + pY*beta)
        c0Column = pA*pY/pR
        c1Column = pC*pR/pY
        c2Column = pG*pY/pR
        c3Column = pT*pR/pY
        d0Row = pG/pR
        d1Row = pT/pY
        d2Row = pA/pR
        d3Row = pC/pY
        m2 = expParam * expParam
        km = expParam * kWeight
        k2 = kWeight * kWeight
        p00 = (((d0Row + c0Column + pA) * m2) + ((((c0Column + pA) * f1)+(beta * d0Row)+(pA *beta)) * km) + (pA * beta * f1 * k2)) / ((expParam + (beta*kWeight)) * (expParam + (f1*kWeight)))
        p01 = pC * beta * kWeight / (expParam + (beta*kWeight))
        p02 = -1 * (((d0Row - c2Column - pG) * m2) + (((((-1*c2Column) - pG) * f1)+(beta * d0Row)-(pG *beta)) * km) - (pG * beta * f1 * k2)) / ((expParam + (beta*kWeight)) * (expParam + (f1*kWeight)))
        p03 = pT * beta * kWeight / (expParam + (beta*kWeight))
        p10 = pA * beta * kWeight / (expParam + (beta*kWeight))
        p11 = (((d1Row + c1Column + pC) * m2) + ((((c1Column + pC) * f2)+(beta * d1Row)+(pC *beta)) * km) + (pC * beta * f2 * k2)) / ((expParam + (beta*kWeight)) * (expParam + (f2*kWeight)))
        p12 = pG * beta * kWeight / (expParam + (beta*kWeight))
        p13 = -1 * (((d1Row - c3Column - pT) * m2) + (((((-1*c3Column) - pT) * f2)+(beta * d1Row)-(pT *beta)) * km) - (pT * beta * f2 * k2)) / ((expParam + (beta*kWeight)) * (expParam + (f2*kWeight)))
        p20 = -1 * (((d2Row - c0Column - pA) * m2) + (((((-1*c0Column) - pA) * f1)+(beta * d2Row)-(pA *beta)) * km) - (pA * beta * f1 * k2)) / ((expParam + (beta*kWeight)) * (expParam + (f1*kWeight)))
        p21 = pC * beta * kWeight / (expParam + (beta*kWeight))
        p22 = (((d2Row + c2Column + pG) * m2) + ((((c2Column + pG) * f1)+(beta * d2Row)+(pG *beta)) * km) + (pG * beta * f1 * k2)) / ((expParam + (beta*kWeight)) * (expParam + (f1*kWeight)))
        p23 = pT * beta * kWeight / (expParam + (beta*kWeight))
        p30 = pA * beta * kWeight / (expParam + (beta*kWeight))
        p31 = -1 * (((d3Row - c1Column - pC) * m2) + (((((-1*c1Column) - pC) * f2)+(beta * d3Row)-(pC *beta)) * km) - (pC * beta * f2 * k2)) / ((expParam + (beta*kWeight)) * (expParam + (f2*kWeight)))
        p32 = pG * beta * kWeight / (expParam + (beta*kWeight))
        p33 = (((d3Row + c3Column + pT) * m2) + ((((c3Column + pT) * f2)+(beta * d3Row)+(pT *beta)) * km) + (pT * beta * f2 * k2)) / ((expParam + (beta*kWeight)) * (expParam + (f2*kWeight)))
        probIntegratesMatrix = [[p00,p01,p02,p03],[p10,p11,p12,p13],[p20,p21,p22,p23],[p30,p31,p32,p33]]
    in
    --trace ("Prob Matrix:" ++ ppMatrix probIntegratesMatrix)
    matrixMultiplyScalar kFraction probIntegratesMatrix

-- | HKY85 model with exponential branch length distributions following Yang (2005) rates alpha transition and Beta for transversion
-- matrices reorderd (and eigenvalues) 
-- other 4-states models are simplifications
-- JC69 defualts to Neyman with r=4
-- this is exponential branch length model with 'k' for raet factor (1, 1) if all same etc)
hky85UniformWithK :: [Double] -> [Double] -> Double -> Int -> (Double, Double) -> [[Double]]
hky85UniformWithK [alpha, beta] [pA, pC, pG, pT] expParam iterations (kWeight, kFraction) =
  if kWeight < epsilon then [[kFraction,0,0,0],[0,kFraction,0,0],[0,0,kFraction,0],[0,0,0,kFraction]]
  else
    let pR = pA + pG
        pY = pC + pT
        f1 = (pY*alpha + pR*beta)
        f2 = (pR*alpha + pY*beta)
        c0Column = pA*pY/pR
        c1Column = pC*pR/pY
        c2Column = pG*pY/pR
        c3Column = pT*pR/pY
        d0Row = pG/pR
        d1Row = pT/pY
        d2Row = pA/pR
        d3Row = pC/pY
        ebkm = expE (beta * kWeight * expParam) iterations 0
        enbkm = expE (-1 * beta * kWeight * expParam) iterations 0
        enf1km = expE (-1 * f1 * kWeight * expParam) iterations 0
        enf2km = expE (-1 * f2 * kWeight * expParam) iterations 0
        bkmfactor = (enbkm + ((beta * kWeight * expParam) - 1)) / (beta * kWeight * expParam)
        p00 = ((enbkm * ((ebkm * ((pA * beta * f1 * kWeight * expParam) + (c0Column * f1) + (beta * d0Row))) - (c0Column * f1))) - (beta * d0Row * enf1km)) / (beta * f1 * kWeight * expParam)
        p01 = pC * bkmfactor
        p02 = ((enbkm * ((ebkm * ((pG * beta * f1 * kWeight * expParam) + (c2Column * f1) - (beta * d0Row))) - (c2Column * f1))) + (beta * d0Row * enf1km)) / (beta * f1 * kWeight * expParam)
        p03 = pT * bkmfactor
        p10 = pA * bkmfactor
        p11 = ((enbkm * ((ebkm * ((pC * beta * f2 * kWeight * expParam) + (c1Column * f2) + (beta * d1Row))) - (c1Column * f2))) - (beta * d1Row * enf2km)) / (beta * f2 * kWeight * expParam)
        p12 = pG * bkmfactor
        p13 = ((enbkm * ((ebkm * ((pT * beta * f2 * kWeight * expParam) + (c3Column * f2) - (beta * d1Row))) - (c3Column * f2))) + (beta * d1Row * enf2km)) / (beta * f2 * kWeight * expParam)
        p20 = ((enbkm * ((ebkm * ((pA * beta * f1 * kWeight * expParam) + (c0Column * f1) - (beta * d2Row))) - (c0Column * f1))) + (beta * d2Row * enf1km)) / (beta * f1 * kWeight * expParam)
        p21 = pC * bkmfactor
        p22 = ((enbkm * ((ebkm * ((pG * beta * f1 * kWeight * expParam) + (c2Column * f1) + (beta * d2Row))) - (c2Column * f1))) - (beta * d2Row * enf1km)) / (beta * f1 * kWeight * expParam)
        p23 = pG * bkmfactor
        p30 = pA * bkmfactor
        p31 = ((enbkm * ((ebkm * ((pC * beta * f2 * kWeight * expParam) + (c1Column * f2) - (beta * d3Row))) - (c1Column * f2))) + (beta * d3Row * enf2km)) / (beta * f2 * kWeight * expParam)
        p32 = pG * bkmfactor
        p33 = ((enbkm * ((ebkm * ((pT * beta * f2 * kWeight * expParam) + (c3Column * f2) + (beta * d3Row))) - (c3Column * f2))) - (beta * d3Row * enf2km)) / (beta * f2 * kWeight * expParam)
        probIntegratesMatrix = [[p00,p01,p02,p03],[p10,p11,p12,p13],[p20,p21,p22,p23],[p30,p31,p32,p33]]
    in
    --trace ("Prob Matrix :\n" ++ ppMatrix probIntegratesMatrix)
    matrixMultiplyScalar kFraction probIntegratesMatrix

-- | f81 model with exponential branch length distributions following Yang (2005) rates alpha transition and Beta for transversion
-- matrices reorderd (and eigenvalues) 
-- convwerted from TN93 via kappa -> alphas
f81ExponentialWithK :: [Double] -> [Double] -> Double -> Int -> (Double, Double) -> [[Double]]
f81ExponentialWithK blah [pA, pC, pG, pT] expParam iterations (kWeight, kFraction) =
  if kWeight < epsilon then [[kFraction,0,0,0],[0,kFraction,0,0],[0,0,kFraction,0],[0,0,0,kFraction]]
  else
    let pR = pA + pG
        pY = pC + pT
        c0Column = pA*pY/pR
        c1Column = pC*pR/pY
        c2Column = pG*pY/pR
        c3Column = pT*pR/pY
        d0Row = pG/pR
        d1Row = pT/pY
        d2Row = pA/pR
        d3Row = pC/pY
        m2 = expParam * expParam
        km = expParam * kWeight
        k2 = kWeight * kWeight
        p00 = (((d0Row + c0Column + pA) * m2) + (((c0Column + pA)+d0Row+pA) * km) + (pA * k2)) / ((expParam + kWeight) * (expParam + kWeight))
        p01 = pC * kWeight / (expParam + kWeight)
        p02 = -1 * (((d0Row - c2Column - pG) * m2) + ((((-1*c2Column) - pG)+d0Row-pG) * km) - (pG * k2)) / ((expParam + kWeight) * (expParam + kWeight))
        p03 = pT * kWeight / (expParam + kWeight)
        p10 = pA * kWeight / (expParam + kWeight)
        p11 = (((d1Row + c1Column + pC) * m2) + (((c1Column + pC)+d1Row+pC) * km) + (pC * k2)) / ((expParam + kWeight) * (expParam + kWeight))
        p12 = pG * kWeight / (expParam + kWeight)
        p13 = -1 * (((d1Row - c3Column - pT) * m2) + ((((-1*c3Column) - pT)+d1Row-pT) * km) - (pT * k2)) / ((expParam + kWeight) * (expParam + kWeight))
        p20 = -1 * (((d2Row - c0Column - pA) * m2) + ((((-1*c0Column) - pA)+d2Row-pA) * km) - (pA * k2)) / ((expParam + kWeight) * (expParam + kWeight))
        p21 = pC * kWeight / (expParam + kWeight)
        p22 = (((d2Row + c2Column + pG) * m2) + (((c2Column + pG)+d2Row+pG) * km) + (pG * k2)) / ((expParam + kWeight) * (expParam + kWeight))
        p23 = pT * kWeight / (expParam + kWeight)
        p30 = pA * kWeight / (expParam + kWeight)
        p31 = -1 * (((d3Row - c1Column - pC) * m2) + ((((-1*c1Column) - pC)+d3Row-pC) * km) - (pC * k2)) / ((expParam + kWeight) * (expParam + kWeight))
        p32 = pG * kWeight / (expParam + kWeight)
        p33 = (((d3Row + c3Column + pT) * m2) + (((c3Column + pT)+d3Row+pT) * km) + (pT * k2)) / ((expParam + kWeight) * (expParam + kWeight))
        probIntegratesMatrix = [[p00,p01,p02,p03],[p10,p11,p12,p13],[p20,p21,p22,p23],[p30,p31,p32,p33]]
    in
    --trace ("Prob Matrix:" ++ ppMatrix probIntegratesMatrix)
    matrixMultiplyScalar kFraction probIntegratesMatrix

-- | HKY85 model with exponential branch length distributions following Yang (2005) rates alpha transition and Beta for transversion
-- matrices reorderd (and eigenvalues) 
-- other 4-states models are simplifications
-- JC69 defualts to Neyman with r=4
-- this is exponential branch length model with 'k' for raet factor (1, 1) if all same etc)
f81UniformWithK :: [Double] -> [Double] -> Double -> Int -> (Double, Double) -> [[Double]]
f81UniformWithK blah [pA, pC, pG, pT] expParam iterations (kWeight, kFraction) =
  if kWeight < epsilon then [[kFraction,0,0,0],[0,kFraction,0,0],[0,0,kFraction,0],[0,0,0,kFraction]]
  else
    let pR = pA + pG
        pY = pC + pT
        c0Column = pA*pY/pR
        c1Column = pC*pR/pY
        c2Column = pG*pY/pR
        c3Column = pT*pR/pY
        d0Row = pG/pR
        d1Row = pT/pY
        d2Row = pA/pR
        d3Row = pC/pY
        ebkm = expE (kWeight * expParam) iterations 0
        enbkm = expE (-1 * kWeight * expParam) iterations 0
        enf1km = expE (-1 * kWeight * expParam) iterations 0
        enf2km = expE (-1 * kWeight * expParam) iterations 0
        bkmfactor = (enbkm + ((kWeight * expParam) - 1)) / (kWeight * expParam)
        p00 = ((enbkm * ((ebkm * ((pA * kWeight * expParam) + c0Column + d0Row)) - c0Column)) - (d0Row * enf1km)) / (kWeight * expParam)
        p01 = pC * bkmfactor
        p02 = ((enbkm * ((ebkm * ((pG * kWeight * expParam) + c2Column - d0Row)) - c2Column)) + (d0Row * enf1km)) / (kWeight * expParam)
        p03 = pT * bkmfactor
        p10 = pA * bkmfactor
        p11 = ((enbkm * ((ebkm * ((pC * kWeight * expParam) + c1Column + d1Row)) - c1Column)) - (d1Row * enf2km)) / (kWeight * expParam)
        p12 = pG * bkmfactor
        p13 = ((enbkm * ((ebkm * ((pT * kWeight * expParam) + c3Column - d1Row)) - c3Column)) + (d1Row * enf2km)) / (kWeight * expParam)
        p20 = ((enbkm * ((ebkm * ((pA * kWeight * expParam) + c0Column - d2Row)) - c0Column)) + (d2Row * enf1km)) / (kWeight * expParam)
        p21 = pC * bkmfactor
        p22 = ((enbkm * ((ebkm * ((pG * kWeight * expParam) + c2Column + d2Row)) - c2Column)) - (d2Row * enf1km)) / (kWeight * expParam)
        p23 = pG * bkmfactor
        p30 = pA * bkmfactor
        p31 = ((enbkm * ((ebkm * ((pC * kWeight * expParam) + c1Column - d3Row)) - c1Column)) + (d3Row * enf2km)) / (kWeight * expParam)
        p32 = pG * bkmfactor
        p33 = ((enbkm * ((ebkm * ((pT * kWeight * expParam) + c3Column + d3Row)) - c3Column)) - (d3Row * enf2km)) / (kWeight * expParam)
        probIntegratesMatrix = [[p00,p01,p02,p03],[p10,p11,p12,p13],[p20,p21,p22,p23],[p30,p31,p32,p33]]
    in
    --trace ("Prob Matrix :\n" ++ ppMatrix probIntegratesMatrix)
    matrixMultiplyScalar kFraction probIntegratesMatrix

-- | K80 model with exponential branch length distributions following Yang (2005) rates alpha transition and Beta for transversion
-- matrices reorderd (and eigenvalues) 
-- convwerted from TN93 via kappa -> alphas
k80ExponentialWithK :: [Double] -> [Double] -> Double -> Int -> (Double, Double) -> [[Double]]
k80ExponentialWithK [alpha, beta] blah expParam iterations (kWeight, kFraction) =
  if kWeight < epsilon then [[kFraction,0,0,0],[0,kFraction,0,0],[0,0,kFraction,0],[0,0,0,kFraction]]
  else
    let f = (alpha +beta)/2
        cColumn = 0.25
        dRow = 0.5
        m2 = expParam * expParam
        km = expParam * kWeight
        k2 = kWeight * kWeight
        offDiagFactor = 0.25 * beta * kWeight / (expParam + (beta*kWeight))
        plusFactor = (((dRow + cColumn + 0.25) * m2) + ((((cColumn + 0.25) * f)+(beta * dRow)+(0.25 *beta)) * km) + (0.25 * beta * f * k2)) / ((expParam + (beta*kWeight)) * (expParam + (f*kWeight)))
        minusFactor = -1 * (((dRow - cColumn - 0.25) * m2) + (((((-1*cColumn) - 0.25) * f)+(beta * dRow)-(0.25 *beta)) * km) - (0.25 * beta * f * k2)) / ((expParam + (beta*kWeight)) * (expParam + (f*kWeight)))
        probIntegratesMatrix = [[plusFactor,offDiagFactor,minusFactor,offDiagFactor],
                                [offDiagFactor,plusFactor,offDiagFactor,minusFactor],
                                [minusFactor,offDiagFactor,plusFactor,offDiagFactor],
                                [offDiagFactor,minusFactor,offDiagFactor,plusFactor]]
    in
    --trace ("Prob Matrix:" ++ ppMatrix probIntegratesMatrix)
    matrixMultiplyScalar kFraction probIntegratesMatrix

-- | K80 model with uniform branch length distributions following Yang (2005) rates alpha transition and Beta for transversion
-- matrices reorderd (and eigenvalues) 
-- this is uniform branch length model with 'k' for raet factor (1, 1) if all same etc)
k80UniformWithK :: [Double] -> [Double] -> Double -> Int -> (Double, Double) -> [[Double]]
k80UniformWithK [alpha, beta] blah expParam iterations (kWeight, kFraction) =
  if kWeight < epsilon then [[kFraction,0,0,0],[0,kFraction,0,0],[0,0,kFraction,0],[0,0,0,kFraction]]
  else
    let f = (alpha +beta)/2
        cColumn = 0.25
        dRow = 0.5
        ebkm = expE (beta * kWeight * expParam) iterations 0
        enbkm = expE (-1 * beta * kWeight * expParam) iterations 0
        enfkm = expE (-1 * f * kWeight * expParam) iterations 0
        offDiagFactor = 0.25 * (enbkm + ((beta * kWeight * expParam) - 1)) / (beta * kWeight * expParam)
        plusFactor = ((enbkm * ((ebkm * ((0.25 * beta * f * kWeight * expParam) + (cColumn * f) + (beta * dRow))) - (cColumn * f))) - (beta * dRow * enfkm)) / (beta * f * kWeight * expParam)
        minusFactor = ((enbkm * ((ebkm * ((0.25 * beta * f * kWeight * expParam) + (cColumn * f) - (beta * dRow))) - (cColumn * f))) + (beta * dRow * enfkm)) / (beta * f * kWeight * expParam)
        probIntegratesMatrix = [[plusFactor,offDiagFactor,minusFactor,offDiagFactor],
                                [offDiagFactor,plusFactor,offDiagFactor,minusFactor],
                                [minusFactor,offDiagFactor,plusFactor,offDiagFactor],
                                [offDiagFactor,minusFactor,offDiagFactor,plusFactor]]
    in
    --trace ("Prob Matrix :\n" ++ ppMatrix probIntegratesMatrix)
    matrixMultiplyScalar kFraction probIntegratesMatrix
