{- |
Module      :  MathUtilities 
Description :  mathematical utuilities for calculating Kolmnogorov Complexity
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



module Complexity.MathUtilities where

-- import Complexity.Parsers
import Complexity.Types
-- import Complexity.Constants
-- import Debug.Trace

-- | factorial is naive recursive calculation of factorials
-- Double is more stable numerically
factorial :: (Eq a, Num a) => a -> a
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)


-- | baseE is naive calcultions of 'e' base of natural logorithms
-- take numbr of iterations, counter set to 0, curValue set to 0.0 
baseE :: Int -> Int -> Double
baseE iterations counter =
  if counter == iterations then 0
  else
    --trace (show (factorial counter) ++ " " ++ show curValue ++ " ")
    (1.0 / fromIntegral (factorial counter)) + baseE iterations (counter + 1) --(fromIntegral $ factorial counter)))


-- | expE naive claculation of 'e^x' exponent of natural logorithm
-- can be used to calcilaue value of 'e' with 1 as exponent
-- counter set to 0, curValue set to 0.0
-- (1+ (x/n))^n
-- this is a problem--causes issues
-- expE :: Double -> Int -> Int -> Double
-- expE exponentLocal iterations counter =
expE :: Double -> Double
expE exponentLocal =
  exp exponentLocal
  {-
  if counter == fixedPrecision then 0 
  else  
    ((power exponent counter 0) / (fromIntegral (factorial counter))) + expE exponent iterations (counter + 1)
  
  --trace (show $ power (1+(1/(fromIntegral 1000))) 1000 0)
  power (1+(exponent/(fromIntegral fixedPrecision))) fixedPrecision 0
 -} 

-- | expX2Y exponentiation general x^y via e^(y * log x)
expX2Y :: Double -> Double -> Double
expX2Y base exponentLocal =
  base ** exponentLocal
  {-
  expE (exponent * (logE base iterations 0 0)) iterations 0
  -}


-- | power take a base and positive integer exponent and returns value
-- this for use in expE calculation  initialized with counter value of 0
power :: Double -> Int -> Int -> Double
power base exponentLocal counter
  | exponentLocal == 0 = 1.0
  | exponentLocal == counter = 1
  | otherwise =
      base * power base exponentLocal (counter + 1)


{-Unused args hit trigger warning but are needed so functions can be passed alternately-}
-- | log2 is logarithm base 2 defined in terms of Taylor series for log base e 
-- 2 unused args do same as LogE for functional argument use
log2 :: Double -> Int-> Int -> Double -> Double
log2 value iterations blah bleh = logE value iterations 0 0.0 / logE 2.0 iterations 0 0.0

-- | Taylor series for natural logorithm, value >= 0 
--- counter set to 0, curValue set to 0.0
logE :: Double -> Int -> Int -> Double -> Double
logE value iterations counter curValue = log value
  {-
  if value < 0 then error "logE arg must be > 0"
  else if counter == iterations then 2.0 * curValue
  else  
    let factor = 1 + (2 * counter)
        newValue = (power ((value - 1.0) / (value + 1.0)) factor 0) / (fromIntegral factor)
    in
    logE value iterations (counter + 1) (curValue + newValue)
    -}

-- | gammma function of input--Guess method
-- from https://www.csie.ntu.edu.tw/~b89089/link/gammaFunction.pdf
-- Gamma a = lim (n-> infty) (n!n^a)/(a(a+1)...(a+n))_
-- counter starts at 0, curValue 1
-- goes 0 to N (not n-1) 
gammaFun :: Double -> Int -> Int -> Double -> Double
gammaFun value iterations counter curValue =
  if counter > iterations then factorial (fromIntegral iterations) * expX2Y (fromIntegral iterations) value / curValue
  else gammaFun value iterations (counter + 1) (curValue * (value + fromIntegral counter))

-- | gammaPDF returns probbaility of given argument based on inputs and parameters assumes alpha=beta
-- hence only 1 gamma parameter 
gammaPDF :: Double -> Int -> Double -> Double -> Double
gammaPDF alpha iterations interval rVal  =
  let result = interval * expX2Y alpha alpha  * expX2Y rVal (alpha - 1)  * expE (-1 * alpha * rVal) / gammaFun alpha iterations 0 1
  in
  --trace ("alpha " ++ (show alpha) ++ " iterations " ++ (show iterations) ++ " interval " ++ (show interval) ++ " rVal " ++ (show rVal) ++ " => " ++ (show result)) 
  result

-- | cumulativeSum sums using gamma PDF
cumulativeSum :: [Double] -> Double -> [Double]
cumulativeSum probList previousValue =
  if null probList then []
  else (previousValue + head probList) : cumulativeSum (tail probList) (previousValue +  head probList)

-- | getNtiles takes cumulative probbalilities and weights (intervals)  and checks for critical value
-- puts odd criticals in list ie 2x criticals--chooses median for each n-tile
getNtiles :: Int -> [Double] -> [Double] -> Int -> Bool-> [Double]
getNtiles classes cdfList weightList nTileCounter isOdd =
  if null cdfList then []
  else
      let firstCDF = head cdfList
          firstWeight = head weightList
          criticalValue = fromIntegral nTileCounter / (2 * fromIntegral classes)
      in
      --trace (" CV (" ++ (show criticalValue) ++ "," ++ (show firstCDF) ++ "," ++ (show firstWeight) ++ "," ++ (show nTileCounter) ++ "," ++ (show isOdd) ++ ")") (
      if firstCDF >= criticalValue then
        if isOdd then firstWeight : getNtiles classes (tail cdfList) (tail weightList) (nTileCounter + 1) False
        else getNtiles classes (tail cdfList) (tail weightList) (nTileCounter + 1) True
      else getNtiles classes (tail cdfList) (tail weightList) nTileCounter isOdd
      --)

-- | discreteGamma teh Yang 1994 modifier for rates
-- implemented as a factor for each "t" in rate class then average
-- sets alpha = beta so expentation is 1
-- return is the median gamma rate factor for each 1/numClasses
-- component of distribution, maxRate set on input  10 prob OK (that would be near randomization rate).
-- do "iterations" for each class
discreteGamma :: DistributionParameter -> Int -> Double -> Int -> Int -> [Double]
discreteGamma alpha numClasses maxRate iterations rectangles
  | numClasses == 0 = error "Cannot have 0 Gamma classes"
  | numClasses == 1 = [1]
  | otherwise =
      let numSteps = numClasses * rectangles
          interval = maxRate / fromIntegral numSteps
          intervalList = replicate numSteps interval
          rValList =  cumulativeSum intervalList 0 --really just accumulates
          rProbList = fmap (gammaPDF alpha iterations interval) rValList
          cdfList = cumulativeSum rProbList 0
          --adjust for prob mass missed in 0th cell when alha is small
          adjustment = 1 - last cdfList
          adjustedCDFList = fmap (+ adjustment) cdfList
          --this below is wrong--need to sum till prob = 1/N 2/N etc
          --then take averages of thos sets--not equal by iterations
          rMeanList = getNtiles numClasses adjustedCDFList rValList 1 True
          --Normalizes values so that the rate expectation is 1.
          rNormalized = fmap (* (fromIntegral numClasses / sum rMeanList)) rMeanList
      in
      --trace ("\nLast = " ++ (show $ last adjustedCDFList) ++ "\nSum = " ++ (show $ sum rProbList))
      rNormalized


{-Extra arguments are to maintain same types for these two functions so can be used as alternate paramters to function later -}
-- | getUniformPdf gets pdf on interval [0,maxValue]
-- the time and iteration arguemens are ignored-- there to nake like exponential
getUniformPdf :: Double -> Int -> Double -> Double
getUniformPdf  maxValue iterations time = 1 / maxValue

-- | getExponentialPdf on [0,Infty)
getExponentialPdf ::  Double -> Int -> Double -> Double
getExponentialPdf  lambda iterations time =
  -- lambda * exp (-1 * lambda * time)
  lambda * expE (-1 * lambda * time) -- iterations 0


