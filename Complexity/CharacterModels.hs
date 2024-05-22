{- |
Module      :  CharacterModels for Algorithmic (Kolmogorov) complexity
Description :  Functions to generate(algorithmic) complexity of character change models
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
{-
ToDo:  other models
    1) Code to "generate" models by passing parameters
        e.g. JC+Indels  pass two parameters general matrix
            up to GTR where all are specified and code just reads them
            allows for extra complexity savings for the simpler models
        siple models should have analytica solutions so can specify like Neyman
        but with some additional machine complexity to read params and create Pii/ij
        then can use same invarant/gamma codes for all
   2) Huffman bits for models?
   3) separate out invariants alone--or convert to gamma 2 classes?
-}

module Complexity.CharacterModels
  (  makeTCM -- general for TCM construcviton log2 or logE
  ,  makeProgramStringCharacters --program to outpput character models (all of them)
  ,  matrix2String
  ,  getAICBIC
  )  where

import           Complexity.MathUtilities
import           Control.Applicative
import           Data.List
-- import Complexity.Parsers
import           Complexity.CodeStrings
import           Complexity.Constants
import           Complexity.GTRExt
import           Complexity.IntegratedModels
import           Complexity.MatrixUtilities
import           Complexity.Types
import           Complexity.Utilities
import           Control.Concurrent
import           Control.DeepSeq
import           Control.Parallel.Strategies
import Debug.Trace



-- |list of functions required by Neyman models, special cases to reduce code size for special cases
-- can union lists to write the ones (and only ones) needed
neymanUniformDependencies :: [String]
neymanUniformDependencies = [headString,tailString,concatString, makeTCMBitsString, neymanUniformString, factorialString, expEString, powerString, log2String, logEString, replicateString, makeMatrixString, matrix2StringString, fmapString]

neymanExponentialDependencies :: [String]
neymanExponentialDependencies = [headString,tailString,concatString, makeTCMBitsString, neymanExponentialString, powerString, log2String, logEString, replicateString, makeMatrixString, matrix2StringString, fmapString]

neymanUniformWithKDependencies :: [String]
neymanUniformWithKDependencies = [headString,tailString,concatString,lastString, getModifierListSmallString, zipString, fstString, sndString, factorialString, expEString, powerString, log2String, logEString, replicateString, makeMatrixString, matrix2StringString, fmapString] ++ [makeNeymanUniformMatrixString, neymanUniformWithKString] ++ modifierDependencies ++ gammaDependencies

neymanExponentialWithKDependencies :: [String]
neymanExponentialWithKDependencies = [headString,tailString,concatString, lastString, getModifierListSmallString, zipString, fstString, sndString, powerString, log2String, logEString, replicateString, makeMatrixString, matrix2StringString, fmapString] ++ [makeNeymanExponentialMatrixString, neymanExponentialWithKString, foldlString] ++ modifierDependencies ++ gammaDependencies

neymanGeneralWithKDependencies :: [String]
neymanGeneralWithKDependencies = [headString,tailString,concatString,lastString, getModifierListSmallString, zipString, fstString, sndString, factorialString, expEString, powerString, log2String, logEString, replicateString, makeMatrixString, matrix2StringString, fmapString] ++ [makeNeymanGeneralMatrixString, neymanGeneralWithKString, foldlString] ++ modifierDependencies ++ gammaDependencies

-- | these are for local GTR functions
-- assuming the functions will recieve a good Q matrix
gtrDependencies :: [String]
gtrDependencies = [makeGTRMatrixLocalString, makeGTRLogMatrixString, split2MatrixString, regularizeRString, makeQString, addDiagValuesString, invertMatrixString, adjustSymString, adjustDiagString, getLogMatrixString, addMatricesString, integrateGTRMatrixWithKString, determinantNumericalString, cofactorTMatrixString, takeWhileString, getCofactor1String, getRowString, removeColumnString, removeRowAndColumnString, isPosRString, lastString, tailString, logEString, matrix2StringString, log2String] ++ qrDependencies ++ numericalIntegrationDependencies

fourStateSequenceModelDependencies :: [String]
fourStateSequenceModelDependencies = [fmapString, headString, tailString, split2MatrixString, adjustSymString, foldlString, addMatricesString, replicateString, getLogMatrixString, adjustDiagString, takeString, dropString, lengthString, zipWithString, matrix2StringString, log2String, powerString, concatString, logEString]

f81ExponentialDependencies :: [String]
f81ExponentialDependencies = [f81ExponentialWithKString, matrixMultiplyScalarString, makeGTRLogMatrix4StateString] ++ fourStateSequenceModelDependencies

k80ExponentialDependencies :: [String]
k80ExponentialDependencies = [k80ExponentialWithKString, matrixMultiplyScalarString, makeGTRLogMatrix4StateString] ++ fourStateSequenceModelDependencies

hky85ExponentialDependencies :: [String]
hky85ExponentialDependencies = [hky85ExponentialWithKString, matrixMultiplyScalarString, makeGTRLogMatrix4StateString] ++ fourStateSequenceModelDependencies

f84ExponentialDependencies :: [String]
f84ExponentialDependencies = [f84ExponentialWithKString, matrixMultiplyScalarString, makeGTRLogMatrix4StateString] ++ fourStateSequenceModelDependencies

tn93ExponentialDependencies :: [String]
tn93ExponentialDependencies = [tn93ExponentialWithKString, matrixMultiplyScalarString, makeGTRLogMatrix4StateString] ++ fourStateSequenceModelDependencies

f81UniformDependencies :: [String]
f81UniformDependencies = [f81UniformWithKString, matrixMultiplyScalarString, makeGTRLogMatrix4StateString] ++ [expEString, factorialString] ++ fourStateSequenceModelDependencies

k80UniformDependencies :: [String]
k80UniformDependencies = [k80UniformWithKString, matrixMultiplyScalarString, makeGTRLogMatrix4StateString] ++ [expEString, factorialString] ++ fourStateSequenceModelDependencies

hky85UniformDependencies :: [String]
hky85UniformDependencies = [hky85UniformWithKString, matrixMultiplyScalarString, makeGTRLogMatrix4StateString] ++ [expEString, factorialString] ++ fourStateSequenceModelDependencies

f84UniformDependencies :: [String]
f84UniformDependencies = [f84UniformWithKString, matrixMultiplyScalarString, makeGTRLogMatrix4StateString] ++ [expEString, factorialString] ++ fourStateSequenceModelDependencies

tn93UniformDependencies :: [String]
tn93UniformDependencies = [tn93UniformWithKString, matrixMultiplyScalarString, makeGTRLogMatrix4StateString] ++ [expEString, factorialString] ++ fourStateSequenceModelDependencies

-- | QR factorization dependencies
qrDependencies :: [String]
qrDependencies = [qrFactorizationString,  getDiagValuesString,  qrDecompositionString,  matrixMultiplyString,  concatString,  fmapString,  lengthString,  absString,  foldlString,  getHouseholderListString, transposeMatrixString,  getHouseholderString,  padOutMinorString,  makeMatrixMinorString,  makeDiagMatrixString,  takeString,  dropString,  zipWithString,  getColumnVectorString,  makeEVectorString,  euclidNormString,  matrixMultiplyScalarString,  subtractMatricesString,  normalizeColumnVectorString,  headString,  replicateString,  makeDiagRowString,  normalizeVectorString,  sqrtString,  getRowsString,  getElementString, getDiagValuesString, factorialString, expEString, powerString]

-- | dependencies for numerical integration
numericalIntegrationDependencies :: [String]
numericalIntegrationDependencies = [trapezoidIntegrationString, makeGTRLogMatrixString, replicateString, fmapString, split2MatrixString,integrateGTRMatrixWithKString, foldlString, addMatricesString, adjustDiagString, getLogMatrixString, getPijString]

-- adding in rate modifiers dependencies to this
modifierDependencies :: [String]
modifierDependencies = [getModifierListSmallString]

gammaDependencies :: [String]
gammaDependencies = [gammaFunString, expX2YString, getNTilesString, gammaPDFString, cumulativeSumString, discreteGammaString, replicateString, fmapString, factorialString, logEString, factorialString, expEString, powerString, foldlString, zipString, lastString]

-- | getModelList take a list of character models and returns the dependency list for all models
-- combined --nub used later to rtemove dups.
getModelList :: [CharacterModel] -> Bool -> Bool -> [String]
getModelList charModelList areModifiers areBothDistributions =
  if null charModelList then []
  else
    let firstModel = head charModelList
        dist = fst $ branchLength firstModel
        (chModel,_,_,_) = changeModel firstModel
        rateM = fst $ head $ rateModifiers firstModel
    in
    --Neyman
    if chModel == Neyman then
      --need to modify so tell whetther to use general neyman withk (ie have exp and uni with k)
      if length (rateModifiers firstModel) == 1 then
        if dist == Uniform then
          if rateM == None then
            if not areModifiers then neymanUniformDependencies ++ getModelList (tail charModelList) areModifiers areBothDistributions
            else if areBothDistributions then neymanGeneralWithKDependencies ++ getModelList (tail charModelList) areModifiers areBothDistributions
            else neymanUniformWithKDependencies ++ getModelList (tail charModelList) areModifiers areBothDistributions
          else if rateM == Invariant then
            if areBothDistributions then neymanGeneralWithKDependencies ++ getModelList (tail charModelList) areModifiers areBothDistributions
            else neymanUniformWithKDependencies ++ getModelList (tail charModelList) areModifiers areBothDistributions
          else if rateM == Gamma then
            if areBothDistributions then neymanGeneralWithKDependencies ++ gammaDependencies ++ getModelList (tail charModelList) areModifiers areBothDistributions
            else neymanUniformWithKDependencies ++ gammaDependencies ++ getModelList (tail charModelList) areModifiers areBothDistributions
          else error "Rate modifier not implemented"
        else if dist == Exponential then
          if rateM == None then
            if not areModifiers then neymanExponentialDependencies ++ getModelList (tail charModelList) areModifiers areBothDistributions
            else if areBothDistributions then neymanGeneralWithKDependencies ++ getModelList (tail charModelList) areModifiers areBothDistributions
            else neymanExponentialWithKDependencies ++ getModelList (tail charModelList) areModifiers areBothDistributions
          else if rateM == Invariant then
            if areBothDistributions then neymanGeneralWithKDependencies ++ getModelList (tail charModelList) areModifiers areBothDistributions
            else neymanExponentialWithKDependencies ++ getModelList (tail charModelList) areModifiers areBothDistributions
          else if rateM == Gamma then
            if areBothDistributions then neymanGeneralWithKDependencies ++ gammaDependencies ++ getModelList (tail charModelList) areModifiers areBothDistributions
            else neymanExponentialWithKDependencies ++ gammaDependencies ++ getModelList (tail charModelList) areModifiers areBothDistributions
          else error "Rate modifier not implemented"
        else error "Distribution not implemented"
      else if length (rateModifiers firstModel) == 2 then
          if dist == Exponential then
            if areBothDistributions then neymanGeneralWithKDependencies ++ gammaDependencies ++ getModelList (tail charModelList) areModifiers areBothDistributions
            else neymanExponentialWithKDependencies ++ gammaDependencies ++ getModelList (tail charModelList) areModifiers areBothDistributions
          else if dist == Uniform then
            if areBothDistributions then neymanGeneralWithKDependencies ++ gammaDependencies ++ getModelList (tail charModelList) areModifiers areBothDistributions
            else neymanUniformWithKDependencies ++ gammaDependencies ++ getModelList (tail charModelList) areModifiers areBothDistributions
          else error "Distribution not implemented"
      else error "Too many (>2) rate modifiers"
    --Other Models (GTR 4-state)
    else
      let modelDependencies = getDependencies chModel dist (fst <$> rateModifiers firstModel)
      in
      modelDependencies ++ getModelList (tail charModelList) areModifiers areBothDistributions

-- | getDependencies takes a model, distribution, and rate modifiers klist and returns dependency list
-- only GTR for distribution because others are already baked in--no numerical integration
getDependencies :: MarkovModel -> Distribution -> [Modifier] -> [String]
getDependencies modelType distribution modifierList =
  if modelType == GTR then getModelDep modelType distribution ++ getDistDep distribution ++ getModList modifierList
  else getModelDep modelType distribution ++ getModList modifierList

-- | getModelDep takes model and returns dependency list
getModelDep :: MarkovModel -> Distribution -> [String]
getModelDep modelType distribution
  | modelType == GTR =  gtrDependencies
  | modelType == K80 =
                        if distribution == Exponential then k80ExponentialDependencies
                        else k80UniformDependencies
  | modelType == F81 =
                        if distribution == Exponential then f81ExponentialDependencies
                        else f81UniformDependencies
  | modelType == HKY85 =
                        if distribution == Exponential then hky85ExponentialDependencies
                       else hky85UniformDependencies
  | modelType == F84 =
                       if distribution == Exponential then f84ExponentialDependencies
                       else f84UniformDependencies
  | modelType == TN93 =
                       if distribution == Exponential then tn93ExponentialDependencies
                       else tn93UniformDependencies
  | otherwise = error ("Markov model " ++ show modelType ++ " not implemented")

-- | getDistDep take sdistrinbution and returns dependencies
getDistDep :: Distribution -> [String]
getDistDep distribution
  | distribution == Uniform = [getUniformPdfString]
  | distribution == Exponential = [getExponentialPdfString]
  | otherwise = error ("Distribution " ++ show distribution ++ " not implemented")

-- | getModList takes rate modifiers and returns dependency list
getModList :: [Modifier] -> [String]
getModList modList =
  let first = head modList
  in
  if first == None then []
  else if (length modList == 2 || first == Invariant) || (first == Gamma) then
    modifierDependencies ++ gammaDependencies
  else error ("Rate modifiers list " ++ show modList ++ " has unimplemented modifier(s)")


-- | scanModifiers checks to see if ther are any non-None rate modifiers
scanModifiers :: [CharacterModel] -> Bool
scanModifiers charList =
  not (null charList) &&
  let first =  fst $ head $ rateModifiers $ head charList
  in
  first /= None || scanModifiers (tail charList)

-- | scanDistributions checks to see whether both Uniform and Exponential are used
-- really for Neyman to see if use general or specific versions of model
-- if both are used then return True else False
scanDistributions :: [CharacterModel] -> Bool -> Bool -> Bool
scanDistributions charList hasUniform hasExponential =
  if null charList then hasUniform && hasExponential
  else
    let first =  fst $ branchLength $ head charList
    in
    if first == Uniform then scanDistributions (tail charList) True hasExponential
    else if first == Exponential then scanDistributions (tail charList) hasUniform True
    else error "Unreconized distibution in scanDistributions"

-- | makeProgramStringCharacters takes character parameters and returns String of HAskell program
makeProgramStringCharacters :: [CharacterModel] -> String
makeProgramStringCharacters inModelList =
    let areRateModifiers = scanModifiers inModelList
        areBothDistributions = scanDistributions inModelList False False
        modelList = nub $ nullString : getModelList inModelList areRateModifiers areBothDistributions
        firstPart = makeDependenciesString modelList
        modelString = makeCharacterModelsString inModelList 0 areRateModifiers areBothDistributions
    in
    programStartString ++ firstPart ++ "main=do\n" ++ modelString

-- | makeDependenciesString take a list of Strings of functions need by the output program and puts
-- them in a single String
makeDependenciesString :: [String] -> String
makeDependenciesString inList =
    if null inList then []
    else
        let first = head inList
        in
        first ++ makeDependenciesString (tail inList)

-- | getInfo takes CharacterModel type and ertuns field as a tuple
getInfo :: CharacterModel -> (String, [String], (Distribution, [DistributionParameter]),
  [(Modifier, [DistributionParameter])], (MarkovModel, RMatrix, PiVector, [ModelParameter]), Int, Int)
getInfo charInfo =
  (characterName charInfo, alphabet charInfo, branchLength charInfo, rateModifiers charInfo, changeModel
    charInfo, precision charInfo, charLength charInfo)

-- | getModifierParams gets params for sending to getModifierListSmall
getModifierParams :: [(Modifier, [DistributionParameter])] -> (Int, Double, Int, Int, Double)
getModifierParams tcmRateModifiers =
  let first = fst $ head tcmRateModifiers
  in
  if first == None then (0,0,0,0,0)
  else if length tcmRateModifiers == 1 then
    if fst (head tcmRateModifiers) == Invariant then (1,head $ snd $ head tcmRateModifiers,0,0,0)
    else -- gamma only
      let argList = snd $ head tcmRateModifiers
          alpha = argList !! 1
          numClasses = round $ head argList
      in
      (0,0,1,numClasses,alpha)
  else -- both
      let theta = (head $ snd $ head tcmRateModifiers)
          argList = snd $ tcmRateModifiers !! 1
          alpha = argList !! 1
          numClasses = round $ head argList
      in
      (1,theta,1,numClasses,alpha)

-- | getDistString returna stroing value of bracnh distribution function
getDistString :: Distribution -> String
getDistString dist
  | dist == Uniform = "e8"
  | dist == Exponential = "h1"
  | otherwise = error ("Distribution " ++ show dist ++ " not implemented")

-- |makeCharacterModelsString take character model list and returns string of character blocks
--program as String
makeCharacterModelsString :: [CharacterModel] -> Int -> Bool-> Bool -> String
makeCharacterModelsString charModelList counter areRateModifiers areBothDistributions =
    if null charModelList then []
    else
       let (_, tcmAlphabet,(branchDist, branchParams), tcmRateModifiers,(tcmChangeModel, tcmQ, tcmP, modelParams),tcmPrecision, _) = getInfo $ head charModelList
           (a,b,c,d,e) = getModifierParams tcmRateModifiers
           -- weightString = pairList2String "[" (getModifierListSmall a b c d e tcmPrecision)
           maximumTime = getEndTime branchDist (head branchParams)
       in
       if tcmChangeModel == K80 || tcmChangeModel == F81 || tcmChangeModel == HKY85 || tcmChangeModel == F84 || tcmChangeModel == TN93 then
        let letString = ("  let wC" ++ show counter ++ "=a1 " ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d ++ " " ++ show e ++ " " ++ show tcmPrecision ++ "\n")
            modelFunctionName = get4StateModelName tcmChangeModel branchDist
            outString = ("  putStr (s0$j1 b0 " ++ modelFunctionName ++ " " ++ show (head branchParams) ++ " " ++ show tcmPrecision ++ " " ++ show modelParams ++ " " ++ show tcmP++ " wC" ++ show counter ++ ")\n")
            outString2 = ("  putStr (s0$j1 b0 " ++ modelFunctionName ++ " " ++ show (head branchParams) ++ " " ++ show tcmPrecision ++ " " ++ show modelParams ++ " " ++ show tcmP++ " [(1,1)] " ++ ")\n")
        in
        if fst (head tcmRateModifiers) == None then outString2 ++ makeCharacterModelsString (tail charModelList) (counter + 1) areRateModifiers areBothDistributions
        else letString ++ outString ++ makeCharacterModelsString (tail charModelList) (counter + 1) areRateModifiers areBothDistributions

       else if tcmChangeModel == GTR then
        let eigenString = ("  let (zC" ++ show counter ++ ",uC" ++ show counter ++ ",iC" ++ show counter ++ ") = a0 " ++ show (length tcmAlphabet) ++ " " ++ show tcmQ ++ " " ++ show tcmP ++ show tcmPrecision ++ "\n")
            distFunction = getDistString branchDist
        in
        if fst (head tcmRateModifiers) == None then
          let outString = ("  putStr (s0$c1 b0 " ++ show (last tcmAlphabet) ++ " zC" ++ show counter ++ " uC" ++ show counter ++ " iC" ++ show counter ++ " " ++ show (length tcmAlphabet) ++ " " ++ show (head branchParams) ++ " " ++ show maximumTime ++ " " ++ show tcmPrecision ++ " " ++ distFunction ++ " [(1,1)]" ++ ")\n")
          in
          eigenString ++ outString ++ makeCharacterModelsString (tail charModelList) (counter + 1) areRateModifiers areBothDistributions
        else
          let letString = ("  let wC" ++ show counter ++ "=a1 " ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d ++ " " ++ show e ++ " " ++ show tcmPrecision ++ "\n")
              outString = ("  putStr (s0$c1 b0 " ++ show (last tcmAlphabet) ++ " zC" ++ show counter ++ " uC" ++ show counter ++ " iC" ++ show counter ++ " " ++ show (length tcmAlphabet) ++ " " ++ show (head branchParams) ++ " " ++ show maximumTime ++ " " ++ show tcmPrecision ++ " " ++ distFunction ++ " wC" ++ show counter ++ ")\n")
          in
          letString ++ eigenString ++ outString ++ makeCharacterModelsString (tail charModelList) (counter + 1) areRateModifiers areBothDistributions
       else if tcmChangeModel == Neyman then
         -- change to generated in code
         if fst (head tcmRateModifiers) == None && not areRateModifiers then
           if branchDist == Uniform then
             let outString = ("  putStr (s0$c9 u0 " ++ show (length tcmAlphabet) ++ " " ++ show (head branchParams) ++ " " ++ show tcmPrecision ++ " " ++ show (last tcmAlphabet) ++ ")\n")
             in
             outString ++ makeCharacterModelsString (tail charModelList) counter areRateModifiers areBothDistributions
           else
             let outString = ("  putStr (s0$c9 y0 " ++ show (length tcmAlphabet) ++ " " ++ show (head branchParams) ++ " " ++ show tcmPrecision ++ " " ++ show (last tcmAlphabet) ++ ")\n")
             in
             outString ++ makeCharacterModelsString (tail charModelList)  counter areRateModifiers areBothDistributions
         else if fst (head tcmRateModifiers) == None && areRateModifiers then -- there are rateModifiers in other charcater blocks
            if not areBothDistributions then -- only a single branch distribution so use only specific function
               if branchDist == Uniform then
               let outString = ("  putStr (s0$a7 " ++ show (length tcmAlphabet) ++ " " ++ show (head branchParams) ++ " " ++ show tcmPrecision ++ " [(1,1)] " ++ show (last tcmAlphabet) ++ ")\n")
               in
               outString ++ makeCharacterModelsString (tail charModelList) counter areRateModifiers areBothDistributions
             else
               let outString = ("  putStr (s0$a8 " ++ show (length tcmAlphabet) ++ " " ++ show (head branchParams) ++ " " ++ show tcmPrecision ++ " [(1,1)] " ++ show (last tcmAlphabet) ++ ")\n")
               in
               outString ++ makeCharacterModelsString (tail charModelList)  counter areRateModifiers areBothDistributions
            else -- there are both uniform and exponential distributions so use general function
              if branchDist == Uniform then
               let outString = ("  putStr (s0$n0 0 " ++ show (length tcmAlphabet) ++ " " ++ show (head branchParams) ++ " " ++ show tcmPrecision ++ " [(1,1)] " ++ show (last tcmAlphabet) ++ ")\n")
               in
               outString ++ makeCharacterModelsString (tail charModelList) counter areRateModifiers areBothDistributions
             else
               let outString = ("  putStr (s0$n0 1 " ++ show (length tcmAlphabet) ++ " " ++ show (head branchParams) ++ " " ++ show tcmPrecision ++ " [(1,1)] " ++ show (last tcmAlphabet) ++ ")\n")
               in
               outString ++ makeCharacterModelsString (tail charModelList)  counter areRateModifiers areBothDistributions
         else --has rate modifiers in this character block
           if not areBothDistributions then --only a single branch distribution so use only specific function
              if branchDist == Uniform then
               let  letString = ("  let wC" ++ show counter ++ "=a1 " ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d ++ " " ++ show e ++ " " ++ show tcmPrecision ++ "\n")
                    outString = ("  putStr (s0$a7 " ++ show (length tcmAlphabet) ++ " " ++ show (head branchParams) ++ " " ++ show tcmPrecision ++ " wC" ++ show counter ++ " " ++ show (last tcmAlphabet) ++ ")\n")
               in
               letString ++ outString ++ makeCharacterModelsString (tail charModelList) (counter + 1) areRateModifiers areBothDistributions
             else
               let letString = ("  let wC" ++ show counter ++ "=a1 " ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d ++ " " ++ show e ++ " " ++ show tcmPrecision ++ "\n")
                   outString = ("  putStr (s0$a8 " ++ show (length tcmAlphabet) ++ " " ++ show (head branchParams) ++ " " ++ show tcmPrecision ++ " wC" ++ show counter ++ " " ++ show (last tcmAlphabet) ++ ")\n")
               in
               letString ++ outString ++ makeCharacterModelsString (tail charModelList)  (counter + 1) areRateModifiers areBothDistributions
           else -- there are both uniform and exponential distributions so use general function
             if branchDist == Uniform then
               let  letString = ("  let wC" ++ show counter ++ "=a1 " ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d ++ " " ++ show e ++ " " ++ show tcmPrecision ++ "\n")
                    outString = ("  putStr (s0$n0 0 " ++ show (length tcmAlphabet) ++ " " ++ show (head branchParams) ++ " " ++ show tcmPrecision ++ " wC" ++ show counter ++ " " ++ show (last tcmAlphabet) ++ ")\n")
               in
               letString ++ outString ++ makeCharacterModelsString (tail charModelList) (counter + 1) areRateModifiers areBothDistributions
             else
               let letString = ("  let wC" ++ show counter ++ "=a1 " ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d ++ " " ++ show e ++ " " ++ show tcmPrecision ++ "\n")
                   outString = ("  putStr (s0$n0 1 " ++ show (length tcmAlphabet) ++ " " ++ show (head branchParams) ++ " " ++ show tcmPrecision ++ " wC" ++ show counter ++ " " ++ show (last tcmAlphabet) ++ ")\n")
               in
               letString ++ outString ++ makeCharacterModelsString (tail charModelList)  (counter + 1) areRateModifiers areBothDistributions
       else error "Character model not implemented"
       --)

-- | matrix2String converts matrinx to string
matrix2String :: [[Double]] -> String
matrix2String inMatrix =
    if null inMatrix then "\n"
    else
        let firstRow = show Control.Applicative.<$> head inMatrix
            firstString = concatMap (++ " ") firstRow
        in
        firstString ++ "\n" ++ matrix2String (tail inMatrix)

-- | makeNeymanMatrix is a general version of makeTCM taing arguments for all of Neyman versions
-- and returning the log matrix for TCM file creation
makeNeymanMatrix :: (Double -> Int-> Int -> Double -> Double) ->  Distribution -> Int -> DistributionParameter -> Int ->  [(Double, Double)] -> String -> [[Double]]
makeNeymanMatrix logType distribution alphabetSize alphaParam iterations modifiers lastElement=
  let pairList = parmap rdeepseq (neymanGeneralWithK distribution alphabetSize alphaParam iterations) modifiers
      pii = sum $ fmap fst pairList
      pij = sum $ fmap snd pairList
      logPii = (-1) *  logType pii iterations 0 0
      logPij = (-1) * logType pij iterations 0 0
      logMatrix = makeSimpleMatrix alphabetSize logPii logPij 0 lastElement
  in
  logMatrix

-- | getEndTime sets maximum time for later numerical integration
-- if exponential--maxTime, if uniform then the parameter, else error
getEndTime :: Distribution -> Double -> Double
getEndTime dist distParam
  | dist == Uniform = distParam
  | dist == Exponential = maxTime
  | otherwise = error ("Distribution " ++ show dist ++ " not implemented")

-- | get4StateModel takes model and dist and retuirns correct integrated function
get4StateModel :: MarkovModel -> Distribution -> ([Double] -> [Double] -> Double -> Int -> (Double, Double) -> [[Double]])
get4StateModel modelType distribution
  | (distribution /= Exponential) && (distribution /= Uniform) = error ("Distribution model " ++ show distribution ++ " not implemented")
  | modelType == K80 =
    if distribution == Exponential then k80ExponentialWithK
    else k80UniformWithK
  | modelType == F81 =
    if distribution == Exponential then f81ExponentialWithK
    else f81UniformWithK
  | modelType == HKY85 =
    if distribution == Exponential then hky85ExponentialWithK
    else hky85UniformWithK
  | modelType == F84 =
    if distribution == Exponential then f84ExponentialWithK
    else f84UniformWithK
  | modelType == TN93 =
    if distribution == Exponential then tn93ExponentialWithK
    else tn93UniformWithK
  | otherwise = error ("Markov model " ++ show modelType ++ " not implemented")

-- | get4StateModelName takes model and dist and retuirns name of integrated function
get4StateModelName :: MarkovModel -> Distribution -> String
get4StateModelName modelType distribution
  | (distribution /= Exponential) && (distribution /= Uniform) = error ("Distribution model " ++ show distribution ++ " not implemented")
  | modelType == K80 =
    if distribution == Exponential then "k0" --"k80ExponentialWithK"
    else "k1"
  | modelType == F81 =
    if distribution == Exponential then "j8" --"f81ExponentialWithK"
    else "j9"
  | modelType == HKY85 =
    if distribution == Exponential then "j6" --"hky85ExponentialWithK"
    else "j7"
  | modelType == F84 =
    if distribution == Exponential then "j4" --"f84ExponentialWithK"
    else "j5"
  | modelType == TN93 =
    if distribution == Exponential then "j2" --"tn93ExponentialWithK"
    else "j3"
  | otherwise = error ("Markov model " ++ show modelType ++ " not implemented")

-- Map a function over a traversable structure in parallel
-- Preferred over parMap which is limited to lists
-- Add chunking (with arguement) (via chunkList) "fmap blah blah `using` parListChunk chunkSize rseq/rpar"
-- but would have to do one for lists (with Chunk) and one for vectors  (splitAt recusively)
parmap :: (Traversable t) => Strategy b -> (a -> b) -> t a -> t b
parmap strat f = withStrategy (parTraversable strat) . fmap f

-- | makeTCM takes unit function (logE or Log2) model type, branch model (and param(s)), and alphabet to create TCMs for POY/PCG
-- the values will be log2/e/10 whatever (depending on input function) of integrated probabilitites of change (pij)
-- the "name" string is returned for filename purposes
makeTCM :: (Double -> Int-> Int -> Double -> Double) -> CharacterModel -> (String, [String], [[Double]])
makeTCM logType charInfo =
    let (tcmName, tcmAlphabet,(branchDist, branchParams), tcmRateModifiers,(tcmChangeModel, tcmR, tcmP, modelParams),tcmPrecision, _) = getInfo charInfo
        classList = getModifiers tcmRateModifiers tcmPrecision
        maximumTime = getEndTime branchDist (head branchParams)
    in
    -- trace ("MTCM: " <> (show tcmChangeModel)) $
    if tcmChangeModel == Neyman then
      let logMatrix = makeNeymanMatrix logType branchDist (length tcmAlphabet) (head branchParams) tcmPrecision classList (last tcmAlphabet)
      in
      (tcmName, tcmAlphabet, logMatrix)
    else
      if tcmChangeModel == GTR then
        let -- external library seems to violate an order or other invaritant in my code jumbles eigenvectors etc
            -- (eigenValueList, uMatrix, uInvMatrix) =  makeGTRMatrixExt (length tcmAlphabet) tcmR tcmP
            (eigenValueList, uMatrix, uInvMatrix) =  makeGTRMatrixLocal (length tcmAlphabet) tcmR tcmP
            logMatrix = makeGTRLogMatrix logType (last tcmAlphabet) eigenValueList uMatrix uInvMatrix (length tcmAlphabet) (head branchParams) maximumTime tcmPrecision branchDist classList
            -- (_, uMatrix2, uInvMatrix2) =  makeGTRMatrixLocal (length tcmAlphabet) tcmR tcmP
        in
        --trace ("MTCM: " <> (show (eigenValueList, uMatrix, uInvMatrix))) $
        (tcmName, tcmAlphabet, logMatrix)
      else
        let fourStateModel = get4StateModel tcmChangeModel branchDist
            logMatrix = makeGTRLogMatrix4State logType fourStateModel (head branchParams) tcmPrecision modelParams tcmP classList
        in
        (tcmName, tcmAlphabet, logMatrix)


-- | makeSimpleMatrix takes values for diagonal and non-diagnoal and row/column size and returns
-- square matrix
makeSimpleMatrix :: Int -> Double -> Double -> Int -> String -> [[Double]]
makeSimpleMatrix size diag nondiag rowCounter lastElement=
    if rowCounter == size then []
    else
        let first = replicate rowCounter nondiag
            second = replicate (size - rowCounter - 1) nondiag
        in
        -- if (lastElement /= "-") || (rowCounter < (size -1)) then (first ++ [diag] ++ second) : makeSimpleMatrix size diag nondiag (rowCounter + 1) lastElement
        -- else 
        --[first ++ [0]]
        (first ++ [diag] ++ second) : makeSimpleMatrix size diag nondiag (rowCounter + 1) lastElement
        -- else 
        --[first ++ [0]]

-- | getRateParams returns the total number of rate parameters but adjusting for the convention
-- of not including the number of classes as a parameer for discrete gamma
getRateParams :: [(Modifier, [DistributionParameter])] -> Int -> Int
getRateParams rateModList acc =
  if null rateModList then acc
  else
    let first = head rateModList
        modifier = fst first
    in
    if modifier == Gamma then getRateParams (tail rateModList) (acc + 1)
    else getRateParams (tail rateModList) (acc + length (snd first))

-- | getAICBIC takes list of character models and returns Akaike and
-- Bayesian Information Content (AIC, BIC) values
-- AIC = -2 * |P|; BIC = \Sum_{models} ln (charLength * numchars of type) * |P_{model}|
-- initialted with 0 0
-- length = 1 for static--mutiplying by number of block characters
-- since branches are not estimated, but a distribution, the tre "edge"
-- paramter number is only that of its distibution (e.g. 1 for exponential)
-- not the 2n-1 (n leaves) typical of a tree
-- if GTR rates nromalized to 1 and symetrical 5 parameters for DNA (n^2 -n)/2 -1 in gebneral for
-- alphabet size of n
-- For discrete gamma including 2 parameters (alpha, number classes). Looks like some only
-- say there is one parameter, since only alpha is estimated although number of classes is specified.
getAICBIC :: [(CharacterModel, Int)] -> Double -> Double -> (Double, Double)
getAICBIC charInfoPairList aicCounter bicCounter =
  if null charInfoPairList then (2*aicCounter, bicCounter)
  else
    let charInfo = fst $ head charInfoPairList
        numThisBlockType = fromIntegral $ snd $ head charInfoPairList
        -- tcmAlphabet = alphabet charInfo
        (_, branchParams) = branchLength charInfo
        tcmRateModifiers = rateModifiers charInfo
        (tcmChangeModel, _, tcmP, _) = changeModel charInfo
        lnLength = log $ fromIntegral $ charLength charInfo * numThisBlockType
        branchParamNumber = fromIntegral $ length branchParams
        rateModParamNumber = fromIntegral $ getRateParams tcmRateModifiers 0 --fromIntegral $ sum $ fmap length $ fmap snd tcmRateModifiers
        paramCounter = branchParamNumber + rateModParamNumber
    in
    if tcmChangeModel == Neyman then --only single parameter 'r' alphabet size -- but by convention (JC69) has zero parameters
      let curAIC = 0 + paramCounter
      in
      getAICBIC (tail charInfoPairList) (curAIC + aicCounter) ((lnLength * curAIC) + bicCounter)
    else if tcmChangeModel == K80 then
      let curAIC = 1 + paramCounter
      in
      getAICBIC (tail charInfoPairList) (curAIC + aicCounter) ((lnLength * curAIC) + bicCounter)
    else if tcmChangeModel == F81 then
      let curAIC = 3 + paramCounter
      in
      getAICBIC (tail charInfoPairList) (curAIC + aicCounter) ((lnLength * curAIC) + bicCounter)
    else if tcmChangeModel == HKY85 || tcmChangeModel == F84 then
      let curAIC = 4 + paramCounter
      in
      getAICBIC (tail charInfoPairList) (curAIC + aicCounter) ((lnLength * curAIC) + bicCounter)
    else if tcmChangeModel == TN93 then
      let curAIC = 5 + paramCounter
      in
      getAICBIC (tail charInfoPairList) (curAIC + aicCounter) ((lnLength * curAIC) + bicCounter)
    else if tcmChangeModel == GTR then
      let alphSize = fromIntegral $ length tcmP
          gtrParams = alphSize - 1 + (((alphSize * alphSize) - alphSize)/2) - 1
          curAIC = gtrParams + paramCounter
      in
      getAICBIC (tail charInfoPairList) (curAIC + aicCounter) ((lnLength * curAIC) + bicCounter)
    else error ("Change model " ++ show tcmChangeModel ++ " not implemented for AIC, BIC")

-- | makeLogMatrix takes the probMatrix List and converts to logMatrix for return
makeLogMatrix :: (Double -> Int-> Int -> Double -> Double) -> String -> Int -> Int -> [[[Double]]]-> [[Double]]
makeLogMatrix logType lastAlphElement alphSize iterations probMatrixList =
  let zeroMatrix = replicate alphSize $ replicate alphSize 0.0
      pMatrix = foldl' addMatrices  zeroMatrix probMatrixList
      -- make symmetrical
      pMatrixSym =  split2Matrix alphSize $ adjustSym pMatrix 0 0
      pMatrixAdjusted = adjustDiag pMatrixSym pMatrixSym 0
      logMatrix = split2Matrix alphSize $ (* (-1)) <$> getLogMatrix logType pMatrixAdjusted alphSize lastAlphElement 0 0 iterations
  in
  --trace ("MLM: " <> (show pMatrixAdjusted))
  logMatrix

-- | makeGTRLogMatrix is a general version of makeTCM taking arguments for all of GTR versions
-- and returning the log matrix for TCM file creation
makeGTRLogMatrix ::  (Double -> Int-> Int -> Double -> Double) -> String -> [Double] -> [[Double]] -> [[Double]] -> Int ->  Double -> Double -> Int -> Distribution -> [(Double, Double)] -> [[Double]]
makeGTRLogMatrix logType lastAlphElement eigenValueList uMatrix uInvMatrix alphSize probDistParam maxValue iterations distribution modifiers =
  let -- zeroMatrix = replicate alphSize $ replicate alphSize 0.0
      probMatrixList  = parmap rdeepseq (split2Matrix alphSize . integrateGTRMatrixWithK eigenValueList uMatrix uInvMatrix 0 0 probDistParam maxValue iterations alphSize distribution) modifiers
      logMatrix = makeLogMatrix logType lastAlphElement alphSize iterations probMatrixList
  in
  --trace ("MGTLM: " <> (show probMatrixList))
  logMatrix

-- | makeGTRLogMatrix4State is a general version of makeTCM taking arguments for all of 4-state model
-- and returning the log matrix for TCM file creation
-- uses the integrated model versions
-- Assumes ACGT in order
makeGTRLogMatrix4State ::  (Double -> Int-> Int -> Double -> Double) -> ([Double] -> [Double] -> Double -> Int -> (Double, Double) -> [[Double]]) -> Double -> Int -> [Double] -> [Double] -> [(Double, Double)] -> [[Double]]
makeGTRLogMatrix4State logType modelFunction probDistParam  iterations modelParams piVector modifiers =
  let lastAlphElement = "T"
      alphSize = 4
      probMatrixList  = parmap rdeepseq (modelFunction modelParams piVector probDistParam iterations) modifiers
      logMatrix = makeLogMatrix logType lastAlphElement alphSize iterations probMatrixList
  in
  logMatrix

-- | getLogMatrix takes the logType of each element, (n-1)(n-1) <- 0 if
-- last alphabet element is "-" (removed last element gap thing)
getLogMatrix :: (Double -> Int-> Int -> Double -> Double) -> [[Double]] -> Int -> String -> Int -> Int -> Int -> [Double]
getLogMatrix logType aMatrix alphSize lastElement iRow jColumn iterations
  | iRow == alphSize = []
  | jColumn == alphSize = getLogMatrix logType aMatrix alphSize lastElement (iRow + 1) 0 iterations
  | (iRow == (alphSize - 1)) && (jColumn == (alphSize - 1)) =
    --if lastElement == "-" then [0]
    --else 
    logType ((aMatrix !! iRow) !! jColumn) iterations 0 0 : getLogMatrix logType aMatrix alphSize lastElement iRow (jColumn + 1) iterations
  | otherwise = logType ((aMatrix !! iRow) !! jColumn) iterations 0 0 : getLogMatrix logType aMatrix alphSize lastElement iRow (jColumn + 1) iterations

-- | integrateGTRMatrixWithK takes arguments to generate Pij with time and intergates with distibution of time probs (exponential or uniform)
-- uses the weights pair (kweight, kfraction) for weight class functions
-- makes overall probs symmetrical
-- start with row 0 and column 0
integrateGTRMatrixWithK :: [Double] -> [[Double]] -> [[Double]]-> Int -> Int -> Double -> Double -> Int -> Int -> Distribution -> (Double, Double) -> [Double]
integrateGTRMatrixWithK eigenValueList uMatrix uInvMatrix iRow jColumn probDistParam maxValue iterations alphabetSize distribution (kWeight, kFraction) =
  -- trace ("KWF :" ++ show kWeight ++ " " ++ show kFraction ++ " " ++ show iRow ++ " " ++ show jColumn) (
  let pijFun = getPij
  in
  if iRow == alphabetSize then []
  else if jColumn == alphabetSize then integrateGTRMatrixWithK eigenValueList uMatrix uInvMatrix (iRow + 1) 0 probDistParam maxValue iterations alphabetSize distribution (kWeight, kFraction)
  else
    if distribution == Uniform then
      if kWeight == 0 then  0:integrateGTRMatrixWithK eigenValueList uMatrix uInvMatrix iRow (jColumn + 1) probDistParam maxValue iterations alphabetSize distribution (kWeight, kFraction)
      else
        let value = trapezoidIntegration pijFun getUniformPdf eigenValueList uMatrix uInvMatrix iRow jColumn probDistParam probDistParam iterations 0 kWeight
        in
        (kFraction * value) : integrateGTRMatrixWithK eigenValueList uMatrix uInvMatrix iRow (jColumn + 1) probDistParam maxValue iterations alphabetSize distribution (kWeight, kFraction)
    else if distribution == Exponential then
      if kWeight == 0 then 0:integrateGTRMatrixWithK eigenValueList uMatrix uInvMatrix iRow (jColumn + 1) probDistParam maxValue iterations alphabetSize distribution (kWeight, kFraction)
      else
        let value = trapezoidIntegration pijFun getExponentialPdf eigenValueList uMatrix uInvMatrix iRow jColumn probDistParam maxTime iterations 0 kWeight
        in
        (kFraction * value) : integrateGTRMatrixWithK  eigenValueList uMatrix uInvMatrix iRow (jColumn + 1) probDistParam maxValue iterations alphabetSize distribution (kWeight, kFraction)
  else error ("Distribution " ++ show distribution ++ " is not implemented")
  -- )

-- | getModifiers takes rate modifier information (invariant, gamma) and retirns a list
-- of rate class pairs of class weight and class fraction
getModifiers :: [(Modifier, [DistributionParameter])] -> Int -> [(Double, Double)]
getModifiers tcmRateModifiers iterations
  | length tcmRateModifiers == 1 =
  if fst (head tcmRateModifiers) == None then [(1,1)]
  else if fst (head tcmRateModifiers) == Invariant then
    [(0, head $ snd $ head tcmRateModifiers), (1 / (1 - head (snd $ head tcmRateModifiers)),1 - head (snd $ head tcmRateModifiers))]
  else if fst (head tcmRateModifiers) == Gamma then
    let argList = snd $ head tcmRateModifiers
        alpha = argList !! 1
        numClasses = round $ head argList
        maxRate = maxGammaRate
        rectangles = iterations * numClasses
        weightList = discreteGamma alpha numClasses maxRate iterations rectangles
        fractionList = replicate numClasses (1.0 / head argList)
    in
    zip weightList fractionList
  else error ("Rate modifier " ++ show (fst $ head tcmRateModifiers) ++ " not implemented)")
  | length tcmRateModifiers == 2 =
    if fst (head tcmRateModifiers) == Invariant then
      let theta = (head $ snd $ head tcmRateModifiers)
          argList = snd $ tcmRateModifiers !! 1
          alpha = argList !! 1
          numClasses = round $ head argList
          maxRate = maxGammaRate --probably should be in config file
          rectangles = iterations * numClasses
          gammaWeightList = discreteGamma alpha numClasses maxRate iterations rectangles
          gammaFractionList = replicate numClasses (1/ head argList)
          adjustedGammaWeightList = fmap (* (1/ (1 - theta))) gammaWeightList
          adjustedGammaFractionList = fmap (* (1 - theta)) gammaFractionList
      in
      zip (0 : adjustedGammaWeightList) (theta : adjustedGammaFractionList)
    else error "Invariants should always be first in rate modifiers"
  | otherwise = error ("Maximum of two rate modifiers" ++ show (length tcmRateModifiers) ++ " were specified")

