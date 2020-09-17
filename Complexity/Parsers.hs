{- |
Module      :  Parser for Kolmogorov (algorithmic) complexity 'Machine' definition
Description :  Program reads input file with description and parameters for 
               machine containing models for graphs and charcater models
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

-}


module Complexity.Parsers
  ( parseMachineFile
  ) where

import Data.List
import Control.Applicative
import Data.String.Utils
import Data.Char
import Complexity.Types
import Data.Maybe
import Complexity.Utilities

-- | ArgCrust are charcaterts in arguments that may need to be filtered
argCruft :: [Char]
argCruft = ['(', ')','[',']']

-- | removeContents filters out comments lines (beginning with '--') from file input
removeComments :: [String] -> String
removeComments inStringList =
  if null inStringList then []
  else
    let firstLine = head inStringList
    in
    --is Comment
    if (length firstLine > 1) && (head firstLine == '-') && ((firstLine !! 1) == '-') then removeComments $ tail inStringList
    else firstLine ++ removeComments (tail inStringList)

-- | stupid way to remove white space
removeWhiteSpace :: String -> String
removeWhiteSpace inString =
  if null inString then []
  else
    let first = head inString
    in
    if isSpace first then removeWhiteSpace $ tail inString
    else first : removeWhiteSpace (tail inString)

-- | split with takes  astring and splits on a character returning a list of strings
-- remove target from list strings
divideWith :: (Eq a) => a -> [a] -> [[a]]
divideWith target inString =
  if null inString then []
  else
    let first = takeWhile (/= target) inString
        rest = dropWhile (/= target) inString
    in
    if null rest then [first]
    else first : divideWith target (tail rest)

-- | getSections takes file contents (after comments removed) and splits into the machine sections
getSections :: String -> [String]
getSections inString =
  if null inString then []
  else
    let first = takeWhile (/= '}') inString
    in
    first : getSections (tail $ dropWhile (/= '}') inString)

-- | getMachineElement extract machine parts from [string]
getMachineElements :: [String] -> [(String, String, Int)]
getMachineElements inStringList =
  if null inStringList then []
  else
    let first = divideWith ':' $ head inStringList
    in
    --trace ((show $ head inStringList) ++ " tail= " ++ (show $ tail inStringList)) (
    if fmap toLower (head first) == "graph" then
      ("graph", last first, 1) : getMachineElements (tail inStringList)
    else if fmap toLower (head first) == "block" then
      ("block",  first !! 1, read (first !! 2 ) :: Int) : getMachineElements (tail inStringList)
    else error ("Unrecognized machine element " ++ head first)


-- | getGraphName extracts graph from elements list
getGraphName :: [(String, String, Int)] -> String
getGraphName elementList =
  if null elementList then error "Graph specification not found"
  else
    let (fType, fName, _) = head elementList
    in
    if fmap toLower fType == "graph" then fName
    else getGraphName $ tail elementList

-- | getBlocks extracts block pairs from elements list
getBlockName :: [(String, String, Int)] -> [(String, Int)]
getBlockName elementList =
  if null elementList then []
  else
    let (fType, fName, fNumber) = head elementList
    in
    if fmap toLower fType == "block" then (fName, fNumber) : getBlockName (tail elementList)
    else getBlockName $ tail elementList


-- | parseMachine takes machine string and returns names in order for parsing of other elements
parseMachine :: String -> (String, String, [(String, Int)])
parseMachine inString =
  if null inString then error "No machine specification in input file"
  else
    let name = last $ words $ takeWhile (/= '{') inString
        guts = removeWhiteSpace $ takeWhile (/= '}') $ tail $ dropWhile (/= '{') inString
        pieces = divideWith ';' guts
        machineElements =  getMachineElements pieces
    in
    let graphNameLocal = getGraphName machineElements
        blockPairs = getBlockName machineElements
    in
    --trace (name ++ " " ++ graphName ++ " " ++ show blockPairs)
    (name, graphNameLocal, blockPairs)


-- | getNumber looks through contents of graph specification and 
-- pulls parts matching in string throuws error if not found
getNumber :: String -> [String] -> Int
getNumber findHere guts =
  if null guts then error ("No parameter " ++ findHere ++ " specified in graph")
  else
    let firstGut = toLower Control.Applicative.<$> head guts
        parts = divideWith ':' firstGut
    in
    if length parts /=2 then error ("Incorrect number of parameters " ++ show (length parts) ++ " in " ++ findHere ++ " in graphModel\n Should be 1.")
    else if findHere == head parts then (read (last parts) :: Int)
    else getNumber findHere (tail guts)

-- | parseGraph takes graph string and parses
parseGraph :: String -> [String] -> [GraphModel]
parseGraph graphNameLocal inStringList=
  if null inStringList then []
  else
    let inString = head inStringList
        gName = last $ words $ takeWhile (/= '{') inString
        guts = removeWhiteSpace $ takeWhile (/= '}') $ tail $ dropWhile (/= '{') inString
        pieces = divideWith ';' guts
        nLeaves = getNumber "leaves" pieces
        nRoots = getNumber "roots" pieces
        nSingletons = getNumber "singletons" pieces
        nNetworkEdges = getNumber "networkedges" pieces
    in
    if graphNameLocal == gName then
      let theGraphModel = GraphModel {graphName = gName, numLeaves = nLeaves, numRoots = nRoots, numSingletons = nSingletons, numNetworkEdges = nNetworkEdges}
      in
      --defaultGraph
      theGraphModel : parseGraph graphNameLocal (tail inStringList)
    else parseGraph graphNameLocal (tail inStringList)

-- | getBlock takes a Sring--the name of the block and looks into a list of (String, Int)
-- pairs and returns the number of chats if the name finds a match--zwero otherwise if not found.
getBlock :: String -> [(String, Int)] -> Int
getBlock name pairList =
  if null pairList then 0
  else
    let (fName, fNumber) = head pairList
    in
    if fName == name then fNumber
    else getBlock name (tail pairList)

-- | getAlphabet take string list and sees if it starts with alpohabet then parses
getAlphabet :: [String] -> [String]
getAlphabet inList =
  if null inList then error "No alphabet specifed in blockModel"
  else
    let first = removeWhiteSpace $ head inList
        parts = divideWith ':' first
    in
    if fmap toLower (head parts) == "alphabet" then
      let alphString = filter (/= '"') $ takeWhile (/= ']') (last parts)
          alphList = divideWith ',' $ tail alphString --removes leading '['
      in
      --trace (show alphList)
      alphList
    else getAlphabet (tail inList)

-- | getBranchLength gets brankch length distribution and parameters
getBranchLength :: [String] -> (Distribution, [DistributionParameter])
getBranchLength inList =
  if null inList then error "No BranchLength specifed in blockModel"
  else
    let first = removeWhiteSpace $ head inList
        parts = divideWith ':' first
    in
    if fmap toLower (head parts) == "branchlength" then
      if length parts /= 3 then error "Incorrect number of arguments in BranchLength (distibution and parameter)"
      else if fmap toLower (parts !! 1) == "uniform" then (Uniform, [read (parts !! 2) :: DistributionParameter])
      else if fmap toLower (parts !! 1) == "exponential" then (Exponential, [read (parts !! 2) :: DistributionParameter])
      else error ("Distibution " ++ (parts !! 1) ++ " is unsupported")
    else getBranchLength (tail inList)

-- | getRateModifiers gets modifiers distribution and parameters
getRateModifiers :: [String] -> [(Modifier, [DistributionParameter])]
getRateModifiers inList =
  if null inList then error "No Rate Modifier specifed in blockModel"
  else
    let first = fmap toLower $ removeWhiteSpace $ head inList
        parts = divideWith ':' first
    in
    --trace ("grm: " ++ show (tail parts)) (
    if head parts == "ratemodifiers" then
      if length parts < 2 then error "Too few arguments in RateModifiers"
      else if (parts !! 1) == "none" then [(None, [])]
      else
        let restMod = tail parts
        in
        if length restMod == 2 then --invariants OR gamma
            if head restMod == "invariant" then [(Invariant, [read (last restMod) :: DistributionParameter])]
            else error ("In getRate Modifier--Rate Modifier " ++ first ++ " is improperly specified\n")
        else if length restMod == 4 then --gamma
            --trace ("LRM4: " ++ (tail (restMod !! 1)) ++ " "++ (takeWhile (/= ',') (restMod !! 2)) ++ " " ++ (tail $ dropWhile (/= ',') (restMod !! 2)) ++ " " ++ (restMod !! 3) ) (
            if head restMod == "gamma" then
              let firstArg = tail (restMod !! 1)
                  thirdArg = tail $ dropWhile (/= ',') (restMod !! 2)
                  secondArg = read (takeWhile (/= ',') (restMod !! 2)) :: DistributionParameter
                  fourthArg = read (takeWhile (/= ')') (restMod !! 3)) :: DistributionParameter
              in
              if firstArg == "classes" && thirdArg == "alpha" then
                --convert classes to Int later
                [(Gamma, [secondArg, fourthArg])]
              else if firstArg == "alpha" && thirdArg == "classes" then
                [(Gamma, [fourthArg, secondArg])]
              else error ("In getRate Modifier--Gamma Rate Modifier: " ++ first ++ " is improperly specified\n")
            else error ("In getRate Modifier--Rate Modifier " ++ first ++ " is improperly specified\n")
            --)
        else if length restMod > 4 then --invariants and gamma
            if head restMod == "invariant" then --invariant first
              let invariantArg = read (takeWhile (/=',') (restMod !! 1)) :: DistributionParameter
                  firstArg = tail (restMod !! 2)
                  thirdArg = tail $ dropWhile (/= ',') (restMod !! 3)
                  secondArg = read (takeWhile (/= ',') (restMod !! 3)) :: DistributionParameter
                  fourthArg = read (takeWhile (/= ')') (restMod !! 4)) :: DistributionParameter
              in
              if firstArg == "classes" && thirdArg == "alpha" then
                --convert classes to Int later
                [(Invariant, [invariantArg]),(Gamma, [secondArg, fourthArg])]
              else if firstArg == "alpha" && thirdArg == "classes" then
                [(Invariant, [invariantArg]),(Gamma, [fourthArg, secondArg])]
              else error ("In getRate Modifier--Gamma Rate Modifier: " ++ first ++ " is improperly specified\n")
            else if head restMod == "gamma"  then --gamma first
              let invariantArg = read (restMod !! 4) :: DistributionParameter
                  firstArg = tail (restMod !! 1)
                  thirdArg = tail $ dropWhile (/= ',') (restMod !! 2)
                  secondArg = read (takeWhile (/= ',') (restMod !! 2)) :: DistributionParameter
                  fourthArg = read (takeWhile (/= ')') (restMod !! 3)) :: DistributionParameter
              in
              if firstArg == "classes" && thirdArg == "alpha" then
                --convert classes to Int later
                [(Invariant, [invariantArg]),(Gamma, [secondArg, fourthArg])]
              else if firstArg == "alpha" && thirdArg == "classes" then
                [(Invariant, [invariantArg]),(Gamma, [fourthArg, secondArg])]
              else error ("In getRate Modifier--Gamma Rate Modifier: " ++ first ++ " is improperly specified\n")
            else error ("In getRate Modifier--Rate Modifier: " ++ head restMod ++ " "++ (restMod !! 1) ++ " " ++ (restMod !! 2) ++ " " ++ (restMod !! 3) ++ " combination is not implemented\n")
              --[(Invariant, [0.9]),(Gamma, [5, 0.9])]
        else error ("In getRate Modifier--Rate Modifier " ++ show restMod)
    else getRateModifiers (tail inList)
    --)

-- | convert2Numbers takes string and converts to numbers only if its first char  is a number
convert2Numbers :: [String] -> [Double]
convert2Numbers inStringList =
  if null inStringList then []
  else
      let inString = head inStringList
      in
      if head inString `notElem` ['0','1','2','3','4','5','6','7','8','9','.',',','-'] then convert2Numbers $ tail inStringList
      else (read inString :: Double) : convert2Numbers (tail inStringList)

-- | getRValues take alphabet size and list of strings and returns 
-- rmatrix values 
getRValues :: Int -> String -> [[Double]]
getRValues alphSize inList =
  if null inList then error "Empty RMatrix"
  else
    let parts = divideWith ',' inList
        numberList = convert2Numbers parts
    in
    --trace  (show $ split2Matrix alphSize numberList) (
    if length numberList /= (alphSize * alphSize) then error "Error--mismatch between alphabet size and R Matrix element number"
    else split2Matrix alphSize numberList
    --)

-- | getPiValues take alphabet size and list of strings and returns 
-- Pi vector values 
getPiValues :: Int -> [String] -> [Double]
getPiValues alphSize parts = -- inList = 
  if null parts then error "Empty PiMatrix"
  else
    let --parts = divideWith ',' inList
        numberList = convert2Numbers parts
    in
    --trace (show numberList) (
    if length numberList /= alphSize then error "Error--mismatch between alphabet size and Pi Vector element number"
    else
      let total = sum numberList
      in
      --normalizes to sum to one
      fmap (/total) numberList
    --)
-- | getPiVector gets teh pi part from list of parameters
getPiVector :: String -> Int -> [String] -> [Double]
getPiVector modelString alphaSize inParts
  | modelString == "k80" = []
  | null inParts = error ("Pi vector not found for " ++ modelString)
  | head inParts == "pivector" = getPiValues alphaSize (take alphaSize (tail inParts))
  | otherwise = getPiVector modelString alphaSize (tail inParts)
    --)

-- | getParam take String of parameter name and returns Double value of parameter
getParam :: String -> [String] -> Double
getParam paramName paramList
  | null paramList = error ("Param " ++ paramName ++ " not found")
  | head paramList == paramName = read (paramList !! 1) :: Double
  | otherwise = getParam paramName (tail paramList)

-- | getNonDiagMatrixSum sums values of non-diaghnonal cells
getNonDiagMatrixSum :: Int -> Int -> Int -> [[Double]] -> Double
getNonDiagMatrixSum alphSize iRow jColumn inMatrix
  | iRow == alphSize = 0
  | jColumn == alphSize = getNonDiagMatrixSum alphSize (iRow + 1) 0 inMatrix
  | iRow == jColumn = getNonDiagMatrixSum alphSize iRow (jColumn + 1) inMatrix
  | otherwise = ((inMatrix !! iRow) !! jColumn) + getNonDiagMatrixSum alphSize iRow (jColumn + 1) inMatrix

-- | log2NormalizedTransitions takes values in GTR R matrix and rtats them as logs certing anew
-- R matrix raising each to base^x and then normalizing so average is 1.0
log2NormalizedTransitions :: Double -> [[Double]] -> [[Double]]
log2NormalizedTransitions base inR =
  if null inR then error "Null input matrix in log2NormalizedTransitions"
  else
    let alphSize = length inR
        numValues = fromIntegral ((alphSize * alphSize) - alphSize)
        negR = fmap (fmap ((-1) *)) inR
        expR = fmap (fmap (base**)) negR
        sumR = getNonDiagMatrixSum alphSize 0 0 expR
        normFactor = numValues / sumR
    in
    --trace (ppMatrix inR ++ "\n" ++ ppMatrix expR ++ "\n" ++ (ppMatrix $ fmap (fmap (* normFactor)) expR))
    fmap (fmap (* normFactor)) expR


-- | getChangeModel gets modifiers distribution and parameters
-- 4-state models adjusted to rates average 1
getChangeModel :: Int -> [String] -> (MarkovModel, QMatrix, PiVector, [ModelParameter])
getChangeModel alphSize inList =
  if null inList then error "No Character Change model specifed in blockModel"
  else
    let first = removeWhiteSpace $ head inList
        parts = divideWith ':' first
        lcParts = fmap toLower $ parts !! 1
    in
    if fmap toLower (head parts) == "changemodel" then
      if length parts < 2 then error "Too few arguments in RateModifiers"
      else if lcParts == "neyman" || lcParts == "jc69" then (Neyman, [[]],[],[]) else
        let rest = (fmap toLower <$> fmap (filter (`notElem` argCruft)) (drop 2 parts)) 
            partsSplit = concatMap (divideWith ',') rest
            piMatrix = getPiVector lcParts alphSize partsSplit --rest
        in
        if lcParts == "k80" then
          let alphaParam = getParam "alpha" partsSplit
              betaParam = getParam "beta" partsSplit
              adjustFactor = 6/(2*alphaParam + 4*betaParam)
          in
          (K80, [[]],[],[alphaParam*adjustFactor, betaParam*adjustFactor])
        else if lcParts == "f81" then
          (F81, [[]],piMatrix,[])
        else if lcParts == "hky85" then
          let alphaParam = getParam "alpha" partsSplit
              betaParam = getParam "beta" partsSplit
              adjustFactor = 6/(2*alphaParam + 4*betaParam)
          in
          (HKY85, [[]],piMatrix,[alphaParam*adjustFactor, betaParam*adjustFactor])
        else if lcParts == "f84" then
          let kappaParam = getParam "kappa" partsSplit
              betaParam = getParam "beta" partsSplit
              adjustFactor = 6/(betaParam * (6 + (kappaParam*(head piMatrix + piMatrix!!2)) + (kappaParam*(piMatrix!!1 + piMatrix!!3))))
          in
          (F84, [[]],piMatrix,[kappaParam, betaParam*adjustFactor])
        else if lcParts == "tn93" then
          let alpha1Param = getParam "alpha1" partsSplit
              alpha2Param = getParam "alpha2" partsSplit
              betaParam = getParam "beta" partsSplit
              adjustFactor = 6/(alpha1Param + alpha2Param + 4*betaParam)
          in
          (TN93, [[]],piMatrix,[alpha1Param*adjustFactor, alpha2Param*adjustFactor, betaParam*adjustFactor])
        else if lcParts == "gtr" then
          if head rest == "rmatrix" then
            let rMatrix =  getRValues alphSize (rest !! 1)
            in
            (GTR, rMatrix,piMatrix,[])
          else -- Pivector first
            let rMatrix =  getRValues alphSize (rest !! 2)
            in
            (GTR, rMatrix,piMatrix,[])
        else if lcParts == "logmatrix" then
          let newPi = replicate alphSize (1/ fromIntegral alphSize)
              baseParam = getParam "base" partsSplit
          in
          if head rest == "rmatrix" then
            let rMatrix =  getRValues alphSize (rest !! 1)
                newRMatrix = log2NormalizedTransitions baseParam rMatrix
            in
            (GTR, newRMatrix,newPi,[])
          else -- Pivector first
            let rMatrix =  getRValues alphSize (rest !! 2)
                newRMatrix = log2NormalizedTransitions baseParam rMatrix
            in
            (GTR, newRMatrix,newPi,[])
        else error ("Change Model " ++ (parts !! 1) ++ " is not yet implemented")
    else getChangeModel alphSize (tail inList)

-- | getPrecision take string list and sees if it starts with precision then parses
getPrecision :: [String] -> Int
getPrecision inList =
  if null inList then error "No precision specifed in blockModel"
  else
    let first = removeWhiteSpace $ head inList
        parts = divideWith ':' first
    in
    if fmap toLower (head parts) == "precision" then
      if length parts /= 2 then error "Incorrect number of arguments in precision, should be 1"
      else (read (last parts) :: Int)
    else getPrecision (tail inList)

-- | getLenfgth take string list and sees if it starts with charLength then parses
getCharLength :: [String] -> Int
getCharLength inList =
  if null inList then error "No character charLength specifed in blockModel"
  else
    let first = removeWhiteSpace $ head inList
        parts = divideWith ':' first
    in
    if fmap toLower (head parts) == "length" then
      if length parts /= 2 then error "Incorrect number of arguments in charLength, should be 1"
      else (read (last parts) :: Int)
    else getCharLength (tail inList)

-- | putGapAtEnd puts alphabets so Gap at and for GTR matrix interpretations
putGapAtEnd :: [String] -> [String]
putGapAtEnd inList =
  if null inList then error "Empty alphabet in putGapAtEnd"
    else
      let indexGap = elemIndex "-" inList
      in
      if isNothing indexGap then inList
      else
        let first = take (fromJust indexGap) inList
            second = drop (1 + fromJust indexGap) inList
        in
        first ++ second ++ ["-"]

-- | getBlockParams takes strings opf args and retruns tuples of params
getBlockParams :: [String] -> ([String], (Distribution, [DistributionParameter]), [(Modifier, [DistributionParameter])], (MarkovModel, QMatrix, PiVector, [ModelParameter]), Int, Int)
getBlockParams inStringList =
  let alphabetLocal = putGapAtEnd (nub $ getAlphabet inStringList) --Puts -'- last if in alphabetLocal'
      branchLengthLocal = getBranchLength inStringList --(Uniform,[])
      rateModifiersLocal = getRateModifiers inStringList --[(None,[])] 
      changeModelLocal = getChangeModel (length alphabetLocal) inStringList --(Neyman, [[]], [])
      precisionLocal = getPrecision inStringList
      charLengthLocal = getCharLength inStringList
  in
  if head alphabetLocal /= "-" then (alphabetLocal, branchLengthLocal, rateModifiersLocal, changeModelLocal, precisionLocal, charLengthLocal)
  else
  (tail alphabetLocal ++ ["-"], branchLengthLocal, rateModifiersLocal, changeModelLocal, precisionLocal, charLengthLocal)


-- | parseCharModel takes character model string and parses
parseCharModel :: [(String, Int)] -> [String] -> [(CharacterModel, Int)]
parseCharModel blockNameList inStringList
  | null inStringList = []
  | null blockNameList = error "No block models specified.  Must have at least 1"
  | otherwise =
      let inString = head inStringList
          bmName = last $ words $ takeWhile (/= '{') inString
          guts = removeWhiteSpace $ takeWhile (/= '}') $ tail $ dropWhile (/= '{') inString
          pieces = divideWith ';' guts
          numChars = getBlock bmName blockNameList
      in
      if numChars > 0 then
        let (cAlphabet, cBranchLength, cRateModifiers, cChangeModel,cPrecision, cLength) = getBlockParams pieces
            thisCharModel = CharacterModel { characterName = bmName
                              , alphabet = cAlphabet
                              , branchLength = cBranchLength
                              , rateModifiers = cRateModifiers
                              , changeModel = cChangeModel
                              , precision = cPrecision
                              , charLength = cLength
                              }
        in
        (thisCharModel, numChars) : parseCharModel blockNameList (tail inStringList)
      else
        parseCharModel blockNameList (tail inStringList)

-- | getMachineString takes section string and returns the one with machine in it
getElementString :: String -> [String]-> [String]
getElementString element inStringList =
  if null inStringList then []
  else
    let firstString = head inStringList
    in
    if fmap toLower (head $ words firstString) == element then firstString : getElementString element (tail inStringList)
    else getElementString element (tail inStringList)

-- | getCharModelByName takes String and pull charModel by name
getCharModelByName :: String -> [(CharacterModel, Int)] -> (CharacterModel, Int)
getCharModelByName inName inCharModelList =
  if null inCharModelList then error ("Char model " ++ inName ++ " not found")
  else
    let (firstChar, number) = head inCharModelList
        firstName = characterName firstChar
    in
    if inName == firstName then (firstChar, number)
    else getCharModelByName inName (tail inCharModelList)

-- | reorderCharacterModels reorders model list to machine specification
reorderCharacterModels :: [(CharacterModel, Int)] -> [(String, Int)] -> [(CharacterModel, Int)]
reorderCharacterModels inCharModelList inBlockPairList =
  if null inBlockPairList then []
  else
    let (name, _) = head inBlockPairList
        (charModel, number) = getCharModelByName name inCharModelList
    in
    (charModel, number) : reorderCharacterModels inCharModelList (tail inBlockPairList)

-- | parseSections parses sections according to type
parseSections :: [String] -> MachineModel
parseSections inSectionList =
  if null inSectionList then error "Empty list of machine sections"
  else
        --Find and pull Machine
    let machineModels = parseMachine Control.Applicative.<$> getElementString "machine" inSectionList
    in
    if length machineModels > 1 then error "Can only specify a single machine model"
    else
        --Find and pull Graph
      let (gMachineName, graphNameLocal, blockModels) = head machineModels
          graphModels = parseGraph graphNameLocal $ getElementString "graph" inSectionList
        --Find and pull CharModel(s) 
          cCharModelList = parseCharModel blockModels $ getElementString "blockmodel" inSectionList
          reorderedCharacterModelList = reorderCharacterModels cCharModelList blockModels
      in
      if null graphModels then error "No graph specification in machine file--or not matching name"
      else if length graphModels > 1 then error ("Can only have a single graph specification in machine file.  There are " ++ show (length graphModels))
      else if null reorderedCharacterModelList then error "Need at least one character model"
      else
        --IN HERE for finding parsed graph and models
      --(machineModel, graphModel, characterModelList)
        let thisMachineModel = MachineModel {machineName = gMachineName, graphSpecification = head graphModels, characterModelList = reorderedCharacterModelList}
        in
        thisMachineModel


-- | parseMachineFile takes a file name as String and returns the machine model aspects
parseMachineFile :: String -> MachineModel
parseMachineFile fileContents =
  let filteredContents = removeComments (lines fileContents)
      sections = strip Control.Applicative.<$> getSections filteredContents
      parsedSections = parseSections sections
  in
  --trace (show sections ++ show parsedSections)
  parsedSections
