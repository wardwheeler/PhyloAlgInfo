{- |
Module      :  Parser for Kolmogorov (algorithmic) complexity 'Machine' definition
Description :  Program readMaybes input file with description and parameters for
               machine containing models for graphs and character models
Copyright   :  (c) 2018-2023 Ward C. Wheeler, Division of Invertebrate Zoology, AMNH. All rights reserved.
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

import           Complexity.PhyloParsers
import           Complexity.Types
import           Complexity.Utilities
import           Data.Char
import qualified Data.Graph.Inductive.Graph        as G
import qualified Data.Graph.Inductive.PatriciaTree as P
import           Data.List
import           Data.Maybe
import           Data.String.Utils
import qualified Data.Text.Lazy                    as T
import           Text.Read
import           Debug.Trace



-- | ArgCrust are charcaterts in arguments that may need to be filtered
argCruft :: String
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

-- | split with takes  a string and splits on a character returning a list of strings
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
      let midVal = readMaybe (first !! 2 ) :: Maybe Int
      in 
      if isNothing midVal then errorWithoutStackTrace ("Error parsine machine element: " <> (first !! 2 ))
      else ("block",  first !! 1, (fromJust midVal)) : getMachineElements (tail inStringList)
    else errorWithoutStackTrace ("Unrecognized machine element " ++ head first)


-- | getGraphName extracts graph from elements list
getGraphName :: [(String, String, Int)] -> String
getGraphName elementList =
  if null elementList then errorWithoutStackTrace "Graph specification not found"
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
  if null inString then errorWithoutStackTrace "No machine specification in input file"
  else
    let name = last $ words $ takeWhile (/= '{') inString
        guts = removeWhiteSpace $ takeWhile (/= '}') $ tail $ dropWhile (/= '{') inString
        pieces = divideWith ';' guts
        machineElements =  getMachineElements pieces
    in
    let graphNameLocal = getGraphName machineElements
        blockPairs = getBlockName machineElements
    in
    (name, graphNameLocal, blockPairs)


-- | getValue looks through contents of graph specification and
-- pulls parts matching in string throuws error if not found
getValue :: String -> [String] -> String
getValue findHere guts =
  if null guts then
    if findHere == "representation" then "No representation input"
    else errorWithoutStackTrace ("No parameter " ++ findHere ++ " specified in graph")
  else
    let firstGut = toLower <$> head guts
        parts = divideWith ':' firstGut
    in
    if length parts /=2 then errorWithoutStackTrace ("Incorrect number of parameters " ++ show (length parts) ++ " in " ++ findHere ++ " in graphModel\n Should be 1.")
    else if findHere == head parts then last parts -- (readMaybe (last parts) :: Int)
    else getValue findHere (tail guts)

-- | parseGraph takes graph string and parses
parseGraph :: String -> [String] -> [GraphModel]
parseGraph graphNameLocal inStringList=
  if null inStringList then []
  else
    let inString = head inStringList
        gName = last $ words $ takeWhile (/= '{') inString
        guts = removeWhiteSpace $ takeWhile (/= '}') $ tail $ dropWhile (/= '{') inString
        pieces = divideWith ';' guts
        graphRepresentation = getValue "representation" pieces
    in
    -- values specified
    if graphNameLocal == gName then
      if graphRepresentation == "No representation input" then
        let nLeaves = readMaybe (getValue "leaves" pieces) :: Maybe Int
            nRoots = readMaybe (getValue "roots" pieces) :: Maybe Int
            nSingletons = readMaybe (getValue "singletons" pieces) :: Maybe Int
            nNetworkEdges = readMaybe (getValue "networkedges" pieces) :: Maybe Int
            theGraphModel = GraphModel {graphName = gName, numLeaves = fromJust nLeaves, numRoots = fromJust nRoots, numSingletons = fromJust nSingletons, numNetworkEdges = fromJust nNetworkEdges}
        in
        if isNothing nLeaves then errorWithoutStackTrace ("Error specifying number leaves: " <> (getValue "leaves" pieces))
        else if isNothing nRoots then errorWithoutStackTrace ("Error specifying number roots: " <> (getValue "rootPairList" pieces))
        else if isNothing nSingletons then errorWithoutStackTrace ("Error specifying number singleton leaves: " <> (getValue "singletons" pieces))
        else if isNothing nNetworkEdges then errorWithoutStackTrace ("Error specifying number network edges: " <> (getValue "networkedges" pieces))
        else theGraphModel : parseGraph graphNameLocal (tail inStringList)
      -- get values from graph representation
      -- assumes (for now) Forest Enhanced Newick format
      else
        let graphString = reverse $ takeWhile (/=':') $ drop 1 $ reverse guts
        in
        if (head graphString /= '"') || (last graphString /= '"') then
          errorWithoutStackTrace ("Graph representation must be in double quotes: " ++ graphString)
        else -- only processes first graph if more than one
          let fglGraph = head $ forestEnhancedNewickStringList2FGLList (T.tail $ T.init $ T.pack graphString)
              (nLeaves, nRoots, nSingletons, nNetworkEdges) = getGraphAspects fglGraph
              theGraphModel = GraphModel {graphName = gName, numLeaves = nLeaves, numRoots = nRoots, numSingletons = nSingletons, numNetworkEdges = nNetworkEdges}
          in
          theGraphModel : parseGraph graphNameLocal (tail inStringList)
    else parseGraph graphNameLocal (tail inStringList)

-- | getGraphAspects takes an fgl graph and reurns 4-tuple of numLeaves, numRoots, numSingleton nodes, and numNetworkEdges
getGraphAspects :: P.Gr T.Text Double -> (Int, Int, Int, Int)
getGraphAspects inGraph =
  if G.isEmpty inGraph then (0,0,0,0)
  else
    let nodeSet = G.nodes inGraph
        nodeDegreeList = fmap (G.deg inGraph) nodeSet
        nodeDegreePairList = zip nodeDegreeList nodeSet
        singletonNodePairList = filter ((== 0).fst) nodeDegreePairList
        nodeInDegreeList = fmap (G.indeg inGraph) nodeSet
        nodeInDegreePairList = zip nodeInDegreeList nodeSet
        nodeOutDegreeList = fmap (G.outdeg inGraph) nodeSet
        nodeOutDegreePairList = zip nodeOutDegreeList nodeSet
        rootPairList = filter ((== 0).fst) nodeInDegreePairList
        leafPairList = filter ((== 0).fst) nodeOutDegreePairList
        networkNodePairList = filter ((> 1).fst) nodeInDegreePairList
        numberNetworkEdges = sum $ fmap fst networkNodePairList
    in
    (length leafPairList, length rootPairList, length singletonNodePairList, numberNetworkEdges)



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
  if null inList then errorWithoutStackTrace "No alphabet specifed in blockModel"
  else
    let first = removeWhiteSpace $ head inList
        parts = divideWith ':' first
    in
    if fmap toLower (head parts) == "alphabet" then
      let alphString = filter (/= '"') $ takeWhile (/= ']') (last parts)
          alphList = divideWith ',' $ tail alphString --removes leading '['
      in
      alphList
    else getAlphabet (tail inList)

-- | getBranchLength gets brankch length distribution and parameters
getBranchLength :: [String] -> (Distribution, [DistributionParameter])
getBranchLength inList =
  if null inList then errorWithoutStackTrace "No BranchLength specifed in blockModel"
  else
    let first = removeWhiteSpace $ head inList
        parts = divideWith ':' first
        val = readMaybe (parts !! 2) :: Maybe DistributionParameter
    in
    if fmap toLower (head parts) == "branchlength" then
      if length parts /= 3 then errorWithoutStackTrace "Error: Incorrect number of arguments in BranchLength (distibution and parameter)"
      else if isNothing val then errorWithoutStackTrace ("Error specifying branch length: " <> (parts !! 2))
      else if fmap toLower (parts !! 1) == "uniform" then (Uniform, [fromJust val])
      else if fmap toLower (parts !! 1) == "exponential" then (Exponential, [fromJust val])
      else errorWithoutStackTrace ("Distibution " ++ (parts !! 1) ++ " is unsupported")
    else getBranchLength (tail inList)

-- | getRateModifiers gets modifiers distribution and parameters
getRateModifiers :: [String] -> [(Modifier, [DistributionParameter])]
getRateModifiers inList =
  if null inList then errorWithoutStackTrace "No Rate Modifier specifed in blockModel"
  else
    let first = fmap toLower $ removeWhiteSpace $ head inList
        parts = divideWith ':' first
    in
    if head parts == "ratemodifiers" then
      if length parts < 2 then errorWithoutStackTrace "Too few arguments in RateModifiers"
      else if (parts !! 1) == "none" then [(None, [])]
      else
        let restMod = tail parts
        in
        if length restMod == 2 then --invariants OR gamma
            if head restMod == "invariant" then 
              let val = readMaybe (last restMod) :: Maybe DistributionParameter
              in
              if isNothing val then errorWithoutStackTrace ("Error specifying invariant paramter: " <> (last restMod)) 
              else [(Invariant, [fromJust val])]

            else errorWithoutStackTrace ("In getRate Modifier--Rate Modifier " ++ first ++ " is improperly specified\n")
        else if length restMod == 4 then --gamma
            if head restMod == "gamma" then
              let firstArg = tail (restMod !! 1)
                  thirdArg = tail $ dropWhile (/= ',') (restMod !! 2)
                  secondArg = readMaybe (takeWhile (/= ',') (restMod !! 2)) :: Maybe DistributionParameter
                  fourthArg = readMaybe (takeWhile (/= ')') (restMod !! 3)) :: Maybe DistributionParameter
              in
              if firstArg == "classes" && thirdArg == "alpha" then
                --convert classes to Int later
                [(Gamma, [fromJust secondArg, fromJust fourthArg])]
              else if firstArg == "alpha" && thirdArg == "classes" then
                [(Gamma, [fromJust fourthArg, fromJust secondArg])]
              else errorWithoutStackTrace ("In getRate Modifier--Gamma Rate Modifier: " ++ first ++ " is improperly specified\n")
            else errorWithoutStackTrace ("In getRate Modifier--Rate Modifier " ++ first ++ " is improperly specified\n")

        else if length restMod > 4 then --invariants and gamma
            if head restMod == "invariant" then --invariant first
              let invariantArg = readMaybe (takeWhile (/=',') (restMod !! 1)) :: Maybe DistributionParameter
                  firstArg = tail (restMod !! 2)
                  thirdArg = tail $ dropWhile (/= ',') (restMod !! 3)
                  secondArg = readMaybe (takeWhile (/= ',') (restMod !! 3)) :: Maybe DistributionParameter
                  fourthArg = readMaybe (takeWhile (/= ')') (restMod !! 4)) :: Maybe DistributionParameter
              in
              if isNothing invariantArg then errorWithoutStackTrace ("Error parsing invariant parameter: " <> (restMod !! 4))
              else if isNothing secondArg then errorWithoutStackTrace ("Error processing parameter: " <> (takeWhile (/= ',') (restMod !! 2)))
              else if isNothing fourthArg then errorWithoutStackTrace ("Error processing parameter: " <> (takeWhile (/= ')') (restMod !! 3)))
              else if firstArg == "classes" && thirdArg == "alpha" then
                --convert classes to Int later
                [(Invariant, [fromJust invariantArg]),(Gamma, [fromJust secondArg, fromJust fourthArg])]
              else if firstArg == "alpha" && thirdArg == "classes" then
                [(Invariant, [fromJust invariantArg]),(Gamma, [fromJust fourthArg, fromJust secondArg])]
              else errorWithoutStackTrace ("In getRate Modifier--Gamma Rate Modifier: " ++ first ++ " is improperly specified\n")
            else if head restMod == "gamma"  then --gamma first
              let invariantArg = readMaybe (restMod !! 4) :: Maybe DistributionParameter
                  firstArg = tail (restMod !! 1)
                  thirdArg = tail $ dropWhile (/= ',') (restMod !! 2)
                  secondArg = readMaybe (takeWhile (/= ',') (restMod !! 2)) :: Maybe DistributionParameter
                  fourthArg = readMaybe (takeWhile (/= ')') (restMod !! 3)) :: Maybe DistributionParameter
              in
              if isNothing invariantArg then errorWithoutStackTrace ("Error parsing invariant parameter: " <> (restMod !! 4))
              else if isNothing secondArg then errorWithoutStackTrace ("Error processing gamma parameter: " <> (takeWhile (/= ',') (restMod !! 2)))
              else if isNothing fourthArg then errorWithoutStackTrace ("Error processing gamma parameter: " <> (takeWhile (/= ')') (restMod !! 3)))
              else if firstArg == "classes" && thirdArg == "alpha" then
                --convert classes to Int later
                [(Invariant, [fromJust invariantArg]),(Gamma, [fromJust secondArg, fromJust fourthArg])]
              else if firstArg == "alpha" && thirdArg == "classes" then
                [(Invariant, [fromJust invariantArg]),(Gamma, [fromJust fourthArg, fromJust secondArg])]
              else errorWithoutStackTrace ("In getRate Modifier--Gamma Rate Modifier: " ++ first ++ " is improperly specified\n")
            else errorWithoutStackTrace ("In getRate Modifier--Rate Modifier: " ++ head restMod ++ " "++ (restMod !! 1) ++ " " ++ (restMod !! 2) ++ " " ++ (restMod !! 3) ++ " combination is not implemented\n")
              --[(Invariant, [0.9]),(Gamma, [5, 0.9])]
        else errorWithoutStackTrace ("In getRate Modifier--Rate Modifier " ++ show restMod)
    else getRateModifiers (tail inList)
    --)

-- | convert2Numbers takes string and converts to numbers only if its first char is a number
convert2Numbers :: [String] -> [Double]
convert2Numbers inStringList =
  if null inStringList then []
  else
      let inString = head inStringList
      in
      if head inString `notElem` ['0','1','2','3','4','5','6','7','8','9','.',',','-'] then convert2Numbers $ tail inStringList
      else 
        let val = readMaybe inString :: Maybe Double
        in
        if isNothing val then errorWithoutStackTrace ("Error parsing input as double value: " <> inString)
        else (fromJust val) : convert2Numbers (tail inStringList)

-- | getRValues take alphabet size and list of strings and returns
-- rmatrix values
getRValues :: Int -> String -> [[Double]]
getRValues alphSize inList =
  if null inList then errorWithoutStackTrace "Empty RMatrix"
  else
    let parts = divideWith ',' inList
        numberList = convert2Numbers parts
    in
    --trace  (show $ split2Matrix alphSize numberList) (
    if length numberList /= (alphSize * alphSize) then errorWithoutStackTrace "Error--mismatch between alphabet size and R Matrix element number"
    else split2Matrix alphSize numberList
    --)

-- | getPiValues take alphabet size and list of strings and returns
-- Pi vector values
getPiValues :: Int -> [String] -> [Double]
getPiValues alphSize parts = -- inList =
  if null parts then errorWithoutStackTrace "Empty PiMatrix"
  else
    let numberList = convert2Numbers parts
    in
    if length numberList /= alphSize then errorWithoutStackTrace "Error--mismatch between alphabet size and Pi Vector element number"
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
  | null inParts = errorWithoutStackTrace ("Pi vector not found for " ++ modelString)
  | head inParts == "pivector" = getPiValues alphaSize (take alphaSize (tail inParts))
  | otherwise = getPiVector modelString alphaSize (tail inParts)


-- | getParam take String of parameter name and returns Double value of parameter
getParam :: String -> [String] -> Double
getParam paramName paramList
  | null paramList = errorWithoutStackTrace ("Param " ++ paramName ++ " not found")
  | head paramList == paramName = 
      let val = readMaybe (paramList !! 1) :: Maybe Double
      in
      if isNothing val then errorWithoutStackTrace ("Error processing parameter value as Double: " <> (paramList !! 1))
      else fromJust val
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
  if null inR then errorWithoutStackTrace "Null input matrix in log2NormalizedTransitions"
  else
    let alphSize = length inR
        numValues = fromIntegral ((alphSize * alphSize) - alphSize)
        negR = fmap (fmap ((-1) *)) inR
        expR = fmap (fmap (base**)) negR
        sumR = getNonDiagMatrixSum alphSize 0 0 expR
        normFactor = numValues / sumR
    in
    fmap (fmap (* normFactor)) expR


-- | getChangeModel gets modifiers distribution and parameters
-- 4-state models adjusted to rates average 1
getChangeModel :: Int -> [String] -> (MarkovModel, QMatrix, PiVector, [ModelParameter])
getChangeModel alphSize inList =
  if null inList then errorWithoutStackTrace "No Character Change model specifed in blockModel"
  else
    let first = removeWhiteSpace $ head inList
        parts = divideWith ':' first
        lcParts = fmap toLower $ parts !! 1
    in
    if fmap toLower (head parts) == "changemodel" then
      if length parts < 2 then errorWithoutStackTrace "Too few arguments in RateModifiers"
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
        else errorWithoutStackTrace ("Change Model " ++ (parts !! 1) ++ " is not yet implemented")
    else getChangeModel alphSize (tail inList)

-- | getPrecision take string list and sees if it starts with precision then parses
getPrecision :: [String] -> Int
getPrecision inList =
  if null inList then errorWithoutStackTrace "No precision specifed in blockModel"
  else
    let first = removeWhiteSpace $ head inList
        parts = divideWith ':' first
    in
    if fmap toLower (head parts) == "precision" then
      if length parts /= 2 then errorWithoutStackTrace "Incorrect number of arguments in precision, should be 1"
      else 
        let val = (readMaybe (last parts) :: Maybe Int)
        in
        if isNothing val then errorWithoutStackTrace  ("Error processing precsion value as Int: " <> (last parts))
        else fromJust val
    else getPrecision (tail inList)

-- | getLenfgth take string list and sees if it starts with charLength then parses
getCharLength :: [String] -> Int
getCharLength inList =
  if null inList then errorWithoutStackTrace "No character charLength specifed in blockModel"
  else
    let first = removeWhiteSpace $ head inList
        parts = divideWith ':' first
    in
    if fmap toLower (head parts) == "length" then
      if length parts /= 2 then errorWithoutStackTrace "Incorrect number of arguments in charLength, should be 1"
      else 
        let val = (readMaybe (last parts) :: Maybe Int)
        in
        if isNothing val then errorWithoutStackTrace ("Error processing length value as Int: " <> (last parts))
        else fromJust val
    else getCharLength (tail inList)

-- | putGapAtEnd puts alphabets so Gap at and for GTR matrix interpretations
putGapAtEnd :: [String] -> [String]
putGapAtEnd inList =
  if null inList then errorWithoutStackTrace "Empty alphabet in putGapAtEnd"
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
  | null blockNameList = errorWithoutStackTrace "No block models specified.  Must have at least 1"
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
  if null inCharModelList then errorWithoutStackTrace ("Char model " ++ inName ++ " not found")
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
    let machineModels = parseMachine <$> getElementString "machine" inSectionList
    in
    if length machineModels > 1 then errorWithoutStackTrace "Can only specify a single machine model"
    else
        --Find and pull Graph
      let (gMachineName, graphNameLocal, blockModels) = head machineModels
          graphModels = parseGraph graphNameLocal $ getElementString "graph" inSectionList
        --Find and pull CharModel(s)
          cCharModelList = parseCharModel blockModels $ getElementString "blockmodel" inSectionList
          reorderedCharacterModelList = reorderCharacterModels cCharModelList blockModels
      in
      if null graphModels then errorWithoutStackTrace "No graph specification in machine file--or not matching name"
      else if length graphModels > 1 then errorWithoutStackTrace ("Can only have a single graph specification in machine file.  There are " ++ show (length graphModels))
      else if null reorderedCharacterModelList then errorWithoutStackTrace "Need at least one character model"
      else
        --IN HERE for finding parsed graph and models
        let thisMachineModel = MachineModel {machineName = gMachineName, graphSpecification = head graphModels, characterModelList = reorderedCharacterModelList}
        in
        thisMachineModel

-- | fst4 returns the first element of a 4-tuple
fst4 :: (a,b,c,d) -> a
fst4 (f, _, _ ,_ ) = f

-- | makeSimpleR takes an alphabet size and returns 0 diag and 1 non-diag square matrix
makeSimpleR :: Int -> Int -> [[Double]]
makeSimpleR rowIndex alphabetSize =
  if rowIndex == alphabetSize then []
  else
    let newRow = makeRow 0 rowIndex alphabetSize
    in
    newRow : makeSimpleR (rowIndex + 1) alphabetSize

-- | makeRow make a row where index = 0 and all others 1
makeRow :: Int -> Int -> Int -> [Double]
makeRow colCounter rowNum alphabetSize
  | colCounter == alphabetSize = []
  | colCounter == rowNum = (0 :: Double) : makeRow (colCounter + 1) rowNum alphabetSize
  | otherwise = (1 :: Double) : makeRow (colCounter + 1) rowNum alphabetSize

-- | checkK80F81BranchDistributions takes a list of charcater models and checks teh K80 and F81
-- models for their distributions. Tis is to see if there is a benefit of convverting them to KY85 models
-- in terms of complexity.  If their branch distibutins are all the same, or overlapping if there are multiple
-- models with multiple branch length distributions.  The basic idea is that if the K80 and F81 models have non-overlapping
-- branch distributions, then converting them both to HKY85 will not reduce complexity, but increase it
-- since they will not have shared generated code, but non-shared more complex code then they would have had 
-- otherwise.
checkK80F81BranchDistributions :: [Distribution] -> [Distribution] -> [CharacterModel] -> Bool
checkK80F81BranchDistributions k80List f81List modelList = 
  if null modelList then not $ null (intersect (nub k80List) (nub f81List))
  else 
    let firstModel = fst4 $ changeModel $ head modelList
        firstDist = fst $ branchLength $ head modelList
    in
    if firstModel == K80 then checkK80F81BranchDistributions (firstDist : k80List) f81List (tail modelList)
    else if firstModel == F81 then checkK80F81BranchDistributions k80List (firstDist : f81List) (tail modelList)
    else checkK80F81BranchDistributions k80List f81List (tail modelList)

-- | parseMachineFile takes a file name as String and returns the machine model aspects
--  optimizeModels here to reduce number of different models (ie GTR and Neyman to just 2x GTR with diff parameters)
parseMachineFile :: Bool -> String -> (MachineModel, MachineModel)
parseMachineFile optimizeModels fileContents =
  let filteredContents = removeComments (lines fileContents)
      sections = strip <$> getSections filteredContents
      parsedSections = parseSections sections
  in
  if not optimizeModels then (parsedSections, parsedSections)
  else
    let inModelList = nub $ fmap (fst4 . changeModel) $ fmap fst $ characterModelList parsedSections
        --Optimization for special case of K80 and F81 both present hence converted to HKY85 so single model (JC69 also converted)
        moreComplexModelsNotPresent = (GTR `notElem` inModelList) && (LOGMATRIX `notElem` inModelList) && (TN93 `notElem` inModelList) && (HKY85 `notElem` inModelList) && (F84 `notElem` inModelList)
        k80F81Present = (K80 `elem` inModelList) && (F81 `elem` inModelList)
        k80F81BranchDistributionsFactor = checkK80F81BranchDistributions [] [] (fmap fst $ characterModelList parsedSections)
        additionallModels = if (moreComplexModelsNotPresent && k80F81Present && k80F81BranchDistributionsFactor) then [HKY85] else []
        newModels = reduceModels (additionallModels ++ inModelList) $ characterModelList parsedSections
        optmizedSections = MachineModel {machineName = machineName parsedSections, graphSpecification = graphSpecification parsedSections, characterModelList = newModels}
    in
    (optmizedSections, parsedSections)

-- | reduceModels tkaes model specification and tries to reduce the number of
-- different Markov Models by using more genreal models if alreadMaybey specified.
-- e.g subsitution a GTR with all params the same-> no need for Neyman code
-- or more complex and less comples 4-state DNA models
reduceModels :: [MarkovModel] -> [(CharacterModel, Int)] -> [(CharacterModel, Int)]
reduceModels markovPresent inModelList
  | null markovPresent = errorWithoutStackTrace "No markov models specified in reduceModels"
  | length markovPresent == 1 = inModelList
  | null inModelList = []
  | otherwise =
    let (firstCharModel, numChar) = head inModelList
        fName = characterName firstCharModel
        fAlph = alphabet firstCharModel
        fBL = branchLength firstCharModel
        rMod = rateModifiers firstCharModel
        inModel = changeModel firstCharModel
        fPrec = precision firstCharModel
        fCharLength = charLength firstCharModel
    in
    let newChangeModel = findExistingModel markovPresent (length fAlph) inModel
        newModel = CharacterModel {characterName = fName, alphabet = fAlph, branchLength = fBL, rateModifiers = rMod, changeModel = newChangeModel, precision = fPrec, charLength = fCharLength}
    in
    (newModel, numChar) : reduceModels  markovPresent (tail inModelList)
    -- )

-- | findExistingModel markovPresent oldModel
-- F84 and HKY85 same but alternate parameterization
--What about 2 models not GTR--is GTR better?  maybe check experimentally
findExistingModel :: [MarkovModel] -> Int -> (MarkovModel, RMatrix, PiVector, [ModelParameter]) -> (MarkovModel, RMatrix, PiVector, [ModelParameter])
findExistingModel markovPresent alphabetSize inModel@(modelType, _, _, _)
  | modelType == GTR = inModel
  | modelType == LOGMATRIX = inModel
  | GTR `elem` markovPresent || LOGMATRIX `elem` markovPresent = -- Neyman and all 4-states can be converted to GTR
    transformModel alphabetSize inModel GTR
      | alphabetSize /= 4 = inModel
      | TN93 `elem` markovPresent = -- F84, HKY85, F81, K80, JC69 models can be converted to TN93
    transformModel alphabetSize inModel TN93
      | HKY85 `elem` markovPresent = -- F84, F81, K80, JC69 models can be converted to HKY85
    transformModel alphabetSize inModel HKY85
      | F84 `elem` markovPresent = -- F81, K80, JC69 models can be converted to F84
    transformModel alphabetSize inModel F84
      | K80 `elem` markovPresent = -- JC69 only to K80
    transformModel alphabetSize inModel K80
      | F81 `elem` markovPresent = -- JC69 only to F81
    transformModel alphabetSize inModel F81

-- | transformModel take an alphabet size and model and retusn the r and pi matrices implied by that model
-- e.g. Newyman to all equal
-- can't transforma complex model into a simpler one (e.g. TN93 to K80)--so then it just returns the input model
transformModel :: Int -> (MarkovModel, RMatrix, PiVector, [ModelParameter]) -> MarkovModel -> (MarkovModel, RMatrix, PiVector, [ModelParameter])
transformModel alphabetSize inModel@(modelType, _, piVector, paramList) targetModel =
  if modelType == targetModel then inModel
  else
    trace ("Transforming model " ++ show modelType ++ " to " ++ show targetModel) (
    let equalPiVect = replicate alphabetSize (1.0 / fromIntegral alphabetSize)
        equalRMatrix = makeSimpleR 0 alphabetSize
        pA = head piVector
        pC = piVector !! 1
        pG = piVector !! 2
        pT = piVector !! 3
    in
    -- transform model from input modelType to targetModel
    if (modelType == Neyman) || (modelType == JC69) then
      if targetModel == GTR then
        (GTR, equalRMatrix, equalPiVect, [])
      else if targetModel == TN93 then
        (TN93, [[]], equalPiVect, [1,1,1])
      else if targetModel == HKY85 then
        (HKY85, [[]], equalPiVect, [1,1])
      else if targetModel == F84 then
        (F84, [[]], equalPiVect, [1,1])
      else if targetModel == K80 then
        (K80, [[]], equalPiVect, [1,1])
      else if targetModel == F81 then
        (F81, [[]], equalPiVect, [])
      else errorWithoutStackTrace ("Optimization target model " ++ show modelType ++ " not implemented")

    else if modelType == TN93 then
      let a1 = head paramList
          a2 = paramList !! 1
          b  = paramList !! 2
      in
      if targetModel == GTR then
        (GTR, [[0, b, a2, b],[b, 0, b, a1],[a2 , b, 0, b],[b, a1, b, 0]], piVector, [])
      else if targetModel == TN93 then
        (TN93, [[]], piVector, [a1,a2,b]) -- keeping identit models for legibility
      else inModel -- error (show modelType ++ " can't be converted into " ++ show targetModel)

    else if modelType == HKY85 then
      let a1 = head paramList
          a2 = head paramList
          a  = head paramList
          b  = paramList !! 1
      in
      if targetModel == GTR then
        (GTR, [[0, b, a2, b],[b, 0, b, a1],[a2 , b, 0, b],[b, a1, b, 0]], piVector, [])
      else if targetModel == TN93 then
        (TN93, [[]], piVector, [a1,a2,b]) -- keeping identit models for legibility
      else if targetModel == HKY85 then
        (HKY85, [[]], piVector, [a,b])
      else inModel --error (show modelType ++ " can't be converted into " ++ show targetModel)

    else if modelType == F84 then
      let k  = head paramList
          b  = paramList !! 1
          a1 = (1 +(k / (pC + pT))) * b
          a2 = (1 +(k / (pA + pG))) * b
          a  = (a1 + a2) / 2
      in
      if targetModel == GTR then
        (GTR, [[0, b, a2, b],[b, 0, b, a1],[a2 , b, 0, b],[b, a1, b, 0]], piVector, [])
      else if targetModel == TN93 then
        (TN93, [[]], piVector, [a1,a2,b]) -- keeping identit models for legibility
      else if targetModel == HKY85 then
        (HKY85, [[]], piVector, [a,b])
      else if targetModel == F84 then
        (F84, [[]], piVector, paramList)
      else inModel

    else if modelType == K80 then
      let a1 = head paramList
          a2 = head paramList
          a  = head paramList
          b  = paramList !! 1
          k1 = ((a1/b) - 1) * 0.5
          k2 = ((a2/b) - 1) * 0.5
          k  = (k1 + k2) / 2
      in
      if targetModel == GTR then
        (GTR, [[0, b, a2, b],[b, 0, b, a1],[a2 , b, 0, b],[b, a1, b, 0]], equalPiVect, [])
      else if targetModel == TN93 then
        (TN93, [[]], equalPiVect, [a1,a2,b]) -- keeping identit models for legibility
      else if targetModel == HKY85 then
        (HKY85, [[]], equalPiVect, [a,b])
      else if targetModel == F84 then
        (F84, [[]], equalPiVect, [k,b])
      else if targetModel == K80 then
        (K80, [[]], equalPiVect, [a,b])
      else inModel -- error (show modelType ++ " can't be converted into " ++ show targetModel)
      else if targetModel == F81 then
        inModel -- error ("Optimization target model " ++ show modelType ++ " not implemented")

    else if modelType == F81 then
      let a1 = 1
          a2 = 1
          a  = 1
          b  = 1
          k1 = ((a1/b) - 1) * (pC + pT)
          k2 = ((a2/b) - 1) * (pA + pG)
          k  = (k1 + k2) / 2
      in
      if targetModel == GTR then
        (GTR, equalRMatrix, piVector, [])
      else if targetModel == TN93 then
        (TN93, [[]], piVector, [a1,a2,b]) -- keeping identit models for legibility
      else if targetModel == HKY85 then
        (HKY85, [[]], piVector, [a,b])
      else if targetModel == F84 then
        (F84, [[]], piVector, [k,b])
      else if targetModel == K80 then
        inModel -- error (show modelType ++ " can't be converted into " ++ show targetModel)
      else if targetModel == F81 then
        (F81, [[]], piVector, [])
      else errorWithoutStackTrace ("Optimization target model " ++ show modelType ++ " not implemented")

  else errorWithoutStackTrace ("Optimization input model " ++ show modelType ++ " not implemented")
  )
