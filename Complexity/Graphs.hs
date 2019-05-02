{- |
Module      :  Graphs 
Description :  Functions to generate(algorithmic) complexity of graphs
               Generates a Haskell program (compiles) 
				       with a description of a graph. The output program can be executed with 
				       GHCi interpreter. ghci --:load progName
               also oputputs Huffman binary code of program
Copyright   :  (c) 2018-2019 Ward C. Wheeler, Division of Invertebrate Zoology, AMNH. All rights reserved.
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

{- |E| = |L| + (|L|-2) - 2(|R|-1) + 3|N| + |S|
       = 2|L| - 2|R| + 3|N| + |S|
   |V| = |L| + (|L|-1) - (|R|-1) + 2|N| + |S|
       = 2|L| - |R| + 2|N| + |S|
    E = edge set, L = leave set, R = root set, N = network edge set,
    S = "singleton" (single leaf + root) components.
-}

module Complexity.Graphs
  (  makeProgramStringGraph
  )  where

import Complexity.CodeStrings

mainStartString = "main=do\n"

-- | makeProgramString is wrapper for makeGraphProgramString to simplify interface
makeProgramStringGraph :: Int -> Int -> Int -> Int -> String
makeProgramStringGraph  numLeaves numSingle numRoots numNetEdges  =
  let (outProgram, middleString, sumString) = makeGraphProgramString numLeaves numRoots numSingle programStartStringGraph mainStartString "" numLeaves numRoots numNetEdges numSingle
  in
  outProgram  ++ middleString ++ sumString

-- | makeGraphProgramString puts apropriate code and values into string
makeGraphProgramString :: Int -> Int -> Int -> String -> String -> String -> Int -> Int -> Int -> Int -> (String, String, String)
makeGraphProgramString numLeavesOrig numRootsOrig numSingleOrig beforeMain afterMain sumString numLeaves numRoots numNetEdges numSingle =
  let numLeaves2 =  numLeavesOrig - numSingleOrig
      numRoots2 = numRootsOrig - numSingleOrig
      numLeaves3 = numLeaves2 - max 0 (2 * (numRootsOrig - numSingleOrig - 1))
      maxVertex = (2 * numSingleOrig) + max 0 (3 * (numRootsOrig - numSingleOrig - 1))
      maxVertex2 = maxVertex + (2 * numLeaves3) - 1
      doSingle = (numSingleOrig > 0)
      doMinimal = (numRootsOrig > (numSingleOrig + 1))
      doTree = numLeavesOrig > max 0 (2*(numRootsOrig - numSingleOrig - 1)) + numSingleOrig
      doEdges = numNetEdges > 0
  in
  if numSingle > 0 then --make singleton trees one root -> one leaf each
    if doMinimal || doTree then -- More stuff to come
      makeGraphProgramString numLeavesOrig numRootsOrig numSingleOrig (beforeMain ++ getSingletonEdgesString) (afterMain ++ "  let s=a 0 " ++ show numSingleOrig ++ "\n") "  p \"\" (s++"  (numLeaves - numSingle) (numRoots - numSingle) numNetEdges 0
    else --singles only
      makeGraphProgramString numLeavesOrig numRootsOrig numSingleOrig (beforeMain ++ getSingletonEdgesString) (afterMain ++ "  let s=a 0 " ++ show numSingleOrig ++ "\n") "  p \"\" s"  (numLeaves - numSingle) (numRoots - numSingle) numNetEdges 0
  else if numRoots > (numSingle + 1) then  -- have trivial trees.  one root -> two leaves--always do a tree so always a tree follows if this is done. hence "m++""
    if not doSingle then
      makeGraphProgramString numLeavesOrig numRootsOrig numSingleOrig (beforeMain ++ minimalTreesString) (afterMain ++ "  let m=b " ++ show (max 0 $ 2*numSingleOrig) ++ " " ++ show numLeaves2 ++ " " ++ show numRoots2 ++ "\n") (sumString ++ "  p \"\" (m++") (numLeaves - 2*(numRoots - 1)) 1 numNetEdges 0
    else
      makeGraphProgramString numLeavesOrig numRootsOrig numSingleOrig (beforeMain ++ minimalTreesString) (afterMain ++ "  let m=b " ++ show (max 0 $ 2*numSingleOrig) ++ " " ++ show numLeaves2 ++ " " ++ show numRoots2 ++ "\n") (sumString ++ "m++") (numLeaves - 2*(numRoots - 1)) 1 numNetEdges 0
  else if numLeaves > 0 then --pectinate tree for remaining leaves one root -> all remining leaves
      if not doEdges then
        if not doSingle && not doMinimal then
          (beforeMain ++ fullTreeString, afterMain ++ "  let t=c True " ++ show maxVertex ++ " " ++ show numLeaves3 ++ "\n", sumString ++ "  p \"\" t")
        else
          (beforeMain ++ fullTreeString, afterMain ++ "  let t=c True " ++ show maxVertex ++ " " ++ show numLeaves3 ++ "\n", sumString ++ "t)")
      else
        if not doSingle && not doMinimal then
          (beforeMain ++ fullTreeString ++ addEdgeString, afterMain ++ "  let t=c True " ++ show maxVertex ++ " " ++ show numLeaves3 ++ "\n" ++ "  let n=d "++ show maxVertex2 ++ " t " ++ show numNetEdges ++ "\n", sumString ++ "  p \"\" n")
        else
          (beforeMain ++ fullTreeString ++ addEdgeString, afterMain ++ "  let t=c True " ++ show maxVertex ++ " " ++ show numLeaves3 ++ "\n" ++ "  let n=d " ++ show maxVertex2 ++ " t " ++ show numNetEdges ++ "\n", sumString ++ "n)")
  else (beforeMain, afterMain, sumString)
