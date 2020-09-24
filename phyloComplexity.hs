{- |
Module      :  Kolmogorov (algorithmic) complexity of overall machine
Description :  Program reads input file with m,achine configuration and
               determines Kolmnogorov Complexity of Graph and Character models
Copyright   :  (c) 2018-2020 Ward C. Wheeler, Division of Invertebrate Zoology, AMNH. All rights reserved.
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

module Main where

import           Complexity.CharacterModels
import           Complexity.Graphs
import           Complexity.MathUtilities
import           Complexity.Parsers
import           Complexity.Types
import           Complexity.Utilities
import           System.Environment
import           System.IO
-- import Debug.Trace

-- | writeTCM takes model structure (name, alphabet, matrix) and writes file
-- creates filename from stub and character name
writeTCMFile :: String -> String -> (String, [String], [[Double]]) -> IO()
writeTCMFile unitType stub (charName, localAlphabet, tcmMatrix) =
    let outFileName = stub ++ charName ++ "." ++ unitType ++ ".tcm"
        alphString = concatMap (++ " ") localAlphabet
        matrixString = matrix2String tcmMatrix
        tcmString = alphString ++ "\n" ++ matrixString
    in
    writeFile outFileName tcmString

-- | 'main' Main Function to execute program
main :: IO ()
main =
    do
        --Read input parameter
        args <- getArgs
        if length args /= 2 then error "Error:  Incorrect number of arguments.  Require a filename for input configuration and a stub for output files"
        else do hPutStr stderr "Inputs: "
                hPutStrLn stderr (head args ++ " machine configuration file and " ++ args!!1 ++ " stub ")
                hPutStrLn stderr ("Output files: " ++ ((args!!1) ++ ".complexity") ++ " and potentially multiple " ++ ((args!!1) ++ ".X.tcm"))
                hPutStrLn stderr ("Haskell code files: " ++ ((args!!1) ++ ".graph.hs") ++ " and " ++ ((args!!1) ++ ".characters.hs"))
                hPutStrLn stderr ("Huffman code files: " ++ ((args!!1) ++ ".graph.huffman") ++ " and " ++ ((args!!1) ++ ".characters.huffmans"))
        let stub = args!!1

        --Read and parse contents of input file
        inFileHandle <- openFile (head args) ReadMode
        inContents <- hGetContents inFileHandle
        let machineConfig = parseMachineFile inContents
        let graphConfig = graphSpecification machineConfig
        let charInfo = characterModelList machineConfig
        putStrLn ("Machine configuration: " ++ show machineConfig)
        --putStrLn ("\tGraph configuration: " ++ show graphConfig)
        --putStrLn ("\tCharacter configurations: " ++ show charInfo)

        --Calculate complexity of Graph Component
        -- based on number of edges |E| * 2 (for vertex specification) * log |V| (for number of bits required to specify largest vertex index).
        let graphProgram = makeProgramStringGraph (numLeaves graphConfig) (numSingletons graphConfig) (numRoots graphConfig) (numNetworkEdges graphConfig)
        let (graphShannonBits, graphHuffmanLengthBits, graphHuffmanBitRep) = getInformationContent graphProgram

        --Output complexity of Graph Component
        putStrLn ("Shannon bits of Graph program = " ++ show graphShannonBits)
        putStrLn ("Huffman bits of Graph program = " ++ show graphHuffmanLengthBits)

        graphHuffmanBinaryHandle <- openFile (stub ++ ".graph.huffman") WriteMode
        graphHaskellHandle <- openFile (stub ++ ".graph.hs") WriteMode
        hPutStrLn graphHuffmanBinaryHandle graphHuffmanBitRep
        hPutStrLn graphHaskellHandle graphProgram
        hClose graphHuffmanBinaryHandle
        hClose graphHaskellHandle

        --Calculate cost of converting general graph to tree -- 1 bit for each pair of network edges (r/2)
        ----(must come in pairs for phylogenetic network) and at most log 2^r (or just r) for number of display trees
        ----the number of display trees can be determined by creating resolutions but r is an upper bound

        -- upper bound on number of display trees 2^r
        -- log 2^r bits to encde which display tree to use for a character, hence r
        let displayTreeSwitchingComplexity = fromIntegral (numNetworkEdges graphConfig)
        -- bits to converte a phylogenetic graph with r network edges to a tree
        ---- network edges come in pairs (specification of phylogenetic graph) so need to delete r/2 edges to make a tree
        ---- each edge is defined by 2 vertices hence, r * log |V| conversion cost multiplied by number of display trees
        ---- 2^r upper bound on number of display trees
        -- number of vertices in phylogenetic graph
        let numGraphVertices = fromIntegral $ (2 * numLeaves graphConfig) - numRoots graphConfig + (2 * numNetworkEdges graphConfig) + numSingletons graphConfig
        let graph2DisplayTreeComplexity = fromIntegral (numNetworkEdges graphConfig) * logBase 2.0 numGraphVertices
        hPutStrLn stderr ("Softwired Graph -> Tree complexity: " ++ show graph2DisplayTreeComplexity)

        --calculate and output Bit TCMs for each character change model for complexity calculations
        let tcmListBit = fmap (makeTCM log2 . fst) charInfo
        mapM_ (writeTCMFile "bit" stub) tcmListBit

        --calculate and output Nat TCMs for each character change model for likelihood calculations
        let tcmListE = fmap (makeTCM logE . fst) charInfo
        mapM_ (writeTCMFile "nat" stub) tcmListE

        let (aic, bic) = getAICBIC charInfo 0 0
        putStrLn ("Akaike Information adjustment : " ++ show aic)
        putStrLn ("Bayesian Information adjustment : " ++ show bic)

        --Calculate complexity of Character Model Components
        let characterProgram = makeProgramStringCharacters (fmap fst charInfo)

        -- if want to show huffman representation otherwise don't need.
        -- let (characterShannonBits, characterHuffmanLengthBits, characterHuffmanBitRep) = getInformationContent characterProgram
        let (characterShannonBits, characterHuffmanLengthBits, _) = getInformationContent characterProgram
        writeFile (stub ++ ".blockModels.hs") characterProgram

        let characterModelComplexity = characterShannonBits
        hPutStrLn stderr ("Shannon bits of character program: " ++ show characterShannonBits)
        hPutStrLn stderr ("Huffman bits of character program: " ++ show characterHuffmanLengthBits)
        --Calaculate Complexity of model switching over Character Components
        --Need to read from MachineModel
        --Need to add the log number of characters for each
        let modelSpecificationComplexity = fromIntegral (length charInfo) * logBase 2.0 (fromIntegral $ length charInfo)
        hPutStrLn stderr ("Character model switching complexity: " ++ show modelSpecificationComplexity)
        let charNumComplexity = sum $ fmap (logBase 2.0 . fromIntegral) (snd <$> characterModelList machineConfig)
        let characterNumber = fromIntegral $ sum $ snd <$> characterModelList machineConfig
        hPutStrLn stderr ("Character number : " ++ show characterNumber)
        hPutStrLn stderr ("Character number complexity: " ++ show charNumComplexity)

        -- display tree could be at most 2^r but is limited by the number of 'blocks' or characters that could
        ----follow individual display trees so the number of characters (take smaller of two),
        ----but still need 'r' bits to encoding which display tree
        -- let characterNumber = fromIntegral $ sum $ snd Control.Applicative.<$> characterModelList machineConfig
        let softWiredFactor = displayTreeSwitchingComplexity + (min (2 ** fromIntegral (numNetworkEdges graphConfig)) characterNumber * graph2DisplayTreeComplexity)
        hPutStrLn stderr ("Softwire complexity factor: " ++ show softWiredFactor)

        --Output machine Complexity
        let machineComplexity = graphShannonBits + softWiredFactor + characterModelComplexity + modelSpecificationComplexity + charNumComplexity
        hPutStrLn stderr ("Machine complexity : " ++ show machineComplexity)
        machineHandle <- openFile (stub ++ ".complexity") WriteMode
        hPutStrLn machineHandle ("Machine complexity : " ++ show machineComplexity)
        hPutStrLn machineHandle ("Graph complexity : " ++ show graphShannonBits)
        hPutStrLn machineHandle ("Softwire complexity factor: " ++ show softWiredFactor)
        hPutStrLn machineHandle ("Model specification complexity : " ++ show modelSpecificationComplexity)
        hPutStrLn machineHandle ("Character model complexity : " ++ show characterModelComplexity)
        hPutStrLn machineHandle ("Character number complexity : " ++ show charNumComplexity)
        hClose machineHandle

        hPutStrLn stderr "All done"
