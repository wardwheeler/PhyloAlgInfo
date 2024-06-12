{- |
Module      :  Types
Description :  Types used by complexity functions
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

module Complexity.Types where

-- | polyList is a list of integers each is the coefficient of polynomial term
-- a_0x^0, a_1x^1, ..., a_{n-1}x^{n-1}
-- list has length 'n' for 'n-1' degree polynomial
type PolynomialList = [Double]

-- | Polynomial Matrix fro matrix utilities
type PolyMatrix = [[PolynomialList]]

-- | Overall machine model including Graph and charcater models (with number of chars for models)
data MachineModel = MachineModel { machineName        :: String
                                 , graphSpecification :: GraphModel
                                 , characterModelList :: [(CharacterModel, Int)]
                                 } deriving (Show, Eq)

-- | Informastion to determine character model
-- model parameter is list of Double in order [alpha, beta] or [alpha1, alpha2, beta] or
-- [kappa, beta]
data CharacterModel = CharacterModel { characterName  :: String
                                     , alphabet       :: [String]
                                     , branchLength   :: (Distribution, [DistributionParameter])
                                     , rateModifiers  :: [(Modifier, [DistributionParameter])]
                                     , changeModel    :: (MarkovModel, RMatrix, PiVector, [ModelParameter])
                                     , precision      :: Int
                                     , charLength     :: Int
                                     } deriving (Show, Eq)

-- | Informatnio to determine Graph Complexity
data GraphModel = GraphModel { graphName       :: String
                             , numLeaves       :: Int
                             , numRoots        :: Int
                             , numSingletons   :: Int
                             , numNetworkEdges :: Int
                             } deriving (Show, Eq)

-- | data types for machine  Graph model and charcaterBlockModel
--data MachineElement = GraphModel | CharacterModel
--    deriving (Show, Eq)

-- | Data type for rate modifiers such as gamma and invariant sites
data Modifier = Gamma | Invariant | None
  deriving (Show, Eq)

-- | Edge length distributions
data Distribution = Uniform | Exponential
  deriving (Show, Eq)

-- | Character transformation models
-- JC69 maps to Neyman
-- LOGMATRIX to GTR (used for ordered/additive and matrix cost characters)
data MarkovModel = Neyman | GTR | JC69 | K80 | F81 | HKY85 | F84 | TN93 | LOGMATRIX
  deriving (Show, Eq)

-- | Markov input matrix
type RMatrix = [[Double]]

-- | Markov transition matrix = R x Pi
type QMatrix = [[Double]]

-- | Alphabet element frequencies
type PiVector = [Double]

-- | Distribution parameter type
type DistributionParameter = Double

-- | Model parmeter type
type ModelParameter = Double

-- | Matrix foo matrix utilities
-- may comflict with external libraries so hide there
type Matrix = [[Double]]

-- | Vector for matrix utilities
-- may comflict with external libraries so hide there
type Vector = [Double]

-- | emptyGraphModel if no graph in machine
emptyGraphModel :: GraphModel
emptyGraphModel =
    GraphModel
        { graphName         = "EmptyGraph"
        , numLeaves         = 0
        , numRoots          = 0
        , numSingletons     = 0
        , numNetworkEdges   = 0
        } 
