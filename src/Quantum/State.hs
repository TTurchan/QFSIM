-- src/Quantum/State.hs
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Quantum.State (
    Qubit,
    QuantumState,
    C, -- Export Complex type alias
    createQubit,
    normalize, -- Export normalize
    zeroState,
    oneState,
    initialState, -- Export initialState
    numQubits,
    tensor -- Export tensor
) where

import Numeric.LinearAlgebra hiding (normalize) -- Hide HMatrix's normalize to avoid clash
import qualified Numeric.LinearAlgebra as LA (normalize) -- Use qualified import for HMatrix's normalize

-- | A type alias for complex numbers provided by hmatrix.
type C = Complex Double

-- | Represents a quantum state vector. Using hmatrix's Vector C type.
-- Example: A single qubit |ψ⟩ = α|0⟩ + β|1⟩ is represented as a vector [α, β]
--          Two qubits |ψ⟩ = α|00⟩ + β|01⟩ + γ|10⟩ + δ|11⟩ as [α, β, γ, δ]
type QuantumState = Vector C

-- | Represents a single qubit state. This is just a QuantumState of size 2.
type Qubit = QuantumState

-- | Creates a single qubit in the specified state, ensuring it is normalized.
-- Requires α^2 + β^2 = 1 (up to floating point precision).
createQubit :: C -> C -> Qubit
createQubit alpha beta = normalize $ fromList [alpha, beta]

-- | Normalizes a quantum state vector (divides by its magnitude).
-- Returns the zero vector if the input norm is zero.
normalize :: QuantumState -> QuantumState
normalize v = if n < 1e-10 then v else v / scalar n -- Use HMatrix's normalize
  where n = LA.norm_2 v

-- | The standard |0⟩ qubit state.
zeroState :: Qubit
zeroState = fromList [1.0 :+ 0.0, 0.0 :+ 0.0] -- Explicit complex numbers

-- | The standard |1⟩ qubit state.
oneState :: Qubit
oneState = fromList [0.0 :+ 0.0, 1.0 :+ 0.0] -- Explicit complex numbers

-- | Creates the initial |0...0⟩ state for n qubits.
initialState :: Int -> QuantumState
initialState n
  | n <= 0    = error "Number of qubits must be positive."
  | otherwise = fromList $ (1.0 :+ 0.0) : replicate (2^n - 1) (0.0 :+ 0.0)

-- | Computes the tensor product of two quantum states. |ψ⟩ ⊗ |φ⟩
tensor :: QuantumState -> QuantumState -> QuantumState
tensor v1 v2 = flatten (outer v1 v2) -- Using outer product and flattening for state tensor product

-- | Get the number of qubits represented by a state vector.
-- The dimension of the state vector is 2^n for n qubits.
numQubits :: QuantumState -> Int
numQubits state =
  let d = size state
  in if d == 0 then 0 else
     let n = logBase 2 (fromIntegral d)
     in if abs (n - fromIntegral (round n)) < 1e-10 -- Check if dimension is a power of 2
        then round n
        else error "State vector dimension is not a power of 2." 