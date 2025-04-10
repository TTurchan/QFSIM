-- src/Quantum/State.hs
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Quantum.State (
    Qubit,
    QuantumState,
    createQubit,
    zeroState,
    oneState,
    numQubits
) where

import Numeric.LinearAlgebra -- Assuming hmatrix; adjust if using a different lib

-- | A type alias for complex numbers provided by hmatrix.
type C = Complex Double

-- | Represents a quantum state vector. Using hmatrix's Vector type.
-- Example: A single qubit |ψ⟩ = α|0⟩ + β|1⟩ is represented as a vector [α, β]
--          Two qubits |ψ⟩ = α|00⟩ + β|01⟩ + γ|10⟩ + δ|11⟩ as [α, β, γ, δ]
type QuantumState = Vector C

-- | Represents a single qubit state. This is just a QuantumState of size 2.
type Qubit = QuantumState

-- | Creates a single qubit in the specified state.
-- Requires α^2 + β^2 = 1 (up to floating point precision).
createQubit :: C -> C -> Qubit
createQubit alpha beta = fromList [alpha, beta]
-- TODO: Add normalization or check? For now, assumes valid inputs.

-- | The standard |0⟩ qubit state.
zeroState :: Qubit
zeroState = fromList [1.0, 0.0]

-- | The standard |1⟩ qubit state.
oneState :: Qubit
oneState = fromList [0.0, 1.0]

-- | Get the number of qubits represented by a state vector.
-- The dimension of the state vector is 2^n for n qubits.
numQubits :: QuantumState -> Int
numQubits state = round $ logBase 2 (fromIntegral $ size state) 