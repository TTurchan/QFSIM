-- src/Quantum/Gate.hs
module Quantum.Gate (
    Gate,
    C, -- Re-export complex type alias
    identity,
    pauliX,
    pauliY,
    pauliZ,
    hadamard,
    cnot, -- Keep simple CNOT for now, can be redefined later
    sGate, -- Added S gate
    tGate, -- Added T gate
    applyGateToQubit -- Export new function
    -- TODO: Add parameterized gates (U3, Rx, Ry, Rz)
    -- TODO: Implement general controlled gates
    -- TODO: Redefine CNOT using applyGateToQubit logic for clarity/generality
) where

import Numeric.LinearAlgebra -- Assuming hmatrix
import Quantum.State (C) -- Import C from Quantum.State

-- | A quantum gate represented as a matrix.
-- For a single qubit, it's a 2x2 matrix.
-- For n qubits, it's a 2^n x 2^n matrix.
type Gate = Matrix C

-- --- Single Qubit Gates ---

-- | Identity gate (I) for a single qubit.
identity1 :: Gate
identity1 = ident 2

-- | Identity gate for n qubits.
identity :: Int -> Gate
identity n = ident (2^n) -- 2^n x 2^n identity matrix

-- | Pauli-X gate (NOT gate).
pauliX :: Gate
pauliX = (2><2) [ 0:+0, 1:+0
               , 1:+0, 0:+0 ]

-- | Pauli-Y gate.
pauliY :: Gate
pauliY = (2><2) [ 0:+0, 0:+(-1)
               , 0:+1, 0:+0 ]

-- | Pauli-Z gate.
pauliZ :: Gate
pauliZ = (2><2) [ 1:+0,  0:+0
               , 0:+0, -1:+0 ]

-- | Hadamard gate (H).
hadamard :: Gate
hadamard = scale (1 / sqrt 2) $ (2><2) [ 1:+0,  1:+0
                                       , 1:+0, -1:+0 ]

-- | Phase gate (S).
sGate :: Gate
sGate = (2><2) [ 1:+0, 0:+0
              , 0:+0, 0:+1 ] -- S = sqrt(Z)

-- | π/8 gate (T).
tGate :: Gate
tGate = (2><2) [ 1:+0, 0:+0
              , 0:+0, cos(pi/4):+sin(pi/4) ] -- T = fourth_root(Z)

-- --- Multi-Qubit Gates ---
-- Note: These are simple examples. Real implementation needs tensor products
--       or a more structured way to define multi-qubit gates acting on specific qubits.

-- | Controlled-NOT gate (CNOT).
-- Assumes control qubit is the first, target is the second (in a 2-qubit system).
-- |00> -> |00>
-- |01> -> |01>
-- |10> -> |11>
-- |11> -> |10>
-- Basis order: |00>, |01>, |10>, |11>
cnot :: Gate
cnot = (4><4) [ 1:+0, 0:+0, 0:+0, 0:+0
              , 0:+0, 1:+0, 0:+0, 0:+0
              , 0:+0, 0:+0, 0:+0, 1:+0
              , 0:+0, 0:+0, 1:+0, 0:+0 ]

-- | Constructs the matrix for a single-qubit gate acting on a specific qubit
--   within an n-qubit system using tensor products.
--   Example: H gate on qubit 0 in a 3-qubit system: H ⊗ I ⊗ I
--   Example: H gate on qubit 1 in a 3-qubit system: I ⊗ H ⊗ I
applyGateToQubit :: Gate -> Int -> Int -> Gate
applyGateToQubit gate targetQubit totalQubits
    | targetQubit < 0 || targetQubit >= totalQubits = error "Target qubit index out of bounds."
    | rows gate /= 2 || cols gate /= 2             = error "Gate must be a single-qubit (2x2) gate."
    | otherwise = foldr1 kronecker components
        where
            components = [ if i == targetQubit then gate else identity1
                         | i <- [0 .. totalQubits - 1]
                         ]
-- Note: `kronecker` is the tensor product (Kronecker product) for matrices in HMatrix.

-- TODO: Implement functions to construct multi-qubit gates using tensor products
-- e.g., applyGateToQubit :: Gate -> Int -> Int -> Gate  -- Gate, Target Qubit Index, Total Qubits -> Full Gate 