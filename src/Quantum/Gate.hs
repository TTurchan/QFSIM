-- src/Quantum/Gate.hs
module Quantum.Gate (
    Gate,
    identity,
    pauliX,
    pauliY,
    pauliZ,
    hadamard,
    cnot
    -- TODO: Add more gates (S, T, Phase, CPhase, Toffoli, etc.)
    -- TODO: Add parameterized gates (U3, Rx, Ry, Rz)
) where

import Numeric.LinearAlgebra -- Assuming hmatrix

type C = Complex Double

-- | A quantum gate represented as a matrix.
-- For a single qubit, it's a 2x2 matrix.
-- For n qubits, it's a 2^n x 2^n matrix.
type Gate = Matrix C

-- --- Single Qubit Gates ---

-- | Identity gate (I).
identity :: Int -> Gate
identity n = ident (2^n) -- 2^n x 2^n identity matrix

-- | Pauli-X gate (NOT gate).
pauliX :: Gate
pauliX = (2><2) [ 0, 1
               , 1, 0 ]

-- | Pauli-Y gate.
pauliY :: Gate
pauliY = (2><2) [ 0, 0 :+ (-1)
               , 0 :+ 1, 0 ]

-- | Pauli-Z gate.
pauliZ :: Gate
pauliZ = (2><2) [ 1,  0
               , 0, -1 ]

-- | Hadamard gate (H).
hadamard :: Gate
hadamard = scale (1 / sqrt 2) $ (2><2) [ 1,  1
                                       , 1, -1 ]

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
cnot = (4><4) [ 1, 0, 0, 0
              , 0, 1, 0, 0
              , 0, 0, 0, 1
              , 0, 0, 1, 0 ]

-- TODO: Implement functions to construct multi-qubit gates using tensor products
-- e.g., applyGateToQubit :: Gate -> Int -> Int -> Gate  -- Gate, Target Qubit Index, Total Qubits -> Full Gate 