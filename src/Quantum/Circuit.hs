-- src/Quantum/Circuit.hs
module Quantum.Circuit (
    QuantumCircuit(..),
    addGate,
    emptyCircuit
    -- TODO: Define more structured circuit representation?
    --       Maybe track qubit count, add measurements, classical registers?
) where

import Quantum.Gate (Gate)

-- | A simple quantum circuit represented as a list of gates to be applied sequentially.
-- This is a very basic representation. Might need refinement later.
newtype QuantumCircuit = QuantumCircuit { getGates :: [Gate] } deriving (Show, Eq)

-- | Creates an empty quantum circuit.
emptyCircuit :: QuantumCircuit
emptyCircuit = QuantumCircuit []

-- | Adds a gate to the end of the circuit.
addGate :: QuantumCircuit -> Gate -> QuantumCircuit
addGate (QuantumCircuit gs) g = QuantumCircuit (gs ++ [g])

-- Consider a representation that specifies which qubit(s) a gate acts on,
-- especially for multi-qubit systems. E.g., [(Gate, [QubitIndex])]
