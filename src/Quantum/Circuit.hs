-- src/Quantum/Circuit.hs
module Quantum.Circuit (
    GateSpec(..),
    QuantumCircuit(..),
    addGate,
    addSingleQubitGate,
    addTwoQubitGate,
    emptyCircuit
    -- TODO: Define more structured circuit representation?
    --       Maybe track qubit count, add measurements, classical registers?
) where

import Quantum.Gate (Gate)

-- | Specifies a gate application: the gate matrix and the qubit indices it acts upon.
-- For single-qubit gates, the list will have one index.
-- For two-qubit gates (like CNOT), it might have [control, target].
-- Qubit indices are 0-based.
data GateSpec = GateSpec Gate [Int] deriving (Show, Eq)

-- | Represents a quantum circuit, including the number of qubits and a sequence of gate applications.
-- TODO: Add support for measurements, classical registers.
data QuantumCircuit = QuantumCircuit {
    numQubitsCircuit :: Int,       -- Number of qubits in the circuit
    getGateSpecs     :: [GateSpec] -- Sequence of gates and their target qubits
} deriving (Show, Eq)

-- | Creates an empty quantum circuit for a specified number of qubits.
emptyCircuit :: Int -> QuantumCircuit
emptyCircuit n
    | n <= 0    = error "Number of qubits must be positive."
    | otherwise = QuantumCircuit { numQubitsCircuit = n, getGateSpecs = [] }

-- | Adds a general gate specification to the end of the circuit.
-- Use with caution: ensures gate dimension matches the number of target qubits.
addGate :: QuantumCircuit -> GateSpec -> QuantumCircuit
addGate circuit spec@(GateSpec gate targets) = 
    -- Basic validation (can be expanded)
    if maximum targets >= numQubitsCircuit circuit || minimum targets < 0
        then error "Gate target index out of bounds for the circuit."
    -- TODO: Add check for gate dimension vs number of targets (e.g., 2^length targets)
    else circuit { getGateSpecs = getGateSpecs circuit ++ [spec] }

-- | Helper to add a single-qubit gate to the circuit.
addSingleQubitGate :: QuantumCircuit -> Gate -> Int -> QuantumCircuit
addSingleQubitGate circuit gate targetQubit = 
    if rows gate /= 2 || cols gate /= 2
        then error "addSingleQubitGate requires a 2x2 gate."
    else addGate circuit (GateSpec gate [targetQubit])

-- | Helper to add a two-qubit gate (like CNOT) to the circuit.
-- Assumes the gate provided is already the 4x4 matrix.
addTwoQubitGate :: QuantumCircuit -> Gate -> Int -> Int -> QuantumCircuit
addTwoQubitGate circuit gate controlQubit targetQubit =
    if rows gate /= 4 || cols gate /= 4
        then error "addTwoQubitGate requires a 4x4 gate."
    else if controlQubit == targetQubit
        then error "Control and target qubits cannot be the same."
    -- Targets list convention: [control, target]
    else addGate circuit (GateSpec gate [controlQubit, targetQubit])

-- Consider a representation that specifies which qubit(s) a gate acts on,
-- especially for multi-qubit systems. E.g., [(Gate, [QubitIndex])]
