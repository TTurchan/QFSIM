-- src/Quantum/Simulate.hs
module Quantum.Simulate (
    applyGate,
    runCircuit
    -- TODO: Add measurement simulation
    -- TODO: Add error handling (e.g., gate size mismatch)
) where

import Numeric.LinearAlgebra
import Quantum.State (QuantumState, numQubits)
import Quantum.Gate (Gate, applyGateToQubit)
import Quantum.Circuit (QuantumCircuit(..), GateSpec(..))

-- | Applies a gate matrix directly to a quantum state.
-- Performs matrix-vector multiplication: |ψ'⟩ = U |ψ⟩
-- Assumes the gate matrix dimension matches the state vector dimension.
applyGate :: Gate -> QuantumState -> QuantumState
applyGate gate state
    | rows gate == size state && cols gate == size state = gate #> state
    | otherwise = error $ "Gate dimension (" ++ show (rows gate) ++ "x" ++ show (cols gate)
                       ++ ") does not match state vector dimension (" ++ show (size state) ++ ")"

-- | Runs a full quantum circuit on an initial state.
runCircuit :: QuantumCircuit -> QuantumState -> QuantumState
runCircuit circuit initialState =
    let n = numQubitsCircuit circuit
        initialNumQubits = numQubits initialState
    in if n /= initialNumQubits
        then error $ "Circuit qubit count (" ++ show n ++ ") does not match initial state qubit count (" ++ show initialNumQubits ++ ")."
        else foldl (applyGateSpec n) initialState (getGateSpecs circuit)

-- | Helper function for foldl to apply a single GateSpec from the circuit.
applyGateSpec :: Int -> QuantumState -> GateSpec -> QuantumState
applyGateSpec totalQubits state (GateSpec gate targetIndices)
    | length targetIndices == 1 = applySingleQubitGate totalQubits state gate (head targetIndices)
    -- Handle specific known multi-qubit gates or use general construction
    | length targetIndices == 2 && isCNOTLike gate = applyCNOTLike totalQubits state gate (targetIndices !! 0) (targetIndices !! 1)
    -- Add cases for other known multi-qubit gates (Toffoli, etc.)
    | gateDimensionsMatchState state gate = applyGate gate state -- Apply directly if dimensions match (e.g., pre-constructed multi-qubit gate)
    | otherwise = error "Unsupported gate specification or dimension mismatch."
    -- TODO: Implement general controlled-U gate construction
    -- TODO: Handle gates acting on more than 2 qubits

-- | Apply a single-qubit gate specified by GateSpec.
applySingleQubitGate :: Int -> QuantumState -> Gate -> Int -> QuantumState
applySingleQubitGate totalQubits state singleQubitGate targetQubit = 
    let fullGate = applyGateToQubit singleQubitGate targetQubit totalQubits
    in applyGate fullGate state

-- | Placeholder/Simplification: Apply a CNOT-like (4x4) gate.
-- Assumes the GateSpec provides the correct 4x4 matrix and targets [control, target].
-- A more robust implementation would construct the CNOT matrix based on control/target.
applyCNOTLike :: Int -> QuantumState -> Gate -> Int -> Int -> QuantumState
applyCNOTLike totalQubits state cnotGate control target =
    if totalQubits == 2 then
        applyGate cnotGate state -- Directly apply if it's a 2-qubit system
    else
        -- TODO: Implement proper construction of CNOT for n-qubit systems
        --       This involves permutation matrices or more complex tensor math.
        --       For now, error out if not a 2-qubit system.
        error "CNOT application currently only supported for 2-qubit circuits directly."

-- Helper to check if gate dimensions match state dimensions
gateDimensionsMatchState :: QuantumState -> Gate -> Bool
gateDimensionsMatchState state gate = 
    let stateDim = size state
    in rows gate == stateDim && cols gate == stateDim

-- Helper to identify if a 4x4 matrix is likely our hardcoded CNOT (very basic check)
-- TODO: Replace this with a more robust gate identification or construction method.
isCNOTLike :: Gate -> Bool
isCNOTLike gate = rows gate == 4 && cols gate == 4 -- Simplistic check for now 