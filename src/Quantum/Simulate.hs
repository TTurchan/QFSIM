-- src/Quantum/Simulate.hs
module Quantum.Simulate (
    applyGate,
    runCircuit
    -- TODO: Add measurement simulation
    -- TODO: Add error handling (e.g., gate size mismatch)
) where

import Numeric.LinearAlgebra
import Quantum.State (QuantumState, numQubits)
import Quantum.Gate (Gate)
import Quantum.Circuit (QuantumCircuit, getGates)

-- | Applies a single gate to a quantum state.
-- Performs matrix-vector multiplication: |ψ'⟩ = U |ψ⟩
applyGate :: Gate -> QuantumState -> QuantumState
applyGate gate state
    | rows gate == size state = gate #> state -- Using hmatrix's matrix-vector product
    | otherwise               = error $ "Gate size (" ++ show (rows gate) ++ "x" ++ show (cols gate)
                                     ++ ") does not match state vector size (" ++ show (size state) ++ ")"
-- TODO: Add check: cols gate == size state? (Implicit if matrix is square)
-- TODO: Handle non-square matrices if necessary (e.g., for measurements?)

-- | Runs a full quantum circuit on an initial state.
-- Applies the gates sequentially.
runCircuit :: QuantumCircuit -> QuantumState -> QuantumState
runCircuit circuit initialState = foldl (flip applyGate) initialState (getGates circuit)
-- Note: `flip applyGate` is needed because foldl applies the function as `f accumulator element`,
-- we want `applyGate gate state`. 