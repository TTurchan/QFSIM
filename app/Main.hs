-- app/Main.hs
module Main (main) where

import Quantum.State
import Quantum.Gate
import Quantum.Circuit
import Quantum.Simulate
import Quantum.Visualize
import Numeric.LinearAlgebra (fromList, Complex((:+))) -- Import Complex constructor explicitly

main :: IO ()
main = do
    putStrLn "--- QFSim Start ---"

    -- Example 1: Single qubit Hadamard
    putStrLn "\nExample 1: Applying Hadamard to |0>"
    let initialState0 = zeroState
    let circuitH = emptyCircuit `addGate` hadamard
    let finalStateH = runCircuit circuitH initialState0
    putStrLn $ "Initial State: " ++ visualizeState initialState0
    putStrLn $ "Final State (H|0>):\n" ++ visualizeState finalStateH

    -- Example 2: Bell State (|00> + |11>) / sqrt(2)
    putStrLn "\nExample 2: Creating Bell State (|Φ+>)"
    -- Start with |00> state vector: [1, 0, 0, 0]
    -- Need Complex type for the state vector elements
    let initialState00 = fromList [(1.0 :+ 0.0), (0.0 :+ 0.0), (0.0 :+ 0.0), (0.0 :+ 0.0)] :: QuantumState
    -- Need to apply H to the first qubit, then CNOT
    -- Building multi-qubit gates properly requires tensor products
    -- This is a simplification using pre-defined CNOT for 2 qubits
    -- TODO: Replace this with proper gate application logic
    -- let hOnQubit0 = -- Construct H ⊗ I using tensor products
    -- let bellCircuit = emptyCircuit `addGate` hOnQubit0 `addGate` cnot
    -- For now, let's just show CNOT acting on |10> for demonstration
    putStrLn "\nApplying CNOT to |10>:"
    let initialState10 = fromList [(0.0 :+ 0.0), (0.0 :+ 0.0), (1.0 :+ 0.0), (0.0 :+ 0.0)] :: QuantumState
    let circuitCNOT = emptyCircuit `addGate` cnot
    let finalStateCNOT = runCircuit circuitCNOT initialState10
    putStrLn $ "Initial State: " ++ visualizeState initialState10
    putStrLn $ "Final State (CNOT|10>):\n" ++ visualizeState finalStateCNOT


    putStrLn "\n--- QFSim End ---"

-- TODO: Add command-line argument parsing to define initial state and circuit
-- TODO: Implement proper multi-qubit gate construction (tensor product)
