module Quantum.Visualize (
    visualizeState,
    visualizeCircuit -- Placeholder
    -- TODO: Implement actual visualization (text-based, or hook into external plotting)
) where

import Numeric.LinearAlgebra
import Quantum.State (QuantumState, numQubits)
import Quantum.Circuit (QuantumCircuit)
import Data.Complex (Complex(..), magnitude, phase)
import Text.Printf (printf)
import Data.List (unfoldr) -- Correct import for unfoldr

-- | Provides a basic string representation of a quantum state.
-- Shows amplitudes and probabilities.
visualizeState :: QuantumState -> String
visualizeState state =
    let n = numQubits state
        indices = [0 .. size state - 1]
        stateList = toList state
        basisLabel i = printf "|%s>" (decToBinary n i)
        ampToString (amp@(r :+ i)) =
             printf "(%.3f + %.3fi)" r i ++ printf " [Mag:%.3f, Phase:%.3f rad]" (magnitude amp) (phase amp)
        prob amp = magnitude amp ** 2
        lines' = zipWith (\i amp -> printf "  %s : %s, Prob: %.3f" (basisLabel i) (ampToString amp) (prob amp)) indices stateList
    in "Quantum State (" ++ show n ++ " qubits, dim=" ++ show (size state) ++ "):\n" ++ unlines lines'

-- Helper function to convert decimal to binary string of fixed length
decToBinary :: Int -> Int -> String
decToBinary numBits n = 
    let binDigits = unfoldr (\x -> if x == 0 then Nothing else Just (x `mod` 2, x `div` 2)) n
        paddedBin = reverse $ take numBits (map show binDigits ++ repeat '0') -- Use map show and repeat '0'
    in map intToChar paddedBin -- Convert Int digits back to Char '0' or '1'
    where intToChar i = if i == 1 then '1' else '0' -- Added helper for Int to Char

-- | Placeholder for circuit visualization.
visualizeCircuit :: QuantumCircuit -> String
visualizeCircuit _ = "Circuit visualization not implemented yet." 