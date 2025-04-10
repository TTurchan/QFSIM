# QFSim - Quantum Circuit Simulator

QFSim is a minimal quantum circuit simulator written in Haskell, focusing on a purely functional approach.

## Overview

This project aims to simulate the behavior of quantum circuits using Haskell's strong type system and functional features. It utilizes the `hmatrix` library for underlying linear algebra operations.

## Features (Planned)

*   Representation of quantum states (qubits, registers)
*   Implementation of common quantum gates (Pauli, Hadamard, CNOT, etc.)
*   Circuit definition and sequential application of gates
*   Simulation of state evolution
*   (Future) Measurement simulation
*   (Future) Basic circuit visualization or textual representation
*   (Future) Noise models
*   (Future) WASM compilation target

## Quick Start

**Prerequisites:**

*   [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) (Haskell build tool)

**Build:**

```bash
stack build
```

**Run the executable:**

```bash
stack exec QFSim-exe
```

**Run tests (currently empty):**

```bash
stack test
```

## Project Structure

*   `src/`: Library code containing the core quantum simulation logic.
    *   `Quantum/State.hs`: Defines quantum state representations.
    *   `Quantum/Gate.hs`: Defines quantum gates.
    *   `Quantum/Circuit.hs`: Defines circuit structures.
    *   `Quantum/Simulate.hs`: Implements the simulation logic (applying gates).
    *   `Quantum/Visualize.hs`: Utilities for representing states/circuits.
*   `app/`: Executable code (CLI entry point).
    *   `Main.hs`: Main application driver.
*   `test/`: Test suite.
*   `stack.yaml`: Stack project configuration.
*   `package.yaml`: Cabal package description (used by Stack/Hpack).
*   `.github/workflows/`: GitHub Actions CI configuration.

## Contributing

Contributions are welcome! Please feel free to open an issue or submit a pull request. 