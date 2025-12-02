# Brainfuck LLVM Compiler

Part of ACC (Anton's Compiler Collection).

## Description

Brainfuck LLVM Compiler

## Features

- BNFC-based parser generator
- Haskell implementation  
- LLVM backend for native code generation
- Complete toolchain from source to executable

## Build

### Prerequisites

```bash
# Install Stack (Haskell)
curl -sSL https://get.haskellstack.org/ | sh

# Install LLVM
brew install llvm@14  # macOS
# or
sudo apt install llvm-14 llvm-14-dev  # Linux
```

### Build with Task

```bash
# Install Task
brew install go-task/tap/go-task  # macOS
# or see: https://taskfile.dev/installation/

# Build
task build
```

### Build manually

```bash
# Generate parser
bnfc -m --haskell grammar.cf

# Build with Stack
stack build
```

## Usage

See project-specific documentation.

## Project Structure

```
.
├── README.md          # This file
├── stack.yaml         # Stack configuration
├── *.cabal            # Cabal package
├── grammar.cf         # BNFC grammar (if applicable)
├── src/               # Source code
├── test/              # Test files
└── examples/          # Example programs
```

## License

See LICENSE file.

## Part of ACC

This project was extracted from the ACC (Anton's Compiler Collection) monorepo.

Original repository: https://github.com/yourusername/acc

---

*Created: 2025-12-02*
