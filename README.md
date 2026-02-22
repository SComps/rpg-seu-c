# RPG-SEU-C Toolchain

A modern VB.NET toolchain for RPG II development, including a TN3270-based SEU editor and an RPG-to-C transpiler.

## Components

### 1. RPG2SEU (RPG II SEU Editor)
An authentic MVS 3.8J style RPG II Source Entry Utility (SEU) editor.
- **TN3270 Interface**: Operates as a TN3270 server (Port 2323). Connect using any 3270 emulator (e.g., Vista TN3270).
- **Form Prompting**: Supports H, F, E, L, I, C, and O form types with dynamic field prompting.
- **Fixed Format**: Preserves exact 80-character fixed-format width required by vintage RPG II compilers.

### 2. RPG2C (RPG to C Transpiler)
A utility to transpile RPG II source code into standard ANSI C.
- **Logic Mapping**: Translates Math (ADD, SUB, MULT, DIV), Data (MOVE, MOVEL), and Control (COMP, GOTO, TAG) opcodes.
- **File Mapping**: RPG F-Specs are automatically mapped to command-line arguments in the generated C program.
- **Compiler Listing**: Generates a `.lst` file with sequence numbers and error diagnostics.

### 3. TN3270Framework
A shared library for handling TN3270 telnet negotiation and data streams.

## Requirements
- .NET 9.0 SDK
- (Optional) MSVC Build Tools or Clang for compiling generated C code on Windows.

## Getting Started

### Running the SEU Editor
1. Navigate to `RPG2SEU` directory.
2. Run `dotnet run`.
3. Connect your 3270 emulator to `localhost:2323`.

### Transpiling RPG to C
1. Navigate to `RPG2C` directory.
2. Run `dotnet run <source.rpg>`.
3. Check the generated `<source>.c` and `<source>.lst` files.

## Visual Studio Compatibility
The solution and projects are compatible with Visual Studio 2022 and higher (including future previews like VS 2026), targeting .NET 9.0.
