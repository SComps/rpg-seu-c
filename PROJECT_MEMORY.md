# RPG2C Project - Critical Information & Context

## Project Overview

**Project Name**: RPG2C Transpiler  
**Location**: `/home/scott/rpg-seu-c/`  
**Purpose**: Transpile IBM RPG II code to C  
**Language**: VB.NET (.NET 10.0)  
**Status**: Active development - Major parser fixes completed 2026-06-28

## Project Structure

```
/home/scott/rpg-seu-c/
├── RPG2C/                      # Transpiler source code
│   ├── RpgParser.vb           # Parser (RECENTLY FIXED)
│   ├── CGenerator.vb          # C code generator
│   ├── Program.vb             # Main entry point
│   └── RPG2C.vbproj           # Project file
├── Runtime/                    # RPG runtime library (C)
│   ├── rpg_runtime.c/h        # Core runtime
│   ├── rpg_file.c/h           # File I/O
│   └── rpg_data.c/h           # Data structures
├── examples/                   # Test programs
│   ├── READPEOPL.RPG          # Corrected sample (2026-06-28)
│   └── *.rpg                  # Other examples
├── RDPPL                       # Reference: Correct RPG II from IBM mainframe
├── PARSER_FIX_SUMMARY.md      # Documentation of recent fixes
└── PROJECT_MEMORY.md          # This file
```

## Critical Lessons Learned (2026-06-28)

### IBM RPG II Column Specifications - ABSOLUTE RULES

**IBM RPG II is STRICTLY column-based. Every character position matters. Spaces are SIGNIFICANT.**

#### F-Spec (File Description Specification)
```
Columns  Field
------   -----
6        Form Type (F)
7-14     Filename
15       File Type (I=Input, O=Output, U=Update, C=Combined)
16       File Designation (P=Primary, S=Secondary, R=Record, T=Table)
17       End of File (E)
18       File Format (F=Fixed, V=Variable)
24-27    Record Length (first part, zero-filled)
28-31    Record Length continuation OR Key Field Start
33-34    Overflow Indicator (OF)
40-46    Device (READ40, LPRINTER, DISK, etc.)
```

#### L-Spec (Line Counter Specification) - REQUIRED for printer files
```
Columns  Field
------   -----
6        Form Type (L)
7-14     Filename
15-17    Lines per page
18-20    Line number for overflow
21-24    Line counter data
```

#### I-Spec (Input Specification)

**Record Identification:**
```
Columns  Field
------   -----
6        Form Type (I)
7-14     Filename
15-16    Sequence (AA, BB, etc.) - MUST have space if needed
17       Number (N)
18       Option (O)
19-20    Record identifying indicator (01-99)
21-22    Sequence number
```

**Field Description:**
```
Columns  Field
------   -----
6        Form Type (I)
43       Data Type (P=Packed, B=Binary)
44-47    From position (zero-filled, right-justified)
48-51    To position (zero-filled, right-justified)
52       Decimal positions
53-58    Field name
59-60    Control level (L1-L9)
```

#### O-Spec (Output Specification) - MOST CRITICAL

**Record Identification:**
```
Columns  Field
------   -----
6        Form Type (O)
7-14     Filename
15       Type (H=Heading, D=Detail, T=Total, E=Exception)
16       Fetch overflow
17-18    Space before
19       Space after
21-22    Skip before
23       Skip after
25-31    Output indicators (1P, OF, 01, etc.)
```

**Field Description:**
```
Columns  Field
------   -----
6        Form Type (O)
32-37    Field name (for VARIABLES) ← CRITICAL: NOT 21-28!
38       Edit code
39       Blank after
40-43    End position (zero-filled, right-justified)
45-70    Constant (for LITERALS in quotes)
```

**CRITICAL DISTINCTION:**
- **Variables (field names)**: Columns 32-37
- **Constants (literals)**: Columns 45-70
- **NEVER put variable names in columns 45-70!**

#### C-Spec (Calculation Specification)
```
Columns  Field
------   -----
6        Form Type (C)
7-8      Control Level (L1-L9, LR) or Indicators
9-17     Factor 1
18-27    Operation Code
28-32    Factor 2
33-42    Result Field
43-45    Field Length
46-48    Decimal Positions
49       Half Adjust
50-51    Resulting Indicator Hi
52-53    Resulting Indicator Lo
54-55    Resulting Indicator Eq
```

## Parser Implementation Details

### Critical Functions

#### ExtractExact() - MUST USE for column-sensitive data
```vb
Private Function ExtractExact(line As String, startCol As Integer, length As Integer) As String
    ' Extract exact content WITHOUT trimming
    ' Spaces are SIGNIFICANT in RPG II
    If line.Length < startCol Then Return ""
    Dim len = Math.Min(length, line.Length - startCol + 1)
    Return line.Substring(startCol - 1, len)
End Function
```

#### Extract() - Use ONLY for field names where trim is acceptable
```vb
Private Function Extract(line As String, startCol As Integer, length As Integer) As String
    ' Extract and trim - use for field names and values where spaces don't matter
    If line.Length < startCol Then Return ""
    Dim len = Math.Min(length, line.Length - startCol + 1)
    Return line.Substring(startCol - 1, len).Trim()
End Function
```

### Data Structures

#### FileSpec
- Filename, FileType, FileDesignation
- EndOfFile, FileFormat
- RecordLength, KeyFieldStart
- OverflowIndicator, Device

#### LineCounterSpec (NEW - added 2026-06-28)
- Filename
- LinesPerPage, OverflowLine
- LineCounterData

#### InputSpec
- IsRecordLine, Filename
- Sequence, Number, OptionEntry
- RecordIdIndicator, SequenceNumber
- StartPos, EndPos, DecimalPos
- FieldName, IsNumeric, ControlLevel, DataType

#### OutputSpec
- IsRecordLine, IsOverflowLine, OverflowType
- Filename, Type
- FetchOverflow, SpaceBefore, SpaceAfter
- SkipBefore, SkipAfter
- OutputIndicator1
- FieldName (cols 32-37), EndPos (cols 40-43)
- Constant (cols 45-70)
- EditCode, BlankAfter

## Common Mistakes to AVOID

1. **Using Trim() on column extractions** - Destroys significant whitespace
2. **Wrong column positions for O-Spec field names** - Must be 32-37, NOT 21-28
3. **Forgetting L-Spec for printer files** - Required for line counter control
4. **Missing space in I-Spec sequence** - "AA01" is WRONG, "AA  01" is CORRECT
5. **Confusing variable names and constants in O-Specs** - Different column positions!

## Reference Files

### RDPPL - The Gold Standard
Located at: `/home/scott/rpg-seu-c/RDPPL`

This file contains **correct RPG II code from an IBM mainframe** (wrapped in JCL). Lines 7-38 contain the actual RPG source. This is the authoritative reference for correct column positioning.

### Corrected READPEOPL.RPG
Located at: `/home/scott/rpg-seu-c/examples/READPEOPL.RPG`

This is the corrected version extracted from RDPPL, demonstrating proper column positioning for all spec types.

## Build & Test Commands

```bash
# Build transpiler
cd /home/scott/rpg-seu-c/RPG2C
dotnet build

# Test transpiler
cd /home/scott/rpg-seu-c/examples
dotnet ../RPG2C/bin/Debug/net10.0/RPG2C.dll READPEOPL.RPG

# Build generated C code
cd /home/scott/rpg-seu-c/examples
./build.sh

# Run generated program
./READPEOPL input.txt output.txt
```

## Distribution Directory

**DO NOT MODIFY**: `/home/scott/rpg2c/`

This is the compiled distribution directory. All development work should be done in `/home/scott/rpg-seu-c/`.

## Recent Major Changes (2026-06-28)

1. Fixed all column position specifications in RpgParser.vb
2. Added ExtractExact() function to preserve whitespace
3. Added L-Spec parser support
4. Fixed O-Spec field name extraction (32-37, not 21-28)
5. Added OR/OF overflow indicator support
6. Created corrected READPEOPL.RPG sample
7. Successfully tested transpilation with real IBM RPG II code

## Next Steps

1. Update RPG_II_COLUMN_FORMAT.md with verified specifications
2. Review and correct all other example files
3. Create comprehensive parser validation test suite
4. Test with additional real RPG II programs from IBM mainframes
5. Consider adding more detailed error messages for column violations

## Important Notes

- **Always refer to RDPPL for correct column positioning**
- **Test with real IBM RPG II code, not made-up examples**
- **Column positions are 1-based (column 1 is the first character)**
- **Spaces matter - they are not just whitespace in RPG II**
- **When in doubt, count characters manually from column 1**

## Contact & Context

This project is for transpiling legacy IBM RPG II code to modern C. The user (Scott) has access to real IBM mainframe RPG II code and expects strict compliance with IBM RPG II specifications.