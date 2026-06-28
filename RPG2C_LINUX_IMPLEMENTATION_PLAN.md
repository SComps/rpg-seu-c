# RPG2C Linux Implementation Plan
## Complete RPG II Specification for Linux Environment

**Project:** RPG2C Enhanced Linux Implementation  
**Date:** June 27, 2026  
**Target Platform:** Linux (Ubuntu/Debian/RHEL compatible)  
**Language:** VB.NET (.NET 9.0) → C (ANSI C99/C11)

---

## 1. Feature Classification

### 1.1 Linux-Compatible Features (IMPLEMENT) ✅

These features can be fully implemented in a Linux environment using standard POSIX APIs and C libraries.

#### File Operations
- ✅ **Sequential File I/O** - Standard file operations
- ✅ **Random Access Files** - Using `fseek()`, `ftell()`
- ✅ **Indexed Files** - Implement using SQLite or custom B-tree
- ✅ **Relative Files** - Direct record access by number
- ✅ **Stream Files** - Standard text/binary files
- ✅ **File Locking** - Using `flock()` or `fcntl()`

#### Data Types
- ✅ **Packed Decimal** - Already implemented
- ✅ **Binary** - Standard integer types
- ✅ **Zoned Decimal** - ASCII numeric with sign
- ✅ **Character** - Standard strings
- ✅ **Arrays** - C arrays
- ✅ **Data Structures** - C structs
- ✅ **Pointers** - C pointers (limited support)

#### Calculation Operations
- ✅ **Arithmetic** - ADD, SUB, MULT, DIV, MVR, SQRT
- ✅ **String** - CAT, SUBST, SCAN, XLATE
- ✅ **Array** - LOOKUP, XFOOT, SORTA
- ✅ **Logical** - AND, OR, NOT (bitwise)
- ✅ **Comparison** - COMP, IF, DO, DOW, DOU
- ✅ **Date/Time** - Using `time.h` functions

#### Control Flow
- ✅ **Structured** - IF/ELSE/ENDIF, DO/ENDDO, DOW/ENDDO, DOU/ENDDO
- ✅ **Subroutines** - BEGSR/ENDSR/EXSR
- ✅ **Procedures** - Function calls (RPG IV style)
- ✅ **GOTO/TAG** - Direct translation to C

#### I/O Operations
- ✅ **READ** - Sequential read
- ✅ **READE** - Read equal key
- ✅ **READP** - Read previous
- ✅ **READPE** - Read previous equal
- ✅ **CHAIN** - Random access by key
- ✅ **WRITE** - Write new record
- ✅ **UPDATE** - Update existing record
- ✅ **DELETE** - Delete record
- ✅ **SETLL** - Set lower limit
- ✅ **SETGT** - Set greater than
- ✅ **OPEN/CLOSE** - File management

#### Display/Terminal Operations (via TN3270Framework)
- ✅ **WORKSTN Files** - 3270 display files using TN3270Framework
- ✅ **Screen I/O** - READ/WRITE to 3270 terminal
- ✅ **Field-level I/O** - Individual field operations
- ✅ **Indicators for Display** - Function keys, attention keys
- ✅ **Subfile Support** - Scrollable lists/grids
- ✅ **Screen Formatting** - Attributes, colors, highlighting

### 1.2 Mainframe-Only Features (EXCLUDE) ❌

These features are specific to IBM mainframe environments and cannot be meaningfully implemented in Linux.

#### System-Specific
- ❌ **SPOOL Files** - IBM-specific print spooling (use files instead)
- ❌ **JCL Integration** - Job Control Language
- ❌ **QSYS Library System** - AS/400 specific
- ❌ **Data Areas** - AS/400 specific (can simulate with files)
- ❌ **Message Queues** - AS/400 specific (can use IPC)
- ❌ **Program Calls (CALL)** - System-specific (can simulate with shared libraries)
- ❌ **Commitment Control** - AS/400 transaction management (can use SQLite transactions)

#### Device-Specific
- ❌ **5250 Display Files** - AS/400 specific (use 3270 instead)
- ❌ **Printer Files (IBM style)** - Use standard output/files
- ❌ **Tape Files** - Obsolete, use regular files
- ❌ **SPECIAL Files** - System-specific devices

### 1.3 Adaptable Features (IMPLEMENT WITH MODIFICATIONS) ⚠️

These features need adaptation for Linux environment.

#### File Types
- ⚠️ **PRINTER** → Standard output or text file
- ⚠️ **DISK** → Regular files with indexing
- ⚠️ **DATABASE** → SQLite or PostgreSQL backend
- ⚠️ **KEYED** → Indexed file implementation
- ⚠️ **WORKSTN** → 3270 terminal via TN3270Framework

#### Output
- ⚠️ **EXCPT** → Custom output routines
- ⚠️ **Overflow** → Page break handling in files
- ⚠️ **Forms Control** → ANSI escape sequences or PDF generation

#### Display Files (3270 Implementation)
- ⚠️ **DDS (Display Data Specification)** → 3270 screen definitions
- ⚠️ **Subfiles** → 3270 scrollable regions
- ⚠️ **Function Keys** → 3270 AID keys (PF1-PF24)
- ⚠️ **Field Attributes** → 3270 field attributes (protected, numeric, etc.)

---

## 2. Implementation Architecture

### 2.1 Core Components

```
RPG2C/
├── Parser/
│   ├── RpgParser.vb (Enhanced)
│   ├── SpecParsers/
│   │   ├── HSpecParser.vb
│   │   ├── FSpecParser.vb
│   │   ├── DSpecParser.vb (Data structures)
│   │   ├── ISpecParser.vb
│   │   ├── CSpecParser.vb
│   │   ├── OSpecParser.vb
│   │   └── DisplaySpecParser.vb (3270 screens)
│   └── Validators/
│       ├── TypeValidator.vb
│       ├── SyntaxValidator.vb
│       └── SemanticValidator.vb
│
├── CodeGen/
│   ├── CGenerator.vb (Enhanced)
│   ├── Generators/
│   │   ├── FileIOGenerator.vb
│   │   ├── DataStructureGenerator.vb
│   │   ├── CalculationGenerator.vb
│   │   ├── OutputGenerator.vb
│   │   ├── DisplayGenerator.vb (3270 screens)
│   │   └── RuntimeGenerator.vb
│   └── Optimizers/
│       ├── CodeOptimizer.vb
│       └── BufferOptimizer.vb
│
├── Runtime/
│   ├── rpg_runtime.h (C header)
│   ├── rpg_runtime.c (C implementation)
│   ├── rpg_file.c (File operations)
│   ├── rpg_decimal.c (Packed/zoned decimal)
│   ├── rpg_string.c (String operations)
│   ├── rpg_array.c (Array operations)
│   ├── rpg_index.c (Indexed file support)
│   └── rpg_display.c (3270 display operations)
│
├── Display/
│   ├── TN3270Framework/ (from Flashback)
│   ├── rpg_3270.h (3270 interface)
│   ├── rpg_3270.c (3270 implementation)
│   ├── screen_manager.c (Screen buffer management)
│   └── field_manager.c (Field handling)
│
├── DataTypes/
│   ├── VariableDef.vb (Enhanced)
│   ├── ArrayDef.vb
│   ├── DataStructureDef.vb
│   ├── FileDef.vb
│   └── DisplayFileDef.vb (3270 screens)
│
└── Tests/
    ├── ParserTests/
    ├── GeneratorTests/
    ├── DisplayTests/
    └── IntegrationTests/
```

### 2.2 Runtime Library Design

The generated C code will link against a comprehensive runtime library:

```c
// rpg_runtime.h - Core runtime functions

// Decimal operations
double rpg_decode_packed(const unsigned char* buf, int start, int len, int decimals);
void rpg_encode_packed(unsigned char* buf, int start, int len, int decimals, double value);
double rpg_decode_zoned(const char* buf, int len, int decimals);
void rpg_encode_zoned(char* buf, int len, int decimals, double value);

// String operations
void rpg_cat(char* dest, const char* src1, const char* src2, int blanks);
void rpg_subst(char* dest, const char* src, int start, int len);
int rpg_scan(const char* search, const char* source, int start);
void rpg_xlate(char* dest, const char* src, const char* from, const char* to);

// Array operations
int rpg_lookup(const void* array, int count, int size, const void* key, 
               int (*compare)(const void*, const void*));
double rpg_xfoot(const double* array, int count);
void rpg_sorta(void* array, int count, int size, 
               int (*compare)(const void*, const void*));

// File operations
typedef struct RPG_FILE RPG_FILE;
RPG_FILE* rpg_open(const char* filename, const char* mode, int reclen);
int rpg_read(RPG_FILE* file, void* buffer);
int rpg_reade(RPG_FILE* file, void* buffer, const void* key);
int rpg_readp(RPG_FILE* file, void* buffer);
int rpg_chain(RPG_FILE* file, void* buffer, const void* key);
int rpg_write(RPG_FILE* file, const void* buffer);
int rpg_update(RPG_FILE* file, const void* buffer);
int rpg_delete(RPG_FILE* file);
int rpg_setll(RPG_FILE* file, const void* key);
int rpg_setgt(RPG_FILE* file, const void* key);
void rpg_close(RPG_FILE* file);

// Display operations (3270)
typedef struct RPG_DISPLAY RPG_DISPLAY;
RPG_DISPLAY* rpg_display_open(const char* screen_name, int port);
int rpg_display_write(RPG_DISPLAY* display, const void* format);
int rpg_display_read(RPG_DISPLAY* display, void* format);
void rpg_display_close(RPG_DISPLAY* display);
int rpg_display_get_aid(RPG_DISPLAY* display);  // Get attention key
void rpg_display_set_field(RPG_DISPLAY* display, const char* field_name, const char* value);
void rpg_display_get_field(RPG_DISPLAY* display, const char* field_name, char* value);

// Indicator operations
extern bool IND[100];
extern bool IN_LR, IN_1P, IN_MR, IN_L[10];
extern bool IN_KA, IN_KB, IN_KC, IN_KD, IN_KE, IN_KF, IN_KG, IN_KH;  // Function keys
void rpg_seton(int ind);
void rpg_setof(int ind);
```

### 2.3 3270 Display File Implementation

For WORKSTN files, implement using TN3270Framework:

```c
// rpg_display.c - 3270 display support using TN3270Framework

typedef struct {
    TN3270Listener* listener;
    char* screen_buffer;
    int rows;
    int cols;
    int cursor_row;
    int cursor_col;
    unsigned char aid_key;
    struct field_def* fields;
    int field_count;
} RPG_DISPLAY;

typedef struct field_def {
    char name[10];
    int row;
    int col;
    int length;
    unsigned char attr;  // 3270 field attribute
    char* buffer;
} FIELD_DEF;

RPG_DISPLAY* rpg_display_open(const char* screen_name, int port);
int rpg_display_write(RPG_DISPLAY* display, const void* format);
int rpg_display_read(RPG_DISPLAY* display, void* format);
void rpg_display_close(RPG_DISPLAY* display);
```

### 2.4 Indexed File Implementation

For KEYED files, implement using SQLite:

```c
// rpg_index.c - Indexed file support using SQLite

typedef struct {
    sqlite3* db;
    char* table_name;
    int record_length;
    int key_length;
    int current_position;
} RPG_INDEXED_FILE;

RPG_FILE* rpg_open_indexed(const char* filename, const char* mode, 
                           int reclen, int keylen);
int rpg_chain_indexed(RPG_FILE* file, void* buffer, const void* key);
int rpg_setll_indexed(RPG_FILE* file, const void* key);
// ... etc
```

---

## 3. Implementation Phases

### Phase 1: Foundation (Weeks 1-2)
**Goal:** Enhance parser and establish runtime library

#### Tasks:
1. **Enhance Parser**
   - [ ] Add D-Spec (data structure) parsing
   - [ ] Add array definition parsing
   - [ ] Improve error recovery
   - [ ] Add semantic validation

2. **Create Runtime Library**
   - [ ] Implement `rpg_runtime.c` with core functions
   - [ ] Implement packed decimal encode/decode
   - [ ] Implement zoned decimal support
   - [ ] Create basic file I/O wrapper

3. **Testing Infrastructure**
   - [ ] Set up unit test framework
   - [ ] Create parser test suite
   - [ ] Create runtime test suite

**Deliverables:**
- Enhanced parser with D-Spec support
- Basic runtime library (rpg_runtime.c/h)
- Test framework with initial tests

### Phase 2: Data Types & Structures (Weeks 3-4)
**Goal:** Complete data type support

#### Tasks:
1. **Data Structures**
   - [ ] Parse D-Spec data structures
   - [ ] Generate C struct definitions
   - [ ] Handle nested structures
   - [ ] Support qualified names

2. **Arrays**
   - [ ] Parse array definitions
   - [ ] Generate C array declarations
   - [ ] Implement compile-time arrays
   - [ ] Implement runtime arrays

3. **Binary Fields**
   - [ ] Complete binary field decoding
   - [ ] Support different integer sizes
   - [ ] Handle endianness

**Deliverables:**
- Full data structure support
- Array implementation
- Complete binary field support

### Phase 3: File Operations (Weeks 5-7)
**Goal:** Implement all file I/O operations

#### Tasks:
1. **Sequential Files**
   - [ ] Implement READ operation
   - [ ] Implement WRITE operation
   - [ ] Implement UPDATE operation
   - [ ] Implement DELETE operation

2. **Indexed Files**
   - [ ] Design SQLite schema for indexed files
   - [ ] Implement CHAIN operation
   - [ ] Implement SETLL/SETGT operations
   - [ ] Implement READE/READPE operations

3. **File Management**
   - [ ] Implement OPEN/CLOSE operations
   - [ ] Add file locking support
   - [ ] Handle file errors properly
   - [ ] Support multiple open files

**Deliverables:**
- Complete file I/O implementation
- Indexed file support using SQLite
- File operation test suite

### Phase 4: Calculation Operations (Weeks 8-10)
**Goal:** Implement all calculation opcodes

#### Tasks:
1. **String Operations**
   - [ ] Implement CAT (concatenate)
   - [ ] Implement SUBST (substring)
   - [ ] Implement SCAN (search)
   - [ ] Implement XLATE (translate)
   - [ ] Implement TRIM, TRIML, TRIMR

2. **Array Operations**
   - [ ] Implement LOOKUP
   - [ ] Implement XFOOT (sum array)
   - [ ] Implement SORTA (sort array)
   - [ ] Implement MOVEA (move array)

3. **Advanced Math**
   - [ ] Implement MVR (move remainder)
   - [ ] Implement SQRT (square root)
   - [ ] Implement DIV with remainder
   - [ ] Add half-adjust support

4. **Structured Operations**
   - [ ] Implement IF/ELSE/ENDIF
   - [ ] Implement DO/ENDDO loops
   - [ ] Implement DOW/ENDDO (while)
   - [ ] Implement DOU/ENDDO (until)
   - [ ] Implement SELECT/WHEN/OTHER

**Deliverables:**
- All string operations
- All array operations
- Structured programming support
- Calculation test suite

### Phase 5: 3270 Display Support (Weeks 11-13)
**Goal:** Implement WORKSTN file support using TN3270Framework

#### Tasks:
1. **TN3270 Integration**
   - [ ] Integrate TN3270Framework from Flashback
   - [ ] Create RPG display wrapper
   - [ ] Implement screen buffer management
   - [ ] Handle 3270 data streams

2. **Display File Parsing**
   - [ ] Parse display file specifications
   - [ ] Define screen layouts
   - [ ] Define field attributes
   - [ ] Support subfile definitions

3. **Display Operations**
   - [ ] Implement WRITE (display screen)
   - [ ] Implement READ (get user input)
   - [ ] Implement EXFMT (write then read)
   - [ ] Handle function keys (PF1-PF24)
   - [ ] Handle attention keys

4. **Field Management**
   - [ ] Field-level I/O
   - [ ] Field validation
   - [ ] Field attributes (protected, numeric, etc.)
   - [ ] Cursor positioning

5. **Subfile Support**
   - [ ] Subfile definition
   - [ ] Subfile loading
   - [ ] Subfile display
   - [ ] Subfile scrolling

**Deliverables:**
- Complete 3270 display support
- WORKSTN file implementation
- Display file test suite
- Example interactive programs

### Phase 6: Advanced Features (Weeks 14-15)
**Goal:** Add advanced RPG features

#### Tasks:
1. **Procedures (RPG IV style)**
   - [ ] Parse procedure definitions
   - [ ] Generate C functions
   - [ ] Handle parameters
   - [ ] Support return values

2. **Date/Time Operations**
   - [ ] Implement date arithmetic
   - [ ] Support date formats
   - [ ] Implement ADDDUR, SUBDUR
   - [ ] Implement EXTRACT

3. **Error Handling**
   - [ ] Implement %ERROR built-in
   - [ ] Implement %STATUS built-in
   - [ ] Add error indicators
   - [ ] Support MONITOR/ON-ERROR

**Deliverables:**
- Procedure support
- Date/time operations
- Error handling framework

### Phase 7: Optimization & Polish (Weeks 16-17)
**Goal:** Optimize and finalize

#### Tasks:
1. **Code Optimization**
   - [ ] Optimize generated C code
   - [ ] Reduce redundant operations
   - [ ] Improve buffer management
   - [ ] Add compiler hints

2. **Documentation**
   - [ ] Complete API documentation
   - [ ] Write user guide
   - [ ] Create migration guide
   - [ ] Add code examples
   - [ ] Document 3270 display features

3. **Testing & Validation**
   - [ ] Complete integration tests
   - [ ] Performance benchmarking
   - [ ] Memory leak detection
   - [ ] Security audit

**Deliverables:**
- Optimized code generator
- Complete documentation
- Full test coverage
- Performance report

---

## 4. Technical Specifications

### 4.1 Enhanced Parser Specifications

#### D-Spec (Data Structure) Format
```
Columns:
7-21:   Data structure name or field name
22:     External data structure (E)
24-25:  Data structure type (DS, PR, PI)
26-32:  From position
33-39:  To position / Length
40:     Data type (A, P, S, U, B, I, F, D, T, Z)
41-42:  Decimal positions
43-80:  Keywords (DIM, OVERLAY, etc.)
```

#### Enhanced F-Spec Support
```
Additional features:
- KEYED files with key field definitions
- USAGE(*INPUT, *OUTPUT, *UPDATE)
- WORKSTN device type for 3270 displays
- COMMIT keyword for transaction support
- BLOCK keyword for buffering
```

#### Display File Specification (3270)
```
Format similar to DDS but adapted for 3270:
- Screen format definitions
- Field definitions with row/col positions
- Field attributes (protected, numeric, etc.)
- Function key indicators
- Subfile definitions
```

### 4.2 Generated Code Structure

```c
// Generated C code structure

#include "rpg_runtime.h"
#include "rpg_display.h"

// Data structures
typedef struct {
    char field1[20];
    double field2;
    int field3;
} DATASTRUCT1;

// Display formats
typedef struct {
    char CUSTNO[11];
    char NAME[31];
    char ADDR[51];
    unsigned char aid_key;
} SCREEN1_FMT;

// Arrays
double ARRAY1[100];
char ARRAY2[50][20];

// File handles
RPG_FILE* INPUTFILE;
RPG_FILE* OUTPUTFILE;
RPG_DISPLAY* DISPLAY1;

// Indicators
// (from runtime library)

// Procedures
double calculate_total(double amount, double rate) {
    return amount * rate;
}

// Main program
int main(int argc, char** argv) {
    // Initialization
    rpg_init();
    
    // File opens
    INPUTFILE = rpg_open(argv[1], "r", 100);
    OUTPUTFILE = rpg_open(argv[2], "w", 132);
    DISPLAY1 = rpg_display_open("SCREEN1", 2323);
    
    // Main logic cycle
    SCREEN1_FMT screen1;
    
    while (1) {
        // Display screen and get input
        rpg_display_write(DISPLAY1, &screen1);
        rpg_display_read(DISPLAY1, &screen1);
        
        // Check function keys
        if (screen1.aid_key == AID_PF3) break;
        
        // Process input
        if (rpg_chain(INPUTFILE, &record, screen1.CUSTNO) == 0) {
            strcpy(screen1.NAME, record.name);
            strcpy(screen1.ADDR, record.addr);
        }
    }
    
    // Cleanup
    rpg_close(INPUTFILE);
    rpg_close(OUTPUTFILE);
    rpg_display_close(DISPLAY1);
    return 0;
}
```

### 4.3 3270 Display File Format

#### Screen Definition
```c
typedef struct {
    int rows;
    int cols;
    char title[80];
    FIELD_DEF fields[50];
    int field_count;
} SCREEN_DEF;

typedef struct {
    char name[10];
    int row;
    int col;
    int length;
    char type;  // 'A'=alpha, 'N'=numeric, 'P'=protected
    unsigned char attr;  // 3270 attribute byte
} FIELD_DEF;
```

#### Example Screen Layout
```
Row 1:  "Customer Maintenance"
Row 3:  "Customer Number: ______"
Row 5:  "Name: ____________________________"
Row 7:  "Address: ____________________________"
Row 24: "F3=Exit F12=Cancel"
```

### 4.4 File Format Specifications

#### Sequential Files
- Standard text or binary files
- Fixed or variable length records
- Line-oriented or block-oriented

#### Indexed Files (SQLite Implementation)
```sql
CREATE TABLE rpg_file_<name> (
    _rpg_key TEXT PRIMARY KEY,
    _rpg_record BLOB,
    _rpg_deleted INTEGER DEFAULT 0
);

CREATE INDEX idx_<name>_key ON rpg_file_<name>(_rpg_key);
```

#### Relative Files
- Direct access by record number
- Implemented using `fseek()` with fixed record length
- Sparse file support using `lseek()` with SEEK_HOLE

---

## 5. 3270 Display Integration Details

### 5.1 TN3270Framework Integration

The existing TN3270Framework from Flashback will be used to handle:
- TN3270 protocol negotiation
- 3270 data stream encoding/decoding
- Terminal emulation
- Network communication

### 5.2 RPG Display Wrapper

A C wrapper will be created to interface between generated RPG code and TN3270Framework:

```c
// rpg_3270.h

#define AID_ENTER    0x7D
#define AID_PF1      0xF1
#define AID_PF3      0xF3
#define AID_PF12     0xFC
// ... etc

typedef struct RPG_DISPLAY {
    void* tn3270_session;  // TN3270Framework session
    char screen_buffer[1920];  // 24x80 screen
    FIELD_DEF* fields;
    int field_count;
    unsigned char last_aid;
} RPG_DISPLAY;

// Initialize display session
RPG_DISPLAY* rpg_display_open(const char* screen_name, int port);

// Write screen to terminal
int rpg_display_write(RPG_DISPLAY* display, const void* format);

// Read input from terminal
int rpg_display_read(RPG_DISPLAY* display, void* format);

// Get last attention key pressed
unsigned char rpg_display_get_aid(RPG_DISPLAY* display);

// Field operations
void rpg_display_set_field(RPG_DISPLAY* display, const char* field_name, 
                          const char* value);
void rpg_display_get_field(RPG_DISPLAY* display, const char* field_name, 
                          char* value, int maxlen);

// Close display session
void rpg_display_close(RPG_DISPLAY* display);
```

### 5.3 Display File Compilation

Display files will be compiled into C structures:

**RPG Display File:**
```rpg
     A          R SCREEN1
     A                                  1  2'Customer Maintenance'
     A            CUSTNO        10A  I  3 20
     A                                  3  2'Customer Number:'
     A            NAME          30A  O  5 20
     A                                  5  2'Name:'
     A            ADDR          50A  O  7 20
     A                                  7  2'Address:'
     A                                 24  2'F3=Exit'
```

**Generated C:**
```c
typedef struct {
    char CUSTNO[11];
    char NAME[31];
    char ADDR[51];
    unsigned char aid_key;
} SCREEN1_FMT;

static SCREEN_DEF screen1_def = {
    .rows = 24,
    .cols = 80,
    .title = "Customer Maintenance",
    .fields = {
        {"CUSTNO", 3, 20, 10, 'A', 0x00},
        {"NAME", 5, 20, 30, 'A', 0x20},  // Protected
        {"ADDR", 7, 20, 50, 'A', 0x20},  // Protected
    },
    .field_count = 3
};
```

### 5.4 Function Key Handling

```c
// In generated code
rpg_display_read(DISPLAY1, &screen1);

// Check which key was pressed
switch (screen1.aid_key) {
    case AID_ENTER:
        // Process input
        break;
    case AID_PF3:
        IN_KA = true;  // Set indicator KA for PF3
        break;
    case AID_PF12:
        IN_KB = true;  // Set indicator KB for PF12
        break;
}
```

---

## 6. Testing Strategy

### 6.1 Unit Tests

```
Tests/ParserTests/
├── HSpecTests.vb
├── FSpecTests.vb
├── DSpecTests.vb
├── ISpecTests.vb
├── CSpecTests.vb
├── OSpecTests.vb
└── DisplaySpecTests.vb

Tests/GeneratorTests/
├── FileIOGeneratorTests.vb
├── DataStructureGeneratorTests.vb
├── CalculationGeneratorTests.vb
├── OutputGeneratorTests.vb
└── DisplayGeneratorTests.vb

Tests/RuntimeTests/
├── DecimalTests.c
├── StringTests.c
├── ArrayTests.c
├── FileTests.c
└── DisplayTests.c
```

### 6.2 Integration Tests

```
Tests/IntegrationTests/
├── BasicPrograms/
│   ├── hello_world.rpg
│   ├── simple_calc.rpg
│   └── file_copy.rpg
├── DataStructures/
│   ├── nested_ds.rpg
│   ├── array_ops.rpg
│   └── qualified_names.rpg
├── FileOperations/
│   ├── sequential_io.rpg
│   ├── indexed_io.rpg
│   └── update_delete.rpg
├── DisplayPrograms/
│   ├── simple_screen.rpg
│   ├── data_entry.rpg
│   ├── inquiry.rpg
│   └── subfile_list.rpg
└── ComplexPrograms/
    ├── customer_report.rpg
    ├── inventory_update.rpg
    └── order_processing.rpg
```

### 6.3 Display Testing

- Manual testing with 3270 emulator (x3270, Vista TN3270)
- Automated testing using scripted 3270 sessions
- Screen layout validation
- Function key testing
- Field validation testing

### 6.4 Performance Tests

- Transpilation speed for various file sizes
- Generated code execution speed
- Memory usage profiling
- File I/O performance benchmarks
- Display I/O performance (3270 response time)

---

## 7. Success Criteria

### 7.1 Functional Requirements

- [ ] Parse 100% of Linux-compatible RPG II specifications
- [ ] Generate compilable C code for all test cases
- [ ] Pass all unit tests (>95% coverage)
- [ ] Pass all integration tests
- [ ] Handle errors gracefully with clear messages
- [ ] Support interactive 3270 programs

### 7.2 Performance Requirements

- [ ] Transpile 1000-line RPG program in <1 second
- [ ] Generated code performs within 10% of hand-written C
- [ ] Memory usage <100MB for typical programs
- [ ] Support files up to 1GB in size
- [ ] 3270 screen response time <100ms

### 7.3 Quality Requirements

- [ ] Zero buffer overflows in generated code
- [ ] No memory leaks in runtime library
- [ ] Clean compilation with `-Wall -Wextra -Werror`
- [ ] Valgrind clean (no memory errors)
- [ ] Thread-safe runtime library (where applicable)

---

## 8. Migration Path for Existing Code

### 8.1 Current State → Enhanced State

```
Current RPG2C:
- Basic arithmetic ✓
- Simple I/O ✓
- Limited opcodes ✓

Enhanced RPG2C:
- All arithmetic operations ✓
- Complete file I/O ✓
- All string operations ✓
- Data structures ✓
- Arrays ✓
- Procedures ✓
- Error handling ✓
- 3270 display support ✓
```

### 8.2 Backward Compatibility

- All existing test programs continue to work
- Generated code structure remains similar
- Runtime library is additive (no breaking changes)
- New features are opt-in

---

## 9. Documentation Plan

### 9.1 User Documentation

1. **Installation Guide**
   - Prerequisites
   - Building from source
   - Installing runtime library
   - Setting up TN3270Framework

2. **User Manual**
   - Command-line options
   - Supported features
   - Limitations
   - Troubleshooting
   - 3270 display programming

3. **Migration Guide**
   - Converting mainframe RPG to Linux
   - Feature mapping
   - Common pitfalls
   - Best practices
   - 5250 to 3270 conversion

### 9.2 Developer Documentation

1. **Architecture Guide**
   - Component overview
   - Data flow
   - Extension points
   - 3270 integration

2. **API Reference**
   - Parser API
   - Generator API
   - Runtime library API
   - Display API

3. **Contributing Guide**
   - Code style
   - Testing requirements
   - Pull request process

---

## 10. Risk Assessment

### 10.1 Technical Risks

| Risk | Impact | Probability | Mitigation |
|------|--------|-------------|------------|
| SQLite performance for large files | High | Medium | Implement caching, optimize queries |
| 3270 protocol complexity | High | Medium | Leverage existing TN3270Framework |
| Packed decimal precision issues | Medium | Low | Use arbitrary precision library if needed |
| Complex data structure mapping | Medium | Medium | Incremental implementation, extensive testing |
| Memory management in generated code | High | Medium | Automated testing, Valgrind integration |
| Display file compatibility | Medium | Medium | Comprehensive testing with various screens |

### 10.2 Schedule Risks

| Risk | Impact | Probability | Mitigation |
|------|--------|-------------|------------|
| Underestimated complexity | High | Medium | Phased approach, regular reviews |
| Scope creep | Medium | High | Strict feature prioritization |
| Testing bottleneck | Medium | Medium | Parallel test development |
| 3270 integration issues | High | Low | Early prototype, incremental testing |

---

## 11. Resource Requirements

### 11.1 Development Environment

- Linux development machine (Ubuntu 22.04+ or equivalent)
- .NET 9.0 SDK
- GCC/Clang compiler
- SQLite development libraries
- TN3270Framework (from Flashback)
- 3270 emulator (x3270 or Vista TN3270)
- Valgrind for memory testing
- Git for version control

### 11.2 Testing Environment

- Multiple Linux distributions (Ubuntu, Debian, RHEL)
- Various file sizes for performance testing
- 3270 terminal emulator for display testing
- Continuous integration setup (GitHub Actions)

---

## 12. Next Steps

### Immediate Actions (This Week)

1. **Review and Approve Plan**
   - Stakeholder review
   - Technical review
   - Budget approval

2. **Set Up Development Environment**
   - Create feature branch
   - Set up CI/CD pipeline
   - Configure testing framework
   - Integrate TN3270Framework

3. **Begin Phase 1**
   - Start parser enhancements
   - Design runtime library API
   - Create initial test cases

### Short-term Goals (Next Month)

- Complete Phase 1 (Foundation)
- Complete Phase 2 (Data Types)
- Begin Phase 3 (File Operations)

### Long-term Goals (4 Months)

- Complete all 7 phases
- Achieve 90%+ test coverage
- Release beta version for testing
- Create sample 3270 applications

---

## Appendix A: RPG II Opcode Reference (Linux-Compatible)

### Arithmetic Operations
| Opcode | Description | Status | Priority |
|--------|-------------|--------|----------|
| ADD | Add | ✅ Current | - |
| SUB | Subtract | ✅ Current | - |
| MULT | Multiply | ✅ Current | - |
| DIV | Divide | ✅ Current | - |
| MVR | Move Remainder | ❌ Missing | High |
| SQRT | Square Root | ❌ Missing | Medium |
| Z-ADD | Zero and Add | ✅ Current | - |
| Z-SUB | Zero and Subtract | ❌ Missing | Low |

### String Operations
| Opcode | Description | Status | Priority |
|--------|-------------|--------|----------|
| CAT | Concatenate | ❌ Missing | High |
| SUBST | Substring | ❌ Missing | High |
| SCAN | Scan String | ❌ Missing | High |
| XLATE | Translate | ❌ Missing | Medium |
| TRIM | Trim Blanks | ❌ Missing | Medium |
| MOVE | Move | ⚠️ Partial | High |
| MOVEL | Move Left | ⚠️ Partial | High |

### Array Operations
| Opcode | Description | Status | Priority |
|--------|-------------|--------|----------|
| LOOKUP | Lookup Array | ❌ Missing | High |
| XFOOT | Sum Array | ❌ Missing | High |
| SORTA | Sort Array | ❌ Missing | Medium |
| MOVEA | Move Array | ❌ Missing | Medium |

### File Operations
| Opcode | Description | Status | Priority |
|--------|-------------|--------|----------|
| READ | Read Record | ❌ Missing | Critical |
| READE | Read Equal | ❌ Missing | Critical |
| READP | Read Previous | ❌ Missing | High |
| READPE | Read Previous Equal | ❌ Missing | High |
| CHAIN | Random Read | ❌ Missing | Critical |
| WRITE | Write Record | ❌ Missing | Critical |
| UPDATE | Update Record | ❌ Missing | Critical |
| DELETE | Delete Record | ❌ Missing | High |
| SETLL | Set Lower Limit | ❌ Missing | High |
| SETGT | Set Greater Than | ❌ Missing | High |
| OPEN | Open File | ❌ Missing | High |
| CLOSE | Close File | ❌ Missing | High |

### Display Operations (3270)
| Opcode | Description | Status | Priority |
|--------|-------------|--------|----------|
| WRITE | Write Screen | ❌ Missing | Critical |
| READ | Read Screen | ❌ Missing | Critical |
| EXFMT | Write/Read | ❌ Missing | Critical |

### Control Operations
| Opcode | Description | Status | Priority |
|--------|-------------|--------|----------|
| IF | If | ❌ Missing | Critical |
| ELSE | Else | ❌ Missing | Critical |
| ENDIF | End If | ❌ Missing | Critical |
| DO | Do Loop | ❌ Missing | High |
| ENDDO | End Do | ❌ Missing | High |
| DOW | Do While | ❌ Missing | High |
| DOU | Do Until | ❌ Missing | High |
| SELECT | Select | ❌ Missing | Medium |
| WHEN | When | ❌ Missing | Medium |
| OTHER | Other | ❌ Missing | Medium |
| GOTO | Go To | ✅ Current | - |
| TAG | Tag | ✅ Current | - |
| EXSR | Execute Subroutine | ✅ Current | - |
| BEGSR | Begin Subroutine | ✅ Current | - |
| ENDSR | End Subroutine | ✅ Current | - |

---

## Appendix B: Data Type Mapping

### RPG → C Type Mapping

| RPG Type | Description | C Type | Notes |
|----------|-------------|--------|-------|
| A | Character | `char[]` | Null-terminated |
| P | Packed Decimal | `double` | Runtime conversion |
| S | Zoned Decimal | `double` | Runtime conversion |
| B | Binary | `int16_t`, `int32_t` | Size-dependent |
| I | Integer | `int32_t`, `int64_t` | Size-dependent |
| U | Unsigned | `uint32_t`, `uint64_t` | Size-dependent |
| F | Float | `float`, `double` | Size-dependent |
| D | Date | `struct rpg_date` | Custom type |
| T | Time | `struct rpg_time` | Custom type |
| Z | Timestamp | `struct rpg_timestamp` | Custom type |

---

## Appendix C: Example Transformations

### Example 1: Data Structure

**RPG Input:**
```rpg
     D CUSTOMER       DS
     D  CUSTNO                       10A
     D  NAME                         30A
     D  BALANCE                       9P 2
```

**Generated C:**
```c
typedef struct {
    char CUSTNO[11];
    char NAME[31];
    double BALANCE;  // 9 digits, 2 decimals
} CUSTOMER;

CUSTOMER customer = {0};
```

### Example 2: File Operations

**RPG Input:**
```rpg
     FCUSTFILE IF   E           K DISK
     C     CUSTKEY   CHAIN     CUSTREC
     C                   IF        %FOUND
     C                   EVAL      BALANCE = BALANCE + 100
     C                   UPDATE    CUSTREC
     C                   ENDIF
```

**Generated C:**
```c
RPG_FILE* CUSTFILE = rpg_open_indexed("custfile.db", "r+", 100, 10);

if (rpg_chain(CUSTFILE, &CUSTREC, CUSTKEY) == 0) {
    BALANCE = BALANCE + 100;
    rpg_update(CUSTFILE, &CUSTREC);
}
```

### Example 3: 3270 Display

**RPG Input:**
```rpg
     FDISPLAY   CF   E             WORKSTN
     C                   EXFMT     SCREEN1
     C                   IF        *INKC
     C                   RETURN
     C                   ENDIF
```

**Generated C:**
```c
RPG_DISPLAY* DISPLAY = rpg_display_open("DISPLAY", 2323);
SCREEN1_FMT screen1;

rpg_display_write(DISPLAY, &screen1);
rpg_display_read(DISPLAY, &screen1);

if (screen1.aid_key == AID_PF3) {  // *INKC = PF3
    rpg_display_close(DISPLAY);
    return 0;
}
```

### Example 4: Array Operations

**RPG Input:**
```rpg
     D TOTALS         S              9P 2 DIM(12)
     C                   XFOOT     TOTALS    YEARLY
```

**Generated C:**
```c
double TOTALS[12] = {0};
double YEARLY;

YEARLY = rpg_xfoot(TOTALS, 12);
```
