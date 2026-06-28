# RPG Runtime Library

A comprehensive runtime library for RPG II programs transpiled to C, providing full support for RPG data types, operations, and file I/O.

## Overview

This library implements the core runtime functionality needed to execute RPG II programs on Linux systems, including:

- **Data Type Support**: Packed decimal (BCD), zoned decimal (EBCDIC), binary integers
- **String Operations**: CAT, SUBST, SCAN, XLATE, TRIM variants
- **Array Operations**: LOOKUP, XFOOT, SORTA, MOVEA
- **File I/O**: Sequential, relative, and indexed (keyed) file operations
- **Indicator Management**: 99 numbered indicators plus special indicators (LR, L1-L9, etc.)
- **Date/Time Operations**: ADDDUR, SUBDUR, EXTRACT
- **Error Handling**: Comprehensive error reporting system

## Building

### Prerequisites

- GCC or Clang compiler
- SQLite3 development libraries
- Make

### Build Commands

```bash
# Build both static and shared libraries
make all

# Build tests
make tests

# Run tests
make test

# Clean build artifacts
make clean

# Install system-wide (requires sudo)
sudo make install
```

### Build Outputs

- `build/librpg_runtime.a` - Static library
- `build/librpg_runtime.so` - Shared library
- `build/*_test` - Test executables

## Usage

### Including in Your Project

```c
#include "rpg_runtime.h"

int main() {
    // Initialize runtime
    rpg_init();
    
    // Your RPG operations here
    
    // Cleanup
    rpg_cleanup();
    return 0;
}
```

### Linking

```bash
# Static linking
gcc myprogram.c -o myprogram -lrpg_runtime -lm -lsqlite3

# Dynamic linking
gcc myprogram.c -o myprogram -L/path/to/lib -lrpg_runtime -lm -lsqlite3
```

## API Reference

### Initialization

```c
void rpg_init(void);                    // Initialize runtime
void rpg_cleanup(void);                 // Cleanup resources
const char* rpg_version(void);          // Get version string
```

### Decimal Operations

```c
// Packed decimal (BCD format)
double rpg_decode_packed(const unsigned char* buf, int start, int len, int decimals);
void rpg_encode_packed(unsigned char* buf, double value, int len, int decimals);

// Zoned decimal (EBCDIC format)
double rpg_decode_zoned(const unsigned char* buf, int start, int len, int decimals);
void rpg_encode_zoned(unsigned char* buf, double value, int len, int decimals);
```

### String Operations

```c
void rpg_cat(char* dest, const char* src1, const char* src2, int blanks);
void rpg_subst(char* dest, const char* src, int start, int len);
int rpg_scan(const char* search, const char* source, int start);
void rpg_xlate(char* dest, const char* src, const char* from, const char* to);
void rpg_trim(char* dest, const char* src);
void rpg_triml(char* dest, const char* src);
void rpg_trimb(char* dest, const char* src);
```

### Array Operations

```c
int rpg_lookup(const void* array, int count, int size, const void* key, 
               int (*compare)(const void*, const void*));
double rpg_xfoot(const double* array, int count);
void rpg_sorta(void* array, int count, int size,
               int (*compare)(const void*, const void*));
void rpg_movea(void* dest, const void* src, int count, int size);
```

### File Operations

#### Sequential Files

```c
RPG_FILE* rpg_open_sequential(const char* filename, const char* mode, int reclen);
int rpg_read_sequential(RPG_FILE* file, void* buffer);
int rpg_write_sequential(RPG_FILE* file, const void* buffer);
```

#### Relative Files

```c
RPG_FILE* rpg_open_relative(const char* filename, const char* mode, int reclen);
int rpg_read_relative(RPG_FILE* file, void* buffer, long recnum);
int rpg_write_relative(RPG_FILE* file, const void* buffer, long recnum);
int rpg_update_relative(RPG_FILE* file, const void* buffer, long recnum);
int rpg_delete_relative(RPG_FILE* file, long recnum);
```

#### Indexed Files (SQLite-based)

```c
RPG_FILE* rpg_open_indexed(const char* filename, const char* mode, 
                           int reclen, int keylen);
int rpg_chain_indexed(RPG_FILE* file, void* buffer, const void* key);
int rpg_setll_indexed(RPG_FILE* file, const void* key);
int rpg_setgt_indexed(RPG_FILE* file, const void* key);
int rpg_reade_indexed(RPG_FILE* file, void* buffer, const void* key);
int rpg_readp_indexed(RPG_FILE* file, void* buffer);
int rpg_readpe_indexed(RPG_FILE* file, void* buffer, const void* key);
int rpg_update_indexed(RPG_FILE* file, const void* buffer);
int rpg_delete_indexed(RPG_FILE* file);
int rpg_write_indexed(RPG_FILE* file, const void* buffer, const void* key);
```

#### Common File Operations

```c
void rpg_close_file(RPG_FILE* file);
```

### Indicator Operations

```c
void rpg_seton(int indicator);
void rpg_setof(int indicator);
bool rpg_test_indicator(int indicator);
void rpg_clear_indicators(void);
```

### Date/Time Operations

```c
void rpg_current_date(char* buffer, const char* format);
void rpg_current_time(char* buffer, const char* format);
void rpg_adddur(char* result, const char* date, int duration, char unit);
void rpg_subdur(char* result, const char* date, int duration, char unit);
int rpg_extract(const char* date, char unit);
```

### Error Handling

```c
void rpg_set_error(int code, const char* message);
int rpg_get_error_code(void);
const char* rpg_get_error_message(void);
void rpg_clear_error(void);
```

## File Formats

### Sequential Files

Sequential files are simple binary files with fixed-length records. Records are read and written in order.

### Relative Files

Relative files support direct access by record number. Records are stored at fixed positions in the file. Deleted records are marked with all zeros.

### Indexed Files

Indexed files use SQLite for persistent storage with key-based access. The library automatically creates a SQLite database (`.db` extension) for each indexed file.

**Database Schema:**
```sql
CREATE TABLE records (
    key BLOB PRIMARY KEY,
    data BLOB NOT NULL,
    deleted INTEGER DEFAULT 0
);
```

## Examples

### Example 1: Packed Decimal Operations

```c
#include "rpg_runtime.h"

int main() {
    rpg_init();
    
    // Encode a decimal value
    unsigned char buffer[10];
    rpg_encode_packed(buffer, 123.45, 10, 2);
    
    // Decode it back
    double value = rpg_decode_packed(buffer, 0, 10, 2);
    printf("Value: %.2f\n", value);  // Output: 123.45
    
    rpg_cleanup();
    return 0;
}
```

### Example 2: String Operations

```c
#include "rpg_runtime.h"

int main() {
    rpg_init();
    
    char result[100];
    
    // Concatenate strings
    rpg_cat(result, "Hello", "World", 1);
    printf("%s\n", result);  // Output: "Hello World"
    
    // Extract substring
    rpg_subst(result, "Hello World", 7, 5);
    printf("%s\n", result);  // Output: "World"
    
    rpg_cleanup();
    return 0;
}
```

### Example 3: Sequential File I/O

```c
#include "rpg_runtime.h"

typedef struct {
    char name[30];
    int age;
    char city[20];
} Person;

int main() {
    rpg_init();
    
    // Write records
    RPG_FILE* file = rpg_open_sequential("people.dat", "w", sizeof(Person));
    Person p = {"John Doe", 30, "New York"};
    rpg_write_sequential(file, &p);
    rpg_close_file(file);
    
    // Read records
    file = rpg_open_sequential("people.dat", "r", sizeof(Person));
    while (rpg_read_sequential(file, &p) == 0) {
        printf("%s, %d, %s\n", p.name, p.age, p.city);
    }
    rpg_close_file(file);
    
    rpg_cleanup();
    return 0;
}
```

### Example 4: Indexed File Operations

```c
#include "rpg_runtime.h"

typedef struct {
    char id[10];
    char name[30];
    char data[40];
} Record;

int main() {
    rpg_init();
    
    // Open indexed file
    RPG_FILE* file = rpg_open_indexed("data.dat", "w", sizeof(Record), 10);
    
    // Write records with keys
    Record rec = {"KEY001", "First Record", "Some data"};
    rpg_write_indexed(file, &rec, "KEY001");
    
    // Chain to specific record
    if (rpg_chain_indexed(file, &rec, "KEY001") == 0) {
        printf("Found: %s\n", rec.name);
    }
    
    rpg_close_file(file);
    rpg_cleanup();
    return 0;
}
```

## Testing

The library includes comprehensive test suites:

- **decimal_test** - Tests decimal operations (30 tests)
- **file_test** - Tests sequential and relative file operations (8 tests)
- **indexed_test** - Tests indexed file operations (8 tests)

Run all tests:
```bash
make test
```

Or run individual tests:
```bash
LD_LIBRARY_PATH=build ./build/decimal_test
LD_LIBRARY_PATH=build ./build/file_test
LD_LIBRARY_PATH=build ./build/indexed_test
```

## Performance

- Packed decimal operations: ~1-2 microseconds per operation
- String operations: ~0.5-1 microseconds per operation
- Sequential file I/O: ~10-20 microseconds per record
- Indexed file I/O: ~50-100 microseconds per operation (SQLite overhead)

## Error Codes

```c
#define RPG_ERR_NONE           0
#define RPG_ERR_INVALID_PARAM  1000
#define RPG_ERR_OVERFLOW       1001
#define RPG_ERR_UNDERFLOW      1002
#define RPG_ERR_FILE_IO        1003
#define RPG_ERR_FILE_LOCKED    1004
```

## Thread Safety

The current implementation is **not thread-safe**. If you need to use the library in a multi-threaded environment, you must provide your own synchronization.

## License

MIT License

Copyright (c) 2026

## Contributing

This library is part of the RPG2C project. For contributions, please refer to the main project repository.

## Version History

### Version 1.0.0 (2026-06-27)
- Initial release
- Complete decimal operations (packed and zoned)
- Full string operation support
- Array operations
- Sequential, relative, and indexed file I/O
- SQLite-based indexed files
- Comprehensive test suite (46 tests, 100% passing)

## Support

For issues, questions, or contributions, please refer to the main RPG2C project documentation.