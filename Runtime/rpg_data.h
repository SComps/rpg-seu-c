/**
 * RPG Data Structures and Arrays
 * 
 * Support for RPG II data structures, arrays, and binary fields
 * 
 * Copyright (c) 2026
 * License: MIT
 */

#ifndef RPG_DATA_H
#define RPG_DATA_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * DATA STRUCTURE SUPPORT
 * ============================================================================ */

/**
 * Field types for data structures
 */
typedef enum {
    RPG_FIELD_CHAR,         /* Character field */
    RPG_FIELD_PACKED,       /* Packed decimal */
    RPG_FIELD_ZONED,        /* Zoned decimal */
    RPG_FIELD_BINARY,       /* Binary integer */
    RPG_FIELD_FLOAT,        /* Floating point */
    RPG_FIELD_DATE,         /* Date field */
    RPG_FIELD_TIME,         /* Time field */
    RPG_FIELD_TIMESTAMP,    /* Timestamp field */
    RPG_FIELD_STRUCT        /* Nested structure */
} rpg_field_type;

/**
 * Field definition in a data structure
 */
typedef struct rpg_field_def {
    char name[64];              /* Field name */
    rpg_field_type type;        /* Field type */
    int offset;                 /* Offset in structure */
    int length;                 /* Field length in bytes */
    int decimals;               /* Decimal positions (for numeric) */
    int array_size;             /* Array size (0 if not array) */
    struct rpg_field_def* subfields;  /* For nested structures */
    int subfield_count;         /* Number of subfields */
} rpg_field_def;

/**
 * Data structure definition
 */
typedef struct {
    char name[64];              /* Structure name */
    int size;                   /* Total size in bytes */
    int field_count;            /* Number of fields */
    rpg_field_def* fields;      /* Field definitions */
    bool qualified;             /* Use qualified names */
} rpg_data_struct;

/**
 * Create a new data structure definition
 */
rpg_data_struct* rpg_create_struct(const char* name, bool qualified);

/**
 * Add a field to a data structure
 */
int rpg_add_field(rpg_data_struct* ds, const char* name, rpg_field_type type,
                  int length, int decimals);

/**
 * Add an array field to a data structure
 */
int rpg_add_array_field(rpg_data_struct* ds, const char* name, 
                        rpg_field_type type, int length, int decimals,
                        int array_size);

/**
 * Add a nested structure field
 */
int rpg_add_struct_field(rpg_data_struct* ds, const char* name,
                         rpg_data_struct* nested);

/**
 * Get field by name (supports qualified names)
 */
rpg_field_def* rpg_get_field(rpg_data_struct* ds, const char* name);

/**
 * Get field value as string
 */
void rpg_get_field_value(const void* data, rpg_field_def* field, 
                         char* buffer, int bufsize);

/**
 * Set field value from string
 */
int rpg_set_field_value(void* data, rpg_field_def* field, const char* value);

/**
 * Free data structure definition
 */
void rpg_free_struct(rpg_data_struct* ds);

/* ============================================================================
 * ARRAY SUPPORT
 * ============================================================================ */

/**
 * Array descriptor
 */
typedef struct {
    void* data;                 /* Array data */
    rpg_field_type element_type;/* Element type */
    int element_size;           /* Size of each element */
    int element_count;          /* Number of elements */
    int decimals;               /* Decimal positions (for numeric) */
    bool is_compile_time;       /* Compile-time vs runtime array */
} rpg_array;

/**
 * Create a new array
 */
rpg_array* rpg_create_array(rpg_field_type type, int element_size,
                            int count, int decimals, bool compile_time);

/**
 * Get array element
 */
void* rpg_array_get(rpg_array* array, int index);

/**
 * Set array element
 */
int rpg_array_set(rpg_array* array, int index, const void* value);

/**
 * Initialize compile-time array from data
 */
int rpg_array_init_compile_time(rpg_array* array, const void* data, int size);

/**
 * Sort array
 */
void rpg_array_sort(rpg_array* array, bool ascending);

/**
 * Search array (binary search for sorted arrays)
 */
int rpg_array_search(rpg_array* array, const void* key);

/**
 * Sum numeric array elements
 */
double rpg_array_sum(rpg_array* array);

/**
 * Copy array
 */
int rpg_array_copy(rpg_array* dest, rpg_array* src, int start, int count);

/**
 * Free array
 */
void rpg_free_array(rpg_array* array);

/* ============================================================================
 * BINARY FIELD SUPPORT
 * ============================================================================ */

/**
 * Binary integer types
 */
typedef enum {
    RPG_BINARY_INT8,            /* 1-byte integer */
    RPG_BINARY_INT16,           /* 2-byte integer */
    RPG_BINARY_INT32,           /* 4-byte integer */
    RPG_BINARY_INT64,           /* 8-byte integer */
    RPG_BINARY_UINT8,           /* 1-byte unsigned */
    RPG_BINARY_UINT16,          /* 2-byte unsigned */
    RPG_BINARY_UINT32,          /* 4-byte unsigned */
    RPG_BINARY_UINT64           /* 8-byte unsigned */
} rpg_binary_type;

/**
 * Endianness
 */
typedef enum {
    RPG_ENDIAN_BIG,             /* Big-endian (network byte order) */
    RPG_ENDIAN_LITTLE,          /* Little-endian (Intel) */
    RPG_ENDIAN_NATIVE           /* Native system endianness */
} rpg_endian;

/**
 * Decode binary integer
 */
int64_t rpg_decode_binary(const unsigned char* buf, rpg_binary_type type,
                          rpg_endian endian);

/**
 * Encode binary integer
 */
void rpg_encode_binary(unsigned char* buf, int64_t value, 
                       rpg_binary_type type, rpg_endian endian);

/**
 * Get size of binary type
 */
int rpg_binary_size(rpg_binary_type type);

/**
 * Convert binary to double
 */
double rpg_binary_to_double(const unsigned char* buf, rpg_binary_type type,
                            rpg_endian endian);

/**
 * Convert double to binary
 */
void rpg_double_to_binary(unsigned char* buf, double value,
                          rpg_binary_type type, rpg_endian endian);

/* ============================================================================
 * UTILITY FUNCTIONS
 * ============================================================================ */

/**
 * Get system endianness
 */
rpg_endian rpg_get_system_endian(void);

/**
 * Swap bytes (for endian conversion)
 */
void rpg_swap_bytes(void* data, int size);

/**
 * Align offset to boundary
 */
int rpg_align_offset(int offset, int alignment);

#ifdef __cplusplus
}
#endif

#endif /* RPG_DATA_H */

// Made with Bob
