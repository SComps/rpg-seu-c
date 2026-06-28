/**
 * RPG Runtime Library
 *
 * Comprehensive runtime support for RPG II programs transpiled to C
 * Provides decimal operations, string handling, array operations, and file I/O
 *
 * Copyright (c) 2026
 * License: MIT
 */

#ifndef RPG_RUNTIME_H
#define RPG_RUNTIME_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <time.h>
#include "rpg_file.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * VERSION INFORMATION
 * ============================================================================ */
#define RPG_RUNTIME_VERSION_MAJOR 1
#define RPG_RUNTIME_VERSION_MINOR 0
#define RPG_RUNTIME_VERSION_PATCH 0

/* ============================================================================
 * DECIMAL OPERATIONS
 * ============================================================================ */

/**
 * Decode packed decimal (BCD) format to double
 * @param buf Buffer containing packed decimal data
 * @param start Starting position in buffer (0-based)
 * @param len Length in bytes
 * @param decimals Number of decimal places
 * @return Decoded value as double
 */
double rpg_decode_packed(const unsigned char* buf, int start, int len, int decimals);

/**
 * Encode double to packed decimal (BCD) format
 * @param buf Buffer to write packed decimal data
 * @param start Starting position in buffer (0-based)
 * @param len Length in bytes
 * @param decimals Number of decimal places
 * @param value Value to encode
 */
void rpg_encode_packed(unsigned char* buf, int start, int len, int decimals, double value);

/**
 * Decode zoned decimal (EBCDIC numeric) to double
 * @param buf Buffer containing zoned decimal data
 * @param len Length in bytes
 * @param decimals Number of decimal places
 * @return Decoded value as double
 */
double rpg_decode_zoned(const char* buf, int len, int decimals);

/**
 * Encode double to zoned decimal (EBCDIC numeric) format
 * @param buf Buffer to write zoned decimal data
 * @param len Length in bytes
 * @param decimals Number of decimal places
 * @param value Value to encode
 */
void rpg_encode_zoned(char* buf, int len, int decimals, double value);

/**
 * Format numeric value with edit code
 * @param dest Destination buffer
 * @param val Value to format
 * @param code Edit code (Z, 1-4, A-D, J-Q, X-Z)
 * @param len Field length
 * @param dec Decimal positions
 */
void rpg_format_edit(char* dest, double val, char code, int len, int dec);

/* ============================================================================
 * STRING OPERATIONS
 * ============================================================================ */

/**
 * Concatenate strings with optional blanks
 * @param dest Destination buffer
 * @param src1 First source string
 * @param src2 Second source string
 * @param blanks Number of blanks between strings
 */
void rpg_cat(char* dest, const char* src1, const char* src2, int blanks);

/**
 * Extract substring
 * @param dest Destination buffer
 * @param src Source string
 * @param start Starting position (1-based)
 * @param len Length to extract
 */
void rpg_subst(char* dest, const char* src, int start, int len);

/**
 * Scan for substring
 * @param search String to search for
 * @param source String to search in
 * @param start Starting position (1-based)
 * @return Position of match (1-based) or 0 if not found
 */
int rpg_scan(const char* search, const char* source, int start);

/**
 * Translate characters
 * @param dest Destination buffer
 * @param src Source string
 * @param from Characters to translate from
 * @param to Characters to translate to
 */
void rpg_xlate(char* dest, const char* src, const char* from, const char* to);

/**
 * Trim trailing blanks
 * @param dest Destination buffer
 * @param src Source string
 */
void rpg_trim(char* dest, const char* src);

/**
 * Trim leading blanks
 * @param dest Destination buffer
 * @param src Source string
 */
void rpg_triml(char* dest, const char* src);

/**
 * Trim leading and trailing blanks
 * @param dest Destination buffer
 * @param src Source string
 */
void rpg_trimb(char* dest, const char* src);

/* ============================================================================
 * MATH OPERATIONS
 * ============================================================================ */

/**
 * Square root
 * @param value Value to calculate square root of
 * @return Square root of value
 */
double rpg_sqrt(double value);

/**
 * Move remainder (from last DIV operation)
 * @param dividend Dividend from division
 * @param divisor Divisor from division
 * @return Remainder
 */
double rpg_mvr(double dividend, double divisor);

/**
 * Round value with half-adjust
 * @param value Value to round
 * @param decimals Number of decimal places
 * @return Rounded value
 */
double rpg_half_adjust(double value, int decimals);

/* ============================================================================
 * ARRAY OPERATIONS
 * ============================================================================ */

/**
 * Lookup value in array
 * @param array Array to search
 * @param count Number of elements
 * @param size Size of each element
 * @param key Key to search for
 * @param compare Comparison function
 * @return Index of match (0-based) or -1 if not found
 */
int rpg_lookup(const void* array, int count, int size, const void* key,
               int (*compare)(const void*, const void*));

/**
 * Sum numeric array (cross-foot)
 * @param array Array of doubles
 * @param count Number of elements
 * @return Sum of all elements
 */
double rpg_xfoot(const double* array, int count);

/**
 * Sort array
 * @param array Array to sort
 * @param count Number of elements
 * @param size Size of each element
 * @param compare Comparison function
 */
void rpg_sorta(void* array, int count, int size,
               int (*compare)(const void*, const void*));

/**
 * Move array
 * @param dest Destination array
 * @param src Source array
 * @param count Number of elements
 * @param size Size of each element
 */
void rpg_movea(void* dest, const void* src, int count, int size);

/* ============================================================================
 * FILE OPERATIONS
 * ============================================================================ */

/* File handle structure (opaque) */
typedef struct RPG_FILE RPG_FILE;

/* File open modes */
#define RPG_MODE_INPUT    "r"
#define RPG_MODE_OUTPUT   "w"
#define RPG_MODE_UPDATE   "r+"
#define RPG_MODE_APPEND   "a"

/* File status codes */
#define RPG_FILE_OK       0
#define RPG_FILE_EOF      1
#define RPG_FILE_ERROR    -1
#define RPG_FILE_NOT_FOUND -2
#define RPG_FILE_LOCKED   -3

/**
 * Open file
 * @param filename File name
 * @param mode Open mode (RPG_MODE_*)
 * @param reclen Record length (0 for variable)
 * @return File handle or NULL on error
 */
RPG_FILE* rpg_open(const char* filename, const char* mode, int reclen);

/**
 * Read next record
 * @param file File handle
 * @param buffer Buffer to read into
 * @return RPG_FILE_OK, RPG_FILE_EOF, or RPG_FILE_ERROR
 */
int rpg_read(RPG_FILE* file, void* buffer);

/**
 * Read record with matching key
 * @param file File handle
 * @param buffer Buffer to read into
 * @param key Key to match
 * @return RPG_FILE_OK, RPG_FILE_EOF, or RPG_FILE_ERROR
 */
int rpg_reade(RPG_FILE* file, void* buffer, const void* key);

/**
 * Read previous record
 * @param file File handle
 * @param buffer Buffer to read into
 * @return RPG_FILE_OK, RPG_FILE_EOF, or RPG_FILE_ERROR
 */
int rpg_readp(RPG_FILE* file, void* buffer);

/**
 * Read previous record with matching key
 * @param file File handle
 * @param buffer Buffer to read into
 * @param key Key to match
 * @return RPG_FILE_OK, RPG_FILE_EOF, or RPG_FILE_ERROR
 */
int rpg_readpe(RPG_FILE* file, void* buffer, const void* key);

/**
 * Random access by key (CHAIN)
 * @param file File handle
 * @param buffer Buffer to read into
 * @param key Key to find
 * @return RPG_FILE_OK or RPG_FILE_ERROR
 */
int rpg_chain(RPG_FILE* file, void* buffer, const void* key);

/**
 * Write new record
 * @param file File handle
 * @param buffer Buffer containing record
 * @return RPG_FILE_OK or RPG_FILE_ERROR
 */
int rpg_write(RPG_FILE* file, const void* buffer);

/**
 * Update current record
 * @param file File handle
 * @param buffer Buffer containing updated record
 * @return RPG_FILE_OK or RPG_FILE_ERROR
 */
int rpg_update(RPG_FILE* file, const void* buffer);

/**
 * Delete current record
 * @param file File handle
 * @return RPG_FILE_OK or RPG_FILE_ERROR
 */
int rpg_delete(RPG_FILE* file);

/**
 * Set lower limit (position before key)
 * @param file File handle
 * @param key Key to position at
 * @return RPG_FILE_OK or RPG_FILE_ERROR
 */
int rpg_setll(RPG_FILE* file, const void* key);

/**
 * Set greater than (position after key)
 * @param file File handle
 * @param key Key to position after
 * @return RPG_FILE_OK or RPG_FILE_ERROR
 */
int rpg_setgt(RPG_FILE* file, const void* key);

/**
 * Close file
 * @param file File handle
 */
void rpg_close(RPG_FILE* file);

/**
 * Check if record was found (after CHAIN, SETLL, etc.)
 * @param file File handle
 * @return true if record found, false otherwise
 */
bool rpg_found(RPG_FILE* file);

/**
 * Check if end of file reached
 * @param file File handle
 * @return true if EOF, false otherwise
 */
bool rpg_eof(RPG_FILE* file);

/**
 * Check if file operation had error
 * @param file File handle
 * @return true if error, false otherwise
 */
bool rpg_error(RPG_FILE* file);

/* ============================================================================
 * INDICATOR OPERATIONS
 * ============================================================================ */

/* Global indicator array */
extern bool IND[100];

/* Special indicators */
extern bool IN_LR;  /* Last Record */
extern bool IN_1P;  /* First Page */
extern bool IN_MR;  /* Matching Record */
extern bool IN_L[10]; /* Level indicators L1-L9 */

/* Function key indicators (for display files) */
extern bool IN_KA, IN_KB, IN_KC, IN_KD, IN_KE, IN_KF, IN_KG, IN_KH;
extern bool IN_KI, IN_KJ, IN_KK, IN_KL, IN_KM, IN_KN, IN_KP, IN_KQ;
extern bool IN_KR, IN_KS, IN_KT, IN_KU, IN_KV, IN_KW, IN_KX, IN_KY;

/**
 * Set indicator on
 * @param ind Indicator number (1-99)
 */
void rpg_seton(int ind);

/**
 * Set indicator off
 * @param ind Indicator number (1-99)
 */
void rpg_setof(int ind);

/**
 * Test indicator
 * @param ind Indicator number (1-99)
 * @return true if on, false if off
 */
bool rpg_test_ind(int ind);

/* ============================================================================
 * DATE/TIME OPERATIONS
 * ============================================================================ */

/* Date structure */
typedef struct {
    int year;
    int month;
    int day;
} rpg_date;

/* Time structure */
typedef struct {
    int hour;
    int minute;
    int second;
} rpg_time;

/* Timestamp structure */
typedef struct {
    rpg_date date;
    rpg_time time;
    int microsecond;
} rpg_timestamp;

/**
 * Get current date
 * @param date Pointer to date structure
 */
void rpg_current_date(rpg_date* date);

/**
 * Get current time
 * @param time Pointer to time structure
 */
void rpg_current_time(rpg_time* time);

/**
 * Get current timestamp
 * @param ts Pointer to timestamp structure
 */
void rpg_current_timestamp(rpg_timestamp* ts);

/**
 * Add duration to date
 * @param result Result date
 * @param date Source date
 * @param years Years to add
 * @param months Months to add
 * @param days Days to add
 */
void rpg_adddur_date(rpg_date* result, const rpg_date* date, 
                     int years, int months, int days);

/**
 * Subtract duration from date
 * @param result Result date
 * @param date Source date
 * @param years Years to subtract
 * @param months Months to subtract
 * @param days Days to subtract
 */
void rpg_subdur_date(rpg_date* result, const rpg_date* date,
                     int years, int months, int days);

/**
 * Extract date component
 * @param date Source date
 * @param component Component to extract ('Y'=year, 'M'=month, 'D'=day)
 * @return Extracted value
 */
int rpg_extract_date(const rpg_date* date, char component);

/* ============================================================================
 * ERROR HANDLING
 * ============================================================================ */

/* Error codes */
#define RPG_ERR_NONE          0
#define RPG_ERR_FILE_NOT_FOUND 1001
#define RPG_ERR_FILE_IO       1002
#define RPG_ERR_FILE_LOCKED   1003
#define RPG_ERR_DIVIDE_ZERO   2001
#define RPG_ERR_NUMERIC_OVERFLOW 2002
#define RPG_ERR_ARRAY_BOUNDS  3001
#define RPG_ERR_STRING_OVERFLOW 3002
#define RPG_ERR_INVALID_DATE  4001
#define RPG_ERR_INVALID_PARAM 5001

/**
 * Get last error code
 * @return Error code
 */
int rpg_get_error(void);

/**
 * Get last error message
 * @return Error message string
 */
const char* rpg_get_error_message(void);

/**
 * Clear error
 */
void rpg_clear_error(void);

/**
 * Set error
 * @param code Error code
 * @param message Error message
 */
void rpg_set_error(int code, const char* message);

/* ============================================================================
 * UTILITY FUNCTIONS
 * ============================================================================ */

/**
 * Initialize RPG runtime
 * Call once at program start
 */
void rpg_init(void);

/**
 * Cleanup RPG runtime
 * Call once at program end
 */
void rpg_cleanup(void);

/**
 * Get runtime version string
 * @return Version string (e.g., "1.0.0")
 */
const char* rpg_version(void);

#ifdef __cplusplus
}
#endif

#endif /* RPG_RUNTIME_H */

// Made with Bob
