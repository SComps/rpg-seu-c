/**
 * RPG Runtime Library Implementation
 * 
 * Core runtime support for RPG II programs transpiled to C
 * 
 * Copyright (c) 2026
 * License: MIT
 */

#include "rpg_runtime.h"
#include "rpg_file.h"
#include <math.h>
#include <ctype.h>
#include <errno.h>
#include <unistd.h>
#include <sys/file.h>

/* ============================================================================
 * GLOBAL STATE
 * ============================================================================ */

/* Indicator arrays */
bool IND[100] = {false};
bool IN_LR = false;
bool IN_1P = true;
bool IN_MR = false;
bool IN_L[10] = {false};

/* Function key indicators */
bool IN_KA = false, IN_KB = false, IN_KC = false, IN_KD = false;
bool IN_KE = false, IN_KF = false, IN_KG = false, IN_KH = false;
bool IN_KI = false, IN_KJ = false, IN_KK = false, IN_KL = false;
bool IN_KM = false, IN_KN = false, IN_KP = false, IN_KQ = false;
bool IN_KR = false, IN_KS = false, IN_KT = false, IN_KU = false;
bool IN_KV = false, IN_KW = false, IN_KX = false, IN_KY = false;

/* Error state */
static int last_error_code = RPG_ERR_NONE;
static char last_error_message[256] = {0};

/* Runtime initialized flag */
static bool runtime_initialized = false;

/* ============================================================================
 * DECIMAL OPERATIONS
 * ============================================================================ */

/**
 * Decode packed decimal (BCD) format to double
 * 
 * Packed decimal format:
 * - Each byte contains two decimal digits (nibbles)
 * - Last nibble contains sign (C=positive, D=negative, F=unsigned)
 * - Example: 12345+ = 0x12 0x34 0x5C
 */
double rpg_decode_packed(const unsigned char* buf, int start, int len, int decimals) {
    if (!buf || len <= 0) {
        rpg_set_error(RPG_ERR_NUMERIC_OVERFLOW, "Invalid packed decimal buffer");
        return 0.0;
    }
    
    double val = 0.0;
    int sign = 1;
    
    /* Process all bytes except the last */
    for (int i = 0; i < len - 1; i++) {
        unsigned char b = buf[start + i];
        int high = (b >> 4) & 0x0F;
        int low = b & 0x0F;
        
        if (high > 9 || low > 9) {
            rpg_set_error(RPG_ERR_NUMERIC_OVERFLOW, "Invalid packed decimal digit");
            return 0.0;
        }
        
        val = val * 100.0 + (high * 10.0) + low;
    }
    
    /* Process last byte (contains one digit and sign) */
    unsigned char last = buf[start + len - 1];
    int digit = (last >> 4) & 0x0F;
    int sign_nibble = last & 0x0F;
    
    if (digit > 9) {
        rpg_set_error(RPG_ERR_NUMERIC_OVERFLOW, "Invalid packed decimal digit");
        return 0.0;
    }
    
    val = val * 10.0 + digit;
    
    /* Check sign nibble */
    if (sign_nibble == 0x0D || sign_nibble == 0x0B) {
        sign = -1;
    }
    
    /* Apply decimal places */
    for (int i = 0; i < decimals; i++) {
        val /= 10.0;
    }
    
    return val * sign;
}

/**
 * Encode double to packed decimal (BCD) format
 */
void rpg_encode_packed(unsigned char* buf, int start, int len, int decimals, double value) {
    if (!buf || len <= 0) {
        rpg_set_error(RPG_ERR_NUMERIC_OVERFLOW, "Invalid packed decimal buffer");
        return;
    }
    
    /* Determine sign */
    int sign = (value < 0) ? 0x0D : 0x0C;
    value = fabs(value);
    
    /* Scale by decimal places */
    for (int i = 0; i < decimals; i++) {
        value *= 10.0;
    }
    
    /* Round to nearest integer */
    long long ival = (long long)(value + 0.5);
    
    /* Extract digits from right to left */
    /* Last byte: one digit + sign */
    int digit = ival % 10;
    ival /= 10;
    buf[start + len - 1] = (digit << 4) | sign;
    
    /* Remaining bytes: two digits each */
    for (int i = len - 2; i >= 0; i--) {
        int low = ival % 10;
        ival /= 10;
        int high = ival % 10;
        ival /= 10;
        buf[start + i] = (high << 4) | low;
    }
}

/**
 * Decode zoned decimal (EBCDIC numeric) to double
 * 
 * Zoned decimal format:
 * - Each byte is an EBCDIC digit (0xF0-0xF9)
 * - Last byte contains sign in high nibble (C=positive, D=negative)
 * - Example: 12345+ = 0xF1 0xF2 0xF3 0xF4 0xC5
 */
double rpg_decode_zoned(const char* buf, int len, int decimals) {
    if (!buf || len <= 0) {
        rpg_set_error(RPG_ERR_NUMERIC_OVERFLOW, "Invalid zoned decimal buffer");
        return 0.0;
    }
    
    double val = 0.0;
    int sign = 1;
    
    /* Process all bytes except the last */
    for (int i = 0; i < len - 1; i++) {
        unsigned char b = (unsigned char)buf[i];
        int digit = b & 0x0F;
        
        if (digit > 9) {
            rpg_set_error(RPG_ERR_NUMERIC_OVERFLOW, "Invalid zoned decimal digit");
            return 0.0;
        }
        
        val = val * 10.0 + digit;
    }
    
    /* Process last byte (contains digit and sign) */
    unsigned char last = (unsigned char)buf[len - 1];
    int digit = last & 0x0F;
    int sign_nibble = (last >> 4) & 0x0F;
    
    if (digit > 9) {
        rpg_set_error(RPG_ERR_NUMERIC_OVERFLOW, "Invalid zoned decimal digit");
        return 0.0;
    }
    
    val = val * 10.0 + digit;
    
    /* Check sign nibble (D=negative, C or F=positive) */
    if (sign_nibble == 0x0D) {
        sign = -1;
    }
    
    /* Apply decimal places */
    for (int i = 0; i < decimals; i++) {
        val /= 10.0;
    }
    
    return val * sign;
}

/**
 * Encode double to zoned decimal (EBCDIC numeric) format
 */
void rpg_encode_zoned(char* buf, int len, int decimals, double value) {
    if (!buf || len <= 0) {
        rpg_set_error(RPG_ERR_NUMERIC_OVERFLOW, "Invalid zoned decimal buffer");
        return;
    }
    
    /* Determine sign */
    int sign = (value < 0) ? 0xD0 : 0xC0;
    value = fabs(value);
    
    /* Scale by decimal places */
    for (int i = 0; i < decimals; i++) {
        value *= 10.0;
    }
    
    /* Round to nearest integer */
    long long ival = (long long)(value + 0.5);
    
    /* Extract digits from right to left */
    /* Last byte: digit + sign */
    int digit = ival % 10;
    ival /= 10;
    buf[len - 1] = sign | digit;
    
    /* Remaining bytes: digit + 0xF0 */
    for (int i = len - 2; i >= 0; i--) {
        digit = ival % 10;
        ival /= 10;
        buf[i] = 0xF0 | digit;
    }
}

/**
 * Format numeric value with edit code
 */
void rpg_format_edit(char* dest, double val, char code, int len, int dec) {
    if (!dest) return;
    
    char fmt[32];
    char tmp[128];
    
    switch (code) {
        case 'Z': /* Zero suppress */
            if (val == 0.0) {
                memset(dest, ' ', len);
                dest[len] = '\0';
                return;
            }
            snprintf(fmt, sizeof(fmt), "%%.%df", dec);
            snprintf(tmp, sizeof(tmp), fmt, val);
            /* Remove leading zeros */
            {
                int start = 0;
                while (tmp[start] == '0' || tmp[start] == ' ') start++;
                if (tmp[start] == '.') start--; /* Keep one zero before decimal */
                snprintf(dest, len + 1, "%-*s", len, tmp + start);
            }
            break;
            
        case '1': /* Edit with commas, no sign */
        case '2': /* Edit with commas and CR */
        case '3': /* Edit with commas and minus */
        case '4': /* Edit with commas and $ */
            /* TODO: Implement full edit code support */
            snprintf(fmt, sizeof(fmt), "%%*.%df", dec);
            snprintf(dest, len + 1, fmt, len, val);
            break;
            
        default: /* No edit */
            snprintf(fmt, sizeof(fmt), "%%*.%df", dec);
            snprintf(dest, len + 1, fmt, len, val);
            break;
    }
}

/* ============================================================================
 * STRING OPERATIONS
 * ============================================================================ */

/**
 * Concatenate strings with optional blanks
 */
void rpg_cat(char* dest, const char* src1, const char* src2, int blanks) {
    if (!dest || !src1 || !src2) return;
    
    strcpy(dest, src1);
    
    /* Add blanks */
    int len = strlen(dest);
    for (int i = 0; i < blanks; i++) {
        dest[len++] = ' ';
    }
    dest[len] = '\0';
    
    /* Append second string */
    strcat(dest, src2);
}

/**
 * Extract substring
 */
void rpg_subst(char* dest, const char* src, int start, int len) {
    if (!dest || !src || start < 1 || len < 0) {
        if (dest) dest[0] = '\0';
        return;
    }
    
    int src_len = strlen(src);
    int start_idx = start - 1; /* Convert to 0-based */
    
    if (start_idx >= src_len) {
        dest[0] = '\0';
        return;
    }
    
    int copy_len = (start_idx + len > src_len) ? (src_len - start_idx) : len;
    strncpy(dest, src + start_idx, copy_len);
    dest[copy_len] = '\0';
}

/**
 * Scan for substring
 */
int rpg_scan(const char* search, const char* source, int start) {
    if (!search || !source || start < 1) return 0;
    
    int source_len = strlen(source);
    int start_idx = start - 1; /* Convert to 0-based */
    
    if (start_idx >= source_len) return 0;
    
    const char* found = strstr(source + start_idx, search);
    if (!found) return 0;
    
    return (int)(found - source) + 1; /* Convert to 1-based */
}

/**
 * Translate characters
 */
void rpg_xlate(char* dest, const char* src, const char* from, const char* to) {
    if (!dest || !src || !from || !to) {
        if (dest) dest[0] = '\0';
        return;
    }
    
    int from_len = strlen(from);
    int to_len = strlen(to);
    
    strcpy(dest, src);
    
    for (int i = 0; dest[i]; i++) {
        for (int j = 0; j < from_len && j < to_len; j++) {
            if (dest[i] == from[j]) {
                dest[i] = to[j];
                break;
            }
        }
    }
}

/**
 * Trim trailing blanks
 */
void rpg_trim(char* dest, const char* src) {
    if (!dest || !src) {
        if (dest) dest[0] = '\0';
        return;
    }
    
    strcpy(dest, src);
    int len = strlen(dest);
    
    while (len > 0 && dest[len - 1] == ' ') {
        len--;
    }
    dest[len] = '\0';
}

/**
 * Trim leading blanks
 */
void rpg_triml(char* dest, const char* src) {
    if (!dest || !src) {
        if (dest) dest[0] = '\0';
        return;
    }
    
    while (*src == ' ') src++;
    strcpy(dest, src);
}

/**
 * Trim leading and trailing blanks
 */
void rpg_trimb(char* dest, const char* src) {
    if (!dest || !src) {
        if (dest) dest[0] = '\0';
        return;
    }
    
    /* Trim leading */
    while (*src == ' ') src++;
    strcpy(dest, src);
    
    /* Trim trailing */
    int len = strlen(dest);
    while (len > 0 && dest[len - 1] == ' ') {
        len--;
    }
    dest[len] = '\0';
}

/* ============================================================================
 * MATH OPERATIONS
 * ============================================================================ */

/**
 * Square root
 */
double rpg_sqrt(double value) {
    if (value < 0.0) {
        rpg_set_error(RPG_ERR_NUMERIC_OVERFLOW, "Square root of negative number");
        return 0.0;
    }
    return sqrt(value);
}

/**
 * Move remainder (from last DIV operation)
 */
double rpg_mvr(double dividend, double divisor) {
    if (divisor == 0.0) {
        rpg_set_error(RPG_ERR_DIVIDE_ZERO, "Division by zero in MVR");
        return 0.0;
    }
    return fmod(dividend, divisor);
}

/**
 * Round value with half-adjust
 */
double rpg_half_adjust(double value, int decimals) {
    double multiplier = pow(10.0, decimals);
    double shifted = value * multiplier;
    double rounded = (shifted >= 0.0) ? floor(shifted + 0.5) : ceil(shifted - 0.5);
    return rounded / multiplier;
}

/* ============================================================================
 * ARRAY OPERATIONS
 * ============================================================================ */

/**
 * Lookup value in array
 */
int rpg_lookup(const void* array, int count, int size, const void* key,
               int (*compare)(const void*, const void*)) {
    if (!array || !key || !compare || count <= 0 || size <= 0) {
        return -1;
    }
    
    const unsigned char* arr = (const unsigned char*)array;
    
    for (int i = 0; i < count; i++) {
        if (compare(arr + (i * size), key) == 0) {
            return i;
        }
    }
    
    return -1;
}

/**
 * Sum numeric array (cross-foot)
 */
double rpg_xfoot(const double* array, int count) {
    if (!array || count <= 0) return 0.0;
    
    double sum = 0.0;
    for (int i = 0; i < count; i++) {
        sum += array[i];
    }
    
    return sum;
}

/**
 * Sort array
 */
void rpg_sorta(void* array, int count, int size,
               int (*compare)(const void*, const void*)) {
    if (!array || !compare || count <= 0 || size <= 0) return;
    
    qsort(array, count, size, compare);
}

/**
 * Move array
 */
void rpg_movea(void* dest, const void* src, int count, int size) {
    if (!dest || !src || count <= 0 || size <= 0) return;
    
    memcpy(dest, src, count * size);
}

/* ============================================================================
 * INDICATOR OPERATIONS
 * ============================================================================ */

/**
 * Set indicator on
 */
void rpg_seton(int ind) {
    if (ind >= 1 && ind <= 99) {
        IND[ind] = true;
    }
}

/**
 * Set indicator off
 */
void rpg_setof(int ind) {
    if (ind >= 1 && ind <= 99) {
        IND[ind] = false;
    }
}

/**
 * Test indicator
 */
bool rpg_test_ind(int ind) {
    if (ind >= 1 && ind <= 99) {
        return IND[ind];
    }
    return false;
}

/* ============================================================================
 * DATE/TIME OPERATIONS
 * ============================================================================ */

/**
 * Get current date
 */
void rpg_current_date(rpg_date* date) {
    if (!date) return;
    
    time_t now = time(NULL);
    struct tm* tm_info = localtime(&now);
    
    date->year = tm_info->tm_year + 1900;
    date->month = tm_info->tm_mon + 1;
    date->day = tm_info->tm_mday;
}

/**
 * Get current time
 */
void rpg_current_time(rpg_time* time_val) {
    if (!time_val) return;
    
    time_t now = time(NULL);
    struct tm* tm_info = localtime(&now);
    
    time_val->hour = tm_info->tm_hour;
    time_val->minute = tm_info->tm_min;
    time_val->second = tm_info->tm_sec;
}

/**
 * Get current timestamp
 */
void rpg_current_timestamp(rpg_timestamp* ts) {
    if (!ts) return;
    
    rpg_current_date(&ts->date);
    rpg_current_time(&ts->time);
    ts->microsecond = 0; /* TODO: Get microseconds */
}

/**
 * Add duration to date
 */
void rpg_adddur_date(rpg_date* result, const rpg_date* date,
                     int years, int months, int days) {
    if (!result || !date) return;
    
    struct tm tm_info = {0};
    tm_info.tm_year = date->year - 1900;
    tm_info.tm_mon = date->month - 1;
    tm_info.tm_mday = date->day;
    
    tm_info.tm_year += years;
    tm_info.tm_mon += months;
    tm_info.tm_mday += days;
    
    mktime(&tm_info); /* Normalize */
    
    result->year = tm_info.tm_year + 1900;
    result->month = tm_info.tm_mon + 1;
    result->day = tm_info.tm_mday;
}

/**
 * Subtract duration from date
 */
void rpg_subdur_date(rpg_date* result, const rpg_date* date,
                     int years, int months, int days) {
    rpg_adddur_date(result, date, -years, -months, -days);
}

/**
 * Extract date component
 */
int rpg_extract_date(const rpg_date* date, char component) {
    if (!date) return 0;
    
    switch (component) {
        case 'Y': case 'y': return date->year;
        case 'M': case 'm': return date->month;
        case 'D': case 'd': return date->day;
        default: return 0;
    }
}

/* ============================================================================
 * ERROR HANDLING
 * ============================================================================ */

/**
 * Get last error code
 */
int rpg_get_error(void) {
    return last_error_code;
}

/**
 * Get last error message
 */
const char* rpg_get_error_message(void) {
    return last_error_message;
}

/**
 * Clear error
 */
void rpg_clear_error(void) {
    last_error_code = RPG_ERR_NONE;
    last_error_message[0] = '\0';
}

/**
 * Set error
 */
void rpg_set_error(int code, const char* message) {
    last_error_code = code;
    if (message) {
        strncpy(last_error_message, message, sizeof(last_error_message) - 1);
        last_error_message[sizeof(last_error_message) - 1] = '\0';
    } else {
        last_error_message[0] = '\0';
    }
}

/* ============================================================================
 * UTILITY FUNCTIONS
 * ============================================================================ */

/**
 * Initialize RPG runtime
 */
void rpg_init(void) {
    if (runtime_initialized) return;
    
    /* Initialize indicators */
    memset(IND, 0, sizeof(IND));
    IN_LR = false;
    IN_1P = true;
    IN_MR = false;
    memset(IN_L, 0, sizeof(IN_L));
    
    /* Initialize function key indicators */
    IN_KA = IN_KB = IN_KC = IN_KD = false;
    IN_KE = IN_KF = IN_KG = IN_KH = false;
    IN_KI = IN_KJ = IN_KK = IN_KL = false;
    IN_KM = IN_KN = IN_KP = IN_KQ = false;
    IN_KR = IN_KS = IN_KT = IN_KU = false;
    IN_KV = IN_KW = IN_KX = IN_KY = false;
    
    /* Clear error state */
    rpg_clear_error();
    
    runtime_initialized = true;
}

/**
 * Cleanup RPG runtime
 */
void rpg_cleanup(void) {
    if (!runtime_initialized) return;
    
    /* Nothing to cleanup yet */
    
    runtime_initialized = false;
}

/**
 * Get runtime version string
 */
const char* rpg_version(void) {
    static char version[32];
    snprintf(version, sizeof(version), "%d.%d.%d",
             RPG_RUNTIME_VERSION_MAJOR,
             RPG_RUNTIME_VERSION_MINOR,
             RPG_RUNTIME_VERSION_PATCH);
    return version;
}

/* ============================================================================
 * FILE OPERATIONS (Wrappers to rpg_file.c)
 * ============================================================================ */

/**
 * Open file
 */
RPG_FILE* rpg_open(const char* filename, const char* mode, int reclen) {
    if (!filename || !mode) {
        rpg_set_error(RPG_ERR_FILE_IO, "Invalid filename or mode");
        return NULL;
    }
    
    /* Default to sequential file */
    return rpg_open_sequential(filename, mode, reclen);
}

/**
 * Read next record
 */
int rpg_read(RPG_FILE* file, void* buffer) {
    if (!file || !buffer) {
        rpg_set_error(RPG_ERR_FILE_IO, "Invalid file or buffer");
        return RPG_FILE_ERROR;
    }
    
    if (file->type == RPG_FILE_SEQUENTIAL) {
        return rpg_read_sequential(file, buffer);
    } else {
        rpg_set_error(RPG_ERR_FILE_IO, "READ not supported for this file type");
        return RPG_FILE_ERROR;
    }
}

/**
 * Read record with matching key
 */
int rpg_reade(RPG_FILE* file, void* buffer, const void* key) {
    if (!file || !buffer || !key) {
        rpg_set_error(RPG_ERR_FILE_IO, "Invalid parameters");
        return RPG_FILE_ERROR;
    }
    
    if (file->type == RPG_FILE_INDEXED) {
        return rpg_reade_indexed(file, buffer, key);
    } else {
        rpg_set_error(RPG_ERR_FILE_IO, "READE only supported for indexed files");
        return RPG_FILE_ERROR;
    }
}

/**
 * Read previous record
 */
int rpg_readp(RPG_FILE* file, void* buffer) {
    if (!file || !buffer) {
        rpg_set_error(RPG_ERR_FILE_IO, "Invalid parameters");
        return RPG_FILE_ERROR;
    }
    
    if (file->type == RPG_FILE_INDEXED) {
        return rpg_readp_indexed(file, buffer);
    } else {
        rpg_set_error(RPG_ERR_FILE_IO, "READP only supported for indexed files");
        return RPG_FILE_ERROR;
    }
}

/**
 * Read previous record with matching key
 */
int rpg_readpe(RPG_FILE* file, void* buffer, const void* key) {
    if (!file || !buffer || !key) {
        rpg_set_error(RPG_ERR_FILE_IO, "Invalid parameters");
        return RPG_FILE_ERROR;
    }
    
    if (file->type == RPG_FILE_INDEXED) {
        return rpg_readpe_indexed(file, buffer, key);
    } else {
        rpg_set_error(RPG_ERR_FILE_IO, "READPE only supported for indexed files");
        return RPG_FILE_ERROR;
    }
}

/**
 * Random access by key (CHAIN)
 */
int rpg_chain(RPG_FILE* file, void* buffer, const void* key) {
    if (!file || !buffer || !key) {
        rpg_set_error(RPG_ERR_FILE_IO, "Invalid parameters");
        return RPG_FILE_ERROR;
    }
    
    if (file->type == RPG_FILE_INDEXED) {
        return rpg_chain_indexed(file, buffer, key);
    } else {
        rpg_set_error(RPG_ERR_FILE_IO, "CHAIN only supported for indexed files");
        return RPG_FILE_ERROR;
    }
}

/**
 * Write new record
 */
int rpg_write(RPG_FILE* file, const void* buffer) {
    if (!file || !buffer) {
        rpg_set_error(RPG_ERR_FILE_IO, "Invalid parameters");
        return RPG_FILE_ERROR;
    }
    
    if (file->type == RPG_FILE_SEQUENTIAL) {
        return rpg_write_sequential(file, buffer);
    } else {
        rpg_set_error(RPG_ERR_FILE_IO, "WRITE not supported for this file type");
        return RPG_FILE_ERROR;
    }
}

/**
 * Update current record
 */
int rpg_update(RPG_FILE* file, const void* buffer) {
    if (!file || !buffer) {
        rpg_set_error(RPG_ERR_FILE_IO, "Invalid parameters");
        return RPG_FILE_ERROR;
    }
    
    if (file->type == RPG_FILE_INDEXED) {
        return rpg_update_indexed(file, buffer);
    } else {
        rpg_set_error(RPG_ERR_FILE_IO, "UPDATE only supported for indexed files");
        return RPG_FILE_ERROR;
    }
}

/**
 * Delete current record
 */
int rpg_delete(RPG_FILE* file) {
    if (!file) {
        rpg_set_error(RPG_ERR_FILE_IO, "Invalid file");
        return RPG_FILE_ERROR;
    }
    
    if (file->type == RPG_FILE_INDEXED) {
        return rpg_delete_indexed(file);
    } else {
        rpg_set_error(RPG_ERR_FILE_IO, "DELETE only supported for indexed files");
        return RPG_FILE_ERROR;
    }
}

/**
 * Set lower limit (position before key)
 */
int rpg_setll(RPG_FILE* file, const void* key) {
    if (!file || !key) {
        rpg_set_error(RPG_ERR_FILE_IO, "Invalid parameters");
        return RPG_FILE_ERROR;
    }
    
    if (file->type == RPG_FILE_INDEXED) {
        return rpg_setll_indexed(file, key);
    } else {
        rpg_set_error(RPG_ERR_FILE_IO, "SETLL only supported for indexed files");
        return RPG_FILE_ERROR;
    }
}

/**
 * Set greater than (position after key)
 */
int rpg_setgt(RPG_FILE* file, const void* key) {
    if (!file || !key) {
        rpg_set_error(RPG_ERR_FILE_IO, "Invalid parameters");
        return RPG_FILE_ERROR;
    }
    
    if (file->type == RPG_FILE_INDEXED) {
        return rpg_setgt_indexed(file, key);
    } else {
        rpg_set_error(RPG_ERR_FILE_IO, "SETGT only supported for indexed files");
        return RPG_FILE_ERROR;
    }
}

/**
 * Close file
 */
void rpg_close(RPG_FILE* file) {
    if (!file) return;
    
    if (file->fp) {
        /* Release file lock if held */
        if (strcmp(file->mode, RPG_MODE_UPDATE) == 0) {
            int fd = fileno(file->fp);
            flock(fd, LOCK_UN);
        }
        fclose(file->fp);
    }
    
    if (file->index_data) {
        /* TODO: Close SQLite database */
    }
    
    free(file);
}

/**
 * Check if record was found
 */
bool rpg_found(RPG_FILE* file) {
    return file ? file->found_flag : false;
}

/**
 * Check if end of file reached
 */
bool rpg_eof(RPG_FILE* file) {
    return file ? file->eof_flag : true;
}

/**
 * Check if file operation had error
 */
bool rpg_error(RPG_FILE* file) {
    return file ? file->error_flag : true;
}
