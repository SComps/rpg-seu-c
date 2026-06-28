/**
 * RPG File Operations
 * 
 * File I/O support for RPG II programs
 * Supports sequential, indexed (keyed), and relative files
 * 
 * Copyright (c) 2026
 * License: MIT
 */

#ifndef RPG_FILE_H
#define RPG_FILE_H

#include <stdio.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * FILE TYPES
 * ============================================================================ */

typedef enum {
    RPG_FILE_SEQUENTIAL,    /* Sequential access */
    RPG_FILE_INDEXED,       /* Keyed access (SQLite-based) */
    RPG_FILE_RELATIVE       /* Direct access by record number */
} rpg_file_type;

/* ============================================================================
 * FILE STRUCTURE
 * ============================================================================ */

typedef struct RPG_FILE {
    char filename[256];
    FILE* fp;
    rpg_file_type type;
    int record_length;
    int key_length;
    long current_position;
    bool eof_flag;
    bool found_flag;
    bool error_flag;
    char mode[4];
    void* index_data;  /* For indexed files (SQLite handle) */
} RPG_FILE;

/* ============================================================================
 * SEQUENTIAL FILE OPERATIONS
 * ============================================================================ */

/**
 * Open sequential file
 */
RPG_FILE* rpg_open_sequential(const char* filename, const char* mode, int reclen);

/**
 * Read next record from sequential file
 */
int rpg_read_sequential(RPG_FILE* file, void* buffer);

/**
 * Write record to sequential file
 */
int rpg_write_sequential(RPG_FILE* file, const void* buffer);

/* ============================================================================
 * INDEXED FILE OPERATIONS (SQLite-based)
 * ============================================================================ */

/**
 * Open indexed (keyed) file
 */
RPG_FILE* rpg_open_indexed(const char* filename, const char* mode, 
                           int reclen, int keylen);

/**
 * Chain to record by key
 */
int rpg_chain_indexed(RPG_FILE* file, void* buffer, const void* key);

/**
 * Read next record with matching key
 */
int rpg_reade_indexed(RPG_FILE* file, void* buffer, const void* key);

/**
 * Read previous record
 */
int rpg_readp_indexed(RPG_FILE* file, void* buffer);

/**
 * Read previous record with matching key
 */
int rpg_readpe_indexed(RPG_FILE* file, void* buffer, const void* key);

/**
 * Set lower limit (position before key)
 */
int rpg_setll_indexed(RPG_FILE* file, const void* key);

/**
 * Set greater than (position after key)
 */
int rpg_setgt_indexed(RPG_FILE* file, const void* key);

/**
 * Update current record
 */
int rpg_update_indexed(RPG_FILE* file, const void* buffer);

/**
 * Delete current record
 */
int rpg_delete_indexed(RPG_FILE* file);

/**
 * Write new record
 */
int rpg_write_indexed(RPG_FILE* file, const void* buffer, const void* key);

/* ============================================================================
 * RELATIVE FILE OPERATIONS
 * ============================================================================ */

/**
 * Open relative file
 */
RPG_FILE* rpg_open_relative(const char* filename, const char* mode, int reclen);

/**
 * Read record by number
 */
int rpg_read_relative(RPG_FILE* file, void* buffer, long recnum);

/**
 * Write record by number
 */
int rpg_write_relative(RPG_FILE* file, const void* buffer, long recnum);

/**
 * Update record by number
 */
int rpg_update_relative(RPG_FILE* file, const void* buffer, long recnum);

/**
 * Delete record by number
 */

/* ============================================================================
 * COMMON FILE OPERATIONS
 * ============================================================================ */

/**
 * Close file
 */
void rpg_close_file(RPG_FILE* file);
int rpg_delete_relative(RPG_FILE* file, long recnum);

#ifdef __cplusplus
}
#endif

#endif /* RPG_FILE_H */

// Made with Bob
