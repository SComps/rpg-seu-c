/*
 * RPG Runtime Library - Indexed File Operations Test Suite
 * Tests SQLite-based indexed (keyed) file I/O operations
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "../rpg_runtime.h"

#define TEST_IDX_FILE "test_indexed.dat"
#define RECORD_LENGTH 80
#define KEY_LENGTH 10

// Test record structure
typedef struct {
    char key[10];
    char name[30];
    char data[40];
} TestRecord;

// Helper function to initialize a test record
void init_test_record(TestRecord* rec, const char* key, const char* name, const char* data) {
    memset(rec, ' ', sizeof(TestRecord));
    strncpy(rec->key, key, sizeof(rec->key));
    strncpy(rec->name, name, sizeof(rec->name) - 1);
    strncpy(rec->data, data, sizeof(rec->data) - 1);
}

// Helper function to compare records
int compare_records(const TestRecord* r1, const TestRecord* r2) {
    return memcmp(r1, r2, sizeof(TestRecord)) == 0;
}

// Helper function to print record
void print_record(const char* label, const TestRecord* rec) {
    char key[11], name[31], data[41];
    memcpy(key, rec->key, 10); key[10] = '\0';
    memcpy(name, rec->name, 30); name[30] = '\0';
    memcpy(data, rec->data, 40); data[40] = '\0';
    printf("  %s: Key='%s' Name='%s' Data='%s'\n", label, key, name, data);
}

// Test indexed file write
int test_indexed_write() {
    printf("\nTesting indexed file write...\n");
    
    RPG_FILE* file = rpg_open_indexed(TEST_IDX_FILE, "w", sizeof(TestRecord), KEY_LENGTH);
    if (!file) {
        printf("  ✗ Failed to open indexed file for writing\n");
        return 0;
    }
    
    // Write test records with different keys
    const char* keys[] = {"KEY001", "KEY005", "KEY003", "KEY007", "KEY002"};
    TestRecord rec;
    
    for (int i = 0; i < 5; i++) {
        char name[30], data[40];
        snprintf(name, sizeof(name), "Record %s", keys[i]);
        snprintf(data, sizeof(data), "Test data for %s", keys[i]);
        init_test_record(&rec, keys[i], name, data);
        
        if (rpg_write_indexed(file, &rec, keys[i]) != 0) {
            printf("  ✗ Failed to write record with key %s\n", keys[i]);
            rpg_close_file(file);
            return 0;
        }
    }
    
    rpg_close_file(file);
    printf("  ✓ Wrote 5 records with different keys\n");
    return 1;
}

// Test CHAIN operation
int test_chain() {
    printf("\nTesting CHAIN operation...\n");
    
    RPG_FILE* file = rpg_open_indexed(TEST_IDX_FILE, "r", sizeof(TestRecord), KEY_LENGTH);
    if (!file) {
        printf("  ✗ Failed to open indexed file for reading\n");
        return 0;
    }
    
    // Chain to specific key
    TestRecord rec, expected;
    const char* key = "KEY003";
    
    if (rpg_chain_indexed(file, &rec, key) != 0) {
        printf("  ✗ Failed to CHAIN to key %s\n", key);
        rpg_close_file(file);
        return 0;
    }
    
    // Verify the record
    init_test_record(&expected, key, "Record KEY003", "Test data for KEY003");
    if (!compare_records(&rec, &expected)) {
        printf("  ✗ CHAIN returned wrong record\n");
        print_record("Expected", &expected);
        print_record("Got", &rec);
        rpg_close_file(file);
        return 0;
    }
    
    rpg_close_file(file);
    printf("  ✓ CHAIN successfully retrieved record by key\n");
    return 1;
}

// Test SETLL operation
int test_setll() {
    printf("\nTesting SETLL operation...\n");
    
    RPG_FILE* file = rpg_open_indexed(TEST_IDX_FILE, "r", sizeof(TestRecord), KEY_LENGTH);
    if (!file) {
        printf("  ✗ Failed to open indexed file\n");
        return 0;
    }
    
    // Position at or after KEY003
    const char* key = "KEY003";
    if (rpg_setll_indexed(file, key) != 0) {
        printf("  ✗ SETLL failed\n");
        rpg_close_file(file);
        return 0;
    }
    
    if (!file->found_flag) {
        printf("  ✗ SETLL did not find position\n");
        rpg_close_file(file);
        return 0;
    }
    
    rpg_close_file(file);
    printf("  ✓ SETLL positioned correctly\n");
    return 1;
}

// Test SETGT operation
int test_setgt() {
    printf("\nTesting SETGT operation...\n");
    
    RPG_FILE* file = rpg_open_indexed(TEST_IDX_FILE, "r", sizeof(TestRecord), KEY_LENGTH);
    if (!file) {
        printf("  ✗ Failed to open indexed file\n");
        return 0;
    }
    
    // Position after KEY003
    const char* key = "KEY003";
    if (rpg_setgt_indexed(file, key) != 0) {
        printf("  ✗ SETGT failed\n");
        rpg_close_file(file);
        return 0;
    }
    
    if (!file->found_flag) {
        printf("  ✗ SETGT did not find position\n");
        rpg_close_file(file);
        return 0;
    }
    
    rpg_close_file(file);
    printf("  ✓ SETGT positioned correctly\n");
    return 1;
}

// Test UPDATE operation
int test_update() {
    printf("\nTesting UPDATE operation...\n");
    
    RPG_FILE* file = rpg_open_indexed(TEST_IDX_FILE, "r+", sizeof(TestRecord), KEY_LENGTH);
    if (!file) {
        printf("  ✗ Failed to open indexed file for update\n");
        return 0;
    }
    
    // Chain to record and update it
    TestRecord rec;
    const char* key = "KEY005";
    
    if (rpg_chain_indexed(file, &rec, key) != 0) {
        printf("  ✗ Failed to CHAIN before update\n");
        rpg_close_file(file);
        return 0;
    }
    
    // Modify the record
    init_test_record(&rec, key, "Updated Record", "This record was updated");
    
    if (rpg_update_indexed(file, &rec) != 0) {
        printf("  ✗ Failed to UPDATE record\n");
        rpg_close_file(file);
        return 0;
    }
    
    // Read it back to verify
    TestRecord verify;
    if (rpg_chain_indexed(file, &verify, key) != 0) {
        printf("  ✗ Failed to read updated record\n");
        rpg_close_file(file);
        return 0;
    }
    
    if (!compare_records(&rec, &verify)) {
        printf("  ✗ Updated record mismatch\n");
        print_record("Expected", &rec);
        print_record("Got", &verify);
        rpg_close_file(file);
        return 0;
    }
    
    rpg_close_file(file);
    printf("  ✓ UPDATE modified record successfully\n");
    return 1;
}

// Test DELETE operation
int test_delete() {
    printf("\nTesting DELETE operation...\n");
    
    RPG_FILE* file = rpg_open_indexed(TEST_IDX_FILE, "r+", sizeof(TestRecord), KEY_LENGTH);
    if (!file) {
        printf("  ✗ Failed to open indexed file for delete\n");
        return 0;
    }
    
    // Chain to record and delete it
    TestRecord rec;
    const char* key = "KEY007";
    
    if (rpg_chain_indexed(file, &rec, key) != 0) {
        printf("  ✗ Failed to CHAIN before delete\n");
        rpg_close_file(file);
        return 0;
    }
    
    if (rpg_delete_indexed(file) != 0) {
        printf("  ✗ Failed to DELETE record\n");
        rpg_close_file(file);
        return 0;
    }
    
    // Try to read deleted record (should fail)
    int result = rpg_chain_indexed(file, &rec, key);
    
    rpg_close_file(file);
    
    if (result == 0) {
        printf("  ✗ Deleted record still accessible\n");
        return 0;
    }
    
    printf("  ✓ DELETE removed record successfully\n");
    return 1;
}

// Test READP operation
int test_readp() {
    printf("\nTesting READP operation...\n");
    
    RPG_FILE* file = rpg_open_indexed(TEST_IDX_FILE, "r", sizeof(TestRecord), KEY_LENGTH);
    if (!file) {
        printf("  ✗ Failed to open indexed file\n");
        return 0;
    }
    
    // Position at KEY005
    TestRecord rec;
    const char* key = "KEY005";
    
    if (rpg_chain_indexed(file, &rec, key) != 0) {
        printf("  ✗ Failed to CHAIN to starting position\n");
        rpg_close_file(file);
        return 0;
    }
    
    // Read previous record (should be KEY003)
    if (rpg_readp_indexed(file, &rec) != 0) {
        printf("  ✗ READP failed\n");
        rpg_close_file(file);
        return 0;
    }
    
    // Verify we got KEY003
    if (memcmp(rec.key, "KEY003", 6) != 0) {
        printf("  ✗ READP returned wrong record\n");
        print_record("Got", &rec);
        rpg_close_file(file);
        return 0;
    }
    
    rpg_close_file(file);
    printf("  ✓ READP read previous record successfully\n");
    return 1;
}

// Test error conditions
int test_error_conditions() {
    printf("\nTesting error conditions...\n");
    
    // Test opening with invalid parameters
    RPG_FILE* file = rpg_open_indexed(NULL, "r", 80, 10);
    if (file != NULL) {
        printf("  ✗ Should fail with NULL filename\n");
        rpg_close_file(file);
        return 0;
    }
    printf("  ✓ Correctly rejected NULL filename\n");
    
    // Test CHAIN with non-existent key
    file = rpg_open_indexed(TEST_IDX_FILE, "r", sizeof(TestRecord), KEY_LENGTH);
    if (file) {
        TestRecord rec;
        const char* key = "NOKEY";
        int result = rpg_chain_indexed(file, &rec, key);
        if (result == 0) {
            printf("  ✗ Should fail to CHAIN to non-existent key\n");
            rpg_close_file(file);
            return 0;
        }
        rpg_close_file(file);
        printf("  ✓ Correctly failed to CHAIN to non-existent key\n");
    }
    
    return 1;
}

// Cleanup test files
void cleanup_test_files() {
    unlink(TEST_IDX_FILE);
    char db_name[256];
    snprintf(db_name, sizeof(db_name), "%s.db", TEST_IDX_FILE);
    unlink(db_name);
}

int main() {
    int passed = 0;
    int total = 0;
    
    printf("===========================================\n");
    printf("RPG Indexed File Operations Test Suite\n");
    printf("===========================================\n");
    
    // Initialize runtime
    rpg_init();
    
    // Clean up any existing test files
    cleanup_test_files();
    
    // Run indexed file tests
    total++; if (test_indexed_write()) passed++;
    total++; if (test_chain()) passed++;
    total++; if (test_setll()) passed++;
    total++; if (test_setgt()) passed++;
    total++; if (test_update()) passed++;
    total++; if (test_delete()) passed++;
    total++; if (test_readp()) passed++;
    total++; if (test_error_conditions()) passed++;
    
    // Cleanup
    rpg_cleanup();
    cleanup_test_files();
    
    printf("\n===========================================\n");
    if (passed == total) {
        printf("✓ All %d tests passed!\n", total);
        printf("===========================================\n");
        return 0;
    } else {
        printf("✗ %d of %d tests failed\n", total - passed, total);
        printf("===========================================\n");
        return 1;
    }
}

// Made with Bob
