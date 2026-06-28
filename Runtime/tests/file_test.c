/*
 * RPG Runtime Library - File Operations Test Suite
 * Tests sequential and relative file I/O operations
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "../rpg_runtime.h"

#define TEST_SEQ_FILE "test_sequential.dat"
#define TEST_REL_FILE "test_relative.dat"
#define RECORD_LENGTH 80

// Test record structure
typedef struct {
    char id[10];
    char name[30];
    char data[40];
} TestRecord;

// Helper function to initialize a test record
void init_test_record(TestRecord* rec, int id, const char* name, const char* data) {
    memset(rec, ' ', sizeof(TestRecord));
    snprintf(rec->id, sizeof(rec->id), "%09d", id);
    strncpy(rec->name, name, sizeof(rec->name) - 1);
    strncpy(rec->data, data, sizeof(rec->data) - 1);
}

// Helper function to compare records
int compare_records(const TestRecord* r1, const TestRecord* r2) {
    return memcmp(r1, r2, sizeof(TestRecord)) == 0;
}

// Helper function to print record
void print_record(const char* label, const TestRecord* rec) {
    char id[11], name[31], data[41];
    memcpy(id, rec->id, 10); id[10] = '\0';
    memcpy(name, rec->name, 30); name[30] = '\0';
    memcpy(data, rec->data, 40); data[40] = '\0';
    printf("  %s: ID='%s' Name='%s' Data='%s'\n", label, id, name, data);
}

// Test sequential file write
int test_sequential_write() {
    printf("\nTesting sequential file write...\n");
    
    RPG_FILE* file = rpg_open_sequential(TEST_SEQ_FILE, "w", sizeof(TestRecord));
    if (!file) {
        printf("  ✗ Failed to open file for writing\n");
        return 0;
    }
    
    // Write test records
    TestRecord rec;
    for (int i = 1; i <= 5; i++) {
        char name[30], data[40];
        snprintf(name, sizeof(name), "Record %d", i);
        snprintf(data, sizeof(data), "Test data for record %d", i);
        init_test_record(&rec, i, name, data);
        
        if (rpg_write_sequential(file, &rec) != 0) {
            printf("  ✗ Failed to write record %d\n", i);
            rpg_close_file(file);
            return 0;
        }
    }
    
    rpg_close_file(file);
    printf("  ✓ Wrote 5 records successfully\n");
    return 1;
}

// Test sequential file read
int test_sequential_read() {
    printf("\nTesting sequential file read...\n");
    
    RPG_FILE* file = rpg_open_sequential(TEST_SEQ_FILE, "r", sizeof(TestRecord));
    if (!file) {
        printf("  ✗ Failed to open file for reading\n");
        return 0;
    }
    
    // Read and verify records
    TestRecord rec, expected;
    int count = 0;
    
    while (rpg_read_sequential(file, &rec) == 0) {
        count++;
        char name[30], data[40];
        snprintf(name, sizeof(name), "Record %d", count);
        snprintf(data, sizeof(data), "Test data for record %d", count);
        init_test_record(&expected, count, name, data);
        
        if (!compare_records(&rec, &expected)) {
            printf("  ✗ Record %d mismatch\n", count);
            print_record("Expected", &expected);
            print_record("Got", &rec);
            rpg_close_file(file);
            return 0;
        }
    }
    
    rpg_close_file(file);
    
    if (count != 5) {
        printf("  ✗ Expected 5 records, got %d\n", count);
        return 0;
    }
    
    printf("  ✓ Read and verified 5 records\n");
    return 1;
}

// Test sequential file append
int test_sequential_append() {
    printf("\nTesting sequential file append...\n");
    
    RPG_FILE* file = rpg_open_sequential(TEST_SEQ_FILE, "a", sizeof(TestRecord));
    if (!file) {
        printf("  ✗ Failed to open file for append\n");
        return 0;
    }
    
    // Append additional records
    TestRecord rec;
    for (int i = 6; i <= 8; i++) {
        char name[30], data[40];
        snprintf(name, sizeof(name), "Record %d", i);
        snprintf(data, sizeof(data), "Appended data for record %d", i);
        init_test_record(&rec, i, name, data);
        
        if (rpg_write_sequential(file, &rec) != 0) {
            printf("  ✗ Failed to append record %d\n", i);
            rpg_close_file(file);
            return 0;
        }
    }
    
    rpg_close_file(file);
    
    // Verify total count
    file = rpg_open_sequential(TEST_SEQ_FILE, "r", sizeof(TestRecord));
    int count = 0;
    while (rpg_read_sequential(file, &rec) == 0) {
        count++;
    }
    rpg_close_file(file);
    
    if (count != 8) {
        printf("  ✗ Expected 8 records after append, got %d\n", count);
        return 0;
    }
    
    printf("  ✓ Appended 3 records, total now 8\n");
    return 1;
}

// Test relative file write
int test_relative_write() {
    printf("\nTesting relative file write...\n");
    
    RPG_FILE* file = rpg_open_relative(TEST_REL_FILE, "w", sizeof(TestRecord));
    if (!file) {
        printf("  ✗ Failed to open relative file for writing\n");
        return 0;
    }
    
    // Write records at specific positions
    TestRecord rec;
    int positions[] = {1, 5, 10, 3, 7};
    
    for (int i = 0; i < 5; i++) {
        int pos = positions[i];
        char name[30], data[40];
        snprintf(name, sizeof(name), "Relative %d", pos);
        snprintf(data, sizeof(data), "Data at position %d", pos);
        init_test_record(&rec, pos, name, data);
        
        if (rpg_write_relative(file, &rec, pos) != 0) {
            printf("  ✗ Failed to write record at position %d\n", pos);
            rpg_close_file(file);
            return 0;
        }
    }
    
    rpg_close_file(file);
    printf("  ✓ Wrote 5 records at specific positions\n");
    return 1;
}

// Test relative file read
int test_relative_read() {
    printf("\nTesting relative file read...\n");
    
    RPG_FILE* file = rpg_open_relative(TEST_REL_FILE, "r", sizeof(TestRecord));
    if (!file) {
        printf("  ✗ Failed to open relative file for reading\n");
        return 0;
    }
    
    // Read records at specific positions
    TestRecord rec, expected;
    int positions[] = {1, 5, 10, 3, 7};
    
    for (int i = 0; i < 5; i++) {
        int pos = positions[i];
        char name[30], data[40];
        snprintf(name, sizeof(name), "Relative %d", pos);
        snprintf(data, sizeof(data), "Data at position %d", pos);
        init_test_record(&expected, pos, name, data);
        
        if (rpg_read_relative(file, &rec, pos) != 0) {
            printf("  ✗ Failed to read record at position %d\n", pos);
            rpg_close_file(file);
            return 0;
        }
        
        if (!compare_records(&rec, &expected)) {
            printf("  ✗ Record at position %d mismatch\n", pos);
            print_record("Expected", &expected);
            print_record("Got", &rec);
            rpg_close_file(file);
            return 0;
        }
    }
    
    rpg_close_file(file);
    printf("  ✓ Read and verified 5 records at specific positions\n");
    return 1;
}

// Test relative file update
int test_relative_update() {
    printf("\nTesting relative file update...\n");
    
    RPG_FILE* file = rpg_open_relative(TEST_REL_FILE, "r+", sizeof(TestRecord));
    if (!file) {
        printf("  ✗ Failed to open relative file for update\n");
        return 0;
    }
    
    // Update record at position 5
    TestRecord rec;
    init_test_record(&rec, 5, "Updated Record", "This record was updated");
    
    if (rpg_update_relative(file, &rec, 5) != 0) {
        printf("  ✗ Failed to update record at position 5\n");
        rpg_close_file(file);
        return 0;
    }
    
    // Read back and verify
    TestRecord verify;
    if (rpg_read_relative(file, &verify, 5) != 0) {
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
    printf("  ✓ Updated and verified record at position 5\n");
    return 1;
}

// Test relative file delete
int test_relative_delete() {
    printf("\nTesting relative file delete...\n");
    
    RPG_FILE* file = rpg_open_relative(TEST_REL_FILE, "r+", sizeof(TestRecord));
    if (!file) {
        printf("  ✗ Failed to open relative file for delete\n");
        return 0;
    }
    
    // Delete record at position 3
    if (rpg_delete_relative(file, 3) != 0) {
        printf("  ✗ Failed to delete record at position 3\n");
        rpg_close_file(file);
        return 0;
    }
    
    // Try to read deleted record (should fail)
    TestRecord rec;
    int result = rpg_read_relative(file, &rec, 3);
    
    rpg_close_file(file);
    
    if (result == 0) {
        printf("  ✗ Deleted record still readable\n");
        return 0;
    }
    
    printf("  ✓ Deleted record at position 3\n");
    return 1;
}

// Test error conditions
int test_error_conditions() {
    printf("\nTesting error conditions...\n");
    
    // Test opening non-existent file for reading
    RPG_FILE* file = rpg_open_sequential("nonexistent.dat", "r", 80);
    if (file != NULL) {
        printf("  ✗ Should fail to open non-existent file\n");
        rpg_close_file(file);
        return 0;
    }
    printf("  ✓ Correctly failed to open non-existent file\n");
    
    // Test reading from write-only file
    file = rpg_open_sequential(TEST_SEQ_FILE, "w", sizeof(TestRecord));
    if (file) {
        TestRecord rec;
        if (rpg_read_sequential(file, &rec) == 0) {
            printf("  ✗ Should fail to read from write-only file\n");
            rpg_close_file(file);
            return 0;
        }
        rpg_close_file(file);
        printf("  ✓ Correctly failed to read from write-only file\n");
    }
    
    // Test writing to read-only file
    file = rpg_open_sequential(TEST_SEQ_FILE, "r", sizeof(TestRecord));
    if (file) {
        TestRecord rec;
        init_test_record(&rec, 999, "Test", "Test");
        if (rpg_write_sequential(file, &rec) == 0) {
            printf("  ✗ Should fail to write to read-only file\n");
            rpg_close_file(file);
            return 0;
        }
        rpg_close_file(file);
        printf("  ✓ Correctly failed to write to read-only file\n");
    }
    
    return 1;
}

// Cleanup test files
void cleanup_test_files() {
    unlink(TEST_SEQ_FILE);
    unlink(TEST_REL_FILE);
}

int main() {
    int passed = 0;
    int total = 0;
    
    printf("===========================================\n");
    printf("RPG File Operations Test Suite\n");
    printf("===========================================\n");
    
    // Initialize runtime
    rpg_init();
    
    // Clean up any existing test files
    cleanup_test_files();
    
    // Run sequential file tests
    total++; if (test_sequential_write()) passed++;
    total++; if (test_sequential_read()) passed++;
    total++; if (test_sequential_append()) passed++;
    
    // Run relative file tests
    total++; if (test_relative_write()) passed++;
    total++; if (test_relative_read()) passed++;
    total++; if (test_relative_update()) passed++;
    total++; if (test_relative_delete()) passed++;
    
    // Run error condition tests
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

