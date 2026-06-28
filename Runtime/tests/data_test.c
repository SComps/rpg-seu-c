/**
 * Test suite for RPG data structures and arrays
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "../rpg_runtime.h"
#include "../rpg_data.h"

int tests_passed = 0;
int tests_failed = 0;

#define TEST(name) \
    printf("\nTest: %s\n", #name); \
    if (test_##name()) { \
        printf("  ✓ PASSED\n"); \
        tests_passed++; \
    } else { \
        printf("  ✗ FAILED\n"); \
        tests_failed++; \
    }

/* ============================================================================
 * DATA STRUCTURE TESTS
 * ============================================================================ */

int test_create_simple_struct() {
    rpg_data_struct* ds = rpg_create_struct("CUSTOMER", false);
    assert(ds != NULL);
    assert(strcmp(ds->name, "CUSTOMER") == 0);
    assert(ds->qualified == false);
    assert(ds->field_count == 0);
    
    rpg_free_struct(ds);
    return 1;
}

int test_add_char_field() {
    rpg_data_struct* ds = rpg_create_struct("PERSON", false);
    
    int result = rpg_add_field(ds, "NAME", RPG_FIELD_CHAR, 30, 0);
    assert(result == 0);
    assert(ds->field_count == 1);
    assert(strcmp(ds->fields[0].name, "NAME") == 0);
    assert(ds->fields[0].type == RPG_FIELD_CHAR);
    assert(ds->fields[0].length == 30);
    assert(ds->fields[0].offset == 0);
    
    rpg_free_struct(ds);
    return 1;
}

int test_add_numeric_fields() {
    rpg_data_struct* ds = rpg_create_struct("INVOICE", false);
    
    rpg_add_field(ds, "INVNO", RPG_FIELD_PACKED, 10, 0);
    rpg_add_field(ds, "AMOUNT", RPG_FIELD_PACKED, 15, 2);
    rpg_add_field(ds, "QTY", RPG_FIELD_ZONED, 5, 0);
    
    assert(ds->field_count == 3);
    assert(ds->fields[0].decimals == 0);
    assert(ds->fields[1].decimals == 2);
    assert(ds->fields[2].type == RPG_FIELD_ZONED);
    
    rpg_free_struct(ds);
    return 1;
}

int test_nested_structures() {
    rpg_data_struct* address = rpg_create_struct("ADDRESS", false);
    rpg_add_field(address, "STREET", RPG_FIELD_CHAR, 40, 0);
    rpg_add_field(address, "CITY", RPG_FIELD_CHAR, 30, 0);
    rpg_add_field(address, "ZIP", RPG_FIELD_CHAR, 10, 0);
    
    rpg_data_struct* customer = rpg_create_struct("CUSTOMER", false);
    rpg_add_field(customer, "CUSTNO", RPG_FIELD_PACKED, 10, 0);
    rpg_add_field(customer, "NAME", RPG_FIELD_CHAR, 50, 0);
    rpg_add_struct_field(customer, "ADDR", address);
    
    assert(customer->field_count == 3);
    assert(customer->fields[2].type == RPG_FIELD_STRUCT);
    assert(customer->fields[2].subfields != NULL);
    
    rpg_free_struct(customer);
    return 1;
}

int test_qualified_names() {
    rpg_data_struct* ds = rpg_create_struct("ORDER", true);
    rpg_add_field(ds, "ORDNO", RPG_FIELD_PACKED, 10, 0);
    rpg_add_field(ds, "DATE", RPG_FIELD_CHAR, 8, 0);
    
    assert(ds->qualified == true);
    
    rpg_free_struct(ds);
    return 1;
}

int test_field_value_operations() {
    rpg_data_struct* ds = rpg_create_struct("PRODUCT", false);
    rpg_add_field(ds, "CODE", RPG_FIELD_CHAR, 10, 0);
    rpg_add_field(ds, "PRICE", RPG_FIELD_PACKED, 10, 2);
    
    // Allocate data buffer
    void* data = calloc(1, ds->size);
    
    // Set values
    rpg_set_field_value(data, &ds->fields[0], "ABC123");
    rpg_set_field_value(data, &ds->fields[1], "99.95");
    
    // Get values
    char buffer[100];
    rpg_get_field_value(data, &ds->fields[0], buffer, sizeof(buffer));
    assert(strncmp(buffer, "ABC123", 6) == 0);
    
    rpg_get_field_value(data, &ds->fields[1], buffer, sizeof(buffer));
    double price = atof(buffer);
    assert(price > 99.94 && price < 99.96);
    
    free(data);
    rpg_free_struct(ds);
    return 1;
}

/* ============================================================================
 * ARRAY TESTS
 * ============================================================================ */

int test_create_numeric_array() {
    rpg_array* arr = rpg_create_array(RPG_FIELD_PACKED, 10, 100, 2, false);
    
    assert(arr != NULL);
    assert(arr->element_type == RPG_FIELD_PACKED);
    assert(arr->element_size == 10);
    assert(arr->element_count == 100);
    assert(arr->decimals == 2);
    assert(arr->is_compile_time == false);
    
    rpg_free_array(arr);
    return 1;
}

int test_create_char_array() {
    rpg_array* arr = rpg_create_array(RPG_FIELD_CHAR, 20, 50, 0, false);
    
    assert(arr != NULL);
    assert(arr->element_type == RPG_FIELD_CHAR);
    assert(arr->element_size == 20);
    assert(arr->element_count == 50);
    
    rpg_free_array(arr);
    return 1;
}

int test_array_set_get() {
    rpg_array* arr = rpg_create_array(RPG_FIELD_PACKED, 10, 10, 2, false);
    
    // Encode value to packed decimal
    unsigned char packed_value[10];
    double value = 123.45;
    rpg_encode_packed(packed_value, 0, 10, 2, value);
    
    int result = rpg_array_set(arr, 5, packed_value);
    assert(result == 0);
    
    void* elem = rpg_array_get(arr, 5);
    assert(elem != NULL);
    double retrieved = rpg_decode_packed((const unsigned char*)elem, 0, 10, 2);
    assert(retrieved > 123.44 && retrieved < 123.46);
    
    rpg_free_array(arr);
    return 1;
}

int test_array_bounds_checking() {
    rpg_array* arr = rpg_create_array(RPG_FIELD_PACKED, 10, 10, 0, false);
    
    unsigned char packed_value[10];
    double value = 100.0;
    rpg_encode_packed(packed_value, 0, 10, 0, value);
    
    // Valid index
    assert(rpg_array_set(arr, 0, packed_value) == 0);
    assert(rpg_array_set(arr, 9, packed_value) == 0);
    
    // Invalid indices
    assert(rpg_array_set(arr, -1, packed_value) == -1);
    assert(rpg_array_set(arr, 10, packed_value) == -1);
    
    rpg_free_array(arr);
    return 1;
}

int test_compile_time_array() {
    rpg_array* arr = rpg_create_array(RPG_FIELD_PACKED, 10, 5, 0, true);
    
    // Encode values to packed decimal
    unsigned char packed_data[50];  // 5 elements * 10 bytes each
    double values[] = {10.0, 20.0, 30.0, 40.0, 50.0};
    for (int i = 0; i < 5; i++) {
        rpg_encode_packed(packed_data + (i * 10), 0, 10, 0, values[i]);
    }
    
    int result = rpg_array_init_compile_time(arr, packed_data, 50);
    assert(result == 0);
    
    void* elem = rpg_array_get(arr, 2);
    assert(elem != NULL);
    double retrieved = rpg_decode_packed((const unsigned char*)elem, 0, 10, 0);
    assert(retrieved > 29.9 && retrieved < 30.1);
    
    rpg_free_array(arr);
    return 1;
}

int test_array_sort() {
    rpg_array* arr = rpg_create_array(RPG_FIELD_PACKED, 10, 5, 0, false);
    
    double values[] = {50.0, 20.0, 40.0, 10.0, 30.0};
    unsigned char packed_value[10];
    for (int i = 0; i < 5; i++) {
        rpg_encode_packed(packed_value, 0, 10, 0, values[i]);
        rpg_array_set(arr, i, packed_value);
    }
    
    rpg_array_sort(arr, true);  // Ascending
    
    void* elem = rpg_array_get(arr, 0);
    double val = rpg_decode_packed((const unsigned char*)elem, 0, 10, 0);
    assert(val > 9.9 && val < 10.1);
    
    elem = rpg_array_get(arr, 4);
    val = rpg_decode_packed((const unsigned char*)elem, 0, 10, 0);
    assert(val > 49.9 && val < 50.1);
    
    rpg_free_array(arr);
    return 1;
}

int test_array_search() {
    rpg_array* arr = rpg_create_array(RPG_FIELD_PACKED, 10, 5, 0, false);
    
    double values[] = {10.0, 20.0, 30.0, 40.0, 50.0};
    unsigned char packed_value[10];
    for (int i = 0; i < 5; i++) {
        rpg_encode_packed(packed_value, 0, 10, 0, values[i]);
        rpg_array_set(arr, i, packed_value);
    }
    
    unsigned char packed_key[10];
    rpg_encode_packed(packed_key, 0, 10, 0, 30.0);
    int index = rpg_array_search(arr, packed_key);
    assert(index == 2);
    
    rpg_encode_packed(packed_key, 0, 10, 0, 99.0);
    index = rpg_array_search(arr, packed_key);
    assert(index == -1);
    
    rpg_free_array(arr);
    return 1;
}

int test_array_sum() {
    rpg_array* arr = rpg_create_array(RPG_FIELD_PACKED, 10, 5, 0, false);
    
    double values[] = {10.0, 20.0, 30.0, 40.0, 50.0};
    unsigned char packed_value[10];
    for (int i = 0; i < 5; i++) {
        rpg_encode_packed(packed_value, 0, 10, 0, values[i]);
        rpg_array_set(arr, i, packed_value);
    }
    
    double sum = rpg_array_sum(arr);
    assert(sum > 149.9 && sum < 150.1);
    
    rpg_free_array(arr);
    return 1;
}

int test_array_copy() {
    rpg_array* src = rpg_create_array(RPG_FIELD_PACKED, 10, 5, 0, false);
    rpg_array* dst = rpg_create_array(RPG_FIELD_PACKED, 10, 5, 0, false);
    
    double values[] = {10.0, 20.0, 30.0, 40.0, 50.0};
    unsigned char packed_value[10];
    for (int i = 0; i < 5; i++) {
        rpg_encode_packed(packed_value, 0, 10, 0, values[i]);
        rpg_array_set(src, i, packed_value);
    }
    
    int result = rpg_array_copy(dst, src, 0, 5);
    assert(result == 0);
    
    void* elem = rpg_array_get(dst, 2);
    double val = rpg_decode_packed((const unsigned char*)elem, 0, 10, 0);
    assert(val > 29.9 && val < 30.1);
    
    rpg_free_array(src);
    rpg_free_array(dst);
    return 1;
}

/* ============================================================================
 * BINARY FIELD TESTS
 * ============================================================================ */

int test_binary_int16() {
    unsigned char buf[2];
    int64_t value = 12345;
    
    rpg_encode_binary(buf, value, RPG_BINARY_INT16, RPG_ENDIAN_BIG);
    int64_t decoded = rpg_decode_binary(buf, RPG_BINARY_INT16, RPG_ENDIAN_BIG);
    
    assert(decoded == 12345);
    return 1;
}

int test_binary_int32() {
    unsigned char buf[4];
    int64_t value = 1234567890;
    
    rpg_encode_binary(buf, value, RPG_BINARY_INT32, RPG_ENDIAN_LITTLE);
    int64_t decoded = rpg_decode_binary(buf, RPG_BINARY_INT32, RPG_ENDIAN_LITTLE);
    
    assert(decoded == 1234567890);
    return 1;
}

int test_binary_int64() {
    unsigned char buf[8];
    int64_t value = 9876543210LL;
    
    rpg_encode_binary(buf, value, RPG_BINARY_INT64, RPG_ENDIAN_NATIVE);
    int64_t decoded = rpg_decode_binary(buf, RPG_BINARY_INT64, RPG_ENDIAN_NATIVE);
    
    assert(decoded == 9876543210LL);
    return 1;
}

int test_binary_endianness() {
    unsigned char buf[4];
    int64_t value = 0x12345678;
    
    // Big endian
    rpg_encode_binary(buf, value, RPG_BINARY_INT32, RPG_ENDIAN_BIG);
    assert(buf[0] == 0x12);
    assert(buf[1] == 0x34);
    assert(buf[2] == 0x56);
    assert(buf[3] == 0x78);
    
    // Little endian
    rpg_encode_binary(buf, value, RPG_BINARY_INT32, RPG_ENDIAN_LITTLE);
    assert(buf[0] == 0x78);
    assert(buf[1] == 0x56);
    assert(buf[2] == 0x34);
    assert(buf[3] == 0x12);
    
    return 1;
}

int test_binary_negative_values() {
    unsigned char buf[4];
    int64_t value = -12345;
    
    rpg_encode_binary(buf, value, RPG_BINARY_INT32, RPG_ENDIAN_NATIVE);
    int64_t decoded = rpg_decode_binary(buf, RPG_BINARY_INT32, RPG_ENDIAN_NATIVE);
    
    assert(decoded == -12345);
    return 1;
}

/* ============================================================================
 * MAIN TEST RUNNER
 * ============================================================================ */

int main() {
    printf("=================================================\n");
    printf("RPG Data Structures and Arrays Test Suite\n");
    printf("=================================================\n");
    
    // Data structure tests
    printf("\n--- Data Structure Tests ---\n");
    TEST(create_simple_struct);
    TEST(add_char_field);
    TEST(add_numeric_fields);
    TEST(nested_structures);
    TEST(qualified_names);
    TEST(field_value_operations);
    
    // Array tests
    printf("\n--- Array Tests ---\n");
    TEST(create_numeric_array);
    TEST(create_char_array);
    TEST(array_set_get);
    TEST(array_bounds_checking);
    TEST(compile_time_array);
    TEST(array_sort);
    TEST(array_search);
    TEST(array_sum);
    TEST(array_copy);
    
    // Binary field tests
    printf("\n--- Binary Field Tests ---\n");
    TEST(binary_int16);
    TEST(binary_int32);
    TEST(binary_int64);
    TEST(binary_endianness);
    TEST(binary_negative_values);
    
    // Summary
    printf("\n=================================================\n");
    printf("Test Results:\n");
    printf("  Passed: %d\n", tests_passed);
    printf("  Failed: %d\n", tests_failed);
    printf("  Total:  %d\n", tests_passed + tests_failed);
    printf("=================================================\n");
    
    return tests_failed > 0 ? 1 : 0;
}

// Made with Bob
