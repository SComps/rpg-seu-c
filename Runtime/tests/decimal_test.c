/**
 * Test program for RPG Runtime Library - Decimal Operations
 */

#include "../rpg_runtime.h"
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <math.h>

#define EPSILON 0.0001

void test_packed_decimal_encode_decode() {
    printf("Testing packed decimal encode/decode...\n");
    
    unsigned char buf[10];
    double value, decoded;
    
    /* Test 1: Positive number with 2 decimals: 123.45 */
    value = 123.45;
    memset(buf, 0, sizeof(buf));
    rpg_encode_packed(buf, 0, 3, 2, value);
    decoded = rpg_decode_packed(buf, 0, 3, 2);
    assert(fabs(decoded - value) < EPSILON);
    printf("  ✓ Positive decimal: %.2f\n", decoded);
    
    /* Test 2: Negative number with 2 decimals: -456.78 */
    value = -456.78;
    memset(buf, 0, sizeof(buf));
    rpg_encode_packed(buf, 0, 3, 2, value);
    decoded = rpg_decode_packed(buf, 0, 3, 2);
    assert(fabs(decoded - value) < EPSILON);
    printf("  ✓ Negative decimal: %.2f\n", decoded);
    
    /* Test 3: Zero */
    value = 0.0;
    memset(buf, 0, sizeof(buf));
    rpg_encode_packed(buf, 0, 2, 0, value);
    decoded = rpg_decode_packed(buf, 0, 2, 0);
    assert(fabs(decoded - value) < EPSILON);
    printf("  ✓ Zero: %.2f\n", decoded);
    
    /* Test 4: Large number: 9999999.99 */
    value = 9999999.99;
    memset(buf, 0, sizeof(buf));
    rpg_encode_packed(buf, 0, 5, 2, value);
    decoded = rpg_decode_packed(buf, 0, 5, 2);
    assert(fabs(decoded - value) < EPSILON);
    printf("  ✓ Large number: %.2f\n", decoded);
}

void test_zoned_decimal_encode_decode() {
    printf("Testing zoned decimal encode/decode...\n");
    
    char buf[20];
    double value, decoded;
    
    /* Test 1: Positive number: 12345 */
    value = 12345.0;
    memset(buf, 0, sizeof(buf));
    rpg_encode_zoned(buf, 5, 0, value);
    decoded = rpg_decode_zoned(buf, 5, 0);
    assert(fabs(decoded - value) < EPSILON);
    printf("  ✓ Positive integer: %.0f\n", decoded);
    
    /* Test 2: Negative number with decimals: -123.45 */
    value = -123.45;
    memset(buf, 0, sizeof(buf));
    rpg_encode_zoned(buf, 5, 2, value);
    decoded = rpg_decode_zoned(buf, 5, 2);
    assert(fabs(decoded - value) < EPSILON);
    printf("  ✓ Negative decimal: %.2f\n", decoded);
}

void test_string_operations() {
    printf("Testing string operations...\n");
    
    char dest[100];
    
    /* Test CAT */
    rpg_cat(dest, "Hello", "World", 1);
    assert(strcmp(dest, "Hello World") == 0);
    printf("  ✓ CAT: '%s'\n", dest);
    
    /* Test SUBST */
    rpg_subst(dest, "Hello World", 7, 5);
    assert(strcmp(dest, "World") == 0);
    printf("  ✓ SUBST: '%s'\n", dest);
    
    /* Test SCAN */
    int pos = rpg_scan("World", "Hello World", 1);
    assert(pos == 7);
    printf("  ✓ SCAN: position %d\n", pos);
    
    /* Test XLATE */
    rpg_xlate(dest, "Hello", "el", "ip");
    assert(strcmp(dest, "Hippo") == 0);
    printf("  ✓ XLATE: '%s'\n", dest);
    
    /* Test TRIM */
    rpg_trim(dest, "Hello   ");
    assert(strcmp(dest, "Hello") == 0);
    printf("  ✓ TRIM: '%s'\n", dest);
    
    /* Test TRIML */
    rpg_triml(dest, "   Hello");
    assert(strcmp(dest, "Hello") == 0);
    printf("  ✓ TRIML: '%s'\n", dest);
    
    /* Test TRIMB */
    rpg_trimb(dest, "   Hello   ");
    assert(strcmp(dest, "Hello") == 0);
    printf("  ✓ TRIMB: '%s'\n", dest);
}

void test_array_operations() {
    printf("Testing array operations...\n");
    
    /* Test XFOOT */
    double arr[] = {10.5, 20.3, 30.2, 40.0};
    double sum = rpg_xfoot(arr, 4);
    assert(fabs(sum - 101.0) < EPSILON);
    printf("  ✓ XFOOT: %.1f\n", sum);
    
    /* Test MOVEA */
    double dest[4];
    rpg_movea(dest, arr, 4, sizeof(double));
    assert(fabs(dest[0] - 10.5) < EPSILON);
    assert(fabs(dest[3] - 40.0) < EPSILON);
    printf("  ✓ MOVEA: copied %d elements\n", 4);
    
    /* Test SORTA */
    double unsorted[] = {40.0, 10.5, 30.2, 20.3};
    int compare_double(const void* a, const void* b) {
        double diff = *(const double*)a - *(const double*)b;
        return (diff > 0) ? 1 : (diff < 0) ? -1 : 0;
    }
    rpg_sorta(unsorted, 4, sizeof(double), compare_double);
    assert(fabs(unsorted[0] - 10.5) < EPSILON);
    assert(fabs(unsorted[3] - 40.0) < EPSILON);
    printf("  ✓ SORTA: sorted array\n");
}

void test_indicator_operations() {
    printf("Testing indicator operations...\n");
    
    /* Test SETON/SETOF */
    rpg_seton(10);
    assert(IND[10] == true);
    printf("  ✓ SETON: indicator 10 is on\n");
    
    rpg_setof(10);
    assert(IND[10] == false);
    printf("  ✓ SETOF: indicator 10 is off\n");
    
    /* Test special indicators */
    IN_LR = true;
    assert(IN_LR == true);
    printf("  ✓ Special indicator LR works\n");
}

void test_date_operations() {
    printf("Testing date/time operations...\n");
    
    rpg_date date;
    rpg_current_date(&date);
    assert(date.year >= 2026);
    assert(date.month >= 1 && date.month <= 12);
    assert(date.day >= 1 && date.day <= 31);
    printf("  ✓ Current date: %04d-%02d-%02d\n", date.year, date.month, date.day);
    
    rpg_time time_val;
    rpg_current_time(&time_val);
    assert(time_val.hour >= 0 && time_val.hour <= 23);
    assert(time_val.minute >= 0 && time_val.minute <= 59);
    printf("  ✓ Current time: %02d:%02d:%02d\n", 
           time_val.hour, time_val.minute, time_val.second);
    
    /* Test ADDDUR */
    rpg_date result;
    rpg_date start = {2026, 6, 15};
    rpg_adddur_date(&result, &start, 0, 1, 10);
    assert(result.month == 7);
    assert(result.day == 25);
    printf("  ✓ ADDDUR: added 1 month 10 days\n");
    
    /* Test EXTRACT */
    int year = rpg_extract_date(&start, 'Y');
    assert(year == 2026);
    printf("  ✓ EXTRACT: year = %d\n", year);
}

void test_error_handling() {
    printf("Testing error handling...\n");
    
    rpg_clear_error();
    assert(rpg_get_error() == RPG_ERR_NONE);
    printf("  ✓ Error cleared\n");
    
    rpg_set_error(RPG_ERR_FILE_NOT_FOUND, "Test error");
    assert(rpg_get_error() == RPG_ERR_FILE_NOT_FOUND);
    assert(strcmp(rpg_get_error_message(), "Test error") == 0);
    printf("  ✓ Error set: %s\n", rpg_get_error_message());
    
    rpg_clear_error();
    assert(rpg_get_error() == RPG_ERR_NONE);
    printf("  ✓ Error cleared again\n");
}

void test_utility_functions() {
    printf("Testing utility functions...\n");
    
    rpg_init();
    printf("  ✓ Runtime initialized\n");
    
    const char* version = rpg_version();
    printf("  ✓ Runtime version: %s\n", version);
    
    rpg_cleanup();
    printf("  ✓ Runtime cleaned up\n");
}

int main() {
    printf("===========================================\n");
    printf("RPG Runtime Library Test Suite\n");
    printf("===========================================\n\n");
    
    rpg_init();
    
    test_packed_decimal_encode_decode();
    printf("\n");
    
    test_zoned_decimal_encode_decode();
    printf("\n");
    
    test_string_operations();
    printf("\n");
    
    test_array_operations();
    printf("\n");
    
    test_indicator_operations();
    printf("\n");
    
    test_date_operations();
    printf("\n");
    
    test_error_handling();
    printf("\n");
    
    test_utility_functions();
    printf("\n");
    
    printf("===========================================\n");
    printf("✓ All tests passed!\n");
    printf("===========================================\n");
    
    return 0;
}

