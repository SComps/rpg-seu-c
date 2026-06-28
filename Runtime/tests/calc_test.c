/**
 * Calculation Operations Test Suite
 * 
 * Tests string operations, array operations, and advanced math
 */

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <math.h>
#include "../rpg_runtime.h"
#include "../rpg_data.h"

/* Test counter */
static int tests_run = 0;
static int tests_passed = 0;

#define TEST(name) \
    do { \
        tests_run++; \
        printf("Testing %s... ", name); \
        fflush(stdout);

#define PASS() \
        tests_passed++; \
        printf("PASSED\n"); \
    } while(0)

/* Comparison function for numeric arrays */
int compare_double(const void* a, const void* b) {
    double da = *(const double*)a;
    double db = *(const double*)b;
    if (da < db) return -1;
    if (da > db) return 1;
    return 0;
}

/* Comparison function for string arrays */
int compare_string(const void* a, const void* b) {
    return strcmp((const char*)a, (const char*)b);
}

/* ============================================================================
 * STRING OPERATION TESTS
 * ============================================================================ */

void test_cat() {
    TEST("rpg_cat");
    char result[50] = {0};
    
    /* Basic concatenation */
    rpg_cat(result, "Hello", "World", 0);
    assert(strcmp(result, "HelloWorld") == 0);
    
    /* Concatenation with blanks */
    rpg_cat(result, "Hello", "World", 1);
    assert(strcmp(result, "Hello World") == 0);
    
    /* Concatenation with multiple blanks */
    rpg_cat(result, "Hello", "World", 3);
    assert(strcmp(result, "Hello   World") == 0);
    
    PASS();
}

void test_subst() {
    TEST("rpg_subst");
    char result[50] = {0};
    const char* source = "Hello World";
    
    /* Extract from beginning */
    rpg_subst(result, source, 1, 5);
    assert(strcmp(result, "Hello") == 0);
    
    /* Extract from middle */
    rpg_subst(result, source, 7, 5);
    assert(strcmp(result, "World") == 0);
    
    /* Extract single character */
    rpg_subst(result, source, 1, 1);
    assert(strcmp(result, "H") == 0);
    
    /* Extract beyond end */
    rpg_subst(result, source, 7, 20);
    assert(strcmp(result, "World") == 0);
    
    PASS();
}

void test_scan() {
    TEST("rpg_scan");
    const char* source = "Hello World";
    
    /* Find at beginning */
    int pos = rpg_scan("Hello", source, 1);
    assert(pos == 1);
    
    /* Find in middle */
    pos = rpg_scan("World", source, 1);
    assert(pos == 7);
    
    /* Find single character */
    pos = rpg_scan("o", source, 1);
    assert(pos == 5);
    
    /* Not found */
    pos = rpg_scan("xyz", source, 1);
    assert(pos == 0);
    
    /* Start from position */
    pos = rpg_scan("o", source, 6);
    assert(pos == 8);
    
    PASS();
}

void test_xlate() {
    TEST("rpg_xlate");
    char result[50] = {0};
    
    /* Simple translation */
    rpg_xlate(result, "Hello", "elo", "310");
    assert(strcmp(result, "H3110") == 0);
    
    /* Multiple character translation */
    rpg_xlate(result, "abcdef", "ace", "ACE");
    assert(strcmp(result, "AbCdEf") == 0);
    
    /* No translation */
    rpg_xlate(result, "Hello", "xyz", "XYZ");
    assert(strcmp(result, "Hello") == 0);
    
    PASS();
}

void test_trim() {
    TEST("rpg_trim/triml/trimb");
    char result[50] = {0};
    
    /* Trim trailing blanks */
    rpg_trim(result, "Hello   ");
    assert(strcmp(result, "Hello") == 0);
    
    /* Trim leading blanks */
    rpg_triml(result, "   Hello");
    assert(strcmp(result, "Hello") == 0);
    
    /* Trim both */
    rpg_trimb(result, "   Hello   ");
    assert(strcmp(result, "Hello") == 0);
    
    /* No blanks to trim */
    rpg_trim(result, "Hello");
    assert(strcmp(result, "Hello") == 0);
    
    PASS();
}

/* ============================================================================
 * MATH OPERATION TESTS
 * ============================================================================ */

void test_sqrt() {
    TEST("rpg_sqrt");
    
    /* Perfect squares */
    assert(fabs(rpg_sqrt(4.0) - 2.0) < 0.0001);
    assert(fabs(rpg_sqrt(9.0) - 3.0) < 0.0001);
    assert(fabs(rpg_sqrt(16.0) - 4.0) < 0.0001);
    assert(fabs(rpg_sqrt(25.0) - 5.0) < 0.0001);
    
    /* Non-perfect squares */
    assert(fabs(rpg_sqrt(2.0) - 1.41421) < 0.001);
    assert(fabs(rpg_sqrt(10.0) - 3.16228) < 0.001);
    
    /* Zero */
    assert(rpg_sqrt(0.0) == 0.0);
    
    /* Negative (should set error) */
    rpg_clear_error();
    double result = rpg_sqrt(-1.0);
    assert(result == 0.0);
    assert(rpg_get_error() == RPG_ERR_NUMERIC_OVERFLOW);
    rpg_clear_error();
    
    PASS();
}

void test_mvr() {
    TEST("rpg_mvr");
    
    /* Integer division remainders */
    assert(fabs(rpg_mvr(10.0, 3.0) - 1.0) < 0.0001);
    assert(fabs(rpg_mvr(17.0, 5.0) - 2.0) < 0.0001);
    assert(fabs(rpg_mvr(20.0, 7.0) - 6.0) < 0.0001);
    
    /* Exact division (no remainder) */
    assert(fabs(rpg_mvr(10.0, 5.0) - 0.0) < 0.0001);
    assert(fabs(rpg_mvr(20.0, 4.0) - 0.0) < 0.0001);
    
    /* Negative numbers */
    assert(fabs(rpg_mvr(-10.0, 3.0) - (-1.0)) < 0.0001);
    assert(fabs(rpg_mvr(10.0, -3.0) - 1.0) < 0.0001);
    
    /* Division by zero (should set error) */
    rpg_clear_error();
    double result = rpg_mvr(10.0, 0.0);
    assert(result == 0.0);
    assert(rpg_get_error() == RPG_ERR_DIVIDE_ZERO);
    rpg_clear_error();
    
    PASS();
}

void test_half_adjust() {
    TEST("rpg_half_adjust");
    
    /* Round up */
    assert(fabs(rpg_half_adjust(1.5, 0) - 2.0) < 0.0001);
    assert(fabs(rpg_half_adjust(2.7, 0) - 3.0) < 0.0001);
    
    /* Round down */
    assert(fabs(rpg_half_adjust(1.4, 0) - 1.0) < 0.0001);
    assert(fabs(rpg_half_adjust(2.3, 0) - 2.0) < 0.0001);
    
    /* Decimal places */
    assert(fabs(rpg_half_adjust(1.234, 2) - 1.23) < 0.0001);
    assert(fabs(rpg_half_adjust(1.235, 2) - 1.24) < 0.0001);
    assert(fabs(rpg_half_adjust(1.567, 1) - 1.6) < 0.0001);
    
    /* Negative numbers */
    assert(fabs(rpg_half_adjust(-1.5, 0) - (-2.0)) < 0.0001);
    assert(fabs(rpg_half_adjust(-1.4, 0) - (-1.0)) < 0.0001);
    
    PASS();
}

/* ============================================================================
 * ARRAY OPERATION TESTS
 * ============================================================================ */

void test_lookup() {
    TEST("rpg_lookup");
    double numbers[] = {1.0, 3.0, 5.0, 7.0, 9.0};
    int count = sizeof(numbers) / sizeof(numbers[0]);
    
    /* Find existing values */
    double key = 5.0;
    int idx = rpg_lookup(numbers, count, sizeof(double), &key, compare_double);
    assert(idx == 2);
    
    key = 1.0;
    idx = rpg_lookup(numbers, count, sizeof(double), &key, compare_double);
    assert(idx == 0);
    
    key = 9.0;
    idx = rpg_lookup(numbers, count, sizeof(double), &key, compare_double);
    assert(idx == 4);
    
    /* Not found */
    key = 4.0;
    idx = rpg_lookup(numbers, count, sizeof(double), &key, compare_double);
    assert(idx == -1);
    
    PASS();
}

void test_xfoot() {
    TEST("rpg_xfoot");
    
    /* Sum positive numbers */
    double numbers1[] = {1.0, 2.0, 3.0, 4.0, 5.0};
    double sum = rpg_xfoot(numbers1, 5);
    assert(fabs(sum - 15.0) < 0.0001);
    
    /* Sum with decimals */
    double numbers2[] = {1.5, 2.5, 3.5};
    sum = rpg_xfoot(numbers2, 3);
    assert(fabs(sum - 7.5) < 0.0001);
    
    /* Sum with negatives */
    double numbers3[] = {10.0, -5.0, 3.0, -2.0};
    sum = rpg_xfoot(numbers3, 4);
    assert(fabs(sum - 6.0) < 0.0001);
    
    /* Empty array */
    sum = rpg_xfoot(NULL, 0);
    assert(sum == 0.0);
    
    PASS();
}

void test_sorta() {
    TEST("rpg_sorta");
    
    /* Sort numbers */
    double numbers[] = {5.0, 2.0, 8.0, 1.0, 9.0, 3.0};
    int count = sizeof(numbers) / sizeof(numbers[0]);
    
    rpg_sorta(numbers, count, sizeof(double), compare_double);
    
    assert(numbers[0] == 1.0);
    assert(numbers[1] == 2.0);
    assert(numbers[2] == 3.0);
    assert(numbers[3] == 5.0);
    assert(numbers[4] == 8.0);
    assert(numbers[5] == 9.0);
    
    /* Sort strings */
    char strings[][10] = {"dog", "cat", "bird", "ant"};
    count = sizeof(strings) / sizeof(strings[0]);
    
    rpg_sorta(strings, count, sizeof(strings[0]), compare_string);
    
    assert(strcmp(strings[0], "ant") == 0);
    assert(strcmp(strings[1], "bird") == 0);
    assert(strcmp(strings[2], "cat") == 0);
    assert(strcmp(strings[3], "dog") == 0);
    
    PASS();
}

void test_movea() {
    TEST("rpg_movea");
    
    /* Copy numeric array */
    double source[] = {1.0, 2.0, 3.0, 4.0, 5.0};
    double dest[5] = {0};
    int count = sizeof(source) / sizeof(source[0]);
    
    rpg_movea(dest, source, count, sizeof(double));
    
    for (int i = 0; i < count; i++) {
        assert(dest[i] == source[i]);
    }
    
    /* Copy string array */
    char src_strings[][10] = {"one", "two", "three"};
    char dst_strings[3][10] = {{0}};
    count = sizeof(src_strings) / sizeof(src_strings[0]);
    
    rpg_movea(dst_strings, src_strings, count, sizeof(src_strings[0]));
    
    for (int i = 0; i < count; i++) {
        assert(strcmp(dst_strings[i], src_strings[i]) == 0);
    }
    
    PASS();
}

/* ============================================================================
 * MAIN TEST RUNNER
 * ============================================================================ */

int main() {
    printf("=======================================================\n");
    printf("RPG CALCULATION OPERATIONS TEST SUITE\n");
    printf("=======================================================\n\n");
    
    /* Initialize runtime */
    rpg_init();
    
    /* String operations */
    printf("STRING OPERATIONS:\n");
    test_cat();
    test_subst();
    test_scan();
    test_xlate();
    test_trim();
    printf("\n");
    
    /* Math operations */
    printf("MATH OPERATIONS:\n");
    test_sqrt();
    test_mvr();
    test_half_adjust();
    printf("\n");
    
    /* Array operations */
    printf("ARRAY OPERATIONS:\n");
    test_lookup();
    test_xfoot();
    test_sorta();
    test_movea();
    printf("\n");
    
    /* Cleanup runtime */
    rpg_cleanup();
    
    /* Print summary */
    printf("=======================================================\n");
    printf("RESULTS: %d/%d tests passed\n", tests_passed, tests_run);
    printf("=======================================================\n");
    
    return (tests_passed == tests_run) ? 0 : 1;
}

// Made with Bob
