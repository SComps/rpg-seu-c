/**
 * RPG Data Structures and Arrays Implementation
 * 
 * Support for RPG II data structures, arrays, and binary fields
 * 
 * Copyright (c) 2026
 * License: MIT
 */

#include "rpg_data.h"
#include "rpg_runtime.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* ============================================================================
 * DATA STRUCTURE SUPPORT
 * ============================================================================ */

/**
 * Create a new data structure definition
 */
rpg_data_struct* rpg_create_struct(const char* name, bool qualified) {
    rpg_data_struct* ds = (rpg_data_struct*)calloc(1, sizeof(rpg_data_struct));
    if (!ds) {
        rpg_set_error(RPG_ERR_INVALID_PARAM, "Failed to allocate data structure");
        return NULL;
    }
    
    strncpy(ds->name, name, sizeof(ds->name) - 1);
    ds->qualified = qualified;
    ds->size = 0;
    ds->field_count = 0;
    ds->fields = NULL;
    
    return ds;
}

/**
 * Add a field to a data structure
 */
int rpg_add_field(rpg_data_struct* ds, const char* name, rpg_field_type type,
                  int length, int decimals) {
    if (!ds || !name || length <= 0) {
        rpg_set_error(RPG_ERR_INVALID_PARAM, "Invalid parameters");
        return -1;
    }
    
    /* Reallocate fields array */
    rpg_field_def* new_fields = (rpg_field_def*)realloc(ds->fields,
        (ds->field_count + 1) * sizeof(rpg_field_def));
    if (!new_fields) {
        rpg_set_error(RPG_ERR_INVALID_PARAM, "Failed to allocate field");
        return -1;
    }
    ds->fields = new_fields;
    
    /* Initialize new field */
    rpg_field_def* field = &ds->fields[ds->field_count];
    memset(field, 0, sizeof(rpg_field_def));
    
    strncpy(field->name, name, sizeof(field->name) - 1);
    field->type = type;
    field->offset = ds->size;
    field->length = length;
    field->decimals = decimals;
    field->array_size = 0;
    field->subfields = NULL;
    field->subfield_count = 0;
    
    /* Update structure size and count */
    ds->size += length;
    ds->field_count++;
    
    return 0;
}

/**
 * Add an array field to a data structure
 */
int rpg_add_array_field(rpg_data_struct* ds, const char* name, 
                        rpg_field_type type, int length, int decimals,
                        int array_size) {
    if (rpg_add_field(ds, name, type, length * array_size, decimals) != 0) {
        return -1;
    }
    
    /* Set array size on the last added field */
    ds->fields[ds->field_count - 1].array_size = array_size;
    
    return 0;
}

/**
 * Add a nested structure field
 */
int rpg_add_struct_field(rpg_data_struct* ds, const char* name,
                         rpg_data_struct* nested) {
    if (!ds || !name || !nested) {
        rpg_set_error(RPG_ERR_INVALID_PARAM, "Invalid parameters");
        return -1;
    }
    
    if (rpg_add_field(ds, name, RPG_FIELD_STRUCT, nested->size, 0) != 0) {
        return -1;
    }
    
    /* Copy nested structure fields */
    rpg_field_def* field = &ds->fields[ds->field_count - 1];
    field->subfields = (rpg_field_def*)malloc(nested->field_count * sizeof(rpg_field_def));
    if (!field->subfields) {
        rpg_set_error(RPG_ERR_INVALID_PARAM, "Failed to allocate subfields");
        return -1;
    }
    
    memcpy(field->subfields, nested->fields, 
           nested->field_count * sizeof(rpg_field_def));
    field->subfield_count = nested->field_count;
    
    return 0;
}

/**
 * Get field by name (supports qualified names)
 */
rpg_field_def* rpg_get_field(rpg_data_struct* ds, const char* name) {
    if (!ds || !name) return NULL;
    
    /* Check for qualified name (struct.field) */
    const char* dot = strchr(name, '.');
    if (dot) {
        /* Qualified name - find parent structure */
        int parent_len = dot - name;
        for (int i = 0; i < ds->field_count; i++) {
            if (strncmp(ds->fields[i].name, name, parent_len) == 0 &&
                ds->fields[i].type == RPG_FIELD_STRUCT) {
                /* Search in subfields */
                const char* subfield_name = dot + 1;
                for (int j = 0; j < ds->fields[i].subfield_count; j++) {
                    if (strcmp(ds->fields[i].subfields[j].name, subfield_name) == 0) {
                        return &ds->fields[i].subfields[j];
                    }
                }
            }
        }
    } else {
        /* Simple name */
        for (int i = 0; i < ds->field_count; i++) {
            if (strcmp(ds->fields[i].name, name) == 0) {
                return &ds->fields[i];
            }
        }
    }
    
    return NULL;
}

/**
 * Get field value as string
 */
void rpg_get_field_value(const void* data, rpg_field_def* field, 
                         char* buffer, int bufsize) {
    if (!data || !field || !buffer || bufsize <= 0) return;
    
    const unsigned char* field_data = (const unsigned char*)data + field->offset;
    
    switch (field->type) {
        case RPG_FIELD_CHAR:
            snprintf(buffer, bufsize, "%.*s", field->length, (const char*)field_data);
            break;
            
        case RPG_FIELD_PACKED: {
            double value = rpg_decode_packed(field_data, 0, field->length, field->decimals);
            snprintf(buffer, bufsize, "%.*f", field->decimals, value);
            break;
        }
        
        case RPG_FIELD_ZONED: {
            double value = rpg_decode_zoned((const char*)field_data, field->length, field->decimals);
            snprintf(buffer, bufsize, "%.*f", field->decimals, value);
            break;
        }
        
        case RPG_FIELD_BINARY: {
            int64_t value = rpg_decode_binary(field_data, RPG_BINARY_INT32, RPG_ENDIAN_NATIVE);
            snprintf(buffer, bufsize, "%lld", (long long)value);
            break;
        }
        
        default:
            buffer[0] = '\0';
            break;
    }
}

/**
 * Set field value from string
 */
int rpg_set_field_value(void* data, rpg_field_def* field, const char* value) {
    if (!data || !field || !value) {
        rpg_set_error(RPG_ERR_INVALID_PARAM, "Invalid parameters");
        return -1;
    }
    
    unsigned char* field_data = (unsigned char*)data + field->offset;
    
    switch (field->type) {
        case RPG_FIELD_CHAR:
            memset(field_data, ' ', field->length);
            strncpy((char*)field_data, value, field->length);
            break;
            
        case RPG_FIELD_PACKED: {
            double num_value = atof(value);
            rpg_encode_packed(field_data, 0, field->length, field->decimals, num_value);
            break;
        }
        
        case RPG_FIELD_ZONED: {
            double num_value = atof(value);
            rpg_encode_zoned((char*)field_data, field->length, field->decimals, num_value);
            break;
        }
        
        case RPG_FIELD_BINARY: {
            int64_t num_value = atoll(value);
            rpg_encode_binary(field_data, num_value, RPG_BINARY_INT32, RPG_ENDIAN_NATIVE);
            break;
        }
        
        default:
            rpg_set_error(RPG_ERR_INVALID_PARAM, "Unsupported field type");
            return -1;
    }
    
    return 0;
}

/**
 * Free data structure definition
 */
void rpg_free_struct(rpg_data_struct* ds) {
    if (!ds) return;
    
    if (ds->fields) {
        /* Free subfields in nested structures */
        for (int i = 0; i < ds->field_count; i++) {
            if (ds->fields[i].subfields) {
                free(ds->fields[i].subfields);
            }
        }
        free(ds->fields);
    }
    
    free(ds);
}

/* ============================================================================
 * ARRAY SUPPORT
 * ============================================================================ */

/**
 * Create a new array
 */
rpg_array* rpg_create_array(rpg_field_type type, int element_size,
                            int count, int decimals, bool compile_time) {
    if (element_size <= 0 || count <= 0) {
        rpg_set_error(RPG_ERR_INVALID_PARAM, "Invalid array parameters");
        return NULL;
    }
    
    rpg_array* array = (rpg_array*)calloc(1, sizeof(rpg_array));
    if (!array) {
        rpg_set_error(RPG_ERR_INVALID_PARAM, "Failed to allocate array");
        return NULL;
    }
    
    array->element_type = type;
    array->element_size = element_size;
    array->element_count = count;
    array->decimals = decimals;
    array->is_compile_time = compile_time;
    
    /* Allocate array data */
    array->data = calloc(count, element_size);
    if (!array->data) {
        free(array);
        rpg_set_error(RPG_ERR_INVALID_PARAM, "Failed to allocate array data");
        return NULL;
    }
    
    return array;
}

/**
 * Get array element
 */
void* rpg_array_get(rpg_array* array, int index) {
    if (!array || index < 0 || index >= array->element_count) {
        rpg_set_error(RPG_ERR_INVALID_PARAM, "Invalid array index");
        return NULL;
    }
    
    return (unsigned char*)array->data + (index * array->element_size);
}

/**
 * Set array element
 */
int rpg_array_set(rpg_array* array, int index, const void* value) {
    if (!array || !value || index < 0 || index >= array->element_count) {
        rpg_set_error(RPG_ERR_INVALID_PARAM, "Invalid parameters");
        return -1;
    }
    
    void* element = rpg_array_get(array, index);
    if (!element) return -1;
    
    memcpy(element, value, array->element_size);
    return 0;
}

/**
 * Initialize compile-time array from data
 */
int rpg_array_init_compile_time(rpg_array* array, const void* data, int size) {
    if (!array || !data || !array->is_compile_time) {
        rpg_set_error(RPG_ERR_INVALID_PARAM, "Invalid parameters");
        return -1;
    }
    
    int copy_size = (size < array->element_count * array->element_size) ? 
                    size : array->element_count * array->element_size;
    
    memcpy(array->data, data, copy_size);
    return 0;
}

/**
 * Compare function for sorting
 */
static int compare_elements(const void* a, const void* b, rpg_array* array) {
    switch (array->element_type) {
        case RPG_FIELD_CHAR:
            return memcmp(a, b, array->element_size);
            
        case RPG_FIELD_PACKED: {
            double val_a = rpg_decode_packed((const unsigned char*)a, 0, 
                                            array->element_size, array->decimals);
            double val_b = rpg_decode_packed((const unsigned char*)b, 0,
                                            array->element_size, array->decimals);
            return (val_a > val_b) - (val_a < val_b);
        }
        
        case RPG_FIELD_BINARY: {
            int64_t val_a = rpg_decode_binary((const unsigned char*)a, 
                                             RPG_BINARY_INT32, RPG_ENDIAN_NATIVE);
            int64_t val_b = rpg_decode_binary((const unsigned char*)b,
                                             RPG_BINARY_INT32, RPG_ENDIAN_NATIVE);
            return (val_a > val_b) - (val_a < val_b);
        }
        
        default:
            return 0;
    }
}

/* Global array pointer for qsort callback */
static rpg_array* g_sort_array = NULL;

static int qsort_compare(const void* a, const void* b) {
    return compare_elements(a, b, g_sort_array);
}

/**
 * Sort array
 */
void rpg_array_sort(rpg_array* array, bool ascending) {
    if (!array) return;
    
    g_sort_array = array;
    qsort(array->data, array->element_count, array->element_size, qsort_compare);
    g_sort_array = NULL;
    
    /* Reverse if descending */
    if (!ascending) {
        unsigned char* temp = (unsigned char*)malloc(array->element_size);
        if (temp) {
            for (int i = 0; i < array->element_count / 2; i++) {
                void* elem1 = rpg_array_get(array, i);
                void* elem2 = rpg_array_get(array, array->element_count - 1 - i);
                memcpy(temp, elem1, array->element_size);
                memcpy(elem1, elem2, array->element_size);
                memcpy(elem2, temp, array->element_size);
            }
            free(temp);
        }
    }
}

/**
 * Search array (linear search)
 */
int rpg_array_search(rpg_array* array, const void* key) {
    if (!array || !key) return -1;
    
    for (int i = 0; i < array->element_count; i++) {
        void* element = rpg_array_get(array, i);
        if (compare_elements(element, key, array) == 0) {
            return i;
        }
    }
    
    return -1;
}

/**
 * Sum numeric array elements
 */
double rpg_array_sum(rpg_array* array) {
    if (!array) return 0.0;
    
    double sum = 0.0;
    
    for (int i = 0; i < array->element_count; i++) {
        void* element = rpg_array_get(array, i);
        
        switch (array->element_type) {
            case RPG_FIELD_PACKED:
                sum += rpg_decode_packed((const unsigned char*)element, 0,
                                        array->element_size, array->decimals);
                break;
                
            case RPG_FIELD_ZONED:
                sum += rpg_decode_zoned((const char*)element,
                                       array->element_size, array->decimals);
                break;
                
            case RPG_FIELD_BINARY:
                sum += (double)rpg_decode_binary((const unsigned char*)element,
                                                 RPG_BINARY_INT32, RPG_ENDIAN_NATIVE);
                break;
                
            default:
                break;
        }
    }
    
    return sum;
}

/**
 * Copy array
 */
int rpg_array_copy(rpg_array* dest, rpg_array* src, int start, int count) {
    if (!dest || !src || start < 0 || count < 0) {
        rpg_set_error(RPG_ERR_INVALID_PARAM, "Invalid parameters");
        return -1;
    }
    
    if (start + count > src->element_count || count > dest->element_count) {
        rpg_set_error(RPG_ERR_INVALID_PARAM, "Array bounds exceeded");
        return -1;
    }
    
    if (dest->element_size != src->element_size) {
        rpg_set_error(RPG_ERR_INVALID_PARAM, "Element size mismatch");
        return -1;
    }
    
    void* src_data = (unsigned char*)src->data + (start * src->element_size);
    memcpy(dest->data, src_data, count * src->element_size);
    
    return 0;
}

/**
 * Free array
 */
void rpg_free_array(rpg_array* array) {
    if (!array) return;
    
    if (array->data) {
        free(array->data);
    }
    
    free(array);
}

/* ============================================================================
 * BINARY FIELD SUPPORT
 * ============================================================================ */

/**
 * Decode binary integer
 */
int64_t rpg_decode_binary(const unsigned char* buf, rpg_binary_type type,
                          rpg_endian endian) {
    if (!buf) return 0;
    
    int64_t value = 0;
    int size = rpg_binary_size(type);
    unsigned char temp[8];
    
    /* Copy and handle endianness */
    memcpy(temp, buf, size);
    if (endian != RPG_ENDIAN_NATIVE && endian != rpg_get_system_endian()) {
        rpg_swap_bytes(temp, size);
    }
    
    /* Decode based on type */
    switch (type) {
        case RPG_BINARY_INT8:
            value = *(int8_t*)temp;
            break;
        case RPG_BINARY_INT16:
            value = *(int16_t*)temp;
            break;
        case RPG_BINARY_INT32:
            value = *(int32_t*)temp;
            break;
        case RPG_BINARY_INT64:
            value = *(int64_t*)temp;
            break;
        case RPG_BINARY_UINT8:
            value = *(uint8_t*)temp;
            break;
        case RPG_BINARY_UINT16:
            value = *(uint16_t*)temp;
            break;
        case RPG_BINARY_UINT32:
            value = *(uint32_t*)temp;
            break;
        case RPG_BINARY_UINT64:
            value = (int64_t)*(uint64_t*)temp;
            break;
    }
    
    return value;
}

/**
 * Encode binary integer
 */
void rpg_encode_binary(unsigned char* buf, int64_t value, 
                       rpg_binary_type type, rpg_endian endian) {
    if (!buf) return;
    
    int size = rpg_binary_size(type);
    unsigned char temp[8];
    
    /* Encode based on type */
    switch (type) {
        case RPG_BINARY_INT8:
            *(int8_t*)temp = (int8_t)value;
            break;
        case RPG_BINARY_INT16:
            *(int16_t*)temp = (int16_t)value;
            break;
        case RPG_BINARY_INT32:
            *(int32_t*)temp = (int32_t)value;
            break;
        case RPG_BINARY_INT64:
            *(int64_t*)temp = value;
            break;
        case RPG_BINARY_UINT8:
            *(uint8_t*)temp = (uint8_t)value;
            break;
        case RPG_BINARY_UINT16:
            *(uint16_t*)temp = (uint16_t)value;
            break;
        case RPG_BINARY_UINT32:
            *(uint32_t*)temp = (uint32_t)value;
            break;
        case RPG_BINARY_UINT64:
            *(uint64_t*)temp = (uint64_t)value;
            break;
    }
    
    /* Handle endianness */
    if (endian != RPG_ENDIAN_NATIVE && endian != rpg_get_system_endian()) {
        rpg_swap_bytes(temp, size);
    }
    
    memcpy(buf, temp, size);
}

/**
 * Get size of binary type
 */
int rpg_binary_size(rpg_binary_type type) {
    switch (type) {
        case RPG_BINARY_INT8:
        case RPG_BINARY_UINT8:
            return 1;
        case RPG_BINARY_INT16:
        case RPG_BINARY_UINT16:
            return 2;
        case RPG_BINARY_INT32:
        case RPG_BINARY_UINT32:
            return 4;
        case RPG_BINARY_INT64:
        case RPG_BINARY_UINT64:
            return 8;
        default:
            return 0;
    }
}

/**
 * Convert binary to double
 */
double rpg_binary_to_double(const unsigned char* buf, rpg_binary_type type,
                            rpg_endian endian) {
    return (double)rpg_decode_binary(buf, type, endian);
}

/**
 * Convert double to binary
 */
void rpg_double_to_binary(unsigned char* buf, double value,
                          rpg_binary_type type, rpg_endian endian) {
    rpg_encode_binary(buf, (int64_t)value, type, endian);
}

/* ============================================================================
 * UTILITY FUNCTIONS
 * ============================================================================ */

/**
 * Get system endianness
 */
rpg_endian rpg_get_system_endian(void) {
    uint16_t test = 0x0102;
    unsigned char* bytes = (unsigned char*)&test;
    
    if (bytes[0] == 0x01) {
        return RPG_ENDIAN_BIG;
    } else {
        return RPG_ENDIAN_LITTLE;
    }
}

/**
 * Swap bytes (for endian conversion)
 */
void rpg_swap_bytes(void* data, int size) {
    if (!data || size <= 1) return;
    
    unsigned char* bytes = (unsigned char*)data;
    for (int i = 0; i < size / 2; i++) {
        unsigned char temp = bytes[i];
        bytes[i] = bytes[size - 1 - i];
        bytes[size - 1 - i] = temp;
    }
}

/**
 * Align offset to boundary
 */
int rpg_align_offset(int offset, int alignment) {
    if (alignment <= 1) return offset;
    
    int remainder = offset % alignment;
    if (remainder == 0) return offset;
    
    return offset + (alignment - remainder);
}

// Made with Bob
