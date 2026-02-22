#ifndef RPG_FORMATS_H
#define RPG_FORMATS_H

#define MAX_FIELDS 32

typedef struct {
    char name[20];
    int start_col;
    int length;
    int is_numeric;
} FieldDef;

typedef struct {
    char type;
    char description[40];
    int field_count;
    FieldDef fields[MAX_FIELDS];
} FormDef;

/* Helper to get form definition by type code */
FormDef* get_form_def(char type);

#endif
