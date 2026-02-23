#include "asm_formats.h"
#include <string.h>

static FormDef forms[1];

void init_formats() {
    FormDef* f;

    /* A - Assembly IFOX */
    f = &forms[0];
    f->type = 'A';
    strcpy(f->description, "IFOX Assembly Format");
    f->field_count = 5;
    strcpy(f->fields[0].name, "Label");      f->fields[0].start_col = 1;  f->fields[0].length = 8;
    strcpy(f->fields[1].name, "Operation");  f->fields[1].start_col = 10; f->fields[1].length = 5;
    strcpy(f->fields[2].name, "Operands");   f->fields[2].start_col = 16; f->fields[2].length = 56;
    strcpy(f->fields[3].name, "Cont");       f->fields[3].start_col = 72; f->fields[3].length = 1;
    strcpy(f->fields[4].name, "Sequence");   f->fields[4].start_col = 73; f->fields[4].length = 8;
}

FormDef* get_form_def(char type) {
    if (type == 'A' || type == ' ') return &forms[0];
    return NULL;
}

FormDef* get_form_def(char type) {
    int i;
    for (i = 0; i < 5; i++) {
        if (forms[i].type == type) return &forms[i];
    }
    return NULL;
}
