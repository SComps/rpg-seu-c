#include "rpg_formats.h"
#include <string.h>

static FormDef forms[6];

void init_formats() {
    FormDef* f;

    /* H - Control */
    f = &forms[0];
    f->type = 'H';
    strcpy(f->description, "Control Specification");
    f->field_count = 5;
    strcpy(f->fields[0].name, "Page/Line"); f->fields[0].start_col = 1; f->fields[0].length = 5;
    strcpy(f->fields[1].name, "Form Type"); f->fields[1].start_col = 6; f->fields[1].length = 1;
    strcpy(f->fields[2].name, "Core Size Comp"); f->fields[2].start_col = 7; f->fields[2].length = 1;
    strcpy(f->fields[3].name, "Program Name"); f->fields[3].start_col = 75; f->fields[3].length = 6;

    /* F - File Description */
    f = &forms[1];
    f->type = 'F';
    strcpy(f->description, "File Description");
    f->field_count = 8;
    strcpy(f->fields[0].name, "Page/Line"); f->fields[0].start_col = 1; f->fields[0].length = 5;
    strcpy(f->fields[1].name, "Form Type"); f->fields[1].start_col = 6; f->fields[1].length = 1;
    strcpy(f->fields[2].name, "Filename");  f->fields[2].start_col = 7; f->fields[2].length = 8;
    strcpy(f->fields[3].name, "File Type"); f->fields[3].start_col = 15; f->fields[3].length = 1;
    strcpy(f->fields[4].name, "File Desig"); f->fields[4].start_col = 16; f->fields[4].length = 1;
    strcpy(f->fields[5].name, "Rec Length"); f->fields[5].start_col = 24; f->fields[5].length = 4;
    strcpy(f->fields[6].name, "Device");     f->fields[6].start_col = 40; f->fields[6].length = 7;
    strcpy(f->fields[7].name, "Comments");   f->fields[7].start_col = 75; f->fields[7].length = 6;

    /* I - Input */
    f = &forms[2];
    f->type = 'I';
    strcpy(f->description, "Input Specification");
    f->field_count = 7;
    strcpy(f->fields[0].name, "Page/Line"); f->fields[0].start_col = 1; f->fields[0].length = 5;
    strcpy(f->fields[1].name, "Form Type"); f->fields[1].start_col = 6; f->fields[1].length = 1;
    strcpy(f->fields[2].name, "Filename");  f->fields[2].start_col = 7; f->fields[2].length = 8;
    strcpy(f->fields[3].name, "Packed/Bin"); f->fields[3].start_col = 43; f->fields[3].length = 1;
    strcpy(f->fields[4].name, "Field Loc");  f->fields[4].start_col = 44; f->fields[4].length = 8;
    strcpy(f->fields[5].name, "Decimal Pos"); f->fields[5].start_col = 52; f->fields[5].length = 1;
    strcpy(f->fields[6].name, "Field Name");  f->fields[6].start_col = 53; f->fields[6].length = 6;

    /* C - Calculation */
    f = &forms[3];
    f->type = 'C';
    strcpy(f->description, "Calculation Specification");
    f->field_count = 10;
    strcpy(f->fields[0].name, "Page/Line"); f->fields[0].start_col = 1; f->fields[0].length = 5;
    strcpy(f->fields[1].name, "Form Type"); f->fields[1].start_col = 6; f->fields[1].length = 1;
    strcpy(f->fields[2].name, "Ctrl Level"); f->fields[2].start_col = 7; f->fields[2].length = 2;
    strcpy(f->fields[3].name, "Factor 1");   f->fields[3].start_col = 18; f->fields[3].length = 10;
    strcpy(f->fields[4].name, "Operation");  f->fields[4].start_col = 28; f->fields[4].length = 5;
    strcpy(f->fields[5].name, "Factor 2");   f->fields[5].start_col = 33; f->fields[5].length = 10;
    strcpy(f->fields[6].name, "Result Field"); f->fields[6].start_col = 43; f->fields[6].length = 6;
    strcpy(f->fields[7].name, "Field Len");   f->fields[7].start_col = 49; f->fields[7].length = 3;
    strcpy(f->fields[8].name, "Decimal Pos"); f->fields[8].start_col = 52; f->fields[8].length = 1;
    strcpy(f->fields[9].name, "Indicators");  f->fields[9].start_col = 54; f->fields[9].length = 6;

    /* O - Output */
    f = &forms[4];
    f->type = 'O';
    strcpy(f->description, "Output Specification");
    f->field_count = 6;
    strcpy(f->fields[0].name, "Page/Line"); f->fields[0].start_col = 1; f->fields[0].length = 5;
    strcpy(f->fields[1].name, "Form Type"); f->fields[1].start_col = 6; f->fields[1].length = 1;
    strcpy(f->fields[2].name, "Filename");  f->fields[2].start_col = 7; f->fields[2].length = 8;
    strcpy(f->fields[3].name, "Type");      f->fields[3].start_col = 15; f->fields[3].length = 1;
    strcpy(f->fields[4].name, "Field Name"); f->fields[4].start_col = 32; f->fields[4].length = 6;
    strcpy(f->fields[5].name, "End Pos");    f->fields[5].start_col = 40; f->fields[5].length = 4;
}

FormDef* get_form_def(char type) {
    int i;
    for (i = 0; i < 5; i++) {
        if (forms[i].type == type) return &forms[i];
    }
    return NULL;
}
