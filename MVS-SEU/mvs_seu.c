#include <stdio.h>
#include <string.h>
#include "term3270.h"
#include "rpg_formats.h"

#define MAX_RECORDS 1000
static char records[MAX_RECORDS][81];
static int record_count = 0;
static int cur_line = 0;

void draw_header() {
    term_move(0, 0);
    term_attr(ATTR_PROT | ATTR_HIGH);
    term_put("RPG SEU (MVS Edition) - ");
    term_attr(ATTR_PROT);
    term_put("Press PF3 to Exit, PF1 for Prompt");
    
    term_move(1, 0);
    term_attr(ATTR_PROT | ATTR_HIGH);
    term_put("Line  Source Data (001-080)");
    term_move(2, 0);
    term_put("--------------------------------------------------------------------------------");
}

void draw_page() {
    int i;
    for (i = 0; i < 20; i++) {
        int line_idx = cur_line + i;
        term_move(3 + i, 0);
        if (line_idx < record_count) {
            char line_num[8];
            sprintf(line_num, "%04d", line_idx + 1);
            term_attr(ATTR_PROT);
            term_put(line_num);
            term_put("  ");
            term_attr(ATTR_NORM);
            term_put(records[line_idx]);
        } else {
            term_attr(ATTR_PROT);
            term_put("....  ");
            term_attr(ATTR_NORM);
            term_put("                                                                                ");
        }
    }
}

int main(int argc, char** argv) {
    init_formats();
    term_init();
    
    /* Dummy data */
    strcpy(records[0], "     H                                                                          ");
    strcpy(records[1], "     FINPUT   I   F  80        DISK                                             ");
    strcpy(records[2], "     FREPORT  O   F  80        PRINTER                                          ");
    record_count = 3;

    int running = 1;
    while (running) {
        term_clear();
        draw_header();
        draw_page();
        term_refresh();
        
        /* 
         * term_refresh triggers mvs_tput.
         * We then need an mvs_tget to wait for input.
         */
        unsigned char aid;
        // aid = mvs_tget(); /* Stub */
        aid = AID_PF3; /* For now, just exit */
        
        if (aid == AID_PF3) running = 0;
    }

    return 0;
}
