#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

// --- Indicators ---
bool IND[100] = {false};
bool IN_LR = false;
bool IN_MR = false;
bool IN_L[10] = {false};
#define IN_L1 IN_L[1] 
#define IN_L2 IN_L[2] 
#define IN_L3 IN_L[3] 
#define IN_L4 IN_L[4] 
#define IN_L5 IN_L[5] 
#define IN_L6 IN_L[6] 
#define IN_L7 IN_L[7] 
#define IN_L8 IN_L[8] 
#define IN_L9 IN_L[9] 

// --- Program Variables ---
char CAT[5] = {0};
double AMNT = 0.0;
double TOTAL = 0.0;
double GRND = 0.0;
// --- Control Level Buffers ---
char PREV_CAT[5] = {0};

// --- Subroutines ---
int main(int argc, char** argv) {
    if (argc < 3) {
        printf("Usage: %s <INPUT> <REPORT> \n", argv[0]);
        return 1;
    }

    FILE* INPUT = fopen(argv[1], "r");
    if (!INPUT) { printf("Failed to open INPUT\n"); return 1; }
    FILE* REPORT = fopen(argv[2], "w");
    if (!REPORT) { printf("Failed to open REPORT\n"); return 1; }

    char recordBuf[82];
    bool firstRecord = true;

    // --- Main Logic Cycle ---
    while (fgets(recordBuf, sizeof(recordBuf), INPUT)) {
        recordBuf[strcspn(recordBuf, "\r\n")] = 0;
        // Check for Level Breaks
        {
            char cur_val[5];
            strncpy(cur_val, recordBuf + 0, 4); cur_val[4] = 0;
            if (!firstRecord && strcmp(cur_val, PREV_CAT) != 0) IN_L1 = true;
        }

        if (!firstRecord && (
            IN_L1 || IN_L2 || IN_L3 || IN_L4 || IN_L5 || IN_L6 || IN_L7 || IN_L8 || IN_L9 || IN_LR
        )) {
            // --- Total Calculations ---
            GRND = GRND + TOTAL;
            TOTAL = 0;

            // --- Total Output ---
        }
        for(int i=1; i<10; i++) IN_L[i] = false;

        // Extract Input Fields
        {
            char tmp[5]; strncpy(tmp, recordBuf + 0, 4); tmp[4] = 0;
            strcpy(CAT, tmp);
            strcpy(PREV_CAT, CAT);
        }
        {
            char tmp[8]; strncpy(tmp, recordBuf + 6, 7); tmp[7] = 0;
            AMNT = atof(tmp);
        }

        // --- Detail Calculations ---
        TOTAL = TOTAL + AMNT;

        // --- Detail Output ---
        firstRecord = false;
        if (IN_LR) break;
    }

    // --- Final LR Cycle ---
    IN_LR = true;
    // Total Calculations
    GRND = GRND + TOTAL;
    TOTAL = 0;
    // Total Output
    if (INPUT) fclose(INPUT);
    if (REPORT) fclose(REPORT);
    return 0;
}
