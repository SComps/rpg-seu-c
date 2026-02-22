#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

// --- Indicators ---
bool IND[100] = {false};
bool IN_LR = false;
bool IN_MR = false;

// --- Program Variables ---
double NUM1 = 0.0;
double NUM2 = 0.0;
double RES = 0.0;

int main(int argc, char** argv) {
    if (argc < 3) {
        printf("Usage: %s <INPUT> <OUTPUT> \n", argv[0]);
        return 1;
    }

    FILE* INPUT = fopen(argv[1], "r");
    if (!INPUT) { printf("Failed to open INPUT\n"); return 1; }
    FILE* OUTPUT = fopen(argv[2], "w");
    if (!OUTPUT) { printf("Failed to open OUTPUT\n"); return 1; }

    char recordBuf[82];

    // --- Main Logic Cycle ---
    while (fgets(recordBuf, sizeof(recordBuf), INPUT)) {
        recordBuf[strcspn(recordBuf, "\r\n")] = 0;
        // Extract NUM1
        char tmp_NUM1[6];
        strncpy(tmp_NUM1, recordBuf + 0, 5);
        tmp_NUM1[5] = '\0';
        NUM1 = atof(tmp_NUM1);
        // Extract NUM2
        char tmp_NUM2[6];
        strncpy(tmp_NUM2, recordBuf + 5, 5);
        tmp_NUM2[5] = '\0';
        NUM2 = atof(tmp_NUM2);

        // --- Detail Calculations ---
        RES = NUM1 + NUM2;

        // --- Detail Output ---
        fprintf(OUTPUT, "%-*g ", 6, RES);
        fprintf(OUTPUT, "%s ", "HELLO WORLD");
        fprintf(OUTPUT, "\n");
    }

    if (INPUT) fclose(INPUT);
    if (OUTPUT) fclose(OUTPUT);
    return 0;
}
