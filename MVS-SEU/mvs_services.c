#include <stdio.h>

/*
 * MVS SVC wrappers for GCCMVS.
 * On TK5 with GCCMVS, these are often provided by the library,
 * but let's define the logic.
 */

void mvs_tput(unsigned char* buf, int len) {
    /* 
     * In a real GCCMVS environment, this would be:
     * __asm__ volatile (
     *     "LR   0,%0\r\n"   // Length
     *     "LR   1,%1\r\n"   // Buffer
     *     "SVC  93\r\n"     // TPUT
     *     : : "r"(len), "r"(buf) : "0", "1"
     * );
     */
}

unsigned char mvs_tget() {
    /*
     * Similar logic for SVC 94 (TGET).
     * Returns the AID byte (first byte of input buffer usually).
     */
    return 0x7D; /* Enter */
}
