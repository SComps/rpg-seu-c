#include "term3270.h"
#include <stdio.h>
#include <string.h>

static unsigned char screen_buf[4096];
static int buf_ptr = 0;

/* 3270 code table for address encoding */
static unsigned char code_table[] = {
    0x40, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7,
    0xC8, 0xC9, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F,
    0x50, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7,
    0xD8, 0xD9, 0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F,
    0x60, 0x61, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7,
    0xE8, 0xE9, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F,
    0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7,
    0xF8, 0xF9, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F
};

void term_init() {
    buf_ptr = 0;
    /* Write Erase Write command code */
    screen_buf[buf_ptr++] = 0xF5; /* Erase/Write */
    screen_buf[buf_ptr++] = 0xC1; /* WCC: Reset Partition + Unlock Keyboard */
}

void term_move(int row, int col) {
    int addr = row * 80 + col;
    screen_buf[buf_ptr++] = ORDER_SBA;
    screen_buf[buf_ptr++] = code_table[(addr >> 6) & 0x3F];
    screen_buf[buf_ptr++] = code_table[addr & 0x3F];
}

void term_attr(unsigned char attr) {
    screen_buf[buf_ptr++] = ORDER_SF;
    screen_buf[buf_ptr++] = attr;
}

void term_put(const char* s) {
    while (*s) {
        screen_buf[buf_ptr++] = *s++;
    }
}

void term_clear() {
    term_init();
    /* More complex clear would use RA (Repeat to Address) */
}

/* 
 * This function depends on the MVS environment's TPUT.
 * For now, we stub it. In GCCMVS, you would use a TPUT svc wrapper.
 */
extern void mvs_tput(unsigned char* buf, int len);

void term_refresh() {
    mvs_tput(screen_buf, buf_ptr);
    buf_ptr = 0;
    screen_buf[buf_ptr++] = 0xF1; /* Write */
    screen_buf[buf_ptr++] = 0xC1; /* WCC */
}
