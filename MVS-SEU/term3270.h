#ifndef TERM3270_H
#define TERM3270_H

/* 3270 Orders */
#define ORDER_SBA 0x11  /* Set Buffer Address */
#define ORDER_SF  0x1D  /* Start Field */
#define ORDER_IC  0x13  /* Insert Cursor */
#define ORDER_PT  0x05  /* Program Tab */
#define ORDER_RA  0x3C  /* Repeat to Address */
#define ORDER_EUA 0x12  /* Erase Unprotected to Address */

/* Attributes */
#define ATTR_PROT 0x20  /* Protected */
#define ATTR_NUM  0x10  /* Numeric (only used with PROT usually) */
#define ATTR_NORM 0x00  /* Normal Intensity */
#define ATTR_HIGH 0x08  /* High Intensity */
#define ATTR_SEL  0x04  /* Selector Pen */
#define ATTR_NON  0x0C  /* Non-display */

/* AID Keys */
#define AID_ENTER 0x7D
#define AID_PF1   0xF1
#define AID_PF2   0xF2
#define AID_PF3   0xF3
#define AID_PF4   0xF4
#define AID_PF5   0xF5
#define AID_PF6   0xF6
#define AID_PF7   0xF7
#define AID_PF8   0xF8
#define AID_PF9   0xF9
#define AID_CLEAR 0x6D

/* Terminal functions */
void term_init();
void term_clear();
void term_move(int row, int col);
void term_put(const char* s);
void term_attr(unsigned char attr);
void term_refresh();

#endif
