ASMSEU   CSECT
         STM   14,12,12(13)        SAVE REGISTERS
         BASR  12,0                ESTABLISH ADDRESSABILITY
         USING *,12
         ST    13,SAVEAREA+4       CHAIN SAVE AREAS
         LA    0,SAVEAREA
         ST    0,8(13)
         LR    13,0
*
* MAIN LOOP
*
LOOP     DS    0H
         BAL   11,DRAWHDR          DRAW THE HEADER
         BAL   11,DRAWPAGE         DRAW THE PAGE
         BAL   11,TPUTPRM          TPUT THE BUFFER
*
         BAL   11,TGETINP          TGET USER INPUT
         CLM   1,1,AIDPF3          CHECK FOR PF3 (EXIT)
         BE    EXIT
*
         B     LOOP                REPEAT
*
EXIT     DS    0H
         L     13,SAVEAREA+4       RESTORE SAVE AREA
         LM    14,12,12(13)        RESTORE REGISTERS
         XR    15,15               RETURN CODE 0
         BR    14                  RETURN TO TSO
*
* SUBROUTINE: DRAW HEADER
*
DRAWHDR  DS    0H
         MVC   SCREEN(26),HDRTXT   "Assembly SEU (IFOX/MVS)"
         BR    11
*
* SUBROUTINE: DRAW PAGE
*
DRAWPAGE DS    0H
         MVC   SCREEN+80(80),LIN1  DUMMY DATA LINE 1
         MVC   SCREEN+160(80),LIN2 DUMMY DATA LINE 2
         BR    11
*
* SUBROUTINE: TPUT BUFFER
*
TPUTPRM  DS    0H
         TPUT  SCREEN,SCRLEN,FULLSCR  FULLSCREEN TPUT
         BR    11
*
* SUBROUTINE: TGET INPUT
*
TGETINP  DS    0H
         TGET  INBUF,INLEN         GET USER INPUT
         L     1,INBUF             FIRST WORD CONTAINS AID
         BR    11
*
* DATA AREAS
*
SAVEAREA DC    18F'0'
AIDPF3   EQU   X'F3'               PF3 AID BYTE
*
HDRTXT   DC    CL26'Assembly SEU (IFOX/MVS)'
LIN1     DC    CL80'MYPROG   START 0'
LIN2     DC    CL80'         END'
*
SCREEN   DS    CL1920              24X80 SCREEN BUFFER
SCRLEN   EQU   *-SCREEN
*
INBUF    DS    CL256
INLEN    EQU   *-INBUF
*
         END   ASMSEU
