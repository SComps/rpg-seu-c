//SEU     JOB (TSO),'COMPILE SEU',CLASS=A,MSGCLASS=A,NOTIFY=HERC01,
//        USER=HERC01,PASSWORD=CUL8TR
//*
//UPLOAD   EXEC PGM=IEBGENER
//SYSPRINT DD  SYSOUT=A
//SYSUT2   DD  DSN=HERC01.SOURCE.ASM(SEU),DISP=SHR
//SYSIN    DD  DUMMY
//SYSUT1   DD  DATA,DLM='$$'
SEU      CSECT
* ------------------------------------------------------------------ *
* SEU  -  SCREEN EDITING UTILITY                                      *
*         MVS 3.8J / S370 / IFOX00                                    *
* ------------------------------------------------------------------ *
* INVOKE FROM TSO READY PROMPT:
*   SEU 'HLQ.MYLIB(MEMBER)'     EDIT PDS MEMBER                       *
*   SEU 'HLQ.MYSEQ'             EDIT SEQUENTIAL DATASET               *
*
* PF3=EXIT  PF7=SCROLL UP  PF8=SCROLL DOWN  PF10=SAVE                 *
* COMMAND===>  TYPE MEMBER NAME + ENTER TO SWITCH  (PDS ONLY)         *
*
* SCREEN LAYOUT (80 COLUMN x 24 ROW 3270):                            *
*   ROW  1     TITLE BAR  (DSN AND MEMBER NAME)                       *
*   ROW  2     COMMAND LINE
*   ROW  3     RPG COLUMN RULER
*   ROWS 4-22  19 EDIT ROWS (5-DIGIT LINE NUMBER + 66 DATA CHARS)     *
*   ROW 23     PF KEY LEGEND
*   ROW 24     STATUS LINE (STATE AND RECORD COUNT)
*
* NOTES:
*   MAXIMUM 200 RECORDS (RECFM=FB LRECL=80) HELD IN MEMORY            *
*   PDS MEMBERS:  RDJFCB + OPEN TYPE=J INJECTS MEMBER NAME AT +44     *
*   SEQUENTIAL:   STANDARD OPEN, NO JFCB MANIPULATION                 *
*   SVC 99 ALLOCATES DDNAME SYSASMEU AT STARTUP                       *
*   SVC 99 KEY X'0004' STATUS DISPOSITION CODES:                      *
*     X'01'=NEW  X'02'=OLD  X'03'=MOD  X'04'=SHR                     *
*   SAVE USES OLD FOR EXISTING MEMBER/FILE, MOD TO CREATE NEW         *
*
* DEBUG OUTPUT (REMOVE WHEN STABLE):
*   THREE TSO SCROLL-MODE LINES ARE WRITTEN BEFORE FULLSCREEN MODE.   *
*   THEY SHOW THE PARSED DSN, MEMBER, AND DYNALL RESULT SO YOU CAN    *
*   VERIFY PARSE AND ALLOCATION ARE CORRECT.                          *
*   TO SUPPRESS: REMOVE THE TWO BAL R14,DBGOUT / BAL R14,DBGDYN CALLS*
*   AND THE DBGOUT/DBGDYN SUBROUTINES AND THEIR WORKING STORAGE.      *
* ------------------------------------------------------------------ *
*
* REGISTER EQUATES
*
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
*
* SYMBOLIC CONSTANTS
*
DISPSHR  EQU   X'04'                  SVC 99 STATUS DISP: SHARE
DISPOLD  EQU   X'02'                  SVC 99 STATUS DISP: OLD
DISPMOD  EQU   X'03'                  SVC 99 STATUS DISP: MOD
DCBOFOPN EQU   X'10'                  DCBOFLGS OPEN-SUCCESSFUL BIT
NROWS    EQU   19                     DATA ROWS ON SCREEN (ROWS 4-22)
*
* ------------------------------------------------------------------ *
* PROGRAM ENTRY  -  STANDARD OS LINKAGE                               *
* BASE REGISTERS: R12 (CSECT+0)  R11 (CSECT+4096)  R10 (CSECT+8192) *
* R9 IS A FREE WORK REGISTER THROUGHOUT                               *
* ------------------------------------------------------------------ *
         STM   R14,R12,12(R13)        SAVE CALLER REGISTERS
         BALR  R12,R0                 ESTABLISH FIRST BASE
         USING *,R12,R11,R10
         LA    R11,2048(R12)
         LA    R11,2048(R11)          R11 = CSECT + 4096
         LA    R10,2048(R11)
         LA    R10,2048(R10)          R10 = CSECT + 8192
         ST    R13,SAVEARA+4          BACKWARD SAVE AREA CHAIN
         LA    R15,SAVEARA
         ST    R15,8(R13)             FORWARD CHAIN INTO CALLER'S SA
         LR    R13,R15                R13 -> OUR SAVE AREA
         ST    R1,SAVER1              PRESERVE CPPL/PARM POINTER
*
         MVI   SCRBUF,X'F5'           ERASE/WRITE CLEARS THE
         MVI   SCRBUF+1,X'C3'         TERMINAL SCREEN ON ENTRY
         LA    R1,SCRBUF
         TPUT  (R1),2,FULLSCR
*
*        PARSE DSN AND MEMBER, UPPERCASE THE FULL DSN VIA TR,
*        EMIT DEBUG LINES TO TSO SCROLL MODE, THEN ALLOCATE AND LOAD
         BAL   R14,DBGR1              DEBUG: DUMP SAVER1 AND BUFFER
         BAL   R14,PARSECP            PARSE DSN AND MEMBER
*
*        UPPERCASE ENTIRE DSN USING TRANSLATE TABLE (CATCHES ALL CASES)
         LH    R2,TU0002L             DSN LENGTH
         LTR   R2,R2
         BZ    NOUCDSN                NOTHING TO UPPERCASE
         BCTR  R2,R0                  TR LENGTH = ACTUAL - 1
         EX    R2,TRUCDSN             TRANSLATE IN PLACE
NOUCDSN  EQU   *
*
         BAL   R14,DBGOUT             DEBUG: DSN/MBR/FLAGS TO TSO
*
         MVI   TU0004D,DISPSHR        INITIAL ALLOCATION IS DISP=SHR
         BAL   R14,DYNALL             ALLOCATE DDNAME SYSASMEU
         BAL   R14,DBGDYN             DEBUG: DYNALL RESULT TO TSO
*
*        PAUSE: DISPLAY PROMPT AND WAIT FOR ENTER BEFORE FULLSCREEN
         LA    R1,DBGPAUS
         TPUT  (R1),38
         TGET  INBUF,512,ASIS
*
         LTR   R15,R15
         BNZ   MAINLP                 ALLOC FAILED - ENTER LOOP
         BAL   R14,LOADP              LOAD RECORDS INTO BUFFER
*
* ------------------------------------------------------------------ *
* MAIN EVENT LOOP
* ------------------------------------------------------------------ *
MAINLP   BAL   R14,DRAWSCN            PAINT SCREEN
         TGET  INBUF,512,ASIS         WAIT FOR AID KEY
         LR    R2,R1                  R2 = BYTES RETURNED BY TGET
         ST    R2,TGTLEN              SAVE TGET LENGTH FOR DOENTR
         MVI   AIDBYTE,X'7D'          DEFAULT TO ENTER AID
         LTR   R2,R2
         BZ    ACHECK                 ZERO BYTES - USE DEFAULT
         MVC   AIDBYTE(1),INBUF       CAPTURE AID BYTE
*
ACHECK   CLI   AIDBYTE,X'F3'          PF3  = EXIT
         BE    EXITPGM
         CLI   AIDBYTE,X'F7'          PF7  = SCROLL UP
         BE    DOUP
         CLI   AIDBYTE,X'F8'          PF8  = SCROLL DOWN
         BE    DODN
         CLI   AIDBYTE,X'FA'          PF10 = SAVE
         BE    DOSAVE
         CLI   AIDBYTE,X'7D'          ENTER = PROCESS EDITS
         BE    DOENTR
         B     MAINLP                 UNKNOWN AID - REPAINT
*
* ------------------------------------------------------------------ *
* DOENTR  -  PROCESS ENTER KEY
*
* 3270 INPUT FORMAT: AID(1) + CURSOR-SBA(2) + [X'11'+SBA(2)+DATA]... *
* ONLY FIELDS WITH MDT SET ARE RETURNED BY THE TERMINAL.              *
* ------------------------------------------------------------------ *
DOENTR   L     R2,TGTLEN
         SH    R2,=H'3'               SUBTRACT AID(1) + CURSOR-SBA(2)
         BNP   MAINRE                 NOTHING TO PROCESS
         LA    R3,INBUF+3             R3 -> FIRST FIELD ORDER
*
ENTRLP   LTR   R2,R2
         BNP   MAINRE
         CLI   0(R3),X'11'            SBA ORDER?
         BNE   ENTRNXT
*
*        COMMAND LINE: ATTRIBUTE AT ROW2+20 = BUF POS 100 (SBA X'C164')
*        FIRST DATA AT ROW2+21 = BUF POS 101 (SBA X'C165')
*        ACCEPT EITHER - DIFFERENT EMULATORS REPORT DIFFERENT POSITIONS
         CLC   1(2,R3),=X'C164'
         BE    CAPCMD
         CLC   1(2,R3),=X'C165'
         BE    CAPCMD
*
*        DATA ROW: SCAN 38-ENTRY SBADATA TABLE.
*        ENTRIES 0-18  = ATTRIBUTE-BYTE SBAs (WHAT SOME EMULATORS SEND)
*        ENTRIES 19-37 = FIRST-DATA-BYTE SBAs (WHAT OTHERS SEND)
         LA    R15,SBADATA
         LA    R7,38                  SCAN ALL 38 ENTRIES
CHKDLP   CLC   1(2,R3),0(R15)
         BE    CAPDAT
         LA    R15,2(R15)
         BCT   R7,CHKDLP
         B     ENTRNXT                SBA NOT OURS
*
* CAPDAT: FIELD MATCHED.
*         R7 = REMAINING COUNT (38 DOWN TO 1).
*         ROW OFFSET = (38 - R7) MOD 19  (0=ROW4 .. 18=ROW22)
CAPDAT   LA    R1,38
         SR    R1,R7                  R1 = TABLE POSITION (0..37)
         LA    R6,19
         XR    R0,R0                  CLEAR HIGH WORD OF DIVIDEND
         DR    R0,R6                  R0 = R1 MOD 19 (REMAINDER)
         LR    R1,R0                  R1 = ROW OFFSET (0..18)
         L     R6,TOPREC              R6 = TOP-OF-SCREEN RECORD INDEX
         AR    R6,R1                  R6 = ABSOLUTE RECORD INDEX
         LA    R1,1(R6)               R1 = NEW RECCNT IF WE EXTEND
         C     R1,RECCNT
         BNH   CAPDGO                 RECORD ALREADY EXISTS
         C     R1,=F'200'
         BH    CAPDGO                 AT CAPACITY - DO NOT EXTEND
         ST    R1,RECCNT              EXTEND RECORD COUNT
CAPDGO   LR    R8,R6
         MH    R8,=H'80'              BYTE OFFSET = INDEX * 80
         LA    R8,RECS(R8)            R8 -> TARGET RECORD
         LA    R5,3(R3)               R5 -> FIRST DATA BYTE
         BAL   R14,CAPSYNC
         B     ENTRLP
*
* CAPCMD: COMMAND LINE.  IN PDS MODE, SWITCH TO THE NAMED MEMBER.
CAPCMD   LA    R5,3(R3)               R5 -> FIRST DATA BYTE
         LA    R8,CMDLINE
         BAL   R14,CAPFLD8            COPY UP TO 8 CHARS INTO CMDLINE
         CLI   CMDLINE,X'40'          BLANK = EMPTY COMMAND?
         BE    MAINRE
         CLI   ISPDS,X'01'            PDS MODE?
         BNE   MAINRE                 NO - IGNORE FOR SEQUENTIAL
         MVC   MEMNAME,CMDLINE        SET NEW MEMBER NAME
         LA    R3,MEMNAME             BLANK-PAD MEMNAME TO 8
         LA    R9,8
MBPAD    CLI   0(R3),X'40'
         BE    MBPADD
         LA    R3,1(R3)
         BCT   R9,MBPAD
MBPADD   LTR   R9,R9                  R9 = REMAINING BLANK POSITIONS
         BZ    MBPDN                  ALREADY FULL - NOTHING TO PAD
MBPLN    MVI   0(R3),X'40'            BLANK-FILL REMAINDER
         LA    R3,1(R3)
         BCT   R9,MBPLN
MBPDN    MVC   TU0003D,MEMNAME        UPDATE ALLOC MEMBER TU
         LA    R9,8
         STH   R9,TU0003L             MEMBER LENGTH = 8
         BAL   R14,LOADP              LOAD NEW MEMBER
         B     MAINRE
*
ENTRNXT  LA    R3,1(R3)               STEP PAST NON-SBA BYTE
         BCTR  R2,R0
         B     ENTRLP
MAINRE   B     MAINLP
*
* ------------------------------------------------------------------ *
* CAPSYNC  -  COPY ONE 3270 FIELD (UP TO 80 BYTES) INTO DEST          *
*   IN:   R5 -> FIRST DATA BYTE   R8 -> DEST   R2 = INBUF REMAINING  *
*   OUT:  R3 = UPDATED SCAN PTR   R2 DECREMENTED                      *
*   DEST IS PRE-CLEARED TO BLANKS BEFORE COPY BEGINS                  *
* ------------------------------------------------------------------ *
CAPSYNC  SR    R9,R9                  R9 = BYTE COUNTER
         MVC   0(80,R8),BLANKS        PRE-CLEAR 80-BYTE DESTINATION
CAPSLP   LTR   R2,R2
         BNP   CAPSDN
         CLI   0(R5),X'11'            NEXT SBA ORDER = END OF FIELD
         BE    CAPSDN
         MVC   0(1,R8),0(R5)
         LA    R5,1(R5)
         LA    R8,1(R8)
         LA    R9,1(R9)
         BCTR  R2,R0
         CH    R9,=H'80'              REACHED 80-BYTE MAXIMUM?
         BL    CAPSLP
CAPSDN   LR    R3,R5                  UPDATE MASTER SCAN POINTER
         BR    R14
*
* ------------------------------------------------------------------ *
* CAPFLD8  -  COPY ONE 3270 FIELD (UP TO 8 BYTES) INTO DEST           *
*   SAME INTERFACE AS CAPSYNC.  USED FOR COMMAND LINE (8-CHAR MAX).   *
*   CLEARS ONLY 8 BYTES TO AVOID OVERRUN INTO ADJACENT STORAGE.       *
* ------------------------------------------------------------------ *
CAPFLD8  SR    R9,R9                  R9 = BYTE COUNTER
         MVC   0(8,R8),BLANKS8        PRE-CLEAR 8-BYTE DESTINATION
CAPF8LP  LTR   R2,R2
         BNP   CAPF8DN
         CLI   0(R5),X'11'            NEXT SBA ORDER = END OF FIELD
         BE    CAPF8DN
         MVC   0(1,R8),0(R5)
         LA    R5,1(R5)
         LA    R8,1(R8)
         LA    R9,1(R9)
         BCTR  R2,R0
         CH    R9,=H'8'               REACHED 8-BYTE MAXIMUM?
         BL    CAPF8LP
CAPF8DN  LR    R3,R5                  UPDATE MASTER SCAN POINTER
         BR    R14
*
* ------------------------------------------------------------------ *
* SBADATA  -  3270 RETURN SBA FOR EACH DATA FIELD (ROWS 4-22)         *
*
* ENTRIES 0-18:  ATTRIBUTE-BYTE POSITIONS  (ROW-1)*80+13              *
* ENTRIES 19-37: FIRST-DATA-BYTE POSITIONS  (ROW-1)*80+14             *
*
* 3270 SBA ENCODING (12-BIT POSITION -> TWO 6-BIT CHARACTERS):        *
*  0->40  1->C1..9->C9  10->4A..15->4F  16->50  17->D1..25->D9       *
*  26->5A..31->5F  32->60..47->6F  48->F0..63->FF                     *
* ------------------------------------------------------------------ *
SBADATA  DC    X'C3FD'                ROW  4  ATTR POS= 253
         DC    X'C54D'                ROW  5  ATTR POS= 333
         DC    X'C65D'                ROW  6  ATTR POS= 413
         DC    X'C76D'                ROW  7  ATTR POS= 493
         DC    X'C8FD'                ROW  8  ATTR POS= 573
         DC    X'4A4D'                ROW  9  ATTR POS= 653
         DC    X'4B5D'                ROW 10  ATTR POS= 733
         DC    X'4C6D'                ROW 11  ATTR POS= 813
         DC    X'4DFD'                ROW 12  ATTR POS= 893
         DC    X'4F4D'                ROW 13  ATTR POS= 973
         DC    X'505D'                ROW 14  ATTR POS=1053
         DC    X'D16D'                ROW 15  ATTR POS=1133
         DC    X'D2FD'                ROW 16  ATTR POS=1213
         DC    X'D44D'                ROW 17  ATTR POS=1293
         DC    X'D55D'                ROW 18  ATTR POS=1373
         DC    X'D66D'                ROW 19  ATTR POS=1453
         DC    X'D7FD'                ROW 20  ATTR POS=1533
         DC    X'D94D'                ROW 21  ATTR POS=1613
         DC    X'5A5D'                ROW 22  ATTR POS=1693
         DC    X'C3FE'                ROW  4  DATA POS= 254
         DC    X'C54E'                ROW  5  DATA POS= 334
         DC    X'C65E'                ROW  6  DATA POS= 414
         DC    X'C76E'                ROW  7  DATA POS= 494
         DC    X'C8FE'                ROW  8  DATA POS= 574
         DC    X'4A4E'                ROW  9  DATA POS= 654
         DC    X'4B5E'                ROW 10  DATA POS= 734
         DC    X'4C6E'                ROW 11  DATA POS= 814
         DC    X'4DFE'                ROW 12  DATA POS= 894
         DC    X'4F4E'                ROW 13  DATA POS= 974
         DC    X'505E'                ROW 14  DATA POS=1054
         DC    X'D16E'                ROW 15  DATA POS=1134
         DC    X'D2FE'                ROW 16  DATA POS=1214
         DC    X'D44E'                ROW 17  DATA POS=1294
         DC    X'D55E'                ROW 18  DATA POS=1374
         DC    X'D66E'                ROW 19  DATA POS=1454
         DC    X'D7FE'                ROW 20  DATA POS=1534
         DC    X'D94E'                ROW 21  DATA POS=1614
         DC    X'5A5E'                ROW 22  DATA POS=1694
*
* ------------------------------------------------------------------ *
* EXITPGM  -  CLEAR SCREEN AND RETURN TO TSO WITH RC=0               *
* ------------------------------------------------------------------ *
EXITPGM  MVI   SCRBUF,X'F5'
         MVI   SCRBUF+1,X'C3'
         LA    R1,SCRBUF
         TPUT  (R1),2,FULLSCR
         L     R13,SAVEARA+4          RESTORE CALLER SAVE AREA PTR
         LM    R14,R12,12(R13)        RESTORE CALLER REGISTERS
         XR    R15,R15                RC = 0
         BR    R14
*
* ------------------------------------------------------------------ *
* DBGOUT  -  WRITE PARSED PARAMETERS TO TSO IN SCROLL MODE (DEBUG)    *
*
* WRITES TWO LINES BEFORE FULLSCREEN MODE STARTS:                     *
*   SEU DBG1: DSN=[xxxxxxx...] LEN=nnn ISPDS=n                       *
*   SEU DBG2: MBR=[xxxxxxxx]                                          *
*
* USES TPUT WITHOUT FULLSCR SO LINES APPEAR IN NORMAL TSO OUTPUT.     *
* DOES NOT ALTER ANY WORKING STORAGE EXCEPT DBGLN1 AND DBGLN2.       *
* ------------------------------------------------------------------ *
DBGOUT   ST    R14,SDBGO
*
*        BUILD LINE 1:  SEU DBG1: DSN=[44chars] L=nnn P=h
         MVC   DBGLN1,DBGLIN1T        COPY TEMPLATE
*
*        INSERT DSN AT OFFSET 15 (INSIDE THE BRACKETS)
         LH    R2,TU0002L             DSN LENGTH
         LTR   R2,R2
         BZ    DBGNODS                ZERO LENGTH - SKIP
         CH    R2,=H'44'
         BNH   DBGDSNL
         LA    R2,44                  CLAMP TO 44
DBGDSNL  LA    R1,DBGLN1+15           INSIDE '[' AT OFFSET 15
         BCTR  R2,R0
         EX    R2,DBGMVCD             MVC 0(R2+1,R1),TU0002D
         LA    R2,1(R2)               RESTORE ACTUAL LENGTH
DBGNODS  EQU   *
*
*        FORMAT DSN LENGTH AS 3 DECIMAL DIGITS AT OFFSET 63
         LH    R7,TU0002L
         CVD   R7,DBLWRK
         UNPK  DBGLN1+63(3),DBLWRK+6(2)
         OI    DBGLN1+65,X'F0'        CLEAR SIGN NIBBLE
*
*        INSERT ISPDS FLAG AS HEX DIGIT AT OFFSET 69
         MVC   DBGLN1+69(1),ISPDS
         TR    DBGLN1+69(1),HXTAB-240
*
*        SEND LINE 1 (79 CHARS, SCROLL MODE)
         LA    R1,DBGLN1
         TPUT  (R1),79
*
*        BUILD LINE 2:  SEU DBG2: MBR=[8chars] TU3L=nn
         MVC   DBGLN2,DBGLIN2T        COPY TEMPLATE
         MVC   DBGLN2+15(8),MEMNAME   INSERT MEMBER NAME AT OFFSET 15
*
*        FORMAT TU0003L AS 2 DECIMAL DIGITS AT OFFSET 30
         LH    R7,TU0003L
         CVD   R7,DBLWRK
         UNPK  DBGLN2+30(3),DBLWRK+6(2)
         OI    DBGLN2+32,X'F0'        CLEAR SIGN NIBBLE
*        KEEP ONLY THE 2 SIGNIFICANT DIGITS (OFFSETS 30-31)
         MVC   DBGLN2+30(2),DBGLN2+31
         MVI   DBGLN2+32,C' '
*
         LA    R1,DBGLN2
         TPUT  (R1),79
*
         L     R14,SDBGO
         BR    R14
*
* EX TARGET: MVC 0(1,R1),TU0002D  - LENGTH SET BY EX BEFORE CALL
DBGMVCD  MVC   0(1,R1),TU0002D
*
* ------------------------------------------------------------------ *
* DBGDYN  -  WRITE DYNALL RESULT TO TSO IN SCROLL MODE (DEBUG)        *
*
* WRITES ONE LINE:
*   SEU DBG3: DYNALL RC=xxxx S99ERR=xxxx S99INFO=xxxx                 *
* ------------------------------------------------------------------ *
DBGDYN   ST    R14,SDBGD
         ST    R15,DBGSAVRC           SAVE RC FROM DYNALL (R15 LIVE)
         MVC   DBGLN3,DBGLIN3T        COPY TEMPLATE
*
*        FORMAT RC AS 4 HEX DIGITS AT OFFSET 13
         UNPK  DBGLN3+13(5),DBGSAVRC+2(3) UNPACK LOW 3 BYTES
         TR    DBGLN3+13(4),HXTAB-240 TRANSLATE NIBBLES -> PRINTABLE
*
*        FORMAT S99ERR AS 4 HEX DIGITS AT OFFSET 22
         UNPK  DBGLN3+22(5),S99ERR(3)
         TR    DBGLN3+22(4),HXTAB-240
*
*        FORMAT S99INFO AS 4 HEX DIGITS AT OFFSET 33
         UNPK  DBGLN3+33(5),S99INFO(3)
         TR    DBGLN3+33(4),HXTAB-240
*
         LA    R1,DBGLN3
         TPUT  (R1),79
*
         L     R15,DBGSAVRC           RESTORE RC FOR CALLER'S LTR TEST
         L     R14,SDBGD
         BR    R14
*
* ------------------------------------------------------------------ *
* DBGPRE  -  DUMP RAW OPERAND BYTES BEFORE PARSE (DEBUG)            *
*                                                                    *
* CALLED AFTER CPPL/PARM OFFSET ARITHMETIC, BEFORE SKIP LOOP.       *
* R3 = CURRENT SCAN POINTER (POINTS TO FIRST OPERAND BYTE)          *
* R4 = REMAINING LENGTH                                              *
* SAVES AND RESTORES R14.  DOES NOT ALTER R3 OR R4.                 *
*                                                                    *
* OUTPUT LINE FORMAT (79 CHARS):                                     *
*   SEU DBG0: OFF=nnnn R4=nnnn B0=xx B1=xx B2=xx B3=xx B4=xx B5=xx *
* ------------------------------------------------------------------ *
DBGPRE   ST    R14,SDBGP
         ST    R3,DBGP3SAV            SAVE R3 (SCAN POINTER)
         ST    R4,DBGP4SAV            SAVE R4 (REMAINING LEN)
*
         MVC   DBGLN0,DBGLIN0T        COPY TEMPLATE
*
*        FORMAT CPPL OFFSET AS 4 HEX DIGITS AT OFFSET 13
         UNPK  DBGLN0+13(5),DBGCPOFF+2(3)
         TR    DBGLN0+13(4),HXTAB-240
*
*        FORMAT R4 (REMAINING LEN) AS 4 HEX DIGITS AT OFFSET 22
         UNPK  DBGLN0+22(5),DBGP4SAV+2(3)
         TR    DBGLN0+22(4),HXTAB-240
*
*        FORMAT 6 BYTES AT R3 AS HEX PAIRS AT OFFSETS 30,33,36,39,42,45
*        FIRST CHECK R4 >= 1..6 AND ONLY COPY AVAILABLE BYTES
         LA    R7,DBGP3SAV            R7 = ADDRESS OF SAVED R3
         L     R7,0(R7)               R7 = VALUE OF R3 (SCAN PTR)
         LA    R9,6                   LOOP 6 BYTES
         LA    R8,DBGLN0+30           OUTPUT POSITION
DBGPLP   LTR   R4,R4
         BNP   DBGPDN                 NO MORE INPUT BYTES
         MVI   DBGHXWK,X'00'          ISOLATE BYTE: ZERO HIGH HALF
         MVC   DBGHXWK+1(1),0(R7)     PLACE BYTE IN LOW HALF OF WORK
         UNPK  0(3,R8),DBGHXWK(2)     UNPACK ISOLATED BYTE -> 2 NIBBLES
         TR    0(2,R8),HXTAB-240      TRANSLATE TO PRINTABLE HEX
         MVI   2(R8),C' '             SPACE SEPARATOR
         LA    R7,1(R7)
         LA    R8,3(R8)               ADVANCE OUTPUT BY 3 (XX SPACE)
         BCTR  R4,R0
         BCT   R9,DBGPLP
DBGPDN   LA    R1,DBGLN0
         TPUT  (R1),79
*
         L     R4,DBGP4SAV            RESTORE R4
         L     R3,DBGP3SAV            RESTORE R3
         L     R14,SDBGP
         BR    R14
*
* ------------------------------------------------------------------ *
* DBGR1  -  DUMP SAVER1 AND BYTES AT SAVER1 AND ONE INDIRECTION DEEP *
*   LINE A: SEU DBR1: R1=xxxxxxxx  B: xx xx xx xx xx xx              *
*   LINE B: SEU DBR2: ID=xxxxxxxx  B: xx xx xx xx xx xx              *
* ------------------------------------------------------------------ *
DBGR1    ST    R14,SDBGR1
*        LINE A: R1 VALUE AS 8 HEX DIGITS, THEN 6 BYTES AT R1
*        STORE ADDRESS IN DBGR1WK, LOOP THROUGH 4 BYTES FOR HEX
         MVC   DBGLN4,DBGLIN4T
         L     R7,SAVER1              R7 = R1 AT ENTRY
         ST    R7,DBGR1WK             STORE ADDRESS FOR HEX LOOP
         LA    R6,DBGR1WK             R6 -> ADDRESS BYTES
         LA    R8,DBGLN4+17           R8 -> OUTPUT AREA
         LA    R9,4                   4 ADDRESS BYTES = 8 HEX CHARS
DBR1ADR  MVI   DBGHXWK,X'00'
         MVC   DBGHXWK+1(1),0(R6)
         UNPK  0(3,R8),DBGHXWK(2)
         TR    0(2,R8),HXTAB-240
         LA    R6,1(R6)
         LA    R8,2(R8)
         BCT   R9,DBR1ADR
         MVI   0(R8),C' '
         LA    R8,1(R8)
         L     R7,SAVER1
         LA    R9,6
DBR1LP1  MVI   DBGHXWK,X'00'
         MVC   DBGHXWK+1(1),0(R7)
         UNPK  0(3,R8),DBGHXWK(2)
         TR    0(2,R8),HXTAB-240
         MVI   2(R8),C' '
         LA    R7,1(R7)
         LA    R8,3(R8)
         BCT   R9,DBR1LP1
         LA    R1,DBGLN4
         TPUT  (R1),79
*        LINE B: WORD AT SAVER1 AS 8 HEX DIGITS, THEN 6 BYTES THERE
         MVC   DBGLN5,DBGLIN5T
         L     R7,SAVER1
         L     R7,0(R7)               ONE LEVEL INDIRECTION
         ST    R7,DBGR1WK
         LA    R6,DBGR1WK
         LA    R8,DBGLN5+17
         LA    R9,4
DBR2ADR  MVI   DBGHXWK,X'00'
         MVC   DBGHXWK+1(1),0(R6)
         UNPK  0(3,R8),DBGHXWK(2)
         TR    0(2,R8),HXTAB-240
         LA    R6,1(R6)
         LA    R8,2(R8)
         BCT   R9,DBR2ADR
         MVI   0(R8),C' '
         LA    R8,1(R8)
         L     R7,SAVER1
         L     R7,0(R7)
         LA    R9,6
DBR2LP2  MVI   DBGHXWK,X'00'
         MVC   DBGHXWK+1(1),0(R7)
         UNPK  0(3,R8),DBGHXWK(2)
         TR    0(2,R8),HXTAB-240
         MVI   2(R8),C' '
         LA    R7,1(R7)
         LA    R8,3(R8)
         BCT   R9,DBR2LP2
         LA    R1,DBGLN5
         TPUT  (R1),79
         L     R14,SDBGR1
         BR    R14
*
* ------------------------------------------------------------------ *
* DRAWSCN  -  BUILD AND SEND COMPLETE 3270 SCREEN                     *
*
* SCRBUF LAYOUT:
*   2 BYTES    E/W (X'F5') + WCC (X'C2')                              *
*  80 BYTES    ROW 1  TITLE BAR
*  80 BYTES    ROW 2  COMMAND LINE
*  80 BYTES    ROW 3  RPG COLUMN RULER
*  80*19 BYTES ROWS 4-22  EDIT ROWS
*  80 BYTES    ROW 23  PF KEY LEGEND
*  80 BYTES    ROW 24  STATUS LINE
*  TOTAL = 2 + 24*80 = 1922 BYTES                                     *
*
* DATA ROW LAYOUT (80 BYTES, FROM TERMINAL BUFFER POSITION ROW-1*80): *
*  +0   X'11'        SBA ORDER                                        *
*  +1   XX XX        2-BYTE SBA  (ROW COL 1 FROM POSTBL)              *
*  +3   X'1D' X'60'  SF PROTECTED NORMAL  (LINE NUMBER AREA)          *
*  +5   5 BYTES      LINE NUMBER 00001-99999
*  +10  X'1D' X'60'  SF PROTECTED NORMAL  (SEPARATOR)                 *
*  +12  X'1D' X'40'  SF UNPROTECTED NORMAL  (DATA FIELD)              *
*  +14  66 BYTES     RECORD DATA
* ------------------------------------------------------------------ *
DRAWSCN  ST    R14,SDRAW
         LA    R4,SCRBUF
*
         MVI   0(R4),X'F5'            ERASE/WRITE
         MVI   1(R4),X'C2'            WCC: RESET MDT, UNLOCK KEYBOARD
         LA    R4,2(R4)
*
* ROW 1: TITLE BAR  (SBA X'4040' = POS 0)
         MVI   0(R4),X'11'
         MVC   1(2,R4),=X'4040'
         MVI   3(R4),X'1D'
         MVI   4(R4),X'E8'            PROTECTED HIGH INTENSITY
         MVC   5(11,R4),=CL11'SEU EDITOR '
         MVC   16(60,R4),BLANKS       CLEAR DSN/MEMBER DISPLAY AREA
         LH    R2,TU0002L             R2 = DSN LENGTH
         LA    R1,16(R4)              R1 -> DSN AREA IN TITLE ROW
         LTR   R2,R2
         BZ    TITMBR                 NO DSN - SKIP COPY
         CH    R2,=H'44'
         BNH   TITCPY
         LA    R2,44                  CLAMP TO 44
TITCPY   BCTR  R2,R0                  EX LENGTH = ACTUAL - 1
         EX    R2,MVCDSNX             COPY DSN
         LA    R2,1(R2)               RESTORE ACTUAL LENGTH
TITMBR   CLI   ISPDS,X'01'
         BNE   TITDN                  SEQUENTIAL - NO MEMBER
         CLI   MEMNAME,X'40'
         BE    TITDN                  NO MEMBER NAME SET
         AR    R1,R2                  R1 -> BYTE AFTER LAST DSN CHAR
         MVI   0(R1),C'('
         LA    R1,1(R1)
         LA    R9,8                   R9 = MEMBER CHAR LOOP COUNT
         LA    R3,MEMNAME
TITMLP   CLI   0(R3),X'40'            END OF MEMBER NAME?
         BE    TITMDN
         MVC   0(1,R1),0(R3)
         LA    R1,1(R1)
         LA    R3,1(R3)
         BCT   R9,TITMLP
TITMDN   MVI   0(R1),C')'
TITDN    LA    R4,80(R4)
*
* ROW 2: COMMAND LINE  (SBA X'C150' = POS 80)
*        OFFSET +3/+4   = SF/ATTR PROTECTED NORMAL  (LABEL AREA)
*        OFFSET +5..+18 = LABEL TEXT 'COMMAND  ===>'  (14 CHARS)
*        OFFSET +19     = SF
*        OFFSET +20     = ATTR: UNPROTECTED NORMAL  (INPUT FIELD)
*        OFFSET +21     = IC ORDER  (CURSOR PLACED HERE)
*        OFFSET +22..+29= 8 CHARS CMDLINE DATA
*        OFFSET +30     = SF
*        OFFSET +31     = ATTR: PROTECTED NORMAL  (REST OF ROW)
*        TERMINAL RETURNS SBA X'C164' OR X'C165' FOR THIS FIELD
         MVI   0(R4),X'11'
         MVC   1(2,R4),=X'C150'
         MVI   3(R4),X'1D'
         MVI   4(R4),X'60'
         MVC   5(14,R4),=CL14'COMMAND  ===>'
         MVI   19(R4),X'1D'
         MVI   20(R4),X'40'           UNPROTECTED INPUT FIELD
         MVI   21(R4),X'13'           IC ORDER: CURSOR TO THIS POSITION
         MVC   22(8,R4),CMDLINE       DISPLAY PRIOR COMMAND VALUE
         MVI   30(R4),X'1D'
         MVI   31(R4),X'60'           PROTECTED NORMAL REST OF ROW
         MVC   32(48,R4),BLANKS
         LA    R4,80(R4)
*
* ROW 3: RPG COLUMN RULER  (SBA X'C260' = POS 160)
         MVI   0(R4),X'11'
         MVC   1(2,R4),=X'C260'
         MVI   3(R4),X'1D'
         MVI   4(R4),X'60'            PROTECTED NORMAL
         MVC   5(75,R4),RULER
         LA    R4,80(R4)
*
* ROWS 4-22: 19 DATA EDIT ROWS
         LA    R5,4                   R5 = SCREEN ROW (4..22)
         L     R6,TOPREC              R6 = RECORD INDEX
DRWLP    LR    R1,R5
         BCTR  R1,R0
         SLL   R1,1                   (ROW-1)*2 = OFFSET INTO POSTBL
         LA    R15,POSTBL
         AR    R15,R1                 R15 -> POSTBL ENTRY FOR THIS ROW
         MVI   0(R4),X'11'
         MVC   1(2,R4),0(R15)         ROW COL 1 SBA FROM POSTBL
         MVI   3(R4),X'1D'
         MVI   4(R4),X'60'            PROTECTED NORMAL: LINE NUMBER
         LA    R7,1(R6)               R7 = LINE NUMBER (1-BASED)
         CVD   R7,DBLWRK
         UNPK  LINUM,DBLWRK+5(3)      3 PACKED BYTES -> 5 DECIMAL DIGIT
         OI    LINUM+4,X'F0'          CLEAR SIGN NIBBLE
         MVC   5(5,R4),LINUM
         MVI   10(R4),X'1D'
         MVI   11(R4),X'60'           PROTECTED NORMAL: SEPARATOR
         MVI   12(R4),X'1D'
         MVI   13(R4),X'40'           UNPROTECTED NORMAL: DATA FIELD
         C     R6,RECCNT              RECORD EXISTS?
         BNL   DRWBLK                 NO - PAST EOF
         LR    R8,R6
         MH    R8,=H'80'
         LA    R8,RECS(R8)            R8 -> RECORD IN RECS BUFFER
         MVC   14(66,R4),0(R8)        COPY 66 CHARS OF RECORD DATA
         B     DRWNXT
DRWBLK   MVC   14(66,R4),BLANKS       BLANK LINE (PAST EOF)
DRWNXT   LA    R4,80(R4)
         LA    R5,1(R5)
         LA    R6,1(R6)
         CH    R5,=H'23'              STOP AFTER ROW 22
         BL    DRWLP
*
* ROW 23: PF KEY LEGEND  (SBA X'5B60' = POS 1760)
         MVI   0(R4),X'11'
         MVC   1(2,R4),=X'5B60'
         MVI   3(R4),X'1D'
         MVI   4(R4),X'E8'            PROTECTED HIGH INTENSITY
         MVC   5(75,R4),BLANKS
         MVC   5(38,R4),=CL38'PF3=EXIT  PF7=UP  PF8=DOWN  PF10=SAVE'
         LA    R4,80(R4)
*
* ROW 24: STATUS LINE  (SBA X'5CF0' = POS 1840)
         MVI   0(R4),X'11'
         MVC   1(2,R4),=X'5CF0'
         MVI   3(R4),X'1D'
         MVI   4(R4),X'60'
         MVC   5(8,R4),STATMSG        STATUS MESSAGE (8 CHARS)
         MVI   13(R4),X'1D'
         MVI   14(R4),X'60'
         MVC   15(13,R4),=CL13' RECS LOADED:'
         L     R7,RECCNT
         CVD   R7,DBLWRK
         UNPK  RCNTFLD,DBLWRK+6(2)    2 PACKED BYTES -> 3 DECIMAL DIGIT
         OI    RCNTFLD+2,X'F0'        CLEAR SIGN NIBBLE
         MVC   28(3,R4),RCNTFLD
         MVI   31(R4),X'1D'
         MVI   32(R4),X'60'           PROTECTED: CLOSE STATUS FIELD
*
         LA    R1,SCRBUF
         TPUT  (R1),1922,FULLSCR      2 + 24*80 = 1922 BYTES
         L     R14,SDRAW
         BR    R14
*
* ------------------------------------------------------------------ *
* PARSECP  -  PARSE DSN AND OPTIONAL MEMBER FROM CPPL OR PARM         *
*
* CPPL PATH (TSO COMMAND PROCESSOR):
*   SAVER1 -> CPPL  CPPL+0 -> COMMAND BUFFER                          *
*   COMMAND BUFFER: H'TOTLEN' || H'OFFSET' || VERB || SPACES || OPNDS *
*   OFFSET IS FROM BUFFER START TO FIRST OPERAND CHARACTER            *
*
* PARM PATH (CALLED PROGRAM):
*   SAVER1 WITH HIGH BIT SET -> PARM:  H'LEN' || DATA                 *
*
* OUTPUTS: TU0002D/L  TU0003D/L  MEMNAME  ISPDS                       *
*
* NOTE: UPPERCASE FOLDING IS DONE CHAR-BY-CHAR HERE AND ALSO VIA TR   *
* ON THE FULL DSN BUFFER AFTER RETURN, ENSURING ALL CHARACTERS ARE    *
* UPPERCASED REGARDLESS OF EBCDIC RANGE EDGE CASES.                   *
* ------------------------------------------------------------------ *
PARSECP  ST    R14,SPARSE
         MVC   TU0002D,BLANKS44       CLEAR DSN BUFFER
         MVC   MEMNAME,BLANKS8        CLEAR DISPLAY MEMBER NAME
         MVC   TU0003D,BLANKS8        CLEAR ALLOC MEMBER NAME
         XR    R6,R6
         STH   R6,TU0002L             DSN LENGTH = 0
         STH   R6,TU0003L             MEMBER LENGTH = 0
         MVI   ISPDS,X'00'            DEFAULT: SEQUENTIAL
         L     R1,SAVER1
*        IF R1 ITSELF HAS HIGH BIT SET IT IS A PARM POINTER.
*        CPPL POINTER NEVER HAS HIGH BIT SET (24-BIT ADDRESS).
         LTR   R1,R1
         BM    CPRSSPRM               R1 HIGH BIT SET = PARM PATH
*        CPPL PATH
*        R1 -> CPPL.  CPPL+0 = COMMAND BUFFER ADDRESS (HIGH BIT SET).
*        CLEAR HIGH BIT TO GET REAL ADDRESS, THEN SKIP THE 4-BYTE
*        HEADER, SKIP THE VERB, SKIP BLANKS TO REACH THE OPERAND.
         L     R3,0(R1)               R3 = CMD BUF ADDR (HIGH BIT SET)
         N     R3,=X'7FFFFFFF'        CLEAR HIGH BIT -> REAL ADDRESS
         LH    R4,0(R3)               R4 = TOTAL BUFFER LENGTH
         LH    R5,2(R3)               R5 = OFFSET FIELD (FOR DBG ONLY)
         ST    R5,DBGCPOFF            SAVE FOR DBG0 DISPLAY
         LA    R3,4(R3)               SKIP 4-BYTE HEADER TO VERB TEXT
         SH    R4,=H'4'               ADJUST REMAINING COUNT
         BNP   PRSDDN                 BUFFER TOO SHORT - NO OPERANDS
*        SKIP VERB (NON-BLANK CHARACTERS)
SKPVERB  CLI   0(R3),C' '
         BE    SKPBTWN
         LA    R3,1(R3)
         BCTR  R4,R0
         LTR   R4,R4
         BP    SKPVERB
         B     PRSDDN                 ALL VERB, NO OPERANDS
*        SKIP BLANKS BETWEEN VERB AND OPERAND
SKPBTWN  CLI   0(R3),C' '
         BNE   CPLDUMP
         LA    R3,1(R3)
         BCTR  R4,R0
         LTR   R4,R4
         BP    SKPBTWN
         B     PRSDDN                 NO OPERANDS AFTER VERB
CPLDUMP  BAL   R14,DBGPRE             DBG0: DUMP RAW OPERAND BYTES
         B     CPRSLLP
CPRSSPRM L     R3,0(R1)
         N     R3,=X'7FFFFFFF'        CLEAR HIGH BIT TO GET ADDRESS
         LH    R4,0(R3)               R4 = PARM DATA LENGTH
         LA    R3,2(R3)               R3 -> PARM DATA
         MVC   DBGCPOFF,=F'0'         PARM PATH: OFFSET N/A
         BAL   R14,DBGPRE             DBG0: DUMP RAW OPERAND BYTES
CPRSLLP  LTR   R4,R4
         BNP   PRSDDN                 NOTHING TO PARSE
PRSSKP   CLI   0(R3),C' '             SKIP LEADING BLANKS
         BE    ADVSKP
         CLI   0(R3),X'7D'            SKIP LEADING QUOTES
         BE    ADVSKP
         B     PRSDSN
ADVSKP   LA    R3,1(R3)
         BCTR  R4,R0
         LTR   R4,R4
         BP    PRSSKP
         B     PRSDDN
PRSDSN   LA    R5,TU0002D             R5 -> DSN OUTPUT BUFFER
         SR    R6,R6                  R6 = DSN CHAR COUNT
DSNLP    CLI   0(R3),C'('             MEMBER NAME DELIMITER?
         BE    FNDMBR
         CLI   0(R3),C' '             SPACE TERMINATES DSN
         BE    PDSDONE
         CLI   0(R3),X'7D'            CLOSING QUOTE TERMINATES DSN
         BE    PDSDONE
         CH    R6,=H'44'              AT 44-CHAR DSN MAXIMUM?
         BNL   PDSDONE
         MVC   0(1,R5),0(R3)          COPY ONE DSN CHARACTER
         CLI   0(R5),X'81'            EBCDIC LOWERCASE a (X'81')?
         BL    DSNUT
         CLI   0(R5),X'A9'            EBCDIC LOWERCASE z (X'A9')?
         BH    DSNUT
         OI    0(R5),X'40'            FOLD LOWERCASE TO UPPERCASE
DSNUT    LA    R3,1(R3)
         LA    R5,1(R5)
         LA    R6,1(R6)
         BCTR  R4,R0
         LTR   R4,R4
         BP    DSNLP
         B     PDSDONE
FNDMBR   STH   R6,TU0002L             SAVE DSN LENGTH
         LA    R3,1(R3)               SKIP PAST '('
         BCTR  R4,R0
         BNP   PRSDDN
         LA    R5,MEMNAME             R5 -> DISPLAY MEMBER BUFFER
         LA    R7,TU0003D             R7 -> ALLOC MEMBER BUFFER
         SR    R6,R6                  R6 = MEMBER CHAR COUNT
MBRLP    CLI   0(R3),C')'             ')' TERMINATES MEMBER NAME
         BE    MBRDN
         CLI   0(R3),C' '             SPACE TERMINATES MEMBER NAME
         BE    MBRDN
         CH    R6,=H'8'               AT 8-CHAR MEMBER NAME MAXIMUM?
         BNL   MBRDN
         MVC   0(1,R5),0(R3)          COPY TO DISPLAY BUFFER
         MVC   0(1,R7),0(R3)          COPY TO ALLOC BUFFER
         CLI   0(R5),X'81'
         BL    MBRNXT
         CLI   0(R5),X'A9'
         BH    MBRNXT
         OI    0(R5),X'40'            FOLD DISPLAY COPY TO UPPERCASE
         OI    0(R7),X'40'            FOLD ALLOC COPY TO UPPERCASE
MBRNXT   LA    R3,1(R3)
         LA    R5,1(R5)
         LA    R7,1(R7)
         LA    R6,1(R6)
         BCTR  R4,R0
         LTR   R4,R4
         BP    MBRLP
MBRDN    STH   R6,TU0003L             SAVE MEMBER LENGTH
         MVI   ISPDS,X'01'            SET PDS MODE FLAG
         B     PRSDDN
PDSDONE  STH   R6,TU0002L             SAVE DSN LENGTH
         XR    R6,R6
         STH   R6,TU0003L             MEMBER LENGTH = 0 (SEQUENTIAL)
PRSDDN   L     R14,SPARSE
         BR    R14
*
* ------------------------------------------------------------------ *
* DYNALL  -  DYNAMIC ALLOCATION VIA SVC 99                            *
*
* 1.  FREE SYSASMEU (IGNORE RC - MAY NOT EXIST YET)                   *
* 2.  ALLOCATE WITH TU0004D STATUS DISP (SET BY CALLER BEFORE CALL)   *
*     PDS MODE:  TEXT UNIT LIST S99TUAL (INCLUDES MEMBER TU0003)      *
*     SEQ MODE:  TEXT UNIT LIST S99TUSE (NO MEMBER TU)                *
* ON FAILURE: ERROR CODE FROM S99ERR FORMATTED INTO STATMSG AS E-XXXX *
* ------------------------------------------------------------------ *
DYNALL   ST    R14,SDYN
         MVI   S99VERB,X'02'          VERB: FREE
         LA    R1,S99RBPTR
         LA    R15,S99TUFR
         ST    R15,S99TUPLP
         SVC   99                     IGNORE RC - DD MAY NOT EXIST
         MVI   S99VERB,X'01'          VERB: ALLOCATE
         LA    R1,S99RBPTR
         CLI   ISPDS,X'01'
         BE    DYNPDS
         LA    R15,S99TUSE            SEQ: NO MEMBER TEXT UNIT
         B     DYNGO
DYNPDS   LA    R15,S99TUAL            PDS: MEMBER TEXT UNIT INCLUDED
DYNGO    ST    R15,S99TUPLP
         SVC   99
         LTR   R15,R15
         BZ    DYOK
         MVC   STATMSG(2),=CL2'E-'   PREFIX ERROR CODE
         UNPK  STATMSG+2(5),S99ERR(3) UNPACK 3 BYTES TO 5 HEX CHARS
         TR    STATMSG+2(4),HXTAB-240 TRANSLATE TO PRINTABLE HEX
         MVI   STATMSG+6,C' '
DYOK     L     R14,SDYN
         BR    R14
HXTAB    DC    C'0123456789ABCDEF'
*
* ------------------------------------------------------------------ *
* LOADP  -  LOAD DATASET INTO RECS BUFFER (MAX 200 RECORDS)           *
*
* DD SYSASMEU MUST BE ALLOCATED BEFORE CALLING (BY DYNALL).           *
* PDS: RDJFCB, INJECT MEMBER AT JFCB+44, OPEN TYPE=J                  *
* SEQ: STANDARD OPEN(INPUT)
* SETS NEWMEMF=X'01' IF ZERO RECORDS LOADED (NEW OR EMPTY FILE).      *
* ------------------------------------------------------------------ *
LOADP    ST    R14,SLOAD
         MVI   NEWMEMF,X'00'          ASSUME EXISTING FILE
         CLI   ISPDS,X'01'
         BNE   LOADSEQ
         RDJFCB (INDCB)               READ JFCB INTO JFCBI
         MVC   JFCBI+44(8),MEMNAME    INJECT MEMBER NAME AT JFCB+44
         OPEN  (INDCB,(INPUT)),TYPE=J  OPEN USING PATCHED JFCB
         B     LOADCHK
LOADSEQ  OPEN  (INDCB,(INPUT))
LOADCHK  TM    INDCB+48,DCBOFOPN      OPEN SUCCESSFUL?
         BZ    LNF                    NO - TREAT AS NEW FILE
         LA    R7,RECS                R7 -> RECS BUFFER
         SR    R8,R8                  R8 = RECORD COUNT
LLP      GET   INDCB,0(R7)            READ NEXT 80-BYTE RECORD
         LA    R7,80(R7)              ADVANCE BUFFER POINTER
         LA    R8,1(R8)               INCREMENT COUNT
         CH    R8,=H'200'             AT 200-RECORD MAXIMUM?
         BL    LLP
LEO      CLOSE (INDCB)
         ST    R8,RECCNT
         LTR   R8,R8
         BNZ   LOKLD                  AT LEAST ONE RECORD LOADED
         MVI   NEWMEMF,X'01'          ZERO RECORDS = NEW/EMPTY FILE
LOKLD    MVC   STATMSG,LDMSG
         B     LOK
LNF      CLOSE (INDCB)
         SR    R8,R8
         ST    R8,RECCNT
         MVI   NEWMEMF,X'01'          FILE NOT FOUND = NEW FILE
         MVC   STATMSG,NFMSG
LOK      XR    R15,R15
         ST    R15,TOPREC             RESET SCROLL TO TOP OF FILE
         L     R14,SLOAD
         BR    R14
*
* ------------------------------------------------------------------ *
* DOSAVE  -  WRITE RECS BUFFER BACK TO DATASET                        *
*
* NEWMEMF=X'00':  DISP=OLD  (OVERWRITE EXISTING MEMBER OR FILE)       *
* NEWMEMF=X'01':  DISP=MOD  (CREATE NEW MEMBER OR EXTEND NEW FILE)    *
* ------------------------------------------------------------------ *
DOSAVE   ST    R14,SSAVE
         CLI   NEWMEMF,X'01'
         BE    DSMOD
         MVI   TU0004D,DISPOLD        EXISTING FILE - DISP=OLD
         B     DSALLOC
DSMOD    MVI   TU0004D,DISPMOD        NEW FILE - DISP=MOD
DSALLOC  BAL   R14,DYNALL             RE-ALLOCATE WITH WRITE DISP
         LTR   R15,R15
         BNZ   SFAIL                  ALLOC FAILED
         CLI   ISPDS,X'01'
         BNE   SAVESEQ
         RDJFCB (OUTDCB)              READ JFCB INTO JFCBO
         MVC   JFCBO+44(8),MEMNAME    INJECT MEMBER NAME AT JFCB+44
         OPEN  (OUTDCB,(OUTPUT)),TYPE=J OPEN USING PATCHED JFCB
         B     SAVECHK
SAVESEQ  OPEN  (OUTDCB,(OUTPUT))
SAVECHK  TM    OUTDCB+48,DCBOFOPN     OPEN SUCCESSFUL?
         BZ    SFAIL
         LA    R7,RECS                R7 -> RECS BUFFER
         L     R8,RECCNT              R8 = NUMBER OF RECORDS TO WRITE
         LTR   R8,R8
         BZ    SCLO                   NOTHING TO WRITE
SLP      PUT   OUTDCB,0(R7)           WRITE ONE 80-BYTE RECORD
         LA    R7,80(R7)
         BCT   R8,SLP
SCLO     CLOSE (OUTDCB)
         MVI   TU0004D,DISPSHR        RESTORE DISP=SHR
         BAL   R14,DYNALL             RE-ALLOCATE SHR FOR READS
         MVI   NEWMEMF,X'00'          FILE NOW EXISTS
         MVC   STATMSG,SVMSG
         B     SDN
SFAIL    MVC   STATMSG,ERMSG
         MVI   TU0004D,DISPSHR        ALWAYS RESTORE SHR ON FAILURE
         BAL   R14,DYNALL
SDN      L     R14,SSAVE
         BR    R14
*
* ------------------------------------------------------------------ *
* DOUP / DODN  -  SCROLL UP OR DOWN ONE SCREENFUL (19 LINES)          *
* DODN CLAMPS TOPREC TO MAX(0, RECCNT-19) SO THE LAST PAGE OF DATA   *
* IS ALWAYS ANCHORED WITH AT LEAST ONE RECORD VISIBLE AT TOP.         *
* ------------------------------------------------------------------ *
DOUP     L     R15,TOPREC
         SH    R15,=H'19'
         BP    UPOK
         SR    R15,R15                CLAMP AT ZERO
UPOK     ST    R15,TOPREC
         B     MAINLP
*
DODN     L     R15,TOPREC
         AH    R15,=H'19'
         L     R6,RECCNT
         SH    R6,=H'19'              R6 = MAX PERMITTED TOPREC
         BP    DNCLMP
         SR    R6,R6                  CLAMP AT ZERO IF RECCNT < 19
DNCLMP   CR    R15,R6
         BNH   DNOK
         LR    R15,R6                 CLAMP AT MAX
DNOK     ST    R15,TOPREC
         B     MAINLP
*
         LTORG
*
* ------------------------------------------------------------------ *
* WORKING STORAGE
* ------------------------------------------------------------------ *
         DS    0D
SAVEARA  DC    18F'0'                 OS STANDARD 72-BYTE SAVE AREA
RECCNT   DC    F'0'                   NUMBER OF RECORDS IN RECS
TOPREC   DC    F'0'                   INDEX OF TOP VISIBLE RECORD
TGTLEN   DC    F'0'                   TGET BYTE COUNT
DBLWRK   DC    D'0'                   CVD WORK DOUBLEWORD
AIDBYTE  DC    X'00'                  AID BYTE FROM LAST TGET
ISPDS    DC    X'00'                  X'01'=PDS  X'00'=SEQUENTIAL
NEWMEMF  DC    X'00'                  X'01'=NEW OR EMPTY FILE
SAVER1   DC    F'0'                   ENTRY R1: CPPL OR PARM POINTER
SDRAW    DC    F'0'                   BAL RETURN SAVES
SLOAD    DC    F'0'
SSAVE    DC    F'0'
SPARSE   DC    F'0'
SDYN     DC    F'0'
SDBGO    DC    F'0'                   DBGOUT RETURN SAVE
SDBGD    DC    F'0'                   DBGDYN RETURN SAVE
SDBGP    DC    F'0'                   DBGPRE RETURN SAVE
SDBGR1   DC    F'0'                   DBGR1 RETURN SAVE
DBGR1WK  DC    XL8'0000000000000000'   DBGR1 WORK (8 BYTES, ADDR AT +4)
DBGSAVRC DC    F'0'                   DYNALL RC SAVE FOR DBGDYN
DBGCPOFF DC    F'0'                   CPPL OFFSET SAVE FOR DBGPRE
DBGP3SAV DC    F'0'                   DBGPRE R3 SAVE
DBGHXWK  DC    XL2'0000'              DBGPRE SINGLE-BYTE HEX WORK AREA
DBGP4SAV DC    F'0'                   DBGPRE R4 SAVE
LINUM    DC    CL5' '                 LINE NUMBER WORK FIELD
RCNTFLD  DC    CL3' '                 RECORD COUNT WORK FIELD
CMDLINE  DC    CL8' '                 COMMAND LINE INPUT BUFFER
MEMNAME  DC    CL8' '                 CURRENT MEMBER NAME (DISPLAY)
BLANKS   DC    CL80' '
BLANKS44 DC    CL44' '
BLANKS8  DC    CL8' '
MVCDSNX  MVC   0(1,R1),TU0002D        EX TARGET: COPY DSN TO TITLE ROW
TRUCDSN  TR    TU0002D(1),UCTAB       EX TARGET: UPPERCASE FULL DSN
STATMSG  DC    CL8'READY   '
LDMSG    DC    CL8'LOADED  '
NFMSG    DC    CL8'NEW FILE'
SVMSG    DC    CL8'SAVED   '
ERMSG    DC    CL8'ERR-SAVE'
*
* EBCDIC UPPERCASE TRANSLATE TABLE
* MAPS LOWERCASE a-z (X'81'-X'89', X'91'-X'99', X'A2'-X'A9') TO
* UPPERCASE A-Z (X'C1'-X'C9', X'D1'-X'D9', X'E2'-X'E9').
* ALL OTHER CHARACTERS MAP TO THEMSELVES.
UCTAB    DC    256AL1(*-UCTAB)        INITIALISE: EACH BYTE = ITS INDEX
         ORG   UCTAB+X'81'
         DC    C'ABCDEFGHI'           a-i -> A-I
         ORG   UCTAB+X'91'
         DC    C'JKLMNOPQR'           j-r -> J-R
         ORG   UCTAB+X'A2'
         DC    C'STUVWXYZ'            s-z -> S-Z
         ORG   ,
*
* DEBUG LINE TEMPLATES AND LIVE OUTPUT BUFFERS
*
* LINE 0 LAYOUT (79 CHARS) - PRE-PARSE RAW OPERAND DUMP:
*  OFFSET  0-12: 'SEU DBG0: OFF='  (13 CHARS)
*  OFFSET 13-16: CPPL OFFSET HEX   ( 4 CHARS, FILLED BY DBGPRE)
*  OFFSET 17-21: ' R4='            ( 5 CHARS)  (WAIT - ' RL=')
*  OFFSET 22-25: REMAINING LEN HEX ( 4 CHARS, FILLED BY DBGPRE)
*  OFFSET 26-29: ' B0='            ( 4 CHARS)
*  OFFSET 30-31: BYTE 0 HEX        ( 2 CHARS)
*  OFFSET 32-34: ' B1='  ... etc up to B5
SEUVERN  DC    CL4'V04 '              BUILD VERSION NUMBER
DBGLIN4T DC    CL79'SEU V04 DBR1: R1=         '
DBGLIN5T DC    CL79'SEU V04 DBR2: ID=         '
DBGLN4   DC    CL79' '
DBGLN5   DC    CL79' '
*
DBGLIN0T DC    CL13'SEU DBG0: OFF='
         DC    CL4'    '
         DC    CL5' RL= '
         DC    CL4'    '
         DC    CL4' B0='
         DC    CL3'   '
         DC    CL4' B1='
         DC    CL3'   '
         DC    CL4' B2='
         DC    CL3'   '
         DC    CL4' B3='
         DC    CL3'   '
         DC    CL3'   '
         DC    CL22' '
DBGLN0   DC    CL79' '
*
* LINE 1 LAYOUT (79 CHARS):
*  OFFSET  0-14: 'SEU DBG1: DSN=['  (15 CHARS, '[' AT OFFSET 14)
*  OFFSET 15-58: DSN DATA           (44 CHARS, FILLED BY DBGOUT)
*  OFFSET 59-62: '] L='             ( 4 CHARS)
*  OFFSET 63-65: LEN DIGITS         ( 3 CHARS, FILLED BY DBGOUT)
*  OFFSET 66-68: ' P='              ( 3 CHARS)
*  OFFSET    69: ISPDS HEX DIGIT    ( 1 CHAR,  FILLED BY DBGOUT)
*  OFFSET 70-78: SPACES
DBGLIN1T DC    CL15'SEU DBG1: DSN=['
         DC    CL44' '
         DC    CL4'] L='
         DC    CL3'   '
         DC    CL3' P='
         DC    CL1' '
         DC    CL9' '
*
* LINE 2 LAYOUT (79 CHARS):
*  OFFSET  0-14: 'SEU DBG2: MBR=['  (15 CHARS)
*  OFFSET 15-22: MEMBER DATA        ( 8 CHARS, FILLED BY DBGOUT)
*  OFFSET 23-29: '] TU3L='          ( 7 CHARS)
*  OFFSET 30-31: TU0003L DIGITS     ( 2 CHARS, FILLED BY DBGOUT)
*  OFFSET 32-78: SPACES
DBGLIN2T DC    CL15'SEU DBG2: MBR=['
         DC    CL8' '
         DC    CL7'] TU3L='
         DC    CL2'  '
         DC    CL47' '
*
* LINE 3 LAYOUT (79 CHARS):
*  OFFSET  0-12: 'SEU DBG3: RC='   (13 CHARS)
*  OFFSET 13-16: RC HEX DIGITS      ( 4 CHARS, FILLED BY DBGDYN)
*  OFFSET 17-21: ' ERR='            ( 5 CHARS)
*  OFFSET 22-25: S99ERR HEX DIGITS  ( 4 CHARS, FILLED BY DBGDYN)
*  OFFSET 26-32: ' INFO='           ( 6 CHARS)
*  OFFSET 33-36: S99INFO HEX DIGITS ( 4 CHARS, FILLED BY DBGDYN)
*  OFFSET 37-78: SPACES
DBGLIN3T DC    CL13'SEU DBG3: RC='
         DC    CL4'    '
         DC    CL5' ERR='
         DC    CL4'    '
         DC    CL6' INFO='
         DC    CL4'    '
         DC    CL43' '
*
* PAUSE PROMPT WRITTEN AFTER DEBUG LINES, BEFORE FULLSCREEN MODE
DBGPAUS  DC    CL38'SEU: PRESS ENTER TO CONTINUE...      '
*
* LIVE OUTPUT BUFFERS (COPIED FROM TEMPLATES THEN PATCHED AT RUNTIME)
DBGLN1   DC    CL79' '
DBGLN2   DC    CL79' '
DBGLN3   DC    CL79' '
*
* RPG COLUMN RULER (75 CHARACTERS COVERING COLS 1-70 PLUS PADDING)
RULER    DC    C'....+....1....+....2....+....3'
         DC    C'....+....4....+....5....+....6'
         DC    C'....+....'
         DC    CL7' '
*
* ------------------------------------------------------------------ *
* POSTBL  -  ROW COL-1 SBA ADDRESSES FOR ROWS 1-24                    *
* POS=(ROW-1)*80.  ENCODED AS TWO 3270 6-BIT CHARACTERS.             *
* ------------------------------------------------------------------ *
POSTBL   DC    X'4040'                ROW  1  POS=   0
         DC    X'C150'                ROW  2  POS=  80
         DC    X'C260'                ROW  3  POS= 160
         DC    X'C3F0'                ROW  4  POS= 240
         DC    X'C540'                ROW  5  POS= 320
         DC    X'C650'                ROW  6  POS= 400
         DC    X'C760'                ROW  7  POS= 480
         DC    X'C8F0'                ROW  8  POS= 560
         DC    X'4A40'                ROW  9  POS= 640
         DC    X'4B50'                ROW 10  POS= 720
         DC    X'4C60'                ROW 11  POS= 800
         DC    X'4DF0'                ROW 12  POS= 880
         DC    X'4F40'                ROW 13  POS= 960
         DC    X'5050'                ROW 14  POS=1040
         DC    X'D160'                ROW 15  POS=1120
         DC    X'D2F0'                ROW 16  POS=1200
         DC    X'D440'                ROW 17  POS=1280
         DC    X'D550'                ROW 18  POS=1360
         DC    X'D660'                ROW 19  POS=1440
         DC    X'D7F0'                ROW 20  POS=1520
         DC    X'D940'                ROW 21  POS=1600
         DC    X'5A50'                ROW 22  POS=1680
         DC    X'5B60'                ROW 23  POS=1760
         DC    X'5CF0'                ROW 24  POS=1840
*
* ------------------------------------------------------------------ *
* SVC 99 (DYNALLOC) REQUEST BLOCK AND TEXT UNITS
*
* S99RBPTR: ADDRESS OF S99RB WITH HIGH BIT SET (REQUIRED BY SVC 99)   *
*
* S99RB  20-BYTE REQUEST BLOCK:
*   +0   AL1(20)  LENGTH
*   +1   AL1      VERB: X'01'=ALLOC  X'02'=FREE  (PATCHED AT RUNTIME) *
*   +2   H'0'     FLAGS1
*   +4   H'0'     ERROR CODE (OUTPUT FROM SVC 99)                     *
*   +6   H'0'     INFO CODE  (OUTPUT FROM SVC 99)                     *
*   +8   A        TU POINTER LIST ADDRESS  (PATCHED AT RUNTIME)       *
*   +12  F'0'     RESERVED
*
* TEXT UNIT FORMAT: X'NNNN' H'COUNT' H'LEN1' DATA1 ...                *
*   TU0001  X'0001'  DDNAME  = SYSASMEU (8 CHARS)                     *
*   TU0002  X'0002'  DSN     = SET BY PARSECP (UP TO 44 CHARS)        *
*   TU0003  X'0003'  MEMBER  = SET BY PARSECP OR CAPCMD (8 CHARS)     *
*   TU0004  X'0004'  STATUS  = DISPSHR / DISPOLD / DISPMOD            *
*   TU0005  X'0005'  NORMAL  = X'01' KEEP                             *
* ------------------------------------------------------------------ *
         DS    0F
S99RBPTR DC    X'80',AL3(S99RB)       HIGH BIT SET REQUIRED BY SVC 99
S99RB    DS    0F
S99RBLN  DC    AL1(20)
S99VERB  DC    AL1(1)                 PATCHED: X'01'=ALLOC  X'02'=FREE
S99FLAG1 DC    H'0'
S99ERR   DC    H'0'                   ERROR CODE SET BY SVC 99
S99INFO  DC    H'0'                   INFO CODE SET BY SVC 99
S99TUPLP DC    A(0)                   PATCHED TO TU LIST BEFORE SVC 99
S99RSV1  DC    F'0'
*
S99TUFR  DC    A(TU0001)              FREE: DDNAME + NORMAL DISP
         DC    X'80',AL3(TU0005)
*
S99TUAL  DC    A(TU0001),A(TU0002)    ALLOC PDS: DDNAME DSN MEMBER
         DC    A(TU0003),A(TU0004)
         DC    X'80',AL3(TU0005)
*
S99TUSE  DC    A(TU0001),A(TU0002)    ALLOC SEQ: DDNAME DSN (NO MEMBER)
         DC    A(TU0004)
         DC    X'80',AL3(TU0005)
*
TU0001   DC    X'0001',H'1',H'8',CL8'SYSASMEU'
TU0002   DC    X'0002',H'1'
TU0002L  DC    H'0'                   DSN LENGTH (SET BY PARSECP)
TU0002D  DC    CL44' '                DSN DATA   (SET BY PARSECP)
TU0003   DC    X'0003',H'1'
TU0003L  DC    H'0'                   MEMBER LENGTH (SET BY PARSECP)
TU0003D  DC    CL8' '                 MEMBER DATA   (SET BY PARSECP)
TU0004   DC    X'0004',H'1',H'1'
TU0004D  DC    X'04'                  STATUS DISP: DISPSHR AT STARTUP
TU0005   DC    X'0005',H'1',H'1',X'01' NORMAL DISP: KEEP
*
* ------------------------------------------------------------------ *
* DCB DEFINITIONS
* INDCB AND OUTDCB EACH HAVE A SEPARATE JFCB BUFFER AND EXIT LIST.    *
* JFCB LAYOUT: +0 DSNAME(44)  +44 MEMBER(8)  +52 VOLSER(6)  ...      *
* EXIT LIST ENTRY X'87' TELLS OPEN TO READ THE JFCB INTO OUR BUFFER.  *
* ------------------------------------------------------------------ *
INDCB    DCB   DDNAME=SYSASMEU,DSORG=PS,MACRF=(GM),RECFM=FB,           X
               LRECL=80,EODAD=LEO,EXLST=EXLSTI
OUTDCB   DCB   DDNAME=SYSASMEU,DSORG=PS,MACRF=(PM),RECFM=FB,           X
               LRECL=80,EXLST=EXLSTO
*
JFCBI    DS    CL176                  JFCB FOR INDCB
EXLSTI   DS    0F
         DC    X'87',AL3(JFCBI)       X'87' = RDJFCB EXIT TYPE
         DC    X'00',AL3(0)           END OF EXIT LIST
*
JFCBO    DS    CL176                  JFCB FOR OUTDCB
EXLSTO   DS    0F
         DC    X'87',AL3(JFCBO)
         DC    X'00',AL3(0)
*
         DS    0F
SCRBUF   DS    CL2200                 3270 OUTPUT BUFFER (1922 BYTES)
         DS    0F
INBUF    DS    CL512                  TGET INPUT BUFFER
         DS    0F
RECS     DS    200CL80                RECORD STORAGE: 200 * 80 = 16000
         END   SEU
$$
//*
//* COMPILER JCL FOR IFOX00 ON MVS 3.8J
//*
//ASM      EXEC PGM=IFOX00,PARM='OBJ,NODECK,LIST'
//SYSLIB   DD  DSN=SYS1.MACLIB,DISP=SHR
//         DD  DSN=SYS1.AMODGEN,DISP=SHR
//SYSUT1   DD  UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT2   DD  UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT3   DD  UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSPRINT DD  SYSOUT=A
//SYSGO    DD  DSN=&&OBJSET,UNIT=SYSDA,SPACE=(80,(200,200)),
//             DISP=(MOD,PASS)
//SYSIN    DD  DSN=HERC01.SOURCE.ASM(SEU),DISP=SHR
//*
//LINK     EXEC PGM=IEWL,PARM='LIST,XREF,LET,MAP'
//SYSLIN   DD  DSN=&&OBJSET,DISP=(OLD,DELETE)
//SYSLMOD  DD  DSN=HERC01.TEST.LOADLIB(SEU),DISP=SHR
//SYSUT1   DD  UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSPRINT DD  SYSOUT=A