SEU      CSECT
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
* ================================================================ *
* SEU - SCREEN EDITING UTILITY FOR MVS 3.8J / IFOX00               *
* MODELED AFTER IBM SEU FOR RPG II WORKBENCH ON S/3X               *
* ----------------------------------------------------------------- *
* FIXES VS PRIOR BUILD:                                             *
*   1. DRAWSCN NOW USES ERASE/WRITE (X'F5') - CLEAN REPAINT ALWAYS *
*   2. POSTBL CORRECTED FOR ALL 24 ROWS (ROW 21 WAS OFF BY 1       *
*      BUFFER-ADDRESS GROUP, CAUSING LINE 18 TO OVERLAP LINE 17)   *
*   3. STATUS BAR SBA CORRECTED TO ROW 23 COL 1 (X'5B60')          *
*   4. DATA ROWS EXPANDED TO 19 (ROWS 4-22)                        *
*   5. RECORD CAPACITY EXPANDED TO 200 MEMBERS                     *
* ================================================================ *
*
* STANDARD OS LINKAGE
         STM   R14,R12,R12(R13)
         BALR  R12,R0
         USING *,R12,R11,R10,R9   QUAD BASE REGS: 16K COVERAGE
         LA    R11,2048(R12)
         LA    R11,2048(R11)
         LA    R10,2048(R11)
         LA    R10,2048(R10)
         LA    R9,2048(R10)
         LA    R9,2048(R9)
         ST    R13,SAVEARA+4
         LA    R15,SAVEARA
         ST    R15,R8(R13)
         LR    R13,R15
         ST    R1,SAVER1
*
* STFSMODE N/A IN MVS 3.8J - TPUT FULLSCR HANDLES SCREEN I/O
*
* TRIPLE ERASE/WRITE TO GUARANTEE CLEAN SCREEN ON ENTRY
         LA    R4,SCRBUF
         MVI   SCRBUF,X'F5'
         MVI   SCRBUF+1,X'C3'
         LA    R1,SCRBUF
         TPUT  (R1),1927,FULLSCR
         LA    R1,SCRBUF
         TPUT  (R1),1927,FULLSCR
         LA    R1,SCRBUF
         TPUT  (R1),1927,FULLSCR
*
* PARSE DSN/MEMBER FROM PARM OR CPPL, ALLOCATE, LOAD
         BAL    R14,PARSECP
         BAL    R14,DYNALL
         LTR    R15,R15              DID ALLOC SUCCEED?
         BNZ   MAINLP             IF NOT, SHOW ERROR AND WAIT
         BAL    R14,LOADP
*
* ================================================================ *
* MAIN EVENT LOOP                                                   *
* ================================================================ *
MAINLP                R14,DRAWSCN
         TGET  INBUF,512,ASIS
         LR    R2,R1                 R2 = BYTES RETURNED
         MVI   AIDBYTE,X'7D'      DEFAULT = ENTER AID
         LTR    R2,R2
         BZ    ACHECK
         MVC   AIDBYTE(R1),INBUF   CAPTURE ACTUAL AID BYTE
*
ACHECK   CLI   AIDBYTE,X'F3'      PF3  = EXIT
         BE    EXITPGM
         CLI   AIDBYTE,X'F7'      PF7  = SCROLL UP
         BE    DOUP
         CLI   AIDBYTE,X'F8'      PF8  = SCROLL DOWN
         BE    DODN
         CLI   AIDBYTE,X'FA'      PF10 = SAVE
         BE    DOSAVE
         CLI   AIDBYTE,X'7D'      ENTER = COMMAND LINE / EDITS
         BE    DOENTR
         B     MAINLP
*
* ================================================================ *
* DOENTR - PARSE 3270 INPUT BUFFER ON ENTER                        *
* ================================================================ *
DOENTR   EQU   *
         L     R2,SAVER1           R2 = TOTAL BYTES
         SH    R2,=H'3'            MINUS AID/CURSOR
         BNP   MAINRE             NO DATA
         LA    R3,INBUF+3          R3 -> FIRST DATA ORDER
*
ENTRLP   CLI   0(R3),X'11'         SBA ORDER?
         BNE   ENTRNXT
*        MATCH SBA ADDRESSES
*        CMD LINE: ROW 2 COL 21 (X'C164')
         CLC   1(2,R3),=X'C164'
         BE    CAPCMD
*        DATA ROWS: COL 13 (STARTING X'C37C')
         LA    R15,SBADATA         SBA TABLE
         LA    R7,R19               19 ROWS
CHKDLP   CLC   1(2,R3),0(R15)       MATCH?
         BE    CAPDAT
         LA    R15,R2(R15)
         BCT   R7,CHKDLP
         B     ENTRNXT            NOT MANAGED
*
CAPDAT   L     R6,TOPREC           OFFSET
         LA    R1,19
         SR    R1,R7                R1 = VISIBLE OFF
         AR    R6,R1                R6 = FILE INDEX
*        UPDATE COUNT
         LA    R1,R1(R6)
         C     R1,RECCNT
         BNH   CAPDGO
         ST    R1,RECCNT           GROW
CAPDGO   MH    R6,=H'80'           OFFSET
         LA    R8,RECS
         AR    R8,R6                R8 -> RECORD
         LA    R5,R3(R3)             R5 -> TYPED DATA
         BAL   R14,CAPSYNC         SYNC FIELD
         B     ENTRLP               PROCESS NEXT SBA IN BUFFER
*
CAPCMD   LA    R8,CMDLINE
         MVC   0(8,R8),BLANKS
         LA    R5,R3(R3)
         BAL   R14,CAPSYNC
         CLI   CMDLINE,X'40'      EMPTY?
         BE    MAINRE
         MVC   MEMNAME,CMDLINE    SYNC
         BAL   R14,LOADP
         B     MAINRE
*
CAPSYNC  EQU   *
         SR    R9,R9              COUNT
         MVC   0(80,R8),BLANKS    CLEAR LINE BEFORE CAPTURE
CAPSLP   CLI   0(R5),X'11'        NEXT SBA?
         BE    CAPSDN
         LTR   R2,R2              END OF BUFFER?
         BNP   CAPSDN
         MVC   0(1,R8),0(R5)      COPY BYTE
         LA    R5,R1(R5)
         LA    R8,R1(R8)
         LA    R9,R1(R9)
         BCTR  R2,R0              DECREMENT REMAINING BYTES
         CH    R9,=H'80'          MAX LINE LEN
         BL    CAPSLP
CAPSDN   LR    R3,R5              ADVANCE MASTER POINTER
         BR    R14
*
ENTRNXT  LA    R3,R1(R3)             STEP PAST NON-SBA BYTE
         BCTR  R2,R0
         LTR   R2,R2
         BP    ENTRLP
MAINRE   B     MAINLP
*
* SBAs (COL 13)
SBADATA  DC    X'C37C',X'C54C',X'C65C',X'C76C',X'C87C'
         DC    X'4A4C',X'4B5C',X'4C6C',X'4D7C',X'4F4C'
         DC    X'505C',X'D16C',X'D27C',X'D44C',X'D55C'
         DC    X'D66C',X'D77C',X'D94C',X'5A5C'
*
* ================================================================ *
* EXIT - SEND FULL BLANK SCREEN THEN RETURN RC=0                   *
* ================================================================ *
EXITPGM  EQU   *
         LA    R4,SCRBUF
         MVI   0(R4),X'F5'
         MVI   1(R4),X'C3'
         MVI   2(R4),X'11'
         MVI   3(R4),X'40'
         MVI   4(R4),X'40'
         MVI   5(R4),X'1D'
         MVI   6(R4),X'60'
         LA    R2,1920            LEN TO CLEAR
         LA    R3,R7(R4)           START DATA
EXITCLR  MVI   0(R3),C' '         CLEAR SCREEN
         LA    R3,R1(R3)
         BCT    R2,EXITCLR
         LA    R1,SCRBUF
         TPUT  (R1),1927,FULLSCR
         L     R13,SAVEARA+4
         LM    R14,R12,R12(R13)
         XR    R15,R15
         BR    R14
*
* ================================================================ *
* DRAWSCN - BUILD AND SEND 3270 FULLSCREEN DATASTREAM              *
*                                                                   *
* SCREEN MAP (80 X 24):                                            *
*  ROW 1  : TITLE BAR  - DSN(MEMBER)  BRIGHT PROTECTED            *
*  ROW 2  : COMMAND LINE - COMMAND ===>  (INPUT FIELD)             *
*  ROW 3  : RPG COLUMN RULER (PROTECTED)                           *
*  ROWS 4-22: EDITOR CONTENT (19 DATA ROWS, UNPROTECTED)           *
*  ROW 23 : PF-KEY LEGEND (BRIGHT PROTECTED)                       *
*  ROW 24 : STATUS (RECORD COUNT, POSITION)                        *
*                                                                   *
* USES ERASE/WRITE (X'F5') ON EVERY CALL - NO SCREEN GHOSTS        *
* ================================================================ *
DRAWSCN  ST    R14,SDRAW
         LA    R4,SCRBUF            R4 = BUFFER WRITE POINTER
* --- ERASE/WRITE COMMAND + WCC ---
         MVI   0(R4),X'F5'
         MVI   1(R4),X'C2'
         LA    R4,R2(R4)
* --- ROW 1: TITLE BAR ---
         MVI   0(R4),X'11'
         MVC   1(2,R4),=X'4040'
         MVI   3(R4),X'1D'         SF ORDER
         MVI   4(R4),X'E8'         ATTR: PROTECTED, HIGH INTENSITY
         MVC   5(11,R4),=CL11'SEU EDITOR '
         MVC   16(60,R4),BLANKS    BLANK DSN+MEMBER AREA
         LH    R2,TU0002L          R2 = DSN LENGTH (0..44)
         LTR   R2,R2
         BZ    TITMBR             ZERO LENGTH - SKIP DSN COPY
         CH    R2,=H'44'
         BH    TITMX              CLAMP TO 44
         B     TITCPY
TITMX    LA    R2,44
TITCPY   LA    R1,16(R4)           R1 -> DSN AREA IN BUFFER
         BCTR  R2,R0               MVC LENGTH MINUS 1
         EX    R2,MVCDSNX         COPY DSN BYTES
         LA    R2,R1(R2)             RESTORE ACTUAL LENGTH
TITMBR   CLI   MEMNAME,X'40'      MEMBER SPECIFIED?
         BE    TITDN              NO - SKIP MEMBER APPEND
         AR    R1,R2                R1 -> BYTE AFTER LAST DSN CHAR
         MVI   0(R1),C'('
         LA    R1,R1(R1)
         LA    R2,R8                MAX MEMBER LEN
         LA    R3,MEMNAME          R3 -> SOURCE
TITMLP   CLI   0(R3),X'40'         END?
         BE    TITMDN
         MVC   0(1,R1),0(R3)        COPY CHAR
         LA    R1,R1(R1)
         LA    R3,R1(R3)
         BCT   R2,TITMLP
TITMDN   MVI   0(R1),C')'
TITDN    EQU   *
         LA    R4,80(R4)            ADVANCE TO NEXT ROW SLOT
*
* --- ROW 2: COMMAND LINE ---
*     SBA ROW 2 COL 1 = X'C150'  (POS 80:  80/64=1 REM 16)
         MVI   0(R4),X'11'
         MVC   1(2,R4),=X'C150'
         MVI   3(R4),X'1D'
         MVI   4(R4),X'60'         ATTR: PROTECTED NORMAL
         MVC   5(14,R4),=CL14'COMMAND  ===>'
         MVI   19(R4),X'1D'        SF: UNPROTECTED INPUT
         MVI   20(R4),X'40'        ATTR: UNPROTECTED NORMAL
         MVI   21(R4),X'13'        INSERT CURSOR (IC)
         MVC   22(8,R4),CMDLINE    PRIOR COMMAND VALUE
         MVI   30(R4),X'1D'        SF: FILL REST PROTECTED
         MVI   31(R4),X'60'
         MVC   32(48,R4),BLANKS
         LA    R4,80(R4)
* --- ROW 3: RPG COLUMN RULER ---
         MVI   0(R4),X'11'
         MVC   1(2,R4),=X'C260'
         MVI   3(R4),X'1D'
         MVI   4(R4),X'60'         ATTR: PROTECTED NORMAL
         MVC   5(75,R4),RULER      75 CHARS OF RULER
         LA    R4,80(R4)
* --- ROWS 4-22: DATA ROWS (19 ROWS) ---
         LA    R5,R4                SCREEN ROW 4
         L     R6,TOPREC           STARTING RECORD
DRWLP    EQU   *
         LA    R15,POSTBL          R15 -> POSTBL BASE
         LR    R1,R5                R1 = ROW
         BCTR  R1,R0                R1 = ROW-1
         SLL   R1,R1                R1 = (ROW-1)*2
         AR    R15,R1               R15 -> POSTBL[ROW-1]
         MVI   0(R4),X'11'         SBA ORDER
         MVC   1(2,R4),0(R15)       ROW SBA ADDRESS
         MVI   3(R4),X'1D'         SF (COL 1)
         MVI   4(R4),X'60'         ATTR: PROTECTED (COL 2)
         LA    R7,R1(R6)             LINE NUMBER = INDEX+1
         CVD   R7,DBLWRK
         UNPK  LINUM,DBLWRK+5(R3)
         OI    LINUM+4,X'F0'
         MVC   5(5,R4),LINUM       5-DIGIT LINE NUMBER (COL 3-7)
         MVI   10(R4),X'1D'        SF (COL 11)
         MVI   11(R4),X'60'        ATTR: PROTECTED SEP (COL 12)
         MVI   12(R4),X'1D'        SF (COL 13)
         MVI   13(R4),X'40'        ATTR: UNPROTECTED DATA (COL 14)
         C     R6,RECCNT           BEYOND EOF?
         BNL   DRWBLKR
         LA    R8,RECS
         LR    R1,R6
         MH    R1,=H'80'
         AR    R8,R1
         MVC   14(66,R4),0(R8)     COPY 66 CHARS
         B     DRWNXT
DRWBLKR  MVC   14(66,R4),BLANKS
DRWNXT   LA    R4,80(R4)
         LA    R5,R1(R5)
         LA    R6,R1(R6)
         CH    R5,=H'23'
         BL    DRWLP
*
* --- ROW 23: PF-KEY LEGEND ---
*     SBA ROW 23 COL 1 = X'5B60'  (POS 1760: 1760/64=27 REM 32)
         MVI   0(R4),X'11'
         MVC   1(2,R4),=X'5B60'
         MVI   3(R4),X'1D'
         MVI   4(R4),X'E8'         ATTR: PROTECTED HIGH INTENSITY
         MVC   5(75,R4),BLANKS
         MVC   5(55,R4),=CL55'PF3=EXIT  PF7=UP  PF8=DOWN  PF10=SAVE'
         LA    R4,80(R4)
*
* --- ROW 24: STATUS LINE ---
*     SBA ROW 24 COL 1 = X'5CF0'  (POS 1840: 1840/64=28 REM 48)
         MVI   0(R4),X'11'
         MVC   1(2,R4),=X'5CF0'
         MVI   3(R4),X'1D'
         MVI   4(R4),X'60'         ATTR: PROTECTED NORMAL
         MVC   5(8,R4),STATMSG     STATUS TEXT
         MVI   13(R4),X'1D'
         MVI   14(R4),X'60'
         MVC   15(13,R4),=CL13' RECS LOADED:'
         LA    R7,0
         L     R7,RECCNT
         CVD    R7,DBLWRK
         UNPK  RCNTFLD,DBLWRK+6(R2)
         OI    RCNTFLD+2,X'F0'
         MVC   28(3,R4),RCNTFLD
         LA    R4,80(R4)
*
* --- SEND BUFFER TO TERMINAL ---
         LR    R3,R4                R3 = END OF BUFFER
         LA    R14,SCRBUF
         SR    R3,R14               R3 = ACTUAL BUFFER LENGTH
         LA    R1,SCRBUF
         TPUT  (R1),1927,FULLSCR
         L     R14,SDRAW
         BR    R14
*
* ================================================================ *
* PARSECP - PARSE DSN AND MEMBER FROM CPPL OR CALL PARM            *
* CPPL: R1 -> CPPL PTR.  CALL: R1 -> PARM (HIGH BIT SET)          *
* OUTPUT: TU0002D = DSN (44 CHARS), MEMNAME = MEMBER (8 CHARS)     *
* ================================================================ *
PARSECP  ST    R14,SPARSE
         MVC   MEMNAME,BLANKS
         MVC   TU0002D,BLANKS
         MVC   MEMNAME,BLANKS
                      R1,SAVER1
         TM    0(R1),X'80'         CALL INVOCATION?
         BO    CPRSSPRM
                      R3,0(R1)             CPPL -> COMMAND BUFFER
         LH    R4,0(R3)             TOTAL LENGTH
         LH    R5,R2(R3)             OFFSET TO DATA
         LA    R3,R4(R3,R5)           POINT PAST VERB
         SR    R4,R5
         SH    R4,=H'4'
         B     CPRSLLP
CPRSSPRM              R3,0(R1)             PARM POINTER
                      R3,=X'7FFFFFFF'     CLEAR HIGH BIT
         LH    R4,0(R3)             PARM LENGTH
         LA    R3,R2(R3)             POINT TO PARM DATA
CPRSLLP  LTR   4,R4
         BNP   PRSDDN
* --- Unified Parse Loop ---
PRSSKP   CLI   0(R3),C' '
         BE    ADVSKP
         CLI   0(R3),X'7D'         QUOTE?
         BE    ADVSKP
         B     PRSDSN
ADVSKP                R3,R1(R3)
         BCTR    R4,R0
         LTR    R4,R4
         BP    PRSSKP
         B     PRSDDN
*
PRSDSN                R5,TU0002D          R5 -> PDS BUF
         SR    R6,R6                R6 = LEN
DSNLP    CLI   0(R3),C'('          MEMBER?
         BE    FNDMBR
         CLI   0(R3),C' '
         BE    PDSDONE
         CLI   0(R3),X'7D'         QUOTE?
         BE    PDSDONE
         MVC   0(1,R5),0(R3)
         CLI   0(R5),X'81'         LOWER?
         BL    DSNUT
         CLI   0(R5),X'A9'
         BH    DSNUT
         OI    0(R5),X'40'         UPPER
DSNUT                 R3,R1(R3)
         LA    R5,R1(R5)
         LA    R6,R1(R6)
         BCTR    R4,R0
         LTR    R4,R4
         BP    DSNLP
         B     PDSDONE
*
FNDMBR                R6,TU0002L
         LA    R3,R1(R3)             SKIP (
         BCTR    R4,R0
         BNP   PRSDDN
         LA    R5,MEMNAME          DISPLAY
         LA    R7,TU0003D          ALLOC
         SR    R6,R6                MLEN
MBRLP    CLI   0(R3),C')'
         BE    MBRDN
         CLI   0(R3),C' '
         BE    MBRDN
         MVC   0(1,R5),0(R3)
         MVC   0(1,R7),0(R3)
         CLI   0(R5),X'81'
         BL    MBRNXT
         CLI   0(R5),X'A9'
         BH    MBRNXT
         OI    0(R5),X'40'
         OI    0(R7),X'40'
MBRNXT                R3,R1(R3)
         LA    R5,R1(R5)
         LA    R7,R1(R7)
         LA    R6,R1(R6)
         BCTR    R4,R0
         LTR    R4,R4
         BP    MBRLP
MBRDN                 R6,TU0003L
         B     PRSDDN
*
PDSDONE               R6,TU0002L
         SR    R6,R6
         STH    R6,TU0003L
PRSDDN   L     R14,SPARSE
         BR    R14
*
DYNALL   ST    R14,SDYN
         DS    0H
         MVI   S99VERB,X'02'      FREE
         LA    R1,S99RBPTR
         MVC   S99TUPLP,PTRFREE
         SVC   99
         MVI   S99VERB,X'01'      ALLOC
         LA    R1,S99RBPTR
         MVC   S99TUPLP,PTRALOC
         SVC   99
         LTR    R15,R15
         BZ    DYOK
         MVC   STATMSG(R2),=CL2'E-'
         UNPK  STATMSG+2(R5),S99ERR(R3)
         TR    STATMSG+2(R4),HXTAB-240
         MVI   STATMSG+6,C' '
DYOK                  R14,SDYN
         BR    R14
HXTAB    DC    C'0123456789ABCDEF'
*
* ================================================================ *
* LOADP - LOAD PDS MEMBER INTO RECS BUFFER (MAX 200 RECORDS)       *
*         USES RDJFCB/OPEN TYPE=J TO INJECT MEMBER NAME INTO JFCB  *
* ================================================================ *
LOADP    ST    R14,SLOAD
         RDJFCB (INDCB)
         CLI   MEMNAME,X'40'      MEMBER SPECIFIED?
         BE    SKPMBR2
         MVC   JFCBBUF+44(R8),MEMNAME
SKPMBR2  OPEN  (INDCB,(INPUT)),TYPE=J
         TM    INDCB+48,X'10'     OPEN SUCCESSFUL?
         BZ    LNF                NO -> FILE NOT FOUND
         LA    R7,RECS
         SR    R8,R8
LLP      GET   INDCB,R0(R7)
         LA    R7,80(R7)
         LA    R8,R1(R8)
         CH    R8,=H'200'          MAX 200 RECORDS
         BL    LLP
LEO      CLOSE (INDCB)
         ST    R8,RECCNT
         MVC   STATMSG,LDMSG
         B     LOK
LNF                   R8,R8
         ST    R8,RECCNT
         MVC   STATMSG,NFMSG
LOK                   R15,R15
         ST    R15,TOPREC          RESET TO TOP OF FILE
                      R14,SLOAD
         BR    R14
*
* ================================================================ *
* DOSAVE - WRITE ALL RECORDS BACK TO PDS MEMBER                    *
* ================================================================ *
DOSAVE   ST    R14,SSAVE
         RDJFCB (OUTDCB)
         CLI   MEMNAME,X'40'
         BE    SKPMBR1
         MVC   JFCBBUF+44(R8),MEMNAME
SKPMBR1  OPEN  (OUTDCB,(OUTPUT)),TYPE=J
         TM    OUTDCB+48,X'10'
         BZ    SFAIL
         LA    R7,RECS
                      R8,RECCNT
         LTR    R8,R8
         BZ    SCLO
SLP      PUT   OUTDCB,R0(R7)
         LA    R7,80(R7)
         BCT    R8,SLP
SCLO     CLOSE (OUTDCB)
         MVC   STATMSG,SVMSG
         B     SDN
SFAIL    MVC   STATMSG,ERMSG
SDN                   R14,SSAVE
         BR    R14
*
* ================================================================ *
* DOUP / DODN - SCROLL UP / DOWN 19 ROWS                          *
* ================================================================ *
DOUP                  R15,TOPREC
         SH    R15,=H'19'
         BP    UPOK
         SR    R15,R15
UPOK                  R15,TOPREC
         B     MAINLP
DODN                  R15,TOPREC
         AH    R15,=H'19'
                      R6,RECCNT
         CR    R15,R6
         BNH   DNOK
         LR    R15,R6
DNOK                  R15,TOPREC
         B     MAINLP
*
         LTORG
*
* ================================================================ *
* WORKING STORAGE                                                   *
* ================================================================ *
         DS    0D                 DOUBLEWORD ALIGNMENT
SAVEARA  DC    18F'0'
RECCNT   DC    F'0'
TOPREC   DC    F'0'
DBLWRK   DC    D'0'
AIDBYTE  DC    X'00'
SAVER1   DC    F'0'
SDRAW    DC    F'0'
SLOAD    DC    F'0'
SSAVE    DC    F'0'
SPARSE   DC    F'0'
SDYN     DC    F'0'
LINUM    DC    CL5' '
RCNTFLD  DC    CL3' '
CMDLINE  DC    CL8' '
MEMNAME  DC    CL8' '
BLANKS   DC    CL80' '
MVCDSNX  MVC   0(1,R1),TU0002D    EX TARGET: COPY DSN (LEN IN R2)
STATMSG  DC    CL8'READY   '
LDMSG    DC    CL8'LOADED  '
NFMSG    DC    CL8'NEW FILE'
SVMSG    DC    CL8'SAVED   '
ERMSG    DC    CL8'ERR-SAVE'
*
* ================================================================ *
* RPG COLUMN RULER (70 VISIBLE DATA COLUMNS)                       *
* ================================================================ *
RULER    DC    C'....+....1....+....2....+....3'
         DC    C'....+....4....+....5....+....6'
         DC    C'....+....'
         DC    CL7' '             PAD TO 75
*
*
* ================================================================ *
* SBA POSITION TABLE (2 BYTES PER ROW, ROWS 1-24)                  *
*                                                                   *
* FORMULA: POS=(ROW-1)*80, HIGH=POS/64, LOW=POS MOD 64            *
* ENCODING: 0->40 1->C1 2->C2 3->C3 4->C4 5->C5 6->C6 7->C7      *
*           8->C8 9->C9 10->4A 11->4B 12->4C 13->4D 14->4E 15->4F  *
*           16->50 17->D1 18->D2 19->D3 20->D4 21->D5 22->D6 23->D7*
*           24->D8 25->D9 26->5A 27->5B 28->5C 29->5D 30->5E 31->5F*
*           32->60 ... 48->F0 ... 63->7F                           *
*                                                                   *
* ROW 21 FIX: WAS X'D840' (POS 1536=ROW20COL17) NOW X'D940'       *
*             X'D940' -> 25*64+0=1600 -> ROW21 COL1  CORRECT       *
* ROW 22 FIX: WAS X'D950' (POS 1616=ROW21COL17) NOW X'5A50'       *
*             X'5A50' -> 26*64+16=1680 -> ROW22 COL1 CORRECT       *
* ================================================================ *
POSTBL   DC    X'4040'            ROW  1: POS    0  (0*64+0)
         DC    X'C150'            ROW  2: POS   80  (1*64+16)
         DC    X'C260'            ROW  3: POS  160  (2*64+32)
         DC    X'C3F0'            ROW  4: POS  240  (3*64+48)
         DC    X'C540'            ROW  5: POS  320  (5*64+0)
         DC    X'C650'            ROW  6: POS  400  (6*64+16)
         DC    X'C760'            ROW  7: POS  480  (7*64+32)
         DC    X'C8F0'            ROW  8: POS  560  (8*64+48)
         DC    X'4A40'            ROW  9: POS  640  (10*64+0)
         DC    X'4B50'            ROW 10: POS  720  (11*64+16)
         DC    X'4C60'            ROW 11: POS  800  (12*64+32)
         DC    X'4DF0'            ROW 12: POS  880  (13*64+48)
         DC    X'4F40'            ROW 13: POS  960  (15*64+0)
         DC    X'5050'            ROW 14: POS 1040  (16*64+16)
         DC    X'D160'            ROW 15: POS 1120  (17*64+32)
         DC    X'D2F0'            ROW 16: POS 1200  (18*64+48)
         DC    X'D440'            ROW 17: POS 1280  (20*64+0)
         DC    X'D550'            ROW 18: POS 1360  (21*64+16)
         DC    X'D660'            ROW 19: POS 1440  (22*64+32)
         DC    X'D7F0'            ROW 20: POS 1520  (23*64+48)
         DC    X'D940'            ROW 21: POS 1600  (25*64+0)  *FIXED*
         DC    X'5A50'            ROW 22: POS 1680  (26*64+16) *FIXED*
         DC    X'5B60'            ROW 23: POS 1760  (27*64+32)
         DC    X'5CF0'            ROW 24: POS 1840  (28*64+48)
*
* ================================================================ *
* SVC 99 REQUEST BLOCK FOR DYNAMIC ALLOCATION                      *
* ================================================================ *
         DS    0F
S99RBPTR DC    X'80',AL3(S99RB)
S99RB    DS    0F
S99RBLN  DC    AL1(20)
S99VERB  DC    AL1(R1)
S99FLAG1 DC    H'0'
S99ERR   DC    H'0'
S99INFO  DC    H'0'
S99TUPLP DC    A(0)               INJECTED AT RUNTIME
S99RSV1  DC    H'0',H'0'
PTRFREE  DC    A(S99TUFR)
PTRALOC  DC    A(S99TUAL)
S99TUFR  DC    A(TU0001)
         DC    X'80',AL3(TU0004)
S99TUAL  DC    A(TU0001),A(TU0002)
         DC    A(TU0003),A(TU0004)
         DC    X'80',AL3(TU0005)
TU0001   DC    X'0001',H'1',H'8',CL8'SYSASMEU'
TU0002   DC    X'0002',H'1'
TU0002L  DC    H'0'
TU0002D  DC    CL44' '
TU0003   DC    X'0003',H'1'
TU0003L  DC    H'0'
TU0003D  DC    CL8' '
TU0004   DC    X'0004',H'1',H'1',X'08'
TU0005   DC    X'0005',H'1',H'1',X'01'
*
* ================================================================ *
* DCB DEFINITIONS AND JFCB AREA                                    *
* ================================================================ *
INDCB    DCB   DDNAME=SYSASMEU,DSORG=PS,MACRF=(GM),RECFM=FB,           X
               LRECL=80,EODAD=LEO,EXLST=EXLST1
OUTDCB   DCB   DDNAME=SYSASMEU,DSORG=PS,MACRF=(PM),RECFM=FB,           X
               LRECL=80,EXLST=EXLST1
JFCBBUF  DS    CL176
EXLST1   DS    0F
         DC    X'87',AL3(JFCBBUF)
         DC    X'00',AL3(0)
*
* ================================================================ *
* I/O BUFFERS AND RECORD STORAGE (200 RECORDS MAX)                 *
* ================================================================ *
         DS    0F
SCRBUF   DS    CL2200             3270 OUTPUT DATASTREAM BUFFER
         DS    0F
INBUF    DS    CL512              TGET INPUT BUFFER
         DS    0F
RECS     DS    200CL80            EDITOR RECORD STORAGE (200*80)
         END   SEU
