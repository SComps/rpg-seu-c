SEU      CSECT
* ================================================================ *
* SEU - SIMPLE TSO EDITOR FOR MVS 3.8J / IFOX00                   *
* CLIST ALLOCATES SYSASMEU DD BEFORE CALLING THIS PROGRAM.        *
* PROGRAM READS, DISPLAYS, AND SAVES SEQUENTIAL / PDS MEMBERS.    *
* PF3=EXIT  PF7=UP  PF8=DOWN  PF10=SAVE                           *
* ================================================================ *
         STM   14,12,12(13)        SAVE ALL REGISTERS
         BALR  12,0                ESTABLISH BASE R12
         USING *,12,11,10          TRIPLE BASE: 12KB
         LA    11,2048(12)         R11 = BASE + 4096 (2X2048)
         LA    11,2048(11)
         LA    10,2048(11)         R10 = BASE + 8192 (2X2048)
         LA    10,2048(10)
*
         ST    13,SAVEARA+4        CHAIN SAVEAREAS
         LA    15,SAVEARA
         ST    15,8(13)
         LR    13,15
*
         BAL   14,LOADP            READ DATASET INTO RECS
*
* ================================================================ *
* MAIN EVENT LOOP                                                  *
* ================================================================ *
MAINLP   BAL   14,DRAWSCN          BUILD SCREEN BUFFER
         L     1,CURPTR
         LA    0,SCRBUF
         SR    1,0                 BUFFER LENGTH
         LR    0,1                 R0 = LEN FOR SVC 93
         TPUT  SCRBUF,(0),FULLSCR  DISPLAY FULL SCREEN
*
         TGET  INBUF,512,ASIS      READ TERMINAL INPUT
         MVI   AIDBYTE,X'7D'       DEFAULT: NO KEY
         LTR   1,1
         BZ    ACHECK
         MVC   AIDBYTE(1),INBUF    CAPTURE AID BYTE
*
ACHECK   CLI   AIDBYTE,X'F3'       PF3 = EXIT
         BE    EXITPGM
         CLI   AIDBYTE,X'FA'       PF10 = SAVE
         BE    DOSAVE
         CLI   AIDBYTE,X'F7'       PF7 = SCROLL UP
         BE    DOUP
         CLI   AIDBYTE,X'F8'       PF8 = SCROLL DOWN
         BE    DODN
         B     MAINLP
*
* ================================================================ *
* EXIT                                                             *
* ================================================================ *
EXITPGM  L     13,SAVEARA+4
         LM    14,12,12(13)
         XR    15,15
         BR    14
*
* ================================================================ *
* SCROLL UP AND DOWN                                               *
* ================================================================ *
DOUP     L     15,TOPREC
         S     15,=F'18'
         BP    DUPOK
         SR    15,15
DUPOK    ST    15,TOPREC
         B     MAINLP
*
DODN     L     15,TOPREC
         A     15,=F'18'
         L     6,RECCNT
         S     6,=F'18'
         BP    DDNCK
         SR    6,6
DDNCK    CR    15,6
         BNH   DDNOK
         LR    15,6
DDNOK    ST    15,TOPREC
         B     MAINLP
*
* ================================================================ *
* DOSAVE: WRITE ALL RECORDS BACK TO SYSASMEU                      *
* ================================================================ *
DOSAVE   ST    14,SSAVE
         MVC   STATMSG,SAVMSG
         OPEN  (OUTDCB,(OUTPUT))
         TM    OUTDCB+48,X'10'     OPEN OK?
         BZ    SFAIL
         LA    7,RECS
         L     8,RECCNT
         LTR   8,8
         BZ    SCLOSE
SLOOPP   PUT   OUTDCB,0(7)
         LA    7,80(7)
         BCT   8,SLOOPP
SCLOSE   CLOSE (OUTDCB)
         B     SDONE
SFAIL    MVC   STATMSG,SERMSG
SDONE    L     14,SSAVE
         BR    14
SSAVE    DS    F
*
* ================================================================ *
* LOADP: OPEN INPUT DCB, READ UP TO 50 RECORDS                    *
* ================================================================ *
LOADP    ST    14,SLOAD
         OPEN  (INDCB,(INPUT))
         TM    INDCB+48,X'10'      OPEN OK?
         BZ    LNEW
         LA    7,RECS
         SR    8,8
LLOOPP   GET   INDCB,0(7)
         LA    7,80(7)
         LA    8,1(8)
         CH    8,=H'50'
         BL    LLOOPP
LEOF     CLOSE (INDCB)
         ST    8,RECCNT
         MVC   STATMSG,LDMSG
         B     LRTN
LNEW     SR    8,8
         ST    8,RECCNT
         MVC   STATMSG,NEWMSG
LRTN     L     14,SLOAD
         BR    14
SLOAD    DS    F
*
* ================================================================ *
* DRAWSCN: BUILD 3270 FULLSCREEN BUFFER IN SCRBUF                 *
* ================================================================ *
DRAWSCN  ST    14,SDRAW
         LA    4,SCRBUF            R4 = BUFFER WRITE POINTER
* HEADER ROW 1
         MVI   0(4),X'11'          SBA ORDER
         MVC   1(2,4),=X'4040'     ROW 1 COL 1
         MVI   3(4),X'1D'          SF ORDER
         MVI   4(4),X'E8'          PROTECTED + HIGH INTENSITY
         MVC   5(26,4),HDRTXT
         LA    4,31(4)
* STATUS ROW 24
         MVI   0(4),X'11'          SBA ORDER
         MVC   1(2,4),=X'5C20'     ROW 24 COL 1
         MVI   3(4),X'1D'          SF ORDER
         MVI   4(4),X'E8'          PROTECTED + HIGH INTENSITY
         MVC   5(30,4),STATMSG
         LA    4,35(4)
* DATA ROWS 3-20 (18 ROWS)
         LA    5,3                 SCREEN ROW COUNTER
         L     6,TOPREC            RECORD INDEX (0-BASED)
DRWLP    LA    15,POSTBL           SBA POSITION TABLE
         LR    1,5
         SLL   1,1                 2 BYTES PER ENTRY
         AR    15,1
*
         MVI   0(4),X'11'          SBA
         MVC   1(2,4),0(15)        POSITION BYTES
         MVI   3(4),X'1D'          SF PROTECTED
         MVI   4(4),X'60'
         LA    7,1(6)              LINE NUMBER (1-BASED)
         CVD   7,DBLWRK
         UNPK  LINUM,DBLWRK+5(3)
         OI    LINUM+4,X'F0'
         MVC   5(5,4),LINUM        5-CHAR LINE NUMBER
         MVI   10(4),X'1D'         SF UNPROTECTED
         MVI   11(4),X'40'
         LA    8,RECS
         LR    15,6
         MH    15,=H'80'
         AR    8,15
         MVC   12(80,4),0(8)       COPY RECORD TEXT
         LA    4,92(4)             ADVANCE BUFFER PTR
         LA    5,1(5)              NEXT ROW
         LA    6,1(6)              NEXT RECORD
         CH    5,=H'21'            DONE AFTER ROW 20
         BL    DRWLP
         ST    4,CURPTR            SAVE END OF BUFFER
         L     14,SDRAW
         BR    14
SDRAW    DS    F
*
* ================================================================ *
* DATA AREAS                                                       *
* ================================================================ *
SAVEARA  DC    18F'0'
RECCNT   DC    F'0'
TOPREC   DC    F'0'
DBLWRK   DC    D'0'
AIDBYTE  DC    X'00'
CURPTR   DC    F'0'
LINUM    DC    CL5' '
STATMSG  DC    CL30'LOADING...'
HDRTXT   DC    CL26'SEU EDITOR  PF3=EXIT PF10='
LDMSG    DC    CL30'LOADED'
NEWMSG   DC    CL30'NEW MEMBER'
SAVMSG   DC    CL30'SAVING...'
SERMSG   DC    CL30'SAVE FAILED'
AIDPF3   EQU   X'F3'
AIDPF7   EQU   X'F7'
AIDPF8   EQU   X'F8'
AIDPF10  EQU   X'FA'
FULLSCR  EQU   X'03'
ASIS     EQU   X'01'
*
* SBA POSITION TABLE FOR ROWS 1-20 (2 BYTES EACH)
POSTBL   DC    X'4040'             ROW 1  COL 1
         DC    X'4040'             ROW 2  COL 1 (UNUSED)
         DC    X'C150'             ROW 3  COL 1
         DC    X'C260'             ROW 4  COL 1
         DC    X'C3F0'             ROW 5  COL 1
         DC    X'C540'             ROW 6  COL 1
         DC    X'C650'             ROW 7  COL 1
         DC    X'C760'             ROW 8  COL 1
         DC    X'C8F0'             ROW 9  COL 1
         DC    X'4A40'             ROW 10 COL 1
         DC    X'4B50'             ROW 11 COL 1
         DC    X'4C60'             ROW 12 COL 1
         DC    X'4DF0'             ROW 13 COL 1
         DC    X'4F40'             ROW 14 COL 1
         DC    X'5050'             ROW 15 COL 1
         DC    X'D160'             ROW 16 COL 1
         DC    X'D2F0'             ROW 17 COL 1
         DC    X'D440'             ROW 18 COL 1
         DC    X'D550'             ROW 19 COL 1
         DC    X'D660'             ROW 20 COL 1
         DC    X'D7F0'             ROW 21 COL 1 (UNUSED)
*
INDCB    DCB   DDNAME=SYSASMEU,DSORG=PS,MACRF=(GM),RECFM=FB,           X
               LRECL=80,EODAD=LEOF
OUTDCB   DCB   DDNAME=SYSASMEU,DSORG=PS,MACRF=(PM),RECFM=FB,LRECL=80
*
         LTORG
*
SCRBUF   DS    CL3000
INBUF    DS    CL512
RECS     DS    50CL80
*
         END   SEU
