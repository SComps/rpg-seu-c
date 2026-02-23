SEU      CSECT
* ================================================================ *
* SEU - SIMPLE TSO EDITOR FOR MVS 3.8J / IFOX00                   *
* CLIST ALLOCATES SYSASMEU DD BEFORE CALLING THIS PROGRAM.        *
* PROGRAM READS, DISPLAYS, AND SAVES SEQUENTIAL / PDS MEMBERS.    *
* PF3=EXIT  PF7=UP  PF8=DOWN  PF10=SAVE  ENTER=APPLY EDITS       *
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
* CLEAR RECORD BUFFER TO SPACES BEFORE LOADING
         LA    0,RECS              DEST ADDR
         L     1,=F'4000'          50 * 80 = 4000
         SR    2,2                 SRC ADDR (UNUSED)
         SR    3,3                 SRC LEN = 0
         ICM   3,8,=X'40'          PAD = SPACE
         MVCL  0,2                 FILL WITH SPACES
*
         BAL   14,LOADP            READ DATASET INTO RECS
         TPUT  STATMSG,30          SHOW LOADED/NEW STATUS
*
* ================================================================ *
* MAIN EVENT LOOP                                                  *
* ================================================================ *
MAINLP   BAL   14,DRAWSCN          BUILD SCREEN BUFFER
         L     1,CURPTR
         LA    0,SCRBUF
         SR    1,0                 BUFFER LENGTH IN R1
         LR    0,1                 COPY TO R0 FOR SVC
         TPUT  SCRBUF,(0),FULLSCR  DISPLAY
         TGET  INBUF,512,ASIS      READ (BLOCKS UNTIL KEY)
         LR    2,1                 SAVE BYTE COUNT
         MVI   AIDBYTE,X'7D'       DEFAULT = ENTER
         LTR   2,2
         BZ    ACHECK
         MVC   AIDBYTE(1),INBUF    AID BYTE FROM DATA STREAM
*
ACHECK   CLI   AIDBYTE,X'F3'       PF3 = EXIT
         BE    EXITPGM
         CLI   AIDBYTE,X'FA'       PF10 = SAVE
         BE    DOSAVE
         CLI   AIDBYTE,X'F7'       PF7 = SCROLL UP
         BE    DOUP
         CLI   AIDBYTE,X'F8'       PF8 = SCROLL DOWN
         BE    DODN
         CLI   AIDBYTE,X'7D'       ENTER = APPLY EDITS
         BE    DOENTER
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
* DOENTER: PARSE TGET RESPONSE AND UPDATE RECS                    *
* ================================================================ *
DOENTER  CH    2,=H'3'            NEED AID+CURSOR (3 BYTES MIN)
         BL    MAINLP             TOO SHORT, SKIP
         LA    3,INBUF+3          SKIP AID(1) + CURSOR ADDR(2)
         LR    9,2                TOTAL TGET LENGTH
         SH    9,=H'3'            REMAINING DATA LENGTH
PRSLP    CH    9,=H'4'            NEED SBA(3) + 1 DATA BYTE MIN
         BL    MAINLP             DONE PARSING
         CLI   0(3),X'11'         SBA ORDER?
         BNE   MAINLP             NOT SBA, DONE
* DECODE SBA: BYTE1=HI6, BYTE2=LO6 -> 12-BIT BUFFER ADDR
         SR    5,5
         IC    5,1(3)             HIGH BYTE
         BAL   14,DECD6           DECODE TO 6-BIT
         SLL   5,6                SHIFT TO HIGH POSITION
         ST    5,DBLWRK           SAVE TEMPORARILY
         SR    5,5
         IC    5,2(3)             LOW BYTE
         BAL   14,DECD6           DECODE TO 6-BIT
         A     5,DBLWRK           COMBINE HI+LO = BUFFER POS
         ST    5,DBLWRK           STORE COMPLETE POSITION
* NOW R5 = 3270 BUFFER POSITION OF FIELD DATA
* MAP POSITION TO SCREEN ROW AND RECORD INDEX
         SR    4,4                ROW COUNTER
         LA    7,FPOSTBL          FIELD POSITION TABLE
         LA    4,0                START AT ROW 0
PRFND    CH    4,=H'18'           CHECKED ALL 18 ROWS?
         BNL   PRSKIP             NOT FOUND, SKIP FIELD
         CLC   0(2,7),DBLWRK+2    COMPARE STORED POS (HALFWORD)
         BE    PRFOUND
         LA    7,2(7)             NEXT TABLE ENTRY
         LA    4,1(4)             NEXT ROW
         B     PRFND
PRFOUND  DS    0H
* R4 = ROW OFFSET (0-17), RECORD = TOPREC + R4
         A     4,TOPREC           ABSOLUTE RECORD INDEX
         C     4,=F'50'           BOUNDS CHECK
         BNL   PRSKIP
* CALCULATE DEST ADDRESS IN RECS
         LR    8,4
         MH    8,=H'80'           OFFSET = RECIDX * 80
         LA    8,RECS(8)          R8 = DEST IN RECS
* COPY FIELD DATA FROM TGET RESPONSE
         LA    3,3(3)             SKIP SBA ORDER (3 BYTES)
         SH    9,=H'3'            ADJUST REMAINING LENGTH
* DETERMINE FIELD DATA LENGTH (UP TO 80 OR NEXT SBA)
         LR    6,9                MAX POSSIBLE LENGTH
         CH    6,=H'80'
         BNH   PRLNOK
         LA    6,80               CAP AT 80
PRLNOK   LR    7,6                SAVE MAX
         SR    6,6                ACTUAL LENGTH COUNTER
PRSCN    CR    6,7                REACHED MAX?
         BNL   PRCOPY
         LR    15,3               COPY BASE PTR
         AR    15,6               ADD OFFSET
         CLI   0(15),X'11'        NEXT SBA ORDER?
         BE    PRCOPY             STOP HERE
         LA    6,1(6)
         B     PRSCN
PRCOPY   DS    0H
* CLEAR DEST FIELD TO SPACES FIRST
         MVC   0(80,8),BLANKS
* COPY ACTUAL DATA (R6 = LENGTH)
         LTR   6,6
         BZ    PRPADV             NO DATA TO COPY
         BCTR  6,0                LENGTH-1 FOR EX
         EX    6,PRMVC            MVC 0(?,8),0(3)
         LA    6,1(6)             RESTORE LENGTH
PRPADV   LA    3,0(6,3)           ADVANCE INPUT POINTER
         SR    9,6                ADJUST REMAINING LENGTH
* UPDATE RECCNT IF NEW RECORD BEYOND CURRENT COUNT
         LA    4,1(4)             RECIDX + 1
         C     4,RECCNT
         BNH   PRSLP
         ST    4,RECCNT           EXPAND RECORD COUNT
         B     PRSLP
PRSKIP   DS    0H
* SKIP PAST THIS SBA + ITS DATA
         LA    3,3(3)             SKIP SBA ORDER
         SH    9,=H'3'
PRSK2    CH    9,=H'1'
         BL    MAINLP
         CLI   0(3),X'11'         NEXT SBA?
         BE    PRSLP              YES, PARSE IT
         LA    3,1(3)
         SH    9,=H'1'
         B     PRSK2
*
PRMVC    MVC   0(0,8),0(3)        EXECUTED MVC
*
* ================================================================ *
* DECD6: DECODE ONE 3270 ENCODED BYTE IN R5 TO 6-BIT VALUE        *
* USES LOOKUP TABLE. RESULT RETURNED IN R5.                        *
* ================================================================ *
DECD6    DS    0H
         LA    15,DCOTBL          DECODE TABLE (64 ENTRIES)
         LA    7,0                INDEX
DCD6LP   CH    7,=H'64'
         BNL   DCD6DF             NOT FOUND, DEFAULT TO 0
         CLM   5,1,0(15)          COMPARE INPUT BYTE
         BE    DCD6OK
         LA    15,1(15)
         LA    7,1(7)
         B     DCD6LP
DCD6OK   LR    5,7                R5 = DECODED 6-BIT VALUE
         BR    14
DCD6DF   SR    5,5                DEFAULT = 0
         BR    14
*
* 3270 SBA ENCODE TABLE: INDEX=6-BIT VALUE, BYTE=ENCODED CHAR
DCOTBL   DC    X'40'              0
         DC    X'C1'              1
         DC    X'C2'              2
         DC    X'C3'              3
         DC    X'C4'              4
         DC    X'C5'              5
         DC    X'C6'              6
         DC    X'C7'              7
         DC    X'C8'              8
         DC    X'C9'              9
         DC    X'4A'              10
         DC    X'4B'              11
         DC    X'4C'              12
         DC    X'4D'              13
         DC    X'4E'              14
         DC    X'4F'              15
         DC    X'50'              16
         DC    X'D1'              17
         DC    X'D2'              18
         DC    X'D3'              19
         DC    X'D4'              20
         DC    X'D5'              21
         DC    X'D6'              22
         DC    X'D7'              23
         DC    X'D8'              24
         DC    X'D9'              25
         DC    X'5A'              26
         DC    X'5B'              27
         DC    X'5C'              28
         DC    X'5D'              29
         DC    X'5E'              30
         DC    X'5F'              31
         DC    X'60'              32
         DC    X'E1'              33
         DC    X'E2'              34
         DC    X'E3'              35
         DC    X'E4'              36
         DC    X'E5'              37
         DC    X'E6'              38
         DC    X'E7'              39
         DC    X'E8'              40
         DC    X'E9'              41
         DC    X'6A'              42
         DC    X'6B'              43
         DC    X'6C'              44
         DC    X'6D'              45
         DC    X'6E'              46
         DC    X'6F'              47
         DC    X'F0'              48
         DC    X'F1'              49
         DC    X'F2'              50
         DC    X'F3'              51
         DC    X'F4'              52
         DC    X'F5'              53
         DC    X'F6'              54
         DC    X'F7'              55
         DC    X'F8'              56
         DC    X'F9'              57
         DC    X'7A'              58
         DC    X'7B'              59
         DC    X'7C'              60
         DC    X'7D'              61
         DC    X'7E'              62
         DC    X'7F'              63
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
         MVC   STATMSG,SVDMSG
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
         LTR   8,8
         BNZ   LLDMSG
         MVC   STATMSG,NEWMSG      EMPTY MEMBER = NEW
         B     LRTN
LLDMSG   MVC   STATMSG,LDMSG
         B     LRTN
LNEW     SR    8,8
         ST    8,RECCNT
         MVC   STATMSG,NEWMSG      NEW FILE
LRTN     L     14,SLOAD
         BR    14
SLOAD    DS    F
*
* ================================================================ *
* DRAWSCN: BUILD 3270 FULLSCREEN BUFFER IN SCRBUF                 *
* ================================================================ *
DRAWSCN  ST    14,SDRAW
         LA    4,SCRBUF            R4 = BUFFER WRITE POINTER
* WCC - TPUT FULLSCR PREPENDS ERASE/WRITE COMMAND
         MVI   0(4),X'C3'          WCC: UNLOCK KB + RESET MDT
         LA    4,1(4)
* HEADER ROW 1
         MVI   0(4),X'11'          SBA ORDER
         MVC   1(2,4),=X'4040'     ROW 1 COL 1
         MVI   3(4),X'1D'          SF ORDER
         MVI   4(4),X'E8'          PROTECTED + HIGH INTENSITY
         MVC   5(26,4),HDRTXT
         LA    4,31(4)
* STATUS ROW 24
         MVI   0(4),X'11'          SBA ORDER
         MVC   1(2,4),=X'5CF0'     ROW 24 COL 1
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
* LINE NUMBER
         LA    7,1(6)              LINE NUMBER (1-BASED)
         CVD   7,DBLWRK
         UNPK  LINUM,DBLWRK+5(3)
         OI    LINUM+4,X'F0'
         MVC   5(5,4),LINUM        5-CHAR LINE NUMBER
* EDITABLE DATA FIELD
         MVI   10(4),X'1D'         SF UNPROTECTED
         MVI   11(4),X'40'
* CHECK IF RECORD EXISTS (BOUNDS CHECK)
         C     6,RECCNT            PAST END OF DATA?
         BNL   DRWBLK              YES, SHOW BLANK LINE
         LA    8,RECS
         LR    15,6
         MH    15,=H'80'
         AR    8,15
         MVC   12(80,4),0(8)       COPY RECORD TEXT
         B     DRWNXT
DRWBLK   MVC   12(80,4),BLANKS     BLANK LINE
DRWNXT   LA    4,92(4)             ADVANCE BUFFER PTR
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
BLANKS   DC    CL80' '
STATMSG  DC    CL30'LOADING...'
HDRTXT   DC    CL26'SEU EDITOR  PF3=EXIT PF10='
LDMSG    DC    CL30'LOADED'
NEWMSG   DC    CL30'NEW MEMBER'
SAVMSG   DC    CL30'SAVING...'
SVDMSG   DC    CL30'SAVED OK'
SERMSG   DC    CL30'SAVE FAILED'
*
* SBA POSITION TABLE - INDEXED BY SCREEN ROW NUMBER
* ENTRY N = ROW N COL 1 SBA ADDRESS (2 BYTES EACH)
POSTBL   DC    X'4040'             ROW 1  COL 1 (POS 0)
         DC    X'C150'             ROW 2  COL 1 (POS 80)
         DC    X'C260'             ROW 3  COL 1 (POS 160)
         DC    X'C3F0'             ROW 4  COL 1 (POS 240)
         DC    X'C540'             ROW 5  COL 1 (POS 320)
         DC    X'C650'             ROW 6  COL 1 (POS 400)
         DC    X'C760'             ROW 7  COL 1 (POS 480)
         DC    X'C8F0'             ROW 8  COL 1 (POS 560)
         DC    X'4A40'             ROW 9  COL 1 (POS 640)
         DC    X'4B50'             ROW 10 COL 1 (POS 720)
         DC    X'4C60'             ROW 11 COL 1 (POS 800)
         DC    X'4DF0'             ROW 12 COL 1 (POS 880)
         DC    X'4F40'             ROW 13 COL 1 (POS 960)
         DC    X'5050'             ROW 14 COL 1 (POS 1040)
         DC    X'D160'             ROW 15 COL 1 (POS 1120)
         DC    X'D2F0'             ROW 16 COL 1 (POS 1200)
         DC    X'D440'             ROW 17 COL 1 (POS 1280)
         DC    X'D550'             ROW 18 COL 1 (POS 1360)
         DC    X'D660'             ROW 19 COL 1 (POS 1440)
         DC    X'D7F0'             ROW 20 COL 1 (POS 1520)
*
* FIELD START POSITION TABLE (HALFWORD)
* EACH ENTRY = BUFFER POSITION OF FIRST DATA BYTE IN THAT ROW
* ROW N DATA FIELD STARTS AT: ROW_START + 7
FPOSTBL  DC    H'167'             ROW 3  FIELD (160+7)
         DC    H'247'             ROW 4  FIELD (240+7)
         DC    H'327'             ROW 5  FIELD (320+7)
         DC    H'407'             ROW 6  FIELD (400+7)
         DC    H'487'             ROW 7  FIELD (480+7)
         DC    H'567'             ROW 8  FIELD (560+7)
         DC    H'647'             ROW 9  FIELD (640+7)
         DC    H'727'             ROW 10 FIELD (720+7)
         DC    H'807'             ROW 11 FIELD (800+7)
         DC    H'887'             ROW 12 FIELD (880+7)
         DC    H'967'             ROW 13 FIELD (960+7)
         DC    H'1047'            ROW 14 FIELD (1040+7)
         DC    H'1127'            ROW 15 FIELD (1120+7)
         DC    H'1207'            ROW 16 FIELD (1200+7)
         DC    H'1287'            ROW 17 FIELD (1280+7)
         DC    H'1367'            ROW 18 FIELD (1360+7)
         DC    H'1447'            ROW 19 FIELD (1440+7)
         DC    H'1527'            ROW 20 FIELD (1520+7)
*
INDCB    DCB   DDNAME=SYSASMEU,DSORG=PS,MACRF=(GM),RECFM=FB,           X
               LRECL=80,EODAD=LEOF
OUTDCB   DCB   DDNAME=SYSASMEU,DSORG=PS,MACRF=(PM),RECFM=FB,           X
               LRECL=80
*
         LTORG
*
SCRBUF   DS    CL3000
INBUF    DS    CL512
RECS     DS    50CL80
*
         END   SEU
