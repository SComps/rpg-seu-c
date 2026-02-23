SEU      CSECT
         STM   14,12,12(13)        SAVE REGISTERS
         BALR  12,0                ESTABLISH BASE
         USING *,12,11             DUAL BASE 8KB
BASEINIT LA    11,2048(12)         R11 = R12 + 4096
         LA    11,2048(11)
*
         ST    13,SAVEAREA+4       CHAIN SAVEAREAS
         LA    15,SAVEAREA
         ST    15,8(13)
         LR    13,15
*
* ============================================================== *
* PARAMETER PARSING  (HANDLES BOTH OS PARM AND TSO CPPL/CBUF)
* ON ENTRY R1 -> ADDRESS OF PARM POINTER (OS PARM FORMAT)
*   PARM FORMAT:  HL2=LEN  CL(N)=TEXT
*   CPPL  FORMAT: A(CBUF) A(...) A(...) A(...)
*   CBUF  FORMAT: HL2=TOTLEN  HL2=OFFSET  CL(N)=CMDTEXT
* ============================================================== *
         L     3,0(1)              -> PARM AREA
         LTR   3,3
         BZ    PARAMERR            NO PARM
*
* DETECT FORMAT: OS PARM HL2 LENGTH FIELD IS BELIEVABLE?
         LH    4,0(3)              GET H-LEN
         LTR   4,4
         BZ    PARAMERR            ZERO LENGTH
*
* IF LENGTH > 256 OR BYTE 0 IS UNPRINTABLE, TREAT AS CPPL
         CH    4,=H'256'
         BNH   TRYOS               SMALL: PROBABLY OS PARM
*
* TREAT AS CPPL: DEREFERENCE CBUF POINTER
         L     3,0(3)              -> CBUF
         LH    15,2(3)             CBUF OFFSET TO TEXT
         LH    4,0(3)              CBUF TOTAL LENGTH
         SR    4,15                TEXT LENGTH
         BNP   PARAMERR
         LA    5,0(3,15)           R5 -> TEXT START
         B     PSKIP
*
* OS PARM FORMAT
TRYOS    LA    5,2(3)              R5 -> TEXT (SKIP 2-BYTE LEN)
*
* SKIP LEADING BLANKS AND QUOTES
PSKIP    LTR   4,4
         BNP   PARAMERR
         CLI   0(5),X'40'          BLANK?
         BE    PNEXT
         CLI   0(5),X'7D'          QUOTE?
         BE    PNEXT
         B     PFOUND
PNEXT    LA    5,1(5)
         BCT   4,PSKIP
         B     PARAMERR
*
* FOUND START OF DSN
PFOUND   LR    8,5                 R8 -> DSN START
*
* SCAN FOR END OF DSN (BLANK/QUOTE/END)
PSCAN    LTR   4,4
         BZ    PEND                END OF FIELD = END OF DSN
         CLI   0(5),X'40'
         BE    PEND
         CLI   0(5),X'7D'
         BE    PEND
         LA    5,1(5)
         BCT   4,PSCAN
*
PEND     SR    5,8                 R5 = DSN LENGTH (REUSE)
         LR    4,5
         LTR   4,4
         BZ    PARAMERR            EMPTY DSN
*
         CH    4,=H'44'            CAP AT 44
         BNH   PLENOK
         LH    4,=H'44'
PLENOK   STH   4,DSNLEN
*
         MVI   TUNAMDSN,X'40'      BLANK DSN BUFFER
         MVC   TUNAMDSN+1(43),TUNAMDSN
         BCTR  4,0                 LEN-1 FOR EX
         EX    4,MVCDSN            COPY DSN
         LH    4,DSNLEN
         STH   4,TUNAMLEN          TELL SVC99 DSN LENGTH
*
* DEBUG: SHOW PARSED DSN WITH BRACKETS
         MVC   DEBUGBUF(10),DSNPRFX
         MVI   DEBUGBUF+10,C'['
         MVC   DEBUGBUF+11(44),TUNAMDSN
         MVI   DEBUGBUF+55,C']'
         TPUT  DEBUGBUF,56
*
* ALLOCATE VIA SVC 99
         BAL   14,ALLOCDS
         LTR   15,15
         BNZ   ALLOCERR
*
* LOAD DATASET INTO MEMORY
         BAL   14,LOADP
*
* ============================================================== *
* MAIN EVENT LOOP
* ============================================================== *
MAINLOOP BAL   14,DRAWSCN
         BAL   14,DOTPUT
         BAL   14,DOTGET
         BAL   14,PROCINP
         CLI   AIDBYTE,AIDPF3
         BE    TERMINAT
         CLI   AIDBYTE,AIDPF10
         BE    SAVEP
         B     MAINLOOP
*
TERMINAT BAL   14,FREEDS
         L     13,SAVEAREA+4
         LM    14,12,12(13)
         XR    15,15
         BR    14
*
* ============================================================== *
* ERROR EXITS
* ============================================================== *
PARAMERR TPUT  USAGE,L'USAGE
         B     TERMINAT
*
ALLOCERR ST    15,DBLWRK
         MVC   DBLWRK+4(2),S99ERROR
         MVI   DBLWRK+6,X'0F'
         UNPK  ERRDISP(9),DBLWRK+3(5)
         TR    ERRDISP(8),HEXTAB-240
         TPUT  ERRMSG1,L'ERRMSG1
         TPUT  ERRDISP,8
         B     TERMINAT
*
* ============================================================== *
* ALLOCDS - DYNAMIC ALLOCATION VIA SVC 99
* ============================================================== *
ALLOCDS  ST    14,SALLOC
         MVI   S99VERB,X'01'       ALLOC
         LA    1,S99RB
         ST    1,S99RBP
         OI    S99RBP,X'80'        LAST ENTRY
         LA    1,S99RBP
         SVC   99
         LR    15,15
         L     14,SALLOC
         BR    14
SALLOC   DS    F
*
* ============================================================== *
* LOADP - OPEN AND READ DATASET (QSAM)
* ============================================================== *
LOADP    ST    14,SLOAD
         OPEN  (INDCB,(INPUT))
         TM    INDCB+48,X'10'      OPEN OK?
         BZ    LNEW
         LA    7,RECS
         SR    8,8
LLOOP    GET   INDCB,0(7)
         LA    7,80(7)
         LA    8,1(8)
         CH    8,=H'100'
         BL    LLOOP
LEOF     CLOSE (INDCB)
         ST    8,RECCNT
         B     LRTN
LNEW     SR    8,8
         ST    8,RECCNT
         MVC   STATMSG,NEWMSG
LRTN     L     14,SLOAD
         BR    14
SLOAD    DS    F
*
* ============================================================== *
* SAVEP - WRITE DATASET BACK (QSAM)
* ============================================================== *
SAVEP    ST    14,SSAVE
         OPEN  (OUTDCB,(OUTPUT))
         TM    OUTDCB+48,X'10'
         BZ    SFAIL
         LA    7,RECS
         L     8,RECCNT
SLOOP2   PUT   OUTDCB,0(7)
         LA    7,80(7)
         BCT   8,SLOOP2
         CLOSE (OUTDCB)
         MVC   STATMSG,SVOKMSG
         B     SRTN
SFAIL    MVC   STATMSG,SVERMSG
SRTN     L     14,SSAVE
         BR    14
SSAVE    DS    F
*
FREEDS   MVI   S99VERB,X'02'       UNALLOC
         SVC   99
         BR    14
*
* ============================================================== *
* DRAWSCN - BUILD 3270 SCREEN BUFFER
* ============================================================== *
DRAWSCN  ST    14,SDRAW
         LA    4,DATSTR
* HEADER ROW
         MVI   0(4),X'11'          SBA
         MVC   1(2,4),POS0101      ROW 1 COL 1
         MVI   3(4),X'1D'          SF
         MVI   4(4),X'E8'          PROT/HIGH
         MVC   5(26,4),HDRTXT
         LA    4,31(4)
* STATUS ROW
         MVI   0(4),X'11'          SBA
         MVC   1(2,4),POS2401      ROW 24 COL 1
         MVI   3(4),X'1D'          SF
         MVI   4(4),X'E8'          PROT/HIGH
         MVC   5(30,4),STATMSG
         LA    4,35(4)
* DATA ROWS
         LA    5,3
         L     6,TOPREC
DLOOP    MVI   0(4),X'11'          SBA
         BAL   14,GETROW
         MVC   1(2,4),ROWPOS
         MVI   3(4),X'1D'          SF
         MVI   4(4),X'60'          UNPROT
         LA    7,1(6)
         CVD   7,DBLWRK
         UNPK  DLINNUM,DBLWRK+5(3)
         OI    DLINNUM+4,X'F0'
         MVC   5(5,4),DLINNUM
         MVI   10(4),X'1D'         SF UNPROT
         MVI   11(4),X'40'
         LA    8,RECS
         LR    15,6
         MH    15,H80
         AR    8,15
         MVC   12(80,4),0(8)       COPY RECORD
         LA    4,92(4)
         LA    5,1(5)
         LA    6,1(6)
         CH    5,=H'21'
         BL    DLOOP
         ST    4,CURPTR
         L     14,SDRAW
         BR    14
SDRAW    DS    F
*
* ============================================================== *
* DOTPUT / DOTGET / PROCINP
* ============================================================== *
DOTPUT   L     1,CURPTR
         LA    0,DATSTR
         SR    1,0
         TPUT  DATSTR,(0),FULLSCR
         BR    14
*
DOTGET   TGET  INBUF,512,ASIS
         STH   1,INBUFLEN
         MVI   AIDBYTE,X'7D'
         LTR   1,1
         BZ    GRTN
         MVC   AIDBYTE(1),INBUF
GRTN     BR    14
*
PROCINP  ST    14,SPROC
         CLI   AIDBYTE,AIDPF7      PF7=UP
         BE    PUP
         CLI   AIDBYTE,AIDPF8      PF8=DOWN
         BE    PDN
         B     PRTN
PUP      L     15,TOPREC
         S     15,=F'18'
         BP    PUPOK
         SR    15,15
PUPOK    ST    15,TOPREC
         B     PRTN
PDN      L     15,TOPREC
         A     15,=F'18'
         CH    15,=H'82'
         BL    PDNOK
         LH    15,=H'82'
PDNOK    ST    15,TOPREC
PRTN     L     14,SPROC
         BR    14
SPROC    DS    F
*
* ============================================================== *
* GETROW - LOOK UP SBA ROW POSITION
* ============================================================== *
GETROW   LA    15,POSTBL
         LR    1,5
         SLL   1,1
         AR    15,1
         MVC   ROWPOS(2),0(15)
         BR    14
*
* EX TARGET FOR DSN COPY
MVCDSN   MVC   TUNAMDSN(0),0(8)
*
* ============================================================== *
* DATA AREAS
* ============================================================== *
SAVEAREA DC    18F'0'
RECCNT   DC    F'0'
TOPREC   DC    F'0'
H80      DC    H'80'
DBLWRK   DC    D'0'
AIDBYTE  DC    X'00'
INBUFLEN DC    H'0'
ROWPOS   DC    CL2' '
DLINNUM  DC    CL5' '
CURPTR   DC    F'0'
DSNLEN   DC    H'0'
HDRTXT   DC    CL26'SEU Editor (IFOX/MVS)'
STATMSG  DC    CL30'LOADED'
NEWMSG   DC    CL30'NEW MEMBER'
SVOKMSG  DC    CL30'SAVE COMPLETE'
SVERMSG  DC    CL30'SAVE FAILED'
ERRMSG1  DC    CL15'ALLOC ERR RC/E:'
ERRDISP  DC    CL8' '
DSNPRFX  DC    CL10'DSN IS:   '
DEBUGBUF DC    CL64' '
USAGE    DC    CL36'USAGE: SEU ''DATASET.NAME(MEMBER)'''
HEXTAB   DC    C'0123456789ABCDEF'
AIDPF3   EQU   X'F3'
AIDPF7   EQU   X'F7'
AIDPF8   EQU   X'F8'
AIDPF10  EQU   X'FA'
FULLSCR  EQU   X'03'
ASIS     EQU   X'01'
POS0101  DC    X'4040'
POS2401  DC    X'5C20'
POSTBL   DC    X'4040',X'4040',X'C150',X'C260',X'C3F0',X'C540',X'C650',X
               X'C760',X'C8F0',X'4A40',X'4B50',X'4C60',X'4DF0',X'4F40',X
               X'5050',X'D160',X'D2F0',X'D440',X'D550',X'D660',X'D7F0'
*
INDCB    DCB   DDNAME=SYSASMEU,DSORG=PS,MACRF=(GM),RECFM=FB,LRECL=80, X
               EODAD=LEOF
OUTDCB   DCB   DDNAME=SYSASMEU,DSORG=PS,MACRF=(PM),RECFM=FB,LRECL=80
*
S99RBP   DS    F
S99RB    DS    0F
         DC    AL1(20)             RB LENGTH
S99VERB  DC    AL1(1)              VERB (ALLOC=1)
S99FLAG1 DC    H'0'
S99ERROR DC    H'0'
S99INFO  DC    H'0'
S99TXTP2 DC    A(S99TUPL)          TEXT UNIT POINTER LIST
         DC    F'0',F'0'
S99TUPL  DC    A(TUNAM)
         DC    A(TUDDN)
         DC    X'80',AL3(TUSTA)    LAST ENTRY
TUNAM    DC    X'0001',H'1'        KEY=DSNAME, 1 VALUE
TUNAMLEN DC    H'0'                FILL IN AT RUN TIME
TUNAMDSN DC    CL44' '
TUDDN    DC    X'0002',H'1',X'0008',CL8'SYSASMEU'
TUSTA    DC    X'0004',H'1',X'0001',X'08'
*
         LTORG
*
DATSTR   DS    CL3000
INBUF    DS    CL512
RECS     DS    100CL80
*
         END   SEU
