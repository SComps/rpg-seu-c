SEU      CSECT
         STM   14,12,12(13)        SAVE REGISTERS
         BALR  12,0                ADDRESSABILITY
         USING *,12                PRIMARY BASE (4KB)
*
         ST    13,SAVEAREA+4       SAVEAREA CHAINING
         LA    15,SAVEAREA
         ST    15,8(13)
         LR    13,15
*
* ------------------------------------------------------------------- *
* INITIALIZATION & DATASET ALLOCATION
* ------------------------------------------------------------------- *
         LR    2,1                 SAVE CPPL ADDR
         USING CPPL,2
         L     3,CPPLCBUF          GET CBUF
         TPUT  0(3),72             DEBUG: SHOW CBUF
*
         BAL   14,PARSECP          EXTRACT DSN
         LTR   15,15
         BNZ   PARAMERR
*
         MVC   DEBUGBUF(10),DSNPRFX
         MVI   DEBUGBUF+10,C'['
         MVC   DEBUGBUF+11(44),TUNAMDSN
         MVI   DEBUGBUF+55,C']'
         TPUT  DEBUGBUF,56         DEBUG: SHOW DSN
*
         BAL   14,ALLOCDS          SVC 99 ALLOC
         LTR   15,15
         BNZ   ALLOCERR
*
         BAL   14,LOADP            QSAM LOAD
*
* ------------------------------------------------------------------- *
* MAIN EVENT LOOP
* ------------------------------------------------------------------- *
MAINLOOP DS    0H
         BAL   14,DRAWSCN          BUILD 3270 BUFFER
         BAL   14,DOTPUT           TPUT FULLSCREEN
         BAL   14,DOTGET           TGET USER INPUT
*
         BAL   14,PROCINP          PROCESS INPUT
         CLI   AIDBYTE,AIDPF3      EXIT?
         BE    TERMINAT
         CLI   AIDBYTE,AIDPF10     SAVE?
         BE    SAVEP
*
         B     MAINLOOP            LOOP
*
TERMINAT DS    0H
         BAL   14,FREEDS           UNALLOCATE
         L     13,SAVEAREA+4
         LM    14,12,12(13)
         XR    15,15
         BR    14
*
* ------------------------------------------------------------------- *
* ERROR HANDLING
* ------------------------------------------------------------------- *
PARAMERR DS    0H
         TPUT  USAGE,36
         B     TERMINAT
*
ALLOCERR DS    0H
         ST    15,DBLWRK           RC
         MVC   DBLWRK+4(2),S99ERROR ERRCODE
         MVI   DBLWRK+6,X'0F'
         UNPK  ERRDISP(9),DBLWRK+3(5)
         TR    ERRDISP(8),HEXTAB-240
         TPUT  ERRMSG1,15
         TPUT  ERRDISP,8
         B     TERMINAT
*
* ------------------------------------------------------------------- *
* SUBROUTINE: PARSECP - SIMPLE SCANNER
* ------------------------------------------------------------------- *
PARSECP  ST    14,SPARSE
         L     3,CPPLCBUF
         USING CBUF,3
         LH    4,CBUFLEN
         LH    15,CBUFPL
         SR    4,15                PARAM LEN
         BNP   PERR
         LA    5,0(3,15)           START ADDR
* SKIP BLANKS/QUOTES
PSKIP    CLI   0(5),X'40'
         BE    PNEXT
         CLI   0(5),X'7D'          QUOTE
         BE    PNEXT
         B     PFOUND
PNEXT    LA    5,1(5)
         BCT   4,PSKIP
         B     PERR
PFOUND   LR    8,5                 DSN START
* SCAN FOR END (BLANK OR QUOTE)
PSCAN    CLI   0(5),X'40'
         BE    PEND
         CLI   0(5),X'7D'
         BE    PEND
         LA    5,1(5)
         BCT   4,PSCAN
PEND     LR    4,5
         SR    4,8                 CALC LEN
         STH   4,DSNLEN
         LTR   4,4
         BZ    PERR
         CH    4,=H'44'
         BNH   PLENOK
         LH    4,=H'44'
PLENOK   STH   4,DSNLEN
         BCTR  4,0
         MVI   TUNAMDSN,X'40'
         MVC   TUNAMDSN+1(43),TUNAMDSN
         EX    4,MVCDSN
         SR    15,15
         B     PRTN
PERR     LA    15,8
PRTN     L     14,SPARSE
         BR    14
MVCDSN   MVC   TUNAMDSN(0),0(8)
SPARSE   DS    F
*
* ------------------------------------------------------------------- *
* SUBROUTINE: ALLOCDS - SVC 99
* ------------------------------------------------------------------- *
ALLOCDS  ST    14,SALLOC
         MVI   S99VERB,X'01'
         MVC   TUNAMLEN,DSNLEN
         LA    1,S99RB
         ST    1,S99RBP
         OI    S99RBP,X'80'
         LA    1,S99RBP
         SVC   99
         LR    15,15
         L     14,SALLOC
         BR    14
SALLOC   DS    F
*
* ------------------------------------------------------------------- *
* SUBROUTINE: LOADP - QSAM READ
* ------------------------------------------------------------------- *
LOADP    ST    14,SLOAD
         OPEN  (INDCB,(INPUT))
         TM    INDCB+48,X'10'
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
* ------------------------------------------------------------------- *
* SUBROUTINE: DRAWSCN - RENDER
* ------------------------------------------------------------------- *
DRAWSCN  ST    14,SDRAW
         LA    4,DATSTR            BUFFER
         MVI   0(4),X'11'          SBA
         MVC   1(2,4),POS0101
         MVI   3(4),X'1D'          SF
         MVI   4(4),X'E8'          PROT/HIGH
         MVC   5(26,4),HDRTXT
         LA    4,31(4)
* STATUS
         MVI   0(4),X'11'          SBA
         MVC   1(2,4),POS2401
         MVI   3(4),X'1D'          SF
         MVI   4(4),X'E8'          PROT/HIGH
         MVC   5(30,4),STATMSG
         LA    4,35(4)
* RECORDS (ROW 3-20)
         LA    5,3                 ROW
         L     6,TOPREC            INDEX
DLOOP    DS    0H
         MVI   0(4),X'11'          SBA
         BAL   14,GETROW           NESTED CALL
         MVC   1(2,4),ROWPOS
         MVI   3(4),X'1D'          SF
         MVI   4(4),X'60'          PROT
         LA    7,1(6)              LINE#
         CVD   7,DBLWRK
         UNPK  DLINNUM,DBLWRK+5(3)
         OI    DLINNUM+4,X'F0'
         MVC   5(5,4),DLINNUM
         MVI   10(4),X'1D'         SF (UNPROT)
         MVI   11(4),X'40'
         LA    8,RECS
         LR    15,6
         MH    15,H80
         AR    8,15
         MVC   12(80,4),0(8)
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
* ------------------------------------------------------------------- *
* SUBROUTINE: DOTGET / DOTPUT / SPROC (SIMPLIFIED)
* ------------------------------------------------------------------- *
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
         CLI   AIDBYTE,AIDPF7      UP
         BE    PUP
         CLI   AIDBYTE,AIDPF8      DOWN
         BE    PDN
* (Input field logic would go here if needed)
         B     PRTN
PUP      L     15,TOPREC
         S     15,=F'18'
         BP    PUPOK
         L     15,=F'0'
PUPOK    ST    15,TOPREC
         B     PRTN
PDN      L     15,TOPREC
         A     15,=F'18'
         CH    15,=H'82'
         BL    PDNOK
         L     15,=F'82'
PDNOK    ST    15,TOPREC
PRTN     L     14,SPROC
         BR    14
SPROC    DS    F
*
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
FREEDS   MVI   S99VERB,X'02'
         SVC   99
         BR    14
*
GETROW   LA    15,POSTBL
         LR    1,5
         SLL   1,1
         AR    15,1
         MVC   ROWPOS(2),0(15)
         BR    14
*
* ------------------------------------------------------------------- *
* CONTROL BLOCKS & SMALL VARIABLES
* ------------------------------------------------------------------- *
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
INDCB    DCB   DDNAME=SYSASMEU,DSORG=PS,MACRF=(GM),RECFM=FB,LRECL=80,  X
               EODAD=LEOF
OUTDCB   DCB   DDNAME=SYSASMEU,DSORG=PS,MACRF=(PM),RECFM=FB,LRECL=80
*
S99RBP   DS    F
S99RB    DS    0F
         DC    AL1(20)
S99VERB  DC    AL1(1)              VERB
S99FLAG1 DC    H'0'
S99ERROR DC    H'0'
S99INFO  DC    H'0'
S99TXTP2 DC    A(S99TUPL)
         DC    F'0',F'0'
S99TUPL  DC    A(TUNAM)
         DC    A(TUDDN)
         DC    X'80',AL3(TUSTA)
TUNAM    DC    X'0001',H'1'
TUNAMLEN DC    H'0'
TUNAMDSN DC    CL44' '
TUDDN    DC    X'0002',H'1',X'0008',CL8'SYSASMEU'
TUSTA    DC    X'0004',H'1',X'0001',X'08'
*
* ------------------------------------------------------------------- *
* LARGE BUFFERS (END OF CSECT)
* ------------------------------------------------------------------- *
         LTORG                     POOL
DATSTR   DS    CL3000
INBUF    DS    CL512
RECS     DS    100CL80
*
CPPL     DSECT
CPPLCBUF DS    A
CBUF     DSECT
CBUFLEN  DS    H
CBUFPL   DS    H
CBUFDATA DS    C
*
         END   SEU
