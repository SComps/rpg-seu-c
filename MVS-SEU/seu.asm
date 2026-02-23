SEU      CSECT
         STM   14,12,12(13)        SAVE REGISTERS
         BALR  12,0                ADDRESSABILITY
         USING *,12,11             DUAL BASES
BASEHIGH LA    11,2048(12)         R11 = R12 + 4096
         LA    11,2048(11)
*
         ST    13,SAVEAREA+4
         LA    15,SAVEAREA
         ST    15,8(13)
         LR    13,15
*
* ------------------------------------------------------------------- *
* INITIALIZATION & PARM PARSING
* ------------------------------------------------------------------- *
         L     3,0(1)              R3 -> PARM OR CBUF
         TPUT  DEBUGM1,8           'DEBUG:  '
         ST    3,DBLWRK            SAVE PTR
         BAL   14,HEXDUMP          SHOW PTR
*
         BAL   14,PARSECP          EXTRACT DSN
         LTR   15,15
         BNZ   PARAMERR
*
         MVC   DEBUGBUF(10),DSNPRFX
         MVI   DEBUGBUF+10,C'['
         MVC   DEBUGBUF+11(44),TUNAMDSN
         MVI   DEBUGBUF+55,C']'
         TPUT  DEBUGBUF,56
*
         BAL   14,ALLOCDS          SVC 99
         LTR   15,15
         BNZ   ALLOCERR
*
         BAL   14,LOADP            QSAM
*
* ------------------------------------------------------------------- *
* MAIN EVENT LOOP
* ------------------------------------------------------------------- *
MAINLOOP DS    0H
         BAL   14,DRAWSCN
         BAL   14,DOTPUT
         BAL   14,DOTGET
*
         BAL   14,PROCINP
         CLI   AIDBYTE,AIDPF3
         BE    TERMINAT
         CLI   AIDBYTE,AIDPF10
         BE    SAVEP
*
         B     MAINLOOP
*
TERMINAT DS    0H
         BAL   14,FREEDS
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
         ST    15,DBLWRK
         MVC   DBLWRK+4(2),S99ERROR
         BAL   14,HEXDUMP
         TPUT  ERRMSG1,15
         TPUT  ERRDISP,8
         B     TERMINAT
*
* ------------------------------------------------------------------- *
* SUBROUTINE: PARSECP - HANDLES CBUF AND PARM
* ------------------------------------------------------------------- *
PARSECP  ST    14,SPARSE
* TRY AS CBUF (TSO COMMAND) - OFFSET AT 2
         LH    4,0(3)              LEN
         LH    15,2(3)             OFFSET
         SR    4,15                TEXT LEN
         BNP   TRYPARM             NOT CBUF
         CH    15,=H'4'
         BL    TRYPARM             TOO SMALL FOR CBUF
         LA    5,0(3,15)           TEXT START
         B     PSKIP
TRYPARM  LH    4,0(3)              LEN (PARM FORMAT)
         LA    5,2(3)              TEXT START
* SCAN
PSKIP    CLI   0(5),X'40'
         BE    PNEXT
         CLI   0(5),X'7D'
         BE    PNEXT
         B     PFOUND
PNEXT    LA    5,1(5)
         BCT   4,PSKIP
         B     PERR
PFOUND   LR    8,5                 START
PSCAN    CLI   0(5),X'40'
         BE    PEND
         CLI   0(5),X'7D'
         BE    PEND
         LA    5,1(5)
         BCT   4,PSCAN
PEND     LR    4,5
         SR    4,8
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
         B     PARSRTN
PERR     LA    15,8
PARSRTN  L     14,SPARSE
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
* SUBROUTINE: LOADP / SAVEP / FREEDS
* ------------------------------------------------------------------- *
LOADP    ST    14,SLOAD
         OPEN  (INDCB,(INPUT))
         TM    INDCB+48,X'10'
         BZ    L_NEW
         LA    7,RECS
         SR    8,8
L_LOOP   GET   INDCB,0(7)
         LA    7,80(7)
         LA    8,1(8)
         CH    8,=H'100'
         BL    L_LOOP
L_EOF    CLOSE (INDCB)
         ST    8,RECCNT
         B     L_RTN
L_NEW    SR    8,8
         ST    8,RECCNT
         MVC   STATMSG,NEWMSG
L_RTN    L     14,SLOAD
         BR    14
SLOAD    DS    F
*
SAVEP    ST    14,SSAVE
         OPEN  (OUTDCB,(OUTPUT))
         TM    OUTDCB+48,X'10'
         BZ    S_FAIL
         LA    7,RECS
         L     8,RECCNT
S_LOOP2  PUT   OUTDCB,0(7)
         LA    7,80(7)
         BCT   8,S_LOOP2
         CLOSE (OUTDCB)
         MVC   STATMSG,SVOKMSG
         B     S_RTN
S_FAIL   MVC   STATMSG,SVERMSG
S_RTN    L     14,SSAVE
         BR    14
SSAVE    DS    F
*
FREEDS   MVI   S99VERB,X'02'
         SVC   99
         BR    14
*
* ------------------------------------------------------------------- *
* SUBROUTINE: DRAWSCN / DOTGET / DOTPUT
* ------------------------------------------------------------------- *
DRAWSCN  ST    14,SDRAW
         LA    4,DATSTR
         MVI   0(4),X'11'          SBA
         MVC   1(2,4),POS0101
         MVI   3(4),X'1D'          SF
         MVI   4(4),X'E8'          PROT/HIGH
         MVC   5(26,4),HDRTXT
         LA    4,31(4)
         MVI   0(4),X'11'          SBA
         MVC   1(2,4),POS2401
         MVI   3(4),X'1D'          SF
         MVI   4(4),X'E8'          PROT/HIGH
         MVC   5(30,4),STATMSG
         LA    4,35(4)
         LA    5,3                 ROW
         L     6,TOPREC            INDEX
DLOOP    DS    0H
         MVI   0(4),X'11'          SBA
         BAL   14,GETROW
         MVC   1(2,4),ROWPOS
         MVI   3(4),X'1D'          SF
         MVI   4(4),X'60'          PROT
         LA    7,1(6)
         CVD   7,DBLWRK
         UNPK  DLINNUM,DBLWRK+5(3)
         OI    DLINNUM+4,X'F0'
         MVC   5(5,4),DLINNUM
         MVI   10(4),X'1D'         SF
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
         BZ    G_RTN
         MVC   AIDBYTE(1),INBUF
G_RTN    BR    14
*
PROCINP  ST    14,SPROC
         CLI   AIDBYTE,AIDPF7
         BE    P_UP
         CLI   AIDBYTE,AIDPF8
         BE    P_DN
         B     P_RTN
P_UP     L     15,TOPREC
         S     15,=F'18'
         BP    P_UPOK
         L     15,=F'0'
P_UPOK   ST    15,TOPREC
         B     P_RTN
P_DN     L     15,TOPREC
         A     15,=F'18'
         CH    15,=H'82'
         BL    P_DNOK
         L     15,=F'82'
P_DNOK   ST    15,TOPREC
P_RTN    L     14,SPROC
         BR    14
SPROC    DS    F
*
GETROW   LA    15,POSTBL
         LR    1,5
         SLL   1,1
         AR    15,1
         MVC   ROWPOS(2),0(15)
         BR    14
*
HEXDUMP  UNPK  ERRDISP(9),DBLWRK(5)
         TR    ERRDISP(8),HEXTAB-240
         BR    14
*
* ------------------------------------------------------------------- *
* CONSTANTS & CONTROL BLOCKS
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
DEBUGM1  DC    CL8'DEBUG:  '
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
               EODAD=L_EOF
OUTDCB   DCB   DDNAME=SYSASMEU,DSORG=PS,MACRF=(PM),RECFM=FB,LRECL=80
*
S99RBP   DS    F
S99RB    DS    0F
         DC    AL1(20)
S99VERB  DC    AL1(1)
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
         LTORG
*
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
