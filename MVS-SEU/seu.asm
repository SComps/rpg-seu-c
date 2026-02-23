SEU      CSECT
         STM   14,12,12(13)        SAVE REGISTERS
         BALR  12,0                ADDRESSABILITY
         USING *,12,11,10,9        FOUR BASE REGISTERS
         LA    11,4095(12)         OFFSET 4096
         LA    11,1(11)
         LA    10,4095(11)         OFFSET 8192
         LA    10,1(10)
         LA    9,4095(10)          OFFSET 12288
         LA    9,1(9)
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
         BAL   11,PARSECP          EXTRACT DSN FROM CBUF
         LTR   15,15               DSN FOUND?
         BNZ   PARAMERR            NO, SHOW USAGE
*
         BAL   11,ALLOCDS          DYNALLOC DSN (SVC 99)
         LTR   15,15
         BNZ   ALLOCERR
*
         BAL   11,LOADP            QSAM GET RECORDS
*
* ------------------------------------------------------------------- *
* MAIN EVENT LOOP
* ------------------------------------------------------------------- *
MAINLOOP DS    0H
         BAL   11,DRAWSCN          BUILD 3270 BUFFER
         BAL   11,DOTPUT           TPUT FULLSCREEN
         BAL   11,DOTGET           TGET USER INPUT
*
         BAL   11,PROCINP          PROCESS AID & MODIFIED FIELDS
         CLI   AIDBYTE,AIDPF3      EXIT?
         BE    TERMINAT
         CLI   AIDBYTE,AIDPF10     SAVE?
         BE    SAVEP
*
         B     MAINLOOP            LOOP FOREVER
*
TERMINAT DS    0H
         BAL   11,FREEDS           UNALLOCATE
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
         CLC   S99ERROR(2),=X'0210'
         BNE   AERRGEN
         TPUT  ERRDSN,20
         B     TERMINAT
AERRGEN  UNPK  ERRDISP(5),S99ERROR(3)
         TR    ERRDISP(4),HEXTAB-240
         TPUT  ERRMSG1,15
         TPUT  ERRDISP,4
         B     TERMINAT
*
* ------------------------------------------------------------------- *
* SUBROUTINE: DRAWSCN - 3270 DATA STREAM GENERATION
* ------------------------------------------------------------------- *
DRAWSCN  DS    0H
         LA    4,DATSTR            STREAM PTR
         MVI   0(4),X'11'          SBA
         MVC   1(2,4),POS0101      1,1
         MVI   3(4),X'1D'          SF
         MVI   4(4),X'E8'          PROT/HIGH
         MVC   5(26,4),HDRTXT      TITLE
         LA    4,31(4)
*        --- STATUS MSG ---
         MVI   0(4),X'11'          SBA
         MVC   1(2,4),POS2401      24,1
         MVI   3(4),X'1D'          SF
         MVI   4(4),X'E8'          PROT/HIGH
         MVC   5(30,4),STATMSG
         LA    4,35(4)
*        --- SOURCE LINES (ROW 3-20) ---
         LA    5,3                 START ROW
         L     6,TOPREC            RECORD INDEX
DSLOOP   DS    0H
         MVI   0(4),X'11'          SBA
         BAL   14,GETROW6          ADDR
         MVC   1(2,4),ROWPOS
         MVI   3(4),X'1D'          SF
         MVI   4(4),X'60'          PROT
         LA    7,1(6)              LINE#
         CVD   7,DBLWRK
         UNPK  DLINNUM,DBLWRK+5(3)
         OI    DLINNUM+4,X'F0'
         MVC   5(5,4),DLINNUM
         MVI   10(4),X'1D'         SF (DATA FIELD)
         MVI   11(4),X'40'         UNPROT
         LA    8,RECS
         L     15,H80L             SAVE H80
         MR    14,6                INDEX * 80
         AR    8,15                REC ADDR
         MVC   12(80,4),0(8)
         LA    4,92(4)
         LA    5,1(5)
         LA    6,1(6)
         CH    5,=H'21'
         BL    DSLOOP
         ST    4,CURPTR
         BR    11
*
* ------------------------------------------------------------------- *
* SUBROUTINE: DOTGET - READ TERMINAL
* ------------------------------------------------------------------- *
DOTGET   DS    0H
         TGET  INBUF,512,ASIS
         STH   1,INBUFLEN
         MVI   AIDBYTE,X'7D'       ENTER
         LTR   1,1
         BZ    TGETDONE
         MVC   AIDBYTE(1),INBUF
TGETDONE BR    11
*
* ------------------------------------------------------------------- *
* SUBROUTINE: PROCINP - INPUT PROCESSING
* ------------------------------------------------------------------- *
PROCINP  DS    0H
         CLI   AIDBYTE,AIDPF7      UP
         BE    PGUP
         CLI   AIDBYTE,AIDPF8      DOWN
         BE    PGDN
*        --- FIELD PARSING ---
         LA    8,INBUF+3
         LH    9,INBUFLEN
         SH    9,=H'3'
         BNP   PROCDONE
         LA    10,INBUF
         AH    10,INBUFLEN
PARSEFLD DS    0H
         CR    8,10
         BNL   PROCDONE
         CLI   0(8),X'11'          SBA
         BNE   NEXTBYTE
         MVC   FIELDADR(2),1(8)
         LA    8,3(8)
         BAL   14,UPDATREC
         B     PARSEFLD
NEXTBYTE LA    8,1(8)
         B     PARSEFLD
PROCDONE BR    11
*
PGUP     DS    0H
         L     15,TOPREC
         S     15,=F'18'
         BP    PGUPOK
         L     15,=F'0'
PGUPOK   ST    15,TOPREC
         BR    11
PGDN     DS    0H
         L     15,TOPREC
         A     15,=F'18'
         CH    15,=H'82'
         BL    PGDNOK
         L     15,=F'82'
PGDNOK   ST    15,TOPREC
         BR    11
*
* ------------------------------------------------------------------- *
* SUBROUTINE: UPDATREC - MAP FIELD TO RECORD
* ------------------------------------------------------------------- *
UPDATREC DS    0H
         LA    1,POSTBL+4
         LA    15,2
FINDROW  DS    0H
         CLC   FIELDADR(2),0(1)
         BE    FOUNDROW
         LA    1,2(1)
         LA    15,1(15)
         CH    15,=H'21'
         BL    FINDROW
         BR    14
FOUNDROW S     15,=F'3'
         BM    14
         LA    7,RECS
         MH    15,H80
         AR    7,15
         LR    2,8                 SCAN FOR END
SCANEND  CR    2,10
         BNL   DOMOVE
         CLI   0(2),X'11'
         BE    DOMOVE
         LA    2,1(2)
         B     SCANEND
DOMOVE   LR    0,2
         SR    0,8
         CH    0,H80
         BNH   OKLEN
         LH    0,H80
OKLEN    LTR   0,0
         BZ    14
         BCTR  0,0
         EX    0,MOVREC
         LR    8,2
         BR    14
MOVREC   MVC   0(1,7),0(8)
*
* ------------------------------------------------------------------- *
* QSAM I/O & DYNALLOC ROUTINES
* ------------------------------------------------------------------- *
PARSECP  L     3,CPPLCBUF          GET BUFFER
         USING CBUF,3
         LH    4,CBUFLEN           TOTAL LEN
         LH    15,CBUFPL           OFFSET
         SR    4,15                PARAM LEN
         BNP   PARSERR             NO PARAMS
         LA    5,0(3,15)           START ADDR
* SKIP SPACES
PSKIP    CLI   0(5),X'40'
         BNE   PSTART
         LA    5,1(5)
         BCT   4,PSKIP
         B     PARSERR
* CHECK FOR QUOTE
PSTART   STH   4,DSNLEN            MAX POSS LEN
         CLI   0(5),X'7D'          QUOTE?
         BNE   PCOPY               NO, JOIN COPY
         LA    5,1(5)              SKIP LEADING QUOTE
         BCTR  4,0                 DEC LEN
         STH   4,DSNLEN
* SCAN FOR TRAILING QUOTE
         LTR   4,4
         BZ    PARSERR
         LR    6,5                 SCAN PTR
         LR    7,4                 SCAN COUNT
PSCAN    CLI   0(6),X'7D'          QUOTE?
         BE    PFNDQ
         LA    6,1(6)
         BCT   7,PSCAN
         B     PCOPY               NO TRAILING QUOTE
PFNDQ    LR    4,6
         SR    4,5                 LENGTH TO QUOTE
         STH   4,DSNLEN
PCOPY    DS    0H
         LH    4,DSNLEN
         LTR   4,4
         BZ    PARSERR
         CH    4,=H'44'
         BNH   PLENOK
         LA    4,44
PLENOK   STH   4,DSNLEN
         BCTR  4,0
         MVI   DSNWORK,X'40'
         MVC   DSNWORK+1(43),DSNWORK
         EX    4,MVCDSN
         SR    15,15
         BR    11
PARSERR  LA    15,8
         BR    11
MVCDSN   MVC   DSNWORK(0),0(5)
*
ALLOCDS  DS    0H
         MVI   S99VERB,X'01'       ALLOC
         LA    1,S99RB             ADDR OF RB
         ST    1,S99RBP            STORE IN PTR
         OI    S99RBP,X'80'        END OF LIST
         LA    1,S99RBP            R1 -> PTR
         SVC   99
         LR    15,15               SAVE RETURN CODE
         BR    11
*
LOADP    DS    0H
         OPEN  (INDCB,(INPUT))
         TM    INDCB+48,X'10'
         BZ    LDSNEW              IF FAIL, ASSUME NEW MEMBER
         LA    7,RECS
         SR    8,8
LDSLOOP  GET   INDCB,0(7)
         LA    7,80(7)
         LA    8,1(8)
         CH    8,=H'100'
         BL    LDSLOOP
LDSEOF   CLOSE (INDCB)
         ST    8,RECCNT
         BR    11
LDSNEW   DS    0H
         ST    8,RECCNT            8 IS ZERO HERE
         MVC   STATMSG,NEWMSG
         BR    11
LDSFAIL  LA    15,8
         BR    11
*
SAVEP    DS    0H
         OPEN  (OUTDCB,(OUTPUT))
         TM    OUTDCB+48,X'10'
         BZ    SFAIL
         LA    7,RECS
         L     8,RECCNT
SLOOP    PUT   OUTDCB,0(7)
         LA    7,80(7)
         BCT   8,SLOOP
         CLOSE (OUTDCB)
         MVC   STATMSG,SVOKMSG
         BR    11
SFAIL    MVC   STATMSG,SVERMSG
         BR    11
*
FREEDS   MVI   S99VERB,X'02'       UNALLOC
         SVC   99
         BR    11
*
DOTPUT   L     1,CURPTR
         LA    0,DATSTR
         SR    1,0
         TPUT  DATSTR,(0),FULLSCR
         BR    11
*
GETROW6  LA    15,POSTBL
         LR    1,5
         SLL   1,1
         AR    15,1
         MVC   ROWPOS(2),0(15)
         BR    14
*
* ------------------------------------------------------------------- *
* DATA AREAS
* ------------------------------------------------------------------- *
SAVEAREA DC    18F'0'
RECCNT   DC    F'0'
TOPREC   DC    F'0'
H80      DC    H'80'
H80L     DC    F'80'
DBLWRK   DC    D'0'
AIDBYTE  DC    X'00'
INBUFLEN DC    H'0'
FIELDADR DC    CL2' '
ROWPOS   DC    CL2' '
DLINNUM  DC    CL5' '
CURPTR   DC    F'0'
DSNWORK  DC    CL44' '
DSNLEN   DC    H'0'
HDRTXT   DC    CL26'SEU Editor (IFOX/MVS)'
STATMSG  DC    CL30'LOADED'
NEWMSG   DC    CL30'NEW MEMBER'
SVOKMSG  DC    CL30'SAVE COMPLETE'
SVERMSG  DC    CL30'SAVE FAILED'
ERRMSG1  DC    CL15'ALLOC ERROR: '
ERRDSN   DC    CL20'DATASET NOT FOUND'
ERRDISP  DC    CL4' '
USAGE    DC    CL36'USAGE: SEU ''DATASET.NAME(MEMBER)'''
HEXTAB   DC    C'0123456789ABCDEF'
*
AIDPF3   EQU   X'F3'
AIDPF7   EQU   X'F7'
AIDPF8   EQU   X'F8'
AIDPF10  EQU   X'FA'
FULLSCR  EQU   X'03'
ASIS     EQU   X'01'
*
POS0101  DC    X'4040'
POS2401  DC    X'5C20'             ROW 24 COL 1
POSTBL   DC    X'4040',X'4040',X'C150',X'C260',X'C3F0',X'C540',X'C650',X
               X'C760',X'C8F0',X'4A40',X'4B50',X'4C60',X'4DF0',X'4F40',X
               X'5050',X'D160',X'D2F0',X'D440',X'D550',X'D660',X'D7F0'
*
INDCB    DCB   DDNAME=SYSASMEU,DSORG=PS,MACRF=(GM),RECFM=FB,LRECL=80,  X
               EODAD=LDSEOF
OUTDCB   DCB   DDNAME=SYSASMEU,DSORG=PS,MACRF=(PM),RECFM=FB,LRECL=80
*
S99RBP   DS    F
S99RB    DS    0H
         DC    AL1(20)             RBLEN
S99VERB  DC    AL1(1)              VERB
S99FLAG1 DC    H'0'
S99ERROR DC    H'0'
S99INFO  DC    H'0'
S99TXTP2 DC    A(S99TUPL)
         DC    F'0'                RSVD
         DC    F'0'                RSVD
*
S99TUPL  DC    A(TUNAM),A(TUDDN),X'80',AL3(TUSTA)
TUNAM    DC    X'0001',H'1'
TUNAMLEN DC    H'44'
         DC    CL44' '
TUDDN    DC    X'0002',H'1',H'8',CL8'SYSASMEU'
TUSTA    DC    X'0004',H'1',H'1',X'08'
*
DATSTR   DS    CL4096
INBUF    DS    CL512
RECS     DS    100CL80
*
         LTORG                     POOL
*
CPPL     DSECT
CPPLCBUF DS    A
CBUF     DSECT
CBUFLEN  DS    H
CBUFPL   DS    H
CBUFDATA DS    C
*
         END   SEU
