ASMSEU   CSECT
         STM   14,12,12(13)        SAVE REGISTERS
         BALR  12,0                ADDRESSABILITY
         USING *,12
         ST    13,SAVEAREA+4       SAVEAREA CHAINING
         LA    15,SAVEAREA
         ST    15,8(13)
         LR    13,15
*
* INITIALIZATION
*
         BAL   11,CLRSCN           CLEAR INTERNAL BUFFER
         BAL   11,INITDATA         INIT DUMMY DATA
*
* MAIN EVENT LOOP
*
MAINLOOP DS    0H
         BAL   11,DRAWSCN          BUILD 3270 DATA STREAM
         BAL   11,DOTPUT           TPUT FULLSCREEN
         BAL   11,DOTGET           TGET USER INPUT
*
         BAL   11,PROCINP          PROCESS AID AND FIELDS
         CLI   AIDBYTE,AIDPF3      PF3 PRESSED?
         BE    TERMINAT            YES, EXIT
*
         B     MAINLOOP            LOOP FOREVER
*
TERMINAT DS    0H
         L     13,SAVEAREA+4       RESTORE SAVE AREA
         LM    14,12,12(13)        RESTORE REGISTERS
         XR    15,15               RC=0
         BR    14                  RETURN TO TSO
*
* ------------------------------------------------------------------- *
* SUBROUTINE: CLRSCN - ZERO OUT SCREEN BUFFER
* ------------------------------------------------------------------- *
CLRSCN   DS    0H
         MVI   SCRBUF,X'40'        SPACE
         MVC   SCRBUF+1(1919),SCRBUF
         BR    11
*
* ------------------------------------------------------------------- *
* SUBROUTINE: INITDATA - LOAD SAMPLE SOURCE
* ------------------------------------------------------------------- *
INITDATA DS    0H
         MVC   RECS(80),SAMP1
         MVC   RECS+80(80),SAMP2
         MVC   RECS+160(80),SAMP3
         MVI   RECCNT,3
         BR    11
*
* ------------------------------------------------------------------- *
* SUBROUTINE: DRAWSCN - BUILD 3270 DATA STREAM (SBA, SF, DATA)
* ------------------------------------------------------------------- *
DRAWSCN  DS    0H
*        --- BUILD HEADER ---
         LA    4,DATSTR            POINTER TO OUTPUT STREAM
         MVI   0(4),X'11'          SBA
         MVC   1(2,4),POS0101      POS 1,1
         MVI   3(4),X'1D'          SF
         MVI   4(4),X'E8'          ATTR: PROT, HIGH
         MVC   5(26,4),HDRTXT      TITLE
         LA    4,31(4)
*
*        --- BUILD SOURCE LINES (ROW 3-20) ---
         LA    5,3                 START ROW
         L     6,TOPREC            INDEX OF TOP RECORD
DSLOOP   DS    0H
         MVI   0(4),X'11'          SBA
         BAL   14,GETROW6          GET SBA POS FOR ROW IN R5
         MVC   1(2,4),ROWPOS
         MVI   3(4),X'1D'          SF
         MVI   4(4),X'60'          ATTR: PROT, NORM
*        DRAW LINE NUMBER
         LA    7,1(6)              LINE NUM
         CVD   7,DBLWRK
         UNPK  DLINNUM,DBLWRK+5(3)
         OI    DLINNUM+4,X'F0'     FIX SIGN
         MVC   5(5,4),DLINNUM      PUT LINE NUM
*        DRAW DATA
         MVI   10(4),X'1D'         SF (FIELD FOR DATA)
         MVI   11(4),X'40'         ATTR: UNPROT
         LA    8,RECS              CALC RECORD ADDR
         MH    6,H80               RECCNT * 80
         AR    8,6
         MVC   12(80,4),0(8)       MOVE DATA
         LA    4,92(4)
*
         LA    5,1(5)              NEXT ROW
         LA    6,1(6)              NEXT REC
         CH    5,=H21              DONE WITH PAGE?
         BL    DSLOOP
*
         ST    4,CURPTR            SAVE STREAM END
         BR    11
*
* ------------------------------------------------------------------- *
* SUBROUTINE: GETROW6 - GET 3270 ROW/COL ADDR (STUB)
* ------------------------------------------------------------------- *
GETROW6  DS    0H
* SIMPLE TABLE LOOKUP FOR SBA ADDRS
         LA    15,POS_TBL
         LR    1,5                 ROW
         SLL   1,1                 * 2
         AR    15,1
         MVC   ROWPOS(2),0(15)
         BR    14
*
* ------------------------------------------------------------------- *
* SUBROUTINE: DOTPUT - WRITE TO TERMINAL
* ------------------------------------------------------------------- *
DOTPUT   DS    0H
         L     1,CURPTR
         LA    0,DATSTR
         SR    1,0                 LENGTH
         TPUT  DATSTR,(0),FULLSCR
         BR    11
*
* ------------------------------------------------------------------- *
* SUBROUTINE: DOTGET - READ FROM TERMINAL
* ------------------------------------------------------------------- *
DOTGET   DS    0H
         TGET  INBUF,512,ASIS
         STH   1,INBUFLEN          SAVE LENGTH
         LTR   15,15               CHECK TGET STATUS
         MVI   AIDBYTE,X'7D'       DEFAULT TO ENTER
         CLI   INBUF,0             DID WE GET ANYTHING?
         BZ    TGETDONE
         MVC   AIDBYTE(1),INBUF    FIRST BYTE IS AID
TGETDONE DS    0H
         BR    11
*
* ------------------------------------------------------------------- *
* SUBROUTINE: PROCINP - PROCESS USER ACTIONS
* ------------------------------------------------------------------- *
PROCINP  DS    0H
         CLI   AIDBYTE,AIDPF3      PF3 PRESSED?
         BE    RETPROC             YES, HANDLED IN MAIN
         CLI   AIDBYTE,AIDPF7      PAGE UP?
         BE    PGUP
         CLI   AIDBYTE,AIDPF8      PAGE DOWN?
         BE    PGDN
*
*        --- PARSE MODIFIED FIELDS ---
         LA    8,INBUF+3           SKIP AID AND CURSOR
         LH    9,INBUFLEN          TOTAL BYTES READ
         SH    9,=H3               ADJUST FOR HEADER
         BNP   RETPROC
         LA    10,INBUF            END ADDR
         AH    10,INBUFLEN
*
PARSEFLD DS    0H
         CR    8,10                REACHED END?
         BNL   RETPROC
         CLI   0(8),X'11'          SBA FOUND?
         BNE   NEXTBYTE
*        FOUND SBA - EXTRACT DATA
         MVC   FIELDADR(2),1(8)    SAVE FIELD SBA
         LA    8,3(8)              SKIP SBA
         BAL   14,UPDATREC         UPDATE RECORD DATA
         B     PARSEFLD
*
NEXTBYTE LA    8,1(8)
         B     PARSEFLD
*
RETPROC  BR    11
*
PGUP     DS    0H
         L     15,TOPREC
         S     15,=F'18'           PAGE SIZE 18
         BP    PGUP_OK
         L     15,=F'0'
PGUP_OK  ST    15,TOPREC
         BR    11
PGDN     DS    0H
         L     15,TOPREC
         A     15,=F'18'
         C     15,=F'82'           MAX START (100-18)
         BL    PGDN_OK
         L     15,=F'82'
PGDN_OK  ST    15,TOPREC
         BR    11
*
* ------------------------------------------------------------------- *
* SUBROUTINE: UPDATREC - MAP FIELD SBA TO RECORD AND UPDATE
* ------------------------------------------------------------------- *
UPDATREC DS    0H
* SEARCH POS_TBL FOR ROW
         LA    1,POS_TBL+4         SKIP ROW 0,1
         LA    15,2                ROW COUNTER
FINDROW  DS    0H
         CLC   FIELDADR(2),0(1)    MATCH?
         BE    FOUNDROW
         LA    1,2(1)              NEXT POS
         LA    15,1(15)            NEXT ROW
         CH    15,=H21             FAILSAFE
         BL    FINDROW
         BR    14
*
FOUNDROW DS    0H
         S     15,=F'3'            SUBTRACT HEADER ROWS
         BM    14                  HEADER FIELD?
*        FOUND RECORD ROW IN R15
         LA    7,RECS
         MH    15,H80
         AR    7,15                RECORD ADDR
*        SCAN FOR NEXT SBA OR END
         LR    2,8                 SCAN PTR
SCANEND  DS    0H
         CR    2,10
         BNL   DOMOVE
         CLI   0(2),X'11'
         BE    DOMOVE
         LA    2,1(2)
         B     SCANEND
DOMOVE   DS    0H
         LR    0,2
         SR    0,8                 LEN TO MOVE
         CH    0,H80
         BNH   OKLEN
         LH    0,H80
OKLEN    DS    0H
         LTR   0,0
         BZ    14                  ANY DATA?
         BCTR  0,0                 FOR EX
         EX    0,MOVREC            MVC 0(0,7),0(8)
         LR    8,2                 UPDATE PTR
         BR    14
*
MOVREC   MVC   0(1,7),0(8)
*
* ------------------------------------------------------------------- *
* DATA AREAS
* ------------------------------------------------------------------- *
SAVEAREA DC    18F'0'
DBLWRK   DC    D'0'
H80      DC    H'80'
RECCNT   DC    F'0'
TOPREC   DC    F'0'                TOP RECORD INDEX
AIDBYTE  DC    X'00'
DLINNUM  DC    CL5' '
CURPTR   DC    F'0'
*
AIDPF3   EQU   X'F3'
AIDPF7   EQU   X'F7'
AIDPF8   EQU   X'F8'
FIELDADR DS    CL2
INBUFLEN DS    H
*
HDRTXT   DC    CL26'Assembly SEU (IFOX/MVS)'
SAMP1    DC    CL80'SAMPL    START 0'
SAMP2    DC    CL80'         BASR  12,0'
SAMP3    DC    CL80'         END   SAMPL'
*
POS0101  DC    X'4040'             ROW 1 COL 1
POS_TBL  DC    X'4040'             ROW 0
         DC    X'4040'             ROW 1
         DC    X'C150'             ROW 2
         DC    X'C260'             ROW 3
         DC    X'C3F0'             ROW 4
         DC    X'C540'             ROW 5
         DC    X'C650'             ROW 6
         DC    X'C760'             ROW 7
         DC    X'C8F0'             ROW 8
         DC    X'4A40'             ROW 9
         DC    X'4B50'             ROW 10
         DC    X'4C60'             ROW 11
         DC    X'4DF0'             ROW 12
         DC    X'4F40'             ROW 13
         DC    X'5050'             ROW 14
         DC    X'D160'             ROW 15
         DC    X'D2F0'             ROW 16
         DC    X'D440'             ROW 17
         DC    X'D550'             ROW 18
         DC    X'D660'             ROW 19
         DC    X'D7F0'             ROW 20
*
ROWPOS   DS    CL2
SCRBUF   DS    CL1920
DATSTR   DS    CL4096              3270 DATA STREAM BUFFER
INBUF    DS    CL512
*
RECS     DS    100CL80             100 RECORDS
         END   ASMSEU
