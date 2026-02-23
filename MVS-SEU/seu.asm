SEU      CSECT
* ================================================================ *
* SEU - SIMPLE TSO EDITOR FOR MVS 3.8J / IFOX00                   *
* ================================================================ *
         STM   14,12,12(13)
         BALR  12,0
         USING *,12,11
         L     11,=A(DATAREAS)     POINT TO DATA AREA
         USING DATAREAS,11
*
         ST    13,SAVEARA+4
         LA    15,SAVEARA
         ST    15,8(13)
         LR    13,15
*
* INITIALIZE RECORD BUFFER
         LA    0,RECS
         L     1,=F'4000'
         SR    2,2
         SR    3,3
         ICM   3,8,=X'40'
         MVCL  0,2
*
         BAL   14,LOADP            LOAD DATASET
*
* ENTER TSO FULLSCREEN MODE - ATTEMPT TO SILENCE ABEND
* IF STFSMODE FAILS, WE WILL CONTINUE AND HOPE TPUT WORKS
         BAL   14,SETMODE
*
* ================================================================ *
* MAIN LOOP                                                       *
* ================================================================ *
MAINLP   BAL   14,DRAWSCN          BUILD SCREEN
         L     1,CURPTR
         LA    0,SCRBUF
         SR    1,0                 LENGTH
         LR    0,1
         TPUT  SCRBUF,(0),FULLSCR  SHOW SCREEN
*
         TGET  INBUF,512,ASIS      WAIT FOR INPUT
         LR    2,1                 BYTE COUNT
         MVI   AIDBYTE,X'7D'       DEFAULT=ENTER
         LTR   2,2
         BZ    ACHECK
         MVC   AIDBYTE(1),INBUF
*
ACHECK   CLI   AIDBYTE,X'F3'       PF3=EXIT
         BE    EXITPGM
         CLI   AIDBYTE,X'FA'       PF10=SAVE
         BE    DOSAVE
         CLI   AIDBYTE,X'F7'       PF7=UP
         BE    DOUP
         CLI   AIDBYTE,X'F8'       PF8=DOWN
         BE    DODN
         CLI   AIDBYTE,X'7D'       ENTER=APPLY
         BE    DOENTER
         B     MAINLP
*
EXITPGM  DS    0H
         L     13,SAVEARA+4
         LM    14,12,12(13)
         XR    15,15
         BR    14
*
* ================================================================ *
* DRAWSCN: BUILD THE 3270 DATA STREAM                             *
* ================================================================ *
DRAWSCN  ST    14,SDRAW
         LA    4,SCRBUF
         MVI   0(4),X'C3'          WCC (UNLOCK KB)
         LA    4,1(4)
* HEADER (ROW 1)
         MVI   0(4),X'11'          SBA
         MVC   1(2,4),=X'4040'     POS 0
         MVI   3(4),X'1D'          SF
         MVI   4(4),X'E8'          HI-INTENSE
         MVC   5(26,4),HDRTXT
         LA    4,31(4)
* STATUS (ROW 24)
         MVI   0(4),X'11'          SBA
         MVC   1(2,4),=X'5CF0'     POS 1840
         MVI   3(4),X'1D'          SF
         MVI   4(4),X'E8'
         MVC   5(30,4),STATMSG
         LA    4,35(4)
* DATA ROWS 3-20 (18 ROWS)
         LA    5,3                 SCREEN ROW COUNTER
         L     6,TOPREC            RECORD INDEX
DRWLP    LA    15,POSTBL           SBA TABLE
         LR    1,5
         BCTR  1,0                 ROW-1
         SLL   1,1                 *2
         AR    15,1                TABLE ENTRY
*
         MVI   0(4),X'11'          SBA
         MVC   1(2,4),0(15)        SET ROW POS
         MVI   3(4),X'1D'          SF PROTECT
         MVI   4(4),X'60'
         LA    7,1(6)              LINE NUM
         CVD   7,DBLWRK
         UNPK  LINUM,DBLWRK+5(3)
         OI    LINUM+4,X'F0'
         MVC   5(5,4),LINUM
         MVI   10(4),X'1D'         SF UNPROTECT
         MVI   11(4),X'40'
* COPY RECORD OR BLANK
         C     6,RECCNT
         BNL   DRWBLK
         LA    8,RECS
         LR    1,6
         MH    1,=H'80'
         AR    8,1
         MVC   12(80,4),0(8)
         B     DRWNXT
DRWBLK   MVC   12(80,4),BLANKS
DRWNXT   LA    4,92(4)
         LA    5,1(5)
         LA    6,1(6)
         CH    5,=H'21'
         BL    DRWLP
*
         ST    4,CURPTR
         L     14,SDRAW
         BR    14
*
* ================================================================ *
* DOENTER: PARSE RETURNED SCREEN FIELDS                           *
* ================================================================ *
DOENTER  CH    2,=H'3'
         BL    MAINLP
         LA    3,INBUF+3           SKIP AID/CURSOR
         LR    9,2
         SH    9,=H'3'             REM LEN
PRSLP    CH    9,=H'4'             NEED SBA+1 BYTE MIN
         BL    MAINLP
         CLI   0(3),X'11'          SBA?
         BNE   MAINLP
* DECODE
         SR    5,5
         IC    5,1(3)
         BAL   14,DECD6
         SLL   5,6
         ST    5,DBLWRK
         SR    5,5
         IC    5,2(3)
         BAL   14,DECD6
         A     5,DBLWRK            TOTAL POS
         ST    5,DBLWRK
* MAP POS TO ROW
         SR    4,4
         LA    7,FPOSTBL
PRFND    CH    4,=H'18'
         BNL   PRSKIP
         CLC   0(2,7),DBLWRK+2
         BE    PRFOUND
         LA    7,2(7)
         LA    4,1(4)
         B     PRFND
PRFOUND  A     4,TOPREC
         C     4,=F'50'
         BNL   PRSKIP
         LR    8,4
         MH    8,=H'80'
         LA    8,RECS(8)
* COPY
         LA    3,3(3)
         SH    9,=H'3'
         LR    6,9
         CH    6,=H'80'
         BNH   PRLNOK
         LA    6,80
PRLNOK   LR    7,6
         SR    6,6
PRSCN    CR    6,7
         BNL   PRCPY
         LR    15,3
         AR    15,6
         CLI   0(15),X'11'
         BE    PRCPY
         LA    6,1(6)
         B     PRSCN
PRCPY    MVC   0(80,8),BLANKS
         LTR   6,6
         BZ    PRADV
         BCTR  6,0
         EX    6,PRMVC
         LA    6,1(6)
PRADV    ALR   3,6
         SLR   9,6
* UPDATE COUNT
         LA    4,1(4)
         C     4,RECCNT
         BNH   PRSLP
         ST    4,RECCNT
         B     PRSLP
PRSKIP   LA    3,3(3)
         SH    9,=H'3'
PRSK2    CH    9,=H'1'
         BL    MAINLP
         CLI   0(3),X'11'
         BE    PRSLP
         LA    3,1(3)
         BCT   9,PRSK2
         B     MAINLP
PRMVC    MVC   0(0,8),0(3)
*
DECD6    LA    15,DCOTBL
         LA    7,0
DCDLP    CH    7,=H'64'
         BNL   DCDDF
         CLM   5,1,0(15)
         BE    DCDOK
         LA    15,1(15)
         LA    7,1(7)
         B     DCDLP
DCDOK    LR    5,7
         BR    14
DCDDF    SR    5,5
         BR    14
*
SETMODE  BR    14                  STFSMODE REMOVED CAUSES S806
*
DOUP     L     15,TOPREC
         SH    15,=H'18'
         BP    UPOK
         SR    15,15
UPOK     ST    15,TOPREC
         B     MAINLP
DODN     L     15,TOPREC
         AH    15,=H'18'
         L     6,RECCNT
         C     15,6
         BNH   DNOK
         LR    15,6
DNOK     ST    15,TOPREC
         B     MAINLP
DOSAVE   ST    14,SSAVE
         MVC   STATMSG,SAVMSG
         OPEN  (OUTDCB,(OUTPUT))
         TM    OUTDCB+48,X'10'
         BZ    SFAIL
         LA    7,RECS
         L     8,RECCNT
         LTR   8,8
         BZ    SCLO
SLP      PUT   OUTDCB,0(7)
         LA    7,80(7)
         BCT   8,SLP
SCLO     CLOSE (OUTDCB)
         MVC   STATMSG,SVDMSG
         B     SDN
SFAIL    MVC   STATMSG,SERMSG
SDN      L     14,SSAVE
         BR    14
LOADP    ST    14,SLOAD
         OPEN  (INDCB,(INPUT))
         TM    INDCB+48,X'10'
         BZ    LNF
         LA    7,RECS
         SR    8,8
LLP      GET   INDCB,0(7)
         LA    7,80(7)
         LA    8,1(8)
         CH    8,=H'50'
         BL    LLP
LEO      CLOSE (INDCB)
         ST    8,RECCNT
         B     LOK
LNF      SR    8,8
         ST    8,RECCNT
LOK      MVC   STATMSG,LDMSG
         L     14,SLOAD
         BR    14
*
         LTORG
*
         DS    0F
DATAREAS EQU   *
SAVEARA  DC    18F'0'
RECCNT   DC    F'0'
TOPREC   DC    F'0'
DBLWRK   DC    D'0'
AIDBYTE  DC    X'00'
CURPTR   DC    F'0'
SDRAW    DC    F'0'
SSAVE    DC    F'0'
SLOAD    DC    F'0'
LINUM    DC    CL5' '
BLANKS   DC    CL80' '
STATMSG  DC    CL30'READY'
HDRTXT   DC    CL26'SEU EDITOR - PF3 TO EXIT'
LDMSG    DC    CL30'DATA LOADED'
SAVMSG   DC    CL30'SAVING...'
SVDMSG   DC    CL30'SAVED OK'
SERMSG   DC    CL30'SAVE FAILED'
POSTBL   DC    X'4040',X'C150',X'C260',X'C3F0',X'C540',X'C650',X'C760'
         DC    X'C8F0',X'4A40',X'4B50',X'4C60',X'4DF0',X'4F40',X'5050'
         DC    X'D160',X'D2F0',X'D440',X'D550',X'D660',X'D7F0'
FPOSTBL  DC    H'167',H'247',H'327',H'407',H'487',H'567',H'647',H'727'
         DC    H'807',H'887',H'967',H'1047',H'1127',H'1207',H'1287'
         DC    H'1367',H'1447',H'1527'
DCOTBL   DC    X'40C1C2C3C4C5C6C7C8C94A4B4C4D4E4F'
         DC    X'50D1D2D3D4D5D6D7D8D95A5B5C5D5E5F'
         DC    X'60E1E2E3E4E5E6E7E8E96A6B6C6D6E6F'
         DC    X'F0F1F2F3F4F5F6F7F8F97A7B7C7D7E7F'
*
         DS    0F
INDCB    DCB   DDNAME=SYSASMEU,DSORG=PS,MACRF=(GM),RECFM=FB,LRECL=80,  X
               EODAD=LEO
OUTDCB   DCB   DDNAME=SYSASMEU,DSORG=PS,MACRF=(PM),RECFM=FB,LRECL=80
*
SCRBUF   DS    CL3000
INBUF    DS    CL512
RECS     DS    50CL80
         END   SEU
