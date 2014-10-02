       PCC                                                              PAGE 006
       ZST
* M948-508,FMS,DEBUG,20,40,20000,700              ASSEMBLE LISP 1.5     LISPHERE
*      FAP
       COUNT   13000
       ABS
* FIELD TEST ASSEMBLY OF LISP 1.5       SEPTEMBER 1961
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *  
*
*
*    THIS IS THE 709 SECTION OF THE UPPER VERSION OF RWTML
*        SHARE DIST  NO.  709 AND  741
*        IT LOADS  BINARY 704 STYLE CARDS AND OCTAL CORRECTION CARDS
*    ON LINE
*
L      HED
       ORG     -47               IO POSITION LOAD AT -42
*      709 BINARY-OCTAL BOOTSTRAP LOADER
       IOCD    LOAD,0,21         COMMAND TO LOAD REMAINDER OF LOADER
       TCOA    1                 DELAY TILL LOADER IN
       TRA     LOAD
 LOAD  RCDA                      INITIATE NEXT CARD
       RCHA    LOAD5
       TCOA    *                 DELAY TILL CARD IS IN
       TEFA    CONTIN
       CAL     9L
       TZE     LOAD8             ZERO IMPLIES OCTAL CARD
       PDX     ,6                SET WORD COUNT
       STP     LOAD4             SET TO CHECK OR IGNORE SUM
       STA     LOAD2             SET CARD ORIGIN
 LOAD2 TXL     ****,4,0          OUT IF TRANSFER CARD
       AXT     0,4               SET I4 TO ZERO
 LOAD3 LDQ     9R+1,4            PICK UP WORD
       STQ*    LOAD2             STORE WORD
       ACL*    LOAD2             ADD TO CHECK SUM
       TXI     *+1,4,-1          ADVANCE FOR NEXT WORD
       TIX     LOAD3,2,1         COUNT WORDS TO BE STORED
       ERA     9R                COMPARE CHECK SUMS
       TZE     LOAD              AGREE-LOAD NEXT CARD
 LOAD4 HTR     LOAD              ERROR-START TO READ NEXT CARD
 LOAD5 IOCD    9L,0,24           COMMAND TO BRING IN BINARY IMAGE
 LOAD8 AXT     14,5              14 TO IR1 AND IR 4
*
       ABS                       RESUME STANDARD PUNCHING
*
 LOAD9 AXT     2,2               SET TO COUNT FIELD PAIRS
       CAL     9L+18,4           ROW
       ORS     9L+14,1           ROW UNION
       LDQ     9L+14,1
       TXI     LOD11,2,22        SET TO PEEL OFF SIX BITS
 LOD10 TXH     *+2,4,2           SKIP STORE TILL AFTER ONE-ROW
       SLW     ****              STORE OCTAL CORRECTION
       TIX     *-3,2,1           ADVANCE TO NEXT PAIR, THIS HALF
       TIX     LOAD9,4,2         ADVANCE TO NEXT ROW
       TNX     LOAD+1,5,13       OUT AFTER RIGHT HALF                   PAGE 007
       RCDA                      START NEXT CARD
       TXI     LOAD9,5,12        ADVANCE TO RIGHT HALF CARD
 LOD11 CLM                       CLEAR AC
       ALS     2
       LGL     1                 PEEL OFF BITS
       TIX     LOD11+1,2,4       COUNT COLUMNS PER FIELD
       TXH     *+2,4,12          USE 7-ROW AS FIRST SUM
       ACL     11L+4,2           ADD PREVIOUS SUM
       SLW     11L+4,2           NEW PARTIAL SUM
       TNX     LOD10,2,2         OUT IF SECOND FIELD OF PAIR
       STA     LOD10+1           STORE ADDRESS OF CORRECTION
       TXI     LOD11,2,44        RETURN TO PEEL OFF 12 BITS
*
               -1,,-STS          LEAD WORD FOR ATOM VERITAS-NUMQUAM-PERIT
*
       ORG     LOAD-34           COMMON STORAGE
COMMON BSS     0 
 9L    BSS     24                INPUT BUFFER
 9R    SYN     9L+1              CARD CHECK SUM
 11L   SYN     9L+20             TEMPORARY FOR OCTAL
LOADER SYN     LOAD
*
* PROPERTY LISTS FOR THE SPECIAL ATOMS NIL AND VERITAS-NUNQUAM-PERIT THE
* ZERO AND THE BINARY TRUTH ATOMS RESPECTIVELY
*
       ORG     COMMON-18
NILSXX         $PNAME,,-*-1
               -*-1
       MZE     -*-1
       OCT     453143777777      NIL
NILLOC         $ZERO
*
 STS           $APVAL,,-*-1
       MZE     -*-1,,-*-2
               1                 IS A CONSTANT ,,1 FOR APPLY
               $PNAME,,-*-1
               -*-1
       MZE     -*-1
       BCI     1,*TRUE*
*
       REM     **************************************************
       REM     BOOTSTRAP RECORD FOR 709 LISP
       REM
       ORG     100                BEGIN LISP
       HEAD    B
*
* BOTTOM       THE BOOTSTRAP RECORD FOR LISP ON SYSTEM AND TEMPORARY TAP
*
BOTTOM IOCD    BOTTOM+3,,BSRECL-2 I-O COMMAND TO READ IN  BOOTSTRAP REC.
       TCOA    1                  WAIT UNTIL RECORD IS READ IN
       TRA     BOTTOM+3           START F LISP
       AXT     3,4                NUMBER OF WORDS IN LOWER MEMORY
       CLA     3,4                MOVE THEM TO ORIGINAL POSITION
       STO     BOTTOM+3,4                                               PAGE 008
       TIX     *-2,4,1
       AXT     BSRECL,4           LENGTH OF BOOTSTRAP RECORD
       PXD     ,0,                CLEAR THE AC
       ACL     CHKSUM,4           COMPUTE THE CHECK SUM FOR RECORD
       TIX     *-1,4,1
       ERA     CHKSUM             COMPARE WITH THE CHECKSUM ON TAPE
       TZE     *-2                SKIP IF THEY ARE EQUAL
       HPR     1                  THEY DO NOT, STOP
       CLA     STRA               STR TRAP
       STO     2                  SET STR CELL
       CLA     FLTRA              FLOATING POINT TRAP
       STO     8                  SET TRAP CELL
       CLA     SYSTAP             TAPE SPECIFICATION FOR SYSTEM TAPE
       TSX     $(IOS),4           SET UP I-O COMMANDS
       TSX     LRTAPE,4           READ REST OF SYSTEM TAPE
               LOWREG,,-LOWREG    REST OF CORE
       XEC     $REW               REWIND SYSTAP
       TRA     $LOAD              GO TO READ ANY CORRECTION CARDS
*
CONTIN CLA     ZERO               LOADER RETURNS HERE, GO TO OVERLORD
       STO     0                  SET ZERO CELL
       TRA     OVRLRD             GO. TO OVERLORD
*
* NORMAL CONTENTS FOR CELLS 0, 2, 10 (OCTAL) RESPECTIVELY
*
 ZERC          -1,,-NILSXX        BEGINNING OF ATOM  NIL
 FLTRA TTR     FLAPTR
STRA   TTR     C$LINK
FLAPCX SYN     FLTRA
FLAPCY SYN     STRA
FLAPCZ SYN     ZERC
*
*
* LRTAPE       LISP READ TAPE PROGRAM FOR BINARY TAPES
*
LRTAPE CLA     1,4                PARAMETER WORD
       SXA     RTRX,4             SAVE INDEX REGISTERS
       SXD     RTRX,2
 RTTWO PAX     0,2                START ADDRESS
       STO     *+1                COUNT
       TXI     *+1,2,**           END + 1 ADDRESS
       SXA     RTADR,2            INITIALIZE ADDRESS
       PDX     0,2                COUNT IN IR 2
       CLA     RTTWO              TAG OF 2
       STT     RTADR              SET TAG
       SXD     RTADR,0            ZERO DECREMENT
       CLA     $LCH               PICK UP CURRENT LOAD CHANNEL INS.
       STO     RTLCH              MAKE IMMUNE FROM OVER WRITING
       CLA     $(IOU)             GET CURRENT I-O UNIT
       STO     RTIOU              MAKE PREVENT OVERWRITING
       STL     $TCO               WAIT FOR CHANNEL
       XEC     $TCO               TO GO OUT OF OPERATION
       IOT                        TURN OFF I-O CHECK                    PAGE 009
       NOP
       AXC     *,4
       XEC     $TRC               TURN OFF INDICATOR
       XEC     $TEF               TURN OFF INDICATOR
 RTRD  XEC     $RDS               SELECT TAPE
       PXD     0,0                CLEAR AC
       AXC     RTIOC,5            POINTER TO I-O COMMAND
       XEC     $RCH               RESET AND LOAD CHANNEL
 RTLC  XEC     RTLCH              LOAD CHANNEL
       LDQ     CHKSUM             PICK UP WORD READ IN
       STQ*    RTADR              PUT IT AWAY
       ACL*    RTADR              ADD TO CHECK SUM
       TIX     RTLC,2,1           DO ANOTHER LOAD CHANNEL
       AXC     RTIOD,4            POINTER TO DISCONNECT INSTRUCTION
       XEC     RTLCH              XEC LCH INS.
       ERA     CHKSUM             SUBSTRACT CHECK SUMS
       SLW     CHKSUM             STORE DIFFERECE
       CLA     RTIOU              PICK UP CURRENT IOU
       TSX     $(IOS),4           SET UP I-O COMMANDS
       STL     $TCO               WAIT FOR CHANNEL TO GO OUT OF OPERATION
       XEC     $TCO
       IOT                        TEST INDICATOR
       TRA     RCK                TRY AGAIN
       ZET     CHKSUM             SKIP IF CHECK SUMS AGREE
       TRA     RCK                TRY AGAIN
       AXC     RCK,4
       XEC     $TRC               TEST FOR REDUNDANCY
       XEC     $TEF               AND EOF
       LXA     RTRX,4             RESTORE INDEX REGISTERS
       LXD     RTRX,2
       TRA     2,4                EXIT
*
 RCK   LXD     RTADR,2            DID NOT WORK, SEE IF FIRST OR SECOND
       TXL     *+2,2,0
       HPR     2                  SECOND TRY FAILED, STOP
       SXD     RTADR,4            MAKE NON-ZERO
       XEC     $BSR               BACK SPACE AND TRY AGAIN
       LXA     RTRX,4             GET CALL WORD IR
       CLA     1,4                CALL PARAMETER
       PDX     0,2                COUNT TO IR 2
       TRA     RTRD
*
 RTIOC IOCT    CHKSUM,,1          BRING IN 1 WORD
 RTIOD IOCD    0,,0               DISCONNECT CHANNEL
*
*
*      (IOS)   INPUT OUTPUT SUPERVISOR A LA BELL LABS BE SYS 3
*
 (IOS) CAS     IOU                CHECK TO SEE IF SAME UNIT AS LAST TIME
       ERA     *+2                NO
       TRA     1,4                YES EXIT
       SXA     IOSX,4             NO, SAVE LINK 1B
       SXA     IOSY,2             SAVE INDEX 2
       STO     IOU                UPDATE IOU
       STA     $RDS               UPDATE ADDRESSES OF TAPE COMMANDS     PAGE 010
       STA     $WRS
       STA     $REW
       STA     $BSR
       STA     $WEF
       TPL     *+2                TAPE IN NORMAL DENSITH (BIN=HI, BCD=LO
       ERA     IOSBB              CHANGE DENSITY BIT
       STA     $SDN
       XEC     $SDN
       AXT     5,2                NUMBER OF COMMANDS TO BE SET
       PDX     0,4                CHANNEL NUMBER TO R
       TXI     *+1,4,12           TOTAL NUMBER OF COMMANDS - 3
 IOSA  CAL     IOU,4              PICK UP PROPER COMMAND
       SLW     COMAND,2           PUT IN PROPER PLACE
       TNX     IOSY,4,3           DECREMENT BY NUMBER OF CHANNEL
       TIX     IOSA,2,1           LOOP 5 TIMES
 IOSY  AXT     **,2               RESTORE INDEX 2
 IOSX  AXT     **,4               RESTORE LINK IR
       TRA     1,4
*
*      TAPE COMMANDS FOLLOW
*
       TEFC    0,4
       TEFB    0,4
       TEFA    0,4
       TCOC    **
       TCOB    **
       TCOA    **
       TRCC    0,4
       TRCB    0,4
       TRCA    0,4
       RCHC    0,4
       RCHB    0,4
       RCHA    0,4
       LCHC    0,4
       LCHB    0,4
       LCHA    0,4
IOU    PZE                        LAST UNIT USED
 IOSBB PZE     16                 BINARY BIT
       HEAD    0
*
*      ACTUAL TAPE COMMANDS USED BY PROGRAMS (SHOULD BE UNHEADED)
*
 RDS   RTBA    **
 WRS   WTBA    **
 REW   REWA    **
 WEF   WEFA    **
 SDN   NOP                        MAKE A SDN INSTRUCTION FOR 7090
 BSR   BSRA    **
 TEF   TEFA    0,4
 TCO   TCOA    **
 TRC   TRCA    0,4
 RCH   RCHA    0,4
 LCH   LCHA    0,4
COMAND BSS     0                                                        PAGE 011
SYSPPT PZE                        ADDRESS,,CHANNEL
SYSPOT         1*512+2*64+3,,1    INITIAL ASSIGNMENT OF A3
SYSPIT
SYSTMP
SYSTAP
TAPASG BSS     0
 (IOS) SYN     B$(IOS)
 LOAD  SYN     LOADER
 (IOU) SYN     B$IOU
       EJECT                                                            
*      CONSTANT POOL                                                    PAGE 012
       REM
 ZERO  PZE
 Q1    DEC     1
 Q2    DEC     2
 Q3    DEC     3
 Q4    DEC     4
 Q5    DEC     5
 Q6    DEC     6
 Q7    DEC     7
 Q8    DEC     8
 Q9    DEC     9
 Q10   DEC     10
Q12    DEC     12
 Q13           13
 Q14           14
 Q17   DEC     17
 Q20   DEC     20
 Q21   DEC     21
 Q22           22
 Q36   DEC     36
 Q63   DEC     63
 Q64   DEC     64
 Q128  DEC     128
 QO14  OCT     14
 QO17  OCT     17
 QO20  OCT     20
 QO22  OCT     22
 QO25  SYN     Q21
 QO33  OCT     33
 QO40  OCT     40
 QO41  OCT     41
 QO43  OCT     43
 QO50  OCT     50
 QO60  OCT     60
 QO61  OCT     61
 QO77  SYN     $Q63
 QO200 SYN     Q128
QO33Q2 OCT     3300
QO1Q9  OCT     1000000000
Q233Q9 OCT     233000000000
Q777Q9 OCT     777000000000
QO2Q11 OCT     200000000000
QT1            ,1
 QT2           ,2
 QT4           ,4
QT5            0,5
 QD1   PZE     ,,1
 QD2   PZE     ,,2
 QD5   PZE     ,,5
 QD6   PZE     ,,6
 QD7   PZE     ,,7
 QD20  PZE     ,,20
 QD21  PZE     ,,21                                                     PAGE 013
QP5    STR
OBLANK BCI     1, 00000
ZBLANK BCI     1,0
 QF1   DEC     1.0
 SBIT  MZE
MAGMSK OCT     377777777777
 AMASK PZE     -1
 DMASK PZE     ,,-1
PMASK  TXL     0,0,0
ADMASK PZE     -1,,-1
ATMASK PZE     -1,7
PDMASK SVN     ,,-1
PDTMSK SVN     0,7,-1
PTAMSK SVN     -1,7
CNTMSK OCT     000077000000
TAGMSK PZE     ,7
SEVENS SVN     -1,7,-1
BLANKS BCI     1,
BCONAT BSS     0                  BEGINNING OF CONSTANT ATOMS
PNAMEA PZE     PNAME
APVALD PZE     ,,APVAL
 BIND  PZE     ,,BIN
 FIXD  SYN     BIND
FLOATD         ,,$FLOAT
FSUBRD PZE     ,,FSUBR
FNARGD PZE     ,,FUNARG
LABELD PZE     ,,LABEL
LAMDAD PZE     ,,LAMBDA
 OCTD          ,,$OCT
PNAMED PZE     ,,PNAME
QUOTED PZE     ,,QUOTE
 SUBRD PZE     ,,SUBR
QSPECD PZE     0,,SPECAL
 QSYMD PZE     0,,SYM
               ERSETO,,PJ36
               PJ37,,PJ38         LOGAND LOGXOR
               -II7,,-II8         MAX MIN
               PLUS,,TIMES
               H01,,H02           PROTECT INTEGER OBJECTS
               H03,,H04
               H05,,H06
               H07,,H10
 H00A  PZE     H00
 H12A  PZE     H12
 H72A  PZE     H72
 H11D  PZE     ,,H11
 H14D  PZE     ,,H14
 H33D  PZE     ,,H33
 H34D  PZE     ,,H34
 H40D  PZE     ,,H40
 H74D  PZE     ,,H74
ECONAT SYN     H740               END OF CONSTANT ATOMS
       EJECT                                                            
CHKSUM BSS     5                  THESE CELLS ARE NOT WRITTEN ON TAPE   PAGE 014
       HEAD    B                  CELLS FOR LRTAPE
 RTRX  SYN     CHKSUM+1           PROTECTED STORAGE
 RTADR SYN     CHKSUM+2
 RTLCH SYN     CHKSUM+3 
 RTIOU SYN     CHKSYM+4
BSRECL EQU     CHKSUM-BOTTOM      LENGTH OF BOOTSTRAP RECORD
LOWREG SYN     *                  LOWEST REGISTER ON LISP RECORD
*
LWTAPE CLA     1,4                PARAMETER WORD
       STA     WTIOC              SET UP I-O COMMANDS
       STD     WTIOC
       STD     WTAD               COUNT
       SXA     WTX,4              SAVE LINK IR
       STZ     WTAG               ZERO TEST CELL
       STZ     WERC
       STL     $TCO
       XEC     $TCO               WAIT FOR CHANNEL
       IOT                        TURN OFF INDICATORS
       NOP
       AXC     *,4
       XEC     $TRC
       XEC     $TEF
 WTWS  XEC     $WRS               SELECT TAPE
       AXC     WTIOC,4            POINTER TO IO COMMAND
       XEC     $RCH               RESET AND LOAD CHANNEL
       LXA     WTIOC,4            ADDRESS OF BEGINNING OF BLOCK
 WTAD  TXI     *+1,4,**           END + 1 OF BLOCK
       SXA     WTACL,4            SET CHECKSUM COMPUTE ADDRESS
       LXD     WTIOC,4            COUNT OF BLOCK
       PXD     0,0                CLAER AC
 WTACL ACL     **,4               COMPUTE CHECKSUM
       TIX     *-1,4,1            LOOP
       SLW     CHKSUM             STOE IN CHECK SUM CELL
       AXC     WTIOD,4            CHECMSUM WRITE COMMAND
       XEC     $LCH               LOAD CHANNEL
       AXC     WRCK,4             TEST FOR WRITE REDUNDANCY
       XEC     $TRC
 WTX   AXT     **,4               RESTORE LINK IR
       TRA     2,2                EXIT
*
WRCK   NZT     WTAG
       TRA     WAGN               TRY TO WRITE ABAIN
       STL     WERC               CELL SAYS THERE WAS BAD TAPE TROULLE
       LXD     SYSTMP,4           FORM MESSAGE TO OPERATOR
       PXA     0,4
       ADD     $QO20
       ALS     6
       STO     WERM
       CLA     SYSTMP
       ANA     $QO17
       ORS     WERM
       TSX     OUTPUT,4           WRITE CHANGE TAPE MESSAGE
       MZE     BCDOUT                                                   PAGE 015
               WERM,,7
       HPR     3
WAGN   XEC     $BSR
       STL     WTAG
       TRA     WTWS
*
WERM   BCI     7,       IS BAD, CHANGE IT AND PUSH START.
*
WERC
 WTAG                             CELL NON-ZERO ON SECOND TRY
 WTIOC IOCT    **,,**             WRITE OUT BLOCK
 WTIOD IOCD    CHKSUM,,1          WRITE OUT CHECK SUM
*
* TAPDMP       DUMP CODE ON SYSTMP. USED BY OVERLORD
*
TAPDMP SXA     TPDMX,4            SAVE LINK IR
       TSX     TEREAD,4           CLEAN UP READ BUFFER
       CLA     SYSTMP             SPEC. FOR TEMPORARY TAPE
       TSX     $(IOS),4           SET UP I-O COMMANDS
TPRTY  TSX     LWTAPE,4           WRITE BOOTSTRAP RECORD
               BOTTOM,,BSRECL
       TSX     LWTAPE,4           WRITE REST OF CODE
               LOWREG,,-LOWREG
       XEC     $WEF               WRITE AN EOF MARK
       XEC     $REW               REWIND SYSTMP
       ZET     WERC               SEE IF SSYTMP WAS CHANGEDAFTER FIRST
       TRA     TPRTY              RECORD WAS WRITTE  IF SO REWRITE IT
 TPDMX AXT     **,4               RESTORE LINK IR
       TRA     1,4                EXIT
*
* OVLT         READS A NEW CORE IMAGE IN FROM SYSTMP, USED BY OVERLORD
*
OVLTXX PXD     0,4                LINK IR TO AC
       PDX     0,2                PUT IN IR 2 FOR SAFE KEEPING
       CLA     SYSTMP             TERMPORARY TAPE SPEC.
       TSX     $(IOS),4           SET UP I-O COMMANDS
       TSX     LRTAPE,4           READ IN BOOTSTRAP RECORD
               BOTTOM,,BSRECL
       TSX     LRTAPE,4           READIN RST OF LISP
               LOWREG,,-LOWREG
       XEC     $REW               REWIND SYSTMP
       TRA     1,2                EXIT
*
*
 INPUT CLA     2,4
       SXA     INX4,4             SAVE LINK IR
       STO     CALL
       CLA     SYSPIT             INPUT TAPE SPEC.                      PAGE 016
       TSX     $(IOS),4           SET UP I-O COMMANDS
       XEC     $SWT1              TEST FOR ON-LINE INPUT
       XEC     $RDS               SELECT INPUT TAPE
       TSX $RTX,4
 CALL      **,,-1
       TRA *+3
 INX4  AXT     **,4               RESTORE LINK IR
       TRA 5,4
       LXA     INX4,4             RESTORE LINK IR
       TMI 3,4
       TRA 4,4
       REM
C      HED
*
 RTX   SXA     RTXX,4             SAVE LINK IR
       CLA     1,4                GET PARAMETER WORD
       XEC     $SWT1              TEST FOR ON-LINE INPUT
       TXI     H1,,0              IS FROM TAPE
       RCDA
       TXI     RDBCD,,0
 H1    STA     CMMND              SET ADDRESS OF I-O COMMAND
       AXC     *+2,4              LOCATION TO INDEX REGISTER
       XEC     $TEF               TURN OFFF EOF INDICATOR
       CAL     H2                 PIC UP SWITCH
 H3    STO     H2                 SET TO TXH FIRST TIME THROUGH
       AXC     CMMND,4            LOCATION OF I-O COMMAND
       XEC     $RCH               RESET AND LOAD CHANNEL
       STL     $TCO               SET UP TCO COMMAND
       XEC     $TCO               WAIT FOR CHANNEL TO GO OUT OF OPERATION
       AXC     RTXBE,4            LOACTION OF BAD EXIT
       XEC     $TEF               GO IF EOF FOUND
       AXC     H2,4               LOCATION TO TRY AGAIN
       XEC     $TRC               GO IF REDUNDANCY CHECK FOUND
 RTXX  AXT     **,4               RESTORE LINK IR
       TRA     3,4                GOOD EXIT
 H2    TXH     RTXBE,,0           IS TXL ON SECOND TRY
       XEC     $BSR               BACKSPACE RECORD
       XEC     $RDS               SELECT TAPE
       CLS     H2                 PIC UP SWITCH
       TXL     H3,,0              GO TRY AGAIN
 RTXBE LXA     RTXX,4             LINK IR
       TRA     2,4
 RCD   RCDA                       RESTART AFTER ERROR
       LXD B2,1          X
       LXD B3,2          X
 RDBCD TEFA *+1                   TURN OFF END FILE INDICATOR
       STI B50                    SAVE INDICATORS
       RIL 3                      TURN INDICATORS 1,2 OFF
       RCHA LR                    READ IN 9 LEFT + RT INTO L,R
       LCHA BLR                   DELEAY, START 8LEFT + RT INTO 8L,8R
       TEFA 2,4                   GO TO END OF FILE RETURN IF EOF ON
 B1    LDQ L                      X
       STQ LS                     SET LEFT SUM
       SXD B2,1                   SAVE INDEX REGISTERS                  PAGE 017
       SXD B3,2                   X
       LXD B4,1                   SET DIGIT ROW COUNT
       LDQ R
       STQ RS                     SET RIGHT SUMP
       TSX C1,2            ENTER CONVERSION LOOP
 B2    TXL B5              LEAVE CONVERSION LOOP
       ALS 1
 B3    TXL C2              INITIALIZE BCD RECORD
 B5    LCHA LR                    DELAY UNTIL 8 IN, START READING 7
       LDQ 8L                     USE 8 ROW AS SUM
       STQ LS                     X
       LDQ 8R                     X
       STQ RS                     X
       TSX C1,2            ENTER CONVERSION LOOP
 B4    TXL B6,0,8          LEAVE CONVERSION LOOP
       ALS 3               ADD 8 TIMES 8 ROW
       TXL C3              X
 B6    CAL L               USE 9 ROW AS SUM
       SLW LS              X
       CAL R               X
       SLW RS              X
 B13   TXL B25,1,2                IS IT ZERO OR ONE ROW  YES'
 B14   LCHA LR                    DELAY, READ IN N RT AND LEFT
       LFT 1                      IS END OF RECORD INDICATOR ON
       TRA B9                     YES' END OF RECORD
 B8    CAL L                      NO' TEST LEFT ROW FOR
       ANA LS              ILLEGAL DOUBLE PUNCH
       TNZ B17             X
 B10   CAL L               FORM LOGICAL SUM
       ORS LS              OF LEFT ROWS
       CAL R               TEST FOR ILLEGAL
       ANA RS              DOUBLE PUNCH
       TNZ B17             X
 B11   CAL R               FORM LOGICAL SUM OF
       ORS RS              RIGHT RWS
       TNX B12,1,1         TEST FOR ZONE ROWS
       TSX C1,2            ENTER CONVERSION LOOP
       TXL B13             LEAVE CONVERSION LOOP
       TXL C3              ADD TO BCD RECORD
 B7    CAL 8L              ADD 8 LEFT ROW TO
       ORA LS              LEFT LOGICAL SUM
       SLW LDS             X
       LCHA LR                    DELAY, START READING X-L,R INTO L,R
       ANA LZ                     FORM INDICATOR FOR
       SLW LS              BOTH DIGIT AND ZERO
       CAL 8R              ADD 8 RIGHT ROW TO
       ORA RS              RIGHT LOGICAL SUM
       SLW RDS             X
       ANA RZ              FORM INDICATOR FOR
       SLW RS              BOTH DIGIT AND ZERO
 B40   TSX C1,2                   ENTER CONVERSION LOOP
       TXL B14             LEAVE CONVERSION LOOP
       ALS 4               SHIFT TO ZONE POSITION
       TXL C3              X                                            PAGE 018
 B9    CAL LS              SAVE LEFT ZONE SUM
       SLW L               X
       CAL LDS             FORM INDICATOR FOR
       COM                 ZERO AND X AND / OR Y
       ANA LZ              IN LEFT ROWS
       ANS LS              X
       CAL RS              SAVE RIGHT ZONE SUM
       SLW R               X
       CAL RDS             FORM INDICATOR FOR
       COM                 ZERO AND X AND/OR Y
       ANA RZ              IN RIGHT ROWS
       ANS RS              X
       TSX C1,2            ENTER CONVERSION LOOP
       TXL B15             LEAVE CONVERSION LOOP
       SLW TP              MULTIPLY INDICATOR
       ALS 2               BITS BY TEN
       ACL TP              X
       ALS 1               X
       TXL C3              X
 B15   CAL LDS             FORM INDICATOR FOR
       ORA LZ              BLANK COLUMNS IN
       ORA L               LEFT HALF OF CARD
       COM                 X
       SLW LS              X
       CAL RDS             FORM INDICATOR FOR
       ORA RZ              BLANK COLUMNS IN
       ORA R               RIGHT HALF OF CARD
       COM                 X
       SLW RS              X
       TSX C1,2            ENTER CONVERSION LOOP
       TXL B16             LEAVE CONVERSION LOOP
       SLW TP              MULTIPLY INDICATOR
       ALS 1               BITS BY 3 AND
       ACL TP              SHIFT TO ZONE POSITION
       ALS 4               X
       TXL C3              X
 B16   LXD B2,1            RESTORE INDEX REGISTERS
       LXD B3,2            AND RETURN TO MAIN
       LDI B50                    RESTORE INDICATORS
       TRA ,34             PROGRAM
 C1    SXD C4,1            SAVE ROW COUNT
 C9    CAL 1,4             INITIALIZE ADDRESSES
       ADM C7              X  ADD 6
 C4    TXL C6,,**          TRANSFER IO LEFT ROW
       ADM C7              RIGHT ROW, ADD 6 MORE
       LDQ RS              OBTAIN RIGHT SUM AND
       TXI C8               SKIP OVER LEFT SUM
 C6    LDQ LS              OBTAIN LEFT SUM
 C8    STA C2              SET BCD RECORD ADDRESS
       STA C3              X
       TXH C5,1,1          SKIP TEST IF DIGIT ROW
       STQ TP              TEST FOR NO SUM
       CAL TP              x
       TZE C11             X                                            PAGE 019
 C5    LXA C7,1            SET WORD COUNT
C7     PXD     6,0                CONVERT ROW
       LGL 1               X
       ALS 5               X
       LGL 1               X
       ALS 5               X
       LGL 1               X
       ALS 5               X
       LGL 1               X
       ALS 5               X
       LGL 1               X
       ALS 5               X
       LGL 1               X
       TRA 2,2             EXIT FROM ROW PROCEDURE
 C3    ACL 0,2             ADD TO BCD RECORD
 C2    SLW 0,1             STORE IN BCD RECORD
       TIX C7,1,1          COUNT WORDS
       LXD C4,1            RESTORE ROW COUNT
 C11   CLS C4              INVERT ROW SWITCH AND
       STO C4               TEST FOR RIGHT ROW DONE
       TMI 1,2             TRANSFER IF RIGHT ROW DONE
 C10   TXI C9              GO CONVERT RIGHT ROW
 B12   IIL 2                      CHANGE INDICATOR BIT 17
       LNT 2                      IS THIS TWELVE ROW
       TRA B100                   CHANGE
       TRA B40                    NO
 B25   TXL B7,1,1          IT IS XERO ROW OR ONE ROW
       LCHA ZLR
       TRA B8
 B17   SSM                        SET ERROR SIGN
       TXI     B16,4,1            RESTORE INDEX REGISTERS AND MAKE BAD X
 B100  TCOA *
       TSX C1,2
       TXL B200
       ALS 4
       TXL C3
 B200  TRA B9
 B50   PZE                        INDICATOR STORAGE
 LR    MTH L,0,2
 BLR   MTH 8L,0,2
 ZLR   MTH LZ,0,2
CMMND  MTH     **,0,-1
       ORG     COMMON
 TP    BSS 1               TEMPORARY
 LS    BSS 1               LEFT SUM
 RS    BSS 1               RIGHT SUM
 LDS   BSS 1               LEFT DIGIT SUM
 RDS   BSS 1               RIGHT DIGIT SUM
 LZ    BSS 1               LEFT ZERO ROW
 RZ    BSS 1               RIGHT ZERO ROW
 L     BSS 1               LEFT ROW
 R     BSS 1               RIGHT ROW
 8L    SYN LDS             8 LEFT ROW
 8R    SYN RDS             8 RIGHT ROW                                  PAGE 020
       ORG CMMND+1
0      HED
 BCDIN EQU 0
 RTX   SYN     C$RTX
       HEAD    D
*
*      SPACEX  PROVIDES A VARITY OF SPACES ON OFF LINE PRINTER
*
SPACEX XEC     $SWT5              TEST FOR NO OFF-LINE OUTPUT
       TRA     *+2
       TRA     2,4                RETURN
       SXA     SPX,4              SAVE LINK IR
       CLA     SYSPOT             SET UP TAPES
       TSX     $(IOS),4
 SPX   AXT     **,4               RESTORE LINK IR
       CLA     1,4                GET PARAMETER
       PAC     0,4                COMPLEMENT INTO IR 4
       XEC     $WRS
       XEC     $RCH
       LXA     SPX,4
       TRA     2,4                RETURN
8SPACE IORP    ZBLANK,,1          DOUBLE SPACE
6SPACE IORP    ZBLANK,,1          DOUBLE SPACE
4SPACE IORP    ZBLANK,,1          DOUBLE SPACE
2SPACE IORP    ZBLANK,,1          DOUBLE SPACE
       IOCD    0,,0               DISCONNECT CHANNEL
*
* OUTPUT       BCD OUTPUT ROUTINE FOR LISP
* SWITCHES...
*   3  PRINT ON-LINE
*   5  DONT WRITE TAPE FOR OFF-LINE PRINTING
*
OUTPUT SXA     WOTX,4             SAVE LINK IR
       CLA     2,4                GET PARAMETER WORD
       STD     WOTC               SET COUNT OF I-O COMMAND
       ADD     $Q20               END OF BLOCK
       STA     WOTM               SET MOVE LOOP
       STL     $TCO               WAIT FOR COMPLETION OF LAST OPERATION
       XEC     $TCO
       CLA*    1,4                GET TAPE SPECIFICATION
       TSX     $(IOS),4           SET UP I-O COMMANDS
       AXT     20,4               MAXIMIUM THAT MAY BE ON 1 RECORD
 WOTM  CLA     **,4               MOVE INTO BUFFER
       STO     WOTB,4
       TIX     WOTM,4,1
       XEC     $SWT5              TEST FOR NO TAPE OUTPUT
       TRA     *+2                IS OUTPUT ON TAPE
       TRA     WOTX               TEST FOR ON-LINE OUTPUT
       XEC     $WRS               SELECT TAPE
       AXC     WOTC,4             POINTER TO I-O COMMAND
       XEC     $RCH               RESET ANF LOAD CHANNEL
 WOTX  AXT     **,4               RESTORE LINK IR
       CLA     1,4                TEST FOR ON-LINE
       XEC     $SWT3              ON-LINE SENSE SWITCH                  PAGE 021
       TPL     3,4                EXIT IF DONE
* DM 716A - 48 CARDS - 02-09-59
*BCD ON-LINE PRINT ROUTINE FOR 709
* MODIFED FOR USE IN LISP 1.5
 WOTON SXA     WOTU,4             PRINT ON LINE
       SXA     WOTV,2             SAVE INDEX REGISTERS
       SXA     WOTW,1
       STZ     WOTT               SET SWITCH
       STZ     WOTS               SET SWITCH TO SKIP FIRST CHARACTER
       LXD     WOTC,6             COUNT IN  INDEX 4 AND 2
       TXI     *+1,4,WOTB-20      ADD BEGINNING OF BUFFER
       SXA     BC05,4             SET ADDRESS
 BC02  WPDA                  SELECT PRINTER
       ZET     WOTT               SKIP ON FIRST 72 CHARACTERS
       SPRA    9                  SET UP SECOND HALF OF LINE
       AXT     24,4          CLEAR
       STZ     COMMON+26,4     WORKING
       TIX     *-1,4,1         STORAGE
 BC03  CAL     BC50          STROBE STARTER
 BC04  SXA     BC01,1        WORKING CELL FOR N
 BC05  LDQ     0,2           PICK UP WORD TO CONVERT
       AXT     6,2           X2 COUNTS 6 CHARACTERS
 BC06  SLW     COMMON+26     STROBE
 BC07  PXD     **,0
       LGL     6             LOOK AT
       NZT     WOTS               SKIP IF NOT FIRST CHARACTER
       CLA     $Qo60              GET BCD BLANK FOR LEADNING CHARACTER
       ALS     1               ONE CHARACTER
       PAX     ,1
       CAL     COMMON+26     STROBE
       TNX     *+2,1,96      NOT 0
       ORS     COMMON+21,4   0
       TXH     BC08,1,94     BLANK
       TNX     *+3,1,62      NOT 11
       ORS     COMMON+23,4   11
       TNX     BC08,1,2
       TNX     *+3,1,30      NOT 12
       ORS     COMMON+25,4   12
       TNX     BC08,1,2
       TNX     *+3,1,18      NOT 8 COMBINATION
       TXI     *+1,1,2
       ORS     COMMON+5,4    8 COMBINATION
       ORS     COMMON+21,5   NUMBER
 BC08  ARS     1             MOVE STROBE
       STL     WOTS               SET SWITCH
       TIX     BC06,2,1      BACK FOR NEXT CHARACTER
       LXA     BC01,3        N
       TNX     BC15,2,1      OUT IF N WORDS DONE
       TNZ     BC04          BACK FOR REST OF HALF-CARD
       TXL     BC15,4,0      RIGHT-HALF DONE
       TXI     BC03,4,-1     BACK FOR RIGHT HALF
 BC15  RCHA    BC49
       STL     WOTT               SET SWITCH FOR SECOND HALF LINE
       TXH     BC02,1,1      BACK FOR MORE WORDS                        PAGE 022
       TCOA    *
 WOTU  AXT     **,4               RESTORE INDEX REGISTERS
 WOTV  AXT     **,2
 WOTW  AXT     **,1
       TRA     3,4                EXIT
*
 WOTT                             NON-ZERO ON SECOND HALF LINE
 WOTS                             ZERO FOR FIRST CHARACTER
 BC49  IOCD    COMMON+2,,24
 BC01  SYN     BC07
 BC50  SYN     $SBIT
*
 WOTB  BES     20                 OUTPUT BUFFER
 WOTC  IORP    WOTB-20,,**        WRITE RECORD FROM BUFFER
       IOCD    0,,0               DISCONNECT CHANNEL
BCDOUT SYN     SYSPOT
PPTOUT SYN     SYSPPT
*
       REM
PSHLDB RCDA
       RCHA *+3
       LCHA 0
       TTR 1
       IOCT 0,,3
       HEAD    0
* $SWTN COMMANDS                  ALL SWT COMMANDS ARE EXECUTED
* NOTE....     SWT COMMANDS MAY BE SIMULATED BY MAKING DOWN SWITCHES
*      ZET     $ZERO
*              AND UP SWITCHES
*      NZT     $ZERO
*
 SWT1  SWT     1
 SWT2  SWT     2
 SWT3  SWT     3
 SWT4  SWT     4
 SWT5  SWT     5
 SWT6  SWT     6
*
* SENSE LIGHT AND TEST INSTRUCTIONS TO BE EXECUTED OF DUMMYED
*
 SLN1  SLN     1
 SLN2  SLN     2
 SLN3  SLN     3
 SLN4  SLN     4
 SLF   SLF
 SLT1  SLT     1
 SLT2  SLT     2
 SLT3  SLT     3
 SLT4  SLT     4
       REM
       HEAD    O
*     C043 786 R. DALEY  ... GETTM ... READ CLOCK ROUTINE FOR 709 ......
*  RECODED AND SQUEEZED BY 0. 4. EDWARDS
 GETTM RPRA                                                             PAGE 023
       SXA     EXA,1
       SXA     EXB,2              ..
       SXA     EXC,4              ..
       AXT     33,2              SET UP FOR LOOP
       STZ     COMMON+33,2       ZERO CARD IMAGE AND WORKING STORAGE
       TIX     *-1,2,1           LOOP
       RCHA    SKP27             SET PRINTER TO SKIPPING FIRST 27 WORDS
       SPRA    7                  SENSE TIME CLOCK
       SPRA    9                  SET ECHO ENTRIES
       TNO     *+2               SKIP IF OVERFLOW LIGHT OFF
       STL     COMMON+5          OVERFLOW LIGHT ON, MAKE COMMON+4 =/ 0
       LCHA    ONWD               9 RIGHT ECHO
       AXT     9,4                ROW COUNT
       LCHA    SKP3               IOCPN ZERO,,3  IOCT COMMON,,1
 LOAD  LDQ     COMMON
       AXT     2,2                ..
CONV   PXD     ,0
       AXT     6,1                ..
       ALS     5                  ..
       LGL     1                  ..
       TIX     *-2,1,1            ..
       ORS     COMMON+3,2         ..
       CAL     COMMON+5,2         ..
       ACL     COMMON+3,2         ..
       SLW     COMMON+5,2         ..
       TIX     CONV,2,1           ..
       LCHA    SKP1              IOCPN ZERO,,1  IOCT COMMON,,1
       TIX     LOAD,4,1           COUNTS ROWS
       LCHA    ZERO              IOCD 0,,0  DISCONNECT PRINTER
       LDQ     COMMON+3           DATE
       PXD     ,0
       LGL     6                  ..
       TNZ     *+2                ..
       CLA     OCT60             INSERT BLANK
       LGL     12                 ..
       ORA     OCT61             INSERT / BETWEEN MONTH AND DAY
       ALS     18                 ..
       SLW     COMMON+3           ..
       PXD     ,0
       LGL     6                  ..
       TNZ     *+2                ..
       CAL     OCT60             INSERT BLANK
       LGL     12                 ..
       ORA     OCT60             PROVIDE BLANK AS LAST CHARACTER
       ORA     COMMON+3           ..
 EXC   AXT     **,4               RESTORE LINK IR
       SLW*    1,4                STORE DATE IN REGISTER SPECIFIED
       PXD     ,0
       LDQ     COMMON+4           TIME
       LGL     6                  ..
       TNZ     *+2                ..
       CAL     OCT60             BLANK
       LGL     30                 ..
       ORA     OCT33             PROVIDE DECIMAL POINT                  PAGE 024
       SLW*    2,4               STORE TIME
       ALS     8                 TURN ON OVER FLOW
       NZT     COMMON+5          LEAVE ON IF COMMON+5 IS NON ZERO
       TOV     *+1               TURN OFF OVER FLOW LIGHT
 EXA   AXT     0,1                RESTORE IRS
 EXB   AXT     0,2                ..
       TRA     3,4                EXIT........
 ZERO  PZE     0                  ..
       PZE
 SKP27 IOCTN   COMMON+6,,27
 SKP3  IOCPN   ZERO,,2            SKIP TWO WORDS
 SKP1  IOCPN   ZERO,,1            SKIP ONE WORD
 ONWD  IOCT    COMMON,,1          TRANSMIT ONE WORD TO COMMON
 OCT60 SYN     $QO60
 OCT61 SYN     $QO61              BCD /
 OCT33 SYN     QO33Q2             BCD .0
*  TIME  PRINTS THE DATE AND TIME .
 TIME  SXA     TIR,4              SAVE LINK IR
       TSX     GETTM,4            GET TIME FROM ON-LINE CLOCK
               TR+2               STORE DATE
               TR+2+1             STORE TIME
       TSX     OUTPUT,4           PRINT OUT DATE AND TIME
               BCDOUT             ON BCD OUTPUT TAPE
               TR,,17
       PXD     0,0
 TIR   AXT     **,4               RESTORE LINK IR
       TRA     1,4                RETURN
 TR    BCI     1,0 THE
       BCI     9,TIME (            ) HAS COME, THE WALRUS SAID, TO TALK
       BCI     7, OF MANY THINGS .....   -LEWIS CARROLL-
0      HED
 TIME  SYN     D$TIME
 GETTM SYN     D$GETTM
       REM
PAUSEF HPR 7
       TRA 1,4
       REM
       REM
*
* ERROR        PROCESSES ALL LISP ERRORS. NORMALLY GIVES ERROR NUMBERS, PAGE 025
*              ERROR LOCATION, LISP PRINT OF AC AND BACK TRACE OFALL
*              FUNCTIONS ENTERED ON PUSH DOWN LIST.
*
 ERAC                             PLACE TO STORE MACHINE REGISTERS
 ERMQ
 ERIND
 ERX                              INDEX 1,,INDEX 2
 ERROR TXH     *+1,,**            INDEX 4
       NZT     ERNULL             SEE IF ERROR PROGRAM IS TO BE EXECUTED
       XEC     EREXIT             NORMAL SETTING GOES TO EVALQUOTE
       STQ     ERMQ               SAVE MACHINE REGISTERS
       STI     ERIND
       SXA     ERX,1
       SXD     ERX,2
       LDI     SYSIND             PICK UP SYSTEM INDICATORS
       SIR     ERRORI             SET ERROR HAS OCURRED INDICATOR
       STI     SYSIND             UPDATE SYSTEM INDICATORS CELLS
       STO     ERT                AC TO BE PRINTED
       CLA     1,4
       STO     ERM                PUT IN ERROR MESSAGE
       LDC     ERROR,4
       PXD     0,4
       XCA                        AND CONVERT TO OCATL
       TSX     OCTALP,4
       ORA     OBLANK             INSERT LEADING BLANK
       SLW     ERN                PUT IN ERROR MESSAGE
       TSX     OUTPUT,4           WRITE OUT ERROR MESSAGE
               BCDOUT
               ERO,,9
       ZET     BACACT             SKIP IF BACK TRACE IS NOT ACTIVE
       TRA     BACER              GO TO SPECIAL ROUTINE
       STL     BACACT             MAKE BACK TRACE ROUTINE ACTIVE
       CLA     ERT                PICK UP AC ON ENTRANCE
       TSX     $PRINT,4           PRINT IT IN LISP
       RFT     NOBACT             TEST FOR NO BACK TRACE
       TRA     BACD               GO TO EXIT
       LDQ     $ZERO              ZERO THE ERROR LIST
       LXD     NUBPDL,4           BEGINNING OF PUSH DOWN LIST
       TXI     *+1,4,-1           PUSH UP BY -1
       SXD     BEX,4              SET UP ALL DONE TEST INSTRUCTION
       LXD     $CPPI,4            PICK UP CURRENT PDL COUNTER
 BEX   TXH     BACTD,4,**         GO IF ALL UNSAVED
       CAL     -1,4               EITHER UNSAVE OR UNWND
       ANA     $PMASK             DEPENDING ON COMPILED OR
       ERA     $QP5               SYSTEM SUBROUTINE PUT IT THERE
       TZE     *+3                TEST IS FOR STR OP
       TSX     UNSAVE,4           IN LAST WORD OF BLOCK FROM COMPILER
       TRA     *+2
       TSX     C$UNWND,4
       LXD     $CPPI,4            BEGINNING OF BLOCK JUST UNSAVED
       CLA     0,4                LAST IR 4 WORD
       PAX     0,4                FUNCTION ATOMIC SYMBOL
       PXD     0,4                PUT IN DECREMENT
       TSX     $CONS,4            ADD TO ERROR LIST                     PAGE 026
       XCA                        ANSWER TO MQ
       LXD     $CPPI,4            PUSH DOWN INDICATOR
       TRA     BEX                GO BACK FOR NEXT
 BACTD XCA                        LIST TO AC
       TSX     $PRINT,4           PRINT THE ERROR LIST
 BACD  STZ     BACACT             DE-ACTIVATE THE BACK TRACE ROUTINE
       XEC     EREXIT             NORMAL SETTING GOES TO EVALQUOTE
*
 BACER TSX     OUTPUT,4           WRITE OUT MESSAGE THAT BACK TRACE
               BCDOUT             CAUSED ANOTHER ERROR
               BACE,,7
       TRA     BACD               RESET AND RETURN
NOBACT BOOL    200                NO BACK TRACE INDICATOR
BACACT                            NON-ZERO MEANS BACK TRACE ACTIVE
 ERT                              TEMPORARY STORAGE FOR AC
 ERO   BCI     3,0*** ERROR NUMBER
 ERM                              ERROR NUMBER IN BCD GOES HERE
       BCI     2, INDEX 4 =
       REM
 ERN                              OCATL LOCATION GOES HERE
       BCI     2, OCTAL, ***
 BACE  BCI     7,0*** ABOVE ERROR TERMINATED BACK-TRACE ***
*
* FLAPTR AND OCT     GIVE ERROR DIAGNOSTICS FOR FLOATING POINT  TRAP AND
*              DIVIDE CHECK INCLUDING LOCATION AND CONTENTS OF AC.
*              BOTH MY BE IGNORED BY MAKNG CELL FPTGNR NON-ZERO.
 DCT   ZET     FPTGNR             TEST FOR IGNORE ERROR FLAG
       TRA     1,4                RETURN
       SXD     $ERROR,4           SAVE IR 4
       LDC     $ERROR,4           COMPLEMENT LOCATION OF ENTRANCE
       SXA     FLXT,4             SET TRAP ADDRESS
       SXD     FLXT,0             ZERO THE DECREMENT
       STL     FPTDV              SET DIVIDE CHECK FLAG
       TRA     FPTA               DO FLOATING POINT TRAP ERROR
*
FPTGNR                            TEST CELL IS NON-ZERO TO IGNORE TRAPS
*
FLAPTR STO     COMMON             SAVE AC
       CLA     0                  GET TRAP LOCATION
       STA     FLXT               SET EXT CELL
       STD     FLXT
       CLA     FLAPCZ             NORMAL CONTENTS OF ZERO
       STO     0
       CLA     COMMON             RESTORE AC                            PAGE 027
       ZET     FPTGNR             TEST FOR IGNORE TRAP
 FLXT  TXL     **,,**             IMMEDIATE EXIT INSTRUCTION
       STZ     FPTDV              INDICATE FLAPPING TRAP
       STD     $ERROR,4           SAVE LINK IR
 FPTA  XCA                        AC TO MQ
       TSX     OCTALP,4           CONVERT TO OCTAL
       SLW     FPTAC              STORE OCTAL FOR LEFT HALF OF AC
       TSX     OCTALP,4           CONVERT TO OCTAL
       SLW     FPTAD              STORE AWAY IN ERROR MESSAGE
       LDQ     FLXT               GET TRAP CELL CONTENTS
       RQL     18                 POSITION IN LEFT HALF OF MQ
       TSX     OCTALP,4           CONVERT TO OCTAL
       ORA     OBLANK             MAKE LAEDING ZERO A BLANK
       SLW     FPTLO              SAVE OCTAL FOR LOCATION OF ERROR
       AXC     FPTF,4             POINTER TO BEGINNING OF ERROR MESSAGE
       ZET     FPTDV              TEST FOR DIVIDE CHECK ERROR
       AXC     FPTD,4             DIVIDE CHECK MESSAGE
       CLA     0,4                PICK UP PROPER MESSAGE
       STO     FPTTY              STORE IN MESSAGE
       CLA     1,4
       STO     FPTTY+1            
       TSX     OUTPUT,4           WRITE ERROR MESSAGE
               BCDOUT
               FPTTY,,8
       PXD     0,0                CLAER AC
       TSX     $ERROR+1,4         GO TO ERROR PROGRAM
       BCI     1,*G  1*           FLOATING POINT TRAP OR DCT
 FPTTY BCI     3,            AT....
 FPTLO                            LOCATION OF ERROR
       BCI     2, WITH AC =
 FPTAC                            OCTAL LEFT HALF OF AC
 FPTAD                            OCTAL RIGHT HALF OF AC
 FPTF  BCI     2,0FLAP TRAP
 FPTD  BCI     2,0DIVIDE CHK
*
* THIS ROUTINE USES $ERROR,$ERRORP11 AND FPTGNR
 FPTDV                            DIVIDE CHECK INDICATOR CELL
*
*
* STRPNT       A DEBUGGING AID WHICH PRINTS THE DECREMENT OF THE AC AS
*      A LIST OR DUMPS AC AND IR 4 IN OCTAL WHICH EVER IS APPROPRIATE.
*
STRPNT ZET     STRT               TEST IF ROUNTINE IS ACTIVE.
       TRA     STREX              IT IS THEREFORE EXIT
       SXA     STRX,4             NO, SAVE LINK IR
       STO     STRA               SAVE AC
       STQ     STRQ               SAVE MQ
       STL     STRT               SET CELL TO INDAICTE ACTIVE
       LDQ     0                  PICK UP TRAP LOCATION                 PAGE 028
       STQ     STRXT              SAVE CONTENTS
       RQL     18                 ADDRESS PORTION TO LEFT HALF OF MQ
       TSX     OCTALP,4
       ORA     OBLANK             LEADING BLANK
       SLW     STRM               STORE TRAP ADDRESS IN OCATL
       CLA     FLAPCZ             RESTORE ORIGNAL CONTENTS OF ZERO
       STO     0
       TSX     OUTPUT,4
               BCDOUT             OUTPUT BCD MESSAGE
               STRN,,5
 STRO  LDQ     STRA               AC AT TIME OF TRAP
       TSX     OCTALP,4           CONVERT TO OCTAL
       SLW     STRAMA             STORE LEFT HALF IN OCTAL
       TSX     OCTALP,4           CONVERT TO OCTAL
       SLW     STRAMB             RIGHT HALF IN OCTAL
       LDQ     STRX               PICK UP LINK IR
       RQL     21                 SHIFT TO LEFT OF MQ
       TSX     OCTALP,4           CONVERT TO OCTAL
       ARS     6                  MAKE A HOLE
       ORA     OBLANK             MAKE LEADING ZERO A BLANK
       SLW     STRMC              PUT IN MESSAGE
       TSX     OUTPUT,4
               BCDOUT             OUTPUT IN BCD
               STRMD,,9
       CAL     STRA               PICK UP AC
       PDX     0,4
       ANA     PTAMSK             MASK OUT ONLY DECREMENT
       TNZ     STRF               GO IF ANY THING LEFT
STRTOP TXL     STRF,4,**          -TFS-1   IF NOT IN LIST STRUCTURE
STRBTM TXH     STRF,4,**          -BRK  GO TO EXIT IF NOT IN FREE STORAG
       PXD     0,4                OTHERWISE
       TSX     $PRINT,4           PRINT AS LISP LIST
 STRF  BSS     0
 STRX  AXT     **,4               DITTO LINK IR
       STZ     STRT               INDICATE ROUTINE IS INACTIVE
 STREX XEC     $SWT6              SHOULD WE GO BACK TO OVERLORD
       TRA     *+2
       TRA     OVRLRD             FIND NEXT OVERLORD DIRECTION CARD
       PXD     0,0
       SXD     $ERROR,4
       TSX     $ERROR+1,4
       BCI     1,*F  5*           STR TRAP ERROR
*
*
 STRA                             AC STROAGE
 STRQ                             MQ
 STRD  TXL     **,,**             MASK FOR PREFIX
 STRT                             CELL INDICATES ACTIVE IF NON-ZERO
 STRXT                            STORAGE FOR CONTENTS OF ZERO
 STRN  BCI     2,OSTR TRAP AT
 STRM  PZE                        TRAP LOCATION IN OCTAL
       BCI     2, OCTAL.
 STRMD BCI     4,0OCTAL CONTENTS OF AC                                  PAGE 029
STRAMA
STRAMB                            OCTAL CONTENTS OF AC GO HERE
       BCI     2, AND INDEX 4
STRMC                             OCATL LINK IR CONTENTS GO HERE
*
* THIS ROUTINE USES $PRINT,OUTPUT,BCDOUT AND OBLANK
*
*
       REM
       REM ERROR1 USER BY APPLY HAS ONE ARGUMENT AND PRINTS IT USING
       REM PRINT
       REM
ERROR1 SXD $ERROR,4
       TSX $ERROR+1,4
       BCI     1,*A  1*           APPLIED FUNCTION CALLED ERROR
*
* SETUP        TAKES  SIZE PARAMETERS AND SETS UP THE DEPENDENT CELLS
*              MAINLY IN THE RECLAIMER (GARBAGGE COLLECTOR) AND STRPNT
*
       HEAD    E
*
*      RESETP  ALTERNATE ENTRANCE TO SETUP TO CHANGE COMPOSITION OF
* FREE STRORAGE SLIGHTLY.
*
RESETP STL     RST                SET RESETUP SWITCH
       SXA     SUPX,4             SAVE LINK IR
       TRA     RSU                CHANGE GARBAGGE COLLECTOR PARAMETERS
*
 SETUP SXA     SUPX,4             SAVE LINK IR
       CLA     $TPG
       STO     $ORG
       ADD     LBINPG
       PAX     0,4
       TXI     *+1,4,-1
       SXD     C$LBPTP,4          SETUP FOR LAP
       PAC     0,4
       SXD     BLKETP,4           END OF BLOCK RESERVATION
       ADD     $Q1
       PAC     0,4
       SXD     $CPPI,4            SET PUSH DOWN CELLS
       SXD     $CSSI,4
       SXD     NUBPDL,4           PRIVATE COPY FOR BACKTRACE
       ADD     LPBPDL
       STA     ZPDL               G C ZEROS THE UNUSED PDL
       SUB     $Q20               PROTECTION AGINST COMPILER SAVING
       PAC     0,4                WTH OUT LOOKING
       SXD     ENDPDL,4           OUT OF PDL TEST
       CLA     $TFS
       SUB     LFREES                                                   PAGE 030
       STA     $TBT
       ADD     $Q1
       STA     $BFS
       CLA     LFULWS
       ARS     5
       ADD     $Q1
       STO     $LBT
       CLA     $BFS
       SUB     $LBT
       STO     $BBT
       SUB     $Q1
       STO     $TFW
       CLA     $BFS
       SUB     LFULWS
       STO     $BFW
       SUB     LPBPDL
       SUB     LBINPG
       SUB     $TPG
       TMI     SETERR             OVER LAPPING STORAGE ERROR
*              STRPNT SETUP
 RSU   LAC     $TFS,4
       TXI     *+1,4,-1
       SXD     STRTOP,4
       LAC     $BFS,4
       SXD     STRBTM,4
*              RECLAIMER SETUP
       LXA     $LBT,4
       SXA     A,4
       LXA     $BFS,4
       SXA     B,4
       LXA     $TFW,4
       SXD     C,4
       SXD     I,4
       SXD     MONE,4
       LXA     $TBT,4
       SXA     MBTTA,4
       SXA     D,4
       SXA     E,4
       SXA     MLTBT,4
       LXA     $TFS,4
       SXA     F,4
       LXA     $BFS,4
       SXA     SFWLD,4
       LXA     $BFW,4
       SXA     H,4
       LAC     $BFW,4
       SXD     MRKLST,4
       SXD     MLBDW,4
       LAC     $TFS,4
       TXI     *+1,4,-1
       SXD     MRKLST+1,4
       SXD     MLIST,4
       LAC     $BFS,4
       SXD     G,4                                                      PAGE 031
       SXD     MLBFA,4
       LDC     ENDPDL,4
       TXI     *+1,4,1
       SXA     MLEPD,4
       SXA     MLEPE,4
       LAC     $BBT,4
       SXD     MLBBJ,4
       ZET     RST                SKIP IF INITIAL SETIP
       TRA     SUPX               GO TO EXIT OTERWISE
       LAC     $BFS,4             BOTTOM OF FREE STORAGE
       TXI     *+1,4,-1           SUBSTRACT 1
       SXD     SUPFS,4            SET DECREMENT
       LAC     $MFS,4             LOWERP
       PXD     0,4                POINTER TO LWERP IN DECREMENT
       STO     $FREE              SET UP FREE
       ADD     $QD1
       STO     0,4                START MAKING FREE STORAGE
       TXI     *+1,4,1
 SUPFS TXL     *-3,4,**           -BFS
       STZ     0,4
       LAC     $BFW,4             BOTTOM FULL WORD SPACR
       PXD     0,4
       STD     FWORDL             SET UP FULL WORD LIST
       PDC     0,4                GET IT RUE IN INDEX
       SXD     SUPFV,4            USE TO CALCULATE LENGTH OF FULL WORD S
       LXA     $BBT,4             TFW + 1
       SXA     SUPFV,4            SET END + 1 ADDRESSS
 SUPFV TIX     *+1,4,**           LENGHT OF FULL WORD
       SUB     $QD1
 SUPFW STO     **,4               MAKE LIST
       TIX     *-2,4,1            LOOP
       STZ*    SUPFW              MAKE LAST ENTRY ZERO
       CLA     $OBLB              BEGINNING OF UNSORTED OBJECT LIST
       TSX     CNSFWL,4
 SUPX  AXT     **,4
       STZ     RST                ZERO RESETUP SWITCH
       PXD     0,0
       TRA     1,4
 RST                              RESETUP TEST CELL
SETERR TSX     OUTPUT,4
       MZE     BCDOUT             PRINT ON-LINE
               NOSET,,9
       TRA     SUPX               EXIT
 NOSET BCI     9,0OVERLAPPING PARAMETERS -SETUP- ERROR NUMBER *0  7*
*
       HEAD    0                                                        PAGE 032
*
*              STORAGE MAP CELLS FOR LISP
*
 TFS           UPERML-1           UPPER LIMIT OF FREE STORAGE
 MFS           LOWERP             LOW LIMIT OF PERM. LIST STRUCTURE
 BFS                              BOTTOM OF FREE STORAGE
 TBT                              TOP OF BIT TABLE
 BBT                              BOTTOM OF BIT TABLR
 TFW                              TOP OF FULL WORD SPACE
 BFW                              BOTTOM OF FULL WORD SPACE PROPER
 TPG           TOPROG
 ORG                              ORIGIN OF BINARY PROGRAM IN DECREMENT
LBINPG                            LENGTH OF BINATY PROGRAM
LPBPDL                            LENGTH OF PUBLIC PUSH DOWN LIST
LFULWS                            LENGTH OF FULL WORD SPACE + BIT TABLE
LFREES                            LENGTH OF FREE STORAGE
 LBT                              LENGTH OF FULL WORD BIT TABLE
* SAVE AND UNSAVE                 THE CLOSRD SUBROUTINES THAT CONTROL
*              THE PUBLIC PUSH DOWN LIST. THE CALLING SEQUENCES ARE ...
*
*      TSX     $SAVE,4
*      TXL     $ENDN,,END OF BLOCK TO BE SAVED  + 2
*      RETURN
*              WHERE N IN $ENDN IS THE NUMBER OF ITEMS TO BE SAVED
*
*      TSX     UNSAVE,4
*      RETURN
*              THE SAVED ITEMS MUST BE IN A CONTIGOUS BLOCK WITH THE
*              THE FIRST ITEM    PZE ATOMIC NAME OF SUBR,,IR 4
*              THE SAVE PARAMETER WORD IS ADDED AS THE LAST ITEM ON THE
*              BLOCK TO BE SAVED BUT IS NOT UNSAVED.
*
 SAVE  SXA     SAVY,2             SAVE INDEX 2 AND 1
       SXA     SAVZ,1
       STO     SAVT               SAVE THE AC
       CLA*    1,4                AMMOUNT TO SUBTRACT FROM CPPI IN AC
       PAX     0,1                PUT - NUMBER OF ITEMS TO BE SAVED + 1
 CPPI  TXI     *+1,1,**           IN IR 1 AND INCREMENT BE PUSH DOWN CNT
       TXL     NOPDL,1,**         GO TO NOPDL IF NOT ENOUGH PDL
       SXD     $CPPI,1            UP DATE PDL COUNTER LOCATION
       CLA     1,4                PARAMETER WORD
       STO     -1,1               PUT ON PUSH DOWN LIST
       PDC     0,2                LOCATION OF BLOCK TO BE SAVED + 2
       XEC     1,4                JUMP INTO SAVE TABLE
*
UNSAVE SXA     SAVY,2             SAVE INDEX 2 AND 1
       SXA     SAVZ,1
       STO     SAVT               SAVE THE AC
       LXD     $CPPI,2            CURRENT PUSH DOWN COUNTER
       CLA     -1,2               LAST SAVE PARAMETER WORD
       STA     SAVJ               SET FETCH AND TXI INSTRUCTIONS 
       STA     SAVK
       SXD     SAVI,2             SET UP TO RESTORE PDL COUNTER
 SAVJ  LAC     **,1               NUMBER TO BE UNSAVED                  PAGE 033
 SAVI  TXI     *+1,1,**           ADD PUSH DOWN COUNTER
       SXD     $CPPI,1            UPDATE PDL COUNTER CELL
       PDC     0,1                LOCATION OF END OF BLOCK + 2
 SAVK  TXI     **,4,1             JUMP TO PUSH DOWN TABLE AND SET IR 4
*                                 PROPER EXIT .
*
*              SAVE AND UNSAVE TABLE TO DO THE ACTUAL MOVING TO AND FROM
*              THE PUBLIC PUSHD DOWN LIST.
*
 END16 CLA     -17,2
       STO     -17,1
 END15 CLA     -16,2
       STO     -16,1
 END14 CLA     -15,2
       STO     -15,1
 END13 CLA     -14,2
       STO     -14,1
 END12 CLA     -13,2
       STO     -13,1
 END11 CLA     -12,2
       STO     -12,1
 END10 CLA     -11,2
       STO     -11,1
 END9  CLA     -10,2
       STO     -10,1
 END8  CLA     -9,2
       STO     -9,1
 END7  CLA     -8,2
       STO     -8,1
 END6  CLA     -7,2
       STO     -7,1
 END5  CLA     -6,2
       STO     -6,1
 END4  CLA     -5,2
       STO     -5,1
 END3  CLA     -4,2
       STO     -4,1
 END2  CLA     -3,2
       STO     -3,1
 END1  CLA     -2,2
       STO     -2,1
 END0  CLA     SAVT               RESTORE THE AC
 SAVZ  AXT     **,1               AND INDEX 1 + 2
 SAVY  AXT     **,2
       TRA     2,4                EXIT
*
 SAVT                             TEMPORARY STORAGE FOR AC
*              TIMING INFORMATION .. SAVE AND UNSAVE 34 + 4N CYCLES
*              ON THE 709  (SUBTRACT 5 CYCLES FOR SAVE AND 4 FOR UNSAVE
*              ON THE 7090)
*
       REM
       REM TERPDL
       REM RESETS PUBLIC PUSH DOWN LIST TO ZERO                         PAGE 034
       REM
TERPDL CLA $CSSI
       STD CPPI
       TRA 1,4
 CSSI
ENDPDL TXL     *+1,4,**           OUT OF PDL TEST INSTRUCTION (IS XEC)
 NOPDL SXD $ERROR,4
       TSX $ERROR+1,4
       BCI     1,*G  2*           OUT OF PUBLIC PUSH DOWN LIST
       REM
       REM
*
       HEAD    E
       REM
*
* CNSFWL       USED BY SETUP TO MOVE ALL FULL WORDS ON PERMENENT OBJECTS
*              TO THE FULL WORD SPACE.
*              ALSO BUCKET SORTS THE PERMENENT OBJECTS.
*
CNSFWL SXA     CNFWX,4            SAVE INDEX REGISTERS
       SXA     CNFWY,2
       PDX     0,4                POINTER TO OBJECT LIST
 CNMLP CLA     0,4                NEXT WORD ON LIST
       STD     CNXT               POINTER TO NEXT WORD
       PAX     0,2                POINTET TO AN ATOM
       SXD     CNAT,2             SAVE THE POINTER TO THE ATOM
       CLA     0,2
       ANA     TAGMSK             TEST FOR NUMBER
       TNZ     CNNM               MAKE A NUMVER
 CNSLP CLA     0,2                NEXT WORD ON ATOM
       PAX     0,2                CAR OF ATOM, SEARCH FOR FULL WORD
       TXH     *+2,2,$SUBR        SUCH AS $SUBR
       TXH     CMKO,2,$SUBR-1
       TXH     *+2,2,$FSUBR
       TXH     CMKO,2,$FSUBR-1
       TXH     *+2,2,$PNAME
       TXH     CMPNT,2,$PNAME-1
 CNRS  PDX     0,2                IS NONE OF THE ABOVE SO CDR TO IR 2
 CNRT  TXH     CNSLP,2,0          GO BACK IF NOT END OF PROPERTY LIST
CNNR   LXD     CNXT,4             POINTER TO NEXT OBJECT
       TXH     CNMLP,4,0          GO BACK IF NOT END
       PXD     0,0                CLAER AC
 CNFWX AXT     **,4               RESTORE INDEX REGISTERS
 CNFWY AXT     **,2
       TRA     1,4                EXIT
*
CNNM   CLA     0,2
       TMI     CNNR               DONT MOVE NUMBERS WITH MZE PREFIX
       PDX     0,4
       CLA     0,4
       TSX     $CONSW,4
       STD     0,2
       TRA     CNNR               MAKE UP THE NEW NUMBER
*                                                                       PAGE 035
 CMKO  PDX     0,2                PUT ONE WORD IN FULL WORD SPACE
       CLA     0,2                GET NEXT WORD ON PROPERTY LIST
       STD     CNX                POINTER TO REST OF OBJECT
       TMI     CMK                SKIP MOVING TO REST OF OBJECT
       PAX     0,4                SENSED, OTHERWISE GET POINTER TO FULL
       CLA     0,4                WORD AND WORD IT SELF IN AC
       TSX     $CONSW,4           PUT IT IN FULL WORD SPACE
       ARS     18                 MOVE POINTER TO WORD IN FWS TO ADDRESS
       STA     0,2                REPLACE THE ADDRESS
       LXD     CNX,2              POINTRE TO NEXT WORD ON PROPERTY LIST
       TRA     CNRT               RETURN
*
 CMK   SLW     0,2                RESTORE WORD WITH PLUS SIGN
       TRA     CNRS               GO BACK
*
 CMPNT PDX     0,2                PUT PRINT NAME IN FULL WORD SPACE
       CLA     0,2                NEXT WORD ON PROPERTY LIST
       STD     CNX                POINTER TO NEXT WORD ON PROPERTY LIST
       PAX     0,2                POINTET TO PNAME LIST
       SXD     CNVA,2             SAVE IT
 CMPLP CLA     0,2                FIRST FORD ON PNAME LIST
       TMI     CMPS               SKIP IF WORD IS FLAGGED
       STD     CNFT               POINTER TO NEXT WORD ON PNAME LIST
       PAX     0,4                POINTER TO FULL WORD
       CLA     0,4                FULL WORD
       TSX     $CONSW,4           PUT IN FULL WORD SPACE
       ARS     18                 POINTER TO WORD
       STA     0,2                RPLACE THE ADDRESS
       LXD     CNFT,2             POINTER TO NEXT WORD ON PNAME LIST
       TXH     CMPLP,2,0          GO BACK IF NOT END
 CMPS  CLA     CNVA               POINTER TO PNAME LIST
       LDQ     CNAT               ATOM THAT WE ARE WORKING ON
       TSX     BUKSRT,4           PUT ON BUCKET SORTED OBJECT LIST
       LXD     CNX,2              POINTER TO NEXT WORD ON ATOM
       TRA     CNRT               GO BACK
*
*
*
* RECLAIMER    LISP 1.5 STORAGE CONTROL PROGRAM. CODED 1 MARCH 1961
*
RECLAM SXA     RCX,4              SAVE INDEX REGISTER
       SXA     RCY,2
       SXA     RCZ,1
       STI     RCIND              AND MACHINE REGISTETS
       STO     RCAC
       STQ     RCMQ
       STZ     RCBE               INITIALIZE BAD EXIT CELL
 RCA   LDQ     RCSGNL             SIGNAL PHASE 1
 A     AXT     **,4               BIT TABLE LENGTH
 B     STZ     **,4               DOTTOM FREE STORAGE
       TIX     *-1,4,1            ZERO THE BIT TABLE
       LXD     ENDPDL,4           END OF PDL
       SXD     RCIA,4             SET UP TNX INSTRUCTION
       LXD     $CPPI,4            CURRENT PUSH DOWN LIST LOC.           PAGE 036
 RCIA  TNX     MLPDE,4,**         AMMOUNT OF PUSH DOWN LIST AVAILABLE
       SXD     MLPDC,4            SET CELL IN MRKLST
       SXA     ZPDLA,4            LENGTH LEFT BAR FOR ZEROIND PDL
       AXC     OBLIST,2           POINTER TO OBJECT LIST
       LDI     SYSIND             SYSTEM INDICATORS
       ZET     EVQRTS             SKIP F DURING READ IN THE EVALQUOTE
       RNT     DEBUGI             SKIP MARKING OBLIST IF IN A DEBUG
       TSX     MRKLST,4           MARK THE LIST
*
*               TEMLIS MARKER
*               TEMLIS IS A LIST IN FREE STORAGE AND FULL WORD SPACE
*               OF THE FORM (CONS (CONSW BEG,,END) TEMLIS) AND INDICATES
*               PLACES WHERE LIST STRUCTURE MAY BE DURING A GARBAGE
*               COLLECTION. USED PRINCIPALLY BY THE COMPILER
*
       STZ     TMLM               SET EXIT SWITCH
       LXD     TEMLIS,4
 TMLJ  CLA     0,4                NEXT WORD ON TEMLIS
       STD     TMLM               SAVE POINTER TO NEXT WORD
       PAX     0,4                POINTER TO FULL WORD
       CLA     0,4                FULL WORD
       PAX     0,4                BEGINNING OF ARRAY
       SXD     TMLD,4
       PDX     0,1                END OF ARRAY
       TXI     *+1,1,1            ADD 1
 TMLK  SXA     TMLE,1
 TMLD  TNX     TMLH,1,**          SUBTRACT BEGINNING , GIVES COUNT IN IR
       SXA     GCPDLC,1           LAST USE IS MARKING PDL, SAVE LENGTH
 TMLE  LDI     **,1               PICK UP WORD
       OFT     TMPTM              SKIP IF NOTAG OR PREFIX
       TRA     TMLG               NOT A LIST, DO NOT MARK
       PIA                        ITEM TO AC
       STA     TMLF               SAVE ADDRESS
       PDX     0,2
       TSX     MRKLST,4           MARK THE DECREMENT
 TMLF  AXT     **,2               ADDRESS OF WORD TO IR
       TSX     MRKLST,4           MARK IT
 TMLG  TIX     TMLE,1,1           GET NEXT WORD IN ARRAY
 TMLH  LXD     TMLM,4             NEXT TEMLIS ITEM
       TXH     TMLJ,4,0           GO IF NOT DONE
       ZET     TMLM               TEST FOR EXIT
       TRA     MPDLF              ALL DONE
       LDC     $CSSI,4            BEGINNING OF PDL
       SXD     TMLD,4             SET UP CELL
       LDC     $CPPI,1            FIRST FREE CELL ON PDL
       STL     TMLM               INDICATE LAST USE OF LOOP
       STZ     GCPDLC             PUSH DOWN LENGTH INITIALLY ZERO
       TRA     TMLK               GO MARK PUSH DOWN LIST
 MPDLF LXD     ARYLIS,4           START TO MARK ACTIVE ARRAYS
MARYB  TXL     RCB,4,0            GO IF NO ARRAYS
       CLA     0,4                NEXT WORD ON ARYLIS
       STD     MARYT              SAVE POINTER TO NEXT WORD
       PAX     0,4                ARYATOM TO AC
 MARYA CLA     0,4                NEXT WORD ON ATOM                     PAGE 037
       PAX     0,4
       TXL     *+2,4,$ARRAY-1     SERCH FOR ARRAY SPECIFICATION
       TXL     MRKA,4,$ARRAY      GO IF FOUND
       PDX     0,4                POINTER TO NEXT WORD
       TXH     MARYA,4,0          GO IF NOT END OF ATOM
 MARYC LXD     MARYT,4            NEXT WORD ON ARYLIS
       TRA     MARYB
*
 MRKA  PDX     0,4                GET ARRAY SPECIFICATIONS
       CLA     0,4
       PAX     0,4
       CLA     0,4
       PAX     0,4
       CLA     0,4                FIRST SPEC. WORD
       PAX     0,2                END OF ARRAY + 1
       STA     MRKE               END OF ARRAY + 1
       CLA     1,4
       STO     MRKP               SECOND SPEC. WORD  TOTAL L,, LIST L
       PAX     0,1                TOTAL LENGTH
       SXD     MAA,1              UPDATE TNX INSTRUCTION
 MAA   TNX     MARYC,2,**         LOCATION OF BEGINNING OF ARRAY
       SXA     MAB,2              PREPARE TO COMPLEMENT
 MAB   AXC     **,2
 C     TXI     *+1,2,*            TOP FULL WORD
       PXA     0,2                CALCULATE BIT TABLE WORD AND BIT
       LGR     5                  BIT NUMBER IN TO MQ
       PAX     0,2                WORD NUMBER IN IR 2
       PXD     0,0                ZERO AC
       LGL     5                  BIT NUMBER
       PAX     0,4
       TXH     MBTT,4,30          GO TO MARK BY 32
       PXD     0,0                ZERO AC
 MAC   ORA     BIT,4
       TNX     MBTTA,1,           DECREMENT COUNT
       TIX     MAC,4,1            RUN BIT COUNT DOWN
       ORA     BIT                PUT IN ZERO BIT
 MBTTA ORS     **,2               TOP BIT TABLE, SET BITS
       TNX     MRKF,1,1           GO IF DONE
       TXI     *+1,2,-1           DECREMENT BIT WORD BY ONE
 MBTT  CAL     MONS               ALL ONES TO AC
 MAE   TNX     MAD,1,32           DECREMENT COUNT BY 32
 D     ORS     **,2               TOP BIT TABLE, SET ALL BITS
       TXI     MAE,2,-1           DECREMENT BIT TABLE WORD COUNT
 MAD   PXA     0,1                PREPARE TO MARK LAST BITS
       PAC     0,1                COMPLMENT COUNT
       PXD     0,0                ZERO AC
 MAF   ORA     MBITF,1            SET PROPER BIT
       TXI     *+1,1,1            INCREMENT COUNT BY ONE
       TXH     MAF,1,0            GO UNTIL COUNT REACHES ZERO
 E     ORS     **,2               TOP BIT TABLE, SET BITS
 MRKF  LXD     MRKP,1             GET LIST LENGTH IF ANY
       TXL     MARYC,1,0          EXIT IF A NON-LIST ARRAY
 MRKE  CLA     **,1               LIST ITEM
       PDX     0,2                                                      PAGE 038
       TSX     MRKLST,4           MARK IT
       TIX     MRKE,1,1           GET NEXT ITEM
       TRA     MARYC              EXIT
*
*               ALL MARKING DONE. NOW SWEEP FREE STORAGE.
*
 RCB   AXT     0,2                ZERO COUNT IR
       STZ     FSC                INITIALIZE COUNTER
       LDQ     RCSGNM             SWEEPING SIGNAL TO MQ
       AXC     $FREE,1            INITIALIZE LAST LOC IR
 F     AXC     **,4               TOP FREE STORAGE
 SFSL  CLS     0,4                PICK UP WORD
       TMI     SFSC               COLLECT IF SIGN NOW MINUS
       STQ     0,4                RESTORE WORD WITH + SIGN
 SFSA  TXI     *+1,4,1            INCREMENT BY ONE
 G     TXL     SFSL,4,**          LOOP IF LESS THAN BOTTOM FREE STORAGE
       STZ     0,1                ZERO LAST WORD COLLECTED
       SXA     FSC,2              SAVE COUNT
       TRA     SWPFWS
 SFSC  PXD     0,4                THIS LOCATION
       STO     0,1                STORE POINTER IN LAST WORD COLLECTED
       PDX     0,1                UP DATE LAST WORD IR
       TXI     SFSA,2,1           UPDATE COUNTER
*
*               NOW SWEEP FULL WORD SPACE WITH THE BIT TABLE
*
SWPFWS AXT     FWORDL,4           BEGINNING OF FULL WORD LIST
       SXA     SFWA,4             INITIALIZE ADDRESS
       STZ     FWC                ZERO FULL WORD COUNTER
 H     AXC     **,1               BOTTOM FULL WORD SPACE
 I     TXI     *+1,1,**           TOP FULL WORD SPACE
       PXA     0,1                GET ADDRESS OF BIT TABLE CORRESPONDING
       LGR     5                  TO THE BOTTOM OF FULL WORD SPACE
       PAX     0,4                BIT TABLE WORD
       TXI     *+1,4,1            MAKE INDEXING EASY
       PXD     0,0                ZERO AC
       LGL     5                  BIT NUMBER
       PAX     0,2                INTO IR 2
       TXI     *+1,2,1            MAKE INDEXING EASY
       LAC     H,1                SET UP IR 1
 SFWLD LDI     **,4               BOTTOM FREE STORAGE, (TBT + 1)
       ONT     MONES              SKIP IF ALL WORDS TO BE SAVED
       TRA     SFWSC              SEARCH FOR THE WORDS TO BE COLLECTED
       TXI     *+1,1,-32          DECREMENT CURRENT LOC IR
 SFWB  TIX     SFWLD,4,1          INDEX THROUGH BIT TABLE
 SFWDN CLA     FWC                ALL DONE, GET FULL WORD COUNTER
       STO*    SFWA               SET UP LAST CELL COLLECTED
       LDQ     RCSGNN             PASE 3 SIGNAL
       ZET     RCT                TEST FOR OUT OF ARRAY SPACE ENTRANCE
       TSX     RELOC,4            RELOCATE AND COMPACT FULL WORD SPACE
ZPDLA  AXT     **,4               ZERO UNUSED PDL
ZPDL   STZ     **,4               ZERO PDL WORD
       TIX     *-1,4,1
       LDQ     CRITWN             CRITACL WORD NUMBER                   PAGE 039
       STZ     RCBE               INITIALIZE BAD EXIT TEST CELL
       CLA     FWC                NUMBER OF FULL WORDS COLLECTED
       TLQ     RCEA               TRANSFER IF MORE THAN CRITACL COLLECT
       STL     RCBE               NOT ENOUGH, SIGNAL BAD EXIT
 RCEA  ADD     TFWC               ADD TOTAL OF FULL WORDS COLLECTED
       STO     TFWC               UPDATE COUNTER
       LGL     4                  INCREASE TOLERENCE BY 2 TO THE 4 TH
       CLA     FSC                NUMBER OF FREE STORAGE CELLS PICKED UP
       TLQ     RCEB               TRA IF GREATER THAN CRITACL NUMBER
       STL     RCBE               NO, SIGNAL BAD EXIT
 RCEB  ADD     TFSC               ADD TOTAL OF FREE COLLECTED TO DATE
       STO     TFSC               UPDATE TOTAL
       CLA     RCC                NUMBER OF RECLAIMATION CYCLES EXECUTED
       ADD     $Q1                INCREMENT BY 1
       STO     RCC                UPDATE TOTAL
       CLA     RLC                NUMBER OF TIMES RELOCATION OF FWS
       ZET     RCT                SKIP IF NO RELOCATION
       ADD     $Q1
       STO     RLC                UPDATE COUNTER
       NZT     RCBE               SKIP IF BAD EXIT
       TRA     RCED               DO GOOD EXIT
       TRA     RCEC               DO VERBOSE AND BAD EXIT
 RCED  NZT     VERBOS             SKIP IF TALKATIVE
       TRA     RCEXIT             DO EXIT
RCEC   LAC     RCX,4              GET EXIT IR4
       PXD     0,4                AND CONVERT FOR PRINTING
       XCA
       TSX     OCTALP,4
       ORA     OBLANK
       SLW     RCT1
       CLA     FWC                FULL WORD COUNTER
       TSX     $DECON,4           CONVERT TO BCD DECIMAL
       SLW     RCT4               PUT IN MESSAGE
       CLA     FSC                FREE STORAGE COUNTER
       TSX     $DECON,4           TO DECIMAL
       SLW     RCT5               PUT IN MESSAGE
       CLA     GCPDLC             NUMBER OF ACTIVE REGISTERS ON PDL
       TSX     $DECON,4           TO DECIMAL
       SLW     RCT6               IN MESSAGE
       TSX     OUTPUT,4           WRITE OUT MESSAGE
               BCDOUT
               RCTM,,19
       ZET     RCBE               SKIP IF GOOD EXIT
       TRA     RCBEX              DO BAD EXIT
RCEXIT CLA     RCAC               RESTORE MACHINE REGISTERS
       LDQ     RCMQ
       LDI     RCIND
 RCX   AXT     **,4               AND INDEX REGISTERS
 RCY   AXT     **,2
 RCZ   AXT     **,1
       TRA     1,4                EXIT
 SFWSC ONT     MBIT,2             CHECK FOR CURRENT BIT
       TRA     SFWC               IS OFF, COLLECT WORD
       TXI     *+1,1,-1           IS ON, DECREMENT CURRENT LOC IR       PAGE 040
 SFWD  TIX     SFWSC,2,1          INDEX THROUGH THE BITS
       AXT     32,2               SET UP IR WITH NUMBER OF BITS PER WORD
       TRA     SFWB               EXAMINE NEXT WORD IN BIT TABLE
*
 SFWC  PXD     0,1                COLLECT THIS WORD, POINTER TO THIS WOR
       ADD     FWC                D PLUS NUMBER OF WORDS COLLECTED IN AC
 SFWA  STO     **                 SET LAST WORD COLLECTED
       ADD     $Q1                INCREMENT NUMBER OF FULL WORDS COLLECT
       STA     FWC                SAVE FULL WORD COUNTER
       PDC     0,1                COMPLEMENT CURRENT LOCATION
       SXA     SFWA,1             TO FORM TRUE ADDRESS FOR UPDATE STORE
       PDX     0,1                CURRENT LOCATION POINTER
       TXI     SFWD,1,-1          DECREMENT CURRENT LOCATION AND RETURN
*
* MRKLST       THE RECURSIVE SUBROUTINE THAT DOES ALL LIST MARKING
*
MRKLST TXH     MLEXT,2,**         BFW BAR, REJECT POINTERS TO PROGRAM
       TXL     MLEXT,2,**         TFS BAR - 1, REJECT POINTERS TO LOADER
       SXA     MSRTN,1            SAVE IR 1
       SXA     MRKX,4             SAVE LINK IR
       AXT     1,1                PRESET TO ONE FOR FAST PUSH DOWN ACESS
       TRA     MLIST              DO ACTUAL MARKING
*
 MWIN  CLS     0,2                MARK THIS WORD IN FREE STORAGE
       TPL     MOUT               TRANSFER OUT IF ALREADY MARKED
       STO     0,2                CAR OF LIST
       PAX     0,2                CAR TO IR 2
 MLEPD STD     **,1               ENDPDL + 1, SAVE CDR OF LIST ON PDR
       TXI     *+1,1,1            INCREMENT PUSH DOWN COUNTER
 MLPDC TXL     MLIST,1,**         ENDPDL - C($CPPI) BAR, GO IF NOT NOPDL
 MLPDE TSX     RCERR,4            OUT OF PUSH DOWN LIST, FATAL ERROR
       BCI     3,0NO PLD -MRKLST-
 MLEPE CLA     **,1               ENDPDL + 1, GET CDR OF LIST
       PDX     0,2                PUT IN  IR 2
 MLIST TXL     MOUT,2,**          TFS BAR - 1, OUT IF NOT IN LISP STORAG
 MLBFA TXL     MWIN,2,**          BOTTOM FREE STORAGE BAR, IN FREE
 MLBBJ TXL     MOUT,2,**          BBT BAR  OUT IF POINTER TO BIT TABLE
 MLBDW TXL     MONE,2,**          BOTTOM FULL WORD BAR, IN FULL WORD
       TRA     MOUT               EXIT , NOT ANY OF THE ABOVE
*
 MONE  TXI     *+1,2,**           TOP FULL WORD
       PXA     0,2                CALCULATE BIT TABLE WORD AND BIT
       LGR     5
       PAX     0,2                BIT TABLE WORD
       PXD     0,0
       LGL     5                  BIT TABLE BIT
       PAX     0,4
       CAL     BIT 4              PICK UP BIT
 MLTBT ORS     **,2               TOP BIT TABLE, PUT IN BIT
 MOUT  TIX     MLEPE,1,           GO BACK IF IN RECURSION
 MSRTN AXT     **,1               OTHERWISE RESTORE IR 1
 MRKX  AXT     **,4               AND LINK IR                           PAGE 041
 MLEXT TRA     1,4                AND EXIT
*
* RCERR        RECLAIMER FATAL ERROR DUMP ROUTINE
*
 RCERR SXD     $ERROR,4           SAVE IR 4
       SXA     *+1,4              COMPLEMENT IR 4 TO GET ERROR MESSAGE
       AXC     **,4
       TXI     *+1,4,1            LOCATION OF ERROR MESSAGE
       SXA     RCFEM,4            BUILD OUTPUT CALL
       TSX     OUTPUT,4           WRITE ERROR MESSAGE ON TAPE
               BCDOUT
 RCFEM         **,,3              WRITE OUT 3 WORDS
       STZ     $FREE
       STZ     FWORDL             ZERO STORAGE LISTS
       LDI     SYSIND             GET SYSTEM INDICATORS
       SIR     ERRORI             SET ERRIR INDICATOR
       STI     SYSIND             UPDATE REGISTER
       TSX     $TIME,4            PRINT THE CURRENT TO TIME
       TRA     OVRLRD             GET NEXT DIRECTION CARD
*
 RCBEX LDI     RCIND              RESTORE MACHINE REGISTERS
       CLA     RCAC
       LDQ     RCMQ
       LXA     RCX,4              AND INDEX REGISTERS
       LXA     RCY,2
       LXA     RCZ,1
       SXD     $ERROR,4           SAVE IR 4
       STO     $ERAC              SAVE THE CONTENTS OF THE AC
       PXD     0,0
       TSX     $ERROR+1,4         GO TO ERROR
       BCI     1,*GC 2*           NOT ENOUGH WORDS COLLECTED -RECLAIMER-
*
* RELOC        RELOCATES ALL ITEMS IN FULL WORDS SPACE INTO A COMPACTED
*              BLOCK TO MAKE BLOCKS OF CONTIGOUS STORAGE AVAILABLE FOR
*              ARRAYS.
*
 RELOC SXA     RELX,4             SAVE LINK IR
       TSX     RCERR,4            THIS RPUTINE HAS NOT BEEN CODED YET.
       BCI     3,0NO RELOCATOR
 RELX  AXT     **,4               RESTORE LINK IR
       TRA     1,4                RETURN TO MAIN PROGRAM
*
* MESSAGES AND CONSTANTS PLUS STORAGE GO HERE
*
RCTM   BCI     5,0GARBAGE COLLECTOR ENTERED AT
RCT1                              THE CALL LOCATION IS PUT HERE
       BCI     4, OCTAL.
       BCI     2,  FULL WORDS                                           PAGE 042
 RCT4                             NUMBER FULL WORDS COLLECTED
       BCI     1, FREE
 RCT5                             FREE STORAGE WORDS COLLECTED
       BCI     3,   PUSH DOWN DEPTH
 RCT6                             DEPTH ON PUSH DOWN LIST GOES HERE
 FWC   SYN     RCT4
 FSC   SYN     RCT5               STORAGE SAVING SYN S
GCPDLC SYN     RCT6
 RCC                              TOTAL NUMBER OF RECLAMATION CYCLES
 RCT                              TEST CELL TO SEE IF RELOCATION WAS DON
RCRLOC SYN     RCT
 RLC                              NUMBER OF TIMES RELOCATION WAS DONE
 TFWC                             TOTAL FULL WORDS COLLECTED
 TFSC                             TOTAL FREE STORAGE COLLECTED
 MONES SYN     SEVENS
 MONS  SYN     SEVENS
 RCIND                            INDICATOR STORAGE
 RCBE                             TEST CELL FOR BAD EXIT
MARYT                             TEMPORAY STORAGE
CRITWN SYN     $Q10
*
* BIT TABLES FOR MARKING AND SWEEPING FULL WORD SPACE
*
       OCT     20
       OCT     40,100,200,400,1000,2000,4000,10000,20000,40000,100000
       OCT     200000,400000,1000000,2000000,4000000,10000000,20000000
       OCT     40000000,100000000,200000000,400000000,1000000000
       OCT     2000000000,4000000000,10000000000,20000000000            PAGE 043
       OCT     40000000000,100000000000,200000000000
 BIT   OCT     400000000000
 MBIT  SYN     BIT+1
 MBITF SYN     BIT-32
*
*
VERBOS OCT     777777777777       THIS CELL NON ZERO MAKES THE RECLAIMER
*                                 VERY TALKATIVE
RCSGNL OCT     111111111111
RCSGNM OCT     222222222222
RCSGNN OCT     333333333333       PHASE SIGNAL FOR MQ
 TMLM                             TEMPORARY STORAGE
 TMPTM SVN     ,7                 PREFIX AND TAG MASK
 MRKP                             TEMPORARY STORAGE
*
 TEMXX         -*-1,,-*-2         PERMENANT TEMLIS ITEMS
               BCONAT,,ECONAT
               -*-1,,-*-2
               C$PROBE,,C$PROEN   LAP PROTECTED AREA
               -*-1               END OF TEMLIS
               BEGBLK,,ENDBLK-1   FUNCTION STORAGE
*
       EJECT                                                            PAGE 044
*              STORAGE BLOCK FOR FUNCTIONS ALL OVER THE PACKAGE
*
BEGBLK BSS     0
*              RECLAIMER STORAGE TO BE MARKED
TEMLIS         ,,-TEMXX
ARYLIS                            LIST OF ACTIVE ARRAYS
 RCAC                             AC STORAGE
 RCMQ                             MQ-STORAGE
*              CNSFWL STORAGE
 CNXT                             POINTER TO NEXT WORD ON LINEAR OBJLIST
 CNX                              POINTER TO NEXT WORD ON PROPERTY LIST
 CNFT                             POINTER TO NEXT WORD  ON PNAME LIST
 CNAT          *                  POINTER TO FIRST WORD OF CURRENT ATOM
 CNVA                             POINTER TO FIRST WORD OF PNAME LIST
*******************************************************
*                                 THESE CARDS ARE A BLOCK
       HEAD    A                  $ALIST AND RET IR4
CSV
       HEAD    0                  ARGUMENT REGISTERS
ALIST                             REFERED TO BY COMPILED FUNCTIONS
       REM
       REM REGISTERS FOR FUNCTION ARGUMENTS.  ARG1 ANDARG2 ARE NOT
       REM      NORMALLY USED.
       REM
ARG1
 ARG2
 ARG3
 ARG4
 ARG5
 ARG6
 ARG7
 ARG8
 ARG9
 ARG10
ARG11
ARG12
ARG13
ARG14
ARG15
ARG16
ARG17
ARG18
ARG19
ARG20
       REM
*************************************************
       HEAD    R                  AND
 EVA1          $AND
 EVA2
 EVA9
       HEAD    A                  APPEND
AS1            $F1
CWR1
       HEAD    A                  APPLY                                 PAGE 045
 ASS1
 ASSL
 ASSA
 AST1
 AST2
 AST3
 AST4
       HEAD    R                  COPY
 CS1           $COPYN
 CS2
       HEAD    C                  CP1
 CR1           $F12
 CR2
 CWRL
       HEAD    A                  EVCON
 ECS1          $COND
 ECS2
 ECS3
 ECS4
       HEAD    R                  EVLIS
 EVLX          EVLISL             LINK IR
 ELA                         ALIST
       HEAD    A                  EVP26
 EVS1                             IR4, BOTTOM OF PROTECTED TEMP. STORAGE
 EVSE
 EVSA
 EVTRK MZE                        TRACE SWITCH
 EVCDR                            ARG LIST FOR SUBR ARGUMENTS
 EAG11 BES     10                 ARGUMENT BLOCK FOR EVAL
 EVTDE                            CDR(E)
 EVD2
       HEAD    R                  GO SPECIAL FORM
 GOX           $GO                LINK IR
       HEAD    R                  LABP
 BFS4
       HEAD    R                  LAMP
 BFS2
 BFS3
*
       HEAD    C                  LINK FOR COMPILED FUNCTIONS
 LNKA                             LINK STORAGE FOR AC
 LNKB                             LINK STORAGE FOR MQ
       HEAD    D                  MAPCAR
 RET           $PMAPCA
 L
 F
       HEAD    R                  MAPCON
 MCN5          -$)069B
 MCN4
 MCN3
 MCN2
       HEAD    R                  MAPLIS
 MS1           -$)069A            LINK IR STORAGE
 MS2                         ARGUMENT L                                 PAGE 046
 MS3                         FUNCTIONAL ARGUMENT
 MS4                         FINAL ANSWER
 MS5                         INTERMEDIATE ANSWER
       HEAD    R                  OR
 EVR1          $OR
 EVR2
 EVR9
       HEAD    A                  PAIR
 TEM                              FIRST ARGUMENT
 LIS                              SECOND ARGUMENT
       HEAD    P                  PRINAR
 PAS3
 PAS4
       HEAD    R                  PROGRAM FEATURE
 INTRX         $PROG               LINK INDEX REGISTER
 INTB                             CURRENT STATEMENT
 INTGL                            GO LIS,(LIST OF PROGRAM POINTS) + IR2
 INTPL                            PAIR LIST
 INTGS                            GO SWITCH , NON-ZERO IF GO OR RETURN
       HEAD    I                  READ1
 RS1           $F13
 RS2
PRINTL                            TEMPORARY STORAGE FOR PRINT OR PUNCH
       HEAD    R                  SEARCH
 SRS1          $SRCH              IR4
 SRS2                             L
 SRS3                             P
 SRS4                             F
 SRS5                             U
       HEAD    R                  SETQP
 REPS1         $SETQ
 REPV
 REPT1
       HEAD    B                  SUBLIS
 X1            $F17               IR4 OF SUBLIS
 X2                               CDR(E)
 X3                               CAR(E)
 X4                               SUBLIS(P,CDR(E))
 X5                               CDAR(J)
 P
 E
       HEAD    R                  SUBST
 SXT
 SZ
 SX
 SY
 ST
       HEAD    Q                  ADD, ETC.
 AMIR                             IR 4 STRAGE
 AMIND                            INDICATOR REGISTER STORAHE
 AMLIS                            LIST STORAGE
 AMQ                              TYPE STORAGE
*                                 ARRAY MAKE PROGRAM
 AFAT                             ARRAY ATOM GOES HERE                  PAGE 047
 ATMP                             TEMPORARY STORAGE
       HEAD    S                  EVALQUOTE STORAGE
 EVQAN
       BSS     100                EVALQUOTE BUFFER
 EVQB  MZE                        TEST CELL FOR READ IN
       HEAD    F
*              CHARACTER FUNCTIONS
 BBPNT BSS     1                  POINTER TO REMAINDER OF LIST
 PIND  BSS     1
*              MKNO
 MKT1                             TEMP STORAGE TYPE (FIX OR FLD)
       BSS     25                 ROOM FOR MORE STORAGE
ENDBLK BSS     0
       EJECT                                                            
       HEAD    0                                                        PAGE 048
* CONSW        PUTS FILL WORDS IN FULL WORD SPACE
*
 CONSW SXA     CSWX,4             SAVE LINK IR
 FWLOR LXD     FWORDL,4           PICK UP FULL WORD LIST
       TXL     FWLOUT,4,0         TEST FOR NO MORE
       STQ     CSWQ               SAVE MQ
       LDQ     0,4                PICK UP POINTER TO NEXT WORD ON FWL
       SLQ     FWORDL             UP DATE FULL WORD LIST POINTER
       STO     0,4                PUT AC IN FULL WORD AREA
       PXD     0,4                POINTER TO AC
       LXD     FWORDL,4           POINTER TO NEXT AVAILABLE WORD
LOWARY TXH     CSWO,4,**          BOTTOM FULL WORD SPACE, TEST FOR ARY
       SXD     *-1,4              AVAILABLE LOCATION AND UPDATE SAME
 CSWO  LDQ     CSWQ               RESTORE MQ
 CSWX  AXT     **,4               RESTORE LINK IR
       TRA     1,4                EXIT
 CSWQ                             TEMPORARY STORAGE FOR MQ
FWORDL                            POINTER TO FULL WORD LIST
*
* CONS         BASIC LISP FUNCTION PUTS A WORD IN FREE STORAGE
*
 CONS  SXA     CNSX,4             SAVE LINK IR
       LXD     $FREE,4            GET FREE STORAGE LIST POINTER
       TXH     *+2,4,0            SKIP IF NOT OUT OF FREE STORAGE
       TSX     FROUT,4            OUT OF FREE STORAGE
       ARS     18                 DECREMENT TO ADDRESS
       STA     0,4                PUT ADDRESS AWY
       CLA     0,4                GET POINTER TO NEXT WORD IN FREE
       STD     FREE               PUT IN FREE
       SLQ     0,4                PUT DECREMENT AWAY
       PXD     0,4                POINTER TO WORD
 CNTR1 AXT     **,4               LOW ORDER 15 BITS OF CONS COUNTER KEPT
       TIX     *+3,4,1            DECREMENT COUNT BY 1
       TSX     ARREST,4           COUNT EXHAUSTED, RELOAD OR STOP
       AXT     -1,4               RELOAD NUMBER
       SXA     CNTR1,4            PUT IN COUNTER
 CNSX  AXT     **,4               RESTORE LINK IR
       TRA     1,4                EXIT
 FREE                             POINTER TO FREE STORAGE LIST
*
ARREST NZT     TCOUNT             SKIP IF COUNS COUNTER ON
       TRA     1,4                OTERWISE RETURN
       STO     CNTM               SAVE AC
       CLA     CNTS               GET REST OF COUNTER
       TZE     AWHOA              GO TO ERROR CALL IF EXHAUSTED
       SUB     CTG                DECREMENT BY 32,768
       STO     CNTS               UPDATE COUNTER
       CLA     CNTM               RESTORE AC
       TRA     1,4                E7IT TO RELOAD CETR1
*
 AWHOA SXA     TCOUNT,0           DESACTIVATE THE CONS COUNTER
       CLA     CNTST              PICK UP  INITIAL COUNT
       LDQ     $FIXD              PICK UP $FIX                          PAGE 049
       SXD     $ERROR,4           SAVE LINK IR
       AXT     8,4                8 SPARE CONSES FOR $MKNO
       SXA     CNTR1,4
       TSX     $MKNO,4            MAKE THE COUNT A NUMBER
       TSX     $ERROR+1,4         GO TO ERROT
       BCI     1,*F  1*           CONS COUNTER TRAP
*
* SPEAK        TURNS THE CONTENTS OF THE CONS COUNTER INTO A FIXED POINT
*              NUMBER.
*
 SPEAK CLA     $AMASK             GET ADDRESS MASK
       ANA     CNTR1              PICK UP 15 LOW ORDER BITS
       ORA     CNTS               OR IN REST OF COUNT
       STO     CNTM               SAVE CURRENT VALUE
       CLA     CNTST              PICK UP INITIAL VALUE
       SUB     CNTM               SUBSTRACT CURRENT VALUE TO GET NUMBER
       LDQ     $FIXD              OF CONSES.  PUT $FIX IN MQ
       TRA     $MKNO              MAKE THE RESULT A NUMBER
*
* BLOCKR       BLOCK RESERVATION ROUTING USED IN DECLARING ARRAYS.
*
BLOCKR SXA     BLKX,4             SAVE LINK IR
       STL     NROOM              SET UP TOO BIG TEST CELL
       STA     BLKB               BE RESERVED
       LXD     $ORG,4             ADDRESSOF FIRST REGISTER AVAIALABER
 BKOR  PXA     0,4                ADDRESS OF FIRST REGISTER FOR ARRAYS
       ADM     BLKB               ADDRESS OF END OF BLOCK
       STA     BLKC               INITIALIZE STZ LOOP TO CLEAN OUT BLOCK
       PAX     0,4
       SUB     $Q1
       STA     BLKBB
BLKETP TXL     BLKOUT,4,**        BOTTOM BIT TABLE AR, GO IF WONT FIT
       SXD     $ORG,4             UPDATE ORG
       CLA     -1,4               POINTER TO NEXT WORD ON FULL WORD LIST
       STD     FWORDL             UPDATE FULL WORD LIST
 BLKB  AXT     **,4               LENGTH OF BLOCK
 BLKC  STZ     **,4               ZEROP THE BLOCK
       TIX     *-1,4,1
       CLA     BLKBB              GET ANSWER
 BLKX  AXT     **,4               RESTORE LINK IR
       TRA     1,4
BLKBB                             ANSWER STORED HERE
*
*               VAROUIS ENTRANCES TO THE RECLAIMER
*
*               FWLOUT - OUT OF FULL WORD LIST
FWLOUT STO     CSWQ               SAVE FULL WORD
       PXD     0,0                ZERO AC
       STZ     RCRLOC             SIGNAL NO RELOCATION IS NECESSARY
       TSX     RECLAM,4           DO THE WORK
       CLA     CSWQ               RESTORE AC
       TRA     FWLOR              RETURN TO CONSW
*              FROUT - OUT OF REE STORAGE
 FROUT SXA     FRX,4              SAVE LINK IR                          PAGE 050
       STZ     RCRLOC             SIGNAL NO RELOCATION NECESSARY
       TSX     RECLAM,4           DO THE WORK
 FRX   AXT     **,4               RESTORE LINK OR
       TRA     -2,4               NON-STANDARD EXIT
*              BLKOUT - OUT OF FULL WORD SPACE FOR ARRAYS
BLKOUT STL     RCRLOC             SIGNAL RELOCATION NECESSARY
       PXD     0,0                CLEAR AC
       NZT     NROOM              FALL THROUGH ON SECOND CONSECUTIVE ENT
       TRA     BLKX               EXIT FROM BLOCKR ROUTINE
       TSX     RECLAM,4           DO THE WORK
       CLA     FWORDL             PICK UP POINTER TO FIRST AVAILABLE WOR
       STD     LOWARY             SET UP LOWARY
       PAC     0,4                COMPLEMENT INTO IR 4
       STZ     NROOM              SET UP TOO BIG TEST CELL
       TRA     BKOR               DO BLOCK RESERVATION
NROOM
*
*
* COUNT        A FUNCTION OF 1 ARGUMENT ( AFIXED POINT NUMBER) TURNS ON
*              THE CONS COUNTRE AND LOADS IT WITH THAT NUMBER
*              A LOAD OF NIL  SIMPLY LEAVES THE PREVIOUS CONTENTS IN THE
*              COUNTER
*
 COUNT STL     TCOUNT             ACTIVATE THE CONS COUNTER
       TNZ     CNTA               GO IF ARUGMENT S NOT NULL
       CLA     CNTM               OLD VALUE OF CNTR1
       STA     CNTR1              PUT IT THERE
       TRA     CNTB               CLEAR AC AND EXIT
 CNTA  SXA     CNTX,4             RELOAD COUNTER WITH FIXED POINT ARG.
       SXA     CNTY,2             SAVE IDNEX REGISTERS
       PDX     0,2                ARGUMENT TO INDEX 2
       TSX     FIXVAL,4           EVALUATE AS A FIXED POINT NUMBER
       STO     CNTST              SET INITIAL VALUE CELL
       STA     CNTR1              LOW ORDER 15 BITS TO CNTR1
       ANA     PDTMSK             MASK OUT LOW ORDER 15 BITS
       STO     PDTMSK             STORE REMAINDER IN H IGH ORDER CELL
 CNTX  AXT     **,4               RESTORE INDEX REGISTERS
 CNTY  AXT     **,2
 CNTB  PXD     0,0                GIVE VALUE OF NIL
       TRA     1,4                EXIT
 CNTST                            INTAL VALUE OF COUNT
*
* UNCONT       DEACTIVATE THE CONS COUNTER
*
UNCONT SXA     TCOUNT,0           DEACTIVATE THE CONS COUNTER
       CLA     CNTR1              GET CURENT CONTENST OF COUNTER
       STA     CNTM               SAVE IN TEMP STORAGE
       PXD     0,0                GIVE VALUE OF NULL
       TRA     1,4                EXIT
*
 CNTS                             HIGH ORDER BITS OF CONS COUNTER
 CNTM                             TEMPORARY STORAGE
 CTG           ,1                 LOW ORDER BIT OF HIGH ORDER 20 BITS
*                                                                       PAGE 051
*
E      HED
*      DECON AND NUMNAM
*
*      DECON TAKES A DECIMAL NUMBER (+ OR -) AS INPUT IN THE AC AND
*      GIVES AS OUTPUT THE BCD REPRESENTATION OF THAT NUMBER.  LO ORDER
*      BITS ARE IN AC. HI ORDER BITS IN MQ.  LEADING ZEROS ARE
*      SUPPRESSED.  IF THERE ARE NO HI ORDER BITS, MQ IS ZERO.  THE
*      P BIT AND SIGN OF AC WILL AGREE.
*
*      NUMNAM TAKES AS INPUT A POINTER TO A DECIMAL INTEGER (+ OR 0) AND
*      CAUSES THE BCD REPRESENTATION OF THAT NUMBER TO BE PRINTER, WITH
*      LEADING ZEROS SUPPRESSED.
       REM
       REM
 DECON STZ     DETS1              SIGNAL FOR DECON EXIT
       STZ     DELOD              SET LO ORDER DIGITS TO ZERO
       SXA     DEIR4,4            SAVE IR4
       TRA     DE7
       REM
NUMNAM STL     DETS1              SIGNAL FOR NUMNAM EXIT
       SXA     DEIR4,4            SAVE IR4
       PDX     ,4                 PLACE INPUT NUMBER IN AC
       CLA     0,4
 DE7   STL     DETS2              SIGNAL FOR NO HI- ORDER DIGITS
       STO     DEINP              SAVE INPUT FOR SIGN TEST
       DCT                        SHUT OFF DIVIDE CHECK LIGHT
       NOP
       XCL                        NUMBER TO MQ
       AXT     36,4               INDEX FOR SHIFTING
 DE4   STZ     DEDIG              DEDIG WILL RECIEVE DIGITS
       REM
 DE1   PXD     ,0
       DVP     $Q10               PUT ANOTHER DIGIT IN DEDIG
       ALS     36,4
       ORS     DEDIG
       STQ     DEMQ               IF QUOTIENT ZERO, CONVERSION
       NZT     DEMQ                 IS DONE
       TRA     DE2
       TIX     DE1,4,6
       REM
       CLA     DEDIG              STORE LO ORDER DIGITS
       STO     DELOD
       STZ     DETS2              SIGNAL THAT HI ORDER DIGITS EXIST
       TXI     DE4,4,30           RESTORE SHIFT INDEX AND LOOP AGAIN
       REM
 DE2   LDQ     DEINP              SEE IF MINUS SIGN NEEDED
       TQP     DEV
       TIX     DEQ,4,6
       REM
*      MINUS SIGN BEGINS A NEW WORD
       CLA     DEDIG              STORE LO ORDER DIGITS
       STO     DELOD
       STZ     DETS2              SIGNAL THAT HI ORDER DIGITS EXIST     PAGE 052
       STZ     DEDIG              CLEAR DIGITS REGISTER
       AXT     36,4               RESTORE SHIFT INDEX
 DEQ   CLA     DEMIN              INSERT MINUS SIGN
       ALS     36,4
       ORS     DEDIG
       REM
DEV    DCT
       TSX     $DCT,4             MACHINE ERROR
       ZET     DETS1              SEE WHICH EXIT TO USE
       TRA     DE5
       REM
*      DECON EXIT
       CAL     DEDIG              PICK UP DIGITS
       TXL     DEJ,4,6            TRANSFER IF FULL WORD OF DIGITS
       LGR     42,4               INSERT LEADING BLANKS
       CAL     BLANKS
       LGL     42,4
 DEJ   LDQ     DELOD              LO ORDER DIGITS OR ZERO -
       NZT     DETS2                SEE WHICH
       XCL                        LO ORDER DIGITS TO AC
       PBT                        SIGN AND P BIT MUST AGREE
       TRA     *+2
       SSM
 DEIR4 AXT     **,4               RESTORE IR4 AND EXIT
       TRA     1,4
       REM
*      NUMNAM EXIT
 DE5   CAL     DEDIG              INSERT TRAILING SEVENS INTO
       LDQ     SEVENS               DIGITS WORD
       LGR     42,4
       XCA
       TSX     $PRIN2,4           PRINT WORD OF DIGITS
       ZET     DETS2              SEE IF ANOTHER WORD MUST
       TRA     DEY                  BE PRITNER
       CAL     DELOD              PRINT LO ORDER DIGITS
       TSX     $PRIN2,4
       REM
 DEY   LXA     DEIR4,4            RESTORE IR4,  CLEAR AC, AND EXIT
       PXD     ,0
       TRA     1,4
       REM
       REM
       REM
 DEMIN SYN     $QO40               BCD MINUS SIGN
 DEORG BSS
       ORG     COMMON
 DETS1 BSS     1                  ZERO MEANS DECON EXIT
 DETS2 BSS     1                  ZERO MEANS HI ORDER DIGITS
 DELOD BSS     1                  LO ORDER DIGITS
 DEDIG BSS     1                  CURRENT DIGITS
 DEMQ  BSS     1                  MQ FOR ZERO TEST
 DEINP BSS     1                  INPUT NUMBER
       ORG     DEORG
       REM                                                              PAGE 053
*      THIS ROUTINE USES COMMON, SEVENS, $PRIN2, BLANKS, AND $Q10
*
       REM
R      HED
       REM      MAPLIS       NEW, FASTER VERSION WITH OPEN SAVE AND CONS
*
MAPLIS TZE     1,4           NULL(L) = NIL
       SXD     MS1,4         SAVE LINK IR
       LXD     $CPPI,4       GET PDL POINTER
       TXI     *+1,4,-6           SAVE TOTAL OF 6 ITEMS
       XEC     $ENDPDL            TEST FOR OUT OF PUSH DOWN LIST
       SXD     $CPPI,4       UPDATE PDL POINTER LOCATION
       STO     $ARG1         SAVE AC
       CLA     MS1           START SAVING    LINK IR
       STO     -6,4
       CLA     MS2           L ARGUMENT
       STO     -5,4
       CLA     MS3           FUNCTIONAL ARGUMENT
       STO     -4,4
       CLA     MS4           FINAL ANSWER
       STO     -3,4
       CLA     MS5           INTERMEDIATE ANSWER
       STO     -2,4
       CLA     MS6                SAVE MARKER
       STO     -1,4
       CLA     $ARG1         SAVING ALL DONE, RESTORE AC
       STO     MS2           PUT L ARGUMENT AWAY
       STQ     MS3           PUT FUNCTION ARGUMENT AWAY
       TQP     CMP           IF TRANSFER, F NOT A TXL, SO GO TO COMPAT
       TSX     MS3,4         EXECUTE FUNCTIONAL ARGUMENT
 MAIN  LXD     $FREE,4       START OPEN CONS
       TXH     *+2,4,0       TEST FOR OUT OF FREE STORAGE
       TSX     $FROUT,4      GO IF NO MORE FS
       ARS     18            PUT F(L) IN ADDRESS
       LDQ     0,4           GET NEXT REGISTER ON FSL
       SLQ     $FREE         UPDATE FREE
       STO     0,4           CONS(F(L),NIL)
       SXD     MS4,4         FINAL ANSWER
       SXD     MS5,4         INT. ANSWER
       LXA     $CNTR1,4           PICK UP CONS COUNTER
       TIX     *+3,4,1            DECREMENT BY 1
       TSX     ARREST,4           GO IF OUT OF COUNTER
       AXT     -1,4               RELOAD OF -1 FOR COUNTER
       SXA     $CNTR1,4           RESTORE CONS COUNTER
 MLOP1 LXD     MS2,4         MAUN LOOP,  GET L
       CLA     0,4           TAKE CDR(L)
       PDX     0,4
       TXH     MPRG1,4,0     IF NOT NULL GO ON TO MAIN PROGRAM
       CLA     MS4           ALL DONE, PICK UP FINAL ANSWER
       LXD     $CPPI,4       START OPEN UNSAVE BY GETTING PDL POINTER
       LDQ     -2,4
       STQ     MS5
       LDQ     -3,4
       STQ     MS4                                                      PAGE 054
       LDQ     -4,4
       STQ     MS3
       LDQ     -5,4
       STQ     MS2
       LDQ     -6,4
       STQ     MS1
       TXI     *+1,4,6            RESTORE PDL COUNTER
       SXD     $CPPI,4       SET CPPI
       LXD     MS1,4         PICK UP LINK IR
       TRA     1,4           RETURN
*
 MPRG1 PXD     0,4           MAIN PROGRAM  PUT L IN AC
       STO     MS2           SAVE IN L ARGUMENT REGISTER
       LXD     MS3,4         SEE IF FUNCTIONAL ARG IS S EXPRESSION
       TXH     CMP1,4,10     GO IF S EXPRESSION
       TSX     MS3,4         EXECUTE FUNCTION ARGUMENT (TXL INS.)
 MAIN1 LXD     $FREE,4       START OPEN CONS
       TXH     *+2,4,0       TEST FOR OUT OF FREE STORAGE
       TSX     $FROUT,4      GO IF OUT
       LDQ     0,4           PICK UP POINTER TO NEXT FREE REGISTER
       SLQ     $FREE         UPDATE FREE
       ARS     18            ITEM TO ADDRESS
       STO     0,4           CONS(F(L),NIL)
       PXD     0,4           ANSWER TO AC
       LXA     $CNTR1,4           PICK UP CONS COUNTER
       TIX     *+3,4,1            DECREMENT BY 1
       TSX     ARREST,4           GO IF OUT OF COUNTER
       AXT     -1,4               RELOAD OF -1 FOR COUNTER
       SXA     $CNTR1,4           RESTORE CONS COUNTER
       LXD     MS5,4         PICK UP LAST ANSWER
       STD     0,4           CONCATENATE THE ANSWERS BY RPLACD
       STO     MS5           UPDATE INT. ANSWER
       TRA     MLOP1         GO TO HEAD OF MAIN LOOP
*
 CMP   SLQ     *+2           COMPAT CALL FOR S EXPRESSION FUN. ARG.
       TSX     COMPAT,4
               1,,**         FUNCTION OF 1 ARGUMENT
       TRA     MAIN          GO BACK TO MAIN PROGRAM
*
 CMP1  SXD     *+2,4         ANOTHER COMPAT CALL
       TSX     COMPAT,4
               1,,**
       TRA     MAIN1         RETURN TO MAIN PROGRAM
*
 MS6   TXL     $END5,,MS5+2       SAVE 5 ITEMS
       REM
       REM FUNCTION COPY
       REM COPY(L)= (L=0 YIELDS 0, CAR(L)=-1 YIELDS L,
       REM    OTHERWISE CONS(COPY(CAR(L)),COPY(CDR(L))))
R      HED
 COPY  TZE 1,4               L=0
       SXD CS1,4
       PDX 0,4               L
       SXD CT1,4             L                                          PAGE 055
       CLA 0,4               CWR(L)
       PAX 0,4               CAR(L)
       TXL C1,4,-2           CAR(L)=-1
       CLA CT1
       LXD CS1,4
       TRA 1,4
 C1    TSX $SAVE,4
       TXL     $END2,,CS2+2  SAVE 2 ITEMS
       LXD CT1,4             L
       CLA 0,4               CWR(L)
       STO CS2
       ANA DECM              CDR(L)
       TSX COPY,4            COPY(CDR(L))
       LXA CS2,4             CAR(L)
       STO CS2               COPY(CDR(L))
       PXD 0,4
       TSX COPY,4            COPY(CAR(L))
       LDQ CS2
       TSX $CONS,4
       TSX UNSAVE,4
       LXD CS1,4
       TRA 1,4
 CT1
 DECM  SYN     $DMASK
       REM
       REM FUNCTION SEARCH
       REM SEARCH(L,P,F,U)=(L=0 YIELDS U,P(L) YIELDS F(L),
       REM     OTHERWISE SEARCH (CDR(L),P,F,U))
       REM
R      HED
SEARCH SXD     SRS1,4
       TSX $SAVE,4
       TXL     $END5,,SRS5+2 SAVE 5 ITEMS
       STQ SRS3              P
 SR3   TZE     SR4
       STO SRS2              L
       LDQ $ARG3             F
       STQ SRS4
       LDQ $ARG4             U
       STQ SRS5
       LXD SRS3,4
       TXH *+3,4,10
       TSX SRS3,4
       TRA *+4
       SXD *+2,4
       TSX COMPAT,4
           1,,**
       TZE SR1               NOT P(L)
       CLA SRS2              L
       LXD SRS4,4
       TXH *+3,4,10
       TSX SRS4,4
       TRA *+4
       SXD *+2,4                                                        PAGE 056
       TSX COMPAT,4
           1,,**
       TSX UNSAVE,4
       LXD SRS1,4
       TRA 1,4
 SR1   CLA SRS5              I YIELDS
       STO $ARG4             U
       CLA SRS4
       STO $ARG3             F
       LXD SRS2,4            L
       CLA 0,4
       ANA DECM              CDR(L)
       TRA     SR3
 SR4   TSX     UNSAVE,4
       LXD $ARG4,4
       TXH     SRCMPT,4,10
       LXD     SRS1,4
       TRA     $ARG4
*
SRCMPT STZ     $ARG3
       LDQ     $ARG3
       TSX     $CONS,4
       XCA
       CLA     $ARG4
       LXD     SRS1,4
       TRA     $APPLY
       REM
       REM FUNCTION EQUAL
       REM EQUAL(L1,L2)=(L1=L2 YIELDS1,L1=OVL2=0 YIELDS 0,
       REM CAR(L1)=-1VCAR(L2)=-1 YIELDS 0, OTHERWISE
       REM     EQUAL(CAR(L1,(CARL2))AEQUAL(CDR(L1),CDR(L2)))
       REM
L      HED
* EQUAL        A FUNCTION OF 2 ARGUMENTS DETERMINES WETHER 2 LIST
*              STRUCTURES ARE EQUIVELENT. REPROGRAMMED 5 OCTOBER 1960
*              TO MAKE USE OF THE NUMBER CONVENTIONS CURRENTLY IN USE.
*
 EQUAL SXD     EQXR,4             SAVE LINK IR
       STQ     EQL2               SAVE ARGUMENT 2
       STO     EQL1               SAVE ARGUMENT 1
 EQLP  SUB     EQL2               EQ TEST
       TZE     EQT                TWO LIST ARE EQ. EXIT TRUE
       NZT     EQL1               SKIP IF L1 NON NULL
       TRA     EQF                L1 NULL BUT NOT EQ L2, EXIT FALSE
       NZT     EQL2               NULL TEST L2
       TRA     EQF                L2 NULL BUT NOT EQ  L1, EXIT FALSE
       LXD     EQL2,4             PICK UP LIST 2
       CLA     0,4                GET NEXT ELEMENT
       STD     EQL2               SAVE CDR OF LIST 2
       PAX     0,4                CAR OF LIST 2
       TXH     EQA,4,-2           GO IF ATOM
       PXD     0,4                CAR OF LIST TO DECREMENT OF AC
       XCA                        SWITCH TO MQ
       LXD     EQL1,4             PICK UP LIST 1                        PAGE 057
       CLA     0,4                GET NEXT ELEMENT
       STD     EQL1               SAVE CDR OF LIST 1
       PAX     0,4                CAR OF LIST TO IR 4
       TXH     EQF,4,-2           GO TO FALSE EXIT IF THIS IS AN ATOM
       PXD     0,4                CAR OF LIST TO DECREMENT OF AC
       TSX     $SAVE,4            SAVE CALL
       TXL     $END3,,EQL2+2      SAVE 3 ITEMS
       TSX     $EQUAL,4           TEST FOR EQUALITY IN CAR  DIRECTION
       TSX     UNSAVE,4           UNSAVE CALL
       TZE     EQF                WHOLE LIST IS FALSE IF CAR DIRECTION F
       CLA     EQL1               PICK UP REST OF LIST 1
       TRA     EQLP               TEST EQUALITY IN CDR DIRECTION
*
EQT    CLA     $QD1               TRUE EXIT, PICK UP 1 IN DECREMENT
       LXD     EQXR,4             RESTORE LINK IR
       TRA     1,4
*
 EQF   PXD     0,0                FALSE EXIT, CLEAR AC
       LXD     EQXR,4             RESTORE LINK IR
       TRA     1,4
*
EQA    LDQ     EQL1
       AXC     EQAR,4
       SXA     EQPX,4
EQAR   TRA     EQPE
       TZE     EQF
       TRA     EQT
*
*      EQP TESTS FOR EQ BETWEEN LISTS AND NUMERICAL EQUALITY BETWEEN
*      NUMBERS.   USES A TOLERENCE  IN TESTIONG FLOATION PT NUMBERS
*
EQP    TLQ     EQPF
       XCA
       TLQ     EQPF
EQPTX  CLA     $QD1
       TRA     1,4
EQPF   SXA     EQPX,4
       PDX     0,4
       CLA     0,4
EQPE   PDX     0,4
       ANA     TAGMSK
       TZE     EQPFX
       STO     EQPT
       CLA     0,4
       XCA
       PDX     0,4
       CLA     0,4
       PDX     0,4
       ANA     TAGMSK
       ANA     EQPT
       TZE     EQPFX
       ANA     $QT1
       STO     EQPT
       CLA     0,4                                                      PAGE 058
       STO     EQPS
       XCA
       SUB     EQPS
       LXA     EQPX,4
       TZE     EQPTX
       ZET     EQPT
       TRA     EQPFX
       SSP
       SUB     FLOTOL
       TMI     EQPTX
EQPFX  PXD     0,0
EQPX   AXT     **,4
       TRA     1,4
EQPT                              TEST CELL NON 0 YIELDS FIX
EQPS                              STORAGE
 EQXR          $F8                INDEX REGISTER STORAGE
 EQL1                             LIST 1 STORAGE
 EQL2                             LIST 2 STORAGE
 EQTS                             TEST CELL  0 FIX, NON 0 FLO
*
*  EQUAL USES $SAVE,$QD1,UNSAVE,$EQUAL AND FIXFLO
       REM
       REM PRINT                  MAY 14,1959
       REM
       REM
       REM PRINT(L)=(CAR(L)=-1 YIELDS PRIN1(L),1 YIELDS                 
       REM (PRIN2(LPAR2),PRINT(CAR(L)),(CDR(L)=0YIELDS
       REM PRIN2(RPAR2),1 YIELDS(PRIN2(COMMA2),PRINT
       REM (CDR(L))))))
       REM
       REM THE LIST L IS PRINTED IN THE RESTRICTED NOTATION
       REM
       REM PRINT REQUIRES THE SUBROUTINES PRIN1,PRIN2,
       REM TERPRI,MISPH2(OR UASPH2) ALL HEADED BY P
       REM AND SAVE,UNSAVE,ERROR UNHEADED
       REM
T      HED
       REM
       REM PRINT MASTERMINDER
       REM
PRINT  SXA     PRPS1,4            SAVE LINK IR
       LXD     $CPPI,4            SAVE CURRENT CONTENTS OF CPPI
       SXD     PCPPI,4
       STZ     WALLPC             ZERO WALL PAPER COUNTER
       STO     PRINTL         SAVE THE ARGUMENT
       TSX PRIN0,4
 PRTT1 TSX TERPRI,4
       CLA     PRINTL         RESTORE THE ARGUMENT
 PRPS1 AXT     **,4               RESTORE LINK IR
       TRA     1,4
 PRNIL CLA     PRBLW              PICK UP NIL REPRESENTATION
       TRA     $PRIN2         PUT IN PRINT LINE AND EXIT
 PRIN0 SXD PS1,4
       TZE     PRNIL              PRINT THE NULL LIST                   PAGE 059
       PDX 0,4
       SXD L1,4
       CLA 0,4
       STO CWRL
       PAX 0,4
       TXL     XA1,4,-2
       CLA L1
       LXD PS1,4
       TRA $PRIN1
XA1    CLA     LPAR2
       TSX $PRIN2,4
       CLA CWRL
       TSX $SAVE,4
       TXL     $END2,,PS2+2       SAVE 2 ITEMS
 A3    STO     PS2                SAVE LIST
       PAX     0,4                CAR TO IR 4
       TXL PRP2,4,0
       PXD 0,4
       TSX PRIN0,4
 A4    LXD PS2,4
       TXL     A6,5,0             EXIT IF NULL
       CLA     0,4                TEST FOR ATOM
       PAX     0,4
       TXL     A2,4,-2            GO TO A2 IF NOT AN ATOM
       CLA     DOT                OTHERWISE PRINT IN DOT NOTATION
       TSX     $PRIN2,4           PUT IN PRINT LINE
       CLA     PS2                CDR OF LIST
       TSX     $PRIN1,4           PRINT AS ATOM
 A6    TSX     UNSAVE,4
       CLA RPAR2
       LXD PS1,4
       TRA $PRIN2
 A2    CLA COMM2
       TSX $PRIN2,4
       LXD PS2,4
       CLA 0,4
       TRA A3
       REM
 PRP2  CLA PRBLW
       TSX $PRIN2,4
       TRA A4
 DOT   OCT     603360777777       .
 PRBLW OCT     453143777777       NIL
 PS1           $F4
 PS2
 RPAR2 OCT 347777777777
 LPAR2 OCT 747777777777
 COMM2 OCT 607777777777           BLANK    INSTEAD OF A COMMA
 CWRL
 L1
       REM
       REM
T      HED
       REM
       REM
       REM     SUBROUTINE(PRIN1(L))                                     PAGE 060
**     /       CAR(L)   N=-1 YIELDS ERROR
       REM     ST = L
**     A1      CDR(L) = 0 YIELDS ERROR
       REM     L = CDR(L)
       REM     CAR(L) = PNAME YIELDS GO(A3)
       REM     CAR(L) N= FLOAT YIELDS GO(A1)
       REM     L = CAR(CDR(L))
       REM     VAL = FLONAM(L)
       REM     REPLACD(CONS(PNAME,CONS(VAL,CDR(ST))),ST)
       REM     L = CDR(ST)
**     A3      L= CAR(CDR(L))
**     A2      PRIN2(CWR(CAR(L))
       REM     L = CDR(L)
       REM                      L=0 YIELDS RETURN
**     */      GO(A2)
       REM
 PRIN1 SXD PR1,4
       STO PRSS                   SAVE OBJECT
       PDX ,4
       CLA ,4
       STT     PTTGR
       ANA ADDM
       SUB ADDM
       TZE PR3                    CAR(L) N=-1 YIELDS ERROR
 PR2   SXD $ERROR,4
       TSX TERPRI,4
       PXD     0,0
       TSX $ERROR+1,4
       BCI     1,*P  1*           TRIED TO PRINT NON-OBJECT -PRIN1-
 ADDM  SYN     $AMASK
PR3    ZET     PTTGR
       TRA     PR3N
       CLA     0,4                FIRST WORD OF ATOM
       TRA     *+3
PR3P   TXL     *+2,4,$PNAME-1
       TXL     PA3,4,$PNAME
       PDX     0,4                CDR
       TXL     PR5,4,0            UNPRINTABLE
       CLA     0,4                NEXT WORD
       PAX     0,4
       TRA     PR3P               EXAMINE WORD
PR3N   LXD     PRSS,4
       CLA     0,4
       PDX     0,4
       SXA     PTPNT,4
       CLA     PTTGR
       ANA     $QT2
       TNZ     PR4F
       CLA     PTTGR
       ANA     $QT4
       TNZ     LUCY
       PXD     0,4                                                      PAGE 061
       TSX     NUMNAM,4
       TRA     PR4E
*
 PA3   PDX     0,4                FOUND A PNAME
       CLA     0,4
       PAX     0,4                POINTER TO PRINT LIST
 PR4   CLA     0,4                POINTRE TO PRINT LIST
       STD     L                  SAVE REST OF LIST IF ANY
       PAX     0,4                POINTER TO FIRST FULL FULL WORD
       CLA     0,4                FULL WORD
       TSX     $PRIN2,4           PRINT IT
       LXD     L,4                PICK UP REST OF LIST
       TXH     PR4,4,0            PRINT MORE IF MORE
 PR4E  LXD     PR1,4              EXIT BY RESTORING LINK IR
       TRA     1,4                EXIT
PR4F   PXD     0,4
       TSX     FLONAM,4
       TRA     PR4E
*
*      PRINT THE NUMBER OCTALLY
 LUCY  LXA     PTPNT,2            GET POINTER TO NUMBER
       LDQ     0,2
       TQP     BETTY              TEST FOR NEGATIVE NUMBER
       CLA     MISGN              IF SO, PRINT -
       TSX     $PRIN2,4
       CLA     0,2                REMOVE MINUS SIGN
       XCL
 BETTY NZT     0,2                TEST IF NUMBER ALL ZEROS
       TRA     MARIE
*      LOOK FOR NON-ZERO DIGIT ON LEFT
       PXD     ,0
       AXT     12,2               IR2 COUNTS ZEROS ON RIGHT
       LGL     3
       TXI     *+1,2,-1           COUNT VACATED POSITIONS
       TZE     *-2
*      A NON-ZERO DIGIT HAS APPEARED ON THE LEFT
       ORA     $Q64               PUT IN OVERFLOW FLIPPER
       TOV     *+1                SHUT OFF OVERFLOW LIGHT
 GRETA STQ     TONI               TEST IF ALL DIGITS ARE SPREAD
       TQP     *+2                TEST FOR NON-ZERO SIGN BIT
       TXI     FIFI,2,-1          SOME DIGITS NOT SPREAD, SO CONTINUE
       NZT     TONI
       TRA     DEBBY              TRA IF ALL NON-ZERO DIGITS SPREAD
       TXI     *+1,2,-1
 FIFI  ALS     3                  SPREAD ONE DIGIT
       LGL     3
       TNO     GRETA              SEE IF FULL WORD OF DIGITS
       STQ     TONI               PRIT THE WORD
       TSX     $PRIN2,4
       CLA     $Q1                PUT IN OVERFLOW FILPPER
       LDQ     TONI
       TOV     *+1                SHUT OFF OVERFLOW LIGHT
       TQP     *+2                TEST FOR NON-ZERO SIGN BIT
       TXI     FIFI,2,-1                                                PAGE 062
       ZET     TONI               SEE IF ALL DIGIS SPREAD
       TXI     FIFI,2,-1
       TRA     VICKI
*      FORM WORD FOR PRINTING
 DEBBY LDQ     SEVENS             PUT 77S IN RIGHT END OF WORD
       LGL     6                    OVERFLOW SIGNALS LEFT END OF WORD
       TNO     *-1
       TSX     $PRIN2,4
*      PRINT Q AND SCALE FACTOR IF ANY
 VICKI TXH     MICKY,2,0          CONTINUE IF 0 SCALE FACTOR
       CLA     BCIQ
       TRA     PATSY
 MICKY TXL     SANDY,2,9          TRA IF SCALE FACTOR LESS THAN 10
*      OCTAL SCALE FACTOR MORE THAN 10
       PXD     ,2
       ADD     BQ10               FORM SCALE FACTOR FOR PRINTING
       SSM
       TRA     PATSY
*      OCTAL SCALE FACTOR LESS THAN 10
 SANDY PXD     ,2
       ALS     6
       ADD     BQ0
       SSM
       TRA     PATSY
 MARIE CLA     BCI0Q              PRINT Q0
 PATSY TSX     $PRIN2,4
       TRA     PR4E
       REM
       REM GENERATE A PRINT NAME FOR AN OBJECT WITHOUT ONE.
       REM
       REM THE PRINT NAME IS OF THE FORM LDDDDD WHERE THE D,S ARE THE
       REM OCTAL DIGITS OF THE 2,S COMPLMENT OF THE FIRST WORD OF
       REM  THE PROPERTY LIST OF THE OBJECT.
       REM
PR5    LDC     PRSS,4
       PXD     0,4
       XCA
       TSX     OCTALP,4
       ORA     PRC1
       SSM                   FIX SIGN TO AGREE WITH P BIT FOR PRIN2
       PBT
       CHS
       LXD     PR1,4              RESTORE LINK IR
       TRA     $PRIN2             PUT IN PRINT LINE AND EXIT
       REM
 PRC1  BCI     1,L0000            L SYMBOL
PRSS                              STORAGE FOR POINTER TO OBJECT
 PR1
 L
 TONI  BSS     1
 BQ10  OCT     100066777777       USED TO FORM BCI Q1N
 BQ0   OCT     100077777777       USED TO FORM BCI QN
 BCI0Q OCT     005077777777       BCI 0Q
 MISGN OCT     407777777777       BCI -                                 PAGE 063
 BCIQ  OCT     507777777777
 PTPNT BSS     1
PTTGR                             TEST CELL FOR NUMBER FLAGS
       REM
       REM
       REM PRIN2    PRINTS UP TO 6 CHARACTERS IN ONE WORD WHEN THE
       REM CHARACTERS ARE JUSTIFIED TO THE LEFT AND FOLLOWED BY THE
       REM ILLEGAL CHARACTER WHOSE OCTAL FORM IS 77
       REM
       REM
PRINT2 SXD PR9,4
       PDX 0,4                    BRING BCD WORD TO AC
       CLA 0,4
       TRA     *+3
 PRIN2 TXH     $PUN2,,0           SWITCH TO PUNCH OUT ROUTINE
       SXD     PR9,4
       SXD PR8,2
       SXD PR7,1
       LXD WORDS,4                ROOM LEFT IN OUTPUT RECORD
       TXL INIT,4,0               CAN BE ZERO ONLY IF ROUTINE NOTUSED
 COMB4 AXT 1,1
       STO TEMP
       CAL TEMP
       LAS SEVENS                 WORD OF ALL 77-S CAUSES NO ACTION
       TRA *+2
       TRA NOJOB
 SHIFL ANA RCHM    IS THE RIGHT CHARACTER 77
       SUB RCHM
       TNZ JUST                   NOT 77
       CAL TEMP
       ARS 6
       SLW TEMP
       TXI SHIFL,1,1
 JUST  CAL TEMP
       TRA LSHIF+1,1
       ALS 6
       ALS 6
       ALS 6
       ALS 6
       ALS 6
 LSHIF SLW TEMP
       LDQ TEMP
       CAL PART
       LXD PARTS,2
 COMB  LGL 6
       SLW PART
       TNX WFULL,2,1
 COMB5 TXI *+1,1,1
       TXL COMb,1,6
 COMB1 SXD PARTS,2
       SXD WORDS,4
 NOJOB LXD PR7,1
       LXD PR8,2
       LXD PR9,4                                                        PAGE 064
       PXD 0,0
       TRA 1,4
 WFULL SLW REC,4
       TNX RECFL,4,1
 COMB3 AXT 6,2
       TRA COMB5                  /
 RECFL STQ TEMP
       CLA     WALLPC             GET MAX NUMBER OF LINES PER LIST
       ADD     $Q1
       CAS     BRKOUT             COMPARE WITH MAX NUMBER
       TRA     *+2                NO, GO ON
       TRA     PRTB               = BREAKOUT
       STO     WALLPC             PUT AWAY
       TSX OUTPUT,4
PRINTD     BCDOUT
           REC-20,,20
       LDQ TEMP
       LXD QD20,4
       CAL BLNKA
       SLW PART
       LXD QD20,4
       LXD QD5,2
       TRA COMB5
 PRTB  LXD     PCPPI,4            PUSH DOWN COUNTER
       SXD     $CPPI,4            RESTORE TO ENTRACE VALUE
       LXD     PR7,1              RESTORE INDEX 1 AND 2
       LXD     PR8,2
       TRA     PRTT1              BREAKOUT
TERPRI SXD PR8,2
       SXD PR9,4
       LXD PARTS,2
       LXD WORDS,4
       CAL PART
       LDQ BLANK
 TER1  LGL 6
       TIX TER1,2,1
 TER3  SLW REC,4
       TNX TER2,4,1
       CAL BLANK
       TRA TER3
 TER2  TSX OUTPUT,4
PRINTC     BCDOUT
           REC-20,,20
       LXD QD20,4
       SXD WORDS,4
       LXD QD5,2
       SXD PARTS,2
       LXD PR8,2
       LXD PR9,4
       CLA BLNKA
       STO PART
       PXD 0,0
       TRA 1,4
 INIT  LXD QD20,4                                                       PAGE 065
       LDQ BLNKA
       STQ PART
       AXT 5,2
       SXD PARTS,2
       TRA COMB4
*
 PR7
 PR8
 PR9
 WORDS
 PARTS                            ROOM IN PARTIAL WORD
 RCHM  OCT 77
 PART
 TEMP
 REC   BES 20
 PCPPI                            PUSHDOWN COUNTER STORAGE
WALLPC                            NUMBER OF LINES IN THIS LIST SO FAR
BRKOUT DEC     25                 MAXIMUM NUMBER OF LINES IN ANY LIST
 QD5   SYN     $QD5
 QD20  SYN     $QD20
 BLANK SYN     BLANKS
 BLNKA SYN     BLANKS
*
* BCDAD1       A CONVERT TABLE FOR ADDING 1 TO A 6 DIGIT BCD NUMBER
*              USED BY LOADING BCD NUMBER INTO AC AND DOING
*      CVR     BCDAD1,,6
*
 ADT   PZE     ADT                0
BCDAD1 PZE     ADT,,1*4096        1
       PZE     ADT,,2*4096        
       PZE     ADT,,3*4096        3
       PZE     ADT,,4*4096        4
       PZE     ADT,,5*4096        5
       PZE     ADT,,6*4096        6
       PZE     ADT,,7*4096        7
       PON     ADT                8
       PON     ADT,,1*4096        9
       PZE     BCDAD1             10
*
* PUNCH        WRITES OUT A LIST ON TH SYSTEM PERFIAL PUNCH TAPE
*              (SYSPPT) IN A FORM SUTABLE FOR PUNCHING IN BCD.
*
 PUNCH SXA     PNCHX,5            SAVE LINK IR
       STL     PUNACT             ACTVTE PUNCH ROUTINE
       PXD     0,4                ARGUMENT TO IR 4
       CLS     $PRIN2             SE SWITCH TO
       STO     $PRIN2             GO TO PUNCH ROUTINE
       PXD     0,4                ARGUMENT TO AC
       STO     PRINTL         SAVE THE ARGUMENT
       TSX     $PRIN0,4           USES PRINT ROUTINE
       TSX     TERPUN,4           TERMINATE PUNCHING
       CLA     PRINTL         RESTORE THE ARGUMENT
 PNCHX AXT     **,4               RESTORE LINK IR
       TRA     1,4                EXIT                                  PAGE 066
*
* PUN2         PUNCH EQUIVELENT OF PRIN 2
*
 PUN2  SXA     PNX,4              SAVE INDEX REGISTERS
       SXA     PNY,2
       SXA     PNZ,1
 PWRDS AXT     12,4               NUMBER OF WORDS LEFT IN BUFFER
 PPRTS AXT     6,2                CHARACTER POSITION
       AXT     6,1                MAXIMUM NUMBER OF CHARACTERS
       XCA                        ARGUMENT TO MQ
 PLP   PXD     0,0                CLEAR AC
       LGL     6                  CHARACTER TO MQ
       CAS     PSS                COMPARE WITH 77
       NOP                        GREATER, (IMPOSSIBLE)
       TRA     POUT               = , GO TO EXIT
       XEC     PCNT,2             LESS THAN, SHIFT CHARACTER
       ORS     POUP,4             PUT IN OUTPUT LINE
       TNX     PRPLP,2,1          GO IF LAST CHARACTER IN WORD
 PGRA  TIX     PLP,1,1            GET NEXT CHARACTER
 POUT  SXA     PPRTS,2            SAVE INDEX 2 N 4
       SXA     PWRDS,4
       PXD     0,0                CLEAR AC
 PNX   AXT     **,4               RESTORE INDEX REGISTERS
 PNY   AXT     **,2
 PNZ   AXT     **,1
       TRA     1,4                EXIT
*
 PRPLP AXT     6,2                RELOAD CHARACTER COUNT
       TIX     PGRA,4,1           GO IF WORD COUNT NOT EXAUSTED
       CLA     PCNT               GET CARD NUMBER IN BCD
       CVR     BCDAD1,,6          ADD 1 IN BCD
       STO     PCNT
       STQ     PNCQ               SAVE CONTENTS OF MQ
       LDQ     $ZERO              ZERO MQ
       LGR     6                  SHIFT LOW ORDER DIGITS
       ACL     PLIS               ADD BCD NAME OF CARD
       SLW     POUP               PUT IN ID FIELD
       STQ     POUP+1
       TSX     OUTPUT,4           GO TO OUTPUT
               PPTOUT             PUNCH OUT TAPE
               POUP-12,,14        14 WORDS OUT
       PIA                        SAVE INDICATORS IN AC
       LDI     SYSIND             PICK UP SYSTEM INDICATORS
       STR     PPTIND             SET PUNCH TAPE INDICATOR
       STI     SYSIND             UPDATE SYSTEM INDICATORS
       PAI                        RESTORE INDICATORS
       AXT     12,4               NUMBER OF WORDS FROM CC 1 TO 72
       STZ     POUP,4             ZERO OUTPUT BUFFER
       TIX     *-1,4,1
       AXT     12,4               RELOAD WORD COUNT
       LDQ     PNCQ               RESTORE CONTENTS OF MQ
       TRA     PGRA               CONTINUE WORK
*
* TERPUN       FILLS OUT BUFFER WITH BLANKS AND PUNCHES OUT LAST CARD   PAGE 067
*              OPERATES ONLY IF PUNCH ROUTINE IS CURRENTLY ACTIVE
*
TERPUN NZT     PUNACT             SKIP IF PUNCH ROUTINE IS CURRENTLY ACT
       TRA     1,4                IMMEDIATE EXIT
       STZ     PUNACT             DE ACTIVATE THE PUNCH ROUTINE
       SXA     PNX,4              SAVE INDEX REGISTERS
       SXA     PNY,2
       SXA     PNZ,1
       CLA     $PRIN2
       SLW     $PRIN2             RESTORE PRIN2 SWITCH
       LXA     PWRDS,4            PICK UP WORD COUNT
       LXA     PPRTS,2            CHARACTER COUNT
       AXT     1,1                CONSTANT 1
       LDQ     BLANKS             BLANK MQ
 TPLP  PXD     0,0                CLEAR AC
       LGL     6                  1 INTO AC
       XEC     PCNT,2             SHIFT INTO POSITIN
       ORS     POUP,4             PUT IN OUTPUT LINE
       TIX     TPLP,2,1           FILL OUT THIS WORD
       TNX     PRPLP,4,1          GO IF LAST WORD IN BUFFER
       CLA     BLANKS             BLANK AC
       STO     POUP,4             BLANK REST OF BUFFER
       TIX     *-1,4,1
       TRA     PRPLP              GO PUNCH IT OUT
*               COSTANTS, STORAGE AND SHIFT TABLE
       ALS     20
       ALS     24
       ALS     18
       ALS     12
       ALS     6
       NOP
 PCNT  PZE                        BASE OF SHIFT TABLE AND CARD COUNT
 PLIS  BCI     1,LISP00           CARD ID
 PSS   OCT     77                 CHARACTER THAT TERMINATES A PNAME
 PNCQ
PUNACT                            NON-ZERO IF PUNCH ROUTINE ACTIVE
       BSS     12
 POUP  OCT     0,0                OUTPUT BUFFER
       REM
       REM FLONAM                 MAY 14,1559
       REM     FORMS THE BCD LIST  FOR A FLOATING NUMBER IN THE ACC
       REM
T      HED
FLONAM SXA     FLNX,4
       PDX     0,4
       CLA     0,4
       TNZ     FLNA
       LXA     FLNX,4
       XCA
       CLA     FLZPZ              0.0
       TQP     $PRIN2
       SUB     C0                 -0,0
       TRA     $PRIN2                                                   PAGE 068
 FLNA  SXA     FLNY,2
       SXA     FLNZ,1
       AXT     1,1                SET UP BUFFER IRS
       AXT     36,2
       STZ     FLOPB-3
       STZ     FLOPB-2
       STZ     FLOPB-1
       STO     COMMON+5
       XCA
       PXD     ,0                 CLEAR ACC. AND SIGN.
 FL73  LRS     0                  SIGN TO MQ
       LLS     8                  CHARACTERSITIC.
       SUB     A128               128
       STQ     COMMON             SAVE MANTISSA.
       XCA                        MULTIPLY BY
       MPY     LOG2               LOG BASE 10 OF 2.
       STO     COMMON+2
       TPL     FL75
 FL74  SUB     A1                 1
       STO     COMMON+2
       XCA
       COM
       SSP
       XCA
 FL75  MPY     LOG10              LOG BASE 2 OF 10/4.
       LRS     33
       STA     FL76A
       STQ     COMMON+8
       AXT     7,4
       LDQ     C7
 FL76  MPY     COMMON+8
       ADD     C0+1,4
       XCA
       TIX     FL74,4,1
       MPY     COMMON             MANTISSA.
 FL76A AXT     **,4
       LRS     34,4
       TZE     FL77
       DVP     A1-1               10.
       CLA     COMMON+2
       ADD     A1                 1.
       STO     COMMON+2
 FL77  AXT     8,4
 FL78  MPR     A1,4               10 TO DEC. PLACES.
       CAS     A1,4
       NOP                        GREATER.
       TRA     FL79               EQUAL.
       TRA     FL80               LESS.
 FL79  CLA     A1                 ROUDING CAUSED CARRY.
       ADD     COMMON+2
       STO     COMMON+2           EXP+1.
       CLA     A1+1,4             10 TO THE DEC. PL.-1.
 FL80  STO     COMMON+2
       PXD     ,0                                                       PAGE 069
       LDQ     COMMON+2           ENTER DEC EXP.
       DVP     A1-1               10
       STQ     COMMON+7
       SXA     FL82,4
       TSX     INBCD,4
       PXD     ,0
       LDQ     COMMON+7
       DVP     A1-1
       TZE     *+2
       TSX     INBCD,4
       CLA     COMMON+2
       TZE     FL81
       TPL     FL81
       CLA     ONEMI              MINUS SIGN
       TSX     INBCD,4
 FL81  CLA     ONEE
       TSX     INBCD,4
 FL82  AXT     **,4
       STZ     FLZET
 FL65  CLA     COMMON+8
 FL67  LRS     35
       DVP     A1-1               10.
       STQ     COMMON+8           FRACTIONAL PART.
       NZT     FLZET
       TZE     FL01
       ORS     FLZET
       SXA     *+2,4              SAVE IR4.
       TSX     INBCD,4            ENTER DIGIT.
       AXT     **,4               RESTORE.
 FL01  TIX     FL65,4,1
       PXD     0,0
       NZT     FLZET
       TSX     INBCD,4
       CLA     A33                DEC. POINT.
       TSX     INBCD,4            ENTER.
       PXD     0,0
       TSX     INBCD,4
       LDQ     COMMON+5
       CAL     ONEBL              BLANK
       TQP     FL70               FOR PLUS.
       CAL     ONEMI              NEGATIVE.
 FL70  TSX     INBCD,4            INSERT BLANK OR MINUS.
       PXD     0,2
       PDC     0,2
       LDQ     ONES                FILL OUT LAST WORD WITH 77S
       CAL     FLOPB,1
       LGL     0,2
       XCL
       XCA
       TSX     $PRIN2,4
       TNX     FLNX,1,1
       CLA     FLOPB,1
       TSX     $PRIN2,4
       TIX     *-2,1,1                                                  PAGE 070
 FLNX  AXT     **,4
 FLNY  AXT     **,2
 FLNZ  AXT     **,1
       PXD     0,0
       TRA     1,4
*
 INBCD ANA     A77
       ALS     36,2
       ORS     FLOPB,1
       TIX     *+3,2,6
       TXI     *+1,1,1
       AXT     36,2
       TRA     1,4
*
 FLZET
 FLOPB BES     3
FLZPZ  VFD     H24/ 0.0,012/7777
       DEC     100000000
       DEC     10000000
       DEC     1000000
       DEC     100000
       DEC     10000
 THSND DEC     1000
       DEC     100
       DEC     10
 A1    DEC     1
 LOG2  OCT     115040465025       LOG BASE 10 OF 2.
 LOG10 OCT     324464741127       LOG BASE 2 OF 10-4.
 C7    OCT     1601225
 C6    OCT     7762774
 C5    OCT     132240566
 C4    OCT     1164125106
 C3    OCT     7066267024
 C2    OCT     36577252307
 C1    OCT     130562064437
 C0    TIX     0,0,0
 A33   SYN     $QO33
 A77   SYN     $Q63
 A128  SYN     $Q128
 ONEPL SYN     $QO20
 ONEE  SYN     $QO25
 ONEMI SYN     $QO40
 ONEBL SYN     $QO60
 ONES  SYN     SEVENS
*
       REM READ
       REM
       REM   READ = SELECT(RD.,LPAR,READ1.,
       REM                     LITER,INTERN.,
       REM                     NUM,INTERN.,
       REM                     RPAR,ERROR.,
       REM                     1,ERROR)
       REM
       REM                                                              PAGE 071
       REM READ1
       REM
       REM READ1 = SELECT(RD.,RPAR,0.,
       REM                    LPAR,CONS(READ1,READ1).,
       REM                    LITER,CONS(INTERN,READ1).,
       REM                    NUMB,CONS(INTERN,READ1))
       REM
I      HED
 READ  SXA     REDS1,4            SAVE LINK IR
       TSX     $RD,4              GET FIRST ITEM
 REDS1 AXT     **,4               RSTORE LINK IR
 REDIS CAS     RLPAR              DISPATCH ON TYPE OF ITEM READ
       TRA     *+2
       TRA     READ1              WAS (
       CAS     RRPAR
       TRA     *+2
       TRA     REDER
       CAS     RDOT
       TRA     1,4
       TRA     REDER
       TRA     1,4
 REDER SXD     $ERROR,4           MUST BE AN ERROR
       STO     RS2                SAVE TYPE
       TSX     OUTPUT,4           WRITE OUT INPUT BUFFER
               BCDOUT
               CELL-15,,14
       CLA     RS2                GET TYPE
       TSX     $ERROR+1,4         GOT O ERROR
       BCI     1,*8  1*           CONTEXT ERROR
*
 READ1 SXD     RS1,4              SAVE LINK IR
       TSX     $RD,4              GET NEXT ITEM
       CAS     RRPAR
       TRA     *+2
       TRA     RP1                WAS )  RETURN WITH NIL
       TSX     $SAVE,4
       TXL     $END2,,RS2+2       SAVE 2 ITEMS
       CAS     RDOT
       TRA     *+2
       TRA     RP2                WAS .
       CAS     RLPAR
       TRA     *+2
       TSX     READ1,4
       STO     RS2                SAVE RESULTS
       TSX     READ1,4            GET NEXT ITEM
       XCA                        PUT IN MQ
       CLA     RS2                FIRST ITEM
       TSX     UNSAVE,4
       LXD     RS1,4              RESTORE LINK IR
       TRA     $CONS              CONSTRUCT A LIST
*
 RP1   PXD     0,0                WAS )  RETURN WITH NIL
       LXD     RS1,4
       TRA     1,4                                                      PAGE 072
*
 RP2   TSX     $RD,4              WAS .  GET NEXT ITEM
       TSX     REDIS,4            DISPATCH ON IT
       STO     RS2                SAVE RESULTS
       TSX     $RD,4              GET NEXT ITEM
       CAS     RRPAR              SHOULD BE )
       TRA     REDER              GO TO ERROR IF NOT
       TRA     *+2
       TRA     REDER
       CLA     RS2                GET ITEM READ
       TSX     UNSAVE,4
       LXD     RS1,4              RESTORE LINK IR
       TRA     1,4                RETURN WITH IT
*
RLTR   SYN     QUOTED             SYMBOL FLAG
 RNUMB SYN     FLOATD             FLOAT (USED TO SIGNIFY ANY KIND NUMBER
*
I      HED
       REM
       REM RD(A)
       REM
       REM READS BCD LISTS FROM CARDS (SW 1 DOWN) OR TAPE 4 (SW1 UP)
       REM
 RLPAR         ,,$H74D
 RRPAR         ,,$H34D
 RDOT          ,,$H33D
 RDVAL BSS     0
LRCIS          1                  CARD IMAGE EMPTY TEST CELL
RD     CLA     RDLST
       TZE     RDAA               GO IF NOT
       STZ     RDLST              OTHERWISE ZERO
       TRA     1,4                AND EXIT
 RDAA  SXA     RDX,4              SAVE INDEX REGISTERS
       SXA     RDY,2
       SXA     RDZ,1
       STI     RDIND              SAVE THE INDICATORS
       LDI     $ZERO
 RDPTS AXT     6,2                SET UP IR 2 AND 1
 RDWDS AXT     12,1
 RDGC  TSX     GET,4              GET THE FIRST CHARACTER
       PAX     0,4                TYPE TO INDEX REGISTER
       TRA     RDJT1,4            DISPATCH ON TYPE
       TRA     RDDLR              $
       TRA     RDLT
       TRA     RDNM               NUMBER
       TRA     RDGC               ,
       TRA     RDPU               (
       TRA     RDPU               )
       TRA     RDPU               .
 RDJT1 TSX     OUTPUT,4           ILLEGAL CHARACTER
               BCDOUT
               RDPB,,15
       PXD     0,0                CLEAR AC
       SXD     $ERROR,4           SAVE IR 4                             PAGE 073
       TSX     $ERROR+1,4         GO TO ERROR ROUTINE
       BCI     1,*R  3*
RDPU   CLA     RDVAL,4
 RDX   AXT     **,4
 RDFIN SXA     RDPTS,2            SAVE INDEX REGISTERS
       SXA     RDWDS,1
       LDI     RDIND              RESTORE INDICATORS
 RDZ   AXT     **,1               RESTORE INDEX REGISTERS
 RDY   AXT     **,2
       TRA     1,4                EXIT
*
 RDDLR SIR     3                  SET FIRST CHARCTER AND LITERAL INDICAT
       TSX     GET,4              IS NEXT CHARACTER A $
       PAX     0,4                IF SO INDICATES A LITERAL STRING
       CLA     GTVAL              SET VALUE OF GET
       STO     RDDDC
       TXH     RDDD,4,6           GO IF A $
       SXA     RDT,4              NOT SO DO A REGULAR D
       CLA     RDDLS              $
       STO     GTVAL
       TSX     PUT,4              PUT IN OUTPUT BUFFER
       CLA     RDDDC              LAST VALUE OF GET
       STO     GTVAL
 RDT   AXT     **,4               TYPE OF LAST CHARACTER
       TRA     RDJT2,4            DISPATCH ON TYPE
*
 RDDD  TSX     GET,4              IS A LITERAL STRING
       CLA     GTVAL              USE THIS ITEM AS A DELIMITER
       STO     RDDDC
 RDDDL TSX     GET,4              GET NEXT CHARACTER
       CLA     RDDDC              GET DELIMITER
       CAS     GTVAL              COMAPRE WITH CHARACTER JUST READ
       TRA     *+2                NO
       TRA     RDXT               YES, EXIT
       TSX     PUT,4              NO, PUT AWAY THE CHARACTER
       TRA     RDDDL              GET NEXT CHARACTER
*
 RDLT  SIR     2                  SET LITERAL INDICATOR
 RDNM  SIR     1                  SET FIRST CHARACTER INDICATOR
 RDNN  TSX     PUT,4              PUT THE CHARACTER AWAY
       TSX     GET,4              GET NEXT CHARACTER
       PAX     0,4
       TRA     RDJT2,4            DISPATCH ON TYPE
       TRA     RDNN               $
       TRA     RDNN               LITERAL
       TRA     RDNN               NUMBER
       TRA     RDXT               ,
       TRA     RDPS               (
       TRA     RDPS               )
       TRA     RDPD               .
 RDJT2 TRA     RDJT1              ILLEGAL CHARACTER
*
RDPS   CLA     RDVAL,4            SETUP RDLST CELL
       STO     RDLST                                                    PAGE 074
 RDXT  LXA     PUTMC,4            CHARACTER COUNT
       PXD     0,0                CLEAR AC
       TXH     TPF,4,5            GO IF LAST WORD COMPLETED
       LDQ     SEVENS             GET 77 S
       XEC     PTSFT-1,4          PROPER SHIFT
       AXT     6,4                RESET CHARACTER COUNT
       SXA     PUTMC,4
       LXA     PUTPC,4            WORD COUNT
       ORS     RDPNB,4            PUT IN PNAME BUFFER
       PXD     0,0                CLEAR AC
 TPFA  STD     PUTVL+6,4          CHIP OFF PNMAE SAUSAGE
       CLA     PUTVL              GET VALUE
       AXC     RDPU,4             SET UP TRASNFER TO EXIT
       RNT     2                  TEST LITERAL INDICATOR
       TRA     $NUTRN             MAKE IT A NUMBER
       TRA     INTRN1             MAKE IT AN OBJECT
*
 TPF   LXA     PUTPC,4            CORRECT PART COUNT
       TXI     TPFA,4,1
*
 RDPD  RFT     2                  TEST FOR LITERAL
       TRA     RDPS               FIRST . TERMONATES A LITERAL
       RFT     20                 TEST FOR FIRST DOT IN A NUMBER
       TRA     RDPS               SECOND . TERMINATES A NUMBER
       SIR     20                 SET DOT INDICATOR
       TRA     RDNN
*
 GET   SXA     GTX,4              SAVE LINK IR
       ZET     LRCIS              TEST FOR NEW CARD NEEDED
       TRA     GTGCD              GET A NEW CAERD
 GETGO PXD     0,0                CLEAR AC
       LDQ     CELL,1             GET NEXT WORD
       LGL     3                  HIGH ORDER BITS
       PAX     0,4
       LGL     3                  CHARACTER
       CAS     $QO14              IS IT ILLEGAL MINUS SIGN
       TRA     *+2                NO
       CLA     $QO40              YES GET LEGAL ONE
       STO     GTVAL              VALUE OF GET FOR PUT
       ANA     $Q7                MASK OUT HIGH ORDER BIT
       STA     GTPT
       STQ     CELL,1             UPDATE WORD
       TNX     GTPC,2,1           UPDATE PART COUNT
 GTMC  LDQ     GTTBL,4            GET TABLE ENTRY
 GTPT  LGL     **                 SHIFT PROPER ITEM TO AC
       XEC     GTPT
       XEC     GTPT
       PXD     0,0                CLEAR AC
       LGL     3                  TYPE NOW IN AC
 GTX   AXT     **,4               RESTORE LINK IR
       TRA     1,4
*
 GTPC  AXT     6,2                RELOAD PART COUNT
       TIX     GTMC,1,            GO IF NEW WORD NOT NEEDED             PAGE 075
       STI     LRCIS              GET NEW CARD
       AXT     12,1               ERELOAD IR 1
       TRA     GTMC               GO BACJ
*
 GTGCD TSX     $INPUT,4
               $BCDIN
               LWPO,,28           GET NEXT BCD CARD
       TRA     *+2                IGNORE REDUNDNACY ERROR
       TRA     GTEOF              EOF RETURN
       STZ     LRCIS              SET SWITCH THAT CARD IS PRESENT
       TRA     GETGO              NO GO ON
*
 GTEOF PXD     0,0                CLEAR AC
       TSX     $ERROR,4           GO TO ERROR
       BCI     1,* R  4*          EOF ON READ IN
*
 PUT   RFT     40                 TEST TO SEE IF TOOMUCH PNAME
       TRA     PTTFA              GO TO ERROR COMMENT
       SXA     PUTX,4             SAVE LINK IR
       RNT     10                 TEST FOR FIRST TIME THRU
       TRA     PUTZB              ZERO PNAME BUFFER
 PUTMC AXT     6,4                CHARACTER COUNT
       CLA     GTVAL              GET CHARACTER
       LDQ     $ZERO
       XEC     PTSFT,4            PROPER SHIFT TO CHARACTER
       TNX     PTRFP,4,1          DECREMENT CHARACTER COUNT
       SXA     PUTMC,4            UPDATE COUNT CELL
 PUTPC AXT     5,4                NUMBER OF WORDS IN PNAME
 PUTGA ORS     RDPNB,4            PUT CHARACTER IN
 PUTX  AXT     **,4               RESTORE LINK IR
       TRA     1,4                EXIT
 PTRFP AXT     6,4                RELOAD PART COUNT
       SXA     PUTMC,4
       LXA     PUTPC,4            WORD COUNT
       ORS     RDPNB,4
       TIX     *+2,4,1            DECREMENT WORD COUNT
       SIR     40                 INDICATE PNAME BUFFER FULL
       SXA     PUTPC,4            UPDATE COUNTER
       TRA     PUTX               GO ON
*
 PTTFA TSX     OUTPUT,4           TOO MANY CHARACTER
               BCDOUT             WRITE OUT PNAME SO FAR
               RDPNB-6,,6
       PXD     0,0                CLEAR AC
       TSX     $ERROR,4           GO TO ERROR
       BCI     1,*R  5*
*
 PUTZB SIR     10                 SET SWITCH
       AXT     5,4                FIX UP BUFFER
       SXA     PUTPC,4            AND PART COUNT
       STZ     RDPNB,4
       TIX     *-1,4,1
       CLA     PUTVL              RELINK THE WORDS
       AXT     5,4                                                      PAGE 076
       SUB     $QD1               SET POINTERS
       STO     PUTVL+6,4
       TIX     *-2,4,1
       TRA     PUTMC
*
TEREAD STL     LRCIS              SET SWITCH TO GET A NEW CARD
       CLA     $Q6                SET CELLS
       STA     RDPTS
       STA     PUTMC
       CLA     $Q12
       STA     RDWDS
       STZ     RDLST
       PXD     0,0                CLEAR AC
       TRA     1,4                EXIT
*
       LGL     30
       LGL     24
       LGL     18
       LGL     12
       LGL     6
       NOP
 PTSFT BSS     0
RDPNB  BES     5
 PUTVL         ,,-*-1             VALUE OF RDA
               -RDPNB+5,,-*-1     FOR INTERN OF NUTRN
               -RDPNB+4,,-*-1
               -RDPNB+3,,-*-1
               -RDPNB+2,,-*-1
               -RDPNB+1
       OCT     660430000000,466666660000,660760000000,566666660000
       OCT     660120000000,566666660000,350650000000
 GTTBL OCT     555555550000
 RDPB  BCI     1,0
 LWPO
 LWCKS
 CELL  BES     10
 LWDPB BES     12
       BSS     6                  ROOM FOR ID AND LLOK AHEAD BITS
 RDDLS BCI     1,00000$
 RDDDC
 RDIND                            INDICATOR STORAGE
RDLST
GTVAL
*
       REM
       REM INTERN
       REM
I      HED
*                                                                       PAGE 077
* INTERN       CHANGED AND MODIFIED TO INCLUDE EXTERNAL ENTRACES AND
*              THE BUCKET SORT
*
BUKSRT STQ     BSRT               ATOM TO BE PLACED (CNSFWL ENTRANCE)
INTRN1 STO     $VALUE             EXTERNAL ENTRANCE FROM APPLY
INTERN SXA     ITRX,4             ENTRANCE FROM READ
       SXA     ITRY,2             SAVE IR 2
       LXD     $VALUE,4           PICK UP POINTER TO PNAME LIST
       CLA     0,4                GET FIRST WORD OF PNAME
       PAX     0,4
       CAL     0,4                GET FIRST WORD IN LOGICAL AC
       LRS     35                 PUT IN MQ AND BIT 35 OF AC
       DVP     BUCKNO             DIVIDE BY NUMBER OF BUCKETS
       DCT                        CHECK DIVISION
       TSX     $DCT,4             DIVIDE ERROR
       PAX     0,4                REMAIDNER TO IR 4
       CLA     BUCKET,4           PICK UP BUCKET
       SXA     BUCK,4             SAVE THE REMAINDER
       PAX     0,4
       SXD     O5,4               SET UP WORD
       ZET     BSRT               TEST FOR CNSFWL ENTRANCE
       TRA     INTAD              YES, GO
       SXD O1,4
 O4    LXD 01,4                   NEXT OBJECT
       TXL OUT,4,0                END OF OBJLIST
       CLA ,4
       STD O1
       PAX ,4                     OBJECT M/C NAME
       SXD O2,4                   PRESERVE IT
       CLA ,4
 O3    PDX ,4                     ADDRESS PART IS -1
       TXL O4,4,0                 END OF PROPERTY LIST
       CLA ,4
       PAX ,4
       TXL O3,4,$PNAME-1          NO
       TXH O3,4,$PNAME            NO
       PDX ,4                     YES IT IS
       CLA ,4
       PDX ,4                     U
       LXD $VALUE,2               V
 O7    TXL     O4,2,0
       CLA     0,4
       STO Q4                     CDR(U)
       PAX ,4                     CAR(U)
       CLA ,2
       STD Q2                     CDR(V)
       PAX ,2
       CLA ,4                      CWR(CAR(U))
       SUB ,2                     -CWR(CAR(V))
       TNZ O4                     NOT THE SAME,NEXT OBJECT
       LXD Q4,4                   CDR(U)
       LXD Q2,2
       TXH O7,4,0                 IF NOT YET END OF NAME
       TXH Q4,2,0                 IF U,V OF DIFFERENT LENGTH,NEXT       PAGE 078
       CLA O2
       TRA     ITRX
 OUT   CLA $VALUE
       TSX $CP1,4
       LDQ     $ZERO
       TSX $CONS,4
       XCA
       CLA OPNA
       TSX $CONS,4
       XCA                        INTO MQ
       CLA     $DMASK             ATOM SYMBOL
       TSX     $CONS,4            MAKE IT AN ATOM
 INTCN LDQ     O5                 LIST OF ATOMS IN BUCKET
       STD     O5                 SAVE ATOM AS ANSWER
       TSX     $CONS,4            ATTACH TO BEGINNING OF LIST
       ARS     18                 PUT IN ADDRESS
 BUCK  AXT     **,4               BUCKET NUMBER
       STA     BUCKET,4           PUT IN PROPER BUCJET
       CLA     O5                 ATOM AS ANSWER
 ITRX  AXT     **,4               RESTORE LINK IR
 ITRY  AXT     **,2
       TRA     1,4                EXIT
 INTAD CLA     BSRT               PICK UP ATOM
       STZ     BSRT               ZERO LOCATION
       TRA     INTCN              PLACE ATOM IN BICKET
 VALUE                            POINTER TO PNAME LIST
 BSRT                             ATOM IN CNSFWL WENTRANCE
BUCKNO PZE     127                NUMBER OF BUCKETS
*
 O1
 O2
 O5
 OPNA  SYN     PNAMED
 Q2
 Q4
       REM
T      HED
 NUTRN SXA     NX4,4              SAVE IDNEX REGISVERS
       SXA     NX2,2
       SXA     NX1,1
       AXT 6,1
       LXD $VALUE,4
 NA1   CLA 0,4
       PDX 0,4
       PAX 0,2
       CLA 0,2
       STO BUFFER+6,1
       TXL NA2,4,0
       TIX NA1,1,1
 NE    SXD $ERROR,4
       TSX OUTPUT,4
           BCDOUT
           I$CELL-15,,14
       PXD     0,0                CLEAR AC                              PAGE 079
       TSX $ERROR+1,4
       BCI     1,*R  6*           NUMBER TO LARGE IN CONVERSION
 NA2   CLA BLANKS
       STO BUFFER+7,1
       CLA     KBPOS              PARAMETER FOR NUMBR
       TSX     $NUMBR,4           NUMBER TO MQ
       TZE     NE                 OUT-OF-RANGE ERROR
       TMI     NA7                TRA IF FLOATING NUMBER
       PBT                        TEST FOR OCTAL NUMBER
       TRA     NA3                TRA IF OCTAL
       CLA     $OCTD              OCTAL SIGNAL FOR $MKNO
       XCA
       TRA     NA8
 NA3   XCA                        NUMBER TO AC
       LDQ     $FIXD              FIX TO MQ
       TMI     NA8
       CAS     $Q10               TEST FOR 0 THRU 9
       TRA     NA8
       TRA     NA8
       ACL     $H00A              FORM PRINT OBJECT
       ALS     18
       TRA     NX1
 NA7   CLA     FLOATD             FLOAT SIGNAL FOR $MKNO
       XCA                        NUMBER TO AC
 NA8   TSX     $MKNO,4            MAKE A NUMBER
 NX1   AXT     **,1               RESTORE INDEX REGISTERS
 NX2   AXT     **,2
 NX4   AXT     **,4
       TRA     1,4
 KBPOS PZE     BUFFER,,1
       REM
       REM
F      HED
       REM
       REM NUMBR CONVERTS PACKET BCD CHARACTERS TO A NUMBER WHICH
       REM APPEARS IN MQ.   DBC CONVERSIONS ARE FOLLOWED.  OCTAL
       REM NUMBERS ARE SIGNALLED BY Q AND MAY BE FOLLOWED BY A
       REM SCALE FACTOR.
       REM
       REM ROUTINE STOLEN FROM UADBC1
       REM
       REM
 NUMBR SXA PX1,1          SAVE INDEX REGISTERS
       SXA PX2,2
       SXA PX4,4
       SLW T
       PAC ,2             IR2 HAS WORD COUNT
       PDC ,1             IR1 WILL GET CHARACTER COUNT
       ARS 17
       STO N
       ALS 1
       ADD N
       PAC ,4
       LDQ 0,2            PUT BCD WORD IN MQ                            PAGE 080
       LGL -6,4           SHIFT OUT EXTRA CHARACTERS
       STQ MQ             SAVE FIRST BATCH OF CHARACTERS
       TXI *+1,1,7
       REM
       REM LOOK AT CHARACTERS UNTIL A Q OR NON-OCTAL CHARACTER APPEARS.
       REM
CY3    PXD     ,0
       LGL 6
       SUB Q8             TEST FOR OCTAL DIGIT
       TPL CY4
 CY2   TIX CY3,1,1        GET NEXT CHARACTER
       TXI *+1,2,-1
       LDQ 0,2
       TXI CY3,1,5
 CY4   ADD Q8
       CAS Q
       TRA DECNO
       TRA OCTNO          IF Q, NUMBER IS OCTAL
       CAS MINUS          IF CHARACTER IS MINUS, PLUS OR DASH,
       TRA DECNO            LOOK AT MORE CHARACTERS,
       TRA CY2              OTHERWISE NUMBER IS DECIMAL
       CAS PLUS
       TRA DECNO
       TRA CY2
       CAS DASH
       TRA DECNO
       TRA CY2
       REM
 DECNO LAC T,2            IR2 HAS WORD COUNT
       LDC T,1            IR1 WILL GET CHARACTER COUNT
       LDQ MQ             RESTORE FIRST GRUOP OF CHARACTERS
       PXD     ,0
 BN2   SLW BN             REGISTERS
 EX2   SLW EXPN
 INTN  SLW N
       LXD Q10,4          SET DECIMAL COUNT TO ZERO
       CAL SW1            RESET SWITCHES FOR
       STP CM2             FIXED POINT
       STP CM6             X
       STP EXS             EXP
       STP CM3             POINT
       STP CX3             DECIMAL NUMBER
       CAL INTN           INITIALIZE CONVERSION
       TXI BN3,1,8        FIX INITIAL CHARACTER COUNT
 PT1   CLS CM3            INVERT SWITCH TO SIGNAL DECIMAL POINT
       STO CM3
       CAL CV3
       STA CV5            ROUTINE TO COUNT
       STA CV6            DECIMAL PLACES
       TXI CV5,4,1
 PT3   TXI CV3,4,-1       COUNT DECIMAL PLACES
 EX1   CLS EXS            INVERT SWITCH TO SIGNAL EXPONENT
       STO EXS
       CAL EX2            SET UP EXPONENT CONVERSION                    PAGE 081
 BN3   STA CV7            STORE CONVERSION
       STA CV8            ADDRESS
       STA CV9
       CAL PT3            INITIAL CONVERSION
       STA CV5            WITHOUT DECIMAL COUNT
       STA CV6
 PL1   CAL CV8
 MN3   STD CV10
       TOV CV5
       TXL CV5
 BN1   CLA CM2            INVERT SWITCHES TO SIGNAL FIXED POINT
       STP CM2
       STP CM6
       CAL BN2            SET UP B CONVERSION
       TXL BN3
 MN1   CLA PBIT           START NEGATIVE ACCUMULATION WITH NEG. ZERO
       STO* CV7
       CAL MN2            OP CODE TO MAKE CVIO A SUB INSTRUCTION
 MN2   TXL MN3,0,258*64
CV3    PXD     PT3,0
       LGL 6
       CAS TEN            TEST FOR DIGIT
       TXL CM
       TXL CV2
       SLW CH             PERFORM CODED
 CV7   CLA N              MULTIPLICATION
       ALS 2              BY TEN AND ADD
 CV8   ADD N
       ALS 1
 CV10  ADD CH
       TOV OVF            TEST FOR OVERFLOW
 CV9   STO N
 CV5   TIX CV3,1,1        COUNT CHARACTERS
       TXI CV4,2,-1       OBTAIN NEXT BCD
 CV4   LDQ 0,2            WORD AND RESTORE
 CV6   TXI CV3,1,5        CHARACTER COUNT
 OVF   TXI CV5,4,1        COUNT DECIMAL OVERFLOWS
 CM    CAS MINUS
 SW1   TXL CV2
       TXL MN1
       CAS POINT
       TXL CV2
       TXL PT1
       CAS E
       TXL CV2
       TXL EX1
       CAS B
       TXL CV2
       TXL BN1
       CAS PLUS
       TXL CV2
       TXL PL1
       CAS DASH           DASH TREATED LINK MINUS
       TRA CV2                                                          PAGE 082
       TRA MN1
 CV2   CLA N
       TZE STZ            SEE IF ZERO FIXED OR FLOATING
 EXS   TXL CX3            SWITCH - TXH INDICATES EXPONENT
       CAL PBIT           PREPARE TRUE
       ADD EXPN           DECIMAL EXPONENT
       ALS 18
       STD CM4
       CLA N
CM4    TXL     CM5,4,0
 CX3   TXL CM2            SWITCH - TXH INDICATE OCTAL
       REM SCALE OCTAL NUMBER
       CLA BN             MULTIPLY SCALE FACTOR BY 3
       ALS 1               FOR NUMBER OF SHFITS NEEDED
       ADD BN
       STA CX5
       CLA N
 CX5   ALS **
       PBT                ALLOW FOR P BIT
       TRA ISTOR
       SSM
       TRA ISTOR
       REM
 CM2   TXL CM3            SWITCH - INVERTED TO TXH INDICATES FIXED POINT
       TXL CM5
 CM3   TXL ISTOR          SWITCH - TXH INDICATES POINT
 CM5   STA FL1            35 BIT INTEGER
       ARS 15
       ORA FL2
       FAD FL2
       TPL CMF1
       FSB FL1
       TXL CMF2
 CMF1  FAD FL1
 CMF2  STQ RESID
       TXL     CM6,4,0
 SW2   TXH CM7,4,38       TEST FOR NEGATIVE EXP
       SXA *+1,4          COMPUTE ABSOLUTE VALUE OF EXPONENT
       AXC **,4
       STO DATUM
       LDQ ONE,4          COMPUTE FLOATING
       FMP DATUM          BINARY REPRESENTATION
       STO T              OF INTEGER TIMES THE
       STQ T+1            POWER OF TEN GIVEN
       LDQ ONE,4          BY THE TRUE EXPONENT
       FMP RESID
       FAD T+1
       FAD T
       ACL EXC1
       PBT
       TXL CM6
CM8    PXD     ,0
       TRA PX1            NUMBER OUT OF RANGE, EXIT WITH 0 IN AC
 CM7   TXL CM8,4,-49      TEST FOR ILLEGAL EXP                          PAGE 083
 CM13  TQO CM13+1
       FDP ONE,4          COMPUTE FLOATING
       STQ T              BINARY EQUIVALENT
       FAD RESID          OF INTEGER TIMES
       FDP ONE,4          POWER OF TEN GIVEN
       TQO CM8
       STQ T+1            BY TRUE EXPONENT
       CLA T+1
       FAD T
       ACL EXC2
       PBT
       TXL CM8
 CM6   TXL FSTOR          SWITCH - TXH INDICATES FIXED POINT
       STO T
       ALS 2
       SSM                DETERMINE SHIFT
       ARS 29             NECESSARY TO POSITION
       ADD Q128           NUMBER AS INDICATED
       ADD BN             BY B
       TPL SHIFT
       TNZ CM8
 SHIFT STA CM12
       CLA T              REMOVE CHARACTERISTICS
       LLS 8              FROM FLOATING NUMBER
       ALS 2
       ARS 10
       LLS 8
CM12   LRS     **
 ISTOR XCA                RESULT TO MQ
 ISTO1 CAL SW1            SET FIXED POINT INDICATOR SWITCH
       TRA XT3
 FSTOR XCA                RESULT TO MQ
       CLA SW1            SET FLOAT INDICATOR SWITCH
 XT3   STP XT1
       TIX XT2,1,1        IF NO SIGNIFICANT CHARACTERS
       TXI *+1,2,-1        LEFT IN WORD, MOVE TO  NEXT WORD
       AXT 6,1
 XT2   PXD ,1             SET POSITION INDICATORS
       SUB QD7
       SLW T
       CAL CX3                    P BIT IN OUTPUT INDICATES OCTAL
       ANA $SBIT
       ORS T
       COM
       STP T
       SXA *+1,2
       AXC **,2
       PXA ,2
       ACL T
 XT1   TXL *+2            SET SIGN + FOR FIXED.
       SSM                 - FOR FLOATING
 PX1   AXT ,1             RESTORE INDEX REGISTERS
 PX2   AXT ,2
 PX4   AXT ,4                                                           PAGE 084
       TRA 1,4            EXIT
       REM
       REM WE GET HERE IF NUMBER IS ZERO.
       REM WE HERE DECIDE WHETHER WE ARE FACED WITH A FIXED OR FLOATING
       REM  ZERO.
       REM
 STZ   LDQ CM2                    TXH (+) IF B
       TQP ISTOR
       LDQ CM3                    TXH (+) IF DECIMAL POINT FOUND
       TQP FSTOR
       LDQ EXS                    TXH (+) IF E FOUND
       TQP FSTOR
       TRA ISTOR
       REM
       REM PROCESS OCTAL NUMBER
       REM
 OCTNO LAC T,2            IR2 HAS WORD COUNT
       LDC T,1            IR1 WILL GET CHARACTER COUNT
       LDQ MQ             RESTORE FIRST GROUP OF CHARACTERS
       PXD     ,0
       STA CV7            SET SIGNAL FOR OCTAL NUMBER
       TXI OCT9,1,8       FIX CHARACTER COUNT
OCT1   PXD     ,0
       LGL 3
       TNZ OCT8
       CLA N
       LGL 3
 OCT9  STO N              ALLOW FOR BOTH P BIT AND MINUS SIGN
       ORS N
 OCT6  TIX OCT1,1,1
       TXI OCT2,2,-1
 OCT2  LDQ 0,2            NEW PACKED WORD
       TXI OCT1,1,5
 OCT8  LGL 3
       CAS Q              TEST FOR OCTAL SCALE FACTOR
       TRA OCT3
       TRA OCT10
       CAS MINUS
       TXL OCT3
       TXL OCT5
       CAS PLUS
       TXL OCT3
       TXL OCT6
       CAS DASH           DASH TREATED LINK -
       TXL OCT3
       TXL OCT5
 OCT3  LDQ N
       TXI ISTO1
 OCT5  CLA PBIT           SET NEGATIVE SIGN
       TXL OCT9
 OCT10 CLA CX3            SET SWITCH FOR OCTAL SCALE FACTOR
       STP CX3
       STZ BN             CLEAR SCALE FACTOR CELL
       CAL SW1            SET EXPONENT SWITCH TO OFF                    PAGE 085
       STP EXS
       CAL BN2            SET UP Q CONVERSION
       TRA BN3
       REM
       REM
       REM
 Q8    SYN $Q8
 Q10   SYN $Q10
 Q128  SYN $Q128
 QD7   SYN $QD7
 PBIT  SYN $SBIT
 BLANK SYN $QO60
 MINUS SYN $QO40
 POINT SYN $QO33
 E     SYN $QO25
 B     SYN $QO22
 Q     SYN $QO50
 PLUS  SYN $QO20
 DASH  SYN $QO14
 EXC1  DEC 3588           CHARACTERISTIC=35
 EXC2  DEC 22188          CHAR.=COMPL. 35
 FL1   DEC 15588
 FL2   DEC 17088
       OCT 141500000000,144620000000,147764000000,153470400000
       OCT 156606500000,161750220000,165461132000,170575360400
       OCT 173734654500,177452013710,202564416672,205721522451
       OCT 211443023471,214553630410,217706576512,223434157116
       OCT 226543212741,231674055532,235425434430,240532743536
       OCT 243661534466,247417031702,252522640262,255647410336
       OCT 261410545213,264512676456,267635456171,273402374714
       OCT 276503074077,301623713116,304770675742,310473426555
       OCT 313612334311,316755023373,322464114135,325601137164          PAGE 086
       OCT 330741367021,334454732313,337570120775,342726145174
       OCT 346445677216,351557257061,354713132676,360436770626
       OCT 363546566774,366700324573,372430204755,375536246150
 TEN   SYN Q10
 ONE   SYN FL2
 REORG BSS 0
       ORG COMMON
 BN    BSS 1
 MQ    SYN BN
 EXPN  BSS 1
 CH    BSS 1
 CHD   BSS 1
 T     SYN CHD
 N     BSS 1
 DATUM BSS 1
 RESID BSS 1
       ORG REORG                  RESTORE ORIGIN
BUFFER BSS 14
       REM
       REM
R      HED
       REM
       REM FUNCTION CP1
       REM CP1(L)=(L=0 YIELDS 0.
       REM        OTHERWISE CONS(CONSW(CWR(CAR(L)))),CP1(CDR(L))))
       REM
C      HED
 CP1   TZE 1,4
       SXD CR1,4
       PDX ,4
       CLA ,4                     CWR(L)
       STO CWRL
       PAX ,4                     CAR(L)
       CLA ,4                     CWR(CAR(L))
       TSX $CONSW,4
       TSX $SAVE,4
       TXL     $END2,,CR2+2       SAVE 2 ITEMS
       STO CR2
       LXD CWRL,4                 CDR(L)
       PXD ,4                     IN DEC                                PAGE 087
       TSX CP1,4
       STO CWRL
       LDQ CWRL                   C(MQ)=CP1(CDR(L))
       CLA CR2
       TSX UNSAVE,4
       LXD CR1,4
       TRA $CONS
       REM
       REM SUBST
       REM
       REM      SUBST(L,V,M) =
       REM          (M = 0 YIELDS 0,
       REM           EQUAL(M,V) YIELDS COPY(L),
       REM                         CAR(M)=-1 YIELDS M 
       REM           1 YIELDS CONS(SUBST(L,V,CAR(M)),SUBSTL,V,CDR(M))))
       REM
R      HED
 SUBST STO      SX
       STO      SY
       CLA      $ARG3
 SUB1  SXD      SXT,4
       STO      ST
       LDQ      SY
       TSX      $EQUAL,4
       TNZ      SUB4
       LXD      ST,4
       CLA      0,4
       PAX      0,4
       CLA      ST
       TXH      SUB2,4,-2
       TSX      $SAVE,4
       TXL      $END2,,SZ+2
       STD      SZ
       PDX      0,4
       CLA      0,4
       PDX      0,4
       SXA      SZ,4
       PAX      0,4
       PXD      0,4
       TSX      SUB1,4
       LXA      SZ,4
       ARS      18
       STA      SZ
       PXD      0,4
       TSX      SUB1,4
       LXD      SZ,4
       STD      SZ
       CLA      0,4
       SUB      SZ
       TZE      SUB3
       LXD      $FREE,4
       TXH      *+2,4,0
       TSX      $FROUT,4
       CLA      0,4                                                     PAGE 088
       STD      $FREE
       CLA      SZ
       STO      0,4
 SUB3  PXD      0,4
       TSX      UNSAVE,4
 SUB2  LXD      SXT,4
       TRA      1,4
 SUB4  CLA      SX
       TRA      SUB2
B      HED
       REM
       REM FUNCTION SUBLIS
       REM
SUBLIS STQ E
       TNZ SU1
       CLA E                       P=0
       TRA 1,4
 SU1   STO P
       CLA E
       TNZ SU2
       TRA 1,4                     E=0
 SU2   SXD X1,4
       CLA F                       U
       STO $ARG4                   U
       CLA F+1                     F
       STO $ARG3                   F
       LDQ F+2                     P
       CLA P                       
       TRA SEARCH
 F     TXL NF,,0                   U
       TXL NF1,,0                  F
       TXL NF2,,0                  P
 NF    LXD E,4                     U
       CLA ,4
       PAX ,4                      CAR(E)
       TXL SU3,4,-2                E IS NOT AN OBJECT
       CLA E
       LXD X1,4
       TRA 1,4
 SU3   TSX $SAVE,4
       TXL     $END4,,X4+2         SAVE 4 ITEMS
       STD X2
       PAX ,4
       SXD X3,4                    CAR(E)
       LDQ X2
       CLA P
       TSX SUBLIS,4
       STO X4                      SUBLIS(P,CDR(E))
       LDQ X3
       CLA P
       TSX SUBLIS,4
       LDQ X4
       TSX $CONS,4
       TSX UNSAVE,4                                                     PAGE 089
       LXD X1,4
       TRA 1,4
 NF2   SXD N1,4                   EQUAL(E,CAAR(J))
       PXD ,4                     J
       CLA ,4
       PAX ,4                     CAR(J)
       CLA ,4
       STD X5                     CDAR(J)
       PAX ,4
       SXD N2,4
       LDQ N2                     CAAR(J) IN MQ
       CLA E
       TSX $EQUAL,4
       LXD N1,4
       TRA 1,4
 NF1   CLA     X5
       TRA 1,4
 N1                               IR4 OF P OF SEARCH
 N2
       REM
       REM APPEND(L1,L2)=
       REM (L1=0 YIELDS L2,1 YIELDS CONS(CAR(L1),APPEND(CDR(L1),L2))
A      HED
APPEND TNZ APNP1
       XCA
       TRA 1,4
 APNP1 SXD AS1,4
       TSX $SAVE,4
       TXI     $END2,,CWR1+2      SAVE 2 ITEMS
       PDX 0,4
       CLA 0,4
       STO CWR1
       ANA DECM
       TSX APPEND,4
       XCA
       LXA CWR1,4
       PXD 0,4
       TSX UNSAVE,4
       LXD AS1,4
       TRA $CONS
 DECM  SYN     $DMASK
       REM
       REM PAIR
*              RECODED TO MAKE LISTS IN DOT NOTATION
       REM
A      HED
 PAIR  SXA     PAIRX,4            SAVE LINK IR
       STQ     LIS                ARG 2
       LDQ     FARG               PICK UP FUNCTIONAL ARGUMENT
       TSX     MAPLIS,4           LET MAPLIST DO THE CONSING
       ZET     LIS                TEST FOR ARG 2 GONE TO END
       TRA     PERF               DID NOT, GO TO ERROR
 PAIRX AXT     **,4               RESTORE LINK IR
       TRA     1,4                EXIT                                  PAGE 090
*
 FARG  TXL     *+1,,1             PAIR FUNCTIONAL ARGUMENT FOR MAPLIST
       SXA     FARGX,4            SAVE LINK IR
       STD     TEM                SAVE ARGUMENT
       LXD     LIS,4              PICK UP 2ND ARG LIST
       TXL     PERS,4,0           GO IF NO MORE 2ND ARG
       CLA     0,4                NEXT WORD
       PAX     0,4                CAR
       STD     LIS                SAVE CDR
       PXD     0,4                CAR INTO DECREMENT
       XCA                        INTO MQ
       LXD     TEM,4              LIST 1
       CLA     0,4                TAKE CAR OF LIST
       PAX     0,4
       PXD     0,4
 FARGX AXT     **,4               RESTORE LINK IR
       TRA     $CONS
*              FIRST ARG LIST TOO SHORT ERROR
 PERF  SXD     $ERROR,4           SAVE LINK IR
       TSX     $ERROR+1,4         GO TO ERROR
       BCI     1,*F  2*           FIRST ARG$ LIST TOO SHORT
*              ERROR, SECOND ARG LIST TOO SHORT
 PERS  SXD     $ERROR,4           SAVE LINK IR
       TSX     $ERROR+1,4         GO TO ERROR
       BCI     1,*F  3*           SECOND ARG. LIST TOO SHORT
*
*
*
       REM
       REM MAPCAR(L,F) = (L=0 YIELDS 0,
       REM                F(L) YIELDS 0,
       REM                1 YIELDS MAPAR(CDR(L),F))
       REM
D      HED
MAPCAR TZE 1,4
       SXD RET,4
       TSX $SAVE,4
       TXL     $END3,,F+2         SAVE 3 ITEMS
       STQ F
 MCPR  STO L
       LXD F,4
       TXH *+3,4,10
       TSX F,4
       TRA *+4
       SXD *+2,4
       TSX COMPAT,4
           1,,**
       LXD 1,4
       CLA 0,4
       PDX ,4
       PXD ,4
       TNZ     MCPR
 RTRN  TSX UNSAVE,4
       LXD RET,4                                                        PAGE 091
       TRA 1,4
       REM MAPCON(L,F)=
       REM (L=0 YIELDS 0,,1 YIELDS NCONC(F(L),MAPCON(CDR(L),F)))
R      HED
MAPCON TZE 1,4
       SXD MCN5,4
       TSX $SAVE,4
       TXL     $END4,,MCN2+2      SAVE 4 ITEMS
       STO MCN3
       STQ MCN4
       LXD MCN4,4
       TXH *+3,4,10
       TSX MCN4,4
       TRA *+4
       SXD *+2,4
       TSX COMPAT,4
           1,,**
       STO MCN2
       LXD MCN3,4
       CLA 0,4
       ANA MCDM
       LDQ MCN4
       TSX MAPCON,4
       XCA
       CLA MCN2
       TSX UNSAVE,4
       LXD MCN5,4
       TRA     $NCONC
 MCDM  SYN     $DMASK
       REM FUNCTION NCONC
       REM /    L1=0 YIELDS RETURN(L2)
       REM      M=L1
       REM A2   CDR(M)=0 YIELDS GO A1
       REM      M=CDR(M)
       REM      GO A2
       REM A1   CDR(M)=L2
       REM //   RETURN(L1)
R      HED
 NCONC TNZ NCI1
       XCA
       TRA 1,4
 NCI1  SXA     NCS1,4             SAVE LINK IR
       STO NCS3
  NCI2 PDX 0,4
       CLA 0,4
       ANA NCDM
       TNZ NCI2
       XCA
       STD 0,4
       CLA NCS3
 NCS1  AXT     **,4               RESTORE LINK IR
       TRA 1,4
 NCDM  SYN     $DMASK
 NCS3                                                                   PAGE 092
       REM REMPRP REMOVES THE PROPERTY GIVEN BY THE MQ FROM THE
       REM OBJECT GIVEN BY THE AC
REMPRP SXD RMPRX,4
       STQ $ARG2
       LXD $ARG2,4
       SXD RMPRT+1,4
       TXI *+1,4,-1
       SXD RMPRT,4
       PXD 0,4
       TRA RMPR2
 RMPR1 CLA $ARG2
       STO $ARG3
 RMPR2 SXD $ARG2,4
       CLA 0,4
       PAX 0,4
 RMPRT TXL *+2,4,**
       TXL RMPRE,4,**
       PDX 0,4
       TXH RMPR1,4,0
 RMPRO LXD RMPRX,4
       TRA 1,4
 RMPRE PDX 0,4
       CLA 0,4
       LXD $ARG3,4
       STD 0,4
       TRA RMPR2
 RMPRX
       REM
       REM
       REM PRINAR
       REM
       REM USES WOT AND PRINT
       REM CALLING SEQ IS..
       REM         TSX PRINAR,4
       REM             NOARG
       REM             BCDZ NAME OF FUN
       REM               (RETURN)
       REM ARGUMENTS NOT ACCEPTABLE TO PRINT WILL CAUSE ERRORS
* HAS BEEN CRIPPLED TO PRINT ONLY FIRST 2 ARGUMENTS
       REM
P      HED
PRINAR SXA     PAS1,4             SAVE INDEX REGISTERS
       SXA     PAS2,2
       STO PAS3
       STQ PAS4
       CLA 2,4
       STO PAL1
       CLA 3,4
       STO PAL2
       CLA 1,4
       PAX 0,2
       TSX OUTPUT,4
           BCDOUT
           PAL3,,PAL4-PAL3                                              PAGE 093
       CLA PAS3
       TSX $PRINT,4
       TNX PAP3,2,1
       CLA PAS4
 PAP2  TSX $PRINT,4
 PAP3  TSX OUTPUT,4
           BCDOUT
           PAL5,,1
       CLA PAS3
       LDQ PAS4
 PAS1  AXT     **,4               RESTORE INDEX REGISYERS
 PAS2  AXT     **,2
       TRA 4,4
 PAL3  BCD 20 FUNCTION
 PAL1
 PAL2
       BCD 5 HAS BEEN ENTERED, ARGUMENTS..
 PAL4  BSS 0
 PAL5  BCD 1
       EJECT                                                            PAGE 094
       REM
       REM PROP AND SASSOC
       REM SPECIALIZED SEARCH ROUTINES WHICH SHARE STORAGE
       REM
R      HED
       REM
       REM PROP(O,P,U)
       REM  = (NULL(O) YIELDS U, CAR(O) = P YIELDS CDR(O),
       REM      T YIELDS PROP(CDR(O),P,U))
       REM
 PROP  SXA     SAST1,4            SAVE LINK IR
       XCA                        PROPERTY TO AC
       STD SASP1                  SET TXH
       SUB SASQ1
       STD SASP2                  SET TXL
       XCA                        OBJECT TO AC
 SASL1 PDX 0,4                    L = CDR(L)
       REM INSERT TXH INSTRUCTION HERE IF NILL IS NADE NON-ZERO
       TXL SASP3,4,0              NULL(L)
       CLA 0,4                    CWR(L)
       PAX 0,4                    CAR(L)
 SASP2 TXL SASL1,4,**
 SASP1 TXH SASL1,4,**
       ANA SASDM
       LXA     SAST1,4            RESTORE LINK IR
       TRA 1,4
       REM
 SASP3 PXD     0,0                CLEAR
       LXD     $ARG3,4            INSPECT FUNCTIONAL ARGUMENT
       TXH     *+3,4,10           SKIP IF NOT A TXL
       LXA     SAST1,4
       TRA     $ARG3
       STZ     $ARG3
       LDQ     $ARG3
       PXD     ,4
       LXA     SAST1,4            RESTORE LINK IR
       TRA     $APPLY
       REM
       REM SASSOC(O,A,U)
       REM  = (NULL(A) YIELDS U, CAAR(A) YIELDS  CAR(A),
       REM      T YIELDS SASSOC(O,CDR(A),U))
       REM
SASSOC SXA     SAST1,4            SAVE LINK IR
       SXA     SAST2,2            SAVE IR 2
       SXA     SAST3,1            SAVE IR 1
       STD SASP7                  SET TXH
       SUB SASQ1
       STD SASP6                  SET TXH
       XCA                        PAIR LIST TO AC
       PDX     0,4                TO INDEX 4
 SASP5 TXL SASP4,4,0              NULL(A)
       REM INSERT TXH INSTRUCTION HERE IF NILL IS NADE NON-ZERO
       CLA 0,4                    CWR(A)
       PDX ,4                     CDR(A)                                PAGE 095
       PAX ,2                     CAR(A)
       CLA ,2
       PAX     0,1                CAAR(A) TO INDX REGISTER
 SASP6 TXL     SASP5,1,**         LOOK FOR ITEM
 SASP7 TXH     SASP5,1,**
 SAST3 AXT     **,1               FOUND ITEM, RESTORE IR 1
       PXD     0,2                POINTER TO WORD
 SAST2 AXT     **,2               RESTORE IR 2
 SAST1 AXT     **,4               RESTORE LINK IR
       TRA     1,4
       REM
 SASP4 LXA     SAST2,2            RESTORE IR 2
       LXA     SAST3,1            RESTORE IR 1
       TRA     SASP3              EXECUTE SASSOC EXIT
 SASQ1 SYN     $QD1
 SASDM SYN     $DMASK
       REM
SPREAD TZE     1,4                EXIT IF AGLIST IS NULL
       SXA     SPRX,4             SAVE LINK IR
       PDX     0,4                POINTER TO ARG LIST
       CLA     0,4                FIRST WORD
       LDQ     0,4                POINTER TO ARG LIST
       LGR     18                 CAR TO CDR OF MQ
       TZE     NLY                GO IF A  SINGLE ARGUMENT
       PAX     0,4                POINTER TO NEXT WORD
       CLA     0,4                NEXT WORD
       PAX     0,4                POINTER TO ARGUMENT
       ANA     $DMASK             MASK OUT ALL BUT DECREMENT
       TZE     TWA                GO IF 2 ARGUMENT
       SXD     $ARG2,4            PUT AWAY
       SXA     SPRY,2             SAVE INDEX 1 AND 2
       SXA     SPRZ,1
       AXT     18,1               20 IS MAX NO OF ARGS
       PDX     0,4                REST OF ARG LIST TO IR 4
 SPP1  TXL     SPRZ,4,0           GO IF END OF LIST
       CLA ,4
       PDX ,4
       PAX ,2
       PXD ,2
       STO     $ARG20+1,1
       TIX SPP1,1,1
SPPERR SXD     $ERROR,4
       TSX $ERROR+1,4
       BCI     1,*A  7*           TOO MANY ARGUMENTS---SPREAD*()
       REM
 SPRZ  AXT     **,1               RESTORE IR 1
 SPRY  AXT     **,2               DITTO IR 2
       LXD     $ARG2,4            ARG 2
 TWA   PXD     0,4                PUT IN DECREMENT AC
 NLY   XCA                        ARG 1 AND 2 TO RIGHT REGISTERS
 SPRX  AXT     **,4               RESTORE LINK IR
       TRA     1,4                EXIT
       REM
       REM FUNCTION ATTRIB(O,L)                                         PAGE 096
       REM ATTRIB(O,L)=/ CDR(O)=0 YIELDS (L REPLACES CDR(O))
       REM              ELSE ATTRIB(CDR(O),L)  /
       REM
R      HED
ATTRIB SXA AT1,4
       TNZ     ATRB               GO IF BEGINNING OF LIST
       XCA                        OTHERWISE EXIT WITH ARG 2
       TRA     1,4
 ATRB  PDX ,4                     O
       CLA ,4
       ANA DMASK                  CDR(O)
       TNZ ATRB
       XCA                        ARG 2 TO AC
       STO ,4
 AT1   AXT **,4
       TRA 1,4
 DMASK SYN     $DMASK
       REM
       REM
       REM NOT FUNCTION
       REM
R      HED
 NOTS  TZE *+3
       PXD ,0
       TRA 1,4
       CLA NOTC1
       TRA 1,4
 NOTC1 SYN     $QD1
       REM
       REM THE RPLACX FUNCTIONS  REPLACE THE X PART OF THE FIRST ARG
       REM WITH THE SECOND ARGUMENT
       REM THE VALUE OF REPLACA,REPLACD, AND REPLACW IS ZERO
S      HED
RPLACA SXA REPL,4
       PDX 0,4
       LGL 18
       STA 0,4
 RPLEX PXD     0,4                ARG1 TO AC AS ANSWER
 REPL  AXT     **,4               RESTORE LINK IR
       TRA 1,4
RPLACD SXA REPL,4
       PDX 0,4
       SLQ 0,4
       TRA     RPLEX              EXIT
RPLACW SXA REPL,4
       PDX 0,4
       STQ 0,4
       TRA     RPLEX              EXIT
       REM
       REM
       REM OBJECT GENERATOR
       REM
GENSYM SXA     GENX,4             SAVE LINK IR
       CLA     DIGIT              GET DIGITS                            PAGE 097
       CVR     BCDAD1,,6          ADD 1 IN BCD
       STO     DIGIT
       ORA     LETTR
       TSX $CONSW,4
       LDQ GENZ
       TSX $CONS,4
       LDQ GENZ
       TSX $CONS,4
       XCA
       CLA GENPN
       TSX $CONS,4
       XCA
       CLA     GENC
       TSX     $CONS,4
 GENX  AXT     **,4               RESTORE LINK IR
       TRA 1,4
 GENZ  SYN     $ZERO
 GENPN SYN     PNAMED
 GENC  SYN     $DMASK
 LETTR BCI     1,G00000
 DIGIT BCI     1,000000
       REM
*
* OVERLORD     THE TAPE HANDLING SECTION OF LISP. RECODED 20 FEBRUARY
*              1961 BY D. J. EDWARDS.
*
* OVERLORD DIRECTION CARDS ARE PUNCHED IN FAP FORMAT WITH THE VARIABLE
*              FIELD BEGINNING IN COLUMN 16. DIRECTION CARDS ARE
*      ONE     (USE NO TAPES FOR THIS RUN)
*      SET     ( SAVE RESULTS ON SYSTMP IF NO ERROR OCCURS)
*      TST     (GET NEW CORE IMAGE AFTER OPERATION)
*      TEST    (SAVE AS ABOVE)
*      FIN     (ALL DONE, STOP MACHINE OR RETURN TO A HIGHER MONITOR)
*      SETSET  (AVE RESULTS ON SYSTMP NO MATTER WHAT)
*      DEBUG   (SAME AS TEST BUT OBJECTLIST IS NOT SAVED AFTER READ IN)
*      SIZE    N1,N2,N3,N4  (GIVES SIZE OF BINPRG, PPDL, FWS AND FREE)
*      TAPE    SYSXXX,A7  (ASSIGNS SYSXXX TO UNIT A 7)
*      DUMP    BEG,END,TYPE   (MAKES OCTAL DUMP ON SYSPOT ACCORDING TO
*                              TYPE, 0 FOR STRAIGHT OCTAL, NON-ZERO FOR
*                              LISP (COMPLEMENT) DUMP.)
*      REMARK  (LOG AS DIRECTION CARD AND LOKK FOR NEXT DIRECTION CARD)
*      EXCISE  I  (I IS COMPILER, INTERPRETER OR BOTH. TURNS ITEM INTO
*                 FREE STORAGE OR FULL WOTD SPACE)
*
*
 OVBGN STI     OVSVI              BEGIN BY SAVING INDICATORS AND
       SXA     OVRLX,4            INDEX REGISTERS
       SXA     OVRLY,2
       SXA     OVRLZ,1
       LDI     OVIND              PRESET INDICATORS
       STI     SYSIND             AND SYSTEM INDICATORS
       CLA     FLAPCZ             CONTENT OF CELL ZERO
       STO     0                  FIX ANY GLOBERRING THAT MAT BE DONE
*                                                                       PAGE 098
OVRLRD TSX     $INPUT,4           GET OVERLORD DIRECTION CAR
               $BCDIN             FROM BCD INPUT TAPE
               OVBUF,,14          PUT IN OVERLORD CARD BUFFER
       TRA     OVERR              ERROR RETURN
       TRA     OVEOF              END OF FILE RETURN
 OVGOR LDQ     OVBUF+2            PICK UP OVERLORD DIRECTION
       CAL     OVBUF+1
       LGL     6                  SHIFT DIRECTION IN LOGICAL AC
       AXT     24,4               TWICE NUMBER OF DIRECTION CARDS
 OVSRC LAS     OVTBL,4            LOOK UP  DIRECTION
       TRA     *+2                NOT THIS ONE
       TRA     OVPNT              FOUND IT GO PRINT CARD
       TIX     OVSRC,4,2          TRY AGAIN
 OVBSW TXH     OVRLRD,,0          NOT IN TABLE, PRINT FIRST BAD CARD
       CLS     OVBSW              AND GET NEXT CARD.
       STO     OVBSW              FLIP SWITCH
       TSX     OUTPUT,4           PRINT CARD OUT
       MZE     BCDOUT             ON BCD OUTPUT TAPE, AND ON LINE
               OVBUF-1,,15
       TRA     OVRLRD             GET NEXT CARD
*
 OVERR TSX     OUTPUT,4           WRITE ERROR MESSAGE
               BCDOUT
               OVRDM,,9
       TRA     OVGOR              RY TO MAKE SENSE OUT OF CARD
*
 OVEOF TSX     OUTPUT,4           WRITE EOF REMARK
               BCDOUT
               OVALF,,7
       TRA     OVDN               GO AS IF A FIN CARD READ
*
 OVPNT CLA     OVBSW              RESTORE PRINT SWITCH TO TXH
       SLW     OVBSW
       CLA     OVTBL+1,4          PICK UP TRA ADDRESS AND SAVE IT
       STA     OVTRA
       CLA     FLAPCX             SET CELLS IN LOWER CORE
       STO     8
       CLA     FLAPCY
       STO     2
       CLA     FLAPCZ
       STO     0
       TSX     OUTPUT,4           PRINT DIRECTION CARD
       MZE     BCDOUT             ON BCD OUTPUT TAPE, AND ON ILNE
               OVBUF-1,,15
       TOV     *+1                TURN OFF AC OVERFLOW LIGHT
       LDI     SYSIND             PICK UP SYSTEM INDICATORS
       RIR     14                 RESET ERROR AND DEBIG INDICATORS
       STI     SYSIND
 OVTRA TRA     **                 EXECUTE SPECIFIC OVERLORD PROGRAM
ERRORI BOOL    10                 ERROR INDICATOR
*
*      DIRECTION CARD TABLE
       BCI     1,ONE              ** ASSUMING THIS IS THIS
       TRA     OVONE                                                    PAGE 099
       BCI     1,SET
       TRA     OVSET
       BCI     1,TST
       TRA     OVTST
       BCI     1,TEST             ** ASSUMING THIS IS THIS
       TRA     OVTST              ** ASSUMING THIS IS THIS
       BCI     1,FIN
       TRA     OVDN
       BCI     1,SIZE
       TRA     OVSZE
       BCI     1,SETSET
       TRA     OVSST
       BCI     1,DEBUG
       TRA     OVDBG
       BCI     1,TAPE
       TRA     OVTAP
       BCI     1,DUMP
       TRA     OVDMP              ** ASSUMING THIS IS THIS
       BCI     1,REMARK           ** ASSUMING THIS IS THIS
       TRA     OVRLRD
       BCI     1,EXCISE
       TRA     OVEXS
 OVSVI                            TEMPORARY STORAGE FOR INDICATORS
 OVTBL SYN     OVSVI              FOR INDEXING DIRECTION CARD TABLE
 OVIND STR                        PRESET FOR LISP INDICATORS
SYSIND                            SYSTEM INDICATORES GO HERE
 OVCEM BCI     7,0ERROR IN SIZE CARD -OVERLORD-  *O 1*
 OVNSM BCI     9,0ATTEMPT TO OPERATE BEFORE SIZE CARD READ -OVERLORD-
       BCI     1, *O 3*
 OVRDM BCI     9,0ERROR ON INPUT, BUT GOING ON ANYHOW -OVERLORD- *O  5*
 OVALF BCI     7,0END OF FILE ON INPUT -OVERLORD- *O  6*
 OVPOS         OVBUF+2,,4         BEGINNING OF VARIABLE FIELD IN DIR CDRPAGE 100
       BCI     1,0                DOUBLE SPACE PRINT OF DIRECTION CARD
 OVBUF BSS     14                 OVERLORD DIRECTION CARD BUFFER
*
*      DEBUG   OVERLORD DIRECTION
OVDBG  STR     4                  SET DEBUG INDICATOR
*                                 PREFORM OVTST
*
*
*      TEST OR TST OVERLORD DIRECTION
 OVTST RNT     20                 TEST FOR SETUP
       TRA     OVNSZ              ERROR FOR NOO SIZE CARD HAS BEEN READ
       RIR     TAPIND             RESET TAPE INDICATOR
       RFT     2                  WRITE TEST
       TSX     TAPDMP,4           DUMP ON SYSTMP
       RFT     1                  TEST FOR NEW CORE IMAGE
       TSX     OVLT,4             GET ONE
       SIR     1                  SET READ INDICATOR
       RIR     2                  TURN OFF WRITE INDICATORS
 OVTA  STI     SYSIND             UPDATE SYSTEM INDICATORS
       TSX     $EVALQ,4           PERFORM THE EVAL QUOTE OPERATOR
       TRA     OVRLRD             GET NEXT OVERLORD DIRECTION CARD
DEBUGI BOOL    4                  DEBUG INDICATOR
*
*      SETSET DIRECTION CARD
 OVSST RNT     20                 TEST FOR SIZE
       TRA     OVNSZ              ERROR, NO SIZE
       RIR     TAPIND             RESET TAPE INDICATOR
       RFT     2                  TEST FOR SAVE CORE
       TSX     TAPDMP,4           SAVE IT
       RFT     1                  TEST FOR NEW IMAGE
       TSX     OVLT,4             GET ONE
       SIR     2                  SET WRITE INDICATOR
       RIR     1                  RESET READ INDICATOR
       TRA     OVTA               PERFORM EVALQ AND GET NEXT CARD
*
*      SET     OVERLORD DIRECTION
 OVSET RNT     20                 TEST FOR SIZE
       TRA     OVNSZ              ERROR, NO SIZE CARD
       RIR     TAPIND             RESET TAPE INDICATOR
       RFT     2                  CHECK WRITE INDICATOR
       TSX     TAPDMP,4           DUMP ON SYSTMP
       RFT     1                  TEST FOR NEW CORE IMAGE
       TSX     OVLT,4             GET ONE FROM SYSTMP
       SIR     2                  SET WRITE INDICATOR
       RIR     1                  RESET READ INDICATOR
       STI     SYSIND             UPDATE SYSTEM INDICATORS
       TSX     $EVALQ,4           EVALUATE SET
       LDI     SYSIND             GET SYSTEM INDICATORS                 PAGE 101
       RNT     10                 TEST ERROR INDICATOR
       TRA     OVRLRD             OFF, GET NEXT DIRECTION CARD
       IIR     3                  ON, INVERT READ AND WRITE INDICATORS
       STI     SYSIND
       TRA     OVRLRD             GET NEXT CARD
*
*      FIN     OVERLORD DIRECTION CARD
*
 OVDN  RFT     2                  TEST WRITE INDICATOR
       TSX     TAPDMP,4           DUMP CORE ON SYSTMP
       RIR 2
       RIR     TAPIND             RESET TAPE INDICATOR
       RNT     PPTIND             SEE IF PUNCH TAPE USED
       TRA     *+2                SKIP IF NOT USED
       CLA     SYSPPT             TAPE SPEC.
       TSX     $(IOS),4           SET UP I-O COMMANDS
       XEC     $WEF               WRITE EOF ON PPT
       RIR     PPTIND             RESET INDICATORS
       STI     SYSIND             UPDATE SYSTEM INDICATORS
       CLA     SYSPOT             TAPE SPEC.
       TSX     $(IOS),4           SET UP I-O COMMANDS
       XEC     $WEF               WRITE EOF ON  SYSPOT
       LDI     OVSVI              RESTORE ORIGINAL INDICATORS AND
 OVRLX AXT     **,4               INDEX REGISTERS
 OVRLY AXT     **,2
 OVRLZ AXT     **,1
       CLA     OVTOV              PICK UP RESTART INSTRUCTION
       STO     0                  STORE IN ZERO
       PXD     0,0                LIGHT THE PANEL
       COM
       LGR     37
       COM
       SSM
       HPR     -1,7               STOP
       TRA     *-1                PRESS RESET AND START TO RESTART LISP
OVTOV  TRA     OVRLRD             TRANSFER TO GET NEXT DIRECTION CARD
PPTIND BOOL    40                 PUNCH TAPE INDICATOR
*
*      ONE     OVERLORD DIRECTION
*
 OVONE RNT     20                 TEST FOR SIZE
       TRA     OVNSZ              ERROR, NO SIZE CARD READ
       RIR     3                  RESET READ AND WRITE INDICATORS
       TRA     OVTA               SAVE INDICATORS AND DO EVAL Q
*
*      SIZE    N1,N2,N3,N4        (OVERLORD DIRECTION CARD)
*      N1 = LENGTH OF BINARY PROGRAM, N2 = LENGTH OF PUBLICH PUSH DOWN
*      LIST, N3 = LENGTH OF FULL WORD SPACE, N4 = LENGTH OF FREE STORAGE
*
 OVSZE RFT     2                  TEST FOR DUMP OF CURRENT CORE IMAGE
       TSX     TAPDMP,4           DUMP ON SYSTMP
       CLA     OVPOS              SET TO TRANSLATE NUMBERS ON SIZE CARD
       TSX     $NUMBR,4           LENGTH OF BINARY PROGRAM
       TZE     OVCER              ERROR IF ZERO                         PAGE 102
       STQ     LBINPG             SAVE NUMBER
       TSX     $NUMBR,4           LENGTH OF PUBLIC PUSH DOWN LIST
       TZE     OVCER              ZERO IS ERROR
       STO     LPBPDL             SAVE NUMBER
       TSX     $NUMBR,4           LENGTH OF FULL WORD SPACE
       TZE     OVCER              ZERO IS ERROR
       STO     LFULWS             SAVE NUMBER
       TSX     $NUMBR,4           LENGTH OF FREE STORAGE
       TZE     OVCER              ZERO IS ERROR
       STO     LFREES             SAVE NUMBER
       TSX     $SETUP,4           PERFORM SETUP
       LDI     SYSIND             SYSTEM INDICATORS
       RFT     10                 TEST FOR ERROR IN SETUP
       TRA     OVCER              YES, DO ERROR PROCEDURE
       SIR     22                 SET SIZE AND WRITE INDICATORS
       RIR     1                  RESET READ INDICATORS
       STI     SYSIND             UPDATE SYSTEM INDICATORS
       TRA     OVRLRD             GET NEXT DIRECTION CARD
*
 OVCER LDI     SYSIND             GETT SYSTEM INDICATORS
       SIR     1                  CONVERSION ERROR IN SIZE, SET READ IND
       STI     SYSIND             UPDATE SYSTEM INDICATORS
       TSX     OUTPUT,4           WRITE ERROR MESSAGE
       MZE     BCDOUT             ON BCD OUTPUT TAPE AND ONLINE
               OVCEM,,7
       TRA     OVRLRD             GET NEXT DIRECTION CARD
*
 OVNSZ TSX     OUTPUT,4           WRITE ERROR MESSAGE
       MZE     BCDOUT             ON BCD OUTPT TAPE AND ONLINE
               OVNSM,,10
       TRA     OVRLRD             GET NEXT DIRECTION CARD
*
*      DUMP    BEGINNING,END,N    (OVERLORD DIRECTION)
*              ALSO AVAILABLE TO LISP
*              BEGINNNING IS A NUMBER TO START DUMP AT, END A NUMBER
*              (MEANING OBVIOUS) AND N IS A NUMBER IF ZERO GIVES A
*              STRAIGHT OCTAL DUMP AND IF NON-ZERO GIVES A COMPLEMENT
*              (LISP TYPE) DUMP.
*
 OVDMP SXA     OVDX,4             SAVE INDEX REGISTERS
       SXA     OVDY,2
       SXA     OVDZ,1
       STZ     OVDEX              INDICATE OVERLORD ENTRANCE
       STO     OVDC               SAVE AC
       STQ     OVDQ               SAVE MQ
       STI     OVDI               SAVE SI
       CLA     OVPOS              POSITION OF VARIABLE FIELD
       TSX     $NUMBR,4           BEGGINNING OF DUMP
       TZE     ODER               ERROR IN CONVERSION
       TMI     OVENK              IF FLOATING POINT NUMBER, LOOK AT KEYS
       STQ     OBEG
       TSX     $NUMBR,4           NUMBER TO END DUMP AT
       STQ     OEND
       TZE     ODER               CONVERSION ERROR                      PAGE 103
       TSX     $NUMBR,4           TYPE OF DUMP
       STQ     OLISD
       TZE     ODER               CONVERSION ERROR
 OVGE  CLA     OEND               END DUMP NUMBER
       ADD     $Q1
       STA     OLDQ               SET ADDRESS
       SUB     OBEG               GIVES COUNT OF WORDS TO BE DUMPED
       TMI     ODER               NEGATIVE NUMBER YIELDS ERROR
       PAX     0,1                COUNT IN INDEX 1
       LXA     OBEG,4             GET BEGINNING
       TXI     *+1,4,-6           DECREMETN BY 6 (NUMBER OF WORDS / LINE)
       SXD     OBEG,4             PUT IN DECREMENT FOR OCTAL CONVERSION
       TRA     OVDSH              START THE DUMP
*
 OAXT1 CAL     OVDSF              PICK UP STAR FLAG
 OAXT  SLW     OUP+1              PUT STARS OF BLANKS IN LINE
       AXT     18,2               SET IR 2
       CLA     OBEG               BEGININNING OF LINE
       ADD     $QD6               6 WORDS PER LINE
       STD     OBEG               UPDATE LINE NUMBER
       XCA                        NUMBER TO MQ
       TSX     OCTLP,4            CONVERT TO OCTAL
       SLW     OUP                BEGIN OUTPUT LINE
 OLDQ  CAL     **,1               PICK UP WORD TO BE DUMPED
       TZE     OSTZ               EASY IF ALL ZERO
       STL     OVDZS              INDICATE SOMETHING NON-ZERO DUMPED
       ZET     OLISD              SKIP IF STRAIGHT DUMP
       TRA     OLID               DO LISP DUMP
 ODXCL XCL                        NUMBER TO MQ
       TSX     OCTLP,4            CONVERT LEFT HALF
       SLW     OUP+20,2           PUT IN OUTPUT LINE
       TSX     OCTLP,4            CONVERT RIGHT HALF
 OBQ   LDQ     BLANKS             BLANKS TO MQ
       LGR     6                  MAKE A HOLE
       ORA     OBLANK             INSERT ONE BLANK
       SLW     OUP+21,2           PUT IN OUTPUT LINE
       STQ     OUP+22,2           DITTO
       TNX     OVDFN,1,1          EXIT IF DONE
       TIX     OLDQ,2,3           LOOP 6 TIMES
       NZT     OVDZS              SKIP IF NOT ALL ZEROS
       TRA     OAXT1              GO BACK AND GET STAR FLAG FOR ZEROS
       TSX     OUTPUT,4           WRITE LINE OF DUMP
               BCDOUT             ON BCDOUT
               OUP,,20
 OVDSH STZ     OVDZS              SET SWITCH TO TEST FOR LINE OF ZEROS
       CAL     BLANKS             BLANK THE FLAG FIELD
       TRA     OAXT               GET NEXT LINE
*
 OSTZ  STZ     OUP+20,2           IF ZERO PUT ZERO S IN OUTPUT LINE
       TRA     OBQ                GO AS IF CONVERTED
*
 OLID  SLW     ODLT               LISP TYPE (COMPLEMENT DUMP)
       ANA     OLDM               MASK OUT ALL BUT TAG AND PREFIX
       TZE     ODC                TRANSFER IF LISP                      PAGE 104
       CAL     ODLT               HAS PREFIX AND/OR TAG, DUMP STRAIGHT
       TRA     ODXCL              GO TO NORMAL DUMP
 ODC   LDC     ODLT,4             COMPLEMENT DECREMENT
       SXD     ODLT,4             STORE
       LAC     ODLT,4             COMPLEMENT ADDRESS
       SXA     ODLT,4             STORE
       LDQ     ODLT               PUT IN MQ
       TSX     OCTLP,4            CONVERT LEFT HALF
       ORA     ODSAR              OR IN A *
       SLW     OUP+20,2           PUT IN OUTPUT LINE
       TSX     OCTLP,4            CONVERT RIGHT HALF
       ORA     ODSAR              PUT IN *
       TRA     OBQ                PUT AWAY AS USUAL
*
 OVDFN TNX     OVDLL,2,3          SKIP IF LINE FILLED OUT
       CAL     BLANKS             GET BLANKS IN AC
       SLW     OUP+20,2           BLANK REST OF LINE
       TIX     *-1,2,1
 OVDLL TSX     OUTPUT,4           WRITE LAST OUTPUT LINE
               BCDOUT
               OUP,,20
       TRA     OVDX               GO TO EXIT
*   FOLLOWING 6 SELLS CONTAIN AC, MQ , SI, AND IR S  UPON DUMP ENTRANCE
 OVDC                             AC CONTENTS
 OVDQ                             DITTO MQ
 OVDI                             DITTO SI
 OVDX  AXT     **,4               RESTORE INDEX REGISTERS
 OVDY  AXT     **,2
 OVDZ  AXT     *8,1
       ZET     OVDEX              TEST FOR LISP OR OVERLORD EXIT
       TRA     1,4                LISP EXIT
       ZET     OVDEK              TEST FOR ENK MODE
       TRA     OVENK              GO TO KEEYS
       TRA     OVRLRD             GO BACK FOR NEXT DIRECTION CARD
*
DUMPXX SXA     OVDX,4             LISP ENTRANCE
       SXA     OVDY,2             SAVE INDEX REGISTERS
       SXA     OVDZ,1
       STL     OVDEX              SET FOR LISP EXIT
       STD     OVDEX              SAVE ARG1
       CLA     $ARG4              PICK UP ID FOR DUMP
       TSX     $PRINT,4           PRINT IT
       LXD     OVDEX,2            ARG 1
       TSX     FIXVAL,4           EVALUATE AS FIXED POINT NUMBER
       STO     OBEG               STORE IN BEGINNING
       XCA                        ARG 2
       PDX     0,2                ARG TO INDEX 2
       TSX     FIXVAL,4           EVALUATE AS FIXED POINT NUMBER
       STO     OEND
       LXD     $ARG3,2            ARG 3
       TSX     FIXVAL,4           EVALUATE AS FIXED POINT NUMBER
       STO     OLISD
       TRA     OVGE               EXECUTE DUMP
*                                                                       PAGE 105
DUMPYY SXA     OVDX,4
       SXA     OVDY,2
       SXA     OVDZ,1
       STL     OVDEX 
       STQ     OEND
       STO     OBEG
       STZ     OLISD
       TRA     OVGE
*
 ODER  TSX     OUTPUT,4            WRITE ERROR MESSAGE
       MZE     BCDOUT             ON BCD OUTPUT TAPE AND ONLINE
               ODBAD,,6
       TRA     OVDX               RESTORE AND EXIT
 ODBAD BCI     6,0BAD DUMP ARGUMENTS -OVERLORD- *O 4*
*
 OVENK HPR     -1,7,63            STOP FOR KEYS
       ENK
       PXD     0,0                CLEAR AC
       LGL     1                  TYPE OF DUMP IN SIGN BIT
       STO     OLISD              PUT AWAY
       PXD     0,0                CLEAR AC
       LGL     17                 BEGINNING
       STO     OBEG
       PXD     0,0                CLEAR AC
       LGL     18                 END
       STO     OEND
       STO     OVDEK              SET SWITCH ON EXIT
       STO     OVDEX              SET OVERLORD EXIT
       TZE     OVDX               EXIT ON ZERO REGUEST
       TRA     OVGE               PROCESS DUMP
*
 OCTLP PXD     0,0                CONVERT LEFT HALF OF MQ TO OCTAL
       LGL     3                  CLEAR AC AND DO SHIFT DANCE
       ALS     3
       LGL     3
       ALS     3
       LGL     3
       ALS     3
       LGL     3
       ALS     3
       LGL     3
       ALS     3
       LGL     3
       TRA     1,4                EXIT
*
OCTALP SYN     OCTLP
 OVLT  SYN     OVLTXX
 OVDSF BCI     1, ****            STAR FLAG AFTER DUMPING ZEROS
 OVDZS                            SUPPRESSES OUTPUT WHEN DUMPING ZEROS  PAGE 106
 OVDEX                            ZERO FOR OVERLORD EXIT NON-ZERO , LISP
 OVDEK                            TEST CELL NON-ZERO FOR ENK MODE
 OBEG                             BEGIN DUMP
 OEND                             END DUMP
 ODSAR BCI     1,*00000           A * FOR COMPLEMENT DUMPING
 ODLT                             TEMPORARY STORAGE
 OLISD                            NON-ZERO FOR LISP TYPE DUMP
 OLDM  SVN     ,4+2+1             MASK FOR TAG AND PREFIX
 OUP   BCI     2,                 BLANKS FOR BEGINNING OF OUT PUT LINE
       BSS     18                 ROOM FOR REST OF LINE
*
*
*      TAPE    SYSXXX,A6          (OVERLORD DIRECTION CARD)
*              SYSTAP, SYSTMP, SYSPIT AND SYSPOT ARE CURRENTLY
*              RECOGINIZED LISP TAPES. UNIT DESIGNATION IS BY CHANNEL
*              (A, B, OR C) AND NUMBER (1 THRU 10).
*
 OVTAP SXA     OVTPX,4            SAVE INDEX REGISTERS
       SXA     OVTPY,2
       SXA     OVTPZ,1
       RFT     TAPIND             SKIP IF LAST CARD WAS NOT A TAPE CARD
       TRA     OVTJJ              SKIP READ AND WRITE SECTION
       RFT     2                  TEST FOR TAPE DUMP ON SYSTMP
       TSX     TAPDMP,4           DO IT
       RFT     1                  TEST FOR READ
       TSX     OVLT,4             GET NEW IMAGE
       RIR     1                  RESET READ INDICATOR
       SIR     2                  SET WRITE INDICATOR
 OVTJJ SIR     TAPIND             SET TAPE DIRECTION INDICATOR
       STI     SYSIND             UPDATE SYSTEM INDICATORS
       AXT     5,4                NUMBER OF ENTRIES IN TAPE TABLE
       CAL     OVBUF+2
       LDQ     OVBUF+3            GET TAPE DESIGNATION IN AC AND MQ
       LGL     19                 SHIFT INTO AC
       LGL     1                  DUMPING Q BIT
 OVLA  LAS     OVTTB,4            COMPARE WITH TAPE TABLE
       TRA     *+2                NOT THIS ONE
       TRA     OVTAA              THIS IS IT
       TIX     OVLA,4,1           TRY AGAIN             
 OVCMP SLW     OVTRM              NOT FOUND, COMPLAIN
       TSX     OUTPUT,4
               BCDOUT
               OVTRN,,12
       TRA     OVRLRD             GET NEXT DIRECTION CARD
 OVTRN BCI     2,0 SORRY,
 OVTRM
       BCI     9, IS NOT A VALID LISP TAPE DESIGNATION -OVERLORD- *O 2*
 OVTAA RQL     6                  DUMP THE COMMA                        PAGE 107
       PXD     0,0                CLAER AC
       LGL     6                  CHANELL LETTER IN AC
       PAI                        IN INDICATORS
       AXT     3,2                TRY CHAN. C
       RNT     3                  SKIP IF C
       AXT     2,2                TRY B
       RNT     2                  SKIP IF B OR C
       AXT     1,2                IF NO SKIP, MUST BE A
       LGL     6                  TAPE NUMBER IN AC
       ANA     $QO17              MASK OUT ALL BUT 4 LOW ORDER BITS
       LDI     OVBUF+4            NEXT WORD IN INDICATORS
       LFT     770000             KIP IF LEFT MOST CHARACTER IS A0
       TRA     *+2
       ADD     $Q9                IF LEFT MOST IS 0 ADD 9 TO THE 1
       PAX     0,1                RESULT TO INDEX 1
       TXL     *+2,1,0            ZERO UNIT DOES NOT GO
       TXL     OVTPS,1,10         UNITS OVER TEN DON T GO
       CLA     BLANKS
       LDQ     OVBUF+3
       RQL     24                 POSITION TABPE DESIG
       LGL     12
       LDQ     OVBUF+4
       LGL     6                  ALL IN AC
       TRA     OVCMP              GO COMPLAIN
 OVTPS ORA     OVTCT,4            OR IN BIN OR BCD FOR THAT TAPE
       ORA     OVCHN,2            OR IN PROPER CHANEL DESIGNATION
       SLW     TAPASG,4           CHANGE TAPE ASSIGNMENT
       LDQ     OVTTB,4            MAKE OUTPUT MESSAGE BY GETTING NAME
       STQ     OVTPO              PUT INTO MESSAGE
       TXH     OVTXX,4,1          SKIP FOLLOWING IF NOT SYSTAP
       CLA     SYSTAP
       TSX     $(IOS),4           SET UP I-O COMMANDS FOR SYSTAP
       LDQ     $TCO               MAKE PROPER SYSTEM CALL CARD
       SLQ     BOTTOM+1
       LDQ     $RDS
       STQ     GCRDB
       LDQ     $RCH
       SLQ     GCRDC
       LDQ     $LCH
       SLQ     GCRDD
       WPUA                       PUCH OUT THE 2 CARD CALLER
       RCHA    GCIOC              CHANNEL COMMANDS
 OVTXX CLA     OVCLT,2            TELL WHAT YOU HAVE DONE BY MAKING
       ORA     OVCTN,1            A MESSAGE
       STA     OVTPP
       STT     OVTPP
       TSX     OUTPUT,4           PRINT OUT THE NEW ASSIGNMENT
               BCDOUT
               OVTPP,,5
 OVTPX AXT     **,4               RESTORE INDEX REGISTERS               PAGE 108
 OVTPY AXT     **,2
 OVTPZ AXT     **,1
       TRA     OVRLRD             GET NEXT DIRECTION CARD
 OVTPP BCI     3,0  000IS NOW LISP
 OVTPO BCI     2,000000.
*      TABLES FOR OVTAP
       BCI     5,SYSPPTSYSPOTSYSPITSYSTMPSYSTAP
 OVTTB PZE                        LOW DENS, BCD PPT
       PZE                        LOW DENS, BCD PIT
       PZE                        LOW DENS, BCD POT
       PZE     16                 HI DENS, BIN TMP
       PZE     16                 HI DENS, BIN TMP
 OVTCT PZE     3*512+2*64,,3      CHANNEL C
       PZE     2*512+2*64,,2      B
       PZE     1*512+2*64,,1      A
 OVCHN BCI     3,000C00000B00000A00
 OVCLT BCI     9,00001000009 00008 00007 00006 00005 00004 00003 00002
       BCI     1,00001
 OVCTN BSS     0
*
*
* SYSTEM CALL CARD PERFORMS A LOAD TAPE SEQUENCE ON THE SYSTAP
*
 GCRD  IOCD    9,,6
       TCOA    1
       TTR     9
 GCRDB RTBA    1                  SELECT THE SYSTEM TAPE
 GCRDC RCHA    14
       STZ     1                  STOP IF TAPE DOES NOT LOAD
 GCRDD LCHA    0                  LOAD I-O COMMAND FROM TAPE
       TTR     1                  TRANSFER TO 1
       IOCT    0,,3               LOAD FIRST 3 WORDS FROM TAPE
*                                 SECOND CARD OF CALLER
 GCRDE HTR     CONTIN             BECOMES A TRANSFER CARD
*
 GCIOC IORP    GCRD,,4
       IORP    GCRDE,,1           TRANSFER CARD                         PAGE 109
       IORP    *+2,,0             2 BLANK CARDS
       IORP    *+1,,0
       IOCD    0,,0               DISCONNECT CHANNEL
*
TAPIND BOOL    100
*
*      EXCISE  DIRECTION CARD TO THROW OUT THE COMPILER AND/OR THE INTER
*              PRETER GOES HERE
*
 OVEXS TRA     OVRLRD             ROUTINE NOT WRITTEN YET.  8 APRIL 1961
*
*      EVALQ   A SUCCESSOR TO THE APPLY OPERATOR, THE GRAND NEW
*              (AS OF 1 MARCH 1961) THE EVALQUOTE OPERATOR.
*
 EVALQ SXA     EVLQX,4            SAVE LINK IR
       SXA     EVLQY,2            SAVE IR 2
       TSX     $TIME,4            PRINT TIME AND DATE
       TSX     OUTPUT,4           WRITE OPENNING MESSAGE
               BCDOUT
               EVQBM,,12
       STZ     EVQRTS             INITIALIZE TEST CELLS
       STZ     EVQB               DITTO
       AXT     EVQBL,2            LENGTH OF EVAL QUOTE BUFFER
 EVQRD SXA     EVQRX,2            SAVE INDEX 2 INCASE OF READ ERROR     
       TSX     $READ,4            READ THE INPUT LISTS
       STO     EVQAN              SAVE THE LIST
       CAS     EVQSP              COMPARE WITH STOP ATOM
       TRA     *+2                IS NOT
       TXI     EVQOP,2,1          SET IR 2 TO PROER VALUE
       ZET     EVQB               SKIP IF FIRST LIST OF DOUBLET
       TRA     EVQA               IS SECOND LIST
       STL     EVQB               FLIP SWITCH
       STO     EVQB,2             SAVE FIRST LIST OF DOUBLET IN BUFFER
       TRA     EVQRD              GET NEXT LIST
 EVQA  PDX     0,4                LIST TO INDEX
       PXD     0,4                MOVE TO ADDRESS
       STA     EVQB,2             SAVE SECOND LIST OF DOUBLET IN BUFFER
       STZ     EVQB               FLIP SWITCH
       TIX     EVQRD,2,1          GET NEXT LIST
 EVQOP SXD     EVQTH,2            INDEX VALUE OF LAST LIST READ IN
       TSX     $TIME,4            PRINT TIME
       STL     EVQRTS             SET ERROR RETURN SWITCH
       AXT     EVQBL,2            LENGTH OF BUFFER
 EVQLP SXA     EVQER,2            SAVE IDNEX VALUE
 EVQS  TSX     SPACEX,4           WRITE OUT SOME BLANK LINES
               6SPACE              3 DOUBLE SPACES
       CLA     EVQB,2             PICK UP FIRST ITEM IN BUFFER
       STZ     EVQB,2             ZERO THE BUFFER ENTRY
       STZ     $ALIST             RESET ALIST
       PDX     0,4                MAKE AN ATOM TEST
       LDQ     $ZERO
       LGR     18                 SECOND LIST INTO MQ
       PXD     0,4                FIRST LIST INTO AC
       TSX     PRINAR,4           PRINT HEADING                         PAGE 110
               2
       BCI     2, EVALQUOTE
       AXC     EVQFT,4            SET RETURN INDEX CELL
       SXA     EVQD,4
 EVQMP AXT     $APPLY,4           SET CELL OF PROGRAM TO BE EXECUTED
       SXA     EVQFT,4            INITIALIZE PROGRAM TO BE EXECUTED CELL
       STO     EVQAC              SAVE AC
       PDX     0,4                FIRST LIST TO IR 4
       CLA     0,4
       PAX     0,4
       TXH     EVQAT,4,-2         TRANSFER IF FIRST LIST IS ATOMIC
 EVQNF CLA     EVQAC              RESTORE AC
EVQZ   STZ     $ARG3              NULL ALIST FOR APPLY
 EVQD  AXT     **,4               RETURN INDEX REGISTER
 EVQFT TRA     **                 PROGRAM TO BE EXECUTED
 EVQE  STO     EVQAN              SAVE ANSWER
       TSX     OUTPUT,4           PRINT END OF EVALQUOTE MESSAGE
               BCDOUT
               EVQAM,,5
       CLA     EVQAN              PICK UP ANSWER
       TSX     $PRINT,4           PRINT IT
       STZ     EVQAN              ZERO TEMP STORAGE
 EVQER AXT     **,2               ERRORS COME BACK HERE, RESTORE IR 2
 EVQTH TXL     EVQDN,2,**         EXIT IF LAST DOUBLET EXECUTED
       TIX     EVQLP,2,1          EXECUTE NEXT DOUBLET
 EVQDN TSX     $TIME,4            ALL DONE, PRINT THE TIME
       TSX     OUTPUT,4           PRINT COLSING MESSAGE
               BCDOUT
               EVQME,,5
 EVLQX AXT     **,4               RESTORE LINK IR
 EVLQY AXT     **,2
       TRA     1,4                EXIT
*
*      EVALQT  LISP ENTRANCE TO EVALQUOTE
*
EVALQT SXA     EVQD,4             SET RETURN INDEX CELL
       TRA     EVQMP              GO TO MAIN PROGRAM
*
* ERROR RETURNS CONTROL HERE
*
EVQERR TSX     TEREAD,4           CLEAN UP READ BUFFER
       TSX     TERPRI,4           CLEAN UP PRINT BUFFER
       TSX     TERPUN,4           CLEAN UP PUNCH BUFFER
       TSX     TERPDL,4           RESET PUSH DOWN LIST
       ZET     EVQRTS             SKIP IF IN READ IN SECTION OF EVALQUOT
       TRA     EVQER              EXECUTE NEXT DOUBLET
       STL     EVQRTS             MOVE TO OPREATE SECTION OF EVALQUOTE
       TSX     OUTPUT,4           MESSAGE THAT READ WAS ERROR TERMINATED
               BCDOUT
               EVQRE,,10
       CLA     EVQAN              PICK UP LAST LIST READ IN
       TSX     $PRINT,4
 EVQRX AXT     **,2               RESTORE IR 2 TO RIGHT VALUE           PAGE 111
       TXI     EVQOP,2,1          SET IR 2 TO PROER VALUE
*
* CASE FOR ATOMIC FIRST LIST OF DOUBLET
*
 EVQAT PDX     0,4
       TXL     EVQNF,4,0          EXIT IF END OF ATOM
       CLA     0,4                NEXT WORD
       PAX     0,4                CAR OF ATOM
       STL     EVQST              SET SWITCH FOR SUBR OF EXPR
       TXL     *+2,4,$SUBR-1      LOOK FOR $SUBR
       TXL     EVQFS,4,$SUBR      TREAT AS FSUBR (ALMOST)
       TXL     *+2,4,$EXPR-1      LOOK FOR $EXPR
       TXL     EVQFX,4,$EXPR      TREAT AS FEXPR (ALMOST)
       STZ     EVQST              SET SWITCH FOR FSUBR OR FEXPR
       TXL     *+2,4,$FSUBR-1     LOOK FOR FSUBR
       TXL     EVQFS,4,$FSUBR
       TXL     EVQAT,4,$FEXPR-1   LOOK FOR FEXPR
       TXH     EVQAT,4,$FEXPR
 EVQFX PDX     0,4                FOUND AN FEXPR
       CLA     0,4
       PAX     0,4                THE EXPRESSION FOR THE FEXPR
       PXD     0,4                EXPRESSION TO AC
       ZET     EVQST              SKIP IF FEXPR
       TRA     EVQZ               GO TO APPLY CALL FOR EXPR
       STO     EVQAN              SAVE THE EXPRESSION
       STQ     EVQMQ              SAVE MQ
       PXD     0,0                CLEAR
       XCA                        MQ AND
       PXD     0,0                AC
       TSX     $CONS,4            NULL A LIST
       XCA                        INTO MQ
       CLA     EVQMQ              PUT SECOND LIST IN AC
       TSX     $CONS,4            CONS(L,A)
       XCA                        ANSWER TO ARG 2
       CLA     EVQAN              FEXPR
       TRA     EVQZ               GO TO APPLY FOR FEXPR
*
 EVQFS PDX     0,4                FOUND FSUBR, GET TXL INSTRUCTION
       CLA     0,4
       PAX     0,4
       CLA     0,4
       STA     EVQFT              SAVE ADDRESS
       PXD     0,0                ZERO
       XCA                        THE MQ AND PUT LIST IN AC
       ZET     EVQST              SKIP IF FSUBR
       TSX     SPREAD,4           SPREAD THE ARGUMENTS
       TRA     EVQD               EXECUTE THE SUBR OR FSUBR
*
 EVQAC                            TEMPORARY STORAGE
 EVQMQ                            DITTO
 EVQST                            TEST CELL IS NON-ZERO FOR SUBR OR EXPR
EVQRTS                            TEST CELL IS ZERO DURING READ IN
 EVQBL EQU     100                LENGTH OF BUFFER
 EVQSP         ,,$STOP            STOP ATOM                             PAGE 112
 EVQBM BCI     7,0EVALQUOTE OPERATOR AS OF 1 MARCH 1961.
       BCI     5, INPUT LISTS NOW BEING READ.
 EVQAM BCI     5,0END OF EVALQUOTE, VALUE IS ....
 EVQME BCI     5,1END OF EVALQUOTE OPERATOR
 EVQRE BCI     9,0READING TERMIANTED BY AN ERROR. LAST LIST READ IN IS
       BCI     1, .....
*
H      HED
*      ERRORSET(E,N,SW)
*
*      ERRORSET ATTEMPTS TO EVALUATE ITS FIRST ARGUMENT.  IF AN
*      ERROR OCCURS DURING THE EVALUATION, OR IF MORE THAN N CONS-S
*      OCCUR DURING THE EVALUATION, ERRORSET RETURNS WITH A VALUE OF F
*      AFTER RESTORING CONDITIONS TO WHAT THEY WERE BEFORE THE
*      ATTEMPTED EVALUATION.  IF THE EVLAUATION SUCCEEDS, ERRORSET
*      RETURNS LIST OF THE RESULT.  IF SW * F, ERROR DIAGNOSTICS ARE
*      SUPPRESSED, AND IF SW = T, THEY ARE INCLUDED.
*
ERRSET SXD     HORN,4
       TSX     $SAVE,4
       TXL     $END8,,HORN+9
       SXD     HORN+1,2
       SXA     HORN+1,1
       STI     HORN+4
       PDX     0,1                EXPRESSION TO BE EVALUATED
       LXD     $ARG3,4            ERROR BYPASS SWITCH
       SXA     ERNULL,4
       XCA                                                              PAGE 113
       PDX     0,2                GET CONS COUNTER LIMIT
       TSX     FIXVAL,4
       STO     HORN+6
       CLA     $CNTR1             GET CURRENT CONS COUNT
       ANA     $AMASK
       ADD     $CNTS
       SUB     HORN+6             COMPARE WITH THE LIMIT
       TMI     OBOE               TRA IF COUNTER NEED NOT BE CHANGED
       SSM                        NEG. NUMBER FOR GARBAGE COLLECTOR
       STO     HORN+5             SAVE (LIMIT - OLD COUNT)
       CLA     HORN+6             SET CONS COUNTER TO LIMIT
       STA     $CNTR1
       ANA     PDTMSK
       STO     $CNTS
       TRA     *+2
 OBOE  STZ     HORN+5             TAKE LIMIT = OLD COUNT
       LDQ     $ZERO              NULL P-LIST FOR EVALUATION
       CLS     $CPPI              SAVE PUSHDOWN  POINTER
       STO     HORN+6
       STL     TCOUNT             TURN ON CONS COUNTER
       AXT     BSOON,4            SET UP EXIT IN ERROR
       SXA     EREXIT,4
*      ATTEMPT TO PERFORM THE EVALUATION
       PXD     0,1                EXPRESSION TO BE EVALUATED
       TSX     $EVAL,4
*      WE GET HERE IF THE EVALUATION WORKED
       LDQ     $ZERO              FORM LIST OF THE RESULT
       TSX     $CONS,4
*      AN ERROR IN THIS CONS ACTS LIKE AN ERROR IN THE EVALUATION
       TRA     SHAWM              RESTORE PARAMETERS AND EXIT
*      WE GET HERE IN CASE OF ERROR
 BSOON LDC     HORN+6,4           UNSAVE ALL RECURSIVE FUNCTIONS
       SXD     TUBA,4              ENTERED SINCE THE ERROR
       TRA     TUBA-1
HARP   LXD     $CPPI,4
       CAL     -1,4
       ANA     $PMASK             TEST FOR STR FROM COMPILER
       ERA     $QP5
       TZE     *+3
       TSX     UNSAVE,4
       TRA     *+2
       TSX     C$UNWND,4
       LDC     $CPPI,4
TUBA   TXH     HARP,4,**
       PXD     0,0                RETURN VALUE OF NIL
*      RESTORE PARAMETERS FOR EITHER KIND OF EXIT
 SHAWM STO     HORN+6             SAVE EXIT VALUE
       CLA     $CNTR1             RESTORE CONS COUNTER
       ANA     $AMASK
       ADD     $CNTS
       SUB     HORN+5
       STA     $CNTR1
       ANA     PDTMSK
       STO     $CNTS                                                    PAGE 114
       LXA     HORN+1,1           RESTORE INDICATORS, IR1, AND IR2
       LXD     HORN+1,2
       LDI     HORN+4
       CLA     HORN+6             PICK UP EXIT VALUE
       TSX     UNSAVE,4           RESTORE HORN BLOCK
       LXD     HORN,4             RESTORE IR4 AND EXIT
       TRA     1,4
*      PROTECTED TEMPORARY STORAGE FOR ERRORSET
       HEAD    H
 HORN  MZE     ERSETO             (+0) ERRORSET OBJECT IN A, IR4 IN D
       MZE                        (+1) IR1 IN A, IR2 IN D
ERNULL MZE     *                  (+2) ZERO MEANS SKIP DIAGNOSTICS
EREXIT TXL     EVQERR             (+3) EXIT INSTRUCTION FOR $ERROR
       MZE                        (+4) INDICATORS
       MZE                        (+5) CONS COUNTER INCREMENT
NUBPDL MZE                        (+6) PDL BACKUP POINT IN D
TCOUNT MZE                        (+7) NON-ZERO ACTIVATES CONS COUNTER
       HEAD    0
 TERA2 SYN     EREXIT
*
       HEAD    H
*
*              EXTENDED CAR S AND CDR S FOR THE INTERPRETER
*
CAAARX SXA     CAX,4              SAVE LINK IR
       PDX     0,4
       CLA     0,4
       PAX     0,4
 AA    CLA     0,4
       PAX     0,4
 A     CLA     0,4
       PAX     0,4
       PXD     0,4
 CAX   AXT     **,4               RESTORE LINK IR
       TRA     1,4                EXIT
*
CAADRX SXA     CAX,4
       PDX     0,4
       CLA     0,4
 AAX   PDX     0,4
       TRA     AA
*
CADARX SXA     CAX,4
       PDX     0,4
       CLA     0,4
       PAX     0,4
 AD    CLA     0,4
       PDX     0,4
       TRA     A
CADDRX SXA     CAX,4
       PDX     0,4
       CLA     0,4
 ADX   PDX     0,4
       TRA     AD                                                       PAGE 115
*
CAARXX SXA     CAX,4
       TRA     AAX
*
CADRXX SXA     CAX,4
       TRA     ADX
*
CDAARX SXA     CDX,4
       PDX     0,4
       CLA     0,4
       PAX     0,4
 DA    CLA     0,4
       PAX     0,4
 D     CLA     0,4
       ANA     $DMASK
 CDX   AXT     **,4
       TRA     1,4
CDADRX SXA     CDX,4
       PDX     0,4
       CLA     0,4
 DAX   PDX     0,4
       TRA     DA
*
CDDARX SXA     CDX,4
       PDX     0,4
       CLA     0,4
       PAX     0,4
 DD    CLA     0,4
       PDX     0,4
       TRA     D
*
CDDDRX SXA     CDX,4
       PDX     0,4
       CLA     0,4
 DDX   PDX     0,4
       TRA     DD
*
CDARXX SXA     CDX,4
       TRA     DAX
*
CDDRXX SXA     CDX,4
       TRA     DDX
*
       HEAD    C
 GET   SXA     GETX,4             SAVE LINK IR
       STO     GETL
       CLA FCN31
       STO $ARG3
       CLA GETL
       TSX $PROP,4
       PDX 0,4
       CLA 0,4
       PAX 0,4
       PXD 0,4                                                          PAGE 116
 GETX  AXT     **,4               RESTORE  LINK IR
       TRA 1,4
 FCN31 TXL     GETX,,0
 GETL
*
* COMPAT       FUNCTIONAL ARGUMENT LINKAGE PROGRAM BETWEEN COMPILED
*              PROGRAMS AND APPLY FOR S-EXPRESSION FUNCTIONAL ARGUMENTS
*
COMPAT SXA     CX,4               SAVE INDEX REGISTERS
       SXA     CY,2
       STO     $ARG1              SAVE AC
       STQ     $ARG2              DITTO MQ
       LDQ     $ZERO              END OF ARGUMENT LIST
       CLA     1,4                ARGUMENTS FOR COMPAT
       STD     CA                 S-EXPRESSION FUNCTIONAL ARGUMENT
       PAC     0,2                COMPLEMENT NUMBER OF ARGUMENTS
 CL    TXL     CD,2,0             GO WHEN ALL DONE
       CLA     $ARG1-1,2          PICK UP ARGUMENT
       TSX     $CONS,4            CONS ON TO ARGUMENT LIST
       XCA                        LIST TO MQ
       TXI     CL,2,1             GO BACK FOR NEXT
 CD    CLA     CA                 FUNCTIONAL ARGUMENT
       STZ     $ARG3              ZERO PAIR LIST
 CX    AXT     **,4               RESTORE INDEX REGISTERS
 CY    AXT     **,2
       TXI     $APPLY,4,-1        GO TO APPLY AND ADJUST EXIT INDEX
 CA                               S-EXPRESSION GOES HERE
F      HED
*      PACK(CHAR)
*
*      PACK ADDS ANOTHER CHARACTER TO THE CHARACTER BUFFER BOFFO
*
*
 PACK  ARS     18                 GET CHARACTER CODE FROM
       SUB     HORG                 LOCATION OF OBJECT
       LGR     6                  PUT NEW CHARACTER INTO PACKED WORD
       CAL     CHARS
       TOV     *+1                SHUT OFF OVERFLOW LIGHT
       LGL     6
       TOV     B5                 IF WORD FULL, PUT IT IN BUFFER
       SLW     CHARS
       PXD     ,0                 CLEAR AC FOR EXIT
       TRA     1,4                EXIT
 B5    SXA     B1,4               SAVE IR4
 BFLOC AXT     20,4               ADDRESS HAS INDEX FOR BOFFO
       SLW     BOFFO,4            STORE FULL WORD OF CHARACTERS
       TNX     B3,4,1             IF BUFFER FULL, TRANSFER
       CLA     A1                 WHEN 1 SHIFTS PAST P BIT,
       STO     CHARS                NEW WORD HAS 6 CHARACTERS
       SXA     BFLOC,4            SAVE BUFFER INDEX
 B1    AXT     ,4                 RESTORE IR4
       PXD     ,0                 CLEAR AC FOR EXIT
       TRA     1,4                EXIT
 B3    TXL     B4,4,0             IF MORE THAN 120 CHARS, TRANSFER      PAGE 117
       SXA     BFLOC,0            SET INDEX TO SHOW BUFFER FILLED
       TRA     B6
 B4    TSX     $MKNAM,4           FORM OBJECT FOR ERROR PRINTOUT
       TSX     INTRN1,4
       SXD     $ERROR,4
       TSX     $ERROR+1,4
       BCI     1,*CH 1*           TOO MANY CHARACTERS IN PRINT NAME
 B6    CLA     SEVENS             BIT 1 IN CHARS WILL MAKE
       STO     CHARS                WORD LOOK FULL
       TRA     B1
*      PACK USES $ERROR, $EROR1, AND $Q1
       SPACE   5
*      NUMOB
*
*      NUMOB MAKES A NUMERICAL OBJECT CORRESPONDING TO THE BCD
*      CHARACTERS IN THE BUFFER BOFFO.
*
*      THIS ROUTINE HAS CORSS-REFERENCES TO THE INNARDS OF NUMBR
*
 NUMOB SXA     GV1,4              SAVE IR4
       TOV     *+1                SHUT OFF OVERFLOW LIGHT
       CAL     CHARS              SHIFT SEVENS INTO LAST PACKED WORD
       LDQ     SEVENS
       LGL     6
       TNO     *-1                DONE WHEN 1 PASSES THROUGH P BIT
       LXA     BFLOC,4            PUT LAST WORD INTO BOFFO
       SLW     BOFFO,4
       CLA     PARAM              INPUT PARAMETER FOR NUMBR IS
       TSX     NUMBR,4              BEGINNING OF BOFFO
       TZE     GV3                ERROR IF ZERO IN AC
       TPL     GV2                TRANSFER IF FIXED POINT OUTPUT
       XCA                        GET NUMBER FROM MQ
       LDQ     FLOS               FLOATING POINT SIGNAL
       TSX     $MKNO,4            FORM OBJECT
       LXA     GV1,4              RESTORE IR4
       TRA     CLEAR              RESET BOFFO AND EXIT
 GV2   PBT                        OCTAL SIGNAL IN NUMBR OUTPUT
       TRA     GV6                TRA IF NOT OCTAL
       XCA
       LDQ     $OCTD              MAKE OCTAL NUMBER
       TSX     $MKNO,4
       LXA     GV1,4
       TRA     CLEAR              RESET BOFFO AND EXIT
 GV6   XCA                        BRING THE NUMBER TO THE AC
       TMI     GV4                TEST FOR DIGITS 0 THRU 9
       CAS     $Q10
       TRA     GV4
       TRA     GV4
       ACL     HORG               FORM OBJECT DIRECTLY                  PAGE 118
       ALS     18
       LXA     GV1,4              RESTORE IR4
       TRA     CLEAR
 GV4   LDQ     FIXS               FIXED POINT SIGNAL FOR $MKNO
       TSX     $MKNO,4            FORM NUMERICAL OBJECT
 GV1   AXT     ,4                 RESTORE IR4
       TRA     CLEAR              RESET BOFFO AND EXIT
 GV3   TSX     OUTPUT,4
               BCDOUT
               GVA,,4
*      BCI     1,*CH 2*           FLOATING POINT NUMBER OUT OF RANGE
       PXD     0,0
       TRA     GV1
 GVA   BCI     4, ERROR NUMBER *CH 2*
*
*      THIS ROUTINE USES $CONS, $MKNO,$ZERO,$ERROR, AD $EROR1
       SPACE   5
*      MKNAM AND CLEARBUFF
*
*
*      CLEARBUFF STARTS AT CLEAR AND RESETS THE BUFFER BOFFO TO
*      THE BEGINNING
*
*      MKNAM() HAS AS OUTPUT A PNAME LIST STRUCTURE CORRESPONDING
*      TO THE CHARACTERS IN THE BUFFER BOFFO.  THE BEGINNING OF
*      BOFFO IS RESET.
*
*      THIS ROUTINE HAS CROSS-REFERENCES TO THE INNARDS OF PACK.
*
 MKNAM SXA     BB1,4              SAVE IR4
       SXA     BBIR2,2            SAVE IR2
       CAL     CHARS              IF C(CHARS) = 1, CHARS CONTAINS
       LAS     A1                   NO SIGNIFICANT CHARACTERS
       TRA     BB5
       PXD     ,0
       TRA     BB2                  NO SIGNIFICANT CHARACTERS IN CHARS
 BB5   TOV     *+1                SHUT OFF OVERFLOW LIGHT
       LDQ     SEVNS              SHIFT SEVENS INTO LAST WORD
       LGL     6                    OF LIST
       TNO     *-1
       SLW     T1                 PUT P BIT INTO SIGN
       CLA     T1
       TSX     $CONSW,4           FORM POINTER TO LAST WORD OF LIST
       LDQ     ZERO
       TSX     $CONS,4
 BB2   LXA     BFLOC,2            LOC OF LAST SIGNIFICAN BUFFER WORD    PAGE 119
 BB4   TXH     BBIR2,2,19         TRA IF BUFFER IS EXHAUSTED
       SLW     BBPNT              SAVE DECREMENT FOR FUTURE USE
       CLA     BOFFO-1,2          GET NEXT WORD OF BUFFER
       TSX     $CONSW,4
       LDQ     BBPNT
       TSX     $CONS,4
       TXI     BB4,2,1            MOVE TO NEXT WORD OF BUFFER
 BBIR2 AXT     **,2               RESTORE IR2
       TRA     BB3                RESET POSITION IN BOFFO
 CLEAR SXA     BB1,4              ENTRANCE FOR CLEARING BUFFER
 BB3   LDQ     A1                 RESET CHARS CELL TO 0 CHARACTERS
       STO     CHARS
       AXT     20,4               SET INDEX IN PACK FOR FIRST
       SXA     BFLOC,4              BUFFER WORD
       STZ     BBPNT              AVOID UNNECESSARY GARBAGE COLL.
 BB1   AXT     ,4                 RESTORE IR4
       TRA     1,4                EXIT
       SPACE   5
*      ADVANCE, STARTREAD, AND ENDREAD PROGRAMS
*
*      ADVANCE SETS CURCHAR TO THE NEXT CHARACTER
*      STARTREAD READS A NEW RECORD
*      ENDREAD MOVES TO THE END OF THE CURRENT RECORD AND
*        GIVES ERROR OUTPUT, IF ANNY
       REM
ADVANC SXD PORK,4                 SAVE IR
       LXD CHPOS,4                FIND NO. OF CHARS. LEFT IN PACKED
       TIX CHOPS,4,6                WORD
       LXD WDNUM,4                FIND NEW PACKED WORD
       TIX LAMB,4,1               IF NEW RECORD NEEDED, CONTINUE
       NZT EORTS                  IF NONZERO GIVE EOR AS OUTPUT CHAR-
       TRA VEAL                     ACTER, OTHERWISE READ NEW RECORD
PORK   TXI STEW,,0                READ A NEW RECORD
STREAD SXD PORK,4                 SAVE IR4
       TRA *+3
 VEAL  NZT ERSIG
       TRA JOYCE
       STZ ERSIG                  TURN OFF ERROR SIGNAL
       AXT 12,4                   PUT BLANKS IN ERROR BUFFER
       CAL BLANKS
 RUTH  SLW ERBFL,4
       SLW ERBFU,4
       TIX RUTH,4,1
 JOYCE TSX $INPUT,4               READ A NEW RECORD
           $BCDIN
           BUFF-12,,14
CHPOS  TXI RIBS,,0                ERROR RETURN
WDNUM  TXI RUMP,,0                EOF RETURN
       STL EORTS                  SET SIGNAL FOR EOR OUTPUT NEXT TIME   PAGE 120
       STZ $CHACT                 INITIALIZE CHARACTER COUNT
       AXT 12,4                   SET INDEX FOR START OF INPUT BUFFER
LAMB   SXD WDNUM,4
       CLA BUFF,4                 PICK UP NEW PACKED WORD FROM
       STO PWORD                    INPUT BUFFER AND STORE IT
       AXT 36,4                   INITIALIZE POSITION IN PACKED WORD
CHOPS  SXD CHPOS,4
       PXD ,0                     PICK OFF ONE CHARACTER
       LDQ PWORD
A6     LGL 6
       STQ PWORD                  SAVE SHIFTED PACKED WORD
       PAX 0,4
       TXH SHANK,4,12             CHECK FOR 8-4 MINUS
       TXL SHANK,4,11
       AXT 32,4                   CHANGE 8-4 MINUS TO 11 MINUS
 SHANK TXI *+1,4,$H00             POINTER TO NEW CHARACTER OBJECT
 BACON CLA $CHACT                 BUMP CHARACTER COUNT
       ADD $Q1
       STO $CHACT
       PXD ,4                     SET CURCHAR TO NEW CHARACTER
       SLW $CURC                  POINTER IN DECREMENT FOR BIN
       SXA $CURC1,4               POINTER IN ADDRESS FOR APVAL1
       LXD PORK,4                 RESTORE IR4
       TRA 1,4                    RETURN
RUMP   LXA EOF,4                  END OF FILE CHARACTER
       TRA JEAN
ENDRED SXD PORK,4                 SAVE IR4 FOR EXIT (ENDREAD ENTRANCE)
       SXD CHPOS,0                SET CHARACTER POSITION AND WORD
       SXD WDNUM,0                NUMBER AT END OF RECORD
STEW   NZT ERSIG                  TEST IF ERROR PRINTOUT NEEDED
       TRA SUZIE
       TSX TERPRI,4               PRINT BLANK LINE
       TSX OUTPUT,4               PRINT UPPER ERROR  BUFFER
           BCDOUT
           ERBFU-13,,13
       TSX OUTPUT,4               PRINT BAD LINE
           BCDOUT
           BUFF-13,,13
       TSX OUTPUT,4               PRINT LOWER ERROR BUFFER
           BCDOUT
           ERBFL-13,,13
       TSX TERPRI,4               PRINT BLANK LINE
 SUZIE LXA EOR,4                  LOAD END OF RECORD CHARACTER
JEAN   STZ EORTS
       TRA BACON
 RIBS  SXD $ERROR,4
       TSX $ERROR+1,4
       BCI 1,*CH 3*
*      TAPE READING ERROR -ADVANCE, STARTREAD-
 SEVNS SYN     SEVENS
       SPACE   5                                                        PAGE 121
       REM ALPHABETIC FUNCTIONS
       REM
       REM LITER(CHAR)
LITER  SXD AL1,4
       PDC 0,4
       CLA A2
AL3    SUB CHTYP-$H00,4           COMAPRE WITH TABLE ENTRY
       LXD AL1,4
       TNZ AL6
       CLA $QD1                   EXIT WITH T
       TRA 1,4
AL6    PXD ,0                     EXIT WITH F
       TRA 1,4
       REM OPCHAR(CHAR)
OPCHAR SXD AL1,4
       PDC 0,4
       CLA A3
 AL1   TXI AL3,,0
       REM DIGIT(CHAR)
DIGIT  CAS HOL9
       TRA AL5
       NOP
       CLA $QD1
       TRA 1,4
AL5    PXD ,0
       TRA 1,4
       SPACE   5
*      ERROR1
*
*      ER1 CREATES A VISUAL POINTER IN ERBFU AND ERBFL
*      TO A READING ERROR
*
 EROR1 STL     ERSIG              TURN ON ERROR SIGNAL
       SXA     ERIR,4             SAVE IR4
       CLA     $Q5                V FOR UPPER BUFFER
       LDQ     OCT41              A FOR LOWER BUFFER
       LDC     CHPOS,4            SHIFT BOTH LETTERS INTO POSITION
       LGL     -6,4
       LXD     WDNUM,4
       TXL     ERX,4,0            DO NOTHING IF END OF RECORD
       ORS     ERBFU,4            INSERT V INTO UPPER BUFFER
       XCL
       ERA     ERBFL,4            INSERT A INTO LOWER BUFFER
       SLW     ERBFL,4
 ERX   PXD     ,0
 ERIR  AXT     **,4               RESTORE IR4
       TRA     1,4                EXIT
       SPACE   5                                                        PAGE 122
*      UNPACK(NAME)
*
*      UNPACK(NAME) GIVES A LIST OF THE CHARACTER OBJECTS
*      IN THE CELL -NAME-, UP TO THE FIRST 77.
*
UNPACK SXA     UPI4,4             SAVE IR2 AND IR4
       SXA     UPI2,2
       PDX     ,4                 PUT ARGUMENT CELL IN MQ
       LDQ     0,4
       AXT     6,2
 UP2   PXD     ,0                 LOOK AT A CHARACTER
       LGL     6
       CAS     $Q63
       TXI     UP1,2,1            ADJUST IR2 FOR CHARACTER
       TXI     UP1,2,1              COUNT
       STO     T1+6,2             STORE THE CHARACTER
       TIX     UP2,2,1
       REM
 UP1   STZ     UPLST              SET END OF LIST TO NIL
 UP4   TXH     UP3,2,6            EXIT IF ALL CHARACTERS LISTED
       CLA     T1+6,2             PICK UP NEXT CHARACTER
       ADD     HORG                 AND FORN OBJECT
       ALS     18
       LDQ     UPLST
       TSX     $CONS,4            PUT CHAR AT HEAD OF LIST
       STO     UPLST
       TXI     UP4,2,1
 UP3   CLA     UPLST              RETURN WITH LOCATION OF LIST
       STZ     UPLST              AVOID UNNECESSARY GARBAGE COLL.
 UPI4  AXT     **,4
 UPI2  AXT     **,2
       TRA     1,4                EXIT
*
*      THIS ROUTINE USES $CONS
       SPACE   5
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
       REM
       REM         STORAGE
       REM
 HORG  SYN     $H00A
 EOF   SYN     $H12A
 EOR   SYN     $H72A
 HOL9  SYN     $H11D
 HOL14 SYN     $H14D
 HOL40 SYN     $H40D
       TITLE
CHTYP  DEC 1,1,1,1,1,1,1,1        0 = ILLEGAL CHARACTER
       DEC 1,1,4,3,3,0,0,0        1 = DIGIT
       DEC 3,2,2,2,2,2,2,2        2 = LETTER                            PAGE 123
       DEC 2,2,4,4,4,0,0,0        3 = OPERATION CHARACTER
       DEC 3,2,2,2,2,2,2,2        4 = OTHER
       DEC 2,2,4,4,3,0,0,0
       DEC 4,3,2,2,2,2,2,2
       DEC 2,2,4,4,4,0,0,0
       DETAIL
 ZERO  SYN $ZERO
A1     SYN CHTYP
A2     SYN CHTYP+17
A3     SYN CHTYP+16
 OCT41 SYN $Q041
 A36   SYN $Q36
 ERSIG BSS 1                      ERROR INDICATOR
       BCI     1,0                DOUBLE SPACE UNDER PROGRAMM CONTROL
 ERBFU BES     12                 UPPER ERROR BUFFER
       BCI     1,                 SINGLE SPACE UNDER PROGRAM CONTROL
 BUFF  BES     12                 BUFFER FOR INPUT RECORD
       BES     3                  ROOM FOR EXTRA WORDS IN READ-IN
       BCI     1,                 SINGLE SPACE UNDER PROGRAM CONTROL
 ERBFL BES     12                 LOWER ERROR BUFFER
PWORD  BSS 1
 PARAM PZE     BOFFO-20,,1
 FLOS  SYN     FLOATD
 T1    BSS     7
 FIXS  SYN     $FIXD
 CHARS BSS     1
 EORTS BSS 1                      NONZERO INDICATES EOR OUTPUT CHAR
 BOFFO BES     20
       BSS     1                  JUNK WORD FOR BOFFO REMNANTS
 UPLST SYN     BBPNT              CUMULATIVE LIST OF CHARACTERS
       BSS     1
CURC1  PZE                        POINTER APPEARS IN ADDRESS
CURC   PZE                        POINTER APPEARS IN DECREMENT
 CHACT PZE                        CHARACTER COUNT
*
*   MKNO   A FUNCTION OF TWO ARGUMENTS, THE FIRST IS A NUMBER, THE SECO
*          ND IS A TYPE (FLO OR FIX), MKNO FORMS A NON UNIQUE NUMBER
 MKNO  SXA     MKIR,4             SAVE LINK IR
       STQ     MKT1               TYPE OF NUMBER TO MQ
       TSX     $CONSW,4
       XCA
       CLA     $DMASK
       TSX     $CONS,4            
       LXD     MKT1,4             TYPE TO IR 4
       STD     MKT1
       CLA     $QT5               ASSUME IT IS OCTAL
       TXL     *+3,4,$FIX-1
       TXH     *+2,4,$FIX
       CLA     $QT1
       TXL     *+3,4,$FLOAT-1
       TXH     *+2,4,$FLOAT
       CLA     $QT2
       LXD     MKT1,4             LOCATION OF NUMBER
       ORS     0,4                PUT IN NUMBER FLAG                    PAGE 124
       PXD     0,4                ANSWER TO AC
 MKIR  AXT     **,4               RESTORE LINK IR
       TRA     1,4
*
*
H      HED
*      LOGOR, LOGAND, AND LOGXOR
*
*      THESE FUNCTIONS TAKE THE LOGICAL AND, LOGICAL OR, AND LOGICAL
*      EXCLUSIVE OR RESPECTIVELY OF THEIR ARGUMENTS, WHICH ARE NUMBER
*      OBJECTS.  THE RESULT IS AN OCTAL NUMBER OBJECT.
*
 LOGOR TZE     1,4                RETURN 0 IF 0 INPUT
       SXD     T1,4               SAVE IR4
       AXT     -$)PJ37,4          LOGOR ATOM
       SXA     T1,4               SET FUNCTION ON PDL
       TSX     $SAVE,4
       TXL     $END1,,T1+2        SAVE 1 ITEM
       TSX     $EVLIS,4           EVALUATE LIST OF ARGUMENTS
       TSX     UNSAVE,4
       LDQ     $ZERO              OR OF NO ARGUMENTS
       STQ     T1+1
       LDQ     ORS                INSTRUCTION FOR INNER LOOP
       TRA     LOG2
*
LOGAND TZE     1,4                EXIT WITH 0 IF 0 INPUT
       SXD     T1,4               SAVE IR4
       AXT     -$)PJ36,4          LOGAND ATOM
       SXA     T1,4               SET FUNCTION ON PDL
       TSX     $SAVE,4
       TXL     $END1,,T1+2        SAVE 1 ITEM
       TSX     $EVLIS,4           EVALUATE LIST OF ARGUMENTS
       TSX     UNSAVE,4
       LDQ     SEVENS             AND OF NO ARGUMENT
       STQ     T1+1
       LDQ     ANS                INSTRUCTION FOR INNER LOOP
       TRA     LOG2
*
LOGXOR TZE     1,4                EXIT WITH 0 IF 0 INPUT
       SXD     T1,4               SAVE IR4
       AXT     -$)PJ38,4          LOGXOR ATOM
       SXA     T1,4               SET FUNCTION ON PDL
       TSX     $SAVE,4
       TXL     $END1,,T1+2        SAVE 1 ITEM
       TSX     $EVLIS,4           EVALUATE LIST OF ARGUMENTS
       TSX     UNSAVE,4
       LDQ     $ZERO              RIGNSUM OF NO ARGUMENTS
       STQ     T1+1
       LDQ     ERS                TRA TO INSTRUCTIONS FOR INNER LOOP
*      COMMON PART OF LOGAND, LOGOR AND LOGXOR
 LOG2  STQ     LOG5
       SXA     LOG4,2             SAVE IR2
       PDX     ,2                 POINTER TO ARGUMENT LIST
*      FORM THE PROPER LOGICAL COMBINATION OF THE ARGUMENTS             PAGE 125
 LOG1  CLA     0,2                1
       PDX     0,2                CDR(L)
       PAX     0,4
       PXD     0,4                CAR(L)
       TSX     NUMVAL,4           GET NUMBER FOR THIS ELEMENT
       PXD     0,4
       CAL     0,4
 LOG5          **                 INSTRUCTION SET EARLIER
       TXH     LOG1,2,0           LOOP AGAIN IF CDR(L) NOT NULL
*      RETURN A POINTER TO THE RESULT
 LOG6  CAL     T1+1               PICK UP RESULT
       LDQ     $OCTD              MAKE AN OBJECT OF IT
       TSX     $MKNO,4
       LXD     T1,4               RESTORE IR4 AND IR2
 LOG4  AXT     **,2
       TRA     1,4
*      INSTRUCTIONS TO BE INSERTED IN INNER LOOP
 ORS   ORS     T1+1
 ANS   ANS     T1+1
 ERS   TRA     *+1                TRA SINCE ERS TAKES 2 INSTRUCTIONS
       ERA     T1+1
       SLW     T1+1
       TXH     LOG1,2,0
       TRA     LOG6
*
 T1    OCT     -0,-0              STORAGE FOR LOGAND, ETC.
*      THIS ROUTINE USES NUMVAL,$MKNO,$ZERO,AND SEVENS
       SPACE   5
*      LEFTSHIFT(X,N)
*
*      IF N IS +, X IS SHIFTED LEFT N PLACES.
*      IF N IS -, X IS SHIFTED RIGHT -N PLACES.
*      BOTH INPUTS MUST BE NUMERICAL OBJECTS.
*
LSHIFT SXA     LSH1,4             SAVE IR4
       SXA     LSH4,2             SAVE IR2
       STO     T2                 SAVE X
       XCA
       PDX     0,2                FIND VALUE OF N
       TSX     FIXVAL,4
       AXT     7*4096+7*512+1*64,4  SET UP ARS
       TMI     LSH2               IF NEGATIVE, SET UP ARS
       AXT     7*4096+6*512+7*64,4  SET UP ALS
 LSH2  SXD     LSH3,4             PUT OP CODE INTO INSTRUCTION
       STA     LSH3
       CLA     T2                 FIND VLAUE OF X
       TSX     NUMVAL,4
       PDX     0,4                                                      PAGE 126
       CAL     0,4
 LSH3  ALS     **                 THIS INSTRUCTION WAS SET UP EARLIER
       LDQ     $OCTD              FORM OCTAL NUMBER
       TSX     $MKNO,4
 LSH4  AXT     **,2               RESTORE IR2
 LSH1  AXT     **,4
       TRA     1,4
 T2    SYN     T1
*
*      THIS ROUTINE USES $MKNO,$OCTD,AND NUMVAL
Q      HED
*
*
* ARYGET       THE FUNCTION THAT GETS AND SETS THE VALUES OF ARRAYS
*              USED IN LISP AS FOLLOWS ...
*              TO GET A VALUE  (NAME,D1,D2,D3)
*              TO SET A VLUAE   (NAME,SET,VALUE,D1,D2,D3)
*
*              THE CALLING SEQUENCE IS AS FOLLOWS
*      SXA     ARYGTX,4
*      TSX     ARYGET,4
*      PZE     LOCATION OF TABLE 1,,NUMBER OF DIMENSIONS
*
ARYGET SXA     ARYY,2             SAVE INDEX REGISTER
       SXA     ARYZ,1
       STO     AGAO               SAVE ARGUMENT 1
       CLA     3,4                TABLE ZERO PARAMETER WORD
       STA     AGXEX              ADDRESS OF END OF TABLE 1
       PDX     0,2                NUMBER OF DIMENSIONS
       STQ     AGAT               ARG 2
       CLA     $ARG3
       STO     AGATH              ARGUMENT 3
       CLA     AX                 XEC INSTRUCTION
       LXD     AGAO,4             GET ARG 1
       TXL     AGN,4,$SET-1       TEST FOR SET OPERATION
       TXH     AGN,4,$SET         GO ON IF NOT $SET
       STQ     AGV                IS SET SAVE VALUE
       CLA     $ARG3
       STO     AGAO               DIMENSION 1
       CLA     $ARG4
       STO     AGAT               DIMENSION 2
       CLA     $ARG5
       STO     AGATH              DIMENSION 3
       CLA     AXS                XEC* INSTRUCTION
 AGN   STD     AGXEX              SET UP FETCH OR STORE INSTUCTION
       TXH     AGDTH,2,2          GO IF 3 D ARRAY
       TXH     AGDT,2,1           GO IF 2 D ARRAY
       LXD     AGAO,2             DIMENSION 1
       TSX     FIXVAL,4           EVALUATE THE FIXED POINT NUMBER
       PAX     0,1                INTO PROPER INDEX
       AXT     0,6                ZERO INDEX REGISTERS
 AGXE  CLA     AGV                GET THE VALUE
 AGXEX XEC     **,4               FETCH BY XEC OR STORE BY XEC*
ARYGTX AXT     **,4               RESTORE INDEX REGISTERS               PAGE 127
 ARYY  AXT     **,2
 ARYZ  AXT     **,1
       TRA     1,4
*
 AGDTH LXD     AGATH,2            DIMENSION 3
       TSX     FIXVAL,4           EVALUATE AS A FIXED POINT NUMBER
       PAX     0,1                INTO INDEX
       LXD     AGAO,2             DIMENSION 1
       TSX     FIXVAL,4           EVALUATE IT
       STA     AGR                SET UP AXT INSTRUCTION
       TRA     AGD                GO EVALUATE DIMENSUON 2
*
 AGDT  SXA     AGR,0              PRESET AXT INSTRUCTION
       LXD     AGAO,2             DIMENSION 1
       TSX     FIXVAL,4           FIXED POINT NUMBER EVALUATION
       PAX     0,1                INTO INDEX 1
 AGD   LXD     AGAT,2             DIMENSION 2
       TSX     FIXVAL,4           FIXED POINT NUMBER EVALUATION
       PAX     0,2                INTO INDEX 2
 AGR   AXT     **,4               ZERO OR DIMENSION 1
       TRA     AGXE               GO BACK TO MAIN PROGRAM
*
 AXS   XEC*                       THE STORE INSTRUCTION
 AX    XEC                        THE FETCH INSTRUCTION
 AGV                              VALUE TO BE STORED PUT HERE
 AGAO                             DIMENSION 1
 AGAT                             DIMENSION 2
 AGATH                            DIMENSION 3
*
*      FIXVAL
*
*      FIXVAL HAS AS INPUT A POINTER TO A FIXED POINT NUMBER OBJECT IN
*      IR2, AND HANDS BACK THE NUMERICAL VALUE OF THAT OBJECT.
*
FIXVAL SXA     FXVE,2             SAVE IR2 IN CASE OF ERROR
       CLA     0,2
       PAX     0,2
       TXL     FXVE,2,-2          ERROR IF NOT ATOMIC
       PDX     0,2
       ANA     $QT1
       TZE     FXVE
       CLA     0,2                PICK UP VALUE
       TRA     1,4                NORMAL EXIT
 FXVE  AXT     **,2               IR2 SHOULD LAND IN DECR. OF AC
       SXD     $ERROR,4
       PXD     0,2                IT DOES INDEED LAND THERE
       TSX     $ERROR+1,4
       BCI     1,*I  4*           BAD ARGUMENT -- FIXVAL
*
*
* ARYMAK       THE FUNCTION THAT MAKES ARRAYS
*              THE ARGUMENT IS A SINGLE LIST WHOSE SUB-LISTS HAVE THE
*              FORM  (NAME,(DIMENSION1,DIMENSION2,DIMENSION3),TYPE)
*      ARRAYS MAY BE 1, 2, OR 3 DIMENSIONAL AND MAY BE OF LIST OR       PAGE 128
*              NON-LIST TYPE.
*
*              ARRAY IS STORED AS FOLLOWS ...
*      SXA     ARTGTX,4           ADDRESS OF SUBR TXL INSTRUCTION
*      TSX     ARYGET,4
*      PZE     END + 1,, N OF DIMENSIONS  (ARRAY PROPERTY POINTS HERE)
*      PZE     TOTAL LENGTH,,LIST OF LENGTH
*      PZE     TABLE ZERO,, NUMBER OF DIMENSIONS (ARYGET PARAMETER WORD)
*      CLA*    **,2               TABLE 1
*      *************************
*      STO     **,1               TABLE 2
*      *****************************
*                                 ARRAY PROPER GOES HERE
*
ARYMAK LDQ     AMFAG              PICK UP FUNCTIONAL ARGUMENT
       TRA     MAPLIS             LET MALPIST HANDLE ITERATION ALONG LIS
*
 AMFAG TXL     *+1,,1             FUNCTIONAL ARGUMENT
       SXA     AFRX,4             SAVE INDEX REGISTERS
       SXA     AFRY,2
       PDX     0,4                POINTER TO LIST
       CLA     0,4
       PAX     0,4                POINTER TO SUBLIST
       CLA     0,4
       PAX     0,4                NAME
       SXD     AFAT,4             SAVE IT
       PDX     0,4
       CLA     0,4
       PAX     0,2                POINTER TO DIMENSION LIST
       PDX     0,4
       CLA     0,4
       PAX     0,4                TYPE
       STZ     ATYP
       TXL     ADA,4,$LIST-1      GO IF NOT $ LIST
       TXH     ADA,4,$LIST
       SXD     ATYP,4             MAKES ATYPE NON-ZERO FOR LIST ARRAYS
 ADA   CLA     0,2                FIRST WORD ON DIMENSION LIST
       PAX     0,2                DIMENSION 1
       STD     ATMP               POINTER TO REST
       TSX     FIXVAL,4           EVALUATE THE FIXED POINT NUMBER
       STO     ADO                DIMENSION 1
       LXD     ATMP,4             PICK UP POINTER TO REST OF LIST
       TXL     AOD,4,0            GO IF 1 D
       CLA     0,4                NEXT WORD
       STD     ATMP               SAVE POINTER
       PAX     0,2                DIMENSION 2
       TSX     FIXVAL,4           GET NUMBER VALUE
       STO     ADT                DIMENSION 2
       LXD     ATMP,4             POINTER TO REST OF LIST
       TXL     ATD,4,0            GO IF 2 D ARRAY
       CLA     0,4
       PAX     0,2                DIMENSION 3
       TSX     FIXVAL,4           NUMBER VALUE
       STO     ADTH               DIMENSION 3                           PAGE 129
       AXT     3,2                NUMBER OF DIMENSIONS
       TRA     AGA                GO TO NEXT PART OF PROGRAM
 AOD   CLA     ADO                1D, TREAT AS A 1 X 1 X D1 ARRAY
       STO     ADTH
       CLA     $Q1
       STO     ADT                DIMENSION 2
       STO     ADO                DIMENSION 1
       AXT     1,2                1 D ARRAY
       TRA     AGA                GO NEXT PART
 ATD   CLA     ADO                2 D, TREAT AS A 1 X D2 X D1 ARRAY
       CLA     $Q1
       STO     ADO                DIMENSION 1
       AXT     2,2                2 D ARRAY
 AGA   LDQ     ADO                DIMENSION 1
       PXD     0,0                ZERO AC
       MPY     ADT                DIMENSION 2
       STQ     ADOT               D1 X D2
       MPY     ADTH               DIMENSION 3
       ZET     ATYP               SKIP NEXT IF NON-LIST ARRAY
       STQ     ATYP               LIST LENGTH
       XCA                        D1 X D2 X D3 TO AC
       ADD     ADOT               ADD INDEX TABLE LENGTHS
       ADD     ADO
       ADD     $Q5                CONSTANT LENGTH
       STA     APWT               PARAMETER WORD TWO
       STA     ATMQ               SAVE LENGTH
       LXA     ATYP,4             ZERO OR LIST LENGTH
       SXA     APWT,4             PARAMETER WORD 2
       TSX     BLOCKR,4           RESERVE A BLOCK OF THIS LENGTH
       TZE     ARYTL              GO IF ARRAY WILL NOT FIT
       STA     ATMP               END OF BLOCK ADDRESS
       ADD     $Q1                ADD 1
       STA     APWO               PARAMETER WORD 1
       SXD     ATBZ,2             NUMBER OF DIMENSIONS
       SXD     APWO,2   
       SXD     ASBR,2
       SUB     ATMQ               LENGTH OF BLOCK
       STA     ASBR               ADDRESS OF BEGINNING OG BLOCK
       PAC     0,4                POINTER IN IR 4
       TXI     *+1,4,-2           POINTER TO ARRAY PROPERTY
       SXD     AARY,4             SAVE POINTER
       PAC     0,4                POINTER TO BEGINNING OF ARRAY
       ADD     $Q4                LENGTH OF PREFIX - 1
       ADD     ADO
       STA     ATBZ               LAST LOC. IN TAQBLE ONE
       AXT     5,2                LENGTH OF PREFIX TO ARRAY
 ACLA  CLA     ADOT,2             PICK UP PREFIX
       STO     0,4                AND STORE IN CORE
       TXI     *+1,4,-1           UPDTAEC CORE LOCATION
       TIX     ACLA,2,1       GET REST OF PREFIX
       ANA     $AMASK             TABLE ZERO IN AC
       ORA     ACLAS              OR IN CLA* INSTRUCTION
       LXA     ADO,2              LENGTH OF TABLE
 AADD  ADD     ADT                INCREMENT BY DIMENSION 2              PAGE 130
       STO     0,4                PUT IN CODE
       TXI     *+1,4,-1           UP DATE CORE COUNTER
       TIX     AADD,2,1           FINISH OFFF
       LXA     ADOT,2             LENGTH OF TABLE 2
       ANA     $AMASK             CLEAR OUT ALL BUT ADDRESS
       ORA     ARSTO              PUT INSTRUCTION
 AAA   ADD     ADTH               ADD DIMENSION 3
       STO     0,4                PUT IN CORE
       TXI     *+1,4,-1           UPDATE CORE COUNTER
       TIX     AAA,2,1            CONTINUE TO CONSTRUCT TABLE
*              TABLE CONSTRUCTION ALL DONE.
*              THE FOLLOWING ADDS PROPERTYS TO THE ARYATOM
       CLA     AARY               PICK UP POINTER TO TO ARRAY PROPERTY
       LDQ     $ZERO
       TSX     $CONS,4
       LDQ     $ZERO
       TSX     $CONS,4
       XCA
       CLA     ARY                POINTER TO ATOMIC SYMBOL ARRAY
       TSX     $CONS,4            (ARRAY,(POINTER TO ARRAY PROPERTY))
       STO     ATMP               SAVE IN TEMP STORAGE
       CLA     ASBR               TXL INSTRUCTIONM
       TSX     $CONSW,4           PUT IN FULL WORD SPACE
       LDQ     ATMP               REST OF PROPERTIES
       TSX     $CONS,4
       XCA
       CLA     ASB                POINTER TO $SUBR ATOMIC SYMBOL
       TSX     $CONS,4
       XCA                        SAVE IN MQ
       LXD     AFAT,4             POINTER TO NAME
       CLA     0,4                FIRST WORD
       PXD     0,4                SAVE POINTER TO REST
       PXD     0,4                PUT IN AC
       XCA                        INTER CHANGE AC AND MQ
       TSX     $NCONC,4           SPLICE  2 LISTS TOGETHER
       LXD     AFAT,4             POINTER TO FIRST WORD ON PROPERTY LIST
       STD     0,4                REPLACE DECREMENT OPERATION
       PXD     0,4                POINTER TO ARRY ATOM
       LDQ     ARYLIS             PICK UP ARRAY LIST
       TSX     $CONS,4            PUT ON AS ACTIVE ARRAY
       STD     ARYLIS             UPDATE ARRAY LIST
       CLA     AFAT               FINAL ANSWER
 AFRX  AXT     **,4               RESTORE INDEX REGISTERS
 AFRY  AXT     **,2
       TRA     1,4                EXIT
*
 ARYTL SXD     $ERROR,4           SAVE INDEX 4
       LXA     AFRY,2             RESTORE INDEX 2
       CLA     AFAT               ARRAY NAME
       TSX     $ERROR+1,4         GO TO ERROR
       BCI     1,*I  1*           NOT ENOUGH ROOM FOR ARRAY
*              CONSTANTS AND STORAGE
       SXA     ARYGTX,4           5 WORD PREFIX TO ARRAYS
       TSX     ARYGET,4                                                 PAGE 131
 APWO                             END+1,,N OF D
 APWT                             LENGTH,,LIST LENGTH
 ATBZ                             TABLE ZERO,, N OF D
 ADOT                             D1 X D2
 ATYP                             ZERO OR LIST LENGTH
 ATMQ                             TEMPORARY STORAGE
 ADO                              D1
 ADT                              D2
 ADTH                             D3
 ASBR  TXL     **,**
 AARY                             POINTER TO ARRAY PROPERTY
 ACLAS CLA*    **,2               FETCH INSTRUCTION
 ARSTO STO     **,1               PUT INSTRUCTION
 ARY           ,,$ARRAY
 ASB   SYN     $SUBRD
*
*
* UNUMIX       EVALUATES ITS 2 NUMERICAL ARGUMENTS AND FLOATS THE FIXED
*              POINT ARGUMENT IF A MIXED EXPRESSION.  THE NUMERICAL
*              VALUES ARE LEFT IN AC AND MQ WITH TYPE OF NUMBER IN $ARG3
*
UNUMIX SXA     UNUX,4             SAVE LINK IR
       STQ     UNUT               SAVE SECOND ARGUMENT
       TSX     NUMVAL,4           NUMERICALLY EVALUATE THE FIRST ARG
       PDX     0,4                POINTER TO FULL WORD
       CLA     0,4                NUMERICAL VALUE
       STO     UNUS               SAVE IT
       STQ     UNUR               SAVE TYPE OF NUMBER
       CLA     UNUT               PICK UP SECOND ARG
       TSX     NUMVAL,4           NUMERICALLY EVALUATE IT
       PDX     0,4                POINTER TO FULL WORD
       CLA     0,4                NUMERICAL VALUE
       XCA                        VLUE TO MQ, TYPE TO AC
       SUB     UNUR               COMPARE WITH TYPE OF FIRST
       TNZ     UNMXA              TRA IF NOT SAME
 UNUE  CLA     UNUR               PICK UP NUMBER TYPE
       STO     $ARG3
       CLA     UNUS               PICK UP FIRST NUMERICAL VALUE
 UNUX  AXT     **,4               RESTORE LINK IR
       TRA     1,4                EXIT
*
 UNMXA STQ     UNUT               MIXED TYPES, SAVE SECOND VALUE
       SXA     UNUX2,2            SAVE IR 2
       LXD     UNUR,2             PICK UP TYPE OF FIRST NUMBER
       TSX     FIXFLO,4           DISPATCH
       NOP                        IMPOSSIBLE RETURN
       TRA     UNMXB              FLOAT SECOND NUMBER
       CLA     UNUS               FIRST NUMBER
       TSX     $UNFIX,4           FLOAT IT
       LDQ     UNFLT              $FLOAT FOR TYPE
       STQ     $ARG3
       LDQ     UNUT               SECOND NUMBER
 UNUX2 AXT     **,2               RESTORE IR 2
       TRA     UNUX               RESTRE LINK AND EXIT                  PAGE 132
*
 UNMXB XCA                        FLOAT SECOND NUMBER
       TSX     $UNFIX,4           FLOAT FUNCTION
       XCA                        BACK TO MQ
       LXA     UNUX2,2            RESTORE IR 2
       TRA     UNUE               GET FIRST NUMBER, RESTORE LINK + EXIT
 UNUS                             FIRST NUMERICAL VALUE
 UNUT                             SECOND ARG AND VALUE
 UNUR                             TYPE OF FIRST ARG
UNFLT  SYN     FLOATD             FLOAT INDICATOR
*
* THIS ROUTINE USES NUMVAL,$UNFIX,FIXFLO, AND $ARG3  + $FLOAT
*
*
* DIVIDE       DIVIDES THE FIRST NUMERICAL ARGUMENT BY THE SECOND. THE
*              ANSWER IS A LIST OF THE QUOTIENT AND THE REMAINDER.
*
* QUOTEN       GIVES THE QUOTIENT WHEN THE FIRST NUMERICAL ARGUMENT IS
*              DIVIDED BY THE SECOND.
*
* REMAIN       GIVES THE REMAINDER WHEN THE FIRST NUMERICAL ARGUMENT IS
*              DIVIDED BY THE SECOND.
DIVIDE STI     DIVND              SAVE INDICATORS
       RIR     3                   DIIDE INDICATE
       TRA     DIVOP              DO OPERATION
*
REMAIN STI     DIVND              SAVE INDICATORS
       RIR     3                  DIVIDE INDICATE
       SIR     2                  SET REMAINDER INDICATOR
       TRA     DIVOP              DO OPERATION
*
QUOTEN STI     DIVND              SAVE INDICATORS
       RIR     3                  DIVIDE INDICATE
       SIR     1                  QUOTIENT INDICATOR
 DIVOP SXA     DIVX,4             SAVE LINK IR
       SXA     DIVX2,2            SAVE IR 2
       TSX     UNUMIX,4           NUMERICALLY EVALUATE THE ARGUMENTS
       LXD     $ARG3,2            PICK UP TYPE
       STQ     DIVT               SECOND ARGUMENT
       TSX     FIXFLO,4           DISPATCH ON TYPE
       NOP                        IMPOSSIBLE RETURN
       FDP     DIVT               FLOATING DIVIDE
       TRA     DIVFX              DO FIXED POINT DIVIDE
 DIVDC DCT                        CHECK  FOR ILLEGAL DIVISION
       TSX     $DCT,4             DIVIDE CHECK ERROR
       RFT     1                  SEE IF REMAINDER IS TO BE SAVED
       TRA     DIVA               NO, SET UP QUOTIENT
       STQ     DIVT               YES, SAVE QUOTEINT
       LDA     $ARG3              PICK UP TYPE
       TSX     $MKNO,4            MAKE REMAINDER A NUMBER
       RFT     2                  SEST TO SEE IF QUOTIENT IS WANTED
       TRA     DIVEX              NO, RESTORE AND EXIT
       LDQ     $ZERO              NIL IN MQ
       TSX     $CONS,4            LIST OF REMAINDER                     PAGE 133
       XCA                        SHUTTLE INTO MQ
       CLA     DIVT               PICK UP QUOTIENT
       STQ     DIVT               SAVE LIST OF REMAINDER
       LDQ     $ARG3              PICK UP TYPE
       TSX     $MKNO,4            MAKE QUOTIENT A NUMBER
       LDQ     DIVT               LIST(REMAINDER)
       TSX     $CONS,4            LIST(QUOTIENT,REMAINDER)
       LXA     DIVX,4             RESTORE LINK IR
       LXA     DIVX2,2            RESTORE IR 2
       LDI     DIVND              RESTORE INDICATORS
       TRA     1,4                EXIT
*
 DIVFX XCA                        FIXED POINT DIVISION. PUT ARG 1 IN MQ
       PXD     0,0                CLEAR AC
       LLS     0                  MQ SIGN TO AC
       DVP     DIVT               DIVIDE BY ARG 2
       TRA     DIVDC              PREFORM DIVIDE CHECK AND CARRY ON
 DIVA  XCA                        QUOTIENT TO AC
       LDQ     $ARG3              TYPE TO MQ
 DIVX2 AXT     **,2               RESTORE IR 2
 DIVX  AXT     **,4               RESTORE LINK IR
       LDI     DIVND              RESTORE INDICATORS
       TRA     $MKNO
*
 DIVEX LXA     DIVX2,2            EXIT ROUTINE, RESTORE IR 2
       LXA     DIVX,4             RESTORE LINK IR
       LDI     DIVND              RESTORE INDICATORS
       TRA     1,4
*
 DIVND                            INDICATORS STORAGE
 DIVT                             LIST AND NON-LIST TEMPORARY STORAGE
*
* THIS ROUTINE USES $MKNO,$DCT,$CONS,$ARG3 AND UNUMIX
*
*
* DIFFER       COMPUTES THE DIFFERENCE BETWEEN ITS 2 NUMERICAL ARGUMENTS
*
DIFFER SXA     DIFX,4             SAVE LINK IR
       SXA     DIFX2,2            SAVE IR 2
       TSX     UNUMIX,4           NUMERICALLY EVALUATE THE ARGUMENTS
       LXD     $ARG3,2            PICK UP TYPE OF NUMBERS
       STQ     DIFT               STORE SECOND NUMBER
       TSX     FIXFLO,4           DISPATCH ON TYPE
       NOP                        IMPOSSIBLE RETURN
       FSB     DIFT               FLOATING POINT
       SUB     DIFT               FIXED POINT
       LDQ     $ARG3              TYPE OF NUMBER
 DIFX2 AXT     **,2               RESTORE IR 2
 DIFX  AXT     **,4               RESTORE LINK IR
       TRA     $MKNO              MAKE RESULT A NUMBER
*
 DIFT                             TEMPORARY STORAGE
*
* THIS ROUTINE USES UNUMIX,FIXFLO,$ARG3 AND $MKNO                       PAGE 134
*
*
* EXPT TAKES 2 FIXED OR FLOATING POINT NUMBERS AS ARGUMENTS AND RAISES
*              THE FIRST TO THE POWER INDICATED BY THE SECOND.
*
 EXPT  SXA     EXPX,4             SAVE LINK IR
       SXA     EXPY,2             SAVE IR 2
       TSX     UNUMIX,4           EVALUATE THE 2 ARGUMENTS AS NUMBERS
       LXD     $ARG3,2            PICK UP TYPE OF NUMBERS
       TSX     FIXFLO,4           DISPATCH ON FIX OR FLOAT
       NOP                        IMPOSSIBLE RETURN
       TRA     EXPA               IS FLOATING POINT
       TPL     EXPB
 EXPC  LXA     EXPY,2             RESTORE IR 2
       LXA     EXPX,4             RESTORE IR 4
       SXD     $ERROR,4           SAVE IN $ERROR
       PXD     0,0                CLEAR AC
       TSX     $ERROR+1,4         GO TO ERROR
       BCI     1,*I  2*           FIRST ARGUMENT IS NEGATIVE -EXPT-
 EXPB  XCA                        INTERCHANGED FIXED POINT ARGUMENTS.
       STQ     COMMON             TEMPORARY STORAGE
       PAX     0,4                EXPONENT
       TXL     OUT,4,0            GO IF ZERO POWER
       TNX     OUT1,4,1           GO IF TO FIRST POWER
       PXD     0,0                CLEAR AC
       MPY     COMMON             RAISE TO GIVEN POWER
       TIX     *-1,4,1            IN LOOP
 OUT1  XCA                        ANSWER TO AC
       LDQ     $FIXD              $FIX TO DECREMENT
       TRA     EXPX               RESTORE INDEX REGISYERS AND MAKE NUMBR
 OUT   CLA     $Q1                ANSWER IS 1
       LDQ     $FIXD              $FIX TO MQ
       TRA     EXPX               EXIT
 EXPA  TMI     EXPC
       TSX     $POWR,4            POWER ROUTINE
       LDQ     FLOATD             $FLOAT TO MQ
 EXPX  AXT     **,4               RESTORE INDEX REGISTERS
 EXPY  AXT     **,2
       TRA     $MKNO              MAKE ANSWER AN NUMBER
       REM POWER
G      HED
 POWR  STQ N
       SXD COMMON,1
       SXD COMMON+1,2
 P19   LXA ZERO,1
       LXA ZERO,2
       LRS 27
       SUB L200
       STQ FN
       LDQ ZERO
       TNZ P01
       STO E
       TRA P02
 P01   LRS 1                                                            PAGE 135
       TXI P03,1,1
 P03   TNZ P01
       PXD 0,1
       ARS 18
       SSP
       ADD L200
       LLS 27
       STO E
       CLM
 P02   LDQ FN
       LLS 27
       ADD LL200
       FAD RSQ
       STO P04
       FSB SQ
       FDH P04
       STQ P05
       FMP P05
       STO P06
 P08   LDQ P06
       FMP C7,2
       FAD C5,2
       STO C5,2
       TXI P07,2,1
 P07   TXL P08,2,2
       LDQ C1
       FMP P05
       FSB R2
       FAD E
 P18   STO E
       LDQ N
       FMP E
       STO N
       TRA P09
 M1
 M2
 M3        1
 M4        0,0,256
 P41   LXA ZERO,1
 P11   LDQ W
       FMP A6,1
       FAD A5,1
       STO A5,1
       TXI P10,1,1
 P10   TXL P11,1,5
       STO W
       LXA ZERO,2
 P13   CLA AP6,2
       STO A6,2
       TXI P12,2,1
 P12   TXL P13,2,6
       LXA ZERO,1
 P15   CLA CP7,1
       STO C7,1                                                         PAGE 136
       TXI P14,1,1
 P14   TXL P15,1,3
       LDQ W
       FMP W
       STO W
       LDQ W
       FMP W
       STO W
       TRA P16
 EA
 P171  TRA P17
 P24
 P21   STO E
 S1
 S2
 N
 ZERO  SYN $ZERO
 L200  SYN $QO200
 FN
 E
 LL200 SYN QO2Q11
 RSQ   OCT +200552023632
 P04
 P05
 P06
 LOG   OCT +200542710300
 S3
 W
 SQ    OCT +201552023632
 R2    OCT +200400000000
 C1    OCT +202561250731
       OCT +200754342231
 C5    OCT +200447154100
 C7    OCT +177674535132
       OCT +202561250731
       OCT +200754342231
       OCT +200447154100
 CP7   OCT +177674535132
       OCT +201400000000
       OCT +176777776476
       OCT +174400037635
       OCT +170523517764
       OCT +164547625227
 A5    OCT +157554324201
 A6    OCT +154562606535
 L1    OCT +201400000000
       OCT +176777776476
       OCT +174400037635
       OCT +170523517764
       OCT +164547625227
       OCT +157554324201
 AP6   OCT +154562606535
 P16   STO EW
       CLA P171                                                         PAGE 137
       STO P17
       CLA EQ
       TRA P19
 P17   TRA P20
 P42   TRA P21
       STO P18
       LXA ZERO,1
 P23   CLA CP7,1
       STO C7,1
       TXI P22,1,1
 P22   TXL P23,1,3
       LDQ P24
       FMP LOG
       FSB S3
       TOV P25
       STO S1
       LDQ S1
       FMP R2
       FSB L1
       STO S2
       LDQ S1
       FMP S2
       FAD L1
       STO S1
       LDQ EW
       FMP S1
       STO EW
 P25   LDQ M1
       FMP EW
       STO EW
       CLA N
       TPL P26
       CLA L1
       TRA P27
 EW
 P09   LDQ ZERO
       SSP
       LRS 27
       SUB L200
       TRA P28
 P40   CLA ZERO
P39    LLS     **
       ADD L200
       ADD M3
       ALS 27
       ADD M4
       STO M1
       STO M2
       CLA M2
       TNZ P29
       CLA N
       TPL P30
       CLA L1
       FDH M1                                                           PAGE 138
       STQ M1
 P30   LXA ZERO,1
 P32   CLA CP7,1
       STO C7,1
       TXI P31,1,1
 P31   TXL P32,1,3
       CLA M1
       LXD COMMON,1
       LXD COMMON+1,2
       TRA 1,4
 P29   CLM
       LXA ZERO,2
 P34   LLS 1
       TXI P33,2,1
 P33   TZE P34
       LRS 1
       PXD 0,2
       ARS 18
       SSM
       ADD L200
       ADD M3
       LLS 27
 P36   STO M2
       LDQ LOG
       FMP M2
       STO W
       TRA P35
 P37   CLA L1
       STO M1
       CLA N
       SSP
       TRA P36
 P28   TZE P37
       TMI P37
       TRA P38
 P27   FDH EW
       STQ EW
 P26   CLA EW
       LXD COMMON,1
       LXD COMMON+1,2
       TRA 1,4
 P38   STA P39
       TRA P40
 P35   STO S3
       TRA 41
 P20   SSP
       STO P24
       TRA P42
       HEAD    Q
* ADD  ADDS A STRING OF FIXED POINT OR FLOATING POINT NUMBERS
 ADDP  SXD     AMIR,4             SAVE LINK IR
       AXT     $PLUS,4
       STI     AMIND              SAVE INDICATORS
       RIR     177                RESET FIRST 7 INDICATORS              PAGE 139
       SIR     1                  SET ADD INDICATOR (1)
       TRA     AMMMF              GO TO MAIN FUNCTION
*
 MULT  SXD     AMIR,4             SAVE LINK IR
       AXT     $TIMES,4
       STI     AMIND              SAVE INDICATORS
       RIR     177                RESET FIRST 7 INDICATORS
       SIR     2                  SET MULTIPLY INDICATOR (2)
       TRA     AMMMF              GO TO MAIN FUNCTION
*
 MIN   SXD     AMIR,4             SAVE LINK IR
       AXT     $MINP,4
       STI     AMIND              SAVE INDICATORS
       RIR     177                RESET FIRST 7 INDICATORS
       SIR     10                 SET MINIMUM INDICATOR (10)
       TRA     AMMMF              GO TO MAIN FUNCTION
*
 MAX   SXD     AMIR,4             SAVE LINK IR
       AXT     $MAXP,4
       STI     AMIND              SAVE INDICATORS
       RIR     177                RESET FIRST 7 INDICATORS
       SIR     4                  SET MAXIMUM INDICATOR (4)
 AMMMF SXA     AMIR,4             PUT PROGRAM NAME WITH LINK IR
       TSX     $SAVE,4            OTHER 3 FUNCTIONS ENTER AT *-1
       TXL     $END2,,AMIND+2     SAVE 2 ITEMS
       TSX     $EVLIS,4           EVALUATE THE LIST OF ARGUMENTS
       TSX     UNSAVE,4           RESTORE IR 4 AND INDICATORS
       SXA     AMIR2,2            SAVE IR 2
       STZ     AMSUM              ZERO FINAL ANSWER REGISTER
 AMLP  PDX     0,4                PUT POINTER TO ARG LIST IN IR 4
       TXL     AMEND,4,0          GO TO EXIT IF NULL
       CLA     0,4                GET FIRST WORD
       STO     AMLIS              SAVE THE WORD
       PAX     0,4                CAR OF LIST
       PXD     0,4                TO DECREMENT
       TSX     NUMVAL,4           EVALUATE THE ITEM
       STQ     AMQ                SAVE CHARACTERISTIC ($FIX OR $FLOAT)
       RNT     100                TEST FOR FIRST TIME THROUGH
       TRA     AMFRS              IS FIRST TIME GO TO INITIALIZE AMSUM
       RFT     2                  TEST FOR MULT FUNCTION
       TRA     AMLT               EXECUTE MULT FUNCTION
       PXD     0,4                POINTER TO FULL WORD
       CLA     0,4                GET NUMERICAL VALUE
       RNT     1                  SKIP NEXT INSTRUCTION IF ADD FUNCTION
       TRA     AMM                EXECUTE MAX OR MIN FUNCTION
       LXD     AMQ,2              ADD FUNCTION. PICK UP TYPE OF NUMBER
       TSX     FIXFLO,4           TEST FOR FIX OR FLOAT
       NOP                        IMPOSSIBLE RETURN
       TRA     AFLL               EXECUTE FAD
       SIR     20                 IS FIXED POINT. SET FIXED POINT IND.
       AXC     AFLR,4             PRESET IR 4
       RFT     40                 SKIP NEXT INSTRUCTION IF NOT MIXED EXP
       TRA     UNFX               IS MIXED, FLOAT THIS NUMBER
       ADD     AMSUM              FIXED ADD OF SUM                      PAGE 140
 AMRT  STO     AMSUM              STORE NEW SUM
       CLA     AMLIS              PICK UP ARG LIST
       TRA     AMLP               DO NEXT ITEM
 AMFRS PDX     0,4                POINTER TO FULL WORD
       CLA     0,4                GET NUMERICAL VALUE
       STO     AMSUM              STORE NUMERICAL VALUE IN AMSUM
       LXD     AMQ,2              PICK UP TYPE OF NUMBER
       TSX     FIXFLO,4           TEST FOR FIX OR FLOAT
       NOP                        IMPOSSIBLE EXIT
       SIR     40                 SET FLOAT INDICATOR
       SIR     20                 SET FIX INDICATOR
       SIR     100                SET INDICATOR SO IT WILL NOT GET BACK
       CLA     AMLIS              PICK UP REST OF ARG LIST
       TRA     AMLP               DO NEXT ITEM
 AFLL  SIR     40                 IS FLOATING POINT, SET PROPER INDICATO
       RFT     20                 SKIP NEXT INSTRUCTION IF NOT MIXED EXP
 AFLR  TSX     MIXFL,4            UNMIX THE EXPRESSION
       FAD     AMSUM              FLOATING ADD THE CURRENT SUM
       TRA     AMRT               STORE AND DO NEXT ITEM ON LIST
 AMLT  PDX     0,4                POINTER TO FULL WORD
       CLA     0,4                GET NUMERICAL VALUE
       LXD     AMQ,2              PICK UP TYPE
       TSX     FIXFLO,4           TEST FOR FIX OR FLOAT
       NOP                        IMPOSSIBLE RETURN
       TRA     AFMP               DO FMP
       SIR     20                 SET FIXED POINT INDICATOR
       AXC     AFLT,4             PRESET IR 4
       RFT     40                 SKIP NEXT INSTRUCTION IF NOT MIXED EXP
       TRA     UNFX               IS MIXED, FLOAT THIS NUMBER
       XCA                        NUMBER TO MQ
       MPY     AMSUM              MPY BY CURRENT ANSWER
       XCA                        PUT LEAST SIGNIFICANT DIGITS IN AC
 AMRU  STO     AMSUM              STORE NEW ANSWER
       CLA     AMLIS              PICK UP ARG LIST
       TRA     AMLP               DO NEXT ITEM
 AFMP  SIR     40                 SET FLOATING POINT INDICATOR
       RFT     20                 TEST FOR MIXED EXP
 AFLT  TSX     MIXFL,4            UNMIX THE EXPRESSION
       XCA                        NUMBER TO MQ
       FMP     AMSUM              FMP BY CURRENT ANSWER
       TRA     AMRU               STORE NEW ANSER AND DO NEXT ITEM
 UNFX  RIR     20                 RESET FIXED POINT INDICATOR
       TRA     $UNFIX             FLOAT THE NUMBER IN THE AC
 MIXFL SXA     MXIR,4             FIX MIXED EXPRESSION
       STO     AMR                SAVE AC
       CLA     AMSUM              PICK UP CURRENT ANSWER
       TSX     UNFX,4             FLOAT IT
       STO     AMSUM              PUT IT AWAY
       CLA     AMR                RESTORE AC
 MXIR  AXT     **,4               RESTORE IR 4
       TRA     1,4                RETURN
 AMM   LXD     AMQ,2              MAX OR MIN FUNCTION. GET TYPE
       TSX     FIXFLO,4           TEST FOR FIX OR FLOAT
       NOP                        IMPOSSIBLE RETURN                     PAGE 141
       TRA     AFL                EXECUTE FLOATING SECTION
       SIR     20                 SET FIXED PONT INDICATOR
       RFT     40                 TEST FOR MIXED EXP
       TSX     UNFX,4             FLOAT THE ARGUMENT IF MIXED
 AMRNT RNT     4                  TEST FORMAX FUNCTION
       TRA     AMIN               EXECUTE MIN FUNCTION
       CAS     AMSUM              COMPARE WITH CURRENT ANSWER
       STO     AMSUM              IS GREATER, STORE AS NEW ANSWER
       NOP                        THEY ARE EQUAL
       CAL     AMLIS              IS LESS, PICK UP ARGUMENT LIST
       TRA     AMLP               DO NEXT ITEM
 AFL   SIR     40                 SET FLOATING POINT INDICATOR
       RFT     20                 TEST FOR MIXED EXPRESSION
       TSX     MIXFL,4            UNMIX THE EXPRESSION
       TRA     AMRNT              COMPARE AND DO NEXT ITEM
 AMIN  CAS     AMSUM              MIN FUNCTION, COMPARE WITH CURRENT VAL
       TRA     *+3                IS GREATER
       TRA     *+2                IS EQUAL
       STO     AMSUM              IS LESS, STORE AS NEW ANSWER
       CLA     AMLIS              PICK UP NEXT ITEM
       TRA     AMLP               EXECUTE IT
 AMEND CLA     AMSUM              ALL DONE. PICKUP CURRENT ANSWER
       LDQ     AMFXC              PRESET MQ
       RFT     40                 SKIP NEXT IF FIXED POINT
       LDQ     AMFLC              PICK UPI FIX IN MQ
       LDI     AMIND              RESTORE INDICATORS
       LXD     AMIR,4             RESTORE IR 4
 AMIR2 AXT     **,2               RESTORE IR 2
       TRA     $MKNO              MAKE THE ANSWER A NUMBER
 AMFLC SYN     FLOATD             FLAOT CONSTANT
 AMFXC SYN     $FIXD              FIX CONSTANT
 AMR                              TEMP STORAGE
 AMSUM                            CURRENT ANSWER STORAGE
* NUMVAL       NUMERICAL VALUE TAKES ANY LIST AND DECIDES IF IT
*      REPRESENTS A FIXED POINT OR FLOATING POINT NUMBER. IF IT DOES NOT
*      THE ROUTINE CLEARS THE AC AND MQ DOES AN XEC 1,4 AND THEN GOES
*      TO ERROR WITH A BAD ARGUMENT COMPLAINT. IF THE LIST DOES
*      REPRESENT A NUMBER, UPON EXIT THE FOLLOWING THINGS ARE LEFT
*      AS INDICATED               POINTER TO FULL WORD IN AC
*                                 $FIX OR $FLOAT IN MQ
NUMVAL SXA     NVIR4,4            SAVE LINK IR
       STO     $ARG3              SAVE ORIGINAL ARGUMENT
       PDX     0,4                POINTER TO NUMBER IN IR 4
 NVLP  TXL     NVNO,4,0           NULL  LIST IS NOT A NUMBER
       CLA     0,4                FIRST ELEMENT
       PAX     0,4                CAR LIST
       TXH     NVATM,4,-2         GO IF AN ATOM
*
 NVNO  PXD     0,0                IS NOT NUMBER, CLEAR AC
       XCA                        PUT IN MQ
       PXD     0,0                CLEAR AC AGAIN
       LXA     NVIR4,4            RESTORE LINK IR
       XEC     1,4                EXECUTE POSSIBLE EXIT INSTRUCTION
       CLA     $ARG3              MUST BE AN ERROR, PICK UP ORIGINAL ARGPAGE 142
       SXD     $ERROR,4
       TSX     $ERROR+1,4         GO TO ERROR
       BCI     1,*I  3*           BAD ARGUMENT NUMVAL
*
NVATM  PXD     0,4                                                      
       ANA     TAGMSK
       TZE     NVNO
       ARS     15
       STA     *+2
       PXD     0,4
       AXC     **,4
       LDQ     NVTBL,4
 NVIR4 AXT     **,4               RESTORE IR 4
NVTBL  TRA     1,4
               0,,$FIX
               0,,$FLOAT
               0,,0
               0,,0
               0,,$FIX
*
*
* ADD1         ADD 1 ADDS ONE TO ANY FIXED POINT OR FLOATING POINT
*      NUMBER AND EXITS WITH THE NUMBER NUMBER
 ADD1  SXA     A1IR1,1            SAVE IR 1
       AXT     0,1                ZERO IR 1(INDICATES ADD OP)
 AD1   SXA     A1IR2,2            SAVE IR 2
       SXA     A1IR4,4            SAVE LINK IR
       TSX     NUMVAL,4           EVALUTE NUMERICAL ARGUMENT
       STQ     A1T                SAVE $FIX OR $FLOAT
       PDX     0,4                POINTER TO FULL WORD
       CLA     0,4                GET NUMERICAL VALUE
       LXD     A1T,2              PICK UP $FIX OR $FLOAT
       TSX     FIXFLO,4
       NOP                        IMPOSSIBLE RETURN
       XEC     FAD,1              IS FLOAT, DO FLOATING POINT OP
       XEC     ADDF,1             DO FIXED POINT OP
       LDQ     A1T                RESTORE $FLOAT AFTER FAD
 A1IR1 AXT     **,1               RESTORE IR 1
 A1IR2 AXT     **,2               RESTORE IR 2
 A1IR4 AXT     **,4               RESTORE LINK IR
       TRA     $MKNO              MAKE RESULT A NUMBER
*
 A1T                              TEMPORARY STORAGE
 FAD   FAD     $QF1               FLOATING ADD FOR ADD1
       FSB     $QF1               FOR SUB1
 ADDF  ADD     $Q1                FOR ADD1
       SUB     $Q1                FOR SUB1
*
* SUB1         SUBTRACT 1 SUBTRACTS ONE FROM A FIXED POINT OR FLOATING
*      POINT NUMBER. USES CODING OF ADD1 WITH AN INITIALIZATION.
 SUB1  SXA     A1IR1,1            SAVE IR1
       AXT     -1,1               SET FOR SUBTRACT OPERATIONS
       TRA     AD1                PERFORM ADD1 CODING
* SUB1 USES THE CODING OF ADD1                                          PAGE 143
*
*
GRTRTP SXA     GRTIR,4            SAVE LINK IR
       TSX     UNUMIX,4           EVALUATE NUMERICAL ARGUMENTS
       TLQ     GRTT               PREDICATE TRUE
       PXD     0,0                FALSE, CLEAR AC
 GRTIR AXT     **,4
       TRA     1,4                EXIT
*
 GRTT  CLA     $QD1               GET TRUE VALUE
       TRA     GRTIR              RESTORE LINK IR AND EXIT
*
*
* LESSTP       LESS THAN PREDICATE.  SIMPLE DOES GREATER THAN  PREDICATE
*      WITH THA ARGUMENT REVERSED.
*
LESSTP XCA                        INTERCHANGE ARGUMENTS
       TRA     GRTRTP             DO GREATER THAN PREDICATE
*
* THE FOLLOWING IS A NUMBER PREDICATE PACKAGE WHICH INCLUDES NUMBER
*      PREDICATE, ZERO PREDICATE, MINUS PREDICATE, ONE PREDICATE, FIX
*      PREDICATE AND FLOAT PREDICATE. ALL THESE PREDICATES SHARE CERTAIN
*      BLOCKS OF CODING AND TEMPORARY STORAGE.
* NUMBRP       NUMBER PREDICATE TEST ITS ARGUMENT FOR A NUMBER
NUMBRP SXA     NPIR,4             SAVE LINK IR
       TSX     NUMVAL,4           EVALUATE ARGUMENT
       TZE     NPIR               IF ZERO NOT A NUMBER
 NPT   CLA     $QD1               IS A NUMBER, PICK UP TRUTH
 NPIR  AXT     **,4               RESTORE LINK IR
       TRA     1,4                EXIT
*
* FLOATP       FLOATING POINT NUMBER PREDICATE TESTS TO SEE IF ITS
*      ARGUMENT IS A FLOATING POINT NUMBER
FLOATP SXA     NPIR,4             SAVE LINK IR
       SXA     ZPIR,2             SAVE IR 2
       TSX     NUMVAL,4           EVALUATE ARGUMENT
       XCA                        GET TYPE IN AC
       PDX     0,2                TYPE IN IR 2
       TSX     FIXFLO,4           TEST FOR $FIX OR $FLOAT
       NOP                        IMPOSSIBLE RETURN
       TRA     FLT                IS FLOATING POINT
       TRA     ZPF                IS NOT FLOATING POINT, EXIT FALSE
 FLT   CLA     $QD1               GET TRUTH VALUE
       TRA     ZPIR               RESTORE IR S AND EXIT
*
* FIXP         FIXED POINT PREDICATE TESTS FOR FIXED POINT NUMBERS.
 FIXP  SXA     NPIR,4             SAVE LINK IR
       SXA     ZPIR,2             SAVE IR 2
       TSX     NUMVAL,4           EVALUATE ARGUMENT
       XCA                        GET TYPE IN AC
       PDX     0,2                TYPE IN IR 2
       TSX     FIXFLO,4           TEST FOR $FIX OR $FLOAT
       NOP                        IMPOSSIBLE EXIT
       TRA     ZPF                IS FLOAT, EXIT FALSE                  PAGE 144
       CLA     $QD1               IS FIX, GET TRUTH VALUE
       TRA     ZPIR               RESTORE IR S AND RETURN
*
* MINUSP       MINUS PREDICATE TESTS TO SEE IF ITS ARGUMENT IS A
*      NEGATIVE NUMBER.
MINUSP SXA     NPIR,4             SAVE LINK IR
       TSX     NUMVAL,4           EVALUATE ARGUMENT
       PDX     0,4
       CLA     0,4                PICK UP NUMBER
       TMI     NPT                EXIT TRUE IF MINUS
       PXD     0,0                IS NOT, EXIT FALSE
       TRA     NPIR               RESTORE LINK IR AND EXIT
*
* ZEROP        ZERO PREDICATE TESTS ITS ARGUMENT FOR A FIXED POINT
*      ZERO    OR
*      ZERO OR A FLOATING POINT ZERO + OR - A TOLERANCE (FLOTOL).
 ZEROP SXA     NPIR,4             SAVE LINK IR
       SXA     ZPIR,2             SAVE IR 2
       TSX     NUMVAL,4           EVALUATE ARGUMENT
       PDX     0,4                GET POINTER TO IR 4
       CLA     0,4                FULL WORD
ZPG    SSP                        GET MAGNITUDE OF N
       TZE     ZPT                EXIT TRUE IF ZERO
       XCA                        PUT NUMBER IN MQ
       PDX     0,2                PUT TYPE IN IR 2
       CLA     FLOTOL             PICK UP FLOATING POINT TOLERENCE
       TSX     FIXFLO,4           TEST FOR FIX OR FLOAT
       TRA     ZPTS               NOT FIX OR FLO MEANS FLO FROM ONEP
       TRA     ZPTS               IS FLOATING POINT, COMPARE WITH FLOTOL
 ZPF   PXD     0,0                IS FIXED POINT, EXIT FALSE
 ZPIR  AXT     **,2               RESTORE IR 2
       TRA     NPIR               RESTORE IR 4 AND EXIT
 ZPT   CLA     $QD1               GET TRUTH VALUE
       TRA     ZPIR               RESTORE IR S AND EXIT
 ZPTS  TLQ     ZPT                IS FLOATING POINT, EXIT TRUE IF LESS
       TRA     ZPF                OTHERWISE EXIT FALSE
* ONEP         ONE PREDICAT TESTS TO SEE IF ITS ARGUMENT IS ONE
*      BY SUBTRACTIGN ONE AND TESTING THE RESULT WITH ZEROP.
 ONEP  SXA     NPIR,4             SAVE LINK IR
       SXA     ZPIR,2             SAVE IR 2
       TSX     NUMVAL,4           EVALUATE ARGUMENT
       PDX     0,4                POINTER TO AC
       CLA     0,4                FULL WORD TO AC
       XCA                        TYPE TO AC
       PDX     0,2                TYPE TO IR 2
       XCA
       TSX     FIXFLO,4           DISPATCH ON FIX OR FLOAT
       NOP                        IMPOSSIBLE RETURN
       FSB     $QF1
       SUB     $Q1                SUBTRACT 1
       TRA     ZPG                APPLY ZERO PREDICATE
*
*      FIXFLO                     SUBROUTINE TO DISPATCH ON FIX OR FLO,
*                                 ARGUMENT IN IR 2.                     PAGE 145
*
FIXFLO TXL     *+2,2,$FIX-1       TXL - TXL FILTER FOR $FIX
       TXL     FX,2,$FIX          GO IF $FIX
       TXL     *+2,2,$FLOAT-1     TXL - TXL FILTER FOR FLOAT
       TXL     FL,2,$FLOAT        GO IF $FLOAT
       XEC     1,4                EXECUTE IF NEITHER FIX OR FLOAT
       TRA     4,4                RETURN
       TRA     5,4                SKIP EXIT
       TRA     6,4                SKIP 2 EXIT
 FL    XEC     2,4                EXECUTE IF $FLOAT
       TRA     4,4                RETURN
       TRA     5,4                SKIP EXIT
       TRA     6,4                SKIP 2 EXIT
 FX    TRA     3,4
*      FIXFLO USES $FIX AND $FLAOT
* UNFIX        UNFIX MAKES A FIXED POINT ARGUMENT IN THE AC A FLOATING
*      POINT NUMBER LEFT IN AC. MQ IS PRESERVED.
 UNFIX STO     UFC                SAVE ARGUMENT
       ANA     UFMSK              MASK OUT ALL BUT CHARACTERISTIC
       TNZ     UFE                IF ANY THING LEFT IT MUST BE NORMALIZD
       CLA     UFC                NOTHING LEFT, RESTORE ARGUMENT
       ORA     UFMC               OR IN CHARACTERISTIC
       STQ     UFQ                SAVE MQ
       FAD     UFMC               ESSENTIALLY FAD OR ZERO TO NORMALIZE
       LDQ     UFQ                RESTORE MQ
       TRA     1,4                EXIT
*
 UFE   SXA     UFXR,4             NUMBER GREATER THAN 2 TO 27. SAVE IR 4
       AXT     2*64+3*8+4,4        CHARACTERISTIC SO FAR
       STZ     UFS                INITIALIZE SIGN PORTION
       TPL     UFF                SKIP IF +
       SSP                        MAKE IT +
       STL     UFS                RECORD FACT BY MAKING UFS NON-ZERO
 UFF   ARS     1                  DIVIDE NUMBER BY 2
       CAS     UFNC               SEE IF NORMALIZED YET
       TXI     UFF,4,1            ADD 1 TO CHARACTERISTIC AND TRY AGAIN
       TXI     UFF,4,1            DITTO
       STO     UFC                IS NORMALIZED
       PXD     0,4                CHARACTERISTIC TO AC
       ALS     9                  POSITION CHARACTERISTIC
       ORA     UFC                OR IN NORMALIZED NUMBER
       ZET     UFS                TEST FOR SIGN, 0 MEANS +
       SSM                        NOT ZERO SO MAKE MINUS
 UFXR  AXT     **,4               RESTORE IR 4
       TRA     1,4                EXIT
 UFMSK SYN Q777Q9                 CHARACTERISTIC MASK
 UFMC  SYN Q233Q9                 GENERAL CHARACTERISTIC
 UFNC  SYN Q01Q9
 UFQ                              MQ
 UFC                              AC TEMPORARY STORAGE
 UFS                              SIGN STORAGE
* UNFIX USES NO EXTERNAL CONSTANTS.
* FLOTOL       FLOATING POINT TOLERENCE USED IN DESIDING IF FLOATING
*      POINT NUMBERS ARE INTEGERS.                                      PAGE 146
FLOTOL DEC     3E-6               FLOATING POINT TOLERENCE VALUE
* MNSPRG       MINUS PROGRAM MAKES A LIST OF MINUS AND ITS ARGUMENT
*
* MNSPRG       CREATES A NUMBER OF OPPOSITE SIGN OF NUMERAL ARGUMENT
*
MNSPRG SXA     MRXR,4             SAVE LINK IR
       TSX     NUMVAL,4           EVALUATE THE NUMERICAL ARGUMENT
       PDX     0,4                POINTER TO FULL WORD
       CLA     0,4                NUMERICAL VALUE
       CHS                        MAKE OPPOSITE SIGN
 MRXR  AXT     **,4               RESTORE LINK IR
       TRA     $MKNO              MAKE IT A NUMBER
*
* RCPPRG       CALCULATES THE RECIPORICAL OF A NUMBER.
RCPPRG SXA     RRXR,4             SAVE LINK IR
       SXA     RRXR2,2            SAVE IR 2
       TSX     NUMVAL,4           EVALUTE THE NUMERICAL ARGUMENT
       PDX     0,4                POINTER TO FULL WORD
       CLA     0,4                NUMERICAL VALUE
       STO     RCPT               SAVE VALUE
       XCA                        TYPE TO AC
       PDX     0,2                TYPE TO IR 2
       TSX     FIXFLO,4           DISPATCH ON FIX OR FLOAT
       NOP                        IMPOSSIBLE RETURN
       CLA     $QF1               IS FLOAT, PICK UP FLOATING POINT 1
       TRA     RCPFX              IS FIXED POINT
       FDP     RCPT               DIVIDE BY ARGUMENT
       DCT                        CHECK FOR ILLEGAL DIVISION
       TSX     $DCT,4             DIVIDE CHECK ERROR
       XCA                        QUOTENT TO AC
       LDQ     RCPS               $FLOAT TO MQ
 RRXR  AXT     **,4               RESTORE LINK IR
 RRXR2 AXT     **,2               RESTORE IR 2
       TRA     $MKNO              MAKE ANSWER A NUMBER
*
 RCPFX XCA                        FIXED POINT RECIP, ANSWER IS ZERO
       PXD     0,0                CLEAR AC
       TRA     RRXR               RESTORE IR S AND MAKE A NUMBER
*
 RCPT                             TEMPORARY STORAGE
 RCPS  SYN     FLOATD             FLOAT INDICATOR
*
       EJECT                                                            PAGE 147
       REM APPLY
       REM
       REM APPLY(F,L,A) =
       REM    SELECT(CAR(L).,
       REM           -1,APP2(F,L,A).,
       REM           LAMBDA,EVAL(F,APPEND(PAIR(CADR(F),L),A)).,
       REM           LABEL,APPLY(CADDR(F),L,APPEND(
       REM                PAIR1(CADR(F),CADDR(F))),A).,
       REM           APPLY(EVAL(F,A),L,A))
       REM
A      HED
 APPLY SXD ASS1,4
       TZE 1,4
       STO AST1                   F
       PDX 0,4
       SXA     ASS1,4             SAVE FUNCTION ALONG WITH INDEX REGISTE
       CLA 0,4                    CWR(F)
       PAX 0,4                    CAR(F)
       TXH ASP1,4,-2              =-1
       PXD 0,4
       CAS ASLMD                  = LAMBDA
       TRA *+2
       TRA ASP2
       CAS ASFUN
       TRA *+2
       TRA ASP4
       CAS ASLBL                  = LABEL
       TRA *+2
       TRA ASP3
       TSX $SAVE,4
       TXL     $END3,,ASSA+2      SAVE 3 ITEMS
       STQ ASSL
       LDQ $ARG3
       STQ ASSA
       CLA AST1                   F
       TSX $EVAL,4                EVAL(F,A)
       LDQ ASSA
       STQ $ARG3
       LDQ ASSL
       TSX UNSAVE,4
       LXD ASS1,4
       TRA APPLY                  APPLY(EVAL(F,A),L,A)
       REM
 ASP1  CLA AST1                   F
       LXD ASS1,4
       TRA $APP2                  P APP29F,L,A)
*              LAMBDA BRANCH
 ASP2  LXD AST1,4                 F
       CLA $ARG3
       STO AST3
       CLA 0,4                    CWR(F)
       PDX 0,4                    CDR(F)
       CLA 0,4                   CWDR(F)
       STO AST4                                                         PAGE 148
       PAX 0,4                    CADR(F)
       PXD 0,4
       TSX $PAIR,4                PAIR(CADR(F),L)
       LDQ AST3                   A
       TSX     $NCONC,4
       XCA
       LXD AST4,4                 CDDR(F)
       CLA 0,4
       PAX 0,4
       PXD 0,4
       LXD ASS1,4
       TRA $EVAL      EVAL(CADDR(F),APPEND(PAIR(CADR(F),L),A))
       REM
*              LABEL BRANCH
 ASP3  LXD AST1,4                 F
       STQ AST2                   L
       LDQ $ARG3                  A
       STQ AST3
       CLA 0,4                    CWR(F)
       PDX 0,4                    CDR(F)
       CLA 0,4
       STO AST4                   CWDR(F)
       PDX 0,4                    CDDR(F)
       CLA 0,4
       PAX 0,4                    CADDR(F)
       PXD 0,4
       STO AST1
       XCA
       LXA AST4,4
       PXD 0,4                    CADR(F)
       TSX $CONS,4                CONS(CADR(F),CONS(CADDR(F),0))
       LDQ AST3                   A
       TSX $CONS,4                APPEND( ABOVE,A)
       STO $ARG3
       LDQ AST2
       CLA AST1                   CADDR(F)
       LXD ASS1,4
       TRA APPLY   APPLY(CADDR(F),L,APPEND(PAIR(CADR(F),CADDR(F)),A))
       REM
*              FUNARG BRANCH
 ASP4  LXD AST1,4                 F
       CLA ,4
       PDX ,4                     CDR(F)
       CLA ,4
       STO AST1                   CWDR(F)
       PDX ,4                     CDDR(F)
       CLA ,4
       PAX ,4                     CADDR(F)
       PXD ,4
       STO $ARG3                  A
       LXA AST1,4                 CADR(F)
       PXD ,4                     F
       LXD ASS1,4                                                       
       TRA $APPLY                                                       PAGE 149
       REM
 ASLBL SYN LABELD
 ASLMD SYN LAMDAD
 ASFUN SYN     FNARGD
 ASZRO SYN     $ZERO
       REM
       REM APP2(F,L,A)=SELECT(F.,CAR,CAAR(L).,CDR,
       REM CDAR(L).,CONS,CONS(CAR(L),CADR(L)).,LIST,COPY(L).,SEARCH(F,
       REM LAMBDA(J,CAR(J)=SUBR OR CAR(J)=EXP),
       REM LAMBDA(J,CAR(J)=SUBR YIELDS APP3(CWADR
       REM (J),DISTRIB(L)),1 YIELDS APPLY(CADR(J),L,A)))
       REM ERROR)
       REM
A      HED
 APP2  SXD     AST1,4             SAVE LINK IR
       LXD     $ARG3,4            GET ALIST
       SXD     A,4                SAVE IT
       STQ     AL                 ARGUMENT LIST
       STO     F                  FUNCTION (IS ATOMIC SYMBOL)
       STZ     APTRT              INITIALIZE TRACE TEST CELL
 APSES PDX     0,4                ARG TO IR
       TXL     APSAL,4,0          GO IF NO MORE PROPERTY LIST
       CLA     0,4                FIRST WORD
       PAX     0,4                CAR
       TXL     *+2,4,$TRACE-1
       TXL     APTRK,4,$TRACE     LOOK FOR TRACE
       TXL     *+2,4,$SUBR-1      LOOK FOR
       TXL     R2,4,$SUBR         $SUBR OR
       TXL     APSES,4,$EXPR-1    $EXPR
       TXH     APSES,4,$EXPR
*              EXPR BRANCH IN APPLY
 R21   PDX     0,4                POINTER TO NEXT WORD AFTER $EXPR
       CLA     0,4                NEXT WORD
       PAX     0,4                CAR
       PXD     0,4                IS FUNCTION
       ZET     APTRT              TEST FOR TRACE MODE
       TRA     APTXP              TRACE THIS EXPRESSION
       LXD     ATS1,4             RESTORE LINK IR
       TRA     $APPLY             GO TO APPLY
*              RZ THE SUBR BRANCH OF APPLY
 R2    PDX     0,4                GET THE TXL INSTRUCTION BT  TAKING
       CLA     0,4                CWR (CADR L))
       PAX     0,4
       CLA     0,4
       STO     CWADR              TXL INSTRUCTION
       CLA     ASS1
       STO     CSV
       CLA     AL                 GET THE ARGUMENT LIST
       TSX     SPREAD,4           SPREAD IT INTO AC, MQ, ARG3, ETC.
       ZET     APTRT              TEST FOR TRACE MODE
       TRA     APTSB              TRACE THIS SUBROUTINE
       TSX     $SAVE,4
       TXL     $END2,,$ALIST+2
       LXD     A,4                                                      PAGE 150
       SXD     $ALIST,4
       TSX     CWADR,4
       TSX     UNSAVE,4
       LXD     CSV,4
       TRA     1,4
*
APSAL  CLA     FAS                WHERE TO GO IF NOT FOUND ON PAIR LIST
       STO     $ARG3
       CLA     F                  ATOMIC FUNCTION
       LDQ     A
       TSX     SASSOC,4           SEARCH PAIR LIST FOR LABEL DEFINITION
       PDX     0,4                POINTER TO ASSOCIATED ITEM
       CLA     0,4
       PDX     0,4                POINTER TO ITEM
       PXD     0,4
       LDQ     A                  RESTORE PAIR LIST
       STQ     $ARG3
       LDQ     AL                 RESTORE ARGUMENT LIST
       ZET     APTRT              TEST FOR TRACE MODE
       TRA     APTXP              TRACE THIS EXPRESSION
       LXD     ATS1,4             RESTORE LINK IR
       TRA     $APPLY             GO TO APPLY WITH ITEM ASSOCIATED WITH
*                                 THE ATOMIC FUNCTION
APTXP  TSX     $SAVE,4            TRACE EXPR
       TXL     $END1,,CSV+2
       TSX     $APPLY,4
       TRA     APEXC              FINISH UP
*
 R33   SXD     $ERROR,4
       CLA     F                  PICK UP FUNCTION
       TSX     $ERROR+1,4         GO TO ERROR
       BCI     1,*A  2*           FUNCTION OBJECT HAS NO DEFINITION
*
 APTRK STL     APTRT
       STO     APA                SAVE THE AC
       LXA     ASS1,4             ATOM NAME
       PXD     0,4
       TSX     ARGOF,4            PRINT ARGUMETNS OF
       LDQ     AL                 RESTORE MQ AFTER PRINTING
       CLA     APA                RESTORE AC
       TRA     APSES              CONTINUE PROPERTY LIST SEARCH
*
APTSB  TSX     $SAVE,4            TRACE SUBR
       TXL     $END2,,$ALIST+2
       LXD     A,4
       SXD     $ALIST,4
       TSX     CWADR,4
APEXC  TSX     UNSAVE,4
       XCA                        VALUE TO MQ
       LXA     CSV,4
       PXD     0,4                TO AC
       LXD     CSV,4
       TRA     VALOF              PRINT VALUE OF
*                                                                       PAGE 151
 APA                              AC STORAGE
 APTRT                            TRACE MODE TEST SWITCH
 CWADR                            TXL INSTRUCTION FOR SUBR
 ATS1                             LINK INDEX REGISTER
 FAS   TXL     R33,,0             NOT FOUND ON PAIR LIST SO CALL ERROR
 F                                ATOMIC FUNCTION GOES HERE
 AL                               ARGUMENT LIST
 A                                A OR PAIR LIST
*
       REM
       REM
A      HED
 EVCON TZE E3
       SXD ECS1,4
       TSX $SAVE,4
       TXL     $END4,,ECS4+2      SAVE 4 ITEMS
       STQ ECS2
       PDX 0,4
 E1    CLA 0,4
       STO ECS3
       PAX 0,4
       CLA 0,4
       STO ECS4
       PAX 0,4
       PXD 0,4
       TSX $EVAL,4
       LDQ ECS2
       TZE E2
       LXD ECS4,4
       CLA 0,2
       PAX 0,2
       PXD 0,4
       TSX UNSAVE,4
       LXD ECS1,4
       TRA $EVAL
 E2    LXD ECS3,4
       TXH E1,4,0
 E3    SXD $ERROR,4
       LXA     ECS3,4
       PXD     0,4                PRINT LAST CONDITION
       TSX $ERROR+1,4
       BCI     1,*A  3*           CONDITIONAL UNSATISFIED
       REM BASIC LISP FUNCTIONS FOR APPLY
       REM
       REM
R      HED
       REM CAR
       REM
CARP   SXA     CARX,4
       PDX ,4
       CAL ,4
       PAX ,4
       PXD ,4
CARX   AXT     **,4                                                     PAGE 152
       TRA 1,4
 BFS1
       REM
CDRP   SXA     CDRX,4
       PDX ,4
       CLA ,4
       ANA BFDM
CDRX   AXT     **,4
       TRA 1,4
 BFDM  SYN     $DMASK
       REM
       REM
ATOMP  SXA     ATMX,4
       TZE ATP1
       PDX ,4
       CLA ,4
       PAX ,4
       TXL *+3,4,-2
 ATP1  CLA BFQ1
       TRA *+2
       PXD ,2
ATMX   AXT     **,4
       TRA 1,4
 BFQ1  SYN     $QD1
       REM
 NULLP TZE *+3
       PXD ,0
       TRA 1,4
       CLA BFQ1
       TRA 1,4
       REM
       REM
       REM
       REM LAMBDA FOR FUNCTIONAL ARGUMENTS
       REM
 LAMP  SXD BFS1,4
       STO BFS2                   L
       XCA
       LDQ BFZRO
       TSX $CONS,4                CONS(A,0)
       XCA
       CLA BFS2
       TSX APPEND,4
       XCA
       CLA BFFAG
       LXD BFS1,4
       TRA $CONS                  LIST(FUNARG,L,A)
 BFFAG SYN     FNARGD
 BFZRO SYN     $ZERO
       REM
       REM LABEL FSUBR
       REM
 LABP  SXD BFS1,4
       STQ BFS3                   A                                     PAGE 153
       PDX ,4                     L
       CLA ,4
       STO BFS2                   CWR(L)
       PDX ,4                     CDR(L)
       CLA ,4
       PAX ,4                     CADR(L)
       PXD ,4
       STO BFS4
       XCA
       LXA BFS2,4                 CAR(L)
       XCA
       PXD ,4
       TSX $COND,4                LIST(CAR(L),CADR(L))
       LDQ BFS3
       TSX $CONS,4                CONS(LIST,A)
       XCA
       CLA BFS4                   CADR(L)
       LXD BFS1,4
       TRA $EVAL
       REM
       REM
       REM
       REM SETQ
       REM
 SETQP SXD REPS1,4
       TSX $SAVE,4
       TXL     $END2,,REPV+2
       PDX ,4                     L
       CLA ,4
       PAX ,4                     CAR(L)
       SXD REPV,4
       PDX ,4                     CDR(L)
       CLA ,4
       PAX ,4                     CADR(L)
       PXD ,4
       TSX $EVAL,4                EVAL(CADR(L),A)
       STO REPT1
       CLA REPP1
       STO $ARG3
       LDQ PRGVAR
       CLA REPV
       TSX SASSOC,4               SASSOC(CAR(L),PV,ERROR)
       PDX ,4
       CLA REPT1
       STD     0,4                REPLACE DECREMENT
       TSX UNSAVE,4
       LXD REPS1,4
       TRA 1,4
       REM
 REPP1 TXL *+1,,0
       SXD $ERROR,4
       CLA REPV
       TSX $ERROR+1,4
       BCI     1,*A  4*           SETQ GIVEN ON NON-EXISTENT VARIABLE   PAGE 154
       REM
       REM
       REM SET
       REM
 SETP  SXD BFS1,4
       STO BFS5
       STQ BFS2
       LDQ SETP1
       STQ $ARG3
       LDQ PRGVAR
       TSX SASSOC,4
       PDX ,4
       CLA BFS2
       STD 0,4
       LXD BFS1,4
       TRA 1,4
       REM
 SETP1 TXL *+1,,0
       SXD $ERROR,4
       CLA BFS5
       TSX $ERROR+1,4
       BCI     1,*A  5*           SET  GIVEN ON NON EXISTENT VARIABLE
BFS5
       REM
*              AND SPECIAL FORM
 EVA8  TNZ EVA6
       CLA EVCT
       TRA 1,4
 EVA6  SXD EVA1,4
       TSX $SAVE,4
       TXL     $END3,,EVA9+2      SAVE 3 ITEMS
       PDX ,4
 EVA4  CLA ,4
       STO EVA2
       PAX ,4
       PXD ,4
       STQ EVA9
       TSX $EVAL,4
       LDQ EVA9
       TNZ EVA3
 EVA5  TSX UNSAVE,4
       LXD EVA1,4
       TRA 1,4
 EVA3  LXD EVA2,4
       TXH EVA4,4,0
       CLA EVCT
       TRA EVA5
*              OR SPECIAL FORM
 EVR8  TNZ EVR6
       CLA EVCF
       TRA 1,4
 EVR6  SXD EVR1,4
       TSX $SAVE,4
       TXL     $END3,,EVR9+2      SAVE 3 ITEMS                          PAGE 155
       PDX ,4
 EVR4  CLA ,4
       STO EVR2
       PAX ,4
       PXD ,4
       STQ EVR9
       TSX $EVAL,4
       LDQ EVR9
       TZE EVR3
       CLA EVCT
 EVR5  TSX UNSAVE,4
       LXD EVR1,4
       TRA 1,4
 EVR3  LXD EVR2,4
       TXH EVR4,4,0
       CLA EVCF
       TRA EVR5
 EVCT  SYN     $QD1
 EVCF  SYN     $ZERO
       REM
 EQP   STQ BFS1
       SUB BFS1
       TNZ *+3
       CLA BFQ1
       TRA 1,4
       PXD ,0
       TRA 1,4
       REM
       REM EVAL(E,A)  5/6/59
       REM
A      HED
 EVAL  SXD EVS1,4
       TZE 1,4
       STO EVTE                   E
       PDX ,4
       CLA ,4
       STT     EVLNS              SEE IF A NUMBER
       ZET     EVLNS              SKIP IF NOT A NUMBER
       TRA     EV1N               IS A NUMBER(CONSTANT)
       PAX ,4                     CAR(E)
       TXH EVP1,4,-2              = - 1
       SXD EVTAE,4                CAR(E)
       SXA     EVS1,4             SAVE FUNCTION WITH INDEX REGISTER
       STD EVTDE                  CDR(E)
       CLA ,4
       STT     EVLNS              SEE IF A NUMBER
       ZET     EVLNS              TEST FOR A NUMBER
       TRA     EVP26              UNDEFINED FUNCTION IF A NUMBER
       PAX ,4                     CAAR(E)
       TXL     EVP27,4,-2         GO IF CAR(E) NOT AN ATOM
*
*              CAAR(E) = -1
*
       SXA     EVTRK,0            ZERO THE ADDRESS                      PAGE 156
       SXD     EVTRK,0            ZERO DECREMENT
 EVP2  PDX ,4                     CDAR(E)
       TXL EVP25,4,0              NULL(J)
       CLA ,4
       PAX ,4                     CAR(J)
       TXH     *+2,4,$TRACE
       TXH     EVTRT,4,$TRACE-1   =TRACE
       TXH *+2,4,$SUBR
       TXH     EVP27,4,$SUBR-1    OF IF A SUBR
       TXH *+2,4,$FSUBR
       TXH EVP22,4,$FSUBR-1       =FSUBR
       TXH *+2,4,$EXPR
       TXH EVP23,4,$EXPR-1        =EXPR
       TXH EVP2,4,$FEXPR
       TXL EVP2,4,$FEXPR-1        /= FEXPR
       STD EVD2                   CDR(J)
       STQ $ARG3                  A
       CLA $ARG3
       LDQ EVZRO                  0
       TSX $CONS,4                CONS(A,0)
       XCA
       CLA EVTDE
       TSX $CONS,4                LIST(CDR(E),A)
       XCA
       LXD EVD2,4                 CDR(J)
       CLA ,4                     
       PAX ,4                     CADR(J)
       PXD ,4
       ZET     EVTRK              TEST FOR TRACE MODE
       TRA     EVTXP
       LXD EVS1,4
       TRA $APPLY                 APPLY(CADR(J),LIST(CDR(E),A),A)
*
 EVTRT STL     EVTRK              SET THE TRACE SWITCH
       TRA     EVP2               GO SEARCH MORE
*
*
*              CAR(E) = -1
*
 EV1N  CLA     EVTE               GET THE NUMBER
       LXD     EVS1,4             RESTORE LINK INDEX
       TRA     1,4
*
 EVP1  PDX ,4                     J
       TXL EVP11,4,0              = 0
       CLA ,4
       PAX ,4                     CAR(J)
       TXH EVP1,4,$APVAL          = APVAL
       TXL EVP1,4,$APVAL-1
 EVP13 PDX ,4                     CDR(J)
       CLA ,4
       PAX ,4                     CADR(J)
       CLA ,4
       PAX ,4                     CAADR(J)                              PAGE 157
       PXD ,4
       LXD EVS1,4
       TRA 1,4
*
 EVP11 STQ EVTA                   A
       CLA EVTE                   E
       STD EVI1
       SUB EVQD1
       STD EVI2
       SXD EVD1,2
       LXD EVTA,4
 EVL1  TXL EVP12,4,0              NULL(J)
       CLA ,4
       PAX ,2                     CAR(J)
       PDX ,4                     CDR(J)
       CLA ,2
       PAX ,2                     CAAR(J)
 EVI1  TXH EVL1,2,**              CAAR(J) = E
 EVI2  TXL EVL1,2,**
       PDX ,4                     CDAR(J)
       PXD ,4
       LXD EVD1,2
       LXD EVS1,4
       TRA 1,4
*
 EVP12 SXD $ERROR,4
       CLA EVTE
       TSX $ERROR+1,4
       BCI     1,*A  8*           UNBOUND VARIBLE MENTIONED -EVAL-
*
 EVP22 PDX ,4                     CDR(J)     FSUBR
       CLA ,4
       PAX ,4                     CADR(J)
       CLA ,4                     CWADR(J)
       STO EVT1
       CLA     EVS1               ATOM AN DIR4 FOR SAVING $ALIST
       STO     CSV
       TSX     $SAVE,4
       TXL     $END2,,$ALIST+2
       STQ     $ALIST
       ZET     EVTRK              TEST WHETERT TO TRACT
       TRA     EVTFS              YES,TRACE FSUBR
       CLA     EVTDE              GET BACK ARGUMENTS
       TSX     EVT1,4
       TSX     UNSAVE,4
       LXD     CSV,4
       TRA     1,4
*
*              EVP23 THE EXPR BRANCH FOR EVAL
*
 EVP23 PDX     0,4                REST OF PROPERTY LIST
       CLA     0,4                GET THE EXPR
       PAX     0,4
       SXD     EVTAE,4            SAVE IN TEMPORARY STORAGE             PAGE 158
       LXD     $CPPI,4            PUSH DOWN COUNTER
       TXI     EVP28,4,-5         SAVE 5 ITEMS
*
 EVP25 CLA EVTAE                  CAR(E)
       STD EVI3                   TXH
       SUB EVQD1
       STD EVI4                   TXL
       SXD EVT1,2
       STQ EVD1
       LXD EVD1,4                 A
 EVL2  TXL EVP26,4,0              NULL(J)
       CLA ,4
       PDX ,4                     CDR(J)
       PAX ,2                     CAR(J)
       CLA ,2
       PAX ,2                     CAAR(J)
 EVI3  TXH EVL2,2,**              /= CAR(E)
 EVI4  TXL EVL2,2,**
       LXD     EVT1,2
       STD     EVTAE              SAVE FUNCTION
EV27   LXD     $CPPI,4
       TXI     *+1,4,-5           SAVE TOTAL OF 4 ITEMS
 EVP28 XEC     ENDPDL             TEST FOR OUT OF PUSH DOWN LIST
       SXD     $CPPI,4
       CLA     EVS1
       STO     -5,4
       CLA     EVSE
       STO     -4,4
       CLA     EVSA
       STO     -3,4
       CLA     EVTRK
       STO     -2,4
       CLA     EVCM
       STO     -1,4
       CLA     EVTAE              GET THE FUNCTION
       STD     EVSE               
       STQ EVSA                   A
       CLA EVTDE                  CDR(E)                                
       LDQ     ELP1          FUNCTIONAL ARGUMENT
       TSX     MAPLIS,4      MAPLIST(L,EVAL(CAR(L),A))
       STO EVT1
       CLA     EVSA
       STO     $ARG3
       CLA     EVSE
       LXD     $CPPI,4       START OPEN UNSAVE
       LDQ     -5,4
       STQ     EVS1
       LDQ     -4,4
       STQ     EVSE
       LDQ     -3,4
       STQ     EVSA
       LDQ     -2,4
       STQ     EVTRK
       TXI     *+1,4,5                                                  PAGE 159
       SXD     $CPPI,4
       LDQ     EVT1
       ZET     EVTRK              TEST RACE SWITCH
       TRA     EVDCO              DECODE EVTRAK
EVAPG  LXD     EVS1,4
       TRA $APPLY                 APPLY(CADAR(J),EVLIS(CDR(E),A),A)
*
*              IF CAR E IS A SUBR, THE POINTRE TO THE TXL INSTRUCTION
*              IS SAVED IN THE DECREMENT OF VETRK.  THE ADDRESS OF
*              EVTRK IS THE TRACE SWITCH.
*
EVDCO  LXD     EVTRK,4            LOOK FOR SUBR POINTER
       TXL     EVTXP,4,0          THERE ISNT ANY. SO GO AND TRACE EXPR
       LXA     EVTRK,4            SEE IF THE SUBR IS TRACED
       TXH     EVAPG,4,0          YES IT IS. LET APPLY HANDLE IT
       LXD     EVTRK,4            GET THE TXL SUBR WORD
       CLA     0,4
       STO     EVT1               READY TO EXECUTE
       CLA     EVS1               GET RETURN INDEX AND ATOM NAME
       STO     CSV                AND SAVE THEM ALONG WITH $ALIST
       TSX     $SAVE,4
       TXL     $END2,,$ALIST+2
       CLA     $ARG3
       STO     $ALIST             POST CURRENT ALIST
       XCA                        ARGUMENT LIST TO AC
       TSX     $SPREAD,4          SMEAR IT OUT
       TSX     EVT1,4             EXECUTE SUBR
       TSX     UNSAVE,4           RESTORE ALIST AND IX
       LXD     CSV,4
       TRA     1,4                AND RETURN
*
EVP27  PDX     0,4                SUBR BRANCH
       CLA     0,4
       PAX     0,4                POINTER TO TXL WORD
       SXD     EVTRK,4            TO SAVE POSITION
       TRA     EV27               EVALUATE ARGUMENTS
*
 ELP1  TXL *+1,,0
       SXA     ELT1,4             SAVE LINK IR
       PDX ,4                     J
       CLA ,4
       PAX ,4
       PXD ,4                     CAR(J)
       LDQ     EVSA               GET CURRENT A LIST
 ELT1  AXT     **,4               RESTORE LINK IR
       TRA $EVAL
*
*      EVLIS
*
 EVLIS SXD     EVS1,4             SAVE LINK IR
       AXT     EVLISL,4           ATOM EVLIS
       SXA     EVS1,4             FOR BACKTRACE
       TSX     $SAVE,4            SAVE EVAL STORAGE
       TXL     $END3,,EVSA+2                                            PAGE 160
       STQ     EVSA
       LDQ     ELP1
       TSX     MAPLIS,4
       TSX     UNSAVE,4
       LXD     EVS1,4
       TRA     1,4
*
 EVP26 SXD $ERROR,4
       LXD     EVT1,2
       CLA EVTE
       TSX $ERROR+1,4
       BCI     1,*A  9*           FUNCTION OBJECT HAS NO DEFINITION EVAL
*
EVTFS  PAX     0,4                ATOM NAME
       PXD     0,4                TO PRINT POSITION
       LDQ     EVTDE
       TSX     ARGOF,4            PRINT ARGUMENT MESSAGE
       LDQ     $ALIST             RESTORE ALIST AFTER ARGOF
       CLA     EVTDE              AND ARGUMENT LIST
       TSX     EVT1,4             DO THE FSUBR
       TSX     UNSAVE,4           RESTORE THE IR
       XCA                        VALUE TO MQ
       LXA     CSV,4              GET ATOM NAME FOR VALUE MESSAGE
       PXD     0,4                TO AC
       LXD     CSV,4              AND RETURN IR4
       TRA     VALOF              PRINT VALUE MESSAGE
*
EVTXP  STD     EVTDE              SAVE LAMBDA EXPRESSION
       LXA     EVS1,4             GET ATOMIC FUNCTION
       PXD     0,4                TO PRINT POSITION
       TSX     ARGOF,4            PRINT ARGUMENT MESSAGE
       TSX     $SAVE,4            SAVE THERETURN IX
       TXL     $END1,,EVS1+2
       LDQ     EVT1               RESTORE THE LIST OF ARGUMENTS
       CLA     EVTDE              AND THE LAMBDA EXPRESSION
       TSX     $APPLY,4           APPLY THE FUNCTION TO ITS ARGS
       TSX     UNSAVE,4
       XCA                        PUT VALUE IN AC
       LXA     EVS1,4             NAME OF ROUTINE TRACED
       PXD     0,4                PUT IN AC
       LXD     EVS1,4             LINK IR
       TRA     VALOF              PRINT VALUE OF STATEMETN
*
*      ARGOF   PRINTS ARGUMENTS OF  NAME FOLLOWED BY THE LIST OF ARGUMEN
*
 ARGOF SXA     PRX,4              SAVE INDEX REGISTERS
       SXA     PRY,2
       STO     AGA                SAVE ATOM NAME
       STQ     AGQ                SAVE LIST OF ARGUMENTS
       TSX     TERPRI,4           PRINT A BLANK LINE
       AXT     3,2                PRINT2 OUT 3 WORDS
       CLA     AGM+3,2
       TSX     $PRINT2,4
       TIX     *-2,2,1            LOOP                                  PAGE 161
       CLA     AGA
       TSX     $PRINT,4           PRINT OUT THE LINE
       LXD     AGQ,2              START THE PRINLIS
 PLL   TXL     PRY,2,0            EXIT IF END OF LIST
       CLA     0,2                NEXT ITEM
       PDX     0,2                CDR OF LIST
       PAX     0,4                CAR
       PXD     0,4
       TSX     $PRINT,4
       TRA     PLL                GET NEXT ITEM
 PRY   AXT     **,2               RESTORE INDEX REGISTERS
 PRX   AXT     **,4
       TRA     1,4                EXIT
*
 AGA                              TEMPORARY STORAGE
 AGQ
 AGM   BCI     1,ARGUME
       OCT     456362607777       ARGUMENTS
 AGO   OCT     462640777777       OF
 VALV  BCI     1,VALUE
*
*      VALOF   PRINTS VALUE OF NAME FOLLOWED BY ONE LIST
*              SHARES STORAGE WITH ARGOF ROUTINE
*
 VALOF SXA     VAX,4              SAVE LINK IR
       STO     AGA                ATOM NAME
       STQ     AGQ                VALUE OF EXPRESSION
       TSX     TERPRI,4           PRINT A BLANK LINE
       CLA     VALV               WORD VALUE
       TSX     $PRIN2,4           PUT IN OUTPUT LINE
       CLA     AGO                WORD OF
       TSX     $PRIN2,4
       CLA     AGA                ATOM
       TSX     $PRINT,4           PRINT OUT THE LINE
       CLA     AGQ                VALUE
 VAX   AXT     **,4               RESTORE LINK IR
       TRA     $PRINT             PRINT OUT VALUE AND RETURN
 EVTE                             E
 EVTAE                            CAR(E)
 EVTA                             A
 EVT1
 EVD1
 EVLNS                            TST CELL FOR NUMBERS
 EVCM  TXL     $END4,,EVTRK+2
 EVZRO SYN     $ZERO
 EVQD1 SYN     $QD1
* INTER        MULTIPLE LISP STATEMENT PROGRAM FEATURE INTERPRETER
*              RECODED TO MAKE THE INTERPRETER AND COMPILER PROGRAM
*              FEATURE UNDER STAND THE SAME LANGUAGE
*
R      HED
 INTER SXD     INTRX,4            SAVE LINK IR
       TSX     $SAVE,4            SAVE PROTECTED TEMPORARY STORAGE
       TXL     $END5,,INTGS+2     SAVE 5 ITEMS                          PAGE 162
       SXA     INTGL,2              SAVE INDEX REGISTER 2
       STQ     INTPL              SAVE PAIR LIST
       STZ     INTGS              ZERO THE GO SWITCH
       PDX     0,4                POINTER TO PROGRAM
       CLA     0,4                FIRST WORD
       STD     INTB               POINTER TO BEGINNING OF PROGRAM
       STD     INTE               DITTO
       PAX     0,4                POINTER TO LIST OF PROGRAM VARIABL    CLIPSCAN
       PXD     0,4                TO DECREMENT                          
       LDQ     INTFB              FUNCTIONAL ARGUMENT                   
       TSX     MAPLIS,4           (MAPLIST PV (LAMBDA (L) (CONS (CA     CLIPSCAN
       LDQ     INTPL              NIL)))   PICK UP PAIR LIST
       TSX     $NCONC,4           ATTACH PROGARM VARIBLES TO PAIR L     CLIPSCAN
       STO     INTPL              PUT IN PAIR LISDT REGISTER
       LDQ     $ZERO              ZERO THE MQ
 INTGM LXD     INTE,4             SEARCH PROGRAM FOR GO TO POINTS
       TXL     INTAA,4,0          GO IF END OF PROGRAM
       CLA     0,4                NEXT WORD
       STD     INTE               SAVE CDR
       PAX     0,2                CAR
       CLA     0,2                MAKE ATOM TEST
       PAX     0,2
       TXL     INTGM,2,-1         GO IF NOT AN ATOM
       PXD     0,4                IS AN ATOM, PUT POINTER TO CURREN     CLIPSCAN
       TSX     $CONS,4            PUT ON GO LOST
       XCA                        ANSWER TO MQ
       TRA     INTGM              NEXT ITEM
 INTAA SLQ     INTGL              ALL DONE, STORE GO LIST
 INTGA LXD     INTB,4,0           NEXT PROGRAM LOCATION
       TXL     INTRN,4,0          RETURN WITH NIL IF RAN OUT OF STA     CLIPSCAN
       CLA     0,4                NEXT WORD
       STD     INTB               SAVE CDR
       PAX     0,4                CAR
       CLA     0,4                FIRST WORD
       PAX     0,2                CHECK FOR ATOM OR $COND
       TXH     INTGA,2,-1         GO TO NEXT STEP IF ATOM
       TXL     INTEV,2,$COND-1    GO TO EVAL IF NOT $COND
       TXH     INTEV,2,$COND
       PDX     0,2                IS $COND DO AN EVCOND
 INTEB TXL     INTGA,2,0          GO TO NEXT STEP IF COND UNSATISFIED   CLIPSCAN
       CLA     0,2                FIRST COND STATEMENT
       PDX     0,2                CDR
       PAX     0,4                FIRST SUB COND
       CLA     0,4
       PDX     0,4                POINTER TO THEN PART
       SXA     INTB,4             SAVE IN PROTECTED STORAGE
       PAX     0,4                POINTRE TO IF PART
       PXD     0,4                PUT IN DECREMENT
       LDQ     INTPL              PAIR LIST
       TSX     $EVAL,4            EVALUATE IT
       TZE     INTEB              GO IF IF PART IS FALSE
       LXA     INTB,4             GET THEN PART
       CLA     0,4
       PXA     0,4                PPRINTER TPO THEN PART                PAGE 163
 INTEV PXD     0,4                LIST TO BE EVALUATED
       LDQ     INTPL              GET PAIR LIST
       TSX     $EVAL,4            EVALUATE IT
       NZT     INTGS              SEE IF GO SWITCH SET
       TRA     INTGA              GO TO NEXT STATEMENT
       LXA     INTGS,4            WAS SET, SEE IF GO OR RETURN
       TXH     INTRN,4,-2         TRA IF RETURN
       PXD     0,4                POINTER TO ITEM
       LDQ     INTFC              GET SASSOC FUNCTIONAL ARGUMENT
       STQ     $ARG3              PUT IN $ARG3
       LDQ     INTGL              GET GO LIST
       TSX     SASSOC,4           SEARCH FOR ATOM
       PDX     0,4                POINTRE TP  PROGRAM POINT
       CLA     0,4                TAKE CDR
       STD     INTB               SET PROGRAM POINT
       STZ     INTGS              ZERO THE GO SWITCH
       TRA     INTGA              GO TO THAT STATEMENT
*
INTFB  TXL     *+1,,1             MAPLIST FUNCTIONAL ARGUMENT
       SXA     INTFX,4            (LAMBDA (L) (CONS (CAR L) NIL))
       PDX     0,4
       CLA     0,4
       PAX     0,4
       PXD     0,4
       LDQ     $ZERO
 INTFX AXT     **,4
       TRA     $CONS
*
 INTFC TXL     *+1,,1             UNLABELED GO TO POINT ERROR
       SXD     $ERROR,4           SAVE LINK IR
       LXA     INTGS,4            POINTER TO GO POINT LABEL
       PXD     0,4                PUT IN DECREMENT
       LXA     INTGL,2            RESTORE INDEX REGISTER 2
       TSX     $ERROR+1,4         GO TO ERROR
       BCI     1,*A  6*                GO TO POINT NOT LABELED
*
 INTRN LXD     INTGS,4            RETURN VALUE
       PXD     0,4                PUT IN DECREMENT
       STZ     INTGS              ZERO THE GO SWITCH
       LXA     INTGL,2            RESTORE INDEX REGISTER 2
       TSX     UNSAVE,4           RESTORE PROTECTED STORAGE
       LXD     INTRX,4            RESTORE LINK IR
       TRA     1,4
*              TEMPORARY STORAGE FOR INTERPRETERS
 INTE                             TEMPORARY STORAGE
PRGVAR SYN     INTPL
*
*
* RETURN                          SPECIAL PROGRAM SETS RETURN SWITCH
*                                 IN PROGRAM INTERPRETER
*
RETURN ORA     $AMASK             SIGNAL THAT IT IS A RETURN
       STO     INTGS              SET UP GO SWITCH
       CLA     $QD1               PICK UP TRUTH VALUE                   PAGE 164
       TRA     1,4                EXIT
*
* GO           SPECIAL FORM FOR PROGRAM INTERPRETER, GIVES GO TO POINT
*
GOGOGO SXD     GOX,4              SAVE LINK IR
       PDX     0,4                POINTER TO ARGUMENT LIST
       CLA     0,4
       STA     INTGS              PUT GAR IN GO SWITCH
       PAX     0,4                CAR TO IR
       CLA     0,4                GET FIRST WORD
       PAX     0,4                SEE IF ATOMIC
       TXH     GOT,4,-2           EXIT TRUE IF ATIMIC
       LXA     INTGS,4            OTHERWISE GET ARGUMENT
       PXD     0,4                PUT INDECREMENT
       TSX     $SAVE,4            SAVE LINK IR
       TXL     $END1,,GOX+2       SAVE 1 ITEM
       TSX     $EVAL,4            EVALUATE THE ARGUMENT
       TSX     UNSAVE,4           RSTORE LINK IR
       PDX     0,4                VALUE
       SXA     INTGS,4            PU IN GO SWITCH
 GOT   CLA     $QD1               TRUTH VALUE
       LXD     GOX,4              RESTORE LINK IR
       TRA     1,4                EXIT
*
       DECK                       LAP PART ONE
       HEAD    C                  THIS IS THE COMPILER AND ASMBLR
*
* LAP IS THE ASSEMBLER. ONE ARG IS LISTING.  IT IS LIST OF INSTRUC-
* TIONS, NON-ATOMIC OR NIL. THE ATOMIC SYMBOLS ARE LOCATION SYMBOLS
* SECOND ARG IS START OF SYMBIL TABLE WHICH IS AN A-LIST.
* THE FIRST ITEM IS ORG AS FOLLOWS-
*      NIL= IN BPS
*      ATOM= AT SYMBOLIC LOCATION
*      NUM= ATHIS NUMBER
*      (NAME TYPE NUM) = IN BPS, AND PUT TXL ON PROP LIST OF NAME
*      WITH FLAG TYPE AND NUM (B DEC. OF TXL.
* INSTRUCTION FORMAT IS (OP ADDR TAG DEC)
* FIELD FORMAT IS AS FOLLOWS-
*      TEMP SYMBOL
*      NUMBER
*      SYM SUBR OR FSUBR
*      (E NAME) FOR IMMEDIATE AS IN TXL FILTER
*      (QUOTE NAME) FOR IMTE IN DEC OF WORD ON QTLST
*      POINTER TO COMMON WORD.MAKES ONE IF NONE ALREADY
*      SUM OF ANY OF ABOVE
* LAP IS IDENTITY FUNCTION
*      LAP DOES NOT USE IX1. IX2,4 ARE SCARTCH
* ERRORS IN LAP AS FOLLOWS-
*      *L  1*  UNABLE TO EVALUATE ORIGIN
*      *L  2*  OUT OF BPS DISCOVERED AFTER PASS 1
*      *L  3* UNDEFINED SYMBOL
*      *L  4*  FIELD WAS RECURSIVE
*
LAP    SXA     LAX,4                                                    PAGE 165
       SXA     LAX+1,2
       STO     LIST               THIS IS THE INPUT
       STQ     TAB                START OF SYMBOL TABLE
       PDX     0,4
       CLA     0,4
       STD     REST               SAVE REST OF LISTING
       PAX     0,2                ORIGIN IN IX2
       TXL     INBP,2,0           NIL MEANS  BPS ASSEMBLY
       CLA     0,2
       PAX     0,4                CAR OF ORIGIN
       TXL     INBP,4,-2          NOT ATOM MEANS BPS MODE SO GO
       STL     MODE               NOISE = NOT BPS
       PXD     0,2                MAKE NUMBER TEST
       TSX     NUMBRP,4
       TNZ     LSQ                IF A NUMBER
       PXD     0,2                ORIGIN TO AC
       LDQ     $QSYMD,4           (QUOTE SYM)
       TSX     GET,4
       TNZ     *+4                ORIGINA WAS FIOUND
       SXD     $ERROR,4
       PXD     0,2                SHOW IT
       TSX     $ERROR+1,4         UNDEFINED ORIGIN
       BCI     1,*L  1*
LSQ    PXD     0,2
       TSX     NUMVAL,4           GET NUMERICAL VALUE
LSO    PDX     0,4
       CLA     0,4                PUTS SYM IN AC FOR NOT BPS MODE
       TRA     *+4
INBP   CLA     $ORG               PUTS ORG IN AC FOR BPS MODE
       STZ     MODE               INDICATES BPS MODE
       TSX     JUST,4             JUSTIFY AC
       STO     STAR               UPDATE MARKER
       STO     START              RESET MARKER
       STZ     PASWD              INDICATE PASS 1
       TSX     PASS,4
       CLA     TAB
       TSX     $PRINT,4           PRINT SYMBOL TABLE
       ZET     MODE
       TRA     *+3                IF NOT IN BPS MODE
       LXA     STAR,4
       XEC     LBPTP              TEST FOR OUT OF BPS
       LXA     START,4            RESET STAR FOR SECOND PASS
       SXA     STAR,4
       LXD     LIST,4
       CLA     0,4
       STD     REST               USED BY PASS AGAIN
       STL     PASWD              NOISE MEANS PASS 2
       TSX     PASS,4             FOR PASS 2
       ZET     MODE
       TRA     LEND               IF NOT IN BPS MODE
       LXA     STAR,4             RSET ORG FOR NEXT ASSEMBLY
       SXA     $ORG,4
       LXD     LIST,4
       CLA     0,4                CWR OF LISTING                        PAGE 166
       PAX     0,4
       CLA     0,4                GET CWR OF ORIGIN
       PAX     0,2                CAR OF ORIGIN
       PDX     0,4                CDR OF ORIGIN
       TXH     LEND,2,-2          IF ATOM THEN NO TXL NEEDED
       SXD     NAME,2             CAR OF ORG IS NAME
       CLA     0,4
       PAX     0,2                CADR OF ORIGIN IS TYPE
       SXD     TYPE,2             STORE TYPE
       ANA     $DMASK             CDDR IS NOE IN AC
       TSX     CADARX,4           CADAR PUTS PART OF NUM IN DECR OF AC
       STD     INDC               FOR TXL WORD
       LXA     START,4
       SXA     INDC,4             COMPLETES TXL WORD
       CLA     NAME
       TSX     PRO,4
       LDQ     TYPE
       TSX     GET,4
       TZE     MKIND              IF THERE WAS NO OLD TXL
       PDX     0,2                SAVE POINTER TO TXL
       CLA     0,2                CWR OF OLD TXL
       PAC     0,4                POINTER TO OLD BIN PTROG.
       CLA     START              START OF NEW PROGRAM
       ORA     PATCH              MAKE TRA INSTRUCTION
       STO     0,4                CLOBBER OLD PROG.
       CLA     INDC
       STO     0,2                ON TOP OF OLD TXL
       TRA     LEND
MKIND  CLA     INDC
       LXD     TYPE,4             SYM SHOULD HAVE TO TXL ON POINTER
       TXL     IND2,4,$SYM-1
       TXH     IND2,4,$SYM
       ANA     $AMASK
 IND2  TSX     $CONSW,4
       XCA                        SAVE AC
       LXD     NAME,2             NAME OF SUBR OR TYOE
       CLA     0,2
       ANA     $DMASK             CDR OF NAME NOW IN AC
       XCA
       TSX     $CONS,4            CONS (TXL,RESTOF PROPERTY LIST)
       XCA
       CLA     TYPE
       TSX     $CONS,4            CONS,TYPE,RST OF ATM)
       STD     0,2                RPLACD OF PROPERTY 9IST
LEND   CLA     LIST
       STZ     LIST
       STZ     TAB
       STZ     INST
*      DONT STORE ZERO IN QTLST
LAX    AXT     **,4
       AXT     **,2
       TRA     1,4
* ALL LAP REGISTERS FOLLOW,INCL. THOSE USED BY SUBROUTINES
NAME                              NAME OF FUNCTION                      PAGE 167
TYPE                              SUBR FSUBR ETC
INDC   TXL     **,,**             FOR TSL WORD
PATCH  TRA     **                 FOR CLOBBER INSTRUCTION
       DECK                       PERM PROTECTED LAP STORAGE
PROBE  SYN     *                  BEGINNING OF PROTECTED AREA
LIST                              MAIN LISTING GOES HERE
QTLST                             THE LIST OF QUOTES.NEVER ERASE
TAB                               TEMPORARY SYM TABLE
LCOM                              STORAGE FOR COMMON ONLY.PROTECTED
PROS                              PROTECTED FUNCTION NAMES AND SPECIALS
PROEN  SYN     *-1                END OF PROTECTED AREA
       DECK                       LAP PART TWO
INST                              HOLDS CURRENT INSTRUCTION OR FRACTION
REST                              REMAINDER OF LISTING. PASS ALTERS THIS
STAR                              * DIRECT ADDRESS POINTER TO CURRENT LO
START                             RESET CELL FOR *
PASWD                             ZERO MEANS PASS 1. NOISE = PASS 2
MODE                              ZERO MEANS BPS ASSEMBLY
HOLD                              SCRATCH CELL FOR AFELD ONLY.WATCH OUT
SUM                               FOR USE BY AFELD LIST ONLY
NOCUR                             FOR AFELD LIST ONLY.PREVENTS RECURSION
REM                               FOR AFELD LIST ONLY.
ALST           $ALIST
ERCC
LSAC   TXL     *+1,,0
       PXD     0,0
       TRA     1,4
       DECK                       ATOM PIECES
MOV            MOVE               THE WORD POINTED TO BY SYM ON *MOVE
LSTR           LST                POINTED TO BY SYM ON ATOM *LIST
RTRN           RESTOR
       DECK                       LAP PART THREE
*      ADDR(REM)=IX4 SAVED.DECR=REST OF LIST FIELD
*
* LBPTP CHECKS FOR OUT OF BPS AND MAKES ERROR IF D SO.
LBPTP  TXH     *+1,4,**           SETUP FILLS THIS CELL
       SXD     $ERROR,4
       PXD     0,4
       LDQ     $OCTD
       TSX     $MKNO,4
       TSX     $ERROR+1,4
       BCI     1,*L  2*
*
*  JUST REDUCES THE AC MOD 2**15.THE RESULT IS 15 BITS IN ADDR OF AC
*   IT IS ALWAYS POSITIVE
JUST   TPL     *+3
       COM
       SUB     $Q1
       ANA     $AMASK
       TNO     1,4
       TRA     1,4
*
*  PASS DOES BOTH PASSES FOR LAP
*      FIRST PASS MAKES SYMBOL TABLE AND UPDATES ON INSTRUCTIONS        PAGE 168
*      SECOND PASS IGNORES SYMBOLS ASSEMBLES AND UPDTS INSTRUCTIONS
PASS   SXA     PAUX,4
LOP1   LXD     REST,4
       TXL     PAUX,4,0           IF NO MORE LISTING
       CLA     0,4
       STD     REST               RESET REST OF LISTING
       PAX     0,4
       SXD     INST,4
       TXL     AMBL,4,0           IF NIL
       CLA     0,4
       PAX     0,4
       TXL     AMBL,4,-2          IF NOT ATOMO
       ZET     PASWD
       TRA     LOP1               IF PASS 2
       CLA     STAR               OTHERWISE ADD    TO TABLE
       LDQ     $OCTD
       TSX     $MKNO,4            MAKE A NUMBER
       XCA
       CLA     INST
       TSX     $CONS,4            (NAME.VALUE)
       LDQ     TAB
       TSX     $CONS,4
       STO     TAB
       TRA     LOP1
AMBL   ZET     PASWD              LAND HERE IF INSTRUCTION NOT SYMBOL
       TSX     AINS,4             ON PASS 2 ONLY
       LXA     STAR,4
       TXI     *+1,4,1            UPDATE * AFTER INSTRUCTION IS ASSEMBLE
       SXA     STAR,4
       TRA     LOP1
PAUX   AXT     **,4
       TRA     1,4
*
* AINS IS THE INSTRUCTION ASSEMBLER. ARG IS IN INST. VAL IS IN AC
AINS   SXA     AINX,4
       TSX     AFELD,4
       STO*    STAR
       TSX     AFELD,4
       TSX     JUST,4
       ORS*    STAR               THIS IS ADDRESS FIELD
       TSX     AFELD,4
       ALS     15
       TOV     *+1
       ORS*    STAR               TAG FIELD
       TSX     AFELD,4
       TSX     JUST,4
       ALS     18                 NO OVERFLOW AFTER JUST
       ORS*    STAR
AINX   AXT     **,4
       TRA     ,14
*
* AFELD IS THE FIELD EVALUATOR. A LIST OF FIELDS IS EXPECTED IN INST.
* IT EVALUATES THE FIRST AND SETS INST TO THE REST. IF NO MORE FIELDS LE
* ARE LEFT, IT GOES TOAINX, THE EXIT POINT OF AINS                      PAGE 169
* AFELD HAS CERTAIN PRIVATE CELLS,SEE AFTER LAP.) THE LIST AFELD IS A
* SLIGHTLY RECURSIVE DEVISE WHICH HAS SPECIAL CELLS AND CANNOT REENTER I
* ITSELF WITHOUT ERROR.
AFELD  SXA     FELX,4
       STZ     NOCUR
       LXD     INST,4
       TXL     AINX,4,0           IF NO MORE FIELDS
       CLA     0,4
       STD     INST               REST OF FIELDS
       PAX     0,2
LEM    CLA     0,2
       PAX     0,4
       TXL     NATM,4,-2          IF NOT ATOMIC FIELD
       TXH     *+3,2,0
       CLA     $ORG               NIL SYMBOL MEANS ORIGIN
       TRA     FELX
       PXD     0,2
       LDQ     LSAC               FN ARG FOR SASSOC
       STQ     $ARG3
       LDQ     TAB
       TSX     SASSOC,4           LOOK UP IN SYM TABLE
       TZE     NTAB               NOT IN TAB
       TSX     $CDRP,4
NEVAL  TSX     NUMVAL,4
       PDX     0,4
       CLA     0,4
       TRA     FELX
NTAB   PXD     0,2
       TSX     NUMBRP,4
       TZE     *+3                IF NOT A NUMBER
       PXD     0,2                LISP NUMBER IN AC
       TRA     NEVAL
       SXD     ERCC,2             SAVE ATOM
LOP2   CLA     0,2     LOOP FOR SYM,SUBR,FSUBR
       PDX     0,2
       PAX     0,4
       TXH     PA,2,0             IF NOT NIL
       SXD     $ERROR,4
       CLA     ERCC
       TSX     $ERROR+1,4
       BCI     1,*L  3*           UNDEFINED SYMBOL
PA     TXL     *+2,4,$SYM-1
       TXL     FINX,4,$SYM
       TXL     *+2,4,$SUBR-1
       TXL     FIND,4,$SUBR
       TXL     LOP2,4,$FSUBR-1
       TXH     LOP2,4,$FSUBR
FIND   CLA     0,2
       PAX     0,4
       CLA     0,4
       ANA     $AMASK
       TRA     FELX
*
FINX   CLA     0,2                                                      PAGE 170
       PAX     0,4
       CLA     0,4
       TRA     FELX
*
NATM   TXL     NTE,4,$H25-1
       TXH     NTE,4,$H25         FOR (E EXP)
       PXD     0,2                ENTIRE FIELD
       TSX     CADRXX,4
       ARS     18
       TRA     FELX
NTE    TXL     NQT,4,$QUOTE-1     LAND HERE FOR NOT (...
       TXH     NQT,4,$QUOTE       ABOVE AND THIS FOR (QUOTE...
       PDX     0,2                AC HAS CWR OF FIELD
       CLA     0,2
       PAX     0,2                POINTER TO EQ QUANTITY
       LXD     QTLST,4
       TXL     NON,4,0            TEST FOR NO LIST
FLOOP  CLA     0,4                AN EQUAL TYPE SEARCH
       STO     HOLD               TEMPORARY SAVING OF REST
       PAX     0,4
       CLA     0,4
       ANA     $DMASK             LITREAL QUANTITY FOR EQUAL COMPARISON
       XCA
       PAXD    0,2                THE NEW ITEM
       TSX     $EQUAL,4           TEST FOR EUQALITY
       TNZ     ONQT               IF ALREADY ON LIST
       LXD     HOLD,4
       TXH     FLOOP,4,0          IF NOT HEAD OF QTLIST
NON    PXD     0,2                NEED TO MAKE ENTRY
       XCA
       PXA     0,0
       TSX     $CONS              CONS(NIL EXP)
       STO     HOLD               NEEDS NO PROTECTION AS SEEN BY WHAT FO
*              FOLLOWS
       LDQ     QTLST
       TSX     $CONS,4            CONS((NIL.EXP, ...
       STO     QTLST
       LDC     HOLD,4             WANT TRUE POINTER
TRP    PXA     0,4
       TRA     FELX
*      THIS    IS POINTER TO A NIL.EXP WORD IN FREE STORAGE
ONQT   LAC     HOLD,4
       TRA     TRP
NQT    TXL     FDLST,4,SPECAL-1
       TXH     FDLST,4,SPECAL     (SPECIAL NAME)
       LDQ     QSPECD             SPECIAL IN MQ
       ANA     $DMASK             (NAME) IN AC
       TSX     $CARP,4
       TSX     GET,4
       TNZ     SPP                JUST NEED TO ASSURE PROTECTION
       LDQ     $ZERO
       TSX     $CONS,4            AC HAS ZERO IF YOU ARE HERE
       STO     LCOM               PROTECTED TEMP CELL
       PXD     0,2                (SPECIAL NAME)                        PAGE 171
       TSX     CAADRX,4           CDR(NAME)
       PDX     0,2                SAVE ABOVE
       XCA
       CLA     LCOM
       TSX     $CONS,4
       XCA
       CLA     QSPECD
       TSX     $CONS,4
       STD     0,2                RPLACD OF NAME
       TSX     CADRXX,4           POINTER TO (NIL)
SPP    TSX     PRO,4
       PDC     0,4
       TRA     TRP
FDLST  NZT     NOCUR              NO RE-ENTRY TO AFELD LIST IS ALLOWED
       TRA     *+4
       SXD     $ERROR,4
       TSX     $ERROR+1,4
       BCI     1,*L  4*           NO RECURSIVE FIELDS ALLOWED
       STL     NOCUR              PREVENT RECURSION
       STZ     SUM                RESET SUM WORD
       LXA     FELX,4
       SXA     REM,4              SAVES THE RETURN FOR AFELD
LOPL   CLA     0,2
       STD     REM
       PAX     0,2
       AXC     *+2,4
       SXA     FELX,4             REENTER THE EVALUATOR
       TRA     LEM
       ADD     SUM
       STO     SUM
       LXD     REM,2              REST OF FIELDS
       TXH     LOPL,2,0           IF THERE ARE MORE FIELDS (SUBFLDS)
       STZ     NOCUR              ALLOWS ENTRY TO LIST AFELD AGAIN
       LXA     REM,4
       TRA     1,4
FELX   AXT     **,4
       TRA     1,4
*
PRO    SXA     PX,4
       STO     PTR                SAVE ARGUMENT
       STD     PH                 SET UP TXH
       SUB     $QD1               AND
       STD     PL                 TXL SIEVE
       CLA     PROS               GET PROTECTED LIST
PNL    PDX     0,4
       TXL     PMK,4,0            END OF LIST, SO MAKE NEW ENTRY
       CLA     0,4
       PAX     0,4
PL     TXL     PNL,4,**
PH     TXH     PNL,4,**           FALL THROUGH IF FOUND
PX     AXT     **,4
       CLA     PTR                RESTORE AC
       TRA     1,4
*                                                                       PAGE 172
PMK    LDQ     PROS               MAKE A NEW ITEM
       CLA     PTR
       TSX     $CONS,4
       STO     PROS               STORE NEW LIST
       TRA     PX                 AND RETURN
*
PTR
*
*
       DECK                       PERMANENT COMPILER SUBROUTINES
*              LINK HANDLES ALL SUBROUTINE CALLS FROM COMPILED FUNCTION
*              IT REPLACES STR WITH TSX IF SUBROUTINE BEING CALLED
*              IS A SUBR OR FSUBR
*              IT GOES TO APPLY IF THE CALL IS TO EXPR OR FEXPR WITH
*              $ALIST AS THIRD ARGUMENT
*              LINK EXPECTS A TAG OF 7 IN THE STR INST, NAME OF FUNCTION
*              IN THE ADDRESS, AND THE NUMBER OF ARGUMENTS IN THE DECREM
*              ENT                LINK WILL GO TO THE ROUTINE WHICH
*              HANDLES ERROR TRAPS IF THE CALLING INST DOESNT HAVE A 7
*              TAG
*
LINK   STO     LNKA
       STQ     LNKB               SAVE AC AND MQ
       SXA     LER,4              SAVE IR4
       LAC     0,4                COMP POINTER TO STR+1
       TXI     *+1,4,1            MAKE ORDINARY TSX POINTER
       CLA     0,4                GET STR INST  7
       STO     LNKC               SAVE IT
       ANA     TAGMSK             CHECK FOR 7 TAG
       ERA     TAGMSK
       TNZ     LER                IF NOT 7 TAG
       SXD     LNKC,4             SAVE POINTER
       CLA     B$ZERO             RESTORE NIL
       STO     0
       STZ     LNTRS              RESET TRACE SWITCH
       LXA     LNKD,4             FUNCTION ATIM
       CLA     0,4                START PROPERTY LIST SEARCH
LNLP   PDX     0,4
       TXL     LNNF,4,0           NO DEFINITION SO FN VARIABLE
       CLA     0,4
       PAX     0,4
       TXL     *+2,4,$SUBR-1
       TXL     LNSBR,4,$SUBR
       TXL     *+2,4,$FSUBR-1
       TXL     LNSBR,4,$FSUBR
       TXL     *+2,4,$TRACE-1
       TXL     LNTR,4,$TRACE
       TXL     *+2,4,$EXPR-1
       TXL     LNEXP,4,$EXPR
       TXL     LNLP,4,$FEXPR-1
       TXH     LNLP,4,$FEXPR
LNEXP  PDX     0,4                EXPR-FEXPR BRANCH
       CLA     0,4
       PAX     0,4                LAMBDA EXPRESSION                     PAGE 173
LNGN   SXD     LNFN,4             SAVE IT
       CLA     $ALIST
       STO     $ARG3              PROPER ALIST
       ZET     LNTRS              TRACE TEST
       TRA     LNTEX              TRACE EXPR OF FEXPR
       TSX     LNARS,4            LIST ARGUMENTS
       XCA
       CLA     LNFN               LAMBDA EXPRESSION
       LXD     LNKC,4             RETURN IR
       TRA     $APPLY             DO
*
LNNF   LXA     LNKD,4             FUNCTION DEFN IS ON ALIST
       TRA     LNGN               APPLY WILL TAKE CARE OF THIS
*
LNTR   STL     LNTRS              SET TRACE SWITCH
       STO     LNAC               SAVE AC
       TSX     LNARS,4            LIST ARGUMENTS
       STO     LNRGL              AND SAVE THEM
       XCA                        TO PRINT POSITION
       LXA     LNKD,4             ATOM NAME
       SXA     LNKC,4             SAVE WITH INDEX REGISTER
       PXD     0,4                ALSO FOR TRACE MESSAGE
       TSX     $SAVE,4            SAVE NAME AND RETRN
       TSX     $END1,,LNKC+2
       TSX     A$ARGOF,4          PRINT ARGUMENTS
       CLA     LNAC               RESTORE AC
       TRA     LNLP               AND CONTINUE PROPERTY LIST SEARCH
*
LNTEX  CLA     LNFN               TRACE EXPR OR FEXPR
       LDQ     LNRGL              SET UP ARGUMENTS OF APPLY
       TSX     $APPLY,4           AND DO THE FUNCTION
LNTEN  TSX     UNSAVE,4           GET BACK IR4 AND FN NAME
       XCA
       LXA     LNKC,4             ATOM NAME TO AC
       PXD     0,4
       LXD     LNKC,4             RESTORE INDEX
       TRA     A$VALOF            PRINT VALUE MESSAGE
*
LNTSB  STA     LNDIS              TRACE SUBR OF FSUBR
       CLA     LNKA               RESTORE AC
       LDQ     LNKB               AND MQ
LNDIS  TSX     **,4               EXECUTER SUBROUTINE
       TRA     LNTEN              AND REPORT VALUE
*
LNSBR  PDX     0,4                SUBR OR FSUBR BRANCH
       CLA     0,4
       PAX     0,4
       CLA     0,4                TXL SUBR,,N WORD
       ZET     LNTRS              TEST FOR TRACING
       TRA     LNTSB
       STA     LNTSX              MAKE A TSX
       CLA     LNTSX              GET IT
       LXD     LNKC,4             RETURN IR
       STD     0,4                CHANGE THE STR TO TSX                 PAGE 174
       CLA     LNKA               RESTORE AC
       LDQ     LNKB
       TRA     0,4                GO TO NEW TSX
*
LNARS  SXA     LNLX,4             SUBROUTINE WHICH LISTS ARGS
       LXD     LNKD,4             NUMBER OF ARGS
       TXL     LNN,4,0            LST WONT WORK ON ZERO THINGS
       SXD     LNKP,4             PUT IN LST ARG POSITION
       TSX     LST,4              LIST THEM
LNKP   TXH     LNKA,0,**
       PAX     LNKB,0
       PAX     $ARG3,0
       PAX     $ARG4,0
       PAX     $ARG5,0
       PAX     $ARG6,0
       PAX     $ARG7,0
       PAX     $ARG8,0
       PAX     $ARG9,0
       PAX     $ARG10,0
       PAX     $ARG11,0
       PAX     $ARG12,0
       PAX     $ARG13,0
       PAX     $ARG14,0
       PAX     $ARG15,0
       PAX     $ARG16,0
       PAX     $ARG17,0
       PAX     $ARG18,0
       PAX     $ARG19,0
       PAX     $ARG20,0
LNLX   AXT     **,4               RESTORE INTEX
       TRA     1,4
LNN    PXD     0,0                NIL
       TRA     LNLX
*
LER    AXT     *,4                RESTORE IR4
       CLA     LNKA
       TRA     STRPNT             GO TO ERROR HANDLING ROUTINEPP
*              LINK STORAGE
*              IS HERE, EXCEPT FOR LINKA NAD LINKB WHICH ARE IN GARB    CLIPSCAN
LNTRS                             TRACE SWITCH
LNFN                              FUNCTION DEFINITION
LNAC                              TEMPORARY AC STORAGE
LNRGL                             ARGS LISTED DURNING TRANCE INTERLUD   CLIPSCAN
LNKC                              IR4 POINTRE TO STR WORD
LNKD                              CONTAINS STR NAME,7,NUM
LNTSX  TSX     **,4               INSTRUCTION TO BE PLANTED
*              LST IS THE SUBROUTINE WHICH DOES LISTING IN COMPILED
*              FUNCTION           N ELEMENTS HWERE N IS IN AC, ARE
*              LISTED             ARGUMENTS ARE GOTTEN BY CLA*
*              FROM THE N REGISTERS SUCEDING THE CALL
*
LST    SXA     LX2,2              SAVE IR2
       CLA     1,4                TO GET N FROM FIRSTDECREMENT
       STD     LSN                TO DECREMENT IR4 FOR POINT EXIT       PAGE 175
       STD     LSC                TO DECREMENT THE CONS COUNTER
LSN    TNX     *+1,4,**
       PDX     0,2                N TO IR2
       PXD     0,4                START TO COMPLEMENT IR4
       PDC     0,4                OH FOR A 7094
       TXI     *+1,4,1            ONE MORE FOR EXIT
       SXA     LSP,4              SET UP GET INST
       SXA     LSE,4              AND RETURN
       LXD     $FREE,4            FIRST FREE WORD
       TXH     *+2,4,0            TEST FOR OUT OF FREE
       TSX     $FROUT,4           WILL RETURN -2,4
       SXD     LAN,4              THE ANSWER TO THIS SAUSAGE CONS
       LXA     $CNTR1,4           GET CONS COUNTER
LSC    TIX     *+3,4,**           REDUCE IT BY N
       TSX     ARREST,4           OUT OF CONSES
       AXT     -1,4               RESET COUNTRE (UP TO N CONSES MAY BE
       SXA     $CNTR1,4           LOST EVERY 7777 OCTAL CONSES)
       LXD     LAN,4              RESTORE IR4 TO FREE WORD POINTER
LSP    CLA*    **,2               GET ARGUMENT
       ARS     18                 TO ADDRESS
       STA     0,4                PUT IT IN THE FREE WORD ADDR
       CLA     0,4                NEXT FREE WORD
       SXA     LFX,4              SAVE PRECEDING WORD TO CUT OFF
LSR    PDX     0,4                NEXT FREE WORD TO IR
       TXL     LFIX,4,0           OUT OF FREE STORAGE7
       TIX     LSP,2,1            COUNT DOWN
       STD     $FREE              RESTORE FREE
       PXD     0,0                CLEAR
LFX    AXT     **,4               LAST WORD IN LIST
       STD     0,4                GETS NIL IN ITS DECREMENT
LX2    AXT     **,2               RESTORE IR2
       CLA     LAN                GET THE ANSWER
LSE    TRA     **                 RETURN
LFIX   CLA     LAN                TO GET IT PROTECTED DURING MOP UP
       TSX     RECLAM,4
       CLA     $FREE              FIX UP THE SAUSAGE
       XEC     LFX                GET LAST WORD TO IR
       STD     0,4                FIX ITS DECREMENT
       TRA     LSR
LAN    PZE
*      UNWND IS UNSAVE FOR COMPILED FUNCTIONS, USED BY ERRORSET
*      TO RESTORE THE PDL TO PRISTINE STATE
UNWND  SXA     UNR,4              SAVE RETURN
       SXA     UNR+1,2            SAVE IR2
       LXD     $CPPI,4            $CPPI IS COMPLEMENT OF PDL POSITOIN
       CLA     -1,4               SO THIS GETS  STR 0,,N
       STO     UNJ                SAVE N TO RESTORE PDL
       SUB     $QD1               AND SET UP TEST WHICH SAYS THAT
       STO     UNH                WE HABE CRAWLED UP THE PDL ALL WAY
       LDC     $CPPI,4            NEED TRUE POINTER FOR CALLING WORDS
       SXA     UNG,4              IN VERSE ORDER FROM PDL
       AXT     1,4                INITIALIZE THE RECALL LOOP
UNF    TXI     *+1,4,1            INCREMENT THE GET IR
UNH    TXH     UND,4,**           TEST FOR LAST WORD RESTORED           PAGE 176
UNG    CLA     **,4               GET SAVED ITEM (GOING FROM BOT TO TOP)
       PAX     0,2                ZERO ADDRESS INTICATES NOT NECESS RES
       TNX     UNF,2,0            FALL THROUGH IS TO RESTORE WORD
       PAC     0,2                ADDR IS TRUE POINTER TO LOCATION
       TXI     UNH,4,1            WOK ON NEXT ONE
UND    LXD     $CPPI,4            PUSH UP $CPPI
UNJ    TXI     *+1,4,**           BY N
       SXD     $CPPI,4
UNR    AXT     **,4               RESTORE LINK
       AXT     **,2               AND IR2
       TRA     1,4
*   MOVE IS A SPECIAL COMPILER SERVICE SUBROTUINE WITH BAD CALLING.
*      TSX     *MOVE,1
*      TNX     NAME,1,*MN
MOVE   SXA     MOVY,1
       LXD     $CPPI,1            PICK UP PDL PPINTER
       STO     1,1                SAVE AC
       STQ     2,1
       SXD     TXLW,4             SAVE RETURN INDEX
MOVY   AXT     **,4               PICK UP REFERECE TO CALLING HEAD
       CLA     1,4                TNX WORD HAS NAME IN ADDR.
       STA     TXLW               COMPLETES THE TXL WORD
       STD     STRW               PUT N IN STRW DECREMENT
       CLA     TXLW               
       STO     0,1                PUT IT AT HEAD OF PDL BLOCK
       CLA     0,4                TSX HAS COUNT FIELD
       ANA     CNTMSK             COUNT FIELD MASK
       TZE     MOVD               IF LESS THAN 3 ARGS
       PDX     0,4                COUNT FIELD TO IX
       TRA     MOVD-1,4           ENTER PART OF MOVE ROUTINE
       CLA     $ARG20
       STO     20,1
       CLA     $ARG19
       STO     19,1
       CLA     $ARG18
       STO     18,1
       CLA     $ARG17
       STO     17,1
       CLA     $ARG16
       STO     16,1
       CLA     $ARG15
       STO     15,1
       CLA     $ARG14
       STO     14,1
       CLA     $ARG13
       STO     13,1
       CLA     $ARG12
       STO     12,1
       CLA     $ARG11
       STO     11,1
       CLA     $ARG10
       STO     10,1
       CLA     $ARG9
       STO     9,1                                                      PAGE 177
       CLA     $ARG8
       STO     8,1
       CLA     $ARG7
       STO     7,1
       CLA     $ARG6
       STO     6,1
       CLA     $ARG5
       STO     5,1
       CLA     $ARG4
       STO     4,1
       CLA     $ARG3
       STO     3,1
       LXA     MOVY,4             RESTORE IR4
MOVD   XEC     1,4         XECED TNX DECREMENTS TXI BUT NO TRANSFER
       SXD     $CPPI,1            KEEP CPPI UP TO DATE ALSO
       XEC     ENDPDL             TEST FOR OUT OF BPS
       CLA     STRW               CREATES SECOND PARAMETER WORD
       STO     -1,1               PUT AT VERY END OF BLOCK
       TRA     2,4                RETURN FROM LINK
STRW   STR     **
TXLW   TXL     **,,**
*
* RESTOR PICKS UP IX4 FROM PDL,SETS BACK CPPI ,AND EXITS.
RESTOR SXD     $CPPI,1
       XCA                        SAVE VALUE OF FUNCTION
       CLA     0,1                PICK UP RETURN WORD
       PDX     0,4                RESTORE IX4
       XCA                        RESTORE AC
       TRA     1,4                EXIT
*
       DECK                       PERMANENT ATOMS
TOPROG BSS     0
       EJECT                                                            PAGE 178
       ORG     27800              PERMANENT OBJECTS START HERE
LOWERP BSS     1                  LWER LIMIT OF PERMENANT LIST STRUCTURE
       REM *********************HEAD OR HED*****************************
0     HED   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
       REM
       REM LOWER LIMIT OF PERM. LIST STRUCTURE
       REM
                                  LAST BUCKET
       TITLE                      ** TO AVOID DUP EXPANSION BY ASM7090...
       DUP     1,125              MAKE BUCKETS
               ,,-*+1
       DETAIL                     ** TO AVOID DUP EXPANSION BY ASM7090...
BUCKET         ,,-*+1             POINTER TO BUCKETS
OBLIST SYN     BUCKET
       REM
** OUR ASSEMBLER CAN'T HANDLE -)SYM EXPRESSIONS
       REM
ZZALST SYN )ALST
ZZ069B SYN )069B
ZZ069A SYN )069A
       EJECT                                                            PAGE 179
       HEAD    0
       REM OBJECT LIST
       REM
OBLB       -1,,-*-1
               -II14,,-*-1        ADD 1                                 GENER000
               -ZZALST,,-*-1                                            
           AND,,-*-1                                                    GENER002
           F1,,-*-1                                                     GENER003
           F18,,-*-1                                                    GENER004
           APVAL,,-*-1                                                  GENER005
               -II1,,-*-1         ARRAY                                 GENER006
           ATOM,,-*-1                                                   GENER007
           F29,,-*-1                                                    GENER008
           CAR,,-*-1                                                    GENER009
           CDR,,-*-1                                                    GENER010
           CAAR,,-*-1                                                   GENER011
           CDAR,,-*-1                                                   GENER012
           CADR,,-*-1                                                   GENER013
           CDDR,,-*-1                                                   GENER014
           CAAAR,,-*-1                                                  GENER015
           CAADR,,-*-1                                                  GENER016
           CADAR,,-*-1                                                  GENER017
           CADDR,,-*-1                                                  GENER018
           CDAAR,,-*-1                                                  GENER019
           CDADR,,-*-1                                                  GENER020
           CDDAR,,-*-1                                                  GENER021
           CDDDR,,-*-1                                                  GENER022
           COND,,-*-1                                                   GENER023
           CONSN,,-*-1                                                  GENER024
           COPYN,,-*-1                                                  GENER025
               DUMP,,-*-1                                               GENER026
           F12,,-*-1                                                    GENER027
           F35,,-*-1                                                    GENER028
               -IJ01,,-*-1        DIFFER                                GENER029
               -IJ02,,-*-1        DIVIDE                                GENER030
           EQ,,-*-1                                                     GENER031
           F8,,-*-1                                                     GENER032
           F21,,-*-1                                                    GENER033
           F19,,-*-1                                                    GENER034
           EVLISL,,-*-1                                                 GENER035
           EXPR,,-*-1                                                   GENER036
           F32,,-*-1                                                    GENER037
           FEXPR,,-*-1                                                  GENER038
           FIX,,-*-1                                                    GENER039
               -II11,,-*-1        FIX P                                 GENER040
           FLOAT,,-*-1                                                  GENER041
               -II12,,-*-1        FLOAT P                               GENER042
           FSUBR,,-*-1                                                  GENER043
           FUNARG,,-*-1                                                 GENER044
           FUNCT,,-*-1                                                  GENER045
           SYMGEN,,-*-1                                                 GENER046
           GO,,-*-1                                                     GENER047
               -II3,,-*-1         GREATER THAN P                        GENER048
           F16,,-*-1                                                    GENER049
           LABEL,,-*-1                                                  GENER050
           LAMBDA,,-*-1                                                 GENER051
               LAP,,-*-1                                                GENER052
               -II4,,-*-1         LESS THAN P                           GENER053
           LIST,,-*-1                                                   GENER054
               LOADA,,-*-1        LOADER OBJECT                         GENER055
           PMAPCA,,-*-1                                                 GENER056
            -ZZ069B,,-*-1                                               
            -ZZ069A,,-*-1                                               
               -II7,,-*-1         MAXIMUM                               GENER059
               -II8,,-*-1         MINIMUM                               GENER060
           MINUS,,-*-1                                                  GENER061
               -II16,,-*-1        MINUS P                               GENER062
           F3,,-*-1                                                     GENER063
           NIL,,-*-1                                                    GENER064
           NOT,,-*-1                                                    GENER065
           NULL,,-*-1                                                   GENER066
               -II13,,-*-1        NUMBER P                              GENER067
               OBLBA,,-*-1        OBLIST OBJECT                         GENER068
               -II9,,-*-1         ONE P                                 GENER069
           OR,,-*-1                                                     GENER070
           F2,,-*-1                                                     GENER071
           PAUSE,,-*-1                                                  GENER072
           PLB,,-*-1                                                    GENER073
           PLUS,,-*-1                                                   GENER074
           PNAME,,-*-1                                                  GENER075
           F4,,-*-1                                                     GENER076
           PROG,,-*-1                                                   GENER077
           PROPO,,-*-1                                                  GENER078
               -IJ05,,-*-1        PUNCH                                 GENER079
           QUOTE,,-*-1                                                  GENER080
               -IJ03,,-*-1        QUOTIENT                              GENER081
           F13,,-*-1                                                    GENER082
               -II18,,-*-1        RECIP                                 GENER083
           RCLAM,,-*-1                                                  GENER084
           PRPLCA,,-*-1                                                 GENER085
           PRPLCD,,-*-1                                                 GENER086
               -IJ04,,-*-1        REMAINDER                             GENER087
               RETATM,,-*-1       RETURN                                GENER088
           SASCO,,-*-1                                                  GENER089
           SRCH,,-*-1                                                   GENER090
           SET,,-*-1                                                    GENER091
           SETQ,,-*-1                                                   GENER092
           F34,,-*-1                                                    GENER093
           STOP,,-*-1                                                   GENER094
           SUBR,,-*-1                                                   GENER095
               TRACE,,-*-1                                              GENER096
               SMOVE,,-*-1                                              GENER097
               SRETUR,,-*-1                                             GENER098
               SLIST,,-*-1                                              GENER099
               SPECAL,,-*-1                                             GENER100
               -II15,,-*-1        SUBTRACT 1                            GENER101
           F17,,-*-1                                                    GENER102
           F30,,-*-1                                                    GENER103
               1,,-*-1            *T* BINARY TRUE ATOM                  GENER104
           F27,,-*-1                                                    GENER105
               SYM,,-*-1                                                GENER106
           TIMES,,-*-1                                                  GENER107
           F36,,-*-1                                                    GENER108
               -II10,,-*-1        ZERO P                                GENER109
           CGET,,-*-1                                                   GENER110
               REMPP,,-*-1                                              GENER111
           H00,,-*-1                                                    GENER112
           H01,,-*-1                                                    GENER113
           H02,,-*-1                                                    GENER114
           H03,,-*-1                                                    GENER115
           H04,,-*-1                                                    GENER116
           H05,,-*-1                                                    GENER117
           H06,,-*-1                                                    GENER118
           H07,,-*-1                                                    GENER119
           H10,,-*-1                                                    GENER120
           H11,,-*-1                                                    GENER121
           H12,,-*-1                                                    GENER122
           H13,,-*-1                                                    GENER123
           H15,,-*-1                                                    GENER124
           H14,,-*-1                                                    GENER125
           H16,,-*-1                                                    GENER126
           H17,,-*-1                                                    GENER127
           H20,,-*-1                                                    GENER128
           H21,,-*-1                                                    GENER129
           H22,,-*-1                                                    GENER130
           H23,,-*-1                                                    GENER131
           H24,,-*-1                                                    GENER132
           H25,,-*-1                                                    GENER133
           H26,,-*-1                                                    GENER134
           H27,,-*-1                                                    GENER135
           H30,,-*-1                                                    GENER136
           H31,,-*-1                                                    GENER137
           H32,,-*-1                                                    GENER138
           H33,,-*-1                                                    GENER139
           H34,,-*-1                                                    GENER140
           H35,,-*-1                                                    GENER141
           H36,,-*-1                                                    GENER142
           H37,,-*-1                                                    GENER143
           H40,,-*-1                                                    GENER144
           H41,,-*-1                                                    GENER145
           H42,,-*-1                                                    GENER146
           H43,,-*-1                                                    GENER147
           H44,,-*-1                                                    GENER148
           H45,,-*-1                                                    GENER149
           H46,,-*-1                                                    GENER150
           H47,,-*-1                                                    GENER151
           H50,,-*-1                                                    GENER152
           H51,,-*-1                                                    GENER153
           H52,,-*-1                                                    GENER154
           H53,,-*-1                                                    GENER155
           H54,,-*-1                                                    GENER156
           H55,,-*-1                                                    GENER157
           H56,,-*-1                                                    GENER158
           H57,,-*-1                                                    GENER159
           H60,,-*-1                                                    GENER160
           H61,,-*-1                                                    GENER161
           H62,,-*-1                                                    GENER162
           H63,,-*-1                                                    GENER163
           H64,,-*-1                                                    GENER164
           H65,,-*-1                                                    GENER165
           H66,,-*-1                                                    GENER166
           H67,,-*-1                                                    GENER167
           H70,,-*-1                                                    GENER168
           H71,,-*-1                                                    GENER169
           H72,,-*-1                                                    GENER170
           H73,,-*-1                                                    GENER171
           H74,,-*-1                                                    GENER172
           H75,,-*-1                                                    GENER173
           H76,,-*-1                                                    GENER174
           H77,,-*-1                                                    GENER175
           PJ1,,-*-1                                                    GENER176
           PJ2,,-*-1                                                    GENER177
           PJ4,,-*-1                                                    GENER179
           PJ5,,-*-1                                                    GENER180
           PJ6,,-*-1                                                    GENER181
           PJ7,,-*-1                                                    GENER182
           PJ8,,-*-1                                                    GENER183
           PJ9,,-*-1                                                    GENER184
           PJ10,,-*-1                                                   GENER185
           PJ11,,-*-1                                                   GENER186
           PJ12,,-*-1                                                   GENER187
           PJ14,,-*-1                                                   GENER189
           PJ15,,-*-1                                                   GENER190
           PJ16,,-*-1                                                   GENER191
           PJ17,,-*-1                                                   GENER192
           PJ18,,-*-1                                                   GENER193
           PJ19,,-*-1                                                   GENER194
           PJ21,,-*-1                                                   GENER195
           PJ23,,-*-1                                                   GENER196
               PJ24,,-*-1                                               GENER197
               PJ25,,-*-1                                               GENER198
               PJ26,,-*-1                                               GENER199
               PJ27,,-*-1                                               GENER200
               PJ28,,-*-1                                               GENER201
               PJ30,,-*-1                                               GENER202
               PJ31,,-*-1                                               GENER203
               PJ32,,-*-1                                               GENER204
               PJ33,,-*-1                                               GENER205
               PJ34,,-*-1                                               GENER206
               PJ35,,-*-1                                               GENER207
               PJ36,,-*-1                                               GENER208
               PJ37,,-*-1                                               GENER209
               PJ38,,-*-1                                               GENER210
               PJ39,,-*-1                                               GENER211
               ERSETO,,-*-1       ERRORSET                              GENER212
               PVW1,,-*-1         LAST OBJECT   - LEFTSHIFT             GENER213
       EJECT                                                            PAGE 184
       REM PROPERTY LISTS
       REM
II14           -1,,-*-1                                                 GPLI0000
               $SUBR,,-*-1                                              GPLI0001
               -*-1,,-*-2                                               GPLI0002
       TXL     ADD1,,1                                                  GPLI0003
               $PNAME,,-*-1                                             GPLI0004
               -*-1               ADD1                                  GPLI0005
               -*-1                                                     GPLI0006
       OCT     212424017777                                             GPLI0007
*                                                                       GPLI0008
)PJ2           -1,,-*-1                                                 GPLI0009
               SUBR,,-*-1                                               GPLI0010
               -*-1,,-*-2                                               GPLI0011
       TXL     ADVANC,,0                                                GPLI0012
               PNAME,,-*-1                                              GPLI0013
               -*-1               ADVANCE                               GPLI0014
               -*-1,,-*-2                                               GPLI0015
       OCT     212465214523                                             GPLI0016
               -*-1                                                     GPLI0017
       OCT     257777777777                                             GPLI0018
*                                                                       GPLI0019
)ALST          -1,,-*-1                                                 GPLI0020
               PNAME,,-*-1                                              GPLI0021
               -*-1,,-*-3         $ALIST                                GPLI0022
               -*-1                                                     GPLI0023
       OCT     532143316263                                             GPLI0024
               SYM,,-*-1                                                GPLI0025
               -C$ALST                                                  GPLI0026
*                                                                       GPLI0027
)002           -1,,-*-1                                                 GPLI0028
               FSUBR,,-*-1                                              GPLI0029
               -*-1,,-*-2                                               GPLI0030
       TXL     $EVAND,,0                                                GPLI0031
               $PNAME,,-*-1                                             GPLI0032
               -*-1               AND                                   GPLI0033
               -*-1                                                     GPLI0034
       OCT     214524777777                                             GPLI0035
*                                                                       GPLI0036
)003           -1,,-*-1                                                 GPLI0037
               SUBR,,-*-1                                               GPLI0038
               -*-1,,-*-2                                               GPLI0039
       TXL     APPEND,,2                                                GPLI0040
               PNAME,,-*-1                                              GPLI0041
               -*-1               APPEND                                GPLI0042
               -*-1                                                     GPLI0043
       OCT     214747254524                                             GPLI0044
*                                                                       GPLI0045
)004           -1,,-*-1                                                 GPLI0046
               SUBR,,-*-1                                               GPLI0047
               -*-1,,-*-2                                               GPLI0048
       TXL     APPLY,,3                                                 GPLI0049
               PNAME,,-*-1                                              GPLI0050
               -*-1               APPLY                                 GPLI0051
               -*-1                                                     GPLI0052
       OCT     214747437077                                             GPLI0053
*                                                                       GPLI0054
)005           -1,,-*-1                                                 GPLI0055
               PNAME,,-*-1                                              GPLI0056
               -*-1               APVAL                                 GPLI0057
               -*-1                                                     GPLI0058
       OCT     214765214377                                             GPLI0059
*                                                                       GPLI0060
II1            -1,,-*-1                                                 GPLI0061
               SUBR,,-*-1                                               GPLI0062
               -*-1,,-*-2                                               GPLI0063
       TXL     ARYMAK,,1                                                GPLI0064
               PNAME,,-*-1                                              GPLI0065
               -*-1               ARRAY                                 GPLI0066
               -*-1                                                     GPLI0067
       OCT     215151217077                                             GPLI0068
*                                                                       GPLI0069
)007           -1,,-*-1                                                 GPLI0070
               SUBR,,-*-1                                               GPLI0071
               -*-1,,-*-2                                               GPLI0072
       TXL     ATOMP,,1                                                 GPLI0073
               PNAME,,-*-1                                              GPLI0074
               -*-1               ATOM                                  GPLI0075
               -*-1                                                     GPLI0076
       OCT     216346447777                                             GPLI0077
*                                                                       GPLI0078
)008           -1,,-*-1                                                 GPLI0079
               SUBR,,-*-1                                               GPLI0080
               -*-1,,-*-2                                               GPLI0081
       TXL     ATTRIB,,2                                                GPLI0082
               PNAME,,-*-1                                              GPLI0083
               -*-1               ATTRIB                                GPLI0084
               -*-1                                                     GPLI0085
       OCT     216363513122                                             GPLI0086
*                                                                       GPLI0087
)PJ12          -1,,-*-1                                                 GPLI0088
               PNAME,,-*-1                                              GPLI0089
               -*-1,,-*-3         BLANK                                 GPLI0090
               -*-1                                                     GPLI0091
       OCT     224321454277                                             GPLI0092
               APVAL1,,-*-1                                             GPLI0093
               -*-1                                                     GPLI0094
               H60                                                      GPLI0095
*                                                                       GPLI0096
)011           -1,,-*-1                                                 GPLI0097
               SUBR,,-*-1                                               GPLI0098
               -*-1,,-*-2                                               GPLI0099
       TXL     CARP,,1                                                  GPLI0100
               PNAME,,-*-1                                              GPLI0101
               -*-1               CAR                                   GPLI0102
               -*-1                                                     GPLI0103
       OCT     232151777777                                             GPLI0104
*                                                                       GPLI0105
)012           -1,,-*-1                                                 GPLI0106
               SUBR,,-*-1                                               GPLI0107
               -*-1,,-*-2                                               GPLI0108
       TXL     CDRP,,1                                                  GPLI0109
               PNAME,,-*-1                                              GPLI0110
               -*-1               CDR                                   GPLI0111
               -*-1                                                     GPLI0112
       OCT     232451777777                                             GPLI0113
*                                                                       GPLI0114
)201           -1,,-*-1                                                 GPLI0115
               SUBR,,-*-1                                               GPLI0116
               -*-1,,-*-2                                               GPLI0117
       TXL     CAARXX,,1                                                GPLI0118
               PNAME,,-*-1                                              GPLI0119
               -*-1               CAAR                                  GPLI0120
               -*-1                                                     GPLI0121
       OCT     232121517777                                             GPLI0122
*                                                                       GPLI0123
)202           -1,,-*-1                                                 GPLI0124
               SUBR,,-*-1                                               GPLI0125
               -*-1,,-*-2                                               GPLI0126
       TXL     CDARXX,,1                                                GPLI0127
               PNAME,,-*-1                                              GPLI0128
               -*-1               CDAR                                  GPLI0129
               -*-1                                                     GPLI0130
       OCT     232421517777                                             GPLI0131
*                                                                       GPLI0132
)203           -1,,-*-1                                                 GPLI0133
               SUBR,,-*-1                                               GPLI0134
               -*-1,,-*-2                                               GPLI0135
       TXL     CADRXX,,1                                                GPLI0136
               PNAME,,-*-1                                              GPLI0137
               -*-1               CADR                                  GPLI0138
               -*-1                                                     GPLI0139
       OCT     232124517777                                             GPLI0140
*                                                                       GPLI0141
)204           -1,,-*-1                                                 GPLI0142
               SUBR,,-*-1                                               GPLI0143
               -*-1,,-*-2                                               GPLI0144
       TXL     CDDRXX,,1                                                GPLI0145
               PNAME,,-*-1                                              GPLI0146
               -*-1               CDDR                                  GPLI0147
               -*-1                                                     GPLI0148
       OCT     232424517777                                             GPLI0149
*                                                                       GPLI0150
)205           -1,,-*-1                                                 GPLI0151
               SUBR,,-*-1                                               GPLI0152
               -*-1,,-*-2                                               GPLI0153
       TXL     CAAARX,,1                                                GPLI0154
               PNAME,,-*-1                                              GPLI0155
               -*-1               CAAAR                                 GPLI0156
               -*-1                                                     GPLI0157
       OCT     232121215177                                             GPLI0158
*                                                                       GPLI0159
)206           -1,,-*-1                                                 GPLI0160
               SUBR,,-*-1                                               GPLI0161
               -*-1,,-*-2                                               GPLI0162
       TXL     CAADRX,,1                                                GPLI0163
               PNAME,,-*-1                                              GPLI0164
               -*-1               CAADR                                 GPLI0165
               -*-1                                                     GPLI0166
       OCT     232121245177                                             GPLI0167
*                                                                       GPLI0168
)207           -1,,-*-1                                                 GPLI0169
               SUBR,,-*-1                                               GPLI0170
               -*-1,,-*-2                                               GPLI0171
       TXL     CADARX,,1                                                GPLI0172
               PNAME,,-*-1                                              GPLI0173
               -*-1               CADAR                                 GPLI0174
               -*-1                                                     GPLI0175
       OCT     232124215177                                             GPLI0176
*                                                                       GPLI0177
)208           -1,,-*-1                                                 GPLI0178
               SUBR,,-*-1                                               GPLI0179
               -*-1,,-*-2                                               GPLI0180
       TXL     CADDRX,,1                                                GPLI0181
               PNAME,,-*-1                                              GPLI0182
               -*-1               CADDR                                 GPLI0183
               -*-1                                                     GPLI0184
       OCT     232124245177                                             GPLI0185
*                                                                       GPLI0186
)209           -1,,-*-1                                                 GPLI0187
               SUBR,,-*-1                                               GPLI0188
               -*-1,,-*-2                                               GPLI0189
       TXL     CDAARX,,1                                                GPLI0190
               PNAME,,-*-1                                              GPLI0191
               -*-1               CDAAR                                 GPLI0192
               -*-1                                                     GPLI0193
       OCT     232421215177                                             GPLI0194
*                                                                       GPLI0195
)210           -1,,-*-1                                                 GPLI0196
               SUBR,,-*-1                                               GPLI0197
               -*-1,,-*-2                                               GPLI0198
       TXL     CDADRX,,1                                                GPLI0199
               PNAME,,-*-1                                              GPLI0200
               -*-1               CDADR                                 GPLI0201
               -*-1                                                     GPLI0202
       OCT     232421245177                                             GPLI0203
*                                                                       GPLI0204
)211           -1,,-*-1                                                 GPLI0205
               SUBR,,-*-1                                               GPLI0206
               -*-1,,-*-2                                               GPLI0207
       TXL     CDDARX,,1                                                GPLI0208
               PNAME,,-*-1                                              GPLI0209
               -*-1               CDDAR                                 GPLI0210
               -*-1                                                     GPLI0211
       OCT     232424215177                                             GPLI0212
*                                                                       GPLI0213
)212           -1,,-*-1                                                 GPLI0214
               SUBR,,-*-1                                               GPLI0215
               -*-1,,-*-2                                               GPLI0216
       TXL     CDDDRX,,1                                                GPLI0217
               PNAME,,-*-1                                              GPLI0218
               -*-1               CDDDR                                 GPLI0219
               -*-1                                                     GPLI0220
       OCT     232424245177                                             GPLI0221
*                                                                       GPLI0222
)PJ32          -1,,-*-1                                                 GPLI0223
               PNAME,,-*-1                                              GPLI0224
               -*-1,,-*-5         CHARCOUNT                             GPLI0225
               -*-1,,-*-2                                               GPLI0226
       OCT     233021512346                                             GPLI0227
               -*-1                                                     GPLI0228
       OCT     644563777777                                             GPLI0229
               APVAL1,,-*-1                                             GPLI0230
               -*-1                                                     GPLI0231
               -*-1                                                     GPLI0232
       MZE     -1,1,-CHACT                                              GPLI0233
*                                                                       GPLI0234
)PJ27          -1,,-*-1                                                 GPLI0235
               SUBR,,-*-1                                               GPLI0236
               -*-1,,-*-2                                               GPLI0237
       TXL     CLEAR,,0                                                 GPLI0238
               PNAME,,-*-1                                              GPLI0239
               -*-1               CLEARBUFF                             GPLI0240
               -*-1,,-*-2                                               GPLI0241
       OCT     234325215122                                             GPLI0242
               -*-1                                                     GPLI0243
       OCT     642626777777                                             GPLI0244
*                                                                       GPLI0245
)PJ6           -1,,-*-1                                                 GPLI0246
               PNAME,,-*-1                                              GPLI0247
               -*-1,,-*-3         COMMA                                 GPLI0248
               -*-1                                                     GPLI0249
       OCT     234644442177                                             GPLI0250
               APVAL1,,-*-1                                             GPLI0251
               -*-1                                                     GPLI0252
               H73                                                      GPLI0253
*                                                                       GPLI0254
)016           -1,,-*-1                                                 GPLI0255
               FSUBR,,-*-1                                              GPLI0256
               -*-1,,-*-2                                               GPLI0257
       TXL     $EVCON,,0                                                GPLI0258
               PNAME,,-*-1                                              GPLI0259
               -*-1               COND                                  GPLI0260
               -*-1                                                     GPLI0261
       OCT     234645247777                                             GPLI0262
*                                                                       GPLI0263
)017           -1,,-*-1                                                 GPLI0264
               SUBR,,-*-1                                               GPLI0265
               -*-1,,-*-2                                               GPLI0266
       TXL     CONS,,2                                                  GPLI0267
               PNAME,,-*-1                                              GPLI0268
               -*-1               CONS                                  GPLI0269
               -*-1                                                     GPLI0270
       OCT     234645627777                                             GPLI0271
*                                                                       GPLI0272
)019           -1,,-*-1                                                 GPLI0273
               SUBR,,-*-1                                               GPLI0274
               -*-1,,-*-2                                               GPLI0275
       TXL     CP1,,1                                                   GPLI0276
               PNAME,,-*-1                                              GPLI0277
               -*-1               CP1                                   GPLI0278
               -*-1                                                     GPLI0279
       OCT     234701777777                                             GPLI0280
*                                                                       GPLI0281
)020           -1,,-*-1                                                 GPLI0282
               SUBR,,-*-1                                               GPLI0283
               -*-1,,-*-2                                               GPLI0284
       TXL     $COPY,,1                                                 GPLI0285
               PNAME,,-*-1                                              GPLI0286
               -*-1               COPY                                  GPLI0287
               -*-1                                                     GPLI0288
       OCT     234647707777                                             GPLI0289
*                                                                       GPLI0290
)021           -1,,-*-1                                                 GPLI0291
               SUBR,,-*-1                                               GPLI0292
               -*-1,,-*-2                                               GPLI0293
       TXL     COUNT,,0                                                 GPLI0294
               PNAME,,-*-1                                              GPLI0295
               -*-1               COUNT                                 GPLI0296
               -*-1                                                     GPLI0297
       OCT     234664456377                                             GPLI0298
*                                                                       GPLI0299
)PJ1           -1,,-*-1                                                 GPLI0300
               APVAL1,,-*-1                                             GPLI0301
               -CURC1,,-*-1                                             GPLI0302
               PNAME,,-*-1                                              GPLI0303
               -*-1,,-*-5         CURCHAR                               GPLI0304
               -*-1,,-*-2                                               GPLI0305
       OCT     236451233021                                             GPLI0306
               -*-1                                                     GPLI0307
       OCT     517777777777                                             GPLI0308
               SPECAL,,-*-1                                             GPLI0309
               -CURC                                                    GPLI0310
*                                                                       GPLI0311
)PJ16          -1,,-*-1                                                 GPLI0312
               APVAL1,,-*-1                                             GPLI0313
               -*-1,,-*-2                                               GPLI0314
               H40                                                      GPLI0315
               PNAME,,-*-1                                              GPLI0316
               -*-1               DASH                                  GPLI0317
               -*-1                                                     GPLI0318
       OCT     242162307777                                             GPLI0319
*                                                                       GPLI0320
IJ01           -1,,-*-1                                                 GPLI0321
               $SUBR,,-*-1                                              GPLI0322
               -*-1,,-*-2                                               GPLI0323
       TXL     DIFFER,,2                                                GPLI0324
               $PNAME,,-*-1                                             GPLI0325
               -*-1               DIFFERENCE                            GPLI0326
               -*-1,,-*-2                                               GPLI0327
       OCT     243126262551                                             GPLI0328
               -*-1                                                     GPLI0329
       OCT     254523257777                                             GPLI0330
*                                                                       GPLI0331
)PJ19          -1,,-*-1                                                 GPLI0332
               SUBR,,-*-1                                               GPLI0333
               -*-1,,-*-2                                               GPLI0334
       TXL     DIGIT,,1                                                 GPLI0335
               PNAME,,-*-1                                              GPLI0336
               -*-1               DIGIT                                 GPLI0337
               -*-1                                                     GPLI0338
       OCT     243127316377                                             GPLI0339
*                                                                       GPLI0340
IJ02           -1,,-*-1                                                 GPLI0341
               $SUBR,,-*-1                                              GPLI0342
               -*-1,,-*-2                                               GPLI0343
       TXL     DIVIDE,,2                                                GPLI0344
               $PNAME,,-*-1                                             GPLI0345
               -*-1               DIVIDE                                GPLI0346
               -*-1                                                     GPLI0347
       OCT     243165312425                                             GPLI0348
*                                                                       GPLI0349
)PJ10          -1,,-*-1                                                 GPLI0350
               PNAME,,-*-1                                              GPLI0351
               -*-1,,-*-3         DOLLAR                                GPLI0352
               -*-1                                                     GPLI0353
       OCT     244643432151                                             GPLI0354
               APVAL1,,-*-1                                             GPLI0355
               -*-1                                                     GPLI0356
               H53                                                      GPLI0357
*                                                                       GPLI0358
DMPCB          -1,,-*-1                                                 GPLI0359
               SUBR,,-*-1                                               GPLI0360
               -*-1,,-*-2                                               GPLI0361
       TXL     DUMPXX,,4                                                GPLI0362
               PNAME,,-*-1                                              GPLI0363
               -*-1               DUMP                                  GPLI0364
               -*-1                                                     GPLI0365
       OCT     246444477777                                             GPLI0366
*                                                                       GPLI0367
)PJ30          -1,,-*-1                                                 GPLI0368
               SUBR,,-*-1                                               GPLI0369
               -*-1,,-*-2                                               GPLI0370
       TXL     ENDRED,,0                                                GPLI0371
               PNAME,,-*-1                                              GPLI0372
               -*-1               ENDREAD                               GPLI0373
               -*-1,,-*-2                                               GPLI0374
       OCT     254524512521                                             GPLI0375
               -*-1                                                     GPLI0376
       OCT     247777777777                                             GPLI0377
*                                                                       GPLI0378
)PJ34          -1,,-*-1                                                 GPLI0379
               APVAL1,,-*-1                                             GPLI0380
               -*-1,,-*-2                                               GPLI0381
               H12                                                      GPLI0382
               PNAME,,-*-1                                              GPLI0383
               -*-1               EOF                                   GPLI0384
               -*-1                                                     GPLI0385
       OCT     254626777777                                             GPLI0386
*                                                                       GPLI0387
)PJ35          -1,,-*-1                                                 GPLI0388
               APVAL1,,-*-1                                             GPLI0389
               -*-1,,-*-2                                               GPLI0390
               H72                                                      GPLI0391
               PNAME,,-*-1                                              GPLI0392
               -*-1               EOR                                   GPLI0393
               -*-1                                                     GPLI0394
       OCT     254651777777                                             GPLI0395
*                                                                       GPLI0396
)030           -1,,-*-1                                                 GPLI0397
               SUBR,,-*-1                                               GPLI0398
               -*-1,,-*-2                                               GPLI0399
       TXL     EQ,,2                                                    GPLI0400
               PNAME,,-*-1                                              GPLI0401
               -*-1               EQ                                    GPLI0402
               -*-1                                                     GPLI0403
       OCT     255077777777                                             GPLI0404
*                                                                       GPLI0405
)PJ5           -1,,-*-1                                                 GPLI0406
               PNAME,,-*-1                                              GPLI0407
               -*-1,,-*-3         EQSIGN                                GPLI0408
               -*-1                                                     GPLI0409
       OCT     255062312745                                             GPLI0410
               APVAL1,,-*-1                                             GPLI0411
               -*-1                                                     GPLI0412
               H13                                                      GPLI0413
*                                                                       GPLI0414
)032           -1,,-*-1                                                 GPLI0415
               SUBR,,-*-1                                               GPLI0416
               -*-1,,-*-2                                               GPLI0417
       TXL     EQUAL,,2                                                 GPLI0418
               PNAME,,-*-1                                              GPLI0419
               -*-1               EQUAL                                 GPLI0420
               -*-1                                                     GPLI0421
       OCT     255064214377                                             GPLI0422
*                                                                       GPLI0423
)034           -1,,-*-1                                                 GPLI0424
               SUBR,,-*-1                                               GPLI0425
               -*-1,,-*-2                                               GPLI0426
       TXL     ERROR1,,1                                                GPLI0427
               PNAME,,-*-1                                              GPLI0428
               -*-1               ERROR                                 GPLI0429
               -*-1                                                     GPLI0430
       OCT     255151465177                                             GPLI0431
*                                                                       GPLI0432
)PJ4           -1,,-*-1                                                 GPLI0433
               SUBR,,-*-1                                               GPLI0434
               -*-1,,-*-2                                               GPLI0435
       TXL     EROR1,,0                                                 GPLI0436
               PNAME,,-*-1                                              GPLI0437
               -*-1               ERROR1                                GPLI0438
               -*-1                                                     GPLI0439
       OCT     255151465101                                             GPLI0440
*                                                                       GPLI0441
)PJ41          -1,,-*-1                                                 GPLI0442
               SUBR,,-*-1                                               GPLI0443
               -*-1,,-*-2                                               GPLI0444
       TXL     ERRSET,,3                                                GPLI0445
               PNAME,,-*-1                                              GPLI0446
               -*-1               ERRORSET                              GPLI0447
               -*-1,,-*-2                                               GPLI0448
       OCT     255151465162                                             GPLI0449
               -*-1                                                     GPLI0450
       OCT     256377777777                                             GPLI0451
*                                                                       GPLI0452
)035           -1,,-*-1                                                 GPLI0453
               SUBR,,-*-1                                               GPLI0454
               -*-1,,-*-2                                               GPLI0455
       TXL     EVAL,,2                                                  GPLI0456
               PNAME,,-*-1                                              GPLI0457
               -*-1               EVAL                                  GPLI0458
               -*-1                                                     GPLI0459
       OCT     256521437777                                             GPLI0460
*                                                                       GPLI0461
)036           -1,,-*-1                                                 GPLI0462
               $SUBR,,-*-1                                              GPLI0463
               -*-1,,-*-2                                               GPLI0464
       TXL     EVLIS,,2                                                 GPLI0465
               $PNAME,,-*-1                                             GPLI0466
               -*-1               EVLIS                                 GPLI0467
               -*-1                                                     GPLI0468
       OCT     256543316277                                             GPLI0469
*                                                                       GPLI0470
)037           -1,,-*-1                                                 GPLI0471
               PNAME,,-*-1                                              GPLI0472
               -*-1               EXPR                                  GPLI0473
               -*-1                                                     GPLI0474
       OCT     256747517777                                             GPLI0475
*                                                                       GPLI0476
)038           -1,,-*-1                                                 GPLI0477
               SUBR,,-*-1                                               GPLI0478
               -*-1,,-*-2                                               GPLI0479
       TXL     EXPT,,2                                                  GPLI0480
               PNAME,,-*-1                                              GPLI0481
               -*-1               EXPT                                  GPLI0482
               -*-1                                                     GPLI0483
       OCT     256747637777                                             GPLI0484
*                                                                       GPLI0485
)040           -1,,-*-1                                                 GPLI0486
               PNAME,,-*-1                                              GPLI0487
               -*-1               FEXPR                                 GPLI0488
               -*-1                                                     GPLI0489
       OCT     262567475177                                             GPLI0490
*                                                                       GPLI0491
)041           -1,,-*-1                                                 GPLI0492
               PNAME,,-*-1                                              GPLI0493
               -*-1               FIX                                   GPLI0494
               -*-1                                                     GPLI0495
       OCT     263167777777                                             GPLI0496
*                                                                       GPLI0497
II11           -1,,-*-1                                                 GPLI0498
               $SUBR,,-*-1                                              GPLI0499
               -*-1,,-*-2                                               GPLI0500
       TXL     FIXP,,1                                                  GPLI0501
               $PNAME,,-*-1                                             GPLI0502
               -*-1               FIXP                                  GPLI0503
               -*-1                                                     GPLI0504
       OCT     263167477777                                             GPLI0505
*                                                                       GPLI0506
)042           -1,,-*-1                                                 GPLI0507
               PNAME,,-*-1                                              GPLI0508
               -*-1               FLOAT                                 GPLI0509
               -*-1                                                     GPLI0510
       OCT     264346216377                                             GPLI0511
*                                                                       GPLI0512
II12           -1,,-*-1                                                 GPLI0513
               $SUBR,,-*-1                                              GPLI0514
               -*-1,,-*-2                                               GPLI0515
       TXL     FLOATP,,1                                                GPLI0516
               $PNAME,,-*-1                                             GPLI0517
               -*-1               FLOATP                                GPLI0518
               -*-1                                                     GPLI0519
       OCT     264346216347                                             GPLI0520
*                                                                       GPLI0521
)043           -1,,-*-1                                                 GPLI0522
               PNAME,,-*-1                                              GPLI0523
               -*-1               FSUBR                                 GPLI0524
               -*-1                                                     GPLI0525
       OCT     266264225177                                             GPLI0526
*                                                                       GPLI0527
)044           -1,,-*-1                                                 GPLI0528
               PNAME,,-*-1                                              GPLI0529
               -*-1               FUNARG                                GPLI0530
               -*-1                                                     GPLI0531
       OCT     266445215127                                             GPLI0532
*                                                                       GPLI0533
)045           -1,,-*-1                                                 GPLI0534
               FSUBR,,-*-1                                              GPLI0535
               -*-1,,-*-2                                               GPLI0536
       TXL     $LAMP,,0                                                 GPLI0537
               PNAME,,-*-1                                              GPLI0538
               -*-1               FUNCTION                              GPLI0539
               -*-1,,-*-2                                               GPLI0540
       OCT     266445236331                                             GPLI0541
               -*-1                                                     GPLI0542
       OCT     464577777777                                             GPLI0543
*                                                                       GPLI0544
)046           -1,,-*-1                                                 GPLI0545
               $SUBR,,-*-1                                              GPLI0546
               -*-1,,-*-2                                               GPLI0547
       TXL     GENSYM,,0                                                GPLI0548
               $PNAME,,-*-1                                             GPLI0549
               -*-1               GENSYM                                GPLI0550
               -*-1                                                     GPLI0551
       OCT     272545627044                                             GPLI0552
*                                                                       GPLI0553
)231           -1,,-*-1                                                 GPLI0554
               SUBR,,-*-1                                               GPLI0555
               -*-1,,-*-2                                               GPLI0556
       TXL     C$GET,,2                                                 GPLI0557
               PNAME,,-*-1                                              GPLI0558
               -*-1               GET                                   GPLI0559
               -*-1                                                     GPLI0560
       OCT     272563777777                                             GPLI0561
*                                                                       GPLI0562
)047           -1,,-*-1                                                 GPLI0563
               $FSUBR,,-*-1                                             GPLI0564
               -*-1,,-*-2                                               GPLI0565
       TXL     GOGOGO,,1                                                GPLI0566
               PNAME,,-*-1                                              GPLI0567
               -*-1               GO                                    GPLI0568
               -*-1                                                     GPLI0569
       OCT     274677777777                                             GPLI0570
*                                                                       GPLI0571
II3            -1,,-*-1                                                 GPLI0572
               $SUBR,,-*-1                                              GPLI0573
               -*-1,,-*-2                                               GPLI0574
       TXL     GRTRTP,,2                                                GPLI0575
               $PNAME,,-*-1                                             GPLI0576
               -*-1               GREATERP                              GPLI0577
               -*-1,,-*-2                                               GPLI0578
       OCT     275125216325                                             GPLI0579
               -*-1                                                     GPLI0580
       OCT     514777777777                                             GPLI0581
*                                                                       GPLI0582
)052           -1,,-*-1                                                 GPLI0583
               SUBR,,-*-1                                               GPLI0584
               -*-1,,-*-2                                               GPLI0585
       TXL     INTRN1,,1                                                GPLI0586
               PNAME,,-*-1                                              GPLI0587
               -*-1               INTERN                                GPLI0588
               -*-1                                                     GPLI0589
       OCT     314563255145                                             GPLI0590
*                                                                       GPLI0591
)054           -1,,-*-1                                                 GPLI0592
               FSUBR,,-*-1                                              GPLI0593
               -*-1,,-*-2                                               GPLI0594
       TXL     LABP,,0                                                  GPLI0595
               PNAME,,-*-1                                              GPLI0596
               -*-1               LABEL                                 GPLI0597
               -*-1                                                     GPLI0598
       OCT     432122254377                                             GPLI0599
*                                                                       GPLI0600
)055           -1,,-*-1                                                 GPLI0601
               PNAME,,-*-1                                              GPLI0602
               -*-1               LABEL                                 GPLI0603
               -*-1                                                     GPLI0604
       OCT     432122254377                                             GPLI0605
*                                                                       GPLI0606
)LAP           -1,,-*-1                                                 GPLI0607
               SUBR,,-*-1                                               GPLI0608
               -*-1,,-*-2                                               GPLI0609
       TXL     C$LAP,,2                                                 GPLI0610
               PNAME,,-*-1                                              GPLI0611
               -*-1               LAP                                   GPLI0612
               -*-1                                                     GPLI0613
       OCT     432147777777                                             GPLI0614
*                                                                       GPLI0615
PVV1           -1,,-*-1                                                 GPLI0616
               SUBR,,-*-1                                               GPLI0617
               -*-1,,-*-2                                               GPLI0618
       TXL     LSHIFT,,2                                                GPLI0619
               PNAME,,-*-1                                              GPLI0620
               -*-1               LEFTSHIFT                             GPLI0621
               -*-1,,-*-2                                               GPLI0622
       OCT     432526636230                                             GPLI0623
               -*-1                                                     GPLI0624
       OCT     312663777777                                             GPLI0625
*                                                                       GPLI0626
II4            -1,,-*-1                                                 GPLI0627
               $SUBR,,-*-1                                              GPLI0628
               -*-1,,-*-2                                               GPLI0629
       TXL     LESSTP,,2                                                GPLI0630
               $PNAME,,-*-1                                             GPLI0631
               -*-1               LESSP                                 GPLI0632
               -*-1                                                     GPLI0633
       OCT     432562624777                                             GPLI0634
*                                                                       GPLI0635
)057           -1,,-*-1                                                 GPLI0636
               FSUBR,,-*-1                                              GPLI0637
               -*-1,,-*-2                                               GPLI0638
       TXL     EVLIS,,0                                                 GPLI0639
               PNAME,,-*-1                                              GPLI0640
               -*-1               LIST                                  GPLI0641
               -*-1                                                     GPLI0642
       OCT     433162637777                                             GPLI0643
*                                                                       GPLI0644
)PJ17          -1,,-*-1                                                 GPLI0645
               SUBR,,-*-1                                               GPLI0646
               -*-1,,-*-2                                               GPLI0647
       TXL     LITER,,1                                                 GPLI0648
               PNAME,,-*-1                                              GPLI0649
               -*-1               LITER                                 GPLI0650
               -*-1                                                     GPLI0651
       OCT     433163255177                                             GPLI0652
*                                                                       GPLI0653
)234A          -1,,-*-1                                                 GPLI0654
               SUBR,,-*-1                                               GPLI0655
               -*-1,,-*-2                                               GPLI0656
       TXL     LOADER,,0                                                GPLI0657
               PNAME,,-*-1                                              GPLI0658
               -*-1               LOAD                                  GPLI0659
               -*-1                                                     GPLI0660
       OCT     434621247777                                             GPLI0661
*                                                                       GPLI0662
)PJ37          -1,,-*-1                                                 GPLI0663
               FSUBR,,-*-1                                              GPLI0664
               -*-1,,-*-2                                               GPLI0665
       TXL     LOGAND,,0                                                GPLI0666
               PNAME,,-*-1                                              GPLI0667
               -*-1               LOGAND                                GPLI0668
               -*-1                                                     GPLI0669
       OCT     434627214524                                             GPLI0670
*                                                                       GPLI0671
)PJ36          -1,,-*-1                                                 GPLI0672
               FSUBR,,-*-1                                              GPLI0673
               -*-1,,-*-2                                               GPLI0674
       TXL     LOGOR,,0                                                 GPLI0675
               PNAME,,-*-1                                              GPLI0676
               -*-1               LOGOR                                 GPLI0677
               -*-1                                                     GPLI0678
       OCT     434627465177                                             GPLI0679
*                                                                       GPLI0680
)PJ38          -1,,-*-1                                                 GPLI0681
               FSUBR,,-*-1                                              GPLI0682
               -*-1,,-*-2                                               GPLI0683
       TXL     LOGXOR,,0                                                GPLI0684
               PNAME,,-*-1                                              GPLI0685
               -*-1               LOGXOR                                GPLI0686
               -*-1                                                     GPLI0687
       OCT     434627674651                                             GPLI0688
*                                                                       GPLI0689
)PJ7           -1,,-*-1                                                 GPLI0690
               PNAME,,-*-1                                              GPLI0691
               -*-1,,-*-3         LPAR                                  GPLI0692
               -*-1                                                     GPLI0693
       OCT     434721517777                                             GPLI0694
               APVAL1,,-*-1                                             GPLI0695
               -*-1                                                     GPLI0696
               H74                                                      GPLI0697
*                                                                       GPLI0698
)065           -1,,-*-1                                                 GPLI0699
               SUBR,,-*-1                                               GPLI0700
               -*-1,,-*-2                                               GPLI0701
       TXL     MAPCAR,,2                                                GPLI0702
               PNAME,,-*-1                                              GPLI0703
               -*-1               MAP                                   GPLI0704
               -*-1                                                     GPLI0705
       OCT     442147777777                                             GPLI0706
*                                                                       GPLI0707
)069B          -1,,-*-1                                                 GPLI0708
               SUBR,,-*-1                                               GPLI0709
               -*-1,,-*-2                                               GPLI0710
       TXL     MAPCON,,2                                                GPLI0711
               PNAME,,-*-1                                              GPLI0712
               -*-1               MAPCON                                GPLI0713
               -*-1                                                     GPLI0714
       OCT     442147234645                                             GPLI0715
*                                                                       GPLI0716
)069A          -1,,-*-1                                                 GPLI0717
               SUBR,,-*-1                                               GPLI0718
               -*-1,,-*-2                                               GPLI0719
       TXL     MAPLIS,,2                                                GPLI0720
               PNAME,,-*-1                                              GPLI0721
               -*-1               MAPLIST                               GPLI0722
               -*-1,,-*-2                                               GPLI0723
       OCT     442147433162                                             GPLI0724
               -*-1                                                     GPLI0725
       OCT     637777777777                                             GPLI0726
*                                                                       GPLI0727
II7            -1,,-*-1                                                 GPLI0728
               $FSUBR,,-*-1                                             GPLI0729
               -*-1,,-*-2                                               GPLI0730
       TXL     MAX,,2                                                   GPLI0731
               $PNAME,,-*-1                                             GPLI0732
               -*-1               MAX                                   GPLI0733
               -*-1                                                     GPLI0734
       OCT     442167777777                                             GPLI0735
*                                                                       GPLI0736
II8            -1,,-*-1                                                 GPLI0737
               $FSUBR,,-*-1                                             GPLI0738
               -*-1,,-*-2                                               GPLI0739
       TXL     MIN,,2                                                   GPLI0740
               $PNAME,,-*-1                                             GPLI0741
               -*-1               MIN                                   GPLI0742
               -*-1                                                     GPLI0743
       OCT     443145777777                                             GPLI0744
*                                                                       GPLI0745
)070           -1,,-*-1                                                 GPLI0746
               $SUBR,,-*-1                                              GPLI0747
               -*-1,,-*-2                                               GPLI0748
       TXL     MNSPRG,,1                                                GPLI0749
               $PNAME,,-*-1                                             GPLI0750
               -*-1               MINUS                                 GPLI0751
               -*-1                                                     GPLI0752
       OCT     443145646277                                             GPLI0753
*                                                                       GPLI0754
II16           -1,,-*-1                                                 GPLI0755
               $SUBR,,-*-1                                              GPLI0756
               -*-1,,-*-2                                               GPLI0757
       TXL     MINUSP,,1                                                GPLI0758
               $PNAME,,-*-1                                             GPLI0759
               -*-1               MINUSP                                GPLI0760
               -*-1                                                     GPLI0761
       OCT     443145646247                                             GPLI0762
*                                                                       GPLI0763
)PJ26          -1,,-*-1                                                 GPLI0764
               SUBR,,-*-1                                               GPLI0765
               -*-1,,-*-2                                               GPLI0766
       TXL     MKNAM,,0                                                 GPLI0767
               PNAME,,-*-1                                              GPLI0768
               -*-1               MKNAM                                 GPLI0769
               -*-1                                                     GPLI0770
       OCT     444245214477                                             GPLI0771
*                                                                       GPLI0772
)071           -1,,-*-1                                                 GPLI0773
               SUBR,,-*-1                                               GPLI0774
               -*-1,,-*-2                                               GPLI0775
       TXL     NCONC,,2                                                 GPLI0776
               PNAME,,-*-1                                              GPLI0777
               -*-1               NCONC                                 GPLI0778
               -*-1                                                     GPLI0779
       OCT     452346452377                                             GPLI0780
*                                                                       GPLI0781
)074           -1,,-*-1                                                 GPLI0782
               $SUBR,,-*-1                                              GPLI0783
               -*-1,,-*-2                                               GPLI0784
       TXL     NOTS,,1                                                  GPLI0785
               $PNAME,,-*-1                                             GPLI0786
               -*-1               NOT                                   GPLI0787
               -*-1                                                     GPLI0788
       OCT     454663777777                                             GPLI0789
*                                                                       GPLI0790
)075           -1,,-*-1                                                 GPLI0791
               SUBR,,-*-1                                               GPLI0792
               -*-1,,-*-2                                               GPLI0793
       TXL     NULLP,,1                                                 GPLI0794
               PNAME,,-*-1                                              GPLI0795
               -*-1               NULL                                  GPLI0796
               -*-1                                                     GPLI0797
       OCT     456443437777                                             GPLI0798
*                                                                       GPLI0799
II13           -1,,-*-1                                                 GPLI0800
               $SUBR,,-*-1                                              GPLI0801
               -*-1,,-*-2                                               GPLI0802
       TXL     NUMBRP,,1                                                GPLI0803
               $PNAME,,-*-1                                             GPLI0804
               -*-1               NUMBERP                               GPLI0805
               -*-1,,-*-2                                               GPLI0806
       OCT     456444222551                                             GPLI0807
               -*-1                                                     GPLI0808
       OCT     477777777777                                             GPLI0809
*                                                                       GPLI0810
)PJ25          -1,,-*-1                                                 GPLI0811
               SUBR,,-*-1                                               GPLI0812
               -*-1,,-*-2                                               GPLI0813
       TXL     NUMOB,,0                                                 GPLI0814
               PNAME,,-*-1                                              GPLI0815
               -*-1               NUMOB                                 GPLI0816
               -*-1                                                     GPLI0817
       OCT     456444462277                                             GPLI0818
*                                                                       GPLI0819
)079A          -1,,-*-1                                                 GPLI0820
               APVAL1,,-*-1                                             GPLI0821
               -*-1,,-*-2                                               GPLI0822
               -OBLIST                                                  GPLI0823
               PNAME,,-*-1                                              GPLI0824
               -*-1               OBLIST                                GPLI0825
               -*-1                                                     GPLI0826
       OCT     462243316263                                             GPLI0827
*                                                                       GPLI0828
)PJ28          -1,,-*-1                                                 GPLI0829
               PNAME,,-*-1                                              GPLI0830
               -*-1               OCTAL                                 GPLI0831
               -*-1                                                     GPLI0832
       OCT     462363214377                                             GPLI0833
*                                                                       GPLI0834
II9            -1,,-*-1                                                 GPLI0835
               $SUBR,,-*-1                                              GPLI0836
               -*-1,,-*-2                                               GPLI0837
       TXL     ONEP,,1                                                  GPLI0838
               $PNAME,,-*-1                                             GPLI0839
               -*-1               ONEP                                  GPLI0840
               -*-1                                                     GPLI0841
       OCT     464525477777                                             GPLI0842
*                                                                       GPLI0843
)PJ18          -1,,-*-1                                                 GPLI0844
               SUBR,,-*-1                                               GPLI0845
               -*-1,,-*-2                                               GPLI0846
       TXL     OPCHAR,,1                                                GPLI0847
               PNAME,,-*-1                                              GPLI0848
               -*-1               OPCHAR                                GPLI0849
               -*-1                                                     GPLI0850
       OCT     464723302151                                             GPLI0851
*                                                                       GPLI0852
)079           -1,,-*-1                                                 GPLI0853
               FSUBR,,-*-1                                              GPLI0854
               -*-1,,-*-2                                               GPLI0855
       TXL     $EVOR,,0                                                 GPLI0856
               $PNAME,,-*-1                                             GPLI0857
               -*-1               OR                                    GPLI0858
               -*-1                                                     GPLI0859
       OCT     465177777777                                             GPLI0860
*                                                                       GPLI0861
)PJ24          -1,,-*-1                                                 GPLI0862
               SUBR,,-*-1                                               GPLI0863
               -*-1,,-*-2                                               GPLI0864
       TXL     PACK,,1                                                  GPLI0865
               PNAME,,-*-1                                              GPLI0866
               -*-1               PACK                                  GPLI0867
               -*-1                                                     GPLI0868
       OCT     472123427777                                             GPLI0869
*                                                                       GPLI0870
)080           -1,,-*-1                                                 GPLI0871
               SUBR,,-*-1                                               GPLI0872
               -*-1,,-*-2                                               GPLI0873
       TXL     PAIR,,2                                                  GPLI0874
               PNAME,,-*-1                                              GPLI0875
               -*-1               PAIR                                  GPLI0876
               -*-1                                                     GPLI0877
       OCT     472131517777                                             GPLI0878
*                                                                       GPLI0879
)234C          -1,,-*-1                                                 GPLI0880
               SUBR,,-*-1                                               GPLI0881
               -*-1,,-*-2                                               GPLI0882
       TXL     PAUSEF,,0                                                GPLI0883
               PNAME,,-*-1                                              GPLI0884
               -*-1               PAUSE                                 GPLI0885
               -*-1                                                     GPLI0886
       OCT     472164622577                                             GPLI0887
*                                                                       GPLI0888
)PJ9           -1,,-*-1                                                 GPLI0889
               PNAME,,-*-1                                              GPLI0890
               -*-1,,-*-3         PERIOD                                GPLI0891
               -*-1                                                     GPLI0892
       OCT     472551314624                                             GPLI0893
               APVAL1,,-*-1                                             GPLI0894
               -*-1                                                     GPLI0895
               H33                                                      GPLI0896
*                                                                       GPLI0897
)234B          -1,,-*-1                                                 GPLI0898
               SUBR,,-*-1                                               GPLI0899
               -*-1,,-*-2                                               GPLI0900
       TXL     PSHLDB,,0                                                GPLI0901
               PNAME,,-*-1                                              GPLI0902
               -*-1               PLB                                   GPLI0903
               -*-1                                                     GPLI0904
       OCT     474322777777                                             GPLI0905
*                                                                       GPLI0906
)081           -1,,-*-1                                                 GPLI0907
               $FSUBR,,-*-1                                             GPLI0908
               -*-1,,-*-2                                               GPLI0909
       TXL     ADDP,,2                                                  GPLI0910
               $PNAME,,-*-1                                             GPLI0911
               -*-1               PLUS                                  GPLI0912
               -*-1                                                     GPLI0913
       OCT     474364627777                                             GPLI0914
*                                                                       GPLI0915
)PJ11          -1,,-*-1                                                 GPLI0916
               PNAME,,-*-1                                              GPLI0917
               -*-1,,-*-3         PLUSS                                 GPLI0918
               -*-1                                                     GPLI0919
       OCT     474364626277                                             GPLI0920
               APVAL1,,-*-1                                             GPLI0921
               -*-1                                                     GPLI0922
               H20                                                      GPLI0923
*                                                                       GPLI0924
)083           -1,,-*-1                                                 GPLI0925
               PNAME,,-*-1                                              GPLI0926
               -*-1               PNAME                                 GPLI0927
               -*-1                                                     GPLI0928
       OCT     474521442577                                             GPLI0929
*                                                                       GPLI0930
)PJ33          -1,,-*-1                                                 GPLI0931
               SUBR,,-*-1                                               GPLI0932
               -*-1,,-*-2                                               GPLI0933
       TXL     $PRIN1,,1                                                GPLI0934
               PNAME,,-*-1                                              GPLI0935
               -*-1               PRIN1                                 GPLI0936
               -*-1                                                     GPLI0937
       OCT     475131450177                                             GPLI0938
*                                                                       GPLI0939
)087           -1,,-*-1                                                 GPLI0940
               SUBR,,-*-1                                               GPLI0941
               -*-1,,-*-2                                               GPLI0942
       TXL     PRINT,,1                                                 GPLI0943
               PNAME,,-*-1                                              GPLI0944
               -*-1               PRINT                                 GPLI0945
               -*-1                                                     GPLI0946
       OCT     475131456377                                             GPLI0947
*                                                                       GPLI0948
)PJ39          -1,,-*-1                                                 GPLI0949
               SUBR,,-*-1                                               GPLI0950
               -*-1,,-*-2                                               GPLI0951
       TXL     $PRIN2,,1                                                GPLI0952
               PNAME,,-*-1                                              GPLI0953
               -*-1               PRINT2                                GPLI0954
               -*-1                                                     GPLI0955
       OCT     475131456302                                             GPLI0956
*                                                                       GPLI0957
)089           -1,,-*-1                                                 GPLI0958
               FSUBR,,-*-1                                              GPLI0959
               -*-1,,-*-2                                               GPLI0960
       TXL     INTER,,0                                                 GPLI0961
               PNAME,,-*-1                                              GPLI0962
               -*-1               PROG                                  GPLI0963
               -*-1                                                     GPLI0964
       OCT     475146277777                                             GPLI0965
*                                                                       GPLI0966
IJ05           -1,,-*-1                                                 GPLI0967
               $SUBR,,-*-1                                              GPLI0968
               -*-1,,-*-2                                               GPLI0969
       TXL     $PUNCH,,1                                                GPLI0970
               $PNAME,,-*-1                                             GPLI0971
               -*-1               PUNCH                                 GPLI0972
               -*-1                                                     GPLI0973
       OCT     476445233077                                             GPLI0974
*                                                                       GPLI0975
)090           -1,,-*-1                                                 GPLI0976
               SUBR,,-*-1                                               GPLI0977
               -*-1,,-*-2                                               GPLI0978
       TXL     APROP,,3                                                 GPLI0979
               PNAME,,-*-1                                              GPLI0980
               -*-1               PROP                                  GPLI0981
               -*-1                                                     GPLI0982
       OCT     475146477777                                             GPLI0983
*                                                                       GPLI0984
)094           -1,,-*-1                                                 GPLI0985
               FSUBR,,-*-1                                              GPLI0986
               -*-1,,-*-2                                               GPLI0987
       TXL     CARP,,0                                                  GPLI0988
               PNAME,,-*-1                                              GPLI0989
               -*-1               QUOTE                                 GPLI0990
               -*-1                                                     GPLI0991
       OCT     506446632577                                             GPLI0992
*                                                                       GPLI0993
IJ03           -1,,-*-1                                                 GPLI0994
               $SUBR,,-*-1                                              GPLI0995
               -*-1,,-*-2                                               GPLI0996
       TXL     QUOTEN,,2                                                GPLI0997
               $PNAME,,-*-1                                             GPLI0998
               -*-1               QUOTIENT                              GPLI0999
               -*-1,,-*-2                                               GPLI1000
       OCT     506446633125                                             GPLI1001
               -*-1                                                     GPLI1002
       OCT     456377777777                                             GPLI1003
*                                                                       GPLI1004
)096           -1,,-*-1                                                 GPLI1005
               SUBR,,-*-1                                               GPLI1006
               -*-1,,-*-2                                               GPLI1007
       TXL     READ,,0                                                  GPLI1008
               PNAME,,-*-1                                              GPLI1009
               -*-1               READ                                  GPLI1010
               -*-1                                                     GPLI1011
       OCT     512521247777                                             GPLI1012
*                                                                       GPLI1013
II18           -1,,-*-1                                                 GPLI1014
               $SUBR,,-*-1                                              GPLI1015
               -*-1,,-*-2                                               GPLI1016
       TXL     RCPPRG,,1                                                GPLI1017
               $PNAME,,-*-1                                             GPLI1018
               -*-1               RECIP                                 GPLI1019
               -*-1                                                     GPLI1020
       OCT     512523314777                                             GPLI1021
*                                                                       GPLI1022
)234D          -1,,-*-1                                                 GPLI1023
               SUBR,,-*-1                                               GPLI1024
               -*-1,,-*-2                                               GPLI1025
       TXL     RECLAM,,0                                                GPLI1026
               PNAME,,-*-1                                              GPLI1027
               -*-1               RECLAIM                               GPLI1028
               -*-1,,-*-2                                               GPLI1029
       OCT     512523432131                                             GPLI1030
               -*-1                                                     GPLI1031
       OCT     447777777777                                             GPLI1032
*                                                                       GPLI1033
IJ04           -1,,-*-1                                                 GPLI1034
               $SUBR,,-*-1                                              GPLI1035
               -*-1,,-*-2                                               GPLI1036
       TXL     REMAIN,,2                                                GPLI1037
               $PNAME,,-*-1                                             GPLI1038
               -*-1               REMAINDER                             GPLI1039
               -*-1,,-*-2                                               GPLI1040
       OCT     512544213145                                             GPLI1041
               -*-1                                                     GPLI1042
       OCT     242551777777                                             GPLI1043
*                                                                       GPLI1044
)250           -1,,-*-1                                                 GPLI1045
               SUBR,,-*-1                                               GPLI1046
               -*-1,,-*-2                                               GPLI1047
       TXL     REMPRP,,2                                                GPLI1048
               PNAME,,-*-1                                              GPLI1049
               -*-1               REMPROP                               GPLI1050
               -*-1,,-*-2                                               GPLI1051
       OCT     512544475146                                             GPLI1052
               -*-1                                                     GPLI1053
       OCT     477777777777                                             GPLI1054
*                                                                       GPLI1055
)I02           -1,,-*-1                                                 GPLI1056
               $SUBR,,-*-1                                              GPLI1057
               -*-1,,-*-2                                               GPLI1058
       TXL     RETURN,,1                                                GPLI1059
               $PNAME,,-*-1                                             GPLI1060
               -*-1               RETURN                                GPLI1061
               -*-1                                                     GPLI1062
       OCT     512563645145                                             GPLI1063
*                                                                       GPLI1064
)100           -1,,-*-1                                                 GPLI1065
               SUBR,,-*-1                                               GPLI1066
               -*-1,,-*-2                                               GPLI1067
       TXL     RPLACA,,0                                                GPLI1068
               PNAME,,-*-1                                              GPLI1069
               -*-1               RPLACA                                GPLI1070
               -*-1                                                     GPLI1071
       OCT     514743212321                                             GPLI1072
*                                                                       GPLI1073
)101           -1,,-*-1                                                 GPLI1074
               SUBR,,-*-1                                               GPLI1075
               -*-1,,-*-2                                               GPLI1076
       TXL     RPLACD,,0                                                GPLI1077
               PNAME,,-*-1                                              GPLI1078
               -*-1               RPLACD                                GPLI1079
               -*-1                                                     GPLI1080
       OCT     514743212324                                             GPLI1081
*                                                                       GPLI1082
)PJ8           -1,,-*-1                                                 GPLI1083
               PNAME,,-*-1                                              GPLI1084
               -*-1,,-*-3         RPAR                                  GPLI1085
               -*-1                                                     GPLI1086
       OCT     514721517777                                             GPLI1087
               APVAL1,,-*-1                                             GPLI1088
               -*-1                                                     GPLI1089
               H34                                                      GPLI1090
*                                                                       GPLI1091
)SPCL          -1,,-*-1                                                 GPLI1092
               PNAME,,-*-1                                              GPLI1093
               -*-1               SPECIAL                               GPLI1094
               -*-1,,-*-2                                               GPLI1095
       OCT     624725233121                                             GPLI1096
               -*-1                                                     GPLI1097
       OCT     437777777777                                             GPLI1098
*                                                                       GPLI1099
)MOV           -1,,-*-1                                                 GPLI1100
               PNAME,,-*-1                                              GPLI1101
               -*-1,,-*-3         *MOVE                                 GPLI1102
               -*-1                                                     GPLI1103
       OCT     544446652577                                             GPLI1104
               SYM,,-*-1                                                GPLI1105
       MZE     -C$MOV                                                   GPLI1106
*                                                                       GPLI1107
)RTRN          -1,,-*-1                                                 GPLI1108
               PNAME,,-*-1                                              GPLI1109
               -*-1,,-*-5         *RETURN                               GPLI1110
               -*-1,,-*-2                                               GPLI1111
       OCT     545125636451                                             GPLI1112
               -*-1                                                     GPLI1113
       OCT     457777777777                                             GPLI1114
               SYM,,-*-1                                                GPLI1115
       MZE     -C$RTRN                                                  GPLI1116
*                                                                       GPLI1117
)LST           -1,,-*-1                                                 GPLI1118
               PNAME,,-*-1                                              GPLI1119
               -*-1,,-*-3         *LIST                                 GPLI1120
               -*-1                                                     GPLI1121
       OCT     544331626377                                             GPLI1122
               SYM,,-*-1                                                GPLI1123
       MZE     -C$LSTR                                                  GPLI1124
*                                                                       GPLI1125
)I06           -1,,-*-1                                                 GPLI1126
               SUBR,,-*-1                                               GPLI1127
               -*-1,,-*-2                                               GPLI1128
       TXL     APSSOC,,3                                                GPLI1129
               PNAME,,-*-1                                              GPLI1130
               -*-1               SASSOC                                GPLI1131
               -*-1                                                     GPLI1132
       OCT     622162624623                                             GPLI1133
*                                                                       GPLI1134
)236           -1,,-*-1                                                 GPLI1135
               SUBR,,-*-1                                               GPLI1136
               -*-1,,-*-2                                               GPLI1137
       TXL     SEARCH,,4                                                GPLI1138
               PNAME,,-*-1                                              GPLI1139
               -*-1               SEARCH                                GPLI1140
               -*-1                                                     GPLI1141
       OCT     622521512330                                             GPLI1142
*                                                                       GPLI1143
)107           -1,,-*-1                                                 GPLI1144
               $SUBR,,-*-1                                              GPLI1145
               -*-1,,-*-2                                               GPLI1146
       TXL     SETP,,2                                                  GPLI1147
               $PNAME,,-*-1                                             GPLI1148
               -*-1               SET                                   GPLI1149
               -*-1                                                     GPLI1150
       OCT     622563777777                                             GPLI1151
*                                                                       GPLI1152
)108           -1,,-*-1                                                 GPLI1153
               $FSUBR,,-*-1                                             GPLI1154
               -*-1,,-*-2                                               GPLI1155
       TXL     SETQP,,0                                                 GPLI1156
               PNAME,,-*-1                                              GPLI1157
               -*-1               SETQ                                  GPLI1158
               -*-1                                                     GPLI1159
       OCT     622563507777                                             GPLI1160
*                                                                       GPLI1161
)PJ14          -1,,-*-1                                                 GPLI1162
               PNAME,,-*-1                                              GPLI1163
               -*-1,,-*-3         SLASH                                 GPLI1164
               -*-1                                                     GPLI1165
       OCT     624321623077                                             GPLI1166
               APVAL1,,-*-1                                             GPLI1167
               -*-1                                                     GPLI1168
               H61                                                      GPLI1169
*                                                                       GPLI1170
)109           -1,,-*-1                                                 GPLI1171
               SUBR,,-*-1                                               GPLI1172
               -*-1,,-*-2                                               GPLI1173
       TXL     SPEAK,,4                                                 GPLI1174
               PNAME,,-*-1                                              GPLI1175
               -*-1               SPEAK                                 GPLI1176
               -*-1                                                     GPLI1177
       OCT     624725214277                                             GPLI1178
*                                                                       GPLI1179
)111           -1,,-*-1                                                 GPLI1180
               PNAME,,-*-1                                              GPLI1181
               -*-1               STOP                                  GPLI1182
               -*-1                                                     GPLI1183
       OCT     626346477777                                             GPLI1184
*                                                                       GPLI1185
)PJ15          -1,,-*-1                                                 GPLI1186
               PNAME,,-*-1                                              GPLI1187
               -*-1,,-*-3         STAR                                  GPLI1188
               -*-1                                                     GPLI1189
       OCT     626321517777                                             GPLI1190
               APVAL1,,-*-1                                             GPLI1191
               -*-1                                                     GPLI1192
               H54                                                      GPLI1193
*                                                                       GPLI1194
)PJ21          -1,,-*-1                                                 GPLI1195
               SUBR,,-*-1                                               GPLI1196
               -*-1,,-*-2                                               GPLI1197
       TXL     STREAD,,0                                                GPLI1198
               PNAME,,-*-1                                              GPLI1199
               -*-1               STARTREAD                             GPLI1200
               -*-1,,-*-2                                               GPLI1201
       OCT     626321516351                                             GPLI1202
               -*-1                                                     GPLI1203
       OCT     252124777777                                             GPLI1204
*                                                                       GPLI1205
II15           -1,,-*-1                                                 GPLI1206
               $SUBR,,-*-1                                              GPLI1207
               -*-1,,-*-2                                               GPLI1208
       TXL     SUB1,,2                                                  GPLI1209
               $PNAME,,-*-1                                             GPLI1210
               -*-1               SUB1                                  GPLI1211
               -*-1                                                     GPLI1212
       OCT     626422017777                                             GPLI1213
*                                                                       GPLI1214
)113           -1,,-*-1                                                 GPLI1215
               PNAME,,-*-1                                              GPLI1216
               -*-1               SUBR                                  GPLI1217
               -*-1                                                     GPLI1218
       OCT     626422517777                                             GPLI1219
*                                                                       GPLI1220
)114           -1,,-*-1                                                 GPLI1221
               SUBR,,-*-1                                               GPLI1222
               -*-1,,-*-2                                               GPLI1223
       TXL     SUBLIS,,2                                                GPLI1224
               PNAME,,-*-1                                              GPLI1225
               -*-1               SUBLIS                                GPLI1226
               -*-1                                                     GPLI1227
       OCT     626422433162                                             GPLI1228
*                                                                       GPLI1229
)115           -1,,-*-1                                                 GPLI1230
               SUBR,,-*-1                                               GPLI1231
               -*-1,,-*-2                                               GPLI1232
       TXL     SUBST,,3                                                 GPLI1233
               PNAME,,-*-1                                              GPLI1234
               -*-1               SUBST                                 GPLI1235
               -*-1                                                     GPLI1236
       OCT     626422626377                                             GPLI1237
*                                                                       GPLI1238
)SYM           -1,,-*-1                                                 GPLI1239
               PNAME,,-*-1                                              GPLI1240
               -*-1               SYM                                   GPLI1241
               -*-1                                                     GPLI1242
       OCT     627044777777                                             GPLI1243
*                                                                       GPLI1244
)PJ23          -1,,-*-1                                                 GPLI1245
               SUBR,,-*-1                                               GPLI1246
               -*-1,,-*-2                                               GPLI1247
       TXL     TERPRI,,0                                                GPLI1248
               PNAME,,-*-1                                              GPLI1249
               -*-1               TERPRI                                GPLI1250
               -*-1                                                     GPLI1251
       OCT     632551475131                                             GPLI1252
*                                                                       GPLI1253
)122           -1,,-*-1                                                 GPLI1254
               SUBR,,-*-1                                               GPLI1255
               -*-1,,-*-2                                               GPLI1256
       TXL     $TIME,,0                                                 GPLI1257
               PNAME,,-*-1                                              GPLI1258
               -*-1               TEMPUS-FUGIT                          GPLI1259
               -*-1,,-*-2                                               GPLI1260
       OCT     632544476462                                             GPLI1261
               -*-1                                                     GPLI1262
       OCT     402664273163                                             GPLI1263
*                                                                       GPLI1264
)124           -1,,-*-1                                                 GPLI1265
               $FSUBR,,-*-1                                             GPLI1266
               -*-1,,-*-2                                               GPLI1267
       TXL     MULT,,2                                                  GPLI1268
               $PNAME,,-*-1                                             GPLI1269
               -*-1               TIMES                                 GPLI1270
               -*-1                                                     GPLI1271
       OCT     633144256277                                             GPLI1272
*                                                                       GPLI1273
)213           -1,,-*-1                                                 GPLI1274
               $PNAME,,-*-1                                             GPLI1275
               -*-1               TRACE                                 GPLI1276
               -*-1                                                     GPLI1277
       OCT     635121232577                                             GPLI1278
*                                                                       GPLI1279
)127           -1,,-*-1                                                 GPLI1280
               SUBR,,-*-1                                               GPLI1281
               -*-1,,-*-2                                               GPLI1282
       TXL     UNCONT,,0                                                GPLI1283
               PNAME,,-*-1                                              GPLI1284
               -*-1               UNCOUNT                               GPLI1285
               -*-1,,-*-2                                               GPLI1286
       OCT     644523466445                                             GPLI1287
               -*-1                                                     GPLI1288
       OCT     637777777777                                             GPLI1289
*                                                                       GPLI1290
)PJ31          -1,,-*-1                                                 GPLI1291
               SUBR,,-*-1                                               GPLI1292
               -*-1,,-*-2                                               GPLI1293
       TXL     UNPACK,,1                                                GPLI1294
               PNAME,,-*-1                                              GPLI1295
               -*-1               UNPACK                                GPLI1296
               -*-1                                                     GPLI1297
       OCT     644547212342                                             GPLI1298
*                                                                       GPLI1299
II10           -1,,-*-1                                                 GPLI1300
               $SUBR,,-*-1                                              GPLI1301
               -*-1,,-*-2                                               GPLI1302
       TXL     ZEROP,,1                                                 GPLI1303
               $PNAME,,-*-1                                             GPLI1304
               -*-1               ZEROP                                 GPLI1305
               -*-1                                                     GPLI1306
       OCT     712551464777                                             GPLI1307
*                                                                       GPLI1308
       EJECT                                                            PAGE 209
*
*
       EJECT                                                            PAGE 210
*      PROPERTY LISTS FOR ALPHABETIC OBJECTS
*
HH00           0                                                        GPLA0000
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0001
HH01           1                                                        GPLA0002
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0003
HH02           2                                                        GPLA0004
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0005
HH03           3                                                        GPLA0006
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0007
HH04           4                                                        GPLA0008
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0009
HH05           5                                                        GPLA0010
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0011
HH06           6                                                        GPLA0012
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0013
HH07           7                                                        GPLA0014
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0015
HH10   OCT     10                                                       GPLA0016
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0017
HH11   OCT     11                                                       GPLA0018
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0019
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0020
HH12           PNAME,,-*-1        END OF FILE                           GPLA0021
               -*-1,,-*-3         $EOF$                                 GPLA0022
               -*-1                                                     GPLA0023
       OCT     532546265377                                             GPLA0024
               APVAL1,,-*-1                                             GPLA0025
               -*-1                                                     GPLA0026
               H12                                                      GPLA0027
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0028
HH13           PNAME,,-*-1        =                                     GPLA0029
               -*-1               =                                     GPLA0030
               -*-1                                                     GPLA0031
       OCT     137777777777                                             GPLA0032
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0033
HH14           PNAME,,-*-1        8-4 MINUS                             GPLA0034
               -*-1                                                     GPLA0035
               -*-1                                                     GPLA0036
       OCT     147777777777                                             GPLA0037
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0038
HH15           PNAME,,-*-1        ILLEGAL                               GPLA0039
               -*-1               $IL15$                                GPLA0040
               -*-1                                                     GPLA0041
       OCT     533143010553                                             GPLA0042
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0043
HH16           PNAME,,-*-1        ILLEGAL                               GPLA0044
               -*-1               $IL16$                                GPLA0045
               -*-1                                                     GPLA0046
       OCT     533143010653                                             GPLA0047
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0048
HH17           PNAME,,-*-1        ILLEGAL                               GPLA0049
               -*-1               $IL17$                                GPLA0050
               -*-1                                                     GPLA0051
       OCT     533143010753                                             GPLA0052
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0053
HH20           PNAME,,-*-1        +                                     GPLA0054
               -*-1               +                                     GPLA0055
               -*-1                                                     GPLA0056
       OCT     207777777777                                             GPLA0057
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0058
HH21           PNAME,,-*-1        A                                     GPLA0059
               -*-1               A                                     GPLA0060
               -*-1                                                     GPLA0061
       OCT     217777777777                                             GPLA0062
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0063
HH22           PNAME,,-*-1        B                                     GPLA0064
               -*-1               B                                     GPLA0065
               -*-1                                                     GPLA0066
       OCT     227777777777                                             GPLA0067
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0068
HH23           PNAME,,-*-1        C                                     GPLA0069
               -*-1               C                                     GPLA0070
               -*-1                                                     GPLA0071
       OCT     237777777777                                             GPLA0072
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0073
HH24           PNAME,,-*-1        D                                     GPLA0074
               -*-1               D                                     GPLA0075
               -*-1                                                     GPLA0076
       OCT     247777777777                                             GPLA0077
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0078
HH25           PNAME,,-*-1        E                                     GPLA0079
               -*-1               E                                     GPLA0080
               -*-1                                                     GPLA0081
       OCT     257777777777                                             GPLA0082
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0083
HH26           -1,,-*-1           F                                     GPLA0084
               PNAME,,-*-1                                              GPLA0085
               -*-1,,-*-3         F                                     GPLA0086
               -*-1                                                     GPLA0087
       OCT     267777777777                                             GPLA0088
               APVAL,,-*-1                                              GPLA0089
               -*-1                                                     GPLA0090
               0                                                        GPLA0091
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0092
HH27           PNAME,,-*-1        G                                     GPLA0093
               -*-1               G                                     GPLA0094
               -*-1                                                     GPLA0095
       OCT     277777777777                                             GPLA0096
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0097
HH30           PNAME,,-*-1        H                                     GPLA0098
               -*-1               H                                     GPLA0099
               -*-1                                                     GPLA0100
       OCT     307777777777                                             GPLA0101
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0102
HH31           PNAME,,-*-1        I                                     GPLA0103
               -*-1               I                                     GPLA0104
               -*-1                                                     GPLA0105
       OCT     317777777777                                             GPLA0106
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0107
HH32           PNAME,,-*-1        +0                                    GPLA0108
               -*-1               $IL32$                                GPLA0109
               -*-1                                                     GPLA0110
       OCT     533143030253                                             GPLA0111
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0112
HH33           PNAME,,-*-1        .                                     GPLA0113
               -*-1               .                                     GPLA0114
               -*-1                                                     GPLA0115
       OCT     337777777777                                             GPLA0116
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0117
HH34           PNAME,,-*-1        )                                     GPLA0118
               -*-1               )                                     GPLA0119
               -*-1                                                     GPLA0120
       OCT     347777777777                                             GPLA0121
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0122
HH35           PNAME,,-*-1        ILLEGAL                               GPLA0123
               -*-1               $IL35$                                GPLA0124
               -*-1                                                     GPLA0125
       OCT     533143030553                                             GPLA0126
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0127
HH36           PNAME,,-*-1        ILLEGAL                               GPLA0128
               -*-1               $IL36$                                GPLA0129
               -*-1                                                     GPLA0130
       OCT     533143030653                                             GPLA0131
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0132
HH37           PNAME,,-*-1        ILLEGAL                               GPLA0133
               -*-1               $IL37$                                GPLA0134
               -*-1                                                     GPLA0135
       OCT     533143030753                                             GPLA0136
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0137
HH40           PNAME,,-*-1        11 MINUS                              GPLA0138
               -*-1               -                                     GPLA0139
               -*-1                                                     GPLA0140
       OCT     407777777777                                             GPLA0141
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0142
HH41           PNAME,,-*-1        J                                     GPLA0143
               -*-1               J                                     GPLA0144
               -*-1                                                     GPLA0145
       OCT     417777777777                                             GPLA0146
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0147
HH42           PNAME,,-*-1        K                                     GPLA0148
               -*-1               K                                     GPLA0149
               -*-1                                                     GPLA0150
       OCT     427777777777                                             GPLA0151
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0152
HH43           PNAME,,-*-1        L                                     GPLA0153
               -*-1               L                                     GPLA0154
               -*-1                                                     GPLA0155
       OCT     437777777777                                             GPLA0156
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0157
HH44           PNAME,,-*-1        M                                     GPLA0158
               -*-1               M                                     GPLA0159
               -*-1                                                     GPLA0160
       OCT     447777777777                                             GPLA0161
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0162
HH45           PNAME,,-*-1        N                                     GPLA0163
               -*-1               N                                     GPLA0164
               -*-1                                                     GPLA0165
       OCT     457777777777                                             GPLA0166
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0167
HH46           PNAME,,-*-1        O                                     GPLA0168
               -*-1               O                                     GPLA0169
               -*-1                                                     GPLA0170
       OCT     467777777777                                             GPLA0171
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0172
HH47           PNAME,,-*-1        P                                     GPLA0173
               -*-1               P                                     GPLA0174
               -*-1                                                     GPLA0175
       OCT     477777777777                                             GPLA0176
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0177
HH50           PNAME,,-*-1        Q                                     GPLA0178
               -*-1               Q                                     GPLA0179
               -*-1                                                     GPLA0180
       OCT     507777777777                                             GPLA0181
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0182
HH51           PNAME,,-*-1        R                                     GPLA0183
               -*-1               R                                     GPLA0184
               -*-1                                                     GPLA0185
       OCT     517777777777                                             GPLA0186
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0187
HH52           PNAME,,-*-1        -0                                    GPLA0188
               -*-1               $IL52$                                GPLA0189
               -*-1                                                     GPLA0190
       OCT     533143050253                                             GPLA0191
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0192
HH53           PNAME,,-*-1        $                                     GPLA0193
               -*-1               $                                     GPLA0194
               -*-1                                                     GPLA0195
       OCT     537777777777                                             GPLA0196
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0197
HH54           PNAME,,-*-1        *                                     GPLA0198
               -*-1,,-*-3         *                                     GPLA0199
               -*-1                                                     GPLA0200
       OCT     547777777777                                             GPLA0201
               SYM,,-*-1                                                GPLA0202
               -C$STAR                                                  GPLA0203
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0204
HH55           PNAME,,-*-1        ILLEGAL                               GPLA0205
               -*-1               $IL55$                                GPLA0206
               -*-1                                                     GPLA0207
       OCT     533143050553                                             GPLA0208
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0209
HH56           PNAME,,-*-1        ILLEGAL                               GPLA0210
               -*-1               $IL56$                                GPLA0211
               -*-1                                                     GPLA0212
       OCT     533143050653                                             GPLA0213
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0214
HH57           PNAME,,-*-1        ILLEGAL                               GPLA0215
               -*-1               $IL57$                                GPLA0216
               -*-1                                                     GPLA0217
       OCT     533143050753                                             GPLA0218
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0219
HH60           PNAME,,-*-1        BLANK                                 GPLA0220
               -*-1                                                     GPLA0221
               -*-1                                                     GPLA0222
       OCT     607777777777                                             GPLA0223
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0224
HH61           PNAME,,-*-1        /                                     GPLA0225
               -*-1               /                                     GPLA0226
               -*-1                                                     GPLA0227
       OCT     617777777777                                             GPLA0228
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0229
HH62           PNAME,,-*-1        S                                     GPLA0230
               -*-1               S                                     GPLA0231
               -*-1                                                     GPLA0232
       OCT     627777777777                                             GPLA0233
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0234
HH63           -1,,-*-1           T                                     GPLA0235
               PNAME,,-*-1                                              GPLA0236
               -*-1,,-*-3         T                                     GPLA0237
               -*-1                                                     GPLA0238
       OCT     637777777777                                             GPLA0239
               APVAL,,-*-1                                              GPLA0240
               -*-1                                                     GPLA0241
               1                                                        GPLA0242
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0243
HH64           PNAME,,-*-1        U                                     GPLA0244
               -*-1               U                                     GPLA0245
               -*-1                                                     GPLA0246
       OCT     647777777777                                             GPLA0247
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0248
HH65           PNAME,,-*-1        V                                     GPLA0249
               -*-1               V                                     GPLA0250
               -*-1                                                     GPLA0251
       OCT     657777777777                                             GPLA0252
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0253
HH66           PNAME,,-*-1        W                                     GPLA0254
               -*-1               W                                     GPLA0255
               -*-1                                                     GPLA0256
       OCT     667777777777                                             GPLA0257
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0258
HH67           PNAME,,-*-1        X                                     GPLA0259
               -*-1               X                                     GPLA0260
               -*-1                                                     GPLA0261
       OCT     677777777777                                             GPLA0262
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0263
HH70           PNAME,,-*-1        Y                                     GPLA0264
               -*-1               Y                                     GPLA0265
               -*-1                                                     GPLA0266
       OCT     707777777777                                             GPLA0267
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0268
HH71           PNAME,,-*-1        Z                                     GPLA0269
               -*-1               Z                                     GPLA0270
               -*-1                                                     GPLA0271
       OCT     717777777777                                             GPLA0272
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0273
HH72           PNAME,,-*-1        END OF RECORD                         GPLA0274
               -*-1,,-*-3         $EOR$                                 GPLA0275
               -*-1                                                     GPLA0276
       OCT     532546515377                                             GPLA0277
               APVAL1,,-*-1                                             GPLA0278
               -*-1                                                     GPLA0279
               H72                                                      GPLA0280
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0281
HH73           PNAME,,-*-1        ,                                     GPLA0282
               -*-1               ,                                     GPLA0283
               -*-1                                                     GPLA0284
       OCT     737777777777                                             GPLA0285
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0286
HH74           PNAME,,-*-1        (                                     GPLA0287
               -*-1               (                                     GPLA0288
               -*-1                                                     GPLA0289
       OCT     747777777777                                             GPLA0290
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0291
HH75           PNAME,,-*-1        ILLEGAL                               GPLA0292
               -*-1               $IL75$                                GPLA0293
               -*-1                                                     GPLA0294
       OCT     533143070553                                             GPLA0295
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0296
HH76           PNAME,,-*-1        ILLEGAL                               GPLA0297
               -*-1               $IL76$                                GPLA0298
               -*-1                                                     GPLA0299
       OCT     533143070653                                             GPLA0300
       REM * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *GPLA0301
HH77           PNAME,,-*-1        ILLEGAL                               GPLA0302
               -*-1               $IL77$                                GPLA0303
               -*-1                                                     GPLA0304
       OCT     533143070753                                             GPLA0305
*                                                                       GPLA0306
*                                                                       GPLA0307
)H77           -1,,-HH77                                                GPLA0308
)H76           -1,,-HH76                                                GPLA0309
)H75           -1,,-HH75                                                GPLA0310
)H74           -1,,-HH74                                                GPLA0311
)H73           -1,,-HH73                                                GPLA0312
)H72           -1,,-HH72                                                GPLA0313
)H71           -1,,-HH71                                                GPLA0314
)H70           -1,,-HH70                                                GPLA0315
)H67           -1,,-HH67                                                GPLA0316
)H66           -1,,-HH66                                                GPLA0317
)H65           -1,,-HH65                                                GPLA0318
)H64           -1,,-HH64                                                GPLA0319
)H63           -1,,-HH63                                                GPLA0320
)H62           -1,,-HH62                                                GPLA0321
)H61           -1,,-HH61                                                GPLA0322
)H60           -1,,-HH60                                                GPLA0323
)H57           -1,,-HH57                                                GPLA0324
)H56           -1,,-HH56                                                GPLA0325
)H55           -1,,-HH55                                                GPLA0326
)H54           -1,,-HH54                                                GPLA0327
)H53           -1,,-HH53                                                GPLA0328
)H52           -1,,-HH52                                                GPLA0329
)H51           -1,,-HH51                                                GPLA0330
)H50           -1,,-HH50                                                GPLA0331
)H47           -1,,-HH47                                                GPLA0332
)H46           -1,,-HH46                                                GPLA0333
)H45           -1,,-HH45                                                GPLA0334
)H44           -1,,-HH44                                                GPLA0335
)H43           -1,,-HH43                                                GPLA0336
)H42           -1,,-HH42                                                GPLA0337
)H41           -1,,-HH41                                                GPLA0338
)H40           -1,,-HH40                                                GPLA0339
)H37           -1,,-HH37                                                GPLA0340
)H36           -1,,-HH36                                                GPLA0341
)H35           -1,,-HH35                                                GPLA0342
)H34           -1,,-HH34                                                GPLA0343
)H33           -1,,-HH33                                                GPLA0344
)H32           -1,,-HH32                                                GPLA0345
)H31           -1,,-HH31                                                GPLA0346
)H30           -1,,-HH30                                                GPLA0347
)H27           -1,,-HH27                                                GPLA0348
)H26           -1,,-HH26                                                GPLA0349
)H25           -1,,-HH25                                                GPLA0350
)H24           -1,,-HH24                                                GPLA0351
)H23           -1,,-HH23                                                GPLA0352
)H22           -1,,-HH22                                                GPLA0353
)H21           -1,,-HH21                                                GPLA0354
)H20           -1,,-HH20                                                GPLA0355
)H17           -1,,-HH17                                                GPLA0356
)H16           -1,,-HH16                                                GPLA0357
)H15           -1,,-HH15                                                GPLA0358
)H14           -1,,-HH14                                                GPLA0359
)H13           -1,,-HH13                                                GPLA0360
)H12           -1,,-HH12                                                GPLA0361
)H11           -1,1,-HH11                                               GPLA0362
)H10           -1,1,-HH10                                               GPLA0363
)H07           -1,1,-HH07                                               GPLA0364
)H06           -1,1,-HH06                                               GPLA0365
)H05           -1,1,-HH05                                               GPLA0366
)H04           -1,1,-HH04                                               GPLA0367
)H03           -1,1,-HH03                                               GPLA0368
)H02           -1,1,-HH02                                               GPLA0369
)H01           -1,1,-HH01                                               GPLA0370
)H00           -1,1,-HH00                                               GPLA0371
UPERML BSS     0
       EJECT                                                            PAGE 217
       EJECT                                                            PAGE 218
       HEAD    0
*      SYN CARDS CAUSE MANY SYMBOLS TO HAVE O-HEADED EQUIVALENTS
*
H00    SYN     -)H00                                                    GPLA0372
H01    SYN     -)H01                                                    GPLA0373
H02    SYN     -)H02                                                    GPLA0374
H03    SYN     -)H03                                                    GPLA0375
H04    SYN     -)H04                                                    GPLA0376
H05    SYN     -)H05                                                    GPLA0377
H06    SYN     -)H06                                                    GPLA0378
H07    SYN     -)H07                                                    GPLA0379
H10    SYN     -)H10                                                    GPLA0380
H11    SYN     -)H11                                                    GPLA0381
H12    SYN     -)H12                                                    GPLA0382
H13    SYN     -)H13                                                    GPLA0383
H14    SYN     -)H14                                                    GPLA0384
H15    SYN     -)H15                                                    GPLA0385
H16    SYN     -)H16                                                    GPLA0386
H17    SYN     -)H17                                                    GPLA0387
H20    SYN     -)H20                                                    GPLA0388
H21    SYN     -)H21                                                    GPLA0389
H22    SYN     -)H22                                                    GPLA0390
H23    SYN     -)H23                                                    GPLA0391
H24    SYN     -)H24                                                    GPLA0392
H25    SYN     -)H25                                                    GPLA0393
H26    SYN     -)H26                                                    GPLA0394
H27    SYN     -)H27                                                    GPLA0395
H30    SYN     -)H30                                                    GPLA0396
H31    SYN     -)H31                                                    GPLA0397
H32    SYN     -)H32                                                    GPLA0398
H33    SYN     -)H33                                                    GPLA0399
H34    SYN     -)H34                                                    GPLA0400
H35    SYN     -)H35                                                    GPLA0401
H36    SYN     -)H36                                                    GPLA0402
H37    SYN     -)H37                                                    GPLA0403
H40    SYN     -)H40                                                    GPLA0404
H41    SYN     -)H41                                                    GPLA0405
H42    SYN     -)H42                                                    GPLA0406
H43    SYN     -)H43                                                    GPLA0407
H44    SYN     -)H44                                                    GPLA0408
H45    SYN     -)H45                                                    GPLA0409
H46    SYN     -)H46                                                    GPLA0410
H47    SYN     -)H47                                                    GPLA0411
H50    SYN     -)H50                                                    GPLA0412
H51    SYN     -)H51                                                    GPLA0413
H52    SYN     -)H52                                                    GPLA0414
H53    SYN     -)H53                                                    GPLA0415
H54    SYN     -)H54                                                    GPLA0416
H55    SYN     -)H55                                                    GPLA0417
H56    SYN     -)H56                                                    GPLA0418
H57    SYN     -)H57                                                    GPLA0419
H60    SYN     -)H60                                                    GPLA0420
H61    SYN     -)H61                                                    GPLA0421
H62    SYN     -)H62                                                    GPLA0422
H63    SYN     -)H63                                                    GPLA0423
H64    SYN     -)H64                                                    GPLA0424
H65    SYN     -)H65                                                    GPLA0425
H66    SYN     -)H66                                                    GPLA0426
H67    SYN     -)H67                                                    GPLA0427
H70    SYN     -)H70                                                    GPLA0428
H71    SYN     -)H71                                                    GPLA0429
H72    SYN     -)H72                                                    GPLA0430
H73    SYN     -)H73                                                    GPLA0431
H74    SYN     -)H74                                                    GPLA0432
H75    SYN     -)H75                                                    GPLA0433
H76    SYN     -)H76                                                    GPLA0434
H77    SYN     -)H77                                                    GPLA0435
 AND   SYN     -)002                                                    
 F1    SYN     -)003                                                    
 F18   SYN     -)004                                                    
 APVAL SYN     -)005
APVAL1 SYN     -)005
 ARRAY SYN     -II1 
 ATOM  SYN     -)007
 F29   SYN     -)008
 F     SYN     H26
 T     SYN     H63
 CAR   SYN     -)011                                                    
 CDR   SYN     -)012                                                    
 COMMA SYN     H73
 COND  SYN     -)016                                                    
 CONSN SYN     -)017                                                    
DUMP   SYN     -DMPOB                                                   
 F12   SYN     -)019
 COPYN SYN     -)020
 F35   SYN     -)021
 EQ    SYN     -)030                                                    
 F8    SYN     -)032
 F21   SYN     -)034                                                    
 F19   SYN     -)035                                                    
EVLISL SYN     -)036                                                    
 EXPR  SYN     -)037
 F32   SYN     -)038                                                    
 FEXPR SYN     -)040
 BIN   SYN     -)041
 FIX   SYN     -)041
 FLOAT SYN     -)042
 FSUBR SYN     -)043
FUNARG SYN     -)044
 FUNCT SYN     -)045
SYMGEN SYN     -)046
 CGET  SYN     -)231
 GO    SYN     -)047
 F16   SYN     -)052
 LABEL SYN     -)054
LAMBDA SYN     -)055
LAP    SYN     -)LAP                                                    
 LIST  SYN     -)057                                                    PAGE 220
 LOADA SYN     -)234A
PMAPCA SYN     -)065
 MAXP  SYN     -II7
 MINUS SYN     -)070
 MINP  SYN     -II8
 F3    SYN     -)071
 NIL   SYN     0
 NOT   SYN     -)074
 NULL  SYN     -)075
 OBLBA SYN     -)079A
 OR    SYN     -)079
 F2    SYN     -)080
 PAUSE SYN     -)234C
 PLB   SYN     -)234B
 PLUS  SYN     -)081
 PNAME SYN     -)083
 F4    SYN     -)087
 PROG  SYN     -)089
 PROPO SYN     -)090
 QUOTE SYN     -)094
 F13   SYN     -)096
 RCLAM SYN     -)234D
PRPLCA SYN     -)100
PRPLCD SYN     -)101
RETATM SYN     -)102
 SASCO SYN     -)106
SLIST  SYN     -)LST
SPECAL SYN     -)SPCL
SMOVE  SYN     -)MOV
SRETUR SYN     -)RTRN
 SRCH  SYN     -)236
 SET   SYN     -)107
 SETQ  SYN     -)108
 STOP  SYN     -)111
 F34   SYN     -)109
 SUBR  SYN     -)113
 F17   SYN     -)114
 F30   SYN     -)115
 F27   SYN     -)122
SYM    SYN     -)SYM
 TIMES SYN     -)124
TRACE  SYN     -)213
 F36   SYN     -)127
 CAAR  SYN     -)201
 CDAR  SYN     -)202
 CADR  SYN     -)203
 CDDR  SYN     -)204
 CAAAR SYN     -)205
 CAADR SYN     -)206
 CADAR SYN     -)207
 CADDR SYN     -)208
 CDAAR SYN     -)209
 CDADR SYN     -)210
 CDDAR SYN     -)211                                                    PAGE 221
 CDDDR SYN     -)212
REMPP  SYN     -)250
PJ1    SYN     -)PJ1                                                        0000
PJ2    SYN     -)PJ2                                                        0001
PJ4    SYN     -)PJ4                                                        0003
PJ5    SYN     -)PJ5                                                        0004
PJ6    SYN     -)PJ6                                                        0005
PJ7    SYN     -)PJ7                                                        0006
PJ8    SYN     -)PJ8                                                        0007
PJ9    SYN     -)PJ9                                                        0008
PJ10   SYN     -)PJ10                                                       0009
PJ11   SYN     -)PJ11                                                       0010
PJ12   SYN     -)PJ12                                                       0011
PJ14   SYN     -)PJ14                                                       0013
PJ15   SYN     -)PJ15                                                       0014
PJ16   SYN     -)PJ16                                                       0015
PJ17   SYN     -)PJ17                                                       0016
PJ18   SYN     -)PJ18                                                       0017
PJ19   SYN     -)PJ19                                                       0018
PJ21   SYN     -)PJ21                                                       0020
PJ23   SYN     -)PJ23                                                       0022
PJ24   SYN     -)PJ24                                                       0023
PJ25   SYN     -)PJ25                                                       0024
PJ26   SYN     -)PJ26                                                       0025
PJ27   SYN     -)PJ27                                                       0026
PJ28   SYN     -)PJ28                                                       0027
PJ30   SYN     -)PJ30                                                       0029
PJ31   SYN     -)PJ31                                                       0030
PJ32   SYN     -)PJ32                                                       0031
PJ33   SYN     -)PJ33                                                       0032
PJ34   SYN     -)PJ34                                                       0033
PJ35   SYN     -)PJ35                                                       0034
PJ36   SYN     -)PJ36                                                       0035
PJ37   SYN     -)PJ37                                                       0036
PJ38   SYN     -)PJ38                                                       0037
PJ39   SYN     -)PJ39                                                       0038
ERSETO SYN     -)PJ41
 PVW1  SYN     -PVV1              LEFTSHIFT
 OCT   SYN     PJ28
 RECIP SYN     -II18
 ADD1  SYN     Q$ADD1
 ADDP  SYN     Q$ADDP
 APP2  SYN     A$APP2
 APPLY SYN     A$APPLY
 APROP SYN     R$PROP
 ATOMP SYN     R$ATOMP
 CARP  SYN     R$CARP
 CDRP  SYN     R$CDRP
 CELL  SYN     T$CELL
 CHACT SYN     F$CHACT
 CLEAR SYN     F$CLEAR
 COPY  SYN     R$COPY
 CP1   SYN     C$CP1
 CURC  SYN     F$CURC                                                   PAGE 222
 CURC1 SYN     F$CURC1
 DECON SYN     E$DECON
 DIGIT SYN     F$DIGIT
 EQP   SYN     R$EQP
 EQUAL SYN     L$EQUAL
 EROR1 SYN     F$EROR1
 EVAL  SYN     A$EVAL
 EVALQ SYN     S$EVALQ
 EVAND SYN     R$EVA8
 EVCON SYN     A$EVCON
EVLIS  SYN     A$EVLIS
 EVOR  SYN     R$EVR8
 EXPT  SYN     Q$EXPT
 FIXP  SYN     Q$FIXP
 INPUT SYN     B$INPUT
 INTER SYN     R$INTER
 LABP  SYN     R$LABP
 LAMP  SYN     R$LAMP
 LITER SYN     F$LITER
 LOGOR SYN     H$LOGOR
  MAX  SYN     Q$MAX
 MAP   SYN     MAPCAR
 MIN   SYN     Q$MIN
 MKNAM SYN     F$MKNAM
 MKNO  SYN     F$MKNO
 MULT  SYN     Q$MIN
 NCONC SYN     R$NCONC
 NOTS  SYN     R$NOTS
 NULLP SYN     R$NULLP
 NUMBR SYN     F$NUMBR
 NUMOB SYN     F$NUMOB
 NUTRN SYN     T$NUTRN
 ONEP  SYN     Q$ONEP
 OVBGN SYN     S$OVBGN            OVERLORD BEGINNING
 PACK  SYN     F$PACK
 PAIR  SYN     A$PAIR
 POWR  SYN     G$POWR
 PRIN0 SYN     T$PRIN0
 PRIN1 SYN     T$PRIN1
 PRIN2 SYN     T$PRIN2
 PRINT SYN     T$PRINT
 PROP  SYN     R$PROP
 PUN2  SYN     T$PUN2
 PUNCH SYN     T$PUNCH
 RD    SYN     I$RD
 READ  SYN     I$READ
 READ1 SYN     I$READ1
 SETP  SYN     R$SETP
 SETQP SYN     R$SETQP
 SETUP SYN     E$SETUP
 SUB1  SYN     Q$SUB1
 SUBST SYN     R$SUBST
 UNFIX SYN     Q$UNFIX
 VALUE SYN I$VALUE                                                      PAGE 223
 ZEROP SYN     Q$ZEROP
APSSOC SYN SASSOC
       DECK                       BUTCH REGION AND END
       HEAD    0
*
*      BUTCH, A HOME FOR PATCHES
*
 BUTCH
BUTCHL EQU     NILSXX-*+1         LENGTH OF BUTCH REGION
       REM
       TCD     LOADER             GO TO RW TML FOR OCTAL CORRECTION CDS
*
*              THE FOLLOWNG PRODUCE A ROW BINARY TRASNFER CARD TO CONTIN
*
       FUL
       ORG     0
 AAAAA EQU     CONTIN
 BBBBB EQU     AAAAA-AAAAA/2*2
 CCCCC EQU     AAAAA/2-AAAAA/4*2
 DDDDD EQU     AAAAA/4-AAAAA/8*2
 EEEEE EQU     AAAAA/8-AAAAA/16*2
 FFFFF EQU     AAAAA/16-AAAAA/32*2
 GGGGG EQU     AAAAA/32-AAAAA/64*2
 HHHHH EQU     AAAAA/64-AAAAA/128*2
 IIIII EQU     AAAAA/128-AAAAA/256*2
 JJJJJ EQU     AAAAA/256-AAAAA/512*2
       OCT     0,0,0,0,0,0,0,0,0
       PZE     HHHHH+4096*IIIII,,64*JJJJJ
       PZE     EEEEE+4096*FFFFF,,64*GGGGG
       PZE     BBBBB+4096*CCCCC,,64*DDDDD
* Local Variables:
* mode: asm
* eval: (when (fboundp 'asm7090) (asm7090))
* End:
ENDEND END     0
