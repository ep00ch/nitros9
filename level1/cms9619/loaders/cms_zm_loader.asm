* cms_zm_loader.asm
* Copyright (c) 2020 Eric S. Pooch
* Released under MIT License:
* Permission is hereby granted, free of charge, to any person obtaining a copy of
* this software and associated documentation files (the "Software"), to deal in
* the Software without restriction, including without limitation the rights to
* use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
* the Software, and to permit persons to whom the Software is furnished to do so,
* subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
* FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
* COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
* IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
* CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

********************************************************************
* Zmodem receive protocol for CMS 9609/19 SBCs.
* This is the basis of a Zmodem receive command and virtual drive.
*
* Memory Map:
* $xx00         Sector buffer
* $x(x+1)00     I/O buffer
* $x(x+1)10     end   user stack
* $x(x+1)20     start user stack
ZPAD    EQU     '*'             * $2A/052 Padding character begins frames
ZDLE    EQU     @30             * Ctrl-X Zmodem escape - `ala BISYNC DLE
ZDLEE   EQU     (ZDLE^0100)     * Escaped ZDLE as transmitted
ZBIN    EQU     'A'             * Binary frame indicator (CRC-16)
ZDBIN    EQU    $01             * Binary frame indicator (CRC-16)
ZHEX    EQU     'B'             * HEX frame indicator
ZDHEX    EQU    $02             * HEX frame indicator
ZBIN32  EQU     'C'             * Binary frame with 32 bit FCS
ZBINR32 EQU     'D'             * RLE packed Binary frame with 32 bit FCS
ZVBIN   EQU     'a'             * Binary frame indicator (CRC-16)
ZVHEX   EQU     'b'             * HEX frame indicator
ZVBIN32 EQU     'c'             * Binary frame with 32 bit FCS
ZVBINR32 EQU    'd'             * RLE packed Binary frame with 32 bit FCS
ZRESC   EQU     @176            * RLE flag/escape character
ZMAXHL  EQU     16              * Max header information length  NEVER CHANGE
ZMAXSPL EQU     1024            * Max subpacket length  NEVER CHANGE

* Frame types (see array "frametypes" in zm.c)
ZRQINIT EQU     $00               * Request receive init
ZRINIT  EQU     $01               * Receive init
ZSINIT  EQU     $02               * Send init sequence (optional)
ZACK    EQU     $03               * ACK to above
ZFILE   EQU     $04               * File name from sender
ZSKIP   EQU     $05               * To sender: skip this file
ZNAK    EQU     $06               * Last packet was garbled
ZABORT  EQU     $07               * Abort batch transfers
ZFIN    EQU     $08               * Finish session
ZRPOS   EQU     $09               * Resume data trans at this position
ZDATA   EQU     $0a              * Data packet(s) follow
ZEOF    EQU     $0b              * End of file
ZFERR   EQU     $0c              * Fatal Read or Write error Detected
ZCRC    EQU     $0d              * Request for file CRC and response
ZCHAL   EQU     $0e              * Receiver's Challenge
ZCOMPL  EQU     $0f              * Request is complete
ZCAN    EQU     $10              * Other end canned session with CAN*5
ZFREE   EQU     $11              * Request for free bytes on filesystem
ZCMD    EQU     $12              * Command from sending program
ZSTDERR EQU     $13              * Output to standard error, data follows
ZCNT    EQU     $14              * Count of commands. 1 more than last valid.

* ZDLE sequences
ZCRCE   EQU     'h'             * CRC next, frame ends, header packet follows
ZCRCG   EQU     'i'             * CRC next, frame continues nonstop
ZCRCQ   EQU     'j'             * CRC next, frame continues, ZACK expected
ZCRCW   EQU     'k'             * CRC next, ZACK expected, end of frame
ZRUB0   EQU     'l'             * Translate to rubout 0177
ZRUB1   EQU     'm'             * Translate to rubout 0377

* Bit Masks for ZRINIT flags byze ZF1 */
ZF1_CANVHDR     EQU $01 * Variable headers OK, unused in lrzsz
ZF1_TIMESYNC    EQU $02 * nonstandard, Receiver request timesync

* Parameters for ZFILE frame
ZXSPARS EQU     64              * Encoding for sparse file operations

* ZDLREAD Breg return values (internal)
READERR EQU     -1
TIMEOUT EQU     -2
GOTCRCX EQU     $60

ZF3     EQU     %00000000
ZF0     EQU     %00000000

        use     defsfile

        ORG     $0000
* HEADER BYTE LOCATIONS, OFFSET FROM U POINTER
ZFORM   RMB     1
ZTYPE   RMB     1
ZF3P0   RMB     1
ZF2P1   RMB     1
ZF1P2   RMB     1
ZF0P3   RMB     1       * CAN ALSO BE THE ZCRCX TYPE LOC FOR DATA FRAMES
ZCRC1   RMB     1
ZCRC2   RMB     1
ZHCRLF  RMB     2       * CAN ALSO INDICATE THE FRAME END ENDING FOR DATA FRAMES
ZNULL   RMB     1
ZTEMP   RMB     2

ZCRCX   EQU     ZF0P3
ZFEND   EQU     ZHCRLF

        ORG     $9000
*        ORG     $D000

*********************************************************************
Z_JMPS
* Table of branches that will not change as code is added.
	BRA	RZM
        NOP
        NOP
        BRA     Z_MODER
        NOP
        NOP
        LBRA    Z_MODES
        NOP
        LBRA    CZM
        NOP
        LBRA    Z_LPR
        NOP
        LBRA    Z_LOAD
        RTS

*********************************************************************
RZM
* Receive ZMODEM command for DEBUG09/19
* TAKES 2 PARAMETERS FROM DEBUG COMMAND LINE:
*       4 DIGIT HEX BLOCK NUMBER   (U)
*       4 DIGIT HEX BUFFER LOCATION (D)
*
        JSR     CHKEOL
        BEQ     RZMERR          * NO ARGS ERR
        JSR     BEGEND          * READ IN PARAMETERS
        BCS     RZMERR          * BAD ARGS

        PSHS    X,Y,DP          * STORE DIRECT PAGE

        TFR     U,X             * PUT BLOCK NUM INTO X FOR Z_MODEM
        TFR     A,DP            * SECOND ARG IS BLOCK BUFF DP
        INCA                    * HEADER BUFF IS BLOCK BUFF PAGE+1
        TFR     D,U             * SET U TO HEADER BUFFER
        LDD     #$0100          * A=HI BLOCK SIZE, B=HI BITS BLOCK NUM
        LBSR    Z_MODER         * ENTER THE RECEIVE MAIN LOOP

        PULS    PC,DP,X,Y          * RESTORE THE CALLER'S DP, RETURN
RZMERR  JMP     RPT2ERR



*********************************************************************
ZRTYPE_TBL
* TABLE OF RESPONSE FRAME TYPES, INDEXED TO SENDER FRAME TYPES
* ALLOWS A RESPONSE BASED ON RECEIVED FRAME TYPE
*
        FCB     ZRINIT          * ZRQINIT RESPONSE
        FCB     ZABORT          * ZRINIT  RESPONSE
        FCB     ZACK            * ZSINIT  RESPONSE
        FCB     ZRINIT          * ZACK    RESPONSE
        FCB     ZRPOS           * ZFILE   RESPONSE
        FCB     ZABORT          * ZSKIP   RESPONSE
        FCB     ZRINIT          * ZNAK    RESPONSE
        FCB     ZRINIT          * ZABORT  RESPONSE
        FCB     ZFIN            * ZFIN    RESPONSE
        FCB     ZABORT          * ZRPOS   RESPONSE
        FCB     ZSKIP           * ZDATA   RESPONSE
        FCB     ZSKIP           * ZEOF    RESPONSE
        FCB     ZRINIT          * ZFERR   RESPONSE
        FCB     ZACK            * ZCRC    RESPONSE
        FCB     ZACK            * ZCHAL   RESPONSE
        FCB     ZCOMPL          * ZCOMPL  RESPONSE
        FCB     ZACK            * ZCAN    RESPONSE
        FCB     ZACK            * ZFREE   RESPONSE
        FCB     ZABORT          * ZCMD    RESPONSE
        FCB     ZACK            * ZSTDERR RESPONSE
ZRTYPE_END    FDB     $0000

*********************************************************************
Z_MODER
* Receive ZMODEM main loop, header processor
* in  DP=SECTOR BUFFER LOC
* in  U =HEADER BUFFER LOC
* in  A =NUM PAGES/SECTOR (1=256 bytes)
* in  B =HI PAGE OFFSET IN FILE
* in  X =PAGE OFFSET IN FILE (SECTOR)
* This allows MAX 2^24 256 byte sectors = 2^32 bytes = ~4GB total
*
* STORE PARAMS IN STACK IN A WAY THAT PROVIDES EASY CONVERSION TO ZM FLAG/POS
        PSHS    A
        CLRA
        PSHS    A
        PSHS    B             * SAVE FILE OFFSET/SECTOR
        TFR     X,D
        PSHS    A
        PSHS    B
        CLRA
        PSHS    A
* STACK: A, $00, B, XHI, XLO, $00 |<TOP

        LDA     #ZRINIT         * 8.1 Session Startup SEQUENCE
        BRA     Z_LKUP1         * SEND THE INIT

Z_READ  LBSR    Z_RHDR          * GET SENDER HEADER
        BCS     Z_MODE          * IF ERROR, EXIT NOW WITH ERR

        CMPA    #ZFILE          * FILE HEADER RESPONSE?
        BEQ     Z_DATAT         * READ THE FILE INFO INTO THE DATA BUFFER
        CMPA    #ZDATA          * DATA HEADER RESPONSE?
        BNE     Z_LKUP          * IF NOT, SKIP READ THE FILE DATA INTO THE DATA BUFFER

Z_DATAT PSHS    A
        TFR     DP,A            * GET THE SECTOR BUFFER IN DP
        CLRB
        TFR     D,X             * X POINTS TO START OF SECTOR BUFF

        LBSR    Z_RDATA         * LOAD THE DATA PACKET
        PULS    A

Z_DATA2 CMPB    #ZCRCE          * SENDER SENDING ANOTHER HEADER?
        BEQ     Z_READ          * IF SO, GET ANOTHER FRAME FROM SENDER

        CMPB    #ZCRCG          * SENDER IS IGNORING OUR RX BUFFER?
        BNE     Z_LKUP          * IF NOT, LOOK UP FRAME TO SEND
        LBSR    Z_CRCG          * CLEANUP lrzsz's MESS.

Z_LKUP  LEAY    ZRTYPE_TBL,PCR  * START OF THE RESPONSE HEADER TYPE TABLE
        LDA     A,Y             * LOOKUP RESPONSE TYPE FOR SENDER HEADER TYPE

        BEQ     Z_MODE          * IF NULL, EXIT NOW WITH ERR

Z_LKUP1 LDX     #$0000          * DEFAULT NO F0/F1 FOR NEXT SHDR
        LDY     #$0000          * DEFAULT NO F2/F3 FOR NEXT SHDR

        CMPA    #ZRINIT         * INIT RESPONSE?
        BNE     Z_LKUP2         * IF NOT, KEEP GOING
        LDX     $04,S           * IF INIT, SPECIFY BYTE BUFFER (SECTOR SIZE)

Z_LKUP2 CMPA    #ZRPOS          * POSITION RESPONSE?
        BNE     Z_SEND          * IF NOT, SEND IT
        LDX     ,S              * IF SO, RESTORE THE OFFSET LO  INTO X
        LDY     $02,S           * AND,   RESTORE THE OFFSET HIs INTO Y

Z_SEND  BSR     Z_SHDR           * SEND RECEIVER HEADER

        CMPA    #ZFIN           * SENT FIN?
        LBNE     Z_READ          * IF NOT, CONTINUE READING HEADERS

        BSR     Z_MOOR          * READ IN FINAL "OO"
        CLRB                    * CLEAR CARRY, IGNORE TIMEOUT
Z_MOD2
* IF LSZ GETS ANY INPUT NOW BEFORE IT QUITS (INCLUDING THE PROMPT), IT CAN HANG.
* SO, WAIT FOR A BIT TO EXIT.
        LDD     #$8000
DLY25M2 SUBD    #$0001
        BNE     DLY25M2

        PULS    PC,Y,X,D           * CLEAN UP STACK, RETURN

Z_MODE  ORCC    #$01            * INDICATE AN ERROR
        BRA     Z_MOD2

*********************************************************************
Z_MOOR
* READ ENDING "OO" WITH TIMEOUT
* out CC=CARRY IS ERROR
*
        LDX     ACVECT          * GET ACIA ADDRESS
        BSR     Z_MOO1          * GET FIRST 'O'
        RTS

        * FALL THROUGH TO GET SECOND 'O'
Z_MOO1  LDY     #$FFFF          * SET LONG TIMEOUT
Z_MOO2  LEAY    -1,Y            * DECREMENT THE TIMEOUT COUNTER
        BEQ     Z_MOOE          * TIMED-OUT
        LDA     $01,X           * READ ACIA STATUS REGISTER
        BITA    #$08            * MASK RDR FULL BIT
        BEQ     Z_MOO2          * WAIT TIL FULL
        LDA     ,X              * GET CHAR
        CMPA    #'O'            * 'O'?
        BNE     Z_MOO2          * IF NOT, KEEP READING
Z_MOOD  RTS

Z_MOOE  ORCC #$01               * INDICATE TIMEOUT ERROR
        BRA  Z_MOOD             * AND FINISH UP

*********************************************************************
Z_SHDR
* Send ZMODEM header
* in  A= HEADER TYPE
* in  B= HEADER FORMAT
* in  X= F3/P0, F2/P1 FIELDS
* in  Y= F1/P2, F0/P3 FIELDS
*
* REGS  XH XL YH YL
* -------------------
* TYPE  F3 F2 F1 F0
* -------------------
* TYPE  P0 P1 P2 P3
*
        CMPB    #ZBIN           * UNLESS ZBIN SPECIFIED,
        BEQ     Z_SHD1
        LDB     #ZHEX           * DEFAULT TO HEX

Z_SHD1  EXG     B,A             * MAYBE FIX INPUTS TO COMPLY. FIXME?
        STD     ZFORM,U

        STX     ZF3P0,U
        STY     ZF1P2,U

        LDA     #ZPAD           * PAD BYTE
        JSR     A1OUT           * SEND PAD
        JSR     A1OUT           * SEND PAD AGAIN

        LDA     #ZDLE           * ZDLE BYTE
        JSR     A1OUT           * SEND ZDLE

        LEAX    ZTYPE,U         * X POINTS TO START OF DATA TO CRC
        LEAY    ZCRC1,U         * Y POINTS TO END+1 OF DATA TO CRC
        BSR     Z_CRC16         * CALCULATE THE CRC
        LEAY    2,Y             * ADVANCE THE END TO PAST THE NEW CRC

        LDA     ZFORM,U
        JSR     A1OUT           * SEND HEADER FORMAT
* Fall through to Z_SENDB

********************************************************************
Z_SENDB
* SEND FROM BUFFER AT Xreg UP TO Yreg POINTER
* SEND IN FORMAT SPECIFIED IN Areg
* IF HEX, SEND WITH CRLF
* Z_SHDH and Z_SHDB could be merged, but IT WOULD CHECK FLAG ONCE PER BYTE
* OR USE A JUMP TABLE.
* in  A= HEADER FORMAT
* in  X= START ADDR OF SEND BUFFER
* in  Y= END+1 ADDR OF SEND BUFFER
*
        STY     ZTEMP,U         * STORE END ADDRESS IN THE TEMP SPOT
        CMPA    #ZBIN           * BINARY HEADER FORMAT?
        BEQ     Z_SHDB
*        CMPA    #ZHEX           * HEX HEADER FORMAT?
*        BEQ     Z_SHDH
*        BNE     Z_SENR          * UNKNOWN HEADER FORMAT

Z_SHDH  LDA     ,X+
        BSR     ZHEXSEND
        CMPX    ZTEMP,U         * AT END?
        BLT     Z_SHDH          * KEEP SENDING

        LDD     #$0D0A          * LOAD CRLF
        STD     ,Y++            * SAVE TO BUFFER
        STY     ZTEMP,U         * MOVE U TO NEW END OF BUFFER

* Fall through to SEND CRLF AS BINARY
Z_SHDB  LDA     ,X+
        BSR     ZDLBSEND
        CMPX    ZTEMP,U         * AT END?
        BLT     Z_SHDB          * IF NOT KEEP SENDING

Z_SENR  LDA     ZTYPE,U
        RTS                     * RETURN

*********************************************************************
Z_CRC16
* Calculate Z/XMODEM CRC-16 (CRC16-CCITT) from buffer
* and add it to the end of the buffer.
* Credit to J.G.Harston
* in  X = Pointer to start of buffer
* in  Y = Pointer to byte after end of buffer
*
* XMODEM polynomic
ZPOLYH  EQU     $10
ZPOLYL  EQU     $21

        LDD     #$0000          * Initial CRC
* Continue here if adding to CRC
Z_CRCC  PSHS    X               * Start address (direct page or extended)
        STY     ZTEMP,U         * STORE END ADDRESS IN THE TEMP SPOT
* Calculates the CRC with the byte in the operand of the eora statement
ZCRCB   EORA    ,X+             * Fetch byte and XOR into CRC high byte
        LDY     #8              * Rotate loop counter
ZCRCR   ASLB                    * Shift CRC left, first low
        ROLA                    * and than high byte
        BCC     ZCRCC           * Justify or ...
        eora    #ZPOLYH         * CRC=CRC XOR polynomic, high
        eorb    #ZPOLYL         * and low byte
ZCRCC
        LEAY    -1,Y            * dec Shift loop counter (8 bits)
        BNE     ZCRCR

        CMPX    ZTEMP,U         * AT END?
        BLO     ZCRCB

XCRCH   LDY     ZTEMP,U         * RESTORE Y END ADDRESS
*        STD     ,Y++            * ADD THE CRC TO THE END (FIX ME? NEEDED?)
        STD     ZCRC1,U         * ADD THE CRC TO CRC SPOT
        PULS    X               * RESTORE X CRC START ADDRESS
        RTS

********************************************************************
ZDLBSEND
* SEND Areg as ZDLE encoded binary
* This escapes ZDLE and @020-@023, @220-@223. Close enough.
* in  A= value in binary
* out A= value after  ZDLE encoding, if necessary
* out B= value before ZDLE encoding (same as A input)
*
        CMPA    #ZDLE
        BEQ     ZDLBS1          * ENCODE IT
        BITA    #%01101100      * ANY BUT $00-$03 and $10-$13? (ignore hi bit)
        BNE     ZDLBS3          * IF SO, SEND IT
        BITA    #%00010000      * $10-$13? (ignore hi bit)
        BEQ     ZDLBS3          * IF NOT SEND IT
* ONLY $10-$13 AND $80-$83 ARE LEFT, SO ENCODE THEM
ZDLBS1  TFR     A,B             * SAVE BYTE FOR LATER
        LDA     #ZDLE
        JSR     A1OUT           * SEND ZDLE
        TFR     B,A             * RESTORE BYTE
        EORA    #%01000000      * INVERT BIT 6
ZDLBS3  JSR     A1OUT           * SEND BYTE
        RTS

********************************************************************
ZHEXSEND
* SEND Areg as 2 DIGIT HEX
* in  A= value in binary
* out A= LO nibble HEX digit
* out B= same as A input
*
        TFR     A,B     * SAVE A COPY
        ANDA    #$F0    * GET HI NIBBLE
        LSRA            * MOVE TO LO
        LSRA
        LSRA
        LSRA
        BSR     ZHEXCHR * CONVERT HI NIB TO HEX DIGIT AND SEND
        TFR     B,A     * RESTORE BYTE
        ANDA    #$0F    * GET LO NIBBLE
* FALL THROUGH TO CONVERT LO NIB TO HEX DIGIT
ZHEXCHR CMPA    #$09
        BLS     ZHEXCH2
        ADDA    #$27    * CONVERT TO LOWER CASE HEX
ZHEXCH2 ADDA    #$30    * CONVERT TO ASCII
        JSR     A1OUT   * SEND THE HEX NIBBLE
        RTS

*********************************************************************
Z_RDATA
* RECEIVE/SAVE THE DATA IN THE PACKET
* in  X=location to store data
* out B=CRC type

Z_RDAT1 BSR     ZDLREAD         * READ NEXT DECODED BYTE FROM SENDER
        CMPB    #GOTCRCX        * CRC FLAG?
        BEQ     Z_RDAT2         * IF SO, FINISH UP
        STA     ,X+             * SAVE THE DECODED DATA BYTE
*        TFR     X,B
*        CMPB    #$00            * ABOUT TO BUFFER OVERFLOW?
*        BEQ     ZBADLSZ
        BRA     Z_RDAT1         * KEEP READING

Z_RDAT2 PSHS    A               * SAVE CRC INFO
Z_RDAT3 BSR     ZDLREAD         * READ/IGNORE CRC1
        BSR     ZDLREAD         * READ/IGNORE CRC2
        PULS    B               * RESTORE CRC INFO INTO TO B

Z_RDAR  RTS

* lrzsz ignores our buffer size and will try to overflow it.
* If that is the case, stop at 256 bytes and indicate more data available.
*ZBADLSZ LDB     #ZCRCG
*        BRA     Z_RDAR

********************************************************************
ZDLREAD
* RECEIVE TO Areg ZDLE DECODED
* out A= value after ZDLE decoding, if necessary
* out B= return codes
*       Bad  escape sequence:   $20
*       Good escape sequence:   $40
*       GOT CRC (type in Areg): $60
*       No Escape needed:       ACIA Status Reg
*
        CLRA
ZDLRE0  TFR     A,B             * COPY ZDLE TO B
        BSR     A1RCV           * GET A BYTE
        CMPA    #ZDLE           * IS CURRENT ZDLE?
        BEQ     ZDLRE0          * IF SO, GET NEXT
        CMPB    #ZDLE           * WAS PREV BYTE ZDLE?
        BNE     ZDLRE1          * IF NOT, RETURN IT
        TFR     A,B             * SAVE A COPY FOR BIT CHECKS/RETURN CODES
        ANDB    #$60            * CHECK BIT 5,6
        CMPB    #$40            * ONLY BIT 6 IS HI?
        BNE     ZDLRE1          * IF NOT, DON'T MODIFY (CRC)
        EORA    #$40            * CLEAR BIT 6 (UN-ESCAPE)
ZDLRE1  RTS

********************************************************************
A1RCV
* RECEIVE CHARACTER INTO Areg
* out A= value in binary
*
        PSHS    X               * SAVE XREG
        LDX     ACVECT          * GET ACIA ADDRESS
A1RCV2  LDA     $01,X           * READ ACIA STATUS REGISTER
        BITA    #$08            * MASK RDR FULL BIT
        BEQ     A1RCV2          * WAIT TIL FULL
        LDA     ,X              * GET CHAR
        PULS    PC,X            * RESTORE XREG, RETURN

*        JMP     #$FC5E            *A0INPCH2 RETURNS FOR US

*********************************************************************
Z_RHDR
* Receive ZMODEM header
* out A= RECEIVED HEADER TYPE
* out CC=CARRY IS ERROR
*
        BSR     A1RCV           * GET BYTE FROM ACIA
        CMPA    #$1B            * USER TRYING TO ESC?
        BEQ     Z_RHDE2         * IF SO, RETURN NOW WITH ERROR (ESC IN A)

Z_RHD0  CMPA    #ZPAD
        BNE     Z_RHDR          * SKIP PADDING - NEED TIMEOUT (?)
Z_RHD1  BSR     A1RCV
        CMPA    #ZDLE
        BNE     Z_RHD1          * SKIP UNTIL ZDLE - NEED TIMEOUT (?)

        BSR     ZDLREAD
        STA     ZFORM,U         * HEADER FORMAT

        CMPA    #ZBIN           * 16 BIT CRC BINARY HEADER FORMAT?
        BEQ     Z_RHDB
        CMPA    #ZHEX           * HEX HEADER FORMAT?
        BEQ     Z_RHDH

        BRA     Z_RHDE          * UNKNOWN FORMAT RETURN NOW WITH ERROR

*Unrolled loops
Z_RHDB  BSR     ZDLREAD
        STA     ZTYPE,U         * HEADER TYPE
        BSR     ZDLREAD
        STA     ZF3P0,U
        BSR     ZDLREAD
        STA     ZF2P1,U
        BSR     ZDLREAD
        STA     ZF1P2,U
        BSR     ZDLREAD
        STA     ZF0P3,U
        BSR     ZDLREAD
        STA     ZCRC1,U
        BSR     ZDLREAD
        BRA     Z_RHD2          * DONE, SKIP OVER HEX READ

Z_RHDH  BSR     ZHEXREAD
        STA     ZTYPE,U         * HEADER TYPE
        BSR     ZHEXREAD
        STA     ZF3P0,U
        BSR     ZHEXREAD
        STA     ZF2P1,U
        BSR     ZHEXREAD
        STA     ZF1P2,U
        BSR     ZHEXREAD
        STA     ZF0P3,U
        BSR     ZHEXREAD
        STA     ZCRC1,U
        BSR     ZHEXREAD

Z_RHD2  STA     ZCRC2,U         * PUT LAST STA HERE TO SAVE A FEW BYTES

        LDA     ZTYPE,U         * GET THE TYPE BACK
        CMPA    #ZCNT           * VALID TYPE?
        BCC     Z_RHDE          * IF NOT, EXIT WITH ERR

        ANDCC   #$FE            * CLEAR THE CARRY THAT CMPA JUST SET
Z_RHDX  RTS

Z_RHDE  LDA     #ZFERR          * UNKNOWN FORMAT FATAL ERROR
        LBSR    Z_SHDR          * SEND ERROR TO SENDER
Z_RHDE2 ORCC    #$01            * SET CARRY TO INDICATE ERROR
        BRA     Z_RHDX          * EXIT

********************************************************************
ZHEXREAD
* RECEIVE Areg AS 2 DIGIT HEX
* out A= value in binary
* out B= byte  as binary, if not HEX
*
        BSR     ZhexDEC         * GET HEX HI DIGIT AS BIN
        ASLA
        ASLA
        ASLA
        ASLA
        TFR     A,B             * SAVE HI NIBBLE
        BSR     ZhexDEC         * GET HEX LO DIGIT AS BIN
        PSHS    B               *
        ADDA    ,S+             * ADD NEW DIGIT TO SUM, PULL B
        RTS

ZhexDEC  LBSR    A1RCV          * READ THE BYTE
        CMPA    #'a'            * LOWER CASE LETTER?
        BLO     hexDER          * IF LOWER, CONTINUE
        SUBA    #$20            * CONVERT TO UPPER CASE HEX
hexDER  JSR     HEXDEC          * CONVERT TO BIN
        RTS

********************************************************************
Z_CRCG
* STOP SENDER FROM FULL STREAMING AND DRAIN INPUT.
* This is needed due to major bugs in lrzsz 0.12.20, which is the standard.
* It does not comply with section 9.5 of the ZMODEM protocol document.
* Even worse, it quickly ignores the specifed Rx buffer size.
* out A= ZDATA frame type
*
        PSHS    A               * SINCE WE SENT A RX BUFFER SIZE,
        LDA     #'J'            * WE SHOULDN'T GET ZCRCG PER SPEC 9.5.
        JSR     A1OUT           * BUT lrsz HAS _MAJOR_ BUG. WHAT CAN YOU DO?
        JSR     A1OUT           * OTHER METHODS CORE-DUMP LSZ TO RECEIVER!
        JSR     A1OUT           * SEND ENOUGH JUNK TO GET A ZCRCW
        JSR     A1OUT

Z_CRCG2 LBSR    ZDLREAD         * GET THE NEXT DATA BYTE.
        CMPB    #GOTCRCX        * GET A CRC?
        BNE     Z_CRCG2         * IF NOT, KEEP DRAINING PENDING DATA BYTES.
        CMPA    #ZCRCW          * GET A ZCRCW?
        BNE     Z_CRCG2         * IF NOT, KEEP DRAINING PENDING DATA FRAMES.
        PULS    A               * SHOULD BE #ZDATA
        RTS

        IFNE    1       * IGNORE ALL OF THIS FOR NOW
********************************************************************
Z_SDATA
* SEND THE DATA PACKET FROM INDICATED BUFFER.
* USE THE HEADER BYTE LOCATIONS TO FINISH UP THE FRAME.
* in  A =ZCRC TYPE
* in  X =Pointer to buffer with data to send
* in  Y =Pointer to byte after end of buffer

        STA     ZCRCX,U         * SET UP END OF FRAME ZCRCX

        LBSR    Z_CRC16         * CALC THE CRC INTO ZCRC1 LOC

        LDA     #ZBIN           * INDICATE SEND BINARY
        LBSR    Z_SENDB         * SEND THE DATA

        LDA     #ZDLE           * SEND THE ZDLE FOR THE ZCRCX
        LBSR    A1OUT

        LEAX    ZCRCX,U         * START AT THE ZCRCX TYPE
        LEAY    1,X             * END JUST PAST THE CRC TYPE
        LDD     ZCRC1,U         * LOAD CRC STARTING VALUE
        LBSR    Z_CRCC          * CONTINUE TO CRC INTO ZCRC1 LOC

        LDA     #ZBIN           * INDICATE SEND BINARY
        LEAY    ZFEND,U         * END+1 TO SEND

        LBSR    Z_SENDB         * SEND THE CLOSE OF THE DATA FRAME.

*        LDA     ZCRC1,U
*        LBSR    ZHEXSEND
*        LDA     ZCRC2,U
*        LBSR    ZHEXSEND

        RTS

CZM
        PSHS    DP,U,D
        LDU     #$9E80
        LDA     #$9E
        TFR     A,DP
        JSR     Z_CLK
        PULS    PC,DP,U,D


********************************************************************
Z_CLK
Z_LPR
Z_MODES
* Send date and time from unixy receiver via ZMODEM tricks.
* in  DP=SECTOR BUFFER LOC
* in  U =HEADER BUFFER LOC
*
        *PSHS    U,X,DP          * CPYSTR WILL PSHS D
        LEAY    Z_DTHLP,PCR     * OUTPUT SOME INFO
        JSR     CPYSTR           * SEND STRING TO ACIA - ROM CPYSTR
        *PULS    U,X,DP

        LDA     #ZRQINIT        * 8.1 Session Startup SEQUENCE
        LDX     #$0000          * DEFAULT NO F2/F3 FOR NEXT SHDR
        LDY     #ZCMD
        LBSR    Z_SHDR          * SEND SENDER HEADER

        LBSR    Z_RHDR          * GET RECEIVER INIT HEADER

        LDA     #ZCMD           * 11.19  ZCOMMAND
        LDB     #ZHEX
        LDX     #$0000          * DEFAULT NO F2/F3 FOR NEXT SHDR
        LDY     #$0000          * DEFAULT NO F0/F1 FOR NEXT SHDR
        LBSR    Z_SHDR          * SEND SENDER HEADER

        LEAX    Z_DTCMD,PCR
        TFR     X,D
        ADDD    -2,X            * ADD IN THE LENGTH OF THE STRING
        TFR     D,Y             * PUT IT INTO Y, THE END
        LDA     #ZCRCW
        BSR     Z_SDATA

        LBSR    Z_RHDR          * GET RECEIVER HEADER
        CMPA    #ZRQINIT        * OTHER SIDE SENDING DATA?
        BNE     Z_MODS2
        LDD     #$0100          * A=HI BLOCK SIZE, B=HI BITS BLOCK NUM
        LDX     #$0000
        LBSR    Z_MODER         * RECEIVE FILE WITH TIME

* I think the delay at the end of Z_MODER is causing us not to get the ZCOMPL
        *LBSR    Z_RHDR          * GET RECEIVER HEADER
Z_MODS2 *CMPA    #ZCOMPL          * OTHER SIDE COMPLETE?
        LDA     #ZFIN           *
        LBSR    Z_SHDR          * SEND SENDER HEADER
        LBSR    Z_RHDR          * GET RECEIVER HEADER
        BSR     Z_MOOS

        TFR     DP,A
        CLRB
        TFR     D,X
        JSR     $E700           * SET THE CLOCK USING ROM ROUTINE

        RTS


********************************************************************
Z_MOOS
	LDA     #'O'
	JSR     A1OUT           * SEND "O"
	JSR     A1OUT           * SEND "O" AGAIN"
	RTS

********************************************************************
Z_STRINGS
* Strings used by ZMODEM
*
* ASK THE RECEIVER TO SEND US BACK A FILE WITH THE DATE STRING
* SURPRISINGLY, THIS APPEARS TO WORK ON lrz, if started with -U -C
Z_DTHLP FCC     `Getting Date. Start: rz -C -U`
        FCB     $0D,$0A,$00

        FCB     $00,Z_DTEND-Z_DTCMD
Z_DTCMD FCN     `date "+%y%m%d %H%M%S">/tmp/date.txt;sz -y /tmp/date.txt`
Z_DTEND EQU     .

********************************************************************
Z_LOAD
* Get sectors $264-$273 to memory at 2600-35xx
*
        PSHS    X,Y,DP          * STORE DIRECT PAGE

        LDX     #$0264          * START AT SECTOR $264
        LDD     #$2600          * SAVE IN MEMORY $2600
        TFR     A,DP            * BLOCK BUFFER PAGE IN DP
        INCA                    * HEADER BUFF IS BLOCK BUFF PAGE+1
        TFR     D,U             * SET U TO HEADER BUFFER
        LDD     #$0100          * A=HI BLOCK SIZE, B=HI BITS BLOCK NUM

ZLOADL  PSHS    X
        LBSR    Z_MODER         * ENTER THE RECEIVE MAIN LOOP
        PULS    X
        * NEXT BLOCK/BUFFER LOC
        LEAX    1,X             * NEXT BLOCK BUFF/SECTOR
        TFR     DP,A
        INCA                    * NEXT BLOCK BUFFER
        CMPA    #$36            * LAST BLOCK LOC IS $35
        BEQ     ZLOADE
        TFR     A,DP            * BLOCK BUFFER PAGE IN DP

        INCA                    * HEADER BUFF IS BLOCK BUFF PAGE+1
        CLRB
        TFR     D,U             * SET U TO HEADER BUFFER
        LDD     #$0100          * A=HI BLOCK SIZE, B=HI BITS BLOCK NUM
        BRA     ZLOADL
ZLOADE
        PULS    PC,DP,X,Y          * RESTORE THE CALLER'S DP, RETURN
