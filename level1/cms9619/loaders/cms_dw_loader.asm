
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
* DriveWire 4 bootstrap for CMS 9609/19 SBCs.
* This is a small utility that gets loaded from DEBUG19 to bootstrap
* the kernel track using DriveWire.
*
        use     defsfile
        use     drivewire.d

DOSBUF  equ     $2600           * RAM LOAD LOCATION FOR THE DOS COMMAND
SECMAX  equ     18              * MAXIMUM NUMBER OF SECTORS PER TRACK

        ORG     $9E00

* CMS 9619 Init
        ldx     #ACIA0Base      * GET ACIA0 ADDRESS
        stx     ACVECT          * SET THE ACIA VECTOR
        LDB     #%00011111       INTERNAL 19200
        LDA     #%00001011       DTR LOW, RTS LOW, NO IRQ, NO PARITY
        STD     $02,X            SET ACIA COMMAND & CONTROL REG

*        * Load baud setting from DIP switch on PIA0
*        ldx     #PIA0Base       * GET ACIA0 ADDRESS
*        clr     $01,X           * CLEAR PIA0 CRA
*        clr     ,X              * CLEAR PIA0 DDRA REG
*        ldb     #%00111100      * -IRQ, +POR, CA2 OUTPUT HI ON WRITE TO CRA
*        stb     $01,X           * SET PIA0 CRA
*        ldb     ,X              * GET PIA0 PORA
*        andb    #$0F            * GET LO NIBBLE (SW1, JP2)
*        orb     #%00010000      * USE INTERNAL BAUD RATE GEN
*        lda     #%00001011      * DTR LOW, RTS LOW, NO IRQ, NO PARITY
*        std     $02,X           * SET ACIA0 COMMAND & CONTROL REG

* DW Init
        lda     #OP_INIT
        jsr     A1OUT           * Send DW init command (no response)

* Read sector 0
        ldx     #DOSBUF         * start address to save to
        lda     #$00

        pshs    x
        ldd     #$0000          * Get disk description at LSN 0
        bsr     DW_READ         * Send DW read command

        leax    ,s              * restore start of track location
*        ldb     DD.TKS,x        * get sectors/track
*        clra
*        tfr     d,y             * y is sector counter to load track
*        ldy     SECMAX          * y is sector counter to load track

*        lda     #34             * want track 34
*        mul                     * d is boot LSN < Not working
        ldd     #$0264          * START AT SECTOR $264
        puls    x               * restore start address to save kernel sector

* read kernel track
sec     bsr     DW_READ         * get next sector
        incb
        cmpb    #$77            * last sector?
        blt     sec             * continue until have complete track

* restore ACIA1
dw_rest
        ldx     #ACIA1Base      * GET ACIA1 ADDRESS
        stx     ACVECT          * RESET THE ACIA VECTOR

* start
        LDD     DOSBUF          * GET FIRST TWO BYTES OF RAM DATA
        CMPD    #$4F53           * #'OS   * LOOK FOR 'OS' (OS9) AT START OF BUFFER
        LBEQ    DOSBUF+2        * IF 'OS' THEN BRANCH TO DATA LOADED IN RAM
        RTS

********************************************************************
DW_READ
* save disk 0 sector from DW
* in  D= LSN lower 16 bits (HI bit is 0)
* in  X= location to store data
* out X= next address after the stored sector (entry+$100)
*
        pshs    d
        lda     #OP_READ
        jsr     A1OUT           * send DW Read command
        clra
        jsr     A1OUT           * drive 0
        jsr     A1OUT           * LSN Bits 23-16 = 0
        ldd     ,s              * restore LSN
        jsr     A1OUT           * LSN Bits 15-8
        tfr     b,a
        jsr     A1OUT           * LSN Bits  7-0
* check error code
        bsr     A1RCV           * get error, if any
        bne     DW_ERR          * if error, handle it
* ignore checksum
        bsr     A1RCV
        bsr     A1RCV
* save sector to memory
        ldb     #$ff             * init counter
* loop
dwdata    bsr     A1RCV           * get next data byte
        sta     ,x+
        decb                    * decrement the counter
        bne     dwdata
        bsr     A1RCV           * get byte 0
        sta     ,x+
DW_ERR
        puls    d,pc


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
A1Exit
        PULS    X,PC            * RESTORE XREG, RETURN

* Time
