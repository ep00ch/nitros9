*******************************************************************
* CMSI - CMS Init Module
* This module should run immediately before krn.
* It can either be run in place of rel if booting from ROM,
* or just after rel if booting from disk.
* I/O init is usually handled by DEBUG19 ROM. However, if booting
* from NOS9 ROM or using ACIA 1 we need to init the I/O ourselves.
*
* Setup interrupt vectors, initialize I/O, and output startup message to ACIA.
*
* $Id$
*
* Edt/Rev  YYYY/MM/DD  Modified by
* Comment
* ------------------------------------------------------------------
*   1/0    2020/03/13  Eric S. Pooch
* Created.
*   1/1    2020/10/09  Eric S. Pooch
* Added features for ROM boot.
*
         nam   cmsi
         ttl   CMS Init Routines

         ifp1
         use   defsfile
         endc

        IFEQ  Level-1
ScStart  equ   $8000        screen start in memory
        ELSE
ScStart  equ   $8008        screen start in memory
        ENDC

tylg     set   Systm+Objct
atrv     set   ReEnt+rev
rev      set   $01
edition  set   1
       IFEQ     ROM-1
        fcc     /OS/        sync bytes. Probably not necessary.
        ORG     $0000
        bra     Start+2
    ENDC

         mod    eom,name,tylg,atrv,InitCMS9619,size

size     equ    .

name     fcs    /CmsI/
         fcb    edition

*--------------------------------------------------------------------------
InitCMS9619
* Jump table/main program.
*
Start
*        lbsr    InitVectors
*        lbsr    InitIO
*        lbsr    StartupMsg
*        lbra    JmpKrn

*--------------------------------------------------------------------------
InitVectors
* Setup CMS 9619 interrupt vectors in $9F page to point to
* NitrOS-9's vectors in the $01 page, if DEBUG19 installed.
*
        ldd     $FD1C               check if DEBUG19 ROM installed
        cmpd    #$4445              first 2 letters of "DEBUG19"
        bne     init1               if not, skip vector changes

        ldx     #$0100
        ldb     #$03
        stx     RAM_SW3_VEC
        abx
        stx     RAM_SW2_VEC
        abx
        stx     RAM_SWI_VEC
        abx
        stx     RAM_NMI_VEC
        abx
        stx     RAM_IRQ_VEC
        abx
        stx     RAM_FRQ_VEC
init1
        lds     #$1fff      set stack to the end of the block
        ldd     #$00ff
        stb     ,-s         save status of start, $00=cold, $01=warm, $FF = startup: do complete boot (above)
        tfr     a,dp        Set the DP to $00

*--------------------------------------------------------------------------
InitIO
* Initialize ACIAs based on switch 1 settings.
* Could skip most of this if DEBUG19 already did it.
*
* setup PIA0
        ldy     #PIA0Base
        clr     $01,y               CLEAR PIA0 CRA
        clr     ,y                  CLEAR PIA0 DDRA REG
        ldb     #%00111100
        stb     $01,y               SET PIA0 CRA -IRQ, +POR, +CA2
* get baud rate settings
        lda     #%00001011          DTR LOW, RTS LOW, -IRQ, NO PARITY
* configure settings for ACIA 0
        ldb     ,y                  GET PIA0 PORA (BAUD RATE SWITCHES)
        ldx     #ACIA0Base          GET ACIA0 ADDRESS
        lsrb
        lsrb
        lsrb
        lsrb                        GET SWITCHES HI NIBBLE
        orb     #%00010000          USE INTERNAL BAUD RATE GEN
        std     $02,x               SET ACIA0 COMMAND & CONTROL REG
* configure settings for ACIA 1
        ldb     ,y                  GET PIA0 PORA (BAUD RATE SWITCHES)
        ldx     #ACIA1Base          GET ACIA1 ADDRESS
        andb    #$0f                GET SWITCHES LO NIBBLE
        orb     #%00010000          USE INTERNAL BAUD RATE GEN
        std     $02,x               SET ACIA1 COMMAND & CONTROL REG
* configure PTM
       ldx     #PTMBase            GET TIMER ADDRESS
       lda     #$43                TIMER INIT VALUE
       sta     $01,X               INIT TIMER
*       ldd     #$000C              GET TIMEOUT VALUE
*       std     $04,X


*--------------------------------------------------------------------------
StartupMsg
* Output the startup message to ACIA 1.
*
    IFEQ  ROM-1
* Use our message
        leay    <BootMsg,pcr
    ELSE
* Use rel's message
        ldy     #ScStart+$100   Screen buffer Msg Line
        *leax    <BootEnd,pcr
        ldd     <BootEnd,pcr         CRLF
        std     $1c,y           add it to the message
        ldd     <BootEnd+2,pcr       NULLs
        std     $1e,y           End the line with NULLs at last column
    ENDC
        ldx     #ACIA1Base
smsg2
        lda     ,y+             load next char in string
        beq     JmpKrn          end if NULL
        cmpa    #$60
        blo     smsg3
        anda    #%00111111      clear high bits to make it upper ASCII.
smsg3
        ldb     $01,x           get ACIA status
        andb    #$10            busy?
        beq     smsg3           if so, try again
        sta     ,x              otherwise, send A reg
        bra     smsg2           check next char


*--------------------------------------------------------------------------
JmpKrn
* Compute the absolute address of the entry point
* of the next module (which should be krn) and go there.
*
        leax    <eom,pcr
        ldd     M$Exec,x
        jmp     d,x
        bra     StartupMsg      in case of error, keep looping

*--------------------------------------------------------------------------
    IFEQ  ROM-1
BootMsg
* Bootup Message
        fcc   /NITROS9 ROM BOOT/
    ENDC
BootEnd
        fcb   $0D,$0A,$00,$00

        emod
eom     equ   *
        end
