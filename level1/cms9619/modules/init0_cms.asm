*******************************************************************
* CMSI - CMS Init Module
* Setup interrupt vectors and output Relocation routine's message to ACIA.
*
* $Id$
*
* Edt/Rev  YYYY/MM/DD  Modified by
* Comment
* ------------------------------------------------------------------
*   1      2020/03/13  Eric S. Pooch
* Created.
*
         nam   cmsi
         ttl   CMS Init Routine

         ifp1
         use   defsfile
         endc

        IFEQ  Level-1
ScStart  equ   $8000      screen start in memory
        ELSE
ScStart  equ   $8008      screen start in memory
        ENDC

tylg     set   Systm+Objct
atrv     set   ReEnt+rev
rev      set   $00
edition  set   1

         mod   eom,name,tylg,atrv,InitCMS9619,size

size     equ   .

name     fcs    /CmsI/
         fcb    edition

*--------------------------------------------------------------------------
InitCMS9619
* Main program
*
        lbsr    InitInterrupts
        lbsr    OutputStartup
        lbra    JmpKrn

*--------------------------------------------------------------------------
InitInterrupts
* Setup CMS 9619 interrupt vectors in $9F page to point to
* NitrOS-9's vectors in the $01 page.
*
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

        clrd
        tfr     a,dp    * Set the DP to $00
        rts

*--------------------------------------------------------------------------
OutputStartup
* Output the startup message that rel put in the screen buffer to 
* DEBUG19's output string to ACIA function.
*
        ldy     #ScStart+$100  Screen buffer Msg Line
        tfr     y,x
        ldd     #$0D0A
        std     $1c,x
        ldd     #$0000
        std     $1e,x          End the line with a NULL at last column

out1    lda     ,x+
        beq     out2
        cmpa    #$60
        blo     out1
        anda    #%00111111      clear high bits to make it upper ASCII.
        sta     -1,x            Save it over the top
        bne     out1            continue if not null

out2    jsr     WRTSTR         Write the line to ACIA
        rts

*--------------------------------------------------------------------------
JmpKrn
* Compute the absolute address of the entry point
* of the next module (which must be krn) and go there.
*
        leax    <eom,pcr
        ldd     M$Exec,x
        jmp     d,x

        emod
eom     equ   *
        end
