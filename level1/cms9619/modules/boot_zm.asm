********************************************************************
* Boot - CMS 9619 ZMODEM Boot module
* Provides HWInit, HWTerm, HWRead which are called by code in
* "use"d boot_common.asm
*
* $Id$
*
* Edt/Rev  YYYY/MM/DD  Modified by
* Comment
* ------------------------------------------------------------------
*   1      2020/02/15  Eric Pooch
* Created.

               nam   Boot
               ttl   CMS 9619 ZMODEM Boot Module

        IFP1
               use  defsfile
        ENDC

tylg           set       Systm+Objct
atrv           set       ReEnt+rev
rev            set       $01
edition        set       1

               mod       eom,name,tylg,atrv,start,size

* Default Boot is from ACIA 1
BootDr  set    1

* Alternate Boot is from ACIA 0
   IFEQ        DNum-1
BootDr  set    1
   ENDC

               use     llzm.asm

               org     0

* NOTE: these are U-stack offsets, not DP
seglist        rmb       2                  pointer to segment list
blockloc       rmb       2                  pointer to memory requested
blockimg       rmb       2                  duplicate of the above
bootsize       rmb       2                  size in bytes
LSN0Ptr        rmb       2                  In memory LSN0 pointer

aciaddr        rmb       2                  acia address store
zmvars         rmb       14             * ZMODEM buffer/variables
size           equ       .

name           fcs       /Boot/
               fcb       edition

* Common booter-required defines
LSN24BIT       equ       1
FLOPPY         equ       0


*--------------------------------------------------------------------------
HWInit
* Initialize the device
*
*    Entry:
*       Y  = hardware address
*    Exit:
*       Carry Clear = OK, Set = Error
*       B  = error (Carry Set)
*
          * reserve $9800-$9F00 area with reset vectors, etc.
          ldd     #$00ff
          std     $212

          ldx     ACVECT          * get the debugger's ACIA vector
          stx     aciaddr,u       * store it for later
*         sty     ACVECT          * save our ACIA vector in its place
          clrb                    * clear carry, no error
          rts

*--------------------------------------------------------------------------
HWTerm
* Terminate the device
*
*    Entry:
*       Y  = hardware address
*
*    Exit:
*       Carry Clear = OK, Set = Error
*       B = error (Carry Set)
*
        ldx     aciaddr,u
*        stx     ACVECT          * restore the debugger's ACIA vector
        clrb                    * clear carry, no error
        rts

***************************************************************************
     use  boot_common.asm
***************************************************************************
*
HWRead
* Read a 256 byte sector from the device
*
*    Entry:
*       Y  = hardware address
*       B  = bits 23-16 of LSN
*       X  = bits 15-0  of LSN
*       blockloc,u = where to load the 256 byte sector
*
*    Exit:
*       X = ptr to data (i.e. ptr in blockloc,u)
*       Carry Clear = OK, Set = Error
*
        pshs    dp,u
        lda     blockloc,u      * hope there was nothing important in LO byte.
        tfr     a,dp            * ROM Z_MODEM expects block buffer ptr in DP,
        leau    zmvars,u        * header buffer ptr in U,
        lda     #$01            * and num pages/sector in A. B,X,Y are same as entry.
        jsr     Z_MODEM         * use ROM'd ZMODEM function
        *jmp   [$FFFE]
        puls    u,dp
        ldx     blockloc,u      * restore sector loc, as req'd by boot_common.
ReadR   rts                     * return

               page

Address        fdb       ACIA1Base

               emod
eom            equ       *
EOMSize        equ       *-Address

               end
