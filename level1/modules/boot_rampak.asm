********************************************************************
* Boot - Disto RAMPak Boot Module
*
* $Id$
*
* Edt/Rev  YYYY/MM/DD  Modified by
* Comment
* ------------------------------------------------------------------
*   5      ????/??/??  Alan DeKok
* Created.
*
*   6      1998/10/20  Boisy G. Pitre
* Fixed small bugs, improved speed.
*
*   7      2005/10/14  Boisy G. Pitre
* Now uses boot_common.asm for fragmented bootfile support.

         nam   Boot
         ttl   Disto RAMPak Boot Module

* Disassembled 94/06/25 11:37:47 by Alan DeKok

         IFP1  
         use   defsfile
         ENDC  

tylg     set   Systm+Objct
atrv     set   ReEnt+rev
rev      set   $00
edition  set   7

         mod   eom,name,tylg,atrv,start,size

* Common booter-required defines
LSN24BIT equ   0
FLOPPY   equ   0


* on-stack buffer to use
         org   0
mpisave  rmb   1
* common booter required static variables
ddtks    rmb   1
ddfmt    rmb   1
seglist  rmb   2
bootsize rmb   2
blockloc rmb   2
blockimg rmb   2
size     equ   .

name     equ   *
         fcs   /Boot/
         fcb   edition

         use   ../../6809l1/modules/boot_common.asm

* HWInit - Initialize the device
*   Entry: Y = hardware address
*   Exit:  Carry Clear = OK, Set = Error
*          B = error (Carry Set)
HWInit   lda   >MPI.Slct  get current slot
         sta   mpisave,u
         lda   >PakSlot,pcr get multipak slot number
         bmi   cont       if >127, invalid slot number
         anda  #$03       force it to be legal
         ldb   #$11
         mul              put it into both nibbles
         stb   >MPI.Slct  go to the desired slot
cont     clrb
         rts


* HWTerm - Terminate the device
*   Entry: Y = hardware address
*   Exit:  Carry Clear = OK, Set = Error
*          B = error (Carry Set)
HWTerm   lda   mpisave,u
         sta   >MPI.Slct
         clrb
         rts   


* HWRead - Read a 256 byte sector from the device
*   Entry: Y = hardware address
*          B = bits 23-16 of LSN
*          X = bits 15-0  of LSN
*          blockloc,u = ptr to 256 byte sector
*   Exit:  X = ptr to data (i.e. ptr in blockloc,u)
HWRead   tfr   x,d        move 16 bit LSN into 2 8-bit registers
         sta   2,y        save HB LSN
         stb   1,y        save LB LSN

         ldx   blockloc,u
         clrb             and start out at byte zero

ReadLp   stb   ,y         save byte number
         lda   3,y        grab the byte
         sta   ,x+        save in the buffer
         incb             go to the next byte
         bne   ReadLp
         leax  -256,x
         clrb             no errors
         rts

         IFGT  Level-1
Pad      fill  $39,$1D0-3-2-1-*
         ENDC

Address  fdb   $FF40      address of the device to boot from
PakSlot  fcb   $01        multipak slot number

         emod  
eom      equ   *
         end   
