********************************************************************
* Prog - Program EEPROMS
*
* $Id$
*
* Edt/Rev  YYYY/MM/DD  Modified by
* Comment
* ------------------------------------------------------------------
*   5      ????/??/??
* From CMS 9619 DEBGUG19

         nam   Prog
         ttl   Program EEPROMS


         ifp1
         use   defsfile
         endc

tylg     set   Prgrm+Objct   
atrv     set   ReEnt+rev
rev      set   $01
edition  set   1

         mod   eom,name,tylg,atrv,start,size

         org   0

EPROM    equ   $E863          * Entry point for EPROM program in DEBUG19 ROM

u0000    rmb   450
size     equ   .

name     fcs   /Prog/
         fcb   edition

start    orcc  #$10          * disable interrupts so progam can get ACIA input.
         jsr EPROM

Exit
         andcc #$EF           * enable interrupts so OS9 can get ACIA input.
         os9   F$Exit

         emod
eom      equ   *
         end

