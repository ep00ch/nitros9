* (c) 1982 MICROWARE CORP.
* Maybe for CMS 9670 EPDT host adapter,
* or CMS 9671 intelligent floppy disk controller.

         nam   Boot
         ttl   os9 disk boot module.

         use   defsfile

tylg     set   Systm+Objct
atrv     set   ReEnt+rev
rev      set   $01
         mod   BootEnd,name,tylg,atrv,start,0

         org   0
* NOTE: these are s and u-stack offsets, not DP
* This still needs alot of work figuring out the variables,
* and replacing them in the code.
*
vals1     rmb   10
LSN24Ptr  rmb   1       $11 current sector 24 bit HI byte
LSN16Ptr  rmb   2       $12 current sector
        org   $20
bootsize  rmb   2       $20 size of boot sector in bytes

name     equ   *
         fcs   /Boot/
         fcb   $05

L0012    fcb   $02,$00,$21,$01,$01,$04,$02,$64,$02,$65,$02,$65,$00,$00
L0020    fcb   $00,$00,$21,$01,$01,$02,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00

L002E    lda   >DiskCBase
         orcc  #$01
         bita  #$40
         bne   L0046
         stx   <$18,u
         sty   <$1A,u
         stb   <$1D,u
         stb   >DiskCBase
         andcc #$FE
L0046    rts

L0047    lda   >DiskCBase
         bmi   L0052
         bita  #$40
         bne   L0047
         bra   L0062
L0052    tfr   a,b
         bita  #$40
         beq   L0071
         anda  #$07
         leax  >L0074,pcr
         lda   a,x
         jsr   a,x
L0062    tst   <$1D,u
         bne   L0047
         lda   <$1C,u
         bita  #$02
         bne   L0071
         andcc #$FE
         rts

L0071    orcc  #$01
         rts

L0074    fcb   L008F-L0074  0
         fcb   L00A2-L0074  1
         fcb   L007C-L0074  2
         fcb   L00B3-L0074  3
         fcb   RETRN-L0074  4
         fcb   RETRN-L0074  5
         fcb   RETRN-L0074  6
         fcb   L00BA-L0074  7

L007C    ldx   <$18,u
L007F    lda   ,x+
         sta   >DiskCBase+1
L0084    brn   L0084
         cmpb  >DiskCBase
         beq   L007F
         stx   <$18,u
RETRN    rts

* Copy to drive
L008F    ldx   <$1A,u
L0092    lda   ,x+
         sta   >DiskCBase+1
L0097    brn   L0097
         cmpb  >DiskCBase
         beq   L0092
         stx   <$1A,u
         rts

* Copy from drive
L00A2    ldx   <$1A,u
L00A5    lda   >DiskCBase+1
         sta   ,x+
         cmpb  >DiskCBase
         beq   L00A5
         stx   <$1A,u
         rts

L00B3    lda   >DiskCBase+1
         sta   <$1C,u
         rts

L00BA    tst   >DiskCBase+1
         clr   <$1D,u
         rts

L00C1    pshs  y,x,b,a
         sta   <$14,u
         stx   <$12,u
         clrb
         lda   >DiskCBase
         bita  #$10
         bne   L00D3
         ldb   #$40
L00D3    orb   $01,s
         stb   <$11,u
         clr   <$15,u
         puls  pc,y,x,b,a

L00DD    pshs  y,x,b,a
         leax  <$10,u
         ldy   <$16,u
         bsr   L00EE
         bcc   L00EC
         stb   $01,s
L00EC    puls  pc,y,x,b,a

L00EE    ldb   #$01
         lbsr  L002E
         bcs   L00EE
         lbsr  L0047
         bcc   L00FC
         ldb   #E$Read          Read error
L00FC    rts

L00FD    pshs  u,y,x,b,a
         sta   <$1E,u
         lda   #$01
         bsr   L00C1
         clr   <$10,u
L0109    bsr   L00DD
         bcs   L0109
         lda   #$08
         sta   <$10,u
L0112    leax  <$10,u
         ldy   <$1A,u
         bsr   L00EE
         bcs   L0135
         ldd   <$12,u           load the value
         addd  #$0001
         std   <$12,u           increment it
         ldb   #$00
         adcb  <$11,u           Add carry to HI byte
         stb   <$11,u           save it
         dec   <$1E,u           decrement the counter
         bne   L0112            countinue if not zero
         bra   L0137
L0135    stb   $01,s
L0137    puls  pc,u,y,x,b,a

* Delay
Delay25ms    pshs  b,a,cc
         ldd   #$1BE6
L013E    subd  #$0001
         bne   L013E
         puls  pc,b,a,cc

start    equ   *
         pshs  u,y,x,b,a
         leas  <-$20,s          Add space for variables on stack
         ldd   #$0100           Need 256 byte buffer
         os9   F$SRqMem         Request it from kernel
         lbcs  ErrRet           If problem, exit with error
         stu   <$16,s           Save HI address of buffer to local vars
         leau  ,s               User stack points to local vars on stack
         lda   #$10
         sta   >DiskCBase
         bsr   Delay25ms
         lda   >DiskCBase
         leay  >L0012,pcr
         clrb
         clr   <$1F,u
         bita  #$10
         bne   L0175
         leay  >L0020,pcr
         ldb   #$40
L0175    stb   <$11,u
         lda   #$C4
         leax  <$10,u
         sta   ,x
         clr   <$15,u
         pshs  y,x
         lbsr  L00EE
         puls  y,x
         lbsr  L00EE
         ldd   #$0100
         ldx   #$0000   LSN 0
         lbsr  L00FD
         bcs   ErrRet
         ldx   <$16,u
         ldy   <DD.BSZ,x    os9boot size in bytes
         beq   L01CA        No boot file?
         sty   <$20,s
         ldb   <DD.BT,x     MSB fd sector location
         leau  ,x
         ldx   <DD.BT+1,x   LSW fd sector location
         pshs  y,x,b
         ldd   #$0100
         os9   F$SRtMem     Return memory boot sector
         ldd   $03,s
         os9   F$SRqMem
         puls  y,x,b
         stu   <$16,s
         stu   <$22,s
         leau  ,s
         lbsr  L00FD
         bcc   L01D0
         bcs   ErrRet
L01CA    comb
         ldb   #E$BTyp      Bad Type (incompatable) media
ErrRet   stb   <$21,s
L01D0    leas  <$20,s       Give space for variables back to stack
         puls  pc,u,y,x,b,a
         emod

BootEnd      equ   *
