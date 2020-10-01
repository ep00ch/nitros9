********************************************************************
* Clock2 - CMS Tick/RTC Driver
*
* $Id$
*
* Configure the MC6840 to output a ~60Hz signal to the MC6821 CA1.
* Use CMS DEBUG09 ROM routines to set the RTC on CMS 9619.
* RTC is EPSON 58321B connected through MC6821 PIA port B
* and a decoder for the READ, WRITE, and ADDRESS WRITE lines.
*
* http://pdf.datasheetcatalog.com/datasheet/epson/RTC58321.pdf
*
* Edt/Rev  YYYY/MM/DD  Modified by
* Comment
* ------------------------------------------------------------------
*   1      2020/01/07  Eric S. Pooch
* Created.
*
          nam       Clock2
          ttl       CMS RTC Driver

          ifp1
          use       defsfile
          endc

tylg      set       Sbrtn+Objct
atrv      set       ReEnt+rev
rev       set       $00
edition   set       1

RTC.Base  equ       RTCBase        RTC is on PIA0

RTC.THr   equ       $05            RTH 10 hour digit address
RTC.TYr   equ       $0C            RTC 10 year digit address

          mod       eom,name,tylg,atrv,JmpTable,RTC.Base

name      fcs       "Clock2"
          fcb       edition


* These ROM-based subroutines could be incorporated in future revisions.
* Each needs PIA base address in regU, RTC address in regB, value in regA
RTCRead   equ    $E71D             DEBUG09 RTC read  subroutine
RTCWrite  equ    $E7AF             DEBUG09 RTC write subroutine


* Three Entry Points:
*   - Init
*   - GetTime
*   - SetTIme
JmpTable
          bra  Init                  MC6840 Init for ~60hz interrupt
          nop
          lbra  GetTime              RTC Get Time
          lbra  SetTime              RTC Set Time

********************************************************************
Init
* Configure the MC6840 PTM timer 1 to output a ~60Hz square wave on O1.
* This signal is routed to PIA0 CB1 by jumping TS14-1 to the CB1 side of TS10.
* The standard NitrOS-9 System Clock will handle things from there.
*
        lda     #$01    * enable Control Reg 1
        sta     CR2SR   * Control Reg 1 (CR10) may now be written @ CR13
        sta     CR13    * All timers held in preset (not decrementing)
*       ldx     #$4000  * Calculation: 1/((16384+1)(2E-6)/2) = 61.03Hz.
        ldx     #$4119  * ~60Hz.
        stx     MSBT1

        lda     #%10000010  * E clk, continuous, mask interrupts, output,
        sta     CR13    * CR10: operate clocks
        rts

********************************************************************
SetTime
          pshs u,y,x,d
          ldx  #D.Time             Start of OS9 DP System Date/Time in DP
          ldu  #RTC.Base
          ldb  #RTC.TYr            Start at the RTC decade digit register
SetTim1   lda  ,x+                 Get the next time unit value
          bsr  bin2bcd             Convert regA from binary to BCD
          pshs a                   save BCD
          asra
          asra
          asra
          asra                     10s nibble in regA
          cmpb RTC.THr             On 10 Hr digit register...
          bne  SetTim2
          ora  #$08                force 24 Hr Clock (not AM/PM)
SetTim2   jsr  RTCWrite            write the 10s digit to RTC
          decb                     Decrement the RTC address
          puls a                   Restore BCD value
          anda #$0f                1s nibble in regA
          jsr  RTCWrite            Write the 1s digit to RTC
          decb                     Decrement RTC address to smaller time unit
          bpl  SetTim1             Continue with the next time unit
          bra  UpdLeave

************************************************
bin2bcd
*
* in  A= value in binary
* out A= value in BCD
*
          pshs b
          clrb
bcdnxt    cmpa #10                 for each 10s
          bcs  bcddone
          addd #$f610              decrease bin regA by 10 add bcd $10 to regB
          bra  bcdnxt
bcddone   pshs b
          adda ,s+                 finally, add back the 10s to regA for BCD
          puls b,pc                return

********************************************************************
GetTime
          pshs u,y,x,d
          ldx  D.Time              Start of OS9 DP System Date/Time in DP
          ldu  #RTC.Base
          ldb  #RTC.TYr            Start at the RTC decade digit register
GetTim1   jsr  RTCRead             Read the 10s digit from RTC
          cmpb RTC.THr             On 10 Hr digit register...
          bne  GetTim2
          anda #$03                get the digit, not AM/PM or 24/12 setting
GetTim2   decb                     Decrement RTC address to 1s digit
          asla
          asla
          asla
          asla                     Move 10s digit to the high nibble
          pshs a                   Save 10s nibble
          jsr  RTCRead             Read the 1s digit from RTC
          adda ,s+                 Add back the 10s digit to regA for BCD
          bsr  bcd2bin             Convert regA from BCD to binary
          sta  ,x+                 Store value in OS9 DP System Date/Time
          decb                     Decrement RTC address to smaller time unit
          bpl  GetTim1             Get the next time unit
UpdLeave  puls d,x,y,u,pc

************************************************
bcd2bin
*
* in  A= value in BCD
* out A= value in binary
*
          pshs b
          tfr  a,b
          andb  #15                keep the lowest digit
          pshs  b                  save it
          anda  #$f0
          ldb   #160
          mul                      times 10
          adda ,s+                 add back partials
          puls b,pc                return

************************************************
          emod
eom       equ   *
          end
