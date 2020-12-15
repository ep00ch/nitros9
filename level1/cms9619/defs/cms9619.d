CMS9619.D      set       1

********************************************************************
* cms9619.d - NitrOS-9 System Definitions for the CMS 9619 SBC
*
* This is a high level view of the CMS 9619 memory map as setup by
* NitrOS-9.
*
* $Id$
*
* Edt/Rev  YYYY/MM/DD  Modified by
* Comment
* ------------------------------------------------------------------
*          2017/04/29  Eric Pooch
* Started

               nam       cms9619.d
               ttl       NitrOS-9 System Definitions for the CMS 9619 SBC


RTC58321       set 58321
MC6840         set 6840

ClocType       set MC6840
*ClocType      set RTC58321 * This does not work as a tick source yet.

**********************************
* Ticks per second
*
        ifeq    ClocType-MC6840
TkPerSec       SET       60
        endc

        ifeq    ClocType-RTC58321
TkPerSec       SET       1
        endc


*************************************************
*
* NitrOS-9 Level 1 Section
*
*************************************************

HW.Page        set       $F0                 Device descriptor hardware page


********************************************************************
* NitrOS-9 Memory Definitions for the CMS 9619
*

********************************************************************
* CMS 9619 Hardware Definitions
*
IOBase          equ $FFC0   * U12 PAL DECODER


EXTIO           equ $FFC0   * Free for External IO
ROMBase         equ $E000   * Start of NitrOS-9 Boot ROM, if equipped


PIA0Base        equ $FFC4   *-2 * U31 MC6821 PIA - RTC and BAUD DIP
RTCBase         equ $FFC4   * RTC is on PIA0

PIA1Base        equ $FFC8   * U30 MC6821 PIA
PIA2Base        equ $FFCC   * U29 MC6821 PIA
ACIA0Base       equ $FFD0   * U27 MC6551 ACIA
ACIA1Base       equ $FFD4   * U26 MC6551 ACIA
PTMBase         equ $FFD8   * U28 MC6840 PTM

        org     PTMBase
CR13    rmb     1       * Control reg for timers 1 and 3
CR2SR   rmb     1       * Control reg for timer  2 and the status reg
MSBT1   rmb     1       * MSB for timer 1
T1LSB   rmb     1       * LSB for timer 1
MSBT2   rmb     1       * MSB for timer 2
T2LSB   rmb     1       * LSB for timer 2
MSBT3   rmb     1       * MSB for timer 3
T3LSB   rmb     1       * LSB for timer 3


* ACIA0 is a lower address, but is the secondary ACIA.
termBase        equ ACIA1Base
t0Base          equ ACIA1Base
t1Base          equ ACIA0Base
*t2Base          equ PIA1Base
pBase           equ PIA1Base

* Terminals for CMS 9650A SERIAL I/O Card.
t2Base         equ EXTIO+0
t3Base         equ EXTIO+2
t4Base         equ EXTIO+4
t5Base         equ EXTIO+6
t6Base         equ EXTIO+8
t7Base         equ EXTIO+10
t8Base         equ EXTIO+12
t9Base         equ EXTIO+14

SY6551B         equ ACIA0Base   * DriveWire ACIA

SYCONSET        equ %00011111   * 19200 bps

SHIFTBIT        equ 'A'

WRTSTR          equ $FE88

********************************
* Boot defs for NitrOS-9 Level 1
*
* These defs are not strictly for 'Boot', but are for booting the
* system.
*
Bt.Start        EQU     $C000               Start address of the boot track in HI memory

Bt.Track        EQU     34                  Boot track
Bt.Sec          EQU     0                   Start LSN of boot area on boot track

Bt.Size         EQU     $2000               Maximum size of bootfile


********************************
* RAM interrupt vector locations for CMS 9619.
* These need to be set to NitrOS-9 Level 1 locations
*
RAM_SW3_VEC     EQU     $9F0D
RAM_SW2_VEC     EQU     $9F0F
RAM_SWI_VEC     EQU     $9F11
RAM_IRQ_VEC     EQU     $9F13
RAM_FRQ_VEC     EQU     $9F15
RAM_NMI_VEC     EQU     $9F17



