         IFNE	DRAGON.D-1
DRAGON.D set   1         
                         
********************************************************************
* DgnDefs - Dragon I/O Definitions
*
* $Id$
*
* Edt/Rev  YYYY/MM/DD  Modified by
* Comment
* ------------------------------------------------------------------
*          2004/11/16  P.Harvey-Smith.
* Fixed the stupid error I made in the defines below that made all the
* non DPxxxxx defines equal to FF00 !!!
*
*	   2005/04/14  P.Harvey-Smith
* Added non DP defines for ACIA on Dragon 64/Alpha
*
*	   2005/04/21  P.Harvey-Smith
* Fixed errors in defines for WD2797 non-DP registers.
*

         nam   DgnDefs  
         ttl   Dragon I/O Definitions
                         
*************************************************
*
* NitrOS-9 Level 1 Section
*
*************************************************

HW.Page        SET       $FF                 Device descriptor hardware page

**********************************
* Power Line Frequency Definitions
*
Hz50           EQU       1                   Assemble clock for 50 hz power
Hz60           EQU       2                   Assemble clock for 60 hz power
PwrLnFrq       SET       Hz60                Set to Appropriate freq


**********************************
* Ticks per second
*
               IFEQ      PwrLnFrq-Hz50
TkPerSec       SET       50
               ELSE      
TkPerSec       SET       60
               ENDC      


****************************************
* Special character Bit position equates
*
SHIFTBIT       EQU       %00000001
CNTRLBIT       EQU       %00000010
ALTERBIT       EQU       %00000100
UPBIT          EQU       %00001000
DOWNBIT        EQU       %00010000
LEFTBIT        EQU       %00100000
RIGHTBIT       EQU       %01000000
SPACEBIT       EQU       %10000000

********************
* VTIO Static Memory
*
* Definitions for ports on Dragon 32/64/Alpha.
*
*
IO		equ		$ff00		IO page on Dragon

*
* Most of these symbols will be defined twice, as some 
* of the Dragon code, sets DP=$FF, and uses direct page
* addressing to access the io ports, whilst some of it
* uses absolute addressing.
* The versions starting DP must be used with DP=$FF.
*

* PIA 0 and 1 standard on all Dragons.
DPPIA0DA	EQU		$00		Side A Data/DDR
PIA0Base  EQU       DPPIA0DA
DPPIA0CRA	EQU		$01		Side A Control.
DPPIA0DB	EQU		$02		Side B Data/DDR
DPPIA0CRB	EQU		$03		Side B Control.

PIA0DA		EQU		DPPIA0DA+IO	Side A Data/DDR
PIA0CRA		EQU		DPPIA0CRA+IO	Side A Control.
PIA0DB		EQU		DPPIA0DB+IO	Side A Data/DDR
PIA0CRB		EQU		DPPIA0CRB+IO	Side A Control.

DPPIA1DA	EQU		$20		Side A Data/DDR
PIA1Base  EQU       DPPIA1DA
DPPIA1CRA	EQU		$21		Side A Control.
DPPIA1DB	EQU		$22		Side B Data/DDR
DPPIA1CRB	EQU		$23		Side B Control.

PIA1DA		EQU		DPPIA1DA+IO	Side A Data/DDR
PIA1CRA		EQU		DPPIA1CRA+IO	Side A Control.
PIA1DB		EQU		DPPIA1DB+IO	Side A Data/DDR
PIA1CRB		EQU		DPPIA1CRB+IO	Side A Control.

* Dragon Alpha has a third PIA at $FF24.
DPPIA2DA	EQU		$24		Side A Data/DDR
DPPIA2CRA	EQU		$25		Side A Control.
DPPIA2DB	EQU		$26		Side B Data/DDR
DPPIA2CRB	EQU		$27		Side B Control.

PIA2DA		EQU		DPPIA2DA+IO	Side A Data/DDR
PIA2CRA		EQU		DPPIA2CRA+IO	Side A Control.
PIA2DB		EQU		DPPIA2DB+IO	Side A Data/DDR
PIA2CRB		EQU		DPPIA2CRB+IO	Side A Control.

* WD2797 Floppy disk controler, used in Alpha Note registers in reverse order !
DPCmdRegA	EQU		$2F		command/status			
DPTrkRegA	EQU		$2E		Track register
DPSecRegA	EQU		$2D		Sector register
DPDataRegA	EQU		$2C		Data register

CmdRegA		EQU		DPCMDREGA+IO	command/status			
TrkRegA		EQU		DPTRKREGA+IO	Track register
SecRegA		EQU		DPSECREGA+IO	Sector register
DataRegA	EQU		DPDATAREGA+IO	Data register

DPort          SET       DataRegA               Disk controller base address

* Constants for Alpha AY-8912 sound chip, which is used to control
* Drive select and motor on the Alpha
AYIOREG		EQU		$0E		AY-8912, IO Register number.
AYIdle		EQU		$00		Make AY Idle.
AYWriteReg	EQU		$01		Write AY Register
AYReadReg	EQU		$02		Read AY Register
AYREGLatch	EQU		$03		Latch register into AY

DSMask		EQU		$03		Drive select mask.
MotorMask	EQU		$04		Motor enable mask
DDENMask	EQU		$08		DDEN Mask
ENPMask		EQU		$10		Enable Precomp mask
NMIMask		EQU		$20		NMI enable Mask

* Dragon 64/Alpha Serial port.
DPAciaData	EQU		$04		ACIA Rx/Tx Register
DPAciaStat	EQU		$05		ACIA status register
DPAciaCmd	EQU		$06		ACIA command register
DPAciaCtrl	EQU		$07		ACIA control register

AciaData	EQU		DPAciaData+IO	ACIA Rx/Tx Register
AciaStat	EQU		DPAciaStat+IO	ACIA status register
AciaCmd		EQU		DPAciaCmd+IO	ACIA command register
AciaCtrl	EQU		DPAciaCtrl+IO	ACIA control register

* DragonDos Cartrage IO for WD2797
* WD2797 Floppy disk controler, used in DragonDos.
DPCmdRegD	EQU		$40		command/status			
DPTrkRegD	EQU		$41		Track register
DPSecRegD	EQU		$42		Sector register
DPDataRegD	EQU		$43		Data register

CmdRegD		EQU		DPCMDREGD+IO	command/status			
TrkRegD		EQU		DPTRKREGD+IO	Track register
SecRegD		EQU		DPSECREGD+IO	Sector register
DataRegD	EQU		DPDATAREGD+IO	Data register

DPDSKCTL	EQU		$48		Disk DS/motor control reg
DSKCTL		EQU		DPDSKCTL+IO		

* Disk IO bitmasks (DragonDos).
NMIEnD    	EQU		%00100000 
WPCEnD    	EQU   		%00010000 
SDensEnD  	EQU   		%00001000 
MotorOnD  	EQU   		%00000100 
Drive0D		EQU		%00000000
Drive1D		EQU		%00000001
Drive2D		EQU		%00000010
Drive3D		EQU		%00000011
DDosDriveMask	EQU		%00000011	Mask out all non drive select bits
DDosCtrlMask	EQU		%11111100	Mask in all non drive select bits


* Disk IO bitmasks (Dragon Alpha).
PIANMIEnA    	EQU		%00001000	PIA2, CA2, used to enable/disable NMI 
PIANMIDisA	EQU		%11110111	Bitmask to force CA2 off, and disable NMI

NMIEnA		EQU		%10000000	Flag to enable disable NMI, passed to AlphaDskCtl
WPCEnA    	EQU   		%01000000 	According to circuit trace by R.Harding.
SDensEnA  	EQU   		%00100000 	DDen, from circuit trace on R.Harding's machine.
MotorOnA  	EQU   		%00010000 	
Drive0A		EQU		%00000001
Drive1A		EQU		%00000010
Drive2A		EQU		%00000100
Drive3A		EQU		%00001000
AlphaDrvMask	EQU		%00001111	Mask out all non drive select bits
AlphaCtrlMask	EQU		%11110000	Mask in all non drive select bits

Mask58		EQU		%01111111	And mask to make sure 5.25" clock selected by WD2797

NMICA2En	EQU		$3C		Value for PIA CRA to enable NMI
NMICA2Dis	EQU		$34		Value for PIA CRA to disable NMI

* Disk Commands
FrcInt   	EQU   	%11010000 
ReadCmnd 	EQU   	%10001000 
RestCmnd 	EQU   	%00000000 
SeekCmnd 	EQU   	%00010000 
StpICmnd 	EQU   	%01000000 
WritCmnd 	EQU   	%10101000 
WtTkCmnd 	EQU   	%11110000 
Sid2Sel  	EQU   	%00000010 

* Disk Status Bits
BusyMask 	EQU   	%00000001 
LostMask 	EQU   	%00000100 
ErrMask  	EQU   	%11111000 
CRCMask  	EQU   	%00001000 
RNFMask  	EQU   	%00010000 
RTypMask 	EQU   	%00100000 
WPMask   	EQU   	%01000000 
NotRMask 	EQU   	%10000000 

DensMask 	EQU   	%00000001 
T80Mask  	EQU   	%00000010 

		ENDC