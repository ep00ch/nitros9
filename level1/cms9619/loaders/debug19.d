DEBUG19.D      set       1

********************************************************************
* cms9619.d - DEBUG19 Definitions for CMS 9619 SBC
*
*
* $Id$
*
* Edt/Rev  YYYY/MM/DD  Modified by
* Comment
* ------------------------------------------------------------------
*          2017/04/29  Eric Pooch
* Started

        nam       cms9619.d
        ttl       DEBUG19 Definitions for CMS 9619 SBC

ACVECT  EQU     $9F0B

* DEBUG19 SUBROUTINES. JSR/JMP TO THEM.
RPT2ERR EQU     $F947
BEGEND  EQU     $FA77
CHKEOL  EQU     $FE2A
A1OUT   EQU     $FE79
CPYSTR  EQU     $FE88
HEX4BIN EQU     $FEBF
HEX2DEC EQU     $FECB
HEXDEC  EQU     $FEE2

