********************************************************************
* VI - VRN (VIRQ/RAM/Nil driver) device descriptor
*
* $Id$
*
* Ed.    Comments                                       Who YY/MM/DD
* ------------------------------------------------------------------

         nam   VI
         ttl   VRN (VIRQ/RAM/Nil driver) device descriptor

         ifp1  
         use   defsfile
         endc  

Edtn     equ   1
Vrsn     equ   1

         mod   ModSize,DvcNam,Devic+Objct,ReEnt+Vrsn,MgrNam,DrvNam

         fcb   UPDAT.     access mode(s)
         fcb   HW.Page    hardware page
         fdb   $FF02      hardware port
         fcb   OptSize
OptStart fcb   DT.SCF
OptSize  equ   *-OptStart
MgrNam   fcs   "SCF"
DrvNam   fcs   "VRN"
DvcNam   fcs   "VI"
         fcb   Edtn

         emod  
ModSize  equ   *
         end   
