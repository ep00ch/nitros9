*******************************************************
*
* DWInit
*    Initialize DriveWire for CoCo Bit Banger

DWInit
               IFNE     ARDUINO

* setup PIA PORTA (read)
               clr       $FF51
               clr       $FF50
               lda       #$2C
               sta       $FF51

* setup PIA PORTB (write)
               clr       $FF53
               lda       #$FF
               sta       $FF52
               lda       #$2C
               sta       $FF53
               rts

               ELSE

               * reserve $9800-$9F00 area with reset vectors, etc.
               ldd     #$00ff
               std     $212

               IFNE     SY6551B

* init ACIA baud settings
               pshs      d,x

               LDX     #SY6551B         GET ACIA ADDRESS
               LDB     #%00001111       19200
               ORB     #%00010000       USE INTERNAL BAUD RATE GEN
               LDA     #%00001011       DTR LOW, RTS LOW, NO IRQ, NO PARITY
               STD     $02,X            SET ACIA COMMAND & CONTROL REG
               puls       d,x,pc

               ELSE

               pshs      a,x
               IFDEF     PIA1Base
               ldx       #PIA1Base           $FF20
               clr       1,x                 clear CD
               lda       #%11111110
               sta       ,x
               lda       #%00110100
               sta       1,x
               lda       ,x
               ENDC
               puls       a,x,pc

               ENDC


