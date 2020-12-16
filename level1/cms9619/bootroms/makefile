include ../port.mak

# Module directory
MD		= ../modules
CMDS		= ../cmds

DEPENDS		= ./makefile

DISTRO		= NOS9_L$(LEVEL)_$(NITROS9VER)_$(PORT)

KERNEL_ROM	= $(MD)/init0_rom $(MD)/krn_cms_rom $(MD)/krnp2 $(MD)/init_cms

ALLROMS		= $(DISTRO)_dw_cm6551.rom \
		  $(DISTRO)_dw_cm6551.dbgee $(DISTRO)_dw_cm6551.dbge


all:	$(ALLROMS)

kernelrom_%: 


# Kernel file and booter in ROM but bootfile on disk
$(DISTRO)_%.rom: $(KERNEL_ROM) $(MD)/boot_%
	$(MERGE) $^ > $@.tmp 
	# $12 (NOP) works better than $ff for echo text pacing
	$(PADROM) -c=18 8176 $@.tmp 
	$(MERGE) $@.tmp $(MD)/vectors_cms > $@
	# Just a check - should already be 8K
	$(PADROM) -b 8192 $@
	$(RM) $@.tmp
	#xxd -o $$((16#E000)) $@

# Format to program EEPROM in U17 using DEBUG19. 
# Slow and generates lots of error messages, but the EEPROM programmed correctly.
%.dbgee : %.rom
	xxd -c1 -u -o $$((16#C000)) $< | \
	sed -e 's/0000/E /' -e 's/  .*//' -e 's/: /:/'| \
	tr '[:lower:]' '[:upper:]' | tr ':' '\n'> $@
	#echo "V C000 DFFF" >> $@

%.dbge : %.rom
	$(ECHO) "E C000" >  $@
	xxd -p -u $< | tr -d "\n" >> $@

clean:
	$(RM) $(ALLROMS)

