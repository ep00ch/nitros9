include ../port.mak

# Module directory
MD		= ../modules
BF		= ../bootfiles
CMDS		= ../cmds

DEPENDS		= ./makefile

DISTRO		= NOS9_L$(LEVEL)_$(NITROS9VER)_$(PORT)

KERNEL_ROM	= $(MD)/init0_rom $(MD)/krn $(MD)/krnp2 $(MD)/init_cms

ALLROMS		= $(DISTRO)_dw_cm6551.rom

all:	$(ALLROMS)


# Kernel file and booter in ROM but boot file on disk
$(DISTRO)_%.rom: $(KERNEL_ROM) $(MD)/boot_%
	$(MERGE) $^ > $@.tmp
	$(PADROM) 8176 $@.tmp 
	$(MERGE) $@.tmp $(MD)/vectors_cms > $@
	# Just a check - should already be 8K
	$(PADROM) -b 8192 $@
	$(RM) $@.tmp
	#xxd -o $$((16#E000)) $@


clean:
	$(RM) $(ALLROMS)

