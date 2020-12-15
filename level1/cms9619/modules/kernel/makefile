ifeq ($(PORT),)
        PORT=cms9619
endif
include $(NITROS9DIR)/rules.mak

vpath %.asm $(LEVEL1)/modules/kernel

DEPENDS		= ./makefile

KERNEL		= krn_cms krn
KERNELP2	= krnp2
SYSCALLS	= fcmpnam.asm fprsnam.asm

AFLAGS		+= -I$(LEVEL1)/modules/kernel

ALLOBJS		= $(KERNEL) $(KERNELP2) krn_rom

AUTOGEN		= krn_cms.asm

all:	$(ALLOBJS)

%_cms.asm: ../../../modules/kernel/%.asm %.patch
	# Patch a copy of of the kernel code for CMS 9619 debug ROM.
	$(CP) $< ./
	git apply -v -p4 --directory="level1/cms9619/modules/kernel" $*.patch
	$(CP) $*.asm $@
	$(RM) $*.asm

krn%:	krn%.asm $(SYSCALLS)
	$(AS) -l$@.lst $< $(ASOUT)$@ $(AFLAGS)

clean:
	$(RM) $(ALLOBJS) $(AUTOGEN)

showobjs:
	@$(ECHO) $(ALLOBJS)

showcopyobjs:
	@$(ECHO) $(COPYOBJS)

identify:
	$(IDENT_SHORT) $(ALLOBJS)
