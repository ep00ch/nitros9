ifeq ($(PORT),)
        PORT=cms9619
endif
include $(NITROS9DIR)/rules.mak

vpath %.asm $(LEVEL1)/modules/kernel

DEPENDS		= ./makefile

KERNEL		= krn_cms krn_cms_rom
KERNELP2	= krnp2
SYSCALLS	= fcmpnam.asm fprsnam.asm

AFLAGS		+= -I$(LEVEL1)/modules/kernel

ALLOBJS		= $(KERNEL) $(KERNELP2)

AUTOGEN		= krn_cms.asm

all: $(ALLOBJS) $(AUTOGEN)


krn_%.asm: krn_%.patch ../../../modules/kernel/krn.asm 
	# Patch a copy of of the kernel code for CMS 9619 debug ROM.
	$(CP) ../../../modules/kernel/krn.asm  ./
	git apply -v -p4 --directory="level1/$(PORT)/modules/kernel" $<
	#git apply --verbose -p2 --directory=level1/cms9619 $<
	git apply --verbose -p2 --directory=$(LEVEL1)/$(PORT) $<
	$(CP) ./krn.asm ./$@
	$(RM) ./krn.asm

krn_cms: krn_cms.asm $(SYSCALLS)
	$(AS) -l$@.lst $< $(ASOUT)$@ $(AFLAGS)

%_rom:	%.asm	$(SYSCALLS)
	$(AS) -l$@.lst $< $(ASOUT)$@ $(AFLAGS) -DROM=1
	#-DCHECK_FOR_VALID_RAM


$(KERNELP2):	krnp2.asm $(SYSCALLS)
	$(AS) -l$@.lst $< $(ASOUT)$@ $(AFLAGS)

clean:
	$(RM) $(ALLOBJS) $(AUTOGEN)

showobjs:
	@$(ECHO) $(ALLOBJS)

showcopyobjs:
	@$(ECHO) $(COPYOBJS)

identify:
	$(IDENT_SHORT) $(ALLOBJS)
