ifdef LOCAL
# so we can compile from within the loaders folder
AS = lwasm 
AFLAGS = 
LEVEL1 = ../../modules
RM = rm -f
else
include ../port.mak
endif

AFLAGS		+= --includedir=$(LEVEL1)/modules

# Format to dump boot loader to RAM using DEBUG19.
%.dbge : %.asm
	$(AS) $(AFLAGS) -f hex $<
	tail -n +2 a.out | tr -d '\r' > $*.hex
	printf "E " > $@
	head -1 $*.hex | cut -c1-4 >> $@
	sed -e 's/....://' a.out | \
	tr -d '\r\n,'| tr ':' '\n' >> $@
	$(RM) a.out
	printf "\nJ " >> $@
	head -1 $*.hex | cut -c1-4 >> $@


all:	cms_zm_loader.dbge cms_dw_loader.dbge


load-%: $(DISTRO)_dw_cm6551.dbgee /dev/%
	@echo "Transferring loader to CMS9619"
	/usr/bin/expect loaders/cms_loader.exp /dev/$* $<

program-%: ../bootroms/NOS9_L1_v030300_cms9619_dw_cm6551.dbgee /dev/% 
	@while [ -z "$$CONTINUE" ]; do \
		read -r -p "Programming NitrOS-9 to EEPROM in U13. \n\
		This takes about 10 minutes to complete and \
		generates many DEBUG19 errors which can be ignored. \n\
		Continue? [y/N]: " CONTINUE; \
	done ; \
    	[ $$CONTINUE = "y" ] || [ $$CONTINUE = "Y" ] || (echo "Exiting."; exit 1;)
	/usr/bin/expect cms_loader.exp /dev/$* $<


clean:
	$(RM) *.dbge
	$(RM) *.hex
	$(RM) *.out

.PHONY: clean all
