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

clean:
	$(RM) *.dbge
	$(RM) *.hex
	$(RM) *.out

.PHONY: clean all
