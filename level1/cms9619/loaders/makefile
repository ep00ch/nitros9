include ../port.mak


# Format to dump boot loader to RAM using DEBUG19.
%.dbge : %.asm
	$(AS) $(AFLAGS)  -f hex -o $*.hex $<
	echo "E " | cat - $*.hex | \
	sed -e '4,$$s/....://' | \
	tr -d '\r\n,'| tr ':' '\n' > $@
	echo $$'\n' >> $@
	echo "J 9014" >> $@

all:	cms_zm_loader.dbge cms_dw_loader.dbge

clean:
	$(RM) *.dbge
	$(RM) *.hex

.PHONY: clean all
