include port.mak

# Level 1 - Specify which shell should be used
#WHICHSHELL	= shellplus
WHICHSHELL	= shell_21

DISTRO		= $(CPU)L$(LEVEL)
DISTRONAME	= NOS9_$(CPU)_L$(LEVEL)
DISTROVER	= $(DISTRONAME)_$(NITROS9VER)_$(PORT)


DIRS		= defs cmds modules sys bootfiles bootroms loaders


CMDS		= $(shell $(CD) cmds; make --no-print-directory showobjs)
CMDS_D2		= $(shell $(CD) cmds; make --no-print-directory showobjs_d2)
CMDS_DW		= $(shell $(CD) cmds; make --no-print-directory showobjs_dw)
CMDS_DEMO	= $(shell $(CD) cmds; make --no-print-directory showobjs_demo)
SYS		= $(shell $(CD) sys; make --no-print-directory showobjs)
DEFS		= $(shell $(CD) defs; make --no-print-directory showobjs)
STARTUP		= startup

DSK		= $(DISTROVER).dsk

DSKS		= $(DISTROVER)_dw_cm6551.dsk $(DISTROVER)_dw_cm6821.dsk

# Make all components
all:
	@$(ECHO) "************************************************************"
	@$(ECHO) "*"
	@$(ECHO) "*      NitrOS-9/$(CPU) Level $(LEVEL) $(MACHINE) ($(PORT))"
	@$(ECHO) "*"
	@$(ECHO) "************************************************************"
	$(foreach dir,$(DIRS),$(MAKE) -C $(dir) &&) :

# Clean all components
clean:	dskclean
	$(foreach dir,$(DIRS),$(MAKE) -C $(dir) clean &&) :

dskclean:
	$(RM) $(DSKS)

dsk: all $(DSKS)


dskcopy: $(DSKS)
	$(CP) $(DSKS) $(DSKDIR)

$(DISTROVER)_%.dsk:
	$(RM) $@
	$(OS9FORMAT_SS80) -e -q $@ -n"NitrOS-9/$(CPU) Level 1"
	$(OS9GEN) $@ -b=bootfiles/bootfile_$* -t=bootfiles/kernel_$*
	$(MAKDIR) $@,CMDS
	$(MAKDIR) $@,SYS
	$(MAKDIR) $@,DEFS
	$(CD) cmds; $(OS9COPY) $(sort $(CMDS) $(CMDS_D2)) ../$@,CMDS
	$(OS9ATTR_EXEC) $(foreach file,$(CMDS),$@,CMDS/$(file))
	$(OS9RENAME) $@,CMDS/$(WHICHSHELL) shell
	$(CD) sys; $(CPL) $(SYS) ../$@,SYS
	$(OS9ATTR_TEXT) $(foreach file,$(SYS),$@,SYS/$(file))
	$(CD) defs; $(CPL) $(DEFS) ../$@,DEFS
	$(OS9ATTR_TEXT) $(foreach file,$(DEFS),$@,DEFS/$(file))
	$(CPL) $(STARTUP) $@,startup
	$(OS9ATTR_TEXT) $@,startup

info:
	@$(ECHO) "*** NitrOS-9/6809 Level 1 for the $(MACHINE) SBC ***"
	@$(foreach dsk, $(DSKS), $(ECHO) $(dsk);)
