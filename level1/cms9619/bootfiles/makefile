include ../port.mak

# Build directories
MD		= ../modules
CMDS		= ../cmds
DEPENDS		= ./makefile

KERNEL_CMS	= $(MD)/rel $(MD)/init0_cms $(MD)/krn $(MD)/krnp2 $(MD)/init_cms

TERM		= $(MD)/sc6551_cms.dr $(MD)/scbbp.dr $(MD)/term_sc6551_cms.dt \
			$(MD)/t0_sc6551_cms.dd $(MD)/t1_sc6551_cms.dd \
			$(MD)/p_scbbp_cms.dd
PIPE		= $(MD)/pipeman.mn $(MD)/piper.dr $(MD)/pipe.dd
CLOCKCMS	= $(MD)/clock $(MD)/clock2_cms 

BOOTF_PRE	= $(MD)/ioman \
		$(MD)/rbf.mn

BOOTF_POST	= $(MD)/ddx0.dd $(MD)/scf.mn \
		$(TERM) \
		$(PIPE) \
		$(CLOCKCMS) \
		$(MD)/sysgo

KERNELS 	= kernel_dw_sc6551 kernel_dw_mc6821 kernel_zm

BOOTFILES	= bootfile_dw_sc6551 bootfile_dw_mc6821 bootfile_zm

ALLOBJS		= $(BOOTFILES) $(KERNELS)

all : $(ALLOBJS)

rb_zm :
	# still need to make this.
	touch $@

rb_dw_% : $(MD)/rbdw.dr $(MD)/dwio_%.sb $(MD)/x1.dd $(MD)/x2.dd $(MD)/x3.dd
	# Temporary DriveWire module
	$(MERGE) $^ > $@

kernel_% : $(KERNEL_CMS) $(MD)/boot_%
	$(MERGE) $^ > $@

bootfile_% : $(BOOTF_PRE) rb_% $(BOOTF_POST) $(DEPENDS)
	$(MERGE) $^ > $@

clean :
	$(RM) $(ALLOBJS)
	$(RM) rb_zm

