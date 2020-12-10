include ../port.mak

# Build directories
MD		= ../modules
CMDS		= ../cmds
DEPENDS		= ./makefile

KERNEL_CMS	= $(MD)/rel $(MD)/init0_cms $(MD)/krn $(MD)/krnp2 $(MD)/init_cms

TERM		= $(MD)/cm6551.dr $(MD)/scbbp.dr $(MD)/term_cm6551.dt \
			$(MD)/t0_cm6551.dd $(MD)/t1_cm6551.dd \
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

KERNELS 	= kernel_dw_cm6551 kernel_dw_cm6821 kernel_zm

BOOTFILES	= bootfile_dw_cm6551 bootfile_dw_cm6821 bootfile_zm

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

bootfile_% : $(BOOTF_PRE) rb_% $(BOOTF_POST)
	$(MERGE) $^ > $@

clean :
	$(RM) $(ALLOBJS)
	$(RM) rb_zm

