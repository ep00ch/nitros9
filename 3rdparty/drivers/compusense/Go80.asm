******************************************************************** Go80 - DragonPlus 6845 based 80x25 (driver)** Edt/Rev  YYYY/MM/DD  Modified by* Comment* ------------------------------------------------------------------*   -      1985/??/??* Original Compusense distribution version** 2005-09-01, P.Harvey-Smith.* 	Disassembled and cleaned up.*         * 2005-09-05, P.Harvey-Smith.* 	This is not a driver !, all this appears to do is patch the *	already loaded kbvidio module to talk to the 6845 on the plus board.*	These patches are stored in a set of S-records at the end of this file.	nam   GO80        ttl   program module       * Disassembled 2005/05/31 16:28:10 by Disasm v1.5 (C) 1988 by RML        ifp1        use   	/dd/defs/defsfile	use 	dplusdef        endctylg    set   	Prgrm+Objct   atrv    set   	ReEnt+revrev     set   	$01        mod   	eom,name,tylg,atrv,start,size	u0000    		rmb   1KbVidioAddress    	rmb   1		* Address of loaded KBVidio Moduleu0002    		rmb   1u0003    		rmb   1SRecCount    		rmb   1		* S record count, decremented as each byte fetched.AddrHi    		rmb   1		* Hi and low bytes of the address to patch (offset from kbvideo start addr)AddrLo    		rmb   1SRecHeadBuff    	rmb   1		* Buffer for 'S' of s recordSRecTypeBuff    	rmb   1		* Type buffer used to determine record type ('1' or '9')SRecCountBuff    	rmb   2		* Byte count buffer, number of bytes to fetch (in ascii)AddrHiBuff    		rmb   2		* Buffers for high and low parts of address as ascii hexAddrLoBuff    		rmb   2DataBuff    		rmb   90	* Buffer for data part of S record lineCopyBuffSrcAddr    	rmb   2		* Address of buffer when copying bytesBinBuffPtr    		rmb   34	* Pointer to binary data buffer		BinBuff    		rmb   2298	* Buffer to hold binary equivelent of S records size     		equ   .name    equ   	*        fcs   	/GO80/SignonMess   	fcc	'GO80 (C) COPYRIGHT COMPUSENSE LIMITED 1985'	fcb   	$0D SignonMessLen	equ	*-SignonMess	SwitchMess   	fcc	'please switch to DRAGONPLUS (TM) video now'        fcb   	$0D SwitchMessLen	equ	*-SwitchMessstart   equ   	*        lbsr  	PrintNewLine	leax  	>SignonMess,pcr		* Display signon message        ldy   	#SignonMessLen        lda   	#$01        os9   	I$WritLn 	        lbcs  	ErrorTerminate		* Error, exit imediatly        lbsr  	PrintNewLine		        lbsr  	GetKbVidioAddr		* Get address of (already loaded kbdvidio).        	lda   	#$01						leax  	>SRecordData,pcr	* Point to S-Record data in memory        stx   	<CopyBuffSrcAddr,u	* Save in src addr	        sta   	u0003,u        leax  	>BinBuff,u        stx   	<BinBuffPtr,uFindSrecordHead   	leax  	SRecHeadBuff,u		* destination address, header buff byte        ldy   	#$0001			* Process 1 byte        lda   	u0003,u			* Humm why do this, a is overwritten in CopyBytes ???        lbsr  	CopyBytes        lbcs 	ErrorTerminate        ldb   	SRecHeadBuff,u		* Get copied byte        cmpb  	#'S'			* is this begining of record ?        bne  	FindSrecordHead		* no : do next	* if we reach this point we have found the beginning of an S record	        leax  	SRecTypeBuff,u		* Point to buffer for record type	        ldy   	#$0001			* process 1 byte        lda   	u0003,u        lbsr  	CopyBytes		* get it        lbcs  	ErrorTerminate		* error : exit        	ldb   	SRecTypeBuff,u		* Check record type        cmpb  	#'1'			* Type 1 record ?	        lbne  	SwitchOver		* If not type 1 record we have done all	        leax  	SRecCountBuff,u		* Point to count buffer        ldy   	#$0002			* Fetch 2 bytes        lda   	u0003,u	        lbsr  	CopyBytes		* Fetch count        lbcs  	ErrorTerminate	        ldd   	SRecCountBuff,u		* Get Hex byte count        lbsr  	HexDtobinB		* Convert to binary        tfr   	d,y			* Set as new byte count        stb   	SRecCount,u		* Save in count buffer	        leay  	d,y			* Set count of bytes to fetch        leax  	AddrHiBuff,u        lda   	u0003,u        lbsr  	CopyBytes		* Fetch rest of srecord        lbcs  	ErrorTerminate        	ldd   	AddrHiBuff,u		* Get high part of address and convert to binary        lbsr  	HexDtobinB        stb   	AddrHi,u	        ldd   	AddrLoBuff,u		* Get low part of address and convert to binary        lbsr  	HexDtobinB        stb   	AddrLo,u	* Format the above read ASCII data, as binary	        ldx   	<BinBuffPtr,u		* get pointer to binary buffer        ldb   	SRecCount,u		* Get byte count        decb  				* skip over address        decb          decb  				* and ignore checksum byte        stb   	SRecCount,u		* update count        stb   	,x+			* save count in buffer        ldd  	AddrHi,u		* get address offset        addd  	KbVidioAddress,u	* add base of kbdvidio        std   	AddrHi,u		* save it back        std   	,x++			* save in binary buffer        stx   	<BinBuffPtr,u		* save buffer pointer        ldx   	<BinBuffPtr,u        leay  	DataBuff,u		* Point to loaded ascii hex s-record dataL010E   ldd   	,y++			* Get a byte's worth of hex digits         lbsr  	HexDtobinB		* convert them to binary        stb   	,x+			* save in binary buffer        stx   	<BinBuffPtr,u		* update pointer        clr   	,x			* make sure binary copy always zero terminated        pshs 	x			* increment Addr pointer        ldx   	AddrHi,u        ldb   	,x+        stx   	AddrHi,u        puls  	x        dec   	SRecCount,u		* Decrement record data counter        bne   	L010E			* more data : continue	lbra  	FindSrecordHead		* Else find next s record header	 SwitchOver   	leax  	>SwitchMess,pcr		* Instruct user to switch to Plus video        ldy   	#SwitchMessLen        lda   	#$01        os9   	I$WritLn * This does the actual patching of kbvidio, from the binary copy of the s-record.	leax  	>BinBuff,u		* point to binary buffer        pshs  	cc			* save status        orcc  	#$50			* disable inturrupts while we patchRecordPatchLoop   	ldb   	,x+			* get byte count        beq   	PatchDone		* if zero, we have finished patching        ldy   	,x++			* get address to start patchingBytePatchLoop   	lda   	,x+			* copy a byte from patch buffer         sta   	,y+			* to kbvidio        decb  				* decrement count        bne   	BytePatchLoop		* all done ?        bra   	RecordPatchLoopPatchDone   	puls  	cc        lbsr  	PrintNewLine        clrb  ErrorTerminate   	os9   	F$Exit * Convert hex digits in A:B into binary equivelent in BHexDtobinB   	bsr   	HexBtobinA	* Convert Most significant nibble        lsla  			* Mul by 16, to move into top 4 bits        lsla          lsla          lsla          pshs  	a		* Save MSN        bsr   	HexBtobinB	* Convert Least significant nibble        clra  			* Zero a        addb  	,s+		* Recombine, to make binary in B        rts   				* Convert Hex digit in B to it's binary equivelent, and return in A	HexBtobinA   	exg   	a,b		*        bsr   	HexBtobinB        exg   	a,b        rts   * Convert Hex digit in B to it's binary equivelent, and return in BHexBtobinB   	cmpb  	#$39        bhi   	L0175        andb  	#$0F        rts   	L0175   andb  	#$07        addb  	#$09        rts   PrintNewLine   	pshs  	y,x,b,a		* save regs        ldy   	#$0001		* Write 1 byte        leax  	,u		* Begining of data area (buffer)        ldb   	#$0D		* Carrage Return        stb   	,x		* save b in buffer        bsr   	PrintChars	* Output byte        puls  	pc,y,x,b,a	* Restore and return* Output a character(s) pointed to by X, count in Y PrintChars   	lda   	#$01		* output channel        os9   	I$WritLn 	* Write string        lbcs  	ErrorTerminate	* Error: exit program        rts   	* Copy Y bytes from addr in CopyBuffSrcAddr to X, if a zero byte is reached, return errorCopyBytes	pshs  	u,y,x		* Save regs        ldu   	<CopyBuffSrcAddr,u * Get address of data to copyCopyBytesLoop   	lda   	,u+		* Get a byte from source        sta   	,x+		* store in dest         beq   	L01AC		* Zero byte: yes set error        leay  	-$01,y		* decrement character count        bne   	CopyBytesLoop	* do next        tfr   	u,d		* Save end address        puls  	u,y,x		* restore regs        std   	<CopyBuffSrcAddr,u * update CopyBuffSrcAddr, with new next free        clrb  			* Flag no error        rts   			* Return	L01AC   ldb   	#$D3		* Return EOF error        puls  	pc,u,y,x	* restore and return	        lda   	#$01        os9   	I$Write          bcs   	ErrorTerminate        rts   	L01B8   pshs  	y,x,b,a        leax  	,u        ldy   	#$0001        sta   	,x        lda   	#$01        os9   	I$Write          bcs   	ErrorTerminate        puls  	pc,y,x,b,a        	lda   	#$20L01CD   bsr   	L01B8        decb          bne   	L01CD        rts   	L01D3   exg   	a,b        bsr   	L01DC        exg   	a,b        rts           	bsr   	L01D3	L01DC   pshs  	y,x,b,a        lsrb          lsrb          lsrb          lsrb          leax  	>L01F4,pcr        lda   	b,x        bsr   	L01B8        ldb   	$01,s        andb  	#$0F        lda   	b,x        bsr   	L01B8        puls  	pc,y,x,b,aL01F4   fcc	'0123456789ABCDEF'	* Hex table KBVidioModName   	fcc	'KBVDI'	fcb	'O'+$80	* GetKbVidioAddr, links to kbdvidio module to determine it's address* and then unlinks it.	GetKbVidioAddr   	pshs  u,y,x,b,a			* Save regs        leax  >KBVidioModName,pcr	* point to module name        bsr   DoLink			* Link it        lbcs  ErrorTerminate		* Error: end program        stx   KbVidioAddress,u		* Save module address        bsr   DoUnLink			* unlink it        puls  pc,u,y,x,b,a		* restore and return		fcc	'GO80 COPYRIGHT (C) COMPUSENSE LIMITED 1985'	fcb	$0d* DoLink, Links in module pointed to by X, header addr returned in XDoLink  pshs  	u        lda   	#$E1		* Module type Pysical device ($E0) + 6809 code ($01)        os9   	F$Link   	* Attempt to link the module        tfr   	u,x		* Return module header address in X        puls  	pc,u		* Restore and return* DoUnLink, unlink module who's header pointed to by XDoUnLink   	pshs  	u		* save regs        tfr   	x,u		* Get address into U        os9   	F$UnLink 	* unlink it        puls  	pc,u		* Restore and return	 * The rest of this module is the data below, this is a Motorola S record file !!!* This is converted to binary and used to patch kbvidio, WHY Copusense did this I (currently)* have no idea.	 SRecordData		fcc	'S123002739003442EEC8222706CC1800103F29EE616FC8226FC8236FC81F6FC8396FC8386E'	fcc	'S12300476C8CDE1707731707538D0235C24F308D0012E680FDFFE04C811026F68E0000AFA1'	fcc	'S11700678D06043971505D37191E1819A20A600A00000000DE'	fcc	'S123011D6D8DFF07260317FF03E6C81F2636811B26056CC81F5F3981202506817F2402208F'	fcc	'S123013D31308D06A16D8426066FC81F16059FA180260DEC84308D068D308BAFC8206E8484'	fcc	'S123015D300220E16CC81F308D068EC10127D66ED8206CC83217051BA6C8244C81502515FC'	fcc	'S123017D6FC824A6C8254C811825051705952008A7C8252003A7C8246FC833ECC824EDC847'	fcc	'S123019D3017054A6AC8325F398EFF20C664A68488C0A78486194A121226FB5A26F016FF85'	fcc	'S12301BD886AC8242A10864FA7C8246AC8252A066FC82517055F1700CA16FF6DA6C8254CFE'	fcc	'S10701DD8118250557'	fcc	'S12301E117053C2003A7C82520E86FC82420E36CC8321705CA4F5F6FC833EDC8306AC832D8'	fcc	'S123020116FF426CC8328D066AC83216FF376FC83316052F6CC8323411170453AC8D044D82'	fcc	'S12302212605351116FFCB8620C6011A50F7FFE28C080025038E0000AC8D04312704A780AF'	fcc	'S123024120EE7FFFE235116AC83216FEF86FC8246FC82516FF7CE6C81FC00226025F395A84'	fcc	'S1230261260B81502502864FA7C8245F39811825028617A7C82516FF596CC824A6C8248186'	fcc	'S12302815025F36FC82416FF4F86FFA7C83816FEB44F20F786FFA7C83916FEA94F20F7EC9C'	fcc	'S10702A1C8246CC835'	fcc	'S12302A5326DC8332605EDC83020203406ECC8306CC8345D2B07C1182403EDC8243506ED35'	fcc	'S11302C5C824EDC8306FC8336AC8346AC8325F3988'	fcc	'S1080502121212121296'	fcc	'S123066E000034068D0684071F013586A6C825E6C8243404C6503D1F01E6E03A1F10E38C87'	fcc	'S123068EDF39341FE6C839EAC83827028A808DD21A50C601F7FFE2A7847FFFE2359F3405D9'	fcc	'S12306AE8D08A68430018D16358534161A5086018EFFE2C601E58426FCB7FFE23596341632'	fcc	'S12306CE4F20ED34278DE3108EFFE2C601E5A427FCE5A426FCA78430018DE335A73416173B'	fcc	'S12306EEFF8A843F1F01860EB7FFE01F10B7FFE1860FB7FFE0F7FFE135968407ED8DFF605B'	fcc	'S123070E860CE68DFF5AFDFFE04CE68DFF53FDFFE03934775F8D44EC8DFF45C300508DDA55'	fcc	'S107072EC61720398D'	fcc	'S12307323477C6178D31EC8DFF328300508DC75F2027347717FF27C650E0C8241A5086012B'	fcc	'S1230752B7FFE286208C080025038E0000A7805A26F37FFFE235F734774F1E8917FF0F848B'	fcc	'S1230772071F01C65020D5474F38302028632920434F5059524947485420434F4D5055538F'	fcc	'S1230792454E5345204C494D49544544203139383534218E0013318CCE1A508601B7FFE2EF'	fcc	'S12307B2C62AA6A0A7805A26F97FFFE235A18E0000862034011A50C601F7FFE2A7808C08EA'	fcc	'S12307D20025F97FFFE23501CC0000EDC82416FF2507F9C308F9DB0AF9F60DFA080CFA0DB6'	fcc	'S10707F20BFA6B008F'	fcc	'S12207F641FA7442FA2143FA9744F9E545F9F646FAA747FAAF48FAB249FABA4AFA32006C'	fcc	'S9'	fcb	0,$FB,$57,$48	         emodeom      equ   *         end