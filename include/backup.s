
; ------------------------------------------
; init scroll 16x16
scrollInit:
		move.l	mem_bss_chip(pc),a6
		move.l	a6,d0										; set scroll bpl addr
		addi.l	#Scroll_buffer,d0
		move.l	d0,a3
		lea		copper_scroll_scr_bpls,a2
		move	d0,6(a2)
		swap	d0
		move	d0,2(a2)		

		move.l	mem_bss_public(pc),a5
		adda.l	#Font_16_aligned,a5
		move.l	a5,a2
		lea		font_16_15_1,a4
		moveq	#0,d0
		moveq	#59-1,d7							; nr of fonts
.fntPrep:											; align fonts with all words of each letter next to each other
		.FOFS:	SET 0
		REPT	15
		move	.FOFS(a4),(a5)+
		.FOFS:	SET .FOFS+118
		ENDR
		move	d0,(a5)+
		lea		2(a4),a4
		dbf		d7,.fntPrep

		lea		scroll_text,a1
	move.b	#0,20(a1)
.siLoop:
		moveq	#0,d0
		move.b	(a1)+,d0
		beq		.siEnd
		subi	#32,d0				; space
		lsl		#5,d0
		lea		(a2,d0.w),a4
		.FOFS:	SET 0
		REPT	15
		move	(a4)+,.FOFS(a3)
		.FOFS:	SET .FOFS+SCROLL_LEN
;		.FOFS:	SET .FOFS+40
		ENDR
		lea		2(a3),a3
		bra.s	.siLoop
.siEnd:
		rts


