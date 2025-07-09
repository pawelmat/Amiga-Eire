; Eire - 40k intro for the Boom 2025 party, Tuchola, Poland
; Pawel Matusz / Kane (kane@konto.pl)
; 25/06/2025 - TBD

    TTL         "Eire"

; ----------- Constants

;MUSIC_ON:		equ		1					; comment out for no music
MUS_BPM:		equ		125					; music BPL
MUS_TPB:		equ		(3000/MUS_BPM)		; ticks per beat (24)
MUS_TPN:		equ		(MUS_TPB/8)			; ticks per note (3)

; ----------- Includes

    INCDIR      "include"
    INCLUDE     "custom.i"
    INCLUDE     "macros.i"

; ----------- CODE section
    code
s:
        movem.l d0-a6,-(sp)
        bsr     OsSave

		bsr 	initMusic
		lea		CUSTOM,a0
		clr		d0
       	move    d0,FMODE(a0)			; make sure AGA is not messing around on such machines
       	move    d0,BPLCON3(a0)
       	move    d0,BPLCON4(a0)

        lea     configBlock(pc),a6
		move.l	VBR_BASE(a6),a1
        lea     interruptL3(pc),a2		; L3 interrupt (for VB, Copper and Blitter)
		move.l	a2,$6c(a1)

		lea		mem_bss_public(pc),a2	; store allocated memory addresses locally to be able to get them by (pc) reference
		lea		mbp,a1
		move.l	a1,(a2)
		lea		mem_bss_chip(pc),a2
		lea		mbc,a1
		move.l	a1,(a2)
		lea		mem_data_public(pc),a2
		lea		mdp,a1
		move.l	a1,(a2)
		lea		mem_data_chip(pc),a2
		lea		mdc,a1
		move.l	a1,(a2)

		lea		copper_base-mdc(a1),a2
		move.l	a2,COP1LC(a0)
		lea		copper_blank_black-mdc(a1),a2
		move.l	a2,COP2LC(a0)
		move	#0,COPJMP1(a0)
		move	#INTF_SETCLR|INTF_INTEN|INTF_VERTB|INTF_COPER,INTENA(a0)		; enable selected interrupts
		move	#DMAF_SETCLR|DMAF_DMAEN|DMAF_BPLEN|DMAF_COPEN|DMAF_BLTEN,DMACON(a0)

		;moveq	#11,d0
		;bsr		setMusicPos

;		bsr		swipeScreenAtStart
;		bsr		eirePart
		bsr		scrollPart

		move.l	mem_data_chip(pc),a1
		lea		copper_blank_purple-mdc(a1),a2
		move.l	a2,COP2LC(a0)
		lea		sl(pc),a2
infiniLoop:
		VBLANKNL 30
		tst		beat_strobe-sl(a2)
		beq		.noStrobe
		move	#$e5e,COLOR00(a0)
.noStrobe:
		btst.b    #6,CIAA
        bne.s     infiniLoop

        ;TESTLMB

exit:
        bsr     OsRestore
        movem.l (sp)+,d0-a6
        clr     d0                      ; exit code
        rts

;-----------------------------------------------------------------------
; Main L3 interrupt
interruptL3:
		btst.b	#5,CUSTOM+INTREQR+1		; Which L3 interrupt was raised?
		bne.s	interruptL3Vertb
		btst.b	#6,CUSTOM+INTREQR+1
		beq.s	interruptL3Copper

;blitter finished interrupt
		move	#$40,CUSTOM+INTREQ		; clear BLIT INTREQ
		move	#$40,CUSTOM+INTREQ		; double just in case to prevent any error in emu or fast CPU from calling the int again too fast
		nop
		rte

; copper L3 interrupt
interruptL3Copper:
		movem.l	d0-d2/a0-a6,-(sp)
		ifd	MUSIC_ON
		lea		CUSTOM+AUD0LCH,a6		; always set a6 to dff0a0 before calling LSP tick
		bsr		LSP_MusicPlayTick		; player music tick
		endif

		lea		sl(pc),a0
		tst		beat_strobe-sl(a0)
		beq.s	.noStrobe
		clr		beat_strobe-sl(a0)
		bra.s	.noBeat					; can skip beat checking right after strobe as that means it's the very next frame, so beat not possible
.noStrobe
		move	beat_next(pc),d0
		cmp		tick_cnt-sl(a0),d0
		bne.s	.noBeat
		addq	#1,beat-sl(a0)				; count beats
		addi	#MUS_TPB,beat_next-sl(a0)	; move to next beat
		move	beat-sl(a0),beat_strobe-sl(a0)
.noBeat:
		addq	#1,tick_cnt-sl(a0)			; increase tick (frame) counter


		movem.l	(sp)+,d0-d2/a0-a6
		move	#$10,CUSTOM+INTREQ		; clear COPER INTREQ
		move	#$10,CUSTOM+INTREQ		; double just in case to prevent any error in emu or fast CPU from calling the int again too fast
		nop
		rte

;vertical blank interrupt
interruptL3Vertb:
		move	#$20,CUSTOM+INTREQ
		move	#$20,CUSTOM+INTREQ		; double just in case to prevent any error in emu or fast CPU from calling the int again too fast
		nop
		rte

;-----------------------------------------------------------------------
; Init LSP player
initMusic:
		move	#$7fff,CUSTOM+ADKCON
		ifd	MUSIC_ON
		lea		LSP_Music,a0
		lea		LSP_Bank,a1
		lea		copper_DMAConPatch+3,a2
		bsr		LSP_MusicInit
		endif
		rts

; Set music position and recalc related variables. Each pos has 8 beats (24 ticks each) and each beat 8 notes (3 ticks each).
; in: d0 - seq position (from 0 to last seq of the song)
setMusicPos:
		movem.l	d1-d2/a0,-(sp)
		move	#MUS_TPN*64,d1			; length of one pos (64 notes) in ticks
		mulu	d0,d1
		lea		sl(pc),a0
		move 	d1,tick_cnt-sl(a0)
		move	d0,d2
		lsl		#3,d2					; 8 beats per pos
		move 	d2,beat-sl(a0)
		addi	#MUS_TPB,d1
		move 	d1,beat_next-sl(a0)
		bsr		LSP_MusicSetPos			; move actual mus
		movem.l	(sp)+,d1-d2/a0
		rts

;-----------------------------------------------------------------------
;-----------------------------------------------------------------------
; 2-stage screen transition (left right and then centre->up/down) at the start
START_COPPER_LINES = 272
swipeScreenAtStart:
		move.l	mem_bss_chip(pc),a1
		lea		Copper_start_swipe(a1),a2
		lea		Copper_start_wait_addr(a1),a3
		lea		(a2),a5
		move.l	#$2039fffe,d0
		move.l	#(COLOR00<<16),d1
		move.l	#$208ffffe,d2
		move.l	#(COLOR00<<16),d3
		move.l	#$01000000,d4
		move	#START_COPPER_LINES-1,d7
.buildCopper:
		move.l	a2,(a3)+					; remember wait addresses left
		move.l	d0,(a2)+
		add.l	d4,d0
		andi	#$fffe,d0
		move.l	d1,(a2)+
		move.l	d2,(a2)+
		add.l	d4,d2
		andi	#$fffe,d2
		move.l	d3,(a2)+
		move.l	d2,d5
		andi.l	#$ff000000,d5
		bne.s	.noPalWait
		move.l	#$ffdffffe,(a2)+
.noPalWait:
		dbf		d7,.buildCopper
		move.l	#-2,(a2)	

		lea		CUSTOM,a0
		WAIT	30
		move.l	a5,COP2LC(a0)

		move	#22-1,d7
		move	#$8f-(21*4),d0
.pass1:	move	#(START_COPPER_LINES)-1,d6				; left->right
		lea		Copper_start_wait_addr(a1),a3
.l1:	move.l	(a3)+,a5
		move	#$313,6(a5)
		move.b	d0,9(a5)
		dbf		d6,.l1
		VBLANKNL 310
		addq	#4,d0
		dbf		d7,.pass1

		move	#23-1,d7
		lea		Copper_start_wait_addr+(((START_COPPER_LINES/2))*4)(a1),a3
		lea		(a3),a4
.pass2:	moveq	#5,d6									; middle -> up/down
.l2:	move.l	-(a3),a5
		move	#$313,14(a5)
		move.l	(a4)+,a5
		move	#$313,14(a5)
		dbf		d6,.l2
		VBLANKNL 310
		dbf		d7,.pass2
		rts

;-----------------------------------------------------------------------
;-----------------------------------------------------------------------
eirePart:
		lea		CUSTOM,a0
		lea		logo_Eire,a1
		lea		copper_eire_logo_bpls,a2
		moveq	#EIRE_BPL-1,d0
		moveq	#(EIRE_SIZE_X/8),d1
		bsr		setLogoAddr

		lea		copper_eire,a2
		move.l	a2,COP2LC(a0)
		VBLANK

		lea		copper_eire_logo_cols,a1
		lea		logo_eire_palette,a2
		moveq	#2,d0
		bsr		fadeColorsIn

		TESTLMB

		bsr		fadeColorsOut

		rts


;-----------------------------------------------------------------------
;-----------------------------------------------------------------------
scrollPart:
		lea		CUSTOM,a0
		bsr		logoTransformPrecalc						; prepare logo anims

		move.l	mem_bss_chip(pc),a6
		lea		Logo_trans_buffer(a6),a1
		lea		Logo_screen1(a6),a2
		move.l	#LOGO_SIZE/4-1,d7
.cpl1:	move.l	(a1)+,(a2)+
		dbf		d7,.cpl1

		lea		Logo_screen1(a6),a1							; chip screen buffer with logo
		lea		copper_scroll_logo_bpls,a2
		moveq	#LOGO_BPL-1,d0
		moveq	#(LOGO_SIZE_X/8),d1
		bsr		setLogoAddr

		lea		copper_scroll,a2
		move.l	a2,COP2LC(a0)
		VBLANK

		lea		copper_scroll_logo_cols,a1
		lea		base_purple_palette,a2
		lea		logo_suspect_palette,a3
		moveq	#2,d0
		bsr		transformColors

; loop through the pictures

		lea		CUSTOM,a0 ; not needed?
		lea		logo_trans_frames(pc),a5
		moveq	#0,d0							; frame
		moveq	#4,d2							; directional add
.loop:	WAIT	4
	move #$0f0,COLOR00(a0)
		add		d2,d0

;	move #$f00,COLOR00(a0)
		move.l	(a5,d0.w),a1					; frame addr
		lea		Logo_screen1(a6),a2
		move.l	#LOGO_SIZE/16-1,d6
.cpl2:	rept	8
		move.w	(a1)+,(a2)+
		endr
		dbf		d6,.cpl2

		cmpi	#(LOGO_TRANS_NR-1)*4,d0
		beq.s	.negDir
		cmpi	#0,d0
		bne.s	.noLoop
.negDir:
		neg		d2
		WAIT	50
.noLoop:
	move #$000,COLOR00(a0)

		btst.b    #6,$bfe001
        bne	     .loop

		lea		copper_scroll_logo_cols,a1
		lea		logo_suspect_palette,a2
		lea		base_purple_palette,a3
		moveq	#2,d0
		bsr		transformColors

		rts


; ------------------------------------------
; Pre-calc log transformaiton
logoTransformPrecalc:
		movem.l	ALL,-(sp)
		lea		logo_trans_frames(pc),a5				; table of all the frames

		lea		logo_Suspect,a1
		move.l	a1,(a5)									; first frame
		move.l	mem_bss_public(pc),a6
		lea		Logo_chunky_Suspect(a6),a2
		bsr		logoToChunky

		lea		logo_Scoopex,a1
		move.l	a1,(LOGO_TRANS_NR-1)*4(a5)					; last frame
		lea		Logo_chunky_Scoopex(a6),a2
		bsr		logoToChunky

		; create anim frames
		lea		4(a5),a5							; start from frame 2
		lea		Logo_scale_tab(a6),a4
		move.l	mem_bss_chip(pc),a6
		lea		Logo_trans_buffer(a6),a6			; bitplanes destination
		moveq	#1,d7								; scaling factor
.frameLoop:
	move #$f00,CUSTOM+COLOR00
		moveq	#0,d3
		moveq	#30,d4
		moveq	#-15,d0								; only up to +/- 15 is possible
.st1:	move	d0,d2								; build scaling tab for this factor
		muls	d7,d2
		lsr.w	#4,d2								; * n/16
		move.b	d0,d3
		move.b	d2,(a4,d3.w)
		addq	#1,d0
		dbf		d4,.st1

		move.l	mem_bss_public(pc),a3
		lea		Logo_chunky_Suspect(a3),a1
		lea		Logo_chunky_Scoopex(a3),a2
		adda.l	#Logo_chunky_buffer,a3

		moveq	#1,d6								; interpolate colors for this scaling factor
		moveq	#0,d1
		move.l	#LOGO_SIZE_X*LOGO_SIZE_Y/4-1,d6
.colInterp:
		rept	4
		move.b	(a1)+,d0							; suspect logo texel
		move.b	(a2)+,d1							; scoopex logo texel
		sub.b	d0,d1
		add.b	(a4,d1.w),d0
		move.b	d0,(a3)+
		endr
		dbf		d6,.colInterp

		move.l	mem_bss_public(pc),a0
		adda.l	#Logo_chunky_buffer,a0				; c2p for this scaling factor
		move.l	a6,a1								; bitplanes destination
		move.l	#LOGO_SIZE_X*LOGO_SIZE_Y,d0			; size of the chunky source 
		bsr		c2p1x1_4_c5

		move.l	a6,(a5)+							; save current frame and move to next one
		adda.l	#LOGO_SIZE,a6

	move #$000,CUSTOM+COLOR00
		addq	#1,d7
		cmpi	#16,d7
		bne		.frameLoop
		movem.l	(sp)+,ALL
		rts


; ------------------------------------------
; a1 - logo addr, a2 - chunky buffer
logoToChunky:
		moveq	#LOGO_SIZE_Y-1,d7
.loopY:	moveq	#(LOGO_SIZE_X/8)/4-1,d6
		move.l	a1,a3
.loopX:	move.l	LOGO_SIZE_X/8(a3),d1
		move.l	2*LOGO_SIZE_X/8(a3),d2
		move.l	3*LOGO_SIZE_X/8(a3),d3
		move.l	(a3)+,d0
		moveq	#32/4-1,d5
.conv:	
		moveq.l	#0,d4						; convert 4 bits
		lsl.l	d3
		roxl.w	d4
		lsl.l	d2
		roxl.w	d4
		lsl.l	d1
		roxl.w	d4
		lsl.l	d0
		roxl.w	d4
		lsl.w	#4,d4
		lsl.l	d3
		roxl.w	d4
		lsl.l	d2
		roxl.w	d4
		lsl.l	d1
		roxl.w	d4
		lsl.l	d0
		roxl.w	d4

		swap	d4
		lsl.l	d3
		roxl.w	d4
		lsl.l	d2
		roxl.w	d4
		lsl.l	d1
		roxl.w	d4
		lsl.l	d0
		roxl.w	d4
		lsl.w	#4,d4
		lsl.l	d3
		roxl.w	d4
		lsl.l	d2
		roxl.w	d4
		lsl.l	d1
		roxl.w	d4
		lsl.l	d0
		roxl.w	d4

		move.l	d4,(a2)+
		dbf		d5,.conv
		dbf		d6,.loopX
		lea		(LOGO_SIZE_X/8)*LOGO_BPL(a1),a1		; move to next Y row
		dbf		d7,.loopY
		rts


; ------------------------------------------
; Set logo adddr
; a1 - logo addr, a2 - copper bpl, d0 - nr bitplanes -1, d1.l - row in bytes
setLogoAddr:
		move.l	a1,d2
.sa1:	move	d2,6(a2)
		swap	d2
		move	d2,2(a2)
		swap	d2
		add.l	d1,d2
		lea		8(a2),a2
		dbf		d0,.sa1
		rts

; ------------------------------------------
; fade colours in
; a1 - copper colour list
; a2 - colour tab preceded by length
; d0 - speed (0 - fastest)
fadeColorsIn:
		movem.l	ALL,-(sp)
		lea		CUSTOM,a0
		move	d0,d3
		moveq	#0,d0		; scaling factor
		movem.l	a1/a2,-(sp)
.loop:	movem.l	(sp),a1/a2
		bsr.s	scaleColors
		move	d3,d7
.wait:	VBLANK
		dbf		d7,.wait
		addq	#1,d0		; increase scaling factor
		cmpi	#17,d0
		bne.s	.loop
		movem.l	(sp)+,a1/a2
		movem.l	(sp)+,ALL
		rts


; ------------------------------------------
; fade colours out
; a1 - copper colour list
; a2 - colour tab preceded by length
; d0 - speed (0 - fastest)
fadeColorsOut:
		movem.l	ALL,-(sp)
		lea		CUSTOM,a0
		move	d0,d3
		moveq	#16,d0		; scaling factor
		movem.l	a1/a2,-(sp)
.loop:	movem.l	(sp),a1/a2
		bsr.s	scaleColors
		move	d3,d7
.wait:	VBLANK
		dbf		d7,.wait
		dbf		d0,.loop	; decrease scaling factor, last run will be with 0
		movem.l	(sp)+,a1/a2
		movem.l	(sp)+,ALL
		rts

; set colours
; a1 - copper colour list
; a2 - colour tab preceded by length
setColors:
		movem.l	ALL,-(sp)
		moveq	#16,d0		; scaling factor
		bsr.s	scaleColors
		movem.l	(sp)+,ALL
		rts

; a1 - copper colour list
; a2 - colour tab preceded by length
; d0 - scaling factor (0-16 inclusive)
scaleColors:
		move	(a2)+,d6	; nr of colours
		subq	#1,d6

.scale:	moveq	#0,d5
		move	(a2)+,d1
		move	d1,d7
		andi	#$f,d1
		mulu	d0,d1
		lsr.l	#4,d1
		andi	#$f,d1
		move	d1,d5

		move	d7,d1
		andi	#$f0,d1
		mulu	d0,d1
		lsr.l	#4,d1
		andi	#$f0,d1
		or		d1,d5
		
		move	d7,d1
		andi	#$f00,d1
		mulu	d0,d1
		lsr.l	#4,d1
		andi	#$f00,d1
		or		d1,d5

		move	d5,2(a1)
		lea		4(a1),a1
		dbf		d6,.scale
		rts

; ------------------------------------------
; fade colours in
; a1 - copper colour list
; a2 - colour tab preceded by length
; a3 - target colour tab preceded by length
; d0 - speed (0 - fastest)
transformColors:
		movem.l	ALL,-(sp)
		lea		CUSTOM,a0
		movem.l	a1/a2/a3/d0,-(sp)
		moveq	#0,d0		; scaling factor
.loop:	movem.l	(sp),a1/a2/a3/d7
		bsr.s	scaleColorsDiff
;		move	d3,d7
.wait:	VBLANK
		dbf		d7,.wait
		addq	#1,d0		; increase scaling factor
		cmpi	#17,d0
		bne.s	.loop
		movem.l	(sp)+,a1/a2/a3/d0
		movem.l	(sp)+,ALL
		rts

; a1 - copper colour list
; a2 - source colour tab preceded by length
; a3 - target colour tab preceded by length
; d0 - scaling factor (0-16 inclusive)
scaleColorsDiff:
		move	(a2)+,d6	; nr of colours
		lea		2(a3),a3
		subq	#1,d6

.scale:	moveq	#0,d5
		move	(a2)+,d1	; src
		move	(a3)+,d2	; target

		move	d1,d3
		move	d2,d4
		andi	#$f,d1
		andi	#$f,d2
		sub		d1,d2
		muls	d0,d2
		asr.l	#4,d2
		add		d2,d1
		andi	#$f,d1
		move	d1,d5

		move	d3,d1
		move	d4,d2
		andi	#$f0,d1
		andi	#$f0,d2
		sub		d1,d2
		muls	d0,d2
		asr.l	#4,d2
		add		d2,d1
		andi	#$f0,d1
		or		d1,d5

		move	d3,d1
		move	d4,d2
		andi	#$f00,d1
		andi	#$f00,d2
		sub		d1,d2
		muls	d0,d2
		asr.l	#4,d2
		add		d2,d1
		andi	#$f00,d1
		or		d1,d5

		move	d5,2(a1)
		lea		4(a1),a1
		dbf		d6,.scale
		rts

; ----------- Include other code files

    INCLUDE     "os.s"
	INCLUDE		"LightSpeedPlayer.s"
	INCLUDE		"c2p.s"


; ----------- Local data which can be (pc) referenced
sl:										; state_local
tick_cnt:				dc.w	0			; current tick
beat:					dc.w	0			; current beat (absolute nr from 0=first at pos00 note 00, increases every MUS_TPB)
beat_strobe				dc.w	0			; beat strobe, lit for 1 frame at the start of the beat with the nr of the beat
beat_next:				dc.w	MUS_TPB		; next beat in ticks

mem_data_chip:			dc.l	0			; addresses of allocated memory regions
mem_data_public:		dc.l	0
mem_bss_chip:			dc.l	0
mem_bss_public:			dc.l	0

logo_trans_frames:		dcb.l	LOGO_TRANS_NR,0		; transition frame pointers and counters
logo_trans_cnt:			dcb.b	LOGO_TRANS_NR,0
logo_trans_cnt_last:	dcb.b	LOGO_TRANS_NR,0

end:
    ;echo 		"Total code length: ", (end-s)

; ------------------------- DATA CHIP section ---------------------------------
    data_c
	EVEN
mdc:
; ----------- Base copper header, which fires the copper interrupt playing the music. 
; In COP1LC and not to be touched. All demo copperlists use COP2LC.
copper_base:
		dc.w	(4<<8)|$09, $fffe, INTREQ, INTF_SETCLR|INTF_COPER		; start copper interrupt
		dc.w	((4+11)<<8)|$09, $fffe 									; wait for scanline +11 and set audio DMA
copper_DMAConPatch:
		dc.w	DMACON, $8000
		dc.w	COPJMP2, 0

; ----------- Blank coppers
copper_blank_black:
       	dc.w    COLOR00, $000, BPLCON0, $0200  						; 0 bitplanes
		dc.l	-2
copper_blank_purple:
       	dc.w    COLOR00, $313, BPLCON0, $0200  						; 0 bitplanes
		dc.l	-2

; ----------- Eire screen coppers
copper_eire:
		dc.w	BPLCON0, $0200  										 ; 0 bitplanes
		dc.w	DIWSTRT, $6Ce1, DIWSTOP, $0061
		dc.w	DDFSTRT, $0068, DDFSTOP, $00a0
copper_eire_logo_cols:
		dc.w	COLOR00,0,COLOR01,0,COLOR02,0,COLOR03,0,COLOR04,0,COLOR05,0,COLOR06,0,COLOR07,0
		dc.w	COLOR08,0,COLOR09,0,COLOR10,0,COLOR11,0,COLOR12,0,COLOR13,0,COLOR14,0,COLOR15,0
		dc.w	BPL1MOD,(EIRE_SIZE_X/8)*(EIRE_BPL-1),BPL2MOD,(EIRE_SIZE_X/8)*(EIRE_BPL-1)
copper_eire_logo_bpls:
		dc.w	BPL1PTH,0,BPL1PTL,0,BPL2PTH,0,BPL2PTL,0,BPL3PTH,0,BPL3PTL,0,BPL4PTH,0,BPL4PTL,0
		dc.w	$6c01,$fffe
		dc.w	BPLCON0, $4200
		dc.w	$cc01,$fffe
		dc.w	BPLCON0, $0200
		dc.l	-2

; ----------- Scroll screen coppers
BC_PURPLE = $313
copper_scroll:
		dc.w	BPLCON0, $0200  										 ; 0 bitplanes
		dc.w	DIWSTRT, $2C81, DIWSTOP, $2CC1
		dc.w	DDFSTRT, $0038, DDFSTOP, $00D0
copper_scroll_logo_cols:
		dc.w	COLOR00,BC_PURPLE,COLOR01,BC_PURPLE,COLOR02,BC_PURPLE,COLOR03,BC_PURPLE,COLOR04,BC_PURPLE,COLOR05,BC_PURPLE,COLOR06,BC_PURPLE,COLOR07,BC_PURPLE
		dc.w	COLOR08,BC_PURPLE,COLOR09,BC_PURPLE,COLOR10,BC_PURPLE,COLOR11,BC_PURPLE,COLOR12,BC_PURPLE,COLOR13,BC_PURPLE,COLOR14,BC_PURPLE,COLOR15,BC_PURPLE
		dc.w	BPL1MOD,(LOGO_SIZE_X/8)*(LOGO_BPL-1),BPL2MOD,(LOGO_SIZE_X/8)*(LOGO_BPL-1)
copper_scroll_logo_bpls:
		dc.w	BPL1PTH,0,BPL1PTL,0,BPL2PTH,0,BPL2PTL,0,BPL3PTH,0,BPL3PTL,0,BPL4PTH,0,BPL4PTL,0
		dc.w	$3001,$fffe, BPLCON0, $4200
;		dc.w	$318f,$fffe, COLOR00, $424, COLOR00, $535, COLOR00, $646, COLOR00, $535, COLOR00, $424, COLOR00, BC_PURPLE
;		dc.w	$328f,$fffe, COLOR00, $424, COLOR00, $535, COLOR00, $646, COLOR00, $535, COLOR00, $424, COLOR00, BC_PURPLE
;		dc.w	$338f,$fffe, COLOR00, BC_PURPLE, COLOR00, $424, COLOR00, $535, COLOR00, $424, COLOR00, BC_PURPLE
;		dc.w	$348f,$fffe, COLOR00, BC_PURPLE, COLOR00, BC_PURPLE, COLOR00, $424, COLOR00, BC_PURPLE
		dc.w	$7001,$fffe, BPLCON0, $0200
		dc.l	-2

	EVEN
LSP_Bank:
	ifd	MUSIC_ON
   	INCBIN		"assets/Bartesek - Hey Simone!.lsbank"
	endif
	EVEN
logo_Suspect:
   	INCBIN		"assets/sct_73_inv.bpl"
	EVEN
logo_Scoopex:
   	INCBIN		"assets/scx_65_inv.bpl"
LOGO_SIZE_X = 320
LOGO_SIZE_Y = 64
LOGO_COLS = 16
LOGO_BPL = 4
LOGO_SIZE = (LOGO_SIZE_X/8)*(LOGO_SIZE_Y)*(LOGO_BPL)
	EVEN
logo_Eire:
   	INCBIN		"assets/eire_128x96.bpl"
EIRE_SIZE_X = 128
EIRE_SIZE_Y = 96
EIRE_COLS = 16
EIRE_BPL = 4

; ------------------------- DATA PUBLIC section ---------------------------------
	data
mdp:
	EVEN
LSP_Music:
	ifd	MUSIC_ON
   	INCBIN		"assets/Bartesek - Hey Simone!.lsmusic"
	endif
	EVEN
logo_suspect_palette:
	dc.w		LOGO_COLS		; nr of colours
   	INCBIN		"assets/sct_73_inv.pal"
base_purple_palette:
	dc.w		LOGO_COLS
	dcb.w		16,BC_PURPLE
logo_eire_palette:
	dc.w		EIRE_COLS
   	INCBIN		"assets/eire_128x96.pal"
font_8:
   	INCBIN		"assets/font.bpl"

; ------------------------- BSS CHIP section ---------------------------------
	bss_c
mbc:
	RSRESET
	Copper_start_swipe:			rs.l	((START_COPPER_LINES+1)*4)+1
	Copper_start_wait_addr:		rs.l	START_COPPER_LINES
	BSS_CHIP_1:					rs.w	0

LOGO_TRANS_NR = 17

	RSRESET
	Logo_screen1:				rs.b	LOGO_SIZE
	Logo_trans_buffer:			rs.b	LOGO_SIZE*LOGO_TRANS_NR
	BSS_CHIP_2:					rs.w	0

BSS_CHIP_ALLOC_MAX set BSS_CHIP_1
	if BSS_CHIP_2 > BSS_CHIP_ALLOC_MAX
BSS_CHIP_ALLOC_MAX set BSS_CHIP_2
	endif

	; echo 		"BSS 1: ", BSS_CHIP_1
	; echo 		"BSS 2: ", BSS_CHIP_2

	ds.b		BSS_CHIP_ALLOC_MAX

; ------------------------- BSS public section ---------------------------------
	bss
mbp:
	RSRESET
	Logo_scale_tab:				rs.b	256
	Logo_chunky_Suspect:		rs.b	LOGO_SIZE_X*LOGO_SIZE_Y
	Logo_chunky_Scoopex:		rs.b	LOGO_SIZE_X*LOGO_SIZE_Y				; do no change the order of this and previous row
;	Logo_chunky_diff:			rs.b	LOGO_SIZE_X*LOGO_SIZE_Y
	Logo_chunky_buffer:			rs.b	LOGO_SIZE_X*LOGO_SIZE_Y
	BSS_PUBLIC_ALLOC_MAX:		rs.w	0

	ds.b		BSS_PUBLIC_ALLOC_MAX

; ------------------------------------------------------------------------------
	echo 		"BSS CHIP alloc: ", BSS_CHIP_ALLOC_MAX
	echo 		"BSS PUB alloc: ", BSS_PUBLIC_ALLOC_MAX

    END