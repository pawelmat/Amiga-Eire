; Eire - 40k intro for the Boom 2025 party, Tuchola, Poland
; Pawel Matusz / Kane (kane@konto.pl)
; 25/06/2025 - TBD

    TTL         "Eire"

; ----------- Constants

MUSIC_ON:		equ		1					; comment out for no music
MUS_BPM:		equ		125					; music BPL
MUS_TPB:		equ		(3000/MUS_BPM)		; ticks per beat (24). 8 beats per pos (1 pos =~4 sec)
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

		; moveq	#20,d0
		; bsr		setMusicPos

		lea		copper_base,a2
		move.l	a2,COP1LC(a0)
		lea		copper_blank_black,a2
		move.l	a2,COP2LC(a0)
		move	#0,COPJMP1(a0)
		move	#INTF_SETCLR|INTF_INTEN|INTF_VERTB|INTF_COPER,INTENA(a0)		; enable selected interrupts
		move	#DMAF_SETCLR|DMAF_DMAEN|DMAF_BPLEN|DMAF_COPEN|DMAF_BLTEN,DMACON(a0)

		lea		sinus_14b_256(pc),a1
		moveq	#63,d7
.mkCos:	move	(a1)+,256*2-2(a1)					; make extend sine to cosine 
		dbf		d7,.mkCos

parts:
;		bsr		swipeScreenAtStart
;		bsr		eirePart
;		bsr		lsystemPart
		bsr		scrollPart
		bsr		endPart

exit:
        bsr     OsRestore
        movem.l (sp)+,d0-a6
        clr     d0                      ; exit code
        rts

;-----------------------------------------------------------------------
; Main L3 interrupt
interruptL3:
		btst.b	#5,CUSTOM+INTREQR+1		; Which L3 interrupt was raised?
		bne		interruptL3Vertb
		btst.b	#6,CUSTOM+INTREQR+1
		beq		interruptL3Copper

;blitter finished interrupt
		movem.l	d0-d1/a0-a2,-(sp)
		lea		CUSTOM,a0
		lea		blitter_queue(pc),a1
		move	(a1),d0
		beq.s	.bExit						; is there a queue to process?
		bsr		blitterQueueProcess			; process the next step in the queue. Returns with Z set it end of queue
		bne.s	.bExit
		move	#INTF_BLIT,INTENA(a0)		; disable blitter L3 interrupt
.bExit:
		movem.l	(sp)+,d0-d1/a0-a2
		move	#$40,CUSTOM+INTREQ		; clear BLIT INTREQ
		move	#$40,CUSTOM+INTREQ		; double just in case to prevent any error in emu or fast CPU from calling the int again too fast
		nop
		rte

; copper L3 interrupt
interruptL3Copper:
		movem.l	d0-d2/a0-a6,-(sp)
		ifd	MUSIC_ON
		lea		music_ticks_left(pc),a1
		tst		(a1)
		beq.s	.skipMus				; play only for the number of ticks set when initialising the music
		subi	#1,(a1)
		lea		CUSTOM+AUD0LCH,a6		; always set a6 to dff0a0 before calling LSP tick
		bsr		LSP_MusicPlayTick		; player music tick
.skipMus:
		endif

		lea		sl(pc),a1
		tst		beat_strobe-sl(a1)
		beq.s	.noStrobe
		clr		beat_strobe-sl(a1)
		bra.s	.noBeat					; can skip beat checking right after strobe as that means it's the very next frame, so beat not possible
.noStrobe
		move	beat_next(pc),d0
		cmp		tick_cnt-sl(a1),d0
		bne.s	.noBeat
		addq.w	#1,beat-sl(a1)				; count beats (absolute counter)
		addq.w	#1,beat_relative-sl(a1)		; count beats (relative counter)
		addi	#MUS_TPB,beat_next-sl(a1)	; move to next beat
		move	beat-sl(a1),beat_strobe-sl(a1)
.noBeat:
		addq	#1,tick_cnt-sl(a1)			; increase tick (frame) counter
		movem.l	(sp)+,d0-d2/a0-a6
		move	#$10,CUSTOM+INTREQ		; clear COPER INTREQ
		move	#$10,CUSTOM+INTREQ		; double just in case to prevent any error in emu or fast CPU from calling the int again too fast
		nop
		rte

;vertical blank interrupt
interruptL3Vertb:
		movem.l	a1/d0,-(sp)
		move.l	intL3Proc(pc),d0
		beq		.noPr
		move.l	d0,a1
		jsr		(a1)
.noPr:	movem.l	(sp)+,a1/d0
		move	#$20,CUSTOM+INTREQ
		move	#$20,CUSTOM+INTREQ		; double just in case to prevent any error in emu or fast CPU from calling the int again too fast
		nop
		rte

; a1 - proc addr
intL3ProcSet:
		lea		intL3Proc(pc),a2
		move.l	a1,(a2)
		rts

intL3ProcClear:
		lea		intL3Proc(pc),a1
		clr.l	(a1)
		rts

intL3Proc:		dc.l	0

;-----------------------------------------------------------------------
; a0 - CUSTOM
blitterQueueStart:
		move	#INTF_SETCLR|INTF_BLIT,INTENA(a0)		; enable blitter L3 interrupt
		lea		blitter_queue(pc),a1
		move	(a1),d0
		bne.s	.qNotEmpty
		rts
.qNotEmpty:
		move	#-2,-2(a1,d0.w)							; end blitter queue
		moveq	#2,d0
		move	d0,(a1)
		WAITBLIT
;		bsr		blitterQueueProcess						; kick off blitter queue processing

; a0 - CUSTOM, a1 - blitter_queue, d0 - queue pointer
blitterQueueProcess:
		move.l	a1,a2
		lea		(a1,d0.w),a1
.bqLoop:
		move	(a1)+,d0
		bmi.s	.bqEnd
		move	(a1)+,(a0,d0.w)
		bra.s	.bqLoop
.bqEnd:	cmpi	#-2,d0
		beq.s	.bqStop
		suba.l	a2,a1
		move	a1,(a2)			; end this batch and move on queue pos
		rts
.bqStop:
		move	#0,(a2)			; queue pos -2 means no processing required
		rts

;-----------------------------------------------------------------------
; Init LSP player
initMusic:
		move	#$7fff,CUSTOM+ADKCON
		ifd	MUSIC_ON
		lea		LSP_Music,a0
		lea		LSP_Bank,a1
		lea		copper_DMAConPatch+3,a2
		bsr		LSP_MusicInit
		lea		music_ticks_left(pc),a1
		move	d0,(a1)					; save music length in ticks
		endif
		rts

; Set music position and recalc related variables. Each pos has 8 beats (24 ticks each) and each beat 8 notes (3 ticks each).
; in: d0 - seq position (from 0 to last seq of the song, e.g. 23)
setMusicPos:
		movem.l	ALL,-(sp)
		move	#MUS_TPN*64,d1			; length of one pos (64 notes) in ticks
		mulu	d0,d1
		lea		sl(pc),a1
		sub		d1,music_ticks_left-sl(a1)	; fix ticks left in music
		move 	d1,tick_cnt-sl(a1)		; fix current tick
		move	d0,d2
		lsl		#3,d2					; 8 beats per pos
		move 	d2,beat-sl(a1)
		addi	#MUS_TPB,d1
		move 	d1,beat_next-sl(a1)
		bsr		LSP_MusicSetPos			; move actual music
		movem.l	(sp)+,ALL
		rts

; d0 - delay after strobe
syncToStrobe:
		lea		beat_strobe(pc),a1
.noStr:	tst		(a1)
		beq.s	.noStr
.delay:	VBLANKS
		dbf		d0,.delay
		rts

; just reset the relative beat for local counting
resetRelativeBeat:
		lea		beat_relative(pc),a1
		move	#0,(a1)
		rts

vBlank:
		VBLANK
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
		lea		logo_eire_palette(pc),a2
		moveq	#2,d0
		bsr		fadeColorsIn

		TESTLMB

		bsr		fadeColorsOut

		rts

;-----------------------------------------------------------------------
;-----------------------------------------------------------------------
LSYS_X = 32
LSYS_Y = 256
LSYS_DISPLACEMENT = 3
LSYS_BOBSTRIP = 32
LSYS_RECTAB_MAX_LEN = 400
LSYS_BOBS_MAX_LEN = 600		; 250 bobs

lsystemPart:
		lea		CUSTOM,a0
		lea		copper_blank_purple,a1
		move.l	a1,COP2LC(a0)
		bsr		vBlank

		move.l	mem_bss_chip(pc),a6
		move.l	a6,d0
		addi.l	#LS_Screen1,d0
		lea		lsys_Screens(pc),a3
		move.l	d0,(a3)							; buffer screen
		lea		copper_lsys_bpls,a2				; d0 - addr, a2 bpl pointers
		bsr		lsysSetBplAddr

		move.l	a6,d0
		addi.l	#LS_Screen2,d0
		move.l	d0,4(a3)						; active screen (later)

		WAITBLIT
		move.l	#-1,BLTAFWM(a0)					; blitter config part that does not change
		move	#LSYS_BOBSTRIP-4,BLTAMOD(a0)
		move	#LSYS_X-4,BLTBMOD(a0)
		move	#LSYS_X-4,BLTDMOD(a0)
		move	#0,BLTCON1(a0)

		move.l	d0,a1							; clear screens
		move	#LSYS_Y<<6+LSYS_X/2,d0			; a1 addr, d0 size
		bsr		lsysClearScr
		move.l	(a3),a1
		move	#LSYS_Y<<6+LSYS_X/2,d0			; a1 addr, d0 size
		bsr		lsysClearScr

		lea		copper_lsys,a2
		move.l	a2,COP2LC(a0)
		bsr		vBlank

		lea		lsys_p1(pc),a1
		move.l	mem_bss_public(pc),a5
		adda.l	#LS_recursion_tab1,a3
		bsr		lsysCreateIterations

		lea		lsys_p2(pc),a1
		move.l	mem_bss_public(pc),a3
		adda.l	#LS_recursion_tab2,a3
		bsr		lsysCreateIterations

		move.l	mem_bss_public(pc),a1
		adda.l	#LS_bobs_tab1,a1
		lea		lsys_p1(pc),a3
		bsr		lsysCalcBobs
		lea		lsys_Bob_Tab_Ptr(pc),a3
		move.l	a1,(a3)							; start from the beginning of the bobs table

		move.l	mem_bss_public(pc),a1
		adda.l	#LS_bobs_tab2,a1
		lea		lsys_p2(pc),a3
		bsr		lsysCalcBobs

		moveq	#0,d0							; no delay after strobe
		bsr		syncToStrobe					; sync beat counter to next beat
		bsr		resetRelativeBeat

		lea		lsysPlaySequence1(pc),a1		; play the first sequence from L3 int
		bsr		intL3ProcSet

		bsr		logoTransformPrecalc			; prepare logo anims while the first part plays in L3

		lea		lsys_finished(pc),a1
.checkFinishedPart1:
		bsr		vBlank
		tst		(a1)
		beq		.checkFinishedPart1

.mainLoopLSystem:								; play the second part and move on
		bsr		vBlank
		bsr		lsysPlaySequence2
		bne		.mainLoopLSystem
		rts

;------------------------------
; L3 int part
lsysPlaySequence1:
		movem.l	d1-d7/a0/a2-a6,-(sp)			; the other flags are remembered in the L3 int
		lea		CUSTOM,a0
		lea		parts_state(pc),a6				; init etc. state

		tst.b	flash_cnt-parts_state(a6)		; clear color flash
		beq		.noFlashClr
		lea		copper_lsys_cols,a5
		move	flash_ofs-parts_state(a6),d0
		move	flash_col-parts_state(a6),(a5,d0.w)
		clr.b	flash_cnt-parts_state(a6)
.noFlashClr:

LSYS_START_BEAT = 9
;	move #$f00,COLOR00(a0)
		move	beat_relative(pc),d0
		cmp		#LSYS_START_BEAT,d0
		bge		.finished
		tst		beat_strobe-parts_state(a6)
		beq		.noS1
		move	#$fff,d1
		move	#$0dbb,d2
		move	#15*4+2,d3
		bsr		lsysFlashCol
.noS1:	bsr		lsysDrawBobs
		bra		.exit
.finished:
		st		lsys_finished-parts_state(a6)
		bsr		intL3ProcClear
;	move #$000,COLOR00(a0)
.exit:
		movem.l	(sp)+,d1-d7/a0/a2-a6
		rts

;--------------------------------------------
; normal looped part
lsysPlaySequence2:
		lea		CUSTOM,a0
		lea		parts_state(pc),a6				; init etc. state

		tst.b	flash_cnt-parts_state(a6)		; clear color flash
		beq		.noFlashClr2
		lea		copper_lsys_cols,a5
		move	flash_ofs-parts_state(a6),d0
		move	flash_col-parts_state(a6),(a5,d0.w)
		clr.b	flash_cnt-parts_state(a6)
.noFlashClr2:

;	move #$f00,COLOR00(a0)
		move	beat_relative(pc),d0
		cmp		#LSYS_START_BEAT+8,d0
		bge		.part3
		tst.b	(a6)
;		bne.s	.p2main
		bne.s	.p3main
		st		(a6)
		move.l	mem_bss_public(pc),a1			; part 2 init - do only once
		adda.l	#LS_bobs_tab1,a1
		move.l	a1,lsys_Bob_Tab_Ptr-parts_state(a6)							; start from the beginning of the bobs table
		bsr		.flashWhite
		bra		.p3main
; .p2main:
; 		bsr		lsysSwitchScreens
; 		bsr		.sinZoom
; 		bsr		lsysRotateBobs
; 		bra		.exit

;--------
.part3:
		cmp		#LSYS_START_BEAT+8+8,d0
		bge		.part4
		tst.b	1(a6)
		bne.s	.p3main
		st		1(a6)
		lea		lsys_Bob_Angle(pc),a5
		move	#-2,2(a5)						; speed reverse
		bsr		.flashWhite
		move	#BC_PURPLE,15*4+2(a5)			; a5 is set to copper colorlist in the preceding bsr
.p3main:
		bsr		lsysSwitchScreens
		bsr		.sinZoom
		bsr		lsysRotateBobs
		bra		.exit

;--------
.part4:
		cmp		#LSYS_START_BEAT+8+8+8,d0
		bge		.part5
		tst.b	2(a6)
		bne.s	.p4main
		st		2(a6)
		move.l	lsys_Screens(pc),a1				; clear foreground screen
		move	#LSYS_Y<<6+LSYS_X/2,d0
		bsr		lsysClearScr
		WAITBLIT
		move.l	a1,d0
		lea		copper_lsys_bpls,a2				; d0 - addr, a2 bpl pointers
		bsr		lsysSetBplAddr

		move.l	mem_bss_public(pc),a1
		adda.l	#LS_bobs_tab2,a1
		lea		lsys_Bob_Tab_Ptr(pc),a3
		move.l	a1,(a3)							; start from the beginning of the bobs table
		lea		copper_lsys_cols,a1
		lea		lsys_golden_palette_16(pc),a2
		bsr		setColors
		bsr		.flashWhite
		bra		.p4m2
.p4main:
		bsr		.flashPurple
.p4m2:	;moveq	#0,d7							; speed - how many bobs to draw at once
		bsr		lsysDrawBobs
		bra		.exit

;--------
.part5:
		cmp		#LSYS_START_BEAT+8+8+8+8,d0
		bge		.part6
		tst.b	3(a6)
;		bne.s	.p5main
		bne.s	.p6main
		st		3(a6)
		move.l	mem_bss_public(pc),a1
		adda.l	#LS_bobs_tab2,a1
		lea		lsys_Bob_Tab_Ptr(pc),a3
		move.l	a1,(a3)							; back to beginning of the bobs table
		lea		lsys_Bob_Angle(pc),a5
		move	#2,2(a5)						; speed reverse
		move	#0,6(a5)						; no zoom
		bra		.p6_2
; 		bsr		.flashWhite
; 		bra		.p5m2
; .p5main:
; 		bsr		.flashPurple
; .p5m2:	bsr		lsysSwitchScreens
; 		lea		lsys_bob_change_tab(pc),a1		; advance bob animation to next one
; 		addi	#1,(a1)
; 		bsr		lsysRotateBobs
; 		bra		.exit

;--------
.part6:
		cmp		#LSYS_START_BEAT+8+8+8+8+8,d0
		bge		.part7
		tst.b	4(a6)
		bne.s	.p6main
		st		4(a6)
		lea		lsys_Bob_Angle(pc),a5
		move	#-4,2(a5)						; speed reverse
.p6_2:	bsr		.flashWhite
		bra		.p6m2
.p6main:
		bsr		.flashPurple
.p6m2:	bsr		lsysSwitchScreens
		lea		lsys_bob_change_tab(pc),a1		; advance bob animation to next one
		addi	#1,(a1)
		bsr		lsysRotateBobs
		bra		.exit

;--------
.part7:
		cmp		#LSYS_START_BEAT+8+8+8+8+8+8,d0
		bge		.part8
		tst.b	5(a6)
		bne.s	.p8main
		; bne.s	.p7main
		st		5(a6)
		lea		lsys_Bob_Angle(pc),a5
		move	#6,2(a5)						; speed reverse
;		bra		.p8_2
 		bsr		.flashWhite
		move	#BC_PURPLE,15*4+2(a5)			; a5 is set to copper colorlist in the preceding bsr
		bra		.p9_2
; 		bra		.p7m2
; .p7main:
; 		bsr		.flashPurple
; .p7m2:	bra		.p9_2
		; bsr		lsysSwitchScreens
		; lea		lsys_bob_change_tab(pc),a1		; advance bob animation to next one
		; addi	#1,(a1)
		; bsr		.sinZoom
		; bsr		lsysRotateBobs
		; bra		.exit

;--------
.part8:
		cmp		#LSYS_START_BEAT+8+8+8+8+8+8+8,d0
		bge		.part9
		tst.b	6(a6)
		bne.s	.p8main
		st		6(a6)
		lea		lsys_Bob_Angle(pc),a5
		move	#-10,2(a5)						; speed reverse
.p8_2:	bsr		.flashWhite
		move	#$0cc7,15*4+2(a5)			; a5 is set to copper colorlist in the preceding bsr
		bra		.p8m2
.p8main:
		bsr		.flashPurple
.p8m2:	bra		.p9_2
		; bsr		lsysSwitchScreens
		; lea		lsys_bob_change_tab(pc),a1		; advance bob animation to next one
		; addi	#1,(a1)
		; bsr		.sinZoom
		; bsr		lsysRotateBobs
		; bra		.exit

;--------  $0c98 d77
.part9:
		cmp		#LSYS_START_BEAT+8+8+8+8+8+8+8+4,d0
		bge		.finished
		subi.b	#1,7(a6)
		bne		.noB
		move.b	#4,7(a6)
		lea		bobs,a1
		moveq	#15,d7
		moveq	#0,d6
.mb:	REPT	7
		move.l	4(a1),(a1)+						; move bobs left and clear the last one
		ENDR
		move.l	d6,(a1)+
		dbf		d7,.mb
.noB:
.p9_2:	bsr		lsysSwitchScreens
		lea		lsys_bob_change_tab(pc),a1		; advance bob animation to next one
		addi	#1,(a1)
		bsr		.sinZoom
		bsr		lsysRotateBobs
		bra		.exit

;--------
.finished:
		moveq	#0,d0							; if finished return 0
		rts
.exit
		moveq	#1,d0							; otherwise 1 = not yet finished
		rts

;------------------------------
; perform sin-based zooming
.sinZoom:
		lea		lsys_Bob_Angle(pc),a1
		moveq	#0,d0
		move	6(a1),d0						; zoom index
		addi	#1,d0
		cmpi	#2*MUS_TPB,d0
		blo		.ns1
		moveq	#0,d0
.ns1:	move	d0,6(a1)
		lea		scroll_sin+1(pc),a2
		move.b	(a2,d0.w),d0					; sin value
		moveq	#33,d1
		sub		d0,d1
		move	d1,4(a1)						; zoom factor
		rts

.flashPurple:
		tst		beat_strobe-parts_state(a6)
		beq		.noS2
		move	#$424,d1
		move	#BC_PURPLE,d2
		move	#2,d3
		bra		lsysFlashCol
.noS2:	rts
.flashWhite:
		move	#$fff,d1
		move	#BC_PURPLE,d2
		move	#2,d3
; flash given copper color for 1 frame
; d1 - color to flash, d2 - color to return to, d3 - color table offset
lsysFlashCol:
		lea		copper_lsys_cols,a5
		move	d1,(a5,d3.w)
		move	d3,flash_ofs-parts_state(a6)
		move	d2,flash_col-parts_state(a6)
		st		flash_cnt-parts_state(a6)
		rts

parts_state:		dc.b	0,0,0,0,0,0,0,6
flash_cnt:			dc.w	0
flash_ofs:			dc.w	0
flash_col:			dc.w	0
		EVEN

;------------------------------
; a1 - adr, d0 - H<<6+W/2
lsysClearScr:
		WAITBLIT
		move.l	#-1,BLTAFWM(a0)
		move	#0,BLTDMOD(a0)
		move.l	#$1000000,BLTCON0(a0)
		move.l	a1,BLTDPT(a0)
		move	d0,BLTSIZE(a0)
		rts

; d0 - addr, a2 bpl pointers
lsysSetBplAddr:
		move.l	d0,-(sp)
		moveq	#1,d7
.sadr:	
		move	d0,6(a2)
		move	d0,6+16(a2)
		swap	d0
		move	d0,2(a2)
		move	d0,2+16(a2)
		swap	d0
		lea		8(a2),a2
		addi.l	#(LSYS_DISPLACEMENT-1)*LSYS_X,d0
		dbf		d7,.sadr	
		move.l	(sp)+,d0
		rts

; Switch screens and clear buffer screen
lsysSwitchScreens:
		lea		lsys_Screens(pc),a3
		move.l	(a3),d0
		move.l	4(a3),d1
		move.l	d1,(a3)
		move.l	d0,4(a3)						; drawn screen becomes visible
		lea		copper_lsys_bpls,a2				; d0 - addr, a2 bpl pointers
		bsr		lsysSetBplAddr

		move.l	d1,a1							; clear buffer screen
		move	#LSYS_Y<<6+LSYS_X/2,d0
		bsr		lsysClearScr
		rts

lsys_bob_change_tab:	dc.w	0, 0,4,8,12,16,20,24,28,28,24,20,16,12,8,4,0		; index + bob offsets
;------------------------------
; a0 - custom, lsys_Bob_Tab_Ptr has to be initialised with a pointer to the bob table.
lsysRotateBobs:
		move.l	lsys_Screens(pc),a1					; buffer screen
		move.l	lsys_Bob_Tab_Ptr(pc),a4
		lea		bobs,a6
		lea		lsys_Bob_Angle(pc),a5
		move	2(a5),d7							; speed
		add		d7,(a5)								; progress rotation
		move	(a5),d7
		andi	#510,d7
		move	4(a5),d6							; zoom (15 bits, 0=none)
		lea		sinus_14b_256+32*2(pc),a5
.drawLoop:
		move	(a4)+,d0
		bmi		.finished
		move	(a4)+,d1
		move	lsys_bob_change_tab(pc),d2			; shift bob image by a certain index if required
		add		(a4)+,d2
		andi	#$1c,d2					; limit to 8 bobs + filter out the low counter bits
		move	lsys_bob_change_tab+2(pc,d2.w),d2		; shift bobs by an indexed value
		lea		(a6,d2.w),a2			; bob addr

		subi	#120,d0					; centre
		subi	#120,d1

		asl		#2,d0
		asl		#2,d1
		move	d0,d2
		move	d1,d3

		muls	32*2(a5,d7.w),d0		; x' = x*cos - y*sin
		swap	d0
		muls	-32*2(a5,d7.w),d1
		swap	d1
		sub		d1,d0					; x'

		muls	-32*2(a5,d7.w),d2		; y' = x*sin + y*cos
		swap	d2
		muls	32*2(a5,d7.w),d3
		swap	d3
		add		d2,d3
		move	d3,d1					; y'

		tst		d6
		beq		.noZoom
		muls	d6,d0					; simple zoom with scale 0-32
		asr		#5,d0
		muls	d6,d1
		asr		#5,d1
.noZoom:
		addi	#120,d0
		addi	#120,d1

		ror		#4,d0
		move	d0,d2
		andi	#$f000,d2
		ori		#$0dfc,d2
		andi	#$0fff,d0
		add		d0,d0
		lsl		#5,d1					; screen width is 32 bytes
		add		d0,d1
		lea		(a1,d1.w),a3

		WAITBLIT
		move	#LSYS_X-4,BLTDMOD(a0)
		move.l	a2,BLTAPT(a0)
		move	d2,BLTCON0(a0)
		move.l	a3,BLTBPT(a0)
		move.l	a3,BLTDPT(a0)
		move	#16<<6+2,BLTSIZE(a0)

		bra		.drawLoop
.finished:
		rts


;------------------------------
; a0 - custom, (d7 - nr to draw). lsys_Bob_Tab_Ptr has to be initialised with a pointer to the bob table.
lsysDrawBobs:
		move.l	lsys_Screens(pc),a1					; buffer screen
		move.l	lsys_Bob_Tab_Ptr(pc),a4
		lea		bobs,a6
.drawLoop:
		move	(a4)+,d0
		bmi.s	.finished
		move	(a4)+,d1
		move	(a4)+,d2
		lea		(a6,d2.w),a2

		ror		#4,d0
		move	d0,d2
		andi	#$f000,d2
		ori		#$0dfc,d2
		andi	#$0fff,d0
		add		d0,d0
		lsl		#5,d1					; screen width is 32 bytes
		add		d0,d1
		lea		(a1,d1.w),a3

		WAITBLIT
		move	#LSYS_X-4,BLTDMOD(a0)
		move.l	a2,BLTAPT(a0)
		move	d2,BLTCON0(a0)
		move.l	a3,BLTBPT(a0)
		move.l	a3,BLTDPT(a0)
		move	#16<<6+2,BLTSIZE(a0)
		;dbf		d7,.drawLoop

		lea		lsys_Bob_Tab_Ptr(pc),a3	; save pointer to where processing finished
		move.l	a4,(a3)
		bra		.exit
.finished:
		suba	#2,a4					; go back to the end marker
		lea		lsys_Bob_Tab_Ptr(pc),a3	; save pointer
		move.l	a4,(a3)
.exit:
		rts

;------------------------------
; a3 - L system params, a1 - bobs tab
lsysCalcBobs:
		movem.l	ALL,-(sp)
		move	LSYS_OFS_X(a3),d0
		move	LSYS_OFS_Y(a3),d1
		move	LSYS_OFS_Ang(a3),d2
		move	LSYS_OFS_Bob(a3),d3

		asl		#2,d2
		move	d0,(a1)+
		move	d1,(a1)+
		move	d3,(a1)+

		move.l	LSYS_OFS_RFN(a3),a4					; iteration sequence to print
		lea		lsys_rotation_tab(pc),a5

.lParseLoop:
		moveq	#0,d6
		move.b	(a4)+,d6
		beq		.lStopParse
		bmi.s	.lNeg
		cmpi.b	#10,d6
		ble		.lJustDraw
		sub		#30,d6								; rotate
		asl		#2,d6
		add		d6,d2
		bmi		.lrot1
		cmpi	#24*4,d2
		ble		.lParseLoop
		sub		#24*4,d2
		bra		.lParseLoop
.lrot1:	add		#24*4,d2
		bra		.lParseLoop
.lJustDraw:
		add		(a5,d2),d0							; change x and y by whatever the current rotation is
		add		2(a5,d2),d1
		move	d0,(a1)+
		move	d1,(a1)+
		move	d3,(a1)+
		bra		.lParseLoop
.lNeg:	cmpi.b	#-1,d6
		bne.s	.lPull
		addq	#4,d3								; decrease bob size
		cmpi	#7*4,d3
		ble		.l1
		move	#7*4,d3
;		andi	#31,d3
.l1:	movem	d0-d3,-(sp)							; -1 push
		bra		.lParseLoop
.lPull:
		movem	(sp)+,d0-d3							; -2 pull
		bra		.lParseLoop
.lStopParse:
		move	#-1,(a1)+							; end of bob table
		movem.l	(sp)+,ALL
		rts

;------------------------------
; a1 - L system params, a3 - recursion tab
lsysCreateIterations:
		movem.l	ALL,-(sp)
		move	LSYS_OFS_RNr(a1),d7
		subq	#1,d7

		move.l	a3,a6				; a6 - last recursion
		move	LSYS_OFS_Ax(a1),d0
		lea		(a1,d0.w),a2
.ci1:	move.b	(a2)+,(a3)+			; axiom data - recursion 0
		bne.s	.ci1

		move.l	a6,a2				; a2 last recursion
		move.l	a3,a6
		; a2 last recursion, a3 new recursion
		moveq	#0,d0
.ciIter:
		move.b	(a2)+,d0
		beq		.ciEndIter
		bpl		.ciNoNeg
		move.b	d0,(a3)+			; negatives - just copy
		bra		.ciIter
.ciNoNeg:
		cmpi.b	#10,d0
		ble		.ciIsRule
		move.b	d0,(a3)+			; >10 are angle changes centered on 30
		bra		.ciIter
.ciIsRule:
		move.b	LSYS_OFS_Rules(a1,d0.w),d0
		lea		(a1,d0.w),a4		; rule addr
.ci2:	move.b	(a4)+,d0
		beq		.ciIter
		move.b	d0,(a3)+
		bra		.ci2				; copy rule
.ciEndIter:
		move.b	d0,(a3)+
		move.l	a6,a2
		move.l	a3,a6
		dbf		d7,.ciIter

		move.l	a2,LSYS_OFS_RFN(a1)		; final recursion table addr
		movem.l	(sp)+,ALL
		rts

		RSRESET
		LSYS_OFS_X:		rs.w	1
		LSYS_OFS_Y:		rs.w	1
		LSYS_OFS_Ang:	rs.w	1
		LSYS_OFS_Bob:	rs.w	1
		LSYS_OFS_RNr:	rs.w	1
		LSYS_OFS_RFN:	rs.l	1
		LSYS_OFS_Ax:	rs.w	1
		LSYS_OFS_Rules:	rs.w	1

lsys_p1:
.l_p1:		dc.w	120,120,2,0		; x, y, angle offset (0-23), starting bob index
			dc.w	3				; recursions
			dc.l	0				; final recursion adress
			dc.w	.l_a1-.l_p1
			dc.b	0,.l_r1_1-.l_p1,.l_r1_2-.l_p1,0
.l_a1:		dc.b	-1,42,1,-2,1,0				; axiom
.l_r1_1:	dc.b	2,29,2,29,2,27,-1,38,1,-2,2,-1,31,1,-2,26,2,0
.l_r1_2:	dc.b	2,0



lsys_p2:
.l_p1:		dc.w	120,120,0,0		; x, y, angle offset (0-23), starting bob index
			dc.w	3				; recursions
			dc.l	0				; final recursion adress
			dc.w	.l_a1-.l_p1
			dc.b	0,.l_r1_1-.l_p1,.l_r1_2-.l_p1,0
.l_a1:		dc.b	-1,38,1,-2,-1,46,1,-2,1,0				; axiom
.l_r1_1:	dc.b	2,31,2,31,2,-1,33,1,-2,28,1,0
.l_r1_2:	dc.b	2,0


lsys_Bob_Tab_Ptr:	dc.l	0
lsys_Screens:		dc.l	0,0				; buffer screen, acive screen
lsys_Bob_Angle:		dc.w	-4, 2, 0, 0		; current angle (0-510 step 2), speed, zoom (32-0), zoom sin pos
lsys_finished:		dc.w	0				; 1 - flag indicating that the whole lsys part is finished

		EVEN
lsys_rotation_tab:	dc.w	0,10,-3,9,-5,8,-8,7,-9,5,-10,2, -10,0,-10,-3,-9,-5,-8,-8,-5,-9,-3,-10, 0,-10,3,-10,5,-8,8,-8,9,-5,10,-3, 10,0,9,3,8,5,7,8,5,8,3,9	; 24 - evey 15 degs
		EVEN

;-----------------------------------------------------------------------
;-----------------------------------------------------------------------
scrollPart:
		lea		CUSTOM,a0
		lea		copper_blank_purple,a1
		move.l	a1,COP2LC(a0)
		bsr		vBlank
;		bsr		logoTransformPrecalc						; prepare logo anims - this is done in the previous part

		move.l	mem_bss_chip(pc),a6
		lea		logo_trans_frames(pc),a5
		move.l	(a5),a1										; 1st frame = Suspect logo
		lea		Logo_screen1(a6),a2
		move.l	#LOGO_SIZE/4-1,d7
.cpl1:	move.l	(a1)+,(a2)+
		dbf		d7,.cpl1

		lea		Logo_screen1(a6),a1							; chip screen buffer with logo
		lea		copper_scroll_logo_bpls,a2
		moveq	#LOGO_BPL-1,d0
		moveq	#(LOGO_SIZE_X/8),d1
		bsr		setLogoAddr

		bsr 	scrollInit

		lea		copper_scroll,a2
		move.l	a2,COP2LC(a0)
		bsr		vBlank

		lea		copper_scroll_logo_cols,a1					; show first logo
		lea		base_purple_palette_16(pc),a2
		lea		logo_suspect_palette(pc),a3
		moveq	#2,d0
		bsr		transformColors

		moveq	#1,d0
		bsr		syncToStrobe				; sync next action to strobe (music beat)
		lea		scrollMove(pc),a1			; scroll move procedure to be called from the L3 int
		bsr		intL3ProcSet

; Main scroll loop
.mainLoopScroll:
		VBLANK

		lea		logo_trans_delay(pc),a1
		subi	#1,(a1)
		bne.s	.noBlend
		bsr		blendLogos
.noBlend:

		lea		sl(pc),a1					; if music finished then finish part
		tst		music_ticks_left-sl(a1)
		beq		.exit

		btst.b  #6,CIAA
        bne	    .mainLoopScroll

.exit:
		bsr		intL3ProcClear				; remove L3 int proc
		lea		blitter_queue(pc),a1		; make sure blitter queue is empty
		BLITTERWAITQUEUE
		WAITBLIT

		lea		copper_scroll_logo_cols,a1
		lea		logo_suspect_palette(pc),a2
		lea		base_purple_palette_16(pc),a3
		moveq	#2,d0
		bsr		transformColors

		rts

; ------------------------------------------
scrollMove:
;		movem.l	a1/d0,-(sp)
		lea		copper_scroll_shift,a1
		subi	#1,scroll_cnt-copper_scroll_shift(a1)
		bmi.s	.smExit				; stop scroll once all shown
		subi	#$11,2(a1)			; shift scroll
		bpl.b	.smJump
		move	#$ff,2(a1)
		add		#2,10(a1)			; move addr
		bne.s	.sm1
		add		#1,6(a1)
.sm1:	add		#2,10+8(a1)
		bne.s	.smJump
		add		#1,6+8(a1)
.smJump:
		lea		scroll_sin(pc),a1
		moveq	#0,d0
		move.b	(a1),d0				; counter 23..0
		subq	#1,d0
		bpl.s	.sm2
		move	#2*MUS_TPB-1,d0
.sm2:	move.b	d0,(a1)+
		move.b	(a1,d0.w),d0		; sin value
		lea		copper_scroll_ypos,a1
		.FOFS:	SET 0
		REPT	5
		move.b	d0,.FOFS(a1)
		addq	#3,d0
		.FOFS:	SET .FOFS+8
		ENDR
.smExit:
;		movem.l	(sp)+,a1/d0
		rts

scroll_sin:		dc.b	MUS_TPB*2,1,2,3,4,5,6,7,8,9,10,11,12,12,13,14,14,15,15,16,16,16,17,17,17,17,17,17,17,16,16,16,15,15,14,14,13,12,12,11,10,9,8,7,6,5,4,3,2
		EVEN

; ------------------------------------------
; init scroll 8x12
SCROLL_Y = 12
scrollInit:
		movem.l	ALL,-(sp)
		move.l	mem_bss_chip(pc),a6
		move.l	a6,d0
		addi.l	#Scroll_buffer,d0
		move.l	d0,a3
		lea		copper_scroll_bpls,a2			; set scroll bpl addr
		moveq	#1,d7
.sadr:	move	d0,6(a2)
		swap	d0
		move	d0,2(a2)
		swap	d0
		lea		8(a2),a2
		addi.l	#SCROLL_LEN,d0
		dbf		d7,.sadr	

		bsr		fontPrep_8_12					; out: a2 - font addr

		lea		scroll_text,a1					; print the whole scroll at once
		move	#SCROLL_LEN,d1
		bsr		printText_8_12

		movem.l	(sp)+,ALL
		rts

; In: - , Out: a2 font addr
fontPrep_8_12:
		move.l	mem_bss_public(pc),a5
		adda.l	#Font_16_aligned,a5
		move.l	a5,a2
		lea		font_8_12_4,a4
		moveq	#0,d0
		moveq	#SCROLL_CHARS-1,d7					; nr of fonts
.fntPrep:											; align fonts with all words of each letter next to each other
		.FOFS:	SET 0
		REPT	SCROLL_Y
		move.b	.FOFS(a4),(a5)+
		move.b	.FOFS+SCROLL_CHARS(a4),(a5)+
		.FOFS:	SET .FOFS+2*SCROLL_CHARS
		ENDR
		lea		32-2*SCROLL_Y(a5),a5				; skip not used lines up to 32
		lea		1(a4),a4
		dbf		d7,.fntPrep
		rts

; a1 - text, a2 - font, a3 - screen, d1 - line length
printText_8_12:
		move	d1,d5
		lsl		d5
.siLoop:
		moveq	#0,d0
		move.b	(a1)+,d0
		beq.s	.siEnd
		subi	#32,d0				; space
		lsl		#5,d0
		lea		(a2,d0.w),a4
		moveq	#0,d2
		move	d1,d3
		moveq	#SCROLL_Y-1,d4
.fnt:	move.b	(a4)+,(a3,d2.w)
		move.b	(a4)+,(a3,d3.w)
		add		d5,d2
		add		d5,d3
		dbf		d4,.fnt
		lea		1(a3),a3
		bra.s	.siLoop
.siEnd:	rts

; ------------------------------------------
; Blend logos step
blendLogos:
		lea		CUSTOM,a0
		move.l	mem_bss_chip(pc),a6
		lea		logo_trans_frames(pc),a5
		lea		logo_trans_sched_nr(pc),a1
		move	2(a1),d0					; current schedule offset
		subi	#1,(a1)						; schedule type timer 0-BL_RANGE
		bne.s	.blNrc
		move	#2*BL_RANGE,(a1)
		lea		logo_trans_speed(pc),a2
		moveq	#3,d7
		moveq	#1,d6						; re-init the speed tab
.blClSpd:
		REPT 5
		move	d6,(a2)+
		ENDR
		dbf		d7,.blClSpd
		addi	#20*2+2,d0					; move to next schedule
		cmp		#(20*2+2)*TRANS_SCHED_MAX,d0
		bne.s	.blRc
		moveq	#0,d0
.blRc:	move	d0,2(a1)
.blNrc:
		lea		logo_trans_schedule(pc),a3
		lea		(a3,d0.w),a3				; move to the right schedule

		lea		blitter_queue(pc),a1		; make sure blitter queue is empty
		BLITTERWAITQUEUE

		lea		logo_trans_delay(pc),a1
		move	(a3)+,(a1)					; speed

;	move #$0f0,COLOR00(a0)
		lea		logo_trans_speed(pc),a4
		moveq	#0,d7						; X size in words *2
.blIterate:
		move	(a3),d0
		move	(a4),d1
		add		d1,d0							; move schedule
		bpl.s	.bl1
		neg		d1
		bra.s	.bl2
.bl1:	cmpi	#BL_RANGE,d0
		bmi.s	.bl2
		neg		d1
.bl2:
		move	d0,(a3)+
		move	d1,(a4)+

		sub		#(BL_RANGE-LOGO_TRANS_NR)/2,d0	; start transition in the middle of the counter
		bmi		.blNoBl
		cmpi	#LOGO_TRANS_NR,d0
		bpl		.blNoBl
		add		d0,d0
		add		d0,d0

		move.l	(a5,d0.w),a1					; frame addr
		lea		(a1,d7.w),a1
		move.l	a1,d1
		lea		Logo_screen1(a6,d7.w),a2
		move.l	a2,d2

		lea		blitter_queue(pc),a1
		move.l	a1,a2
		move	(a1),d3
		bne.s	.blNotNeg
		moveq	#2,d3
.blNotNeg:
		lea		(a1,d3.w),a1					; start of free space in queue
		tst		d0
 		bne.s	.blLimBlt
		move.l	#BLTAMOD<<16+38,(a1)+			; this part has to be done only once
		move.l	#BLTDMOD<<16+38,(a1)+
		move.l	#BLTCON0<<16+$09f0,(a1)+
		move.l	#BLTCON1<<16+0,(a1)+
		move.l	#BLTAFWM<<16+$ffff,(a1)+
		move.l	#BLTALWM<<16+$ffff,(a1)+
.blLimBlt:
		move	#BLTAPTL,(a1)+
		move	d1,(a1)+
		move	#BLTAPTH,(a1)+
		swap	d1
		move	d1,(a1)+
		move	#BLTDPTL,(a1)+
		move	d2,(a1)+
		move	#BLTDPTH,(a1)+
		swap	d2
		move	d2,(a1)+
		move	#BLTSIZE,(a1)+
		move	#((LOGO_SIZE_Y*LOGO_BPL)<<6)+1,(a1)+	; logo heigth + 1 word width
		move	#-1,(a1)+						; end step
		suba.l	a2,a1
		move.w	a1,(a2)							; update queue length

; 		WAITBLIT
; 		tst		d0
; 		bne.s	.blLimBlt
; 		move	#38,BLTAMOD(a0)				; A modulo
; 		move	#38,BLTDMOD(a0)				; D modulo
; 		move.l	#$09f00000,BLTCON0(a0)		; BLTCON0: LF4,5,6,7 + USEA + USED.   BLTCON1 = 0
; 		move.l	#$ffffffff,BLTAFWM(a0)		; BLTAFWM, BLTALWM = FF..FF
; .blLimBlt:
; 		move.l	a1,BLTAPT(a0)				; Source A
; 		move.l	a2,BLTDPT(a0)				; Dest D
; 		move	#((LOGO_SIZE_Y*LOGO_BPL)<<6)+1,BLTSIZE(a0)

.blNoBl:
		addq	#2,d7
		cmpi	#20*2,d7
		bne		.blIterate

		bsr		blitterQueueStart			; kick off blitter queue

.qEmpty
;	move #$000,COLOR00(a0)
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
		adda.l	#Logo_trans_buffer,a6				; bitplanes destination
		moveq	#1,d7								; scaling factor
.frameLoop:
	; move #$f00,CUSTOM+COLOR00
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

	; move #$000,CUSTOM+COLOR00
		addq	#1,d7
		cmpi	#16,d7								; total of 15 frames generated + 1 start and 1 end = 17
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
.wait:	bsr		vBlank
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
.wait:	bsr		vBlank
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
.wait:	bsr		vBlank
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

;-----------------------------------------------------------------------
;-----------------------------------------------------------------------
ENDPART_X = 8
ENDPART_FONT_Y = 12
ENDPART_MIDSCR = 167
endPart:
		lea		CUSTOM,a0
		lea		copper_blank_purple,a1
		move.l	a1,COP2LC(a0)
		WAIT	24

		move.l	mem_bss_chip(pc),a6
		move.l	a6,d0
		addi.l	#Scroll_buffer,d0
		move.l	d0,a3
		lea		copper_end_bpls1,a2				; set bpl addr
		bsr		.endSetBplAddr
		addq	#ENDPART_X,d0					; second line
		lea		copper_end_bpls2,a2				; set bpl addr
		bsr		.endSetBplAddr

		bsr		fontPrep_8_12					; out: a2 - font addr

		lea		.endTxt,a1						; print the message
		move	#ENDPART_X*2,d1
		bsr		printText_8_12

		lea		copper_end,a1
		move.l	a1,COP2LC(a0)
		bsr		vBlank

		lea		copper_end_cols1,a1					; fade in
		lea		base_purple_palette_4(pc),a2
		lea		theend_palette(pc),a3
		moveq	#1,d0
		bsr		transformColors
		lea		copper_end_cols2,a1
		bsr		transformColors

		lea		copper_end_cols1,a1					; fade out
		lea		base_purple_palette_4(pc),a3
		lea		theend_palette(pc),a2
		bsr		transformColors
		lea		copper_end_cols2,a1
		bsr		transformColors

		lea		copper_end2,a1
		move.l	a1,COP2LC(a0)
		bsr		vBlank

		lea		copper_end2_stripe,a1
		lea		.offsets(pc),a2
		lea		.cols(pc),a3
.shrink1:
		WAIT	1
		move.b	(a2)+,d0
		bmi.s	.shEnd1
		move	#ENDPART_MIDSCR,d1
		move	d1,d2
		sub		d0,d1
		add		d0,d2
		move.b	d1,(a1)							; start and end pos
		move.b	d2,8(a1)
		move	(a3)+,d1
		move	d1,6(a1)						; color
		bra		.shrink1
.shEnd1:

		move	#111,d0							; shrink to dot
.shrink2:
		WAIT	1
		move	#136,d1
		move	d1,d2
		sub		d0,d1
		add		d0,d2
		move.b	d1,1(a1)						; start and end pos
		move.b	d2,9(a1)
		subi	#10,d0
		bpl		.shrink2
		bsr		vBlank

		lea		copper_blank_black,a1
		move.l	a1,COP2LC(a0)
		WAIT	50
		rts


.endSetBplAddr:
		move.l	d0,-(sp)
		moveq	#1,d7
.sadr:	move	d0,6(a2)
		swap	d0
		move	d0,2(a2)
		swap	d0
		lea		8(a2),a2
		addi.l	#2*ENDPART_X,d0
		dbf		d7,.sadr	
		move.l	(sp)+,d0
		rts

.endTxt:	dc.b	"   THE  "
			dc.b	"   END  ",0
	EVEN
.offsets:	dc.b	70,49,37,29,22,16,11,7,4,2,1,0,-1
	EVEN
.cols:		dc.w	$313,$424,$535,$646,$757,$868,$979,$a8a,$bab,$cbc,$eee,$fff

; ----------- Include other code files

    INCLUDE     "os.s"
	INCLUDE		"LightSpeedPlayer.s"
	INCLUDE		"c2p.s"
	INCLUDE		"sin.i"	; sine
	ds.w		64		; cosine extension

; ----------- Local data which can be (pc) referenced
sl:											; state_local
tick_cnt:				dc.w	0			; current tick
beat:					dc.w	0			; current beat (absolute nr from 0=first at pos00 note 00, increases every MUS_TPB)
beat_strobe:			dc.w	0			; beat strobe, lit for 1 frame at the start of the beat with the nr of the beat
beat_next:				dc.w	MUS_TPB		; next beat in ticks
beat_relative:			dc.w	0			; relative beat counter which can be reset and keeps counting up every beat
music_ticks_left:		dc.w	0			; fill in after initialising music - indicates how many ticks to play

; music_lastpos			dc.w	0			; last position
; music_finished:			dc.b	0			; first byte non-zero: music finished and stopped playing

; mem_data_chip:			dc.l	0			; addresses of allocated memory regions
; mem_data_public:		dc.l	0
mem_bss_chip:			dc.l	0
mem_bss_public:			dc.l	0

logo_trans_frames:		dcb.l	LOGO_TRANS_NR,0		; transition frame pointers

BL_RANGE = 3*MUS_TPB
TRANS_SCHED_MAX = 5
logo_trans_delay:		dc.w	3
logo_trans_sched_nr:	dc.w	2*BL_RANGE, 0												; cnt and nr
logo_trans_schedule:	dc.w	3, 19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0		; speed + pattern 20 words
						dc.w	2, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19
						dc.w	3, 0,1,2,3,4,5,6,7,8,9,10,8,7,6,5,4,3,2,1,0
						dc.w	2, 9,8,7,6,5,4,3,2,1,0,0,1,2,3,4,5,6,7,8,9
						dc.w	3, 0,2,4,6,8,10,11,12,13,13,13,13,12,11,10,8,6,4,2,0
logo_trans_speed:		dc.w	1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1

BLT_Q_MAX = 200
blitter_queue:			dc.w	0						; offset in queue
						dcb.l	BLT_Q_MAX,0

logo_suspect_palette:
						dc.w		LOGO_COLS		; nr of colours
						INCBIN		"assets/sct_73_inv.pal"
logo_eire_palette:
						dc.w		EIRE_COLS
						INCBIN		"assets/eire_128x96x16.pal"
theend_palette:
						dc.w		4
						dc.w		BC_PURPLE,$0b9b,$0666,$0ece
base_purple_palette_16:
						dc.w		LOGO_COLS
						dcb.w		16,BC_PURPLE
base_purple_palette_4:
						dc.w		4
						dcb.w		4,BC_PURPLE
lsys_golden_palette_16:
						dc.w	16,BC_PURPLE,$0652,$0762,$0984,$0652,$0762,$0984,$0aa6
						dc.w	$0541,$0642,$0752,$0974,$0642,$0752,$0974,$0dd9
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

; ----------- Blank copperlists
copper_blank_black:
       	dc.w    COLOR00, $000, BPLCON0, $0200  						; 0 bitplanes
		dc.l	-2
copper_blank_purple:
       	dc.w    COLOR00, $313, BPLCON0, $0200  						; 0 bitplanes
		dc.l	-2

; ----------- Eire screen copperlist
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

; ----------- L systems copperlist
copper_lsys:
		dc.w	BPLCON0, $0200, BPLCON1, LSYS_DISPLACEMENT
		dc.w	DIWSTRT, $2C81, DIWSTOP, $2aC1
		dc.w	DDFSTRT, $0048, DDFSTOP, $00C0
		dc.w	BPL1MOD,0, BPL2MOD,0,COLOR15,$0cc7
copper_lsys_cols:
;		dc.w	COLOR00,BC_PURPLE,COLOR01,$0652,COLOR02,$0762,COLOR03,$0984,COLOR04,$0652,COLOR05,$0762,COLOR06,$0984,COLOR07,$0aa6
;		dc.w	COLOR08,$0541,COLOR09,$0642,COLOR10,$0752,COLOR11,$0974,COLOR12,$0642,COLOR13,$0752,COLOR14,$0974,COLOR15,$0dd9
		dc.w	COLOR00,BC_PURPLE,COLOR01,$0311,COLOR02,$0623,COLOR03,$0a56,COLOR04,$0412,COLOR05,$0734,COLOR06,$0956,COLOR07,$0b89
		dc.w	COLOR08,$0411,COLOR09,$0311,COLOR10,$0623,COLOR11,$0a56,COLOR12,$0412,COLOR13,$0734,COLOR14,$0956,COLOR15,$0dbb
copper_lsys_bpls:
		dc.w	BPL1PTH,0,BPL1PTL,0,BPL2PTH,0,BPL2PTL,0,BPL4PTH,0,BPL4PTL,0,BPL3PTH,0,BPL3PTL,0
		dc.w	$2c01,$fffe, BPLCON0, $4200
		dc.w	$ffdf,$fffe
		dc.w	$2901,$fffe, BPLCON0, $0200
		dc.l	-2

; ----------- Scroll screen copperlist
BC_PURPLE = $313
scroll_cnt:		dc.w	(SCROLL_LEN-44)*8
copper_scroll:
		dc.w	BPLCON0, $0200, BPLCON1, $0000							 ; 0 bitplanes, no scroll
		dc.w	DIWSTRT, $2C81, DIWSTOP, $1EC1
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
		dc.w	$ffdf,$fffe

		dc.w	$0001,$fffe
		dc.w	BPL1MOD,2*SCROLL_LEN-40
		dc.w	BPL2MOD,2*SCROLL_LEN-40
		dc.w	DIWSTRT, $2C91, DIWSTOP, $1EB1
		dc.w	COLOR01,$0b9b,COLOR02,$0444,COLOR03,$0cac
copper_scroll_shift:
		dc.w	BPLCON1, $0000
copper_scroll_bpls:
		dc.w	BPL1PTH,0,BPL1PTL,0,BPL2PTH,0,BPL2PTL,0
copper_scroll_ypos:
		dc.w	$1101,$fffe, BPLCON0, $2200
		dc.w	$1401,$fffe, COLOR03,$0dbd
		dc.w	$1701,$fffe, COLOR03,$0ece
		dc.w	$1A01,$fffe, COLOR03,$0fdf
		dc.w	$1d01,$fffe, BPLCON0, $0200
		dc.l	-2

; ----------- End screen copperlist
copper_end:
		dc.w	BPLCON0, $0200, BPLCON1, $0088
		dc.w	DIWSTRT, $2C81, DIWSTOP, $1EC1
		dc.w	DDFSTRT, $0070, DDFSTOP, $0088
		dc.w	BPL1MOD,3*ENDPART_X, BPL2MOD,3*ENDPART_X
copper_end_cols1:
		dc.w	COLOR00,BC_PURPLE,COLOR01,BC_PURPLE,COLOR02,BC_PURPLE,COLOR03,BC_PURPLE
copper_end_bpls1:
		dc.w	BPL1PTH,0,BPL1PTL,0,BPL2PTH,0,BPL2PTL,0
		dc.w	$9901,$fffe, BPLCON0, $2200
		dc.w	$a501,$fffe, BPLCON0, $0200
copper_end_cols2:
		dc.w	COLOR00,BC_PURPLE,COLOR01,BC_PURPLE,COLOR02,BC_PURPLE,COLOR03,BC_PURPLE
copper_end_bpls2:
		dc.w	BPL1PTH,0,BPL1PTL,0,BPL2PTH,0,BPL2PTL,0
		dc.w	$a901,$fffe, BPLCON0, $2200
		dc.w	$b501,$fffe, BPLCON0, $0200
		dc.l	-2

copper_end2:
		dc.w	COLOR00,0
		dc.w	BPLCON0, $0200
copper_end2_stripe:
		dc.w	$5001,$fffe, COLOR00,BC_PURPLE
		dc.w	$fff1,$fffe, COLOR00,0
		dc.l	-2


bobs:
   	INCBIN		"assets/shrinkingdots_8.bpl"
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
LOGO_TRANS_NR = 17
	EVEN
logo_Eire:
   	INCBIN		"assets/eire_128x96x16.bpl"
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
font_16_15_1:
   	INCBIN		"assets/font_16x15x1.bpl"
font_8_12_4:
   	INCBIN		"assets/font_8x12x4.bpl"
scroll_text:
   	INCBIN		"assets/scroll.txt"
	dc.b		0
	EVEN
scroll_text_end:
S
	EVEN

; ------------------------- BSS CHIP section ---------------------------------
	bss_c
mbc:
	RSRESET
	Copper_start_swipe:			rs.l	((START_COPPER_LINES+1)*4)+1
	Copper_start_wait_addr:		rs.l	START_COPPER_LINES
	BSS_CHIP_1:					rs.w	0

SCROLL_LEN = (scroll_text_end-scroll_text)
SCROLL_CHARS = 60

	RSRESET
	Logo_screen1:				rs.b	LOGO_SIZE
	LS_Screen2:					rs.b	LSYS_X*LSYS_Y
	Scroll_buffer:				rs.b	SCROLL_LEN*2*SCROLL_Y
	Logo_trans_buffer:			rs.b	LOGO_SIZE*(LOGO_TRANS_NR-2)
	BSS_CHIP_2:					rs.w	0

LS_Screen1 = Logo_screen1

BSS_CHIP_ALLOC_MAX set BSS_CHIP_1
	if BSS_CHIP_2 > BSS_CHIP_ALLOC_MAX
BSS_CHIP_ALLOC_MAX set BSS_CHIP_2
	endif

	;  echo 		"scrb: ", SCROLL_LEN*2*SCROLL_Y
	;  echo 		"logo: ", LOGO_SIZE

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
	Logo_chunky_buffer:			rs.b	LOGO_SIZE_X*LOGO_SIZE_Y
	Font_16_aligned:			rs.b	32*SCROLL_CHARS
	LS_recursion_tab1:			rs.b	LSYS_RECTAB_MAX_LEN
	LS_bobs_tab1:				rs.b	LSYS_BOBS_MAX_LEN
	LS_recursion_tab2:			rs.b	LSYS_RECTAB_MAX_LEN
	LS_bobs_tab2:				rs.b	LSYS_BOBS_MAX_LEN
	BSS_PUBLIC_ALLOC_MAX:		rs.w	0

	ds.b		BSS_PUBLIC_ALLOC_MAX

; ------------------------------------------------------------------------------
	echo 		"BSS CHIP alloc: ", BSS_CHIP_ALLOC_MAX
	echo 		"BSS PUB alloc: ", BSS_PUBLIC_ALLOC_MAX

    END