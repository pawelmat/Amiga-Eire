; Eire - 40k intro for the Boom 2025 party, Tuchola, Poland
; Pawel Matusz / Kane (kane@konto.pl)
; 25/06/2025 - TBD

    TTL         "Eire"

; ----------- Constants

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

		move.l	#copper_base,COP1LC(a0)
		move.l	#copper_blank_black,COP2LC(a0)
		move	#0,COPJMP1(a0)
		move	#INTF_SETCLR|INTF_INTEN|INTF_VERTB|INTF_COPER,INTENA(a0)		; enable selected interrupts
		move	#DMAF_SETCLR|DMAF_DMAEN|DMAF_BPLEN|DMAF_COPEN|DMAF_BLTEN,DMACON(a0)

		bsr		swipeScreenAtStart

		;moveq	#11,d0
		;bsr		setMusicPos

        TESTLMB

exit:
        bsr     OsRestore
        movem.l (sp)+,d0-a6
        clr     d0                      ; exit code
        rts

; ------------------------------------------
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
		lea		CUSTOM+AUD0LCH,a6		; always set a6 to dff0a0 before calling LSP tick
		bsr		LSP_MusicPlayTick		; player music tick

		lea		state_local(pc),a0
		move	beat_next(pc),d0
		cmp		tick_cnt-state_local(a0),d0
		bne.s	.noBeat
		addq	#1,beat-state_local(a0)				; count beats
		addi	#MUS_TPB,beat_next-state_local(a0)	; move to next beat
		;move	#$e5e,copper_blank_black+2			; flash screen on beat
		;bra.s	.cont1
.noBeat:
		;move	#$313,copper_blank_black+2			; flash screen on beat
;.cont1:
		addq	#1,tick_cnt-state_local(a0)			; increase tick (frame) counter


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

; ------------------------------------------
initMusic:
		; Init LSP player
		move	#$7fff,CUSTOM+ADKCON
		lea		LSP_Music,a0
		lea		LSP_Bank,a1
		lea		copper_DMAConPatch+3,a2
		bsr		LSP_MusicInit
		rts

; Set music position and recalc related variables. Each pos has 8 beats (24 ticks each) and each beat 8 notes (3 ticks each).
; in: d0 - seq position (from 0 to last seq of the song)
setMusicPos:
		movem.l	d1-d2/a0,-(sp)
		move	#MUS_TPN*64,d1			; length of one pos (64 notes) in ticks
		mulu	d0,d1
		lea		state_local(pc),a0
		move 	d1,tick_cnt-state_local(a0)
		move	d0,d2
		lsl		#3,d2					; 8 beats per pos
		move 	d2,beat-state_local(a0)
		addi	#MUS_TPB,d1
		move 	d1,beat_next-state_local(a0)
		bsr		LSP_MusicSetPos			; move actual mus
		movem.l	(sp)+,d1-d2/a0
		rts

; ------------------------------------------
; 2-stage screen transition (left right and then centre->up/down) at the start
START_COPPER_LINES = 272
swipeScreenAtStart:
		lea		mem_chip,a1
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

		move	#22-1,d7
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

; ----------- Include other code files

    INCLUDE     "os.s"
	INCLUDE		"LightSpeedPlayer.s"


; ----------- Local data which can be (pc) referenced

state_local:
tick_cnt:		dc.w	0
beat:			dc.w	0			; current beat (absolute nr from 0=first at pos00 note 00)
beat_next:		dc.w	MUS_TPB		; next beat in ticks


end:
    ;echo 		"Total code length: ", (end-s)

; ------------------------- DATA CHIP section ---------------------------------
    data_c
	EVEN

; ----------- Base copper header, which fires the copper interrupt playing the music. 
; In COP1LC and not to be touched. All demo copperlists use COP2LC.
copper_base:
		dc.w	(10<<8)|$09, $fffe, INTREQ, INTF_SETCLR|INTF_COPER		; start copper interrupt
		dc.w	((10+11)<<8)|$09, $fffe 								; wait for scanline +11 and set audio DMA
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

LSP_Bank:
	EVEN
   	INCBIN		"assets/Bartesek - Hey Simone!.lsbank"
	EVEN

; ------------------------- DATA PUBLIC section ---------------------------------
	data

LSP_Music:
	EVEN
   	INCBIN		"assets/Bartesek - Hey Simone!.lsmusic"
logo_Suspect:
	EVEN
   	INCBIN		"assets/sct_73_inv.bpl"
logo_Scoopex:
	EVEN
   	INCBIN		"assets/scx_65_inv.bpl"
logo_Suspect_palette:
	EVEN
   	INCBIN		"assets/sct_73_inv.pal"
font_8:
	EVEN
   	INCBIN		"assets/font.bpl"

; ------------------------- BSS CHIP section ---------------------------------
	bss_c

CHIP_ALLOC_START:		equ		0
mem_chip:
	RSRESET
	Copper_start_swipe:			rs.l	((START_COPPER_LINES+1)*4)+1
	Copper_start_wait_addr:		rs.l	START_COPPER_LINES
	CHIP_ALLOC_MAX:				rs.w	0

	ds.b		CHIP_ALLOC_MAX

	echo 		"Total CHIP alloc: ", CHIP_ALLOC_MAX

; ------------------------- BSS public section ---------------------------------
	bss


    END