; Eire - 40k intro for the Boom 2025 party, Tuchola, Poland
; Pawel Matusz / Kane (kane@konto.pl)
; 25/06/2025 - TBD

    TTL         "Eire"

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
       	move    #0,FMODE(a0)

        lea     configBlock(pc),a6
		move.l	VBR_BASE(a6),a1
        lea     interruptL3(pc),a2		; L3 interrupt (for VB, Copper and Blitter)
		move.l	a2,$6c(a1)

		move.l	#copper_base,COP1LC(a0)
		move.l	#copper_blank,COP2LC(a0)
		move	#0,COPJMP1(a0)
		move	#INTF_SETCLR|INTF_INTEN|INTF_VERTB|INTF_COPER,INTENA(a0)		; enable selected interrupts
		move	#DMAF_SETCLR|DMAF_DMAEN|DMAF_BPLEN|DMAF_COPEN|DMAF_BLTEN,DMACON(a0)
		VBLANK

        TESTLMB

exit:
        bsr     OsRestore
        movem.l (sp)+,d0-a6
        clr     d0                      ; exit code
        rts

; ------------------------------------------
; Main L3 interrupt
interruptL3:
		movem.l ALL,-(sp)
		move	CUSTOM+INTREQR,d0				; INTREQQ - which L3 interrupt was raised?
		move	d0,d1
		andi	#$20,d0
		bne.s	interruptL3Vertb

		andi	#$40,d1
		beq.s	interruptL3Coper

;blitter finished interrupt
		;blitter code here
		movem.l	(sp)+,ALL
		move	#$40,$dff09c			; clear BLIT INTEREQ
		move	#$40,$dff09c			; double just in case to prevent any error in emu or fast CPU from calling the int again too fast
		nop
		rte

; coprocessor L3 interrupt do nothing
interruptL3Coper:
		lea		$dff0a0,a6				; always set a6 to dff0a0 before calling LSP tick
		bsr		LSP_MusicPlayTick		; player music tick
		movem.l	(sp)+,ALL
		move	#$10,$dff09c			; clear COPER INREREQ
		move	#$10,$dff09c			; double just in case to prevent any error in emu or fast CPU from calling the int again too fast
		nop
		rte

;vertical blank interrupt
interruptL3Vertb:

		movem.l	(sp)+,ALL
		move	#$20,$dff09c
		move	#$20,$dff09c		; double just in case to prevent any error in emu or fast CPU from calling the int again too fast
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

	

; ----------- Include other code files

    INCLUDE     "os.s"
	EVEN
	INCLUDE		"LightSpeedPlayer.s"

end:
    echo 		"Total code length: ", (end-s)

; ------------------------- DATA CHIP section ---------------------------------
	bss

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

; ----------- Blank copper
copper_blank:
       	dc.w    COLOR00, $313, BPLCON0, $0200  						; 0 bitplanes
		dc.l	-2

LSP_Bank:
	EVEN
   	INCBIN		"assets/Bartesek - Hey Simone.lsbank"
	EVEN

; ------------------------- DATA PUBLIC section ---------------------------------
	data

LSP_Music:
	EVEN
   	INCBIN		"assets/Bartesek - Hey Simone.lsmusic"
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

    data
    END