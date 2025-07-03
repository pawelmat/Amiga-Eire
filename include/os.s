; Pawel Matusz / Kane (kane@konto.pl)
; OS related functions
; 

; ------------------------------------------
; Save key OS variables, interrupts etc. so that later we can graecfull exit back to the OS
OsSave:
        lea     configBlock(pc),a4
		clr.l	VBR_BASE(a4)                ;get vbr base
		move.l	4.w,a6                      ; execbase
		move	$128(a6),d0			        ; AttnFlags
        andi    #$f,d0                      ; lower 4 bits specify the CPU type (up to 040)
        beq.s   .mc68k
		lea		getVbr(pc),a5				; on >68000 get VPR and CACR
		jsr		-30(a6)						;exec Supervisor(a5 - function)
.mc68k:
		
	    ;get gfx and dos bases
		lea		gfxName(pc),a1
		jsr		-408(a6)					;exec OldOpenLibrary(a1 - lib name)
		move.l	d0,GFX_LIB(a4)				;osGfxBase

	    ;save old view
		move.l	d0,a6
		move.l  $22(a6),OS_VIEW(a4)         ; save copper

	    ;steal blitter to avoid blitter + copjmp nasty bug
		jsr		-228(a6)	                ;gfx WaitBlit()
		jsr		-456(a6)	                ;gfx OwnBlitter() ; note: not calling disownblitter() after this will hang asm one
		
        lea		CUSTOM,a0
		move.w	INTENAR(a0),d0			    ; os intena: 602c: INTEN + EXTER + VERTB + PORTS + SOFT
		ori		#$C000,d0				    ; add the "set" bit
		move.w	d0,OS_INTENA(a4)
		move.w 	DMACONR(a0),d0			    ; os dmacon: 03f0: DMAEN | BPLEN | COPEN | BLTEN | SPREN | DSKEN
		andi	#$0ff0,d0				    ; prevent showing read only bits and remove audio
		ori		#DMAF_SETCLR,d0				; add the "set" bit
		move.w	d0,OS_DMACON(a4)
		VBLANK
		move.w	#$7fff,d0
		move.w	d0,DMACON(a0)	            ; all dma off
		move.w	d0,INTENA(a0)	            ; disable interrupts
		move.w	d0,INTREQ(a0)	            ; clear pending ints

	    ;store interrupts - PORTS and VERTB
		move.l	VBR_BASE(a4),a5	;get vbr base
		move.l	$68(a5),OS_INT_PORTS(a4)	; L2
		move.l	$6c(a5),OS_INT_VERTB(a4)	; L3
		move.l	$78(a5),OS_INT_LEV6(a4)		; L6

		lea		SPR0POS(a0),a1              ; clear all sprites so that they don't randomly "blink" on some screens (even though sprite DMA should be off)
		moveq	#16-1,d0					
		moveq	#0,d1
.clspr:		
		move.l	d1,(a1)+
		dbf		d0,.clspr

		rts

; ------------------------------------------
; Restore all OS variables, interrupts etc. It is assumed that stack is be left intact 
OsRestore:
        lea     configBlock(pc),a4
		lea		CUSTOM,a0
		move.w	#$7fff,d0
		move.w	d0,DMACON(a0)	            ; dma off
		move.w	d0,INTENA(a0)	            ; disable ints
		move.w	d0,INTREQ(a0)	            ; clear pending ints
        clr.l    d0
		move.l	d0,AUD0VOL(a0)			    ; all volume to 0 and dat to 0
		move.l	d0,AUD1VOL(a0)
		move.l	d0,AUD2VOL(a0)
		move.l	d0,AUD3VOL(a0)
		move.w	#$00ff,ADKCON(a0)		    ; clear audio         

	    ;restore ints pointers (PORTS)
		move.l	VBR_BASE(a4),a3	            ;get vbr base
		move.l	OS_INT_PORTS(a4),$68(a3)	; L2
		move.l	OS_INT_VERTB(a4),$6c(a3)	; L3
		move.l	OS_INT_LEV6(a4),$78(a3)		; L6

	    ;restore hardware regs
		move.w	OS_INTENA(a4),INTENA(a0)
		move.w	OS_DMACON(a4),DMACON(a0)

		move.l	GFX_LIB(a4),a6
		jsr		-228(a6)	;gfx WaitBlit()
		jsr		-462(a6)	;gfx DisownBlitter()

	    ;restore old view (copper list)
		move.l	OS_VIEW(a4),a1
		jsr		-222(a6)	;gfx LoadView(a1 - view)
		jsr		-270(a6)	;gfx WaitTOF()
		jsr		-270(a6)	;gfx WaitTOF()

	    ;restore system clist .gb_copinit
		lea		CUSTOM,a0
		move.l	$26(a6),COP1LC(a0)
		move.l	$2a(a6),COP2LC(a0)

	    ;close lib
		move.l	4.w,a6
		move.l	GFX_LIB(a4),a1	;library base
		jsr		-414(a6)		;exec CloseLibrary(a1 - lib name)
		rts	

getVbr:
		; IN: A4 - config block
		;movec	VBR,d0			            ; $4e7a0801
        dc.l    $4e7a0801
		move.l	d0,VBR_BASE(a4)				; save vbrBase
		rte

    EVEN
; ----------- CONFIG BLOCK indices 
	RSRESET
	OS_INTENA:			rs.w	1
	OS_DMACON:			rs.w	1
	OS_INT_PORTS:		rs.l	1		        ; OS L2 interrupt
	OS_INT_VERTB:		rs.l	1		        ; OS L3 interrupt
	OS_INT_LEV6:		rs.l	1		        ; OS L6 interrupt
	OS_VIEW:			rs.l	1
	GFX_LIB:			rs.l	1
	VBR_BASE:			rs.l	1
	CB_LENGTH:			rs.w	0		        ; length of config block

; ----------- Local data
configBlock: 	    ds.b	CB_LENGTH
	EVEN
gfxName:	        dc.b	'graphics.library',0
    EVEN
