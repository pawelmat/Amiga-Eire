; Original version: https://github.com/Kalmalyzer/kalms-c2p/blob/main/normal/c2p1x1_4_c5_gen.s
; Changes include interleaved bitmap support and tweaking for the particular use case.
; This is a CPU based 68k version, slow for the A500, used only for static c2p conversions

BPLX	EQU	320
BPLY	EQU	64
BPLROW	EQU	BPLX/8

; a0	chunky source
; a1	bitplanes
; d0    chunky size
c2p1x1_4_c5:
	movem.l	d0-d7/a0-a6,-(sp)

	add.w	#BPLROW,a1

    lea     c2p1x1_4_c5_end(pc),a5
    add.l   a0,d0
    move.l  d0,(a5)         ; end of chunky buffer
    lea     BPLX+32(a0),a2     ; end of chunky row 1

	move.l	#$33333333,d5
	move.l	#$55555555,a5
	move.l	#$00ff00ff,a6


	move.l	(a0)+,d0		; Merge 4x1
	lsl.l	#4,d0
	or.l	(a0)+,d0
	move.l	(a0)+,d1
	lsl.l	#4,d1
	or.l	(a0)+,d1

	move.l	(a0)+,d2
	lsl.l	#4,d2
	or.l	(a0)+,d2
	move.l	(a0)+,d3
	lsl.l	#4,d3
	or.l	(a0)+,d3

	move.w	d2,d6			; Swap 16x2
	move.w	d3,d4
	move.w	d0,d2
	move.w	d1,d3
	swap	d2
	swap	d3
	move.w	d2,d0
	move.w	d3,d1
	move.w	d6,d2
	move.w	d4,d3

	move.l	d2,d6			; Swap 2x2
	move.l	d3,d7
	lsr.l	#2,d6
	lsr.l	#2,d7
	eor.l	d0,d6
	eor.l	d1,d7
	and.l	d5,d6
	and.l	d5,d7
	eor.l	d6,d0
	eor.l	d7,d1
	lsl.l	#2,d6
	lsl.l	#2,d7
	eor.l	d6,d2
	eor.l	d7,d3

	move.l	a6,d4			; Swap 8x1
	move.l	d1,d6
	move.l	d3,d7
	lsr.l	#8,d6
	lsr.l	#8,d7
	eor.l	d0,d6
	eor.l	d2,d7
	and.l	d4,d6
	and.l	d4,d7
	eor.l	d6,d0
	eor.l	d7,d2
	lsl.l	#8,d6
	lsl.l	#8,d7
	eor.l	d6,d1
	eor.l	d7,d3

	bra.s	.start
.x:
	move.l	(a0)+,d0		; Merge 4x1
	lsl.l	#4,d0
	or.l	(a0)+,d0
	move.l	(a0)+,d1
	lsl.l	#4,d1
	or.l	(a0)+,d1

	move.l	(a0)+,d2
	lsl.l	#4,d2
	or.l	(a0)+,d2
	move.l	(a0)+,d3
	lsl.l	#4,d3
	or.l	(a0)+,d3

	move.l	d6,BPLROW(a1)

	move.w	d2,d6			; Swap 16x2
	move.w	d3,d4
	move.w	d0,d2
	move.w	d1,d3
	swap	d2
	swap	d3
	move.w	d2,d0
	move.w	d3,d1
	move.w	d6,d2
	move.w	d4,d3

	move.l	d7,-BPLROW(a1)

	move.l	d2,d6			; Swap 2x2
	move.l	d3,d7
	lsr.l	#2,d6
	lsr.l	#2,d7
	eor.l	d0,d6
	eor.l	d1,d7
	and.l	d5,d6
	and.l	d5,d7
	eor.l	d6,d0
	eor.l	d7,d1
	lsl.l	#2,d6
	lsl.l	#2,d7
	eor.l	d6,d2
	eor.l	d7,d3

	move.l	a3,BPLROW*2(a1)

	move.l	a6,d4			; Swap 8x1
	move.l	d1,d6
	move.l	d3,d7
	lsr.l	#8,d6
	lsr.l	#8,d7
	eor.l	d0,d6
	eor.l	d2,d7
	and.l	d4,d6
	and.l	d4,d7
	eor.l	d6,d0
	eor.l	d7,d2
	lsl.l	#8,d6
	lsl.l	#8,d7
	eor.l	d6,d1
	eor.l	d7,d3

	move.l	a4,(a1)+
.start:
	move.l	a5,d4			; Swap 1x1
	move.l	d1,d6
	move.l	d3,d7
	lsr.l	#1,d6
	lsr.l	#1,d7
	eor.l	d0,d6
	eor.l	d2,d7
	and.l	d4,d6
	and.l	d4,d7
	eor.l	d6,d0
	eor.l	d7,d2
	add.l	d6,d6
	add.l	d7,d7
	eor.l	d1,d6
	eor.l	d3,d7

	move.l	d0,a3
	move.l	d2,a4

    cmpa.l  c2p1x1_4_c5_end(pc),a0
    beq.s   .end
	cmpa.l	a0,a2
	bne	    .x
    lea     3*BPLROW(a1),a1
    lea     BPLX(a2),a2
	bra	    .x
.end:
	move.l	d6,BPLROW(a1)
	move.l	d7,-BPLROW(a1)
	move.l	a3,BPLROW*2(a1)
	move.l	a4,(a1)+

	movem.l	(sp)+,d0-d7/a0-a6
	rts

c2p1x1_4_c5_end:    dc.l    0

