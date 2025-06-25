; Common macros
                IFND      MACROS_I

MACROS_I:       EQU       1
ALL:            REG       d0-a6

; version using CUSTOM in a0
VBLANK:         macro
                cmp.b     #$ff,6(a0)
                bne.s     *-6
                cmp.b     #$ff,6(a0)
                beq.s     *-6
                endm

; standalone version that does not need a0
VBLANKS:        macro
.v1\@:
                cmp.b     #$ff,$dff006
                bne.s     .v1\@
.v2\@:
                cmp.b     #$ff,$dff006
                beq.s     .v2\@
                endm

; Wait for raster N
VBLANKN:        macro
                cmp.b     #\1,6(a0)
                bne.s     *-6
                cmp.b     #\1,6(a0)
                beq.s     *-6
                endm

VBLANKNS:       macro
                cmp.b     #\1,$dff006
                bne.s     *-6
                cmp.b     #\1,$dff006
                beq.s     *-6
                endm

; wait for the given nr of frames
WAIT:           MACRO
                move      #\1,d0
.w\@:
                cmp.b     #$ff,6(a0)
                bne.s     *-6
                cmp.b     #$ff,6(a0)
                beq.s     *-6
                dbf       d0,.w\@
                ENDM

WAITS:          MACRO
                move      #\1,d0
.v1\@:          cmp.b     #$ff,$dff006
                bne.s     .v1\@
.v2\@:          cmp.b     #$ff,$dff006
                beq.s     .v2\@
                dbf       d0,.v1\@
                ENDM

	
; wait N rasters - standalone version without CUSTOM
RASTNS:         MACRO
                moveq     #\1,d7
.v1\@:          move.b    $dff006,d6
.v2\@:          cmpi.b    $dff006,d6
                beq.s     .v2\@
                dbf       d7,.v1\@
                ENDM

TESTLMB:        MACRO
.m\@:           btst.b    #6,$bfe001
                bne.s     .m\@
                ENDM


TESTRMB:        MACRO
.rmb\@:
                btst      #10,$dff016
                bne.b     .rmb\@:
                ENDM

; this macro tests whether any button was pressed in devices on port 0 or 1 and sets d0 accordingly (0=not pressed, -1 = pressed)
TESTANYBUTTON:  MACRO
                clr       d0
                lea       CIAA,a0
                move.b    ciapra(a0),d1
                btst      #CIAB_GAMEPORT0,d1    ; LMB port 0
                beq.b     .m1\@
                btst      #CIAB_GAMEPORT1,d1    ; LMB port 1
                beq.b     .m1\@
                lea       CUSTOM,a0
                move      POTGOR(a0),d1
                btst      #10,d1                ; RMB port 0
                beq.b     .m1\@
                btst      #14,d1                ; RMB port 1
                bne.b     .m2\@
.m1\@:
                st        d0                    ; something pressed
.m2\@:          tst       d0                    ; 0 - not pressed, -1 - pressed
                ENDM


;a0 - $dff000
WAITBLIT:       MACRO
.1\@:           btst      #14,DMACONR(a0)
                bne.b     .1\@
                ENDM

; Standalone version
WAITBLITS:      MACRO
.1\@:           btst      #14,CUSTOM+DMACONR
                bne.b     .1\@
                ENDM

                ENDC