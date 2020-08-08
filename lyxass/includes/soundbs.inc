
echo "------------"
echo "Chipper Code v 12.3.2013"
echo "included miniplayer 29.9.2013"
sound_code_begin	equ *
****************
SndInit::
                php
                sei
                lda #%10011000|_31250Hz
                sta $fd01+SND_TIMER*4
                lda #129
                sta $fd00+SND_TIMER*4           ; set up a 240Hz IRQ
                SETIRQVEC SND_TIMER,SndIRQ

                stz $fd20
                stz $fd28
                stz $fd30
                stz $fd38       ; all volumes zero

;	lda 	#$FF			; Wert alle Soundkan„le Panning aktivieren
;	sta 	$fd44			; alle Soundkan„le Panning aktivieren
;	stz 	$fd50			; alle Soundkan„le anschalten
;	stz		$fd40			; Soundkanal 1 Lautst„rke init
;	stz		$fd41			; Soundkanal 2 Lautst„rke init
;	stz		$fd42			; Soundkanal 3 Lautst„rke init
;	stz		$fd43			; Soundkanal 4 Lautst„rke init

                stz $fd44       ; all channels full volume / no attenuation
                lda #$ff
                stz $fd50       ; all channels on

                lda #0 ; %01011000
                sta $fd20+5
                sta $fd28+5
                sta $fd30+5
                sta $fd38+5

; 				LYNX_CHIP_INIT

                ldx #3
                lda #0
.0                stz SndActive,x
                  stz SndReqStop,x
                  stz SndEnvVol,x
                  stz SndEnvFrq,x
                  stz SndEnvWave,x
                  stz SndMute,x
;>                  stz SndEnvPly,x
                  ldy SndOffsets,x
                  sta SndChannel+2,y
                  dex
                bpl .0
		stz SndRetAFlag
                stz SndSema
                plp
                rts

SndOffsets      dc.b $00,$08,$10,$18

****************
*SampleIRQ::
*	lda (SndSamplePtr)
*	sta da-converter
*	dec SndSampleLen
*	_IFEQ
*		dec SndSampleLen+1
*		_IFEQ
*			Sample deaktivieren
*		_ENDIF
*	_ENDIF
*	inc SndSamplePtr
*	_IFEQ
*		inc SndSamplePtr+1
*	_ENDIF
*	END_IRQ

SndIRQ::
                lda #$ff
                tsb SndSema
                _IFNE
;                  dec $fdb0
                  END_IRQ
                _ENDIF
;				inc $fda0
                phy
                jsr SndSetValues ; *NOW* set all values which were "pre-set" in last interrupt
                cli

		lda SndRetAFlag   ; reset the return flag, but save it first
		_IFNE
		  lda #$0F ; MASK
		sta SndRetAFlag2
		stz SndRetAFlag
		_ENDIF

                ldx #3
.0                phx
                  lda SndActive,x
                  _IFNE
                    _IFNE {SndEnvVol,x}
                      phx
                      jsr SndChangeVol
;;					  dec $FDA0
                      plx
                    _ENDIF
                    _IFNE {SndEnvFrq,x}
                      phx
                      jsr SndChangeFrq
                      plx
                    _ENDIF
                    _IFNE {SndEnvWave,x}
                      phx
                      jsr SndChangeWave
                      plx
                    _ENDIF
                    jsr SndGetCmd
                  _ENDIF
                  plx
                  dex
                bpl .0
IFD MINI_PLAYER
                ;; Now the miniplayer

mini_snd_player:
    lda mini_snd_ptr+1
    beq ms_ende2
    
    lda mini_snd_cnt
    bne ms_ende

    stz mp_flag
    
ms_loop:
        ldy #1
        lda (mini_snd_ptr),y
        bmi ms_stop_sound

IF 	MINI_PLAY_CHAN=0
ENDIF
IF 	MINI_PLAY_CHAN=1
        clc         ;; play on 
        adc #$08    ;; channel 3 .. we use 0 now
ENDIF
IF 	MINI_PLAY_CHAN=2
        clc         ;; play on 
        adc #$10    ;; channel 3 .. we use 0 now
ENDIF
IF 	MINI_PLAY_CHAN=3
        clc         ;; play on 
        adc #$18    ;; channel 3 .. we use 0 now
ENDIF

        pha
        and #$1F
        tax
        pla
        bit #$60
        beq endif2
            pha
            and #$18
            sta mp_offset
            pla
            bit #$20
            beq endif3
                inc mp_flag
                bra endif3
else3
                ; bit #$40
                ; Sound Timer stoppen
                phx
                ldx mp_offset
                stz $fd25,x
                plx
endif3
endif2
        iny
        lda (mini_snd_ptr),y
        sta mp_backup,x
        sta $FD20,x

        lda mp_flag
        beq endif1
            ; Sound Timer starten
            ldx mp_offset
            lda 5+mp_backup,x
            sta $fd25,x
            stz mp_flag
endif1
        clc
        lda #3
        adc mini_snd_ptr
        sta mini_snd_ptr
        bcc ms_noc
        inc mini_snd_ptr+1
ms_noc
        lda (mini_snd_ptr)
        beq ms_loop

;;        asl ;; von 240 Hz auf 60 Hz
;;        asl
ms_ende:
    dea
    sta mini_snd_cnt
    bra ms_ende2

ms_stop_sound:
    stz mini_snd_ptr+1
    stz SndMute+MINI_PLAY_CHAN ;; enable channel MINI_PLAY_CHAN
    lda #$FF
    sta SndChannel+2+MINI_PLAY_CHAN*8 ;; channel MINI_PLAY_CHAN reset values
ms_ende2:

ENDIF
                sei
                ply
                stz SndSema
                END_IRQ
****************
* SndGetCmd
SndGetCmd::
                lda SndReqStop,x
                bne SndStop

;>                _IFNE {SndReqStop,x}
;>                  stz SndActive,x
;>                  rts
;>                _ENDIF

		    lda SndRetAFlag2
		    and SndMask,x
		    _IFNE
		      eor SndRetAFlag2
		      sta SndRetAFlag2
		      lda SndRetAddr,x
		      sta SndPtrLo,x
		      lda SndRetAddr+4,x
		      sta SndPtrHi,x
                      ;;; force the direct continue return
		    _ELSE
		      dec SndDelay,x
		      bne .991 ;; check special case
		    _ENDIF

                lda SndPtrLo,x
                sta SndPtrTmp
                lda SndPtrHi,x
                sta SndPtrTmp+1

.0              lda (SndPtrTmp)
                beq SndStop
                _IFMI
                  and #$7f
                  tay
                  jsr SndCallCmd
                _ELSE
                  jsr SndNewNote
                _ENDIF

                clc
                tya
                and #$7f
                adc SndPtrTmp
                sta SndPtrLo,x
                sta SndPtrTmp
                lda #0
                adc SndPtrTmp+1
                sta SndPtrHi,x
                sta SndPtrTmp+1

                tya
                bmi .0 ; Process next command if no delay

.991
		;; now check if delay is only 1 AND next one is return all.
		lda #1
		cmp SndDelay,x
		bne .99

		;; NOW read ahead ONE
                lda SndPtrLo,x
                sta SndPtrTmp
                lda SndPtrHi,x
                sta SndPtrTmp+1

                lda (SndPtrTmp)
		cmp #$92 ;; Return all
		_IFEQ
		  sta SndRetAFlag ; just set !=0
		_ENDIF
.99             rts


SndCallCmd::    lda SndCmdsHi,y
                pha
                lda SndCmdsLo,y
                pha
                ldy #1
                rts
*
SndStop::
                stz SndReqStop,x
                stz SndActive,x
                ldy SndOffsets,x
				lda #0
                sta SndChannel,y
				inc ;lda #1
                sta SndChannel+2,y
				tay ;ldy #1
                rts
*
SndNewNote::
; Note,length,volume

                phx  ; Channel
                  sta SndNotePlaying,x ; a is always !=0

                  pha ; Freq Index
                  ldy #1
                  lda (SndPtrTmp),y         ; laenge
                  sta SndDelay,x

                  ldy SndOffsets,x
                  lda SndVolume,x
                  sta SndChannel,y

                  plx ; Freq Index
SndModPrescale:
                  lda $4242,x ;; was SndPrescaler,x
                  sta SndChannel+5,y
SndModReload:
                  lda $4242,x ;; was SndReload,x
                  sta SndChannel+4,y
                  lda #-1
                  sta SndChannel+2,y

                plx ; Channel
                _IFNE {SndEnvVol,x}
                  jsr SndSetEnvVol1
                _ENDIF
                _IFNE {SndEnvFrq,x}
                  jsr SndSetEnvFrq1
                _ENDIF
                _IFNE {SndEnvWave,x}
                  jsr SndSetEnvWave1
                _ENDIF
                ldy #$2
;;;>                dec SndNotePlaying,x
;;;>                ldy #$81
;		lda #$2
;		ldy SndDelay,x
;		_IFNE  ;; soll das nicht IFEQ sein??
;		  ora #$80
;		_ENDIF
;		tay
                rts
*
SndLoop::
                lda (SndPtrTmp),y
                sta SndLoopCnt,x
                lda SndPtrTmp
                sta SndLoopPtrLo,x
                lda SndPtrTmp+1
                sta SndLoopPtrHi,x
                ldy #$82
                rts
*
SndDo::
		lda SndLoopCnt,x
		beq .loop_always
                dec SndLoopCnt,x
                _IFNE
.loop_always
                  lda SndLoopPtrLo,x
                  sta SndPtrTmp
                  lda SndLoopPtrHi,x
                  sta SndPtrTmp+1
                  ldy #$82
                _ELSE
                  ldy #$81
                _ENDIF
                rts

SndSetNoteTab::
                lda (SndPtrTmp),y
                sta SndModPrescale+1
                iny
                lda (SndPtrTmp),y
                sta SndModPrescale+2
                iny
                lda (SndPtrTmp),y
                sta SndModReload+1
                iny
                lda (SndPtrTmp),y
                sta SndModReload+2
                ldy #$85
                rts


SndDefEnvVol::
*
                phx
                lda (SndPtrTmp),y               ; env #
                tax

                iny
                lda (SndPtrTmp),y
                sta SndEnvVolPtrLo,x
                iny
                lda (SndPtrTmp),y
                sta SndEnvVolPtrHi,x            ; Ptr to [cnt,inc]

                ldy #$84
                plx
                rts
*
SndSetEnvVol::
                lda (SndPtrTmp),y               ; # env

SndSetEnvVol1   and #$7f
                sta SndEnvVol,x                 ; save
                _IFEQ
                  ldy #$82
                  rts
                _ENDIF

                tay

                lda SndEnvVolPtrLo,y
                sta SndEnvPtr
                lda SndEnvVolPtrHi,y
                sta SndEnvPtr+1

                lda (SndEnvPtr)
                sta SndTmp
                asl
                sta SndEnvVolLoop,x             ; here is the loop-start

                ldy #1 ;; Attention, this is needed
                lda (SndEnvPtr),y
                sta SndEnvVolParts,x
                sec
                sbc SndTmp
                sta SndEnvVolParts2,x

                stz SndEnvVolCnt,x
                lda #2
                sta SndEnvVolOff,x

                ldy #$82
                rts
*
SndDefEnvFrq::
*
                phx
                lda (SndPtrTmp),y               ; env #
                tax

                iny
                lda (SndPtrTmp),y
                sta SndEnvFrqPtrLo,x
                iny
                lda (SndPtrTmp),y
                sta SndEnvFrqPtrHi,x            ; Ptr to [inc,cnt]
                plx
                ldy #$84
                rts
*
SndSetEnvFrq::
                lda (SndPtrTmp),y               ; # env

SndSetEnvFrq1   and #$7f
                sta SndEnvFrq,x                 ; save
                _IFEQ
                  ldy #$82
                  rts
                _ENDIF

                tay

                lda SndEnvFrqPtrLo,y
                sta SndEnvPtr
                lda SndEnvFrqPtrHi,y
                sta SndEnvPtr+1

                lda (SndEnvPtr)
                sta SndTmp
                asl
                sta SndEnvFrqLoop,x

                ldy #1 ;; this is needed
                lda (SndEnvPtr),y
                sta SndEnvFrqParts,x
                sec
                sbc SndTmp
                sta SndEnvFrqParts2,x

                stz SndEnvFrqCnt,x
                lda #2
                sta SndEnvFrqOff,x

                ldy #$82
                rts

*
SndDefEnvWave::
*
                phx
                lda (SndPtrTmp),y               ; env #
                tax

                iny
                lda (SndPtrTmp),y
                sta SndEnvWavePtrLo,x
                iny
                lda (SndPtrTmp),y
                sta SndEnvWavePtrHi,x            ; Ptr to [inc,cnt]
                plx
                ldy #$84
                rts
*
SndSetEnvWave::
                lda (SndPtrTmp),y               ; # env

SndSetEnvWave1   and #$7f
                sta SndEnvWave,x                 ; save
                _IFEQ
                  ldy #$82
                  rts
                _ENDIF

                tay

                lda SndEnvWavePtrLo,y
                sta SndEnvPtr
                lda SndEnvWavePtrHi,y
                sta SndEnvPtr+1

                lda (SndEnvPtr)
                sta SndTmp
                asl ; *4 -2
				dec
                asl
                sta SndEnvWaveLoop,x

                ldy #1 ;; thsi is needed
                lda (SndEnvPtr),y
                sta SndEnvWaveParts,x
                sec
                sbc SndTmp
                sta SndEnvWaveParts2,x

                stz SndEnvWaveCnt,x
                lda #2
                sta SndEnvWaveOff,x

                ldy #$82
                rts

*
SndPause::
                lda (SndPtrTmp),y
                sta SndDelay,x
                iny
SndDummy        rts

* This set the new Player Freq instantanious!!!
SndPlayerFreq::
;                phx
                lda (SndPtrTmp),y
                sta $fd01+SND_TIMER*4   ;;; Prescale
                iny
                lda (SndPtrTmp),y
                sta $fd00+SND_TIMER*4   ;; Reload
;;                plx
                ldy #$83
                rts
*


IFD CHIP_SAMPLE_PLAY
PlaySampleOnce::	; A ist Index des Samples
	php
	sei

	asl ; words
	tax
	ldy #3
	lda SndOffsets,y
	sta ch1_adroffset

	lda	sam_start_table,x	; 1 Byte Startadresse Sample laden
	sta	ch1_start			; 1 Byte Startadresse Sample schreiben
	lda	sam_start_table+1,x	; 2 Byte Startadresse Sample laden
	sta	ch1_start+1			; 2 Byte Startadresse Sample schreiben
	lda	sam_end_table,x		; 1 Byte Endadresse Sample laden
	sta	ch1_end				; 1 Byte Endadresse Sample schreiben
	lda	sam_end_table+1,x	; 2 Byte Endadresse Sample laden
	sta	ch1_end+1			; 2 Byte Endadresse Sample schreiben

	stz	ch1_repeat			; Sample Repeat Wert schreiben

	lda	#1					; Vorkommaanteil laden
	sta	ch1_off1			; Vorkommaanteil schreiben
	stz	ch1_off2			; 1 Byte Nachkommaanteil schreiben
	stz ch1_off3

	ldx ch1_adroffset
	stz $fd20+5,x                 ; stop sound timer

	plp
	rts

*
SndPlaySample::
				php
				sei
				phx

				; Sample Interrupt DeAktivieren
				; Nehmen wir doch einfach mal den HBL

                lda SndOffsets,x
				sta ch1_adroffset
                lda (SndPtrTmp),y
				tax
				; Samplenummer

				lda	sam_start_table,x	; 1 Byte Startadresse Sample laden
				sta	ch1_start			; 1 Byte Startadresse Sample schreiben
				lda	sam_start_table+1,x	; 2 Byte Startadresse Sample laden
				sta	ch1_start+1			; 2 Byte Startadresse Sample schreiben
				lda	sam_end_table,x		; 1 Byte Endadresse Sample laden
				sta	ch1_end				; 1 Byte Endadresse Sample schreiben
				lda	sam_end_table+1,x	; 2 Byte Endadresse Sample laden
				sta	ch1_end+1			; 2 Byte Endadresse Sample schreiben

				lda	repeat_table,x			; Sample Repeat Wert laden
				sta	ch1_repeat				; Sample Repeat Wert schreiben
				beq	.noloop					; Weiter wenn Repeat Wert = 0
					lda	rep_start_table,x	; 1 Byte Repeat Startadresse Sample laden
					sta	ch1_rep_start		; 1 Byte Repeat Startadresse Sample schreiben
					lda	rep_start_table+1,x	; 2 Byte Repeat Startadresse Sample laden
					sta	ch1_rep_start+1		; 2 Byte Repeat Startadresse Sample schreiben
					lda	rep_end_table,x		; 1 Byte Repeat Endadresse Sample laden
					sta	ch1_rep_end			; 1 Byte Repeat Endadresse Sample schreiben
					lda	rep_end_table+1,x	; 2 Byte Repeat Endadresse Sample laden
					sta	ch1_rep_end+1		; 2 Byte Repeat Endadresse Sample schreiben
.noloop

                iny
                lda (SndPtrTmp),y 		; "Note"
				tax
				lda	period_vor,x		; Vorkommaanteil laden
				sta	ch1_off1			; Vorkommaanteil schreiben
				lda	period_nach,x		; 1 Byte Nachkommaanteil laden
				sta	ch1_off2			; 1 Byte Nachkommaanteil schreiben
				stz ch1_off3


;				lda	volume_table,x		; Lautst„rke laden
;				sta	(volume)			; Lautst„rke setzen

				ldx ch1_adroffset
                stz $fd20+5,x                 ; stop sound timer

;               stz SndChannel+$18+2
;				stz SndActive+3

				; Sample Interrupt Aktivieren
				plx
				plp
                ldy #$83
                rts
ENDIF
*
SndNoteOff::
                ldy SndOffsets,x
                stz SndNotePlaying,x
                lda SndEnvVol,x
                ora #$80
                sta SndEnvVol,x
                lda SndEnvFrq,x
                ora #$80
                sta SndEnvFrq,x
                lda SndEnvWave,x
                ora #$80
                sta SndEnvWave,x
                lda #0
                sta SndChannel,y
                sta SndChannel+4,y
                sta SndChannel+5,y
                dec
                sta SndChannel+2,y
                ldy #$81
                rts
*
SndSetInstr::
                phx
                lda SndOffsets,x
                tax
                lda (SndPtrTmp),y
                sta SndChannel+3,x
                iny
                lda (SndPtrTmp),y
                sta SndChannel+7,x
                iny
                lda (SndPtrTmp),y
                sta SndChannel+1,x
                plx
                iny
                lda (SndPtrTmp),y
                sta SndVolume,x
                iny
                lda (SndPtrTmp),y
                sta SndMaxVolume,x

                ldy #$86
                rts
*
SndCallPattern::
                clc
                lda SndPtrTmp
                adc #3
                sta SndRetAddr,x
                lda SndPtrTmp+1
                adc #0
                sta SndRetAddr+4,x
                ;;ldy #1;; set byCmdCall
                lda (SndPtrTmp),y
                pha
                iny
                lda (SndPtrTmp),y
                sta SndPtrTmp+1
                pla
                sta SndPtrTmp
                ldy #$80
                rts
*
SndRetToSong::
                lda SndRetAddr,x
                sta SndPtrTmp
                lda SndRetAddr+4,x
                sta SndPtrTmp+1
                ldy #$80
                rts
*
SndReturnAll:: ;;; never come here
		lda #1
		sta SndRetAFlag
                sta SndDelay,x
                ldy #$0
		rts

*
SndNewNote2::
; Note,length,volume

                ;; phx ; Channel
                  sta SndNotePlaying,x
                  ;;;ldy #1 ;; actually CallCmd sets it to 1
                  lda (SndPtrTmp),y             ; reload
                  pha
                  iny
                  lda (SndPtrTmp),y             ; prescale
                  pha
                  iny
                  lda (SndPtrTmp),y         ; laenge
                  sta SndDelay,x

                  ldy SndOffsets,x
                  lda SndVolume,x
                  sta SndChannel,y

                  pla
                  sta SndChannel+5,y
                  pla
                  sta SndChannel+4,y
                  lda #-1
                  sta SndChannel+2,y

                ;; plx ; Channel
                _IFNE {SndEnvVol,x}
                  jsr SndSetEnvVol1
                _ENDIF
                _IFNE {SndEnvFrq,x}
                  jsr SndSetEnvFrq1
                _ENDIF
                _IFNE {SndEnvWave,x}
                  jsr SndSetEnvWave1
                _ENDIF
                ldy #4
                rts


SndSetStereo::
                ;;ldy #1 ;; actually CallCmd sets it to 1
                lda (SndPtrTmp),y
				sta $FD50
			    ldy #$82
				rts

SndSetAttenuationOn::
                ;;ldy #1 ;; actually CallCmd sets it to 1
                lda (SndPtrTmp),y
				sta $FD44
			    ldy #$82
				rts

SndSetChnAttenuation::
                ;;ldy #1 ;; actually CallCmd sets it to 1
                lda (SndPtrTmp),y
				sta $FD40,x
			    ldy #$82
				rts


****************
                MACRO LO
                dc.b <((\0)-1)
                ENDM

                MACRO HI
                dc.b >((\0)-1)
                ENDM

SndCmdsLo::     LO SndLoop              ; count
                LO SndDo
                LO SndPause             ; count
                LO SndNoteOff
                LO SndSetInstr          ; shiferinit1,shifterinit2,feedback
                LO SndNewNote2             ;
                LO SndCallPattern           ; SndCallSubr
                LO SndRetToSong             ; SndRetSubr

                LO SndDefEnvVol
                LO SndSetEnvVol
                LO SndDefEnvFrq
                LO SndSetEnvFrq
                LO SndDefEnvWave
                LO SndSetEnvWave

                LO SndSetStereo
                LO SndSetAttenuationOn
                LO SndSetChnAttenuation

                LO SndPlayerFreq
                LO SndReturnAll             ; all channels, NOT "USED"
                LO SndSetNoteTab

;				LO SndPlaySample



SndCmdsHi::
                HI SndLoop
                HI SndDo
                HI SndPause
                HI SndNoteOff
                HI SndSetInstr
                HI SndNewNote2
                HI SndCallPattern           ; SndCallSubr
                HI SndRetToSong             ; SndRetSubr

                HI SndDefEnvVol
                HI SndSetEnvVol
                HI SndDefEnvFrq
                HI SndSetEnvFrq
                HI SndDefEnvWave
                HI SndSetEnvWave

                HI SndSetStereo
                HI SndSetAttenuationOn
                HI SndSetChnAttenuation

		        HI SndPlayerFreq
                HI SndReturnAll             ; all channels, NOT "USED"
                HI SndSetNoteTab

;				HI SndPlaySample

*
SndChangeVol::
                tay
                _IFMI
.99               rts
                _ENDIF
                lda SndNotePlaying,x
                beq .99

                lda SndEnvVolPtrLo,y
                sta SndEnvPtr
                lda SndEnvVolPtrHi,y
                sta SndEnvPtr+1

    ;;dec SndEnvVolCnt,x
    ;;_IFMI
    lda SndEnvVolCnt,x
    and #$7F
    _IFEQ
                  dec SndEnvVolParts,x
                  _IFMI
                    _IFNE {SndEnvVolLoop,x}
                      tay
                      lda SndEnvVolParts2,x
                      sta SndEnvVolParts,x
                      bra .1v
                    _ELSE
                      tya
                      ora #$80
                      sta SndEnvVol,x
                    _ENDIF
                  _ELSE
                    ldy SndEnvVolOff,x
.1v                 lda (SndEnvPtr),y
                    sta SndEnvVolCnt,x
                    iny
                    lda (SndEnvPtr),y
                    sta SndEnvVolInc,x
                    iny
                    tya
                    sta SndEnvVolOff,x
                  _ENDIF
				  rts
				_ENDIF
;               _ELSE
    dec SndEnvVolCnt,x

                  ldy SndOffsets,x
                  clc
                  lda SndEnvVolInc,x
    _IFNE
                  _IFPL
                    adc SndChannel,y
                    cmp SndMaxVolume,x
                    _IFCS
                      lda SndMaxVolume,x
                      stz SndEnvVolInc,x
                    _ENDIF
                    cmp #$80
                    _IFGE
                      lda #$7f
                      stz SndEnvVolInc,x
                    _ENDIF
                 _ELSE
                   adc SndChannel,y
                   _IFCC
				     lda #0
                     sta SndEnvVolInc,x
					 ; NEU: switch Tremolo off
                	 sta SndNotePlaying,x
                ;      tya
                ;      ora #$80
                ;      sta SndEnvVol,x
                   _ENDIF
                   cmp #$80
                   _IFGE
				     lda #0
                     sta SndEnvVolInc,x
					 ; NEU: switch Tremolo off
                     sta SndNotePlaying,x
                ;      tya
                ;      ora #$80
                ;      sta SndEnvVol,x
                   _ENDIF
                 _ENDIF
;;				  sta $FDB0 ;;; debug -> chnage green
                  sta SndChannel,y
                  lda SndChannel+2,y
                  ora #1 ;; if already -1 ... no effect
                  sta SndChannel+2,y
    _ENDIF
                rts

*
SndChangeFrq::
                tay
                _IFMI
.99               rts
                _ENDIF
                lda SndNotePlaying,x
                beq .99

                lda SndEnvFrqPtrLo,y
                sta SndEnvPtr
                lda SndEnvFrqPtrHi,y
                sta SndEnvPtr+1

;;                dec SndEnvFrqCnt,x
;;                _IFMI
            lda SndEnvFrqCnt,x
            sta SndTmp
            and #$7F
            _IFEQ
                  dec SndEnvFrqParts,x
                  _IFMI
                    _IFNE {SndEnvFrqLoop,x}
                      tay
                      lda SndEnvFrqParts2,x
                      sta SndEnvFrqParts,x
                      bra .1f
                    _ELSE
                      tya
                      ora #$80
                      sta SndEnvFrq,x
;                      rts
                    _ENDIF
                  _ELSE
                    ldy SndEnvFrqOff,x
.1f                 lda (SndEnvPtr),y
                    sta SndEnvFrqCnt,x
                    iny
                    lda (SndEnvPtr),y
                    ;;; eor #$ff ;;negate value? useless, do it in chipper!!!
                    ;;; inc
                    sta SndEnvFrqInc,x
                    iny
                    tya
                    sta SndEnvFrqOff,x
                  _ENDIF
                  rts
                _ENDIF
;				_ELSE
    dec SndEnvFrqCnt,x

;                  phy
                  ldy SndOffsets,x
				  clc
                  lda SndEnvFrqInc,x
                  _IFEQ
;                    ply
                    rts
                  _ENDIF

    adc SndChannel+4,y
    pha
    lda SndTmp
                  _IFMI
        lda SndEnvFrqInc,x
        _IFMI
            pla
                    _IFPL
                      pha
                      lda SndChannel+5,y
                      _IFNE
                        dec
                        sta SndChannel+5,y
                        pla
                    eor #128  ;; effective an and $7F?  better asl?
                      _ELSE
                        pla
                        pha
                        clc
                        adc SndEnvFrqInc,x
;                    _IFCC
;                          stz SndEnvFrqInc,x
;                    _ENDIF
                        pla
                      _ENDIF
                    _ELSE
                      pha
                      lda SndChannel+5,y
                      _IFEQ
                        pla
;                        stz SndEnvFrqInc,x
                        sta SndChannel+4,y
;                        ply
                        rts
                      _ENDIF
                      pla
                    _ENDIF
                  _ELSE
            pla
                    _IFPL
                      pha
                      lda SndChannel+5,y
                      cmp #6
                      _IFNE
                        inc
                        sta SndChannel+5,y
                        pla
                      eor #128             ; effective an or $80 ?
                      _ELSE
                        lda SndChannel+4,y
                        _IFMI
;                          stz SndEnvFrqInc,x
                          pla
;                          ply
                          rts
                        _ENDIF
                        pla
                      _ENDIF
                    _ELSE
                      pha
                      lda SndChannel+5,y
                      cmp #6
                      _IFEQ
                        lda SndChannel+4,y
                        _IFPL
;                          stz SndEnvFrqInc,x
                          pla
;                          ply
                          rts
                        _ENDIF
                      _ENDIF
                      pla
                    _ENDIF
                  _ENDIF
    _ELSE
        pla ;; Just wrap around and dont modify Prescale
    _ENDIF

                  sta SndChannel+4,y

                  lda SndChannel+2,y
                  ora #$40 ;; if already -1 -> no effect 
                  sta SndChannel+2,y
                rts

*

SndChangeWave::
                tay
				;; Ab hier x Kanal 0-3, y Environment
                _IFMI
.99               rts
                _ENDIF
                lda SndNotePlaying,x
                beq .99


                lda SndEnvWavePtrLo,y
                sta SndEnvPtr
                lda SndEnvWavePtrHi,y
                sta SndEnvPtr+1

;;                dec SndEnvWaveCnt,x
;;                _IFMI
                lda SndEnvWaveCnt,x
                _IFEQ
                  dec SndEnvWaveParts,x
                  _IFMI
                    _IFNE {SndEnvWaveLoop,x}
                      tay
					  ;; Ab hier x Kanal 0-3, y Offset im Environment
                      lda SndEnvWaveParts2,x
                      sta SndEnvWaveParts,x
                      bra .1v
                    _ELSE
                      tya
                      ora #$80 ;; beende Env
                      sta SndEnvWave,x
                    _ENDIF
;;					rts ;; dont set anything
                  _ELSE
                    ldy SndEnvWaveOff,x
				;; Ab hier x Kanal 0-3, y Offset im  Environment
.1v                 lda (SndEnvPtr),y
                    sta SndEnvWaveCnt,x

				  phx
                  lda SndOffsets,x
				  tax
				;; Ab hier x Kanal (0-3)*8, y Offset im  Environment

                    iny
                    lda (SndEnvPtr),y
					sta SndChannel+3,x  ; Shift LO
					iny
                    lda (SndEnvPtr),y
					sta SndChannel+7,x   ; Shift HI
					iny
                    lda (SndEnvPtr),y
					sta SndChannel+1,x  ; Feedback
                    iny
                    tya
					ply
				;; Ab hier x Kanal (0-3)*8,  y Kanal 0-3
                    sta SndEnvWaveOff,y
                  lda #-1 ;; stop timer to set new values...
                  sta SndChannel+2,x
                  _ENDIF
                  rts
				_ENDIF
;;                _ELSE
                dec SndEnvWaveCnt,x
                rts

*
***************
* SndSetValues
* set new values for all channels
* This is the main set function called in the Snd-"env" IRQ

SndSetValues::
                ldx #4-1
.0
                  lda SndMute,x
                  _IFEQ                        ; flag != 0 => don`t set
                ldy SndOffsets,x
                  lda SndChannel+2,y
                  _IFNE                        ; flag == 0 => don`t set
;>                    lda $fd25,y
;>                    and #$ef
;>                    sta $fd25,y

		    bit #$80
                    _IFNE                       ;
                      lda #0
                      sta $fd25,y                 ; stop sound timer

                      lda SndChannel+3,y
                      sta $fd23,y                 ; shifter 1
                      ;;lda $fd27,y
                      ;;and #$0F
                      ;;ora SndChannel+7,y          ; shifter 2
                      lda SndChannel+7,y          ; shifter 2
                      sta $fd27,y
                      lda SndChannel+1,y
                      sta $fd21,y                 ; feedback
                    _ENDIF

                    lda SndChannel,y
                    sta $fd20,y                 ; volume
                    lda SndChannel+2,y
		    bit #$C0
                    _IFNE                       ;
                    lda SndChannel+4,y
                    sta $fd24,y                 ; reload
                    lda SndChannel+5,y
                    ora #%00011000 ;;; #%01011000
		    ;; and #%00111111
                    sta $fd25,y                 ; re-enable timer
                    _ENDIF

                    lda #0
                    sta SndChannel+2,y          ; clear flag
    _ENDIF
    _ENDIF

                dex
		_IFPL
		    jmp .0
		_ENDIF
;;                bpl .0
                rts
****************
SndStartSound::
*
                php
                pha
                _IFNE {SndActive,x}
                  dec SndReqStop,x
                  lda #1
                  sta SndDelay,x
.0                lda SndActive,x
                  bne .0
                _ENDIF
                bra .1
SndStartSoundx
                php
                pha

.1              sei
                pla
                sta SndPtrLo,x
                tya
                sta SndPtrHi,x
                lda #1
                sta SndDelay,x
                stz SndEnvVol,x
                stz SndEnvFrq,x
                stz SndEnvWave,x
;>                stz SndEnvPly,x
                sta SndActive,x
                stz SndReqStop,x
                plp
                rts

****************
SndStartSound2::
* x - default channel
* search for a free channel, if found use it
* else use default
                pha

                lda SndActive,x         ; check default
                beq .0                  ; inactive => ok
                phx
                ldx #3                  ; search free channel
.1                lda SndActive,x
                  beq .2                ; found =>
                  dex
                bpl .1
                plx                     ; not found
                dec SndReqStop,x        ; stop default-channel
                lda #1
                sta SndDelay,x
.3                lda SndActive,x
                bne .3
                bra .0
.2              pla             ; clear stack

.0              pla
                phx
                jsr SndStartSoundx      ; launch new sound
                plx
                rts
****************
SndStopAll::
                ldx #3
                  _IFNE {SndActive,x}
.0                  dec SndReqStop,x
                    lda #1
                    sta SndDelay,x
.1                  lda SndActive,x
                    bne .1
                  _ENDIF
                  dex
                bpl .0
                rts
****************
SndStopChannel::
                _IFNE {SndActive,x}
                  dec SndReqStop,x
                  lda #1
                  sta SndDelay,x
.1                lda SndActive,x
                  bne .1
                _ENDIF
                rts
*****************
; Tell me which channels are on
SndChannelsActive:: 
                ldx #3
                lda #0
.0                ldy SndActive,x
                  _IFNE
                    ora SndMask,x
                  _ENDIF
                  dex
                bpl .0
                rts

SndMask::       dc.b 1,2,4,8
*****************
SndPauseOn::    php
                sei
                lda $fd01+SND_TIMER*4
                sta SndPauseOff1+1
                stz $fd01+SND_TIMER*4
                lda $fd50
                sta SndPauseOff2+1
                lda #$ff
                sta $fd50
                lda #$18
                trb $fd25
                trb $fd25+8
                trb $fd25+16
                trb $fd25+24
                plp
                rts

SndPauseOff::   php
                sei
SndPauseOff1    lda #0 ; Selbsmodifizierter Code!!!
                sta $fd01+SND_TIMER*4
SndPauseOff2    lda #0 ; Selbsmodifizierter Code!!!
                sta $fd50

                lda #$18
                tsb $fd25
                tsb $fd25+8
                tsb $fd25+16
                tsb $fd25+24

                plp
                rts


sound_code_end	equ *

sound_zp_len	set	sound_zp_end - sound_zp_begin
sound_mem_len	set	sound_mem_end - sound_mem_begin
sound_code_len	set	sound_code_end - sound_code_begin

echo "Sound ZP: %Dsound_zp_len"
echo "Sound MEM: %Dsound_mem_len"
echo "Sound Code: %Dsound_code_len"
echo "------------"
