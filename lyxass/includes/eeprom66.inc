****************
* EEPROM-routs
* for 93C46 (1024bit => 64 16-bit words)
*
* created : 11.05.95
* last modified :
* 
* 16.02.96      leaner (thanks to Harry)
* 12.03.96      test for busy after write and erase (well, Harry ;)) )
* 22.08.97      ported to ra65 for use with cc65
* 02.12.97	added xref for the new ra65
*     2010 93c66 support B. Spruck
* 2017 backport to lyxass by Bjoern Spruck
*
* (c) 1995..97 Bastian Schick
* CS    = A7 (18)
* CLK   = A1 (11)
* DI/DO = AUDIN (32)
*
*And now how to contact the EEPROM :
*
*CARD
*PORT               ----\/----      93C66(SMD too)
*(18)  A7   --------| CS     |- +5V
*(11)  A1   --------| CLK    |- NC
*               +---| DI     |- NC
*(32) AUDIN ----+---| DO     |- GND
*                   ----------
*
****************

* -------------------
* EEPROM command list
* -------------------
EE_C_WRITE      EQU    $14
EE_C_READ       EQU    $18
; EE_C_ERASE      EQU    $1C
EE_C_EWEN       EQU    $13
EE_C_EWEN2      EQU    $FF   ;; C0 schould be enough
EE_C_EWDS       EQU    $10
EE_C_EWDS2      EQU    $00

****************
* read 16bit Wort from address A
****************
EE_Read66::
                ldx #EE_C_READ
                jsr EE_Send11Bit
;>               jmp EE_Read16Bit                ; fall into
****************
* reads EEPROM-word to I2Cword
* A,Y destroyed
****************
EE_Read16Bit::
                lda #$a
                sta $fd8A       ; set AUDIN to Input
                
                clc
                stz I2Cword
                stz I2Cword+1
                ldy #15
.loop           EE_TOGGLE_CLK   ; CLK = 1
                EE_TOGGLE_CLK   ; CLK = 0
                lda $fd8b
                and #$10        ; mask bit
                adc #$f0        ; C=1 if A=$10
                rol I2Cword
                rol I2Cword+1   ; shifts 0 to Carry
                dey
                bpl .loop
                
                ldx #$1a
                stx $fd8a       ; set AUDIN for output

                EE_SET_CS_LOW

                rts

****************
* write word at address A, word in I2Cword
* A,X,Y, I2Cword destroyed
****************
EE_Write66::
                pha       ; save adress
                ldx #EE_C_EWEN  ; EWEN
                lda #EE_C_EWEN2  ; EWEN2
                jsr EE_Send11Bit
                pla
                ldx #EE_C_WRITE ; WRITE
                jsr EE_Send11Bit
                jsr EE_Send16Bit
                
                EE_SET_CS_HIGH
                lda #$0A
                sta $fd8a       ; AUDIN to input
                lda #$10
.wait           bit $fd8b       ; 'til ready :D0-read is /D0-written
                beq .wait
                lda #$1a        ; AUDIN to output
                sta $fd8a
                
                ldx #EE_C_EWDS  ; EWDS
                lda #EE_C_EWDS2  ; EWDS2
;>               bra EE_Send11Bit ; fall into
****************
* send A via I2C
* A,Y destroyed
****************
EE_Send11Bit::
		pha  
		txa 
                ;  Ok erstmal x abarbeiten und A sichern
                EE_SET_CS_LOW   ; CLK = 0 / CS = 0; EE_SET_CS_HIGH
                EE_SET_CS_HIGH

                ldy #2 ; 3 times
.loop            tax
                 and #$10
                 ora #$b
                 sta $fd8b
                 EE_TOGGLE_CLK  ; CLK = 1
                 EE_TOGGLE_CLK  ; CLK = 0
                 txa
                 rol
                 dey
                bpl .loop

                ldy #7 ; 8 times
		pla  ;; jetzt kommt a an die reihe

                ror
                ror
                ror           ; bit 7 at pos. 4
.loop2           tax
                 and #$10
                 ora #$b
                 sta $fd8b
                 EE_TOGGLE_CLK  ; CLK = 1
                 EE_TOGGLE_CLK  ; CLK = 0
                 txa
                 rol
                 dey
                bpl .loop2
                rts

****************
* send I2Cword to EEPROM
* A,Y and I2Cword destroyed
****************
EE_Send16Bit::
                lda I2Cword+1

                REPT 3
                ror
                ror I2Cword
                ENDR

                ldy #15
.loop            tax
                 and #$10
                 ora #$b
                 sta $fd8b
                 EE_TOGGLE_CLK  ; CLK = 1
                 EE_TOGGLE_CLK  ; CLK = 0
                 txa
                 rol I2Cword
                 rol
                 dey
                bpl .loop

                EE_SET_CS_LOW
                rts

