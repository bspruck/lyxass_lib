****************
* EEPROM-routs
* for 93C46 (1024bit => 128 8-bit words)
*
* created : 11.05.95
* last modified : 2017
* 
* 16.02.96      leaner (thanks to Harry)
* 12.03.96      test for busy after write and erase (well, Harry ;)) )
* 22.08.97      ported to ra65 for use with cc65
* 02.12.97  added xref for the new ra65
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
*PORT               ----\/----      93C46(SMD too)
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
EE_C_WRITE      EQU    $10
EE_C_READ       EQU    $18
; EE_C_ERASE      EQU    $C0
EE_C_EWEN       EQU    $10
EE_C_EWEN2       EQU   $7F
EE_C_EWDS       EQU    $10
EE_C_EWDS2       EQU   $00

****************
* read 8bit Wort from address A
****************
EE_Read46_8Bit::
                ldx #EE_C_READ
		and #$7F
                jsr EE_Send10Bit
;>               jmp EE_Read8Bit                ; fall into
****************
* reads EEPROM-word to I2Cword
* A,Y destroyed
****************
EE_Read8Bit:
                lda #$a
                sta $fd8A       ; set AUDIN to Input
                
                clc
                stz I2Cword
                ldy #7
.loop           EE_TOGGLE_CLK   ; CLK = 1
                EE_TOGGLE_CLK   ; CLK = 0
                lda $fd8b
                and #$10        ; mask bit
                adc #$f0        ; C=1 if A=$10
                rol I2Cword
                dey
                bpl .loop
                
                ldx #$1a
                stx $fd8a       ; set AUDIN for output

                EE_SET_CS_LOW

                rts

****************
* write byte at address A
****************
EE_Write46_8Bit::
                pha       ; save adress
                ldx #EE_C_EWEN  ; EWEN
                lda #EE_C_EWEN2  ; EWEN2
                jsr EE_Send10Bit
                pla
                ldx #EE_C_WRITE ; WRITE
		ora #$80
                jsr EE_Send10Bit
                jsr EE_Send8Bit

EE_wait         EE_SET_CS_HIGH
                lda #$0A
                sta $fd8a       ; AUDIN to input
                lda #$10
.wait           bit $fd8b       ; 'til ready :D0-read is /D0-written
                beq .wait
                lda #$1a        ; AUDIN to output
                sta $fd8a

                ldx #EE_C_EWDS  ; EWDS
                lda #EE_C_EWDS2  ; EWDS2
;>               bra EE_Send10Bit ; fall into
****************
* send X,A via I2C
* A,Y destroyed
****************
EE_Send10Bit::
		pha  
		txa   ;; Ok erstmal x abarbeiten und A sichern
                EE_SET_CS_LOW   ; CLK = 0 / CS = 0; EE_SET_CS_HIGH
                EE_SET_CS_HIGH

                ldy #1 ; 2 times
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
EE_Send8Bit::
                lda I2Cword

                ror
                ror
                ror
                ldy #7
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

                EE_SET_CS_LOW
                rts

