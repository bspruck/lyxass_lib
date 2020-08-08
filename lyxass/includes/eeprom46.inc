****************
* EEPROM-routs
* for 93C46 (1024bit => 64 16-bit words)
*
* created : 11.05.95
* last modified : 2017
* 
* 16.02.96      leaner (thanks to Harry)
* 12.03.96      test for busy after write and erase (well, Harry ;)) )
* 22.08.97      ported to ra65 for use with cc65
* 02.12.97	added xref for the new ra65
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
EE_C_WRITE      EQU    $40
EE_C_READ       EQU    $80
; EE_C_ERASE      EQU    $C0
EE_C_EWEN       EQU    $30
EE_C_EWDS       EQU    $00

****************
* read 16bit Wort from address A
****************
EE_Read46::
                and #$3f
                ora #EE_C_READ
                jsr EE_Send9Bit
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
EE_Write46::
                pha       ; save adress
                lda #EE_C_EWEN  ; EWEN
                jsr EE_Send9Bit
                pla
                and #$3f
                ora #EE_C_WRITE ; WRITE
                jsr EE_Send9Bit
                jsr EE_Send16Bit
                
                EE_SET_CS_HIGH
                lda #$0A
                sta $fd8a       ; AUDIN to input
                lda #$10
.wait           bit $fd8b       ; 'til ready :D0-read is /D0-written
                beq .wait
                lda #$1a        ; AUDIN to output
                sta $fd8a
                
                lda #EE_C_EWDS  ; EWDS
;>               bra EE_Send9Bit ; fall into
****************
* send A via I2C
* A,Y destroyed
****************
EE_Send9Bit::
                EE_SET_CS_LOW   ; CLK = 0 / CS = 0; EE_SET_CS_HIGH
                EE_SET_CS_HIGH

                ldy #8
                sec             ; start bit
                ror
                ror
                ror
                ror           ; bit 8 at pos. 4
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

