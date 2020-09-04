no_attributes = %00000000
top_of_screen = $08
bottom_of_screen = $08

MACRO_ADD_A MACRO ;adds using CLC  
            CLC
            ADC \1
            ENDM

  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring

;;;;;;;;;;;;;;;
;DECLARE SOME VARIABLES HERE
  .rsset $0000  ;start variables at ram location 0
  
player_x  .rs 1  ; .rs 1 means reserve one byte of space 
player_y  .rs 1  ; .rs 1 means reserve one byte of space 

;;;;;;;;;;;;;;;

  .bank 1
  .org $E000
background_palette:
  .db $22,$24,$1A,$0F	;background palette 1
  .db $22,$32,$18,$0F	;background palette 2
  .db $22,$31,$21,$0F	;background palette 3
  .db $22,$25,$16,$0F	;background palette 4
  
sprite_palette:
  .db $22,$27,$14,$42	;sprite palette 1
  .db $22,$2A,$20,$27	;sprite palette 2
  .db $22,$36,$30,$47	;sprite palette 3
  .db $22,$2F,$56,$17 ;sprite palette 4

sprites:
  .db top_of_screen, $3A, no_attributes, $08   ;sprite 0
  .db top_of_screen, $37, no_attributes, $10   ;sprite 1
  .db $10, $4f, %00000000, $08   ;sprite 2
  .db $10, $4f, %01000000, $10   ;sprite 3

  .include "background_tiles.asm" ;our background map

  .include "background_tiles_attributes.asm" ;our background map attributes

RESET:
  SEI          ; disable IRQs
  CLD          ; disable decimal mode
  LDX #$40	
  STX $4017    ; disable APU frame IRQ
  LDX #$FF	
  TXS          ; Set up stack
  INX          ; now X = 0
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering
  STX $4010    ; disable DMC IRQs

vblankwait1:       ; First wait for vblank to make sure PPU is ready
  BIT $2002
  BPL vblankwait1

clrmem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0300, x
  INX
  BNE clrmem
   
vblankwait2:      ; Second wait for vblank, PPU is ready after this
  BIT $2002
  BPL vblankwait2

LoadPalettes:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006             ; write the high byte of $3F00 address
  LDA #$00
  STA $2006             ; write the low byte of $3F00 address
  LDX #$00              ; start out at 0
LoadPalettesLoop:
  LDA background_palette, x        ; load data from address (palette + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$10              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero  
  LDX #$00  ;reset the x register to zero so we can start loading sprite palette colors.    

LoadSpritePaletteLoop:
  LDA sprite_palette, x     ;load palette byte
  STA $2007					;write to PPU
  INX                   	;set index to next byte
  CPX #$10            
  BNE LoadSpritePaletteLoop  ;if x = $10, all done
  LDX #$00              ; start at 0
LoadSpritesLoop:
  LDA sprites, x        ; load data from address (sprites +  x)
  STA $0200, x          ; store into RAM address ($0200 + x)
  INX                   ; X = X + 1
  CPX #$10              ; Compare X to hex $10, decimal 16
  BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero
  LDX #$00 

LoadBackground:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address
  LDX #$00              ; start out at 0
LoadBackgroundLoop:
  LDA background_tiles, x     ; load data from address (background + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$256           ; Compare X to hex $80, decimal 128 - copying 128 bytes
  BNE LoadBackgroundLoop  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero
                        ; if compare was equal to 128, keep going down

  LDX #$00
LoadAttribute:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$23
  STA $2006             ; write the high byte of $23C0 address
  LDA #$C0
  STA $2006             ; write the low byte of $23C0 address
  LDX #$00  ; start out at 0
LoadAttributeLoop:
  LDA background_tiles_attributes, x      ; load data from address (attribute + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$40              ; Compare X to hex $08, decimal 8 - copying 8 bytes
  BNE LoadAttributeLoop

  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0
  STA $2000

  LDA #%00011110   ; enable background and sprites
  STA $2001

  LDA $0203        ;fetching player variables
  STA player_x
  LDA $0200
  STA player_y


Foreverloop:
  JMP Foreverloop     ;jump back to Forever, infinite loop

NMI:

  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer

LatchController:
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016       ; tell both the controllers to latch buttons

ReadA: 
  LDA $4016       ; player 1 - A
  AND #%00000001  ; only look at bit 0
  BEQ ReadADone   ; branch to ReadADone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)

ReadADone:        ; handling this button is done

ReadSelect: 
  LDA $4016       ; player 1 - A
  AND #%00000001  ; only look at bit 0
  BEQ ReadSelectDone   ; branch to ReadADone if button is NOT pressed (0)

ReadSelectDone:

ReadStart: 
  LDA $4016       ; player 1 - A
  AND #%00000001  ; only look at bit 0
  BEQ ReadStartDone   ; branch to ReadADone if button is NOT pressed (0)

ReadStartDone:

ReadUp: 
  LDA $4016       ; player 1 - A
  AND #%00000001  ; only look at bit 0
  BEQ ReadUpDone   ; branch to ReadADone if button is NOT pressed (0)
  BNE MoveUp

MoveUp: ;move player down
  LDA player_y        ;fetch player y position variable
  STA $0200           ;get y position of accumulator and apply to top-left sprite
  STA $0204           ;get y position of accumulator and apply to top-right sprite
  TAX                 ;transfer accumulator to X to store original position
  MACRO_ADD_A #$08    ;add 8 to accumulator
  STA $0208           ;get y position of accumulator and apply to bottom-left sprite
  STA $020C           ;get y position of accumulator and apply to bottom-right sprite
  DEX                 ;decrement X 
  STX player_y        ;store X to player y position variable (i.e., next loop will technically push player)

ReadUpDone:

ReadDown: 
  LDA $4016       ; player 1 - A
  AND #%00000001  ; only look at bit 0
  BEQ ReadDownDone   ; branch to ReadADone if button is NOT pressed (0)
  BNE MoveDown

MoveDown: ;move player down
  LDA player_y
  STA $0200
  STA $0204
  TAX
  MACRO_ADD_A #$08
  STA $0208
  STA $020C
  INX
  STX player_y

ReadDownDone:

ReadLeft: 
  LDA $4016       ; player 1 - A
  AND #%00000001  ; only look at bit 0
  BNE MoveLeft
  JMP ReadLeftDone   ; branch to ReadADone if button is NOT pressed (0)
  
MoveLeft: ;move player left
  LDA player_x
  STA $0203
  STA $020B
  TAX
  MACRO_ADD_A #$08
  STA $0207
  STA $020F
  DEX
  STX player_x  

ReadLeftDone:

ReadRight: 
  LDA $4016       ; player 1 - A
  AND #%00000001  ; only look at bit 0
  BNE MoveRight
  JMP ReadRightDone ; branch to ReadADone if button is NOT pressed (0)

MoveRight: ;move player right
  LDA player_x
  STA $0203
  STA $020B
  TAX
  MACRO_ADD_A #$08
  STA $0207
  STA $020F
  INX
  STX player_x

ReadRightDone:

  RTI

;;;;;;;;;;;;;;  

  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used in this tutorial
  
;;;;;;;;;;;;;;  

  .bank 2
  .org $0000
  .incbin "mario.chr"   ;includes 8KB graphics file from SMB1