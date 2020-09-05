no_attributes = %00000000
top_of_screen = $08
bottom_of_screen = $08
nametable_max = $383

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
  
low_byte_pointer   .rs 1
high_byte_pointer   .rs 1
player_x  .rs 1  ; .rs 1 means reserve one db of space 
player_y  .rs 1  ; .rs 1 means reserve one db of space 

;;;;;;;;;;;;;;;

  .bank 1
  .org $E000

  .include "background_tiles"

background_palette:
  .db $0f,$17,$28,$36
  .db $0f,$04,$16,$25
  .db $0f,$17,$27,$3a
  .db $0f,$09,$19,$29
  
sprite_palette:
  .db $0f,$17,$28,$36
  .db $0f,$04,$16,$25
  .db $0f,$17,$27,$3a
  .db $0f,$09,$19,$29

sprites:
  .db top_of_screen, $04, no_attributes, $08   ;sprite 0
  .db top_of_screen, $05, no_attributes, $10   ;sprite 1
  .db $10, $14, no_attributes, $08   ;sprite 2
  .db $10, $15, no_attributes, $10   ;sprite 3

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
  STA $2006             ; write the high db of $3F00 address
  LDA #$00
  STA $2006             ; write the low db of $3F00 address
  LDX #$00              ; start out at 0
LoadPalettesLoop:
  LDA background_palette, x        ; load data from address (palette + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$10              ; Compare X to hex $10, decimal 16 - copying 16 dbs = 4 sprites
  BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero  
  LDX #$00  ;reset the x register to zero so we can start loading sprite palette colors.    

LoadSpritePaletteLoop:
  LDA sprite_palette, x     ;load palette db
  STA $2007					;write to PPU
  INX                   	;set index to next db
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
  STA $2006             ; write the high db of $2000 address
  LDA #$00
  STA $2006             ; write the low db of $2000 address

  LDA #$00
  STA low_byte_pointer ;store low db of loop/background_tiles
  LDA #HIGH(background_tiles)
  STA high_byte_pointer ;store high db of loop/background_tiles
  
  LDX #$00            ; start at pointer + 0
  LDY #$00
LoadBackgroundOutsideLoop:

LoadBackgroundInsideLoop:
  LDA [low_byte_pointer], Y  ; copy one background db from address in pointer plus Y
  STA $2007           ; this runs 256 * 4 times

  INY                 ; inside loop counter
  CPY #$00
  BNE LoadBackgroundInsideLoop      ; run the inside loop 256 times before continuing down

  INC high_byte_pointer       ; low db went 0 to 256, so high db needs to be changed now

  INX
  CPX #$04
  BNE LoadBackgroundOutsideLoop     ; run the outside loop 256 times before continuing down

EnablingSpritesAndBackgrounds:
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0
  STA $2000

  LDA #%00011110   ; enable background and sprites
  STA $2001

  LDA #$00
  STA $2005
  STA $2005

  LDA $0203        ;fetching player variables
  STA player_x
  LDA $0200
  STA player_y


Foreverloop:
  JMP Foreverloop     ;jump back to Forever, infinite loop

NMI:

  LDA #$00
  STA $2003       ; set the low db (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high db (02) of the RAM address, start the transfer

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
  .incbin "spriteSheet.chr"   ;includes 8KB graphics file from SMB1