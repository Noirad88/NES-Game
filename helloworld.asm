no_attributes =     %00000000
top_of_screen =     $08
bottom_of_screen =  $08
nametable_max =     $383
direction_up =      $01
direction_down =    $02
direction_left =    $03
direction_right =   $04
moveAmount =        $08


CHANGE_BG_TILE MACRO               
               LDX $2002             ; read PPU status to reset the high/low latch
               LDX #$20
               STX $2006             ; write the high db of $2000 address
               LDX \1                ; low byte var is first
               STX $2006             ; write the low db of $2000 address
               LDX \2                ; tile hex
               STX $2007
               ENDM

RESET_SCROLL MACRO
             LDA world_scroll_x
             STA $2005
             LDA world_scroll_y
             STA $2005
             ENDM

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
  
low_byte_pointer    .rs 1
high_byte_pointer   .rs 1
player_x            .rs 1  ; .rs 1 means reserve one db of space 
player_y            .rs 1  ; .rs 1 means reserve one db of space 
player_vel          .rs 1
player_move_dir     .rs 1
low_byte_count      .rs 1
do_draw             .rs 1
world_scroll_x      .rs 1
world_scroll_y      .rs 1
nmi_flag            .rs 1
; variables for core drawing operaions
draw_loop_start_low_byte  .rs 1
draw_loop_start_high_byte .rs 1
draw_loop_width           .rs 1
draw_loop_height          .rs 1
draw_loop_tile_position   .rs 1
draw_loop_current_x       .rs 1
draw_loop_current_y       .rs 1

;;;;;;;;;;;;;;;

  .bank 1
  .org $E000

  .include "background_tiles"

metasprite:

	.byte $09,$03,$09,$04,$03,$09,$03,$01,$09
  .byte $09,$09,$09,$09,$09,$09,$09,$09,$09
  .byte $09,$09,$09,$09,$09,$09,$09,$09,$09
  .byte $09,$09,$09,$09,$09,$09,$09,$09,$09
  .byte $09,$09,$09,$09,$09,$09,$09,$09,$09
  .byte $09,$09,$09,$09,$09,$09,$09,$09,$09
  .byte $09,$09,$09,$09,$09,$09,$09,$09,$09
  .byte $09,$09,$09,$09,$09,$09,$09,$09,$09
  
  

background_palette:
  .db $0f,$17,$28,$36
  .db $0f,$04,$16,$25
  .db $0f,$17,$27,$3a
  .db $0f,$09,$19,$29
  
sprite_palette:
  .db $0f,$17,$28,$39
  .db $0f,$04,$16,$25
  .db $0f,$17,$27,$3a
  .db $0f,$09,$19,$29

sprites:
  .db top_of_screen, $05, no_attributes, $08   ;sprite 0
  .db top_of_screen, $06, no_attributes, $10   ;sprite 1
  .db $10, $15, no_attributes, $08   ;sprite 2
  .db $10, $16, no_attributes, $10   ;sprite 3

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

  LDA $0203        ;fetching player variables
  STA player_x
  LDA $0200
  STA player_y

  LDA #$00          ;setting our test variable
  STA do_draw
  STA world_scroll_x
  STA world_scroll_y

Sounds:
  
GameLoop: ;------------------------------------------------------------------------------
  
vblankwait3:       ; First wait for vblank to make sure PPU is ready
  BIT $2002
  BPL vblankwait3

  LDA #$00
  STA nmi_flag

CheckVelocityBeforeLatchingController:

  LDA #$00
  STA $2003       ; set the low db (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high db (02) of the RAM address, start the transfer
  
  ; first check if we can move; we shouldn't move if we're already moving
  LDX player_vel
  CPX #$00             
  BEQ LatchController 
  JMP MovePlayer 

LatchController:
  ; tell both the controllers to latch buttons
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016 

ReadA:

  LDA #$00
  STA do_draw
  LDA $4016       ; player 1 - A
  AND #%00000001  ; only look at bit 0
  BEQ ReadADone   ; branch to ReadADone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)
  LDA #$01
  STA do_draw

ReadADone:        ; handling this button is done

ReadB: 
  LDA $4016       ; player 1 - A
  AND #%00000001  ; only look at bit 0
  BEQ ReadBDone   ; branch to ReadADone if button is NOT pressed (0)
                  ; add instructions here to do something when button IS pressed (1)

ReadBDone:        ; handling this button is done

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
  BEQ ReadDown   ; branch to ReadADone if button is NOT pressed (0)

SetUp: ;set player up
  LDX #direction_up
  STX player_move_dir
  ;reset our velocity
  LDX #moveAmount
  STX player_vel
  JMP MovePlayer

ReadDown: 
  LDA $4016       ; player 1 - A
  AND #%00000001  ; only look at bit 0
  BEQ ReadLeft   ; branch to ReadADone if button is NOT pressed (0)

SetDown: ;set player down
  LDX #direction_down
  STX player_move_dir
  ;reset our velocity
  LDX #moveAmount
  STX player_vel
  JMP MovePlayer

ReadLeft: 
  LDA $4016       ; player 1 - A
  AND #%00000001  ; only look at bit 0
  BEQ ReadRight
  
SetLeft: ;set player left
  LDX #direction_left
  STX player_move_dir
  ;reset our velocity
  LDX #moveAmount
  STX player_vel
  JMP MovePlayer 

ReadRight: 
  LDA $4016       ; player 1 - A
  AND #%00000001  ; only look at bit 0
  BEQ MovePlayer

SetRight: ;set player right
  LDX #direction_right
  STX player_move_dir
  ;reset our velocity
  LDX #moveAmount
  STX player_vel

; set continues moves player in the respective direction for 8 pixels
MovePlayer:
  LDX player_vel
  CPX #$00             
  BNE MoveUp    ; if player is set to move, continue movement along 8 pixels
  JMP MovePlayerEnd

MoveUp:
  ;if this direction is not set, check next direction
  LDX player_move_dir    
  CPX #direction_up 
  BNE MoveDown 

  ;incrememnt the player positions (+8 for other 2, right sprites)
  LDX player_y
  DEX
  TXA
  STA player_y
  STA $0200
  STA $0204
  MACRO_ADD_A #$08
  STA $0208
  STA $020C       
  
  ; decrement the player 'velocity' so we know when to stop moving
  LDY player_vel
  DEY
  STY player_vel   
  JMP Animate 

MoveDown:
  LDX player_move_dir  
  CPX #direction_down 
  BNE MoveLeft            

  LDX player_y
  INX
  TXA
  STA player_y
  STA $0200
  STA $0204
  MACRO_ADD_A #$08
  STA $0208
  STA $020C
  
  LDY player_vel
  DEY
  STY player_vel   
  JMP Animate

MoveLeft:

  LDX player_move_dir    
  CPX #direction_left 
  BNE MoveRight            
  
  LDX player_x
  DEX
  TXA
  STA player_x
  STA $0203
  STA $020B
  MACRO_ADD_A #$08
  STA $0207
  STA $020F
  
  LDY player_vel
  DEY
  STY player_vel    
  JMP Animate

MoveRight:

  LDX player_move_dir   
  CPX #direction_right 
  BNE MovePlayerEnd            

  LDX player_x
  INX
  TXA
  STA player_x
  STA $0203
  STA $020B
  MACRO_ADD_A #$08
  STA $0207
  STA $020F
  
  LDY player_vel
  DEY
  STY player_vel  

Animate:
; we jump to here if we have moved, so this is the place where we animate

MovePlayerEnd:

  JMP GameLoop ;---------------------------------------------------------------------


NMI: ;start of our game loop ------------------------------------------------------
  
  LDA #$01        ; just test/checking NMI for understanding
  STA nmi_flag 

  LDA #%10010000  ;refresh PPU
  STA $2000
  LDA #%00011110   
  STA $2001

  ; done setting up PPU

DrawLogic:
  
  LDA do_draw
  CMP #$01
  BNE EndDrawLogic
  ; assigning relative variables
  LDA #$09
  STA draw_loop_width
  LDA #$2D
  STA draw_loop_start_low_byte
  LDA #$20
  STA draw_loop_start_high_byte
  LDA #$00
  STA draw_loop_current_x


  LDY #$00
  ;draw_loop_start_low_byte
  ;draw_loop_start_high_byte (constant)
  ;draw_loop_width (#$09)
  ;draw_loop_height (this should be calculated with start byte)
  ;draw_loop_tile position (Y)
  ;draw_loop_current_x
  ;draw_loop_current_y 

drawLoopInit:

  LDX $2002             ; read PPU status to reset the high/low latch
  LDX draw_loop_start_high_byte
  STX $2006             ; write the high db of $2000 address
  LDA draw_loop_start_low_byte
  STA $2006             ; write the low db of $2000 address

drawLoop:
  LDX metasprite, Y                 ; add tile on metasprite at Y in ppu
  STX $2007
  INY                               ;increment metasprite for tile

  LDA draw_loop_current_x           ;icrement current position in row
  MACRO_ADD_A #$01
  STA draw_loop_current_x 

  CMP draw_loop_width               ;compare current position in row with width of metasprite draw operation
  BNE drawLoop                      ; if y is not 9, we continue to loop.
                                    ; otherwise, we need to:
  LDA #$00                          ;reset current position (draw_loop_current_x )
  STA draw_loop_current_x

  LDA draw_loop_start_low_byte      ;increment 20 to low_byte_pointer
  MACRO_ADD_A #$20       
  STA draw_loop_start_low_byte
                                    ;has low_byte rolled back to 00?
  CMP #$ED                          ;compare current position OF row with height of metasprites draw operation
  BNE drawLoopInit                  ;if not, start drawing the new row
  

EndDrawLogic:
  RESET_SCROLL

  RTI ;end of NMI/Done drawing

;Vectors  

  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;label to jump to for NMI
  .dw RESET      ;label for when we first start the program
  .dw 0          ;external interrupt?

;Pattern

  .bank 2
  .org $0000
  .incbin "spriteSheet.chr"   ;includes 8KB graphics file from SMB1