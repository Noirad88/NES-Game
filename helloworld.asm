no_attributes =         %00000000
top_of_screen =         $08
bottom_of_screen =      $08
nametable_max =         $383
direction_up =          $01
direction_down =        $02
direction_left =        $03
direction_right =       $04
moveAmount =            $08
drawingOperationPerFrame =  $02
drawBuffer =                $00F1  

ADD_DRAW_REQUEST_METASPRITE MACRO ; adds a set of ops to drawing buffer for a metasprite (collection of sprites)
                            PUSH_DRAWING_BUFFER #LOW(\1) ; sprite high byte
                            PUSH_DRAWING_BUFFER #HIGH(\1) ; sprite low byte
                            PUSH_DRAWING_BUFFER \3 ; pos high byte
                            PUSH_DRAWING_BUFFER \4 ; pos low byte
                            ENDM

ADD_DRAW_REQUEST_TILE       MACRO ; adds a set of ops to drawing buffer 
                            PUSH_DRAWING_BUFFER #$00 ; sprite high byte (tiles never have high bytes)
                            PUSH_DRAWING_BUFFER \1   ; sprite low byte
                            PUSH_DRAWING_BUFFER \2   ; pos high byte
                            PUSH_DRAWING_BUFFER \3   ; pos low byte
                            ENDM

PUSH_DRAWING_BUFFER    MACRO ; adds op to drawing buffer
                       lda \1
                       ldx drawBufferPointer
                       sta drawBuffer, X
                       inx
                       stx drawBufferPointer
                       ENDM

POP_DRAWING_BUFFER     MACRO ; removes the top item and assigns accumulator
                       ldx drawBufferPointer
                       lda #$00
                       dex
                       ldy drawBuffer, X
                       sta drawBuffer, X
                       stx drawBufferPointer
                       tya
                       ENDM

CLEAR_DRAWING_BUFFER   MACRO ; clears drawing buffer
                       ldx #$00
                       lda #$00
                       drawingLoop:
                       sta drawBuffer, X
                       inx
                       cpx #$16
                       bne drawingLoop
                       ldx #$00
                       stx drawBufferPointer
                       ENDM

;change a single tile (more efficient function if you're not drawing a metasprite) \1=tile hex, \2=low byte, \3=high byte
SET_BG_TILE MACRO               

            LDX $2002             ; read PPU status to reset the high/low latch
            LDX draw_loop_start_high_byte
            STX $2006             ; write the high db of $2000 address
            LDX draw_loop_start_low_byte                ; low byte var is first
            STX $2006             ; write the low db of $2000 address
            LDX draw_loop_sprite                ; tile hex
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

MACRO_SUB_A MACRO ;adds using CLC  
            CLC
            SBC \1
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

; variables for core drawing operaions
draw_loop_start_low_byte  .rs 1
draw_loop_start_high_byte .rs 1
draw_loop_width           .rs 1
draw_loop_height          .rs 1
draw_loop_height_counter  .rs 1
draw_loop_current_x       .rs 1
draw_loop_sprite_low_byte         .rs 1
draw_loop_sprite_high_byte         .rs 1
draw_loop_operation       .rs 1

  .rsset $00F0
drawBufferPointer      .rs 1

;;;;;;;;;;;;;;;

  .bank 1
  .org $E000

  .include "background_tiles"

;this is where we buffer drawings to be used in NMI

metasprite:

  ;first row is data: w, h,
  .byte $0b,$04
  
  ;the draw operation fetches the width first and initially sets to y 
	.byte $03,$02,$09,$02
  .byte $02,$02,$02,$02
  .byte $09,$09,$09,$09
  .byte $09,$09,$09,$09
  
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

InitiliazingVars:
  LDA $0200        ;fetching player variables
  STA player_x
  LDA $0200
  STA player_y
  
  LDA #$00          ;setting our test variable
  STA do_draw
  STA world_scroll_x
  STA world_scroll_y

Sounds:


;----------------------------------------------------------------------------------------












GameLoop: ;------------------------------------------------------------------------------
  
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

  ADD_DRAW_REQUEST_METASPRITE metasprite, #HIGH(metasprite), #$20, #$2D

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

Animate: ; we jump to here if we have moved, so this is the place where we animate

MovePlayerEnd:





vblankwait3:       ; First wait for vblank to make sure PPU is ready; keeps game at 60 (50?) fps
  BIT $2002
  BPL vblankwait3


  JMP GameLoop ;---------------------------------------------------------------------










NMI: ;start of our game loop ------------------------------------------------------
 
  pha ;hold our registers incase the NMI changes them
  tya
  pha
  txa
  pha

  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0
  STA $2000
  LDA #%00011110   ; enable background and sprites
  STA $2001


CheckDrawingBuffer:   ;checking what draw requests have been added to the stack, if any

  ;drawing buffer: 01do - 01ef
  ;draw metasprite: 01, 00, 00, 00
  ;draw tile:       02, 00, 00, 00
  
  ;to prevent running out of time, we limit the amount of drawing operations per frame (drawingOperationLimit)
  ;max, we probably only want to allow 2 drawing operations per frame.
  ;maybe one if we can get away with it.
  ;we'll need to fetch the stack pointer (TSX) next frame to continue from where we left off 
  ;if the current byte the stack pointer sits on is 00 on the next frame, then we have nothing to draw
  ; if its 01, its a metasprite operation
  ; if its 02, its a tile operation
  ; we'll need to clear these out once we've fetched them 

  ;1 - check the stack pointer
  ;1 -  if 0, nothing to do \ if not, we have to draw things
  ;3 -  

TestDraw:
  lda drawBufferPointer
  cmp #$00
  bne PrepareVars
  jmp EndDrawLogic

PrepareVars:

  ;op tile high low
  ;***instead of poping here, we could do it in the game loop first
  ; by passing to the stack, then passing to our buffer
  ;that way we save more nmi time
  
  POP_DRAWING_BUFFER
  sta draw_loop_start_low_byte
  POP_DRAWING_BUFFER
  sta draw_loop_start_high_byte
  POP_DRAWING_BUFFER
  sta draw_loop_sprite_low_byte
  POP_DRAWING_BUFFER
  sta draw_loop_sprite_high_byte
  cmp #$00                      ;00 = no high byte = tile
  bne DrawingMetaSpriteOperation

DrawingTileOperation:
  
  ldx $2002                                 ; read PPU status to reset the high/low latch
  ldx draw_loop_start_high_byte
  stx $2006                                 ; write the high db of $2000 address
  ldx draw_loop_start_low_byte              ; low byte var is first
  stx $2006                                 ; write the low db of $2000 address
  ldx draw_loop_sprite_low_byte                      ; tile hex
  stx $2007
  
  jmp EndDrawLogic
  
;setup vars from stack
DrawingMetaSpriteOperation:

  ldy draw_loop_start_low_byte
  lda (draw_loop_start_high_byte),Y

  ; assigning extra relative variables for metasprite
  ldx $2002                       ; read PPU status to reset the high/low latch
  ldx draw_loop_start_high_byte
  stx $2006                       ; write the high db of $2000 address
  ldx draw_loop_start_low_byte    ; low byte var is first
  LDY #$00
  LDA draw_loop_sprite_high_byte, Y         ;sprite data at 00 (w)
  STA draw_loop_width             ;store it in our var
  LDY #$01
  LDA draw_loop_sprite_high_byte, Y         ;sprite data at 01 (h)
  STA draw_loop_height            ;store it in our var
  LDA #$00
  STA draw_loop_height_counter
  LDA #$00
  STA draw_loop_current_x         ;this keeps track of our current x positon so we can check draw_loop_width
                                  ;to know when to jump to the next row
  ;we start to read the metasprite ahead 2 since the first two pos are w,h
  LDY #$02

drawLoopInit:

  LDX $2002                         ; read PPU status to reset the high/low latch
  LDX draw_loop_start_high_byte
  STX $2006                         ; write the high db of $2000 address
  LDA draw_loop_start_low_byte
  STA $2006                         ; write the low db of $2000 address

  drawLoop:
  LDX  (draw_loop_sprite_high_byte), Y                 ; add tile on metasprite at Y in ppu
  STX $2007
  INY                               ;increment metasprite for tile

  LDA draw_loop_current_x           ;increment current position in row
  MACRO_ADD_A #$01
  STA draw_loop_current_x 

  CMP draw_loop_width               ;compare current position in row with width of metasprite draw operation
  BNE drawLoop                      ; if y is not 9, we continue to loop.
                                    ; otherwise, we need to:
  LDA #$00                          ;reset current position (draw_loop_current_x )
  STA draw_loop_current_x
  LDX draw_loop_height_counter      ;increment the height_counter
  INX
  STX draw_loop_height_counter

                                    ;here, we need to get X to hold the low byte. after, we check if its less, which means it rolled back to #$00
  LDA draw_loop_start_low_byte      ;increment 20 to low_byte_pointer (20 moves the starting point to the new row)
  TAX
  MACRO_ADD_A #$20       
  STA draw_loop_start_low_byte
  CPX draw_loop_start_low_byte      ;has low_byte rolled back to 00?
  BCS increment_high_byte           ;then increment the high_byte
  JMP init_new_row

  increment_high_byte:
  LDX draw_loop_start_high_byte
  INX
  STX draw_loop_start_high_byte

  init_new_row:
  ;here, we check to see if the loop_height_counter has exceeded the loop_height
  LDA draw_loop_height_counter
  CMP draw_loop_height              ;compare current position of row with height of metasprites draw operation
  BNE drawLoopInit                  ;if not, start drawing the new row

EndDrawLogic:

  ;CLEAR_DRAWING_BUFFER

  RESET_SCROLL  ;reset the scroll

  pla ;restoring our registers before NMI started
  tax
  pla
  tay
  pla

  RTI ;end of NMI/Done drawing

;-----------------------------------------------------------------------------------------------------------







;Vectors  

  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;label to jump to for NMI
  .dw RESET      ;label for when we first start the program
  .dw 0          ;external interrupt?

;Pattern

  .bank 2
  .org $0000
  .incbin "spriteSheet.chr"   ;includes 8KB graphics file from SMB1