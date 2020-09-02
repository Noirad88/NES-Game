no_attributes = %1000010
top_of_screen = $08

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
  .db $08, $3A, %00000000, $08   ;sprite 0
  .db $08, $37, %00000000, $10   ;sprite 1
  .db $10, $4f, %00000000, $08   ;sprite 2
  .db $10, $4f, %01000000, $10   ;sprite 3

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
                          ; 1st time through loop it will load palette+0
                          ; 2nd time through loop it will load palette+1
                          ; 3rd time through loop it will load palette+2
                          ; etc
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
                        ; if compare was equal to 32, keep going down 

  LDA #%10000000   ; enable NMI, sprites from Pattern Table 0
  STA $2000
  
  LDA #%00010000   ; enable sprites
  STA $2001

  LDA $0203
  STA player_x


Foreverloop:
  JMP Foreverloop     ;jump back to Forever, infinite loop

NMI:

  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer

  LDA player_x
  STA $0203
  STA $020B
  TAX
  CLC
  ADC #$08
  STA $0207
  STA $020F
  INX
  STX player_x

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