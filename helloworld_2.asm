sprites:
  .db top_of_screen,$3A,no_attributes,$08 
  .db $08,$37,no_attributes,$10
  .db $10,$4F,no_attributes,$08
  .db $10,$4F,no_attributes,$10

LoadSprites:
  LDX $00
LoadSpritesLoop:
  LDA sprites,X
  STA $0200,X
  INX
  CPX #$20
  BNE LoadSpritesLoop

  RTI