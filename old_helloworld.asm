.inesprg 1   ; 1x 16KB PRG code
.ineschr 1   ; 1x  8KB CHR data
.inesmap 0   ; mapper 0 = NROM, no bank swapping
.inesmir 1   ; background mirroring

 LDA #$01
 STA $0200
 LDA #$05
 STA $0201
 LDA #$08
 STA $0202