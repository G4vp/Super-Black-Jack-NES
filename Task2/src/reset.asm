.include "constants.inc"

.segment "CODE"
.import main
.export reset_handler
.proc reset_handler
  SEI
  CLD
  LDX #$40
  STX $4017
  LDX #$FF
  TXS
  INX
  STX PPUCTRL
  STX PPUMASK
  STX $4010
  BIT PPUSTATUS
vblankwait:
  BIT PPUSTATUS
  BPL vblankwait

  LDX #$00
  LDA #$ff
clear_oam:
  STA $0200,X ; set sprite y-positions off the screen
  INX
  INX
  INX
  INX
  BNE clear_oam

vblankwait2:
  BIT PPUSTATUS
  BPL vblankwait2

  LDA #$36
  STA player_x
  LDA #$30
  STA player_y
  LDA #%01
  STA color

  JMP main
.endproc

.segment "ZEROPAGE"
.importzp player_x, player_y, color