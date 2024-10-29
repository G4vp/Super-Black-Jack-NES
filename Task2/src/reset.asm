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

  LDA #$80
  STA player_x
  LDA #$a0
  STA player_y

  JMP main
.endproc

.segment "ZEROPAGE"
.importzp player_x, player_y