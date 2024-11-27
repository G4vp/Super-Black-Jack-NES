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

set_counters:
  LDA #$00
  STA dealer_counter_cards
  STA player_counter_cards

  ; We start sprite_counter at 6 because the first 24 bytes are reserved for the numbers.
  LDA #$0A
  STA sprite_counter

set_card_coords:
  LDA #$3E
  STA dealer_x
  LDA #$20
  STA dealer_y

  LDA #$26
  STA player_x
  LDA #$90
  STA player_y

set_numbers:
    LDA #$00
    STA rank_counter
    STA suit_counter

  LDA #$00
  STA cash_first_digit
  LDA #$02
  STA cash_second_digit


vblankwait2:
  BIT PPUSTATUS
  BPL vblankwait2


  LDA #%01
  STA card_color

  JMP main
.endproc

.segment "ZEROPAGE"
.importzp card_color, player_x, player_y, dealer_x, dealer_y, sprite_counter, player_counter_cards, dealer_counter_cards, rank_counter, suit_counter,cash_first_digit,cash_second_digit