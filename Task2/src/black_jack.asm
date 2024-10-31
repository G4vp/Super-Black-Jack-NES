.include "constants.inc"
.include "header.inc"

.segment "CODE"
.proc irq_handler
  RTI
.endproc

.import read_controller1
.import reset_handler

.proc nmi_handler
  ; save registers
  PHA
  TXA
  PHA
  TYA
  PHA

  LDA #$00
  STA OAMADDR
  LDA #$02
  STA OAMDMA

  ; read controller
  JSR read_controller1
  JSR update_hands

  LDA #$00
  STA PPUSCROLL
  STA PPUSCROLL

  ; restore registers
  PLA
  TAY
  PLA
  TAX
  PLA

  RTI
.endproc

.export main
.proc main

  LDX PPUSTATUS
  LDX #$3f
  STX PPUADDR
  LDX #$00
  STX PPUADDR
  load_palettes:
    LDA palettes,X
    STA PPUDATA
    INX
    CPX #$20
    BNE load_palettes    

  load_attribute_table:
    ; finally, attribute table
    LDA PPUSTATUS
    LDA #$23
    STA PPUADDR
    LDA #$c2
    STA PPUADDR
    LDA #%00000000
    STA PPUDATA

    LDA PPUSTATUS
    LDA #$23
    STA PPUADDR
    LDA #$e0
    STA PPUADDR
    LDA #%00000000
    STA PPUDATA

  vblankwait:       ; wait for another vblank before continuing
    BIT PPUSTATUS
    BPL vblankwait

    LDA #%10010000  ; turn on NMIs, sprites use first pattern table
    STA PPUCTRL
    LDA #%00011110  ; turn on screen
    STA PPUMASK

  forever:
    JMP forever
.endproc

  ; Draw card: display a card on the screen with its sprites and background.
  ; Receives X, Y, card_color (00 or 01), Card Rank (number), and Card Suit (symbol).
.proc draw_card
  LDA card_x
  STA offset_x

  LDA card_rank
  CLC
  ADC #$05
  STA card_rank

  LDA card_set
  CLC
  ADC #$11
  STA card_set

  LDA card_color
  BEQ color_is_zero
  JMP load_sprites

  ; Move the sprite 3 pixels to the left if the color changes.
  color_is_zero:
  LDA card_x
  CLC
  SBC #03
  STA offset_x
  

  load_sprites:
    ; Multiply 8 * counter_cards
    LDA counter_cards
    ASL A
    ASL A
    ASL A
    TAX

    ; Number Tile
    LDA card_y
    STA $0200, X
    INX
    LDA card_rank
    STA $0200, X
    INX
    LDA card_color
    STA $0200, X
    INX
    LDA offset_x
    STA $0200, X


    ; Bottom Tile (Y + 8):
    INX
    LDA card_y
    CLC
    ADC #$08
    STA $0200, X
    INX
    LDA card_set
    STA $0200, X
    INX
    LDA card_color
    STA $0200, X
    INX
    LDA offset_x
    STA $0200, X

    ; add 1 to counter
    LDX counter_cards 
    INX
    STX counter_cards

  convert_xy_coords_to_nt: 

  ; X: XXXXX***
  ; Y: YYYYY***
  ; NT address: 0010NNYY YYYXXXXX

  ; All you have to do is a little bit shifting, ANDing and ORing to get the bits where they need to be.
    LDA #%00001000 ; base value for $2000 (change lower 2 bits for other NTs)
    STA high_byte
    LDA card_y
    AND #%11111000
    ASL
    ROL high_byte
    ASL
    ROL high_byte
    STA low_byte
    LDA card_x
    LSR
    LSR
    LSR
    ORA low_byte
    STA low_byte
    JSR draw_background_card

  ; return to where the subroutine was called
  RTS
.endproc

.proc draw_background_card

  LDA PPUSTATUS ; LEFT TOP
  LDA high_byte
  STA PPUADDR
  LDA low_byte
  STA PPUADDR
  LDX #$04
  STX PPUDATA

  LDA PPUSTATUS   
  LDA low_byte ; RIGHT TOP
  CLC 
  ADC #$01
  TAX
  LDA high_byte
  ADC #$00
  STA PPUADDR
  STX PPUADDR
  LDX #$05
  STX PPUDATA

  LDA PPUSTATUS ; LEFT CENTER (ADD $20 to low byte and if set carry add it to high byte)
  LDA low_byte
  CLC 
  ADC #$20
  TAX
  LDA high_byte
  ADC #$00
  STA PPUADDR
  STX PPUADDR
  LDX #$06
  STX PPUDATA

  LDA PPUSTATUS ; RIGHT CENTER (ADD $21 to low byte and if carry is set add it to high byte)
  LDA low_byte
  CLC 
  ADC #$21
  TAX
  LDA high_byte
  ADC #$00
  STA PPUADDR
  STX PPUADDR
  LDX #$07
  STX PPUDATA

  LDA PPUSTATUS ; LEFT BOTTOM (ADD $40 to low byte and if carry is set add it to high byte)
  LDA low_byte
  CLC 
  ADC #$40
  TAX
  LDA high_byte
  ADC #$00
  STA PPUADDR
  STX PPUADDR
  LDX #$08
  STX PPUDATA

  LDA PPUSTATUS ; RIGHT BOTTOM (ADD $41 to low byte and if carry is set add it to high byte)
  LDA low_byte
  CLC 
  ADC #$41
  TAX
  LDA high_byte
  ADC #$00
  STA PPUADDR
  STX PPUADDR
  LDX #$09
  STX PPUDATA

  RTS
.endproc

.proc update_hands
  LDA pad1
  AND #BTN_SELECT
  BEQ check_A
  JSR reset_handler

  check_A:
    LDA pad1
    AND #BTN_A
    BEQ check_B

    ; Set MAX cards for players
    LDA dealer_counter_cards
    CMP #$0C
    BEQ check_B

    ; Set arguments for cards.
    LDA #$00    
    STA card_rank
    LDA #$00
    STA card_set

    LDA dealer_x
    STA card_x
    LDA dealer_y
    STA card_y

    ; Call Subroutine to draw the card
    JSR draw_card
    
    ; Set X to the next position.
    LDA dealer_x
    CLC
    ADC #32
    STA dealer_x

    ; If the horizontal line reaches its maximum, then draw on the next line.
    LDA dealer_counter_cards ;if dealer_counter_cards > 5
    CMP #$05
    BEQ  dealer_next_line
    JMP dealer_continue

    dealer_next_line:
      LDA #$36 
      STA dealer_x
      LDA #$50
      STA dealer_y

    dealer_continue: 
      ; Change card_color from 00 to 01. Change card_color from 01 to 00.
      LDA card_color
      EOR #%00000001
      STA card_color

      ; Increment dealers counter cards
      LDX dealer_counter_cards
      INX
      STX dealer_counter_cards
  check_B:
    LDA pad1
    AND #BTN_B
    BEQ done_checking

    ; Set MAX cards for players
    LDA player_counter_cards
    CMP #$0E ; 14
    BEQ done_checking

    ; Set arguments for cards.
    LDA #$0A    
    STA card_rank
    LDA #$00
    STA card_set

    ; Set X Y for player's card
    LDA player_x
    STA card_x
    LDA player_y
    STA card_y

    JSR draw_card
      
    ; Set X to the next position.
    LDA player_x
    CLC
    ADC #32
    STA player_x

    ; If the horizontal line reaches its maximum, then draw on the next line.
    LDA player_counter_cards
    CMP #$06
    BEQ  player_next_line; 
    JMP player_continue

    player_next_line:
      LDA #$16
      STA player_x
      LDA #$B0
      STA player_y

    player_continue: 
    
      ; Change card_color from 00 to 01. Change card_color from 01 to 00.
      LDA card_color
      EOR #%00000001
      STA card_color

      ; Increment player counter cards
      LDX player_counter_cards
      INX
      STX player_counter_cards
  done_checking:  

  RTS
.endproc

.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler

.segment "RODATA"
palettes:
  .byte $0f, $0f, $30, $30
  .byte $0f, $2b, $3c, $39
  .byte $0f, $0c, $07, $13
  .byte $0f, $19, $09, $29

  .byte $0f, $30, $15, $23
  .byte $0f, $0f, $30, $25
  .byte $0f, $26, $27, $28
  .byte $0f, $15, $16, $17

.segment "CHR"
.incbin "background_cards.chr"

.segment "ZEROPAGE"

  ; dealer
  offset_x: .res 1
  card_x: .res 1
  card_y: .res 1

  dealer_x: .res 1
  dealer_y: .res 1

  player_x: .res 1
  player_y: .res 1

  low_byte: .res 1
  high_byte: .res 1
  card_rank: .res 1
  card_set: .res 1

  card_color: .res 1

  counter_cards: .res 1
  dealer_counter_cards: .res 1
  player_counter_cards: .res 1
  pad1: .res 1
.exportzp card_color, pad1, player_x, player_y, dealer_x, dealer_y, counter_cards, player_counter_cards, dealer_counter_cards

