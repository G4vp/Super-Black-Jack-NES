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
  JSR update_dealer_hand

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
  ; Receives X, Y, Color (00 or 01), Card Rank (number), and Card Suit (symbol).
.proc draw_card
  LDA player_x
  STA current_x

  LDA counter_dealer
  STA dealer_index

  LDA card_rank
  CLC
  ADC #$03
  STA card_rank

  LDA card_set
  CLC
  ADC #$11
  STA card_set

  LDA color
  BEQ color_is_zero
  JMP load_sprites

  color_is_zero:
  LDA current_x
  CLC
  SBC #03
  STA current_x
  

  load_sprites:
    ; Multiply 8 * counter_dealer
    ASL dealer_index
    ASL dealer_index
    ASL dealer_index


    LDX dealer_index

    ; Number Tile
    LDA player_y
    STA $0200, X
    INX
    LDA card_rank
    STA $0200, X
    INX
    LDA color
    STA $0200, X
    INX
    LDA current_x
    STA $0200, X


    ; Bottom Tile (Y + 8):
    INX
    LDA player_y
    CLC
    ADC #$08
    STA $0200, X
    INX
    LDA card_set
    STA $0200, X
    INX
    LDA color
    STA $0200, X
    INX
    LDA current_x
    STA $0200, X

    ; add 1 to counter for dealer's cards
    LDX counter_dealer 
    INX
    STX counter_dealer

  convert_xy_coords_to_nt: 

  ; X: XXXXX***
  ; Y: YYYYY***
  ; NT address: 0010NNYY YYYXXXXX

  ; All you have to do is a little bit shifting, ANDing and ORing to get the bits where they need to be.
    LDA #%00001000 ; base value for $2000 (change lower 2 bits for other NTs)
    STA high_byte
    LDA player_y
    AND #%11111000
    ASL
    ROL high_byte
    ASL
    ROL high_byte
    STA low_byte
    LDA player_x
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

.proc update_dealer_hand
  LDA pad1
  AND #BTN_SELECT
  BEQ check_A
  JSR reset_handler
  check_A:
    LDA pad1
    AND #BTN_A
    BEQ done_checking

    ; Set arguments for cards.
    LDA #$0A    
    STA card_rank
    LDA #$00
    STA card_set

    JSR draw_card
    
    ; Set X to the next position.
    LDA player_x
    CLC
    ADC #32
    STA player_x
    
    ; Change color from 00 to 01. Change color from 01 to 00.
    LDA color
    EOR #%00000001
    STA color


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
  player_x: .res 1
  current_x: .res 1
  player_y: .res 1
  tile_x: .res 1
  tile_y: .res 1

  low_byte: .res 1
  high_byte: .res 1
  card_rank: .res 1
  card_set: .res 1

  color: .res 1

  counter_dealer: .res 1
  dealer_index: .res 1
  pad1: .res 1
.exportzp player_x, player_y, color, pad1

