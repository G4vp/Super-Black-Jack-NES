.include "constants.inc"
.include "header.inc"


.segment "CODE"
.proc irq_handler
  RTI
.endproc

.import read_controller1
.import reset_handler

.include "./task3_galois16.asm"

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

  INC counter

  LDA PPUSTATUS
  EOR seed
  STA seed

  LDA counter
  EOR seed+1
  STA seed+1

  ; read controller
  JSR read_controller1
  JSR update_hands

  ; LDA wait_to_draw
  ; CMP #$01
  ; BNE continue_bk_card
  ; WaitForVBlank:
  ;     LDA $2002
  ;     AND #%10000000
  ;     BEQ WaitForVBlank
  ; JSR draw_background_card
  ; LDA #$00
  ; STA wait_to_draw

  ; continue_bk_card:

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

  JSR load_background_graphics

  LDA #$1F
  STA bid_first_sprite
  STA bid_second_sprite
  STA bid_third_sprite
  JSR load_bid_sprites

  LDA #$00
  STA prev_controls
  
  load_attribute_table:
    LDA PPUSTATUS
    LDA #$23
    STA PPUADDR
    LDA #$c2
    STA PPUADDR
    LDA #%11111111
    STA PPUDATA

    LDA PPUSTATUS
    LDA #$23
    STA PPUADDR
    LDA #$e0
    STA PPUADDR
    LDA #%11111111
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

  LDA card_set
  CMP #$02
  BCC set_color_first_pallete
  LDA #$01
  STA card_color
  JMP continue
  set_color_first_pallete:
    LDA #$00
    STA card_color
  continue:

  LDA card_x
  STA offset_x

  LDA card_rank
  CLC
  ADC #$05
  STA card_rank

  LDA card_set
  CLC
  ADC #$12
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
    ; Multiply 8 * sprite_counter
    LDA sprite_counter
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
    LDX sprite_counter 
    INX
    STX sprite_counter

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


    ; LDA #$01
    ; STA wait_to_draw
    jsr draw_background_card

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
    CMP prev_controls
    BNE test    ; Branch to an intermediate label within range
    JMP check_B  ; Use JMP to reach the final label

    test:
    LDA pad1
    AND #BTN_A
    BEQ check_B

    LDA PPUSTATUS
    EOR seed
    STA seed
    INC seed
  
    ; Set MAX cards 
    LDA dealer_counter_cards
    CMP #$0A
    BEQ check_B

    jsr generate_cards

    ;Set arguments for cards.
    LDA rank_counter    
    STA card_rank
    LDA suit_counter
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
    ADC #24 ; It has to be a multiple of 8
    STA dealer_x

    ; If the horizontal line reaches its maximum, then draw on the next line.
    LDA dealer_counter_cards ;if dealer_counter_cards > 5
    CMP #$05
    BEQ dealer_next_line
    JMP dealer_continue

    dealer_next_line:
      LDA #$3E
      STA dealer_x
      LDA #$40
      STA dealer_y

    dealer_continue: 
      ; Change card_color from 00 to 01. Change card_color from 01 to 00.
      ; LDA card_color
      ; EOR #%00000001
      ; STA card_color

      ; Increment dealers counter cards
      LDX dealer_counter_cards
      INX
      STX dealer_counter_cards
    JMP change_card_info


  check_B:
    LDA pad1
    CMP prev_controls
    BNE test1    ; Branch to an intermediate label within range
    JMP check_UP  ; Use JMP to reach the final label

    test1:
    LDA pad1
    AND #BTN_B
    BEQ check_UP


    ; Set MAX cards for players
    LDA player_counter_cards
    CMP #$0A 
    BEQ check_UP
    
    jsr generate_cards

    ;Set arguments for cards.
    LDA rank_counter    
    STA card_rank
    LDA suit_counter
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
    ADC #24
    STA player_x

    ; If the horizontal line reaches its maximum, then draw on the next line.
    LDA player_counter_cards
    CMP #$06
    BEQ  player_next_line; 
    JMP player_continue


    player_next_line:
      LDA #$26
      STA player_x
      LDA #$B0
      STA player_y

    player_continue: 
    
      ; Change card_color from 00 to 01. Change card_color from 01 to 00.
      ; LDA card_color
      ; EOR #%00000001
      ; STA card_color

      ; Increment player counter cards
      LDX player_counter_cards
      INX
      STX player_counter_cards
    JMP change_card_info

  check_UP:
    LDA pad1
    CMP prev_controls
    BEQ check_DOWN

    LDA pad1
    AND #BTN_UP
    BEQ check_DOWN

    JSR add_three_digits
    JSR load_bid_sprites

    JMP done_checking
  
  check_DOWN:
    LDA pad1
    CMP prev_controls
    BEQ done_checking

    LDA pad1
    AND #BTN_DOWN
    BEQ done_checking

    JSR sbc_three_digits
    JSR load_bid_sprites

    JMP done_checking
  ; Change the card's suit and number after input
  change_card_info:
    LDX suit_counter
    INX
    STX suit_counter
    LDA suit_counter
    CMP #$04
    BEQ reg_was_4
    JMP done_checking

    reg_was_4:
      LDA #$00
      STA suit_counter
      LDX rank_counter
      INX
      STX rank_counter
  

  done_checking:  
    LDA pad1
    STA prev_controls

  RTS
.endproc

;(suit*13) +rank
.proc calculate_card_index
  ; Input: A = rank (0–12), X = suit (0–3)
  ; Output: X = unique index (0–51)
  ; Modifies: A, Y
  LDA suit_counter
  CLC
  ADC suit_counter ; 2
  CLC
  ADC suit_counter ; 3
  CLC
  ADC suit_counter ; 4
  CLC 
  ADC suit_counter ; 5
  CLC
  ADC suit_counter ; 6
  CLC 
  ADC suit_counter ; 7
  CLC
  ADC suit_counter ; 8
  CLC 
  ADC suit_counter ; 9
  CLC 
  ADC suit_counter ; 10
  CLC 
  ADC suit_counter ; 11
  CLC
  ADC suit_counter ; 12
  CLC 
  ADC suit_counter ; 13

  CLC 
  ADC rank_counter
  
  SEC
  SBC #$01

  STA index

  RTS

.endproc

.proc generate_cards
  generate_cardA:
      ; LDA PPUSTATUS
      ; EOR seed
      ; STA seed
      ; INC seed
      ; STA seed

      jsr card_52
      and #%00001111
      ;tax

      check_rangeA:
        cmp #13
        bcc rank_doneA
        sbc #1
        jmp check_rangeA

      rank_doneA:
        sta rank_counter

      ; ;random suit
      jsr card_52
      and #%00000011
      ;tax
      ; clc
      ; adc #0
      sta suit_counter

  JSR calculate_card_index
  ; jsr mark_card_used
  ldx index
  
  LDA index
  CMP #$34
  BCS generate_cardA
  
  LDA deck_cards, x
  cmp #$01
  beq generate_cardA

  LDA #$01             ; Load the "used" marker (value = 1)
  STA deck_cards, X    ; Store it in the deck_cards array at the calculated index

  rts
.endproc

.proc load_background_graphics

	load_background:
	LDA PPUSTATUS
	LDA #$20
	STA PPUADDR
	LDA #$00
	STA PPUADDR
	LDX #$00

	Loop1: 
	LDA background, X   ;starts at Zero, and loads the first element
	STA PPUDATA       
	INX					; increses x
	CPX #$FF
	BNE Loop1

	Loop2: 
	LDA background1, X   
	STA PPUDATA      
	INX
	CPX #$FF
	BNE Loop2

	Loop3: 
	LDA background2, X  
	STA PPUDATA       
	INX
	CPX #$FF
	BNE Loop3

	Loop4: 
	LDA background3, X   
	STA PPUDATA       
	INX
	CPX #$FF
	BNE Loop4

	LDX #$00

	RTS

.endproc

.proc load_bid_sprites
  LDX bid_first_digit
  LDA digits, X
  STA bid_first_sprite

  LDX bid_second_digit
  LDA digits, X
  STA bid_second_sprite

  LDX bid_third_digit
  LDA digits, X
  STA bid_third_sprite

  ; Assign first 12 bytes of OAM to the bid sprites.
  LDA #$76
  STA $0200
  LDA bid_first_sprite
  STA $0201
  LDA #$00
  STA $0202
  LDA #$D8
  STA $0203

  LDA #$76
  STA $0204
  LDA bid_second_sprite
  STA $0205
  LDA #$00
  STA $0206
  LDA #$D0
  STA $0207

  LDA #$76
  STA $0208
  LDA bid_third_sprite
  STA $0209
  LDA #$00
  STA $020A
  LDA #$C8
  STA $020B
  RTS
.endproc

.proc add_three_digits

    ; Add 5 to the leftmost digit
    first_digit:
      LDA bid_first_digit
      CLC
      ADC #$05
      STA bid_first_digit

      LDA bid_first_digit
      CMP #$0A   ; bid_first_digit >= 10
      BEQ first_gt_10
      BCS first_gt_10
      JMP done

      first_gt_10:
        LDA #$00
        STA bid_first_digit

    second_digit:
      LDX bid_second_digit
      INX
      STX bid_second_digit
      
      LDA bid_second_digit
      CMP #$0A   ; bid_first_digit >= 10
      BEQ second_gt_10
      BCS second_gt_10
      JMP done

      second_gt_10:
        LDA #$00
        STA bid_second_digit

    third_digit:
      LDX bid_third_digit
      INX
      STX bid_third_digit
      
      LDA bid_third_digit
      CMP #$0A   ; bid_first_digit >= 10
      BEQ third_gt_10
      BCS third_gt_10
      JMP done

      third_gt_10:
        LDA #$00
        STA bid_third_digit

    done:
  RTS
.endproc

.proc sbc_three_digits

    ; Check if the digits are all zero before any action
    a_is_zero:
      LDA bid_first_digit
      BEQ b_is_zero
      JMP first_digit
    b_is_zero:
      LDA bid_second_digit
      BEQ c_is_zero
      JMP first_digit
    c_is_zero:
      LDA bid_third_digit
      BEQ done
      JMP first_digit


    first_digit:
      LDA bid_first_digit
      BEQ first_is_zero

      LDA #$00
      STA bid_first_digit
      JMP done

      first_is_zero:
        LDA #$05
        STA bid_first_digit

    second_digit:
      LDX bid_second_digit
      DEX
      STX bid_second_digit

      LDA bid_second_digit
      CMP #$FF
      BEQ second_is_zero
      JMP done

      second_is_zero:
        LDA #$09
        STA bid_second_digit

    third_digit:
      LDX bid_third_digit
      DEX
      STX bid_third_digit

      ; LDA bid_third_digit
      ; CMP #$FF
      ; BEQ third_is_zero
      ; JMP done

      ; third_is_zero:
      ;   LDA #$09
      ;   STA bid_third_digit

    done:
  RTS
.endproc


.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler

.segment "CHR"
.incbin "background_check3.chr"

.segment "ZEROPAGE"

  ; Card
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
  
  ; Counters
  rank_counter: .res 1
  suit_counter: .res 1
  dealer_counter_cards: .res 1
  player_counter_cards: .res 1
  sprite_counter: .res 1

  ; Controllers
  pad1: .res 1
  prev_controls: .res 1

  ; Bid Number
  bid_first_digit: .res 1
  bid_second_digit: .res 1
  bid_third_digit: .res 1

  bid_first_sprite: .res 1
  bid_second_sprite: .res 1
  bid_third_sprite: .res 1

  ; Randomizer
  seed: .res 2
  counter: .res 1
  deck_cards: .res 52
  temp_A: .res 1
  temp_B: .res 1
  index: .res 1

  wait_to_draw: .res 1


.exportzp card_color, pad1, player_x, player_y, dealer_x, dealer_y, sprite_counter, player_counter_cards, dealer_counter_cards, rank_counter, suit_counter, seed, counter

.segment "RODATA"
digits:
  .byte $1F, $16, $17, $18, $19, $1A, $1B, $1C, $1D, $1E

suit_table:
  .byte $12, $13, $14, $15

rank_table:
  .byte $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F, $10, $11

palettes:
  .byte $19, $0f, $21, $32
  .byte $19, $0f, $21, $32
  .byte $19, $0f, $21, $32
  .byte $19, $0f, $21, $32

  .byte $19, $32, $15, $23
  .byte $19, $19, $32, $32
  .byte $19, $0f, $21, $32
  .byte $19, $0f, $21, $32

background:
	.byte $00,$00,$cb,$cb,$cb,$cb,$cb,$cb,$cb,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$cb,$23,$16,$2e,$cb,$cb,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$cb,$cb,$00,$00,$cb,$cb,$cb,$cb
	.byte $00,$cb,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
	.byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$cb,$cb
	.byte $00,$cb,$12,$cb,$cb,$cb,$cb,$cb,$cb,$cb,$cb,$cb,$cb,$cb,$cb,$cb
	.byte $cb,$cb,$cb,$cb,$cb,$cb,$cb,$cb,$cb,$cb,$cb,$cb,$cb,$13,$00,$00
	.byte $cb,$75,$12,$75,$0a,$0b,$cb,$00,$cb,$cb,$cb,$cb,$cb,$cb,$cb,$cb
	.byte $cb,$cb,$cb,$00,$cb,$00,$00,$00,$00,$00,$42,$00,$00,$13,$00,$00
	.byte $cb,$cb,$12,$00,$0c,$0d,$00,$00,$00,$00,$00,$75,$75,$75,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $75,$75,$12,$75,$0e,$0f,$75,$00,$75,$75,$75,$00,$75,$75,$75,$00
	.byte $00,$00,$00,$00,$00,$00,$cb,$cb,$00,$cb,$cb,$cb,$cb,$13,$cb,$cb
	.byte $00,$59,$12,$75,$cb,$75,$75,$75,$75,$75,$00,$00,$75,$00,$75,$75
	.byte $75,$75,$75,$cb,$cb,$cb,$cb,$cb,$cb,$cb,$cb,$00,$00,$13,$00,$a2

background1:
	.byte $59,$59,$12,$75,$cb,$75,$75,$75,$00,$75,$00,$cb,$75,$cb,$cb,$75
	.byte $cb,$cb,$cb,$cb,$cb,$cb,$cb,$cb,$00,$00,$00,$00,$00,$13,$00,$a2
	.byte $59,$59,$12,$cb,$75,$cb,$cb,$cb,$cb,$75,$cb,$cb,$cb,$cb,$cb,$cb
	.byte $cb,$cb,$cb,$cb,$cb,$cb,$cb,$cb,$00,$00,$00,$00,$00,$13,$a2,$a2
	.byte $cb,$75,$12,$cb,$75,$cb,$cb,$cb,$cb,$cb,$cb,$cb,$cb,$cb,$75,$cb
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$a2,$a2
	.byte $75,$cb,$12,$cb,$cb,$cb,$cb,$cb,$cb,$75,$75,$75,$cb,$cb,$cb,$75
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$a2,$a2
	.byte $cb,$cb,$12,$cb,$75,$cb,$cb,$00,$75,$cb,$cb,$cb,$cb,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $75,$75,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11
	.byte $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$00,$00
	.byte $cb,$cb,$ab,$ab,$ab,$ab,$cb,$cb,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$16,$14,$26,$1b,$2e,$2f,$00,$00,$00,$00,$00,$00,$00
	.byte $cb,$cb,$23,$1f,$14,$2c,$18,$25,$2e,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$15,$1c,$17,$2e,$2f,$00,$00,$00,$00,$00,$00,$00

background2:
	.byte $cb,$cb,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
	.byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$00,$00
	.byte $cb,$cb,$12,$00,$cb,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$cb,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $78,$78,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $78,$78,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $78,$78,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00

background3:
	.byte $78,$78,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $78,$78,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11
	.byte $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $25,$14,$24,$28,$18,$1f,$00,$2c,$83,$1a,$14,$15,$25,$1c,$18,$1f
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $55,$55,$51,$50,$50,$50,$51,$55,$55,$55,$55,$55,$55,$51,$54,$51
	.byte $55,$55,$55,$55,$05,$05,$00,$55,$55,$55,$15,$05,$45,$55,$15,$01
	.byte $55,$05,$05,$05,$05,$05,$05,$11,$55,$00,$00,$00,$00,$00,$00,$11
	.byte $45,$50,$50,$50,$50,$50,$50,$11,$05,$05,$05,$05,$00,$00,$00,$00