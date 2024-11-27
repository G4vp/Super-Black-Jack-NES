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

  ; Generate first 3 round cards.
  JSR initial_hands

  ; Generate Dealers Cards
  JSR pc_auto_generator

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
  JSR load_bid_sprites

  ; Set Initial Cash to $20
  JSR load_cash_sprites

  JSR load_pc_score_sprites
  JSR load_player_score_sprites

  LDA #$00
  STA rank_counter

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

; Update Game State
.proc initial_hands
  LDA initial_hand_state
  CMP #$01
  BNE stop_index

  check_index:
  LDA initial_hand_index
  CMP #$03
  BCS stop_index

  first_call:
  LDA initial_hand_index
  CMP #$00
  BNE second_call

  JSR generate_player_card
  JSR change_card_info
  JMP done

  second_call:
  LDA initial_hand_index
  CMP #$01
  BNE third_call

  JSR generate_player_card
  JSR change_card_info
  JMP done

  third_call:
  LDA initial_hand_index
  CMP #$02
  BNE reset

  JSR generate_dealer_card
  JSR change_card_info
  JMP done

  reset:
    LDA #$00
    STA initial_hand_state
    STA initial_hand_index
    JMP stop_index

  done:
    LDX initial_hand_index
    INX
    STX initial_hand_index

  stop_index:
  RTS
.endproc

; Dealers Auto Card Generator
.proc pc_auto_generator

  LDA is_pc_auto
  CMP #$01
  BNE done

  LDA pc_score
  CMP #$12
  BCS stop_generator

  ; Set how long the delay for each card is going to take.
  LDA pc_auto_counter
  CMP #$20
  BNE add_to_counter

  LDA #$00
  STA pc_auto_counter

  JSR generate_dealer_card
  JSR change_card_info
      
  JSR check_dealer_win

  add_to_counter:
    LDX pc_auto_counter
    INX
    STX pc_auto_counter

  JMP done

  stop_generator:
    LDA #$00
    STA is_pc_auto
    STA pc_auto_counter
  
  done:

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
    BEQ check_B

    LDA pad1
    AND #BTN_A
    BEQ check_B

    LDA bid_first_digit
    BNE first_state
    LDA bid_second_digit
    BNE first_state
    LDA bid_third_digit
    BNE first_state
    JMP done_A

    first_state:
    LDA game_state
    CMP #$00
    BEQ A_second_state
    JMP A_check_second_state

    A_second_state: ; Spawn 1 dealer card, 2 players cards and set STATE to 01

      LDX #$01
      STX game_state

      LDX #$01
      STX initial_hand_state

      LDA player_score
      CMP #$15
      BNE done_A
      LDA #$01
      STA is_black_jack

      JMP done_A

    A_check_second_state:
    LDA game_state
    CMP #$01
    BNE A_check_third_state

    ; Set MAX cards for players
    LDA player_counter_cards
    CMP #$0E
    BEQ check_B

    JSR generate_player_card
    JSR change_card_info

    ; Check if player won
    JSR check_player_win
    JMP done_A

    A_check_third_state:
      LDA game_state
      CMP #$03
      BCC done_A
      JSR reset_game
      
    
    done_A:

  check_B:
    LDA pad1
    CMP prev_controls
    BEQ check_UP

    LDA pad1
    AND #BTN_B
    BEQ check_UP

    ; Move from State 1 to State 2
    B_check_second_state:
      LDA game_state
      CMP #$01
      BNE B_check_third_state

      LDX game_state
      INX
      STX game_state

    ; State When The Dealers Cards Are Generated
    B_check_third_state:
      LDA game_state
      CMP #$02
      BNE B_check_four_state

      ; Set MAX cards for dealer
      LDA dealer_counter_cards
      CMP #$0C ; 12
      BEQ check_UP

      ; Activate Card Generator
      LDA #$01
      STA is_pc_auto

      JMP done_B

    ; Check if round finished
    B_check_four_state:
      LDA game_state
      CMP #$03
      BCC done_B
      JSR reset_game
      
    
    done_B:

  check_UP:
    LDA pad1
    CMP prev_controls
    BEQ check_DOWN

    LDA pad1
    AND #BTN_UP
    BEQ check_DOWN

    LDA game_state
    CMP #$00
    BNE done_up

    ; JSR add_cash
    ; JSR load_cash_sprites

    JSR add_bid
    JSR load_bid_sprites

    done_up:
    JMP done_checking
  
  check_DOWN:
    LDA pad1
    CMP prev_controls
    BEQ done_checking

    LDA pad1
    AND #BTN_DOWN
    BEQ done_checking

    LDA game_state
    CMP #$00
    BNE done_down

    ; JSR sbc_cash
    ; JSR load_cash_sprites

    JSR sbc_bid
    JSR load_bid_sprites

    done_down:
    JMP done_checking
  
  done_checking:  
    LDA pad1
    STA prev_controls
  RTS
.endproc

; ========== CARD LOGIC ================
; Draw card: display a card on the screen with its sprites and background.
; Receives X, Y, card_color (00 or 01), Card Rank (number), and Card Suit (symbol).
.proc draw_card

  ; Set card color from his suit
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

  ; Move the sprite 3 pixels to the left if the color is 00.
  color_is_zero:
    LDA card_x
    CLC
    SBC #03
    STA offset_x

  load_sprites:
    ; Multiply 4 * sprite_counter
    LDA sprite_counter
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

    ; add 2 to counter
    LDX sprite_counter 
    INX
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

.proc generate_dealer_card
     ; Set arguments for cards.
    
    LDA rank_counter
    STA card_rank
    LDA suit_counter
    STA card_set

    ; Set PC score
    JSR add_pc_score

    ; Set Card Position
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
    BEQ  dealer_next_line
    JMP dealer_continue

    dealer_next_line:
      LDA #$3E
      STA dealer_x
      LDA #$40
      STA dealer_y

    dealer_continue: 
      ; Increment dealers counter cards
      LDX dealer_counter_cards
      INX
      STX dealer_counter_cards
  RTS
.endproc

.proc generate_player_card
    ; Set arguments for cards.
    LDA rank_counter    
    STA card_rank
    LDA suit_counter
    STA card_set

    ; Set Player score
    JSR add_player_score

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
      ; Increment player counter cards
      LDX player_counter_cards
      INX
      STX player_counter_cards
  RTS
.endproc

.proc change_card_info
  ; Change the card's suit and number after input
  change_card_info:
    LDX suit_counter
    INX
    STX suit_counter
    LDA suit_counter
    CMP #$04
    BEQ reg_was_4
    JMP done_checking

    ; After all suits for a number were shown, we change the rank and reset the suit counter.
    reg_was_4:
      LDA #$00
      STA suit_counter
      LDX rank_counter
      INX
      STX rank_counter
    
  done_checking:
  RTS
.endproc
; ========= BACKGROUND LOGIC ============
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

; ========= BID, CASH, and SCORES LOGIC=========
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

.proc load_cash_sprites
  LDX cash_first_digit
  LDA digits, X
  STA cash_first_sprite

  LDX cash_second_digit
  LDA digits, X
  STA cash_second_sprite

  LDX cash_third_digit
  LDA digits, X
  STA cash_third_sprite

  ; Assign 12 bytes of OAM to the cash sprites.
  LDA #$6E
  STA $020C
  LDA cash_first_sprite
  STA $020D
  LDA #$00
  STA $020E
  LDA #$D8
  STA $020F

  LDA #$6E
  STA $0210
  LDA cash_second_sprite
  STA $0211
  LDA #$00
  STA $0212
  LDA #$D0
  STA $0213

  LDA #$6E
  STA $0214
  LDA cash_third_sprite
  STA $0215
  LDA #$00
  STA $0216
  LDA #$C8
  STA $0217
  RTS
.endproc

; Set pc's score sprites by taking the pc_score as argument.
.proc load_pc_score_sprites
  ; Divide second digit by 10, N times
  LDA pc_score
  LDX #$00
  loop:
    CMP #$0A
    BCC done
    SEC
    SBC #$0A
    INX
    JMP loop
  done: 
  STA pc_first_digit
  STX pc_second_digit

  LDX pc_first_digit
  LDA digits, X
  STA pc_first_sprite

  LDX pc_second_digit
  LDA digits, X
  STA pc_second_sprite

  ; Assign first 8 bytes of OAM to the PC score sprites.
  LDA #$06
  STA $0218
  LDA pc_first_sprite
  STA $0219
  LDA #$00
  STA $021A
  LDA #$2C
  STA $021B

  LDA #$06
  STA $021C
  LDA pc_second_sprite
  STA $021D
  LDA #$00
  STA $021E
  LDA #$24
  STA $021F

  RTS
.endproc

; Set player's score sprites by taking the player_score as argument. NEEDS UPDATE
.proc load_player_score_sprites

  ; Divide second digit by 10, N times
  LDA player_score
  LDX #$00
  loop:
    CMP #$0A
    BCC done
    SEC
    SBC #$0A
    INX
    JMP loop
  done: 
  STA player_first_digit
  STX player_second_digit

  LDX player_first_digit
  LDA digits, X
  STA player_first_sprite

  LDX player_second_digit
  LDA digits, X
  STA player_second_sprite

  ; Assign first 12 bytes of OAM to the bid sprites.
  LDA #$76
  STA $0220
  LDA player_first_sprite
  STA $0221
  LDA #$00
  STA $0222
  LDA #$4C
  STA $0223

  LDA #$76
  STA $0224
  LDA player_second_sprite
  STA $0225
  LDA #$00
  STA $0226
  LDA #$44
  STA $0227

  RTS
.endproc

.proc add_bid
    ; Check if the bid reached the cash limit.
    a_is_zero:
      LDA bid_third_digit
      CMP cash_third_digit
      BEQ b_is_zero
      JMP first_digit
    b_is_zero:
      LDA bid_second_digit
      CMP cash_second_digit
      BEQ c_is_zero
      JMP first_digit
    c_is_zero:
      LDA bid_first_digit
      CMP cash_first_digit
      BEQ done
      JMP first_digit
    
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
      CMP #$0A   ; bid_second_digit >= 10
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
      CMP #$0A   ; bid_third_digit >= 10
      BEQ third_gt_10
      BCS third_gt_10
      JMP done

      third_gt_10:
        LDA #$00
        STA bid_third_digit

    done:
  RTS
.endproc

.proc sbc_bid
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
    done:
  RTS
.endproc

.proc add_cash
    ; Add cash and bid first digit
    LDA cash_first_digit
    CLC 
    ADC bid_first_digit
    CMP #$0A        ; Compare against decimal 10
    BCC no_carry_first_digit ; If result < 10, no carry
    SBC #$0A        ; Subtract 10 to adjust BCD
    STA cash_first_digit
    LDA cash_second_digit ; Add carry to the next digit
    CLC
    ADC #$01
    STA cash_second_digit
    JMP second_digit_continue

  no_carry_first_digit:
    STA cash_first_digit

  second_digit_continue:
    ; Add cash and bid second digit
    LDA cash_second_digit
    CLC
    ADC bid_second_digit
    CMP #$0A        ; Compare against decimal 10
    BCC no_carry_second_digit ; If result < 10, no carry
    SBC #$0A        ; Subtract 10 to adjust BCD
    STA cash_second_digit
    LDA cash_third_digit ; Add carry to the next digit
    CLC
    ADC #$01
    STA cash_third_digit
    JMP third_digit_continue

  no_carry_second_digit:
    STA cash_second_digit

  third_digit_continue:
    ; Add cash and bid third digit 
    LDA cash_third_digit
    CLC
    ADC bid_third_digit
    CMP #$0A        
    BCC no_carry_third_digit ; If result < 10, no carry
    SBC #$0A        
    STA cash_third_digit
    JMP done

  no_carry_third_digit:
    STA cash_third_digit
  done:
    RTS        
.endproc

.proc sbc_cash
    ; Subtract bid from cash (first digit)
    LDA cash_first_digit
    SEC                 ; Set carry for subtraction
    SBC bid_first_digit
    BPL no_borrow_first_digit ; If result >= 0, no borrow
    ADC #$0A            ; Adjust result by adding 10
    STA cash_first_digit
    LDA cash_second_digit ; Subtract borrow from next digit
    SEC
    SBC #$01
    STA cash_second_digit
    JMP second_digit_continue

  no_borrow_first_digit:
    STA cash_first_digit

  second_digit_continue:
    ; Subtract bid from cash (second digit)
    LDA cash_second_digit
    SEC
    SBC bid_second_digit
    BPL no_borrow_second_digit ; If result >= 0, no borrow
    ADC #$0A            ; Adjust result by adding 10
    STA cash_second_digit
    LDA cash_third_digit ; Subtract borrow from next digit
    SEC
    SBC #$01
    STA cash_third_digit
    JMP third_digit_continue

  no_borrow_second_digit:
    STA cash_second_digit

  third_digit_continue:
    ; Subtract bid from cash (third digit)
    LDA cash_third_digit
    SEC
    SBC bid_third_digit
    BPL no_borrow_third_digit ; If result >= 0, no borrow
    ADC #$0A            ; Adjust result by adding 10
    STA cash_third_digit
    JMP done

  no_borrow_third_digit:
    STA cash_third_digit

  done:
    RTS                 ; Return from subroutine
.endproc 

.proc add_pc_score

  LDA card_rank

  CMP #$0C 
  BNE less_than_10  ; if rank != A, then check

  LDA pc_score 
  CMP #$0B
  BCS add_A_1 ; if score >= 11, then add 1 to score
  ; else, add 11.
  LDA pc_score
  CLC
  ADC #$0B
  STA pc_score

  LDA #$01 ; Set pc_used_eleven to 01
  STA pc_used_eleven
  JMP continue

  add_A_1: ; If score >= 11, then A = 1
    LDA pc_score 
    CLC
    ADC #$01
    STA pc_score
    JMP continue
  
  less_than_10: ;  Add the cards from (2 to 10)
    LDA card_rank
    CMP #$09
    BCS more_than_10

    LDA pc_score
    CLC
    ADC #$02
    STA pc_score
    
    LDA pc_score
    CLC
    ADC card_rank
    STA pc_score
    JMP continue

  more_than_10: ; Call when card suit is J,Q,K and adds 10 to pc score.
    LDA pc_score
    CLC
    ADC #$0A
    STA pc_score

  continue: 

  LDA pc_score ; Check if Score > 21, and then if a 11 was used, subtracts 11 and add 1 to score.
  CMP #$16 
  BCC continue_to_load
  LDA pc_used_eleven
  CMP #$01
  BNE continue_to_load
  LDA pc_score
  SEC
  SBC #$0B ; PC score - 11
  CLC
  ADC #$01 ; PC Score + 1
  STA pc_score
  LDA #$00
  STA pc_used_eleven

  continue_to_load:
  JSR load_pc_score_sprites

  RTS
.endproc

.proc add_player_score

  LDA card_rank

  CMP #$0C 
  BNE less_than_10  ; if rank != A, then check

  LDA player_score
  CMP #$0B
  BCS add_A_1 ; if score >= 11, then add 1 to score
  ; else, add 11.
  LDA player_score
  CLC
  ADC #$0B
  STA player_score

  LDA #$01 ; Set player_used_eleven to 01
  STA player_used_eleven
  JMP continue

  add_A_1: ; If score >= 11, then A = 1
    LDA player_score
    CLC
    ADC #$01
    STA player_score
    JMP continue
  
  less_than_10: ;  Add the cards from (2 to 10)
    LDA card_rank
    CMP #$09
    BCS more_than_10

    LDA player_score
    CLC
    ADC #$02
    STA player_score
    
    LDA player_score
    CLC
    ADC card_rank
    STA player_score
    JMP continue

  more_than_10: ; Call when card suit is J,Q,K and adds 10 to pc score.
    LDA player_score
    CLC
    ADC #$0A
    STA player_score

  continue: 

  LDA player_score ; Check if Score > 21, and then if a 11 was used, subtracts 11 and add 1 to score.
  CMP #$16 
  BCC continue_to_load
  LDA player_used_eleven
  CMP #$01
  BNE continue_to_load
  LDA player_score
  SEC
  SBC #$0B ; PC score - 11
  CLC
  ADC #$01 ; PC Score + 1
  STA player_score
  LDA #$00
  STA player_used_eleven

  continue_to_load:
  JSR load_player_score_sprites

  RTS
.endproc

; ========== CHECK FOR WIN OR LOSE ================
.proc check_player_win
  LDA player_score
  CMP #$15
  BEQ done_checking
  BCS if_player_lost
  JMP done_checking

  if_player_lost:
    LDA #$04
    STA game_state
    JSR player_lost
    JMP done_checking
  
  done_checking:

  RTS
.endproc

.proc check_dealer_win
  LDA pc_score
  CMP #$15
  BEQ continue_checking
  BCS if_player_won

  continue_checking:

  LDA pc_score
  CMP #$11
  BCC done_checking

  LDA pc_score
  CMP player_score
  BEQ if_is_draw
  BCS if_player_lost
  JMP if_player_won

  if_player_won:
    JSR player_won
    JMP done_checking

  if_player_lost:
    JSR player_lost
    JMP done_checking
  
  if_is_draw:
    JSR player_tie

  
  done_checking:

  RTS
.endproc

.proc player_won
  LDA #$03
  STA game_state

  JSR add_cash

  LDA is_black_jack ; If a black jack happened, player wins 2x bid
  CMP #$01
  BNE continue
  JSR add_cash

  continue:
  JSR load_cash_sprites

  ; Reset BID
  LDA #$00
  STA bid_first_digit
  STA bid_second_digit
  STA bid_third_digit
  JSR load_bid_sprites

  RTS
.endproc

.proc player_lost
  LDA #$04
  STA game_state

  JSR sbc_cash
  JSR load_cash_sprites

  ; Reset BID
  LDA #$00
  STA bid_first_digit
  STA bid_second_digit
  STA bid_third_digit
  JSR load_bid_sprites
  
  RTS
.endproc

.proc player_tie
  LDA #$05
  STA game_state

  ; Reset BID
  LDA #$00
  STA bid_first_digit
  STA bid_second_digit
  STA bid_third_digit
  JSR load_bid_sprites

  RTS
.endproc

.proc reset_game

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
    STA $0218,X ; set sprite y-positions off the screen
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

  ; Reset BID
  LDA #$00
  STA bid_first_digit
  STA bid_second_digit
  STA bid_third_digit

  ; Reset Scores
  LDA #$00
  STA player_score
  STA pc_score

  ; Reset States
  LDA #$00
  STA game_state

  STA player_used_eleven
  STA pc_used_eleven

  STA initial_hand_state
  STA initial_hand_index

  STA is_pc_auto
  STA pc_auto_counter

  STA is_black_jack

  vblankwait2:
    BIT PPUSTATUS
    BPL vblankwait2


    LDA #%01
    STA card_color

    JMP main
.endproc 

.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler

.segment "CHR"
.incbin "task4_background_cards.chr"

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

  bid_high: .res 1
  bid_low: .res 1

  ; Cash Number
  cash_first_digit: .res 1
  cash_second_digit: .res 1
  cash_third_digit: .res 1

  cash_first_sprite: .res 1
  cash_second_sprite: .res 1
  cash_third_sprite: .res 1

  ; PC Number
  pc_score: .res 1
  pc_first_digit: .res 1
  pc_second_digit: .res 1

  pc_first_sprite: .res 1
  pc_second_sprite: .res 1

  ; Player Number
  player_score: .res 1
  player_first_digit: .res 1
  player_second_digit: .res 1

  player_first_sprite: .res 1
  player_second_sprite: .res 1

  ; Temp for Arithmethic Operations
  A_temp: .res 1
  B_temp: .res 1

  ; Temp to keep track of when the A with 11 was used
  pc_used_eleven: .res 1
  player_used_eleven: .res 1

  ; Game State (00 Select Bid, 01 Player's turn, 02 Dealer's turn)
  game_state: .res 1

  initial_hand_state: .res 1
  initial_hand_index: .res 1

  ; Dealers Auto Generator
  is_pc_auto: .res 1
  pc_auto_counter: .res 1

  ; check if black jack happened
  is_black_jack: .res 1

.exportzp card_color, pad1, player_x, player_y, dealer_x, dealer_y, sprite_counter, player_counter_cards, dealer_counter_cards, rank_counter, suit_counter,cash_first_digit, cash_second_digit

.segment "RODATA"
  cards_test: 
    .byte $0C, $0A, $01
  digits:
    .byte $1F, $16, $17, $18, $19, $1A, $1B, $1C, $1D, $1E
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