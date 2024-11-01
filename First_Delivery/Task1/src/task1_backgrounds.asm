.include "constants.inc"
.include "header.inc"


.segment "CODE"
.proc irq_handler
  RTI
.endproc

.proc nmi_handler

  PHA
  TXA
  PHA
  TYA
  PHA

  LDA #$00
  STA OAMADDR
  LDA #$02
  STA OAMDMA
	LDA #$00
	STA $2005
	STA $2005


  LDA #$00
  STA PPUSCROLL
  STA PPUSCROLL

  PLA
  TAY
  PLA
  TAX
  PLA

  RTI
.endproc

.import reset_handler

.export main
.proc main
;   ; write a palette
  LDX PPUSTATUS
  LDX #$3f
  STX PPUADDR
  LDX #$00
  STX PPUADDR
;   LDA #$29
;   STA PPUDATA
;   LDA #$19
;   STA PPUDATA
;   LDA #$09
;   STA PPUDATA
;   LDA #$0f
;   STA PPUDATA

LDX #$00

load_palettes:
  LDA palettes,X
  STA PPUDATA
  INX
  CPX #$20
  BNE load_palettes



LDX #$00
JSR load_background_graphics ;;CALLING THE SUBROUTINE


LDX #$00
STX loop_counter
load_card:
  
  LDA POS_X , X
  STA player_x
  LDA POS_Y , X
  STA player_y

  LDA SPRITE_COLOR, X
  STA color
  LDA SPRITES , X	
  STA card_rank
  LDA SPRITE_SUIT ,X
  STA card_set

  JSR draw_card

  LDX loop_counter
  INX
  STX loop_counter

  CPX #$13 
  BNE load_card


	;finally, attribute table
	LDA PPUSTATUS
	LDA #$23
	STA PPUADDR
	LDA #$c9
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

;Subroutine for background graphics 
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



.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler

.segment "RODATA"
palettes: ;
.byte $19, $0f, $21, $32 ; b
.byte $19, $0f, $21, $32 ; b
.byte $19, $0f, $21, $32  ;b
.byte $19, $0f, $21, $32  ;b

.byte $19, $32, $16, $38 ;s n
.byte $19, $0f, $32, $38
.byte $19, $0C, $16, $38 ;s
.byte $19, $0C, $16, $38 ;s


POS_X:
.byte $3E, $56, $6E, $86, $9E, $B6, $CE, $3E
.byte $56, $26, $3E, $56, $6E, $86, $9E, $B6 
.byte $CE, $3E, $56

POS_Y:
.byte $20, $20, $20, $20, $20, $20, $20, $40 
.byte $40, $90, $90, $90,$90, $90, $90, $90 
.byte $90, $B0, $B0

SPRITES:
.byte $02, $03, $04, $05, $06, $07, $08, $09
.byte $0A, $0B, $0C, $0D, $0E, $02, $03, $04
.byte $05, $06, $07, $08, $09, $0A, $0B, $0C
.byte $0D, $0E, $02, $03, $04, $05, $03, $04
.byte $0A, $0B, $0C, $0D, $0E, $02

SPRITE_SUIT:

.byte $01, $02, $03, $04, $02, $03, $01, $02
.byte $03, $01, $02, $04, $02, $03, $04, $04
.byte $04, $03, $03, $01, $01, $02, $03, $02
.byte $03, $01, $04, $04, $02, $03, $04, $01
.byte $01, $02, $03, $04, $01, $04


SPRITE_COLOR:
.byte $00, $00, $00, $00, $01, $01, $00, $00 
.byte $01, $01, $01, $01, $01, $01, $01, $01
.byte $00, $00, $01, $01, $00, $00, $00, $00
.byte $00, $00, $00, $00, $01, $01, $01, $01
.byte $01, $01, $01, $01, $00, $00 


; SPRITE_CASH: 
; ;CASH

; .byte $6E, $16, $04, $C8 ;black
; .byte $6E, $1A, $04, $D0
; .byte $6E, $1C, $04, $D8 ;black

; .byte $77, $1D, $04, $C8 ;black
; .byte $77, $1F, $04, $D0
; .byte $77, $1F, $04, $D8 ;black

; .byte $77, $16, $04, $48 ;black
; .byte $77, $1E, $04, $50

; .byte $08, $17, $04, $28 ;black
; .byte $08, $1D, $04, $30






background:
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$23,$16,$2e,$32,$38,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$2a,$1c,$21,$30,$00,$00
	.byte $00,$00,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
	.byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$00,$00
	.byte $00,$00,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$00,$0a,$0b,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$00,$0c,$0d,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$00,$0e,$0f,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$44,$45,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $42,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
background1:
	.byte $00,$00,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $f2,$00,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11
	.byte $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$16,$14,$26,$1b,$2e,$2f,$38,$3a,$3a,$00,$00,$00,$00
	.byte $00,$00,$23,$1f,$14,$2c,$18,$25,$2e,$31,$38,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$15,$1c,$17,$2e,$2f,$31,$35,$3a,$00,$00,$00,$00
background2:
	.byte $00,$00,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
	.byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$00,$00
	.byte $00,$00,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$44,$00,$00,$41,$42,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $42,$00,$00,$00,$00,$00,$42,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00

background3:
	.byte $00,$00,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$00,$00,$00,$00,$42,$00,$41,$42,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11
	.byte $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $25,$14,$24,$28,$18,$1f,$00,$2c,$00,$1a,$14,$15,$25,$1c,$18,$1f
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
	.byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
	.byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
	.byte $55,$55,$55,$55,$55,$55,$55,$55,$00,$00,$00,$00,$00,$00,$00,$00

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
  loop_counter: .res 1

.segment "CHR"
.incbin "task1_background_cards3.chr" ;cambiar con todo los sprites correctos
