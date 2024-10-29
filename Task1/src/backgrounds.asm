.include "constants.inc"
.include "header.inc"


.segment "CODE"
.proc irq_handler
  RTI
.endproc

.proc nmi_handler
  LDA #$00
  STA OAMADDR
  LDA #$02
  STA OAMDMA
	LDA #$00
	STA $2005
	STA $2005
  RTI
.endproc

.import reset_handler

.export main
.proc main
  ; write a palette
  LDX PPUSTATUS
  LDX #$3f
  STX PPUADDR
  LDX #$00
  STX PPUADDR
  LDA #$29
  STA PPUDATA
  LDA #$19
  STA PPUDATA
  LDA #$09
  STA PPUDATA
  LDA #$0f
  STA PPUDATA


load_palettes:
  LDA palettes,X
  STA PPUDATA
  INX
  CPX #$20
  BNE load_palettes

load_background:
  LDA PPUSTATUS
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR

;   LDX #$00
;   LDY #$20

Loop: 
  LDA background, X   ; as x starts at zero, it starts loading in a the first element in the palettes code section ($0f). This address mode allows us to copy elements from a tag with .data directives and the index in x
  STA PPUDATA       ;THE PPU-RAM POINTER GETS INCREASED AUTOMATICALLY WHENEVER WE WRITE ON IT
  INX
  CPX #$C0
  BNE Loop

  INY
  CPY #$24
  BNE Loop

;   LDA #$23            
;   STA PPUADDR
;   LDA #$D8            ;location
;   STA PPUADDR
;   LDX #$05

  ; write sprite data
  LDX #$00
; load_sprites: ; 
;   LDA sprites,X
;   STA $0200,X
;   INX
;   CPX #$C0
;   BNE load_sprites

	; write nametables
	;cards
	; LDA PPUSTATUS
	; LDA #$04
	; STA PPUADDR
	; LDA #$05
	; STA PPUADDR

	; LDX #$21     ; First tile of the card base 
	; STX PPUDATA            
  	; LDX #$22     ; Second tile
  	; STX PPUDATA 

	; LDX #$31     ; First tile of the card base 
	; STX PPUDATA            
  	; LDX #$32     ; Second tile
  	; STX PPUDATA 

	; LDX #$41     ; First tile of the card base 
	; STX PPUDATA            
  	; LDX #$42     ; Second tile
  	; STX PPUDATA          

	
	; LDA PPUSTATUS
	; LDA #$21
	; STA PPUADDR
	; LDA #$C7
	; STA PPUADDR
	; LDX #$05
	; STX PPUDATA

	; LDA PPUSTATUS
	; LDA #$22
	; STA PPUADDR
	; LDA #$E6
	; STA PPUADDR
	; LDX #$06
	; STX PPUDATA

	; LDA PPUSTATUS
	; LDA #$22
	; STA PPUADDR
	; LDA #$E7
	; STA PPUADDR
	; LDX #$07
	; STX PPUDATA

	; big stars first
	; LDA PPUSTATUS
	; LDA #$20
	; STA PPUADDR
	; LDA #$6b
	; STA PPUADDR
	; LDX #$2f
	; STX PPUDATA

	; LDA PPUSTATUS
	; LDA #$21
	; STA PPUADDR
	; LDA #$57
	; STA PPUADDR
	; STX PPUDATA

	; LDA PPUSTATUS
	; LDA #$22
	; STA PPUADDR
	; LDA #$23
	; STA PPUADDR
	; STX PPUDATA

	; LDA PPUSTATUS
	; LDA #$23
	; STA PPUADDR
	; LDA #$52
	; STA PPUADDR
	; STX PPUDATA

	; next, small star 1
	; LDA PPUSTATUS
	; LDA #$20
	; STA PPUADDR
	; LDA #$74
	; STA PPUADDR
	; LDX #$2d
	; STX PPUDATA

	; LDA PPUSTATUS
	; LDA #$21
	; STA PPUADDR
	; LDA #$43
	; STA PPUADDR
	; STX PPUDATA

	; LDA PPUSTATUS
	; LDA #$21
	; STA PPUADDR
	; LDA #$5d
	; STA PPUADDR
	; STX PPUDATA

	; LDA PPUSTATUS
	; LDA #$21
	; STA PPUADDR
	; LDA #$73
	; STA PPUADDR
	; STX PPUDATA

	; LDA PPUSTATUS
	; LDA #$22
	; STA PPUADDR
	; LDA #$2f
	; STA PPUADDR
	; STX PPUDATA

	; LDA PPUSTATUS
	; LDA #$22
	; STA PPUADDR
	; LDA #$f7
	; STA PPUADDR
	; STX PPUDATA

	; finally, small star 2
	; LDA PPUSTATUS
	; LDA #$20
	; STA PPUADDR
	; LDA #$f1
	; STA PPUADDR
	; LDX #$2e
	; STX PPUDATA

	; LDA PPUSTATUS
	; LDA #$21
	; STA PPUADDR
	; LDA #$a8
	; STA PPUADDR
	; STX PPUDATA

	; LDA PPUSTATUS
	; LDA #$22
	; STA PPUADDR
	; LDA #$7a
	; STA PPUADDR
	; STX PPUDATA

	; LDA PPUSTATUS
	; LDA #$23
	; STA PPUADDR
	; LDA #$44
	; STA PPUADDR
	; STX PPUDATA

	; LDA PPUSTATUS
	; LDA #$23
	; STA PPUADDR
	; LDA #$7c
	; STA PPUADDR
	; STX PPUDATA

	; finally, attribute table
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

.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler

.segment "RODATA"
palettes: ;check if this is right
.byte $19, $0f, $21, $32
.byte $19, $0f, $21, $32
.byte $19, $0f, $21, $32
.byte $19, $0f, $21, $32

.byte $19, $0f, $21, $32
.byte $19, $0f, $21, $32
.byte $19, $0f, $21, $32
.byte $19, $0f, $21, $32

sprites: 
.byte $70, $21, $00, $80
.byte $70, $22, $00, $88
.byte $78, $31, $00, $80
.byte $78, $32, $00, $88
.byte $80, $41, $00, $80
.byte $80, $42, $00, $88

; background:
; .byte $04,$05,$06,$07,$08,$09

background:
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$23,$16,$2e,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
	.byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$00,$00
	.byte $00,$00,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$00,$0a,$0b,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$00,$0c,$0d,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$34,$0e,$0f,$00,$00,$00,$31,$32,$00,$00,$32,$00,$00
	.byte $32,$00,$00,$32,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$44,$45,$00,$00,$00,$00,$41,$00,$00,$00,$00,$00,$00
	.byte $42,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$00,$00,$00,$31,$32,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11
	.byte $11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$11,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$16,$14,$26,$1b,$2e,$2f,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$23,$1f,$14,$2c,$18,$25,$2e,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$15,$1c,$17,$2e,$2f,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
	.byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$00,$00
	.byte $00,$00,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$34,$35,$00,$31,$32,$00,$31,$32,$00,$31,$32,$00,$31
	.byte $32,$00,$31,$32,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$44,$45,$00,$41,$42,$00,$41,$42,$00,$41,$42,$00,$00
	.byte $42,$00,$00,$00,$00,$00,$42,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
	.byte $00,$00,$12,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$13,$00,$00
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
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
	.byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
	.byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
	.byte $55,$55,$55,$55,$55,$55,$55,$55,$05,$05,$05,$05,$05,$05,$05,$05




.segment "CHR"
.incbin "background_cards1.chr" ;cambiar con todo los sprites correctos
