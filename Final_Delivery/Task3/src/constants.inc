PPUCTRL   = $2000
PPUMASK   = $2001
PPUSTATUS = $2002
PPUSCROLL = $2005
PPUADDR   = $2006
PPUDATA   = $2007
OAMADDR   = $2003
OAMDMA    = $4014
CONTROLLER1 = $4016
CONTROLLER2 = $4017

BTN_RIGHT   = %00000001
BTN_LEFT    = %00000010
BTN_DOWN    = %00000100
BTN_UP      = %00001000
BTN_START   = %00010000
BTN_SELECT  = %00100000
BTN_B       = %01000000
BTN_A       = %10000000

; VRAM Addresses
NAMETABLE_A = $2000
NAMETABLE_B = $2400
NAMETABLE_C = $2800
NAMETABLE_D = $2c00
ATTR_A      = $23c0
ATTR_B      = $27c0
ATTR_C      = $2bc0
ATTR_D      = $2fc0
PALETTE     = $3f00

.macro EnableRendering
  lda #%00001110
  sta PPU_MASK
.endmacro

.macro DisableRendering
  lda #0
  sta PPU_MASK
.endmacro

.macro EnableNMI
  lda #%10000000
  sta PPU_CTRL
.endmacro

.macro DisableNMI
  lda #0
  sta PPU_CTRL
.endmacro

.macro Vram address
  bit PPU_STATUS
  lda #.HIBYTE(address)
  sta PPU_ADDR
  lda #.LOBYTE(address)
  sta PPU_ADDR
.endmacro

.macro VramColRow col, row, nametable
  Vram (nametable + row*$20 + col)
.endmacro

.macro VramReset
  bit PPU_STATUS
  lda #0
  sta PPU_ADDR
  sta PPU_ADDR
.endmacro

.macro VramPalette
  lda #$3f
  sta $2006
  lda #$00
  sta $2006
.endmacro

.macro OAMReset
  lda #0
  sta OAM_ADDR
.endmacro

.macro LoadPalettes address
  Vram PALETTE
  ldx #0
: lda address, x
  sta PPU_DATA
  inx
  cpx #$20
  bne :-
.endmacro

.macro Sprite0ClearWait
: bit PPU_STATUS
	bvs :-
.endmacro

.macro Sprite0HitWait
: bit PPU_STATUS
	bvc :-
.endmacro

