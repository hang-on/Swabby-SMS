.section "Level Data" free
  Palette:
    BackgroundPalette:
      .db $35
      .ds 15 0
    SpritePalette:
      .include "Objects/Sprite-Palette.inc"
    SpritePaletteEnd:

  ; Tilebank 2 map:
  .equ PLAYER_1_TILES_START $2000
  .equ ENEMY_1_TILES_START $2400
  .equ ENEMY_2_TILES_START $2600

  ; Make character code indexes for the meta sprite data blocks.
  .equ P1 (PLAYER_1_TILES_START-$2000)/32
  .equ E1 (ENEMY_1_TILES_START-$2000)/32
  .equ E2 (ENEMY_2_TILES_START-$2000)/32

  Make3x2MetaSprites P1, Swabby1:, P1+6, Swabby2:, P1+12, Swabby3:

.ends
