.include "Base.inc"
.include "Spritelib.inc"
.include "Objectlib.inc"

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.ramsection "Main variables" slot 3
  FrameCounter db
  PlayerHandle db
  GargoyleHandle db
  Zombie1Handle db
  NextEventIndex db
.ends
; -----------------------------------------------------------------------------
.section "Main" free
; -----------------------------------------------------------------------------
  Palette:
    BackgroundPalette:
      .db $35
      .ds 15 0
    SpritePalette:
      .include "Data/Sprite-Palette.inc"
    SpritePaletteEnd:

  ; Tilebank 2 map:
  .equ PLAYER_1_TILES_START $2000
  .equ ENEMY_1_TILES_START $2400
  .equ ENEMY_2_TILES_START $2600

  ; Make character code indexes for the meta sprite data blocks.
  .equ P1 (PLAYER_1_TILES_START-$2000)/32
  .equ E1 (ENEMY_1_TILES_START-$2000)/32
  .equ E2 (ENEMY_2_TILES_START-$2000)/32

  SwabbyTiles:
    .include "Data/Swabby-Tiles.inc"
  SwabbyTilesEnd:
  GargoyleTiles:
    .include "Data/Gargoyle-Tiles.inc"
  GargoyleTilesEnd:
  ZombieTiles:
    .include "Data/Zombie-Tiles.inc"
  ZombieTilesEnd:

  BatchLoadTable:
    .db (BatchLoadTableEnd-BatchLoadTable-1)/6
    .dw SwabbyTiles PLAYER_1_TILES_START SwabbyTilesEnd-SwabbyTiles
    .dw GargoyleTiles ENEMY_1_TILES_START GargoyleTilesEnd-GargoyleTiles
    .dw ZombieTiles ENEMY_2_TILES_START ZombieTilesEnd-ZombieTiles
  BatchLoadTableEnd:

  .macro Make3x2MetaSprites
    .rept nargs/2
      \2
        .db 6
        .db -4, -4, -4, 4, 4, 4
        .db -8, \1, 0, \1+1, 8, \1+2, -8, \1+3, 0, \1+4, 8, \1+5
        .shift
        .shift
    .endr
  .endm

  .macro Make2x2MetaSprites
    .rept nargs/2
      \2
        .db 4
        .db -4, -4, 4, 4
        .db -4, \1, 4, \1+1, -4, \1+2, 4, \1+3
        .shift
        .shift
    .endr
  .endm

  .macro Make2x3MetaSprites
    .rept nargs/2
      \2
        .db 6
        .db -8, -8, 0, 0, 8, 8
        .db -4, \1, 4, \1+1, -4, \1+2, 4, \1+3, -4, \1+4, 4, \1+5
        .shift
        .shift
    .endr
  .endm

  Make3x2MetaSprites P1, Swabby1:, P1+6, Swabby2:, P1+12, Swabby3:
  Make2x2MetaSprites E1, Gargoyle1:, E1+4, Gargoyle2:
  Make2x3MetaSprites E2, Zombie1:, E2+6, Zombie2:

  ; Animation table: An array of metasprites with corresponding timer values.
  SwabbyFlying:
    .dw $2000 Swabby1 $2000 Swabby3 $2000 Swabby1 $2000 Swabby3
    .dw $2000 Swabby2 $20ff Swabby3 ; $ff (or non-zero value means loop).

  ; Movement patterns.
  Pattern1: ; The chineese bird-men.
    .db 0, -1, 70, 1, -1, 90, 0, 1, 175, SELF_DESTRUCT

  Pattern2: ; Classic sine-wave.
    .db 1, -1, 32, -1, -1, 64, 1, -1, 32, SELF_DESTRUCT

  Pattern3: ; The chineese bird-men, with delay change.
    .db 0, -1, 70
    .db SET_DELAY, 2
    .db 1, -1, 90,
    .db SET_DELAY, 1
    .db 0, 1, 175
    .db SELF_DESTRUCT

  Pattern4: ; Neo sine-wave.
    .db 2, -1, 16, -2, -1, 32, 2, -1, 16
    .db 2, -1, 16, -2, -1, 32, 2, -1, 16
    .db 2, -1, 16, -2, -1, 32, 2, -1, 16
    .db 2, -1, 16, -2, -1, 32, 2, -1, 16
    .db 2, -1, 24
    .db SELF_DESTRUCT

  Pattern5: ; Slope.
    .db SET_DELAY 2
    .rept 40
      .db 0, -1, 5,
      .db 1, -1, 1,
    .endr
    .db -1, 1, 20
    .db SET_DELAY 3
    .db -2, 1,20
    .db SET_DELAY 0
    .db -1,1,140
    .db SELF_DESTRUCT

  Pattern6: ; Go straight left...
    .db SET_DELAY 4
    .db 0,-1,250
    .db 0,-1,50
    .db SELF_DESTRUCT

  SwabbyInitString:
    .db 1                     ; Initial status.
    .db 20 100                ; Start Y and start X.
    .dw Swabby1               ; MetaSpritePointer.
    .db JOYSTICK_1 2 2        ; Movement type, vertical and horizontal speed.
    .dw SwabbyFlying          ; Animation table base address.
    .db 0 0                   ; Animation table index and timer.
    .dw $0000                 ; Movement pattern table.
    .db 0                     ; Pattern timer.
    .db 0,0                   ; Delay counter and delay value.
    .db NO_MASKS              ; MetaSprite flags.
    .db 0                     ; Self destruct.

  GargoyleFlying:
    .dw $0700 Gargoyle1 $07ff Gargoyle2

  GargoyleInitString:
    .db 1
    .db 94 16
    .dw Gargoyle1
    .db PATTERN 1 1
    .dw GargoyleFlying
    .db 0 0
    .dw Pattern4
    .db 0
    .db 3,3
    .db LEFT_MASK             ; Hard enable left mask.
    .db 0

  ZombieHeadBanging:
    .dw $2f00 Zombie1 $2fff Zombie2

  ZombieInitString:
    .db 1
    .db 160 16
    .dw Zombie1
    .db PATTERN, 0, -1
    .dw ZombieHeadBanging
    .db 0,0
    .dw Pattern6
    .db 0
    .db 1,1
    .db LEFT_MASK
    .db 0

  WaveScript1:
    .dw 10                    ; Timer (count down to next wave).
    .db 70, 4                 ; Interval between enemies in wave, number of en.
    .dw GargoyleInitString    ; Init string for the enemies.
    .dw 600
    .db 0, 1
    .dw ZombieInitString
    ; ----
    .dw $ffff                 ; End of wavescript marker.


  SetupMain:
    ld a,0
    ld b,32
    ld hl,Palette
    call LoadCRam

    ld hl,BatchLoadTable
    call _BatchLoadTiles

    ld a,FULL_SCROLL_BLANK_LEFT_COLUMN_KEEP_SPRITES_NO_RASTER_INT
    ld b,0
    call SetRegister

    ; Put Swabby on the screen.
    ld hl,SwabbyInitString
    call CreateObject
    ld (PlayerHandle),a

    ld hl,WaveScript1
    call InitializeWaveScript

    ld a,ENABLE_DISPLAY_ENABLE_FRAME_INTERRUPTS_NORMAL_SPRITES
    ld b,1
    call SetRegister
    ei
    call AwaitFrameInterrupt

    ; Fall through to main loop...?

  Main:
    call AwaitFrameInterrupt
    call LoadSAT

    call GetInputPorts

    call ProcessWaveScript
    call RunWaveMaker
    call ObjectFrame

  jp Main

  _BatchLoadTiles:
    ; Entry: HL = Base address of BatchLoadTable to use
    ld b,(hl)             ; Read number of entries in the BatchLoadTable.
    inc hl
    push hl
    pop ix
    -:
      push bc
        ld l,(ix+0)       ; Get pointer to tile data.
        ld h,(ix+1)
        ld e,(ix+2)       ; Get pointer to destination in vram.
        ld d,(ix+3)
        ld c,(ix+4)       ; Get number of bytes to load.
        ld b,(ix+5)
        call LoadVRam
        ld de,6
        add ix,de         ; Advance the BatchLoadTable index to next entry.
      pop bc
    djnz -                ; Process all elements in BatchLoadTable
  ret
.ends

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.ramsection "WaveMaker variables" slot 3
  WAMAKCue db             ; How many enemies left in current wave?
  WAMAKInterval db        ; Frames between enemies.
  WAMAKTimer db           ; Counting frames from WAMAKInterval to zero.
  WAMAKPointer dw         ; When WAMAKTimer depletes, then make this object.
.ends
; -----------------------------------------------------------------------------
.section "Wavemaker (WAMAK)" free
; -----------------------------------------------------------------------------
  RunWaveMaker:
    ld a,(WAMAKCue)
    or a
    ret z                 ; Exit if no more enemies are waiting in the cue.

    ld a,(WAMAKTimer)
    dec a
    ld (WAMAKTimer),a
    or a
    call z,_GenerateEnemy
  ret

  _GenerateEnemy:
    ld hl,(WAMAKPointer)
    call CreateObject
    ld a,(WAMAKInterval)
    ld (WAMAKTimer),a
    ld hl,WAMAKCue
    dec (hl)
  ret

  LoadWaveMaker:
    ; Procedure: Load the WAMAK variables.
    ; Entry: A = Interval between enemies in this wave.
    ;        B = Number of enemies in this wave.
    ;        HL = Pointer to enemy object init string.
    ; Cannot use DE!
    ld (WAMAKInterval),a
    ld a,b
    ld (WAMAKCue),a
    ld (WAMAKPointer),hl
  ret
.ends


; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.ramsection "WaveScript variables" slot 3
  WaveScriptStatus db
  WaveScriptTimer dw
  WaveScriptPointer dw
.ends
; -----------------------------------------------------------------------------
.section "Wavescript" free
; -----------------------------------------------------------------------------
  InitializeWaveScript:
    ; Load the WaveScriptTimer and WaveScriptPointer variables with values
    ; from a wavescript pointed to by HL. Set WaveScriptStatus to non-zero to
    ; activate it.
    ; Entry: HL = Pointer to wave script
    ld a,(hl)
    inc hl
    ld d,(hl)
    ld e,a
    inc hl
    ld (WaveScriptTimer),de
    ld (WaveScriptPointer),hl
    ld a,1
    ld (WaveScriptStatus),a       ; Activate the wavescript handler.
  ret

  ProcessWaveScript:
    ; Frame-by-frame processing of the wavescript.
    ld a,(WaveScriptStatus)       ; If it is not active, then bye bye...
    or a
    ret z

    ; Decrement timer (word-sized). If timer is depleted then load data from
    ; the currently active wavescript and use it to call LoadWaveMaker. If
    ; $ffff (end-of-wavescript) is encountered, then set WaveScriptStatus to
    ; zero to disable it.
    ld a,(WaveScriptTimer)
    dec a
    ld (WaveScriptTimer),a
    ret nz
    ld a,(WaveScriptTimer+1)
    or a
    jp z,_TimerDepleted
    dec a
    ld (WaveScriptTimer+1),a
  ret

  _TimerDepleted:
    ; Fetch values from wavescript and set up a call to LoadWaveMaker.
    ld hl,(WaveScriptPointer)
    ld a,(hl)
    inc hl
    ld b,(hl)

    inc hl
    ld e,(hl)
    inc hl
    ld d,(hl)
    ex de,hl
    call LoadWaveMaker
    ex de,hl

    ; Load the WaveScriptTimer with the next value, and update the
    ; WaveScriptPointer as well.
    inc hl
    ld a,(hl)
    ld (WaveScriptTimer),a
    inc hl
    ld a,(hl)
    ld (WaveScriptTimer+1),a
    inc hl
    ld (WaveScriptPointer),hl

    ; Check if end-of-wavescript was encountered.
    ld a,(WaveScriptTimer)
    cp $ff
    ret nz
    ld a,(WaveScriptTimer+1)
    cp $ff
    ret nz
    xor a
    ld (WaveScriptStatus),a
  ret
.ends
