.include "Base.inc"

.bank 0 slot 0
  .include "Spritelib.inc"
  .include "Objects/Objectlib.inc"

.bank 1 slot 1
  .include "Objects/Object-Data.inc"
  .include "Levels/Level-Data.inc"

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.ramsection "Main variables" slot 3
  FrameCounter db
  PlayerHandle db
.ends
; -----------------------------------------------------------------------------
.section "Main" free
; -----------------------------------------------------------------------------

  BatchLoadTable:
    .db (BatchLoadTableEnd-BatchLoadTable-1)/6
    .dw SwabbyTiles $2000 SwabbyTilesEnd-SwabbyTiles
    .dw GargoyleTiles $2400 GargoyleTilesEnd-GargoyleTiles
  BatchLoadTableEnd:

  WaveScript1:
    .dw 10                    ; Timer (count down to next wave).
    .db 70, 4                 ; Interval between enemies in wave, number of en.
    .dw GargoyleInitString    ; Init string for the enemies.
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
