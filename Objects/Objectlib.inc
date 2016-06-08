; *****************************************************************************
;                           OBJECT LIBRARY
; *****************************************************************************
; Note: It builds on top of the Spritelib.

; Object Attribute Table definitions.
.equ OAT_ELEMENT_SIZE 64
.equ MAX_OBJECTS 16

.equ OAT_STATUS 0
.equ OAT_Y 1
.equ OAT_X 2
.equ OAT_META_SPRITE_POINTER_LSB 3
.equ OAT_MOVEMENT 5
.equ OAT_VERTICAL_SPEED 6
.equ OAT_HORIZONTAL_SPEED 7
.equ OAT_ANIM_TABLE_BASE_ADDRESS_LSB 8
.equ OAT_ANIM_TABLE_INDEX 10
.equ OAT_ANIM_TIMER 11
.equ OAT_MOVEMENT_PATTERN_POINTER_LSB 12
.equ OAT_PATTERN_TIMER 14
.equ OAT_DELAY_COUNTER 15
.equ OAT_DELAY_VALUE 16
.equ OAT_META_SPRITE_FLAGS 17
.equ OAT_DESTROY_OBJECT 18


; Definitions for object's movement type.
.equ STATIC            0  ; Ignore vertical and horizontal speeds. Stay still.
.equ JOYSTICK_1        1  ; Object is controlled with joystick in port 1.
.equ JOYSTICK_2        2  ; Object is controlled with joystick in port 2.
.equ SIMPLE_MOVEMENT   3  ; Object moves by values in v- and h-speed.
.equ PATTERN           4  ; Object moves according to a pattern.
; NOTE: Be sure to align _MovementTable to these definitions!

; Control characters in the pattern data.
.equ PATTERN_JUMP     $aa ; The following word is pointer to pattern (LSB).
.equ SELF_DESTRUCT    $ab ; Make the object self destruct at this point.
.equ SET_DELAY        $ac ; Set the delay (the following byte).





.ramsection "Objectlib Variables" slot 3
  ObjectAttributeTable dsb 16*OAT_ELEMENT_SIZE
    ; 0     : Status
    ; 1     : Y
    ; 2     : X
    ; 3-4   : MetaSpritePointer
    ; 5     : Movement
    ; 6     : Vertical speed
    ; 7     : Horizontal speed
    ; 8-9   : AnimTableBaseAddress
    ; 10    : AnimationTableIndex
    ; 11    : AnimationTimer
    ; 12-13 : Movement pattern table pointer.
    ; 14    : Pattern timer.
    ; 15    : DelayCounter - counts from DelayValue to zero.
    ; 16    : DelayValue - how many frames to skip during each movement.
    ; 17    : MetaSpriteFlags - uuuu uulr
    ;           - enable left or right mask (for horizontal clipping).
    ; 18    : Destroy object
    ; 19-63 : Unused
.ends

; -----------------------------------------------------------------------------
.section "AnimateObject" free
; -----------------------------------------------------------------------------
  ; Decrement object's animation timer. Load new meta sprite pointer (i.e. a
  ; new frame of animation) if the timer is up.
  ; Entry: IX = Base address of OAT element.
  ; Uses: AF, DE, HL, IX
  AnimateObject:
    ld a,(ix+OAT_ANIM_TIMER)
    or a                  ; Zero: Time to load a new meta sprite pointer.
    call z,_LoadMetaSpriterPointer
    dec (ix+OAT_ANIM_TIMER)
  ret

  _LoadMetaSpriterPointer:
    ; Entry: IX = Pointer to OAT element.
    ld a,(ix+OAT_ANIM_TABLE_INDEX)      ; Get animation table index.
    add a,a
    add a,a           ; Times 4.
    ld d,0
    ld e,a
    ld l,(ix+OAT_ANIM_TABLE_BASE_ADDRESS_LSB)
    ld h,(ix+OAT_ANIM_TABLE_BASE_ADDRESS_LSB+1)
    add hl,de
    ld a,(hl)         ; Read control byte.
    or a
    jp z,+           ; Zero means forward to next frame.
      ld a,-1        ; .. So it wil be incremented to zero below.
      ld (ix+OAT_ANIM_TABLE_INDEX),a
    +:
      inc hl
      ld a,(hl)       ; Read new timer value from table.
      ld (ix+OAT_ANIM_TIMER),a    ; Store it in OAT.
      inc hl
      ld a,(hl)
      ld (ix+OAT_META_SPRITE_POINTER_LSB),a
      inc hl
      ld a,(hl)
      ld (ix+OAT_META_SPRITE_POINTER_LSB+1),a
      inc (ix+OAT_ANIM_TABLE_INDEX)     ; Increment the animation table index.
  ret
.ends

; -----------------------------------------------------------------------------
.section "CreateObject" free
; -----------------------------------------------------------------------------
  ; Find an object with Status = 0 in OAT. Load data into this object from a
  ; string pointed to by HL.
  ; Entry: HL = Pointer to object initialization data.
  ; Exit: A = Table index assigned to this object.
  ; Uses: AF,B,DE,HL,IX
  CreateObject:
    ; Search for the next free object in OAT
    ld ix,ObjectAttributeTable
    ld b,0
    -:
      ld a,(ix+OAT_STATUS)
      or a                  ; Zero means object is idle.
      jp nz,+
        call _LoadElement
        or a                ; Clear carry (operation successful).
        ld a,b              ; Return with A holding the index.
        ret
      +:
      ld de,OAT_ELEMENT_SIZE
      add ix,de
      inc b
      ld a,b
      cp MAX_OBJECTS
    jp nz,-
    scf                      ; Set carry to indicate error.
  ret

  _LoadElement:
    push ix
    pop de
    .rept OAT_ELEMENT_SIZE
      ldi
    .endr
  ret
.ends

; -----------------------------------------------------------------------------
.section "DestroyObjectUsingHandle" free
; -----------------------------------------------------------------------------
  ; Detroys an object.
  ; Entry: A = Handle of object to destroy.
  ; Uses: AF,B,HL
  DestroyObjectUsingHandle:
    ld hl,ObjectAttributeTable
    cp 0
    jp z,+
    ld de,OAT_ELEMENT_SIZE
    ld b,a
    -:
      add hl,de
    djnz -
    +:
    xor a             ; Write zero to signal that this obj. is now 'idle'.
    ld (hl),a
  ret
.ends

; -----------------------------------------------------------------------------
.section "DrawObjects" free
; -----------------------------------------------------------------------------
  ; Utilize the Spritelib for drawing the active objects' metasprites.
  ; Search for the next free object in OAT (from last to first element).
  ; Uses: AF,B,DE,HL,IX
  DrawObjects:
    call BeginMetaSprites
    ld ix,ObjectAttributeTable
    .rept MAX_OBJECTS
      ld a,(ix+OAT_STATUS)
      or a                  ; Zero means object is idle.
      call nz,_DrawObject
      ld de,OAT_ELEMENT_SIZE
      add ix,de
    .endr
    call FinalizeMetaSprites
  ret

  _DrawObject:
    ; Entry: IX = Pointer to OAT element.
    ; Uses: AF,B,HL,IX
    ld l,(ix+OAT_META_SPRITE_POINTER_LSB)
    ld h,(ix+OAT_META_SPRITE_POINTER_LSB+1)
    ld a,(ix+OAT_Y)
    ld b,(ix+OAT_X)
    ld c,(ix+OAT_META_SPRITE_FLAGS)
    call AddMetaSprite
  ret
.ends


; -----------------------------------------------------------------------------
.section "HandleDestruction" free
; -----------------------------------------------------------------------------
  HandleSelfDestruct:
  ; Destroy object if OAT_DESTROY_OBJECT != 0.
  ; Entry: IX = OAT element.
  ld a,(ix+OAT_DESTROY_OBJECT)
  or a
  ret z
  xor a
  ld (ix+OAT_STATUS),a
  ret
.ends

; -----------------------------------------------------------------------------
.section "HandleMask" free
; -----------------------------------------------------------------------------
  ; The left and right masks' purpose is to let larger objects appear/disappear
  ; smoothly at the screen's horizontal borders. Screen is divided into three
  ; zones: 0-63 : 64-190 : 191-255. An object that scrolls into the screen will
  ; be initialized with either left og right mask enabled. When the object
  ; is in the central zone, masks are disabled. When object is in the left zone
  ; then the left mask is enabled, and vice versa.

  ; Masks explained: Left and right side masks are used by the function
  ; AddMetaSprite in Spritelib.inc. When the right mask is active on a
  ; metasprite, every hardware sprite that is about to be drawn somewhere
  ; within 191-255 is cancelled (actually moved into the blanked column). So
  ; the right mask is for making metasprites move smoothly out of the left
  ; side of the screen. Left mask vice versa.

  ; FIXME!!: Make code correspond to logic explained above!
  ; Check object's mask status, and toggle mask depending on object's
  ; horizontal position and mask status.
  ; Entry: IX = OAT element.
  HandleMasks:
    bit 1,(ix+OAT_META_SPRITE_FLAGS)      ; Is left mask on?
    jp z,+                                ; No? - forward to next test.
      ld a,(ix+OAT_X)                     ; Get object's horizontal position.
      cp 64                               ; Is it 63 or less?
      ret c                               ; Yes? - return and keep mask on.
      sub 191                             ; No? Is it 191 or more?
      ret nc                              ; Yes? - return and keep mask on.
      res 1,(ix+OAT_META_SPRITE_FLAGS)    ; No? - object is somewhere in the
      ret                                 ; middle of the screen, turn mask off.
    +:
    bit 0,(ix+OAT_META_SPRITE_FLAGS)           ; Is right mask on?
    jp z,+
      ld a,(ix+OAT_X)
      cp 191
      ret nc
      sub 64
      ret c
      res 0,(ix+OAT_META_SPRITE_FLAGS)        ; Turn right mask off.
      ret
    +:
    ; else fall through to 'no masks on' below...
    ld a,(ix+OAT_X)
    cp 64
    jp nc,+
      set 0,(ix+OAT_META_SPRITE_FLAGS)
      ret
    +:
    cp 191
    ret c
    set 1,(ix+OAT_META_SPRITE_FLAGS)
  ret
.ends

; -----------------------------------------------------------------------------
.section "MoveObject" free
; -----------------------------------------------------------------------------
  MoveObject:
    ; Entry: IX = Base address of active OAT element.

    ; First handle delay.
    ld a,(ix+OAT_DELAY_VALUE)            ; Get delay value (0 means no delay).
    or a
    jp z,+
      ; Okay, delay is active...
      dec (ix+OAT_DELAY_COUNTER)
      ret nz
      ld (ix+OAT_DELAY_COUNTER),a        ; A still holds the delay value.
    +:
    ld a,(ix+OAT_MOVEMENT)
    add a,a
    ld d,0
    ld e,a
    ld hl,_MovementTable
    add hl,de
    ld a,(hl)
    inc hl
    ld h,(hl)
    ld l,a
    jp (hl)

    _MovementTable:
      .dw _Static, _Joystick1Controlled, _Joystick2Controlled
      .dw _SimpleMovement _PatternMovement

    _Static:
      jp _EndSwitch

    _Joystick1Controlled:
      call IsPlayer1RightPressed
      jp nc,+
        ld a,(ix+7)
        add a,(ix+2)
        ld (ix+2),a
      +:
      call IsPlayer1LeftPressed
      jp nc,+
        ld a,(ix+2)
        sub (ix+7)
        ld (ix+2),a
      +:
      call IsPlayer1UpPressed
      jp nc,+
        ld a,(ix+1)
        sub (ix+6)
        ld (ix+1),a
      +:
      call IsPlayer1DownPressed
      jp nc,+
        ld a,(ix+1)
        add a,(ix+6)
        ld (ix+1),a
      +:
    jp _EndSwitch

    _Joystick2Controlled:
      call IsPlayer2RightPressed
      jp nc,+
        ld a,(ix+7)
        add a,(ix+2)
        ld (ix+2),a
      +:
      call IsPlayer2LeftPressed
      jp nc,+
        ld a,(ix+2)
        sub (ix+7)
        ld (ix+2),a
      +:
      call IsPlayer2UpPressed
      jp nc,+
        ld a,(ix+1)
        sub (ix+6)
        ld (ix+1),a
      +:
      call IsPlayer2DownPressed
      jp nc,+
        ld a,(ix+1)
        add a,(ix+6)
        ld (ix+1),a
      +:
    jp _EndSwitch

    _SimpleMovement:
      ; Simply apply the vertical and horizontal speed to y and x...
      ld a,(ix+1)
      add a,(ix+6)
      ld (ix+1),a
      ld a,(ix+2)
      add a,(ix+7)
      ld (ix+2),a
    jp _EndSwitch

    _PatternMovement:
      ld a,(ix+OAT_PATTERN_TIMER)
      or a
      jp nz,++
        ld l,(ix+OAT_MOVEMENT_PATTERN_POINTER_LSB)
        ld h,(ix+OAT_MOVEMENT_PATTERN_POINTER_LSB+1)
        ld a,(hl)       ; Get vertical speed from pattern table.

        ; Check for the presence of control characters, and branch accordingly.
        cp SELF_DESTRUCT
        jp nz,+
          ld (ix+OAT_DESTROY_OBJECT),a ; By loading a non-zero value here, we
          jp _EndSwitch                ; effectively destroy the object.
        +:

        cp SET_DELAY
        jp z,_SetDelay

        cp PATTERN_JUMP ; If vertical speed is $aa, it means that we should
        call z,_LoadPatternPointer ; load a new value into the pattern pointer.

        ld (ix+OAT_VERTICAL_SPEED),a
        inc hl
        ld a,(hl)       ; Get horizontal speed from pattern table.
        ld (ix+OAT_HORIZONTAL_SPEED),a
        inc hl
        ld a,(hl)       ; Get pattern timer value
        ld (ix+OAT_PATTERN_TIMER),a
        inc hl
        ld (ix+OAT_MOVEMENT_PATTERN_POINTER_LSB),l
        ld (ix+OAT_MOVEMENT_PATTERN_POINTER_LSB+1),h
      ++:
      dec (ix+OAT_PATTERN_TIMER)

      ; Simply apply the vertical and horizontal speed to y and x...
      ld a,(ix+OAT_Y)
      add a,(ix+OAT_VERTICAL_SPEED)
      ld (ix+OAT_Y),a
      ld a,(ix+OAT_X)
      add a,(ix+OAT_HORIZONTAL_SPEED)
      ld (ix+OAT_X),a
    jp _EndSwitch

    _EndSwitch:
  ret

  _LoadPatternPointer:
    ; Read pointer from pattern table (at HL+1 and HL+2), and store it in the
    ; pattern pointer in object's OAT element.
    ; Entry: HL = Pointer to control byte $ff in a pattern table.
    ;        IX = This object on the OAT.
    ; Exit:  A = vertical speed.
    inc hl
    ld a,(hl)
    ld (ix+OAT_MOVEMENT_PATTERN_POINTER_LSB),a
    inc hl
    ld a,(hl)
    ld (ix+OAT_MOVEMENT_PATTERN_POINTER_LSB+1),a
    ld l,(ix+OAT_MOVEMENT_PATTERN_POINTER_LSB)
    ld h,(ix+OAT_MOVEMENT_PATTERN_POINTER_LSB+1)
    ld a,(hl)       ; Leave with vertical speed in A...
  ret

  _SetDelay:
    ; Entry: HL + 1 will be the new delay counter and value
    ; Exit: Pattern pointer for this object updated to next entry.
    inc hl
    ld a,(hl)
    ld (ix+OAT_DELAY_VALUE),a
    ld (ix+OAT_DELAY_COUNTER),A
    inc hl
    ld (ix+OAT_MOVEMENT_PATTERN_POINTER_LSB),l
    ld (ix+OAT_MOVEMENT_PATTERN_POINTER_LSB+1),h
  jp _EndSwitch
.ends

; -----------------------------------------------------------------------------
.section "ObjectFrame" free
; -----------------------------------------------------------------------------
  ; Frame-by-frame housekeeping of the objects.
  ; Assume that all registers are used.
  ObjectFrame:
    ld ix,ObjectAttributeTable
    .rept MAX_OBJECTS       ; Run through all OAT objects.
      ld a,(ix+OAT_STATUS)  ; Get the status byte.
      or a                  ; Zero means object is idle.
      jp z,+                ; If idle, then skip forward, else process object.
        call HandleSelfDestruct
        call HandleMasks
        call AnimateObject
        call MoveObject

      +:
      ld de,OAT_ELEMENT_SIZE
      add ix,de             ; Point to status byte of next object.
    .endr                   ; Repeat to test and process all objects.

    ; Finish by drawing the objects as they look by now.
    call DrawObjects
  ret
.ends
