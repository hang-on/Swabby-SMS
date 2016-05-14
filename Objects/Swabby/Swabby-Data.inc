

SwabbyTiles:
  ; Tile index $000
  .db $01 $01 $01 $01 $03 $03 $03 $03 $06 $07 $06 $06 $0C $0F $0C $0C $18 $1F $18 $18 $18 $17 $10 $10 $3C $33 $30 $30 $3C $23 $20 $20
  ; Tile index $001
  .db $FC $FC $FC $FC $06 $FE $06 $06 $03 $FF $03 $03 $0F $FF $0F $0F $18 $F8 $17 $10 $3F $FF $3F $3F $23 $FF $3F $23 $3F $F3 $3F $33
  ; Tile index $002
  .db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $9C $9C $9C $9C $E4 $E4 $FC $E4 $FC $FC $FC $FC $5C $6C $FC $4C $78 $78 $B8 $38
  ; Tile index $003
  .db $FE $F1 $F0 $F0 $DF $D8 $B8 $98 $FF $F8 $88 $88 $7F $78 $68 $68 $1F $1F $17 $17 $0F $0F $08 $08 $07 $07 $07 $07 $00 $00 $00 $00
  ; Tile index $004
  .db $1F $EF $0C $0C $FF $07 $06 $06 $FE $06 $07 $06 $FC $0C $0B $08 $FF $FF $F3 $F3 $FF $FF $0F $0F $F0 $F0 $F0 $F0 $00 $00 $00 $00
  ; Tile index $005
  .db $A0 $A0 $60 $20 $60 $60 $E0 $60 $80 $80 $80 $80 $80 $80 $80 $80 $40 $40 $C0 $40 $C0 $C0 $C0 $C0 $00 $00 $00 $00 $00 $00 $00 $00
  ; Tile index $006
  .db $01 $01 $01 $01 $03 $03 $03 $03 $06 $07 $06 $06 $0C $0F $0C $0C $18 $1F $18 $18 $18 $17 $10 $10 $3C $33 $30 $30 $3C $23 $20 $20
  ; Tile index $007
  .db $FC $FC $FC $FC $06 $FE $06 $06 $03 $FF $03 $03 $0F $FF $0F $0F $1F $FF $10 $10 $3C $FC $33 $30 $39 $F9 $27 $21 $33 $F3 $3F $33
  ; Tile index $008
  .db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $9C $9C $9C $9C $E4 $E4 $FC $E4 $FC $FC $FC $FC $DC $EC $FC $CC $78 $78 $B8 $38
  ; Tile index $009
  .db $FE $F1 $F0 $F0 $DF $D8 $B8 $98 $FF $F8 $88 $88 $7F $78 $68 $68 $1F $1F $17 $17 $0F $0F $08 $08 $07 $07 $07 $07 $00 $00 $00 $00
  ; Tile index $00A
  .db $1F $EF $0E $0E $FF $07 $06 $06 $FE $06 $07 $06 $FC $0C $0B $08 $FF $FF $F3 $F3 $FF $FF $0F $0F $F0 $F0 $F0 $F0 $00 $00 $00 $00
  ; Tile index $00B
  .db $A0 $A0 $60 $20 $60 $60 $E0 $60 $80 $80 $80 $80 $80 $80 $80 $80 $40 $40 $C0 $40 $C0 $C0 $C0 $C0 $00 $00 $00 $00 $00 $00 $00 $00
  ; Tile index $00C
  .db $01 $01 $01 $01 $03 $03 $03 $03 $06 $07 $06 $06 $0C $0F $0C $0C $18 $1F $18 $18 $18 $17 $10 $10 $3C $33 $30 $30 $3C $23 $20 $20
  ; Tile index $00D
  .db $FC $FC $FC $FC $06 $FE $06 $06 $03 $FF $03 $03 $0F $FF $0F $0F $18 $F8 $17 $10 $3F $FF $3F $3F $23 $FF $3F $23 $3F $F3 $3F $33
  ; Tile index $00E
  .db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $9C $9C $9C $9C $E4 $E4 $FC $E4 $FC $FC $FC $FC $5C $6C $FC $4C $78 $78 $B8 $38
  ; Tile index $00F
  .db $3E $31 $30 $30 $5F $58 $78 $58 $BF $B8 $C8 $88 $FF $F8 $88 $88 $FF $FF $A7 $A7 $5F $5F $58 $58 $07 $07 $07 $07 $00 $00 $00 $00
  ; Tile index $010
  .db $1F $EF $0C $0C $FF $07 $06 $06 $FE $06 $07 $06 $FC $0C $0B $08 $FF $FF $F3 $F3 $FF $FF $0F $0F $F0 $F0 $F0 $F0 $00 $00 $00 $00
  ; Tile index $011
  .db $A0 $A0 $60 $20 $60 $60 $E0 $60 $80 $80 $80 $80 $80 $80 $80 $80 $40 $40 $C0 $40 $C0 $C0 $C0 $C0 $00 $00 $00 $00 $00 $00 $00 $00
SwabbyTilesEnd:

.define SW ($2000-$2000)/32

; Metasprite
Swabby1:
  .db 6
  .db -4, -4, -4, 4, 4, 4
  .db -8, SW+0, 0, SW+1, 8, SW+2, -8, SW+3, 0, SW+4, 8, SW+5
Swabby2:
  .db 6
  .db -4, -4, -4, 4, 4, 4
  .db -8, SW+6, 0, SW+7, 8, SW+8, -8, SW+9, 0, SW+10, 8, SW+11
Swabby3:
  .db 6
  .db -4, -4, -4, 4, 4, 4
  .db -8, SW+12, 0, SW+13, 8, SW+14, -8, SW+15, 0, SW+16, 8, SW+17


; Animation table: An array of metasprites with corresponding timer values.
SwabbyFlying:
  .dw $2000 Swabby1 $2000 Swabby3 $2000 Swabby1 $2000 Swabby3
  .dw $2000 Swabby2 $20ff Swabby3 ; MSB $ff (or non-zero value) means loop.

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
