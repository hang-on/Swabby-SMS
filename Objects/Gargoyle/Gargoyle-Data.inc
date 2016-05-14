GargoyleTiles:
  ; Tile index $000
  .db $00 $00 $00 $00 $0C $0C $0C $0C $1E $12 $12 $1E $27 $21 $21 $3F $23 $21 $21 $3F $15 $15 $15 $1F $0A $0A $0A $0F $0F $0F $0F $0F
  ; Tile index $001
  .db $0E $0E $0E $0E $1F $11 $11 $1F $3F $21 $21 $3F $63 $41 $41 $7F $8F $85 $85 $FF $BA $9A $9A $FA $B8 $A8 $A8 $F8 $4C $44 $44 $FC
  ; Tile index $002
  .db $1F $10 $10 $1F $3F $20 $20 $3F $7F $40 $40 $5E $61 $52 $40 $4C $A0 $A0 $A0 $FF $91 $91 $91 $FF $AE $AE $AE $FE $50 $50 $50 $50
  ; Tile index $003
  .db $C6 $82 $82 $FE $CA $4A $4A $FE $F5 $75 $75 $FF $FA $5A $5A $FA $F8 $E8 $E8 $F8 $88 $88 $88 $F8 $48 $48 $48 $78 $F0 $F0 $F0 $F0
  ; Tile index $004
  .db $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $00 $31 $31 $31 $31 $7F $4F $4F $7F $8E $82 $82 $FF $BF $BF $BF $FF
  ; Tile index $005
  .db $00 $00 $00 $00 $00 $00 $00 $00 $18 $18 $18 $18 $7E $66 $66 $7E $FF $C1 $C1 $FF $87 $81 $81 $FF $83 $81 $81 $FF $67 $67 $67 $FF
  ; Tile index $006
  .db $5F $50 $50 $5F $3F $20 $20 $3F $7F $40 $40 $5E $61 $52 $40 $4C $A0 $A0 $A0 $FF $91 $91 $91 $FF $AE $AE $AE $FE $50 $50 $50 $50
  ; Tile index $007
  .db $DE $9E $9E $FE $DA $5A $5A $FE $F5 $75 $75 $FF $FA $5A $5A $FA $F8 $E8 $E8 $F8 $88 $88 $88 $F8 $48 $48 $48 $78 $F0 $F0 $F0 $F0
GargoyleTilesEnd:

.define GC ($2400-$2000)/32

; Metasprite
Gargoyle1:
  .db 4
  .db -4, -4, 4, 4
  .db -4, GC+0, 4, GC+1, -4, GC+2, 4, GC+3
Gargoyle2:
  .db 4
  .db -4, -4, 4, 4
  .db -4, GC+4, 4, GC+5, -4, GC+6, 4, GC+7

GargoyleFlying:
  .dw $0700 Gargoyle1 $07ff Gargoyle2

Pattern4: ; Neo sine-wave.
  .db 2, -1, 16, -2, -1, 32, 2, -1, 16
  .db 2, -1, 16, -2, -1, 32, 2, -1, 16
  .db 2, -1, 16, -2, -1, 32, 2, -1, 16
  .db 2, -1, 16, -2, -1, 32, 2, -1, 16
  .db 2, -1, 24
  .db SELF_DESTRUCT

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
