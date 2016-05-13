.section "Object Data" free
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

  .include "Objects/Swabby/Swabby-Data.inc"
  .include "Objects/Gargoyle/Gargoyle-Data.inc"
.ends
