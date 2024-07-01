.ORIG x3000
label1
  ADD R0, R1, R2
label2
  ADD R2, R3, #-7
  BR label1
  BRn label2
  BRz #1
TARGET
  BRzp label2
  BRnp label1
  BRnz label2
  BRnzp label1
  JMP R4
label3
  .FILL HERE
  JSR label3
  JSRR R5
  LD R6, label2
  LDI R2, label1
  LDR R4, R2, #-5
  LEA R4, TARGET
HERE
  NOT R4, R6
  RET
  .STRINGZ "howdly doodly"
  RTI
THERE
  ST R5, HERE
  STI R5, THERE
  STR R4, R2, #5
test_longer_label_
  .FILL x5
  TRAP x27
  GETC
  OUT
  PUTS
  IN
  PUTSP
  HALT
HELLO
  .STRINGZ "Hello, World!"
.END
