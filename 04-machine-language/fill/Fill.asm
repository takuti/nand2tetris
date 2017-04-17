// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel. When no key is pressed, the
// program clears the screen, i.e. writes "white" in every pixel.

(LOOP)
  @24576 // read keyboard
  D=M
  @WHITE
  D;JEQ // if no input => set white
(BLACK)
  @0
  D=A-1
  @color
  M=D // all of 16 bits are 1 (i.e. -1 in decimal)
  @FILL
  0;JMP
(WHITE)
  @color
  M=0
(FILL)
  @SCREEN // base address: 16384
  D=A
  @addr
  M=D
  @8192 // 32 (words/row) * 256 (row) = 8192 words in total
  D=A
  @n
  M=D
(FILL_LOOP)
  @color
  D=M
  @addr
  A=M
  M=D
  @addr
  M=M+1
  @SCREEN // fill all words => break
  D=A
  @addr
  D=M-D
  @n
  D=M-D
  @FILL_LOOP
  D;JGT
(END)
  @LOOP
  0;JMP
