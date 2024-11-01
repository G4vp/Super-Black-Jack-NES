# Task 1
ca65 src/backgrounds.asm
ca65 src/reset.asm
ld65 src/reset.o src/backgrounds.o -C nes.cfg -o Task1.nes