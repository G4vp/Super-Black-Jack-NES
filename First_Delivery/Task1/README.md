# Task 1
ca65 src/task1_backgrounds.asm
ca65 src/task1_reset.asm
ld65 src/task1_reset.o src/task1_backgrounds.o -C nes.cfg -o Task1.nes