# Task 2
ca65 src/black_jack.asm
ca65 src/controllers.asm
ca65 src/reset.asm
ld65 src/controllers.o src/reset.o src/black_jack.o -C nes.cfg -o Task2.nes