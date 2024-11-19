# Task 4
ca65 src/task4_main.asm
ca65 src/task4_controllers.asm
ca65 src/task4_reset.asm
ld65 src/task4_controllers.o src/task4_reset.o src/task4_main.o -C nes.cfg -o task4.nes