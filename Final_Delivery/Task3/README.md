# Task 2
ca65 src/task2_main.asm
ca65 src/task2_controllers.asm
ca65 src/task2_reset.asm
ld65 src/task2_controllers.o src/task2_reset.o src/task2_main.o -C nes.cfg -o Task2.nes