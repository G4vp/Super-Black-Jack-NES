# Task 3
ca65 src/task3_main.asm
ca65 src/task3_controllers.asm
ca65 src/task3_reset.asm
ld65 src/task3_controllers.o src/task3_reset.o src/task3_main.o -C nes.cfg -o Task3.nes