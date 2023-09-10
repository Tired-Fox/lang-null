BUILD_DIR := target
TOOLCHAIN_NASM := ./toolchain/nasm
TOOLCHAIN_LINKER := ./toolchain/GoLink

FILE_NAME := main

LINKED := 
MODE :=

assemble:
	$(TOOLCHAIN_NASM)/nasm.exe -fwin64 $(BUILD_DIR)/$(FILE_NAME).asm -o $(BUILD_DIR)/$(FILE_NAME).obj

link:
	$(TOOLCHAIN_LINKER)/GoLink.exe $(MODE) /entry _start $(BUILD_DIR)/$(FILE_NAME).obj $(LINKED)

build:
	@make assemble link

run:
	@target/main.exe

