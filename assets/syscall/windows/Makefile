TOOLCHAIN_NASM= ../../toolchain/nasm/
TOOLCHAIN_Link= ../../toolchain/GoLink/
FILE=

assemble:
	$(TOOLCHAIN_NASM)nasm.exe -fwin64 $(FILE).asm -o obj/$(FILE).obj

link:
	$(TOOLCHAIN_LINKER)GoLink.exe /console /entry start obj/$(FILE).obj kernel32.dll msvcrt.dll /fo exe/$(FILE).exe

build:
	@make assemble link
