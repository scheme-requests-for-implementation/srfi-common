# Makefile for building srfi-tools for particular schemes

.PHONY: all chibi chicken chicken-clean

all:
	@echo "Available targets:"
	@echo "make chibi: Install prerequisites and build chibi-scheme image file"
	@echo "make chicken: Build and install the Chicken srfi-tools egg"

chibi:
	snow-chibi install srfi.19 || true
	chibi-scheme -I .. -m srfi-tools.main -d chibi-srfi.img

chicken:
	chicken-install -s
	@echo "You can now use make chicken-clean to remove build files"

chicken-clean:
	rm -f *.so *.o *.link *.import.scm *.build.sh *.install.sh
