# Makefile for building srfi-tools for particular schemes

.PHONY: all chibi

all:
	@echo "Availa le targets:"
	@echo "make chibi: Install prerequisites and build chibi-scheme image file"

chibi:
	snow-chibi install srfi.19 || true
	chibi-scheme -I .. -m srfi-tools.main -d chibi-srfi.img
