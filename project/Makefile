


all: tigc

PHONY: all clean

tigc: tiger.sml tiger.mlb tokens.sml
	mllex tiger.lex
	mlton -output tigc tiger.mlb

clean: 
	   rm tigc
	   


