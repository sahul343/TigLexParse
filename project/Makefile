
%.lex.sml : %.lex
	mllex $<

all: tigc

PHONY: all clean

tigc: tiger.mlb tokens.sml term.sml tigh.sml tiger.lex.sml
	mlton -output tigc tiger.mlb

clean: 
	rm tigc
	   


