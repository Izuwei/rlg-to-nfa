# Author: Jakub Sadilek
# Login: xsadil07

all:
	@ghc rlg-2-nfa.hs -o rlg-2-nfa

clean:
	@rm rlg-2-nfa rlg-2-nfa.o rlg-2-nfa.hi
