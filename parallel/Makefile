RM = rm -rf
GHC = ghc
GHCFLAGS = -O2 -Wall
MAIN = AsmConv/Main.hs
BINDIR = bin

.PHONY: pas
pas:
	mkdir -p $(BINDIR)
	$(GHC) $(GHCFLAGS) -odir $(BINDIR) -hidir $(BINDIR) -o $@ $(MAIN)

.PHONY: clean
clean:
	$(RM) $(BINDIR)
