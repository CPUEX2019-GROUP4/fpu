RM = rm -rf
GHC = ghc
GHCFLAGS = -O2 -Wall
MAIN = AsmConv/Main.hs
BINDIR = bin
EXE = pas

.PHONY: $(EXE)
$(EXE):
	mkdir -p $(BINDIR)
	$(GHC) $(GHCFLAGS) -odir $(BINDIR) -hidir $(BINDIR) -o $@ $(MAIN)

.PHONY: clean
clean:
	$(RM) $(BINDIR) $(EXE)
