OBJECTStexerupter = Main.hs ParseHTML.hs CompileTex.hs Options.hs ProcessHTML.hs
COMPILE= ghc

texerupter: $(OBJECTStexerupter)
	$(COMPILE) -o $@ $<


clean:
	rm -f texerupter *.hi *.o

