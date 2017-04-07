CFLAG = -threaded -rtsopts -with-rtsopts=-N
CC = ghc

test: $(OBJS)
	$(CC) $(CFLAG) hw5.hs

clean:
	del *.exe
	del *.hi
	del *.o