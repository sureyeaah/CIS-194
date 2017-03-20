CFLAG = -threaded -rtsopts -with-rtsopts=-N
CC = ghc

test: $(OBJS)
	$(CC) $(CFLAG) hw2.hs

clean:
	del *.exe
	del *.hi
	del *.o