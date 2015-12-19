canvas.js: canvas.hs
	hastec -Wall canvas.hs

clean:
	rm -r main canvas.js canvas.o canvas.hi
