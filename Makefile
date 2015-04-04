render.ppm:	htracer
	./htracer +RTS -N12 > render.ppm
htracer:	$(shell find . -type f -name '*.hs')
	ghc -O2 -threaded -o htracer htracer.hs
