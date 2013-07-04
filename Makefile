FPC='/usr/local/lib/fpc/2.7.1/ppcx64'
all:
	make build
	make clean

build:
	$(FPC) index.pas -oindex.cgi -Fumodules/*

clean:
	rm index.o
	rm index.or
	rm modules/viewimagemodule/viewimagemodule.o
	rm modules/viewimagemodule/viewimagemodule.ppu
	rm modules/addvotemodule/addvotemodule.o
	rm modules/addvotemodule/addvotemodule.ppu
	rm modules/uploadmodule/uploadmodule.o
	rm modules/uploadmodule/uploadmodule.ppu
	rm modules/homemodule/homemodule.o
	rm modules/homemodule/homemodule.ppu
	rm modules/randommodule/randommodule.o
	rm modules/randommodule/randommodule.ppu
	rm modules/searchmodule/searchmodule.o
	rm modules/searchmodule/searchmodule.ppu
