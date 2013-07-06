FPC='/usr/local/lib/fpc/2.7.1/ppcx64'
all:
	make build
	make clean

build:
	$(FPC) index.pas -oindex.cgi -Fumodules/*

clean:
	rm *.o *.or */*/*.o */*/*.ppu
