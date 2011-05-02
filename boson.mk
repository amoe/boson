# boson.mk

prefix = /usr/local
libdir = $(prefix)/share/scheme/r6rs
bindir = $(prefix)/bin
libname = boson

daemontools_path = $(prefix)/etc/daemontools/boson

live:
	rsync -rtv

install:
	cp run-server.sps $(bindir)/boson
	chmod +x $(bindir)/boson
	mkdir -p $(libdir)/boson
	cp src/* $(libdir)/boson

install_dt: install
	mkdir -p $(daemontools_path)
	ln -sf $(bindir)/boson $(daemontools_path)/run

test:
	if [ ! -h boson ]; then ln -s src boson; fi
	mosh --loadpath=/usr/local/share/scheme/r6rs test.scm
	rm -f boson
