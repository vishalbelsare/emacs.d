.PHONY: clone install

all: install

clone:
	@if [ -d ~/.emacs.d ]; then \
	echo "Do something with the existing ~/.emacs.d folder first."; \
	exit 1; \
	fi;
	git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d

install:
	ln -svf `pwd`/.spacemacs ~/.spacemacs
	if [ ! -d ~/.spacemacs-layers ]; then ln -svf `pwd` ~/.spacemacs-layers; fi

