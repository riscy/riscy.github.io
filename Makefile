EMACSBIN?=emacs

.PHONY: all
all:
	${EMACSBIN} --script riscy-io.el index.org
	${EMACSBIN} --script riscy-io.el portfolio.org
