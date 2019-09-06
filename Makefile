.PHONY: all
all: index.html portfolio.html
	open index.html

EMACSBIN?=emacs

index.html: riscy-io.el index.org portfolio.html ./notes/*.org
	${EMACSBIN} --script riscy-io.el index.org

portfolio.html: riscy-io.el portfolio.org
	${EMACSBIN} --script riscy-io.el portfolio.org
