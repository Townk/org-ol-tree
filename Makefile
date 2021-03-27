CASK ?= cask
EMACS ?= emacs

all: test

test:
	${CASK} exec buttercup -L . -L tests

update:
	${CASK} install

.PHONY:	all test install
