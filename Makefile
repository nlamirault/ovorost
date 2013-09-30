PROJECT_NAME=ovorost

PROJ_HOME=$(shell pwd)
COVER_DIR=/tmp/
REP_DIR=$(COVER_DIR)/$(PROJECT_NAME)-coverage-report

all:
	echo "targets: clean test"

.config.lisp: Makefile
	@test -d $(REP_DIR) || mkdir $(REP_DIR)
	@echo "(push \"$(PROJ_HOME)/\" asdf:*central-registry*)" > .config.lisp

clean: .config.lisp
	-find . \( -name '*.fasl' -o -name 'lift.dribble' \) | xargs rm


test: .config.lisp clean
	sbcl --noinform --load .config.lisp --load misc/ut-cover.lisp 
