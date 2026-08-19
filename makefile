TD ?= ./test/
export CTL_TESTDIR := $(TD)

RUN = dune exec src-test/main.exe --no-buffer --force --

.PHONY: test test-slow clean
test:
	$(RUN) -e -q
test-slow:
	$(RUN) -e
coverage:
	dune build @coverage --no-buffer --force --instrument-with bisect_ppx_ng
	bisect-ppx-report summary
clean:
	dune clean && rm -rf _coverage