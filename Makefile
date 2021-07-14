.PHONY: build exec exec-docker clean test test-docker

build: src shader package.yaml
	stack build --test --no-run-tests

build-in-docker: src shader package.yaml
	docker-compose run --rm app stack build --test --no-run-tests

exec:
	@if [ -z ${EXAMPLE} ]; then echo "usage: make exec EXAMPLE=flat-color-1" >&2; exit 1; fi
	stack exec ${EXAMPLE} -- ${ARGS}

clean:
	stack clean
	rm -f cbits/glxoverride.o
	rm -f libglxoverride.so

test: build libglxoverride.so
	LD_PRELOAD=./libglxoverride.so stack test hree:hree-test

test-in-docker: libglxoverride.so
	docker-compose run --rm -e LD_PRELOAD=./libglxoverride.so app stack test hree:hree-test

libglxoverride.so: cbits/glxoverride.c
	gcc -shared -fPIC cbits/glxoverride.c -o libglxoverride.so
