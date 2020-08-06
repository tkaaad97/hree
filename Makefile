.PHONY: lib clean test

ghci:
	docker-compose run --rm app stack ghci

lib: src shader package.yaml
	docker-compose run --rm app stack build --test --no-run-tests

exec: src shader examples package.yaml
	@if [ -z ${EXAMPLE} ]; then echo "usage: make exec EXAMPLE=flat-color-1" >&2; exit 1; fi
	docker-compose run --rm app stack exec ${EXAMPLE} -- ${ARGS}

clean:
	docker-compose run --rm app stack clean
	rm -f cbits/glxoverride.o
	rm -f libglxoverride.so

test: libglxoverride.so
	docker-compose run --rm -e LD_PRELOAD=./libglxoverride.so app stack test hree:hree-test

libglxoverride.so: cbits/glxoverride.c
	docker-compose run --rm app gcc -shared -fPIC cbits/glxoverride.c -o libglxoverride.so
