.PHONY: lib clean ex1 test

lib: src shader package.yaml
	docker-compose run --rm app stack build

run: src shader examples package.yaml
	@if [ -z ${EXAMPLE} ]; then echo "usage: make run EXAMPLE=flat-color-1" >&2; exit 1; fi
	docker-compose run --rm app stack run ${EXAMPLE}

clean:
	docker-compose run --rm app stack clean
	rm -f cbits/glxoverride.o
	rm -f libglxoverride.so

test: libglxoverride.so
	docker-compose run --rm -e LD_PRELOAD=./libglxoverride.so app stack test

libglxoverride.so: cbits/glxoverride.c
	docker-compose run --rm app gcc -shared -fPIC cbits/glxoverride.c -o libglxoverride.so
