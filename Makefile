.PHONY: lib clean ex1 test

lib: src shader package.yaml
	docker-compose run --rm app stack build

ex1: src shader example1 package.yaml
	docker-compose run --rm app stack run hree-exapmle1

clean:
	docker-compose run --rm app stack clean
	rm -f cbits/glxoverride.o
	rm -f libglxoverride.so

test: libglxoverride.so
	docker-compose run --rm -e LD_PRELOAD=./libglxoverride.so app stack test

libglxoverride.so: cbits/glxoverride.c
	docker-compose run --rm app gcc -shared -fPIC cbits/glxoverride.c -o libglxoverride.so
