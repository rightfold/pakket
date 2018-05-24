BUILD=build

FC=gfortran
FCFLAGS=-Wall -Wextra -Wpedantic -J ${BUILD}

F_SOURCES=$(shell find src/*.f90)
F_OBJECTS=$(patsubst %.f90,${BUILD}/%.f90.o,${F_SOURCES})

all: build

.PHONY: clean
clean:
	rm -rf ${BUILD}

build: ${F_OBJECTS}

${BUILD}/%.f90.o: %.f90
	mkdir -p $(dir $@)
	${FC} ${FCFLAGS} -c -o $@ $<
