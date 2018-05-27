BUILD=build

FC=gfortran
FCFLAGS=-Wall -Wextra -Wpedantic -J ${BUILD}

F_SOURCES=$(shell find src -name '*.f90')
F_OBJECTS=$(patsubst %.f90,${BUILD}/%.f90.o,${F_SOURCES})

F_T_SOURCES=$(shell find test -name '*.f90')
F_T_OBJECTS=$(patsubst %.f90,${BUILD}/%.f90.out,${F_T_SOURCES})

all: build

.PHONY: clean
clean:
	rm -rf ${BUILD}

build: ${F_OBJECTS}

.PHONY: test
test: ${F_T_OBJECTS}
	prove -v $^

${BUILD}/%.f90.o: %.f90
	mkdir -p $(dir $@)
	${FC} ${FCFLAGS} -c -o $@ $<

${BUILD}/%.f90.out: %.f90 ${F_OBJECTS}
	mkdir -p $(dir $@)
	${FC} ${FCFLAGS} -o $@ $^
