
#	Compiler
override F90 = gfortran
override FFL = `sdl2-config --cflags --libs` -lSDL2_ttf

#	Files
override EXE = ringwormgo

override FSR = $(shell find ./ -type f -name '*.f90')
override OBJ := $(FSR:.f90=.o)

#	targets
.PHONY: all
all: $(EXE)

$(EXE): $(OBJ)
	$(F90) $(OBJ) -o $@ libsdl2.a libsdl2_ttf.a $(FFL)

%.o: %.f90
	$(F90) -c $< -o $@ $(FFL)

.PHONY: clean
clean:
	rm $(OBJ)
	rm $(EXE)