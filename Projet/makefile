# Usage:
# make        # compile all binary
# make clean  # remove ALL binaries and objects

.PHONY = all files compile run clean

CC = gfortran	# compiler to use

SRCS := $(wildcard *.f90)
BINS := $(SRCS:%.f90=%)

all:	${BINS}

%:	%.o
	@echo "Checking.."
	${CC}  $< -o $@
	
%.o:	%.c
	@echo "Creating object.."
	${CC} -c $<
	
files:
	
	@echo "Directory:	"
	pwd;
	@echo "Files:	"	
	ls
	@echo "BINS:	"
	@echo ${BINS}
	@echo "SRCS:	"
	@echo ${SRCS}
	

compile:
	@echo "Compiler Running..."
	${CC} fente.f90 -no-pie -L./cfftpack -l Fft -o data.x
	${CC} data_array.f90 -ffree-line-length-512 -no-pie -L./cfftpack -l Fft -o data_array.x

run:
	@echo "Program Running..."
	#./data.x
	./data_array.x

draw:
	@echo "drawing..."
	gnuplot -p -e "load'all_fft.plot'"
	gnuplot -p -e "load'all_fentes.plot'"

animation:
	cd ./Code; \
	pwd; \
	ls; \
	${CC} anim.f90 -no-pie -L./cfftpack -l Fft -o anim.x ; \
	mv *.x ../Bin ; \
	cd ../Bin ; \
	./anim.x ; \
	mv *.data ../Plots ; \
	cd ../Plots ;\
	gnuplot -p -e "load'anim.plot'" ; \
	mv *.data ../Data ; \
	mv *.png ../Images ; \

clean:
	@echo "Cleaning up..."
	cd Images/ ; \
	rm -Ivf *.o *.x *.data *.png
	cd Data/ ; \
	rm -Ivf *.o *.x *.data *.png
	cd Bin/ ; \
	rm -Ivf *.o *.x *.data *.png