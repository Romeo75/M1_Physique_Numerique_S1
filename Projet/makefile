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
	pwd
	@echo "Files:	"	
	ls

	@echo "BINS:	"
	@echo ${BINS}
	@echo "SRCS:	"
	@echo ${SRCS}
	

compile:
	@echo "Compiler Running..."
	#${CC} fente.f90 -no-pie -L./cfftpack -l Fft -o data.x
	#${CC} data_array.f90 -ffree-line-length-512 -no-pie -L./cfftpack -l Fft -o data_array.x
	${CC} anim.f90 -no-pie -L./cfftpack -l Fft -o anim.x
run:
	@echo "Program Running..."
	#./data.x
	#./data_array.x
	./anim.x

draw:
	@echo "drawing..."
	#gnuplot -p -e "load'all_fft.plot'"
	#gnuplot -p -e "load'all_fentes.plot'"
	gnuplot -p -e "load'anim.plot'"

clean:
	@echo "Cleaning up..."
	rm -rvf *.o ${BINS} *.x *.data *.png
