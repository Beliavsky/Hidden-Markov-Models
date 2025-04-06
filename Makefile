executables = xbasic_stats_gfort.exe
FC     = gfortran
FFLAGS = -O0 -Wall -Werror=unused-parameter -Werror=unused-variable -Werror=unused-function -Wno-maybe-uninitialized -Wno-surprising -fbounds-check -static -g -fmodule-private
obj    = kind.o constants.o util.o basic_stats.o random.o xbasic_stats.o

all: $(executables)

# Compile .f90 to .o
%.o: %.f90
	$(FC) $(FFLAGS) -c $<

xbasic_stats_gfort.exe: kind.o constants.o util.o basic_stats.o random.o xbasic_stats.o
	$(FC) -o xbasic_stats_gfort.exe kind.o constants.o util.o basic_stats.o random.o xbasic_stats.o $(FFLAGS)

run: $(executables)
	./xbasic_stats_gfort.exe

clean:
	rm -f $(executables) $(obj)

