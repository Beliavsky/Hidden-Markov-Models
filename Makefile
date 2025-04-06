executables = xhidden_markov_gfort.exe
FC     = gfortran
FFLAGS = -O0 -Wall -Werror=unused-parameter -Werror=unused-variable -Werror=unused-function -Wno-maybe-uninitialized -Wno-surprising -fbounds-check -static -g -fmodule-private
obj    = kind.o constants.o util.o random.o basic_stats.o hidden_markov.o xhidden_markov.o

all: $(executables)

# Compile .f90 to .o
%.o: %.f90
	$(FC) $(FFLAGS) -c $<

xhidden_markov_gfort.exe: kind.o constants.o util.o random.o basic_stats.o hidden_markov.o xhidden_markov.o
	$(FC) -o xhidden_markov_gfort.exe kind.o constants.o util.o random.o basic_stats.o hidden_markov.o xhidden_markov.o $(FFLAGS)

run: $(executables)
	./xhidden_markov_gfort.exe

clean:
	rm -f $(executables) $(obj)

