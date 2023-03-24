rm -r results
# SAMPLE
gfortran -c r1279/r1279.f90 r1279/ran2.f code/model.f code/metropolis.f
chmod +x metropolis.o model.o r1279.o ran2.o
gfortran metropolis.o model.o r1279.o ran2.o -o metropolis.out
rm *.o
rm *.mod
./metropolis.out
rm *.out
# OBSERVABLE
# gfortran -c r1279/r1279.f90 r1279/ran2.f code/model.f code/observables.f
# chmod +x observables.o model.o r1279.o ran2.o
# gfortran observables.o model.o r1279.o ran2.o -o observables.out
# rm *.o
# rm *.mod
# ./observables.out
# rm *.out
# # PLOT OBSERVABLE
# python3 plots/mz.py
# open results/fig*.pdf