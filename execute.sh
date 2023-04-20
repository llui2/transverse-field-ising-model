rm -r results
# SAMPLE
gfortran -c r1279/r1279.f90 r1279/ran2.f code/model.f code/metropolis.f
chmod +x metropolis.o model.o r1279.o ran2.o
gfortran metropolis.o model.o r1279.o ran2.o -o metropolis.out
rm *.o
rm *.mod
./metropolis.out
rm *.out
# BINNING
python3 code/binning.py
# PLOTTING
python3 plots/timeseries.py
python3 plots/averages.py
python3 plots/error.py
open results/averages_T*.pdf

