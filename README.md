# TRANSVERSE FIELD ISING MODEL

Path-integral quantum Monte Carlo simulation of the Ising model in a transverse field.

## About

`code` contains all the scripts for the simulation calculations.

`plots` contains the scripts for the plots.

`r1279` contains the random number generator scripts, provided by Matteo Palassini.

## Execution
In order to run the simulations in the console, one should set the parameters in the `input.txt` file and then execute `execute.sh` to obtain the results. A new directory `results` will appear with a `sample` file containing the results of the simulation, a `binning` file containing the results of the binning analysis, and all the plots for the parameters introduced in the `input.txt` file.