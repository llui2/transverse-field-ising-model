import numpy as np
import pandas as pd
import os

print("Binning data...")

os.system("mkdir -p results/binning")

# Total number of binning steps
numbins = 12

# Read 'input.txt'
with open("input.txt") as f:
    f.readline()
    L = int(f.readline())
    for i in range(4):
        f.readline()
    TEMP_list = f.readline().split()
    for i in range(2):
        f.readline()
    H_list = f.readline().split()
    for i in range(3):
        f.readline()
    MCTOT = f.readline().split() #line 14
    for i in range(1):
        f.readline()
    SC = f.readline().split() #line 16

MCTOT = int(MCTOT[0])
SC = int(SC[0])

for i in range(len(TEMP_list)):
    for j in range(len(H_list)):

        TEMP = TEMP_list[i][:1] + TEMP_list[i][2:]
        H = H_list[j][:1] + H_list[j][2:]

        # Read data from sample file
        data = []
        df = pd.read_csv(f"results/sample/T{TEMP}_Γ{H}.dat")
        mcs = df["MCS"].to_numpy()
        #check if the non equilibrated part was discarded in the sample file for the binning
        if len(mcs)-1 == int(MCTOT/SC):
            mz = df["Mz"].to_numpy()[int(MCTOT/(2.5*SC)):]
            mx = df["Mx"].to_numpy()[int(MCTOT/(2.5*SC)):]
            ene = df["Ene"].to_numpy()[int(MCTOT/(2.5*SC)):]
        else:
            mz = df["Mz"].to_numpy()
            mx = df["Mx"].to_numpy()
            ene = df["Ene"].to_numpy()
        data.append(mz)
        data.append(mx)
        data.append(ene)

        size0 = len(data[0])

        # Initialize variables to compute averages
        mz_avg = 0
        mx_avg = 0
        averages = []

        with open(f"results/binning/T{TEMP}_Γ{H}.dat", "w") as f:
            f.write(f"binsize,<mz>,σ_mz,<mx>,σ_mx,<e>,σ_e\n")
            for m in range(numbins):
                # Compute averages every 2**m steps
                bin_size = 2**m
                binned_data = [[] for l in range(len(data))]
                for k in range(len(data)):
                    binned_data[k] = [data[k][l:l+bin_size] for l in range(0, size0, bin_size)]

                averages = [[] for i in range(len(data))]
                for k in range(len(binned_data[0])):
                    mz_avg = sum(binned_data[0][k]) / bin_size
                    mx_avg = sum(abs(binned_data[1][k])) / bin_size
                    ene_avg = sum(binned_data[2][k]) / bin_size
                    averages[0].append(mz_avg)
                    averages[1].append(mx_avg)
                    averages[2].append(ene_avg)

                # Compute averages and variance
                mz_mean = np.mean(averages[0])
                mz_std = np.std(averages[0])/np.sqrt(len(averages[0])-1)
                
                mx_mean = np.mean(averages[1])
                mx_std = np.std(averages[1])/np.sqrt(len(averages[1])-1)

                ene_mean = np.mean(averages[2])
                ene_std = np.std(averages[2])/np.sqrt(len(averages[2])-1)

                f.write(f"{bin_size:5d},{mz_mean:0.8f},{mz_std:0.8f},{mx_mean:0.8f},{mx_std:0.8f},{ene_mean:0.8f},{ene_std:0.8f}\n")

