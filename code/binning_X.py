import numpy as np
import pandas as pd


# Total number of binning steps
numbins = 17

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
    
# Number of spins
N = L**2
print(TEMP_list)
print(H_list)

for i in range(len(TEMP_list)):
    for j in range(len(H_list)):

        TEMP = TEMP_list[i][:1] + TEMP_list[i][2:]
        H = H_list[i][:1] + H_list[i][2:]

        df = pd.read_csv(f"results/sample/T{TEMP}_Γ{H}.dat")
        print(df)
        exit()


        # Read data from sample file
        data = []
        with open(f"results/sample/T{TEMP}_Γ{H}.dat") as f:
            f.readline()
            for line in f:
                mz = float(line.strip().split(",")[1])
                mx = float(line.strip().split(",")[2])
                E = float(line.strip().split(",")[3])
                data.append({"mz": mz}, {"mx": mx}, {"E": E})

        print(mz)
        exit()

        size0 = len(data)

        # Initialize variables to compute averages
        x_avg = 0
        averages = []

        with open("binned_energy.dat", "w") as f:
            for m in range(numbins):
                # Compute averages every 2**m steps
                bin_size = 2**m
                binned_data = [data[i:i+bin_size] for i in range(0, size0, bin_size)]
                for binned in binned_data:
                        x_avg = sum(d["x"] for d in binned) / bin_size
                        averages.append({"x": x_avg})

                # Compute averages and variance
                x_mean = np.mean([d["x"] for d in averages])
                x_std = np.std([d["x"] for d in averages])/np.sqrt(len(averages)-1)

                f.write(f"{bin_size:5d}, {x_mean/N:12.8f}, {x_std/N:12.8f}\n")
                averages = []
