import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import os

plt.rc('font', family='Times', size=15)
plt.rc('mathtext', fontset='cm')

# Read 'input.txt'
with open("input.txt") as f:
       f.readline()
       L = int(f.readline()) #line 2
       for i in range(4):
              f.readline()
       TEMP_list = f.readline().split() #line 7
       for i in range(2):
              f.readline()
       H_list = f.readline().split() #line 10
       for i in range(3):
              f.readline()
       MCTOT = f.readline().split() #line 14

# Number of spins
MCTOT = int(MCTOT[0])

for i in range(len(TEMP_list)):
       for j in range(len(H_list)):

              TEMP = TEMP_list[i][:1] + TEMP_list[i][2:]
              H = H_list[j][:1] + H_list[j][2:]

              # Read data from sample file
              df = pd.read_csv(f"results/sample/T{TEMP}_Γ{H}.dat")
              mcs = df["MCS"].to_numpy()
              mz = df["Mz"].to_numpy()
              mx = df["Mx"].to_numpy()
              ene = df["Ene"].to_numpy()

              # Set up figure and subplots
              fig, (ax, ax1, ax2) = plt.subplots(nrows=3, ncols=1, sharex=True, figsize=(12, 6))
              plt.subplots_adjust(wspace=0.1,hspace=0.13,left=0.07,top=0.9,right=0.98,bottom=0.1)

              ax.tick_params(direction='in', top=True, right=True)
              ax1.tick_params(direction='in', top=True, right=True)
              ax2.tick_params(direction='in', top=True, right=True)

              ls = 15
              ax.tick_params(axis='both', labelsize=ls)
              ax1.tick_params(axis='both', labelsize=ls)
              ax2.tick_params(axis='both', labelsize=ls)

              # Plot
              fig.suptitle(f"Time Series for $T=${TEMP_list[i]} and $\\Gamma=${H_list[j]}")
              ax.plot(mcs, mz, linestyle='-',linewidth=1,marker=' ',color='red')
              ax.set_ylabel('$| \\hat{m}_z |$', fontfamily='Times')
              steps = MCTOT/10
              ax.set_xticks(np.arange(0, MCTOT+steps, step=steps))
              ax.set_yticks(np.arange(-1, 2, step=1))
              ax.set_xlim(0, MCTOT)
              ax.set_ylim(0, 1.1)

              ax1.plot(mcs, mx, linestyle='-',linewidth=1,marker=' ',color='red')
              ax1.set_ylabel('$ \\hat{m}_x $', fontfamily='Times')
              ax1.set_yticks(np.arange(-1, 2, step=1))
              ax1.set_ylim(0, 1.1)

              ax2.plot(mcs, ene, linestyle='-',linewidth=1,marker=' ',color='red')
              ax2.set_xlabel('MCS', fontfamily='Times')
              ax2.set_ylabel('$E/N$', fontfamily='Times')
              ax2.set_ylim(top=0)

              # Save figure
              plt.savefig(f'results/timelseries_T{TEMP}_Γ{H}.pdf')