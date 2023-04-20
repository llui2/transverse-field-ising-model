import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import FormatStrFormatter
import pandas as pd
import os

plt.rc('font', family='Times', size=16)
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

for i in range(len(TEMP_list)):
       for j in range(len(H_list)):

              TEMP = TEMP_list[i][:1] + TEMP_list[i][2:]
              H = H_list[j][:1] + H_list[j][2:]

              # Read data from sample file
              df = pd.read_csv(f"results/binning/T{TEMP}_Γ{H}.dat")
              # remove last n rows
              df = df[:-1]
              binsize = df["binsize"].to_numpy()
              s_mz = df["σ_mz"].to_numpy()
              s_mx = df["σ_mx"].to_numpy()
              s_e = df["σ_e"].to_numpy()

              # Set up figure and subplots
              fig, (ax, ax1, ax2) = plt.subplots(nrows=1, ncols=3, figsize=(15, 5))
              plt.subplots_adjust(wspace=0.2,hspace=0.2,left=0.05,top=0.90,right=0.98,bottom=0.1)

              ax.tick_params(direction='in', top=True, right=True)
              ax1.tick_params(direction='in', top=True, right=True)
              ax2.tick_params(direction='in', top=True, right=True)

              ls = 15
              ax.tick_params(axis='both', labelsize=ls)
              ax1.tick_params(axis='both', labelsize=ls)
              ax2.tick_params(axis='both', labelsize=ls)

              ax.set_yscale('log')
              ax1.set_yscale('log')
              ax2.set_yscale('log')

              ax.yaxis.set_minor_formatter(FormatStrFormatter(' '))
              ax1.yaxis.set_minor_formatter(FormatStrFormatter(' '))
              ax2.yaxis.set_minor_formatter(FormatStrFormatter(' '))

              # Plot
              fig.suptitle(f"Binning error for $T=${TEMP_list[i]} and $\\Gamma=${H_list[j]}")
              ax.plot(binsize, s_mz, linestyle='-',linewidth=1,marker='x',color='red')
              ax.set_xlabel('bin size', fontfamily='Times')
              ax.set_ylabel('$\\sigma_{\\hat{m}_z}$', fontfamily='Times')
              #ax.set_xticks(np.arange(0, 1, step=0.1))
              #ax.set_xlim(0, MCTOT)
              ax.set_ylim(1e-4,)

              ax1.plot(binsize, s_mx, linestyle='-',linewidth=1,marker='x',color='red')
              ax1.set_xlabel('bin size', fontfamily='Times')
              ax1.set_ylabel('$\\sigma_{\\hat{m}_x}$', fontfamily='Times')
              #ax.set_xticks(np.arange(0, 1, step=0.1))
              #ax.set_xlim(0, MCTOT)
              ax1.set_ylim(1e-4,)

              ax2.plot(binsize, s_e, linestyle='-',linewidth=1,marker='x',color='red')
              ax2.set_xlabel('bin size', fontfamily='Times')
              ax2.set_ylabel('$\\sigma_{E/N}$', fontfamily='Times')
              #ax.set_xticks(np.arange(0, 1, step=0.1))
              #ax.set_xlim(0, MCTOT)
              ax2.set_ylim(1e-4,)

              # Save figure
              plt.savefig(f'results/error_T{TEMP}_Γ{H}.pdf')