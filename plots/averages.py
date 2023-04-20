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

bs = 256 # binsize = 2^m

for i in range(len(TEMP_list)):
       mz_list = []
       s_mz_list = []
       mx_list = []
       s_mx_list = []
       e_list = []
       s_e_list = []
       for j in range(len(H_list)):

              TEMP = TEMP_list[i][:1] + TEMP_list[i][2:]
              H = H_list[j][:1] + H_list[j][2:]

              data = []
              df = pd.read_csv(f"results/binning/T{TEMP}_Γ{H}.dat")
              binsize = df["binsize"].to_numpy()
              mz = df["<mz>"].to_numpy()
              s_mz = df["σ_mz"].to_numpy()
              mx = df["<mx>"].to_numpy()
              s_mx = df["σ_mx"].to_numpy()       
              e = df["<e>"].to_numpy()
              s_e = df["σ_e"].to_numpy()

              k = np.where(binsize==bs)[0][0]

              mz_list.append(mz[k])
              s_mz_list.append(s_mz[k])
              mx_list.append(mx[k])
              s_mx_list.append(s_mx[k])
              e_list.append(e[k])
              s_e_list.append(s_e[k])

       H_list_plot = np.array(H_list, dtype=float)

       # Set up figure and subplots

       fig, (ax, ax1, ax2) = plt.subplots(nrows=1, ncols=3, figsize=(15, 5))
       plt.subplots_adjust(wspace=0.2,hspace=0.2,left=0.05,top=0.9,right=0.98,bottom=0.1)

       ax.tick_params(direction='in', top=True, right=True)
       ax1.tick_params(direction='in', top=True, right=True)
       ax2.tick_params(direction='in', top=True, right=True)

       ls = 15
       ax.tick_params(axis='both', labelsize=ls)
       ax1.tick_params(axis='both', labelsize=ls)
       ax2.tick_params(axis='both', labelsize=ls)

       labels_formatted = [label.get_text()+'\n' for i, label in enumerate(ax.yaxis.get_majorticklabels())]
       ax.set_yticklabels(labels_formatted)
       labels_formatted = [label.get_text()+'\n' for i, label in enumerate(ax1.yaxis.get_majorticklabels())]
       ax1.set_yticklabels(labels_formatted)

       # Plot
       fig.suptitle(f"Averages for $T=${TEMP_list[i]}")

       ax.errorbar(H_list_plot, mz_list, yerr=s_mz_list, linestyle='-',linewidth=1, marker=' ',markersize=3, capsize=3,markeredgewidth=1,elinewidth=1.5,color='red')
       ax.set_xlabel('$\\Gamma$', fontfamily='Times')
       ax.set_ylabel('$\\langle | \\hat{m}_z | \\rangle$', fontfamily='Times')
       ax.set_xticks(np.arange(0, 5, step=0.5))
       ax.set_yticks(np.arange(0, 2, step=0.2))
       ax.set_xlim(0, 4.6)
       ax.set_ylim(0, 1.1)

       ax1.errorbar(H_list_plot, mx_list, yerr=s_mx_list, linestyle='-',linewidth=1, marker=' ',markersize=3, capsize=3,markeredgewidth=1,elinewidth=1.5,color='red')
       ax1.set_xlabel('$\\Gamma$', fontfamily='Times')
       ax1.set_ylabel('$\\langle \\hat{m}_x \\rangle$', fontfamily='Times')
       ax1.set_xticks(np.arange(0, 5, step=0.5))
       ax1.set_yticks(np.arange(0, 2, step=0.2))
       ax1.set_xlim(0, 4.6)
       ax1.set_ylim(0, 1.1)

       ax2.errorbar(H_list_plot, e_list, yerr=s_e_list, linestyle='-',linewidth=1, marker=' ',markersize=3, capsize=3,markeredgewidth=1,elinewidth=1.5,color='red')
       ax2.set_xlabel('$\\Gamma$', fontfamily='Times')
       ax2.set_ylabel('$\\langle E \\rangle / N$', fontfamily='Times')
       ax2.set_xticks(np.arange(0, 6, step=0.5))
       ax2.set_xlim(0, 4.6)
       #ax2.set_ylim(top=0)

       # Save figure
       plt.savefig(f'results/averages_T{TEMP}.pdf')