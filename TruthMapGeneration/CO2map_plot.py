import matplotlib.pyplot as plt
import numpy as np
import math




NstepMax=121

Ndata=17
iloc_plot=5


nx=51
ny=51



filename='run.00181_sca_node.dat'

sat_data=np.zeros(51*51)
sat_map=np.zeros((51,51))





count = 0    
for lines in open(filename):
    count += 1
    lines = lines.rstrip('\n')
    #lines = lines.split(None,1)
    #times, features = lines
    count_columns = 0
    if count>26011:
        for e in lines.split():
            count_columns +=1
            if count_columns==5:
                val = e
                xi = float(val)
                sat_data[count-26012]= xi
                
                
for i in range(0,nx):
    for j in range(0,ny):
        #logperm[i,j]=math.log(AA[j+i*nx])
        sat_map[i,j]=sat_data[j+i*nx]

#plt.imshow(logperm, cmap='jet', interpolation='gaussian')

sat_map[sat_map == 0] = np.nan

fig,ax=plt.subplots()

plt.imshow(sat_map, cmap='jet', interpolation='gaussian')
plt.colorbar()
#plt.clim(2.5,7)

figname = 'Sat_Map_15yr'
plt.savefig(figname,bbox_inches='tight')
plt.show()
