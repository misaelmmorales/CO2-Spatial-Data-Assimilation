import matplotlib.pyplot as plt
import numpy as np
import math


nx=51
ny=51

filename1='perm_opt_1yr.dat'
filename2='perm_opt_3yrs.dat'
filename2a='perm_opt_5yrs.dat'
filename3='perm_opt_7yrs.dat'
filename3a='perm_opt_10yrs.dat'
filename_true='perm_true.dat'

AA1=np.zeros(nx*ny)
logperm1=np.zeros((nx, ny))
icount=0
for lines in open(filename1):     
    lines = lines.rstrip('\n')
    AA1[icount-1]=float(lines)        
    icount=icount+1    
for i in range(0,nx):
    for j in range(0,ny):
        #logperm[i,j]=math.log(AA[j+i*nx])
        logperm1[i,j]=AA1[j+i*nx]
        
AA2=np.zeros(nx*ny)
logperm2=np.zeros((nx, ny))
icount=0
for lines in open(filename2):     
    lines = lines.rstrip('\n')
    AA2[icount-1]=float(lines)        
    icount=icount+1    
for i in range(0,nx):
    for j in range(0,ny):
        #logperm[i,j]=math.log(AA[j+i*nx])
        logperm2[i,j]=AA2[j+i*nx]
        
AA2a=np.zeros(nx*ny)
logperm2a=np.zeros((nx, ny))
icount=0
for lines in open(filename2a):     
    lines = lines.rstrip('\n')
    AA2a[icount-1]=float(lines)        
    icount=icount+1    
for i in range(0,nx):
    for j in range(0,ny):
        #logperm[i,j]=math.log(AA[j+i*nx])
        logperm2a[i,j]=AA2a[j+i*nx]

AA3=np.zeros(nx*ny)
logperm3=np.zeros((nx, ny))
icount=0
for lines in open(filename3):     
    lines = lines.rstrip('\n')
    AA3[icount-1]=float(lines)        
    icount=icount+1    
for i in range(0,nx):
    for j in range(0,ny):
        #logperm[i,j]=math.log(AA[j+i*nx])
        logperm3[i,j]=AA3[j+i*nx]
        
AA3a=np.zeros(nx*ny)
logperm3a=np.zeros((nx, ny))
icount=0
for lines in open(filename3a):     
    lines = lines.rstrip('\n')
    AA3a[icount-1]=float(lines)        
    icount=icount+1    
for i in range(0,nx):
    for j in range(0,ny):
        #logperm[i,j]=math.log(AA[j+i*nx])
        logperm3a[i,j]=AA3a[j+i*nx]

BB=np.zeros(nx*ny)
logperm_true=np.zeros((nx, ny))
icount=0
for lines in open(filename_true):     
    lines = lines.rstrip('\n')
    BB[icount-1]=float(lines)        
    icount=icount+1    
for i in range(0,nx):
    for j in range(0,ny):
        #logperm[i,j]=math.log(AA[j+i*nx])
        logperm_true[i,j]=BB[j+i*nx]


AAD1=np.mean(np.abs(logperm1-np.transpose(logperm_true)))
AAD2=np.mean(np.abs(logperm2-np.transpose(logperm_true)))
AAD2a=np.mean(np.abs(logperm2a-np.transpose(logperm_true)))
AAD3=np.mean(np.abs(logperm3-np.transpose(logperm_true)))
AAD3a=np.mean(np.abs(logperm3a-np.transpose(logperm_true)))

print AAD1, AAD2, AAD2a, AAD3, AAD3a

y=[AAD1, AAD2, AAD2a, AAD3, AAD3a]
x=[1,3,5,7,10]

plt.plot(x,y)
plt.xlabel('Monitoring duration (years)',fontsize=14) #,fontweight="bold")
##ylablename='M' + str(iloc_plot)+' - Saturation'
#ylablename='M' + str(iloc_plot-1)+' - Saturation'
##ylablename='Inj - Saturation'
plt.ylabel('Average absolute difference',fontsize=14)#,fontweight="bold")
x_ticks = np.arange(0, 11, 2)
plt.xticks(x_ticks)

plt.show()
figname = 'AAD.png'
plt.savefig(figname,bbox_inches='tight')