import matplotlib.pyplot as plt
import numpy as np
import math


nx=51
ny=51

Nens=1

data_raw=np.zeros((2601,100))

AA=np.zeros(nx*ny)
BB=np.zeros(nx*ny)
logperm=np.zeros((nx, ny))

#filename='PERMX_'+ str(Nens) + '.inc'
filename='param_1_0.txt'

for iensemble in range(0,10):

    count = 0    
    for lines in open(filename):
        
        lines = lines.rstrip('\n')
        #lines = lines.split(None,1)
        #times, features = lines
        count_columns = 0
    
        for e in lines.split():
            val = e
            xi = float(val)
            data_raw[count][count_columns]= xi
            count_columns += 1
        
        count += 1
    
    #print filename
        
    AA=data_raw[:,iensemble]
    
    for i in range(0,nx*ny):    
        #BB[i]=math.log(AA[i]/9.869233e-16)
        BB[i]=math.log(AA[i])
    
    
    #BB[8230:8281]=0
    #BB[8139:8190]=0
    #BB[8049:8099]=0
    #BB[7958:8008]=0
    #BB[7868:7917]=0
    #BB[7778:7826]=0
    #BB[7687:7735]=0
    #BB[7597:7644]=0
    #BB[7507:7553]=0
    #BB[7416:7462]=0
    #BB[7326:7371]=0
    #BB[7236:7280]=0
    #BB[7146:7189]=0
    #BB[7056:7098]=0
    #BB[6967:7007]=0
    #BB[6877:6916]=0
    #BB[6788:6825]=0
    #BB[6698:6734]=0
    #BB[6608:6643]=0
    #BB[6519:6552]=0
    #BB[6429:6461]=0
    #BB[6340:6370]=0
    #BB[6250:6279]=0
    #BB[6161:6188]=0
    #BB[6071:6097]=0
    #BB[5981:6006]=0
    #BB[5892:5915]=0
    #BB[5802:5824]=0
    #BB[5713:5733]=0
    #BB[5623:5642]=0
    #BB[5533:5551]=0
    #BB[5444:5460]=0
    #BB[5354:5369]=0
    #BB[5265:5278]=0
    #BB[5175:5187]=0
    #BB[5085:5096]=0
    #BB[4996:5005]=0
    #BB[4906:4914]=0
    #BB[4817:4823]=0
    #BB[4727:4732]=0
    #BB[4637:4641]=0
    #BB[4548:4550]=0
    
        
    for i in range(0,nx):
        for j in range(0,ny):
            logperm[i,j]=BB[j+i*nx]
    
    
    #plt.imshow(logperm, cmap='jet', interpolation='gaussian')
    
    
    fig,ax=plt.subplots()
    
    plt.imshow(logperm, cmap='jet', interpolation='gaussian',origin='lower')
    
    
    
    ##circle1=plt.Circle((16,16), 0.5, color='black')
    ##plt.text(14.5,14.5,'M1', fontsize=12)
    #circle2=plt.Circle((58,54), 0.5, color='black')
    #plt.text(58,54,'M5', fontsize=12)
    ##plt.text(24.5,14.5,'M2', fontsize=12)
    ##circle3=plt.Circle((36,16), 0.5, color='black')
    ##plt.text(34.5,14.5,'M3', fontsize=12)
    #circle4=plt.Circle((37,53), 0.5, color='black')
    #plt.text(37,53,'M4', fontsize=12)
    ##plt.text(14.5,24.5,'M4', fontsize=12)
    #circle5=plt.Circle((41,44), 0.5, color='red')
    #plt.text(41,44,'M3', fontsize=12, color='red')
    #circle6=plt.Circle((55,24), 0.5, color='black')
    #plt.text(55,24,'M1', fontsize=12)
    ##plt.text(34.5,24.5,'M6', fontsize=12)
    ##circle7=plt.Circle((16,36), 0.5, color='black')
    ##plt.text(14.5,34.5,'M7', fontsize=12)
    #circle8=plt.Circle((31,32), 0.5, color='black')
    #plt.text(31,32,'M2', fontsize=12)
    ##plt.text(24.5,34.5,'M8', fontsize=12)
    ##circle9=plt.Circle((36,36), 0.5, color='black')
    ##plt.text(34.5,34.5,'M9', fontsize=12)
    #
    #circle9=plt.Circle((41,34), 0.5, color='blue',fill=False)
    #plt.text(41,34,'N1', fontsize=12, color='blue')
    #circle10=plt.Circle((25,45), 0.5, color='blue',fill=False)
    #plt.text(25,45,'N2', fontsize=12, color='blue')
    #circle11=plt.Circle((48,57), 0.5, color='blue',fill=False)
    #plt.text(48,57,'N3', fontsize=12, color='blue')
    #
    ###ax.add_artist(circle1)
    #ax.add_artist(circle2)
    ###ax.add_artist(circle3)
    #ax.add_artist(circle4)
    #ax.add_artist(circle5)
    #ax.add_artist(circle6)
    ###ax.add_artist(circle7)
    #ax.add_artist(circle8)
    #ax.add_artist(circle9)
    #ax.add_artist(circle10)
    #ax.add_artist(circle11)
    
    
    plt.colorbar()
    plt.gca().invert_yaxis()
    plt.clim(3,6.5)
    plt.show()
    #savename='PERMX_updated_'+ str(iensemble+1)
    savename='PERMX_prior_'+ str(iensemble+1)
    
    plt.savefig(savename,bbox_inches='tight')