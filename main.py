#################################################################
###   SPATIAL DATA ASSIMILATION IN GEOLOGIC CO2 SEQUESTRATION ###
#################       Misael M. Morales      ##################
################# Los Alamos National Laboratoy #################
#################          Summer 2023         ##################
#################################################################
from utils import *
sda = spatialDA()
sda.check_torch_gpu()
octave = sda.mrst_startup()

########################## PROCESSING ##########################
perm_true, sat_true = sda.load_perm_sat_true()
perm_ens            = sda.load_perm_ens()
perm_all            = sda.load_perm_all()
sda.plot_perm_sat(perm_true, sat_true)

############################# ROM ##############################


######################## PLOTS & PRINTS ########################


################################################################
############################## END #############################