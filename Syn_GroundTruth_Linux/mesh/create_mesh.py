from pylagrit import PyLaGriT,zone_to_zonn
import numpy as np
import os

lg = PyLaGriT(lagrit_exe=os.environ['LAGRIT_EXE'])

# Create locations of nodes
x = np.linspace(-2000,2000,51)
y = np.linspace(-2000,2000,51)
z = np.linspace(-1300.,-1000.,11)

# Create mesh
mtri = lg.gridder(x,y,z,elem_type='tet',connect=True)

# Define point sets of zones
p_res = mtri.pset_geom(mins=(mtri.xmin,mtri.ymin,-1300.),maxs=(mtri.xmax,mtri.ymax,-1000.))
#p_res = mtri.pset_geom(mins=(mtri.xmin,mtri.ymin,-1090.),maxs=(mtri.xmax,mtri.ymax,-1060.))
#p_cap = mtri.pset_geom(mins=(mtri.xmin,mtri.ymin,-1060.1),maxs=(mtri.xmax,mtri.ymax,-1030.))
#p_aqu = mtri.pset_geom(mins=(mtri.xmin,mtri.ymin,-1030.1),maxs=(mtri.xmax,mtri.ymax,0.))

# Set zone property ids
p_res.setatt('imt',11)
#p_cap.setatt('imt',12)
#p_aqu.setatt('imt',13)

# Dump FEHM files
mtri.resetpts_itp()
mtri.dump_fehm('mesh')
zone_to_zonn('mesh_outside.zone')
zone_to_zonn('mesh_material.zone')

# View mesh if desired
mtri.paraview(filename='mesh.inp')

