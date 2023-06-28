########################################################################################################################
###################################################### IMPORT PACKAGES #################################################
########################################################################################################################
import os
import shutil
from time import time
from tqdm import tqdm

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import torch

from sklearn.preprocessing import MinMaxScaler
from sklearn.model_selection import train_test_split, KFold
from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score
  
class spatialHM:
    def __init__(self):
        self.return_data = False
        self.return_plot = True
        self.save_data   = True
        self.verbose     = True

    def check_torch_gpu(self):
        version, cuda_avail = torch.__version__, torch.cuda.is_available()
        count, name = torch.cuda.device_count(), torch.cuda.get_device_name()
        print('Torch version: {}'.format(version))
        print('Torch build with CUDA? {}'.format(cuda_avail))
        print('# Device(s) available: {}, Name(s): {}'.format(count, name))
        self.device = torch.device('cuda' if cuda_avail else 'cpu')
        return None