
#%%
#####
##Install libraries & assign API key
#####

import requests
import pandas as pd
import numpy as np
from dotenv import load_dotenv
import os
from datetime import datetime
from econdatapy import read

#%%

x=read.dataset(id="SARS_TRADE",
               series_key="S.73.TOTAL.X.ZA.TX")
# %%
print(os.getcwd())


