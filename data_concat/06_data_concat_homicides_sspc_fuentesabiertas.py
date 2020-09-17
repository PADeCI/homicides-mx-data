#!/usr/bin/env python
# coding: utf-8

# # Concat all data frames 

# In[1]:


#Libraries
import glob
import pandas as pd


# ## Monthly

# In[3]:


extension = 'csv'
directory = "../data_raw/county/2019_2020/"
all_filenames = [i for i in sorted(glob.glob(directory + '*.{}'.format(extension)))]
print(all_filenames)            


# In[4]:


combined_csv = pd.concat([pd.read_csv(f) for f in all_filenames ])
combined_csv.to_csv( directory +"df_homicides_daily_fuentesabiertas.csv", index=False, encoding='utf-8')

