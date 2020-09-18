#!/usr/bin/env python
# coding: utf-8

# In[1]:


####################################################
#          Extraction of data from SSPC            #
#       Homicidios Dolosos Fuentes Abiertas        #
#                                                  #
# Author: Mariana Fernandez                        #
# Github: marianafdz465                            #
# email: mariana.fernadez@cide.edu                 #
#                                                  #
# Date: 19/08/2020                                 #
####################################################


# In[10]:


#Libraries
import pandas as pd
import camelot
from os.path import isfile, isdir
import os
from pathlib import Path
import glob
import zipfile
from io import StringIO
import logging
from pathlib import Path
from shutil import unpack_archive


# In[11]:


# Read pdf's and extract all tables from pdf's saved as .zip

def extract_tables(dirname):
    """ 
    extract_tables 
  
    Extract tables from pdf's 
  
    Parameters: 
    dirname (str)     : Directory where the new data sets will be save
      
    """
    
    for filename in sorted(glob.glob(dirname + '/*.pdf')):
        #print(filename)
        
        try:
          tables = camelot.read_pdf(filename,flavor = "lattice", pages= "all",line_scale=40)
          #camelot.plot(tables[0], kind='grid').show() Just show the structure of the table
          path = Path(filename).stem
          #Saving in root directory
          destination = "/Users/marianafernandez/Documents/PADeCI/homicides-mx-data/data_raw/county/2020/"  
          tables.export(destination  + path + ".csv", f="csv", compress=True)
          print("Saved")

        except:
            print("Error, cannot read file")


# In[12]:


extract_tables("/Users/marianafernandez/Documents/PADeCI/homicides-mx-data/data_download/data_source/")


# In[11]:


# Unzip al .zip files 

def unzip_files(dirname):
    """ 
    unzip_files 
  
    Parameters: 
    dirname (str)     : Directory where the new data sets will be save
      
    """
    zip_files = Path(dirname).rglob("*.zip")
    while True:
        try:
            path = next(zip_files)
        except StopIteration:
            break #No more files
        except PermissionError:
            logging.exception("Permission error")
        else:
            extract_dir = path.with_name(path.stem)
            unpack_archive(str(path), str(extract_dir), 'zip')


# In[12]:


unzip_files("/Users/marianafernandez/Documents/PADeCI/homicides-mx-data/data_raw/county/2020/")


# In[23]:


#Remove zip directories

def remove_zips(dirname):
    """ 
    remove_zips 
  
    Parameters: 
    dirname (str)     : Directory where the new data sets will be save
      
    """
    for filename in sorted(glob.glob(dirname + '/*.zip')):
            #print(filename)

            try:
              os.remove(filename)
              print("Removed")

            except:
                print("Error, cannot remove file")
    


# In[24]:


remove_zips("/Users/marianafernandez/Documents/PADeCI/homicides-mx-data/data_raw/county/2020/")


# In[8]:


pwd

