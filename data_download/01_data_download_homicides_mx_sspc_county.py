#!/usr/bin/env python
# coding: utf-8

# In[ ]:


#####################################################################
#              Download pdf's from SSPC                             #
#                                                                   #
#      Source: http://www.informeseguridad.cns.gob.mx/              #
#                                                                   #
#     Column name: "Homicidios dolosos Fuentes abiertas"            #
#                                                                   #
#           Author: Mariana Fernandez                               #
#         contact: mariana.fernadez@cide.edu                        #
# Date:17/08/2020                                                   #
#####################################################################


# In[1]:


# Libraries

import urllib
from datetime import date, timedelta
import datetime


# In[2]:


def download_pdfs_sspc(dirname):
    """ 
    download_pdfs_sspc. 
  
    Download pdfs required for posterior scraping 
  
    Parameters: 
    dirname (str)     : Directory where the new data sets will be save
      

    """
    ## Ask for start date and end date (Range of dates that you want to download)
    date_entry = input('Enter a date in YYYY-MM-DD format (start date): ')
    year, month, day = map(int, date_entry.split('-'))
    start_date = datetime.date(year, month, day)

    date_entry = input('Enter a date in YYYY-MM-DD format (end date): ')
    year, month, day = map(int, date_entry.split('-'))
    end_date = datetime.date(year, month, day)
    
    
    # Create a range of dates 
    v_dates = []
    
    #start_date = date()
    #end_date = date(end_date)

    #end_date = yesterday
    #end_date = end_date

    delta = timedelta(days=1)
    while start_date <= end_date:
        print(start_date.strftime("%d%m%Y"))
        v_dates.append(start_date.strftime("%d%m%Y"))
        start_date += delta
        
        
    for i in (v_dates):
        url = "http://www.informeseguridad.cns.gob.mx/files/homicidios_" + i + ".pdf"
        path = dirname
        try:
            data = urllib.request.urlretrieve(url, path + i + ".pdf")
            print(url)
        except:
            print("Error, cannot read url")
            print(url)


# In[3]:


#download_pdfs_sspc("/Users/marianafernandez/Documents/PADeCI/homicides-mx-data/data_download/data_source/")
download_pdfs_sspc("/Users/marianafernandez/Documents/PADeCI/homicides-mx-data/Anomalias/")


# In[ ]:




