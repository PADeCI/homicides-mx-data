#!/usr/bin/env python
# coding: utf-8

# In[2]:


################
# Libraries
###############
import pandas as pd
import glob
from pathlib import Path
import re
import numpy as np
import math


# ## Functions

# ### Standardize columns

# In[15]:


def homogenize_columns(dirname, destination):
    """ 
    homogenize_columns. 
  
    Set the same estructure of columns for all the data frames. 
  
    Parameters: 
    dirname (str)     : Directory to extract and change the colum names 
    destination (str) : Directory where the new data sets will save
      

    """
    
    DF_list= list()
    
    for filename in sorted(glob.glob(dirname + '/*.csv')):
        #print(filename)
        df = pd.read_csv(filename)
        
        #Rename all columns 
        #df = df.rename(columns={  "Entidad"          : "Entidad",
        #                          "Municipio"        : "Municipio",
        #                         "No de \nMuertos"  : "Homicidios",
        #                          "No. de \nmuertos" : "Homicidios",
        #                          "Hombre"           : "Hombre",
        #                          "Mujer"            : "Mujer",
        #                          "No \nIdentificado": "No Identificado"})
        
        df = df.rename(columns={ 
                          df.columns[0]: "Entidad",
                          df.columns[1]: "Municipio", 
                          df.columns[2]: "Homicidios",
                          df.columns[3]: "Hombre",
                          df.columns[4]: "Mujer",
                          df.columns[5]: "No Identificado",

                          
                          
                         })
        ##Just for January 2019##
        ##Comment if month != January
        #df["Hombre"] = np.nan
        #df["Mujer"] = np.nan
        #df["No Identificado"] = np.nan


        path = Path(filename).stem
        print(path)

        #print(df)
        df.to_csv( destination  + path + ".csv",encoding="utf-8", index = False)


# In[16]:


homogenize_columns("../Anomalias/","../data_raw/county/2020/February/" )



# ### Remove row "Total"

# In[17]:


def drop_row_totales(dirname):
    """ 
    drop_row_totales. 
  
    Remove all rows with the string "Totales"
  
    Parameters: 
    dirname (str)     : Directory to extract and change the colum names       

    """
    for filename in sorted(glob.glob(dirname + '/*.csv')):
        df = pd.read_csv(filename)
    

        path = Path(filename).stem
        print(path)
        df = df.drop(df[df.Entidad == "Totales"].index)
        
        df.to_csv( dirname  + path + ".csv",encoding="utf-8", index = False)
        
    


# In[18]:


drop_row_totales("../data_raw/county/2020/February/")
#drop_row_totales("../Anomalias/")


# ### Clean digits and characters

# In[19]:


def clean_digits_scha(dirname):
    """
    clean_digits_str.
    
    Clean digits and special characters '()' of column "Entidad"
    
    Parameters: 
    
    dirname (str)     : Directory to extract and change the colum names 
    destination (str) : Directory where the new data sets will save
    
    """
    for filename in sorted(glob.glob(dirname + '/*.csv')):
        df = pd.read_csv(filename)


        path = Path(filename).stem
        print(path)

        #Replace values NaN with the value of last string
        df = df.fillna(method ='pad') 

        df['Entidad']=df['Entidad'].apply(str)
        df["Entidad"] = df["Entidad"].apply(lambda x: re.sub('[()]', '', x))
        df["Entidad"] = df["Entidad"].apply(lambda x: re.sub('["\b\d+\b"]', '', x))
        
        df.to_csv( dirname  + path + ".csv",encoding="utf-8", index = False)


# In[20]:


clean_digits_scha("../data_raw/county/2020/February/")
#clean_digits_scha("../Anomalias/")


# ### Group States and Counties 

# In[21]:


def group_states_counties(dirname):
    """
   group_states_counties
    
    Group by Entidad and Municipio and sum values
    
    Parameters: 
    
    dirname (str)     : Directory to extract and overwrite the new data frames 
    
    """
    for filename in sorted(glob.glob(dirname + '/*.csv')):
        df = pd.read_csv(filename)

        #Just for January
        #df = df.groupby(['Entidad',"Municipio"]).agg('sum')
        #df =df.reset_index()
        
        df[['Homicidios',
             'Hombre',
             'Mujer',
             'No Identificado']] = df[['Homicidios',
                                         'Hombre',
                                         'Mujer',
                                         'No Identificado']].apply(pd.to_numeric,errors = 'coerce')
        
        df = df.groupby(['Entidad',"Municipio"]).sum().reset_index()
        
        path = Path(filename).stem
        print(path)
        
        df.to_csv( dirname  + path + ".csv",encoding="utf-8", index = False)


# In[22]:


group_states_counties("../data_raw/county/2020/February/")
#group_states_counties("../Anomalias/")


# In[ ]:


#Just for January

def complete_columns(dirname):
    for filename in sorted(glob.glob(dirname + '/*.csv')):
        df = pd.read_csv(filename)
        
        df["Hombre"] = np.nan
        df["Mujer"] = np.nan
        df["No identificado"] = np.nan
        
        path = Path(filename).stem
        print(path)
        
        df.to_csv( dirname  + path + ".csv",encoding="utf-8", index = False)
    


# In[ ]:


complete_columns("./data-cleaning/county/data-raw/2019-final-version/January/")


# ## Fuzzy Logic

# In[ ]:



def checker_counties_name(dirname):
    v_correct_states = [  "Aguascalientes",
                          "Baja California",
                          "Baja California Sur",
                          "Campeche",
                          "Coahuila",
                          "Colima",
                          "Chiapas",
                          "Chihuahua",
                          "Ciudad de México",
                          "Durango",
                          "Guanajuato",
                          "Guerrero",
                          "Hidalgo",
                          "Jalisco",
                          "Estado de México",
                          "Michoacán",
                          "Morelos",
                          "Nayarit",
                          "Nuevo León",
                          "Oaxaca",
                          "Puebla",
                          "Querétaro",
                          "Quintana Roo",
                          "San Luis Potosí",
                          "Sinaloa",
                          "Sonora",
                          "Tabasco",
                          "Tamaulipas",
                          "Tlaxcala",
                          "Veracruz",
                          "Yucatán",
                          "Zacatecas"
                       ]
    col_list = ["Entidad", "Municipio"]
    
    for filename in sorted(glob.glob(dirname + '/*.csv')):
        df = pd.read_csv(filename, usecols=col_list)
        print(df)
        


# In[ ]:


checker_counties_name("./data-cleaning/county/data-raw/2019-final-version/January/")


# In[ ]:


### Explore Directories
dirname = "./data-cleaning/county/data-raw/2019-cleanVersion/January/"
directory = "./data-cleaning/county/data-raw/2019-cleanVersion2/"
DF_list= list()

for filename in sorted(glob.glob(dirname + '/*.csv')):
    #print(filename)
    df7 = pd.read_csv(filename)
    
    #Rename all columns
    df7 = df7.rename(columns={"Entidad": "Entidad",
                              "Municipio" : "Municipio",
                              "No de \nMuertos": "Homicidios",
                              "No. de \nmuertos": "Homicidios",
                               "Hombre": "Hombre",
                                 "Mujer": "Mujer",
                             "No \nIdentificado": "No Identificado"})
    #df7['Entidad']=df7['Entidad'].apply(str)
    df7["Entidad"] = df7["Entidad"].apply(lambda x: re.sub('[()]', '', x))
    #df7["Entidad"] = df7["Entidad"].apply(lambda x: re.sub('["\b\d+\b"]', '', x))
    
    #x =df7['Entidad'].isna()
    print(x)
    #df7["Entidad"] = df7.fillna(method ='pad') 
    #path = Path(filename).stem
    #print(path)

    #print(df7)
    #df7.to_csv( directory  + path + ".csv",encoding="utf-8", index = False)
   
    


# In[ ]:





# In[ ]:


df1 = pd.read_csv("/Users/marianafernandez/Documents/PADeCI/homicidios-mx/data-cleaning/county/data-raw/2019-final-version/January/homicidios_01012019.csv")


# In[ ]:


df1


# In[ ]:


df1 = df1.drop(df1[df1.Entidad == "Totales"].index)

df1 = df1.replace("-", 0)
df1


# In[ ]:


import numpy as np
import math


#df1['Entidad']=df1['Entidad'].apply(str)
#df1['Entidad'] = df1['Entidad'].replace(['nan'],'NaN')
df1 = df1.fillna(method ='pad') 

df1
df1['Entidad']=df1['Entidad'].apply(str)
df1["Entidad"] = df1["Entidad"].apply(lambda x: re.sub('[()]', '', x))
df1["Entidad"] = df1["Entidad"].apply(lambda x: re.sub('["\b\d+\b"]', '', x))

#Sum entidad y municipio



df1


# In[ ]:


#df1 = df1.groupby(['Entidad',"Municipio", "Hombre", "Mujer", "No \nIdentificado"]).agg('sum')
#df1 =df1.reset_index()
#df1    

df1[['No de \nMuertos','Hombre','Mujer','No \nIdentificado']]=df1[['No de \nMuertos','Hombre','Mujer','No \nIdentificado']].apply(pd.to_numeric,errors = 'coerce')
df1 = df1.groupby(['Entidad',"Municipio"]).sum().reset_index()
#df1.groupby(["Entidad",'Municipio']).agg('sum')
# "No de \nMuertos", "Hombre", "Mujer", "No \nIdentificado"
df1


# In[ ]:


df1["Hombres"] = np.nan
df1["Mujeres"] = np.nan
df1["No identificado"] = np.nan

df1


# In[ ]:





# In[ ]:





# In[ ]:




