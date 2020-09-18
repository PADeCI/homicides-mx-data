#!/usr/bin/env python
# coding: utf-8

# In[1]:


####################################################
#          Concat Data Frames                      #
#       Homicidios Dolosos Fuentes Abiertas        #
#                                                  #
# Author: Mariana Fernandez                        #
# Github: marianafdz465                            #
# email: mariana.fernadez@cide.edu                 #
#                                                  #
# Date: 19/08/2020                                 #
####################################################


# In[2]:


### Libraries

import pandas as pd
import glob
from pathlib import Path


# In[74]:


### Explore Directories
#dirname = "./2020/05082020/"
#Anomalias
dirname = "/Users/marianafernandez/Documents/PADeCI/homicides-mx-data/Anomalias/29022020/"
DF_list= list()

for filename in sorted(glob.glob(dirname + '/*.csv')):
    print(filename)
    df7 = pd.read_csv(filename, header = None)
    DF_list.append(df7)
df_list_len = len(DF_list)
df_list_len


# In[75]:


two_tables()


# In[34]:


two_tables()


# In[17]:


two_tables()


# In[ ]:


four_tables()


# In[ ]:


switch(df_list_len)


# In[4]:


################
# THREE TABLES
##############
def three_tables():
    #Allocate Data Frame 1
    df1 = DF_list[0]
    df1
    ### Remove first row ##
    df1.columns = df1.iloc[1]
    df1 = df1[2:]

    df1 = df1.drop(["Fuente"], axis = 1)
    df1
    
    #Rename Columns just if is necessary

    df1 = df1.rename(columns={ 
                          df1.columns[2]: "No de \nMuertos",
                          df1.columns[3]: "Hombre", 
                          df1.columns[4]: "Mujer",
                          df1.columns[5]: "No \nIdentificado",
                          
                          
                         })
    df1
    ###################
# TABLE 2
###################

    df2 = DF_list[1]
    df2
    
    #Rename Columns

    df2.columns = ["Entidad", 
               "Municipio", 
               "No de \nMuertos", 
               "Hombre", 
               "Mujer", 
               "No \nIdentificado",
               "Fuente"]

    df2 = df2.drop(["Fuente"], axis = 1)
    df2
#####################
# TABLE 3
#################

    df3 = DF_list[2]
    df3
#Rename Columns
    df3.columns = ["Entidad", 
               "Municipio", 
               "No de \nMuertos", 
               "Hombre", 
               "Mujer", 
               "No \nIdentificado",
               "Fuente"
              
              
              
               
               ]
    df3 = df3.drop(["Fuente"], axis = 1)
    df3
    
###############
# CONCAT 3 TABLES
##############

    new_df = pd.concat([df1, df2,df3])

#Remove Fuente
#new_df = new_df.drop(["Fuente"], axis=1)
    
    path = Path(dirname).stem
    new_df.to_csv( dirname +  path  +  ".csv",encoding="utf-8", index = False)
    return(new_df)


# In[5]:


################
# TWO TABLES
##############

def two_tables():
    ############
    # TABLE 1  #
    ###########
    
    df1 = DF_list[0]
    #df1
    
    ### Remove first row ##
    df1.columns = df1.iloc[1]
    df1 = df1[2:]

    df1 = df1.drop(["Fuente"], axis = 1)
    df1

    #Rename Columns just if is necessary

    df1 = df1.rename(columns={ 
                          df1.columns[2]: "No de \nMuertos",
                          df1.columns[3]: "Hombre", 
                          df1.columns[4]: "Mujer",
                          df1.columns[5]: "No \nIdentificado",
                          
                          
                         })
    #df1
    
    ############
    # TABLE 2  #
    ###########

    df2 = DF_list[1]
    #df2
    
    #Rename Columns

    df2.columns = ["Entidad", 
               "Municipio", 
               "No de \nMuertos", 
               "Hombre", 
               "Mujer", 
               "No \nIdentificado",
               "Fuente"]

    df2 = df2.drop(["Fuente"], axis = 1)
    #df2
    
   ###################
   # CONCAT 2 TABLES #
   ##################

    new_df = pd.concat([df1, df2])
    path = Path(dirname).stem
    new_df.to_csv( dirname +  path  +  ".csv",encoding="utf-8", index = False)
    return(new_df)


# In[15]:


################
# THREE TABLES
##############

def three_tables():
     ############
    # TABLE 1  #
    ###########
    
    df1 = DF_list[0]
    #df1
    
    ### Remove first row ##
    df1.columns = df1.iloc[1]
    df1 = df1[2:]

    df1 = df1.drop(["Fuente"], axis = 1)
    df1

    #Rename Columns just if is necessary

    df1 = df1.rename(columns={ 
                          df1.columns[2]: "No de \nMuertos",
                          df1.columns[3]: "Hombre", 
                          df1.columns[4]: "Mujer",
                          df1.columns[5]: "No \nIdentificado",
                          
                          
                         })
    #df1
    
    ############
    # TABLE 2  #
    ###########

    df2 = DF_list[1]
    #df2
    
    #Rename Columns

    df2.columns = ["Entidad", 
               "Municipio", 
               "No de \nMuertos", 
               "Hombre", 
               "Mujer", 
               "No \nIdentificado",
               "Fuente"]

    df2 = df2.drop(["Fuente"], axis = 1)
    #df2

#####################
# TABLE 3
#################

    df3 = DF_list[2]
    #df3
    #Rename Columns
    df3.columns = ["Entidad", 
               "Municipio", 
               "No de \nMuertos", 
               "Hombre", 
               "Mujer", 
               "No \nIdentificado",
               "Fuente"           
               
               ]
    df3 = df3.drop(["Fuente"], axis = 1)
    #df3
   ###################
   # CONCAT 2 TABLES #
   ##################

    new_df = pd.concat([df1, df2,df3])
    path = Path(dirname).stem
    new_df.to_csv( dirname +  path  +  ".csv",encoding="utf-8", index = False)
    return(new_df)

    
    


# In[20]:


################
# FOUR TABLES
##############

def four_tables():
     ############
    # TABLE 1  #
    ###########
    
    df1 = DF_list[0]
    #df1
    
    ### Remove first row ##
    df1.columns = df1.iloc[1]
    df1 = df1[2:]

    df1 = df1.drop(["Fuente"], axis = 1)
    df1

    #Rename Columns just if is necessary

    df1 = df1.rename(columns={ 
                          df1.columns[2]: "No de \nMuertos",
                          df1.columns[3]: "Hombre", 
                          df1.columns[4]: "Mujer",
                          df1.columns[5]: "No \nIdentificado",
                          
                          
                         })
    #df1
    
    ############
    # TABLE 2  #
    ###########

    df2 = DF_list[1]
    #df2
    
    #Rename Columns

    df2.columns = ["Entidad", 
               "Municipio", 
               "No de \nMuertos", 
               "Hombre", 
               "Mujer", 
               "No \nIdentificado",
               "Fuente"]

    df2 = df2.drop(["Fuente"], axis = 1)
    #df2
    
    #####################
    # TABLE 3
    #################

    df3 = DF_list[2]
    #df3
    #Rename Columns
    df3.columns = ["Entidad", 
               "Municipio", 
               "No de \nMuertos", 
               "Hombre", 
               "Mujer", 
               "No \nIdentificado",
               "Fuente"           
               
               ]
    df3 = df3.drop(["Fuente"], axis = 1)
    #df3
    
    ############
    # TABLE 4
    ##########

    df4 = DF_list[3]
    #df4
    #Rename Columns
    df4.columns = ["Entidad", 
               "Municipio", 
               "No de \nMuertos", 
               "Hombre", 
               "Mujer", 
               "No \nIdentificado",
               "Fuente"
               
               
               ]
    df4 = df4.drop(["Fuente"], axis = 1)

   ###################
   # CONCAT 2 TABLES #
   ##################

    new_df = pd.concat([df1, df2,df3,df4])
    path = Path(dirname).stem
    new_df.to_csv( dirname +  path  +  ".csv",encoding="utf-8", index = False)
    return(new_df)

    
    


# In[ ]:


def one_table():
    return("Just one table")


# In[ ]:


def switch(case):
   sw = {
      1: one_table(),
      2: two_tables(),
      3: three_tables(),
      4: four_tables(),
   }
   return sw.get(case)


# In[ ]:





# In[ ]:


switch(df_list_len)

