#!/usr/bin/env python
# coding: utf-8

# ## Rename State column

# In[2]:


#Libraries
import pandas as pd
import numpy as np
from fuzzywuzzy import fuzz
from fuzzywuzzy import process
import glob
from pathlib import Path


# ### Change Row "Mexico" for "Estado de México"

# In[34]:


def rename_row_mexico(dirname):
    """ 
    rename_row_mexico 
  
    Rename all rows with the string "México"
  
    Parameters: 
    dirname (str)     : Directory to extract and change the colum names       

    """
    for filename in sorted(glob.glob(dirname + '/*.csv')):
        df = pd.read_csv(filename)
    

        path = Path(filename).stem
        print(path)
        #df['Entidad'] = np.where((df.Entidad == 'México '),'Estado de México',df.Entidad)
        df['Entidad'] = np.where((df.Entidad == 'México '),'Estado de México',df.Entidad)
        df.to_csv( dirname  + path + ".csv",encoding="utf-8", index = False)
        


# In[35]:


rename_row_mexico("../data_raw/county/2020/February/")


# ## Check Consistency column "Entidad"

# ### Fuzzy Wuzzy

# In[36]:



#col_list = ["Entidad"]

df_states = pd.read_csv("./dictionary_states.csv")


# In[37]:


def cambios_state(mal, bien):
    for i in mal:
        x = process.extractOne(i, df_states["bien"])
        nombres.append(x[0])
        proporcion.append(x[1])
    return nombres, proporcion


# ## Estado y Directorio

# In[38]:



dirname = "../data_raw/county/2020/February/"
#dirname = "../Anomalias/"


for filename in sorted(glob.glob(dirname + '/*.csv')):
    df = pd.read_csv(filename)

    nombres = []
    proporcion = []

    mal=df["Entidad"].dropna().values
    bien=df_states["bien"].dropna().values

    corregido, rango=cambios_state(mal, bien)
    df["Entidad"]=pd.Series(corregido)
    #df["ratiom"]=pd.Series(rango)

    path = Path(filename).stem
    print(path)
    df.to_csv( dirname +path + ".csv",encoding="utf-8", index = False)


# ### Add time_stamp

# In[40]:


from datetime import datetime

def time_stamp(dirname):
    for filename in sorted(glob.glob(dirname + '/*.csv')):
        df = pd.read_csv(filename)
        path = Path(filename).stem
        print(path)

        preliminar_date = path
        dts = datetime.strptime(preliminar_date, '%d%m%Y')
        dts = dts.strftime('%Y-%m-%d')
        df['Fecha'] = dts

        df.to_csv(dirname  + path + ".csv",encoding="utf-8", index = False)


# In[47]:


time_stamp("../data_raw/county/2020/April/")
#time_stamp("../Anomalias/")


# In[ ]:


mal=df["Entidad"].dropna().values
bien=df_states["bien"].dropna().values

corregido, rango=cambios_state(mal, bien)


# In[ ]:


df["Entidad"]=pd.Series(corregido)
#df["ratiom"]=pd.Series(rango)


# In[ ]:


df


# ### Check Consistency column "Municipio"
# 

# In[42]:


df_counties = pd.read_csv("./dictionary_counties.csv")
#df_counties


# In[43]:


def cambios_counties(mal, bien):
    for i in mal:
        x = process.extractOne(i, df_counties["bien"])
        nombres.append(x[0])
        proporcion.append(x[1])
    return nombres, proporcion


# In[52]:


dirname = "../data_raw/county/2020/August/"                #
                                                            #
############################################################
for filename in sorted(glob.glob(dirname + '/*.csv')):

    df = pd.read_csv(filename)
    nombres = []
    proporcion = []
    mal=df["Municipio"].dropna().values
    bien=df_counties["bien"].dropna().values


    corregido, rango=cambios_counties(mal, bien)
    df["Municipio"]=pd.Series(corregido)
    #df["ratioMunnicpios"]=pd.Series(rango)

    path = Path(filename).stem
    print(path)
    df.to_csv(dirname  + path + ".csv",encoding="utf-8", index = False)


# In[ ]:





# In[ ]:


from datetime import datetime

date_str3 = "31012020"
dts = datetime.strptime(date_str3, '%d%m%Y')
dts = dts.strftime('%Y-%m-%d')
dts


# In[ ]:


df['Fecha'] = dts
df.to_csv("AQUIII" + ".csv",encoding="utf-8", index = False)


# In[ ]:


states = {"Entidad": v_correct_states}
df_states = pd.DataFrame(states, columns = ['Entidad'])


# In[ ]:


df_states.to_csv("dictionary_states.csv",encoding="utf-8", index = False)


# In[ ]:


df_states = pd.read_csv("./dictionary_states.csv")


# In[ ]:


col_list = ["Entidad"]
df = pd.read_csv("../data_raw/county/2020/January/02012020.csv", usecols=col_list)
nombres = []
proporcion = []
df


# In[ ]:


mal=df["Entidad"].dropna().values
bien=df_states["bien"].dropna().values
corregido, rango=cambios(mal, bien)
df["Corregido"]=pd.Series(corregido)
df["ratiom"]=pd.Series(rango)


# In[ ]:


df


# In[ ]:


#nombres = []
#proporcion = []

def cambios(mal, bien):
    for i in mal:
        x = process.extractOne(i, v_correct_states)
        nombres.append(x[0])
        proporcion.append(x[1])
    return nombres, proporcion


# In[ ]:


nombres = []
proporcion = []

def cambios(mal):
    for i in mal:
        x = process.extractOne(i, v_correct_states)
        nombres.append(x[0])
        proporcion.append(x[1])
    return nombres, proporcion


# In[ ]:


def check_state_name(dirname):
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
    col_list = ["Entidad"]
    
    #for filename in sorted(glob.glob(dirname + '/*.csv')):
        df = pd.read_csv(filename, usecols=col_list)
        #df['Entidad'] = np.where(df['Entidad'] == "Michoacán de \nOcampo ", "Michoacán", df['Entidad'])
        #df['Entidad'] = np.where(df['Entidad'] == "Michoacán \nde \nOcampo \n", "Michoacán", df['Entidad'])
        #df['Entidad'] = np.where(df['Entidad'] == " 'Michoacán \nde Ocampo \n'", "Michoacán", df['Entidad'])

        #df['Entidad'] = np.where(df['Entidad'] == "Veracruz de \nIgnacio de la \nLlave ", "Veracruz", df['Entidad'])
        #for col in df:
        #    print(df[col].unique())
        #    print(df)
        


# In[ ]:


mal=df["Entidad"].dropna().values
bien=df["bien"].dropna().values

corregido, rango=cambios(mal)


# In[ ]:


df["corregido"]=pd.Series(corregido)
df["ratiom"]=pd.Series(rango)


# In[ ]:


df["Entidad"]=pd.Series(corregido)
df


# In[ ]:


v_bien= [
"Aguascalientes",
"Baja California",
"Baja California Sur",
"Campeche",
"Coahuila de Zaragoza",
"Colima",
"Chiapas",
"Chihuahua",
"Ciudad de Mexico",
"Durango",
"Guanajuato",
"Guerrero",
"Hidalgo"
]


# In[ ]:


ok= {"Estado de México","Ciudad de México", "Argentina", "Chile"}
usuario="Estado México "
aprox = process.extractOne(usuario, ok)
print("La palabra mas cercana: ", usuario)
print(aprox)


# In[ ]:


df


# In[ ]:


import pandas as pd
import numpy as np

df = pd.DataFrame({'Date' : ['11/8/2011', '11/9/2011', '11/10/2011',
                                        '11/11/2011', '11/12/2011'],
                'Event' : ['Dance', 'Painting', 'Dance', 'Dance', 'Painting']})

df


# In[ ]:


df['Entidad'].mask(df['Entidad'] == 'México ', 'POP', inplace=True)
df


# In[ ]:


df['Entidad'] = np.where((df.Entidad == 'México '),'Estado de México',df.Entidad)
df


# In[ ]:


df_counties = pd.read_csv("../data_cleaning/dictionary_counties.csv")
df_counties


# In[ ]:


def correct_road(roadname):
    if roadname in correct_roadnames:  # might want to make this a dict for O(1) lookups
        return roadname, 100

    new_name, score = process.extractOne(roadname, correct_roadnames)
    if score < 90:
        return roadname, score
    else:
        return new_name, score


# In[ ]:


df = pd.read_csv("../data_raw/county/2019/January/")
df


# In[ ]:


df2 = pd.read_csv("/Users/marianafernandez/Documents/PADeCI/homicides-mx-data/data_raw/county/2019/January/04012019.csv")


# In[27]:


dirname = "../data_raw/county/2020/February/"
col_list = ["Entidad"]

for filename in sorted(glob.glob(dirname + '/*.csv')):
    df = pd.read_csv(filename, usecols=col_list)
    print(df["Entidad"].unique())

