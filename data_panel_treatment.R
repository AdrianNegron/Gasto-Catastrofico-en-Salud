# Librerias ====================================================================
library(haven)
library(expss)
library(tidyverse)
getwd()

# Base de datos 2 ==============================================================
# Caracteristicas de los miembros del hogar 
#       -> Presenta la mayor cantidad de datos 
#       -> También presneta los id relativos para los años 2019 y 2020, ello se observa
#           en variables "numpanh19" y "numpanh20", dichas variables hacen referencia
#           al código del hogar en dichos años

df2 <- read_stata("Data/Data_Panel/743-Modulo1479/enaho01-2016-2020-200-panel.dta")

# Base de datos sumaria ========================================================
# Summaria 
#       -> Es una base de datos segmentada por los hogares
#       -> Se hará uso de la variable "numpanh" la cual presenta los códigos de hogar
#           de todos los años (2016-2020), es así que a través del  un merge con 
#           la base de datos "df2" permitirá obtener solo que hogares fueron encuestados
#           en los años 2019 y 2020

dfs <- read_stata("Data/Data_Panel/743-Modulo1478/sumaria-2016-2020-panelf.dta")


# Filtrando solo los hogares que fueron encuestados en los años 2019 y 2020
df2 = df2 %>% filter(numpanh19 == numpanh20)
# Identificando los hogares encuestados en el año 2019 y 2020 y generando un id para
# ambos casos
df2 = df2 %>% 
    # Generando el id para año 2019 en base a las varaibles conglome, vivienda y
    #   hogar del mismo año
    mutate(id19 = paste(conglome_19,vivienda_19,hogar_19)) %>%
    # Generando el id para año 2020 en base a las varaibles conglome, vivienda y
    #   hogar del mismo año
    mutate(id20 = paste(conglome_20,vivienda_20,hogar_20)) %>%
    # Guardando solo los id creados y el codigos de los hogares para el año 2020,
    #   ello debido que tanto "numpanh19" es igual a "numpanh20"
    select(id20,id19,numpanh20)

# Guardando los valores unicos, pero manteniendo las demás variables
df2 = distinct(df2,id19,.keep_all = TRUE)

# Uniendo sumaria del data panel y las variables creadas del df2
df = merge(dfs,df2,by.x = "numpanh",by.y = "numpanh20")

df[1,c("id19","id20","gashog1d_19","gashog1d_20")]

# Obteniendo los id dentro de vectores
ids <- df %>% select(id19,id20)

# Guardando los id por hoagr para cada año
    # final data
save(ids,file = "Data/Final_Data/ids_panel_data.Rda")

remove(df2,dfs,df)




# Obteniendo el panel de datos (Peru) ==============================================
# Datos del 2019
load("Data/2019/R/data.Rda")
df19 = df

# Datos del 2020
load("Data/2020/R/data.Rda")
df20 = df

# Cargando los id's para data panel 2019-2020
load("Data/Final_Data/ids_panel_data.Rda")

# Eliminando df
remove(df)

# Filtrando la base de datos en función a los id obtenidos
#   2019
df19 <- df19 %>%
    mutate(panel = ifelse(id %in% ids$id19,1,0)) %>%
    mutate(year = 2019)

df20 <- df20 %>%
    mutate(panel = ifelse(id %in% ids$id20,1,0)) %>%
    mutate(year = 2020)

# Juntando las dos bases de datos en una sola
df <- bind_rows(df19,df20)
# Aplicando niveles y label a year y panel
df <- apply_labels(df,
                   panel = "Obs. Longitudinal",
                   year = "Año")
# Guandando la data sin considerar si es panel
save(df,file = "Data/Final_Data/data.Rda")

# Obteniendo solo la base de datos para el análisis panel
df_panel <- df %>%
    filter(panel == 1) %>%
    # Generando un id de hogar para panel, en este caso el id tanto para 2019 como
    # 2020 será el id generado para el año 2020
    mutate(id_panel = id)

for(i in 1:nrow(df_panel)){
    if(df_panel$year[i] == 2019) {
        p = which(ids$id19 == df_panel$id[i])
        df_panel$id_panel[i] = ids$id20[p] 
    } else {
        p = which(ids$id20 == df_panel$id[i])
        df_panel$id_panel[i] = ids$id20[p] 
    }
}


df_panel <- df_panel %>%
    mutate(id_panel = str_replace_all(id_panel," ","")) %>%
    mutate(id_panel = as.numeric(id_panel))
# Guandando la data que de tipo panel
save(df_panel,file = "Data/Final_Data/data_panel.Rda")    

# Guarndadno en formato de Stata
write_dta(df_panel,path = "Data/Final_Data/data_panel.dta")

# Eliminando las variables creadas
remove(df,df_panel,df19,df20,ids,i,p)

# Obteniendo el data panel (Cusco) =============================================
# Datos del 2019
load("Data/2019/R/data_c.Rda")
df_c19 = df_c

# Datos del 2020
load("Data/2020/R/data_c.Rda")
df_c20 = df_c


# Cargando los id's para data panel 2019-2020
load("Data/Final_Data/ids_panel_data.Rda")

# Eliminando df
remove(df_c)

# Filtrando la base de datos en función a los id obtenidos
#   2019
df_c19 <- df_c19 %>%
    mutate(panel = ifelse(id %in% ids$id19,1,0)) %>%
    mutate(year = 2019)

df_c20 <- df_c20 %>%
    mutate(panel = ifelse(id %in% ids$id20,1,0)) %>%
    mutate(year = 2020)

# Juntando las dos bases de datos en una sola
df_c <- bind_rows(df_c19,df_c20)
# Aplicando niveles y label a year y panel
df_c <- apply_labels(df_c,
                     panel = "Obs. Longitudinal",
                     year = "Año")
# Guandando la data sin considerar si es panel
save(df_c,file = "Data/Final_Data/data_cusco.Rda")

# Obteniendo solo la base de datos para el análisis panel
df_cpanel <- df_c %>%
    filter(panel == 1) %>%
    # Generando un id de hogar para panel, en este caso el id tanto para 2019 como
    # 2020 será el id generado para el año 2020
    mutate(id_panel = id)

for(i in 1:nrow(df_cpanel)){
    if(df_cpanel$year[i] == 2019) {
        p = which(ids$id19 == df_cpanel$id[i])
        df_cpanel$id_panel[i] = ids$id20[p] 
    } else {
        p = which(ids$id20 == df_cpanel$id[i])
        df_cpanel$id_panel[i] = ids$id20[p] 
    }
}


df_cpanel <- df_cpanel %>%
    mutate(id_panel = str_replace_all(id_panel," ","")) %>%
    mutate(id_panel = as.numeric(id_panel))
# Guandando la data que de tipo panel
save(df_cpanel,file = "Data/Final_Data/data_cpanel.Rda")    

# Guarndadno en formato de Stata
write_dta(df_cpanel,path = "Data/Final_Data/data_cpanel.dta")

# Eliminando las variables creadas
remove(df_c,df_cpanel,df_c19,df_c20,ids,i,p)
