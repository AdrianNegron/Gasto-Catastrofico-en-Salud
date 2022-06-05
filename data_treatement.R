# Librerias a usar =============================================================
# Cambiando el directorio de trabajo
getwd()

library(haven)
library(tidyverse)
library(expss)
library(Hmisc)
# Importando funciones de a ser utilizadas
source("Funciones/Funciones.R",encoding = "utf-8") 

# Selecionar el año para tratar los datos
year <- 2020

# Base de Datos Nº01 ===========================================================

# Base de datos: Caracteristicas de la Vivienda y del Hogar 
if(year == 2019) {
    df1 <- read_stata("Data/2019/687-Modulo01/enaho01-2019-100.dta")
} else if (year == 2020) {
    df1 <- read_stata("Data/2020/737-Modulo01/enaho01-2020-100.dta")
}

# Delimitando solo para el Departamento del Cusco
df1 <- df1 %>%
    # Generando el id -> concatenado las varibles cong;ome, vivienda y hogar
    mutate(id = paste(conglome,vivienda, hogar)) %>%
    # Creando la variable dep el cual contendrá el ubigeo del departamento
    mutate(dep = substr(ubigeo,1,2)) %>%
    # filtrando la data solo para obtener lo conserniente al departamento de CUsco 
    # filter(dep == "08")
    # Servicios basicos
        # Agua Potable
    mutate(a_potable = ifelse(p110a1 == 1, 1 , 0) ) %>%
        # sanemaiento
        # https://cdn.www.gob.pe/uploads/document/file/1294024/Ley%20General%20de%20Servicios%20de%20Saneamiento%20Ley%20N%2026338.pdf
        # pag 6
    mutate(saneamiento = ifelse(p111a >= 1 & p111a <= 4, 1, 0))

# Guardando las variables de interes
if (year == 2019) {
    df1 <- df1 %>% 
        select(id, dep, a_potable, saneamiento, factor07)
} else if (year == 2020) {
    df1 <- df1 %>%
        # Hogares que fueron encuestados en 2019
        mutate(panel = ifelse(panel == 1,1,0)) %>%
        select(id, dep, a_potable, saneamiento, factor07, panel)
    # Llenando los missing values con 0
    df1$panel[is.na(df1$panel)] <- 0
}

# Llenando los missing values con 0
df1$saneamiento[is.na(df1$saneamiento)] <- 0
df1$a_potable[is.na(df1$a_potable)] <- 0



# Conviertiendo los valores de dep a valores enteros
df1$dep <- as.integer(df1$dep)

# poniendo labels a las columnas y valoores de las columnas
df1 <- apply_labels(df1,
                    id = "Identificador del Hogar",
                    dep = "Region",
                    dep = c('Amazonas'=1, 'Áncash'=2, 'Apurímac'=3, 'Arequipa'=4, 'Ayacucho'=5, 'Cajamarca'=6, 'Callao'=7, 'Cusco'=8, 'Huancavelica'=9, 'Huánuco'=10, 'Ica'=11, 'Junín'=12, 'La Libertad'=13, 'Lambayeque'=14, 'Lima'=15, 'Loreto'=16, 'Madre de Dios'=17, 'Moquegua'=18, 'Pasco'=19, 'Piura'=20, 'Puno'=21, 'San Martín'=22, 'Tacna'=23, 'Tumbes'=24, 'Ucayali'=25),
                    a_potable = "Agua Potable",
                    a_potable = c("No" = 0, "Si" = 1),
                    saneamiento ="Saneamiento Básico",
                    saneamiento = c("No" = 0, "Si" = 1))

# Convirtiendo en variables caregóricas a_potable y saneamiento
# df1$a_potable <- factor(df1$a_potable)
# df1$saneamiento <- factor(df1$saneamiento)
# df1$dep <- factor(df1$dep)

# Guardando la base de datos
if(year == 2019){
    save(df1,file="Data/2019/R/module1.Rda")
}else if (year == 2020) {
    save(df1,file="Data/2020/R/module1.Rda")
}

remove(df1)


# Base de Datos Nº02 ===========================================================

# Base de datos: Caracteristicas de los Miembros del Hogar 
if(year == 2019) {
    df2 <- read_stata("Data/2019/687-Modulo02/enaho01-2019-200.dta")
} else if (year == 2020) {
    df2 <- read_stata("Data/2020/737-Modulo02/enaho01-2020-200.dta")
}

# Llenando los missing values de edad con 0
df2$p208a[is.na(df2$p208a)] <- 0
df2$p208b[is.na(df2$p208b)] <- 0

# Creando variables para el tratamiento de la data
df2 <- df2 %>%
    # Generando el id -> concatenado las varibles cong;ome, vivienda y hogar
    mutate(id = paste(conglome,vivienda, hogar)) %>%
    # Generando el id2 solo para los data frames df2,df3,df4,df5
    mutate(id2 = paste(conglome,vivienda, hogar, codperso)) %>%
    # Jefe del hogar
    mutate(jefe = ifelse(p203 == 1, 1,0)) %>%
    # Sexo del jefe del hogar
    mutate(s_jefe = ifelse(p207 == 1, 0, 1)) %>%
    # Edad
    mutate(Edad = p208a + (p208b/12)) %>%
    # Niños menores a 5 años
    mutate(E_nino = ifelse(Edad < 6, 1, 0)) %>%
    # Adultos mayores a 65 años
    mutate(E_adulto = ifelse(p208a >= 65, 1, 0)) %>% 
    # Adultos mayores a 65 y niños menores a 5
    mutate(P_peligro = ifelse(E_adulto == 1 | E_nino == 1, 1, 0)) %>%
    # Area de residencia
    mutate(Area = ifelse(estrato > 5, 1, 0))

# Frecuencia de niños menores a 5 años y adultos mayores de 65 años
Frecuency <- aggregate(df2$P_peligro,by=list(Category=df2$id),FUN=sum)
names(Frecuency) <- c("id","N_pvul")

# Obteniendo las variables necesarias
df2 <- df2 %>%
    select(id, id2, jefe, s_jefe, E_nino, E_adulto,Edad,Area,facpob07)

# Juntando los data Frame
df2 <- merge(df2,Frecuency,by="id")

# Aplicando labels a la data frame 
df2 <- apply_labels(df2,
                    id = "Identificador del Hogar",
                    id2 = "Identificador de la persona",
                    jefe = "Jefe del Hogar",
                    jefe = c("No" = 0 ,"Si" = 1),
                    s_jefe = "Sexo del Jefe del Hogar",
                    s_jefe = c("Varon" = 0 ,"Mujer" = 1),
                    E_nino = "Niño menor igual a 5 años",
                    E_nino = c("No" = 0, "Si" = 1),
                    E_adulto = "Adulto mayor igual a 65 años",
                    E_adulto = c("No" = 0, "Si" = 1),
                    Edad = "Edad de la persona",
                    Area = "Area de residencia",
                    Area = c("Urbano" = 0, "Rural" = 1),
                    N_pvul = "Numero de personas vulnerables (<= 5 años & >= 65 años)")

# Convirtiendo en variables caregóricas a_potable y saneamiento
# df2$jefe <- as.factor(df2$jefe)
# df2$s_jefe <- as.factor(df2$s_jefe)
# df2$E_nino <- as.factor(df2$E_nino)
# df2$E_adulto <- as.factor(df2$E_adulto)
# df2$Area <- as.factor(df2$Area)

# Guardando la base de datos
if(year == 2019){
    save(df2,file="Data/2019/R/module2.Rda")
}else if (year == 2020) {
    save(df2,file="Data/2020/R/module2.Rda")
}

remove(df2,Frecuency)


# Base de Datos Nº03 ===========================================================

# Base de datos: Educación 
if(year == 2019) {
    df3 <- read_stata("Data/2019/687-Modulo03/enaho01a-2019-300.dta")
} else if (year == 2020) {
    df3 <- read_stata("Data/2020/737-Modulo03/enaho01a-2020-300.dta")
}

# Reemplazando los valores nulos
    # Nivel Eductivo -> En caso sea na se considerará como sin nivel
df3$p301a[is.na(df3$p301a)] <- 1
    # Años de educación -> en caso sea na se considera como 0 años de educación
df3$p301b[is.na(df3$p301b)] <- 0

# Permite generar numeros aleatorios
set.seed(800)

# Creando variables 
df3 <- df3 %>%
    # id
    mutate(id2 = paste(conglome,vivienda, hogar, codperso)) %>%
    # Nivel de educación
    mutate(educ = ifelse(p301a == 1, 0, ifelse(p301a == 2, 1, ifelse( p301a == 3 | p301a == 4, 2, ifelse(p301a == 5 | p301a == 6, 3, ifelse(p301a == 7 | p301a == 8, 4, ifelse(p301a >= 9 & p301a <= 11, 5, 6))))))) %>%
    mutate(a_educ = case_when(p301a == 1 ~ 0, 
                              p301a == 2 ~ 0, 
                              p301a == 3 ~ p301b,
                              p301a == 4 ~ 6,
                              p301a == 5 ~ 6 + p301b,
                              p301a == 6 ~ 11,
                              # sample.int genera nuemeros enteros aleatorios ente 1 y 3
                              p301a == 7 & p301b >= 3 ~ 11 + sample.int(3,1),
                              p301a == 7 & p301b < 3 ~ 11 + p301b,
                              p301a == 8 ~ 14,
                              p301a == 9 ~ 11  + sample.int(5,1),
                              p301a == 10 ~ 16,
                              p301a == 11 ~ 16 + p301b,
                              p301a == 12 ~ p301b)) %>%
    # Seleciionando las variables de interés
    select(id2,educ,a_educ,factor07)    


# Applicando a etiquetas a las variables
df3 <- apply_labels(df3,
                    id2 = "Identificador de la persona",
                    educ = "Nivel de educación",
                    educ = c("Sin nivel" = 0, "Inicial" = 1, "Primaria" = 2, "Secundaria" = 3, "Técnica" = 4, "Superior" = 5, "Especial" = 6),
                    a_educ = "Años de Educación")

# Convirtiendo en factores las variables
# df3$educ <- as.factor(df3$educ)    

# Guardando la data
if(year == 2019){
    save(df3,file="Data/2019/R/module3.Rda")
}else if (year == 2020) {
    save(df3,file="Data/2020/R/module3.Rda")
}

remove(df3)

# Base de Datos Nº04 ===========================================================

# Base de datos: Salud))
if(year == 2019) {
    df4 <- read_stata("Data/2019/687-Modulo04/enaho01a-2019-400.dta")
} else if (year == 2020) {
    df4 <- read_stata("Data/2020/737-Modulo04/enaho01a-2020-400.dta")
}

df4 <- df4 %>%
    # Generando el id para el hogar
    mutate(id = paste(conglome,vivienda, hogar)) %>%
    # Generando el id para cada persona 
    mutate(id2 = paste(conglome,vivienda, hogar, codperso)) %>%
    # Tipo de Seguro de salud
        # No tiene seguro -> 0
        # SIS/Seguro universitario -> 1
        # ESSALUD -> 2
        # FFAA/ PNP -> 3
        # Seguro privado/EPS/Escolar privado -> 4
        # Otro -> 5
    mutate(T_seg = ifelse(p4191 == 1,2,ifelse(p4192 == 1 | p4193 == 1 | p4197 == 1, 4, ifelse(p4194 == 1, 3, ifelse(p4195 == 1 | p4196 == 1, 1,ifelse(p4198 == 1,5,0)))))) %>%
    # Seguro de salud
    mutate(Seg = ifelse(T_seg >= 1,1,0)) %>%
    # dis -> discapaidad alguna
    mutate(Discap = ifelse(p401h1 == 1 | p401h2 == 1 | p401h3 == 1 | p401h4 == 1 | p401h5 == 1 | p401h6 == 1, 1, 0)) %>%
    # m_cronico -> malestar crónico
    mutate(m_cronic = ifelse(p401 == 1,1,0))

# Guardando solo las variables creadas necesarias
if (year == 2019) {
    df4 <- df4 %>%
        # Covid
        mutate(covid = 0) %>%
        select(id,id2,covid,Seg,T_seg,Discap,m_cronic,factor07)
} else if (year == 2020) {
    df4 <- df4 %>%
        # Covid
        mutate(covid = ifelse(p407j == 2, 1, 0)) %>%
        select(id,id2,covid,Seg,T_seg,Discap,m_cronic,factor07)   
}

# llenando los missing value con 0
df4[is.na(df4)] <- 0

# Numero de personas en el hogar con limitación permanente 
df4$Discap <- as.numeric(df4$Discap)
Frecuency <- aggregate(df4$Discap,by=list(Category=df4$id),FUN=sum)
names(Frecuency) <- c("id","N_discap")

# Uniendo las bases de datos
df4 = merge(df4,Frecuency, by = "id")

# Numero de personas con alguna enfermedad o malestar crónico
Frecuency <- aggregate(df4$m_cronic,by=list(Category=df4$id),FUN=sum)
names(Frecuency) <- c("id","N_mcronic")

# Uniendo las bases de datos
df4 = merge(df4,Frecuency, by = "id")

# Número de personas con algun seguro en el hogar
Frecuency <- aggregate(df4$Seg,by=list(Category=df4$id),FUN=sum)
names(Frecuency) <- c("id","N_seg")

# Uniendo las bases de datos
df4 = merge(df4,Frecuency, by = "id")

# Número de personas que han tenido COVID-19 en el hogar
Frecuency <- aggregate(df4$covid,by=list(Category=df4$id),FUN=sum)
names(Frecuency) <- c("id","N_covid")

# Uniendo las bases de datos
df4 = merge(df4,Frecuency, by = "id")

# Eliminando el id del hogar 
df4$id <- NULL

# Aplicando los labeles
df4 <- apply_labels(df4,
                    id2 = "Identificador de la persona",
                    covid = "Ha tenido COVID-19",
                    covid = c("No" = 0, "Si" = 1),
                    N_covid = "Nº miembros con covid en el hogar",
                    T_seg = "Tipo de Seguro de Salud",
                    T_seg = c("Ninguno" = 0, "SIS/Seguro Universitario" = 1, "ESSALUD" = 2, "FFAA/PNP" = 3, "Seguro Privado" = 4, "Otros" = 5),
                    Seg = "Seguro de salud",
                    Seg = c("No" = 0, "Si" = 1),
                    N_seg = "Nº personas con algun tipo de Seguro de salud",
                    Discap = "Limitación permanente",
                    Discap = c("No" = 0, "Si" = 1),
                    m_cronic = "Nº Peronas en el hogar con alguna enfermedad o malestar crónico",
                    m_cronic = c("No" = 0, "Si" = 1),
                    N_discap = "Nº Personas con limitaciones permanentes",
                    N_mcronic = "Nº Personas con alguna enfermedad o malestrar crónico")

# Convieteindo en factores 
# df4$covid <- as.factor(df4$covid)
# df4$Seg <- as.factor(df4$Seg)
# df4$Discap <- as.factor(df4$Discap)

# Guardando la base de datos
if(year == 2019){
    save(df4,file="Data/2019/R/module4.Rda")
}else if (year == 2020) {
    save(df4,file="Data/2020/R/module4.Rda")
}

# Eliminando las bases creadas
remove(df4,Frecuency)

# Base de Datos Nº05 ===========================================================

# Base de datos: Empleo e ingresos 
if(year == 2019) {
    df5 <- read_stata("Data/2019/687-Modulo05/enaho01a-2019-500.dta")
} else if (year == 2020) {
    df5 <- read_stata("Data/2020/737-Modulo05/enaho01a-2020-500.dta")
}

df5 <- df5 %>%
    # Generando el id del hogar
    mutate(id = paste(conglome,vivienda, hogar)) %>%
    # Generendo el id de la persona
    mutate(id2 = paste(conglome,vivienda, hogar, codperso)) %>%
    # Persona asalariado en terminos monetarios
    # Salario Ocupación principal
        # Se considera como personas asalariados las que hayan respondido en la 
        # pregunta 511 y 537 de enaho cualquiera de los siguiente
            # Sueldo
            # Salario
            # comisión
            # suvención
            # Honorarios profesionales
            # Ingreso (ganancia) por negocio o servicio
            # Ingreso como productor agropecuario
    mutate(s_ocupp = ifelse(p5111 == 1 | p5112 == 1 | p5113 == 1 | p5114 == 1 | p5115 == 1 | p5116 == 1 | p5117 == 1 | p5118 == 1, 1, 0)) %>%
    # Salario Ocupación secundaria
    mutate(s_ocups = ifelse(p5371 == 1 | p5372 == 1 | p5373 == 1 | p5374 == 1 | p5375 == 1 | p5376 == 1 | p5377 == 1 | p5378 == 1, 1, 0)) %>%
    # Perona Asalariada
    mutate(Asalariado = ifelse(s_ocupp == 1 | s_ocups == 1, 1, 0)) %>%
    # Pea ocupado
    # 0 -> No PEA
    # 1 -> PEA Desocupada
    # 2 -> PEA Ocupada
    mutate(ocup = ifelse(ocu500 == 3 | ocu500 == 4 | ocu500 == 0, 0, ifelse(ocu500 == 2, 1, 2)))

# Llenando los missing values en la variable Asalariado
df5$Asalariado[is.na(df5$Asalariado)] <- 0

# Numero de personas asalariadas por hogar
Frecuency <- aggregate(df5$Asalariado, by = list(Category = df5$id), FUN = sum)
names(Frecuency) <- c("id","N_asalariado")

# Manteniendo las variables de interes
if (year == 2019) {
    df5 <- df5 %>%
        select(id,id2,ocup,fac500a)
} else if (year == 2020) {
    df5 <- df5 %>%
        select(id,id2,ocup,fac500a,fac500_p)
}

# Uniendo frecuency con df5
df5 <- merge(df5,Frecuency,by = "id")

# Eliminando el id del hogar
df5$id <- NULL

# Aplicando los label a la data
df5 <- apply_labels(df5,
                    id2 = "Identificador de la persona",
                    ocup = "Jefe del hogar Ocupado",
                    ocup = c("No PEA" = 0, "PEA Descupada" = 1, "PEA Ocupada" = 2),
                    N_asalariado = "Nº personas asalariadas en el hogar")

# Declarando como factor la variable ocup
# df5$ocup <- as.factor(df5$ocup)

# Guandando la Base de datos
if(year == 2019){
    save(df5,file="Data/2019/R/module5.Rda")
}else if (year == 2020) {
    save(df5,file="Data/2020/R/module5.Rda")
}
# Eliminando los data frame
remove(df5,Frecuency)


# Base de Datos Sumaria ========================================================

# Base de datos: sumaria 
if(year == 2019) {
    dfs <- read_stata("Data/2019/687-Modulo34/sumaria-2019.dta")
} else if (year == 2020) {
    dfs <- read_stata("Data/2020/737-Modulo34/sumaria-2020.dta")
}
dfs <- dfs %>%
    # Generando el id -> concatenado las varibles cong;ome, vivienda y hogar
    mutate(id = paste(conglome,vivienda, hogar)) %>%
    
    # # Obteniendo el ubigeo de la persona
    # mutate(dep = substr(ubigeo,1,2)) %>%
    
    # Costo de bolsillo en salud
    mutate(OOP = gru51hd) %>%
    
    # CAPACITY TO PAY
    # De acuerdo a la definición de la OMS en Wagstaff no se debe considerar gastos
    # en alimentos es así que se sutrae
        # g05hd ->  Gastos por alimentos consumidos fuera del hogar – Otros - Pagó
        # g05hd1 -> Gastos por alimentos consumidos fuera del hogar – Otro Hogar - Pagó
        # g05hd2 -> Gastos por alimentos consumidos fuera del hogar – Prepara sus alimentos - Pagó
        # g05hd3 -> Gastos por alimentos consumidos fuera del hogar – Prepara sus alimentos en  el Centro de Trabajo - Pag
        # g05hd4 -> Gastos por alimentos consumidos fuera del hogar – Aula del Instituto - Pagó
        # g05hd5 -> Gastos por alimentos consumidos fuera del hogar – Autosuministro - Pagó
        # g05hd6 -> Gastos por alimentos consumidos fuera del hogar – Campamento - Pagó
        # gru11hd -> Grupo 1: Alimentos - Gasto
        # gru14hd -> Grupo 1: Alimentos preparados consumir en el hogar - Pagado
        # sg23 -> Gastos por alimentos para consumir dentro del hogar obtenidos de instituciones benéficas- pagado
        # sg25 -> Gastos por alimentos consumidos fuera del hogar de instituciones benéficas - pagado
    
        # Gasto Básico
    mutate(basic_exp = g05hd + g05hd1 + g05hd2 + g05hd3 + g05hd4 + g05hd5 + g05hd6 + gru11hd + gru14hd + sg23 + sg25) %>%
    mutate(CTP1 = ifelse(gashog1d - basic_exp > 0, gashog1d - basic_exp, ingmo2hd - basic_exp)) %>%
    # Capacity To Pay 
    # Basado en la segunda definicón de Xu(2003) es igual al gasto del hogar menos
    # la linea de la pobreza 
    mutate(CTP2 = ifelse(gashog1d - linea > 0,gashog1d - linea,ingmo2hd)) %>%
    
    # Gasto Catastrófico en Salud 
        # Considerando como CTP la definición de Gasto Total menos gastos en alimentos
    mutate(GCS1 = ifelse(is.na(OOP/CTP1),0,OOP/CTP1)) %>%
        # Umbral de 20%
    mutate(GCS120 = ifelse(GCS1 >= 0.2,1,0)) %>%
        # Umbral de 30%
    mutate(GCS130 = ifelse(GCS1 >= 0.3,1,0)) %>%
        # Umbral de 40%
    mutate(GCS140 = ifelse(GCS1 >= 0.4,1,0)) %>%
    
    # Gasto Catastrófico en Salud 
        # Considerando como CTP la definición de Gasto Total menos linea de pobreza
    mutate(GCS2 = ifelse(is.na(OOP/CTP2),0,OOP/CTP2)) %>%
        # Umbral de 20%
    mutate(GCS220 = ifelse(GCS2 >= 0.2,1,0)) %>%
        # Umbral de 30%
    mutate(GCS230 = ifelse(GCS2 >= 0.3,1,0)) %>%
        # Umbral de 40%
    mutate(GCS240 = ifelse(GCS2 >= 0.4,1,0)) %>%
    
    # Ingreso de la persona en función del consumo
    mutate(Ing = gashog1d) %>%
    
    # Manteniendo solo las variables de interés
    select(id,CTP1,CTP2,OOP,GCS1,GCS2,GCS120,GCS130,GCS140,GCS220,GCS230,GCS240,Ing,factor07) %>%
    
    # Ordenar de forma ascendente el ingreso
    arrange(Ing)


# Definiendo los quintiles
percentil <- percentile(5,length(dfs$Ing),dfs$Ing)

# Guardnado los intervalos de quintiles de ingresos
if(year == 2019) {
    quintiles_2019 <- data.frame("Quintiles_Ingresos" = c("1º quintil (<=)","2º quintil (<=)",
                                                             "3º quintil (<=)","4º quintil (<=)",
                                                             "5º quintil (>)"),
                                 "Ingresos_2019" = c(percentil,percentil[4]))
    
    save(quintiles_2019, file = "Data/Ingresos_Quintil/quintil_peru_2019.Rda")
}else {
    quintiles_2020 <- data.frame("Quintiles_Ingresos" = c("1º quintil (<=)","2º quintil (<=)",
                                                             "3º quintil (<=)","4º quintil (<=)",
                                                             "5º quintil (>)"),
                                 "Ingresos_2020" = c(percentil,percentil[4]))
    
    save(quintiles_2020, file = "Data/Ingresos_Quintil/quintil_peru_2020.Rda")
}


dfs <- dfs %>%
    mutate(Q_ing = ifelse(Ing <= percentil[1,1], 0,ifelse(Ing > percentil[1,1] & Ing <= percentil[1,2],1,ifelse(Ing > percentil[1,2] & Ing <= percentil[1,3],2,ifelse(Ing > percentil[1,3] & Ing <= percentil[1,4],3,4)))))

# Aplicando label a las variables de la base de datos
dfs <- apply_labels(dfs,
                    id = "Identificador del Hogar",
                    CTP1 = "Capacidad para pagar del hogar (Gasto.T-Alimentos)",
                    CTP2 = "Capacidad para pagar del hogar (Gasto.T-Linea Pobreza)",
                    OOP = "Gasto del Bolsillo del Hogar",
                    GCS1 = "Gasto Catastrófico en Salud def. 1",
                    GCS2 = "Gasto Catastrófico en Salud def. 2",
                    GCS120 = "Gasto catastrófico (def1, U = 20%)",
                    GCS120 = c("Sin GCS" = 0, "Con GCS" = 1),
                    GCS130 = "Gasto catastrófico (def1, U = 30%)",
                    GCS130 = c("Sin GCS" = 0, "Con GCS" = 1),
                    GCS140 = "Gasto catastrófico (def1, U = 40%)",
                    GCS140 = c("Sin GCS" = 0, "Con GCS" = 1),
                    GCS220 = "Gasto catastrófico (def2, U = 20%)",
                    GCS220 = c("Sin GCS" = 0, "Con GCS" = 1),
                    GCS230 = "Gasto catastrófico (def2, U = 30%)",
                    GCS230 = c("Sin GCS" = 0, "Con GCS" = 1),
                    GCS240 = "Gasto catastrófico (def2, U = 40%)",
                    GCS240 = c("Sin GCS" = 0, "Con GCS" = 1),
                    Ing = "Ingreso del hogar",
                    Q_ing = "Quintil del hogar segun Ingreso",
                    Q_ing = c("1º Quintil" = 0, "2º Quintil" = 1, "3º Quintil" = 2, "4º Quintil" = 3, "5º Quintil" = 4))

# Convietiendo las variables como factores
# dfs$GCS120 <- as.factor(dfs$GCS120)
# dfs$GCS130 <- as.factor(dfs$GCS130)
# dfs$GCS140 <- as.factor(dfs$GCS140)
# dfs$GCS220 <- as.factor(dfs$GCS220)
# dfs$GCS230 <- as.factor(dfs$GCS230)
# dfs$GCS240 <- as.factor(dfs$GCS240)
# dfs$Q_ing <- as.factor(dfs$Q_ing)

# Guardando la base de datos
if(year == 2019){
    save(dfs,file="Data/2019/R/module_sumaria.Rda")
}else if (year == 2020) {
    save(dfs,file="Data/2020/R/module_sumaria.Rda")
}

# Eliminando las variables creadas
remove(dfs,percentil)


# (Peru) Juntando todas las bases de datos  ===========================================

# Cargando las bases de datos que presentan información a nivel poblacional
if(year == 2019) {
    load(file = "Data/2019/R/module1.Rda")
    load(file = "Data/2019/R/module2.Rda")
    load(file = "Data/2019/R/module3.Rda")
    load(file = "Data/2019/R/module4.Rda")
    load(file = "Data/2019/R/module5.Rda")
    load(file = "Data/2019/R/module_sumaria.Rda")
} else if (year == 2020) {
    load(file = "Data/2020/R/module1.Rda")
    load(file = "Data/2020/R/module2.Rda")
    load(file = "Data/2020/R/module3.Rda")
    load(file = "Data/2020/R/module4.Rda")
    load(file = "Data/2020/R/module5.Rda")
    load(file = "Data/2020/R/module_sumaria.Rda")
}

# Elimiando los factores de expansión
df1$factor07 <- NULL
df2$facpob07 <- NULL
df3$factor07 <- NULL
df4$factor07 <- NULL
df5$fac500a <- NULL
df5$fac500_p <- NULL
dfs$factor07 <- NULL

# Filtrando por medio del jefe del hogar
df <- df2 %>%
    filter(jefe == 1)

# Uniendo las diferente bases de datos en base a una sola
df <- merge(df, df1, by = "id")
df <- merge(df, dfs, by = "id")
df <- merge(df, df3, by.x = "id2")
df <- merge(df, df4, by.x = "id2")
df <- merge(df, df5, by.x = "id2")

# Realizando algunas modificaciones
df <- df %>%
    # Hogar con al menos una miembro vulnerable (Mayor a 65 años o menor a 5 años)
    mutate(p_vul = ifelse(N_pvul >= 1,1,0)) %>%
    # Hogar con acceso de agua potable y saneamiento
    mutate(Acceso_as = ifelse(saneamiento == 1 & a_potable == 1, 1, 0)) %>%
    # Hogar con algun miembro discapacitado
    mutate(Discap_hog = ifelse(N_discap >= 1, 1, 0)) %>%
    # Hogar con algún miembro con Enfermedad o malestar crónico
    mutate(m_cronichog = ifelse(N_mcronic >=1, 1,0)) %>%
    # Hogar con al menos un miembro que tenga seguro de salud
    mutate(seg_hog = ifelse(N_seg >=1, 1,0)) %>%
    # Hogar con al menos un mimbro que haya tenido covid-19
    mutate(covid_hog = ifelse(N_covid >= 1, 1, 0))


# Aplicando Etiquetas a las variables
df <- apply_labels(df,
                   # DF1
                   id = "Identificador del Hogar",
                   dep = "Region",
                   dep = c('Amazonas'=1, 'Áncash'=2, 'Apurímac'=3, 'Arequipa'=4, 'Ayacucho'=5, 'Cajamarca'=6, 'Callao'=7, 'Cusco'=8, 'Huancavelica'=9, 'Huánuco'=10, 'Ica'=11, 'Junín'=12, 'La Libertad'=13, 'Lambayeque'=14, 'Lima'=15, 'Loreto'=16, 'Madre de Dios'=17, 'Moquegua'=18, 'Pasco'=19, 'Piura'=20, 'Puno'=21, 'San Martín'=22, 'Tacna'=23, 'Tumbes'=24, 'Ucayali'=25),
                   a_potable = "Agua Potable",
                   a_potable = c("No" = 0, "Si" = 1),
                   saneamiento ="Saneamiento Básico",
                   saneamiento = c("No" = 0, "Si" = 1),
                   # DF2
                   id2 = "Identificador del persona",
                   jefe = "Jefe del Hogar",
                   jefe = c("No" = 0 ,"Si" = 1),
                   s_jefe = "Sexo del Jefe del Hogar",
                   s_jefe = c("Varon" = 0 ,"Mujer" = 1),
                   E_nino = "Niño menor igual a 5 años",
                   E_nino = c("No" = 0, "Si" = 1),
                   E_adulto = "Adulto mayor igual a 65 años",
                   E_adulto = c("No" = 0, "Si" = 1),
                   Edad = "Edad de la persona",
                   Area = "Area de residencia",
                   Area = c("Urbano" = 0, "Rural" = 1),
                   N_pvul = "Numero de personas vulnerables (<= 5 años & >= 65 años)",
                   # DF3
                   educ = "Nivel de educación",
                   educ = c("Sin nivel" = 0, "Inicial" = 1, "Primaria" = 2, "Secundaria" = 3, "Técnica" = 4, "Superior" = 5, "Especial" = 6),
                   a_educ = "Años de educación",
                   # DF4
                   covid = "Ha tenido COVID-19",
                   covid = c("No" = 0, "Si" = 1),
                   N_covid = "Nº miembros con covid en el hogar",
                   Seg = "Seguro de salud",
                   Seg = c("No" = 0, "Si" = 1),
                   T_seg = "Tipo de seguro de Salud",
                   T_seg = c("Ninguno" = 0, "SIS/Seguro Universitario" = 1, "ESSALUD" = 2, "FFAA/PNP" = 3, "Seguro Privado" = 4, "Otros" = 5),
                   N_seg = "Nº personas con algun tipo de Seguro de salud",
                   Discap = "Limitación permanente",
                   Discap = c("No" = 0, "Si" = 1),
                   N_discap = "Nº Personas con limitaciones permanentes",
                   m_cronic = "Enfermedad o malestar crónico",
                   m_cronic = c("No" = 0, "Si" = 1),
                   N_mcronic = "Nº Personas con alguna enfermedad o malestar crónico",
                   # DF5
                   ocup = "Jefe del hogar Ocupado",
                   ocup = c("No PEA" = 0, "PEA Descupada" = 1, "PEA Ocupada" = 2),
                   N_asalariado = "Nº personas asalariadas en el hogar",
                   #DFS
                   CTP1 = "Capacidad para pagar del hogar (Gasto.T-Alimentos)",
                   CTP2 = "Capacidad para pagar del hogar (Gasto.T-Linea Pobreza)",
                   OOP = "Gasto del Bolsillo del Hogar",
                   GCS1 = "Gasto Catastrófico en Salud def. 1",
                   GCS2 = "Gasto Catastrófico en Salud def. 2",
                   GCS120 = "Gasto catastrófico (def1, U = 20%)",
                   GCS120 = c("Sin GCS" = 0, "Con GCS" = 1),
                   GCS130 = "Gasto catastrófico (def1, U = 30%)",
                   GCS130 = c("Sin GCS" = 0, "Con GCS" = 1),
                   GCS140 = "Gasto catastrófico (def1, U = 40%)",
                   GCS140 = c("Sin GCS" = 0, "Con GCS" = 1),
                   GCS220 = "Gasto catastrófico (def2, U = 20%)",
                   GCS220 = c("Sin GCS" = 0, "Con GCS" = 1),
                   GCS230 = "Gasto catastrófico (def2, U = 30%)",
                   GCS230 = c("Sin GCS" = 0, "Con GCS" = 1),
                   GCS240 = "Gasto catastrófico (def2, U = 40%)",
                   GCS240 = c("Sin GCS" = 0, "Con GCS" = 1),
                   Ing = "Ingreso del hogar",
                   Q_ing = "Quintil del hogar segun Ingreso",
                   Q_ing = c("1º Quintil" = 0, "2º Quintil" = 1, "3º Quintil" = 2, "4º Quintil" = 3, "5º Quintil" = 4),
                   # Last add
                   p_vul = "Hogar con al menos un persona vulnerable (> 65 años y < 5 años)",
                   p_vul = c("No" = 0, "Si" = 1),
                   Acceso_as = "Hogar con acceso a agua potable y saneamiento básico",
                   Acceso_as = c("No" = 0, "Si" = 1),
                   Discap_hog = "Hogar con al menos un miembro con limitación permanente",
                   Discap_hog = c("No" = 0, "Si" = 1),
                   m_cronichog = "Hogar con al menos un miembro con enferemedado o malestar crónico",
                   m_cronichog = c("No" = 0, "Si" = 1),
                   seg_hog = "Hogar con almenos un miembro que presenta un seguro de Salud",
                   seg_hog = c("No" = 0, "Si" = 1),
                   covid_hog = "Hogar con al menos una miembro que presenntó COVID-19",
                   covid_hog = c("No" = 0, "Si" = 1))



# # Conviertiendo en factors las variables categóricas
#     # DF1
# df$dep <- factor(df$dep,levels = c(1:25),labels = c('Amazonas','Áncash','Apurímac','Arequipa','Ayacucho','Cajamarca','Callao','Cusco','Huancavelica','Huánuco','Ica','Junín','La Libertad','Lambayeque','Lima','Loreto','Madre de Dios','Moquegua','Pasco','Piura','Puno','San Martín','Tacna','Tumbes','Ucayali'))
# df$a_potable <- factor(df$a_potable, levels = c(0,1), labels = c("No","Si"))
# df$saneamiento <- factor(df$saneamiento, levels = c(0,1), labels = c("No","Si"))
#     # DF2
# df$jefe <- factor(df$jefe, levels = c(0,1), labels = c("No","Si"))
# df$s_jefe <- factor(df$s_jefe, levels = c(0,1), labels = c("Varon","Mujer"))
# df$E_nino <- factor(df$E_nino, levels = c(0,1), labels = c("No","Si"))
# df$E_adulto <- factor(df$E_adulto, levels = c(0,1), labels = c("No","Si"))
# df$Area <- factor(df$Area, levels = c(0,1), labels = c("Urbano","Rural"))
#     # DF3
# df$educ <- factor(df$educ, levels = c(0:6), labels = c("Sin nivel", "Inicial", "Primaria", "Secundaria", "Técnica", "Superior", "Especial"))
#     # DF4
# df$covid <- factor(df$covid, levels = c(0,1), labels = c("No","Si"))
# df$T_seg <- factor(df$T_seg, levels = c(0:5), labels = c("Ninguno", "SIS/Seguro Universitario", "ESSALUD", "FFAA/PNP", "Seguro Privado", "Otros"))
# df$Seg <- factor(df$Seg, levels = c(0,1), labels = c("No","Si"))
# df$Discap <- factor(df$Discap, levels = c(0,1), labels = c("No","Si"))
# df$m_cronic <- factor(df$m_cronic, levels = c(0,1), labels = c("No","Si"))
#     # DF5
# df$ocup <- factor(df$ocup, levels = c(0:2), labels = c("No PEA", "PEA Descupada", "PEA Ocupada"))
#     # DFS
# df$GCS120 <- factor(df$GCS120, levels = c(0,1), labels = c("Sin GCS", "Con GCS"))
# df$GCS130 <- factor(df$GCS130, levels = c(0,1), labels = c("Sin GCS", "Con GCS"))
# df$GCS140 <- factor(df$GCS140, levels = c(0,1), labels = c("Sin GCS", "Con GCS"))
# df$GCS220 <- factor(df$GCS220, levels = c(0,1), labels = c("Sin GCS", "Con GCS"))
# df$GCS230 <- factor(df$GCS230, levels = c(0,1), labels = c("Sin GCS", "Con GCS"))
# df$GCS240 <- factor(df$GCS240, levels = c(0,1), labels = c("Sin GCS", "Con GCS"))
# df$Q_ing <- factor(df$Q_ing, levels = c(0:4),labels = c("1º Quintil", "2º Quintil", "3º Quintil", "4º Quintil", "5º Quintil"))
#     # Last add
# df$p_vul <- factor(df$p_vul, levels = c(0,1), labels = c("No","Si"))
# df$Acceso_as <- factor(df$Acceso_as, levels = c(0,1), labels = c("No","Si"))
# df$Discap_hog <- factor(df$Discap_hog, levels = c(0,1), labels = c("No","Si"))
# df$m_cronichog <- factor(df$m_cronichog, levels = c(0,1), labels = c("No","Si"))
# df$seg_hog <- factor(df$seg_hog, levels = c(0,1), labels = c("No","Si"))
# df$covid_hog <- factor(df$covid_hog,levels = c(0,1), labels = c("No","Si"))

# Conviertiendo en factors las variables categóricas
    # DF1
df$dep <- as.factor(df$dep)
df$a_potable <- as.factor(df$a_potable)
df$saneamiento <- as.factor(df$saneamiento)
# DF2
df$jefe <- as.factor(df$jefe)
df$s_jefe <- as.factor(df$s_jefe)
df$E_nino <- as.factor(df$E_nino)
df$E_adulto <- as.factor(df$E_adulto)
df$Area <- as.factor(df$Area)
# DF3
df$educ <- as.factor(df$educ)
# DF4
df$covid <- as.factor(df$covid)
df$T_seg <- as.factor(df$T_seg)
df$Seg <- as.factor(df$Seg)
df$Discap <- as.factor(df$Discap)
df$m_cronic <- as.factor(df$m_cronic)
# DF5
df$ocup <- as.factor(df$ocup)
# DFS
df$GCS120 <- as.factor(df$GCS120)
df$GCS130 <- as.factor(df$GCS130)
df$GCS140 <- as.factor(df$GCS140)
df$GCS220 <- as.factor(df$GCS220)
df$GCS230 <- as.factor(df$GCS230)
df$GCS240 <- as.factor(df$GCS240)
df$Q_ing <- as.factor(df$Q_ing)

# Last add
df$p_vul <- as.factor(df$p_vul)
df$Acceso_as <- as.factor(df$Acceso_as)
df$Discap_hog <- as.factor(df$Discap_hog)
df$m_cronichog <- as.factor(df$m_cronichog)
df$seg_hog <- as.factor(df$seg_hog)
df$covid_hog <- as.factor(df$covid_hog)


#     # Last add
# df$p_vul <- factor(df$p_vul, levels = c(0,1), labels = c("No","Si"))
# df$Acceso_as <- factor(df$Acceso_as, levels = c(0,1), labels = c("No","Si"))
# df$Discap_hog <- factor(df$Discap_hog, levels = c(0,1), labels = c("No","Si"))
# df$m_cronichog <- factor(df$m_cronichog, levels = c(0,1), labels = c("No","Si"))
# df$seg_hog <- factor(df$seg_hog, levels = c(0,1), labels = c("No","Si"))
# df$covid_hog <- factor(df$covid_hog,levels = c(0,1), labels = c("No","Si"))


# Poniendo de nuevo las labels a las variables
df <- apply_labels(df,
                   id = "Identificador del Hogar",
                   dep = "Region",
                   a_potable = "Agua Potable",
                   saneamiento ="Saneamiento Básico",
                   # DF2
                   id2 = "Identificador del persona",
                   jefe = "Jefe del Hogar",
                   s_jefe = "Sexo del Jefe del Hogar",
                   E_nino = "Niño menor igual a 5 años",
                   E_adulto = "Adulto mayor igual a 65 años",
                   Edad = "Edad de la persona",
                   Area = "Area de residencia",
                   N_pvul = "Numero de personas vulnerables (<= 5 años & >= 65 años)",
                   # DF3
                   educ = "Nivel de educación",
                   a_educ = "Años de educación",
                   # DF4
                   covid = "Ha tenido COVID-19",
                   N_covid = "Nº miembros con covid en el hogar",
                   T_seg = "Tipo de Seguro de Salud",
                   Seg = "Seguro de salud",
                   N_seg = "Nº personas con algun tipo de Seguro de salud",
                   Discap = "Limitación permanente",
                   N_discap = "Nº Personas con limitaciones permanentes",
                   m_cronic = "Enfermedad o malestar crónico",
                   N_mcronic = "Nº Personas con alguna enfermedad o malestar crónico",
                   # DF5
                   ocup = "Jefe del hogar Ocupado",
                   N_asalariado = "Nº personas asalariadas en el hogar",
                   #DFS
                   CTP1 = "Capacidad para pagar del hogar (Gasto.T-Alimentos)",
                   CTP2 = "Capacidad para pagar del hogar (Gasto.T-Linea Pobreza)",
                   OOP = "Gasto del Bolsillo del Hogar",
                   GCS1 = "Gasto Catastrófico en Salud def.1",
                   GCS2 = "Gasto Catastrófico en Salud def.2",
                   GCS120 = "Gasto catastrófico (def1, U = 20%)",
                   GCS130 = "Gasto catastrófico (def1, U = 30%)",
                   GCS140 = "Gasto catastrófico (def1, U = 40%)",
                   GCS220 = "Gasto catastrófico (def2, U = 20%)",
                   GCS230 = "Gasto catastrófico (def2, U = 30%)",
                   GCS240 = "Gasto catastrófico (def2, U = 40%)",
                   Ing = "Ingreso del hogar",
                   Q_ing = "Quintil del hogar segun Ingreso",
                   # Last add
                   p_vul = "Hogar con al menos un persona vulnerable (> 65 años y < 5 años)",
                   Acceso_as = "Hogar con acceso a agua potable y saneamiento básico",
                   Discap_hog = "Hogar con al menos un miembro con limitación permanente",
                   m_cronichog = "Hogar con al menos un miembro con enferemedado o malestar crónico",
                   seg_hog = "Hogar con almenos un miembro que presenta un seguro de Salud",
                   covid_hog = "Hogar con al menos una miembro que presenntó COVID-19")


# Guardando el df con todas las variables necesarias
if(year == 2019){
    save(df,file="Data/2019/R/data.Rda")
}else if (year == 2020) {
    save(df,file="Data/2020/R/data.Rda")
}

# Eliminando las bases de datos anteriores
remove(df1,df2,df3,df4,df5,dfs,df)

# (Cusco) Data solo para la región Cusco  ======================================
if(year == 2019) {
    load("Data/2019/R/data.Rda")    
} else if (year == 2020) {
    load("Data/2020/R/data.Rda")
}

df_c <- df %>%
    # Filtrando por ciudad del cusco
    filter(dep == "Cusco") %>%
    # ordenando la data
    arrange(Ing)

quintil = percentile(5,length(df_c$Ing),df_c$Ing)

if(year == 2019) {
    quintiles_2019_c <- data.frame("Quintiles_Ingresos" = c("1º quintil (<=)","2º quintil (<=)",
                                                             "3º quintil (<=)","4º quintil (<=)",
                                                             "5º quintil (>)"),
                                 "Ingresos_2019" = c(quintil,quintil[4]))
    
    save(quintiles_2019_c, file = "Data/Ingresos_Quintil/quintil_cusco_2019.Rda")
}else {
    quintiles_2020_c <- data.frame("Quintiles_Ingresos" = c("1º quintil (<=)","2º quintil (<=)",
                                                             "3º quintil (<=)","4º quintil (<=)",
                                                             "5º quintil (>)"),
                                 "Ingresos_2020" = c(quintil,quintil[4]))
    
    save(quintiles_2020_c, file = "Data/Ingresos_Quintil/quintil_cusco_2020.Rda")
}

# Crendo los quintiles en la data
df_c <- df_c %>%
    mutate(Q_ing = ifelse(Ing <= quintil[1,1], 0,ifelse(Ing > quintil[1,1] & Ing <= quintil[1,2],1,ifelse(Ing > quintil[1,2] & Ing <= quintil[1,3],2,ifelse(Ing > quintil[1,3] & Ing <= quintil[1,4],3,4)))))

df_c <- apply_labels(df_c,Ing = "Ingreso del hogar")
df_c$Q_ing <- factor(df_c$Q_ing, levels = c(0:4),labels = c("1º Quintil", "2º Quintil", "3º Quintil", "4º Quintil", "5º Quintil"))

# Guardando la data
if(year == 2019){
    save(df_c,file="Data/2019/R/data_c.Rda")
}else if (year == 2020) {
    save(df_c,file="Data/2020/R/data_c.Rda")
}

# Eliminando  la info creada
remove(df,df_c,quintil,year)
