# Función para obtener los percentiles =========================================

#' Obtine percentiles, deciles, quintiles y quariles
#' 
#' percentile() permite obtener percentiles, deciles, quintiles y deciles acorde
#' a lo que se expecificque en p
#'
#' @param p número entero que hace referencia a los percentiles, es asi que puede
#'  tomar valores: 100 -> percentil, 10 <- decil, quintil <- 5 y quartil <- 4 
#' @param N número entero que hace referencia al número de observaciones que contiene
#'  la data. 
#' @param data vector u objeto ordenado sobre el cual se obtendrá las el percentile.
#'
#' @return retorna un vector con labels que contine los percentiles especificados 
#' @export
#'
#' @examples
percentile <- function(p,N,data) {
    # P -> hace refencia si es:
    #       * percentil (Toma el número de 100)
    #       * decil (Toma el Numero de 10)
    #       * quintil (Toma el Numero de 5)
    #       * quartil (Toma el Numero de 4)
    # N -> es el numero de datos
    # data -> es la data en base a la cual se designaran el percentil, decil, quintil o cuartil
    
    k = 100/p
    percentiles <- seq(k,100,by = k)
    
    # Eliminando el ultimo dato
    percentiles <- percentiles[-length(percentiles)]
    
    N <- N+1
    
    # Almacenando los percentiles
    n_percentil <- c()
    n_percentiles <- c()
    
    for (i in percentiles) {
        # Opteniendo el iesimo numero de la data
        x_i = (N*i)/100
        
        if(x_i - floor(x_i) != 0){
            # Obteniendo la parte decimal
            dec <- x_i - floor(x_i)
            
            # Posición del percentile, decil, quintil o cuartil
            p_i <- data[x_i] + dec * (data[x_i+1] - data[x_i])
            
            n_percentil <- append(n_percentil,paste0(i,"%"))
            
            # Allmacenando el valor del percentile, decil, quintil o cuartil
            n_percentiles <- append(n_percentiles,p_i)
        } else {
            # Posición del percentile, decil, quintil o cuartil
            p_i <- data[x_i]
            
            n_percentil <- append(n_percentil,paste0(i,"%"))
            # Almacenando el valor del percentile, decil, quintil o cuartil
            n_percentiles <- append(n_percentiles,p_i)
        }
        
    }
    # Conviertiendo en data frame el vector que contine los percentiles y el data frame
    # a su vez transponiendolo
    n_percentiles <- t(as.data.frame(n_percentiles))
    # Insertando en las columnas los percentiles
    names(n_percentiles) <- n_percentil
    return(n_percentiles)
}

# Función para Contar los valores de un vector =================================

#' Contar valores repetidos
#' 
#' count_values_vector() cuenta los valores repetidos de un vector sin ordenar los
#' mismos
#'
#' @param vector es el vector cuyos valores se busca contar. 
#'
#' @return retorna un vector con labels el cual contiene los valores repetidos y 
#' su cuenta respectiva
#' @export
#'
#' @examples
count_values_vector <- function(vector){
    keys <- c()
    values <- c()
    for(i in 1:length(vector)) {
        if(vector[i] %in% keys){
            # Guardar el index en ua variable
            index <- match(vector[i],keys)
            # Sumando la coincidencia
            values[index] = values[index] + 1 
            
        }else {
            keys <- append(keys,vector[i])
            values <- append(values,1)
        }
        
    }
    
    names(values) <- keys
    return(values)
}

# función para modificar tablas ================================================
    # Uso de librerias requeridas:
    #   -> expss
    #   -> tidyverse -> stringr

#' Modifica tablas
#' 
#' modify_table() modifica tablas obtenidas por medio de la librería "expss", para
#' luego por medio de la función create_table() se peudan tener un formato adecuado
#' para exportarlo a word o imprimirlo como imagen
#'
#' @param tab objeto de tipo data frame que contine la tabla generada por "expss"
#'
#' @return retorna una lista con la tabla modificada y un vector que hace referencia
#' a los headers de las columnas con el número de columnas que abarca da una
#' @export
#'
#' @examples
modify_table <- function(tab) {
    # Separando las columnas de string que tengan |
    tab = split_columns(tab)
    cols = colnames(tab)
    
    del_col = c()
    names_above = c()
    for(i in 1:length(cols)){
        # Verificando si presenta dos nombres la columna
        # SI en caso no tiene ninguno quiere decir que las categorias se encuentran
        # en las filas por lo que el nombre de la columna por ejemplo 2 será
        # el primer elemento de la columna 1
        
        if (is.na(str_split(cols[i],"[|]")[[1]][2])) {
            col_name = str_split(cols[i],"[|]")[[1]]
            # En caso de que sea un índice con las observaciones de una variable 
            if(col_name[1] == "" && i%%2 == 1) {
                # Obteniendo los columnas que iran arriba de las columnas de la tabla
                names_above[i] <- " "
                # Poniendo a la siguiente columna la 
                colnames(tab)[i+1] = tab[1,i]
                # COlumnas a eliminar 
                del_col <- append(del_col,-i)
                # En caso de totales en las filas moviendo los la fila total a la siguiente columna
                if(tab[nrow(tab),i] == "#Total"){
                    tab[nrow(tab),i+1] = tab[nrow(tab),i]
                }
                # En caso de una columna que contenga total
            }else {
                if(col_name[1] == "") {
                    # Obteniendo los columnas que iran arriba de las columnas de la tabla
                    names_above[i] <- " "
                }else{
                    # Obteniendo los columnas que iran arriba de las columnas de la tabla
                    names_above[i] <- col_name[1]
                    # Renombrando la columna
                    colnames(tab)[i] = " "
                }
            }
            
            # Si la columna presenta dos nombres se separará el mismo y se creará
            # asignará al segundo nombre como nombre de la columna en la tabla,
            # pero el primer nombre se guardará en un vector tabla para luego 
            # especificarlo al momento de imprimir la tabla
        } else {
            # Separando texto por "|" 
            col_name = str_split(cols[i],"[|]")[[1]]
            # Obteniendo los columnas que iran arriba de las columnas de la tabla
            names_above[i] <- col_name[1]
            # Renombrando la columna
            colnames(tab)[i] = col_name[2]
        }
        
    }
    # Eliminando algunas columnas
    tab <- tab[del_col]
    
    # Contando los nombres repetidos de las columnas
    names_above = count_values_vector(names_above)
    
    # Modificando el valor de la tabla
    names_above[" "] = names_above[" "] - length(del_col)
    return(list(tab,names_above))
}

# Función para separar las filas de las tablas =================================
    # Uso de librerias requeridas: 
    #   -> tidyverse -> stringr

#' Separa las rownames de un dataframe
#'
#'split_rowsnames() función que permite tratar el row index de una tabla generada 
#'por medio de la librería "expss", seprando así el row index en dos nuevas columnas
#'una que hacer referencia a la variable y otra a la categoría
#'
#' @param df objeto de tipo data frame que contiene la tabla creada por la librería
#' "expss"
#'
#' @return retorna el data frame insertado pero modificado con dos columnas adicionales
#' que son "variable" que hace refencia al a variable y "category" que hace referencias
#' a las categórias dentro de la variable explicitada
#' @export
#'
#' @examples
split_rowsnames <- function(df) {
    # Creando una nueva columna con los row names
    df["x"] <- row.names(df)
    # Generando unuevos indices para el df
    row.names(df) <- c(1:length(df$value))
    # Separando los valores de la col "X" en las columnas "Variable" y "ID"
    df["variable"] <- str_split_fixed(df$x,"[|]",2)[,1]
    df["category"] <- str_split_fixed(df$x,"[|]",2)[,2]
    # Eliminando la columna x
    df["x"] <- NULL 
    
    return(df)
}

# Función par realizar Tablas ==================================================
    # Uso de librerias requeridas:
    #   -> officer
    #   -> officedown
    #   -> knitr
    #   -> expss
    #   -> purrr
    #   -> Flextable

#' Crear tablas con expss y flextable
#'
#'create_table() crea tablas de contingencia con ayuda de las librerias expss y 
#'flextable y a su ves le da una determinado formato para que esta se pueda visualizar
#'de mejor manera en word si es que se renderiza en markdown 
#'
#' @param data objeto de tipo data frame
#' @param var_cell string que hace referencia al nombre de la varible dentro del 
#' data frame que se ubicará en el row index de la tabla
#' @param var_col string que hace referencia al nombre de la varible dentro del 
#' data frame que se ubicará en las columna de la tabla
#' @param title String que hace referencia al título de la tabla y que se mostrará
#' si se renderiza en un archivo word por medio de markdown
#' @param r_markdown de tipo lógico, cuando tomael valor de TRUE hace refencia que 
#' se renderizará en marksdown y por tanto que agregará ciertas propiedades a la
#' tabla 
#' @param freq indica el tipo de frecuencia que la tabla considerará los valores
#' aceptados son "absolute" (por defecto) y relative (valores en porcentajes)
#'
#' @return retorna un objeto de tipo flextable (tabla de contiengenica foramteada)
#' @export
#'
#' @examples
create_table <- function(data,var_cell, var_col,var_row = NULL, title,r_markdown = FALSE, freq = "absolute") {
    cells <- data[paste(var_cell)]
    cols <- data[paste(var_col)]
    
    if(!is.null(var_row)){
        rows <- data[paste(var_row)]
    }
    
    # Tabla
    # Titulo de la tabla
    if(r_markdown == TRUE) {
        t_table <- run_autonum(seq_id = "Table",pre_label = "Tabla ")
            # knit_print_block -> premite crear una bloque dentro de word para 
            #   insertar algo en el mismo
            # block_caption -> premite insertar un titulo de tabla o gráfico
        knit_print_block(block_caption(label = title,style = "Table Caption", autonum = t_table))
    }
    
    #Creación de la tabla
    # table <- cro(cell_vars = data[paste0(var_cell)],
    #              col_vars = data[paste0(var_col)] ,
    #              total_statistic = c("u_cpct","u_rpct","u_tpct"))
    
    if(freq == "absolute") {
        if (is.null(var_row)) {
            table <- cro_cases(cell_vars = cells,
                               col_vars = list(cols,total()),
                               total_statistic = "u_cases",
                               total_label = "% Total")   
        } else {
            table <- cro_cases(cell_vars = cells,
                               col_vars = list(cols,total()),
                               row_vars = rows,
                               total_statistic = "u_cases",
                               total_label = "Total")
        }
    } else if (freq == "relative") {
        if (is.null(var_row)) {
            table <- cro_tpct(cell_vars = cells,
                              col_vars = list(cols,total()),
                              total_statistic = "u_tpct",
                              total_label = "% Total")   
        } else {
            table <- cro_tpct(cell_vars = cells,
                              col_vars = list(cols,total()),
                              row_vars = rows,
                              total_statistic = "u_tpct",
                              total_label = "Total")
        }
    }
    
    # Modificando la tabla para presentarlo en word
    table <- modify_table(table)

    # Redondeando los valores de table [[1]] si sus valores son numéricos 
    table[[1]] = purrr::modify_if(table[[1]], ~is.numeric(.), ~round(.,2))
    
    # Reemplazano los valores nulos por -
    table[[1]][is.na(table[[1]])] = "-"
    
    # Renderizado de la tabla
    table = table[[1]] %>%
        # Creando la tabla en flextable
        flextable() %>%
        # Agregando columnas en el header
        add_header_row(colwidths = as.numeric(table[[2]]),values = names(table[[2]])) %>%
        # Agregando el título de la tabla
        # set_caption(caption = "Title") %>%
        theme_booktabs(bold_header = TRUE) %>%
        # Añadiendo un nota debajo de la tabla
        add_footer_lines(values = "Fuente: ENAHO (2020). Elaboración propia.") %>%
        # autofit() %>%
        align(align = "center",part = "header")

    # Numero de filas del body
    n_rows = nrow(table[["body"]]$dataset)
    # Numero de columnas del body
    n_cols = ncol(table[["body"]]$dataset)
    
    # Obtneiendo la ubicación en donde hay el valor de "#Total" en el body
    positions_b = which(table[["body"]]$dataset == "#Total", arr.ind = TRUE)
    
    # Modificando la tabla cuando presenta filas con el valor  "#Total"
    for(l in 1:nrow(positions_b)) {
        if(!is_empty(positions_b)){
            # Poniendo en negrita la fila que presenta el valor de "#Total"
            table = bold(table,i = positions_b[l,"row"])
            # Insertando bordes
            table = border(table,
                           i = positions_b[l,"row"],
                           border.bottom = fp_border(width = 2),
                           part = "body")
            table = border(table,
                           i = positions_b[l,"row"],
                           j = positions_b[l,"col"]:n_cols,
                           border.top = fp_border(width = 2),
                           part = "body")
        }    
    }
    
    # Obtneiendo la ubicación en donde hay el valor de "#Total" en el header
    positions_h = which(table[["header"]]$dataset == "#Total", arr.ind = TRUE)
    for (l in 1:nrow(positions_h)) {
        if(!is_empty(positions_h)){
            # Poniendo en negrita la fila que presenta el valor de "#Total"
            table = bold(table,j = positions_h[l,"col"])
            # Insertando bordes
            table = border(table,
                           i = positions_h[l,"row"],
                           j = positions_h[l,"col"],
                           border.bottom = fp_border(color = "white",width = 1),
                           part = "header")
        }
    }
    return(table)
}

# Función par realizar gráficos ================================================
    # Uso de librerias requeridas:
    #   -> officer
    #   -> tidyverse (dplyr, ggplot, etc.)
    #   -> ggthemes
    #   -> officedown
    #   -> knitr
    #   -> expss
    #   -> purrr
    #   -> Flextable
    #   -> rlang

#' crear gráfico de barras
#'
#'create_graph() función que permite crear graficos de barras por medio de ggplot 
#'en base a variables categóricas y que le dá un determinado formato cuando se 
#'renderiza por markdown a word
#'
#' @param data objeto de tipo data frame
#' @param var_cell string que hace referencia al nombre de la varible dentro del 
#' data frame, y el conteo de las variables caterógicas se utilizaran en el eje "y"
#' #' @param var_col string que hace referencia al nombre de la varible dentro del 
#' data frame, que se encontrará en el eje "X" del gráfico, cabe meniconar que también
#' es una varible categórica 
#' @param title String que hace referencia al título de gráfico y que se mostrará
#' si se renderiza en un archivo word por medio de markdown
#' @param y_lab string que hace refencia al label del eje y
#' @param x_lab string que hace refencia al label del eje x
#' @param r_markdown de tipo lógico, cuando tomael valor de TRUE hace refencia que 
#' se renderizará en marksdown y por tanto que agregará ciertas propiedades a la
#' tabla 
#'
#' @return retorna un gráfico de barras
#' @export
#'
#' @examples
create_graph <- function(data,var_cell, var_col, var_row, title, y_lab, x_lab, r_markdown = FALSE) {
    
    # Verificando si se va ha renderizar en r markdown o no
    if(r_markdown == TRUE) {
        # Salto de linea
        cat("\n  <br>  \n")
        
        # Gráfico
        # Insertando el el titulo de la figura
        t_graph <- run_autonum(seq_id = "Figure",pre_label = "Figura ")
        knit_print_block(block_caption(label = title,style = "Image Caption", autonum = t_graph))
    } 
    
    # Creación del gráfico
    # Cambiando el formato para que lo pueda leer tydiverse
    var_col <- sym(var_col)
    var_cell <- sym(var_cell)
    var_row <- sym(var_row)
    
    # Número de 
    n_2019 <- data %>% filter(year == "2019") %>% nrow()
    n_2020 <- data %>% filter(year == "2020") %>% nrow()
    
    graph <- data %>%
        # reagrupando la data
        # !! -> para que tydiver pueda entender un symbolo como una variable del data frame
        group_by(!!var_col,!!var_cell,!!var_row) %>%
        # Obteneindo un
        count(!!var_cell,!!var_row,name = "value") %>%
        # Percentage
        mutate(value = ifelse(year == 2020,round(value/n_2020,3),round(value/n_2019,3))) %>%
        # Realizando el gráfico
        ggplot(aes(x = !!var_col, y = value*100)) +
        geom_bar(aes(fill= !!var_cell),stat='identity', position='dodge',width = 0.6) +
        # scales::percent -> permite que los valores se puestran como porcentajes
        geom_text(aes(label = scales::percent(value,accuracy = 0.1), y = value*100 + 7),size = 3.5,
                  position = position_dodge2(width = 0.6),angle=90,vjust = 0.4,hjust = 0.75) +
        # ylim(0,value+500) +
        # Cambiando el titulo, subtitulo y labels de los ejes
        labs(x = NULL,
             y = "Porcentaje\n") +
        # Eliminando el titulo de la legenda
        guides(fill=guide_legend(title=NULL)) +
        # separando los gráficos por año
        facet_wrap(~year) +
        # Agregando el thema
        theme_economist() +
        scale_colour_economist()
    
    print(graph)
    
    # Verificando si se va ha renderizar en r markdown o no
    if(r_markdown == TRUE) {
        # Agregando un salto de línea entre el gráfico y la fuente
        cat("\n  <br>  \n")
        
        # Footnote de la imagen
            # knit_print_run -> premite que se imprima dentro del renderizado de markdown
            # ftext -> permite insertar texto en word
        knit_print_run(ftext("Fuente: ENAHO (2020). Elaboración propia."))
        
        # Agregando una salto de linea entre el gráfico y el párrafo que viene
        cat("\n  <br>  \n")
    }
}


# Función que obtiene las estadisticas del ANOVA ===============================
    # Uso de librerias requeridas:
    #   -> car
    #   -> expss
    #   -> tidyversse (dplyr)
    #   -> rlang

#' Realización de modelos anova
#'
#' @param data objeto de tipo data frame
#' @param v_dep string que hace referencia a la variable de tipo continua dentro 
#' del data frame que será asumida como variable dependiente
#' @param v_indep string que hace referencia a la variable de tipo factor dentro
#' del data frame que será asumida como variable independiente dependiente
#' 
#' @return retorna el p-value del análisis anova como también del test de levene 
#' (que hace referencia al analisis de homogeneidad en los grupos) en media como
#' en mediana, así como un gráfico de los residuos del modelo anova 
#' @export
#'
#' @examples
anova_statistics <- function(data,v_dep,v_indep) {
    # Convietiendo en símbolo el texto insertado 
    v_indep = sym(v_indep)
    v_dep = sym(v_dep)
    
    # Balancenado la data
    data <- data %>%
        group_by(!!v_indep) %>%
        slice(tail(row_number(),min(table(data[paste(v_indep)]))))
    
    # COnvietiendo como data frame la data agrupada y balanceada
    data <- as.data.frame(data)
    
    # Análisis de anova
    anova <- aov(data[,paste0(v_dep)]~data[,paste0(v_indep)])
    anova$residuals = unlab(anova$residuals) 
    
    # Levene Test en media
    levene_mean <- leveneTest(data[,paste0(v_dep)]~data[,paste0(v_indep)],center="mean")
    
    # Levene Test en mediana
    levene_median <- leveneTest(data[,paste0(v_dep)]~data[,paste0(v_indep)],center="median")
    
    
    # Resultados
    print(paste0("Anova (p-value)        : ",summary(anova)[[1]]$`Pr(>F)`[1]))
    print(paste0("Levene mean (p-value)  : ",levene_mean$`Pr(>F)`[1]))
    print(paste0("Levene median (p-value): ",levene_median$`Pr(>F)`[1]))
    
    par(mfrow = c(2,2))
    plot(anova)
}

# Función para tabla resumen ###################################################
    # Uso de librerias requeridas:
    #   -> tidyversse (dplyr)
    #   -> rlang
    #   -> flextable

#' Tabla de resumen de modelos stadisticos con regresiones
#'
#' @param ... parametro que admite objetos que continienen los resultados de modelos 
#' estadisticos, así mismo cada modelo debe llevar el nombre ejem: "GCS|modleo1" = modelo
#' @param exp parametro obtener el exponencial de la los coeeficientes prinicpalmento 
#' en caso de modelos random effect
#'  
#' @return retorna una tabla consolidada de todos los modelos insertados, a través 
#' de la estructuración de flextable que puede ser exportado a word por medio de
#' renderización de markdown 
#' @export
#'
#' @examples 
#' # regreisón lineal
#' m_1 <- lm(cyl~disp,data = mtcars)
#' m_2 <- lm(disp~speed,data = mtcars)
#' summary_model_table("Modelo 1| Coeff" = m_1,"Modelo 2| Coeff" = m_2)
#' # regresión de un modelo logistico
#' logit_1 <- glm(GCS~n_adult+ocup + covid,family = binomial(link = "logit"),data = df)
#' logit_1mfx <- logitmfx(GCS~n_adult+ocup + covid,data = df) # efectos marinales modelo 1
#' logit_2 <- glm(GCS~n_adult+ocup,family = binomial(link = "logit"),data = df)
#' summary_model_table("GCS model 1|Coeff" = logit_1,"GCS model 1|dF/dx" = logit_1mfx,"GCS model 2|Coeff" = logit_2)
summary_model_table <- function(...,exp = FALSE){
    # El primer modelo insertado deberá ser el que contenga la mayor cnatidad de 
    #variables
    
    # Los nombres de las modelos deben ser estar acompañados por:
    #   => Casos normales                   -> "|coeff"
    #   => Efectos marginales de un logit   -> "|dF/dx" 
    #   => odds ratio de un logit           -> "|Odds Ratio"
    
    # lista que almacenará los modelos
    ls = list(...)
    # Vector que almacenará los nombres de los modelos
    models_names = names(ls)
    
    # Data frame madre vacio
    tab = data.frame()
    for (i in 1:length(ls)) {
        
        # Caso de Coeficientes
        if (grepl("Coeff",models_names[i])) {
            # Conviertiendo en data frame la lista que contiene el resumen del modelo, la columna de coeficinetes
            results = as.data.frame(summary(ls[[i]])$coefficients)
        # Caso de efectos marginales
        } else if (grepl("dF/dx",models_names[i])) {
            results = as.data.frame(ls[[i]]$mfxest)
        } else if (grepl("Odds Ratio",models_names[i])) {
            results = as.data.frame(ls[[i]]$oddsratio)
        } else if (grepl("Exp",models_names[i]))  {
            # Conviertiendo en data frame la lista que contiene el resumen del modelo, la columna de coeficinetes
            results = summary(ls[[i]])$coefficients
            results[,"Estimate"] = exp(results[,"Estimate"]) 
            results = as.data.frame(results)
        }
        
        # Cambiando los nombres de las columnas
        names(results) = c("coeff","Std. Error","z-value","Pr(>|z|)")
        
        results = results %>%
            # Creanod la columna p-value que indicará el nivel de significancia
            mutate(`p-value` = ifelse(`Pr(>|z|)`<= 0.001,"***",
                                      ifelse(`Pr(>|z|)` > 0.001 & `Pr(>|z|)` <= 0.01, "** ", 
                                             ifelse(`Pr(>|z|)` > 0.01 & `Pr(>|z|)` <= 0.05,"*  ",
                                                    ifelse(`Pr(>|z|)` > 0.05 & `Pr(>|z|)` <= 0.1, ".  ","   "))))) %>%
            # Redondeando el coeff
            mutate(coeff = round(coeff,3)) %>%
            # Concatenando el coeficiiente con el p-value
            mutate(coeff = paste(coeff,`p-value`)) %>%
            # Redondenado el valor del error estandar
            mutate(`Std. Error` = round(`Std. Error`,3)) %>%
            # Conviertiendo en string los valores obtenidos
            mutate(`Std. Error` = paste("(",`Std. Error`,")",sep = "")) %>%
            # Menteniendo en el data frame solo los coeficientes y el error estandar
                # dplyr::select -> dado que la librería MASS también presenta la función
                #   select
            dplyr::select(coeff,`Std. Error`)
        
        # Obteniendo los nombres de las variables consideradas en el modelo 
        r_names = rownames(results)
        
        # Creanod un data frame vacio
        df_model = data.frame()
        for (j in 1:(2*nrow(results))) {
            if(j %% 2 == 1){
                # A las celdas impares insertando el nombre de la variable real
                    # (j+1)/2 -> nos devuelve la mitad de j+1 (j es impar)
                df_model[j," "] = r_names[(j+1)/2]
                # Inserando en celdas impares el coeficiente de la varible (ubicado en df results, columna coeff)
                df_model[j,paste0(models_names[i])] = results$coeff[(j+1)/2]
            } else {
                # A las celdas impares insertando el nombre de la variable real + (Std.Error)
                df_model[j," "] = paste(df_model[j-1," "],"(Std.Error)")
                # A las celdas pares insetando error standar del la variable (ubicado en df results, columna Std.Error)
                df_model[j,paste0(models_names[i])] = results$`Std. Error`[j/2]
            }

        }

        # Igualando el data frame base con el del primer modelo
        if(i == 1) {
            tab = df_model
        } else {
            # Uniendo la dos bases de datos
            tab = tab %>%
                # Juntando la el data frame madre con los demás data frames
                left_join(df_model,by = " ")            
        }

    }
    
    # Eliminando las celdas pares de la columna " "
    for(i in seq(2,nrow(tab),by = 2)) {
        tab[i," "] = ""
    }
    
    # Modificnado la tabla
        # Seprando los nombres las columnas por "|"
    str_colnames <- str_split(colnames(tab),"[|]") 
    header_names <- c()
    columns_names <- c()
    for(i in 1:length(str_colnames)) {
        if(i == 1) {
            header_names <- append(header_names,str_colnames[[i]][1])
            columns_names <- append(columns_names,str_colnames[[i]][1])
        } else {
            header_names <- append(header_names,str_colnames[[i]][1])
            columns_names <- append(columns_names,str_colnames[[i]][2])
        }
    }
    
    header_names <- count_values_vector(header_names)
    # colnames(tab) <- columns_names
    # Creando la tabla
    tab <- tab %>%
        # Creando la tabla en flextable
        flextable() %>%
        # Elimiando el header
        delete_part(part = "header") %>%
        # Incorporando el primer nivel del header
        add_header_row(values = columns_names,top = FALSE) %>%
        # Agregando columnas en el header
        add_header_row(colwidths = as.numeric(header_names),values = names(header_names)) %>%
        # Agregando el título de la tabla
        # set_caption(caption = "Title") %>%
        theme_booktabs(bold_header = TRUE) %>%
        # Añadiendo un nota debajo de la tabla
        add_footer_lines(values = "Significancia estadística: . = p<0,1, * = p<0,05, ** = p<0,01, *** = p<0,01.\nError Estandar en paréntesis.") %>%
        # autofit() %>%
        align(align = "center",part = "header")
    
    return(tab)
}


    
