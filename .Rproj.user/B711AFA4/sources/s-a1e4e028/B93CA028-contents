## Importación de datos

Esta sección contiene instrucciones y ejemplos para importar datos a R, ya sea desde Excel, creando tus propias tablas de datos, importar desde PDF en R, u otros.

#### Importar archivos desde internet

```
library(readxl)
library(gdata)
```

Archivo Excel:
```
url_xls <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_1478/datasets/latitude.xls"

## Import the .xls file with gdata: excel_gdata
excel_gdata <- read.xls(url_xls)

## Download file behind URL, name it local_latitude.xls
download.file(url_xls, destfile = "local_latitude.xls")

## Import the local .xls file with readxl: excel_readxl
excel_readxl <- read_excel("local_latitude.xls")
```

Cualquier archivo:
```
## https URL to the wine RData file.
url_rdata <- "https://s3.amazonaws.com/assets.datacamp.com/production/course_1478/datasets/wine.RData"

## Download the wine file to your working directory
download.file(url_rdata, destfile = "wine_local.RData")

## Load the wine data into your workspace using load()
load("wine_local.RData")

## Print out the summary of the wine data
summary(wine)
```
#### Importar archivos csv
```
read.csv(”myfile”, sep = ";")
```
- Por defecto importa la primera fila como nombres de variables, y usa coma como separador.
- Para importar variables como caracteres y no factores:
	```
	read.csv('csvsoundsystem.com/soundsystem.csv', stringsAsFactors = FALSE)
	```
- Para importar datos donde los decimales sean _comas_ y no puntos, y los separadores sean `;`, usar `read.csv2`.

Importar csv: `read_csv`
```
bakeoff <- read_csv("bakeoff.csv")
```
Agregar `skip=TRUE` para saltarse la primera fila.
Para definir datos missing, agregar argumento: `na = c("", "NA", "UNKNOWN", "0")'`

Importar archivos separados por tabulación: `read_tsv`

Importar archivos con formato de tabla: `read_delim`
```
potatoes<-read_delim("potatoes.txt", delim="\t", col_names=properties)
```

Determinar colectores para importar los datos con los tipos y categorías correctos:
```
fac <- col_factor(levels = c("Beef", "Meat", "Poultry"))
int <- col_integer()

## Edit the col_types argument to import the data correctly: hotdogs_factor
hotdogs_factor <- read_tsv("hotdogs.txt",
                           col_names = c("type", "calories", "sodium"),
                           col_types = list(fac, int, int))
```


###### Importar csv con `fread`
Importa tablas en formato `data.table` y `data.frame`. 

```
library(data.table)
potatoes <- fread("potatoes.csv")
```

Argumentos para botar o seleccionar variables:
```
fread("path/to/file.txt", drop = 2:4) ##botar variables 2 a 4
fread("path/to/file.txt", select = c(1, 5)) ##importar variables 1  y 5
fread("path/to/file.txt", drop = c("b", "c", "d"))
fread("path/to/file.txt", select = c("a", "e"))
```



###### Importar archivos separados por tabulación:
```
hotdogs<-read.delim("hotdogs.txt", header=T)
```
- Por defecto importa la primera fila como nombres de variables, y usa tabulación `\t` como separador.
- `header` indica que al primera fila son los nombres de variables. 
- Para importar datos donde los decimales sean _comas_ y no puntos, y los separadores sean `;`, usar `read.csv2`.

#### Importar datos desde Excel 
Usando `readxl`
```
library(readxl)
```

```
encuesta <- readxl::read_excel("clase_25-6/Encuesta.xlsx")
```

Importar una hoja específica:
```
diccionario <- readxl::read_excel("clase_25-6/Encuesta.xlsx",
                                  sheet="Diccionario")
```


Nombres de hojas:
```
excel_sheets("urbanpop.xlsx")
```

Importar hojas de Excel: `sheet= 1`

Definir nombres de columnas manualmente:
```
cols <- c("country", paste0("year_", 1960:1966))
pop_b<- read_excel("urbanpop_nonames.xlsx", col_names=cols)
```

Saltarse casos: `skip=4`

Ver las hojas del documento:
```
readxl::excel_sheets()
```

Para importar todas las hojas en un archivo:
```
archivo <- "clase_25-6/Encuesta.xlsx"

lista <- lapply(readxl::excel_sheets(archivo), readxl::read_excel, path = archivo)

lista[[1]]
lista[[2]]
```


###### Importar y editar datos desde Excel con `XLConnect `

```
library(XLConnect)
```

```
my_book<-loadWorkbook("urbanpop.xlsx")
```

```
## List the sheets in my_book
getSheets(my_book)

## Import the second sheet in my_book
readWorksheet(my_book, sheet = 2)
```

```
## Import columns 3, 4, and 5 from second sheet in my_book: urbanpop_sel
urbanpop_sel <- readWorksheet(my_book, sheet = 2, startCol = 3, endCol = 5)

## Import first column from second sheet in my_book: countries
countries <- readWorksheet(my_book, sheet = 2, startCol = 1, endCol = 1)

## cbind() urbanpop_sel and countries together: selection
selection <- cbind(countries, urbanpop_sel)
```

```
## Build connection to urbanpop.xlsx
my_book <- loadWorkbook("urbanpop.xlsx")

## Add a worksheet to my_book, named "data_summary"
createSheet(my_book, "data_summary")

## Use getSheets() on my_book
getSheets(my_book)
```

```

## Rename "data_summary" sheet to "summary"
renameSheet(my_book, "data_summary", "summary")

## Print out sheets of my_book
getSheets(my_book)

## Save workbook to "renamed.xlsx"
saveWorkbook(my_book, "renamed.xlsx")
```

```
## Remove the fourth sheet
removeSheet(my_book, "summary")

## Save workbook to "clean.xlsx"
saveWorkbook(my_book, "clean.xlsx")
```
#### Importar tabla pivotada de Excel

Cuando las tablas no vienen en formato tidy, sino con varios headers e incluso headers al lado izquierdo, se usan los paquetes `tidyxl` y `unpivotr`

Importar usando `readxl`
```
alojo <- readxl::read_xlsx("Datos/3 Establecimientos de alojamiento turístico 2017-2019.xlsx",
                           sheet=2+1,
                           skip=4)
```

Luego se transforma para que cada _celda_ de Excel corresponda a una fila en R:
```
alojo2 <- unpivotr::as_cells(alojo)
```

Luego se aplica `unpivotr::behead()` para indicar dónde están los headers y en qué dirección:
```
library(unpivotr)

alojo3 <- alojo2 %>%
  behead("up-left", "fecha") %>%
  behead("up", "nivel") %>%
  behead("left", "region")
```

[https://github.com/nacnudus/tidyxl](https://github.com/nacnudus/tidyxl)
[https://github.com/nacnudus/unpivotr](https://github.com/nacnudus/unpivotr)

###### Importar archivos de Stata
```
foreign::read.dta()
```


```
casen <- readstata13::read.dta13("~/Casen/Casen 2017.dta") %>% as_tibble()
```
###### Importar archivos de SPSS
```
library(foreign)
read.spss
```
Reads SPSS data file	read.spss(“myfile”)

###### Importar datos con `readr`
Importa datos creando objetos de clase `tbl_df`, `tbl` y `data.frame`.

- Saltarse casos, y especificar la muestra: `skip = 6, n_max = 5`
- Para definir los nombres de columnas: `col_names=c("area", "temp", "size", "storage", "method","texture", "flavor", "moistness")`
- Para definir manualmente los tipos de cada columna: `col_types="cdil"` (character, double, integer, logical).
	```
	potatoes_char <- read_tsv("potatoes.txt", col_types = "iiiiiddd", col_names = properties)
	```
- El delimitador es `delim=""`

#### Importar otros archivos con formato de tabla
```
path <- file.path("data", "hotdogs.txt") ## directorio

hotdogs <- read.table(path, 
                      sep = "/t", ##tabulación
                      head=FALSE, ##sin nombres de columna
                      col.names = c("type", "calories", "sodium"))
```
- `header` indica que al primera fila son los nombres de variables. En `read.table`, `FALSE` va por defecto. El separador se indica con `sep`.
- Para especificar los tipos de variables, agregar el argumento `colClasses = c("factor", "NULL", "numeric"))`


#### Importar tablas desde PDF

Usando Tabulizer:
```
library(tabulizer)

out <- extract_tables("/Users/bastianolea/Documents/RStudio/Tarapacá\ R/Servel/Participación\ Municipales\ 2016\ comuna\ género.pdf",
                      output = "data.frame")

out2 <- as_tibble(out, .name_repair = "minimal")

View(out2)
```


```
library(tabulizer)
##Convertir PDF a data.frame
##Sirvió recoratar el PDF para que no tuviera logo en al página 1
mesas_2016 <- extract_tables("~/Servel/Mesas Primarias 2016.pdf",
                             pages = c(55:63), ##páginas 1 a 7
                             output = "data.frame")

mesas_2016b <- bind_rows(mesas_2016, .id = NULL) ##%>% ##Combinar lista de dataframes en un solo dataframe

```

Instalación en Mac: [https://gist.github.com/tomsing1/1da54d3f720ed96fbbb5a3f075bd2a56](https://gist.github.com/tomsing1/1da54d3f720ed96fbbb5a3f075bd2a56)
Tutorial: [https://datascienceplus.com/extracting-tables-from-pdfs-in-r-using-the-tabulizer-package/](https://datascienceplus.com/extracting-tables-from-pdfs-in-r-using-the-tabulizer-package/)

#### Combinar bases de datos
  
Para combinar bases donde las columnas son las mismas (es decir, agregar casos)
```
bind_rows(datos1, datos2)
```

Par añadir columnas a una base
```
bind_cols
```

Unir a partir de una columna que coincida:
```
datos1 %>% ##Unir datos con mapa
    left_join(datos2)
```

Combinar en base a variables compartidas dentro de dos datasets:
```
## Print the votes_processed dataset
votes_processed

## Print the descriptions dataset
descriptions

## Join them together based on the "rcid" and "session" columns
votes_joined <- votes_processed %>%
  inner_join(descriptions, by = c("rcid", "session"))
```

Ejemplo:
```
ilo_data <- ilo_hourly_compensation %>%
  inner_join(ilo_working_hours, by = c("country", "year"))
```
#### Conectar a base de datos

```
library(DBI)
```


```
## Connect to the MySQL database: con
con <- dbConnect(RMySQL::MySQL(), 
                 dbname = "tweater", 
                 host = "courses.csrrinzqubik.us-east-1.rds.amazonaws.com", 
                 port = 3306,
                 user = "student",
                 password = "datacamp")

## Build a vector of table names: tables
tables <- dbListTables(con)
```

Importar una tabla en específico:
```
## Import the users table from tweater: users
users<- dbReadTable(con, "users")
```

Importar todas las tablas:
```
## Get table names
table_names <- dbListTables(con)
table_names
## Import all tables
tables <- lapply(table_names, dbReadTable, conn = con)
```
#### Aplicar factor de expansión
Cuando los datos provienen de fuentes estadísticas que requieren expansión:

```
library(mefa)
casen_w <- rep(casen, times = casen$expc)
```


O con tidyr:
```
casen %>% 
    select(expr, sexo, pco1, oficio4) %>%
    slice(1:100) %>%
    tidyr::uncount(expr, .remove = F) %>%
    print(n=Inf)
```
## Codificación

En esta sección encontrarás comandos para producir estructuras de dato de forma “manual”; es decir, imputando los datos caso por caso, o bien trabajando con ellos de forma personalizada.

#### Importar datos desde vectores
```
Año = c(1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)
Nacimientos= c(4571,4782,4834,4701,4787,4467,4571,4583,4776,4761,5017,5287)
Defunciones= c(889,946,946,916,991,1026,1040,1127,1092,1070,1156,1199)
TasaNatalidad= c(20.8, 21.0, 20.4, 19.1, 19.0, 17.2, 17.2, 16.8, 17.1, 16.6, 17.1, 17.6)
TasaMortalidad= c(4.0, 4.1, 4.0, 3.7, 3.9, 3.9, 3.9, 4.1, 3.9, 3.7, 3.9, 4.0)
TasaFecundidad= c(2.61, 2.63, 2.58, 2.43, 2.42, 2.21, 2.22, 2.18, 2.23, 2.17, 2.23, 2.29)
Tarapaca <- data.frame(Año,Nacimientos,Defunciones,TasaFecundidad,TasaMortalidad,TasaNatalidad)
```
#### Escribir dataframe
Crear un dataframe escribiendo las columnas

```
sales <- tibble::tribble(
  ~quarter, ~year, ~sales,
  "Q1",    2000,    66013,
  "Q2",      NA,    69182,
  "Q3",      NA,    53175,
  "Q4",      NA,    21001,
  "Q1",    2001,    46036,
  "Q2",      NA,    58842,
  "Q3",      NA,    44568,
  "Q4",      NA,    50197,
  "Q1",    2002,    39113,
  "Q2",      NA,    41668,
  "Q3",      NA,    30144,
  "Q4",      NA,    52897,
  "Q1",    2004,    32129,
  "Q2",      NA,    67686,
  "Q3",      NA,    31768,
  "Q4",      NA,    49094
)
```
#### Crear vector y matriz de datos
```
linkedin <- c(16, 9, 13, 5, 2, 17, 14)
facebook <- c(17, 7, 5, 16, 8, 13, 14)
views <- matrix(c(linkedin, facebook), nrow = 2, byrow = TRUE)
```
#### Repetir un valor

Repetir un 5 10 veces:
```
rep(5, times=10)
```

Repetir dos valores, uno cada x veces:
```
rep(c("a", "b"), each = 100)
```
#### Crear sequencia de números

Generar del 1 al 10 de uno en uno
```
seq(from=1, to=10, by=1)
```

Generar 10 valores desde 1 a 100
```
seq(from=1, to=100, length.out=10)
```
#### Crear data frame a partir de vectores

```
data <- data.frame(llamados_ventas,
                   planes_contratados)
```
#### Crear listas

```
my_list <- list(my_vector, my_matrix, my_df)
```

```
pop_list<-list(pop_1, pop_2, pop_3)
```

Listas con nombres:
```
my_list <- list(vec=my_vector, mat=my_matrix, df=my_df)
shining_list <- list(moviename = mov, actors=act, reviews=rev)
```

Seleccionar desde listas:
```
shining_list[["reviews"]]
shining_list$reviews

## Print out the vector representing the actors
shining_list[[2]]

## Print the second element of the vector representing the actors
shining_list[[2]][2]
```

Expandir una lista:
```
## We forgot something; add the year to shining_list
shining_list_full <- c(shining_list, year = 1980)

## Have a look at shining_list_full
str(shining_list_full)
```
#### Añadir casos manualmente

```
datos %>%
	add_row(x = 4, y = 0, Nombre = "Mapache")
```
#### Crear vector de nombres

Útil para selectores Shiny donde las alternativas son largas.
El segundo argumento son los nombres. En el ejemplo, se crea un vector de números donde los nombres son el vector, cosa que en Shiny aparezcan las alternativas con el contenido del vector pero internamente se retorne un número.

```
vector_nombrado <- setNames(c(1:33), vector)
```
#### Añadir vector a una matriz

```
all_wars_matrix <- cbind(star_wars_matrix, worldwide_vector)
```
#### Contar hacia abajo

Hacer una columna o variable que contenga números del 1 hasta el máximo de filas de la base:

```
mutate(fila = 1:n())
```

```
e %>%
    mutate(fila = 1:nrow(e))
```
#### Usar output como texto de vector
Retorna el output como el texto necesario para reproducir un vector

```
dput()
```
## Limpieza de datos
Esta sección contiene instrucciones y ejemplos para limpiar datos en R, con operaciones que cambian los nombres de las variables, eliminan observaciones, tratan los datos perdidos de distintas maneras, etc.

#### Seleccionar variables
El verbo `select` permite mantener, botar, y reordenar variables.

Seleccionar variables que contengan un término:
```
datos %>%
select(contains("término"))
```

Filtrar datos para dejar o eliminar ciertas variables en la base de datos.
```
select(datos, var1, var2, var3)
select(datos, var1:var9)
```
Se puede usar `-var4` para ignorar una columna y `-(var1:var5)` para ignorar varias seguidas.

Ejemplo:
```
ratings %>% 
  select(series, channel, bbc, viewer_growth)
```
Acá no es necesario poner la base de datos como primer argumento.

Poner una variable antes que todas:
```
datos %>% 
  select(var1, everything())
```
El argumento `everything()` pone el resto de las variables al final.

###### Des-seleccionar variables
El signo menos sirve para indicar que se seleccionan todas menos lo especificado:
```
ratings %>% 
  select(-ends_with("day"))
```


Combinar comandos de selección:
```
ratings %>% 
  select(channel, everything(), -ends_with("day"))
```
La variable especificada al frente, luego todas las demás, menos las que terminan con "day".

#### Reordenar columnas

```
library(dplyr)
weather5 <- select(weather4, date, Events, CloudCover:WindDirDegrees)
```
#### Ordenar datos

```
arrange(cran2, ip_id) ## ascendente
arrange(cran2, desc(ip_id)) ## descendente
```

Ordenar por una variable, y luego por la otra:
```
arrange(datos, var1, var2)
```

Ejemplos:
```
arrange(top_counts, desc(count))
```

```
arrange(series, episode) %>% 
```

```
bakers_mini %>% 
  arrange(age) %>% 
  glimpse()
```

###### Ordenar datos con base

```
sort(datos$var1)
order(datos$var1)
```

```
## Use order() to create positions
positions <-  order(planets_df$diameter)

## Use positions to sort planets_df
planets_df[positions, ]
```
#### Cambiar tipo de datos

Convertir a carácter:
```
students$Grades <- as.character(students$Grades)

```

Convertir a factor:
```
mutate(var1 = as.factor(skill))
```

```
students$Fedu <- as.factor(students$Fedu)
```

```
weather6 <- mutate_at(weather5, vars(CloudCover:WindDirDegrees), funs(as.numeric))
```
#### Filtrar datos

```
filter(datos, var1=="Peq")
filter(pack_sum, count>679)
```

Filtrar datos de acuerdo a ciertas condiciones de los casos.
```
filter(datos, var2=="categoría")
filter(datos, var2=="categoría", var3=="mapache")
filter(cran, r_version <= "3.0.2", country == "IN")
filter(cran, country=="US" | country == "IN")
```


```
filter(episode == 1 | episode == max(episode))
```

```
filter(star_baker==0 & technical_winner == 0)
```

###### Con subset
```
nombredefiltro<-subset(datos,var1=="categoria")
```
Ejemplo:
```
west<-subset(pollution,region=="west")

## Select planets with diameter < 1
subset(planets_df, diameter <1 )
```

###### Con base

Otra forma:
```
nuevodataframe <- datos[datos$var1=="categoria",]
```

```
nuevodataframe <- datos[datos$var1=="categoria" & datos$var2=="categoria",]
```

Filtrar datos específicos de una variable:
```
common_cyl <- filter(cars, ncyl %in% c(4, 6, 8))
```

```
## Select the non-US revenue for first two movies
non_us_some <- all_wars_matrix[1:2,2]
```
#### Convertir datos a tibble
Convertir una tabla de datos a el formato tibble, más moderno, ordenado, y usado en el _tidyverse._
```
datos_tibble <- tibble::as_tibble(datos)
```
#### Cortar datos o eliminar filas

Permite recortar un dataframe por las filas, para separar por ejemplo las primeras 200 filas:
```
datos %>%
	slice(1:200)
```

Si se necesita eliminar algunas filas:
```
datos %>%
	slice(-1, -2, -3)
```
#### Eliminar más de una palabra a la vez

```
datos %>%
mutate(ambito = stringr::str_remove_all(ambito, "\\r|\\n"))
```
#### Reemplazar más de una palabra a la vez

```
datos %>%
mutate(ambito = stringi::stri_replace_all_fixed(ambito, 
                                                  c("gnero", "indicgena", "plotica", "oiriginarios"), 
                                                  c("genero", "indigena", "politica", "originarios"),
                                                  vectorize_all = FALSE))
```
#### Eliminar caracteres especiales de un texto

Para eliminar acentos y eñes de los textos:
```
iconv(tolower(comuna), from = 'UTF-8', to = 'ASCII//TRANSLIT')
```
#### Filtrar columnas en base a condición
```
datos %>%
	select(which(nlevels(.) == 0))
```
#### Limpiar números

Elimina cualquier texto y deja sólo el número
```
library(readr)
parse_number(datos)
```

Cuando un número viene en formato caracter pero también con símbolos, espacios o letras. También sirve para eliminar los caracteres y dejar sólo números:
```
mutate(var1 = readr::parse_number(var1))
```
#### Reemplazar datos perdidos
Reemplazar casos missing o NA con 0
```
mutate(filtro_calculado = tidyr::replace_na(filtro_calculado, 0))
```

Convertir los missing en un valor:
```
mutate(cantidad3 = ifelse(is.na(cantidad3), 0, cantidad3)) %>%
```

Para convertir un texto o símbolo en missing:
```
mutate(variable = na_if(variable, "símbolo"))
```

```
na_if() to replace specified values with a NA.
```

```
coalesce() to replace missing values with a specified value.
```

```
tidyr::replace_na() to replace NA with a value
```
#### Convertir texto vacío a missing

```
mutate(nombres = replace(nombres, !str_detect(nombres, ""), NA))
```
#### Recodificar a missing

Usando `na_if()`
```
datos %>% mutate(d11 = na_if(d11, "No sabe")) 
```

Usando `replace()` 
```
datos %>% mutate(d11 = replace(d11,"No sabe", NA)) 
```


na_if() to replace specified values with a NA.

coalesce() to replace missing values with a specified value.

tidyrreplace_na() to replace NA with a value

#### Datos perdidos

```
sum(is.na(datos))
```
Retorna cantidad de datos missing. También se logra con `summary(datos)` o con `datos %>% filter(is.na(variable))`

```
which(is.na(datos))
```
Entrega la ubicación de los datos missing.

```
## Replace all empty strings in status with NA
social_df$status[social_df$status == ""] <- NA

## Print social_df to the console
social_df

## Use complete.cases() to see which rows have no missing values
complete.cases(social_df)

## Use na.omit() to remove all rows with any missing values
social_df<-na.omit(social_df)
```
  
Definir valores missing al importar un archivo:
```

## Edit to add list of missing values
bakeoff <- read_csv("bakeoff.csv", skip = 1,
                    na = c("", "NA", "UNKNOWN"))
```
#### Filtrar datos perdidos
```
datos %>%
  filter(!is.na(var1) & !is.na(var2)) 
```

```
datos %>%
select(User_Score, Critic_Score) %>%
   na.omit() %>%
```
#### Filtrar datos perdidos en varias columnas
Filtrar observaciones donde sean missing en varias columnas

```
datos %>%
filter_at(vars(eval_act_pol_1_a:eval_act_pol_1_ad), any_vars(!is.na(.)))
```
#### Limpiar nombres de variables

Elimina símbolos de las variables y las estandariza:
```
library(janitor)
ratings <- messy_ratings %>%  
  clean_names("lower_camel")
```

En el argumento de  `clean_names` determina el tipo de nombre:
```
"snake" produces snake_case

"lower_camel" or "small_camel" produces lowerCamel

"upper_camel" or "big_camel" produces UpperCamel

"screaming_snake" or "all_caps" produces ALL_CAPS

"lower_upper" produces lowerUPPER

"upper_lower" produces UPPERlower
```

Para convertir los nombres de las categorías de una variable factor:
```
library(stringr)
levels(eme2$nacionalidad) <- str_to_title(eme2$nacionalidad)
```
#### Ordenar columnas alfabéticamente

```
datos %>%
select(sort(names(.)))
```
#### Renombrar todas las columnas

Reemplazar un texto en lo nombres de variables
```
datos %>%
  rename_all(funs(str_replace(., "pregunta", "p")))
```

Renombrar columnas específicas
```
rename_at(vars(starts_with("b")), funs(str_replace(., "b", "agua")))
```
#### Renombrar columnas con base

```
nombres <- gsub("Pregunta ", "p", names(encuesta))
```
#### Renombrar variables o columnas


```
rename(nombrenuevo = nombreviejo) %>%
```

Cambiar nombre de variables:
```
view(datos)
col(datos)
names(datos)[1]<-"Campo"
names(datos)[2]<-"Ciudad"
```
El número 1 significa que es el primer elemento o primera columna. Entre comillas va el nombre.

#### Añadir sufijo a nombres de columnas

```
rename_at(vars(c(30:53)),function(x) paste0(x,"_x"))
```
#### Eliminar un término en el título de variables

Seleccionar variables que contengan un término:
```
datos %>%
select(contains("término")) %>%
gather(variable, frequency) %>%
mutate(variable = str_remove(variable, "término"))
```

Eliminar todas las palabras anteriores a un término, incluyendo al término:
```
gathered_data %>%
    mutate(response_var = str_remove(response_var, '.*rude to '))
```

Ejemplo:
```
learning_platform_usefulness <- multiple_choice_responses %>%
  ## select columns with LearningPlatformUsefulness in title
  select(contains("LearningPlatformUsefulness")) %>%
  ## change data from wide to long
  gather(learning_platform, usefulness) %>%
  ## remove rows where usefulness is NA
  filter(!is.na(usefulness)) %>%
  ## remove "LearningPlatformUsefulness" from each string in learning_platform 
  mutate(learning_platform = str_remove(learning_platform, "LearningPlatformUsefulness"))
```
#### Rellenar casos

Cuando una columna sólo tiene valores cuando éstos cambian, y lo que se quiere es llenar de repeticiones entre cada cambio de los valores,
cosa que `1 NA NA 2 NA NA 3 NA NA` se vuelva `1 1 1 2 2 2 3 3 3`

```
datos %>%
fill(columna)
```
#### Convertir fechas de Excel

```
as.Date(42736, origin = "1899-12-30")
```


```
library(tibble)
library(janitor)

excel_numeric_to_date(as.numeric(as.character(42736)), date_system = "modern")
```
#### Eliminar una categoría

```
comics_filtered <- comics %>%
  filter(align != "Reformed Criminals") %>%
  droplevels()
```
#### Añadir identificadores o folio

```
iris$Flower <- 1:nrow(iris)
```
#### Remover texto de una categoría

```
mutate(tipo = str_remove(tipo, "texto a borrar"))
```
#### Borrar espacios antes y después
```
stringr::desaparecidos$Región <- str_trim(desaparecidos$Región)
```
#### Agregar caracteres antes o después
```
stringr::str_pad(c("23485W", "8823453Q", "994Z"), width=9, side="left", pad="0")
```
#### Detectar patrones en texto
```
stringr::str_detect(students3$dob, "1997")
```
#### Buscar y reemplazar valores
```
stringr::str_replace(students3$sex, "M", "Male")
weather3$day <- stringr::str_replace(weather3$day, "X", "")
```
También sirve para eliminar caracteres o patrones.

Ejemplo:
```
bakers <- bakers %>% 
  mutate(position_reached = stringr::str_replace(position_reached, "-", " "),
         position_reached = stringr::str_replace(position_reached, "THIRD PLACE", "RUNNER UP"))
```


```
mutate(tipo = stringr::str_replace(tipo, "mal escrito", "bien escrito"))
```
#### Filtrar outliers

Se crea una variable que especifique si son o no outliers, y luego se filtra la base según esa variable.


```
## Filter for Asia, add column indicating outliers
gap2007 <- gap2007 %>%
  mutate(is_outlier = lifeExp < 50)

## Remove outliers, create box plot of lifeExp
gap2007 %>%
  filter(!is_outlier) %>%
  ggplot(aes(x = 1, y = lifeExp)) +
  geom_boxplot()
```
#### Convertir valores a mayúsculas o minúsculas:
Convertir a minúsculas:
```
tolower("AAAA")
```

Convertir a mayúsculas:
```
toupper("aaaa")
states_upper <- toupper(states)
```

Ejemplo:
```
bakers <- bakers %>% 
  mutate(position_reached = str_to_upper(position_reached))
```
#### Borrar prefijo de variables

Borra el prefijo de los años, dejando solo caracteres entre ubicaciones 2 y 5
```
names(datos) <- substring(names(datos),2,5)
```
#### Rellenar las fechas en una serie de tiempo

```
##inicio y final de la serie parcial
min(conceptos$date)
max(conceptos$date)

##crear serie completa
ts <- seq.POSIXt(as.POSIXct(min(conceptos$date),
as.POSIXct(max(conceptos$date)), 
by="day")

df <- data.frame(date=ts)

left_join(df, conceptos)
##unir la serie completa con la serie parcial
```
#### Ejemplos de Regex en R

`"."` equivale a cualquier dato.
`"ha."` retorna `TRUE` para `"happy"`

`".*"` cualquier carácter antes o después del texto
`.` cualquier carácter
`$` final de palabra


`\\d` es cualquier dígito único
`\\d+ ` es cualquier cantidad de dígitos

`\\w+` es cualquier palabra

`\\.` es un punto

Ejemplo:
```
"\\d\\d\\. \\w+ - "
```
...va a hacer match a "06. VI - "

Dejar sólo el último caracter:
```
^(.*)(?=.$)
```

Borrar todo hasta un caracter:
```
^[^\\)]*\\) ##hasta ")"
```

Borrar todo dentro de paréntesis
```
([^()]*)
```

Borrar todo desde "POB."
```
POB\\..*$
```

Borrar todo después de una palabra pero mantener la palabra:
```
(?<=caleta).*
```

[https://help.relativity.com/9.3/Content/Relativity/Regular\_expressions/Regular\_expression\_metacharacters.htm](https://help.relativity.com/9.3/Content/Relativity/Regular_expressions/Regular_expression_metacharacters.htm)

#### Fechas

```
library(lubridate)
```

La fecha se pone como argumento entre comillas, y se utiliza la función correspondiente:
```
dmy("17 August 2010")
mdy("August 17 2010")
ymd("2010 August 17")
```

Luego de definir las fechas con `lubridate`, se pueden realizar operaciones, por ejemplo, con `mutate`.

- `interval`: intervalo temporal entre dos fechas.
- `duration`: número de segundos en un intervalo.
- `period`: cambio de tiempo en un intervalo.

Crear variables a partir de fechas:
```
datos %>%
mutate(fecha= dmy(variableoriginal))
```

Calcular diferencias entre fechas:
```
datos %>%
mutate(intervalo = interval(var1, var2)
```

Luego se pueden convertir estos intervalos a otras unidades de tiempo:
```
datos %>%
mutate(años_decimal = intervalo / years(1),
	años_enteros = intervalo %/% years(1))
```
En vez de `years` pueden usarse `months`, `hours`, etc.

Obtener el mes de una fecha:
```
mutate(month(last_date_appeared_us, label = TRUE))
```

Ejemplos:
```
baker_time <- baker_time  %>% 
  mutate(time_on_air = interval(first_date_appeared_uk, last_date_appeared_uk),
         weeks_on_air = time_on_air / weeks(1),
         months_on_air = time_on_air %/% months(1))
```
#### Agrupar datos
Permite que los cálculos posteriores se apliquen a todas las categorías de una variable por separado.

```
grupodedatos<-group_by(datos, var1)
```

Ejemplos:
```
grupodedatos<-group_by(datos, var1)
summarize(grupodedatos, mean(var2))
```
Retorna el promedio de la variable dos para cada categoría de la variable 1.

```
bakeoff %>% 
  filter(!is.na(us_season)) %>% 
  group_by(us_season)  %>% 
  skim()
```

 Para desagrupar: `ungroup()`

#### Crear variables

```
mutate(cran3, size_mb = size / 2^20)
mutate(cran3, correct_size = size + 1000)
```

Crear una nueva variable que indique el promedio de una variable:
```
summarize(cran, avg_bytes = mean(size))
```
Esto se puede usar para obtener puntos promedios de una variable para una serie temporal:
```
datos2 <- datos %>%
group_by(year) %>%
summarize(var_promedio
= mean(var1))
```
Y luego usar la nueva variable `var_promedio` como eje y de un gráfico.

Este código crea nuevas variables  a partir de operaciones realizadas sobre un conjunto de datos agrupados (con `group_by`):
```
pack_sum <- summarize(by_package,
                      count = n(),
                      unique = n_distinct(ip_id),
                      countries = n_distinct(country),
                      avg_bytes = mean(size))
## The 'count' column, created with n(), contains the total number of rows (i.e. downloads) for each package. The 'unique' column, created with n_distinct(ip_id), gives the total number of unique downloads for each package, as measured by the number of distinct ip_id's. The 'countries' column, created with n_distinct(country), provides the number of countries in which each package was downloaded. And finally, the 'avg_bytes' column, created with mean(size), contains the mean download size (in bytes) for each package.
```

Crear variable para todo el data frame:
```
mutate(datos, var9="si")
```
#### Crear variable categórica

Variable nominal:
```
## Sex vector
sex_vector <- c("Male", "Female", "Female", "Male", "Male")

## Convert sex_vector to a factor
factor_sex_vector <- factor(sex_vector)

## Print out factor_sex_vector
factor_sex_vector
```

Variable ordinal:
```
temperature_vector <- c("High", "Low", "High","Low", "Medium")
factor_temperature_vector <- factor(temperature_vector, order = TRUE, levels = c("Low", "Medium", "High"))
factor_temperature_vector
```

```
## Create speed_vector
speed_vector <- c("medium", "slow", "slow", "medium", "fast")

## Convert speed_vector to ordered factor vector
factor_speed_vector <- factor(speed_vector, ordered=TRUE, levels=c("slow", "medium", "fast"))
```

Cambiar nombre de niveles:
```
## Specify the levels of factor_survey_vector
levels(factor_survey_vector) <- c("Female","Male")
```

Desagregar variable categórica:
```
## Male
male <- factor_survey_vector[1]

## Female
female <- factor_survey_vector[2]
```


## Recodificación de datos
Esta sección cubre instrucciones y ejemplos sobre recodificación de datos; es decir, operaciones que alteran el contenido de las variables o crean nuevas variables en base a condiciones o instrucciones.

#### Recodificar variables

Recodificar variables:
```
gapminder %>%
mutate(gdp = gdpPercap*pop)
```
El primer argumento de `mutate` es el nombre de la nueva variable.

```
datos2 <- datos %>% 
  mutate(categorianueva = recode(categoria, "var_antigua" = "var_nueva", 
    "missing" = NA_character_))
```
Si la variable missing es numérica, `NA_integer_`

Para recodificar todos los demás valores en una sola variable, usar `.default`:
```
mutate(tech_win = recode(technical, `1` = 1,
	.default = 0))
```

```
email %>%
  mutate(has_image = recode(image, `0` = FALSE, .default = TRUE))
```

Si la variable a crear es un factor (por ejemplo, dummy), usar `recode_factor` en vez de `recode`.

```
mutate(episode = recode(episode, `1` = "first", .default = "last"))
```

Ejemplos:
```
ratings <- ratings %>% 
  mutate(bbc = recode_factor(channel, 
	"Channel 4" = 0,
	.default = 1))
```

```
mutate(bump = (last - first) / first)
```
#### Crear una variable a partir de operaciones
```
mutate(cran3, size_mb = size / 2^20)
mutate(cran3, correct_size = size + 1000)
```

```
summarize(cran, avg_bytes = mean(size))
```

```
datos %>% mutate(variable = recode(
student, `0` ) NA_character_,

```
Con `.default` cualquier valor que no es 0 es recodificado por esta variable.

Este código crea nuevas variables  a partir de operaciones realizadas sobre un conjunto de datos agrupados (con `group_by`):
```
pack_sum <- summarize(by_package,
                      count = n(),
                      unique = n_distinct(ip_id),
                      countries = n_distinct(country),
                      avg_bytes = mean(size))
```
#### Recodificar con base
```
library(car)
datos$nuevavariable<-recode(datos$variableoriginal,"
			25:34.9='Pequeño';
			35:54.9='Mediano';
			55:65='Grande'",as.factor=TRUE)
```
Se rectifica en "datos" la variable titulada `nuevavariable`, basada en `variableoriginal`, y luego se especifican sus categorías.

```
datos$nuevavariable<-recode(datos$variableoriginal, "
	'hombre'=0;
	'mujer'=1,
				as.integer=TRUE)
```

Cortar el rango de los datos de una variable:
```
adult <- adult[adult$SRAGE_P <= 84, ]
```
#### Crear variable desde condicional

```
datos %>%
mutate(has_image = image > 0)
```

```
datos %>%
mutate(zero = variable == 0)
```

Revisar niveles: `levels(datos$variable)`
Convertir los niveles a factor:
```
datos$variable <- factor(datos$variable, levels=c("TRUE", "FALSE"))
```
#### Recodificación según condiciones
Funciona como un _if true, then._

###### Condicional simple
```
datos %>%
mutate(gen = if_else(
	between(birth_year, 1981, 1996), "millenial", "not_millenial"))
```
La segunda categoría es para los casos que no cumplen la condición.

```
mutate(Valor = ifelse(Delito=="Otras causas", Valor/4, Valor)) %>% ##Si el delito es "otros", dividir por la cantidad de categorías colapsadas por fct_lump (22-n) para sacar el promedio
```


```
 mutate(pobreza2 = case_when(pobreza == "Pobres extremos" ~ "Condición de pobreza",
                                 pobreza == "Pobres no extremos" ~ "Condición de pobreza",
                                 pobreza == "No pobres" ~ "Fuera de condición de pobreza"))

```

###### Múltiples condiciones
```
datos %>%
mutate(gen = case_when(
	between(birth_year, 1965, 1980) ~ "gen_x",
	between(birth_year, 1981, 1996) ~ "millenial"))
```


```
case_when(x %% 15 ==0 ~ "fizz buzz",
		  x %% 3 == 0 ~ "fizz",
		  x %% 5 == 0 ~ "buzz",
		  TRUE ~as.character(x) )
```

Agregar `TRUE ~ "asdas"` para crear una variable para los casos que no cumplen. De lo contrario, serán missing.

Ejemplos:
```
bakers_skill <- bakers %>% 
  mutate(skill = case_when(
    star_baker > technical_winner ~ "super_star",
    star_baker < technical_winner ~ "high_tech",
    TRUE ~ "well_rounded"
  ))
```

```
bakers_skill <- bakers %>% 
  mutate(skill = case_when(
    star_baker > technical_winner ~ "super_star",
    star_baker < technical_winner ~ "high_tech",
    star_baker == 0 & technical_winner == 0 ~ NA_character_,
    star_baker == technical_winner  ~ "well_rounded"
  ))
```

Ojo que para recodificar a missing, `NA_character_` va sin comillas.

#### Convertir caracter a factor

```
is.character(datos$variable)
datos %>%
mutate_if(is.character, as.factor)
```
#### Etiquetar variables
Utilizando base
```
Etiqueta <- c("Año","Nacimientos","Defunciones","Tasa de fecundidad","Tasa de mortalidad","Tasa de natalidad")

library(sjlabelled)
Datos <- set_label(Datos, label = Etiqueta)
```

```
adult$RBMI <- factor(adult$RBMI, labels = c("Under-weight", "Normal-weight", "Over-weight", "Obese"))
```
#### Manipulación de objetos con base

Buscar el atributo en cierto objeto, reportando las filas donde se puede encontrar
```
grep(atributo, objeto)
posicion_valpo <-  grep("Valparaíso", encuesta$`Pregunta 3`)
```

Reemplazar un valor inicial por uno final en cierto objeto.
```
gsub(inicial, final, objeto)
df$region <- gsub("Región", "R.", encuesta$`Pregunta 3`)
```

Substraer de un objeto un carácter entre la posiciones inicial y final.
```
substr(objeto, inicial, final)
```

Permite unir uno o más objetos, en formato carácter.
```
paste(objeto1,..., sep=“ ”)
```
#### Crear variable de interacción

Crea una nueva variable que tiene por categorías a las combinaciones de todos los factores de dos variables.

```
mutate(interacción = fct_cross(var1, var2, sep = " + "))
```
#### Recodificar meses a texto
Para pasar desde el número del mes a el mes con nombre:

```
data %>%
    mutate(Month = lubridate::month(Month, label = T, abbr= F, locale = "es_ES"))
```
#### Calcular promedio móvil

Rolling average, media móvil

```
mutate(hits_rm = zoo::rollmean(hits, k = 7, 
                                        fill = 0, align="left")) %>%
```

Donde `k` representa las filas para atrás que va a promediar

#### Aplicar mutate a varias columnas

```
datos %>%
mutate(across(c(col1, col2), ~ round(.x*100, 1)))
```

Otros ejemplos:
```
mutate(across(g2_1:g2_6, as.numeric)) %>% ##convertir a numéricos
```

```
mutate(across(g2_1:g2_6, ~replace(.x, is.na(.x), 0))) %>% ##poner ceros en vez de NA
```


```
cep_r %>%
  mutate(across(c(percepcion_2, percepcion_3), 
         ~forcats::fct_relevel(.x, "No sabe + No contesta", after=0))) %>%
  count(percepcion_2)
```

O también:
```
mutate_at(vars(4:length(notas)), list(as.character))
```
#### Convertir a logaritmos

Convertir variable a logaritmo:
```
gap2007 <- gap2007 %>%
  mutate(log_pop = log(pop))
```
#### Crear categoría "otros"

Agrupar automáticamente los factores menos comunes en una categoría "Otras":
```
datos %>%
mutate(variable = fct_lump(variable, prop = .08, other_level = "Otras"))
```
Las variables que sean menores al 8% se agrupan en "otras".

Mantener sólo las de mayor frecuencia:
```
datos %>%
mutate(variable = fct_lump(variable, n = 3, other_level = "Otras"))
```
El `n` especifica la cantidad de variables superiores que se mantienen.

Para nombrar categorías como "otros", se seleccionan las que se desean mantener:
```
datos %>%
mutate(variable = fct_other(variable, keep = c("antigua1", "antigua2"), other_level = "Otras"))
```
El resto de las variables no elegidas se vuelven "otras".

El argumento `other_level = "other method"` determina el nombre de la categoría "otros".

Seleccionar las que se desean transformar en "otros":
```
datos %>%
mutate(variable = fct_other(variable, drop = c("antigua1", "antigua2")))
```
De este modo, las variables especificadas pasan a formar parte de una variable "otras".


#### Combinar categorías
Combinar o agrupar múltiples categorías de una variable categórica en una cantidad menor.

```
datos %>%
mutate(variable = fct_collapse(variable,
nueva = c("antigua1", "antigua2"),
nueva 2 = c("antigua3", "antigua4")))
```
#### Dicotomizar la variable de respuesta:

Hacer variable dummy:
```
datos %>%
filter(!is.na(variable)) %>%
	mutate(variable = if_else(
		variable %in% c("categoria", "otra categoria"),
		1,
		0))
```
#### Recodificar categoría si coincide con texto
Sirve, por ejemplo, para corregir errores ortográficos

```
mutate(variable = stringr::str_replace(variable, "texto incorrecto", "Nuevo nombre de la categoría que contiene ese texto")) %>%
```


Para coincidir texto ignorando mayúsculas o minúsculas:
```
mutate(variable = stringr::str_replace(variable, regex('texto', ignore_case = T), "Nuevo nombre de la categoría que contiene ese texto")) %>%
```
#### Reemplazar un valor por otro

Sirve para cambiar algún valor que esté malo por otro, o recodificar un valor a missing (NA)

```
mutate(Casos = replace(Casos, Fecha == ymd("2020-04-15"), valornuevo))
```

Reemplazar para imputar un valor missing:
```
mutate(Casos = replace(Casos, Casos == 0, NA))
```

Reemplazar un missing por otro valor:
```
mutate(nacionalidad = replace(nacionalidad, is.na(nacionalidad), "missing"))
```
#### Crear columna que sea la diferencia entre los valores de otra

Para calcular brechas en columnas con categorías en formato long:
```
mutate(diferencia = porcentaje[sexo == "Mujer"] - porcentaje) %>%
```
#### Renombrar categorías

```
mutate(variable = recode(variable, "antigua" = "nueva"))
```
#### Añadir columna con porcentaje

```
library(janitor)
```

Agregar fila con suma de totales:
```
adorn_totals("row") %>%
```

Agregar columna con suma de totales:
```
adorn_totals("col") %>%
```


```
perc_useful_platform <- learning_platform_usefulness %>%
  ## change dataset to one row per learning_platform usefulness pair with number of entries for each
  count(learning_platform, usefulness) %>%
  ## use add_count to create column with total number of answers for that learning_platform
  add_count(learning_platform, wt = n) %>%
  ## create a new column, perc, that is the percentage of people giving that response for that learning_platform
  mutate(perc = n / nn)
```
#### Añadir columna con promedio
```
usefulness_by_platform <- learning_platform_usefulness %>%
    ## If usefulness is "Not Useful", make 0, else 1 
    mutate(usefulness = if_else(usefulness == "Not Useful", 0, 1)) %>%
    ## Group by learning platform 
    group_by(learning_platform) %>%
    ## Summarize the mean usefulness for each platform
    summarize(avg_usefulness = mean(usefulness))
```
#### Calcular tasa

```
  mutate(Tasa = (Casos/Poblacion)*100000) %>%
```
#### Dicotomizar variables
Crear variable dicotómica:
```
learning_platform_usefulness %>%
    ## If usefulness is "Not Useful", make 0, else 1 
    mutate(usefulness = if_else(usefulness == "Not Useful", 0, 1))
```

```
## Dichotomize the value variable to make a new variable, rude
    mutate(rude = if_else(value %in% c('No, not rude at all', 'No, not at all rude'), 0, 1))
```

Ejemplo:
```
mutate(extranjeros = case_when(nacionalidad == "Chile" ~ "Chilena", TRUE ~ "Extranjera")) %>%
```
#### Cortar una variable continua a factores

Para cortar la variable en intervalos discretos con el límite izquierdo abierto y el límite derecho cerrado (de 0 a 5, de 6 a 10):
```
cut(datos$variable, c(0,5,10,15,20,25,30))
```

Cortar datos en una secuencia fija:
```
cut(datos$variable, seq(0, 30, 5))
```
Indica que empieza en 0 y termina en 30 en intervalos de 5 en 5.

#### Reordenar factores

```
mutate(ocupación_CIUO = forcats::fct_relevel(ocupación_CIUO, c("Estudiantes", "Sin empleo", "Sin información", "Jubilados/as", "Otras"), after = Inf)) %>%
```

```
datos <- datos %>%
mutate(variable = forcats::fct_relevel(variable, "categoria1", "categoria2", "categoria3"))
```
###### Reordenar factor según otra variable
Por ejemplo, reordenar una variable categórica a partir de los valores de una variable numérica:
```
ggplot(datos, aes(x = forcats::fct_reorder(variable, referencia), y=...
```
Según otra pero en descendiente:
```
mutate(Genre = forcats::fct_reorder(Genre, n, .desc = TRUE))
```


###### Invertir el orden de un factor
```
iris$Species <- factor(iris$Species, levels = rev(levels(iris$Species)))
```

```
datos %>%
	mutate(variable = forcats::fct_rev(variable))
```
###### Especificar un orden a un facyor
Obtener los niveles primero: `levels(var)`

```
datos1 <- datos %>%
    mutate(variable1 = forcats::fct_relevel(variable1,
                 "Entirely internal", 
                 "More internal than external",
                 "Approximately half internal",
                 "More external than internal", 
                 "Entirely external"))
```

```
eme2<- eme2 %>%
	mutate(tramo_ingresos = forcats::fct_relevel(tramo_ingresos, "Entre 0 y $193.000","Entre $193.001 y $ 375.000","Entre $ 375.001 y $600.000","Entre $600.001 y $1.125.000","Entre $1.125.001 y $2.500.000","Entre $ 2.500.001 y $ 4.500.000","Entre $4.500.001 y $10.000.000","Entre $10.000.001 y $20.000.000","Entre $20.000.001 y $50.000.000","Entre $50.000.001 y $200.000.000"))
```

Mover un nivel al final:
```
multiple_choice_responses %>%
    mutate(FormalEducation = forcats::fct_relevel(FormalEducation, "I prefer not to answer", after = Inf)) %>%

```

Mover un nivel a una posición:
```
multiple_choice_responses %>%
    mutate(FormalEducation = forcats::fct_relevel(FormalEducation, "Doctoral degree", after = 5))
```
#### Crear columna que tenga los valores de una categoría de una columna en formato long

Para repetir en una columna un dato específico a una categoría de agrupación (por ejemplo, el género en una variable “género”:
```
mutate(mujer = porcentaje[sexo == "Mujer"]) %>%
```

Por ejemplo, para añadir barras de brecha:
```
casen %>%
    filter(region=="Región de Tarapacá") %>%
    rename(trabajan=o1) %>%
    group_by(sexo, comuna, trabajan) %>%
    summarise(cantidad=n()) %>%
    mutate(porcentaje = cantidad/sum(cantidad)) %>%
    group_by(comuna) %>%
    mutate(diferencia = porcentaje[sexo == "Mujer"] - porcentaje) %>%
    group_by(comuna, trabajan) %>%
    mutate(mujer = porcentaje[sexo == "Mujer"]) %>%
    mutate(hombre = porcentaje[sexo == "Hombre"]) %>%
    filter(trabajan=="Sí") %>%
    ##filter(!is.na(trabajan)) %>%
    ##graficar
    ggplot(aes(fct_reorder(str_wrap(comuna, 4), diferencia), porcentaje, fill=sexo)) +
    ##facet_wrap(~comuna, strip.position = "bottom") +
    geom_col(position="dodge", width=0.8) +
    geom_text(aes(label = percent(porcentaje)), position = position_dodge2(width=0.8), 
              hjust = 1.2, vjust=0.5, size=3.5, angle=90, color="white") +
    geom_linerange(aes(ymin = mujer, ymax = hombre, linetype="Brecha\nde género"), size=3, col="##DF1A57")
```
## Estructura de datos
Instrucciones y ejemplos para realizar transformaciones de la estructura de los datos.

#### Desordenar una base
```
datos %>%
    mutate(desorden = sample(1:nrow(datos))) %>%
    arrange(desorden)
```
#### Convertir a tidy
Los datos _tidy_ cumplen lo siguiente:
- Las observaciones son filas
- Las variables son columnas
- Existe una sola unidad observacional por tabla
- La primera fila corresponde a nombres de variables, no valores
Para convertis a tidy se usa `gather`:
```
datos_untidy %>%
gather(key="columna_key", value="valores", columna_1:columna_3)
```
![](DraggedImage.jpeg)
![](DraggedImage-1.jpeg)
![](DraggedImage-2.jpeg)

```
datos_long <- gather(datos, key, value, -year)
```
   - `data: data set
	`- `key`: nueva columna que contiene folios o valores
- `value`: nueva columna que expresa el valor
- `-…`: nombre de las columnas a ignorar (que se va a mantener)
- `na.rm=TRUE` eliminar casos missing.

Ejemplos:
```
weather2 <- gather(weather, day, value, X1:X31, na.rm = TRUE)
```

```
tidy_ratings <- ratings %>%
    ## Gather and convert episode to factor
	gather(key = "episode", value = "viewers_7day", -series, 
           factor_key = TRUE, na.rm = TRUE)
```

```
## Add column with unique ids (don't need to change)
iris$Flower <- 1:nrow(iris)

## Fill in the ___ to produce to the correct iris.wide dataset
iris.wide <- iris %>%
  gather(key, value, -Species, -Flower) %>%
  separate(key, c("Part", "Measure"), "\\.") %>%
  spread("Measure", value)
```

Use `gather()` to move from `fish.species` to a tidy data frame, `fish.tidy`. This data frame should have three columns: Year (int), Species (factor) and Capture (int).
`gather()` takes four arguments: the _original data frame_ (fish.species), the name of the _key column_ (Species), the name of the _value column_ (Capture) and the name of the _grouping variable,_ with a minus in front (-Year). They can all be specified as object names (i.e. no " "):
```
fish.tidy <- gather(fish.species, Species, Capture, -Year)
```


#### Tidyr 1.0

Los siguientes son equivalentes:
```
gather(key="título", value="cantidad", ingresos_2016:gastos_2017) %>%

pivot_longer(ingresos_2016:gastos_2017, names_to = "título", values_to = "cantidad")
```

Otro ejemplo:
```
  pivot_longer(cols = starts_with("total"), names_to = "tipo", values_to = "cantidad") %>%
```
#### Trasponer data frame

Transponer, para que las fila sean columnas y las columnas filas
```
library(reshape)
matricula2 <- t(matricula)
```


```
indicadores %>%
gather(key = indicador, value = value, 2:ncol(indicadores)) %>% 
    spread_(key = names(indicadores)[1],value = 'value')
```
#### Unir dos columnas

```
unite(datos, nuevacolumna, col1, col2)
```
Agregar el argumento `, sep = ", "` para determinar el separador para la nueva columna, que por defecto es guión bajo.
El argumento `remove = TRUE` elimina las columnas originales luego de unirlas.

Sirve para cuando una base de datos viene con una misma variable con cada categoría de respuestas en texto pero en columnas distintas. Así se unen las columnas en una sola.

#### Separar dos columnas
Separar una columna que contiene dos variables en dos columnas.
```
require(tidyr)
separate(datos, variable_pegada, into=c("variable", "pegada"))
```
Separa automáticamente en símbolos, de lo contrario hay que especificarle.

Se puede agregar un `sep` para determinar el separador.

```
separate(census_long3, yr_month, c("year", "month"))
```

```
datos_untidy %>%
separate(col=spice, into=c("spice","order"))
```

Agregar `convert=TRUE`...

Se pueden botar el resto de las columnas:
```
separate(episode, into = "episode", extra = "drop"
```



Separar celdas que contengan varios casos en filas distintas:
```
separate_rows(MESAS, sep = "-", convert = FALSE)
```
#### Convertir a wide
```
spread(datos, var1, var2)
```
En caso de que hayan dos valores de una variable que se correspondan con un solo valor de otra variable (por ejemplo, dos pruebas en un solo curso).

Although it may not be immediately obvious, if we treat the values in the type column as variables and create a separate column for each of them, we can set things straight. To do this, we use the spread() function. Run the following code to see for yourself:

```
spread(pets, type, num)
```
The result shows the exact same information in a much clearer way! Notice that the spread() function took in three arguments. The first argument takes the name of your messy dataset (pets), the second argument takes the name of the column to spread into new columns (type), and the third argument takes the column that contains the value with which to fill in the newly spread out columns (num).

#### Anidar datos
Cambia el data frame a una sola columna, produciendo una lista que contiene un data frame para cada caso.

```
datos %>%
	nest(-country)
```
La variable indicada es la que va a quedar en el data frame, mientras que el resto se anidan.

Obtener un dato de la lista:
```
## All countries are nested besides country
nested <- by_year_country %>%
  nest(-country)

## Print the nested data for Brazil
nested$data[[7]]
```

Para desanidar:
```
## All countries are nested besides country
nested <- by_year_country %>%
  nest(-country)

## Unnest the data column to return it to its original form
nested %>%
unnest(data)
```


###### Aplicar una función a todos los elementos de una lista
```
map()
```
Se usa `.` para representar cada uno de los ítems en la columna data.

## Exportación de datos
Exportar datos

#### Exportar tablas

```
library(htmlTable)

tabla <- comunas %>%
  mutate(n_pueblos_originarios = pueblos_originarios * poblacion) %>%
  mutate(n_pueblos_originarios = round(n_pueblos_originarios, digits= 0)) %>%
  group_by(comuna) %>%
  select(poblacion, pueblos_originarios, n_pueblos_originarios)

htmlTable(tabla)
```
Luego copiar y pegar en Pages y formatearlo

#### Exportar a Excel

```
writexl::write_xl(datos, file = "ruta/archivo.xlsx")
```
##ar data frame

```ile="objeto.Rdata")
```
