#### ggplot2
Se basa en siete componentes principales:
- DATA FRAME which contains the data you're trying to plot. 
- AESTHETIC MAPPINGS determine how data are mapped to color, size, etc. 
- The GEOMS (geometric objects) are what you see in the plot (points, lines, shapes) 
- FACETS are the panels used in conditional plots. 
- STATS are statistical transformations such as binning, quantiles, and smoothing which ggplot2 applies to the data. 
- SCALES show what coding an aesthetic map uses (for example, male = red, female = blue). 
- plots are depicted on a COORDINATE SYSTEM. When you use qplot these were taken care of for you.

```
g+geom_point()+geom_smooth(method="lm")+facet_grid(.~drv)
```
Donde `g` es un "objeto gráfico" de ggplot guardado como variable:
```
g<-ggplot(mpg,aes(displ,hwy))
```
El primer valor `mpg` es el conjunto de datos, y luego, dentro de la función "estética" `aes`, van como argumento las dos variables que queremos graficar.
`geom_point()` es una capa que al estar vacía grafica un gráfico de dispersión.
`geom_smooth()` con su argumento vacío grafica el intervalo de confianza. Pero al llevar `lm` como argumento, grafica una línea de regresión.
`facet_grid(.~drv)` desagrega los datos en facetas según la variable indicada.
Otros elementos, como títulos `ggtitle` y etiquetas `ylab` `xlab` se añaden como suma.

Ejemplos:
```
g+geom_point(aes(color=drv))+labs(title="Swirl Rules!")+labs(x="Displacement",y="Hwy Mileage")
```

```
g+geom_point(aes(color=drv), size=2,alpha=1/2)+geom_smooth(size=4,linetype=3,method="lm",se=FALSE)
```
`alpha` grafica los puntos del plot transparentes.
`linetype` vuelve la línea en intermitente.
`se` elimina el intervalo de confianza.

```
g+geom_point(aes(color=drv))+theme_bw(base_family="Times")
```


Graficar variable como factor (para que sólo aparezcan en el eje los valores presentes): `factor(variable)`

###### Gráfico con múltiples instancias según variable
Se usa la capa `facet`:
```
ggplot(iris.tidy, aes(x = Species, y = Value, col = Part)) +
  geom_jitter() +
  facet_grid(. ~ Measure)
```
## Tipos de gráfico
Ejemplos de los distintos tipos de gráficos que se pueden hacer en R

#### Barras
```
geom_bar
```

```
ggplot(Tarapaca, aes(y=Nacimientos, x=Año)) + geom_bar(stat="identity")
```

```
ggplot(df2, aes(measurements, value)) + 
  geom_line(aes(colour = samples, group = samples))
```

Barras una sobre otra:
```
cyl.am <- ggplot(mtcars, aes(x = factor(cyl), fill = factor(am)))

## Add geom (position = "stack" by default)
cyl.am + 
geom_bar(position = "stack")
```

Barras encima de otras pero llenando el eje vertical:
```
## Fill - show proportion
cyl.am + 
  geom_bar(position = "fill")  
```

Barras lado a lado:
```
## Dodging - principles of similarity and proximity
cyl.am +
  geom_bar(position = "dodge") 
```

Barras lado a lado pero superpuestas:
```
ggplot(mtcars, aes(x = cyl, fill = am)) +
  geom_bar(position = "dodge")

## 2 - Define posn_d with position_dodge()
posn_d <- position_dodge(width=0.2)

## 3 - Change the position argument to posn_d
ggplot(mtcars, aes(x = cyl, fill = am)) +
  geom_bar(position = posn_d)


## 4 - Use posn_d as position and adjust alpha to 0.6
ggplot(mtcars, aes(x = cyl, fill = am)) +
  geom_bar(position = posn_d, alpha=0.6)
```

Para definir el color de las barras, hay que cambiar `col` y `fill`:
```
m <- ggplot(mtcars, aes(x = cyl,y = wt, col = am, fill = am))
```

De barras con porcentaje para variables categóricas o factores:
```
ggplot(eme2, aes(x = sexo)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill=sexo)) +
  scale_y_continuous(labels=scales::percent) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), 
                stat = "count", 
                vjust = 4,
                color="white")
```

###### Barra "dinamita" (con marcas de error)
```
## Base layers
m <- ggplot(mtcars, aes(x = cyl, y = wt))

## Draw dynamite plot
m +
  stat_summary(fun.y = mean, geom = "bar", fill = "skyblue") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1)
```

```
## Base layers
m <- ggplot(mtcars, aes(x = cyl, y = wt))

## Set your dodge posn manually
posn.d <- position_dodge(0.9)

## Plot 3: Redraw dynamite plot
m +
  stat_summary(fun.y = mean, geom = "bar", position = posn.d) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1, position = posn.d)
```

Gráfico de barras con faceta:
```
ggplot(email, aes(x= number)) +
  geom_bar() +
  facet_wrap(.~spam)
```
#### Dispersión
Útil para agregar sobre boxplots.

```
+ geom_jitter(color="steelblue", alpha=0.3) +
    geom_boxplot(alpha=0)
```
#### Torta
Representa la distribución de categorías dentro de un todo.

Gráfico de torta:
```
## Create a stacked bar plot: wide.bar
wide.bar <- ggplot(mtcars, aes(x = 1, fill = cyl)) +
              geom_bar()

## Convert wide.bar to pie chart
wide.bar +
  coord_polar(theta = "y")
```

Gráfico de anillo:
```
## Create stacked bar plot: thin.bar
thin.bar <- ggplot(mtcars, aes(x = 1, fill = cyl)) +
              geom_bar(width = 0.1) +
              scale_x_continuous(limits = c(0.5,1.5))

## Convert thin.bar to "ring" type pie chart
thin.bar + 
  coord_polar(theta = "y")
```

Definir los colores:
```
scale_fill_manual(values=c("##320D70", "##DF1A58"))
```

Torta de variable categórica o factores con porcentajes:
```
eme_genero2_gg <- eme2 %>%
  ggplot(aes(x=1, fill=sexo)) + 
  geom_bar(position="stack",width = 0.5, aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(limits = c(0.5,1.3)) +
  scale_fill_manual(values=c("##320D70", "##DF1A58")) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
              label = scales::percent((..count..)/sum(..count..))), 
              stat = "count", 
              vjust = -9,
              color="white") +
    theme(axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      rect = element_blank(),
      panel.grid = element_blank(),
      legend.title = element_blank(),
      legend.direction = "horizontal",
      legend.position = "bottom") +
    labs(title="Microemprendedores según género") +
  coord_polar(theta = "y")
```

Ejemplo:
```
eme_genero_gg <- eme2 %>%
    ggplot(aes(x = 1, sexo, fill = sexo)) +
   geom_col() +
    scale_fill_manual(values=c("##320D70", "##DF1A58")) +
    coord_polar(theta = "y") +
    theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank(),
  legend.direction = "horizontal",
legend.position = "bottom") +
  labs(title="Microemprendedores según género")

eme_genero_gg

```


Ejemplo:
```
extranjeros_verano <- turismo %>%
    filter(periodo=="Verano") %>%
    mutate(extranjeros = case_when(nacionalidad == "Chile" ~ "Chilena",
                                   TRUE ~ "Extranjera")) %>%
    group_by(extranjeros) %>%
    summarize(cantidad = n()) %>%
    ggplot(aes(x=1, y=cantidad, fill=extranjeros)) +
    geom_col() +
    coord_polar(theta = "y", start=0, direction = -1) +
    scale_fill_discrete(name = "Nacionalidad") +
    theme(axis.text = element_blank(), axis.title = element_blank()) +
    ##geom_text(aes(x=1, y = cumsum(cantidad) - cantidad/3, label = percent(cantidad/sum(cantidad)))) +
    geom_text(aes(label = percent(cantidad/sum(cantidad), accuracy=1)), position = position_stack(vjust = 0.5)) +
    labs(subtitle="Verano")
```
#### Densidad

```
geom_density
```

El gráfico de densidad  es una línea curva de la silueta del histograma:
```
ggplot(common_cyl, aes(x = city_mpg, fill = as.factor(ncyl))) +
  geom_density(alpha = .3)
```

La desviación estándar `bw` suaviza el histograma.

```
ggplot(truck_speeding, aes(x = hour_of_day)) +
    ## switch to density with bin width of 1.5, keep fill 
    geom_density(fill = 'steelblue', bw=1.5) +
    ## add a subtitle stating binwidth
    labs(title = 'Citations by hour', subtitle= "Gaussian kernel SD= 1.5")
```


Parámetros:
- `bw` - the smoothing bandwidth to be used, see ?density for details
- `adjust` - adjustment of the bandwidth, see density for details
- `kernel` - kernel used for density estimation, defined as
	- "g" = gaussian
	- "r" = rectangular
	- "t" = triangular
	- "e" = epanechnikov
	- "b" = biweight
	- "c" = cosine
	- "o" = optcosine

#### Boxplot

```
ggplot(aes(x = 1, y = city_mpg)) +
  geom_boxplot()
```

Se puede poner 1 en el eje x para mostrar solo un boxplot.

El ancho de cada caja puede depender de la cantidad de casos:
```
geom_boxplot(varwidth=TRUE)
```
#### Curvas y modelos lineales
Agregar líneas de regresión al gráfico

```
geom_smooth
```

Curva Loess:
```
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_smooth()
```
Cambiar

Línea de regresión:
```
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) ## línea de regresión
```


Ejemplo:
```
ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  stat_smooth(method = "lm", se = FALSE, aes(group=1))
```

```
myColors <- c(brewer.pal(3, "Dark2"), "black")

ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE, span = 0.7) +
  stat_smooth(method = "loess", 
              aes(group = 1, col="All"), 
              se = FALSE, span = 0.7) +
  scale_color_manual("Cylinders", values=myColors)
```
#### Barras redondeadas


```
library(ggchicklet)

ggplot(count(mtcars, cyl), aes(x = cyl, y = n)) +
  geom_chicklet(radius = grid::unit(15, 'mm'), fill = 'skyblue') +
  theme_minimal()
```
#### Nubes de palabras

[https://medium.com/@rohitnair\_94843/analysis-of-twitter-data-using-r-part-2-word-cloud-dd423af1b2c6](https://medium.com/@rohitnair_94843/analysis-of-twitter-data-using-r-part-2-word-cloud-dd423af1b2c6)

[https://www.r-bloggers.com/awesome-twitter-word-clouds-in-r/](https://www.r-bloggers.com/awesome-twitter-word-clouds-in-r/)

#### Convertir a 3D

[https://www.rayshader.com](https://www.rayshader.com)

#### Estadísticas en gráficos

Curva normal:
```
+ stat_function
```


```
## Plot 2: Mean and SD - the easy way
wt.cyl.am +
stat_summary(fun.data=mean_sdl, fun.args=list(mult=1), position=posn.d)


## Plot 3: Mean and 95% CI - the easy way
wt.cyl.am +
stat_summary(fun.data=mean_cl_normal, position=posn.d)


## Plot 4: Mean and SD - with T-tipped error bars - fill in ___
wt.cyl.am +
  stat_summary(geom = "point", fun.y = mean,
               position = posn.d) +
  stat_summary(geom = "errorbar", fun.data = mean_sdl,
               position = posn.d, fun.args = list(mult = 1), width = 0.1)
```
#### Crestas
```
library(ggridges)
p + geom_density_ridges()
```

```
md_speeding %>% 
    mutate(day_of_week = factor(day_of_week, levels = c("Mon","Tues","Wed","Thu","Fri","Sat","Sun") )) %>% 
    ggplot(aes( x = percentage_over_limit, y = day_of_week)) + 
    ## make ridgeline densities a bit see-through with alpha = 0.7
    geom_density_ridges(bandwidth = 3.5, alpha=0.7) +
    ## set expand values to c(0,0)
    scale_x_continuous(limits = c(0,150), expand=c(0,0)) +
    labs(subtitle = 'Guassian kernel SD = 3.5') +
    ## remove y axis ticks
    theme(axis.ticks.y=element_blank())
```
#### Enjambre
Alternativa al boxplot. Agrega dispersión de forma inteligente, con los puntos lo más cercanos al eje. 

```
library(ggbeeswarm)
p + geom_beeswarm()
```

Muestra todos los puntos de datos y la forma de la distribución.
Funciona mal con demasiados datos.

Cambiar el tamaño de los puntos:
```
geom_beeswarm(cex=0.5) +
```
#### Violín
Alternativa al boxplot. Muestra la distirbución de forma simétrica. 

```
geom_violin()
```


Funciona bien con muchos datos, pero no muestra cada dato individualmente.

```
geom_violin(bw = 2.5) +
```
#### Líneas
```
geom_line
```

```
ggplot(economics, aes(x = date, y = unemploy/pop)) +
geom_line()
```

```
ggplot(fish.tidy, aes(x = Year, y = Capture, color=Species)) + geom_line()
```

```
ggplot(ChickWeight, aes(x = Time, y = weight, col=Diet)) +
  geom_line(aes(group = Chick), alpha=0.3) +
  geom_smooth(lwd=2, se=FALSE)
```

Seis mapas de líneas horizontalmente:
```
ggplot(barley, aes(x=year, y=yield, col=variety, group=variety)) +
geom_line() +
facet_wrap( ~ site, nrow= 1)
```

Planos de líneas de promedios con desviación estándar como barritas transparentes:
```
## Create overlapping ribbon plot from scratch
ggplot(barley, aes(year, yield, col= site, group=site, fill=site)) +
stat_summary(fun.y=mean, geom="line") +
stat_summary(fun.data=mean_sdl, fun.args=list(mult=1), geom="ribbon", col=NA, alpha=0.1)
```
#### Histograma

```
geom_histogram
```

A mayor `binwidth`, más gruesas las líneas. 
`bins` es el número de barras.
`center` hace que la barra central esté en un punto determinado.

```
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(aes(y=..density..), binwidth=1, bins=40, fill="##377EB8")
```

```
ggplot(adult, aes(SRAGE_P, fill=factor(RBMI))) + geom_histogram(binwidth=1)
```

```
qplot(var1,data=datos,fill=var2)
```
Con color

```
qplot(var1,var2,data=datos,facets=.~var3)
```
Tres histogramas horizontalmente, divididos por variable 3.

```
qplot(var1,data=datos,facets=var2~.,binwidth=2)
```
Tres histogramas, uno encima del otro.

Polígono de frecuencias:
```
ggplot(mtcars, aes(mpg, color=cyl)) +
  geom_freqpoly(binwidth=1)
```

Alfombra de casos:
```
+ geom_rug
```
#### Dumbell

```
ggplot(first_last, aes(x = series, y = viewers, color = episode)) +
  geom_point() + ## keep
  geom_line(aes(group = series)) + ## keep
  coord_flip() ## keep
```
#### Mapas de calor

```
geom_tile
```

```
## Create color palette
myColors <- brewer.pal(9, "Reds")

## Build the heat map from scratch
ggplot(barley, aes(x = year, y = variety, fill = yield)) +
  geom_tile() + ## Geom layer
  facet_wrap( ~ site, ncol = 1) + ## Facet layer
  scale_fill_gradientn(colors = myColors) ## Adjust colors
```
#### Splom (scatterplot matrix)
```
pairs(datos)
```

```
library(PerformanceAnalytics)
chart.Correlation(iris[1:4])
```

```
library(GGAlly)
ggpairs(datos[1:9])
```

#### Matriz de correlación

#### Coordenadas paralelas
```
ggparcoord(datos, columns =1:4, groupColumn=5, scale="globalminmax", order="anyClass", alphaLines=0.4)
```
#### Mapas

Mapoteca congreso nacional:
[https://www.bcn.cl/siit/mapas\_vectoriales/index\_html](https://www.bcn.cl/siit/mapas_vectoriales/index_html)

[http://guillermoacuna.blogspot.com/2017/02/como-hacer-un-mapa-de-chile-en-r.html](http://guillermoacuna.blogspot.com/2017/02/como-hacer-un-mapa-de-chile-en-r.html)

[https://geocompr.robinlovelace.net/adv-map.html](https://geocompr.robinlovelace.net/adv-map.html)

```
  tm_fill() +
  tm_borders() 
```


[https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html](https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html)

#### Mapa de densidad
Mapa lineal de densidad, como un mapa topográfico:
```
geom_density_2d()
```

Mapa con degradado de calor:
```
geom_density_2d(geom="tile", aes(fill=..density..), contour=FALSE)
```

Mapa de círculos:
```
geom_density_2d(geom="point", aes(fill=..density..), n=20, contour=FALSE) + 
scale_size(range=c(0,9))
```


Agregar línea de medianas:
```
p+geom_vline(data=mu, aes(xintercept=grp.mean, color=sex),
             linetype="dashed")
```
#### Scatter
Permiten explorar preguntas e hipótesis iniciales, sugerir estrategias para siguientes pasos, y resumir los datos gráficamente para destacar características generales.

```
geom_point
```
Requiere especificar x e y.

```
ggplot(df, aes(x = wt, y = mpg)) + geom_point()
```

Cambiar color y forma:
```
b + geom_point(color = "##00AFBB", size = 2, shape = 23)
```

Cambiar forma de los puntos según otra variable:
```
b + geom_point(aes(shape = cyl))
```

Cambiar forma y color de los puntos según otras variables:  
```
b + geom_point(aes(shape = cyl, color = cyl))	
```
Para colores personalizados: `+ scale_color_manual(values = c("##00AFBB", "##E7B800", "##FC4E07")`

Texto del eje x: rotar y mostrar todos los valores:
```
+ scale_x_continuous("Año", labels = as.character(Año), breaks = Año)
```

Definir mínimos y máximos:
```
+ coord_cartesian(ylim = c(800, 5500)) ## especificar mínimos y máximos del eje y
```

Añadir dispersión a los puntos:
```
jitter can be 1) an argument in geom_point(position = 'jitter'), 2) a geom itself, geom_jitter(), or 3) a position function, position_jitter(0.1)
```

---- 

Ejemplo de scatter:
```
ggplot(Vocab, aes(x=education, y=vocabulary)) + geom_jitter(alpha=0.2, shape=1)
```

Ejemplo:
```
gD<- ggplot(Tarapaca, aes(x = Año, y = Defunciones))

gD+ geom_smooth(se=FALSE, alpha=10, size=0.5, color="##C19BDE") + ##línea de tendencia, falso para no mostrar intervalos
    geom_point(aes(size = TasaMortalidad), alpha = 0.7, color = "##8A1CDD") + ##variable graficada como "size", con color y transparencia
    scale_size(range = c(0.5, 10)) + ##rango del tamaño de círculos
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + ##ángulo del texto del eje x
    scale_x_continuous("Año", labels = as.character(Año), breaks = Año) + ##etiquetas del eje x
    theme(panel.grid.major = element_line(Año, color = "white"), ## líneas del fondo
          panel.grid.minor = element_blank(), ## borrar líneas menores
          panel.background = element_rect(fill = "##EDE6F2"), ## color de fondo
          legend.key = element_rect(fill = "##EDE6F2")) + ## color de fondo de leyenda
    labs(size="Tasa de mortalidad") + ##título del elemento "size"
    coord_cartesian(ylim = c(800, 5500)) ## especificar mínimos y máximos del eje y
```

Ejemplo de scatter con dos variables:
```
gX <- ggplot(Tarapaca)
gX + geom_point(aes(y=Nacimientos , x=Año, size=TasaNatalidad, colour=TasaFecundidad), alpha = 0.7, color = "##DD4814") + 
    geom_point(aes(y = Defunciones, x=Año, size=TasaMortalidad), alpha = 0.7, color = "##8A1CDD") + 
    ##geom_point(aes(y = , x=Año, size=Nacimientos), alpha = 0.7, color = "##DD4814")
    scale_size(range = c(2, 15)) + ##rango del tamaño de círculos
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + ##ángulo del texto del eje x
    scale_x_continuous("Año", labels = as.character(Año), breaks = Año) + ##etiquetas del eje x
    theme(panel.grid.major = element_line(Año, color = "white"), ## líneas del fondo
          panel.grid.minor = element_blank(), ## borrar líneas menores
          panel.background = element_rect(fill = "##EDE6F2"), ## color de fondo
          legend.key = element_rect(fill = "##EDE6F2")) + ## color de fondo de leyenda
    labs(size="Tasa de mortalidad") ##título del elemento "size"
```

Ejemplo de qplot:
```
qplot(wt, mpg, data = mtcars)
```
#### Gráficos con iconos o logotipos

Instalar iconos de símbolos como FontAwesome o IonIcons

Luego graficar con `geom_text` usando el código del glifo a utilizar:
```
geom_text(label="\uF236", size = 14, family = 'FontAwesome', col="##1C2366") +
```
#### Waffle

```
disease_counts <- who_disease %>%
	group_by(disease) %>%
	summarise(total_cases = sum(cases)) %>% 
	mutate(percent = round(total_cases/sum(total_cases)*100))

## Create an array of rounded percentages for diseases.
case_counts <- disease_counts$percent
## Name the percentage array
names(case_counts) <- disease_counts$disease

## Pass case_counts vector to the waffle function to plot
waffle(case_counts)
```


```
parts <- c(80, 30, 20, 10)
chart <- waffle(parts, rows=8)
## print(chart)

## library(extrafont)
## waffle(parts, rows=8, use_glyph="shield")

parts <- c(One=80, Two=30, Three=20, Four=10)
chart <- waffle(parts, rows=8)
## print(chart)

```

```
library(waffle)

parts <- c('TRUE' = 3, 'FALSE' = 77)
p <- waffle(parts, rows = 8, colors = c("black", "grey70"))
p
```

Ejemplo:
```
originarios_total <- read_excel("/Users/rndzvs/RStudio/Tarapacá\ R/Indígenas.xlsx",
                                sheet="Región")

originarios_total

originarios_total_tidy <- originarios_total %>%
  gather(`Pueblo originario`, Porcentaje, Aimara:`Sin pertenencia`) %>%
  mutate_if(is.character, as.factor)

originarios_total_tidy

## Explorar porcentajes de cada pueblo en total
originarios_total_tidy %>%
  group_by(`Pueblo originario`) %>%
  ##filter(`Pueblo originario`=="Aimara") %>%
  summarize(percent(Porcentaje/10))

originarios_desagregado_tidy

pueblos_originarios_vector <- c(Aimara=29.1, Diaguita=1.44, Mapuche=2.18, Quechua=3.24, `Otra etnia`=3.24, `Sin pertenencia`=62.5)

pueblos_originarios_waffle <- waffle(pueblos_originarios_vector, 
                                     rows=9, 
                                     size=2)

pueblos_originarios_waffle_gg <- pueblos_originarios_waffle + 
  labs(title="Pertenencia a pueblos originarios", 
       subtitle="Cada cuadro representa a 1 de cada 100 tarapaqueños") +
  scale_fill_manual(values=c("##1A9D75", ##aymara
                             "##69A500", ##diaguita
                             "##7370B5", ##mapuche
                             "##DB6200", ##quechua
                             "##E8348B", ##otros
                             "##AFAFAF", ##sin
                             "##FFFFFF" ##fondo
                             )) +
  tema_comparaciones2 +
  theme(legend.title = element_blank(),
        legend.position = "bottom")

pueblos_originarios_waffle_gg
```
#### Voronoi

[https://rspatial\_es.gitlab.io/blog/2020-08-02-diagrama-voronoi-r-ggplot/](https://rspatial_es.gitlab.io/blog/2020-08-02-diagrama-voronoi-r-ggplot/)

#### Densidad 2D

Densidad con líneas concéntricas:
```
## Base layers
p <- ggplot(faithful, aes(x = waiting, y = eruptions)) +
  scale_y_continuous(limits = c(1, 5.5), expand = c(0, 0)) +
  scale_x_continuous(limits = c(40, 100), expand = c(0, 0)) +
  coord_fixed(60 / 4.5)

## 1 - Use geom_density_2d()
p + geom_density_2d()

## 2 - Use stat_density_2d() with arguments
p + stat_density_2d(aes(col = ..level..), h = c(5, 0.5))
```

Densidad con gradiente:
```
## Load in the viridis package
library(viridis)

## Add viridis color scale
ggplot(faithful, aes(x = waiting, y = eruptions)) +
  scale_y_continuous(limits = c(1, 5.5), expand = c(0,0)) +
  scale_x_continuous(limits = c(40, 100), expand = c(0,0)) +
  coord_fixed(60/4.5) +
  stat_density_2d(geom = "tile", aes(fill = ..density..), h=c(5,.5), contour = FALSE) +
  scale_fill_viridis()

```
#### Gráficos animados

```
library(gganimate)
library(tween)
library(ggplot2)
```

###### Animar según otra variable
```
p<- ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) +
	geom_point() + ##Gráfico de dispersión común
	transition_states(Species, ##Anima los puntos según variable
                    transition_length = 2,
                    state_length = 1) + 
	ease_aes('cubic-in-out') + ##suavizar animación
	ggtitle('Now showing {closest_state}',
          subtitle = 'Frame {frame} of {nframes}') ##Título con estados de la animación
```
`transition_states()` splits up plot data by a discrete variable and animates between the different states.
`ease_aes()` defines the velocity with which aesthetics change during an animation.

###### Animar transiciones de entrada y salida:
```
anim <- ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) + 
  geom_point(aes(colour = Species), size = 2) + 
  transition_states(Species,
                    transition_length = 2,
                    state_length = 1)
anim + 
  enter_fade() + 
  exit_shrink()
```
`enter` and `exit` functions are used to modify the aesthetics of appearing and disappearing data so that their entrance or exit may be animated.

###### Agregar título
Si es `transition_reveal`:
```
labs(x = "Años", y = "Nacimientos", title="Año {as.integer(frame_along)}") +
```

Si es `transition_states`:
```
labs(x = "Años", y = "Nacimientos", title="Año {closest_state}") +
```

```
ggtitle('Año {closest_state}') ##título de años
```

###### Exportar:
```
animate(gA_anim, nframes=300, fps=24, width=750, height=675, res=150, end_pause = 30)
anim_save("Animación2.gif", animation = last_animation())
```
`nframes` es la cantidad total de cuadros de la animación
`fps` son los cuadros por segundo de la animación
`end_pause` son cuadros de pausa al final de la animación
`rewind=TRUE` para rebobinar.

Ejemplo:
```
library(ggplot2)
library(gganimate)

p<- ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) +
    geom_point() + ##Gráfico de dispersión común
    transition_states(Species, transition_length = 2, state_length = 1) + ##Anima los puntos según variable
    ease_aes('cubic-in-out') + ##suavizar animación
    ggtitle('Now showing {closest_state}',
            subtitle = 'Frame {frame} of {nframes}') ##Título con estados de la animación

animate(p, nframes=300, fps=30) 
anim_save("Animación2.gif", animation = last_animation())
```


[https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/##show-preceding-frames-with-gradual-falloff](https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/##show-preceding-frames-with-gradual-falloff)

Regresión:
[http://varianceexplained.org/files/loess.html](http://varianceexplained.org/files/loess.html)



###### Gráficos de países que se van adelantando
- [https://towardsdatascience.com/create-animated-bar-charts-using-r-31d09e5841da](https://towardsdatascience.com/create-animated-bar-charts-using-r-31d09e5841da)
- [https://stackoverflow.com/questions/53162821/animated-sorted-bar-chart-with-bars-overtaking-each-other?source=post\_page---------------------------](https://stackoverflow.com/questions/53162821/animated-sorted-bar-chart-with-bars-overtaking-each-other?source=post_page---------------------------)

```
nata<-read.csv("nata.csv", sep=";")

names(nata)[2] <- "2000"
names(nata)[3] <- "2001"
names(nata)[4] <- "2002"
names(nata)[5] <- "2003"
names(nata)[6] <- "2004"
names(nata)[7] <- "2005"
names(nata)[8] <- "2006"
names(nata)[9] <- "2007"
names(nata)[10] <- "2008"
names(nata)[11] <- "2009"
names(nata)[12] <- "2010"
names(nata)[13] <- "2011"
names(nata)[14] <- "2012"
names(nata)[15] <- "2013"
names(nata)[16] <- "2014"
names(nata)[17] <- "2015"
names(nata)[18] <- "2016"

library(ggplot2)
library(gganimate)
library(tidyverse)
library(janitor)
library(scales)


#### Aplicar guía a mis datos
            
#### Cambiar formato de base de datos a "long format"
nata_tidy <- nata %>% 
      mutate_at(vars(contains("20")),as.numeric) %>% ##selecionar los años 
      gather(year,value,2:18) %>% ##elegir las columnas de los años
      janitor::clean_names() %>% 
      mutate(year = as.numeric(stringr::str_sub(year,1,4)))

write_csv(nata_tidy,"nata_tidy.csv")
View(nata_tidy)
View(nata)

#### We’re going to filter our dataset to retain only the top 10 countries for every given year

nata_tidy <- read_csv("nata_tidy.csv")
nata_formatted <- nata_tidy %>%
      group_by(year) %>%
      ## The * 1 makes it possible to have non-integer ranks while sliding
      mutate(rank = rank(-value),
             Value_rel = value/value[rank==1],
             Value_lbl = paste0(" ",round(value/1e9))) %>%
      group_by(region) %>% 
      filter(rank <=10) %>%
      ungroup()

View(nata_formatted)

#### Gráfico seguido de animación

staticplot = ggplot(nata_formatted, aes(rank, group = region, 
                                       fill = as.factor(region), 
                                       color = as.factor(region))) +
      geom_tile(aes(y = value/2,
                    height = value,
                    width = 0.9), alpha = 0.8, color = NA) +
      geom_text(aes(y = 0, label = paste(region, " ")), vjust = 0.2, hjust = 1) +
      geom_text(aes(y=value,label = paste(round(value,digits= 1)), hjust=-0.2)) +
      coord_flip(clip = "off", expand = FALSE) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_reverse() +
      guides(color = FALSE, fill = FALSE) +
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.grid.major.x = element_line( size=.1, color="grey" ),
            panel.grid.minor.x = element_line( size=.1, color="grey" ),
            plot.background=element_blank(),
            plot.margin = margin(1,1, 1, 4, "cm"))

animacion = staticplot + transition_states(year, transition_length = 2, state_length = 4) +
      view_follow(fixed_x = TRUE)  +
      labs(title = 'Tasa de natalidad anual: {closest_state}')

animate(animacion, nframes=640, fps=30, width=400, height=400, res=100, detail=2)
anim_save("Natalidad regiones.gif", animation = last_animation())
```
#### Cortar datos en intervalos
![](DraggedImage.jpeg)![](DraggedImage-1.jpeg)

```
## Plot object p
p <- ggplot(diamonds, aes(x = carat, y = price))

## Use cut_interval
p + geom_boxplot(aes(group = cut_interval(carat, n=10)))

## Use cut_number
p + geom_boxplot(aes(group = cut_number(carat, n=10)))

## Use cut_width
p + geom_boxplot(aes(group = cut_width(carat, width=0.25)))
```
#### Mapas
Graficar mapas en ggplot2

###### Graficar mapas de Chile
Paquete Chilemapas
[https://github.com/pachamaltese/chilemapas](https://github.com/pachamaltese/chilemapas)

###### Recortar mapa

```
mapa_cortado <- st_crop(mapa, 
	xmin = -20, xmax = 45,
	ymin = 30, ymax = 73)
```
###### Cambiar formato de coordenadas
Necesario cuando se usan dos mapas distintos en un solo gráfico y sus sistemas de coordenadas difieren.

Revisar el formato de coordenadas de los dos mapas:
```
mapa1$geometry
mapa2$geometry
```

Cambiar uno por el del otro:
```
mapa2$geometry <- st_transform(mapa2$geometry, 
	crs = 32719)
```
###### Acercarse a un mapa
```
mapa +
coord_sf(ylim=c(-30, -10),
		 xlim=c(-80, -60))
```
###### Importar shape

El archivo .shp tiene que estar dentro de la carpeta con el resto de archivos necesarios.

```
mapa <- sf::st_read("Catastro_Campamentos_2019/Catastro_Campamentos_2019/Catastro_Campamentos_2019.shp")    

```
###### Geocodificar

[https://www.jessesadler.com/post/geocoding-with-r/](https://www.jessesadler.com/post/geocoding-with-r/)

## Figuras y capas
Agregar figuras o capas extra a un gráfico

#### Agregar capas condicionales

```
switch=TRUE

datos %>%
ggplot(aes(x, y)) +
	{if(switch) geom_hline(yintercept = 15)}+
	geom_point()
```


```
geom_line(data = subset(afp_cotizantes2, región=="Iquique"), size=1) +
geom_line(data = subset(afp_cotizantes2, región!="Iquique"), size=0.5, show.legend = FALSE) +
```


[https://gist.github.com/jcheng5/3971908](https://gist.github.com/jcheng5/3971908)

#### Agregar barras de brecha:

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
#### Líneas verticales grises
Agregar linea

Verticales:
```
theme(panel.grid.major.x = element_line(color="gray90"))
```

Horizontales:
```
theme(panel.grid.major.y = element_line(color="gray90"))
```
#### Poner logo en esquina del gráfico

Importar logo: 
```
logo_tarapaca <- readPNG("logo_tarapaca_2.png")
```

```
coord_cartesian(clip="off") +
annotation_custom(grob = rasterGrob(logo_tarapaca_chico, width=unit(2,'cm'), hjust = 7, vjust=-0.3, x = unit(1,"npc"), y = unit(1,"npc")))
```
Ajustar `hjust` y `vjust`

Si el gráfico está horizontal:
```
coord_flip(clip="off") +
```
#### Insertar flechas en el gráfico

```
library(gridExtra)

 annotate(
        "segment",
        x=dates,
        xend=dates,
        y=prices-11,
        yend=prices-1,
        color="blue",
        arrow=arrow(length=unit(0.05,"npc")
        ))
```


Afuera del gráfico
```
annotate("segment", x=4.5, xend=3, y=-70, yend=-70, col="black", arrow=arrow(length=unit(0.3, "cm"))) +
scale_y_continuous(expand=c(0,0)) +
coord_cartesian(ylim = c(0, 700), clip="off") +
theme(plot.margin = unit(c(1,1,3,1), "lines"))
```
#### Agregar línea horizontal de promedio

```
geom_hline(yintercept = mean(Indcatotvalue), color="blue")
```
#### Graficar una única variable

```
ggplot(mammals[mammals$vore == "Insectivore", ], aes(x = sleep_total, fill = vore)) +
  geom_density(col = NA, alpha = 0.35) +
  scale_x_continuous(limits = c(0, 24)) +
  coord_cartesian(ylim = c(0, 0.3))
```
#### Insertar barras

```
geom_vline
geom_hline
```


```
geom_rect(data = recess,
         aes(xmin = begin, xmax = end, ymin = -Inf, ymax = +Inf),
         inherit.aes = FALSE, fill = "red", alpha = 0.2)
```

```
+ geom_vline(xintercept=0)
```
#### Guardar y aplicar capas

```
BMI_fill <- scale_fill_brewer("BMI Category", palette = "Reds")
```
Luego esta capa puede aplicarse con `+ BMI_fill`

Otro ejemplo:
```
fix_strips <- theme(strip.text.y = element_text(angle = 0, hjust = 0, vjust = 0.1, size = 14),
                    strip.background = element_blank(),
                    legend.position = "none")

ggplot(...) + fix_strips
```
#### Shapes

Las figuras `shape` de 21 a 25 tienen borde y relleno independientes.

## Texto
Operaciones que se pueden realizar sobre el texto de los gráficos

#### Insertar texto en el gráfico

```
annotate("text", x=2.8, y=-0.025, hjust=1, col="black", label="Menores ingresos") +
annotate("text", x=8.4, y=-0.025, hjust=0, col="black", label="Mayores ingresos") +
```
`hjust = 1` significa que el texto se posiciona desde el borde derecho, `0` depende del borde izquierdo.

Para ponerlas fuera del gráfico:
```
    coord_cartesian(clip = 'off') +
    theme(plot.margin = unit(c(1,1,3,1), "lines")) + ##extender área hacia abajo
```
#### Texto en barras

Crear variable que calcule el porcentaje:
```
basura <- basura %>% ##crear variable que mide el porcenaje
    mutate(Porcentaje = (`Toneladas al año` / sum(`Toneladas al año`) * 100))
```

```
+ geom_text(aes(label = percent(Porcentaje)), 
            color="gray25", size=3, hjust = -0.2) +
```

Cuando son barras stack:
```
geom_text(aes(label = percent(porcentaje)), color="gray25", size=3, hjust = 0.5, vjust=0.5, position="stack") +
```

Para centrar el número en las barras:
```
geom_text(aes(label = percent(porcentaje)), color="gray25", size=3, hjust = 0.5, vjust=0.5, position = position_stack(vjust = 0.5)) +
```


Para variables categóricas o factores:
```
geom_text(aes(y = ((..count..)/sum(..count..)), 
			label = scales::percent((..count..)/sum(..count..))), 
			stat = "count", hjust = -0.1, size=3, color="white") +
```

porcentaje total de las barras stacked con datos tidy:
```
stat_summary(fun.y = sum, aes(label = paste0(round(..y..*100, digits=1), "%"), group = Comuna), geom = "text", vjust=-0.5, size=3, col="gray25") +
```
Alternativa para pegar total
```
geom_text(
        aes(label = stat(y)), 
        stat = 'summary', fun.y = sum, vjust = -1
    ) +
```

Para barras dodge con `coord_flip`
```
turismo %>%
    filter(!is.na(edad)) %>%
    group_by(edad, genero, periodo) %>%
    summarize(cantidad=n()) %>%
    ggplot(aes(edad, cantidad)) +
    geom_col(aes(fill=genero), position = "dodge") + 
    coord_flip() + 
    geom_text(aes(label = cantidad), position = position_dodge2(width=1), 
              size=3.5,
              hjust=-0.5) +
    facet_wrap(~periodo)
```


Para permitir que el texto se salga del margen del gráfico hay que poner `clip = off` y  expandir el margen del gráfico con `plot.margin`:
```
coord_cartesian(clip = 'off') +
theme(plot.margin = unit(c(1,1,1,1),"cm")) ##top right bottom left
```
#### Mostrar texto en el gráfico según condición

Mostrar etiquetas sólo para puntos o segmentos superiores a una cantidad.
Usando `mutate`, las etiquetas que no cumplan la función quedan en blanco, manteniendo su posición:

```
    ##Texto grande
    geom_text(aes(label = ifelse(Cantidad > 1000, Cantidad, "")), 
              position = position_dodge2(width=0.8), 
              size=3, color="red", hjust=1.2) +
    ##Texto chico
    geom_text(aes(label = ifelse(Cantidad < 1000, Cantidad, "")), 
              position = position_dodge2(width=0.8), 
              size=3, color="green", hjust=-0.2) +
```

Poner sólo los porcentajes grandes en un gráfico de barras stacked:
```
geom_text(aes(label = ifelse(Porcentaje > 0.12, percent(Porcentaje, accuracy = 0.1), "")), 
              ##position = position_dodge2(width=0.8),
              position = position_stack(vjust = .5),
              size=2.5, color="white") +
```

Para un texto más complejo (con paste, porcentaje y frecuencia)
```
    ##Texto grande
    geom_text(aes(label = ifelse(porcentaje > .1, paste(percent(porcentaje, accuracy = 0.1), "-", format(cantidad, big.mark=".")), "")), 
              position = position_dodge2(width=0.7), 
              size=3, color="red", hjust=1.2) +
    ##Texto chico
    geom_text(aes(label = ifelse(porcentaje < .1, paste(percent(porcentaje, accuracy = 0.1), "-", format(cantidad, big.mark=".")), "")), 
              position = position_dodge2(width=0.7), 
              size=3, color="green", hjust=-0.2) +
```


#### Poner valores sobre las barras

```
geom_text(aes(label=variabley), vjust=1.5, hjust=0.5, col="white")
```

Para dodge:
```
geom_text(aes(label = percent(var1), 
                  y = var1 + .03), 
              position = position_dodge(0.9),
              vjust = 1)
```

Si se trata de un factor:
```
geom_text(stat='count', aes(label=..count..), vjust=0.5) +
```
#### Repeler etiquetas de los puntos

```
library(ggplot2)
library(ggrepel)

x = c(0.8846, 1.1554, 0.9317, 0.9703, 0.9053, 0.9454, 1.0146, 0.9012, 
      0.9055, 1.3307)
y = c(0.9828, 1.0329, 0.931, 1.3794, 0.9273, 0.9605, 1.0259, 0.9542, 
      0.9717, 0.9357)
z= c("a", "b", "c", "d", "e", "f", 
             "g", "h", "i", "j")


df <- data.frame(x = x, y = y, z = z)
ggplot(data = df, aes(x = x, y = y)) + theme_bw() + 
geom_text_repel(aes(label = z), 
box.padding = unit(0.45, "lines")) +
geom_point(colour = "green", size = 3)
```


```
ggplot(dt, 
       aes(x = one, y = two, color = diff_cat)) +
  geom_point() +
  geom_text_repel(data = . %>% 
                    mutate(label = ifelse(diff_cat %in% c("type_1", "type_2") & abs(diff) > 2,
                                          name, "")),
                  aes(label = label), 
                  box.padding = 1,
                  show.legend = FALSE) + ##this removes the 'a' from the legend
  coord_cartesian(xlim = c(-5, 5), ylim = c(-5, 5)) +
  theme_bw()
```
#### Convertir etiquetas a minúscula, mayúscula, titulares, o frase

```
stringr::str_to_upper(string, locale = "es")

stringr::str_to_lower(string, locale = "es")

stringr::str_to_title(string, locale = "es")

stringr::str_to_sentence(string, locale = "es")
```
#### Poner suma de los factores sobre una barra stacked

```
stat_summary(fun.y = sum,                          ##pone como texto la suma de los factores en cada barra
               aes(label = ..y.., group = Año), 
               geom = "text", vjust=-0.5, size=3, col="gray25") + 
```
#### Ajustar cita o referencia

Horizontalmente:
```
plot.caption = element_text(hjust = 1.4)) +
```
#### Mover título y subtítulo

Horizontalmente:
```
plot.title = element_text(hjust = 0.3),
plot.subtitle = element_text(hjust = 0.3),
```
#### Buscar y reemplazar palabras de las etiquetas

```
## buscar y reemplazar palabras para borrarlas de las etiquetas
eme2 <- eme2 %>% 
  mutate(tramo_ingresos = str_replace(tramo_ingresos, "Entre ", "")) %>%
  mutate(tramo_ingresos = str_replace(tramo_ingresos, "y", "a"))
```
#### Redondear cifras
```
round(variable, digits=1)
```

Ejemplo:
```
geom_text(aes(label = 
                      paste0(##pegar símbolo de porcentaje
                            round(Porcentaje, digits=1),##redondear cifra
                            "%")), 
              color="white", size=3.5, position=position_stack(vjust = 0.5)) +
```
#### Pegar signo de porcentaje
```
paste0(Porcentaje,"%")
```

Ejemplo:
```
geom_text(aes(label = 
                      paste0(##pegar símbolo de porcentaje
                            round(Porcentaje, digits=1),##redondear cifra
                            "%")), 
              color="white", size=3.5, position=position_stack(vjust = 0.5)) +
```
#### Cambiar etiquetas del gráfico
Cambiar títulos de los ejes
```
+ labs(x="Título del eje x", y="Título del eje y")
```

```
titled_plot <- initial_plot + 
    labs(title = "Hell Is Other People In A Pressurized Metal Tube",
         subtitle = "Percentage of 874 air-passenger respondents who said action is very or somewhat rude",
         caption = "Source: SurveyMonkey Audience", 
         x = "", 
         y = "") 
```
#### Poner nombre de barras dentro de las barras

Para que el texto o etiqueta del eje `y` o `x` aparezca dentro de la barra:

En gráficos con `coord_flip`:
```
geom_text(aes(label = str_to_title(variable), y=0.02), hjust=0, size = 3.2, col="white") +
theme(axis.text.y = element_blank()) ##ocultar eje
```

Sin `coord_flip`:
```
geom_text(aes(label = str_to_title(variable), y=0.02), angle=90, hjust=0, size = 3.2, col="white") +
theme(axis.text.x = element_blank())
```
#### Calcular porcentajes

Calcular porcentaje por categorías:
```
group_by(gastos_forma, periodo, extranjeros) %>%
    summarise(cantidad = n()) %>%
    mutate(porcentaje = cantidad/sum(cantidad)) %>% ##Porcentaje
```


Calcular porcentaje respecto del total:
```
group_by(gastos_forma, periodo, extranjeros) %>%
    summarise(cantidad = n()) %>%
	ungroup() %>%
    mutate(porcentaje = cantidad/sum(cantidad)) %>% ##Porcentaje
```
Hay que agregar un `ungroup`.

Agregar fila con suma de totales:
```
adorn_totals("row") %>%
```

Agregar columna con suma de totales:
```
adorn_totals("col", name = "Total") %>%
```
#### Poner texto al final del gráfico de líneas

```
    geom_text(aes(label = ifelse(Año==2018, 
                                 str_pad(number(media, accuracy = 1, big.mark="."), 20, side="right"),
                                 "")), 
              size=3, hjust=-0.15) +
```

Puede que sea necesario mover la leyenda hacia la derecha:
```
theme(legend.box.margin=margin(c(0,0,0,10))) +
```
#### Formatear números
Quitar decimales a un número que por ejemplo es una media:
```
round(Toneladas, digits = 2)
```
#### Añadir texto antes o después del nombre de la variable

```
ggplot(aes(x = paste("texto", variable)))
```

```
scale_x_continuous(labels = function(x) paste(x, "años"),
```
#### Formatear una fecha
Hay que tener instalado el `locale` necesario, en este caso el de español. Activar el `locale`:
```
Sys.setlocale(category = "LC_TIME", locale="es_ES.UTF-8") ##Meses en español
```

Luego las fechas usarán meses en castellano:
```
format(max(covid_comuna$Fecha), "%d de %B")
```
#### Aumentar el tamaño de todo el texto

```
theme(text = element_text(size=16))
```
#### Cambiar tipografía
```
library(extrafont)

theme(plot.title = element_text(size=18, family="Bebas Kai", color="##1c2366"),
```


Cambiar todo el texto:
```
text = element_text(family="Open Sans"),
```


Ver tipografías instaladas:
```
extrafont::fonts()
```
#### Cambiar color de un texto según condición

Crear la condición necesaria y poner como categorías de la variable los colores a elegir:

```
datos %>%
mutate(Region2 = case_when(Region=="Tarapacá" ~ "##red",
    TRUE ~ "gray80"))
```


Luego agregar en el geom:
```
geom_text(aes(x=Comuna, y=-50, label = Region),
	color = datos$Region2)
```
#### Añadir valor o texto a las etiquetas del eje x

```
scale_x_discrete(labels = paste0(Region, "\n", Poblacion))
```
#### Ajustar etiquetas largas

Para cortar la categorías largas, por ejemplo, en la leyenda de un gráfico o las etiquetas de un eje:
```
datos %>%
  ggplot(aes(variable, str_wrap(variable, width = 35))) +
  geom_line()
```
Donde el número es los caracteres del ancho de línea.

Para aplicar desde fuera:
```
scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
```

Para mantener el orden de los factores:
```
str_wrap_factor <- function(x, ...) {
    levels(x) <- str_wrap(levels(x), ...)
    x
}
```

Y luego usar `str_wrap_factor` del mismo modo que `str_wrap`

Cortar factores:
```
mutate(ocupación_CIUO = str_trunc(as.character(ocupación_CIUO), 30, side="right")) %>%
```
Luego volver a convertir a factor.

#### ggtext

[Enhance Your ggplot2 Data Visualizations with ggtext](https://thomasadventure.blog/posts/enhance-ggplot2-with-ggtext/)

## Escalas
Manipulación de las escalas de los gráficos, que son los elementos que controlan la disposición espacial de las observaciones en el plano.

#### Ordenar grafico de barras
Ordenar las barras para que una categoría del stack vaya de mayor a menor: crear una variable con `case_when` que sea un valor numérico, luego sumar esos valores numéricos en `summarize` según los grupos.

```
turismo2 %>%
  mutate(orden = case_when(interés == "Es de interes" ~ "1", TRUE ~ "0")) %>%
  mutate(orden = as.numeric(orden)) %>%
  group_by(ubicación, interés, orden) %>%
  summarize(cantidad = n(),
            orden2 = sum(orden)) %>%
    ggplot(aes(fct_reorder(ubicación, orden2), porcentaje, fill=interés))
```


Alternativa: crear variable de orden con un subset
```
group_by(Region) %>%
  mutate(Orden = Valor[Grupo == "Casos"]) %>%
  ggplot(aes(fct_reorder(Region, Orden), 
             Valor, fill = Grupo)) +
```
#### Expandir un eje

Por ejemplo, si una línea queda muy corta en la parte superior de un gráfico de densidad. Similar a `coord_cartesian`

```
scale_y_discrete(expand = expand_scale(add = c(0.3, 5.5))) +
```
#### Poner comas en los ejes

```
scale_x_continuous(labels = scales::comma)
```
#### Intercambiar ejes

```
+ coord_flip()
```

Intercambiar y cortar o extender:  
```
coord_flip(xlim=c(0, 800)) + 
```
#### Cambiar orden de los ejes

```
ggplot(aes(x = logFoldChange, y = reorder(variable, variable_de_ordenamiento)) +
	geom_point()
```

Invertir el orden:
```
ggplot(datos, aes(x=fct_rev(var1), y=...))
```

Reordenar ascendente:
```
ggplot(datos, aes(x = fct_infreq(var1), y=...
```

Reordenar descendente
```
ggplot(datos, aes(x = fct_rev(fct_infreq(variable))))
```

Reordenar según otra variable
```
ggplot(datos, aes(x= fct_reorder(variable, referencia), y=...
```
Según otra pero en descendiente:
```
mutate(Genre = fct_reorder(Genre, n, .desc = TRUE))
```

Reordenar manualmente:
```
name = factor(name, levels=c("north", "north-east", "east", "south-east", "south", "south-west", "west", "north-west")))
```

Reordenar según suma de los valores del eje:
```
  ggplot(aes(x=fct_reorder(Comuna, Porcentaje, .fun=sum, .desc=TRUE)
```
Util cuando son barras stacked y hay que ordenar por la suma de los factores

#### Girar etiquetas del eje x
Girar etiquetas del eje, dándole ángulo o poniéndola horizontal, por ejemplo

```
axis.text.x = element_text(angle = -90, hjust=0)) +
```
`hjust` justifica el texto

Girar a la derecha y justificar:
```
theme(axis.text.x = element_text(hjust=0, vjust=0.5, angle = -90)
```
#### Cambiar márgenes de elementos

```
theme(axis.text.x = element_text(margin = margin(t = 0)),
	axis.text.y = element_text(margin = margin(r = -5)),
    plot.subtitle = element_text(margin = margin(b = 15)))
```
#### Mostrar todas las etiquetas del eje x

Mostrar todas las etiquetas del eje x
```
scale_x_continuous("ID", labels = as.character(ID), breaks = ID)
```

```
scale_x_continuous(breaks = c(1973:1980))
```
#### Definir cortes del eje y
```
scale_y_continuous(breaks = c(10, 50, 100, 150, 200)) +
```
También sirve para eliminar el 0 del eje. Para combinar con logaritmo: `, trans="log10"`


#### Transformar escalas a logaritmo

```
## Transform the scale
d + scale_y_log10()

```

```
## Transform the coordinates
d + coord_trans(y="log10")
```
#### Agregar alfombra
Muestra los puntos exactos donde existen datos.

`+ geom_rug()`

#### Cambiar eje a logaritmo 
Para cambiar una escala a logarítmica: 
```
+ scale_x_log10()
```
#### Ampliar eje

Para que un eje empiece desde cero y así se elimine el margen o espaciado por defecto de ggplot:
```
expand_limits(y=0)
```

```
coord_flip(expand=FALSE)
```

```
scale_x_discrete(expand = c(0, 0)) +
```

```
scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))
```
#### Insertar un nivel en blanco para una escala

Para que en la leyenda aparezca un espacio vacío entremedio.

Primero agregar el nivel blanco con `\n`:
```
previsión_t$previsión = factor(previsión_t$previsión, levels=c("Fonasa A", "Fonasa B", "Fonasa C", "Fonasa D", "Fonasa desconocido", "\n\n","Isapre", "FF.AA. y de Orden", "Ninguno (Particular)", "Otro sistema"))  
```

Luego definir el color blanco y determinar que no se salte el nivel al estar vacío.
```
scale_fill_manual(values = c(degradado1(4), "gray70",
                                 "white", ##Color del nivel en blanco
                                 degradado4(3), "gray70"),
                       drop = FALSE) + ##Determinar que el nivel en blanco no se salte
```
#### Escalas de series de tiempo

[https://www.statworx.com/at/blog/customizing-time-and-date-scales-in-ggplot2/](https://www.statworx.com/at/blog/customizing-time-and-date-scales-in-ggplot2/)

Configurar el eje x cuando es fecha:
```
scale_x_date(breaks = seq(from = ymd('2020-03-30'), to = max(covid_comuna$Fecha), 
                            ##by=1),
                            length.out=12),
               date_labels = "%d/%B") +
```

La opción `by=1` pone un break por fecha, y `length.out` pone la cantidad de breaks definidos, dispersados entre el menos y el mayor.

#### Cambiar escalas continuas
```
    scale_fill_viridis(option="magma", ##color
                       name = "Porcentaje \nde votantes", 
                       breaks = c(0, 25, 50, 75, 100),
                       labels=c("0%", "25%", "50%", "75%", "100%"),
                       limits=c(20,80)) +
```
#### Escalas
Cambiar el rango de tamaños de los puntos
```
+ scale_size(range = c(1, 10))
```

Etiquetas en la escala manualmente:
```
scale_y_continuous(breaks = c(300000, 1000000,5000000,10000000))
```

Etiquetas menores en la escala:
```
scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010, 2014, 2017),
                       minor_breaks = seq(1991, 2016, 1)) +
```

Secuencia de breaks en la escala:
```
scale_y_continuous(limits = c(0, 12000), breaks = seq(0,12000,by = 2000)
```
#### Escala en miles

Puntos de miles en la escala:
```
scale_y_continuous(labels = function(x) format(x, big.mark = ".")) +
```
Escala de miles

O bien: 
```
scale_size_continuous(labels = scales::number) +
```
#### Escala en porcentajes

```
library(scales)

scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
```

Pegar porcentaje en eje (si los valores ya vienen multiplicados por 100:
```
scale_y_continuous(labels = function(x) paste0(x, "%")) +
```
## Colores
Operaciones que permiten controlar y modificar los colores utilizados en los gráficos.

#### Agregar más de una escala de color

[https://eliocamp.github.io/ggnewscale/](https://eliocamp.github.io/ggnewscale/)

#### Escalas continuas de colores (degradado)

Definir color del mínimo y máximo:
```
scale_fill_gradient(low = "gray90", high = color_verde) +
```


Definir color del mínimo, intermedio y máximo:
```
scale_color_gradient2(low = colorspace::darken(color_verde, 0.2), 
                            mid = colorspace::darken(color_verde, 0.4), 
                            high = "white", 
                            midpoint = 3.8) +
```
Opcionalmente, definir en `midpoint` el punto que equivale al intermedio.

#### Aplicar paleta de colores específicas a determinadas variables

```
cyl <- sort(unique(mpg$cyl))
ncat <- length(cyl)          ## 4 types of cylinders

## create palettes
library(RColorBrewer)
purples <- tibble(cyl, colr = brewer.pal(ncat, "Purples"))
reds    <- tibble(manufacturer = "audi", cyl, colr = brewer.pal(ncat, "Reds"))
blues   <- tibble(manufacturer = "ford", cyl, colr = brewer.pal(ncat, "Blues"))

## merge them with the data
dd_p <- dd %>% filter(!(manufacturer %in% c("audi", "ford"))) %>% left_join(purples)
dd_r <- dd %>% filter(manufacturer == "audi") %>% left_join(reds)
dd_b <- dd %>% filter(manufacturer == "ford") %>% left_join(blues)

gg_dd <- rbind(dd_p, dd_r, dd_b) %>%
        left_join(mm)

gg_dd %>% 
        ggplot(mapping = aes(x = reorder(manufacturer, mcyl), y = n, fill = colr)) + 
        geom_bar(stat = "identity", position = "fill") +
        coord_flip() +
        scale_fill_identity() 
```
#### Aplicar escala de color automática

Primero crear la escala de color con tantos colores como se necesiten:
```
degradado <- colorRampPalette(c("##DF1A57", "##AF87EB", "##1D3284"))
```

Luego aplicar la escala al gráfico, determinando el número de colores a usar:
```
scale_color_manual(values = degradado(7),
aesthetics = c("fill", "col")) +
```
#### Crear escala o degradado de colores

```
degradado <- colorRampPalette(c("##DF1A57", "##c444c4", "##6739b2", "##1D3284"), bias=0.9)
```

Bias es la separación de los colores en los extremos de la escala. Un bias alto (mayor a 1) hace que los colores cambien más rápido y uno bajo (cercano a 0) distribuye los colores más suavemente.

#### Previsualizar colores

Para ver un color o vector de colores:

```
scales::show_col("##DF1A57")

scales::show_col(c("##DF1A57", "##c444c4", "##6739b2", "##1D3284"))

scales::show_col(degradado7b(7))
```
#### Aclarar u oscurecer colores

```
colorspace::lighten("red", amount = 0.5)
```

```
scales::show_col(colorspace::lighten(color_naranjo, amount = 0.6))
```
#### RColorBrewer
```
scale_fill_brewer(palette = "Dark2") +
```

Para ver todas las paletas disponibles: 
```
RColorBrewer::display.brewer.all()
```

Extraer colores:
```
colores_presidencial <- rev(brewer.pal(9, 'YlGnBu')) ##extraer paleta de colores
```

Luego aplicar como gradiente:
```
scale_fill_gradientn(colours = colores_presidencial, ##crear gradiente de colores
                         name = "Porcentaje \nde votantes", 
                         breaks = c(10, 30, 50, 70, 90)) + 
```


#### Crear degradados de color
Crear escala de colores entre colores específicos:
```
degradado1 <- colorRampPalette(c("##DF1A57", "##AF87EB", "##1D3284"))
```
Luego aplicar así:
```
scale_color_manual(values = degradado1(5)) +
```
Donde el número es la cantidad de colores

#### Extender escalas de colores
```
## Definition of a set of blue colors
blues <- brewer.pal(9, "Blues") ## from the RColorBrewer package

## 1 - Make a color range using colorRampPalette() and the set of blues
blue_range <- colorRampPalette(blues)

## 2 - Use blue_range to adjust the color of the bars, use scale_fill_manual()
ggplot(Vocab, aes(x = education, fill = vocabulary)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values=blue_range(11))
```
#### Determinar colores para variable categórica
```
colores_redes<- c("Facebook"="##1877f2","WhatsApp y similares"="##25d366","Instagram"="##c32aa3","Skype"="##00aff0","Twitter"="##1da1f2","Tinder y similares"="##FF5864","Snapchat"="##fffc00","LinkedIn"="##007bb5")
```
Luego, dependiendo del tipo del elemento:
```
scale_fill_manual(values = colores_redes) + 
scale_color_manual(values = colores_redes) +
```


#### Más escalas de colores


###### Viridis
```
scale_color_viridis() +
```

```
scale_fill_viridis(option="magma") +
```
 “magma”, “plasma”, and “inferno.”

```
discrete = TRUE
```

Más paletas: 
- [https://twitter.com/wearerladies/status/1180545410974257152?s=12](https://twitter.com/wearerladies/status/1180545410974257152?s=12)
- [https://github.com/EmilHvitfeldt/r-color-palettes/blob/master/type-sorted-palettes.md##qualitative-color-palettes](https://github.com/EmilHvitfeldt/r-color-palettes/blob/master/type-sorted-palettes.md##qualitative-color-palettes)



###### CartoColor
[https://github.com/Nowosad/rcartocolor](https://github.com/Nowosad/rcartocolor)
Ver colores:
```
cartocolor::display_carto_all()
```

Usar:
```
scale_fill_carto_c(name = "Life expectancy: ", type = "diverging", palette = "Earth", direction = -1)
```



###### Scico
Ver colores:
```
scico::scico_palette_show()
```

Generar paleta:
```
paleta <- scico::scico(30, palette = 'lapaz')
```

Usar escala:
```
ggplot(volcano, aes(x = x, y = y, fill = height)) + 
  geom_raster() + 
  scale_fill_scico(palette = 'davos') 
```


###### Fishualize
```
devtools::install_github("nschiett/fishualize", force = TRUE)
library(fishualize)

paleta <- fish(10, option = "Ostracion_cubicus")
```

Ver colores:
[https://nschiett.github.io/fishualize/articles/overview\_colors.html](https://nschiett.github.io/fishualize/articles/overview_colors.html)
```
scales::show_col(fishualize::fish(10, option = "Antennarius_commerson"))
```

Bonitas:
```
Oncorhynchus_tshawytscha
Bodianus_pulchellus
Antennarius_commerson
```

```
scales::show_col(fishualize::fish(10, option = "Oncorhynchus_keta"))
```

Ver lista de peces:
```
spp <- fishualize::fish_palettes()
```

```
##Escalas continuas:
scale_color_fish(option = "Hypsypops_rubicundus", direction = -1)

##Escalas discretas:
scale_color_fishd(option = "Hypsypops_rubicundus", direction = -1)

scale_color_fish(option = "Coris_gaimard", discrete = TRUE)
```


###### Ghibli
[https://ewenme.github.io/ghibli/index.html](https://ewenme.github.io/ghibli/index.html)![](https://ewenme.github.io/ghibli/reference/figures/README-palettes-1.png)
```
library(ghibli)
scale_colour_ghibli_d("LaputaMedium", direction = -1)
```

###### PaletteR
```
devtools::install_github("AndreaCirilloAC/paletter")

library(paletter)

image_path <- "path_to_your_image"
colours_vector <- create_palette(image_path = image_path,
number_of_colors =32,
type_of_variable = “categorical")

ggplot(data = mtcars, aes(x = rownames(mtcars),y = hp,color = rownames(mtcars),
                          fill = rownames(mtcars))) +
  geom_bar(stat = 'identity') +
  scale_color_manual(values = colours_vector) +
  scale_fill_manual(values=colours_vector)+
```


###### Pirate palette

```
yarrr::piratepal(palette = "all")
```




## Leyendas
Manipular la leyenda, o la sección del gráfico que contiene los colores y/o etiquetas de los elementos gráficos que lo componen.

#### Crear gráfico con leyenda manual

Cuando los elementos el gráfico se hacen por capas separadas y por lo tanto no aparecen con leyenda:
```
cols <- c("Investigadores\nhombres"="##B077E5",
          "Investigadoras\nmujeres"="##DF1A57",
          "Total de\ninvestigadores"="##1D3284")

investigadores %>%
  rename(año=1) %>%
  ##pivot_longer(cols=c(3:4), names_to="género", values_to="valor") %>%
  ggplot(aes(año)) +
  geom_col(aes(y=total, fill="Total de\ninvestigadores")) +
  geom_line(aes(y=mujeres, col="Investigadoras\nmujeres"), size=2, alpha=0.6) +
  geom_point(aes(y=mujeres, col="Investigadoras\nmujeres"), size=4) +
  geom_line(aes(y=hombres, col="Investigadores\nhombres"), size=2, alpha=0.6) +
  geom_point(aes(y=hombres, col="Investigadores\nhombres"), size=4) +
  ##texto
  geom_text(aes(y= mujeres-3.5, label = mujeres), col="white") +
  geom_text(aes(y= hombres+3.5, label = ifelse(año!=2013, hombres, "")), col="white") +
  geom_text(aes(y= hombres+5.5, label = ifelse(año==2013, hombres, "")), col="white") +
  geom_text(aes(y= total+3, label = total)) +
  ##escalas
  scale_colour_manual(name="Error Bars", values = cols) + 
  scale_fill_manual(name="Bar", values = cols) +
  scale_x_continuous(breaks=c(2009:2017)) +
  labs(y="Cantidad de investigadores/as")
```



Alternativa más rápida: 
```
geom_hline(aes(yintercept = promedio, col="Arancel promedio")) +
scale_colour_manual(name="Error Bars", values = c("Arancel promedio"="red")) + 
```
#### Invertir orden de leyenda

```
guides(fill = guide_legend(reverse = TRUE)) +
```
#### Orientación y ubicación de la leyenda

Ubicación de la leyenda: `theme(legend.position = c(0.85, 0.85))`

Mover la leyenda en relación a su ubicación por defecto:
```
legend.box.margin=margin(c(0,0,0,-60)) ##moverla a la izquierda
```


Leyenda dentro del gráfico: 
```
theme(legend.position = c(.8,.8))
```

Eliminar leyenda: `theme(legend.position = "none")`

Añadir margen al gráfico: 
```
theme(panel.spacing.x=unit(2, "cm"), plot.margin=unit(c(1,2,1,1), "cm"))
```

Eliminar todos los rectángulos: `no_panels <- theme(rect = element_blank())`

Poner leyenda abajo:
```
theme(legend.position = "bottom")
```

Cambiar orientación de los elementos de la leyenda:  
```
theme(legend.direction = "horizontal")
```

Cambiar ubicación de leyenda:
```
+ theme(legend.position = "bottom",
          legend.box = "vertical") 
```


#### Textos de leyenda
Cambiar título de leyenda:
```
+ scale_fill_discrete(name = "")
```

Si es una combinación de geoms:
```
labs(fill = "Nacionalidad", col = "Nacionalidad") +
```

Cambiar nombre de elementos de la leyenda:
```
+ scale_fill_discrete(name = "Padrón electoral", labels = c("Inscritos que no votaron", "Votantes")) +
```

Ocultar una leyenda específica:
```
+ guides(col = FALSE)
```
O poner dentro del geom: `show.legend=FALSE`


#### Elementos de la leyenda
Cambiar espaciado horizontal de leyenda:
```
legend.spacing.x = unit(0.4, 'cm') ##NO SIRVE?
```

Cambiar espaciado vertical de elementos de leyenda:
```
legend.text = element_text(margin = margin(t=4, b = 4), size=9))
```

Tamaño de cuadrados de leyenda
```
legend.key.size = unit(1.7, 'lines')
```

Ajustar espacio vertical:
```
legend.text = element_text(margin = margin(t=10, b=10)),
```

Ajustar espaciado de leyendas:
```
legend.text = element_text(margin = margin(l = -2, r = 10), size=8))
```

Definir cantidad de filas en que aparecen las leyendas:
```
guides(fill = guide_legend(nrow = 3)) + ##cantidad de filas de la leyenda
```

Invertir elementos de leyenda:
```
guides(fill = guide_legend(reverse = TRUE))
```


Cambiar forma de la leyenda:
```
guides(colour = guide_legend(override.aes = list(shape = 15)))
```

Borrar el cuadrado gris de fondo de la leyenda:
```
theme(legend.key = element_blank())
```

Cambiar color de la leyenda:
```
guides(shape = guide_legend(override.aes = list(colour = "pink")))
```
#### Margen de la leyenda
```
theme(legend.margin = margin(20, 20, 20, 0)) + ##caja de la leyenda
```
#### Leyendas redondas

Cambiar una leyenda redonda de `geom_line` o `geom_point` por una cuadrada como de `geom_col`
```
guides(col = guide_legend(nrow = 3, override.aes = list(shape = 15, size=7))) +
```
Para que las barras tengan círculos en la leyenda:
- poner fill y col en aes()
- poner un `geom_point`  de size 0, alpha 0 
- show-legend F en las columnas
- que los títulos coincidan en labs, y que se use o no reverse en guides de forma consistente:
```
theme(legend.key = element_blank(),
        legend.background = element_blank()) +
  geom_point(size=0) +
  ##guides(fill = guide_legend(override.aes = list(fill = NA, text = NA, alpha = 1))) +
  guides(col = guide_legend(reverse = TRUE,
                            override.aes = list(size=5, fill=NA, text=NA)))
```


Ejemplo en gráfico de líneas
```
ggplot(aes(fecha, valor, col=parque, fill=parque)) +
  geom_line(show.legend = FALSE) +
  geom_point(size=0, alpha=0) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".")) +
  labs(y="Visitas") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.text = element_text(margin = margin(t=3, b=3, r=6))) +
  guides(col = guide_legend(ncol=2,
                            override.aes = list(size=4, fill=NA, text=NA))) +
  guides(fill = guide_legend(override.aes = list(fill = NA, text = NA, alpha = 1)))
```
## Temas y espaciado

#### Espaciado entre facetas
El espacio horizontal entre dos facetas de un gráfico, cuando el gráfico se separa en facetas con `facet_wrap()`
```
theme(panel.spacing.x=unit(2, "cm"))
```
#### Reducir margen de gráficos

Especialmente útil para gráficos de torta
```
theme(plot.margin = margin(-20, 0, -20, -0)) +
```

Ejemplo:
```
plot.margin = margin(20, ##arriba
                               -90, ##derecha
                               10, ##abajo
                               -50) ##izquierda
          )
```
#### Múltiples gráficos
Facetas

```
ggplot(comics, aes(x = align)) + 
  geom_bar() +
  facet_wrap(~ gender)
```

```
ggplot(email, aes(x= number)) +
  geom_bar() +
  facet_wrap(.~spam)
```


```
## 1 - Separate rows according to transmission type, am
p +
  facet_grid(am ~ .)
```

```
## 2 - Separate columns according to cylinders, cyl
p +
  facet_grid(. ~ cyl)
```

```
## 3 - Separate by both columns and rows 
p +
  facet_grid(am ~ cyl)
```

Liberar los ejes de los gráficos para que sean distintos:
```
facer_wrap(~ country, scales="free_y")
```

Eliminar cruces donde no hay datos:
```
p +
  facet_grid(vore ~ .,scale= "free_y", space = "free_y")
```


Espaciado entre facetas:
```
library(grid)
z + theme(panel.spacing.x=unit(2, "cm"))
```

Gráficos verticales
```
facet_wrap( ~ site, ncol = 1) + ## Facet layer
```


Espaciado entre facetas:
```
panel.spacing.y =unit(0.4, "cm")) +
```

Ocultar títulos de faceta:
```
theme(strip.background = element_blank(), strip.text = element_blank())
```

Facetas de lado
```
facet_wrap(~ str_to_title(tipo), nrow = 2, scales = "free_y", 
               strip.position="left")
```



Cortar textos de faceta (sin modificar los factores):
```
facet_wrap(~egreso, nrow=1, labeller = label_wrap_gen(width=10)) +
```
#### Elementos y temas
```
+ theme()
```

Dentro de la capa `theme()`:

Especificar líneas: `=element_line()`
```
theme(
		axis.line=element_line(color="red")
)
```

Remover un elemento: `=element_blank()`
```
theme(
	panel.grid=element_blank()
)
```
Para especificar el eje afectado, se pone un punto y el eje al final del nombre del elemento, por ejemplo: `axis.text.x`

###### Elementos:
- `element_text()`
- `element_rect()`
- `element_line()`
- `element_blank()`

- Ejes:
	- Marcas en los ejes: `axis.ticks`
	- Líneas de los ejes: `axis.line`
	- Etiquetas de los ejes: `axis.text`

- Fondos:
	- `panel.background`
- Rejillas:
	- `panel.grid.major`
	- `panel.grid.minor`

###### Cambiar texto
- Texto de facetas: `strip.text`
- Título de los ejes: `axis.title`
- Números en los ejes: `axis.text`
```
plot +
  theme_minimal() +
  ## Customize the "minimal" theme with another custom "theme" call
  theme(
    text = element_text(family = "Bookman"),
    title = element_text(color = "gray25"),
    plot.caption = element_text(color = "gray30"),
    plot.subtitle = element_text(size = 12)
  )
```

Cambiar margen del texto:
```
  theme(axis.text.y = element_text(margin = margin(r = -10)),
        axis.text.x = element_text(margin = margin(b = -15))) +
```

Renombrar ejes:
```
labs(y="Cantidad de reclamos y consultas", x="Institución o vía de emisión") 
```


#### Combinar múltiples gráficos
Unir dos gráficos:

Si usan los mismos datos:
```
library(gridExtra)
basura_x2 <- grid.arrange(basura_gg, basura_p_gg, ncol=2)
```

Para dos gráficos distintos:
```
ggsave("foo.pdf", arrangeGrob(plot1, plot2))
```


Con cowplot:
```
library(cowplot)
empresas_combinado_cowplot <- plot_grid(empresas_tamaño_porc_gg, 
          empresas_trabajadores_porc_gg,
          leyenda_1,
          nrow = 3,
          align = "v",
          rel_heights = c(3, 3, 1))  
```
#### Ajustar espacio entre etiquetas de ejes y gráfico
Para acercar o alejar las etiquetas de las barras:
```
theme(axis.text.y = element_text(margin = margin(r = -3, l = 5))) +
```
#### Acercar leyenda al gráfico
```
theme(legend.margin = margin(20, 20, 20, 0)) + ##caja de la leyenda
```
#### Cambiar tamaño y hacer zoom en los datos

```
+ coord_cartesian(xlim=c(3,6))
```


Graficar con la misma relación entre ejes (si las unidades de medida son iguales)
```
+ coord_equal()
```
#### Guardar y aplicar temas

Guardar la configuración de elementos de la capa `theme` en un tema:
```
mi_tema <- theme(element.axis...)
plot + mi_tema
```

Expandir temas guardados con nuevas configuraciones:
```
mi_tema2 <- mi_tema + theme(element...
```
Ejemplo:
```
z +
  no_panels +
  theme(plot.background = element_rect(fill = myPink, color="black", size=3))
```


Elegir un tema por defecto (que se aplique a todos lo gráficos):
```
theme_set(mi_tema)
```

The arguments for theme_update() are the same as for theme(). When you call theme_update() and assign it to an object (e.g. called old), that object stores the current default theme, and the arguments update the default theme. If you want to restore the previous default theme, you can get it back by using theme_update() again. Let's see how:_
```
## 2 - Update the default theme, and at the same time
## assign the old theme to the object old.
old <- theme_update(panel.background = element_blank(),
             legend.key = element_blank(),
             legend.background = element_blank(),
             strip.background = element_blank(),
             plot.background = element_rect(fill = myPink, color = "black", size = 3),
             panel.grid = element_blank(),
             axis.line = element_line(color = "red"),
             axis.ticks = element_line(color = "red"),
             strip.text = element_text(size = 16, color = myRed),
             axis.title.y = element_text(color = myRed, hjust = 0, face = "italic"),
             axis.title.x = element_text(color = myRed, hjust = 0, face = "italic"),
             axis.text = element_text(color = "black"),
             legend.position = "none")

## 3 - Display the plot z2 - new default theme used
z2

## 4 - Restore the old default theme
theme_set(old)

## Display the plot z2 - old theme restored
z2
```

######## Cargar temas con el paquete `ggthemes`
```
library(ggthemes)
```
# Exportar gráficos

```
ggsave(file="grafico7_7_2c.jpg", plot=basura_x2, dpi="retina")
```

Cambiar DPI para especificar resolución
```
ggsave("Gráfico.png", units="in", width=5, height=4, dpi=300)
```

Exportar animación:
```
anim_save("Animación.gif", animation = last_animation())
```
