# Canon petrolero y el índice de desarrollo humano en Loreto 2019 a nivel distrital {#canon-idh}

*Felipe Bedoya*

## Introducción

El presente análisis busca explorar el comportamiento de la relación entre las transferencias de canon petrolero al departamento de Loreto y el nivel de desarrollo humano para el año 2019. 

El canon petrolero es el derecho de las zonas petroleras a participar en el reparto de la renta que produce la extracción de petróleo. Los beneficiarios del canon son los gobiernos regionales y locales. 

El Índice de Desarrollo Humano (IDH) es una medida del desarrollo humano que toma en cuenta tres dimensiones: una vida larga y saludable, medida por la esperanza de vida al nacer; el logro educativo, que toma en cuenta la educación secundaria completa a los 18 años y los años de educación de la población mayor de 25 años; y el ingreso familiar per cápita. 

## Paquetes usados

Para explorar la relación entre las transferencias de canon petrolero al departamento de Loreto y el nivel de desarrollo humano, utilizaremos los paquetes tidyverse y readx. El primero permite seleccionar, filtrar, organizar, modificar, resumir y agrupar datos. El paquete readxl permite importar archivos de Excel. 


```r
library(tidyverse)
library(readxl)
```


## Conjuntos de datos usados

Utilizaremos los datos de Índice de Desarrollo Humano del Programa de Naciones Unidas para el Desarrollo (PNUD), disponibles en:  <https://www.pe.undp.org/content/peru/es/home/library/poverty/el-reto-de-la-igualdad.html>.

El conjunto de datos “Índice de Desarrollo Humano 2019” muestra el índice de desarrollo humano y cada una de sus dimensiones (esperanza de vida, logro educativo y ingreso familiar per cápita) a nivel departamental, provincial y distrital. 

También las transferencias de canon petrolero a los gobiernos locales disponibles en el Portal de Transparencia Económica: Consulta de Transferencias a los Gobiernos Nacionales, Locales y Regionales del Ministerio de Economía y Finanzas (MEF) en el siguiente enlace: <https://apps5.mineco.gob.pe/transferencias/gl/default.aspx>

Este conjunto de datos contiene información de las transferencias de canon petrolero a los gobiernos locales (provinciales y distritales) del departamento de Loreto según nombre y montos para el alo 2019. 

Ambos conjuntos de datos me permitirán explorar la relación entre las transferencias de canon petrolero y el nivel de desarrollo humano en el departamento de Loreto para el año 2019. 

A continuación procedederemos a hacer la limpieza y ordenamiento de datos, para lo cual nombraremos nuestro conjunto de datos como sigue: 


```r
canon_2019 <- read_excel("data/70891853.xls", skip = 17, col_names = FALSE) 
```



```r
idh_2019 <- read_excel("data/IDH 2019.xlsx", sheet = 4, skip = 4) 
```

En el caso de datos de canon para el año 2019 realizaremos lo siguiente: primero establecemos los nuevos nombres a las columnas con la funcion set_names. Luego eliminamos de la columna "nombre" la palabra "municipalidad" con la funcion "str_remove". Además separamos los nombres, provincial y distrital en cada caso, con la funcion "separate".  También eliminamos todo lo que está delante de los guiones que acompañan a los nombres de los municipios con la funcion "str_replace". Finalmente, seleccionamos las columnas "tipo", "nombre" y "monto acreditado".  

Para guardar los cambios, renombramos nuestros datos. 


```r
canon_data <- canon_2019 %>% 
  set_names(c("codigo", "nombre", "monto_aut", "monto_acred")) %>% 
  mutate(nombre = str_remove(nombre, "MUNICIPALIDAD ")) %>% 
  separate(nombre, into = c("tipo", "nombre"), sep = " DEL? ", extra = "merge") %>%
  mutate(nombre = str_replace(nombre, " - .*", "")) %>% 
  select(tipo, nombre, monto_acred)
```

A continuación realizaremos la limpieza del conjunto de datos referido al IDH. 

En primer lugar seleccionamos las columnas referidas al ubigeo, distrito, población y IDH con la funcion "select". Con la función "set_names" asignamos los nombres a las columnas seleccionadas: "ubigeo", "distrito", y "idh". Luego con la función "separate" convertimos la columna "de "ubigeo" en tres columnas: "codigo departamento", "código de provincia" y "código de distrito", que nos permitirá identificar el departamento de interés.En línea con lo anterior, con la función "filter" seleccionamos los datos para el departamento de Loreto. Finalmente, ponemos en mayúscula los datos correspondientes a la columna distrito. 

Para guardar los cambios, renombramos nuestros datos. 


```r
idh_data <- idh_2019 %>% 
  select(1, 3, 6, 8) %>% 
  set_names(c("ubigeo", "distrito", "poblacion", "idh")) %>% 
  separate(ubigeo, into = c("cod_dep", "cod_prov", "cod,dist"), sep = c(2, 4)) %>%
  filter(cod_dep == "16") %>% 
  mutate(distrito = str_to_upper(distrito))
  
```

Con nuestros datos "limpios" procederemos a explorar la relación entre las transferencias de canon al departamento de Loreto y los niveles de desarrollo humano. 

Para ello, usamos la función "left_join" mediante la cual unimos (a la izquierda) la data de canon correspondiente a cada distrito de nuestra data de idh, con el argumento "by".  

Renombraremos esta nueva información como "Loreto". 


```r
Loreto <- idh_data %>% 
  left_join(canon_data, by = c("distrito" = "nombre"))
```

## Resultados

Para visualizar la relación entre las transferencias de canon y los niveles de desarrollo humano alcanzado en Loreto, utilizaremos la función "ggplot" dado que nos permitirá hacer un gráfico de puntos para observar el comportamiento de estas variables. También renombramos las provincias con el comando "mutate" y le daremos color a los puntos para diferenciarlos según provincias al interior del departamento. 


```r
Loreto <- Loreto %>% 
  mutate(Provincia = case_when(
    cod_prov == "01" ~ "MAYNAS", 
    cod_prov == "02" ~ "ALTO AMAZONAS", 
    cod_prov == "03" ~ "LORETO",
    cod_prov == "04" ~ "MARISCAL RAMON CASTILLA",
    cod_prov == "05" ~ "REQUENA", 
    cod_prov == "06" ~ "UCAYALI",
    cod_prov == "07" ~ "DATEM DEL MARANON",
    cod_prov == "08" ~ "PUTUMAYO",
    TRUE ~ "OTROS"))
```



```r
Loreto <- Loreto %>% 
  mutate(monto_acred_millones = (monto_acred/1000000))
```


```r
Loreto %>% 
  ggplot(aes(monto_acred_millones, idh, color = Provincia)) + 
  geom_point() +
  labs(title = "Relación transferencias de canon petrolero y desarrollo", subtitle = "Distritos Loreto 2019", x = "Millones de soles", y = "IDH")
```

<img src="04-felipe_files/figure-html/unnamed-chunk-9-1.png" width="672" />

De manera complementaria, consideraremos una variable adicional para tomar en cuenta el comportamiento entre las transferencias de canon y el nivel de desarollo humano, para lo cual crearemos la variable "transferencias de canon per cápita". 

Para lo anterior, usaremos la función "mutate" y divideremos las trasnferencias de canon (monto acreditado) entre habitantes (población).

Finalmente, renombramos esta nueva información. 


```r
Loreto_percap <- Loreto %>% 
   mutate(canon_percap = (monto_acred/poblacion))
```

Similarmente, procedemos a explorar la relación entre nuestras variables de interes. En esta ocasión tomando en cuenta el monto de canon per capita. 


```r
Loreto_percap %>% 
  ggplot(aes(canon_percap, idh, color = Provincia)) + 
  geom_point() +
  labs(title = "Relación transferencias de canon petrolero per cápita y desarrollo", subtitle = "Distritos Loreto 2019", x = "Soles", y = "IDH")
```

<img src="04-felipe_files/figure-html/unnamed-chunk-11-1.png" width="672" />

## Conclusiones

En el primer caso hemos explorado la relación entre las transferencias de canon a nivel distrital para el departamento de Loreto con el nivel de desarrollo humano para el año 2019. Los resultados muestran que en la mayoria de los casos existe una relación directamente proporcional, esto quiere decir, que a mayores transferencias de canon, mayor nivel de desarollo humano. 

En el segundo caso, la relación entre las transferencias de canon per capita y los niveles de desarrollo humano alcanzados, se mantiene. Esto podria significar que independientemente del numero de habitantes, la relación entre ambas variables es la misma. 

No obstante lo anterior, debemos tomar en cuenta que los componentes del índice de desarollo humano referidos a salud, educación y nivel de ingresos, se "mueven" a distintas velocidades, es decir, mientras la salud y la educación responden a politicas de largo y mediano plazo, los niveles de ingreso en una región que recibe transferencias de canon petrolero, puede verse afectadas por la volatilidad que caracteriza las rentas extractivas dado que dependen de los precios internacionales del petroleo y los niveles de producción. En suma, los altos niveles de desarrollo humano podrian estar relacionado a mayores ingresos por canon petrolero principalmente.  

Finalmente, cabe señalar que diversos estudios profundizan en la importancia de las instituciones para la adecuada gestión de la renta de los recursos naturales que inciden en altos niveles de desarrollo humano. Esto quiere decir que en localidades con altos niveles de institucionalidad la renta extractiva genera altos niveles de desarrollo; lo contrario si los niveles de institucionalidad son bajos. Lo mismo en el caso de una economía más divesificada y por tanto, menos dependiente de la volatilidad de los precios en el mercado internacional. 




