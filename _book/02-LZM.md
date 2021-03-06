
# Género, discriminación y participación ¿ciudadana?: Un acercamiento a la data existente.  {#genero-y-discriminacion}

*Leanna Sofía de los Ángeles Zúñiga Montaño*

## Introducción

La palabra democracia, en términos generales e inclusive poco claros, se instala en el pensamiento de la ciudadanía bajo el imaginario de una forma de gobierno, modelo o sistema político, dejando una suerte de nebulosa. Históricamente, el término democracia ha experimentado diversas maneras de comprenderse y, por ende, diferentes formas de ser ejercida; estos cambios no son gratuitos, pues dependen de épocas, contextos, experiencias y razonamientos diferentes. (Held, 2008) 

Para Dahl existen tres condiciones  fundamentales para la existencia de la democracia y de un gobierno que responda sin distinciones las preferencias de sus ciudadanos. Estas condiciones se enfocan en la igualdad de oportunidad que ellos deberían tener para: 1). Formular sus preferencias, 2). Manifestar públicamente dichas preferencias entre sus partidarios y ante el gobierno, individual y colectivamente y 3). Recibir por parte del gobierno igualdad de trato: es decir, éste no debe hacer discriminación alguna por causa del contenido o del origen de tales preferencias (Dahl 1989:18)

En línea con lo anterior, John, R. (2012), tomando como referencia a Fabio Velásquez, explica que la democracia también requiere ser pensada como algo más que las instituciones y los actores políticos; para él, resulta vital pensarla desde el ejercicio de la ciudadanía y las prácticas participativas que desarrollan en el escenario público. No obstante, para Nuria Cunill(1991) será importante considerar que la participación ciudadana debe estar relacionada con la titularidad de los derechos colectivos y con el derecho a tener derechos. 

Así, para autores como Restrepo (2005) la denominada democracia radical es fortalecida por la ciudadanía democrática radical, aquella que ha permitido visibilizar grupos sociales como homosexuales, obreros, ecologistas, mujeres que buscan las reivindicaciones desde la diferencia y la aceptación de la heterogeneidad para obtener derechos. 

Según vamos observando, la participación en lo público es una constante al pensar sobre la democracia y es una condición necesaria que supone la intervención de los ciudadanos y ciudadanas en las deliberaciones colectivas, construcción de actores sociales y articulación de identidades e intereses. 

Por tal razón, nos resulta interesante explorar la relación entre género, discriminación y participación ciudadana en Perú, en un contexto donde las personas LGBTQ+, consideradas como parte de los grupos especiales de protección, continúan movilizándose a razón de la sistemática discriminación y exclusión a las que son sometidas por el Estado y otras instancias privadas. En ese sentido, el presente proyecto plantea como pregunta de investigación ¿ Cómo manifiesta la participación política las personas LGBTQ+?. 

Para ello, exploramos qué tipo de información estadística se había generado desde el Estado u otros espacios con la finalidad de emplearla como material principal para la obtención de resultados. No obstante, de la exploración identificamos que la única fuente con información estadística disponible para ser empleada en el software R era la base de datos de la Primera Encuesta Virtual para las personas LGBTI, elaborada el 2017 por el Instituto Nacional de Estadística e Informática (INEI). 

Si bien existen otras encuestas realizadas como la I y II Encuesta Nacional de Derechos Humanos que recoge información relevante sobre esta comunidad, lamentablemente no se dispone del acceso público a sus bases de datos. Por ello, el presente proyecto tomará como base los resultados obtenidos del análisis a la base de datos de la Primera Encuesta Virtual para las personas LGBTI y como insumos las estadísticas presentadas en informes relevantes como el de la II Encuesta Nacional de Derechos Humanos, Comportamiento Electoral LGBTQ+ en Perú y otros. 


## Paquetes usados

Con el objetivo de emplear bases de datos, se utilizaron tres paquetes: `heaven`,`labelled` y la colección de paquetes `tidyverse`. El primer paquete permite la lectura de data que contiene archivos con extensión .sav, que pertenecen al formato SPSS Statistics. 

El segundo paquete ayuda a generar un diccionario de datos, herramienta que facilita la manipulación de la data. Finalmente, el tercer paquete nos permite desarrollar una limpieza de datos, además de ejecutar funciones que ayudan con la organización, el filtrado y la selección de variables relevantes. Otro aporte de este tercer paquete es que permite presentar la información extraída de la data a través de gráficos que pueden ser creados con la función ggplot.



```r
library(haven) 
library(labelled)
library(tidyverse)
```


## Conjuntos de datos usados

### Lectura de datos

Este proyecto empleó la base de datos de la "Primera Encuesta Virtual para personas LGBTI,2017" elaborada por el Instituto de Estadística e Informática (INEI) y que se encuentra disponible para descarga en el enlace <http://iinei.inei.gob.pe/microdatos/>. 

De acuerdo al informe técnico (2018) elaborado por el INEI, la encuesta tiene como finalidad generar información estadística que permita formular políticas, acciones y estrategias que garaticen el reconocimiento y protección de los derechos de las personas Lesbianas, Gays, Bisexuales, Transexuales e Intersexuales (LGBTI). No obstante, resulta necesario indicar que también se manifestó que la encuesta realizada fue de carácter exploratorio, no probabilístico; es decir, que la información que de ella devenga o sus resultados no son necesariamente representativos de la comunidad LGBTI. 


A continuación se realizará una descripción de los conjuntos de datos ejecutados en relación a las preguntas planteadas:

Iniciamos con la descarga y guardado de nuestra base de datos; a partir de ello, procedimos a leerla a través de la función `read_sav()`.


```r
dataLGBTI <- read_sav("data/602-Modulo1287.sav") %>% as_factor()
```


Luego, ejecutamos el código que permite obtener el valor de las etiquetas de datos provenientes de la extensión .sav. También se crea la función `change_id()`, que permite reemplazar los nombres de variable por sus etiquetas según están definidas en el archivo SPSS.  


```r
mis_variables <- look_for(dataLGBTI, details = FALSE) %>% mutate(label = str_wrap(label, 50))

change_id <- function(x, data_var) {
  map_chr(x, function(.x) {
    data_var %>% 
      filter(variable == .x) %>% 
      pull(label)
  })
}
```


A partir de la lectura de la base de datos y la obtención del valor de las etiquetas, procedimos a explorar la data e identificar la información más relevante en consonancia con el tema central. Al respecto, identificamos nuestras variables principales:

1. Sobre características sociodemográficas: p101, vinculada a los niveles educativos declarados por las personas encuestadas.
2. Sobre identidad, cuerpo y sexualidad: p116, a través de la que se recoge la respuesta a la pregunta ¿usted expresa sin temor su orientación sexual y/o identidad de género?
3. Sobre discriminación y violencia: p201, p202, p203, a través de las que se recoge información sobre experiencias de discriminaicón y/o violencia, espacios/lugares/ámbitos de violencia y personas que perpetraron discriminación y/o violencia, respectivamente. 
4. Sobre participación ciudadana: p401 y p402, las que permiten identificar si existe alguna vinculación con organizaciones y cuáles han sido las acciones de participacion ciudadanas desarrolladas, respectivamente. 


Las variables identificadas permitirán responder a las siguientes preguntas: 

1. ¿Cuál es la percepción de las personas LGBTI sobre al respeto que la población tiene hacia su orientación sexual e identidad de género? 
2. ¿Cuál es la percepción de las personas LGBTI en torno al avance en el reconocimiento de los derechos humanos de la comunidad LGBTI en Perú?.
3. ¿Cuáles son los grupos etáreos dentro de la comunidad LGBTI que experimentan mayor porcentaje de violencia?.
4. ¿Cuáles son los principales espacios donde se ejerce violencia hacia miembros de la comunidad LGBTIQ?.
5. ¿Quiénes ejercen violencia hacia  miembros de la comunidad LGBTIQ en los espacios que habita?.
6. ¿Cómo manifiesta la participación ciudadana las personas LGBTI  que experimentaron violencia y discriminación?.
7. ¿Cuáles son las principales acciones de carácter políticos en las que participan los miembros de la comunidad LGBTI que experimentaron violencia y discriminación?.


### Limpieza u ordenamiento de datos  

En esta sección se presentan los códigos empleados para la obtención de la data que será usada en el análisis de resultados. La data obtenida se encuentra en recuentos y porcentajes

#### Percepción sobre el respeto a la orientación sexual, identidad de género y reconocimiento a los derechos humanos de las personas LGBTI 

El siguiente bloque de códigos me permite identificar el porcentaje de la percepción de las personas LGBTI en torno al respeto hacia la orientación sexual e identidad de género. Esta información se almacena en el objeto denominado `sociedad_respeta`. 


```r
sociedad_respeta <- dataLGBTI %>% 
  group_by(p501) %>% 
  summarise(recuento = n()) %>% 
  mutate(porcentaje = recuento/sum(recuento)*100)
```

Asimismo, se ejecutó el código que permite obtener el porcentaje de la percepción de las personas LGBTI en torno al reconocimiento de los derechos humanos. Los resultados se encuentran en el objeto denominado `sociedad_reconoce`. 



```r
sociedad_reconoce <- dataLGBTI %>% 
  group_by(p504) %>% 
  summarise(recuento = n()) %>% 
  mutate(porcentaje = recuento/sum(recuento)*100)
```


Posterior a ello, se creo la data frame denominada `respeta_reconoce` en donde se almacena la información que cruza los resultados sobre respecto a la orientación de género e identidad sexual y el reconocimiento de los derechos humanos de las personas LGBTI. 


```r
respeta_reconoce <- dataLGBTI %>% 
  group_by(p501,p504) %>% 
  summarise(recuento = n()) %>% 
  mutate(porcentaje = recuento/sum(recuento)*100) %>% 
  arrange(desc(porcentaje)) %>% 
  filter(!is.na(p501), !is.na(p504))
```

#### Discriminación y violencia ejercida hacia las personas LGBTI. 

Los siguientes bloques de códigos permiten identificar el porcentaje de personas LGBTI que experimentaron alguna manifestación de discriminación y/o violencia por grupo etáreo, además de información vinculada a los espacios/lugares/ámbitos donde indicaron haber experimentado discriminación y/o violencia y quienes la perpetraron. Al respecto, se crearon tres dataframes para almacenar los resultados obtenidos: `grupo_violencia`, `espacios_violencia` y `ejercen_violencia`. 



```r
grupo_violencia <- dataLGBTI %>% 
  group_by(gedad, p201) %>% 
  summarise(recuento = n()) %>% 
  mutate(porcentaje = recuento/sum(recuento)*100) %>% 
  arrange(desc(porcentaje)) %>% 
  filter (p201 =="Si")
```



```r
espacios_violencia <- dataLGBTI %>% 
  pivot_longer(c(p202_1,p202_2,p202_3,p202_4,p202_5,p202_6,p202_7,p202_8)) %>% 
  group_by(name,value) %>% 
  summarise(recuento = n()) %>% 
  mutate(porcentaje = recuento/sum(recuento)*100) %>% 
  filter(value == "Si") %>% 
  arrange(desc(porcentaje)) %>% 
  mutate(name = change_id(name, mis_variables)) %>% 
  mutate(name = str_squish(name) %>% str_remove(".*: ") %>% str_wrap(40))
```


```r
ejercen_violencia <- dataLGBTI %>% 
  pivot_longer(c(p203_1,p203_2,p203_3,p203_4,p203_5,p203_6,p203_7,p203_8)) %>% 
  group_by(name,value) %>% 
  summarise(recuento = n()) %>% 
  mutate(porcentaje = recuento/sum(recuento)*100) %>% 
  filter(value == "Si") %>% 
  arrange(desc(porcentaje)) %>% 
  mutate(name = change_id(name, mis_variables)) %>% 
  mutate(name = str_squish(name) %>% str_remove(".*: ") %>% str_wrap(40))
```


#### Participación ciudadana de personas LGBTI discriminadas y/o violentadas.

Para fines del trabajo nos propusimos extraer información en relación a la participación ciudadana de las personas LGBTI y, en específico, de aquellas que indicaron haber experimentado alguna manifestación de discriminación y/o violencia. Para ello, primero ejecutamos un bloque de códigos que nos permitió hacer un recuento de los principales tipos de organizaciones en los que  participan los miembros de la comunidad LGBTI. La información obtenida fue guardada en el objeto `organizaciones`. 


```r
organizaciones <- dataLGBTI %>% 
  pivot_longer(starts_with("p401_")) %>% 
  group_by(name,value) %>% 
  summarise(recuento = n()) %>% 
  mutate(porcentaje = recuento/sum(recuento)*100) %>% 
  filter(value == "Si") %>% 
  arrange(desc(porcentaje)) %>% 
  mutate(name = change_id(name, mis_variables)) %>% 
  mutate(name = str_squish(name) %>% str_remove(".*: ") %>% str_wrap(40))
```

En relación al código anterior, decidimos hacer un recuento y extraer los porcentajes respecto a cuáles son las principales acciones vinculadas a participación ciudadana desarrolladas por las personas LGBTI. Para ello fue necesario realizar el cruce entre las variables `tipo_organización` y `acciones`. El resultado fue almacenado en la dataframe `acciones_organizaciones`.


```r
acciones_organizaciones <- dataLGBTI %>% 
  pivot_longer(matches("p402_\\d$"), names_to = "acciones", values_to = "participa") %>% 
  pivot_longer(matches("p401_\\d\\d?$"), names_to = "tipo_organizacion", values_to = "vinculado") %>% 
  group_by(tipo_organizacion, acciones, vinculado, participa) %>% 
  summarise(recuento = n()) %>% 
  ungroup() %>%
  group_by(tipo_organizacion, acciones) %>%
  mutate(porcentaje = recuento/sum(recuento)*100) %>% 
  ungroup() %>%
  filter(participa == "Si", vinculado == "Si") %>%
  arrange(desc(porcentaje)) %>% 
  select(tipo_organizacion, acciones, porcentaje) %>%
  filter(tipo_organizacion %in%c("p401_11", "p401_4", "p401_5")) %>%
  filter(acciones %in%c("p402_1", "p402_2", "p402_6")) %>%
  mutate(tipo_organizacion = change_id(tipo_organizacion, mis_variables))%>%
  mutate(tipo_organizacion = str_squish(tipo_organizacion) %>% str_remove(".*: ")) %>%
  mutate(acciones = change_id(acciones, mis_variables)) %>% 
  mutate(acciones = str_squish(acciones) %>% str_remove(".*: ") %>% str_wrap(40))
```

A partir de lo anterior, resultó relevante para fines de la investigación identificar, del total de personas que declararon haber experimentado alguna manifestación de discriminación y/o violencia, el recuento y porcentaje de los principales tipos de organizaciones a los que están vinculados. Asimismo, fue necesario ejecutar un bloque de códigos que nos permita conocer las acciones de participaicón ciudada desarrolladas por ellas. La información obtenida fue registradas en los objetos `personasviol_organizaciones` y `personasviol_acciones`. 


```r
personasviol_organizaciones <- dataLGBTI %>% 
  pivot_longer(matches("p401_\\d\\d?$"), names_to = "tipo_organizacion", values_to = "vinculado") %>% 
  group_by(p201, tipo_organizacion, vinculado) %>% 
  summarise(recuento = n()) %>% 
  mutate(porcentaje = recuento/sum(recuento)*100) %>%
  ungroup() %>% 
  filter(vinculado == "Si", p201 == "Si") %>% 
  arrange(desc(porcentaje)) %>% 
  mutate(tipo_organizacion = change_id(tipo_organizacion, mis_variables)) %>% 
  mutate(tipo_organizacion = str_squish(tipo_organizacion) %>% str_remove(".*: ") %>% str_wrap(40))
```


```r
personasviol_acciones <- dataLGBTI %>% 
  pivot_longer(matches("p402_\\d$"), names_to = "acciones", values_to = "participa") %>% 
  group_by(p201, acciones, participa)%>% 
  summarise(recuento = n()) %>% 
  mutate(porcentaje = recuento/sum(recuento)*100) %>%
  ungroup() %>% 
  filter(participa == "Si", p201 == "Si") %>% 
  arrange(desc(porcentaje)) %>% 
  mutate(acciones = change_id(acciones, mis_variables)) %>% 
  mutate(acciones = str_squish(acciones) %>% str_remove(".*: ") %>% str_wrap(40)) 
```

En línea con lo anterior, quisimos extraer información sobre el nivel educativo de aquellas personas que indicaron haber experimentado alguna manifestación de discriminación y/o violencia y haber desarrollado alguna acción de participaicón ciudadana. Al respecto, se debe indicar que para identificar los principales niveles educativos se filtro los resultados con porcentaje mayor a 28. Los resultados del siguiente bloque de códigos se registraron en la data frame `niveleduc_participación`.


```r
  niveleduc_participacion <- dataLGBTI %>% 
  filter (p201 == "Si") %>% 
  relocate (p201) %>% 
  pivot_longer(matches("p402_\\d$"), names_to = "acciones", values_to = "participa") %>% 
  group_by(acciones, participa, p101) %>% 
  summarise(recuento = n()) %>% 
  mutate(porcentaje = recuento/sum(recuento)*100) %>%
  ungroup() %>% 
  filter(participa == "Si") %>% 
  arrange(desc(porcentaje)) %>% 
  filter(porcentaje > 28 ) %>%
  mutate(acciones = change_id(acciones, mis_variables)) %>% 
  mutate(acciones = str_squish(acciones) %>% str_remove(".*: ") %>% str_wrap(40)) %>%
  select(-participa, - recuento)
```

No obstante, asumiendo que podría existir algún sesgo en la variable **p101** que nos brinda información sobre los niveles educativos de las personas encuestadas, se realizó un recuento sobre la distribución de los niveles educativos respecto al total de las personas encuestadas. Los resultados se encuentran almacenados en el objeto `recuento_niveledu`.


```r
recuento_niveledu <- dataLGBTI %>% 
  group_by(p101) %>% 
  summarise(recuento = n()) %>% 
  mutate(porcentaje = recuento/sum(recuento)*100) %>% 
  arrange(desc(porcentaje)) 
```


Finalmente, identificamos que la variable **p116** que contiene las respuesta a la pregunta ¿expresa libremente su orientación de género/identidad de género? era de interés y podía ser cruzada con las variables **p201** que nos permite conocer si la persona encuestada experimentó alguna situación de discriminación y/O violencia y la variable **acciones**. Así obtuvimos el recuento y porcentaje de acciones de participación ciudadana realizadas de quienes indicaron haber experimentado alguna manifestación de discriminación y/o violencia y dependiendo si expresan o no libremente su orientación sexual e identidad de género. Se creo el objeto `expresion_participa` para guardar la información obtenida. 


```r
expresion_participa <- dataLGBTI %>%
  filter (p201 == "Si") %>% 
  relocate (p201) %>% 
  pivot_longer(matches("p402_\\d$"), names_to = "acciones", values_to = "participa") %>% 
  group_by(acciones, participa, p116) %>% 
  summarise(recuento = n()) %>% 
  mutate(porcentaje = recuento/sum(recuento)*100) %>%
  ungroup() %>% 
  filter(participa == "Si") %>% 
  mutate(acciones = change_id(acciones, mis_variables)) %>% 
  mutate(acciones = str_squish(acciones) %>% str_remove(".*: ") %>% str_wrap(40)) %>%
  select(-participa, - recuento)
```

## Resultados

### Aún nos están estigmatizando: Percepción sobre el respeto y avance en el reconocimiento de los derechos humanos. 

En el año 2015, la Defensoría del Pueblo indicó, a través del Informe Derechos Humanos de las personas LGBTI: Necesidad de una política pública para la igualdad en el Perú que "además de cambios institucionales, legales y de política pública, se debe transformar las mentes y las consciencias de las personas para el respeto por la diversidad y erradicar la discriminación" (pág. 12)

Al respecto, empleando un bloque de códigos podemos graficar a través de una barra de datos, la percepción que las personas LGBTI tuvieron en torno al respeto de la población hacia su orientación de género e identidad de género, además del avance en el reconocimiento de sus derechos humanos en el año 2017. 


```r
sociedad_respeta %>% 
  ggplot(aes(porcentaje, p501)) +
  geom_col() +
  geom_label(aes(label = round(porcentaje, 1)))+
  labs(title = "Respeto a la orientación sexual/identidad de género", 
       caption = "Fuente: INEI- Primera Encuesta para personas LGBTI 2017") +
  labs(x = "Porcentaje",
       y = "Percepción de personas LGBTI")
```

<img src="02-LZM_files/figure-html/unnamed-chunk-17-1.png" width="672" />

De acuerdo al gráfico podemos identificar que del total de personas LGBTI encuestadas el **55.5%** manifestó que la población peruana **no respeta su orientación sexual e identidad de género**; mientras que un 22% indicó que sí siente ser respetada.



```r
sociedad_reconoce %>% 
  ggplot(aes(porcentaje, p504)) +
  geom_col() +
  geom_label(aes(label = round(porcentaje, 1)))+
  labs(title = "Avance en el reconocimiento de los derechos humanos de las \n personas LGBTI en el Perú", 
       caption = "Fuente: INEI- Primera Encuesta para personas LGBTI 2017") +
  labs(x = "Porcentaje",
       y = "Percepción de personas LGBTI")
```

<img src="02-LZM_files/figure-html/unnamed-chunk-18-1.png" width="672" />

Asimismo, de acuerdo a la información brindada por la población encuestada, la principal percepción en torno al avance del reconocimiento de los derechos humanos es negativa. Un 54.8% considera que en Perú existe un mal avance, un 25.7% que es regular y solo el 3.2% considera que hay un buen avance.

En línea con lo anterior, a continuación ejecutamos un bloque de códigos que nos permite obtener un gráfico de barras a partir del cruce de las varibles **501** y **504**. Al respecto, la variable **501** se encuentra filtrada y presenta información en función a los valores Sí, No, No sé. Así, la información obtenida por la variable **504** se ordenará en estos tres grupos y de presentará la información porcentual por categorías "No sé/ no respondo", "Malo", "Regular" y "Bueno".


```r
respeta_reconoce %>% 
  ggplot(aes(porcentaje, p504)) +
  geom_col() +
  facet_grid(rows = "p501")+
  geom_label(aes(label = round(porcentaje, 1)))+
  labs(caption = "Fuente: INEI- Primera Encuesta para personas LGBTI 2017") +
  labs(x = "Porcentaje",
       y = "Recocimiento de derechos humanos en personas LGBTI")
```

<img src="02-LZM_files/figure-html/unnamed-chunk-19-1.png" width="672" />

En función al gráfico, podemos observar que del total de personas encuestadas, el mayor porcentaje se ubica en el bloque "No" con un 71.8%. Es decir que el grupo que indicó que la sociedad **no respeta** su orientación sexual e identidad de género también afirma, en un **71.8%**, que el avance del reconocimiento de los derechos humanos es malo. Asimismo, del total de personas que indicaron que la sociedad peruana **si respeta su orientación sexual e identidad de género el 47.9% también manifestó un mal avance en el país**. 

De acuerdo al Informe de la II Encuesta Nacional de Derechos Humanos, para el 2019, la población LGBTI que fue encuestada manifestó que el derecho que percibe como más vulnerado es el de un trato digno y no ser discriminación (51%). De acuerdo a las estadísticas, el segundo derecho vulnerado sería la libertad de expresión y opinión (37%). Asimismo, el informe indica que un grupo de la población peruana (45%) considera que la homosexualidad es producto de algún trauma o que las personas trans viven confundidas. 



### El maltrato gratuito: Espacios y perpetradores de discriminación y/o violencia. 

En relación con los resultados anteriores, resulta importante mencionar que en el Informe Defensorial N° 175, la Defensoría del Pueblo advertía que las personas LGBTI son víctimas de violencia y discriminación a razón de los estigmas, estereotios y prejuicios vinculados a su orientación sexual y/o identidad de género. 

En ese sentido, cobra sustento que en el 2017, el 63% de encuestadas que participaron de la Primera Encuesta Virtual para personas LGBTI hayan indicado haber sido víctimas de actos de discriminación y/o violencia.

El siguiente gráfico ayuda a identificar porcentualmente cuáles son los principales grupos etáreos que estarían comunicando el haber experimentado con mayor incidencia manifestaciones de discriminación y/o violencia. 


```r
grupo_violencia %>% 
  ggplot(aes(porcentaje, gedad)) +
  geom_col()+
  geom_label(aes(label = round(porcentaje, 1)))+
  labs(title = "Porcentaje de violencia recibida por grupos etáreos", 
       caption = "Fuente: INEI- Primera Encuesta para personas LGBTI 2017") +
  labs(x = "Porcentaje",
       y = "Grupos etáreos")
```

<img src="02-LZM_files/figure-html/unnamed-chunk-20-1.png" width="672" />

Del gráfico, podemos identificar que existen cuatro grupos etáreos que registran mayores porcentajes respecto a haber sido víctima de discriminación y/o violencia. El primer grupo etáreo es del rango de 25 a 29 años con un 65.1%, el segundo de 30 a 34 años con un 63.3%, el tercer grupo es del rango de 20 a 24 años con un 62.4% y el cuarto de 35 a 39 años con un 61.1%. 

Para el 2019, los resultados de la II Encuesta Nacional de Derechos Humanos arroja que en el Perú, una de las poblaciones que se percibe como más discriminada son las personas LGBTI.  Del total de encuestados, la mayoría (47%) indicó que las personas homosexuales, trans y bisexuales se encuentran en el grupo de **muy discriminados**. Junto con ello, se indica que para ese año la tercera razón (36%) de discriminación en el Perú es la orientación sexual. 

En línea con lo anterior, presentamos dos gráficos de barras con información relevante. El primero expone un gráfico de barras que presenta los principales ámbitos en donde las personas LGBTI han experimentado manifestaciones de discriminación y violencia; mientras que el segundo identifica, según lo manifestado  por las personas encuestadas, los principales perpetradores de discriminación y/O violencia.



```r
espacios_violencia %>% 
  ggplot(aes(porcentaje, name)) +
  geom_col()+
  geom_label(aes(label = round(porcentaje, 1)))+
    labs(title = "Principales espacios de violencia", 
       caption = "Fuente: INEI- Primera Encuesta para personas LGBTI 2017") +
  labs(x = "Porcentaje",
       y = "Espacios/Lugares")
```

<img src="02-LZM_files/figure-html/unnamed-chunk-21-1.png" width="672" />

De la información brindada, podemos observar que los espacios públicos como parques, playas, plazas y vía pública son identificados en un 39.2% como el principal ámbito en donde las personas LGBTI experimentaron ser víctimas de alguna muestra de discriminación y/o violencia. El segundo ámbito identificado es el educativo con un 34.1%. 

Si consideramos que actualmente no se cuenta con una segunda encuesta virtual para personas LGBTI u otro instrumento específico para esta población que permita recabar información estadística desde el Estado, los resultados presentados durante el 2017 son preocupantes porque evidencian que los espacios públicos son asimilados como ámbitos inseguros para personas LGBTI, en donde existe una potencial vulneración al derecho a un trato digno y libertad de expresión y opinión. 

Para autoras como Fraser, la instalación de una esfera pública homogénea aisla los espacios discursivos, cierra la puertas al debate e imposibilita la igualdad de participación debido a un marco institucional que crea grupos sociales dominanres y subordinados (1997, pag 95-133)



```r
ejercen_violencia %>% 
  ggplot(aes(porcentaje, name)) +
  geom_col()+
  geom_label(aes(label = round(porcentaje, 1)))+
    labs(title = "Principales perpetradores de violencia hacia \n personas LGBTI", 
       caption = "Fuente: INEI- Primera Encuesta para personas LGBTI 2017") +
  labs(x = "Porcentaje",
       y = "Personas / Perfiles")
```

<img src="02-LZM_files/figure-html/unnamed-chunk-22-1.png" width="672" />
Asimismo, considerando que los resultados posicionan como los principales perpetradores de discriminaicón y/o violancia hacia las personas LGBTI a compañeros(as) de escuela y padres de compañeros(as) con 33.1%, preocupa las consecuencias que estaría generando la violencia homofóbica o transfóbica en el ámbito educativo. 

De acuerdo a la Defensoría del Pueblo (2015), este tipo de violencia genera en los estudiantes dificultades para prestar atención en clases, empeorar calificaciones, abandonar o cambiar de institución. 

Junto con ello, también resulta importante leer estos resultados en su relación con la construcción de la ciudadanía y la democracia. A saber, según estudios que analizan ambos temas, las familias esperan que la democracia y la escuela ayuden a construir mejores condiciones de vida y oportunidades. En esa línea, el ámbito educativo es crucial para mejorar la relación entre ciudadanos, Estado y demoracia si el sistema educativo brinda un servicio de calidad para todos y todas (2015, pág 6) 


### Participación ¿ciudadana?: Vinculación con organizaciones y principales acciones.  

Al igual que el término democracia, el termino participación ciudadana ha sido empleado de manera diversa, en contextos y propósitos diferenciados. Para autores como Serrano, resulta importante diferenciar la participación ciudadana y la participación política ciudadana; para él (2009), tomando como referencia a Cunill, la participación ciudadana está orientada a promover y crear nuevos mecanismos para que la administración conozca mejor las actividades de sus administrados; en ese sentido, se pueda generar una colaboración conjunta para la ejecución de algunas tareas o se sustituya al Estado en la relación de determinadas funciones. 

En esa línea, considera que la participación ciudadana puede ser comprendida como una herramienta importante que ayuda a advertir la necesidad de optimizar los procedimientos institucionales frente a la pérdida de eficacia de sus mecanismos tradicionales. Así, la participación ciudadana puede ser ubicada como herramienta para la  gestión.

Diferente a ello, menciona que la participación política ciudadana se ejerce cuando existe un interés de tipo político; es decir, "la búsqueda de la trascendencia e incidencia de las opiniones de los particulares en la toma de decisiones de interéses públicos que de manera tradicional están en manos de partidos políticos y grupos de poder"

Para fines del presente proyecto, nos resulta interesante tomar la propuesta de Velásquez (2010), quien habla de la participación como proceso que no se restringue a lo institucional-normativo. Para él, la participaicón ciudadana "es el proceso mdiante el cual diferntes actores (sociales, económicos, políticos), en forma individual o colectiva y en función de sus necesidades, intereses, recursos y motivaciones, intervienen en el escenario públilco con el fin de obtener bienes y sercicios públicos y/o de incidir en la definición de ausntos de interpes colectivo" 

Así, diferencia este tipo de participación de la político-electoral, oarticipaicón social o comunitaria Para él se trata de la incidencia en la formulación, ejecución y seguimiento de las políticas públicas. 

En tal sentido, es relevante indicar que, en función a la revisión de la data y la manera en cómo se encuentran configuradas las preguntas sobre participación ciudadana, no se cuenta con información suficiente que ayude a comprender la repercusión o incidencia que las personas LGBTI estarían generando en el marco del ciclo de las políticas públicas. A diferencia de ello, solo sería posible identificar la vinculación que tienen con determinados tipos de organizaciones y las principales acciones que desarrollan.  

En ese sentido, a continuación se presenta un gráfico con la distribución porcentual de los principales tipos de organización a los que estarían adcritas o con las que mantendrían vinculación las personas LGBTI. 



```r
organizaciones %>% 
  ggplot(aes(porcentaje, name)) +
  geom_col()+
  geom_label(aes(label = round(porcentaje, 1)))+
    labs(title = "Vinculación de personas LGBTI en organizaciones", 
       caption = "Fuente: INEI- Primera Encuesta para personas LGBTI 2017") +
  labs(x = "Porcentaje",
       y = "Tipo de organizaciones")
```

<img src="02-LZM_files/figure-html/unnamed-chunk-23-1.png" width="672" />

El gráfico nos muestra que del total de las personas encuestadas la mayoría (34%) no tiene ninguna vinculación con organizaciones. También indica que el segundo bloque mayoritario manifiesta que está vinculado con organizaciones LGBTI (18.2%)  y que entre los menores porcentajes se encuentra la participación en organizaciones políticas (3.7%)

En relación a la vinculación con alguna organización o no, el gráfico que se muestra a continuación muestra las principales acciones en las que participan los miembros de la comunidad LGBTI.



```r
acciones_organizaciones %>% 
  ggplot(aes(porcentaje, acciones)) +
  geom_col() +
  facet_wrap(~tipo_organizacion, nrow = 4)+
  geom_label(aes(label = round(porcentaje, 1)))+
  labs(caption = "Fuente: INEI- Primera Encuesta para personas LGBTI 2017") +
  labs(x = "Porcentaje",
       y = "Acciones")
```

<img src="02-LZM_files/figure-html/unnamed-chunk-24-1.png" width="672" />

Según lo registrado, podemos observar que si bien la mayoría de personas encuestadas manifestó no pertenecer a ninguna organización no se encuentran ajenas a desarrollar acciones vinculadas a participación ciudadana. Así lo manifestó el 23.8% que indicó haber participado en una manifestación o marcha. Esta acción también es la que predomina en las personas que indicaron pertenecer a organizaciones LGBTI o de estudiantes y/o jóvenes. 

Para fines de la investigación, nos parece relevante los resultados obtenidos en el siguiente gráfico. De acuerdo a la información, no existen cambios porcentuales respecto a la participación o vinculación a una organización en personas que indicaron haber sido víctimas de discriminación y violencia. Al respecto, también observamos que del total de personas la mayoría no se encuentra vinculada con ninguna organización (41%);asimismo, el segundo grupo mayoritario también indica pertenecer a organizaciones LGBTI (24%), seguido de un 14.7 que mantiene vinculación con organizacones de estudiantes y/o jóvenes. Entre los menores porcentajes también se ubican las organizaciones políticas (4.4%).


```r
personasviol_organizaciones %>% 
  ggplot(aes(porcentaje, tipo_organizacion)) +
  geom_col()+
  geom_label(aes(label = round(porcentaje, 1)))+
    labs(title = "Participación por tipo de organizaciones de personas \n LGBTI violentadas o discriminadas", 
       caption = "Fuente: INEI- Primera Encuesta para personas LGBTI 2017") +
  labs(x = "Porcentaje",
       y = "Tipo de organizaciones")
```

<img src="02-LZM_files/figure-html/unnamed-chunk-25-1.png" width="672" />

Ahora bien, respecto a las acciones de participación ciudadana desarrolladas por personas que experimentaron discriminación y/o violencia, identificamos que la mayoría opta por participar en una manifstación o marcha (53.3%); a este porcentaje le sigue un 33.3 que afirma haber desarrollado acciones vinculadas a votar por alguna autoridad. El menor porcentaje lo registra postular a cargos públicos con un 1.4%. 



```r
personasviol_acciones %>% 
  ggplot(aes(porcentaje, acciones)) +
  geom_col()+
  geom_label(aes(label = round(porcentaje, 1)))+
    labs(title = "Acciones desarrolladas por personas LGBTI violentadas \n o discriminadas", 
       caption = "Fuente: INEI- Primera Encuesta para personas LGBTI 2017") +
  labs(x = "Porcentaje",
       y = "acciones")
```

<img src="02-LZM_files/figure-html/unnamed-chunk-26-1.png" width="672" />

A partir de los resultados obtenidos, quisimos observar el nivel educativo al que pertenecen los encuestados por acciones. Para ello, calculamos en primer lugar la distribución de la muestra. Al respecto, ésta presenta como categorías con mayor porcentaje a **superior universitaria Incompleta con 29.3%** y **superior universitaria completa con 27.6%**, de acuerdo al siguiente gráfico. 


```r
recuento_niveledu %>% 
  ggplot(aes(porcentaje, p101)) +
  geom_col() +
  geom_label(aes(label = round(porcentaje, 1)))+
  labs(title = "Distribución del nivel educativo en la muestra", 
       caption = "Fuente: INEI- Primera Encuesta para personas LGBTI 2017") +
  labs(x = "Porcentaje",
       y = "Nivel educativo")
```

<img src="02-LZM_files/figure-html/unnamed-chunk-27-1.png" width="672" />

En línea con el gráfico anterior, observamos que los resultados que arroja el siguiente podrían ser reflejo de los anteriores. En ese sentido, no permite ver que la categoría nivel educativo determine el desarrollo de las acciones participativas. 


```r
niveleduc_participacion %>% 
  ggplot(aes(porcentaje, acciones)) +
  geom_col(aes(fill = p101)) +
  geom_label(aes(label = round(porcentaje, 1)), position = position_stack(vjust = 0.5))+
  labs(caption = "Fuente: INEI- Primera Encuesta para personas LGBTI 2017") +
  labs(x = "Porcentaje",
       y = "p101")
```

<img src="02-LZM_files/figure-html/unnamed-chunk-28-1.png" width="672" />

A continuación observamos que del total de la población que manifestó experimentar alguna muestra de discrimininación y/o violencia, el mayor porcentaje de respuestas (64.7%) se ubica en el bloque de personas que indicaron **no expresar librevemete su orientación sexual o identidad de género** precisando que **no participa de ninguna acción**.El segundo porcentaje mayoritario también se ubica en el mismo bloque indicando *ser parte de procesos de presupuesto participativo** (61.5%). Respecto al bloque que indicó **expresar librevemte su orientación sexual o identidad de género** el 50.8% manifestó que **participó en reuniones con autoridades**, seguido de un 45.5% que señaló haber **mandado una carta a alguna autoridad**


```r
expresion_participa %>%
  ggplot(aes(porcentaje, acciones)) +
  geom_col(aes(fill = p116), position = position_dodge(1)) +
  geom_label(aes(label = round(porcentaje, 1), group = p116), 
             position = position_dodge(1), size = 2.5)+
  labs(caption = "Fuente: INEI- Primera Encuesta para personas LGBTI 2017") +
  labs(x = "Porcentaje",
       y = "acciones",
       fill = "Expresa libremente"
       )
```

<img src="02-LZM_files/figure-html/unnamed-chunk-29-1.png" width="672" />


## Conclusiones

La exploración de variables vinculadas a percepción dan cuenta que existe un escenario desfavorable en torno al respeto hacia la orientación sexual e identidad de género de las personas LGBTQ+, así como el avance en el reconocimiento de sus derechos humanos. 

De acuerdo a los porcentajes obtenidos se concluye que las personas que indicaron que la sociedad no respeta su orientación sexual e identidad de género manifiestan como respuesta frecuente (71.8%) que el avance en el reconocimiento de los derechos humanos es malo. La misma afirmación fue identificada como respuesta frecuente (47.9%) por quienes indicaron que la sociedad sí respeta su orientación sexual e identidad de género.  

Si bien el INEI no ha desarrollado la Segunda Encuesta Virtual para personas LGBTI u otra encuesta orientada específicamente a obtener información sobre este grupo poblacional, se cuenta con información estadística generada por otros entidades o instituciones que guardar coherencia con la obtenida en la Primera Encuesta Virtual para personas LGBTI. Por ejemplo, la II Encuesta de Derechos Humanos indica que para el 2019 la población LGBTI que fue encuestada manifestó que el derecho que percibe como más vulnerado es el de un trato digno y no ser discriminación (51%). 

El escenario desfavorable en torno al respecto a la orientación sexual e identidad de género de las personas LGBTQ+ y los ínfimos avances que se tienen en materia de reconocimiento de derechos humanos perpetúan acciones y comportamientos de discriminación y/o violencia en espacios públicos y educativos. Esto podría estar limitando la participaicón en la esfera pública de las personas LGBTI si consideramos que del total de encuestados que manifestó experimentar alguna muestra de discrimininación y/o violencia, el mayor porcentaje (64.7%) se ubicó en el bloque no expresar librevemente su orientación sexual o identidad de género precisando que no participa de ninguna acción. 

Los resultados obtenidos permiten afirmar que para comprender la participación ciudadana de las personas LGBTQ+, en los términos que este proyecto propone, se requiere del desarrollo y aplicación de mecanismos permanentes e idóneos que generen información estadística confiable y actualizada sobre la situación de los derechos humanos de este grupo poblacional, en realación a ámbitos como educación, salud, integridad, comportamiento electoral y otros.


Al respecto, resulta importante indicar que para el ciudadano, el Estado representa una organización, una posibilidad, una acción y un responsable respecto a la provisión de diferentes bienes y servicios que deberían cubrir necesidades insatisfechas que atienden expectativas de calidad de vida (Ausejo, 2008). Sin embargo, en momentos donde se cuestiona el accionar de las autoridades y la legitimidad de las instituciones, por la carencia de atención o insuficientes mecanismos que hagan visibles las diferencias, tenderíamos a pensar que es resposabilidad del ciudadano comprender y participar de procesos que inciden en la calidad de vida de la colectividad. 

Pero ¿qué ocurre si los espacios públicos en donde debería ser factible manifestar las demandas representan escenarios de inseguridad, discriminación y violencia?. Asimismo, ¿cómo esperamos que las personas LGBTI desarrollen y ejerza una participación ciudadana no condicionada si los espacios educativos violentan y estigmatizan aquello que está fuera del esquema heteronormativo? 

Finalmente, esta aproximación permite identificar limitaciones y abrir otras preguntas de investigación que son necesarias para pensar la configuración de la participación ciudadana en las personas LGBTQ+. 


## Referencias 


Ausejo (2008) De las Políticas Públicas a la gestión pública: una visión sistémica. Pease, G.; Villafranca, L.(Ed) El papel de las políticas públicas (29-36) Recuperado de: https://escuela.pucp.edu.pe/gobierno/images/documentos/publicaciones/reforma2009.pdf

Cunill, N. (1997) Repensando lo público a través de la sociedad. Nuevas formas de gestión pública y representación social, Venezuela, Nueva Sociedad. Recuperado de: http://sitp.pichincha.gob.ec/repositorio/diseno_paginas/archivos/Repensando%20lo%20p%C3%BAblico%20a%20trav%C3%A9s%20de%20la%20sociedad.pdf

Dahl, R. (1989) La Poliarquía. Participaicón y oposición, Madrid: Editorial Tecnos (Grupo Anaya, S.A)

Defensoría del Pueblo (2016) Informe Defensorial N° 175 "Derechos humanos de las personas LGBTI: Necesidades de una política pública para la igualdad en el Perú". Recuperado de: https://www.defensoria.gob.pe/wp-content/uploads/2018/05/Informe-175--Derechos-humanos-de-personas-LGBTI.pdf

Defensoría del Pueblo (2018) Informe de Adjuntía N° 007-2018-DP/ADHPD. "A dos años del Informe Defensorial N° 175 "Derechos humanos de las personas LGBTI: Necesidades de una política pública para la igualdad en el Perú". Recuperado de: https://www.defensoria.gob.pe/wp-content/uploads/2018/12/Informe-Defensorial-N%C2%B0-007-2018-DPADHPD-%E2%80%9CA-2-a%C3%B1os-del-Informe-Defensorial-N%C2%B0-175.-Estado-actual-de-los-derechos-de-las-personas-LGBTI%E2%80%9D.pdf 

Fraser, N.(1997) Iustitia Interumpa: Reflexiones críticas desde la posición "postsocialista". "Pensando de nuevo la esfera pública". Santafé de Bogotá: Siglo del hombre editores. Facultad de Derecho, 1997.pp, 95-133. 

Held, D. (2008) Modelos de Democracia, Madrid: Alianza Editorial.

Instituto de Estudios Peruanos (2016) La ciudadanía desde la escuela: democracia y ciudadanía. Recuperado de: https://iep.org.pe/wp-content/uploads/2015/12/la_ciudadania_desde_la_escuela__democracia_y_ciudadania-1.pdf

Instituto Nacional de Estadística e Informática (2018) Primera Encuesta Virtual para Personas LGBTI. Principales Resultados. Recuperado de: https://www.inei.gob.pe/media/MenuRecursivo/boletines/lgbti.pdf

López, J.;López, J. (2009) La participaicón política ciudadana; sus límites y controles institucionales en el caso mexicano. Estudios Políticos, vol. 9, num. 16, enero-abril, 2009, pp. 9-46. Universidad Nacional Autónoma de México. Recuperado en:https://www.redalyc.org/pdf/4264/426439540001.pdf 

John, R. (2012) Participación ciudadana de la población LGBT en la localidad de Chapinero del 2007 al 2009, (tesis de maestría)  Bogotá D.C-Colombia, Pontificia Universidad Javeriana, Maestría en Estudios Políticos. Recuperado de: https://repository.javeriana.edu.co/bitstream/handle/10554/1389/RodriguezGarciaJohnMarlon2011.pdf;jsessionid=793E40824B1DD5FB7CCD6EE7288BA3AF?sequence=1

Ministerio de Justicia y Derechos Humanos (2019) II Encuesta Nacional de Derechos Humanos. Informe Completo. Recuperado de: https://cdn.www.gob.pe/uploads/document/file/1611180/3.-Informe-completo-de-la-II-Encuesta-Nacional-de-Derechos-Humanos.pdf.pdf

Restrepo, J. (2005) Estándares Básicos en Competencias Ciudadanas: ¿Cuál concepción ciudadana?: Una aproximación teórica al problema de la formación ciudadana, (tesis de maestría), Bogotá D.C-Colombia, Pontificia Universidad Javeriana, Maestría en Estudios Políticos. 

Serrano, J. (2007) "Hacerse sujetos públicos a propósito de la Marcha de la Ciudadanía LGBT en Bogotá", en: Revista Javeriana, año 74, num. 735, oo. 16-25. 

