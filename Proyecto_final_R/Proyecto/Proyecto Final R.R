library("tidyverse")
library("eurostat")
library("rio")

help(package = "eurostat")

#1)IMPORTAR LOS DATOS DE EUROSTAT :-Infractions enregistrées par la police par type d'infraction (crim_gen) + Lympiar la base de datos.

df <- get_eurostat("crim_gen",time_format = 'raw')
df <- label_eurostat(df, code = c("geo","iccs"))
df <- df %>% select(geo_code,geo,iccs_code,iccs,time,values) %>%
  mutate(time = as.numeric(time))%>%
  rename(infraccion = iccs, code_pais = geo_code, pais = geo, anyo = time, numinf = values)

#-limpio mas porque France(metropolitan)=france y el Total en linea me molesta entonces lo borro.
df <- df %>% filter(code_pais !="FX" & iccs_code != "TOTAL") %>% filter(code_pais !="TR")

df <-df %>% mutate(infraccion = case_when( 
  infraccion == "Intentional homicide" ~ "Homicidios",
  infraccion == "Acts causing harm or intending to cause harm to the person, injurious acts of a sexual nature and acts against property involving violence or threat against a person"  ~ "Actos violentos",
  infraccion == "Robbery"  ~ "Robos",
  infraccion == "Burglary of private residential premises"  ~ "Robo en residencias privadas",
  infraccion == "Theft of a motorized land vehicle"  ~ "Robo de vehiculos",
  infraccion == "Unlawful acts involving controlled drugs or precursors"  ~ "Actos ilicitos con drogas", TRUE ~ as.character(infraccion)))

df <-df %>% mutate(pais = case_when( 
  pais == "Germany (until 1990 former territory of the FRG)" ~ "Germany",
  pais == "England and Wales"  ~ "RU",
  TRUE ~ as.character(pais)))

rio::export(df, "./datos/df_crim_gen.rds")



#intro) PODEMOS TRABAJAR DESDE AQUI DIRECTAMENTE :-cargamos las librerias + Importamos los datos + miramos como son:
library("tidyverse")
library("gganimate")
library("viridis")
library("rio")
library("summarytools")
df <- rio::import("./datos/df_crim_gen.rds")
str(df)
summary.data.frame(df)
dfSummary(df)
skimr::skim(df)

#I) PREGUNTAS INTERESSANTES: EN GENERAL:

#1-Numero de infraccion por pais, cada anyo : hacer un mapa de Europa clasificado en 4 grupos los paises con mas infracciona menos infraccion para los anyos 1993, 2000,2007

# a) mapa que reprensenta en 1994, 2000 y 2007 Los paises europeos mas peligrosos en terminos de homicidios :
df1a <- df %>% filter(iccs_code == "ICCS0101") %>% 
  group_by(anyo, numinf) %>% 
  arrange(numinf) %>% 
  ungroup() %>% 
  group_by(iccs_code) %>% 
  mutate(cat_inf = cut_to_classes(numinf, n = 6)) %>% 
  ungroup()
              

geometrias <- get_eurostat_geospatial(resolution = "20", nuts_level = "0")
mapdata <- full_join(df1a, geometrias, by = c("code_pais" = "id"))


mapdata_1a <- mapdata %>%  filter(iccs_code == "ICCS0101", anyo %in% c(1994,2000,2007))

p <- ggplot(mapdata_1a) +
     geom_sf(aes(fill = cat_inf, geometry = geometry), color = "black", size = .1) +
     facet_wrap( ~ anyo) +
     scale_fill_brewer(palette = "RdYlBu", direction = -1)  +
     labs(title = "Número de homicidios en Europa en 1994, 2000 y 2007",
     subtitle = "(Para Europa)",
     fill = "Intencional homicide",
     caption = "Datos Eurostat") + 
     theme_linedraw() +
     coord_sf(xlim = c(-10,29), ylim = c(35,70))
p <- p + theme(plot.title = element_text(face = "bold"))+
         theme(axis.title.y = element_text(size=8))+
         theme(axis.title.x = element_text(size=8))
p

# b) mapa que reprensenta en 1998, 2000 y 2007 Los paises europeos mas peligrosos en terminos de robbos :
df1b <- df %>% filter(iccs_code == "ICCS0401") %>% 
  group_by(anyo, numinf) %>% 
  arrange(numinf) %>% 
  ungroup() %>% 
  group_by(iccs_code) %>% 
  mutate(cat_inf  = cut_to_classes(numinf, n = 6)) %>% 
  ungroup()
              
geometrias <- get_eurostat_geospatial(resolution = "20", nuts_level = "0")
mapdata <- full_join(df1b, geometrias, by = c("code_pais" = "id"))

mapdata_1b <- mapdata %>%  filter(iccs_code == "ICCS0401", anyo %in% c(1998,2000,2007))

p <- ggplot(mapdata_1b) +
     geom_sf(aes(fill = cat_inf, geometry = geometry), color = "black", size = .1) +
     facet_wrap( ~ anyo) +
     scale_fill_brewer(palette = "RdYlBu", direction = -1)  +
     labs(title = "Número de robos en Europa en 1998, 2000 y 2007",
     subtitle = "Datos Eurostat",
     fill = "Robbery",
     caption = "Datos Eurostat") + 
     theme_linedraw() +
     coord_sf(xlim = c(-10,29), ylim = c(35,70))
p <- p + theme(plot.title = element_text(face = "bold"))+
         theme(axis.title.y = element_text(size=8))+
         theme(axis.title.x = element_text(size=8))
p

#c) mapa que reprensenta en 1998, 2000 y 2007 Los paises europeos mas peligrosos en terminos de drogas :
df1c <- df %>% filter(iccs_code == "ICCS0601") %>% 
  group_by(anyo, numinf) %>% 
  arrange(numinf) %>% 
  ungroup() %>% 
  group_by(iccs_code) %>% 
  mutate(cat_inf  = cut_to_classes(numinf, n = 6)) %>% 
  ungroup()
              
geometrias <- get_eurostat_geospatial(resolution = "20", nuts_level = "0")
mapdata <- full_join(df1c, geometrias, by = c("code_pais" = "id"))

mapdata_1c <- mapdata %>%  filter(iccs_code == "ICCS0601", anyo %in% c(1998,2000,2007))

p <- ggplot(mapdata_1c) +
     geom_sf(aes(fill = cat_inf, geometry = geometry), color = "black", size = .1) +
     facet_wrap( ~ anyo) +
     scale_fill_brewer(palette = "RdYlBu", direction = -1)  +
     labs(title = "Número de actos ilícitos con drogas en Europa en 1998, 2000 y 2007",
     subtitle = "Datos Eurostat",
     fill = "Actos ilícitos con Drogas",
     caption = "Datos Eurostat") + 
     theme_linedraw() +
     coord_sf(xlim = c(-10,29), ylim = c(35,70))
p <- p + theme(plot.title = element_text(face = "bold"))+
         theme(axis.title.y = element_text(size=8))+
         theme(axis.title.x = element_text(size=8))
p

#2-Cuales son los 5 paises europeos donde hay mas a)homicidios b) Robos c) Actos violentos d) etc (hacerlo para cada typo de infraccion y como un grafico de barras ordenadas.

df2 <- df %>% select(anyo, pais, infraccion, numinf) %>% 
  group_by(anyo, pais, infraccion) %>% 
  summarise(sum(NN = numinf)) %>% 
  ungroup() %>% 
  rename(numinf_total = 4)

#para robos :
df2a <- df2 %>% filter(infraccion == "Robos" & anyo == 2007) %>% 
  group_by(pais) %>% 
  summarise(sum(NN = numinf_total)) %>% 
  ungroup() %>% 
  rename(numinf_robos = 2) %>% 
  arrange(desc(numinf_robos)) %>% 
  slice(c(1:5))

df2a <- df2a %>% 
  mutate(numinf_robos = forcats::as_factor(numinf_robos)) %>%
  mutate(numinf_robos = forcats::fct_infreq(numinf_robos))

p <- ggplot(df2a) +
     geom_col(mapping = aes(x = pais, y = fct_infreq(numinf_robos), fill = pais)) +
     scale_fill_brewer(palette = "Spectral", direction = -1)  +
     labs(title = "5 Países con más robos en 2007",
     fill = "Paises",
     caption = "Datos Eurostat") + 
     xlab(label = "Países") +
     ylab(label = "Número de robos") + 
     theme_classic()
p <- p + theme(plot.title = element_text(face = "bold"))+
         theme(axis.title.y = element_text(size=8))+
         theme(axis.title.x = element_text(size=8))
p

#para homicidios :
df2b <- df2 %>% filter(infraccion == "Homicidios" & anyo == 2007) %>% 
  group_by(pais) %>% 
  summarise(sum(NN = numinf_total)) %>% 
  ungroup() %>% 
  rename(numinf_homicidios = 2) %>% 
  arrange(desc(numinf_homicidios)) %>% 
  slice(c(1:5))

df2b <- df2b %>% 
  mutate(numinf_homicidios = forcats::as_factor(numinf_homicidios)) %>%
  mutate(numinf_homicidios = forcats::fct_infreq(numinf_homicidios))

p <- ggplot(df2b) +
     geom_col(mapping = aes(x = pais, y = fct_infreq(numinf_homicidios), fill = pais)) +
     scale_fill_brewer(palette = "Spectral", direction = -1)  +
     labs(title = "5 Países con más homiciodios en 2007",
     fill = "Paises",
     caption = "Datos Eurostat") + 
     xlab(label = "Países") +
     ylab(label = "Número de homicidios") + 
     theme_classic()
p <- p + theme(plot.title = element_text(face = "bold"))+
         theme(axis.title.y = element_text(size=8))+
         theme(axis.title.x = element_text(size=8))
p

#Para Drogas : 
df2c <- df2 %>% filter(infraccion == "Actos ilicitos con drogas" & anyo == 2007) %>% 
  group_by(pais) %>% 
  summarise(sum(NN = numinf_total)) %>% 
  ungroup() %>% 
  rename(numinf_drogas = 2) %>% 
  arrange(desc(numinf_drogas)) %>% 
  slice(c(1:5))

df2c <- df2c %>% 
  mutate(numinf_drogas = forcats::as_factor(numinf_drogas)) %>%
  mutate(numinf_drogas = forcats::fct_infreq(numinf_drogas))

p <- ggplot(df2c) +
     geom_col(mapping = aes(x = pais, y = fct_infreq(numinf_drogas), fill = pais)) +
     scale_fill_brewer(palette = "Spectral", direction = -1)  +
     labs(title = "5 Países con más actos ilícitos en relacion a las drogas en 2007",
     fill = "Paises",
     caption = "Datos Eurostat") + 
     xlab(label = "Países") +
     ylab(label = "Número de actos ilícitos con droga") + 
     theme_classic()
p <- p + theme(plot.title = element_text(face = "bold"))+
         theme(axis.title.y = element_text(size=8))+
         theme(axis.title.x = element_text(size=8))
p

#3-Para Francia, Alemania, Espana, Italia ver cual es la proporcion de cada una de las infracciones en las infracciones totales del pais (proporcion = x/sum(x)) hacer graficos de barras donde se vea la proporcion.
df3a <- df %>% select(anyo, pais, infraccion, numinf) %>%
  filter(pais == "France" & anyo == 2007) %>%
  mutate(Prop_infraccion = (numinf/sum(numinf))*100) %>%
  arrange(Prop_infraccion) %>%
  ungroup()

df3b <- df3a %>% mutate(Prop_infraccion = forcats::as_factor(Prop_infraccion)) 
df3b <- df3b %>% mutate(Prop_infraccion = forcats::fct_infreq(Prop_infraccion))


p1 <- ggplot(df3a) + geom_col(aes(x = pais, y = Prop_infraccion, fill = infraccion), position = "dodge2") +
     scale_fill_brewer(palette = "Reds", direction = -1) +
     labs(title = "Tipos de infracciones en Francia en 2007",
     fill = "Infracciones") +
     xlab(NULL) +
     ylab(label = "Proporción en %") + 
     theme_classic()
p1 <- p1 + theme(plot.title = element_text(face = "bold"))+
         theme(axis.title.y = element_text(size=12))
p1

p2 <- ggplot(df3b) + geom_col(aes(x = pais, y = Prop_infraccion, fill = infraccion), position = "dodge2")+
     scale_fill_brewer(palette = "Dark2", direction = 1) +
     labs(title = "Tipos de infracciones en Francia en 2007",
     fill = "Infracciones",
     caption = "Datos Eurostat") + 
     xlab(NULL) +
     ylab(label = "Proporcion en %") + 
     theme_classic()
p2 <- p2 + theme(plot.title = element_text(face = "bold"))+
         theme(axis.title.y = element_text(size=12))
p2
grid.arrange(p1,p2,nrow =1)

#Para Francia :
df3a <- df %>% select(anyo, pais, infraccion, numinf) %>%
  filter(pais == "France" & anyo == 2007) %>%
  mutate(Prop_infraccion = (numinf/sum(numinf))*100) %>%
  arrange(Prop_infraccion) %>%
  ungroup()  

p <- ggplot(df3a) + geom_col(aes(x = pais, y = Prop_infraccion, fill = infraccion), position = "dodge2") +
     scale_fill_brewer(palette = "Reds", direction = -1) +
     labs(title = "Tipos de infracciones en Francia en 2007",
     fill = "Infracciones") +
     xlab(NULL) +
     ylab(label = "Proporción en %") + 
     theme_classic()
p <- p + theme(plot.title = element_text(face = "bold"))+
         theme(axis.title.y = element_text(size=12))
p

df3b <- df3a %>% mutate(Prop_infraccion = forcats::as_factor(Prop_infraccion)) 
df3b <- df3b %>% mutate(Prop_infraccion = forcats::fct_infreq(Prop_infraccion)) 
p <- ggplot(df3b) + geom_col(aes(x = pais, y = Prop_infraccion, fill = infraccion), position = "dodge2")+
     scale_fill_brewer(palette = "Dark2", direction = 1) +
     labs(title = "Tipos de infracciones en Francia en 2007",
     fill = "Infracciones",
     caption = "Datos Eurostat") + 
     xlab(NULL) +
     ylab(label = "Proporcion en %") + 
     theme_classic()
p <- p + theme(plot.title = element_text(face = "bold"))+
         theme(axis.title.y = element_text(size=12))
p


#Para Espagna: 
df3c <- df %>% select(anyo, pais, infraccion, numinf) %>%
  filter(pais == "Spain" & anyo == 2007) %>%
  mutate(Prop_infraccion = (numinf/sum(numinf))*100) %>%
  arrange(Prop_infraccion) %>%
  ungroup()  

p <- ggplot(df3c) + geom_col(aes(x = pais, y = Prop_infraccion, fill = infraccion), position = "dodge2")+
     scale_fill_brewer(palette = "Reds", direction = -1) +
     labs(title = "Tipos de infracciones en Espana en 2007",
     fill = "Infracciones",
     caption = "Datos Eurostat") + 
     xlab(NULL) +
     ylab(label = "Proporción en %") + 
     theme_classic()
p <- p + theme(plot.title = element_text(face = "bold"))+
         theme(axis.title.y = element_text(size=12))
p

df3d <- df3c %>% mutate(Prop_infraccion = forcats::as_factor(Prop_infraccion)) 
df3d <- df3d %>% mutate(Prop_infraccion = forcats::fct_infreq(Prop_infraccion)) 
p <- ggplot(df3d) + geom_col(aes(x = pais, y = Prop_infraccion, fill = infraccion), position = "dodge2")+
     scale_fill_brewer(palette = "Dark2", direction = 1) +
     labs(title = "Tipos de infracciones en Espana en 2007",
     fill = "Infracciones",
     caption = "Datos Eurostat") + 
     xlab(NULL) +
     ylab(label = "Proporción en %") + 
     theme_classic()
p <- p + theme(plot.title = element_text(face = "bold"))+
         theme(axis.title.y = element_text(size=12))
p

#Para Francia, Espana, Alemania,RU al mismo tiempo:

df3e <- df %>% select(anyo, pais, infraccion, numinf) %>%
  filter(pais %in% c("France","Spain","Germany","RU") & anyo == 2007) %>%
  group_by(pais) %>%
  mutate(Prop_infraccion = (numinf/sum(numinf))*100) %>%
  ungroup()  

p <- ggplot(df3e) + geom_col(aes(x = pais, y = Prop_infraccion, fill = infraccion), position = "dodge2")+
     scale_fill_brewer(palette = "Reds", direction = -1) +
     labs(title = "Tipos de infracciones en Francia, Espana, Alemania y RU en 2007",
     fill = "Infracciones",
     caption = "Datos Eurostat") + 
     xlab(label = "Países") +
     ylab(label = "Proporción en %") + 
     theme_classic()
p <- p + theme(plot.title = element_text(face = "bold"))+
         theme(axis.title.y = element_text(size=12))+
         theme(axis.title.x = element_text(size=12))
p

df3f <- df3e %>% mutate(Prop_infraccion = forcats::as_factor(Prop_infraccion)) 
df3f <- df3f %>% mutate(Prop_infraccion = forcats::fct_infreq(Prop_infraccion)) 
p <- ggplot(df3f) + geom_col(aes(x = pais, y = Prop_infraccion, fill = infraccion), position = "dodge2")+
     scale_fill_brewer(palette = "Dark2", direction = 1) +
     labs(title = "Tipos de infracciones en Francia, España, Alemania y RU en 2007",
     fill = "Infracciones",
     caption = "Datos Eurostat") + 
     xlab(label = "Países") +
     ylab(label = "Proporción en %") + 
     theme_classic()
p <- p + theme(plot.title = element_text(face = "bold"))+
         theme(axis.title.y = element_text(size=12))+
         theme(axis.title.x = element_text(size=12))
p


#4 Mostrar la evolución del numero de actos ílicitos en europa con un ano base para poder comparar:

# a) en toda Europa desde el año 1993 al 2007 en un gráfico

df4a <- df %>% select(anyo, pais, infraccion, numinf) %>% 
               group_by(anyo, infraccion) %>% 
               summarise(sum(NN = numinf)) %>% 
               ungroup() %>%
               rename(numinf_total = 3)

df4a <-df4a %>% group_by(infraccion) %>% 
                mutate(rescaled_inf = 100*numinf_total/numinf_total[1])

p <-ggplot(df4a, mapping = aes(x = anyo, y = rescaled_inf, group = infraccion, color = infraccion)) + 
    geom_point() + 
    geom_line()+
    scale_colour_viridis_d() + 
    labs(title ="Evolución de las infracciones en Europa",
    subtitle = "(Para Europa)",
    caption = "Datos de Eurostat", 
    x = "Años",
    y = "Número de infracciones año base 1993") +
    scale_x_continuous(breaks = seq(1993, 2007, 2), limits = c(1993, 2007))+ 
    theme_linedraw()
p <- p + theme(plot.title = element_text(face = "bold"))+
         theme(axis.title.y = element_text(size=8))+
         theme(axis.title.x = element_text(size=8))
p



#b) Evolucion para Francia y Espana: 
 #Drogas:  
df4b1 <- df %>% select(anyo, pais, infraccion, numinf) %>% 
  group_by(anyo, infraccion, pais) %>%
  filter(pais %in% c("France","Spain") & infraccion == "Actos ilicitos con drogas") %>%
  summarise(sum(NN = numinf)) %>% 
  ungroup() %>%
  rename(numinf_total = 4)

df4b1 <-df4b1 %>% group_by(infraccion, pais) %>% mutate(rescaled_inf = 100*numinf_total/numinf_total[1]) %>% ungroup()

p <-ggplot(df4b1, mapping = aes(x = anyo, y = rescaled_inf, group = pais, color = pais)) + 
    geom_point() + 
    geom_line()+
    scale_colour_manual(values = c(France = "blue", Spain = "red"))+ 
    labs(title ="Evolución de los actos ilícitos relacionados a drogas",
    subtitle = "(Para Francia y España)",
    caption = "Datos de Eurostat", 
    x = "Años",
    y = "Número de infracciones año base 1993") +
    scale_x_continuous(breaks = seq(1993, 2007, 2), limits = c(1993, 2007))+ 
    theme_linedraw()
p <- p + theme(plot.title = element_text(face = "bold"))+
         theme(axis.title.y = element_text(size=8))+
         theme(axis.title.x = element_text(size=8))
p


#Homicidios :
df4b2 <- df %>% select(anyo, pais, infraccion, numinf) %>% 
  group_by(anyo, infraccion, pais) %>%
  filter(pais %in% c("France","Spain") & infraccion == "Homicidios") %>%
  summarise(sum(NN = numinf)) %>% 
  ungroup() %>%
  rename(numinf_total = 4)

df4b2 <-df4b2 %>% group_by(infraccion, pais) %>% mutate(rescaled_inf = 100*numinf_total/numinf_total[1]) %>% ungroup()

p <-ggplot(df4b2, mapping = aes(x = anyo, y = rescaled_inf, group = pais, color = pais)) + 
    geom_point() + 
    geom_line()+
    scale_colour_manual(values = c(France = "blue", Spain = "red")) + 
    labs(title ="Evolución de los homicidios",
    subtitle = "(Para Francia y España)",
    caption = "Datos de Eurostat", 
    x = "Años",
    y = "Número de homicidios año base 1993") +
    scale_x_continuous(breaks = seq(1993, 2007, 2), limits = c(1993, 2007))+ 
    theme_linedraw()
p <- p + theme(plot.title = element_text(face = "bold"))+
         theme(axis.title.y = element_text(size=8))+
         theme(axis.title.x = element_text(size=8))
p




#Robos :

df4b3 <- df %>% select(anyo, pais, infraccion, numinf) %>% 
  group_by(anyo, infraccion, pais) %>%
  filter(pais %in% c("France","Spain") & infraccion == "Robos") %>%
  summarise(sum(NN = numinf)) %>% 
  ungroup() %>%
  rename(numinf_total = 4)

df4b3 <-df4b3 %>% group_by(infraccion, pais) %>% mutate(rescaled_inf = 100*numinf_total/numinf_total[1]) %>% ungroup()

p <-ggplot(df4b3, mapping = aes(x = anyo, y = rescaled_inf, group = pais, color = pais)) + 
    geom_point() + 
    geom_line()+
    scale_colour_manual(values = c(France = "blue", Spain = "red")) + 
    labs(title ="Evolución de los Robos",
    subtitle = "(Para Francia y España)",
    caption = "Datos de Eurostat", 
    x = "Años",
    y = "Número de robos año base 1993") +
    scale_x_continuous(breaks = seq(1993, 2007, 2), limits = c(1993, 2007))+ 
    theme_linedraw()
p <- p + theme(plot.title = element_text(face = "bold"))+
         theme(axis.title.y = element_text(size=8))+
         theme(axis.title.x = element_text(size=8))
p

#Robos de vehiculos :
df4b4 <- df %>% select(anyo, pais, infraccion, numinf) %>% 
  group_by(anyo, infraccion, pais) %>%
  filter(pais %in% c("France","Spain") & infraccion == "Robo de vehiculos") %>%
  summarise(sum(NN = numinf)) %>% 
  ungroup() %>%
  rename(numinf_total = 4)

df4b4 <-df4b4 %>% group_by(infraccion, pais) %>% mutate(rescaled_inf = 100*numinf_total/numinf_total[1]) %>% ungroup()

p <-ggplot(df4b4, mapping = aes(x = anyo, y = rescaled_inf, group = pais, color = pais)) + 
    geom_point() + 
    geom_line()+
    scale_colour_manual(values = c(France = "blue", Spain = "red")) + 
    labs(title ="Evolución de robos de vehículo",
    subtitle = "(Para Francia y España)",
    caption = "Datos de Eurostat", 
    x = "Años",
    y = "Número de robos de vehículo año base 1993") +
    scale_x_continuous(breaks = seq(1993, 2007, 2), limits = c(1993, 2007))+ 
    theme_linedraw()
p <- p + theme(plot.title = element_text(face = "bold"))+
         theme(axis.title.y = element_text(size=8))+
         theme(axis.title.x = element_text(size=8))
p

#Robos de residencia :

df4b5 <- df %>% select(anyo, pais, infraccion, numinf) %>% 
  group_by(anyo, infraccion, pais) %>%
  filter(pais %in% c("France","Spain") & infraccion == "Robo en residencias privadas") %>%
  summarise(sum(NN = numinf)) %>% 
  ungroup() %>%
  rename(numinf_total = 4)

df4b5 <-df4b5 %>% group_by(infraccion, pais) %>% mutate(rescaled_inf = 100*numinf_total/numinf_total[1]) %>% ungroup()

p <-ggplot(df4b5, mapping = aes(x = anyo, y = rescaled_inf, group = pais, color = pais)) + 
    geom_point() + 
    geom_line()+
    scale_colour_manual(values = c(France = "blue", Spain = "red")) + 
    labs(title ="Evolución de los robos de residencia",
    subtitle = "(Para Francia y España)",
    caption = "Datos de Eurostat", 
    x = "Años",
    y = "Número de robos de residencia año base 1993") +
    scale_x_continuous(breaks = seq(1993, 2007, 2), limits = c(1993, 2007))+ 
    theme_linedraw()
p <- p + theme(plot.title = element_text(face = "bold"))+
         theme(axis.title.y = element_text(size=8))+
         theme(axis.title.x = element_text(size=8))
p

#Actos violentos :

df4b6 <- df %>% select(anyo, pais, infraccion, numinf) %>% 
  group_by(anyo, infraccion, pais) %>%
  filter(pais %in% c("France","Spain") & infraccion == "Actos violentos") %>%
  summarise(sum(NN = numinf)) %>% 
  ungroup() %>%
  rename(numinf_total = 4)

df4b6 <-df4b6 %>% group_by(infraccion, pais) %>% mutate(rescaled_inf = 100*numinf_total/numinf_total[1]) %>% ungroup()

p <-ggplot(df4b6, mapping = aes(x = anyo, y = rescaled_inf, group = pais, color = pais)) + 
    geom_point() + 
    geom_line()+
    scale_colour_manual(values = c(France = "blue", Spain = "red")) + 
    labs(title ="Evolución de los Actos violentos",
    subtitle = "(Para Francia y España)",
    caption = "Datos de Eurostat", 
    x = "Años",
    y = "Número de actos violentos año base 1993") +
    scale_x_continuous(breaks = seq(1993, 2007, 2), limits = c(1993, 2007))+ 
    theme_linedraw()
p <- p + theme(plot.title = element_text(face = "bold"))+
         theme(axis.title.y = element_text(size=8))+
         theme(axis.title.x = element_text(size=8))
p

#II) OTRAS PREGUNTAS INTERESSANTES: 

#1-Esta Francia y Espana por debajo o por encima de la media europea en numeros de infraccion? (mapa que asigna un color diferente a cada pais si esta por encima o debajo de la media europea)
df_1 <-df %>%
  select(anyo,code_pais,pais,infraccion,numinf) %>%
  group_by(pais) %>%
  mutate(mean_pais = mean(numinf)) %>%
  ungroup()%>%
  mutate(mean_europ = mean(numinf)) %>%
  mutate(GOOD_or_BAD = case_when( 
    mean_pais > mean_europ ~ "Bad",
    mean_pais < mean_europ ~ "Good"))

geometrias <- get_eurostat_geospatial(resolution = "20", nuts_level = "0")
mapdata <- full_join(df_1, geometrias, by = c("code_pais" = "id"))


mapdata_1a <- mapdata %>%  filter(anyo %in% c(1994:2007))

p <- ggplot(mapdata_1a) +
     geom_sf(aes(fill = GOOD_or_BAD, geometry = geometry), color = "black", size = .1) +
     labs(title = "Gráfico representando los países europeos que tienen en media 
     más o menos infracciones comparando a la media europea",
     subtitle = "(entre 1994 y 2007)",
     fill = "Países por encima o debajo?",
     caption = "Datos Eurostat") + 
     theme_linedraw() +
     coord_sf(xlim = c(-10,29), ylim = c(35,70))
p <- p + theme(plot.title = element_text(face = "bold"))+
         theme(axis.title.y = element_text(size=8))+
         theme(axis.title.x = element_text(size=8))
p

#2-Muestra la evolucion del numero de infracciones por infracciones para francia y Espagna.

# a) para Homicidios :
df_2a <- df %>% filter(pais %in% c("Spain","France") &  infraccion == "Homicidios")
p<-ggplot(df_2a, aes(anyo, numinf, group =pais, colour = pais)) +
   geom_line() +
   geom_point() +
   scale_colour_manual(values = c(France = "blue", Spain = "red")) +
   labs(title ="Evolución de los homicidios en Francia y España",
   subtitle = "(Desde 1993 hasta 2007)",
   caption = "Datos de Eurostat", 
   x = "Años",
   y = "Número de homicidios") +
   scale_x_continuous(breaks = seq(1993, 2007, 2), limits = c(1993, 2007)) +
   transition_reveal(anyo)+
   theme_linedraw()
p <-p + theme(plot.title = element_text(face = "bold")) +
        theme(axis.title.y = element_text(size=12)) +
        theme(axis.title.x = element_text(size=12))
p

p<-ggplot(df_2a, aes(anyo, numinf, group =pais, colour = pais)) +
   geom_line() +
   scale_colour_manual(values = c(France = "blue", Spain = "red")) +
   labs(title ="Evolución de los homicidios en Francia y España",
   subtitle = "(Desde 1993 hasta 2007)",
   caption = "Datos de Eurostat", 
   x = "Años",
   y = "Número de homicidios") +
   scale_x_continuous(breaks = seq(1993, 2007, 2), limits = c(1993, 2007)) +
   theme_linedraw()
p <-p + theme(plot.title = element_text(face = "bold")) +
        theme(axis.title.y = element_text(size=12)) +
        theme(axis.title.x = element_text(size=12))
p

# b) para drogas :
df_2b <- df %>% filter(pais %in% c("Spain","France") &  infraccion == "Actos ilicitos con drogas")
p<-ggplot(df_2b, aes(anyo, numinf, group =pais, colour = pais)) +
   geom_line() +
   geom_point() +
   scale_colour_manual(values = c(France = "blue", Spain = "red")) +
   labs(title ="Evolución de los actos ilícitos relacionados a drogas en Francia y España",
   subtitle = "(Desde 1993 hasta 2007)",
   caption = "Datos de Eurostat", 
   x = "Años",
   y = "Número de actos ilícitos con drogas") +
   scale_x_continuous(breaks = seq(1993, 2007, 2), limits = c(1993, 2007)) +
   transition_reveal(anyo)+
   theme_linedraw()
p <-p + theme(plot.title = element_text(face = "bold")) +
        theme(axis.title.y = element_text(size=12)) +
        theme(axis.title.x = element_text(size=12))
p

p<-ggplot(df_2b, aes(anyo, numinf, group =pais, colour = pais)) +
   geom_line() +
   scale_colour_manual(values = c(France = "blue", Spain = "red")) +
   labs(title ="Evolución de los actos ilícitos relacionados a drogas en Francia y España",
   subtitle = "(Desde 1993 hasta 2007)",
   caption = "Datos de Eurostat", 
   x = "Años",
   y = "Número de actos ilícitos con drogas") +
   scale_x_continuous(breaks = seq(1993, 2007, 2), limits = c(1993, 2007)) +
   theme_linedraw()
p <-p + theme(plot.title = element_text(face = "bold")) +
        theme(axis.title.y = element_text(size=12)) +
        theme(axis.title.x = element_text(size=12))
p

# c) para el resto de infracciones:
df_2c <- df %>% filter(pais %in% c("Spain","France") & infraccion != c("Homicidios") & infraccion != c("Actos ilicitos con drogas")) %>% group_by(infraccion)
p<-ggplot(df_2c, aes(anyo, numinf, group =pais, colour = pais)) +
   geom_line() +
   geom_point() +
   facet_wrap(~ infraccion, nrow = 2) +
   scale_colour_manual(values = c(France = "blue", Spain = "red")) +
   labs(title ="Evolución de las infracciones en Francia y España",
   subtitle = "(Desde 1993 hasta 2007)",
   caption = "Datos de Eurostat", 
   x = "Años",
   y = "Número de infracciones") +
   scale_x_continuous(breaks = seq(1993, 2007, 2), limits = c(1993, 2007)) +
   transition_reveal(anyo)+
   theme_linedraw()
p <-p + theme(plot.title = element_text(face = "bold")) +
        theme(axis.title.y = element_text(size=12)) +
        theme(axis.title.x = element_text(size=12))
p


p<-ggplot(df_2c, aes(anyo, numinf, group =pais, colour = pais)) +
   geom_line() +
   facet_wrap(~ infraccion, nrow = 2) +
   scale_colour_manual(values = c(France = "blue", Spain = "red")) +
   labs(title ="Evolución de las infracciones en Francia y España",
   subtitle = "(Desde 1993 hasta 2007)",
   caption = "Datos de Eurostat", 
   x = "Años",
   y = "Número de infracciones") +
   scale_x_continuous(breaks = seq(1993, 2007, 2), limits = c(1993, 2007)) +
   theme_linedraw()
p <-p + theme(plot.title = element_text(face = "bold")) +
        theme(axis.title.y = element_text(size=12)) +
        theme(axis.title.x = element_text(size=12))
p


#3- Tablas

df_3 <- df %>% select(2,4,6) %>%
        group_by(pais) %>%
        mutate(numinf_total = sum(NN = numinf)) %>% 
        group_by(pais,infraccion) %>%
        mutate(numinf_by_inf_and_country = sum(NN = numinf)) %>%
        mutate(percent = (numinf_by_inf_and_country/numinf_total)*100) %>%
        select(1,2,6) %>%
        slice(1:218) %>%
        mutate(row_num = 1:n()) %>%
        pivot_wider(names_from = infraccion, values_from = percent) %>%
        filter(row_num == "1") %>% select(-2) %>% mutate(Total = "100%")
     
library(kableExtra)

knitr::kable(df_3,format = "html") %>%
kableExtra::kable_styling(bootstrap_options = c("striped", "hover"),fixed_thead = list(enabled = T, background = "lightgrey"))

DT::datatable(df_3)

#Hacer el RmarKdown
