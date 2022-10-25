# Librerias
library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)
library(xlsx)

# Leer datos
BASE <- read_excel("Bases de datos/BASE.xlsx")

# Agrupar solo 2019
base2019 = subset(BASE, subset = ano == 2019)
precio2019i = base2019[1:27]

# Agrupar solo 2020
base2020 = subset(BASE, subset = ano == 2020)
precio2020i = base2020[1:27]

# Quitar variables que interfieren
precio2019 = select(precio2019i, -c('dia', 'ano', "mes"))
precio2020 = select(precio2020i, -c('dia', 'ano', "mes"))

#Unir data
hora = seq(0, 23, by=1)
ano = rep(2019,24)
PromedioH = apply(precio2019, 2, mean)
BasexHa1 = cbind(hora,ano,PromedioH)
ano = rep(2020,24)
PromedioH = apply(precio2020, 2, mean)
BasexHa2 = cbind(hora,ano,PromedioH)
BasexHa = rbind(BasexHa1, BasexHa2)

# Ordenar
Base = Base[order(Base$mes_l),]
Base = Base[order(Base$ano),]
Base = select(Base,-"mes_l")

# Concatenar
install.packages ("tidyr")
Base <- unite(Base, Periodo ,c(1:2),  sep = " ", remove = TRUE)

# Guardar datas
write.table(BasexHa, "BasexHaPrecio.txt", sep = "\t", quote = F, row.names = F)

#######################################################

# Agrupar solo 2019
base2019 = subset(BASE, subset = ano == 2019)
Generacion2019i = cbind(base2019[1:3],base2019[28:51])

# Agrupar solo 2020
base2020 = subset(BASE, subset = ano == 2020)
Generacion2020i = cbind(base2020[1:3],base2020[28:51])

# Quitar variables que interfieren
Generacion2019 = select(Generacion2019i, -c('dia', 'ano', "mes", "mes_l"))
Generacion2020 = select(Generacion2020i, -c('dia', 'ano', "mes", "mes_l"))

#Unir data
hora = seq(0, 23, by=1)
ano = rep(2019,24)
PromedioH = apply(Generacion2019, 2, mean)
BasexHa1 = cbind(hora,ano,PromedioH)
ano = rep(2020,24)
PromedioH = apply(Generacion2020, 2, mean)
BasexHa2 = cbind(hora,ano,PromedioH)
BasexHa = rbind(BasexHa1, BasexHa2)

# Guardar datas
write.table(BasexHa, "BasexHaGeneracion.txt", sep = "\t", quote = F, row.names = F)

#######################################################
# Valor faltante
Demanda2019[is.na(Demanda2019$Demanda_X22),]

# Agrupar solo 2019
base2019 = subset(BASE, subset = ano == 2019)
Demanda2019i = cbind(base2019[1:3],base2019[52:75])

# Agrupar solo 2020
base2020 = subset(BASE, subset = ano == 2020)
Demanda2020i = cbind(base2020[1:3],base2020[52:75])

# Quitar variables que interfieren
Demanda2019 = select(Demanda2019i, -c('dia', 'ano', "mes"))
Demanda2020 = select(Demanda2020i, -c('dia', 'ano', "mes"))

#Unir data
hora = seq(0, 23, by=1)
ano = rep(2019,24)
PromedioH = apply(Demanda2019, 2, mean)
BasexHa1 = cbind(hora,ano,PromedioH)
ano = rep(2020,24)
PromedioH = apply(Demanda2020, 2, mean)
BasexHa2 = cbind(hora,ano,PromedioH)
BasexHa = rbind(BasexHa1, BasexHa2)

# Guardar datas
write.table(BasexHa, "BasexHaDemanda.txt", sep = "\t", quote = F, row.names = F)

##########################################################

# Graficos

# Cargar por precio
BasexHaPrecio <- read_delim("Desktop/Programacion para el analisis de datos/BasexHaPrecio.txt","\t", escape_double = FALSE, trim_ws = TRUE)
BasexHaPrecio = as.data.frame(BasexHaPrecio)
BasexHaPrecio$ano = as.factor(BasexHaPrecio$ano)
# Grafico precio x hora anual
precioxhoraanual <- ggplot(BasexHaPrecio, aes(x=hora, y=PromedioH)) +
  geom_line(aes(color = ano))
precioxhoraanual

# Cargar por generacion
BasexHaGeneracion <- read_delim("Desktop/Programacion para el analisis de datos/BasexHaGeneracion.txt","\t", escape_double = FALSE, trim_ws = TRUE)
BasexHaGeneracion = as.data.frame(BasexHaGeneracion)
BasexHaGeneracion$ano = as.factor(BasexHaGeneracion$ano)
# Grafico generacion x hora anual
generacionxhoraanual <- ggplot(BasexHaGeneracion, aes(x=hora, y=PromedioH)) +
  geom_line(aes(color = ano))
generacionxhoraanual

# Cargar por demanda
BasexHaDemanda <- read_delim("
                             BasexHaGeneracion.txt","\t", escape_double = FALSE, trim_ws = TRUE)
BasexHaDemanda = as.data.frame(BasexHaDemanda)
BasexHaDemanda$ano = as.factor(BasexHaDemanda$ano)
# Grafico demanda x hora anual
demandaxhoraanual <- ggplot(BasexHaDemanda, aes(x=hora, y=PromedioH)) +
  geom_line(aes(color = ano))
demandaxhoraanual

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Librerias
library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)
library(xlsx)

# Leer datos
BASE <- read_excel("BASE-MIA.xlsx")
# Agrupar por ano
base2019 = subset(BASE, subset = ano == 2019)
base2020 = subset(BASE, subset = ano == 2020)
# Promedio por dia de la semnana
Precioxsemana2019 = base2019  %>% group_by(semana) %>% summarise_each(funs(mean)) 
Precioxsemana2019 = select(Precioxsemana2019, c('ano','semana','Precio_tot','Generacion_tot','Demanda_tot'))
Precioxsemana2020 = base2020  %>% group_by(semana) %>% summarise_each(funs(mean)) 
Precioxsemana2020 = select(Precioxsemana2020, c('ano','semana','Precio_tot','Generacion_tot','Demanda_tot'))
Total = rbind(Precioxsemana2019,Precioxsemana2020)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
library(dtw)
# Cluster metodo dtw ward x columnas
# Generacion
dtwgene = hclust(dist(t(generacion), method = "dtw"), method = "ward.D2") 
plot(dtwgene)
rect.hclust(dtwgene, k = 4, border = "red")
# Precio
dtwprecio = hclust(dist(t(precio), method = "dtw"), method = "ward.D2") 
plot(dtwprecio)
rect.hclust(dtwprecio, k = 4, border = "red")
# Demanda
dtwdemanda = hclust(dist(t(demanda), method = "dtw"), method = "ward.D2") 
plot(dtwdemanda)
rect.hclust(dtwdemanda, k = 4, border = "red")

# Cluster metodo dtw completo x filas
# Generacion
dtwgene = hclust(dist(generacion, method = "dtw"), method = "ward.D2") 
plot(dtwgene)
rect.hclust(dtwgene, k = 4, border = "red")
# Precio
dtwprecio = hclust(dist(precio, method = "dtw"), method = "ward.D2") 
plot(dtwprecio)
rect.hclust(dtwprecio, k = 4, border = "red")
# Demanda
dtwdemanda = hclust(dist(demanda, method = "dtw"), method = "ward.D2") 
plot(dtwdemanda)
rect.hclust(dtwdemanda, k = 4, border = "red")
