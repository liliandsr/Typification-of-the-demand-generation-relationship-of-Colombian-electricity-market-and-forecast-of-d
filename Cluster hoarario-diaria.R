# Instalar paquetes
install.packages('dplyr')
install.packages('xlsx')
install.packages('readxl')
install.packages('ggplot2')

# Abrir librerias
library(dplyr)
library(readxl)
library(openxlsx)
library(ggplot2)
library(lattice)
library(stats)
library(cluster)
library(dtw)
#library(lsa)
library(proxy)
# Leer base
base <- read_excel("Bases de datos/BASE.xlsx")
View(base)
summary(base)
View(base)

# Convertir a factor
base$mes<-as.factor(base$mes)
base$dia<-as.factor(base$dia)
base$ano<-as.factor(base$ano)

# Matriz de correlaciones
correlacionPrecioGeneracion_2019<-cor(base[base$ano==2019,4:51])
View(correlacionPrecioGeneracion_2019[1:24,25:48])
correlacionPrecioGeneracion_2020<-cor(base[base$ano==2020,4:51])
View(correlacionPrecioGeneracion_2020[1:24,25:48])
correlacionPrecioDemanda_2020<-cor(select(base[base$ano==2020,],-(dia:ano),-(Generacion_X0:Generacion_X23)))
View(correlacionPrecioDemanda_2020[1:24,25:48])
correlacionPrecioDemanda_2019<-cor(select(base[base$ano==2019,],-(dia:ano),-(Generacion_X0:Generacion_X23)))
View(correlacionPrecioDemanda_2019[1:24,25:48])

# Graficos
# Boxplot mes vs Precio - por mes y ano
bp <- ggplot(base, aes(x=mes, y=Precio_tot, group=mes)) + 
  geom_boxplot(aes(fill=mes))
bp + facet_grid(ano ~ .)
bp
# Lineal mes vs Precio - por dia, mes y ano
bp2 <- ggplot(base, aes(x=dia, y=Precio_tot, group=mes, color=mes)) + 
  geom_line(aes(linetype=mes)) + geom_point()
bp2 + facet_grid(ano ~ .)
bp2
# Boxplot Demanda - por mes y año
bp3 <- ggplot(base, aes(x=mes, y=Demanda_tot, group=mes)) + 
  geom_boxplot(aes(fill=mes))
bp3 + facet_grid(ano ~ .)
bp3
# Lineal Demanda - por dia, mes y año
bp4 <- ggplot(base, aes(x=dia, y=Demanda_tot, group=mes, color=mes)) + 
  geom_line(aes(linetype=mes)) + geom_point()
bp4 + facet_grid(ano ~ .)
bp4

# Boxplot Demanda - por mes y año
bp5 <- ggplot(base, aes(x=mes, y=Generacion_tot, group=mes)) + 
  geom_boxplot(aes(fill=mes))
bp5 + facet_grid(ano ~ .)
bp5
# Lineal Demanda - por dia, mes y ano
bp6 <- ggplot(base, aes(x=dia, y=Generacion_tot, group=mes, color=mes)) + 
  geom_line(aes(linetype=mes)) + geom_point()
bp6 + facet_grid(ano ~ .)
bp6

# Cargar por precio
BasexHaPrecio <- read_delim("BasexHaPrecio.txt","\t", escape_double = FALSE, trim_ws = TRUE)
BasexHaPrecio = as.data.frame(BasexHaPrecio)
BasexHaPrecio$ano = as.factor(BasexHaPrecio$ano)
# Grafico precio x hora anual
precioxhoraanual <- ggplot(BasexHaPrecio, aes(x=hora, y=PromedioH)) +
  geom_line(aes(color = ano))
precioxhoraanual

# Cargar por generacion
BasexHaGeneracion <- read_delim("BasexHaGeneracion.txt","\t", escape_double = FALSE, trim_ws = TRUE)
BasexHaGeneracion = as.data.frame(BasexHaGeneracion)
BasexHaGeneracion$ano = as.factor(BasexHaGeneracion$ano)
# Grafico generacion x hora anual
generacionxhoraanual <- ggplot(BasexHaGeneracion, aes(x=hora, y=PromedioH)) +
  geom_line(aes(color = ano))
generacionxhoraanual

# Cargar por demanda
BasexHaDemanda <- read_delim("BasexHaGeneracion.txt","\t", escape_double = FALSE, trim_ws = TRUE)
BasexHaDemanda = as.data.frame(BasexHaDemanda)
BasexHaDemanda$ano = as.factor(BasexHaDemanda$ano)
# Grafico demanda x hora anual
demandaxhoraanual <- ggplot(BasexHaDemanda, aes(x=hora, y=PromedioH)) +
  geom_line(aes(color = ano))
demandaxhoraanual

base2019 = subset(base, subset = ano == 2019)
base2020 = subset(base, subset = ano == 2020)
#Grafico promedio diario Precio - Generacion
plot(base2019$Precio_tot[1:20],base$Generacion_tot[1:20], type="l", col="green")
#Grafico promedio diario Demanda - Generacion
plot(base2019$Demanda_tot[1:20],base$Generacion_tot[1:20], type="l", col="green")
#Grafico promedio diario Demanda - Precio
plot(base2019$Demanda_tot[1:20],base$Precio_tot[1:20], type="l", col="green")

# Cargar por semana
Valoresxsemana <- read_delim("Valoresxsemana.txt","\t", escape_double = FALSE, trim_ws = TRUE)
Valoresxsemana = as.data.frame(Valoresxsemana)
a = c(7,4,1,2,3,6,5)
Valoresxsemana = cbind(Valoresxsemana,a)
Valoresxsemana$ano = as.factor(Valoresxsemana$ano)
# Grafico por semana
Precio <- ggplot(Valoresxsemana, aes(x=a, y=Precio_tot)) +
  geom_line(aes(color = ano))
Precio
Generacion <- ggplot(Valoresxsemana, aes(x=a, y=Generacion_tot)) +
  geom_line(aes(color = ano))
Generacion
Demanda <- ggplot(Valoresxsemana, aes(x=a, y=Demanda_tot)) +
  geom_line(aes(color = ano))
Demanda

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Construir conjunto de datos.
precio = select(base,-(Demanda_X0:Demanda_X23),-(Generacion_X0:Generacion_X23), -(dia:ano), -(Precio_tot:Demanda_tot))
demanda = select(base,-(Precio_X0:Precio_X23),-(Generacion_X0:Generacion_X23), -(dia:ano), -(Precio_tot:Demanda_tot))
generacion = select(base,-(Precio_X0:Precio_X23),-(Demanda_X0:Demanda_X23), -(dia:ano), -(Precio_tot:Demanda_tot))
# Subset de 2019
base2019 = subset(base, subset = ano == 2019)
# Generacion y demanda por 2019
generacion9 = select(base2019,-(Precio_X0:Precio_X23),-(Demanda_X0:Demanda_X23), -(dia:ano), -(Precio_tot:Demanda_tot))
demanda9 =  select(base2019,-(Precio_X0:Precio_X23),-(Generacion_X0:Generacion_X23), -(dia:ano), -(Precio_tot:Demanda_tot))
precio9 = select(base2019,-(Demanda_X0:Demanda_X23),-(Generacion_X0:Generacion_X23), -(dia:ano), -(Precio_tot:Demanda_tot))
# Subset de 2020
base2020 = subset(base, subset = ano == 2020)
# Generacion y demanda por 2019
generacion0 = select(base2020,-(Precio_X0:Precio_X23),-(Demanda_X0:Demanda_X23), -(dia:ano), -(Precio_tot:Demanda_tot))
demanda0 =  select(base2020,-(Precio_X0:Precio_X23),-(Generacion_X0:Generacion_X23), -(dia:ano), -(Precio_tot:Demanda_tot))
precio0 = select(base2020,-(Demanda_X0:Demanda_X23),-(Generacion_X0:Generacion_X23), -(dia:ano), -(Precio_tot:Demanda_tot))
# Mirar si hay valores faltantes
sum(is.null(demanda))
sum(is.null(generacion))
sum(is.null(precio))
# Cambiar a matriz
precio = as.matrix(precio)
demanda = as.matrix(demanda)
generacion = as.matrix(generacion)
generacion9 = as.data.frame(generacion9)
demanda9 = as.matrix(demanda9)

# Cluster metodo coseno ward x columnas
# Generacion
cluster1wc= hclust(dist(t(scale(generacion)), method = "cosine"), method = "ward.D2")
plot(cluster1wc, main = 'Cluster de generacion horaria', ylab = 'Altura')
rect.hclust(cluster1wc, k = 7, border = "red")
result1wc = cbind(cluster1wc$labels,as.matrix(cutree(cluster1wc, k = 7)))
# Precio
cluster2wc = hclust(dist(t(scale(precio)), method = "cosine"), method = "ward.D2")
plot(cluster2wc, main = 'Cluster de precio horario', ylab = 'Altura')
rect.hclust(cluster2wc, k = 7, border = "red")
result2wc = cbind(cluster2wc$labels,as.matrix(cutree(cluster2wc, k = 7)))
# Demanda
cluster3wc = hclust(dist(t(scale(demanda)), method = "cosine"), method = "ward.D2")
plot(cluster3wc, main = 'Cluster de demanda horaria', ylab = 'Altura')
rect.hclust(cluster3wc, k = 7, border = "red")
result3wc = cbind(cluster3wc$labels,as.matrix(cutree(cluster3wc, k = 7)))
# Resultados generales
resultadoswc = cbind(result1wc, result2wc, result3wc)
write.xlsx(resultadoswc, "ResultadosVariables.xlsx")

## Cluster metodo coseno ward x filas 2019
# Generacion
s = simil(scale(generacion9), method = "cosine")
d = 1-s
cluster1wf= hclust(d, method = "ward.D2")
plot(cluster1wf,main = 'Cluster de generación diaria', ylab = 'Altura')
rect.hclust(cluster1wf, k = 3, border = "red")
g = cutree(cluster1wf, k = 3)
table(g)
result1wf = cbind(select(base2019,(dia:ano)),  g, generacion9)
write.xlsx(result1wf, "ResultadosOGeneracion.xlsx")
# Precio
s = simil(scale(precio9), method = "cosine")
d = 1-s
cluster2wf= hclust(d, method = "ward.D2")
plot(cluster2wf, main = 'Cluster de precio diario', ylab = 'Altura')
rect.hclust(cluster2wf, k = 3, border = "red")
p = cutree(cluster2wf, k = 3)
table(p)
result2wf = cbind(select(base2019,(dia:ano)), p, precio9)
write.xlsx(result2wf, "ResultadosOPrecio.xlsx")
# Demanda
s = simil(scale(demanda9), method = "cosine")
d = 1-s
cluster3wf= hclust(d, method = "ward.D2")
plot(cluster3wf, main = 'Cluster de demanda diaria', ylab = 'Altura')
rect.hclust(cluster3wf, k = 3, border = "red")
de = cutree(cluster3wf, k = 3)
table(de)
result3wf = cbind(select(base2019,(dia:ano)), de, demanda9)
write.xlsx(result3wf, "ResultadosODemanda.xlsx")

## Cluster metodo coseno ward x filas 2020
# Generacion
s = simil(scale(generacion0), method = "cosine")
d = 1-s
cluster1wf= hclust(d, method = "ward.D2")
plot(cluster1wf,main = 'Cluster de generación diaria', ylab = 'Altura')
rect.hclust(cluster1wf, k = 3, border = "red")
g = cutree(cluster1wf, k = 3)
table(g)
result1wf = cbind(select(base2020,(dia:ano)),  g, generacion0)
write.xlsx(result1wf, "ResultadosOGeneracion.xlsx")
# Precio
s = simil(scale(precio0), method = "cosine")
d = 1-s
cluster2wf= hclust(d, method = "ward.D2")
plot(cluster2wf, main = 'Cluster de precio diario', ylab = 'Altura')
rect.hclust(cluster2wf, k = 3, border = "red")
p = cutree(cluster2wf, k = 3)
table(p)
result2wf = cbind(select(base2020,(dia:ano)), p, precio0)
write.xlsx(result2wf, "ResultadosOPrecio.xlsx")
# Demanda
s = simil(scale(demanda0), method = "cosine")
d = 1-s
cluster3wf= hclust(d, method = "ward.D2")
plot(cluster3wf, main = 'Cluster de demanda diaria', ylab = 'Altura')
rect.hclust(cluster3wf, k = 3, border = "red")
de = cutree(cluster3wf, k = 3)
table(de)
result3wf = cbind(select(base2020,(dia:ano)), de, demanda0)
write.xlsx(result3wf, "ResultadosODemanda.xlsx")

