install.packages("vegan")
library(iNEXT)

##cargar cada uno de los sitios en un objeto
####modificando algunos aspectos de los objetos para poder hacer los pasos posteriores sin problemas
datos_<-read_csv("data/Sitio_1.csv")
datos_1 <- as.data.frame(datos_1)
rownames(datos_1) <- datos_1[,1]
datos_1 <- datos_1[,-1]
datos_1 <- datos_1[,-13]

datos_2<-read_csv("data/Sitio_2.csv")
datos_2 <- as.data.frame(datos_2)
rownames(datos_2) <- datos_2[,1]
datos_2 <- datos_2[,-1]
datos_2 <- datos_2[,-13]

datos_3<-read_csv("data/Sitio_3.csv")
datos_3 <- as.data.frame(datos_3)
rownames(datos_3) <- datos_3[,1]
datos_3 <- datos_3[,-1]
datos_3 <- datos_3[,-13]

datos_4<-read_csv("data/Sitio_4.csv")
datos_4 <- as.data.frame(datos_4)
rownames(datos_4) <- datos_4[,1]
datos_4 <- datos_4[,-1]
datos_4 <- datos_4[,-13]

datos_5<-read_csv("data/Sitio_5.csv")
datos_5 <- as.data.frame(datos_5)
rownames(datos_5) <- datos_5[,1]
datos_5 <- datos_5[,-1]
datos_5 <- datos_5[,-13]

datos_6<-read_csv("data/Sitio_6.csv")
datos_6 <- as.data.frame(datos_6)
rownames(datos_6) <- datos_6[,1]
datos_6 <- datos_6[,-1]
datos_6 <- datos_6[,-13]

datos_7<-read_csv("data/Sitio_7.csv")
datos_7 <- as.data.frame(datos_7)
rownames(datos_7) <- datos_7[,1]
datos_7 <- datos_7[,-1]
datos_7 <- datos_7[,-13]

#observación de nuestras tablas en el objeto
glimpse(datos_1)
glimpse(datos_2)
glimpse(datos_3)
glimpse(datos_4)
glimpse(datos_5)
glimpse(datos_6)
glimpse(datos_7)

#verificación que no haya NA's
stopifnot(!any(is.na(datos_1)))
stopifnot(!any(is.na(datos_2)))
stopifnot(!any(is.na(datos_3)))
stopifnot(!any(is.na(datos_4)))
stopifnot(!any(is.na(datos_5)))
stopifnot(!any(is.na(datos_6)))
stopifnot(!any(is.na(datos_7)))


#generamos una sola matriz a partir de la suma de las abundancias de todas las especies en un sitio, se tuvo que hacer esta corrección posterior a hacer las tablas, ya que no habiamos entendido como se debian de acomodar los datos, ya que al realizar la práctica nos dimos cuenta que solo se necesita una para el analisis
sp_matrix<-sp_matrix <- rbind(
  sitio1 = colSums(datos_1),
  sitio2 = colSums(datos_2),
  sitio3 = colSums(datos_3),
  sitio4 = colSums(datos_4),
  sitio5 = colSums(datos_5),
  sitio6 = colSums(datos_6),
  sitio7 = colSums(datos_7)
)

sp_matrix

#calcular diversidad alfa
alfa <- data.frame(
  sitio = rownames(sp_matrix),
  riqueza = specnumber(sp_matrix),
  shannon = diversity(sp_matrix, index = "shannon"),
  simpson = diversity(sp_matrix, index = "simpson"),
  inv_simp = diversity(sp_matrix, index = "invsimpson"),
  pielou = diversity(sp_matrix, "shannon") / log(specnumber(sp_matrix))
)

alfa
write.csv(alfa, "output/tabla_diversidad_alfa.csv") #exportamos la tabla en la carpeta correspondiente

#Sacar Valor de Chao
#Chao1 requiere conteos crudos (no proporciones)
alfa$chao1 <- estimateR(sp_matrix)["S.chao1",]
alfa


#VAMOS A HACER LA CURVA DE RAREFACCIÓN USANDO EL CÓDIGO DE REFERENCIA

##Usa el paquete iNEXT para generar curvas de interpolación y extrapolación (Hill numbers, q = 0):

lista_sitios <- split(t(sp_matrix), rownames(t(sp_matrix)))
out <- iNEXT(lista_sitios, q = 0, datatype = "abundance")
ggiNEXT(out, type = 1) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Curvas de rarefacción e interpolación",
       x = "Número de individuos muestreados",
       y = "Riqueza de especies (q = 0)") +
  theme_classic(base_size = 12)

#la imagen la guardamos en la carpeta correspondiente