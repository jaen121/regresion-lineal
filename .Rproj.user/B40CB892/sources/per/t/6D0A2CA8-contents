

library(dplyr)
library(readxl)
library(data.table)
library(paqueteMET)
library(writexl)#libreria para guardar el archivo como excel.

data <- readRDS("Datos/vivienda4.RDS")

# Realice un análisis exploratorio de las variables precio de vivienda (millones de pesos COP)
# y área de la vivienda (metros cuadrados) - incluir gráficos e indicadores apropiados interpretados.

# Medidas de centralidad para el precio
promedio <- mean(data$preciom,na.rm = T)
mediana <- median(data$preciom,na.rm = T)
# moda (con datos agrupados en intervalos usando el caso de igual longitud de intervalos)

# Medidas de variabilidad para el precio
coef_apertura <- max(data$preciom, na.rm = T)/min(data$preciom,na.rm = T)
varianza <- var(data$preciom, na.rm = T)
desv_estandar <- sd(data$preciom, na.rm = T)
CV <- round(100*desv_estandar/promedio,2)

# Coeficiente de asimertria y curtosis para el precio
CA <- mean((data$preciom - promedio)^3)/(mean((data$preciom - promedio)^2))^(3/2)
CC <- mean((data$preciom - promedio)^4)/(mean((data$preciom - promedio)^2))^(2) - 3



# Medidas de posicion para el precio
Q1 <- quantile(data$preciom, probs = 0.25, type = 6)
Q2 <- quantile(data$preciom, probs = 0.50, type = 6)
Q3 <- quantile(data$preciom, probs = 0.75, type = 6)


# Tabla resumen para el precio
tb_resumen <- data.frame(
  Minimo = min(data$preciom, na.rm = T),
  Q1 = Q1,
  Q2 = Q2,
  Promedio = promedio,
  Mediana = mediana,
  Q3 = Q3,
  Maximo = max(data$preciom, na.rm = T),
  Coef_apertura = coef_apertura,
  varianza = varianza,
  Desv.Estandar = desv_estandar,
  Coef.Variacion = CV,
  Coef.Asimetria = CA,
  Coef.Curtosis = CC
)

tb_resumen
saveRDS(tb_resumen, "Resultados/tb_resumen.rds")
write_xlsx(tb_resumen,"Resultados/AED_precio.xlsx")

library(ggplot2)

# Histograma para el precio
hist(data$preciom, freq = FALSE,main = "Distribución del Precio de las Viviendas",
     xlab = "Precio dela vivienda", ylab = "Conteo", col = "green")


lines(density(data$preciom),col="red",lwd=2)

curve(dnorm(x,mean=mean(data$preciom),sd=sd(data$preciom)), from=0,to=1000, 
      add=TRUE, col="blue", lwd=2)

legend("topleft",col=c("blue","red"),legend =c("Densidad normal estimada","Estimador de núcleo de la densidad"),lwd=2, bty = "n")


# Diagrama de caja para el precio
boxplot(data$preciom, main = "Distribución del Precio de las Viviendas",
        xlab = "Precio dela vivienda", ylab = "Conteo", col = "green")

# Datos atípicos para el precio

bi <- Q1 - 1.5*(Q3 - Q1)

bi1 <- max(bi,78)
bi1
# bigote inferior es 78


bs <- Q3 + 1.5*(Q3 - Q1)
bs2<- min(bs,760)
bs2
#bigote superior  es 422.5


# Análisis descriptivo para el área construida


# Medidas de centralidad para el precio
promedioa <- mean(data$areaconst,na.rm = T)
medianaa <- median(data$areaconst,na.rm = T)
# moda (con datos agrupados en intervalos usando el caso de igual longitud de intervalos)

# Medidas de variabilidad para el precio
coef_aperturaa <- max(data$areaconst, na.rm = T)/min(data$areaconst,na.rm = T)
varianzaa <- var(data$areaconst, na.rm = T)
desv_estandara <- sd(data$areaconst, na.rm = T)
CVa <- round(100*desv_estandara/promedioa,2)

# Coeficiente de asimertria y curtosis para el precio
CAa <- mean((data$areaconst - promedioa)^3)/(mean((data$areaconst - promedioa)^2))^(3/2)
CCa <- mean((data$areaconst - promedioa)^4)/(mean((data$areaconst - promedioa)^2))^(2) - 3



# Medidas de posicion para el precio
Q1a <- quantile(data$areaconst, probs = 0.25, type = 6)
Q2a <- quantile(data$areaconst, probs = 0.50, type = 6)
Q3a <- quantile(data$areaconst, probs = 0.75, type = 6)


# Tabla resumen para el precio
tb_resumen <- data.frame(
  Minimoa = min(data$areaconst, na.rm = T),
  Q1a = Q1a,
  Q2a = Q2a,
  Promedioa = promedioa,
  Medianaa = medianaa,
  Q3a = Q3a,
  Maximoa = max(data$areaconst, na.rm = T),
  Coef_aperturaa = coef_aperturaa,
  varianzaa = varianzaa,
  Desv.Estandara = desv_estandara,
  Coef.Variaciona = CVa,
  Coef.Asimetriaa = CAa,
  Coef.Curtosisa = CCa
)

tb_resumen
saveRDS(tb_resumen, "Resultados/tb_resumenareaconst.rds")
write_xlsx(tb_resumen,"Resultados/AED_areaconst.xlsx")

library(ggplot2)

# Histograma para el precio
hist(data$areaconst, freq = FALSE,main = "Distribución del área construida",
     xlab = "Área construida", ylab = "Conteo", col = "green")


lines(density(data$areaconst),col="red",lwd=2)

curve(dnorm(x,mean=mean(data$areaconst),sd=sd(data$areaconst)), from=0,to=1000, 
      add=TRUE, col="blue", lwd=2)

legend("topleft",col=c("blue","red"),legend =c("Densidad normal estimada","Estimador de núcleo de la densidad"),lwd=2, bty = "n")


# Diagrama de caja para el precio
boxplot(data$areaconst, main = "Distribución del Precio de las Viviendas",
        xlab = "Precio dela vivienda", ylab = "Conteo", col = "green")

# Datos atípicos para el precio

bia <- Q1a - 1.5*(Q3a - Q1a)

bi1a <- max(bia,40)
bi1a
# bigote inferior es 40

bsa <- Q3a + 1.5*(Q3a - Q1a)
bs1a <- min(bsa,200)
bs1a
# bigote superior es 200


# Datos sin datos atípicos

library(dplyr)
horas_agr1 <- data %>% 
  select(preciom, zona) #seleccionar solo precio y zona
  
str(horas_agr1)
#=========== para hacer el conteo por zonas
conteo_h <- horas_agr1 %>% 
  group_by(zona) %>% #agrupar por zonas
  summarise(n_i = n()) %>%  #sumarizar valores a una sola fila por grupo
  mutate(N_i = cumsum(n_i), f_i = n_i /sum(n_i), F_i = cumsum(f_i))#crear encabezado de la tabla
str(conteo_h)#imprimir tabla

saveRDS(conteo_h, "Resultados/conteo_h.rds")#guardar archivo en formato .RDS más facile de leer.

conteo_h5 <- conteo_h %>% # realizar el conteo del top de los 5 mejores.
  arrange(desc(n_i)) %>% 
  slice(1:5)

library(esquisse)#libreria para crear gráficos
esquisser()# crear el gráfico, importando datos.

library(ggplot2)

ggplot(horas_agr1) +
 aes(x = zona, y = preciom, fill = preciom, colour = zona) +
 geom_col() +
 scale_fill_gradient() +
 scale_color_hue(direction = 1) +
 labs(x = "Zonas") +
 theme_minimal()

esquisser()

conteo_h2 <- data %>% 
  group_by(tipo) %>% #agrupar por zonas
  summarise(n_i = n()) %>%  #sumarizar valores a una sola fila por grupo
  mutate(N_i = cumsum(n_i), f_i = n_i /sum(n_i), F_i = cumsum(f_i))#crear encabezado de la tabla
str(conteo_h2)#imprimir tabla

saveRDS(conteo_h2, "Resultados/conteo_h.rds")#guardar archivo en formato .RDS más facile de leer.

conteo_h2 <- conteo_h2 %>% # realizar el conteo del top de los 5 mejores.
  arrange(desc(n_i)) %>% 
  slice(1:5)

library(ggplot2)

ggplot(data) +
 aes(x = tipo, y = preciom, colour = tipo) +
 geom_col(fill = "#112446") +
 scale_color_hue(direction = 1) +
 theme_minimal()
