## -------------------------------------------------- ##
## ANÁLISIS ESTADÍSTICOS CON EL LENGUAJE R EN RSTUDIO ##
## -------------------------------------------------- ##

# Configurando mi ambiente y cargando librerías: -----------------------------------------------

set.seed(123999)
options(scipen = 10000, digits = 4)
library(tidyverse, warn.conflicts = F)
library(car)    	## pruebas estadísticas
library(nortest)	## pruebas estadísticas
library(FSA)		## Dunn test package
library(WRS2)
load(file = 'statistical_analyses.RData')
base::save.image(file = 'statistical_analyses.RData')

# setwd("/Users/mariohg/Library/CloudStorage/Dropbox/posdoc/Cursos a impartir_posdoc/CURSO-UJAT-2023")

# 1. Cargando nuestros datos ----------------------------------------------

	data <- read.csv('data/fisicoquimicos_mimosa.csv') %>%
	dplyr::mutate(experimento = rep("Mimona", times = 54))
	
	head(data)
	dim(data)
	str(data)


# 2. Evaluando y visualizando la distribución de los datos ----------------
## graficamos whisker-plot para observar la distribución de Nitrógeno por la variable "tratamiento" (n = 3)

	data %>% 
		ggplot2::ggplot(aes(x = Tratamiento, y = Nitrogen, colour = Tratamiento)) +
		geom_boxplot(size = 1.5) +
		geom_point(size = 3, color = "black") +
		xlab ("Tratamiento") +
		scale_y_continuous(name = "Contenido de Nitrógeno (%)", 
				   breaks = seq(from = 0.14, to = 0.22, by = 0.02),
				   limits=c(0.14,0.22)) +
		theme_light() + 
		theme(legend.position = "none",
		      panel.grid = element_blank(),
		      text = element_text(size = 16, colour = "black"))

	summary(data)
### El histograma y el gráfico de densidad son herramientas muy útiles porque sirven para mostrar la distribución,
### la simetría, el sesgo, variabilidad, moda, mediana y observaciones atípicas de un conjunto de datos. 
### Para explorar la normalidad de un conjunto de datos lo que se busca es que el histograma o gráfico de 
### densidad presenten un patrón más o menos simétrico.


# 3. Graficando la distribución de los datos por tratamiento --------------

	## Densidad
	data %>% 
		ggplot2::ggplot(aes(x = Nitrogen, color = Tratamiento)) +
		geom_density(aes(fill = Tratamiento), alpha = 0.5) +
		scale_colour_brewer(palette = "Set1", type = "div") +
		labs(colour = "Tratamiento") +
		scale_x_continuous(name="Nitrogeno (%)", expand = c(0, 0)) +
		scale_y_continuous(name = "Función de densidad", expand=c(0,0),limits=c(0,NA)) +
		theme_bw() +
		theme (legend.position = c(0.2,0.8),
		       text = element_text(size = 16, colour = "black"))
	
	## Histograma
	hist(data[,5])

# 4. Calculamos los estadísticos ---------------------------------------
	## Determinamos el promedio y SD de la variable de respuesta Nitrógeno, por Tratamiento

	base::tapply(X = data$Nitrogen, INDEX = data$Tratamiento, FUN = summary)
	
	# co <- subset(data, Tratamiento=="C0")


##### REALIZANDO GRAFICO QQPLOT
#### El gráfico cuantil-cuantil muestra puntos alienados cuando los datos siguen una distribución normal.
#### El gráfico cuantil-cuantil (qq-plot) es una herramienta gráfica para explorar si un conjunto de 
#### datos o muestra proviene de una población con cierta distribución.


	## qq-plot por Tratamiento (n = 3): C0, C1, y C2

	## subset Nitrogen values for "C0" treatment only: and then its draw qqplot
	head(data)
	# grDevices::svg(filename = 'Nitrogen-qqplot-Treatment.svg', width = 10, height = 5)
	data %>% 
		dplyr::select(Tratamiento, Nitrogen) %>% 
		# dplyr::filter(Tratamiento == "C0") %>%
		ggplot(aes(sample = Nitrogen)) +
		theme_bw(base_size = 16) + 
		labs (x = "Normal theoretical quantiles", y = "Sample quantiles") +
		stat_qq(size = 3) + 
		stat_qq_line() + facet_grid(cols = vars(Tratamiento)); 
	# dev.off()
		
	## Another option to plot with 
	graphics::par(mfcol = c(1,3))
	car::qqPlot(data$Nitrogen[data$Tratamiento=="C0"])
	car::qqPlot(data$Nitrogen[data$Tratamiento=="C1"])
	car::qqPlot(data$Nitrogen[data$Tratamiento=="C2"]); graphics::par(mfrow = c(1,1))


### Si se tuviese una muestra distribuída perfectamente normal, se esperaría que los puntos estuviesen 
### perfectamente alineados con la línea de referencia, sin embargo, las muestran con las que 
### se trabajan en la práctica casi nunca presentan este comportamiento aún si fueron obtenidas de una población normal.



# 5. Prueba de normalidad: Shapiro-Wilk test ---------------------------------

## Una vez procesados y examinados los gráficos, empleamos el test de Shapiro-Wilk para contrastar 
## la hipótesis nula de normalidad: Se aplicamos a cada nivel de TRATAMIENTO.

## Nivel de Significancia: alpha = 0.05
## Criterio de Decisión:  
				# Si P < alpha Se rechaza Ho
				# Si P >= alpha No se rechaza Ho

	# Entonces, los datos no siguen una distribución normal si P es ¿">", "<" or "=" a alpha?
	
	## Aplicando la prueba sobre Nitrogeno por tratamiento
	help(shapiro.test)
	tapply(X = data$Nitrogen, INDEX = data$Tratamiento, FUN = shapiro.test)
	
	## Aplicando sobre todos los datos (Columna en comun para los 3 niveles de Tratamiento)
	tapply(X = data$Nitrogen, INDEX = data$experiment, FUN = shapiro.test) # ¿ Tiene distribución Normal?


# 6. Prueba de Normalidad: Anderson-Darling -------------------------------
	
	help(ad.test, package = "nortest") ## ayuda de la función y prueba.
	
	## Prueba de Normalidad de los datos de Nitrógeno separada por niveles de Tratamiento (n = 3):
	base::tapply(X = data$Nitrogen, INDEX = data$Tratamiento, FUN = nortest::ad.test)

	## ¿Cuáles son las hipótesis de esta prueba?

	### El hecho de no poder asumir la normalidad influye principalmente en lo prueba de 
	### hipótesis paramétricos (t-test, anova, etc) y en los modelos de regresión.


# 6. ANÁLISIS DE VARIANZA: ANOVA UN FACTOR (ONE WAY ANOVA) ----------------

	
	# ANALISIS DE VARIANZA: ANOVA
	## La técnica de análisis de varianza (ANOVA) también conocida como análisis factorial y desarrollada por Fisher en 1930, 
	## constituye la herramienta básica para el estudio del efecto de uno o más factores (cada uno con dos o más niveles) 
	## sobre la media de una variable continua.
	
	## El funcionamiento básico de un ANOVA consiste en calcular la media de cada uno de los grupos para a continuación 
	## comparar la varianza de estas medias (varianza explicada por la variable grupo, intervarianza) 
	## frente a la varianza promedio dentro de los grupos (la no explicada por la variable grupo, intravarianza).

	## Bajo la hipótesis nula de que las observaciones de los distintos grupos proceden 
	## todas la misma población (tienen la misma media y varianza):
  
	##	H_0: No hay diferencias entre las medias de los diferentes grupos: μ1 = μ2... = μ_k ("miu" subíndice k; k-media).
	##
	##	H_1: Al menos un par de medias son significativamente distintas la una de la otra.


	## Aplicamos ANOVA por tratamiento: todos los datos.
	## summary(model): imprimite la tabla del análisis de varianza
	model_nitrogen <- stats::aov(formula = Nitrogen ~ Tratamiento, data = data)
	summary(model_nitrogen)
	
	## visualization of models: including qq-plot
	plot(model_nitrogen)

	## ejercicio ANOVA con Iris dataframe: https://jcoliver.github.io/learn-r/002-intro-stats.html

# 6. Prueba "Pos-hoc": Prueba de comparación de medias o múltiples --------

	## Si un Análisis de Varianza resulta significativo, implica que al menos dos de las medias comparadas 
	## difieren significativamente entre sí. Ignoramos quiénes difieren. 
	##
	## Para identificarlas se debe comparar dos a dos las medias de todos los grupos introducidos 
	## en el análisis mediante uan prueba que compare 2 grupos, ha esto se le conoce como análisis post-hoc.
	## 
	## El método de Tukey se utiliza en ANOVA para crear intervalos de confianza para 
	## todas las diferencias en parejas entre las medias de los niveles de los factores 
	## mientras controla la tasa de error por familia en un nivel especificado.

	## Prueba de Tukey para Nitrógeno, por tratamiento
	help(TukeyHSD, package = "stats")
	TukeyHSD(model_nitrogen, conf.level = 0.95, ordered = T)
	
	## Gráfico de Medias de Tukey:
	plot(TukeyHSD(model_nitrogen))

	

# 8. No paramétricos: Prueba de Kruskal Wallis --------

	
	## El test de Kruskal-Wallis, también conocido como test H, 
	## es la alternativa no paramétrica al test ANOVA de una vía para datos no pareados. 
	## A diferencia del ANOVA en el que se comparan medias, el test de Kruskal-Wallis
	## contrasta si las diferentes muestras están equidistribuidas y que por lo tanto 
	## pertenecen a una misma distribución (población).
	
	## HIPOTESIS A EVALUAR
	 
	## H_0: Todas las muestras provienen de la misma población (distribución).
	## 
	## H_1: Al menos una muestra proviene de una población con una distribución distinta.

	help("kruskal.test", package = "stats")
	Nitrogen_KW <- kruskal.test(formula = Nitrogen ~ Tratamiento, data = data)
	Nitrogen_KW


	## POST-HOC test: Dunn test
	## 
	## Al igual que ocurre con un ANOVA, si el test de Kruskal-Wallis es significativo, 
	## implica que al menos dos grupos de entre los comparados son significativamente diferentes, 
	## pero no indica cuales. Para saberlo es necesario compararlos todos entre ellos.
	
	## DUNN TEST
	## 
	## If the Kruskal–Wallis test is significant, a post-hoc analysis can be performed to determine 
	## which levels of the independent variable differ from each other level. 
	## Probably the most popular test for this is the Dunn test, which is performed with 
	## the dunnTest function in the FSA package.

	test_dunn <- FSA::dunnTest(x = Nitrogen ~ Tratamiento, data = data, method = "bh")
	test_dunn

	## Conclusión: No hay diferencias en el contenido de N entre tratamientos.
	

# 9. Determinación de estadísticos en forma de loop -----------------------

	## Habrá ocasiones en las que necesitamos hacer pruebas estadísticas sobre un número de largo de variables (columnas)
	## y es impráctico copiar y correr el mismo código sobre esas variables una por una.
	
	## R te permite programar la determinación de estadísticos por medio de "loop" que llevan a cabo la determinación
	## de manera repetitiva y te regresan los estadísticos en forma tabular con los p-values.
	
	## Para ello se necesitan scripts configurados con las órdenes adecuadas, i.e., programación en R.


# 9.1. Kruskall Wallis loop -----------------------------------------------
## Kruskall Wallis loop
	
source("scripts/loop_for_kruskal_wallis_test_function.R") ## loop para la prueba de Kruskall-Wallis sobre varias variables

	## a. Configuramos el dataframe: variable independiente (Factor), y variables de respuesta: wide format
	
	data.treatment <- data %>%
		dplyr::select(-1,-3,-4, -experimento); data.treatment
	
	## b. Aplicamos la función "loop" para realizar Kruskal-Wallis sobre todas las variables de df:
	loop.for.kruskal.wallis.test(data.treatment)
	
	## c. Resultado: se obtiene el tabular de las variables, y las valores de "p" correspondientes.


	
	data.practice <- data %>%
		dplyr::select(-1,-2,-4, -experimento); data.practice
	
	loop.for.kruskal.wallis.test(data.practice)


# 9.2. Dunn test in loop --------------------------------------------------
## DUNN TEST ##
## SOURCE https://stackoverflow.com/questions/63653727/dunns-test-to-loop-over-columns-of-a-data-frame

data
library(rstatix, warn.conflicts = F)

	## a. extraemos el nombre de la columna Tratamiento
	colname1 <- names(data)[2]; colname1
	class(colname1)
	
	## b. extraemos la variables numéricas: 5 a la última columna
	colname2 <- names(data)[5:10]
	class(colname2)
	colname2

	## c. Aplicaciomos el loop
	data_1 <- lapply(colname2, function(x) {
		rstatix::dunn_test(data, reformulate(colname1, x), p.adjust.method = "BH")
		}
		)

data_1





