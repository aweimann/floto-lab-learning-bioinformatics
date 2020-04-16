###############################################################################
## Script for linear models. Bioinformatics workshop session 2.
###############################################################################

required_packs <- c("data.table", "magrittr", "ggplot2", "ggpubr", "ggpubr", 
                    "sjPlot", "broom")
packages_in <- required_packs %in% rownames(installed.packages())

if(!all(packages_in)) {
  
  install.packages(required_packs[!packages_in])

}

rm(required_packs, packages_in)
library(data.table)
library(magrittr)
library(ggplot2)
library(ggpubr)
library(sjPlot)
library(broom)

theme_set(ggpubr::theme_pubclean(base_size = 15)) # Load theme

# Grafica 1, para la presentación
d <- data.table(x = 1:10)
d[, y := x * 3 + rnorm(.N)]

ggplot(d, aes(x, y)) + 
  geom_point(col = "cornflowerblue", size = 3) + 
  geom_smooth(method = "lm", se = F, col = "black") + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  labs(x = "", y = "")
rm(d)

d <- data.table(Pois = rpois(500, 3))

ggplot(d, aes(x = Pois)) + 
  geom_histogram(fill = "cornflowerblue", bins = 20, col = "grey80",) + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),) + 
  labs(x = "", y = "")

# Sección 1: Regresión lineal simple ----------------------------------------------------------

# Importar datos

d <- fread("Datasets/BrownFat_2011.csv")

# Representaciones preliminares

d[sample(.N, 200)] %>% # Representar solo una parte de los datos 
  ggplot(., aes(x = as.factor(1), y = BMI)) +
  geom_point(shape = "-", size = 10, alpha = 0.3) +
  geom_point(aes(x = 1, y = mean(BMI)), shape = "_", size = 20, col = "brown1") +
  scale_y_continuous(name = bquote("Indice de Masa corporal - Kg/m"^2)) +
  theme_pubclean(base_size = 20) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

d %>% # Histograma
  ggplot(., aes(x = BMI)) +
  geom_histogram(shape = "-", col = "grey80", fill = "cornflowerblue") +
  coord_flip() + # Posicionar en vertical
  theme_void(base_size = 20) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),)

# Gráfica X-Y

d %>% 
  ggplot(., aes(x = Weight, y = BMI)) +
  geom_point(alpha = 0.33) +
  scale_x_continuous(name = "Peso - Kg") +
  theme_pubclean(base_size = 20) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 

# Primer modelo

m <- lm(BMI ~ Weight, data = d)

d[, fitted_m := fitted(m)] # Extraer datos ajustados

d %>% 
  ggplot(., aes(x = Weight, y = BMI)) +
  geom_point(alpha = 0.33) +
  geom_segment(aes(x = Weight,    y = BMI, 
                   xend = Weight, yend = fitted_m), alpha = 0.25, col = "brown1") +
  geom_smooth(method = "lm", se = F) +
  scale_x_discrete(name = "Peso en Kg")

summary(m) # Resumen del modelo

par(mfrow=c(2, 2)) # Para el siguiente paso, dividir el dispositivo de gráfico en 4 (solo base plot)
plot(m, cex.main = 1.25, cex.lab = 1.5, cex.axis = 1.5)
par(mfrow=c(1, 1))  # Devolver a la configuración original

# Variables discretas 

d[,Sex_c := ifelse(Sex == 1, "Female", "Male")]         # Trasnformar la variable Sex, en factor comprensible
#d[, Sex_c :=  relevel(as.factor(Sex_c), ref = "Hombre")] # Definir factor de referencia

d %>% 
  ggplot(., aes(x = Sex_c, y = Weight, fill = Sex_c)) +
  geom_boxplot(show.legend = FALSE) +
  scale_x_discrete(name = "Gender?") +
  scale_y_continuous(name = "Weigth Kg") +
  theme_pubclean(base_size = 20) +
  scale_fill_brewer(palette = "Set1")

m1 <- lm(Weight ~ Sex_c, data = d)

summary(m1)
plot(m1)

ref <- fread("Datasets/Key_for_brownfat.csv", )

cancer_t <- ref$Cancer_type

d[, Cancer_t_cat := cancer_t[Cancer_Type + 1] %>% as.factor()]
d[, Cancer_t_cat := relevel(Cancer_t_cat, ref = "No cancer") ]
m1.2 <- lm(Weight ~ Cancer_t_cat, data = d)

summary(m1.2)


d %>% 
  ggplot(., aes(x = Cancer_t_cat, y = Weight, fill = Cancer_t_cat)) +
  geom_boxplot(show.legend = FALSE) +
  scale_x_discrete(name = "Cancer Type") +
  scale_y_continuous(name = "Weigth Kg") +
  theme_pubclean(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Sección 2: Modelos multivariable --------------------------------------------------------------------------------

m2 <- update(m, ~ . + Sex_c )  # Dos parametros
m3.1 <- update(m, ~ Weight*Sex_c )    # Dos parámetros con interacción
m3.2 <- update(m, ~ Weight:Sex_c )    # Dos parámetros con interacción

summary(m2)
summary(m3.1)
summary(m3.2)

anova(m, m2, m3.1)   # Verificar si los modelos son identicos

d %>% 
  ggplot(., aes(x = Weight, y = BMI, col = Sex_c)) +
  geom_point(alpha = 0.33) +
  
  geom_smooth(method = "lm", mapping = aes(y = predict(m2, d)), fullrange = T) +  
  scale_x_continuous(name = "Weight - Kg") + 
  scale_color_brewer(palette = "Set1")


d %>% 
  ggplot(., aes(x = Weight, y = BMI, col = Sex_c)) +
  geom_point(alpha = 0.33) +
  
  geom_smooth(method = "lm", mapping = aes(y = predict(m3.1, d)), fullrange = T) +  
  scale_x_continuous(name = "Weight - Kg") + 
  scale_y_continuous(name = "") +
  scale_color_brewer(palette = "Set1")

d %>% 
  ggplot(., aes(x = Weight, y = BMI, col = Sex_c)) +
  geom_point(alpha = 0.33) +
  
  geom_smooth(method = "lm", mapping = aes(y = predict(m3.2, d)), fullrange = T) +  
  scale_x_continuous(name = "Weight - Kg") + 
  scale_y_continuous(name = "") +
  scale_color_brewer(palette = "Set1")

# Los modelos crecen - la base del overfitting -----------------------------
m4 <- lm(BMI ~ Weight*Age + Height + Day + Season + Ext_Temp, data = d)
m5 <- lm(BMI ~ Weight*Age + Height + Sex, data = d)

summary(m4)
summary(m5)
AIC(m4, m5, k = log(nrow(d))) %$% .[order(AIC), ]

# Presentar resultados

tidy(m5, conf.int = 95)     # Ver resultados
tidy(m5, conf.int = 95) %>% # Usar tidy de broom para producir tabla de resultados
  write.csv(., file = "Temp_table.csv") # Exportar resultados

# Representación gráfica, plot_model de sjPlot

plot_model(model = m5,                
           show.values = TRUE, 
           sort.est = TRUE,
           value.offset = .3)
# non-normally dist. data

d[BrownFat ==1] %>% # Histograma
  ggplot(., aes(x = Total_vol)) +
  geom_histogram(col = "grey80", fill = "cornflowerblue") +
  labs(x = "Brown fat volume") +
  theme_pubclean(base_size = 20) 

d[BrownFat ==1] %>%  # Histograma
  ggplot(., aes(x = log(Total_vol))) +
  geom_histogram(col = "grey80", fill = "cornflowerblue") +
  labs(x = "Log. of the brown fat volume") +
  theme_pubclean(base_size = 20) 

m6 <- lm(Total_vol ~ Age + Sex + BMI, data = d)
m7 <- lm(log(Total_vol + 1) ~ Age + Sex + BMI, data = d)
m8 <- lm(rank(Total_vol) ~ Age + Sex + BMI, data = d)

AIC(m6, m7, m8)

# Bola extra -----------------------------------------------------------------

summary(data) # Informe sumario de la tabla 
model <- lm(y ~ x, data = d) # Modelo simple
summary(model) # Informe sumario del modelo
coef(model)    # Extraer coeficientes
confint(model) # Extraer intervalos de confianza
plot(model)    # Representar modelo
predict(model, newdata = ) # Predecir nuevos datos no vistos por el modelo
fitted(model)  # Estraer valores ajustados
resid(model)   # Extraer residuos
allEffects(model) # Extraer todos los effectos del modelo
prcomp()      # Analisis de componentes principal para reducir variables
cor(x, y)         # Correlación entre dos variables
GGally::ggpairs(data) # Gráfica de pares para todas las variables
GGally::ggcorr(data)  # Gráfica de correlaciones para todas las variables


