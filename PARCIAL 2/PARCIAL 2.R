################################################################################################
#################                                                       ########################
################             UNIVERSIDAD DEL QUINDIO                    ########################
################                ECONOMETRIA 1                           ########################
################                  PARCIAL2                              ########################
################                                                        ########################
################################################################################################

# INTEGRANTES 
# SEBATSIAN GARCIA, NATALIA VALENCIA 
# 18/10/2026



# 1. Cargar librerías 
library(haven)
library(dplyr)
library(tidyr)
library(writexl)
library(psych)
library(car)
library(lmtest)
library(tseries)
library(sandwich)

# 2. Leer la base y filtrar el año
nls <- read_dta("C:/Users/User/Desktop/QUINTO SEMESTRE/ECONOMETRIA/UNIDAD 2/ACTIVIDADES/ACTIVIDAD 3/ACTIVIDAD_3_ECONOMETRIA/nls_panel.dta")

# 3. Año seleccionado 
nls83 <- nls %>% filter(year == 83)

# 4. Seleccionar variables del modelo
vars <- nls83 %>%
  select(lwage, educ, exper, tenure, union, black) %>%
  mutate(across(everything(), as.numeric)) %>%
  drop_na()

# 5. Exportar para Excel
datos_excel <- vars
write_xlsx(datos_excel, "nls_1983_modelo.xlsx")


glimpse(nls)
table(nls$year)


nrow(nls83)

write_xlsx(nls83, "nls_1983.xlsx")


describe(vars)

# 6. Asegurar que las variables sean numéricas (haven las trae etiquetadas)
vars <- nls83 %>%
  select(lwage, educ, exper, tenure, union, black) %>%
  mutate(across(everything(), as.numeric)) %>%
  drop_na()


# 7. Tabla de descriptivos
desc <- data.frame(
  n        = sapply(vars, length),
  media    = sapply(vars, mean),
  mediana  = sapply(vars, median),
  min      = sapply(vars, min),
  Q1       = sapply(vars, quantile, 0.25),
  Q3       = sapply(vars, quantile, 0.75),
  max      = sapply(vars, max),
  rango    = sapply(vars, function(x) max(x) - min(x)),
  IQR      = sapply(vars, IQR),
  varianza = sapply(vars, var),
  desv_est = sapply(vars, sd),
  CV       = sapply(vars, function(x) sd(x) / mean(x))
)


round(desc, 4)

# 8. Variables ficticias: frecuencias
table(vars$union)
prop.table(table(vars$union))
table(vars$black)
prop.table(table(vars$black))



# 9. Gráficos
hist(vars$lwage, main = "Histograma de lwage (1983)", xlab = "log del salario", col = "steelblue")
boxplot(lwage ~ union, data = vars, names = c("No sindicalizado", "Sindicalizado"),
        main = "lwage según afiliación sindical", ylab = "lwage")
cor(vars)

datos_excel <- vars %>% select(lwage, educ, exper, tenure, union, black)
write_xlsx(datos_excel, "nls_1983_modelo.xlsx")

datos_excel <- vars   # lwage, educ, exper, tenure, union, black
write_xlsx(datos_excel, "nls_1983_modelo.xlsx")
getwd()               # te muestra en qué carpeta se guardó


library(dplyr); library(car); library(lmtest); library(tseries); library(sandwich)

# --- 1. Estimación -------------------------------------------
modelo <- lm(lwage ~ educ + exper + tenure + union + black, data = vars)
s <- summary(modelo)
print(s)
confint(modelo, level = 0.95)

# --- 2. Contraste con Excel (valores de la hoja "BASE 2") ----
excel <- data.frame(
  termino = c("Intercepto", "educ", "exper", "tenure", "union", "black"),
  coef_excel = c(0.6135374, 0.0701255, 0.0286460, 0.0090594, 0.1591092, -0.1233524),
  ee_excel   = c(0.0872666, 0.0056688, 0.0055067, 0.0039428, 0.0320915,  0.0308705)
)
comparacion <- data.frame(
  excel,
  coef_R = coef(modelo),
  ee_R   = s$coefficients[, "Std. Error"],
  t_R    = s$coefficients[, "t value"],
  p_R    = s$coefficients[, "Pr(>|t|)"]
)
comparacion$dif_coef <- comparacion$coef_R - comparacion$coef_excel
comparacion$dif_ee   <- comparacion$ee_R   - comparacion$ee_excel
print(round(comparacion[, -1], 6))

# Estadísticos globales: Excel vs R
data.frame(
  medida = c("R2", "R2 ajustado", "Error estándar residual", "F", "n"),
  Excel  = c(0.3100214, 0.3051624, 0.3627780, 63.80349, 716),
  R      = c(s$r.squared, s$adj.r.squared, s$sigma, s$fstatistic[1], nobs(modelo))
)

# --- 3. Bondad de ajuste y significancia ---------------------
s$r.squared; s$adj.r.squared
s$fstatistic                                   # F global y gl
pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE)
AIC(modelo); BIC(modelo)

# Prueba conjunta adicional (ej.: las dos dummies a la vez)
linearHypothesis(modelo, c("union = 0", "black = 0"))

# --- 4. Diagnósticos de los supuestos ------------------------
vif(modelo)                                    # multicolinealidad
bptest(modelo)                                 # Breusch-Pagan (heterocedasticidad)
jarque.bera.test(residuals(modelo))            # normalidad de residuos
dwtest(modelo)                                 # autocorrelación (Durbin-Watson)
resettest(modelo, power = 2:3, type = "fitted")# forma funcional (RESET)

# Errores estándar robustos (corrige heterocedasticidad)
coeftest(modelo, vcov = vcovHC(modelo, type = "HC1"))

# --- 5. Gráficos de residuos ---------------------------------
par(mfrow = c(2, 2)); plot(modelo); par(mfrow = c(1, 1))
hist(residuals(modelo), breaks = 30, col = "steelblue",
     main = "Histograma de residuos", xlab = "Residuos")


