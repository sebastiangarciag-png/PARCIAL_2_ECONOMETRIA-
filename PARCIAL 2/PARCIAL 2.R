

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



