library(wooldridge)
library(tidyverse)

wage1 <- wage1[, -1] # se quita la variable wage; la dependiente es lwage.

corwage1 <- wage1 %>% cor(method = "pearson") %>% round(digits = 2)
corwage1

set.seed(206)
# División de los datos en conjunto de entrenamiento (79%) y de prueba (21%)
train.sample <- sample(nrow(wage1)*.79)
wage1.train <- wage1[train.sample,]
wage1.test <- wage1[-train.sample,]

## 1a ## ----------------------------------------------------------------------

mco <- lm(lwage ~ ., data = wage1.train)
pred.mco <- predict(mco, newdata = wage1.test)
mse.mco <- mean((wage1.test[, "lwage"] - pred.mco)^2)
mse.mco

## 1b MSS ## ------------------------------------------------------------------
library(leaps)
nvar <- as.numeric(dim(wage1)[2] -1)
ssets.full <- regsubsets(lwage ~ ., data = wage1.train, nvmax = nvar)

pred.ssets.fs <- function(ob, newdata, id){ # función predictora
  form = as.formula(ob$call[[2]])
  mtx = model.matrix(form, newdata)
  coeff = coef(ob, id = id)
  xvars = names(coeff)
  mtx[, xvars] %*% coeff
}

k = 10
set.seed(206)
folds = sample(1:k, nrow(wage1.train), replace = T)
cv.err.ssets.10 = matrix(NA, k, nvar, dimnames = list(NULL, paste(1:nvar)))

for (j in 1:k){
  best.fit = regsubsets(lwage ~ ., data = wage1.train[folds != j,], nvmax = nvar)
  for (i in 1:nvar){
    pred = pred.ssets.fs(best.fit, wage1.train[folds == j, ], id = i)
    cv.err.ssets.10[j, i] = mean((wage1.train$lwage[folds == j] - pred)^2)
  }
}

rmse.cv = sqrt(apply(cv.err.ssets.10, 2, mean)) # raíz del error medio por columnas
plot(rmse.cv, pch = 19, type = "b")
min.point = min(rmse.cv)
sd.points = sd(rmse.cv)
abline(h = min.point + sd.points, col = "red", lty = "dashed")
legend("topright", "Línea de 1-desviación típica", lty = "dashed", col = "red")

# Elegimos el número de variables dependiendo de la regla del codo
dt.cv = sd(rmse.cv)
codo = which.max(rmse.cv - dt.cv <= min(rmse.cv))
coef(ssets.full, codo)

# Se hace la regresión por Mejor Selección de Conjuntos usando "codo" variables.
lm.pred.codo = pred.ssets.fs(ssets.full, newdata = wage1.test, id = codo)
error.mss.codo.10 <- mean((wage1.test[, "lwage"] - lm.pred.codo)^2)
error.mss.codo.10

## 1c MSHA ## -----------------------------------------------------------------
set.seed(206)
# implementamos forward selection con method = "forward"
# k y nvar se toman de 1b
fs.full <- regsubsets(lwage ~ ., data = wage1.train, nvmax = nvar, method = "forward")
cv.err.fs.10 = matrix(NA, k, nvar, dimnames = list(NULL, paste(1:nvar)))

for (j in 1:k){ # folds se toma de 1b
  best.fit = regsubsets(lwage ~ ., data = wage1.train[folds != j,], 
                        nvmax = nvar, method = "forward")
  for (i in 1:nvar){
    pred = pred.ssets.fs(best.fit, wage1.train[folds == j, ], id = i)
    cv.err.fs.10[j, i] = mean((wage1.train$lwage[folds == j] - pred)^2)
  }
}

rmse.cv = sqrt(apply(cv.err.fs.10, 2, mean))
plot(rmse.cv, pch = 19, type = "b")
min.point = min(rmse.cv)
sd.points = sd(rmse.cv)
abline(h=min.point + sd.points, col = "red", lty = "dashed")
legend("topright", "Línea de 1-desviación típica", lty = "dashed", col = "red")

dt.cv = sd(rmse.cv)
codo = which.max(rmse.cv - dt.cv <= min(rmse.cv))
coef(fs.full, codo)

lm.pred.codo = pred.ssets.fs(fs.full, newdata = wage1.test, id = codo)
error.msha.codo.10 <- mean((wage1.test[, "lwage"] - lm.pred.codo)^2)
error.msha.codo.10

## d MSS y MSHA con k = 5 ## --------------------------------------------------

# MEJOR SELECCIÓN DE CONJUNTOS con k = 5
k = 5
set.seed(206)
folds = sample(1:k, nrow(wage1.train), replace = T)
cv.err.ssets.5 = matrix(NA, k, nvar, dimnames = list(NULL, paste(1:nvar)))

for (j in 1:k){
  best.fit = regsubsets(lwage ~ ., data = wage1.train[folds != j,], nvmax = nvar)
  for (i in 1:nvar){
    pred = pred.ssets.fs(best.fit, wage1.train[folds == j, ], id = i)
    cv.err.ssets.5[j, i] = mean((wage1.train$lwage[folds == j] - pred)^2)
  }
}

rmse.cv = sqrt(apply(cv.err.ssets.5, 2, mean))
plot(rmse.cv, pch = 19, type = "b")
min.point = min(rmse.cv)
sd.points = sd(rmse.cv)
abline(h=min.point + sd.points, col = "red", lty = "dashed")
legend("topright", "Línea de 1-desviación típica", lty = "dashed", col = "red")

# Elegimos el número de variables dependiendo de la regla del codo
dt.cv = sd(rmse.cv)
codo = which.max(rmse.cv - dt.cv <= min(rmse.cv))
coef(ssets.full, codo)

# Se hace la regresión por Mejor Selección de Conjuntos usando "codo" variables.
lm.pred.codo = pred.ssets.fs(ssets.full, newdata = wage1.test, id = codo)
error.mss.codo.5 <- mean((wage1.test[, "lwage"] - lm.pred.codo)^2)
error.mss.codo.5

# FORWARD SELECTION (k = 5)
set.seed(206)
cv.err.fs.5 = matrix(NA, k, nvar, dimnames = list(NULL, paste(1:nvar)))

for (j in 1:k){
  best.fit = regsubsets(lwage ~ ., data = wage1.train[folds != j,], 
                        nvmax = nvar, method = "forward")
  for (i in 1:nvar){
    pred = pred.ssets.fs(best.fit, wage1.train[folds == j, ], id = i)
    cv.err.fs.5[j, i] = mean((wage1.train$lwage[folds == j] - pred)^2)
  }
}

rmse.cv = sqrt(apply(cv.err.fs.5, 2, mean))
plot(rmse.cv, pch = 19, type = "b")
min.point = min(rmse.cv)
sd.points = sd(rmse.cv)
abline(h=min.point + sd.points, col = "red", lty = "dashed")
legend("topright", "Línea de 1-desviación típica", lty = "dashed", col = "red")

dt.cv = sd(rmse.cv)
codo = which.max(rmse.cv - dt.cv <= min(rmse.cv))
coef(fs.full, codo)

lm.pred.codo = pred.ssets.fs(fs.full, newdata = wage1.test, id = codo)
error.msha.codo.5 <- mean((wage1.test[, "lwage"] - lm.pred.codo)^2)
error.msha.codo.5

## 1e Resumen ## --------------------------------------------------------------
A <- matrix( c(mse.mco, error.mss.codo.10, error.mss.codo.5, error.msha.codo.10, 
               error.msha.codo.5), nrow = 1, ncol = 5)
dimnames(A) <- list("RMSE", c("MCO", "MSS.VC10", "MSS.VC5", "MSHA.VC10", "MSHA.VC5"))
which.min(A)

## 1f Mejor modelo ## ---------------------------------------------------------
# El mejor modelo parece ser el de Mínimos Cuadrados Ordinarios (MCO) con todas
# las variables explicativas.
summary(mco)

# Sin embargo, se hace necesario quitar muchas de las variables.

# Se va quitando uno cada vez hasta que no queda ningún coeficiente con p-valor
# por encima de 0.05:
# se quita la primera (1) variable: construc
best.mco <- lm(lwage ~ . -construc, data = wage1.train)
summary(best.mco)
# se quita la segunda (2) variable: nonwhite
best.mco <- lm(lwage ~ . -construc-nonwhite, data = wage1.train)
summary(best.mco)
# se quita la tercera (3) variable: married
best.mco <- lm(lwage ~ . -construc-nonwhite-married, data = wage1.train)
summary(best.mco)
# se quita la cuarta (4) variable: south
best.mco <- lm(lwage ~ . -construc-nonwhite-married-south, data = wage1.train)
summary(best.mco)
# se quita la quinta (5) variable: northcen
best.mco <- lm(lwage ~ . -construc-nonwhite-married-south-northcen, 
               data = wage1.train)
summary(best.mco)
# se quita la sexta (6) variable: clerocc
best.mco <- lm(lwage ~ . -construc-nonwhite-married-south-northcen-clerocc, 
               data = wage1.train)
summary(best.mco)
# se quita la séptima (7) variable: trcommpu
best.mco <- lm(lwage ~ . -construc-nonwhite-married-south-northcen-clerocc-trcommpu, 
               data = wage1.train)
summary(best.mco)
# se quita la octava (8) variable: profserv
best.mco <- lm(lwage ~ . -construc-nonwhite-married-south-northcen-clerocc-profserv-trcommpu, 
               data = wage1.train)
summary(best.mco)
# se quita la novena (9) variable: numdep
best.mco <- lm(lwage ~ . -construc-nonwhite-married-south-northcen-clerocc-profserv-trcommpu-numdep, 
               data = wage1.train)
summary(best.mco)
# se quita la décima (10) variable: ndurman
best.mco <- lm(lwage ~ . -construc-nonwhite-married-south-northcen-clerocc-profserv-trcommpu-numdep-ndurman, 
               data = wage1.train)
summary(best.mco)
# se quita la undécima (11) variable: tenursq
best.mco <- lm(lwage ~ . -construc-nonwhite-married-south-northcen-clerocc-profserv-trcommpu-numdep-ndurman-tenursq, 
               data = wage1.train)
summary(best.mco)
# se quita la deodécima (12) variable: west
best.mco <- lm(lwage ~ . -construc-nonwhite-married-south-northcen-clerocc-profserv-trcommpu-numdep-ndurman-tenursq-west, 
               data = wage1.train)
summary(best.mco)

pred.best.mco <- predict(best.mco, newdata = wage1.test)
mse.best.mco <- mean((wage1.test[, "lwage"] - pred.best.mco)^2)
mse.best.mco
mse.mco 
# El error empeora con respecto del modelo MCO con todas las variables.


## 1g Ridge ## ----------------------------------------------------------------
set.seed(206)
library(glmnet)

x = model.matrix(lwage ~ ., wage1)[, -1] # Se excluye la columna que genera intercepto
y = wage1$lwage
y.test = y[-train.sample]

grid = 10^seq(10, -4, length = 100) # se definen distintos valores de lambda.

cv.ridge = cv.glmnet(x[train.sample,], y[train.sample], alpha = 0, 
                     lambda = grid) # nfolds = 10 por defecto.

plot(cv.ridge)

mejorlambda.ridge.10 = cv.ridge$lambda.min
mejorlambda.ridge.10

ridge.mod = glmnet(x[train.sample, ], y[train.sample], alpha = 0, lambda = grid)

# Regla del "codo" de una DT del error de VC:
lambda.codo.ridge.10 <- cv.ridge$lambda.1se
lambda.codo.ridge.10

ridge.pred.10 = predict(ridge.mod, s = lambda.codo.ridge.10, newx = x[-train.sample,])
error.ridge.10 <- mean((ridge.pred.10 - wage1.test[, "lwage"] )^2)
error.ridge.10


## 1h LASSO ## ----------------------------------------------------------------
set.seed(206)

cv.lasso = cv.glmnet(x[train.sample,], y[train.sample], alpha = 1, 
                     lambda = grid) # nfolds = 10 por defecto.

plot(cv.lasso)
bestlam = cv.lasso$lambda.min
bestlam

lasso.mod = glmnet(x[train.sample,], y[train.sample], alpha = 1, lambda = grid)

coef(cv.lasso) # Se han seleccionado 11 variables: educ(1), exper(2), tenure(3), 
# female(5), married(6), smsa(8), west(11), trade(15), services(16), profocc(18), 
# servocc(20).

# Regla del "codo" de una DT del error de VC:
lambda.codo.l.10 <- cv.lasso$lambda.1se
lambda.codo.l.10

lasso.pred.10 = predict(lasso.mod, s = lambda.codo.l.10, newx = x[-train.sample,])
error.lasso.10 <- mean((lasso.pred.10 - wage1.test[, "lwage"] )^2)
error.lasso.10


## 1i Ridge & LASSO 5-CV ## ---------------------------------------------------
# RIDGE
set.seed(206)
cv.ridge = cv.glmnet(x[train.sample,], y[train.sample], alpha = 0, 
                     lambda = grid, nfolds = 5)

mejorlambda = cv.ridge$lambda.min
mejorlambda

ridge.mod = glmnet(x[train.sample, ], y[train.sample], alpha = 0, lambda = grid)

# Regla del "codo" de una DT del error de VC:
lambda.codo.ridge.5 <- cv.ridge$lambda.1se
lambda.codo.ridge.5

ridge.pred.5 = predict(ridge.mod, s = lambda.codo.ridge.5, newx = x[-train.sample,])
error.ridge.5 <- mean((ridge.pred.5 - wage1.test[, "lwage"] )^2)
error.ridge.5

# LASSO
set.seed(206)
cv.lasso = cv.glmnet(x[train.sample,], y[train.sample], alpha = 1, 
                     lambda = grid, nfolds = 5)

bestlam = cv.lasso$lambda.min
bestlam

lasso.mod = glmnet(x[train.sample,], y[train.sample], alpha = 1, 
                   lambda = grid)
plot(lasso.mod)

coef(cv.lasso) # Se han seleccionado 11 variables: educ(1), exper(2), tenure(3),
# female(5), married(6), smsa(8), west(11), trade(15), services(16), profocc(18),
# servocc(20)

# Regla del "codo" de una DT del error de VC:
lambda.codo.l.5 <- cv.lasso$lambda.1se
lambda.codo.l.5

lasso.pred.5 = predict(lasso.mod, s = lambda.codo.l.5, newx = x[-train.sample,])
error.lasso.5 <- mean((lasso.pred.5 - wage1.test[, "lwage"] )^2)
error.lasso.5


## 1j CP ## -------------------------------------------------------------------
library(pls)
set.seed(206)

# 10-FOLD-CV
pcr.fit = pcr(lwage ~ ., data = wage1.train, scale = T, validation = "CV") # segments = 10 por defecto.
pcr.cv <- crossval(pcr.fit, segments = 10)
plot(RMSEP(pcr.cv), legendpos = "topright") # RMSE

## Selecciona el número de componentes principales
## Regla del codo: 1 desviación típica
ncomp.1.d.t.10 <- selectNcomp(pcr.fit, method = "onesigma", plot = T,
                              validation = "CV", segments = 10)
pcr.pred = predict(pcr.fit, newdata = x[-train.sample,], ncomp = ncomp.1.d.t.10)
error.pcr.10 <- mean((pcr.pred - wage1.test[, "lwage"])^2)
error.pcr.10

# 5-FOLD-CV
pcr.fit = pcr(lwage ~ ., data = wage1.train, scale = T, validation = "CV", segments = 5)
pcr.cv <- crossval(pcr.fit, segments = 5)
plot(RMSEP(pcr.cv), legendpos = "topright") # RMSE

summary(pcr.cv, what = "validation")

## Selecciona el número de componentes principales
## Regla del codo: 1 desviación típica
ncomp.1.d.t.5 <- selectNcomp(pcr.fit, method = "onesigma", plot = T, 
                            validation = "CV", segments = 5)
pcr.pred = predict(pcr.fit, newdata = x[-train.sample,], ncomp = ncomp.1.d.t.5)
error.pcr.5 <- mean((pcr.pred - wage1.test[, "lwage"])^2)
error.pcr.5

# RESUMEN DE RESULTADOS
CP <- matrix(c(ncomp.1.d.t.10, error.pcr.10, ncomp.1.d.t.5, error.pcr.5),
             nrow = 2, ncol = 2, byrow = T)
dimnames(CP) <- list(c("10-FOLDS-CV", "5-FOLDS-CV"), c("CP", "RMSE"))
CP

## 1k PLS ## ------------------------------------------------------------------

# 10-FOLD-CV
set.seed(206)
pls.fit = plsr(lwage ~ ., data = wage1.train, scale = T, validation = "CV")

pls.cv <- crossval(pls.fit, segments = 10)
plot(RMSEP(pls.cv), legendpos = "topright")

summary(pls.cv, what = "validation")

## Utilizamos 16 componentes por el Mínimo Error de VC
pls.pred = predict(pls.fit, newdata = x[-train.sample,], ncomp = 16)
error.pls.10 <- mean((pls.pred - wage1.test[, "lwage"])^2)
error.pls.10

## Selecciona el número de componentes principales
## Regla del codo: 1 d.t.
ncomp.1.d.t. <- selectNcomp(pls.fit, method = "onesigma", plot = T, 
                            validation = "CV", segments = 10)
pls.pred.2 = predict(pls.fit, newdata = x[-train.sample,], ncomp = ncomp.1.d.t.)
error.pls.codo.10 <- mean((pls.pred.2 - wage1.test[, "lwage"])^2)
error.pls.codo.10

# 5-FOLD-CV
set.seed(206)
pls.fit = plsr(lwage ~ ., data = wage1.train, scale = T, validation = "CV", segments = 5)
summary(pls.fit)

pls.cv <- crossval(pls.fit, segments = 5)
plot(RMSEP(pls.cv), legendpos = "topright")

summary(pls.cv, what = "validation")

## Utilizamos 16 componentes por el Mínimo Error de VC
pls.pred = predict(pls.fit, newdata = x[-train.sample,], ncomp = 16)
error.pls.5 <- mean((pls.pred - wage1.test[, "lwage"])^2)
error.pls.5

## Selecciona el número de componentes principales
## Regla del codo: 1 d.t.
ncomp.1.d.t. <- selectNcomp(pls.fit, method = "onesigma", plot = T, 
                            validation = "CV", segments = 5)
pls.pred.2 = predict(pls.fit, newdata = x[-train.sample,], ncomp = ncomp.1.d.t.)
error.pls.codo.5 <- mean((pls.pred.2 - wage1.test[, "lwage"])^2)
error.pls.codo.5

# RANDOMIZATION
## Se selecciona el ncomp que nos da el min Error de VC.
## Luego se contrasta si hay una deterioro del Error de VC al quitar un CP
## del Modelo de PLS, utilizando un alpha = 0.05.
ncomp.perm <- selectNcomp(pls.fit, method = "randomization", plot = T)
ncomp.perm
pls.pred.3 = predict(pls.fit, newdata = x[-train.sample,], ncomp = ncomp.perm)
error.pls.perm <- mean((pls.pred.3 - wage1.test[, "lwage"])^2)
error.pls.perm

## 1l Elastic Net ## ----------------------------------------------------------

library(glmnet)
library(caret) # se crean dos rangos, uno para alphas y otro para lambdas.
lambda.grid <- 10^seq(-2,-3, length = 100) # conjunto de valores de lambda
alpha.grid <- seq(0,1, by = 0.05) # conjunto de valores de alfa (elasticidad), siempre entre 0 y 1

# 10-Folds-CV
Control <- trainControl(method = "cv", number = 10) # 10-Folds-CV

busca.grid <- expand.grid(alpha = alpha.grid, lambda = lambda.grid)

set.seed(206) # Entrenamiento del modelo:
mi.entrenamiento.10 <- train(lwage ~., data = wage1.train, method = "glmnet", 
                             tuneGrid = busca.grid, trControl = Control,
                             tuneLength = 10,
                             standardize = T, maxit = 1000000)

plot(mi.entrenamiento.10)
mi.entrenamiento.10$bestTune # mejor modelo

mi.modelo.glmnet.10 <- mi.entrenamiento.10$finalModel
coef(mi.modelo.glmnet.10, s = mi.entrenamiento.10$bestTune$lambda)

mej.modelo.10 <- glmnet(x[train.sample,],y[train.sample], 
                        alpha = mi.entrenamiento.10$bestTune$alpha,
                        lambda = mi.entrenamiento.10$bestTune$lambda) # el modelo final contiene 21 variables con los beta contraidos.

coef(mej.modelo.10, s = mi.entrenamiento.10$bestTune$lambda)
cbind(coef(mej.modelo.10, s = mi.entrenamiento.10$bestTune$lambda), 
      coef(mi.modelo.glmnet.10, s = mi.entrenamiento.10$bestTune$lambda))
# el método de red elástica en este caso excluye las mismas variables.

lre.pred.en.10 <- predict(mej.modelo.10, s = mi.entrenamiento.10$bestTune$lambda, 
                          newx = x[-train.sample,])
error.pred.en.10 <- mean((lre.pred.en.10 - y.test)^2)
error.pred.en.10 # mejor que error.pls

alpha.en.10 <- mi.entrenamiento.10$bestTune[1,1]
lambda.en.10 <- mi.entrenamiento.10$bestTune[1,2]

## 1m Elastic Net VC 5 ## -----------------------------------------------------
# 5-Folds-CV
Control <- trainControl(method = "cv", number = 5) # 5-Folds-CV

busca.grid <- expand.grid(alpha = alpha.grid, lambda = lambda.grid)

set.seed(206) # Entrenamiento del modelo:
mi.entrenamiento.5 <- train(lwage~., data = wage1.train, method = "glmnet", 
                            tuneGrid = busca.grid, trControl = Control,
                            tuneLength = 5,
                            standardize = T, maxit = 1000000)

plot(mi.entrenamiento.5)
mi.entrenamiento.5$bestTune # mejor modelo

mi.modelo.glmnet.5 <- mi.entrenamiento.5$finalModel
coef(mi.modelo.glmnet.5, s = mi.entrenamiento.5$bestTune$lambda)

mej.modelo.5 <- glmnet(x[train.sample,],y[train.sample], 
                       alpha=mi.entrenamiento.5$bestTune$alpha,
                       lambda = mi.entrenamiento.5$bestTune$lambda) # el modelo final contiene 21 variables con los beta contraidos.

coef(mej.modelo.5, s = mi.entrenamiento.5$bestTune$lambda)
cbind(coef(mej.modelo.5, s = mi.entrenamiento.5$bestTune$lambda), 
      coef(mi.modelo.glmnet.5, s = mi.entrenamiento.5$bestTune$lambda))
# el método de red elástica en este caso excluye las mismas variables.

lre.pred.en.5 <- predict(mej.modelo.5, s = mi.entrenamiento.5$bestTune$lambda, 
                         newx = x[-train.sample,])
error.pred.en.5 <- mean((lre.pred.en.5 - y.test)^2)
error.pred.en.5 # mejor que error.pls pero peor que el de 10-Folds-CV.

alpha.en.5 <- mi.entrenamiento.5$bestTune[1,1]
lambda.en.5 <- mi.entrenamiento.5$bestTune[1,2]

## 1n Métodos de contracción con inferencia ## --------------------------------

# BOOTSTRAP FUNCTION

boot.alpha.cv <- function(alpha, nfolds){
  set.seed(206)
  B = 400
  betas <- matrix(0, B, (nvar + 1))
  lambdas <- rep(0, B)
  n.x <- nrow(x[train.sample,])
  for (i in 1:B){
    mi.muestra <- sample(nrow(x[train.sample,]), size = n.x, replace = T)
    x.b <- x[mi.muestra,]
    y.b <- y[mi.muestra]
    cv.out = cv.glmnet(x.b, y.b, alpha = alpha, nfolds = nfolds)
    fit1 = glmnet(x.b, y.b, alpha = alpha)
    coefs <-coef(fit1, s = cv.out$lambda.min)
    betas[i,] <- coefs[,1]
    lambdas[i] <- cv.out$lambda.min
  }
  media.betas <- apply(betas, 2, mean)
  points(drop(media.betas), col = 3, pch = 18)
  sd.betas <- apply(betas, 2, sd)
  limite.inferior <- apply(betas, 2, quantile, probs = 0.025, na.rm = F)
  limite.superior <- apply(betas, 2, quantile, probs = 0.975, na.rm = F)
  intervalo.percentil = cbind(limite.inferior, limite.superior)
  rownames(intervalo.percentil) <- rownames(coefs)
  return(intervalo.percentil)
}

# RIDGE VC 10
bootsridge10 <- boot.alpha.cv(alpha = 0, nfolds = 10)

# El bucle siguiente elimina todas las variables cuyos intervalos de confianza
# contienen el valor 0.
delete.var <- c() 
for (i in 2:nrow(bootsridge10)){
  if (bootsridge10[i, 1]*bootsridge10[i, 2] <= 0){
    delete.var <- c(delete.var, i-1)
  }
}

x1 <- x[,-delete.var]
colnames(x1)

set.seed(206)
grid = 10^seq(3, -4, length = 100)
cv.ridge = cv.glmnet(x1[train.sample,], y[train.sample], alpha = 0, 
                     lambda = grid)
plot(cv.ridge)

ridge.mod = glmnet(x1[train.sample,], y[train.sample], alpha = 0, lambda = grid)

lambda.codo <- cv.ridge$lambda.1se
lambda.codo

ridge.pred.2 = predict(ridge.mod, s = lambda.codo, newx = x1[-train.sample,])
error.ridge.2.10 <- mean((ridge.pred.2 - wage1.test[, "lwage"] )^2)
error.ridge.2.10

# RIDGE VC 5
bootsridge5 <- boot.alpha.cv(alpha = 0, nfolds = 5)

delete.var <- c()
for (i in 2:nrow(bootsridge5)){
  if (bootsridge5[i, 1]*bootsridge5[i, 2] <= 0){
    delete.var <- c(delete.var, i-1)
  }
}

x1 <- x[,-delete.var]

set.seed(206)
grid = 10^seq(3, -4, length = 100)
cv.ridge = cv.glmnet(x1[train.sample,], y[train.sample], alpha = 0, lambda = grid)
plot(cv.ridge)

ridge.mod = glmnet(x1[train.sample,], y[train.sample], alpha = 0, lambda = grid)

lambda.codo <- cv.ridge$lambda.1se
lambda.codo

ridge.pred.2 = predict(ridge.mod, s = lambda.codo, newx = x1[-train.sample,])
error.ridge.2.5 <- mean((ridge.pred.2 - wage1.test[, "lwage"] )^2)
error.ridge.2.5

# LASSO VC 10
bootslasso10 <- boot.alpha.cv(alpha = 1, nfolds = 10)

delete.var <- c()
for (i in 2:nrow(bootslasso10)){
  if (bootslasso10[i,1]*bootslasso10[i,2] <= 0){
    delete.var <- c(delete.var, i-1)
  }
}

x1 <- x[,-delete.var]
colnames(x1)

set.seed(206)
grid = 10^seq(1, -4, length = 100)
cv.lasso = cv.glmnet(x1[train.sample,], y[train.sample], alpha = 1, lambda = grid)
plot(cv.lasso)

lasso.mod = glmnet(x1[train.sample,], y[train.sample], alpha = 1, lambda = grid)

lambda.codo <- cv.lasso$lambda.1se
lambda.codo

lasso.pred.10 = predict(lasso.mod, s = lambda.codo, newx = x1[-train.sample,])
error.lasso.2.10 <- mean((lasso.pred.10-wage1.test[, "lwage"] )^2)
error.lasso.2.10

# LASSO VC 5
bootslasso5 <- boot.alpha.cv(alpha = 1, nfolds = 5)

delete.var <- c()
for (i in 2:nrow(bootslasso5)){
  if (bootslasso5[i, 1]*bootslasso5[i, 2] <= 0){
    delete.var <- c(delete.var, i-1)
  }
}

x1 <- x[,-delete.var]
colname(x1)

set.seed(206)
grid = 10^seq(1, -4, length = 100)
cv.lasso = cv.glmnet(x1[train.sample,], y[train.sample], alpha = 1, lambda = grid)
plot(cv.lasso)

lasso.mod = glmnet(x1[train.sample,], y[train.sample], alpha = 1, lambda = grid)

lambda.codo <- cv.lasso$lambda.1se
lambda.codo

lasso.pred.5 = predict(lasso.mod, s = lambda.codo, newx = x1[-train.sample,])
error.lasso.2.5 <- mean((lasso.pred.5 - wage1.test[, "lwage"] )^2)
error.lasso.2.5 # el mismo de VC 10 veces

# ELASTIC NET VC 10 veces

library(glmnet)
library(caret)

lambda.grid <- 10^seq(2, -2, length = 100)
alpha.grid <- seq(0, 1, by = 0.05)
busca.grid <- expand.grid(alpha = alpha.grid, lambda = lambda.grid)

B <- 400
betas <- matrix(0, B, (nvar + 1))
lambdas <- rep(0, B)
n.x <- nrow(x[train.sample,])
for (i in 1:B){
  mi.muestra <- sample(nrow(x[train.sample,]), size = n.x, replace = T)
  x.b <- x[mi.muestra,]
  y.b <- y[mi.muestra]
  mi.entrenamiento <- train(lwage~., data = wage1.train, method = "glmnet", 
                            tuneGrid = busca.grid, trControl = Control,
                            tuneLength = 10,
                            standardize = T, maxit = 100)
  mi.modelo.glmnet <- mi.entrenamiento$finalModel
  mejmodelo <- glmnet(x.b, y.b, alpha = mi.entrenamiento$bestTune$alpha,
                       lambda = mi.entrenamiento$bestTune$lambda)
  coefs <- coef(mejmodelo, s = mi.entrenamiento$bestTune$lambda)
  betas[i,] <- coefs[,1]
}

boxplot(betas)
abline(h = 0, col = "green", pch = 20)
media.betas <- apply(betas, 2, mean) # para obtener los betas.
points(drop(media.betas), col = 3, pch = 18)
sd.betas <- apply(betas, 2, sd)
limite.inferior <- apply(betas, 2, quantile, probs = .025, na.rm = F)
limite.superior <- apply(betas, 2, quantile, probs = .975, na.rm = F)
intervalo.percentil = cbind(limite.inferior,limite.superior)
rownames(intervalo.percentil) <- rownames(coefs)
intervalo.percentil
mean(lambdas)

# Se seleccionan educ, tenure, female, married, smsa, trade, services, profocc,
# y servocc

x1 <- x[, c(1, 3, 5, 6, 8, 15, 16, 18, 20)]

lambda.grid <- 10^seq(2, -2, length = 100)
alpha.grid <- seq(0, 1, by = .05)
Control <- trainControl(method = "cv", number = 10)
busca.grid <- expand.grid(alpha = alpha.grid, lambda = lambda.grid)

mi.entrenamiento <- train(lwage~educ+tenure+female+married+smsa+trade+services+profocc+servocc, 
                          data = wage1.train, method = "glmnet", 
                          tuneGrid = busca.grid, trControl = Control,
                          tuneLength = 10,
                          standardize = T, maxit = 100)

mi.modelo.glmnet <- mi.entrenamiento$finalModel

mej.modelo.10 <- glmnet(x1[train.sample,], y[train.sample], 
                        alpha = mi.entrenamiento$bestTune$alpha,
                        lambda = mi.entrenamiento$bestTune$lambda)

coef(mej.modelo.10, s = mi.entrenamiento$bestTune$lambda)
cbind(coef(mej.modelo.10, s = mi.entrenamiento$bestTune$lambda), 
      coef(mi.modelo.glmnet, s = mi.entrenamiento$bestTune$lambda))
# el método de red elástica en este caso no excluye ninguna variable y los coeficientes
# se parecen.

lre.pred.en.10 <- predict(mej.modelo.10, s = mi.entrenamiento$bestTune$lambda, 
                          newx = x1[-train.sample,])
error.pred.en.2.10 <- mean((lre.pred.en.10 - y.test)^2)
error.pred.en.2.10

# ELASTIC NET VC 5 veces

lambda.grid <- 10^seq(2, -2, length = 100)
alpha.grid <- seq(0, 1, by = .05)
Control <- trainControl(method = "cv", number = 5)

B <- 400
betas <- matrix(0,B,(nvar + 1))
lambdas <- rep(0, B)
n.x <- nrow(x[train.sample,])
for (i in 1:B){
  mi.muestra <- sample(nrow(x[train.sample,]), size = n.x, replace = T)
  x.b <- x[mi.muestra,]
  y.b <- y[mi.muestra]
  mi.entrenamiento <- train(lwage ~ ., data = wage1.train, method = "glmnet", 
                            tuneGrid = busca.grid, trControl = Control,
                            tuneLength = 5,
                            standardize = T, maxit = 100)
  mi.modelo.glmnet <- mi.entrenamiento$finalModel
  mejmodelo <- glmnet(x.b, y.b, alpha = mi.entrenamiento$bestTune$alpha,
                      lambda = mi.entrenamiento$bestTune$lambda)
  coefs <- coef(mejmodelo, s = mi.entrenamiento$bestTune$lambda)
  betas[i,] <- coefs[,1]
}

sd.betas <- apply(betas, 2, sd)
limite.inferior <- apply(betas, 2, quantile, probs = .025, na.rm = F)
limite.superior <- apply(betas, 2, quantile, probs = .975, na.rm = F)
intervalo.percentil = cbind(limite.inferior,limite.superior)
rownames(intervalo.percentil) <- rownames(coefs)
intervalo.percentil

# Se seleccionan educ, tenure, female, married, smsa, trade, services, profocc,
# servocc

x1 <- x[, c(1, 3, 5, 6, 8, 15, 16, 18, 20)]

lambda.grid <- 10^seq(2, -2, length = 100)
alpha.grid <- seq(0, 1, by = .05)
Control <- trainControl(method = "cv", number = 5)
busca.grid <- expand.grid(alpha = alpha.grid, lambda = lambda.grid)

mi.entrenamiento <- train(lwage~educ+tenure+female+married+smsa+trade+services+profocc+servocc, 
                          data = wage1.train, method = "glmnet", 
                          tuneGrid = busca.grid, trControl = Control,
                          tuneLength = 5,
                          standardize = T, maxit = 100)
mi.modelo.glmnet <- mi.entrenamiento$finalModel

mej.modelo.5 <- glmnet(x1[train.sample,], y[train.sample], 
                       alpha=mi.entrenamiento$bestTune$alpha,
                       lambda = mi.entrenamiento$bestTune$lambda)

cbind(coef(mej.modelo.5, s = mi.entrenamiento$bestTune$lambda), 
      coef(mi.modelo.glmnet, s = mi.entrenamiento$bestTune$lambda))
# el método de red elástica en este caso no excluye variables y los coeficientes
# se parecen.

lre.pred.en.5 <- predict(mej.modelo.5, s = mi.entrenamiento$bestTune$lambda, 
                         newx = x1[-train.sample,])
error.pred.en.2.5 <- mean((lre.pred.en.5 - y.test)^2)
error.pred.en.2.5

## 1o LASSO riguroso y ajustes Post-LASSO ## ----------------------------------

library(hdm)
set.seed(206)

# penalización independiente de los datos (LASSO)
lasso.reg = rlasso(y[train.sample] ~ x[train.sample,], post = F)
summary(lasso.reg, all = F)

yhat.lasso = predict(lasso.reg) # in sample prediction
yhat.lasso.new = predict(lasso.reg, newdata = x[-train.sample,])
error.rlasso <- mean((yhat.lasso.new - wage1.test[, "lwage"] )^2)
error.rlasso # 11 variables distintas de 0

# penalización dependiente de los datos (LASSO)

lasso.reg.dep = rlasso(y[train.sample] ~ x[train.sample,], post = F, X.dependent.lambda = T)
summary(lasso.reg.dep, all = F)

yhat.lasso.dep = predict(lasso.reg.dep) # in sample prediction
yhat.lasso.new.dep = predict(lasso.reg, newdata = x[-train.sample,])
error.rlasso.dep <- mean((yhat.lasso.new.dep-wage1.test[, "lwage"] )^2)
error.rlasso.dep # 11 variables distintas de 0

# penalización independiente de los datos (Post-LASSO)
post.lasso.reg = rlasso(y[train.sample] ~ x[train.sample,], post = T)
print(post.lasso.reg, all = F)
yhat.postlasso.new = predict(post.lasso.reg, newdata=x[-train.sample,])
error.postlasso <- mean((yhat.postlasso.new - wage1.test[, "lwage"] )^2)
error.postlasso
summary(post.lasso.reg)

# penalización dependiente de los datos (Post-LASSO)
post.lasso.reg.dep = rlasso(y[train.sample] ~ x[train.sample,], post = T, X.dependent.lambda = T)
print(post.lasso.reg.dep, all = F)
yhat.postlasso.new = predict(post.lasso.reg.dep, newdata=x[-train.sample,])
error.postlasso.dep <- mean((yhat.postlasso.new - wage1.test[, "lwage"] )^2)
error.postlasso.dep
summary(post.lasso.reg.dep)


## 1p LASSO riguroso y ajustes Post-LASSO con inferencia ## -------------------

# Rlasso con inferencia y penalización independiente de los datos
summary(lasso.reg)
x.nuevo <- x[, c(1, 2, 3, 5, 6, 8, 11, 15, 16, 18, 20)]
lasso.effect = rlassoEffects(x = x.nuevo[train.sample,], y = y[train.sample])
summary(lasso.effect)
x.nuevo <- x.nuevo[, -c(2, 5)]
lasso.effect = rlassoEffects(x = x.nuevo[train.sample,], y = y[train.sample])
summary(lasso.effect)

lasso.reg.2 = rlasso(y[train.sample]~x.nuevo[train.sample,], post = F)
yhat.lasso.new.2 = predict(lasso.reg.2, newdata = x.nuevo[-train.sample,])
error.rlasso.2 <- mean((yhat.lasso.new.2 - wage1.test[, "lwage"] )^2)
error.rlasso.2

# Post-LASSO con inferencia y penalización independiente de los datos
post.lasso.reg.2 = rlasso(y[train.sample] ~ x.nuevo[train.sample,], post = T)
summary(post.lasso.reg.2, all = F) 

yhat.postlasso.new.2 = predict(post.lasso.reg.2, newdata=x.nuevo[-train.sample,])
error.postlasso.2 <- mean((yhat.postlasso.new.2 - wage1.test[, "lwage"] )^2)
error.postlasso.2

# Rlasso con inferencia y penalización dependiente de los datos
summary(lasso.reg.dep)
x.nuevo <- x[, c(1, 2, 3, 5, 6, 8, 11, 15, 16, 18, 20)]
lasso.effect.dep = rlassoEffects(x = x.nuevo[train.sample,], y = y[train.sample], 
                                 X.dependent.lambda = T)
summary(lasso.effect.dep)
x.nuevo <- x.nuevo[, -c(2,5)]
lasso.effect.dep = rlassoEffects(x = x.nuevo[train.sample,], y = y[train.sample])
summary(lasso.effect.dep)

lasso.reg.2.dep = rlasso(y[train.sample] ~ x.nuevo[train.sample,], post = F)
yhat.lasso.new.2.dep = predict(lasso.reg.2.dep, newdata=x.nuevo[-train.sample,])
error.rlasso.2.dep <- mean((yhat.lasso.new.2-wage1.test[, "lwage"] )^2)
error.rlasso.2.dep

# Post-LASSO con inferencia y penalización independiente de los datos
post.lasso.reg.2.dep = rlasso(y[train.sample] ~ x.nuevo[train.sample,], post = T, 
                              X.dependent.lambda = T)
summary(post.lasso.reg.2.dep, all = F)

yhat.postlasso.new.2.dep = predict(post.lasso.reg.2.dep, newdata=x.nuevo[-train.sample,])
error.postlasso.2.dep <- mean((yhat.postlasso.new.2.dep - wage1.test[, "lwage"] )^2)
error.postlasso.2.dep

## 1q Resumen 2 ## ------------------------------------------------------------

resume.1 <- matrix(c(error.ridge.10, error.lasso.10, error.pred.en.10, error.ridge.2.10, error.lasso.2.10, error.pred.en.2.10,
                     error.ridge.5, error.lasso.5, error.pred.en.5, error.ridge.2.5, error.lasso.2.5, error.pred.en.2.5),
             nrow = 2, ncol = 6, byrow = T)
dimnames(resume.1) <- list(c("MSE.VC10","MSE.VC5"), 
                           c("Ridge", "LASSO", "Red elástica", "Ridge c/ inferencia", "LASSO c/ inferencia", "Red elástica c/ inferencia"))


resume.2 <- matrix(c(error.pcr.10, error.pls.10, error.pls.codo.10, "-",
                     error.pcr.5, error.pls.5, error.pls.codo.5, "-",
                     "-", "-", "-", error.pls.perm), nrow = 3, ncol = 4, byrow = T)
dimnames(resume.2) <- list(c("MSE.VC10", "MSE.VC5", "MSE.LOO"), 
                           c("CP, regla del codo", "PLS, mínimo", "PLS, regla del codo", "PLS, regla de la permutación"))

resume.3 <- matrix(c(error.rlasso, error.postlasso, error.rlasso.2, error.postlasso.2,
                     error.rlasso.dep, error.postlasso.dep, error.rlasso.2.dep, error.postlasso.2.dep), 
                   nrow = 2, ncol = 4, byrow = T)
dimnames(resume.3) <- list(c("Independiente de los datos", "Dependiente de los datos"), 
                           c("LASSO robusto", "LASSO robusto ajustado", "LASSO robusto con inferencia", "LASSO robusto ajustado con inferencia"))


A
resume.1
resume.2
resume.3

## 1r Coeficientes y p-valores del mejor modelo ## ----------------------------
# El mejor modelo es Red Elástica por VC 10 veces.
# Es necesario Bootstrap para obtener coeficientes y p-valores insesgados

x.b <- x[train.sample, -c(12)]
B <- 1000
betas <- matrix(0, B, ncol(x.b) + 1)
lambdas <- rep(0, B)
n.x <- nrow(x[train.sample,])
for (i in 1:B){
  mi.muestra <- sample(nrow(x[train.sample,]), size = n.x, replace = T)
  x.b <- x[mi.muestra, -c(12)]
  y.b <- y[mi.muestra]
  mejmodelo <- glmnet(x.b, y.b, alpha = alpha.en.10, lambda = lambda.en.10)
  coefs <- coef(mejmodelo, s = lambda.en.10)
  betas[i,] <- coefs[,1]
}

x.b <- x[train.sample, -c(12)]
media.betas <- apply(betas, 2, mean) # para obtener los betas.
sd.betas <- apply(betas, 2, sd) # para obtener desviaciones típicas de los betas
t.scores <- c()
p.values <- c()
for (i in 1:length(media.betas)){ # se almacenan los t-scores y p-valores en vectores
  t = (media.betas[i] / sd.betas[i])
  t.scores <- c(t.scores, t)
  p = 2*pt(-abs(t), df = nrow(x.b) - 1)
  p.values <- c(p.values, p)
}

# Se crea la tabla
resume_table <- data.frame(Estimate = round(media.betas, 7), 
                           Std.Error = round(sd.betas, 7),
                           t_value = round(t.scores, 4), 
                           p_values = p.values)
varnames = c("(Intercept)", colnames(x.b))
rownames(resume_table) = varnames
resume_table