library(glmtrans)
set.seed(1)

# Here I suppose there are 100 sources of data but only 5 are transferable sources
# Generate synthetic data
D_training <- models(family = "poisson", type = "all", cov.type = 2, Ka = 5,
                     K = 100, s = 10, n.target = 100, n.source = rep(100, 100))
# see ?models for more details and the paper for the explanation of the parameters


library(glmnet)

# Fit the model
# simple lasso (no transferability)
fit_lasso <- cv.glmnet(x = D_training$target$x, y = D_training$target$y,
                       family = "poisson")
# fit the model using all sources of data 
fit_pooled <- glmtrans(target = D_training$target, source = D_training$source,
                       family = "poisson", transfer.source.id = "all", cores = 4)
# fit the model using only the transferable sources (selected by an algo)
# that's the one we are interested in
fit_detection <- glmtrans(target = D_training$target, source = D_training$source,
                          family = "poisson", transfer.source.id = "auto", cores = 4)

# plot detection
plot(fit_detection)

# fit the model using the first 5 sources of data, ideal situation
fit_oracle <- glmtrans(target = D_training$target, source = D_training$source,
                       family = "poisson", transfer.source.id = 1:5, cores = 4)

# Compare the models with RMSE
beta <- c(0, rep(0.5, 10), rep(0, 500 - 10))
er <- numeric(4)
names(er) <- c("Lasso", "Pooled-Trans-GLM", "Trans-GLM", "Oracle-Trans-GLM")
er["Lasso"] <- sqrt(sum((coef(fit_lasso) - beta)^2))
er["Pooled-Trans-GLM"] <- sqrt(sum((fit_pooled$beta - beta)^2))
er["Trans-GLM"] <- sqrt(sum((fit_detection$beta - beta)^2))
er["Oracle-Trans-GLM"] <- sqrt(sum((fit_oracle$beta - beta)^2))
er
# The RMSE of Trans-GLM is the smallest


