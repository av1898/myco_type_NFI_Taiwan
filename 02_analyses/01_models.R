## Libraries
library(tidyverse)
library(plotly)
library(GGally)
library(corrplot)
library(viridis)
library(mgcv)
library(gratia)
library(DHARMa)
library(visreg)

## Dataset for analyses
data_models <- read_csv('01_data/data_NFI_Taiwan.csv')
summary(data_models)

## Percentage of plots with cumulative sum of EM + AM >= 0.9
sum_EM_AM <- data_models |>
  select(AM, EM) |>
  mutate(sum_EMAM = EM + AM) |>
  summarise(n_plots = n(),
            n = sum(sum_EMAM >= 0.90),
            p = n / n_plots)

sum_EM_AM

plot(data_models$AM, data_models$EM)

## Distribution of variables
data_models_hist <- data_models |>
  select(EM, basal_area, density, mean_dbh, max_dbh, mean_dbh_large, cv_dbh, elevation,
         cmi_mean, bio11_tmin, soil_ph_0_200, soil_n_0_200, plot_area, 
         total_richness, total_simpson)

data_models_hist_long <- data_models_hist |>
  pivot_longer(colnames(data_models_hist))

head(data_models_hist_long)

ggplot(data_models_hist_long, aes(x = value)) +    
  geom_density() + 
  facet_wrap(~ name, scales = "free")

## Boxplots
ggplot(data_models_hist_long, aes(y = value, x = name)) +    
  geom_boxplot(outlier.colour = "red") + 
  facet_wrap(~ name, scales = "free")

## Forest structure
structure_data <- data_models |>
  select(basal_area, density, mean_dbh, max_dbh, mean_dbh_large, cv_dbh)

pca_structure <- prcomp(~ log(basal_area) + log(density) + log(max_dbh) + log(mean_dbh) +
                          log(mean_dbh_large) + cv_dbh, center = TRUE, scale. = TRUE, 
                        data = structure_data)

print(pca_structure)

summary(pca_structure)

biplot(pca_structure)

## Climate
climate_data <- data_models |>
  select(cmi_mean, bio11_tmin)

pca_climate <- prcomp(climate_data,center = TRUE, scale. = TRUE)

print(pca_climate)

summary(pca_climate)

biplot(pca_climate)

## Covariation
correl_data <- data_models |>
  select(EM, bio11_tmin, soil_ph_0_200, soil_n_0_200, density, mean_dbh_large, 
         plot_area, total_richness, total_simpson)

ggpairs(correl_data, title = "correlogram") 

## GAM fitting and disgnostics

## Richness model
model_r <- gam(
  total_richness ~  te(EM, mean_dbh_large, bio11_tmin, bs = c("cr", "cr", "cr"), 
                       k = c(6, 6, 6)) + s(longitude, latitude) + log(density) + log(plot_area),
  family = nb(link = "log"), method = "REML", data = data_models)

summary(model_r)

par(mfrow = c(2,2))

gam.check(model_r)

performance::r2(model_r)

## visreg
visreg(model_r)

## gratia
gratia::appraise(model_r)

## calibration plot
pred <- predict(model_r, newdata = data_models, type = "response")

ggplot(data.frame(obs = data_models$total_richness, pred = pred)) +
  geom_point(aes(pred, obs)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_smooth(aes(pred, obs))

## Residuals
res_model_r <- simulateResiduals(model_r, integerResponse = TRUE)

plot(res_model_r)
plot(res_model_r, form = data_models$EM)
plot(res_model_r, form = data_models$bio11_tmin)
plot(res_model_r, form = data_models$mean_dbh_large)
plot(res_model_r, form = data_models$density)
plot(res_model_r, form = data_models$plot_area)
plot(res_model_r, form = data_models$longitude)
plot(res_model_r, form = data_models$latitude)

plotQQunif(res_model_r)

testOutliers(res_model_r, type = "bootstrap")

## Residual spatial correlation
testSpatialAutocorrelation(simulationOutput = res_model_r, x = data_models$longitude,
                           y= data_models$latitude)

## Save model
saveRDS(model_r, "01_data/richness_model.RDS")

## Simpson model
model_s <- gam(
  total_simpson ~  te(EM, mean_dbh_large, bio11_tmin, bs = c("cr", "cr", "cr"), 
                       k = c(6, 6, 6)) + s(longitude, latitude) + log(density) + log(plot_area),
  family = Gamma(link = "log"), method = "REML", data = data_models)

summary(model_s)

par(mfrow = c(2,2))

gam.check(model_s)

performance::r2(model_s)

## visreg
visreg(model_s)

## gratia
gratia::appraise(model_s)

## calibration plot
pred <- predict(model_s, newdata = data_models, type = "response")

ggplot(data.frame(obs = data_models$total_simpson, pred = pred)) +
  geom_point(aes(pred, obs)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_smooth(aes(pred, obs))

## Residuals
res_model_s <- simulateResiduals(model_s)

plot(res_model_s)
plot(res_model_s, form = data_models$EM)
plot(res_model_s, form = data_models$bio11_tmin)
plot(res_model_s, form = data_models$mean_dbh_large)
plot(res_model_s, form = data_models$density)
plot(res_model_s, form = data_models$plot_area)
plot(res_model_s, form = data_models$longitude)
plot(res_model_s, form = data_models$latitude)

plotQQunif(res_model_s)

testOutliers(res_model_s, type = "bootstrap")

## Residual spatial correlation
testSpatialAutocorrelation(simulationOutput = res_model_s, x = data_models$longitude,
                           y= data_models$latitude)

## Save model
saveRDS(model_s, "01_data/simpson_model.RDS")
