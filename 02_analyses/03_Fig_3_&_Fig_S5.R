## Figure 3
  
## Libraries
library(tidyverse)
library(gratia)
library(viridis)
library(grafify)
library(gridExtra)
library(ggpubr)
library(patchwork)
library(ggdist)
library(here)
library(mgcv)

richness_model <- read_rds("01_data/richness_model.RDS")

data_models <- read_csv('01_data/data_NFI_Taiwan.csv')

## top panel
sm <- smooth_estimates(richness_model, dist = 0.1, select = "bio11_tmin", partial_match = TRUE)

quantile(data_models$bio11_tmin)

sm_climate <- sm |>
  mutate(
    climate = case_when(bio11_tmin < 7.35 ~  "Cold climate",
                        bio11_tmin >= 14.35 ~ "Warm climate", 
                        T ~ "Median climate"
    ))

data_models_climate <- data_models |>
  mutate(
    climate = case_when(bio11_tmin < 7.35 ~  "Cold climate",
                        bio11_tmin >= 14.35 ~ "Warm climate", 
                        T ~ "Median climate"
    ))

sm_climate_g <- sm_climate |>
  group_by(EM, mean_dbh_large, climate) |>
  summarise(est_g = median(.estimate)) |>
  ungroup()

pattern <- ggplot(sm_climate_g, aes(x = EM, y = mean_dbh_large, group = climate)) +
  geom_tile(aes(fill = est_g)) +
  geom_point(data = data_models_climate, aes(x = EM, y = mean_dbh_large, group = climate), alpha = 0.2) + 
  geom_contour(aes(z = est_g), colour = "black", alpha = 0.5, binwidth = 0.1) +
  facet_wrap(climate ~ .) + labs(
    y = "Mean Dbh larger trees (cm)", x = "EM proportion") + scale_fill_gradient2(
      mid = "white",
      midpoint = 0,
      low = "#2166AC",
      high = "#B2182B",  
      name = "Partial effect on\nlog-woody species richness") +
  scale_x_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
  theme(
    plot.title = element_text(color = "black", size = 16),
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.5),
    panel.grid.minor = element_line(colour = "grey90", linewidth = 0.5),
    panel.background = element_blank(),
    legend.position = "right",
    legend.title = element_text(color = "black", size = 16),
    legend.text = element_text(color = "black", size = 12),
    axis.title.y = element_text(color = "black", size = 16),
    axis.title.x = element_text(color = "black", size = 16),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    strip.text = element_text(color = "black", size = 16)
  ) + ggtitle('A')

## bottom panel
tmin_q <- quantile(data_models$bio11_tmin, c(0.25, 0.5, 0.75))

# Cold climate
data_cold_climate <- data_models |>
  filter(bio11_tmin < 7.35)

max_dbh_q <- quantile(data_cold_climate$mean_dbh_large, c(0.05, 0.5, 0.95))
EM_q <- quantile(data_cold_climate$EM, c(0.05, 0.5, 0.95))
tmin_q <- quantile(data_cold_climate$bio11_tmin, c(0.25, 0.5, 0.75))

data_models1 <- data_models

# Predictions

# late

data_model_EM_1_late_c <- data_models1 |> 
  mutate(
    EM = 0.002,
    mean_dbh_large = 96,
    bio11_tmin = 4.85
    
  )

data_model_EM_2_late_c <- data_models1 |> 
  mutate(
    EM = 0.5,
    mean_dbh_large = 96,
    bio11_tmin = 4.85
  )

data_model_EM_3_late_c <- data_models1 |> 
  mutate(
    EM = 1,
    mean_dbh_large = 96,
    bio11_tmin = 4.85
  )

# median

data_model_EM_1_median_c <- data_models1 |> 
  mutate(
    EM = 0.002,
    mean_dbh_large = 53.8,
    bio11_tmin = 4.85
    
  )

data_model_EM_2_median_c <- data_models1 |> 
  mutate(
    EM = 0.5,
    mean_dbh_large = 53.8,
    bio11_tmin = 4.85
  )

data_model_EM_3_median_c <- data_models1 |> 
  mutate(
    EM = 1,
    mean_dbh_large = 53.8,
    bio11_tmin = 4.85
  )

# early

data_model_EM_1_early_c <- data_models1 |> 
  mutate(
    EM = 0.002,
    mean_dbh_large = 22.84,
    bio11_tmin = 4.85
    
  )

data_model_EM_2_early_c <- data_models1 |> 
  mutate(
    EM = 0.5,
    mean_dbh_large = 22.84,
    bio11_tmin = 4.85
  )

data_model_EM_3_early_c <- data_models1 |> 
  mutate(
    EM = 1,
    mean_dbh_large = 22.84,
    bio11_tmin = 4.85
  )


newdata_list_c <- list(
  data_models1,
  data_model_EM_1_late_c,
  data_model_EM_2_late_c,
  data_model_EM_3_late_c,
  data_model_EM_1_median_c,
  data_model_EM_2_median_c,
  data_model_EM_3_median_c,
  data_model_EM_1_early_c,
  data_model_EM_2_early_c,
  data_model_EM_3_early_c
)

# make the predictions for each dataset
predictions_c <- map(newdata_list_c, \(x) predict(
  object = richness_model, newdata = x, se.fit = TRUE, type = "response"
))

arg2_dat_pred_c <- list(dat = newdata_list_c, pred = predictions_c)

# add predictions to the corresponding dataset
add_predictions_c <- function(dat, pred){
  dat |> 
    mutate(
      predictions_c = pred$fit
    )
}

newdata_list_predictions_c <- arg2_dat_pred_c |>
  pmap(add_predictions_c)

names(newdata_list_predictions_c) <- c(
  "data_models1",
  "data_model_EM_1_late_c",
  "data_model_EM_2_late_c",
  "data_model_EM_3_late_c",
  "data_model_EM_1_median_c",
  "data_model_EM_2_median_c",
  "data_model_EM_3_median_c",
  "data_model_EM_1_early_c",
  "data_model_EM_2_early_c",
  "data_model_EM_3_early_c"
)

#View(newdata_list_predictions_c)

# select the variables of interest from each dataset
predictions_selected_c <- map(
  newdata_list_predictions_c,
  \(x) dplyr::select(x, plot_id, predictions_c, total_richness))

# add prediction name to each dataset
mutate_name_c <- function(x, names_df){
  predictions_selected_c[[x]] |> 
    dplyr::mutate(
      df = names_df
    )
}

arg2_x_names_df_c <- list(x = 1:length(predictions_selected_c),
                        names_df = names(predictions_selected_c))

predictions_selected_c <- arg2_x_names_df_c |>
  pmap_df(mutate_name_c)

rename_predictions_c <- function(dat, pred_name){
  predictions_selected_c |> 
    filter(df == dat) |> 
    rename({{ pred_name }} := predictions_c)
}

dat_v_c <- c(
  "data_models1",
  "data_model_EM_1_late_c",
  "data_model_EM_2_late_c",
  "data_model_EM_3_late_c",
  "data_model_EM_1_median_c",
  "data_model_EM_2_median_c",
  "data_model_EM_3_median_c",
  "data_model_EM_1_early_c",
  "data_model_EM_2_early_c",
  "data_model_EM_3_early_c"
)

pred_name_v_c <- c(
  "data_models1",
  "pred_EM_1_late_c",
  "pred_EM_2_late_c",
  "pred_EM_3_late_c",
  "pred_EM_1_median_c",
  "pred_EM_2_median_c",
  "pred_EM_3_median_c",
  "pred_EM_1_early_c",
  "pred_EM_2_early_c",
  "pred_EM_3_early_c"
)

arg2_dat_pred_v_c <- list(
  dat = dat_v_c,
  pred_name = pred_name_v_c
)

predictions_selected_r_c <- arg2_dat_pred_v_c |>
  pmap(rename_predictions_c)

predictions_all_c <- predictions_selected_r_c |>
  reduce(left_join, by = "plot_id")

# get the difference between EM levels in each stand development stage 
predictions_effects_c <- predictions_all_c |> 
  mutate(
    effect_EM_AM_late = pred_EM_3_late_c - pred_EM_1_late_c,
    effect_EM_MIX_late = pred_EM_3_late_c - pred_EM_2_late_c,
    effect_MIX_AM_late = pred_EM_2_late_c - pred_EM_1_late_c,
    effect_EM_AM_median = pred_EM_3_median_c - pred_EM_1_median_c,
    effect_EM_MIX_median = pred_EM_3_median_c - pred_EM_2_median_c,
    effect_MIX_AM_median = pred_EM_2_median_c - pred_EM_1_median_c,
    effect_EM_AM_early = pred_EM_3_early_c - pred_EM_1_early_c,
    effect_EM_MIX_early = pred_EM_3_early_c - pred_EM_2_early_c,
    effect_MIX_AM_early = pred_EM_2_early_c - pred_EM_1_early_c
  )

pred_effects_l_c <- predictions_effects_c |>
  pivot_longer(
    cols = c(
      effect_EM_AM_late,
      effect_EM_MIX_late,
      effect_MIX_AM_late,
      effect_EM_AM_median,
      effect_EM_MIX_median,
      effect_MIX_AM_median,
      effect_EM_AM_early,
      effect_EM_MIX_early,
      effect_MIX_AM_early
    ),
    names_to = "name",
    values_to = "value"
  )

## evaluate predictions on the observed data
predictions_filtered_test_c <- predictions_selected_c |>
  filter(df == "data_models1") |>
  dplyr::select(plot_id, total_richness, predictions_c)

eval_pred_c <- ggplot(predictions_filtered_test_c, aes(
  predictions_c, total_richness)) + geom_point(alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, col = "red")

## plot the effect of EM in young, mid and mature on sp richness
pred_effects_ll_c <- pred_effects_l_c |>
  mutate(std = case_when(name == "effect_MIX_AM_early" | name == "effect_EM_AM_early" | 
                           name == "effect_EM_MIX_early" ~ "Early", 
                         name == "effect_MIX_AM_median" | name == "effect_EM_AM_median" | 
                           name == "effect_EM_MIX_median" ~ "Median", 
                         name == "effect_MIX_AM_late" | name == "effect_EM_AM_late" | 
                           name == "effect_EM_MIX_late" ~ "Late"))

pred_effects_ll_c$std <- factor(pred_effects_ll_c$std, levels=c("Early", "Median", "Late"))

pred_comparison_c <- ggplot(pred_effects_ll_c, aes
                               (x = name, y = value,
                                 fill = std, color = std)) +
  stat_pointinterval(
    point_interval = median_qi,
    position = position_dodge(width = c(0.66, 0.95)),
    alpha = .9) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  ylab("Change in woody species richness") + scale_y_continuous(breaks = c(-8, -6, -4, -2, 0, 2, 4, 6, 8)) +
  scale_x_discrete(labels = c(expression("EM vs. AM"), expression("EM vs. Mixed"),
                              expression("Mixed vs. AM"))) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  facet_grid(vars(std), scales = "free") +
  ggtitle("Cold climate") +
  theme(
    plot.title = element_text(color = "black", size = 14),
    plot.subtitle = element_text(color = "black", size = 8),
    legend.position = "none",
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.5),
    panel.background = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(color = "black", size = 14),
    axis.text.x = element_text(color = "black", size = 10),
    axis.text.y = element_text(color = "black", size = 10),
    strip.text = element_text(color = "black", size = 14)
  ) 


# Plot continuous prediction for better interpretation
# We take the median of each dataset in "predictions_all_c" and obtain 3 vectors,
# one for each stand dev stage and plot a line Prediction - EM

median_pred_c <- predictions_all_c |>
  summarise(median_AM_late = median(pred_EM_1_late_c),
            median_MIX_late = median(pred_EM_2_late_c),
            median_EM_late = median(pred_EM_3_late_c),
            median_AM_median = median(pred_EM_1_median_c),
            median_MIX_median = median(pred_EM_2_median_c),
            median_EM_median = median(pred_EM_3_median_c),
            median_AM_early = median(pred_EM_1_early_c),
            median_MIX_early = median(pred_EM_2_early_c),
            median_EM_early = median(pred_EM_3_early_c))

median_pred_l_c <- median_pred_c |>
  pivot_longer(
    cols = c(
      median_AM_late,
      median_MIX_late,
      median_EM_late,
      median_AM_median,
      median_MIX_median,
      median_EM_median,
      median_AM_early,
      median_MIX_early,
      median_EM_early
    ),
    names_to = "name",
    values_to = "value"
  )

median_pred_ll_c <- median_pred_l_c |>
  mutate(EM = case_when(name == "median_AM_late" | name == "median_AM_median" | 
                          name == "median_AM_early" ~ 0.002,
                        name == "median_MIX_late" | name == "median_MIX_median" | 
                          name == "median_MIX_early" ~ 0.5,
                        name == "median_EM_late" | name == "median_EM_median" | 
                          name == "median_EM_early" ~ 1), 
         std = case_when(name == "median_AM_early" | name == "median_MIX_early" | 
                           name == "median_EM_early" ~ "Early", 
                         name == "median_AM_median" | name == "median_MIX_median" | 
                           name == "median_EM_median" ~ "Median", 
                         name == "median_AM_late" | name == "median_MIX_late" | 
                           name == "median_EM_late" ~ "Late"))

median_pred_ll_c$std <- factor(median_pred_ll_c$std, levels=c("Early", "Median", "Late"))

median_pred_ll_c

line_effect_c <- ggplot(
  median_pred_ll_c, aes(x = EM, y = value, fill = std, 
                       color = std)) +
  geom_smooth(method = "loess",linewidth = 2)  + 
  ylab("Woody species richness") + xlab("EM proportion") + 
  scale_y_continuous(limits = c(0, 18), breaks = c(0, 5, 10, 15, 20)) +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))  +
  scale_color_viridis(discrete = TRUE, option = "D") +
  theme(
    plot.title = element_text(color = "black", size = 16),
    plot.subtitle = element_text(color = "black", size = 8),
    legend.position = "none",
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.5),
    panel.background = element_blank(),
    axis.title.x = element_text(color = "black", size = 16),
    axis.title.y = element_text(color = "black", size = 16),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    strip.text = element_text(color = "black", size = 12)
  ) + ggtitle('B')

# Median climate

tmin_q <- quantile(data_models$bio11_tmin, c(0.25, 0.5, 0.75))

data_median_climate <- data_models |>
  filter(bio11_tmin >= 7.35 & bio11_tmin < 14.35)

max_dbh_q <- quantile(data_median_climate$mean_dbh_large, c(0.05, 0.5, 0.95))
EM_q <- quantile(data_median_climate$EM, c(0.05, 0.5, 0.95))
tmin_q <- quantile(data_median_climate$bio11_tmin, c(0.25, 0.5, 0.75))

data_models1 <- data_models

# Predictions

# late

data_model_EM_1_late_m <- data_models1 |> 
  mutate(
    EM = 0,
    mean_dbh_large = 74.32,
    bio11_tmin = 11.05
    
  )

data_model_EM_2_late_m <- data_models1 |> 
  mutate(
    EM = 0.5,
    mean_dbh_large = 74.32,
    bio11_tmin = 11.05
  )

data_model_EM_3_late_m <- data_models1 |> 
  mutate(
    EM = 0.81,
    mean_dbh_large = 74.32,
    bio11_tmin = 11.05
  )

# median

data_model_EM_1_median_m <- data_models1 |> 
  mutate(
    EM = 0,
    mean_dbh_large = 45.32,
    bio11_tmin = 11.05
    
  )

data_model_EM_2_median_m <- data_models1 |> 
  mutate(
    EM = 0.5,
    mean_dbh_large = 45.32,
    bio11_tmin = 11.05
  )

data_model_EM_3_median_m <- data_models1 |> 
  mutate(
    EM = 0.81,
    mean_dbh_large = 45.32,
    bio11_tmin = 11.05
  )

# early

data_model_EM_1_early_m <- data_models1 |> 
  mutate(
    EM = 0,
    mean_dbh_large = 24.21,
    bio11_tmin = 11.05
    
  )

data_model_EM_2_early_m <- data_models1 |> 
  mutate(
    EM = 0.5,
    mean_dbh_large = 24.21,
    bio11_tmin = 11.05
  )

data_model_EM_3_early_m <- data_models1 |> 
  mutate(
    EM = 0.81,
    mean_dbh_large = 24.21,
    bio11_tmin = 11.05
  )

newdata_list_m <- list(
  data_models1,
  data_model_EM_1_late_m,
  data_model_EM_2_late_m,
  data_model_EM_3_late_m,
  data_model_EM_1_median_m,
  data_model_EM_2_median_m,
  data_model_EM_3_median_m,
  data_model_EM_1_early_m,
  data_model_EM_2_early_m,
  data_model_EM_3_early_m
)

# make predictions

# make the predictions for each dataset
predictions_m <- map(newdata_list_m, \(x) predict(
  object = richness_model, newdata = x, se.fit = TRUE, type = "response"
))

arg2_dat_pred_m <- list(dat = newdata_list_m, pred = predictions_m)

# add predictions to the corresponding dataset
add_predictions_m <- function(dat, pred){
  dat |> 
    mutate(
      predictions_m = pred$fit
    )
}

newdata_list_predictions_m <- arg2_dat_pred_m |>
  pmap(add_predictions_m)

names(newdata_list_predictions_m) <- c(
  "data_models1",
  "data_model_EM_1_late_m",
  "data_model_EM_2_late_m",
  "data_model_EM_3_late_m",
  "data_model_EM_1_median_m",
  "data_model_EM_2_median_m",
  "data_model_EM_3_median_m",
  "data_model_EM_1_early_m",
  "data_model_EM_2_early_m",
  "data_model_EM_3_early_m"
)

#View(newdata_list_predictions_m)

# select the variables of interest from each dataset
predictions_selected_m <- map(
  newdata_list_predictions_m,
  \(x) dplyr::select(x, plot_id, predictions_m, total_richness))

# add prediction name to each dataset
mutate_name_m <- function(x, names_df){
  predictions_selected_m[[x]] |> 
    dplyr::mutate(
      df = names_df
    )
}

arg2_x_names_df_m <- list(x = 1:length(predictions_selected_m),
                        names_df = names(predictions_selected_m))

predictions_selected_m <- arg2_x_names_df_m |>
  pmap_df(mutate_name_m)

rename_predictions_m <- function(dat, pred_name){
  predictions_selected_m |> 
    filter(df == dat) |> 
    rename({{ pred_name }} := predictions_m)
}

dat_v_m <- c(
  "data_models1",
  "data_model_EM_1_late_m",
  "data_model_EM_2_late_m",
  "data_model_EM_3_late_m",
  "data_model_EM_1_median_m",
  "data_model_EM_2_median_m",
  "data_model_EM_3_median_m",
  "data_model_EM_1_early_m",
  "data_model_EM_2_early_m",
  "data_model_EM_3_early_m"
)

pred_name_v_m <- c(
  "data_models1",
  "pred_EM_1_late_m",
  "pred_EM_2_late_m",
  "pred_EM_3_late_m",
  "pred_EM_1_median_m",
  "pred_EM_2_median_m",
  "pred_EM_3_median_m",
  "pred_EM_1_early_m",
  "pred_EM_2_early_m",
  "pred_EM_3_early_m"
)

arg2_dat_pred_v_m <- list(
  dat = dat_v_m,
  pred_name = pred_name_v_m
)

predictions_selected_r_m <- arg2_dat_pred_v_m |>
  pmap(rename_predictions_m)

#View(predictions_selected_r_m)

predictions_all_m <- predictions_selected_r_m |>
  reduce(left_join, by = "plot_id")

# get the difference between of EM in young, # mid & mature

predictions_effects_m <- predictions_all_m |> 
  mutate(
    effect_EM_AM_late = pred_EM_3_late_m - pred_EM_1_late_m,
    effect_EM_MIX_late = pred_EM_3_late_m - pred_EM_2_late_m,
    effect_MIX_AM_late = pred_EM_2_late_m - pred_EM_1_late_m,
    effect_EM_AM_median = pred_EM_3_median_m - pred_EM_1_median_m,
    effect_EM_MIX_median = pred_EM_3_median_m - pred_EM_2_median_m,
    effect_MIX_AM_median = pred_EM_2_median_m - pred_EM_1_median_m,
    effect_EM_AM_early = pred_EM_3_early_m - pred_EM_1_early_m,
    effect_EM_MIX_early = pred_EM_3_early_m - pred_EM_2_early_m,
    effect_MIX_AM_early = pred_EM_2_early_m - pred_EM_1_early_m
  )

pred_effects_l_m <- predictions_effects_m |>
  pivot_longer(
    cols = c(
      effect_EM_AM_late,
      effect_EM_MIX_late,
      effect_MIX_AM_late,
      effect_EM_AM_median,
      effect_EM_MIX_median,
      effect_MIX_AM_median,
      effect_EM_AM_early,
      effect_EM_MIX_early,
      effect_MIX_AM_early
    ),
    names_to = "name",
    values_to = "value"
  )

## plot the effect of EM in young, mid and late in species richness
pred_effects_ll_m <- pred_effects_l_m |>
  mutate(std = case_when(name == "effect_MIX_AM_early" | name == "effect_EM_AM_early" | 
                           name == "effect_EM_MIX_early" ~ "Early", 
                         name == "effect_MIX_AM_median" | name == "effect_EM_AM_median" | 
                           name == "effect_EM_MIX_median" ~ "Median", 
                         name == "effect_MIX_AM_late" | name == "effect_EM_AM_late" | 
                           name == "effect_EM_MIX_late" ~ "Late"))

pred_effects_ll_m$std <- factor(pred_effects_ll_m$std, levels=c("Early", "Median", "Late"))

pred_comparison_m <- ggplot(pred_effects_ll_m, aes
                                 (x = name, y = value,
                                   fill = std, color = std)) +
  stat_pointinterval(
    point_interval = median_qi,
    position = position_dodge(width = c(0.66, 0.95)),
    alpha = .9) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  ylab("Change in woody species richness") + scale_y_continuous(breaks = c(-8, -6, -4, -2, 0, 2, 4, 6, 8)) +
  scale_x_discrete(labels = c(expression("EM vs. AM"), expression("EM vs. Mixed"),
                              expression("Mixed vs. AM"))) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  facet_grid(vars(std), scales = "free") +
  ggtitle("Median climate") +
  theme(
    plot.title = element_text(color = "black", size = 14),
    plot.subtitle = element_text(color = "black", size = 8),
    legend.position = "none",
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.5),
    panel.background = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(color = "black", size = 14),
    axis.text.x = element_text(color = "black", size = 10),
    axis.text.y = element_text(color = "black", size = 10),
    strip.text = element_text(color = "black", size = 14)
  ) 

# Plot continuous prediction for better interpretation
# We take the median of each dataset in "predictions_all_m" and obtain 3 vectors,
# one for each stand dev stage and plot a line Prediction - EM

median_pred_m <- predictions_all_m |>
  summarise(median_AM_late = median(pred_EM_1_late_m),
            median_MIX_late = median(pred_EM_2_late_m),
            median_EM_late = median(pred_EM_3_late_m),
            median_AM_median = median(pred_EM_1_median_m),
            median_MIX_median = median(pred_EM_2_median_m),
            median_EM_median = median(pred_EM_3_median_m),
            median_AM_early = median(pred_EM_1_early_m),
            median_MIX_early = median(pred_EM_2_early_m),
            median_EM_early = median(pred_EM_3_early_m))

median_pred_l_m <- median_pred_m |>
  pivot_longer(
    cols = c(
      median_AM_late,
      median_MIX_late,
      median_EM_late,
      median_AM_median,
      median_MIX_median,
      median_EM_median,
      median_AM_early,
      median_MIX_early,
      median_EM_early
    ),
    names_to = "name",
    values_to = "value"
  )

median_pred_ll_m <- median_pred_l_m |>
  mutate(EM = case_when(name == "median_AM_late" | name == "median_AM_median" | 
                          name == "median_AM_early" ~ 0,
                        name == "median_MIX_late" | name == "median_MIX_median" | 
                          name == "median_MIX_early" ~ 0.5,
                        name == "median_EM_late" | name == "median_EM_median" | 
                          name == "median_EM_early" ~ 0.81), 
         std = case_when(name == "median_AM_early" | name == "median_MIX_early" | 
                           name == "median_EM_early" ~ "Early", 
                         name == "median_AM_median" | name == "median_MIX_median" | 
                           name == "median_EM_median" ~ "Median", 
                         name == "median_AM_late" | name == "median_MIX_late" | 
                           name == "median_EM_late" ~ "Late"))

median_pred_ll_m$std <- factor(median_pred_ll_m$std, levels=c("Early", "Median", "Late"))

median_pred_ll_m

line_effect_m <- ggplot(
  median_pred_ll_m, aes (x = EM, y = value, fill = std, 
                       color = std)) +
  geom_smooth(method = "loess",linewidth = 2)  + 
  ylab("Woody species richness") + xlab("EM proportion") + 
  scale_y_continuous(limits = c(0, 18), breaks = c(0, 5, 10, 15, 20)) +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))  +
  scale_color_viridis(discrete = TRUE, option = "D") +
  theme(
    plot.title = element_text(color = "black", size = 10),
    plot.subtitle = element_text(color = "black", size = 8),
    legend.position = "none",
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.5),
    panel.background = element_blank(),
    axis.title.x = element_text(color = "black", size = 16),
    axis.title.y = element_blank(),
    axis.text.x = element_text(color = "black", size = 12),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank())

# Warm climate

tmin_q <- quantile(data_models$bio11_tmin, c(0.25, 0.5, 0.75))

data_warm_climate <- data_models |>
  filter(bio11_tmin >= 14.35)

max_dbh_q <- quantile(data_warm_climate$mean_dbh_large, c(0.05, 0.5, 0.95))
EM_q <- quantile(data_warm_climate$EM, c(0.05, 0.5, 0.95))
tmin_q <- quantile(data_warm_climate$bio11_tmin, c(0.25, 0.5, 0.75))

data_models1 <- data_models

# Predictions

# late

data_model_EM_1_late_w <- data_models1 |> 
  mutate(
    EM = 0,
    mean_dbh_large = 57.23,
    bio11_tmin = 16.2
    
  )

data_model_EM_2_late_w <- data_models1 |> 
  mutate(
    EM = 0.5,
    mean_dbh_large = 57.23,
    bio11_tmin = 16.2
  )

data_model_EM_3_late_w <- data_models1 |> 
  mutate(
    EM = 0.62,
    mean_dbh_large = 57.23,
    bio11_tmin = 16.2
  )

# median

data_model_EM_1_median_w <- data_models1 |> 
  mutate(
    EM = 0,
    mean_dbh_large = 32.8,
    bio11_tmin = 16.2
    
  )

data_model_EM_2_median_w <- data_models1 |> 
  mutate(
    EM = 0.5,
    mean_dbh_large = 32.8,
    bio11_tmin = 16.2
  )

data_model_EM_3_median_w <- data_models1 |> 
  mutate(
    EM = 0.62,
    mean_dbh_large = 32.8,
    bio11_tmin = 16.2
  )

# early

data_model_EM_1_early_w <- data_models1 |> 
  mutate(
    EM = 0,
    mean_dbh_large = 15.81,
    bio11_tmin = 16.2
    
  )

data_model_EM_2_early_w <- data_models1 |> 
  mutate(
    EM = 0.5,
    mean_dbh_large = 15.81,
    bio11_tmin = 16.2
  )

data_model_EM_3_early_w <- data_models1 |> 
  mutate(
    EM = 0.62,
    mean_dbh_large = 15.81,
    bio11_tmin = 16.2
  )


newdata_list_w <- list(
  data_models1,
  data_model_EM_1_late_w,
  data_model_EM_2_late_w,
  data_model_EM_3_late_w,
  data_model_EM_1_median_w,
  data_model_EM_2_median_w,
  data_model_EM_3_median_w,
  data_model_EM_1_early_w,
  data_model_EM_2_early_w,
  data_model_EM_3_early_w
)

# make predictions

# make the predictions for each dataset
predictions_w <- map(newdata_list_w, \(x) predict(
  object = richness_model, newdata = x, se.fit = TRUE, type = "response"
))

arg2_dat_pred_w <- list(dat = newdata_list_w, pred = predictions_w)

# add predictions to the corresponding dataset
add_predictions_w <- function(dat, pred){
  dat |> 
    mutate(
      predictions_w = pred$fit
    )
}

newdata_list_predictions_w <- arg2_dat_pred_w |>
  pmap(add_predictions_w)

names(newdata_list_predictions_w) <- c(
  "data_models1",
  "data_model_EM_1_late_w",
  "data_model_EM_2_late_w",
  "data_model_EM_3_late_w",
  "data_model_EM_1_median_w",
  "data_model_EM_2_median_w",
  "data_model_EM_3_median_w",
  "data_model_EM_1_early_w",
  "data_model_EM_2_early_w",
  "data_model_EM_3_early_w"
)

#View(newdata_list_predictions_w)

# select the variables of interest from each dataset
predictions_selected_w <- map(
  newdata_list_predictions_w,
  \(x) dplyr::select(x, plot_id, predictions_w, total_richness))

# add prediction name to each dataset
mutate_name_w <- function(x, names_df){
  predictions_selected_w[[x]] |> 
    dplyr::mutate(
      df = names_df
    )
}

arg2_x_names_df_w <- list(x = 1:length(predictions_selected_w),
                        names_df = names(predictions_selected_w))

predictions_selected_w <- arg2_x_names_df_w |>
  pmap_df(mutate_name_w)

rename_predictions_w <- function(dat, pred_name){
  predictions_selected_w |> 
    filter(df == dat) |> 
    rename({{ pred_name }} := predictions_w)
}

dat_v_w <- c(
  "data_models1",
  "data_model_EM_1_late_w",
  "data_model_EM_2_late_w",
  "data_model_EM_3_late_w",
  "data_model_EM_1_median_w",
  "data_model_EM_2_median_w",
  "data_model_EM_3_median_w",
  "data_model_EM_1_early_w",
  "data_model_EM_2_early_w",
  "data_model_EM_3_early_w"
)

pred_name_v_w <- c(
  "data_models1",
  "pred_EM_1_late_w",
  "pred_EM_2_late_w",
  "pred_EM_3_late_w",
  "pred_EM_1_median_w",
  "pred_EM_2_median_w",
  "pred_EM_3_median_w",
  "pred_EM_1_early_w",
  "pred_EM_2_early_w",
  "pred_EM_3_early_w"
)

arg2_dat_pred_v_w <- list(
  dat = dat_v_w,
  pred_name = pred_name_v_w
)

predictions_selected_r_w <- arg2_dat_pred_v_w |>
  pmap(rename_predictions_w)

#View(predictions_selected_r_w)

predictions_all_w <- predictions_selected_r_w |>
  reduce(left_join, by = "plot_id")

# get the difference between of EM in young, mid & mature
predictions_effects_w <- predictions_all_w |> 
  mutate(
    effect_EM_AM_late = pred_EM_3_late_w - pred_EM_1_late_w,
    effect_EM_MIX_late = pred_EM_3_late_w - pred_EM_2_late_w,
    effect_MIX_AM_late = pred_EM_2_late_w - pred_EM_1_late_w,
    effect_EM_AM_median = pred_EM_3_median_w - pred_EM_1_median_w,
    effect_EM_MIX_median = pred_EM_3_median_w - pred_EM_2_median_w,
    effect_MIX_AM_median = pred_EM_2_median_w - pred_EM_1_median_w,
    effect_EM_AM_early = pred_EM_3_early_w - pred_EM_1_early_w,
    effect_EM_MIX_early = pred_EM_3_early_w - pred_EM_2_early_w,
    effect_MIX_AM_early = pred_EM_2_early_w - pred_EM_1_early_w
  )

pred_effects_l_w <- predictions_effects_w |>
  pivot_longer(
    cols = c(
      effect_EM_AM_late,
      effect_EM_MIX_late,
      effect_MIX_AM_late,
      effect_EM_AM_median,
      effect_EM_MIX_median,
      effect_MIX_AM_median,
      effect_EM_AM_early,
      effect_EM_MIX_early,
      effect_MIX_AM_early
    ),
    names_to = "name",
    values_to = "value"
  )

## plot the effect of EM in young, mid and late in species richness
pred_effects_ll_w <- pred_effects_l_w |>
  mutate(std = case_when(name == "effect_MIX_AM_early" | name == "effect_EM_AM_early" | 
                           name == "effect_EM_MIX_early" ~ "Early", 
                         name == "effect_MIX_AM_median" | name == "effect_EM_AM_median" | 
                           name == "effect_EM_MIX_median" ~ "Median", 
                         name == "effect_MIX_AM_late" | name == "effect_EM_AM_late" | 
                           name == "effect_EM_MIX_late" ~ "Late"))

pred_effects_ll_w$std <- factor(pred_effects_ll_w$std, levels=c("Early", "Median", "Late"))

pred_comparison_w <- ggplot(pred_effects_ll_w, aes
                               (x = name, y = value,
                                 color = std)) +
  stat_pointinterval(
    point_interval = median_qi,
    position = position_dodge(width = c(0.66, 0.95)),
    alpha = .9) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  ylab("Change in woody species richness") + scale_y_continuous(breaks = c(-8, -6, -4, -2, 0, 2, 4, 6, 8)) +
  scale_x_discrete(labels = c(expression("EM vs. AM"), expression("EM vs. Mixed"),
                              expression("Mixed vs. AM"))) +
  scale_color_viridis(discrete = TRUE, option = "D", name = "Stand development") +
  #scale_fill_viridis(discrete = TRUE, option = "D") +
  facet_grid(vars(std), scales = "free") +
  ggtitle("Warm climate") +
  theme(
    plot.title = element_text(color = "black", size = 14),
    plot.subtitle = element_text(color = "black", size = 8),
    legend.position = "right",
    legend.title = element_text(color = "black", size = 16),
    legend.text = element_text(color = "black", size = 12),
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.5),
    panel.background = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(color = "black", size = 14),
    axis.text.x = element_text(color = "black", size = 10),
    axis.text.y = element_text(color = "black", size = 10),
    strip.text = element_text(color = "black", size = 14)
  ) 

# Plot continuous prediction for better interpretation
# We take the median of each dataset in "predictions_all_w" and obtain 3 vectors,
# one for each stand dev stage and plot a line Prediction - EM

median_pred_w <- predictions_all_w |>
  summarise(median_AM_late = median(pred_EM_1_late_w),
            median_MIX_late = median(pred_EM_2_late_w),
            median_EM_late = median(pred_EM_3_late_w),
            median_AM_median = median(pred_EM_1_median_w),
            median_MIX_median = median(pred_EM_2_median_w),
            median_EM_median = median(pred_EM_3_median_w),
            median_AM_early = median(pred_EM_1_early_w),
            median_MIX_early = median(pred_EM_2_early_w),
            median_EM_early = median(pred_EM_3_early_w))

median_pred_l_w <- median_pred_w |>
  pivot_longer(
    cols = c(
      median_AM_late,
      median_MIX_late,
      median_EM_late,
      median_AM_median,
      median_MIX_median,
      median_EM_median,
      median_AM_early,
      median_MIX_early,
      median_EM_early
    ),
    names_to = "name",
    values_to = "value"
  )

median_pred_ll_w <- median_pred_l_w |>
  mutate(EM = case_when(name == "median_AM_late" | name == "median_AM_median" | 
                          name == "median_AM_early" ~ 0,
                        name == "median_MIX_late" | name == "median_MIX_median" | 
                          name == "median_MIX_early" ~ 0.5,
                        name == "median_EM_late" | name == "median_EM_median" | 
                          name == "median_EM_early" ~ 0.62), 
         std = case_when(name == "median_AM_early" | name == "median_MIX_early" | 
                           name == "median_EM_early" ~ "Early", 
                         name == "median_AM_median" | name == "median_MIX_median" | 
                           name == "median_EM_median" ~ "Median", 
                         name == "median_AM_late" | name == "median_MIX_late" | 
                           name == "median_EM_late" ~ "Late"))

median_pred_ll_w$std <- factor(median_pred_ll_w$std, levels=c("Early", "Median", "Late"))

median_pred_ll_w

line_effect_w <- ggplot(
  median_pred_ll_w, aes (x = EM, y = value, 
                       color = std)) +
  geom_smooth(method = "loess",linewidth = 2)  + 
  ylab("Woody species richness") + xlab("EM proportion") + 
  scale_y_continuous(limits = c(0, 18), breaks = c(0, 5, 10, 15, 20)) +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1))  +
  scale_color_viridis(discrete = TRUE, option = "D", name = "Stand development") +
  theme(
    plot.title = element_text(color = "black", size = 10),
    plot.subtitle = element_text(color = "black", size = 8),
    legend.position = "right",
    legend.title = element_text(color = "black", size = 16),
    legend.text = element_text(color = "black", size = 12),
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.5),
    panel.background = element_blank(),
    axis.title.x = element_text(color = "black", size = 16),
    axis.title.y = element_blank(),
    axis.text.x = element_text(color = "black", size = 12),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank())


design <- "AAAAAA
           BBCCDD"

Fig_3 <- pattern + line_effect_c + line_effect_m + line_effect_w + plot_layout(design = design)

ggsave(
  plot = Fig_3,
  here("03_results", "Fig_3.png"),
  width = 18, height = 10,
  dpi = 600
)

comp <- pred_comparison_c + pred_comparison_m + pred_comparison_w

ggsave(
  plot = comp,
  here("03_results", "Fig_S5.png"),
  width = 18, height = 10,
  dpi = 600
)
