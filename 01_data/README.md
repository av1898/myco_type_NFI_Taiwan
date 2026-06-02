data_NFI_Taiwan.csv: processed plot-level data obtained from raw data

name of variables, definition and units in brackets

-   plot_id: unique identifier for each plot
-   longitude: plot longitude
-   latitude: plot latitude
-   basal_area: sum of basal area of all trees in the plot (m2 ha-1)
-   density: number of trees per unit area (trees ha-1)
-   mean_dbh: mean diameter at breast height (dbh) of all trees in the plot (cm)
-   max_dbh: maximum dbh in the plot (cm)
-   cv_dbh: coefficient of variation of dbh in the plot
-   plot_area: area of each plot (ha)
-   elevation: elevation of each plot (m a.s.l.)
-   mean_dbh_large: mean dbh of the largest 100 trees ha-1 in the plot (cm)
-   total_richness: number of species in the plot (no species)
-   total_simpson: species evenness in the plot
-   AM: proportion of AM-associated species in the plot
-   EM: proportion of EM-associated species in the plot
-   cmi_mean: climatological mean climate moisture index of each plot (Kg m-2 mt-1)
-   bio11_tmin: climatological mean minimum temperature of each plot (ºC)
-   soil_ph_0_200: soil pH average values weighted across the six soil depth levels
-   soil_n_0_200: soil nitrogen average values weighted across the six soil depth levels

richness_model.RDS: model of tree species richness on EM dominance, stand development, and mean minimum temperature 
simpson_model.RDS: model of tree species eveness on EM dominance, stand development, and mean minimum temperature  