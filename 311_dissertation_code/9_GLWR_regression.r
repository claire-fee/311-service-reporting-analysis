# ================================
# Beyond the Squeaky Wheel: 311 Engagement & Equity Analysis
# Notebook 9: Engagement Score Binomial GLWR and Supplementary Regression Analysis
# ================================


# Runs the binomial Geographically Weighted Logistic Regression (GLWR) for one city
# Also runs supplementary SAR, SEM, and SLX spatial regression checks
# Produces 21 CSV files and 22 maps and 2 graphs as PDF files


#### INSTRUCTIONS ####
# CTRL+F for "austin" and replace-all with appropriate city name
    # lower case letters and use under-scores if multiple words
# once directory and file links are updated hit "Source" to run entire notebook




# ================================
# SECTION 1: Script Preparation
# ================================

# step 1: activate library packages

library("sf")
library("tmap")
library("spdep")
library("dplyr")
library("ggplot2")
library("spatialreg")
library("GWmodel")  # for binary GWR model
library("MuMIn")    # for AIC global
library("broom")    # for stats table exports 

library("sp")
library("spgwr")

library("tmaptools")
library("RColorBrewer")
library("cols4all")




# ================================
# PRE-WORK: Exploratory Data Analysis (Notebook 8)
# ================================




# ================================
# PRE-WORK: Set Standard Map Frame and PDF Export Formatting
# ================================

# step 0.A: configure page
MAP_EXPORT <- list(
  page_width_in     = 8.5,
  page_height_in    = 11,
  content_width_in  = 8,
  content_height_in = 10,
  compass_position  = c("right", "top"),
  scalebar_position = c("left", "top"),
  legend_position    = c("right", "bottom"),  # inside the map frame
  legend_bg_alpha    = 0.75,                  # slight transparency so map still reads underneath
  legend_text_size   = 0.6,
  legend_title_size  = 0.8)


# step 0.B: set details and combine final page format
finalize_map <- function(tm_obj, filename, orientation = "portrait", cfg = MAP_EXPORT) {
  if (orientation == "portrait") {
    w <- cfg$page_width_in;  h <- cfg$page_height_in
  } else {
    w <- cfg$page_height_in; h <- cfg$page_width_in
  }
  margin_x <- (w - cfg$content_width_in)  / 2 / w
  margin_y <- (h - cfg$content_height_in) / 2 / h
  final_map <- tm_obj +
    tm_compass(position = cfg$compass_position) +
    tm_scalebar(position = cfg$scalebar_position) +
    tm_layout(frame = FALSE,
              outer.margins = c(margin_y, margin_x, margin_y, margin_x),
              legend.outside = FALSE,
              legend.position = cfg$legend_position,
              legend.bg.color = "white",
              legend.bg.alpha = cfg$legend_bg_alpha,
              legend.text.size = cfg$legend_text_size,
              legend.title.size = cfg$legend_title_size)
  tmap_save(final_map, filename = filename, width = w, height = h, units = "in")}


# step 0.C: set chart export format
CHART_EXPORT <- list(
  page_width_in  = 8.5,
  page_height_in = 11,
  content_width_in  = 6,
  content_height_in = 6)

finalize_chart <- function(gg_obj, filename, cfg = CHART_EXPORT) {
  ggsave(filename = filename,
         plot = gg_obj,
         width = cfg$content_width_in,
         height = cfg$content_height_in,
         units = "in")}

# step 0.D: have option to make plotted map interactive

# tmap_mode("view") 
# tmap_mode("plot")




# ================================
# SECTION 2: Upload Data and Identify City
# ================================


# step 2: set working directory
setwd("INSERT FILE FOLDER PATH")


# step 3: upload prepared census tracts with full data
cen_tracts <- st_read("cen_tracts3_prepped.gpkg")
tract_scores <- read.csv("stats_per_tract.csv", colClasses = c(GEOID = "character"))

cen_tracts_scores <- cen_tracts %>%
  left_join(tract_scores, by = "GEOID")


# step 4: import reference layers
city_limits <- st_read("final_cities_2.gpkg")
water <- st_read("final_water.gpkg") 
roads <- st_read("final_major_roads.gpkg")
rails <- st_read("final_national_rails.gpkg")
parks <- st_read("final_parks_clipped.gpkg")

# step 5: select city 
        # (CTRL+F replacement for entire program)
austin <- cen_tracts %>%
  filter(city == "austin")

austin_city <- city_limits %>%
  filter(NAME == "austin")

# step 6: compute and set bounding box for city
austin_bbox <- st_bbox(austin_city)

# step 7: clip layers to the bounding box
tract_scores_clip   <- st_crop(cen_tracts_scores, austin_bbox)
water_clip   <- st_crop(water, austin_bbox)
roads_clip   <- st_crop(roads, austin_bbox)
rails_clip   <- st_crop(rails, austin_bbox)
parks_clip   <- st_crop(parks, austin_bbox)




# ================================
# SECTION 3: Export Initial Maps
# ================================


# step 8A: plot reference base map
austin_base <- tm_shape(tract_scores_clip, austin_bbox, unit = "mi") +
  tm_polygons(fill = "#f6f0e5", lwd = 0.12, col = "white") +
  tm_shape(parks_clip)+
  tm_polygons("#c5e4cd",  lwd = 0.1, col = "darkgreen") +
  tm_shape(water_clip) +
  tm_polygons("#cce9ff", lwd = 0.1, col = "navy") +
  tm_shape(austin_city) +
  tm_polygons(col = "black", fill_alpha = 0, lwd = 1.5) +
  tm_shape(rails_clip) +
  tm_lines(lwd = 0.4, col = "black", lty = "dashed") +
  tm_shape(roads_clip) +
  tm_lines(lwd = 0.2, col = "#6d6e70")    #"#58595b"
finalize_map(austin_base, "0_austin_basemap.pdf")


# step 8B: plot 311 SRI scores
austin_311 <- tm_shape(tract_scores_clip, austin_bbox, unit = "mi") +
  tm_polygons(fill = "X311_SRI_score",
              fill.scale = tm_scale_continuous(limits = c(0, 100), values = "brewer.yl_gn"),
              lwd = 0.1, col = "lightgray",
              fill.legend = tm_legend(frame = FALSE, title = "311 SRI Scores")) +
  tm_shape(water_clip) +
  tm_polygons("#efefef", lwd = 0.1, col = "gray60") +
  tm_shape(austin_city) +
  tm_polygons(col = "black", fill_alpha = 0, lwd = 1.5)
finalize_map(austin_311, "1_austin_311.pdf")


# step 8C: plot SNI scores
austin_SNI <- tm_shape(tract_scores_clip, austin_bbox, unit = "mi") +
  tm_polygons(fill = "SNI_score",
              fill.scale = tm_scale_continuous(limits = c(0, 100), values = "brewer.bu_pu"),
              lwd = 0.1, col = "lightgray",
              fill.legend = tm_legend(frame = FALSE, title = "SNI Scores")) +
  tm_shape(water_clip) +
  tm_polygons("#efefef", lwd = 0.1, col = "gray50") +
  tm_shape(austin_city) +
  tm_polygons(col = "black", fill_alpha = 0, lwd = 1.5)
finalize_map(austin_SNI, "2_austin_SNI.pdf")


# step 8D: plot ES scores
austin_ES <- tm_shape(tract_scores_clip, austin_bbox, unit = "mi") +
  tm_polygons(fill = "ES_score",
              fill.scale = tm_scale_continuous(limits = c(-100, 100), midpoint = 0, values = "brewer.rd_yl_bu"),
              lwd = 0.1, col = "lightgray",
              fill.legend = tm_legend(frame = FALSE, title = "Engagement Scores")) +
  tm_shape(water_clip) +
  tm_polygons("#efefef", lwd = 0.1, col = "gray60") +
  tm_shape(austin_city) +
  tm_polygons(col = "black", fill_alpha = 0, lwd = 1.5)
finalize_map(austin_ES, "3_austin_ES.pdf")


# step 8E: plot binomial distribution
austin_binomial <- tm_shape(austin, austin_bbox, unit = "mi") +
  tm_polygons(fill = "SQRT_score",
              fill.scale = tm_scale_categorical(values = c("blue", "red")),
              fill.legend = tm_legend(title = "Engagement\n(0 = Over, 1 = Under)"),
              lwd = 0.1, col = "lightgray") +
  tm_shape(water_clip) +
    tm_polygons("#efefef", lwd = 0.1, col = "gray60") +
  tm_shape(city_limits) +
    tm_polygons(col = "black", fill_alpha = 0, lwd = 1.5)
finalize_map(austin_binomial, "4_austin_binomial.pdf")


# step 8F: plot variables - median HH income
austin_var_medHH <- tm_shape(austin, austin_bbox, unit = "mi") +
  tm_polygons(fill = "median_hh_income",
              fill.scale = tm_scale_intervals(style = "quantile", n = 5, values = "brewer.greens"),
              lwd = 0.1, col = "lightgray",
              fill.legend = tm_legend(frame = FALSE, title = "Median Household Income")) +
  tm_shape(water_clip) +
    tm_polygons("#efefef", lwd = 0.1, col = "gray60") +
  tm_shape(city_limits) +
    tm_polygons(col = "black", fill_alpha = 0, lwd = 1.5)
finalize_map(austin_var_medHH, "5_austin_var_medHH.pdf")


# step 8G: plot variables - population density
austin_var_popD <- tm_shape(austin, austin_bbox, unit = "mi") +
  tm_polygons(fill = "pop_density_sqmi",
              fill.scale = tm_scale_intervals(style = "quantile", n = 5, values = "brewer.oranges"),
              lwd = 0.1, col = "lightgray",
              fill.legend = tm_legend(frame = FALSE, title = "Population Density")) +
tm_shape(water_clip) +
  tm_polygons("#efefef", lwd = 0.1, col = "gray60") +
  tm_shape(city_limits) +
  tm_polygons(col = "black", fill_alpha = 0, lwd = 1.5)
finalize_map(austin_var_popD, "6_austin_var_popD.pdf")

# step 8H: plot variables - percent renter
austin$renter_bucket <- cut(austin$pct_renter,
                             breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                             labels = c("0–20%", "20–40%", "40–60%", "60–80%", "80–100%"),
                             include.lowest = TRUE)

austin_var_renter <- tm_shape(austin, austin_bbox, unit = "mi") +
  tm_polygons(fill = "renter_bucket",
              fill.scale = tm_scale_categorical(values = "brewer.blues"),
              lwd = 0.1, col = "lightgray",
              fill.legend = tm_legend(frame = FALSE, title = "Percent Rental Housing")) +
  tm_shape(water_clip) +
  tm_polygons("#efefef", lwd = 0.1, col = "gray60") +
  tm_shape(city_limits) +
  tm_polygons(col = "black", fill_alpha = 0, lwd = 1.5)
finalize_map(austin_var_renter, "7_austin_var_renter.pdf")


# step 8I: plot variables - percent age 65+
austin$sixtyfive_bucket <- cut(austin$pct_65_plus, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                                labels = c("0–20%", "20–40%", "40–60%", "60–80%", "80–100%"),
                                include.lowest = TRUE)

austin_var_65 <- tm_shape(austin, austin_bbox, unit = "mi") +
  tm_polygons(fill = "sixtyfive_bucket",
              fill.scale = tm_scale_categorical(values = "brewer.purples"),
              lwd = 0.1, col = "lightgray",
              fill.legend = tm_legend(frame = FALSE, title = "Percent Age 65+")) +
tm_shape(water_clip) +
  tm_polygons("#efefef", lwd = 0.1, col = "gray60") +
  tm_shape(city_limits) +
  tm_polygons(col = "black", fill_alpha = 0, lwd = 1.5)
finalize_map(austin_var_65, "8_austin_var_65.pdf")


# step 8J: plot variables - percent limited english
austin$english_bucket <- cut(austin$pct_limited_english_pop, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                              labels = c("0–20%", "20–40%", "40–60%", "60–80%", "80–100%"),
                              include.lowest = TRUE)

austin_var_english <- tm_shape(austin, austin_bbox, unit = "mi") +
  tm_polygons(fill = "english_bucket",
              fill.scale = tm_scale_categorical(values = "carto.brwn_yl"),
              lwd = 0.1, col = "lightgray",
              fill.legend = tm_legend(frame = FALSE, title = "Percent Limited English")) +
tm_shape(water_clip) +
  tm_polygons("#efefef", lwd = 0.1, col = "gray60") +
  tm_shape(city_limits) +
  tm_polygons(col = "black", fill_alpha = 0, lwd = 1.5)
finalize_map(austin_var_english, "9_austin_var_english.pdf")


# step 8K: plot variables - percent non-white
austin$race_bucket <- cut(austin$pct_non_white, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                           labels = c("0–20%", "20–40%", "40–60%", "60–80%", "80–100%"),
                           include.lowest = TRUE)

austin_var_race <- tm_shape(austin, austin_bbox, unit = "mi") +
  tm_polygons(fill = "race_bucket",
              fill.scale = tm_scale_categorical(values = "brewer.reds"),
              lwd = 0.1, col = "lightgray",
              fill.legend = tm_legend(frame = FALSE, title = "Percent Non-White")) +
tm_shape(water_clip) +
  tm_polygons("#efefef", lwd = 0.1, col = "gray60") +
  tm_shape(city_limits) +
  tm_polygons(col = "black", fill_alpha = 0, lwd = 1.5)
finalize_map(austin_var_race, "10_austin_var_race.pdf")




# ================================
# SECTION 4: Run Model for Individual City (full re-run for each city)
# ================================


# step 9: review statistics for each variable and export results
vars_to_summarize <- c("median_hh_income", "pop_density_sqmi", "pct_renter",
                       "pct_limited_english_pop", "pct_65_plus", "pct_non_white")

austin_var_summary <- data.frame(
  variable = vars_to_summarize,
  min      = sapply(vars_to_summarize, function(v) min(austin[[v]], na.rm = TRUE)),
  mean     = sapply(vars_to_summarize, function(v) mean(austin[[v]], na.rm = TRUE)),
  median   = sapply(vars_to_summarize, function(v) median(austin[[v]], na.rm = TRUE)),
  max      = sapply(vars_to_summarize, function(v) max(austin[[v]], na.rm = TRUE)),
  sd       = sapply(vars_to_summarize, function(v) sd(austin[[v]], na.rm = TRUE)))
write.csv(austin_var_summary, file = "1_austin_var_summary.csv", row.names = FALSE)

# histograms do not export, option to add function if desired

    # visualize distribution for each variable
    # only run histograms for target cities
hist(austin$median_hh_income,
     main = "Median HH Income Histogram")

hist(austin$pop_density_sqmi,
     main = "Population Density Histogram")

hist(austin$pct_renter,
     main = "Percent Renters Histogram")

hist(austin$pct_limited_english_pop,
     main = "Percent Limited English Speaking Histogram")

hist(austin$pct_65_plus,
     main = "Percent 65+ Histogram")

hist(austin$pct_non_white,
     main = "Percent Non-White")




# ================================
# SECTION 5: Moran's I and Local Moran's I Tests
# ================================


# step 10: convert polygons into an adjacency matrix to get spatial weights
    # Review islands from test plot if there is a "no neighbours" warning"
austin_nb <- poly2nb(austin, queen = TRUE, snap = 10)


    # check adjacency matrix for islands
summary(austin_nb)


# step 11: convert adjacency matrix into spatial weights
  # use zero.policy for islands
austin_weights <- nb2listw(austin_nb, style = "W", zero.policy = TRUE)


# step 12: run Global Moran's I test and export results
austin_moran <- moran.test(austin$SQRT_score,
  austin_weights,
  zero.policy = TRUE)

austin_moran_summary <- data.frame(
  moran_I    = austin_moran$estimate[["Moran I statistic"]],
  expected_I = austin_moran$estimate[["Expectation"]],
  variance   = austin_moran$estimate[["Variance"]],
  p_value    = austin_moran$p.value)
write.csv(austin_moran_summary, file = "2_austin_moran_summary.csv", row.names = FALSE)


# step 13: run the Local Moran's I test
austin_local_moran <- localmoran(austin$SQRT_score, austin_weights, zero.policy = TRUE)


    # standardize the dependent variable
austin$scale_SQRT_score <- scale(austin$SQRT_score)


    # compute spatial lag of standardized variable
austin$lag_scale_SQRT_score <- lag.listw(
  austin_weights,
  austin$scale_SQRT_score,
  zero.policy = TRUE)


    # add Local Moran's I stats as columns back into baseline sf/geographic object 
austin$local_I <- austin_local_moran[, "Ii"]
austin$local_p <- austin_local_moran[, "Pr(z != E(Ii))"]


# step 14: run else/if functions to assess non-significant quadrant classifications
    # generate new column for census tracts of quadrant analysis 
austin$quad_non_sig <- ifelse(
  austin$scale_SQRT_score > 0 & austin$lag_scale_SQRT_score > 0, "High High",
  ifelse(
    austin$scale_SQRT_score < 0 & austin$lag_scale_SQRT_score < 0, "Low Low",
    ifelse(
      austin$scale_SQRT_score > 0 & austin$lag_scale_SQRT_score < 0, "High Low",
      ifelse(
        austin$scale_SQRT_score < 0 & austin$lag_scale_SQRT_score > 0, "Low High", NA
      )
    )
  )
)


    # check distribution of non-significant quadrant analysis results
table(austin$quad_non_sig)


# step 15: plot the results with full categorization of quadrants WITHOUT significance check
austin_quad_non_sig <- tm_shape(austin, bbox = austin_bbox, unit = "mi") +
  tm_polygons(fill = "quad_non_sig", 
              fill.scale = tm_scale_categorical(
                values = c("#de2d26", "#fee0d2", "#deebf7", "#3182bd")),
                lwd = 0.1, col = "lightgray") +
  tm_shape(water_clip) +
    tm_polygons("#efefef", lwd = 0.1, col = "gray60") +
  tm_shape(city_limits) +
    tm_polygons(col = "black", fill_alpha = 0, lwd = 1.5)

finalize_map(austin_quad_non_sig, "11_austin_quad_non_sig.pdf")


# step 16: run else/if functions to assess significant quadrant classifications
    # set significance level
sig_level <- 0.5


# generate new column for census tracts of quadrant analysis 
austin$quad_sig <- ifelse(
  austin$scale_SQRT_score > 0 & austin$lag_scale_SQRT_score > 0 & austin$local_p <= sig_level, "High High",
  ifelse(
    austin$scale_SQRT_score < 0 & austin$lag_scale_SQRT_score < 0 & austin$local_p <= sig_level,  "Low Low",
    ifelse(
      austin$scale_SQRT_score > 0 & austin$lag_scale_SQRT_score < 0 & austin$local_p <= sig_level,  "High Low",
      ifelse(
        austin$scale_SQRT_score < 0 & austin$lag_scale_SQRT_score > 0 & austin$local_p <= sig_level,  "Low High", 
        'Not_Significant'
      )
    )
  )
)

    # check distribution of significant quadrant classification results and export
    # confirmed same results from both formats, re-assess in morning
austin_quad_table <- as.data.frame(table(austin$quad_sig))

colnames(austin_quad_table) <- c("quadrant", "count")
write.csv(austin_quad_table, file = "3_austin_quad_sig_summary.csv", row.names = FALSE)


# step 17: plot the results with full categorization of quadrants WITH significance check
austin_quad_sig <- tm_shape(austin, bbox = austin_bbox, unit = "mi") +
  tm_polygons(fill = "quad_sig",
              fill.scale = tm_scale_categorical(
              values = c("#de2d26", "#fee0d2", "#deebf7", "#3182bd", "white")),
              lwd = 0.1, col = "lightgray") +
  tm_shape(water_clip) +
    tm_polygons("#efefef", lwd = 0.1, col = "gray60") +
  tm_shape(city_limits) +
    tm_polygons(col = "black", fill_alpha = 0, lwd = 1.5)

finalize_map(austin_quad_sig, "12_austin_quad_sig.pdf")


# step 18: plot the results with full categorization of quadrants WITHOUT significance check
    # color code dots by quadrant
quad_non_sig_plot <- ggplot(austin, aes(x = scale_SQRT_score,
                    y = lag_scale_SQRT_score,
                    color = quad_non_sig)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  xlab("Standardized Engagement Score") +
  ylab("Lagged Standardized Engagement Score") +
  labs(colour = "Rel. to Nghbrs W/O Sig") +
  geom_point()

finalize_chart(quad_non_sig_plot, "austin_quad_NS_plt.pdf")


# step 19: plot the results with full categorization of quadrants WITH significance check
    # color code dots by quadrant
quad_sig_plot <- ggplot(austin, aes(x = scale_SQRT_score,
                                     y = lag_scale_SQRT_score,
                                     color = quad_sig)) +
  geom_vline(xintercept = 0) + # plot vertical line
  geom_hline(yintercept = 0) + # plot horizontal line
  xlab('Standardized Engagement Score') +
  ylab('Lagged Standardized Engagement Score') +
  labs(colour = 'Rel. to Nghbrs W/ Sig') +
  geom_point()

finalize_chart(quad_sig_plot, "austin_quad_sig_plt.pdf")




# ================================
# SECTION 6: Geographic Local Weighted Regressions
# ================================


# step 20: data and layer preparation for GLWR
    # create clean base layer
austin_gwr <- austin
    # create clean polygon geometry
austin_gwrPoly <- austin
    #test plot and sanity check of new shapefile
tm_shape(austin_gwrPoly) + 
  tm_polygons()


# step 21: extract centroids of census tracts polygons for GLWR analysis
austin_gwr <- st_centroid(austin_gwr)
austin_gwr <- cbind(austin_gwr, st_coordinates(austin_gwr))
    #test plot and sanity check of new shapefile
tm_shape(austin_gwr) + 
  tm_dots()


# step 22: find the optimal bandwidth
    # utilize factor of +1 for datasets with 0 values
    # lowest score is the optimum bandwidth 

DM <- gw.dist(dp.locat = st_coordinates(austin_gwr))

austin_bndwth_BB <- bw.ggwr(SQRT_score ~ log10(median_hh_income) + log10(pop_density_sqmi) +
                              log10(pct_renter + 1) + log10(pct_65_plus + 1) +
                              log10(pct_limited_english_pop + 1) + log10(pct_non_white + 1),
                            data = austin_gwr,
                            family = "binomial",
                            approach = "AIC",
                            adaptive = TRUE,
                            dMat = DM)
    # print and check optimal bandwidth
austin_bndwth_BB


# step 23: fit the GLWR to the adaptive bandwidth
    # maytake time, leave R-Studio alone while model runs
    # run timer with model

    # start timer to track GLWR run time
start.timer <- proc.time()
    # gwr() model with hatmatrix and se.fit TRUE to test statistical significance (show standard deviation) 
    # adapted per Claude
austin_model <- ggwr.basic(SQRT_score ~ log10(median_hh_income) + log10(pop_density_sqmi) +
                               log10(pct_renter + 1) + log10(pct_65_plus + 1) +
                               log10(pct_limited_english_pop + 1) + log10(pct_non_white + 1),
                             bw = austin_bndwth_BB,
                             data = austin_gwr,
                             family = "binomial",
                             adaptive = TRUE,
                             dMat = DM)
    # end timer and total run/churn time
end.timer <- proc.time() - start.timer
    # report time taken
end.timer

    # print and check the results
austin_model

austin_diag <- as.data.frame(austin_model$GW.diagnostic)
austin_diag$bandwidth <- austin_bndwth_BB
write.csv(austin_diag, file = "4_austin_model_diagnostics.csv", row.names = FALSE)


# step 24: save and export the results
    # can skip running model again
gwr.data <- as.data.frame(austin_model$SDF)
write.csv(gwr.data, file = "5_austin_GWR_output.csv", row.names = FALSE)


# step 25: AIC global-GWR diagnostic
global_model <- glm(SQRT_score ~ log10(median_hh_income) + log10(pop_density_sqmi) +
                      log10(pct_renter + 1) + log10(pct_65_plus + 1) +
                      log10(pct_limited_english_pop + 1) + log10(pct_non_white + 1),
                    data = austin_gwr,
                    family = "binomial")
AIC(global_model)
austin_model$GW.diagnostic$AICc

    # export model & diagnostic resul
austin_aic_comparison <- data.frame(
  global_aic = AIC(global_model),
  gwr_aicc   = austin_model$GW.diagnostic$AICc,
  delta_aicc = AIC(global_model) - austin_model$GW.diagnostic$AICc)
write.csv(austin_aic_comparison, file = "6_austin_global_GWR_comp.csv", row.names = FALSE)


# step 26: create clean spatial data frame to link with GLWR results
austin_results <- st_drop_geometry(austin_gwr[, c("GEOID")])

    # confirm results and check columns
colnames(gwr.data)

    # review for 0 values
    # log10(x + 1) precedent addresses all percentages, empty income and density values should already be removed
vars_to_check <- c("median_hh_income", "pop_density_sqmi", "pct_renter", "pct_65_plus", "pct_limited_english_pop", "pct_non_white")
sapply(austin_gwr[vars_to_check], function(x) sum(x == 0, na.rm = TRUE))


# step 27: add GLWR results back into clean dataframe

# coefficients
austin_results$CoefLogIncome   <- gwr.data[["log10(median_hh_income)"]]
austin_results$CoefLogDensity  <- gwr.data[["log10(pop_density_sqmi)"]]
austin_results$CoefLogRenter   <- gwr.data[["log10(pct_renter + 1)"]]
austin_results$CoefLog65       <- gwr.data[["log10(pct_65_plus + 1)"]]
austin_results$CoefLogLanguage <- gwr.data[["log10(pct_limited_english_pop + 1)"]]
austin_results$CoefLogRace     <- gwr.data[["log10(pct_non_white + 1)"]]

# standard errors
austin_results$SELogIncome   <- gwr.data[["log10(median_hh_income)_SE"]]
austin_results$SELogDensity  <- gwr.data[["log10(pop_density_sqmi)_SE"]]
austin_results$SELogRenter   <- gwr.data[["log10(pct_renter + 1)_SE"]]
austin_results$SELog65       <- gwr.data[["log10(pct_65_plus + 1)_SE"]]
austin_results$SELogLanguage <- gwr.data[["log10(pct_limited_english_pop + 1)_SE"]]
austin_results$SELogRace     <- gwr.data[["log10(pct_non_white + 1)_SE"]]


# step 28: rejoin GLWR model results to geometric layer
austin_results_gwr <- merge(austin_gwrPoly, austin_results, by.x = "GEOID", by.y = "GEOID")

    # confirm results and check columns
colnames(austin_results_gwr)


# step 29: check statistics for each coefficient variable
coeff_vars <- c("CoefLogIncome", "CoefLogDensity", "CoefLogRenter",
                "CoefLog65", "CoefLogLanguage", "CoefLogRace")

austin_coeff_summary <- data.frame(
  variable = coeff_vars,
  min      = sapply(coeff_vars, function(v) min(austin_results_gwr[[v]], na.rm = TRUE)),
  mean     = sapply(coeff_vars, function(v) mean(austin_results_gwr[[v]], na.rm = TRUE)),
  median   = sapply(coeff_vars, function(v) median(austin_results_gwr[[v]], na.rm = TRUE)),
  max      = sapply(coeff_vars, function(v) max(austin_results_gwr[[v]], na.rm = TRUE)),
  sd       = sapply(coeff_vars, function(v) sd(austin_results_gwr[[v]], na.rm = TRUE)))

write.csv(austin_coeff_summary, file = "7_austin_coeff_summary.csv", row.names = FALSE)


# step 30: plot the results of GLWR coefficients for each variable
    # keep mid-point in plots


# step 30A: plot median_hh_income coefficient
austin_med_income_coeff <- tm_shape(austin_results_gwr, bbox = austin_bbox, unit = "mi") + 
  tm_polygons(fill = "CoefLogIncome", 
              fill.scale = tm_scale_continuous(midpoint = 0, values = "brewer.rd_bu"), lwd = 0,
              fill.legend = tm_legend(frame = FALSE, title="Coef: Log(Med HH Income)")) +
  tm_shape(water_clip) +
    tm_polygons("#efefef", lwd = 0.1, col = "gray60") +
  tm_shape(city_limits) +
    tm_polygons(col = "black", fill_alpha = 0, lwd = 1.5)
finalize_map(austin_med_income_coeff, "13_austin_med_income_coeff.pdf")


# step 30B: plot pop_density_sqmi coefficient
austin_density_coeff <- tm_shape(austin_results_gwr, bbox = austin_bbox, unit = "mi") + 
  tm_polygons(fill = "CoefLogDensity", 
              fill.scale = tm_scale_continuous(midpoint = 0, values = "brewer.rd_bu"), lwd = 0,
              fill.legend = tm_legend(frame = FALSE, title="Coef: Log(Pop Density)")) +
  tm_shape(water_clip) +
    tm_polygons("#efefef", lwd = 0.1, col = "gray60") +
  tm_shape(city_limits) +
    tm_polygons(col = "black", fill_alpha = 0, lwd = 1.5)
finalize_map(austin_density_coeff, "14_austin_density_coeff.pdf")


# step 30C: plot pct_renter coefficient
austin_renter_coeff <- tm_shape(austin_results_gwr, bbox = austin_bbox, unit = "mi") + 
  tm_polygons(fill = "CoefLogRenter", 
              fill.scale = tm_scale_continuous(midpoint = 0, values = "brewer.rd_bu"), lwd = 0,
              fill.legend = tm_legend(frame = FALSE, title="Coef: Log(Pct Renter)")) +
  tm_shape(water_clip) +
    tm_polygons("#efefef", lwd = 0.1, col = "gray60") +
  tm_shape(city_limits) +
    tm_polygons(col = "black", fill_alpha = 0, lwd = 1.5)
finalize_map(austin_renter_coeff, "15_austin_renter_coeff.pdf")


# step 30D: plot pct_65_plus coefficient
austin_65_coeff <- tm_shape(austin_results_gwr, bbox = austin_bbox, unit = "mi") + 
  tm_polygons(fill = "CoefLog65", 
              fill.scale = tm_scale_continuous(midpoint = 0, values = "brewer.rd_bu"), lwd = 0,
              fill.legend = tm_legend(frame = FALSE, title="Coef: Log(Pct 65+)")) +
  tm_shape(water_clip) +
    tm_polygons("#efefef", lwd = 0.1, col = "gray60") +
  tm_shape(city_limits) +
    tm_polygons(col = "black", fill_alpha = 0, lwd = 1.5)
finalize_map(austin_65_coeff, "16_austin_65_coeff.pdf")


# step 30E: plot pct_limited_english_pop coefficient
austin_language_coeff <- tm_shape(austin_results_gwr, bbox = austin_bbox, unit = "mi") + 
  tm_polygons(fill = "CoefLogLanguage", 
              fill.scale = tm_scale_continuous(midpoint = 0, values = "brewer.rd_bu"), lwd = 0,
              fill.legend = tm_legend(frame = FALSE, title="Coef: Log(Pct Non-English)")) +
  tm_shape(water_clip) +
    tm_polygons("#efefef", lwd = 0.1, col = "gray60") +
  tm_shape(city_limits) +
    tm_polygons(col = "black", fill_alpha = 0, lwd = 1.5)
finalize_map(austin_language_coeff, "17_austin_language_coeff.pdf")


# step 30F: plot pct_non_white coefficient
austin_race_coeff <- tm_shape(austin_results_gwr, bbox = austin_bbox, unit = "mi") + 
  tm_polygons(fill = "CoefLogRace", 
              fill.scale = tm_scale_continuous(midpoint = 0, values = "brewer.rd_bu"), lwd = 0,
              fill.legend = tm_legend(frame = FALSE, title="Coef: Log(Pct Non-White)")) +
  tm_shape(water_clip) +
    tm_polygons("#efefef", lwd = 0.1, col = "gray60") +
  tm_shape(city_limits) +
    tm_polygons(col = "black", fill_alpha = 0, lwd = 1.5)
finalize_map(austin_race_coeff, "18_austin_race_coeff.pdf")


# step 31: compute estimates to determine significance per variable


# step 31A: Median HH Income significance
    # t-score
austin_results_gwr$tstatIncome <- austin_results_gwr$CoefLogIncome / austin_results_gwr$SELogIncome
    # generate 3 buckets of significance and column of results
austin_results_gwr$Income_sig <- cut(austin_results_gwr$tstatIncome,
                                   breaks=c(min(austin_results_gwr$tstatIncome), -2, 2, max(austin_results_gwr$tstatIncome)),
                                   labels=c("Reduction: Significant","Not Significant", "Increase: Significant"))
    # plot significance map 
austin_income_sig <- tm_shape(austin_results_gwr, bbox = austin_bbox, unit = "mi") + 
  tm_polygons(fill = "Income_sig", 
              fill.scale = tm_scale_categorical(values = c("red", "white", "blue"),
                                                labels = c("Reduction: Significant", "Not Significant", "Increase: Significant")),
              fill.legend = tm_legend(frame = FALSE), lwd = 0.1) +
  tm_shape(water_clip) +
    tm_polygons("#efefef", lwd = 0.1, col = "gray60") +
  tm_shape(city_limits) +
    tm_polygons(col = "black", fill_alpha = 0, lwd = 1.5) 
finalize_map(austin_income_sig, "19_austin_income_sig.pdf")


# step 31B: Population Density significance
    # t-score
austin_results_gwr$tstatDensity <- austin_results_gwr$CoefLogDensity / austin_results_gwr$SELogDensity
    # generate 3 buckets of significance and column of results
austin_results_gwr$Density_sig <- cut(austin_results_gwr$tstatDensity,
                                breaks=c(min(austin_results_gwr$tstatDensity), -2, 2, max(austin_results_gwr$tstatDensity)),
                                labels=c("Reduction: Significant","Not Significant", "Increase: Significant"))
    # plot significance map
austin_density_sig <- tm_shape(austin_results_gwr, bbox = austin_bbox, unit = "mi") + 
  tm_polygons(fill = "Density_sig", 
              fill.scale = tm_scale_categorical(values = c("red", "white", "blue"),
                                                labels = c("Reduction: Significant", "Not Significant", "Increase: Significant")),
              fill.legend = tm_legend(frame = FALSE), lwd = 0.1) +
  tm_shape(water_clip) +
    tm_polygons("#efefef", lwd = 0.1, col = "gray60") +
  tm_shape(city_limits) +
    tm_polygons(col = "black", fill_alpha = 0, lwd = 1.5)
finalize_map(austin_density_sig, "20_austin_density_sig.pdf")


# step 31C: Pct Renter significance
    # t-score
austin_results_gwr$tstatRenter <- austin_results_gwr$CoefLogRenter / austin_results_gwr$SELogRenter
# generate 3 buckets of significance and column of results
austin_results_gwr$Renter_sig <- cut(austin_results_gwr$tstatRenter,
                                breaks=c(min(austin_results_gwr$tstatRenter), -2, 2, max(austin_results_gwr$tstatRenter)),
                                labels=c("Reduction: Significant","Not Significant", "Increase: Significant"))
    # plot significance map 
austin_renter_sig <- tm_shape(austin_results_gwr, bbox = austin_bbox, unit = "mi") + 
  tm_polygons(fill = "Renter_sig", 
              fill.scale = tm_scale_categorical(values = c("red", "white", "blue"),
                                                labels = c("Reduction: Significant", "Not Significant", "Increase: Significant")),
              fill.legend = tm_legend(frame = FALSE), lwd = 0.1) +
  tm_shape(water_clip) +
    tm_polygons("#efefef", lwd = 0.1, col = "gray60") +
  tm_shape(city_limits) +
    tm_polygons(col = "black", fill_alpha = 0, lwd = 1.5)
finalize_map(austin_renter_sig, "21_austin_renter_sig.pdf")


# step 31D: Pct 65+ significance
    # t-score
austin_results_gwr$tstat65 <- austin_results_gwr$CoefLog65 / austin_results_gwr$SELog65
    # generate 3 buckets of significance and column of results
    # can't start variable with number, A65 instead of 65
austin_results_gwr$A65_sig <- cut(austin_results_gwr$tstat65,
                                      breaks=c(min(austin_results_gwr$tstat65), -2, 2, max(austin_results_gwr$tstat65)),
                                      labels=c("Reduction: Significant","Not Significant", "Increase: Significant"))
    # plot significance map 
austin_65_sig <- tm_shape(austin_results_gwr, bbox = austin_bbox, unit = "mi") + 
  tm_polygons(fill = "A65_sig", 
              fill.scale = tm_scale_categorical(values = c("red", "white", "blue"),
                                                labels = c("Reduction: Significant", "Not Significant", "Increase: Significant")),
              fill.legend = tm_legend(frame = FALSE), lwd = 0.1) +
  tm_shape(water_clip) +
    tm_polygons("#efefef", lwd = 0.1, col = "gray60") +
  tm_shape(city_limits) +
    tm_polygons(col = "black", fill_alpha = 0, lwd = 1.5)
finalize_map(austin_65_sig, "22_austin_65_sig.pdf")


# step 31E: pct_limited_english_pop significance
    # t-score
austin_results_gwr$tstatLanguage <- austin_results_gwr$CoefLogLanguage / austin_results_gwr$SELogLanguage
    # generate 3 buckets of significance and column of results
austin_results_gwr$Language_sig <- cut(austin_results_gwr$tstatLanguage,
                               breaks=c(min(austin_results_gwr$tstatLanguage), -2, 2, max(austin_results_gwr$tstatLanguage)),
                               labels=c("Reduction: Significant","Not Significant", "Increase: Significant"))
    # plot significance map 
austin_language_sig <- tm_shape(austin_results_gwr, bbox = austin_bbox, unit = "mi") + 
  tm_polygons(fill = "Language_sig", 
              fill.scale = tm_scale_categorical(values = c("red", "white", "blue"),
                                                labels = c("Reduction: Significant", "Not Significant", "Increase: Significant")),
              fill.legend = tm_legend(frame = FALSE), lwd = 0.1) +
  tm_shape(water_clip) +
    tm_polygons("#efefef", lwd = 0.1, col = "gray60") +
  tm_shape(city_limits) +
    tm_polygons(col = "black", fill_alpha = 0, lwd = 1.5)
finalize_map(austin_language_sig, "23_austin_language_sig.pdf")


# step 31F: pct_non_white significance
    # t-score
austin_results_gwr$tstatRace <- austin_results_gwr$CoefLogRace / austin_results_gwr$SELogRace
# generate 3 buckets of significance and column of results
austin_results_gwr$Race_sig <- cut(austin_results_gwr$tstatRace,
                                          breaks=c(min(austin_results_gwr$tstatRace), -2, 2, max(austin_results_gwr$tstatRace)),
                                          labels=c("Reduction: Significant","Not Significant", "Increase: Significant"))
    # plot significance map 
austin_race_sig <- tm_shape(austin_results_gwr, bbox = austin_bbox, unit = "mi") + 
  tm_polygons(fill = "Race_sig", 
              fill.scale = tm_scale_categorical(values = c("red", "white", "blue"),
                                                labels = c("Reduction: Significant", "Not Significant", "Increase: Significant")),
              fill.legend = tm_legend(frame = FALSE), lwd = 0.1) +
  tm_shape(water_clip) +
    tm_polygons("#efefef", lwd = 0.1, col = "gray60") +
  tm_shape(city_limits) +
    tm_polygons(col = "black", fill_alpha = 0, lwd = 1.5)
finalize_map(austin_race_sig, "24_austin_race_sig.pdf")


# step 32: define variable list once, reused across all exports below


# step 32A: significance
sig_vars <- list(
  list(label = "Median HH Income", sig_col = "Income_sig"),
  list(label = "Pop Density",      sig_col = "Density_sig"),
  list(label = "Pct Renter",       sig_col = "Renter_sig"),
  list(label = "Pct 65+",          sig_col = "A65_sig"),
  list(label = "Pct Non-English",  sig_col = "Language_sig"),
  list(label = "Pct Non-White",    sig_col = "Race_sig")
)

# long-format tract-level significance (one row per tract per variable)
austin_tract_sig <- do.call(rbind, lapply(sig_vars, function(v) {
  data.frame(
    GEOID        = austin_results_gwr$GEOID,
    variable     = v$label,
    significance = austin_results_gwr[[v$sig_col]]
  )
}))
write.csv(austin_tract_sig, file = "8_austin_tract_significance.csv", row.names = FALSE)


# step 32B: raw tract counts by significance category, per variable
austin_sig_counts <- do.call(rbind, lapply(sig_vars, function(v) {
  vals <- austin_results_gwr[[v$sig_col]]
  data.frame(
    variable        = v$label,
    n_increase_sig  = sum(vals == "Increase: Significant", na.rm = TRUE),
    n_reduction_sig = sum(vals == "Reduction: Significant", na.rm = TRUE),
    n_not_sig       = sum(vals == "Not Significant", na.rm = TRUE),
    n_total         = sum(!is.na(vals))
  )
}))
write.csv(austin_sig_counts, file = "9_austin_sig_counts.csv", row.names = FALSE)


# step 32C: city-level % significance summary (one row per variable)
austin_sig_summary <- do.call(rbind, lapply(sig_vars, function(v) {
  vals <- austin_results_gwr[[v$sig_col]]
  data.frame(
    variable          = v$label,
    pct_increase_sig  = round(mean(vals == "Increase: Significant", na.rm = TRUE) * 100, 1),
    pct_reduction_sig = round(mean(vals == "Reduction: Significant", na.rm = TRUE) * 100, 1),
    pct_not_sig       = round(mean(vals == "Not Significant", na.rm = TRUE) * 100, 1)
  )
}))
write.csv(austin_sig_summary, file = "10_austin_sig_summary.csv", row.names = FALSE)


# step 32D: TV-based significance (independent cross-check using |t| >= 1.96)
# issues pulling from a different t value, it was an extra check anyways


# step 32E: city-level Low/Non/High summary — one row per city
lnh_map <- c("Reduction: Significant" = "Low", "Not Significant" = "Non", "Increase: Significant" = "High")

variable_dominant <- sapply(sig_vars, function(v) {
  vals <- lnh_map[as.character(austin_results_gwr[[v$sig_col]])]
  names(sort(table(vals), decreasing = TRUE))[1]
})
names(variable_dominant) <- sapply(sig_vars, function(v) v$label)

lnh_counts <- table(factor(variable_dominant, levels = c("Low", "Non", "High")))
lnh_pct <- round(prop.table(lnh_counts) * 100, 1)

austin_city_summary <- cbind(
  city = "austin",
  as.data.frame(t(variable_dominant)),
  low_non_high = paste0(lnh_pct["Low"], "%, ", lnh_pct["Non"], "%, ", lnh_pct["High"], "%"),
  dominant = names(sort(lnh_counts, decreasing = TRUE))[1]
)
write.csv(austin_city_summary, file = "12_austin_city_lnh_summary.csv", row.names = FALSE)


# step 32F: tract-level Low/Non/High table — one row per tract
tract_lnh <- as.data.frame(sapply(sig_vars, function(v) {
  lnh_map[as.character(austin_results_gwr[[v$sig_col]])]
}))
colnames(tract_lnh) <- sapply(sig_vars, function(v) v$label)

tract_counts <- t(apply(tract_lnh, 1, function(row) table(factor(row, levels = c("Low", "Non", "High")))))
tract_pct <- round(prop.table(tract_counts, margin = 1) * 100, 1)

austin_tract_lnh <- cbind(
  GEOID = austin_results_gwr$GEOID,
  tract_lnh,
  low_non_high = paste0(tract_pct[, "Low"], "%, ", tract_pct[, "Non"], "%, ", tract_pct[, "High"], "%"),
  dominant = apply(tract_counts, 1, function(row) names(sort(row, decreasing = TRUE))[1])
)
write.csv(austin_tract_lnh, file = "13_austin_tract_lnh_table.csv", row.names = FALSE)




# ================================
# SECTION 7: Robustness Checks - Spatial Lag Model
# ================================


# NOTE: engagement_score is bounded -100/100, not a ratio and left untransformed
# (do not log10() it, negative values become NaN)


# step 33: fit Spatial Autoregressive (SAR) lag model and review fit
modelSLY <- spatialreg::lagsarlm(
  ES_sqrt ~ median_hh_income + pop_density_sqmi + pct_renter +
    pct_65_plus + pct_limited_english_pop + pct_non_white,
  data = austin, austin_weights, zero.policy = TRUE
)

summary(modelSLY)
AIC(modelSLY)

# step 34: calculate pseudo R-squared for SAR model
fitted_lag <- fitted(modelSLY)
pseudoR2_lag <- cor(austin$ES_sqrt, fitted_lag)^2
pseudoR2_lag


# step 35: check residual spatial autocorrelation (Moran's I) for SAR model
austin$RESID_SLY <- modelSLY$residuals
moran.mc(austin$RESID_SLY, austin_weights, 1000, zero.policy = TRUE)


# step 36: compute direct/indirect/total impacts for SAR model (decomposition magnitude)
Weights_2.0 <- as(austin_weights, "CsparseMatrix")
trMC <- trW(Weights_2.0, type = "MC")
imp_summary <- summary(impacts(modelSLY, tr = trMC, R = 100), zstats = TRUE)
imp_summary

# ================================
# SECTION 8: Robustness Checks - Spatial Error Model
# ================================


# step 37: fit Spatial Error Model (SEM) and review fit
modelSER <- errorsarlm(
  ES_sqrt ~ median_hh_income + pop_density_sqmi + pct_renter +
    pct_65_plus + pct_limited_english_pop + pct_non_white,
  data = austin, austin_weights, zero.policy = TRUE
)

summary(modelSER)
AIC(modelSER)


# step 38: calculate pseudo R-squared for SEM model
fitted_error <- fitted(modelSER)
pseudoR2_error <- cor(austin$ES_sqrt, fitted_error)^2
pseudoR2_error


# step 39: check residual spatial autocorrelation (Moran's I) for SEM model
austin$RESID_SER <- modelSER$residuals
moran.mc(austin$RESID_SER, austin_weights, 1000, zero.policy = TRUE)


# ================================
# SECTION 9: Robustness Checks - SLX
# ================================


# step 40: fit SLX model and review fit, impacts, and AIC
modelSLX <- lmSLX(
  ES_sqrt ~ median_hh_income + pop_density_sqmi + pct_renter +
    pct_65_plus + pct_limited_english_pop + pct_non_white,
  data = austin, austin_weights, zero.policy = TRUE
)

summary(modelSLX)
impacts(modelSLX, tr = trMC, R = 100)
AIC(modelSLX)


# step 41: export SLX coefficient summary
slx_coeffs <- as.data.frame(summary(modelSLX)$coefficients)
slx_coeffs$term <- rownames(slx_coeffs)
write.csv(slx_coeffs, file = "14_austin_slx_coeff_summary.csv", row.names = FALSE)


# ================================
# SECTION 10: Model comparisons and Export Results
# ================================


# step 42: compare AIC across SAR, SEM, and SLX models and export
model_comp <- data.frame(
  model = c("SAR", "SEM", "SLX"),
  AIC = c(AIC(modelSLY), AIC(modelSER), AIC(modelSLX))
)
model_comp

# model comparison
write.csv(model_comp, file = "15_austin_sar_sem_comp.csv", row.names = FALSE)


# step 43: export coefficient tables for SAR, SEM, and SLX models
sar_coeffs <- as.data.frame(summary(modelSLY)$Coef)
sar_coeffs$term <- rownames(sar_coeffs)
write.csv(sar_coeffs, file = "16_austin_sar_coeff_summary.csv", row.names = FALSE)

sem_coeffs <- as.data.frame(summary(modelSER)$Coef)
sem_coeffs$term <- rownames(sem_coeffs)
write.csv(sem_coeffs, file = "17_austin_sem_coeff_summary.csv", row.names = FALSE)

slx_coeffs <- as.data.frame(summary(modelSLX)$coefficients)
slx_coeffs$term <- rownames(slx_coeffs)
write.csv(slx_coeffs, file = "18_austin_slx_coeff_summary.csv", row.names = FALSE)


# step 44: export Moran's I residual test results for SAR and SEM models
sar_moran <- broom::tidy(moran.mc(austin$RESID_SLY, austin_weights, 1000, zero.policy = TRUE))
write.csv(sar_moran, file = "19_austin_sar_moran_resid.csv", row.names = FALSE)

sem_moran <- broom::tidy(moran.mc(austin$RESID_SER, austin_weights, 1000, zero.policy = TRUE))
write.csv(sem_moran, file = "20_austin_sem_moran_resid.csv", row.names = FALSE)


# step 45: export SAR impacts summary
# impacts (direct/indirect/total)
impacts_df <- data.frame(
  variable = rownames(imp_summary$pzmat),
  direct = imp_summary$res$direct,
  indirect = imp_summary$res$indirect,
  total = imp_summary$res$total,
  direct_p = imp_summary$pzmat[, "Direct"],
  indirect_p = imp_summary$pzmat[, "Indirect"],
  total_p = imp_summary$pzmat[, "Total"]
)
write.csv(impacts_df, file = "21_austin_sar_impacts_summary.csv", row.names = FALSE)


# step 46: assemble cross-model reconciliation table
extract_pval <- function(coef_df, term_name, pval_col = "Pr(>|z|)") {
  row <- coef_df[coef_df$term == term_name, ]
  if (nrow(row) == 0) return(NA)
  row[[pval_col]]
}

reconcile_vars <- list(
  list(label = "Median HH Income", glwr_sig = "Income_sig",  term = "median_hh_income"),
  list(label = "Pop Density",      glwr_sig = "Density_sig", term = "pop_density_sqmi"),
  list(label = "Pct Renter",       glwr_sig = "Renter_sig",  term = "pct_renter"),
  list(label = "Pct 65+",          glwr_sig = "A65_sig",     term = "pct_65_plus"),
  list(label = "Pct Non-English",  glwr_sig = "Language_sig",term = "pct_limited_english_pop"),
  list(label = "Pct Non-White",    glwr_sig = "Race_sig",    term = "pct_non_white")
)

austin_reconciliation <- do.call(rbind, lapply(reconcile_vars, function(v) {
  glwr_vals <- austin_results_gwr[[v$glwr_sig]]
  glwr_pct_sig <- round(mean(glwr_vals != "Not Significant", na.rm = TRUE) * 100, 1)
  
  sar_p <- extract_pval(sar_coeffs, v$term)                       # Pr(>|z|)
  sem_p <- extract_pval(sem_coeffs, v$term)                       # Pr(>|z|)
  slx_p <- extract_pval(slx_coeffs, v$term, pval_col = "Pr(>|t|)") # Pr(>|t|)
  
  data.frame(
    variable         = v$label,
    glwr_pct_sig     = glwr_pct_sig,
    sar_significant  = !is.na(sar_p) & sar_p < 0.05,
    sem_significant  = !is.na(sem_p) & sem_p < 0.05,
    slx_significant  = !is.na(slx_p) & slx_p < 0.05
  )
}))

write.csv(austin_reconciliation, file = "22_austin_model_reconciliation.csv", row.names = FALSE)


### end of code!



