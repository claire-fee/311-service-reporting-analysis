# ================================
# Beyond the Squeaky Wheel: 311 Engagement & Equity Analysis
# Notebook 8: Exploratory Data Analysis for Engagement Score Binomial GLWR
# ================================


# Runs iterative correlation matrices and VIF screening on potential demographic variables 
# Joins the selected variables to census tract geometry
# Removes tracts with missing data
# Poduces prepped dataset GLWR in Notebook 9




# ================================
# SECTION 1: Script Preparation
# ================================


# step 1: activate library packages

library("sf")
library("car")
library("tidyr")
library("tidyverse")
library("corrplot")
library("tmap")

# step 2: set working directory

setwd("INSERT FILE FOLDER PATH")

# step 2.b: have option to make plotted map interactive

tmap_mode("view") 
tmap_mode("plot")




# ================================
# SECTION 2: EDA - All Variables
# ================================

# step 3: upload full variable dataset for census tracts

full_data <- read.csv("INSERT NAME OF DATASET CSV FILE")

# step 4: confirm column names and dimensions
names(full_data)
nrow(full_data)
ncol(full_data)

#### run correlation matrix to assess variables

# step 5: prepare dataset for correlation matrix
correlation_subset <- (full_data %>% select(population, median_hh_income, median_home_value, median_gross_rent, median_year_built,
                                             land_area_sqmi, pop_density_sqmi, pct_indv_poverty, unemployment_rate, pct_vacant, pct_renter,
                                             pct_no_vehicle, pct_no_internet, pct_smartphone_only, pct_limited_english_pop, pct_foreign_born, 
                                             pct_under_18, pct_65_plus, pct_bachelors_plus,pct_non_white, pct_white_nh, pct_black_nh, 
                                             pct_hispanic, pct_asian_nh, pct_aian_nh, pct_nhpi_nh, pct_other_nh, pct_two_plus_nh))

# step 6: run correlation matrix
correlation_matrix <- cor(correlation_subset, use = "pairwise.complete.obs")

# step 7: print results of correlation matrix
pdf("correlation_matrix_full.pdf", width = 17, height = 11)  # inches, landscape tabloid

corrplot(correlation_matrix,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.cex = 1,
         addCoef.col = "black",
         number.cex = 0.5)

dev.off()




# ================================
# SECTION 3: EDA - Refined Variable List
# ================================

#### run correlation matrix to assess variables

# step 8: prepare dataset for correlation matrix
correlation_subset2 <- (full_data %>% select(population, median_hh_income, median_home_value, median_gross_rent,
                                            pop_density_sqmi, pct_indv_poverty, unemployment_rate, pct_vacant, pct_renter,
                                            pct_no_vehicle, pct_no_internet, pct_smartphone_only, pct_limited_english_pop, 
                                            pct_foreign_born, pct_under_18, pct_65_plus, pct_bachelors_plus,pct_non_white))
                                            
# step 9: run correlation matrix
correlation_matrix2 <- cor(correlation_subset2, use = "pairwise.complete.obs")

# step 10: print results of correlation matrix
pdf("correlation_matrix_2.pdf", width = 17, height = 11)  # inches, landscape tabloid

corrplot(correlation_matrix2,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.cex = 1,
         addCoef.col = "black",
         number.cex = 0.5)

dev.off()




# ================================
# SECTION 4: EDA - More Refined Variable List
# ================================

#### run correlation matrix to assess variables

# step 11: prepare dataset for correlation matrix
correlation_subset3 <- (full_data %>% select(median_hh_income, median_home_value, pop_density_sqmi, 
                                             unemployment_rate, pct_vacant, pct_renter, pct_no_internet, 
                                             pct_smartphone_only, pct_limited_english_pop, pct_foreign_born, 
                                             pct_under_18, pct_65_plus, pct_bachelors_plus,pct_non_white)) 

# step 12: run correlation matrix
correlation_matrix3 <- cor(correlation_subset3, use = "pairwise.complete.obs")

# step 13: print results of correlation matrix
pdf("correlation_matrix_3.pdf", width = 17, height = 11)  # inches, landscape tabloid

corrplot(correlation_matrix3,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.cex = 1,
         addCoef.col = "black",
         number.cex = 0.5)

dev.off()




# ================================
# SECTION 5: EDA - Most Refined Variable List
# ================================

#### run correlation matrix to assess variables

# step 14: prepare dataset for correlation matrix
correlation_subset4 <- (full_data %>% select(median_hh_income, pop_density_sqmi, pct_renter, 
                                             pct_smartphone_only, pct_limited_english_pop, 
                                             pct_foreign_born, pct_65_plus, pct_non_white)) 

# step 15: run correlation matrix
correlation_matrix4 <- cor(correlation_subset4, use = "pairwise.complete.obs")

# step 16: print results of correlation matrix
pdf("correlation_matrix_4.pdf", width = 17, height = 11)  # inches, landscape tabloid

corrplot(correlation_matrix4,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.cex = 1,
         addCoef.col = "black",
         number.cex = 0.5)

dev.off()




# ================================
# SECTION 6: EDA - VIF screening for Multicollinearity
# ================================

# step 17: prepare first VIF screening 

vif_model <- lm( SQRT_score ~
                   median_hh_income +
                   median_home_value +
                   pop_density_sqmi +
                   unemployment_rate +
                   pct_vacant +
                   pct_renter +
                   pct_no_internet +
                   pct_smartphone_only +
                   pct_65_plus +
                   pct_limited_english_pop +
                   pct_non_white, 
                 data = full_data)

# step 18: calculate and display VIF scores
vif(vif_model)

# step 19: prepare second/refined VIF screening

vif_model2 <- lm( SQRT_score ~
                   median_hh_income +
                   pop_density_sqmi +
                   pct_renter +
                   pct_smartphone_only +
                   pct_65_plus +
                   pct_limited_english_pop +
                   pct_non_white, 
                 data = full_data)

# step 20: calculate and display VIF scores
vif(vif_model2)

# step 21: prepare third/most refined VIF screening

vif_model3 <- lm( SQRT_score ~
                    median_hh_income +
                    pop_density_sqmi +
                    pct_renter +
                    pct_65_plus +
                    pct_limited_english_pop +
                    pct_non_white, 
                  data = full_data)

# step 22: calculate and display VIF scores
vif(vif_model3)




# ================================
# SECTION 7: Data Cleaning Preparation
# ================================

# step 23: load the base census tract shapefiles!

cen_tracts <- st_read("INSERT NAME OF CENSUS TRACTS GPKG FILE")

# step 24: check and fix CRS as needed
st_crs(cen_tracts)

# step 25: confirm and clean data types for join

class(full_data$GEOID)
class(cen_tracts$GEOID)

# step 26: fix GEOIDs that became numeric
full_data <- full_data %>%
  mutate(GEOID = str_pad(as.character(GEOID), width = 11, side = "left", pad = "0"))

# step 27: join the demographic data to census tracts
cen_tracts2 <- cen_tracts %>%
  left_join(full_data, by = "GEOID")

# step 28: confirm join and review column names
names(cen_tracts2)
nrow(cen_tracts2)
ncol(cen_tracts2)


# step 29: dataset cleaning: keep only relevant columns
keep_cols <- c("STATEFP.x", "COUNTYFP.x", "TRACTCE", "GEOID", 
               "ALAND.x", "AWATER.x", "city", "ES_log", "ES_sqrt", "ES_yj",
               "Log_score", "SQRT_score", "YJ_score", "median_hh_income",
               "pop_density_sqmi", "pct_renter", "pct_limited_english_pop",
               "pct_65_plus", "pct_non_white", "geom")  

# step 30: drop excess columns
cen_tracts2 <- cen_tracts2 %>% select(all_of(keep_cols))

#### drop null values that cannot be processed

# step 31: check dataset for blank variables
colSums(is.na(cen_tracts2))

# step 32: count null values by city
na_by_city <- cen_tracts2 %>%
  st_drop_geometry() %>%
  group_by(city) %>%
  summarise(across(everything(), ~ sum(is.na(.))))

# step 33: count total null tracts by city
na_by_city_total <- cen_tracts2 %>%
  st_drop_geometry() %>%
  mutate(has_any_na = if_any(c(median_hh_income, pop_density_sqmi, pct_renter,
                               pct_limited_english_pop,
                               pct_non_white, pct_65_plus),
                             is.na)) %>%
  group_by(city) %>%
  summarise(tracts_with_any_na = sum(has_any_na),
            total_tracts = n())

write.csv(na_by_city, "null_value_tracts.csv", row.names = FALSE)

# step 34: remove census tracts with blank variables, not suitable for analysis

cen_tracts3 <- cen_tracts2[!is.na(cen_tracts2$SQRT_score) &
                             !is.na(cen_tracts2$median_hh_income) &
                             !is.na(cen_tracts2$pop_density_sqmi) &
                             !is.na(cen_tracts2$pct_renter) &
                             !is.na(cen_tracts2$pct_limited_english_pop) &
                             !is.na(cen_tracts2$pct_65_plus) &
                             !is.na(cen_tracts2$pct_non_white), ]

# step 35: confirm all census tracts with blank variables removed
colSums(is.na(cen_tracts3))

# step 36: count final number of census tracts regions for analysis
nrow(cen_tracts3)

# Sanity Check: test plot reference map and visually check variable distribution
tm_shape(cen_tracts3) +
  tm_polygons(fill = "SQRT_score",
              fill.scale = tm_scale_categorical(values = c("lightblue", "orange")),
              fill.legend = tm_legend(title = "All Cities: Engagement (0 = Over, 1 = Under)"),
              lwd = 0.15)

st_write(cen_tracts3, "cen_tracts3_prepped.gpkg", driver = "GPKG")

# to scroll in map
tmap_mode("view") 
tmap_mode("plot")


# end of code!



