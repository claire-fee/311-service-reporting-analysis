# ================================
# Beyond the Squeaky Wheel: 311 Engagement & Equity Analysis
# Notebook 1: 311 Data EDA and Cleaning (Sample)
# ================================


# This notebook shows a sample EDA and data cleaning techniques for two cities
# All cities with available 311 data were processed
# Core cleaning techniques were combining multiple CSVs and normalizing date/timestamps
# EDA included basic dataset stats and temporal counts, and pulling unique values for:
        # service requests, reporting methodology, jurisdictional departments
        # Not all cities share reporting methodology data

#### INSTRUCTIONS ####
# Review and adapt techniques for applicable 311 data
        # Update file names, links, cities, categories, etc.
# Original file names left in for referential purposes 




# ================================
# SECTION 1: Script Preparation
# ================================

# step 1: import libraries

library(dplyr)

# step 2: set working directory

setwd("INSERT FILE FOLDER PATH")




# ================================
# SECTION 2: Sample City - Chicago (single online repository)
# ================================


# step 1: upload all CSVs
Chicago <- read.csv("Chicago/CHI_21-25_noXCL.csv")


# step 2: get summarial info
colnames(Chicago)  #column names
nrow(Chicago)   # total rows
ncol(Chicago)   # total columns


# step 3: extract and count all unique service requests and jurisdictional deperatment
Chicago_counts <- as.data.frame(table(Chicago$OWNER_DEPARTMENT, Chicago$SR_TYPE))
Chicago_counts <- Chicago_counts[Chicago_counts$Freq > 0, ]

write.csv(Chicago_counts, "Chicago_summary.csv", row.names = FALSE)


# step 4: extract date/time from table
Chicago$date_parsed <- as.POSIXct(
  Chicago$CREATED_DATE,
  format = "%m/%d/%Y %I:%M:%S %p",
  tz = "UTC")

Chicago$year <- format(Chicago$date_parsed, "%Y")
Chicago$year_month <- format(Chicago$date_parsed, "%Y-%m")


# step 5: count SRs per year
# remove all years outside temporal scope
Chicago <- Chicago %>%
  filter(!year %in% c(2020, 2026))

as.data.frame(table(Chicago$year))
as.data.frame(table(Chicago$year_month))


# step 6: count SRs by reporting method
Chi_methods <- Chicago %>%
  count(year, ORIGIN)


# ================================
# SECTION 3: Sample City - Atlanta (multiple CSV files)
# ================================


# step 1: upload all CSVs
ATL22 <- read.csv("Atlanta/ATL311_ORR_2022.csv")
ATL23 <- read.csv("Atlanta/ATL311_ORR_2023.csv")
ATL24 <- read.csv("Atlanta/ATL311_ORR_2024.csv")
ATL25 <- read.csv("Atlanta/ATL311_ORR_025.csv")


# step 2: crate single master dataset
        # If more than 1,048,576 rows file is to big for Excel      
Atlanta <- do.call(rbind, list(ATL22, ATL23, ATL24, ATL25))
write.csv(Atlanta, "Atlanta_22-25_noXCL.csv", row.names = FALSE)


# step 3: check each column for broken/invalid character encoding
for (col in names(Atlanta)) {
  tryCatch({
    nchar(Atlanta[[col]], type = "width")
  }, error = function(e) {
    cat("Problem column:", col, "\n")
  })
}


# step 4: get summarial info
colnames(Atlanta)  #column names
nrow(Atlanta)   # total rows
ncol(Atlanta)   # total columns


# step 5: extract and count all unique service requests and jurisdictional deperatment
Atlanta_counts <- as.data.frame(table(Atlanta$Owning.Department, Atlanta$SR.Description))
Atlanta_counts <- Atlanta_counts[Atlanta_counts$Freq > 0, ]

write.csv(Atlanta_counts, "Atlanta_summary.csv", row.names = FALSE)


# step 6: extract date/time from table
Atlanta$date_parsed <- as.POSIXct(
  Atlanta$Created.Date,
  format = "%Y %b %d %I:%M:%S %p",
  tz = "UTC")

Atlanta$year <- format(Atlanta$date_parsed, "%Y")
Atlanta$year_month <- format(Atlanta$date_parsed, "%Y-%m")


# step 7: count SRs per year
        # remove all years outside temporal scope
Atlanta <- Atlanta %>%
  filter(!year %in% c(2020, 2026))

as.data.frame(table(Atlanta$year))
as.data.frame(table(Atlanta$year_month))


# step 8: count SRs by reporting method
as.data.frame(table(Atlanta$Method.Received))

Atlanta_methods <- Atlanta %>%
  count(year, Method.Received)


# end of code!



