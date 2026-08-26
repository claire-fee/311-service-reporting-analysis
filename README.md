# Beyond the Squeaky Wheel: 311 Engagement & Equity Analysis

Code and data supporting a dissertation examining whether 311 non-emergency
service request rates reflect actual urban service need, and which
demographic characteristics predict over- or under-engagement with 311
reporting at the census tract level. The analysis covers 18-20 U.S. cities
and builds three tract-level measures: a 311 Service Request Index (SRI), a
Service Need Index (SNI) built from physical and land-use conditions, and an
Engagement Score capturing the gap between the two. A binomial Geographically
Weighted Logistic Regression (GLWR) tests which demographic variables predict
that gap, with SAR, SEM, and SLX spatial regression as supplementary
robustness checks.

## Repository Structure

### `311_dissertation_code/`

Notebooks are numbered by their place in the analytical pipeline, not
sequentially. Gaps in the numbering are reserved for supporting notebooks
(311 geocoding, additional SNI data assembly, per-city robustness checks)
planned as later additions and not required to follow the core pipeline
below.

| Notebook | File | Description |
|---|---|---|
| 1 | `1_311_EDA_v1.r` | Sample 311 data cleaning and exploratory analysis (Chicago and Atlanta shown as worked examples) |
| 2 | `311_keyword_classifier_v7.ipynb` | Keyword classifier assigning 311 service requests to 77 thematic categories |
| 3 | `311_SR_keyword_match_v6.ipynb` | Matches classified categories back to each city's raw 311 records |
| 8 | `GLWR_variable_EDA.r` | Correlation and VIF screening to select the final GLWR predictor set |
| 9 | `GLWR_SQRT_v6.r` | Binomial GLWR model plus SAR/SEM/SLX supplementary regressions (single-city template, re-run per city) |
| 10 | `311_tracts_histo_transform_v1.ipynb` | Builds the 311 Service Request Index (SRI) from raw tract-level point counts |
| 13 | `SNI_03a_SNI_data_prep_v3.ipynb` | Assembles raw per-tract SNI physical/land-use variables |
| 14 | `SNI_v3_SNI_index_construction.ipynb` | Builds the final Service Need Index (SNI) from the prepped variables |
| 15 | `Engagement_Score_v4.ipynb` | Calculates the Engagement Score (SRI minus SNI) per tract |

### `311_dissertation_data/`

- **`Base_Geographies/`** - census tract and city boundary GeoPackages for all cities in the study
- **`CSV_datasets/`** - keyword classification results, final tract-level demographics, and final tract-level scores (SRI, SNI, Engagement Score)
- **`SNI_data/`** - prepped SNI input variables (transit, roads, jobs, housing age, impervious surface) and per-city zoning GeoPackages

## Data Availability

Three GeoPackages used in SNI construction exceed GitHub's 100MB per-file
limit and could not be uploaded through standard git:

- `SNI_data/SNI_All_Minor_Roads.gpkg` (363 MB)
- `SNI_data/SNI_Overture_POIs.gpkg` (553 MB)
- `SNI_data/zoning/zoning_Los_Angeles.gpkg` (179 MB)

These are raw input layers consumed by Notebook 13 (`SNI_03a_SNI_data_prep_v3.ipynb`)
during SNI variable assembly. Running that notebook from scratch will fail
without them. All other 17 cities' zoning files are present, and the
downstream prepped outputs of Notebook 13 (used by every later notebook in
the pipeline) are included in full, so this only affects someone trying to
reproduce the SNI raw data assembly step itself, not any other part of the
analysis. These files are planned for addition via Git Large File Storage
(LFS) as part of ongoing repository maintenance.

## License

Code in this repository is licensed under the MIT License (see `LICENSE`).
Data files are licensed under Creative Commons Attribution 4.0 International
(CC-BY 4.0) (see `DATA_LICENSE.txt`). Some underlying source data originates
from third-party public open data portals; see `DATA_LICENSE.txt` for
details.

## Citation

See `CITATION.cff` for citation information, or use GitHub's "Cite this
repository" option on this page.
