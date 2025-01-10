## ----setup, message = FALSE---------------------------------------------------
library(BerkeleyForestsAnalytics)
library(dplyr)
library(tidyr)

## -----------------------------------------------------------------------------
# Note that the example data used in this vignette is included with the package
# which is why we do not have to read in the data

head(vign_trees_1)

## ----error = TRUE-------------------------------------------------------------
tree_bio <- SummaryBiomass(data = vign_trees_1,
                           site = "id",
                           plot = "plot",
                           exp_factor = "exp_factor",
                           status = "status",
                           decay_class = "decay",
                           species = "species",
                           dbh = "dbh",
                           ht = "ht")

## -----------------------------------------------------------------------------
vign_trees_1 %>%
  filter(exp_factor == 0)

vign_trees_1 %>%
  filter(time == "post", site == "60", plot == "112")

## ----error = TRUE-------------------------------------------------------------
tree_bio <- SummaryBiomass(data = vign_trees_2,
                           site = "id",
                           plot = "plot",
                           exp_factor = "exp_factor",
                           status = "status",
                           decay_class = "decay",
                           species = "species",
                           dbh = "dbh",
                           ht = "ht",
                           results = "by_plot")

## -----------------------------------------------------------------------------
vign_trees_2 %>%
  filter(species == "ABCCO" | species == "SME")

## -----------------------------------------------------------------------------
tree_bio <- SummaryBiomass(data = vign_trees_3,
                           site = "id",
                           plot = "plot",
                           exp_factor = "exp_factor",
                           status = "status",
                           decay_class = "decay",
                           species = "species",
                           dbh = "dbh",
                           ht = "ht",
                           results = "by_plot")

head(tree_bio)

## -----------------------------------------------------------------------------
vign_trees_3 %>% 
  filter(status == 0, decay == 0 | is.na(decay))

## -----------------------------------------------------------------------------
tree_bio <- SummaryBiomass(data = vign_trees_4,
                           site = "id",
                           plot = "plot",
                           exp_factor = "exp_factor",
                           status = "status",
                           decay_class = "decay",
                           species = "species",
                           dbh = "dbh",
                           ht = "ht",
                           results = "by_plot")

head(tree_bio)

## -----------------------------------------------------------------------------
vign_trees_4 %>% 
  filter(exp_factor > 0, is.na(dbh))

## -----------------------------------------------------------------------------
tree_bio <- SummaryBiomass(data = vign_trees_5,
                           site = "id",
                           plot = "plot",
                           exp_factor = "exp_factor",
                           status = "status",
                           decay_class = "decay",
                           species = "species",
                           dbh = "dbh",
                           ht = "ht",
                           results = "by_plot")

head(tree_bio)

## ----error = TRUE-------------------------------------------------------------
for_comp <- ForestComp(data = vign_trees_5,
                       site = "id",
                       plot = "plot",
                       exp_factor = "exp_factor",
                       status = "status",
                       species = "species",
                       dbh = "dbh",
                       relative = "ba",
                       units = "metric")

## -----------------------------------------------------------------------------
for_comp <- ForestComp(data = vign_trees_5,
                       site = "id",
                       plot = "plot",
                       exp_factor = "exp_factor",
                       status = "status",
                       species = "species",
                       dbh = "dbh",
                       relative = "BA",
                       units = "metric")

head(for_comp, n = 16)

## -----------------------------------------------------------------------------
for_comp %>%
  filter(site == "post_60", plot == "113")

## ----error = TRUE-------------------------------------------------------------
for_str <- ForestStr(data = vign_trees_5,
                     site = "id",
                     plot = "plot",
                     exp_factor = "Exp_factor",
                     dbh = "dbh",
                     ht = "ht")

## -----------------------------------------------------------------------------
for_str <- ForestStr(data = vign_trees_5,
                     site = "id",
                     plot = "plot",
                     exp_factor = "exp_factor",
                     dbh = "dbh",
                     ht = "ht")

head(for_str)

## -----------------------------------------------------------------------------
for_str %>%
  filter(site == "post_60", plot == "113")

## -----------------------------------------------------------------------------
head(vign_fuels_1)

## ----error = TRUE-------------------------------------------------------------
FWD <- FineFuels(tree_data = vign_trees_5,
                 fuel_data = vign_fuels_1)

## -----------------------------------------------------------------------------
vign_fuels_1 %>%
  filter(time == "post", site == "400", plot == "9")

vign_fuels_1 %>%
  filter(time == "pre", site == "400", plot == "9")

## ----error = TRUE-------------------------------------------------------------
FWD <- FineFuels(tree_data = vign_trees_5,
                 fuel_data = vign_fuels_2)

## -----------------------------------------------------------------------------
vign_fuels_2 %>%
  mutate(count_100h_check = abs(round(count_100h))) %>%
  filter(count_100h != count_100h_check)

## ----error = TRUE, warning = FALSE--------------------------------------------
FWD <- FineFuels(tree_data = vign_trees_5,
                 fuel_data = vign_fuels_3)

## -----------------------------------------------------------------------------
FWD <- FineFuels(tree_data = vign_trees_5,
                 fuel_data = vign_fuels_4)

head(FWD)

## -----------------------------------------------------------------------------
vign_fuels_4 %>%
  filter(is.na(count_10h))

## ----error = TRUE-------------------------------------------------------------
CWD <- CoarseFuels(tree_data = vign_trees_5,
                   fuel_data = vign_fuels_4,
                   summed = "yes")

## -----------------------------------------------------------------------------
vign_fuels_4 %>%
  filter(is.na(length_1000h))

## -----------------------------------------------------------------------------
CWD <- CoarseFuels(tree_data = vign_trees_5,
                   fuel_data = vign_fuels_5,
                   summed = "yes")

head(CWD)

## -----------------------------------------------------------------------------
LD <- LitterDuff(tree_data = vign_trees_5,
                 fuel_data = vign_fuels_5)

head(LD)

## -----------------------------------------------------------------------------
head(tree_bio)

## -----------------------------------------------------------------------------
tree_bio_2 <- tree_bio %>%
  separate(site, c("time", "site")) %>% # separate id into time and site columns 
  mutate(trt_type = "fire") %>% # create a trt_type column 
  select(time, trt_type, site, plot, everything()) # organize columns as desired 

head(tree_bio_2)

## -----------------------------------------------------------------------------
# keep the defaults for wt_data (= "not_needed")
tree_bio_sum <- CompilePlots(data = tree_bio_2,
                             design = "FFS")

## -----------------------------------------------------------------------------
tree_bio_sum$site # pull out site-level summary 
tree_bio_sum$trt_type # pull out treatment-level summary 

## -----------------------------------------------------------------------------
head(FWD)

## -----------------------------------------------------------------------------
FWD_2 <- FWD %>%
  mutate(trt_type = "fire") %>% # create a trt_type column 
  select(time, trt_type, site, plot, everything()) # organize columns as desired 

head(FWD_2)

## -----------------------------------------------------------------------------
# keep the defaults for cwd_data (= "none), wt_data (= "not_needed"), and units (= "metric")
FWD_sum <- CompileSurfaceFuels(fwd_data = FWD_2,
                               design = "FFS")

## -----------------------------------------------------------------------------
FWD_sum$site # pull out site-level summary
FWD_sum$trt_type # pull out treatment-level summary 

