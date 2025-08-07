### Setup ###

location <- "large_models/"
sublocation <- "input_unscaled_squared_in_model/"
data_subset <- "unscaled_"

library(nlme) # linear mixed-effects models
library(MuMIn) # automated model selection
library(data.table)
library(tidyverse) # data manipulation
library(cAIC4) # conditional Akaike Information Criterion

# load standard libraries
source(paste0(location,"00_helpers/rename_fun.R"))

# load random effects and autoregression parameter
random_simple <- readRDS(paste0(location, "own_data/output_data/random_simple.rds"))
autoregression_simple <- readRDS(paste0(location, "own_data/output_data/autoregression_simple.rds"))

# function to extract cAIC and return NA if failed
fun_get_caic <- function(x) {
  tryCatch({
    cAIC(x)$caic # accessing the specific value from the cAIC function
  }, error = function(e) {
    NA # return NA or some indication of failure for models that cause an error
  })
}

# create array ####
# array for parallelized computing (not required if run on a single machine)
if (!(Sys.getenv("SLURM_ARRAY_TASK_ID") == "")) {
  i <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
}

table <- expand.grid(
  species = c("FS", "LD", "PA", "TC"),
  phase = "leaf_unfolding",
  model = c("mai", "con"),
  stringsAsFactors = FALSE) %>%
  mutate(ID = row_number()) %>%
  relocate(ID)

species_ID <- table[i, "species"]
phase_ID <- table[i, "phase"]
model_ID <- table[i, "model"]

# load species specific data ####
data <- readRDS(paste0(location, "own_data/output_data/data_", data_subset , species_ID, "_", phase_ID, ".rds")) %>%
  rename_fun() %>%
  select(-LEAF_COLOURING_LAG) %>%
  # make sure data types are set correctly
  mutate(METEO_ID = factor(METEO_ID),
         RELIEF = factor(RELIEF),
         SOCIAL_SITUATION = factor(SOCIAL_SITUATION),
         EDGE_DIST_IMPUTED = factor(EDGE_DIST_IMPUTED)) # somehow cAIC is really bad with handling factors and it's best to make them explicit, even if R has recognized them correctly when reading the data set

# load fixed effects
fixed_mainstream <- readRDS(paste0(location, "own_data/output_data/",
                                   "fixed_mainstream_", species_ID, "_", gsub(" ", "_", phase_ID), ".rds"))
fixed_conservative <- readRDS(paste0(location, "own_data/output_data/",
                                     "fixed_conservative_", species_ID, "_", gsub(" ", "_", phase_ID), ".rds"))

# load model
model <- readRDS(paste0(location, "own_data/output_data/",
                        "global_", model_ID, "_", species_ID, "_", phase_ID, ".rds"))

# dredge ####
dredge_result <- dredge(model, extra = list("RMSE" = function(x) {sqrt(mean(residuals(x)^2))}),
                        rank = list(cAIC = fun_get_caic),
                        m.lim = c(1, 10),
                        fixed = c("poly(GDD_mainstream, degree = 2)", "GDD_mainstream", "poly(GDD_conservative, degree = 2)", "GDD_conservative")
                        )

# remove failed models from the results
dredge_result_clean <- subset(dredge_result, subset = !is.na(cAIC))

# subset delta 4 and recalculate weights
dredge_result_d4 <- subset(dredge_result_clean, subset = delta < 4)
Weights(dredge_result_d4$cAIC)

# subset delta 2 and recalculate weights
dredge_result_d2 <- subset(dredge_result_clean, subset = delta < 2)
Weights(dredge_result_d2$cAIC)

# remove large object for faster processing
rm(dredge_result_clean)

# get most parsimonious models
parsimonious_d4 <- get.models(dredge_result_d4, subset = order(dredge_result_d4$df)[1:3], method = "ML")
parsimonious_d2 <- get.models(dredge_result_d2, subset = order(dredge_result_d2$df)[1:3], method = "ML")

# extract weights
summary_weight_d4 <- sw(dredge_result_d4) %>% as.data.frame() %>% rownames_to_column("PARAMETER") %>% rename(WEIGHT=2) %>% mutate(MODEL= model_ID)
summary_weight_d2 <- sw(dredge_result_d2) %>% as.data.frame() %>% rownames_to_column("PARAMETER") %>% rename(WEIGHT=2) %>% mutate(MODEL= model_ID)

# save three best models
best_names <- c(paste0("best_", model_ID, "_1"),
                paste0("best_", model_ID, "_2"),
                paste0("best_", model_ID, "_3"))
best <- get.models(dredge_result_d2, subset = order(cAIC)[1:3], method = "ML") %>% setNames(best_names)
saveRDS(best, paste0(location, "own_data/output_data/", #sublocation,
                     "best_", model_ID, "_", species_ID, "_", gsub(" ", "_", phase_ID), ".rds"))

# save result ####
saveRDS(list(
  model_name = model_ID,
  dredge_result = dredge_result,
  dredge_result_d4 = dredge_result_d4,
  dredge_result_d2 = dredge_result_d2,
  parsimonious_d4 = parsimonious_d4,
  parsimonious_d2 = parsimonious_d2,
  summary_weight_d4 = summary_weight_d4,
  summary_weight_d2 = summary_weight_d2
),
paste0(location,"own_data/output_data/", # sublocation,
       "dredge_", model_ID, "_", species_ID, "_", phase_ID, ".rds"))
