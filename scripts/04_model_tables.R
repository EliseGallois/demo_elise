# Elise Gallois 4th September 2023
# a - load models
# b - create model tables
# c - save in outputs folder

#### 1 - LOAD PACKAGES  ##### 
library(readr)
library(tidyverse)
library(brms)
library(broom)
library(stargazer)

#### 2a - LOAD LTER MODELS  ##### 
load("users/egallois/pheno_quantiles/models/itex_lter_growing_ssn_temp.RData") #rq2_growing_ssn
load("users/egallois/pheno_quantiles/models/lter_growing_ssn_snow.RData") # rq2_growing_ssn_melt
load("users/egallois/pheno_quantiles/models/lter_sen_temp.RData") # rq2_yellow_sep
load("users/egallois/pheno_quantiles/models/lter_bud_temp.RData") # rq2_bud_may
load("users/egallois/pheno_quantiles/models/lter_time.RData") # rq2_bud_may

#### 2b - MAKE LTER TABLE ####
# using code adapted from Mariana: https://github.com/ShrubHub/ShrubHub/blob/master/scripts/users/mgarciacriado/traits_vs_ranges/scripts/8_model_table.R
# Load a few models, run the table, and then close R and clean the environment
# Rinse and repeat with the rest of models

# Jonathan Chang's function
p_summarize <- function(model) {
  brms::posterior_summary(model) %>% 
    as_tibble(rownames = "parameter")
}

# add model objects to list
models.list <- list(rq2_bud_may,rq2_yellow_sep,rq2_growing_ssn,rq2_growing_ssn_melt)

# compile model titles
model_names <- c( "Date of Budburst vs Air Temperature",
                  "Date of Last Senesced Leaf vs Air Temperature",
                  "Growing Season Length vs Air Temperature",
                  "Growing Season Length vs Snowmelt Date")

# number the models 1 through 5
model_number <- 1:4
# bind the model tables together
mod.df <- data.frame(model_number, model_names)

# Extract parameters
mod.table <- lapply(models.list, p_summarize) %>% 
  bind_rows(.id = "model_number") 

# Add model name to table
mod.table$model_number <- as.integer(mod.table$model_number)
mod.table.final <- left_join(mod.table, mod.df, by = "model_number")

# Clean model parameters
mod.table.final <- mod.table.final %>% filter(parameter != "lp__") %>% filter(parameter != "Intercept")
mod.table.final$model_names[duplicated(mod.table.final$model_names)] <- "  "
mod.table.final$model_names <- as.character(mod.table.final$model_names)
mod.table.final$model_number[duplicated(mod.table.final$model_number)] <- "  "

colnames(mod.table.final) <- c("Model number", "Term", "Estimate", "Std. error", "Lower 95% CI", "Upper 95% CI", "Model name")
mod.table.final <- mod.table.final[, c(1, 7, 2, 3, 4, 5, 6)]

#  Round to 3 decimals only because not working on stargazer function
mod.table.final <- mod.table.final %>% mutate_if(is.numeric, round, digits = 3)


# Save in csv
write.csv(mod.table.final, "users/egallois/pheno_quantiles/outputs/lter_model_table.csv")

# remove underscores which are fudging html table production
mod.table.final$Term <- sapply(mod.table.final$Term , function(x) gsub("_", "",  x))

# Convert to table
stargazer(mod.table.final, type = "html",  summary = FALSE, out = "users/egallois/pheno_quantiles/outputs/lter_model_table.html")
