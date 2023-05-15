library(tidymodels)
library(h2o)
library(rstudioapi)
library(openxlsx)
library(janitor)
options(scipen=999) #Remove scientific notation

#Get the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

#Select location of analysis-ready data file
file_path <- file.choose()

#Read data file
df <- read.xlsx(file_path)

#Clean column names
df <- clean_names(df)

#Convert character columns to factor
df <- df %>%
  mutate_if(base::is.character, as.factor)

#Optional: Create data weights by institution
# Step 1: Calculate the frequency of each unique value in the "name" column
freqs <- df %>%
  count(name) %>%
  rename(freq = n)

# Step 2: Compute the inverse of the frequency for each value
freqs <- freqs %>%
  mutate(ipw = 1 / freq)

# Step 3: Normalize the weights so that the highest weight is at most 10 times greater than the lowest weight
max_ratio <- 10
min_weight <- min(freqs$ipw)
max_weight <- min_weight * max_ratio

freqs <- freqs %>%
  mutate(ipw = if_else(ipw > max_weight, max_weight, ipw))

# Normalize the weights so that the mean is 1
mean_weight <- mean(freqs$ipw)
freqs <- freqs %>%
  mutate(ipw = ipw / mean_weight)

# Step 4: Assign the weights to the corresponding rows in the dataframe
df <- df %>%
  left_join(freqs, by = "name") %>%
  select(-freq) # Remove the 'freq' column, as it is not needed anymore

df_model <- df %>%
  select(nsc_fall_to_fall,
         hs_gpa,
         major,
         binned_race_eth,
         tuition_in_state,
         faculty_salary)

###############################################################
#Create predictive models
###############################################################
#h2o.init() #initialize H2O

#h2o.shutdown()
h2o.init(nthreads=-1, enable_assertions  = FALSE)

# Create dataframe with ONLY your outcome variable ("nsc_fall_to_fall" or similar) and all of your predictor variables.
#df_model <- #ADD HERE

# Define the name of the outcome variable (whether student was retained from 1st to 2nd year)
y <- "nsc_fall_to_fall"

# Define the names of the predictor variables by finding the set difference between all column names in df_model and the outcome variable name
excluded_vars <- c(y, "ipw") #Y = outcome variable; ipw = weight variable
x <- setdiff(names(df_model), excluded_vars)

# Convert the data frame to an H2O object for efficient machine learning processing
train.h2o <- as.h2o(df_model)

####Gradient Boosting######
hyper_params = list( max_depth = seq(2,30,1) )
grid_id <- paste0("depth_grid_", format(Sys.time(), "%Y%m%d_%H%M"))

grid <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params,
  
  ## full Cartesian hyper-parameter search
  search_criteria = list(strategy = "Cartesian"),
  
  ## which algorithm to run
  algorithm="gbm",
  
  ## identifier for the grid, to later retrieve it
  grid_id = grid_id,
  
  ## standard model parameters
  x = x, 
  y = y, 
  training_frame = train.h2o,
  nfolds = 5,
  weights_column = "ipw",
  
  ## more trees is better if the learning rate is small enough 
  ## here, use "more than enough" trees - we have early stopping
  ntrees = 10000,                                                            
  
  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
  learn_rate = 0.05,                                                         
  
  ## learning rate annealing: learning_rate shrinks by 1% after every tree 
  ## (use 1.00 to disable, but then lower the learning_rate)
  learn_rate_annealing = 0.99,                                               
  
  ## sample 80% of rows per tree
  sample_rate = 0.8,                                                       
  ## sample 80% of columns per split
  col_sample_rate = 0.8, 
  
  ## fix a random number generator seed for reproducibility
  seed = 1234,                                                             
  
  ## early stopping once the validation logloss doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5,
  stopping_tolerance = 1e-4,
  stopping_metric = "logloss", 
  
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10                                                
)
## by default, display the grid search results sorted by increasing logloss (since this is a classification task)
grid        

## sort the grid models by decreasing increasing logloss
sortedGrid <- h2o.getGrid(grid_id, sort_by="logloss", decreasing = FALSE)    
sortedGrid

## find the range of max_depth for the top 5 models
topDepths = sortedGrid@summary_table$max_depth[1:5]                       
minDepth = min(as.numeric(topDepths))
maxDepth = max(as.numeric(topDepths))
minDepth
maxDepth

hyper_params = list( 
  ## restrict the search to the range of max_depth established above
  max_depth = seq(minDepth,maxDepth,1),                                      
  
  ## search a large space of row sampling rates per tree
  sample_rate = seq(0.2,1,0.01),                                             
  
  ## search a large space of column sampling rates per split
  col_sample_rate = seq(0.2,1,0.01),                                          
  
  ## search a large space of column sampling rates per tree
  col_sample_rate_per_tree = seq(0.2,1,0.01),                                
  
  ## search a large space of how column sampling per split should change as a function of the depth of the split
  col_sample_rate_change_per_level = seq(0.9,1.1,0.01),                      
  
  ## search a large space of the number of min rows in a terminal node
  min_rows = 2^seq(0,log2(nrow(train.h2o))-1,1),                              
  
  ## search a large space of the number of bins for split-finding for continuous and integer columns
  nbins = 2^seq(4,10,1),                                                     
  
  ## search a large space of the number of bins for split-finding for categorical columns
  nbins_cats = 2^seq(4,12,1),                                                
  
  ## search a few minimum required relative error improvement thresholds for a split to happen
  min_split_improvement = c(0,1e-8,1e-6,1e-4),                               
  
  ## try all histogram types (QuantilesGlobal and RoundRobin are good for numeric columns with outliers)
  histogram_type = c("UniformAdaptive","QuantilesGlobal","RoundRobin")       
)

search_criteria = list(
  ## Random grid search
  strategy = "RandomDiscrete",      
  
  ## limit the runtime to 120 minutes
  max_runtime_secs = 7200,         
  
  ## build no more than 500 models
  max_models = 1000,                  
  
  ## random number generator seed to make sampling of parameter combinations reproducible
  seed = 1234,                        
  
  ## early stopping once the leaderboard of the top 5 models is converged to 0.1% relative difference
  stopping_rounds = 5,                
  stopping_metric = "logloss",
  stopping_tolerance = 1e-3
)

grid_id = paste0("final_grid_", format(Sys.time(), "%Y%m%d_%H%M%S"))

grid <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params,
  
  ## hyper-parameter search configuration (see above)
  search_criteria = search_criteria,
  
  ## which algorithm to run
  algorithm = "gbm",
  
  ## identifier for the grid, to later retrieve it
  grid_id = grid_id,
  
  ## standard model parameters
  x = x, 
  y = y, 
  weights_column = "ipw",
  training_frame = train.h2o, 
  nfolds = 5,
  
  ## more trees is better if the learning rate is small enough
  ## use "more than enough" trees - we have early stopping
  ntrees = 10000,                                                            
  
  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
  learn_rate = 0.05,                                                         
  
  ## learning rate annealing: learning_rate shrinks by 1% after every tree 
  ## (use 1.00 to disable, but then lower the learning_rate)
  learn_rate_annealing = 0.99,                                               
  
  ## early stopping based on timeout (no model should take more than 1 hour - modify as needed)
  max_runtime_secs = 10800,                                                 
  
  ## early stopping once the validation logloss doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "logloss", 
  
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10,                                                
  
  ## base random number generator seed for each model (automatically gets incremented internally for each model)
  seed = 1234,
  
  ## These are important for creating stacked ensembles (see https://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/stacked-ensembles.html)
  keep_cross_validation_predictions = TRUE,
  fold_assignment = "Modulo"
)

# Train a stacked ensemble using the GBM grid
ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                training_frame = train.h2o,
                                base_models = grid@model_ids,
                                weights_column = "ipw")

# Eval ensemble performance 
perf <- h2o.performance(ensemble, newdata = train.h2o)

# Compare to base learner performance on the test set
.getlogloss <- function(mm) h2o.logloss(h2o.performance(h2o.getModel(mm), newdata = train.h2o))
baselearner_loglosss <- sapply(grid@model_ids, .getlogloss)
baselearner_best_logloss_test <- min(baselearner_loglosss)
ensemble_logloss_test <- h2o.logloss(perf)
print(sprintf("Best Base-learner Test logloss:  %s", baselearner_best_logloss_test))
print(sprintf("Ensemble Test logloss:  %s", ensemble_logloss_test))

# Generate predictions on the data from ensemble
pred <- h2o.predict(ensemble, newdata = train.h2o)
pred <- as.data.frame(pred)

#Add predictions to dataframe
df_model$pred <- pred$p1

# Save model
h2o.saveModel(ensemble, "ensemble model - nsc retention")
h2o.saveModelDetails(ensemble, "ensemble model details - nsc retention")
mojo_destination <- h2o.save_mojo(ensemble, "model mojo")

## Save the updated dataset with predictions to an Excel file
write.xlsx(df_model, "data (with predictions) - gbm plus.xlsx", overwrite = TRUE)

## Generate a Partial Dependence Plot (PDP) for the variable "hs_gpa" (as an example) in the best model
#partial_plot <- h2o.partialPlot(object = ensemble, data = train.h2o, cols = "hs_gpa")

#Analyze individual prediction models
sortedGrid <- h2o.getGrid(grid_id, sort_by = "logloss", decreasing = FALSE)    
s <- sortedGrid@summary_table

#Get best model
gbm <- h2o.getModel(sortedGrid@model_ids[[1]])
print(h2o.logloss(h2o.performance(gbm, newdata = train.h2o)))

## Obtain variable importance and feature interactions from the best model
vars <- as.data.frame(h2o.varimp(gbm)) # Variable importance
ints <- h2o.feature_interaction(gbm)   # Variable interactions
ints1 <- as.data.frame(ints[2]) 

## Save variable importance and feature interactions to separate Excel files
write.xlsx(vars, "most imp variables - gbm plus.xlsx")
write.xlsx(ints1, "most imp interactions - gbm plus.xlsx")

# Retrieve the model performance
perf <- h2o.performance(gbm, train.h2o)
perf_e <- h2o.performance(ensemble, train.h2o)
perf_e

# Save top model
h2o.saveModel(gbm, "model gbm plus - nsc retention")
h2o.saveModelDetails(gbm, "model details gbm plus- nsc retention")
mojo_destination <- h2o.save_mojo(gbm, "model gbm plus mojo")

###############################################################
#Reload university/college data (from College Scorecard)
###############################################################
#Import College Scorecard data
c <- read.xlsx("data for code/college scorecard.xlsx")

#Rename column
colnames(c)[4] = "name"

#Select columns
c <- c %>%
  select(ope8_id, name, main_campus,
         location.lat, location.lon,
         carnegie_basic, carnegie_undergrad, carnegie_size_setting,
         minority_serving.historically_black, minority_serving.predominantly_black, minority_serving.hispanic,
         men_only, women_only, religious_affiliation,
         sat_scores.midpoint.math, sat_scores.midpoint.critical_reading,
         admission_rate.overall,
         demographics.race_ethnicity.white:demographics.race_ethnicity.non_resident_alien,
         part_time_share,
         tuition.in_state, tuition.out_of_state,
         instructional_expenditure_per_fte, faculty_salary,
         ft_faculty_rate,
         pell_grant_rate,
         share_firstgeneration,
         demographics.female_share,
         demographics.over_23_at_entry,
         demographics.median_family_income,
         demographics.share_white.home_ZIP,
         demographics.share_bachelors_degree_age25.home_ZIP)

# Replace "NULL" with NA
c[c == "NULL"] <- NA

# Count number of missing datapoints for each school
c <- c %>%
  mutate(na = rowSums(is.na(.)))

# Keep row with fewest missing datapoints for each ope8 ID
cc <- c %>%
  group_by(ope8_id) %>%
  summarise(fewest_nas = min(na),
            n = n()) 

# Merge cc with c
c <- merge(c, cc,
           by = "ope8_id",
           all.x = TRUE)

# Filter rows with fewest NAs
c <- c %>%
  filter(na == fewest_nas)

# Convert selected columns to factors and numeric types
columns_to_factor <- c("ope8_id", "main_campus", "carnegie_basic", "carnegie_size_setting", "carnegie_undergrad")

columns_to_numeric <- c("location.lat", "location.lon", "sat_scores.midpoint.critical_reading", "sat_scores.midpoint.math", "admission_rate.overall", 
                        "demographics.race_ethnicity.white", "demographics.race_ethnicity.black", "demographics.race_ethnicity.hispanic", 
                        "demographics.race_ethnicity.asian", "demographics.race_ethnicity.non_resident_alien", "part_time_share", "tuition.in_state", 
                        "tuition.out_of_state", "instructional_expenditure_per_fte", "faculty_salary", "ft_faculty_rate", "pell_grant_rate", 
                        "share_firstgeneration", "demographics.female_share", "demographics.median_family_income", "demographics.over_23_at_entry", 
                        "demographics.share_bachelors_degree_age25.home_ZIP", "demographics.share_white.home_ZIP")

c <- c %>% 
  mutate(across(columns_to_factor, as.factor),
         across(columns_to_numeric, as.numeric))

# # Convert ope8_id in cc to factor
# cc$ope8_id <- as.factor(cc$ope8_id)

########################################################################
#####Make Model Predictions (by Institution)                          ##
########################################################################
# Create list of colleges and their ope8_ids (for model predictions)
colleges <- list("wentworth" = as.factor("002225-00"),
                 "umass_boston" = as.factor("002222-00"),
                 "umass_lowell" = as.factor("002161-00"),
                 "umass_amherst" = as.factor("002221-00"),
                 "roger_williams" = as.factor("003410-00"),
                 "umass_dartmouth" = as.factor("002210-00"),
                 "rensselaer" = as.factor("002803-00"),
                 "rochester" = as.factor("002806-00"),
                 "northeastern" = as.factor("002199-00"),
                 "uconn" = as.factor("001417-00"),
                 "wpi" = as.factor("002233-00"),
                 "u_new_hampshire" = as.factor("002589-00"),
                 "u_maine" = as.factor("002053-00"),
                 "u_rhode_island" = as.factor("003414-00"),
                 "suffolk" = as.factor("002218-00"),
                 "western_new_england" = as.factor("002226-00"),
                 "merrimack" = as.factor("002120-00"))

# Define function to predict college using trained model and input data
predict_college <- function(college_name, ope8_id, df_model, c, gbm, df) {
  
  # Add ope8_id to df_model
  df_model_college <- df_model %>%
    mutate(nsc_first_college_attended = ope8_id)
  
  # Convert ope8_id to factor
  df_model_college$nsc_first_college_attended <- as.factor(df_model_college$nsc_first_college_attended)
  
  # Find common and unique columns between df_model_college and c
  common_cols <- intersect(names(df_model_college), names(c))
  unique_cols <- setdiff(names(df_model_college), common_cols)
  
  # Remove common columns from df_model_college
  df_model_college <- df_model_college[, !names(df_model_college) %in% common_cols]
  
  # Filter c to get the row with matching ope8_id
  cc <- c %>%
    filter(ope8_id == ope8_id)
  
  # Merge df_model_college with cc by ope8_id
  df_model_college <- merge(df_model_college, cc,
                            by.x = "nsc_first_college_attended",
                            by.y = "ope8_id",
                            all.x = TRUE)
  
  # Convert df_model_college to h2o format
  train_college.h2o <- as.h2o(df_model_college)
  
  # Make predictions using gbm model
  pred_college <- h2o.predict(gbm, train_college.h2o, type="probs")
  pred_college <- as.data.frame(pred_college)
  
  # Rename prediction column and remove unnecessary columns
  colnames(pred_college)[3] <- paste0(college_name,"_pred")
  pred_college <- pred_college %>% select(-predict,-p0)
  
  # Combine prediction with original data frame
  df <- cbind(df, pred_college)
  
  # Return updated data frame
  return(df)
}

# Loop through colleges and predict using predict_college function
for (college in names(colleges)) {
  df <- predict_college(college, colleges[[college]], df_model, c, gbm, df)
}

write.xlsx(df,"model predictions (by institution) - gbm plus.xlsx")



########################################################################
#####Examine Model Bias                                               ##
########################################################################

Create code that runs through all variables in df model and runs linear models examining bias for each variable
