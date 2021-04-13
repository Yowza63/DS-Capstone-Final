##################################################################################################################
# Project DS-Capstone-Final for HarvardX -  PH125.9x, Data Science: Capstone
# Becky Johnson / Github: Yowza63
# R code to generate data set and analysis to predict county level death rate from COVID-19 in the US
##################################################################################################################

# ---------------------------------------------------------------------------------------------------------------
#  SETUP: Load the libraries needed for the data exploration and model fitting
# ---------------------------------------------------------------------------------------------------------------
# Data manipulation tools
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")

# String manipulation tools
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")

# Tools to help create tidy dat
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")

# Graphing tools
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")

# Formatting for graphs
if(!require(hrbrthemes)) install.packages("hrbrthemes", repos = "http://cran.us.r-project.org")

# Machine learning tools
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# Regression Tree tools
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")

# Visualize the regression trees
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")

# Random Forest package
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

# Extend the basic functionality of tables produced using knitr::kable()
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")

# Package of data sets, functions to go along with the book "Data Visualization: A Practical Introduction" (Princeton University Press, 2019)
if(!require(socviz)) install.packages("socviz", repos = "http://cran.us.r-project.org")

# A graphical display of the correlation matrix
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")

# Some extra themes for graphing
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")

# Various tools (quantcut is used here)
if(!require(gtools)) install.packages("gtools", repos = "http://cran.us.r-project.org")

# ---------------------------------------------------------------------------------------------------------------
# STEP 1: Create the Data set
# Create a data set of various health and socioeconomic variables for every county in the US and add
# COVID-19 data for cases, deaths, and mask usage
# ---------------------------------------------------------------------------------------------------------------

# STEP 1a: Census Data
# The data has been captured from census.gov and previously stored in the project Github repository.
# To download from census, I followed the following steps:
#   1. Paste the link below into a Chrome browser
# https://api.census.gov/data/2019/acs/acs5/profile?get=NAME,DP03_0119PE,DP05_0017PE,DP05_0016PE,DP03_0096PE,DP03_0097PE,DP02_0016E,DP04_0046PE,DP05_0038PE,DP05_0039PE,DP05_0071PE&for=county:*
#   2. Save Page As a .csv changing the file type to "All Files"

# Download prepared file from my Github repository
dl <- tempfile()
download.file("https://raw.githubusercontent.com/Yowza63/DS-Capstone-Final/main/census.csv", dl)
dat <- read.csv(dl)

# Drop the "X" column
dat <- select(dat, -"X")

# Change the column names to be meaningful
names(dat) <- 
  c("name", "poverty", "over85", "age74to85", "insured", "private_insured", "household_size", 
    "owner_occ", "african_am", "native_am", "hispanic", "state", "county")

# Remove any "[" or "]" from the name and county fields
dat$name <- gsub("\\[|\\]", "", dat$name)
dat$county <- gsub("\\[|\\]", "", dat$county)

# Create a fips code field made up of the state and county identifiers
dat <- dat %>% tidyr::unite("fips", state:county, sep = "", remove = FALSE)
dat$fips <- strtoi(dat$fips)

# Reorder the columns
dat <- dat[, c("name", "fips", "state", "county", "poverty", "over85", "age74to85", "insured", 
  "private_insured", "household_size", "owner_occ", "african_am", "native_am", "hispanic")]

# STEP 1b: Incorporate the New York Times COVID-19 cases and deaths to compute the death rate by county
# This file was downloaded from the New York Times site and then stored in my Github repository
# download.file("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv", "us-counties-recent.csv")
# The New York Times files are updated daily so I needed to save one to repeatability of my analysis

# Download New York Times file from  Github repository
dl_nyt <- tempfile()
download.file("https://raw.githubusercontent.com/Yowza63/DS-Capstone-Final/main/us-counties-recent.csv", dl_nyt)
dat_nyt <- as.data.frame(read.csv(dl_nyt)) # load the data 
dat_nyt <- dat_nyt %>% dplyr::filter(date == "2021-04-01") # keep only the most recent data

# Examine the rows with NA values in the fips field. This highlights the issue with the New York City data
# These rows will be discarded when we join to the census data
dat_nyt[which(is.na(dat_nyt$fips)),]
nrow(dat_nyt[which(is.na(dat_nyt$fips)),])

# Add a death_rate column in dat_nyt
dat_nyt$death_rate <- dat_nyt$deaths/dat_nyt$cases

# Keep only columns we need
dat_nyt <- select(dat_nyt, fips, cases, deaths, death_rate)

# Add the COVID-19 data to dat using inner_join() to keep only the fips codes that are in both tables. 
dat <- inner_join(dat, dat_nyt, by = "fips")

# STEP 1c: Incorporate the New York Times mask use information
# https://github.com/nytimes/covid-19-data
# download.file("https://raw.githubusercontent.com/nytimes/covid-19-data/master/mask-use/mask-use-by-county.csv", "mask-use.csv")

# Download stored file from  Github repository
dl_mask <- tempfile()
download.file("https://raw.githubusercontent.com/Yowza63/DS-Capstone-Final/main/mask-use.csv", dl_mask)
mask <- as.data.frame(read.csv(dl_mask)) # load the data 
mask <- rename(mask, fips = COUNTYFP)
# add two summary columns for "good" and "bad" mask compliance
mask$good = mask$FREQUENTLY + mask$ALWAYS
mask$bad = mask$NEVER + mask$RARELY
# keep just these new columns
mask <- mask %>% select(fips, good, bad)

# Add the mask data to dat
dat <- inner_join(dat, mask, by = "fips")

# STEP 1d: Add county level health statistics from the University of Wisconsin Population Health Institute
# To download this file, I followed the following steps:
# Choose - "2020 CHR CSV Analytic Data", https://www.countyhealthrankings.org/explore-health-rankings/rankings-data-documentation
# Documentation can be found here - https://www.countyhealthrankings.org/sites/default/files/media/document/2020%20Analytic%20Documentation_0.pdf

# Download the previously downloaded file stored on my Github repository
dl_chr <- tempfile()
download.file("https://raw.githubusercontent.com/Yowza63/DS-Capstone-Final/main/analytic_data2020_0.csv", dl_chr)
chr <- read.csv(dl_chr)

# keep only columns we need
chr <- chr %>% select(fips = X5.digit.FIPS.Code,
  poor_health = Poor.physical.health.days.raw.value,
  smoking = Adult.smoking.raw.value, 
  obesity = Adult.obesity.raw.value, 
  sedentary = Physical.inactivity.raw.value,
  flu_vac = Flu.vaccinations.raw.value, 
  air_quality = Air.pollution...particulate.matter.raw.value, 
  overcrowding = Percentage.of.households.with.overcrowding,
  diabetes = Diabetes.prevalence.raw.value, 
  sleep = Insufficient.sleep.raw.value, 
  doctors = Ratio.of.population.to.primary.care.providers.other.than.physicians.)

# Convert fips to integer
chr$fips = as.integer(chr$fips)

# For these variables we want to join for all available fips codes
dat <- left_join(dat, chr, by = "fips")

# Convert fields to numeric or integer
dat$poor_health = as.numeric(dat$poor_health)
dat$household_size = as.numeric(dat$household_size)
dat$smoking = as.numeric(dat$smoking)
dat$obesity = as.numeric(dat$obesity)
dat$sedentary = as.numeric(dat$sedentary)
dat$flu_vac = as.numeric(dat$flu_vac)
dat$air_quality = as.numeric(dat$air_quality)
dat$overcrowding = as.numeric(dat$overcrowding)
dat$diabetes = as.numeric(dat$diabetes)
dat$sleep = as.numeric(dat$sleep)
dat$doctors = as.numeric(dat$doctors)

# For "doctors", reset negative values and NA's to the mean
dat$doctors[which(dat$doctors < 0)] <- 0
dat$doctors[which(is.na(dat$doctors) == TRUE)] <- 0
dat$doctors[which(dat$doctors == 0)] <- mean(dat$doctors[which(dat$doctors > 0)])

# Replace "flu_vac" NA's with the mean
dat$flu_vac[which(is.na(dat$flu_vac) == TRUE)] <- mean(dat$flu_vac[which(is.na(dat$flu_vac) == FALSE)])

# Replace "air_quality" NA's with the mean
dat$air_quality[which(is.na(dat$air_quality) == TRUE)] <- mean(dat$air_quality[which(is.na(dat$air_quality) == FALSE)])

# ---------------------------------------------------------------------------------------------------------------
# STEP 2: Explore the data
# ---------------------------------------------------------------------------------------------------------------

# Show the min, max, and quartile for each variable
summary(dat)

# The number of counties in the data set
nrow(dat)

# Look at counties with the top 10 highest death rates
tmp <- dat %>% select(name, death_rate, african_am, native_am, hispanic, age74to85)
tmp <- top_n(tmp, 10, wt = death_rate)
kbl(tmp[with(tmp, order(-death_rate)),], booktabs = T) %>% kable_styling(latex_options = "striped")

# Look at counties with the top 10 highest case rates
tmp <- dat %>% select(name, death_rate, cases,  african_am, native_am, hispanic, age74to85)
tmp <- top_n(tmp, 10, wt = cases)
kbl(tmp[with(tmp, order(-cases)),], booktabs = T) %>% kable_styling(latex_options = "striped")

# Draw a map of the us showing counties color coded by the quantiles of death_rate
# Source: https://socviz.co/maps.html
# Copy the county_map data and create a numeric fips column, join to dat, and create quantiles
tmp <- county_map %>% mutate(fips = as.numeric(id)) %>% left_join(., dat, by = "fips") %>%
  mutate(death_rate_tier = quantcut(death_rate, q = 4))

p <- ggplot(data = tmp, mapping = aes(x = long, y = lat, fill = death_rate_tier, group = group))
p1 <- p + geom_polygon(color = "gray90", size = 0.05) + coord_equal()
p2 <- p1 + scale_fill_brewer(palette="Greens", 
  labels = c("0-1.16%", "1.16-1.70%", "1.70-2.34%", "2.34-10.50%", "Missing Data"))
p2 + labs(fill = "COVID-19 Death Rate (Deaths/Cases)") +
  guides(fill = guide_legend(nrow = 1)) + 
  theme_map() + theme(legend.position = "bottom") + 
  labs(title = "Quantiles of COVID-19 Death Rates (Deaths/Cases)")

# Examine correlation across variables by creating a chart of correlation across variables
tmp <- dat %>% select(-name, -county, -state, -fips)
M <-cor(tmp)
corrplot(M, method = "square", order = "FPC", type = "upper", 
         tl.col = "black", tl.srt = 45, tl.cex = .8, 
         title = "Correlation Across Variables")

# Histogram of death rates by county
dat %>% ggplot(aes(x=death_rate)) + 
  geom_histogram(binwidth = .005, fill = "#69b3a2", col = "black") + 
  xlab("Death Rate") +
  ylab("Number of Counties") + 
  labs(title = "County Level Death Rates (Deaths/Cases") + 
  theme_ipsum()

# ---------------------------------------------------------------------------------------------------------------
# STEP 3: Split the data into train (70%), test (30%), and validation (15%)
# ---------------------------------------------------------------------------------------------------------------

# remove columns from dat not needed for modeling
tmp <- dat%>% select(-"name", -"state", -"county")

# set the seed for replicability
set.seed(755)

# create an index to split the data into a train (70%) and tmp (30%)
# then split tmp into test (15%) and validation (15%)

# create the first index for the train_set / tmp_set split
test_index <- createDataPartition(y = tmp$fips, times = 1, p = 0.3, list = FALSE) 
# split the data into a training set and testing set
train_set <- tmp[-test_index,]
tmp_set <- tmp[test_index,]

# create the second index for the validation_set / test_set split
test_index <- createDataPartition(y = tmp_set$fips, times = 1, p = 0.5, list = FALSE) 
# split the data into a training set and testing set
validation_set <- tmp_set[-test_index,]
test_set <- tmp_set[test_index,]

# ---------------------------------------------------------------------------------------------------------------
# STEP 4: Build the model
# ---------------------------------------------------------------------------------------------------------------

# STEP 4a: DEFINE THE LOSS FUNCTION TO MEASURE THE MODEL EFFECTIVENESS
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# STEP 4b: THE SIMPLEST MODEL
just_the_average <- RMSE(test_set$death_rate, mean(train_set$death_rate))

# Create a table to store the results and add this simple model to it
rmse_results <- 
  tibble("Method" = "Just the Average", RMSE = just_the_average)
kbl(rmse_results, booktabs = T) %>% kable_styling(latex_options = "striped")

# STEP 4c: A LINEAR REGRESSION MODEL
lm_model <- lm(death_rate ~ obesity + poverty + age74to85 + air_quality, data = train_set)
pred <- predict(lm_model, test_set)
rmse_results <- bind_rows(rmse_results, 
  tibble("Method" = "Linear Regression - Obesity, Poverty, Age, Air Quality", 
         RMSE = RMSE(pred, test_set$death_rate)))
kbl(rmse_results, booktabs = T) %>% kable_styling(latex_options = "striped")

# STEP 4d: A REGRESSION TREE MODEL
# Source: "Regression Trees" tutorial found here: http://uc-r.github.io/regression_trees

# Start with a simple regression tree
tree1 <- rpart(
  formula = death_rate ~ .,
  data    = train_set,
  method  = "anova"
)

# examine the model
tree1

# visualize the model
rpart.plot(tree1)

# plot the error rates by tree size
plotcp(tree1)

# Predict the results and add to the table
pred <- predict(tree1, test_set)
rmse_results <- bind_rows(rmse_results, 
      tibble("Method" = "Simple Regression Tree", RMSE = RMSE(pred, test_set$death_rate)))
kbl(rmse_results, booktabs = T) %>% kable_styling(latex_options = "striped")

# Create a regression tree model with bootstrap aggregating or "bagging" with the Caret package
# Specify 10-fold cross validation
ctrl <- trainControl(method = "cv",  number = 10) 

# Cross validation bagged model
bagged_cv <- train(
  death_rate ~ .,
  data = train_set,
  method = "treebag",
  trControl = ctrl,
  importance = TRUE
)

# assess results
bagged_cv

# plot most important variables
plot(varImp(bagged_cv), 20)  

# Evaluate on the test_set, add to the results table and print the table
pred <- predict(bagged_cv, test_set)
rmse_results <- bind_rows(rmse_results, 
        tibble("Method" = "Cross Validated Bagged Tree", RMSE = RMSE(pred, test_set$death_rate)))
kbl(rmse_results, booktabs = T) %>% kable_styling(latex_options = "striped")

# STEP 4e: RANDOM FOREST MODEL
# Source: "Random Forests" tutorial found here: https://uc-r.github.io/random_forests
# Create a tuned Random Forest model using the tuneRF from the randomForest package

# Create a list of features which is the column names without "death_rate"
features <- setdiff(names(train_set), "death_rate")

# Run the tuning process, this takes a few seconds
set.seed(123)
rf_tune <- tuneRF(
  x          = train_set[features],
  y          = train_set$death_rate,
  ntreeTry   = 500,
  mtryStart  = 2,
  stepFactor = 1.5,
  improve    = 0.001,
  trace      = FALSE      # to not show real-time progress 
)

# Find the best mtry for our model
rf_tune <- as.data.frame(rf_tune)
best_mtry <- rf_tune$mtry[which(rf_tune$OOBError == min(rf_tune$OOBError))]

# Create a model with the results of tuning
rf <- randomForest::randomForest(
  formula = death_rate ~ .,
  data    = train_set,
  mtry = best_mtry
)

# Look at the model and plot the errors associated with the different number of trees
rf
plot(rf)

# Show the most important variables
varImpPlot(rf, n.var = 10)

# Find the number of trees with lowest MSE
which.min(rf$mse)

# RMSE of the best random forest
sqrt(rf$mse[which.min(rf$mse)])

# Predict the results and add to our table
pred <- predict(rf, test_set)
rmse_results <- bind_rows(rmse_results, 
  tibble("Method" = "Random Forest with Tuning", RMSE = RMSE(pred, test_set$death_rate)))
kbl(rmse_results, booktabs = T) %>% kable_styling(latex_options = "striped")

# ---------------------------------------------------------------------------------------------------------------
# STEP 5: Evaluate the final model on the holdout validation set
# ---------------------------------------------------------------------------------------------------------------

# Use the validation set to confirm the results of the models

# Predict the results for the simple average and add to our table
just_the_average <- RMSE(validation_set$death_rate, mean(validation_set$death_rate))
rmse_results <- bind_rows(rmse_results, 
  tibble("Method" = "Validation of Just the Average", RMSE = just_the_average))

# Predict the results for the cross validation bagged tree on the validation set and add to our table
pred <- predict(bagged_cv, validation_set)
rmse_results <- bind_rows(rmse_results, 
  tibble("Method" = "Validation of Cross Validated Bagged Tree", RMSE = RMSE(pred, validation_set$death_rate)))

# Predict the results for the random forest model on the validation set and add to our table
pred <- predict(rf, validation_set)
rmse_results <- bind_rows(rmse_results, 
  tibble("Method" = "Validation of Random Forest with Tuning", RMSE = RMSE(pred, validation_set$death_rate)))

# Show the final table
kbl(rmse_results, booktabs = T) %>% kable_styling(latex_options = "striped")

