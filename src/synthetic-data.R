library("lme4")

set.seed(123) # reproducibility

# define waters and respondents
waters <- LETTERS[1:5]  # A-E
respondents <- c("Alice", "Bob", "Carl", "Dana", "Ethan", 
                 "Fay", "Grace", "Hugo", "Ivy", "Jack")

# Create all combinations
data_processed <- expand.grid(water = waters, respondent = respondents)

# Assign a "true" underlying mean rating per water (E is best, A is worst)
water_means <- c(A=-5, B=0, C=3, D=-2, E=9)

# Generate ratings: true mean plus noise, rounded to integer within 1-10
data_processed$rating <- mapply(function(water) {
  round(rnorm(1, mean = water_means[water], sd = 3))
}, data_processed$water)

# Make sure ratings are within 1 to 10
data_processed$rating <- pmax(-10, pmin(10, data_processed$rating))

# Add descriptive words correlated to rating
data_processed$words <- ifelse(data_processed$rating >= 8, "luscious, crystalline",
                               ifelse(data_processed$rating >= 4, "refreshing, clear",
                                      ifelse(data_processed$rating >= -4, "mediocre, neutral",
                                             "briny, unpleasant")))

data_processed <- as.data.frame(data_processed) |>
  arrange(water)

save(data_processed, file = "throughput/processed-data-TEST.RData")


