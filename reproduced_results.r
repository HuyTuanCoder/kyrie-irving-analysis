# Load the data from the csv file
# Please download the csv files using the link in README and change the file path accordingly
kyrie_stat <- read.csv("~/Downloads/kyrie_irving_stat.csv", header = TRUE, sep = ",")
kyrie_playoffs <- read.csv("~/Downloads/kyrie_playoffs_stat.csv", header = TRUE, sep = ",")

# Check the current state of the data before deciding on what to do
head(kyrie_stat)
str(kyrie_stat)
nrow(kyrie_stat)

head(kyrie_playoffs)
str(kyrie_playoffs)
nrow(kyrie_playoffs)

# Select the columns that will be used in analysis
kyrie_stat <- kyrie_stat[, c("Date", "Age", "MP", "AST", "TRB", "TOV", "PTS")]

# Check for missed games of Kyrie in the dataset
did_not_dress_count <- sum(kyrie_stat$MP == "Did Not Dress")
inactive_count <- sum(kyrie_stat$MP == "Inactive")
did_not_play_count <- sum(kyrie_stat$MP == "Did Not Play")
not_with_team_count <- sum(kyrie_stat$MP == "Not With Team")

# Remove all the games that Kyrie did not play
kyrie_stat <- kyrie_stat[kyrie_stat$MP != "Did Not Dress", ]
kyrie_stat <- kyrie_stat[kyrie_stat$MP != "Inactive", ]
kyrie_stat <- kyrie_stat[kyrie_stat$MP != "Did Not Play", ]
kyrie_stat <- kyrie_stat[kyrie_stat$MP != "Not With Team", ]

# Turn all data into the right format (everything is currently characters)
kyrie_stat$Age <- as.numeric(sapply(strsplit(as.character(kyrie_stat$Age), "-"), function(x) {
  year <- as.numeric(x[1])  # Extract year
  days <- as.numeric(x[2])  # Extract days
  year + days / 365  # Convert days to fraction of a year
}))

kyrie_stat$MP <- as.numeric(sapply(strsplit(as.character(kyrie_stat$MP), ":"), function(x) {
  minutes <- as.numeric(x[1])  # Extract minutes
  seconds <- as.numeric(x[2])  # Extract seconds
  minutes + seconds / 60  # Convert seconds to fraction of a minute
}))

kyrie_stat$AST <- as.numeric(kyrie_stat$AST)
kyrie_stat$TRB <- as.numeric(kyrie_stat$TRB)
kyrie_stat$TOV <- as.numeric(kyrie_stat$TOV)
kyrie_stat$PTS <- as.numeric(kyrie_stat$PTS)
kyrie_stat$Date <- as.Date(kyrie_stat$Date)

# Now we do the same for the statistics of Kyrie playoffs
# Notes that for this dataset, age is not inside the dataset

kyrie_playoffs <- kyrie_playoffs[, c("Date", "MP", "AST", "TRB", "TOV", "PTS")]

did_not_dress_count2 <- sum(kyrie_playoffs$MP == "Did Not Dress")
inactive_count2 <- sum(kyrie_playoffs$MP == "Inactive")
did_not_play_count2 <- sum(kyrie_playoffs$MP == "Did Not Play")
not_with_team_count2 <- sum(kyrie_playoffs$MP == "Not With Team")

kyrie_playoffs <- kyrie_playoffs[kyrie_playoffs$MP != "Did Not Dress", ]
kyrie_playoffs <- kyrie_playoffs[kyrie_playoffs$MP != "Inactive", ]
kyrie_playoffs <- kyrie_playoffs[kyrie_playoffs$MP != "Did Not Play", ]
kyrie_playoffs <- kyrie_playoffs[kyrie_playoffs$MP != "Not With Team", ]

kyrie_playoffs$MP <- as.numeric(sapply(strsplit(as.character(kyrie_playoffs$MP), ":"), function(x) {
  minutes <- as.numeric(x[1])  # Extract minutes
  seconds <- as.numeric(x[2])  # Extract seconds
  minutes + seconds / 60  # Convert seconds to fraction of a minute
}))

kyrie_playoffs$AST <- as.numeric(kyrie_playoffs$AST)
kyrie_playoffs$TRB <- as.numeric(kyrie_playoffs$TRB)
kyrie_playoffs$TOV <- as.numeric(kyrie_playoffs$TOV)
kyrie_playoffs$PTS <- as.numeric(kyrie_playoffs$PTS)

# Data Overview


# Box Plot and related information
boxplot(kyrie_stat$PTS, 
        main = "Box Plot of Points (PTS)", 
        ylab = "Points", 
        col = "lightgreen", 
        border = "black"
)

quantile(kyrie_stat$PTS, 0.25)
quantile(kyrie_stat$PTS, 0.75)

# Histogram and related information
hist(kyrie_stat$PTS, 
     breaks = 20,
     col = "skyblue",
     border = "white",
     probability = TRUE,
     main = "Histogram and Density Curve of Points (PTS)", 
     xlab = "Points", 
     ylab = "Density")

pts_density <- density(kyrie_stat$PTS)
lines(pts_density, col = "red", lwd = 2) # Add density curve

median(kyrie_stat$PTS)
mean(kyrie_stat$PTS)


pairs(kyrie_stat[, c("Age", "MP", "AST", "TRB", "TOV", "PTS")],
      pch = 19,
)

# Mean Sampling
set.seed(123)

num_samples <- 1000
sample_size <- 50

sample_means <- numeric(num_samples)

for(i in 1:num_samples) {
  # Take a random sample of the specified size
  sample_data <- sample(kyrie_stat$PTS, size = sample_size, replace = TRUE)
  # Calculate the mean of the sample and store it
  sample_means[i] <- mean(sample_data, na.rm = TRUE)
}

summary(sample_means)
hist(sample_means, main = "Distribution of Sample Means", xlab = "Sample Mean", col = "skyblue", border = "white")

# QQ - Plot
qqnorm(kyrie_stat$PTS, main = "QQ Plot of Kyrie's Points")
qqline(kyrie_stat$PTS, col = "red") 

# In-dept Analysis

# One sample t-test
t.test(kyrie_stat$PTS, conf.level = 0.9)

# Two sample t-test

# Filter the initial kyrie data to only show date after 
filtered_data <- kyrie_stat[kyrie_stat$Date > "2014-10-30", ]

# test with full data set
t_test_full_greater <- t.test(filtered_data$PTS, kyrie_playoffs$PTS, alternative = "greater")
print(t_test_full_greater)

# test with a random data set with the same size as the smaller one
set.seed(123)
random_sample <- sample(filtered_data$PTS, size = 52, replace = TRUE)
t_test_sample_greater <- t.test(random_sample, kyrie_playoffs$PTS, alternative = "greater")
print(t_test_sample_greater)


# Linear Regression
kyrie_stat_rounded_age <- kyrie_stat 
kyrie_stat_rounded_age$Age <- floor(kyrie_stat_rounded_age$Age)

lm_model <- lm(PTS ~ Age + TRB + MP + AST + TOV, data = kyrie_stat_rounded_age)
summary(lm_model)

stepwise_model <- step(lm_model, direction = "backward")
summary(stepwise_model)


















