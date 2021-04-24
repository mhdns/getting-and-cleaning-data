# Import necessary packages
library(dplyr)
library(readr)
library(tidyr)

# Check if "data" folder exists and create one if not
if (!file.exists("data")) {
    dir.create("data")
}
setwd("data")
# Download data and store them in  "data" folder

zip_file_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zip_file <- "HAR.zip"
download.file(zip_file_url, zip_file)

## Unzip the data and cleanup folder
unzip(zip_file)
file.remove(zip_file)

rm(zip_file, zip_file_url)
setwd("..")

# Define paths for loading data
raw_data_path <- file.path(getwd(), "data", "UCI HAR Dataset")
train_data_path <- file.path(raw_data_path, "train")
test_data_path <- file.path(raw_data_path, "test")

train_subject_data_path <- file.path(train_data_path, "subject_train.txt")
train_features_data_path <- file.path(train_data_path, "X_train.txt")
train_label_data_path <- file.path(train_data_path, "y_train.txt")

test_subject_data_path <- file.path(test_data_path, "subject_test.txt")
test_features_data_path <- file.path(test_data_path, "X_test.txt")
test_label_data_path <- file.path(test_data_path, "y_test.txt")

feature_names_path <- file.path(raw_data_path, "features.txt")
activity_labels_path <- file.path(raw_data_path, "activity_labels.txt")

# Load data and column names
feature_names <- read.table(feature_names_path)[[2]]
activity_labels <- read.table(activity_labels_path)

subject_train_data <- read.table(train_subject_data_path)
subject_test_data <- read.table(test_subject_data_path)
subject_data <- rbind(subject_train_data, subject_test_data)
names(subject_data) <- "subject"

features_train_data <- read.table(train_features_data_path)
features_test_data <- read.table(test_features_data_path)
features_data <- rbind(features_train_data, features_test_data)
colnames(features_data) <- feature_names

labels_train_data <- read.table(train_label_data_path)
labels_test_data <- read.table(test_label_data_path)
labels_data <- rbind(labels_train_data, labels_test_data)
names(labels_data) <- "label"

# Environment clean up
rm(raw_data_path, train_data_path, test_data_path, test_subject_data_path,
   test_features_data_path, test_label_data_path, train_subject_data_path,
   train_features_data_path, train_label_data_path, feature_names_path,
   activity_labels_path, subject_train_data, subject_test_data,
   features_train_data, features_test_data, labels_train_data,
   labels_test_data, feature_names)

# Remove unwanted columns and keep only mean and std
features_data <- features_data[,grepl("mean()|std()", colnames(features_data))]

# Replace labels with activity
labels_data <- as_tibble(labels_data)
activity_labels <- as_tibble(activity_labels)
activity_data <- left_join(labels_data, activity_labels,
                           by= c("label" = "V1"))[2]
names(activity_data) <- "activity"

# Merge the columns
dataset <- cbind(subject_data, activity_data, features_data)

# Environment clean up
rm(activity_labels, labels_data, activity_data, features_data, subject_data)

# Save cleaned data set into csv file - tidy.csv
write.csv(dataset, file.path(getwd(), 
                             "data", "tidy.csv"), row.names = F)

# Group the data to get mean of the variables for each subject and activity
dataset <- as_tibble(dataset)
grouped_dataset <- group_by(dataset, subject, activity) %>%
    summarise_all(mean)

# Change column names to better represent the variables in grouped dataset
new_names <- vector()

for (i in colnames(grouped_dataset)) {
    if (i == "subject" | i == "activity") {
        new_names <- c(new_names, i)
    } else {
        new_names <- c(new_names, paste("avg_", i))
    }
}

new_names <- gsub(" ", "", new_names)

colnames(grouped_dataset) <- new_names

# Environment clean up
rm(i, new_names)
# Save grouped data into csv file - grp_tidy.csv
write.csv(grouped_dataset, 
          file.path(getwd(), "data", "grp_tidy.csv"), row.names = F)
