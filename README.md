Getting and Cleaning Data Course Project
================

## How the script works

This README explains all of the code from the run\_analysis.R. The
script works as follows.

### Reading the raw files

The script reads in 6 files as follows:

  - *activity\_labels.txt*
  - *features.txt*
  - *subject\_test.txt*
  - *subject\_train.txt*
  - *X\_test.txt*
  - *X\_train.txt*
  - *y\_test.txt*
  - *y\_train.txt*

<!-- end list -->

``` r
library(tidyverse)

path_to_data <- "getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset"

activity_labels <- read.table(paste0(path_to_data, "/activity_labels.txt")) %>% 
  setNames(c("class_labels", "activity"))

features <- read.table(paste0(path_to_data, "/features.txt")) %>%
  setNames(c("feature_id", "feature_name"))


read_datasets <- function (dataset_type) {
  
  path <- paste(path_to_data, dataset_type, sep = "/")
  file_names <- grep("*.txt", list.files(path), value = TRUE)
  data_list <- map(paste(path, file_names, sep = "/"), read.table) %>%
                setNames(sub(paste0("_", dataset_type, ".txt"), "", file_names)) 
  
  data_list[order(names(data_list))]
}

train <- read_datasets("train") 
test <- read_datasets("test")
```

### Merging the data

The raw data is shaped as follows:

``` r
head(activity_labels)
```

    ##   class_labels           activity
    ## 1            1            WALKING
    ## 2            2   WALKING_UPSTAIRS
    ## 3            3 WALKING_DOWNSTAIRS
    ## 4            4            SITTING
    ## 5            5           STANDING
    ## 6            6             LAYING

``` r
head(features)
```

    ##   feature_id      feature_name
    ## 1          1 tBodyAcc-mean()-X
    ## 2          2 tBodyAcc-mean()-Y
    ## 3          3 tBodyAcc-mean()-Z
    ## 4          4  tBodyAcc-std()-X
    ## 5          5  tBodyAcc-std()-Y
    ## 6          6  tBodyAcc-std()-Z

``` r
lapply(train, dim)
```

    ## $subject
    ## [1] 7352    1
    ## 
    ## $X
    ## [1] 7352  561
    ## 
    ## $y
    ## [1] 7352    1

``` r
lapply(test, dim)
```

    ## $subject
    ## [1] 2947    1
    ## 
    ## $X
    ## [1] 2947  561
    ## 
    ## $y
    ## [1] 2947    1

First we merge by row the test and train datasets for *subject*, *X*,
and *y*, which are just different sets of observations.

``` r
merged_rows <- mapply(rbind, train, test)
```

Then we clean up each of the *subject*, *X*, and *y* datasets and join
them with the activity and feature datasets, including only *mean()* and
*std()* variables for the latter as per the assignment instructions.

``` r
activity_data <- merged_rows$y %>% 
  left_join(activity_labels, by = c("V1" = "class_labels")) %>% select(-V1)

features_filtered <- features %>% 
  filter(grepl("std[(][])]|mean[(][)]", feature_name))

measurement_data <- merged_rows$X[,as.vector(features_filtered$feature_id)] %>% 
  setNames(as.vector(features_filtered$feature_name))

subject_data <- merged_rows$subject %>% set_names("subject")
```

The final data consists of these 3 cleaned components merged by column
into one dataframe.

``` r
data <- cbind(subject_data,activity_data, measurement_data)
```

### The Tidy Dataset

Lastly, we transform the full data set into a tidy dataset, where each
case consists of the average for each activity/subject.

``` r
tidy_dataset <- data %>% 
  gather(measurement, value, -c(activity, subject)) %>% 
  extract(measurement, c("variable"), "(std[(][)]|mean[(][)])", remove = FALSE) %>% 
  mutate(variable = sub("[(][)]","", variable), measurement = sub("-", "_",sub("-mean[(][)]","", sub("-std[(][)]","", measurement)))) %>% 
  group_by(activity, subject, measurement, variable) %>% 
  summarise(mean_value = mean(value))  %>% 
  ungroup() %>% 
  spread(measurement, mean_value) 

tidy_dataset
```

    ## # A tibble: 360 x 36
    ##    activity subject variable fBodyAcc_X fBodyAcc_Y fBodyAcc_Z fBodyAccJerk_X
    ##    <fct>      <int> <chr>         <dbl>      <dbl>      <dbl>          <dbl>
    ##  1 LAYING         1 mean         -0.939     -0.867     -0.883         -0.957
    ##  2 LAYING         1 std          -0.924     -0.834     -0.813         -0.964
    ##  3 LAYING         2 mean         -0.977     -0.980     -0.984         -0.986
    ##  4 LAYING         2 std          -0.973     -0.981     -0.985         -0.987
    ##  5 LAYING         3 mean         -0.981     -0.961     -0.968         -0.981
    ##  6 LAYING         3 std          -0.984     -0.964     -0.963         -0.983
    ##  7 LAYING         4 mean         -0.959     -0.939     -0.968         -0.979
    ##  8 LAYING         4 std          -0.952     -0.946     -0.962         -0.980
    ##  9 LAYING         5 mean         -0.969     -0.965     -0.977         -0.983
    ## 10 LAYING         5 std          -0.965     -0.973     -0.966         -0.986
    ## # … with 350 more rows, and 29 more variables: fBodyAccJerk_Y <dbl>,
    ## #   fBodyAccJerk_Z <dbl>, fBodyAccMag <dbl>, fBodyBodyAccJerkMag <dbl>,
    ## #   fBodyBodyGyroJerkMag <dbl>, fBodyBodyGyroMag <dbl>, fBodyGyro_X <dbl>,
    ## #   fBodyGyro_Y <dbl>, fBodyGyro_Z <dbl>, tBodyAcc_X <dbl>, tBodyAcc_Y <dbl>,
    ## #   tBodyAcc_Z <dbl>, tBodyAccJerk_X <dbl>, tBodyAccJerk_Y <dbl>,
    ## #   tBodyAccJerk_Z <dbl>, tBodyAccJerkMag <dbl>, tBodyAccMag <dbl>,
    ## #   tBodyGyro_X <dbl>, tBodyGyro_Y <dbl>, tBodyGyro_Z <dbl>,
    ## #   tBodyGyroJerk_X <dbl>, tBodyGyroJerk_Y <dbl>, tBodyGyroJerk_Z <dbl>,
    ## #   tBodyGyroJerkMag <dbl>, tBodyGyroMag <dbl>, tGravityAcc_X <dbl>,
    ## #   tGravityAcc_Y <dbl>, tGravityAcc_Z <dbl>, tGravityAccMag <dbl>
