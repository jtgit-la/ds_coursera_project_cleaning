library(tidyverse)

# paths to folders ----------

path_to_data <- "getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset"


# read files: activity labels and features 

activity_labels <- read.table(paste0(path_to_data, "/activity_labels.txt")) %>% 
  setNames(c("class_labels", "activity"))

features <- read.table(paste0(path_to_data, "/features.txt")) %>%
  setNames(c("feature_id", "feature_name"))


# read files: training and test datasets 

read_datasets <- function (dataset_type) {
  
  path <- paste(path_to_data, dataset_type, sep = "/")
  file_names <- grep("*.txt", list.files(path), value = TRUE)
  data_list <- map(paste(path, file_names, sep = "/"), read.table) %>%
                setNames(sub(paste0("_", dataset_type, ".txt"), "", file_names)) 
  
  data_list[order(names(data_list))]
}

train <- read_datasets("train") 
test <- read_datasets("test")


# merge and join all files into one dataframe 

merged_rows <- mapply(rbind, train, test)

activity_data <- merged_rows$y %>% 
  left_join(activity_labels, by = c("V1" = "class_labels")) %>% select(-V1)

features_filtered <- features %>% 
  filter(grepl("std[(][])]|mean[(][)]", feature_name))

measurement_data <- merged_rows$X[,as.vector(features_filtered$feature_id)] %>% 
  setNames(as.vector(features_filtered$feature_name))

subject_data <- merged_rows$subject %>% set_names("subject")

# fully merged and joined dataframe 
data <- cbind(subject_data,activity_data, measurement_data)


# create tidy dataset 

tidy_dataset <- data %>% 
  gather(measurement, value, -c(activity, subject)) %>% 
  extract(measurement, c("variable"), "(std[(][)]|mean[(][)])", remove = FALSE) %>% 
  mutate(variable = sub("[(][)]","", variable), measurement = sub("-", "_",sub("-mean[(][)]","", sub("-std[(][)]","", measurement)))) %>% 
  group_by(activity, subject, measurement, variable) %>% 
  summarise(mean_value = mean(value))  %>% 
  ungroup() %>% 
  spread(measurement, mean_value) 
 
  



