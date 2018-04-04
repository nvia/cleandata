
library(data.table)

## 1. Merges the training and the test sets to create one data set
## The data is read and converted into a single data frame

features <- read.csv('./UCI HAR Dataset/features.txt', header = FALSE, sep = ' ')
features <- as.character(features[,2])

train_x <- read.table('./UCI HAR Dataset/train/X_train.txt')
train_y <- read.csv('./UCI HAR Dataset/train/y_train.txt', header = FALSE, sep = ' ')
train_subject <- read.csv('./UCI HAR Dataset/train/subject_train.txt',header = FALSE, sep = ' ')

data_train <-  data.frame(train_subject, train_y, train_x)
names(data_train) <- c(c('subject', 'activity'), features)

test_x <- read.table('./UCI HAR Dataset/test/X_test.txt')
test_y <- read.csv('./UCI HAR Dataset/test/y_test.txt', header = FALSE, sep = ' ')
test_subject <- read.csv('./UCI HAR Dataset/test/subject_test.txt', header = FALSE, sep = ' ')

data_test <-  data.frame(test_subject, test_y, test_x)
names(data_test) <- c(c('subject', 'activity'), features)

data_all <- rbind(data_train, data_test)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement
mean_std <- grep('mean|std', features)
data_sub <- data_all[,c(1,2,mean_std + 2)]

# 3. Uses descriptive activity names to name the activities in the data set
act_labels <- read.table('./UCI HAR Dataset/activity_labels.txt', header = FALSE)
act_labels <- as.character(act_labels[,2])
data_sub$activity <- act_labels[data_sub$activity]
data_sub

# 4. Appropriately labels the data set with descriptive variable names
name_new <- names(data_sub)
name_new <- gsub("[(][)]", "", name_new)
name_new <- gsub("^t", "TimeDomain_", name_new)
name_new <- gsub("^f", "FrequencyDomain_", name_new)
name_new <- gsub("Acc", "Accelerometer", name_new)
name_new <- gsub("Gyro", "Gyroscope", name_new)
name_new <- gsub("Mag", "Magnitude", name_new)
name_new <- gsub("-mean-", "_Mean_", name_new)
name_new <- gsub("-std-", "_StandardDeviation_", name_new)
name_new <- gsub("-", "_", name_new)
names(data_sub) <- name_new
data_sub

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
data <- aggregate(data_sub[,3:81], by = list(activity = data_sub$activity, subject = data_sub$subject),FUN = mean)
write.table(x = data, file = "data.txt", row.names = FALSE)
