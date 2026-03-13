#Group AN
#Kenneth Nathaniel Wong TP078930

df <- read.csv("D:\\Univ\\Year 2\\Sem 1\\Programming For Data Analysis\\Assignment\\UNSW-NB15_uncleaned.csv")
library(VIM)
library(dplyr)
library(ggplot2)

dim(df)
ncol(df)
nrow(df)
summary(df)
str(df)
colSums(is.na(df))
colnames(df)

#Check if there are any unknown symbol values in the dataset
colSums(df == "?", na.rm = T)
colSums(df == "", na.rm = T)
colSums(df == "NA", na.rm = T)

# Convert to numeric
df$spkts <- as.numeric(df$spkts)
df$dpkts <- as.numeric(df$dpkts)
df$sbytes <- as.numeric(df$sbytes)
df$dbytes <- as.numeric(df$dbytes)
df$sload <- as.numeric(df$sload)
df$dload <- as.numeric(df$dload)

class(df$spkts)
class(df$dpkts)
class(df$sbytes)
class(df$dbytes)
class(df$sload)
class(df$dload)

# Numeric subset
df_numeric <- df[, c("spkts","dpkts","sbytes","dbytes","sload","dload")]

# Remove rows where ALL numeric values are NA
df <- df[rowSums(is.na(df_numeric)) != 6, ]
df_numeric <- df[, c("spkts","dpkts","sbytes","dbytes","sload","dload")]

# Separate complete rows
df_numeric_complete <- df_numeric[complete.cases(df_numeric), ]

# Scale complete rows only
df_scaled <- scale(df_numeric_complete)

# K-means clustering on complete rows
set.seed(123)
clusters <- kmeans(df_scaled, centers = 20, nstart = 10)

# Create cluster column
df$cluster <- NA
df$cluster[complete.cases(df_numeric)] <- clusters$cluster

# Scale full dataset (needed to compute distances for missing rows)
df_scaled_all <- scale(df_numeric)

# Assign cluster to rows that originally had missing values
na_rows <- which(is.na(df$cluster))
for (j in na_rows) {
  dist_vec <- rep(0, nrow(clusters$centers))
  for (k in 1:nrow(clusters$centers)) {
    dist_vec[k] <- sum((df_scaled_all[j, ] - clusters$centers[k, ])^2, na.rm = TRUE)
  }
  df$cluster[j] <- which.min(dist_vec)
}

# Perform KNN inside each cluster
df_clean <- df
for(i in unique(df$cluster)) {
  cat("Imputing cluster:", i, "of", length(unique(df$cluster)), "\n")
  
  cluster_rows <- df$cluster == i
  df_cluster <- df_clean[cluster_rows, c("spkts","dpkts","sbytes","dbytes","sload","dload")]
  
  df_cluster_knn <- kNN(df_cluster, k = 5)
  
  df_clean[cluster_rows, c("spkts","dpkts","sbytes","dbytes","sload","dload")] <-
    df_cluster_knn[, c("spkts","dpkts","sbytes","dbytes","sload","dload")]
}

# Remove helper columns created by kNN
df_clean <- df_clean[, !grepl("_imp$", names(df_clean))]

# Final check (should all be zero)
colSums(is.na(df_clean[, c("spkts","dpkts","sbytes","dbytes","sload","dload")]))
write.csv(df_clean, "df_clean_final.csv", row.names = FALSE)

#Checking for duplication of data
library(dplyr)
df_clean <- distinct(df_clean)
nrow(df_clean)
nrow(df)
sum(duplicated(df))

#Data visualization
missing_before <- colSums(is.na(df_numeric))
missing_before
missing_after <- colSums(is.na(df_clean[, c("spkts","dpkts","sbytes","dbytes","sload","dload")]))
missing_after

#Data frame for plotting it into a graph
missing_compare <- data.frame(
  Feature = c("spkts","dpkts","sbytes","dbytes","sload","dload"),
  Before = as.numeric(missing_before),
  After = as.numeric(missing_after)
)

missing_compare

#Bar chart to compare before and after cleaning of NA values 
ggplot() +
  geom_bar(data = missing_compare,
           aes(x = Feature, y = Before, fill = "Before"),
           stat="identity", position="dodge") +
  geom_bar(data = missing_compare,
           aes(x = Feature, y = After, fill = "After"),
           stat="identity", position="dodge") +
  labs(title="Missing Values Before vs After Cleaning",
       x="Feature", y="Number of Missing Values",
       fill="Status") +
  scale_fill_manual(values=c("Before"="red", "After"="blue"))


#How does it contribute to Attack category column
df_clean$attack_cat <- as.factor(df_clean$attack_cat)
#Affect of sbytes to attack_cat
ggplot(df_clean, aes(x = attack_cat, y = sbytes)) +
  geom_boxplot() +
  labs(title="Source Bytes vs Attack Category",
       x="Attack Category", y="sbytes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Affect of dbytes to attack_cat
ggplot(df_clean, aes(x = attack_cat, y = dbytes)) +
  geom_boxplot() +
  labs(title="Destination Bytes vs Attack Category",
       x="Attack Category", y="dbytes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Affect of sload to attack
ggplot(df_clean, aes(x = attack_cat, y = sload)) +
  geom_boxplot() +
  labs(title="Source Bits per Second vs Attack Category",
       x="Attack Category", y="sload") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Affect of dload to attack
ggplot(df_clean, aes(x = attack_cat, y = dload)) +
  geom_boxplot() +
  labs(title="Destination Bits per Second vs Attack Category",
       x="Attack Category", y="dload") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Affect of spkts to attack
ggplot(df_clean, aes(x = attack_cat, y = spkts)) +
  geom_boxplot() +
  labs(title="Source to Desination Packet Count vs Attack Category",
       x="Attack Category", y="spkts") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Affect of dpkts to attack
ggplot(df_clean, aes(x = attack_cat, y = dpkts)) +
  geom_boxplot() +
  labs(title="Destination to Source Packet Count vs Attack Category",
       x="Attack Category", y="dpkts") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Extra Features
#Implementing prediction modelling using Random Forest
library(randomForest)
library(caret)

# Ensure attack_cat is a factor
df_clean$attack_cat <- as.factor(df_clean$attack_cat)

# Use only rows with valid attack labels
df_model <- df_clean[!is.na(df_clean$attack_cat), ]

# Select numeric features
features <- df_model[, c("spkts","dpkts","sbytes","dbytes","sload","dload")]

# Label to predict
labels <- df_model$attack_cat

set.seed(123)

# Train-test split (70% training, 30% test)
train_index <- sample(1:nrow(features), 0.7 * nrow(features))
train_data <- features[train_index, ]
test_data <- features[-train_index, ]
train_labels <- labels[train_index]
test_labels <- labels[-train_index]

# Train Random Forest model
rf_model <- randomForest(x = train_data, y = train_labels, ntree = 200)

# Predict attack category
rf_pred <- predict(rf_model, test_data)

# Accuracy score
accuracy_rf <- mean(rf_pred == test_labels)
accuracy_rf

# Confusion Matrix
conf_matrix_rf <- confusionMatrix(rf_pred, test_labels)
conf_matrix_rf
