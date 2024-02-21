library(readxl)
cancer <- read_excel("C:/Users/Jaxon/Downloads/Medical Insurance Charge.xlsx")
View(cancer)
###################################################################################

#1
summary(cancer)

#2
quantitative_variables <- c("Age", "Calories", "Fat")

quantitative_data <- cancer[, quantitative_variables]

summary_stats <- summary(quantitative_data)

print(summary_stats)



#3
hist(cancer$Calories, xlab = "Calories", main = "Histogram of Calories")
boxplot(cancer$Calories, horizontal = T)

hist(cancer$Age, xlab = "Age", main = "Histogram of Age")
boxplot(cancer$Age,horizontal = T)

hist(cancer$Fat, xlab = "Fat", main = "Histogram of Fat")
boxplot(cancer$Fat, horizontal = T)

#4
agecat <- ifelse(cancer$Age < 40, "0-39", 
                        ifelse(cancer$Age <= 50, "40-50",
                               ifelse(cancer$Age <= 60, "51-60",
                                      "61+")))

# Frequency Table
frequency <- table(cancer$agecat)
prop <- prop.table(frequency)


# Print frequency table
print(frequency)


#Pie Chart

pie(frequency, labels = frequency, main = "Pie Chart of Age Groups")


# Create a bar chart
library(ggplot2)

ggplot(cancer, aes(x = agecat)) +
  geom_bar(fill = "lavender", color = "black") +
  labs(title = "Distribution of Age Categories",
       x = "Age Category",
       y = "Frequency") +
  theme_minimal()


########################
#5 
table(cancer$Gender, cancer$SmokeStat)

contingency_table <- table(cancer$Gender, cancer$SmokeStat)

# Calculate percentages of total
percent_total <- round(prop.table(contingency_table) * 100, digits = 2)

# Calculate percentages of row
percent_row <- round(prop.table(contingency_table, margin = 1) * 100, digits = 2)

# Calculate percentages of column
percent_column <- round(prop.table(contingency_table, margin = 2) * 100, digits = 2)

# Print the contingency table with percentages
print("Contingency Table with Counts:")
print(contingency_table)

print("\nPercentage of Total:")
print(percent_total)

print("\nPercentage of Row:")
print(percent_row)

print("\nPercentage of Column:")
print(percent_column)

###########################################################

#6
counts <- round(prop.table(table(cancer$SmokeStat, cancer$Gender), 1), digits = 3)
counts_df <- as.data.frame(counts)
names(counts_df) <- c("Smoke", "Gender", "Proportion")

library(ggplot2)
library(scales)  

ggplot(data = counts_df, aes(x = Gender, y = Proportion, fill = Smoke, label = paste0(Proportion * 100, "%"))) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(position = position_fill(vjust = 0.5), size = 4) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of Smoking Status by Gender",
       x = "Gender",
       y = "Proportion",
       fill = "Smoke Status") +
  theme_minimal() +
  theme(legend.position = "bottom")

######################################################################
#7
mean_age_by_gender <- aggregate(Age ~ Gender, data = cancer, FUN = mean)

round(print(mean_age_by_gender), digits = 2)


#######################################################
#8

#Used ChatGTP to help me with this question


set.seed(123)


sample_data <- cancer[sample(nrow(cancer), 70), ]


sample_mean <- mean(sample_data$Calories)
sample_sd <- sd(sample_data$Calories)


standard_error <- sample_sd / sqrt(70)


margin_of_error <- qt(0.975, df = 69) * standard_error


lower_bound <- sample_mean - margin_of_error
upper_bound <- sample_mean + margin_of_error

cat("95% Confidence Interval for Calories: (", lower_bound, ",", upper_bound, ")\n")

####################################################################
#9
ggplot(cancer, aes(x = Age, y = Calories)) +
  geom_point() +
  labs(title = "Scatterplot of Calories vs Age",
       x = "Age",
       y = "Calories") +
  theme_minimal()

########################################

boxplot(cancer$Age ~ cancer$Calories, horizontal = TRUE, col = "lightblue", xlab = "Age", ylab = "Calories")
boxplot(cancer$Calories ~ cancer$Age, horizontal = TRUE, col = "lightblue", xlab = "Calories", ylab = "Age")

##############################################

aggregate(cancer$calories~cancer$Age,FUN=mean)
aggregate(cancer$Calories~cancer$Age,FUN=summary)

round(aggregate(Calories ~ Age, data = cancer, FUN = mean), digits =2)
round(aggregate(Calories ~ Age, data = cancer, FUN = summary), digits = 2)

############################################3
set.seed(12345)  
cancer_samp <- cancer[sample(1:312,70,replace=F),]
View(cancer_samp)

CI <- function(x, alpha = .05, dec = 3){ 
  n <- sum(!is.na(x))
  conf_level <- (1-alpha)*100
  me <- qt(1-alpha/2, n-1)*sd(x, na.rm=T)/sqrt(n)
  lower <- round(mean(x, na.rm = T) - me, digits = dec)
  upper <- round(mean(x, na.rm = T) + me, digits = dec)
  mean <- round(mean(x,na.rm = T), digits = dec)
  {limits <-data.frame(cbind(variable = 
                               deparse(substitute(x)), n, c.level = conf_level, mean, 
                             me = round(me, digits = dec), lower, upper))}
  print(limits) 
  rm(n, conf_level, lower, upper, mean)}

CI(cancer_samp$Calories,dec=2)

####################################################
#Used ChatGTP to help me clean up the code
cancer$Age_Group <- cut(cancer$Age, breaks = seq(0, max(cancer$Age) + 10, by = 10))


age_calories <- aggregate(Calories ~ Age_Group, data = cancer, FUN = mean)  # You can use 'mean' or 'sum' here


ggplot(age_calories, aes(x = Age_Group, y = Calories)) +
  geom_bar(stat = "identity", fill = "peachpuff") +
  labs(title = "Mean Calories by Age Groups",
       x = "Ages",
       y = "Mean Calories") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
