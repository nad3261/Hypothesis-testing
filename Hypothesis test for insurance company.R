#Removing unnecessary columns
train$PassengerId <- NULL
train$Name <- NULL
train$Cabin <- NULL
train$Embarked <- NULL
train$Parch <- NULL
train$Fare <- NULL
train$Ticket <- NULL


# Replacing NA Values with mean
for (col in colnames(train)) {
  if (is.numeric(train[[col]])) {

    mean_value <- mean(train[[col]], na.rm = TRUE)
    

    train[[col]][is.na(train[[col]])] <- mean_value
  }
}

# Proportion of survival
prop_survived <- prop.table(table(train$Survived)) * 100
labels_with_percent <- paste0(c("Not Survived", "Survived"), ": ", round(prop_survived, 1), "%")
pie(prop_survived, 
    labels = labels_with_percent,
    col = c("salmon", "lightgreen"),
    main = "Survival Proportion")


# Distribution of Class,Age and Sex and Number of siblings/spouses aboard the Titanic

selected_columns <- c("Age", "Pclass", "Sex","SibSp")

par(mfrow = c(2, 2)) 
for (col in selected_columns) {
  if (col %in% colnames(train)) {
    if (is.numeric(train[[col]])) {
      hist(train[[col]], main = paste("Histogram of", col), xlab = col, col = '#3357FF')
    } else {
      barplot(table(train[[col]]), main = paste("Bar Plot of", col), xlab = col, col = '#3357FF')
    }
  }
}
par(mfrow = c(1, 1))

# hecking for outliers in age and their removal
Q1 <- quantile(train$Age, 0.25, na.rm = TRUE)
Q3 <- quantile(train$Age, 0.75, na.rm = TRUE)
IQR_value <- IQR(train$Age, na.rm = TRUE)
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value
train <- train[train$Age >= lower_bound & train$Age <= upper_bound, ]

# Ensuring that outliers for age are removed
boxplot(train$Age, main = "Boxplot of Age", col = '#3357FF', ylab = "Age")
abline(h = median(train$Age, na.rm = TRUE), col = "red", lty = 2)

#Check of a first hypothesis:
#H0: Mean age of survived people is equal to those, who didn't survive
#H1:Mean age of survived people is not equal to those, who didn't survive

# Visualizing dependency of age and survival
boxplot(train$Age ~ train$Survived, 
        main = "Boxplot of Age by Survival",
        xlab = "Survived",
        ylab = "Age",
        col = c("salmon", "lightgreen"),
        names = c("Not Survived", "Survived"))  



t_test_result <- t.test(Age ~ Survived, data = train)
print(t_test_result)
#Since the p-value = 0,6897, greater then significance level(0.05),we fail 
#to reject null hypothesis, meaning age does not significantly impact survival


#Check of second hypothesis
#H0:Survival rates are the same across all passenger classes
#H1:At least one survival rate is different across passenger classes
# Visualizing dependency of pclass and survival

boxplot(train$Pclass ~ train$Survived,
        main = "Boxplot of Pclass by Survival",
        xlab = "Survived",
        ylab = "Passenger Class (Pclass)",
        col = c("salmon", "lightgreen"),
        names = c("Not Survived", "Survived"))

anova_result <- aov(Survived ~ factor(Pclass), data = train)
summary(anova_result)
#P value is extremely small(2e-16). This means that H0 is rejected, and there is 
#at least one mean class which extremely different accross survival groups


#Check of third hypothesis
#H0: There is no relationship between Sex and the chance of Survival
#H1: There is a relationship between Sex and the chance of Survival

# Visualizing dependency of sex and survival
barplot(table(train$Survived, train$Sex),
        beside = TRUE,
        main = "Survival Count by Sex",
        xlab = "Survived",
        ylab = "Count",
        col = c("salmon", "lightgreen"),
        names.arg = c("Not Survived", "Survived"),
        legend.text = c("Female", "Male"),
        args.legend = list(x = "topright", bty = "n"))

train$Sex <- factor(train$Sex)
logistic_model <- glm(Survived ~ Sex, data = train, family = binomial)
summary(logistic_model)

#H0 is rejected as P-value is very low(7,61e-15).The coefficient is negative and significant
#It means that being a male will significantly decrease survival chance

#Check of fourth hypothesis
#H0:Survival rate are same across different number of Sibsp
#H1:Survival rate are not the same across different number of Sibsp

train$SibSp <- factor(train$SibSp)
boxplot(Survived ~ SibSp, data = train,
        main = "Boxplot of Survival by SibSp",
        xlab = "Number of Siblings/Spouses Aboard (SibSp)",
        ylab = "Survived",
        col = c("salmon", "lightgreen"),
        names = levels(train$SibSp),
        at = 1:length(levels(train$SibSp)),
        ylim = c(-0.5, 1.5))

contingency_table <- table(train$SibSp, train$Survived)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)
#P value is less then significance level(0.05)
#H0 is rejected, meaning there is a relationship between the mber of siblings/spouses aboard and the survival status.