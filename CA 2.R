# Import the World-happiness data frame
world_happiness_data <- read.csv("World-happiness.csv", na = "")

# Check for missing vars and examine missing data
# List rows with missing values
incomplete_data <- world_happiness_data[!complete.cases(world_happiness_data),]
incomplete_data

# Remove any rows that contain NA using listwise deletion
world_happiness_data <- na.omit(world_happiness_data)
world_happiness_data

# Create a subset of the world happiness dataset to only look at the data from the year 2015
attach(world_happiness_data)
new_data <- subset(world_happiness_data, year == "2015")
new_data
world_happiness_data <- new_data

# Check Model Assumptions
# Linearity: There is a linear relation among the variables
# Normality: Residuals are normally distributed
# Homoscedasticity: Residuals have constant variance
# No collinearity: Variables are not linear combinations of each other
# Independence: Residuals are independent or at least not correlated

# Check Linearity

# Correlation Test 1
# Independent variable (x-axis) = Freedom to Make Life Choices
# Dependent variable (y-axis) = Life Ladder

attach(world_happiness_data)
scatter.smooth(x = Freedom.to.make.life.choices, 
               y = Life.Ladder, 
               main = "Life Ladder ~ Freedom to Make Life Choices", 
               xlab = "Freedom to Make Life Choices", ylab = "Life Ladder")

# Check numerically the correlation of these variables
# Values of -0.2 < x < 0.2 - low correlation

cor(Life.Ladder, Freedom.to.make.life.choices) 
# Correlation value = 0.5257293
# The correlation test shows that the correlation between the life ladder variable and  
# the freedom to make life choices variable is 0.5257293 indicating a medium correlation.

# Correlation Test 2
# Independent variable (x-axis) = Social Support
# Dependent variable (y-axis) = Life Ladder

attach(world_happiness_data)
scatter.smooth(x = Social.support, 
               y = Life.Ladder, 
               main = "Life Ladder ~ Social Support", 
               xlab = "Social Support", ylab = "Life Ladder")

# Check numerically the correlation of these variables

cor(Life.Ladder, Social.support)
# Correlation value = 0.7399498
# The correlation test shows that the correlation between the life ladder variable and  
# social support variable is 0.7399498 indicating a high correlation.

# correlation test 3
# Independent variable (x-axis) = Perceptions of Corruption
# Dependent variable (y-axis) = Life Ladder

attach(world_happiness_data)
scatter.smooth(x = Perceptions.of.corruption, 
               y = Life.Ladder, 
               main = "Life Ladder ~ Perceptions of Corruption", 
               xlab = "Perceptions of Corruption", ylab = "Life Ladder")

# Check numerically the correlation of these variables

cor(Life.Ladder, Perceptions.of.corruption) 
# Correlation value = -0.4638415
# The correlation test shows that the correlation between the life ladder variable and  
# the perceptions of corruption variable is -0.4638415 indicating a negative medium correlation.

# correlation test 4
# Independent variable (x-axis) = Log GDP per Capita
# Dependent variable (y-axis) = Life Ladder

attach(world_happiness_data)
scatter.smooth(x = Log.GDP.per.capita, 
               y = Life.Ladder,
               main = "Life Ladder ~ Log GDP per Capita", 
               xlab = "Log GDP per Capita", ylab = "Life Ladder")

# Check numerically the correlation of these variables

cor(Life.Ladder, Log.GDP.per.capita) 
# Correlation value = 0.8213955
# The correlation test shows that the correlation between the life ladder variable and  
# the Log GDP per capita variable is 0.8213955 indicating a high correlation.

# correlation test 5
# Independent variable (x-axis) = Positive Affect
# Dependent variable (y-axis) = Life Ladder

attach(world_happiness_data)
scatter.smooth(x = Positive.affect, 
               y = Life.Ladder, 
               main = "Life Ladder ~ Positive Affect", 
               xlab = "Positive Affect", ylab = "Life Ladder")

# Check numerically the correlation of these variables

cor(Life.Ladder, Positive.affect) 
# Correlation value = 0.5626796
# The correlation test shows that the correlation between the life ladder variable and  
# the positive affect variable is 0.5626796 indicating a medium correlation.

# correlation test 6
# Independent variable (x-axis) = Negative Affect
# Dependent variable (y-axis) = Life Ladder

attach(world_happiness_data)
scatter.smooth(x = Negative.affect, 
               y = Life.Ladder, 
               main = "Life Ladder ~ Negative Affect", 
               xlab = "Negative Affect", ylab = "Life Ladder")

# Check numerically the correlation of these variables

cor(Life.Ladder, Negative.affect) 
# Correlation value = -0.3504786
# The correlation test shows that the correlation between the life ladder variable and  
# the negative affect variable is -0.3504786 indicating a negative medium correlation.

# correlation test 7
# Independent variable (x-axis) = Healthy Life Expectancy at birth
# Dependent variable (y-axis) = Life Ladder

attach(world_happiness_data)
scatter.smooth(x = Healthy.life.expectancy.at.birth, 
               y = Life.Ladder, 
               main = "Life Ladder ~ Healthy Life Expectancy at Birth", 
               xlab = "Healthy Life Expectancy at birth", ylab = "Life Ladder")

# Check numerically the correlation of these variables

cor(Life.Ladder, Healthy.life.expectancy.at.birth) 
# Correlation value = 0.7889729
# The correlation test shows that the correlation between the life ladder variable and  
# the healthy life expectancy at birth variable is 0.7889729 indicating a high correlation.

# correlation test 8
# Independent variable (x-axis) = Generosity
# Dependent variable (y-axis) = Life Ladder

attach(world_happiness_data)
scatter.smooth(x = Generosity, 
               y = Life.Ladder, 
               main = "Life Ladder ~ Generosity", 
               xlab = "Generosity", ylab = "Life Ladder")

# Check numerically the correlation of these variables

cor(Life.Ladder, Generosity) 
# Correlation value = 0.2144477
# The correlation test shows that the correlation between the life ladder variable and  
# the generosity variable is 0.2144477 indicating a low correlation.

paste("Correltion for Life.Ladder and Log GDP per Capita: ", cor(Life.Ladder, Log.GDP.per.capita))
paste("Correltion for Life.Ladder and Social Support: ", cor(Life.Ladder, Social.support))
paste("Correltion for Life.Ladder and Healthy Life Expectancy at Birth: ", cor(Life.Ladder, Healthy.life.expectancy.at.birth))
paste("Correltion for Life.Ladder and Freedom to Make Life Choices: ", cor(Life.Ladder, Freedom.to.make.life.choices))
paste("Correltion for Life.Ladder and Generosity: ", cor(Life.Ladder, Generosity))
paste("Correltion for Life.Ladder and Perceptions of Corruption: ", cor(Life.Ladder, Perceptions.of.corruption))
paste("Correltion for Life.Ladder and Positive Affect: ", cor(Life.Ladder, Positive.affect))
paste("Correltion for Life.Ladder and Negative Affect: ", cor(Life.Ladder, Negative.affect))

# Remove the generosity variable 
# The correlation between life ladder and generosity shows a low correlation of 0.2144477
# Therefore the generosity variable is removed from the list of variables used to build the model

# Check for outliers
opar <- par(no.readonly = TRUE)
attach(world_happiness_data)

boxplot(Life.Ladder, 
        main = "Life Ladder", 
        sub = paste("Outlier rows: ", boxplot.stats(Life.Ladder)$out))
boxplot(Log.GDP.per.capita, 
        main = "Log GDP per Capita",
        sub = paste("Outlier rows: ", boxplot.stats(Log.GDP.per.capita)$out))
boxplot(Social.support, 
        main = "Social Support",
        sub = paste("Outlier rows: ", boxplot.stats(Social.support)$out))
boxplot(Healthy.life.expectancy.at.birth, 
        main = "Healthy Life Expectancy at Birth",
        sub = paste("Outlier rows: ", boxplot.stats(Healthy.life.expectancy.at.birth)$out))
boxplot(Freedom.to.make.life.choices, 
        main = "Freedom to Make Life Choices",
        sub = paste("Outlier rows: ", boxplot.stats(Freedom.to.make.life.choices)$out))
boxplot(Perceptions.of.corruption, 
        main = "Perceptions of Corruption",
        sub = paste("Outlier rows: ", boxplot.stats(Perceptions.of.corruption)$out))
boxplot(Positive.affect, 
        main = "Positive Affect",
        sub = paste("Outlier rows: ", boxplot.stats(Positive.affect)$out))
boxplot(Negative.affect, 
        main = "Negative Affect",
        sub = paste("Outlier rows: ", boxplot.stats(Negative.affect)$out))
par <- opar

# use boxplot.stats to extract the outliers
outlier_values <- boxplot.stats(Life.Ladder)$out
paste("Life Ladder outliers: ", paste(outlier_values, collapse = ", "))

outlier_values <- boxplot.stats(Log.GDP.per.capita)$out
paste("Log GDP per Capita: ", paste(outlier_values, collapse = ", "))

outlier_values <- boxplot.stats(Social.support)$out
paste("Social Support outliers: ", paste(outlier_values, collapse = ", "))

outlier_values <- boxplot.stats(Healthy.life.expectancy.at.birth)$out
paste("Healthy Life Expectancy at Birth outliers: ", paste(outlier_values, collapse = ", "))

outlier_values <- boxplot.stats(Freedom.to.make.life.choices)$out
paste("Freedom to Make Life Choices outliers: ", paste(outlier_values, collapse = ", "))

outlier_values <- boxplot.stats(Perceptions.of.corruption)$out
paste("Perceptions of Corruption outliers: ", paste(outlier_values, collapse = ", "))

outlier_values <- boxplot.stats(Positive.affect)$out
paste("Positive Affect outliers: ", paste(outlier_values, collapse = ", "))

outlier_values <- boxplot.stats(Negative.affect)$out
paste("Negative Affect outliers: ", paste(outlier_values, collapse = ", "))

# Remove the outliers
world_happiness_data <- subset(world_happiness_data, 
                               Social.support != 0.434)
world_happiness_data <- subset(world_happiness_data, 
                               Perceptions.of.corruption != 0.357
                               & Perceptions.of.corruption != 0.191
                               & Perceptions.of.corruption != 0.223
                               & Perceptions.of.corruption != 0.375
                               & Perceptions.of.corruption != 0.186
                               & Perceptions.of.corruption != 0.299
                               & Perceptions.of.corruption != 0.095
                               & Perceptions.of.corruption != 0.099
                               & Perceptions.of.corruption != 0.232
                               & Perceptions.of.corruption != 0.21)
world_happiness_data <- subset(world_happiness_data, 
                               Negative.affect != 0.52
                               & Negative.affect != 0.581
                               & Negative.affect != 0.643)

# Check to see if the outliers have been removed
# Decide whether to delete a few more outliers by looking at the boxplots of the variables
attach(world_happiness_data)
boxplot(Social.support, 
        main = "Social Support", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Social.support)$out)) 

boxplot(Perceptions.of.corruption, 
        main = "Perceptions of Corruption", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Perceptions.of.corruption)$out)) 

boxplot(Negative.affect, 
        main = "Negative Affect", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Negative.affect)$out))                  

outlier_values <- boxplot.stats(Social.support)$out
paste("Social Support outliers: ", paste(outlier_values, collapse = ", "))

outlier_values <- boxplot.stats(Perceptions.of.corruption)$out
paste("Perceptions of Corruption outliers: ", paste(outlier_values, collapse = ", "))

outlier_values <- boxplot.stats(Negative.affect)$out
paste("Negative Affect outliers: ", paste(outlier_values, collapse = ", "))

# Remove more outliers from the perceptions of corruption variable
attach(world_happiness_data)
world_happiness_data <- subset(world_happiness_data, 
                               Perceptions.of.corruption != 0.469
                               & Perceptions.of.corruption != 0.427
                               & Perceptions.of.corruption != 0.412
                               & Perceptions.of.corruption != 0.409
                               & Perceptions.of.corruption != 0.412
                               & Perceptions.of.corruption != 0.456
                               & Perceptions.of.corruption != 0.471)
# Check to see if outliers have been removed
boxplot(Perceptions.of.corruption, 
        main = "Perceptions of Corruption", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Perceptions.of.corruption)$out)) 

# Check Normality
library(e1071)

# moderately skewed = -1 to -0.5 and 0.5 to 1
# highly skewed = < -1 or > 1
# -0.5 to 0.5 = approximately symmetrical

# Normality test for the variable life ladder
opar <- par(no.readonly = TRUE)
attach(world_happiness_data)
plot(density(Life.Ladder), 
     main = "Density plot : Life Ladder", 
     ylab = "Frequency", 
     xlab = "Life Ladder", sub = paste("Skewness : ", 
                                       round(e1071::skewness(Life.Ladder), 2)))
# Fill the area under the plot
polygon(density(Life.Ladder), col = "blue")

# Normality test for the variable Log GDP per capita
plot(density(Log.GDP.per.capita), 
     main = "Density plot : Log GDP per Capita", 
     ylab = "Frequency", 
     xlab = "Log GDP per Capita", sub = paste("Skewness : ", 
                                              round(e1071::skewness(Log.GDP.per.capita), 2)))
polygon(density(Log.GDP.per.capita), col = "blue")

# Normality test for the variable social support
plot(density(Social.support), 
     main = "Density plot : Social Support", 
     ylab = "Frequency", 
     xlab = "Social Support", sub = paste("Skewness : ", 
                                          round(e1071::skewness(Social.support), 2)))
polygon(density(Social.support), col = "blue")

# Normality test for the variable healthy life expectancy at birth
plot(density(Healthy.life.expectancy.at.birth), 
     main = "Density plot : Healthy Life Expectancy at Birth", 
     ylab = "Frequency", 
     xlab = "Healthy Life Expectancy at Birth", sub = paste("Skewness : ", 
                                                            round(e1071::skewness(Healthy.life.expectancy.at.birth), 2)))
polygon(density(Healthy.life.expectancy.at.birth), col = "blue")

# Normality test for the variable freedom to make life choices
plot(density(Freedom.to.make.life.choices), 
     main = "Density plot : Freedom to Make Life Choices", 
     ylab = "Frequency", 
     xlab = "Freedom to Make Life Choices", sub = paste("Skewness : ", 
                                                        round(e1071::skewness(Freedom.to.make.life.choices), 2)))
polygon(density(Freedom.to.make.life.choices), col = "blue")

# Normality test for the variable perceptions of corruption
plot(density(Perceptions.of.corruption), 
     main = "Density plot : Perceptions of Corruption", 
     ylab = "Frequency", 
     xlab = "Perceptions of Corruption", sub = paste("Skewness : ", 
                                                     round(e1071::skewness(Perceptions.of.corruption), 2)))
polygon(density(Perceptions.of.corruption), col = "blue")

# Normality test for the variable positive affect
plot(density(Positive.affect), 
     main = "Density plot : Positive Affect", 
     ylab = "Frequency", 
     xlab = "Positive Affect", sub = paste("Skewness : ", 
                                           round(e1071::skewness(Positive.affect), 2)))
polygon(density(Positive.affect), col = "blue")

# Normality test for the variable negative affect
plot(density(Negative.affect), 
     main = "Density plot : Negative Affect", 
     ylab = "Frequency", 
     xlab = "Negative Affect", sub = paste("Skewness : ", 
                                           round(e1071::skewness(Negative.affect), 2)))
polygon(density(Negative.affect), col = "blue")
par <- opar

paste("Skewness for Life Ladder : ", round(e1071::skewness(Life.Ladder), 2))
paste("Skewness for Log GDP per Capita : ", round(e1071::skewness(Log.GDP.per.capita), 2))
paste("Skewness for Social Support : ", round(e1071::skewness(Social.support), 2))
paste("Skewness for Healthy Life Expectancy at Birth : ", round(e1071::skewness(Healthy.life.expectancy.at.birth), 2))
paste("Skewness for Freedom to Make Life Choices : ", round(e1071::skewness(Freedom.to.make.life.choices), 2))
paste("Skewness for Perceptions of Corruption : ", round(e1071::skewness(Perceptions.of.corruption), 2))
paste("Skewness for Positive Affect : ", round(e1071::skewness(Positive.affect), 2))
paste("Skewness for Negative Affect : ", round(e1071::skewness(Negative.affect), 2))

# The variable Life Ladder is approximately symmetrical
# The variable Log GDP per Capita is approximately symmetrical
# The variable Social Support is moderately skewed
# The variable Healthy Life Expectancy at Birth is approximately symmetrical
# The variable Freedom to Make Life Choices is approximately symmetrical
# The variable Perceptions of Corruption is moderately symmetrical
# The variable Positive Affect is approximately symmetrical
# The variable Negative Affect is approximately symmetrical

# Check normality using qqnorm() function
# Normality of life ladder variable
opar <- par(no.readonly = TRUE)
hist(Life.Ladder, main = "Normality proportion of Life Ladder", xlab = "Life Ladder")

qqnorm(Life.Ladder)
qqline(Life.Ladder)

# Normality of Log GDP per capita variable
hist(Log.GDP.per.capita, main = "Normality proportion of Log GDP per Capita", xlab = "Log GDP per Capita")

qqnorm(Log.GDP.per.capita)
qqline(Log.GDP.per.capita)

# Normality of social support variable
hist(Social.support, main = "Normality proportion of Social Support", xlab = "Social Support")

qqnorm(Social.support)
qqline(Social.support)

# Normality of healthy life expectancy at birth variable
hist(Healthy.life.expectancy.at.birth, main = "Normality proportion of Healthy Life Expectancy at Birth", xlab = "Healthy Life Expectancy at Birth")

qqnorm(Healthy.life.expectancy.at.birth)
qqline(Healthy.life.expectancy.at.birth)

# Normality of freedom to make life choices variable
hist(Freedom.to.make.life.choices, main = "Normality proportion of Freedom to Make Life Choices", xlab = "Freedom to Make Life Choices")

qqnorm(Freedom.to.make.life.choices)
qqline(Freedom.to.make.life.choices)

# Normality of perceptions of corruption variable
hist(Perceptions.of.corruption, main = "Normality proportion of Perceptions of Corruption", xlab = "Perceptions of Corruption")

qqnorm(Perceptions.of.corruption)
qqline(Perceptions.of.corruption)

# Normality of positive affect variable
hist(Positive.affect, main = "Normality proportion of Positive Affect", xlab = "Positive Affect")

qqnorm(Positive.affect)
qqline(Positive.affect)

# Normality of negative affect variable
hist(Negative.affect, main = "Normality proportion of Negative Affect", xlab = "Negative Affect")

qqnorm(Negative.affect)
qqline(Negative.affect)
par <- opar

# Create training and testing data
set.seed(1)
no_rows_data <- nrow(world_happiness_data)
data_sample <- sample(1: no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)

training_data <- world_happiness_data[data_sample, ]
testing_data <- world_happiness_data[-data_sample, ]

# Build the model based on training data
model <- lm(Life.Ladder ~ Log.GDP.per.capita + Social.support + Healthy.life.expectancy.at.birth + Freedom.to.make.life.choices + Perceptions.of.corruption + Positive.affect + Negative.affect, data = training_data)
summary(model)

# confint function creates the 95% confidence interval for all the variables
confint(model)

# Check for collinearity of the model
# Collinearity statistics measure the relationship between multiple variables
# The VIF scores should be close to 1 but under 5 is fine and 10+ indicates that the variable is not needed and can be removed from the model.
library(car)
vif(model)

hist(resid(model),main='Histogram of residuals',xlab='Standardised Residuals',ylab='Frequency')

# Check the model for homoscedasticity
# The fitted values and residuals plot to check the assumption of homoscedasticity.
plot(model, which = 1)

# Non-constant Variance Score Test use the ncvTest() function
# A significant result suggests heteroscedasticity
ncvTest(model)

# Predict life ladder from testing data
life_ladder_predicted <- predict(model, testing_data)

# Make actuals_predicted dataframe
actuals_pred <- data.frame(cbind(actuals = testing_data$Life.Ladder, 
                                 predicted = life_ladder_predicted))
head(actuals_pred)

# Check the correlation between the actual and predicted values
correlation_accuracy <- cor(actuals_pred)
correlation_accuracy

# Min - max accuracy
min_max_accuracy <- mean(apply(actuals_pred, 1, min) / apply(actuals_pred, 1, max))
min_max_accuracy

# MAPE
mape <- mean(abs((actuals_pred$predicted - actuals_pred$actuals)) / actuals_pred$actuals)
mape

# RSE
rse <- sigma(model)/ mean(testing_data$Life.Ladder)
rse

# Look at the ranges in the input data
summary(world_happiness_data)

# Real world example
# Create model with final chosen input variables
model2 <- lm(Life.Ladder ~ Log.GDP.per.capita + Social.support + Freedom.to.make.life.choices + Perceptions.of.corruption + Positive.affect + Negative.affect, data = training_data)
df <- data.frame(Log.GDP.per.capita = c(7.702), Social.support = c(0.529), Freedom.to.make.life.choices = c(0.389), Perceptions.of.corruption = c(0.881), Positive.affect = c(0.554), Negative.affect = c(0.339))

predicted_life_ladder <- predict(model2, df)
predicted_life_ladder
