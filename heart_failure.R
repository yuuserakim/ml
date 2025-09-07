# Library
library(dplyr)
library(readxl)

# Data Import
df = read.csv("/Users/klieeu777/CDS492/Capstone/heart_failure/heart_failure.csv")
str(df)

# Data Overview
table(df$sex)
table(df$cp)
table(df$chol)

# Data Preprocessing: N/A value 
dim(df)
df2 <- na.omit(df)
dim(df2)

####### Question?? ########
df2 <- filter(df2, 
                if_all(everything(), #모든 케이스에 대해
                       ~!is.na(.)& .!="") # NA가 아님 & Blank가 아님 .(period)은 나머지 변수를 의미
)

###### Logistic Regression ---Question (Fit to my data) ########
df2$num
df2$target <- ifelse(df2$num > 0, 1, 0) # (T,F)
df2$target
table(df2$target)

# Model Fitting
df2_x <- subset(df2, select=c(-id, -num))
df2_x %>% colnames()

model <- glm(data = df2_x, target ~ ., family=binomial)

# Model Evaluation
model %>% summary