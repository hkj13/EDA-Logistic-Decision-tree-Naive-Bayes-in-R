data <- read.csv("C:/Users/HARIHARAPUTHRA/Downloads/diabetes.csv")

library(tidyverse)


str(data)

describe(data)

head(data)

missmap(data)

diabetes$DiabetesResult <- factor(diabetes$Outcome, levels = c("0", "1"), labels = c("negative", "positive"))


diabetes_result <- diabetes %>%
  group_by (DiabetesResult) %>%
  count(DiabetesResult)
diabetes_result

ggplot(data=diabetes_result) + geom_col(mapping=aes(x=DiabetesResult, y= n, fill=DiabetesResult)) +labs(title="Number of positive and negative diabetes cases", x= "Outcome", y = "Count")

ggplot(data, aes(Age, colour = Outcome)) +
  geom_freqpoly(binwidth = 1) + labs(title="Age Distribution by Outcome")

c <- ggplot(data, aes(x=Pregnancies, fill=Outcome, color=Outcome)) +
  geom_histogram(binwidth = 1) + labs(title="Pregnancy Distribution by Outcome")
c + theme_bw()

P <- ggplot(data, aes(x=BMI, fill=Outcome, color=Outcome)) +
  geom_histogram(binwidth = 1) + labs(title="BMI Distribution by Outcome")
P + theme_bw()

ggplot(data, aes(Glucose, colour = Outcome)) +
  geom_freqpoly(binwidth = 1) + labs(title="Glucose Distribution by Outcome")

ggpairs(data)

y <- select(diabetes, -(DiabetesResult))
my_colors <- colorRampPalette(c("cyan", "deeppink3"))
heatmap(cor(y), Rowv= NA, Colv= NA, col = my_colors(100))
