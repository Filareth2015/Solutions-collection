library(ggplot2) #loading neccesary library
library(dplyr)

dataset <- read.csv("dataset.csv", head=TRUE) 

### set.seed(12345)

summary(dataset)
head(dataset)

g <- ggplot(dataset, aes(dataset$Type, dataset$Count, fill = Type)) +
  geom_point(colour = "red", size = 4) + 
  geom_boxplot(aes(fill = dataset$Type))
g

means <- dataset %>%
  group_by(Type) %>%
  summarise(mean(Count))
means

water <- filter(dataset, Type =="Water") 
mean(water$Count)

beer <- filter(dataset, Type =="Beer") 
mean(beer$Count)
observedStat <- mean(beer$Count) - mean(water$Count)

t.test(Count ~ Type, data = dataset)

x <- dataset$Count
n <- length(x)
B <- 100000
resamples <- matrix(sample(x,
                           n * B,
                           replace = TRUE),
                    B, n)
resampledMedians <- apply(resamples, 1, median)
hist(resampledMedians, col = "green", breaks = 10)

g = ggplot(data.frame(resampledMedians = resampledMedians), aes(x = resampledMedians))
g = g + geom_histogram(color = "black", fill = "lightblue", binwidth = 0.05)
g

group <- as.character(dataset$Type)
testStat <- function(w, g) {mean(w[g == "Beer"]) - mean(w[g == "Water"])}
observedStat <- testStat(dataset$Count, group)
permutations <- sapply(1 : 10000, function(i) testStat(dataset$Count, sample(group)))
