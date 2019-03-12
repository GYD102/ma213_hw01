# Problem Set #1

# 1)
library("qcc")

data <- data.frame(
  c("Action-Adventure",
    "Comedy",
    "Drama",
    "Epic",
    "Musical",
    "War",
    "Western"),
  c(5,11,35,12,9,3,3)
)

colnames(data) <- c("genre", "count")
data$percentage <- data$count / sum(data$count) * 100

pie(data$count,
    paste(data$genre, " ", data$count),
    main = "Oscar Winners by Genre: 1927 to 2015")

pareto.data <- data$count
names(pareto.data) <- data$genre
pareto.chart(pareto.data, main = "Oscar Winners by Genre: 1927 to 2015")


# 2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggrepel)

data <- read.csv("SAT.csv")
boxplot(select(data, SAT2011, SAT2014))
# Attempt to do same thing w/ ggplot

ggplot(gather(data, year, score, SAT2011, SAT2014),
       aes(x = year, y = score)) +
  geom_boxplot()

# Box-and-whisker plots
p <- ggplot(gather(data, Year, Score, SAT2011, SAT2014),
            aes(x = Year, y = Score)) + geom_boxplot()
p <- p + coord_flip()
# Select the critical values for axis-lines (quartiles, outliers)
b <- ggplot_build(p)$data %>%
  as.data.frame() %>%
  select(ymin, lower, middle, upper, ymax, outliers) %>%
  unlist() %>%
  as.vector %>% round() %>% sort()
# Remove breaks that would be too close to one another
b <- b[abs(b - head(c(0,b), length(b))) > 10]
p <- p + scale_y_continuous(
  breaks = b,
  minor_breaks = NULL) +
  ggtitle("US State SAT Scores: 2011 vs 2014")
p

ggplot(data, aes(x = data$SAT2011, y = data$SAT2014)) +
  geom_point() +
  geom_abline(slope = 1, col = "red") +
  geom_label(aes(x=1600, y=1395, hjust = 0, vjust = 0,
        label = c('Red line is the x = y line. If a point falls above the line,
then that state scored better in 2014 than in 2011.
If a point falls below the line, the state scored
worse in 2014 than in 2011.'))) + 
  geom_text_repel(aes(label = data$STATE),
                  box.padding = 0.4,
                  min.segment.length = 0.2,
                  segment.colour = "lightblue",
                  position = position_dodge(width = 0.5)) +
  xlab("Score in 2011") + ylab("Score in 2014") +
  ggtitle("US State SAT Scores: 2011 vs 2014")
  

data$paired.diff <- data$SAT2014 - data$SAT2011
# Box-and-whisker plots
p <- ggplot(data, aes(x = "score", y = paired.diff)) + geom_boxplot() +
  ylab("Paired differences")
p <- p + coord_flip() +
  theme(axis.title.y = element_blank(),
        axis.text.y.left = element_blank(),
        axis.ticks.y = element_blank())
p <- p + scale_y_continuous(
  breaks = ggplot_build(p)$data %>%
    as.data.frame() %>%
    select(ymin, lower, middle, upper, ymax, outliers) %>%
    unlist() %>%
    as.vector %>% round(),
  minor_breaks = NULL) +
  ggtitle("US State SAT Scores: 2011 vs 2014")
p

boxplot(data$paired.diff)

ggplot(data, aes(x = data$STATE, data$paired.diff)) +
  geom_point(aes(colour = as.character(sign(data$paired.diff)))) +
  scale_colour_manual(values = setNames(c('green','red', "black"),
                                        c("1", "-1", "0"))) +
  theme(axis.text.x = element_blank()) + 
  geom_hline(yintercept = 0, color = "red") +
  geom_text(aes(label = data$STATE), angle = 90, hjust = -0.2) +
  scale_y_continuous(breaks = seq(-250, 150, 50),
                     minor_breaks = seq(-240, 150, 10)) +
  labs(colour = "Positive or Negative Change (2011 to 2014)") +
  ylab("Paired Difference") + xlab("State")


# 3)
library(ggplot2)
data <- read.csv("PGA.csv")
summary(data$INDEX)
sort(data$INDEX)

ggplot(data, aes(x = "Driving Index", y = INDEX)) +
  geom_boxplot() +
  coord_flip() +
  theme(axis.title.y = element_blank()) +
  ggtitle("Driving Performance Index for 40 Golfers")

# 4)
# a)
library(ggplot2)
library(dplyr)
data <- read.csv("NUC.csv")
summary(data$PLANTS)
ggplot(data, aes(x = data$PLANTS)) +
  geom_histogram(binwidth = 1, col = "black") +
  scale_x_continuous(breaks = seq(min(data$PLANTS), max(data$PLANTS), 1)) +
  xlab("Number of Nuclear Power Plants") + ylab("Count") +
  ggtitle("Histogram of Nuclear Power Plant per State")

# b) Remove maximum, redo part (a)
data <- data %>% filter(data$PLANTS < max(data$PLANTS))
summary(data$PLANTS)
ggplot(data, aes(x = data$PLANTS)) +
  geom_histogram(binwidth = 1, col = "black") +
  scale_x_continuous(breaks = seq(min(data$PLANTS), max(data$PLANTS), 1)) +
  xlab("Number of Nuclear Power Plants") + ylab("Count") +
  ggtitle("Histogram of Nuclear Power Plant per State")


# c) Trimmed mean
data <- read.csv("NUC.csv")
sort(data$PLANTS)
sort(data$PLANTS)[3:18]
(1+1+1+2+3+3+3+3+4+4+4+5+5+5+6+7)/16
mean(data$PLANTS, trim = 0.1)
mean(sort(data$PLANTS)[3:18])


# 5)
data <- read.csv("PGA.csv")
var(data$DISTANCE)
var(data$ACCURACY)
var(data$INDEX)

sd(data$DISTANCE)
sd(data$ACCURACY)
sd(data$INDEX) 

v <- sum((data$DISTANCE - mean(data$DISTANCE))^2) / (length(data$DISTANCE) - 1)
v2 <- (sum(data$DISTANCE^2) - sum(data$DISTANCE)^2/length(data$DISTANCE)) / 
  (length(data$DISTANCE) - 1)
c(v,v2)
sd <- sqrt(v)
sd

v <- sum((data$ACCURACY - mean(data$ACCURACY))^2) / (length(data$ACCURACY) - 1)
v2 <- (sum(data$ACCURACY^2) - sum(data$ACCURACY)^2/length(data$ACCURACY)) / 
  (length(data$ACCURACY) - 1)
c(v,v2)
sd <- sqrt(v)
sd

v <- sum((data$INDEX - mean(data$INDEX))^2) / (length(data$INDEX) - 1)
v2 <- (sum(data$INDEX^2) - sum(data$INDEX)^2/length(data$INDEX)) / 
  (length(data$INDEX) - 1)
c(v,v2)
sd <- sqrt(v)
sd
