r_pareto <- function(n, a, b){
  U <- runif(n)
  x <- b * ((1 - U)^(-1/a) - 1)
  x
}

bankruptcy <- function(pre, claim){
  balance <- vector()
  for (i in seq(1:100000)){
    n <- sum(rbinom(1,1000, claim))
    pension <- r_pareto(n, 3, 100000)
    balance[i] <- 250000 + 1000 * pre - sum(pension)
  }
  summary(balance)
  p_bank <<- mean(balance < 0)
}
## Q1, c
set.seed(425)
library(tidyverse)
x <- r_pareto(10000, 3, 100000)
data <- as.data.frame(cbind(seq(1:10000), x))
data <- data %>%
  mutate(pareto = (3*100000^3)/((x+100000)^(3 + 1)))
ggplot(data) +
  geom_histogram(aes(x, stat(density)), binwidth = 2000) +
  geom_line(aes(x, pareto)) +
  coord_cartesian(xlim = c(0, 100000))
## Q2
set.seed(425)
bankruptcy(6000, 0.1)
cat(paste('the probability of bankruptcy is', p_bank))

## Q3,a
set.seed(425)
pre <- as_tibble(seq(from = 5500, to = 8000, by = 250)) %>%
  mutate(bank = 0)
for (i in seq(1:length(pre$value))){
  bankruptcy(pre$value[i], 0.1)
  pre$bank[i] <- p_bank}
ggplot(pre, aes(value, bank)) +
  geom_line() +
  geom_point()
min(pre$value[pre$bank < 0.02])
## Q3, b
set.seed(425)
claim <- as_tibble(seq(from = 0.05, to = 0.15, by = 0.005)) %>%
  mutate(bank = 0)
for (i in seq(1:length(claim$value))){
  bankruptcy(6000, claim$value[i])
  claim$bank[i] <- p_bank}
ggplot(claim, aes(value, bank)) +
  geom_line() +
  geom_point()
max(claim$value[claim$bank < 0.02])

## Extension-data visulization
set.seed(425)
data <- as_tibble(seq(from = 5500, to = 8000, by = 250))
colnames(data) <- c('pre')
data <- as_tibble(data) %>%
  mutate(prob = list(seq(from = 0.05, to = 0.15, by = 0.005))) %>%
  unnest(prob) %>%
  mutate(bank = 0)
for (i in seq(1:length(data$prob))){
  bankruptcy(data$pre[i], data$prob[i])
  data$bank[i] <- p_bank}
library(plotly)
data_3d <- data %>%
  spread(pre, bank)
data_3d <- as.matrix(data_3d)
rownames(data_3d) <- data_3d[,1]
data_3d <- data_3d[,-1]
plot_ly(x = colnames(data_3d), y = rownames(data_3d), z = data_3d, type = 'surface')

## Extension - VaR and Expected Shortfall estimation

## Extension - year by year bankruptcy rate and profit rate

## Newton-Raphson method to solve bankruptcy rate = 0.2
