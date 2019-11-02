# install.packages('plotly')
library(plotly)
# install.packages('tidyverse')
library(tidyverse)

r_pareto <- function(n, a, b){
  U <- runif(n)
  x <- b * ((1 - U)^(-1/a) - 1)
  x
}


bankruptcy <- function(pre, claim){
  set.seed(425)
  balance <- vector()
  for (i in seq(1:100000)){
    n <- sum(rbinom(1,1000, claim))
    pension <- r_pareto(n, 3, 100000)
    balance[i] <- 250000 + 1000 * pre - sum(pension)
  }
  summary(balance)
  assets <<- mean(balance)
  p_bank <<- mean(balance < 0)
  list(balance=balance)
}
## Q1, c
set.seed(425)
x <- r_pareto(10000, 3, 100000)
data <- as.data.frame(cbind(seq(1:10000), x))
data <- data %>%
  mutate(pareto = (3*100000^3)/((x+100000)^(3 + 1)))
p <- ggplot(data) +
  geom_histogram(aes(x, stat(density)), binwidth = 2000) +
  geom_line(aes(x, pareto)) +
  coord_cartesian(xlim = c(0, 100000))
p <- ggplotly(p)
p


## Q2
bankruptcy(6000, 0.1)
cat(paste('the expected assets at year end is', assets))
cat(paste('the probability of bankruptcy is', p_bank))

## Q3,a
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

data_3d <- data %>%
  spread(pre, bank)
data_3d <- as.matrix(data_3d)
rownames(data_3d) <- data_3d[,1]
data_3d <- data_3d[,-1]
plot_ly(x = colnames(data_3d), y = rownames(data_3d), z = data_3d, type = 'surface')

## Extension - VaR and Expected Shortfall estimation
prob<-seq(0.05,0.15,by=0.005)
premium<-seq(5500,8000,by=250)
VaR_95<-matrix(0,nrow=length(prob),ncol=length(premium),dimnames = list(prob,premium))
ES<-matrix(0,nrow=length(prob),ncol=length(premium),dimnames = list(prob,premium))

for(i in 1:length(prob))
{
  for(j in 1:length(premium))
  {
    t.tmp<-bankruptcy(premium[j],prob[i])
    VaR_95[i,j]=quantile(t.tmp$balance,0.05)
  }
}

plot_ly(x=prob,y=premium,z=VaR_95,type="surface")

for(i in 1:length(prob))
{
  for(j in 1:length(premium))
  {
    t.tmp<-fall(premium[j],prob[i])
    ES[i,j]=mean(t.tmp$balance[t.tmp$balance<VaR_95[i,j]])
  }
}
plot_ly(x=prob,y=premium,z=ES,type="surface")

## Extension - year by year survival rate and profit rate
# Calculate the year by year survival rate 
df <- data.frame()
# design a function which simulates the return in the following 10 years for 1000 times
expect_asset_10y <- function(starting_asset,premium_peryear,prob_claim){
  for (i in 1:1000){
    initial <- starting_asset
    pre <- premium_peryear
    prob <- prob_claim
    for (j in 1:10){
      n <- sum(rbinom(1,1000,prob))
      compen <- r_pareto(n,3,100000)
      asset <- initial+1000*pre-sum(compen)
      if(asset<0){
        df[i,j:10] <- NA
        break
      }else
      df[i,j] <- asset
      initial <- asset
    }
  }
  df}
result <- expect_asset_10y(250000,6000,0.1)

## add column names
names <- vector()
for (i in 1:10){
  a <- paste("asset at the end of year",i)
  names[i] <- a
}
colnames(result) <- names

### probability of survival in the following 10 years
survival_prob <- vector()
for (i in 1:10){
  survival_prob[i] <- result %>% 
    filter(!is.na(result[,i])==TRUE) %>% 
    nrow()/1000
}
survival_prob
df_survival_prob <- data.frame(year=seq(1:10),survival_prob=survival_prob)
df_survival_prob %>% 
  ggplot(aes(x=year,y=survival_prob))+
  geom_line()+
  geom_point()+
  coord_cartesian(xlim = c(1, 10))



# Calculate the year by year profit rate
fprofit <- function(pre, claim){
  profit_rate <- vector()
  for (i in seq(1:1000)){
    n <- sum(rbinom(1,1000, claim))
    pension <- r_pareto(n, 3, 100000)
    profit_rate[i] <- (1000 * pre - sum(pension))/(1000*pre)
  }
  pr <<- mean(profit_rate) 
}

profit<-vector()
set.seed(425)
for (i in seq(1:10)){
  fprofit(6000,0.1)
  profit[i] <- pr
}

plot(seq(1:10),profit,xlab = 'year',ylab='profit rate')

## Newton-Raphson method to solve bankruptcy rate = 0.2
# premium
pre <- as_tibble(seq(from = 5500, to = 8000, by = 250)) %>%
  mutate(bank = 0)
for (i in seq(1:length(pre$value))){
  bankruptcy(pre$value[i], 0.1)
}
pos_index <- as.numeric(match(max(pre$value[pre$bank > 0.02]), pre$value))
neg_index <- as.numeric(match(min(pre$value[pre$bank < 0.02]), pre$value))
pos_value <- as.numeric(pre[pos_index, 1])
pos_bank <- as.numeric(pre[pos_index, 2])
neg_value <- as.numeric(pre[neg_index, 1])
neg_bank <- as.numeric(pre[neg_index, 2])
dist = 1
while (dist > 10^-5){
  value_hat <- pos_value - (pos_bank - 0.02) * (pos_value - neg_value)/(pos_bank - neg_bank)
  bank_hat <- bankruptcy(value_hat, 0.1)
  dist <- abs(bank_hat - 0.02)
  if (bank_hat < 0.02){
    neg_bank <- bank_hat
    neg_value <- value_hat
  } else if (bank_hat > 0.02){
    pos_bank <- bank_hat
    pos_value <- value_hat
  }
}
value_hat
bankruptcy(value_hat, 0.1)
# similarly for claim
claim <- as_tibble(seq(from = 0.05, to = 0.15, by = 0.005)) %>%
  mutate(bank = 0)
for (i in seq(1:length(claim$value))){
  bankruptcy(6000, claim$value[i])
  claim$bank[i] <- p_bank}
pos_index <- as.numeric(match(min(claim$value[claim$bank > 0.02]), claim$value))
neg_index <- as.numeric(match(max(claim$value[claim$bank < 0.02]), claim$value))
pos_value <- as.numeric(claim[pos_index, 1])
pos_bank <- as.numeric(claim[pos_index, 2])
neg_value <- as.numeric(claim[neg_index, 1])
neg_bank <- as.numeric(claim[neg_index, 2])
dist = 1
while (dist > 10^-5){
  value_hat <<- pos_value - (pos_bank - 0.02) * (pos_value - neg_value)/(pos_bank - neg_bank)
  bank_hat <- bankruptcy(6000, value_hat)
  dist <- abs(bank_hat - 0.02)
  if (bank_hat < 0.02){
    neg_bank <- bank_hat
    neg_value <- value_hat
  } else if (bank_hat > 0.02){
    pos_bank <- bank_hat
    pos_value <- value_hat
  }
}
value_hat


# the code after this one serves the same function
# df <- data.frame()
# expect_asset_10y <- function(starting_asset,premium_peryear,prob_claim){
# for (i in 1:1000){
#   initial <- 250000
#   pre <- 6000
#   for (j in 1:10){
#     n <- sum(rbinom(1,1000,0.1))
#     compen <- r_pareto(n,3,100000)
#     asset <- initial+1000*pre-sum(compen)
#     df[i,j] <- asset
#     initial <- asset
#   }
# }}
# 
# names <- vector()
# for (i in 1:10){
#   a <- paste("asset at the end of year",i)
#   names[i] <- a
# }
# colnames(df) <- names



df <- data.frame()
### design a function which simulates the return in the following 10 years for 1000 times
expect_asset_10y <- function(starting_asset,premium_peryear,prob_claim){
  for (i in 1:1000){
    initial <- starting_asset
    pre <- premium_peryear
    prob <- prob_claim
    for (j in 1:10){
      n <- sum(rbinom(1,1000,prob))
      compen <- r_pareto(n,3,100000)
      asset <- initial+1000*pre-sum(compen)
      df[i,j] <- asset
      initial <- asset
    }
  }
  df}
result <- expect_asset_10y(250000,6000,0.1)

names <- vector()
for (i in 1:10){
  a <- paste("asset at the end of year",i)
  names[i] <- a
}
colnames(result) <- names

### visualisation of the expected mean over the following 10 years
colMeans(result)

year <- seq(1:10)
expect_return <- data.frame(year=seq(1:10),return=colMeans(result))
expect_return %>% 
  ggplot(aes(x=year,y=return))+
  geom_line()+
  geom_point()+
  theme_bw()



