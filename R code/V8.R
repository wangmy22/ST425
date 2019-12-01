# install.packages('plotly')
# This package is used to draw the 3-D plot.
library(plotly)                                                   

# install.packages('tidyverse')
# This package is usually applied in data cleaning, here we use it to generate "tibble", 
# which is a class of data similar to data.frame but is more convenient to handle.
library(tidyverse)

# Using the inversion method to generate random variables that follows pareto distribution
# with parameters a and b, and n is the sample size. 
r_pareto <- function(n, a, b){
  U <- runif(n)
  x <- b * ((1 - U)^(-1/a) - 1)
  x
}

# Plot pareto distribution and normal distribution
## Generate the PDF for a pareto distribution with parameters a and b.
p_pareto <- function(x,a,b){
  p <- (a*b^a)/((x+b)^(a+1))
  p
}

## This is the range that x takes.
Value <- seq(350000,1000000,by=100)
Pareto <- vector()

## In our case, the claim size follows a pareto distribution with a=3 and b=100000.
for (i in 1:length(Value)){
  Pareto[i] <- p_pareto(Value[i],3,100000)
}

## 50000 is the expectation of the pareto distribution we generated above and 1.5e+10 is the
## theoretical variance. Here we generate a normal distribution with the same mean and variance.
Normal <- dnorm(Value,mean=50000,sd=sqrt(1.5e+10))

## Generate a t-distribution with degree of freedom equals 1 and the range of x equals "Value".
TDis <- dt(Value,1)

## Draw the PDF of Pareto and Normal distribution we generated above, and compare the shape of the PDF.
tmp <- as.data.frame(cbind(Value,Pareto,Normal,TDis))
ggplot(tmp,aes(x=Value))+
  geom_line(aes(y = Pareto,color = "pareto"))+
  geom_line(aes(y = Normal, color = "normal"))+
  theme_bw()

# This Function curves the relationship between bankruptcy and pre and premium.
bankruptcy <- function(pre, claim){
  ## We set seed here to make sure the survival analysis we will discuss later will be consistent.
  set.seed(425)
  balance <- vector()
  for (i in seq(1:100000)){
    ## N is the number of claims, which follows B(p), where p is the claim rate.
    n <- sum(rbinom(1,1000, claim))
    ## Pension is the claim size, which follows pareto(3,1000000)
    pension <- r_pareto(n, 3, 100000)
    ## Balance is the asset the insurance company holds at the end of the year.
    balance[i] <- 250000 + 1000 * pre - sum(pension)
  }
  summary(balance)
  assets <<- mean(balance)
  ## p_bank gives the probability that the company becomes bankruptcy at the end of the year.
  p_bank <<- mean(balance < 0)
  list(balance=balance)
}


## This function is the same as bankruptcy but doesn't output the year-end asset, thus smooths the calculation process.
## We will use this function when we do not need the year-end asset to do the analysis to save time.
bankruptcy2 <- function(pre, claim){
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
}
## Q1, c
set.seed(425)
### x is the simulated pareto(3,100000) distribution.
x <- r_pareto(10000, 3, 100000)
data <- as.data.frame(cbind(seq(1:10000), x))
data <- data %>%
  ### Pareto is the theoretical pareto(3,100000) distribution.
  mutate(pareto = (3*100000^3)/((x+100000)^(3 + 1)))
### Draw the simulated pareto and the theoretical pareto in one plot to examine
### whether the simulation generates a well estimation for the real distribution.
p <- ggplot(data) +
  geom_histogram(aes(x, stat(density)), binwidth = 2000) +
  geom_line(aes(x, pareto)) +
  coord_cartesian(xlim = c(0, 100000))
p <- ggplotly(p)
p


## Q2
### Premium = 6000, claim rate = 0.1
bankruptcy(6000, 0.1)
### Print the simulated expected year-end asset and probability of bankruptcy.
cat(paste('the expected assets at year end is', assets))
cat(paste('the probability of bankruptcy is', p_bank))

## Q3,a
### Compare the bankruptcy probability under different premiums, while holding claim rate fixed, say, claim rate = 10%.
pre <- as_tibble(seq(from = 5500, to = 8000, by = 250)) %>%
  mutate(bank = 0)
for (i in seq(1:length(pre$value))){
  bankruptcy(pre$value[i], 0.1)
  pre$bank[i] <- p_bank}
### Plot the bankruptcy probability under different annual premiums, rangeing from 5500 to 8000.
ggplot(pre, aes(value, bank)) +
  geom_line() +
  geom_point()+
  ylab('probability of bankruptcy')+
  xlab('annual premium')+
  geom_hline(yintercept=0.02,colour='red')
### Find the minimum premium required to control bankruptcy probability under 2%.
min(pre$value[pre$bank < 0.02])

## Q3, b
### Compare the bankruptcy probability under different claim rates, while holding annual premium constant, say, premium = 6000.
claim <- as_tibble(seq(from = 0.05, to = 0.15, by = 0.005)) %>%
  mutate(bank = 0)
for (i in seq(1:length(claim$value))){
  bankruptcy(6000, claim$value[i])
  claim$bank[i] <- p_bank}
ggplot(claim, aes(value, bank)) +
  geom_line() +
  geom_point()+
  ylab('probability of bankruptcy')+
  xlab('probability of making a claim')+
  geom_hline(yintercept=0.02,colour='red')
### Find the maximum claim rate that wiil bring bankrutcy probability under 2%.
max(claim$value[claim$bank < 0.02])

## Extension: data visulization
### Set the same seed as before to make the analysis consistent.
set.seed(425)
### This tibble is the combination of different claim rates and premiums.
data <- as_tibble(seq(from = 5500, to = 8000, by = 250))
colnames(data) <- c('pre')
data <- as_tibble(data) %>%
  mutate(prob = list(seq(from = 0.05, to = 0.15, by = 0.005))) %>%
  unnest(prob) %>%
  mutate(bank = 0)

### Calculate the bankruptcy probability under these combinations.
for (i in seq(1:length(data$prob))){
  bankruptcy(data$pre[i], data$prob[i])
  data$bank[i] <- p_bank}

### Draw the 3-D surface plot to better understand the relationship between bankruptcy probability and premium and claim rate.
data_3d <- data %>%
  spread(pre, bank)
data_3d <- as.matrix(data_3d)
rownames(data_3d) <- data_3d[,1]
data_3d <- data_3d[,-1]
plot_ly(x = colnames(data_3d), y = rownames(data_3d), z = data_3d, type = 'surface')

## Extension - VaR and Expected Shortfall estimation
### VaR is the short for value at risk, VaR and ES are two similar risk measurments widely applied in banking and insurance industry.
prob<-seq(0.05,0.15,by=0.005)
premium<-seq(5500,8000,by=250)
### Set up the VaR 95% and the corresponding ES structure.
VaR_95<-matrix(0,nrow=length(prob),ncol=length(premium),dimnames = list(prob,premium))
ES<-matrix(0,nrow=length(prob),ncol=length(premium),dimnames = list(prob,premium))

for(i in 1:length(prob))
{
  for(j in 1:length(premium))
  {
    t.tmp<-bankruptcy(premium[j],prob[i])
    ### Estimate the 5% VaR and ES under different combinations.
    VaR_95[i,j]=abs(quantile(t.tmp$balance,0.05))
    ES[i,j]=abs(mean(t.tmp$balance[t.tmp$balance<-VaR_95[i,j]]))
  }
}

### Draw the 3-D plot for 95% VaR and ES.
plot_ly(x=premium,y=prob,z=VaR_95,type="surface")
plot_ly(x=premium,y=prob,z=ES,type="surface")

## Extension - year by year survival rate and profit rate
### Calculate the year by year survival rate 
df <- data.frame()
### Design a function which simulates the return in the following 10 years for 1000 times.
expect_asset_10y <- function(starting_asset,premium_peryear,prob_claim){
  for (i in 1:1000){
    initial <- starting_asset
    pre <- premium_peryear
    prob <- prob_claim
    for (j in 1:10){
      n <- sum(rbinom(1,1000,prob))
      pension <- r_pareto(n,3,100000)
      asset <- initial+1000*pre-sum(pension)
      ### If the year-end asset < 0, then the company bankrupts, and we kick it out.
      if(asset<0){
        df[i,j:10] <- NA
        break
      }else
        df[i,j] <- asset
      initial <- asset
    }
  }
  df
}
result <- expect_asset_10y(250000,6000,0.1)

### Add column names
names <- vector()
for (i in 1:10){
  a <- paste("asset at the end of year",i)
  names[i] <- a
}
colnames(result) <- names

### Probability of survival in the following 10 years
survival_prob <- vector()
for (i in 1:10){
  survival_prob[i] <- result %>% 
    filter(!is.na(result[,i])==TRUE) %>% 
    nrow()/1000
}
survival_prob
df_survival_prob <- data.frame(year=seq(1:10),survival_prob=survival_prob)
### Plot the survival probability.
survival_prob_p <- df_survival_prob %>% 
  ggplot(aes(x=year,y=survival_prob))+
  geom_line()+
  geom_point()+
  coord_cartesian(xlim = c(1, 10))+
  ylab('probability of survival')
survival_prob_p <- ggplotly(survival_prob_p)
survival_prob_p


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
### Calculate the premium when claim rate is fixed at 10%.
pre <- as_tibble(seq(from = 5500, to = 8000, by = 250)) %>%
  mutate(bank = 0)
for (i in seq(1:length(pre$value))){
  pre$bank[i] <- bankruptcy2(pre$value[i], 0.1)
}
### Calculate the interval within which the root lies.
pos_index <- as.numeric(match(max(pre$value[pre$bank > 0.02]), pre$value))
neg_index <- as.numeric(match(min(pre$value[pre$bank < 0.02]), pre$value))
### Take the correspondign premium and bankruptcy probability.
pos_value <- as.numeric(pre[pos_index, 1])
pos_bank <- as.numeric(pre[pos_index, 2])
neg_value <- as.numeric(pre[neg_index, 1])
neg_bank <- as.numeric(pre[neg_index, 2])

### Set up the initial distance.
dist = 1

### Newton - Raphson
while (dist > 10^-5){
  value_hat <- pos_value - (pos_bank - 0.02) * (pos_value - neg_value)/(pos_bank - neg_bank)
  bank_hat <- bankruptcy2(value_hat, 0.1)
  dist <- abs(bank_hat - 0.02)
  if (bank_hat < 0.02){
    neg_bank <- bank_hat
    neg_value <- value_hat
  } else if (bank_hat > 0.02){
    pos_bank <- bank_hat
    pos_value <- value_hat
  }
}
pre_1 <-value_hat

# similarly for claim
claim <- as_tibble(seq(from = 0.05, to = 0.15, by = 0.005)) %>%
  mutate(bank = 0)
for (i in seq(1:length(claim$value))){
  bankruptcy2(6000, claim$value[i])
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
  bank_hat <- bankruptcy2(6000, value_hat)
  dist <- abs(bank_hat - 0.02)
  if (bank_hat < 0.02){
    neg_bank <- bank_hat
    neg_value <- value_hat
  } else if (bank_hat > 0.02){
    pos_bank <- bank_hat
    pos_value <- value_hat
  }
}
claim_2 <- value_hat


data <- list(pre = c(pre_1, 6000), claim = c(0.1, claim_2))
data %>% as_tibble()
### Using the ols to find the relationship between claim rate and premium, given bankruptcy probability fixed.
ols <- lm(claim ~ pre, data = data)
data_new <- list(pre = seq(from = 5000, to = 8000, by = 250))
### Using the ols above the predict the maximum claim rate that could be accepted 
### in order to control the probability of default under 2%.
data_new$claim <- predict(ols, data_new)
data_new = data_new%>% 
  as_tibble() %>%
  mutate(dist = NA)
for (i in length(data_new$pre)){
  est <- bankruptcy2(data_new$pre[i], data_new$claim[i])
  data_new$dist = abs(est - 0.02)
}
ggplot(data_new, aes(pre, claim, color = 'bankruptcy = 0.02')) +
  geom_line() +
  theme_bw() +
  theme(legend.title = element_blank())


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
### This function is very similar to that we used to analyse the survival rates, the only difference is that 
### companies default at year i are still considered in the following years.
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
