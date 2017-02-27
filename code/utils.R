CoinFlip <- function(){
  
  sequence <- sample(c(0,1), 10000, replace = TRUE)
  
  avg <- sample(c(0), 10000, replace = TRUE)
  
  
  for(i in 1:10000){
    
    avg[i] <- ((cumsum(sequence)[i])/i)
    
  }
  
  avg
  
  
  plot(avg)
  abline(h = 0.5)
  
}

CLT <- function(distribution=NULL,n, r) {
  samples <- switch(distribution,
                    "Normal" = matrix(rnorm(n*r,50,5),r),
                    "Uniform" = matrix(runif(n*r,0,100),r),
                    "Exponential" = matrix(rexp(n*r,1),r),
                    "Chisquare" = matrix(rchisq(n*r,5),r),
                    "Poisson" = matrix(rpois(n*r,5),r),
                    "Binomial" = matrix(rbinom(n*r,10,0.5),r),
                    "Gamma" = matrix(rgamma(n*r,1,0.5),r))
  
  mean <- apply(samples,1,mean)   
  hist(mean,main="Distribution of Means")
  qqnorm(mean)
  qqline(mean)
}

SLR <- function(file){
  #file = paste(path_to_data_folder,'/hw23R-Advertising.csv',sep='')
  Advertising <- read.csv(file)
  
  TV = lm(Sales~TV, data =Advertising)
  summary(TV)
  TV
  plot(Advertising$TV, Advertising$Sales)
  abline(TV)
  
  
  Radio = lm(Advertising$Sales~Advertising$Radio)
  summary(Radio)
  Radio
  plot(Advertising$Radio, Advertising$Sales)
  abline(Radio)
  
  
  Newspaper = lm(Advertising$Sales~Advertising$Newspaper)
  summary(Newspaper)
  Newspaper
  plot(Advertising$Newspaper, Advertising$Sales)
  abline(Newspaper)
}

MLR <- function(file){
  #file = paste(path_to_data_folder,'/hw23R-Advertising.csv',sep='')
  Advertising <- read.csv(file)
  multiple_R = lm(Advertising$Sales~Advertising$TV+Advertising$Radio+Advertising$Newspaper)
  summary(multiple_R)
  
}

LogisticRegression <- function(file){
  
  #file = paste(path_to_data_folder,'/hw23R-q4.txt',sep='')
  data <- read.delim(file)
  
  fit = glm(Y~X1+X2+X3, data = data, family = binomial)
  fit
  summary(fit)
  
}

LogisticRegressionImproved <- function(file){
  
  #file = paste(path_to_data_folder,'/hw23R-q4.txt',sep='')
  data <- read.delim(file)
  boxplot(data)
  fit = glm(Y~X1+X3, data = data, family = binomial)
  fit
  summary(fit)
  cor(data)
  
}