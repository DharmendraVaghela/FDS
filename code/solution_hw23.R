#cleanup before start
rm(list=ls(all=T))


####
# Path variables to files
#
####
setwd('.') #Inside code folder - DON'T CHANGE THIS PATH
source('installPackages.R')
source('utils.R')
path_to_data_folder = '../data' # DON'T CHANGE THIS PATH 

#Q1 
CoinFlip() 


#Q2
# populationDistribution: string('uniform','normal')
# sampleSize: integer (~30)
# numberOfSamples: integer (>100)
CLT(populationDistribution,sampleSize,numberOfSamples)
# Examples
CLT('Normal',100,1000)
CLT('Uniform',100,1000)
CLT('Exponential',100,1000)
CLT('Chisquare',100,1000)
CLT('Poisson',100,1000)
CLT('Binomial',100,1000)

#Q3a
SLR(paste(path_to_data_folder,'/hw23R-Advertising.csv',sep=''))

#Q3b 
MLR(paste(path_to_data_folder,'/hw23R-Advertising.csv',sep=''))

#Q4
LogisticRegression(paste(path_to_data_folder,'/hw23R-q4.txt',sep=''))

#Q5
LogisticRegressionImproved(paste(path_to_data_folder,'/hw23R-q4.txt',sep=''))

