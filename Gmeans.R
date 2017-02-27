####################################################################
# Implement G-means algorithm                                      #
####################################################################
library(nortest)
require(graphics)
library(cluster)
library(fpc)
library(plot3D)

# X is the input dataset(it is a dataframe contain multiple cols reading from csv file)
Gmeans <- function(X,alpha = 0.0001,k=1){
  
  if(k==1){
    k_me = kmeans(X, 1)
  }
  else{
    k_me = kmeans(X, k)
  }
  
  returnCenters = c()
  numClusters = nrow(k_me$centers)
  for(i in 1:numClusters){
    xi = data[k_me$cluster == i,]     #Subset of X corresponding to cluster "i"
    ci = k_me$centers[i,]
    newKm = getKm(xi,ci)
    #initializing new "children" centers of c
    c1 = newKm$centers[1,]
    c2 = newKm$centers[2,]
    v = c1-c2
    xdash = as.matrix(xi)%*%v/(norm(as.matrix(v))^2)  #calculate X' by projecting X on V
    xdash = scale(xdash)    #scale to get mean=0 and var = 1
    ad = ad.test(xdash)       #test value
    if(ad$p.value <= alpha){  #reject NULL hyposthesis, use new centers
      returnCenters = rbind(returnCenters, newKm$centers[1,],newKm$centers[2,])
    }
    else{
      returnCenters = rbind(returnCenters, ci)
    }
  }
  return (returnCenters)

}

getKm = function(xi, c){
  pca = prcomp(xi)
  lambda = pca$sdev[1]^2  #lambda corresponding to PC
  s = pca$rotation[,1]    #s is the eigen vector
  m = s*sqrt(2*lambda/pi)
  km = kmeans(xi,rbind(c+m,c-m))  #use c+-m as the new centers
  return (km)
}

main = function(data, alpha){
  k = 1
  initialCenter = kmeans(data,k)$centers           #initial set of centers
  centersList = initialCenter
  temp = c()
  repeat {
    temp = Gmeans(data, alpha, centersList )
    if(length(centersList) == length(temp)){   #no new centers created in this iteration
      break
    }
    centersList = temp
  }
  optimalK = dim(centersList)[1]
  cat ("Optimal number of clusters: ",optimalK)
  #create the model for the optimal centers found
  model = kmeans(data, centersList)
  #plot the data, use PCA for >2 dimensions
  clusplot(data, model$cluster, lines = 4, cex = 0.4, xlab = "Dimension 1", ylab = "Dimension 2", col.p = "dark blue" )
}

data <- read.csv("hw45-r3b-test-data.csv", header = TRUE)
clusters = main(data,0.0001)
clusters