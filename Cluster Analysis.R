home_data <- read.csv('Colleges.csv',header = T,as.is = T)
##Question 1
home_data$AcceptanceRate <- home_data$App_accepted/home_data$App_received
View(home_data)
##Question 2
plot(home_data[, c('Instate_tuition','AcceptanceRate')]
     , xlab = 'Instate_tuition'
     , ylab = 'AcceptanceRate')
###I believe four clusters would be specific.
##Question 3
new_data <- data.frame('AcceptanceRate'=home_data$AcceptanceRate,
                       'Instate_tuition'=home_data$Instate_tuition)
norm_data <- new_data
for (i in ncol(new_data)){
  norm_data[,i] <- (new_data[, i] - mean(new_data[, i]))/sd(new_data[, i])
}
norm_data
##Question 4
set.seed(0)
km_subset<- kmeans(norm_data, centers = 4)
km_subset$cluster
km_subset$size
km_subset$centers
##Question 5
dis <- km_subset$withinss
dis
##111 observations in cluster 1, 57 observations in cluster 2
##128 observations in cluster 3, 93 observations in cluster 4
##The sum of squares of cluster 1 is 6.756166
##The sum of squares of cluster 2 is 8.692905
##The sum of squares of cluster 3 is 6.517098
##The sum of squares of cluster 4 is 9.053175
##cluster 3 has the lowest sum of squares
##Therefore, cluster 3 has the best homogeneity
##Question 6
new_data2 <- data.frame('AcceptanceRate'=home_data$AcceptanceRate,
                       'Outofstate_tuition'=home_data$Outofstate_tuition,
                       'Graduaation_rate'=home_data$Graduation_rate)
norm_data2 <- new_data2
for (i in ncol(new_data2)){
  norm_data2[,i] <- (new_data2[, i] - mean(new_data2[, i]))/sd(new_data2[, i])
}
norm_data2
set.seed(0)
k_vec <- 1:15
avgdis2 <- c()
for (i in 1:length(k_vec)){
  km<- kmeans(norm_data2, centers = k_vec[i])
  avgdis2[i] <- mean(km$withinss)
}
avgdis2
##eblow chart
plot(k_vec, avgdis2,
     xlab="Number of clusters, k",
     ylab="Average within-clusters sum of squares")
##I will choose 4 as the best K because average within-clusters sum of
##suqares begains to change a little when k is equal to 4