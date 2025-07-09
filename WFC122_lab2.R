#Emily Dumont
#WFC122 Section A03
#Lab 2

#Given values
nyrs <- 40 #Years (1980-2020)
N <- 1000 #Population size in 1980
r <- 0.04 #Instantaneous rate of increase


#Question 1
lambda <- exp(r) #Finite rate of increase; e^ is replaced using exp() function


#Question 2
sealion <- numeric(nyrs + 1) #Create an empty vector with the same value of years plus 
#one for the initial population size
sealion[1] <- 1000 #Set the initial population size as the first value in the vector above


#Question 3
for(t in 1:nyrs) {
  sealion[t+1] <- lambda * sealion[t]
}
sealion #Print the vector with the population size for each year


#Question 4
years <- 1980:2020
plot(years,sealion,main="Sea Lion Population from 1980-2020",xlab="Year",ylab="Population Size")


#Question 5
#Create a for loop with conditional statements to assign the associated lambda values 
#for each year
lambda_t <- numeric(40) #We already know the population for the first year, so vector
#should be 40 and not 41

for(t in 1:40) { 
  if(years[t] < 1998) {
    lambda_t[t] <- 1.02
  } else if (years[t] < 2015 && years[t] > 1997){ #not necessary to include 
    #"&& years[t] > 1997" due to previous statement
    lambda_t[t] <- 1.07
  } else {
    lambda_t[t] <- 1.01
  }
}
lambda_t


#Question 6
N_vary <- numeric(41) #Vector should be 41 and not 40 because we need to include
#the first population size next
N_vary[1] <- 1000

for(t in 1:nyrs) {
  N_vary[t+1] <- N_vary[t] * lambda_t[t] #Population growth formula accounting for 
  #the population of the previous year
}
N_vary


#Question 7
plot(years,sealion,type="l",col="blue",main="Sea Lion Population from 1980-2020",xlab="Year",ylab="Population Size")
#"type="l"" creates a line plot rather that the automated scatter plot
lines(years,N_vary,type="l",col="red") #"lines()" function plots given data on top of current plot


#Question 8
#Part a (25 culled)
a <- numeric(41)
a[1] <- 1000

for(t in 1:nyrs) {
  a[t+1] <- a[t] * lambda_t[t]
  if(t > 30) {
    a[t+1] <- a[t+1] - 25 #subtract 25 each year for the last ten years
  }
}
a

#Part b (50 culled)
b <- numeric(41)
b[1] <- 1000

for(t in 1:nyrs) {
  b[t+1] <- b[t] * lambda_t[t]
  if(t > 30) {
    b[t+1] <- b[t+1] - 50 #subtract 50 each year for the last ten years
  }
}
b

#Part c (100 culled)
c <- numeric(41)
c[1] <- 1000

for(t in 1:nyrs) {
  c[t+1] <- c[t] * lambda_t[t]
  if(t > 30) {
    c[t+1] <- c[t+1] - 100 #subtract 100 each year for the last ten years
  }
}
c

#create a matrix with each column representing a cull trajectory
matrix <- as.matrix(a)
colnames(matrix) <- "a" #set column name
matrix <- cbind(matrix,b) #bind each cull vector as a column
matrix <- cbind(matrix, c)
matrix_df <- as.data.frame(matrix) #converted to a data frame to try plotting using
#the previous matrix


#Question 9
plot(years,N_vary,type="l",col="red",main="Sea Lion Population from 1980-2020",xlab="Year",ylab="Population Size")
#lines(years,matrix_df[,1:3],type="l", col=c("blue","yellow","green"))
lines(years,a,type="l",col="blue")
lines(years,b,type="l",col="yellow")
lines(years,c,type="l",col="green")


#Question 10
#During positive environmental conditions, such as when the cull first occurred, the sea 
#lion population did not change very drastically. The population growth rate decreased 
#slightly with an increase in cull size. During poor environmental conditions, such as a 
#few years prior to 2020, predicted populations with larger culls decreased towards 
#population decline. Only the smallest cull continued to increase with a smaller growth rate.
#Depending on how drastically managers are trying to limit population growth due to sea 
#lion impact on the marine ecosystems, I would recommend culling 25 sea lions a year 
#because the population becomes more controlled and increases less exponentially compared 
#to no culling. Larger cull sizes, as previously mentioned, result in a drastic decline, 
#though 50 culls a year could be beneficial for a more aggressive control on the sea 
#lion population size.


