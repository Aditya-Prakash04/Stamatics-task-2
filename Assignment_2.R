library(imager)
# QUESTION a

data(iris)
head(iris)
par(mfrow = c(1,2))
boxplot(iris$Sepal.Length,main = "Sepal Length of species",col="blue",xlab ="Sepal Length")
boxplot(iris$Petal.Length, main = "Petal Length Of Species",col = "red",xlab = "Petal Length")
plot(iris$Sepal.Length,iris$Petal.Length, col = iris$Species,xlab = "Sepal Length",ylab = "Petal Length",pch=16)
legend("top",legend = c("setosa", "versicolor", "virginica"),pch=16,col=c("red","green","black"))
#-------------------------------------------------------------------------------------------------------------------------------------------------------------#

# QUESTION b
pic <- load.image("C:/Users/ASUS/OneDrive/Desktop/Study Material/Gekko_Mosh_02.jpeg")
flip <- function(image)
{
  col.mat <- as.array(image[, , 1, ])
  dims <- dim(col.mat)
  flipped_image <- array(0,dim = dims)
  for(i in 1:dims[1]){
    for(j in 1:dims[2]){
      flipped_image[i,j,] <- col.mat[dims[1]-i+1,j,]
    }
  }
  return(flipped_image)
}
par(mfrow = c(1,2))
plot(pic)
plot(as.cimg(flip(pic)))
#--------------------------------------------------------------------------------------------#

# QUESTION d
library(tidyverse)
library(rvest)
html <- read_html("https://stats.stackexchange.com/questions?tab=Votes")
Question_title <- html %>% html_elements(".s-link") %>% html_text()
Question_title <- Question_title[-c(1,17)]
Votes <- html %>% html_elements(".s-post-summary--stats-item-number") %>% html_text()
Votes <- Votes[-c(2,3,5,6,8,9,11,12,14,15,17,18,20,21,23,24,26,27,29,30,32,33,35,36,38,39,41,42,44,45)]
Votes <- as.numeric(Votes)
Answers <- html %>% html_elements(".s-post-summary--stats-item-number") %>%html_text()
Answers <- Answers[-c(1,3,4,6,7,9)]
Answers <-Answers[-c(4,6,7,9,10,12,13,15,16,18,19,21,22,24,25,27,28,30,31,33)]
Answers <-Answers[-c(16,17,19)]
Answers <- Answers[-c(14)]
Answers <-as.numeric(Answers)
Views <- html %>% html_elements(".s-post-summary--stats-item-number") %>%html_text()
Views <- Views[-c(1,2,4,5,7,8,10,11,13,14,16,17,19,20,22,23,25,26,28,29,31,32,34,35,37,38)]
Views <- Views[-c(14,15,17,18)]
stack_table <- data.frame("Title OF Question"=Question_title,"Number of votes"= Votes,"Number of answers"=Answers,"Number of views"= Views)
View(stack_table)
#--------------------------------------------------------------------------------------------#

# QUESTION e
vitamin_experiment <- function(n_trials) {
  days_list <- numeric(n_trials)  
  
  for (i in 1:n_trials) {
    days <- 0
    whole_tablets <- 100  
    
    while (whole_tablets > 0) {
      days <- days + 1
      
      
      if (runif(1) < 0.5) {
        whole_tablets <- whole_tablets - 1  # Cut a whole tablet in half
      }
    }
    
    days_list[i] <- days
  }
  
  avg_days <- mean(days_list)
  return(avg_days)
}
n_trials <- 1000
average_days <- vitamin_experiment(n_trials)
average_days
 #-------------------------------------------------------------------------------------------#
# QUESTION c
library(MASS)
ship_colors <- c("red", "green", "blue", "yellow", "orange")
boxplot(ships$type,ships$incidents,main ="Accidents by ships",xlab="type of ship",ylab="frequency",col=ship_colors)
legend("topright",legend = levels(ships$type),fill=ship_colors)
# the hypothesis is correct.
