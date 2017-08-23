# Maggi Rademacher


#Set Working Directory
# setwd("C:/Users/maggi/Documents/Maggi/University San Diego/Introduction to R/R Directory/")



#Creating Extreme Values Function
#Values greater or less than 3 times the standard deviation from the mean are extreme.


#Create a normally distributed data set
set.seed(1)
mydata  = rnorm(n=1000)
length(mydata) # Determine the length of an object - here a numerical vector

#Defining a function
extreme = function(dat){
  m     = mean(dat)
  sd    = 3*sd(dat)
  count = 0
  
  for(i in 1:length(dat))  {  #stuff to do the number of times that dat is long
    if(dat[i]<(m-sd) || dat[i]>(m+sd)){
      count = count + 1
    }
  }
  if(count>0) {
    print(paste("There are ", count, " extreme values found.", sep = ""))
  } else print("There are no extreme values.")
}
#Call Function
extreme (mydata)



# Write a function calCS to perform the task of calculating area/volumes

calCS = function (var1,radius) {
  if (toupper(var1)=='AC')  # Calculate area of a circle
    return (pi*radius^2)
   else if (toupper(var1)=='CC')  # Calculate circumfrence of a circle
    return (2*pi*radius)
   else if (toupper(var1)=='VS')  # Calculate volume of a sphere
    return (4/3*pi*radius) 
   else if (toupper(var1)=='AS')  # Calculate area of a sphere
    return (4*pi*(radius^2))
  else stop("your method is not supported")
}

#Test function calculations with different test cases
calCS('AC',5)
calCS('CC',5)
calCS('VS',10)
calCS('AS',25)
calCS('ac',5)


# Write a loop for circles with radii values of 5,10,15,20 & 25 

# First solution using a while loop
x = 5
test = 1
while(test>0){
  print(calCS('AC', x ))
  x = x+5
  test =(x<30)
}

# A second solution found using the seq_along function 
# note to self: seq_along(x) takes a vector for x, and it creates a sequence up
# to the count of elements in the vector.

x <- c(5, 10, 15,20,25) # vector of radii for the problem

for (i in seq_along(x)){
  print(calCS('AC', x[i]))  
}
  

#Using the painters data set from the MASS library perform:
library(MASS)
head(painters)

#a.  create a data set with observations Colour >=17 and School equals"D"
paintersNew = subset(painters, Colour >= 17 & is.element(painters$School, c("D")))
paintersNew

#b. create a data set that contains only Daudine and Barocci
rownames(painters)
includelist = c("Da Udine", "Barocci")
paintersNew1 = subset(painters, rownames(painters) == includelist)
paintersNew1

#c create a data set which contains observations with Colour >=17 and School "D"
# but only the Composition and Drawing variable
paintersNew2 = subset(painters, Colour >= 17 & is.element(painters$School, c("D")), c(Composition, Drawing))
paintersNew2

#d create a categorical variable Comp.cat with three approximate equal levels based
# on composition
qt = quantile(painters$Composition, c(0, 0.33, 0.66, 1))
qt

painters$Comp.cat = cut(painters$Composition, qt, labels=c("first", "second", "third"), include.lowest = T)
head(painters)
painters$Comp.cat
#Create frequency table for new Comp.cat variable
install.packages('plyr')
library(plyr)
count(painters, 'Comp.cat')


# Create data frame and transform it into a long form

# Assign Values to columns
c1 = c( "CONT", "RI", "WI")
c2 = c(85, 79, 84)
c3 = c( 85, 79, 85)
c4 = c(86, 79, 84)
c5 = c(85, 80, 83)

# Create a data set with column names (i.e. wide) and print it to check
wide.table = data.frame( "Program" = c1, "s1" = c2, "s2" = c3, "s3" = c4, "s4"= c5)
wide.table
str(wide.table)

#transform it into the long form
long.table = reshape(wide.table, varying=list(c("s1","s2", "s3","s4")),v.names="s", timevar="Time", idvar="Program", direction="long")
long.table

#transform it from the long form back to the wide form
wide.table2 = reshape(long.table, varying=list(c("s1","s2", "s3","s4")),v.names="s", timevar="Time", idvar="Program", direction="wide")
wide.table2
  

#Function stackDataInList

#Change directory to R data set
setwd("C:/Users/maggi/Documents/Maggi/University San Diego/Introduction to R/Assignments and Quizzes")
load("datList.RData")
dir()
datList

stackDataInList= function(alist){
  mydata = data.frame()
  for(i in 1:length(alist))  {  #stuff to do the number of times that alist is long
  mydata = rbind(mydata, data.frame(datList[i]))}
 mydata
}
  
#Call Function
stackDataInList(datList[c(2,3,4)])
stackDataInList(datList[1])
stackDataInList(datList)

