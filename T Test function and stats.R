# Examples of T-tests function & Create Error Functions

set.seed(2)
var1 =rnorm(50,3,2)
var2= rnorm(60,5,3)
result =t.test(var1,var2)
result
mode(result)
str(result)

#since the results are a list it is possible to gain access to the results by using the $operation like.  result$methodint(results$statistic)
print(result$statistic)
print(result$estimate)
print(result$conf.level)
print(result$statistic)

#Create another funtion that perfoms two sample t-test, prints three values and returns the p-value only
printT = function(tValue) {
  print(paste("t", round(tValue$statistic, 3), sep = "= "))
  print(paste("DF", round(tValue$parameter, 3), sep  = "= ")) 
  print(paste("p", round(tValue$p.value, 3), sep = "="))
}
printT(result)

var1 =rnorm(50,3,2)
var2= rnorm(60,5,3)

#create another function that performs two sample t-test, prints three stats and returns p-value only
myTtest = function(x1,x2) {
  result = t.test(x1,x2)
  printT(result)
  return (result$p.value)
}
getP=myTtest(var1,var2)
getP

myTtest1 = function(x1,x2) {
  if(length(x1) == 0 | length (x2) ==0) 
  result = t.test(x1,x2)
  printT(result)
  result$p.value
}
getP1 = myTtest1(numeric(0), var2)
getP1

#if one of the arguments is empty than return NA
myTtest1 = function(x1,x2) {
  if(length(x1) == 0 | length (x2) ==0) return (NA)
  result = t.test(x1,x2)  # Error in t.testdefault(x1,x2) - not enough "x' observations
  printT(result)
  result$p.value
}
getP1 = mytest1(numeric(0), var2)
getP1

#to return several values combine them in a list
myTest2 =
function(x1,x2) {
    if(length(x1) == 0 | length (x2) ==0) return (NA)
    result = t.test(x1,x2)
    printT(result)
    list(method=result$method, t=result$statistic, df=result$parameter, p=result$p.value)
}
#Assign the two variables for the function needed in myTest2
result2 = myTest2(var1,var2)
result2 

#Blocks are noted y {} which forms the group and evaluated until a new line is entered after }

#if statement can have the following forms if (condition)
# true.branch else NULL
# true.branch else false.branch
# if a vector is used only the first component is used
# Example
x = rnorm(10)
x
if(mean(x)>median(x)) print("Mean > Median") else print ("Mean > Median")

# when the if statement is not in a block, the else must appear in the same line as the if statemnt
# if in a block, else must apear in the same line of the closing bracket
# when if statement is in a function, else can be placed in a new line
# multipe cases use the if...else if... structure
# Example
if (any (x<=0)) y = log(10+x) else y = log (x)
y
y = if (any (x=0)) log(10+x) else log(x)
y

# the && and || are useful with the if statement & and && indicate logical
# AND and | and || indicate logical OR. 
# The shorter form performs elementwise comparisons in much the same way as 
# arithmetic operators. The longer form evaluates left to right examining only the 
# first element of each vector. Evaluation proceeds only until the result is 
# determined. The longer form is appropriate for programming control-flow and 
# typically preferred in if clauses.
a = 3
b = 3
a == 3 & b ==3
a ==3 && b == 3
x = c (T,F,T)
y = c (T, T, T)
x & y 
x && y
y1 = c (F,T, T)
x && y1
#situations using and testing matrices
y = matrix (1:2,1)
y
nrow(y) > 1 && y[2,1]  == 2 #False
nrow(y) > 1 &&  y[2,1]  == 2 #Error: unexpected '=' in "nrow(y) > 1 &&="

sqrtAndLog = function(x) {
  if(is.numeric(x) && min(x)>0) {
    x.sqrt <- sqrt (x)
    x.log <- log(x)
  } else
    return (list(x.sqrt, x.log))
}
sqrtAndLog(c(2,3,4))
sqrtAndLog
sqrtAndLog(c(2,4,-33))

#Example calculate the central tendency with different options
central = function (y,measure){
  if (measure == "mean") return (mean(y))
  else if (measure == "harmonic") return (1/mean(1/y))
  else if (measure == "median") return (median(y))
  else stop ("Your mensure is not supported")
}
z <- rnorm(10, mean=2, sd =3)
central (z, "mean")
central (z, "harmonic")
central (z, "median")

#Example equality of variance to performa a two sample ttest (myTtest)
# Test equality of variance
# if variance are equal set the var.equal = T
# print t-sttats, degrees of freedom and p-value
#return t-test result

#Build up t-test function in R
myTtest = function (x,y) {
  vt.p = var.test(x,y)$p.value
  if (vt.p > 0.05) {
    print ("Variances are equal")
    result = t.test(x,y, var.equal = T)
  } else {
    print("Variance are not equal")
    result = t.test(x,y)
  }
  print(paste("t: ", round(result$statistic, 2), sep = " "))
  print(paste("DF: ", round(result$parameter), sep = " "))
  print(paste("p: ", round(result$p.value, 2), sep = " "))
  return(result)
}


#Test of Equality of variance for a two sample t-test
set.seed(3)
a = rnorm (10,2,5)
b = rnorm (8, 3,2)
c = rnorm (9, 5, 5)
r1 = myTtest(a,b)
r1
r2 = myTtest(a,c)
r2

#if else statement, (ifelse (test, true.value, false.value)) with all arguments vectors
#create an indicator variable
treatment = c(rep("case", 3), rep("control", 2))
treat.ind = ifelse(treatment == "case", 1,0)
treat.ind

#example:  taking square root of negative number
x = -3:5
sqrt(x)
#use the ifelse to avoid the warning message
sqrt(ifelse(x >=0, x, NA))
#this code  will still generate a warning message since all the arguments in the ifelse function
ifelse(x >=0, sqrt(x), NA)

##switch function value is a character vector and the element of ... with a name that exactly matches value  
x = 3
switch(x, 2+2, mean(1:10), rnorm(5))
switch(2, 2+2, mean(1:10), rnorm(5)) # returns 5.5
foo = switch(6, 2+2, mean(1:10), rnorm(5))
foo  # "returns NULL
my= "fruit"
switch(y, fruit = "banana", veggi = "broccoli", meat = "beef")
# returns banana
#makes a selection according to the character value of one of the arguments in the function
central = function(y,measure) {
  switch(measure, mean = return(mean(y)),
                  harmonic = return (1/mean(1/y)),
                  median = return(median(y)),
                  stop ("Your measure is not supported"))
}
z = rnorm(10, mean = 2, sd = 3)
central(z,"mean")

#for loop has the following form (for(variable in sequence) statement)
ss <- 0
total <- 0
for (i in c(20, 30, 25,40)) {
      total <- total + i
      ss <- ss + i^2
}
total #result 115
ss    # result 3525

#while loop has the following form while(condition) statement and if the condition is TRUE the statement is evaluated
#this process continues until statement is evaluated to FALSE
x <- 0
test <- 1
while (test>0) {
      x <-x +1
      print (x^2)
      test <- x < 6
}

#repeat loop has the form repeat {statement} which must be a block statement
x <- 0
test <- 1
repeat {
  x <-x +1
  print (x^2)
  if (x==6) break
}

#result is the same output of numbers (1,4,9,16,25,36) as the while statement example

