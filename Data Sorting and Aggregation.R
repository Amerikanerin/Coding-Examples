#Maggi Rademacher
Example of Data Sorting and Aggregating

#Problem 1 - Chol.txt data
# Set directory & read in data file
setwd
chol = read.table(file="chol.txt", stringsAsFactors=FALSE, sep="\t",header=T)
head(chol)

#1.1 use apply function to calculate mean for each variable except SEX
apply(chol[,2:12], 2, mean,na.rm=TRUE)

#1.2 use aggregate function to calculate the sex specific mean for each variable
sexTableAgg = aggregate(chol[,2:12],list(chol$sex), mean,na.rm=TRUE)
print(sexTableAgg) 

#Create a new variable based on the chol variable
Cholmean = mean(chol$chol, na.rm=TRUE)
print(Cholmean)
chol$chol2 <- ifelse(chol$chol > Cholmean, 
                        c("HI"), c("LOW")) 

#1.3 use tapply to calculate the standard deviation of bmi for each chol 2 category
head(chol)
BmiTable1 = tapply(chol$bmi, chol$chol2, sd)
print(BmiTable1)

#1.4 use tapply function to calculate standardr devation of bmi for each combination of sex and cho12
BmiTable2 = tapply(chol$bmi, list(chol$sex,chol$chol2), sd)
print(BmiTable2)

#Problem 2 - calculate median using a simulated matrix
#Create matrix
set.seed(91765)
mat=matrix(rnorm(200), 10)
mat[1,1]=NA
dim(mat)

#2.1 calculate the median of each row using a for loop
matMedian1 = numeric(10)
for (i in 1:10) {
  matMedian1[i] = median(mat[i,], na.rm=TRUE)
}
matMedian1

#2.2 calculate the median of each row using an apply function
#removing NA values
matMedian2 = apply(mat,1,median, na.rm=TRUE)
matMedian2

#Problem 3 - write a function (rowTtest) to perform a row wise two-sample T-test

#Create data set called GeneSample normally distributed
set.seed(515)
GeneSample=rnorm(20000, mean=0, sd=1)
dim(GeneSample)=c(1000,20)

#Create rownames for data using G1...Gn notation
rownames(GeneSample) <- paste('G', 1:nrow(GeneSample)) 
rownames(GeneSample) <- gsub(" ","",rownames(GeneSample))
head(GeneSample)

#Create character vector for Pheno
pheno = list("case","case","case", "case","case",
             "case","case","case", "case","case",
             "control","control","control","control","control",
             "control","control","control","control","control")
length(pheno)

#Create one data matrix
Gene = cbind(GeneSample2, pheno2)

#No Solution found for the remainder of the question 
# to perform a rowTtest function to perform a row-wise two sample t-test


 
#4 Calculate the two-sample t-test fo the chol data frame by the sex variable using the apply function
#read in chol data set
chol = read.table(file="chol.txt", stringsAsFactors=FALSE, sep="\t",header=T)
head(chol)
str(chol)

#Calculate t-test stats by categorical variable (sex)
results = sapply(chol[-1], function(x) 
  unlist(t.test(x~chol$sex)[c("estimate","statistic","parameter","p.value")]))

#transform results table and change into matrix
results = t(results)
results=data.matrix(results)

#Assign Column Names to results table
colnames(results) = c("F.mean", "M.mean", "t", "df", "P")

#Print Results table & review dimensions
print(results)
dim(results)

