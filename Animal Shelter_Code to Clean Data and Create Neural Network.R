install.packages(c("r", "markdown"))
install.packages("nnet")
install.packages("neuralnet")
install.packages("RSNNS")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("lubridate") 
install.packages("rpart")
install.packages("randomForest")
install.packages("ggthemes")
install.packages("xlsx")
install.packages("caret")
install.packages("e1071")
install.packages("Rcpp")
install.packages("lattice")
install.packages("grid")
install.packages("MASS")
install.packages("caTools")

# Load packages
library(ggplot2) # visualization
library(ggthemes) # visualization
library(dplyr) # data manipulation
library(lubridate) # dates
library(rpart) # rpart for imputation
library(randomForest) # classification algorithm
library(nnet) # neural net
library(neuralnet) # neural net
library(RSNNS) # neural net
library(e1071) #for statistic & confusion Matrix
library(Rcpp) #integration of R and C++. 
library(lattice) #visualization system inspired by Trellis graphics
library(grid) #user-level functions to work with "grid" graphics
library(MASS) #Functions and datasets to support Venables and Ripley, "Modern Applied Statistics" with S
library(caTools) #Caret Tools


# Read the data
train <- read.csv('../input/train.csv', stringsAsFactors = F)
test <- read.csv('../input/test.csv', stringsAsFactors = F)

# Rename the ID column so train & test match
names(train)[1] <- 'ID'

# And change ID in test to character
test$ID <- as.character(train$ID)

# Combine test & training data
 full <- train
#full <- bind_rows(train, test)
#Reshape
#outcomes <- full[1:26729, ] %>%
#  group_by(AnimalType, OutcomeType) %>%
#  summarise(num_animals = n())

# Plot
ggplot(outcome, aes(x = AnimalType, y = num_animals, fill = OutcomeType)) +
  geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
  coord_flip() +
  labs(y = 'Proportion of Animals', 
       x = 'Animal',
       title = 'Outcomes: Cats & Dogs') +
  theme_few()

# Quality Check Review Headers in Data Set and First Contents and Create Frequency Tables by Outcome
head(train)
head(test)
head(full)

with(full, table(Breed))
summary(full$OutcomeType)
with(full, table(Color))
summary(full$OutcomeType)

#Preprocessing of Full Data Set
#transform Variables into Character
full$Name <- as.character(full$Name)
full$Breed <- as.character(full$Breed)
full$SexuponOutcome <- as.character(full$SexuponOutcome)
full$Color <- as.character(full$Color)
full$AgeuponOutcome <- as.character(full$AgeuponOutcome)


# Replace blank names with "Nameless"
full$Name <- ifelse(nchar(full$Name)==0, 'Nameless', full$Name)
# Make a name v. no name variable
full$HasName[full$Name == 'Nameless'] <- 0
full$HasName[full$Name != 'Nameless'] <- 1

# Replace blank sex with most common
full$SexuponOutcome <- ifelse(nchar(full$SexuponOutcome)==0, 
                              'Spayed Female', full$SexuponOutcome)

# Take a look as some of the levels
levels(factor(full$Breed))[1:10]

# Use "grepl" to look for "Mix"
full$IsMix <- ifelse(grepl('Mix', full$Breed), 1, 0)

# Split on "/" and remove " Mix" to simplify Breed
full$SimpleBreed <- sapply(full$Breed, 
                           function(x) gsub(' Mix', '', 
                                            strsplit(x, split = '/')[[1]][1]))

#Create frequency tables from data set by IsMix and SimpleBreed
with(full, table(IsMix))
summary(full$IsMix) 
with(full, table(SimpleBreed))
summary(full$SimpleBreed)                        

# Plot out Distributions for Neutered (to be defined) and Breed
# Use strsplit to grab the first color
full$SimpleColor <- sapply(full$Color, 
                           function(x) strsplit(x, split = '/| ')[[1]][1])
levels(factor(full$SimpleColor))
levels(factor(full$Color))
levels(factor(full$SimpleBreed))
levels(factor(full$Breed))

# Use "grepl" to look for "Intact - Values (2= Neutered, 1 = Intact, 0 = Unknown)
full$Neutered <- ifelse(grepl('Intact', full$SexuponOutcome), 2,
                      ifelse(grepl('Unknown', full$SexuponOutcome), 0, 1))

# Use "grepl" to look for sex
full$Sex <- ifelse(grepl('Male', full$SexuponOutcome), 'Male',
                   ifelse(grepl('Unknown', full$Sex), 'Unknown', 'Female'))
# Reshape
Neutered <- full[1:26729, ] %>%
  group_by(AnimalType, Neutered, OutcomeType) %>%
  summarise(num_animals = n())

# Plot
ggplot(Neutered, aes(x = Neutered, y = num_animals, fill = OutcomeType)) +
  geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
  facet_wrap(~AnimalType) +
  coord_flip() +
  labs(y = 'Proportion of Animals', 
       x = 'Animal',
       title = 'Outcomes Whether Neutered: Cats & Dogs') +
  theme_few()

# Get the time value:
full$TimeValue <- sapply(full$AgeuponOutcome,  
                         function(x) strsplit(x, split = ' ')[[1]][1])
# Now get the unit of time:
full$UnitofTime <- sapply(full$AgeuponOutcome,  
                          function(x) strsplit(x, split = ' ')[[1]][2])
# Fortunately any "s" marks the plural, so we can just pull them all out
full$UnitofTime <- gsub('s', '', full$UnitofTime)
full$TimeValue  <- as.numeric(full$TimeValue)
full$UnitofTime <- as.factor(full$UnitofTime)#
# Make a multiplier vector
multiplier <- ifelse(full$UnitofTime == 'day', 1,
                     ifelse(full$UnitofTime == 'week', 7,
                            ifelse(full$UnitofTime == 'month', 30, # Close enough
                                   ifelse(full$UnitofTime == 'year', 365, NA))))
# Apply our multiplier
full$AgeinDays <- full$TimeValue * multiplier
summary(full$AgeinDays)

# Use rpart to predict the missing age values (Risdahl, n.d.)
age_fit <- rpart(AgeinDays ~ AnimalType + Sex + Neutered + SimpleBreed + HasName, 
                 data = full[!is.na(full$AgeinDays), ], 
                 method = 'anova')

# Fill inpredicted age values where missing using "predict"
full$AgeinDays[is.na(full$AgeinDays)] <- predict(age_fit, full[is.na(full$AgeinDays), ])


# Use the age variable to make a variables for AgeSegment as well as classification by group
full$AgeSegment[full$AgeinDays < 3650] <- 'Adult'
full$AgeSegment[full$AgeinDays >= 3650] <- 'Senior'
full$AgeSegment[full$AgeinDays < 365] <- 'Baby'
full$AgeSegment <- factor(full$AgeSegment)


#Create Dummy Variables for different age classifications dependent on the AgeInDays
full$Baby[full$AgeinDays <= 365] <- 1
full$Baby[full$AgeinDays > 365] <- 0
full$Senior[full$AgeinDays >= 3650] <- 1
full$Senior[full$AgeinDays < 3650] <- 0

full$Adult <- 1
full$Adult[full$Baby == 1] <- 0
full$Adult[full$Senior == 1] <- 0

# Plot out Outcomes: Age Segment in ggplot2
ggplot(full[1:26729, ], aes(x = AgeSegment, fill = OutcomeType)) + 
  geom_bar(position = 'fill', colour = 'black') +
  labs(y = 'Proportion', title = 'Animal Outcome: Babies, Adults versus Seniors') +
  theme_few()

# Check Data Set to make sure all values are filled
apply(full,2,function(x) sum(is.na(x)))

#Build up Outcome Dummy Variables in Training Sample
full$Adoption <- ifelse(grepl('Adoption', train$OutcomeType), 1, 0)
full$Died <- ifelse(grepl('Died', train$OutcomeType), 1, 0)
full$Euthanasia <- ifelse(grepl('Euthanasia', train$OutcomeType), 1, 0)
full$Owner<- ifelse(grepl('Return', train$OutcomeType), 1, 0)
full$Transfer<- ifelse(grepl('Transfer', train$OutcomeType), 1, 0)
full$Cat<- ifelse(grepl('Cat', train$AnimalType), 1, 0)
full$Dog<- ifelse(grepl('Dog', train$AnimalType), 1, 0)

# Create Data Subset called NN for neural network 
MyVariables <- c("OutcomeType", "AnimalType","IsMix", "HasName","Neutered", "AgeSegment","Adult", "Senior","Baby", "Dog", "Cat")
NN <- full[MyVariables]

## Create the Sample Size for the Training data (70%) 
smp_size <- floor(0.70 * nrow(full))

## set the seed to make your partition reproductible and random
set.seed(123)
train_ind <- sample(seq_len(nrow(full)), size = smp_size)

train <- full[train_ind, ]
test <- full[-train_ind, ]
#train <- NN[1:1000, ]
#test  <- NN[1000:1500, ]

# remove Character data attributes from training data set

#train.OutcomeType <- NULL
train$AnimalType      <- NULL
train$AgeSegment      <- NULL

test$AnimalType      <- NULL
test$AgeSegment      <- NULL

#Coerce the Class Variable in Test and Train Datasets into Factor instead of character
character_vars <- lapply(train, class) == "character"
train[, character_vars] <- lapply(train[, character_vars], as.factor)
character_vars <- lapply(test, class) == "character"
test[, character_vars] <- lapply(test[, character_vars], as.factor)

#Create a neural network using the NN package for the limited data set as defined in NN above
animal.nn = nnet(OutcomeType ~ IsMix + HasName + Neutered + Adult + Senior + Baby + Cat + Dog,  data = train, size = 3,  rang = 0.1, decay = 0.05, maxit = 400,na.action=na.omit)
summary (animal.nn)

#Generate predictions of the test dataset
animal.predict = predict(animal.nn, train, type = "class")
animal.predict <- factor(animal.predict)
summary(train$OutcomeType)
summary(animal.predict)

#Create predictions for the test data set
#animal.predicttest = predict(animal.nn,test, type = "class")
#animal.predicttest <- factor(animal.predicttest)

#Generate a classification table based on predicted labels and labels of the test data set
nn.table = table(train$OutcomeType, animal.predict)
nn.table
#nn.tabletests = table(test$OutcomeType, animal.predicttest)

#Workaround for Confusion Matrix for the train sample when the nn.table does not have the same number 
# of factors in train outcomes (actuals) vs. predict
x <- as.integer(animal.predict)
y <- train$OutcomeType
l <- union(x, y)
Table2 <- table(factor(y, l), factor(x, l))
confusionMatrix(Table2)

# Create summary table of actual outcomes vs. predicted outcomes for the test sample
animaltest.predict = predict(animal.nn, test, type = "class")
animaltest.predict <- factor(animaltest.predict)
summary(test$OutcomeType)
summary(animaltest.predict)

#Generate a classification table based on predicted labels and labels of the test data set
nn.table2 = table(test$OutcomeType, animaltest.predict)
nn.table2

#Workaround for Confusion Matrix for test sample when the nn.table does not have the same number 
# of factors in train outcomes (actuals) vs. predict
xt <- as.integer(animaltest.predict)
yt <- test$OutcomeType
lt <- union(xt, yt)
Table3 <- table(factor(yt, lt), factor(xt, lt))
confusionMatrix(Table3)

# Create csv file from R
write.table(NN, "C:/Users/maggi/Documents/Maggi/University San Diego/Data Mining II/Data/Shelter Animal Outcomes/NN.csv") 
# Create csv file from R
write.table(full, "C:/Users/maggi/Documents/Maggi/University San Diego/Data Mining II/Data/Shelter Animal Outcomes/full.csv") 


#Other coding tried out but not utilized citing other program files
#animal = tune.nnet(OutcomeType ~ ., data = train, size = 2, rang = 0.1, decay = 0.0005, maxit = 200)
# Create excel file from R data set
#install.packages("xlsx")
#library(xlsx)
#write.xlsx(full, "~/Maggi/University San Diego/Data Mining II/Data/Shelter Animal Outcomes/Data Mining II/Data/Shelter Animal Outcomes/full.xlsx")

