#Setting workspace and dataset file
setwd("C:/Users/nolzu.DESKTOP-8HOO4NF/Documents/Uni/Final Year/Final Year Project/Dissertation Research/R") 

quesinput = read.csv("Questionnaire Input.csv", header=T)


#Normalization Function
normalization <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Apply normalization function to the dataset
ques_input <- as.data.frame(lapply(quesinput, normalization))

# Random Sampling
samplesize = 0.60 * nrow(ques_input)
set.seed(80)
index = sample(nrow(ques_input), size = samplesize)

# Training and Test Data
traindata = ques_input[index, ]
testdata = ques_input[-index, ]

#Neural Network
library(neuralnet)

nn <- neuralnet(recidivism ~ age + violent + violentunder + misdemeanour + felony + priorviolent + jail + prison + juvmisdemeanour + juvfelony + marriage, data=traindata)

plot(nn)

#Collect results from the Neural Network while it's training 

#temp_train <- subset(traindata, select = c("age", "violent", "violentunder", "misdemeanour", "felony", "priorviolent", "jail", "prison", "juvmisdemeanour", "juvfelony", "marriage"))
#nn.results <- compute(nn, temp_train)
#train_results <- data.frame(actual = traindata$recidivism, prediction = nn.results$net.result)


#Testing the algorithm
temp_test <- subset(testdata, select = c("age", "violent", "violentunder", "misdemeanour", "felony", "priorviolent", 
                                         "jail", "prison", "juvmisdemeanour", "juvfelony", "marriage"))
nn.results <- compute(nn, temp_test)


#Calculating the Accuracy
results <- data.frame(actual = testdata$recidivism, prediction = nn.results$net.result)


#Results
roundresults <- sapply(results, round, digits=0)
roundresultsdf = data.frame(roundresults)
attach(roundresultsdf)
table(actual,prediction)


#Export Neural Network results to Excel File 

#total <- rbind(train_results, results)

#library(xlsx)
#write.xlsx(total, "C:/Users/nolzu.DESKTOP-8HOO4NF/Documents/Uni/Final Year/Final Year Project/Dissertation Research/R/Routput.xlsx")
