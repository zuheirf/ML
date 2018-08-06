########################Logistic Regression ########################

Adher = read.csv("training_data.csv")
View(Adher)
str(Adher)
library(caTools)

# Step 1: Divide data into training and test

Sample_Adher = sample.split(Adher$Adherence,SplitRatio = .7)
Sample_Adher

Adher_Train = Adher[Sample_Adher,]
View(Adher_Train)

Adher_Test = Adher[!Sample_Adher,]
View(Adher_Test)


# Step 2: Run model on Training data frame

Adher_Log = glm(Adherence~.,Adher_Train,family="binomial")
summary(Adher_Log)

# Step 3: Drop insignificant variables

Adher_Log = glm(Adherence~Age+Prescription_period+Alcoholism+HyperTension+Smokes,Adher_Train,family="binomial")


#step 4: Predicting test dataset

Adher_test_Pred = predict(Adher_Log,Adher_Test,type="response")
View(as.data.frame(Adher_test_Pred))

Adher_test_Pred1 = cbind(Adher_Test,Adher_test_Pred)
View(Adher_test_Pred1)

  
# Step 5: with 50% cut off for probability. 
Adher_test_Pred2 = transform(Adher_test_Pred1, Adher_Pred = ifelse(Adher_test_Pred>.5,
                                                                          "YES","NO"))
View(Adher_test_Pred2)
#Result File in csv formate 
write.csv(Adher_test_Pred2,"result5.csv")
# Confusion Matrix 
table(Adher_test_Pred2$Adherence,Adher_test_Pred2$Adher_Pred)
###################################################################
