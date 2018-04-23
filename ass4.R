#1
install.packages("arules")
library("arules")
data(Groceries)
summary(Groceries)
basketSize <- size(Groceries)
hist(basketSize)

#2
frequentSets=eclat(Groceries,parameter=list(support=0.001));
closedGroceries <- is.closed(frequentSets)
maxGroceries <- is.maximal(frequentSets)
sum(closedGroceries)
sum(maxGroceries)
#Groceries[closedGroceries]
#Groceries[maxGroceries]
#itemFreq <- itemFrequency(Groceries)
#Groceries[itemFreq>0.001]
#closedFreq <- itemFrequency(closedGroceries)
#closedGroceries[closedFreq>0.001]
#maxFreq <- itemFrequency(maxGroceries)
#maxGroceries[maxFreq>0.001]

#3
frequentSets1=eclat(Groceries,parameter=list(support=0.01));
closedGroceries1 <- is.closed(frequentSets1)
maxGroceries1 <- is.maximal(frequentSets1)
sum(closedGroceries1)
sum(maxGroceries1)
inspect(sort(frequentSets1,by="support")[1:10]);

#4
# This is the test code, which is not required in the problem
# This is to see when mini support is lowered, whether the similarity still exists. 
frequentSets2=eclat(Groceries,parameter=list(support=0.0005));  
closedGroceries2 <- is.closed(frequentSets2)
maxGroceries2 <- is.maximal(frequentSets2)
sum(closedGroceries2)
sum(maxGroceries2)

#5
Rules <- apriori(Groceries, parameter = list(support = 0.01, confidence = 0.9))  
inspect(Rules)
Rules <- apriori(Groceries, parameter = list(support = 0.01, confidence = 0.8))  
inspect(Rules)
Rules <- apriori(Groceries, parameter = list(support = 0.01, confidence = 0.7))  
inspect(Rules)
Rules <- apriori(Groceries, parameter = list(support = 0.01, confidence = 0.6))  
inspect(Rules)
Rules <- apriori(Groceries, parameter = list(support = 0.01, confidence = 0.5))  
inspect(Rules)

#6
Rules <- apriori(Groceries, parameter = list(support = 0.01, confidence = 0.5)) 
Rules.sub <- subset(Rules, subset = rhs %pin% "whole milk")
inspect(Rules.sub)

#7
inspect(sort(Rules.sub, decreasing=T, by="lift")[1])

