library(MASS)
library(nnet)
library(class)
library(rpart)
library(lattice)

#Data load
trPima <- Pima.tr;
tePima <- Pima.te;
tr2Pima <- Pima.tr2;

#Set up training and test sets
train1 <- trPima[ ,1:7]
trtp <- trPima$type
testx <- tePima[, 1:7]
testtp <- tePima$type

#Estimate LDA
(pima.lda <- lda(train1, trtp))
plot(pima.lda,type="b")
ldatrain <- predict(pima.lda,train1)
plot(density(ldapred$x[ldapred$class=="Yes"]));
lines(density(ldapred$x[ldapred$class=="No"]));

#Assess LDA performance
ldapred <- predict(pima.lda,testx);
ldacmat <- table(ldapred$class,tePima$type)
ldacmat

#Now calculate the misclassification rate and comment on its structure.
#What percentage of diabetics are classified as normal for example?

pima.qda <- qda(train1, trtp)
qdapred <- predict(pima.qda,testx)
qdacmat <- table(qdapred$class,tePima$type)
qdacmat
#Now calculate the misclassification rate – and again consider its structure.
