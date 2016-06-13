library(MASS)
library(nnet)
library(class)
library(rpart)
library(lattice)

#Data load
trPima <- Pima.tr;
tePima <- Pima.te;
tr2Pima <- Pima.tr2;

#Histogram and kernel density plots for each explanatory variable
op<-par(mfrow=c(3,3),oma=c(1,0,2,0),mar=c(1.5,2.5,2.5,1.5));
for (v in 1:7) {
    truehist(trPima[,v],15);
    lines(density(trPima[,v]));
    rug(trPima[,v],col="red");
    title(colnames(trPima)[v]);
}
title("Density plots",outer=TRUE);
par(op);
#Possible densities
#npreg: Poisson
#age: Poisson
#ped: Exponential with outliers
#glu, bp, skin, bmi: binomial/normal
#skin has outliers

#Repeat for class conditional densities
summary(trPima[,1:7]);


#Number of diabetics and non-diabetics
table(trPima[,8])/nrow(trPima)
#n.b. 34% are diabetic so declaring all non-diabetic would result in 34% error
#however every diabetic case would be misclassified, so with an asymmetric loss matrix
#i.e. higher cost of false negatives this classification scheme might be costlier than
#classifying everyone as having diabetes
#Also should use Bayes classifier (see notes on chapter 7)

#Scatter plot matrix
trPima[1:5, ];
splom(~trPima[ ,1:7]);

#Scatter plot matrix with class labels
superpose.sym <-trellis.par.get("superpose.symbol");
superpose.sym;
superpose.sym$pch <- c(1:7);
superpose.sym;
trellis.par.set("superpose.symbol", superpose.sym);
splom(~trPima[ ,1:7],
      groups=trPima$type,
      panel=panel.superpose,
      key = list(text=list(c("No Diabetes", "Diabetes")),
                 points=Rows(trellis.par.get("superpose.symbol"),1:2),
                 columns=2)
      );
#Alternative: plot two separate SPLOMs
splom(~trPima[,1:7]|trPima$type,layout=c(2,1));

#Conclusions from multivariate analysis
#Age/npreg:
#Number of preg pos correl with age with outliers: women who have 1 or less children

#All others/age:
#Glucose pos correl with age despite some young high glucosers
#bp,skin and bmi (and slightly,ped) uncorrelated with age but seem to be heterescedastic,
#smaller conditional variance with increasing age

#All others/npreg:
#None have sig high positive correlation with npreg but all seem to show
#heteroscedastic pattern

#Ped:
#Ped uncorrelated with everything else, shows same conditionally heteroscedasticity numb preg

#Bmi,skin,glu,bp:
#bmi and skin are highly correlated (pearson 0.65)
#bmi weakly correlated with glu and npreg (and heterescedastic wrt npreg,decreasing cond var)
#bmi,skin,glu and bp are all generally positively correlated but only  bmi/skin stands out


#PCA
correl <- cor(trPima[,1:7]);
pc <- eigen(correl);
plot(cumsum(pc$values)/sum(pc$values),type="b");
pc$vectors[,1:5]
#Rerun correlation matrix after removing outliers in skin and ped (based on histograms)
trPima$outlier <- logical(nrow(trPima));
trPima$outlier[trPima$skin>60] <- TRUE;
trPima$outlier[trPima$ped>1.5] <- TRUE;
#Rerun PCA
correl.out <- cor(trPima[!trPima$outlier,1:7]);
pc.out <- eigen(correl.out);
plot(cbind(0:7,c(0,cumsum(pc.out$values))),type="b",ylim=c(0,7))
pc.out$vectors[,1:5]
a <- as.matrix(trPima[!trPima$outlier,1:7]);
a.n <- dim(a)[1]
pc.out$scores <- a %*%  pc.out$vectors;
pc.out$scores <- t(apply(pc.out$scores,1,"/",sqrt(diag(var(pc.out$scores)))));
pc.out$sdev <- sqrt(diag(var(pc.out$scores)));
#something wrong with the PCA scores!! they are much too large
#need to rescale them to unit variance (sphered)
#try with cov
pc.out.v <- eigen(var(trPima[!trPima$outlier,1:7]));
pc.out.v$vectors[,1:5];
pc.out.v$scores <- a %*% pc.out.v$vectors;
#Nope, something still wrong!!!

#Try using princomp function instead - on correlation matrix
pc.out2 <- princomp(trPima[!trPima$outlier,1:7],cor=FALSE);
#scree plot
plot(pc.out2$sdev^2/sum(diag(correl.out)),type="b");
#pc loadings matrix for first 3
pc.out2$loadings[,1:3];
#Class conditional univariate densities in each of first 3 PCs
#density
#Scatter plot matrix in PC-space
pdf("pc-splom.pdf");
splom(pc.out2$scores[,1:3],groups=trPima$type[!trPima$outlier]);
dev.off();

#nb. Using classical MDS with Euclidean distances is equivalent to PCA
#using cov matrix up to a reflection
#cs <- cmdscale(dist(trPima[!trPima$outlier,1:7]),k=7,eig=T);
#pdf("cs-splom.pdf");
#splom(cs$points[,1:5],groups=trPima$type[!trPima$outlier]);
#dev.off();

#PC1/2 and PC1/3 seem to separate the classes
#Do biplots for these PCs
#Turn this into a function with choice of PCs to plot
choices=c(1,2)
biplot(pc.out2,xlabs=rep("",a.n),scale=0,choices=choices);
par(usr=as.vector(apply(pc.out2$scores[,choices],2,range)));
points(pc.out2$scores[,choices],col=as.numeric(trPima$type[!trPima$outlier]));
legend("topright",legend=levels(trPima$type),col=c(1,2),pch=1); #fix legend colours

#Conclusions from PCA:
#PCR because scales are different for all variables
#1) PC1 is 'size of all factors', PC2 is 'age', PC3 is 'health'
#First three PCs account for over 85% of correlation
#2) PC1 seems to have some separation power, others not

#Extensions:
#Robust covariance/correlation matrix to account for outliers instead of trimming them
#'Removing size PC and looking at remaining structure' e.g. scale 1 biplot of cov PCA
#Sammon mapping (nonlinear MDS) sammon()
#Ordinal scaling isoMDS()
#Tours and projection pursuit (rggobi)
#Natural clustering (K-means,SOM,hierarchical clusters)

