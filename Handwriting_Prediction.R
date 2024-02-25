## Below assume that you save the datasets in the folder ‘‘C://Temp" in your laptop 

library(lattice) 

library(ggplot2) 

library(reshape2) 



ziptrain <- read.table(file="zip.train.csv", sep = ","); 

ziptrain56 <- subset(ziptrain, ziptrain[,1]==5 | ziptrain[,1]==6); 



## some sample Exploratory Data Analysis 

dim(ziptrain56); ## 1220 257 

sum(ziptrain56[,1] == 5); ##556 

sum(ziptrain56[,1] == 6); ##664 

summary(ziptrain56); 

corr_mat <- round(cor(ziptrain56),2); 



melted_corr_mat <- melt(corr_mat) 

melted_corr_mat 



ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2, fill=value)) 

geom_tile() 





## To see the letter picture of the 10-th row by changing the row observation to a matrix 

rowindex = 10; ## You can try other "rowindex" values to see other rows 

ziptrain56[rowindex, 1]; 

Xval = t(matrix(data.matrix(ziptrain56[,-1])[rowindex,],byrow=TRUE,16,16)[16:1,]); 

image(Xval,col=gray(0:1),axes=FALSE) ## Also try "col=gray(0:32/32)" 

### 2. Build Classification Rules 

### linear Regression 

mod1 <- lm( V1 ~ . , data= ziptrain56); 

mod1 

pred1.train <- predict.lm(mod1, ziptrain56[,-1]); 

y1pred.train <- 5 + (pred1.train >= 5.5); 



## Note that we predict Y1 to $5$ and $6$, 

## depending on the indicator variable whether pred1.train >= 5.5 = (5+6)/2. 

mean( y1pred.train != ziptrain56[,1]); 

## KNN 

library(class); 

kk <- 100; ##KNN with k=3 

xnew <- ziptrain56[,-1]; 

ypred2.train <- knn(ziptrain56[,-1], xnew, ziptrain56[,1], k=kk); 

mean( ypred2.train != ziptrain56[,1]) 

### 3. Testing Error 

### read testing data 

ziptest <- read.table(file="zip.test.csv", sep = ","); 

ziptest56 <- subset(ziptest, ziptest[,1]==5 | ziptest[,1]==6); 

dim(ziptest56) ##330 257 

## Testing error of KNN, and you can change the k values. 

xnew2 <- ziptest56[,-1]; ## xnew2 is the X variables of the "testing" data 

kk <- 3; ## below we use the training data "ziptrain56" to predict xnew2 via KNN 

ypred2.test <- knn(ziptrain56[,-1], xnew2, ziptrain56[,1], k=kk); 

mean( ypred2.test != ziptest56[,1]) ## Here "ziptest56[,1]" is the Y response of the "testing" data 

### 4. Cross-Validation 

### The following R code might be useful, but you need to modify it. 

zip56full = rbind(ziptrain56, ziptest56) ### combine to a full data set 

n1 =dim(ziptrain56)[1]; # training set sample size 

n2= dim(ziptest56)[1]; # testing set sample size 

n = dim(zip56full)[1]; ## the total sample size 

set.seed(7406); ### you can also set othernumber for randomization seed if you want 

### Initialize the TE values for all models in all $B=100$ loops 

B= 100; ### number of loops 

TEALL = NULL; ### Final TE values 

for (b in 1:B){ 
  
  ### randomly select n1 observations as a new training subset in each loop 
  
  flag <- sort(sample(1:n, n1)); 
  
  zip56traintemp <- zip56full[flag,]; ## temp training set for CV 
  
  zip56testtemp <- zip56full[-flag,]; ## temp testing set for CV 
  
  ### you need to write your own R code here to first fit each model to "zip56traintemp" 
  
  ### then get the testing error (TE) values on the testing data "zip56testtemp" 
  
  ### 
  
  #LM 
  
  mod1 <- lm( V1 ~ . , data= zip56traintemp); 
  
  pred1.train <- predict.lm(mod1, zip56testtemp[,-1]); 
  
  y1pred.train <- 5 + (pred1.train >= 5.5); 
  
  
  
  te0 <- mean( y1pred.train != zip56testtemp[,1]); 
  
  
  
  #KNN 
  
  xnew3 <- zip56testtemp[,-1]; 
  
  kk <- 1;  
  
  ypred2.test <- knn(zip56traintemp[,-1], xnew3, zip56traintemp[,1], k=kk); 
  
  te1 <- mean( ypred2.test != zip56testtemp[,1])  
  
  
  
  xnew3 <- zip56testtemp[,-1]; 
  
  kk <- 3;  
  
  ypred2.test <- knn(zip56traintemp[,-1], xnew3, zip56traintemp[,1], k=kk); 
  
  te2 <- mean( ypred2.test != zip56testtemp[,1])  
  
  
  
  kk <- 5;  
  
  ypred2.test <- knn(zip56traintemp[,-1], xnew3, zip56traintemp[,1], k=kk); 
  
  te3 <- mean( ypred2.test != zip56testtemp[,1]) 
  
  
  
  kk <- 7;  
  
  ypred2.test <- knn(zip56traintemp[,-1], xnew3, zip56traintemp[,1], k=kk); 
  
  te4 <- mean( ypred2.test != zip56testtemp[,1]) 
  
  
  
  kk <- 9;  
  
  ypred2.test <- knn(zip56traintemp[,-1], xnew3, zip56traintemp[,1], k=kk); 
  
  te5 <- mean( ypred2.test != zip56testtemp[,1]) 
  
  
  
  kk <- 11;  
  
  ypred2.test <- knn(zip56traintemp[,-1], xnew3, zip56traintemp[,1], k=kk); 
  
  te6 <- mean( ypred2.test != zip56testtemp[,1]) 
  
  kk <- 13;  
  
  ypred2.test <- knn(zip56traintemp[,-1], xnew3, zip56traintemp[,1], k=kk); 
  
  te7 <- mean( ypred2.test != zip56testtemp[,1]) 
  
  kk <- 15;  
  
  ypred2.test <- knn(zip56traintemp[,-1], xnew3, zip56traintemp[,1], k=kk); 
  
  te8 <- mean( ypred2.test != zip56testtemp[,1]) 
  
  
  
  ### IMPORTANT: when copying your codes in (2) and (3), please change 
  
  ### the datasets "ziptrain56" and "ziptest56" to 
  
  ### these temp datasets, "zip56traintemp" and "zip56testtemp" !!! 
  
  ### Otherwise you are not doing cross-validations! 
  
  ### 
  
  ### Within each b-th loop, you are given two temp datasets, 
  
  ### you need to write your own codes to use the temp training datae "zip56traintemp" 
  
  ### to fit one of these 9 methods (1 linear regression and 8 KNN); 
  
  ### and then evaluate the performance of these 9 methods 
  
  ### on the temp testing dataset "zip56testtemp" by reporting the correpsonding TE value 
  
  ### 
  
  ### Suppose you save the TE values for these 9 methods (1 linear regression and 8 KNN) as 
  
  ### te0, te1, te2, te3, te4, te5, te6, te7, te8 respectively, within this loop 
  
  ### Then you can save these $9$ Testing Error values by using the R code 
  
  ### Note that the code is not necessary the most efficient 
  
  TEALL = rbind( TEALL, cbind(te0, te1, te2, te3, te4, te5, te6, te7, te8) ); 
  
  ### 
  
  ### Of course, if you want, you can also report the cross-validation training errors 
  
  ### In many applications, cross-validation testing errors are the most important. 
  
} 

dim(TEALL); ### This should be a Bx9 matrices 



### if you want, you can change the column name of TEALL 

colnames(TEALL) <- c("linearRegression", "KNN1", "KNN3", "KNN5", "KNN7", 
                     
                     "KNN9", "KNN11", "KNN13", "KNN15"); 

## You can report the sample mean/variances of the testing errors so as to compare these models 

plot(apply(TEALL, 2, mean)); 

apply(TEALL, 2, var); 

