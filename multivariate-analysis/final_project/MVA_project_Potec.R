#######################################
#        POTEC MVA project            #
#   Diego Garcia-Olano, Elsa Mullor   #
#######################################

library(FactoMineR)
library(cluster)
library(class)
library(gtools)
library(xtable)

########################################
#        Read the data                 #
########################################
set.seed(10062014)
potec <- 
  read.table("Adult.txt",
             header=FALSE, sep=",", na.strings="NA", dec=".")
names(potec)<-c('age','workclass','fnlwgt','education','education.num','marital.status','occupation','relationship','race','sex','capital.gain','capital.loss','hours.per.week','native.country','target')


########################################
#        Pre-Processing                #
########################################

############## NA Values
#originally ' ?' is a level, so assigned to NA
for (i in 1:15) { potec[potec[,i]==' ?',i]<-NA}
summary(potec)


##############  Outliers detection
#this function is used to compute the initial mahalanobis distance
computeDistances <- function(x,G,V)
{
  lx <- x
  lg <- G
  lv <- V
  s <- svd(lv)
  D <- diag(1/s$d)
  linv <- s$v %*% D %*% t(s$u)
  distances <- seq(0,by=0, length = nrow(lx)) 
  for(i in 1:nrow(lx))
  {
    xi_minus_g <- as.matrix(lx[i,] - lg)
    maha_dist <- (xi_minus_g %*% linv) %*% t(xi_minus_g)
    distances[i] <- sqrt(maha_dist) 
  }
  distances 
}

# This function is used to compute the robust mahalanobis distance
loop.mahalanobis <- function (Dataset) {
  
  Bool <- FALSE
  s<-svd(cov(Dataset))
  D<-diag(1/s$d)
  Cov_inv <- s$v%*%D%*%t(s$u)
  Dm <- rep(0, nrow(Dataset))
  means <- colMeans(Dataset)
  n <- length(Dm)
  h <- round(0.75*n)
  Matrix <- Dataset
  
  while (n>2 && Bool == FALSE) 
  {
    
    for (i in 1:n)
    { centralised  <- as.matrix(Matrix[i,] - means)
      mahasq <- centralised %*%  Cov_inv %*% t(centralised)
      Dm[i] <- sqrt(abs(mahasq))
    }
    Sorted_Dm <- sort.int(Dm, decreasing=TRUE,index.return=TRUE)
    New_index <- Sorted_Dm$ix[1:h]
    New_matrix <- Dataset[New_index,]
    
    s <- svd(cov(New_matrix))
    D <- diag(1/s$d)
    Cov_inv_new <- s$v %*% D %*% t(s$u)
    
    means_new <- colMeans(New_matrix)
    n <- h
    h <- round(0.75*n) 
    
    if (Cov_inv_new == Cov_inv && means_new == means)
    {Bool <- TRUE}
    else {
      Cov_inv <- Cov_inv_new
      means <- means_new 
      Matrix <- New_matrix
    }
  }  
  return(Dm)
}

x <- potec[,c(1,3,5,11,12,13)]   #get only numeric columns
G <- as.matrix(colMeans(x))
V <- cov(x) 

initial.distances <- computeDistances(x,G,V)
DMahalanobis.robust <- loop.mahalanobis(x)

#plot with outlier detection
plot(initial.distances, DMahalanobis.robust)
h = qchisq(.975,df=5)
abline(h = h, lty = 2, col = "red")
abline(v = h, lty = 2, col = "red")
outliers <- which(DMahalanobis.robust > h)

# those are the outliers
outliers<- c(24511,24639,24674,24851,24984,25179,25373,25612,25634,25842,26084,26415,26443,26594,26826,27078,27222,27359,27414,27636,27641,28055,28215,28265,28295,28319,28350,29636,29807,30245,30497,30914,31112,31829,31973,32091,32239,32519)

# the weights are changed accordingly
w<-rep(1,dim(potec)[1])
w[outliers]<-0.00001

#Summary of outliers
summary(potec[outliers,c(1,3,5,11,12,13)])
#Summary of non outliers
summary(potec[-outliers,c(1,3,5,11,12,13)])
#capital gains of outliers
median(potec[outliers,c(11)])
#capital gains of nonoutliers
median(potec[-outliers,c(11)])

#######################  Factorization

cont<-c(3,5,11,12,13)  #the continuous variables that will be split into quartiles
for (i in cont){potec[,i]<-quantcut(potec[,i])} 
for(i in cont) levels(potec[,i]) <- paste(colnames(potec)[i],levels(potec[,i]))

# Age variable
potec[,1]<- cut(potec$age, breaks = c(0,25,35,49,64,90))
levels(potec[,1])<-c("age:25 and under", "age:26 to 35", "age:36 to 49", "age:50 to 64", "age:65 and up")


###################### Dealing with NA values
naval<-c(2,7,14)  # the variables containing NA values

# will set NA as a level: level_NA
for (i in naval){ 
  potec[,i]<- factor(potec[,i], levels = c("level_NA",levels(potec[,i])[-1]))
  potec[is.na(potec[,i]),i]<-'level_NA'
}
summary(potec)

########################################
#               MCA                    #
########################################

illus=c(15) #all the variables (except the target) are active ones


res.mca <- MCA(potec, quali.sup=illus,row.w=w)  # MCA with weights (outliers)

#####################  Plots
#plot of everything
plot(res.mca,label=c("var","quali.sup","quanti.sup"))   # too loaded


plot(res.mca,invisible=c("ind","quanti.sup","quali.sup"),autoLab="y",cex=0.7) # plot active variables
plot(res.mca,invisible=c("ind"),cex=0.7, selectMod="contrib 20", unselect="grey70") # 20 variables contributed most
plot(res.mca,invisible=c("ind"),autoLab="y",cex=0.7,selectMod="cos2 20",unselect="grey70") # 20 Variables most correlated
plot(res.mca, invisible=c("ind","var")) # illustrative variable (target modalities)

#plot of individuals  (not in the report)
plot(res.mca,invisible=c("var","quali.sup"),autoLab="y",cex=0.7) # individus

##################### Description of dimensions

dimdesc(res.mca)

##################### Eigenvalues

plot(res.mca$eig$eigenvalue, type="l") #plot of eigenvalues according to dimensioins
abline(v = 51, lty = 2, col = "grey70")

res.mca$eig[res.mca$eig[1]>1/14,,] #51 dimensions  ! keep eig > 1/(Number actives variables)
# so nd=51

sum(res.mca$eig[1])/109  #109 is the total should be the total number of dimensions
res.mca$eig[res.mca$eig[1]>0.07142857,,] # also 51 dimensions with this rule  (mean)


########################################
#           Clustering                 #
########################################
res.mca2 <- MCA(potec, ncp=51, quali.sup=illus,row.w=w) # redo MCA with 51 dimensionns kept
Psi<-res.mca2$ind$coord[,1:51] # Projetion of individuals on 51 kept dimensions

# CLUSTERING OF LARGE DATA SETS
#################### FIRST 2 KMEANS WITH K=12
n1 = 12   # arbitrary: can be changed
k1 <- kmeans(Psi,n1)
k2 <- kmeans(Psi,n1)
table(k2$cluster,k1$cluster)
clas <- (k2$cluster-1)*n1+k1$cluster
summary(clas) # 144 clusters (cross table of k1 and k2)
freq <- table(clas) # number of elts in each cluster
cdclas <- aggregate(as.data.frame(Psi),list(clas),mean)[,2:52] #52=nd+1, center of gravity of cells of the cross table

##################### SECOND HIERARCHICAL CLUSTERING UPON THE CENTROIDS OF CROSSING THE 2 KMEANS PARTITIONS
d2 <- dist(cdclas)
h2 <- hclust(d2,method="ward",members=freq) # Tree with ward criteria
plot(h2)
barplot(h2$height[(nrow(cdclas)-50):(nrow(cdclas)-1)]) # plot of the last 50 aggregations
nc = 5  # cut after 4th jump, will keep 5 clusters
c2 <- cutree(h2,5)  # cut the tree accordingly
cdg <- aggregate((diag(freq/sum(freq)) %*% as.matrix(cdclas)),list(c2),sum)[,2:52] # final center of gravity of clusters

##################### Plot of clustering
plot(Psi[,1],Psi[,2],type="n",main="Clustering of individuals in 5 classes")
text(Psi[,1],Psi[,2],col=c2,cex = 0.6)
abline(h=0,v=0,col="gray")
legend("topright",c("c1","c2","c3","c4","c5"),pch=20,col=c(1:5))

# to help vizualising (not in the report) plot of the individuals colored according to the target.
plot(Psi[,1],Psi[,2],type="n",main="target distribution") 
text(Psi[,1],Psi[,2],col=unclass(potec[,15]),cex = 0.6)
legend("topright",levels(pote[,15]),pch=20,col=c(1:2))
abline(h=0,v=0,col="gray")

##################### CONSOLIDATION
k6 <- kmeans(Psi,centers=cdg) 
k6$size  #size of the clusters 8259 2449 7917 8184 5752

##################### plot of the consolidatedclustering
plot(Psi[,1],Psi[,2],type="n",main="Clustering of individuals in 5 classes")
text(Psi[,1],Psi[,2],col=unclass(k6$cluster),cex = 0.6)
abline(h=0,v=0,col="gray")
legend("topright",c("c1","c2","c3","c4","c5"),pch=20,col=c(1:5))

##################### Description of clusters
potec.comp = cbind.data.frame(potec,k6$cluster) # A dataset with the cluster assignment
potec.comp[,16]<-as.factor(potec.comp[,16])
pot<-potec.comp[,-c(15)] # don't want to describe the target with our clustering (predicted variable)
CatDes<-catdes(pot,num.var=15)
CatDes$category

########################################
#           Prediction                 #
########################################
library(party)
library(RWeka)
library(partykit)
library(caret)
library(e1071)
library(rpart)


################### Split data into Training/Testing set
N<-dim(potec)[1]
learn<-sample(1:N,round(2*N/3))
nlearn<-length(learn)
ntest<-N-nlearn

################### Parameter optimization C4.5
c_sample <-seq(0.05,0.50,by=0.05)
#length(c_sample)

# create fixed sampling scheme (10-folds)
train <- createFolds(potec$target, k=10)

### (Prediction Tree)  the fitting of parameters will be done on train set, using 10 fold CV
C45Fit <- train(potec[learn,-15], potec[learn,15], "J48",
                tuneLength = 10, 
                tuneGrid=expand.grid(.C=c_sample),
                trControl = trainControl(
                  method = "cv", indexOut = train, repeats=10))

plot(C45Fit$results[,1],C45Fit$results[,2],type='l') # acuracy according to tested parameters
C45Fit  # accuracy keep increasing with C, so final model kept: 0.5
C45Fit$results   # table accuracy/parameters
C45Fit$bestTune  # best parameter
C45Fit$finalModel # can see whole description of tree (it is quite long)

#################### Build the model C4.5
# at this point we use the best parameter (c=0.5) to make a prediction with all training data (no more CV)
model.tree<-J48(target~., data=potec, subset=learn, control=Weka_control(C=0.5), na.action=NULL)

#################### Errors C4.5
# Training sample
pred.learn<-predict(model.tree, data=potec[learn])
tab<-table(pred.learn,potec$target[learn]) # contingency matrix
(error.learn<-100*(1-sum(diag(tab))/nlearn))  #12%  => This model is selected

# Test sample
pred.test<-predict(model.tree, newdata=potec[-learn,])#subset=-learn
tab<-table(pred.test,potec$target[-learn]) # contingency matrix
(error.test<-100*(1-sum(diag(tab))/ntest))  #17%

####################### Parameter optimization with CART

learndata <- potec[learn,]
cp_sample <-seq(0.001,0.01,by=0.0005)
#length(cp_sample)

train <- createFolds(potec$target, k=10)
rpart.fitted <- train(potec[learn,-15], potec[learn,15], "rpart", weights=w[learn],
                      tuneLength = 19, 
                      tuneGrid=expand.grid(.cp=cp_sample),
                      trControl = trainControl(method = "cv", indexOut = train, repeats=10))

rpart.fitted$results  # parameters/accuracy table
rpart.fitted$bestTune  #best parameter cp = 0.001
rpart.fitted$finalModel # can see whole description of tree

######################## Build model CART
p1 <- rpart(target ~ ., data=learndata,control=rpart.control(cp=0.001), weights=w[learn])

######################## Plot a tree CART (no same complexity parameter because the tree is too big)
#p1 <- rpart(target ~ ., data=learndata,control=rpart.control(cp=0.05), weights=w[learn])
plot(rpart.fitted$results[,1],rpart.fitted$results[,2],type='l')
plot(as.party.rpart(p1),type="extended")

######################## Errors CART
#Training sample
pred.learn<-predict(p1, data=learndata, type="class")
tab<-table(pred.learn,learndata$target)
(error.learn<-100*(1-sum(diag(tab))/nlearn))  #15%  => This model isn't selected

# Testing sample
pred.test<-predict(p1, newdata=potec[-learn,], type="class")
tab.test<-table(pred.test,potec$target[-learn])
(error.test<-100*(1-sum(diag(tab.test))/ntest))  #16%

##################### To test the impact of outliers 
# Here we don't re-write all the call of different functions but just indicate how we change our training/testing set.
# use as training data
potec.without.outliers <- potec[-outliers,]
Nw<-dim(potec.without.outliers)[1]
learn<-sample(1:Nw,round(2*Nw/3))

potec.without.outliers[learn,]   # training set
potec.without.outliers$target[learn]    # corresponding target to compute error


# and as testing data
potec.without.outliers[-learn,]   #testing data without outliers
potec.without.outliers$target[-learn]   # corresponding target to compute error

test.tot<-as.data.frame(rbind(potec.without.outliers[-learn,],potec[outliers,]))  # testing data with outliers
test.tot$target      # corresponding target to compute error