####################################################################
# Machine Learning - MIRI Master
# Diego Olano and Christine Doig 

# Project Course: 
# Understanding US counties: 
# Clustering and Classification over voting, socio-economic and public health data.
# version of June 2014
####################################################################


####################################################################
# SECTION 0: PRE-PROCESSING
####################################################################

#pre-process steps
#load file
dd <-read.csv("finaldata-withblueislands.csv", na.strings = '', stringsAsFactors = FALSE)
dd[which(dd$blueisland == 1),c(2:5,32:33,96)]

initialmap <- read.csv("finaldata-withblueislands.csv", na.strings = '', stringsAsFactors = FALSE)
#see variables
colnames(dd)

#get rid of state, county, etc columns and make fips the row.names
dd <- dd[,c(2:30,34:96)]
row.names(dd) <- dd$fips

#get rid of fips column
dd <- dd[,2:92]

#get rid of q columns 
qcols = c(30,32,34,36,38,40,42,44,46,48,50,52,55,57,59,60,62,63,65,67,69,70,71,73,75,77)
dd <- dd[,-qcols]

#Change ratio columns to zero or to 1 / number.   for mphratio (mental health provider), dentist and physicianratio
ratiocols <- c(41,58,60)
colnames(dd)[ratiocols]
#[1] "physicianratio" "mphratio"    "dentistratio


for(i in 1:nrow(dd)){
  currenti <- i
  for(ri in ratiocols){    
    pieces <- strsplit(dd[currenti,ri],":")[[1]]
    
    #change only those formatted as ratios ( not including few missing NAs)
    if(length(pieces) > 1)
    {  
      if( pieces[2] == "1"){ 
        dd[currenti,ri] = 1 / as.numeric(pieces[1]) 
      }
      else
      {
        if( pieces[2] == "0") { 
          dd[currenti,ri] = 0
        }
      }
    }
  }
}

#make these columns numeric, "physicianratio" "mphratio"    "dentistratio"
for(i in 1:ratiocols){ index <- ratiocols[i]; dd[,index] <- as.numeric(dd[,index]);}


#from visual inspection, we notice that the population for some is off ( basically we want to verify at least 10% voter turnout)
#get ids that need to be fixed
count = 0
ids = c()
for( fd in 1:nrow(dd)){  
  row <- dd[fd,];  
  pop.shouldbe <- row$dem + row$rep;  
  if((10 *pop.shouldbe) < row$popul){
    count = count+1; 
    ids[count]=fd;
  }
}

#get fips for ids whose populations are suspected
fips.with.populs.to.fix <- as.numeric(rownames(dd[ids,]))

#we need to use the correct populations from an early combined dataset to get them
comb <- read.csv("combineddata.csv")
count = 0;
newpops = c()

#find correct populations from original combined 
for( fi in 1:length(fips.with.populs.to.fix)){ 
  currentfip <- fips.with.populs.to.fix[fi]; 
  comb.fips <- comb[which(comb$FIPSCode == currentfip),]; 
  newpop <- comb.fips[1,]$Xtotalpopulation; 
  #print(paste("for fips: ",currentfip)); 
  #print(paste("--new popul will be: ",newpop));
  count = count + 1
  newpops[count] = newpop
}

#now change populations to be correct in new dataset
for( ni in 1:length(ids)){
  dd[ids[ni],]$popul = newpops[ni]
}


#at end, classification set where target "winner" is either dem or rep.
#get rid of dem and rep voting columns
dd <- dd[,c(3:65)]
#colnames(dd.4)[1] <- "winner"
dd$winner <- dd$isdem
dd <- dd[,2:64]

dd[which(dd$winner == "no"),63] <- "rep"
dd[which(dd$winner == "yes"),63] <- "dem"
dd[,63] <- as.factor(dd[,63])

summary(dd)
write.csv(dd,"finaldata-classificationset.csv",row.names=TRUE)

####DONE WITH PREPROCESSING####

dd <-read.csv("finaldata-classificationset.csv", na.strings = NA, stringsAsFactors = TRUE)
summary(dd)  

row.names(dd) <- dd[,1]
dd <- dd[,-1]

#Imputing missing data: smokers, motordeath, violentcrime, fastfoodresp, sickdays
#all negative values should be NAs
dd[which(dd$smokers < 0 ),]$smokers <- NA
dd[which(dd$motordeath < 0 ),]$motordeath <- NA
dd[which(dd$violentcrime < 0 ),]$violentcrime <- NA
dd[which(dd$fastfoodresp < 0 ),]$fastfoodresp <- NA
dd[which(dd$sickdays < 0 ),]$sickdays <- NA

library(mice)
ddmice <- mice(dd, m=1)
#iter imp variable
#       1   1  smokers  motordeath  physicianratio  violentcrime  fastfoodresp
#       2   1  smokers  motordeath  physicianratio  violentcrime  fastfoodresp
#       3   1  smokers  motordeath  physicianratio  violentcrime  fastfoodresp
#       4   1  smokers  motordeath  physicianratio  violentcrime  fastfoodresp
#       5   1  smokers  motordeath  physicianratio  violentcrime  fastfoodresp
dd <- complete(ddmice)


##1.   dem/rep dataset with target winner
#data.demrep <- dd[,-62]  

#2.   blueisland dataset with target as blueisland
#data.bluei <- dd[,-63]

#Splitting into test and learn with shuffling
N <- nrow(dd)
learn <- sample(1:N, round(4*N/5))
nlearn <- length(learn)
ntest <- N - nlearn



####################################################################
# SECTION 1: BASIC INSPECTION OF THE DATASET
####################################################################

row.names(dd) <- dd[,1]
dd <- dd[,-1]

#Imputing missing data: smokers, motordeath, violentcrime, fastfoodresp, sickdays
#all negative values should be NAs
dd[which(dd$smokers < 0 ),]$smokers <- NA
dd[which(dd$motordeath < 0 ),]$motordeath <- NA
dd[which(dd$violentcrime < 0 ),]$violentcrime <- NA
dd[which(dd$fastfoodresp < 0 ),]$fastfoodresp <- NA
dd[which(dd$sickdays < 0 ),]$sickdays <- NA

library(mice)
ddmice <- mice(dd, m=1)
#iter imp variable
#       1   1  smokers  motordeath  physicianratio  violentcrime  fastfoodresp
#       2   1  smokers  motordeath  physicianratio  violentcrime  fastfoodresp
#       3   1  smokers  motordeath  physicianratio  violentcrime  fastfoodresp
#       4   1  smokers  motordeath  physicianratio  violentcrime  fastfoodresp
#       5   1  smokers  motordeath  physicianratio  violentcrime  fastfoodresp
dd <- complete(ddmice)


##1.   dem/rep dataset with target winner
#data.demrep <- dd[,-62]  

#2.   blueisland dataset with target as blueisland
#data.bluei <- dd[,-63]

#Splitting into test and learn with shuffling
N <- nrow(dd)
learn <- sample(1:N, round(4*N/5))
nlearn <- length(learn)
ntest <- N - nlearn

summary(dd)

####################################################################
# SECTION 1.2: DETECTION OF OUTLIERS AND REDUNDANT VARIABLES
####################################################################


#Types of variables:
#Education: lesshs, hs, leastbach, graduate, enrollp (%enrolled in school between ages 3-24), educationi
education <- dd[,c(1:5,7,17,59)]
head(education)
summary(education)


#Socio-economic status
socio_economic <- dd[,c(6,8,15,18:25,42,45,58)]
head(socio_economic)
summary(socio_economic)

#Health
health <- dd[, c(26:28,30:41,43,46:48,54:57,61)]
head(health)
summary(health)

#Demographics - 
demographics <- dd[,c(9:14,16,29,44,49:53,60)]
head(demographics)
summary(demographics)

#Blueisland
blueisland <- dd[,62]

#winner
winner <- dd[,63]

## Plots for each of the different variable categories:
#Education plots
education <- cbind(education, winner)
summary(education)
length(education)
pairs(education[,1:8], main = "US counties Education Variables by Party", col = (1:length(levels(dd$winner)))[unclass(dd$winner)])
boxplot(education[,1:8], main = "US counties Dataset Education Variables", outline = FALSE)
#Boxplot by party
par(mfrow=c(3,3))
for( i in 1:(length(education)-1)){
  plot(education[,9], education[,i], main=colnames(education)[i])
}

#Conclusion from education variables inspection:
#Variables hs and lesshs are redundant, we should eliminate one of them, we eliminate lesshs.
education <- education[,2:8]
#Visual inspection: graduate and leastbach higher in democrats, more variance in education_index in democrats.


##Socio-economic status plots
socio_economic <- cbind(socio_economic, winner)
length(socio_economic)
pairs(socio_economic[,1:7], main = "US counties Socio-Economic Variables by Party (I)", col = (1:length(levels(dd$winner)))[unclass(dd$winner)])
pairs(socio_economic[,8:14], main = "US counties Socio-Economic Variables by Party (II)", col = (1:length(levels(dd$winner)))[unclass(dd$winner)])
#Redundant: earnings and income, we'll eliminate income, because earnings is easier to understand

par(mfrow=c(1,1))
boxplot(socio_economic[,c(2:12,14)], main = "US counties Dataset by Socio-Economic Variable", outline = FALSE)
par(mfrow=c(3,3))
for( i in 1:(length(socio_economic)-1)){
  plot(socio_economic[,15], socio_economic[,i], main=colnames(socio_economic)[i])
}

#Democrats: tail in violentcrimes, more variance in poverty and freelunch, and republicans higher in farm workers.
par(mfrow=c(1,1))

#Histograms for the one's that look weird.
hist(socio_economic[,13], main = "ViolentCrimes histogram", xlab="violentcrimes")

library("MASS")
bx <- boxcox(I(socio_economic[,13]+1) ~ . - socio_economic$winner, data = socio_economic,
             lambda = seq(-0.25, 0.25, length = 10))

lambda <- bx$x[which.max(bx$y)]

violentcrime.BC <- (socio_economic[,13]^lambda - 1)/lambda

hist(violentcrime.BC, main="Box-cox transformation of ViolentCrime variable")

par (mfrow=c(1,1))


hist(socio_economic[,14], main = "Freelunch histogram", xlab="freelunch")
#Looks ok.

hist(socio_economic[,5], main = "Gini histogram", xlab="gini")
#Looks ok.

#Conclusions on socio-economic variables inspection:
socio_economic <- socio_economic[,1:12]
socio_economic <- socio_economic[,-2]
socio_economic$violentcrime <- violentcrime.BC

#Health
health <- cbind(health, winner)
length(health)
pairs(health[,1:7], main = "US counties Health Variables by Party (I)", col = (1:length(levels(dd$winner)))[unclass(dd$winner)])
pairs(health[,8:15], main = "US counties Health Variables by Party (II)", col = (1:length(levels(dd$winner)))[unclass(dd$winner)])
pairs(health[,16:24], main = "US counties Health Variables by Party (III)", col = (1:length(levels(dd$winner)))[unclass(dd$winner)])
#Mhp looks like has an outlier.
head(health)
boxplot(health[,1:7], main = "US counties Dataset by Health Variables (I)", outline = FALSE)
boxplot(health[,8:15], main = "US counties Dataset by Health Variables (II)", outline = FALSE)
boxplot(health[,c(16:20,22)], main = "US counties Dataset by Health Variables (III)", outline = FALSE)
boxplot(health[,22], main = "US counties Dataset by Health Costs", outline = FALSE)
hist(health[,22], main = "US counties Dataset by Health Costs")
summary(health)

#Taking a deeper look into the mental_health_providers variable
#Outlier: 36073, Orleans, NY, the data is correct, there is one mental health provider for every 38 citizens.
mhp_scaled <- scale(health[,21], center=TRUE)
health <- cbind(health, mhp_scaled)
summary(health)
which(health$mhp_scaled > 5)
health[which((health$mhp_scaled) > 5),]

#List of counties that have a mental health providers higher than 5 std from the mean.
#       limitedaccesshealthyfood fastfoodresp diabectic    mphratio healthcosts dentistratio healthyfood winner mhp_scaled
#36031                        1           22         9 0.004310345        8215 0.0003047851          48    dem   6.743262
#36041                       13           18        11 0.009433962       10173 0.0000000000          33    rep  15.103939
#36073                       17           49         9 0.026315789        7404 0.0001615248          43    rep  42.651564
#36095                       27           30         9 0.009433962        7491 0.0002432498          12    rep  15.103939
#36115                       20           41        10 0.006944444        8804 0.0001586043          33    dem  11.041564
#36119                        2           43         8 0.003759398        9320 0.0011286682          91    dem   5.844233

bx <- boxcox(I(health[,21]+1) ~ . - health$winner, data = health,
             lambda = seq(-0.25, 0.25, length = 10))

lambda <- bx$x[which.max(bx$y)]

mhpratio.BC <- (health[,21]^lambda - 1)/lambda

hist(mhpratio.BC, main="Box-cox transformation of Mental Health providers variable")

#Histogram for std
hist(health[,9], main = "STD per 100 histogram", xlab="stdsper100")
length(which(health$stdsper100 == 2394))
health[2352,]
#Which is fips 46041: Dewey South Dakota. It's an indian reservation 

hist(health[,10], main = "Teen birth rate histogram", xlab="teenbirthrate")
#Teenbirthrate 99?? Teen birthrate the histogram shows over 100.
#Teenbirthrate:  Teen births / females ages 15-19 * 1,000
#teenbirth is over 1000, so it's possible to have values over 100.

par(mfrow=c(3,3))
for( i in 1:(length(health)-1)){
  plot(health[,25], health[,i], main=colnames(health)[i])
}

#Conclusions for health 
# Mental health providers and STDs per 100 variable not normal and Box-Cox not working.
# DemocratsHigher healthy foods, dentistratio, STDsper100 in democrats, higher variance in adultobese
# Republican: Higher health cost, fairpoorheath, mentaldays, ambulatorycare and diabetics in republican.

# Although we have some outliers, we decide not to do anything about them, since we have checked that they are 
# not errors.
health <- health[,1:24]

#Demographics
demographics <- cbind(demographics, winner)
length(demographics)
pairs(demographics[,1:7], main = "US counties Demographics Variables by Party (I)", col = (1:length(levels(dd$winner)))[unclass(dd$winner)])

pairs(demographics[,8:15], main = "US counties Demographics Variables by Party (II)", col = (1:length(levels(dd$winner)))[unclass(dd$winner)])
par(mfrow=c(1,1))
boxplot(demographics[,1:6], main = "US counties Dataset by Demographics Variables(I)", outline = FALSE)
boxplot(demographics[,7], main = "US counties Dataset by Demographics Variables: Population", outline = FALSE)
boxplot(demographics[,8:14], main = "US counties Dataset by Demographics Variables", outline = FALSE)

par(mfrow=c(3,3))
for( i in 1:(length(demographics)-1)){
  plot(demographics[,16], demographics[,i], main=colnames(demographics)[i])
}

#Conclusions for demographics 
# Democrats: Higher noenglish, more variance in under 18, higher african-american, higher singleparent,
# Republican: Higher over65, higher rural,higher white.
demographics <- demographics[,1:15]

#Bind final dataset with transformations and eliminating redundant variables
dd.winner <- cbind(education, socio_economic,health, demographics, winner)
head(dd.winner)
dim(dd.winner)

dd.blueisland <- cbind(education, socio_economic,health, demographics, blueisland)
head(dd.blueisland)
dim(dd.blueisland)

#############################
# SECTION 3. FEATURE SELECTION
##############################

#############################
# SECTION 3.1 F-FISHER
##############################

# Feature selection for continuous variables: Fisher's F

pvalcon <- NULL

varc <- c(1:61)

for (i in 1:length(varc)) 
  pvalcon[i] <- (oneway.test (dd[,i]~dd$winner))$p.value

pvalcon = matrix(pvalcon)
row.names(pvalcon) = colnames(dd[,1:61])

# Ordered list of continuous variables according to their association to Winner

winner_corr <- sort(pvalcon[,1])
winner_corr
#Top 10 most correlated: white, wcontruct, rural, stdsper100, singleparent, graduate, preschl, leastbach, over65, ambulatorycare

pvalcon <- NULL

varc <- c(1:61)

for (i in 1:length(varc)) 
  pvalcon[i] <- (oneway.test (dd[,i]~dd$blueisland))$p.value

pvalcon = matrix(pvalcon)
row.names(pvalcon) = colnames(dd[,1:61])

# Ordered list of continuous variables according to their association to Winner

blueisland_corr <- sort(pvalcon[,1])
blueisland_corr
#Top 10 most correlated: gini, stdsper100, wtransport, white, excsdrinking, graduate, physicianratio, singleparent, leastbach, wconstruct

#############################
# SECTION 3.2 CFS FILTER
##############################

require(FSelector)

#FOR DEM/REP CLASSIFIER
data.demrep <- dd.winner

subset.CFS <- cfs (winner~., data.demrep)
(subset.CFS.formula <- as.simple.formula(subset.CFS,"winner"))

subset.vars <- c("leastbach","graduate","white","afric","other","popul","preschl","belowpov","wconstruct","lowbirthrate","adultobese","excsdrinking","motordeath","stdsper100","physicianratio","unemployed","noemotionalsupport","singleparent","violentcrime","female","rural","mphratio","illiteracy","winner")
subset.vars.ids <- c()
for( i in 1:length(subset.vars)){ col <- subset.vars[i]; subset.vars.ids[i]<- which(colnames(data.demrep)==col);}


# FOR BLUEISLAND CLASSIFIER
#We can get a subset features using CFS
data.bluei <- dd.blueisland 
summary(data.bluei)
dim(data.bluei[(which(blueisland == 1)),])
subset.CFS <- cfs (blueisland~., data.bluei)
(subset.CFS.formula <- as.simple.formula(subset.CFS,"blueisland"))
#blueisland ~ white + nativ + other + gini + stdsper100 + physicianratio + diabeteshba1c + under18 + over65 + mphratio

subset.vars.b <- c("white","nativ","other","gini","stdsper100","physicianratio","diabeteshba1c","under18","over65","mphratio","blueisland")
subset.vars.b.ids <- c()
for( i in 1:length(subset.vars.b)){ col <- subset.vars.b[i]; subset.vars.b.ids[i]<- which(colnames(data.bluei)==col);}

colnames(data.bluei)[subset.vars.b.ids]


####################################################################
# SECTION 3.3. FEATURE SELECTION WITH FILTER+WRAPPER: BORUTA
####################################################################

# Since we have 58 active variables, we would like to simplify the model by performing Feature Selection on the Dataset.
# We do this with a filter + wrapper method implemented in the Boruta package.
require(Boruta)

# Comparing original attributes' importance 
# with importance achievable at random, using permuted copies several times; 
# importance is estimated with a Random Forest (though it could be another learner)

# WARNING: this call takes about 1 hour, you may don't want to execute it
weights.Boruta <- Boruta (winner ~ ., data=dd,doTrace=2)

# The outcome is:

#Initial round 1: .......
#Initial round 2: .......
#Initial round 3: .......
#Final round: .............
#58  attributes confirmed after this test:  lesshs hs leastbach graduate enrollp earnings educationi incomei white afric nativ asian other latin povo65 popul preschl belowpov gini childpov.x wmanage wsales wfarm wconstruct wtransport fairpoorhealth sickdays lowbirthrate smokers adultobese phyinactive excsdrinking motordeath stdsper100 teenbirthrate uninsured.x physicianratio ambulatorycare mammography unemployed noemotionalsupport singleparent violentcrime recfac limitedaccesshealthyfood under18 over65 noenglish female rural diabectic mphratio healthcosts dentistratio freelunch illiteracy drivealone healthyfood blueisland 

# Now we get the result

subset.Boruta <- names(weights.Boruta$finalDecision[ weights.Boruta$finalDecision %in% c("Confirmed") ])


##################################################################################
# SECTION 4: CLUSTERING US counties based on socio-economical/public health data #
##################################################################################

##################################################################################
# SECTION 4.1: PCA
##################################################################################

par(mfrow=c(1,1))
dim(dd.winner)
summary(dd.winner)

dd.winner.scale <- scale(dd.winner[,1:58], center=TRUE)
winner <-dd.winner[,59]
head(winner)
winner <- as.data.frame(winner)
dd.winner.scale <- cbind(dd.winner.scale, winner)
head(dd.winner.scale)
library(FactoMineR)
library(ggplot2)
?PCA
pca <- PCA(dd.winner.scale, quali.sup = 59, ncp=15)
pca
pca$eig
par(mfrow=c(1,1))
plot(pca, invisible="ind")
?PCA
#> pca$eig
#eigenvalue percentage of variance cumulative percentage of variance
#comp 1  1.426128e+01           2.458841e+01                          24.58841
#comp 2  6.881655e+00           1.186492e+01                          36.45333
#comp 3  4.249955e+00           7.327509e+00                          43.78084
#comp 4  2.877806e+00           4.961734e+00                          48.74257
#comp 5  2.153099e+00           3.712240e+00                          52.45481
#comp 6  2.036962e+00           3.512003e+00                          55.96682
#comp 7  1.721130e+00           2.967466e+00                          58.93428
#comp 8  1.409703e+00           2.430523e+00                          61.36480
#comp 9  1.265619e+00           2.182101e+00                          63.54691
#comp 10 1.178193e+00           2.031367e+00                          65.57827
#comp 11 1.132237e+00           1.952132e+00                          67.53040
#comp 12 1.013729e+00           1.747809e+00                          69.27821
#comp 13 9.905072e-01           1.707771e+00                          70.98599
#comp 14 9.526116e-01           1.642434e+00                          72.62842
#comp 15 8.864419e-01           1.528348e+00                          74.15677
#comp 16 8.465309e-01           1.459536e+00                          75.61630
#comp 17 7.919384e-01           1.365411e+00                          76.98171
#comp 18 7.532477e-01           1.298703e+00                          78.28042
#comp 19 6.972535e-01           1.202161e+00                          79.48258
#comp 20 6.692309e-01           1.153846e+00                          80.63642


#Screeplot
plot(pca$eig[,1], type="l", main = "Screeplot")
eigen <- pca$eig[,1]
iter <- c(1:58)
screeplot <- cbind(iter, data.frame(eigen))
summary(screeplot)
ggplot(screeplot, aes(x= iter,y=eigen)) + geom_point(size=2) + geom_line() + xlab("index") +
  ylab("eigenvalue") + ggtitle("Screeplot")


#PCA plots
pca
pca_quali_coord <- pca$quali.sup$coord
pca_quali_coord
pca_ind_coord <- pca$ind$coord
pca_var_coord <- pca$var$coord
pca_ind_coord  <- data.frame(pca_ind_coord)
pca_var_coord <- data.frame(pca$var$coord)
pca_quali_coord <-data.frame(pca$quali.sup$coord)
pca_ind_coord

circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

#Plot PCA plot of individuals
winner <- dd.winner[,59]
pca_ind_coord <- cbind(pca_ind_coord, winner)
length(pca_ind_coord$Dim.1)
length(pca_ind_coord$winner)
ggplot(data = pca_ind_coord, aes(x = Dim.1, y = Dim.2, colour=winner)) + geom_point() + geom_hline(yintercept = 0, colour = "black") + geom_vline(xintercept = 0, colour = "black") + geom_point()

ggplot(data = pca_ind_coord, aes(x = Dim.1, y = Dim.2, colour=winner)) + geom_hline(yintercept = 0, colour = "gray70") + geom_vline(xintercept = 0, colour = "gray70") + geom_point(colour = "gray50", alpha = 0.7) + geom_density2d(colour = "gray80") + ggtitle("PCA plot of individuals and winner")

head(pca_ind_coord)

#With circle
dat <- circleFun(c(0,0),2.3,npoints = 100)
#geom_path will do open circles, geom_polygon will do filled circles
plot <- ggplot(dat,aes(x,y)) + geom_path()
#plot <- plot + ggplot(data = pca_var_coord, aes(x = Dim.1, y = Dim.2)) + geom_point() + geom_hline(yintercept = 0, colour = "gray70") + geom_vline(xintercept = 0, colour = "gray70") + geom_text() + ggtitle("PCA plot of active variables")
plot <- plot + geom_hline(aes(0), size=.2) + geom_vline(aes(0), size=.2)
plot <- plot + coord_equal() + geom_text(data=pca_var_coord, aes(x=pca_var_coord$Dim.1, y=pca_var_coord$Dim.2, label= rownames(pca_var_coord)), size = 5, vjust=1, color="red")
plot <- plot + geom_segment(data=pca_var_coord, aes(x=0, y=0, xend=pca_var_coord$Dim.1, yend=pca_var_coord$Dim.2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="red")
plot <- plot + geom_hline(yintercept = 0, colour = "black") + geom_vline(xintercept = 0, colour = "black")
plot <- plot + xlab("Dim1") + ylab("Dim2") + ggtitle("Variables factor map PCA")
pca_quali_coord
plot

#> pca_quali_coord
#           Dim.1      Dim.2       Dim.3      Dim.4       Dim.5       Dim.6       Dim.7       Dim.8       Dim.9      Dim.10
#V59 1 -0.8003041  2.4346082 -0.06647588  0.6560869 -0.15973886 -0.26057661 -0.30679586 -0.08364379  0.21800871 -0.35043036
#V59 2  0.2262087 -0.6881505  0.01878964 -0.1854452  0.04515075  0.07365289  0.08671692  0.02364221 -0.06162092  0.09905036



#### VARIMAX ####
ndim <- 8
Psi <- pca$ind$coord[, 1:ndim]
Phi <- pca$var$coord[,1:ndim]

pc.rot <- varimax(Phi)

X
p <- 58
Phi.rot <- pc.rot$loadings[1:p,]

library(calibrate)
ze <- rep(0,p)
#Plot Rotaded PCA Dim1 and Dim2
plot(Phi.rot, type="n", xlim=c(-1,1),ylim=c(-1,1))
text(Phi.rot, labels=colnames(dd.winner[1:58]), col="blue")
arrows(ze, ze, Phi.rot[,1], Phi.rot[,2], length = 0.07,col="blue")
abline(h=0,v=0,col="gray")
circle(1)

Phi.rot

###### END VARIMAX ########


## CLUSTERING

# We apply hierarchical clustering (ward method) plus k-means consolidation
dist.Psi <- dist(Psi, method = "euclidean")
hier.clust <- hclust(dist.Psi, method = "ward")

plot(hier.clust)

nclusters <- 2
clusters <- cutree(hier.clust, nclusters)

cdg <- aggregate(as.data.frame(Psi), list(clusters), mean)[,2:(ndim+1)]
k <- kmeans(as.data.frame(Psi), centers = cdg)
Bss <- sum(rowSums(k$centers^2)*k$size)
Wss <- sum(k$withinss)
100*Bss/(Bss+Wss)
par(mfrow=c(1,2))
plot(Psi[,1], Psi[,2], col = as.factor(dd.winner$winner), main ="By winner")
plot(Psi[,1], Psi[,2], col = clusters, main="By cluster")

table(clusters,winner)
table(dd.winner[,59],clusters)
table(dd.winner[,59],clusters)
#clusters
#       1    2
#dem  445  241
#rep 1280 1147

nclusters <- 3
clusters <- cutree(hier.clust, nclusters)

cdg <- aggregate(as.data.frame(Psi), list(clusters), mean)[,2:(ndim+1)]
k <- kmeans(as.data.frame(Psi), centers = cdg)
Bss <- sum(rowSums(k$centers^2)*k$size)
Wss <- sum(k$withinss)
100*Bss/(Bss+Wss)
par(mfrow=c(1,2))
plot(Psi[,1], Psi[,2], col = as.factor(dd.winner$winner), main ="By winner")
plot(Psi[,1], Psi[,2], col = clusters, main="By cluster")


#KNN
library(cclust)
K <- 2 # 

X <- as.matrix(dd.winner[,1:58])
## execute k-means with a maximum of 100 iterations
kmeans.2 <- cclust(X,K,iter.max=100,method="kmeans",dist="euclidean")
kmeans.2$cluster
table(dd.winner[,59],kmeans.2$cluster)
#> table(dd.winner[,59],kmeans.2$cluster)
#       1    2
#dem  655   31
#rep 2423    4

K <- 3 # 

X <- as.matrix(dd.winner[,1:58])
## execute k-means with a maximum of 100 iterations
kmeans.3 <- cclust(X,K,iter.max=100,method="kmeans",dist="euclidean")
kmeans.3$cluster
table(dd.winner[,59],kmeans.3$cluster)
#> table(dd.winner[,59],kmeans.3$cluster)
#
#       1    2    3
#dem  583    4   99
#rep 2396    2   29


#############################################################################################################
# SECTION 5: CLASSIFICATION OF democrat or republican counties based on socio-economical/public health data #
#############################################################################################################


#######################################################################################################
# SECTION 5:DEMOCRAT VS REPUBLICAN BASELINE CLASSIFICATION USING NAIVE BAYES (with all features, on unscaled data)
######################################################################################################
#naive bayes
library(e1071)
set.seed(1111)

data.demrep <- dd[,-62] #kick out blueisland from 
data.demrep <- dd.winner
#looking at our target we see its quite imbalanced 
summary(data.demrep$winner)

#We are trying to predict whether a county is democrat, 
#though democrat cases make up only 22% of our dataset
library(TunePareto) # for generateCVRuns()
do.cross.validation.naive.bayes <- function (ld,k,numtimes,options="none"){
  
  initd <- ld
  CV.folds <- generateCVRuns(initd$winner, ntimes=numtimes, nfold=k, stratified=TRUE)
  
  ## prepare the structure to store the partial results
  cv.results <- matrix (rep(0,8*k),nrow=k)
  colnames (cv.results) <- c("k","fold",'TR class dem','TR class rep',"TR error",'VA class dem','VA class rep',"VA error")
  cv.results[,"TR class dem"] <- ""
  cv.results[,"TR class rep"] <- ""
  cv.results[,"TR error"] <- 0
  cv.results[,"VA class dem"] <- ""
  cv.results[,"VA class rep"] <- ""
  cv.results[,"VA error"] <- 0
  cv.results[,"k"] <- k
  
  targetindex <- as.numeric(which(colnames(ld)=="winner"))
  
  for(j in 1:k)     #WARNING, this takes long
  {
    #get va data
    va <- unlist(CV.folds[[1]][[j]])
    
    #train on TR data
    if(options == "balancedata"){
      notvd <- ld[-va,]            
      #this makes the sets equal
      notvalidationdata<-SMOTE(winner ~ . , notvd ,k=2,perc.over = 375,perc.under=135)       
      
      #what if we make sets democratic heavy? it doesn't help
      #notvalidationdata<-SMOTE(winner ~ . , notvd ,k=10,perc.over = 475,perc.under=55)  
      
      #smaller values of k give us slightly better values
      if(j == 1){
        print("for first fold, rebalanced data target looks like: ")
        print(summary(notvalidationdata$winner))
      }
      notvalidationdata.withouttarget <- notvalidationdata[,-targetindex]
    }
    else
    {
      notvalidationdata <- ld[-va,]      
      notvalidationdata.withouttarget <- ld[-va,-targetindex]
    }
    
    
    validationdata <- ld[va,]
    classifier <- naiveBayes( notvalidationdata, notvalidationdata$winner )
    
    #predict TR data
    predict.tr <- predict(classifier,notvalidationdata.withouttarget)
    tab <- table(predict.tr,notvalidationdata$winner, dnn=list('predicted','actual'))
    
    dt <- diag(prop.table(tab, 1))
    cv.results[j,"TR class dem"] <- round(dt[1],4)
    cv.results[j,"TR class rep"] <- round(dt[2],4)
    cv.results[j,"TR error"] <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
    
    #predict on Validation data
    tab <- table(predict(classifier,ld[va,-targetindex]),ld[va,]$winner, dnn=list('predicted','actual'))
    dt <- diag(prop.table(tab, 1))
    cv.results[j,"VA class dem"] <- round(dt[1],4)
    cv.results[j,"VA class rep"] <- round(dt[2],4)
    cv.results[j,"VA error"] <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
    
    cv.results[j,"fold"] <- j   
  }
  print(cv.results)
  
  #and overall validation error, democrat specific validation 
  VA.error <- mean(as.numeric(cv.results[,"VA error"]))
  VA.accuracy.dem <- mean(as.numeric(cv.results[,"VA class dem"]))
  
  print(paste("validation error: ",VA.error))
  print(paste("validation accuracy democrat specific: ",VA.accuracy.dem))
}

N <- nrow(data.demrep)
learn <- sample(1:N, round(4*N/5))
k <- 10
numtimes <- 10

#Do naive bayes with 10x10 cross validation
cat("**** naive bayes - all features - imbalanced data - crossvalidation results ***")
cv.results <- do.cross.validation.naive.bayes(data.demrep[learn,],k,numtimes,"none")
#k    fold TR class dem TR class rep TR error            VA class dem VA class rep VA error           
#[1,] "10" "1"  "0.6375"     "0.8936"     "0.163766175814369" "0.6167"     "0.8942"     "0.172690763052209"
#[2,] "10" "2"  "0.636"      "0.896"      "0.163319946452477" "0.6957"     "0.8768"     "0.156626506024096"
#[3,] "10" "3"  "0.6406"     "0.8971"     "0.161535029004909" "0.5962"     "0.8731"     "0.184738955823293"
#[4,] "10" "4"  "0.6349"     "0.8929"     "0.165104863900045" "0.6774"     "0.9251"     "0.136546184738956"
#[5,] "10" "5"  "0.6378"     "0.895"      "0.163319946452477" "0.569"      "0.8796"     "0.192771084337349"
#[6,] "10" "6"  "0.6274"     "0.895"      "0.166889781347613" "0.6"        "0.8942"     "0.176706827309237"
#[7,] "10" "7"  "0.6335"     "0.8953"     "0.164658634538153" "0.6538"     "0.8883"     "0.160642570281124"
#[8,] "10" "8"  "0.6409"     "0.8946"     "0.162427487728693" "0.5625"     "0.8919"     "0.192771084337349"
#[9,] "10" "9"  "0.6301"     "0.8936"     "0.166443551985721" "0.6613"     "0.9198"     "0.144578313253012"
#[10,] "10" "10" "0.6342"     "0.8958"     "0.164212405176261" "0.6667"     "0.8974"     "0.152610441767068"
#[1] "validation error:  0.167068273092369"
#[1] "validation accuracy democrat specific:  0.62993"

#see results of 10x10 cross validation, The "classes" indexes shows how well the classifier
#does on the dem and rep classes, since we'll want to eventually do very well at correctly
#classifying democrat counties to better predict "blue islands".  As it stands now, we don't
#do a great job of correctly identifying democrat counties (~65%) because of imbalanced data.



# NAIVE BAYES WITH BALANCED SETS
#We now we see if we can improve our democrat prediction by oversampling our minority class (democrat) using SMOTE
library(DMwR)
#it is compulsory that the dependent variable that one needs to predict (column “target” in my case) needs to be at the end of the data frame
??SMOTE
#k is the number of nearest neighbours that are used to generate the new examples of the minority class.
#perc.over is how many extra cases from the minority class are generated (known as over-sampling).  (usually above 100)
#  for each case in the orginal data set belonging to the minority class, perc.over/100 new examples of that class will be created. If perc.over is a value below 100 than a single case will be generated for a randomly selected proportion (given by perc.over/100) of the cases belonging to the minority class on the original data set.

#perc.under is how many extra cases from the majority classes are selected for each case generated from the minority class (known as under-sampling)
#  controls the proportion of cases of the majority class that will be randomly selected for the final "balanced" data set. 
#  This proportion is calculated with respect to the number of newly generated minority class cases. 
#    For instance, if 200 new examples were generated for the minority class, 
#    a value of perc.under of 100 will randomly select exactly 200 cases belonging to the majority classes from the 
#    original data set to belong to the final data set. Values above 100 will select more examples from the majority classes.

set.seed(22222)
#looking at our original data we recall
table(data.demrep$winner)
# dem  rep 
# 686 2427 

#now we want roughtly even classes 
N <- nrow(data.demrep)
learndata <- data.demrep[learn,]
k <- 10
numtimes <- 1

#Do naive bayes with 10x10 cross validation (this takes longer than before, because of increase in rows)
cv.results <- do.cross.validation.naive.bayes(learndata,k,numtimes,"balancedata")

#Now look at cross validation results using balanced targets (with SMOTE)
cat("**** naive bayes - all features - balanced data - crossvalidation results ***")
cv.results


#Comparing it to the prior "imbalanced" results, it behaves a little worse in terms of overall validation error
#, and does slightly worse at identifying democrat counties
# [1] "for first fold, balanced data target looks like: "
#dem  rep 
#2020 2045 
#k    fold TR class dem TR class rep TR error            VA class dem VA class rep VA error           
#[1,] "10" "1"  "0.8416"     "0.7261"     "0.228536285362854" "0.6"        "0.9162"     "0.172690763052209"
#[2,] "10" "2"  "0.8442"     "0.7355"     "0.220910209102091" "0.6087"     "0.9167"     "0.168674698795181"
#[3,] "10" "3"  "0.8446"     "0.7363"     "0.220230788116867" "0.6143"     "0.9274"     "0.160642570281124"
#[4,] "10" "4"  "0.8471"     "0.7396"     "0.217039037564449" "0.5789"     "0.8802"     "0.188755020080321"
#[5,] "10" "5"  "0.8499"     "0.7425"     "0.214092806285293" "0.5818"     "0.8763"     "0.188755020080321"
#[6,] "10" "6"  "0.8326"     "0.7259"     "0.231524674686963" "0.629"      "0.9091"     "0.160642570281124"
#[7,] "10" "7"  "0.833"      "0.7355"     "0.2246501350356"   "0.614"      "0.8906"     "0.172690763052209"
#[8,] "10" "8"  "0.8378"     "0.7342"     "0.223913577215811" "0.6393"     "0.9096"     "0.156626506024096"
#[9,] "10" "9"  "0.8562"     "0.7386"     "0.214829364105082" "0.5714"     "0.8925"     "0.188755020080321"
#[10,] "10" "10" "0.8539"     "0.7362"     "0.217284556837712" "0.6557"     "0.9149"     "0.14859437751004" 
#[1] "validation error:  0.170682730923695"
#[1] "validation accuracy democrat specific:  0.60931"

#NOW WE TRY WITH SUBSET OF FEATURES WE GET FROM CFS FILTER TO SEE IF RESULTS ARE JUST AS GOOD

require(FSelector)
subset.CFS <- cfs (winner~., data.demrep)
(subset.CFS.formula <- as.simple.formula(subset.CFS,"winner"))
#winner ~ leastbach + graduate + white + afric + other + popul + preschl + belowpov + wconstruct 
# + lowbirthrate + adultobese + excsdrinking + motordeath + stdsper100 + physicianratio + 
# unemployed + noemotionalsupport + singleparent + violentcrime + female + rural + mphratio + illiteracy

subset.vars <- c("leastbach","graduate","white","afric","other","popul","preschl","belowpov","wconstruct","lowbirthrate","adultobese","excsdrinking","motordeath","stdsper100","physicianratio","unemployed","noemotionalsupport","singleparent","violentcrime","female","rural","mphratio","illiteracy","winner")
subset.vars.ids <- c()
for( i in 1:length(subset.vars)){ col <- subset.vars[i]; subset.vars.ids[i]<- which(colnames(data.demrep)==col);}

ldsubset <- data.demrep[learn,subset.vars.ids]
k <- 10
numtimes <- 10
#Do naive bayes with 10x10 cross validation
cat("**** naive bayes - subset of features - imbalanced data - crossvalidation results ***")
cv.results <- do.cross.validation.naive.bayes(ldsubset,k,numtimes)
cv.results
#k    fold TR class dem TR class rep TR error            VA class dem VA class rep VA error           
#[1,] "10" "1"  "0.6265"     "0.8869"     "0.170013386880857" "0.6212"     "0.9126"     "0.164658634538153"
#[2,] "10" "2"  "0.6366"     "0.8888"     "0.165997322623829" "0.6327"     "0.87"       "0.176706827309237"
#[3,] "10" "3"  "0.63"       "0.8903"     "0.167782240071397" "0.5763"     "0.8842"     "0.188755020080321"
#[4,] "10" "4"  "0.6255"     "0.8872"     "0.170459616242749" "0.66"       "0.8844"     "0.160642570281124"
#[5,] "10" "5"  "0.6429"     "0.8909"     "0.163319946452477" "0.6078"     "0.8737"     "0.180722891566265"
#[6,] "10" "6"  "0.6337"     "0.8872"     "0.167782240071397" "0.625"      "0.9135"     "0.160642570281124"
#[7,] "10" "7"  "0.6198"     "0.8828"     "0.174029451137885" "0.7636"     "0.9278"     "0.108433734939759"
#[8,] "10" "8"  "0.6369"     "0.8902"     "0.165551093261937" "0.625"      "0.8706"     "0.176706827309237"
#[9,] "10" "9"  "0.645"      "0.8924"     "0.161981258366801" "0.5741"     "0.8718"     "0.192771084337349"
#[10,] "10" "10" "0.6303"     "0.8889"     "0.168228469433289" "0.64"       "0.8794"     "0.168674698795181"
#[1] "validation error:  0.167871485943775"
#[1] "validation accuracy democrat specific:  0.63257"


#Using a subset of features (23) we get comparable results to when sending all variables with imbalanced data to Naive Bayes.
#This gives us a very slightly worse overall valdiation error 1% and similar job of predicting democrats.

#NOW we finish by trying using our subset of features on balanced data.

#now we have basically even classes (though much much more classes) and attempt Naive Bayes on it

ldsubsetsmote <- data.demrep[learn,subset.vars.ids]
k <- 10
numtimes <- 1
cat("**** naive bayes - subset of features - balanced data - crossvalidation results ***")
cv.results <- do.cross.validation.naive.bayes(ldsubsetsmote,k,numtimes,"balancedata")
#dem  rep 
#2020 2045 
#k    fold TR class dem TR class rep TR error            VA class dem VA class rep VA error           
#[1,] "10" "1"  "0.84"       "0.7258"     "0.229274292742927" "0.541"      "0.8723"     "0.208835341365462"
#[2,] "10" "2"  "0.8572"     "0.7296"     "0.221156211562116" "0.5072"     "0.8778"     "0.224899598393574"
#[3,] "10" "3"  "0.833"      "0.7163"     "0.238399214338326" "0.6724"     "0.911"      "0.144578313253012"
#[4,] "10" "4"  "0.8375"     "0.7194"     "0.234716425239381" "0.6212"     "0.918"      "0.160642570281124"
#[5,] "10" "5"  "0.8419"     "0.7235"     "0.230297078320648" "0.6604"     "0.8929"     "0.156626506024096"
#[6,] "10" "6"  "0.8404"     "0.7205"     "0.232997790326541" "0.625"      "0.9135"     "0.160642570281124"
#[7,] "10" "7"  "0.8384"     "0.7217"     "0.232752271053278" "0.6852"     "0.9026"     "0.144578313253012"
#[8,] "10" "8"  "0.825"      "0.7256"     "0.234225386692855" "0.5333"     "0.873"      "0.208835341365462"
#[9,] "10" "9"  "0.8513"     "0.7214"     "0.229069481954333" "0.5909"     "0.9071"     "0.176706827309237"
#[10,] "10" "10" "0.8316"     "0.7202"     "0.235944021605696" "0.6557"     "0.9149"     "0.14859437751004" 
#[1] "validation error:  0.173493975903614"
#[1] "validation accuracy democrat specific:  0.60923"

#Similarly to before, the reduced set of features performs only slightly worse than its equivalent full featured
#naive bayes classifier. 


##############################################################################################
#Logistic regression 
#############################################################################################

do.cross.validation.logistic.regression <- function (learndata,k,numtimes,options="none"){  
  initd <- learndata
  CV.folds <- generateCVRuns(initd$winner, ntimes=numtimes, nfold=k, stratified=TRUE)
  
  ## prepare the structure to store the partial results
  cv.results <- matrix (rep(0,8*k),nrow=k)
  colnames (cv.results) <- c("k","fold",'TR class dem','TR class rep',"TR error",'VA class dem','VA class rep',"VA error")
  cv.results[,"TR class dem"] <- ""
  cv.results[,"TR class rep"] <- ""
  cv.results[,"TR error"] <- 0
  cv.results[,"VA class dem"] <- ""
  cv.results[,"VA class rep"] <- ""
  cv.results[,"VA error"] <- 0
  cv.results[,"k"] <- k
  
  for(j in 1:k)    
  {
    #get va data
    va <- unlist(CV.folds[[1]][[j]])
    
    #train on TR data
    if(options == "balancedata"){
      notvd <- learndata[-va,]   
      #print(colnames(notvd))
      #this adds a lot of data but gives better results than our prior one , defaults to k=5 and perc.under = 200
      notvalidationdata<-SMOTE(winner ~ . , notvd, perc.over = 375,perc.under=135)
      
      #what if we make sets democratic heavy? it doesn't help
      #notvalidationdata<-SMOTE(winner ~ . , notvd ,k=10,perc.over = 475,perc.under=55)  
      
      #smaller values of k give us slightly better values
      if(j == 1){
        print("for first fold, rebalanced data target looks like: ")
        print(summary(notvalidationdata$winner))
      }      
    }
    else
    {
      notvalidationdata <- learndata[-va,]             
    }
    
    validationdata <- learndata[va,]
    
    demrep.logreg <- glm(winner ~ ., data = notvalidationdata, family = "binomial")
    
    glfpred<-NULL
    glfpred[demrep.logreg$fitted.values<0.5]<-"dem"
    glfpred[demrep.logreg$fitted.values>=0.5]<-"rep"
    
    #predict TR data
    tab <- table(glfpred,notvalidationdata$winner)
    dt <- diag(prop.table(tab, 1))
    cv.results[j,"TR class dem"] <- round(dt[1],4)
    cv.results[j,"TR class rep"] <- round(dt[2],4)    
    cv.results[j,"TR error"] <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
    
    #predict on Validation data       
    new.pred <- predict(demrep.logreg , newdata=validationdata, type="response",se.fit=T)
    #print(new.pred)
    glfpred<-NULL
    glfpred[new.pred$fit<0.5]<-"dem"
    glfpred[new.pred$fit>=0.5]<-"rep"
    tab <- table(glfpred,validationdata$winner)
    
    dt <- diag(prop.table(tab, 1))
    cv.results[j,"VA class dem"] <- round(dt[1],4)
    cv.results[j,"VA class rep"] <- round(dt[2],4)    
    cv.results[j,"VA error"] <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
    
    cv.results[j,"fold"] <- j        
    
  }
  print(cv.results)
  #and overall validation error
  VA.error <- mean(as.numeric(cv.results[,"VA error"]))
  VA.accuracy.dem <- mean(as.numeric(cv.results[,"VA class dem"]))
  
  print(paste("validation error: ",VA.error))
  print(paste("validation accuracy democrat specific: ",VA.accuracy.dem))
}

N <- nrow(data.demrep)
nlearn <- length(learn)

ld <- data.demrep[learn,]
k <- 10
numtimes <- 10
cat("**** logistic regression - all features - imbalanced data - crossvalidation results ***")
cv.results <- do.cross.validation.logistic.regression(ld,k,numtimes)
#k    fold TR class dem TR class rep TR error            VA class dem VA class rep VA error           
#[1,] "10" "1"  "0.7748"     "0.9104"     "0.116465863453815" "0.75"       "0.9365"     "0.108433734939759"
#[2,] "10" "2"  "0.7896"     "0.9133"     "0.111111111111111" "0.7234"     "0.8861"     "0.144578313253012"
#[3,] "10" "3"  "0.7864"     "0.9112"     "0.113342257920571" "0.7755"     "0.91"       "0.116465863453815"
#[4,] "10" "4"  "0.7891"     "0.9122"     "0.112003569834895" "0.7222"     "0.9128"     "0.1285140562249"  
#[5,] "10" "5"  "0.7854"     "0.9101"     "0.114234716644355" "0.8298"     "0.9158"     "0.100401606425703"
#[6,] "10" "6"  "0.7793"     "0.9075"     "0.117358322177599" "0.78"       "0.9146"     "0.112449799196787"
#[7,] "10" "7"  "0.7889"     "0.9157"     "0.109772423025435" "0.717"      "0.9082"     "0.132530120481928"
#[8,] "10" "8"  "0.7878"     "0.9127"     "0.112003569834895" "0.7708"     "0.9055"     "0.120481927710843"
#[9,] "10" "9"  "0.7815"     "0.9115"     "0.114234716644355" "0.7955"     "0.8976"     "0.120481927710843"
#[10,] "10" "10" "0.7902"     "0.9152"     "0.109772423025435" "0.7209"     "0.8786"     "0.14859437751004" 
#[1] "validation error:  0.123293172690763"
#[1] "validation accuracy democrat specific:  0.75851"


#This validation error is better than what we obtained with Naive Bayes, but the imbalanced 
#nature of our target still makes it so that we aren't great at classifying democrat counties 
#though this way is better than naive bayes

#TRYING WITH BALANCED SET NOW
N <- nrow(data.demrep)
k <- 10
numtimes <- 10
cat("**** logistic regression - all features - balanced data - crossvalidation results ***")
cv.results <- do.cross.validation.logistic.regression(learndata,k,numtimes,"balancedata")
#k    fold TR class dem TR class rep TR error            VA class dem VA class rep VA error           
#[1,] "10" "1"  "0.8694"     "0.8884"     "0.121279212792128" "0.6125"     "0.9527"     "0.156626506024096"
#[2,] "10" "2"  "0.8631"     "0.8933"     "0.122263222632226" "0.6447"     "0.9538"     "0.140562248995984"
#[3,] "10" "3"  "0.8645"     "0.8959"     "0.120304443898846" "0.6076"     "0.9529"     "0.156626506024096"
#[4,] "10" "4"  "0.8636"     "0.898"      "0.11981340535232"  "0.5667"     "0.9686"     "0.176706827309237"
#[5,] "10" "5"  "0.8576"     "0.8896"     "0.126933464276946" "0.6265"     "0.9759"     "0.140562248995984"
#[6,] "10" "6"  "0.8584"     "0.8924"     "0.125214829364105" "0.5694"     "0.9153"     "0.184738955823293"
#[7,] "10" "7"  "0.8814"     "0.8989"     "0.109992634421802" "0.6026"     "0.9474"     "0.160642570281124"
#[8,] "10" "8"  "0.8695"     "0.9023"     "0.114657500613798" "0.6479"     "0.9438"     "0.140562248995984"
#[9,] "10" "9"  "0.8657"     "0.8925"     "0.121286520991898" "0.6719"     "0.9297"     "0.136546184738956"
#[10,] "10" "10" "0.8681"     "0.8836"     "0.124232752271053" "0.5625"     "0.9349"     "0.184738955823293"
#[1] "validation error:  0.157831325301205"
#[1] "validation accuracy democrat specific:  0.61123"


#These results on the balanced set are not great because they add a lot of time for computation (because of synthetic data added)
#and then don't do better on the accuracy of democrat counties on the validation data compared with imbalanced logistic regression.


#NOW WE TRY WITH SUBSET OF FEATURES WE GET FROM CFS FILTER TO SEE IF RESULTS ARE JUST AS GOOD, for logistic regression

subset.vars <- c("leastbach","graduate","white","afric","other","popul","preschl","belowpov","wconstruct","lowbirthrate","adultobese","excsdrinking","motordeath","stdsper100","physicianratio","unemployed","noemotionalsupport","singleparent","violentcrime","female","rural","mphratio","illiteracy","winner")
subset.vars.ids <- c()
for( i in 1:length(subset.vars)){ col <- subset.vars[i]; subset.vars.ids[i]<- which(colnames(data.demrep)==col);}

N <- nrow(data.demrep)
ldsubset <- data.demrep[learn,subset.vars.ids]
k <- 10
numtimes <- 10
#Do naive bayes with 10x10 cross validation
cat("**** logistic regression - subset of features - imbalanced data - crossvalidation results ***")
cv.results <- do.cross.validation.logistic.regression(ldsubset,k,numtimes)

# cv.results <- do.cross.validation.logistic.regression(ldsubset,k,numtimes)
#k    fold TR class dem TR class rep TR error            VA class dem VA class rep VA error            
#[1,] "10" "1"  "0.7633"     "0.8831"     "0.136992414100848" "0.7436"     "0.8667"     "0.152610441767068" 
#[2,] "10" "2"  "0.7657"     "0.8805"     "0.138331102186524" "0.7111"     "0.8775"     "0.152610441767068" 
#[3,] "10" "3"  "0.7546"     "0.8832"     "0.138777331548416" "0.6829"     "0.8654"     "0.164658634538153" 
#[4,] "10" "4"  "0.7593"     "0.8824"     "0.138331102186524" "0.7436"     "0.8714"     "0.14859437751004"  
#[5,] "10" "5"  "0.76"       "0.8816"     "0.138777331548416" "0.7561"     "0.8798"     "0.140562248995984" 
#[6,] "10" "6"  "0.7527"     "0.8804"     "0.141008478357876" "0.7949"     "0.881"      "0.132530120481928" 
#[7,] "10" "7"  "0.7568"     "0.8779"     "0.14190093708166"  "0.814"      "0.8981"     "0.116465863453815" 
#[8,] "10" "8"  "0.7583"     "0.8761"     "0.142793395805444" "0.8478"     "0.9163"     "0.0963855421686747"
#[9,] "10" "9"  "0.766"      "0.8831"     "0.136546184738956" "0.7381"     "0.8792"     "0.144578313253012" 
#[10,] "10" "10" "0.7676"     "0.8859"     "0.134315037929496" "0.7059"     "0.8512"     "0.168674698795181" 
#[1] "validation error:  0.141767068273092"
#[1] "validation accuracy democrat specific:  0.7538"


#NOW we finish by trying using our subset of features on balanced data.
N <- nrow(data.demrep)
ldsubset <- data.demrep[learn,subset.vars.ids]
k <- 10
numtimes <- 10
cat("**** logistic regression - subset of features - balanced data - crossvalidation results ***")
cv.results <- do.cross.validation.logistic.regression(ldsubset,k,numtimes,"balancedata")
# [1] "for first fold, rebalanced data target looks like: "
#dem  rep 
#2020 2045
#k    fold TR class dem TR class rep TR error            VA class dem VA class rep VA error           
#[1,] "10" "1"  "0.8169"     "0.828"      "0.177613776137761" "0.5233"     "0.9264"     "0.21285140562249" 
#[2,] "10" "2"  "0.8162"     "0.8282"     "0.177859778597786" "0.5484"     "0.9615"     "0.192771084337349"
#[3,] "10" "3"  "0.8172"     "0.8263"     "0.178246992388903" "0.5119"     "0.9212"     "0.216867469879518"
#[4,] "10" "4"  "0.8167"     "0.8274"     "0.17800147311564"  "0.5522"     "0.8956"     "0.196787148594378"
#[5,] "10" "5"  "0.804"      "0.8226"     "0.186840166953106" "0.519"      "0.9118"     "0.21285140562249" 
#[6,] "10" "6"  "0.8117"     "0.8205"     "0.18389393567395"  "0.557"      "0.9294"     "0.188755020080321"
#[7,] "10" "7"  "0.81"       "0.8295"     "0.180456665848269" "0.557"      "0.9294"     "0.188755020080321"
#[8,] "10" "8"  "0.8061"     "0.8286"     "0.182911858580899" "0.6026"     "0.9474"     "0.160642570281124"
#[9,] "10" "9"  "0.8128"     "0.8204"     "0.183402897127425" "0.4819"     "0.9036"     "0.236947791164659"
#[10,] "10" "10" "0.8"        "0.8209"     "0.189786398232261" "0.5517"     "0.9506"     "0.188755020080321"
#[1] "validation error:  0.199598393574297"
#[1] "validation accuracy democrat specific:  0.5405"


#As expected this works worse overall validation wise and democrat prediction wise than the logistic regression
#using all features and balanced data



########################################################################################################
#Ensemble learner ( random forest)
########################################################################################################

do.cross.validation.random.forest <- function (learndata,k,numtimes,options="none"){
  initd <- learndata
  CV.folds <- generateCVRuns(initd$winner, ntimes=numtimes, nfold=k, stratified=TRUE)
  
  ## prepare the structure to store the partial results
  cv.results <- matrix (rep(0,8*k),nrow=k)
  colnames (cv.results) <- c("k","fold",'TR class dem','TR class rep',"TR error",'VA class dem','VA class rep',"VA error")
  cv.results[,"TR class dem"] <- ""
  cv.results[,"TR class rep"] <- ""
  cv.results[,"TR error"] <- 0
  cv.results[,"VA class dem"] <- ""
  cv.results[,"VA class rep"] <- ""
  cv.results[,"VA error"] <- 0
  cv.results[,"k"] <- k
  
  targetindex <- as.numeric(which(colnames(ld)=="winner"))
  
  for(j in 1:k)    
  {
    #get va data
    va <- unlist(CV.folds[[1]][[j]])
    
    #train on TR data
    if(options == "balancedata"){
      notvd <- learndata[-va,]            
      #this makes the sets equal
      notvalidationdata<-SMOTE(winner ~ . , notvd ,k=2,perc.over = 375,perc.under=135)       
      
      #what if we make sets democratic heavy? it doesn't help
      #notvalidationdata<-SMOTE(winner ~ . , notvd ,k=10,perc.over = 475,perc.under=55)  
      
      #smaller values of k give us slightly better values
      if(j == 1){
        print("for first fold, rebalanced data target looks like: ")
        print(summary(notvalidationdata$winner))
      }
      notvalidationdata.without.target <- notvalidationdata[,-targetindex]
    }
    else
    {
      notvalidationdata <- learndata[-va,]
      notvalidationdata.without.target <- learndata[-va,-targetindex]
    }
    validationdata <- learndata[va,]  
    validationdata.without.target <- learndata[va,-targetindex]  
    clf <- randomForest(notvalidationdata$winner ~ ., data=notvalidationdata, ntree=20, nodesize=5, mtry=9)
    
    #predict TR data
    results.test <- predict(clf, notvalidationdata.without.target)
    tab <- table( notvalidationdata$winner, results.test )
    dt <- diag(prop.table(tab, 1))
    cv.results[j,"TR class dem"] <- round(dt[1],4)
    cv.results[j,"TR class rep"] <- round(dt[2],4)    
    cv.results[j,"TR error"] <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
    
    
    #predict on Validation data       
    tab <- table(predict(clf,validationdata.without.target),validationdata$winner, dnn=list('predicted','actual'))
    dt <- diag(prop.table(tab, 1))
    cv.results[j,"VA class dem"] <- round(dt[1],4)
    cv.results[j,"VA class rep"] <- round(dt[2],4)
    cv.results[j,"VA error"] <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
    
    cv.results[j,"fold"] <- j        
    
  }
  print(cv.results)
  
  VA.error <- mean(as.numeric(cv.results[,"VA error"]))
  VA.accuracy.dem <- mean(as.numeric(cv.results[,"VA class dem"]))
  
  print(paste("- validation error: ",VA.error))
  print(paste("- validation accuracy democrat specific: ",VA.accuracy.dem))
  
}

library(randomForest)
N <- nrow(data.demrep)
lds <- data.demrep[learn,]
k <- 10
numtimes <- 10
cat("**** random forest - all features - imbalanced data - crossvalidation results ***")
cv.results <- do.cross.validation.random.forest(lds,k,numtimes)
#       k    fold TR class dem TR class rep TR error              VA class dem VA class rep VA error            
# [1,] "10" "1"  "0.9541"     "0.9989"     "0.0111557340473003"  "0.814"      "0.9029"     "0.112449799196787" 
# [2,] "10" "2"  "0.9541"     "0.9989"     "0.0111557340473003"  "0.8511"     "0.9257"     "0.0883534136546185"
# [3,] "10" "3"  "0.9641"     "0.9994"     "0.00847835787594819" "0.814"      "0.9029"     "0.112449799196787" 
# [4,] "10" "4"  "0.9721"     "0.9977"     "0.00803212851405621" "0.8462"     "0.852"      "0.14859437751004"  
# [5,] "10" "5"  "0.954"      "0.9983"     "0.0116019634091923"  "0.878"      "0.9038"     "0.100401606425703" 
# [6,] "10" "6"  "0.952"      "0.9989"     "0.0116019634091923"  "0.8333"     "0.9204"     "0.0963855421686747"
# [7,] "10" "7"  "0.958"      "0.9971"     "0.0116019634091923"  "0.7907"     "0.8932"     "0.124497991967871" 
# [8,] "10" "8"  "0.96"       "0.9971"     "0.0111557340473003"  "0.8649"     "0.8868"     "0.116465863453815" 
# [9,] "10" "9"  "0.954"      "0.9983"     "0.0116019634091923"  "0.7895"     "0.8768"     "0.136546184738956" 
# [10,] "10" "10" "0.964"      "0.9983"     "0.00937081659973227" "0.8571"     "0.9034"     "0.104417670682731" 
# - validation error:  0.114056224899598
# - validation accuracy democrat specific:  0.83388
#Random forest gives very good results immediately with a low overall validation error and democrat specific one

#Now try with balanced data
#newdataSmote<-SMOTE(winner ~ . , data.demrep ,k=10,perc.over = 375,perc.under=135)
N <- nrow(data.demrep)
rfsmote <- data.demrep[learn,]
k <- 10
numtimes <- 10
cat("**** random forest - all features - balanced data - crossvalidation results ***")
cv.results <- do.cross.validation.random.forest(rfsmote,k,numtimes)
# k    fold TR class dem TR class rep TR error              VA class dem VA class rep VA error            
# [1,] "10" "1"  "0.9695"     "0.9971"     "0.00892458723784029" "0.8649"     "0.8962"     "0.108433734939759" 
# [2,] "10" "2"  "0.9492"     "0.9989"     "0.0120481927710844"  "0.8889"     "0.8967"     "0.104417670682731" 
# [3,] "10" "3"  "0.9593"     "0.9983"     "0.0102632753235163"  "0.8043"     "0.9163"     "0.104417670682731" 
# [4,] "10" "4"  "0.9512"     "0.9989"     "0.0116019634091923"  "0.7805"     "0.8942"     "0.124497991967871" 
# [5,] "10" "5"  "0.9654"     "0.9983"     "0.00892458723784029" "0.7179"     "0.8714"     "0.152610441767068" 
# [6,] "10" "6"  "0.9715"     "0.9989"     "0.00713966979027225" "0.7805"     "0.8894"     "0.1285140562249"   
# [7,] "10" "7"  "0.9674"     "0.9983"     "0.00847835787594819" "0.878"      "0.9087"     "0.0963855421686747"
# [8,] "10" "8"  "0.9715"     "0.9977"     "0.00803212851405621" "0.9189"     "0.9009"     "0.0963855421686747"
# [9,] "10" "9"  "0.9674"     "0.9971"     "0.00937081659973227" "0.8947"     "0.9005"     "0.100401606425703" 
# [10,] "10" "10" "0.9776"     "0.9983"     "0.00624721106648818" "0.7576"     "0.8611"     "0.152610441767068" 
# - validation error:  0.116867469879518
# - validation accuracy democrat specific:  0.82862

#Balancing the data gives us basically identical results unfortunately (no gain)

#Now try with subset of features and imbalanced
N <- nrow(data.demrep)
rfsubset <- data.demrep[learn,subset.vars.ids]
k <- 10
numtimes <- 10
cat("**** random forest - subset of features - imbalanced data - crossvalidation results ***")
cv.results <- do.cross.validation.random.forest(rfsubset,k,numtimes)
# k    fold TR class dem TR class rep TR error             VA class dem VA class rep VA error           
# [1,] "10" "1"  "0.9286"     "0.9994"     "0.0160642570281124" "0.8919"     "0.9009"     "0.100401606425703"
# [2,] "10" "2"  "0.9531"     "0.9994"     "0.0107095046854083" "0.8049"     "0.899"      "0.116465863453815"
# [3,] "10" "3"  "0.9531"     "0.9983"     "0.0116019634091923" "0.7609"     "0.9064"     "0.120481927710843"
# [4,] "10" "4"  "0.9469"     "0.9977"     "0.0133868808567604" "0.75"       "0.9104"     "0.120481927710843"
# [5,] "10" "5"  "0.9449"     "0.9983"     "0.0133868808567604" "0.575"      "0.8517"     "0.192771084337349"
# [6,] "10" "6"  "0.9469"     "0.9989"     "0.0124944221329764" "0.8571"     "0.8879"     "0.116465863453815"
# [7,] "10" "7"  "0.953"      "0.9983"     "0.0116019634091923" "0.8378"     "0.8868"     "0.120481927710843"
# [8,] "10" "8"  "0.955"      "0.9983"     "0.0111557340473003" "0.9"        "0.8721"     "0.124497991967871"
# [9,] "10" "9"  "0.9489"     "0.9977"     "0.0129406514948683" "0.8108"     "0.8821"     "0.1285140562249"  
# [10,] "10" "10" "0.9468"     "0.9971"     "0.0138331102186524" "0.8043"     "0.9113"     "0.108433734939759"
# - validation error:  0.124899598393574
# - validation accuracy democrat specific:  0.79927

#This gets us similar results to random forest on all features with imbalanced data though with slightly worse overall
#accuracy and democrat accuracy however.

#Now try with subset of features and balanced data
N <- nrow(data.demrep)
rfsubsetb <- data.demrep[learn,subset.vars.ids]
k <- 10
numtimes <- 10
cat("**** random forest - subset of features - balanced data - crossvalidation results ***")
cv.results <- do.cross.validation.random.forest(rfsubsetb,k,numtimes,"balancedata")
# [1] "for first fold, rebalanced data target looks like: "
# dem  rep 
# 2004 2029 
# k    fold TR class dem TR class rep TR error               VA class dem VA class rep VA error           
# [1,] "10" "1"  "0.997"      "0.9975"     "0.00272749814034212"  "0.6324"     "0.9337"     "0.14859437751004" 
# [2,] "10" "2"  "0.998"      "0.998"      "0.00198363501115795"  "0.6462"     "0.9293"     "0.144578313253012"
# [3,] "10" "3"  "0.9975"     "0.9966"     "0.00297545251673692"  "0.6552"     "0.911"      "0.14859437751004" 
# [4,] "10" "4"  "0.998"      "0.9995"     "0.00123977188197366"  "0.6182"     "0.8918"     "0.168674698795181"
# [5,] "10" "5"  "1"          "0.9965"     "0.00173913043478258"  "0.7193"     "0.9219"     "0.124497991967871"
# [6,] "10" "6"  "0.999"      "0.9995"     "0.000745341614906803" "0.6286"     "0.933"      "0.152610441767068"
# [7,] "10" "7"  "0.9995"     "0.999"      "0.000745341614906803" "0.6984"     "0.9355"     "0.124497991967871"
# [8,] "10" "8"  "0.999"      "0.9975"     "0.00173913043478258"  "0.6984"     "0.9355"     "0.124497991967871"
# [9,] "10" "9"  "0.999"      "0.9965"     "0.00223602484472052"  "0.7121"     "0.9508"     "0.112449799196787"
# [10,]"10" "10" "0.9995"     "0.999"      "0.000745341614906803" "0.6522"     "0.9389"     "0.140562248995984"
# [1] "- validation error:  0.138955823293172"
# [1] "- validation accuracy democrat specific:  0.6661"

#This model gives us slightly less overall validation error than the random forest on balanced data
#with all features (1% lower) but it does much worst in the democrat validation accuracy, while
#using 23 as opposed to 61 variables. The imbalanced version is however still better.

##############################################
#####SUPPORT VECTOR MACHINES
## we have the 'cross' parameter, for cross-validation (default is 0)
library(kernlab)

do.cross.validation.svm <- function(data.demrep,kerneltype,options="none"){
  N <- nrow(data.demrep)
  learn <- sample(1:N, round(4*N/5))
  nlearn <- length(learn)
  svmlearn <- data.demrep[learn,]
  validation <- sample(1:nlearn, round(nlearn/4))
  targetindex <- as.numeric(which(colnames(svmlearn)=="winner"))
  
  if(options == "balancedata"){
    notvd <- svmlearn[-validation,]            
    #this makes the sets equal
    svmlearn.train <-SMOTE(winner ~ . , notvd ,k=2,perc.over = 375,perc.under=135)         
    
    #smaller values of k give us slightly better values
    print("rebalanced data target looks like: ")
    print(summary(svmlearn.train$winner))
    
    svmlearn.train.without.target <- svmlearn.train[,-targetindex]
  }
  else
  {
    svmlearn.train <- svmlearn[-validation,]
    svmlearn.train.without.target <- svmlearn.train[,-targetindex]
  }
  
  #train on training data
  if(kerneltype == "linear"){
    mi.svm <- ksvm (winner~.,data=svmlearn.train,kernel='polydot',C=1,cross=10)
  }
  if(kerneltype == "linearc5"){
    mi.svm <- ksvm (winner~.,data=svmlearn.train,kernel='polydot',C=5,cross=10)
  }
  if(kerneltype == "quadratic"){
    cuad <- polydot(degree = 2, scale = 1, offset = 1)
    mi.svm <- ksvm (winner~.,data=svmlearn.train,kernel=cuad,C=1,cross=10)
  }
  if(kerneltype == "quadratic5")
  {
    cuad <- polydot(degree = 2, scale = 1, offset = 1)
    mi.svm <- ksvm (winner~.,data=svmlearn.train,kernel=cuad,C=5,cross=10)
  }
  if(kerneltype == "rbf"){
    mi.svm <- ksvm (winner~.,data=svmlearn.train,C=1,cross=10)
  }
  if(kerneltype == "rbfc5")
  {
    mi.svm <- ksvm (winner~.,data=svmlearn.train,C=5,cross=10)
  }
  
  
  
  svmlearn.validation <- svmlearn[validation,]  
  svmlearn.validation.without.target <- svmlearn[validation,-targetindex]
  
  #test svm on training data
  svmpred <- predict (mi.svm, svmlearn.train.without.target)
  tab <- table(svmlearn.train$winner,svmpred)
  dt <- diag(prop.table(tab, 1))
  error_rate.train <- 100*(1-sum(diag(tab))/sum(tab))
  print(paste("- - train error: ",error_rate.train))
  print(paste("- - train accuracy democrat specific: ",dt[1]))
  
  #test on validation
  svmpred <- predict (mi.svm, svmlearn.validation.without.target)
  tab <- table(svmlearn.validation$winner,svmpred)
  dt <- diag(prop.table(tab, 1))
  error_rate.validation <- 100*(1-sum(diag(tab))/sum(tab))
  
  print(paste("- - validation error: ",error_rate.validation))
  print(paste("- - validation accuracy democrat specific: ",dt[1]))
}

# we start with a linear kernel and 10 fold cross validation on the imbalanced data
cat(paste("*****svm - linear kernel - all features - imbalanced data: "))
do.cross.validation.svm(data.demrep,"linear") 
#Setting default kernel parameters  
#[1] "- - train error:  10.0107066381156"
#[1] "- - train accuracy democrat specific:  0.689903846153846"
#[1] "- - validation error:  11.8971061093248"
#[1] "- - validation accuracy democrat specific:  0.666666666666667"
#SVM with linear kernel on all features with imbalanced data does a good job overall.
# but worse democrat accuracy than randomforest.

# we then try a linear kernel and 10 fold cross validation on the balanced data
cat(paste("*****svm - linear kernel - all features - balanced data: "))
do.cross.validation.svm(data.demrep,"linear","balancedata")
#[1] "rebalanced data target looks like: "
# dem  rep 
# 1632 1652 
#Setting default kernel parameters  
#[1] "- - train error:  11.1282367447596"
#[1] "- - train accuracy democrat specific:  0.919975186104218"
#[1] "- - validation error:  14.6302250803859"
#[1] "- - validation accuracy democrat specific:  0.846153846153846"

#SVM with linear kernel on all features with balanced data performs much worse in regards to validation error
#compared with svm linear on imbalanced data, but does much much better at detecting democrat counties correctly.

#Now try svm (both on imbalanced and balanced) using the subset of features
cat(paste("*****svm - linear kernel - subset features - imbalanced data: "))
data.demrep.subset <- data.demrep[,subset.vars.ids]
do.cross.validation.svm(data.demrep.subset,"linear")
# Setting default kernel parameters  
# [1] "- - train error:  13.0085653104925"
# [1] "- - train accuracy democrat specific:  0.559907834101382"
# [1] "- - validation error:  14.4694533762058"
# [1] "- - validation accuracy democrat specific:  0.487603305785124"

#This performs way worse than with all features

cat(paste("*****svm - linear kernel - subset features - balanced data: "))

do.cross.validation.svm(data.demrep.subset,"linear","balancedata")
#[1] "rebalanced data target looks like: "
#dem  rep 
#1672 1692 
#Setting default kernel parameters  
#[1] "- - train error:  17.2711058263971"
#[1] "- - train accuracy democrat specific:  0.837918660287081"
#[1] "- - validation error:  19.7749196141479"
#[1] "- - validation accuracy democrat specific:  0.808333333333333"

#This performs better than it does on the imbalanced data (in terms of democrat accuracy), but the overall validation error 
#is quite poor


#Lets see if we can do better with a quadratic kernel on imbalanced date
cat(paste("*****svm - quadratic kernel - all features - imbalanced data: "))
do.cross.validation.svm(data.demrep,"quadratic")
# [1] "- - train error:  0"
# [1] "- - train accuracy democrat specific:  1"
# [1] "- - validation error:  13.9871382636656"
# [1] "- - validation accuracy democrat specific:  0.671532846715328"
#The quadratic svm on imbalanced overfits the training data and performs worse on the validation data than its linear equivalent

#Lets see how the quadratic kernel will do on balanced data
cat(paste("*****svm - quadratic kernel - all features - balanced data: "))
do.cross.validation.svm(data.demrep,"quadratic","balancedata")
# 1] "rebalanced data target looks like: "
# dem  rep 
# 1596 1615 
# [1] "- - train error:  0"
# [1] "- - train accuracy democrat specific:  1"
# [1] "- - validation error:  18.3279742765273"
# [1] "- - validation accuracy democrat specific:  0.62962962962963"
#The quadratic svm on balanced data performs worse than on imbalanced data!

#Lets see how the subset feature versions perform 
#imbalanced, subset
cat(paste("*****svm - quadratic kernel - subset of features - imbalanced data: "))
do.cross.validation.svm(data.demrep.subset,"quadratic")
# [1] "- - train error:  4.49678800856531"
# [1] "- - train accuracy democrat specific:  0.856481481481482"
# [1] "- - validation error:  15.112540192926"
# [1] "- - validation accuracy democrat specific:  0.610169491525424"
#This svm's performance is comparable to the full features version, though with slightly worse validation error
#and worse democrat specifc accuracy

#now, on balanced data
cat(paste("*****svm - quadratic kernel - subset of features - balanced data: "))
do.cross.validation.svm(data.demrep.subset,"quadratic","balancedata")
# [1] "rebalanced data target looks like: "
# dem  rep 
# 1760 1782 
# [1] "- - train error:  3.16205533596838"
# [1] "- - train accuracy democrat specific:  0.977272727272727"
# [1] "- - validation error:  18.1672025723473"
# [1] "- - validation accuracy democrat specific:  0.678260869565217"

#This actually does better than the full variable version to svm, though still bad.



#Finally try with RBF kernel
cat(paste("*****svm - rbf kernel - all features - imbalanced data: "))
do.cross.validation.svm(data.demrep,"rbf")
# Using automatic sigma estimation (sigest) for RBF or laplace kernel 
# [1] "- - train error:  7.76231263383298"
# [1] "- - train accuracy democrat specific:  0.705882352941177"
# [1] "- - validation error:  10.6109324758842"
# [1] "- - validation accuracy democrat specific:  0.588709677419355"
#this performs quite well outside of the box, though with low democrat specific accuracy.


#Lets see how the rbf kernel will do on balanced data
cat(paste("*****svm - rbf kernel - all features - balanced data: "))
do.cross.validation.svm(data.demrep,"rbf","balancedata")
# dem  rep 
# 1696 1717 
# Using automatic sigma estimation (sigest) for RBF or laplace kernel 
# [1] "- - train error:  4.6000585994726"
# [1] "- - train accuracy democrat specific:  0.975825471698113"
# [1] "- - validation error:  12.379421221865"
# [1] "- - validation accuracy democrat specific:  0.843283582089552"
# This performs very well, slightly higher validation error than linear on imbalnced, but with much higher
# democrat specific accuracy.


#Lets see how the subset feature versions perform 
#imbalanced, subset
cat(paste("*****svm - rbf kernel - subset of features - imbalanced data: "))
do.cross.validation.svm(data.demrep.subset,"rbf")
# Using automatic sigma estimation (sigest) for RBF or laplace kernel 
# [1] "- - train error:  8.19057815845824"
# [1] "- - train accuracy democrat specific:  0.685851318944844"
# [1] "- - validation error:  11.4147909967846"
# [1] "- - validation accuracy democrat specific:  0.618705035971223"
#Decent validation error, terrible dem accuracy

#now, on balanced data
cat(paste("*****svm - rbf kernel - subset of features - balanced data: "))
do.cross.validation.svm(data.demrep.subset,"rbf","balancedata")
# [1] "rebalanced data target looks like: "
# dem  rep 
# 1620 1640 
# Using automatic sigma estimation (sigest) for RBF or laplace kernel 
# [1] "- - train error:  6.80981595092025"
# [1] "- - train accuracy democrat specific:  0.945061728395062"
# [1] "- - validation error:  15.5948553054662"
# [1] "- - validation accuracy democrat specific:  0.797297297297297"

#Slightly worse than full feature equivalent ( 3 % worse validation error) and (4 % worse dem accuracy)

#To select which kernel we would use, try with RBF on balanced data with cost = 5
cat(paste("*****svm - rbf kernel - all features - balanced data - cost 5: "))
do.cross.validation.svm(data.demrep,"rbfc5","balancedata")
# [1] "rebalanced data target looks like: "
# dem  rep 
# 1652 1672 
# Using automatic sigma estimation (sigest) for RBF or laplace kernel 
# [1] "- - train error:  1.35379061371841"
# [1] "- - train accuracy democrat specific:  0.995157384987893"
# [1] "- - validation error:  13.1832797427653"
# [1] "- - validation accuracy democrat specific:  0.803418803418803"

#try its subset version as well
cat(paste("*****svm - rbf kernel - subset features - balanced data - cost 5: "))
do.cross.validation.svm(data.demrep.subset,"rbfc5","balancedata")
# [1] "rebalanced data target looks like: "
# dem  rep 
# 1628 1648 
# Using automatic sigma estimation (sigest) for RBF or laplace kernel 
# [1] "- - train error:  3.57142857142857"
# [1] "- - train accuracy democrat specific:  0.967444717444717"
# [1] "- - validation error:  13.3440514469453"
# [1] "- - validation accuracy democrat specific:  0.75968992248062"
#Behaves similar to full feature version but with worse dem accuracy.

#Also try quadratic kernel with cost 5
cat(paste("*****svm - quadratic kernel - all features - balanced data - cost 5: "))
do.cross.validation.svm(data.demrep,"quadratic5","balancedata")
# [1] "rebalanced data target looks like: "
# dem  rep 
# 1760 1782 
# [1] "- - train error:  0"
# [1] "- - train accuracy democrat specific:  1"
# [1] "- - validation error:  16.5594855305466"
# [1] "- - validation accuracy democrat specific:  0.702479338842975"
#not soo good

#Also try linear kernel with cost 5 on balancedata
cat(paste("*****svm - linear kernel - all features - balanced data - cost 5: "))
do.cross.validation.svm(data.demrep,"linearc5","balancedata")
# [1] "rebalanced data target looks like: "
# dem  rep 
# 1528 1547 
# Setting default kernel parameters  
# [1] "- - train error:  10.2439024390244"
# [1] "- - train accuracy democrat specific:  0.914921465968586"
# [1] "- - validation error:  16.3987138263666"
# [1] "- - validation accuracy democrat specific:  0.822368421052632
#Good dem accuracy, but bad overall all validation error

#######
# CONCLUSIONS for dem/rep classifier:
# Because we value simplicity (and hence tolerated a little worse validation error than a little worse dem accuracy), 
# looking at all of our models and their variations we'd either go with (in this order):
#   1) a Random Forest model using a subset features generated from a CFS filter on imbalanced data or 
#   2) a SVM using an rbf kernel and cost 1 using a subset of features on balanced data
# as these both use a fraction of the total variables availabe, have acceptable validation error (around 12 to 15%)
# and also have around 80% accuracy of identifying democrat counties.
# Our accuracy and overal error rate could be improved by including all variables, but at the cost of complexity.
# To improve the performance/reduce complexity further we could try:
#   1) to couple the random forest with boosting or the svm with bagging, 
#   2) try different values of the cost parameter for the svm to tune it and
#   3) try tuning parameters to random forest
#   4) see if we can find better subsets of variables further using backwards search or another method
# We attempted to use the Boruta functionality introduced in lab12, but after 1hour and a half we still had no results.
#######

## Now we retrain our two classifiers on our learning data (4/5 of the data) and see how they do 
## 1) get test error rate on Random Forest model using a subset features on imbalanced data


learnset <- data.demrep[learn,subset.vars.ids]
testset <- data.demrep[-learn,subset.vars.ids]
testset.without.target <- testset[,-24]


#then train classifier on learnset
clf <- randomForest(learnset$winner ~ ., data=learnset, ntree=20, nodesize=5, mtry=9)

#then predict on original test data with trained classifier
#predict on testing data
results.test <- predict(clf, testset.without.target)
tab <- table( testset$winner, results.test )
dt <- diag(prop.table(tab, 1))
error_rate.test <- 100 * (1-sum(tab[row(tab)==col(tab)])/sum(tab))
dt
# dem       rep 
# 0.6180556 0.9665971 
error_rate.test 
#11.39647



###################################################################################################
# SECTION 5.2: CLASSIFICATION OF blue island counties based on socio-economical/public health data  #
###################################################################################################

#blueisland dataset with target as blueisland
#take data.demrep from last time.. get rid of "winner" target and add blueisland from original dd.

data.bluei <- data.demrep
head(data.bluei)
dim(data.bluei)
colnames(data.bluei)

data.bluei <- data.bluei[,-59]
data.bluei$blueisland <- as.factor(dd[,62])

summary(data.bluei)
summary(data.bluei$blueisland)
#    0    1 
# 3029   84

#THIS DATA is massively imbalanced (as there are not many blueislands overall.. .027% to be exact )
#Our plan is to try the two models from last time on our data (except this time, not valuing simplicity, at least initial
#since our target is so small ) using all features on both
#(balanced/imbalanced versions), and then to try to improve from them.


do.cross.validation.random.forest.blueisland <- function (learndata,k,numtimes,options="none",setpriors=FALSE){
  initd <- learndata
  CV.folds <- generateCVRuns(initd$blueisland, ntimes=numtimes, nfold=k, stratified=TRUE)
  
  ## prepare the structure to store the partial results
  cv.results <- matrix (rep(0,8*k),nrow=k)
  colnames (cv.results) <- c("k","fold",'TR class non','TR class blu',"TR error",'VA class non','VA class blu',"VA error")
  cv.results[,"TR class non"] <- ""
  cv.results[,"TR class blu"] <- ""
  cv.results[,"TR error"] <- 0
  cv.results[,"VA class non"] <- ""
  cv.results[,"VA class blu"] <- ""
  cv.results[,"VA error"] <- 0
  cv.results[,"k"] <- k
  
  targetindex <- as.numeric(which(colnames(learndata)=="blueisland"))
  
  for(j in 1:k)    
  {
    #get va data
    va <- unlist(CV.folds[[1]][[j]])
    
    #train on TR data
    if(options == "balancedata"){
      notvd <- learndata[-va,]           
      #balance data with smote
      notvalidationdata <-SMOTE(blueisland ~ . , notvd ,k=2,perc.over = 3600,perc.under=102)         
      #newblueSmote <-SMOTE(blueisland ~ . , data.bluei ,k=10,perc.over = 3600,perc.under=102)
      
      #print("rebalanced data target looks like: ")
      #print(summary(notvalidationdata$blueisland))
      
      notvalidationdata.without.target <- notvalidationdata[,-targetindex]
    }
    else
    {
      notvalidationdata <- learndata[-va,]
      #print("LEARNING SET LOOKS LIKE: ")
      #print(summary(notvalidationdata$blueisland))
      notvalidationdata.without.target <- learndata[-va,-targetindex]
    }
    validationdata <- learndata[va,]  
    #print("VALIDATION SET LOOKS LIKE")
    #print(summary(validationdata$blueisland))
    validationdata.without.target <- learndata[va,-targetindex]  
    
    if( setpriors == TRUE){
      #print("use setpriors")
      clf <- randomForest(notvalidationdata$blueisland ~ ., data=notvalidationdata, ntree=500, nodesize=1, mtry=9, classwt=c(0.9730164,0.02698362))
      #clf <- randomForest(notvalidationdata$blueisland ~ ., data=notvalidationdata, ntree=20, nodesize=1, mtry=9, classwt=c(0.9,0.1))
    }
    else
    {
      clf <- randomForest(notvalidationdata$blueisland ~ ., data=notvalidationdata, ntree=20, nodesize=1, mtry=9)
    }
    
    #predict TR data
    results.test <- predict(clf, notvalidationdata.without.target)
    tab <- table( notvalidationdata$blueisland, results.test )
    dt <- diag(prop.table(tab, 1))
    #print("tr--")
    #print(tab)
    cv.results[j,"TR class non"] <- round(dt[1],4)
    if(is.nan(dt[2])){ cv.results[j,"TR class blu"] <- 0   }
    else{cv.results[j,"VA class blu"] <- tab[4] / (tab[4] + tab[3])    }
    #else{cv.results[j,"TR class blu"] <- round(dt[2],4)    }
    
    cv.results[j,"TR error"] <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
    
    #predict on Validation data       
    tab <- table(predict(clf,validationdata.without.target),validationdata$blueisland, dnn=list('predicted','actual'))
    dt <- diag(prop.table(tab, 1))
    #print("validtion--")
    #print(tab)   
    cv.results[j,"VA class non"] <- round(dt[1],4)
    if(is.nan(dt[2])){ cv.results[j,"VA class blu"] <- 0   }
    else{cv.results[j,"VA class blu"] <- tab[4] / (tab[4] + tab[3])    }
    #else{cv.results[j,"VA class blu"] <- round(dt[2],4)    }
    cv.results[j,"VA error"] <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
    
    cv.results[j,"fold"] <- j        
  }
  print(cv.results)
  VA.error <- mean(as.numeric(cv.results[,"VA error"]))
  VA.accuracy.dem <- mean(as.numeric(cv.results[,"VA class blu"]))
  
  print(paste("- validation error: ",VA.error))
  print(paste("- correctly identified blueisland specific: ",VA.accuracy.dem))
  list(er=VA.error,bl=VA.accuracy.dem,cl=clf) 
}


#Now try 
N <- nrow(data.bluei)
learnset <- data.bluei[learn,]
testset <- data.bluei[-learn,]
testset.without.target <- testset[,-24]
k <- 10
numtimes <- 10
print(summary(learnset$blueisland))
cat("**** blueisland - random forest - all features - imbalanced data - (no priors) crossvalidation results ***")
cv.results <- do.cross.validation.random.forest.blueisland(learnset,k,numtimes)

#     k    fold TR class non TR class blu TR error              VA class non VA class blu        VA error            
# [1,] "10" "1"  "1"          ""           "0.00446229361892014" "0.9759"     "0"                 "0.0240963855421686"
# [2,] "10" "2"  "1"          ""           "0.00133868808567605" "0.9759"     "0"                 "0.0240963855421686"
# [3,] "10" "3"  "1"          ""           "0.00133868808567605" "0.9798"     "0.166666666666667" "0.0200803212851406"
# [4,] "10" "4"  "1"          ""           "0.00178491744756804" "0.9758"     "0.142857142857143" "0.0240963855421686"
# [5,] "10" "5"  "1"          ""           "0.00178491744756804" "0.9758"     "0.142857142857143" "0.0240963855421686"
# [6,] "10" "6"  "1"          ""           "0"                   "0.9719"     "0"                 "0.0281124497991968"
# [7,] "10" "7"  "1"          ""           "0.00178491744756804" "0.9758"     "0.142857142857143" "0.0240963855421686"
# [8,] "10" "8"  "1"          ""           "0.00312360553324409" "0.9758"     "0.142857142857143" "0.0240963855421686"
# [9,] "10" "9"  "1"          ""           "0.00178491744756804" "0.9719"     "0"                 "0.0281124497991968"
# [10,]"10" "10" "1"          ""           "0.00178491744756804" "0.9717"     "0"                 "0.036144578313253" 
# [1] "- validation error:  0.0257028112449799"
# [1] "- correctly identified blueisland specific:  0.0738095238095239"

#As expected our overall validation error is great because of the imbalance of classes, but the classifiers 
#ability to identify blueislands is dismal (9%).  
#Setting the priors for the classes in randomForest to classwt=c(0.9730164,0.02698362), doesn't appear to help much.
#Presumably the function calculates these actual values anyways.
cat("**** blueisland - random forest - all features - imbalanced data - (with priors) crossvalidation results ***")
cv.results <- do.cross.validation.random.forest.blueisland(learnset,k,numtimes,"imbalancedata",setpriors=TRUE)
#      k    fold TR class non TR class blu TR error               VA class non VA class blu        VA error            
# [1,] "10" "1"  "1"          ""           "0.00133868808567605"  "0.9759"     "0"                 "0.0240963855421686"
# [2,] "10" "2"  "1"          ""           "0.00178491744756804"  "0.9798"     "0.166666666666667" "0.0200803212851406"
# [3,] "10" "3"  "1"          ""           "0.00223114680946002"  "0.9759"     "0"                 "0.0240963855421686"
# [4,] "10" "4"  "1"          ""           "0.00133868808567605"  "0.9757"     "0.142857142857143" "0.0281124497991968"
# [5,] "10" "5"  "1"          ""           "0.000892458723784073" "0.9719"     "0"                 "0.0281124497991968"
# [6,] "10" "6"  "1"          ""           "0.00223114680946002"  "0.9719"     "0"                 "0.0281124497991968"
# [7,] "10" "7"  "1"          ""           "0.00133868808567605"  "0.9798"     "0.285714285714286" "0.0200803212851406"
# [8,] "10" "8"  "1"          ""           "0.00223114680946002"  "0.9758"     "0.142857142857143" "0.0240963855421686"
# [9,] "10" "9"  "1"          ""           "0.00133868808567605"  "0.9719"     "0"                 "0.0281124497991968"
# [10,]"10" "10" "1"          ""           "0.00223114680946002"  "0.9798"     "0.285714285714286" "0.0200803212851406"
# [1] "- validation error:  0.0244979919678715"
# [1] "- correctly identified blueisland specific:  0.102380952380952"


#See how RF performs on Balanced data
cv.results <- do.cross.validation.random.forest.blueisland(learnset,k,numtimes,"balancedata",setpriors=TRUE)
# k    fold TR class non TR class blu TR error VA class non VA class blu        VA error            
# [1,] "10" "1"  "1"          ""           "0"      "0.9834"     "0.333333333333333" "0.0401606425702812"
# [2,] "10" "2"  "1"          ""           "0"      "0.9873"     "0.5"               "0.0522088353413654"
# [3,] "10" "3"  "1"          ""           "0"      "0.9917"     "0.666666666666667" "0.0281124497991968"
# [4,] "10" "4"  "1"          ""           "0"      "0.9833"     "0.428571428571429" "0.0401606425702812"
# [5,] "10" "5"  "1"          ""           "0"      "0.9829"     "0.428571428571429" "0.0642570281124498"
# [6,] "10" "6"  "1"          ""           "0"      "0.9791"     "0.285714285714286" "0.0522088353413654"
# [7,] "10" "7"  "1"          ""           "0"      "0.983"      "0.428571428571429" "0.0602409638554217"
# [8,] "10" "8"  "1"          ""           "0"      "0.9793"     "0.285714285714286" "0.0441767068273092"
# [9,] "10" "9"  "1"          ""           "0"      "0.9753"     "0.142857142857143" "0.0441767068273092"
# [10,] "10" "10" "1"          ""           "0"      "0.9873"     "0.571428571428571" "0.0441767068273092"
# [1] "- validation error:  0.0469879518072289"
# [1] "- correctly identified blueisland specific:  0.407142857142857"
#This does much much better actually in terms of blueisland identification (4 times as high) with only 
#marginally worse validation error!  Additionally in this case, we see that setting the priors 
#( which in our code also sets the ntree to be 500 as opposed to 20) improves the performance of identifying
# blueislands as well (those this adds quite a bit of extra calculation time).  Below we show the results
# of not setting the priors

cv.results <- do.cross.validation.random.forest.blueisland(learnset,k,numtimes,"balancedata")
# k    fold TR class non TR class blu TR error VA class non VA class blu        VA error            
# [1,] "10" "1"  "1"          ""           "0"      "0.9757"     "0"                 "0.0321285140562249"
# [2,] "10" "2"  "1"          ""           "0"      "0.9837"     "0.333333333333333" "0.0240963855421686"
# [3,] "10" "3"  "1"          ""           "0"      "0.9837"     "0.333333333333333" "0.0240963855421686"
# [4,] "10" "4"  "1"          ""           "0"      "0.9793"     "0.285714285714286" "0.0441767068273092"
# [5,] "10" "5"  "1"          ""           "0"      "0.9712"     "0"                 "0.0522088353413654"
# [6,] "10" "6"  "1"          ""           "0"      "0.9796"     "0.285714285714286" "0.0281124497991968"
# [7,] "10" "7"  "1"          ""           "0"      "0.9835"     "0.428571428571429" "0.0321285140562249"
# [8,] "10" "8"  "1"          ""           "0"      "0.971"      "0"                 "0.0602409638554217"
# [9,] "10" "9"  "1"          ""           "0"      "0.9754"     "0.142857142857143" "0.0401606425702812"
# [10,] "10" "10" "1"          ""           "0"      "0.9876"     "0.571428571428571" "0.0240963855421686"
# [1] "- validation error:  0.036144578313253"
# [1] "- correctly identified blueisland specific:  0.238095238095238"

#Lets see if we can get more a stable version of random forest with balanced data, and choose classifier which worked best
numtimes <- 2
cl.list <- list()
rf.cost.results <- matrix (rep(0,2*numtimes),nrow=numtimes)
for( j in 1:numtimes)
{
  res <- do.cross.validation.random.forest.blueisland(learnset,10,10,"balancedata",setpriors=TRUE)
  rf.cost.results[j,1] <- res$er
  rf.cost.results[j,2] <- res$bl
  cl.list[j] <- res$cl
}
rf.cost.results
validation.errors <- c()
blueisland.accuracy <- c()
for( i in 1:numtimes)
{  
  validation.errors[i] <- mean(svm.cost.results[i,c(1,3,5,7,9,11,13,15,17,19)])
  blueisland.accuracy[i] <- mean(svm.cost.results[i,c(2,4,6,8,10,12,14,16,18)])
}


#TODO: See how RF + boosting works?

do.cross.validation.svm.blueisland <- function(data.bi,kerneltype,options="none",cost=0){
  N <- nrow(data.bi)
  learn <- sample(1:N, round(4*N/5)) #take 4/5 of data for learning
  nlearn <- length(learn)
  svmlearn <- data.bi[learn,]
  validation <- sample(1:nlearn, round(nlearn/4))  #take 1/4 of learning for validation
  targetindex <- as.numeric(which(colnames(svmlearn)=="blueisland"))
  
  if(options == "balancedata"){
    notvd <- svmlearn[-validation,]            
    #this makes the sets equal
    svmlearn.train <-SMOTE(blueisland ~ . , notvd ,k=2,perc.over = 375,perc.under=135)         
    svmlearn.train.without.target <- svmlearn.train[,-targetindex]
  }
  else
  {
    svmlearn.train <- svmlearn[-validation,]   #training is the learning data without validation    
    svmlearn.train.without.target <- svmlearn.train[,-targetindex]
  }
  #print("training data looks like: ")
  #print(summary(svmlearn.train$blueisland))
  
  #train on training data
  if(kerneltype == "linear"){
    mi.svm <- ksvm (blueisland~.,data=svmlearn.train,kernel='polydot',C=1,cross=10)
  }
  if(kerneltype == "linearc5"){
    mi.svm <- ksvm (blueisland~.,data=svmlearn.train,kernel='polydot',C=5,cross=10)
  }
  if(kerneltype == "rbf"){
    if(cost == 0){ c = 1}
    else{ c = cost }
    mi.svm <- ksvm (blueisland~.,data=svmlearn.train,C=c,cross=10)
  }
  if(kerneltype == "rbfc5")
  {
    mi.svm <- ksvm (blueisland~.,data=svmlearn.train,C=5,cross=10)
  }
  
  svmlearn.validation <- svmlearn[validation,]  
  svmlearn.validation.without.target <- svmlearn[validation,-targetindex]
  #print("validation data looks like: ")
  #print(summary(svmlearn.validation$blueisland))
  
  #test svm on training data
  svmpred <- predict (mi.svm, svmlearn.train.without.target)
  tab <- table(svmpred,svmlearn.train$blueisland)
  dt <- diag(prop.table(tab, 1))
  error_rate.train <- 100*(1-sum(diag(tab))/sum(tab))
  print(paste("- - train error: ",error_rate.train))
  print(paste("- - train accuracy blueisland specific: ",tab[4] / (tab[4] + tab[3])))
  
  #test on validation
  svmpred <- predict (mi.svm, svmlearn.validation.without.target)
  tab <- table(svmpred,svmlearn.validation$blueisland)
  print("validation confusion table")
  print(tab)
  dt <- diag(prop.table(tab, 1))
  error_rate.validation <- 100*(1-sum(diag(tab))/sum(tab))
  
  print(paste("- - validation error: ",error_rate.validation))
  blspecific <- tab[4] / (tab[4] + tab[3])
  print(paste("- - validation accuracy blueisland specific: ", blspecific))
  list(er=error_rate.validation,bl=blspecific)
}

#See how SVM performs on imbalanced
cat("**** blueisland - svm - all features - rbf (cost5) - imbalanced data - (no priors) crossvalidation results ***")
cv.results <- do.cross.validation.svm.blueisland(data.bluei,"rbfc5")
# Using automatic sigma estimation (sigest) for RBF or laplace kernel 
# [1] "- - train error:  1.07066381156317"
# [1] "- - train accuracy blueisland specific:  0.607843137254902"
# [1] "validation confusion table"
# svmpred   0   1
#       0 602  17
#       1   1   2
# [1] "- - validation error:  2.89389067524116"
# [1] "- - validation accuracy blueisland specific:  0.105263157894737"

#This is quite bad compared with the random forest approach.

#Lets see how SVM performs on balanced data
cat("**** blueisland - svm - all features - rbf (cost5) - balanced data - (no priors) crossvalidation results ***")
cv.results <- do.cross.validation.svm.blueisland(data.bluei,"rbfc5","balancedata")
# Using automatic sigma estimation (sigest) for RBF or laplace kernel 
# [1] "- - train error:  1.1049723756906"
# [1] "- - train accuracy blueisland specific:  1"
# [1] "validation confusion table"
# svmpred   0   1
#       0 558   6
#       1  50   8
# [1] "- - validation error:  9.0032154340836"
# [1] "- - validation accuracy blueisland specific:  0.571428571428571"

#This is much better, though the validation error is much higher than the randomforest one on balanced data (with priors set)

#its evident that svms even with cross validation return very different results.  
#lets try cost 1 to 10, and for each run it 10 times to see what we get.  We can find a good cost value this way.
k <- 10
numtimes <- 10
svm.cost.results <- matrix (rep(0,2*numtimes*k),nrow=numtimes)
for( i in 1:numtimes)
{
  for( j in 1:k){
    j.one = (j - 1) * 2
    j.two = j.one + 1
    res <- do.cross.validation.svm.blueisland(data.bluei,"rbf","balancedata",cost=i)
    svm.cost.results[i,j.one] <- res$ev
    svm.cost.results[i,j.two] <- res$bl
  }
}
svm.cost.results
validation.errors <- c()
blueisland.accuracy <- c()
for( i in 1:numtimes)
{  
  blueisland.accuracy[i] <- mean(svm.cost.results[i,c(1,3,5,7,9,11,13,15,17,19)])
  validation.errors[i] <- mean(svm.cost.results[i,c(2,4,6,8,10,12,14,16,18)])
}
blueisland.accuracy
#[1] 0.6626033 0.6648496 0.5927921 0.5211295 0.5276301 0.5277543 0.4882462 0.4713116 0.4211769 0.5031714
validation.errors
#[1] 12.915327 12.290104 10.664523 10.450161 10.235798  9.682029 10.110754  9.503394 10.432297  9.628439




#ADDING BACK IN STATE DATA as our other results weren't amazing 
#( randomforest did the best in terms of finding blueislands )
initialmap <- read.csv("finaldata-withblueislands.csv", na.strings = '', stringsAsFactors = TRUE)

data.bluei.with.states <- data.bluei
data.bluei.with.states$state <- as.factor(initialmap$state.x)
levels(data.bluei.with.states$state) <- levels(initialmap$state.x)
table(data.bluei.with.states$state)

N <- nrow(data.bluei.with.states)
learn <- sample(1:N, round(4*N/5))
learnset <- data.bluei.with.states[learn,]
testset <- data.bluei.with.states[-learn,]
testset.without.target <- testset[,-62]
k <- 10
numtimes <- 10
cv.results <- do.cross.validation.random.forest.blueisland(learnset,k,numtimes,"balancedata",setpriors=TRUE)

#Error in randomForest.default(m, y, ...) : 
#Can not handle categorical predictors with more than 32 categories.

#so make many binary variables (50 total for each state)
mydata <- as.data.frame(data.bluei.with.states$state)
colnames(mydata)[1] <- "state"
mydata$target <- 1
mydata$x <- 0
states <- model.matrix(target ~ x + state,mydata)
colnames(states)[2] <- "stateAL"
states[which(initialmap$state.x == "AL"),2] <- 1
dim(states)
states <- states[,2:51]
summary(states)

#make new dframe with data.blue and states
data.bluei.with.binary.states <- cbind(data.bluei,states)

#now try again, with wide dataset
N <- nrow(data.bluei.with.binary.states)
learnset <- data.bluei.with.binary.states[learn,]
testset <- data.bluei.with.binary.states[-learn,]
testset.without.target <- testset[,-62]
k <- 10
numtimes <- 10
cv.results <- do.cross.validation.random.forest.blueisland(learnset,k,numtimes,"balancedata",setpriors=TRUE)
#      k    fold TR class non TR class blu TR error VA class non VA class blu        VA error            
# [1,] "10" "1"  "1"          ""           "0"      "0.9797"     "0.166666666666667" "0.0281124497991968"
# [2,] "10" "2"  "1"          ""           "0"      "0.9874"     "0.5"               "0.0401606425702812"
# [3,] "10" "3"  "1"          ""           "0"      "0.9876"     "0.5"               "0.0321285140562249"
# [4,] "10" "4"  "1"          ""           "0"      "0.9834"     "0.333333333333333" "0.0401606425702812"
# [5,] "10" "5"  "1"          ""           "0"      "0.9795"     "0.285714285714286" "0.0321285140562249"
# [6,] "10" "6"  "1"          ""           "0"      "0.9792"     "0.285714285714286" "0.0481927710843374"
# [7,] "10" "7"  "1"          ""           "0"      "0.9791"     "0.285714285714286" "0.0522088353413654"
# [8,] "10" "8"  "1"          ""           "0"      "0.9793"     "0.285714285714286" "0.0441767068273092"
# [9,] "10" "9"  "1"          ""           "0"      "0.9755"     "0.142857142857143" "0.036144578313253" 
# [10,] "10" "10" "1"          ""           "0"      "0.9753"     "0.142857142857143" "0.0441767068273092"
# [1] "- validation error:  0.0397590361445783"
# [1] "- correctly identified blueisland specific:  0.292857142857143"

cv.results <- do.cross.validation.random.forest.blueisland(learnset,k,numtimes,"balancedata",setpriors=TRUE)
# k    fold TR class non TR class blu TR error VA class non VA class blu        VA error            
# [1,] "10" "1"  "1"          ""           "0"      "0.9792"     "0.166666666666667" "0.0522088353413654"
# [2,] "10" "2"  "1"          ""           "0"      "0.9874"     "0.5"               "0.0441767068273092"
# [3,] "10" "3"  "1"          ""           "0"      "0.9835"     "0.333333333333333" "0.0321285140562249"
# [4,] "10" "4"  "1"          ""           "0"      "0.9877"     "0.5"               "0.0240963855421686"
# [5,] "10" "5"  "1"          ""           "0"      "0.9757"     "0.142857142857143" "0.0281124497991968"
# [6,] "10" "6"  "1"          ""           "0"      "0.9757"     "0.142857142857143" "0.0281124497991968"
# [7,] "10" "7"  "1"          ""           "0"      "0.9754"     "0.142857142857143" "0.0401606425702812"
# [8,] "10" "8"  "1"          ""           "0"      "0.9875"     "0.571428571428571" "0.0321285140562249"
# [9,] "10" "9"  "1"          ""           "0"      "0.9751"     "0.142857142857143" "0.0522088353413654"
# [10,] "10" "10" "1"          ""           "0"      "0.9833"     "0.428571428571429" "0.0441767068273092"
# [1] "- validation error:  0.0377510040160642"
# [1] "- correctly identified blueisland specific:  0.307142857142857"

#On two different runs, we see that this added information really doesn't buy us much.

#GET FINAL TEST ERROR ON BLUEISLAND TASK
#without states
N <- nrow(data.bluei)
learnset <- data.bluei[learn,]
testset <- data.bluei[-learn,]
testset.without.target <- testset[,-62]
#cv.results <- do.cross.validation.svm.blueisland(data.bluei,"rbfc5","balancedata")

targetindex <- as.numeric(which(colnames(learnset)=="blueisland"))
svmlearn.train <-SMOTE(blueisland ~ . , learnset ,k=2,perc.over = 3600,perc.under=102)         
svmlearn.train.without.target <- svmlearn.train[,-targetindex]

#train on training data
mi.svm <- ksvm (blueisland~.,data=svmlearn.train,C=5,cross=10)

#test svm on testing data
svmpred <- predict (mi.svm, testset.without.target)
tab <- table(svmpred,testset$blueisland)
dt <- diag(prop.table(tab, 1))
error_rate.test <- 100*(1-sum(diag(tab))/sum(tab))
print(paste("- - test error: ",error_rate.test))   #3.6918
print(paste("- - test accuracy blueisland specific: ",tab[4] / (tab[4] + tab[3])))   #.29411


