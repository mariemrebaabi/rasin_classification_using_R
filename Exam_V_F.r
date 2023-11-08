######################Loading Dataset#################
Data <-read.table(file=file.choose(),header=TRUE,sep=",",dec=".")
View(Data)
str(Data)
summary(Data)
#####################Data Pre-processing##############
##########1. Outliers :

##########Area:
summary(Data$Area)
hist(Data$Area,xlab = "Area",main = "Histogram of Area",breaks = sqrt(nrow(Data)))
#Boxplot
boxplot(Data$Area,ylab = "Area")
#outliers
out <- boxplot.stats(Data$Area)$out
out
#indexes of outliers
out_ind <- which(Data$Area %in% c(out))
out_ind
for (x in c(out_ind))
{Data$Area[x] <- NA} 


##########MajorAxisLength:
summary(Data$MajorAxisLength)
hist(Data$MajorAxisLength,xlab = "MajorAxisLength",main = "Histogram of MajorAxisLength",breaks = sqrt(nrow(Data)))
#Boxplot
boxplot(Data$MajorAxisLength,ylab = "MajorAxisLength")
#outliers
out1 <- boxplot.stats(Data$MajorAxisLength)$out
out1
#indexes of outliers
out1_ind <- which(Data$MajorAxisLength %in% c(out1))
out1_ind
for (x in c(out1_ind))
{Data$MajorAxisLength[x] <- NA}

##########MinorAxisLength:
summary(Data$MinorAxisLength)
hist(Data$MinorAxisLength,xlab = "MinorAxisLength",main = "Histogram of MinorAxisLength",breaks = sqrt(nrow(Data)))
#Boxplot
boxplot(Data$MinorAxisLength,ylab = "MinorAxisLength")
#outliers
out2 <- boxplot.stats(Data$MinorAxisLength)$out
out2
#indexes of outliers
out2_ind <- which(Data$MinorAxisLength %in% c(out2))
out2_ind
for (x in c(out2_ind))
{Data$MinorAxisLength[x] <- NA}

##########Eccentricity :
summary(Data$Eccentricity )
hist(Data$Eccentricity ,xlab = "Eccentricity ",main = "Histogram of Eccentricity ",breaks = sqrt(nrow(Data)))
#Boxplot
boxplot(Data$Eccentricity ,ylab = "Eccentricity ")
#outliers
out3 <- boxplot.stats(Data$Eccentricity )$out
out3
#indexes of outliers
out3_ind <- which(Data$Eccentricity  %in% c(out3))
out3_ind
for (x in c(out3_ind))
{Data$Eccentricity[x] <- NA}

##########ConvexArea :
summary(Data$ConvexArea )
hist(Data$ConvexArea ,xlab = "ConvexArea ",main = "Histogram of ConvexArea ",breaks = sqrt(nrow(Data)))
#Boxplot
boxplot(Data$ConvexArea ,ylab = "ConvexArea ")
#outliers
out4 <- boxplot.stats(Data$ConvexArea )$out
out4
#indexes of outliers
out4_ind <- which(Data$ConvexArea  %in% c(out4))
out4_ind
for (x in c(out4_ind))
{Data$ConvexArea[x] <- NA}

##########Extent :
summary(Data$Extent )
hist(Data$Extent ,xlab = "Extent ",main = "Histogram of Extent ",breaks = sqrt(nrow(Data)))
#Boxplot
boxplot(Data$Extent ,ylab = "Extent ")
#outliers
out5 <- boxplot.stats(Data$Extent )$out
out5
#indexes of outliers
out5_ind <- which(Data$Extent  %in% c(out5))
out5_ind
for (x in c(out5_ind))
{Data$Extent[x] <- NA}

##########Perimeter :
summary(Data$Perimeter )
hist(Data$Perimeter ,xlab = "Perimeter ",main = "Histogram of Perimeter ",breaks = sqrt(nrow(Data)))
#Boxplot
boxplot(Data$Perimeter ,ylab = "Perimeter ")
#outliers
out6 <- boxplot.stats(Data$Perimeter )$out
out6
#indexes of outliers
out6_ind <- which(Data$Perimeter  %in% c(out6))
out6_ind
for (x in c(out6_ind))
{Data$Perimeter[x] <- NA}


##########2. Missing Values :
##Rate of missiing values for all the variables
(colMeans(is.na(Data)))*100
##imputation of missing Values column 
library(VIM)
Data_Clean<- kNN(Data)
summary(Data_Clean)
DATA<- subset(Data_Clean,select = Area:Class)
summary(DATA)
##Resulat of imputation 
(colMeans(is.na(DATA)))*100

#####################Task 3 : Univariate Analysis##############

##########normality :
shapiro.test(DATA$Area)
#==> The p-value of the test is less than .05, which indicates that the data is not normally distributed
shapiro.test(DATA$MajorAxisLength)
#==> The p-value of the test is less than .05, which indicates that the data is not normally distributed
shapiro.test(DATA$MinorAxisLength)
#==> The p-value of the test is less than .05, which indicates that the data is not normally distributed
shapiro.test(DATA$Eccentricity)
#==> The p-value of the test is less than .05, which indicates that the data is not normally distributed
shapiro.test(DATA$ConvexArea )
#==> The p-value of the test is less than .05, which indicates that the data is not normally distributed
shapiro.test(DATA$Extent)
#==> The p-value of the test is less than .05, which indicates that the data is not normally distributed
shapiro.test(DATA$Perimeter)
#==> The p-value of the test is less than .05, which indicates that the data is not normally distributed

##########Modality :
#Class
a7=which(DATA$Class=="" )
for(x in c(a7))
{DATA$Class[x]<-NA}
DATA=kNN(DATA,variable=c("Class"))
summary(DATA$Class)
unique(DATA$Class)
View(DATA)

#####################Task 4 : Bivariate Analysis :##############

##########Correlation
 
Numeric_Data<- subset(DATA,select = Area:Perimeter)
summary(Numeric_Data)
cor(Numeric_Data,method = "spearman") 
cor.test(Numeric_Data$Area ,Numeric_Data$MajorAxisLength,method = "spearman",exact = FALSE)
#Since this p-value is below my alpha level (0.05),
#I will reject the null hypothesis and accept the alternative hypothesis.
#In other words, there is a significant (Positive) 
#correlation between the Area and MajorAxisLength of our Data

#Area
cor.test(Numeric_Data$Area ,Numeric_Data$MinorAxisLength,method = "spearman",exact = FALSE)
cor.test(Numeric_Data$Area ,Numeric_Data$Eccentricity,method = "spearman",exact = FALSE)
cor.test(Numeric_Data$Area ,Numeric_Data$ConvexArea,method = "spearman",exact = FALSE)
cor.test(Numeric_Data$Area ,Numeric_Data$Extent,method = "spearman",exact = FALSE)
cor.test(Numeric_Data$Area ,Numeric_Data$Perimeter,method = "spearman",exact = FALSE)

#ConvexArea
cor.test(Numeric_Data$MajorAxisLength ,Numeric_Data$ConvexArea,method = "spearman",exact = FALSE)
cor.test(Numeric_Data$MinorAxisLength ,Numeric_Data$ConvexArea,method = "spearman",exact = FALSE)
cor.test(Numeric_Data$Eccentricity ,Numeric_Data$ConvexArea,method = "spearman",exact = FALSE)
cor.test(Numeric_Data$Extent ,Numeric_Data$ConvexArea,method = "spearman",exact = FALSE)
cor.test(Numeric_Data$Perimeter ,Numeric_Data$ConvexArea,method = "spearman",exact = FALSE)

#MajorAxisLength
cor.test(Numeric_Data$MajorAxisLength ,Numeric_Data$MinorAxisLength,method = "spearman",exact = FALSE)
cor.test(Numeric_Data$MajorAxisLength ,Numeric_Data$Eccentricity,method = "spearman",exact = FALSE)
cor.test(Numeric_Data$MajorAxisLength ,Numeric_Data$Extent,method = "spearman",exact = FALSE)
cor.test(Numeric_Data$MajorAxisLength ,Numeric_Data$Perimeter,method = "spearman",exact = FALSE)

#Eccentricity
cor.test(Numeric_Data$MinorAxisLength ,Numeric_Data$Eccentricity,method = "spearman",exact = FALSE)
cor.test(Numeric_Data$Extent ,Numeric_Data$Eccentricity,method = "spearman",exact = FALSE)
cor.test(Numeric_Data$Perimeter ,Numeric_Data$Eccentricity,method = "spearman",exact = FALSE)

#Perimeter
cor.test(Numeric_Data$Perimeter ,Numeric_Data$Extent,method = "spearman",exact = FALSE)
cor.test(Numeric_Data$Perimeter ,Numeric_Data$MinorAxisLength,method = "spearman",exact = FALSE)

#Extent
cor.test(Numeric_Data$MinorAxisLength ,Numeric_Data$Extent,method = "spearman",exact = FALSE)


#####################Task 5 : Linear regression :##############
reg_multi <- lm(MinorAxisLength~Area+MajorAxisLength+Eccentricity+ConvexArea+Extent+Perimeter,data=DATA)
summary(reg_multi)
reg_multi_1<- lm(MinorAxisLength~Area+MajorAxisLength+Eccentricity+Extent+Perimeter,data=DATA)
summary(reg_multi_1)

##################PCA
#Step 1:Standardize the data by using scale and apply “prcomp” function
pc <- prcomp(DATA[,c(1:7)],center = TRUE,scale. = TRUE)
print(pc)
summary(pc)

#Step 2:  Built PCA_Variance function for exploratory data analysis on Variance
PCA_Variance <- function(x) {
    x.var <- x$sdev ^ 2
    x.pvar <- x.var/sum(x.var)
    print("proportions of variance:")
    print(x.pvar)
    par(mfrow=c(2,2))
    plot(x.pvar,xlab="Principal component",
    ylab="Proportion of variance explained", ylim=c(0,1), type='b')
    plot(cumsum(x.pvar),xlab="Principal component",
    ylab="Cumulative Proportion of variance explained", ylim=c(0,1), type='b')
    screeplot(x)
    screeplot(x,type="l")
    par(mfrow=c(1,1))}

#Step 3: Choose the principal components with highest variances
PCA_Variance(pc)
biplot(pc,scale=0, cex=.7)
#Arrows are closer to each other indicates the high correlation

#Step 4: Visulization of Data in the new reduced dimension
pca.out <- pc
pca.out$rotation <- -pca.out$rotation
pca.out$x <- -pca.out$x
biplot(pca.out,scale=0, cex=.7)

pca.out$rotation[,1:3]







######################## GLM ###############################

install.packages("sjPlot")
install.packages("tidyverse")
install.packages("lme4")


library(sjPlot)
library(tidyverse)
library(lme4)


g1 <- glm(MinorAxisLength~Area+MajorAxisLength+Eccentricity+ConvexArea+Extent+Perimeter,
          family = gaussian(), 
          data = Data)

summary(g1)
tab_model(g1)

