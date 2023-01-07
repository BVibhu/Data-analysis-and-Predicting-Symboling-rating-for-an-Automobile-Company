install.packages("GGally")
install.packages("ggfortify")
install.packages("plotly")
install.packages("vioplot")
install.packages("tree")
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("gbm")

library(gbm)
library(ggplot2)
require(methods)
library(dplyr)
library(plotly)
library(corrplot)
library(ggfortify)
library(GGally)
library(vioplot)
library(randomForest)
library(tree)
library(rpart)				        
library(rpart.plot)	


#attach file
Automob = read.csv('C:/Users/workV/Downloads/Automobile_data.csv')
attach(Automob)
head(Automob)

#look at summary
summary(Automob)

#making price,bore,stroke,peak.rpm as numerical

Automob$price <- as.numeric(Automob$price)
Automob$bore <- as.numeric(Automob$bore)
Automob$stroke <- as.numeric(Automob$stroke)
Automob$peak.rpm <- as.numeric(Automob$peak.rpm)
Automob$horsepower <- as.numeric(Automob$horsepower)




#symboling as categorical
Automob$symboling <- as.factor(Automob$symboling)

#categortical FACTORIZED
Automob$make<-as.factor(Automob$make)
Automob$fuel.type<-as.factor(Automob$fuel.type)
Automob$aspiration<-as.factor(Automob$aspiration)
Automob$num.of.doors<-as.factor(Automob$num.of.doors)
Automob$body.style<-as.factor(Automob$body.style)
Automob$drive.wheels<-as.factor(Automob$drive.wheels)
Automob$engine.location<-as.factor(Automob$engine.location)
Automob$engine.type<-as.factor(Automob$engine.type)
Automob$fuel.system<-as.factor(Automob$fuel.system)
Automob$num.of.cylinders<-as.factor(Automob$num.of.cylinders)




#check for NA values
colSums(is.na(Automob))

#Data Exploration
str(Automob)


#We see presence of "?" so Dealing with '?'
Automob[Automob == '?'] <- NA
colSums(is.na(Automob))

#remove normalized losses as we don need it in our exploration
Automob=Automob[-2]
names(Automob)

#looking at all rows with NA values
na_rows <- Automob[!complete.cases(Automob), ]
na_rows



summary((Automob$num.of.doors))

#num.of.doors is categorical , all others is Continuous
Automob[c(28),5]="four"
Automob[c(64),5]="four"
Automob[c(28,64),]
summary(Automob$num.of.doors)
table(Automob$num.of.doors)


#Using Forest to predict other missing values
forest_price=randomForest(price ~ symboling+length+curb.weight+wheel.base+horsepower+highway.mpg+make+num.of.doors+body.style+engine.type+fuel.system, ntree=1000, data=Automob, importance=TRUE,  na.action = na.omit)
forest_price
forest_bore=randomForest(bore ~ symboling+length+curb.weight+wheel.base+price+horsepower+highway.mpg+make+num.of.doors+body.style+engine.type+fuel.system, ntree=400, data=Automob, importance=TRUE,  na.action = na.omit)
forest_bore
forest_stroke=randomForest(stroke ~ symboling+length+curb.weight+wheel.base+price+horsepower+highway.mpg+make+num.of.doors+body.style+engine.type+fuel.system, ntree=1500, data=Automob, importance=TRUE,  na.action = na.omit)
forest_stroke

p1=predict(forest_price,data.frame(symboling=factor('0', levels=levels(Automob$symboling)),length=178.2,curb.weight=3053,wheel.base=99.5,horsepower=160,highway.mpg=22,make=factor('audi', levels=levels(Automob$make)) , num.of.doors=factor('two' , levels=levels(Automob$num.of.doors)) , body.style=factor('hatchback' , levels=levels(Automob$body.style)),engine.type=factor('ohc' , levels=levels(Automob$engine.type)) , fuel.system=factor('mpfi' , levels=levels(Automob$fuel.system))))
p1
p2=predict(forest_price,data.frame(symboling=factor('1', levels=levels(Automob$symboling)),length=155.9,curb.weight=1874,wheel.base=94.5,horsepower=70,highway.mpg=43,make=factor('isuzu', levels=levels(Automob$make)) , num.of.doors=factor('two' , levels=levels(Automob$num.of.doors)) , body.style=factor('sedan' , levels=levels(Automob$body.style)),engine.type=factor('ohc' , levels=levels(Automob$engine.type)) , fuel.system=factor('2bbl' , levels=levels(Automob$fuel.system))))
p2
p3=predict(forest_price,data.frame(symboling=factor('0', levels=levels(Automob$symboling)),length=155.9,curb.weight=1909,wheel.base=94.5,horsepower=70,highway.mpg=43,make=factor('isuzu', levels=levels(Automob$make)) , num.of.doors=factor('four' , levels=levels(Automob$num.of.doors)) , body.style=factor('sedan' , levels=levels(Automob$body.style)),engine.type=factor('ohc' , levels=levels(Automob$engine.type)) , fuel.system=factor('2bbl' , levels=levels(Automob$fuel.system))))
p3
p4=predict(forest_price,data.frame(symboling=factor('1', levels=levels(Automob$symboling)),length=175.7,curb.weight=3366,wheel.base=98.4,horsepower=288,highway.mpg=28,make=factor('porsche', levels=levels(Automob$make)) , num.of.doors=factor('two' , levels=levels(Automob$num.of.doors)) , body.style=factor('hatchback' , levels=levels(Automob$body.style)),engine.type=factor('dohcv' , levels=levels(Automob$engine.type)) , fuel.system=factor('mpfi' , levels=levels(Automob$fuel.system))))
p4
Automob[c(10),25]=p1
Automob[c(45),25]=p2
Automob[c(46),25]=p3
Automob[c(130),25]=p4

b1=predict(forest_bore,data.frame(symboling=factor('3', levels=levels(Automob$symboling)),length=169,curb.weight=2380,wheel.base=95.3,price=10945,horsepower=101,highway.mpg=23,make=factor('mazda', levels=levels(Automob$make)) , num.of.doors=factor('two' , levels=levels(Automob$num.of.doors)) , body.style=factor('hatchback' , levels=levels(Automob$body.style)),engine.type=factor('rotor' , levels=levels(Automob$engine.type)) , fuel.system=factor('4bbl' , levels=levels(Automob$fuel.system))))
b2=predict(forest_bore,data.frame(symboling=factor('3', levels=levels(Automob$symboling)),length=169,curb.weight=2380,wheel.base=95.3,price=11845,horsepower=101,highway.mpg=23,make=factor('mazda', levels=levels(Automob$make)) , num.of.doors=factor('two' , levels=levels(Automob$num.of.doors)) , body.style=factor('hatchback' , levels=levels(Automob$body.style)),engine.type=factor('rotor' , levels=levels(Automob$engine.type)) , fuel.system=factor('4bbl' , levels=levels(Automob$fuel.system))))
b3=predict(forest_bore,data.frame(symboling=factor('3', levels=levels(Automob$symboling)),length=169,curb.weight=2380,wheel.base=95.3,price=13645,horsepower=101,highway.mpg=23,make=factor('mazda', levels=levels(Automob$make)) , num.of.doors=factor('two' , levels=levels(Automob$num.of.doors)) , body.style=factor('hatchback' , levels=levels(Automob$body.style)),engine.type=factor('rotor' , levels=levels(Automob$engine.type)) , fuel.system=factor('4bbl' , levels=levels(Automob$fuel.system))))
b4=predict(forest_bore,data.frame(symboling=factor('3', levels=levels(Automob$symboling)),length=169,curb.weight=2380,wheel.base=95.3,price=15645,horsepower=135,highway.mpg=23,make=factor('mazda', levels=levels(Automob$make)) , num.of.doors=factor('two' , levels=levels(Automob$num.of.doors)) , body.style=factor('hatchback' , levels=levels(Automob$body.style)),engine.type=factor('rotor' , levels=levels(Automob$engine.type)) , fuel.system=factor('mpfi' , levels=levels(Automob$fuel.system))))
s1=predict(forest_bore,data.frame(symboling=factor('3', levels=levels(Automob$symboling)),length=169,curb.weight=2380,wheel.base=95.3,price=10945,horsepower=101,highway.mpg=23,make=factor('mazda', levels=levels(Automob$make)) , num.of.doors=factor('two' , levels=levels(Automob$num.of.doors)) , body.style=factor('hatchback' , levels=levels(Automob$body.style)),engine.type=factor('rotor' , levels=levels(Automob$engine.type)) , fuel.system=factor('4bbl' , levels=levels(Automob$fuel.system))))
s2=predict(forest_bore,data.frame(symboling=factor('3', levels=levels(Automob$symboling)),length=169,curb.weight=2380,wheel.base=95.3,price=11845,horsepower=101,highway.mpg=23,make=factor('mazda', levels=levels(Automob$make)) , num.of.doors=factor('two' , levels=levels(Automob$num.of.doors)) , body.style=factor('hatchback' , levels=levels(Automob$body.style)),engine.type=factor('rotor' , levels=levels(Automob$engine.type)) , fuel.system=factor('4bbl' , levels=levels(Automob$fuel.system))))
s3=predict(forest_bore,data.frame(symboling=factor('3', levels=levels(Automob$symboling)),length=169,curb.weight=2380,wheel.base=95.3,price=13645,horsepower=101,highway.mpg=23,make=factor('mazda', levels=levels(Automob$make)) , num.of.doors=factor('two' , levels=levels(Automob$num.of.doors)) , body.style=factor('hatchback' , levels=levels(Automob$body.style)),engine.type=factor('rotor' , levels=levels(Automob$engine.type)) , fuel.system=factor('4bbl' , levels=levels(Automob$fuel.system))))
s4=predict(forest_bore,data.frame(symboling=factor('3', levels=levels(Automob$symboling)),length=169,curb.weight=2380,wheel.base=95.3,price=15645,horsepower=135,highway.mpg=23,make=factor('mazda', levels=levels(Automob$make)) , num.of.doors=factor('two' , levels=levels(Automob$num.of.doors)) , body.style=factor('hatchback' , levels=levels(Automob$body.style)),engine.type=factor('rotor' , levels=levels(Automob$engine.type)) , fuel.system=factor('mpfi' , levels=levels(Automob$fuel.system))))
Automob[c(56),18]=b1
Automob[c(57),18]=b2
Automob[c(58),18]=b3
Automob[c(59),18]=b4
Automob[c(56),19]=s1
Automob[c(57),19]=s2
Automob[c(58),19]=s3
Automob[c(59),19]=s4



#since horsepower and compression ratio are correlated,using median after grouping to fill horsepower
summary((Automob$horsepower))
medhorsepower <- Automob[!is.na(Automob$horsepower),] %>% group_by(compression.ratio) %>% summarise(med=median(horsepower))
medhorsepower
med=medhorsepower[c(12),2]
which(is.na(Automob$horsepower))
Automob[c(131,132),21]=med

#using median to fill peak.rpm as renault brand only have 2 rows and both have missing peak rpm
summary((Automob$peak.rpm))
Automob[c(131),22]=5200
Automob[c(132),22]=5200

#looking at all rows with NA values
na_rows <- Automob[!complete.cases(Automob), ]
na_rows
colSums(is.na(Automob))





#categorical and numerical variables
categorical_variables=names(Automob)[which(sapply(Automob, is.character))]
categorical_variables






#for better understanding of symboling
#unique(Automob$symboling)
#Automob$symbolinginverse <- NaN
#Automob$symbolinginverse[which(Automob$symboling==3)] <- -2
#Automob$symbolinginverse[which(Automob$symboling==2)] <- -1
#Automob$symbolinginverse[which(Automob$symboling==1)] <- 0
#Automob$symbolinginverse[which(Automob$symboling==0)] <- 1
#Automob$symbolinginverse[which(Automob$symboling==-1)] <- 2
#Automob$symbolinginverse[which(Automob$symboling==-2)] <- 3
#Automob$symbolinginverse <- as.numeric(Automob$symbolinginverse)
#unique(Automob$symbolinginverse)






#assigning numerical variable
numvar=names(Automob)[which(sapply(Automob, is.numeric))]
numvar
auto_continuous=Automob[numvar]

####################################################################################
#correlation matrix
corrplot(cor(auto_continuous), method="circle")
corr_matrix=data.frame(cor(auto_continuous))
round(corr_matrix,3)

# high correlation
for (i in 1:nrow(corr_matrix)){
  correlations <-  which(((corr_matrix[i,] >= 0.8) & (corr_matrix[i,] != 1)))

  if(length(correlations)> 0){
    print(colnames(auto_continuous)[i])
    print(correlations)
  }
}
#GGpair
ggpairs(auto_continuous)





########################################################################################################
Automob
auto_continuous




sample=sample.split(Automob$symboling, SplitRatio=0.8)
train_set=subset(Automob, sample==TRUE)
test_set=subset(Automob, sample==FALSE)
set.seed (1) 




####Random forest on symbol test 1 new
myforest1=randomForest(symboling ~ length+curb.weight+wheel.base+price+curb.weight+horsepower+highway.mpg+make+fuel.type+aspiration+num.of.doors+body.style+drive.wheels+engine.location+engine.type+num.of.cylinders+fuel.system, ntree=1500, data=Automob, importance=TRUE,  na.action = na.omit, do.trace=50)
importance(myforest1)
varImpPlot(myforest1)
myforest1
summary(myforest1)

####Random forest on symbol test 1 new remove fuel type and engine location
myforest1=randomForest(symboling ~ length+curb.weight+wheel.base+price+curb.weight+horsepower+highway.mpg+make+aspiration+num.of.doors+body.style+drive.wheels+engine.type+num.of.cylinders+fuel.system, ntree=1500, data=Automob, importance=TRUE,  na.action = na.omit, do.trace=50)
importance(myforest1)
varImpPlot(myforest1)
myforest1
summary(myforest1)

####Random forest on symbol test 1 new remove fuel type and engine location and aspiration
myforest1=randomForest(symboling ~ length+curb.weight+wheel.base+price+curb.weight+horsepower+highway.mpg+make+num.of.doors+body.style+drive.wheels+engine.type+num.of.cylinders+fuel.system, ntree=1500, data=Automob, importance=TRUE,  na.action = na.omit, do.trace=50)
importance(myforest1)
varImpPlot(myforest1)
myforest1
summary(myforest1)


####Random forest on symbol test 1 new remove fuel type and engine location and aspiration and num of cylinders
myforest1=randomForest(symboling ~ length+curb.weight+wheel.base+price+curb.weight+horsepower+highway.mpg+make+num.of.doors+body.style+drive.wheels+engine.type+fuel.system, ntree=1500, data=Automob, importance=TRUE,  na.action = na.omit, do.trace=50)
importance(myforest1)
varImpPlot(myforest1)
myforest1
summary(myforest1)




#boosted forest on symboling
boosted=gbm(symboling ~ length+curb.weight+wheel.base+price+curb.weight+horsepower+highway.mpg+make+num.of.doors+body.style+drive.wheels+engine.type+fuel.system, data=train_set,distribution= "gaussian",n.trees=10000, interaction.depth=4) 
summary(boosted)
boosted
predicted_score1=predict(boosted, newdata=Automob, n.trees=10000)
mean((as.numeric(predicted_score1) -as.numeric(Automob$symboling))^2)

predicted_score3=predict(boosted, newdata=test_set, n.trees=10000)
mean((as.numeric(predicted_score3) -as.numeric(Automob$symboling))^2)


predicted_score2=predict(boosted, n.trees=10000)
mean((as.numeric(predicted_score2) -as.numeric(Automob$symboling))^2)





#PCA + clustering of continuous variables + Symboling

auto_continuous_pca_sigmoid<-auto_continuous
unique(Automob$symboling)
auto_continuous_pca_sigmoid$symboling <- NaN
auto_continuous_pca_sigmoid$symboling[which(Automob$symboling==3)] <- 3
auto_continuous_pca_sigmoid$symboling[which(Automob$symboling==2)] <- 2
auto_continuous_pca_sigmoid$symboling[which(Automob$symboling==1)] <- 1
auto_continuous_pca_sigmoid$symboling[which(Automob$symboling==0)] <- 0
auto_continuous_pca_sigmoid$symboling[which(Automob$symboling==-1)] <- -1
auto_continuous_pca_sigmoid$symboling[which(Automob$symboling==-2)] <- -2
unique(auto_continuous_pca_sigmoid$symboling)



pca1=prcomp(auto_continuous_pca_sigmoid, scale=TRUE)
pca1
autoplot(pca1, data = auto_continuous_pca_sigmoid, loadings = TRUE,  loadings.label = TRUE )
autoplot(pca1, data = auto_continuous_pca_sigmoid,  loadings = TRUE, col=ifelse(Automob$make=="porsche","blue","red"),  loadings.label = TRUE )
autoplot(pca1, data = auto_continuous_pca_sigmoid, loadings = TRUE,col = ifelse(auto_continuous_pca_sigmoid$symboling>0,"red","green") ,  loadings.label = TRUE )

comp <- data.frame(pca1$x[,1:2])
comp
km <- kmeans(comp, centers = 3)
km
plot(comp[,1], comp[,2], col = km$cluster, pch = 20)
auto_continuous_pca_sigmoid$cluster<-as.factor(km$cluster)

e <- ggplot(auto_continuous_pca_sigmoid, aes(cluster, symboling,color=cluster))
e + geom_violin(trim=FALSE,width=1.4) + geom_boxplot(width=0.1, color="grey", alpha=0.2)


f <- ggplot(auto_continuous_pca_sigmoid, aes(cluster, price,color=cluster))
f + geom_violin(trim=FALSE,width=1.4) + geom_boxplot(width=0.1, color="grey", alpha=0.2)

g <- ggplot(auto_continuous_pca_sigmoid, aes(cluster, stroke,color=cluster))
g + geom_violin(trim=FALSE,width=1.4) + geom_boxplot(width=0.1, color="grey", alpha=0.2)

h <- ggplot(auto_continuous_pca_sigmoid, aes(cluster, bore,color=cluster))
h + geom_violin(trim=FALSE,width=1.4) + geom_boxplot(width=0.1, color="grey", alpha=0.2)

i <- ggplot(auto_continuous_pca_sigmoid, aes(cluster, horsepower,color=cluster))
i + geom_violin(trim=FALSE,width=1.4) + geom_boxplot(width=0.1, color="grey", alpha=0.2)

j <- ggplot(auto_continuous_pca_sigmoid, aes(cluster, peak.rpm,color=cluster))
j + geom_violin(trim=FALSE,width=1.4) + geom_boxplot(width=0.1, color="grey", alpha=0.2)

k <- ggplot(auto_continuous_pca_sigmoid, aes(cluster, city.mpg,color=cluster))
k +geom_violin(trim=FALSE,width=1.4) + geom_boxplot(width=0.1, color="grey", alpha=0.2)

l <- ggplot(auto_continuous_pca_sigmoid, aes(cluster, highway.mpg,color=cluster))
l + geom_violin(trim=FALSE,width=1.4) + geom_boxplot(width=0.1, color="grey", alpha=0.2)

m <- ggplot(auto_continuous_pca_sigmoid, aes(cluster, wheel.base,color=cluster))
m + geom_violin(trim=FALSE,width=1.4) + geom_boxplot(width=0.1, color="grey", alpha=0.2)

n <- ggplot(auto_continuous_pca_sigmoid, aes(cluster, length,color=cluster))
n + geom_violin(trim=FALSE,width=1.4) + geom_boxplot(width=0.1, color="grey", alpha=0.2)

n <- ggplot(auto_continuous_pca_sigmoid, aes(cluster, width,color=cluster))
n + geom_violin(trim=FALSE,width=1.4) + geom_boxplot(width=0.1, color="grey", alpha=0.2)

o <- ggplot(auto_continuous_pca_sigmoid, aes(cluster, height,color=cluster))
o + geom_violin(trim=FALSE,width=1.4) + geom_boxplot(width=0.1, color="grey", alpha=0.2)

p <- ggplot(auto_continuous_pca_sigmoid, aes(cluster, curb.weight,color=cluster))
p + geom_violin(trim=FALSE,width=1.4) + geom_boxplot(width=0.1, color="grey", alpha=0.2)

q <- ggplot(auto_continuous_pca_sigmoid, aes(cluster, engine.size,color=cluster))
q + geom_violin(trim=FALSE,width=1.4) + geom_boxplot(width=0.1, color="grey", alpha=0.2)




#PCA + clustering of continuous variables
auto_continuous_pca<-auto_continuous
pca=prcomp(auto_continuous, scale=TRUE)
pca
autoplot(pca, data = auto_continuous_pca, loadings = TRUE,  loadings.label = TRUE )
autoplot(pca, data = auto_continuous_pca,  loadings = TRUE, col=ifelse(Automob$make=="porsche","blue","red"),  loadings.label = TRUE )
comp <- data.frame(pca$x[,1:2])
comp
km <- kmeans(comp, centers = 3)
km
plot(comp[,1], comp[,2], col = km$cluster, pch = 20)
auto_continuous_pca$cluster<-as.factor(km$cluster)



f <- ggplot(auto_continuous_pca, aes(cluster, price,color=cluster))
f + geom_violin(trim=FALSE) 

g <- ggplot(auto_continuous_pca, aes(cluster, stroke,color=cluster))
g + geom_violin(trim=FALSE)

h <- ggplot(auto_continuous_pca, aes(cluster, bore,color=cluster))
h + geom_violin(trim=FALSE)

i <- ggplot(auto_continuous_pca, aes(cluster, horsepower,color=cluster))
i + geom_violin(trim=FALSE)

j <- ggplot(auto_continuous_pca, aes(cluster, peak.rpm,color=cluster))
j + geom_violin(trim=FALSE)

k <- ggplot(auto_continuous_pca, aes(cluster, city.mpg,color=cluster))
k + geom_violin(trim=FALSE)

l <- ggplot(auto_continuous_pca, aes(cluster, highway.mpg,color=cluster))
l + geom_violin(trim=FALSE)

m <- ggplot(auto_continuous_pca, aes(cluster, wheel.base,color=cluster))
m + geom_violin(trim=FALSE)

n <- ggplot(auto_continuous_pca, aes(cluster, length,color=cluster))
n + geom_violin(trim=FALSE)

n <- ggplot(auto_continuous_pca, aes(cluster, width,color=cluster))
n + geom_violin(trim=FALSE)

o <- ggplot(auto_continuous_pca, aes(cluster, height,color=cluster))
o + geom_violin(trim=FALSE)

p <- ggplot(auto_continuous_pca, aes(cluster, curb.weight,color=cluster))
p + geom_violin(trim=FALSE)

q <- ggplot(auto_continuous_pca, aes(cluster, engine.size,color=cluster))
q + geom_violin(trim=FALSE)



































