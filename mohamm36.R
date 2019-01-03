install.packages("stats")
library(stats)
library(devtools)
library(ggbiplot)
library(ggplot2)
library(rpart.plot)
library(glmnet)
library(gam)
library(rpart)
library(randomForest)
library(bartMachine)
library(rJava)
library(earth)
library(plotmo)
library(ModelMetrics)
library(corrplot)
library(gam)
library(mgcv)
library(e1071)
library(wesanderson)
options(java.parameters = "-Xmx2g")
df<-read.csv("./pro-edit.csv")
df1<-df[,-c(17,21,28,30,31,32,33,34,35,36,37)]
df2<-df1[complete.cases(df1),]
a<-cor(df2)
corrplot(a, method ="pie", tl.cex=0.5, cl.ratio=0.2,tl.offset = 0.2)

set.seed(556)
#models--------
library(caret)
folds <- 10
df2$folds <- sample(seq(1:folds),size=nrow(df2),replace=T)
error.df<-data.frame(linearmodelis=numeric(folds),linearmodelos=numeric(folds),lassomodelis=numeric(folds),lassomodelos=numeric(folds),ridgemodelis=numeric(folds),ridgemodelos=numeric(folds),GAMmodelis=numeric(folds),GAMmodelos=numeric(folds),cartmodelis=numeric(folds),cartmodelos=numeric(folds),rfmodelis=numeric(folds),rfmodelos=numeric(folds),marsprunedmodelis=numeric(folds),marsprunedmodelos=numeric(folds),marsunprunedmodelis=numeric(folds),marsunprunedmodelos=numeric(folds),SVMmodelis=numeric(folds),SVMmodelos=numeric(folds),bartmodelis=numeric(folds),bartmodelos=numeric(folds))
for(i in (1:folds)){
  df2.test<-df2[which(df2$folds==i),]
  df2.train <-df2[-which(df2$folds==i),]
  lmodel<-lm(Total.BTU~.,data=df2.train)
  lmodelinsample<-predict(lmodel,df2.train)
  lmodelosample<-predict(lmodel,df2.test)
  error.df$linearmodelis[i]<-rmse(actual =df2.train$Total.BTU,predicted = lmodelinsample)
  error.df$linearmodelos[i]<-rmse(actual=df2.test$Total.BTU,predicted=lmodelosample)
  
  #lasso----
  y.train<-as.matrix(df2.train$Total.BTU)
  x.train<-as.matrix(df2.train[,-c(30)])
  glmdata<-data.matrix(df2.test[,-30])
  glmdata1<-data.matrix(df2.train[,-30])
  lassomod<-glmnet(x.train,y.train, family="gaussian",alpha = 1)
  lassomodinsample<-predict(lassomod,glmdata1)
  lassomodosample<-predict(lassomod,glmdata)
  error.df$lassomodelis[i]<-rmse(actual=df2.train$Total.BTU,predicted = lassomodinsample)
  error.df$lassomodelos[i]<-rmse(actual=df2.test$Total.BTU,predicted = lassomodosample)
  
  #ridge-----
  ridgemod<-glmnet(x.train,y.train, family="gaussian",alpha = 0)
  ridgemodinsample<-predict(ridgemod,glmdata1)
  ridgemodosample<-predict(ridgemod,glmdata)
  error.df$rdigemodelis[i]<-rmse(actual=df2.train$Total.BTU,predicted = ridgemodinsample)
  error.df$ridgemodelos[i]<-rmse(actual=df2.test$Total.BTU,predicted = ridgemodosample)
  
  #gam----
  gammodel<-gam(Total.BTU~ï..PBA+PUBCLIM+SQFT+WLCNS+RFCNS+RFCOOL+GLSSPC+NFLOOR+FLCEILHT+RENHVC+RENELC+RENINS+ONEACT+PBAPLUS+FACIL+NOCC+MONUSE+WKHRS+NWKER+ELHT1+ELHT2+HEATP+FURNAC+MAINHT+WTHTEQ+AMIMETER+CDD65,family=gaussian,df2.train)
  GAMmodelinsample<-predict(gammodel,df2.train)
  GAMmodelosample<-predict(gammodel,df2.test)
  error.df$GAMmodelis[i]<-rmse(actual=df2.train$Total.BTU,predicted = GAMmodelinsample)
  error.df$GAMmodelos[i]<-rmse(actual=df2.test$Total.BTU,predicted = GAMmodelosample)
  
  #cart----  
  cartmodel<-rpart(Total.BTU~.,data=df2.train)
  cartmodelinsample<-predict(cartmodel,df2.train)
  cartmodelosample<-predict(cartmodel,df2.test)
  error.df$cartmodelis[i]<-rmse(actual=df2.train$Total.BTU,predicted = cartmodelinsample)
  error.df$cartmodelos[i]<-rmse(actual=df2.test$Total.BTU,predicted = cartmodelosample)
  
  #rf----
  rfmodel<-randomForest(Total.BTU~.,data=df2.train)
  rfmodelinsample<-predict(rfmodel,df2.train)
  rfmodelosample<-predict(rfmodel,df2.test)
  error.df$rfmodelis[i]<-rmse(actual=df2.train$Total.BTU,predicted = rfmodelinsample)
  error.df$rfmodelos[i]<-rmse(actual=df2.test$Total.BTU,predicted = rfmodelosample)
  
  #marspruned----
  marsmodel<-earth(Total.BTU~.,df2.train)
  marsinsample<-predict(marsmodel,df2.train)
  marsosample<-predict(marsmodel,df2.test)
  error.df$marsprunedmodelis[i]<-rmse(actual=df2.train$Total.BTU,predicted = marsinsample)
  error.df$marsprunedmodelos[i]<-rmse(actual=df2.test$Total.BTU,predicted = marsosample)
  
  #marunpruned---
  marsunprunedmodel<-earth(Total.BTU~.,df2,pmethod ="none")
  marsupinsample<-predict(marsunprunedmodel,df2.train)
  marsuposample<-predict(marsunprunedmodel,df2.test)
  error.df$marsunprunedmodelis[i]<-rmse(actual=df2.train$Total.BTU,predicted = marsupinsample)
  error.df$marsunprunedmodelos[i]<-rmse(actual=df2.test$Total.BTU,predicted = marsuposample)
  
  #SVM-----
  SVMmodel<-svm(Total.BTU~.,data=df2.train, cost=100, gamma=1)
  SVMinsample<-predict(SVMmodel,df2.train)
  SVMosample<-predict(SVMmodel,df2.test)
  error.df$SVMmodelis[i]<-rmse(actual=df2.train$Total.BTU,predicted = SVMinsample)
  error.df$SVMmodelos[i]<-rmse(actual=df2.test$Total.BTU,predicted = SVMosample)
  
  
  #bartmachine------
  df.response<-(df2.train$Total.BTU)
  df.covariate<-data.frame(df2.train[,-c(30)])
  bmdata<-data.frame(df2.test[,-c(30)])
  bmdata1<-data.frame(df2.train[,-c(30)])
  bartmodel<-bartMachine(df.covariate,df.response)
  bminsample<-predict(bartmodel,bmdata1)
  bmosample<-predict(bartmodel,bmdata)
  error.df$bartmodelis[i]<-rmse(actual=df2.train$Total.BTU,predicted = bminsample)
  error.df$bartmodelos[i]<-rmse(actual=df2.test$Total.BTU,predicted = bmosample)
}

##exploratory analysis plots------
b<-ggplot(df2,aes(df2$Total.BTU,df2$SQFT))
b+geom_violin(fill='yellow', color="Blue")

c<-ggplot(df2,aes(df2$Total.BTU,df2$ï..PBA))
c+geom_violin(fill='yellow', color="Blue")

theme_set(theme_classic())
t<-ggplot(df2,aes(df2$ï..PBA,df2$Total.BTU),fill=factor(df2$ï..PBA))
t+geom_bar(stat='identity',aes(fill=Total.BTU), width=0.5)+theme(axis.text.x = element_text(angle=65, vjust=0.6))

theme_set(theme_classic())
e<-ggplot(df2,aes(df2$Total.BTU,df2$SQFT))
e+geom_bar(stat='identity',aes(fill=SQFT), width=50)

theme_set(theme_classic())
z<-ggplot(df2,aes(df2$RENELC,df2$Total.BTU))
z+geom_bar(stat='identity',aes(fill=Total.BTU), width=0.5)

theme_set(theme_classic())
y<-ggplot(df2,aes(df2$PUBCLIM,df2$Total.BTU))
y+geom_bar(stat='identity',aes(fill=Total.BTU), width=0.5)

theme_set(theme_classic())
w<-ggplot(df2,aes(df2$PUBCLIM,df2$Total.BTU))
w+geom_bar(stat='identity',aes(fill=Total.BTU), width=0.1)
theme_set(theme_classic())
g<-ggplot(df2,aes(df2$CDD65,df2$Total.BTU),fill=factor(df2$CDD65))
g+geom_bar(stat='identity',aes(fill=Total.BTU), width=5)+theme(axis.text.x = element_text(angle=65, vjust=0.6))


pd_plot(bartmodel,j="SQFT")
pd_plot(bartmodel,j="WLCNS")
pd_plot(bartmodel,j="NWKER")
pd_plot(bartmodel,j="NFLOOR")
pd_plot(bartmodel,j="NOCC")
pd_plot(bartmodel,j="RFCNS")
pd_plot(bartmodel,j="MAINHT")
pd_plot(bartmodel,j="FLCEILHT")
pd_plot(bartmodel,j="RENHVC")
pd_plot(bartmodel,j="ï..PBA")
pd_plot(bartmodel,j="PUBCLIM")
pd_plot(bartmodel,j="RFCOOL")
pd_plot(bartmodel,j="GLSSPC")
pd_plot(bartmodel,j="RENELC")
pd_plot(bartmodel,j="RENINS")
pd_plot(bartmodel,j="PBAPLUS")
pd_plot(bartmodel,j="FACIL")
pd_plot(bartmodel,j="MONUSE")
pd_plot(bartmodel,j="WKHRS")
pd_plot(bartmodel,j="ELHT1")
pd_plot(bartmodel,j="ELHT2")
pd_plot(bartmodel,j="HEATP")
pd_plot(bartmodel,j="FURNAC")
pd_plot(bartmodel,j="MAINHT")
pd_plot(bartmodel,j="WTHTEQ")
pd_plot(bartmodel,j="AMIMETER")
pd_plot(bartmodel,j="CDD65")
investigate_var_importance(bartmodel, type = "splits",plot = TRUE, num_replicates_for_avg = 5, num_trees_bottleneck = 20,num_var_plot = Inf, bottom_margin = 10) 
plot_convergence_diagnostics(bartmodel,plots = c("mh_acceptance"))
plot_convergence_diagnostics(bartmodel,plots = c("sigsqs"))
plot_convergence_diagnostics(bartmodel,plots = c( "num_nodes"))
plot_convergence_diagnostics(bartmodel,plots = c("tree_depths"))
plot_y_vs_yhat(bartmodel, Xtest = NULL, ytest = NULL,credible_intervals = FALSE, prediction_intervals = FALSE,interval_confidence_level = 0.95)

finalmodel<-bartMachine(df.covariate,df.response)
save(finalmodel,file="mohamm36.RData")


