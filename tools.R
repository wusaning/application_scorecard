
#################tools kit for application scorecard development

plotVarImp<-function(object,top,...){
  # object=varImp(model_final);top=5
  object_1<-object
  object_idx<-order(object$importance$Overall,decreasing=TRUE)[1:top]
  object_1$importance<-object$importance %>% slice(object_idx)
  rownames(object_1$importance)<-rownames(object$importance)[object_idx]
  varimplot<-ggplot(object_1,...)+
    theme_grey(base_family = "STKaiti")
  varimp_list<-list(varimp=rownames(object_1$importance),
                    varimplot=varimplot)
  return(varimp_list)
}



#add labels for variables
label.add <- function(name,credit.dd=credit.dd,EngName="English Name",ChineName="Chinese Explanation") { 
  if(name %in% credit.dd[,EngName]){
    result<-credit.dd[,ChineName][credit.dd[,EngName]==name]
  } else if(substr(name,nchar(name)-7,nchar(name))=="_bin_woe" & substr(name,1,nchar(name)-8) %in% credit.dd[,EngName]){
    result<-credit.dd[,ChineName][credit.dd[,EngName]==substr(name,1,nchar(name)-8)]
  } else if(substr(name,nchar(name)-3,nchar(name))=="_woe" & substr(name,1,nchar(name)-4) %in% credit.dd[,EngName]){
    result<-credit.dd[,ChineName][credit.dd[,EngName]==substr(name,1,nchar(name)-4)]
  } else if(substr(name,nchar(name)-3,nchar(name))=="_bin" & substr(name,1,nchar(name)-4) %in% credit.dd[,EngName]){
    result<-credit.dd[,ChineName][credit.dd[,EngName]==substr(name,1,nchar(name)-4)] 
  } else {
    result<-""
  }
  result
}


#function for calculating IV value
var.iv <- function(input,target) {
  if(is.numeric(input) & length(unique(input)[!is.na(unique(input))])>10) input=binning(input,bins=10,method="quantile")
  temp1=table(input,target)
  temp2=as.data.frame(matrix(temp1,nrow(temp1),2))
  temp3=sapply(temp2,function(input) input/sum(input))
  if(!is.matrix(temp3)) {iv=0} else
  {
    woe=log(temp3[,1]/temp3[,2]) #good vs bad
    iv=sum((temp3[,1]-temp3[,2])[!is.infinite(woe)]*woe[!is.infinite(woe)])
  }
  return(iv) }

               
#function for exploratory analysis
basic.stats <- function(x) { 
  stats <-list()
  stats$dd<-label(x)
  stats$n <-length(x)
  clean.x<- x[(!is.na(x))&(!is.null(x))&(!(x =="null"))&(!(gsub("([ ])", "", x)==""))]
  stats$nNNAs <- length(clean.x)
  stats$nNAs <- stats$n-stats$nNNAs
  stats$propNAs <-stats$nNAs/stats$n
  if(length(clean.x)==0){
    stats$type <-"No Data"
    stats$mean <- ""
    stats$std <-""
    stats$median <- ""
    stats$min <- ""
    stats$max <- ""
    stats$skew <- ""
    stats$kurt <- ""
    stats$quantile.01 <-""
    stats$quantile.05 <-""
    stats$quantile.10 <-""
    stats$quantile.25 <-""
    stats$quantile.50 <-""
    stats$quantile.75 <-""
    stats$quantile.90 <-""
    stats$quantile.95 <-""
    stats$quantile.99 <-""
    stats$numOfCateg<- ""
    stats$minPOfCateg<-""
    stats$maxPOfCateg<-""
    stats$mode<-""
  } else {
    if(is.factor(x)|is.character(x)){
      stats$type <-"nominal"
      x <- as.factor(x)
      stats$mean <- ""
      stats$std <-""
      stats$median<- ""
      stats$min <- ""
      stats$max <- ""
      stats$skew<-""
      stats$kurt<-""
      stats$quantile.01 <-""
      stats$quantile.05 <-""
      stats$quantile.10 <-""
      stats$quantile.25 <-""
      stats$quantile.50 <-""
      stats$quantile.75 <-""
      stats$quantile.90 <-""
      stats$quantile.95 <-""
      stats$quantile.99 <-""
      stats$numOfCateg<- length(table(clean.x))
      stats$minPOfCateg<-min(table(clean.x))/stats$nNNAs
      stats$maxPOfCateg<-max(table(clean.x))/stats$nNNAs
      stats$mode<-names(which.max(table(clean.x)))
    } else {
      if(is.numeric(x)){
        stats$type <-"value"
        stats$mean <- mean(clean.x)
        stats$std<-sd(clean.x)
        stats$median<- median(clean.x)
        stats$min <- min(clean.x)
        stats$max <- max(clean.x)
        stats$skew<-sum(((clean.x-stats$mean)/stats$std)^3)/length(clean.x)
        stats$kurt<-sum(((clean.x-stats$mean)/stats$std)^4)/length(clean.x)-3
        stats$quantile.01 <-quantile(clean.x,probs = 0.01)[[1]]
        stats$quantile.05 <-quantile(clean.x,probs = 0.05)[[1]]
        stats$quantile.10 <-quantile(clean.x,probs = 0.10)[[1]]
        stats$quantile.25 <-quantile(clean.x,probs = 0.25)[[1]]
        stats$quantile.50 <-quantile(clean.x,probs = 0.50)[[1]]
        stats$quantile.75 <-quantile(clean.x,probs = 0.75)[[1]]
        stats$quantile.90 <-quantile(clean.x,probs = 0.90)[[1]]
        stats$quantile.95 <-quantile(clean.x,probs = 0.95)[[1]]
        stats$quantile.99 <-quantile(clean.x,probs = 0.99)[[1]]
        stats$numOfCateg<- length(table(clean.x))
        stats$minPOfCateg<-min(table(clean.x))/stats$nNNAs
        stats$maxPOfCateg<-max(table(clean.x))/stats$nNNAs
        stats$mode<-names(which.max(table(clean.x)))
      } else {
        stats$type <-"Unknown"
        stats$mean <- ""
        stats$std <-"" 
        stats$median<- ""
        stats$min <- ""
        stats$max <- ""
        stats$skew<-""
        stats$kurt<-""
        stats$quantile.01 <-""
        stats$quantile.05 <-""
        stats$quantile.10 <-""
        stats$quantile.25 <-""
        stats$quantile.50 <-""
        stats$quantile.75 <-""
        stats$quantile.90 <-""
        stats$quantile.95 <-""
        stats$quantile.99 <-""
        stats$numOfCateg<- length(table(clean.x))
        stats$minPOfCateg<-""
        stats$maxPOfCateg<-""
        stats$mode<-""
      } 
    }
  }
  unlist(stats)
}



#converting to scorecard, model required, data with variables before WOE transformation               
score.card <- function(model, data, A, B,y="target") {
  k=1
  variable=vector()
  description=vector()
  condition=vector()
  score=vector()
  overdue.rate=vector()
  n=length(model$coefficients)-1
  score0 = round(A - B * model$coefficients[[1]],0)
  for(name in names(model$coefficients)[-1]){
    for(class in names(table(data[substr(name,1,(nchar(name)-4))]))){
      variable[k]=substr(name,1,(nchar(name)-4))
      description[k] = label(data[substr(name,1,(nchar(name)-4))])
      condition[k]=class
      overdue.rate[k]=length(data[y][data[y]=="1" & data[substr(name,1,(nchar(name)-4))]==class])/length(data[y][data[substr(name,1,(nchar(name)-4))]==class])
      score[k]=round((-1) * B * model$coefficients[name] * data[data[substr(name,1,(nchar(name)-4))]==class,][1,][[name]]+score0/n,0)
      print(class)
      k=k+1
    }
  }
  score.card.result=data.frame(variable,description,condition,overdue.rate,score)
  names(score.card.result)=c("variable_name","Chinese_explanation","property","overdue_rate","score")
  score.card.result
}



#function for calculating the score for each sample
score.value <- function(score.card.value, x) {
  score.card.value=as.data.frame(apply(score.card.value, 2, as.vector),stringsAsFactors = FALSE)
  score.card.value$score=as.numeric(score.card.value$score)
  Score = 0
  for (name in unique(score.card.value$variable)) {
    for (value in score.card.value$property[score.card.value$variable_name==name]) {
      score.temp=0
      if(x[name]==value){
        score.temp=score.card.value$score[score.card.value$variable_name==name & score.card.value$property==value][[1]]
      }
      Score=Score+score.temp 
    }
  }
  Score
}


#function for stability test
stability <- function(train.scoreBin, test.scoreBin,train.target) {
  score = levels(train.scoreBin)
  train.sample=as.vector(table(train.scoreBin))
  test.sample=as.vector(table(test.scoreBin))
  train.sample.prc=train.sample/sum(train.sample)
  test.sample.prc=test.sample/sum(test.sample)
  psi=(train.sample.prc-test.sample.prc)*log( train.sample.prc/test.sample.prc)
  overdue.rate=table(train.scoreBin,train.target)[,2]/(table(train.scoreBin,train.target)[,1]+table(train.scoreBin,train.target)[,2])
  stability.result=data.frame(score,train.sample,test.sample,train.sample.prc,test.sample.prc,psi,overdue.rate)
  names(stability.result)=c("score","num_dev_model","num_test_model","proportion_dev_model","proportion_test_model","stability_PSI","overdue_rate")
  stability.result
}


#计算变量稳定性的函数
#function for calculating stability
feature.stabilityCSI <- function(score.card.value, credit.train.after,credit.test.after) {
  variable=score.card.value$variable_name
  description=score.card.value$Chinese_explanation
  variable_segment=score.card.value$property
  train_cnt=vector()
  test_cnt=vector()
  train_sample=vector()
  test_sample=vector()
  for (k in 1:length(score.card.value$variable_name)) {
    train_cnt[k]=sum(credit.train.after[as.vector(score.card.value$variable_name[k])]==as.vector(score.card.value$property)[k])
    test_cnt[k]=sum(credit.test.after[as.vector(score.card.value$variable_name[k])]==as.vector(score.card.value$property)[k])
    train_sample[k]=sum(credit.train.after[as.vector(score.card.value$variable_name[k])]==as.vector(score.card.value$property)[k])/nrow(credit.train.after)
    test_sample[k]=sum(credit.test.after[as.vector(score.card.value$variable_name[k])]==as.vector(score.card.value$property)[k])/nrow(credit.test.after)
  }
  weight=score.card.value$score
  CSI=(test_sample-train_sample)*weight
  CSI_SUM=vector()
  for (i in 1:length(variable)) {
    CSI_SUM[i]=sum(CSI[variable==variable[i]])
  }
  feature.stability.result=data.frame(variable,description,variable_segment,train_cnt,test_cnt,train_sample,test_sample,weight,CSI,CSI_SUM)
  names(feature.stability.result)=c("variable_name","Chinese_explanation","取值含义","建模样本","验证样本","建模样本比例","验证样本比例","权重","分段CSI","变量CSI")
  feature.stability.result
}

feature.stabilityPSI <- function(score.card.value, credit.train.after,credit.test.after) {
  variable=score.card.value$variable_name
  description=score.card.value$Chinese_explanation
  variable_segment=score.card.value$property
  train_cnt=vector()
  test_cnt=vector()
  train_sample=vector()
  test_sample=vector()
  for (k in 1:length(score.card.value$variable_name)) {
    train_cnt[k]=sum(credit.train.after[as.vector(score.card.value$variable_name[k])]==as.vector(score.card.value$property)[k])
    test_cnt[k]=sum(credit.test.after[as.vector(score.card.value$variable_name[k])]==as.vector(score.card.value$property)[k])
    train_sample[k]=sum(credit.train.after[as.vector(score.card.value$variable_name[k])]==as.vector(score.card.value$property)[k])/nrow(credit.train.after)
    test_sample[k]=sum(credit.test.after[as.vector(score.card.value$variable_name[k])]==as.vector(score.card.value$property)[k])/nrow(credit.test.after)
  }
  weight=0
  PSI=(train_sample-test_sample)*log(train_sample/test_sample)
  PSI_SUM=vector()
  for (i in 1:length(variable)) {
    PSI_SUM[i]=sum(PSI[variable==variable[i]])
  }
  feature.stability.result=data.frame(variable,description,variable_segment,train_cnt,test_cnt,train_sample,test_sample,weight,PSI,PSI_SUM)
  names(feature.stability.result)=c("variable_name","Chinese_explanation","meaning","dev_model_sample","test_model_sample","dev_sample_proportion","test_sample_proportion","weight","subsection_PSI","variable_PSI")
  feature.stability.result
}


#KS and increase rate               
ks.data<-function(n,data.score,data.target){
  rank.score=rank(data.score,ties.method="random") 
  class.score=vector()
  for (i in 1:length(rank.score)) {
    class.score[i]=min(10,floor((rank.score[i]-1)/n)+1)
  }
  temp1=vector();temp2=vector();temp3=vector();temp4=vector();temp5=vector();temp6=vector();temp7=vector();temp8=vector()
  temp9=vector();temp10=vector();temp11=vector();temp12=vector();temp13=vector()
  for (j in 1:10) {
    temp1[j]=round(j*0.1,2)
    temp2[j]=min(data.score[class.score==j])
    temp3[j]=max(data.score[class.score==j])
    temp4[j]=length(data.score[class.score==j])
    temp5[j]=length(data.score[class.score==j & data.target=="1"])
    temp6[j]=length(data.score[class.score==j & data.target=="0"])
    temp7[j]=temp5[j]/(temp5[j]+temp6[j])
    temp8[j]=length(data.score[class.score<=j & data.target=="1"])
    temp9[j]=length(data.score[class.score<=j & data.target=="1"])/length(data.score[data.target=="1"])
    temp10[j]=length(data.score[class.score<=j & data.target=="0"])
    temp11[j]=length(data.score[class.score<=j & data.target=="0"])/length(data.score[data.target=="0"])
    temp12[j]=temp9[j]-temp11[j]
    temp13[j]=temp7[j]/(length(data.score[data.target=="1"])/length(data.score))
  }
  ks.result=data.frame(temp1,temp2,temp3,temp4,temp5,temp6,temp7,temp8,temp9,temp10,temp11,temp12,temp13)
  names(ks.result)=c("score_range","min","max","number","num_overdue","num_other","overdue_rate","accumulate_num_overdue","proportion_of_overdue","accumulate_num_other","proportion_of_other","K-S","increase_rate")
  ks.result
}

ks.data1<-function(cut.score,data.score,data.target){
  cut.score.temp<-cut(data.score,c(-Inf,cut.score[1:9],Inf))
  class.score<-vector()
  for(i in 1:10) { 
    class.score[cut.score.temp==levels(cut.score.temp)[i]]<-i
  }
  tempN=0
  N=length(data.score)
  temp1=vector();temp2=vector();temp3=vector();temp4=vector();temp5=vector();temp6=vector();temp7=vector();temp8=vector()
  temp9=vector();temp10=vector();temp11=vector();temp12=vector();temp13=vector()
  for (j in 1:10) {
    tempN=tempN+length(data.score[class.score==j])   
    temp1[j]=round(tempN/N,2)
    if(j==1){
      temp2[j]=min(data.score[class.score==j])
      temp3[j]=as.numeric(substr(levels(cut.score.temp)[j], 7, 9))
    } else if(j==10){
      temp2[j]=as.numeric(substr(levels(cut.score.temp)[j], 2, 4))+1
      temp3[j]=max(data.score[class.score==j])
    } else {
      temp2[j]=as.numeric(substr(levels(cut.score.temp)[j], 2, 4))+1
      temp3[j]=as.numeric(substr(levels(cut.score.temp)[j], 6, 8))
    }
    temp4[j]=length(data.score[class.score==j])
    temp5[j]=length(data.score[class.score==j & data.target=="1"])
    temp6[j]=length(data.score[class.score==j & data.target=="0"])
    temp7[j]=temp5[j]/(temp5[j]+temp6[j])
    temp8[j]=length(data.score[class.score<=j & data.target=="1"])
    temp9[j]=length(data.score[class.score<=j & data.target=="1"])/length(data.score[data.target=="1"])
    temp10[j]=length(data.score[class.score<=j & data.target=="0"])
    temp11[j]=length(data.score[class.score<=j & data.target=="0"])/length(data.score[data.target=="0"])
    temp12[j]=temp9[j]-temp11[j]
    temp13[j]=temp7[j]/(length(data.score[data.target=="1"])/length(data.score))
    
  }
  ks.result=data.frame(temp1,temp2,temp3,temp4,temp5,temp6,temp7,temp8,temp9,temp10,temp11,temp12,temp13)
  names(ks.result)=c("score_range","min","max","number","num_overdue","num_other","overdue_rate","accumulate_num_overdue","proportion_of_overdue","accumulate_num_other","proportion_of_other","K-S","increase_rate")
  ks.result
}


model.var <- function(model, data,iv.value) {
  temp1=vector();temp2=vector();temp3=vector();temp4=vector()
  for (k in 2:length(model$coefficients)) {
    temp1[k-1]=substr(names(model$coefficients)[k],1,(nchar(names(model$coefficients)[k])-4))
    temp2[k-1]=label(data[temp1[k-1]])
    temp3[k-1]=model$coefficients[k]
    temp4[k-1]=iv.value$InformationValue[iv.value$Variable==temp1[k-1]]
  }
  model.var.result=data.frame(temp1,temp2,temp3,temp4)
  names(model.var.result)=c("model_characteristic_variable","Chinese_explanation","model_coefficient","IV")
  model.var.result
}

#variable
var.result<- function(score.card.value, credit.train.after) {
  variable=score.card.value$variable_name
  description=score.card.value$Chinese_explanation
  variable_segment=score.card.value$property
  train_cnt=vector()
  train_over=vector()
  for (k in 1:length(score.card.value$variable_name)) {
    train_cnt[k]=sum(credit.train.after[as.vector(score.card.value$variable_name[k])]==as.vector(score.card.value$property)[k])
    train_over[k]=sum(credit.train.after[as.vector(score.card.value$variable_name[k])]==as.vector(score.card.value$property)[k] & credit.train.after$target=="1")
  }
  temp=data.frame(variable,description,variable_segment,train_cnt,train_over)
  names(temp)=c("variable_name","variable","meaning","num_customer","num_overdue_customer")
  temp
}


