########################################################Logistic regression application scorecard######################################################
rm(list = ls())
path <- "F:/Rworking/scoreCard";setwd(path);rm(path)
#load r packages
library(RODBC);library(Hmisc);library(devtools); library(woe) ;library(rattle) #second one for labeling; third and fourth for calculating IV,WOE and WOE transformation; fifth for binning
library(foreach);library(doParallel);library(iterators);library(rpart);library(caret)  #first three for parelleling calculating， following one for splitting
library(plyr);library(party);library(ROCR);library(car) #party package for missing value，random forrest；ROCR for model evaluation ; car for colinearity VIF
library(gbm) ;library(data.table);library(plyr);library(dplyr) ;library(ggplot2);library(ggthemes); library(plotly) ;library(plotmo) ;library(dismo);library(glmnet)
source("tools.R",encoding="utf-8")   #needed tools

###################################################### I. Data preparation and analysis  #######################################################

##### 1、Loading data
credit1_1<-read.csv("DataJSD/jsd0418_1.csv",stringsAsFactors = TRUE)
credit1_2<-read.csv("DataJSD/jsd0418_2.csv",stringsAsFactors = TRUE)

credit1<-merge(credit1_1,credit1_2,by.x = "apply_id_",by.y = "apply_id_",all.x = TRUE)

credit2<-read.csv("DataJSD/jsd0418applyid_2.csv",stringsAsFactors = TRUE)

credit<-merge(credit1,credit2,by.x = "apply_id_",by.y = "apply_id_",all.x = TRUE)
credit.bx<-credit #为后续调整时用
# 存储数据集csv文件
write.csv(credit,file = "DataJSD/credit.csv",row.names = FALSE ) #保存数据集

# 目标变量标准化（为了后续处理方便，统一）
credit$target[credit$y==1] <- 1;   credit$target[credit$y==0] <- 0; credit$target[credit$y==2] <- 2;   #建模样本
credit$target<-factor(credit$target,levels=c(0,1,2));

# 读取数据字典,添加标签
credit.dd<-read.csv("Data/creditDD.csv",stringsAsFactors = FALSE) #变量标签表
#暂停for (i in 1:length(credit)) {label(credit[,i])<-label.add(names(credit)[i],credit.dd=credit.dd,EngName="英文字段",ChineName="中文解释")}

##### 2、数据排除与抽样（目前不需要进行数据抽样，步骤省略）TotalSample 由于具体业务的情况不一样，有些数据分析内容有特殊性，根据实际情况交互进行分析

#详单缺失情况
statisJXLMiss <-as.data.frame(rbind(
  c("建模样本",nrow(credit),nrow(credit[is.na(credit$org_data_id),]),paste(round(nrow(credit[is.na(credit$org_data_id),])/nrow(credit)*100,2),"%",sep = ""))
) , stringsAsFactors = FALSE)
names(statisJXLMiss)<-c("情况","总样本数","没详单样本数","没详单样本数占比")
statisCustomerDev<-as.data.frame(rbind(c("建模样本情况",paste(table(credit$target), paste(round(prop.table(table(credit$target))*100,2),"%)",sep=""),sep=" ("),nrow(credit))
), stringsAsFactors = FALSE)
names(statisCustomerDev)<-c("情况","好客户(占比)","坏客户(占比)","中间客户(占比)","样本总数");

write.csv(statisJXLMiss,file = "DataOutputJSD/statisJXLMiss.csv",row.names = FALSE )
write.csv(statisCustomerDev,file = "DataOutputJSD/statisCustomerDev.csv",row.names = FALSE )

#无样本排除

##### 3、初步变量衍生(补充为主)和变量初筛

credit$target <- as.factor(as.vector(credit$target)); 
#credit.veri$target <- as.factor(as.vector(credit.veri$target));  #无验证样本
basic.stats.result <- as.data.frame(t(sapply(credit, basic.stats)),stringsAsFactors=FALSE);basic.stats.result<-cbind(basic.stats.result,name=row.names(basic.stats.result))
basic.stats.result<- merge(credit.dd,basic.stats.result,by.x ="英文字段",by.y = "name",all.y = TRUE)
write.csv(basic.stats.result,file = "DataOutputJSD/basic.stats.result.csv",row.names = FALSE )

##### 1、错误数据修正

##### 2、单变量分析
basic.stats.result <- as.data.frame(t(sapply(credit, basic.stats)),stringsAsFactors=FALSE);basic.stats.result<-cbind(basic.stats.result,name=row.names(basic.stats.result))
basic.stats.result<- merge(credit.dd,basic.stats.result,by.x ="英文字段",by.y = "name",all.y = TRUE)

### 变量的分布
for (i in 1:length(credit[-which(names(credit) %in% "target")])) {
  if(is.character(credit[-which(names(credit) %in% "target")][,i]) | is.factor(credit[-which(names(credit) %in% "target")][,i])){
    barplot(prop.table(table(credit[-which(names(credit) %in% "target")][,i],useNA = "ifany")),xlab = '',
            main = paste(credit.dd$中文解释[credit.dd$英文字段==names(credit)[-which(names(credit) %in% "target")][i]],credit.dd$中文解释[credit.dd$英文字段==names(credit)[-which(names(credit) %in% "target")][i]],names(credit)[-which(names(credit) %in% "target")][i],sep=""))
  } else if(is.numeric(credit[-which(names(credit) %in% "target")][,i])){
    hist(credit[-which(names(credit) %in% "target")][,i],prob=T,xlab = '',
         main = paste(credit.dd$中文解释[credit.dd$英文字段==names(credit)[-which(names(credit) %in% "target")][i]],credit.dd$中文解释[credit.dd$英文字段==names(credit)[-which(names(credit) %in% "target")][i]],names(credit)[-which(names(credit) %in% "target")][i],sep=""))
    lines(density(credit[-which(names(credit) %in% "target")][,i],na.rm=T))
  } else {
    barplot(prop.table(table(credit[-which(names(credit) %in% "target")][,i],useNA = "ifany")),xlab = '',
            main = paste(credit.dd$中文解释[credit.dd$英文字段==names(credit)[-which(names(credit) %in% "target")][i]],credit.dd$中文解释[credit.dd$英文字段==names(credit)[-which(names(credit) %in% "target")][i]],names(credit)[-which(names(credit) %in% "target")][i],sep=""))
  }
}

# 异常值检查
name.num<-basic.stats.result$英文字段[which(basic.stats.result$type == "数值")]
for(name in name.num){
  boxplot(credit[,name],main = paste(c('Box of ',name),collapse = ""))
  hist(credit[,name],main = name)
}

#偏态分布，变换后再查看下分箱结果
boxplot(log(credit$xd_out_subtotal))
hist(log(credit$xd_out_subtotal))

# 初步变量衍生或变量数据类型不对进行转换
#举例 credit<- transform(creidt,newVar = 计算公式 ,Date=as.Date(Date))这里只是举例，实际以真实数据为准
#credit.dd<-rbind(credit.dd,c("newVar","newVar的中文解释"))

# 初步筛选1：去掉非申请时变量(例如贷后)和明确不参与建模的变量,例如申请id之类的
names.remove.cb=c("home_address","org_data_id","organization_address", "organization_name","organization_telephone","xd_call_day1_pct",
                  "ph_talk_obj_per_6_12","county","city","y","X") 
#names.remove.cb不能为空并且names(credit) %in% names.remove.cb 不能为False,所以加if条件
if(length(names(credit)[names(credit) %in% names.remove.cb])>0){credit<-credit[-which(names(credit) %in% names.remove.cb)]}
#记录特征变量统计数据
explainTotalFeatureVar <- c("特征变量总数",NA,length(credit)-2,NA) #用于记录特征变量统计数据

# 初步筛选2：去掉缺失比例大于0.9的变量
names.remove.miss <- intersect(basic.stats.result$英文字段[as.numeric(basic.stats.result$propNAs)>0.9],names(credit)[-which(names(credit) %in% c("target","apply_id_"))])
if(length(names(credit)[names(credit) %in% names.remove.miss])>0){credit<-credit[-which(names(credit) %in% names.remove.miss)]}
explainMissFeatureVar<-c(c("缺失值占比大于0.9",length(names.remove.miss),length(credit)-2,paste(names.remove.miss,collapse = " | ")))


# 暂时不用，初步筛选3：去掉单一类别比例大于0.9的变量
#names.remove.categ <- intersect(basic.stats.result$英文字段[as.numeric(basic.stats.result$maxPOfCateg)>0.9],names(credit)[-which(names(credit) %in% c("target","apply_id_"))])
#if(length(names(credit)[names(credit) %in% names.remove.categ])>0){credit<-credit[-which(names(credit) %in% names.remove.categ)]}
#explainCategFeatureVar<-c(c("单一类别占比大于0.9",length(names.remove.categ),length(credit)-1,paste(names.remove.miss,collapse = " | ")))

######################################################################II. Variable selection######################################################

# 初步筛选4：通过IV，随机森林和GBDT筛选变量
# 初步查看IV值
credit$education_level <- as.factor(as.vector(credit$education_level))
credit$industry <- as.factor(as.vector(credit$industry))
credit$province <- as.factor(as.vector(credit$province))
iv.value.cb <- sapply(credit[-which(names(credit) %in% c("target","apply_id_"))],var.iv,target=credit$target)  #计算变量的IV值
iv.value.cb<-merge(cbind(as.data.frame(iv.value.cb),name=row.names(as.data.frame(iv.value.cb))),credit.dd,by.x = "name",by.y = "英文字段",all.x = TRUE) 
write.csv(iv.value.cb,file = "DataOutputJSD/iv.value.cb.csv",row.names = FALSE ) #保存数据集

names.remove.iv.cb<-as.vector(iv.value.cb[iv.value.cb$iv.value.cb<0.02,]$name)

#用woe包计算iv值
iv.value.woe<-iv.mult(credit[-which(names(credit) %in% "apply_id_")],"target",TRUE) 
iv.value.woe<-merge(iv.value.woe,credit.dd,by.x = "Variable",by.y = "英文字段",all.x = TRUE)
credit$target<-as.factor(credit$target)
write.csv(iv.value.woe,file = "DataOutputJSD/iv.value.woe.csv",row.names = FALSE ) #保存数据集

names.remove.iv.woe<-as.vector(iv.value.woe[iv.value.woe$InformationValue<0.02,]$Variable)
names.remove.iv<-intersect(names.remove.iv.cb,names.remove.iv.woe)
# IV的并行版本，上面运行速度可以，暂时先不使用，
#cl <- makeCluster(4);registerDoParallel(cl)
#iv.value.cb1 <- foreach(x=iapply(credit[-which(names(credit) %in% "target")], MARGIN=2),.combine='rbind',.packages='rattle') %dopar% var.iv(x,credit$target)
#row.names(iv.value.cb1) <- names(credit[-which(names(credit) %in% "target")])
#stopCluster(cl) #关闭集群
#names.remove.iv=names(which(iv.value<0.030))                  #去掉IV值小于0.03的变量，暂时不按IV值选择，先按排序前50名选取
#names(sort(iv.value.cb,decreasing = T)[1:50])  #IV值从大到小，取前50个


# 随机森林筛选，和IV并行筛选的
set.seed(42)  
#cforest能处理缺失值。randomForest()函数不能处理缺失值，使得之前填补这些缺失值，并且每个分类属性的最大数量不能超过32个，如果属性超过32个，使用之前那些属性必须被转化。
system.time(crf<-cforest(target ~ .,control = cforest_unbiased(mtry = 2, ntree = 1000), data=credit[-which(names(credit) %in% "apply_id_")])  )
system.time(varimpt<-data.frame(varimp(crf))) #代表重要性函数，计算开销太大
varimpt<-merge(cbind(varimpt,name=row.names(varimpt)),credit.dd,by.x = "name",by.y = "英文字段",all.x = TRUE) 
names.remove.randomForest <-as.vector(varimpt[order(varimpt$varimp.crf.),]$name[1:(nrow(varimpt)-50)])
write.csv(varimpt,file = "DataOutputJSD/varimpt.csv",row.names = FALSE ) #保存数据集
save(crf,varimpt,file="crfJSD.RData")
rm(crf)
names.remove.iv.randomForest<-intersect(names.remove.iv,names.remove.randomForest)

# GBDT筛选，先行使用，后续再深入研究
Y<-factor(credit$target,levels = c("1","0"),labels = c("逾期","正常"))
ctrl <- trainControl(method = "none",summaryFunction = twoClassSummary,classProbs = TRUE,allowParallel=TRUE,sampling="smote",savePredictions = TRUE) #caret包中
gbmGrid <- expand.grid(n.trees = 1000,interaction.depth = 2,shrinkage = 0.01,n.minobsinnode = 20)
set.seed(476)
credit_gbmFit_2_2 <- train(x=credit[-which(names(credit) %in% c("target","apply_id_"))],y=Y,method = "gbm",tuneGrid = gbmGrid,metric = "ROC",verbose = FALSE,trControl = ctrl)

crd_varimp<-plotVarImp(varImp(credit_gbmFit_2_2),top = 256)
crd_varimp_top50<-plotVarImp(varImp(credit_gbmFit_2_2),top = 50)
crd_varimp_top50$varimp
crd_varimp_top50$varimplot%>%ggplotly()

names.remove.crd <- names(credit)[-which(names(credit) %in% c(crd_varimp_top50$varimp,"target","apply_id_"))]
write.csv(crd_varimp$varimp,file = "DataOutputJSD/crd_varimp.csv",row.names = FALSE ) #保存数据集
write.csv(crd_varimp_top50$varimp,file = "DataOutputJSD/crd_varimp_top50.csv",row.names = FALSE ) #保存数据集

save(credit_gbmFit_2_2,crd_varimp,file="GDBTJSD.RData")
rm(credit_gbmFit_2_2)
names.remove.iv.randomForest.crd<-intersect(names.remove.iv.randomForest,names.remove.crd )

# 其他筛选的方法，例如单变量K-S后续添加

#取所有筛选变量方法筛选掉的变量的并集作为最终筛选掉的变量集合
names.remove.result <-intersect(names.remove.iv.randomForest.crd,names(credit)[-which(names(credit) %in% c("target","apply_id_"))])
if(length(names(credit)[names(credit) %in% names.remove.result])>0){credit<-credit[-which(names(credit) %in% names.remove.result)]}
explainSelectFeatureVar<-c(c("IV|随机森林|GBDT",length(names.remove.result),length(credit)-2,paste(names.remove.result,collapse = " | ")))

# 初步筛选5：相关性
credit.cor<-credit[-which(names(credit) %in% c("target","education_level","sex","industry","crawler_time_categ","corp","cos_sour_chan","province","apply_id_"))]
cor.value.cb<-cor(credit.cor,use="pairwise.complete.obs")
write.csv(cor.value.cb,file = "DataOutputJSD/cor.value.cb.csv") 
#上面的不方便筛选,两两配对计算
cor.value.cb.sx<-data.frame(); k<-1
for (i in 1:(length(credit.cor)-1)) {
  for (j in (i+1):length(credit.cor)) {
    cor.value.cb.sx[k,1]<-paste(names(credit.cor)[i],names(credit.cor)[j],sep="|")
    cor.value.cb.sx[k,2]<-cor(credit.cor[,i],credit.cor[,j],use="pairwise.complete.obs")
    k<-k+1
  }
}
write.csv(cor.value.cb.sx,file = "DataOutputJSD/cor.value.cb.sx1.csv",row.names = FALSE) 
#去掉相关性变量
names.remove.cor<-c(
  "xd_call_cnt",
  "d150_d0_nocall_days",
  "xd_call_in_cnt_pct",
  "cont_call_out_cnt_pct",
  "contact_phone_cnt",
  "contact_1m",
  "outall_ratio_allcall",
  "ph_talk_freq_per_0_6_zj",
  "call_out_cnt_pct_mth5",
  "d31_180_call_contacts_count",
  "d180_d3_nocall_times",
  "xd_call_day0_pct",
  "xd_call_in_day1_pct",
  "ph_day_fir_now",
  "d90_call_contacts_count",
  "d90_each_other_call_count",
  "contact_1w_1m_avg",
  "d31_90_call_contacts_count",
  "d31_180_each_other_call_count",
  "d31_90_each_other_call_count",
  "contact_1w_1m",
  "d120_d3_nocall_times",
  "ph_talk_time_coincide_work_rest",
  "contact_3m",
  "d120_d0_nocall_days",
  "xd_call_out_cnt_pct",
  "d7_d0_nocall_days",
  "avgm_call_out_time_mth5",
  "xd_call_out_day1_pct",
  "ph_called_freq_per",
  "avgm_call_out_cnt_mth5",
  "xd_in_subtotal",
  "d150_d7_nocall_times",
  "xd_call_len_ave_d",
  "d120_d6_nocall_times",
  "d150_max_nocall_days",
  "holiday_avg",
  "d120_d4_nocall_times",
  "d180_each_other_call_count",
  "d91_180_each_other_call_count",
  "avg_call_len",
  "d180_d6_nocall_times",
  "call_out_len_avg",
  "contact_1m_3m",
  "d150_d6_nocall_times",
  "contact_1m_3m_avg",
  "xd_first_time",
  "d150_d4_nocall_times",
  "d30_each_other_call_count",
  "contact_3m_avg",
  "d91_180_call_contacts_count",
  "xd_call_in_day0_pct",
  "avgm_call_cnt_mth3",
  "avgm_call_out_cnt_mth3",
  "d7_d3_nocall_times",
  "call_out_cnt_std_mth5",
  "call_out_cnt_avg",
  "d30_call_contacts_count",
  "d180_d4_nocall_times",
  "d7_max_nocall_days",
  "call_in_cnt_avg",
  "ph_talk_time_last7",
  "ph_day_contact",
  "call_in_cnt_mth1",
  "call_out_cnt_mth1",
  "d150_d5_nocall_times",
  "mobile_cnt_3m",
  "xd_call_cnt_ave_d",
  "call_cnt_std_mth3",
  "call_in_cnt_std_mth5",
  "call_out_cnt_pct_mth3",
  "call_out_time_std_mth5",
  "d60_d3_nocall_times",
  "avgm_call_time_mth3",
  "d180_call_total_times",
  "ph_cont_obj_effi",
  "ph_calltime_last7_28",
  "d180_d5_nocall_times",
  "call_in_len_avg",
  "d90_d3_nocall_times",
  "ph_mean_talk_time_bey",
  "contacts1_m6_call_min_count",
  "ph_talk_time_per_6_12_cont1",
  "ph_mean_talk_time_3mons_cont1",
  "call_in_time_std_mth3",
  "ph_talk_time_per_12_18_cont1",
  "call_out_cnt_pct_mth1",
  "ph_talk_time_per_6_12",
  "ph_talk_freq_per_12_18_cont2",
  "d90_max_nocall_days",
  "avgm_call_in_cnt_mth3",
  "d60_d6_nocall_times",
  "call_cnt_mth1",
  "night_avg",
  "xd_call_cnt_1m",
  "xd_mobile_cnt_ave_d",
  "ph_talk_time_per_0_6",
  "ph_talk_obj_per_0_6_zj",
  "ph_talk_freq_per_12_18_cont1",
  "call_in_time_std_mth5",
  "ph_talk_time_per_6_12_cont2",
  "call_out_time_pct_mth3",
  "xd_mobile_in_cnt_pct",
  "ph_talk_time_per_18_24",
  "xd_call_out_day0_pct",
  "call_out_time_std_mth3",
  "d30_d5_nocall_times",
  "xd_call_out_cnt_ave_d",
  "ph_mean_talk_freq_bey",
  "call_out_cnt_std_mth3",
  "ph_talk_freq_per_18_24",
  "d60_max_nocall_days",
  "xd_call_out_cnt",
  "contact2_mobile_notin_m6_call_re",
  "ph_talk_freq_per_18_24_cont2"
)

if(length(names(credit)[names(credit) %in% names.remove.cor])>0){credit<-credit[-which(names(credit) %in% names.remove.cor)]}
explainCorFeatureVar<-c(c("相关性筛选",length(names.remove.cor),length(credit)-2,paste(names.remove.cor,collapse = " | ")))
#参考删除相关性的共线性情况
credit.cor1<-credit[-which(names(credit) %in% c("target","education_level","sex","industry","crawler_time_categ","corp","cos_sour_chan","province","apply_id_"))]
cor.value.cb1<-cor(credit.cor1,use="pairwise.complete.obs")
kappa(cor.value.cb1, exact=TRUE) 

####汇总变量统计数据
statistFeatureVar <- as.data.frame(rbind(explainTotalFeatureVar,explainMissFeatureVar,explainSelectFeatureVar,explainCorFeatureVar))
names(statistFeatureVar)<-c("变量排除条件","排除特征变量数","剩余特征变量数","排除的变量")
write.csv(statistFeatureVar,file = "DataOutputJSD/statistFeatureVar.csv",row.names = FALSE) 

#####################################################III. Variable transformation ###################################################


# 这次样本量较少，不进行拆分。数据集拆分（分层随机抽样）拆分训练集和测试集
set.seed(123456)
in_train=createDataPartition(credit$target, p = 0.7, list = FALSE)
credit.train = credit[in_train, ]
credit.test = credit[-in_train, ]
write.csv(credit.train,file = "DataOutputJSD/credit.train.csv",row.names = FALSE) 
write.csv(credit.test,file = "DataOutputJSD/credit.test.csv",row.names = FALSE) 
credit.train<-read.csv("DataOutputJSD/credit.train.csv")
credit.test<-read.csv("DataOutputJSD/credit.test.csv")

##### 2、变量变换
#下面的的函数是用来给数值变量分箱用的，cp值越小，类别越多
#iv.num(german_data,"duration","gb",rcontrol=rpart.control(cp=.02))
#iv.num(german_data,"duration","gb",rcontrol=rpart.control(cp=.005))
#iv.num(german_data,"duration","gb",rcontrol=rpart.control(cp=.001))

credit.train$target <-as.factor(credit.train$target);credit.test$target <-as.factor(credit.test$target)

basic.stats.result.train <- as.data.frame(t(sapply(credit.train, basic.stats)),stringsAsFactors=FALSE);basic.stats.result.train<-cbind(basic.stats.result.train,name=row.names(basic.stats.result.train))
basic.stats.result.train <- merge(credit.dd,basic.stats.result.train,by.x ="英文字段",by.y = "name",all.y = TRUE)

#数值变量离散化
basic.stats.result.train$英文字段[which(basic.stats.result.train$type=="数值")]

#avg_call_in_len  c(-Inf,80,120,Inf )  c(-Inf,50,80,Inf )
summary(credit.train$avg_call_in_len);
iv.num(credit.train,"avg_call_in_len","target",rcontrol=rpart.control(cp=.0016)) #avg_call_in_len
credit.train$avg_call_in_len_bin=cut(credit.train$avg_call_in_len,c(-Inf,50,80,Inf ))
credit.test$avg_call_in_len_bin=cut(credit.test$avg_call_in_len,c(-Inf,50,80,Inf ))
table(credit.train$avg_call_in_len_bin)/nrow(credit.train);table(credit.test$avg_call_in_len_bin)/nrow(credit.test)
iv.mult(credit.train,"target",vars=c("avg_call_in_len_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("avg_call_in_len_bin"),summary=FALSE))
iv.mult(credit.test,"target",vars=c("avg_call_in_len_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("avg_call_in_len_bin"),summary=FALSE))


#avg_call_out_len c(-Inf,70,150,Inf ) c(-Inf,70,140,Inf )
summary(credit.train$avg_call_out_len);#c(-Inf,70,150,Inf ) c(-Inf,70,140,Inf )
iv.num(credit.train,"avg_call_out_len","target",rcontrol=rpart.control(cp=.0016)) #avg_call_out_len
credit.train$avg_call_out_len_bin=cut(credit.train$avg_call_out_len,c(-Inf,60,100,140,Inf ))
credit.test$avg_call_out_len_bin=cut(credit.test$avg_call_out_len,c(-Inf,60,100,140,Inf ))
table(credit.train$avg_call_out_len_bin)/nrow(credit.train);table(credit.test$avg_call_out_len_bin)/nrow(credit.test)
iv.mult(credit.train,"target",vars=c("avg_call_out_len_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("avg_call_out_len_bin"),summary=FALSE))
iv.mult(credit.test,"target",vars=c("avg_call_out_len_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("avg_call_out_len_bin"),summary=FALSE))

#call_cnt_avg 
summary(credit.train$call_cnt_avg);#c(-Inf,4,5.5,6.5,8,Inf ) 之前的分割c(-Inf,2,6,8,Inf )
iv.num(credit.train,"call_cnt_avg","target",rcontrol=rpart.control(cp=.0016)) #call_cnt_avg
credit.train$call_cnt_avg_bin=as.vector(cut(credit.train$call_cnt_avg,c(-Inf,2,6,8,Inf )))
credit.test$call_cnt_avg_bin=as.vector(cut(credit.test$call_cnt_avg,c(-Inf,2,6,8,Inf )))
credit.train$call_cnt_avg_bin[credit.train$call_cnt_avg_bin %in% c("(-Inf,2]","(8, Inf]")]=paste(c("(-Inf,2]","(8, Inf]"),collapse = "|")
credit.test$call_cnt_avg_bin[credit.test$call_cnt_avg_bin %in% c("(-Inf,2]","(8, Inf]")]=paste(c("(-Inf,2]","(8, Inf]"),collapse = "|")
table(credit.train$call_cnt_avg_bin)/nrow(credit.train);table(credit.test$call_cnt_avg_bin)/nrow(credit.test)
iv.mult(credit.train,"target",vars=c("call_cnt_avg_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("call_cnt_avg_bin"),summary=FALSE))
iv.mult(credit.test,"target",vars=c("call_cnt_avg_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("call_cnt_avg_bin"),summary=FALSE))

#call_cnt_std_mth5
summary(credit.train$call_cnt_std_mth5) #c(-Inf,20,40,55,70,Inf )之前的分割
iv.num(credit.train,"call_cnt_std_mth5","target",rcontrol=rpart.control(cp=.0010)) #call_cnt_std_mth5
credit.train$call_cnt_std_mth5_bin=as.vector(cut(credit.train$call_cnt_std_mth5,c(-Inf,20,70,Inf )))
credit.train$call_cnt_std_mth5_bin[is.na(credit.train$call_cnt_std_mth5)] = "缺失"
credit.train$call_cnt_std_mth5_bin[credit.train$call_cnt_std_mth5_bin %in% c("(70, Inf]","缺失")]=paste(c("(70, Inf]","缺失"),collapse = "|")
credit.train$call_cnt_std_mth5_bin<-factor(credit.train$call_cnt_std_mth5_bin,levels = c("(-Inf,20]","(20,70]","(70, Inf]|缺失"))
table(credit.train$call_cnt_std_mth5_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("call_cnt_std_mth5_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("call_cnt_std_mth5_bin"),summary=FALSE))

credit.test$call_cnt_std_mth5_bin=as.vector(cut(credit.test$call_cnt_std_mth5,c(-Inf,20,70,Inf )))
credit.test$call_cnt_std_mth5_bin[is.na(credit.test$call_cnt_std_mth5)] = "缺失"
credit.test$call_cnt_std_mth5_bin[credit.test$call_cnt_std_mth5_bin %in% c("(70, Inf]","缺失")]=paste(c("(70, Inf]","缺失"),collapse = "|")
credit.test$call_cnt_std_mth5_bin<-factor(credit.test$call_cnt_std_mth5_bin,levels = c("(-Inf,20]","(20,70]","(70, Inf]|缺失"))
table(credit.test$call_cnt_std_mth5_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("call_cnt_std_mth5_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("call_cnt_std_mth5_bin"),summary=FALSE))

#call_in_cnt_std_mth3
summary(credit.train$call_in_cnt_std_mth3) 
iv.num(credit.train,"call_in_cnt_std_mth3","target",rcontrol=rpart.control(cp=.0012)) #call_in_cnt_std_mth3
credit.train$call_in_cnt_std_mth3_bin=as.vector(cut(credit.train$call_in_cnt_std_mth3,c(-Inf,7,20,30,Inf )))
credit.train$call_in_cnt_std_mth3_bin[is.na(credit.train$call_in_cnt_std_mth3)] = "缺失"
credit.train$call_in_cnt_std_mth3_bin[credit.train$call_in_cnt_std_mth3_bin %in% c("(30, Inf]","缺失")]=paste(c("(30, Inf]","缺失"),collapse = "|")
credit.train$call_in_cnt_std_mth3_bin<-factor(credit.train$call_in_cnt_std_mth3_bin,levels = c("(-Inf,7]","(7,20]","(20,30]","(30, Inf]|缺失"))
table(credit.train$call_in_cnt_std_mth3_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("call_in_cnt_std_mth3_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("call_in_cnt_std_mth3_bin"),summary=FALSE))

credit.test$call_in_cnt_std_mth3_bin=as.vector(cut(credit.test$call_in_cnt_std_mth3,c(-Inf,7,20,30,Inf )))
credit.test$call_in_cnt_std_mth3_bin[is.na(credit.test$call_in_cnt_std_mth3)] = "缺失"
credit.test$call_in_cnt_std_mth3_bin[credit.test$call_in_cnt_std_mth3_bin %in% c("(30, Inf]","缺失")]=paste(c("(30, Inf]","缺失"),collapse = "|")
credit.test$call_in_cnt_std_mth3_bin<-factor(credit.test$call_in_cnt_std_mth3_bin,levels = c("(-Inf,7]","(7,20]","(20,30]","(30, Inf]|缺失"))
table(credit.test$call_in_cnt_std_mth3_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("call_in_cnt_std_mth3_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("call_in_cnt_std_mth3_bin"),summary=FALSE))

#call_len_avg 
summary(credit.train$call_len_avg);
iv.num(credit.train,"call_len_avg","target",rcontrol=rpart.control(cp=.0016)) #call_len_avg
credit.train$call_len_avg_bin=cut(credit.train$call_len_avg,c(-Inf,350,Inf ))
table(credit.train$call_len_avg_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("call_len_avg_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("call_len_avg_bin"),summary=FALSE))

credit.test$call_len_avg_bin=cut(credit.test$call_len_avg,c(-Inf,350,Inf ))
table(credit.test$call_len_avg_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("call_len_avg_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("call_len_avg_bin"),summary=FALSE))

#call_in_len_avg 
summary(credit.train$call_in_len_avg); 
iv.num(credit.train,"call_in_len_avg","target",rcontrol=rpart.control(cp=.0020)) #call_in_len_avg
credit.train$call_in_len_avg_bin=cut(credit.train$call_in_len_avg,c(-Inf,150,230,Inf ))
table(credit.train$call_in_len_avg_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("call_in_len_avg_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("call_in_len_avg_bin"),summary=FALSE))

credit.test$call_in_len_avg_bin=cut(credit.test$call_in_len_avg,c(-Inf,150,230,Inf ))
table(credit.test$call_in_len_avg_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("call_in_len_avg_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("call_in_len_avg_bin"),summary=FALSE))

#call_result_110_cnt
summary(credit.train$call_result_110_cnt);
iv.num(credit.train,"call_result_110_cnt","target",rcontrol=rpart.control(cp=.0016)) #call_result_110_cnt
credit.train$call_result_110_cnt_bin=cut(credit.train$call_result_110_cnt,c(-Inf,0,Inf ))
table(credit.train$call_result_110_cnt_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("call_result_110_cnt_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("call_result_110_cnt_bin"),summary=FALSE))

credit.test$call_result_110_cnt_bin=cut(credit.test$call_result_110_cnt,c(-Inf,0,Inf ))
table(credit.test$call_result_110_cnt_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("call_result_110_cnt_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("call_result_110_cnt_bin"),summary=FALSE))

#call_time_std_mth5 c(-Inf,60,80,Inf )  c(-Inf,60,Inf )
summary(credit.train$call_time_std_mth5)
iv.num(credit.train,"call_time_std_mth5","target",rcontrol=rpart.control(cp=.0010)) #call_time_std_mth5
credit.train$call_time_std_mth5_bin=as.vector(cut(credit.train$call_time_std_mth5,c(-Inf,60,80,Inf )))
credit.train$call_time_std_mth5_bin[is.na(credit.train$call_time_std_mth5)] = "缺失"
credit.train$call_time_std_mth5_bin[credit.train$call_time_std_mth5_bin %in% c("(80, Inf]","缺失")]=paste(c("(80, Inf]","缺失"),collapse = "|")
credit.train$call_time_std_mth5_bin<-factor(credit.train$call_time_std_mth5_bin,levels = c("(-Inf,60]","(60,80]","(80, Inf]|缺失"))
table(credit.train$call_time_std_mth5_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("call_time_std_mth5_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("call_time_std_mth5_bin"),summary=FALSE))

credit.test$call_time_std_mth5_bin=as.vector(cut(credit.test$call_time_std_mth5,c(-Inf,60,80,Inf )))
credit.test$call_time_std_mth5_bin[is.na(credit.test$call_time_std_mth5)] = "缺失"
credit.test$call_time_std_mth5_bin[credit.test$call_time_std_mth5_bin %in% c("(80, Inf]","缺失")]=paste(c("(80, Inf]","缺失"),collapse = "|")
credit.test$call_time_std_mth5_bin<-factor(credit.test$call_time_std_mth5_bin,levels = c("(-Inf,60]","(60,80]","(80, Inf]|缺失"))
table(credit.test$call_time_std_mth5_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("call_time_std_mth5_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("call_time_std_mth5_bin"),summary=FALSE))

#call_in_time_std_mth5
summary(credit.train$call_in_time_std_mth5)
iv.num(credit.train,"call_in_time_std_mth5","target",rcontrol=rpart.control(cp=.0010)) #call_in_time_std_mth5
credit.train$call_in_time_std_mth5_bin=as.vector(cut(credit.train$call_in_time_std_mth5,c(-Inf,20,40,Inf )))
credit.train$call_in_time_std_mth5_bin[is.na(credit.train$call_in_time_std_mth5)] = "缺失"
credit.train$call_in_time_std_mth5_bin[credit.train$call_in_time_std_mth5_bin %in% c("(40, Inf]","缺失")]=paste(c("(40, Inf]","缺失"),collapse = "|")
credit.train$call_in_time_std_mth5_bin<-factor(credit.train$call_in_time_std_mth5_bin,levels = c("(-Inf,20]","(20,40]","(40, Inf]|缺失"))
table(credit.train$call_in_time_std_mth5_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("call_in_time_std_mth5_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("call_in_time_std_mth5_bin"),summary=FALSE))

credit.test$call_in_time_std_mth5_bin=as.vector(cut(credit.test$call_in_time_std_mth5,c(-Inf,20,40,Inf )))
credit.test$call_in_time_std_mth5_bin[is.na(credit.test$call_in_time_std_mth5)] = "缺失"
credit.test$call_in_time_std_mth5_bin[credit.test$call_in_time_std_mth5_bin %in% c("(40, Inf]","缺失")]=paste(c("(40, Inf]","缺失"),collapse = "|")
credit.test$call_in_time_std_mth5_bin<-factor(credit.test$call_in_time_std_mth5_bin,levels = c("(-Inf,20]","(20,40]","(40, Inf]|缺失"))
table(credit.test$call_in_time_std_mth5_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("call_in_time_std_mth5_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("call_in_time_std_mth5_bin"),summary=FALSE))

#cont_call_in_cnt_pct c(-Inf,0.45,0.55,0.6,Inf )
summary(credit.train$cont_call_in_cnt_pct);
iv.num(credit.train,"cont_call_in_cnt_pct","target",rcontrol=rpart.control(cp=.0016)) #cont_call_in_cnt_pct
credit.train$cont_call_in_cnt_pct_bin=cut(credit.train$cont_call_in_cnt_pct,c(-Inf,0.45,0.6,Inf ))
table(credit.train$cont_call_in_cnt_pct_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("cont_call_in_cnt_pct_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("cont_call_in_cnt_pct_bin"),summary=FALSE))

credit.test$cont_call_in_cnt_pct_bin=cut(credit.test$cont_call_in_cnt_pct,c(-Inf,0.45,0.6,Inf ))
table(credit.test$cont_call_in_cnt_pct_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("cont_call_in_cnt_pct_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("cont_call_in_cnt_pct_bin"),summary=FALSE))

#contact_1m_avg c(-Inf,3.5,4,5.5,Inf )
summary(credit.train$contact_1m_avg);
iv.num(credit.train,"contact_1m_avg","target",rcontrol=rpart.control(cp=.0016)) #contact_1m_avg
credit.train$contact_1m_avg_bin=cut(credit.train$contact_1m_avg,c(-Inf,3.5,Inf ))
table(credit.train$contact_1m_avg_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("contact_1m_avg_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("contact_1m_avg_bin"),summary=FALSE))

credit.test$contact_1m_avg_bin=cut(credit.test$contact_1m_avg,c(-Inf,3.5,Inf ))
table(credit.test$contact_1m_avg_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("contact_1m_avg_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("contact_1m_avg_bin"),summary=FALSE))

#contact_1w_avg #该变量最好不要选入模型中 c(-Inf,0,1.5,2,2.7,3.5,Inf )
summary(credit.train$contact_1w_avg)
iv.num(credit.train,"contact_1w_avg","target",rcontrol=rpart.control(cp=.0011)) #contact_1w_avg
credit.train$contact_1w_avg_bin=as.vector(cut(credit.train$contact_1w_avg,c(-Inf,0,1.5,2,2.5,Inf )))
credit.train$contact_1w_avg_bin[is.na(credit.train$contact_1w_avg)] = "缺失"
credit.train$contact_1w_avg_bin<-factor(credit.train$contact_1w_avg_bin,levels = c("(0,1.5]","(1.5,2]","(2,2.5]","(2.5, Inf]","缺失"))
table(credit.train$contact_1w_avg_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("contact_1w_avg_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("contact_1w_avg_bin"),summary=FALSE))

credit.test$contact_1w_avg_bin=as.vector(cut(credit.test$contact_1w_avg,c(-Inf,0,1.5,2,2.5,Inf )))
credit.test$contact_1w_avg_bin[is.na(credit.test$contact_1w_avg)] = "缺失"
credit.test$contact_1w_avg_bin<-factor(credit.test$contact_1w_avg_bin,levels = c("(0,1.5]","(1.5,2]","(2,2.5]","(2.5, Inf]","缺失"))
table(credit.test$contact_1w_avg_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("contact_1w_avg_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("contact_1w_avg_bin"),summary=FALSE))

#contact1_mobile_notin_m6_call_re
summary(credit.train$contact1_mobile_notin_m6_call_re);
iv.num(credit.train,"contact1_mobile_notin_m6_call_re","target",rcontrol=rpart.control(cp=.0010)) #contact1_mobile_notin_m6_call_re
credit.train$contact1_mobile_notin_m6_call_re_bin=cut(credit.train$contact1_mobile_notin_m6_call_re,c(-Inf,20,Inf ))
table(credit.train$contact1_mobile_notin_m6_call_re_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("contact1_mobile_notin_m6_call_re_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("contact1_mobile_notin_m6_call_re_bin"),summary=FALSE))

credit.test$contact1_mobile_notin_m6_call_re_bin=cut(credit.test$contact1_mobile_notin_m6_call_re,c(-Inf,20,Inf ))
table(credit.test$contact1_mobile_notin_m6_call_re_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("contact1_mobile_notin_m6_call_re_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("contact1_mobile_notin_m6_call_re_bin"),summary=FALSE))

#d150_d3_nocall_times
summary(credit.train$d150_d3_nocall_times);
iv.num(credit.train,"d150_d3_nocall_times","target",rcontrol=rpart.control(cp=.0016)) #d150_d3_nocall_times
credit.train$d150_d3_nocall_times_bin=cut(credit.train$d150_d3_nocall_times,c(-Inf,1,2,Inf ))
table(credit.train$d150_d3_nocall_times_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("d150_d3_nocall_times_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("d150_d3_nocall_times_bin"),summary=FALSE))

credit.test$d150_d3_nocall_times_bin=cut(credit.test$d150_d3_nocall_times,c(-Inf,1,2,Inf ))
table(credit.test$d150_d3_nocall_times_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("d150_d3_nocall_times_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("d150_d3_nocall_times_bin"),summary=FALSE))

#d180_call_contacts_count
summary(credit.train$d180_call_contacts_count); #之前的分割c(-Inf,3,5,15,42,Inf )
iv.num(credit.train,"d180_call_contacts_count","target",rcontrol=rpart.control(cp=.0012)) #d180_call_contacts_count
credit.train$d180_call_contacts_count_bin=cut(credit.train$d180_call_contacts_count,c(-Inf,3,10,42,Inf ))
table(credit.train$d180_call_contacts_count_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("d180_call_contacts_count_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("d180_call_contacts_count_bin"),summary=FALSE))

credit.test$d180_call_contacts_count_bin=cut(credit.test$d180_call_contacts_count,c(-Inf,3,10,42,Inf ))
table(credit.test$d180_call_contacts_count_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("d180_call_contacts_count_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("d180_call_contacts_count_bin"),summary=FALSE))

#d180_d0_nocall_days
summary(credit.train$d180_d0_nocall_days);
iv.num(credit.train,"d180_d0_nocall_days","target",rcontrol=rpart.control(cp=.0016)) #d180_d0_nocall_days
credit.train$d180_d0_nocall_days_bin=cut(credit.train$d180_d0_nocall_days,c(-Inf,15,30,Inf ))
table(credit.train$d180_d0_nocall_days_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("d180_d0_nocall_days_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("d180_d0_nocall_days_bin"),summary=FALSE))

credit.test$d180_d0_nocall_days_bin=cut(credit.test$d180_d0_nocall_days,c(-Inf,15,30,Inf ))
table(credit.test$d180_d0_nocall_days_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("d180_d0_nocall_days_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("d180_d0_nocall_days_bin"),summary=FALSE))

#d180_d7_nocall_times
summary(credit.train$d180_d7_nocall_times);#之前的分割 c(-Inf,0,1,2,Inf )
iv.num(credit.train,"d180_d7_nocall_times","target",rcontrol=rpart.control(cp=.0010)) #d180_d7_nocall_times
credit.train$d180_d7_nocall_times_bin=cut(credit.train$d180_d7_nocall_times,c(-Inf,0,1,Inf ))
table(credit.train$d180_d7_nocall_times_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("d180_d7_nocall_times_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("d180_d7_nocall_times_bin"),summary=FALSE))

credit.test$d180_d7_nocall_times_bin=cut(credit.test$d180_d7_nocall_times,c(-Inf,0,1,Inf ))
table(credit.test$d180_d7_nocall_times_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("d180_d7_nocall_times_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("d180_d7_nocall_times_bin"),summary=FALSE))

#d180_max_nocall_days
summary(credit.train$d180_max_nocall_days);
iv.num(credit.train,"d180_max_nocall_days","target",rcontrol=rpart.control(cp=.0016)) #d180_max_nocall_days
credit.train$d180_max_nocall_days_bin=cut(credit.train$d180_max_nocall_days,c(-Inf,7,12,Inf ))
table(credit.train$d180_max_nocall_days_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("d180_max_nocall_days_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("d180_max_nocall_days_bin"),summary=FALSE))

credit.test$d180_max_nocall_days_bin=cut(credit.test$d180_max_nocall_days,c(-Inf,7,12,Inf ))
table(credit.test$d180_max_nocall_days_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("d180_max_nocall_days_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("d180_max_nocall_days_bin"),summary=FALSE))

#d30_call_total_time c(-Inf,7500,9500,15000,Inf )
summary(credit.train$d30_call_total_time);
iv.num(credit.train,"d30_call_total_time","target",rcontrol=rpart.control(cp=.0016)) #d30_call_total_time
credit.train$d30_call_total_time_bin=cut(credit.train$d30_call_total_time,c(-Inf,7500,9000,15000,Inf ))
table(credit.train$d30_call_total_time_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("d30_call_total_time_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("d30_call_total_time_bin"),summary=FALSE))

credit.test$d30_call_total_time_bin=cut(credit.test$d30_call_total_time,c(-Inf,7500,9000,15000,Inf ))
table(credit.test$d30_call_total_time_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("d30_call_total_time_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("d30_call_total_time_bin"),summary=FALSE))

#d30_call_total_times
summary(credit.train$d30_call_total_times);
iv.num(credit.train,"d30_call_total_times","target",rcontrol=rpart.control(cp=.0006)) #d30_call_total_times
credit.train$d30_call_total_times_bin=as.vector(cut(credit.train$d30_call_total_times,c(-Inf,50,140,200,Inf )))
credit.train$d30_call_total_times_bin[credit.train$d30_call_total_times_bin %in% c("(-Inf,50]","(200, Inf]")]=paste(c("(-Inf,50]","(200, Inf]"),collapse = "|")
table(credit.train$d30_call_total_times_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("d30_call_total_times_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("d30_call_total_times_bin"),summary=FALSE))

credit.test$d30_call_total_times_bin=as.vector(cut(credit.test$d30_call_total_times,c(-Inf,50,140,200,Inf )))
credit.test$d30_call_total_times_bin[credit.test$d30_call_total_times_bin %in% c("(-Inf,50]","(200, Inf]")]=paste(c("(-Inf,50]","(200, Inf]"),collapse = "|")
table(credit.test$d30_call_total_times_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("d30_call_total_times_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("d30_call_total_times_bin"),summary=FALSE))

#d30_d4_nocall_times
summary(credit.train$d30_d4_nocall_times);#c(-Inf,70,100,150,Inf )之前的分割
iv.num(credit.train,"d30_d4_nocall_times","target",rcontrol=rpart.control(cp=.0010)) #d30_d4_nocall_times
credit.train$d30_d4_nocall_times_bin=cut(credit.train$d30_d4_nocall_times,c(-Inf,0,Inf ))
table(credit.train$d30_d4_nocall_times_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("d30_d4_nocall_times_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("d30_d4_nocall_times_bin"),summary=FALSE))

credit.test$d30_d4_nocall_times_bin=cut(credit.test$d30_d4_nocall_times,c(-Inf,0,Inf ))
table(credit.test$d30_d4_nocall_times_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("d30_d4_nocall_times_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("d30_d4_nocall_times_bin"),summary=FALSE))

#d90_d0_nocall_days
summary(credit.train$d90_d0_nocall_days);
iv.num(credit.train,"d90_d0_nocall_days","target",rcontrol=rpart.control(cp=.0008)) #d90_d0_nocall_days
credit.train$d90_d0_nocall_days_bin=cut(credit.train$d90_d0_nocall_days,c(-Inf,5,15,Inf ))
table(credit.train$d90_d0_nocall_days_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("d90_d0_nocall_days_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("d90_d0_nocall_days_bin"),summary=FALSE))

credit.test$d90_d0_nocall_days_bin=cut(credit.test$d90_d0_nocall_days,c(-Inf,5,15,Inf ))
table(credit.test$d90_d0_nocall_days_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("d90_d0_nocall_days_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("d90_d0_nocall_days_bin"),summary=FALSE))

#d90_d5_nocall_times
summary(credit.train$d90_d5_nocall_times);
iv.num(credit.train,"d90_d5_nocall_times","target",rcontrol=rpart.control(cp=.0005)) #d90_d5_nocall_times
credit.train$d90_d5_nocall_times_bin=cut(credit.train$d90_d5_nocall_times,c(-Inf,1,Inf ))
table(credit.train$d90_d5_nocall_times_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("d90_d5_nocall_times_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("d90_d5_nocall_times_bin"),summary=FALSE))

credit.test$d90_d5_nocall_times_bin=cut(credit.test$d90_d5_nocall_times,c(-Inf,1,Inf ))
table(credit.test$d90_d5_nocall_times_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("d90_d5_nocall_times_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("d90_d5_nocall_times_bin"),summary=FALSE))

#early_morning_avg c(-Inf,1.2,1.5,3,4,Inf )
summary(credit.train$early_morning_avg)
iv.num(credit.train,"early_morning_avg","target",rcontrol=rpart.control(cp=.0010)) #early_morning_avg
credit.train$early_morning_avg_bin=as.vector(cut(credit.train$early_morning_avg,c(-Inf,1.2,1.5,3,Inf )))
credit.train$early_morning_avg_bin[is.na(credit.train$early_morning_avg)] = "缺失"
credit.train$early_morning_avg_bin[credit.train$early_morning_avg_bin %in% c("(-Inf,1.2]","缺失")]=paste(c("(-Inf,1.2]","缺失"),collapse = "|")
credit.train$early_morning_avg_bin<-factor(credit.train$early_morning_avg_bin,levels = c("(-Inf,1.2]|缺失","(1.2,1.5]","(1.5,3]","(3, Inf]"))
table(credit.train$early_morning_avg_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("early_morning_avg_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("early_morning_avg_bin"),summary=FALSE))

credit.test$early_morning_avg_bin=as.vector(cut(credit.test$early_morning_avg,c(-Inf,1.2,1.5,3,Inf )))
credit.test$early_morning_avg_bin[is.na(credit.test$early_morning_avg)] = "缺失"
credit.test$early_morning_avg_bin[credit.test$early_morning_avg_bin %in% c("(-Inf,1.2]","缺失")]=paste(c("(-Inf,1.2]","缺失"),collapse = "|")
credit.test$early_morning_avg_bin<-factor(credit.test$early_morning_avg_bin,levels = c("(-Inf,1.2]|缺失","(1.2,1.5]","(1.5,3]","(3, Inf]"))
table(credit.test$early_morning_avg_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("early_morning_avg_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("early_morning_avg_bin"),summary=FALSE))

#ph_callcount_last7_28
summary(credit.train$ph_callcount_last7_28);
iv.num(credit.train,"ph_callcount_last7_28","target",rcontrol=rpart.control(cp=.0016)) #ph_callcount_last7_28
credit.train$ph_callcount_last7_28_bin=cut(credit.train$ph_callcount_last7_28,c(-Inf,0.9,1.5,Inf ))
table(credit.train$ph_callcount_last7_28_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("ph_callcount_last7_28_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("ph_callcount_last7_28_bin"),summary=FALSE))

credit.test$ph_callcount_last7_28_bin=cut(credit.test$ph_callcount_last7_28,c(-Inf,0.9,1.5,Inf ))
table(credit.test$ph_callcount_last7_28_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("ph_callcount_last7_28_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("ph_callcount_last7_28_bin"),summary=FALSE))

#ph_con_day_fir_now_cont1  c(-Inf,160,170,Inf )
summary(credit.train$ph_con_day_fir_now_cont1)  
iv.num(credit.train,"ph_con_day_fir_now_cont1","target",rcontrol=rpart.control(cp=.0008)) #ph_con_day_fir_now_cont1
credit.train$ph_con_day_fir_now_cont1_bin=as.vector(cut(credit.train$ph_con_day_fir_now_cont1,c(-Inf,160,Inf )))
credit.train$ph_con_day_fir_now_cont1_bin[is.na(credit.train$ph_con_day_fir_now_cont1)] = "缺失"
credit.train$ph_con_day_fir_now_cont1_bin[credit.train$ph_con_day_fir_now_cont1_bin %in% c("(-Inf,160]","缺失")]=paste(c("(-Inf,160]","缺失"),collapse = "|")
credit.train$ph_con_day_fir_now_cont1_bin<-factor(credit.train$ph_con_day_fir_now_cont1_bin,levels = c("(-Inf,160]|缺失","(160, Inf]"))
table(credit.train$ph_con_day_fir_now_cont1_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("ph_con_day_fir_now_cont1_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("ph_con_day_fir_now_cont1_bin"),summary=FALSE))

credit.test$ph_con_day_fir_now_cont1_bin=as.vector(cut(credit.test$ph_con_day_fir_now_cont1,c(-Inf,160,Inf )))
credit.test$ph_con_day_fir_now_cont1_bin[is.na(credit.test$ph_con_day_fir_now_cont1)] = "缺失"
credit.test$ph_con_day_fir_now_cont1_bin[credit.test$ph_con_day_fir_now_cont1_bin %in% c("(-Inf,160]","缺失")]=paste(c("(-Inf,160]","缺失"),collapse = "|")
credit.test$ph_con_day_fir_now_cont1_bin<-factor(credit.test$ph_con_day_fir_now_cont1_bin,levels = c("(-Inf,160]|缺失","(160, Inf]"))
table(credit.test$ph_con_day_fir_now_cont1_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("ph_con_day_fir_now_cont1_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("ph_con_day_fir_now_cont1_bin"),summary=FALSE))

#ph_con_day_fir_now_cont2
summary(credit.train$ph_con_day_fir_now_cont2)
iv.num(credit.train,"ph_con_day_fir_now_cont2","target",rcontrol=rpart.control(cp=.0010)) #ph_con_day_fir_now_cont2
credit.train$ph_con_day_fir_now_cont2_bin=as.vector(cut(credit.train$ph_con_day_fir_now_cont2,c(-Inf,100,160,Inf )))
credit.train$ph_con_day_fir_now_cont2_bin[is.na(credit.train$ph_con_day_fir_now_cont2)] = "缺失"
credit.train$ph_con_day_fir_now_cont2_bin<-factor(credit.train$ph_con_day_fir_now_cont2_bin,levels = c("(-Inf,100]","(100,160]","(160, Inf]","缺失"))
table(credit.train$ph_con_day_fir_now_cont2_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("ph_con_day_fir_now_cont2_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("ph_con_day_fir_now_cont2_bin"),summary=FALSE))

credit.test$ph_con_day_fir_now_cont2_bin=as.vector(cut(credit.test$ph_con_day_fir_now_cont2,c(-Inf,100,160,Inf )))
credit.test$ph_con_day_fir_now_cont2_bin[is.na(credit.test$ph_con_day_fir_now_cont2)] = "缺失"
credit.test$ph_con_day_fir_now_cont2_bin<-factor(credit.test$ph_con_day_fir_now_cont2_bin,levels = c("(-Inf,100]","(100,160]","(160, Inf]","缺失"))
table(credit.test$ph_con_day_fir_now_cont2_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("ph_con_day_fir_now_cont2_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("ph_con_day_fir_now_cont2_bin"),summary=FALSE))

#ph_con_day_last_now_cont1
summary(credit.train$ph_con_day_last_now_cont1)
iv.num(credit.train,"ph_con_day_last_now_cont1","target",rcontrol=rpart.control(cp=.0008)) #ph_con_day_last_now_cont1
credit.train$ph_con_day_last_now_cont1_bin=as.vector(cut(credit.train$ph_con_day_last_now_cont1,c(-Inf,18,Inf )))
credit.train$ph_con_day_last_now_cont1_bin[is.na(credit.train$ph_con_day_last_now_cont1)] = "缺失"
credit.train$ph_con_day_last_now_cont1_bin[credit.train$ph_con_day_last_now_cont1_bin %in% c("(18, Inf]","缺失")]=paste(c("(18, Inf]","缺失"),collapse = "|")
credit.train$ph_con_day_last_now_cont1_bin<-factor(credit.train$ph_con_day_last_now_cont1_bin,levels = c("(-Inf,18]","(18, Inf]|缺失"))
table(credit.train$ph_con_day_last_now_cont1_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("ph_con_day_last_now_cont1_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("ph_con_day_last_now_cont1_bin"),summary=FALSE))

credit.test$ph_con_day_last_now_cont1_bin=as.vector(cut(credit.test$ph_con_day_last_now_cont1,c(-Inf,18,Inf )))
credit.test$ph_con_day_last_now_cont1_bin[is.na(credit.test$ph_con_day_last_now_cont1)] = "缺失"
credit.test$ph_con_day_last_now_cont1_bin[credit.test$ph_con_day_last_now_cont1_bin %in% c("(18, Inf]","缺失")]=paste(c("(18, Inf]","缺失"),collapse = "|")
credit.test$ph_con_day_last_now_cont1_bin<-factor(credit.test$ph_con_day_last_now_cont1_bin,levels = c("(-Inf,18]","(18, Inf]|缺失"))
table(credit.test$ph_con_day_last_now_cont1_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("ph_con_day_last_now_cont1_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("ph_con_day_last_now_cont1_bin"),summary=FALSE))

#ph_con_day_last_now_cont2  变量结果有疑问，放弃该变量
summary(credit.train$ph_con_day_last_now_cont2)
iv.num(credit.train,"ph_con_day_last_now_cont2","target",rcontrol=rpart.control(cp=.0010)) #ph_con_day_last_now_cont2
credit.train$ph_con_day_last_now_cont2_bin=as.vector(cut(credit.train$ph_con_day_last_now_cont2,c(-Inf,40,Inf )))
credit.train$ph_con_day_last_now_cont2_bin[is.na(credit.train$ph_con_day_last_now_cont2)] = "缺失"
credit.train$ph_con_day_last_now_cont2_bin[credit.train$ph_con_day_last_now_cont2_bin %in% c("(40, Inf]","缺失")]=paste(c("(40, Inf]","缺失"),collapse = "|")
credit.train$ph_con_day_last_now_cont2_bin<-factor(credit.train$ph_con_day_last_now_cont2_bin,levels = c("(-Inf,40]","(40, Inf]|缺失"))
table(credit.train$ph_con_day_last_now_cont2_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("ph_con_day_last_now_cont2_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("ph_con_day_last_now_cont2_bin"),summary=FALSE))

credit.test$ph_con_day_last_now_cont2_bin=as.vector(cut(credit.test$ph_con_day_last_now_cont2,c(-Inf,40,Inf )))
credit.test$ph_con_day_last_now_cont2_bin[is.na(credit.test$ph_con_day_last_now_cont2)] = "缺失"
credit.test$ph_con_day_last_now_cont2_bin[credit.test$ph_con_day_last_now_cont2_bin %in% c("(40, Inf]","缺失")]=paste(c("(40, Inf]","缺失"),collapse = "|")
credit.test$ph_con_day_last_now_cont2_bin<-factor(credit.test$ph_con_day_last_now_cont2_bin,levels = c("(-Inf,40]","(40, Inf]|缺失"))
table(credit.test$ph_con_day_last_now_cont2_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("ph_con_day_last_now_cont2_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("ph_con_day_last_now_cont2_bin"),summary=FALSE))

#ph_cont_obj_effi_coin 变量结果有疑问，放弃该变量
summary(credit.train$ph_cont_obj_effi_coin);
iv.num(credit.train,"ph_cont_obj_effi_coin","target",rcontrol=rpart.control(cp=.0010)) #ph_cont_obj_effi_coin
credit.train$ph_cont_obj_effi_coin_bin=cut(credit.train$ph_cont_obj_effi_coin,c(-Inf,0,3,Inf ))
table(credit.train$ph_cont_obj_effi_coin_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("ph_cont_obj_effi_coin_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("ph_cont_obj_effi_coin_bin"),summary=FALSE))

credit.test$ph_cont_obj_effi_coin_bin=cut(credit.test$ph_cont_obj_effi_coin,c(-Inf,0,3,Inf ))
table(credit.test$ph_cont_obj_effi_coin_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("ph_cont_obj_effi_coin_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("ph_cont_obj_effi_coin_bin"),summary=FALSE))

#ph_day_last_now 结果不合理，而且用申请日算，与数据获取时间有差距，放弃该变量
summary(credit.train$ph_day_last_now); 
iv.num(credit.train,"ph_day_last_now","target",rcontrol=rpart.control(cp=.0010)) #ph_day_last_now
credit.train$ph_day_last_now_bin=cut(credit.train$ph_day_last_now,c(-Inf,2,8,Inf ))
table(credit.train$ph_day_last_now_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("ph_day_last_now_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("ph_day_last_now_bin"),summary=FALSE))

credit.test$ph_day_last_now_bin=cut(credit.test$ph_day_last_now,c(-Inf,2,8,Inf ))
table(credit.test$ph_day_last_now_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("ph_day_last_now_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("ph_day_last_now_bin"),summary=FALSE))

#ph_mean_talk_time_3mons_cont2 c(-Inf,5,8.5,15,Inf )
summary(credit.train$ph_mean_talk_time_3mons_cont2)
iv.num(credit.train,"ph_mean_talk_time_3mons_cont2","target",rcontrol=rpart.control(cp=.0010)) #ph_mean_talk_time_3mons_cont2
credit.train$ph_mean_talk_time_3mons_cont2_bin=as.vector(cut(credit.train$ph_mean_talk_time_3mons_cont2,c(-Inf,5,8.5,Inf )))
credit.train$ph_mean_talk_time_3mons_cont2_bin[is.na(credit.train$ph_mean_talk_time_3mons_cont2)] = "缺失"
credit.train$ph_mean_talk_time_3mons_cont2_bin<-factor(credit.train$ph_mean_talk_time_3mons_cont2_bin,levels = c("(-Inf,5]","(5,8.5]","(8.5, Inf]","缺失"))
table(credit.train$ph_mean_talk_time_3mons_cont2_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("ph_mean_talk_time_3mons_cont2_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("ph_mean_talk_time_3mons_cont2_bin"),summary=FALSE))

credit.test$ph_mean_talk_time_3mons_cont2_bin=as.vector(cut(credit.test$ph_mean_talk_time_3mons_cont2,c(-Inf,5,8.5,Inf )))
credit.test$ph_mean_talk_time_3mons_cont2_bin[is.na(credit.test$ph_mean_talk_time_3mons_cont2)] = "缺失"
credit.test$ph_mean_talk_time_3mons_cont2_bin<-factor(credit.test$ph_mean_talk_time_3mons_cont2_bin,levels = c("(-Inf,5]","(5,8.5]","(8.5, Inf]","缺失"))
table(credit.test$ph_mean_talk_time_3mons_cont2_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("ph_mean_talk_time_3mons_cont2_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("ph_mean_talk_time_3mons_cont2_bin"),summary=FALSE))

#ph_talk_freq_coincide_work_rest  c(-Inf,2,5,Inf )
summary(credit.train$ph_talk_freq_coincide_work_rest);
iv.num(credit.train,"ph_talk_freq_coincide_work_rest","target",rcontrol=rpart.control(cp=.0008)) #ph_talk_freq_coincide_work_rest
credit.train$ph_talk_freq_coincide_work_rest_bin=cut(credit.train$ph_talk_freq_coincide_work_rest,c(-Inf,2,5,Inf ))
table(credit.train$ph_talk_freq_coincide_work_rest_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("ph_talk_freq_coincide_work_rest_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("ph_talk_freq_coincide_work_rest_bin"),summary=FALSE))

credit.test$ph_talk_freq_coincide_work_rest_bin=cut(credit.test$ph_talk_freq_coincide_work_rest,c(-Inf,2,5,Inf ))
table(credit.test$ph_talk_freq_coincide_work_rest_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("ph_talk_freq_coincide_work_rest_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("ph_talk_freq_coincide_work_rest_bin"),summary=FALSE))

#ph_talk_freq_per_0_6  c(-Inf,0.005,0.04,0.08,Inf )
summary(credit.train$ph_talk_freq_per_0_6) 
iv.num(credit.train,"ph_talk_freq_per_0_6","target",rcontrol=rpart.control(cp=.0016)) #ph_talk_freq_per_0_6
credit.train$ph_talk_freq_per_0_6_bin=as.vector(cut(credit.train$ph_talk_freq_per_0_6,c(-Inf,0.005,0.02,0.04,Inf )))
credit.train$ph_talk_freq_per_0_6_bin[is.na(credit.train$ph_talk_freq_per_0_6)] = "缺失"
credit.train$ph_talk_freq_per_0_6_bin[credit.train$ph_talk_freq_per_0_6_bin %in% c("(0.04, Inf]","缺失")]=paste(c("(0.04, Inf]","缺失"),collapse = "|")
credit.train$ph_talk_freq_per_0_6_bin<-factor(credit.train$ph_talk_freq_per_0_6_bin,levels = c("(-Inf,0.005]","(0.005,0.02]","(0.02,0.04]","(0.04, Inf]|缺失"))
table(credit.train$ph_talk_freq_per_0_6_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("ph_talk_freq_per_0_6_bin"))
# iv.plot.woe(iv.mult(credit.train,"target",vars=c("ph_talk_freq_per_0_6_bin"),summary=FALSE))

credit.test$ph_talk_freq_per_0_6_bin=as.vector(cut(credit.test$ph_talk_freq_per_0_6,c(-Inf,0.005,0.02,0.04,Inf )))
credit.test$ph_talk_freq_per_0_6_bin[is.na(credit.test$ph_talk_freq_per_0_6)] = "缺失"
credit.test$ph_talk_freq_per_0_6_bin[credit.test$ph_talk_freq_per_0_6_bin %in% c("(0.04, Inf]","缺失")]=paste(c("(0.04, Inf]","缺失"),collapse = "|")
credit.test$ph_talk_freq_per_0_6_bin<-factor(credit.test$ph_talk_freq_per_0_6_bin,levels = c("(-Inf,0.005]","(0.005,0.02]","(0.02,0.04]","(0.04, Inf]|缺失"))
table(credit.test$ph_talk_freq_per_0_6_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("ph_talk_freq_per_0_6_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("ph_talk_freq_per_0_6_bin"),summary=FALSE))

#ph_talk_freq_per_0_6_cont1
summary(credit.train$ph_talk_freq_per_0_6_cont1)
iv.num(credit.train,"ph_talk_freq_per_0_6_cont1","target",rcontrol=rpart.control(cp=.0010)) #ph_talk_freq_per_0_6_cont1
credit.train$ph_talk_freq_per_0_6_cont1_bin=as.vector(cut(credit.train$ph_talk_freq_per_0_6_cont1,c(-Inf,0,Inf )))
credit.train$ph_talk_freq_per_0_6_cont1_bin[is.na(credit.train$ph_talk_freq_per_0_6_cont1)] = "缺失"
credit.train$ph_talk_freq_per_0_6_cont1_bin[credit.train$ph_talk_freq_per_0_6_cont1_bin %in% c("(0, Inf]","缺失")]=paste(c("(0, Inf]","缺失"),collapse = "|")
credit.train$ph_talk_freq_per_0_6_cont1_bin<-factor(credit.train$ph_talk_freq_per_0_6_cont1_bin,levels = c("(-Inf,0]","(0, Inf]|缺失"))
table(credit.train$ph_talk_freq_per_0_6_cont1_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("ph_talk_freq_per_0_6_cont1_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("ph_talk_freq_per_0_6_cont1_bin"),summary=FALSE))

credit.test$ph_talk_freq_per_0_6_cont1_bin=as.vector(cut(credit.test$ph_talk_freq_per_0_6_cont1,c(-Inf,0,Inf )))
credit.test$ph_talk_freq_per_0_6_cont1_bin[is.na(credit.test$ph_talk_freq_per_0_6_cont1)] = "缺失"
credit.test$ph_talk_freq_per_0_6_cont1_bin[credit.test$ph_talk_freq_per_0_6_cont1_bin %in% c("(0, Inf]","缺失")]=paste(c("(0, Inf]","缺失"),collapse = "|")
credit.test$ph_talk_freq_per_0_6_cont1_bin<-factor(credit.test$ph_talk_freq_per_0_6_cont1_bin,levels = c("(-Inf,0]","(0, Inf]|缺失"))
table(credit.test$ph_talk_freq_per_0_6_cont1_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("ph_talk_freq_per_0_6_cont1_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("ph_talk_freq_per_0_6_cont1_bin"),summary=FALSE))

#ph_talk_freq_per_12_18
summary(credit.train$ph_talk_freq_per_12_18)
iv.num(credit.train,"ph_talk_freq_per_12_18","target",rcontrol=rpart.control(cp=.0010)) #ph_talk_freq_per_12_18
credit.train$ph_talk_freq_per_12_18_bin=as.vector(cut(credit.train$ph_talk_freq_per_12_18,c(-Inf,0.4,0.45,Inf )))
credit.train$ph_talk_freq_per_12_18_bin[is.na(credit.train$ph_talk_freq_per_12_18)] = "缺失"
credit.train$ph_talk_freq_per_12_18_bin[credit.train$ph_talk_freq_per_12_18_bin %in% c("(-Inf,0.4]","缺失")]=paste(c("(-Inf,0.4]","缺失"),collapse = "|")
credit.train$ph_talk_freq_per_12_18_bin<-factor(credit.train$ph_talk_freq_per_12_18_bin,levels = c("(-Inf,0.4]|缺失","(0.4,0.45]","(0.45, Inf]"))
table(credit.train$ph_talk_freq_per_12_18_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("ph_talk_freq_per_12_18_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("ph_talk_freq_per_12_18_bin"),summary=FALSE))

credit.test$ph_talk_freq_per_12_18_bin=as.vector(cut(credit.test$ph_talk_freq_per_12_18,c(-Inf,0.4,0.45,Inf )))
credit.test$ph_talk_freq_per_12_18_bin[is.na(credit.test$ph_talk_freq_per_12_18)] = "缺失"
credit.test$ph_talk_freq_per_12_18_bin[credit.test$ph_talk_freq_per_12_18_bin %in% c("(-Inf,0.4]","缺失")]=paste(c("(-Inf,0.4]","缺失"),collapse = "|")
credit.test$ph_talk_freq_per_12_18_bin<-factor(credit.test$ph_talk_freq_per_12_18_bin,levels = c("(-Inf,0.4]|缺失","(0.4,0.45]","(0.45, Inf]"))
table(credit.test$ph_talk_freq_per_12_18_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("ph_talk_freq_per_12_18_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("ph_talk_freq_per_12_18_bin"),summary=FALSE))

#ph_talk_freq_per_6_12
summary(credit.train$ph_talk_freq_per_6_12)
iv.num(credit.train,"ph_talk_freq_per_6_12","target",rcontrol=rpart.control(cp=.0010)) #ph_talk_freq_per_6_12
credit.train$ph_talk_freq_per_6_12_bin=as.vector(cut(credit.train$ph_talk_freq_per_6_12,c(-Inf,0.2,0.3,Inf )))
credit.train$ph_talk_freq_per_6_12_bin[is.na(credit.train$ph_talk_freq_per_6_12)] = "缺失"
credit.train$ph_talk_freq_per_6_12_bin[credit.train$ph_talk_freq_per_6_12_bin %in% c("(-Inf,0.2]","缺失")]=paste(c("(-Inf,0.2]","缺失"),collapse = "|")
credit.train$ph_talk_freq_per_6_12_bin<-factor(credit.train$ph_talk_freq_per_6_12_bin,levels = c("(-Inf,0.2]|缺失","(0.2,0.3]","(0.3, Inf]"))
table(credit.train$ph_talk_freq_per_6_12_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("ph_talk_freq_per_6_12_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("ph_talk_freq_per_6_12_bin"),summary=FALSE))

credit.test$ph_talk_freq_per_6_12_bin=as.vector(cut(credit.test$ph_talk_freq_per_6_12,c(-Inf,0.2,0.3,Inf )))
credit.test$ph_talk_freq_per_6_12_bin[is.na(credit.test$ph_talk_freq_per_6_12)] = "缺失"
credit.test$ph_talk_freq_per_6_12_bin[credit.test$ph_talk_freq_per_6_12_bin %in% c("(-Inf,0.2]","缺失")]=paste(c("(-Inf,0.2]","缺失"),collapse = "|")
credit.test$ph_talk_freq_per_6_12_bin<-factor(credit.test$ph_talk_freq_per_6_12_bin,levels = c("(-Inf,0.2]|缺失","(0.2,0.3]","(0.3, Inf]"))
table(credit.test$ph_talk_freq_per_6_12_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("ph_talk_freq_per_6_12_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("ph_talk_freq_per_6_12_bin"),summary=FALSE))

#ph_talk_freq_per_6_12_cont1 
summary(credit.train$ph_talk_freq_per_6_12_cont1)
iv.num(credit.train,"ph_talk_freq_per_6_12_cont1","target",rcontrol=rpart.control(cp=.0010)) #ph_talk_freq_per_6_12_cont1
credit.train$ph_talk_freq_per_6_12_cont1_bin=as.vector(cut(credit.train$ph_talk_freq_per_6_12_cont1,c(-Inf,0,0.005,0.01,0.015,Inf )))
credit.train$ph_talk_freq_per_6_12_cont1_bin[is.na(credit.train$ph_talk_freq_per_6_12_cont1)] = "缺失"
credit.train$ph_talk_freq_per_6_12_cont1_bin[credit.train$ph_talk_freq_per_6_12_cont1_bin %in% c("(-Inf,0]","缺失")]=paste(c("(-Inf,0]","缺失"),collapse = "|")
credit.train$ph_talk_freq_per_6_12_cont1_bin<-factor(credit.train$ph_talk_freq_per_6_12_cont1_bin,levels = c("(-Inf,0]|缺失","(0,0.005]","(0.005,0.01]","(0.01,0.015]","(0.015, Inf]"))
table(credit.train$ph_talk_freq_per_6_12_cont1_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("ph_talk_freq_per_6_12_cont1_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("ph_talk_freq_per_6_12_cont1_bin"),summary=FALSE))

credit.test$ph_talk_freq_per_6_12_cont1_bin=as.vector(cut(credit.test$ph_talk_freq_per_6_12_cont1,c(-Inf,0,0.005,0.01,0.015,Inf )))
credit.test$ph_talk_freq_per_6_12_cont1_bin[is.na(credit.test$ph_talk_freq_per_6_12_cont1)] = "缺失"
credit.test$ph_talk_freq_per_6_12_cont1_bin[credit.test$ph_talk_freq_per_6_12_cont1_bin %in% c("(-Inf,0]","缺失")]=paste(c("(-Inf,0]","缺失"),collapse = "|")
credit.test$ph_talk_freq_per_6_12_cont1_bin<-factor(credit.test$ph_talk_freq_per_6_12_cont1_bin,levels = c("(-Inf,0]|缺失","(0,0.005]","(0.005,0.01]","(0.01,0.015]","(0.015, Inf]"))
table(credit.test$ph_talk_freq_per_6_12_cont1_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("ph_talk_freq_per_6_12_cont1_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("ph_talk_freq_per_6_12_cont1_bin"),summary=FALSE))

#ph_talk_freq_per_6_12_cont2
summary(credit.train$ph_talk_freq_per_6_12_cont2)
iv.num(credit.train,"ph_talk_freq_per_6_12_cont2","target",rcontrol=rpart.control(cp=.0010)) #ph_talk_freq_per_6_12_cont2
credit.train$ph_talk_freq_per_6_12_cont2_bin=as.vector(cut(credit.train$ph_talk_freq_per_6_12_cont2,c(-Inf,0,Inf )))
credit.train$ph_talk_freq_per_6_12_cont2_bin[is.na(credit.train$ph_talk_freq_per_6_12_cont2)] = "缺失"
credit.train$ph_talk_freq_per_6_12_cont2_bin[credit.train$ph_talk_freq_per_6_12_cont2_bin %in% c("(-Inf,0]","缺失")]=paste(c("(-Inf,0]","缺失"),collapse = "|")
credit.train$ph_talk_freq_per_6_12_cont2_bin<-factor(credit.train$ph_talk_freq_per_6_12_cont2_bin,levels = c("(-Inf,0]|缺失","(0, Inf]"))
table(credit.train$ph_talk_freq_per_6_12_cont2_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("ph_talk_freq_per_6_12_cont2_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("ph_talk_freq_per_6_12_cont2_bin"),summary=FALSE))

credit.test$ph_talk_freq_per_6_12_cont2_bin=as.vector(cut(credit.test$ph_talk_freq_per_6_12_cont2,c(-Inf,0,Inf )))
credit.test$ph_talk_freq_per_6_12_cont2_bin[is.na(credit.test$ph_talk_freq_per_6_12_cont2)] = "缺失"
credit.test$ph_talk_freq_per_6_12_cont2_bin[credit.test$ph_talk_freq_per_6_12_cont2_bin %in% c("(-Inf,0]","缺失")]=paste(c("(-Inf,0]","缺失"),collapse = "|")
credit.test$ph_talk_freq_per_6_12_cont2_bin<-factor(credit.test$ph_talk_freq_per_6_12_cont2_bin,levels = c("(-Inf,0]|缺失","(0, Inf]"))
table(credit.test$ph_talk_freq_per_6_12_cont2_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("ph_talk_freq_per_6_12_cont2_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("ph_talk_freq_per_6_12_cont2_bin"),summary=FALSE))

#ph_talk_obj_per_0_6
summary(credit.train$ph_talk_obj_per_0_6)
iv.num(credit.train,"ph_talk_obj_per_0_6","target",rcontrol=rpart.control(cp=.0016)) #ph_talk_obj_per_0_6
credit.train$ph_talk_obj_per_0_6_bin=as.vector(cut(credit.train$ph_talk_obj_per_0_6,c(-Inf,0,0.18,Inf )))
credit.train$ph_talk_obj_per_0_6_bin[is.na(credit.train$ph_talk_obj_per_0_6)] = "缺失"
credit.train$ph_talk_obj_per_0_6_bin[credit.train$ph_talk_obj_per_0_6_bin %in% c("(0.18, Inf]","缺失")]=paste(c("(0.18, Inf]","缺失"),collapse = "|")
credit.train$ph_talk_obj_per_0_6_bin<-factor(credit.train$ph_talk_obj_per_0_6_bin,levels = c("(-Inf,0]","(0,0.18]","(0.18, Inf]|缺失"))
table(credit.train$ph_talk_obj_per_0_6_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("ph_talk_obj_per_0_6_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("ph_talk_obj_per_0_6_bin"),summary=FALSE))

credit.test$ph_talk_obj_per_0_6_bin=as.vector(cut(credit.test$ph_talk_obj_per_0_6,c(-Inf,0,0.18,Inf )))
credit.test$ph_talk_obj_per_0_6_bin[is.na(credit.test$ph_talk_obj_per_0_6)] = "缺失"
credit.test$ph_talk_obj_per_0_6_bin[credit.test$ph_talk_obj_per_0_6_bin %in% c("(0.18, Inf]","缺失")]=paste(c("(0.18, Inf]","缺失"),collapse = "|")
credit.test$ph_talk_obj_per_0_6_bin<-factor(credit.test$ph_talk_obj_per_0_6_bin,levels = c("(-Inf,0]","(0,0.18]","(0.18, Inf]|缺失"))
table(credit.test$ph_talk_obj_per_0_6_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("ph_talk_obj_per_0_6_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("ph_talk_obj_per_0_6_bin"),summary=FALSE))

#ph_talk_obj_per_18_24 
summary(credit.train$ph_talk_obj_per_18_24)
iv.num(credit.train,"ph_talk_obj_per_18_24","target",rcontrol=rpart.control(cp=.0010)) #ph_talk_obj_per_18_24
credit.train$ph_talk_obj_per_18_24_bin=as.vector(cut(credit.train$ph_talk_obj_per_18_24,c(-Inf,0.24,0.32,0.4,0.55,Inf )))
credit.train$ph_talk_obj_per_18_24_bin[is.na(credit.train$ph_talk_obj_per_18_24)] = "缺失"
credit.train$ph_talk_obj_per_18_24_bin[credit.train$ph_talk_obj_per_18_24_bin %in% c("(0.55, Inf]","缺失")]=paste(c("(0.55, Inf]","缺失"),collapse = "|")
credit.train$ph_talk_obj_per_18_24_bin<-factor(credit.train$ph_talk_obj_per_18_24_bin,levels = c("(-Inf,0.24]","(0.24,0.32]","(0.32,0.4]","(0.4,0.55]","(0.55, Inf]|缺失"))
table(credit.train$ph_talk_obj_per_18_24_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("ph_talk_obj_per_18_24_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("ph_talk_obj_per_18_24_bin"),summary=FALSE))

credit.test$ph_talk_obj_per_18_24_bin=as.vector(cut(credit.test$ph_talk_obj_per_18_24,c(-Inf,0.24,0.32,0.4,0.55,Inf )))
credit.test$ph_talk_obj_per_18_24_bin[is.na(credit.test$ph_talk_obj_per_18_24)] = "缺失"
credit.test$ph_talk_obj_per_18_24_bin[credit.test$ph_talk_obj_per_18_24_bin %in% c("(0.55, Inf]","缺失")]=paste(c("(0.55, Inf]","缺失"),collapse = "|")
credit.test$ph_talk_obj_per_18_24_bin<-factor(credit.test$ph_talk_obj_per_18_24_bin,levels = c("(-Inf,0.24]","(0.24,0.32]","(0.32,0.4]","(0.4,0.55]","(0.55, Inf]|缺失"))
table(credit.test$ph_talk_obj_per_18_24_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("ph_talk_obj_per_18_24_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("ph_talk_obj_per_18_24_bin"),summary=FALSE))

#ph_talk_time_per_0_6_cont1
summary(credit.train$ph_talk_time_per_0_6_cont1)
iv.num(credit.train,"ph_talk_time_per_0_6_cont1","target",rcontrol=rpart.control(cp=.0010)) #ph_talk_time_per_0_6_cont1
credit.train$ph_talk_time_per_0_6_cont1_bin=as.vector(cut(credit.train$ph_talk_time_per_0_6_cont1,c(-Inf,0,0.0008,Inf )))
credit.train$ph_talk_time_per_0_6_cont1_bin[is.na(credit.train$ph_talk_time_per_0_6_cont1)] = "缺失"
credit.train$ph_talk_time_per_0_6_cont1_bin[credit.train$ph_talk_time_per_0_6_cont1_bin %in% c("(0.0008, Inf]","缺失")]=paste(c("(0.0008, Inf]","缺失"),collapse = "|")
credit.train$ph_talk_time_per_0_6_cont1_bin<-factor(credit.train$ph_talk_time_per_0_6_cont1_bin,levels = c("(-Inf,0]","(0,0.0008]","(0.0008, Inf]|缺失"))
table(credit.train$ph_talk_time_per_0_6_cont1_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("ph_talk_time_per_0_6_cont1_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("ph_talk_time_per_0_6_cont1_bin"),summary=FALSE))

credit.test$ph_talk_time_per_0_6_cont1_bin=as.vector(cut(credit.test$ph_talk_time_per_0_6_cont1,c(-Inf,0,0.0008,Inf )))
credit.test$ph_talk_time_per_0_6_cont1_bin[is.na(credit.test$ph_talk_time_per_0_6_cont1)] = "缺失"
credit.test$ph_talk_time_per_0_6_cont1_bin[credit.test$ph_talk_time_per_0_6_cont1_bin %in% c("(0.0008, Inf]","缺失")]=paste(c("(0.0008, Inf]","缺失"),collapse = "|")
credit.test$ph_talk_time_per_0_6_cont1_bin<-factor(credit.test$ph_talk_time_per_0_6_cont1_bin,levels = c("(-Inf,0]","(0,0.0008]","(0.0008, Inf]|缺失"))
table(credit.test$ph_talk_time_per_0_6_cont1_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("ph_talk_time_per_0_6_cont1_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("ph_talk_time_per_0_6_cont1_bin"),summary=FALSE))

#ph_talk_time_per_0_6_cont2
summary(credit.train$ph_talk_time_per_0_6_cont2) #结果不是很理解
iv.num(credit.train,"ph_talk_time_per_0_6_cont2","target",rcontrol=rpart.control(cp=.0010)) #ph_talk_time_per_0_6_cont2
credit.train$ph_talk_time_per_0_6_cont2_bin=as.vector(cut(credit.train$ph_talk_time_per_0_6_cont2,c(-Inf,0,0.0014,Inf )))
credit.train$ph_talk_time_per_0_6_cont2_bin[is.na(credit.train$ph_talk_time_per_0_6_cont2)] = "缺失"
credit.train$ph_talk_time_per_0_6_cont2_bin[credit.train$ph_talk_time_per_0_6_cont2_bin %in% c("(0.0014, Inf]","缺失")]=paste(c("(0.0014, Inf]","缺失"),collapse = "|")
credit.train$ph_talk_time_per_0_6_cont2_bin<-factor(credit.train$ph_talk_time_per_0_6_cont2_bin,levels = c("(-Inf,0]","(0,0.0014]","(0.0014, Inf]|缺失"))
table(credit.train$ph_talk_time_per_0_6_cont2_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("ph_talk_time_per_0_6_cont2_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("ph_talk_time_per_0_6_cont2_bin"),summary=FALSE))

credit.test$ph_talk_time_per_0_6_cont2_bin=as.vector(cut(credit.test$ph_talk_time_per_0_6_cont2,c(-Inf,0,0.0014,Inf )))
credit.test$ph_talk_time_per_0_6_cont2_bin[is.na(credit.test$ph_talk_time_per_0_6_cont2)] = "缺失"
credit.test$ph_talk_time_per_0_6_cont2_bin[credit.test$ph_talk_time_per_0_6_cont2_bin %in% c("(0.0014, Inf]","缺失")]=paste(c("(0.0014, Inf]","缺失"),collapse = "|")
credit.test$ph_talk_time_per_0_6_cont2_bin<-factor(credit.test$ph_talk_time_per_0_6_cont2_bin,levels = c("(-Inf,0]","(0,0.0014]","(0.0014, Inf]|缺失"))
table(credit.test$ph_talk_time_per_0_6_cont2_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("ph_talk_time_per_0_6_cont2_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("ph_talk_time_per_0_6_cont2_bin"),summary=FALSE))

#ph_talk_time_per_12_18_cont2
summary(credit.train$ph_talk_time_per_12_18_cont2)
iv.num(credit.train,"ph_talk_time_per_12_18_cont2","target",rcontrol=rpart.control(cp=.0010)) #ph_talk_time_per_12_18_cont2
credit.train$ph_talk_time_per_12_18_cont2_bin=as.vector(cut(credit.train$ph_talk_time_per_12_18_cont2,c(-Inf,0,Inf )))
credit.train$ph_talk_time_per_12_18_cont2_bin[is.na(credit.train$ph_talk_time_per_12_18_cont2)] = "缺失"
credit.train$ph_talk_time_per_12_18_cont2_bin[credit.train$ph_talk_time_per_12_18_cont2_bin %in% c("(-Inf,0]","缺失")]=paste(c("(-Inf,0]","缺失"),collapse = "|")
credit.train$ph_talk_time_per_12_18_cont2_bin<-factor(credit.train$ph_talk_time_per_12_18_cont2_bin,levels = c("(-Inf,0]|缺失","(0, Inf]"))
table(credit.train$ph_talk_time_per_12_18_cont2_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("ph_talk_time_per_12_18_cont2_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("ph_talk_time_per_12_18_cont2_bin"),summary=FALSE))

credit.test$ph_talk_time_per_12_18_cont2_bin=as.vector(cut(credit.test$ph_talk_time_per_12_18_cont2,c(-Inf,0,Inf )))
credit.test$ph_talk_time_per_12_18_cont2_bin[is.na(credit.test$ph_talk_time_per_12_18_cont2)] = "缺失"
credit.test$ph_talk_time_per_12_18_cont2_bin[credit.test$ph_talk_time_per_12_18_cont2_bin %in% c("(-Inf,0]","缺失")]=paste(c("(-Inf,0]","缺失"),collapse = "|")
credit.test$ph_talk_time_per_12_18_cont2_bin<-factor(credit.test$ph_talk_time_per_12_18_cont2_bin,levels = c("(-Inf,0]|缺失","(0, Inf]"))
table(credit.test$ph_talk_time_per_12_18_cont2_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("ph_talk_time_per_12_18_cont2_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("ph_talk_time_per_12_18_cont2_bin"),summary=FALSE))

#ph_talk_time_per_18_24_cont1
summary(credit.train$ph_talk_time_per_18_24_cont1)
iv.num(credit.train,"ph_talk_time_per_18_24_cont1","target",rcontrol=rpart.control(cp=.0012)) #ph_talk_time_per_18_24_cont1
credit.train$ph_talk_time_per_18_24_cont1_bin=as.vector(cut(credit.train$ph_talk_time_per_18_24_cont1,c(-Inf,0.0016,0.088,Inf )))
credit.train$ph_talk_time_per_18_24_cont1_bin[is.na(credit.train$ph_talk_time_per_18_24_cont1)] = "缺失"
credit.train$ph_talk_time_per_18_24_cont1_bin[credit.train$ph_talk_time_per_18_24_cont1_bin %in% c("(-Inf,0.0016]","缺失")]=paste(c("(-Inf,0.0016]","缺失"),collapse = "|")
credit.train$ph_talk_time_per_18_24_cont1_bin<-factor(credit.train$ph_talk_time_per_18_24_cont1_bin,levels = c("(-Inf,0.0016]|缺失","(0.0016,0.088]","(0.088, Inf]"))
table(credit.train$ph_talk_time_per_18_24_cont1_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("ph_talk_time_per_18_24_cont1_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("ph_talk_time_per_18_24_cont1_bin"),summary=FALSE))

credit.test$ph_talk_time_per_18_24_cont1_bin=as.vector(cut(credit.test$ph_talk_time_per_18_24_cont1,c(-Inf,0.0016,0.088,Inf )))
credit.test$ph_talk_time_per_18_24_cont1_bin[is.na(credit.test$ph_talk_time_per_18_24_cont1)] = "缺失"
credit.test$ph_talk_time_per_18_24_cont1_bin[credit.test$ph_talk_time_per_18_24_cont1_bin %in% c("(-Inf,0.0016]","缺失")]=paste(c("(-Inf,0.0016]","缺失"),collapse = "|")
credit.test$ph_talk_time_per_18_24_cont1_bin<-factor(credit.test$ph_talk_time_per_18_24_cont1_bin,levels = c("(-Inf,0.0016]|缺失","(0.0016,0.088]","(0.088, Inf]"))
table(credit.test$ph_talk_time_per_18_24_cont1_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("ph_talk_time_per_18_24_cont1_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("ph_talk_time_per_18_24_cont1_bin"),summary=FALSE))

#xd_call_cnt_1w
summary(credit.train$xd_call_cnt_1w);
iv.num(credit.train,"xd_call_cnt_1w","target",rcontrol=rpart.control(cp=.0016)) #xd_call_cnt_1w
credit.train$xd_call_cnt_1w_bin=cut(credit.train$xd_call_cnt_1w,c(-Inf,30,40,60,Inf ))
table(credit.train$xd_call_cnt_1w_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("xd_call_cnt_1w_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("xd_call_cnt_1w_bin"),summary=FALSE))

credit.test$xd_call_cnt_1w_bin=cut(credit.test$xd_call_cnt_1w,c(-Inf,30,40,60,Inf ))
table(credit.test$xd_call_cnt_1w_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("xd_call_cnt_1w_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("xd_call_cnt_1w_bin"),summary=FALSE))

#xd_call_cnt_3m
summary(credit.train$xd_call_cnt_3m);
iv.num(credit.train,"xd_call_cnt_3m","target",rcontrol=rpart.control(cp=.0016)) #xd_call_cnt_3m
credit.train$xd_call_cnt_3m_bin=as.vector(cut(credit.train$xd_call_cnt_3m,c(-Inf,200,400,700,Inf )))
credit.train$xd_call_cnt_3m_bin[credit.train$xd_call_cnt_3m_bin %in% c("(-Inf,200]","(700, Inf]")]=paste(c("(-Inf,200]","(700, Inf]"),collapse = "|")
table(credit.train$xd_call_cnt_3m_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("xd_call_cnt_3m_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("xd_call_cnt_3m_bin"),summary=FALSE))

credit.test$xd_call_cnt_3m_bin=as.vector(cut(credit.test$xd_call_cnt_3m,c(-Inf,200,400,700,Inf )))
credit.test$xd_call_cnt_3m_bin[credit.test$xd_call_cnt_3m_bin %in% c("(-Inf,200]","(700, Inf]")]=paste(c("(-Inf,200]","(700, Inf]"),collapse = "|")
table(credit.test$xd_call_cnt_3m_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("xd_call_cnt_3m_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("xd_call_cnt_3m_bin"),summary=FALSE))

#xd_call_in_cnt
summary(credit.train$xd_call_in_cnt);
iv.num(credit.train,"xd_call_in_cnt","target",rcontrol=rpart.control(cp=.0012)) #xd_call_in_cnt
credit.train$xd_call_in_cnt_bin=cut(credit.train$xd_call_in_cnt,c(-Inf,200,625,Inf ))
table(credit.train$xd_call_in_cnt_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("xd_call_in_cnt_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("xd_call_in_cnt_bin"),summary=FALSE))

credit.test$xd_call_in_cnt_bin=cut(credit.test$xd_call_in_cnt,c(-Inf,200,625,Inf ))
table(credit.test$xd_call_in_cnt_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("xd_call_in_cnt_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("xd_call_in_cnt_bin"),summary=FALSE))


#xd_call_out_len_ave_d
summary(credit.train$xd_call_out_len_ave_d); 
iv.num(credit.train,"xd_call_out_len_ave_d","target",rcontrol=rpart.control(cp=.0016)) #xd_call_out_len_ave_d
credit.train$xd_call_out_len_ave_d_bin=cut(credit.train$xd_call_out_len_ave_d,c(-Inf,250,460,Inf ))
table(credit.train$xd_call_out_len_ave_d_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("xd_call_out_len_ave_d_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("xd_call_out_len_ave_d_bin"),summary=FALSE))

credit.test$xd_call_out_len_ave_d_bin=cut(credit.test$xd_call_out_len_ave_d,c(-Inf,250,460,Inf ))
table(credit.test$xd_call_out_len_ave_d_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("xd_call_out_len_ave_d_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("xd_call_out_len_ave_d_bin"),summary=FALSE))

#xd_in_subtotal_ave_d
summary(credit.train$xd_in_subtotal_ave_d);
iv.num(credit.train,"xd_in_subtotal_ave_d","target",rcontrol=rpart.control(cp=.0010)) #xd_in_subtotal_ave_d
credit.train$xd_in_subtotal_ave_d_bin=cut(credit.train$xd_in_subtotal_ave_d,c(-Inf,0,Inf ))
table(credit.train$xd_in_subtotal_ave_d_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("xd_in_subtotal_ave_d_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("xd_in_subtotal_ave_d_bin"),summary=FALSE))

credit.test$xd_in_subtotal_ave_d_bin=cut(credit.test$xd_in_subtotal_ave_d,c(-Inf,0,Inf ))
table(credit.test$xd_in_subtotal_ave_d_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("xd_in_subtotal_ave_d_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("xd_in_subtotal_ave_d_bin"),summary=FALSE))

#xd_last_day
summary(credit.train$xd_last_day);#不合理 去除
iv.num(credit.train,"xd_last_day","target",rcontrol=rpart.control(cp=.0016)) #xd_last_day
credit.train$xd_last_day_bin=cut(credit.train$xd_last_day,c(-Inf,5,Inf ))
table(credit.train$xd_last_day_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("xd_last_day_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("xd_last_day_bin"),summary=FALSE))

credit.test$xd_last_day_bin=cut(credit.test$xd_last_day,c(-Inf,5,Inf ))
table(credit.test$xd_last_day_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("xd_last_day_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("xd_last_day_bin"),summary=FALSE))

#xd_mobile_cnt c(-Inf,100,130,200,Inf )
summary(credit.train$xd_mobile_cnt);
iv.num(credit.train,"xd_mobile_cnt","target",rcontrol=rpart.control(cp=.0010)) #xd_mobile_cnt
credit.train$xd_mobile_cnt_bin=cut(credit.train$xd_mobile_cnt,c(-Inf,100,200,Inf ))
table(credit.train$xd_mobile_cnt_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("xd_mobile_cnt_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("xd_mobile_cnt_bin"),summary=FALSE))

credit.test$xd_mobile_cnt_bin=cut(credit.test$xd_mobile_cnt,c(-Inf,100,200,Inf ))
table(credit.test$xd_mobile_cnt_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("xd_mobile_cnt_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("xd_mobile_cnt_bin"),summary=FALSE))

#xd_mobile_in_cnt  
summary(credit.train$xd_mobile_in_cnt);
iv.num(credit.train,"xd_mobile_in_cnt","target",rcontrol=rpart.control(cp=.0016)) #xd_mobile_in_cnt
credit.train$xd_mobile_in_cnt_bin=cut(credit.train$xd_mobile_in_cnt,c(-Inf,80,120,150,Inf ))
table(credit.train$xd_mobile_in_cnt_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("xd_mobile_in_cnt_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("xd_mobile_in_cnt_bin"),summary=FALSE))

credit.test$xd_mobile_in_cnt_bin=cut(credit.test$xd_mobile_in_cnt,c(-Inf,80,120,150,Inf ))
table(credit.test$xd_mobile_in_cnt_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("xd_mobile_in_cnt_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("xd_mobile_in_cnt_bin"),summary=FALSE))

#xd_mobile_out_cnt_ave_d
summary(credit.train$xd_mobile_out_cnt_ave_d);
iv.num(credit.train,"xd_mobile_out_cnt_ave_d","target",rcontrol=rpart.control(cp=.0016)) #xd_mobile_out_cnt_ave_d
credit.train$xd_mobile_out_cnt_ave_d_bin=cut(credit.train$xd_mobile_out_cnt_ave_d,c(-Inf,0.006,Inf ))
table(credit.train$xd_mobile_out_cnt_ave_d_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("xd_mobile_out_cnt_ave_d_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("xd_mobile_out_cnt_ave_d_bin"),summary=FALSE))

credit.test$xd_mobile_out_cnt_ave_d_bin=cut(credit.test$xd_mobile_out_cnt_ave_d,c(-Inf,0.006,Inf ))
table(credit.test$xd_mobile_out_cnt_ave_d_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("xd_mobile_out_cnt_ave_d_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("xd_mobile_out_cnt_ave_d_bin"),summary=FALSE))

#xd_mobile_out_cnt_pct  c(-Inf,0.65,Inf )
summary(credit.train$xd_mobile_out_cnt_pct);
iv.num(credit.train,"xd_mobile_out_cnt_pct","target",rcontrol=rpart.control(cp=.0016)) #xd_mobile_out_cnt_pct
credit.train$xd_mobile_out_cnt_pct_bin=cut(credit.train$xd_mobile_out_cnt_pct,c(-Inf,0.65,Inf ))
table(credit.train$xd_mobile_out_cnt_pct_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("xd_mobile_out_cnt_pct_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("xd_mobile_out_cnt_pct_bin"),summary=FALSE))

credit.test$xd_mobile_out_cnt_pct_bin=cut(credit.test$xd_mobile_out_cnt_pct,c(-Inf,0.65,Inf ))
table(credit.test$xd_mobile_out_cnt_pct_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("xd_mobile_out_cnt_pct_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("xd_mobile_out_cnt_pct_bin"),summary=FALSE))

#xd_my_call_len_pct
summary(credit.train$xd_my_call_len_pct);
iv.num(credit.train,"xd_my_call_len_pct","target",rcontrol=rpart.control(cp=.0008)) #xd_my_call_len_pct
credit.train$xd_my_call_len_pct_bin=cut(credit.train$xd_my_call_len_pct,c(-Inf,0,0.3,Inf ))
table(credit.train$xd_my_call_len_pct_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("xd_my_call_len_pct_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("xd_my_call_len_pct_bin"),summary=FALSE))

credit.test$xd_my_call_len_pct_bin=cut(credit.test$xd_my_call_len_pct,c(-Inf,0,0.3,Inf ))
table(credit.test$xd_my_call_len_pct_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("xd_my_call_len_pct_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("xd_my_call_len_pct_bin"),summary=FALSE))

basic.stats.result.train$英文字段[which(basic.stats.result.train$type=="数值")]

#因子变量类别合并
basic.stats.result.train$英文字段[which(as.numeric(basic.stats.result.train$numOfCateg)>4 & basic.stats.result.train$type=="名义")]

#cos_sour_chan
table(credit.train$cos_sour_chan,useNA = "ifany");
credit.train$cos_sour_chan<-as.vector(credit.train$cos_sour_chan);
credit.train$cos_sour_chan[is.na(credit.train$cos_sour_chan)|credit.train$cos_sour_chan==""] = "缺失"
iv.mult(credit.train,"target",vars=c("cos_sour_chan"))
credit.train$cos_sour_chan_bin[credit.train$cos_sour_chan %in% c("ANDROID","微信","缺失")]=paste(c("ANDROID","微信","缺失"),collapse = "|")
credit.train$cos_sour_chan_bin[credit.train$cos_sour_chan %in% c("IOS","第三方","PC")]=paste(c("IOS","第三方","PC"),collapse = "|")
table(credit.train$cos_sour_chan_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("cos_sour_chan_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("cos_sour_chan_bin"),summary=FALSE))

table(credit.test$cos_sour_chan,useNA = "ifany");
credit.test$cos_sour_chan<-as.vector(credit.test$cos_sour_chan);
credit.test$cos_sour_chan[is.na(credit.test$cos_sour_chan)|credit.test$cos_sour_chan==""] = "缺失"
iv.mult(credit.test,"target",vars=c("cos_sour_chan"))
credit.test$cos_sour_chan_bin[credit.test$cos_sour_chan %in% c("ANDROID","微信","缺失")]=paste(c("ANDROID","微信","缺失"),collapse = "|")
credit.test$cos_sour_chan_bin[credit.test$cos_sour_chan %in% c("IOS","第三方","PC")]=paste(c("IOS","第三方","PC"),collapse = "|")
table(credit.test$cos_sour_chan_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("cos_sour_chan_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("cos_sour_chan_bin"),summary=FALSE))

#education_level
table(credit.train$education_level,useNA = "ifany");
credit.train$education_level<-as.vector(credit.train$education_level);
iv.mult(credit.train,"target",vars=c("education_level"))
credit.train$education_level_bin[credit.train$education_level %in% c("本科","博士及以上","硕士")]=paste(c("本科","博士及以上","硕士"),collapse = "|")
credit.train$education_level_bin[credit.train$education_level %in% c("其他","大专")]=paste(c("其他","大专"),collapse = "|")
credit.train$education_level_bin[credit.train$education_level %in% c("高中")]=paste(c("高中"),collapse = "|")
table(credit.train$education_level_bin)/nrow(credit.train)
iv.mult(credit.train,"target",vars=c("education_level_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("education_level_bin"),summary=FALSE))

table(credit.test$education_level,useNA = "ifany");
credit.test$education_level<-as.vector(credit.test$education_level);
iv.mult(credit.test,"target",vars=c("education_level"))
credit.test$education_level_bin[credit.test$education_level %in% c("本科","博士及以上","硕士")]=paste(c("本科","博士及以上","硕士"),collapse = "|")
credit.test$education_level_bin[credit.test$education_level %in% c("其他","大专")]=paste(c("其他","大专"),collapse = "|")
credit.test$education_level_bin[credit.test$education_level %in% c("高中")]=paste(c("高中"),collapse = "|")
table(credit.test$education_level_bin)/nrow(credit.test)
iv.mult(credit.test,"target",vars=c("education_level_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("education_level_bin"),summary=FALSE))

#industry
table(credit.train$industry,useNA = "ifany");
credit.train$industry<-as.vector(credit.train$industry);
iv.mult(credit.train,"target",vars=c("industry"))
credit.train$industry_bin[credit.train$industry %in% c("法律/教育/翻译/出版","财会/金融/保险","其他","销售/客服/采购/淘宝")]=paste(c("法律/教育/翻译/出版","财会/金融/保险","其他","销售/客服/采购/淘宝"),collapse = "|")
credit.train$industry_bin[credit.train$industry %in% c("网络/通信/电子","人力/行政/管理","医疗/制药/环保","生活/服务业")]=paste(c("网络/通信/电子","人力/行政/管理","医疗/制药/环保","生活/服务业"),collapse = "|")
credit.train$industry_bin[credit.train$industry %in% c("市场/媒介/广告/设计","生产/物流/质控/汽车")]=paste(c("市场/媒介/广告/设计","生产/物流/质控/汽车"),collapse = "|")

table(credit.train$industry_bin)/nrow(credit.train);
iv.mult(credit.train,"target",vars=c("industry_bin"))
iv.plot.woe(iv.mult(credit.train,"target",vars=c("industry_bin"),summary=FALSE))

table(credit.test$industry,useNA = "ifany");
credit.test$industry<-as.vector(credit.test$industry);
iv.mult(credit.test,"target",vars=c("industry"))
credit.test$industry_bin[credit.test$industry %in% c("法律/教育/翻译/出版","财会/金融/保险","其他","销售/客服/采购/淘宝")]=paste(c("法律/教育/翻译/出版","财会/金融/保险","其他","销售/客服/采购/淘宝"),collapse = "|")
credit.test$industry_bin[credit.test$industry %in% c("网络/通信/电子","人力/行政/管理","医疗/制药/环保","生活/服务业")]=paste(c("网络/通信/电子","人力/行政/管理","医疗/制药/环保","生活/服务业"),collapse = "|")
credit.test$industry_bin[credit.test$industry %in% c("市场/媒介/广告/设计","生产/物流/质控/汽车")]=paste(c("市场/媒介/广告/设计","生产/物流/质控/汽车"),collapse = "|")

table(credit.test$industry_bin)/nrow(credit.test);
iv.mult(credit.test,"target",vars=c("industry_bin"))
iv.plot.woe(iv.mult(credit.test,"target",vars=c("industry_bin"),summary=FALSE))
#province 样本量少，波动大，放弃该变量
table(credit.train$province,useNA = "ifany");
credit.train$province<-as.vector(credit.train$province);
iv.mult(credit.train,"target",vars=c("province"))

name.remove1<-basic.stats.result.train$英文字段[which(as.numeric(basic.stats.result.train$numOfCateg)>4 & basic.stats.result.train$type=="名义")]
name.remove2<-basic.stats.result.train$英文字段[which(basic.stats.result.train$type=="数值")]
name.remove<-intersect(c(name.remove1,name.remove2),names(credit.train)[-which(names(credit.train) %in% c("target","apply_id_"))])

credit.train.temp<-credit.train[-which(names(credit.train) %in% name.remove)]
credit.test.temp<-credit.test[which(names(credit.test) %in% names(credit.train.temp))]

basic.stats.result.temp <- as.data.frame(t(sapply(credit.train.temp, basic.stats)),stringsAsFactors=FALSE);basic.stats.result.temp<-cbind(basic.stats.result.temp,name=row.names(basic.stats.result.temp))

for (k in 1:length(credit.train.temp)) {
  credit.train.temp[,k]=as.vector(credit.train.temp[,k])
  credit.train.temp[,k][is.na(credit.train.temp[,k])]="缺失"
  credit.train.temp[,k]=as.factor(credit.train.temp[,k])
}
for (k in 1:length(credit.test.temp)) {
  credit.test.temp[,k]=as.vector(credit.test.temp[,k])
  credit.test.temp[,k][is.na(credit.test.temp[,k])]="缺失"
  credit.test.temp[,k]=as.factor(credit.test.temp[,k])
}


#合并上面没有合并的少数类或少数缺失类，这里没有

credit.train.temp1<-credit.train.temp[-which(names(credit.train.temp) %in% "apply_id_")]
credit.test.temp1<-credit.test.temp[-which(names(credit.test.temp) %in% "apply_id_")]

iv.value.aftet=iv.mult(credit.train.temp1,"target",TRUE)
iv.plot.summary(iv.value.aftet)

#WOE转换
credit.train.after <- iv.replace.woe(credit.train.temp1,iv=iv.mult(credit.train.temp1,"target"))
credit.test.after <- iv.replace.woe(credit.test.temp1,iv=iv.mult(credit.train.temp1,"target"))

credit.train.woe=data.frame(credit.train.after[63:123],credit.train.after$target)
credit.test.woe=data.frame(credit.test.after[63:123],credit.test.after$target)

names(credit.train.woe)[62]="target";names(credit.test.woe)[62]="target";
credit.train.woe$target<-as.factor(credit.train.woe$target);credit.test.woe$target<-as.factor(credit.test.woe$target);
write.csv(credit.train.woe,file = "DataOutputJSD/credit.train.woe.csv")
write.csv(credit.train.after,file = "DataOutputJSD/credit.train.after.csv")
write.csv(credit.test.woe,file = "DataOutputJSD/credit.test.woe.csv")
write.csv(credit.test.after,file = "DataOutputJSD/credit.test.after.csv")
# 读取数据字典,添加标签
for (i in 1:length(credit.train.after)) {label(credit.train.after[,i])<-label.add(names(credit.train.after)[i],credit.dd=credit.dd,EngName="英文字段",ChineName="中文解释")}
for (i in 1:length(credit.train.woe)) {label(credit.train.woe[,i])<-label.add(names(credit.train.woe)[i],credit.dd=credit.dd,EngName="英文字段",ChineName="中文解释")}

iv.value.woe <- data.frame(sapply(credit.train.woe[-which(names(credit.train.woe) %in% c("target"))],var.iv,target=credit.train.woe$target))  #计算变量的IV值
iv.value.woe<-data.frame(cbind(row.names(iv.value.woe),iv.value.woe))
names(iv.value.woe)<-c("name","iv.value.woe")
write.csv(iv.value.woe,file = "DataOutputJSD/iv.value.woe.csv",row.names = FALSE ) #保存数据集

names.remove.iv.woe<-as.vector(iv.value.woe[iv.value.woe$iv.value.woe<0.02,]$name)
names.remove.result.woe <-intersect(names.remove.iv.woe,names(credit.train.woe)[-which(names(credit.train.woe) %in% c("target","apply_id_","xd_my_call_len_pct_bin_woe"))])
if(length(names(credit.train.woe)[names(credit.train.woe) %in% names.remove.result.woe])>0){credit.train.woe<-credit.train.woe[-which(names(credit.train.woe) %in% names.remove.result.woe)]}

#共线性 无问题
cor.value.woe.cb=cor(credit.train.woe[-which(names(credit.train.woe) %in% "target")])
write.csv(cor.value.woe.cb,file = "DataOutputJSD/cor.value.woe.cb.csv") #模型变量概况保存为csv格式
kappa(cor.value.woe.cb, exact=TRUE)
modeltemp = glm(target ~ . ,data=credit.train.woe, family=binomial)
vif(modeltemp)

###################################################### IV. Model Development  #############################################################
#"call_in_len_avg_bin_woe",
#模型1  d180_call_contacts_count_bin_woe "xd_call_cnt_3m_bin_woe","d30_call_total_times_bin_woe","d30_call_total_time_bin_woe",
name.model.remove1=c("ph_talk_obj_per_0_6_bin_woe","xd_call_out_len_ave_d_bin_woe","call_time_std_mth5_bin_woe","avg_call_in_len_bin_woe","d150_d3_nocall_times_bin_woe","call_len_avg_bin_woe","xd_call_in_cnt_bin_woe","d180_max_nocall_days_bin_woe","xd_mobile_out_cnt_ave_d_bin_woe","xd_mobile_out_cnt_pct_bin_woe","contact_1w_avg_bin_woe","d180_call_contacts_count_bin_woe","contact1_mobile_notin_m6_call_re_bin_woe",
                     "ph_talk_time_per_0_6_cont1_bin_woe","cont_call_in_cnt_pct_bin_woe","xd_call_cnt_3m_bin_woe","xd_call_cnt_1w_bin_woe",
                     "d180_d0_nocall_days_bin_woe","cos_sour_chan_bin_woe","call_cnt_avg_bin_woe","contact_1m_avg_bin_woe","education_level_bin_woe",
                     "ph_talk_freq_per_0_6_cont1_bin_woe","ph_mean_talk_time_3mons_cont2_bin_woe","ph_talk_freq_per_6_12_bin_woe","xd_last_day_bin_woe",
                     "ph_con_day_fir_now_cont2_bin_woe","ph_talk_freq_per_6_12_cont1_bin_woe","contact_1m_avg_bin_woe","d30_call_total_times_bin_woe",
                     "ph_day_last_now_bin_woe","call_time_std_mth5","ph_talk_obj_per_18_24_bin_woe","ph_con_day_fir_now_cont1_bin_woe",
                     "early_morning_avg_bin_woe","ph_callcount_last7_28_bin_woe","industry_bin_woe","d30_d4_nocall_times_bin_woe","ph_talk_time_per_0_6_cont2_bin_woe",
                     "ph_talk_time_per_18_24_cont1_bin_woe","d90_d5_nocall_times_bin_woe","ph_cont_obj_effi_coin_bin_woe","ph_con_day_last_now_cont2_bin_woe",
                     "ph_talk_freq_per_12_18_bin_woe","ph_talk_freq_per_12_18_bin_woe","xd_mobile_cnt_bin_woe","call_in_cnt_std_mth3_bin_woe","call_cnt_std_mth5_bin_woe"
)

credit.train.woe1<-credit.train.woe[which(names(credit.train.woe) %in% name.model.temp)]
if(length(names(credit.train.woe)[names(credit.train.woe) %in% name.model.remove1])>0){credit.train.woe1<-credit.train.woe[-which(names(credit.train.woe) %in% name.model.remove1)]} else{credit.train.woe1<-credit.train.woe}
modeltemp1 = glm(target ~ . ,data=credit.train.woe1, family=binomial)

model1 <- step(modeltemp1,direction="both")
summary(model1)


#模型预测
predict.train1 = predict(model1,type='response',newdata=credit.train.woe) #训练集预测
predict.train.class1 =as.factor(ifelse(predict.train1>0.5,'1','0'))
predict.test1 = predict(model1,type='response',newdata=credit.test.woe) #测试集预测
predict.test.class1 =as.factor(ifelse(predict.test1>0.5,'1','0'))

#最终选入模型变量的情况

model.var.value1=model.var(model1,credit.train.after,iv.value.aftet)
write.csv(model.var.value1,file = "DataOutputJSD/modelVar1.csv") #模型变量概况保存

#进入模型变量的共线性情况
cor.value1=cor(credit.train.woe[which(names(credit.train.woe) %in% names(model1$coefficients)[-1])])
write.csv(cor.value1,file = "DataOutputJSD/corValue1.csv") #模型变量概况保存为csv格式
kappa(cor.value1, exact=TRUE)
vif(model1)

##################################################### V. Scorecard Transformation ######################################################
#2个假定：（1）在某个特定的比率设定特定的逾期分值（这里{1:60}时分值为600）；
#         （2）指定比率翻番的分数（PDO)(这里为20)
#Score = A - B*log(Odds),其中Odds为违约比正常
PDO=20; Odds0=1/20; P0= 600;
B=PDO/log(2)
A= P0 + B * log(Odds0)

#输出评分卡
score.card.value1=score.card(model1,credit.train.after,A,B) #转化为评分卡
write.csv(score.card.value1,file = "DataOutputJSD/scoreCard1.csv") #评分卡保存为csv格式

#计算样本的评分，score.value是计算评分的函数，需要参数score.card.value为所用的评分卡
credit.train.after$score1=apply(credit.train.after, 1, score.value,score.card.value=score.card.value1)
credit.test.after$score1=apply(credit.test.after, 1, score.value,score.card.value=score.card.value1)
#
credit.train.score<-cbind(credit.train.temp,credit.train.after)
credit.test.score<-cbind(credit.test.temp,credit.test.after)
write.csv(credit.train.score,file = "DataOutputJSD/credit.train.score.csv") 
write.csv(credit.test.score,file = "DataOutputJSD/credit.test.score.csv") 

######################################################### VI. Model Evaluation #########################################################
#6.1训练集的模型评价
#训练集 K-S表
nrow(credit.train.after)/10 #520
ks.train.value1=ks.data(364,credit.train.after$score1,credit.train.after$target)
write.csv(ks.train.value1,file = "DataOutputJSD/ksTrain1.csv") #保存用KS表
View(ks.train.value1)
#训练集的模型评价 模型1
pred.train1=prediction(predictions = predict.train1,labels = credit.train.woe$target)
perf.train1=performance(pred.train1,measure = "tpr", x.measure =  "fpr")
plot(perf.train1, main = "ROC（开发）", col ="blue", lwd =3) #ROC曲线
abline(a=0, b=1, lwd=2, lty=2) #画参考斜线（用虚线）
perf.train.auc1=performance(pred.train1,measure = "auc")
unlist(perf.train.auc1@y.values) #结果为AUC的值

#训练集 绘制K-S曲线
tpr.train1<- performance(pred.train1,measure='tpr')@y.values[[1]]
fpr.train1 <- performance(pred.train1,measure='fpr')@y.values[[1]]
ks.train1<-(tpr.train1-fpr.train1)
depth.train1 <- performance(pred.train1,measure='rpp')@y.values[[1]]
plot(depth.train1,ks.train1,type='l',main='K-S曲线（开发）',ylab='KS值',xlab='深度(depth)',ylim=0:1,col="orange",lwd =2,col.main="orange")
lines(depth.train1,tpr.train1,type='l',col="green",lwd =2 )
lines(depth.train1,fpr.train1,type='l',col="red",lwd =2  )
kslable.train1<-paste("KS统计量:",max(ks.train1),sep="");max(ks.train1)
legend(0.3,0.2,c(kslable.train1),2:8)

#6.2测试集的模型评价 

nrow(credit.test.after)/10 #1616
ks.test.value1=ks.data(156,credit.test.after$score1,credit.test.after$target)
write.csv(ks.test.value1,file = "DataOutputJSD/ksTest1.csv") #保存

ks.test.value1_1=ks.data1(ks.train.value1$最高分,credit.test.after$score1,credit.test.after$target)
write.csv(ks.test.value1_1,file = "DataOutputJSD/ksTest1_1.csv") #保存

#测试集的模型评价 模型1
pred.test1=prediction(predictions = predict.test1,labels = credit.test.woe$target)
perf.test1=performance(pred.test1,measure = "tpr", x.measure =  "fpr")
plot(perf.test1, main = "ROC（开发）", col ="blue", lwd =3) #ROC曲线
abline(a=0, b=1, lwd=2, lty=2) #画参考斜线（用虚线）
perf.test.auc1=performance(pred.test1,measure = "auc")
unlist(perf.test.auc1@y.values) #结果为AUC的值

#测试集 绘制K-S曲线
tpr.test1<- performance(pred.test1,measure='tpr')@y.values[[1]]
fpr.test1 <- performance(pred.test1,measure='fpr')@y.values[[1]]
ks.test1<-(tpr.test1-fpr.test1)
depth.test1 <- performance(pred.test1,measure='rpp')@y.values[[1]]
plot(depth.test1,ks.test1,type='l',main='K-S曲线（开发）',ylab='KS值',xlab='深度(depth)',ylim=0:1,col="orange",lwd =2,col.main="orange")
lines(depth.test1,tpr.test1,type='l',col="green",lwd =2 )
lines(depth.test1,fpr.test1,type='l',col="red",lwd =2  )
kslable.test1<-paste("KS统计量:",max(ks.test1),sep="");max(ks.test1)
legend(0.3,0.2,c(kslable.test1),2:8)

###################################################### VII. Stability Test #######################################################
#没有测试街和验证集稳定性略过............
#7.1 稳定性检验
table(credit.train.after$score1)
table(credit.test.after$score1)

#根据上面查看的结果，分数区间切割
credit.train.after$scoreBin1=cut(credit.train.after$score1,c(-Inf,seq(from=550,to=620,by=10),Inf ))  #计算训练集分数分段
credit.test.after$scoreBin1=cut(credit.test.after$score1,c(-Inf,seq(from=550,to=620,by=10),Inf ))    #计算测试集分数分段

stability.value1=stability(credit.train.after$scoreBin1,credit.test.after$scoreBin1,credit.train.after$target);stability.value1 #输出稳定性检验结果
write.csv(stability.value1,file = "DataOutputJSD/stability1.csv") #保存用
nrow(stability.value1)
PSI1=sum(stability.value1$稳定性系数PSI);PSI1 #稳定性指标PSI

#7.2 变量稳定性检验
feature.stability.value.csi1=feature.stabilityCSI(score.card.value1,credit.train.after,credit.test.after) #变量稳定性
write.csv(feature.stability.value.csi1,file = "DataOutputJSD/feat_stabil.csi1.csv") 

feature.stability.value.psi1=feature.stabilityPSI(score.card.value1,credit.train.after,credit.test.after) #变量稳定性
write.csv(feature.stability.value.psi1,file = "DataOutputJSD/feat_stabil.psi1.csv") 

#计算单个变量的情况
var.value1=var.result(score.card.value1,credit.train.after)
write.csv(var.value1,file = "DataOutputJSD/var1.Value.csv") #保存用TABLEAU展示

