install.packages("stringi")
install.packages("tidyverse")
install.packages("reshape2")
install.packages("ClusterR")
install.packages("kernlab")
install.packages("factoextra")
install.packages("ggfortify")
install.packages("lsa")
install.packages("e1071")
library(tidyverse)
library(reshape2)
library(ggplot2)
library(ClusterR)
library(kernlab)
library(factoextra)
library(ggfortify)
library(cluster)
library(lsa)
library(e1071)

setwd("C:\\Users\\Administrator\\Desktop\\R\\Work")
data<-read.table("dataset3_d.csv",header=T, sep=",")
#head(data)
str(data)
set.seed(538) 

#####檢查資料有無missing data(N/A)  FALSES->no missing data#####
#install.packages("naniar")
#library(naniar)
#any_na(data)   

#####boxplot#####
#standardization boxplot
stand_data <- scale(data[,c(1:19)])
summary(stand_data)
boxplot(stand_data[,c(1:19)])

#####boxplot#####
sapply(data, summary) 
boxplot(data[,c(1:19)])  

#####factor#####
data$Target <- factor(data$Target, labels = c("Dropout", "Graduate", "Enrolled"))
data$Application_mode  <- factor(data$Application_mode , labels = c("1st_phase", "2nd_phase", "Over_23_years_old", "Other"))
data$Course  <- factor(data$Course , labels = c("Arts", "Sciences", "Other"))
data$Mother_qualification  <- factor(data$Mother_qualification , labels = c("Secondary_education", "Higher_Education", "Administration", "Other"))
data$Father_qualification  <- factor(data$Father_qualification , labels = c("Secondary_education", "Higher_Education", "Administration", "Other"))
data$Mother_occupation  <- factor(data$Mother_occupation , labels = c("Scholars", "farmers", "Workers", "Businessmans", "Military", "Other"))
data$Father_occupation  <- factor(data$Father_occupation , labels = c("Scholars", "farmers", "Workers", "Businessmans", "Military", "Other"))
data$Displaced <- factor(data$Displaced, labels = c("no", "yes"))
data$Gender <- factor(data$Gender, labels = c("female", "male"))
data$Scholarship_holder <- factor(data$Scholarship_holder, labels = c("no", "yes"))
Target <- data[,1]
str(data)

#####barchart#####
Scholarship_holder.tab <- table(data$Scholarship_holder )
Scholarship_holder.tab
Target.tab <- table(data$Target)
Target.tab
prop.table(Scholarship_holder.tab)
round(prop.table(Scholarship_holder.tab), 3)
barplot(Scholarship_holder.tab)
prop.table(Target.tab)
round(prop.table(Target.tab), 3)
barplot(Target.tab)
#combine two barchart 
twoway.tab <- table(data$Target, data$Scholarship_holder )
twoway.tab
cell.prop <- prop.table(twoway.tab, margin = NULL) 
round(cell.prop, 3)
cond_row_prop <- prop.table(twoway.tab, margin = 1) 
round(cond_row_prop, 3)
apply(cond_row_prop, 1, sum) # rows sum to 1
cond_col_prop <- prop.table(twoway.tab, margin = 2) 
round(cond_col_prop, 3)
apply(cond_col_prop, 2, sum) # cols sum to 1
barplot(twoway.tab, 
        beside = TRUE, 
        main = "Target by Scholarship_holder",
        xlab = "Scholarship_holder type")
barplot(twoway.tab,
        beside = FALSE, 
        main = "Target by Scholarship_holder",
        xlab = "Scholarship_holder type")
ggplot(data = data, aes(x = Scholarship_holder, fill = Target)) + 
  geom_bar(position=position_dodge())


#####scale var#####
new.data<-scale(data[,c(11:19)])           #用scale標準化連續型資料
boxplot(new.data)

#####dummy var#####
DummyTable<-model.matrix(~Target+
                           Application_mode+
                           Course+
                           Mother_qualification+
                           Father_qualification+
                           Mother_occupation+
                           Father_occupation+
                           Displaced+
                           Gender+
                           Scholarship_holder  
                         , data=data) 

DummyTable<-DummyTable[,-1]                #刪除intercept
combine.data<-cbind(DummyTable, new.data)  #合併連續與類別的資料

#####correlation matrix#####
CorMatrix <- combine.data %>% cor() %>% melt()
head(CorMatrix, 10)
ggplot(data = CorMatrix,aes(Var1, Var2)) +  
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "red", high = "blue",     
                       mid = "white", midpoint = 0) +  
  guides(fill=guide_legend(title="Correlation")) +     
  theme_bw() +    
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())  

#####PCA#####
pca.model<-prcomp(combine.data[,1:ncol(combine.data)], scale=T)   
names(pca.model)  
summary(pca.model)
pca.model

#####陡坡圖(Scree plot)-凱莎原則
plot(pca.model, type="line", main="Scree Plot")
abline(h=1, col="blue")     
#累積解釋比例畫成圖
cum_prop = cumsum((pca.model$sdev)^2 / sum((pca.model$sdev)^2))
plot(cum_prop, main="Cum_Prop")
abline(h=0.8, col="blue")  #combine.data20個

##### pca$rotation 主成分係數、權重
pca.model$rotation[,1:20]

#####主成分分數：由主成分的權重與標準化的資料矩陣，可得每一個個體在各個主成分的分數
top20_pca.data <- pca.model$x[, 1:20]
top20_pca.data 

####try 累積比例16個
pca.model$rotation[,1:16]
top16_pca.data <- pca.model$x[, 1:16]
top16_pca.data 
top3.pca.eigenvector <- pca.model$rotation[, 1:3]
top3.pca.eigenvector
first.pca <- top3.pca.eigenvector[, 1]   
second.pca <- top3.pca.eigenvector[, 2]  
third.pca <- top3.pca.eigenvector[, 3]   
# 第一主成份：由小到大排序原變數的係數
first.pca[order(first.pca, decreasing=FALSE)]   
# 使用dotchart，繪製主成份負荷圖 (創建點圖以顯示每個變量的係數)
dotchart(first.pca[order(first.pca, decreasing=FALSE)] ,   
         main="Loading Plot for PC1",                      
         xlab="Variable Loadings",                        
         col="red")                                        
#二元圖 1:2->PC1:PC2
biplot(pca.model, choices=1:2)  
var.exp <- tibble(
  pc = paste0("PC_", formatC(1:35, width=2, flag="0")),  
  var = pca.model$sdev^2,  #標準差平方
  prop = (pca.model$sdev)^2 / sum((pca.model$sdev)^2), 
  cum_prop = cumsum((pca.model$sdev)^2 / sum((pca.model$sdev)^2))) 
head(var.exp, 20)
head(pca.model$rotation,20)
#####Heatmap (熱圖)—主成分權重(係數)#####
ggplot(melt(pca.model$rotation[, 1:20]), aes(Var2, Var1)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "firebrick4", high = "steelblue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Coefficient")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())

#####決定群數#####
#用階層式集群分析，dist()計算距離後，hclust()進行分析及繪圖-----
#####歐式距離#####                                 
eu.distance <- dist(combine.data, method = 'euclidean')  
hclust(eu.distance, method = 'complete') %>% plot()  #最遠法
#abline(h=14, col="red") 
#abline(h=12, col="red") 
h.E.cluster <- hclust(eu.distance)
h.E.cluster3=cutree(h.E.cluster, k=4)
rect.hclust(h.E.cluster, k=4)

#####餘弦距離#####
d.cos<-(1-cosine(t(combine.data)))                       
cos.distance <- d.cos %>% as.dist #%>% hclust() %>% plot()
hclust(cos.distance, method = 'complete') %>% plot()  #最遠法
#abline(h=1.7, col="red") 
#abline(h=1.53, col="red") 
h.C.cluster <- hclust(cos.distance)
h.C.cluster3=cutree(h.C.cluster, k=4)
rect.hclust(h.C.cluster, k=4)


#####k-means-----
opt_km = Optimal_Clusters_KMeans(combine.data, max_clusters = 12, initializer = 'random',
                                 criterion = "WCSSE",plot_clusters = T) #計算組內平方和
km2 = KMeans_rcpp(combine.data, clusters = 2, num_init = 10,max_iters = 100, initializer = 'random') 
km3 = KMeans_rcpp(combine.data, clusters = 3, num_init = 10,max_iters = 100, initializer = 'random') 
km4 = KMeans_rcpp(combine.data, clusters = 4, num_init = 10,max_iters = 100, initializer = 'random') 
#組內平方和
km2$WCSS_per_cluster; sum(km2$WCSS_per_cluster)
km3$WCSS_per_cluster; sum(km3$WCSS_per_cluster)
km4$WCSS_per_cluster; sum(km4$WCSS_per_cluster)
#跟原先資料做結合
km2_out <- as.data.frame(km2$clusters) 
km3_out <- as.data.frame(km3$clusters) 
km4_out <- as.data.frame(km4$clusters) 
final2 <- cbind(Target,km2_out)
final3 <- cbind(Target,km3_out)
final4 <- cbind(Target,km4_out)
table(final2[,1],final2[,2])
table(final3[,1],final3[,2])
table(final4[,1],final4[,2])

######k-means++-----
opt_kplus = Optimal_Clusters_KMeans(combine.data, max_clusters = 12, initializer = 'kmeans++',
                                    criterion = "WCSSE",plot_clusters = T)
#KMean_rcpp() 只需更動選項initializer = 'kmeans++'
km2plus = KMeans_rcpp(combine.data, clusters = 2, num_init = 10,max_iters = 100, initializer = 'kmeans++') 
km3plus = KMeans_rcpp(combine.data, clusters = 3, num_init = 10,max_iters = 100, initializer = 'kmeans++') 
km4plus = KMeans_rcpp(combine.data, clusters = 4, num_init = 10,max_iters = 100, initializer = 'kmeans++') 
km2plus$WCSS_per_cluster; sum(km2plus$WCSS_per_cluster)
km3plus$WCSS_per_cluster; sum(km3plus$WCSS_per_cluster)
km4plus$WCSS_per_cluster; sum(km4plus$WCSS_per_cluster)
km2plus_out <- as.data.frame(km2plus$clusters) 
km3plus_out <- as.data.frame(km3plus$clusters) 
km4plus_out <- as.data.frame(km4plus$clusters) 
final2.plus <- cbind(Target,km2plus_out)
final3.plus <- cbind(Target,km3plus_out)
final4.plus <- cbind(Target,km4plus_out)
table(final2.plus[,1],final2.plus[,2])
table(final3.plus[,1],final3.plus[,2])
table(final4.plus[,1],final4.plus[,2])

#####kernel k-means-----
kkmeans2<- kkmeans(new.data, centers=2, kernel = "rbfdot",   
                   kpar = "automatic",alg="kkmeans")   
withinss(kkmeans2) 
kkmeans2
kkmeans3<- kkmeans(new.data, centers=3, kernel = "rbfdot",   
                   kpar = "automatic",alg="kkmeans")   
withinss(kkmeans3) 
sum(withinss(kkmeans3)) 
kkmeans4<- kkmeans(new.data, centers=4, kernel = "rbfdot", 
                   kpar = "automatic",alg="kkmeans")
withinss(kkmeans4) 
sum(withinss(kkmeans4))

#####k-means() factoextra-----
kmeans.cluster <- kmeans(new.data, centers=3) #連續資料
names(kmeans.cluster)
# 分群結果
kmeans.cluster$cluster
# 群內的變異數
kmeans.cluster$withinss
# 分群結果和實際結果比較
table(Target,kmeans.cluster$cluster)  
fviz_cluster(kmeans.cluster,           # 分群結果
             data = new.data,          # 資料
             geom = c("point","text"), # 點和標籤(point & label)
             frame.type = "norm")      # 框架型態
fviz_nbclust(new.data, FUNcluster=kmeans, method="wss", k.max=10) #Elbow Method
#####Kmeans Algorithm-----
K <- kmeans(combine.data,3) 
K          #Clustering vector記錄每筆資料分群類別
names(K)   #方便擷取output的值 -> K$cluster  K$centers不斷(迭代)找組中點(收斂)，點數不會變!
ClusterResult <- cbind( combine.data,K$cluster) %>% as.data.frame()
head(ClusterResult) #多一個column:v36，分類狀況
colnames(ClusterResult)[ncol(ClusterResult)] <- 'Cluster' #將cluster(v36)的欄位命名為cluster
head(ClusterResult) 
table(ClusterResult$Cluster)

str(ClusterResult)
#gather把連續變數整理成dummy var形式，並整合成一個column(扣除類別變數)
ClusterResultForPlot <-gather( ClusterResult, key = Continuous_Variable, value = Normalized_Value, 
                               -c(TargetGraduate, TargetEnrolled, 
                                  Application_mode2nd_phase, Application_modeOver_23_years_old, Application_modeOther,
                                  CourseSciences, CourseOther,
                                  Mother_qualificationHigher_Education, Mother_qualificationAdministration, Mother_qualificationOther,
                                  Father_qualificationHigher_Education, Father_qualificationAdministration, Father_qualificationOther,
                                  Mother_occupationfarmers, Mother_occupationWorkers, Mother_occupationBusinessmans, Mother_occupationMilitary, Mother_occupationOther,
                                  Father_occupationfarmers, Father_occupationWorkers, Father_occupationBusinessmans, Father_occupationMilitary, Father_occupationOther,
                                  Displacedyes, Gendermale, Scholarship_holderyes,
                                  Cluster))  
head(ClusterResultForPlot)
#連續變數名稱轉換為factor資料
ClusterResultForPlot$Continuous_Variable<-as.factor( ClusterResultForPlot$Continuous_Variable)
str(ClusterResultForPlot)
factor( ClusterResultForPlot$Continuous_Variable,levels = c('Application_order','Age_at_enrollment',
                                                            'X1st_enrolled','X1st_evaluations','X1st_approved','X1st_grade',
                                                            'Unemployment_rate','Inflation_rate','GDP'))
str(ClusterResultForPlot)
ggplot( data = ClusterResultForPlot) + 
  geom_boxplot( aes( x=Continuous_Variable, y=Normalized_Value),size = 0.7) +
  facet_wrap( ~ Cluster) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())

#####用factoextra做分群各變量分布狀況(分的較好)#####
kmeansF.cluster <- kmeans(new.data, centers=3) 
names(kmeansF.cluster)
ClusterResultF <- cbind( combine.data,kmeansF.cluster$cluster) %>% as.data.frame()
colnames(ClusterResultF)[ncol(ClusterResultF)] <- 'Cluster' 
table(ClusterResultF$Cluster)
str(ClusterResultF)
ClusterResultForPlotF <-gather( ClusterResultF, key = Continuous_Variable, value = Normalized_Value, 
                                  -c(TargetGraduate, TargetEnrolled, 
                                     Application_mode2nd_phase, Application_modeOver_23_years_old, Application_modeOther,
                                     CourseSciences, CourseOther,
                                     Mother_qualificationHigher_Education, Mother_qualificationAdministration, Mother_qualificationOther,
                                     Father_qualificationHigher_Education, Father_qualificationAdministration, Father_qualificationOther,
                                     Mother_occupationfarmers, Mother_occupationWorkers, Mother_occupationBusinessmans, Mother_occupationMilitary, Mother_occupationOther,
                                     Father_occupationfarmers, Father_occupationWorkers, Father_occupationBusinessmans, Father_occupationMilitary, Father_occupationOther,
                                     Displacedyes, Gendermale, Scholarship_holderyes,
                                     Cluster)) 
ClusterResultForPlotF$Continuous_Variable<-as.factor( ClusterResultForPlotF$Continuous_Variable)
str(ClusterResultForPlotF)
factor( ClusterResultForPlotF$Continuous_Variable,levels = c('Application_order','Age_at_enrollment',
                                                            'X1st_enrolled','X1st_evaluations','X1st_approved','X1st_grade',
                                                            'Unemployment_rate','Inflation_rate','GDP'))
str(ClusterResultForPlotF)
#連續
ggplot( data = ClusterResultForPlotF) + 
  geom_boxplot( aes( x=Continuous_Variable, y=Normalized_Value),size = 0.7) +
  facet_wrap( ~ Cluster) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
#類別
#將分群後的結果合併到原始資料內看出原始資料在不同群的狀況
c.data<-cbind(data, kmeansF.cluster$cluster)
ggplot( data = c.data) + geom_bar( aes( x = Scholarship_holder ,fill = Target)) + facet_wrap( ~kmeansF.cluster$cluster) #+
                      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

#####納入主成分分析視覺化分群結果#####
autoplot(kmeans(combine.data[,27:35], 2), data=combine.data) #分得較好  PCA1&&PCA2
autoplot(kmeans(combine.data[,27:35], 3), data=combine.data) 
autoplot(kmeans(combine.data[,27:35], 4), data=combine.data) #分得較差
autoplot(kmeans(combine.data, 3), data=combine.data)         #分得較差

#####SVM#####
###固定seed=538###
set.seed(538) 
index=sample(1:nrow(data), as.integer(0.7*nrow(data)))  #分70%做traning data，as取整數
#index=sample(1:nrow(data), as.integer(0.9*nrow(data)))  
training=data[index,]    #指定index為traning
testing=data[-index,]    #全部data-index為testing
svmModel=svm(Target ~., data=training) #用svm()來執行svm、分類，~.為全部x
summary(svmModel)

pred.train=predict(svmModel, training) 
(tb=table(pred=pred.train, real=training$Target))   #training error
(acc=sum(diag(tb)/sum(tb)))                         #accuracy
pred.test=predict(svmModel, testing)
(tb.t=table(pred=pred.test, real=testing$Target))   #testing error
(acc.t=sum(diag(tb.t)/sum(tb.t)))                   #accuracy

###kernel svm###
## Linear kernel: cost
#linker=svm(Target ~., data=training, kernel="linear", cost=10, 55)
linker=svm(Target ~., data=training, kernel="linear", cost=10, type='C-classification')
summary(linker)

svmlinker = summary(linker)
svmlinker$gamma

pred.test=predict(linker,testing)
(tb.lin=table(pred=pred.test,real=testing$Target))    #testing error
(acc.t=sum(diag(tb.lin)/sum(tb.lin)))#accuracy
## Polynomial kernel: cost, degree, gamma, coef0
#polyker=svm(Target ~., data=training, kernel="polynomial",cost=10, degree=4, gamma=0.5, coef0=0, scale=F)
polyker=svm(Target ~., data=training, kernel="polynomial",cost=10, type='C-classification')
summary(polyker)

svmpolyker = summary(polyker)
svmpolyker$gamma

pred.test=predict(polyker,testing)
(tb.poly=table(pred=pred.test, real=testing$Target))  #testing error
(acc.t=sum(diag(tb.poly))/sum(tb.poly))#accuracy
## radial kernel: cost, gamma
#radker=svm(Target~., data = training, kernel="radial", cost=10, gamma=0.5, scale=F)
radker=svm(Target~., data=training, kernel="radial", cost=10, type='C-classification')
summary(radker)

svmradker = summary(radker)
svmradker$gamma

pred.test=predict(radker,testing)
(tb.rad=table(pred=pred.test, real=testing$Target))   #testing error
(acc.t=sum(diag(tb.rad))/sum(tb.rad))#accuracy
######

Costlist=c(1 : 1000) / 1000
Gammalist=c(0.1 : 10)
tune.out=tune(svm, Target~., data=training, kernel="radial", 
              range=list(cost=Costlist, gamma=Gammalist))
summary(tune.out)
bestmod=tune.out$best.model 
summary(bestmod)
predict(bestmod, newdata=testing, type=Target)
pred.best=predict(bestmod, newdata=testing, type="Target")
(tb.best=table(pred=pred.best, real=testing$Target))  #testing error
(acc.best=sum(diag(tb.best)/sum(tb.best)))            #accuracy
plot(tune.out)


c = 0.001
maxC = 0
resultAcc = 0
while(c < 1){   
  radker=svm(Target~., data=training, kernel="radial", cost=c, type='C-classification')
  #summary(radker)
  pred.test=predict(radker,testing)
  (tb.rad=table(pred=pred.test, real=testing$Target))   #testing error
  (acc.t=sum(diag(tb.rad))/sum(tb.rad))#accuracy
  if(acc.t > resultAcc){
    resultAcc = acc.t
    maxC = c
  }
  c = c + 0.001
}
resultAcc
maxC
