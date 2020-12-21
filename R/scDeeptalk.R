# training:test 8:2
library(h2o)
load('data/example_data.RData')
h2o.init()
randomseed<- 1
ttratio<- 0.8
sample_p_res<- readRDS('sample_p_res.rds')
sample_n_res<- readRDS('sample_n_res.rds')
ndata_pca<- readRDS('ndata_pca_500.rds')
npc<- 500
hidden<- c(200,200)

# pairs 50%
con_ratio<- 0.5

sample_p_train_res<- list()
sample_p_test_res<- list()
sample_n_train_res<- list()
sample_n_test_res<- list()
for (i in 1:5) {
  sample_p<- sample_p_res[[i]]
  sample_p<- sample_p[order(-sample_p$expressed_pair),]
  sample_n<- sample_p[(floor(con_ratio*nrow(sample_p))+1):nrow(sample_p),]
  sample_p<- sample_p[1:floor(con_ratio*nrow(sample_p)),]
  #p
  set.seed(randomseed)
  sam<- 1:nrow(sample_p)
  sam_train<- sample(x = 1:nrow(sample_p),size = floor(ttratio*nrow(sample_p)),replace = F)
  sam_test<- sam[!sam %in% sam_train]
  rownames(sample_p)<- 1:nrow(sample_p)
  sample_p_train<- sample_p[sam_train,]
  sample_p_test<- sample_p[sam_test,]
  #n
  set.seed(randomseed)
  sam<- 1:nrow(sample_n)
  sam_train<- sample(x = 1:nrow(sample_n),size = floor(ttratio*nrow(sample_n)),replace = F)
  sam_test<- sam[!sam %in% sam_train]
  rownames(sample_n)<- 1:nrow(sample_n)
  sample_n_train<- sample_n[sam_train,]
  sample_n_test<- sample_n[sam_test,]
  sample_p_train_res[[i]]<- sample_p_train
  sample_p_test_res[[i]]<- sample_p_test
  sample_n_train_res[[i]]<- sample_n_train
  sample_n_test_res[[i]]<- sample_n_test
  
}

# sample_p_res[[4]] -- sample 5%
sample_p_train<- sample_p_train_res[[4]]
sample_p_test<- sample_p_test_res[[4]]
sample_n_train<- sample_n_train_res[[4]]
sample_n_test<- sample_n_test_res[[4]]

set.seed(randomseed)
sam<- sample(x = 1:nrow(sample_p_train),size = floor(0.05*nrow(sample_p_train)),replace = F)
rownames(sample_p_train)<- 1:nrow(sample_p_train)
sample_p_train<- sample_p_train[sam,]
sample_p_train_res[[4]]<- sample_p_train

set.seed(randomseed)
sam<- sample(x = 1:nrow(sample_p_test),size = floor(0.05*nrow(sample_p_test)),replace = F)
rownames(sample_p_test)<- 1:nrow(sample_p_test)
sample_p_test<- sample_p_test[sam,]
sample_p_test_res[[4]]<- sample_p_test

set.seed(randomseed)
sam<- sample(x = 1:nrow(sample_n_train),size = floor(0.05*nrow(sample_n_train)),replace = F)
rownames(sample_n_train)<- 1:nrow(sample_n_train)
sample_n_train<- sample_n_train[sam,]
sample_n_train_res[[4]]<- sample_n_train

set.seed(randomseed)
sam<- sample(x = 1:nrow(sample_n_test),size = floor(0.05*nrow(sample_n_test)),replace = F)
rownames(sample_n_test)<- 1:nrow(sample_n_test)
sample_n_test<- sample_n_test[sam,]
sample_n_test_res[[4]]<- sample_n_test

# sample_n_res -- sample 5%
for (i in 1:5) {
  sample_n<- sample_n_res[[i]]
  set.seed(randomseed)
  sam<- sample(x = 1:nrow(sample_n),size = floor(0.05*nrow(sample_n)),replace = F)
  rownames(sample_n)<- 1:nrow(sample_n)
  sample_n<- sample_n[sam,]
  #n
  set.seed(randomseed)
  sam<- 1:nrow(sample_n)
  sam_train<- sample(x = 1:nrow(sample_n),size = floor(ttratio*nrow(sample_n)),replace = F)
  sam_test<- sam[!sam %in% sam_train]
  rownames(sample_n)<- 1:nrow(sample_n)
  sample_n_train<- sample_n[sam_train,]
  sample_n_test<- sample_n[sam_test,]
  
  sample_n_train_res[[i+5]]<- sample_n_train
  sample_n_test_res[[i+5]]<- sample_n_test
}

# sample_p_train
sample_p_train<- sample_p_train_res[[1]]
sample_p_train$label<- '1'
for (i in 2:length(sample_p_train_res)) {
  sample_p_train1<- sample_p_train_res[[i]]
  sample_p_train1$label<- '1'
  sample_p_train<- rbind(sample_p_train,sample_p_train1)
}
rm(sample_p_train1)
# sample_p_test
sample_p_test<- sample_p_test_res[[1]]
sample_p_test$label<- '1'
for (i in 2:length(sample_p_test_res)) {
  sample_p_test1<- sample_p_test_res[[i]]
  sample_p_test1$label<- '1'
  sample_p_test<- rbind(sample_p_test,sample_p_test1)
}
rm(sample_p_test1)

# sample_n_train
sample_n_train<- sample_n_train_res[[1]]
sample_n_train$label<- '0'
for (i in 2:length(sample_n_train_res)) {
  sample_n_train1<- sample_n_train_res[[i]]
  sample_n_train1$label<- '0'
  sample_n_train<- rbind(sample_n_train,sample_n_train1)
}
rm(sample_n_train1)
# sample_n_test
sample_n_test<- sample_n_test_res[[1]]
sample_n_test$label<- '0'
for (i in 2:length(sample_n_test_res)) {
  sample_n_test1<- sample_n_test_res[[i]]
  sample_n_test1$label<- '0'
  sample_n_test<- rbind(sample_n_test,sample_n_test1)
}
rm(sample_n_test1)

cellname<- 1:nrow(sample_p_train)
cellname<- paste('ptr',cellname,sep = '_')
sample_p_train$sample<- cellname
rownames(sample_p_train)<- cellname

cellname<- 1:nrow(sample_n_train)
cellname<- paste('ntr',cellname,sep = '_')
sample_n_train$sample<- cellname
rownames(sample_n_train)<- cellname

cellname<- 1:nrow(sample_p_test)
cellname<- paste('pte',cellname,sep = '_')
sample_p_test$sample<- cellname
rownames(sample_p_test)<- cellname

cellname<- 1:nrow(sample_n_test)
cellname<- paste('nte',cellname,sep = '_')
sample_n_test$sample<- cellname
rownames(sample_n_test)<- cellname

sample_train<- rbind(sample_p_train,sample_n_train)
sample_test<- rbind(sample_p_test,sample_n_test)

# ndata_train
ndata_train<- list()
for (i in 1:nrow(sample_train)) {
  cellname1<- sample_train$cluster1_cell[i]
  cellname2<- sample_train$cluster2_cell[i]
  celldata1<- ndata_pca[cellname1,]
  celldata2<- ndata_pca[cellname2,]
  ndata_train[[i]]<- c(celldata1,celldata2,celldata1-celldata2)
}
ndata_train<- as.data.frame(ndata_train)
colnames(ndata_train)<- sample_train$sample
ndata_train<- as.data.frame(as.matrix(t(ndata_train)))

# ndata_test
ndata_test<- list()
for (i in 1:nrow(sample_test)) {
  cellname1<- sample_test$cluster1_cell[i]
  cellname2<- sample_test$cluster2_cell[i]
  celldata1<- ndata_pca[cellname1,]
  celldata2<- ndata_pca[cellname2,]
  ndata_test[[i]]<- c(celldata1,celldata2,celldata1-celldata2)
}
ndata_test<- as.data.frame(ndata_test)
colnames(ndata_test)<- sample_test$sample
ndata_test<- as.data.frame(as.matrix(t(ndata_test)))

ndata_train$label<- as.factor(sample_train$label)
ndata_train_h2o<- as.h2o(ndata_train)
ndata_test$label<- as.factor(sample_test$label)
ndata_test_h2o<- as.h2o(ndata_test)

# train and test
ndata_model <- h2o.deeplearning(x = 1:(3*npc),
                                y = 3*npc+1,
                                training_frame = ndata_train_h2o,
                                activation = 'Rectifier',
                                hidden = hidden,
                                seed = randomseed,loss = 'CrossEntropy'
)
predictions <- h2o.predict(ndata_model, ndata_test_h2o)
d1<- h2o.performance(ndata_model,newdata = ndata_test_h2o)
auc_res<- h2o.auc(d1)
predictions <- as.data.frame(predictions)
sample_test$predict<- as.character(predictions$predict)

res<- data.frame(acc = 0,pre = 0,rec = 0,
                 f1 = 0, auc = auc_res, mcc = 0,stringsAsFactors = F)
d1<- sample_test
d2<- as.data.frame(table(d1[,c("label","predict")]),stringsAsFactors = F)
tp<- as.numeric(d2[d2$label == '1' & d2$predict == '1',]$Freq)
tn<- as.numeric(d2[d2$label == '0' & d2$predict == '0',]$Freq)
fp<- as.numeric(d2[d2$label == '0' & d2$predict == '1',]$Freq)
fn<- as.numeric(d2[d2$label == '1' & d2$predict == '0',]$Freq)
res$acc[1]<- (tp+tn)/(tp+tn+fp+fn)
res$pre[1]<- (tp)/(tp+fp)
res$rec[1]<- (tp)/(tp+fn)
res$f1[1]<- (2*tp)/(2*tp+fn+fp)
res$mcc[1]<- (tp*tn-fp*fn)/sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn))
