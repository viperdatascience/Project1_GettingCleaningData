rm(list=ls())
x_test<-read.table("X_test.txt")
x_train<-read.table("X_train.txt")
y_test<-read.table("y_test.txt")
y_train<-read.table("y_train.txt")

merged_X<-rbind(x_train,x_test)
merged_Y<-rbind(y_train,y_test)
total_merged<-cbind(merged_Y,merged_X)
features<-read.table("features.txt")
xtracted<-grepl("std|mean",features[,2])
tmp<-total_merged[,2:562]
y<-tmp[,xtracted]
ynames<-t(factor(c("Acitivity",as.character(features[xtracted,2]))))
ymerged<-cbind(merged_Y,y)


subject_train<-read.table("subject_train.txt")
subject_test<-read.table("subject_test.txt")
total_subject<-rbind(subject_train,subject_test)

act<-seq(1:6)
for(i in act){
  index<-which(ymerged[,1] %in% i)
  if(i == 1){
    ymerged[index,1] = "WALKING"
  }
  if(i == 2){
    ymerged[index,1] = "WALKING_UPSTAIRS"
  }
  if(i == 3){
    ymerged[index,1] = "WALKING_DOWNSTAIRS"
  }
  if(i == 4){
    ymerged[index,1] = "SITTING"
  }
  if(i == 5){
    ymerged[index,1] = "STANDING"
  }
  if(i == 6){
    ymerged[index,1] = "LAYING"
  }
}
colnames(ymerged)<-ynames
subject_vec<-seq(1:30)
action_vec<-seq(1:6)
actions<-c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")
result_mean<-matrix(,nrow=30,ncol=6)
result_std<-matrix(nrow=30,ncol=6)

mean_index<-grepl("mean",colnames(ymerged))
std_index<-grepl("std",colnames(ymerged))
mean_names<-c("MEAN WALKING","MEAN WALKING_UPSTAIRS","MEAN WALKING_DOWNSTAIRS","MEAN SITTING","MEAN STANDING","MEAN LAYING")
std_names<-c("STD WALKING","STD WALKING_UPSTAIRS","STD WALKING_DOWNSTAIRS","STD SITTING","STD STANDING","STD LAYING")
for (k in subject_vec){
  subject_index <- which(total_subject[,1] %in% k)
  for (j in action_vec){
    action_index<-which(ymerged[subject_index,1] %in% actions[j])
    avg_mean<-mean(ymerged[action_index,mean_index][,2])
    result_mean[k,j]<-avg_mean
    std_mean<-mean(ymerged[action_index,std_index][,2])
    result_std[k,j]<-std_mean
  }
  
}

total_result<-data.frame(cbind(result_mean,result_std))
tmpnames<-cbind(mean_names,std_names)
total_result<-cbind(subject_vec,total_result)
tmpnames2<-c("ACTIVITY",tmpnames)
colnames(total_result)<-tmpnames2

fileConn<-file("output.txt")
write.table(total_result, fileConn, sep="\t", row.names=FALSE)
