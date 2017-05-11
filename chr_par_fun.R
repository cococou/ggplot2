chr_dis_fun<-function(file1="matrix_replaced.txt",file2="k_matrix_after.txt",k){
	data1=read.table(file1,as.is=TRUE,sep="\t")
	data2=read.table(file2,as.is=TRUE,sep="\t")
	data1=data1[which(rownames(data1) %in% rownames(data2)),]

k_fun<-function(all_12,k){
    win_mat=as.data.frame(matrix(NA,nrow=nrow(all_12),ncol=1))
    rownames(win_mat)=rownames(all_12);colnames(win_mat)=paste("k",k,sep="=")
    for(pos in 1:nrow(all_12)){
    	start=pos
    	end=pos+k-1
        len=length(unique(apply(all_12[start:end,],2,function(x){
                  paste(x,collapse=":")
        })))
        if(len==1){
           win_mat[pos,1]=1
        }else{
           win_mat[pos,1]=-1
        }
    }
    win_mat=sum(win_mat[-c(nrow(win_mat):(nrow(win_mat)-k+1)),1])
    return(win_mat)
}

Dist2<-function(all_3,k){
  win_mat=as.data.frame(matrix(NA,nrow=ncol(all_3),ncol=ncol(all_3)))
  rownames(win_mat)=colnames(all_3);colnames(win_mat)=colnames(all_3)
  for (i in 1:ncol(all_3)){
    for (j in 1:ncol(all_3)){
      win_mat[colnames(all_3)[i],colnames(all_3)[j]]=k_fun(all_3[,c(i,j)],k)
    }
  }
  if (length(which(rownames(installed.packages())=="pheatmap"))==0){
    install.packages("pheatmap")
  }else{
    library("pheatmap")
  }
  pdf("k_heatmap.pdf")
    pheatmap(win_mat,scale = "none",number_format = "%.0f",fontsize=8,
         fontsize_number = 0.6*8,border_color = "black",
         display_numbers=TRUE)
  dev.off()
  write.table(win_mat,"k_distance.txt",sep="\t",quote=FALSE)
  return(win_mat)
}
Dist2(all_3=data1,k=k)
}
#chr_dis_fun(k=7)
