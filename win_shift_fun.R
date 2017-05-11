
win_shift_fun<-function(k=k,MAT=MAT){
    win_mat=as.data.frame(matrix(NA,nrow=nrow(MAT),ncol=length(k)))
    rownames(win_mat)=rownames(MAT);colnames(win_mat)=paste("k",k,sep="=")

        for(i in 1:length(k)){

                for(pos in 1:nrow(MAT)){
           start=pos
           end=pos+k[i]-1
           win_mat[pos,i]=length(table(apply(MAT[start:end,],2,function(x){
                  paste(x,collapse=":")
                })))
                }
    }
        return(win_mat)
}

pict_fun<-function(win_mat,file="curve_plot.pdf"){

    N=ncol(win_mat)
    col=c("black","pink","red","blue","darkkhaki","lightgoldenrod1","olivedrab","royalblue4","orange","gray30",colours())
    win_mat=win_mat[,grep("^k=[\\d]*",colnames(win_mat))]
    col=col[1:ncol(win_mat)]
    pdf(file)
    plot(x=1,y=1,ylim=c(1,(sample_N+sample_N*0.4)),xlim=c(1,nrow(win_mat)),xlab="postion",
        ylab="the number of sample",type="n")
    for (i in 1:ncol(win_mat)){
        points(win_mat[,i],type="o",col=col[i],pch=20,cex=0.5,lwd=2)
        legend(nrow(win_mat)*0.8,sample_N+sample_N*0.4-0.2*i,colnames(win_mat)[i],col=col[i],lty=2,bty="n",cex=0.8)
    }
    dev.off()
}

hap_mv_fun<-function(win_mat,sample_N=NULL,k_i=NULL){
    line=win_mat[,which(colnames(win_mat)==paste("k",k_i,sep="="))]
    win_mat_clean=win_mat[which(line>=sample_N),]
    return(win_mat_clean)
}

ord_fun<-function(MAT){
  ORD=as.data.frame(t(sapply(rownames(MAT),function(x) strsplit(x,":")[[1]])))         
  MAT=split(MAT,f=gsub("chr","",ORD[,1]))
  MAT=lapply(MAT,function(x) x[order(x[,2],decreasing=FALSE),])
  MAT=MAT[as.character(sort(as.numeric(names(MAT))))]
  mat=NULL
  for(i in 1:length(MAT)){
    mat<-rbind(mat,MAT[[i]])
  }
  return(mat)
}


k_mat_fun<-function(file="matrix_replaced.txt",k=1:5,
  sample_N=NULL,k_i=max(k)){
  MAT=read.table(file,sep="\t",as.is=TRUE)
  MAT=ord_fun(MAT)
  if(is.null(sample_N)){sample_N=ncol(sample_N)}

  win_mat=win_shift_fun(MAT=MAT,k=k)
  write.table(win_mat,"k_matrix_before.txt",quote=FALSE,sep="\t")
  pict_fun(win_mat,file="k_matrix_before.pdf")

  win_mat_after=hap_mv_fun(win_mat=win_mat,sample_N=sample_N,k_i=k_i)
  win_mat_after=win_shift_fun(MAT=win_mat_after,k=k)
  write.table(win_mat_after,"k_matrix_after.txt",quote=FALSE,sep="\t")
  pict_fun(win_mat_after,file="k_matrix_after.pdf")
}

#k_mat_fun(file="matrix_replaced.txt",k=1:5,sample_N=7,k_i=3)

