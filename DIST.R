DIST_fun<-function(file="matrix.txt"){
####fun1####
ONE<-function(N=c("A","T","C","G")){
        mat<-as.data.frame(matrix(NA,ncol=length(N),nrow=length(N)))
    pn<-(length(N)*length(N)-length(N))/2+length(N)
    SET<-strsplit(c("BDZFHIJKLMNOPQRSUVWXYZ"),"")[[1]]
    PN=SET[1:pn]
        colnames(mat)<-N;rownames(mat)<-N
        pni=0
        for(i in 1:length(N)){
                for(j in 1:length(N)){
                        if(is.na(mat[i,j])){
                                pni=pni+1
                                mat[i,j]=PN[pni]
                mat[j,i]=PN[pni]
                        }else{
                next
                        }

                }
        }
        return(mat)
}

####fun2####
REP_fun<-function(MAT=data,REP_MAT=ONE()){
    Rev_fun<-function(x){
    	paste(rev(strsplit(x,"")[[1]]),collapse="")
    }
    MAT=t(apply(MAT,1,function(x){
    	  sapply(x,function(x1) {
    	  	  y=strsplit(x1,"")[[1]]
    	  	  if(length(y)==2){
    	  	    REP_MAT[which(rownames(REP_MAT)==y[1]),which(colnames(REP_MAT)==y[2])]    	  	  
    	  	  }else{
    	  	  	paste(sort(y),collapse="")
    	  	  }
    	  	})        
    	}))
    return(MAT)
}

####fun3####
Dist<-function(all){
  all_3=apply(all,2,as.character)
  rownames(all_3)=rownames(all)
  NAME<-colnames(all_3)
  Mat<-as.data.frame(matrix(NA,ncol=ncol(all_3),nrow=ncol(all_3)));colnames(Mat)<-NAME
  rownames(Mat)<-NAME

  for (i in 1:ncol(all_3)){
    for (j in 1:ncol(all_3)){

      yij<-apply(cbind(all_3[,i],all_3[,j]),1,function(x) {
          if((x[1]==x[2])) {
            y=1;return(y)
          }else{
            y=-1;return(y)
          }
      })
      Y=as.data.frame(table(yij));Yij=Y[,2];names(Yij)=Y[,1];rm(Y)
      Mat[NAME[i],NAME[j]]<-paste(Yij["1"],Yij["-1"],sep=":")
    }
  }
  DIST=apply(Mat,1,function(x) {
      sapply(x,function(y) {
        z=strsplit(as.character(y),":")[[1]]
        z[union(which(z=="NA"),which(is.na(z)))]<-0
        as.numeric(as.character(z[1]))-as.numeric(as.character(z[2]))
      })
    })

  if (length(which(rownames(installed.packages())=="pheatmap"))==0){
    install.packages("pheatmap")
  }else{
    library("pheatmap")
  }
  pdf("distance_heatmap.pdf")
    pheatmap(DIST,scale = "none",number_format = "%.0f",fontsize=8,
         fontsize_number = 0.6*8,border_color = "black",
         display_numbers=TRUE)
  dev.off()
  return(DIST)
}

data<-read.table(file,sep="\t",as.is=T)
REP_MAT=ONE()
MAT=REP_fun(data)
DIST=Dist(MAT)
write.table(DIST,"distance_matrix.txt",sep="\t",quote=FALSE)
write.table(MAT,"matrix_replaced.txt",sep="\t",quote=FALSE)
}
##########################################
