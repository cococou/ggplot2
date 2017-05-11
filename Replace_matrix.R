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
print(ONE())
write.table(ONE(),"replace_matrix.txt",sep="\t",quote=FALSE)
