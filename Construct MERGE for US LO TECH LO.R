


MERGE <- function(dirb,output){
	
	nop = length(dirb[,1])
	
	nopt=seq(1:nop)
	
	summary = as.data.frame(matrix(0,nrow=22))
	
	BM="Benchmark"
	
	rownames(summary)=c("Pn","DATE","Inital NAV","Current NAV","NAV closing on precious LOM","Daily P/L", "Daily % change","Long NAV","Short NAV", "Long % daily P/L change","Short % daily P/L change","Long exposure %","Short exposure %","Net exposure %", "MTD % change" ,"Change since inception",paste(BM,"Inital"), paste(BM,"Today"), paste(BM,"closing on precious LOM"),paste(BM,"daily % change"),paste(BM,"MTD % change"),paste(BM,"Change since inception"))
	
	for(i in 1:nop){
		setwd(dirb[i,1])
		summarypath=paste0(dirb[i,1],"/summary_",dirb[i,2],".csv")
		summary1 = as.data.frame(read.csv(summarypath))
		if(dirb[i,2]=="U8293808")
			NAME="US LO"
		if(dirb[i,2]=="U8383220")
			NAME="TWO SP"
		if(dirb[i,2]=="U9721087")
			NAME="TECH LO"
		if(dirb[i,2]=="U9801918")
			NAME="TECH LO 2"
		summary1 = rbind(NAME,summary1)
		summary = cbind(summary,summary1[,-1])
	}
	
	summary=summary[,-1]
	summary=summary[,order(summary[2,])]


	setwd(output)
	write.csv(summary,"summarytotal.csv")
	
}





##########

nop=4

dirb=data.frame(diretion=rep(0,nop),name=rep(0,nop))

dirb[1,]=c("/Users/ouyangming/Desktop/U8293808","U8293808")

dirb[2,]=c("/Users/ouyangming/Desktop/U8383220","U8383220")

dirb[3,]=c("/Users/ouyangming/Desktop/U9721087","U9721087")

dirb[4,]=c("/Users/ouyangming/Desktop/U9801918","U9801918")

output = "/Users/ouyangming/Desktop"

summaryt = MERGE(dirb,output)