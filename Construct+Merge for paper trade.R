####specific name
MERGE <- function(dirb,output){
	options(stringsAsFactors=FALSE)
	nop = length(dirb[,1])
	
	nopt=seq(1:nop)
	
	summary = as.data.frame(matrix(0,nrow=22))
	
	BM="Benchmark"
	
	rownames(summary)=c("Pn","DATE","Inital NAV","Current NAV","NAV closing on precious LOM","Daily P/L", "Daily % change","Long NAV","Short NAV", "Long % daily P/L change","Short % daily P/L change","Long exposure %","Short exposure %","Net exposure %", "MTD % change" ,"Change since inception",paste(BM,"Inital"), paste(BM,"Today"), paste(BM,"closing on precious LOM"),paste(BM,"daily % change"),paste(BM,"MTD % change"),paste(BM,"Change since inception"))
	
	for(i in 1:nop){
		setwd(dirb[i,1])
		summarypath=paste0(dirb[i,1],"/summary_",dirb[i,2],".csv")
		summary1 = as.data.frame(read.csv(summarypath))
		if(dirb[i,2]=="DU846825")
			NAME="HK L/O"
		if(dirb[i,2]=="DU846826")
			NAME="HK L/O-HSI"
		if(dirb[i,2]=="DU846828")
			NAME="A-share L/O Beta"
		if(dirb[i,2]=="DU846829")
			NAME="A-share L/O Raw"
        if(dirb[i,2]=="DU846827")
            NAME="UK L/O-UKX"
        if(dirb[i,2]=="DU878918")
            NAME="UK L/O"
		summary1 = rbind(NAME,summary1)
		summary = cbind(summary,summary1[,-1])
	}
	
	summary=summary[,-1]
	summary=summary[,order(summary[2,])]
    
	setwd(output)
	write.csv(summary,"summarytotalpp.csv")
}





##########

nop=6

dirb=data.frame(diretion=rep(0,nop),name=rep(0,nop))

dirb[1,]=c("/Users/ouyangming/Desktop/DU846825","DU846825")

dirb[2,]=c("/Users/ouyangming/Desktop/DU846826","DU846826")

dirb[3,]=c("/Users/ouyangming/Desktop/DU846828","DU846828")

dirb[4,]=c("/Users/ouyangming/Desktop/DU846829","DU846829")

dirb[5,]=c("/Users/ouyangming/Desktop/DU846827","DU846827")

dirb[6,]=c("/Users/ouyangming/Desktop/DU878918","DU878918")


output = "/Users/ouyangming/Desktop"

summaryt = MERGE(dirb,output)
