

Renew <- function(dir,Pn,BM,BMI){
	
###########	for daily rewnew. summary file exist


library(xlsx)

summarypath=paste0(dir,"/summary_",Pn,".csv")

summary = as.data.frame(read.csv(summarypath))

coln=length(summary[1,])

startday=as.Date(summary[1,coln],"%Y%m%d")+1

endday=Sys.Date()

datetine=seq(as.Date(startday), as.Date(endday), by='day')
options(digits=12)
options(stringsAsFactors=FALSE)


##bm=read.xlsx(paste0(BM,".xlsx"),sheetName=1,startRow=6,header=T) #### read from file

 ##now read from input BMI


for(i in 1:(length(datetine))){             ####collect old data

		todate=format(datetine[i],"%Y%m%d")

	setwd(dir)
	int=tryCatch({
		read.csv(paste0(Pn,"_",todate,"_",todate,".csv"))},
		error=function(e){e=0},	warning=function(w){w=0})


	if(length(int)!=1){
		
		summary = as.data.frame(read.csv(summarypath))

		summary1=cbind(summary,as.data.frame(matrix(0,nrow=21)))

		coln=length(summary1[1,])

		summary1[1,coln]=todate
		
		summary1[2,coln]=summary1[2,coln-1]


		if (length(which(int=="Change in NAV"))!=0) {
			p = which(int=="Ending Value")
			NAV = int[p %% nrow(int),4]
		} else {
			p = which(int=="Current Total",arr.ind=T)
			if (length(which(int=="Interest Accruals"))!=0) {
				NAV = int[p[,1]+8,p[,2]]
			} else {
				NAV = int[p[,1]+6,p[,2]]
			}
		}

	
		NAVn=as.numeric(as.character(NAV))
	
		summary1[3,coln]=prettyNum(NAVn, big.mark = ",")

		NAVp=as.numeric(gsub(",","",(summary1[3,coln-1])))
		
		summary1[4,coln]=summary1[4,coln-1]
		if(as.numeric(summary1[1,coln])-as.numeric(as.character(summary1[1,coln-1]))>5)
			summary1[4,coln]=summary1[3,coln-1]
	
		Dailypnl=NAVn-NAVp

		summary1[5,coln]=prettyNum(Dailypnl, big.mark = ",")

		summary1[6,coln]=paste0(round(Dailypnl/NAVp*100,6),"%")

		p = which(int=="Current Long")

		if (length(which(int=="Cash Collateral"))==0){
			LongNAV <- int[p+4,1]
			ShortNAV <- int[p+4,2]
		} else {
			LongNAV <- int[p+6,1]
			ShortNAV <- int[p+6,2]
		}
	LongNAVn=as.numeric(as.character(LongNAV))
	ShortNAVn=as.numeric(as.character(ShortNAV))

	summary1[7,coln]=prettyNum(LongNAVn, big.mark = ",")
	
	summary1[8,coln]=prettyNum(ShortNAVn, big.mark = ",")
	
	LongNAVp=as.numeric(gsub(",","",(summary1[7,coln-1])))
	
	ShortNAVp=as.numeric(gsub(",","",(summary1[8,coln-1])))
	
	if(LongNAVp==0){
		summary1[9,coln]=0
	}else{
		summary1[9,coln]=paste0(round((LongNAVn-LongNAVp)/LongNAVp,6)*100,"%")
	}
		
	if(ShortNAVp==0){
		summary1[10,coln]=0
	}else{
		summary1[10,coln]=paste0(round(-(ShortNAVn-ShortNAVp)/ShortNAVp,6)*100,"%")
	}

	summary1[11,coln]=paste0(round(LongNAVn/NAVn*100,6),"%")
	
	summary1[12,coln]=paste0(round(ShortNAVn/NAVn*100,6),"%")
	
	summary1[13,coln]=paste0(round((LongNAVn/NAVn+ShortNAVn/NAVn)*100,6),"%")

	summary1[14,coln]=paste0(round((NAVn-as.numeric(gsub(",","",(summary1[4,coln]))))/as.numeric(gsub(",","",(summary1[4,coln])))*100,6),"%")

	summary1[15,coln]=paste0(round((NAVn-as.numeric(gsub(",","",(summary1[2,coln]))))/as.numeric(gsub(",","",(summary1[2,coln])))*100,6),"%")
	
	bml=as.numeric(BMI)	
	
	bmlp=as.numeric(gsub(",","",(summary1[17,coln-1])))
	
	summary1[16,coln]=summary[16,coln-1]
	
	summary1[17,coln]=prettyNum(bml, big.mark = ",")
	
	summary1[18,coln]=summary1[18,coln-1]
	if(as.numeric(summary1[1,coln])-as.numeric(as.character(summary1[1,coln-1]))>5)
			summary1[18,coln]=summary1[17,coln-1]
	
	summary1[19,coln]=paste0(round((bml-bmlp)/bmlp*100,6),"%")

	summary1[20,coln]=paste0(round((bml-as.numeric(gsub(",","",(summary1[18,coln]))))/as.numeric(gsub(",","",(summary1[18,coln])))*100,6),"%")
	
	summary1[21,coln]=paste0(round((bml-as.numeric(gsub(",","",(summary1[16,coln]))))/as.numeric(gsub(",","",(summary1[16,coln])))*100,6),"%")

	write.csv(summary1,summarypath,row.names=F)	}
} 
}



renewMERGE <- function(dirb,output){
	
	nop = length(dirb[,1])
	
	nopt=seq(1:nop)
	
	summaryt=as.data.frame(matrix(0,nrow=22,ncol=nop))

	
	for(i in 1:nop){
		Renew(dirb[i,1],dirb[i,2],dirb[i,3],dirb[i,4])
		summarypath=paste0(dirb[i,1],"/summary_",dirb[i,2],".csv")
		summary1 = as.data.frame(read.csv(summarypath))
		coln1=length(summary1[1,])
		if(dirb[i,2]=="U8293808")
			NAME="US LO"
		if(dirb[i,2]=="U8383220")
			NAME="TWO SP"
		if(dirb[i,2]=="U9721087")
			NAME="TECH LO"
		if(dirb[i,2]=="U9801918")
			NAME="TECH LO 2"
		summaryt[,i]=c(NAME,summary1[,coln1])
	}

	
	
	
	summary11=as.data.frame(read.csv(paste0(output,"/summarytotal.csv")))
	
	coln2=length(summary11[1,])

	if(summary11[2,coln2]!=summaryt[2,1]){
		summary=cbind(summary11,summaryt)}else{
			summary=summary11}

	summaryf=summary
	summaryf[1:2,]=summary[1:2,]
	summaryf[3,]=summary[7,]
	summaryf[4,]=summary[20,]
	summaryf[5,]=summary[16,]
	summaryf[6,]=summary[22,]
	summaryf[7:10,]=summary[3:6,]
	summaryf[11:18,]=summary[8:15,]
	summaryf[19:21,]=summary[17:19,]
	summaryf[22,]=summary[21,]

	setwd(output)
	write.csv(summary,paste0(output,"/summarytotal.csv"),row.names=F)
	write.csv(summaryf,paste0(output,"/summarytotal1.csv"),row.names=F)
}





##########

nop=4

dirb=data.frame(diretion=rep(0,nop),name=rep(0,nop),BM=rep(0,nop),BMI=rep(0,nop))

dirb[1,]=c("/Users/ouyangming/Desktop/U8293808","U8293808","SPXT",5706.66) ####with old order       

dirb[2,]=c("/Users/ouyangming/Desktop/U8383220","U8383220","SPXT",5706.66)

dirb[3,]=c("/Users/ouyangming/Desktop/U9721087","U9721087","XNDX",8409.65)

dirb[4,]=c("/Users/ouyangming/Desktop/U9801918","U9801918","XNDX",8409.65)

output = "/Users/ouyangming/Desktop"

summaryt = renewMERGE(dirb,output)