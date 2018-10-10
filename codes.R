Construct <- function(dir,Pn,startday,endday,BM){
########for no summary file.
      
library(xlsx)

datetine=seq(as.Date(startday), as.Date(endday), by='day')
options(digits=12)
options(stringsAsFactors=FALSE)


bm=read.xlsx(paste0(BM,".xlsx"),sheetName=1,startRow=6,header=T)




	#############Inital 
summary = as.data.frame(matrix(0,nrow=21))
rownames(summary)=c("DATE","Inital NAV","Current NAV","NAV closing on precious LOM","Daily P/L", "Daily % change","Long NAV","Short NAV", "Long % daily P/L change","Short % daily P/L change","Long exposure %","Short exposure %","Net exposure %", "MTD % change" ,"Change since inception",paste(BM,"Inital"), paste(BM,"Today"), paste(BM,"closing on precious LOM"),paste(BM,"daily % change"),paste(BM,"MTD % change"),paste(BM,"Change since inception"))

  
	####without benchmark.
		#summary = as.data.frame(matrix(0,nrow=13))
		#rownames(summary)=c("DATE","Inital NAV","Current NAV","NAV closing on precious LOM","Daily P/L", "Daily % change","Long % daily P/L change","Short % daily P/L change","Long exposure %","Short 			exposure %","Net exposure %", "MTD %change" ,"Change since inception")

	 
	
intday=format(datetine[1],"%Y%m%d") #####start day

summary[1,1]=intday 

int= read.csv(paste0(Pn,"_",intday,"_",intday,".csv"))


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

CCF=prettyNum(NAVn, big.mark = ",")

summary[2,1]=CCF
summary[3,1]=CCF
summary[4,1]=CCF


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

summary[7,1]=prettyNum(LongNAVn, big.mark = ",")

#CCS=ShortNAV
#CCn=ShortNAVn
########record account number
#CCnd=floor(CCn)
#ww=nchar(as.character(CCnd))
#dd=ww%%3
#bb=floor(ww/3)
#CCF=substr(CCS,ww-2,nchar(CCS))
#if((bb-1)>0){
#	for(i in 1:(bb-1))
#		CCF=paste0(substr(CCS,ww-3*(i+1)+1,ww-3*(i)),",",CCF)		
#	if(dd!=0)
#		CCF=paste0(substr(CCS,1,dd),",",CCF)
#}

summary[8,1]=prettyNum(ShortNAVn, big.mark = ",")


bml=bm[which(bm[,1]==datetine[1]),2]

########record account number


summary[16,1]=summary[17,1]=summary[18,1]=prettyNum(bml, big.mark = ",")


summarypath=paste0(dir,"/summary_",Pn,".csv")

write.csv(summary,summarypath)


#####collect old data


for(i in 2:(length(datetine))){             ####collect old data


	#####every day

	todate=format(datetine[i],"%Y%m%d")


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
	
	bml=as.numeric(bm[which(bm[,1]==datetine[i]),2])
	
	bmlp=as.numeric(gsub(",","",(summary1[17,coln-1])))
	
	if(length(bml)==0)
		bml=bmlp
	
	summary1[16,coln]=summary[16,coln-1]
	
	summary1[17,coln]=prettyNum(bml, big.mark = ",")
	
	summary1[18,coln]=summary1[18,coln-1]
	if(as.numeric(summary1[1,coln])-as.numeric(as.character(summary1[1,coln-1]))>5)
			summary1[18,coln]=summary1[17,coln-1]
	
	summary1[19,coln]=paste0(round((bml-bmlp)/bmlp*100,6),"%")

	summary1[20,coln]=paste0(round((bml-as.numeric(gsub(",","",(summary1[18,coln]))))/as.numeric(gsub(",","",(summary1[18,coln])))*100,6),"%")
	
	summary1[21,coln]=paste0(round((bml-as.numeric(gsub(",","",(summary1[16,coln]))))/as.numeric(gsub(",","",(summary1[16,coln])))*100,6),"%")

	write.csv(summary1,summarypath,row.names=F)
	}
} #####collect old data


}







startday="2018-03-05"

endday="2018-10-05"

BM="UKX"


	
	
dir="/Users/ouyangming/Desktop/DU846827"

setwd(dir)

Pn="DU846827"

BM="UKX"

Construct(dir,Pn,startday,endday,BM)

	
	
	
	
	
