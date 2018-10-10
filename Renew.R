###########	for daily rewnew. summary file exist .  ##############
########### Should input the value of todays benchmark.##########
Renew <- function(dir,Pn,BM,BMI){

	library(xlsx)
	options(digits=12)
	options(stringsAsFactors=FALSE)

	summarypath=paste0(dir,"/summary_",Pn,".csv")
	summary = as.data.frame(read.csv(summarypath))
	coln=length(summary[1,])
	startday=as.Date(summary[1,coln],"%Y%m%d")+1
	endday=Sys.Date()
	datetine=seq(as.Date(startday), as.Date(endday), by='day')
	##bm=read.xlsx(paste0(BM,".xlsx"),sheetName=1,startRow=6,header=T) #### read from file
	bm=BMI  ##read from input
	for(i in 1:(length(datetine))){             ####collect old data
		todate=format(datetine[i],"%Y%m%d")
		int=tryCatch({read.csv(paste0(Pn,"_",todate,"_",todate,".csv"))},error=function(e){e=0},warning=function(w){w=0})
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
			bml=BMI
			bmlp=as.numeric(gsub(",","",(summary1[17,coln-1])))
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
	} 
}



dir="/Users/ouyangming/Desktop/U9801918"
setwd(dir)
Pn="U9801918"
BM="XNDX"
BMI=8733.59
Renew (dir,Pn,BM,BMI)





