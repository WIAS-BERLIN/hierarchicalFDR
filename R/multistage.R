multistage <-
function(pValues, alpha=0.05){ #For the special case of 2 families, larger numbers are possible but were computationally too exhausting for large families as studied here
result<-list()
k<-length(pValues)#Here always 2

flpvalues<-numeric()
for (i in 1:k){ #Determine intersection hypothesis p-value for both families
	fpValues<-pValues[[i]]
	sortfpValues<-sort(fpValues)
	fm<-length(fpValues)
	conjpValues<-numeric()
	for (i in 1:fm){
		locpvalue<-(sortfpValues[[i]]*fm/i)
		conjpValues<-c(conjpValues,locpvalue)
		}
	flpvalues<-c(flpvalues,min(conjpValues))
	}

ktilde<-c(1,1)

R<-c(k,k)
famlevel<-BH(flpvalues,alpha=alpha,silent=TRUE)$rejected #Test intersection hypothesis
R[[2]]<-length(famlevel[famlevel==TRUE])
x<-0
if (famlevel[[1]]==FALSE && famlevel[[2]]==FALSE){x<-1}
u<-0
while(x==0){u<-u+1
	if (famlevel[[1]]==TRUE){#computation of new family p-value for the u-partial conjunction hypothesis for family 1
		fpValues<-pValues[[1]]
		sortfpValues<-sort(fpValues)
		fm<-length(fpValues)
		fu<-min(c(fm,u))
		conjpValues<-numeric()
		for (i in 1:(fm-fu+1)){
			locpvalue<-(sortfpValues[[(fu-1+i)]]*(fm-fu+1)/i)
			conjpValues<-c(conjpValues,locpvalue)
			}
		flpvalues[[1]]<-min(conjpValues)
		ktilde[[1]]<-u
		}
	if (famlevel[[1]]==FALSE){flpvalues[[1]]<-1}
	
	if (famlevel[[2]]==TRUE){#computation of new family p-value for the u-partial conjunction hypothesis for family 2
		fpValues<-pValues[[2]]
		sortfpValues<-sort(fpValues)
		fm<-length(fpValues)
		fu<-min(c(fm,u))
		conjpValues<-numeric()
		for (i in 1:(fm-fu+1)){
			locpvalue<-(sortfpValues[[(fu-1+i)]]*(fm-fu+1)/i)
			conjpValues<-c(conjpValues,locpvalue)
			}
		flpvalues[[2]]<-min(conjpValues)
		ktilde[[2]]<-u
		}
	if (famlevel[[2]]==FALSE){flpvalues[[2]]<-1}
	
	famlevel<-BH(flpvalues,alpha=alpha*(R[[2]]/R[[1]]),silent=TRUE)$rejected #Test of the u-partial conjunction hypotheses
	R[[1]]<-R[[2]]
	R[[2]]<-length(famlevel[famlevel==TRUE])
	
	if (famlevel[[1]]==FALSE && famlevel[[2]]==FALSE){x<-1}
	
	if (u==max(length(pValues[[1]]),length(pValues[[2]]))){x<-1}
	}
#Computing the corresponding rejected p-values	
gone<-sort(pValues[[1]])
ktildo<-ktilde[[1]]
grenzone<-gone[[ktildo]]
rejfamone<-(pValues[[1]]<=grenzone)

gtwo<-sort(pValues[[2]])
ktildtwo<-ktilde[[2]]
grenztwo<-gtwo[[ktildtwo]]
rejfamtwo<-(pValues[[2]]<=grenztwo)
result<-c(list(rejfamone),list(rejfamtwo))
return(result)
}
