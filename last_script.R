#Read data
fifat <-read.csv("FifaTrainNew.csv") #fifat is training set
fifav <-read.csv("FifaNoY.csv") # fifav is validation set


#Creating new columns CL, mean_wage and mean_overall
#for fifat 

# for CL (club wage level)
club_name<-levels(fifat$Club)
club_index<-vector(mode="list",length=length(club_name))
names(club_index)<-club_name

for(n in club_name){
  club_index[[n]]<-mean((fifat[which((fifat$Club)==n),]$WageNew),na.rm=T)
}

for (i in club_name){
  
  MC<-club_index[[i]]
  if(is.nan(MC)){
    club_index[i]<-0
  }
  else if(MC<2000){
    club_index[i]<-1
    
  }
  else if(MC<2500){
    club_index[i]<-2
  }
  else if(MC<3000){
    club_index[i]<-3
  }
  
  else if(MC<3500){
    club_index[i]<-4
  }
  
  else if(MC<4000){
    club_index[i]<-5
  }
  
  else if(MC<4500){
    club_index[i]<-6
  }
  
  
  else if(MC<5000){
    club_index[i]<-7
  }
  
  else if(MC<6000){
    club_index[i]<-8
    
  }
  
  else if(MC<7000){
    club_index[i]<-9
    
  }
  else if(MC<10000){
    club_index[i]<-10
    
  }
  else if(MC<13000){
    club_index[i]<-11
    
  }
  else if(MC<17000){
    club_index[i]<-12
    
  }
  else if(MC<20000){
    club_index[i]<-13
    
  }
  else if(MC<25000){
    club_index[i]<-14
    
  }
  else if(MC<40000){
    club_index[i]<-15
    
  }
  else{
    club_index[i]<-16
  }
  
}

fifat<-cbind(fifat,"CL"=rep(0,nrow(fifat)))
for (j in 1:nrow(fifat)){
  if(!is.na(fifat[j,]$Club)){
    fifat[j,]$CL<-club_index[[fifat[j,]$Club]]
  }
}

fifat$CL<-ifelse(fifat$CL==0,median(fifat$CL),fifat$CL)
fifat$CL<-as.factor(fifat$CL)


#for mean_wage
club_mean_wage<-vector(mode="list",length=length(club_name))
names(club_mean_wage)<-club_name
for(n in club_name){
  club_mean_wage[[n]]<-mean((fifat[which((fifat$Club)==n),]$WageNew),na.rm=T)
}

fifat<-cbind(fifat,"mean_wage"=rep(0,nrow(fifat)))
for(i in 1:nrow(fifat)){
  if(!is.na(fifat$Club[i])){
    fifat$mean_wage[i]<-club_mean_wage[[fifat$Club[i]]]
  }
  
}
fifat$mean_wage<-ifelse(fifat$mean_wage==0,mean(fifat[fifat$mean_wage!=0,]$mean_wage),fifat$mean_wage)


#for mean_overall
club_overall<-vector(mode="list",length=length(club_name))
names(club_overall)<-club_name


for(n in club_name){
  
  club_overall[[n]]<-mean((fifat[which((fifat$Club)==n),]$Overall),na.rm=T)
  
}

fifat<-cbind(fifat,"mean_overall"=rep(0,nrow(fifat)))

for(i in 1:nrow(fifat)){
  if(!is.na(fifat$Club[i])){
    fifat$mean_overall[i]<-club_overall[[fifat$Club[i]]]
  }
  
}
fifat$mean_overall<-ifelse(fifat$mean_overall==0,mean(fifat[fifat$mean_overall!=0,]$mean_overall),fifat$mean_overall)

#finish building the model
model <-lm(I(log(WageNew)^(4))~I(Potential^0.75)+I(Overall^3)+Age+mean_wage+mean_overall+CL+I(Overall^3):CL+I(Potential^0.75):CL+mean_wage:CL+mean_overall:CL,data=fifat)


#--------------------------------------------------------------------------------------------------------------------------------------------------------
#now start to modify fifav

#CL 
fifav<-cbind(fifav,"CL"=rep(0,nrow(fifav)))
for (j in 1:nrow(fifav)){
  if(!is.na(fifav[j,]$Club)){
    fifav[j,]$CL<-club_index[[fifav[j,]$Club]]
  }
}

fifav$CL<-ifelse(fifav$CL==0,median(fifav$CL),fifav$CL)
fifav$CL<-as.factor(fifav$CL)

#mean_wage
fifav<-cbind(fifav,"mean_wage"=rep(0,nrow(fifav)))
for(i in 1:nrow(fifav)){
  if(!is.na(fifav$Club[i])){
    fifav$mean_wage[i]<-club_mean_wage[[fifav$Club[i]]]
  }
  
}
fifav$mean_wage<-ifelse(fifav$mean_wage==0,mean(fifav[fifav$mean_wage!=0,]$mean_wage),fifav$mean_wage)

#mean_overall
fifav<-cbind(fifav,"mean_overall"=rep(0,nrow(fifav)))

for(i in 1:nrow(fifav)){
  if(!is.na(fifav$Club[i])){
    fifav$mean_overall[i]<-club_overall[[fifav$Club[i]]]
  }
  
}
fifav$mean_overall<-ifelse(fifav$mean_overall==0,mean(fifav[fifav$mean_overall!=0,]$mean_overall),fifav$mean_overall)

#------------------------------------------------------------------------------------------------------------------------------------------------------
#now make the predictions 

result<-predict(model,fifav)

#transform the data since I used log(wage)^4
result_transformed<-exp((result)^(1/4))

#replace NA with mean
result_transformed<-ifelse(is.na(result_transformed),mean(result_transformed,na.rm=T),result_transformed)
result_transformed

#output the result 
write.csv(result_transformed,"Rua.csv")



