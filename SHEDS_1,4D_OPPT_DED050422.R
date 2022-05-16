
library(openxlsx)
#Note: Before running sheds-ht, have to do a couple of preliminary steps:
#1. First, 
#Note: Beforehand, we need to set up a custom file called the "chem" file, which contains the product's we're interested in exposure to
#The default "var" file and the "scen" files can probably be used, since they contain all products in the CPDat database. 
#However, the scen file should be checked to ensure that the relevant scenarios are indicated
#for the included products.

#1: Narrow down the 'chem" file to only the products of interest: if step 1 already done, go to step 2 and import finished file

#Load in product sheet previously created
setwd("C:/Users/ddawso01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/OPPT/WaterContaminationModel")
#New product ID's for SHEDS-HT, based on prelinary work to ID relevant ID's from product class descriptions
prodids=c("CP.0200.010", "CP.0700.040","VE.0500.010","CP.0400.040","CP.0400.030", "CP.1200.060",
          "HM.1400.010","HM.1400.012", "HM.1400.012.E","AC.0100.010",
          "HM.1400.060","HM.1400.060.E","VE.0100.010","AC.0300.010","HM.1000.010")

#Create subset of relevant product types and format for 1,4 Dixoane. Then export to excel to input weight fractions from Franklyn and/or CADTSC
chem=read.csv("SHEDS_Inputs/sourcechem_cpdat_0.1.9.csv")
chem14d=chem[which(chem$pucid%in%prodids),]
DTXSID="DTXSID4020533"
chem14dformat=NULL
for(i in 1:length(prodids)){
  sub=chem14d[chem14d$pucid==prodids[i],]
  forms=unique(sub$form)
  sub=sub[1:length(forms),]
  sub$form=forms
  sub$dtxsid=DTXSID    
chem14dformat=rbind(chem14dformat, sub)  
}
#Read in vars file to associate product type name with product type
vars=read.csv("SHEDS_Inputs/sourcevars_cpdat_0.1.9.csv")
mass=vars[vars$variable=="mass",]
chem14dformat1=merge(chem14dformat, mass[,c("pucid", "PUC_level3")], by="pucid", all.y=FALSE)

#Write out file and enter in weight fractions from risk evaluation in Excel
#write.csv(chem14dformat1, file="chemfile_Products_14D.csv")
chem14dformat1=read.csv("chemfile_Products_14D.csv")


#2: SHEDS-HT RUns
#Do a SHEDS Run for each PUC ID separately, keeping the SEED the same so its the same people. Then, we'll combine together.
#What way, we can decide whether we want to include particular kinds of products(i.e. body paint) as representative of
#a particular COU. 
#First, need to create chem files for each puc id

#Read in filled data file with filled in weight fractions
library(ShedsHT)
chem14wf=read.csv("chemfile_Products_14D.csv")
chem14wf$dtxsid=DTXSID

#Now, create directory sheds to proceed
Dir="Sheds_Run_RE_Values_6"
dir.create(Dir)
setwd(paste0("C:/Users/ddawso01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/OPPT/WaterContaminationModel/",Dir))
dir.create("inputs")
ShedsHT::unpack_SHEDS()

#Create separate chem file cv's for each puc id and put them in the input folder
for(i in prodids){
  sub=subset(chem14wf, pucid==i)
  write.csv(sub, file=paste0("inputs/chemfile_14d_", i,".csv"), row.names = FALSE)
}

#Next, create run files for all runs. Read in run file to use as template
cpdatrunfile=read.table("inputs/run_cpdat.txt") # file in the WaterContaminationModel folder

for(i in prodids){
  runfile1=cpdatrunfile
  names(runfile1)[1]="Variable,"
  runfile1$Value[which(row.names(runfile1)=="run.name")]=paste0("SHT_Run_14D_",i)
  runfile1$Value[which(row.names(runfile1)=="run.seed")]=round(runif(1,0,100000))
  runfile1$Value[which(row.names(runfile1)=="n.persons")]=10000
  runfile1$Value[which(row.names(runfile1)=="source.chem.file")]=paste0("chemfile_14d_",i,".csv")
  runfile1$Value[which(row.names(runfile1)=="source.vars.file")]="sourcevars_cpdat_0.1.9.csv"
  
  write.table(runfile1, file=paste0("inputs/SHT_RunFile_",i,".txt"), quote=FALSE)
  }


#Run sheds for each PUC ID
for(i in prodids){
  runfile=paste0("SHT_RunFile_",i,".txt")
  run(runfile)}

#3: Post Processing:#Collect and process outputs for use in water contamination model 
setwd(paste0("C:/Users/ddawso01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/OPPT/WaterContaminationModel/",Dir,"/output/"))

#Create template to build on
DTD=read.csv(paste0("SHT_Run_14D_HM.1000.010/DTXSID4020533_all.csv"))
DTD=DTD[,c("person", "gender", "age", "weight")]
#Add each product ID
for(i in prodids){
  datafile=read.csv(paste0("SHT_Run_14D_",i,"/DTXSID4020533_all.csv"))
  sub=datafile[,c("person", "gender", "age", "weight", "exp.drain")]
  names(sub)[which(names(sub)=="exp.drain")]=i
  DTD=merge(DTD, sub[,c("person",i)], by="person")}



#Convert DTD to long form
library(reshape2)
DTDlong=reshape2::melt(DTD, id.vars=c("person", "gender", "age","weight"), variable.name="pucid") 


#Create a reference table to link COUs and PUCs in the DTD table
reftab=chem14dformat1[,c("pucid", "form", "PUC_level3")]
reftab$COU=NA
#Link reftab puc levels and COU's
COUlist=c("paint", "stain", "laundry", "cleaner", "dye", "antifreeze", "soap", "foam","dishwashing")
for(i in COUlist){
  positions=grep(i, reftab$PUC_level3)
  if(i=="paint" | i=="stain"){reftab$COU[positions]<-"Paint"}else
    if(i=="laundry"){reftab$COU[positions]<-"Laundry_Detergent"} else
      if(i=="cleaner"){reftab$COU[positions]<-"Surface_Cleaner"} else
        if(i=="dye"){reftab$COU[positions]<-"Dye"} else
          if(i=="antifreeze"){reftab$COU[positions]<-"Antifreeze"} else
            if(i=="soap"){reftab$COU[positions]<-"Dish_Soap"} else
              if(i=="foam"){reftab$COU[positions]<-"SPF"} else
              {reftab$COU[positions]<-"Dishwashing_Detergent"}
}

###Combine with reftable
DTDmerge=merge(DTDlong, reftab[,c("pucid", "PUC_level3", "COU")], by="pucid")

#All COUS
#data.frame(unique(DTDmerge$PUC_level3), stringsAsFactors = FALSE)
#Template
                       c("body paint",
                       "fabric dye",
                 "bathroom cleaner",
  "automatic dishwashing detergent",
                        "dish soap",
                  "surface cleaner",
                "laundry detergent",
                       "spray foam",
                            "paint",
               "water-based paint",
    "water-based paint - exterior",
                           "stain",
                "stain - exterior",
                      "auto paint",
                      "antifreeze")

COUScreen=c("fabric dye",                     
            "bathroom cleaner", 
            "automatic dishwashing detergent",
            "dish soap",
            "surface cleaner",                
            "laundry detergent",               
            "spray foam",                     
            "paint",                           
            "water-based paint",              
            "water-based paint - exterior",                          
            "antifreeze")
            

DTDmergesub=DTDmerge[which(DTDmerge$PUC_level3%in%COUScreen==TRUE),]



DTDmergesub1=aggregate(value~COU+person, data=DTDmergesub, FUN="sum")

options(scipen = 0)
#Note: Fraction down the drain for things like Antifreeze and Spray Foam are assumed to be zero in 
#SHEDS-HT
#Dye has 100% DTD, but but it has a low use frequency(like 4 times a year) and a low use.prev(like 0.01), so the chances of it contributing are low

#Aggregate to determine median cpDTD by COU; note, including all subtypes here

sumresults=aggregate(value~COU, data=DTDmergesub1, FUN=function(x){
  qs=quantile(x, probs=c(0.01,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9, 0.99))
  mean=mean(x)
  sd=sd(x)
  return(c(qs,mean,sd))
})

sumresults=do.call("data.frame", sumresults)
names(sumresults)[2:14]=c("Q1%","Q10%","Q20%","Q30%","Q40%", "Q50%","Q60%","Q70%","Q80%","Q90%","Q99%","Mean", "SD")
sumresults




