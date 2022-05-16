#This is script uses the XLConnect package to automate the the entry of parameter values into the 
#EWISRDXL model for the purposes of sensitivity analyses and wide-scale probablistic approach to 1,4-concetrations
#in water due to different sources

suff="DED042922"

library(XLConnect)

#Parameter list for EWISRDXL model 

###Latin Hypercube Setup
#Change parameters
#POTW
TotalPop
DWConc
PropTreatRed
Bin1_POTW_COU_Total

#Specific Bin 1 cou's for POTW
Bin1_POTW_Manufacturing
Bin1_POTW_Import_and_repackaging
Bin1_POTW_Recycling
Bin1_POTW_Industrial_uses
Bin1_POTW_Functional_fluids_open_system
Bin1_POTW_Functional fluids_closed_system
Bin1_POTW_Laboratory_chemicals
Bin1_POTW_Film_cement
Bin1_POTW_Spray_polyurethane_foam
Bin1_POTW_Printing_ink_3D
Bin1_POTW_Dry_film_lubricant
Bin1_POTW_Disposal
Bin1_POTW_PETE_byproduct
Bin1_POTW_Ethoxylation_process_byproduct
Bin1_POTW_Hydraulic_fracturing

#Direct Release
Bin1_DR_Total
Bin2_DR_Total

Bin1_DR_Manufacturing
Bin1_DR_Import_and_repackaging
Bin1_DR_Recycling
Bin1_DR_Industrial_uses
Bin1_DR_Functional_fluids_open_system
Bin1_DR_Functional fluids_closed_system
Bin1_DR_Laboratory_chemicals
Bin1_DR_Film_cement
Bin1_DR_Spray_polyurethane_foam
Bin1_DR_Printing_ink_3D
Bin1_DR_Dry_film_lubricant
Bin1_DR_Disposal
Bin1_DR_PETE_byproduct
Bin1_DR_Ethoxylation_process_byproduct
Bin1_DR_Hydraulic_fracturing
Bin2_DR_Antifreeze
Bin2_DR_Dish_Soap
Bin2_DR_Dishwasher_Detergent
Bin2_DR_Spray_polyurethane_foam
Bin2_DR_Laundry_Detergent
Bin2_DR_Surface_Cleaner
Bin2_DR_Textile_Dye
Bin2_DR_Floor_Lacquer
Bin2_DR_Paint

#Flows
AgFlow
MainStemFlow

#DownStreamPWS
PropSW
PropGW
PropRW
GWConc
POTW_Eff_Conc
Treat_Reduct

#Sensitivity analysis using PRCC and Latin Hypercube Sampling Design 
library(lhs)
#Latin Hypercube Function
LHSTab=randomLHS(1000,9)


Paramtab=data.frame(  
  "TotalPop"=round(qunif(LHSTab[,1], min = 1000, max = 1000000)),
  "DWConc"=qunif(LHSTab[,2], min = 0, max = 10),
  "POTW_Treat_Reduct"=qunif(LHSTab[,3], min = 0, max = 1),
  "Bin1_POTW_COU_Total"=qunif(LHSTab[,4], min = 0, max = 100),
  "Bin1_DR_Total"=qunif(LHSTab[,5], min = 0, max = 100),
  "Bin2_DR_Total"=qunif(LHSTab[,6], min = 0, max = 100),
  "AgFlow"=qunif(LHSTab[,7], min = 0, max = 1000),
  "MainStemFlow"=LHSTab[,7]*qunif(LHSTab[,8], min = 0, max = 1),
  "PWS_Treat_Reduct"=qunif(LHSTab[,9], min = 0, max = 1)
)

####
library(XLConnect)
file="EWISRDXL_RInterface_Template_050422_COPY.xlsx" 
#Load File 
wb=loadWorkbook(file) #load workbook


Resultlist=list() #Set up result list
for(i in 1:length(Paramtab[,1])){

#Load pages  
POTW=readWorksheet(wb,3, startRow = 2, endRow = 7, endCol = 7) #pull out sheet you need
Bin1_IR=readWorksheet(wb,3, startRow=55, endRow = 135,endCol = 3) #pull out sheet you need
Bin1_DR=readWorksheet(wb,4, startRow=2, endRow = 66 ,endCol = 3)
Bin2_DR=readWorksheet(wb,4, startRow=69, endRow = 109 ,endCol = 3)
Flows=readWorksheet(wb,5, endRow = 4 ,endCol = 3)
DSPWS=readWorksheet(wb,6, endRow = 8 ,endCol = 3)


#Set parameters of the LHS
POTW$Total.Population[1]=Paramtab[i,which(names(Paramtab)=="TotalPop")]
POTW$DW.Concentration..ug.l.[1]=Paramtab[i,which(names(Paramtab)=="DWConc")]
POTW$Proportion.of.treatment.Reduction[1]=Paramtab[i,which(names(Paramtab)=="POTW_Treat_Reduct")]
#Base these parameters from population 
POTW$Municipal.Flow.Rate_influent[1]=(0.0005*POTW$Total.Population[1])
POTW$Municipal.Flow.Rate_effluent[1]=(0.0005*POTW$Total.Population[1])
Bin1_IR$kg.day[1]=Paramtab[i,which(names(Paramtab)=="Bin1_POTW_COU_Total")]
Bin1_DR$kg.day[1]=Paramtab[i,which(names(Paramtab)=="Bin1_DR_Total")]
Bin2_DR$kg.day[1]=Paramtab[i,which(names(Paramtab)=="Bin2_DR_Total")]
Flows$Aggregate.Incremental.Flow..cfps.[1]=Paramtab[i, which(names(Paramtab)=="AgFlow")]
Flows$Main.stem.flow..cpfs.[1]=Paramtab[i,which(names(Paramtab)=="MainStemFlow")]
DSPWS[which(DSPWS$Parameter=="Treat_Reduct"), "Downstream.PWS"]=Paramtab[i,which(names(Paramtab)=="PWS_Treat_Reduct")]


#Write to appropriate places
writeWorksheet(wb,POTW,sheet=3,startRow = 2) #write the worksheet to the place you need
writeWorksheet(wb,Bin1_IR,startRow=55,sheet=3) #write the worksheet to the place you need
writeWorksheet(wb,Bin1_DR,sheet=4, startRow=2) #write the worksheet to the place you need
writeWorksheet(wb,Bin2_DR,sheet=4,startRow=69) #write the worksheet to the place you need
writeWorksheet(wb,Flows,sheet=5) #write the worksheet to the place you need
writeWorksheet(wb, DSPWS,sheet=6) #write the worksheet to the place you need

#Write 
setForceFormulaRecalculation(wb,sheet=3, value=TRUE)
setForceFormulaRecalculation(wb,sheet=4, value=TRUE)
setForceFormulaRecalculation(wb,sheet=5, value=TRUE)
setForceFormulaRecalculation(wb,sheet=6, value=TRUE)
setForceFormulaRecalculation(wb,sheet=7, value=TRUE)
setForceFormulaRecalculation(wb,sheet=8, value=TRUE)

#Get Results
Results=readWorksheet(wb,8,startRow=107, endRow = 337) #pull out sheet you need

#saveWorkbook(wb, "Processed_Test1.xlsx")

Resultlist=c(Resultlist, list(Results))
}


#Extract model outputs
checklist=SWIconc=FWconc=Prop_POTW=Prop_Bin2_DR=Prop_Bin1_DR=Prop_PrevCont=NULL
for(i in 1:length(Resultlist)){
result=Resultlist[[i]]
  checklist=c(checklist,length(which(result$Output_Check=="FAIL")))
  SWIconc=c(SWIconc, result[which(result$Output_Type=="Concentration" & result$Output_Category=="System" &  
                                    result$Output_Class=="Total_SWI_Conc" &result$Output_Subclass=="SWI_Conc_ug_per_l"), "Output_Value"])
  FWconc=c(FWconc, result[which(result$Output_Type=="Concentration" & result$Output_Category=="System" &  
                                    result$Output_Class=="Total_FW_Conc" &result$Output_Subclass=="FW_ug_per_l"), "Output_Value"])
  Prop_POTW=c(Prop_POTW, result[which(result$Output_Type=="Proportion" & result$Output_Category=="System" &  
                                        result$Output_Class=="SWI_Sources_All" & result$Output_Subclass=="POTW"), "Output_Value"])
  Prop_Bin2_DR=c(Prop_Bin2_DR, result[which(result$Output_Type=="Proportion" & result$Output_Category=="System" &  
                                              result$Output_Class=="SWI_Sources_All" & result$Output_Subclass=="Bin_2_Com_DR"), "Output_Value"])
  Prop_Bin1_DR=c(Prop_Bin1_DR, result[which(result$Output_Type=="Proportion" & result$Output_Category=="System" &  
                                             result$Output_Class=="SWI_Sources_All" & result$Output_Subclass=="Bin_1_DR"), "Output_Value"])
  Prop_PrevCont=c(Prop_PrevCont, result[which(result$Output_Type=="Proportion" & result$Output_Category=="System" &  
                                                result$Output_Class=="SWI_Sources_All" & result$Output_Subclass=="Prev_SW_Contamination"), "Output_Value"])  }   

#Check outputs
check1=Resultlist[[which(checklist>0)[1]]] #Not, a couple of failures. This might be due to rounding issues. 
c1=check1[c(20,25),]
round(sum(c1$Output_Value),4)
round(check1[108,"Output_Value"],4)
check1=check1[99:108,]
sum(check1$Output_Value[1:8])
#looks like rounding issues

#Run PRCC calculations 
library(sensitivity)
library(ggplot2)
SWIconc_prcc=pcc(Paramtab, SWIconc, rank = TRUE, semi = FALSE, logistic = FALSE, nboot = 1000, conf = 0.95)
ggplot(SWIconc_prcc) +
  theme(axis.text.x = element_text(angle=90, hjust=1))

FWconc_prcc=pcc(Paramtab, FWconc, rank = TRUE, semi = FALSE, logistic = FALSE, nboot = 1000, conf = 0.95)
ggplot(FWconc_prcc) +
  theme(axis.text.x = element_text(angle=90, hjust=1))

Prop_POTW_prcc=pcc(Paramtab, Prop_POTW, rank = TRUE, semi = FALSE, logistic = FALSE, nboot = 1000, conf = 0.95)
ggplot(Prop_POTW_prcc) +
  theme(axis.text.x = element_text(angle=90, hjust=1))

Prop_Bin2_DR_prcc=pcc(Paramtab, Prop_Bin2_DR, rank = TRUE, semi = FALSE, logistic = FALSE, nboot = 1000, conf = 0.95)
ggplot(Prop_Bin2_DR_prcc) +
  theme(axis.text.x = element_text(angle=90, hjust=1))

Prop_Bin1_DR_prcc=pcc(Paramtab, Prop_Bin1_DR, rank = TRUE, semi = FALSE, logistic = FALSE, nboot = 1000, conf = 0.95)
ggplot(Prop_Bin1_DR_prcc) +
  theme(axis.text.x = element_text(angle=90, hjust=1))

Prop_PrevCont_prcc=pcc(Paramtab, Prop_PrevCont, rank = TRUE, semi = FALSE, logistic = FALSE, nboot = 1000, conf = 0.95)
ggplot(Prop_PrevCont_prcc) +
  theme(axis.text.x = element_text(angle=90, hjust=1))


#Can save workbook here
saveWorkbook(wb,file)



