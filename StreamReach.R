library(sf)
nclayers=st_layers("NHDPLUS_H_0303_HU4_GDB.gdb")
flows=     st_read("NHDPLUS_H_0303_HU4_GDB.gdb", layer="NHDPlusEROMMA")
#Pull out QEMA, or the gage adjusted estimates or "FlowEstEGageAdjustedMA"
#This is in cubic feet per second
flowsub=subset(flows, select=c("NHDPlusID", "QEMA"))
flowline=st_read("NHDPLUS_H_0303_HU4_GDB.gdb", layer="NHDFlowline")
flowlineVAA=st_read("NHDPLUS_H_0303_HU4_GDB.gdb", layer="NHDPlusFlowlineVAA")
#flowlineVAA[which(flowlineVAA$HydroSeq=="250019968"),]

##Fully elucidate the main pathways using the terminal PA numbers. 

hr=st_zm(flowline)
z=flowline[which(flowline$GNIS_Name=="Big Buffalo Creek"),]
flowline[flowline$ReachCode=="03030003000636",]
sum(z$LengthKM)
names(flowlineVAA)

#Trim to haw, deep and cape fear river
hr=flowline[which(flowline$GNIS_Name=="Haw River" | flowline$GNIS_Name=="Deep River" | flowline$GNIS_Name=="Cape Fear River"),]


hr=st_zm(hr)
plot(hr)
flowsub1=merge(hr, flowsub, by="NHDPlusID", all.x=TRUE)
flowsub2=merge(hr, flowlineVAA[c("NHDPlusID", "ReachCode", "HydroSeq", "ToNode", "FromNode","UpHydroSeq", "DnHydroSeq", "TerminalPa", "TerminalFl")], by="NHDPlusID", all.x=TRUE)
head(flowsub2)

#identfy termimal PAs:

unique(flowsub2$TerminalPa)
unique(flowsub2$TerminalPa)

#trim stream reaches to ones with 


fl3=flowsub2[flowsub2$ReachCode =="03030002002409",]
sum(fl3$LengthKM)
####Info for City of Sanford
#City of Sanford: NC0024147/WQ0000543
#Population served:42000
#Estimated discharge: 4.3 MGD
#Gallons per capita per day:102.381
#Dilution Factor:4.814569
#COMID:8871528

#River segment information 
#COMID:8871528
#Name: Big Buffalo Creek
#Segment length in miles:1.802597
#CFlow:32.0891
#POTWQ:6.6521
#DF=Effluent concentration/river concentration at mixing zone


#City of Pittsboro: NC0020354 wastewater plant
#Population served:3743
#Estimated discharge: 0.34 MGD
#Gallons per capita per day:90.83
#Dilution Factor:28.03791
#COMID:8897136


###
