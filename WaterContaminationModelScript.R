
n=10 #Number of parameter sets
DWconc_upstream=runif(n,0,1)
DTDpc_cons=runif(n,0,0.1)
Popcons=round(runif(n,10000,1000000))
DTDpc_comm=runif(n,0,0.1)
Popcomm=round(runif(n,100,1000))
Popcons_pcFR=runif(n,100,200) #New parameter: Consumer Population Flow Rate
Popcomm_pcFR=runif(n,200,300) #New parameter: Commercial Population Flow Rate
MFR=(Popcons_pcFR*Popcons+Popcomm_pcFR*Popcomm)/1e6 #New parameter: Municipal Flow rate made up of combined consumer and commercial flow rates
DTD_indu=runif(n,0,1)
SWeff_indu=runif(n,0,1)
FlowtoSR_indu=runif(n,0,1)
FlowSR=runif(n,10,100)
PropDW_SW=runif(n,0,1)
PropDW_GW=(1-PropDW_SW)*runif(n,0,1)
PropDW_WW=1-PropDW_SW-PropDW_GW
GWconc=runif(n,0,1)
WWeffconc_downstream=runif(n,0,1)
ConcPropRed_upstream=runif(n,0,0.1)
ConcPropRed_downstream=runif(n,0,0.1)
CF1=rep(1e6,n)
CF2=rep(1e9,n)
CF3=rep(3785411.8,n)
CF4=rep(2.45E+06,n)


#Calculations
DTDcons=Popcons*DTDpc_cons*CF1
DTDcomm=Popcomm*DTDpc_comm*CF1
DTDtotal=DTDcons+DTDcomm+DTD_indu*CF2
WWinfconc_upstream=(DTDtotal+DWconc_upstream*MFR*CF3)/(MFR*CF3)
WWeffconc_upstream=WWinfconc_upstream*(1-ConcPropRed_upstream)
##Proportion f upstream WWConc due to DTD
PropDTDcons=DTDcons/DTDtotal
PropDTDcomm=DTDcomm/DTDtotal
PropDTDindu=(DTD_indu*CF2)/DTDtotal
SWug=SWeff_indu*CF2 + (WWeffconc_upstream*MFR*CF3)
SWflows=(FlowSR*CF4) + ((MFR+FlowtoSR_indu)*CF3)
##SW concentration at intakes
SWconcintakes=SWug/SWflows
##Proportion of downstream SW due to upstream pathways
PropSWConcSWeffindu=(SWeff_indu*CF2)/SWug
PropSWConcDTDTotal=(WWeffconc_upstream*MFR*CF3)/SWug
#Proportion of downstream SW due to upstream DTD inputs
PropSWConcDTD_cons=PropSWConcDTDTotal*PropDTDcons
PropSWConcDTD_comm=PropSWConcDTDTotal*PropDTDcomm
PropSWConcDTD_indu=PropSWConcDTDTotal*PropDTDindu

#Drinking Water concentration
DWconc_downstream=(SWconcintakes*PropDW_SW + GWconc*PropDW_GW + WWeffconc_downstream*PropDW_WW) * (1-ConcPropRed_downstream)

#Proportion of Drinking Water Concentration due to different sources
PropDWConcGW=((PropDW_GW*GWconc)*(1-ConcPropRed_downstream))/DWconc_downstream
PropDWConcWW=((PropDW_WW*WWeffconc_downstream)*(1-ConcPropRed_downstream))/DWconc_downstream
PropDWConcSW=((PropDW_SW*SWconcintakes)*(1-ConcPropRed_downstream))/DWconc_downstream

#Proportion of downstream drinking water concentrtion due to surfacewater components
PropDWconcSW_DTDtotal=PropDWConcSW*PropSWConcDTDTotal
PropDWconcSW_DTDcons= PropDWConcSW*PropSWConcDTD_cons
PropDWconcSW_DTDcomm= PropDWConcSW*PropSWConcDTD_comm
PropDWconcSW_DTDindu= PropDWConcSW*PropSWConcDTD_indu
PropDWconcSW_Indueff= PropDWConcSW*PropSWConcSWeffindu



