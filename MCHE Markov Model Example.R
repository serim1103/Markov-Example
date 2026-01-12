#This script currently demonstrates how to make a simply Markov model with
#PSA in R. It currently only shows the comparator arm but I'll work to add an 
#intervention. I'll also save a deterministic version

#Define number of cycles and total population
ncycles<-20
nstates<-4
n<-10000
discountrate<-0.05
mcruns<-1000

#Set-up a matrix to record the PSA runs
outputs<-matrix(ncol=2,
                nrow=mcruns)
colnames(outputs)<-c("costs","qalys")

#Open the PSA loop
for (j in 1:mcruns){

#Define transition probabilities for comparator making sure they sum to 1
probAtoB<-rbeta(1,202,1000-202)
probAtoC<-rbeta(1,67,1000-67)
probAtoD<-rbeta(1,10,990)
probAtoA<-1-(probAtoB+probAtoC+probAtoD)

probBtoC<-rbeta(1,581,1000-581)
probBtoD<-rbeta(1,407,1000-407)
probBtoB<-1-(probBtoC+probBtoD)

probCtoD<-rbeta(1,250,750)
probCtoC<-1-probCtoD

#Costs
costHIVA<-7119
costHIVB<-8414
costAIDS<-13370
costdeath<-0

#QoL
qolHIVA<-0.9
qolHIVB<-0.7
qolAIDS<-0.3
qoldeath<-0

#State names
statenames<-c("HIVA","HIVB","AIDS","Dead")

#Define transition matrix comparator
transmatcomp<-matrix(c(probAtoA,probAtoB,probAtoC,probAtoD,
                       0,probBtoB,probBtoC,probBtoD,
                       0,0,probCtoC,probCtoD,
                       0,0,0,1),
                     nrow=4,byrow=T)

#Add state names to rows and columns
rownames(transmatcomp)<-colnames(transmatcomp)<-statenames

#Define starting distribution
startdist<-c(n,0,0,0)

#Set-up markov trace
markovtracecomp<-matrix(data=NA,
                    nrow=ncycles+1,
                    ncol=nstates,
                    dimnames=list(NULL,statenames))
markovtracecomp[1,]<-startdist

#Create list of transitions
translistcomp<-list()

#Run markov model to fill the trace
for (i in 1:ncycles){
  markovtracecomp[1+i,]<-markovtracecomp[i,] %*% transmatcomp
  translistcomp[[i]]<-markovtracecomp[i,]*transmatcomp
}

#Change the matrix into a data frame as this is easier to work with
markovtracecomp<-as.data.frame(markovtracecomp)

#Add costs
#Calculate discount rate
markovtracecomp["year"]<-c(0:ncycles)
markovtracecomp["disc"]<- 1/((1+discountrate)^markovtracecomp["year"])

markovtracecomp["cost"]<-(markovtracecomp["HIVA"]*costHIVA)+
                         (markovtracecomp["HIVB"]*costHIVB)+
                         (markovtracecomp["AIDS"]*costAIDS)

#Add QALYs
markovtracecomp["qaly"]<-(markovtracecomp["HIVA"]*qolHIVA)+
                          (markovtracecomp["HIVB"]*qolHIVB)+
                          (markovtracecomp["AIDS"]*qolAIDS)

#Calculate discounted costs and QALYs
markovtracecomp["disccost"]<-markovtracecomp["cost"]*
                              markovtracecomp["disc"]
markovtracecomp["discqaly"]<-markovtracecomp["qaly"]*
                              markovtracecomp["disc"]

#Calculate Outputs
totaloutputscomp<-c(sum(markovtracecomp["disccost"]),
                    sum(markovtracecomp["discqaly"]))
expectedcomp<-totaloutputscomp/n

#Fill outputs matrix with output in mcrun
outputs[j,]<-expectedcomp
print(paste(j/10,"%"))
}
