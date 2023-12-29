#PubMed Venn Diagram script 

#the point of this script is to save the process I used to create a Venn Diagram of the 
#data I collected from my PubMed search. This Venn Diagram is meant to anaylise if any of the CpG
#sites or Genes significant to my article search overlap. The overlapping/repetitive 
#genes are predicted to also be significant in my own EWAS study. This will allow for 
#potential confirmation of significant genes or site from my EWAS. The anaysis might also be
#useful in generating candidate based investigations of the RODAM data in the context of 
#the Developmental Origin of Disease hypothesis for CVD.

#Set up your R session 
getwd()
dir()
setwd("~/Desktop/UvA/Research Project 2")


### WITH ALEX'S HELP ####

#upload your data as a 
getwd()
dir()
setwd("~/Desktop/UvA/Research Project 2")
data <- read.csv("PubMed Venn Data.csv", sep=";")

#Create the subsets of the different health traits that I will be comparing

setdiabetes <- data[data$Trait == "Diabetes", c("Gene")]
setobesity <- data[data$Trait == "Obesity", c("Gene")]
setCM <- data[data$Trait == "Cardiometabolic Disease", c("Gene")]
setvascular <- data[data$Trait == "Vascular Disease", c("Gene")]

setT <- data[data$Author == "Tekola-Ayele, et al.", c("probes")]
setTo <- data[data$Author == "Tobi,  E.W.", c("probes")]
setA <- data[data$Author == "Alexander, et al. ", c("probes")]
setC<- data[data$Author == "Chu, et al.", c("probes")]
setJ <- data[data$Author == "Julian et al.", c("probes")]

#To avoid having to add in the under-score manually you can use this code to add
#in a space at the end of a gene or cpg site name

setdiabetes = paste0(setdiabetes, " ")
setobesity = paste0(setobesity, " ")
setCM  = paste0(setCM, " ")
setvascular  = paste0(setvascular, " ")


setT = paste0(setT, " ")
setTo = paste0(setTo, " ")
setA = paste0(setA, " ")
setC = paste0(setC, " ")
setJ = paste0(setJ, " ")

#upload the VennDiagram library and start the diagram generation process. The library is NOT the
#venndiagram library it is the Vennerable library 

install.packages("Vennerable", repos="http://R-Forge.R-project.org")
library(Vennerable)

#Now code your Venndiagram using the Venn function
venndia_initial <- Venn(list(setdiabetes, setobesity, setvascular, setCM))
plot(venndia_initial, doWeights=TRUE)

#You have a diagram but you cannot see the gene's listed nor is it specifying the health traits.
#Let us try to clarify these points
# *gp = graphical parameters

gp <- VennThemes(compute.Venn(venndia_initial))
gp$FaceText <- lapply(gp$FaceText, function(x) {x$fontsize<-8; return(x)})
plot(venndia_initial, doWeights=TRUE, show=list(FaceText="elements"), gp=gp)

#Titles 
venndia_title <- Venn(list(setdiabetes, setobesity, sethypertension), SetNames = 
                        c("Diabetes", "Hypertension", "Obesity"))
plot(venndia_title, doWeights=TRUE, show=list(FaceText="elements"), gp=gp)

######### NOW TRY WITH CPG SITES ############

#Create the subsets for the health traits but with the CpG sites as the overlapping variable

cpg_setdiabetes <- data[data$Trait == "Diabetes", c("CpG.Site")]
cpg_setobesity <- data[data$Trait == "Obesity", c("CpG.Site")]
cpg_sethypertension <- data[data$Trait == "Hypertension", c("CpG.Site")]

#Remember to add in the spaces so the cpg sites to not run together

cpg_setdiabetes = paste0(cpg_setdiabetes, " ")
cpg_setobesity = paste0(cpg_setobesity, " ")
cpg_sethypertension = paste0(cpg_sethypertension, " ")

#Now lets try a Venn Diagram of the CpG sites with titles
venndia_cpg <- Venn(list(cpg_setdiabetes, cpg_setobesity, cpg_sethypertension), SetNames = 
                      c("Diabetes", "Hypertension", "Obesity"))
gp <- VennThemes(compute.Venn(venndia_cpg))
gp$FaceText <- lapply(gp$FaceText, function(x) {x$fontsize<-8; return(x)})
plot(venndia_cpg, doWeights=TRUE, show=list(FaceText="elements"), gp=gp)


####### CHANGING THE COLOUR #######

#To change the colour of the outline of the circle: SNYTAX

gp[["Set"]][["Set1"]][["col"]] <- "#"
gp[["Set"]][["Set2"]][["col"]] <- "#"
gp[["Set"]][["Set3"]][["col"]] <- "#"

#To change the colour of the inside of the circle: SNYTAX

gp[["Face"]][["110"]][["fill"]] <- "#"
gp[["Face"]][["100"]][["fill"]] <- "#"
gp[["Face"]][["011"]][["fill"]] <- "#"
gp[["Face"]][["010"]][["fill"]] <- "#"
gp[["Face"]][["001"]][["fill"]] <- "#"

#Now try to change the colour of your outlines as a test run
gp[["Set"]][["Set1"]][["col"]] <- "#FFFFFF"
gp[["Set"]][["Set2"]][["col"]] <- "#FFFFFF"
gp[["Set"]][["Set3"]][["col"]] <- "#FFFFFF"
venndia_title <- Venn(list(setdiabetes, setobesity, sethypertension), SetNames = 
                        c("Diabetes", "Hypertension", "Obesity"))
plot(venndia_title, doWeights=TRUE, show=list(FaceText="elements"), gp=gp)

gp[["Face"]][["110"]][["fill"]] <- "#99FFFF"
gp[["Face"]][["100"]][["fill"]] <- "#33CC66"
gp[["Face"]][["010"]][["fill"]] <- "#33CCFF"
gp[["Face"]][["001"]][["fill"]] <- "#FFFF99"

plot(venndia_title, doWeights=TRUE, show=list(FaceText="elements"), gp=gp)


##### FINAL VENN DIAGRAM FOR GENES #####
getwd()
dir()
setwd("~/Desktop/UvA/Research Project 2/PubMed")
data <- read.csv("PubMed Venn Data.csv", sep=";")

setdiabetes <- data[data$Trait == "Diabetes", c("Gene")]
setobesity <- data[data$Trait == "Obesity", c("Gene")]
sethypertension <- data[data$Trait == "Hypertension", c("Gene")]

install.packages("Vennerable", repos="http://R-Forge.R-project.org")
library(Vennerable)

venndia_gene <- Venn(list(setdiabetes, setobesity, sethypertension), 
                SetNames = c("Diabetes", "Obesity", "Hypertension"))

gp <- VennThemes(compute.Venn(venndia_gene))

gp$FaceText <- lapply(gp$FaceText, function(x) {x$fontsize<-8; return(x)})

gp[["Set"]][["Set1"]][["col"]] <- "#FFFFFF"
gp[["Set"]][["Set2"]][["col"]] <- "#FFFFFF"
gp[["Set"]][["Set3"]][["col"]] <- "#FFFFFF"
gp[["Face"]][["110"]][["fill"]] <- "#99FFFF"
gp[["Face"]][["100"]][["fill"]] <- "#CCFF99"
gp[["Face"]][["010"]][["fill"]] <- "#66CCFF"
gp[["Face"]][["001"]][["fill"]] <- "#FFFF99"

plot(venndia_gene, doWeights=TRUE, show=list(FaceText="elements"), gp=gp)

#Now I am doing the same but for the Cardiovascular diseases 

setCVD <- data[data$Trait == "Cardiovascular Disease", c("Gene")]
setCHD <- data[data$Trait == "Coronary Heart Disease", c("Gene")]
setCV_risk <-data[data$Trait == "Cardiovascular Risk", c("Gene")]

ven_cardiodisease <- Venn(list(setCHD, setCVD, setCV_risk),
                          SetNames = c("CHD", "CVD", "Cardiovascular Risk"))
gp <- VennThemes(compute.Venn(ven_cardiodisease)) #GP for CVD diseases
gp$FaceText <- lapply(gp$FaceText, function(x) {x$fontsize<-5; return(x)}) #GP for CVD diseases

gp[["Set"]][["Set1"]][["col"]] <- "#FFFFFF"
gp[["Set"]][["Set2"]][["col"]] <- "#FFFFFF"
gp[["Set"]][["Set3"]][["col"]] <- "#FFFFFF"

gp[["Face"]][["110"]][["fill"]] <- "#FF99CC"
gp[["Face"]][["100"]][["fill"]] <- "#66CCFF" 
gp[["Face"]][["011"]][["fill"]] <- "#FF99FF"
gp[["Face"]][["010"]][["fill"]] <- "#99FFFF"
gp[["Face"]][["001"]][["fill"]] <- "#00CCCC"

plot(ven_cardiodisease, doWeights=TRUE, show=list(FaceText="elements"), gp=gp)

#Test of Multiple Traits 

venn_4traits <- Venn(list(setdiabetes, setobesity, sethypertension, setCVD),
                     SetNames = c("Diabetes", "Obesity", "Hypertension", "CVD"))
gp <- VennThemes(compute.Venn(venn_4traits, doWeights = FALSE, type = "ellipses"))
plot(venn_4traits, doWeights=FALSE, show=list(FaceText="elements"), gp=gp)
plot(venn_4traits, doWeights=FALSE, gp=gp) #Shows the number of overlaps which is 
#easier to read and far more informative
gp[["Set"]][["Set1"]][["col"]] <- "#000000"
gp[["Set"]][["Set2"]][["col"]] <- "#000000"
gp[["Set"]][["Set3"]][["col"]] <- "#000000"
gp[["Set"]][["Set4"]][["col"]] <- "#000000"


gp[["Face"]][["1011"]][["fill"]] <- "#0066CC"
gp[["Face"]][["1010"]][["fill"]] <- "#33CCFF"
gp[["Face"]][["0011"]][["fill"]] <- "#33CCFF"
gp[["Face"]][["0010"]][["fill"]] <- "#99CCFF"
gp[["Face"]][["0001"]][["fill"]] <- "#99CCFF"
gp[["Face"]][["0101"]][["fill"]] <- "#33CCFF"
gp[["Face"]][["0100"]][["fill"]] <- "#99CCFF"
gp[["Face"]][["0111"]][["fill"]] <- "#0066CC"
gp[["Face"]][["0110"]][["fill"]] <- "#33CCFF"
gp[["Face"]][["1111"]][["fill"]] <- "#0000CC"
gp[["Face"]][["1110"]][["fill"]] <- "#0066CC"
gp[["Face"]][["1101"]][["fill"]] <- "#0066CC"
gp[["Face"]][["1100"]][["fill"]] <- "#33CCFF"
gp[["Face"]][["1001"]][["fill"]] <- "#33CCFF"
gp[["Face"]][["1000"]][["fill"]] <- "#99CCFF"

venn_CVD_risk <- Venn(list(setdiabetes, setobesity, sethypertension, setCV_risk),
                     SetNames = c("Diabetes", "Obesity", "Hypertension", "Cardiovascular Risk"))
gp <- VennThemes(compute.Venn(venn_CVD_risk, doWeights = FALSE, type = "ellipses"))
plot(venn_CVD_risk, doWeights=FALSE, show=list(FaceText="elements"), gp=gp)
plot(venn_CVD_risk, doWeights=FALSE, gp=gp)

gp[["Set"]][["Set1"]][["col"]] <- "#000000"
gp[["Set"]][["Set2"]][["col"]] <- "#000000"
gp[["Set"]][["Set3"]][["col"]] <- "#000000"
gp[["Set"]][["Set4"]][["col"]] <- "#000000"


gp[["Face"]][["1011"]][["fill"]] <- "#FF3333"
gp[["Face"]][["1010"]][["fill"]] <- "#FF6666"
gp[["Face"]][["0011"]][["fill"]] <- "#FF6666"
gp[["Face"]][["0010"]][["fill"]] <- "#FFCCCC"
gp[["Face"]][["0001"]][["fill"]] <- "#FFCCCC"
gp[["Face"]][["0101"]][["fill"]] <- "#FF6666"
gp[["Face"]][["0100"]][["fill"]] <- "#FFCCCC"
gp[["Face"]][["0111"]][["fill"]] <- "#FF3333"
gp[["Face"]][["0110"]][["fill"]] <- "#FF6666"
gp[["Face"]][["1111"]][["fill"]] <- "#CC0033"
gp[["Face"]][["1110"]][["fill"]] <- "#FF3333"
gp[["Face"]][["1101"]][["fill"]] <- "#FF3333"
gp[["Face"]][["1100"]][["fill"]] <- "#FF6666"
gp[["Face"]][["1001"]][["fill"]] <- "#FF6666"
gp[["Face"]][["1000"]][["fill"]] <- "#FFCCCC"
    
##### FINAL VENN DIAGRAM FOR CpG Sites #####

cpg_setdiabetes <- data[data$Trait == "Diabetes", c("CpG.Site")]
cpg_setobesity <- data[data$Trait == "Obesity", c("CpG.Site")]
cpg_sethypertension <- data[data$Trait == "Hypertension", c("CpG.Site")]

cpg_setdiabetes = paste0(cpg_setdiabetes, " ")
cpg_setobesity = paste0(cpg_setobesity, " ")
cpg_sethypertension = paste0(cpg_sethypertension, " ")

venndia_cpg <- Venn(list(cpg_setdiabetes, cpg_sethypertension, cpg_setobesity), SetNames = 
                      c("Diabetes", "Hypertension", "Obesity"))

gp <- VennThemes(compute.Venn(venndia_cpg))
gp$FaceText <- lapply(gp$FaceText, function(x) {x$fontsize<-8; return(x)})
gp[["Set"]][["Set1"]][["col"]] <- "#FFFFFF"
gp[["Set"]][["Set2"]][["col"]] <- "#FFFFFF"
gp[["Set"]][["Set3"]][["col"]] <- "#FFFFFF"
gp[["Face"]][["110"]][["fill"]] <- "#FF3333"
gp[["Face"]][["100"]][["fill"]] <- "#FF6666"
gp[["Face"]][["010"]][["fill"]] <- "#FF9900"
gp[["Face"]][["001"]][["fill"]] <- "#FFCC33"

plot(venndia_cpg, doWeights=TRUE, show=list(FaceText="elements"), gp=gp)

#Now I am doing the same but for the Cardiovascular diseases 

cpg_setCVD <- data[data$Trait == "Cardiovascular Disease", c("CpG.Site")]
cpg_setCHD <- data[data$Trait == "Coronary Heart Disease", c("CpG.Site")]
cpg_setCVD_risk <- data[data$Trait == "Cardiovascular Risk", c("CpG.Site")]

cpg_setCVD = paste0(cpg_setCVD, " ")
cpg_setCHD = paste0(cpg_setCHD, " ")
cpg_setCVD_risk = paste0(cpg_setCVD_risk, " ")

venn_cpg_CVD <- Venn(list(cpg_setCVD, cpg_setCHD, cpg_setCVD_risk),
                SetNames = c("CVD", "CHD", "Cardiovascular Risk"))

gp <- VennThemes(compute.Venn(venn_cpg_CVD))
gp$FaceText <- lapply(gp$FaceText, function(x) {x$fontsize<-8; return(x)})
gp[["Set"]][["Set1"]][["col"]] <- "#FFFFFF"
gp[["Set"]][["Set2"]][["col"]] <- "#FFFFFF"
gp[["Set"]][["Set3"]][["col"]] <- "#FFFFFF"
gp[["Face"]][["110"]][["fill"]] <- "#FF3333"
gp[["Face"]][["100"]][["fill"]] <- "#FF6666"
gp[["Face"]][["010"]][["fill"]] <- "#FF9900"
gp[["Face"]][["001"]][["fill"]] <- "#FFCC33"


plot(venn_cpg_CVD, doWeights=TRUE, show=list(FaceText="elements"), gp=gp)
plot(venn_cpg_CVD, doWeights=TRUE, gp=gp)


#######################################################################################################
#Test of Multiple Traits 

venn_4traits <- Venn(list(setvascular, setdiabetes, setobesity),
      SetNames = c("Vascular Disease", "Diabetes", "Obesity"))
gp <- VennThemes(compute.Venn(venn_4traits, doWeights = FALSE, type="ellipses"))
plot(venn_4traits, doWeights=FALSE, show=list(FaceText="elements"), gp=gp)
plot(venn_4traits, doWeights=FALSE, gp=gp) #Shows the number of overlaps which is 
#easier to read and far more informative
gp[["Set"]][["Set1"]][["col"]] <- "#000000"
gp[["Set"]][["Set2"]][["col"]] <- "#000000"
gp[["Set"]][["Set3"]][["col"]] <- "#000000"
gp[["Set"]][["Set4"]][["col"]] <- "#000000"


gp[["Face"]][["1011"]][["fill"]] <- "#0066CC"
gp[["Face"]][["1010"]][["fill"]] <- "#33CCFF"
gp[["Face"]][["0011"]][["fill"]] <- "#33CCFF"
gp[["Face"]][["0010"]][["fill"]] <- "#99CCFF"
gp[["Face"]][["0001"]][["fill"]] <- "#99CCFF"
gp[["Face"]][["0101"]][["fill"]] <- "#33CCFF"
gp[["Face"]][["0100"]][["fill"]] <- "#99CCFF"
gp[["Face"]][["0111"]][["fill"]] <- "#0066CC"
gp[["Face"]][["0110"]][["fill"]] <- "#33CCFF"
gp[["Face"]][["1111"]][["fill"]] <- "#0000CC"
gp[["Face"]][["1110"]][["fill"]] <- "#0066CC"
gp[["Face"]][["1101"]][["fill"]] <- "#0066CC"
gp[["Face"]][["1100"]][["fill"]] <- "#33CCFF"
gp[["Face"]][["1001"]][["fill"]] <- "#33CCFF"
gp[["Face"]][["1000"]][["fill"]] <- "#99CCFF"

venn_CVD_risk <- Venn(list(setdiabetes, setobesity, setvascular, setCM),
        SetNames = c("Diabetes", "Obesity","Vascular Disease", "Cardiometabolic Disease"))
gp <- VennThemes(compute.Venn(venn_CVD_risk, doWeights = FALSE, type = "ellipses"))
plot(venn_CVD_risk, doWeights=FALSE, show=list(FaceText="elements"), gp=gp)
plot(venn_CVD_risk, doWeights=FALSE, gp=gp)
     