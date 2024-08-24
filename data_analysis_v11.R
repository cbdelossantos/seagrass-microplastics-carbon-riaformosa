#### METADATA ####     
## Authors:            Carmen B. de los Santos (cbsantos@ualg.pt)
## Version track:      Faro, 14/04/2022
## Version track:      Faro, 11/05/2022
## Version track:      Faro, 16/05/2022 
## version track:      Faro, 02/06/2022
## version track:      Faro, 05/06/2022
## version track:      Faro, 13/06/2022
## version track:      Faro, 20/06/2022 - Carmen's modifications
## version track:      Faro, 20/06/2022 - Carmen and Charlotte's modifications
## version track:      Faro, 10/05/2023 - Carmen's modifications
## version track:      Faro, 01/03/2024 - Carmen's inputs
## version track:      Faro, 02/07/2024 - Calculation stocks only FTIR-identified MPs, review all script
## version track:      Faro, 22/07/2024 - Minor aesthetics changes in plots
## version track:      Faro, 24/08/2024 - Version for GitHub

#### SETTINGS ####

# libraries
packages <- c("tidyverse",      # for data science (general)
              "gridExtra",      # extra plots
              "plyr",           # for ddply (statistical summary)
              "tidyr",          # reshape
              "reshape2",       # reshape
              "xlsx",           # for saving xlsx files
              "readxl", 
              "MESS",
              "lme4",           # for mixed models
              "lmerTest",       # to obtain p-values in mixed models
              "tidyr",          # for reading xlsx files
              "jtools",         # for weighted sd
              "RColorBrewer")

for (i in seq_along(packages)) {
  if(!do.call(require, list(package = packages[i]))) {
    do.call(install.packages, list(pkgs = packages[i]))
    do.call(require, list(package = packages[i]))
  }
}

# clean
rm(list=ls())

# working directory Carmen
setwd("~/OneDrive - Universidade do Algarve/Trabajo-OneDrive/publications/wip/supervisions/charlotte-plastics/wordir/")

# theme
theme_custom <- theme(plot.background=element_blank()) +
  theme(panel.background=element_rect(fill="white",colour="black")) +
  theme(strip.background=element_blank()) +
  theme(strip.text=element_text(size=9,colour="black",angle=0)) +
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(axis.text.x=element_text(colour="black",size=14,angle=0)) +
  theme(axis.text.y=element_text(colour="black",size=14,angle=0)) +
  theme(axis.title.x=element_text(size=16,vjust=0.2)) +
  theme(axis.title.y=element_text(size=16)) +
  theme(plot.margin= unit(c(0.5,0.5,0.5,0.5),"lines")) +
  theme(legend.background=element_blank()) +
  theme(legend.key=element_blank()) +
  theme(legend.key.height=unit(0.8,"line")) + 
  theme(legend.text.align=0) +
  theme(plot.title=element_text(size=16,face="bold")) +
  theme(plot.tag=element_text(size=20,face="bold"))

#### CITING ####

# citation example: "All analyses were performed using R Statistical Software (v4.1.2; R Core Team 2021)"
# R
citation()
version$version.string
print(citation(),style="text")

# RStudio
RStudio.Version()

#### ------------------------------------- PREPARATION DATA ---------------------------####
#### PREPARE DATA CORES ####

# note: for GitHub users without master excel file - run only the last command in this section

# load data
data.cor <- read_excel("./data/data_charlotte_20240703.xlsx",sheet="cores",na="NA",skip=3)

# select columns
names(data.cor)
data.cor <- data.cor[,c("core_id","core_id_new","species","replicate","actual_length","compaction_perc")]

# add label for figures
data.cor$habitat_label <- ifelse(data.cor$species=="Zostera noltei","Intertidal (Zn)","Subtidal (Cn)")
data.cor$habitat_label <- as.factor(data.cor$habitat_label)

# save data as csv
write_csv(data.cor,"./data/raw/data_cores.csv")
rm(data.cor)

# for users without master excel file - use only this command
data.cor <- read_csv("./data/raw/data_cores.csv")

#### PREPARE DATA CARBON ####

# note: for GitHub users without master excel file - run only the last command in this section

# load data
data.dep <- read_excel("./data/data_charlotte_20240703.xlsx",sheet="depthseries",na="NA",skip=3)

# select columns
names(data.dep)
data.dep <- data.dep[,c("core_id","sample_id",
                        "depth_middle","depth_min","depth_max",
                        "sample_volume","sample_dw",
                        "percentage_organic_matter",
                        "percentage_organic_carbon",
                        "bag_id",
                        "weight_sample_mp"
)]

# calculate dry bulk density
data.dep$dry_bulk_density <- data.dep$sample_dw/data.dep$sample_volume

# check data structure
str(data.dep)

# add info to data.dep
data.dep <- merge(data.cor,data.dep,by="core_id")

# convert some columns into factors
data.dep$core_id <- as.factor(data.dep$core_id)
levels(data.dep$core_id)
data.dep$replicate <- as.factor(data.dep$replicate)

# add label for figures
data.dep$habitat_label <- ifelse(data.dep$species=="Zostera noltei","Intertidal (Zn)","Subtidal (Cn)")
data.dep$habitat_label <- as.factor(data.dep$habitat_label)

# check samples-cores-species
table(data.dep$core_id,data.dep$species)

# save data as csv
write_csv(data.dep,"./data/raw/data_samples.csv")
rm(data.dep)

# for users without master excel file - use only this command
data.dep <- read_csv("./data/raw/data_samples.csv")

#### ------------------------------------- PREPARATION DATA VISUAL ---------------------------####
#### PREPARE DATA VISUAL - ITEMS ####

# note: for GitHub users without master excel file - run only the last command in this section

# load data
data.vis <- read_excel("./data/data_charlotte_20240703.xlsx",sheet="visual",na="NA",skip=3)

# select columns
names(data.vis)
data.vis <- data.vis[,c("core_id","type","cycle","sample_id","bag_id","filter_id","filter_area",
                        "colour","shape","major","minor")]

# exclude items > 5000 μm
nrow(data.vis[data.vis$major >= 5000,]) # number of items larger than 5000 μm (major axis)
nrow(data.vis[data.vis$minor >= 5000,]) # number of items larger than 5000 μm (minor axis)
data.vis <- data.vis[data.vis$major <= 5000,]
data.vis <- data.vis[data.vis$minor <= 5000,]

# exclude a NA row
data.vis <- data.vis[is.na(data.vis$major)==F,]

# add new core_id and species
info <- unique(data.dep[,c("core_id_new","core_id","species")])
data.vis <- merge(data.vis,info,by="core_id",all=T)

# add label for figures
data.vis$habitat_label <- ifelse(data.vis$species=="Zostera noltei","Intertidal (Zn)","Subtidal (Cn)")
data.vis$habitat_label <- as.factor(data.vis$habitat_label)

# samples in data.dep
sam.dep <- unique(data.dep$sample_id)
sam.dep

# samples in data.vis
sam.vis <- unique(data.vis$sample_id[data.vis$type!="control"])
sam.vis

# samples in data.dep that are not in data.vis (= samples with 0 particles found)
sam.dep[!sam.dep %in% sam.vis]

# clean
rm(info,sam.dep,sam.vis)

# create unique id for each item
data.vis$visual_id <- paste0("visual_",1:nrow(data.vis))

# save data as csv
write_csv(data.vis,"./data/raw/data_particles_visual.csv")
rm(data.vis)

# for users without master excel file - use only this command
data.vis <- read_csv("./data/raw/data_particles_visual.csv")

#### PREPARE DATA VISUAL - PER SAMPLE - SHAPE ####

# 5 categories: pellet, fragment, foam, film, line

# 1) count per shape and filter unit
unique(data.vis$shape)
data.vis.fil.sha <- ddply(data.vis,.(core_id,type,cycle,sample_id,filter_id,filter_area,shape),summarise,
                          n_total=length(filter_id))

# 2) correct with filter area
data.vis.fil.sha$n_total <- data.vis.fil.sha$n_total/data.vis.fil.sha$filter_area

# 3) reshape to have type categories as columns
data.vis.fil.sha <- spread(data.vis.fil.sha,shape,n_total)

# 4) replace NAs by 0 in the columns of type categories
data.vis.fil.sha$line[is.na(data.vis.fil.sha$line)] <- 0
data.vis.fil.sha$film[is.na(data.vis.fil.sha$film)] <- 0
data.vis.fil.sha$fragment[is.na(data.vis.fil.sha$fragment)] <- 0
data.vis.fil.sha$pellet[is.na(data.vis.fil.sha$pellet)] <- 0
data.vis.fil.sha$foam[is.na(data.vis.fil.sha$foam)] <- 0

# 5) calculate total number of items per filter (this is already corrected by the fraction of the filter that was analysed)
data.vis.fil.sha$n_total <- rowSums(data.vis.fil.sha[,c("line","film","fragment","pellet","foam")],na.rm=T)

# 6) express by sample units
data.vis.sam.sha <- ddply(data.vis.fil.sha,.(core_id,type,cycle,sample_id),summarise,
                          line=sum(line),
                          film=sum(film),
                          fragment=sum(fragment),
                          pellet=sum(pellet),
                          foam=sum(foam))

# 7) calculate total number of items per sample
data.vis.sam.sha$n_total <- rowSums(data.vis.sam.sha[,c("line","film","fragment","pellet","foam")],na.rm=T)

# 8) express quantities in each shape category as percentage from total (uncorrected)
data.vis.sam.sha$perc_line     <- ifelse(data.vis.sam.sha$n_total==0,0,100*data.vis.sam.sha$line/data.vis.sam.sha$n_total)
data.vis.sam.sha$perc_film     <- ifelse(data.vis.sam.sha$n_total==0,0,100*data.vis.sam.sha$film/data.vis.sam.sha$n_total)
data.vis.sam.sha$perc_fragment <- ifelse(data.vis.sam.sha$n_total==0,0,100*data.vis.sam.sha$fragment/data.vis.sam.sha$n_total)
data.vis.sam.sha$perc_pellet   <- ifelse(data.vis.sam.sha$n_total==0,0,100*data.vis.sam.sha$pellet/data.vis.sam.sha$n_total)
data.vis.sam.sha$perc_foam     <- ifelse(data.vis.sam.sha$n_total==0,0,100*data.vis.sam.sha$foam/data.vis.sam.sha$n_total)

# 9) select data for controls only
data.vis.con <- data.vis.sam.sha[data.vis.sam.sha$type=="control",c("cycle","n_total")]
colnames(data.vis.con) <- c("cycle","n_control")

# 10) select data for sediments only
data.vis.sam.sha.fin <- data.vis.sam.sha[data.vis.sam.sha$type!="control",]

# 11) add control information to the sediment data
data.vis.sam.sha.fin <- merge(data.vis.sam.sha.fin,data.vis.con,by="cycle")

# 12) correct n_total with n_control
data.vis.sam.sha.fin$n_total_corrected <- data.vis.sam.sha.fin$n_total-data.vis.sam.sha.fin$n_control

# 13) check if there is any n_total_corrected negative value
table(data.vis.sam.sha.fin$n_total_corrected<0)

# 14) replace negative values with 0
data.vis.sam.sha.fin$n_total_corrected <- ifelse(data.vis.sam.sha.fin$n_total_corrected<0,0,data.vis.sam.sha.fin$n_total_corrected)
table(data.vis.sam.sha.fin$n_total_corrected<0) # checked!

# 15) rename, select variable, and clean
data.vis.sha <- data.vis.sam.sha.fin
data.vis.sha$cycle <- data.vis.sha$type <- data.vis.sha$n_control <- NULL
rm(data.vis.con,data.vis.fil.sha,data.vis.sam.sha,data.vis.sam.sha.fin)

# 16) check number of samples per core
table(data.vis.sha$core_id)

# 17) add samples in which particles were not found
names(data.vis.sha)
samples.zero <- data.frame(core_id=c("CH4","CH4","CH6","CH9"),
                           sample_id=c("CH4_0","CH4_6","CH6_60","CH9_66"),
                           line=0,film=0,fragment=0,pellet=0,foam=0,n_total=0,
                           perc_line=0,perc_film=0,perc_fragment=0,perc_pellet=0,perc_foam=0,n_total_corrected=0)
data.vis.sha <- rbind(data.vis.sha,samples.zero)
rm(samples.zero)
table(data.vis.sha$core_id)

# 18) add depths for each sample
info <- data.dep[,c("core_id_new","sample_id","species","depth_min","depth_max","depth_middle","replicate")]
data.vis.sha <- merge(data.vis.sha,info,by="sample_id")

# 19) arrange samples
data.vis.sha <- arrange(data.vis.sha,core_id,depth_middle)
rm(info)

#### PREPARE DATA VISUAL - PER SAMPLE - COLOUR ####

# 8 categories: black_grey, blue_green, brown_tan, orange_pink_red, transparent, white_cream, opaque, brown_tan, yellow

# 1) count per colour and filter unit
unique(data.vis$colour)
data.vis.fil.col <- ddply(data.vis,.(core_id,type,cycle,sample_id,filter_id,filter_area,colour),summarise,
                          n_total=length(filter_id))

# 2) correct with filter area
data.vis.fil.col$n_total <- data.vis.fil.col$n_total/data.vis.fil.col$filter_area

# 3) reshape to have type categories as columns
data.vis.fil.col <- spread(data.vis.fil.col,colour,n_total)

# 4) replace NAs by 0 in the columns of type categories
data.vis.fil.col$black_grey[is.na(data.vis.fil.col$black_grey)] <- 0
data.vis.fil.col$blue_green[is.na(data.vis.fil.col$blue_green)] <- 0
data.vis.fil.col$brown_tan[is.na(data.vis.fil.col$brown_tan)] <- 0
data.vis.fil.col$opaque[is.na(data.vis.fil.col$opaque)] <- 0
data.vis.fil.col$orange_pink_red[is.na(data.vis.fil.col$orange_pink_red)] <- 0
data.vis.fil.col$transparent[is.na(data.vis.fil.col$transparent)] <- 0
data.vis.fil.col$white_cream[is.na(data.vis.fil.col$white_cream)] <- 0
data.vis.fil.col$yellow[is.na(data.vis.fil.col$yellow)] <- 0

# 5) calculate total number of items per filter (this is already corrected by the fraction of the filter that was analysed)
data.vis.fil.col$n_total <- rowSums(data.vis.fil.col[,c(
  "black_grey","blue_green","brown_tan", "opaque","transparent","white_cream", 
  "orange_pink_red", "white_cream", "yellow")],na.rm=T)

# 6) express by sample units
data.vis.sam.col <- ddply(data.vis.fil.col,.(core_id,type,cycle,sample_id),summarise,
                          black_grey=sum(black_grey),
                          blue_green=sum(blue_green),
                          brown_tan=sum(brown_tan),
                          opaque=sum(opaque),
                          transparent=sum(transparent),
                          orange_pink_red=sum(orange_pink_red),
                          white_cream=sum(white_cream),
                          yellow=sum(yellow))

# 7) calculate total number of items per sample
data.vis.sam.col$n_total <- rowSums(data.vis.sam.col[,c("black_grey","blue_green","brown_tan","opaque","orange_pink_red", "transparent", "white_cream", "yellow")],na.rm=T)

# 8) express quantities in each colour category as percentage from total (uncorrected)
data.vis.sam.col$perc_black_grey      <- ifelse(data.vis.sam.col$n_total==0,0,100*data.vis.sam.col$black_grey/data.vis.sam.col$n_total)
data.vis.sam.col$perc_blue_green      <- ifelse(data.vis.sam.col$n_total==0,0,100*data.vis.sam.col$blue_green/data.vis.sam.col$n_total)
data.vis.sam.col$perc_brown_tan       <- ifelse(data.vis.sam.col$n_total==0,0,100*data.vis.sam.col$brown_tan/data.vis.sam.col$n_total)
data.vis.sam.col$perc_opaque          <- ifelse(data.vis.sam.col$n_total==0,0,100*data.vis.sam.col$opaque/data.vis.sam.col$n_total)
data.vis.sam.col$perc_orange_pink_red <- ifelse(data.vis.sam.col$n_total==0,0,100*data.vis.sam.col$orange_pink_red/data.vis.sam.col$n_total)
data.vis.sam.col$perc_transparent     <- ifelse(data.vis.sam.col$n_total==0,0,100*data.vis.sam.col$transparent/data.vis.sam.col$n_total)
data.vis.sam.col$perc_white_cream     <- ifelse(data.vis.sam.col$n_total==0,0,100*data.vis.sam.col$white_cream/data.vis.sam.col$n_total)
data.vis.sam.col$perc_yellow          <- ifelse(data.vis.sam.col$n_total==0,0,100*data.vis.sam.col$yellow/data.vis.sam.col$n_total)

# 9) select data for controls only
data.vis.con <- data.vis.sam.col[data.vis.sam.col$type=="control",c("cycle","n_total")]
colnames(data.vis.con) <- c("cycle","n_control")

# 10) select data for sediments only
data.vis.sam.col.fin <- data.vis.sam.col[data.vis.sam.col$type!="control",]

# 11) add control information to the sediment data
data.vis.sam.col.fin <- merge(data.vis.sam.col.fin,data.vis.con,by="cycle")

# 12) correct n_total with n_control
data.vis.sam.col.fin$n_total_corrected <- data.vis.sam.col.fin$n_total-data.vis.sam.col.fin$n_control

# 13) check if there is any n_total_corrected negative value
table(data.vis.sam.col.fin$n_total_corrected<0) # three values

# 14) replace negative values with 0
data.vis.sam.col.fin$n_total_corrected <- ifelse(data.vis.sam.col.fin$n_total_corrected<0,0,data.vis.sam.col.fin$n_total_corrected)
table(data.vis.sam.col.fin$n_total_corrected<0) # checked!

# 15) rename, select variable, and clean
data.vis.col <- data.vis.sam.col.fin
data.vis.col$cycle <- data.vis.col$type <- data.vis.col$n_control <- NULL
rm(data.vis.con,data.vis.fil.col,data.vis.sam.col,data.vis.sam.col.fin)

# 16) check number of samples per core
table(data.vis.col$core_id)

# 17) add samples in which particles were not found
names(data.vis.col)
samples.zero <- data.frame(core_id=c("CH4","CH4","CH6","CH9"),
                           sample_id=c("CH4_0","CH4_6","CH6_60","CH9_66"),
                           black_grey=0,blue_green=0,brown_tan=0,opaque=0,transparent=0,orange_pink_red=0,
                           white_cream=0,yellow=0,n_total=0,
                           perc_black_grey=0,perc_blue_green=0,perc_brown_tan=0,perc_opaque=0,
                           perc_orange_pink_red=0,perc_transparent=0,perc_white_cream=0,perc_yellow=0,n_total_corrected=0)
data.vis.col <- rbind(data.vis.col,samples.zero)
rm(samples.zero)
table(data.vis.col$core_id)

# 18) add depth middle for each sample
info <- data.dep[,c("core_id_new","sample_id","species","depth_min","depth_max","depth_middle","replicate")]
data.vis.col <- merge(data.vis.col,info,by="sample_id")

# 19) arrange samples
data.vis.col <- arrange(data.vis.col,core_id,depth_middle)
rm(info)

#### PREPARE DATA VISUAL - PER SAMPLE ####

# merge data.vis.col and data.vis.sha
data.vis.sam <- merge(data.vis.col,data.vis.sha)

# obtain weight of samples used for microplastic analysis
weights <- data.dep[,c("sample_id","weight_sample_mp")]

# add weights of samples
data.vis.sam <- merge(data.vis.sam,weights,by="sample_id")

# calculate abundance of particles (items kg-1 dw)
data.vis.sam$abundance_ap <- 1000*data.vis.sam$n_total_corrected/data.vis.sam$weight_sample_mp

# add label for figures
data.vis.sam$habitat_label <- ifelse(data.vis.sam$species=="Zostera noltei","Intertidal (Zn)","Subtidal (Cn)")
data.vis.sam$habitat_label <- as.factor(data.vis.sam$habitat_label)

# save data
write_csv(data.vis.sam,"./data/processed/data_particles_visual_persample.csv")

# clean
rm(data.vis.col,data.vis.sha)

#### ------------------------------------- PREPARATION DATA FTIR ---------------------------####
#### PREPARE FTIR LIBRARY ####

# note: for GitHub users without master excel file - skip this section

# objective: check that all the polymers found in our samples are in the library, if not, add them.

# load library
data.lib <- read_excel("./data/data_charlotte_20240703.xlsx",sheet="library",na="NA",skip=3)

# check that all the composition full name have a polymer group
table(data.lib$polymer_group,useNA="ifany")

# load data ftir
data.fti <- read_excel("./data/data_charlotte_20240703.xlsx",sheet="ftir",na="NA",skip=3)

# list of composition full names found in the samples
list_samples_1 <- unique(data.fti$ftir_match_ref[!is.na(data.fti$ftir_match_ref)])
list_samples_2 <- unique(data.fti$ftir_match_atr[!is.na(data.fti$ftir_match_atr)])
list_samples   <- data.frame(composition_full_name=c(list_samples_1,list_samples_2),origin="samples")
list_samples   <- unique(list_samples) # because some repeated from ref and atr
rm(list_samples_1,list_samples_2)

# list of composition full names in the library
list_library <- data.frame(composition_full_name=unique(data.lib$composition_full_name),origin="library")

# merge both
list_polymers <- rbind(list_library,list_samples)
table(list_polymers$origin)

# convert from long to wide 
head(list_polymers)
list_polymers$value <- 1 # needed to convert
list_polymers <- dcast(list_polymers,composition_full_name~origin,value.var="value")

# replace NAs by 0
list_polymers$samples[is.na(list_polymers$samples)] <- 0
list_polymers$library[is.na(list_polymers$library)] <- 0

# create a new column to flag the polymers of our samples that are not in the library
list_polymers$not_in_library <- ifelse(list_polymers$samples>0 & list_polymers$library<=0,1,0)
list_polymers$in_library     <- ifelse(list_polymers$samples>0 & list_polymers$library>0,1,0)

# show those in_library
nrow(list_polymers[list_polymers$in_library==1,])
list_polymers[list_polymers$in_library==1,]

# show those not_in_library
nrow(list_polymers[list_polymers$not_in_library==1,])
list_polymers_not_in_library <- list_polymers[list_polymers$not_in_library==1,]
list_polymers_not_in_library

# clean
rm(list_library,list_polymers,list_samples,list_polymers_not_in_library,data.fti)

#### PREPARE DATA FTIR - ITEMS ####

# note: for GitHub users without master excel file - run only the last command in this section

# load data
data.fti <- read_excel("./data/data_charlotte_20240703.xlsx",sheet="ftir",na="NA",skip=3)

# select columns
names(data.fti)
data.fti <- data.fti[,c("core_id","cycle","type","sample_id","filter_id","num_ftir",
                        "colour","shape",
                        "ref_analysis","atr_analysis",
                        "ftir_match_ref","match_ref",
                        "ftir_match_atr","match_atr")]

# create unique id for each item
data.fti$ftir_id <- paste0("ftir_",1:nrow(data.fti))

# define if the polymer identified (ref or atr) are plastics or not, using the library
dataset1 <- data.fti[is.na(data.fti$ftir_match_ref)==F,c("ftir_id","ftir_match_ref")]
dataset1 <- merge(dataset1,data.lib,by.x="ftir_match_ref",by.y="composition_full_name",all.x=T)
dataset1 <- dataset1[,c("ftir_id","plastic","polymer_group")]
names(dataset1) <- c("ftir_id","plastic_ref","polymer_group_ref")

dataset2 <- data.fti[is.na(data.fti$ftir_match_atr)==F,c("ftir_id","ftir_match_atr")]
dataset2 <- merge(dataset2,data.lib,by.x="ftir_match_atr",by.y="composition_full_name",all.x=T)
dataset2 <- dataset2[,c("ftir_id","plastic","polymer_group")]
names(dataset2) <- c("ftir_id","plastic_atr","polymer_group_atr")

# add the new columns into the data.fir
data.fti <- merge(data.fti,dataset1,by="ftir_id",all.x=T)
data.fti <- merge(data.fti,dataset2,by="ftir_id",all.x=T)

# clean and arrange
rm(dataset1,dataset2,data.lib)
data.fti <- arrange(data.fti,core_id,sample_id)

# define the detection limit (%) used to accept or not the results of the ftir
detection <- 50 

# loop for final decision on plastic identification (yes/no) and polymer type
n.items <- unique(data.fti$ftir_id)
DATA <- data.frame()

for (i in 1:length(n.items)) {
  
  # select item
  data <- unique(data.fti[data.fti$ftir_id==n.items[i],])
  data
  
  # cases only with ref_analysis
  if(data$ref_analysis=="yes" & data$atr_analysis=="no"){
    
    data$ftir_match_final <- ifelse(data$match_ref>=detection,data$ftir_match_ref,"unclear")
    data$plastic_final    <- ifelse(data$match_ref>=detection,data$plastic_ref,"unclear")
    data$final_analysys   <- "ref"
    
    data$polymer_group_final <- data$polymer_group_ref
    
  }else{
    # cases only with atr_analysis
    # example ftir_135
    if(data$ref_analysis=="no" & data$atr_analysis=="yes"){ 
      data$ftir_match_final <- ifelse(data$match_atr>=detection,data$ftir_match_atr,"unclear")
      data$plastic_final     <- ifelse(data$match_atr>=detection,data$plastic_atr,"unclear")
      data$final_analysys    <- "atr"
      
      data$polymer_group_final <- data$polymer_group_atr
      
    }else{
      # cases with both ref_analysis and atr_analsysis
      # cases when both are below the defined detection threshold
      # example with ftir_70 (i=199)
      if(data$match_ref<detection & data$match_atr<detection){ 
        data$ftir_match_final <- "unclear"
        data$plastic_final    <- "unclear"
        data$final_analysys   <- "both-unclear"
        
        data$polymer_group_final <- "unclear"
        
      }else{
        # cases when both ref_analysis and atr_analsysis
        # and at least one is above the defined detection threshold > the highest is selected
        # example with ftir_30 (i = 155)
        data$ftir_match_final <- ifelse(data$match_ref>=data$match_atr,data$ftir_match_ref,data$ftir_match_atr)
        data$plastic_final    <- ifelse(data$match_ref>=data$match_atr,data$plastic_ref,data$plastic_atr)
        data$final_analysys   <- ifelse(data$match_ref>=data$match_atr,"both-but-ref-more-conclusive","both-but-atr-more-conclusive")
        
        data$polymer_group_final <- ifelse(data$match_ref>=data$match_atr,data$polymer_group_ref,data$polymer_group_atr)
      }
    }
  }
  # bind results
  DATA <- rbind(DATA,data)
}

# replace
data.fti <- DATA

# add core_id_new
data.fti <- merge(data.fti,unique(data.cor[,c("core_id","core_id_new","species")]),all=T)

# add label for figures
data.fti$habitat_label <- ifelse(data.fti$species=="Zostera noltei","Intertidal (Zn)","Subtidal (Cn)")
data.fti$habitat_label <- as.factor(data.fti$habitat_label)

# check filters in data.vis
fil.vis <- unique(data.vis$filter_id)
fil.vis

# check filters in data.fti
fil.fti <- unique(data.fti$filter_id)
fil.fti

# filters in data.vis that are not in data.fti
fil.vis[!fil.vis %in% fil.fti]

# filters in data.fti that are not in data.vis
fil.fti[!fil.fti %in% fil.vis]

# add filter area
filters <- unique(data.vis[,c("filter_id","filter_area")])
filters
data.fti <- merge(data.fti,filters,all.x=T)

# correct filter area for filter_id = c(29_2,38_3,38_4,38_5)
data.fti[is.na(data.fti$filter_area)==T,]
data.fti[is.na(data.fti$filter_area)==T,]$filter_area <-  0.25

# save
write.csv(data.fti,"./data/raw/data_particles_ftir.csv")

# difference of number of particles in data.fti and data.vis (only sediment samples)
nrow(data.fti[data.fti$type=="sediment",])-nrow(data.vis[data.vis$type=="sediment",])

# clean
rm(detection,i,n.items,data,DATA,filters,fil.fti,fil.vis)

# for users without master excel file - use only this command
data.fti <- read_csv("./data/raw/data_particles_ftir.csv")

#### PREPARE DATA FTIR - PER SAMPLE ####

# 1) count per plastic_final and filter unit
unique(data.fti$plastic_final)
data.fti.fil <- ddply(data.fti,.(core_id,type,cycle,sample_id,filter_id,filter_area,plastic_final),summarise,
                          n_total=length(filter_id))

# 2) correct with filter area
data.fti.fil$n_total <- data.fti.fil$n_total/data.fti.fil$filter_area

# 3) reshape to have type categories as columns
data.fti.fil <- spread(data.fti.fil,plastic_final,n_total)

# 4) replace NAs by 0 in the columns of type categories
data.fti.fil$no[is.na(data.fti.fil$no)] <- 0
data.fti.fil$unclear[is.na(data.fti.fil$unclear)] <- 0
data.fti.fil$yes[is.na(data.fti.fil$yes)] <- 0

# 5) calculate total number of items per filter (this is already corrected by the fraction of the filter that was analysed)
data.fti.fil$n_total <- rowSums(data.fti.fil[,c("no","unclear","yes")],na.rm=T)

# 6) express by sample units
data.fti.sam <- ddply(data.fti.fil,.(core_id,type,cycle,sample_id),summarise,
                          non_plastic=sum(no),
                          plastic=sum(yes),
                          unclear=sum(unclear))

# 7) calculate total number of items per sample
data.fti.sam$n_total <- rowSums(data.fti.sam[,c("non_plastic","plastic","unclear")],na.rm=T)

# 8) select data for controls only
data.fti.sam.con <- data.fti.sam[data.fti.sam$type=="control",c("cycle","n_total")]
colnames(data.fti.sam.con) <- c("cycle","n_control")

# 9) select data for sediments only
data.fti.sam.sed <- data.fti.sam[data.fti.sam$type!="control",]

# 10) add control information to the sediment data
data.fti.sam.sed <- merge(data.fti.sam.sed,data.fti.sam.con,by="cycle",all.x=T)

# 11) correct n_total with n_control
data.fti.sam.sed$n_total_corrected <- data.fti.sam.sed$n_total-data.fti.sam.sed$n_control

# 12) check if there is any n_total_corrected negative value
table(data.fti.sam.sed$n_total_corrected<0)

# 13) replace negative values with 0
data.fti.sam.sed$n_total_corrected <- ifelse(data.fti.sam.sed$n_total_corrected<0,0,data.fti.sam.sed$n_total_corrected)
table(data.fti.sam.sed$n_total_corrected<0) # checked!

# 14) calculate n_total_corrected but only plastics (using fraction of total particles that are plastics)
data.fti.sam.sed$n_total_corrected_plastic <- with(data.fti.sam.sed,plastic/n_total*(n_total_corrected))

# 15) rename, select variable, and clean
data.fti.sam <- data.fti.sam.sed
data.fti.sam$cycle <- data.fti.sam$type <- data.fti.sam$n_control <- NULL
rm(data.fti.sam.con,data.fti.sam.sed,data.fti.fil)

# 16) check number of samples per core
table(data.fti.sam$core_id)

# 17) add samples in which particles were not found
names(data.fti.sam)
samples.zero <- data.frame(core_id=c("CH4","CH4","CH6","CH9"),
                           sample_id=c("CH4_0","CH4_6","CH6_60","CH9_66"),
                           non_plastic=0,plastic=0,unclear=0,n_total=0,n_total_corrected=0,n_total_corrected_plastic=0)
data.fti.sam <- rbind(data.fti.sam,samples.zero)
rm(samples.zero)
table(data.fti.sam$core_id)

# 18) add depths for each sample
info <- data.dep[,c("core_id_new","sample_id","species","depth_min","depth_max","depth_middle","replicate")]
data.fti.sam <- merge(data.fti.sam,info,by="sample_id")

# 19) arrange samples
data.fti.sam <- arrange(data.fti.sam,core_id,depth_middle)
rm(info)

# 20) obtain weight of samples used for microplastic analysis
weights <- data.dep[,c("sample_id","weight_sample_mp")]

# 21) add weights of samples
data.fti.sam <- merge(data.fti.sam,weights,by="sample_id")

# 22) calculate abundance of mp particles (items kg-1 dw)
data.fti.sam$abundance_mp <- 1000*data.fti.sam$n_total_corrected_plastic/data.fti.sam$weight_sample_mp

# 23) add label for figures
data.fti.sam$habitat_label <- ifelse(data.fti.sam$species=="Zostera noltei","Intertidal (Zn)","Subtidal (Cn)")
data.fti.sam$habitat_label <- as.factor(data.fti.sam$habitat_label)

# clean
rm(weights)

# save data
write_csv(data.fti.sam,"./data/processed/data_particles_ftir_persample.csv")

#### ------------------------------------- CALCULATIONS STOCKS ---------------------------####
#### CALCULATION STOCK CARBON ####

# calculate OC densities
data.dep$oc_density <-  data.dep$percentage_organic_carbon*data.dep$dry_bulk_density/100 # units of g OC cm-3

# prepare data for all the cores
sed_samples <- data.dep[is.na(data.dep$percentage_organic_carbon)==F,
                        c("core_id",             # core ID (unique)
                          "depth_middle",        # middle sample depth (cm) compaction-corrected
                          "oc_density")]         # OC density (g cm-3) (oc_fraction*dry_bulk_density)

## 50 cm 

# stock estimation parameters
min_depth <- 0      # start of stock (cm)
max_depth <- 50     # end of stock (cm)

# how to extrapolate if your data does not reach the end of stock
# extrapolation_rule <- 1 # stops stock calculation at last recorded depth
extrapolation_rule <- 2 # takes last recorded value and extends it to the end

# OC stocks (g OC cm-2)
stocks_oc_50 <- sed_samples %>%
  # group by core 
  dplyr::group_by(core_id) %>%
  # separate the data of each core
  tidyr::nest() %>%
  # map
  dplyr::mutate(
    stock = purrr::map_dbl(
      data,
      function(df) {
        MESS::auc(
          x = df$depth_middle,
          y = df$oc_density,
          from = min_depth,
          to = max_depth,
          type = "linear",
          rule = extrapolation_rule
        )
      }
    )
  )
stocks_oc_50 <- stocks_oc_50[,c("core_id","stock")]
stocks_oc_50$depth <- max_depth
stocks_oc_50 <- merge(stocks_oc_50,data.cor)
stocks_oc_50$type <- "oc"
stocks_oc_50

## 100 cm
# stock estimation parameters
min_depth <- 0      # start of stock (cm)
max_depth <- 100     # end of stock (cm)

# how to extrapolate if your data does not reach the end of stock
# extrapolation_rule <- 1 # stops stock calculation at last recorded depth
extrapolation_rule <- 2 # takes last recorded value and extends it to the end

# OC stocks (g OC cm-2)
stocks_oc_100 <- sed_samples %>%
  
  # group by core 
  dplyr::group_by(core_id) %>%
  # separate the data of each core
  tidyr::nest() %>%
  # map
  dplyr::mutate(
    stock = purrr::map_dbl(
      data,
      function(df) {
        MESS::auc(
          x = df$depth_middle,
          y = df$oc_density,
          from = min_depth,
          to = max_depth,
          type = "linear",
          rule = extrapolation_rule
        )
      }
    )
  )
stocks_oc_100 <- stocks_oc_100[,c("core_id","stock")]
stocks_oc_100$depth <- max_depth
stocks_oc_100 <- merge(stocks_oc_100,data.cor)
stocks_oc_100$type <- "oc"
stocks_oc_100 <- arrange(stocks_oc_100,core_id_new)
stocks_oc_100

# merge 50- and 100-cm stocks (g OC cm-2)
table.sto <- rbind(stocks_oc_50,stocks_oc_100)
table.sto <- arrange(table.sto,depth,core_id_new)

# clean
rm(stocks_oc_50,stocks_oc_100,min_depth,max_depth,extrapolation_rule,sed_samples)

#### CALCULATION STOCK PARTICLES - VISUAL - ALL ####

# add number of particles to data.dep
data.dep <- merge(data.dep,data.vis.sam[,c("sample_id","n_total_corrected","abundance_ap")])

# calculate ap densities
data.dep$ap_density <-  (data.dep$abundance_ap*data.dep$dry_bulk_density)/1000 # units of items cm-3

# prepare data for all the cores
sed_samples <- data.dep[is.na(data.dep$abundance_ap)==F,
                        c("core_id",             # core ID (unique)
                          "depth_middle",        # middle sample depth (cm) compaction-corrected
                          "ap_density")]         # OC density (items cm-3)

## 50 cm 

# stock estimation parameters
min_depth <- 0      # start of stock (cm)
max_depth <- 50     # end of stock (cm)

# how to extrapolate if your data does not reach the end of stock
# extrapolation_rule <- 1 # stops stock calculation at last recorded depth
extrapolation_rule <- 2 # takes last recorded value and extends it to the end

# MP stocks (items cm-2)
stocks_ap_50 <- sed_samples %>%
  # group by core 
  dplyr::group_by(core_id) %>%
  # separate the data of each core
  tidyr::nest() %>%
  # map
  dplyr::mutate(
    stock = purrr::map_dbl(
      data,
      function(df) {
        MESS::auc(
          x = df$depth_middle,
          y = df$ap_density,
          from = min_depth,
          to = max_depth,
          type = "linear",
          rule = extrapolation_rule
        )
      }
    )
  )
stocks_ap_50 <- stocks_ap_50[,c("core_id","stock")]
stocks_ap_50$depth <- max_depth
stocks_ap_50 <- merge(stocks_ap_50,data.cor)
stocks_ap_50$type <- "ap"
stocks_ap_50

## 100 cm
# stock estimation parameters
min_depth <- 0      # start of stock (cm)
max_depth <- 100     # end of stock (cm)

# how to extrapolate if your data does not reach the end of stock
# extrapolation_rule <- 1 # stops stock calculation at last recorded depth
extrapolation_rule <- 2 # takes last recorded value and extends it to the end

# OC stocks (g OC cm-2)
stocks_ap_100 <- sed_samples %>%
  
  # group by core 
  dplyr::group_by(core_id) %>%
  # separate the data of each core
  tidyr::nest() %>%
  # map
  dplyr::mutate(
    stock = purrr::map_dbl(
      data,
      function(df) {
        MESS::auc(
          x = df$depth_middle,
          y = df$ap_density,
          from = min_depth,
          to = max_depth,
          type = "linear",
          rule = extrapolation_rule
        )
      }
    )
  )
stocks_ap_100 <- stocks_ap_100[,c("core_id","stock")]
stocks_ap_100$depth <- max_depth
stocks_ap_100 <- merge(stocks_ap_100,data.cor)
stocks_ap_100$type <- "ap"
stocks_ap_100 <- arrange(stocks_ap_100,core_id_new)
stocks_ap_100

# add 50- and 100-cm stocks (items cm-2) to table.sto
table.sto <- rbind(table.sto,stocks_ap_50,stocks_ap_100)
table.sto <- arrange(table.sto,type,depth,core_id_new)

# save data
write_csv(table.sto,"./results/tables/table_stocks.csv")

# clean
rm(stocks_ap_50,stocks_ap_100,min_depth,max_depth,extrapolation_rule,sed_samples)

#### CALCULATION STOCK PARTICLES - FTIR - PLASTIC ####

# add number of MP items to data.dep
data.dep <- merge(data.dep,data.fti.sam[,c("sample_id","n_total_corrected_plastic","abundance_mp")])

# calculate MP densities
data.dep$mp_density <- (data.dep$abundance_mp*data.dep$dry_bulk_density)/1000 # units of items cm-3

# prepare data for all the cores
sed_samples <- data.dep[is.na(data.dep$abundance_mp)==F,
                        c("core_id",             # core ID (unique)
                          "depth_middle",        # middle sample depth (cm) compaction-corrected
                          "mp_density")]         # OC density (items cm-3)

## 50 cm 

# stock estimation parameters
min_depth <- 0      # start of stock (cm)
max_depth <- 50     # end of stock (cm)

# how to extrapolate if your data does not reach the end of stock
# extrapolation_rule <- 1 # stops stock calculation at last recorded depth
extrapolation_rule <- 2 # takes last recorded value and extends it to the end

# MP stocks (items cm-2)
stocks_mp_50 <- sed_samples %>%
  # group by core 
  dplyr::group_by(core_id) %>%
  # separate the data of each core
  tidyr::nest() %>%
  # map
  dplyr::mutate(
    stock = purrr::map_dbl(
      data,
      function(df) {
        MESS::auc(
          x = df$depth_middle,
          y = df$mp_density,
          from = min_depth,
          to = max_depth,
          type = "linear",
          rule = extrapolation_rule
        )
      }
    )
  )
stocks_mp_50 <- stocks_mp_50[,c("core_id","stock")]
stocks_mp_50$depth <- max_depth
stocks_mp_50 <- merge(stocks_mp_50,data.cor)
stocks_mp_50$type <- "mp"
stocks_mp_50

## 100 cm
# stock estimation parameters
min_depth <- 0      # start of stock (cm)
max_depth <- 100     # end of stock (cm)

# how to extrapolate if your data does not reach the end of stock
# extrapolation_rule <- 1 # stops stock calculation at last recorded depth
extrapolation_rule <- 2 # takes last recorded value and extends it to the end

# OC stocks (g OC cm-2)
stocks_mp_100 <- sed_samples %>%
  
  # group by core 
  dplyr::group_by(core_id) %>%
  # separate the data of each core
  tidyr::nest() %>%
  # map
  dplyr::mutate(
    stock = purrr::map_dbl(
      data,
      function(df) {
        MESS::auc(
          x = df$depth_middle,
          y = df$mp_density,
          from = min_depth,
          to = max_depth,
          type = "linear",
          rule = extrapolation_rule
        )
      }
    )
  )
stocks_mp_100 <- stocks_mp_100[,c("core_id","stock")]
stocks_mp_100$depth <- max_depth
stocks_mp_100 <- merge(stocks_mp_100,data.cor)
stocks_mp_100$type <- "mp"
stocks_mp_100 <- arrange(stocks_mp_100,core_id_new)
stocks_mp_100

# add 50- and 100-cm stocks (items cm-2) to table.sto
table.sto <- rbind(table.sto,stocks_mp_50,stocks_mp_100)
table.sto <- arrange(table.sto,type,depth,core_id_new)

# save
write_csv(table.sto,"./results/tables/table_stocks.csv")

# clean
rm(stocks_mp_50,stocks_mp_100,min_depth,max_depth,extrapolation_rule,sed_samples)

#### ------------------------------------- EXPLORATORY PLOTS ---------------------------####
#### EXPLORATORY ANALYSIS CARBON ####

# profile dry bulk density
ggplot(data.dep,aes(y=dry_bulk_density,x=depth_middle,colour=habitat_label,linetype=as.factor(replicate))) +
  geom_point() + geom_line() +
  scale_y_continuous("Dry bulk dentisy (g cm-3)") +
  scale_x_reverse("Depth (cm)") +
  facet_grid(.~habitat_label) +
  coord_flip() +
  theme_custom

# profile organic matter
ggplot(data.dep,aes(y=percentage_organic_matter,x=depth_middle,colour=habitat_label,linetype=as.factor(replicate))) +
  geom_point() + geom_line() +
  scale_y_continuous("Organic matter (% dw)",limits=c(0,15)) +
  scale_x_reverse("Depth (cm)") +
  facet_grid(.~habitat_label) +
  coord_flip() +
  theme_custom

# profile organic carbon
ggplot(data.dep,aes(y=percentage_organic_carbon,x=depth_middle,colour=habitat_label,linetype=as.factor(replicate))) +
  geom_point() + geom_line() +
  scale_y_continuous("Organic carbon (% dw)",limits=c(0,5)) +
  scale_x_reverse("Depth (cm)") +
  facet_grid(.~habitat_label) +
  coord_flip() +
  theme_custom

# correlation dry bulk density and organic matter
ggplot(data.dep,aes(x=dry_bulk_density,y=percentage_organic_matter)) +
  geom_point() +
  geom_smooth(method="loess",formula="y~x") +
  scale_x_continuous("Dry bulk density (g dw cm-3)") +
  scale_y_continuous("Organic matter (% dw)") +
  theme_custom

ggplot(data.dep,aes(x=dry_bulk_density,y=percentage_organic_matter,colour=habitat_label)) +
  geom_point() +
  geom_smooth(method="loess",formula="y~x") +
  scale_x_continuous("Dry bulk density (g dw cm-3)") +
  scale_y_continuous("Organic matter (% dw)") +
  theme_custom

# stocks organic carbon per species, replicate and depth
ggplot(table.sto[table.sto$type=="oc",],aes(x=habitat_label,y=stock,fill=core_id)) +
  geom_bar(stat="identity",position="dodge") +
  facet_grid(.~depth) +
  theme_custom

# clean
dev.off()

#### EXPLORATORY ANALYSIS PARTICLES ####

# select data excluding controls
data.vis.sed <- data.vis[data.vis$type=="sediment",]
data.fti.sed <- data.fti[data.fti$type=="sediment",]

# depth profiles visual
ggplot(data.dep,aes(x=depth_middle,y=abundance_ap,colour=core_id_new)) +
  geom_point() +
  scale_y_continuous("AP abundance (items g-1 dw)") +
  scale_x_reverse("Depth (cm)") +
  coord_flip() 

# depth profiles ftir
ggplot(data.dep,aes(x=depth_middle,y=abundance_mp,colour=core_id_new)) +
  geom_point() +
  scale_y_continuous("MP abundance plastic (items g-1 dw)") +
  scale_x_reverse("Depth (cm)") +
  coord_flip() 

# colour per core
pdata <- data.frame(table(data.vis.sed$colour,data.vis.sed$core_id_new))
names(pdata) <- c("category","core_id_new","n")
ggplot(pdata,aes(x=core_id_new,y=n,fill=category)) +
  geom_col(position="fill")

# shape per core
pdata <- data.frame(table(data.vis.sed$shape,data.vis.sed$core_id_new))
names(pdata) <- c("category","core_id_new","n")
ggplot(pdata,aes(x=core_id_new,y=n,fill=category)) +
  geom_col(position="fill")

# size per core
pdata <- data.vis.sed
ggplot(pdata,aes(x=major,colour=core_id_new)) +
  geom_density()

# composition per core
pdata <- data.frame(table(data.fti.sed$polymer_group_ref,data.fti.sed$core_id_new))
names(pdata) <- c("category","core_id_new","n")
ggplot(pdata,aes(x=core_id_new,y=n,fill=category)) +
  geom_col(position="fill")

# colour per meadow
pdata <- data.frame(table(data.vis.sed$colour,data.vis.sed$species))
names(pdata) <- c("category","species","n")
ggplot(pdata,aes(x=species,y=n,fill=category)) +
  geom_col(position="fill")

# shape per meadow
pdata <- data.frame(table(data.vis.sed$shape,data.vis.sed$species))
names(pdata) <- c("category","species","n")
ggplot(pdata,aes(x=species,y=n,fill=category)) +
  geom_col(position="fill")

# size per core
pdata <- data.vis.sed
ggplot(pdata,aes(x=major,colour=species)) +
  geom_density()

# composition per meadow
pdata <- data.frame(table(data.fti.sed$polymer_group_ref,data.fti.sed$species))
names(pdata) <- c("category","species","n")
ggplot(pdata,aes(x=species,y=n,fill=category)) +
  geom_col(position="fill")

# explore correlation of minor and major (log-log)
ggplot(data.vis.sed,aes(x=major,y=minor,colour=shape)) +
  geom_point(shape=21) +
  scale_x_log10("Length major dimension (um)") +
  scale_y_log10("Length minor dimension (um)") +
  # facet_wrap(~shape) +
  theme_custom

# explore correlation shape and colour
table <- data.frame(table(data.vis.sed$colour,data.vis.sed$shape))
names(table) <- c("colour","shape","n")
ggplot(table,aes(x=colour,y=shape,fill=n)) +
  geom_tile() +
  geom_text(aes(label=n)) +
  scale_fill_gradient(low="white", high="blue") +
  scale_y_discrete("AP shape") +
  scale_x_discrete("AP colour") +
  theme_custom +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

# clean
rm(table,pdata)
dev.off()
 
#### EXPLORATORY ANALYSIS CARBON AND MICROPLASTICS ####

# OC content and abundance
ggplot(data.dep,aes(x=percentage_organic_carbon,y=abundance_ap,colour=habitat_label)) +
  geom_point(shape=21,size=2)

ggplot(data.dep,aes(x=percentage_organic_carbon,y=abundance_mp,colour=habitat_label)) +
  geom_point(shape=21,size=2)

# OC and MP densities
ggplot(data.dep,aes(x=oc_density,y=ap_density,colour=habitat_label)) +
  geom_point(shape=21,size=2)

ggplot(data.dep,aes(x=oc_density,y=mp_density,colour=habitat_label)) +
  geom_point(shape=21,size=2)

# OC and MP stocks
pdata <- table.sto[table.sto$depth==100,c("core_id_new","species","habitat_label","stock","type")]
pdata <- spread(pdata,type,stock)
ggplot(pdata,aes(x=oc,y=ap,colour=habitat_label)) +
  geom_point(shape=21,size=2)
ggplot(pdata,aes(x=oc,y=mp,colour=habitat_label)) +
  geom_point(shape=21,size=2)

# clean
rm(pdata)
dev.off()

#### ------------------------------------- ANALYSIS ---------------------------####
#### DESCRIPTIVE CORES (TABLE 1) ####

# information cores
data.cor

# organic carbon content per core
n.core     <- unique(data.dep$core_id_new)
TABLE      <- data.frame()
DATA.CORE  <- data.frame()

for(i in 1:length(n.core)){
  data.core <- data.dep[data.dep$core_id_new==n.core[i],]
  data.core <- arrange(data.core,depth_min)
  
  for(j in 1:nrow(data.core)){
    
    if(j<nrow(data.core)){
      data.core$thickness[j] <- data.core$depth_min[j+1]-data.core$depth_min[j]
    }else{
      data.core$thickness[j] <- data.core$depth_max[j]-data.core$depth_min[j]
    }
  }
  table <- data.frame(core_id_new=n.core[i],
                      n=length(data.core$percentage_organic_carbon),
                      mean=mean(data.core$percentage_organic_carbon),
                      sd=sd(data.core$percentage_organic_carbon),
                      min=min(data.core$percentage_organic_carbon),
                      max=max(data.core$percentage_organic_carbon),
                      weighted.mean=weighted.mean(data.core$percentage_organic_carbon,data.core$thickness),
                      weighted.sd=wtd.sd(data.core$percentage_organic_carbon,data.core$thickness))
  table$se          <- table$sd/sqrt(table$n)
  table$weighted.se <- table$weighted.sd/sqrt(table$n)
  TABLE <- rbind(TABLE,table)
  DATA.CORE <- rbind(DATA.CORE,data.core)
}
TABLE <- arrange(TABLE,core_id_new)
TABLE
data.dep <- DATA.CORE

write_csv(TABLE,"./results/tables/table_oc_content.csv")

# ap abundance per core
n.core     <- unique(data.vis.sam$core_id_new)
TABLE      <- data.frame()
DATA.CORE  <- data.frame()

for(i in 1:length(n.core)){
  data.core <- data.vis.sam[data.vis.sam$core_id_new==n.core[i],]
  data.core <- arrange(data.core,depth_min)
  
  for(j in 1:nrow(data.core)){
    
    if(j<nrow(data.core)){
      data.core$thickness[j] <- data.core$depth_min[j+1]-data.core$depth_min[j]
    }else{
      data.core$thickness[j] <- data.core$depth_max[j]-data.core$depth_min[j]
    }
  }
  table <- data.frame(core_id_new=n.core[i],
                      n=length(data.core$abundance_ap),
                      mean=mean(data.core$abundance_ap),
                      sd=sd(data.core$abundance_ap),
                      min=min(data.core$abundance_ap),
                      max=max(data.core$abundance_ap),
                      weighted.mean=weighted.mean(data.core$abundance_ap,data.core$thickness),
                      weighted.sd=wtd.sd(data.core$abundance_ap,data.core$thickness))
  table$se          <- table$sd/sqrt(table$n)
  table$weighted.se <- table$weighted.sd/sqrt(table$n)
  TABLE <- rbind(TABLE,table)
  DATA.CORE <- rbind(DATA.CORE,data.core)
}
TABLE <- arrange(TABLE,core_id_new)
TABLE
data.vis.sam <- DATA.CORE
write_csv(TABLE,"./results/tables/table_smp_abundance.csv")

# mp abundance per core
n.core     <- unique(data.dep$core_id_new)
TABLE      <- data.frame()
DATA.CORE  <- data.frame()

for(i in 1:length(n.core)){
  data.core <- data.dep[data.dep$core_id_new==n.core[i],]
  data.core <- arrange(data.core,depth_min)
  
  for(j in 1:nrow(data.core)){
    
    if(j<nrow(data.core)){
      data.core$thickness[j] <- data.core$depth_min[j+1]-data.core$depth_min[j]
    }else{
      data.core$thickness[j] <- data.core$depth_max[j]-data.core$depth_min[j]
    }
  }
  table <- data.frame(core_id_new=n.core[i],
                      n=length(data.core$abundance_mp),
                      mean=mean(data.core$abundance_mp,na.rm=T),
                      sd=sd(data.core$abundance_mp,na.rm=T),
                      min=min(data.core$abundance_mp,na.rm=T),
                      max=max(data.core$abundance_mp,na.rm=T),
                      weighted.mean=weighted.mean(data.core$abundance_mp,data.core$thickness,na.rm=T),
                      weighted.sd=wtd.sd(data.core$abundance_mp,data.core$thickness))
  table$se          <- table$sd/sqrt(table$n)
  table$weighted.se <- table$weighted.sd/sqrt(table$n)
  TABLE <- rbind(TABLE,table)
  DATA.CORE <- rbind(DATA.CORE,data.core)
}
TABLE <- arrange(TABLE,core_id_new)
TABLE
data.dep <- DATA.CORE
write_csv(TABLE,"./results/tables/table_mp_abundance.csv")

# clean
rm(data.core,i,j,n.core,table,TABLE,DATA.CORE)

#### PARTICLES VISUAL ####

# total number of particles in controls
nrow(data.vis[data.vis$type=="control",])

# total number of particles in all cores
nrow(data.vis[data.vis$type!="control",])

# number of particles per core
table(data.vis$core_id_new)

#### PARTICLES FTIR ####

# total number of particles in controls
nrow(data.fti[data.fti$type=="control",])

# total number of particles in all cores
nrow(data.fti[data.fti$type!="control",])

# number of particles per core
table(data.fti$core_id_new)

#### PROFILES WITH DEPTH (FIGURE 2) ####

# does OC content decrease with depth?
model <- lmer(percentage_organic_carbon~depth_middle+(1|core_id_new),data=data.dep)
summary(model) # yes

# does ap abundance decrease with depth?
model <- lmer(abundance_ap~depth_middle+(1|core_id_new),data=data.dep)
summary(model) # yes

# does mp abundance decrease with depth?
model <- lmer(abundance_mp~depth_middle+(1|core_id_new),data=data.dep)
summary(model) # yes

# oc content profile along depth per core
fig2a <- ggplot(data.dep,aes(y=percentage_organic_carbon,x=depth_middle,colour=habitat_label,linetype=as.factor(replicate))) +
  geom_line() +
  geom_point(shape=21,fill="white",size=3) +
  scale_y_continuous("OC content (% dw)",limits=c(0,5)) +
  scale_x_reverse("Depth (cm)",limits=c(100,0)) +
  facet_grid(.~habitat_label) +
  coord_flip() +
  scale_color_manual("Habitat",values=c("darkorange","purple")) +
  scale_linetype_manual("Habitat",values=c(1,2,3)) +
  theme_custom +
  theme(strip.text.x=element_text(size=16)) +
  guides(linetype="none",colour="none") +
  labs(tag="A")
fig2a

# ap abundance profile along depth per core
fig2b <- ggplot(data.dep,aes(y=abundance_ap,x=depth_middle,colour=habitat_label,linetype=as.factor(replicate))) +
  geom_line() +
  geom_point(shape=21,fill="white",size=3) +
  scale_y_continuous(bquote(SMP~abundance~(items~kg^-1~dw))) +
  scale_x_reverse("Depth (cm)",limits=c(100,0)) +
  facet_grid(.~habitat_label) +
  coord_flip() +
  scale_color_manual("Habitat",values=c("darkorange","purple")) +
  scale_linetype_manual("Habitat",values=c(1,2,3)) +
  theme_custom +
  theme(strip.text.x=element_text(size=16)) +
  guides(linetype="none",colour="none") +
  labs(tag="B")
fig2b

# mp abundance profile along depth per core
fig2c <- ggplot(data.dep,aes(y=abundance_mp,x=depth_middle,colour=habitat_label,linetype=as.factor(replicate))) +
  geom_line() +
  geom_point(shape=21,fill="white",size=3) +
  scale_y_continuous(bquote(MP~abundance~(items~kg^-1~dw))) +
  scale_x_reverse("Depth (cm)",limits=c(100,0)) +
  facet_grid(.~habitat_label) +
  coord_flip() +
  scale_color_manual("Habitat",values=c("darkorange","purple")) +
  scale_linetype_manual("Habitat",values=c(1,2,3)) +
  theme_custom +
  theme(strip.text.x=element_text(size=16)) +
  guides(linetype="none",colour="none") +
  labs(tag="C")
fig2c

# save figure
pdf(file="./results/figures/figure_2.pdf",width=5,height=12)
grid.arrange(fig2a,fig2b,fig2c,top="",nrow=3)
dev.off()

# clean
rm(fig2a,fig2b,fig2c,model)
dev.off()

#### OC CONTENT INTERTIDAL VS SUBTIDAL ####

# is there any significant difference between intertidal and subtidal OC content?
sdata <- data.dep
hist(sdata$percentage_organic_carbon)
shapiro.test(sdata$percentage_organic_carbon) # test normality (normal if > 0.5) NO
fligner.test(sdata$percentage_organic_carbon~sdata$species) # test homocedasticity (homo if > 0.5) YES
test.oc <- wilcox.test(sdata$percentage_organic_carbon~sdata$species,alternative="two.sided",paired=F) # no differences if > 0.5 NO DIF
test.oc # no

# descriptive organic carbon per habitat
table <- ddply(data.dep,.(habitat_label),summarise,
               n=length(percentage_organic_carbon),
               mean=mean(percentage_organic_carbon),
               sd=sd(percentage_organic_carbon),
               min=min(percentage_organic_carbon),
               max=max(percentage_organic_carbon),
               weighted.mean=weighted.mean(percentage_organic_carbon,thickness),
               weighted.sd=wtd.sd(percentage_organic_carbon,thickness),
               medidan=median(percentage_organic_carbon),
               mad=mad(percentage_organic_carbon))
table$se          <- table$sd/sqrt(table$n)
table$weighted.se <- table$weighted.sd/sqrt(table$n)
table

# save
write_csv(table,"./results/tables/table_oc_content_perhabitat.csv")

# clean
rm(sdata,table,test.oc)

#### SMP ABUNDANCE INTERTIDAL VS SUBTIDAL ####

# is there any significant difference between intertidal and subtidal particle abundance?
sdata <- data.dep
hist(sdata$abundance_ap)
shapiro.test(sdata$abundance_ap) # test normality (normal if > 0.5) NO
fligner.test(sdata$abundance_ap~sdata$species) # test homocedasticity (homo if > 0.5) YES
test.mp <- wilcox.test(sdata$abundance_ap~sdata$species,alternative="two.sided",paired=F) # no differences if > 0.5 NO DIF
test.mp # no

# descriptive abundance per habitat
table <- ddply(data.vis.sam,.(habitat_label),summarise,
               n=length(abundance_ap),
               mean=mean(abundance_ap),
               sd=sd(abundance_ap),
               min=min(abundance_ap),
               max=max(abundance_ap),
               weighted.mean=weighted.mean(abundance_ap,thickness),
               weighted.sd=wtd.sd(abundance_ap,thickness),
               medidan=median(abundance_ap),
               mad=mad(abundance_ap))
table$se          <- table$sd/sqrt(table$n)
table$weighted.se <- table$weighted.sd/sqrt(table$n)
table

# save
write_csv(table,"./results/tables/table_smp_abundance_perhabitat.csv")

# is there any significant difference between intertidal and subtidal particle abundance? (only top 3 cm)
sdata <- data.dep[data.dep$depth_middle<=3,]
hist(sdata$abundance_ap)
shapiro.test(sdata$abundance_ap) # test normality (normal if > 0.5) YES
fligner.test(sdata$abundance_ap~sdata$species) # test homocedasticity (homo if > 0.5) YES
test.mp <- t.test(sdata$abundance_ap~sdata$species,alternative="two.sided",paired=F) # no differences if > 0.5 NO DIF
test.mp # no

# descriptive abundance per habitat (only top 3 cm)
table <- ddply(data.vis.sam[data.vis.sam$depth_middle<=3,],.(habitat_label),summarise,
               n=length(abundance_ap),
               mean=mean(abundance_ap),
               sd=sd(abundance_ap),
               min=min(abundance_ap),
               max=max(abundance_ap),
               weighted.mean=weighted.mean(abundance_ap,thickness),
               weighted.sd=wtd.sd(abundance_ap,thickness),
               medidan=median(abundance_ap),
               mad=mad(abundance_ap))
table$se          <- table$sd/sqrt(table$n)
table$weighted.se <- table$weighted.sd/sqrt(table$n)
table

write_csv(table,"./results/tables/table_smp_abundance_perhabitat_top3cm.csv")

# clean
rm(sdata,table,test.mp)

#### MP ABUNDANCE INTERTIDAL VS SUBTIDAL ####

# is there any significant difference between intertidal and subtidal microplastic abundance?
sdata <- data.dep
hist(sdata$abundance_mp)
shapiro.test(sdata$abundance_mp) # test normality (normal if > 0.5) NO
fligner.test(sdata$abundance_mp~sdata$species) # test homocedasticity (homo if > 0.5) NO
test.mp <- wilcox.test(sdata$abundance_mp~sdata$species,alternative="two.sided",paired=F) # no differences if > 0.5 NO DIF
test.mp # no

# descriptive abundance per habitat
table <- ddply(data.dep,.(habitat_label),summarise,
               n=length(abundance_mp),
               mean=mean(abundance_mp,na.rm=T),
               sd=sd(abundance_mp,na.rm=T),
               min=min(abundance_mp,na.rm=T),
               max=max(abundance_mp,na.rm=T),
               weighted.mean=weighted.mean(abundance_mp,thickness,na.rm=T),
               weighted.sd=wtd.sd(abundance_mp,thickness),
               medidan=median(abundance_mp,na.rm=T),
               mad=mad(abundance_mp,na.rm=T))
table$se          <- table$sd/sqrt(table$n)
table$weighted.se <- table$weighted.sd/sqrt(table$n)
table

# save
write_csv(table,"./results/tables/table_mp_abundance_perhabitat.csv")

# clean
rm(sdata,table,test.mp)

#### STOCKS INTERTIDAL VS SUBTIDAL (FIGURE 3) ####

# stocks per habitat (g OC cm-2 or items MP cm-2)
table.sto.hab <- ddply(table.sto,.(depth,habitat_label,type),summarise,
                       mean=mean(stock),
                       sd=sd(stock),
                       n=length(stock),
                       min=min(stock),
                       max=max(stock),
                       medidan=median(stock),
                       mad=mad(stock))

# show values for table 1
table.sto.hab[table.sto.hab$type=="mp" & table.sto.hab$depth==100,]
table.sto.hab[table.sto.hab$type=="oc" & table.sto.hab$depth==100,]

# save
write_csv(table.sto.hab,"./results/tables/table_stocks_perhabitat.csv")

# is there any significant difference between intertidal and subtidal OC stocks?
sdata <- table.sto[table.sto$depth==100 & table.sto$type=="oc",]
hist(sdata$stock)
shapiro.test(sdata$stock) # test normality (normal if > 0.5) YES
fligner.test(sdata$stock~sdata$species) # test homocedasticity (homo if > 0.5) YES
test.st.oc <- t.test(stock~species,sdata,exact=FALSE) # no differences if > 0.5 NO DIF
test.st.oc # no

# is there any significant difference between intertidal and subtidal ap stocks?
sdata <- table.sto[table.sto$depth==100 & table.sto$type=="ap",]
hist(sdata$stock)
shapiro.test(sdata$stock) # test normality (normal if > 0.5) YES
fligner.test(sdata$stock~sdata$species) # test homocedasticity (homo if > 0.5) YES
test.st.ap <- t.test(stock~species,sdata,exact=FALSE) # no differences if > 0.5 NO DIF
test.st.ap # no

# is there any significant difference between intertidal and subtidal mp stocks?
sdata <- table.sto[table.sto$depth==100 & table.sto$type=="mp",]
hist(sdata$stock)
shapiro.test(sdata$stock) # test normality (normal if > 0.5) YES
fligner.test(sdata$stock~sdata$species) # test homocedasticity (homo if > 0.5) YES
test.st.mp <- t.test(stock~species,sdata,exact=FALSE) # no differences if > 0.5 NO DIF
test.st.mp # no

# plot stocks carbon
pdata <- table.sto[table.sto$depth==100 & table.sto$type=="oc",]
fig3a <- ggplot(pdata,aes(x=habitat_label,y=stock,fill=habitat_label)) +
  geom_boxplot() +
  stat_summary(fun=mean,geom="point",shape=18,size=3,show.legend=FALSE) + 
  scale_x_discrete("") +
  scale_y_continuous(bquote(OC~stock~100~cm~(g~cm^-2))) +
  scale_fill_manual("Habitat",values=c("darkorange","purple")) +
  annotate("text",x=2,y=1.1,label=paste0("p-value = ",round(test.st.oc$p.value,3)),size=5) +
  theme_custom +
  guides(fill="none") +
  labs(tag="A")
fig3a

# plot stocks particles
pdata <- table.sto[table.sto$depth==100 & table.sto$type=="ap",]
fig3b <- ggplot(pdata,aes(x=habitat_label,y=stock,fill=habitat_label)) +
  geom_boxplot() +
  stat_summary(fun=mean,geom="point",shape=18,size=3,show.legend=FALSE) + 
  scale_x_discrete("") +
  scale_y_continuous(bquote(SMP~stock~100~cm~(items~cm^-2))) +
  scale_fill_manual("Habitat",values=c("darkorange","purple")) +
  annotate("text",x=2,y=22,label=paste0("p-value = ",round(test.st.ap$p.value,3)),size=5) +
  theme_custom +
  guides(fill="none") +
  labs(tag="B")
fig3b

# plot stocks microplastics
pdata <- table.sto[table.sto$depth==100 & table.sto$type=="mp",]
fig3c <- ggplot(pdata,aes(x=habitat_label,y=stock,fill=habitat_label)) +
  geom_boxplot() +
  stat_summary(fun=mean,geom="point",shape=18,size=3,show.legend=FALSE) + 
  scale_x_discrete("") +
  scale_y_continuous(bquote(MP~stock~100~cm~(items~cm^-2))) +
  scale_fill_manual("Habitat",values=c("darkorange","purple")) +
  annotate("text",x=1,y=3,label=paste0("p-value = ",round(test.st.mp$p.value,3)),size=5) +
  theme_custom +
  guides(fill="none") +
  labs(tag="C")
fig3c

# save figure
pdf(file="./results/figures/figure_3.pdf",width=5,height=12)
grid.arrange(fig3a,fig3b,fig3c,top="",nrow=3)
dev.off()

# clean
rm(fig3a,fig3b,fig3c,pdata,test.st.oc,test.st.ap,test.st.mp,table.sto.hab,sdata)
dev.off()

#### PARTICLE CHARACTERISATION - GENERAL (FIGURE 4) ####

# percentages colour SMP
table <- data.frame(table(data.vis.sed$colour))
names(table) <- c("category","n")
table$percentage <- round(100*table$n/sum(table$n),1)
table
write_csv(table,"./results/tables/table_general_smp_colour.csv")

# percentages shape SMP
table <- data.frame(table(data.vis.sed$shape))
names(table) <- c("category","n")
table$percentage <- round(100*table$n/sum(table$n),1)
table
write_csv(table,"./results/tables/table_general_smp_shape.csv")

# percentages composition SMP
table <- data.frame(table(data.fti.sed$polymer_group_final))
names(table) <- c("category","n")
table$percentage <- round(100*table$n/sum(table$n),1)
table <- arrange(table,-percentage)
table
write_csv(table,"./results/tables/table_general_smp_composition.csv")

# percentage composition MP
data <- data.fti.sed[data.fti.sed$polymer_group_final!="Non-plastic" & data.fti.sed$polymer_group_final!="unclear",]
table <- data.frame(table(data$polymer_group_final))
names(table) <- c("category","n")
table$percentage <- round(100*table$n/sum(table$n),1)
table <- arrange(table,-percentage)
table
write_csv(table,"./results/tables/table_general_mp_composition.csv")

# percentage of particles under 1 mm
100*nrow(data.vis[data.vis$major<1000,])/nrow(data.vis)

# percentage of particles under 200 microns
100*nrow(data.vis[data.vis$major<200,])/nrow(data.vis)

# average thickness of lines
data <- data.vis.sed[data.vis.sed$shape=="line",]
summary(data$minor)
sd(data$minor)
nrow(data)

# percentage of particles blue-green lines
100*nrow(data.vis[data.vis$shape=="line" & data.vis$colour=="blue_green",])/nrow(data.vis)

# descriptive habitat
table <- data.frame(table(data.vis.sed$colour,data.vis.sed$species))
names(table) <- c("category","species","n")
table <- spread(table,species,n)
names(table) <- c("category","Cn","Zn")
table$perc_Cn <- round(100*table$Cn/sum(table$Cn),1)
table$perc_Zn <- round(100*table$Zn/sum(table$Zn),1)
table
chisq.test(table$Cn,table$Zn) # no
write_csv(table,"./results/tables/table_habitat_smp_colour.csv")

table <- data.frame(table(data.vis.sed$shape,data.vis.sed$species))
names(table) <- c("category","species","n")
table <- spread(table,species,n)
names(table) <- c("category","Cn","Zn")
table$perc_Cn <- round(100*table$Cn/sum(table$Cn),1)
table$perc_Zn <- round(100*table$Zn/sum(table$Zn),1)
table
chisq.test(table$Cn,table$Zn) # no
write_csv(table,"./results/tables/table_habitat_smp_shape.csv")

table <- data.frame(table(data.fti.sed$polymer_group_final,data.fti.sed$species))
names(table) <- c("category","species","n")
table <- spread(table,species,n)
names(table) <- c("category","Cn","Zn")
table$perc_Cn <- round(100*table$Cn/sum(table$Cn),1)
table$perc_Zn <- round(100*table$Zn/sum(table$Zn),1)
table
arrange(table,-perc_Cn)
arrange(table,-perc_Zn)
chisq.test(table$Cn,table$Zn) # yes
write_csv(table,"./results/tables/table_habitat_smp_composition.csv")

table <- data.frame(table(data.fti.sed$polymer_group_final,data.fti.sed$species))
names(table) <- c("category","species","n")
table <- table[table$category!="Non-plastic" & table$category!="unclear",]
table <- spread(table,species,n)
names(table) <- c("category","Cn","Zn")
table$perc_Cn <- round(100*table$Cn/sum(table$Cn),1)
table$perc_Zn <- round(100*table$Zn/sum(table$Zn),1)
table
arrange(table,-perc_Cn)
arrange(table,-perc_Zn)
chisq.test(table$Cn,table$Zn) # yes
write_csv(table,"./results/tables/table_habitat_mp_composition.csv")

# plot shape per meadow
pdata <- data.frame(table(data.vis.sed$shape,data.vis.sed$habitat_label))
names(pdata) <- c("category","species","n")
fig4a <- ggplot(pdata,aes(x=species,y=n,fill=category)) +
  geom_col(position="fill",colour="black",width=0.4) +
  scale_x_discrete("") +
  scale_y_continuous("Proportion") +
  scale_fill_brewer("",palette="Set1",labels=c("Film","Foam","Fragment",
                                               "Line","Pellet")) +
  theme_custom +
  theme(legend.position="top") + guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  theme(legend.text=element_text(size=8.3)) +
  labs(tag="A") 
fig4a

# plot colour per meadow
pdata <- data.frame(table(data.vis.sed$colour,data.vis.sed$habitat_label))
names(pdata) <- c("category","species","n")
fig4b <- ggplot(pdata,aes(x=species,y=n,fill=category)) +
  geom_col(position="fill",colour="black",width=0.4) +
  scale_x_discrete("") +
  scale_y_continuous("Proportion") +
  scale_fill_manual("",values=c("gray48","skyblue","sienna","wheat4",
                                "firebrick1","aliceblue","lemonchiffon1","gold1"),
                    labels=c("Black-Grey","Blue-Green","Brown-Tan","Opaque",
                             "Orange-Pink-Red","Transparent","While-Cream","Yellow")) +
  theme_custom +
  theme(legend.position="top") + guides(fill=guide_legend(nrow=4,byrow=TRUE)) +
  theme(legend.text=element_text(size=8.3)) +
  labs(tag="B") 
fig4b

# plot composition per meadow
pdata <- data.frame(table(data.fti.sed$polymer_group_final,data.fti.sed$habitat_label))
names(pdata) <- c("category","species","n")
pdata <- pdata[pdata$category!="unclear",]
pdata <- pdata[pdata$category!="Non-plastic",]
ncols <- 16
mypalette <- colorRampPalette(brewer.pal(8,"Set1"))(ncols)
fig4c <- ggplot(pdata,aes(x=species,y=n,fill=category)) +
  geom_col(position="fill",colour="black",width=0.4) +
  scale_x_discrete("") +
  scale_y_continuous("Proportion") +
  scale_fill_manual("",values=c(mypalette)) +
  theme_custom +
  theme(legend.position="top") + guides(fill=guide_legend(nrow=6,byrow=TRUE)) +
  theme(legend.text=element_text(size=8.3)) +
  labs(tag="C")
fig4c

# save
pdf(file="./results/figures/figure_4.pdf",width=5,height=12)
grid.arrange(fig4a,fig4b,fig4c,top="")
dev.off()

# clean
rm(fig4a,fig4b,fig4c,pdata,ncols,mypalette,table,data)
dev.off()

#### PARTICLE CHARACTERISATION - COMBINATION SHAPE-COLOUR AND SHAPE-POLYMER (FIGURE 5) ####

# shape-colour
table <- data.frame(table(data.vis.sed$colour,data.vis.sed$shape))
names(table) <- c("colour","shape","n")

fig5a <- ggplot(table,aes(x=colour,y=shape,fill=n)) +
  geom_tile() +
  geom_text(aes(label=n)) +
  scale_fill_gradient("Number of \n particles",low="white", high="orange") +
  scale_y_discrete("",labels=c("Film","Foam","Fragment","Line","Pellet")) +
  scale_x_discrete("",labels=c("Black-Grey","Blue-Green","Brown-Tan",
                               "Opaque","Orange-Pink-Red","Transparent","White-Cream","Yellow")) +
  theme_custom +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  labs(tag="A")
fig5a

# shape-polymer
table <- data.frame(table(data.fti.sed$shape,data.fti.sed$polymer_group_final))
names(table) <- c("shape","polymer","n")
table

fig5b <- ggplot(table,aes(y=shape,x=polymer,fill=n)) +
  geom_tile() +
  geom_text(aes(label=n)) +
  scale_fill_gradient("Number of \n particles",low="white", high="orange") +
  scale_y_discrete("",labels=c("Film","Foam","Fragment","Line","Pellet")) +
  scale_x_discrete("") +
  theme_custom +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  labs(tag="B")
fig5b

# save
pdf(file="./results/figures/figure_5.pdf",width=8,height=12)
grid.arrange(fig5a,fig5b,top="",nrow=2)
dev.off()

# clean
rm(table,fig5a,fig5b)
dev.off()

#### PARTICLE CHARACTERISATION - SIZE DISTRIBUTION (FIGURE 6) ####

# compare the two distributions - SMP
major.cn <- data.vis.sed[data.vis.sed$species=="Cymodocea nodosa",c("major")]
major.zn <- data.vis.sed[data.vis.sed$species=="Zostera noltei",c("major")]
ks.test(major.cn,major.zn)

# plot
fig6 <- ggplot(data.vis.sed,aes(x=major,colour=habitat_label,fill=habitat_label)) +
  geom_density() +
  scale_x_continuous(bquote(SMP~length~(mu~m))) +
  scale_y_continuous("Density") +
  scale_colour_manual("",values=c("darkorange","purple")) +
  scale_fill_manual("",values=alpha(c("darkorange","purple"),0.25)) +
  annotate("text",x=4000,y=0.001,label="p-value = 0.873",size=5) +
  theme(legend.position="top") +
  theme_custom
fig6

# save
pdf(file="./results/figures/figure_6.pdf",width=6,height=5)
grid.arrange(fig6,top="")
dev.off()

# clean
rm(major.cn,major.zn,fig6)
dev.off()

#### FTIR RESULTS - CONTROLS ####

# select data
data.fti.con <- data.fti[data.fti$type=="control",]

# number of particles analysed (controls)
nrow(data.fti.con)

# final analysis (plastic, not plastic, unclear)
table <- data.frame(table(data.fti.con$plastic_final))
names(table) <- c("plastic_final","n")
table$perc <- round(100*table$n/sum(table$n),0)
table

# number of polymers found (only those classified as "plastic_final=yes")
data.fti.con.yes <- data.fti.con[data.fti.con$plastic_final=="yes",]
table <- data.frame(table(data.fti.con.yes$polymer_group_final))
names(table) <- c("group_final","n")
table$perc <- round(100*table$n/sum(table$n),0)
arrange(table,-perc)

# clean
rm(data.fti.con.yes,data.fti.con)

#### FTIR RESULTS - SEDIMENT ####

# number of particles analysed (sediment)
nrow(data.fti.sed)

# difference in number of particles analysed under FTIR and those under visual inspection
nrow(data.vis.sed) - nrow(data.fti.sed)

# final analysis (plastic, not plastic, unclear)
table <- data.frame(table(data.fti.sed$plastic_final))
names(table) <- c("plastic_final","n")
table$perc <- round(100*table$n/sum(table$n),0)
table

# final analysis (plastic, not plastic, unclear) by shape
table <- data.frame(table(data.fti.sed$plastic_final,data.fti.sed$shape))
names(table) <- c("plastic_final","shape","n")
table <- spread(table,plastic_final,n)
table$total <- table$no+table$unclear+table$yes
table$perc_no <- round(100*table$no/table$total,0)
table$perc_yes <- round(100*table$yes/table$total,0)
table$perc_unclear <- round(100*table$unclear/table$total,0)
table

# number of polymers found (only those classified as "plastic_final=yes")
data.fti.sed.yes <- data.fti.sed[data.fti.sed$plastic_final=="yes",]
table <- data.frame(table(data.fti.sed.yes$polymer_group_final))
names(table) <- c("group_final","n")
table$perc <- round(100*table$n/sum(table$n),0)
arrange(table,-perc)

# polymer details for plastic particles
arrange(data.frame(table(data.fti.sed.yes$ftir_match_final)),-Freq)

# polymer details for non-plastic particles
data.fti.sed.non <- data.fti.sed[data.fti.sed$plastic_final=="no",]
arrange(data.frame(table(data.fti.sed.non$ftir_match_final)),-Freq)

# polymer details for unclear particles
data.fti.sed.unc <- data.fti.sed[data.fti.sed$plastic_final=="unclear",]
arrange(data.frame(table(data.fti.sed.unc$ftir_match_ref)),-Freq)

# clean
rm(data.fti.sed.yes,data.fti.sed.non,data.fti.sed.unc,table)

#### OC AND SMP or MP CORRELATION (FIGURE 7) ####

# OC content and SMP abundance 
hist(data.dep$percentage_organic_carbon)
hist(data.dep$abundance_ap)
shapiro.test(data.dep$percentage_organic_carbon) # test normality (normal if > 0.5) NO
shapiro.test(data.dep$abundance_ap) # test normality (normal if > 0.5) NO
cor.test(data.dep$percentage_organic_carbon,data.dep$abundance_ap,method = c("spearman")) # no
cor.test(data.dep$percentage_organic_carbon,data.dep$abundance_ap,method = c("pearson")) # yes

# OC content and MP abundance 
hist(data.dep$percentage_organic_carbon)
hist(data.dep$abundance_mp)
shapiro.test(data.dep$percentage_organic_carbon) # test normality (normal if > 0.5) NO
shapiro.test(data.dep$abundance_mp) # test normality (normal if > 0.5) NO
cor.test(data.dep$percentage_organic_carbon,data.dep$abundance_mp,method = c("spearman")) # no
cor.test(data.dep$percentage_organic_carbon,data.dep$abundance_mp,method = c("pearson")) # no

# plot oc-smp
fig7a <- ggplot(data.dep,aes(x=percentage_organic_carbon,y=abundance_ap,colour=habitat_label)) +
  geom_point(shape=21,size=4) +
  scale_x_continuous("OC content (% dw)",limits=c(0,5)) +
  scale_y_continuous(bquote(SMP~abundance~(items~kg^-1~dw))) +
  scale_color_manual("",values=c("darkorange","purple")) +
  annotate("text",x=4,y=850,label="p-value = 0.063",size=5) +
  theme(legend.position="top") +
  theme(legend.text=element_text(size=15)) +
  theme_custom 
fig7a

# plot oc-mp
fig7b <- ggplot(data.dep,aes(x=percentage_organic_carbon,y=abundance_mp,colour=habitat_label)) +
  geom_point(shape=21,size=4) +
  scale_x_continuous("OC content (% dw)",limits=c(0,5)) +
  scale_y_continuous(bquote(MP~abundance~(items~kg^-1~dw))) +
  scale_color_manual("",values=c("darkorange","purple")) +
  annotate("text",x=4,y=400,label="p-value = 0.059",size=5) +
  theme(legend.position="top") +
  theme(legend.text=element_text(size=15)) +
  theme_custom 
fig7b

# save figure
pdf(file="./results/figures/figure_7.pdf",width=5,height=10)
grid.arrange(fig7a,fig7b,top="")
dev.off()

# clean
rm(fig7a,fig7b)
dev.off()

####--------------------------- END ---------------------------####
rm(list=ls())
