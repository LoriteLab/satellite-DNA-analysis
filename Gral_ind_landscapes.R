library(reshape)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(tidyverse)
library(gridExtra)
library(ggforce)

# read the tbl file
tbl_files <- list.files(pattern = "\\.tbl$", full.names = TRUE)
tbl_content <- readLines(tbl_files)

# take the genome info from tbl file
genome_size_line <- grep("total length:", tbl_content)
genome_size_info <- tbl_content[genome_size_line]
genome_size <- as.numeric(sub(".*total length:\\s+(\\d+)\\s+bp.*", "\\1", genome_size_info))


# read the divsum file
divsum_file <- list.files(pattern = "\\.divsum$", full.names = TRUE)[1]

# KimuraDistance
divsum_content <- readLines(divsum_file)

# take divergence info
div_start_line <- grep("^Div", divsum_content)
KimuraDistance <- read.table(text = paste(divsum_content[div_start_line:length(divsum_content)], collapse = "\n"), header = TRUE)

# genome size in bp
genomes_size <- genome_size

kd_melt <- melt(KimuraDistance, id = "Div")
kd_melt$norm <- kd_melt$value / genomes_size * 100

# delete "Satellite/" and change "." by "-" in kd_melt$variable column
kd_melt$variable <- gsub("Satellite\\.", "", kd_melt$variable)
kd_melt$variable <- gsub("\\.", "-", kd_melt$variable)



#plot. You can change the colors by palletes or manual values
ggplot(kd_melt, aes(y=norm, x=Div, fill = variable)) + 
  ggtitle("Arvicola amphibius")+ # fill with the title you want
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+ # reverse
 # geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values=c( # Blues
    "#003F5C", "#075985", "#0C6E9C", "#147EA8", "#1B91B5",
    "#229FBF", "#2AAAC7", "#35B5CC", "#43BDD0", "#55C4D3",
    "#67CBD5", "#7AD1D7", "#8DD7D9", "#A0DDDC", "#B3E3DF",
    
    # Blue-greens
    "#005F73", "#087F8C", "#10989B", "#19A7A5", "#25B5A9",
    "#35C0AB", "#46C8A9", "#58CEA6", "#6BD3A3", "#7FD7A0",
    "#93DA9D", "#A7DD9B", "#BBE09A", "#CDE39A", "#DCE69B",
    
    # Greens
    "#006837", "#168344", "#29974A", "#3BA64D", "#4CB44E",
    "#5CC14D", "#6BCB4C", "#7BD24D", "#8AD84E", "#99DD50",
    "#A8E153", "#B7E557", "#C5E85C", "#D2EB62", "#DDEB6A",
    
    # Yellow-greens
    "#4D7300", "#668800", "#809B00", "#99AD00", "#B0BD00",
    "#C5CA00", "#D5D400", "#E2DC00", "#EBE300", "#F1E900",
    "#F5ED1A", "#F7F02F", "#F8F244", "#F9F45A", "#FAF570",
    
    # Yellows
    "#FFD600", "#FFD000", "#FFC900", "#FFC200", "#FFBB00",
    "#FFB300", "#FFAB00", "#FFA300", "#FF9B00", "#FF9300",
    "#FF8B00", "#FF8300", "#FF7B00", "#FF7300", "#FF6B00",
    
    # Oranges
    "#E85D04", "#ED6705", "#F07106", "#F37B08", "#F5850A",
    "#F78F0C", "#F9990F", "#FBA312", "#FCAE17", "#FDB81D",
    "#FDC223", "#FDCC2A", "#FDD630", "#FDE037", "#FDEA3E",
    
    # Orange-reds
    "#D7301F", "#DD3B20", "#E34620", "#E85121", "#ED5C21",
    "#EF6722", "#F17223", "#F47D25", "#F68728", "#F8912B",
    "#FA9B2E", "#FBA531", "#FDAF34", "#FDB937", "#FEC33A",
    
    # Reds
    "#A50026", "#B20D2B", "#BE1930", "#CA2535", "#D43139",
    "#DE3D3D", "#E74842", "#EC5347", "#F15E4C", "#F46951",
    "#F77456", "#F87E5B", "#F98960", "#FA9365", "#FB9D6A",
    
    # Magentas
    "#7A0177", "#89077F", "#980D87", "#A7138F", "#B51996",
    "#C21F9D", "#CE28A4", "#D830AA", "#E038B0", "#E640B5",
    "#EB49BA", "#EF52BE", "#F35BC2", "#F664C6", "#F86DC9",
    
    # Purples
    "#3F007D", "#4B078A", "#570D97", "#6313A4", "#6F19B1",
    "#7A20BD", "#8526C8", "#902DD2", "#9A34DB", "#A43BE3",
    "#AE42EA", "#B749EF", "#C050F3", "#C957F6", "#D15EF8"))+
  theme_bw()+
  theme(plot.title=element_text(hjust=0.5, face = "italic"))+
  xlab("Kimura substitution level") +
  ylab("Percent of the genome") + 
  labs(fill = "") +
  coord_cartesian(xlim = c(0, 55)) +
  theme(axis.text=element_text(size=11),axis.title =element_text(size=12))+
  theme(legend.position="bottom")+
  theme(legend.key.size = unit(2, 'mm'))   +
  theme(legend.position = "none" )


# separate plots 

# page 1
ggplot(kd_melt, aes(y=norm, x=Div, fill = variable)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values=c( # Blues
    "#003F5C", "#075985", "#0C6E9C", "#147EA8", "#1B91B5",
    "#229FBF", "#2AAAC7", "#35B5CC", "#43BDD0", "#55C4D3",
    "#67CBD5", "#7AD1D7", "#8DD7D9", "#A0DDDC", "#B3E3DF",
    
    # Blue-greens
    "#005F73", "#087F8C", "#10989B", "#19A7A5", "#25B5A9",
    "#35C0AB", "#46C8A9", "#58CEA6", "#6BD3A3", "#7FD7A0",
    "#93DA9D", "#A7DD9B", "#BBE09A", "#CDE39A", "#DCE69B",
    
    # Greens
    "#006837", "#168344", "#29974A", "#3BA64D", "#4CB44E",
    "#5CC14D", "#6BCB4C", "#7BD24D", "#8AD84E", "#99DD50",
    "#A8E153", "#B7E557", "#C5E85C", "#D2EB62", "#DDEB6A",
    
    # Yellow-greens
    "#4D7300", "#668800", "#809B00", "#99AD00", "#B0BD00",
    "#C5CA00", "#D5D400", "#E2DC00", "#EBE300", "#F1E900",
    "#F5ED1A", "#F7F02F", "#F8F244", "#F9F45A", "#FAF570",
    
    # Yellows
    "#FFD600", "#FFD000", "#FFC900", "#FFC200", "#FFBB00",
    "#FFB300", "#FFAB00", "#FFA300", "#FF9B00", "#FF9300",
    "#FF8B00", "#FF8300", "#FF7B00", "#FF7300", "#FF6B00",
    
    # Oranges
    "#E85D04", "#ED6705", "#F07106", "#F37B08", "#F5850A",
    "#F78F0C", "#F9990F", "#FBA312", "#FCAE17", "#FDB81D",
    "#FDC223", "#FDCC2A", "#FDD630", "#FDE037", "#FDEA3E",
    
    # Orange-reds
    "#D7301F", "#DD3B20", "#E34620", "#E85121", "#ED5C21",
    "#EF6722", "#F17223", "#F47D25", "#F68728", "#F8912B",
    "#FA9B2E", "#FBA531", "#FDAF34", "#FDB937", "#FEC33A",
    
    # Reds
    "#A50026", "#B20D2B", "#BE1930", "#CA2535", "#D43139",
    "#DE3D3D", "#E74842", "#EC5347", "#F15E4C", "#F46951",
    "#F77456", "#F87E5B", "#F98960", "#FA9365", "#FB9D6A",
    
    # Magentas
    "#7A0177", "#89077F", "#980D87", "#A7138F", "#B51996",
    "#C21F9D", "#CE28A4", "#D830AA", "#E038B0", "#E640B5",
    "#EB49BA", "#EF52BE", "#F35BC2", "#F664C6", "#F86DC9",
    
    # Purples
    "#3F007D", "#4B078A", "#570D97", "#6313A4", "#6F19B1",
    "#7A20BD", "#8526C8", "#902DD2", "#9A34DB", "#A43BE3",
    "#AE42EA", "#B749EF", "#C050F3", "#C957F6", "#D15EF8"))+
  ggtitle("Arvicola amphibius")+
  theme_classic() +
  theme(plot.title=element_text(hjust=0.5))+
  xlab("Kimura substitution level") +
  ylab("Percent of the genome") + 
  # labs(fill = "") +
  coord_cartesian(xlim = c(0, 55)) +
  theme(axis.text=element_text(size=11),axis.title =element_text(size=12), 
        legend.position = "none")+
  facet_wrap_paginate(~variable,scal = "free", ncol= 5, nrow= 6, page = 1) # change the page number if you need / want more individual plots. Adjust the cols and row per file if needed
