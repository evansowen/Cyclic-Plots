library(openxlsx)
library(tidyverse)
library(rlist)
library(readr)
library(ggpmisc)
library(broom)

df <- read.csv("Cyclic.csv")
Displacement1 <- 1.25
Displacement2 <- 2.75
df$index <- seq(1, nrow(df), by = 1)

######### High Stress Indexing #################

df$low <- ifelse(df$TKS<=Displacement1, "low", "")
f <- rle(df$low)
low.lengths <- data.frame(cycle.length = f$lengths)
no.cycles <- length(which(low.lengths$cycle.length<15))
low.lengths$cumsum <- cumsum(low.lengths$cycle.length)

# Find index for low points - add 1/2 cycle
low.lengths <- mutate(low.lengths, index=cumsum-0.5*cycle.length)
low.lengths <- filter(low.lengths, cycle.length < 15)
low.lengths$index <- ceiling(low.lengths$index)

# Find max stress values
max.stress <- df[low.lengths$index,]
max.stress$cycle <- c(1:nrow(max.stress))

############## Low Stress Indexing #####################

df$high <- ifelse(df$TKS>=Displacement2-0.02, "high", "")
f.high <- rle(df$high)
high.lengths <- data.frame(cycle.length = f.high$lengths)
high.lengths$cumsum <- cumsum(high.lengths$cycle.length)
high.lengths <- mutate(high.lengths, index=cumsum-0.5*cycle.length)
high.lengths <- filter(high.lengths, cycle.length < 15)
high.lengths$index <- floor(high.lengths$index)

# Find min stress values
min.stress <- df[high.lengths$index,]
min.stress$cycle <- c(1:nrow(min.stress))
write.csv(min.stress, "min.stress.csv")
write.csv(max.stress, "max.stress.csv")

# Data Files - Parsed
mylist <- list()
for (i in 1:(no.cycles-1)) {
  mylist[[i]] <- df[c(low.lengths$index[i]:low.lengths$index[i+1]),]
  names(mylist)[[i]] <- paste0("Cycle", i)
}

# Parsed Datafile with separated cycles
write.xlsx(mylist, file = "Rapid_Parsed.xlsx")

# Plotting Function
myplot<- function (mydf, mylabel) {
  ggplot(data=mydf, aes(x=TKS, y=Stress))+
    geom_path(size=1.0, col="steelblue")+
    theme_bw()+
    scale_x_continuous(limits=c(1,3), breaks = c(1.0,1.5,2.0, 2.5,3.0))+
    scale_y_continuous(limits=c(-100,2200))+
    theme(axis.text = element_text(size = 14, 
                                   colour = "black"),
          axis.title = element_text(size=16, face="bold"),
          panel.grid = element_line(size = 0.3, color="grey80"))+
    annotate(geom="text", size = 7, x=2.55, y=2050, color="steel blue", 
             label=paste0("Cycle", mylabel), hjust=0)+
    xlab("\nThickness (mm)")+
    ylab("Stress (kPa)\n")+
    geom_path(data=mylist[[1]], aes(x=TKS, y=Stress),
              size=0.7, col="darkred", linetype="dotted", alpha=0.7)+
    annotate(geom="text", size = 7, x=2.55, y=1750, color="darkred", 
             label="Cycle1", hjust=0)
}

# Generate Plots via for loop and myplot function
myplots <- list() 
for (j in 1:(no.cycles-1)) {
 myplots[[j]] <- myplot(mylist[[j]], mylabel = j)
}

####### For Loop For Saving GGPLOTS ##############
## Set New WD ###
# Watch sorting on list.files function

plotnames <- c()

for (k in 1:(no.cycles-1)) {
  plotnames[k] <- paste0("plot", k, ".png")
  ggsave(plotnames[k], myplots[[k]], device = 'png', dpi = 150)
}

library(gifski)
png_files <- list.files("~/Desktop/RAPID", 
                        pattern = "*.png$", full.names = TRUE)
library(gtools)
png_files <- mixedsort(png_files)
gifski(png_files, gif_file = "animation.gif", width = 800, height = 550, delay = 0.35)

#END







