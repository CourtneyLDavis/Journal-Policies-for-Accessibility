#################################################
#### Plotting journal policy data            ####
#### Created by: Courtney L Davis            #### 
#### Last checked: 22 August 2024            ####
#################################################


# Load in the necessary libraries
library(ggplot2)
library(here)
library(tidyverse)
library(reshape2)

# Read in the journal policy data
policy.data <- readxl::read_xlsx(here("Data", "Data S1. Accessibility policies.xlsx"))

# Split the journal policy data into entries that are found in "journal materials" and "publisher materials"
journals <- subset(policy.data, grepl("Journal", policy.data$`Policy location`))
publishers <- subset(policy.data, grepl("Publisher", policy.data$`Policy location`))


##### Accessibility policy found in journal materials ####

# Summarize information for all disabilities and paper components
journals_summary <- data.frame(table(journals$`Paper component policy targets`,journals$`Disability explicitly of concern`))
journals_summary <- rbind(journals_summary,
                          data.frame("Var1" = journals_summary[1:5,1],
                                     "Var2" = rep("Learning disability",5),
                                     "Freq" = rep(0,5)))

journals_summary$Freq_No <- length(unique(policy.data$Journal))-journals_summary$Freq
journals_summary <- melt(journals_summary)

empty_bar <- 1
nObsType <- nlevels(as.factor(journals_summary$variable))
to_add <- data.frame( matrix(NA, empty_bar*nlevels(journals_summary$Var1)*nObsType, ncol(journals_summary)) )
colnames(to_add) <- colnames(journals_summary)
to_add$Var1 <- rep(levels(journals_summary$Var1), each=empty_bar*nObsType)
journals_summary <- rbind(journals_summary, to_add)
journals_summary <- journals_summary %>% arrange(Var1, Var2)
journals_summary <- journals_summary[c(1,2,7,8,3:6,9,10:12,
                                       13,14,19,20,15:18,21:24,
                                       25,26,31,32,27:30,33:36,
                                       37,38,43,44,39:42,45:48,
                                       49,50,55,56,51:54,57:60),]

journals_summary$id <- rep( seq(1, nrow(journals_summary)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data <- journals_summary %>% group_by(id, Var2) %>% summarize(tot=sum(value))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- journals_summary %>% 
  group_by(Var1) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[c(nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

journals_summary$Var3 <- paste(journals_summary$Var2, "_", journals_summary$variable)
journals_summary$Var3 <- factor(journals_summary$Var3, 
                                levels = c("Color blindness _ Freq",
                                           "Color blindness _ Freq_No",
                                           "Visual impairment (other than color blind) _ Freq",
                                           "Visual impairment (other than color blind) _ Freq_No",
                                           "Non-specific _ Freq",
                                           "Non-specific _ Freq_No",
                                           "Seizures _ Freq",
                                           "Seizures _ Freq_No",
                                           "Learning disability _ Freq",
                                           "Learning disability _ Freq_No",
                                           "NA _ NA"))

percent_data <- journals_summary[journals_summary$variable == "Freq",]
percent_data <- na.omit(percent_data[percent_data$value > 0,])
percent_data$Percent <- (percent_data$value / 541)*100
percent_data$Percent <- paste(round(percent_data$Percent,1),"%")


journals_summary <- left_join(journals_summary, percent_data,by = c("Var1", "Var2","variable","value","id","Var3"))

ggplot(journals_summary) +     

  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=Var3), stat="identity") +
  scale_fill_manual(values = c("#440154FF","grey90",
                               "#2A7B8EFF","grey90",
                               "#22A384FF","grey90",
                               "#7AD151FF","grey90",
                               "#FDE725FF","grey90",
                               "white")) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end+0.5, y = 0, xend = start-0.5, yend = 0), colour = "grey", alpha=1, linewidth=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end+0.5, y = 100, xend = start-0.5, yend = 100), colour = "grey", alpha=1, linewidth=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end+0.5, y = 200, xend = start-0.5, yend = 200), colour = "grey", alpha=1, linewidth=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end+0.5, y = 300, xend = start-0.5, yend = 300), colour = "grey", alpha=1, linewidth=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end+0.5, y = 400, xend = start-0.5, yend = 400), colour = "grey", alpha=1, linewidth=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end+0.5, y = 500, xend = start-0.5, yend = 500), colour = "grey", alpha=1, linewidth=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each line
  ggplot2::annotate("text", 
                    x = rep(max(journals_summary$id),6), 
                    y = c(0, 100, 200, 300, 400, 500), 
                    label = c("0", "100", "200", "300", "400", "500") , 
                    color="grey50", 
                    size=3, 
                    angle=0, 
                    fontface="bold", 
                    hjust=0.6) +
  
  ggplot2::annotate("text", 
                    x = rep(max(journals_summary$id)+0.38,1), 
                    y = 360, 
                    label = "Number of Journals", 
                    color="grey50", 
                    size=3, 
                    angle=-88, 
                    fontface="bold", 
                    hjust=0.6) +
  
  ylim(-200,600) +
  theme_minimal() +
  theme(legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")) +
  coord_polar() +
  
  # Add labels on top of each bar
  #geom_text(data=label_data, aes(x=id, y=tot+10, label=Var2, hjust=0), color="black", fontface="bold",alpha=0.6, size=5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, 
               aes(x = start, y = -5, xend = end, yend = -5), 
               colour = "black", 
               alpha=0.8, 
               size=0.6, 
               inherit.aes = FALSE )  +
  geom_text(data=base_data, 
            aes(x = title, y = -50, label= c("Figures","Main","SI","Tables","Videos")), 
            colour = "black", 
            alpha=0.8, 
            size=4, 
            angle=c(-25,70,10,-65,45), 
            fontface="bold", 
            inherit.aes = FALSE) +
  geom_text(aes(x = id, y = 593, label = Percent))

            

##### Accessibility policy found in publisher materials ####

# Summarize information for all disabilities and paper components
publishers_summary <- data.frame(table(publishers$`Paper component policy targets`,publishers$`Disability explicitly of concern`))
publishers_summary <- rbind(publishers_summary,
                            data.frame("Var1" = rep(c("Main","SI","Tables"),each = 4),
                                       "Var2" = rep(c("Color blindness","Learning disabilities (including dyslexia)",
                                                  "Non-specific", "Visual impairment (other than color blind)"),times = 3),
                                       "Freq" = 0))
publishers_summary <- rbind(publishers_summary,
                          data.frame("Var1" = unique(publishers_summary$Var1),
                                     "Var2" = rep("Seizures",5),
                                     "Freq" = rep(0,5)))

publishers_summary$Freq_No <- length(unique(policy.data$Journal))-publishers_summary$Freq
publishers_summary <- melt(publishers_summary)

empty_bar <- 1
nObsType <- nlevels(as.factor(publishers_summary$variable))
to_add <- data.frame( matrix(NA, empty_bar*nlevels(publishers_summary$Var1)*nObsType, ncol(publishers_summary)) )
colnames(to_add) <- colnames(publishers_summary)
to_add$Var1 <- rep(levels(publishers_summary$Var1), each=empty_bar*nObsType)
publishers_summary <- rbind(publishers_summary, to_add)
publishers_summary <- publishers_summary %>% arrange(Var1, Var2)
publishers_summary <- publishers_summary[c(1,2,7,8,5,6,9,10,3,4,11,12,
                                       25,26,31,32,29,30,33,34,27,28,35,36,
                                       37,38,43,44,41,42,45,46,39,40,47,48,
                                       49,50,55,56,53,54,57,58,51,52,59,60,
                                       13,14,19,20,17,18,21,22,15,16,23,24),]

publishers_summary$id <- rep( seq(1, nrow(publishers_summary)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data <- publishers_summary %>% group_by(id, Var2) %>% summarize(tot=sum(value))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- publishers_summary %>% 
  group_by(Var1) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))
base_data <- base_data[c(1,3,4,5,2),]

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[c(nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

publishers_summary$Var3 <- paste(publishers_summary$Var2, "_", publishers_summary$variable)
publishers_summary$Var3 <- factor(publishers_summary$Var3, 
                                levels = c("Color blindness _ Freq",
                                           "Color blindness _ Freq_No",
                                           "Visual impairment (other than color blind) _ Freq",
                                           "Visual impairment (other than color blind) _ Freq_No",
                                           "Non-specific _ Freq",
                                           "Non-specific _ Freq_No",
                                           "Seizures _ Freq",
                                           "Seizures _ Freq_No",
                                           "Learning disabilities (including dyslexia) _ Freq",
                                           "Learning disabilities (including dyslexia) _ Freq_No",
                                           "NA _ NA"))

percent_data <- publishers_summary[publishers_summary$variable == "Freq",]
percent_data <- na.omit(percent_data[percent_data$value > 0,])
percent_data$Percent <- (percent_data$value / 541)*100
percent_data$Percent <- paste(round(percent_data$Percent,1),"%")


publishers_summary <- left_join(publishers_summary, percent_data,by = c("Var1", "Var2","variable","value","id","Var3"))

ggplot(publishers_summary) +     
  
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=Var3), stat="identity") +
  scale_fill_manual(values = c("#440154FF","grey90",
                               "#2A7B8EFF","grey90",
                               "#22A384FF","grey90",
                               "#7AD151FF","grey90",
                               "#FDE725FF","grey90",
                               "white")) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end+0.5, y = 0, xend = start-0.5, yend = 0), colour = "grey", alpha=1, linewidth=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end+0.5, y = 100, xend = start-0.5, yend = 100), colour = "grey", alpha=1, linewidth=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end+0.5, y = 200, xend = start-0.5, yend = 200), colour = "grey", alpha=1, linewidth=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end+0.5, y = 300, xend = start-0.5, yend = 300), colour = "grey", alpha=1, linewidth=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end+0.5, y = 400, xend = start-0.5, yend = 400), colour = "grey", alpha=1, linewidth=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end+0.5, y = 500, xend = start-0.5, yend = 500), colour = "grey", alpha=1, linewidth=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each line
  ggplot2::annotate("text", 
                    x = rep(max(publishers_summary$id),6), 
                    y = c(0, 100, 200, 300, 400, 500), 
                    label = c("0", "100", "200", "300", "400", "500") , 
                    color="grey50", 
                    size=3, 
                    angle=0, 
                    fontface="bold", 
                    hjust=0.6) +
  
  ggplot2::annotate("text", 
                    x = rep(max(publishers_summary$id)+0.38,1), 
                    y = 360, 
                    label = "Number of journals", 
                    color="grey50", 
                    size=3, 
                    angle=-88, 
                    fontface="bold", 
                    hjust=0.6) +
  
  ylim(-200,600) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4), "cm")) +
  coord_polar() +
  
  # Add labels on top of each bar
  #geom_text(data=label_data, aes(x=id, y=tot+10, label=Var2, hjust=0), color="black", fontface="bold",alpha=0.6, size=5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, 
               aes(x = start, y = -5, xend = end, yend = -5), 
               colour = "black", 
               alpha=0.8, 
               size=0.6, 
               inherit.aes = FALSE )  +
  geom_text(data=base_data, 
            aes(x = title, y = -50, label= c("Figures","Main","SI","Tables","Videos")), 
            colour = "black", 
            alpha=0.8, 
            size=4, 
            angle=c(-25,70,10,-65,45), 
            fontface="bold", 
            inherit.aes = FALSE) +
  geom_text(aes(x = id, y = 593, label = Percent))
