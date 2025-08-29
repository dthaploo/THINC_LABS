#**Author: Adebiyi Sobitan, Ph.D.
# ** Nodes are encoded by 3 of 5 vectors: Eigenvector, Degree of centrality, 
# and Betweenness centrality, 
#** Date: December, 2024
#** Usage: To calculate significant nodes in the Right hemisphere.



setwd("C:/Users/sobitanab/Documents/Ade")

chooseCRANmirror(ind=1)

# Install and access necessary packages
install.packages(c("tidyverse", "factoextra", "ggplot2", "readxl","car","reshape2","psych","lme4","FactoMineR"))
install.packages(c("lmerTest","sjPlot", "lattice", "dplyr","factoextra", "stringr", "ggpubr", "ggcorrplot"))

library(sjPlot)
library(lattice)
library(tidyverse)
library(readxl)
library(car)
library(reshape2)
library(psych)
library(lme4)
library(FactoMineR)
library(ggplot2)
library(dplyr)
library(factoextra)
library(stringr)
library(ggpubr)
library(ggcorrplot)
library(lmerTest) 

data_dir<- "C:/Users/sobitanab/Documents/Ade"

subject_folders<- list.dirs(data_dir, full.names=TRUE, recursive=FALSE)

gender_mapping<- read_csv("C:/Users/sobitanab/Documents/Ade/gender_mapfile.csv")
#gender_mapping

all_data<- list()

for (folder in subject_folders) {
   # Construct each subject path
  
   subject_id<- basename(folder)

   #file_pattern<- paste0(".*_", subject_id, "\\.xlsx$")
   file_pattern<- paste0("time_point_centrality_analysis_",subject_id, ".xlsx")

    file_path<- file.path(folder, pattern = file_pattern)

# check if excel file exist
    if (length(file_path)==0) {
       cat("File does not exist for:", subject_id, "\n")
    next
}

# Read sheet data
sheet2_data <- read_excel(file_path, sheet = "RH_fc")

# Combine
patient_data<- sheet2_data %>%
   mutate(
   PatientID = subject_id, 
   NodeID = rep(1:nrow(sheet2_data))
  )

# Append to the list
  all_data[[subject_id]] <- patient_data
}

# combine into a single data frame
if (length(all_data) > 0) {
   combined_data_RH <- bind_rows(all_data)
} else {
  stop("No data was loaded. Please check file paths.")
}

str(combined_data_RH)

combined_data_RH<- combined_data_RH %>%
        mutate(PatientID = as.character(PatientID))

gender_mapping<- gender_mapping %>%
        mutate(PatientID = as.character(PatientID))
# Filter
filtered_gender_mapping<- gender_mapping %>%
    filter(as.character(PatientID) %in% combined_data_RH$PatientID) 

# Merge with gender information
combined_data_RH <- combined_data_RH %>%
    left_join(filtered_gender_mapping, by = "PatientID")


# Check if gender information was successfully added
if(any(is.na(combined_data_RH$Gender))){
     stop("Some patients are missing gender information.")
}
head(combined_data_RH)


# LINEAR-MIXED MODEL

mm<- lmer(DegreeCentrality ~ Gender + (1 | PatientID) + (1 | Node), data = combined_data_RH)
summary(mm)
anova(mm)

# LMMs for each Node

# Fit models for each node
results_eigen <- combined_data_RH %>%
  group_by(Node) %>%
  summarise(
    Model = list(lmer(EigenvectorCentrality ~ Gender + (1 | PatientID), data = cur_data(),
              control=lmerControl(check.conv.singular="ignore"))),
    GenderEffect = summary(lmer(EigenvectorCentrality ~ Gender + (1 | PatientID), data = cur_data()))$coefficients["GenderMale", "Estimate"],
    PValue = summary(lmer(EigenvectorCentrality ~ Gender + (1 | PatientID), data = cur_data()))$coefficients["GenderMale", "Pr(>|t|)"]
  )

# Print summary of results
#print("Summary of Results for Each Node:")
print(results_eigen)
results_eigen<- results_eigen[, !sapply(results_eigen, is.list)]
write.csv(results_eigen, file="RH_LModel_Eigen.csv")

# Visualization with error bars and p-values
plot_data_eig <- combined_data_RH %>%
  group_by(Node, Gender) %>%
  summarise(
    Mean = mean(EigenvectorCentrality, na.rm = TRUE),
    SE = sd(EigenvectorCentrality, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

ggplot(plot_data_eig, aes(x = Node, y = Mean, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                position = position_dodge(0.8), width = 0.25) +
  scale_fill_manual(values=c("Male" = "blue", "Female" = "red")) +
  labs(
    title = "Gender Differences for Each RH Node (EigenvectorCentrality)",
    x = "Node",
    y = "Mean ± SE"
  ) +
  theme_minimal() +
  theme(
    axis.text.x=element_text(angle=90, hjust=1),
    legend.title = element_blank(),
    panel.grid = element_blank()
  ) +
  geom_text(data = results_eigen, aes(x = Node, y = max(plot_data$Mean) + 0.2,
                                label = ifelse(PValue < 0.05, "*", "")),
            inherit.aes = FALSE, size = 5, color = "black")

ggsave(file = "RH_Eigenvector_LinearModel.jpg", dpi = 300)


# DegreeCentrality

results_deg <- combined_data_RH %>%
  group_by(Node) %>%
  summarise(
    GenderEffect = tryCatch(
    {
       Model2<- lmer(DegreeCentrality ~ Gender + (1 | PatientID), data = cur_data(),
         control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
    summary(Model2)$coefficients["GenderMale", "Estimate"]
   },
   error = function(e) NA
   ),
   Pvalue= tryCatch(
   {
   Model2<- lmer(DegreeCentrality ~ Gender + (1 | PatientID), data = cur_data(),
         control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
    summary(Model2)$coefficients["GenderMale", "Pr(>|t|)"]
    },
    error = function(e) NA
     )    
  )

# Print summary of results
#print("Summary of Results for Each Node:")
print(results_deg)
results_deg<- results_deg[, !sapply(results_deg, is.list)]
write.csv(results_deg, file="RH_LModel_Degree.csv")

# Visualization with error bars and p-values
plot_data_deg <- combined_data_RH %>%
  group_by(Node, Gender) %>%
  summarise(
    Mean = mean(DegreeCentrality, na.rm = TRUE),
    SE = sd(DegreeCentrality, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )
PValue<- results_deg$Pvalue
ggplot(plot_data_deg, aes(x = Node, y = Mean, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                position = position_dodge(0.8), width = 0.25) +
  scale_fill_manual(values=c("Male" = "blue", "Female" = "red")) +
  labs(
    title = "Gender Differences for Each RH Node (DegreeCentrality)",
    x = "Node",
    y = "Mean ± SE"
  ) +
  theme_minimal() +
  theme(
    axis.text.x=element_text(angle=90, hjust=1),
    legend.title = element_blank(),
    panel.grid = element_blank()
  ) +
  geom_text(data = results_deg, aes(x = Node, y = max(plot_data_deg$Mean) + 0.2,
                                label = ifelse(PValue < 0.05, "*", "")),
            inherit.aes = FALSE, size = 5, color = "black")

ggsave(file = "RH_Degree_LinearModel.jpg", dpi = 300)


# Betweenness
results_btw <- combined_data_RH %>%
  group_by(Node) %>%
  summarise(
    GenderEffect = tryCatch(
    {
       Model3<- lmer(BetweennessCentrality ~ Gender + (1 | PatientID), data = cur_data(),
         control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
    summary(Model3)$coefficients["GenderMale", "Estimate"]
   },
   error = function(e) NA
   ),
   Pvalue= tryCatch(
   {
   Model3<- lmer(BetweennessCentrality ~ Gender + (1 | PatientID), data = cur_data(),
         control=lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
    summary(Model3)$coefficients["GenderMale", "Pr(>|t|)"]
    },
    error = function(e) NA
     )    
  )


# Print summary of results
#print("Summary of Results for Each Node:")
print(results_btw)
#results_btw<- results_btw[, !sapply(results_btw, is.list)]
write.csv(results_btw, file="RH_LModel_btw.csv")

# Visualization with error bars and p-values
plot_data_btw <- combined_data_RH %>%
  group_by(Node, Gender) %>%
  summarise(
    Mean = mean(BetweennessCentrality, na.rm = TRUE),
    SE = sd(BetweennessCentrality, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )
PValues<- results_btw$Pvalue
ggplot(plot_data_btw, aes(x = Node, y = Mean, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                position = position_dodge(0.8), width = 0.25) +
  scale_fill_manual(values=c("Male" = "blue", "Female" = "red")) +
  labs(
    title = "Gender Differences for Each RH Node (BetweennessCentrality)",
    x = "Node",
    y = "Mean ± SE"
  ) +
  theme_minimal() +
  theme(
    axis.text.x=element_text(angle=90, hjust=1),
    legend.title = element_blank(),
    panel.grid = element_blank()
  ) +
  geom_text(data = results_btw, aes(x = Node, y = max(plot_data_btw$Mean) + 0.2,
                                label = ifelse(PValues < 0.05, "*", "")),
            inherit.aes = FALSE, size = 5, color = "black")

ggsave(file = "RH_Betweenness_LinearModel.jpg", dpi = 300)


