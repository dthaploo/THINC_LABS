#**Author: Adebiyi Sobitan, Ph.D.
# ** Nodes are encoded by 3 of 5 vectors: Eigenvector, Degree of centrality, 
# and Betweenness centrality, 
#** Date: December, 2024
#** Usage: To calculate significant nodes in the inter hemisphere.


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

if ("inter_fc" %in% excel_sheets(file_path)){
     sheet3_data <- read_excel(file_path, sheet = "inter_fc")
} else if ("inter_fc_" %in% excel_sheets(file_path)){
     sheet3_data <- read_excel(file_path, sheet = "inter_fc_")
} else {
     stop("Neither 'inter_fc' nor 'inter_fc_' sheet exists in the file for:", subject_id)
}

# Combine
patient_data<- sheet3_data %>%
   mutate(
   PatientID = subject_id, 
   NodeID = rep(1:nrow(sheet3_data))
  )

# Append to the list
  all_data[[subject_id]] <- patient_data
}

# combine into a single data frame
if (length(all_data) > 0) {
   combined_data_inter <- bind_rows(all_data)
} else {
  stop("No data was loaded. Please check file paths.")
}

str(combined_data_inter)

combined_data_inter<- combined_data_inter %>%
        mutate(PatientID = as.character(PatientID))

gender_mapping<- gender_mapping %>%
        mutate(PatientID = as.character(PatientID))
# Filter
filtered_gender_mapping<- gender_mapping %>%
    filter(as.character(PatientID) %in% combined_data_inter$PatientID) 

# Merge with gender information
combined_data_inter <- combined_data_inter %>%
    left_join(filtered_gender_mapping, by = "PatientID")


# Check if gender information was successfully added
if(any(is.na(combined_data_inter$Gender))){
     stop("Some patients are missing gender information.")
}
head(combined_data_inter)

# LMMs for each Node

# Fit models for each node
results_eigen_inter <- combined_data_inter %>%
  group_by(Node) %>%
  summarise(
    Model = list(lmer(EigenvectorCentrality ~ Gender + (1 | PatientID), data = cur_data(),
              control=lmerControl(check.conv.singular="ignore"))),
    GenderEffect = summary(lmer(EigenvectorCentrality ~ Gender + (1 | PatientID), data = cur_data()))$coefficients["GenderMale", "Estimate"],
    PValue = summary(lmer(EigenvectorCentrality ~ Gender + (1 | PatientID), data = cur_data()))$coefficients["GenderMale", "Pr(>|t|)"]
  )

# Print summary of results
#print("Summary of Results for Each Node:")
print(results_eigen_inter)
results_eigen_inter<- results_eigen_inter[, !sapply(results_eigen_inter, is.list)]
write.csv(results_eigen_inter, file="interH_LModel_Eigen.csv")

# Visualization with error bars and p-values
plot_interH_eig <- combined_data_inter %>%
  group_by(Node, Gender) %>%
  summarise(
    Mean = mean(EigenvectorCentrality, na.rm = TRUE),
    SE = sd(EigenvectorCentrality, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

ggplot(plot_interH_eig, aes(x = Node, y = Mean, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                position = position_dodge(0.8), width = 0.25) +
  scale_fill_manual(values=c("Male" = "blue", "Female" = "red")) +
  labs(
    title = "Gender Differences for Each interH Node (EigenvectorCentrality)",
    x = "Node",
    y = "Mean ± SE"
  ) +
  theme_minimal() +
  theme(
    axis.text.x=element_text(angle=90, hjust=1),
    legend.title = element_blank(),
    panel.grid = element_blank()
  ) +
  geom_text(data = results_eigen_inter, aes(x = Node, y = max(plot_interH_eig$Mean) + 0.2,
                                label = ifelse(PValue < 0.05, "*", "")),
            inherit.aes = FALSE, size = 5, color = "black")

ggsave(file = "interH_Eigenvector_LinearModel.jpg", dpi = 300)

# DegreeCentrality

results_deg_inter <- combined_data_inter %>%
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
print(results_deg_inter)
#results_deg_inter<- results_deg_inter[, !sapply(results_deg_inter, is.list)]
write.csv(results_deg_inter, file="interH_LModel_Degree.csv")

# Visualization with error bars and p-values
plot_inter_deg <- combined_data_inter %>%
  group_by(Node, Gender) %>%
  summarise(
    Mean = mean(DegreeCentrality, na.rm = TRUE),
    SE = sd(DegreeCentrality, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )
PValue_deg_inter<- results_deg_inter$Pvalue
ggplot(plot_inter_deg, aes(x = Node, y = Mean, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                position = position_dodge(0.8), width = 0.25) +
  scale_fill_manual(values=c("Male" = "blue", "Female" = "red")) +
  labs(
    title = "Gender Differences for Each interH Node (DegreeCentrality)",
    x = "Node",
    y = "Mean ± SE"
  ) +
  theme_minimal() +
  theme(
    axis.text.x=element_text(angle=90, hjust=1),
    legend.title = element_blank(),
    panel.grid = element_blank()
  ) +
  geom_text(data = results_deg_inter, aes(x = Node, y = max(plot_inter_deg$Mean) + 0.2,
                                label = ifelse(PValue_deg_inter < 0.05, "*", "")),
            inherit.aes = FALSE, size = 5, color = "black")

ggsave(file = "interH_Degree_LinearModel.jpg", dpi = 300)


# Betweenness
results_btw_inter <- combined_data_inter %>%
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
print(results_btw_inter)
#results_btw_inter<- results_btw_inter[, !sapply(results_btw_inter, is.list)]
write.csv(results_btw_inter, file="interH_LModel_btw.csv")

# Visualization with error bars and p-values
plot_inter_btw <- combined_data_inter %>%
  group_by(Node, Gender) %>%
  summarise(
    Mean = mean(BetweennessCentrality, na.rm = TRUE),
    SE = sd(BetweennessCentrality, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )
PValues_btw_inter<- results_btw_inter$Pvalue
ggplot(plot_inter_btw, aes(x = Node, y = Mean, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                position = position_dodge(0.8), width = 0.25) +
  scale_fill_manual(values=c("Male" = "blue", "Female" = "red")) +
  labs(
    title = "Gender Differences for Each interH Node (BetweennessCentrality)",
    x = "Node",
    y = "Mean ± SE"
  ) +
  theme_minimal() +
  theme(
    axis.text.x=element_text(angle=90, hjust=1),
    legend.title = element_blank(),
    panel.grid = element_blank()
  ) +
  geom_text(data = results_btw_inter, aes(x = Node, y = max(plot_inter_btw$Mean) + 0.2,
                                label = ifelse(PValues_btw_inter < 0.05, "*", "")),
            inherit.aes = FALSE, size = 5, color = "black")

ggsave(file = "interH_Betweenness_LinearModel.jpg", dpi = 300)

