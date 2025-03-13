library(tidyverse)

#data clean
dissresults.all <- diss.allitems.raw[-1, ]
colnames(dissresults.all) <- as.character(unlist(dissresults.all[1, ]))
dissresults.all <- dissresults.all[-1, ]
colnames(dissresults.all)[5] <- ("Job.Title")

dissresults.all[, 6:35][dissresults.all[,6:35] == ""] <- 0
dissresults.all[, 36:38][dissresults.all[,36:38] ==""] <- NA

dissresults.all <- na.omit(dissresults.all)
dissresults.all <- dissresults.all %>%
  mutate(across(6:35, as.numeric))

dissresults.all <- dissresults.all %>%
  mutate(
    Warmth = rowSums(across(6:11)),
    Personal = rowSums(across(12:17)),
    Professional = rowSums(across(18:23)), 
    Physical = rowSums(across(24:29)),
    Competence = rowSums(across(30:35))
  )
dissresults.all$Job.Title <- factor(dissresults.all$Job.Title,
                                         levels = c("PL", "A", "LD", "P", "MP"),
                                         labels = c("1", "2", "3", "4", "5"))

disslong <- dissresults.all %>%
  select(Warmth, Personal, Professional, Physical, Competence) %>%
  gather(key = "Variable", value = "Value")


# Demographics 
gender_count <- dissresults.all %>%
  group_by(Gender) %>%
  summarise(Count = n())
print(gender_count)

dissresults.all$Gender <- factor(dissresults.all$Gender,
                                 levels = c("Woman", "Man", "Gender queer", "Transgender man", "Gender non-binary"),
                                 labels = c("1", "2", "3", "3", "3"))

dissresults.all$Age <- as.numeric(dissresults.all$Age)
dissresults.all <- dissresults.all %>%
  mutate(AgeGroup = cut(Age,
                        breaks = c(17, 24, 49, 59, Inf),
                        labels = c("18-24", "25-49", "50-59", "60 or above"),
                        right = TRUE))

age_count <- dissresults.all %>%
  group_by(AgeGroup) %>%
  summarise(Count = n(), .groups = "drop")
print(age_count)

# descriptive stats1

comp_table <- dissresults.all %>%
  summarise(
    Mean_Warmth = mean(Warmth, na.rm = TRUE),
    SD_Warmth = sd(Warmth, na.rm = TRUE),
    Mean_Personal = mean(Personal, na.rm = TRUE),
    SD_Personal = sd(Personal, na.rm = TRUE),
    Mean_Professional = mean(Professional, na.rm = TRUE),
    SD_Professional = sd(Professional, na.rm = TRUE),
    Mean_Physical = mean(Physical, na.rm = TRUE),
    SD_Physical = sd(Physical, na.rm = TRUE),
    Mean_Competence = mean(Competence, na.rm = TRUE),
    SD_Competence = sd(Competence, na.rm = TRUE)
  )
comp_table

ggplot(disslong, aes(x = Variable, y = Value, fill = Variable))+
  geom_boxplot(
    color = "black", alpha = 0.6) +
  scale_fill_manual(values = c(
    "Warmth" = "#A6C8FF",  # Light blue
    "Personal" = "#5B99D8",  # Medium blue
    "Professional" = "#1E66A2",  # Dark blue
    "Physical" = "#005B8A",  # Darker blue
    "Competence" = "#1F8CC3"  # Another blue shade
  )) +
  stat_summary(
    fun = "mean",
    geom = "point", 
    shape = 16, 
    size = 3,
    color = "darkblue"
  )+
  theme_minimal(base_size = 12, base_family = "Times New Roman") +
  labs(title = "Total Recognition Task Scores per Item Type", x = "Item Type", y = "Recognition Task Score")+
  theme(plot.title = element_text( face = "bold"))


#Physical Data Analysis
# Physical Sumtable
dissresults.all$Physical <- as.numeric(dissresults.all$Physical)
sum_table <- dissresults.all %>%
  group_by(Job.Title) %>%
  summarise(
    Mean_sum = mean(Physical, na.rm = TRUE), 
    SD_sum = sd(Physical, na.rm = TRUE), 
    N = n()
  )
print(sum_table)

#Physical Plot
sumdata <- dissresults.all %>% 
  group_by(Job.Title) %>%
  summarise(MeanSumScore = mean(Physical),
            SE = sd(Physical) / sqrt(n()))

ggplot(data = sumdata, aes(x = Job.Title, y = MeanSumScore)) +
  geom_jitter(data = dissresults.all, aes(x = Job.Title, y = Physical),
              width = 0.2, alpha = 0.6, colour = "skyblue")+
  geom_col(fill = "grey", alpha = 0.7) +
  geom_errorbar(data = sumdata, aes(ymin = MeanSumScore - SE, ymax = MeanSumScore +SE), width = 0.2) +
  ggtitle("Mean Physical Items Recognition Task Score by Power Level")+
  labs(x = "Power Level",
       y = "Recognition Task Score: Physical Items",
       Title = "Mean Recognition Task Score by Power Level")+
  scale_x_discrete(labels = c("1" = "Paralegal",
                              "2" = "Associate", 
                              "3" = "Legal Director", 
                              "4" = "Partner",
                              "5" = "Managing Partner")) +
  theme_minimal()

#Physical Model
dissresults.all2 <- dissresults.all[dissresults.all$Gender != "3", ]

mdl_full_phys <- dissresults.all2 %>%
  lm(Physical ~ relevel(Job.Title, ref = "3") + Warmth + Gender, data =.)
summary(mdl_full_phys)
confint(mdl_full_phys)


# Warmth Data Analysis
# Sumtable 
sumdata_warmth <- dissresults.all %>% 
  group_by(Job.Title) %>%
  summarise(MeanSumScore = mean(Warmth),
            SE = sd(Warmth) / sqrt(n()))
sumdata_warmth

# Descriptive Plot
ggplot(data = sumdata_warmth, aes(x = Job.Title, y = MeanSumScore)) +
  geom_jitter(data = dissresults.all, aes(x = Job.Title, y = Warmth),
              width = 0.2, alpha = 0.6, colour = "skyblue")+
  geom_col(fill = "grey", alpha = 0.7) +
  geom_errorbar(data = sumdata_warmth, aes(ymin = MeanSumScore - SE, ymax = MeanSumScore +SE), width = 0.2) +
  ggtitle("Mean Warmth Items Recognition Task Score by Power Level")+
  labs(x = "Power Level",
       y = "Recognition Task Score: Warmth Items")+
  scale_x_discrete(labels = c("1" = "Paralegal",
                              "2" = "Associate", 
                              "3" = "Legal Director", 
                              "4" = "Partner",
                              "5" = "Managing Partner")) +
  theme_minimal()
# Warmth Model 
mdl_warmth <- dissresults.all %>% 
  lm(Warmth ~ relevel(Job.Title, ref = "3"), data = .)
confint(mdl_warmth, level = 0.95)

summary(mdl_full)
