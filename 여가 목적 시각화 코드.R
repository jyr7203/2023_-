purpo<-read.csv("C:/purpo.csv")
purpo<-purpo[,-c(1,2)]
head(purpo)
colnames(purpo)<-c("gender","age","region","income","pur1","pur2")

install.packages("ggplot2")
library(ggplot2)

library(RColorBrewer)
display.brewer.all()

subset_pur1 <- purpo[, c("gender", "pur1")]
gender_counts <- table(subset_pur1$gender)

subset_m <- subset_pur1[subset_pur1$gender == "M", ]
subset_f <- subset_pur1[subset_pur1$gender == "F", ]
pur_counts_m <- table(subset_m$pur1)
pur_counts_f <- table(subset_f$pur1)

prop_counts_m <- prop.table(pur_counts_m)
prop_counts_f <- prop.table(pur_counts_f)

plot_data_m <- data.frame(pur1 = names(prop_counts_m), prop_counts = as.vector(prop_counts_m), gender = "남성", purpose = "1번째 목적")
plot_data_f <- data.frame(pur1 = names(prop_counts_f), prop_counts = as.vector(prop_counts_f), gender = "여성", purpose = "1번째 목적")
plot_data <- rbind(plot_data_m, plot_data_f)

# 성별에 따른 여가 1번째 목적
ggplot(plot_data, aes(x = gender, y = prop_counts, fill = pur1)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6) +
  labs(title = "성별에 따른 여가 목적 1") +
  labs(fill = "1번째 목적") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() + scale_fill_brewer(palette = "Spectral")

####################################

subset_pur2 <- purpo[, c("gender", "pur2")]
gender_counts <- table(subset_pur2$gender)

subset_m <- subset_pur2[subset_pur2$gender == "M", ]
subset_f <- subset_pur2[subset_pur2$gender == "F", ]
pur_counts_m <- table(subset_m$pur2)
pur_counts_f <- table(subset_f$pur2)

prop_counts_m <- prop.table(pur_counts_m)
prop_counts_f <- prop.table(pur_counts_f)

plot_data_m <- data.frame(pur2 = names(prop_counts_m), prop_counts = as.vector(prop_counts_m), gender = "남성", purpose = "2번째 목적")
plot_data_f <- data.frame(pur2 = names(prop_counts_f), prop_counts = as.vector(prop_counts_f), gender = "여성", purpose = "2번째 목적")
plot_data <- rbind(plot_data_m, plot_data_f)

# 성별에 따른 여가 2번째 목적
ggplot(plot_data, aes(x = gender, y = prop_counts, fill = pur2)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6) +
  labs(title = "성별에 따른 여가 목적 2") +
  labs(fill = "2번째 목적") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() + scale_fill_brewer(palette = "Spectral")

################################

age_pur1_counts <- table(purpo$age, purpo$pur1)
age_pur1_prop <- prop.table(age_pur1_counts, margin = 1)  

age_pur1_data <- as.data.frame(age_pur1_prop)

colnames(age_pur1_data) <- c("age", "pur1", "prop")

# 연령에 따른 여가 1번째 목적
ggplot(age_pur1_data, aes(x = age, y = prop, fill = pur1)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "연령에 따른 여가 목적 1") +
  labs(fill = "1번째 목적") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() + scale_fill_brewer(palette = "Spectral")

##################################

age_pur2_counts <- table(purpo$age, purpo$pur2)
age_pur2_prop <- prop.table(age_pur2_counts, margin = 1)  

age_pur2_data <- as.data.frame(age_pur2_prop)

colnames(age_pur2_data) <- c("age", "pur1", "prop")

# 연령에 따른 여가 2번째 목적
ggplot(age_pur2_data, aes(x = age, y = prop, fill = pur1)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "연령에 따른 여가 목적 2") +
  labs(fill = "2번째 목적") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() + scale_fill_brewer(palette = "Spectral")

##################################

income_pur1_counts <- table(purpo$income, purpo$pur1)
income_pur1_prop <- prop.table(income_pur1_counts, margin = 1)

income_pur1_data <- as.data.frame(income_pur1_prop)

colnames(income_pur1_data) <- c("income", "pur1", "prop")

# 소득에 따른 여가 1번째 목적
ggplot(income_pur1_data, aes(x = income, y = prop, fill = pur1)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "소득에 따른 여가 목적 1") +
  labs(fill = "1번째 목적") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() + scale_fill_brewer(palette = "Spectral")

####################################

income_pur2_counts <- table(purpo$income, purpo$pur2)
income_pur2_prop <- prop.table(income_pur2_counts, margin = 1)

income_pur2_data <- as.data.frame(income_pur2_prop)

colnames(income_pur2_data) <- c("income", "pur1", "prop")

# 소득에 따른 여가 2번째 목적
ggplot(income_pur2_data, aes(x = income, y = prop, fill = pur1)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "소득에 따른 여가 목적 2") +
  labs(fill = "2번째 목적") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() + scale_fill_brewer(palette = "Spectral")



