library(ggplot2)
library(dplyr)

prep_candidates <- c("data/preprocessed.csv", "../data/preprocessed.csv", "preprocessed.csv")
prep_file <- prep_candidates[file.exists(prep_candidates)][1]
if (is.na(prep_file)) prep_file <- "preprocessed.csv"
df <- read.csv(prep_file, stringsAsFactors = FALSE)

dim(df)
names(df)
str(df)
summary(df)
colSums(is.na(df))

class_freq <- table(df$Is_Ad)
class_percent <- prop.table(class_freq) * 100

class_summary <- data.frame(
  Is_Ad = names(class_freq),
  Frequency = as.vector(class_freq),
  Percentage = round(as.vector(class_percent), 2)
)

print(class_summary)

p_class <- ggplot(df, aes(x = factor(Is_Ad))) +
  geom_bar() +
  labs(
    title = "Distribution of Advertisement Class",
    x = "Is_Ad (0 = Non-Ad, 1 = Ad)",
    y = "Frequency"
  )

print(p_class)

numeric_summary <- data.frame(
  Variable = c("Height", "Width", "Aspect_ratio"),
  Mean = c(mean(df$Height), mean(df$Width), mean(df$Aspect_ratio)),
  Median = c(median(df$Height), median(df$Width), median(df$Aspect_ratio)),
  SD = c(sd(df$Height), sd(df$Width), sd(df$Aspect_ratio)),
  Min = c(min(df$Height), min(df$Width), min(df$Aspect_ratio)),
  Q1 = c(
    as.numeric(quantile(df$Height, 0.25)),
    as.numeric(quantile(df$Width, 0.25)),
    as.numeric(quantile(df$Aspect_ratio, 0.25))
  ),
  Q3 = c(
    as.numeric(quantile(df$Height, 0.75)),
    as.numeric(quantile(df$Width, 0.75)),
    as.numeric(quantile(df$Aspect_ratio, 0.75))
  ),
  Max = c(max(df$Height), max(df$Width), max(df$Aspect_ratio))
)

print(numeric_summary)

p_height_hist <- ggplot(df, aes(x = Height)) +
  geom_histogram(bins = 30) +
  labs(title = "Histogram of Height", x = "Height", y = "Frequency")

p_width_hist <- ggplot(df, aes(x = Width)) +
  geom_histogram(bins = 30) +
  labs(title = "Histogram of Width", x = "Width", y = "Frequency")

p_aspect_hist <- ggplot(df, aes(x = Aspect_ratio)) +
  geom_histogram(bins = 30) +
  labs(title = "Histogram of Aspect Ratio", x = "Aspect Ratio", y = "Frequency")

print(p_height_hist)
print(p_width_hist)
print(p_aspect_hist)

p_height_box <- ggplot(df, aes(y = Height)) +
  geom_boxplot() +
  labs(title = "Boxplot of Height", y = "Height")

p_width_box <- ggplot(df, aes(y = Width)) +
  geom_boxplot() +
  labs(title = "Boxplot of Width", y = "Width")

p_aspect_box <- ggplot(df, aes(y = Aspect_ratio)) +
  geom_boxplot() +
  labs(title = "Boxplot of Aspect Ratio", y = "Aspect Ratio")

print(p_height_box)
print(p_width_box)
print(p_aspect_box)

local_freq <- table(df$Local)
local_percent <- prop.table(local_freq) * 100

local_summary <- data.frame(
  Local = c("0", "1"),
  Frequency = as.vector(local_freq),
  Percentage = round(as.vector(local_percent), 2)
)

print(local_summary)

p_local <- ggplot(df, aes(x = factor(Local))) +
  geom_bar() +
  labs(
    title = "Distribution of Local Variable",
    x = "Local",
    y = "Frequency"
  )

print(p_local)

grouped_summary <- df %>%
  group_by(Is_Ad) %>%
  summarise(
    Mean_Height = mean(Height),
    Mean_Width = mean(Width),
    Mean_Aspect = mean(Aspect_ratio),
    Mean_Local = mean(Local),
    .groups = "drop"
  )

print(grouped_summary)

p_height_class <- ggplot(df, aes(x = factor(Is_Ad), y = Height)) +
  geom_boxplot() +
  labs(title = "Height by Advertisement Class", x = "Is_Ad", y = "Height")

p_width_class <- ggplot(df, aes(x = factor(Is_Ad), y = Width)) +
  geom_boxplot() +
  labs(title = "Width by Advertisement Class", x = "Is_Ad", y = "Width")

p_aspect_class <- ggplot(df, aes(x = factor(Is_Ad), y = Aspect_ratio)) +
  geom_boxplot() +
  labs(title = "Aspect Ratio by Advertisement Class", x = "Is_Ad", y = "Aspect Ratio")

p_local_class <- ggplot(df, aes(x = factor(Is_Ad), fill = factor(Local))) +
  geom_bar(position = "fill") +
  labs(
    title = "Proportion of Local by Advertisement Class",
    x = "Is_Ad",
    y = "Proportion",
    fill = "Local"
  )

print(p_height_class)
print(p_width_class)
print(p_aspect_class)
print(p_local_class)

write.csv(class_summary, "class_summary.csv", row.names = FALSE)
write.csv(numeric_summary, "numeric_summary.csv", row.names = FALSE)
write.csv(local_summary, "local_summary.csv", row.names = FALSE)
write.csv(grouped_summary, "grouped_summary.csv", row.names = FALSE)

ggsave("fig_class_distribution.png", plot = p_class, width = 7, height = 5)
ggsave("fig_height_histogram.png", plot = p_height_hist, width = 7, height = 5)
ggsave("fig_width_histogram.png", plot = p_width_hist, width = 7, height = 5)
ggsave("fig_aspect_ratio_histogram.png", plot = p_aspect_hist, width = 7, height = 5)

ggsave("fig_height_boxplot.png", plot = p_height_box, width = 7, height = 5)
ggsave("fig_width_boxplot.png", plot = p_width_box, width = 7, height = 5)
ggsave("fig_aspect_ratio_boxplot.png", plot = p_aspect_box, width = 7, height = 5)

ggsave("fig_local_distribution.png", plot = p_local, width = 7, height = 5)

ggsave("fig_height_by_class.png", plot = p_height_class, width = 7, height = 5)
ggsave("fig_width_by_class.png", plot = p_width_class, width = 7, height = 5)
ggsave("fig_aspect_ratio_by_class.png", plot = p_aspect_class, width = 7, height = 5)
ggsave("fig_local_by_class.png", plot = p_local_class, width = 7, height = 5)
