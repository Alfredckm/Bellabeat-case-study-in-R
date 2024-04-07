
# Install packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")

# Load packages

library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)

# Importing datasets

Activity <- read.csv("../project/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
Sleep <- read.csv("../project/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
H_Calories <- read.csv("../project/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv")
H_Intensities <- read.csv("../project/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv")
H_Steps <- read.csv("../project/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")


# Cleaning and formatting

# Sleep
# Convert the 'SleepDay' column to Date object
Sleep$SleepDay <- as.Date(Sleep$SleepDay, format = '%m/%d/%Y %I:%M:%S %p')
# Format the 'SleepDay' column to desired format
Sleep$SleepDay <- format(Sleep$SleepDay, '%m/%d/%Y')
# Rename the column 'SleepDay' to 'Date'
Sleep <- Sleep %>%
  rename(Date = SleepDay)

# Activity
# Rename the columns 'ModeratelyActiveDistance' to 'FairlyActiveDistance' and 'ActivityDate' to 'Date'
Activity <- Activity %>%
  rename(FairlyActiveDistance = ModeratelyActiveDistance,
         Date = ActivityDate)
# Format the 'Date' column to desired format
Activity$Date <- format(as.Date(Activity$Date, "%m/%d/%Y"), "%m/%d/%Y")

# H_Calories
# Split 'ActivityHour' into date and time columns
H_Calories$Date <- as.Date(H_Calories$ActivityHour, format = '%m/%d/%Y %I:%M:%S %p')
H_Calories$Time <- format(as.POSIXct(H_Calories$ActivityHour, format = '%m/%d/%Y %I:%M:%S %p'), format = '%H:%M:%S')
# Remove the original 'ActivityHour' column if needed
H_Calories <- subset(H_Calories, select = -c(ActivityHour))
# Format the 'Date' column to desired format
H_Calories$Date <- format(H_Calories$Date, '%m/%d/%Y')

# H_Intensities
# Split 'ActivityHour' into date and time columns
H_Intensities$Date <- as.Date(H_Intensities$ActivityHour, format = '%m/%d/%Y %I:%M:%S %p')
H_Intensities$Time <- format(as.POSIXct(H_Intensities$ActivityHour, format = '%m/%d/%Y %I:%M:%S %p'), format = '%H:%M:%S')
# Remove the original 'ActivityHour' column if needed
H_Intensities <- H_Intensities[, !names(H_Intensities) %in% "ActivityHour"]
# Format the 'Date' column to desired format
H_Intensities$Date <- format(H_Intensities$Date, '%m/%d/%Y')

# H_Steps
# Split 'ActivityHour' into date and time columns
H_Steps$Date <- as.Date(H_Steps$ActivityHour, format = '%m/%d/%Y %I:%M:%S %p')
H_Steps$Time <- format(as.POSIXct(H_Steps$ActivityHour, format = '%m/%d/%Y %I:%M:%S %p'), format = '%H:%M:%S')
# Remove the original 'ActivityHour' column if needed
H_Steps <- subset(H_Steps, select = -c(ActivityHour))
# Format the 'Date' column to desired format
H_Steps$Date <- format(H_Steps$Date, '%m/%d/%Y')

# Determine the number of unique IDs in each data frame:
n_distinct(Activity$Id)
n_distinct(Sleep$Id)
n_distinct(H_Calories$Id)
n_distinct(H_Intensities$Id)
n_distinct(H_Steps$Id)

# Check if there is any duplicate rows within the data frames:
sum(duplicated(Activity))
sum(duplicated(Sleep))
sum(duplicated(H_Calories))
sum(duplicated(H_Intensities))
sum(duplicated(H_Steps))


# Remove the duplicates in data frame 'Sleep':
# Find duplicate rows of Sleep
duplicate_rows <- duplicated(Sleep)
# Remove duplicate rows of Sleep
Sleep <- Sleep[!duplicate_rows, ]

# Merge the data frames 'H_Calories', 'H_Intensities', and 'H_Steps' into a new data frame called 'H_Activity'. Before merging, it's crucial to ensure that the columns 'Id', 'Date', and 'Time' are identical in all three data frames:
if(identical(H_Calories$Id, H_Intensities$Id) && identical(H_Calories$Id, H_Steps$Id)) {
  print("Columns 'Id' are identical in all three tables")
} else {
  print("Columns 'Id' are not identical in all three tables")
}
if(identical(H_Calories$Date, H_Intensities$Date) && identical(H_Calories$Date, H_Steps$Date)) {
  print("Columns 'Date' are identical in all three tables")
} else {
  print("Columns 'Date' are not identical in all three tables")
}
if(identical(H_Calories$Time, H_Intensities$Time) && identical(H_Calories$Time, H_Steps$Time)) {
  print("Columns 'Time' are identical in all three tables")
} else {
  print("Columns 'Time' are not identical in all three tables")
}


# Merging data

# Merge multiple data frames 'H_Calories', 'H_Intensities', and 'H_Steps' into a new single table 'H_Activity'. I aim to create a unified data frame for comprehensive analysis. Let's proceed with the merging process:
H_Activity <- merge(merge(H_Calories, H_Intensities, by = c("Id", "Date", "Time"), all = TRUE), H_Steps, by = c("Id", "Date", "Time"), all = TRUE)
head(H_Activity)

# Merge the 'Activity' and 'Sleep' data frames into a new table called 'D_Sleep', focusing on columns related to 'Sleep' metrics. Considering the 'Sleep' data frame has only 24 unique 'Id' sets (compared to 33 in 'Activity'), I'll exclude any rows with missing values from 'Sleep'. Let's proceed with the merging process:
D_Sleep <- merge(Activity, Sleep, by = c("Id", "Date"), all = TRUE) %>%
  select(Id, Date, TotalSteps, TotalDistance, Calories, TotalMinutesAsleep)%>%
  na.omit()
head(D_Sleep)


# Analyse and visualise the data

# Group the data by unique user IDs and calculate totals and averages:
S_Activity <- Activity %>%
  group_by(Id) %>%
  summarise(
    S_TotalSteps = sum(TotalSteps, na.rm = TRUE),
    S_TotalDistance = sum(TotalDistance, na.rm = TRUE),
    S_VeryActiveDistance = sum(VeryActiveDistance, na.rm = TRUE),
    S_FairlyActiveDistance = sum(FairlyActiveDistance, na.rm = TRUE),
    S_LightActiveDistance = sum(LightActiveDistance, na.rm = TRUE),
    S_VeryActiveMinutes = sum(VeryActiveMinutes, na.rm = TRUE),
    S_FairlyActiveMinutes = sum(FairlyActiveMinutes, na.rm = TRUE),
    S_LightlyActiveMinutes = sum(LightlyActiveMinutes, na.rm = TRUE),
    S_TotalActiveMinutes = sum(VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes , na.rm = TRUE),
    S_Calories = sum(Calories, na.rm = TRUE)
  )
Avg_Activity <- Activity %>%
  group_by(Id) %>%
  summarise(
    Avg_TotalSteps = mean(TotalSteps, na.rm = TRUE),
    Avg_TotalDistance = mean(TotalDistance, na.rm = TRUE),
    Avg_VeryActiveDistance = mean(VeryActiveDistance, na.rm = TRUE),
    Avg_FairlyActiveDistance = mean(FairlyActiveDistance, na.rm = TRUE),
    Avg_LightActiveDistance = mean(LightActiveDistance, na.rm = TRUE),
    Avg_VeryActiveMinutes = mean(VeryActiveMinutes, na.rm = TRUE),
    Avg_FairlyActiveMinutes = mean(FairlyActiveMinutes, na.rm = TRUE),
    Avg_LightlyActiveMinutes = mean(LightlyActiveMinutes, na.rm = TRUE),
    Avg_Calories = mean(Calories, na.rm = TRUE)
  )

head(S_Activity)
head(Avg_Activity)

# Visualize the percentage of 'Very Active Distance', 'Fairly Active Distance' and 'Light Active Distance' in data frame S_Activity with pie chart:
# Calculate the proportions
proportions <- S_Activity %>%
  summarise(
    VeryActive = sum(S_VeryActiveDistance) / sum(S_TotalDistance) * 100,
    FairlyActive = sum(S_FairlyActiveDistance) / sum(S_TotalDistance) * 100,
    LightActive = sum(S_LightActiveDistance) / sum(S_TotalDistance) * 100,
  ) %>%
  gather(ActivityType, Percentage)

# Create pie chart with percentage labels inside slices
ggplot(proportions, aes(x = "", y = Percentage, fill = ActivityType)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_text(aes(label = paste0(round(Percentage), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 5) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("#0072B2", "#009E73", "#D55E00")) +
  labs(title = "Percentage Distribution of Activity Distance",
       fill = "Activity Type",
       x = NULL,
       y = NULL) +
  theme_void() +
  theme(legend.position = "right",
        plot.title = element_text(size = 16, hjust = 0.5, color = "black"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        axis.text = element_blank(),
        axis.title = element_blank())

# Visualize the percentage between 'Very Active Minutes', 'Fairly Active Minutes' and 'Lightly Active Minutes' in data frame S_Activity with pie chart:
# Calculate the proportions
proportions <- S_Activity %>%
  summarise(
    VeryActive = sum(S_VeryActiveMinutes) / sum(S_TotalActiveMinutes) * 100,
    FairlyActive = sum(S_FairlyActiveMinutes) / sum(S_TotalActiveMinutes) * 100,
    LightActive = sum(S_LightlyActiveMinutes) / sum(S_TotalActiveMinutes) * 100,
  ) %>%
  gather(ActivityType, Percentage)

# Create pie chart with percentage labels inside slices
ggplot(proportions, aes(x = "", y = Percentage, fill = ActivityType)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_text(aes(label = paste0(round(Percentage), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 5) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("#0072B2", "#009E73", "#D55E00")) +
  labs(title = "Percentage Distribution of Activity Time",
       fill = "Activity Type",
       x = NULL,
       y = NULL) +
  theme_void() +
  theme(legend.position = "right",
        plot.title = element_text(size = 16, hjust = 0.5, color = "black"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        axis.text = element_blank(),
        axis.title = element_blank())

# Explore how calories expenditure varies throughout the day:
# Convert 'Time' column to numeric format
H_Activity$Time <- as.numeric(substr(H_Activity$Time, 1, 2))  # Assuming the time is in HH:MM:SS format

# Calculate the average calories by time
calories_by_time <- H_Activity %>%
  group_by(Time) %>%
  summarise(Avg_Calories = mean(Calories))

# Plot a bar chart of average calories over time
ggplot(calories_by_time, aes(x = Time, y = Avg_Calories, fill = Avg_Calories)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 100, linetype = "dashed", color = "red") + # Add horizontal line at 100
  scale_fill_gradient(low = "#009E73", high = "#D55E00") + # Gradient fill for bars
  labs(title = "Average Calories Over Time", x = "Time", y = "Average Calories") +
  theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(size = 16, hjust = 0.5, color = "black"))

# Visualize the relationship between 'TotalDistance' and 'Calories' per 'Id' in the 'S_Activity' table:
ggplot(S_Activity, aes(x = S_TotalDistance, y = S_Calories)) +
  geom_jitter(width = 0.2, height = 0.2) + 
  geom_smooth(method = "loess", se = FALSE, color = "#0072B2") + 
  labs(title = "Relationship between Total Distance and Calories per Id",
       x = "Total Distace(Km)", y = "Calories") + 
  theme_minimal()+
  theme(plot.title = element_text(size = 16, hjust = 0.5, color = "black"))

# Explore the relationship between daily walking distance and daily sleep minutes to see if there's any correlation:
ggplot(D_Sleep, aes(x = TotalDistance, y = TotalMinutesAsleep)) +
  geom_jitter(width = 0.2, height = 0.2) +  # Add jitter to avoid overlapping points
  geom_smooth(method = "loess", se = FALSE, color = "#0072B2") +  # Add a smooth line
  labs(title = "Relation between Total Distance and Total Minutes Asleep",
       x = "Total Distance (Km)", y = "Total Minutes Asleep") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, hjust = 0.5, color = "black"))