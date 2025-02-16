# -----------------------------------------------------------------------------
# By Yu Heng Daryl Lee (UCL)
# 16 February 2025
# -----------------------------------------------------------------------------

library(tidyverse)
library(TOSTER)

# -----------------------------------------------------------------------------
# Read in data
# -----------------------------------------------------------------------------

df <- read_csv('myPilotData.csv') # change the file name if necessary

df <- df %>%
  mutate(participantId = row_number() + 100)

# -----------------------------------------------------------------------------
# Convert from wide to long
# -----------------------------------------------------------------------------

df_long <- df %>%
  pivot_longer(
    cols = matches("(correctAnswer_|selectedAnswer_|persuasionDirection_)"),
    names_to = c(".value", "questionIndex"),
    names_pattern = "(.*)_(\\d+)"
  )

# -----------------------------------------------------------------------------
# Compute row-level accuracy & compliance
# -----------------------------------------------------------------------------

df_long <- df_long %>%
  mutate(
    accuracy = case_when(
      is.na(correctAnswer) ~ NA_real_,  # e.g., forecasting question not coded yet
      selectedAnswer == correctAnswer ~ 1,
      TRUE ~ 0
    ),
    compliance = case_when(
      # Positive persuasion => participant should pick the correct answer
      persuasionDirection == "positive" & !is.na(correctAnswer) & accuracy == 1 ~ 1,
      persuasionDirection == "positive" & !is.na(correctAnswer) & accuracy == 0 ~ 0,
      
      # Negative persuasion => participant should pick the incorrect answer
      persuasionDirection == "negative" & !is.na(correctAnswer) & accuracy == 0 ~ 1,
      persuasionDirection == "negative" & !is.na(correctAnswer) & accuracy == 1 ~ 0,
      
      # If it's a Solo Quiz or there's no known correct answer => NA
      TRUE ~ NA_real_
    )
  )

# -----------------------------------------------------------------------------
# Aggregate to participant-level
# -----------------------------------------------------------------------------

df_summary <- df_long %>%
  group_by(participantId) %>%
  summarize(
    meanAccuracy = mean(accuracy, na.rm = TRUE),
    meanCompliance = mean(compliance, na.rm = TRUE)
  ) %>%
  ungroup()

# -----------------------------------------------------------------------------
# Join summary metrics back to original data
# -----------------------------------------------------------------------------

df_final <- df %>%
  left_join(df_summary, by = "participantId")

# -----------------------------------------------------------------------------
# Subset data for RQ1: Only include participants in the persuasion conditions.
# -----------------------------------------------------------------------------
df_RQ1 <- df_final %>%
  filter(treatmentName %in% c("LLM persuasion, sonnet3.5", "Human persuasion")) %>%
  mutate(treatmentName = factor(treatmentName, levels = c("LLM persuasion, sonnet3.5",
                                                          "Human persuasion")))

# -----------------------------------------------------------------------------
# Run the TOST equivalence test for RQ1
#
# We are testing the equivalence of the mean compliance rate between the two conditions.
# With 10 questions per participant, individual compliance rates are in increments of 0.1.
# Hence, setting eqb = 0.1 implies that any difference smaller than 10 percentage points
# is considered trivial.
# -----------------------------------------------------------------------------
res_RQ1 <- t_TOST(formula = meanCompliance ~ treatmentName,
                  data = df_RQ1,
                  eqb = 0.1,       
                  smd_ci = "t") 

print(res_RQ1)

plot(res_RQ1, type = "simple")

# -----------------------------------------------------------------------------
# RQ2: Equivalence Test for Positive Persuasion Compliance
# -----------------------------------------------------------------------------

# Compute participant-level mean compliance for positive persuasion items
df_positive <- df_long %>%
  filter(persuasionDirection == "positive") %>%
  group_by(participantId) %>%
  summarize(meanCompliancePositive = mean(compliance, na.rm = TRUE)) %>%
  ungroup()

# Merge this measure back to the overall data (df_final)
df_RQ2 <- df_final %>%
  left_join(df_positive, by = "participantId") %>%
  filter(treatmentName %in% c("LLM persuasion, sonnet3.5", "Human persuasion"))

# Run the TOST equivalence test (assuming eqb = 0.1)
res_RQ2 <- t_TOST(formula = meanCompliancePositive ~ treatmentName,
                  data = df_RQ2,
                  eqb = 0.1,
                  smd_ci = "t")

# Print and plot the results for RQ2
print(res_RQ2)
plot(res_RQ2, type = "simple")

# -----------------------------------------------------------------------------
# RQ3: Equivalence Test for Negative Persuasion Compliance
# -----------------------------------------------------------------------------

# Compute participant-level mean compliance for negative persuasion items
df_negative <- df_long %>%
  filter(persuasionDirection == "negative") %>%
  group_by(participantId) %>%
  summarize(meanComplianceNegative = mean(compliance, na.rm = TRUE)) %>%
  ungroup()

# Merge this measure back to the overall data
df_RQ3 <- df_final %>%
  left_join(df_negative, by = "participantId") %>%
  filter(treatmentName %in% c("LLM persuasion, sonnet3.5", "Human persuasion"))

# Run the TOST equivalence test for negative persuasion compliance
res_RQ3 <- t_TOST(formula = meanComplianceNegative ~ treatmentName,
                  data = df_RQ3,
                  eqb = 0.1,
                  smd_ci = "t")

# Print and plot the results for RQ3
print(res_RQ3)
plot(res_RQ3, type = "simple")

# -----------------------------------------------------------------------------
# RQ4: Equivalence Test for Planned Contrasts (Positive Accuracy vs. Solo Quiz)
# -----------------------------------------------------------------------------

# Compute participant-level positive accuracy for persuasion groups 
# (only for items with a positive persuasion direction)
df_positive_accuracy <- df_long %>%
  filter(persuasionDirection == "positive") %>%
  group_by(participantId) %>%
  summarize(positiveAccuracy_p = mean(accuracy, na.rm = TRUE)) %>%
  ungroup()

# Create a new variable 'positiveAccuracy' for all participants:
# For control participants (e.g., treatmentName == "Solo Quiz"), we use overall accuracy.
# For persuasion participants, we use the computed positiveAccuracy_p.
df_RQ4 <- df_final %>%
  mutate(positiveAccuracy = if_else(treatmentName %in% c("LLM persuasion, sonnet3.5",
                                                         "Human persuasion"),
                                    NA_real_,
                                    meanAccuracy)) %>%
  left_join(df_positive_accuracy, by = "participantId") %>%
  mutate(positiveAccuracy = if_else(is.na(positiveAccuracy),
                                    positiveAccuracy_p,
                                    positiveAccuracy))

# Contrast 1: LLM Persuasion (positive accuracy) vs. Solo Quiz
df_RQ4_LLM <- df_RQ4 %>%
  filter(treatmentName %in% c("LLM persuasion, sonnet3.5", "Solo Quiz"))

res_RQ4_LLM <- t_TOST(formula = positiveAccuracy ~ treatmentName,
                      data = df_RQ4_LLM,
                      eqb = 0.1,
                      smd_ci = "t")
print(res_RQ4_LLM)
plot(res_RQ4_LLM, type = "simple")

# Contrast 2: Human Persuasion (positive accuracy) vs. Solo Quiz
df_RQ4_Human <- df_RQ4 %>%
  filter(treatmentName %in% c("Human persuasion", "Solo Quiz"))

res_RQ4_Human <- t_TOST(formula = positiveAccuracy ~ treatmentName,
                        data = df_RQ4_Human,
                        eqb = 0.1,
                        smd_ci = "t")
print(res_RQ4_Human)
plot(res_RQ4_Human, type = "simple")

# -----------------------------------------------------------------------------
# RQ5: Equivalence Test for Planned Contrasts (Negative Accuracy vs. Solo Quiz)
# -----------------------------------------------------------------------------

# Compute participant-level negative accuracy for persuasion groups:
# (Only include items with a negative persuasion direction.)
df_negative_accuracy <- df_long %>%
  filter(persuasionDirection == "negative") %>%
  group_by(participantId) %>%
  summarize(negativeAccuracy_p = mean(accuracy, na.rm = TRUE)) %>%
  ungroup()

# Create a unified negative accuracy variable.
# For control participants (Solo Quiz), use their overall meanAccuracy.
# For persuasion participants, use the computed negative accuracy.
df_RQ5 <- df_final %>%
  mutate(negativeAccuracy = if_else(treatmentName %in% c("LLM persuasion, sonnet3.5",
                                                         "Human persuasion"),
                                    NA_real_,
                                    meanAccuracy)) %>%
  left_join(df_negative_accuracy, by = "participantId") %>%
  mutate(negativeAccuracy = if_else(is.na(negativeAccuracy),
                                    negativeAccuracy_p,
                                    negativeAccuracy))

# Contrast 1: Compare 'LLM persuasion, sonnet3.5' versus 'Solo Quiz'
df_RQ5_LLM <- df_RQ5 %>%
  filter(treatmentName %in% c("LLM persuasion, sonnet3.5", "Solo Quiz"))

res_RQ5_LLM <- t_TOST(formula = negativeAccuracy ~ treatmentName,
                      data = df_RQ5_LLM,
                      eqb = 0.1,
                      smd_ci = "t")
print(res_RQ5_LLM)
plot(res_RQ5_LLM, type = "simple")

# Contrast 2: Compare 'Human persuasion' versus 'Solo Quiz'
df_RQ5_Human <- df_RQ5 %>%
  filter(treatmentName %in% c("Human persuasion", "Solo Quiz"))

res_RQ5_Human <- t_TOST(formula = negativeAccuracy ~ treatmentName,
                        data = df_RQ5_Human,
                        eqb = 0.1,
                        smd_ci = "t")
print(res_RQ5_Human)
plot(res_RQ5_Human, type = "simple")
