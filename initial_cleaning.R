library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##### data set-up #######
library(dplyr)
library(haven) # to read .sav file
library(survey) # to incorporate survey weights
library(mice) # for imputation
library(mitools) # for imputation
library(openxlsx) # to export data
library(Hmisc) # for descriptive statistics
library(car) # multicollinearity checks
library(margins) # for finding average marginal effects
library(svyROC) # to incorporate survey weights into ROC plots, robustness check

# select relevant columns
bsa_raw <- read_sav("bsa2024.sav")
View(bsa_raw) # inspect data

# select relevant columns
bsa_select <- select(bsa_raw, serial_EUL, RespAge_Archive, DVsex24, Raceori4, 
                     HEdQual2, EMPSTAT, HHincome, ISSP_UrbRur, leftrigh, ISSP_Q22, 
                     ISSP_Q30, ISSP_Q31, ISSP_Q32, ISSP_Q33, ISSP_Q34, ISSP_Q16, 
                     Voted, ISSP_Q23_A_q, ISSP_Q23_B_q, ISSP_Q23_C_q, ISSP_Q23_D_q, 
                     ISSP_Q23_E_q, BSA24_final_wt, BSA24_strata)

# check which respondents did not receive the ISSP survey, create column indicating which ones did not
bsa_select <- bsa_select %>%
  mutate(
    missing_ISSP = if_all(contains("ISSP"), is.na),
    missing_ISSP = as.integer(missing_ISSP)
  ) 

# create missingness table
n_missing <- colSums(is.na(bsa_select))
n_total <- nrow(bsa_select)
prop_missing <- n_missing/n_total
initial_missing_table <- data.frame(variable = names(bsa_select), count = n_missing, prop = prop_missing)
initial_missing_table # view table

# export into excel
write.xlsx(initial_missing_table, "initial_missing.xlsx", asTable = TRUE)

# filter out missing ISSP responses
bsa_select <- bsa_select %>% 
  filter(missing_ISSP == 0) %>%
  select(-missing_ISSP)

nrow(bsa_select) # count number of remaining rows
View(bsa_select) # check if filtering was successful

# rename columns
colnames(bsa_select) <- c("serial_number", "age", "gender", "race", "education", 
                          "employment", "hh_income", "urban_rural", 
                          "left_right", "interest_politics", "gender_eq", 
                          "age_eq", "urb_rur_eq", "edu_eq", "richpoor_eq", 
                          "express_internet", "voted", "sign_petition", 
                          "participate_protest", "contact_politician", "organise_protest", 
                          "join_group", "weights", "strata")
View(bsa_select) # check that the columns were correctly renamed

# re-code invalid answers as NA
bsa_select <- bsa_select %>% 
  mutate(
    age = if_else(age %in% c(-1, 998, 999), NA, age),
    gender = if_else(gender %in% c(-1, 8, 9), NA, gender),
    race = if_else(race %in% c(-1, 8, 9), NA, race),
    education = if_else(education %in% c(-1, 8, 9, 906), NA, education),
    employment = if_else(employment %in% c(-1, 8, 9), NA, employment),
    hh_income = if_else(hh_income %in% c(-1, 8, 9), NA, hh_income),
    urban_rural = if_else(urban_rural %in% c(-1, 9, 906, 907), NA, urban_rural),
    left_right = if_else(left_right %in% c(-1, 9), NA, left_right),
    interest_politics = if_else(interest_politics %in% c(-1, 6, 9), NA, interest_politics),
    gender_eq = if_else(gender_eq %in% c(-1, 5, 9), NA, gender_eq),
    age_eq = if_else(age_eq %in% c(-1, 5, 9), NA, age_eq),
    urb_rur_eq = if_else(urb_rur_eq %in% c(-1, 5, 9), NA, urb_rur_eq),
    edu_eq = if_else(edu_eq %in% c(-1, 5, 9), NA, edu_eq),
    richpoor_eq = if_else(richpoor_eq %in% c(-1, 5, 9), NA, richpoor_eq),
    express_internet = if_else(express_internet %in% c(-1, 7, 9), NA, express_internet),
    voted = if_else(voted %in% c(-1, 3, 8, 9), NA, voted),
    sign_petition = if_else(sign_petition %in% c(-1, 5, 9), NA, sign_petition),
    participate_protest = if_else(participate_protest %in% c(-1, 5, 9), NA, participate_protest),
    contact_politician = if_else(contact_politician %in% c(-1, 5, 9), NA, contact_politician),
    organise_protest = if_else(organise_protest %in% c(-1, 5, 9), NA, organise_protest),
    join_group = if_else(join_group %in% c(-1, 5, 9), NA, join_group)
  )

# check for missingness: create data frame with count and proportion of each column's missingness
missing <- data.frame(
  full_var_names <- c(
    "serial_number","age","gender", "race", "education", "employment", "hh_income",
    "urban_rural", "left_right", "interest_politics", "gender_eq", "age_eq", "urb_rur_eq",
    "edu_eq","richpoor_eq", "express_internet", "voted", "sign_petition", 
    "participate_protest", "contact_politician", "organise_protest", "join_group",
    "weights", "strata" 
  ), # include all variable names
  missing_count <- sapply(bsa_select, function(i) sum(is.na(i))), # number of missing values
  missing_prop <- colMeans(is.na(bsa_select)) # proportion of missing values
)

# missingness patterns in IVs, DVs
names(missing) <- c("variable", "number of missing values", "proportion of missing values")
write.xlsx(missing, file = "missingness_patterns.xlsx", asTable = TRUE) # export data


# re-code voted variable as binary for logistic regression
bsa_select$voted <- ifelse(bsa_select$voted == 1, 1, 0) # yes = 1, no = 0


# political participation variables: re-code offline and online into only yes or no
bsa_select$sign_petition <- ifelse(bsa_select$sign_petition == 1 | 
                                     bsa_select$sign_petition == 2 | 
                                     bsa_select$sign_petition == 3, 1, 0) # signed = 1, not signed = 0

bsa_select$participate_protest <- ifelse(bsa_select$participate_protest == 1 | 
                                           bsa_select$participate_protest == 2 | 
                                           bsa_select$participate_protest == 3, 1, 0)

bsa_select$contact_politician <- ifelse(bsa_select$contact_politician == 1 | 
                                          bsa_select$contact_politician == 2 | 
                                          bsa_select$contact_politician == 3, 1, 0)

bsa_select$organise_protest <- ifelse(bsa_select$organise_protest == 1 | 
                                        bsa_select$organise_protest == 2 | 
                                        bsa_select$organise_protest == 3, 1, 0)

bsa_select$join_group <- ifelse(bsa_select$join_group == 1 | 
                                  bsa_select$join_group == 2 | 
                                  bsa_select$join_group == 3, 1, 0)

# recode into levels based on frequency of expression
bsa_select$express_internet <- ifelse(bsa_select$express_internet == 1 | 
                                        bsa_select$express_internet == 2
                                      | bsa_select$express_internet == 3 | 
                                        bsa_select$express_internet == 4
                                      | bsa_select$express_internet == 5, 1, 0)

# create data frame summarising number and percentage of occurrences of "neither" in each inequality variable
eq_vars <- c("gender_eq", "age_eq", "edu_eq", "urb_rur_eq", "richpoor_eq")
neither_summary <- data.frame(
  variable  = eq_vars,
  count_4   = sapply(eq_vars, function(v)
    sum(bsa_select[[v]] == 4, na.rm = TRUE)
  ),
  percent_4 = sapply(eq_vars, function(v)
    sum(bsa_select[[v]] == 4, na.rm = TRUE) /
      sum(!is.na(bsa_select[[v]])) * 100
  )
)
# rename columns
names(neither_summary) <- c("variable", "number of 'neither'", 
                            "percentage of 'neither'")
# export into excel
write.xlsx(neither_summary, file = "inequality_neither.xlsx", asTable = TRUE)

# re-code equality variables to indicate "neither benefit" also count as "equal benefit"
bsa_select$gender_eq <- ifelse(bsa_select$gender_eq == 4, 2, bsa_select$gender_eq)
bsa_select$age_eq <- ifelse(bsa_select$age_eq == 4, 2, bsa_select$age_eq)
bsa_select$edu_eq <- ifelse(bsa_select$edu_eq == 4, 2, bsa_select$edu_eq)
bsa_select$urb_rur_eq <- ifelse(bsa_select$urb_rur_eq == 4, 2, bsa_select$urb_rur_eq)
bsa_select$richpoor_eq <- ifelse(bsa_select$richpoor_eq == 4, 2, bsa_select$richpoor_eq)

#### re-code categorical variable level names ####
bsa_select <- bsa_select %>%
  mutate(
    gender = factor(gender,
                    levels = c(1, 2),
                    labels = c("Female", "Male")),
    race = factor(race,
                  levels = c(1, 2, 3, 4, 5),
                  labels = c("Black", "Asian", "White", "Mixed", "Other")),
    education = factor(education,
                       levels = c(1, 2, 3, 4, 6),
                       labels = c("Degree and above", "Other Higher Education", 
                                  "A-levels and equivalent", "GCSE and equivalent",
                                  "No qualifications")),
    hh_income = factor(hh_income,
                       levels = c(1, 2, 3, 4),
                       labels = c("< £431", "£431 - £700", 
                                  "£701 - £1,060",
                                  "£1,061+")),
    employment = factor(employment,
                        levels = c(1, 2, 3, 4),
                        labels = c("Employed", "Self-Employed with employees", 
                                   "Self-employed without employees",
                                   "No job")),
    urban_rural = factor(urban_rural,
                         levels = c(1, 2, 3, 4, 5),
                         labels = c("Big city", "Suburbs", "Small city", 
                                    "Country village", "Farm")),
    interest_politics = factor(interest_politics,
                               levels = c(1, 2, 3, 4, 5),
                               labels = c("Very", "Fairly", "Somewhat", "Not very",
                                          "Not at all")),
    gender_eq = factor(gender_eq, 
                       levels = c(1, 2, 3),
                       labels = c("Women", "Equal", "Men")),
    age_eq = factor(age_eq,
                    levels = c(1, 2, 3),
                    labels = c("Older", "Equal", "Younger")),
    urb_rur_eq = factor(urb_rur_eq, 
                        levels = c(1, 2, 3),
                        labels = c("Countryside", "Equal", "Cities")),
    edu_eq = factor(edu_eq,
                    levels = c(1, 2, 3),
                    labels = c("Highly educated", "Equal", "Less educated")),
    richpoor_eq = factor(richpoor_eq, 
                         levels = c(1, 2, 3),
                         labels = c("Rich", "Equal", "Poor")),
    express_internet = factor(express_internet,
                              levels = c(0, 1),
                              labels = c("No", "Yes")),
    voted = factor(voted,
                   levels = c(0, 1),
                   labels = c("No", "Yes")),
    sign_petition = factor(sign_petition,
                           levels = c(0, 1),
                           labels = c("No", "Yes")),
    participate_protest = factor(participate_protest,
                                 levels = c(0, 1),
                                 labels = c("No", "Yes")),
    contact_politician = factor(contact_politician,
                                levels = c(0, 1),
                                labels = c("No", "Yes")),
    organise_protest = factor(organise_protest,
                              levels = c(0, 1),
                              labels = c("No", "Yes")),
    join_group = factor(join_group,
                        levels = c(0, 1),
                        labels = c("No", "Yes"))
  )


# filter for only complete cases
bsa <- bsa_select[complete.cases(bsa_select), ]
View(bsa)
nrow(bsa) # 724 rows compared to 1529


