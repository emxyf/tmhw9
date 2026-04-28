#### regressions: set-up ####
# set reference levels
bsa$gender_eq <- relevel(bsa$gender_eq, ref = "Equal")
bsa$age_eq <- relevel(bsa$age_eq, ref = "Equal")
bsa$urb_rur_eq <- relevel(bsa$urb_rur_eq, ref = "Equal")
bsa$edu_eq <- relevel(bsa$edu_eq, ref = "Equal")
bsa$richpoor_eq <- relevel(bsa$richpoor_eq, ref = "Equal")

bsa$gender <- relevel(bsa$gender, ref = "Male")
bsa$race <- relevel(bsa$race, ref = "White")
bsa$education <- relevel(bsa$education, ref = "No qualifications")
bsa$gender <- relevel(bsa$gender, ref = "Male")
bsa$employment <- relevel(bsa$employment, ref = "No job")
bsa$hh_income <- relevel(bsa$hh_income, ref = "< £431") 
bsa$urban_rural <- relevel(bsa$urban_rural, ref = "Big city")
bsa$interest_politics <- relevel(bsa$interest_politics, ref = "Not at all")

# set survey design: complete cases only
bsa_design <- svydesign(ids = ~serial_number, strata = ~strata, weights = ~weights, data = bsa, nest = TRUE)

#### stage 1: baseline regressions for each DV ####

# express opinion on internet
baseline_express <- svyglm(express_internet ~ interest_politics + age + gender + race + education + employment + 
                             hh_income + urban_rural + left_right, 
                           data = bsa, family = quasibinomial(link = "logit"), design = bsa_design)
# voted
baseline_voted <- svyglm(voted ~ interest_politics + age + gender + race + education + employment + 
                           hh_income + urban_rural + left_right, 
                         data = bsa, family = quasibinomial(link = "logit"), design = bsa_design)
# sign petition
baseline_sign <- svyglm(sign_petition ~ interest_politics + age + gender + race + education + employment + 
                          hh_income + urban_rural + left_right, 
                        data = bsa, family = quasibinomial(link = "logit"), design = bsa_design)
# participate protest
baseline_participate <- svyglm(participate_protest ~ interest_politics + age + gender + race + education + employment + 
                                 hh_income + urban_rural + left_right, 
                               data = bsa, family = quasibinomial(link = "logit"), design = bsa_design)
# contact politician
baseline_contact <- svyglm(contact_politician ~ interest_politics + age + gender + race + education + employment + 
                             hh_income + urban_rural + left_right, 
                           data = bsa, family = quasibinomial(link = "logit"), design = bsa_design)
# organise protest
baseline_organise <- svyglm(organise_protest ~ interest_politics + age + gender + race + education + employment + 
                              hh_income + urban_rural + left_right, 
                            data = bsa, family = quasibinomial(link = "logit"), design = bsa_design)
# join group
baseline_join <- svyglm(join_group ~ interest_politics + age + gender + race + education + employment + 
                          hh_income + urban_rural + left_right, 
                        data = bsa, family = quasibinomial(link = "logit"), design = bsa_design)


# average marginal effects
tab_base_express <- summary(margins(baseline_express, design = bsa_design))
tab_base_voted <- summary(margins(baseline_voted, design = bsa_design))
tab_base_sign <- summary(margins(baseline_sign, design = bsa_design))
tab_base_participate <- summary(margins(baseline_participate, design = bsa_design))
tab_base_contact <- summary(margins(baseline_contact, design = bsa_design))
tab_base_organise <- summary(margins(baseline_organise, design = bsa_design))
tab_base_join <- summary(margins(baseline_join, design = bsa_design))

# export to excel spreadsheets
write.xlsx(tab_base_express, "base_express.xlsx", asTable = TRUE)
write.xlsx(tab_base_voted, "base_voted.xlsx", asTable = TRUE)
write.xlsx(tab_base_sign, "base_sign.xlsx", asTable = TRUE)
write.xlsx(tab_base_participate, "base_participate.xlsx", asTable = TRUE)
write.xlsx(tab_base_contact, "base_contact.xlsx", asTable = TRUE)
write.xlsx(tab_base_organise, "base_organise.xlsx", asTable = TRUE)
write.xlsx(tab_base_join, "base_join.xlsx", asTable = TRUE)



#### stage 2: full model for each DV ####
# express opinion on internet
full_express <- svyglm(express_internet ~ gender_eq + age_eq + urb_rur_eq + edu_eq + 
                         richpoor_eq + interest_politics + age + gender + race + education + employment + 
                         hh_income + urban_rural + left_right, 
                       data = bsa, family = quasibinomial(link = "logit"), design = bsa_design)

# voted: this has more statistically significant results for IVs
full_voted <- svyglm(voted ~ gender_eq + age_eq + urb_rur_eq + edu_eq + 
                       richpoor_eq + interest_politics + age + gender + race + education + employment + 
                       hh_income + urban_rural + left_right, 
                     data = bsa, family = quasibinomial(link = "logit"), design = bsa_design)

# sign petition
full_sign <- svyglm(sign_petition ~ gender_eq + age_eq + urb_rur_eq + edu_eq + 
                      richpoor_eq + interest_politics + age + gender + race + education + employment + 
                      hh_income + urban_rural + left_right, 
                    data = bsa, family = quasibinomial(link = "logit"), design = bsa_design)

# participate protest
full_participate <- svyglm(participate_protest ~ gender_eq + age_eq + urb_rur_eq + edu_eq + 
                             richpoor_eq + interest_politics + age + gender + race + education + employment + 
                             hh_income + urban_rural + left_right, 
                           data = bsa, family = quasibinomial(link = "logit"), design = bsa_design)

# contact politician
full_contact <- svyglm(contact_politician ~ gender_eq + age_eq + urb_rur_eq + edu_eq + 
                         richpoor_eq + interest_politics + age + gender + race + education + employment + 
                         hh_income + urban_rural + left_right, 
                       data = bsa, family = quasibinomial(link = "logit"), design = bsa_design)

# organise protest
full_organise <- svyglm(organise_protest ~ gender_eq + age_eq + urb_rur_eq + edu_eq + 
                          richpoor_eq + interest_politics + age + gender + race + education + employment + 
                          hh_income + urban_rural + left_right, 
                        data = bsa, family = quasibinomial(link = "logit"), design = bsa_design)
# join group
full_join <- svyglm(join_group ~ gender_eq + age_eq + urb_rur_eq + edu_eq + 
                      richpoor_eq + interest_politics + age + gender + race + education + employment + 
                      hh_income + urban_rural + left_right, 
                    data = bsa, family = quasibinomial(link = "logit"), design = bsa_design)


# average marginal effects
tab_full_express <- summary(margins(full_express, design = bsa_design))
tab_full_voted <- summary(margins(full_voted, design = bsa_design))
tab_full_sign <- summary(margins(full_sign, design = bsa_design))
tab_full_participate <- summary(margins(full_participate, design = bsa_design))
tab_full_contact <- summary(margins(full_contact, design = bsa_design))
tab_full_organise <- summary(margins(full_organise, design = bsa_design))
tab_full_join <- summary(margins(full_join, design = bsa_design))

# export to excel
write.xlsx(tab_full_express, "full_express.xlsx", asTable = TRUE)
write.xlsx(tab_full_voted, "full_voted.xlsx", asTable = TRUE)
write.xlsx(tab_full_sign, "full_sign.xlsx", asTable = TRUE)
write.xlsx(tab_full_participate, "full_participate.xlsx", asTable = TRUE)
write.xlsx(tab_full_contact, "full_contact.xlsx", asTable = TRUE)
write.xlsx(tab_full_organise, "full_organise.xlsx", asTable = TRUE)
write.xlsx(tab_full_join, "full_join.xlsx", asTable = TRUE)

