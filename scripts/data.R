
  library(readxl)
  library(tidyverse)

# Data -------------------------------------------------------------------------

  data_1 <- read_excel("data/Combined Data 2010 to 2016 20170718 LS_AF.xlsx",
                     na = "999")

  data_2 <- read_excel("data/Combined Data 2010 to 2016 20170718 LS_AF.xlsx",
                       na = "999", sheet = 2)


# names(data)
# lapply(data, class)
# lapply(data, table)
# lapply(data, function(x) table(is.na(x)))

  names(data_1) <- c(
    "id",
    "sex",
    "bmi",
    "obese",
    "age",
    "over_65",
    "stay_length",
    "procedure",
    "procedure_year",
    "joint_register_appointment",
    "vte_prophylaxis",
    "vte_type",
    "vte_outcome",
    "pre_surgery_anticoagulant",
    "anticoagulant_type",
    "dvt_hx"
  )

  names(data_2) <- names(data_1)

  data <- bind_rows(data_1, data_2); rm(data_1, data_2)

# Variables --------------------------------------------------------------------

  yesno <- c("obese", "over_65", "vte_outcome", "pre_surgery_anticoagulant")
  data[yesno] <- map_df(data[yesno],
                        function(x) factor(x, labels = c("No", "Yes")))

# Check against continuous data
# ggplot(data, aes(x = age, fill = over_65)) + geom_histogram()
# ggplot(data, aes(x = bmi, fill = obese)) + geom_histogram()

  data$sex <- factor(data$sex, labels = c("Males", "Female"))

  proph_lab <- c(
  "No Chemical Prophylaxis (mechanical only)",
  "Inpatient Enoxaparin",
  "Modified rivaroxaban regimen",
  "LMWH, then warfarin",
  "Rivaroxaban prophylaxis as per SPC",
  "Enoxaparin, then aspirin",
  "Asprin alone",
  "Enoxaparin and Fondaparinux",
  "Surgery Cancelled",
  "LMWH, then therapeutic NOAC",
  "Enoxaparin and Indomethacin",
  "Indomethacin then Aspirin",
  "Enoxaparin for 5/52",
  "Therapeutic LMWH for 4/52",
  "Enoxaparin, then aspirin and clopidogrel",
  "Enoxaparin, then clopidogrel",
  "Enoxaparin, then aspirin and asasantin",
  "Enoxaparin, then aspirin, clopidogrel and asasantin retard",
  "Enoxaparin, then aspirin and persantin retard",
  "Enoxaparin, then asasantin retard",
  "Enoxaparin, then prasugrel and aspirin",
  "Enoxaparin, then persantin retard",
  "Enoxaparin for 4/52",
  "Enoxaparin for 6/52 with aspirin 150mg",
  "Enoxaparin for 6/52",
  "Enoxaparin, then dabigatran thromboprophylaxis dose"
  )

  data$vte_prophylaxis <- factor(data$vte_prophylaxis,
                                 levels = 0:25, labels = proph_lab)

  data <- group_by(data, vte_prophylaxis) %>%
    summarise(n = n()) %>%
    full_join(data, by = "vte_prophylaxis") %>%
    mutate(vte_prophylaxis = reorder(vte_prophylaxis, n))

  data$procedure <- factor(data$procedure,
                           levels = c(1, 2), labels = c("TKR", "THR"))

  vte_labs <- c("None", "DVT before discharge", "PE before discharge",
                "DVT at 6 months", "PE at 6 months")

  data$vte_type <- factor(data$vte_type,
                          levels = c(0:4), labels = vte_labs)

  ac_type <- c("None", "Aspirin", "Aspirin and Asasantin Retard",
               "Asasantin Retard", "Aspirin and Clopidogrel",
               "Aspirin and Dabigatran", "Aspirin and Warfarin",
               "Clopidogrel", "Warfarin", "Rivaroxaban",
               "Clopidogrel and Asasantin Retard",
               "Aspirin and Persantin Retard",
               "Rivaroxaban and Aspirin",
               "Prasugrel and Aspirin",
               "Aspirin and Ticagrelor",
               "Rivaroxaban and Clopidogrel",
               "Warfarin and Clopidogrel",
               "Aspirin clopidogrel and Asasantin Retard",
               "Apixaban",
               "Dabigatran",
               "Apixaban and Aspirin",
               "Aspirin Assasantin Retard and Rivaroxaban",
               "Persantin Retard",
               "Apixaban and Clopidogrel")

  data$anticoagulant_type <- factor(data$anticoagulant_type,
                                    levels = c(0:23), labels = ac_type)


  data$dvt_hx <- factor(data$dvt_hx,
                        levels = c(0:2), labels = c("No", "DVT", "PE"))

  data$joint_register_appointment <- "Yes"

# Bootstrapping the difference

# They are almost exactly the same
  with(
    filter(data, vte_prophylaxis %in% levels(vte_prophylaxis)[c(23, 25)] &
             !is.na(vte_prophylaxis)) %>% droplevels(),
    table(vte_prophylaxis, vte_outcome)
  )

  filter(data, vte_prophylaxis %in% levels(vte_prophylaxis)[c(23, 25)] &
           !is.na(vte_prophylaxis)) %>%
    group_by(vte_prophylaxis) %>%
    summarise(vte_rate = mean(as.numeric(vte_outcome) - 1))

  data <- mutate(data, anticoagulant_type_2 = case_when(
    anticoagulant_type == levels(data$anticoagulant_type)[1] ~
      levels(data$anticoagulant_type)[1],
    anticoagulant_type == levels(data$anticoagulant_type)[2] ~
      levels(data$anticoagulant_type)[2],
    anticoagulant_type %in% levels(data$anticoagulant_type)[2:24] ~
      "Other"
    )
  )

  data$anticoagulant_type_2 <- factor(
    data$anticoagulant_type_2,
    levels = c("None", "Aspirin", "Other")
    )

  data <- mutate(data, vte_prophylaxis_2 = case_when(
    vte_prophylaxis == levels(data$vte_prophylaxis)[25] ~
      levels(data$vte_prophylaxis)[25],
    vte_prophylaxis == levels(data$vte_prophylaxis)[24] ~
      levels(data$vte_prophylaxis)[24],
    vte_prophylaxis == levels(data$vte_prophylaxis)[23] ~
      levels(data$vte_prophylaxis)[23],
    vte_prophylaxis %in% levels(data$vte_prophylaxis)[c(1:22, 26)] ~
      "Other"
    )
  )

  data$vte_prophylaxis_2 <- factor(data$vte_prophylaxis_2)

  data$any_dvt_hx <- NA
  data$any_dvt_hx <- ifelse(data$dvt_hx == "No", 0, data$any_dvt_hx)
  data$any_dvt_hx <- ifelse(data$dvt_hx != "No", 1, data$any_dvt_hx)

  data$year_group <- ifelse(as.numeric(data$procedure_year) < 2013,
                            "Early", "Late") %>% factor()
  data <- mutate(data, vte_outcome_num = as.numeric(vte_outcome) - 1,
                 year_group_num = as.numeric(year_group) - 1) %>%
    droplevels()


  save(data, file = "data.RData")
  rm(list = ls())
  load("data.RData")

