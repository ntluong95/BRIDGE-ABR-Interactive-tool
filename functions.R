#functions.R

#*************************************************************************************
#Import data
read_data <- function(data_folder) {
  csv_files <- list.files(path = data_folder, pattern = "*.csv", full.names = T)
  csv_data <- csv_files %>%
    map(read_csv)
  excel_files <- list.files(
    path = data_folder,
    pattern = "*.xlsx",
    full.names = T
  )
  excel_data <- excel_files %>%
    map(readxl::read_excel)
  full_list <- csv_data %>%
    append(excel_data)
  return(full_list)
}
#*************************************************************************************
sum.na <- function(df) {
  if (all(is.na(df))) {
    suma <- NA
  } else {
    suma <- sum(df, na.rm = T)
  }
  return(suma)
}
#*************************************************************************************
mean.na <- function(df) {
  if (all(is.na(df))) {
    mean.df <- NA
  } else {
    mean.df <- mean(df, na.rm = T)
  }
  return(mean.df)
}
#**************************************************************************************
#Clean data

wrangle_data <- function(data_list, meat_list) {
  population_imports <- data_list[[4]] %>%
    rename(iso3c = 'Country (ISO3 code)') %>%
    select(iso3c, '[2015]', '[2016]', '[2017]') %>%
    pivot_longer(
      '[2015]':'[2017]',
      names_to = "year",
      values_to = "population"
    ) %>%
    mutate(population = population * 1000) #Due to unit in 1000s

  population_production <- data_list[[4]] %>%
    rename(iso3c = 'Country (ISO3 code)') %>%
    select(
      iso3c,
      '[2006]',
      '[2007]',
      '[2008]',
      '[2009]',
      '[2010]',
      '[2011]',
      '[2012]',
      '[2013]',
      '[2014]',
      '[2015]',
      '[2016]'
    ) %>%
    pivot_longer(
      '[2006]':'[2016]',
      names_to = "year",
      values_to = "population"
    ) %>%
    mutate(population = population * 1000) %>%
    group_by(iso3c) %>%
    summarise(av_population = mean(population))

  trade_data <- data_list[[3]] %>%
    rename(iso3c = 'Country (ISO3 code)') %>%
    rename(flow = 'Trade flow (Name)') %>%
    rename(com_code = 'Commodity (Code)') %>%
    select(iso3c, com_code, flow, '[2015]', '[2016]', '[2017]') %>%
    pivot_longer('[2015]':'[2017]', names_to = "year", values_to = "tonnes") %>%
    filter(str_detect(com_code, "^03")) %>%
    filter(com_code != "0301.11") %>%
    filter(com_code != "0301.19") %>%
    filter(flow == "Import") %>% #|flow=="Export" no loger include Exports
    group_by(iso3c, flow, year) %>%
    summarise(flow_sum_year = sum(tonnes) * 1000) %>% #*1000 to get from tonnes to kg
    left_join(population_imports) %>%
    mutate(yearly_import_kg_percap = flow_sum_year / population) %>%
    ungroup() %>%
    group_by(iso3c, flow) %>%
    summarise(import_kg_percap_year = mean(yearly_import_kg_percap)) %>%
    mutate_at(vars(import_kg_percap_year), ~ replace(., is.nan(.), 0)) #Since all NaN have appeared in calculations of 0

  red_meat <- meat_list[[1]] %>%
    select("...1", "...2", "Bovine Meat", "Mutton & Goat Meat", "Pigmeat") %>%
    rename(bovine_09 = "Bovine Meat") %>%
    rename(mutton_09 = "Mutton & Goat Meat") %>%
    rename(pig_09 = "Pigmeat") %>%
    left_join(meat_list[[2]]) %>%
    select(
      "...1",
      "...2",
      bovine_09,
      mutton_09,
      pig_09,
      "Bovine Meat",
      "Mutton & Goat Meat",
      "Pigmeat"
    ) %>%
    rename(bovine_10 = "Bovine Meat") %>%
    rename(mutton_10 = "Mutton & Goat Meat") %>%
    rename(pig_10 = "Pigmeat") %>%
    left_join(meat_list[[3]]) %>%
    select(
      "...1",
      "...2",
      bovine_09,
      mutton_09,
      pig_09,
      bovine_10,
      mutton_10,
      pig_10,
      "Bovine Meat",
      "Mutton & Goat Meat",
      "Pigmeat"
    ) %>%
    rename(bovine_11 = "Bovine Meat") %>%
    rename(mutton_11 = "Mutton & Goat Meat") %>%
    rename(pig_11 = "Pigmeat") %>%
    slice(-(1:2)) %>%
    rename(iso3c = ...1) %>%
    rename(country = ...2) %>%
    mutate(across(bovine_09:pig_11, na_if, "*")) %>%
    mutate_at(vars(-iso3c, -country), as.numeric) %>%
    rowwise() %>%
    mutate(red_meat_09 = sum.na(c(bovine_09, mutton_09, pig_09))) %>%
    mutate(red_meat_10 = sum.na(c(bovine_10, mutton_10, pig_10))) %>%
    mutate(red_meat_11 = sum.na(c(bovine_11, mutton_11, pig_11))) %>%
    mutate(
      red_meat_gcapday = mean.na(c(red_meat_09, red_meat_10, red_meat_11))
    ) %>%
    mutate(red_meat_gcapday = red_meat_gcapday * 0.746) %>% #Conversion of raw to cooked weight
    select(iso3c, red_meat_gcapday)

  ruminant_meat <- meat_list[[1]] %>%
    select("...1", "...2", "Bovine Meat", "Mutton & Goat Meat") %>%
    rename(bovine_09 = "Bovine Meat") %>%
    rename(mutton_09 = "Mutton & Goat Meat") %>%
    left_join(meat_list[[2]]) %>%
    select(
      "...1",
      "...2",
      bovine_09,
      mutton_09,
      "Bovine Meat",
      "Mutton & Goat Meat"
    ) %>%
    rename(bovine_10 = "Bovine Meat") %>%
    rename(mutton_10 = "Mutton & Goat Meat") %>%
    left_join(meat_list[[3]]) %>%
    select(
      "...1",
      "...2",
      bovine_09,
      mutton_09,
      bovine_10,
      mutton_10,
      "Bovine Meat",
      "Mutton & Goat Meat"
    ) %>%
    rename(bovine_11 = "Bovine Meat") %>%
    rename(mutton_11 = "Mutton & Goat Meat") %>%
    slice(-(1:2)) %>%
    rename(iso3c = ...1) %>%
    rename(country = ...2) %>%
    mutate(across(bovine_09:mutton_11, na_if, "*")) %>%
    mutate_at(vars(-iso3c, -country), as.numeric) %>%
    rowwise() %>%
    mutate(ruminant_meat_09 = sum.na(c(bovine_09, mutton_09))) %>%
    mutate(ruminant_meat_10 = sum.na(c(bovine_10, mutton_10))) %>%
    mutate(ruminant_meat_11 = sum.na(c(bovine_11, mutton_11))) %>%
    mutate(
      ruminant_meat_gcapday = mean.na(c(
        ruminant_meat_09,
        ruminant_meat_10,
        ruminant_meat_11
      ))
    ) %>%
    mutate(ruminant_meat_gcapday = ruminant_meat_gcapday * 0.746) %>% #Conversion of raw to cooked weight
    select(iso3c, ruminant_meat_gcapday)

  dalys <- data_list[[8]] %>%
    pivot_longer(!Category, names_to = "iso3c", values_to = "daly") %>%
    pivot_wider(names_from = Category, values_from = daly) %>%
    filter(iso3c != "AFG") %>% #Since no data and is messing up code
    rename(diabetes = 'Diabetes mellitus') %>%
    rename(cardiovascular = 'Cardiovascular diseases') %>%
    rowwise() %>%
    mutate(DALY_cardiovascular_cap = cardiovascular / population) %>%
    select(iso3c, DALY_cardiovascular_cap)

  clean_data <- data_list[[1]] %>%
    full_join(data_list[[2]]) %>%
    select(!'...1') %>%
    full_join(data_list[[5]]) %>%
    select(!'...1') %>%
    full_join(data_list[[6]]) %>%
    select(!'...1') %>%
    full_join(data_list[[7]]) %>%
    full_join(data_list[[9]]) %>%
    full_join(population_production) %>%
    full_join(dalys) %>%
    mutate(mean_total_production = as.numeric(mean_total_production)) %>%
    mutate(
      prod_kgpercap_year = (mean_total_production / av_population) * 1000
    ) %>%
    full_join(trade_data) %>%
    #full_join(GHG_data) %>%
    full_join(red_meat) %>%
    full_join(ruminant_meat) %>%
    filter(!is.na(iso3c)) %>%
    select(
      iso3c,
      ssp585_2050,
      SEV_omega3,
      aq_reliance_ratio,
      SEV_vitB12,
      prod_kgpercap_year,
      import_kg_percap_year,
      red_meat_gcapday,
      ruminant_meat_gcapday,
      DALY_cardiovascular_cap,
      export_percgdp,
      totjobs_percap
    ) %>%
    mutate(aq_reliance_ratio = as.numeric(aq_reliance_ratio)) %>%
    mutate(iso3c = as.factor(iso3c)) %>%
    mutate(num.col = rowSums(!is.na(.))) %>%
    mutate(data_coverage = num.col - 1) %>%
    mutate(data_coverage_percent = round((data_coverage / 11) * 100)) %>%
    select(!num.col) %>%
    ungroup()

  return(clean_data)
}
#***************************************************************************************
#Saving file for app
save_file_and_return_path <- function(clean_data) {
  write.csv(clean_data, file = ("./App-Directory/clean_data.csv"))

  return("./App-Directory/clean_data.csv")
}

#**************************************************************************************
#Policy recommendation assignment
assigning_recommendation <- function(clean_data) {
  #Policy inclusion criteria
  #**************************
  #Policy 0 -
  #SEV_vitB12 AND (prod_kgpercap_year OR import_kg_percap_year)

  #Policy 1 -
  #(SEV_omega3 OR SEV_vitB12)  AND (prod_kgpercap_year OR import_kg_percap_year)

  # Policy 2
  # red_meat_gcapday

  #Policy 3 - full inclusion with AND, partial with OR
  #red_meat_gcapday AND daly_cardiovascular_cap

  #Policy 4 - only full
  #mean_total_production OR total_GHG

  #Policy 5 - full inclusion
  #(totjobs_percap OR exportpercgdp OR (prop_aquatic_omega3 OR prop_aquatic_B12)) AND ssp585_2050
  #Policy 5 - partial inclusion
  #(totjobs_percap OR export_percgdp OR (prop_aquatic_omega3 OR prop_aquatic_B12))

  #Inclusion Cut-offs for each variable
  #************************************
  #SEV_omega3 H =>10
  #SEV_vitB12  H=>10
  #fish_relative_caloric_price =1.75

  #bf_availability = prod_kgpercap_year + import_kg_percap_year > 8 (50% of global without China)

  #red_meat_gcapday =>50 (with pigmeat)
  #ruminant_meat_gcapday-> 7 (without pigmeat)
  #daly_cardiovascular_cap = 0.05
  #mean_total_production > 1 000 000
  #total_GHG >1500000000
  #totjobs_percap >0.01
  #export_percgdp > 0.03
  #prop_aquatic_omega3 > 0.2
  #prop_aquatic_B12 >0.2
  #ssp585_2050 > 50
  #aq_reliance_ratio >0.2

  #Example plotting code for deciding graph based cut-offs - change y variable
  #ggplot(clean_data) +
  #geom_point(aes(reorder(iso3c, DALY_cardiovascular_cap
  #                       , mean),
  #               y=DALY_cardiovascular_cap))+
  #  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  #  xlab("Countries")

  #Update from review process - split omega and b12 into two separate policies.For minimal changes - calling new policy = policy 0
  policy_data <- clean_data %>%
    mutate(
      bf_availability_kgcap_year = prod_kgpercap_year + import_kg_percap_year
    ) %>%
    mutate(
      SEV_omega3_inclusion = case_when(
        (SEV_omega3 >= 10) ~ 1,
        (SEV_omega3 < 10) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      SEV_B12_inclusion = case_when(
        (SEV_vitB12 >= 10) ~ 1,
        (SEV_vitB12 < 10) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      availability_inclusion = case_when(
        (bf_availability_kgcap_year >= 8) ~ 1,
        (bf_availability_kgcap_year < 8) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      red_meat_inclusion = case_when(
        (red_meat_gcapday >= 50) ~ 1,
        (red_meat_gcapday < 50) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      ruminant_meat_inclusion = case_when(
        (ruminant_meat_gcapday >= 7) ~ 1,
        (ruminant_meat_gcapday < 7) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      daly_inclusion = case_when(
        (DALY_cardiovascular_cap >= 0.05) ~ 1,
        (DALY_cardiovascular_cap < 0.05) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      jobs_inclusion = case_when(
        (totjobs_percap >= 0.01) ~ 1,
        (totjobs_percap < 0.01) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      export_inclusion = case_when(
        (export_percgdp >= 0.03) ~ 1,
        (export_percgdp < 0.03) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      consump_inclusion = case_when(
        (aq_reliance_ratio >= 0.2) ~ 1,
        (aq_reliance_ratio < 0.2) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      climate_inclusion = case_when(
        (ssp585_2050 >= 50) ~ 1,
        (ssp585_2050 < 50) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    rowwise() %>%
    #mutate(SEV_inclusion = case_when((SEV_B12_inclusion==1 & SEV_omega3_inclusion==1)~2,
    #                                 (SEV_B12_inclusion==1 | SEV_omega3_inclusion==1)~1,
    #                                 (SEV_B12_inclusion==0 & SEV_omega3_inclusion==0)~0,
    #                                 TRUE~NA_real_)) %>%
    #mutate(policy_1=case_when((SEV_inclusion>0 & availability_inclusion ==1) ~"highly_relevant", #OLD policy 1 where nutrients were combined
    #                          (SEV_inclusion>0 & availability_inclusion !=1) ~"relevant",
    #                          (SEV_inclusion==0) ~ "less_relevant",
    #                          TRUE ~"missing_data")) %>%
    mutate(
      policy_0 = case_when(
        (SEV_B12_inclusion == 1 &
          availability_inclusion == 1) ~ "highly_relevant",
        (SEV_B12_inclusion == 1 & availability_inclusion != 1) ~ "relevant",
        (SEV_B12_inclusion == 0) ~ "less_relevant",
        TRUE ~ "missing_data"
      )
    ) %>%
    mutate(
      policy_1 = case_when(
        (SEV_omega3_inclusion == 1 &
          availability_inclusion == 1) ~ "highly_relevant",
        (SEV_omega3_inclusion == 1 & availability_inclusion != 1) ~ "relevant",
        (SEV_omega3_inclusion == 0) ~ "less_relevant",
        TRUE ~ "missing_data"
      )
    ) %>%
    mutate(
      policy_2 = case_when(
        (red_meat_inclusion == 1 &
          daly_inclusion == 1 &
          availability_inclusion == 1) ~ "highly_relevant",
        (red_meat_inclusion == 1 &
          daly_inclusion == 1 &
          availability_inclusion != 1) ~ "relevant",
        (red_meat_inclusion == 0 & daly_inclusion == 0) ~ "less_relevant",
        (red_meat_inclusion == 1 & daly_inclusion == 0) ~ "less_relevant",
        (red_meat_inclusion == 0 & daly_inclusion == 1) ~ "less_relevant",
        TRUE ~ "missing_data"
      )
    ) %>%
    mutate(
      policy_3 = case_when(
        (ruminant_meat_inclusion == 1 &
          availability_inclusion == 1) ~ "highly_relevant",
        (ruminant_meat_inclusion == 1 &
          availability_inclusion != 1) ~ "relevant",
        (ruminant_meat_inclusion == 0) ~ "less_relevant",
        TRUE ~ "missing_data"
      )
    ) %>%
    mutate(
      policy_4 = case_when(
        ((jobs_inclusion == 1 |
          export_inclusion == 1 |
          consump_inclusion == 1) &
          climate_inclusion == 1) ~ "highly_relevant",
        ((jobs_inclusion == 1 |
          export_inclusion == 1 |
          consump_inclusion == 1) &
          climate_inclusion == 0) ~ "relevant",
        (jobs_inclusion == 0 &
          export_inclusion == 0 &
          consump_inclusion == 0) ~ "less_relevant",
        TRUE ~ "missing_data"
      )
    )

  return(policy_data)
}


#***************************************************************************************
#Plotting each inclusion variable
#Note - this is for further investigation of each variable, not included in paper.
plotting_inclusions <- function(policy_data) {
  world <- ne_countries(scale = "medium", returnclass = "sf")

  inclusion_world <- world %>%
    select(iso_a3, geometry) %>%
    rename(iso3c = iso_a3) %>%
    left_join(policy_data) %>%
    mutate_at(vars(SEV_omega3_inclusion:nutrient_inclusion), as.factor)

  cols <- c("0" = "red", "1" = "green", "NA" = "grey")

  #Variable maps - Change names
  plot <- ggplot(data = inclusion_world) +
    geom_sf(aes(fill = climate_inclusion)) +
    scale_fill_manual(
      values = cols,
      name = "Climate Hazard inclusion",
      labels = c("Low - not included", "High - included", "Missing data")
    ) +
    theme(legend.position = "top")

  return(plot)
}


#**************************************************************************************
#Summarising policy recommendations

summarising_policy <- function(policy_recommendation_data) {
  policy_summary <- policy_recommendation_data %>%
    mutate(
      country_name = countrycode(
        iso3c,
        origin = "iso3c",
        destination = "country.name"
      )
    ) %>%
    select(
      iso3c,
      country_name,
      policy_0,
      policy_1,
      policy_2,
      policy_3,
      policy_4
    ) %>%
    unique()

  return(policy_summary)
}
#***************************************************************************************
#Calculating policy salience
calc_policy_salience <- function(policy_summary) {
  salience_data <- policy_summary %>%
    mutate(
      overal_relevant_policy0 = as.factor(case_when(
        (policy_0 == "highly_relevant" | policy_0 == "relevant") ~ "relevant",
        TRUE ~ policy_0
      ))
    ) %>%
    mutate(
      overal_relevant_policy1 = as.factor(case_when(
        (policy_1 == "highly_relevant" | policy_1 == "relevant") ~ "relevant",
        TRUE ~ policy_1
      ))
    ) %>%
    mutate(
      overal_relevant_policy2 = as.factor(case_when(
        (policy_2 == "highly_relevant" | policy_2 == "relevant") ~ "relevant",
        TRUE ~ policy_2
      ))
    ) %>%
    mutate(
      overal_relevant_policy3 = as.factor(case_when(
        (policy_3 == "highly_relevant" | policy_3 == "relevant") ~ "relevant",
        TRUE ~ policy_3
      ))
    ) %>%
    mutate(
      overal_relevant_policy4 = as.factor(case_when(
        (policy_4 == "highly_relevant" | policy_4 == "relevant") ~ "relevant",
        TRUE ~ policy_4
      ))
    ) %>%
    mutate(
      policy0_policy0 = case_when(
        overal_relevant_policy0 == "relevant" ~ 1,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      policy0_policy1 = case_when(
        (overal_relevant_policy0 == "relevant" &
          overal_relevant_policy1 == "relevant") ~ 1,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      policy0_policy2 = case_when(
        (overal_relevant_policy0 == "relevant" &
          overal_relevant_policy2 == "relevant") ~ 1,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      policy0_policy3 = case_when(
        (overal_relevant_policy0 == "relevant" &
          overal_relevant_policy3 == "relevant") ~ 1,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      policy0_policy4 = case_when(
        (overal_relevant_policy0 == "relevant" &
          overal_relevant_policy4 == "relevant") ~ 1,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      policy1_policy1 = case_when(
        overal_relevant_policy1 == "relevant" ~ 1,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      policy1_policy0 = case_when(
        (overal_relevant_policy1 == "relevant" &
          overal_relevant_policy0 == "relevant") ~ 1,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      policy1_policy2 = case_when(
        (overal_relevant_policy1 == "relevant" &
          overal_relevant_policy2 == "relevant") ~ 1,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      policy1_policy3 = case_when(
        (overal_relevant_policy1 == "relevant" &
          overal_relevant_policy3 == "relevant") ~ 1,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      policy1_policy4 = case_when(
        (overal_relevant_policy1 == "relevant" &
          overal_relevant_policy4 == "relevant") ~ 1,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      policy2_policy2 = case_when(
        overal_relevant_policy2 == "relevant" ~ 1,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      policy2_policy0 = case_when(
        (overal_relevant_policy2 == "relevant" &
          overal_relevant_policy0 == "relevant") ~ 1,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      policy2_policy1 = case_when(
        (overal_relevant_policy2 == "relevant" &
          overal_relevant_policy1 == "relevant") ~ 1,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      policy2_policy3 = case_when(
        (overal_relevant_policy2 == "relevant" &
          overal_relevant_policy3 == "relevant") ~ 1,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      policy2_policy4 = case_when(
        (overal_relevant_policy2 == "relevant" &
          overal_relevant_policy4 == "relevant") ~ 1,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      policy3_policy3 = case_when(
        overal_relevant_policy3 == "relevant" ~ 1,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      policy3_policy0 = case_when(
        (overal_relevant_policy3 == "relevant" &
          overal_relevant_policy0 == "relevant") ~ 1,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      policy3_policy1 = case_when(
        (overal_relevant_policy3 == "relevant" &
          overal_relevant_policy1 == "relevant") ~ 1,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      policy3_policy2 = case_when(
        (overal_relevant_policy3 == "relevant" &
          overal_relevant_policy2 == "relevant") ~ 1,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      policy3_policy4 = case_when(
        (overal_relevant_policy3 == "relevant" &
          overal_relevant_policy4 == "relevant") ~ 1,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      policy4_policy4 = case_when(
        overal_relevant_policy4 == "relevant" ~ 1,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      policy4_policy0 = case_when(
        (overal_relevant_policy4 == "relevant" &
          overal_relevant_policy0 == "relevant") ~ 1,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      policy4_policy1 = case_when(
        (overal_relevant_policy4 == "relevant" &
          overal_relevant_policy1 == "relevant") ~ 1,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      policy4_policy2 = case_when(
        (overal_relevant_policy4 == "relevant" &
          overal_relevant_policy2 == "relevant") ~ 1,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      policy4_policy3 = case_when(
        (overal_relevant_policy4 == "relevant" &
          overal_relevant_policy3 == "relevant") ~ 1,
        TRUE ~ 0
      )
    )

  salience_sum <- salience_data %>%
    select(policy0_policy0:policy4_policy3) %>%
    ungroup() %>%
    summarise_all(list(sum))

  policy0_salience <- salience_sum %>%
    select(policy0_policy0:policy0_policy4) %>%
    pivot_longer(
      policy0_policy0:policy0_policy4,
      names_to = "type",
      names_prefix = "policy0_",
      values_to = "total"
    ) %>%
    mutate(policy0 = round(100 * (total / max(total)))) %>%
    rename(policy0_num = total)

  policy1_salience <- salience_sum %>%
    select(policy1_policy1:policy1_policy4) %>%
    pivot_longer(
      policy1_policy1:policy1_policy4,
      names_to = "type",
      names_prefix = "policy1_",
      values_to = "total"
    ) %>%
    mutate(policy1 = round(100 * (total / max(total)))) %>%
    rename(policy1_num = total)

  policy2_salience <- salience_sum %>%
    select(policy2_policy2:policy2_policy4) %>%
    pivot_longer(
      policy2_policy2:policy2_policy4,
      names_to = "type",
      names_prefix = "policy2_",
      values_to = "total"
    ) %>%
    mutate(policy2 = round(100 * (total / max(total)))) %>%
    rename(policy2_num = total)

  policy3_salience <- salience_sum %>%
    select(policy3_policy3:policy3_policy4) %>%
    pivot_longer(
      policy3_policy3:policy3_policy4,
      names_to = "type",
      names_prefix = "policy3_",
      values_to = "total"
    ) %>%
    mutate(policy3 = round(100 * (total / max(total)))) %>%
    rename(policy3_num = total)

  policy4_salience <- salience_sum %>%
    select(policy4_policy4:policy4_policy3) %>%
    pivot_longer(
      policy4_policy4:policy4_policy3,
      names_to = "type",
      names_prefix = "policy4_",
      values_to = "total"
    ) %>%
    mutate(policy4 = round(100 * (total / max(total)))) %>%
    rename(policy4_num = total)

  salience_percentage <- policy0_salience %>%
    left_join(policy1_salience) %>%
    left_join(policy2_salience) %>%
    left_join(policy3_salience) %>%
    left_join(policy4_salience)

  #writexl::write_xlsx(salience_percentage,"salience_percentage.xlsx")

  plot_data <- salience_percentage %>%
    mutate(
      type = case_when(
        type == "policy0" ~ "policy Aa",
        type == "policy1" ~ "policy Ab",
        type == "policy2" ~ "policy B",
        type == "policy3" ~ "policy C",
        type == "policy4" ~ "policy D"
      )
    ) %>%
    mutate(type = as.factor(type)) %>%
    mutate(type = fct_rev(type)) %>%
    mutate(text_n = "(n = ") %>%
    mutate(percent = "%") %>%
    mutate(end_bracket = ")") %>%
    unite(
      "policy0_text",
      c("policy0", "percent", "text_n", "policy0_num"),
      sep = " ",
      remove = F
    ) %>%
    unite(
      "policy1_text",
      c("policy1", "percent", "text_n", "policy1_num"),
      sep = " ",
      remove = F
    ) %>%
    unite(
      "policy2_text",
      c("policy2", "percent", "text_n", "policy2_num"),
      sep = " ",
      remove = F
    ) %>%
    unite(
      "policy3_text",
      c("policy3", "percent", "text_n", "policy3_num"),
      sep = " ",
      remove = F
    ) %>%
    unite(
      "policy4_text",
      c("policy4", "percent", "text_n", "policy4_num"),
      sep = " ",
      remove = F
    ) %>%
    unite(
      "policy0_text2",
      c("policy0_text", "end_bracket"),
      sep = "",
      remove = F
    ) %>%
    unite(
      "policy1_text2",
      c("policy1_text", "end_bracket"),
      sep = "",
      remove = F
    ) %>%
    unite(
      "policy2_text2",
      c("policy2_text", "end_bracket"),
      sep = "",
      remove = F
    ) %>%
    unite(
      "policy3_text2",
      c("policy3_text", "end_bracket"),
      sep = "",
      remove = F
    ) %>%
    unite(
      "policy4_text2",
      c("policy4_text", "end_bracket"),
      sep = "",
      remove = F
    ) %>%
    unite(
      "policy0_full",
      c("policy0_text2", "policy0"),
      sep = "_",
      remove = T
    ) %>%
    unite(
      "policy1_full",
      c("policy1_text2", "policy1"),
      sep = "_",
      remove = T
    ) %>%
    unite(
      "policy2_full",
      c("policy2_text2", "policy2"),
      sep = "_",
      remove = T
    ) %>%
    unite(
      "policy3_full",
      c("policy3_text2", "policy3"),
      sep = "_",
      remove = T
    ) %>%
    unite(
      "policy4_full",
      c("policy4_text2", "policy4"),
      sep = "_",
      remove = T
    ) %>%
    select(
      !c(
        policy0_num,
        policy1_num,
        policy2_num,
        policy3_num,
        policy4_num,
        text_n,
        percent,
        end_bracket,
        policy0_text,
        policy1_text,
        policy2_text,
        policy3_text,
        policy4_text
      )
    ) %>%
    pivot_longer(
      cols = policy0_full:policy4_full,
      names_to = c("policy", "remove"),
      names_pattern = "(.*)_(.*)",
      values_to = "full"
    ) %>%
    separate(full, c("text", "percentage"), remove = F, sep = "_") %>%
    mutate(percentage = as.numeric(percentage)) %>%
    mutate(
      policy = case_when(
        policy == "policy0" ~ "policy Aa",
        policy == "policy1" ~ "policy Ab",
        policy == "policy2" ~ "policy B",
        policy == "policy3" ~ "policy C",
        policy == "policy4" ~ "policy D"
      )
    ) %>%
    mutate(policy = as.factor(policy))

  salience_plot <- ggplot(plot_data, aes(policy, type)) +
    geom_raster(aes(fill = percentage)) +
    coord_fixed(ratio = 0.5) +
    scale_fill_gradient(low = "white", high = "grey", limits = c(0, 100)) +
    geom_text(aes(label = text), size = 6) +
    scale_x_discrete(position = "top", name = "") +
    ylab("") +
    labs(fill = "Overlap (%)")

  #Go from this ugly plot to nicer in Inkscape
  ggsave(salience_plot, file = "./Plots/salience_rough.svg")

  return(salience_percentage)
}

#***************************************************************************************
#Plotting policy recommendation on global maps

map_plot <- function(policy_summary) {
  # Deactivate s2
  sf::sf_use_s2(FALSE)

  world <- ne_countries(scale = "medium", returnclass = "sf")

  BF_world <- world %>%
    select(iso_a3, geometry) %>%
    rename(iso3c = iso_a3) %>%
    left_join(policy_summary) %>%
    mutate_at(
      vars(policy_0, policy_1, policy_2, policy_3, policy_4),
      as.factor
    ) %>%
    mutate(
      policy_0 = fct_relevel(
        policy_0,
        "highly_relevant",
        "relevant",
        "less_relevant",
        "missing_data"
      )
    ) %>%
    mutate(
      policy_1 = fct_relevel(
        policy_1,
        "highly_relevant",
        "relevant",
        "less_relevant",
        "missing_data"
      )
    ) %>%
    mutate(
      policy_2 = fct_relevel(
        policy_2,
        "highly_relevant",
        "relevant",
        "less_relevant",
        "missing_data"
      )
    ) %>%
    mutate(
      policy_3 = fct_relevel(
        policy_3,
        "highly_relevant",
        "relevant",
        "less_relevant",
        "missing_data"
      )
    ) %>%
    mutate(
      policy_4 = fct_relevel(
        policy_4,
        "highly_relevant",
        "relevant",
        "less_relevant",
        "missing_data"
      )
    ) %>%
    filter(!is.na(policy_1))

  cols <- c(
    "highly_relevant" = "#012998",
    "relevant" = "#205BFE",
    "less_relevant" = "#AEC3FF",
    "missing_data" = "grey"
  )

  map0 <- ggplot(data = BF_world) +
    geom_sf(aes(fill = policy_0)) +
    scale_fill_manual(
      values = cols,
      labels = c("Highly relevant", "Relevant", "Less relevant", "Missing data")
    ) +
    theme(legend.position = "top") +
    labs(fill = "") + #For facet
    theme(
      legend.key.size = unit(0.2, 'cm'), #change legend key size
      legend.key.height = unit(0.2, 'cm'), #change legend key height
      legend.key.width = unit(0.2, 'cm'), #change legend key width
      legend.text = element_text(size = 8)
    ) #change legend text font size
  #labs(fill = "Reducing nutrient deficiencies")

  map1 <- ggplot(data = BF_world) +
    geom_sf(aes(fill = policy_1)) +
    scale_fill_manual(
      values = cols,
      labels = c("Highly relevant", "Relevant", "Less relevant", "Missing data")
    ) +
    theme(legend.position = "top") +
    labs(fill = "") + #For facet
    theme(
      legend.key.size = unit(0.2, 'cm'), #change legend key size
      legend.key.height = unit(0.2, 'cm'), #change legend key height
      legend.key.width = unit(0.2, 'cm'), #change legend key width
      legend.text = element_text(size = 8)
    ) #change legend text font size
  #labs(fill = "Reducing nutrient deficiencies")

  map2 <- ggplot(data = BF_world) +
    geom_sf(aes(fill = policy_2)) +
    scale_fill_manual(
      values = cols,
      labels = c("Highly relevant", "Relevant", "Less relevant", "Missing data")
    ) +
    theme(legend.position = "top") +
    labs(fill = "") + #For facet
    theme(
      legend.key.size = unit(0.2, 'cm'), #change legend key size
      legend.key.height = unit(0.2, 'cm'), #change legend key height
      legend.key.width = unit(0.2, 'cm'), #change legend key width
      legend.text = element_text(size = 8)
    ) #change legend text font size
  #labs(fill = "Reducing cardiovascular disease")

  map3 <- ggplot(data = BF_world) +
    geom_sf(aes(fill = policy_3)) +
    scale_fill_manual(
      values = cols,
      labels = c("Highly relevant", "Relevant", "Less relevant", "Missing data")
    ) +
    theme(legend.position = "top") +
    labs(fill = "") + #For facet
    theme(
      legend.key.size = unit(0.2, 'cm'), #change legend key size
      legend.key.height = unit(0.2, 'cm'), #change legend key height
      legend.key.width = unit(0.2, 'cm'), #change legend key width
      legend.text = element_text(size = 8)
    ) #change legend text font size
  #labs(fill = "Reducing environmental footprints of food consumption and production")

  map4 <- ggplot(data = BF_world) +
    geom_sf(aes(fill = policy_4)) +
    scale_fill_manual(
      values = cols,
      labels = c("Highly relevant", "Relevant", "Less relevant", "Missing data")
    ) +
    theme(legend.position = "top") +
    labs(fill = "") + #For facet
    theme(
      legend.key.size = unit(0.2, 'cm'), #change legend key size
      legend.key.height = unit(0.2, 'cm'), #change legend key height
      legend.key.width = unit(0.2, 'cm'), #change legend key width
      legend.text = element_text(size = 8)
    ) #change legend text font size
  #labs(fill = "Safeguarding food system contributions to climate change")

  facet_labels <- c(
    "Aa: Reducing B12 deficiencies",
    "B: Reducing cardiovascular disease",
    "Ab: Reducing omega3 deficiencies",
    "C: Reducing environmental footprints",
    " ",
    "D: Safeguarding food system contributions"
  )

  facet_map <- plot_grid(
    map0,
    map2,
    map1,
    map3,
    NULL,
    map4,
    ncol = 2,
    labels = facet_labels,
    hjust = 0,
    label_x = 0.01
  )

  #Rework layout in Inkscape to more symmetric

  #ggsave(facet_map,file="./Plots/map_plot.png",dpi=300, height = 18,width = 18,units = "cm")
  ggsave(
    facet_map,
    file = "./Plots/map_plot.svg",
    dpi = 300,
    height = 24,
    width = 18,
    units = "cm"
  )

  return(facet_map)
}
#____________________________________________________________________
#Sensitivity analysis
#____________________________________________________________________

#Inclusion Cut-offs for each variable
#************************************
#SEV_omega3 H -10
#SEV_vitB12  H - 10
#fish_relative_caloric_price - 1.75
#bf_availability = prod_kgpercap_year + import_kg_percap_year - 8 (50% of global without China)
#red_meat_gcapday -50
#ruminant_meat_gcapday- 7
#daly_cardiovascular_cap - 0.05
#mean_total_production - 1 000 000
#total_GHG - 1500000000
#totjobs_percap - 0.01
#export_percgdp - 0.03
#prop_aquatic_omega3 - 0.2
#prop_aquatic_B12 - 0.2
#ssp585_2050 - 50
#aq_reliance_ratio - 0.2
#***********************************

#12 different classification functions ----
#0.1 - policy 0
make_classification_b12_p0 <- function(value, clean_data) {
  data <- clean_data %>%
    mutate(
      bf_availability_kgcap_year = prod_kgpercap_year + import_kg_percap_year
    ) %>%
    mutate(
      SEV_B12_inclusion = case_when(
        (SEV_vitB12 >= value) ~ 1,
        (SEV_vitB12 < value) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      availability_inclusion = case_when(
        (bf_availability_kgcap_year >= 8) ~ 1,
        (bf_availability_kgcap_year < 8) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    rowwise() %>%
    mutate(
      policy_0 = as.factor(case_when(
        (SEV_B12_inclusion == 1 &
          availability_inclusion == 1) ~ "highly_relevant",
        (SEV_B12_inclusion == 1 & availability_inclusion != 1) ~ "relevant",
        (SEV_B12_inclusion == 0) ~ "less_relevant",
        TRUE ~ "missing_data"
      ))
    ) %>%
    select(policy_0) %>%
    rename_with(.fn = ~ paste0("value_", value), .cols = policy_0)
}
#***************************************************
#0.2 - policy 0
make_classification_bf_p0 <- function(value, clean_data) {
  data <- clean_data %>%
    mutate(
      bf_availability_kgcap_year = prod_kgpercap_year + import_kg_percap_year
    ) %>%
    mutate(
      SEV_B12_inclusion = case_when(
        (SEV_vitB12 >= 10) ~ 1,
        (SEV_vitB12 < 10) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      availability_inclusion = case_when(
        (bf_availability_kgcap_year >= value) ~ 1,
        (bf_availability_kgcap_year < value) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    rowwise() %>%
    mutate(
      policy_0 = as.factor(case_when(
        (SEV_B12_inclusion == 1 &
          availability_inclusion == 1) ~ "highly_relevant",
        (SEV_B12_inclusion == 1 & availability_inclusion != 1) ~ "relevant",
        (SEV_B12_inclusion == 0) ~ "less_relevant",
        TRUE ~ "missing_data"
      ))
    ) %>%
    select(policy_0) %>%
    rename_with(.fn = ~ paste0("value_", value), .cols = policy_0)
}

#1 - policy 1
make_classification_o3_p1 <- function(value, clean_data) {
  data <- clean_data %>%
    mutate(
      bf_availability_kgcap_year = prod_kgpercap_year + import_kg_percap_year
    ) %>%
    mutate(
      SEV_omega3_inclusion = case_when(
        (SEV_omega3 >= value) ~ 1,
        (SEV_omega3 < value) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      availability_inclusion = case_when(
        (bf_availability_kgcap_year >= 8) ~ 1,
        (bf_availability_kgcap_year < 8) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    rowwise() %>%
    mutate(
      policy_1 = as.factor(case_when(
        (SEV_omega3_inclusion == 1 &
          availability_inclusion == 1) ~ "highly_relevant",
        (SEV_omega3_inclusion == 1 & availability_inclusion != 1) ~ "relevant",
        (SEV_omega3_inclusion == 0) ~ "less_relevant",
        TRUE ~ "missing_data"
      ))
    ) %>%
    select(policy_1) %>%
    rename_with(.fn = ~ paste0("value_", value), .cols = policy_1)
}
#**
#3 - policy 1
make_classification_bf_p1 <- function(value, clean_data) {
  data <- clean_data %>%
    mutate(
      bf_availability_kgcap_year = prod_kgpercap_year + import_kg_percap_year
    ) %>%
    mutate(
      SEV_omega3_inclusion = case_when(
        (SEV_omega3 >= 10) ~ 1,
        (SEV_omega3 < 10) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      availability_inclusion = case_when(
        (bf_availability_kgcap_year >= value) ~ 1,
        (bf_availability_kgcap_year < value) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    rowwise() %>%
    mutate(
      policy_1 = as.factor(case_when(
        (SEV_omega3_inclusion == 1 &
          availability_inclusion == 1) ~ "highly_relevant",
        (SEV_omega3_inclusion == 1 & availability_inclusion != 1) ~ "relevant",
        (SEV_omega3_inclusion == 0) ~ "less_relevant",
        TRUE ~ "missing_data"
      ))
    ) %>%
    select(policy_1) %>%
    rename_with(.fn = ~ paste0("value_", value), .cols = policy_1)
}

#**
#4 - policy 2
make_classification_redmeat <- function(value, clean_data) {
  data <- clean_data %>%
    mutate(
      bf_availability_kgcap_year = prod_kgpercap_year + import_kg_percap_year
    ) %>%
    mutate(
      availability_inclusion = case_when(
        (bf_availability_kgcap_year >= 8) ~ 1,
        (bf_availability_kgcap_year < 8) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      red_meat_inclusion = case_when(
        (red_meat_gcapday >= value) ~ 1,
        (red_meat_gcapday < value) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      daly_inclusion = case_when(
        (DALY_cardiovascular_cap >= 0.05) ~ 1,
        (DALY_cardiovascular_cap < 0.05) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    rowwise() %>%
    mutate(
      policy_2 = as.factor(case_when(
        (red_meat_inclusion == 1 &
          daly_inclusion == 1 &
          availability_inclusion == 1) ~ "highly_relevant",
        (red_meat_inclusion == 1 &
          daly_inclusion == 1 &
          availability_inclusion != 1) ~ "relevant",
        (red_meat_inclusion == 0 & daly_inclusion == 0) ~ "less_relevant",
        (red_meat_inclusion == 1 & daly_inclusion == 0) ~ "less_relevant",
        (red_meat_inclusion == 0 & daly_inclusion == 1) ~ "less_relevant",
        TRUE ~ "missing_data"
      ))
    ) %>%
    select(policy_2) %>%
    rename_with(.fn = ~ paste0("value_", value), .cols = policy_2)
}
#*************************************************
#5 - policy 2
make_classification_daly <- function(value, clean_data) {
  data <- clean_data %>%
    mutate(
      bf_availability_kgcap_year = prod_kgpercap_year + import_kg_percap_year
    ) %>%
    mutate(
      availability_inclusion = case_when(
        (bf_availability_kgcap_year >= 8) ~ 1,
        (bf_availability_kgcap_year < 8) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      red_meat_inclusion = case_when(
        (red_meat_gcapday >= 50) ~ 1,
        (red_meat_gcapday < 50) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      daly_inclusion = case_when(
        (DALY_cardiovascular_cap >= value) ~ 1,
        (DALY_cardiovascular_cap < value) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    rowwise() %>%
    mutate(
      policy_2 = as.factor(case_when(
        (red_meat_inclusion == 1 &
          daly_inclusion == 1 &
          availability_inclusion == 1) ~ "highly_relevant",
        (red_meat_inclusion == 1 &
          daly_inclusion == 1 &
          availability_inclusion != 1) ~ "relevant",
        (red_meat_inclusion == 0 & daly_inclusion == 0) ~ "less_relevant",
        (red_meat_inclusion == 1 & daly_inclusion == 0) ~ "less_relevant",
        (red_meat_inclusion == 0 & daly_inclusion == 1) ~ "less_relevant",
        TRUE ~ "missing_data"
      ))
    ) %>%
    select(policy_2) %>%
    rename_with(.fn = ~ paste0("value_", value), .cols = policy_2)
}
#**
#6 - policy 2
make_classification_bf_p2 <- function(value, clean_data) {
  data <- clean_data %>%
    mutate(
      bf_availability_kgcap_year = prod_kgpercap_year + import_kg_percap_year
    ) %>%
    mutate(
      availability_inclusion = case_when(
        (bf_availability_kgcap_year >= value) ~ 1,
        (bf_availability_kgcap_year < value) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      red_meat_inclusion = case_when(
        (red_meat_gcapday >= 50) ~ 1,
        (red_meat_gcapday < 50) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      daly_inclusion = case_when(
        (DALY_cardiovascular_cap >= 0.05) ~ 1,
        (DALY_cardiovascular_cap < 0.05) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    rowwise() %>%
    mutate(
      policy_2 = as.factor(case_when(
        (red_meat_inclusion == 1 &
          daly_inclusion == 1 &
          availability_inclusion == 1) ~ "highly_relevant",
        (red_meat_inclusion == 1 &
          daly_inclusion == 1 &
          availability_inclusion != 1) ~ "relevant",
        (red_meat_inclusion == 0 & daly_inclusion == 0) ~ "less_relevant",
        (red_meat_inclusion == 1 & daly_inclusion == 0) ~ "less_relevant",
        (red_meat_inclusion == 0 & daly_inclusion == 1) ~ "less_relevant",
        TRUE ~ "missing_data"
      ))
    ) %>%
    select(policy_2) %>%
    rename_with(.fn = ~ paste0("value_", value), .cols = policy_2)
}

#**
#7 - policy 3
make_classification_rummeat <- function(value, clean_data) {
  data <- clean_data %>%
    mutate(
      bf_availability_kgcap_year = prod_kgpercap_year + import_kg_percap_year
    ) %>%
    mutate(
      availability_inclusion = case_when(
        (bf_availability_kgcap_year >= 8) ~ 1,
        (bf_availability_kgcap_year < 8) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      ruminant_meat_inclusion = case_when(
        (ruminant_meat_gcapday >= value) ~ 1,
        (ruminant_meat_gcapday < value) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    rowwise() %>%
    mutate(
      policy_3 = case_when(
        (ruminant_meat_inclusion == 1 &
          availability_inclusion == 1) ~ "highly_relevant",
        (ruminant_meat_inclusion == 1 &
          availability_inclusion != 1) ~ "relevant",
        (ruminant_meat_inclusion == 0) ~ "less_relevant",
        TRUE ~ "missing_data"
      )
    ) %>%
    select(policy_3) %>%
    rename_with(.fn = ~ paste0("value_", value), .cols = policy_3)
}
#**
#8 - policy 3
make_classification_bf_p3 <- function(value, clean_data) {
  data <- clean_data %>%
    mutate(
      bf_availability_kgcap_year = prod_kgpercap_year + import_kg_percap_year
    ) %>%
    mutate(
      availability_inclusion = case_when(
        (bf_availability_kgcap_year >= value) ~ 1,
        (bf_availability_kgcap_year < value) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      ruminant_meat_inclusion = case_when(
        (ruminant_meat_gcapday >= 7) ~ 1,
        (ruminant_meat_gcapday < 7) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    rowwise() %>%
    mutate(
      policy_3 = case_when(
        (ruminant_meat_inclusion == 1 &
          availability_inclusion == 1) ~ "highly_relevant",
        (ruminant_meat_inclusion == 1 &
          availability_inclusion != 1) ~ "relevant",
        (ruminant_meat_inclusion == 0) ~ "less_relevant",
        TRUE ~ "missing_data"
      )
    ) %>%
    select(policy_3) %>%
    rename_with(.fn = ~ paste0("value_", value), .cols = policy_3)
}
#**
#9 - policy 4
make_classification_jobs <- function(value, clean_data) {
  data <- clean_data %>%
    mutate(
      jobs_inclusion = case_when(
        (totjobs_percap >= value) ~ 1,
        (totjobs_percap < value) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      export_inclusion = case_when(
        (export_percgdp >= 0.03) ~ 1,
        (export_percgdp < 0.03) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      consump_inclusion = case_when(
        (aq_reliance_ratio >= 0.2) ~ 1,
        (aq_reliance_ratio < 0.2) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      climate_inclusion = case_when(
        (ssp585_2050 >= 50) ~ 1,
        (ssp585_2050 < 50) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    rowwise() %>%
    mutate(
      policy_4 = as.factor(case_when(
        ((jobs_inclusion == 1 |
          export_inclusion == 1 |
          consump_inclusion == 1) &
          climate_inclusion == 1) ~ "highly_relevant",
        ((jobs_inclusion == 1 |
          export_inclusion == 1 |
          consump_inclusion == 1) &
          climate_inclusion == 0) ~ "relevant",
        (jobs_inclusion == 0 &
          export_inclusion == 0 &
          consump_inclusion == 0) ~ "less_relevant",
        TRUE ~ "missing_data"
      ))
    ) %>%
    select(policy_4) %>%
    rename_with(.fn = ~ paste0("value_", value), .cols = policy_4)
}

#**
#10 - policy 4
make_classification_export <- function(value, clean_data) {
  data <- clean_data %>%
    mutate(
      jobs_inclusion = case_when(
        (totjobs_percap >= 0.01) ~ 1,
        (totjobs_percap < 0.01) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      export_inclusion = case_when(
        (export_percgdp >= value) ~ 1,
        (export_percgdp < value) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      consump_inclusion = case_when(
        (aq_reliance_ratio >= 0.2) ~ 1,
        (aq_reliance_ratio < 0.2) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      climate_inclusion = case_when(
        (ssp585_2050 >= 50) ~ 1,
        (ssp585_2050 < 50) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    rowwise() %>%
    mutate(
      policy_4 = as.factor(case_when(
        ((jobs_inclusion == 1 |
          export_inclusion == 1 |
          consump_inclusion == 1) &
          climate_inclusion == 1) ~ "highly_relevant",
        ((jobs_inclusion == 1 |
          export_inclusion == 1 |
          consump_inclusion == 1) &
          climate_inclusion == 0) ~ "relevant",
        (jobs_inclusion == 0 &
          export_inclusion == 0 &
          consump_inclusion == 0) ~ "less_relevant",
        TRUE ~ "missing_data"
      ))
    ) %>%
    select(policy_4) %>%
    rename_with(.fn = ~ paste0("value_", value), .cols = policy_4)
}
#**
#11 - policy 4
make_classification_consump <- function(value, clean_data) {
  data <- clean_data %>%
    mutate(
      jobs_inclusion = case_when(
        (totjobs_percap >= 0.01) ~ 1,
        (totjobs_percap < 0.01) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      export_inclusion = case_when(
        (export_percgdp >= 0.03) ~ 1,
        (export_percgdp < 0.03) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      consump_inclusion = case_when(
        (aq_reliance_ratio >= value) ~ 1,
        (aq_reliance_ratio < value) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      climate_inclusion = case_when(
        (ssp585_2050 >= 50) ~ 1,
        (ssp585_2050 < 50) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    rowwise() %>%
    mutate(
      policy_4 = as.factor(case_when(
        ((jobs_inclusion == 1 |
          export_inclusion == 1 |
          consump_inclusion == 1) &
          climate_inclusion == 1) ~ "highly_relevant",
        ((jobs_inclusion == 1 |
          export_inclusion == 1 |
          consump_inclusion == 1) &
          climate_inclusion == 0) ~ "relevant",
        (jobs_inclusion == 0 &
          export_inclusion == 0 &
          consump_inclusion == 0) ~ "less_relevant",
        TRUE ~ "missing_data"
      ))
    ) %>%
    select(policy_4) %>%
    rename_with(.fn = ~ paste0("value_", value), .cols = policy_4)
}

#**
#12 - policy 4
make_classification_climate <- function(value, clean_data) {
  data <- clean_data %>%
    mutate(
      jobs_inclusion = case_when(
        (totjobs_percap >= 0.01) ~ 1,
        (totjobs_percap < 0.01) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      export_inclusion = case_when(
        (export_percgdp >= 0.03) ~ 1,
        (export_percgdp < 0.03) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      consump_inclusion = case_when(
        (aq_reliance_ratio >= 0.2) ~ 1,
        (aq_reliance_ratio < 0.2) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      climate_inclusion = case_when(
        (ssp585_2050 >= value) ~ 1,
        (ssp585_2050 < value) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    rowwise() %>%
    mutate(
      policy_4 = as.factor(case_when(
        ((jobs_inclusion == 1 |
          export_inclusion == 1 |
          consump_inclusion == 1) &
          climate_inclusion == 1) ~ "highly_relevant",
        ((jobs_inclusion == 1 |
          export_inclusion == 1 |
          consump_inclusion == 1) &
          climate_inclusion == 0) ~ "relevant",
        (jobs_inclusion == 0 &
          export_inclusion == 0 &
          consump_inclusion == 0) ~ "less_relevant",
        TRUE ~ "missing_data"
      ))
    ) %>%
    select(policy_4) %>%
    rename_with(.fn = ~ paste0("value_", value), .cols = policy_4)
}
#*************************************************************
get_iso <- function(clean_data) {
  iso3c <- clean_data$iso3c
  return(iso3c)
}

#*************************************************************
#Make sensitivity data function -----

make_sensitivity_data <- function(
  range,
  classification_function,
  clean_data,
  iso3c
) {
  sens_data <- map_dfc(
    .x = range,
    .f = ~ classification_function(.x, clean_data)
  ) %>%
    cbind(iso3c)

  return(sens_data)
}
#*************************************************************

#Make barplot data function ----

make_barplot_data <- function(sensitivity_data) {
  barplot_data <- sensitivity_data %>%
    pivot_longer(!iso3c, names_to = "value", values_to = "outcome") %>%
    group_by(value, outcome) %>%
    count() %>%
    mutate(value_number = as.numeric(str_remove(value, "\\D+"))) %>%
    filter(outcome != "missing_data") %>%
    droplevels()
  return(barplot_data)
}
#*************************************************************
#Run sensitivity plots -----
#Make policy 0 sensitivity plot outputs

make_policy0_sensitivity <- function(b12_barplot_data, bf_p0_barplot_data) {
  cols <- c(
    "highly_relevant" = "#012998",
    "relevant" = "#205BFE",
    "less_relevant" = "#AEC3FF"
  )

  b12_plot <- ggplot(
    b12_barplot_data,
    aes(fill = outcome, y = n, x = value_number)
  ) +
    geom_bar(position = "stack", stat = "identity") +
    theme_classic() +
    scale_y_continuous(n.breaks = 7, name = "No. of countries") +
    scale_x_continuous(n.breaks = 8, name = "Vitamin B12 threshold values") +
    scale_fill_manual(values = cols) +
    theme(legend.position = "top") +
    geom_vline(aes(xintercept = 10, col = "red")) #Ordinary cut-off - need to specify

  bf_p0_plot <- ggplot(
    bf_p0_barplot_data,
    aes(fill = outcome, y = n, x = value_number)
  ) +
    geom_bar(position = "stack", stat = "identity") +
    theme_classic() +
    scale_y_continuous(n.breaks = 7, name = "No. of countries") +
    scale_x_continuous(n.breaks = 9, name = "Blue Foods threshold values") +
    scale_fill_manual(values = cols) +
    theme(legend.position = "top") +
    geom_vline(aes(xintercept = 8, col = "red")) #Ordinary cut-off - need to specify

  bf_p0_plot_short <- ggplot(
    bf_p0_barplot_data,
    aes(fill = outcome, y = n, x = value_number)
  ) +
    geom_bar(position = "stack", stat = "identity") +
    theme_classic() +
    scale_y_continuous(n.breaks = 7, name = "No. of countries") +
    scale_x_continuous(
      n.breaks = 10,
      name = "Blue Foods threshold values",
      limits = c(0, 250)
    ) +
    scale_fill_manual(values = cols) +
    theme(legend.position = "top") +
    geom_vline(aes(xintercept = 8, col = "red")) #Ordinary cut-off - need to specify

  policy0_plots <- plot_grid(
    b12_plot,
    bf_p0_plot,
    bf_p0_plot_short,
    ncol = 2,
    labels = c("Vitamin B12", "Blue Foods - full", "Blue Foods - cropped"),
    hjust = 0,
    label_x = 0.01
  )

  ggsave(
    policy0_plots,
    file = "./Plots/sensitivity_plots/policy0_sensitivity.png",
    dpi = 300,
    height = 18.3,
    width = 18.3,
    units = "cm"
  )
  ggsave(
    policy0_plots,
    file = "./Plots/sensitivity_plots/policy0_sensitivity.svg",
    dpi = 300,
    height = 18.3,
    width = 18.3,
    units = "cm"
  )

  return(policy0_plots)
}
#*************************************************************

#Make policy 1 sensitivity plot outputs

make_policy1_sensitivity <- function(omega3_barplot_data, bf_p1_barplot_data) {
  cols <- c(
    "highly_relevant" = "#012998",
    "relevant" = "#205BFE",
    "less_relevant" = "#AEC3FF"
  )

  omega3_plot <- ggplot(
    omega3_barplot_data,
    aes(fill = outcome, y = n, x = value_number)
  ) +
    geom_bar(position = "stack", stat = "identity") +
    theme_classic() +
    scale_y_continuous(n.breaks = 7, name = "No. of countries") +
    scale_x_continuous(n.breaks = 8, name = "Omega-3 threshold values") +
    scale_fill_manual(values = cols) +
    theme(legend.position = "top") +
    geom_vline(aes(xintercept = 10, col = "red")) #Ordinary cut-off - need to specify

  bf_p1_plot <- ggplot(
    bf_p1_barplot_data,
    aes(fill = outcome, y = n, x = value_number)
  ) +
    geom_bar(position = "stack", stat = "identity") +
    theme_classic() +
    scale_y_continuous(n.breaks = 7, name = "No. of countries") +
    scale_x_continuous(n.breaks = 9, name = "Blue Foods threshold values") +
    scale_fill_manual(values = cols) +
    theme(legend.position = "top") +
    geom_vline(aes(xintercept = 8, col = "red")) #Ordinary cut-off - need to specify

  bf_p1_plot_short <- ggplot(
    bf_p1_barplot_data,
    aes(fill = outcome, y = n, x = value_number)
  ) +
    geom_bar(position = "stack", stat = "identity") +
    theme_classic() +
    scale_y_continuous(n.breaks = 7, name = "No. of countries") +
    scale_x_continuous(
      n.breaks = 10,
      name = "Blue Foods threshold values",
      limits = c(0, 250)
    ) +
    scale_fill_manual(values = cols) +
    theme(legend.position = "top") +
    geom_vline(aes(xintercept = 8, col = "red")) #Ordinary cut-off - need to specify

  policy1_plots <- plot_grid(
    omega3_plot,
    bf_p1_plot,
    bf_p1_plot_short,
    ncol = 2,
    labels = c("Omega-3", "Blue Foods - full", "Blue Foods - cropped"),
    hjust = 0,
    label_x = 0.01
  )

  ggsave(
    policy1_plots,
    file = "./Plots/sensitivity_plots/policy1_sensitivity.png",
    dpi = 300,
    height = 18.3,
    width = 18.3,
    units = "cm"
  )
  ggsave(
    policy1_plots,
    file = "./Plots/sensitivity_plots/policy1_sensitivity.svg",
    dpi = 300,
    height = 18.3,
    width = 18.3,
    units = "cm"
  )

  return(policy1_plots)
}
#*************************************************************
make_policy2_sensitivity <- function(
  redmeat_barplot_data,
  daly_barplot_data,
  bf_p2_barplot_data
) {
  cols <- c(
    "highly_relevant" = "#012998",
    "relevant" = "#205BFE",
    "less_relevant" = "#AEC3FF"
  )

  redmeat_plot <- ggplot(
    redmeat_barplot_data,
    aes(fill = outcome, y = n, x = value_number)
  ) +
    geom_bar(position = "stack", stat = "identity") +
    theme_classic() +
    scale_y_continuous(n.breaks = 7, name = "No. of countries") +
    scale_x_continuous(n.breaks = 8, name = "Red meat threshold values") +
    scale_fill_manual(values = cols) +
    theme(legend.position = "top") +
    geom_vline(aes(xintercept = 50, col = "red")) #Ordinary cut-off - need to specify

  daly_plot <- ggplot(
    daly_barplot_data,
    aes(fill = outcome, y = n, x = value_number)
  ) +
    geom_bar(position = "stack", stat = "identity") +
    theme_classic() +
    scale_y_continuous(n.breaks = 7, name = "No. of countries") +
    scale_x_continuous(n.breaks = 8, name = "DALY threshold values") +
    scale_fill_manual(values = cols) +
    theme(legend.position = "top") +
    geom_vline(aes(xintercept = 0.05, col = "red")) #Ordinary cut-off - need to specify

  bf_p2_plot <- ggplot(
    bf_p2_barplot_data,
    aes(fill = outcome, y = n, x = value_number)
  ) +
    geom_bar(position = "stack", stat = "identity") +
    theme_classic() +
    scale_y_continuous(n.breaks = 7, name = "No. of countries") +
    scale_x_continuous(n.breaks = 9, name = "Blue Foods threshold values") +
    scale_fill_manual(values = cols) +
    theme(legend.position = "top") +
    geom_vline(aes(xintercept = 8, col = "red")) #Ordinary cut-off - need to specify

  bf_p2_plot_short <- ggplot(
    bf_p2_barplot_data,
    aes(fill = outcome, y = n, x = value_number)
  ) +
    geom_bar(position = "stack", stat = "identity") +
    theme_classic() +
    scale_y_continuous(n.breaks = 7, name = "No. of countries") +
    scale_x_continuous(
      n.breaks = 10,
      name = "Blue Foods threshold values",
      limits = c(0, 250)
    ) +
    scale_fill_manual(values = cols) +
    theme(legend.position = "top") +
    geom_vline(aes(xintercept = 8, col = "red")) #Ordinary cut-off - need to specify

  policy2_plots <- plot_grid(
    redmeat_plot,
    daly_plot,
    bf_p2_plot,
    bf_p2_plot_short,
    ncol = 2,
    labels = c("Red meat", "DALY", "Blue Foods - full", "Blue Foods - cropped"),
    hjust = 0,
    label_x = 0.01
  )

  ggsave(
    policy2_plots,
    file = "./Plots/sensitivity_plots/policy2_sensitivity.png",
    dpi = 300,
    height = 18.3,
    width = 18.3,
    units = "cm"
  )
  ggsave(
    policy2_plots,
    file = "./Plots/sensitivity_plots/policy2_sensitivity.svg",
    dpi = 300,
    height = 18.3,
    width = 18.3,
    units = "cm"
  )

  return(policy2_plots)
}
#*************************************************************
make_policy3_sensitivity <- function(rummeat_barplot_data, bf_p3_barplot_data) {
  cols <- c(
    "highly_relevant" = "#012998",
    "relevant" = "#205BFE",
    "less_relevant" = "#AEC3FF"
  )

  rummeat_plot <- ggplot(
    rummeat_barplot_data,
    aes(fill = outcome, y = n, x = value_number)
  ) +
    geom_bar(position = "stack", stat = "identity") +
    theme_classic() +
    scale_y_continuous(n.breaks = 7, name = "No. of countries") +
    scale_x_continuous(n.breaks = 8, name = "Ruminant meat threshold values") +
    scale_fill_manual(values = cols) +
    theme(legend.position = "top") +
    geom_vline(aes(xintercept = 7, col = "red")) #Ordinary cut-off - need to specify

  bf_p3_plot <- ggplot(
    bf_p3_barplot_data,
    aes(fill = outcome, y = n, x = value_number)
  ) +
    geom_bar(position = "stack", stat = "identity") +
    theme_classic() +
    scale_y_continuous(n.breaks = 7, name = "No. of countries") +
    scale_x_continuous(n.breaks = 9, name = "Blue Foods threshold values") +
    scale_fill_manual(values = cols) +
    theme(legend.position = "top") +
    geom_vline(aes(xintercept = 8, col = "red")) #Ordinary cut-off - need to specify

  bf_p3_plot_short <- ggplot(
    bf_p3_barplot_data,
    aes(fill = outcome, y = n, x = value_number)
  ) +
    geom_bar(position = "stack", stat = "identity") +
    theme_classic() +
    scale_y_continuous(n.breaks = 7, name = "No. of countries") +
    scale_x_continuous(
      n.breaks = 10,
      name = "Blue Foods threshold values",
      limits = c(0, 250)
    ) +
    scale_fill_manual(values = cols) +
    theme(legend.position = "top") +
    geom_vline(aes(xintercept = 8, col = "red")) #Ordinary cut-off - need to specify

  policy3_plots <- plot_grid(
    rummeat_plot,
    NA,
    bf_p3_plot,
    bf_p3_plot_short,
    ncol = 2,
    labels = c(
      "Ruminant meat",
      "",
      "Blue Foods - full",
      "Blue Foods - cropped"
    ),
    hjust = 0,
    label_x = 0.01
  )

  ggsave(
    policy3_plots,
    file = "./Plots/sensitivity_plots/policy3_sensitivity.png",
    dpi = 300,
    height = 18.3,
    width = 18.3,
    units = "cm"
  )
  ggsave(
    policy3_plots,
    file = "./Plots/sensitivity_plots/policy3_sensitivity.svg",
    dpi = 300,
    height = 18.3,
    width = 18.3,
    units = "cm"
  )

  return(policy3_plots)
}
#*************************************************************
make_policy4_sensitivity <- function(
  jobs_barplot_data,
  export_barplot_data,
  consump_barplot_data,
  climate_barplot_data
) {
  cols <- c(
    "highly_relevant" = "#012998",
    "relevant" = "#205BFE",
    "less_relevant" = "#AEC3FF"
  )

  jobs_plot <- ggplot(
    jobs_barplot_data,
    aes(fill = outcome, y = n, x = value_number)
  ) +
    geom_bar(position = "stack", stat = "identity") +
    theme_classic() +
    scale_y_continuous(n.breaks = 7, name = "No. of countries") +
    scale_x_continuous(n.breaks = 8, name = "Jobs threshold values") +
    scale_fill_manual(values = cols) +
    theme(legend.position = "top") +
    geom_vline(aes(xintercept = 0.01, col = "red")) #Ordinary cut-off - need to specify

  export_plot <- ggplot(
    export_barplot_data,
    aes(fill = outcome, y = n, x = value_number)
  ) +
    geom_bar(position = "stack", stat = "identity") +
    theme_classic() +
    scale_y_continuous(n.breaks = 7, name = "No. of countries") +
    scale_x_continuous(n.breaks = 8, name = "Export threshold values") +
    scale_fill_manual(values = cols) +
    theme(legend.position = "top") +
    geom_vline(aes(xintercept = 0.03, col = "red")) #Ordinary cut-off - need to specify

  consump_plot <- ggplot(
    consump_barplot_data,
    aes(fill = outcome, y = n, x = value_number)
  ) +
    geom_bar(position = "stack", stat = "identity") +
    theme_classic() +
    scale_y_continuous(n.breaks = 7, name = "No. of countries") +
    scale_x_continuous(n.breaks = 8, name = "Consumption threshold values") +
    scale_fill_manual(values = cols) +
    theme(legend.position = "top") +
    geom_vline(aes(xintercept = 0.2, col = "red")) #Ordinary cut-off - need to specify

  climate_plot <- ggplot(
    climate_barplot_data,
    aes(fill = outcome, y = n, x = value_number)
  ) +
    geom_bar(position = "stack", stat = "identity") +
    theme_classic() +
    scale_y_continuous(n.breaks = 7, name = "No. of countries") +
    scale_x_continuous(n.breaks = 8, name = "Climate Hazard threshold values") +
    scale_fill_manual(values = cols) +
    theme(legend.position = "top") +
    geom_vline(aes(xintercept = 50, col = "red")) #Ordinary cut-off - need to specify

  policy4_plots <- plot_grid(
    jobs_plot,
    export_plot,
    consump_plot,
    climate_plot,
    ncol = 2,
    labels = c("Jobs", "Export", "Consumption", "Climate"),
    hjust = 0,
    label_x = 0.01
  )

  ggsave(
    policy4_plots,
    file = "./Plots/sensitivity_plots/policy4_sensitivity.png",
    dpi = 300,
    height = 18.3,
    width = 18.3,
    units = "cm"
  )
  ggsave(
    policy4_plots,
    file = "./Plots/sensitivity_plots/policy4_sensitivity.svg",
    dpi = 300,
    height = 18.3,
    width = 18.3,
    units = "cm"
  )

  return(policy4_plots)
}
#____________________________________________________________________
#Histogram of distributions ----
make_hist_facet <- function(policy_data) {
  log_data <- policy_data %>%
    mutate(b12_log = log(SEV_vitB12)) %>%
    mutate(bf_log = log(bf_availability_kgcap_year)) %>%
    mutate(jobs_log = log(totjobs_percap)) %>%
    mutate(exp_log = log(export_percgdp))

  b12_hist <- ggplot(policy_data, aes(x = SEV_vitB12)) +
    geom_histogram(binwidth = 1) +
    theme_classic() +
    ylab("No. of countries") +
    xlab("SEV Vitamin B12") +
    theme(legend.position = "none") +
    geom_vline(aes(xintercept = 10, col = "red")) #Ordinary cut-off - need to specify

  b12_log_hist <- ggplot(log_data, aes(x = b12_log)) +
    geom_histogram(binwidth = 1) +
    theme_classic() +
    ylab("No. of countries") +
    xlab("SEV Vitamin B12") +
    theme(legend.position = "none") #+
  #geom_vline(aes(xintercept=10,col="red")) #Ordinary cut-off - need to specify

  omega3_hist <- ggplot(policy_data, aes(x = SEV_omega3)) +
    geom_histogram(binwidth = 1) +
    theme_classic() +
    ylab("No. of countries") +
    xlab("SEV omega-3") +
    theme(legend.position = "none") +
    geom_vline(aes(xintercept = 10, col = "red")) #Ordinary cut-off - need to specify

  BF_hist <- ggplot(policy_data, aes(x = bf_availability_kgcap_year)) +
    geom_histogram(binwidth = 10) +
    theme_classic() +
    ylab("No. of countries") +
    xlab("Blue Food availability") +
    theme(legend.position = "none") +
    geom_vline(aes(xintercept = 8, col = "red")) #Ordinary cut-off - need to specify

  BF_log_hist <- ggplot(log_data, aes(x = bf_log)) +
    geom_histogram(binwidth = 1) +
    theme_classic() +
    ylab("No. of countries") +
    xlab("Blue Food availability - logged") +
    theme(legend.position = "none") +
    geom_vline(aes(xintercept = 2.079442, col = "red")) #Ordinary cut-off - need to specify

  Red_meat_hist <- ggplot(policy_data, aes(x = red_meat_gcapday)) +
    geom_histogram(binwidth = 1) +
    theme_classic() +
    ylab("No. of countries") +
    xlab("Red meat consumption") +
    theme(legend.position = "none") +
    geom_vline(aes(xintercept = 50, col = "red")) #Ordinary cut-off - need to specify

  daly_hist <- ggplot(policy_data, aes(x = DALY_cardiovascular_cap)) +
    geom_histogram(binwidth = 0.001) +
    theme_classic() +
    ylab("No. of countries") +
    xlab("DALYs") +
    theme(legend.position = "none") +
    geom_vline(aes(xintercept = 0.05, col = "red")) #Ordinary cut-off - need to specify

  ruminant_hist <- ggplot(policy_data, aes(x = ruminant_meat_gcapday)) +
    geom_histogram(binwidth = 1) +
    theme_classic() +
    ylab("No. of countries") +
    xlab("Ruminant meat consumption") +
    theme(legend.position = "none") +
    geom_vline(aes(xintercept = 7, col = "red")) #Ordinary cut-off - need to specify

  jobs_hist <- ggplot(policy_data, aes(x = totjobs_percap)) +
    geom_histogram(binwidth = 0.01) +
    theme_classic() +
    ylab("No. of countries") +
    xlab("Employment") +
    theme(legend.position = "none") +
    geom_vline(aes(xintercept = 0.01, col = "red")) #Ordinary cut-off - need to specify

  export_hist <- ggplot(policy_data, aes(x = export_percgdp)) +
    geom_histogram(binwidth = 0.01) +
    theme_classic() +
    ylab("No. of countries") +
    xlab("Export revenue") +
    theme(legend.position = "none") +
    geom_vline(aes(xintercept = 0.03, col = "red")) #Ordinary cut-off - need to specify

  consump_hist <- ggplot(policy_data, aes(x = aq_reliance_ratio)) +
    geom_histogram(binwidth = 0.01) +
    theme_classic() +
    ylab("No. of countries") +
    xlab("Consumption reliance") +
    theme(legend.position = "none") +
    geom_vline(aes(xintercept = 0.2, col = "red")) #Ordinary cut-off - need to specify

  climate_hist <- ggplot(policy_data, aes(x = ssp585_2050)) +
    geom_histogram(binwidth = 1) +
    theme_classic() +
    ylab("No. of countries") +
    xlab("Climate Hazard Score") +
    theme(legend.position = "none") +
    geom_vline(aes(xintercept = 50, col = "red")) #Ordinary cut-off - need to specify

  facet_plot <- plot_grid(
    b12_hist,
    omega3_hist,
    BF_hist,
    Red_meat_hist,
    daly_hist,
    ruminant_hist,
    jobs_hist,
    export_hist,
    consump_hist,
    climate_hist,
    ncol = 2
  )

  ggsave(
    facet_plot,
    file = "./Plots/sensitivity_plots/hist_facet.svg",
    dpi = 300,
    height = 28,
    width = 18,
    units = "cm"
  )
  #ggsave(facet_plot,file="./Plots/sensitivity_plots/hist_facet.png",dpi=300, height = 28,width = 18,units = "cm")

  return(facet_plot)
}
