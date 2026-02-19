#targets script

library(targets)
source("./functions.R") # defines functions

tar_option_set(
  packages = c(
    "tidyverse",
    "countrycode",
    "rnaturalearth",
    "rnaturalearthdata",
    "cowplot",
    "modelr"
  )
)

list(
  tar_target(
    data_list,
    read_data("./data/data")
  ),

  tar_target(
    meat_list,
    read_data("./data/meat_data")
  ),

  tar_target(
    clean_data,
    wrangle_data(data_list, meat_list)
  ),

  tar_target(
    app_file,
    save_file_and_return_path(clean_data),
    format = "file"
  ),

  tar_target(
    policy_data,
    assigning_recommendation(clean_data)
  ),

  tar_target(
    policy_summary,
    summarising_policy(policy_data)
  ),

  tar_target(
    salience_percentage,
    calc_policy_salience(policy_summary)
  ),

  tar_target(
    map_plots,
    map_plot(policy_summary)
  ),

  tar_target(
    iso3c,
    get_iso(clean_data)
  ),

  tar_target(
    range_SEV_omega3,
    seq_range(clean_data$SEV_omega3, by = 1)
  ),

  tar_target(
    range_SEV_B12,
    seq_range(clean_data$SEV_vitB12, by = 1)
  ),

  tar_target(
    range_BF,
    seq_range(policy_data$bf_availability_kgcap_year, by = 1)
  ),

  tar_target(
    range_redmeat,
    seq_range(clean_data$red_meat_gcapday, by = 1)
  ),

  tar_target(
    range_daly,
    seq_range(clean_data$DALY_cardiovascular_cap, by = 0.001)
  ),

  tar_target(
    range_rummeat,
    seq_range(clean_data$ruminant_meat_gcapday, by = 1)
  ),

  tar_target(
    range_jobs,
    seq_range(clean_data$totjobs_percap, by = 0.01)
  ),

  tar_target(
    range_export,
    seq_range(clean_data$export_percgdp, by = 0.01)
  ),

  tar_target(
    range_consump,
    seq_range(clean_data$aq_reliance_ratio, by = 0.01)
  ),

  tar_target(
    range_climate,
    seq_range(clean_data$ssp585_2050, by = 1)
  ),

  tar_target(
    omega3_data,
    make_sensitivity_data(
      range_SEV_omega3,
      make_classification_o3_p1,
      clean_data,
      iso3c
    )
  ),

  tar_target(
    b12_data,
    make_sensitivity_data(
      range_SEV_B12,
      make_classification_b12_p0,
      clean_data,
      iso3c
    )
  ),

  tar_target(
    bf_p0_data,
    make_sensitivity_data(
      range_BF,
      make_classification_bf_p0,
      clean_data,
      iso3c
    )
  ),

  tar_target(
    bf_p1_data,
    make_sensitivity_data(
      range_BF,
      make_classification_bf_p1,
      clean_data,
      iso3c
    )
  ),

  tar_target(
    redmeat_data,
    make_sensitivity_data(
      range_redmeat,
      make_classification_redmeat,
      clean_data,
      iso3c
    )
  ),

  tar_target(
    daly_data,
    make_sensitivity_data(
      range_daly,
      make_classification_daly,
      clean_data,
      iso3c
    )
  ),

  tar_target(
    bf_p2_data,
    make_sensitivity_data(
      range_BF,
      make_classification_bf_p2,
      clean_data,
      iso3c
    )
  ),

  tar_target(
    rummeat_data,
    make_sensitivity_data(
      range_rummeat,
      make_classification_rummeat,
      clean_data,
      iso3c
    )
  ),

  tar_target(
    bf_p3_data,
    make_sensitivity_data(
      range_BF,
      make_classification_bf_p3,
      clean_data,
      iso3c
    )
  ),

  tar_target(
    jobs_data,
    make_sensitivity_data(
      range_jobs,
      make_classification_jobs,
      clean_data,
      iso3c
    )
  ),

  tar_target(
    export_data,
    make_sensitivity_data(
      range_export,
      make_classification_export,
      clean_data,
      iso3c
    )
  ),

  tar_target(
    consump_data,
    make_sensitivity_data(
      range_consump,
      make_classification_consump,
      clean_data,
      iso3c
    )
  ),

  tar_target(
    climate_data,
    make_sensitivity_data(
      range_climate,
      make_classification_climate,
      clean_data,
      iso3c
    )
  ),

  tar_target(
    omega3_barplot_data,
    make_barplot_data(omega3_data)
  ),

  tar_target(
    b12_barplot_data,
    make_barplot_data(b12_data)
  ),

  tar_target(
    bf_p0_barplot_data,
    make_barplot_data(bf_p0_data)
  ),

  tar_target(
    bf_p1_barplot_data,
    make_barplot_data(bf_p1_data)
  ),

  tar_target(
    redmeat_barplot_data,
    make_barplot_data(redmeat_data)
  ),

  tar_target(
    daly_barplot_data,
    make_barplot_data(daly_data)
  ),

  tar_target(
    bf_p2_barplot_data,
    make_barplot_data(bf_p2_data)
  ),

  tar_target(
    rummeat_barplot_data,
    make_barplot_data(rummeat_data)
  ),

  tar_target(
    bf_p3_barplot_data,
    make_barplot_data(bf_p3_data)
  ),

  tar_target(
    jobs_barplot_data,
    make_barplot_data(jobs_data)
  ),

  tar_target(
    export_barplot_data,
    make_barplot_data(export_data)
  ),

  tar_target(
    consump_barplot_data,
    make_barplot_data(consump_data)
  ),

  tar_target(
    climate_barplot_data,
    make_barplot_data(climate_data)
  ),

  tar_target(
    policy0_sensitivity,
    make_policy0_sensitivity(b12_barplot_data, bf_p0_barplot_data)
  ),

  tar_target(
    policy1_sensitivity,
    make_policy1_sensitivity(omega3_barplot_data, bf_p1_barplot_data)
  ),

  tar_target(
    policy2_sensitivity,
    make_policy2_sensitivity(
      redmeat_barplot_data,
      daly_barplot_data,
      bf_p2_barplot_data
    )
  ),

  tar_target(
    policy3_sensitivity,
    make_policy3_sensitivity(rummeat_barplot_data, bf_p3_barplot_data)
  ),

  tar_target(
    policy4_sensitivity,
    make_policy4_sensitivity(
      jobs_barplot_data,
      export_barplot_data,
      consump_barplot_data,
      climate_barplot_data
    )
  ),

  tar_target(
    hist_facet,
    make_hist_facet(policy_data)
  )
)

#***************************************
#to run: tar_make()
#to check: tar_visnetwork()
#to load: tar_load(target_name)
#to delete cache of specific target: tar_delete("target_name")
