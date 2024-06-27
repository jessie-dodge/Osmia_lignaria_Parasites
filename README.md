01_beeParasites_Analysis.Rproj - R project created to contain all Excel data tables, R scripts, R markdowns, and outputs for the "Landscape variation effects on Osmia lignaria reproduction, development, and parasites" project.

01a_libraries.R - Contains all R packages needed to analyze/visualize landscape effects on Osmia lignaria parasitism

01b.1_beeReproDev_2020_NestContents_dataset.R - This file is used to average individual nest cell contents measured within Osmia lignaria nests surveyed in 2020 in Boulder County sites for projects determining landscape effects on Osmia lignaria parasitism and diet. Requires files: "01a_libraries.R", "240219_2020_NestPollenBeeData.csv"

01b.2_beeReproDev_2021_NestContents_dataset.R - This file is used to average individual nest cell contents measured within Osmia lignaria nests surveyed in 2021 in Boulder County sites for projects determining landscape effects on Osmia lignaria parasitism and diet. Requires files: "01a_libraries.R", "240220_2021_NestPollenBeeData.csv"

01b.3_beeReproDev_NestContents_dataset.R - This file is used to combine nest cell contents measured within Osmia lignaria nests surveyed in 2020 and 2021 in Boulder County sites projects determining landscape effects on Osmia lignaria parasitism and diet. Requires files: "01b.1_beeReproDev_2020_NestContents_dataset.R", file="01b.2_beeReproDev_2021_NestContents_dataset.R"

01c_beeReproDev_Flower_dataset.R - This file is used to organize and combine average flower data surveyed in 2020 and 2021 in Boulder County sites for projects determining landscape effects on Osmia lignaria parasitism and diet. Requires files: "file="01a_libraries.R", "240219_2020_Floral_data_Plot_ReOrg_Cleaned_v4_JD.csv", "240223_2021_floralData_Plot_JD.csv"

01d_beeReproDev_Tree_dataset.R - This file is used to organize forest structural measurements surveyed in 2020 in Boulder County sites for projects determining landscape effects on Osmia lignaria parasitism and diet. Requires files: "file="01a_libraries.R", "240222_TreeCC_2020-2021.csv", "230630_2020_TreeDBH.csv", "230531_2021_TreeDBH.csv"

01e_beeReproDev_EVT_dataset.R - This file is used to organize Environmental Vegetation Types (EVT) downloaded from LANDFIRE for Boulder County sites and spatially joined to 500 m buffers around both Osmia lignaria sites sampled in 2020 and 2021 using ArcMap Pro for projects determining landscape effects on Osmia lignaria parasitism and diet. Requires files: "01a_libraries.R", "231116_BoCo2020BuffTsj_Tab.csv", "231116_BoCo2021BuffTsj_Tab.csv"

01f_beeReproDev_climate_dataset.R - This file is used to organize climate variables downloaded from PRISM for Boulder County sites sampled in 2020 and 2021 for projects determining landscape effects on Osmia lignaria parasitism and diet. Requires files: "01a_libraries.R", "PRISM_ppt_tmin_tmean_tmax_30yr_monthly_normals_800m_reorg.csv", "240307_BoCo_sites_2020_PRISMdata_montly.csv", "240307_BoCo_sites_2021_PRISMdata_montly.csv"

01g_beeReproDev_degreeDay_dataset.R - This file is used to calculate degree days accumulated for bees in Boulder County sites sampled in 2020 and 2021 for projects determining landscape effects on Osmia lignaria parasitism and diet. Data was downloaded from PRISM. Required files: "01a_libraries.R", "2020_PRISM_tmean_stable_4km_0512_0727.csv", "240522_2020_accumulatedDegreeDays.csv", "2021_PRISM_tmean_stable_4km_0524_0701.csv", "240522_2021_accumulatedDegreeDays.csv"

01h_beeReproDev_HLI_dataset.R - This file is used to calculate HLI for each site surveyed in Boulder County sites sampled in 2020 and 2021 for projects determining landscape effects on Osmia lignaria parasitism and diet. Required files: "01a_libraries.R", "240219_BoCo_sites_2020-2021.csv"

01i_beeReproDev_MasterData.R - This file is used to combine all data frames of data surveyed Boulder County sites sampled in 2020 and 2021 for projects determining landscape effects on Osmia lignaria parasitism and diet into a metadata file for all measurements averaged by site. Required files, "01a_libraries.R", "01b.3_beeReproDev_NestContents_dataset.R", "01c_beeReproDev_Flower_dataset.R", "01d_beeReproDev_Tree_dataset.R", "01e_beeReproDev_EVT_dataset.R", "01f_beeReproDev_climate_dataset.R", "01g_beeReproDev_degreeDay_dataset.R", "01h_beeReproDev_HLI_dataset.R"

02a_beeParasite_DataExploration.Rmd - R markdown file exploring Osmia lignaria fitness and parasitism data
02b_beeParasite_ExplorationAnalysis.Rmd - R markdown file outlining questions for Landscape effects on Osmia lignaria reproduction, development, and parasites project and exploratory analyses used to answer each question.
02b_beeParasite_ExplorationAnalysis_v2.Rmd - 2nd version of R markdown file outlining questions for Landscape effects on Osmia lignaria reproduction, development, and parasites project and exploratory analyses used to answer each question.
