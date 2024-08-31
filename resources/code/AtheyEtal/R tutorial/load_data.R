my_na <- function(column) {
  ifelse(column %in% c(-999), NA, column)
}

select_dataset <- function(dataset_name){
  dataset_name <<- dataset_name
  if (dataset_name == "social") {
    # Load raw data
    df <- readr::read_csv(file = "https://raw.githubusercontent.com/gsbDBI/ExperimentData/master/Social/ProcessedData/socialpressnofact.csv", na = character())

    # Specify outcome, treatment, and covariate variable names to use
    outcome_variable_name <<- "outcome_voted"
    treatment_variable_name <<- "treat_neighbors"
    covariate_names <<-c("sex", "yob", "city", "hh_size", "totalpopulation_estimate",
                        "percent_male", "median_age", "percent_62yearsandover",
                        "percent_white", "percent_black", "percent_asian", "median_income",
                        "employ_20to64", "highschool", "bach_orhigher","percent_hispanicorlatino")
    dfs <<- data.frame(lapply(df, function(x) as.numeric(as.character(x))))

  } else if (dataset_name == "charitable") {
    # Load raw data
    df <- readr::read_csv(file = "https://raw.githubusercontent.com/gsbDBI/ExperimentData/master/Charitable/ProcessedData/charitable_withdummyvariables.csv", na = character())

    # Specify outcome, treatment, and covariate variable names to use
    outcome_variable_name <<- "out_gavedum"
    treatment_variable_name <<- "treatment"
    covariate_names <<- c("pwhite", "pblack", "ave_hh_sz", "median_hhincome",  "pop_propurban", "female", "couple", "red0", "redcty")

    # This dataset encodes missing values as -999. Let's change these to NA.
       df<- df %>% mutate_all(my_na)
       dfs <<- data.frame(lapply(df, function(x) as.numeric(as.character(x))))

  } else if (dataset_name == "mobilization") {

    # Create a temporary file and download the data to it.
    temp <- tempfile()
    download.file("https://raw.githubusercontent.com/gsbDBI/ExperimentData/master/Mobilization/ProcessedData/mobilization_with_unlisted.zip", temp)

    # # Load raw data
    df <- readr::read_csv((unz(temp, "mobilization_with_unlisted.csv")), na = character())
    unlink(temp)
    df <- subset(dfs, treatment ==1 | treatment==0)

    #  Specify outcome, treatment, and covariate variable names to use
    outcome_variable_name <<- "vote02"
    treatment_variable_name <<- "treatment"
    covariate_names <<- c("comp_ia", "age", "female", "vote98", "vote00",
                         # "state", "newreg", "comp_mi", # These covariates are commented out because
                         "county","st_sen","st_hse")     # they are constant in the first 5000 rows.
    # Thus, they can be included if using the entire dataset.sfor
    dfs <<- data.frame(lapply(df, function(x) as.numeric(as.character(x))))


  } else if (dataset_name == "secrecy") {
    # Load raw data
    df <- readr::read_csv(file = "https://raw.githubusercontent.com/gsbDBI/ExperimentData/master/Secrecy/ProcessedData/ct_ballotsecrecy_processed.csv", na = character())

    # Specify outcome, treatment, and covariate variable names to use
    outcome_variable_name <<- "turnoutindex_12"
    treatment_variable_name <<- "anysecrecytreatment"
    covariate_names <<- c("v_cong_general_10", "v_pres_primary_12",	"v_cong_primary_12","v_pres_general_12",
                         "town1_block",	"town2_block", "town3_block","town4_block", "town5_block",
                         "town6_block",	"i_grp_addr_1",	"i_grp_addr_2",	"i_grp_addr_3",	"i_grp_addr_4",
                         "dem",	"rep",	"female",	"age_md",	"age_sq_md")
    dfs <<- data.frame(lapply(df, function(x) as.numeric(as.character(x))))

  } else if (dataset_name == "welfare") {
    # Load raw data
    df <- readr::read_csv(file = "https://raw.githubusercontent.com/gsbDBI/ExperimentData/master/Welfare/ProcessedData/welfarenolabel3.csv", na = character())


    # Specify outcome, treatment, and covariate variable names to use
    outcome_variable_name <<- "y"
    treatment_variable_name <<- "w"
    covariate_names <<- c("hrs1", "partyid", "income", "rincome", "wrkstat", "wrkslf","age", "polviews",
                         "educ", "earnrs", "race","wrkslf",
                         "marital","sibs","childs", "occ80",  "prestg80", "indus80","res16","reg16","mobile16", "family16", "parborn","maeduc","degree","sex","race","born","hompop","babies","preteen","teens","adults")

    # This dataset encodes missing values as -999. Let's change these to NA.
    dfs<<- df %>% mutate_all(my_na)

  } else if (dataset_name == "advertising") {

    # Load raw data
    current_directory <- getwd()

    #  checking to see if the criteo data already exists in the directory. if not download the data
    if (!("adcontentworth_qje.csv" %in% list.files(current_directory))){
      url <- 'https://raw.githubusercontent.com/gsbDBI/ExperimentData/master/Advertising/adcontentworth_qje.csv'
      download.file(url, paste0(current_directory, "/", "adcontentworth_qje.csv"))
    }

    # Load raw data
    df <- read.csv(file = "adcontentworth_qje.csv")

    # Subsetting based on wave>1 yields the 53,194 observations used in paper
    df <- df[dfs$wave > 1, ]

    # Specify outcome, treatment, and covariate variable names to use
    outcome_variable_name <<- "applied" # alternatively, use "tookup"
    treatment_variable_name <<- "speak_trt" # alternatively, use "oneln_trt" or another
    covariate_names <<- c("offer4", "stripany",  "dphoto_black", "dphoto_female", "prize", "oneln_trt", "use_any", "intshown", "comploss_n", "comp_n", "risk", "waved3", "female", "race", "race_match" , "gender_match")

    # --- Content treatments from paper ---
    # speak_trt, stripany, dphoto_none, dphoto_black, dphoto_female,
    # gender_match, race_match, prize, oneln_trt, use_any, intshown, comploss_n ,comp_n
    # -------------------------------------
    dfs <<- data.frame(lapply(df, function(x) as.numeric(as.character(x))))

  } else if (dataset_name == "criteo") {
    # Load raw data
    current_directory <- getwd()
    #  checking to see if the criteo data already exists in the directory. if not download the data
    if (!("criteo.csv.gz" %in% list.files(current_directory))){
      url <- 'https://s3.us-east-2.amazonaws.com/criteo-uplift-dataset/criteo-uplift.csv.gz'
      download.file(url, paste0(current_directory, "/", "criteo.csv.gz"))
    }

    df <- readr::read_csv("criteo.csv.gz", na = character())

    # Specify outcome, treatment, and covariate variable names to use
    outcome_variable_name <<- "visit"
    treatment_variable_name <<- "treatment"
    covariate_names <<- c( "f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10", "f11" )
    dfs <<- data.frame(lapply(df, function(x) as.numeric(as.character(x))))

  } else {
    # Raise an error if the dataset is unknown
    print("Incorrect dataset is specified. Change 'dataset_name' to one of {charitable, mobilization, secrecy, social, welfare, criteo }")

    # Exiting knitr
    knitr::knit_exit()
    print("Exiting knitr without completion")
  }}


