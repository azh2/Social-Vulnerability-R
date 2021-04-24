## R
readlines <- function(...) {
  lapply(list(...), readline)
}

input = readlines(
  "Please Input Your Census API Key (Get a Free Census Api Key Here: <https://api.census.gov/data/key_signup.html>): ",
  "Enter the State(s) you would like to use separated by a comma (i.e. Oregon, Washington) or enter USA if you want to calculate SVI for all 50 states: ",
  "What Year would you like to calculate? (2009-2019 Are Available): ",
  "Where would you like to save these files? Please type or copy and paste a complete file path: "
)

#**Install and load required packages**

API.key = input[[1]]
States = as.list(unlist(strsplit(input[[2]], split=",")))
Year = as.integer(input[[3]])
dir.create(paste0(gsub("\\\\", "/", input[[4]]), "/Social_Vulnerability"))
setwd(paste0(gsub("\\\\", "/", input[[4]]), "/Social_Vulnerability"))

ReqPkgs <-
  c(
    'knitr',
    'sp',
    'sf',
    'spdep',
    'tidycensus',
    'dplyr',
    'tidyr',
    'mapview',
    'RColorBrewer',
    'leaflet',
    'leafpop',
    'ggplot2',
    'data.table'
  )

ReqPkgs <- as.list(ReqPkgs)

package.check <- lapply(
  ReqPkgs,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

options(tigris_use_cache = TRUE)

### TidyCensus

#For this section you will use the `tidycensus` package to read in data from the American Community Survey, including geometry.

#Get a Free Census Api Key Here: <https://api.census.gov/data/key_signup.html>

tidycensus::census_api_key(key = API.key,
                           install = TRUE,
                           overwrite = TRUE)
readRenviron("~/.Renviron")

### List Counties in State

#Now were making a simple character vector to store the names of all the counties we'll be using to pull in the census data since no option exists to pull out blockgroup level data for the whole state.

'%notin%' <- Negate('%in%')
US.States <-
  as.list(
    c(
      'West Virginia',
      'Florida',
      'Illinois',
      'Minnesota',
      'Maryland',
      'Rhode Island',
      'Idaho',
      'New Hampshire',
      'North Carolina',
      'Vermont',
      'Connecticut',
      'Delaware',
      'New Mexico',
      'California',
      'New Jersey',
      'Wisconsin',
      'Oregon',
      'Nebraska',
      'Pennsylvania',
      'Washington',
      'Louisiana',
      'Georgia',
      'Alabama',
      'Utah',
      'Ohio',
      'Texas',
      'Colorado',
      'South Carolina',
      'Oklahoma',
      'Tennessee',
      'Wyoming',
      'Hawaii',
      'North Dakota',
      'Kentucky',
      'Maine',
      'New York',
      'Nevada',
      'Alaska',
      'Michigan',
      'Arkansas',
      'Mississippi',
      'Missouri',
      'Montana',
      'Kansas',
      'Indiana',
      'South Dakota',
      'Massachusetts',
      'Virginia',
      'District of Columbia',
      'Iowa',
      'Arizona'
    )
  )

Counties <- c()

if (States[[1]] == "USA") {
  States <- US.States
} else if (States %notin% US.States) {
  print("Please check state name(s) and try again, see the list of acceptable state names below:")
  print(as.character(US.States))
} else {
  States <- States
}

for (i in seq(1, length(States))) {
  State <- as.character(States[[i]])
  Counties[[i]] <- tigris::list_counties(state = State)
  names(Counties[[i]]) <- State
}

### List Variables To Be Pulled In

#Now were making a simple character vector to store the names of all the variables we'll be pulling in at the blockgroup and tract level, these were selected through a long process of trial an error

#Blockgroup Level Variables
varsBG <-
  c(
    'B25003_001',
    'B25003_003',
    'B25070_007',
    'B25070_008',
    'B25070_009',
    'B25070_010',
    'B25071_001',
    'B11007_001',
    'B11007_003',
    'B25034_001',
    'B25034_008',
    'B25034_009',
    'B25034_010',
    'B25034_011',
    'B01003_001',
    'B19301_001',
    'B25033_001',
    'B25033_006',
    'B25033_007',
    'B25033_012',
    'B25033_013',
    'B25044_001',
    'B25044_003',
    'B25044_010',
    'B23025_003',
    'B23025_005',
    'B25014_001',
    'B25014_005',
    'B25014_006',
    'B25014_007',
    'B25014_011',
    'B25014_012',
    'B25014_013',
    'B25024_001',
    'B25024_007',
    'B25024_008',
    'B25024_009',
    'B09021_022',
    'B09021_001',
    'B01001_020',
    'B01001_021',
    'B01001_022',
    'B01001_023',
    'B01001_024',
    'B01001_025',
    'B01001_044',
    'B01001_045',
    'B01001_046',
    'B01001_047',
    'B01001_048',
    'B01001_049',
    'B99163_001',
    'B99163_005',
    'B01001_003',
    'B01001_004',
    'B01001_005',
    'B01001_006',
    'B01001_027',
    'B01001_028',
    'B01001_029',
    'B01001_030',
    'B03002_003',
    'B02001_004',
    'B02001_001',
    'B02001_003',
    'B03003_003',
    'B02001_006',
    'B02001_007',
    'B02001_008',
    'B03002_003',
    'B03002_001',
    'B02001_001',
    'B25002_001',
    'B25002_003',
    'B15003_001',
    'B15003_016',
    'B15003_017',
    'B15003_018',
    'B15003_019',
    'B15003_020',
    'B15003_021',
    'B15003_022',
    'B15003_023',
    'B15003_024',
    'B15003_025',
    'B02001_005',
    'B03003_001',
    'B25070_001',
    'B17020_001',
    'C17002_001',
    'C17002_002',
    'C17002_003',
    'C17002_004'
  )

#Tract Level Variables
varsCT <-
  c(
    'B18101_025',
    'B18101_026',
    'B18101_006',
    'B18101_007',
    'C18130_009',
    'C18130_010',
    'C18130_016',
    'C18130_017',
    'B26001_001',
    'B11004_012',
    'B11004_018',
    'B09008_001',
    'B09008_010',
    'B09008_011',
    'B09008_012',
    'B17023_001',
    'B17023_016',
    'B17023_017',
    'B17023_018',
    'B22002_001'
  )

### Pulling in Block Group Level

#This chunk of code is pulling in all the blockgroup level variables described previously

print("Bringing in blockgroup data...")
CBG18_1 <- c()
for (i in seq(1, length(States))) {
  CBG18_1[[i]] <- tidycensus::get_acs(
    #get_decentennial() pulls in data from the decentennial census 1990-2010
    geography = 'block group',
    #other options include us, region, division, state, county subdivision, census tract, block, place, alaska native regional corporation, american indian area/alaska native area/hawaiian home land, american indian area/alaska native area (reservation or statistical entity only), american indian area (off-reservation trust land only)/hawaiian home land, metropolitan statistical area/micropolitan statistical area, combined statistical area, new england city and town area, combined new england city and town area, urban area, congressional district, school district (elementary, secondary or unified), public use microdata area, zip code tabulation area, and state legislative district (upper or lower chamber).
    state = names(Counties[[i]])[1],
    county = as.character(unlist(Counties[[i]][1])),
    #The county list created in the previous step
    survey = 'acs5',
    #could include the ACS 1, 3 or 5 year surveys
    year = Year,
    #2009 through 2018 are available. Defaults to 2018
    variables = varsBG,
    #The variable list created in the previous step, use tidycensus::load_variables to see what variables are available for the survey and or geography, there may be alternatives or others you want to add!
    geometry = FALSE,
    #if TRUE, uses the tigris package to return an sf tibble with simple feature geometry in the 'geometry' column. We use Tigris later to pull the geometry in.
    output = 'wide',
    show_call = FALSE
  )
}

CBG18_1 <- do.call("rbind", CBG18_1)

#Separate Place Names#
CBG18_1 <-
  tidyr::separate(
    data = CBG18_1,
    col = "NAME",
    into = c("BLOCK_GROUP", "CENSUS_TRACT", "COUNTY", "STATE"),
    sep = ",",
    remove = FALSE
  )

CBG18_1$TRACT_GEOID <- substring(CBG18_1$GEOID, 1, 11)

#print(dim(CBG18_1)) #The dimensions should match this for Oregon: 2,634 x 187

### Pulling in Tract Level Variables

#This chunk of code is pulling in all the tract level variables described previously

print("Bringing in tract data...")
CT18_1 <- c()
for (i in seq(1, length(States))) {
  CT18_1[[i]] <- tidycensus::get_acs(
    #get_decentennial() pulls in data from the decentennial census 1990-2010
    geography = 'Tract',
    #other options include us, region, division, state, county subdivision, census tract, block, place, alaska native regional corporation, american indian area/alaska native area/hawaiian home land, american indian area/alaska native area (reservation or statistical entity only), american indian area (off-reservation trust land only)/hawaiian home land, metropolitan statistical area/micropolitan statistical area, combined statistical area, new england city and town area, combined new england city and town area, urban area, congressional district, school district (elementary, secondary or unified), public use microdata area, zip code tabulation area, and state legislative district (upper or lower chamber).
    state = names(Counties[[i]])[1],
    county = as.character(unlist(Counties[[i]][1])),
    #The county list created in the previous step
    survey = 'acs5',
    #could include the ACS 1, 3 or 5 year surveys
    year = Year,
    #2009 through 2018 are available. Defaults to 2018
    variables = varsCT,
    #The variable list created in the previous step, use tidycensus::load_variables to see what variables are available for the survey and or geography, there may be alternatives or others you want to add!
    geometry = FALSE,
    #if TRUE, uses the tigris package to return an sf tibble with simple feature geometry in the 'geometry' column. We use Tigris later to pull the geometry in.
    output = 'wide',
    show_call = FALSE
  )
}

CT18_1 <- do.call("rbind", CT18_1)

#Separate Place Names
CT18_1 <-
  tidyr::separate(
    data = CT18_1,
    col = "NAME",
    into = c("CENSUS_TRACT", "COUNTY", "STATE"),
    sep = ","
  )

CT18_1$TRACT_GEOID <- CT18_1$GEOID

#print(dim(CT18B))

#The dimensions should match this for Oregon: 834 x 45

### Join Tables

#This chunk uses dplyr to join the tract level variables to the block
#groups, the variables remain consistent across the block group, this
#is not ideal and if you find some way to represent these variables
#more accurately at the block group level, please feel free to change
#them.

JndTbls <-
  dplyr::left_join(x = CBG18_1, y = CT18_1, by = "TRACT_GEOID")

### Now to calculate each of the statistics

#SOCIOECONOMIC STATUS:

JndTbls$TOTPOP <- JndTbls$B01003_001E #TOTAL_POPULATION -
JndTbls$POV <-
  (JndTbls$C17002_002E + JndTbls$C17002_003E) / JndTbls$C17002_001E #PER_POVERTY
JndTbls$UNEMP <-
  JndTbls$B23025_005E / JndTbls$B23025_003E #PER_UNEMPLOYED
JndTbls$PCI <- JndTbls$B19301_001E #PER_CAPITA_INCOME

#LANGUAGE AND EDUCATION:

JndTbls$NOHSDP <-
  1 - ((
    JndTbls$B15003_016E + JndTbls$B15003_017E + JndTbls$B15003_018E + JndTbls$B15003_019E +
      JndTbls$B15003_020E + JndTbls$B15003_021E + JndTbls$B15003_022E + JndTbls$B15003_023E +
      JndTbls$B15003_024E + JndTbls$B15003_025E
  ) / JndTbls$B15003_001E
  ) #PER_LESS_HS_GRAD
JndTbls$LIMENG <-
  JndTbls$B99163_005E / JndTbls$B99163_001E #PER_POOR_ENGLISH

#DEMOGRAPHICS:

JndTbls$AGE65 <-
  JndTbls$B09021_022E / JndTbls$B09021_001E #PER_OVER_65
JndTbls$AGE17 <-
  (
    JndTbls$B01001_003E + JndTbls$B01001_004E + JndTbls$B01001_005E + JndTbls$B01001_006E +
      JndTbls$B01001_027E + JndTbls$B01001_028E + JndTbls$B01001_029E + JndTbls$B01001_030E
  ) / JndTbls$B01003_001E #PER_UNDER_17
JndTbls$DISABL <-
  (
    JndTbls$B18101_026E + JndTbls$B18101_007E + JndTbls$C18130_010E + JndTbls$C18130_017E
  ) / (
    JndTbls$B18101_025E + JndTbls$B18101_006E + JndTbls$C18130_009E + JndTbls$C18130_016E
  ) #PER_DISABLED
JndTbls$SNGPNT <-
  (JndTbls$B09008_010E + JndTbls$B09008_011E + JndTbls$B09008_012E) / JndTbls$B09008_001E #PER_SINGL_PRNT

#HOUSING AND TRANSPORTATION:

JndTbls$MUNIT <-
  (JndTbls$B25024_007E + JndTbls$B25024_008E + JndTbls$B25024_009E) / JndTbls$B25024_001E #PER_MULTI_DWELL
JndTbls$MOBILE <-
  (
    JndTbls$B25033_006E + JndTbls$B25033_007E + JndTbls$B25033_012E + JndTbls$B25033_013E
  ) / JndTbls$B25033_001E #PER_MOBILE_DWEL
JndTbls$CROWD <-
  (
    JndTbls$B25014_005E + JndTbls$B25014_006E + JndTbls$B25014_007E + JndTbls$B25014_011E +
      JndTbls$B25014_012E + JndTbls$B25014_013E
  ) / JndTbls$B25014_001E #PER_CROWD_DWELL
JndTbls$NOVEH <-
  (JndTbls$B25044_003E + JndTbls$B25044_010E) / JndTbls$B25044_001E #PER_NO_VEH_AVAIL
JndTbls$GROUPQ <-
  JndTbls$B26001_001E / JndTbls$B01003_001E #PER_GROUP_DWELL

#RACIAL AND ETHNIC MAKEUP:

JndTbls$MINORITY <- 1 - (JndTbls$B03002_003E / JndTbls$B03002_001E)
JndTbls$NTVAMRCN <- JndTbls$B02001_004E / JndTbls$B02001_001E
JndTbls$ASIAN <- JndTbls$B02001_005E / JndTbls$B02001_001E
JndTbls$BLACK <- JndTbls$B02001_003E / JndTbls$B02001_001E
JndTbls$HISPLATX <- JndTbls$B03003_003E / JndTbls$B03003_001E
JndTbls$PACISL <- JndTbls$B02001_006E / JndTbls$B02001_001E
JndTbls$OTHRRACE <- JndTbls$B02001_007E / JndTbls$B02001_001E
JndTbls$MULTRACE <- JndTbls$B02001_008E / JndTbls$B02001_001E
JndTbls$WHITE <- JndTbls$B03002_003E / JndTbls$B03002_001E

#OPTIONAL VARIABLES:

JndTbls$HOMESOCCPD <- 1 - JndTbls$B25002_003E / JndTbls$B25002_001E
JndTbls$RENTER <- JndTbls$B25003_003E / JndTbls$B25003_001E
JndTbls$RENTBURDEN <-
  (
    JndTbls$B25070_007E + JndTbls$B25070_008E + JndTbls$B25070_009E + JndTbls$B25070_010E
  ) / JndTbls$B25070_001E
JndTbls$RENTASPERINCOME <- (JndTbls$B25071_001E / 100)
JndTbls$OVR65ALONE <- JndTbls$B11007_003E / JndTbls$B11007_001E
JndTbls$BLTBFR1969 <-
  (
    JndTbls$B25034_008E + JndTbls$B25034_009E + JndTbls$B25034_010E + JndTbls$B25034_011E
  ) / JndTbls$B25034_001E
JndTbls$SVRPOV <- JndTbls$C17002_002E / JndTbls$C17002_001E
JndTbls$MODPOV <- JndTbls$C17002_004E / JndTbls$C17002_001E
JndTbls$SINGLMTHRPVRTY <-
  (JndTbls$B17023_016E + JndTbls$B17023_017E + JndTbls$B17023_018E) / JndTbls$B17023_001E

#RANKING#

#These functions rank each of the variables, variables with matching values across ranks are given the max score, this is the default in excel where the original formulae were derived

a <-
  JndTbls$RNKPOV <-
  rank(x = -JndTbls$POV,
       na.last = "keep",
       ties.method = "max")
b <-
  JndTbls$RNKUNEMP <-
  rank(x = -JndTbls$UNEMP,
       na.last = "keep",
       ties.method = "max")
c <-
  JndTbls$RNKPCI <-
  rank(x = JndTbls$PCI,
       na.last = "keep",
       ties.method = "max") #Note that we are not taking the inverse here because the higher the Per Capita Income, the greater the Adaptive Capacity of a given blockgroup
d <-
  JndTbls$RNKNOHSDP <-
  rank(x = -JndTbls$NOHSDP,
       na.last = "keep",
       ties.method = "max")
e <-
  JndTbls$RNKLIMENG <-
  rank(x = -JndTbls$LIMENG,
       na.last = "keep",
       ties.method = "max")
f <-
  JndTbls$RNKAGE65 <-
  rank(x = -JndTbls$AGE65,
       na.last = "keep",
       ties.method = "max")
g <-
  JndTbls$RNKAGE17 <-
  rank(x = -JndTbls$AGE17,
       na.last = "keep",
       ties.method = "max")
h <-
  JndTbls$RNKDISABL <-
  rank(x = -JndTbls$DISABL,
       na.last = "keep",
       ties.method = "max")
i <-
  JndTbls$RNKSNGPNT <-
  rank(x = -JndTbls$SNGPNT,
       na.last = "keep",
       ties.method = "max")
j <-
  JndTbls$RNKMUNIT <-
  rank(x = -JndTbls$MUNIT,
       na.last = "keep",
       ties.method = "max")
k <-
  JndTbls$RNKMOBILE <-
  rank(x = -JndTbls$MOBILE,
       na.last = "keep",
       ties.method = "max")
l <-
  JndTbls$RNKCROWD <-
  rank(x = -JndTbls$CROWD,
       na.last = "keep",
       ties.method = "max")
m <-
  JndTbls$RNKNOVEH <-
  rank(x = -JndTbls$NOVEH,
       na.last = "keep",
       ties.method = "max")
n <-
  JndTbls$RNKGROUPQ <-
  rank(x = -JndTbls$GROUPQ,
       na.last = "keep",
       ties.method = "max")

#Sum The Ranks

JndTbls$SUMRANK = a + b + c + d + e + f + g + h + i + j + k + l + m + n

#Derive the Adaptive Capacity Index

JndTbls$ADPTVCAPACITY <- dplyr::percent_rank(JndTbls$SUMRANK)

#**This Finds How Much Each Variable Contributed to The Final Percent Rank (Optional)**

# This Determines the Percentage Contribution to Final Rank
JndTbls$GEOID <-
  JndTbls$GEOID.x #Geoid.s was created in the previous join and needs to be renamed before joining it to the geometry
geoid <- which(colnames(JndTbls) == "GEOID")
a <- which(colnames(JndTbls) == "RNKPOV")
z <- which(colnames(JndTbls) == "RNKGROUPQ")
cols <- as.vector(names(JndTbls[a:z]))
Func <- function(x) {
  round((abs(x) / abs(JndTbls$SUMRANK)), 2) * 100
}
RnkPerc <-
  dplyr::mutate_at(.tbl = JndTbls,
                   .vars = cols,
                   .funs = Func)
RnkPerc <- RnkPerc[c(geoid, a:z)]
JndTbls <- dplyr::right_join(JndTbls, RnkPerc, by = "GEOID")

#### **Now to Bring in Geometry From Tigris**

JndTbls$GEOID <- JndTbls$GEOID.x #Geoid.x was created in the previous join and needs to be renamed before joining it to the geometry

JndTblsSP <- c()
for(i in seq(1, length(States))) {
  State <- names(Counties[[i]])[1]
  County.Selection <- as.character(unlist(Counties[[i]][1]))
  JndTblsSP[[i]] <- tigris::block_groups(state = State, county = County.Selection, cb = TRUE) #we are using simplified geometry here, this can be changed by setting cb = FALSE, but takes a little bit longer to download
}

JndTblsSP <- do.call("rbind", unlist(lapply(JndTblsSP, as_Spatial), recursive = FALSE))

JndTblsSP <- sp::merge(x = JndT, JndTbls, by = 'GEOID')

#### Now Let's Map Our Results!

suppressPackageStartupMessages(require(leaflet))
suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(leaflet.esri))

pop <- paste0(
  "<h3>",
  "<b>",
  JndTblsSP$COUNTY.x,
  "</b>",
  "</h3>",
  "<b>",
  JndTblsSP$CENSUS_TRACT.x,
  "</b>",
  "<br>",
  "<b>",
  "TOTAL POPULATION: ",
  prettyNum(JndTblsSP$TOTPOP, big.mark = ","),
  " +/- ",
  JndTblsSP$B01003_001M,
  "</b>",
  "<br>",
  "<b>",
  "ADAPTIVE CAPACITY: ",
  round(100 * (JndTblsSP$ADPTVCAPACITY), 1),
  "%",
  "</b>",
  "<br>",
  
  "<b><h4>SOCIOECONOMIC STATUS:<b></h4>",
  
  "<b>PCT LIVING IN POVERTY: </b>",
  round(100 * (JndTblsSP$POV), 1),
  "%",
  "<br>",
  "<b>PCT 16+ UNEMPLOYED: </b>",
  round(100 * (JndTblsSP$UNEMP), 1),
  "%",
  "<br>",
  "<b>PER CAPITA INCOME: </b>",
  "$",
  prettyNum(JndTblsSP$PCI, big.mark = ","),
  "<br>",
  
  "<b><h4>LANGUAGE AND EDUCATION:<b></h4>",
  
  "<b>PCT OF POP 25+ LESS THAN 12th GRADE: </b>",
  round(100 * (JndTblsSP$NOHSDP), 1),
  "%",
  "<br>",
  "<b>PCT NO ENGLISH: </b>",
  round(100 * (JndTblsSP$LIMENG), 1),
  "%",
  "<br>",
  
  "<b><h4>DEMOGRAPHICS:</h4><b>",
  
  "<b>PCT UNDER AGE OF 17: </b>",
  round(100 * (JndTblsSP$AGE17), 1),
  "%",
  "<br>",
  "<b>PCT 65+: </b>",
  round(100 * (JndTblsSP$AGE65), 1),
  "%",
  "<br>",
  "<b>PCT DISABLED: </b>",
  round(100 * (JndTblsSP$DISABL), 1),
  "%",
  "<br>",
  "<b>PCT CHLDRN LVNG IN SNGL PARENT HSHLDS: </b>",
  round(100 * (JndTblsSP$SNGPNT), 1),
  "%",
  "<br>",
  
  "<b><h4>HOUSING AND TRANSPORTATION:</h4><b>",
  
  "<b>PCT LIVING IN MULTI-UNIT STRUCTURE: </b>",
  round(100 * (JndTblsSP$MUNIT), 1),
  "%",
  "<br>",
  "<b>PCT MOBILE DWELLING: </b>",
  round(100 * (JndTblsSP$MOBILE), 1),
  "%",
  "<br>",
  "<b>PCT LIVING IN CROWDED DWELLING: </b>",
  round(100 * (JndTblsSP$CROWD), 1),
  "%",
  "<br>",
  "<b>PCT WITH NO VEHICLE ACCESS: </b>",
  round(100 * (JndTblsSP$NOVEH), 1),
  "%",
  "<br>",
  "<b>PCT LIVING IN GROUP QUARTERS: </b>",
  round(100 * (JndTblsSP$GROUPQ), 1),
  "%",
  "<br>",
  
  "<b><h4>RACIAL AND ETHNIC MAKEUP:<b></h4>",
  "<b>PCT MINORITY: </b>",
  round(100 * (JndTblsSP$MINORITY), 1),
  "%"
) #Here we're creating a popup for our interactive map, include whatever variables you want here!

BRBG <- RColorBrewer::brewer.pal(n = 11, name = "BrBG")

pal <- leaflet::colorQuantile(
  palette = BRBG,
  domain = JndTblsSP$ADPTVCAPACITY,
  n = 11,
  reverse = FALSE
) #Creating a Color Pallete, Feel free to choose whatever one you want, see the package Viridis for some cool options

myMap <- leaflet(data = JndTblsSP) %>% addTiles() %>% addPolygons(
  color = "#444444",
  weight = 1,
  smoothFactor = 0.5,
  opacity = 0.5,
  fillOpacity = 0.5,
  fillColor = ~ pal(ADPTVCAPACITY),
  highlightOptions = highlightOptions(
    color = "white",
    weight = 2,
    bringToFront = TRUE
  ),
  popup = pop,
  popupOptions = popupOptions(maxHeight = 250, maxWidth = 250,)
) %>% addLegend(
  "bottomright",
  pal = pal,
  values = JndTblsSP$ADPTVCAPACITY,
  title = "Adaptive Capacity Score",
  labFormat = labelFormat(prefix = ""),
  opacity = 0.75
)

mapview::mapshot(myMap, url = paste0(getwd(), "/social_vulnerability_map.html"))

print(
  paste0(
    "Your Leaflet Web Map can be viewed at: ",
    getwd(),
    "/social_vulnerability_map.html"
  )
)

#### **Use This Function to Export The Shapefile (or csv) to a Folder of Your Choice!**

#Export to Shapefile
rgdal::writeOGR(
  obj = JndTblsSP,
  dsn = getwd(),
  driver = "ESRI Shapefile",
  layer = "AdaptiveCapacityR",
  morphToESRI = FALSE, 
  overwrite_layer = TRUE
)

print(
  paste0(
    "Your Social Vulnerability Shapefile can be viewed at: ",
    getwd(),
    "/AdaptiveCapacityR.shp"
  )
)

#Or CSV (The benefit of this is that it preserves the field names, I found it better to export the geometry (blockgroup_Geom) alone and then join the data by GEOID in ArcGIS)
write.csv(x = JndTbls,
          file = paste0(getwd(), "/AdaptiveCapacityR.csv"))

print(
  paste0(
    "Your Social Vulnerability CSV can be viewed at: ",
    getwd(),
    "/AdaptiveCapacityR.csv"
  )
)

## Licensing/Disclaimer:

#THIS SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
#OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
#MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
#IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
#CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
#TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
#SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#THIS SOFTWARE HAS NOT BEEN PEER-REVIEWED AND IS SUBJECT TO REVISION. THE
#AUTHOR NOR THE NATURE CONSERVANCY MAKE ANY WARRANTY AS TO THE CURRENCY,
#COMPLETENESS, ACCURACY OR UTILITY OF THIS SOFTWARE. IT IS STRONGLY
#RECOMMENDED THAT CAREFUL ATTENTION BE PAID TO THE DOCUMENTATION
#ASSOCIATED WITH THIS SOFTWARE. THE AUTHOR NOR THE NATURE CONSERVANCY
#SHALL BE HELD LIABLE FOR IMPROPER OR INCORRECT USE OF THIS SOFTWARE OR
#ANY DATA PRODUCED HEREIN. ALL PARTIES UTILIZING THIS SOFTWARE MUST BE
#INFORMED OF THESE RESTRICTIONS. THE NATURE CONSERVANCY AND AUTHOR SHALL
#BE ACKNOWLEDGED IN ANY REPORTS OR OTHER PRODUCTS DERIVED FROM THIS
#SOFTWARE.