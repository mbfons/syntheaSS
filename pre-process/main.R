library(tidyverse)
library(openxlsx)

df_pop <- read.xlsx('./pre-process/input/TF_SOC_POP_STRUCT_2024.xlsx')

mycol <- colnames(df_pop)
mychoi <- c(mycol[c(1:13)],"MS_POPULATION")

# df_pop2 <- df_pop %>% group_by(vars(mychoi)) %>%
#                       summarise(n_s = sum(MS_POPULATION)) %>% ungroup()


# Create unique 'cities' and tot pop --------------------------------------------------
df_cit <- df_pop %>% group_by(CD_REFNIS,TX_DESCR_NL,TX_DESCR_FR,CD_DSTR_REFNIS,TX_ADM_DSTR_DESCR_NL,TX_ADM_DSTR_DESCR_FR,CD_PROV_REFNIS,
                               TX_PROV_DESCR_NL,TX_PROV_DESCR_FR,CD_RGN_REFNIS,TX_RGN_DESCR_NL,TX_RGN_DESCR_FR) %>%
  summarise(TOT_POP=sum(MS_POPULATION)) %>% ungroup()


# Create gender distribution ----------------------------------------------

df_pop2 <- df_pop %>% group_by(CD_REFNIS,
                               CD_SEX) %>%
  summarise(n_s = sum(MS_POPULATION)) %>% # numbers by gender
  summarise(CD_SEX = CD_SEX,
            p_s = n_s/sum(n_s)) %>% # relative frequency of gender
  ungroup() %>%
  pivot_wider(names_from=CD_SEX,values_from = p_s)


df_cit <- df_cit %>% left_join(df_pop2)


# Create ethnicity distribution -------------------------------------------

# Ethnicity not collected in Belgium, only country of origin
#https://statbel.fgov.be/en/news/diversity-according-origin-belgium-0
# maintain default from Synthea BE file

df_cit <- df_cit %>% mutate(WHITE=1,
                 HISPANIC=0,
                 BLACK=0,
                 ASIAN=0,
                 NATIVE=0,
                 OTHER=0)

# Create age distribution -------------------------------------------------

df_pop3 <- df_pop %>%
  mutate(GP_AGE = cut(CD_AGE,
      breaks = c(0, seq(4,84,5), Inf),
      #labels = c("0-20", "21-40", "41-60", "61-80", "81+"),
      labels = c(1:18),
      right = TRUE,
      include.lowest=TRUE)) %>% # create age groups
  group_by(CD_REFNIS,
           GP_AGE) %>%
  summarise(n_s = sum(MS_POPULATION)) %>% # numbers by age group
  summarise(GP_AGE = GP_AGE,
            p_s = n_s/sum(n_s)) %>% # relative frequency of gender
  ungroup() %>%
  pivot_wider(names_from=GP_AGE,values_from = p_s)
  
df_cit <- df_cit %>% left_join(df_pop3)



# Create education distribution -------------------------------------------

df_ed <- read.xlsx('./pre-process/input/T01_EDU_BE_NL.xlsx',
          sheet = 'CENSUS_T01_2021_BE_EDU_2021',startRow = 4) %>%
  rename(`CODE-NIS`=X1,Verblijfplaats=X2,Sex=X3) %>%
  fill(`CODE-NIS`,Verblijfplaats) %>% # fill empty row
  filter(Sex=="Totaal") # keep only total


df_ed <- df_ed %>% mutate(LESS_THAN_HS =
                            `Geen.diploma.of.getuigschrift`+
                            `Lager.onderwijs.(ISCED.1)`+
                            `Lager.secundair.onderwijs.(ISCED.2)`, # should this include those less than15??
                          HS_DEGREE = `Hoger.secundair.onderwijs.(ISCED.3)`,
                          SOME_COLLEGE = `Postsecundair.niet-tertiair.onderwijs.(ISCED.4)`+
                            `Tertiar.onderwijs;.korte.cyclus.(ISCED.5)`,
                          BS_DEGREE = `Bachelorniveau.of.gelijkwaardig.(ISCED.6)`,`Masterniveau.of.gelijkwaardig.(ISCED.7)`,          
                          `Doctoraatsniveau.of.gelijkwaardig.(ISCED.8)`,
                          EDUGROUP = LESS_THAN_HS+HS_DEGREE+SOME_COLLEGE+BS_DEGREE,
                          LESS_THAN_HS=LESS_THAN_HS/EDUGROUP,
                          HS_DEGREE=HS_DEGREE/EDUGROUP,
                          SOME_COLLEGE = SOME_COLLEGE/EDUGROUP,
                          BS_DEGREE = BS_DEGREE/EDUGROUP)

  
df_cit <- df_cit %>% left_join(df_ed %>% select(`CODE-NIS`,LESS_THAN_HS,HS_DEGREE,SOME_COLLEGE,BS_DEGREE),by=c("CD_REFNIS"="CODE-NIS"))



# Income ------------------------------------------------------------------
# use some generics here for now. Rough and eye-balled from 'repartition'. Not exact brackets/year-adjusted/no-incomeadjusted etc
# https://statbel.fgov.be/en/themes/work-training/wages-and-labourcost/overview-belgian-wages-and-salaries#:~:text=The%20average%20gross%20monthly%20salary%20is%204%2C076%20euros&text=In%202022%2C%20full%2Dtime%20employees,among%20more%20than%20184%2C000%20employees.

df_cit <- df_cit %>% mutate(`00..10`=0,
                            `10..15`=0,
                            `15..25`=0.11,
                            `25..35`=0.255,
                            `35..50`=0.345,
                            `50..75`=0.187,
                            `75..100`=0.077,
                            `100..150`=0.026,
                            `150..200`=0,
                            `200..999`=0)


df_cit_out <- df_cit %>% mutate(ID=1:nrow(df_cit)) %>%
  rename(COUNTY=CD_REFNIS,
         NAME=TX_DESCR_FR, # city name (city used)
         #STNAME=TX_RGN_DESCR_FR, # region at 'state'
         #CTYNAME = TX_DESCR_FR, # county name (city used)
         TOT_MALE=M,
         TOT_FEMALE=F
         ) %>%
  mutate(POPESTIMATE2015 = TOT_POP,
         CTYNAME=NAME, # city name (city used)
         STNAME = ifelse(`CD_RGN_REFNIS`=="02000","Vlaanderen",
                         ifelse(`CD_RGN_REFNIS`=="03000","Wallonie",
                                "Bruxelles"))) 


# Output variables --------------------------------------------------------

myhead <- read.csv('./pre-process/input/demographics.csv')
myheadcol <- colnames(myhead)

myheadcol <- c(myheadcol[1:15],substr(myheadcol[16:43],2,500),myheadcol[44:47])

df_cit_out <- df_cit_out[myheadcol]

myheadcol


# Save to file ------------------------------------------------------------

write.csv(df_cit_out,"./pre-process/output/demographics.csv")




# Postcodes ---------------------------------------------------------------

#https://bruxellesdata.opendatasoft.com/explore/dataset/codes-ins-nis-postaux-belgique/table/?flg=en-gb&disjunctive.postal_code&disjunctive.refnis_code&disjunctive.gemeentenaam&disjunctive.nom_commune&disjunctive.code_ins_region&disjunctive.region_fr&disjunctive.region_nl&disjunctive.region_en&sort=refnis_code

df_pcd <- read.csv('./pre-process/input/codes-ins-nis-postaux-belgique.csv',
                   sep=";")

df_zip_out <- df_pcd %>% mutate(USPS = ifelse(`NIS.code.Region`==2000,"Vlaanderen",
                                              ifelse(`NIS.code.Region`==3000,"Wallonie",
                                                     "Bruxelles")),
                                ST = ifelse(`NIS.code.Region`==2000,"VLG",
                                              ifelse(`NIS.code.Region`==3000,"WAL",
                                                     "BRU")),
                                ZCTA5=Postal.code,
                                LAT = strsplit(Geographical.coordinates,', ')[1],
                                LONG = strsplit(Geographical.coordinates,', ')[2]))

df_zip_out <- df_zip_out %>% left_join(df_cit_out %>% select())
