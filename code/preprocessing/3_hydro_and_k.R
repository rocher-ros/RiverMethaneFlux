########################################.
#### R script to estimate average monthly discharge and the gas transfer velocities for each river reach in GRADES
#### This part is based on the article by Liu et al: https://doi.org/10.1073/pnas.2106322119
#### Author: Gerard Rocher-Ros, from an original script by Shaoda Liu that I have heavily modified
#### Last edit: 2022-05-10
########################################.


#Common variables, function, and datasets
Qquantiles <- c('Q0', 'Q10', 'Q20', 'Q30', 'Q40', 'Q50', 
                'Q60', 'Q70', 'Q80', 'Q90', 'Q100', 'Qmean', 'Qstd')

# 0. Load  and install packages ----
# List of all packages needed
package_list <- c('tidyverse', 'ncdf4', 'foreign')

# Check if there are any packacges missing
packages_missing <- setdiff(package_list, rownames(installed.packages()))

# If we find a package missing, install them
if(length(packages_missing) >= 1) install.packages(packages_missing) 

# Now load all the packages
lapply(package_list, require, character.only = TRUE)

# 1. Load files ----


#read coordinates from grades
grades <-  read_csv("data/raw/gis/GRADES_attributes/grades_coords.csv") %>% 
  select(COMID, lon= lon_mid, lat = lat_mid, subarea)

#read hydrobasin 
hydroBasinID <- read_csv("data/raw/gis/upscaling_vars/comidHydrobasin4.csv", col_types = 'ic') %>% 
  filter(HYBAS_ID != "0")

hydroBasinID_nobasin <- read_csv("data/raw/gis/upscaling_vars/comidHydrobasin4_noBasin.csv", col_types = 'ic') %>% 
  filter(HYBAS_ID != "0")

hydroBasinID <- rbind(hydroBasinID, hydroBasinID_nobasin)

rm(hydroBasinID_nobasin)

hydroBasinSN <-  read_csv("data/raw/gis/upscaling_vars/hydrobasin4_hemisNS.csv", col_types = 'cc')

runoff <- read_csv("data/raw/gis/upscaling_vars/runoffhydrobasin4.csv", col_types = 'cn') %>% 
  drop_na()


hydroBasinFR <- read_csv("data/raw/gis/upscaling_vars/frclass4.csv", col_types = 'cc')

hydroBasinFR_1 <- read_csv("data/raw/gis/upscaling_vars/frclass4_offland.csv", col_types = 'cc')
hydroBasinFR <- rbind(hydroBasinFR, hydroBasinFR_1)
rm(hydroBasinFR_1)

#ann q stat extractor
qAnnStatsExt<-function(nc,statsNm){
  qStats<-ncvar_get(nc,statsNm)
  qStats[qStats < 0.000000001] = 0.000000001
  if (nrow(qStats) == nrow(dbf)){
    return(qStats)
  } else {print('number of rows do not match ...')}
}

#monthly q stats extractor
qStatsExt <- function(nc,statsNm){
  qStats <- ncvar_get(nc, statsNm)
  qStats[qStats < 0.000000001] = 0.000000001
  qStats <- data.frame(qStats)
  colnames(qStats) <- paste0(month.abb,statsNm) 
  if (nrow(qStats) == nrow(dbf) ){
    return(qStats)
  } else {print('number of rows do not match ...')}
}

for (i in 1:8) {
  print(paste0('Processing ', i))
  
  dbf <- read.dbf( paste0( 'data/raw/grades/pfaf_0',i,'_riv_3sMERIT.dbf') ) %>% 
    dplyr::select(COMID, strmOrder, Slope, Length) %>% as_tibble()
  
  qAnnStatsNC <- nc_open(paste0('data/raw/gis/upscaling_vars/qStat/pfaf_0',i,'_all.nc'))
  
  dbf <- dbf %>% 
    mutate(yeaQmean = qAnnStatsExt(qAnnStatsNC, Qquantiles[12]),
           yeaQstd = qAnnStatsExt(qAnnStatsNC, Qquantiles[13]))
  
  nc_close(qAnnStatsNC)
  
  qStatsNC <- nc_open(paste0('data/raw/gis/upscaling_vars/qStat/pfaf_0', i, '.nc'))
  
  dbf <- dbf %>% 
    bind_cols(qStatsExt(qStatsNC, Qquantiles[12])) %>% 
    bind_cols(qStatsExt(qStatsNC, Qquantiles[13]))
  
  nc_close(qStatsNC)
  
  #convert Qstd to Qrsd
  dbf <-
    dbf %>% 
    mutate(across(ends_with('std'), ~ . /
                    get(str_replace(cur_column(), "std$", "mean")), .names = "{.col}cv")) %>%
    rename_at(vars(ends_with('cv')), ~ str_remove(., "std")) %>% 
    dplyr::select( -all_of(paste0(month.abb, 'Qstd')))
  
  uparea <- read_csv(paste0('data/raw/gis/GRADES_attributes/uparea_0', i, '.csv')) %>% 
    select(COMID, uparea)
  
  elev <- read_csv(paste0('data/raw/gis/GRADES_attributes/eleSlope_0', i, '_c_up.csv')) %>% 
    select(COMID, elev)
  
  #join HYBAS_ID, up area, S/N, runoff, frclass
  dbf <- dbf %>% 
    left_join( uparea, by = 'COMID') %>% 
    left_join( elev, by = 'COMID') %>% 
    left_join( hydroBasinID, by='COMID') %>% 
    left_join( hydroBasinSN, by = 'HYBAS_ID') %>% 
    left_join( runoff, by='HYBAS_ID') %>% 
    left_join( hydroBasinFR, by = 'HYBAS_ID')
  
  
  print(paste0('There are ',sum(is.na(dbf$frclass)),' flowlines without FR'))
  
  
  if (table(is.na(dbf$HYBAS_ID))[1] != dim(dbf)[1])
  {
    print(paste(table(is.na(dbf$HYBAS_ID))[2], 'Flowlines have no HYBAS_ID'))
  } else {
    print('All Flowlines have HYBAS_ID')
  }
  
  dbf <- dbf[!is.na(dbf$HYBAS_ID), ]
  
  print(paste0('There are ',sum(is.na(dbf$runoff)),' flowlines without basin runoff'))
  
  
  dbf <- dbf %>%
    mutate(
      Qlevel = case_when(
        runoff < 50 ~ 'ARID',
        runoff < 200 & runoff >= 50 ~ 'MOD1',
        runoff < 500 & runoff >= 200 ~ 'MOD2',
        runoff < 1000 & runoff >= 500 ~ 'WET1',
        runoff >= 1000 ~ 'WET2',
        TRUE ~ NA_character_
      )
    )
  
  dbf <- dbf %>% 
    drop_na(frclass) %>% 
    mutate(wkbasin= paste0( Hemis, Qlevel, substr(frclass,4,6)))
  
  #temperature and Schmidt vales for each month
  temp <- read_csv(paste0('data/raw/gis/GRADES_attributes/monTemp_0',i,'.csv'), lazy = FALSE) %>% 
    rename_with(.fn = ~paste0(., "_Ta"),
                .cols = !contains("COMID") ) %>% 
    mutate(across(ends_with("_Ta"),
                  function(x){ tw = 0.67 * x + 7.45}, #air-water temperature function, comes from Raymond et al (2013), Nature
                  .names = "{.col}_Tw")) %>% 
    rename_at(vars(ends_with('Tw')), ~ str_remove(., "\\_Ta")) 
  
  #calc month V and eD
  vel <- dbf %>% 
    left_join(temp, by="COMID") %>% 
    mutate(across(ends_with("_Tw"),
                  function(x){ 1824 - 98.12 * x + 2.413 * (x^2) - 0.0241 * (x^3)},
                  .names = "{.col}_Sc")) %>% 
    rename_at(vars(ends_with('Sc')), ~ str_remove(., "\\_Tw")) %>% 
    mutate(across(ends_with("Qmean"),
                  function(x){ exp(0.12 * log(x) - 1.06)}, #using USGS Q-V relationship
                  .names = "{.col}_V")) %>% 
    rename_at(vars(ends_with('V', ignore.case = FALSE)), ~ str_remove(., "Qmean")) %>% 
    mutate(across(ends_with("V", ignore.case = FALSE), 
                  ~9.81 * Slope * .x,
                  .names = "{.col}_eD")) %>% 
    rename_at(vars(ends_with('eD')), ~ str_remove(., "\\_V"))
  
k <- vel %>% 
    dplyr::select(COMID, Slope, ends_with(c("V", "Sc", "eD"), ignore.case = FALSE),
                  -starts_with("yea")) %>% 
    pivot_longer(-c(COMID, Slope), names_to = c("month", ".value"), names_sep = "_") %>% 
    mutate(k = (2841 * Slope * V +2.02) / (600/Sc)^0.5,#case_when(Slope <= 0.01 | eD <= 0.02 ~ (2841 * Slope * V +2.02) / (600/Sc)^0.5,
                #         eD > 0.02 ~  exp(1.18*log(eD)+6.43)*(600/Sc)^0.5),
           k = ifelse(is.na(k) == TRUE, .3, k) ) %>%
  dplyr::select(COMID, month, k) %>% 
  pivot_wider(names_from = "month", values_from = k, names_glue = "{month}_{.value}")

k_sd <- vel %>% 
  dplyr::select(COMID, Slope, ends_with(c("V", "Sc", "eD"), ignore.case = FALSE),
                -starts_with("yea")) %>% 
  pivot_longer(-c(COMID, Slope), names_to = c("month", ".value"), names_sep = "_") %>% 
  mutate(k_sd = (107 * Slope * V + 0.209) / (600/Sc)^0.5         
  ) %>%
  dplyr::select(COMID, month, k_sd) %>% 
  pivot_wider(names_from = "month", values_from = k_sd, names_glue = "{month}_{.value}")
  

print("min K")
print(k %>% 
  summarise(across(Jan_k:Dec_k, min)))


  #join k and temp
  dbf <- dbf %>% 
    left_join(k, by= 'COMID') %>% 
    left_join(k_sd, by= 'COMID') %>% 
    left_join(temp, by= 'COMID') %>% 
    left_join(vel %>% select(COMID, ends_with("_V")), by= 'COMID')
  
  if(i==1){
    df <- dbf
  } else { df <- rbind(df,dbf)}

}

#read runoff file
runoff_site <- read_csv("data/raw/gis/GRADES_attributes/runoff.csv", lazy = FALSE) %>% 
  dplyr::select(COMID, runoff_yr)


df <- df %>% 
  left_join(grades %>% select(COMID, subarea), by = "COMID") %>% 
  left_join(runoff_site, by = "COMID")


#saving the results
df %>% 
  select(-ends_with(c("Ta", "_V"))) %>% 
  write_csv('data/processed/q_and_k.csv')

means_reaches <- df %>% 
  rowwise() %>% 
  mutate(mean_k = mean(Jan_k:Dec_k),
         mean_vel = mean(Jan_V:Dec_V)) %>% 
  group_by(strmOrder) %>% 
  summarise(n = n(),
            length = mean(Length),
            slope = mean(Slope),
            vel = mean(mean_vel),
            k = mean(mean_k),
            q = mean(yeaQmean)) %>% 
  arrange(strmOrder)

means_reaches %>% 
  mutate(depth = exp(-0.895 + 0.294*log(q)),
         bigK= k/depth,
         footprint = 3*(vel*3600*24)/bigK)
