# Info ------
# Author: Gerard Rocher-Ros, modified from Shaoda Liu
# Script upscale CH4 emissions in rivers globally.


#Common variables, function, and datasets
Qquantiles <- c('Q0', 'Q10', 'Q20', 'Q30', 'Q40', 'Q50', 
                'Q60', 'Q70', 'Q80', 'Q90', 'Q100', 'Qmean', 'Qstd')

# 0. Load  and install packages ----
# List of all packages needed
package_list <- c('tidyverse', 'googledrive' ,  'ncdf4', 'foreign')

# Check if there are any packacges missing
packages_missing <- setdiff(package_list, rownames(installed.packages()))

# If we find a package missing, install them
if(length(packages_missing) >= 1) install.packages(packages_missing) 

# Now load all the packages
lapply(package_list, require, character.only = TRUE)

# 1. Load files ----



#read coordinates from grades
grades <-  read_csv("data/raw/gis/GRADES_attributes/grades_coords.csv") %>% 
  select(COMID, lon, lat)

#read hydrobasin 
hydroBasinID <-
  read_csv("data/raw/gis/upscaling_vars/comidHydrobasin4.csv",
           col_types = 'ic')

hydroBasinID <- hydroBasinID[!(hydroBasinID$HYBAS_ID %in% c('0')), ]

hydroBasinID_nobasin <-
  read_csv("data/raw/gis/upscaling_vars/comidHydrobasin4_noBasin.csv",
           col_types = 'ic')

hydroBasinID_nobasin <-
  hydroBasinID_nobasin[!(hydroBasinID_nobasin$HYBAS_ID %in% c('0')), ]
hydroBasinID <- rbind(hydroBasinID, hydroBasinID_nobasin)
rm(hydroBasinID_nobasin)

hydroBasinSN <-  read_csv("data/raw/gis/upscaling_vars/hydrobasin4_hemisNS.csv", col_types = 'cc')

runoff <- read_csv("data/raw/gis/upscaling_vars/runoffhydrobasin4.csv", col_types = 'cn') %>% 
  drop_na()


hydroBasinFR <- read_csv("data/raw/gis/upscaling_vars/frclass4.csv",
                         col_types = 'cc')

hydroBasinFR_1 <- read_csv("data/raw/gis/upscaling_vars/frclass4_offland.csv", col_types = 'cc')
hydroBasinFR <- rbind(hydroBasinFR, hydroBasinFR_1)
rm(hydroBasinFR_1)


#ann q stat extractor

#ann q stat extractor
qAnnStatsExt<-function(nc,statsNm){
  qStats<-ncvar_get(nc,statsNm)
  qStats[qStats < 0.000000001]=0.000000001
  #qStats<-data.frame(qStats)
  #colnames(qStats)<-statsNm
  if (nrow(qStats) == nrow(dbf)){
    return(qStats)
  } else {print('number of rows do not match ...')}
}

#monthly q stats extractor
qStatsExt <- function(nc,statsNm){
  qStats <- ncvar_get(nc, statsNm)
  qStats[qStats < 0.000000001]=0.000000001
  qStats <- data.frame(qStats)
  colnames(qStats) <- paste0(month.abb,statsNm) 
  if (nrow(qStats) == nrow(dbf)){
    return(qStats)
  } else {print('number of rows do not match ...')}
}

# Function to calculate gas saturation, 
# from temperature ( in Kelvin) and atm pressure (derived from elevation (in m.a.s.l) for CH4 

# Henry's Law constants from http://www.henrys-law.org
# Temperature correction using van't Hoff equation , temperature is in (Kelvin)
# Kh is in ("Kh, cp") = (mol/L*Atm) at STP
# Concentrations (mole fraction, also ppmv) in the atmosphere are approximate for 2013, should be updated over time
# AtmP is in (atmospheres)

get_CH4eq <- function(temperature, elevation){
  
  pressure <- (1-(.0000225577*elevation))^5.25588
  Kh <- (1.4*10^-3)*exp(1700*((1/temperature)-(1/298)))
  AtmosphereConc <-  1.83
  EquilSaturation <- AtmosphereConc*Kh/pressure #umol/L, mmol/m3
  
  return(EquilSaturation)
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
  
  #temp
  temp <- read_csv(paste0('data/raw/gis/GRADES_attributes/monTemp_0',i,'.csv')) %>% 
    rename_with(.fn = ~paste0(., "_Ta"),
                .cols = !contains("COMID") ) %>% 
    mutate(across(ends_with("_Ta"),
                  function(x){tw=0.67*x+7.45},
                  .names = "{.col}_Tw")) %>% 
    rename_at(vars(ends_with('Tw')), ~ str_remove(., "\\_Ta"))
  
  #join k
  k <- read_csv(paste0('data/raw/gis/GRADES_attributes/k_0',i,'slopeLT00005.csv'))
  
  #join k and temp
  dbf <- dbf %>% 
    left_join(k, by= 'COMID') %>% 
    left_join(temp, by= 'COMID')
  
  if(i==1){
    df<-dbf
  } else {df<-rbind(df,dbf)}
}


colnames(df)

#upscaled methane concentrations
meth_concs <- read_csv("data/processed/meth_predictions.csv")

df <- df %>% 
  left_join(meth_concs, by="COMID") %>% 
  mutate(Jan_ch4eq = get_CH4eq(Jan_Tw + 273.15, elev),
         Feb_ch4eq = get_CH4eq(Feb_Tw + 273.15, elev),
         Mar_ch4eq = get_CH4eq(Mar_Tw + 273.15, elev),
         Apr_ch4eq = get_CH4eq(Apr_Tw + 273.15, elev),
         May_ch4eq = get_CH4eq(May_Tw + 273.15, elev),
         Jun_ch4eq = get_CH4eq(Jun_Tw + 273.15, elev),
         Jul_ch4eq = get_CH4eq(Jul_Tw + 273.15, elev),
         Aug_ch4eq = get_CH4eq(Aug_Tw + 273.15, elev),
         Sep_ch4eq = get_CH4eq(Sep_Tw + 273.15, elev),
         Oct_ch4eq = get_CH4eq(Oct_Tw + 273.15, elev),
         Nov_ch4eq = get_CH4eq(Nov_Tw + 273.15, elev),
         Dec_ch4eq = get_CH4eq(Dec_Tw + 273.15, elev))


df[df$COMID == 61000003, ]$uparea <- 35
df[df$COMID == 31000001, ]$uparea <- 35

rm(hydroBasinID,runoff,dbf,qStatsNC,i,temp,
   hydroBasinFR,hydroBasinSN,uparea,
   k,qAnnStatsExt,qStatsExt,
   qAnnStatsNC,Qquantiles, meth_concs)



df[df$yeaQcv <= quantile(df$yeaQcv, 0.001), ]$yeaQcv <-
  quantile(df$yeaQcv, 0.001) #0.4,min:0.08
df[df$yeaQcv >= quantile(df$yeaQcv, 0.995), ]$yeaQcv <-
  quantile(df$yeaQcv, 0.995) #16.7,max:10

#calc annual mean width
df <- df %>% 
  mutate(
    yeaWidth = case_when(
      Qlevel %in% c('ARID') ~ exp(2.1 + 0.43 * log(yeaQmean)),
      Qlevel %in% c('MOD1') ~ exp(2.1 + 0.47 * log(yeaQmean)),
      Qlevel %in% c('MOD2') ~ exp(2.24 + 0.47 * log(yeaQmean)),
      Qlevel %in% c('WET1') ~ exp(2.2 + 0.45 * log(yeaQmean)),
      Qlevel %in% c('WET2') ~ exp(1.92 + 0.49 * log(yeaQmean))), 
    runoffFL = yeaQmean / uparea * 3.6 * 24 * 365
  )

#calculate WidthExp(exponent of the width-Q relatinship)
#the following procedure prevents unrealistic widthExp estimates

df<- df %>% mutate(
    WidthExp = case_when(
      log(runoffFL) <= 1.6 & log(yeaQmean) <= -7  ~
        0.074 * log(yeaQcv) - 0.026 * 1.6 - 0.011 * (-7) + 0.36,
      log(runoffFL) > 1.6 & log(runoffFL) <= 8.5 & log(yeaQmean) <= (-7) ~
        0.074 * log(yeaQcv) - 0.026 * log(runoffFL) - 0.011 * (-7) + 0.36,
      log(runoffFL)>8.5&log(yeaQmean)<=(-7) ~
        0.074 * log(yeaQcv) - 0.026 * 8.5 - 0.011 * (-7) + 0.36,
      log(runoffFL) <= 1.6 & log(yeaQmean) > (-7) & log(yeaQmean) <= 10 ~
        0.074 * log(yeaQcv) - 0.026 * 1.6 - 0.011 * log(yeaQmean) + 0.36,
      log(runoffFL) > 1.6 & log(runoffFL) <= 8.5 & log(yeaQmean) > (-7) & log(yeaQmean) <= 10 ~
        0.074 * log(yeaQcv) - 0.026 * log(runoffFL) - 0.011 * log(yeaQmean) + 0.36,
      log(runoffFL) > 8.5 & log(yeaQmean) > (-7) & log(yeaQmean) <= 10 ~
        0.074 * log(yeaQcv) - 0.026 * 8.5 - 0.011 * log(yeaQmean) + 0.36,
      log(runoffFL) <= 1.6 & log(yeaQmean) > 10 ~
        0.074 * log(yeaQcv) - 0.026 * 1.6 - 0.011 * 10 + 0.36,
      log(runoffFL) > 1.6 & log(runoffFL) <= 8.5 & log(yeaQmean) > 10 ~
        0.074 * log(yeaQcv) - 0.026 * log(runoffFL) - 0.011 * 10 + 0.36,
      log(runoffFL) > 8.5 & log(yeaQmean) > 10 ~
        0.074 * log(yeaQcv) - 0.026 * 8.5 - 0.011 * 10 + 0.36)
  )

df[df$WidthExp <= quantile(df$WidthExp, 0.01), ]$WidthExp <-
  quantile(df$WidthExp, 0.01)

#calc at-a-station coefs and monthly width
df <-
  df %>% mutate(
    Widthcoef = yeaWidth / (yeaQmean ^ WidthExp),
    JanWidth = Widthcoef * (JanQmean ^ WidthExp),
    FebWidth = Widthcoef * (FebQmean ^ WidthExp),
    MarWidth = Widthcoef * (MarQmean ^ WidthExp),
    AprWidth = Widthcoef * (AprQmean ^ WidthExp),
    MayWidth = Widthcoef * (MayQmean ^ WidthExp),
    JunWidth = Widthcoef * (JunQmean ^ WidthExp),
    JulWidth = Widthcoef * (JulQmean ^ WidthExp),
    AugWidth = Widthcoef * (AugQmean ^ WidthExp),
    SepWidth = Widthcoef * (SepQmean ^ WidthExp),
    OctWidth = Widthcoef * (OctQmean ^ WidthExp),
    NovWidth = Widthcoef * (NovQmean ^ WidthExp),
    DecWidth = Widthcoef * (DecQmean ^ WidthExp)
  )

df <- df[!df$wkbasin %in% c('NARID15'),] #there is only one flowline, caused by misalignment

#time dryout
timedryout_calculater <- function(Q) {
  tdryout = 1 / (1 + exp(11 + 4 * log(Q)))
  

}

#calculate timedryout
df <- df %>% 
  mutate(across(ends_with("Qmean"), timedryout_calculater, .names = "{.col}timedryout" )) %>% 
  rename_at(vars(ends_with('timedryout')), ~ str_remove(., "Qmean")) 


for(mon in month.abb){
  df[df[, paste0(mon, 'Qmean')] >= 60, paste0(mon, 'timedryout')] = 0
  }

#### generate working basins for SO extrapolation####
wkbasins <- dplyr::count(df, wkbasin) %>% arrange(n)
wkbasins$SOabc <- NA #find out contiguous SO for extrapolation in each basin

for (i in 1:length(wkbasins$wkbasin)) {
  print(i)
  df_p <- df[df$wkbasin %in% c(wkbasins[i, ]$wkbasin), ]
  SOvec <- sort(unique(df_p$strmOrder))
  if (sum(1:5 %in% SOvec) == 5) {
    wkbasins[i, 'SOabc'] <- 4
  } else if (sum(1:4 %in% SOvec) == 4) {
    wkbasins[i, 'SOabc'] <- 3
  } else if (sum(1:3 %in% SOvec) == 3) {
    wkbasins[i, 'SOabc'] <- 2
  } else {
    wkbasins[i, 'SOabc'] <- 2
  }
}
rm(df_p)

#join wkbasin prectemp
prectemp <- read_csv('data/raw/gis/upscaling_vars/basin4TempPrec.csv')
colnames(prectemp)[2:13] <- paste0(month.abb, '_tavg')
colnames(prectemp)[14:25] <- paste0(month.abb, '_prec')
wkbasins <- left_join(wkbasins, prectemp, by = 'wkbasin')
rm(prectemp)

####stream order extrapolation and surface area calculation####
for(i in 1:78){
  print(paste0('Processing the ', i, ' basin ...'))
  
  df_basin <- df %>% 
    filter(wkbasin ==  wkbasins[i, ]$wkbasin ) 
  
  for (Mon in month.abb){
    print(Mon)
   
    #join prec and temp
    df_basin_mon <- df_basin %>%
      dplyr::select(wkbasin, HYBAS_ID, strmOrder, Length, paste0(Mon, 'Width'), paste0(Mon, 'timedryout'),
                     paste0(Mon, '_k'), paste0(Mon, '_ch4'), paste0(Mon, '_ch4eq')  ) %>% 
      left_join(wkbasins %>% 
                 select(wkbasin, paste0(Mon, '_prec'), paste0(Mon, '_tavg')),
                 by = 'wkbasin') %>% 
      rename_at(vars(starts_with(Mon)), ~ str_remove(., c(Mon))) %>% 
      rename_at(vars(everything()),  ~ str_remove(., "\\_") ) %>% 
      rename(temp=tavg) %>% 
      dplyr::mutate(
        isChannel = as.numeric(Width >= 0.3),
        surfArea = Length * Width * isChannel / 1000000,
        ephemArea = timedryout * Width * Length * isChannel / 1000000
      )

    ####use ray2013 method to predict ephemeralness
    #1
    df_basin_mon <-
      df_basin_mon %>% dplyr::mutate(
        percInterm_ray13 = case_when(
          strmOrder > 4 ~ 0,
          strmOrder == 4 ~ ((-0.005) * prec + 0.023 * temp + 0.27),
          strmOrder == 3 ~ ((-0.008) * prec + 0.028 * temp + 0.44),
          strmOrder == 2 ~ ((-0.009) * prec + 0.029 * temp + 0.61),
          strmOrder == 1 ~ ((-0.009) * prec + 0.029 * temp + 0.77)),
        timedryout_ray13 = case_when(
          strmOrder > 4 ~ 0,
          strmOrder == 4 ~ ((-0.0028) * prec + 0.012 * temp + 0.127),
          strmOrder == 3 ~ ((-0.0018) * prec + 0.011 * temp + 0.058),
          strmOrder == 2 ~ ((-0.0021) * prec + 0.011 * temp + 0.088),
          strmOrder == 1 ~ ((-0.0019) * prec + 0.017 * temp + 0.026) )
      )
    #2
    df_basin_mon <-
      df_basin_mon %>% dplyr::mutate(
        percInterm_ray13 = case_when(
          percInterm_ray13 > 0.9 ~ 0.9,
          percInterm_ray13 <= 0.9 & percInterm_ray13 >= 0 ~ percInterm_ray13,
          percInterm_ray13 < 0 ~ 0 ),
        timedryout_ray13 = case_when(
          timedryout_ray13 > 0.9 ~ 0.9,
          (timedryout_ray13 <= 0.9) & timedryout_ray13 >= 0 ~ timedryout_ray13,
          timedryout_ray13 < 0 ~ 0 ),
        ephemArea_ray13 = percInterm_ray13 * timedryout_ray13 * Width * Length * isChannel / 1000000
      )
    
    #SO summary
    SOsum <-
      df_basin_mon %>% group_by(strmOrder) %>% dplyr::summarise(
        length_km = sum(Length * isChannel / 1000), #km
        width_m = mean(Width * isChannel),
        area_km2 = sum(surfArea),
        ephemArea_km2 = sum(ephemArea),
        ephemAreaRatio = ephemArea_km2 / area_km2,
        ephemArea_km2_ray13 = sum(ephemArea_ray13),
        ephemAreaRatio_ray13 = ephemArea_km2_ray13 / area_km2,
        k = sum(k * (surfArea - ephemArea), na.rm = TRUE) / area_km2,
        ch4 = sum(ch4 * (surfArea - ephemArea), na.rm = TRUE) / area_km2,
        ch4eq = sum(ch4eq * (surfArea - ephemArea), na.rm = TRUE) / area_km2
      )
    
    totEphemArea_1 <- sum(SOsum$ephemArea_km2)
    totEphemArea_1_ray13 <- sum(SOsum$ephemArea_km2_ray13)
    
    #extrapolation
    #use width extrapolation to find out endSO
    #predict width for non-captured SO
    fit <- lm(log(width_m) ~ strmOrder, data = SOsum[1:wkbasins[i, ]$SOabc, ])
    fitsum <- summary(fit)
    r2_width <- fitsum$r.squared
    intercept <- round(fit$coefficients[1], 5)
    slope <- round(fit$coefficients[2], 5)
    endSO <- (log(0.3) - intercept) / slope
    endSOi <- 0:ceiling(endSO)
    SO <- c(endSOi, endSO)
    # SO<-c(endSOi,floor(endSO))
    width_extrap <- data.frame(strmOrder = SO)
    width_extrap$width_m <- exp(predict(fit, width_extrap))
    
    #predict totLength for non-captured SOs
    #using endSO estimated from width extrapolation and length scaling relationship across SO
    fit <- lm(log(length_km) ~ strmOrder, data = SOsum[1:wkbasins[i, ]$SOabc, ])
    fitsum <- summary(fit)
    r2_length <- fitsum$r.squared
    length_extrap <- data.frame(strmOrder = SO)
    length_extrap$length_km <- exp(predict(fit, length_extrap))
    
    #extrapolate k
    fit <- lm(k ~ strmOrder, data = SOsum[1:wkbasins[i, ]$SOabc, ])
    fitsum <- summary(fit)
    r2_k <- fitsum$r.squared
    k_extrap <- data.frame(strmOrder = SO)
    if (fitsum$coefficients[2] > 0) {
      k_extrap$k <-
        mean(SOsum$k, na.rm = TRUE)
    } else{
      k_extrap$k <- predict(fit, k_extrap)
    }
    
    # find the largest non-zero SO for ephemeralness
    SOabc_e <- wkbasins[i, ]$SOabc
    while ((SOsum[SOsum$strmOrder %in% c(SOabc_e), ]$ephemAreaRatio == 0)) {
      SOabc_e <- SOabc_e - 1
      if (SOabc_e < 1) {
        break
      }
    }
    
    if(SOabc_e>=2) {
      fit <- lm(ephemAreaRatio ~ strmOrder, data = SOsum[1:SOabc_e, ])
      fitsum <- summary(fit)
      r2_ephemArea <- fitsum$r.squared
      emphemAreaRatio_extrap <- data.frame(strmOrder = SO)
      emphemAreaRatio_extrap$ephemAreaRatio <-
        predict(fit, emphemAreaRatio_extrap)
      totEphemArea_2 <-
        sum(
          emphemAreaRatio_extrap$ephemAreaRatio * width_extrap$width_m * length_extrap$length_km /
            1000
        )
    } else{
      r2_ephemArea <- NA
      totEphemArea_2 <- NA
      emphemAreaRatio_extrap <- data.frame(strmOrder = SO)
      emphemAreaRatio_extrap$ephemAreaRatio <- 0
    }
    
    if(SOabc_e>=2) {
      fit <- lm(ephemAreaRatio_ray13 ~ strmOrder, data = SOsum[1:SOabc_e, ])
      fitsum <- summary(fit)
      r2_ephemArea_ray13 <- fitsum$r.squared
      if (is.na(r2_ephemArea_ray13)) {
        r2_ephemArea_ray13 = 'No Ephem'
        emphemAreaRatio_ray13_extrap <- data.frame(strmOrder = SO)
        emphemAreaRatio_ray13_extrap$ephemAreaRatio_ray13 <- 0
        totEphemArea_2_ray13 = 0
      } else{
        emphemAreaRatio_ray13_extrap <- data.frame(strmOrder = SO)
        emphemAreaRatio_ray13_extrap$ephemAreaRatio_ray13 <-
          predict(fit, emphemAreaRatio_ray13_extrap)
        totEphemArea_2_ray13 <-
          sum(
            emphemAreaRatio_ray13_extrap$ephemAreaRatio_ray13 * width_extrap$width_m *
              length_extrap$length_km / 1000
          )
      }
    } else{
      r2_ephemArea_ray13 <- NA
      totEphemArea_2_ray13 <- NA
    }
    
    extrap<-cbind(wkbasins[i,]$wkbasin,width_extrap,length_extrap[,2],emphemAreaRatio_extrap[,2],emphemAreaRatio_ray13_extrap[,2],k_extrap[,2])
    names(extrap)<-c('wkbasin','strmOrder','width_m','length_km','ephemAreaRatio','ephemAreaRatio_ray13','k')
    extrap$wkbasin <- as.character(extrap$wkbasin)
    extrap['ch4'] <- SOsum[SOsum$strmOrder == 1, 'ch4']
    extrap['ch4eq'] <- SOsum[SOsum$strmOrder == 1, 'ch4eq']
    extrap <- extrap %>% mutate(
      area_km2 = width_m * length_km / 1000,
      ephemArea_km2 = area_km2 * ephemAreaRatio,
      ephemArea_km2_ray13 = area_km2 * ephemAreaRatio_ray13
    )
    #correct for negative effective area
    extrap[extrap$ephemArea_km2 > extrap$area_km2, ]$ephemArea_km2 <- extrap[extrap$ephemArea_km2 > extrap$area_km2, ]$area_km2
    extrap[extrap$ephemArea_km2_ray13 > extrap$area_km2, ]$ephemArea_km2_ray13 <- extrap[extrap$ephemArea_km2_ray13 > extrap$area_km2, ]$area_km2
    
    extrap <- extrap %>% 
      dplyr::mutate( effecArea_km2 = area_km2 - ephemArea_km2,
                     ch4F = (ch4-ch4eq) * k * 12 * 0.365, #gC/m2/yr
                     ch4E = (ch4-ch4eq) * k * effecArea_km2 * 365 * 12 / 1000000#GgC/yr
                    )
    
    
    totLength_2 <- sum(extrap$length_km) #total extrapolated flowline length
    totArea_2 <- sum(extrap$area_km2)# total extrapolated surface area
    totEphemArea_2 <- sum(extrap$ephemArea_km2)
    totEffectArea_2 <- sum(extrap$effecArea_km2)
    totEphemArea_2_ray13 <- sum(extrap$ephemArea_km2_ray13)
    if (sum(extrap$effecArea_km2 != 0)) {
      k_2 <-
        sum(extrap$k * extrap$effecArea_km2) / sum(extrap$effecArea_km2)
    } else{
      k_2 = 0
    }
    ch4_2 <- mean(extrap$ch4)
    ch4_eq_2 <- mean(extrap$ch4_eq)
    co2F_2 <-
      if (sum(extrap$effecArea_km2 != 0)) {
        sum(extrap$co2F * extrap$effecArea_km2) / totEffectArea_2
      } else{
        co2F_2 = 0
      }
    ch4E_2 <- sum(extrap$ch4E)
    
    basinMonArea<-data.frame(basinCode=wkbasins[i,]$wkbasin,Mon,
                             endSO,r2_width,r2_length,r2_ephemArea,r2_ephemArea_ray13,r2_k,totEphemArea_1,
                             totEphemArea_1_ray13,
                             totLength_2,totArea_2,totEphemArea_2,totEphemArea_2_ray13,totEffectArea_2,
                             k_2,ch4_2,ch4_eq_2,co2F_2,co2E_2)
    
    #combining results
    if(i==1 & Mon=='Jan'){basinArea<-basinMonArea} else {basinArea<-rbind(basinArea,basinMonArea)}
  }
}
rm(basinMonArea, df_basin,df_basin_mon,emphemAreaRatio_extrap,emphemAreaRatio_ray13_extrap,extrap,fit,fitsum,k_extrap,length_extrap,
   SOsum,width_extrap,co2_2,co2E_2,co2NM,dryoutNM,endSO,endSOi,i,intercept,k_2,kNM,Mon,precNM,totEphemArea_1_ray13,
   r2_ephemArea,r2_ephemArea_ray13,r2_k,r2_length,r2_width,slope,SO,SOabc_e,tempNM,totArea_2,totEphemArea_1,
   totEffectArea_2, totEphemArea_2,totEphemArea_2_ray13,totLength_2,widthNM,co2F_2,pco2NM,pco2_2)
# write_csv(basinArea,paste0(wd,'/output/table/regionSurfArea/basinArea.csv'))


####replacing yeaWidth with GRWL width where available####
GRWLwidth <-
  read_csv('data/raw/gis/upscaling_vars/GRWLwidthHydroBASIN4_30mplus.csv')
GRWLwidth <- GRWLwidth[, c('COMID', 'width_mean')]
GRWLwidth <- GRWLwidth %>% 
  group_by(COMID) %>% 
  dplyr::summarise(width_mean = min(width_mean))
GRWLwidth <- GRWLwidth[GRWLwidth$width_mean >= 90, ]
nrow(GRWLwidth) / nrow(df) #8.5%,4.3%

df_1 <- df[!df$COMID %in% GRWLwidth$COMID, ]
df_2 <- df[df$COMID %in% GRWLwidth$COMID, ]
rm(df)
gc()

df_2<-left_join(df_2,GRWLwidth,by='COMID')
df_2$rt<-df_2$width_mean/df_2$yeaWidth
if(sum(df_2$rt>2)){df_2[df_2$rt>2,]$rt=2}
# rtm<-median(df_2[df_2$rt<3,]$rt,na.rm=TRUE) #ratio(rt) too high (e.g.>3) is because misalignment.
# if(is.na(rtm)){rtm=1}
df_2<-
  df_2%>%dplyr::mutate(
    JanWidth=JanWidth*rt,
    FebWidth=FebWidth*rt,
    MarWidth=MarWidth*rt,
    AprWidth=AprWidth*rt,
    MayWidth=MayWidth*rt,
    JunWidth=JunWidth*rt,
    JulWidth=JulWidth*rt,
    AugWidth=AugWidth*rt,
    SepWidth=SepWidth*rt,
    OctWidth=OctWidth*rt,
    NovWidth=NovWidth*rt,
    DecWidth=DecWidth*rt)
df_2<-df_2[,!names(df_2)%in%c('yeaWidth')]
names(df_2)[names(df_2)%in%c('width_mean')]<-'yeaWidth'
df_2<-df_2[,names(df_1)]
if(sum(names(df_1)==names(df_2))==ncol(df_1)){
  df<-rbind(df_1,df_2)}else{print('GRWL width is not added sucessfully!')}
rm(df_1,df_2)
gc()

####join hydroBAS atts####
df<-df[!(df$HYBAS_ID%in%c("1040040050","2040059370","5040055420")),]#basins have no valid rivArea
df<-df[!is.na(df$Apr_k),]
#linking wkbasin to HydroSHEDS04 basins
hydroBAS<-df%>%group_by(HYBAS_ID)%>%dplyr::summarise(wkbasin=wkbasin[1])
names(hydroBAS)[2]<-'basinCode'
# write.csv(hydroBAS,paste0(wd,'/output/table/flowregime/hydroBAS.csv'))
#join basinCentroid
basinCentroid<-read_csv('data/raw/gis/upscaling_vars/hydrobasin4_centroid.csv',
                        col_types=cols(.default='d',HYBAS_ID='c'))
hydroBAS<-left_join(hydroBAS,basinCentroid,by='HYBAS_ID')
#give climate zones
hydroBAS<-
  hydroBAS%>%dplyr::mutate(climzone=case_when((Lat>56)~'Polar',
                                              (Lat<=56&Lat>23.5)~'North Temperate',
                                              (Lat<=23.5&Lat>=-23.5)~'Tropical',
                                              (Lat<=-23.5)~'South Temperate'))
hydroBAS$climzone<-factor(hydroBAS$climzone,levels=c('Polar','North Temperate','Tropical','South Temperate'))
#join basin area
basin04area<-read_csv('data/raw/gis/upscaling_vars/area04.csv', col_types='cd')#km2
names(basin04area)[2]<-'basinArea'
hydroBAS<-left_join(hydroBAS,basin04area,by='HYBAS_ID')
rm(basin04area)
#sum up basin areas belonging to the same WKbasin
WKbasinArea<-hydroBAS%>%group_by(basinCode)%>%dplyr::summarise(wkbasinArea=sum(basinArea))
hydroBAS<-left_join(hydroBAS,WKbasinArea,by="basinCode")
hydroBAS$areaRatio<-hydroBAS$basinArea/hydroBAS$wkbasinArea
rm(WKbasinArea)
#join prectemp
prectemp<-read_csv('data/raw/gis/upscaling_vars/tempPrep.csv',
                   col_types=cols(.default='d',HYBAS_ID='c'))
prectemp<-prectemp[,c('HYBAS_ID','annTemp','annPrec')] #degree celcius, mm/yr
hydroBAS<-left_join(hydroBAS,prectemp,by='HYBAS_ID')
rm(prectemp)
#join runoff
runoff<-read_csv('data/raw/gis/upscaling_vars/runoffhydrobasin4.csv'),
col_types=cols(.default='d',HYBAS_ID='c'))
runoff<-runoff%>%dplyr::mutate(
  wetness=case_when(runoff<50~'Arid',
                    runoff>=50&runoff<500~'Mod',
                    runoff>=500~'Wet')
)
hydroBAS<-left_join(hydroBAS,runoff,by='HYBAS_ID')
rm(runoff)

# #join ann gpp npp
# anngppnpp<-read_csv('/output/table/HydroBASINSatts/annPP.csv'),col_types=cols(.default='d',HYBAS_ID='c'))
# hydroBAS<-left_join(hydroBAS,anngppnpp,by='HYBAS_ID')
# rm(anngppnpp)
# #join mon gpp npp
# mongppnpp<-read_csv(paste0(wd,'/output/table/HydroBASINSatts/monPP_new.csv'),col_types=cols(.default='d',HYBAS_ID='c'))
# hydroBAS<-left_join(hydroBAS,mongppnpp,by='HYBAS_ID')
# rm(mongppnpp)
# #join soil respiration
# soilResp<-read_csv(paste0(wd,'/output/table/HydroBASINSatts/soilResp_buffer.csv'),col_types=cols(.default='d',HYBAS_ID='c'))
# #gCm-2yr-1
# soilResp[,paste0('pRS_',str_pad(1:12,2,pad='0'))]<-sapply(soilResp[,paste0('pRS_',str_pad(1:12,2,pad='0'))],function(x){x*365})
# hydroBAS<-left_join(hydroBAS,soilResp,by='HYBAS_ID')
# rm(soilResp)
####join part1####
#join pc02_1
pco2_1<-
  df%>%group_by(HYBAS_ID)%>%
  dplyr::summarise(pco2_Jan=sum(Jan_pco2*Length*JanWidth/1000000*(1-Jantimedryout)*(JanWidth>=0.3))/
                     sum(Length*JanWidth/1000000*(1-Jantimedryout)*(JanWidth>=0.3)),
                   pco2_Feb=sum(Feb_pco2*Length*FebWidth/1000000*(1-Febtimedryout)*(FebWidth>=0.3))/
                     sum(Length*FebWidth/1000000*(1-Febtimedryout)*(FebWidth>=0.3)),
                   pco2_Mar=sum(Mar_pco2*Length*MarWidth/1000000*(1-Martimedryout)*(MarWidth>=0.3))/
                     sum(Length*MarWidth/1000000*(1-Martimedryout)*(MarWidth>=0.3)),
                   pco2_Apr=sum(Apr_pco2*Length*AprWidth/1000000*(1-Aprtimedryout)*(AprWidth>=0.3))/
                     sum(Length*AprWidth/1000000*(1-Aprtimedryout)*(AprWidth>=0.3)),
                   pco2_May=sum(May_pco2*Length*MayWidth/1000000*(1-Maytimedryout)*(MayWidth>=0.3))/
                     sum(Length*MayWidth/1000000*(1-Maytimedryout)*(MayWidth>=0.3)),
                   pco2_Jun=sum(Jun_pco2*Length*JunWidth/1000000*(1-Juntimedryout)*(JunWidth>=0.3))/
                     sum(Length*JunWidth/1000000*(1-Juntimedryout)*(JunWidth>=0.3)),
                   pco2_Jul=sum(Jul_pco2*Length*JulWidth/1000000*(1-Jultimedryout)*(JulWidth>=0.3))/
                     sum(Length*JulWidth/1000000*(1-Jultimedryout)*(JulWidth>=0.3)),
                   pco2_Aug=sum(Aug_pco2*Length*AugWidth/1000000*(1-Augtimedryout)*(AugWidth>=0.3))/
                     sum(Length*AugWidth/1000000*(1-Augtimedryout)*(AugWidth>=0.3)),
                   pco2_Sep=sum(Sep_pco2*Length*SepWidth/1000000*(1-Septimedryout)*(SepWidth>=0.3))/
                     sum(Length*SepWidth/1000000*(1-Septimedryout)*(SepWidth>=0.3)),
                   pco2_Oct=sum(Oct_pco2*Length*OctWidth/1000000*(1-Octtimedryout)*(OctWidth>=0.3))/
                     sum(Length*OctWidth/1000000*(1-Octtimedryout)*(OctWidth>=0.3)),
                   pco2_Nov=sum(Nov_pco2*Length*NovWidth/1000000*(1-Novtimedryout)*(NovWidth>=0.3))/
                     sum(Length*NovWidth/1000000*(1-Novtimedryout)*(NovWidth>=0.3)),
                   pco2_Dec=sum(Dec_pco2*Length*DecWidth/1000000*(1-Dectimedryout)*(DecWidth>=0.3))/
                     sum(Length*DecWidth/1000000*(1-Dectimedryout)*(DecWidth>=0.3)))
hydroBAS_res1<-left_join(hydroBAS,pco2_1,by='HYBAS_ID')
rm(pco2_1)
gc()

#k_1
k_1<-
  df%>%group_by(HYBAS_ID)%>%
  dplyr::summarise(k_Jan=sum(Jan_k*Length*JanWidth/1000000*(1-Jantimedryout)*(JanWidth>=0.3))/
                     sum(Length*JanWidth/1000000*(1-Jantimedryout)*(JanWidth>=0.3)),
                   k_Feb=sum(Feb_k*Length*FebWidth/1000000*(1-Febtimedryout)*(FebWidth>=0.3))/
                     sum(Length*FebWidth/1000000*(1-Febtimedryout)*(FebWidth>=0.3)),
                   k_Mar=sum(Mar_k*Length*MarWidth/1000000*(1-Martimedryout)*(MarWidth>=0.3))/
                     sum(Length*MarWidth/1000000*(1-Martimedryout)*(MarWidth>=0.3)),
                   k_Apr=sum(Apr_k*Length*AprWidth/1000000*(1-Aprtimedryout)*(AprWidth>=0.3))/
                     sum(Length*AprWidth/1000000*(1-Aprtimedryout)*(AprWidth>=0.3)),
                   k_May=sum(May_k*Length*MayWidth/1000000*(1-Maytimedryout)*(MayWidth>=0.3))/
                     sum(Length*MayWidth/1000000*(1-Maytimedryout)*(MayWidth>=0.3)),
                   k_Jun=sum(Jun_k*Length*JunWidth/1000000*(1-Juntimedryout)*(JunWidth>=0.3))/
                     sum(Length*JunWidth/1000000*(1-Juntimedryout)*(JunWidth>=0.3)),
                   k_Jul=sum(Jul_k*Length*JulWidth/1000000*(1-Jultimedryout)*(JulWidth>=0.3))/
                     sum(Length*JulWidth/1000000*(1-Jultimedryout)*(JulWidth>=0.3)),
                   k_Aug=sum(Aug_k*Length*AugWidth/1000000*(1-Augtimedryout)*(AugWidth>=0.3))/
                     sum(Length*AugWidth/1000000*(1-Augtimedryout)*(AugWidth>=0.3)),
                   k_Sep=sum(Sep_k*Length*SepWidth/1000000*(1-Septimedryout)*(SepWidth>=0.3))/
                     sum(Length*SepWidth/1000000*(1-Septimedryout)*(SepWidth>=0.3)),
                   k_Oct=sum(Oct_k*Length*OctWidth/1000000*(1-Octtimedryout)*(OctWidth>=0.3))/
                     sum(Length*OctWidth/1000000*(1-Octtimedryout)*(OctWidth>=0.3)),
                   k_Nov=sum(Nov_k*Length*NovWidth/1000000*(1-Novtimedryout)*(NovWidth>=0.3))/
                     sum(Length*NovWidth/1000000*(1-Novtimedryout)*(NovWidth>=0.3)),
                   k_Dec=sum(Dec_k*Length*DecWidth/1000000*(1-Dectimedryout)*(DecWidth>=0.3))/
                     sum(Length*DecWidth/1000000*(1-Dectimedryout)*(DecWidth>=0.3)))
hydroBAS_res1<-left_join(hydroBAS_res1,k_1,by='HYBAS_ID')
rm(k_1)
gc()

#co2F_1
co2F_1<-
  df%>%group_by(HYBAS_ID)%>%
  dplyr::summarise(co2F_Jan=sum(Jan_co2*Jan_k*365*12/1000*Length*JanWidth/1000000*(1-Jantimedryout)*(JanWidth>=0.3))/
                     sum(Length*JanWidth/1000000*(1-Jantimedryout)*(JanWidth>=0.3)),
                   co2F_Feb=sum(Feb_co2*Feb_k*365*12/1000*Length*FebWidth/1000000*(1-Febtimedryout)*(FebWidth>=0.3))/
                     sum(Length*FebWidth/1000000*(1-Febtimedryout)*(FebWidth>=0.3)),
                   co2F_Mar=sum(Mar_co2*Mar_k*365*12/1000*Length*MarWidth/1000000*(1-Martimedryout)*(MarWidth>=0.3))/
                     sum(Length*MarWidth/1000000*(1-Martimedryout)*(MarWidth>=0.3)),
                   co2F_Apr=sum(Apr_co2*Apr_k*365*12/1000*Length*AprWidth/1000000*(1-Aprtimedryout)*(AprWidth>=0.3))/
                     sum(Length*AprWidth/1000000*(1-Aprtimedryout)*(AprWidth>=0.3)),
                   co2F_May=sum(May_co2*May_k*365*12/1000*Length*MayWidth/1000000*(1-Maytimedryout)*(MayWidth>=0.3))/
                     sum(Length*MayWidth/1000000*(1-Maytimedryout)*(MayWidth>=0.3)),
                   co2F_Jun=sum(Jun_co2*Jun_k*365*12/1000*Length*JunWidth/1000000*(1-Juntimedryout)*(JunWidth>=0.3))/
                     sum(Length*JunWidth/1000000*(1-Juntimedryout)*(JunWidth>=0.3)),
                   co2F_Jul=sum(Jul_co2*Jul_k*365*12/1000*Length*JulWidth/1000000*(1-Jultimedryout)*(JulWidth>=0.3))/
                     sum(Length*JulWidth/1000000*(1-Jultimedryout)*(JulWidth>=0.3)),
                   co2F_Aug=sum(Aug_co2*Aug_k*365*12/1000*Length*AugWidth/1000000*(1-Augtimedryout)*(AugWidth>=0.3))/
                     sum(Length*AugWidth/1000000*(1-Augtimedryout)*(AugWidth>=0.3)),
                   co2F_Sep=sum(Sep_co2*Sep_k*365*12/1000*Length*SepWidth/1000000*(1-Septimedryout)*(SepWidth>=0.3))/
                     sum(Length*SepWidth/1000000*(1-Septimedryout)*(SepWidth>=0.3)),
                   co2F_Oct=sum(Oct_co2*Oct_k*365*12/1000*Length*OctWidth/1000000*(1-Octtimedryout)*(OctWidth>=0.3))/
                     sum(Length*OctWidth/1000000*(1-Octtimedryout)*(OctWidth>=0.3)),
                   co2F_Nov=sum(Nov_co2*Nov_k*365*12/1000*Length*NovWidth/1000000*(1-Novtimedryout)*(NovWidth>=0.3))/
                     sum(Length*NovWidth/1000000*(1-Novtimedryout)*(NovWidth>=0.3)),
                   co2F_Dec=sum(Dec_co2*Dec_k*365*12/1000*Length*DecWidth/1000000*(1-Dectimedryout)*(DecWidth>=0.3))/
                     sum(Length*DecWidth/1000000*(1-Dectimedryout)*(DecWidth>=0.3)))
hydroBAS_res1<-left_join(hydroBAS_res1,co2F_1,by='HYBAS_ID')
rm(co2F_1)
gc()

#join totArea_1
totArea_1<-
  df%>%group_by(HYBAS_ID)%>%
  dplyr::summarise(rivArea_Jan=sum(Length*JanWidth/1000000*(JanWidth>=0.3)),
                   rivArea_Feb=sum(Length*FebWidth/1000000*(FebWidth>=0.3)),
                   rivArea_Mar=sum(Length*MarWidth/1000000*(MarWidth>=0.3)),
                   rivArea_Apr=sum(Length*AprWidth/1000000*(AprWidth>=0.3)),
                   rivArea_May=sum(Length*MayWidth/1000000*(MayWidth>=0.3)),
                   rivArea_Jun=sum(Length*JunWidth/1000000*(JunWidth>=0.3)),
                   rivArea_Jul=sum(Length*JulWidth/1000000*(JulWidth>=0.3)),
                   rivArea_Aug=sum(Length*AugWidth/1000000*(AugWidth>=0.3)),
                   rivArea_Sep=sum(Length*SepWidth/1000000*(SepWidth>=0.3)),
                   rivArea_Oct=sum(Length*OctWidth/1000000*(OctWidth>=0.3)),
                   rivArea_Nov=sum(Length*NovWidth/1000000*(NovWidth>=0.3)),
                   rivArea_Dec=sum(Length*DecWidth/1000000*(DecWidth>=0.3)))
hydroBAS_res1<-left_join(hydroBAS_res1,totArea_1,by='HYBAS_ID')
rm(totArea_1)
gc()

#Join ephemArea_1
ephemArea_1<-
  df%>%group_by(HYBAS_ID)%>%
  dplyr::summarise(ephemArea_Jan=sum(Length*JanWidth/1000000*(Jantimedryout)*(JanWidth>=0.3)),
                   ephemArea_Feb=sum(Length*FebWidth/1000000*(Febtimedryout)*(FebWidth>=0.3)),
                   ephemArea_Mar=sum(Length*MarWidth/1000000*(Martimedryout)*(MarWidth>=0.3)),
                   ephemArea_Apr=sum(Length*AprWidth/1000000*(Aprtimedryout)*(AprWidth>=0.3)),
                   ephemArea_May=sum(Length*MayWidth/1000000*(Maytimedryout)*(MayWidth>=0.3)),
                   ephemArea_Jun=sum(Length*JunWidth/1000000*(Juntimedryout)*(JunWidth>=0.3)),
                   ephemArea_Jul=sum(Length*JulWidth/1000000*(Jultimedryout)*(JulWidth>=0.3)),
                   ephemArea_Aug=sum(Length*AugWidth/1000000*(Augtimedryout)*(AugWidth>=0.3)),
                   ephemArea_Sep=sum(Length*SepWidth/1000000*(Septimedryout)*(SepWidth>=0.3)),
                   ephemArea_Oct=sum(Length*OctWidth/1000000*(Octtimedryout)*(OctWidth>=0.3)),
                   ephemArea_Nov=sum(Length*NovWidth/1000000*(Novtimedryout)*(NovWidth>=0.3)),
                   ephemArea_Dec=sum(Length*DecWidth/1000000*(Dectimedryout)*(DecWidth>=0.3)))
hydroBAS_res1<-left_join(hydroBAS_res1,ephemArea_1,by='HYBAS_ID')
rm(ephemArea_1)
gc()

#EffectArea_1
effectArea_1<-
  df%>%group_by(HYBAS_ID)%>%
  dplyr::summarise(effectArea_Jan=sum(Length*JanWidth/1000000*(1-Jantimedryout)*(JanWidth>=0.3)),
                   effectArea_Feb=sum(Length*FebWidth/1000000*(1-Febtimedryout)*(FebWidth>=0.3)),
                   effectArea_Mar=sum(Length*MarWidth/1000000*(1-Martimedryout)*(MarWidth>=0.3)),
                   effectArea_Apr=sum(Length*AprWidth/1000000*(1-Aprtimedryout)*(AprWidth>=0.3)),
                   effectArea_May=sum(Length*MayWidth/1000000*(1-Maytimedryout)*(MayWidth>=0.3)),
                   effectArea_Jun=sum(Length*JunWidth/1000000*(1-Juntimedryout)*(JunWidth>=0.3)),
                   effectArea_Jul=sum(Length*JulWidth/1000000*(1-Jultimedryout)*(JulWidth>=0.3)),
                   effectArea_Aug=sum(Length*AugWidth/1000000*(1-Augtimedryout)*(AugWidth>=0.3)),
                   effectArea_Sep=sum(Length*SepWidth/1000000*(1-Septimedryout)*(SepWidth>=0.3)),
                   effectArea_Oct=sum(Length*OctWidth/1000000*(1-Octtimedryout)*(OctWidth>=0.3)),
                   effectArea_Nov=sum(Length*NovWidth/1000000*(1-Novtimedryout)*(NovWidth>=0.3)),
                   effectArea_Dec=sum(Length*DecWidth/1000000*(1-Dectimedryout)*(DecWidth>=0.3)))
#get ice coverage
icecov<-read_csv('data/raw/gis/upscaling_vars/iceout.csv',
                 col_types=cols(.default='d',HYBAS_ID='c'))
names(icecov)<-c('HYBAS_ID',paste0('iceCov_',month.abb))
effectArea_1<-left_join(effectArea_1,icecov,by='HYBAS_ID')
#calculate iceCovered area
effectArea_1<-
  effectArea_1%>%dplyr::mutate(
    icecovArea_Jan=effectArea_Jan*iceCov_Jan,
    icecovArea_Feb=effectArea_Feb*iceCov_Feb,
    icecovArea_Mar=effectArea_Mar*iceCov_Mar,
    icecovArea_Apr=effectArea_Apr*iceCov_Apr,
    icecovArea_May=effectArea_May*iceCov_May,
    icecovArea_Jun=effectArea_Jun*iceCov_Jun,
    icecovArea_Jul=effectArea_Jul*iceCov_Jul,
    icecovArea_Aug=effectArea_Aug*iceCov_Aug,
    icecovArea_Sep=effectArea_Sep*iceCov_Sep,
    icecovArea_Oct=effectArea_Oct*iceCov_Oct,
    icecovArea_Nov=effectArea_Nov*iceCov_Nov,
    icecovArea_Dec=effectArea_Dec*iceCov_Dec,
    effectArea_Jan=effectArea_Jan-icecovArea_Jan,
    effectArea_Feb=effectArea_Feb-icecovArea_Feb,
    effectArea_Mar=effectArea_Mar-icecovArea_Mar,
    effectArea_Apr=effectArea_Apr-icecovArea_Apr,
    effectArea_May=effectArea_May-icecovArea_May,
    effectArea_Jun=effectArea_Jun-icecovArea_Jun,
    effectArea_Jul=effectArea_Jul-icecovArea_Jul,
    effectArea_Aug=effectArea_Aug-icecovArea_Aug,
    effectArea_Sep=effectArea_Sep-icecovArea_Sep,
    effectArea_Oct=effectArea_Oct-icecovArea_Oct,
    effectArea_Nov=effectArea_Nov-icecovArea_Nov,
    effectArea_Dec=effectArea_Dec-icecovArea_Dec
  )
#join iceCovArea and effectArea
hydroBAS_res1<-left_join(hydroBAS_res1,effectArea_1[,c('HYBAS_ID',paste0('icecovArea_',month.abb),
                                                       paste0('effectArea_',month.abb))],by='HYBAS_ID')

rm(effectArea_1)
gc()

#join co2E_1,unit: 10^9gCyr-1
co2E_1<-
  df%>%group_by(HYBAS_ID)%>%
  dplyr::summarise(co2E_Jan=sum(Jan_co2*Jan_k*365*12/1000*Length*JanWidth/1000000000*(1-Jantimedryout)*(JanWidth>=0.3)),
                   co2E_Feb=sum(Feb_co2*Feb_k*365*12/1000*Length*FebWidth/1000000000*(1-Febtimedryout)*(FebWidth>=0.3)),
                   co2E_Mar=sum(Mar_co2*Mar_k*365*12/1000*Length*MarWidth/1000000000*(1-Martimedryout)*(MarWidth>=0.3)),
                   co2E_Apr=sum(Apr_co2*Apr_k*365*12/1000*Length*AprWidth/1000000000*(1-Aprtimedryout)*(AprWidth>=0.3)),
                   co2E_May=sum(May_co2*May_k*365*12/1000*Length*MayWidth/1000000000*(1-Maytimedryout)*(MayWidth>=0.3)),
                   co2E_Jun=sum(Jun_co2*Jun_k*365*12/1000*Length*JunWidth/1000000000*(1-Juntimedryout)*(JunWidth>=0.3)),
                   co2E_Jul=sum(Jul_co2*Jul_k*365*12/1000*Length*JulWidth/1000000000*(1-Jultimedryout)*(JulWidth>=0.3)),
                   co2E_Aug=sum(Aug_co2*Aug_k*365*12/1000*Length*AugWidth/1000000000*(1-Augtimedryout)*(AugWidth>=0.3)),
                   co2E_Sep=sum(Sep_co2*Sep_k*365*12/1000*Length*SepWidth/1000000000*(1-Septimedryout)*(SepWidth>=0.3)),
                   co2E_Oct=sum(Oct_co2*Oct_k*365*12/1000*Length*OctWidth/1000000000*(1-Octtimedryout)*(OctWidth>=0.3)),
                   co2E_Nov=sum(Nov_co2*Nov_k*365*12/1000*Length*NovWidth/1000000000*(1-Novtimedryout)*(NovWidth>=0.3)),
                   co2E_Dec=sum(Dec_co2*Dec_k*365*12/1000*Length*DecWidth/1000000000*(1-Dectimedryout)*(DecWidth>=0.3)))
#correct for ice cover
co2E_1<-left_join(co2E_1,icecov,by='HYBAS_ID')
co2E_1<-co2E_1%>%dplyr::mutate(
  co2E_Jan=co2E_Jan*(1-iceCov_Jan),
  co2E_Feb=co2E_Feb*(1-iceCov_Feb),
  co2E_Mar=co2E_Mar*(1-iceCov_Mar),
  co2E_Apr=co2E_Apr*(1-iceCov_Apr),
  co2E_May=co2E_May*(1-iceCov_May),
  co2E_Jun=co2E_Jun*(1-iceCov_Jun),
  co2E_Jul=co2E_Jul*(1-iceCov_Jul),
  co2E_Aug=co2E_Aug*(1-iceCov_Aug),
  co2E_Sep=co2E_Sep*(1-iceCov_Sep),
  co2E_Oct=co2E_Oct*(1-iceCov_Oct),
  co2E_Nov=co2E_Nov*(1-iceCov_Nov),
  co2E_Dec=co2E_Dec*(1-iceCov_Dec)
)
hydroBAS_res1<-left_join(hydroBAS_res1,co2E_1[,1:13],by='HYBAS_ID')
rm(co2E_1)
gc()

####join part2####
#join res2
cols = c(
  'pco2_2',
  'k_2',
  'co2F_2',
  'totArea_2',
  'totEphemArea_2',
  'totEffectArea_2',
  'co2E_2'
)
basinArea_h <- pivot_wider(
  basinArea[, c('basinCode', 'Mon', cols)],
  id_cols = basinCode,
  names_from = Mon,
  values_from = cols
)
basinArea_h$basinCode <- as.character(basinArea_h$basinCode)
hydroBAS_res2 <- left_join(hydroBAS, basinArea_h, by = c('basinCode'))
#correcting names
hydroBAS_res2 <-
  hydroBAS_res2 %>% rename_at(vars(starts_with('pco2_2')), funs(str_replace(., 'pco2_2', 'pco2')))
hydroBAS_res2 <-
  hydroBAS_res2 %>% rename_at(vars(starts_with('k_2')), funs(str_replace(., 'k_2', 'k')))
hydroBAS_res2 <-
  hydroBAS_res2 %>% rename_at(vars(starts_with('co2F_2')), funs(str_replace(., 'co2F_2', 'co2F')))
hydroBAS_res2 <-
  hydroBAS_res2 %>% rename_at(vars(starts_with('totArea_2')), funs(str_replace(., 'totArea_2', 'rivArea')))
hydroBAS_res2 <-
  hydroBAS_res2 %>% rename_at(vars(starts_with('totEphemArea_2')), funs(str_replace(., 'totEphemArea_2', 'ephemArea')))
hydroBAS_res2 <-
  hydroBAS_res2 %>% rename_at(vars(starts_with('totEffectArea_2')), funs(str_replace(., 'totEffectArea_2', 'effectArea')))
hydroBAS_res2 <-
  hydroBAS_res2 %>% rename_at(vars(starts_with('co2E_2')), funs(str_replace(., 'co2E_2', 'co2E')))
#using areaRatio to partition rivArea,ephemArea,icecovArea,and co2E
for (Mon in month.abb) {
  hydroBAS_res2[, paste0('rivArea_', Mon)] <-
    hydroBAS_res2$areaRatio * hydroBAS_res2[, paste0('rivArea_', Mon)]
}
for (Mon in month.abb) {
  hydroBAS_res2[, paste0('ephemArea_', Mon)] <-
    hydroBAS_res2$areaRatio * hydroBAS_res2[, paste0('ephemArea_', Mon)]
}
for (Mon in month.abb) {
  hydroBAS_res2[, paste0('effectArea_', Mon)] <-
    hydroBAS_res2$areaRatio * hydroBAS_res2[, paste0('effectArea_', Mon)]
}
for (Mon in month.abb) {
  hydroBAS_res2[, paste0('co2E_', Mon)] <-
    hydroBAS_res2$areaRatio * hydroBAS_res2[, paste0('co2E_', Mon)]
}
#correcting for ice covered area and co2E for part 2
hydroBAS_res2 <- left_join(hydroBAS_res2, icecov, by = 'HYBAS_ID')
#calculate iceCovered area
hydroBAS_res2<-
  hydroBAS_res2%>%dplyr::mutate(
    icecovArea_Jan = effectArea_Jan * iceCov_Jan,
    icecovArea_Feb = effectArea_Feb * iceCov_Feb,
    icecovArea_Mar = effectArea_Mar * iceCov_Mar,
    icecovArea_Apr = effectArea_Apr * iceCov_Apr,
    icecovArea_May = effectArea_May * iceCov_May,
    icecovArea_Jun = effectArea_Jun * iceCov_Jun,
    icecovArea_Jul = effectArea_Jul * iceCov_Jul,
    icecovArea_Aug = effectArea_Aug * iceCov_Aug,
    icecovArea_Sep = effectArea_Sep * iceCov_Sep,
    icecovArea_Oct = effectArea_Oct * iceCov_Oct,
    icecovArea_Nov = effectArea_Nov * iceCov_Nov,
    icecovArea_Dec = effectArea_Dec * iceCov_Dec,
    effectArea_Jan = effectArea_Jan - icecovArea_Jan,
    effectArea_Feb = effectArea_Feb - icecovArea_Feb,
    effectArea_Mar = effectArea_Mar - icecovArea_Mar,
    effectArea_Apr = effectArea_Apr - icecovArea_Apr,
    effectArea_May = effectArea_May - icecovArea_May,
    effectArea_Jun = effectArea_Jun - icecovArea_Jun,
    effectArea_Jul = effectArea_Jul - icecovArea_Jul,
    effectArea_Aug = effectArea_Aug - icecovArea_Aug,
    effectArea_Sep = effectArea_Sep - icecovArea_Sep,
    effectArea_Oct = effectArea_Oct - icecovArea_Oct,
    effectArea_Nov = effectArea_Nov - icecovArea_Nov,
    effectArea_Dec = effectArea_Dec - icecovArea_Dec,
    co2E_Jan = co2E_Jan * (1 - iceCov_Jan),
    co2E_Feb = co2E_Feb * (1 - iceCov_Feb),
    co2E_Mar = co2E_Mar * (1 - iceCov_Mar),
    co2E_Apr = co2E_Apr * (1 - iceCov_Apr),
    co2E_May = co2E_May * (1 - iceCov_May),
    co2E_Jun = co2E_Jun * (1 - iceCov_Jun),
    co2E_Jul = co2E_Jul * (1 - iceCov_Jul),
    co2E_Aug = co2E_Aug * (1 - iceCov_Aug),
    co2E_Sep = co2E_Sep * (1 - iceCov_Sep),
    co2E_Oct = co2E_Oct * (1 - iceCov_Oct),
    co2E_Nov = co2E_Nov * (1 - iceCov_Nov),
    co2E_Dec = co2E_Dec * (1 - iceCov_Dec)
  )
# write_csv(hydroBAS_res2[,c('HYBAS_ID','basinCode',paste0('co2E_',month.abb))],
#           paste0(wd,'/output/table/MeritHydro/co2E_extrap_hybas.csv'))
#combining res1 and res2
#for rivArea, ephemArea, icecovArea, effect area and co2E, just sum up
for (Mon in month.abb) {
  hydroBAS[, paste0('rivArea_', Mon)] <-
    hydroBAS_res1[, paste0('rivArea_', Mon)] + hydroBAS_res2[, paste0('rivArea_', Mon)]
}
for (Mon in month.abb) {
  hydroBAS[, paste0('ephemArea_', Mon)] <-
    hydroBAS_res1[, paste0('ephemArea_', Mon)] + hydroBAS_res2[, paste0('ephemArea_', Mon)]
}
for (Mon in month.abb) {
  hydroBAS[, paste0('icecovArea_', Mon)] <-
    hydroBAS_res1[, paste0('icecovArea_', Mon)] + hydroBAS_res2[, paste0('icecovArea_', Mon)]
}
for (Mon in month.abb) {
  hydroBAS[, paste0('effectArea_', Mon)] <-
    hydroBAS_res1[, paste0('effectArea_', Mon)] + hydroBAS_res2[, paste0('effectArea_', Mon)]
}
for (Mon in month.abb) {
  hydroBAS[, paste0('co2E_', Mon)] <-
    hydroBAS_res1[, paste0('co2E_', Mon)] + hydroBAS_res2[, paste0('co2E_', Mon)]
}

#for pco2,k and co2F, it weighted avgs
for (Mon in month.abb) {
  hydroBAS[, paste0('pco2_', Mon)] <-
    (hydroBAS_res1[, paste0('effectArea_', Mon)] * hydroBAS_res1[, paste0('pco2_', Mon)] +
       hydroBAS_res2[, paste0('effectArea_', Mon)] *
       hydroBAS_res2[, paste0('pco2_', Mon)]) / hydroBAS[, paste0('effectArea_', Mon)]
}
for (Mon in month.abb) {
  hydroBAS[, paste0('k_', Mon)] <-
    (hydroBAS_res1[, paste0('effectArea_', Mon)] * hydroBAS_res1[, paste0('k_', Mon)] +
       hydroBAS_res2[, paste0('effectArea_', Mon)] * hydroBAS_res2[, paste0('k_', Mon)]) /
    hydroBAS[, paste0('effectArea_', Mon)]
}
for (Mon in month.abb) {
  hydroBAS[, paste0('co2F_', Mon)] <-
    (hydroBAS_res1[, paste0('effectArea_', Mon)] * hydroBAS_res1[, paste0('co2F_', Mon)] +
       hydroBAS_res2[, paste0('effectArea_', Mon)] * hydroBAS_res2[, paste0('co2F_', Mon)]) /
    hydroBAS[, paste0('effectArea_', Mon)]
}
#correcting for basins where there is no effective area
for (Mon in month.abb) {
  hydroBAS[hydroBAS[, paste0('effectArea_', Mon)] == 0, paste0('pco2_', Mon)] <- 0
  hydroBAS[hydroBAS[, paste0('effectArea_', Mon)] == 0, paste0('k_', Mon)] <- 0
  hydroBAS[hydroBAS[, paste0('effectArea_', Mon)] == 0, paste0('co2F_', Mon)] <- 0
}
# rm(hydroBAS_res1,hydroBAS_res2,icecov,basinArea_h,GRWLwidth)
gc()
write_csv(hydroBAS,paste0(wd,'/output/table/regionSurfArea/co2E_hybas.csv'))








