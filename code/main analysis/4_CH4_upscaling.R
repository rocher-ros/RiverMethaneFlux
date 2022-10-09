########################################.
#### Script upscale CH4 emissions in rivers globally.
#### Author:  Gerard Rocher-Ros, modified from Shaoda Liu
#### Last edit: 2022-09-10
########################################.

# Load  and install packages and functions ----
# List of all packages needed
package_list <- c('tidyverse')

# Check if there are any packacges missing
packages_missing <- setdiff(package_list, rownames(installed.packages()))

# If we find a package missing, install them
if(length(packages_missing) >= 1) install.packages(packages_missing) 

# Now load all the packages
lapply(package_list, require, character.only = TRUE)


# Function to calculate gas saturation, 
# from temperature ( in Kelvin) and atm pressure (derived from elevation (in m.a.s.l) for CH4 
# Henry's Law constants from http://www.henrys-law.org
# Temperature correction using van't Hoff equation , temperature is in (Kelvin)
# Kh is in ("Kh, cp") = (mol*L^-1* Atm^-1) at STP
# Concentrations (mole fraction, also ppmv) in the atmosphere are approximate for 2013, should be updated over time
# AtmP is in (atmospheres)

get_Ch4eq <- function(temperature, elevation){
  
  pressure <- (1-(.0000225577*elevation))^5.25588
  Kh <- (1.4*10^-3)*exp(1700*((1/temperature)-(1/298)))
  AtmosphereConc <-  1.83
  EquilSaturation <- AtmosphereConc*Kh*pressure #umol/L, mmol/m3
  
  return(EquilSaturation)
}

# function to estimate dryout time based on Q, source: https://doi.org/10.1073/pnas.2106322119
timedryout_calculater <- function(Q) {
  tdryout = 1 / (1 + exp(11 + 4 * log(Q)))
}

# Load files ----

# load file with discharge and k data 
hydro_k <- read_csv("data/processed/q_and_k.csv") #%>%  
  # Uncomment to cap fluxes with a k threshold based on Ulseth et al. 2018
   #mutate(across(ends_with("_k"), ~ifelse( .x > 35, 35, .x)))

#upscaled methane concentrations
#mean estimates are "month_ch4", while SD are "month_ch4_sd"
meth_concs <- lapply(list.files(path = "data/processed/meth_preds/", pattern = "ch4_preds_uncertainty", full.names = TRUE), 
                     read_csv) %>% 
  purrr::reduce(left_join, by = "COMID") %>% 
  rename_with( ~str_replace(.x, "mean", "ch4")) %>% 
  rename_with( ~str_replace(.x, "sd", "ch4_sd"))

gc()

# Calculate excess methane in water for flux calcs ----

# calculate excess ch4 relative to the atmosphere, based on water temperature and elevation
df <- hydro_k %>% 
  left_join(meth_concs, by="COMID") %>% 
  mutate(Jan_ch4ex = Jan_ch4 - get_Ch4eq(Jan_Tw + 273.15, elev),
         Feb_ch4ex = Feb_ch4 - get_Ch4eq(Feb_Tw + 273.15, elev),
         Mar_ch4ex = Mar_ch4 - get_Ch4eq(Mar_Tw + 273.15, elev),
         Apr_ch4ex = Apr_ch4 - get_Ch4eq(Apr_Tw + 273.15, elev),
         May_ch4ex = May_ch4 - get_Ch4eq(May_Tw + 273.15, elev),
         Jun_ch4ex = Jun_ch4 - get_Ch4eq(Jun_Tw + 273.15, elev),
         Jul_ch4ex = Jul_ch4 - get_Ch4eq(Jul_Tw + 273.15, elev),
         Aug_ch4ex = Aug_ch4 - get_Ch4eq(Aug_Tw + 273.15, elev),
         Sep_ch4ex = Sep_ch4 - get_Ch4eq(Sep_Tw + 273.15, elev),
         Oct_ch4ex = Oct_ch4 - get_Ch4eq(Oct_Tw + 273.15, elev),
         Nov_ch4ex = Nov_ch4 - get_Ch4eq(Nov_Tw + 273.15, elev),
         Dec_ch4ex = Dec_ch4 - get_Ch4eq(Dec_Tw + 273.15, elev),
         Jan_ch4_sd = Jan_ch4_sd - get_Ch4eq(Jan_Tw + 273.15, elev),
         Feb_ch4_sd = Feb_ch4_sd - get_Ch4eq(Feb_Tw + 273.15, elev),
         Mar_ch4_sd = Mar_ch4_sd - get_Ch4eq(Mar_Tw + 273.15, elev),
         Apr_ch4_sd = Apr_ch4_sd - get_Ch4eq(Apr_Tw + 273.15, elev),
         May_ch4_sd = May_ch4_sd - get_Ch4eq(May_Tw + 273.15, elev),
         Jun_ch4_sd = Jun_ch4_sd - get_Ch4eq(Jun_Tw + 273.15, elev),
         Jul_ch4_sd = Jul_ch4_sd - get_Ch4eq(Jul_Tw + 273.15, elev),
         Aug_ch4_sd = Aug_ch4_sd - get_Ch4eq(Aug_Tw + 273.15, elev),
         Sep_ch4_sd = Sep_ch4_sd - get_Ch4eq(Sep_Tw + 273.15, elev),
         Oct_ch4_sd = Oct_ch4_sd - get_Ch4eq(Oct_Tw + 273.15, elev),
         Nov_ch4_sd = Nov_ch4_sd - get_Ch4eq(Nov_Tw + 273.15, elev),
         Dec_ch4_sd = Dec_ch4_sd - get_Ch4eq(Dec_Tw + 273.15, elev))

rm(hydro_k, meth_concs)

# Estimate river width and dryout seasonally ----
#calc annual mean width, and trim the quantiles of Qcv
df <- df  %>% 
  mutate(uparea = if_else(COMID %in% c(61000003,31000001 ), 35, uparea), #those sites have wrong area
         yeaQcv = case_when(yeaQcv <= quantile(df$yeaQcv, 0.001) ~ quantile(df$yeaQcv, 0.001),
                            yeaQcv >= quantile(df$yeaQcv, 0.995) ~ quantile(df$yeaQcv, 0.995),
                            TRUE ~ yeaQcv ),
         yeaWidth = case_when(Qlevel == 'ARID' ~ exp(2.1 + 0.43 * log(yeaQmean)),
                              Qlevel == 'MOD1' ~ exp(2.1 + 0.47 * log(yeaQmean)),
                              Qlevel == 'MOD2' ~ exp(2.24 + 0.47 * log(yeaQmean)),
                              Qlevel == 'WET1' ~ exp(2.2 + 0.45 * log(yeaQmean)),
                              Qlevel == 'WET2' ~ exp(1.92 + 0.49 * log(yeaQmean)) ), 
         runoffFL = yeaQmean / uparea * 3.6 * 24 * 365 )


#calculate WidthExp(exponent of the width-Q relationship)
#the following procedure prevents unrealistic widthExp estimates

df <- df %>% 
  mutate( WidthExp = case_when( log(runoffFL) <= 1.6 & log(yeaQmean) <= -7  ~ 0.074 * log(yeaQcv) - 0.026 * 1.6 - 0.011 * (-7) + 0.36,
                                log(runoffFL) > 1.6 & log(runoffFL) <= 8.5 & log(yeaQmean) <= -7 ~ 0.074 * log(yeaQcv) - 0.026 * log(runoffFL) - 0.011 * (-7) + 0.36,
                                log(runoffFL) > 8.5 & log(yeaQmean) <= -7 ~ 0.074 * log(yeaQcv) - 0.026 * 8.5 - 0.011 * (-7) + 0.36,
                                log(runoffFL) <= 1.6 & log(yeaQmean) > -7 & log(yeaQmean) <= 10 ~ 0.074 * log(yeaQcv) - 0.026 * 1.6 - 0.011 * log(yeaQmean) + 0.36,
                                log(runoffFL) > 1.6 & log(runoffFL) <= 8.5 & log(yeaQmean) > -7 & log(yeaQmean) <= 10 ~ 0.074 * log(yeaQcv) - 0.026 * log(runoffFL) - 0.011 * log(yeaQmean) + 0.36,
                                log(runoffFL) > 8.5 & log(yeaQmean) > -7 & log(yeaQmean) <= 10 ~  0.074 * log(yeaQcv) - 0.026 * 8.5 - 0.011 * log(yeaQmean) + 0.36,
                                log(runoffFL) <= 1.6 & log(yeaQmean) > 10 ~  0.074 * log(yeaQcv) - 0.026 * 1.6 - 0.011 * 10 + 0.36,
                                log(runoffFL) > 1.6 & log(runoffFL) <= 8.5 & log(yeaQmean) > 10 ~  0.074 * log(yeaQcv) - 0.026 * log(runoffFL) - 0.011 * 10 + 0.36,
                                log(runoffFL) > 8.5 & log(yeaQmean) > 10 ~ 0.074 * log(yeaQcv) - 0.026 * 8.5 - 0.011 * 10 + 0.36 ),
          WidthExp = ifelse(WidthExp <= quantile(WidthExp, 0.01), quantile(WidthExp, 0.01), WidthExp) )


#calc at-a-station coefs and monthly width
df <- df %>% 
  mutate(
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
    DecWidth = Widthcoef * (DecQmean ^ WidthExp) ) %>% 
  filter( wkbasin != 'NARID15') #there is only one flowline, caused by misalignment

#calculate timedryout
df <- df %>% 
  mutate(across(ends_with("Qmean"), timedryout_calculater, .names = "{.col}timedryout" )) %>% 
  rename_at(vars(ends_with('timedryout')), ~ str_remove(., "Qmean")) 

#fix some values higher than 60
for(mon in month.abb){
  df[df[, paste0(mon, 'Qmean')] >= 60, paste0(mon, 'timedryout')] = 0
}

# Extrapolation to small streams ----
## Find larger basins for all GRADES to do the extrapolation
#### generate working basins for SO extrapolation####
wkbasins <- df %>% 
  count( wkbasin) %>% 
  arrange(n) %>% 
  mutate(SOabc = NA)


#find out contiguous SO for extrapolation in each basin
for (i in 1:length(wkbasins$wkbasin)) {
  print(i)
  
  df_p <- df %>% 
    filter(wkbasin == wkbasins$wkbasin[i] ) 
  
  SOvec <- df_p %>% 
    distinct(strmOrder) %>% 
    arrange(strmOrder) %>% 
    pull()
  
  if (sum(1:5 %in% SOvec) == 5) {
    wkbasins$SOabc[i]  <- 4
  } else if (sum(1:4 %in% SOvec) == 4) {
    wkbasins$SOabc[i] <- 3
  } else if (sum(1:3 %in% SOvec) == 3) {
    wkbasins$SOabc[i] <- 2
  } else {
    wkbasins$SOabc[i] <- 2
  }
}
rm(df_p)

#join wkbasin prectemp 
prectemp <- read_csv('data/processed/upscaling_extra_data/basin4TempPrec.csv')
colnames(prectemp)[2:13] <- paste0(month.abb, '_tavg')
colnames(prectemp)[14:25] <- paste0(month.abb, '_prec')

wkbasins <- wkbasins %>% 
  left_join( prectemp, by = 'wkbasin')
rm(prectemp)

## Here we do the extrapolation ----
# Extrapolation of stream orders, surface area and fluxes for the wkbasins, for each month 
for(i in 1:78){
  print(paste0('Processing the ', i, ' basin ...'))
  
  df_basin <- df %>% 
    filter(wkbasin == wkbasins[i, ]$wkbasin ) 
  
  for (Mon in month.abb){
    
    #join prec and temp
    df_basin_mon <- df_basin %>%
      dplyr::select(wkbasin, HYBAS_ID, strmOrder, Length, paste0(Mon, 'Width'), paste0(Mon, 'timedryout'),
                    paste0(Mon, '_k'), paste0(Mon, '_k_sd'), paste0(Mon, '_ch4'), paste0(Mon, '_ch4ex'),  paste0(Mon, '_ch4_sd')  ) %>% 
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
    df_basin_mon <- df_basin_mon %>% 
      mutate(
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
      df_basin_mon %>% 
      mutate(
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
      df_basin_mon %>% 
      group_by(strmOrder) %>% 
      summarise(
        length_km = sum(Length * isChannel / 1000), #km
        width_m = mean(Width * isChannel),
        area_km2 = sum(surfArea),
        ephemArea_km2 = sum(ephemArea),
        ephemAreaRatio = ephemArea_km2 / area_km2,
        ephemArea_km2_ray13 = sum(ephemArea_ray13),
        ephemAreaRatio_ray13 = ephemArea_km2_ray13 / area_km2,
        k = sum(k * (surfArea - ephemArea), na.rm = TRUE) / area_km2,
        k_sd = sum(k_sd * (surfArea - ephemArea), na.rm = TRUE) / area_km2,
        ch4 = sum(ch4 * (surfArea - ephemArea), na.rm = TRUE) / area_km2,
        ch4_sd = sum(ch4_sd * (surfArea - ephemArea), na.rm = TRUE) / area_km2,
        ch4ex = sum(ch4ex * (surfArea - ephemArea), na.rm = TRUE) / area_km2
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
    
    if(SOabc_e >= 2) {
      fit <- lm(ephemAreaRatio ~ strmOrder, data = SOsum[1:SOabc_e, ])
      fitsum <- summary(fit)
      r2_ephemArea <- fitsum$r.squared
      emphemAreaRatio_extrap <- data.frame(strmOrder = SO)
      emphemAreaRatio_extrap$ephemAreaRatio <-
        predict(fit, emphemAreaRatio_extrap)
      totEphemArea_2 <-
        sum( emphemAreaRatio_extrap$ephemAreaRatio * width_extrap$width_m * length_extrap$length_km / 1000 )
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
 
    extrap <- tibble(wkbasin  = as.character(wkbasins[i,]$wkbasin),
                     strmOrder = width_extrap$strmOrder,
                     width_m = width_extrap$width_m,
                     length_km = length_extrap$length_km,
                     ephemAreaRatio = emphemAreaRatio_extrap$ephemAreaRatio, 
                     ephemAreaRatio_ray13 = emphemAreaRatio_ray13_extrap$ephemAreaRatio_ray13,
                     k = k_extrap$k,
                     k_sd = SOsum[SOsum$strmOrder == 1, ]$k_sd,
                     ch4 = SOsum[SOsum$strmOrder == 1, ]$ch4,
                     ch4_sd = SOsum[SOsum$strmOrder == 1, ]$ch4_sd,
                     ch4ex = SOsum[SOsum$strmOrder == 1, ]$ch4ex) %>% 
      mutate(
        area_km2 = width_m * length_km / 1000,
        ephemArea_km2 = area_km2 * ephemAreaRatio,
        ephemArea_km2_ray13 = area_km2 * ephemAreaRatio_ray13
      )
    
    #correct for negative effective area
    extrap[extrap$ephemArea_km2 > extrap$area_km2, ]$ephemArea_km2 <- extrap[extrap$ephemArea_km2 > extrap$area_km2, ]$area_km2
    extrap[extrap$ephemArea_km2_ray13 > extrap$area_km2, ]$ephemArea_km2_ray13 <- extrap[extrap$ephemArea_km2_ray13 > extrap$area_km2, ]$area_km2
    
    days_months <- data.frame(Mon = month.abb ,
                              days = c(31,28,31,30,31,30,31,31,30,31,30,31) )
    
    extrap <- extrap %>% 
      mutate( effecArea_km2 = area_km2 - ephemArea_km2,
              ch4F = ch4ex * k * 12 , #gC/m2/d
              ch4E = ch4F * effecArea_km2 * days_months$days[days_months$Mon == Mon] * 1000, #gC/month,
              ch4E_sd = sqrt(( 2*ch4_sd /ch4ex)^2 + (2*k_sd / k)^2 ) * 12  * effecArea_km2 * days_months$days[days_months$Mon == Mon] * 1000
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
    ch4_eq_2 <- mean(extrap$ch4ex)
    ch4F_2 <-
      if (sum(extrap$effecArea_km2 != 0)) {
        sum(extrap$ch4F * extrap$effecArea_km2) / totEffectArea_2
      } else{
        ch4F_2 = 0
      }
    ch4E_2 <- sum(extrap$ch4E)
    ch4E_sd_2 <- sqrt(sum(extrap$ch4E_sd^2))
    
    total_basin_area = sum(df_basin$subarea)
    
    basinMonArea <- data.frame(wkbasin=wkbasins[i,]$wkbasin, Mon, endSO,r2_width,r2_length,r2_ephemArea,
                               r2_ephemArea_ray13,r2_k,totEphemArea_1, totEphemArea_1_ray13, total_basin_area,
                               totLength_2,totArea_2,totEphemArea_2,totEphemArea_2_ray13,totEffectArea_2,
                               k_2,ch4_2,ch4_eq_2,ch4F_2,ch4E_2, ch4E_sd_2)
    
    #combining results
    if(i==1 & Mon=='Jan'){basinArea <- basinMonArea} else {basinArea <- rbind(basinArea,basinMonArea)}
  }
}

#remove everything except the useful stuff
rm(list = setdiff(ls(), c("df", "basinArea" )))
gc()


# Do the proper upscaling of methane fluxes ----

####replacing yeaWidth with GRWL width where available 
GRWLwidth <- read_csv('data/processed/upscaling_extra_data/GRWLwidthHydroBASIN4_30mplus.csv') %>% 
  select(COMID, width_mean) %>% 
  group_by(COMID) %>% 
  summarise(width_mean = min(width_mean)) %>% 
  filter(width_mean >= 90)


df_1 <- df %>% filter( !COMID %in% GRWLwidth$COMID)

df_2 <- df %>% 
  filter( COMID %in% GRWLwidth$COMID) %>% 
  left_join(GRWLwidth, by = 'COMID') %>% 
  mutate(rt = width_mean / yeaWidth)

rm(df)
gc()

if (sum(df_2$rt > 2)) {
  df_2[df_2$rt > 2,]$rt = 2
}

#fix the widths
df_2 <- df_2 %>% 
  mutate(
    JanWidth = JanWidth * rt,
    FebWidth = FebWidth * rt,
    MarWidth = MarWidth * rt,
    AprWidth = AprWidth * rt,
    MayWidth = MayWidth * rt,
    JunWidth = JunWidth * rt,
    JulWidth = JulWidth * rt,
    AugWidth = AugWidth * rt,
    SepWidth = SepWidth * rt,
    OctWidth = OctWidth * rt,
    NovWidth = NovWidth * rt,
    DecWidth = DecWidth * rt ) %>%
  select(-yeaWidth) %>% 
  rename( yeaWidth =width_mean)



df_2 <- df_2 %>% select( names(df_1))

if (sum(names(df_1) == names(df_2)) == ncol(df_1)) {
  df <-
    rbind(df_1, df_2)
} else{
  print('GRWL width is not added sucessfully!')
}
rm(df_1, df_2)
gc()

#remove sites with non valid hybasID
df <- df %>% 
  filter(!HYBAS_ID %in% c("1040040050","2040059370","5040055420"),
         !is.na(Apr_k)) #basins have no valid rivArea

q_to_vel <- function(q){
  exp(0.12 * log(q) - 1.06)
}

q_to_depth <- function(q){
  exp(-0.895 + 0.294*log(q))
}

#ch4 flux for each reach, in g C m^-2 d^-1, but excluding the ones with width below 0.3 meters
df <- df %>% 
  mutate(
         Jan_L95 = 3*3600*24*q_to_vel(JanQmean) /(Jan_k/q_to_depth(JanQmean)),
         Feb_L95 = 3*3600*24*q_to_vel(FebQmean) /(Feb_k/q_to_depth(FebQmean)),
         Mar_L95 = 3*3600*24*q_to_vel(MarQmean) /(Mar_k/q_to_depth(MarQmean)),
         Apr_L95 = 3*3600*24*q_to_vel(AprQmean) /(Apr_k/q_to_depth(AprQmean)),
         May_L95 = 3*3600*24*q_to_vel(MayQmean) /(May_k/q_to_depth(MayQmean)),
         Jun_L95 = 3*3600*24*q_to_vel(JunQmean) /(Jun_k/q_to_depth(JunQmean)),
         Jul_L95 = 3*3600*24*q_to_vel(JulQmean) /(Jul_k/q_to_depth(JulQmean)),
         Aug_L95 = 3*3600*24*q_to_vel(AugQmean) /(Aug_k/q_to_depth(AugQmean)),
         Sep_L95 = 3*3600*24*q_to_vel(SepQmean) /(Sep_k/q_to_depth(SepQmean)),
         Oct_L95 = 3*3600*24*q_to_vel(OctQmean) /(Oct_k/q_to_depth(OctQmean)),
         Nov_L95 = 3*3600*24*q_to_vel(NovQmean) /(Nov_k/q_to_depth(NovQmean)),
         Dec_L95 = 3*3600*24*q_to_vel(DecQmean) /(Dec_k/q_to_depth(DecQmean)),
         #Comment/ uncomment this chunk is to correct by footprint
         Jan_k = ifelse(Jan_L95 < Length, (3*3600*24*q_to_vel(JanQmean)*q_to_depth(JanQmean))/(Length), Jan_k ),
         Feb_k = ifelse(Feb_L95 < Length, (3*3600*24*q_to_vel(FebQmean)*q_to_depth(FebQmean))/(Length), Feb_k ),
         Mar_k = ifelse(Mar_L95 < Length, (3*3600*24*q_to_vel(MarQmean)*q_to_depth(MarQmean))/(Length), Mar_k ),
         Apr_k = ifelse(Apr_L95 < Length, (3*3600*24*q_to_vel(AprQmean)*q_to_depth(AprQmean))/(Length), Apr_k ),
         May_k = ifelse(May_L95 < Length, (3*3600*24*q_to_vel(MayQmean)*q_to_depth(MayQmean))/(Length), May_k ),
         Jun_k = ifelse(Jun_L95 < Length, (3*3600*24*q_to_vel(JunQmean)*q_to_depth(JunQmean))/(Length), Jun_k ),
         Jul_k = ifelse(Jul_L95 < Length, (3*3600*24*q_to_vel(JulQmean)*q_to_depth(JulQmean))/(Length), Jul_k ),
         Aug_k = ifelse(Aug_L95 < Length, (3*3600*24*q_to_vel(AugQmean)*q_to_depth(AugQmean))/(Length), Aug_k ),
         Sep_k = ifelse(Sep_L95 < Length, (3*3600*24*q_to_vel(SepQmean)*q_to_depth(SepQmean))/(Length), Sep_k ),
         Oct_k = ifelse(Oct_L95 < Length, (3*3600*24*q_to_vel(OctQmean)*q_to_depth(OctQmean))/(Length), Oct_k ),
         Nov_k = ifelse(Nov_L95 < Length, (3*3600*24*q_to_vel(NovQmean)*q_to_depth(NovQmean))/(Length), Nov_k ),
         Dec_k = ifelse(Dec_L95 < Length, (3*3600*24*q_to_vel(DecQmean)*q_to_depth(DecQmean))/(Length), Dec_k ),
         footprint_correction = Length / (3*3600*24*q_to_vel(yeaQmean) /(Jan_k/q_to_depth(yeaQmean))),
         Jan_ch4F = Jan_ch4ex*Jan_k*12/1000*(JanWidth >= 0.3),
         Feb_ch4F = Feb_ch4ex*Feb_k*12/1000*(FebWidth >= 0.3),
         Mar_ch4F = Mar_ch4ex*Mar_k*12/1000*(MarWidth >= 0.3),
         Apr_ch4F = Apr_ch4ex*Apr_k*12/1000*(AprWidth >= 0.3),
         May_ch4F = May_ch4ex*May_k*12/1000*(MayWidth >= 0.3),
         Jun_ch4F = Jun_ch4ex*Jun_k*12/1000*(JunWidth >= 0.3),
         Jul_ch4F = Jul_ch4ex*Jul_k*12/1000*(JulWidth >= 0.3),
         Aug_ch4F = Aug_ch4ex*Aug_k*12/1000*(AugWidth >= 0.3),
         Sep_ch4F = Sep_ch4ex*Sep_k*12/1000*(SepWidth >= 0.3),
         Oct_ch4F = Oct_ch4ex*Oct_k*12/1000*(OctWidth >= 0.3),
         Nov_ch4F = Nov_ch4ex*Nov_k*12/1000*(NovWidth >= 0.3),
         Dec_ch4F = Dec_ch4ex*Dec_k*12/1000*(DecWidth >= 0.3),
         Jan_ch4F_sd = sqrt((2*Jan_ch4_sd/Jan_ch4ex)^2 + (2*Jan_k_sd/Jan_k)^2) *12/1000*(JanWidth >= 0.3),
         Feb_ch4F_sd = sqrt((2*Feb_ch4_sd/Feb_ch4ex)^2 + (2*Feb_k_sd/Feb_k)^2) *12/1000*(FebWidth >= 0.3),
         Mar_ch4F_sd = sqrt((2*Mar_ch4_sd/Mar_ch4ex)^2 + (2*Mar_k_sd/Mar_k)^2) *12/1000*(MarWidth >= 0.3),
         Apr_ch4F_sd = sqrt((2*Apr_ch4_sd/Apr_ch4ex)^2 + (2*Apr_k_sd/Apr_k)^2) *12/1000*(AprWidth >= 0.3),
         May_ch4F_sd = sqrt((2*May_ch4_sd/May_ch4ex)^2 + (2*May_k_sd/May_k)^2) *12/1000*(MayWidth >= 0.3),
         Jun_ch4F_sd = sqrt((2*Jun_ch4_sd/Jun_ch4ex)^2 + (2*Jun_k_sd/Jun_k)^2) *12/1000*(JunWidth >= 0.3),
         Jul_ch4F_sd = sqrt((2*Jul_ch4_sd/Jul_ch4ex)^2 + (2*Jul_k_sd/Jul_k)^2) *12/1000*(JulWidth >= 0.3),
         Aug_ch4F_sd = sqrt((2*Aug_ch4_sd/Aug_ch4ex)^2 + (2*Aug_k_sd/Aug_k)^2) *12/1000*(AugWidth >= 0.3),
         Sep_ch4F_sd = sqrt((2*Sep_ch4_sd/Sep_ch4ex)^2 + (2*Sep_k_sd/Sep_k)^2) *12/1000*(SepWidth >= 0.3),
         Oct_ch4F_sd = sqrt((2*Oct_ch4_sd/Oct_ch4ex)^2 + (2*Oct_k_sd/Oct_k)^2) *12/1000*(OctWidth >= 0.3),
         Nov_ch4F_sd = sqrt((2*Nov_ch4_sd/Nov_ch4ex)^2 + (2*Nov_k_sd/Nov_k)^2) *12/1000*(NovWidth >= 0.3),
         Dec_ch4F_sd = sqrt((2*Dec_ch4_sd/Dec_ch4ex)^2 + (2*Dec_k_sd/Dec_k)^2) *12/1000*(DecWidth >= 0.3),
         Jan_ch4F_cv = sqrt((Jan_ch4_sd/Jan_ch4ex)^2 + (Jan_k_sd/Jan_k)^2)/(Jan_ch4ex*Jan_k)*100,
         Feb_ch4F_cv = sqrt((Feb_ch4_sd/Feb_ch4ex)^2 + (Feb_k_sd/Feb_k)^2)/(Feb_ch4ex*Feb_k)*100,
         Mar_ch4F_cv = sqrt((Mar_ch4_sd/Mar_ch4ex)^2 + (Mar_k_sd/Mar_k)^2)/(Mar_ch4ex*Mar_k)*100,
         Apr_ch4F_cv = sqrt((Apr_ch4_sd/Apr_ch4ex)^2 + (Apr_k_sd/Apr_k)^2)/(Apr_ch4ex*Apr_k)*100,
         May_ch4F_cv = sqrt((May_ch4_sd/May_ch4ex)^2 + (May_k_sd/May_k)^2)/(May_ch4ex*May_k)*100,
         Jun_ch4F_cv = sqrt((Jun_ch4_sd/Jun_ch4ex)^2 + (Jun_k_sd/Jun_k)^2)/(Jun_ch4ex*Jun_k)*100,
         Jul_ch4F_cv = sqrt((Jul_ch4_sd/Jul_ch4ex)^2 + (Jul_k_sd/Jul_k)^2)/(Jul_ch4ex*Jul_k)*100,
         Aug_ch4F_cv = sqrt((Aug_ch4_sd/Aug_ch4ex)^2 + (Aug_k_sd/Aug_k)^2)/(Aug_ch4ex*Aug_k)*100,
         Sep_ch4F_cv = sqrt((Sep_ch4_sd/Sep_ch4ex)^2 + (Sep_k_sd/Sep_k)^2)/(Sep_ch4ex*Sep_k)*100,
         Oct_ch4F_cv = sqrt((Oct_ch4_sd/Oct_ch4ex)^2 + (Oct_k_sd/Oct_k)^2)/(Oct_ch4ex*Oct_k)*100,
         Nov_ch4F_cv = sqrt((Nov_ch4_sd/Nov_ch4ex)^2 + (Nov_k_sd/Nov_k)^2)/(Nov_ch4ex*Nov_k)*100,
         Dec_ch4F_cv = sqrt((Dec_ch4_sd/Dec_ch4ex)^2 + (Dec_k_sd/Dec_k)^2)/(Dec_ch4ex*Dec_k)*100
  ) 


#total number of sites corrected
df %>% 
  summarise(frac_corrected = sum(ifelse(footprint_correction > 1, 1, 0))/n()*100)

df %>% 
  mutate(k_uncorrected = (2841 * Slope * q_to_vel(yeaQmean) + 2.02),
         k_corrected = (3*3600*24*q_to_vel(yeaQmean)*q_to_depth(yeaQmean))/(Length)) %>% 
  filter(footprint_correction > 1) %>% 
  ggplot(aes(k_uncorrected, k_corrected, color= Slope))+
  geom_point()+
  geom_abline(slope =1, intercept = 0)+
  scale_color_viridis_c(trans = "sqrt")

df %>% 
  mutate(k_uncorrected = (2841 * Slope * q_to_vel(yeaQmean) + 2.02),
         k_corrected = (3*3600*24*q_to_vel(yeaQmean)*q_to_depth(yeaQmean))/(Length),
         k_diff = (k_corrected - k_uncorrected)/k_uncorrected*100) %>% 
  filter(footprint_correction > 1) %>% 
  ggplot()+
  geom_density(aes(k_diff))+
  geom_vline(aes(xintercept = median(k_diff)))
  
  

df %>% 
  mutate(slope_cat = case_when(Slope < .001 ~ "< 0.001",
                               Slope >= .001 & Slope < .01 ~ "0.001 - 0.01",
                               Slope >= .01 & Slope < .1 ~ "0.01 - 0.1",
                               Slope >= .1 & Slope < .2 ~ "0.1 - 0.2",
                               Slope >= 0.2 ~ "> 0.2") %>% 
           as_factor() %>% 
           fct_relevel("< 0.001", "0.001 - 0.01", "0.01 - 0.1","0.1 - 0.2", "> 0.2"),
         footprint_corrected = ifelse(footprint_correction > 1, 1, 0)) %>% 
  group_by(slope_cat) %>% 
  summarise(foot_corr = sum(footprint_corrected)/n(),
            n= n()) %>% 
  ggplot(aes(slope_cat, foot_corr*100))+
  scale_y_continuous(expand = c(0,0), limits = c(0,40))+
  geom_col()+
  theme_classic()+
  labs(x=" River reach slope categories", y = "% of reaches corrected")


#ggsave("figures/supplementary/footprints_reach_slopes.png")

#stats to cap fluxes at 2 SD, uncoment to do that 
# main_stats <- df %>%
#   select(COMID, ends_with("ch4F")) %>%
#   pivot_longer(-COMID, values_to = "flux", names_to = "month_flux") %>%
#   summarise(sd = sd(flux, na.rm = TRUE),
#             mean = mean(flux, na.rm = TRUE),
#             median = median(flux, na.rm = TRUE),
#             max = max(flux, na.rm = TRUE),
#             min = min(flux, na.rm = TRUE),
#             q.95 = quantile(flux, 0.95, na.rm = TRUE))
# 
# main_stats
#
#two_SD <- main_stats$sd * 2

#get ice coverage file
icecov <- read_csv('data/processed/upscaling_extra_data/iceout.csv', 
                   col_types = cols(.default='d',HYBAS_ID='c'))

names(icecov) <- c('HYBAS_ID', paste0( month.abb, '_iceCov'))

#ch4 emissions for the whole reach, correcting by time dry and time under ice cover, and per month, not day
df <- df %>% 
  mutate(HYBAS_ID = as.character(HYBAS_ID)) %>% 
  left_join(icecov, by = 'HYBAS_ID') %>%
  #uncomment below to cap fluxes at 2SD
  #mutate(across(ends_with("ch4F"), ~ifelse(.x > two_SD, two_SD, .x))) %>% #this line caps fluxes at two SD
  mutate(Jan_ch4E = Jan_ch4F*Length*JanWidth*(1-Jantimedryout)*(1-Jan_iceCov)*31,
         Feb_ch4E = Feb_ch4F*Length*FebWidth*(1-Febtimedryout)*(1-Feb_iceCov)*28,
         Mar_ch4E = Mar_ch4F*Length*MarWidth*(1-Martimedryout)*(1-Mar_iceCov)*31,
         Apr_ch4E = Apr_ch4F*Length*AprWidth*(1-Aprtimedryout)*(1-Apr_iceCov)*30,
         May_ch4E = May_ch4F*Length*MayWidth*(1-Maytimedryout)*(1-May_iceCov)*31,
         Jun_ch4E = Jun_ch4F*Length*JunWidth*(1-Juntimedryout)*(1-Jun_iceCov)*30,
         Jul_ch4E = Jul_ch4F*Length*JulWidth*(1-Jultimedryout)*(1-Jul_iceCov)*31,
         Aug_ch4E = Aug_ch4F*Length*AugWidth*(1-Augtimedryout)*(1-Aug_iceCov)*31,
         Sep_ch4E = Sep_ch4F*Length*SepWidth*(1-Septimedryout)*(1-Sep_iceCov)*30,
         Oct_ch4E = Oct_ch4F*Length*OctWidth*(1-Octtimedryout)*(1-Oct_iceCov)*31,
         Nov_ch4E = Nov_ch4F*Length*NovWidth*(1-Novtimedryout)*(1-Nov_iceCov)*30,
         Dec_ch4E = Dec_ch4F*Length*DecWidth*(1-Dectimedryout)*(1-Dec_iceCov)*31,
         Jan_ch4E_sd = Jan_ch4F_sd*Length*JanWidth*(1-Jantimedryout)*(1-Jan_iceCov)*31,
         Feb_ch4E_sd = Feb_ch4F_sd*Length*FebWidth*(1-Febtimedryout)*(1-Feb_iceCov)*28,
         Mar_ch4E_sd = Mar_ch4F_sd*Length*MarWidth*(1-Martimedryout)*(1-Mar_iceCov)*31,
         Apr_ch4E_sd = Apr_ch4F_sd*Length*AprWidth*(1-Aprtimedryout)*(1-Apr_iceCov)*30,
         May_ch4E_sd = May_ch4F_sd*Length*MayWidth*(1-Maytimedryout)*(1-May_iceCov)*31,
         Jun_ch4E_sd = Jun_ch4F_sd*Length*JunWidth*(1-Juntimedryout)*(1-Jun_iceCov)*30,
         Jul_ch4E_sd = Jul_ch4F_sd*Length*JulWidth*(1-Jultimedryout)*(1-Jul_iceCov)*31,
         Aug_ch4E_sd = Aug_ch4F_sd*Length*AugWidth*(1-Augtimedryout)*(1-Aug_iceCov)*31,
         Sep_ch4E_sd = Sep_ch4F_sd*Length*SepWidth*(1-Septimedryout)*(1-Sep_iceCov)*30,
         Oct_ch4E_sd = Oct_ch4F_sd*Length*OctWidth*(1-Octtimedryout)*(1-Oct_iceCov)*31,
         Nov_ch4E_sd = Nov_ch4F_sd*Length*NovWidth*(1-Novtimedryout)*(1-Nov_iceCov)*30,
         Dec_ch4E_sd = Dec_ch4F_sd*Length*DecWidth*(1-Dectimedryout)*(1-Dec_iceCov)*31) 



#### compare different flux calculations corrections, this needs to be run by commenting up and down multiple times so is disconnected by default
# footprint_method <- df %>%
#   select(COMID, ends_with(c("ch4","ch4F", "_k"))) %>%
#   drop_na(Jan_k, Jan_ch4) %>%
#   rowwise() %>%
#   mutate(k = mean(Jan_k:Dec_k, na.rm = TRUE),
#          ch4 = mean(Jan_ch4:Dec_ch4, na.rm = TRUE),
#          flux = mean(Jan_ch4F:Dec_ch4F, na.rm = TRUE),
#          method = "footprint") %>%
#   select(method, k, ch4, flux)
# 
# write_csv(footprint_method, "data/processed/method_footprint.csv")

# k_cap_method <- df %>% 
#   select(COMID, ends_with(c("ch4","ch4F", "_k"))) %>% 
#   drop_na(Jan_k, Jan_ch4) %>% 
#   rowwise() %>% 
#   mutate(k = mean(Jan_k:Dec_k, na.rm = TRUE),
#          ch4 = mean(Jan_ch4:Dec_ch4, na.rm = TRUE),
#          flux = mean(Jan_ch4F:Dec_ch4F, na.rm = TRUE),
#          method = "k_cap") %>% 
#   select(method, k, ch4, flux)
# 
# write_csv(k_cap_method, "data/processed/method_k.csv")

# flux_cap_method <- df %>% 
#   select(COMID, ends_with(c("ch4","ch4F", "_k"))) %>% 
#   drop_na(Jan_k, Jan_ch4) %>% 
#   rowwise() %>% 
#   mutate(k = mean(Jan_k:Dec_k, na.rm = TRUE),
#          ch4 = mean(Jan_ch4:Dec_ch4, na.rm = TRUE),
#          flux = mean(Jan_ch4F:Dec_ch4F, na.rm = TRUE),
#          method = "flux_cap") %>% 
#   select(method, k, ch4, flux)
# 
# write_csv(flux_cap_method, "data/processed/method_flux.csv")

# uncorrected <- df %>% 
#   select(COMID, ends_with(c("ch4","ch4F", "_k"))) %>% 
#   drop_na(Jan_k, Jan_ch4) %>% 
#   rowwise() %>% 
#   mutate(k = mean(Jan_k:Dec_k, na.rm = TRUE),
#          ch4 = mean(Jan_ch4:Dec_ch4, na.rm = TRUE),
#          flux = mean(Jan_ch4F:Dec_ch4F, na.rm = TRUE),
#          method = "uncorrected") %>% 
#   select(method, k, ch4, flux)
# 
# write_csv(uncorrected, "data/processed/uncorrected.csv")

# paths_methods <- c("data/processed/uncorrected.csv",
#                    "data/processed/method_k.csv",
#                    "data/processed/method_flux.csv",
#                    "data/processed/method_footprint.csv")
# 
# methods <- map_dfr(paths_methods, read_csv)
# 
# write_csv(methods, "data/processed/methods_flux_comparison.csv")
# 
# file.remove(paths_methods)

## Now we assign the upscaled emissions for each basin area, using the proportional catchment area of each basin
#join res2
colnames(basinArea)

small_streams_ba <- pivot_wider(
  basinArea %>%  select(wkbasin, total_basin_area, Mon, totArea_2, totEphemArea_2, ch4E_2, ch4E_sd_2),
  id_cols = c(wkbasin, total_basin_area),
  names_from = Mon,
  values_from = c(totArea_2, totEphemArea_2, ch4E_2, ch4E_sd_2),
  names_glue = "{Mon}_{.value}") %>% 
  mutate(wkbasin = as.character(wkbasin))

#correcting names
names(small_streams_ba) <- str_replace(names(small_streams_ba), "totArea_2", "rivArea_extrap")
names(small_streams_ba) <- str_replace(names(small_streams_ba), "totEphemArea_2", "totEphemArea_extrap")
names(small_streams_ba) <- str_replace(names(small_streams_ba), "_2", "")


names(df)


total_fluxes <- df %>% 
  dplyr::select(COMID, wkbasin, subarea, runoffFL, Jantimedryout:Dectimedryout, Jan_iceCov:Dec_iceCov, 
                Length, JanWidth:DecWidth, Jan_ch4F:Dec_ch4F_cv, Jan_ch4E:Dec_ch4E_sd  ) %>% 
  left_join(small_streams_ba, by = "wkbasin", suffix = c("_reach", "_extrap")) %>% 
  mutate(across(Jan_rivArea_extrap:Dec_ch4E_sd_extrap, ~ .x * (subarea/total_basin_area))) %>% #assign the proportional area and efflux of the extrapolated areas to each comid
  mutate(Jan_ch4E_extrap = Jan_ch4E_extrap*(1-Jan_iceCov), #correct extrapolated emissions by ice cover
         Feb_ch4E_extrap = Feb_ch4E_extrap*(1-Feb_iceCov),
         Mar_ch4E_extrap = Mar_ch4E_extrap*(1-Mar_iceCov),
         Apr_ch4E_extrap = Apr_ch4E_extrap*(1-Apr_iceCov),
         May_ch4E_extrap = May_ch4E_extrap*(1-May_iceCov),
         Jun_ch4E_extrap = Jun_ch4E_extrap*(1-Jun_iceCov),
         Jul_ch4E_extrap = Jul_ch4E_extrap*(1-Jul_iceCov),
         Aug_ch4E_extrap = Aug_ch4E_extrap*(1-Aug_iceCov),
         Sep_ch4E_extrap = Sep_ch4E_extrap*(1-Sep_iceCov),
         Oct_ch4E_extrap = Oct_ch4E_extrap*(1-Oct_iceCov),
         Nov_ch4E_extrap = Nov_ch4E_extrap*(1-Nov_iceCov),
         Dec_ch4E_extrap = Dec_ch4E_extrap*(1-Dec_iceCov),
         Jan_ch4E_sd_extrap = Jan_ch4E_sd_extrap*(1-Jan_iceCov), #correct extrapolated emissions by ice cover
         Feb_ch4E_sd_extrap = Feb_ch4E_sd_extrap*(1-Feb_iceCov),
         Mar_ch4E_sd_extrap = Mar_ch4E_sd_extrap*(1-Mar_iceCov),
         Apr_ch4E_sd_extrap = Apr_ch4E_sd_extrap*(1-Apr_iceCov),
         May_ch4E_sd_extrap = May_ch4E_sd_extrap*(1-May_iceCov),
         Jun_ch4E_sd_extrap = Jun_ch4E_sd_extrap*(1-Jun_iceCov),
         Jul_ch4E_sd_extrap = Jul_ch4E_sd_extrap*(1-Jul_iceCov),
         Aug_ch4E_sd_extrap = Aug_ch4E_sd_extrap*(1-Aug_iceCov),
         Sep_ch4E_sd_extrap = Sep_ch4E_sd_extrap*(1-Sep_iceCov),
         Oct_ch4E_sd_extrap = Oct_ch4E_sd_extrap*(1-Oct_iceCov),
         Nov_ch4E_sd_extrap = Nov_ch4E_sd_extrap*(1-Nov_iceCov),
         Dec_ch4E_sd_extrap = Dec_ch4E_sd_extrap*(1-Dec_iceCov),
         Jan_area_m2 = JanWidth*Length + Jan_rivArea_extrap/1e+6,  #also calculate the total river area for each month
         Feb_area_m2 = FebWidth*Length + Feb_rivArea_extrap/1e+6,  
         Mar_area_m2 = MarWidth*Length + Mar_rivArea_extrap/1e+6,  
         Apr_area_m2 = AprWidth*Length + Apr_rivArea_extrap/1e+6,  
         May_area_m2 = MayWidth*Length + May_rivArea_extrap/1e+6,  
         Jun_area_m2 = JunWidth*Length + Jun_rivArea_extrap/1e+6,  
         Jul_area_m2 = JulWidth*Length + Jul_rivArea_extrap/1e+6,  
         Aug_area_m2 = AugWidth*Length + Aug_rivArea_extrap/1e+6,  
         Sep_area_m2 = SepWidth*Length + Sep_rivArea_extrap/1e+6,  
         Oct_area_m2 = OctWidth*Length + Oct_rivArea_extrap/1e+6,  
         Nov_area_m2 = NovWidth*Length + Nov_rivArea_extrap/1e+6,  
         Dec_area_m2 = DecWidth*Length + Dec_rivArea_extrap/1e+6,  
         Jan_ephemarea_m2 = Length*JanWidth*(1-Jantimedryout)*(1-Jan_iceCov) + Jan_totEphemArea_extrap/1e+6,  #and the area that goes dry of it
         Feb_ephemarea_m2 = Length*FebWidth*(1-Febtimedryout)*(1-Feb_iceCov) + Feb_totEphemArea_extrap/1e+6,  
         Mar_ephemarea_m2 = Length*MarWidth*(1-Martimedryout)*(1-Mar_iceCov) + Mar_totEphemArea_extrap/1e+6,  
         Apr_ephemarea_m2 = Length*AprWidth*(1-Aprtimedryout)*(1-Apr_iceCov) + Apr_totEphemArea_extrap/1e+6,  
         May_ephemarea_m2 = Length*MayWidth*(1-Maytimedryout)*(1-May_iceCov) + May_totEphemArea_extrap/1e+6,  
         Jun_ephemarea_m2 = Length*JunWidth*(1-Juntimedryout)*(1-Jun_iceCov) + Jun_totEphemArea_extrap/1e+6,  
         Jul_ephemarea_m2 = Length*JulWidth*(1-Jultimedryout)*(1-Jul_iceCov) + Jul_totEphemArea_extrap/1e+6,  
         Aug_ephemarea_m2 = Length*AugWidth*(1-Augtimedryout)*(1-Aug_iceCov) + Aug_totEphemArea_extrap/1e+6,  
         Sep_ephemarea_m2 = Length*SepWidth*(1-Septimedryout)*(1-Sep_iceCov) + Sep_totEphemArea_extrap/1e+6,  
         Oct_ephemarea_m2 = Length*OctWidth*(1-Octtimedryout)*(1-Oct_iceCov) + Oct_totEphemArea_extrap/1e+6,  
         Nov_ephemarea_m2 = Length*NovWidth*(1-Novtimedryout)*(1-Nov_iceCov) + Nov_totEphemArea_extrap/1e+6,  
         Dec_ephemarea_m2 = Length*DecWidth*(1-Dectimedryout)*(1-Dec_iceCov) + Dec_totEphemArea_extrap/1e+6 ) %>% 
  dplyr::select(COMID, runoffFL, Jan_ch4F:Dec_ch4E_sd_reach, Jan_ch4E_extrap:Dec_ephemarea_m2, Jan_iceCov:Dec_iceCov )

names(total_fluxes)  

## now get the estimate. For that, do the sum of the measured + extrapolated fluxes, for all months ----
total_fluxes %>% 
  select(COMID, ends_with(c("ch4E_extrap", "ch4E_reach"))) %>% 
  pivot_longer(-COMID, values_to = "flux", names_to = "type") %>% 
  mutate(type = str_remove(type, month.abb)) %>% 
  summarise(total = sum(flux, na.rm = TRUE)/1e+12*16/12) #in TgCH4 / yr


#sd propagated
total_fluxes %>% 
  select(COMID, ends_with(c("ch4E_sd_extrap", "ch4E_sd_reach"))) %>% 
  pivot_longer(-COMID, values_to = "flux", names_to = "type") %>%
  summarise(total =sum(flux, na.rm = TRUE)/1e+12*16/12) #in TgCH4 / yr


## Summary from the different methods :
## Method ------- Total flux (Tg Ch4 yr-1)
## _______________________________________
## Footprint ------- 12.8
## k capped -------- 11.3
## Flux capped ----- 13.9
## Uncorrected ----- 14.1




## Export file ----
total_fluxes %>% 
  select(COMID:Dec_ch4F, Jan_ch4F_cv:Dec_ch4E_reach, Jan_ch4E_extrap:Dec_ch4E_extrap,
         Jan_area_m2:Dec_ephemarea_m2, Jan_iceCov:Dec_iceCov ) %>% 
  write_csv( "data/processed/grades_ch4_fluxes.csv")


#TABLE for the SM
df %>% 
  group_by(strmOrder) %>% 
  summarise(n = n(),
            width = median(yeaWidth),
            length = median(Length),
            slope = mean(Slope)) %>% 
  arrange(strmOrder)

