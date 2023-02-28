########################################.
#### Script upscale CH4 emissions in rivers globally.
#### This script is a modified version of "5_CH4_upscaling.R", which performs an uncertiainity quantification an dsensitivity analysis.
#### Author:  Gerard Rocher-Ros, based on upscaling procedures by Shaoda Liu
#### Last edit: 2023-03-10

########################################.
# To do this:
# First of all, define what you will do, either just a normal uncertainty or a sensitivity analysis, 
# you do this by setting the two variables below. The default is an uncertainity analysis.
# Sensitivity analysis is done by increasing or decreasing a given variable 1SD, the variables included are methane concentration, k and river area.
# To do the full sensitivity analysis, it should be done 6 times, one for eqch variable and change, and thus the script should be re-run with a different 
# combination of parameters. It takes multiple hours to perform with an n = 1000

#Variable that controls how we are doing the sensitivity
sens_var = "none" #can be "ch4" , "k", "area"
sens_change = "none"  # can be "plus1SD" or "minus1SD"


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
hydro_k <- read_csv("data/processed/q_and_k.csv") 


#upscaled methane concentrations
#mean estimates are "month_ch4", while SD are "month_ch4_sd"
meth_concs <- lapply(list.files(path = "data/processed/meth_preds/", pattern = "ch4_preds_uncertainty", full.names = TRUE), 
                     read_csv) %>% 
  purrr::reduce(left_join, by = "COMID") %>% 
  rename_with( ~str_replace(.x, "mean", "ch4")) %>% 
  rename_with( ~str_replace(.x, "se", "ch4_sd")) 

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
#Following Liu et al. 2022

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
        k = sum(k * (surfArea - ephemArea), na.rm = TRUE) / area_km2,
        k_sd = sum(k_sd * (surfArea - ephemArea), na.rm = TRUE) / area_km2,
        ch4 = sum(ch4 * (surfArea - ephemArea), na.rm = TRUE) / area_km2,
        ch4_sd = sum(ch4_sd * (surfArea - ephemArea), na.rm = TRUE) / area_km2,
        ch4ex = sum(ch4ex * (surfArea - ephemArea), na.rm = TRUE) / area_km2
      )
    
    totEphemArea_1 <- sum(SOsum$ephemArea_km2)

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

    width_extrap <- data.frame(strmOrder = SO)
    width_preds <- predict(fit, width_extrap, se.fit = TRUE)
    
    width_extrap <- width_extrap %>% 
      mutate(width_m = exp(width_preds$fit),
             width_m_sd = exp(width_preds$se.fit),
             width_m_sd = ifelse(is.na(width_m_sd), width_m/2, width_m_sd),
             width_m_sd = ifelse(width_m_sd> width_m, width_m/2, width_m_sd))
    

    #width_extrap$width_m <- exp(predict(fit, width_extrap, interval = "confidence"))
    
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
    

    extrap <- tibble(wkbasin  = as.character(wkbasins[i,]$wkbasin),
                     strmOrder = width_extrap$strmOrder,
                     width_m = width_extrap$width_m,
                     width_m_sd = width_extrap$width_m_sd,
                     length_km = length_extrap$length_km,
                     ephemAreaRatio = emphemAreaRatio_extrap$ephemAreaRatio, 
                     k = k_extrap$k,
                     k_sd = SOsum[SOsum$strmOrder == 1, ]$k_sd,
                     ch4 = SOsum[SOsum$strmOrder == 1, ]$ch4,
                     ch4_sd = SOsum[SOsum$strmOrder == 1, ]$ch4_sd,
                     ch4ex = SOsum[SOsum$strmOrder == 1, ]$ch4ex,
                     ch4F = NA,
                     ch4E = NA,
                     ch4E_sd = NA,
                     area_km2 = NA,
                     ephemArea_km2 = NA) 
    
    
    days_months <- data.frame(Mon = month.abb,
                              days = c(31,28,31,30,31,30,31,31,30,31,30,31) )
    
    
    for(j in seq_along(extrap$wkbasin)){
      
      
      ch43 = case_when(sens_var == "ch4" & sens_change == "plus1SD" ~ rnorm(1000, extrap$ch4ex[j] + extrap$ch4_sd[j] , extrap$ch4_sd[j]),
                       sens_var == "ch4" & sens_change == "minus1SD" ~ rnorm(1000, extrap$ch4ex[j] - extrap$ch4_sd[j] , extrap$ch4_sd[j]),
                       TRUE ~ rnorm(1000, extrap$ch4ex[j] , extrap$ch4_sd[j]))
      
      ch43 = ifelse(ch43 < 0, 0, ch43) #set negative values to 0

      ch4F3 = case_when(sens_var == "k" & sens_change == "plus1SD" ~ ch43 * rnorm(1000, extrap$k[j] + extrap$k_sd[j], extrap$k_sd[j])* 12, 
                        sens_var == "k" & sens_change == "minus1SD" ~ ch43 * rnorm(1000, extrap$k[j] - extrap$k_sd[j], extrap$k_sd[j])* 12, 
                        TRUE ~ ch43 * rnorm(1000, extrap$k[j], extrap$k_sd[j])* 12)
      
      
      width = case_when(sens_var == "area" & sens_change == "plus1SD" ~ rnorm(1000, extrap$width_m[j] + extrap$width_m_sd[j], extrap$width_m_sd[j]), 
                        sens_var == "area" & sens_change == "minus1SD" ~ rnorm(1000, extrap$width_m[j] - extrap$width_m_sd[j], extrap$width_m_sd[j]), 
                        TRUE ~ rnorm(1000, extrap$width_m[j], extrap$width_m_sd[j]))
           
      width = ifelse(width < 0, 0, width) # fix negative widths by setting them to 0
      
      area_km2 = width * extrap$length_km[j] / 1000
      ephemArea_km2 = area_km2 * extrap$ephemAreaRatio[j]
      ephemArea_km2 = ifelse(ephemArea_km2 > area_km2, area_km2, ephemArea_km2 )     #correct for negative effective area
      effecArea_km2 = area_km2 - ephemArea_km2
      ch4E = ch4F3 * effecArea_km2
      
      
     extrap$ch4F[j] = median(ch4F3)  #gC/m2/d
     extrap$ch4E[j] = median(ch4E) * days_months$days[days_months$Mon == Mon] * 1000 #gC/month,
     extrap$ch4E_sd[j] = sd(ch4E) * days_months$days[days_months$Mon == Mon] * 1000
     extrap$area_km2[j] = median(area_km2)
     extrap$ephemArea_km2[j] = median(ephemArea_km2)
    }
    
    
    
    totLength_2 <- sum(extrap$length_km) #total extrapolated flowline length
    totArea_2 <- sum(extrap$area_km2)# total extrapolated surface area
    totEphemArea_2 <- sum(extrap$ephemArea_km2)
    totEffectArea_2 <- sum(extrap$effecArea_km2)
    
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
    ch4E_2 <- sum(extrap$ch4E, na.rm = TRUE)
    ch4E_sd_2 <- sum(extrap$ch4E_sd, na.rm = TRUE)
    
    total_basin_area = sum(df_basin$subarea)
    
    basinMonArea <- data.frame(wkbasin=wkbasins[i,]$wkbasin, Mon, endSO,r2_width,r2_length,r2_ephemArea,
                               r2_k,totEphemArea_1, total_basin_area, totLength_2,totArea_2,totEphemArea_2,
                               totEffectArea_2, k_2,ch4_2,ch4_eq_2,ch4F_2,ch4E_2, ch4E_sd_2)
    
    #combining results
    if(i==1 & Mon=='Jan'){basinArea <- basinMonArea} else {basinArea <- rbind(basinArea,basinMonArea)}
  }
}

#remove everything except the useful stuff
rm(list = setdiff(ls(), c("df", "basinArea", "sens_change", "sens_var")))
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
names(small_streams_ba) <- str_replace(names(small_streams_ba), "E_sd", "E_sd_extrap")



#get ice coverage file
icecov <- read_csv('data/processed/upscaling_extra_data/iceout.csv', 
                   col_types = cols(.default='d',HYBAS_ID='c'))

names(icecov) <- c('HYBAS_ID', paste0( month.abb, '_iceCov'))


q_to_vel <- function(q){
  exp(0.12 * log(q) - 1.06)
}

q_to_depth <- function(q){
  exp(-0.895 + 0.294*log(q))
}

#ch4 flux for each reach, in g C m^-2 d^-1, but excluding the ones with width below 0.3 meters
df1 <- df %>% 
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
    Jan_area_m2_reach = JanWidth*Length,  #also calculate the total river area for each month
    Feb_area_m2_reach = FebWidth*Length,  
    Mar_area_m2_reach = MarWidth*Length,  
    Apr_area_m2_reach = AprWidth*Length,  
    May_area_m2_reach = MayWidth*Length,  
    Jun_area_m2_reach = JunWidth*Length,  
    Jul_area_m2_reach = JulWidth*Length,  
    Aug_area_m2_reach = AugWidth*Length,  
    Sep_area_m2_reach = SepWidth*Length,  
    Oct_area_m2_reach = OctWidth*Length,  
    Nov_area_m2_reach = NovWidth*Length,  
    Dec_area_m2_reach = DecWidth*Length,  
    Jan_ch4_cv = Jan_ch4_sd/Jan_ch4ex*100,
    Feb_ch4_cv = Feb_ch4_sd/Feb_ch4ex*100,
    Mar_ch4_cv = Mar_ch4_sd/Mar_ch4ex*100,
    Apr_ch4_cv = Apr_ch4_sd/Apr_ch4ex*100,
    May_ch4_cv = May_ch4_sd/May_ch4ex*100,
    Jun_ch4_cv = Jun_ch4_sd/Jun_ch4ex*100,
    Jul_ch4_cv = Jul_ch4_sd/Jul_ch4ex*100,
    Aug_ch4_cv = Aug_ch4_sd/Aug_ch4ex*100,
    Sep_ch4_cv = Sep_ch4_sd/Sep_ch4ex*100,
    Oct_ch4_cv = Oct_ch4_sd/Oct_ch4ex*100,
    Nov_ch4_cv = Nov_ch4_sd/Nov_ch4ex*100,
    Dec_ch4_cv = Dec_ch4_sd/Dec_ch4ex*100,
    HYBAS_ID = as.character(HYBAS_ID)
    ) %>%   
  left_join(icecov, by = 'HYBAS_ID') %>% 
  mutate(across(c(Jan_ch4ex:Dec_ch4ex), ~ ifelse(is.na(.x) == TRUE , 0.1, .x))) %>% 
  mutate(across(ends_with("ch4_sd"), ~ ifelse(is.na(.x) == TRUE , 0.1, .x))) %>% 
  mutate(across(ends_with("k_sd"), ~ ifelse(is.na(.x) == TRUE , 0.1, .x))) 



rm(df)
gc()

#ch4 emissions for the whole reach, correcting by time dry and time under ice cover, and per month, not day
# here we do the MC ----

#here you set how big should the MC be
n_mc = 1000

vals_out <- tibble(rep= 1:n_mc,
                  flux= NA)


for(i in seq_along(vals_out$rep)){

flux_reaches <- df1 %>% 
    mutate(
      Jan_ch4ex = case_when(sens_var == "ch4" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), Jan_ch4ex + Jan_ch4_sd, Jan_ch4_sd),
                            sens_var == "ch4" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), Jan_ch4ex - Jan_ch4_sd, Jan_ch4_sd),
                            TRUE ~ rnorm(length(df1$COMID), Jan_ch4ex, Jan_ch4_sd)) %>% if_else(. < 0, 0, .),
      Feb_ch4ex = case_when(sens_var == "ch4" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), Feb_ch4ex + Feb_ch4_sd, Feb_ch4_sd),
                            sens_var == "ch4" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), Feb_ch4ex - Feb_ch4_sd, Feb_ch4_sd),
                            TRUE ~ rnorm(length(df1$COMID), Feb_ch4ex, Feb_ch4_sd)) %>% if_else(. < 0, 0, .),
      Mar_ch4ex = case_when(sens_var == "ch4" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), Mar_ch4ex + Mar_ch4_sd, Mar_ch4_sd),
                            sens_var == "ch4" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), Mar_ch4ex - Mar_ch4_sd, Mar_ch4_sd),
                            TRUE ~ rnorm(length(df1$COMID), Mar_ch4ex, Mar_ch4_sd)) %>% if_else(. < 0, 0, .),
      Apr_ch4ex = case_when(sens_var == "ch4" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), Apr_ch4ex + Apr_ch4_sd, Apr_ch4_sd),
                            sens_var == "ch4" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), Apr_ch4ex - Apr_ch4_sd, Apr_ch4_sd),
                            TRUE ~ rnorm(length(df1$COMID), Apr_ch4ex, Apr_ch4_sd)) %>% if_else(. < 0, 0, .),
      May_ch4ex = case_when(sens_var == "ch4" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), May_ch4ex + May_ch4_sd, May_ch4_sd),
                            sens_var == "ch4" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), May_ch4ex - May_ch4_sd, May_ch4_sd),
                            TRUE ~ rnorm(length(df1$COMID), May_ch4ex, May_ch4_sd)) %>% if_else(. < 0, 0, .),
      Jun_ch4ex = case_when(sens_var == "ch4" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), Jun_ch4ex + Jun_ch4_sd, Jun_ch4_sd),
                            sens_var == "ch4" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), Jun_ch4ex - Jun_ch4_sd, Jun_ch4_sd),
                            TRUE ~ rnorm(length(df1$COMID), Jun_ch4ex, Jun_ch4_sd)) %>% if_else(. < 0, 0, .),
      Jul_ch4ex = case_when(sens_var == "ch4" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), Jul_ch4ex + Jul_ch4_sd, Jul_ch4_sd),
                            sens_var == "ch4" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), Jul_ch4ex - Jul_ch4_sd, Jul_ch4_sd),
                            TRUE ~ rnorm(length(df1$COMID), Jul_ch4ex, Jul_ch4_sd)) %>% if_else(. < 0, 0, .),
      Aug_ch4ex = case_when(sens_var == "ch4" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), Aug_ch4ex + Aug_ch4_sd, Aug_ch4_sd),
                            sens_var == "ch4" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), Aug_ch4ex - Aug_ch4_sd, Aug_ch4_sd),
                            TRUE ~ rnorm(length(df1$COMID), Aug_ch4ex, Aug_ch4_sd)) %>% if_else(. < 0, 0, .),
      Sep_ch4ex = case_when(sens_var == "ch4" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), Sep_ch4ex + Sep_ch4_sd, Sep_ch4_sd),
                            sens_var == "ch4" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), Sep_ch4ex - Sep_ch4_sd, Sep_ch4_sd),
                            TRUE ~ rnorm(length(df1$COMID), Sep_ch4ex, Sep_ch4_sd)) %>% if_else(. < 0, 0, .),
      Oct_ch4ex = case_when(sens_var == "ch4" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), Oct_ch4ex + Oct_ch4_sd, Oct_ch4_sd),
                            sens_var == "ch4" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), Oct_ch4ex - Oct_ch4_sd, Oct_ch4_sd),
                            TRUE ~ rnorm(length(df1$COMID), Oct_ch4ex, Oct_ch4_sd)) %>% if_else(. < 0, 0, .),
      Nov_ch4ex = case_when(sens_var == "ch4" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), Nov_ch4ex + Nov_ch4_sd, Nov_ch4_sd),
                            sens_var == "ch4" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), Nov_ch4ex - Nov_ch4_sd, Nov_ch4_sd),
                            TRUE ~ rnorm(length(df1$COMID), Nov_ch4ex, Nov_ch4_sd)) %>% if_else(. < 0, 0, .),
      Dec_ch4ex = case_when(sens_var == "ch4" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), Dec_ch4ex + Dec_ch4_sd, Dec_ch4_sd),
                            sens_var == "ch4" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), Dec_ch4ex - Dec_ch4_sd, Dec_ch4_sd),
                            TRUE ~ rnorm(length(df1$COMID), Dec_ch4ex, Dec_ch4_sd)) %>% if_else(. < 0, 0, .),
      Jan_ch4F = case_when(sens_var == "k" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), Jan_k + Jan_k_sd, Jan_k_sd),
                            sens_var == "k" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), Jan_k - Jan_k_sd, Jan_k_sd),
                            TRUE ~ rnorm(length(df1$COMID), Jan_k, Jan_k_sd))*Jan_ch4ex*12/1000*(JanWidth >= 0.3),
      Feb_ch4F = case_when(sens_var == "k" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), Feb_k + Feb_k_sd, Feb_k_sd),
                            sens_var == "k" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), Feb_k - Feb_k_sd, Feb_k_sd),
                            TRUE ~ rnorm(length(df1$COMID), Feb_k, Feb_k_sd))*Feb_ch4ex*12/1000*(FebWidth >= 0.3),
      Mar_ch4F = case_when(sens_var == "k" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), Mar_k + Mar_k_sd, Mar_k_sd),
                           sens_var == "k" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), Mar_k - Mar_k_sd, Mar_k_sd),
                           TRUE ~ rnorm(length(df1$COMID), Mar_k, Mar_k_sd))*Mar_ch4ex*12/1000*(MarWidth >= 0.3),
      Apr_ch4F = case_when(sens_var == "k" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), Apr_k + Apr_k_sd, Apr_k_sd),
                           sens_var == "k" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), Apr_k - Apr_k_sd, Apr_k_sd),
                           TRUE ~ rnorm(length(df1$COMID), Apr_k, Apr_k_sd))*Apr_ch4ex*12/1000*(AprWidth >= 0.3),
      May_ch4F = case_when(sens_var == "k" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), May_k + May_k_sd, May_k_sd),
                           sens_var == "k" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), May_k - May_k_sd, May_k_sd),
                           TRUE ~ rnorm(length(df1$COMID), May_k, May_k_sd))*May_ch4ex*12/1000*(MayWidth >= 0.3),
      Jun_ch4F = case_when(sens_var == "k" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), Jun_k + Jun_k_sd, Jun_k_sd),
                           sens_var == "k" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), Jun_k - Jun_k_sd, Jun_k_sd),
                           TRUE ~ rnorm(length(df1$COMID), Jun_k, Jun_k_sd))*Jun_ch4ex*12/1000*(JunWidth >= 0.3),
      Jul_ch4F = case_when(sens_var == "k" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), Jul_k + Jul_k_sd, Jul_k_sd),
                           sens_var == "k" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), Jul_k - Jul_k_sd, Jul_k_sd),
                           TRUE ~ rnorm(length(df1$COMID), Jul_k, Jul_k_sd))*Jul_ch4ex*12/1000*(JulWidth >= 0.3),
      Aug_ch4F = case_when(sens_var == "k" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), Aug_k + Aug_k_sd, Aug_k_sd),
                           sens_var == "k" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), Aug_k - Aug_k_sd, Aug_k_sd),
                           TRUE ~ rnorm(length(df1$COMID), Aug_k, Aug_k_sd))*Aug_ch4ex*12/1000*(AugWidth >= 0.3),
      Sep_ch4F = case_when(sens_var == "k" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), Sep_k + Sep_k_sd, Sep_k_sd),
                           sens_var == "k" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), Sep_k - Sep_k_sd, Sep_k_sd),
                           TRUE ~ rnorm(length(df1$COMID), Sep_k, Sep_k_sd))*Sep_ch4ex*12/1000*(SepWidth >= 0.3),
      Oct_ch4F = case_when(sens_var == "k" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), Oct_k + Oct_k_sd, Oct_k_sd),
                           sens_var == "k" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), Oct_k - Oct_k_sd, Oct_k_sd),
                           TRUE ~ rnorm(length(df1$COMID), Oct_k, Oct_k_sd))*Oct_ch4ex*12/1000*(OctWidth >= 0.3),
      Nov_ch4F = case_when(sens_var == "k" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), Nov_k + Nov_k_sd, Nov_k_sd),
                           sens_var == "k" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), Nov_k - Nov_k_sd, Nov_k_sd),
                           TRUE ~ rnorm(length(df1$COMID), Nov_k, Nov_k_sd))*Nov_ch4ex*12/1000*(NovWidth >= 0.3),
      Dec_ch4F = case_when(sens_var == "k" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), Dec_k + Dec_k_sd, Dec_k_sd),
                           sens_var == "k" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), Dec_k - Dec_k_sd, Dec_k_sd),
                           TRUE ~ rnorm(length(df1$COMID), Dec_k, Dec_k_sd))*Dec_ch4ex*12/1000*(DecWidth >= 0.3),
      Jan_ch4E = case_when(sens_var == "area" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), JanWidth + JanWidth*JanQcv, JanWidth*JanQcv),
                           sens_var == "area" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), JanWidth - JanWidth*JanQcv, JanWidth*JanQcv),
                           TRUE ~ rnorm(length(df1$COMID),JanWidth, JanWidth*JanQcv))*Jan_ch4F*Length*(1-Jantimedryout)*(1-Jan_iceCov)*31,
      Feb_ch4E = case_when(sens_var == "area" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), FebWidth + FebWidth*FebQcv, FebWidth*FebQcv),
                           sens_var == "area" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), FebWidth - FebWidth*FebQcv, FebWidth*FebQcv),
                           TRUE ~ rnorm(length(df1$COMID),FebWidth, FebWidth*FebQcv))*Feb_ch4F*Length*(1-Febtimedryout)*(1-Feb_iceCov)*28,
      Mar_ch4E = case_when(sens_var == "area" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), MarWidth + MarWidth*MarQcv, MarWidth*MarQcv),
                           sens_var == "area" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), MarWidth - MarWidth*MarQcv, MarWidth*MarQcv),
                           TRUE ~ rnorm(length(df1$COMID),MarWidth, MarWidth*MarQcv))*Mar_ch4F*Length*(1-Martimedryout)*(1-Mar_iceCov)*31,
      Apr_ch4E = case_when(sens_var == "area" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), AprWidth + AprWidth*AprQcv, AprWidth*AprQcv),
                           sens_var == "area" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), AprWidth - AprWidth*AprQcv, AprWidth*AprQcv),
                           TRUE ~ rnorm(length(df1$COMID),AprWidth, AprWidth*AprQcv))*Apr_ch4F*Length*(1-Aprtimedryout)*(1-Apr_iceCov)*30,
      May_ch4E = case_when(sens_var == "area" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), MayWidth + MayWidth*MayQcv, MayWidth*MayQcv),
                           sens_var == "area" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), MayWidth - MayWidth*MayQcv, MayWidth*MayQcv),
                           TRUE ~ rnorm(length(df1$COMID),MayWidth, MayWidth*MayQcv))*May_ch4F*Length*(1-Maytimedryout)*(1-May_iceCov)*31,
      Jun_ch4E = case_when(sens_var == "area" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), JunWidth + JunWidth*JunQcv, JunWidth*JunQcv),
                           sens_var == "area" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), JunWidth - JunWidth*JunQcv, JunWidth*JunQcv),
                           TRUE ~ rnorm(length(df1$COMID),JunWidth, JunWidth*JunQcv))*Jun_ch4F*Length*(1-Juntimedryout)*(1-Jun_iceCov)*30,
      Jul_ch4E = case_when(sens_var == "area" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), JulWidth + JulWidth*JulQcv, JulWidth*JulQcv),
                           sens_var == "area" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), JulWidth - JulWidth*JulQcv, JulWidth*JulQcv),
                           TRUE ~ rnorm(length(df1$COMID),JulWidth, JulWidth*JulQcv))*Jul_ch4F*Length*(1-Jultimedryout)*(1-Jul_iceCov)*31,
      Aug_ch4E = case_when(sens_var == "area" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), AugWidth + AugWidth*AugQcv, AugWidth*AugQcv),
                           sens_var == "area" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), AugWidth - AugWidth*AugQcv, AugWidth*AugQcv),
                           TRUE ~ rnorm(length(df1$COMID),AugWidth, AugWidth*AugQcv))*Aug_ch4F*Length*(1-Augtimedryout)*(1-Aug_iceCov)*31,
      Sep_ch4E = case_when(sens_var == "area" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), SepWidth + SepWidth*SepQcv, SepWidth*SepQcv),
                           sens_var == "area" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), SepWidth - SepWidth*SepQcv, SepWidth*SepQcv),
                           TRUE ~ rnorm(length(df1$COMID),SepWidth, SepWidth*SepQcv))*Sep_ch4F*Length*(1-Septimedryout)*(1-Sep_iceCov)*30,
      Oct_ch4E = case_when(sens_var == "area" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), OctWidth + OctWidth*OctQcv, OctWidth*OctQcv),
                           sens_var == "area" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), OctWidth - OctWidth*OctQcv, OctWidth*OctQcv),
                           TRUE ~ rnorm(length(df1$COMID),OctWidth, OctWidth*OctQcv))*Oct_ch4F*Length*(1-Octtimedryout)*(1-Oct_iceCov)*31,
      Nov_ch4E = case_when(sens_var == "area" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), NovWidth + NovWidth*NovQcv, NovWidth*NovQcv),
                           sens_var == "area" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), NovWidth - NovWidth*NovQcv, NovWidth*NovQcv),
                           TRUE ~ rnorm(length(df1$COMID),NovWidth, NovWidth*NovQcv))*Nov_ch4F*Length*(1-Novtimedryout)*(1-Nov_iceCov)*30,
      Dec_ch4E = case_when(sens_var == "area" & sens_change == "plus1SD" ~ rnorm(length(df1$COMID), DecWidth + DecWidth*DecQcv, DecWidth*DecQcv),
                           sens_var == "area" & sens_change == "minus1SD" ~ rnorm(length(df1$COMID), DecWidth - DecWidth*DecQcv, DecWidth*DecQcv),
                           TRUE ~ rnorm(length(df1$COMID),DecWidth, DecWidth*DecQcv))*Dec_ch4F*Length*(1-Dectimedryout)*(1-Dec_iceCov)*31) %>% 
  select(COMID, wkbasin, subarea, Jan_ch4F:Dec_ch4F, Jan_ch4E:Dec_ch4E)
  
flux_extrap <-   small_streams_ba %>% 
    left_join(df1 %>% select(wkbasin, Jan_iceCov:Dec_iceCov), by = "wkbasin", multiple = "first") %>% 
    mutate(Jan_ch4E_extrap = rnorm(length(small_streams_ba$wkbasin), Jan_ch4E, Jan_ch4E_sd_extrap) *(1-Jan_iceCov), #correct extrapolated emissions by ice cover
           Feb_ch4E_extrap = rnorm(length(small_streams_ba$wkbasin), Feb_ch4E, Feb_ch4E_sd_extrap) *(1-Feb_iceCov),
           Mar_ch4E_extrap = rnorm(length(small_streams_ba$wkbasin), Mar_ch4E, Mar_ch4E_sd_extrap) *(1-Mar_iceCov),
           Apr_ch4E_extrap = rnorm(length(small_streams_ba$wkbasin), Apr_ch4E, Apr_ch4E_sd_extrap) *(1-Apr_iceCov),
           May_ch4E_extrap = rnorm(length(small_streams_ba$wkbasin), May_ch4E, May_ch4E_sd_extrap) *(1-May_iceCov),
           Jun_ch4E_extrap = rnorm(length(small_streams_ba$wkbasin), Jun_ch4E, Jun_ch4E_sd_extrap) *(1-Jun_iceCov),
           Jul_ch4E_extrap = rnorm(length(small_streams_ba$wkbasin), Jul_ch4E, Jul_ch4E_sd_extrap) *(1-Jul_iceCov),
           Aug_ch4E_extrap = rnorm(length(small_streams_ba$wkbasin), Aug_ch4E, Aug_ch4E_sd_extrap) *(1-Aug_iceCov),
           Sep_ch4E_extrap = rnorm(length(small_streams_ba$wkbasin), Sep_ch4E, Sep_ch4E_sd_extrap) *(1-Sep_iceCov),
           Oct_ch4E_extrap = rnorm(length(small_streams_ba$wkbasin), Oct_ch4E, Oct_ch4E_sd_extrap) *(1-Oct_iceCov),
           Nov_ch4E_extrap = rnorm(length(small_streams_ba$wkbasin), Nov_ch4E, Nov_ch4E_sd_extrap) *(1-Nov_iceCov),
           Dec_ch4E_extrap = rnorm(length(small_streams_ba$wkbasin), Dec_ch4E, Dec_ch4E_sd_extrap) *(1-Dec_iceCov)) 

tot_extrap <- flux_extrap %>% 
  pivot_longer(Jan_ch4E_extrap:Dec_ch4E_extrap, values_to = "flux", names_to = "type") %>% 
  summarise(total = sum(flux, na.rm = TRUE)/1e+12*16/12)  %>% #in TgCH4 / yr
  pull()


tot_reaches <- flux_reaches %>% 
  pivot_longer(Jan_ch4E:Dec_ch4E, values_to = "flux", names_to = "type") %>% 
  summarise(total = sum(flux, na.rm = TRUE)/1e+12*16/12)  %>% #in TgCH4 / yr
  pull()

  ## now get the estimate. For that, do the sum of the measured + extrapolated fluxes, for all months 
tot_flux <- tot_extrap + tot_reaches

vals_out$flux[i] <- tot_flux
print(paste0(i, ", flux: ", round(tot_flux,2)))

}


vals_out %>% 
  summarise(q.05 = quantile(flux, 0.05),
            median = quantile(flux, 0.5),
            mean = mean(flux),
            q.95 = quantile(flux, 0.95))

ggplot(vals_out)+
  geom_histogram(aes(flux))

#If it is the first time running this, comment the line below to first create the file, later you can append new lines
vals_all <- read_csv("figures/supplementary/uncertainty_mc.csv") %>% 
  bind_rows( vals_out %>% 
    mutate(type = sens_var, 
          change = sens_change )) %>% 
 write_csv("figures/supplementary/uncertainty_mc.csv")
#




#partitionng variance 

variance_function <- function(mean_a, sd_a, mean_b, sd_b, mean_c, sd_c, n){
  mc_a <- rnorm(n, mean_a, sd_a)
  mc_b <- rnorm(n, mean_b, sd_b)
  mc_c <- rnorm(n, mean_c, sd_c)
  
  mc_a <- ifelse(mc_a < 0, 0, mc_a)
  
  mc_prod <- mc_a*mc_b*mc_c
  
 aov_mc <-  aov(mc_prod ~ mc_a+mc_b+mc_c)

 summary_mc <- summary(aov_mc)
 
 sum_squares <- data.frame(summary_mc[[1]])[, 2, drop=FALSE]
 
 out <- sum_squares/sum(sum_squares)
 
 tibble(!!"ch4_var" := out[1,],
        !!"k_var" := out[2,], 
        !! "area_var" := out[3,])
 
 }

library(tictoc)

tic()
var_out <- df1 %>% 
  slice_sample(prop=.5) %>% 
  rowwise() %>% 
  mutate(
    Jan_var = variance_function(Jan_ch4ex, Jan_ch4_sd, Jan_k, Jan_k_sd, JanWidth, JanWidth*JanQcv, n = 1000 ), 
    Feb_var = variance_function(Feb_ch4ex, Feb_ch4_sd, Feb_k, Feb_k_sd, FebWidth, FebWidth*FebQcv, n = 1000 ),
    Mar_var = variance_function(Mar_ch4ex, Mar_ch4_sd, Mar_k, Mar_k_sd, MarWidth, MarWidth*MarQcv, n = 1000 ),
    Apr_var = variance_function(Apr_ch4ex, Apr_ch4_sd, Apr_k, Apr_k_sd, AprWidth, AprWidth*AprQcv, n = 1000 ),
    May_var = variance_function(May_ch4ex, May_ch4_sd, May_k, May_k_sd, MayWidth, MayWidth*MayQcv, n = 1000 ),
    Jun_var = variance_function(Jun_ch4ex, Jun_ch4_sd, Jun_k, Jun_k_sd, JunWidth, JunWidth*JunQcv, n = 1000 ),
    Jul_var = variance_function(Jul_ch4ex, Jul_ch4_sd, Jul_k, Jul_k_sd, JulWidth, JulWidth*JulQcv, n = 1000 ),
    Aug_var = variance_function(Aug_ch4ex, Aug_ch4_sd, Aug_k, Aug_k_sd, AugWidth, AugWidth*AugQcv, n = 1000 ),
    Sep_var = variance_function(Sep_ch4ex, Sep_ch4_sd, Sep_k, Sep_k_sd, SepWidth, SepWidth*SepQcv, n = 1000 ),
    Oct_var = variance_function(Oct_ch4ex, Oct_ch4_sd, Oct_k, Oct_k_sd, OctWidth, OctWidth*OctQcv, n = 1000 ),
    Nov_var = variance_function(Nov_ch4ex, Nov_ch4_sd, Nov_k, Nov_k_sd, NovWidth, NovWidth*NovQcv, n = 1000 ),
    Dec_var = variance_function(Dec_ch4ex, Dec_ch4_sd, Dec_k, Dec_k_sd, DecWidth, DecWidth*DecQcv, n = 1000 ),
    .keep = "none") 
toc()




var_unnested <- var_out %>% 
  unnest(c(Jan_var, Feb_var, Mar_var, Apr_var, May_var, Jun_var,
           Jul_var, Aug_var, Sep_var, Oct_var, Nov_var, Dec_var ), names_sep = "_" ) 
  
names(var_unnested) <- str_remove_all(names(var_unnested), "_var")


var_long <- var_unnested %>% 
  pivot_longer(everything(), names_to = c("month", "variable"), names_sep = "_", values_to = "variance")

var_long %>% 
  group_by(variable) %>% 
  summarise(mean=mean(variance)) 




