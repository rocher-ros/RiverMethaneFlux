# Load  and install packages ----
# List of all packages needed
package_list <- c('tidyverse', 'randomForest', 'nlme', 'googledrive' , 'corrr' )

# Check if there are any packacges missing
packages_missing <- setdiff(package_list, rownames(installed.packages()))

# If we find a package missing, install them
if(length(packages_missing) >= 1) install.packages(packages_missing) 

# Now load all the packages
lapply(package_list, require, character.only = TRUE)

# Add some custom functions ----

extremes_to_NA <- function(x){
  quantiles <- quantile( x, c(.025, .95 ), na.rm = TRUE )
  x[ x < quantiles[1] ] <- NA
  x[ x > quantiles[2] ] <- NA
  x
}
extremes_to_NA(1:50)


# Read files ----
## Download the GRiMe ----
if(file.exists("data/processed/grimeDB_concs_with_grade_attributes.csv") == TRUE) {
  print("files already downloaded")
} else {
  drive_download(
    "SCIENCE/PROJECTS/RiverMethaneFlux/processed/grimeDB_concs_with_grade_attributes.csv",
    path = "data/processed/grimeDB_concs_with_grade_attributes.csv",
    overwrite = TRUE
  )
}

grimeDB_attributes <- read_csv("data/processed/grimeDB_concs_with_grade_attributes.csv") 

colnames(grimeDB_attributes)
summary(grimeDB_attributes$CH4mean)

#histograms
grimeDB_attributes %>% 
  mutate(across(where(is.numeric), extremes_to_NA )) %>% 
  select(where(is.numeric)) %>% 
  pivot_longer(cols = everything(), names_to = "predictor", values_to = "value" ) %>% 
  ggplot(aes(value))+
  geom_histogram(bins = 80) +
  theme_classic()+
  facet_wrap(~predictor, scales='free')


vars_to_log <- c('CH4mean','uparea','popdens','slop','S_CACO3','S_CASO4' ,'T_OC', 'T_CACO3', 'T_CASO4', 
                 'T_ECE', 'k_month' )

grimeDB_attr_trans <- grimeDB_attributes %>%
  filter(distance_snapped < 1000) %>% 
  mutate(across(where(is.numeric), extremes_to_NA )) %>% 
  #group_by(COMID) %>% 
  #summarise(across(everything(), mean, na.rm=TRUE)) %>% 
  select( CH4mean,  GPP_yr:sresp_month, -area) %>% 
  mutate(across(.cols=vars_to_log, log)) 

summary(grimeDB_attr_trans$CH4mean)

#pearson correlations for CH4
corr_ch4 <- grimeDB_attr_trans %>% 
  correlate() %>% 
  focus(CH4mean)

corr_ch4 %>% arrange(desc(abs(CH4mean))) %>% print(n=52)


#correlations
grimeDB_attr_trans %>% 
  pivot_longer(GPP_yr:sresp_month, names_to = "predictor", values_to = "value" ) %>% 
  ggplot(aes(value, CH4mean))+
  geom_hex(bins = 30) +
  geom_smooth(method = "lm", se=FALSE, color="red3")+
  theme_classic()+
  scale_fill_continuous(type = "viridis", trans = "log10")+
  facet_wrap(~predictor, scales='free')



####histograms for  predictors####
cols_to_include<-c('lnCO2','area','popdens','tavg_00','prec_00','GPP_00','NPP_00',
                   'AR_ann','HR_ann','SR_00','wetland','elev','slop','SGRAVEL',
                   'SSAND','SSILT','SCLAY','SDENSITY','SOC','SpH','SCEC','SBS',
                   'STEB','SCACO3','SCASO4','SSODICITY','SCONDUCTIVITY')
grimeDB_attributes2<-ds[,which(names(ds) %in% cols_to_include)]
grimeDB_attributes2<-grimeDB_attributes2%>%gather(key='predictor',value=value)
grimeDB_attributes2<-grimeDB_attributes2[!is.na(grimeDB_attributes2$value),]

#rm extreme values
grimeDB_attributes2<-grimeDB_attributes2[!(grimeDB_attributes2$predictor=='SpH'&grimeDB_attributes2$value<quantile(ds$SpH,0.01,na.rm=TRUE)),] #rm SpH<3.7
grimeDB_attributes2<-grimeDB_attributes2[!(grimeDB_attributes2$predictor=='SDENSITY'&grimeDB_attributes2$value<1),] #rm SDENSITY<1
grimeDB_attributes2<-grimeDB_attributes2[!(grimeDB_attributes2$predictor=='SSODICITY'&grimeDB_attributes2$value>5),] #rm SSODICITY>5
grimeDB_attributes2<-grimeDB_attributes2[!(grimeDB_attributes2$predictor=='SCONDUCTIVITY'&grimeDB_attributes2$value>0.3),] #rm SCONDUCTIVITY>0.3

#format predictors
grimeDB_attributes2$predictor<-
  factor(grimeDB_attributes2$predictor,
         levels=c('lnCO2','tavg_00','prec_00',
                  'GPP_00','NPP_00','SR_00','AR_ann','HR_ann',
                  'area','slop','elev','wetland','popdens',
                  'SGRAVEL','SSAND','SSILT','SCLAY','SDENSITY',
                  'SOC','SpH','SCEC','SBS','STEB','SCACO3','SCASO4',
                  'SSODICITY','SCONDUCTIVITY'),
         labels = c('Ln CO2','Temperature (°C)','Precipitation (mm/yr)',
                    'GPP','NPP','Soil resp.','Autotrophic soil resp.','Heterotrophic soil resp.',
                    'Ln Watershed area (km2)','Ln Slope (unitless)','Ln Elevation (m)','Wetland (%)','Ln pop. dens. (people/km2)',
                    'Soil gravel (%v)','Soil sand (%w)','Soil silt (%w)','Soil clay (%w)','Soil density (kg/m3)',
                    'Ln SOC (%w)','Soil pH','Ln Soil CEC (cmol/kg)','Base saturation (%)','Ln Exch. bases (cmol/kg)',
                    'Ln Calcium carbonate (%w)','Ln Soil gypsum (%w)',
                    'Soil sodicity (%)','Soil conductivity (dS/m)'))

#histograms
grimeDB_attributes2%>% ggplot(aes(value))+
  geom_histogram(bins=50)+
  theme_classic()+
  theme(
    axis.title.x=element_blank(),
    panel.border=element_rect(colour="black",fill=NA,size=1),
    strip.background=element_blank(),
    strip.text=element_text(size=12,color='red')
  )+
  facet_wrap(.~predictor,scales='free')
# ggsave(paste0(dir,'/output/figure/co2/hist_pred_mon.png'),
#        width=30, height=20, units='cm')


####scatter plots####
cols_to_include<-c('lnCO2','tavg_00','prec_00','GPP_00','NPP_00','SR_00',
                   'AR_ann','HR_ann','wetland','popdens','area','slop','elev','SGRAVEL',
                   'SSAND','SSILT','SCLAY','SDENSITY','SOC','SpH','SCEC',
                   'SBS','STEB','SCACO3','SCASO4','SSODICITY','SCONDUCTIVITY')

grimeDB_attributes2<-ds[ds$popdens<log(300),cols_to_include]%>%
  gather(key='predictor',value=value,-lnCO2)
grimeDB_attributes2<-grimeDB_attributes2[!(is.na(grimeDB_attributes2$value)),]
grimeDB_attributes2<-grimeDB_attributes2[!(is.infinite(grimeDB_attributes2$value)),]
#rm extreme values
grimeDB_attributes2<-grimeDB_attributes2[!(grimeDB_attributes2$predictor=='SpH'&grimeDB_attributes2$value<3.7),] #rm SpH<3.7
grimeDB_attributes2<-grimeDB_attributes2[!(grimeDB_attributes2$predictor=='SDENSITY'&grimeDB_attributes2$value<1),] #rm SDENSITY<1
grimeDB_attributes2<-grimeDB_attributes2[!(grimeDB_attributes2$predictor=='SSODICITY'&grimeDB_attributes2$value>5),] #rm SSODICITY>5
grimeDB_attributes2<-grimeDB_attributes2[!(grimeDB_attributes2$predictor=='SCONDUCTIVITY'&grimeDB_attributes2$value>0.3),] #rm SCONDUCTIVITY>0.3

grimeDB_attributes2$predictor<-
  factor(grimeDB_attributes2$predictor,
         levels=c('tavg_00','prec_00',
                  'GPP_00','NPP_00','SR_00','AR_ann','HR_ann',
                  'area','slop','elev','wetland','popdens',
                  'SGRAVEL','SSAND','SSILT','SCLAY','SDENSITY',
                  'SOC','SpH','SCEC','SBS','STEB','SCACO3','SCASO4',
                  'SSODICITY','SCONDUCTIVITY'),
         labels = c('Temperature (°C)','Precipitation (mm/yr)',
                    'GPP','NPP','Soil resp.','Autotrophic soil resp.','Heterotrophic soil resp.',
                    'Ln Watershed area (km2)','Ln Slope (unitless)','Ln Elevation (m)','Wetland (%)','Ln Pop dens. (people/km)',
                    'Soil gravel (%v)','Soil sand (%w)','Soil silt (%w)','Soil clay (%w)','Soil density (kg/m3)',
                    'Ln SOC (%w)','Soil pH','Ln Soil CEC (cmol/kg)','Base saturation (%)','Ln Soil TEB (cmol/kg)',
                    'Ln Calcium carbonate (%w)','Ln Soil gypsum (%w)',
                    'Soil sodiciy (%)','Soil conductivity (dS/m)'))

lm_r2<-
  function(predictor){
    fit=lm(lnCO2~value,grimeDB_attributes2[(grimeDB_attributes2$predictor==predictor)&(!is.na(grimeDB_attributes2$predictor)),])
    fitsum<-summary(fit)
    r2=fitsum$adj.r.squared
    return(r2)
  }

pltText<-data.frame(R2=matrix(NA,26,1))
for(i in 1:26){
  print(i)
  r2=lm_r2(unique(grimeDB_attributes2$predictor)[i])
  pltText[i,c('predictor')]<-unique(grimeDB_attributes2$predictor)[i]
  pltText[i,c('R2')]<-round(r2,2)
  pltText[i,c('R2_Label')]<-
    as.character(as.expression(substitute(R^2*' = '*Rsqr, list(Rsqr=round(r2,2)))))
}

grimeDB_attributes2%>%
  ggplot(aes(value,lnCO2))+
  geom_point(alpha=0.2,size=2)+
  geom_smooth(method=lm)+
  scale_y_continuous(limits=c(3,10),breaks=seq(4,10,by=2))+
  labs(y=expression('Ln '*CO[2]*' ('*mu*'atm)'))+
  geom_label(data=pltText,
             mapping=aes(x=Inf,y=3.5,label=R2_Label),
             hjust=1.2, parse=TRUE)+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        panel.border=element_rect(colour = "black",fill=NA,size=1),
        strip.background=element_blank(),
        strip.text=element_text(size=12,color='red'))+
  facet_wrap(.~predictor,ncol=5,scales='free_x')
# ggsave(paste0(dir,'/output/figure/co2/linearRegressions_monMean.jpg'),
#        width=30,height=36,units='cm',device='jpeg')


####step-wise regression####
#1
ds[is.infinite(ds$popdens),]$popdens<-(-15)
m_min<-lm(lnCO2~1,data=ds)
m_max<-lm(lnCO2~tavg_00+prec_00+GPP_00+NPP_00+SR_00+
            slop+elev+wetland+popdens+
            SpH+SBS,data=ds)
m_red<-step(m_max,scope=c(upper=m_max,lower=m_min),
            trace=1)
summary(m_red) #0.4237
vif(m_red)
print(paste("AIC =",round(AIC(m_red),0)))
print(paste("N =",length(m_red$residuals)))

#2
m_min<-lm(lnCO2~1,data=ds)
m_max<-lm(lnCO2~tavg_00+prec_00+SR_00+
            slop+elev+wetland+popdens+
            SpH+SBS,data=ds)
m_red<-step(m_max,scope=c(upper=m_max,lower=m_min),
            trace=1)
summary(m_red) #0.4192
vif(m_red)
print(paste("AIC =",round(AIC(m_red),0)))

#3
m_min<-lm(lnCO2~1,data=ds)
m_max<-lm(lnCO2~tavg_00+prec_00+SR_00+
            slop+elev+wetland+popdens+
            SpH,data=ds)
m_red<-step(m_max,scope=c(upper=m_max,lower=m_min),
            trace=1)
summary(m_red) #0.4191
vif(m_red)
print(paste("AIC =",round(AIC(m_red),0)))

#4
m_min<-lm(lnCO2~1,data=ds)
m_max<-lm(lnCO2~prec_00+SR_00+
            slop+elev+wetland+popdens+
            SpH,data=ds)
m_red<-step(m_max,scope=c(upper=m_max,lower=m_min),
            trace=1)
summary(m_red) #0.4071
vif(m_red)
print(paste("AIC =",round(AIC(m_red),0)))

####lm model validation####
realpred<-
  data.frame(modelvalue=m_red[["fitted.values"]],
             modelres=m_red[["residuals"]],
             realvalue=m_red[["model"]][["lnCO2"]],
             soilResp=m_red[["model"]][["SR_00"]],
             slop=m_red[["model"]][["slop"]],
             elev=m_red[["model"]][["elev"]],
             wetland=m_red[["model"]][["wetland"]],
             SpH=m_red[["model"]][["SpH"]],
             ds[!(1:5910%in%unname(unclass(m_red$na.action))), 
                c('siteNo','SO','Lat','area','popdens','tavg_ann','prec_ann','GPP','NPP','AR_ann','HR_ann','SGRAVEL', 'SSAND','SBS')])

summary(lm(modelvalue~realvalue,data=realpred))#0.4075

#predictors versus residuals
realpredv<-
  realpred[,c('modelres','soilResp','slop','elev','wetland','SpH')]%>%
  gather(key=predictor,value=value,-modelres)

realpredv$predictor<-
  factor(realpredv$predictor,
         levels=c('soilResp','slop','elev','wetland','SpH'),
         labels=c('Soil resp.','Ln Slope (unitless)',
                  'Ln Elevation (m)', 'Wetland (%)','Soil pH'))
realpredv<-realpredv[!(realpredv$predictor=='Soil pH' & realpredv$value<4),]

realpredv%>%
  ggplot(aes(value,modelres))+
  geom_point(alpha=0.4,size=1)+
  geom_hline(yintercept=0,color='blue')+
  labs(y=expression('Residuals'))+
  theme_classic()+
  theme(axis.title.x=element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  facet_wrap(.~predictor,ncol=3,scales='free_x')
# ggsave(paste0(dir,'/output/figure/co2/residual_predictors_mon.png'),
#        width=24,height=12,units='cm',device='png')

#residuals versus non-predictors
realpredv<-
  realpred[,c('modelres','Lat','area','popdens','tavg_ann','prec_ann','GPP','NPP',
              'AR_ann','HR_ann','SGRAVEL','SSAND','SBS')]
# realpredv$popdens<-log(realpredv$popdens)
realpredv<-realpredv%>%gather(key=predictor,value=value,-modelres)

realpredv$predictor<-
  factor(realpredv$predictor,
         levels=c('Lat','area','popdens','tavg_ann','prec_ann','GPP','NPP',
                  'AR_ann','HR_ann','SGRAVEL','SSAND','SBS'),
         labels=c('Latitude','Watershed area','Pop density','Temperature','Precipitation','GPP','NPP',
                  'Autotrophic resp.', 'Heterotrophic resp.',
                  'Soil gravel (%v)','Soil sand (%w)', 'Soil base saturation'))

realpredv%>%
  ggplot(aes(value,modelres))+
  geom_point(alpha=0.4,size=1)+
  geom_hline(yintercept=0,color='blue')+
  labs(y=expression('Residuals'))+
  theme_classic()+
  theme(axis.title.x=element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  facet_wrap(.~predictor,ncol=3,scales='free_x')
# ggsave(paste0(dir,'/output/figure/co2/residual_non-predictors_mon.png'),
#        width=24,height=24,units='cm',device='png')

####Random Forest Model####
set.seed(0)
sa<-sample.split(1:5910,SplitRatio=0.75)
train<-subset(ds,sa)
test<-subset(ds,!sa)

#contruct the model
rfmod<-randomForest(lnCO2~tavg_00+prec_00+GPP_00+NPP_00+SR_00+area+
                      slop+elev+wetland+popdens+
                      SGRAVEL+SSAND+SOC+SpH+SBS,data=train,importance=TRUE,na.action=na.omit)

#model predictors importance
odr<-order(-rfmod$importance[,1])
importRF<-data.frame(rfmod$importance[odr,1])
importRF$pred<-row.names(importRF)
row.names(importRF)<-NULL
names(importRF)[1]<-'Import'
importRF$pred<-
  factor(importRF$pred,
         levels=c('slop','tavg_00','SR_00','elev','SpH','GPP_00',
                  'prec_00','NPP_00','SBS','SOC','popdens','SGRAVEL','area','SSAND','wetland'),
         labels=c('Slope','Temperature','Soil resp.','Elevation','Soil pH','GPP','Precipitation','NPP',
                  'Soil base saturation','SOC','Pop. dens.','Soil gravel',
                  'Watershed area','Soil sand','Wetland'))

importRF%>%ggplot(aes(x=pred,weight=Import))+
  geom_bar(fill='grey70')+
  labs(x='',y='Predictor importance')+
  theme_classic()+
  theme(panel.border=element_rect(fill=NA,size=1),
        axis.text.x=element_text(angle=90))