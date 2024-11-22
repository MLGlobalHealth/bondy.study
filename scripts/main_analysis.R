
# pre-amble ----
## load packages ----

require(data.table)
require(rstan)
require(brms)
require(here)
library(readxl)
library(ggplot2)
library(dplyr)
require(openxlsx)
library(stringr)
require(Hmisc)
library(lubridate)


# set directories ----

if(1){
  gitdir <- here()
  source(file.path(gitdir, "config.R"))
  source(file.path(gitdir, "scripts","functions.R"))
  args <- list(
    overwrite = T,
    analysis = '241010_abnormal_minus2',
    abnormal_thresh = -2
  )
}


# read in baseline data ----

sheets <- c("Table 1_BONDY 15-19","Table 2_BONDY 20-24", "Table 2_BONDY 25+")
cols <- c('Subject ID\r\n(BONDY/1519/XXX)','Ethnicity','Height\r\n(cm)','Weight (kg)','BMI',
          'Current CD4','Nadir CD4','Current viral load (<20,<200,other)','CDC C (yr) diagnoses',
          'CDC (yr) diagnoses','CDC  C (yr) diagnoses','DEXA- VFA',
          'Family Hx of bone disease','smoking\r\n(Y/N/ex)  ','alcohol\r\nunits/wk',
          'Please tell us the type and amount of physical activity involved in your work.',
          'Reduced mobilisation including hypertonic diplegia','Born in the UK/ if abroad year of entry in UK')
# TODO: ART regimens! - need to deal with merge rows. Load those separately to rest of data, dcast them, then merge 

db <- lapply(sheets, function(sheet) {
  
  col_names <- read.xlsx(xlsxFile = file.path(indir.data, file.path.bl.data), 
                         sheet = sheet, 
                         fillMergedCells = TRUE, 
                         skipEmptyRows = FALSE, 
                         skipEmptyCols = FALSE, 
                         colNames = FALSE)
  ifelse(is.na(col_names[1,1]), col_names <- col_names[2, ], col_names <- col_names[1, ])

  tmp <- read.xlsx(xlsxFile = file.path(indir.data, file.path.bl.data), 
                         sheet = sheet, 
                         fillMergedCells = FALSE, # False because there are merged rows for each participant
                         skipEmptyRows = FALSE, 
                         skipEmptyCols = FALSE, 
                         colNames = FALSE)
  if(is.na(col_names[1,1])){
    tmp <- tmp[-c(1:2), ]
  }else{
      
    tmp <- tmp[-1,]
    
  }
  colnames(tmp) <- col_names

  colnames(tmp) <- gsub('CDC \\(yr\\)', 'CDC C \\(yr\\)', colnames(tmp))
  colnames(tmp) <- gsub('CDC  C','CDC C',colnames(tmp))
  
  # Filter column names based on pattern matching
  colnames <- colnames(tmp)[colnames(tmp) %in% c(cols,paste0(cols,'.1'))]

  col_to_filter <- 'Subject ID\r\n(BONDY/1519/XXX)'
  
  tmp <- tmp[!is.na(tmp[[col_to_filter]]), colnames(tmp) %in% colnames]
  
  return(tmp)
})
db <- data.table(do.call(`rbind`,db))

# shorten variable names
setnames(db, c('Subject ID\r\n(BONDY/1519/XXX)','Subject ID\r\n(BONDY/1519/XXX).1','Ethnicity','Ethnicity.1',
               'Height\r\n(cm)','Weight (kg)','BMI',
               'Current CD4','Nadir CD4','Current viral load (<20,<200,other)','Current viral load (<20,<200,other).1',
               'CDC C (yr) diagnoses','CDC C (yr) diagnoses.1','DEXA- VFA',
               'Family Hx of bone disease','Family Hx of bone disease.1','smoking\r\n(Y/N/ex)  ','alcohol\r\nunits/wk',
               'Please tell us the type and amount of physical activity involved in your work.',
               'Reduced mobilisation including hypertonic diplegia','Reduced mobilisation including hypertonic diplegia.1',
               'Born in the UK/ if abroad year of entry in UK','Born in the UK/ if abroad year of entry in UK.1',
               'Born in the UK/ if abroad year of entry in UK.2','Born in the UK/ if abroad year of entry in UK.3'), 
         c('age_gp','pt_id','ethnicity','ethnicity_other',
           'height','weight','BMI','CD4','CD4_nadir','VL','VL_other','CDC_C','CDC_C_diag','BMD_z',
           'fam_hx_bone_disease','fam_hx_bone_disease_detail','smoking','alcohol_units',
           'phys_activity','reduced_mobility','reduced_mobility_detail',
           'foreign-born','foreign-born.1','foreign-born.2','foreign-born.3'))

# remove empty rows
db <- subset(db, pt_id!='select last 3')
columns_to_drop <- c('foreign-born.1','foreign-born.2','foreign-born.3')
db[`foreign-born`=='select choice', `foreign-born`:= 'Unknown']
db <- subset(db, select=setdiff(names(db), columns_to_drop))
db[pt_id=='18', pt_id:='018']

# make study ID to match results
db[, study_id:= paste0('BONDY/',age_gp,'/',pt_id)]
setcolorder(db, neworder = "study_id")

if(args$overwrite){saveRDS(db, file=file.path(indir.data,file.path.bl.data.clean))}

# read in study data ----

sheets <- c("15-19 y","15-19 year 2", "20-24 year 1", "20-24 year 2", "25+ baseline")
cols <- c('BONDY Study Number','BONDY ID','M/F','Date of scan','Visit 1', 'Height', 'Weight', 'Current CD4', 'BMDL2-L4','BMD L2-4 g/cm2',
          'BMC','Mean HipBMD','LS Zscore age matched','Tscore LS','Hip Zscore',
          'Vitamin D','PTH','Calcium','Phosphate','Alkaline phosphatase','NTX','P1NP',
          'Nadir CD4','Nadir CD4 (please review if changed from Visit 1)','Current viral load (<20,<200,other)',
          'Current viral load (<20,<200, other)','Detail','CDC-C diagnoses','CDC (yr) diagnoses','Reduced mobilisation','Reduced mobilisation inc hypertonic diplegia',
          'Previous TDF','Previous TDF exposure?','Duration on TDF (years)','Length of time on TDF (yrs)','Duration of TDF (years)',
          'Previous TAF','Previous TAF exposure?','Duration on TAF (months)','Length of time TAF (months)','Duration of TAF (months)',
          'Current ART 1','Current ART 2','Current ART 3','Current ART 4',
          'Current ART detail'
          )

dt <- lapply(sheets, function(sheet) {
  tmp <- data.table(read_excel(file.path(indir.data,file.path.study.data), sheet = sheet))
  if('BONDY ID' %in% colnames(tmp)) setnames(tmp, 'BONDY ID','BONDY Study Number')
  if('Visit 1' %in% colnames(tmp)) setnames(tmp, 'Visit 1','Date of scan')
  if('BMDL2-L4' %in% colnames(tmp)) setnames(tmp, 'BMDL2-L4','BMD L2-4 g/cm2')
  if('Length of time on TDF (yrs)' %in% colnames(tmp)) setnames(tmp, 'Length of time on TDF (yrs)','Duration on TDF (years)')
  if('Duration of TDF (years)' %in% colnames(tmp)) setnames(tmp, 'Duration of TDF (years)','Duration on TDF (years)')
  if('Length of time TAF (months)' %in% colnames(tmp)) setnames(tmp, 'Length of time TAF (months)','Duration on TAF (months)')
  if('Duration of TAF (months)' %in% colnames(tmp)) setnames(tmp, 'Duration of TAF (months)','Duration on TAF (months)')
  if('Reduced mobilisation inc hypertonic diplegia' %in% colnames(tmp)) setnames(tmp, 'Reduced mobilisation inc hypertonic diplegia','Reduced mobilisation')
  if('Previous TDF exposure?' %in% colnames(tmp)) setnames(tmp, 'Previous TDF exposure?','Previous TDF')
  if('Previous TAF exposure?' %in% colnames(tmp)) setnames(tmp, 'Previous TAF exposure?','Previous TAF')
  if('Nadir CD4 (please review if changed from Visit 1)' %in% colnames(tmp)) 
    setnames(tmp, 'Nadir CD4 (please review if changed from Visit 1)','Nadir CD4')
  colnames(tmp) <- gsub(' other','other',colnames(tmp))
  colnames(tmp) <- gsub('CDC \\(yr\\)', 'CDC-C', colnames(tmp))
  
  to_sub <- colnames(tmp)[grepl('ART detail',colnames(tmp))]
  if(length(to_sub)>0) setnames(tmp,to_sub,c('Current ART 1','Current ART 2','Current ART 3','Current ART 4'))
    
  colnames <- colnames(tmp)[colnames(tmp) %in% cols]
  tmp <- subset(tmp, select=colnames, !is.na(`M/F`))
  tmp[, age_gp_visit := sheet]
  #tmp[, Age_group := gsub('([0-9\\-\\+]+) ([a-z0 ]+)','\\1',age_gp_visit)]
  tmp[, age_group := gsub('([0-9+-]+).*', '\\1', age_gp_visit)]
  tmp[,     visit := ifelse(grepl('year 2',age_gp_visit), 'follow-up', 'baseline')]
  tmp[visit=='follow-up', `Previous TDF`:= NA] # note: no prev TDF in follow-up sheet
  #tmp[visit=='follow-up', `Duration on TDF (years)`:= NA] # note: no prev TDF in follow-up sheet
  tmp[visit=='follow-up', `Previous TAF`:= NA] # note: no prev TDF in follow-up sheet
  #tmp[visit=='follow-up', `Duration on TAF (months)`:= NA] # note: no prev TDF in follow-up sheet
  tmp[visit=='follow-up', `CDC-C diagnoses`:= NA] # note: no prev TDF in follow-up sheet
  tmp[visit=='follow-up', `Reduced mobilisation`:= NA] # note: no prev TDF in follow-up sheet
  
  setnames(tmp, old = "Current viral load (<20,<200,other)", new = "VL")
  
  tmp[VL %in% c('select choice','Other - Please specify'), VL:= Detail]
  
  # re-order columns to match
  tmp <- tmp[, c('BONDY Study Number','Date of scan','age_group','visit','M/F','Height','Weight',
                 'BMD L2-4 g/cm2','LS Zscore age matched','Tscore LS','Hip Zscore',
                 'Vitamin D','PTH','Calcium','Phosphate','Alkaline phosphatase','NTX','P1NP',
                 'Previous TDF','Duration on TDF (years)','Previous TAF','Duration on TAF (months)',
                 'Current CD4','Nadir CD4','VL',
                 'CDC-C diagnoses','Reduced mobilisation',
          'Current ART 1','Current ART 2','Current ART 3','Current ART 4')]
  
  return(tmp)
})
dt <- do.call(`rbind`,dt)

setnames(dt, c('BONDY Study Number','M/F','Date of scan','Height','Weight','BMD L2-4 g/cm2','Current CD4','LS Zscore age matched',
               'Tscore LS','Hip Zscore','Vitamin D','Calcium','Phosphate','Alkaline phosphatase','Previous TDF',
               'Duration on TDF (years)','Previous TAF','Duration on TAF (months)',
               'Current ART 1','Current ART 2','Current ART 3','Current ART 4','Nadir CD4','VL',
               'CDC-C diagnoses','Reduced mobilisation'), 
         c('study_id','sex','date_scan','height','weight','BMD','CD4','BMD_LS_Z',
           'tscore_LS','hip_Z','vitD','calcium','phosphate','alkaline',
           'TDF_exp','TDF_duration','TAF_exp','TAF_duration',
           'ART1','ART2','ART3','ART4',
           'CD4_nadir','VL','CDC_C','reduced_mobility'))

if(nrow(dt)!=230){
  cat(paste0('\n STOP! some patients are missing\n'))
}
if(args$overwrite){saveRDS(dt, file=file.path(indir.data,file.path.results.data.clean))}

# merge additional covariates to main table ----

db <- readRDS(file=file.path(indir.data,file.path.bl.data.clean))
dt <- readRDS(file=file.path(indir.data,file.path.results.data.clean))

dt <- merge(dt,subset(db,select=c('study_id','ethnicity','BMI','fam_hx_bone_disease',
                                  'smoking','alcohol_units','phys_activity','BMD_z','foreign-born')),
            by='study_id',all.x=T)

dt <- clean_data(dt)

# make baseline characteristics table ----

tmp <- subset(dt, visit=='baseline')

tab2 <- make_summary_table_age(tmp,grouping='by_age')
tab <- make_summary_table_age(tmp,grouping='overall')
tab <- merge(tab,tab2,by=c('var','group'),all=T)

write.csv(tab,file=file.path(indir.data,'results',args$analysis,table.characteristics))
saveRDS(tab,file=file.path(indir.data,'results',args$analysis,gsub('.csv','.RDS',table.characteristics)))

# plot BMD at baseline visit ----

g <- ggplot(subset(dt, visit=='baseline'), aes(x = age_group, y = BMD)) +
  geom_jitter(width = 0.2, size = 2, color = "darkblue", alpha = 0.6) +
  stat_summary(fun.data = function(y) {
    data.frame(ymin = quantile(y, 0.25), ymax = quantile(y, 0.75), y = median(y))
  }, geom = "errorbar", width = 0.25, color = "forestgreen", size = 0.9) +
  stat_summary(fun.data = function(y) {
    data.frame(ymin = quantile(y, 0.5), ymax = quantile(y, 0.5), y = median(y))
  }, geom = "errorbar", width = 0.3, color = "orange", size = 0.9) +
  #stat_summary(fun = median, geom = "point", size = 5, color = "orange", shape = 3) +
  labs(
    #title = "LS BMD at baseline",
    x = "Age group",
    y = "LS BMD at baseline (g/cm2)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 13)
  )
ggsave(file=file.path(indir.data,'results',args$analysis,'plot_LS_BMD_bl.png'),g,h=3,w=4,bg="white")

g <- ggplot(subset(dt, visit=='baseline'), aes(x = age_group, y = BMD_LS_Z)) +
  geom_hline(aes(yintercept=-1),colour='red',linetype='dashed',linewidth=0.7) +
  geom_jitter(width = 0.2, size = 2, color = "darkblue", alpha = 0.6) +
  stat_summary(fun.data = function(y) {
    data.frame(ymin = quantile(y, 0.25), ymax = quantile(y, 0.75), y = median(y))
  }, geom = "errorbar", width = 0.25, color = "forestgreen", size = 0.9) +
  stat_summary(fun.data = function(y) {
    data.frame(ymin = quantile(y, 0.5), ymax = quantile(y, 0.5), y = median(y))
  }, geom = "errorbar", width = 0.3, color = "orange", size = 0.9) +
  #stat_summary(fun = median, geom = "point", size = 5, color = "orange", shape = 3) +
  labs(
    #title = "LS BMD at baseline",
    x = "Age group",
    y = "LS BMD at baseline (Z score)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 13)
  )
ggsave(file=file.path(indir.data,'results',args$analysis,'plot_LS_BMD_zscore_bl.png'),g,h=3,w=4,bg="white")

g <- ggplot(subset(dt, visit=='baseline'), aes(x = age_group, y = tscore_LS)) +
  geom_hline(aes(yintercept=-1),colour='red',linetype='dashed',linewidth=0.7) +
  geom_jitter(width = 0.2, size = 2, color = "darkblue", alpha = 0.6) +
  stat_summary(fun.data = function(y) {
    data.frame(ymin = quantile(y, 0.25), ymax = quantile(y, 0.75), y = median(y))
  }, geom = "errorbar", width = 0.25, color = "forestgreen", size = 0.9) +
  stat_summary(fun.data = function(y) {
    data.frame(ymin = quantile(y, 0.5), ymax = quantile(y, 0.5), y = median(y))
  }, geom = "errorbar", width = 0.3, color = "orange", size = 0.9) +
  #stat_summary(fun = median, geom = "point", size = 5, color = "orange", shape = 3) +
  labs(
    #title = "LS BMD at baseline",
    x = "Age group",
    y = "LS BMD at baseline (T score)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15),
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 13)
  )
ggsave(file=file.path(indir.data,'results',args$analysis,'plot_LS_BMD_Tscore_bl.png'),g,h=3,w=4,bg="white")

# define abnormal BMD Z-score for age ----
#dt[, abnormal_bmd:= ifelse(BMD_LS_Z<= -1 ,1,0)]
#dt[, abnormal_bmd:= factor(ifelse(grepl('osteo|OSTE|osto|z=-1|z=-2|z score=          -1',BMD_z)|BMD_LS_Z<= -1,1,0),
#                           levels=c(0,1),labels=c('Normal','Abnormal'))]
dt[, abnormal_bmd:=factor(ifelse(BMD_LS_Z<=args$abnormal_thresh | (!is.na(hip_Z) & hip_Z<=args$abnormal_thresh), 1, 0),levels=c(0,1),labels=c('Normal','Abnormal'))]

#dt[, abnormal_bmd:= ifelse(BMD_LS_Z<= -1 | tscore_LS<= -1,1,0)]
dt[, osteoporosis:= ifelse(BMD_LS_Z<= -2.5,1,0)] # note - need to specify less than or equal to in write-up!

# abnormal BMD score
table(dt$visit,dt$abnormal_bmd)
dt[visit=='baseline',table(age_group,abnormal_bmd)]

dt[!is.na(smoking), smoke:= ifelse(smoking %in% c('No','Ex-smoker'),'No_ex','Yes')]
dt[!is.na(smoking), ever_smoke:= ifelse(smoking =='No','No','Yes')]
dt[, bmi_cat:= cut(BMI,breaks=c(0,18.5,25,30,50),include.lowest=T,right=F,
                    labels=c('<18.5','[18.5-25)','[25-30)','30+'))]
dt[, alcohol:= ifelse(alcohol_units>0 ,'Yes','No')]
dt[, abnormal_vitD:= ifelse(vitD<= 50,1,0)]
dt[, high_PTH:= ifelse(PTH>= 7.2,1,0)]
dt[, PA:= substr(phys_activity,1,2)]
dt[, cdcc_lowvl:= ifelse(CDC_C=='Yes' | (!is.na(CD4_nadir) & CD4_nadir<200) , 1,0)]

tmp <- subset(dt,visit=='baseline')
dbas <- subset(dt,visit == 'baseline')

saveRDS(dt,file=file.path(indir.data,'results',args$analysis,'cleaned_data.RDS'))
write.csv(dt,file=file.path(indir.data,'results',args$analysis,'cleaned_data.csv'))

# summarise missing values
list_cov <- c('abnormal_bmd','age_group','CDC_C','TDF_exp','ART_reg',
              'fam_hx_bone_disease','PA','reduced_mobility','bmi_cat','smoke',
              'alcohol','abnormal_vitD','high_PTH')
for(i in list_cov){
  cat(paste0('\n',i,' ',nrow(tmp[is.na(get(i)),])))
}

tab <- make_visit_summary_table(tmp)

write.csv(tab,file=file.path(indir.data,'results',args$analysis,table.predictors.bmd))
saveRDS(tab,file=file.path(indir.data,'results',args$analysis,gsub('.csv','.RDS',table.predictors.bmd)))


tab <- make_visit_summary_table(subset(dt,visit=='follow-up'))
setnames(tab,'NA','No follow-up visit')

write.csv(tab,file=file.path(indir.data,'results',args$analysis,table.predictors.bmd.fu))
saveRDS(tab,file=file.path(indir.data,'results',args$analysis,gsub('.csv','.RDS',table.predictors.bmd.fu)))

# fit univariable models ----
# intercept: N(0,5)
# covariate coefficients: N(0,1)
generic_priors <- c(
  set_prior("normal(0, 5)", class = "Intercept"),
  set_prior("normal(0, 1)", class = "b")
)
# re-order factor levels
dbas[, ART_reg:= factor(ART_reg,levels=c('TDF','bPI_2NRTI','INSTI_2NRTI','NNRTI_2NRTI','TAF','None','Unknown'))]
dbas[, bmi_cat:= factor(bmi_cat,levels=c('[18.5-25)','<18.5', '[25-30)','30+'))]

model_age <- fit_univariable_model(abnormal_bmd ~ age_group, data=dbas, prior=T)
model_cdcc <- fit_univariable_model(abnormal_bmd ~ cdcc_lowvl, data=dbas, prior=T)
model_tdf <- fit_univariable_model(abnormal_bmd ~ TDF_exp, data=subset(dbas,TDF_exp %in% c('N','Y')), prior=T)
model_tdf_length <- fit_univariable_model(abnormal_bmd ~ TDF_duration , data=dbas, prior=T)
model_art <- fit_univariable_model(abnormal_bmd ~ ART_reg, data=subset(dbas,!ART_reg %in% c('None','Unknown','TAF')), prior=T)
model_fhb <- fit_univariable_model(abnormal_bmd ~ fam_hx_bone_disease, data=subset(dbas,fam_hx_bone_disease %in% c('No','Yes')), prior=T)
model_mob <- fit_univariable_model(abnormal_bmd ~ reduced_mobility, data=subset(dbas,reduced_mobility %in% c('No','Yes')), prior=T)
model_bmi_cat <- fit_univariable_model(abnormal_bmd ~ bmi_cat, data=dbas, prior=T)
# TDF years
model_smoking <- fit_univariable_model(abnormal_bmd ~ smoke, data=subset(dbas,smoke %in% c('No_ex','Yes')), prior=T)
model_eversmoked <- fit_univariable_model(abnormal_bmd ~ ever_smoke, data=subset(dbas,ever_smoke %in% c('No','Yes')), prior=T)
model_alc <- fit_univariable_model(abnormal_bmd ~ alcohol, data=dbas, prior=T)
model_vitD <- fit_univariable_model(abnormal_bmd ~ abnormal_vitD, data=dbas, prior=T)
model_pth <- fit_univariable_model(abnormal_bmd ~ high_PTH, data=dbas, prior=T)

model_age[[2]]$model <- 'Age'
model_cdcc[[2]]$model <- 'CDCC'
model_tdf[[2]]$model <- 'TDF'
model_tdf_length[[2]]$model <- 'TDF_length'
model_art[[2]]$model <-  'ART'
model_fhb[[2]]$model <-  'FHB'
model_mob[[2]]$model <-  'Mobility'
model_bmi_cat[[2]]$model <-  'BMI'
model_smoking[[2]]$model <-  'Smoking'
model_eversmoked[[2]]$model <-  'Ever smoked'
model_alc[[2]]$model <-  'Alcohol'
model_vitD[[2]]$model <-  'VitaminD'
model_pth[[2]]$model <-  'PTH'

coeffs <- rbind(model_age[[2]],
                model_cdcc[[2]],
                model_tdf[[2]],
                model_tdf_length[[2]],
                model_art[[2]],
                model_fhb[[2]],
                model_mob[[2]],
                model_bmi_cat[[2]],
                model_smoking[[2]],
                model_eversmoked[[2]],
                model_alc[[2]],
                model_vitD[[2]],
                model_pth[[2]])
coeffs <- data.table(subset(coeffs,Term!='Intercept'))
coeffs[Term=='age_group20M24', Term:= 'Age: 20-24 vs. 15-19']
coeffs[Term=='age_group25P', Term:= 'Age: 25+ vs. 15-19']
coeffs[Term=='cdcc_lowvl', Term:= 'Prior CDC C,\nCD4% 200 cells/ul\nor <20%']
coeffs[Term=='TDF_expY', Term:= 'Prior TDF exposure']
coeffs[Term=='ART_regINSTI_2NRTI', Term:= 'ART reg: INSTI vs. TDF']
coeffs[Term=='ART_regbPI_2NRTI', Term:= 'ART reg: PI vs. TDF']
coeffs[Term=='ART_regNNRTI_2NRTI', Term:= 'ART reg: NNRTI vs. TDF']
coeffs[Term=='fam_hx_bone_diseaseYes', Term:= 'Family history of\nbone disease']
coeffs[Term=='reduced_mobilityYes', Term:= 'Reduced mobilisation']
coeffs[Term=='bmi_cat<18.5', Term:= 'BMI: <18.5 vs. 18.5-25']
coeffs[Term=='bmi_cat25M30', Term:= 'BMI: 25-30 vs. 18.5-25']
coeffs[Term=='bmi_cat30P', Term:= 'BMI: 30+ vs. 18.5-25']
coeffs[Term=='smokeYes', Term:= 'Smoker: Current vs. never/ex']
coeffs[Term=='ever_smokeYes', Term:= 'Smoker: Ever smoked vs. never']
coeffs[Term=='alcoholYes', Term:= 'Drinks alcohol']
coeffs[Term=='abnormal_vitD', Term:= 'Vitamin D <= 50 nmol/L']
coeffs[Term=='high_PTH', Term:= 'PTH >= 7.2 pmol/L']

pal <- pal_npg('nrc')(10)
pal2 <- pal_nejm('default')(8)

coeffs[, Term:= factor(Term, levels=rev(unique(coeffs$Term)))]

## plot coeffs ----
g <- ggplot(coeffs, aes(x = Estimate, y = Term, color = model)) +
  geom_point(size = 4) +
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.2) +
  scale_color_manual(values=c(pal,pal2)) + 
  xlab("Odds Ratio") +
  ylab("Predictor") +
  scale_x_log10() +
  theme_minimal() +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey30") +
  ggtitle("Forest Plot of Predictors from Univariable\nBayesian Logistic Regression Models") +
  theme(legend.position="none")
ggsave(file=file.path(indir.data,'results',args$analysis,'forest_plot_LS_BMD_bl_univariable_priors_N02.png'),g,h=6,w=5.5,bg="white")

## save stanfits ----
save(model_age, model_cdcc,
     model_tdf,
     model_tdf_length,
     model_art,
     model_fhb,
     model_mob,
     model_bmi_cat,
     model_smoking,
     model_eversmoked,
     model_alc,
     model_vitD,
     model_pth, file=file.path(indir.data,'results',args$analysis,'univariable_models_priors_N02_stanfit.rdata'))

list_pred <- c('age','cdcc','tdf','tdf_duration','art','fhb','mob','bmi','smoke','ever_smoke','alcohol','vitD','pth')
names_models <- list(model_age[[3]],
                     model_cdcc[[3]],
                     model_tdf[[3]],
                     model_tdf_length[[3]],
                     model_art[[3]],
                     model_fhb[[3]],
                     model_mob[[3]],
                     model_bmi_cat[[3]],
                     model_smoking[[3]],
                     model_eversmoked[[3]],
                     model_alc[[3]],
                     model_vitD[[3]],
                     model_pth[[3]])
for(i in seq_along(names_models)){
  names_models[[i]][, model_string:= paste(deparse(model), collapse = "")]
  names_models[[i]][, model:=NULL]
}
tab <- do.call(rbind,names_models)
tab <- unique(tab)
tab[, model:= list_pred]

write.csv(tab,file = file.path(indir.data,'results',args$analysis,'univariable_models_priors_N02_ESS_divergences.csv'))

## save posterior probs OR>1 for each coefficient ----

model_age[[4]]$model <- 'Age'
model_cdcc[[4]]$model <- 'CDCC'
model_tdf[[4]]$model <- 'TDF'
model_tdf_length[[4]]$model <- 'TDF_length'
model_art[[4]]$model <-  'ART'
model_fhb[[4]]$model <-  'FHB'
model_mob[[4]]$model <-  'Mobility'
model_bmi_cat[[4]]$model <-  'BMI'
model_smoking[[4]]$model <-  'Smoking'
model_eversmoked[[4]]$model <-  'Ever smoked'
model_alc[[4]]$model <-  'Alcohol'
model_vitD[[4]]$model <-  'VitaminD'
model_pth[[4]]$model <-  'PTH'

coeffs <- rbind(model_age[[4]],
                model_cdcc[[4]],
                model_tdf[[4]],
                model_tdf_length[[4]],
                model_art[[4]],
                model_fhb[[4]],
                model_mob[[4]],
                model_bmi_cat[[4]],
                model_smoking[[4]],
                model_eversmoked[[4]],
                model_alc[[4]],
                model_vitD[[4]],
                model_pth[[4]])
coeffs <- data.table(coeffs)
coeffs[,Term:= gsub('b_','',variable)]
coeffs[Term=='age_group20M24', Term:= 'Age: 20-24 vs. 15-19']
coeffs[Term=='age_group25P', Term:= 'Age: 25+ vs. 15-19']
coeffs[Term=='cdcc_lowvl', Term:= 'Prior CDC C,\nCD4% 200 cells/ul\nor <20%']
coeffs[Term=='TDF_expY', Term:= 'Prior TDF exposure']
coeffs[Term=='ART_regINSTI_2NRTI', Term:= 'ART reg: INSTI vs. TDF']
coeffs[Term=='ART_regbPI_2NRTI', Term:= 'ART reg: PI vs. TDF']
coeffs[Term=='ART_regNNRTI_2NRTI', Term:= 'ART reg: NNRTI vs. TDF']
coeffs[Term=='fam_hx_bone_diseaseYes', Term:= 'Family history of\nbone disease']
coeffs[Term=='reduced_mobilityYes', Term:= 'Reduced mobilisation']
coeffs[Term=='bmi_cat<18.5', Term:= 'BMI: <18.5 vs. 18.5-25']
coeffs[Term=='bmi_cat25M30', Term:= 'BMI: 25-30 vs. 18.5-25']
coeffs[Term=='bmi_cat30P', Term:= 'BMI: 30+ vs. 18.5-25']
coeffs[Term=='smokeYes', Term:= 'Smoker: Current vs. never/ex']
coeffs[Term=='ever_smokeYes', Term:= 'Smoker: Ever smoked vs. never']
coeffs[Term=='alcoholYes', Term:= 'Drinks alcohol']
coeffs[Term=='abnormal_vitD', Term:= 'Vitamin D <= 50 nmol/L']
coeffs[Term=='high_PTH', Term:= 'PTH >= 7.2 pmol/L']

write.csv(coeffs,file = file.path(indir.data,'results',args$analysis,'univariable_models_priors_N02_pvalues.csv'))

# change in BMD ----

dc <- subset(dt,visit=='baseline')
df <- subset(dt,visit=='follow-up', select=c('study_id','date_scan','BMD','BMD_LS_Z','TAF_duration'))
dc <- merge(dc,df,by='study_id')
setnames(dc,c('date_scan.x','date_scan.y','BMD.x','BMD.y','BMD_LS_Z.x','BMD_LS_Z.y','TAF_duration.x','TAF_duration.y'),
         c('date_scan_bl','date_scan_fu','BMD_bl','BMD_fu','BMD_LS_Z_bl','BMD_LS_Z_fu','TAF_duration_bl','TAF_duration_fu'))

dc[, change_BMD_raw:= BMD_fu - BMD_bl]
dc[, change_BMD_LS:= BMD_LS_Z_fu - BMD_LS_Z_bl]

# remove participants with no follow-up scan
dc <- subset(dc,!is.na(BMD_LS_Z_fu))

# only 20-24yo have hip scores at both visits
df <- subset(dt,visit=='follow-up', select=c('study_id','hip_Z'))
dc <- merge(dc,df,by='study_id')
setnames(dc,c('hip_Z.x','hip_Z.y'),c('BMD_hip_Z_bl','BMD_hip_Z_fu'))
dc[, change_BMD_hip:= BMD_hip_Z_fu - BMD_hip_Z_bl]

# re-order factors
dc[, ART_reg:= factor(ART_reg,levels=c('TAF','bPI_2NRTI','INSTI_2NRTI','NNRTI_2NRTI','TDF','None','Unknown'))]
dc[, bmi_cat:= factor(bmi_cat,levels=c('<18.5', '[18.5-25)', '[25-30)','30+'))]

# TAF vs. TDF
dc[, TAF_TDF:= ifelse(ART_reg=='TAF','TAF',ifelse(ART_reg=='TDF','TDF','Non_TAF_TDF'))]
dc[, TAF_TDF:= factor(TAF_TDF,levels=c('TDF','TAF','Non_TAF_TDF'))]

## fit LMs (adjusted for baseline BMD) ----

model_adj_taf_tdf <- fit_linear_model(change_BMD_LS ~ TAF_TDF + BMD_LS_Z_bl, subset(dc,TAF_TDF %in% c('TAF','TDF','Non_TAF_TDF')), prior=T)

model_adj_age <- fit_linear_model(change_BMD_LS ~ age_group + BMD_LS_Z_bl, dc, prior=T)
model_adj_cdcc <- fit_linear_model(change_BMD_LS ~ cdcc_lowvl + BMD_LS_Z_bl, dc, prior=T)
model_adj_tdf <- fit_linear_model(change_BMD_LS ~ TDF_exp + BMD_LS_Z_bl, subset(dc,TDF_exp %in% c('N','Y')), prior=T)
model_adj_tdf_length <- fit_linear_model(change_BMD_LS ~ TDF_duration + BMD_LS_Z_bl, subset(dc,TDF_exp %in% c('N','Y')), prior=T)
model_adj_art <- fit_linear_model(change_BMD_LS ~ ART_reg + BMD_LS_Z_bl, subset(dc,!ART_reg %in% c('None','Unknown','TDF')), prior=T)
model_adj_fhb <- fit_linear_model(change_BMD_LS ~ fam_hx_bone_disease + BMD_LS_Z_bl, subset(dc,fam_hx_bone_disease %in% c('No','Yes')), prior=T)
model_adj_mob <- fit_linear_model(change_BMD_LS ~ reduced_mobility + BMD_LS_Z_bl, subset(dc,reduced_mobility %in% c('No','Yes')), prior=T)
model_adj_bmi_cat <- fit_linear_model(change_BMD_LS ~ bmi_cat + BMD_LS_Z_bl, dc, prior=T)
model_adj_smoking <- fit_linear_model(change_BMD_LS ~ smoke + BMD_LS_Z_bl, subset(dc,smoke %in% c('No_ex','Yes')), prior=T)
model_adj_ever_smoked <- fit_linear_model(change_BMD_LS ~ ever_smoke + BMD_LS_Z_bl, subset(dc,ever_smoke %in% c('No','Yes')), prior=T)
model_adj_alc <- fit_linear_model(change_BMD_LS ~ alcohol + BMD_LS_Z_bl, dc, prior=T)
model_adj_vitD <- fit_linear_model(change_BMD_LS ~ abnormal_vitD + BMD_LS_Z_bl, dc, prior=T)
model_adj_pth <- fit_linear_model(change_BMD_LS ~ high_PTH + BMD_LS_Z_bl, dc, prior=T)

model_adj_age[[2]]$model <- 'Age'
model_adj_cdcc[[2]]$model <- 'CDCC'
model_adj_tdf[[2]]$model <- 'TDF'
model_adj_tdf_length[[2]]$model <- 'TDF_duration'
model_adj_art[[2]]$model <-  'ART'
model_adj_fhb[[2]]$model <-  'FHB'
model_adj_mob[[2]]$model <-  'Mobility'
model_adj_bmi_cat[[2]]$model <-  'BMI'
model_adj_smoking[[2]]$model <-  'Smoking'
model_adj_ever_smoked[[2]]$model <-  'Ever smoked'
model_adj_alc[[2]]$model <-  'Alcohol'
model_adj_vitD[[2]]$model <-  'VitaminD'
model_adj_pth[[2]]$model <-  'PTH'

coeffs <- rbind(model_adj_age[[2]],
                model_adj_cdcc[[2]],
                model_adj_tdf[[2]],
                model_adj_tdf_length[[2]],
                model_adj_art[[2]],
                model_adj_fhb[[2]],
                model_adj_mob[[2]],
                model_adj_bmi_cat[[2]],
                model_adj_smoking[[2]],
                model_adj_ever_smoked[[2]],
                model_adj_alc[[2]],
                model_adj_vitD[[2]],
                model_adj_pth[[2]])
coeffs <- data.table(subset(coeffs,Term!='Intercept'))
coeffs <- data.table(subset(coeffs,!grepl('ethnicity', Term)))
coeffs <- data.table(subset(coeffs,!grepl('sex', Term)))
coeffs <- data.table(subset(coeffs,!(grepl('age_group', Term) & model!='Age')))
coeffs <- data.table(subset(coeffs,!grepl('BMD_LS_Z_bl', Term)))

coeffs[Term=='age_group20M24', Term:= '20-24 vs. 15-19']
coeffs[Term=='age_group25P', Term:= '25+ vs. 15-19']
coeffs[Term=='cdcc_lowvl', Term:= 'Prior CDC C,\nCD4% 200 cells/ul\nor <20%']
coeffs[Term=='TDF_expY', Term:= 'Prior TDF exposure']
coeffs[Term=='TDF_duration', Term:= 'Duration on TDF (years)']
coeffs[Term=='ART_regINSTI_2NRTI', Term:= 'ART reg: INSTI vs. TAF']
coeffs[Term=='ART_regbPI_2NRTI', Term:= 'ART reg: PI vs. TAF']
coeffs[Term=='ART_regNNRTI_2NRTI', Term:= 'ART reg: NNRTI vs. TAF']
coeffs[Term=='fam_hx_bone_diseaseYes', Term:= 'Family history of\nbone disease']
coeffs[Term=='reduced_mobilityYes', Term:= 'Reduced mobilisation']
coeffs[Term=='bmi_cat<18.5', Term:= 'BMI: <18.5 vs. 18.5-25']
coeffs[Term=='bmi_cat25M30', Term:= 'BMI: 25-30 vs. 18.5-25']
coeffs[Term=='bmi_cat30P', Term:= 'BMI: 30+ vs. 18.5-25']
coeffs[Term=='smokeYes', Term:= 'Smoker: Current vs. never/ex']
coeffs[Term=='ever_smokeYes', Term:= 'Smoker: Ever smoked vs. never']
coeffs[Term=='alcoholYes', Term:= 'Drinks alcohol']
coeffs[Term=='abnormal_vitD', Term:= 'Vitamin D <= 50 nmol/L']
coeffs[Term=='high_PTH', Term:= 'PTH >= 7.2 pmol/L']


g <- ggplot(coeffs, aes(x = Estimate, y = Term, col = model)) +
  geom_point(size = 4) +
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI), height = 0.2) +
  scale_color_manual(values=c(pal,pal2)) +
  xlab("Estimated coefficient") +
  ylab("Predictor") +
  theme_minimal() +
  #scale_x_log10() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  theme(legend.position='none') +
  ggtitle("Forest Plot of Predictors from Bayesian Linear Regression Models\nadjusted for baseline LS BMD")
ggsave(file=file.path(indir.data,'results',args$analysis,'change_BMD_LS_forest_plot_adjust_bl_priors_N02.png'),g,h=6,w=5.5,bg="white")


# save stanfits
save(model_adj_age, model_adj_cdcc,
     model_adj_tdf,
     model_adj_tdf_length,
     model_adj_art,
     model_adj_fhb,
     model_adj_mob,
     model_adj_bmi_cat,
     model_adj_smoking,
     model_adj_ever_smoked,
     model_adj_alc,
     model_adj_vitD,
     model_adj_pth, file=file.path(indir.data,'results',args$analysis,'change_BMD_LS_adjust_bl_priors_N02_stanfit.rdata'))

# save summaries
#tab <- list()

names_models <- list(model_adj_age[[3]],
                     model_adj_cdcc[[3]],
                     model_adj_tdf[[3]],
                     model_adj_tdf_length[[3]],
                     model_adj_art[[3]],
                     model_adj_fhb[[3]],
                     model_adj_mob[[3]],
                     model_adj_bmi_cat[[3]],
                     model_adj_smoking[[3]],
                     model_adj_ever_smoked[[3]],
                     model_adj_alc[[3]],
                     model_adj_vitD[[3]],
                     model_adj_pth[[3]])
for(i in seq_along(names_models)){
  names_models[[i]][, model_string:= paste(deparse(model), collapse = "")]
  names_models[[i]][, model:=NULL]
}
tab <- do.call(rbind,names_models)
tab <- unique(tab)
tab[, model:= list_pred]

write.csv(tab,file = file.path(indir.data,'results',args$analysis,'change_BMD_LS_adjust_blmodels_priors_N02_ESS_divergences.csv'))

## save posterior probs OR>1 for each coefficient ----

model_adj_age[[4]]$model <- 'Age'
model_adj_cdcc[[4]]$model <- 'CDCC'
model_adj_tdf[[4]]$model <- 'TDF'
model_adj_tdf_length[[4]]$model <- 'TDF_duration'
model_adj_art[[4]]$model <-  'ART'
model_adj_fhb[[4]]$model <-  'FHB'
model_adj_mob[[4]]$model <-  'Mobility'
model_adj_bmi_cat[[4]]$model <-  'BMI'
model_adj_smoking[[4]]$model <-  'Smoking'
model_adj_ever_smoked[[4]]$model <-  'Ever smoked'
model_adj_alc[[4]]$model <-  'Alcohol'
model_adj_vitD[[4]]$model <-  'VitaminD'
model_adj_pth[[4]]$model <-  'PTH'

coeffs <- rbind(model_adj_age[[4]],
                model_adj_cdcc[[4]],
                model_adj_tdf[[4]],
                model_adj_tdf_length[[4]],
                model_adj_art[[4]],
                model_adj_fhb[[4]],
                model_adj_mob[[4]],
                model_adj_bmi_cat[[4]],
                model_adj_smoking[[4]],
                model_adj_ever_smoked[[4]],
                model_adj_alc[[4]],
                model_adj_vitD[[4]],
                model_adj_pth[[4]])
coeffs <- data.table(coeffs)
coeffs[,Term:= gsub('b_','',variable)]

coeffs <- data.table(subset(coeffs,!grepl('ethnicity', Term)))
coeffs <- data.table(subset(coeffs,!grepl('sex', Term)))
coeffs <- data.table(subset(coeffs,!(grepl('age_group', Term) & model!='Age')))
coeffs <- data.table(subset(coeffs,!grepl('BMD_LS_Z_bl', Term)))

coeffs[Term=='age_group20M24', Term:= '20-24 vs. 15-19']
coeffs[Term=='age_group25P', Term:= '25+ vs. 15-19']
coeffs[Term=='cdcc_lowvl', Term:= 'Prior CDC C,\nCD4% 200 cells/ul\nor <20%']
coeffs[Term=='TDF_expY', Term:= 'Prior TDF exposure']
coeffs[Term=='TDF_duration', Term:= 'Duration on TDF (years)']
coeffs[Term=='ART_regNNRTI', Term:= 'ART reg: NNRTI vs. INSTI']
coeffs[Term=='ART_regPI', Term:= 'ART reg: PI vs. INSTI']
coeffs[Term=='fam_hx_bone_diseaseYes', Term:= 'Family history of\nbone disease']
coeffs[Term=='reduced_mobilityYes', Term:= 'Reduced mobilisation']
coeffs[Term=='bmi_cat<18.5', Term:= 'BMI: <18.5 vs. 18.5-25']
coeffs[Term=='bmi_cat25M30', Term:= 'BMI: 25-30 vs. 18.5-25']
coeffs[Term=='bmi_cat30P', Term:= 'BMI: 30+ vs. 18.5-25']
coeffs[Term=='smokeYes', Term:= 'Smoker: Current vs. never/ex']
coeffs[Term=='ever_smokeYes', Term:= 'Smoker: Ever smoked vs. never']
coeffs[Term=='alcoholYes', Term:= 'Drinks alcohol']
coeffs[Term=='abnormal_vitD', Term:= 'Vitamin D <= 50 nmol/L']
coeffs[Term=='high_PTH', Term:= 'PTH >= 7.2 pmol/L']

write.csv(coeffs,file = file.path(indir.data,'results',args$analysis,'change_BMD_LS_adjust_blmodels_priors_N02_pvalues.csv'))

# switching to TAF vs. non-TDF/TAF----

# empirical plots

dc[, TAF_fu_fct:= factor(TAF_fu,levels=c(0,1),labels=c('Non-TAF/TDF','TAF'))]

ggplot(subset(dc,TDF_fu==0),aes(x=TAF_fu_fct, y=change_BMD_LS, fill=TAF_fu_fct)) +
  #geom_point() +
  geom_boxplot() +
  labs(x='ART reg at follow-up',y='Change in BMD LS z score') +
  scale_fill_npg() +
  theme_bw() +
  theme(legend.position="none")

model_taf <- fit_linear_model(change_BMD_LS ~ TAF_fu_fct , data=subset(dc,TDF_fu==0), prior=T)

save(model_taf,file=file.path(indir.data,'results',args$analysis,'change_BMD_LS_TAF_switch_stanfit.rdata'))

# add non-parametric function

## first plot change in BMD based on time on TAF -> is there evidence this is non-linear?

# compute TAF duration for those who switched as either duration on TAF or time between visits
ggplot(subset(dc,TAF_fu==1),aes(x=TAF_duration_fu,y=change_BMD_LS)) + geom_point() + 
  geom_smooth() +
  labs(x='Duration on TAF',y='Change in LS BMD zscore over follow-up') +
  theme_bw()
ggsave(file=file.path(indir.data,'results',args$analysis,'change_BMD_LS_TAF_duration.png'),h=3,w=4)

tmp <- subset(dc, TDF==0)

### fit HSGP model ----
dt <- readRDS(file=file.path(indir.data,'results',args$analysis,'cleaned_data.RDS'))
dc <- subset(dt,visit=='baseline')
df <- subset(dt,visit=='follow-up', select=c('study_id','date_scan','BMD','BMD_LS_Z','TAF_duration'))
dc <- merge(dc,df,by='study_id')
setnames(dc,c('date_scan.x','date_scan.y','BMD.x','BMD.y','BMD_LS_Z.x','BMD_LS_Z.y','TAF_duration.x','TAF_duration.y'),
         c('date_scan_bl','date_scan_fu','BMD_bl','BMD_fu','BMD_LS_Z_bl','BMD_LS_Z_fu','TAF_duration_bl','TAF_duration_fu'))

dc[, change_BMD_raw:= BMD_fu - BMD_bl]
dc[, change_BMD_LS:= BMD_LS_Z_fu - BMD_LS_Z_bl]

# remove participants with no follow-up scan
dc <- subset(dc,!is.na(BMD_LS_Z_fu))

# add duration on TAF for the ones that are missing
dc[TAF_fu==1 & is.na(TAF_duration_fu), TAF_duration_fu:= round(as.numeric(date_scan_fu - date_scan_bl)/30.44,0)]

### re-do for TAF vs. non-TDF/TAF ----

# find those still on TDF to exclude from comparison
dc[TDF_fu==1 & !is.na(TAF_duration_fu), TDF_fu:= 0]

# fix 1 pt who switched back to TDF
dc[study_id=='BONDY/1519/044', TDF_fu:= 1]
dc[study_id=='BONDY/1519/044', TAF_fu:= 0]

dc[study_id=='BONDY/2024/055', TDF_fu:= 0]

dc[, TAF_fu_fct:= factor(TAF_fu,levels=c(0,1),labels=c('Non-TAF/TDF','TAF'))]

# remove participants still on TDF
dc <- subset(dc,TDF_fu==0)

# fit univariable model
model_taf <- fit_linear_model(change_BMD_LS ~ TAF_fu_fct + BMD_LS_Z_bl, data=subset(dc,TDF_fu==0), prior=T)
save(model_taf,file=file.path(indir.data,'results',args$analysis,'change_BMD_LS_TAFfu_vs_nonTDFTAF_stanfit.rdata'))

## fit HSGP model ----
tmp <- dc[order(TAF_duration_fu),]
tmp[, row_id:= seq(1,nrow(tmp))]

# tuning parameters
M_f <- 30 # basis functions
L_f <- 1.5 # boundary

# find all coordinates
M <- max(tmp$TAF_duration_fu,na.rm=T)
grid = data.table(months = seq.int(0,M,1))
grid[, x_index := 1:nrow(grid)]

tmp <- merge(tmp,grid,by.x=c('TAF_duration_fu'),by.y=c('months'),all.x=T)
tmp[is.na(x_index), x_index:= 0] # set NAs to 0 to not break stan

tmp <- tmp[order(TAF_duration_fu),]
tmp[, row_id:= seq(1,nrow(tmp))]

# rescale inputs
x <- scale(grid$months, center=TRUE, scale=TRUE)
y <- scale(tmp$change_BMD_LS[], center=TRUE, scale=TRUE)

#Standard deviation and mean of the output
std_y <- attr(y,"scaled:scale")
m_y <- attr(y,"scaled:center")

#Standard deviation of the input
std_x <- attr(x,"scaled:scale")

# make stan data
standata <- list(M_f= M_f, 
                 L_f= L_f, 
                 x= x[,1], 
                 gp_input_dim = max(grid$x_index),
                 x_TAF = unique(x[is.finite(x),1]), # non-NA x values 
                 y= y[,1], 
                 coords_TAF = tmp$x_index, # map individuals to unique duration on TAF
                 N= length(y), 
                 N_TAF = length(unique(x[is.finite(x),1])),
                 TAF = tmp$TAF_fu
)
# compile
model <- cmdstanr::cmdstan_model(stan_file = file.path('stan_models',"lm_HSGP_240919.stan"))
options(mc.cores = parallel::detectCores())

fit <- model$sample(data= standata, iter_warmup=500, 
                    iter_sampling=2000, chains=4, thin=4, 
                    init=0.5, adapt_delta=0.9, 
                    save_warmup=TRUE,
                    output_dir = file.path(indir.data,'results',args$analysis))

model_fit <- rstan::read_stan_csv(fit$output_files())
fit$save_object(file = file.path(indir.data,'results',args$analysis,'HSGP_stanfit.RDS'))
#fit <- readRDS(file = file.path(indir.data,'results',args$analysis,'HSGP_stanfit.RDS'))

fit.target.pars <- c('beta0','beta_TAF','sigma','lscale','alpha','beta_f[1]')
po <- fit$draws(inc_warmup = FALSE,
                format = 'draws_df',
                variables = fit.target.pars
)
su <- as.data.table(posterior::summarise_draws(po))
su[,min(ess_bulk)]
su[,max(rhat)]

### traces----
cat("\n ----------- make trace plots: start ----------- \n")
po <- fit$draws(inc_warmup = TRUE,
                variables = fit.target.pars
)
tmp <- su$variable[which.min(su$ess_bulk)]
tmp <- unique(fit.target.pars,tmp)
fit.target.pars <- c('beta0','beta_TAF','sigma','lscale','alpha','beta_f[1]')

bayesplot:::color_scheme_set("mix-blue-pink")
p <- bayesplot:::mcmc_trace(po,
                            pars = fit.target.pars,
                            n_warmup = 5e2,
                            facet_args = list(ncol = 1, labeller = label_parsed)
)
ggsave(file = file.path(indir.data,'results',args$analysis,'-trace_lwstneff.pdf'), p, w = 12, h = 20)
cat("\n ----------- make trace plots: end ----------- \n")

#
### Pairs plots ----
#
cat("\n ----------- make pairs plots: start ----------- \n")
pd <- fit$draws(inc_warmup = FALSE,
                variables = c(fit.target.pars))
bayesplot:::color_scheme_set("mix-blue-pink")
p <- bayesplot:::mcmc_pairs(pd,
                            pars = c(fit.target.pars),
                            diag_fun = "dens",
                            off_diag_fun = "hex"
)
ggsave(p, file =  file.path(indir.data,'results',args$analysis,"-HMC-pairs_transmission_pars.pdf"), w=length(fit.target.pars)*2, h=length(fit.target.pars)*2)
cat("\n ----------- make pairs plots: end ----------- \n")

#posterior_cp <- as.array(fit$draws(inc_warmup = FALSE,
#                                   variables = c(fit.target.pars)))
posterior_draws <- fit$draws()
posterior_draws_df <- as.array(posterior_draws)
#np_cp <- nuts_params(posterior_draws)
sampler_diagnostics <- fit$sampler_diagnostics()
divergent <- as.vector(sampler_diagnostics[, , "divergent__"])
#np_cp <- fit$nuts_params()
np_cp <- nuts_params(fit)[nuts_params(fit)$Parameter=='divergent__',]
bayesplot::mcmc_pairs(posterior_draws_df, pars = c("beta0", "beta_TAF", "sigma", "lscale", "alpha", "beta_f[1]"))
p <- bayesplot::mcmc_pairs(
  posterior_draws_df, 
  pars = c("beta0", "beta_TAF", "sigma", "lscale", "alpha", "beta_f[1]"),
  np = np_cp,
  diag_fun = "dens",  # Density plots on the diagonal
  off_diag_fun = "hex" # Hexbin scatter plots for the off-diagonal
)
ggsave(file= file.path(indir.data,'results',args$analysis,"-HMC-pairs_transmission_pars_divergences.pdf"), p, w=length(fit.target.pars)*2, h=length(fit.target.pars)*2)

# ppc
#draws <- rstan::extract(fit)
#ppc_dens_overlay(y[,1], po)

# plot y_pred

tmp <- dc[order(TAF_duration_fu),]
tmp[, row_id:= seq(1,nrow(tmp))]

po <- fit$draws(inc_warmup = FALSE,
                format = 'draws_df',
                variables = 'y_pred'
)
po <- as.data.table(po)
setnames(po, colnames(po), gsub('^\\.','',colnames(po)))
po <- melt(po, id.vars = c('chain','iteration','draw'))
po[, row_id := as.integer(gsub(paste0('y_pred','\\[([0-9]+)\\]'),'\\1',as.character(variable)))]
#tmp <- subset(tmp, select = c('row_id','TAF_duration_fu','TAF_fu','change_BMD_LS'))
#po <- merge(po, tmp, by = 'row_id')

po <- po[,
         list( q = quantile(value, probs = c(0.5, 0.025, 0.25, 0.75, 0.975) ),
               stat = c('M','CL','IL', 'IU', 'CU')
         ),
         by = c('row_id')
]
po <- dcast.data.table(po, row_id~stat, value.var = 'q')

tmp <- subset(tmp, select = c('row_id','TAF_duration_fu','TAF_fu','change_BMD_LS'))
po <- merge(po, tmp, by = 'row_id')

ggplot(subset(po,TAF_fu==1)) + geom_point(aes(x=TAF_duration_fu,y=change_BMD_LS),col='black') +
  geom_ribbon(aes(x=TAF_duration_fu,ymin=CL,ymax=CU),fill='red',alpha=0.3) +
  geom_line(aes(x=TAF_duration_fu,y=M),col='red') + theme_bw()
ggsave( file =  file.path(indir.data,'results',args$analysis,"change_BMD_LS_GP.png"), w=5,h=5)


ggplot(subset(po)) + 
  geom_errorbar(aes(x=TAF_duration_fu,ymin=CL,ymax=CU),fill='black',alpha=0.3) +
  geom_point(aes(x=TAF_duration_fu,y=change_BMD_LS),col='red') +
  theme_bw()
ggsave( file =  file.path(indir.data,'results',args$analysis,"ppc.png"), w=5,h=5)

# plot function
f <- summary(model_fit, pars = c("f_TAF"), probs = c(0.025, 0.5, 0.975), digits_summary = 4)$summary

tmp <- dc[order(TAF_duration_fu),]
tmp[, row_id:= seq(1,nrow(tmp))]

ind <- tmp$row_id

ggplot() + geom_point(data=subset(tmp,TAF_fu==1),aes(x=TAF_duration_fu,y=change_BMD_LS)) +
  geom_ribbon(aes(x=grid$months,ymin=f[,4]*std_y+m_y,ymax=f[,6]*std_y+m_y),fill='red',alpha=0.3) +
  geom_line(aes(x=grid$months,y=f[,1]*std_y+m_y),col='red') + theme_bw() +
  labs(x='Time of TAF',y='Change in BMD zscore')
ggsave( file =  file.path(indir.data,'results',args$analysis,"change_BMD_LS_vs_time_TAF_rdm_fn.png"), w=5,h=5)

tmp[, y:= y]
ggplot() + geom_point(data=subset(tmp,TAF_fu==1),aes(x=TAF_duration_fu,y=y)) +
  geom_ribbon(aes(x=grid$months,ymin=f[,4]*std_y+m_y,ymax=f[,6]*std_y+m_y),fill='red',alpha=0.3) +
  geom_line(aes(x=grid$months,y=f[,1]*std_y+m_y),col='red') + theme_bw() +
  labs(x='Time of TAF',y='Change in BMD zscore')


# adjust for baseline zscore ----

args$analysis <- file.path(args$analysis,'adjust_bl_zscore')

tmp <- dc[order(TAF_duration_fu),]
tmp[, row_id:= seq(1,nrow(tmp))]

# tuning parameters
M_f <- 30 # basis functions
L_f <- 1.5 # boundary

# find all coordinates
M <- max(tmp$TAF_duration_fu,na.rm=T)
grid = data.table(months = seq.int(0,M,1))
grid[, x_index := 1:nrow(grid)]

tmp <- merge(tmp,grid,by.x=c('TAF_duration_fu'),by.y=c('months'),all.x=T)
tmp[is.na(x_index), x_index:= 0] # set NAs to 0 to not break stan

tmp <- tmp[order(TAF_duration_fu),]
tmp[, row_id:= seq(1,nrow(tmp))]

# rescale inputs
x <- scale(grid$months, center=TRUE, scale=TRUE)
y <- scale(tmp$change_BMD_LS[], center=TRUE, scale=TRUE)

#Standard deviation and mean of the output
std_y <- attr(y,"scaled:scale")
m_y <- attr(y,"scaled:center")

#Standard deviation of the input
std_x <- attr(x,"scaled:scale")

# make stan data
standata <- list(M_f= M_f, 
                 L_f= L_f, 
                 x= x[,1], 
                 gp_input_dim = max(grid$x_index),
                 x_TAF = unique(x[is.finite(x),1]), # non-NA x values 
                 x_bl = tmp$BMD_LS_Z_bl,
                 y= y[,1], 
                 coords_TAF = tmp$x_index, # map individuals to unique duration on TAF
                 N= length(y), 
                 N_TAF = length(unique(x[is.finite(x),1])),
                 TAF = tmp$TAF_fu
)
# compile
model <- cmdstanr::cmdstan_model(stan_file = file.path('stan_models',"lm_HSGP_240925.stan"))
options(mc.cores = parallel::detectCores())

fit <- model$sample(data= standata, iter_warmup=500, 
                    iter_sampling=2000, chains=4, thin=4, 
                    init=0.5, adapt_delta=0.9, 
                    save_warmup=TRUE,
                    output_dir = file.path(indir.data,'results',args$analysis))

model_fit <- rstan::read_stan_csv(fit$output_files())
fit$save_object(file = file.path(indir.data,'results',args$analysis,'HSGP_stanfit.RDS'))
#fit <- readRDS(file = file.path(indir.data,'results',args$analysis,'HSGP_stanfit.RDS'))

fit.target.pars <- c('beta0','beta_TAF','sigma','lscale','alpha','beta_f[1]')
po <- fit$draws(inc_warmup = FALSE,
                format = 'draws_df',
                variables = fit.target.pars
)
su <- as.data.table(posterior::summarise_draws(po))
su[,min(ess_bulk)]
su[,max(rhat)]

### traces----
cat("\n ----------- make trace plots: start ----------- \n")
po <- fit$draws(inc_warmup = TRUE,
                variables = fit.target.pars
)
tmp <- su$variable[which.min(su$ess_bulk)]
tmp <- unique(fit.target.pars,tmp)
fit.target.pars <- c('beta0','beta_TAF','sigma','lscale','alpha','beta_f[1]')

bayesplot:::color_scheme_set("mix-blue-pink")
p <- bayesplot:::mcmc_trace(po,
                            pars = fit.target.pars,
                            n_warmup = 5e2,
                            facet_args = list(ncol = 1, labeller = label_parsed)
)
ggsave(file = file.path(indir.data,'results',args$analysis,'-trace_lwstneff.pdf'), p, w = 12, h = 20)
cat("\n ----------- make trace plots: end ----------- \n")

#
### Pairs plots ----
#
cat("\n ----------- make pairs plots: start ----------- \n")
pd <- fit$draws(inc_warmup = FALSE,
                variables = c(fit.target.pars))
bayesplot:::color_scheme_set("mix-blue-pink")
p <- bayesplot:::mcmc_pairs(pd,
                            pars = c(fit.target.pars),
                            diag_fun = "dens",
                            off_diag_fun = "hex"
)
ggsave(p, file =  file.path(indir.data,'results',args$analysis,"-HMC-pairs_transmission_pars.pdf"), w=length(fit.target.pars)*2, h=length(fit.target.pars)*2)
cat("\n ----------- make pairs plots: end ----------- \n")

#posterior_cp <- as.array(fit$draws(inc_warmup = FALSE,
#                                   variables = c(fit.target.pars)))
posterior_draws <- fit$draws()
posterior_draws_df <- as.array(posterior_draws)
#np_cp <- nuts_params(posterior_draws)
sampler_diagnostics <- fit$sampler_diagnostics()
divergent <- as.vector(sampler_diagnostics[, , "divergent__"])
#np_cp <- fit$nuts_params()
np_cp <- nuts_params(fit)[nuts_params(fit)$Parameter=='divergent__',]
bayesplot::mcmc_pairs(posterior_draws_df, pars = c("beta0", "beta_TAF", "sigma", "lscale", "alpha", "beta_f[1]"))
p <- bayesplot::mcmc_pairs(
  posterior_draws_df, 
  pars = c("beta0", "beta_TAF", "sigma", "lscale", "alpha", "beta_f[1]"),
  np = np_cp,
  diag_fun = "dens",  # Density plots on the diagonal
  off_diag_fun = "hex" # Hexbin scatter plots for the off-diagonal
)
ggsave(file= file.path(indir.data,'results',args$analysis,"-HMC-pairs_transmission_pars_divergences.pdf"), p, w=length(fit.target.pars)*2, h=length(fit.target.pars)*2)

# ppc
#draws <- rstan::extract(fit)
#ppc_dens_overlay(y[,1], po)

# plot y_pred

tmp <- dc[order(TAF_duration_fu),]
tmp[, row_id:= seq(1,nrow(tmp))]

po <- fit$draws(inc_warmup = FALSE,
                format = 'draws_df',
                variables = 'y_pred'
)
po <- as.data.table(po)
setnames(po, colnames(po), gsub('^\\.','',colnames(po)))
po <- melt(po, id.vars = c('chain','iteration','draw'))
po[, row_id := as.integer(gsub(paste0('y_pred','\\[([0-9]+)\\]'),'\\1',as.character(variable)))]
#tmp <- subset(tmp, select = c('row_id','TAF_duration_fu','TAF_fu','change_BMD_LS'))
#po <- merge(po, tmp, by = 'row_id')

po <- po[,
         list( q = quantile(value, probs = c(0.5, 0.025, 0.25, 0.75, 0.975) ),
               stat = c('M','CL','IL', 'IU', 'CU')
         ),
         by = c('row_id')
]
po <- dcast.data.table(po, row_id~stat, value.var = 'q')

tmp <- subset(tmp, select = c('row_id','TAF_duration_fu','TAF_fu','change_BMD_LS'))
po <- merge(po, tmp, by = 'row_id')

ggplot(subset(po,TAF_fu==1)) + geom_point(aes(x=TAF_duration_fu,y=change_BMD_LS),col='black') +
  geom_ribbon(aes(x=TAF_duration_fu,ymin=CL,ymax=CU),fill='red',alpha=0.3) +
  geom_line(aes(x=TAF_duration_fu,y=M),col='red') + theme_bw()
ggsave( file =  file.path(indir.data,'results',args$analysis,"change_BMD_LS_GP.png"), w=5,h=5)


ggplot(subset(po)) + 
  geom_errorbar(aes(x=TAF_duration_fu,ymin=CL,ymax=CU),fill='black',alpha=0.3) +
  geom_point(aes(x=TAF_duration_fu,y=change_BMD_LS),col='red') +
  theme_bw()
ggsave( file =  file.path(indir.data,'results',args$analysis,"ppc.png"), w=5,h=5)

# plot function
f <- summary(model_fit, pars = c("f_TAF"), probs = c(0.025, 0.5, 0.975), digits_summary = 4)$summary

tmp <- dc[order(TAF_duration_fu),]
tmp[, row_id:= seq(1,nrow(tmp))]

ind <- tmp$row_id

ggplot() + geom_point(data=subset(tmp,TAF_fu==1),aes(x=TAF_duration_fu,y=change_BMD_LS)) +
  geom_ribbon(aes(x=grid$months,ymin=f[,4]*std_y+m_y,ymax=f[,6]*std_y+m_y),fill='red',alpha=0.3) +
  geom_line(aes(x=grid$months,y=f[,1]*std_y+m_y),col='red') + theme_bw() +
  labs(x='Time of TAF',y='Change in BMD zscore')
ggsave( file =  file.path(indir.data,'results',args$analysis,"change_BMD_LS_vs_time_TAF_rdm_fn.png"), w=5,h=5)

tmp[, y:= y]
ggplot() + geom_point(data=subset(tmp,TAF_fu==1),aes(x=TAF_duration_fu,y=y)) +
  geom_ribbon(aes(x=grid$months,ymin=f[,4]*std_y+m_y,ymax=f[,6]*std_y+m_y),fill='red',alpha=0.3) +
  geom_line(aes(x=grid$months,y=f[,1]*std_y+m_y),col='red') + theme_bw() +
  labs(x='Time of TAF',y='Change in BMD zscore')

### plot baseline and TAF coefficients ----

po <- fit$draws(inc_warmup = FALSE,
                format = 'draws_df',
                variables = c('beta_TAF','beta_bl')
)
po <- as.data.table(po)
setnames(po, colnames(po), gsub('^\\.','',colnames(po)))
#po <- melt(po, id.vars = c('chain','iteration','draw'))

po <- po[,
         list( beta_TAF = quantile(beta_TAF, probs = c(0.5, 0.025, 0.975) ),
               beta_bl = quantile(beta_bl, probs = c(0.5, 0.025, 0.975) ),
               stat = c('M','CL', 'CU')
         )]
po <- melt(po, id.vars=c('stat'))
coeffs <- dcast.data.table(po, variable~stat, value.var = c('value'))
coeffs[, Term:= factor(variable,levels=c('beta_TAF','beta_bl'),
                           labels=c('TAF vs. non-TDF/TAF','Baseline BMD Zscore'))]

ggplot(coeffs, aes(x = M, y = Term),color=pal[1]) +
  geom_point(size = 4) +
  geom_errorbarh(aes(xmin = CL, xmax = CU), height = 0.2) +
  scale_color_manual(values=c(pal,pal2)) +
  xlab("Estimated coefficient") +
  ylab("Predictor") +
  theme_minimal() +
  #scale_x_log10() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  theme(legend.position='none') #+


## posterior prob TAF vs. non-TAF <0 (non-inferior) ----


