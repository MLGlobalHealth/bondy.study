
clean_value <- function(x) {
  x <- gsub("<10", "9.5", x)
  as.numeric(x)
}

# Define a function to clean and convert the values
clean_duration <- function(x) {
  # Check if the value is missing or non-numeric and return NA for those
  if (is.na(x) || x == "N/A") {
    return(NA_real_)
  }
  
  # Handle cases where years and months are specified
  x <- gsub("(\\d+)\\s*years?,?\\s*(\\d*)\\s*months?.*", "\\1 * 12 + \\2", x)  # Convert years + months
  x <- gsub("(\\d+)\\s*years?", "\\1 * 12", x)  # Convert years to months
  x <- gsub("mos|months", "", x)  # Remove 'months' or 'mos'
  x <- gsub("\\s+", " ", x)  # Remove extra spaces
  x <- trimws(x)  # Trim leading and trailing spaces
  
  # Evaluate the expression and return the numeric value
  result <- tryCatch(eval(parse(text = paste("(", x, ")"))), error = function(e) NA_real_)
  return(as.numeric(result))
}

clean_data <- function(DT) {
  
  DT[, is_numeric_date := suppressWarnings(!is.na(as.numeric(as.character(date_scan))))]
  DT[is_numeric_date == TRUE, date_scan_fix := as.Date(as.numeric(date_scan), origin = "1899-12-30",format='%Y-%m-%d')]
  DT[, date_scan_fix := format(date_scan_fix, format='%d.%m.%y')]
  DT[is_numeric_date == TRUE, date_scan:= date_scan_fix]
  DT[, date_scan:= as.Date(date_scan, format=c('%d.%m.%y'))]
  set(DT,NULL,'date_scan_fix',NULL)
  
  DT[`foreign-born`=='Entered the UK - Please specify entry date', `foreign-born`:= 'Born outside UK']
  
  DT[BMD>1000, BMD:= BMD/1000] # correct data entry errors (commas instead of decimals)
  DT[, vitD:= gsub("<10", "9.5", vitD)]
  DT[vitD %in% c('ND','NR'), vitD:= '0']
  DT[visit=='follow-up', BMI:=NA] # re-compute BMI at follow-up
  DT[is.na(BMI), BMI:= weight/((height/100)^2)]
  DT[is.na(alcohol_units), alcohol_units:= 0]
  
  DT[VL=='(pls specify if other)', VL:=NA]
  DT[VL=='<40', VL:='<200']
  DT[!VL %in% c('<20','<200'), VL:= cut(as.numeric(VL),breaks=c(0,20,200,100000000),include.lowest=T,right=F,
                                        labels=c('<20','<200','>=200'))]
  DT[, VL:= factor(VL,levels=c('<20','<200','>=200'))]
  
  DT[, vitD:= as.numeric(vitD)]
  DT[, NTX:= as.numeric(NTX)]
  DT[, P1NP:= as.numeric(P1NP)]
  
  DT[PTH %in% c('ND','NR'), PTH:= '0']
  DT[, PTH:= as.numeric(PTH)]
  DT[, BMI:= as.numeric(BMI)]
  DT[, CD4:= as.numeric(CD4)]
  DT[, CD4_nadir:= as.numeric(CD4_nadir)] # WARNING some of these are strings! check how to handle
  
  #DT[, BMD_z_LS:= str_extract_all(BMD_z, "\\d+")[1]]
  DT[, BMD_ab:= ifelse(grepl('osteo',BMD_z),1,0)]
  
  # copy ART reg from baseline for those who don't change
  bl_art <- subset(DT,visit=='baseline',select=c('study_id','ART1','ART2','ART3','ART4'))
  setnames(bl_art,c('ART1','ART2','ART3','ART4'),c('ART1_bl','ART2_bl','ART3_bl','ART4_bl'))
  DT <- merge(DT,bl_art,by=c('study_id'))
  DT[, all_art_na := Reduce(`&`, lapply(.SD, is.na)), .SDcols = c("ART1", "ART2", "ART3", "ART4")]
  DT[visit=='follow-up' & all_art_na==1, ART1:= ART1_bl]
  DT[visit=='follow-up' & all_art_na==1, ART2:= ART2_bl]
  DT[visit=='follow-up' & all_art_na==1, ART3:= ART3_bl]
  DT[visit=='follow-up' & all_art_na==1, ART4:= ART4_bl]
  set(DT,NULL,c('ART1_bl','ART2_bl','ART3_bl','ART4_bl','all_art_na'),NULL)
  
  # length of time on trts
  DT[, TDF_duration := as.numeric(gsub("\\s*(yrs|years|yr)\\s*", "", TDF_duration))]
  DT[, TAF_duration := sapply(TAF_duration, clean_duration)]
  # fix duration for one participant missing data
  first_date <- DT[study_id=='BONDY/2024/043' & visit=='baseline','date_scan']$date_scan
  adjusted_first_date <- first_date %m+% months(9)
  time_diff_months <- as.numeric(interval(adjusted_first_date, as.Date('2022-12-21',format=c('%Y-%m-%d'))) / months(1))
  DT[study_id=='BONDY/2024/043', TAF_duration := round(time_diff_months,0)]
  
  DT[, TAF:= as.integer(
    ART2 %in% c('TAF') | ART4 %in% c('TAF') | (visit=='follow-up' & !is.na(TAF_duration) & TAF_duration>0))]
  
  # fix a couple of odd ones checked against records
  DT[study_id=='BONDY/2024/004' & visit=='follow-up', TAF:= 0]
  DT[study_id=='BONDY/2024/022' & visit=='follow-up', TAF:= 0]
  
  # define ART reg
  map_art <- 
    data.table(
      ART = unique(c(DT$ART1,DT$ART2,DT$ART3,DT$ART4))
    )
  map_art[ART %in% c('DRV/r','DRV/c','LPV/r','ATZ/c','ATZ','DRV','LPV/r'), ART_reg:= 'PI']
  map_art[ART %in% c('DTG','RAL','EVG/c','BIC','CAB','DOL'), ART_reg:= 'INSTI']
  map_art[ART %in% c('ABC','TDF','TAF','3TC','FTC'), ART_reg:= 'NRTI']
  map_art[ART %in% c('RIL','NVP','EFV','ETR'), ART_reg:= 'NNRTI']
  map_art[ART %in% c('off art','N/A','None','NR'), ART_reg:= 'None']
  map_art[, ART_reg:= factor(ART_reg)]
  # to check: NR? no response?
  #DT[, NNRTI_2NRTI := ifelse(apply(.SD, 1, function(x) any(x %in% map_art[ART_reg == 'NNRTI']$ART)), 1, 0), 
  #   .SDcols = c("ART1", "ART2", "ART3", "ART4")]
  #DT[, bPI_2NRTI := ifelse(apply(.SD, 1, function(x) any(x %in% map_art[ART_reg == 'PI']$ART)), 1, 0), 
  #   .SDcols = c("ART1", "ART2", "ART3", "ART4")]
  #DT[, INSTI_2NRTI := ifelse(apply(.SD, 1, function(x) any(x %in% map_art[ART_reg == 'INSTI']$ART)), 1, 0), 
  #   .SDcols = c("ART1", "ART2", "ART3", "ART4")]
  
  DT[, NNRTI_2NRTI := as.integer(
    ART1 %in% map_art[ART_reg == 'NNRTI', ART])]
  DT[, bPI_2NRTI := as.integer(
    ART1 %in% map_art[ART_reg == 'PI', ART])]
  DT[, INSTI_2NRTI := as.integer(
    ART1 %in% map_art[ART_reg == 'INSTI', ART])]
  DT[, TDF:= as.integer(
    ART2 %in% c('TDF'))]
  DT[, ART_reg:= ifelse(TDF==1,'TDF',ifelse(TAF==1,'TAF',ifelse(NNRTI_2NRTI==1,'NNRTI_2NRTI',ifelse(bPI_2NRTI==1,'bPI_2NRTI',
                                                                                                    ifelse(INSTI_2NRTI==1,'INSTI_2NRTI',0)))))]
  DT[ART_reg=="0" & ART1 %in% c('off art','None'), ART_reg:= 'None']
  DT[ART_reg=="0" & ART1=='NR', ART_reg:= 'Unknown']
  DT[ART_reg=="0" & is.na(ART1), ART_reg:= 'Unknown']
  DT[, ART_reg:= factor(ART_reg,levels=c('bPI_2NRTI','INSTI_2NRTI','NNRTI_2NRTI','TDF','TAF','None','Unknown'))]
  
  if(0){
    # define pts ever on TDF
    DT[, ever_TDF:= ifelse(study_id %in% DT[TDF==1]$study_id,1,0)]
    
  }

  # reset those on TDF to non-TDF if duration on TAF is non-zero at follow-up
  DT[TAF_duration>0 & visit=='follow-up', TDF:=0]
  DT[TAF_duration>0 & visit=='follow-up' & TAF==1, ART_reg:='TAF']
  
  # manually recode participants with incorrect data in spreadsheet
  DT[study_id=='BONDY/2024/053' , TDF_exp:= 'Y']
  DT[study_id=='BONDY/2024/043' , TDF_exp:= 'Y']
  DT[study_id=='BONDY/2024/043' & visit=='follow-up', TAF:= 1]
  DT[study_id=='BONDY/2024/053' & visit=='follow-up', TAF:= 1]
  DT[study_id=='BONDY/1519/044' & visit=='follow-up', TDF:= 1]
  DT[study_id=='BONDY/1519/044' & visit=='follow-up', TAF:= 0]
  DT[study_id=='BONDY/1519/044' & visit=='follow-up', ART_reg:= 'TDF']
  DT[study_id=='BONDY/2024/055' & visit=='follow-up', TDF:= 0]
  
  # fix duration for participants with NA at FU but stayed on TAF
  ids <- subset(DT,visit=='follow-up' & TAF==1 & is.na(TAF_duration) & !is.na(date_scan),select=c('study_id','date_scan'))
  duration_bl <- subset(DT,visit=='baseline' & study_id %in% ids$study_id,select=c('study_id','date_scan','TAF_duration'))
  setnames(duration_bl,'date_scan','date_scan_bl')
  ids <- merge(ids,duration_bl,by='study_id')
  ids[, diff_fu_months:= as.numeric(interval(date_scan_bl, date_scan) / months(1))]
  ids[, TAF_duration_FU:= round(diff_fu_months + TAF_duration,0)]
  DT <- merge(DT,subset(ids,select=c('study_id','TAF_duration_FU')),all.x=T)
  DT[!is.na(TAF_duration_FU) & visit=='follow-up', TAF_duration:= TAF_duration_FU]
  set(DT,NULL,'TAF_duration_FU',NULL)
  
  # copy TDF_exp to follow-up visit
  tdf <- subset(DT, visit=='baseline', select=c('study_id','TDF_exp'))
  set(DT,NULL,'TDF_exp',NULL)
  DT <- merge(DT,tdf,by='study_id',all.x=T)
  
  taf_bl <- subset(DT,visit=='baseline',select=c('study_id','TDF','TAF'))
  setnames(taf_bl,c('TDF','TAF'),c('TDF_bl','TAF_bl'))
  
  taf_fu <- subset(DT,visit=='follow-up',select=c('study_id','TDF','TAF'))
  setnames(taf_fu,c('TDF','TAF'),c('TDF_fu','TAF_fu'))
  
  DT <- merge(DT,taf_bl,by='study_id',all.x=T)
  DT <- merge(DT,taf_fu,by='study_id',all.x=T)
  
  # define pts who switched to TAF
  #DT[, TAF_switch:= ifelse(TDF_bl==1 & TAF_fu==1, 1, 0)]
  
  # just use ART1 to determine regimen?
  DT[, PI_reg := as.integer(
    ART1 %in% map_art[ART_reg == 'PI', ART] |
      ART2 %in% map_art[ART_reg == 'PI', ART] |
      ART3 %in% map_art[ART_reg == 'PI', ART] |
      ART4 %in% map_art[ART_reg == 'PI', ART] 
  )]
  DT[, INSTI_reg := as.integer(
    ART1 %in% map_art[ART_reg == 'INSTI', ART]|
      ART2 %in% map_art[ART_reg == 'INSTI', ART] |
      ART3 %in% map_art[ART_reg == 'INSTI', ART] |
      ART4 %in% map_art[ART_reg == 'INSTI', ART] 
  )]
  DT[, NRTI_reg := as.integer(
    ART1 %in% map_art[ART_reg == 'NRTI', ART]|
      ART2 %in% map_art[ART_reg == 'NRTI', ART] |
      ART3 %in% map_art[ART_reg == 'NRTI', ART] |
      ART4 %in% map_art[ART_reg == 'NRTI', ART] 
  )]
  DT[, NNRTI_reg := as.integer(
    ART1 %in% map_art[ART_reg == 'NNRTI', ART]|
      ART2 %in% map_art[ART_reg == 'NNRTI', ART] |
      ART3 %in% map_art[ART_reg == 'NNRTI', ART] |
      ART4 %in% map_art[ART_reg == 'NNRTI', ART] 
  )]
  
  # NEED TO CHECK HOW TO DEFINE ART REG for NRTIs
  #DT <- merge(DT,map_art,by.x='ART1',by.y='ART',all.x=T)
  #DT[NRTI_reg==1, ART_reg:= 'NRTI']
  
  # manually add ART data for participants with missing data
  DT[study_id=='BONDY/2024/043' & visit=='baseline', ART_reg:= 'None']
  DT[study_id=='BONDY/2024/043' & visit=='follow-up', ART_reg:= 'TAF']

  
  DT[study_id=='BONDY/2024/053' & visit=='baseline', ART_reg:= 'TDF']
  DT[study_id=='BONDY/2024/053' & visit=='follow-up', ART_reg:= 'TAF']
  
  DT[study_id=='BONDY/1519/044' & visit=='follow-up', ART_reg:= 'TDF'] #switched from TAF to TDF 1 year after baseline
  DT[study_id=='BONDY/2024/055' & visit=='follow-up', ART_reg:= 'INSTI'] # switched to DTG, ABC/3TC after baseline visit
  
  
  return(DT)
}


make_summary_table_age <- function(DT,grouping='by_age'){
  
  if(grouping=='overall'){
    DT[,age_group:='All']
  }
  
  tab <- list()
  # Sex
  tab[['sex']] <- DT[sex=='F', list(var='Female sex at birth, n (%)',Female=.N), by=c('age_group','sex')]
  counts <- DT[, list(tot=.N), by=c('age_group')]
  tab[['sex']] <- merge(tab[['sex']],counts,by='age_group')
  tab[['sex']][, pct:= Female/tot]
  tab[['sex']][, lab:= paste0(Female, ' (', round(pct*100,0),')')]
  set(tab[['sex']],NULL,c('Female','sex','tot','pct'),NULL)
  
  # Ethnicity
  tab[['eth']] <- DT[, list(var='Ethnicity, n (%)',N=.N), by=c('age_group','ethnicity')]
  tab[['eth']][, pct:= N/sum(N)]
  tab[['eth']][, lab:= paste0(N, ' (', round(pct*100,0),')')]
  set(tab[['eth']],NULL,c('N','pct'),NULL)
  setnames(tab[['eth']],'ethnicity','group')

  # Foreign-born
  tab[['fb']] <- DT[, list(var='Birth place, n (%)',N=.N), by=c('age_group','foreign-born')]
  tab[['fb']][, pct:= N/sum(N)]
  tab[['fb']][, lab:= paste0(N, ' (', round(pct*100,0),')')]
  set(tab[['fb']],NULL,c('N','pct'),NULL)
  setnames(tab[['fb']],'foreign-born','group')
  
  # weight, height, BMI, zscore for weight/height (missing)
  tab[['weight']] <- DT[, list(var='Weight (Kg), mean (SD)',mean=mean(weight),sd=sd(weight)), by=c('age_group')]
  tab[['weight']][, lab:= paste0(round(mean,1), ' (',round(sd,1),')')]
  set(tab[['weight']],NULL,c('mean','sd'),NULL)
  
  tab[['height']] <- DT[, list(var='Height (cm), mean (SD)',mean=mean(height),sd=sd(height)), by=c('age_group')]
  tab[['height']][, lab:= paste0(round(mean,1), ' (',round(sd,1),')')]
  set(tab[['height']],NULL,c('mean','sd'),NULL)
  
  tab[['bmi']] <- DT[, list(var='BMI (kg/m2), mean (SD)',mean=mean(BMI,na.rm=T),sd=sd(BMI,na.rm=T)), by=c('age_group')]
  tab[['bmi']][, lab:= paste0(round(mean,1), ' (',round(sd,1),')')]
  set(tab[['bmi']],NULL,c('mean','sd'),NULL)
  
  ## HIV charcateristics
  # CD4
  tab[['CD4']] <- DT[, list(var='Current CD4 (cells/uL), median (IQR)',
                             median=median(CD4,na.rm=T),min=quantile(CD4, probs=0.25,na.rm=T),max=quantile(CD4, probs=0.75,na.rm=T)), by=c('age_group')]
  tab[['CD4']][, lab:= paste0(round(median,1), ' (',round(min,1),'-',round(max,1),')')]
  set(tab[['CD4']],NULL,c('median','min','max'),NULL)
  
  # CD4 nadir
  tab[['CD4_nadir']] <- DT[, list(var='Nadir CD4 (cells/uL), median (IQR)',
  #                                 median=median(CD4_nadir,na.rm=T),min=min(CD4_nadir,na.rm=T),max=max(CD4_nadir,na.rm=T)), by=c('age_group')]
  median=median(CD4_nadir,na.rm=T),min=quantile(CD4_nadir,probs=0.25,na.rm=T),max=quantile(CD4_nadir,probs=0.75,na.rm=T)), by=c('age_group')]
  tab[['CD4_nadir']][, lab:= paste0(round(median,1), ' (',round(min,1),'-',round(max,1),')')]
  set(tab[['CD4_nadir']],NULL,c('median','min','max'),NULL)
  
  # VL
  tab[['vl']] <- DT[, list(var='Plasma viral load (copies/ml), n (%)',N=.N), by=c('age_group','VL')]
  tab[['vl']][, pct:= N/sum(N)]
  tab[['vl']][, lab:= paste0(N, ' (', round(pct*100,0),')')]
  set(tab[['vl']],NULL,c('N','pct'),NULL)
  setnames(tab[['vl']],'VL','group')
  
  # prev CDC C diag
  tab[['cdcc']] <- DT[, list(var='Previous CDC-C diagnosis, n (%)',N=.N), by=c('age_group','CDC_C')]
  counts <- DT[, list(tot=.N), by=c('age_group')]
  tab[['cdcc']] <- merge(tab[['cdcc']],counts,by='age_group')
  tab[['cdcc']][, pct:= N/tot]
  tab[['cdcc']][, lab:= paste0(N, ' (', round(pct*100,0),')')]
  tab[['cdcc']] <- tab[['cdcc']][CDC_C=='Yes',]
  set(tab[['cdcc']],NULL,c('CDC_C','N','tot','pct'),NULL)
  
  ## ART char
  # INSTI reg
  tab[['INSTI']] <- DT[INSTI_reg==1, list(var='INSTI-containing regimen, n (%)',N=.N), by=c('age_group')]
  counts <- DT[, list(tot=.N), by=c('age_group')]
  tab[['INSTI']] <- merge(tab[['INSTI']],counts,by='age_group')
  tab[['INSTI']][, pct:= N/tot]
  tab[['INSTI']][, lab:= paste0(N, ' (', round(pct*100,0),')')]
  set(tab[['INSTI']],NULL,c('N','tot','pct'),NULL)
  
  # PI reg
  tab[['PI']] <- DT[PI_reg==1, list(var='PI-containing regimen, n (%)',N=.N), by=c('age_group')]
  counts <- DT[, list(tot=.N), by=c('age_group')]
  tab[['PI']] <- merge(tab[['PI']],counts,by='age_group')
  tab[['PI']][, pct:= N/tot]
  tab[['PI']][, lab:= paste0(N, ' (', round(pct*100,0),')')]
  set(tab[['PI']],NULL,c('N','tot','pct'),NULL)
  
  # NNRTI reg
  tab[['NNRTI']] <- DT[NNRTI_reg==1, list(var='NNRTI-containing regimen, n (%)',N=.N), by=c('age_group')]
  counts <- DT[, list(tot=.N), by=c('age_group')]
  tab[['NNRTI']] <- merge(tab[['NNRTI']],counts,by='age_group')
  tab[['NNRTI']][, pct:= N/tot]
  tab[['NNRTI']][, lab:= paste0(N, ' (', round(pct*100,0),')')]
  set(tab[['NNRTI']],NULL,c('N','tot','pct'),NULL)
  
  # NRTI reg
  # TDF
  tab[['NRTI_TDF']] <- DT[TDF==1, list(var='NRTI-containing regimen (TDF), n (%)',N=.N), by=c('age_group')]
  counts <- DT[, list(tot=.N), by=c('age_group')]
  tab[['NRTI_TDF']] <- merge(tab[['NRTI_TDF']],counts,by='age_group')
  tab[['NRTI_TDF']][, pct:= N/tot]
  tab[['NRTI_TDF']][, lab:= paste0(N, ' (', round(pct*100,0),')')]
  set(tab[['NRTI_TDF']],NULL,c('N','tot','pct'),NULL)
  
  # TAF
  tab[['NRTI_TAF']] <- DT[TAF==1, list(var='NRTI-containing regimen (TAF), n (%)',N=.N), by=c('age_group')]
  counts <- DT[, list(tot=.N), by=c('age_group')]
  tab[['NRTI_TAF']] <- merge(tab[['NRTI_TAF']],counts,by='age_group')
  tab[['NRTI_TAF']][, pct:= N/tot]
  tab[['NRTI_TAF']][, lab:= paste0(N, ' (', round(pct*100,0),')')]
  set(tab[['NRTI_TAF']],NULL,c('N','tot','pct'),NULL)
  
  # non-TDF/TAF
  tab[['NRTI_oth']] <- DT[NRTI_reg==1 & TAF==0 & TDF==0, list(var='NRTI-containing regimen (non-TDF/non-TAF), n (%)',N=.N), by=c('age_group')]
  counts <- DT[, list(tot=.N), by=c('age_group')]
  tab[['NRTI_oth']] <- merge(tab[['NRTI_oth']],counts,by='age_group')
  tab[['NRTI_oth']][, pct:= N/tot]
  tab[['NRTI_oth']][, lab:= paste0(N, ' (', round(pct*100,0),')')]
  set(tab[['NRTI_oth']],NULL,c('N','tot','pct'),NULL)
  
  # prev TDF exposure
  tab[['prev_TDF']] <- DT[TDF_exp=='Y', list(var='Previous TDF exposure, n (%)',N=.N), by=c('age_group')]
  counts <- DT[, list(tot=.N), by=c('age_group')]
  tab[['prev_TDF']] <- merge(tab[['prev_TDF']],counts,by='age_group')
  tab[['prev_TDF']][, pct:= N/tot]
  tab[['prev_TDF']][, lab:= paste0(N, ' (', round(pct*100,0),')')]
  set(tab[['prev_TDF']],NULL,c('N','tot','pct'),NULL)
  
  ## bone health char
  # vitD
  tab[['vitd']] <- DT[, list(var='Vitamin D, median (IQR)',
#                            median=median(vitD,na.rm=T),min=min(vitD,na.rm=T),max=max(vitD,na.rm=T)), by=c('age_group')]
  median=median(vitD,na.rm=T),min=quantile(vitD,probs=0.25,na.rm=T),max=quantile(vitD,probs=0.75,na.rm=T)), by=c('age_group')]
  tab[['vitd']][, lab:= paste0(round(median,1), ' (',round(min,1),'-',round(max,1),')')]
  set(tab[['vitd']],NULL,c('median','min','max'),NULL)
  
  # PTH
  tab[['PTH']] <- DT[, list(var='PTH, median (IQR)',
#                             median=median(PTH,na.rm=T),min=min(PTH,na.rm=T),max=max(PTH,na.rm=T)), by=c('age_group')]
  median=median(PTH,na.rm=T),min=quantile(PTH,probs=0.25,na.rm=T),max=quantile(PTH,probs=0.75,na.rm=T)), by=c('age_group')]
  tab[['PTH']][, lab:= paste0(round(median,1), ' (',round(min,1),'-',round(max,1),')')]
  set(tab[['PTH']],NULL,c('median','min','max'),NULL)
  
  # calcium
  tab[['calcium']] <- DT[, list(var='Calcium, median (IQR)',
  #                               median=median(calcium,na.rm=T),min=min(calcium,na.rm=T),max=max(calcium,na.rm=T)), by=c('age_group')]
  median=median(calcium,na.rm=T),min=quantile(calcium,probs=0.25,na.rm=T),max=quantile(calcium,probs=0.75,na.rm=T)), by=c('age_group')]
  tab[['calcium']][, lab:= paste0(round(median,1), ' (',round(min,1),'-',round(max,1),')')]
  set(tab[['calcium']],NULL,c('median','min','max'),NULL)
  
  # phosphate
  tab[['Phosphate']] <- DT[, list(var='Phosphate, median (IQR)',
#                                   median=median(phosphate,na.rm=T),min=min(phosphate,na.rm=T),max=max(phosphate,na.rm=T)), by=c('age_group')]
  median=median(phosphate,na.rm=T),min=quantile(phosphate,probs=0.25,na.rm=T),max=quantile(phosphate,probs=0.75,na.rm=T)), by=c('age_group')]
  tab[['Phosphate']][, lab:= paste0(round(median,1), ' (',round(min,1),'-',round(max,1),')')]
  set(tab[['Phosphate']],NULL,c('median','min','max'),NULL)
  
  # alkaline
  tab[['alk']] <- DT[, list(var='Alkaline phosphate, median (IQR)',
  #                           median=median(alkaline,na.rm=T),min=min(alkaline,na.rm=T),max=max(alkaline,na.rm=T)), by=c('age_group')]
  median=median(alkaline,na.rm=T),min=quantile(alkaline,probs=0.25,na.rm=T),max=quantile(alkaline,probs=0.75,na.rm=T)), by=c('age_group')]
  tab[['alk']][, lab:= paste0(round(median,1), ' (',round(min,1),'-',round(max,1),')')]
  set(tab[['alk']],NULL,c('median','min','max'),NULL)
  
  # NTX
  tab[['NTX']] <- DT[, list(var='NTX, median (IQR)',
  #                           median=median(NTX,na.rm=T),min=min(NTX,na.rm=T),max=max(NTX,na.rm=T)), by=c('age_group')]
  median=median(NTX,na.rm=T),min=quantile(NTX,probs=0.25,na.rm=T),max=quantile(NTX,probs=0.75,na.rm=T)), by=c('age_group')]
  tab[['NTX']][, lab:= paste0(round(median,1), ' (',round(min,1),'-',round(max,1),')')]
  set(tab[['NTX']],NULL,c('median','min','max'),NULL)
  
  # P1NP
  tab[['P1NP']] <- DT[, list(var='P1NP, median (IQR)',
  #                            median=median(P1NP,na.rm=T),min=min(P1NP,na.rm=T),max=max(P1NP,na.rm=T)), by=c('age_group')]
  median=median(P1NP,na.rm=T),min=quantile(P1NP,probs=0.25,na.rm=T),max=quantile(P1NP,probs=0.75,na.rm=T)), by=c('age_group')]
  tab[['P1NP']][, lab:= paste0(round(median,1), ' (',round(min,1),'-',round(max,1),')')]
  set(tab[['P1NP']],NULL,c('median','min','max'),NULL)
  
  names <- names(tab)[!names(tab) %in% c('eth','vl','fb')]
  for(i in names){
    tab[[i]][, group:= '']
    setcolorder(tab[[i]], neworder = c('age_group','group'))
  }
  
  tab <- do.call(`rbind`,tab)
  tab[, var:= factor(var, levels=c(unique(tab$var)))]
  tab <- data.table::dcast(tab,var+group~age_group,value.var='lab')
  #tab[is.na(`15-19`),  `15-19`:= paste0('0 (0)')]
  tab <- subset(tab,!is.na(group))
  
  return(tab)
}


make_visit_summary_table <- function(tmp){
  
  tab <- list()
  
  ## abnormal BMD ----
  tab[['bmd']] <- tmp[, list(var='Abnormal BMD, n (%)',N=.N), by=c('abnormal_bmd')]
  tab[['bmd']][, tot:= sum(N)]
  tab[['bmd']][, pct:= N/tot]
  tab[['bmd']][, lab:= paste0(N, ' (', round(pct*100,0),')')]
  set(tab[['bmd']],NULL,c('N','tot','pct'),NULL)
  tab[['bmd']][,group:='']
  #tab[['bmd']][, abn_bmd:= abnormal_bmd]
  setcolorder(tab[['bmd']], neworder = c('group'))
  
  ## age ----

  # age
  tab[['age_gp']] <- tmp[, list(var='Age group',N=.N), by=c('age_group','abnormal_bmd')]
  counts <- tmp[, list(tot=.N), by=c('age_group')]
  tab[['age_gp']] <- merge(tab[['age_gp']],counts,by='age_group')
  tab[['age_gp']][, pct:= N/tot]
  tab[['age_gp']][, lab:= paste0(N, ' (', round(pct*100,0),')')]
  set(tab[['age_gp']],NULL,c('N','tot','pct'),NULL)
  
  # prior CDC c or CD4<200 ----

  tab[['cdcc']] <- tmp[, list(var='Prior CDC C,\nCD4% 200 cells/ul\nor <20%',N=.N), by=c('cdcc_lowvl','abnormal_bmd')]
  counts <- tmp[, list(tot=.N), by=c('cdcc_lowvl')]
  tab[['cdcc']] <- merge(tab[['cdcc']],counts,by='cdcc_lowvl')
  tab[['cdcc']][, pct:= N/tot]
  tab[['cdcc']][, lab:= paste0(N, ' (', round(pct*100,0),')')]
  set(tab[['cdcc']],NULL,c('N','tot','pct'),NULL)
  
  ## TDF exp ----
  tab[['TDF']] <- tmp[, list(var='Any TDF exposure',N=.N), by=c('TDF_exp','abnormal_bmd')]
  counts <- tmp[, list(tot=.N), by=c('TDF_exp')]
  tab[['TDF']] <- merge(tab[['TDF']],counts,by='TDF_exp')
  tab[['TDF']][, pct:= N/tot]
  tab[['TDF']][, lab:= paste0(N, ' (', round(pct*100,0),')')]
  set(tab[['TDF']],NULL,c('N','tot','pct'),NULL)
  
  ## ART reg ----
  tab[['ART']] <- tmp[, list(var='Current ART regimen',N=.N), by=c('ART_reg','abnormal_bmd')]
  counts <- tmp[, list(tot=.N), by=c('ART_reg')]
  tab[['ART']] <- merge(tab[['ART']],counts,by='ART_reg')
  tab[['ART']][, pct:= N/tot]
  tab[['ART']][, lab:= paste0(N, ' (', round(pct*100,0),')')]
  set(tab[['ART']],NULL,c('N','tot','pct'),NULL)
  
  ## fam hx bone dis ----
  tab[['fm_hx']] <- tmp[, list(var='Family history\nof bone disease',N=.N), by=c('fam_hx_bone_disease','abnormal_bmd')]
  counts <- tmp[, list(tot=.N), by=c('fam_hx_bone_disease')]
  tab[['fm_hx']] <- merge(tab[['fm_hx']],counts,by='fam_hx_bone_disease')
  tab[['fm_hx']][, pct:= N/tot]
  tab[['fm_hx']][, lab:= paste0(N, ' (', round(pct*100,0),')')]
  set(tab[['fm_hx']],NULL,c('N','tot','pct'),NULL)
  
  
  ## phys ----
  tab[['phys']] <- tmp[, list(var='Physical activity level',N=.N), by=c('PA','abnormal_bmd')]
  counts <- tmp[, list(tot=.N), by=c('PA')]
  tab[['phys']] <- merge(tab[['phys']],counts,by='PA')
  tab[['phys']][, pct:= N/tot]
  tab[['phys']][, lab:= paste0(N, ' (', round(pct*100,0),')')]
  set(tab[['phys']],NULL,c('N','tot','pct'),NULL)
  
  
  ## mob ----
  tab[['mob']] <- tmp[, list(var='Reduced mobilisation',N=.N), by=c('reduced_mobility','abnormal_bmd')]
  counts <- tmp[, list(tot=.N), by=c('reduced_mobility')]
  tab[['mob']] <- merge(tab[['mob']],counts,by='reduced_mobility')
  tab[['mob']][, pct:= N/tot]
  tab[['mob']][, lab:= paste0(N, ' (', round(pct*100,0),')')]
  set(tab[['mob']],NULL,c('N','tot','pct'),NULL)
  
  
  ## BMI cat ----

  tab[['bmi_cat']] <- tmp[, list(var='BMI category',N=.N), by=c('bmi_cat','abnormal_bmd')]
  counts <- tmp[, list(tot=.N), by=c('bmi_cat')]
  tab[['bmi_cat']] <- merge(tab[['bmi_cat']],counts,by='bmi_cat')
  tab[['bmi_cat']][, pct:= N/tot]
  tab[['bmi_cat']][, lab:= paste0(N, ' (', round(pct*100,0),')')]
  set(tab[['bmi_cat']],NULL,c('N','tot','pct'),NULL)
  
  ## smoking ----

  tab[['smoke']] <- tmp[!is.na(smoke), list(var='Current smoker',N=.N), by=c('smoke','abnormal_bmd')]
  counts <- tmp[, list(tot=.N), by=c('smoke')]
  tab[['smoke']] <- merge(tab[['smoke']],counts,by='smoke')
  tab[['smoke']][, pct:= N/tot]
  tab[['smoke']][, lab:= paste0(N, ' (', round(pct*100,0),')')]
  set(tab[['smoke']],NULL,c('N','tot','pct'),NULL)
  
  tab[['ever_smoke']] <- tmp[!is.na(ever_smoke), list(var='Ever smoked',N=.N), by=c('ever_smoke','abnormal_bmd')]
  counts <- tmp[, list(tot=.N), by=c('ever_smoke')]
  tab[['ever_smoke']] <- merge(tab[['ever_smoke']],counts,by='ever_smoke')
  tab[['ever_smoke']][, pct:= N/tot]
  tab[['ever_smoke']][, lab:= paste0(N, ' (', round(pct*100,0),')')]
  set(tab[['ever_smoke']],NULL,c('N','tot','pct'),NULL)
  
  
  ## drinks ----

  tab[['alcohol']] <- tmp[, list(var='Drinks alcohol',N=.N), by=c('alcohol','abnormal_bmd')]
  counts <- tmp[, list(tot=.N), by=c('alcohol')]
  tab[['alcohol']] <- merge(tab[['alcohol']],counts,by='alcohol')
  tab[['alcohol']][, pct:= N/tot]
  tab[['alcohol']][, lab:= paste0(N, ' (', round(pct*100,0),')')]
  set(tab[['alcohol']],NULL,c('N','tot','pct'),NULL)
  
  ## vitamin D levels ----

  tab[['vitd']] <- tmp[, list(var='Vitamin D (nmol/L)',N=.N), by=c('abnormal_vitD','abnormal_bmd')]
  counts <- tmp[, list(tot=.N), by=c('abnormal_vitD')]
  tab[['vitd']] <- merge(tab[['vitd']],counts,by='abnormal_vitD')
  tab[['vitd']][, pct:= N/tot]
  tab[['vitd']][, lab:= paste0(N, ' (', round(pct*100,0),')')]
  set(tab[['vitd']],NULL,c('N','tot','pct'),NULL)
  
  ## PTH levels ----

  tab[['pth']] <- tmp[, list(var='PTH (pmol/L)',N=.N), by=c('high_PTH','abnormal_bmd')]
  counts <- tmp[, list(tot=.N), by=c('high_PTH')]
  tab[['pth']] <- merge(tab[['pth']],counts,by='high_PTH')
  tab[['pth']][, pct:= N/tot]
  tab[['pth']][, lab:= paste0(N, ' (', round(pct*100,0),')')]
  set(tab[['pth']],NULL,c('N','tot','pct'),NULL)
  
  if(unique(tmp$visit=='follow-up')){
    tab[['TAF_duration']] <- tmp[, list(group = '',
                                        var='Duration on TAF (median IQR)',
                                        median=median(TAF_duration,na.rm=T),
                                        QL=quantile(TAF_duration,probs=0.25,na.rm=T),
                                        QU=quantile(TAF_duration,probs=0.75,na.rm=T)), by=c('abnormal_bmd')]
    tab[['TAF_duration']][, lab:= paste0(median,' (',QL,'-',QU,')')]
    set(tab[['TAF_duration']],NULL,c('median','QL','QU'),NULL)
    tab[['TAF_duration']] <- tab[['TAF_duration']][, c('group','abnormal_bmd','var','lab')]
  }
  
  names <- names(tab)#[!names(tab) %in% c('eth','vl')]
  for(i in names){
    #tab[[i]][, group:= '']
    #setcolorder(tab[[i]], neworder = c('age_group','group'))
    setnames(tab[[i]],1,'group')
  }
  
  tab <- do.call(`rbind`,tab)
  tab[, var:= factor(var, levels=c(unique(tab$var)))]
  tab <- dcast(tab,var+group~abnormal_bmd,value.var='lab')
  #tab[is.na(`15-19`),  `15-19`:= paste0('0 (0)')]
  tab <- subset(tab,!is.na(group) )
  
  return(tab)
  
}

# Function to fit univariable Bayesian models
fit_univariable_model <- function(formula, data, prior=FALSE, pthresh=1) {
  #data <- data %>% filter(complete.cases(.))
  if(prior==F){
    model <- brm(formula, data = data, family = bernoulli(), 
                 chains = 4, iter = 2000, warmup = 1000, 
                 seed = 12,silent = TRUE)
  }else{
    model <- brm(formula, data = data, family = bernoulli(), 
                 prior = generic_priors,
                 sample_prior = TRUE,
                 chains = 4, iter = 2000, warmup = 1000, 
                 seed = 12,silent = TRUE)
  }
  summary(model)$fixed
  summary_model <- summary(model)$fixed
  # Transform log-odds to odds
  summary_model <- data.frame(
    Term = rownames(summary_model),
    Estimate = exp(summary_model[, "Estimate"]),
    LowerCI = exp(summary_model[, "l-95% CI"]),
    UpperCI = exp(summary_model[, "u-95% CI"])
  )
  summary <- summary(model)$fixed
  
  sum <- summary(model)
  ss <- data.table(
       model = formula,
       min.bulk.ess = min(sum$fixed$Bulk_ESS),
       num.divergent = rstan::get_num_divergent(model$fit))
  
  pars <- rownames(summary(model)$fixed)
  pars <- pars[pars!='Intercept']
  pars <- paste0('b_',pars)
  ps <- as_draws_df(model)
  ps <- subset(ps,colnames(ps)[colnames(ps) %in% pars])
  ps <- data.table(reshape2::melt(ps,id.vars=NULL,variable.names='coeff',value.name='beta'))
  ps[, beta:= exp(beta)]

  po <- ps[, list(pval = (length(beta[beta>pthresh])/.N)),by='variable']
  po <- subset(po,variable %in% pars)
  
  return(list(summary,summary_model,ss,po))
  
}


# Function to fit univariable Bayesian models
fit_linear_model <- function(formula, data, prior=FALSE, pthresh=0) {
  #data <- data %>% filter(complete.cases(.))
  if(prior==F){
    model <- brm(formula, data = data, family = gaussian(), 
                 chains = 4, iter = 2000, warmup = 1000, 
                 seed = 12,silent = TRUE)
  }else{
    model <- brm(formula, data = data, family = gaussian(), 
                 prior = generic_priors,
                 sample_prior = TRUE,
                 chains = 4, iter = 2000, warmup = 1000, 
                 seed = 12,silent = TRUE)
  }
  summary(model)$fixed
  summary_model <- summary(model)$fixed
  # Transform log-odds to odds
  summary_model <- data.frame(
    Term = rownames(summary_model),
    Estimate = (summary_model[, "Estimate"]),
    LowerCI = (summary_model[, "l-95% CI"]),
    UpperCI = (summary_model[, "u-95% CI"])
  )
  summary <- summary(model)$fixed
  
  sum <- summary(model)
  ss <- data.table(
    model = formula,
    min.bulk.ess = min(sum$fixed$Bulk_ESS),
    num.divergent = rstan::get_num_divergent(model$fit))
  
  pars <- rownames(summary(model)$fixed)
  pars <- pars[pars!='Intercept']
  pars <- paste0('b_',pars)
  ps <- as_draws_df(model)
  ps <- subset(ps,colnames(ps)[colnames(ps) %in% pars])
  ps <- data.table(reshape2::melt(ps,id.vars=NULL,variable.names='coeff',value.name='beta'))

  po <- ps[, list(pval = (length(beta[beta<pthresh])/.N)),by='variable']
  po <- subset(po,variable %in% pars)
  
  return(list(summary,summary_model,ss,po))
  
}