t1 = Sys.time()
# set working directory 
setwd("/home/abhijeet")
# loading all packages(plyr must be loaded before dplyr)
library(plyr)
library(dplyr)
library(zoo)
library(lubridate)
library(forecast)
library(caret)
library(data.table)
library(usdm)
library(catboost)
library(foreach)
library(prophet)
library(doParallel)
library(readxl)
library(tseries)
library(tidyr)
library(xlsx)
library(skimr)
######################################custom function#####################################
# combine function used in parallel to combine all files
combine = function(x, ...) {  
  mapply(rbind, x, ..., SIMPLIFY = FALSE)
}
# create a custom function
`%notin%` <- Negate(`%in%`)
##################################data wrangling##########################################
# read sales data
foredata = read_excel("C:/Users/s983304/Downloads/Documents/Exercise/Jet/Usecase2_Dataset.xlsx",skip=3,col_names = FALSE,1)
colnames(foredata) = as.character(seq(as.Date("2016/1/1"), as.Date("2020/2/29"), "weeks")) %>% prepend("Part.No")
foredata  = melt(foredata , id = c("Part.No") ,
                 variable.name = "Date", value.name = "Qty")
foredata= foredata[with(foredata, order(foredata$Part.No,foredata$Date)),]
foredata$Date = as.Date(foredata$Date)
########################################eda & filtering####################################
# descriptive stats on data
foredata %>% skim()
# replaced na with zero 
foredata[is.na(foredata)] = 0
# combining products with multiple rows 
foredata = foredata %>% group_by(Part.No,Date) %>% dplyr:::summarise(Qty = sum(Qty))
# descriptive stats and distribution
foredata %>% skim()

# add year and month and week
foredata$Year = year(foredata$Date)
foredata$Month = month(foredata$Date)
foredata$Week = week(foredata$Date)

# discontinued parts
disparts = foredata %>% group_by(Part.No,Year) %>% dplyr:::summarise(Qty = sum(Qty))
disparts = spread(disparts, key = "Year", value = "Qty", fill = 0)
disparts = disparts[disparts$`2019` ==0,]
foredata = foredata %>% filter(Part.No %notin% disparts$Part.No)

# check for data points and remove parts which have sparse data
dpoints = foredata %>% group_by(Part.No,Year) %>% 
  dplyr:::summarise(Qty = sum(Qty > 0, na.rm = TRUE))
dpoints = spread(dpoints, key = "Year", value = "Qty", fill = 0)
dpoints = dpoints[dpoints$`2018`>10 | dpoints$`2017`>10 | dpoints$`2016`>10,]
foredata = foredata %>%
  filter(Part.No %in% dpoints$Part.No)

# parts with good demand
toparts = foredata %>% group_by(Part.No) %>% 
  dplyr:::summarise(Qty = sum(Qty)) %>%
  arrange(desc(Qty))

datafil = foredata %>% filter(Part.No %in% c("29032913")) %>% filter(Date<="2019-12-06")

datafil %>% ggplot(aes(x = Date, y = Qty)) + geom_line()

datafil = ts(data = datafil$Qty, 
              start = c(year(first(datafil$Date)), month(first(datafil$Date))), 
              end = c(year(last(datafil$Date)), month(last(datafil$Date))), 
              frequency = 52)

datafil %>% skim()
datafil_stl = stl(x = datafil, s.window = "periodic")
plot(datafil_stl)
adf.test(datafil, alternative = "stationary")
##################################global variable######################################
# no of cores not to use
leavecores = 12
# filter data for crossvalidation
cvdate = "2019-12-06"
trainingdate = "2018-12-28"
# min no of datapoints
datapoints = 52
freqcv = 52.17
freqcvs = 'week'
freqf = 52.17
freqfs = 'week'
# horizon for forecast 
hf = 12
# set frequency for fourier terms 
Kcv=(freqcv/2)-1
##########################################################################################
# getting cores for parallel execution
cores=detectCores()
cl <- makeCluster(cores[1]-leavecores,outfile="debug_log.txt",type="FORK")
registerDoParallel(cl)
##############################set hierarchy for forecasting##############################
names(foredata)[names(foredata) == "Part.No"] = "AggregationHierarchy"
# filter out data for training and testing
maindf = foredata %>% filter(Date<=cvdate)
# add identifier for each part in for parallel run
maindf$Identifier = cumsum(!duplicated(maindf$AggregationHierarchy))
##############################CROSSVALIDATION############################################
output = foreach(i = 1:length(unique(maindf$AggregationHierarchy)),.combine =combine, .packages=c("dplyr", "zoo","lubridate","forecast","catboost","doParallel","prophet"),.multicombine=TRUE) %dopar% {
  # start cross validation in sequential mode
  # for(i in unique(maindf$Identifier)){  
  # filter data for a part
  df = subset(maindf,Identifier==i)
  df$Identifier = NULL
  # remove part column with more than 20% zeros
  df = df[,colSums(df != 0) >= (nrow(df)/5)]
  a = unique(df$AggregationHierarchy)
  # store sales to be forecasted in a list
  colname = colnames(df[ ,-which((names(df) %in% c("Date","AggregationHierarchy","Year","Month","Week"))==TRUE)])
  # create empty data frame to store results for crossvalidation and important features
  cv_all = data.frame()
  coeff_all = data.frame()
  for(j in colname){
    # dataframe with date and qty
    dfcv = df[,c("Date",j)]
    # go on next if a expense has less than 52 data points
    if (nrow(dfcv)<datapoints) next
    # list of variables with time
    timevars = c("Date","Year","Month","Week")
    # filter dataframe with regressors and time variables
    dfxreg = df[ ,which((names(df) %in% c(timevars))==TRUE)]
    dfxreg = dfxreg %>% dplyr::select(Date, everything())
    dfcvbox = dfcv
    ts_cv = ts(dfcvbox, frequency=freqcv, start=c(2016,1))
    # no of data points
    k = nrow(dfcvbox[dfcvbox$Date <= as.Date(trainingdate),])
    n = length(ts_cv)/2
    seq = seq(as.Date(trainingdate)+7, by = "weeks", length.out = 49)
    for(p in 1:(n-k)){
      print(paste0(a,"-",j,"-",p))
      # filter training data
      xshort = dfcvbox %>% filter(Date < as.Date(seq[p]))
      # filter testing data
      xnext = dfcvbox %>% filter(!Date %in% xshort$Date)
      daten = xnext[1]
      dates = xshort[1]
      xshort = subset(xshort,select=-Date)
      xnext = subset(xnext,select=-Date)
      # filter training regressors
      xshort_xreg = dfxreg %>% filter(Date < as.Date(seq[p]))
      # filter testing regressors
      xnext_xreg = subset(dfxreg %>% filter(!Date %in% xshort_xreg$Date),select=-Date)
      xshort_xreg = subset(xshort_xreg,select=-Date)
      # create empty data frame to store record for forecast and features
      forpred = data.frame()
      dfcoeff = data.frame()
      # Univariate(Tbats)
      tryCatch({
        fit = tbats(ts(xshort, frequency = freqcv),use.box.cox = NULL)
        fcast = forecast(fit, h=nrow(xnext))
        forpred = cbind(daten,fcast)
        rownames(forpred)= 1:nrow(forpred)
        forpred$Model = "Exponential Smoothing TBATS"
        print(paste0(a,"-",j,"-",p,"-","tbats"))
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      # Univariate(ets)
      tryCatch({
        fit = ets(ts(xshort, frequency = freqcv))
        fcast = forecast(fit, h=nrow(xnext))
        forets = cbind(daten,fcast)
        rownames(forets)= 1:nrow(forets)
        forets$Model = "Exponential Smoothing ETS"
        forpred = rbind(forpred,forets)
        print(paste0(a,"-",j,"-",p,"-","ets"))
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      # Univariate(stlm)
      tryCatch({
        fit = stlm(ts(xshort, frequency = freqcv),s.window = 72, allow.multiplicative.trend = TRUE)
        fcast = forecast(fit, h=nrow(xnext))
        forstlm = cbind(daten,fcast)
        rownames(forstlm)= 1:nrow(forstlm)
        forstlm$Model = "STLM long seasonality"
        forpred = rbind(forpred,forstlm)
        print(paste0(a,"-",j,"-",p,"-","stlm"))
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      # Univariate(snaive)
      tryCatch({
        fit = snaive(ts(xshort, frequency = freqcv))
        fcast = forecast(fit, h=nrow(xnext))
        fornaive = cbind(daten,fcast)
        rownames(fornaive)= 1:nrow(fornaive)
        fornaive$Model = "SNAIVE seasonal arima"
        forpred = rbind(forpred,fornaive)
        print(paste0(a,"-",j,"-",p,"-","snaive"))
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      # Univariate(stlm dec)
      tryCatch({
        fit = stlm(ts(xshort, frequency = freqcv),s.window = 25,robust = TRUE,allow.multiplicative.trend = TRUE)
        fcast = forecast(fit, h=nrow(xnext))
        forstlmdec = cbind(daten,fcast)
        rownames(forstlmdec)= 1:nrow(forstlmdec)
        forstlmdec$Model = "STLM decomposition"
        forpred = rbind(forpred,forstlmdec)
        print(paste0(a,"-",j,"-",p,"-","stlmdec"))
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      # Univariate(prophet)
      tryCatch({
        dfp = data.table::setnames(cbind(dates$Date,xshort),c("ds","y"))
        #lambda <- BoxCox.lambda(dfp$y)
        #dfp$y<-BoxCox(dfp$y,lambda)
        fit = suppressMessages(prophet(changepoint.range=0.7,changepoint.prior.scale=10,seasonality.prior.scale =10 ))
        fit = add_seasonality(fit, name='weekly', period=7, fourier.order=25)
        fit = fit.prophet(fit, dfp)
        future = make_future_dataframe(fit, periods =nrow(xnext), freq =freqcvs,include_history = FALSE)
        fcast = prophet:::predict.prophet(fit, future)
        fcast = fcast %>% dplyr::select(starts_with("yhat"))
        #fcast = cbind(InvBoxCox(fcast$yhat_lower,lambda),InvBoxCox(fcast$yhat_upper,lambda),InvBoxCox(fcast$yhat,lambda))
        fcast = data.table::setnames(as.data.frame(fcast),c("Lo 95","Hi 95","Point Forecast"))
        forprop = cbind(daten,fcast)
        rownames(forprop)= 1:nrow(forprop)
        forprop$Model = "Prophet"
        forpred = dplyr::bind_rows(forpred,forprop)
        print(paste0(a,"-",j,"-",p,"-","prophet"))
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      # Multivariate(Catboost)
      tryCatch({
        xnext_xreg = as.data.frame(lapply(xnext_xreg, function(x) as.numeric(as.character(x))))
        xshort_xreg= as.data.frame(lapply(xshort_xreg, function(x) as.numeric(as.character(x))))
        train_pool = catboost.load_pool(data = xshort_xreg, label = unlist(xshort[c(j)]))
        test_pool = catboost.load_pool(data = xnext_xreg,label = unlist(xnext[c(j)]))
        
        params = list(iterations=1000,
                      learning_rate=0.01,
                      depth=15,
                      loss_function='RMSE',
                      eval_metric='RMSE',
                      random_seed = 42,
                      od_type='Iter',
                      metric_period = 50,
                      od_wait=20,
                      use_best_model=TRUE)
        
        model = catboost.train(learn_pool = train_pool,test_pool=test_pool,params = params)
        forcat = cbind(daten,catboost.predict(model,test_pool))
        names(forcat)[names(forcat) == "catboost.predict(model, test_pool)"] = "Point Forecast"
        forcat$Model = "Cat Boost"
        forpred = dplyr::bind_rows(forpred,forcat)
        dfcoeff = data.frame(catboost.get_feature_importance(model,pool=train_pool))
        dfcoeff$Variables = row.names(dfcoeff)
        rownames(dfcoeff)= 1:nrow(dfcoeff)
        dfcoeff$Model = "Cat Boost"
        colnames(dfcoeff)[1] = "Coeff"
        print(paste0(a,"-",j,"-",p,"-","catboost"))
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      # Univariate(AutoArima)
      #lambda = BoxCox.lambda(ts(xshort,frequency = freqcv))
      #xshort_box = BoxCox(ts(xshort,frequency = freqcv), lambda = lambda)
      tryCatch({
        fit = auto.arima(ts(xshort, frequency = freqcv),stepwise=FALSE,approximation = FALSE,seasonal=TRUE)
        fcast = forecast(fit, h=nrow(xnext))
        forarimau = cbind(daten,fcast)
        rownames(forarimau)= 1:nrow(forarimau)
        forarimau$Model = "Auto Arima"
        forpred = rbind(forpred,forarimau)
        print(paste0(a,"-",j,"-",p,"-","arima"))
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      # Mulvariate(AutoArima)
      tryCatch({
        xshort_xreg_arima =  usdm::exclude(xshort_xreg,usdm::vifstep(xshort_xreg,th=4))
        xnext_xreg_arima = xnext_xreg[,colnames(xshort_xreg_arima)]
        fit = auto.arima(ts(xshort, frequency = freqcv),xreg=as.matrix(xshort_xreg_arima),stepwise=FALSE,approximation = FALSE,seasonal=TRUE)
        fcast = forecast(fit, h=nrow(xnext),xreg=as.matrix(xnext_xreg_arima))
        forarima = cbind(daten,fcast)
        rownames(forarima)= 1:nrow(forarima)
        forarima$Model = "Auto Arima Regressor"
        forpred = rbind(forpred,forarima)
        dfcoeffar = data.frame(fit$coef)
        dfcoeffar$Variables = row.names(dfcoeffar)
        rownames(dfcoeffar)= 1:nrow(dfcoeffar)
        dfcoeffar$Model = "Auto Arima Regressor"
        colnames(dfcoeffar)[1] = "Coeff"
        dfcoeff = rbind(dfcoeff,dfcoeffar)
        print(paste0(a,"-",j,"-",p,"-","arimareg"))
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      # Univariate(Fourier)
      tryCatch({
        x = 1:length(ts(xshort, frequency = freqcv))
        fit_g <- try(McSpatial::fourier(ts(xshort, frequency = freqcv) ~ x,minq=1,maxq=Kcv,crit="gcv"),silent = T)
        if(!'try-error' %in% class(fit_g)){
          const_val = fit_g$q
        }else{
          const_val = Kcv
        }
        fit = auto.arima(ts(xshort, frequency = freqcv),xreg=forecast::fourier(ts(xshort, frequency = freqcv),K=const_val),stepwise=FALSE,approximation = FALSE,seasonal=FALSE)
        fcast = forecast(fit, h=nrow(xnext),xreg=forecast::fourier(ts(xshort, frequency = freqcv), K=const_val, h=nrow(xnext)))
        forfour = cbind(daten,fcast)
        rownames(forfour)= 1:nrow(forfour)
        forfour$Model = "Auto Arima Fourier"
        forpred = rbind(forpred,forfour)
        print(paste0(a,"-",j,"-",p,"-","arimafourier"))
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      # create data frame for visualisation
      forpred$id = p
      forpred = merge(forpred,dfcv,by="Date")
      names(forpred)[names(forpred) == j] = "Observed"
      forpred$Location = a
      forpred$ExpenseType = j
      forpred$Year = year(forpred$Date)
      forpred$Month = month(forpred$Date)
      dfcoeff$id = p
      dfcoeff$Location = a
      dfcoeff$ExpenseType = j
      cv_all = dplyr::bind_rows(cv_all,forpred)
      coeff_all = dplyr::bind_rows(coeff_all,dfcoeff)
    }
  }
  list(cv_all,coeff_all)
}
# stop all processes
parallel::stopCluster(cl)
cv_all = output[[1]]
coeff_all = output[[2]] 
#####################################BEST MODEL##########################################
cv_temp = cv_all

# tag incomplete cross validation models combination
wmape = cv_all %>% group_by(Location,ExpenseType,Model,id)%>%
  dplyr:::summarise(Count =  n_distinct(id))
wmape = wmape %>% group_by(Location,ExpenseType,Model)%>%
  dplyr:::summarise(Countid =  sum(Count))

# remove incomplete cross validation models
cv_all = merge(cv_all,wmape)
cv_all = cv_all %>% filter(Countid==freqcv)
cv_all$Countid = NULL

# calculate wmape and stddev for each model run
wmape = cv_all %>% group_by(Location,ExpenseType,Model,id)%>% 
  dplyr:::summarise(Wmape = abs(sum(abs(Observed-`Point Forecast`)/Observed*100*Observed,na.rm=TRUE)/sum(Observed)),
                    Sd = sd(`Point Forecast`)/sd(Observed))
cv_all = merge(cv_all,wmape)

# calculate average wmape for each model
wmape = cv_all %>% group_by(Location,ExpenseType,Model)%>% 
  dplyr:::summarise(Avgwmape = mean(Wmape))
cv_all = merge(cv_all,wmape)
cv_all = cv_all[is.finite(cv_all$Avgwmape),]

# combine all models for creating ensemble model
ensemble = cv_all %>% filter(Model != "Auto Arima Fourier") %>%
  group_by(Date,Location,ExpenseType,id,Year,Month) %>% 
  dplyr:::summarise(PointForecast = mean(`Point Forecast`),Hi95 = max(`Point Forecast`),Lo95 = min(`Point Forecast`),Observed = mean(Observed),Wmape = mean(Wmape),Sd=mean(Sd,na.rm=TRUE)) %>%  
  dplyr:::mutate(PointForecast  = PointForecast,Hi95 = Hi95,Lo95 = Lo95,Model="Ensemble Model",Wmape=Wmape,Sd=Sd)
names(ensemble)[names(ensemble) %in% c("PointForecast","Hi95","Lo95")] = c("Point Forecast","Hi 95","Lo 95")
cv_all = dplyr::bind_rows(cv_all,ensemble)

# calculate average wmape and stddev for ensemble model
wmape = cv_all %>% group_by(Location,ExpenseType,Model)%>% 
  dplyr:::summarise(Avgwmape = mean(Wmape),Avgsd =mean(Sd,na.rm=TRUE),Avgwmapsd=mean(Wmape)/mean(Sd,na.rm=TRUE))
cv_all$Avgwmape = NULL
cv_all = merge(cv_all,wmape,by = c("Location","ExpenseType","Model"),all.x = TRUE)

# get the best model(lowest avgmapestddev)
wmape = cv_all %>% group_by(Location,ExpenseType)%>% 
  dplyr:::summarise(Avgwmapsdbm = min(Avgwmapsd,na.rm=TRUE))
cv_all = merge(cv_all,wmape)

# save the best model results
cv_all$Bm = ifelse(cv_all$Avgwmapsd==cv_all$Avgwmapsdbm,cv_all$Model,"")
wmape = cv_all %>% filter(Bm != "")
wmape = wmape[c("Location", "ExpenseType", "Bm")]
wmape = wmape %>% distinct
cv_all$Bm = NULL
cv_all = merge(cv_all,wmape,by=c("Location", "ExpenseType"))

# tag combination with more than one best model
wmape = cv_all %>% group_by(Location,ExpenseType)%>%
  dplyr:::summarise(Count =  n_distinct(Bm))

# remove combination with more than one best model
cv_all = merge(cv_all,wmape)
cv_all = cv_all %>% filter(Count==1)
cv_all$Count = NULL

wmape = cv_all[c("Location", "ExpenseType", "Bm")]
wmape = wmape %>% distinct
##############################################FORECAST######################################################
maindf = foredata
maindf$Identifier = cumsum(!duplicated(maindf$AggregationHierarchy))

# create empty data frame to store forecast
for_all = data.frame()

for(i in unique(maindf$Identifier)){  
  # filter data for a part
  df = subset(maindf,Identifier==i)
  df$Identifier = NULL
  # remove part column with more than 20% zeros
  df = df[,colSums(df != 0) >= (nrow(df)/5)]
  a = unique(df$AggregationHierarchy)
  # store sales to be forecasted in a list
  colname = colnames(df[ ,-which((names(df) %in% c("Date","AggregationHierarchy","Year","Month","Week"))==TRUE)])
  for(j in colname){
    # dataframe with date and qty
    dfcv = df[,c("Date",j)]
    # go on next if a expense has less than 52 data points
    if (nrow(dfcv)<datapoints) next
    # list of variables with time
    timevars = c("Date","Year","Month","Week")
    # list of regressors
    regre = c("Month_endrate","Cpi_contoinf","Cpi_idx","Cpi_idxsa","Cpi_natidx","Cpi_perpp","Cpi_perpy","Sales_to_3rdParty_Cpsevg","Sales_to_3rdParty_Flw")
    # filter dataframe with regressors and time variables
    dfxreg = df[ ,which((names(df) %in% c(timevars))==TRUE)]
    dfxreg = dfxreg %>% dplyr::select(Date, everything())
    dfcvbox = dfcv
    # filter past data
    xshort = dfcvbox[dfcvbox$Date <= as.Date(cvdate),]
    dates = xshort[1]
    xshort = subset(xshort,select=-Date)
    # filter past regressors
    xshort_xreg = dfxreg[dfxreg$Date <= as.Date(cvdate), ]
    # filter future regressors
    xnext_xreg = dfxreg[dfxreg$Date > as.Date(cvdate), ]
    daten = xnext_xreg[1]
    xshort_xreg = subset(xshort_xreg,select=-Date)
    xnext_xreg = subset(xnext_xreg,select=-Date)
    # filter best model from cross validation output
    bm = wmape[wmape$Location==a & wmape$ExpenseType==j,]
    bm = bm$Bm
    # if bm is null then replace it with "No Model"
    if(identical(bm, character(0))){
      bm = "No Model"
    }
    # create empty data frame to store record for forecast
    forpred = data.frame()
    #Univariate(Tbats)
    if(bm=="Exponential Smoothing TBATS"){
      tryCatch({
        fit = tbats(ts(xshort, frequency = freqf),use.box.cox = NULL)
        fcast = forecast(fit, h=hf)
        forpred = cbind(daten,fcast)
        rownames(forpred)= 1:nrow(forpred)
        forpred$Model = "Exponential Smoothing TBATS"
        print(paste0(a,"-",j,"-","tbats"))
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
    #Univariate(ets)
    if(bm=="Exponential Smoothing ETS"){
      tryCatch({
        fit = ets(ts(xshort, frequency = freqf))
        fcast = forecast(fit, h=hf)
        forpred = cbind(daten,fcast)
        rownames(forpred)= 1:nrow(forpred)
        forpred$Model = "Exponential Smoothing ETS"
        print(paste0(a,"-",j,"-","ets"))
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
    if(bm=="STLM long seasonality"){
      #Univariate(stlm)
      tryCatch({
        fit = stlm(ts(xshort, frequency = freqf),s.window = 72, allow.multiplicative.trend = TRUE)
        fcast = forecast(fit, h=hf)
        forpred = cbind(daten,fcast)
        rownames(forpred)= 1:nrow(forpred)
        forpred$Model = "STLM long seasonality"
        print(paste0(a,"-",j,"-","stlm"))
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
    #Univariate(snaive)
    if(bm=="SNAIVE seasonal arima"){
      tryCatch({
        fit = snaive(ts(xshort, frequency = freqf))
        fcast = forecast(fit, h=hf)
        forpred = cbind(daten,fcast)
        rownames(forpred)= 1:nrow(forpred)
        forpred$Model = "SNAIVE seasonal arima"
        print(paste0(a,"-",j,"-","snaive"))
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
    #Univariate(stlm dec)
    if(bm=="STLM decomposition"){
      tryCatch({
        fit = stlm(ts(xshort, frequency = freqf),s.window = 25,robust = TRUE,allow.multiplicative.trend = TRUE)
        fcast = forecast(fit, h=hf)
        forpred = cbind(daten,fcast)
        rownames(forpred)= 1:nrow(forpred)
        forpred$Model = "STLM decomposition"
        print(paste0(a,"-",j,"-","stlmdec"))
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
    #Univariate(prophet)
    if(bm=="Prophet"){  
      tryCatch({
        dfp = data.table::setnames(cbind(dates$Date,xshort),c("ds","y"))
        #lambda <- BoxCox.lambda(dfp$y)
        #dfp$y<-BoxCox(dfp$y,lambda)
        suppressMessages(prophet(changepoint.range=0.7,changepoint.prior.scale=10,seasonality.prior.scale =10 ))
        fit = add_seasonality(fit, name='weekly', period=7, fourier.order=25)
        fit = fit.prophet(fit, dfp)
        future = make_future_dataframe(fit, periods=hf, freq = freqfs,include_history = FALSE)
        fcast = prophet:::predict.prophet(fit, future)
        fcast = fcast %>% dplyr::select(starts_with("yhat"))
        #fcast = cbind(InvBoxCox(fcast$yhat_lower,lambda),InvBoxCox(fcast$yhat_upper,lambda),InvBoxCox(fcast$yhat,lambda))
        fcast = data.table::setnames(as.data.frame(fcast),c("Lo 95","Hi 95","Point Forecast"))
        forpred = cbind(daten,fcast)
        rownames(forpred)= 1:nrow(forpred)
        forpred$Model = "Prophet"
        print(paste0(a,"-",j,"-","prophet"))
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
    #Multivariate(Catboost)
    if(bm=="Cat Boost"){
      tryCatch({
        xnext_xreg = as.data.frame(lapply(xnext_xreg, function(x) as.numeric(as.character(x))))
        xshort_xreg= as.data.frame(lapply(xshort_xreg, function(x) as.numeric(as.character(x))))
        train_pool = catboost.load_pool(data = xshort_xreg, label = unlist(xshort[c(j)]))
        test_pool = catboost.load_pool(data = xnext_xreg)
        
        params = list(iterations=1000,
                      learning_rate=0.01,
                      depth=15,
                      loss_function='RMSE',
                      eval_metric='RMSE',
                      random_seed = 42,
                      od_type='Iter',
                      metric_period = 50,
                      od_wait=20,
                      use_best_model=FALSE)
        
        model = catboost.train(learn_pool = train_pool,test_pool=NULL,params = params)
        forpred = cbind(daten,catboost.predict(model,test_pool))
        names(forpred)[names(forpred) == "catboost.predict(model, test_pool)"] = "Point Forecast"
        forpred$Model = "Cat Boost"
        print(paste0(a,"-",j,"-","catboost"))
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
    #Univariate(AutoArima)
    #lambda = BoxCox.lambda(ts(xshort,frequency = freqf))
    #xshort_box = BoxCox(ts(xshort,frequency = freqf), lambda = lambda)
    if(bm=="Auto Arima"){  
      tryCatch({
        fit = auto.arima(ts(xshort,frequency = freqf),stepwise=FALSE,approximation = FALSE,seasonal=TRUE)
        fcast = forecast(fit, h=hf)
        forpred = cbind(daten,fcast)
        rownames(forpred)= 1:nrow(forpred)
        forpred$Model = "Auto Arima"
        print(paste0(a,"-",j,"-","arima"))
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
    #Mulvariate(AutoArima)
    if(bm=="Auto Arima Regressor"){  
      tryCatch({
        xshort_xreg_arima =  usdm::exclude(xshort_xreg,usdm::vifstep(xshort_xreg,th=4))
        xnext_xreg_arima = xnext_xreg[,colnames(xshort_xreg_arima)]
        fit = auto.arima(ts(xshort,frequency = freqf),xreg=as.matrix(xshort_xreg_arima),stepwise=FALSE,approximation = FALSE,seasonal=TRUE)
        fcast = forecast(fit, h=hf,xreg=as.matrix(xnext_xreg_arima))
        forpred = cbind(daten,fcast)
        rownames(forpred)= 1:nrow(forpred)
        forpred$Model = "Auto Arima Regressor"
        print(paste0(a,"-",j,"-","arimareg"))
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
    #Univariate(Fourier)
    if(bm=="Auto Arima Fourier"){   
      tryCatch({
        x = 1:length(ts(xshort,frequency = freqf))
        fit_g <- try(McSpatial::fourier(ts(xshort,frequency = freqf) ~ x,minq=1,maxq=K,crit="gcv"),silent = T)
        if(!'try-error' %in% class(fit_g)){
          const_val = fit_g$q
        }else{
          const_val = Kf
        }
        fit = auto.arima(ts(xshort,frequency = freqf),xreg=forecast::fourier(ts(xshort,frequency = freqf),K=const_val),stepwise=FALSE,approximation = FALSE,seasonal=FALSE)
        fcast = forecast(fit, h=hf,xreg=forecast::fourier(ts(xshort,frequency = freqf), K=const_val, h=hf))
        forpred = cbind(daten,fcast)
        rownames(forpred)= 1:nrow(forpred)
        forpred$Model = "Auto Arima Fourier"
        print(paste0(a,"-",j,"-","arimafourier"))
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
    # create data frame for visualisation
    tryCatch({
      forpred = merge(forpred,dfcv,by="Date")
      names(forpred)[names(forpred) == j] = "Observed"
      forpred$Location = a
      forpred$ExpenseType = j
      forpred$Year = year(forpred$Date)
      forpred$Month = month(forpred$Date)
      for_all = dplyr::bind_rows(for_all,forpred) 
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

for_temp = for_all
# write data for visualization
write.csv(for_all, file = "forecast.csv",row.names=FALSE)
write.csv(cv_all, file = "crossvalidation.csv",row.names=FALSE)
write.csv(coeff_all, file = "coefficient.csv",row.names=FALSE)
write.csv(foredata, file = "rawdata.csv",row.names=FALSE)
t2 = Sys.time()
endtime = round(as.numeric(difftime(time1 = t2, time2 = t1, units = "hours")), 3)