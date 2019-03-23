
#reading data
data = read.csv('analysisData.csv')
model = lm(price~minimum_nights+review_scores_rating,data)

# read in scoring data and apply model to generate predictions
scoringData = read.csv('scoringData.csv')
pred = predict(model,newdata=scoringData)

# construct submision from predictions
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'sample_submission.csv',row.names = F)

#visualization 
library(ggplot2)
ggplot(data,aes(x=cleaning_fee,y=price))+
geom_point()+
geom_smooth(method='lm')
library(dplyr)
bd2<-filter(data,bedrooms==0)

library(ggplot2)
ggplot(data,aes(x=room_type,y=price))+
geom_boxplot()

#looking for outliers
outlier_values <- boxplot.stats(data$bedrooms)$out  # outlier values.
boxplot(data$bedrooms, main="Bedrooms", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)
boxplot(price ~ bedrooms, data=data, main="Boxplot for bedrooms (continuos var) vs price")
#more plots
ggplot(data,aes(x=maximum_nights,y=price))+
geom_point()

ggplot(data,aes(x=minimum_nights,y=price))+
geom_point()

ggplot(data,aes(x=bathrooms,y=price))+
geom_point()
#correlation for feature selection(linear regression)
cor(data$price,data$accommodates)
cor(data$price,data$cleaning_fee)
cor(data$price,data$bathrooms)
cor(data$price,data$bedrooms)
cor(data$price,data$beds)
#corrplot to find interactions
data1<-subset(data,select=c(price,accommodates,bedrooms,bathrooms,num_amenities))
#corrplot for interactions
library(corrplot)
corrplot(cor(data1[,-1]),method = 'square',type = 'lower',diag = F)

#converting categorical variables to numeric variables
data$host_is_superhost<-as.numeric(data$host_is_superhost)
scoringData$host_is_superhost<-as.numeric(scoringData$host_is_superhost)

data$host_identity_verified<-as.numeric(data$host_identity_verified)
scoringData$host_identity_verified<-as.numeric(scoringData$host_identity_verified)

data$host_has_profile_pic<-as.numeric(data$host_has_profile_pic)
scoringData$host_has_profile_pic<-as.numeric(scoringData$host_has_profile_pic)
cor(data$host_has_profile_pic,data$price)

data$is_location_exact<-as.numeric(data$is_location_exact)
scoringData$is_location_exact<-as.numeric(scoringData$is_location_exact)
cor(data$is_location_exact,data$price)

data$has_availability<-as.numeric(data$has_availability)
scoringData$has_availability<-as.numeric(scoringData$has_availability)
cor(data$has_availability,data$price)

data$require_guest_phone_verification<-as.numeric(data$require_guest_phone_verification)
scoringData$require_guest_phone_verification<-as.numeric(scoringData$require_guest_phone_verification)
cor(data$require_guest_phone_verification,data$price)

data$instant_bookable<-as.numeric(data$instant_bookable)
scoringData$instant_bookable<-as.numeric(scoringData$instant_bookable)

#cleaned data for random forest and linear regression (first appraoch : imputing na with zero)
data$cleaning_fee[is.na(data$cleaning_fee)]<-0
scoringData$cleaning_fee[is.na(scoringData$cleaning_fee)]<-0
data$security_deposit[is.na(data$security_deposit)]<-0
scoringData$security_deposit[is.na(scoringData$security_deposit)]<-0
data$beds[is.na(data$beds)]<-0
scoringData$beds[is.na(scoringData$beds)]<-0
scoringData$reviews_per_month[is.na(scoringData$reviews_per_month)]<-0
#cleaning data using kNN (approach 2)
library(VIM)
data2<-kNN(data, variable = c('cleaning_fee','security_deposit','beds'), metric = NULL, k = 5,
           dist_var = colnames(data), weights = NULL, numFun = median,
           catFun = maxCat, makeNA = NULL, NAcond = NULL, impNA = TRUE,
           donorcond = NULL, mixed = vector(), mixed.constant = NULL,
           trace = FALSE, imp_var = TRUE, imp_suffix = "imp", addRandom = FALSE,
           useImputedDist = TRUE, weightDist = FALSE)

scoringdata2<-kNN(scoringData, variable = c('cleaning_fee','security_deposit','beds'), metric = NULL, k = 5,
                  dist_var = colnames(scoringData), weights = NULL, numFun = median,
                  catFun = maxCat, makeNA = NULL, NAcond = NULL, impNA = TRUE,
                  donorcond = NULL, mixed = vector(), mixed.constant = NULL,
                  trace = FALSE, imp_var = TRUE, imp_suffix = "imp", addRandom = FALSE,
                  useImputedDist = TRUE, weightDist = FALSE)

data$cleaning_fee<-data2$cleaning_fee
scoringData$cleaning_fee<-scoringdata2$cleaning_fee
data$security_deposit<-data2$security_deposit
scoringData$security_deposit<-scoringdata2$security_deposit
data$beds<-data2$beds
scoringData$beds<-scoringdata2$beds

#missing values in reviews_per_month
scoringData$reviews_per_month[is.na(scoringData$reviews_per_month)]<-0
#cleaning property_type (using important levels in prop type)
data$property_type[!data$property_type=='Apartment'&!data$property_type=='House'&!data$property_type=='Loft'&!data$property_type=='Condominium'&!data$property_type=='Townhouse']<-'Other'
scoringData$property_type[!scoringData$property_type=='Apartment'&!scoringData$property_type=='House'&!scoringData$property_type=='Loft'&!scoringData$property_type=='Condominium'&!scoringData$property_type=='Townhouse']<-'Other'
data$property_type<-as.factor(data$property_type)
scoringData$property_type<-as.factor(scoringData$property_type)
scoringData$property_type<-factor(scoringData$property_type,levels=levels(data$property_type))

# using amenities to create variables
library(stringr)
data$ifp<-str_detect(data$amenities,'Indoor fireplace')
scoringData$ifp<-str_detect(scoringData$amenities,'Indoor fireplace')
data$ifp<-as.numeric(data$ifp)
scoringData$ifp<-as.numeric(scoringData$ifp)
cor(data$ifp,data$price)

data$ti<-str_detect(data$amenities,c('Cable TV','Internet'))
scoringData$ti<-str_detect(scoringData$amenities,c('Cable TV','Internet'))
data$ti<-as.numeric(data$ti)
scoringData$ti<-as.numeric(scoringData$ti)
cor(data$ti,data$price)

data$ge<-str_detect(data$amenities,c('Gym','TV'))
scoringData$ge<-str_detect(scoringData$amenities,c('Gym','TV'))
data$ge<-as.numeric(data$ge)
scoringData$ge<-as.numeric(scoringData$ge)
cor(data$ge,data$price)

data$wd<-str_detect(data$amenities,c('Washer','Dryer'))
scoringData$wd<-str_detect(scoringData$amenities,c('Washer','Dryer'))
data$wd<-as.numeric(data$wd)
scoringData$wd<-as.numeric(scoringData$wd)
cor(data$wd,data$price)

data$tct<-str_detect(data$amenities,c('TV','Cable TV'))
scoringData$tct<-str_detect(scoringData$amenities,c('TV','Cable TV'))
data$tct<-as.numeric(data$tct)
scoringData$tct<-as.numeric(scoringData$tct)
cor(data$tct,data$price)

data$ak<-str_detect(data$amenities,c('Iron','Hair dryer'))
scoringData$ak<-str_detect(scoringData$amenities,c('Iron','Hair dryer'))
data$ak<-as.numeric(data$ak)
scoringData$ak<-as.numeric(scoringData$ak)
cor(data$ak,data$price)

data$ba<-str_detect(data$amenities,c('Laptop','TV'))
scoringData$ba<-str_detect(scoringData$amenities,c('Laptop','TV'))
data$ba<-as.numeric(data$ba)
scoringData$ba<-as.numeric(scoringData$ba)
cor(data$ba,data$price)

data$pp<-str_detect(data$amenities,'Doorman')
scoringData$pp<-str_detect(scoringData$amenities,'Doorman')
data$pp<-as.numeric(data$pp)
scoringData$pp<-as.numeric(scoringData$pp)
cor(data$pp,data$price)

data$wc<-str_detect(data$amenities,'Wheelchair')
scoringData$wc<-str_detect(scoringData$amenities,'Wheelchair')
data$wc<-as.numeric(data$wc)
scoringData$wc<-as.numeric(scoringData$wc)
cor(data$wc,data$price)

data$d<-str_detect(data$amenities,'Air conditioning')
scoringData$d<-str_detect(scoringData$amenities,'Air conditioning')
data$d<-as.numeric(data$d)
scoringData$d<-as.numeric(scoringData$d)
cor(data$d,data$price)

data$bu<-str_detect(data$amenities,'Buzzer')
scoringData$bu<-str_detect(scoringData$amenities,'Buzzer')
data$bu<-as.numeric(data$bu)
scoringData$bu<-as.numeric(scoringData$bu)
cor(data$bu,data$price)

data$fe<-str_detect(data$amenities,'fire')
scoringData$fe<-str_detect(scoringData$amenities,'fire')
data$fe<-as.numeric(data$fe)
scoringData$fe<-as.numeric(scoringData$fe)

data$iron<-str_detect(data$amenities,'iron')
scoringData$iron<-str_detect(scoringData$amenities,'iron')
data$iron<-as.numeric(data$iron)
scoringData$iron<-as.numeric(scoringData$iron)
cor(data$iron,data$price)
#using information from summary, description, space etc 
library(stringr)
data$lux<-str_detect(data$summary,'luxurious')
scoringData$lux<-str_detect(scoringData$summary,'luxurious')
data$lux<-as.numeric(data$lux)
scoringData$lux<-as.numeric(scoringData$lux)
cor(data$lux,data$price)

data$reno<-str_detect(data$summary,'renovated')
scoringData$reno<-str_detect(scoringData$summary,'renovated')
data$reno<-as.numeric(data$reno)
scoringData$reno<-as.numeric(scoringData$reno)
cor(data$reno,data$price)

data$clean<-str_detect(data$summary,'clean')
scoringData$clean<-str_detect(scoringData$summary,'clean')
data$clean<-as.numeric(data$clean)
scoringData$clean<-as.numeric(scoringData$clean)
cor(data$clean,data$price)

data$name<-str_detect(data$name,'private')
scoringData$name<-str_detect(scoringData$name,'private')
data$name<-as.numeric(data$name)
scoringData$name<-as.numeric(scoringData$name)
cor(data$name,data$price)

data$dsc<-str_detect(data$description,'private')
scoringData$dsc<-str_detect(scoringData$description,'private')
data$dsc<-as.numeric(data$dsc)
scoringData$dsc<-as.numeric(scoringData$dsc)
cor(data$dsc,data$price)

library(stringr)
data$quiet<-str_detect(data$neighborhood_overview,'quiet')
scoringData$quiet<-str_detect(scoringData$neighborhood_overview,'quiet')
data$quiet<-as.numeric(data$quiet)
scoringData$quiet<-as.numeric(scoringData$quiet)
cor(data$quiet,data$price)

data$spacious<-str_detect(data$description,'spacious')
scoringData$spacious<-str_detect(scoringData$description,'spacious')
data$spacious<-as.numeric(data$spacious)
scoringData$spacious<-as.numeric(scoringData$spacious)
cor(data$spacious,data$price)

data$cozy<-str_detect(data$name,'cozy')
scoringData$cozy<-str_detect(scoringData$name,'cozy')
data$cozy<-as.numeric(data$cozy)
scoringData$cozy<-as.numeric(scoringData$cozy)
cor(data$cozy,data$price)

data$terrace<-str_detect(data$name,'terrace')
scoringData$terrace<-str_detect(scoringData$summary,'terrace')
data$terrace<-as.numeric(data$terrace)
scoringData$terrace<-as.numeric(scoringData$terrace)
cor(data$terrace,data$price)

data$threebed<-str_detect(data$name,'3 bedroom')
scoringData$threebed<-str_detect(scoringData$summary,'3 bedroom')
data$threebed<-as.numeric(data$threebed)
scoringData$threebed<-as.numeric(scoringData$threebed)
cor(data$threebed,data$price)

data$dplex<-str_detect(data$name,' garden duplex')
scoringData$dplex<-str_detect(scoringData$name,'garden duplex')
data$dplex<-as.numeric(data$dplex)
scoringData$dplex<-as.numeric(scoringData$dplex)
cor(data$dplex,data$price)

data$cpa<-str_detect(data$name,'central park')
scoringData$cpa<-str_detect(scoringData$name,'central park')
data$cpa<-as.numeric(data$cpa)
scoringData$cpa<-as.numeric(scoringData$cpa)
cor(data$cpa,data$price)

data$condo<-str_detect(data$name,'condo')
scoringData$condo<-str_detect(scoringData$name,'condo')
data$condo<-as.numeric(data$condo)
scoringData$condo<-as.numeric(scoringData$condo)
cor(data$condo,data$price)

#counting the number of amenities
x = strsplit(as.character(data$amenities), ",")
y = c()
for (i in 1:length(x)){
  y[i] = length(x[[i]])
}
data$num_amenities = y

x1 = strsplit(as.character(scoringData$amenities), ",")
y1 = c()
for (i in 1:length(x1)){
  y1[i] = length(x1[[i]])
}
scoringData$num_amenities = y1
cor(data$num_amenities,data$price)

#lasso for feature selection
library(glmnet)
x = model.matrix(price~neighbourhood_group_cleansed+security_deposit+instant_bookable+calculated_host_listings_count+reviews_per_month+availability_30+availability_365+minimum_nights+maximum_nights+host_has_profile_pic+bed_type+is_location_exact+guests_included+extra_people+room_type+property_type+latitude+longitude+bedrooms+bathrooms+beds+accommodates+cleaning_fee+cancellation_policy+is_business_travel_ready+ifp+ti+host_is_superhost+review_scores_location+d+ba+ak+pp+wc+tct+wd+ge+bu+lux+terrace+cpa+threebed+host_identity_verified,data=data)
y = data$price
lassoModel = glmnet(x,y, alpha=1) # Note default for alpha is 1 which corresponds to Lasso
lassoModel
plot(lassoModel,xvar='lambda',label=T)
plot(lassoModel,xvar='dev',label=T)
cv.lasso = cv.glmnet(x,y,alpha=1) # 10-fold cross-validation
plot(cv.lasso)
coef(cv.lasso)

#XGBoost(Best model : RMSE 50.85)

library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
library(Matrix)

#matrix for data (missing values not cleaned for boosting)
data30= subset(data, select = c(price,num_amenities,square_feet,weekly_price,host_response_time,host_response_rate,review_scores_rating,availability_60,availability_90,neighbourhood_group_cleansed,security_deposit,calculated_host_listings_count,reviews_per_month,availability_30,availability_365,minimum_nights,maximum_nights,is_location_exact,guests_included,extra_people,room_type,property_type,latitude,longitude,bedrooms,bathrooms,beds,accommodates,cleaning_fee,cancellation_policy,is_business_travel_ready,ifp,ti,host_is_superhost,d,ba,ak,pp,wc,tct,wd,ge,bu,lux,host_identity_verified))
#matrix for scoringData
scoringData$price<-NA
scoringData30=subset(scoringData,select=c(price,num_amenities,square_feet,weekly_price,host_response_time,host_response_rate,review_scores_rating,availability_60,availability_90,neighbourhood_group_cleansed,security_deposit,calculated_host_listings_count,reviews_per_month,availability_30,availability_365,minimum_nights,maximum_nights,is_location_exact,guests_included,extra_people,room_type,property_type,latitude,longitude,bedrooms,bathrooms,beds,accommodates,cleaning_fee,cancellation_policy,is_business_travel_ready,ifp,ti,host_is_superhost,d,ba,ak,pp,wc,tct,wd,ge,bu,lux,host_identity_verified))
#split into train and test
library(caret)
a101<-createDataPartition(data30$price,p = 0.7,list=FALSE,groups = 100)
train101<-data30[a101,]
test101<-data30[-a101,]

#cross validation to tune parameters
cv.res=xgb.cv(data = data.matrix(train101[,-1]),nfold=5,label = train101$price,nround=3000, objective="reg:linear")

#parameters for my final model
xgb999<- xgboost(data = data.matrix(train101[,-1]), 
                 label = train101$price, 
                 eta = 0.01,gamma=0,
                 max_depth = 7,
                 nround=6000, 
                 objective="reg:linear",
                 early_stopping_rounds=50,
                 print_every_n=500,
                 subsample = 0.5,
                 seed =1
)
#train rmse : 9
predxgb<-predict(xgb999,newdata=data.matrix(train101[,-1]))
RMSE(train101$price,predxgb999)
#test rmse : 51
predxgb1<-predict(xgb999,newdata=data.matrix(test101[,-1]))
RMSE(test101$price,predxgb999)

#running the same model on whole dataset
xgb1000<- xgboost(data = data.matrix(data30[,-1]), 
               label = data30$price, 
               eta = 0.01,gamma=0,
               max_depth = 7,
               nround=6000, 
               objective="reg:linear",
               early_stopping_rounds=50,
               print_every_n=500,
               subsample = 0.5,
               seed =1
)

summary(xgb1000)

predxgb2<-predict(xgb1000,newdata=data.matrix(data30[,-1]))
RMSE(data30$price,predxgb2)
#predictions on scoringData
predxb3<-predict(xgb1000,newdata=data.matrix(scoringData30[,-1]))
predxb3
#submission file (public rmse : 50.85, private rmse : 53.15)
submissionFile= data.frame(id = scoringData$id, price = predxb3)
write.csv(submissionFile, 'sample_submission.csv',row.names = F) 








