library(pROC, lib="//vhasfcreap/Sun/SEI_IIR/R packages")
library(ROCR, lib="//vhasfcreap/Sun/SEI_IIR/R packages")

library ("rlang", lib="//vhasfcreap/Sun/SEI_IIR/R packages")
library(imputeMissings, lib="//vhasfcreap/Sun/SEI_IIR/R packages")
library ("glmnet", lib="//vhasfcreap/Sun/SEI_IIR/R packages")
library ("tictoc", lib="//vhasfcreap/Sun/SEI_IIR/R packages")
library ("withr", lib="//vhasfcreap/Sun/SEI_IIR/R packages")
library ("randomForest", lib="//vhasfcreap/Sun/SEI_IIR/R packages")
library ("ggplot2", lib="//vhasfcreap/Sun/SEI_IIR/R packages")
library ("caret", lib="//vhasfcreap/Sun/SEI_IIR/R packages")
library ("haven", lib="//vhasfcreap/Sun/SEI_IIR/R packages")
library ("leaps", lib="//vhasfcreap/Sun/SEI_IIR/R packages")
library ("survival", lib="//vhasfcreap/Sun/SEI_IIR/R packages")
library ("backports", lib="//vhasfcreap/Sun/SEI_IIR/R packages")
library ("Formula", lib="//vhasfcreap/Sun/SEI_IIR/R packages")
library ("Hmisc", lib="//vhasfcreap/Sun/SEI_IIR/R packages")
library ("SparseM", lib="//vhasfcreap/Sun/SEI_IIR/R packages")
library ("rms", lib="//vhasfcreap/Sun/SEI_IIR/R packages")
library ("PredictABEL", lib="//vhasfcreap/Sun/SEI_IIR/R packages")
library ("nricens", lib="//vhasfcreap/Sun/SEI_IIR/R packages")
library ("stats", lib="//vhasfcreap/Sun/SEI_IIR/R packages")


#data=read.csv("V:/Health and Retirement Study/Sun/Sachin/psychosocial/data_interim/10012020 cohort_2010_allvars.csv") #178 variables + 1 age + 3 (HHIPDN, dod, death_flag) = 182 cols

data_raw= as.data.frame(read_sas("V:/Health and Retirement Study/Sun/Sachin/psychosocial/data_interim/cohort_2010_11212020_surv.sas7bdat"))
data_raw= subset(data_raw, select=-c(MLB037F, MLB046B, R10HLTHLM))


data = data_raw


### VAR LIST

var_list = read.csv("V:/Health and Retirement Study/Sun/Sachin/psychosocial/spreadsheet/08162020 HRS_var_list.csv") #154 (HRS LB)  (24 RAND SHOULD BE ADDED)
cat_var = subset(var_list, is.na(var_list$dummy)& is.na(var_list$year) & !is.na(var_list$group1))

cut_off = t(cat_var[,c(4,5,6, 8, 14)]) #group1, group2, group3, group1_other columns extract
colnames(cut_off) = cat_var[,1]


auc_mx= matrix(NA, ncol=4, nrow=length(cat_var[,1]))
BIC_mx= matrix(NA, ncol=4, nrow=length(cat_var[,1]))
rownames(auc_mx) = cat_var[,1]
colnames(auc_mx) = c("1.continous", "2.collapsed categorical", "3.full categorical", "best")
rownames(BIC_mx) = cat_var[,1]
colnames(BIC_mx) = c("1.continous", "2.collapsed categorical", "3.full categorical", "best")

catvar_manual=NULL


### Fx to compute the BIC comparison 
for (i in 1:length(cat_var[,1])){
  
  var = colnames(cut_off)[i]
  num = which(colnames(data)==colnames(cut_off)[i])
  
  data_sub = data[,c(2, num)] #death and categorical variable 
  thres = cut_off[1:4,i] #threshold
  skip = cut_off[5, i] #skip pattern? 
  
  data_sub$cat[data_sub[,2]>=thres[1]]=1
  data_sub$cat[data_sub[,2]>=thres[2]]=2
  data_sub$cat[data_sub[,2]>=thres[3]]=3
  data_sub$cat[data_sub[,2]==thres[4]]=1 # some special variables where 1, 2, 5 should be coded 1
  
  catvar_manual = cbind(catvar_manual, data_sub$cat)
  
  data_sub_nomiss = as.data.frame(na.omit(data_sub))
  
  #continuous
  test1 = glm(data_sub_nomiss[,1] ~ data_sub_nomiss[,2], family=binomial, na.action=na.omit)
  preds1 = predict(test1, type='response')
  auc_mx[i, 1] = round(auc(data_sub_nomiss[,1], preds1), 2)
  auc_mx[i, 1][skip==1] = -100/0
  BIC_mx[i, 1] = BIC(test1)
  BIC_mx[i, 1][skip==1] = 100/0
  
  #collapsed categorical 
  test2 = glm(data_sub_nomiss[,1] ~ as.factor(data_sub_nomiss[,3]), family=binomial, na.action=na.omit)
  preds2 = predict(test2, type='response')
  auc_mx[i, 2] = round(auc(data_sub_nomiss[,1], preds2), 2)
  BIC_mx[i, 2] = BIC(test2)
  
  #full categorical 
  test3 = glm(data_sub_nomiss[,1] ~ as.factor(data_sub_nomiss[,2]), family=binomial, na.action=na.omit)
  preds3 = predict(test3, type='response')
  auc_mx[i, 3] = round(auc(data_sub_nomiss[,1], preds3), 2)
  BIC_mx[i, 3] = BIC(test3)
  # 
  # test3 = glm(data[,1] ~ (data[,i])^2, family=binomial, na.action=na.omit)
  # BIC_cat[i-5, 3] = BIC(test3)
  
  best_auc =max(auc_mx[i, 1:3])
  auc_mx[i,4][auc_mx[i, 3]==best_auc]=3 #winning model. 3: factor, 2: manual cut-off, 1: row
  auc_mx[i,4][auc_mx[i, 2]==best_auc]=2
  auc_mx[i,4][auc_mx[i, 1]==best_auc]=1
  
  best_BIC =min(BIC_mx[i, 1:3])
  BIC_mx[i,4][BIC_mx[i, 3]==best_BIC]=3 #winning model. 3: factor, 2: manual cut-off, 1: row
  BIC_mx[i,4][BIC_mx[i, 2]==best_BIC]=2
  BIC_mx[i,4][BIC_mx[i, 1]==best_BIC]=1
}

colnames(catvar_manual) = cat_var[,1]

auc_nonskip = auc_mx[auc_mx[,1]>0,]
auc_skip = auc_mx[auc_mx[,1]<0, ]

BIC_nonskip =BIC_mx[BIC_mx[,1]<1000000000,]
BIC_skip =BIC_mx[BIC_mx[,1]>1000000000,]

nonskip = as.data.frame(cbind(auc_nonskip[,4], BIC_nonskip[,4]))
colnames(nonskip) = c("auc", "BIC")
table(nonskip$auc, nonskip$BIC)

skip = as.data.frame(cbind(auc_skip[,4], BIC_skip[,4]))
colnames(skip) = c("auc", "BIC")
table(skip$auc, skip$BIC)


#### HRS :: Deviding the variables by types 

## (1) Cateogircal, nonskip -- raw 
cat_row_names = rownames(BIC_nonskip[BIC_nonskip[,4]==1,])
cat_row = as.data.frame(subset(data, select = cat_row_names))

## (2) categorical -- manual 
cat_manual_names = rownames(BIC_nonskip[BIC_nonskip[,4]==2,])
cat_manual = as.data.frame(subset(catvar_manual, select = cat_manual_names))
cat_manual[] = lapply(cat_manual, factor)

## (2) -a, categorical -- manual, skip patterns
cat_manual_names_s = rownames(BIC_skip[BIC_skip[,4]==2,])
cat_manual_s = as.data.frame(subset(catvar_manual, select=cat_manual_names_s))
cat_manual_s[] = lapply(cat_manual_s, factor)

## (3) categorical -- factor
cat_factor_names = names(which(BIC_nonskip[,4]==3,))
cat_factor = as.data.frame(subset(data, select = cat_factor_names))
cat_factor[] = lapply(cat_factor, factor)

## (4) categorical -- factor, skip patterns
cat_factor_names_s = names(which(BIC_skip[,4]==3,))
cat_factor_s = as.data.frame(subset(data, select = cat_factor_names_s))
cat_factor_s[] = lapply(cat_factor_s, factor)

## (4) Dummies 
dummy_names = as.vector(subset(var_list, !is.na(var_list$dummy))[,1])
dummy = as.data.frame(subset(data, select = dummy_names))
dummy[] = lapply(dummy, factor)

### (5) Vairables that Cut-off is not avaialble : MLB026A, MLB044
var_others = as.data.frame(subset(data, select=c(MLB026A, MLB044)))
var_others[] = lapply(var_others, factor)


### (5) YEAR (there's no continuous; Sachin coded all 10+ cat variables manually. ) -- we decided not to use YEAR variables 
# year_names = as.vector (subset(var_list, !is.na(var_list$year))[,1])
# year= as.data.frame(subset(data, select = year_names))


### are we considering all variables? 
HRS_pred_names = c(cat_row_names, cat_manual_names, cat_manual_names_s, cat_factor_names, cat_factor_names_s, dummy_names, var_others) #133 variables here. (21 year variables excluded = 154


######RAND VARIABLES 
rand_list = read.csv("V:/Health and Retirement Study/Sun/Sachin/psychosocial/spreadsheet/08162020 RAND_var_list.csv") #154 (HRS LB)  (24 RAND SHOULD BE ADDED)


rand_cat_names = as.vector(subset(rand_list, is.na(rand_list$dummy) & is.na(rand_list$year) & is.na(rand_list$cont10cat))[,3])
rand_dummy_names = as.vector(subset(rand_list, !is.na(rand_list$dummy))[,3])
rand_cont_names = as.vector(subset(rand_list, !is.na(rand_list$cont10cat))[,3])

rand_cat = as.data.frame(subset(data, select=rand_cat_names))
rand_cat[] = lapply(rand_cat, factor) #8 predictors

rand_dummy = as.data.frame(subset(data, select=rand_dummy_names))
rand_dummy[] = lapply(rand_dummy, factor) # 11 predictors

rand_cont = as.data.frame(subset(data, select=rand_cont_names)) #5 predictors 
rand_cont$H10ATOTBC = as.numeric(rand_cont$H10ATOTBC)
rand_cont$H10ATOTB = as.numeric(rand_cont$H10ATOTB)

## All variables 
predictors = data.frame(data$death_flag, data$MAGE, cat_row, cat_manual, cat_manual_s, cat_factor, cat_factor_s, dummy, var_others, rand_cat, rand_dummy, rand_cont)
values2 = imputeMissings::compute(predictors)
predictors_imputed = imputeMissings::impute(predictors, object=values2)


gender= data.frame(read_sas("V:/Health and Retirement Study/Sun/Sachin/psychosocial/data_interim/cohort_2010_comorbid.sas7bdat"))
gender = subset(gender, select=c(RAGENDER))
demosvi.mx = cbind(predictors_imputed, gender)

demosvi.mx$RAGENDER = as.factor(demosvi.mx$RAGENDER)
demosvi.mx = subset(demosvi.mx, select=-c(H10CPL, H10ATOTB, H10ATOTBC, R10PENINC, R10HIGOV, R10COVR, R10LBRF))
# model.mx= model.matrix( ~ ., predictors_imputed)[,-1] ## Final data matrix with all functional form transformation. (dim 4302*509)
model.mx= model.matrix( ~ ., demosvi.mx)[,-1] ## Final data matrix with all functional form transformation. (dim 4302*509)


##################################
#### Harrell's Spline regression 
##################################

## 80:20 Training/test sample split
MAGE_spline = rcspline.eval(model.mx[,2], nk=4)
colnames(MAGE_spline) =  c("MAGE_sp1", "MAGE_sp2")
model.mx.sp = cbind(model.mx, MAGE_spline)

# set.seed(31415)
# train_id = sample(nrow(model.mx.sp), round(nrow(model.mx.sp)*0.8)) ## 8:2 split 
# train = model.mx.sp[train_id,]
# test = model.mx.sp[-train_id,]
# 
# summary (predictors)
# outcome =train[,1]
# predictor.mx = train[,-1]
# 
# outcome_test =test[,1]
# predictor.mx_test = test[,-1]


tic()
set.seed(1234321)
cvfit_bin <- cv.glmnet(predictor.mx, outcome, family="binomial") #without applying the model.matrix
toc()



#########################################
############# Cross_validating LASSO 
#########################################

model.mx = model.mx.sp


set.seed(1234)
#cvfit_full<- cv.glmnet(model.mx[, 2:(ncol(model.mx))], model.mx[,1], family="binomial")
cvfit_full<- cv.glmnet(model.mx[, 3:(ncol(model.mx)-1)], model.mx[,1], family="binomial")
glmnet.full <- cvfit_full$glmnet.fit 
optimal.lambda_full = cvfit_full$lambda.1se
lambda.index_full = which(glmnet.full$lambda==optimal.lambda_full)
optimal.beta_full  <- glmnet.full$beta[,lambda.index_full]

nonzero.coef_full <- abs(optimal.beta_full)>0 ##
selectedBeta_full <- optimal.beta_full[nonzero.coef_full] 
names(selectedBeta_full) 


## Should add comorbidity
comorbid= data.frame(read_sas("V:/Health and Retirement Study/Sun/Sachin/psychosocial/data_interim/cohort_2010_comorbid.sas7bdat"))
comorbid = subset(comorbid, select=c(R10CONDE))
comorb_spline = rcspline.eval(comorbid$R10CONDE, nk=4)
colnames(comorb_spline) =  c("comorb_sp1", "comorb_sp2")


model.mx.comorb = cbind(model.mx, comorbid, comorb_spline)


glm_fit =  pred_glm = auc_glm = list()


glm_fit[[1]] =  glm(data.death_flag ~ MLB001B+MLB001F+MLB020C+MLB020J+MLB039C+MLB001D2+MLB020D2+MLB021F2+MLB0262+MLB030A2+  
                                      MLB030C2+MLB017C2+MLB032C6+MLB009A6+MLB037C5+R10RETSAT3+R10WORK1,  
                    data = model.mx.comorb, family = binomial, na.action=na.omit)     

glm_fit[[2]] =  glm(data.death_flag ~ R10CONDE,
                    data = model.mx.comorb, family = binomial, na.action=na.omit)

glm_fit[[3]] =  glm(data.death_flag ~ RAGENDER2 + MAGE_sp1 + MAGE_sp2,
                    data = model.mx.comorb, family = binomial, na.action=na.omit)

#################################################
### subgroup analysis; charlson 
#################################################

charlson= data.frame(read_sas("V:/Health and Retirement Study/Sun/Sachin/psychosocial/data_interim/cohort_2010_charlson.sas7bdat"))
id = read.csv("V:/Health and Retirement Study/Sun/Sachin/psychosocial/data_interim/hhidpn.csv")
model.sp.morbid = cbind(model.mx, comorbid, comorb_spline, charlson, id)

model.sp.morbid = model.sp.morbid[which(model.sp.morbid$subcohort_flag==1),] ##N=2547

glm_fit[[4]] = glm(data.death_flag~Charlson, data=model.sp.morbid , family=binomial, na.action=na.omit )
#glm_fit[[5]] = glm(data.death_flag~Charlson +data.MAGE+MLB001B+MLB001F+MLB020C+MLB020J+MLB039C+MLB001D2+MLB020D2+MLB021F2+MLB0262+MLB030A2+  
#                    MLB030C2+MLB017C2+MLB032C6+MLB009A6+MLB037C5+R10RETSAT3+R10WORK1+RAGENDER2+MAGE_sp1,
#                    data=model.sp.morbid , family=binomial, na.action=na.omit )


################################
#### glm backward selection 
################################

glm_selected = glm(data.death_flag ~ MLB001B+MLB001F+MLB020C+MLB020J+MLB039C+MLB001D2+MLB020D2+MLB021F2+MLB0262+MLB030A2+  
                    MLB030C2+MLB017C2+MLB032C6+MLB009A6+MLB037C5+R10RETSAT3+R10WORK1+RAGENDER2 + data.MAGE+ MAGE_sp1, 
                   data = model.mx.comorb, family = binomial, na.action=na.omit)
n = dim(model.mx.comorb)[1]
glm_backward = step(glm_selected, direction="backward", k=log(n))
pred_bw = predict(glm_backward, model.mx.comorb)

print(roc(as.vector(model.mx.comorb[,1]), pred_bw)) 
predict.test = predict(glm_backward, as.data.frame(model.mx.sp12[, 2:(ncol(model.mx.sp12))]))
roc.test = roc(as.vector(model.mx.sp12[,1]), as.vector(predict.test)) 

