setwd("C:\\My R Work")
rm(list=ls())

############################ 라이브러리, 데이터 로드 ###########################
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("GGally")
#install.packages("stringr")
#install.packages("caret")
#install.packages("corrplot")
#install.packages("nnet")
#install.packages("randomForest")
#install.packages("kernlab")
#install.packages("xgboost")
#install.packages("mltools")
#install.packages("data.table")
#install.packages("fmsb")
#install.packages("ROCR")
#install.packages("DescTools")
#install.packages("devtools")
#install.packages("gridExtra")
library(dplyr)
library(ggplot2)
library(GGally)
library(stringr)
library(caret)
library(corrplot)
library(nnet)
library(randomForest)
library(kernlab)
library(xgboost)
library(mltools)
library(data.table)
library(fmsb)
library(ROCR)
library(DescTools)
library(catboost)
library(gridExtra)

raw_train = read.csv('train.csv')
raw_test = read.csv('test.csv')
train <- raw_train
test <- raw_test

## train, test 데이터 결합
n = nrow(train)
m = nrow(test)
test$credit <- ""
tt <- rbind(train, test)



############################## 데이터 확인 #####################################
## 결측치 확인
str(tt)
sum(is.na(tt))
colSums(tt=="")
# occyp_type : 11323개
# credit : 10000개 (test data)

## 변수마다 카디널리티와 데이터 타입 확인
for(i in 1:(ncol(tt)-1)) {
  print(paste(colnames(tt[i]), 
              length(unique(tt[,i])), 
              class(tt[1,i]), 
              sep ="        "))
}



############################# 데이터 전처리 ####################################
### 범주형(명목) = [gender,     car,           reality,      income_type, 
###                 edu_type,   family_type,   house_type,   work_phone, 
###                 phone,      email,         occyp_type]

## gender
table(tt$gender)
tt$gender <- as.factor(tt$gender)

## car
table(tt$car)
tt$car <- as.factor(tt$car)

## reality
table(tt$reality)
tt$reality <- as.factor(tt$reality)

## income_type
table(tt$income_type)
# 관측치가 작은 그룹을 재범주화
tt$income_type[tt$income_type == 'Student'] <- 'State servant'
tt$income_type <- as.factor(tt$income_type)

## edu_type
table(tt$edu_type)
# 관측치가 작은 그룹을 재범주화
tt$edu_type[tt$edu_type == 'Academic degree'] <- 'Higher education'
tt$edu_type <- as.factor(tt$edu_type)

## family_type
table(tt$family_type)
tt$family_type <- as.factor(tt$family_type)

## house_type
table(tt$house_type)
tt$house_type <- as.factor(tt$house_type)
# 파생변수 (자기 집인지 아닌지 - 근데 구분 어려움)
# tt$house_type <- ifelse(tt$house_type == 'House / apartment', 0, ifelse(tt$house_type == 2, 1, ifelse(train_test$family_size == 3, 2, ifelse(train_test$family_size == 4, 3, 4))))

## work_phone, phone, email
tt$work_phone <- ifelse(tt$work_phone == 0, "N", "Y")
tt$phone <- ifelse(tt$phone == 0, "N", "Y")
tt$email <- ifelse(tt$email == 0, "N", "Y")
table(tt$work_phone)
table(tt$phone)
table(tt$email)
tt$work_phone <- as.factor(tt$work_phone)
tt$phone <- as.factor(tt$phone)
tt$email <- as.factor(tt$email)

## occyp_type
table(tt$occyp_type)
# 결측치 처리
# 새로운 그룹 생성(not in work)
table(tt$occyp_type[tt$DAYS_EMPLOYED > 0])
nrow(tt[tt$DAYS_EMPLOYED > 0,])
tt$occyp_type[tt$DAYS_EMPLOYED > 0] <- 'not in work'
# 나머지 결측치는 하나의 직업으로 간주 (총 20개의 직업)
tt$occyp_type[tt$occyp_type==""] <- "missing value"
tt$occyp_type <- as.factor(tt$occyp_type)


### 범주형(순서) = [child_num, family_size]
## child_num
table(tt$child_num)
tt$child_num_prev <- tt$child_num
# 관측치가 작은 그룹을 재범주화
tt$child_num[tt$child_num > 4] <- 4
tt$child_num <- as.factor(tt$child_num)

## family_size
table(tt$family_size)
tt$family_size_prev <- tt$family_size
# 관측치가 작은 그룹을 재범주화
tt$family_size[tt$family_size > 4] <- 4
tt$family_size <- as.factor(tt$family_size)


### 연속형 = [income_total, DAYS_BIRTH, DAYS_EMPLOYED, begin_month]
## income_total
summary(tt$income_total)
# 이상치 1529개
IRQ_income <- summary(tt$income_total)[5] - summary(tt$income_total)[2]
IRQ_income_t <- summary(tt$income_total)[5] + 1.5*IRQ_income
IRQ_income_b <- summary(tt$income_total)[2] - 1.5*IRQ_income
nrow(tt[tt$income_total>IRQ_income_t | tt$income_total<IRQ_income_b,])
# 로그변환으로 극단적인 이상치 완화
tt$scale_income_total <- scale(log(tt$income_total+1))
summary(tt$scale_income_total)

## DAYS_BIRTH
summary(tt$DAYS_BIRTH)
tt$DAYS_BIRTH <- abs(tt$DAYS_BIRTH)
# 이상치 0개
IRQ_birth <- summary(tt$DAYS_BIRTH)[5] - summary(tt$DAYS_BIRTH)[2]
IRQ_birth_t <- summary(tt$DAYS_BIRTH)[5] + 1.5*IRQ_birth
IRQ_birth_b <- summary(tt$DAYS_BIRTH)[2] - 1.5*IRQ_birth
nrow(tt[tt$DAYS_BIRTH>IRQ_birth_t | tt$DAYS_BIRTH<IRQ_birth_b,])
tt$scale_DAYS_BIRTH <- scale(tt$DAYS_BIRTH)

## DAYS_EMPLOYED
summary(tt$DAYS_EMPLOYED)
# 365243은 고용 일수를 0일로 간주할 수 있음
tt$DAYS_EMPLOYED[tt$DAYS_EMPLOYED == 365243] <- 0
tt$DAYS_EMPLOYED <- abs(tt$DAYS_EMPLOYED)
# 이상치 1770개
IRQ_emp <- summary(tt$DAYS_EMPLOYED)[5] - summary(tt$DAYS_EMPLOYED)[2]
IRQ_emp_t <- summary(tt$DAYS_EMPLOYED)[5] + 1.5*IRQ_emp
IRQ_emp_b <- summary(tt$DAYS_EMPLOYED)[2] - 1.5*IRQ_emp
nrow(tt[tt$DAYS_EMPLOYED>IRQ_emp_t | tt$DAYS_EMPLOYED<IRQ_emp_b,])
# 정규화가 어려움 -> 여러 파생변수를 생성하여 이상치를 다루기
tt$scale_DAYS_EMPLOYED <- scale(log(tt$DAYS_EMPLOYED+1))

## begin_month
summary(tt$begin_month)
tt$begin_month <- abs(tt$begin_month)
# 이상치 0개
IRQ_month <- summary(tt$begin_month)[5] - summary(tt$begin_month)[2]
IRQ_month_t <- summary(tt$begin_month)[5] + 1.5*IRQ_month
IRQ_month_b <- summary(tt$begin_month)[2] - 1.5*IRQ_month
nrow(tt[tt$begin_month>IRQ_month_t | tt$begin_month<IRQ_month_b,])
tt$scale_begin_month <- scale(tt$begin_month)


### 메모리 정리
rm(i, IRQ_birth, IRQ_birth_b, IRQ_birth_t, IRQ_emp, IRQ_emp_b, IRQ_emp_t,
   IRQ_income, IRQ_income_b, IRQ_income_t, IRQ_month, IRQ_month_b, IRQ_month_t)




############################## 파생변수 생성 ###################################
## fam_without_child
tt$fam_without_child <- tt$family_size_prev-tt$child_num_prev
table(tt$fam_without_child)

tt$fam_without_child[tt$fam_without_child < 1] <- 1
tt$fam_without_child <- as.factor(tt$fam_without_child)

## age
tt$age <- tt$DAYS_BIRTH / 365
summary(tt$age)

## iswork : 근무여부
tt$iswork <- ifelse(tt$DAYS_EMPLOYED==0,"N","Y")
table(tt$iswork)
tt$iswork <- as.factor(tt$iswork)

## YEARS_EMPLOYED : 근속연수
tt$YEARS_EMPLOYED <- tt$DAYS_EMPLOYED / 365
summary(tt$YEARS_EMPLOYED)

## DAYS_UNEMPLOYED : DAYS_BIRTH - DAYS_EMPLOYED 
tt$DAYS_UNEMPLOYED <- tt$DAYS_BIRTH - tt$DAYS_EMPLOYED
summary(tt$DAYS_UNEMPLOYED)
tt$scale_DAYS_UNEMPLOYED <- scale(log(tt$DAYS_UNEMPLOYED+1))
summary(tt$scale_DAYS_UNEMPLOYED)

## YEARS_UNEMPLOYED
tt$YEARS_UNEMPLOYED <- tt$DAYS_UNEMPLOYED / 365
summary(tt$YEARS_UNEMPLOYED)

## scale_income_per_year
tt$scale_income_per_year <- scale(log(tt$income_total/(tt$YEARS_EMPLOYED + 1)+1))
summary(tt$scale_income_per_year)

## isna : 결측치가 있었던 행
tt$isna <- ifelse(tt$occyp_type=="" | tt$occyp_type=="not in work", "Y", "N")
table(tt$isna)
tt$isna <- as.factor(tt$isna)

## 필요없는 변수 제거
tt$index            <- NULL
tt$FLAG_MOBIL       <- NULL
tt$family_size_prev <- NULL
tt$child_num_prev   <- NULL

## 최종 변수 확인
colnames(tt)

## 데이터 분리(train : test = 26547 : 10000)
train <- tt[1:n, ]
test <- tt[(n+1):(n+m), ]
train$credit <- as.factor(train$credit)
test$credit <- NULL




############################# 탐색적 데이터 분석 ###############################
# ### 연속형 변수의 이해
# ## 박스플롯
# par(mfrow=c(2,2))
# boxplot(train$age, col="#FFD9FA", main="Age")
# boxplot(train$scale_income_total, col="#E8D9FF", main="scaled Income Total")
# boxplot(train$YEARS_EMPLOYED, col="#CEF279", main="Years Employed")
# boxplot(train$begin_month, col="#FAF4C0", main="Begin Month")
# 
# ### 변수간의 관계 파악
# ## 직업과 성별에 따른 총 소득 박스플롯
# occ_gender <- train %>%
#   group_by(occyp_type, gender) %>%
#   summarise(n = n())
# ggplot(train,
#        aes(x=scale_income_total,
#            y=occyp_type, color=gender)) +
#   geom_boxplot(position = position_dodge(width=0.9)) +
#   stat_summary(geom = 'text',
#                label = occ_gender$n,
#                fun = max,
#                hjust = -0.15,
#                vjust = 0.4,
#                position = position_dodge(width=0.9)) +
#   theme_grey() +
#   labs(title="Income by Occupation") +
#   theme(plot.title = element_text(hjust = 0.5, vjust=1, size=20))
# ## 범주형 변수 모자이크 플롯
# par(mfrow=c(2,2))
# mosaicplot(~reality+work_phone, data = train,
#            color = c("#5CD1E5","grey"),
#            main = "Reality / Work Phone")
# mosaicplot(~phone+work_phone, data = train,
#            color = c("#47C83E","grey"),
#            main = "Phone / Work Phone")
# mosaicplot(~child_num+iswork, data = train,
#            color = c("grey","#F2CB61"),
#            main = "Child Num / Is Work")
# mosaicplot(~car+gender, data = train,
#            color = c("#99004C","grey"),
#            main = "Car / Gender")
# 
# ### 전체 credit 분포 확인
# ## credit 피벗테이블 작성
# n_credit <- train %>%
#   group_by(credit) %>%
#   summarise(n = n())
# n_credit$n_rate <- n_credit$n / sum(n_credit$n)
# ## credit 파이차트 생성
# ggplot(n_credit, aes(x="", y=n_rate, fill=credit), main="Credit") +
#   geom_bar(width=1, stat="identity", color="white") +
#   coord_polar("y") +
#   geom_text(aes(label=paste(round(n_rate*100,1),"%")),
#             position=position_stack(vjust=0.5),
#             color="white", size=10) +
#   theme_void() + labs(title="Credit Rate",
#                       x ="", y = "") +
#   theme(plot.title = element_text(hjust = 0.5, vjust=-5, size=20))
# 
# ### 범주형 변수 그룹별 credit 분포 확인
# ## 막대그래프 함수 정의
# train_plot <- function(str){
#   # 변수별 피벗테이블 작성
#   n_List <- train %>%
#     group_by(.data[[str]], credit) %>%
#     summarise(n = n())
#   n_List$n_rate <- 0
#   colnames(n_List) <- c("s","credit","n","n_rate")
#   for(i in 1:nrow(n_List)){
#     n_List$n_rate[i] <- n_List$n[i]/sum(n_List[n_List$s==n_List$s[i],'n'])
#   }
#   # 변수별 막대그래프 생성
#   ggplot(data=n_List, aes(x=s,y=n,fill=credit)) +
#     geom_col(position="stack") +
#     geom_text(aes(label=paste(round(n_rate*100,1),"%")),
#               color="white",
#               position=position_stack(vjust=.59), size=4.1) +
#     aes(reorder(str_wrap(s, 12),-n), n) + xlab(str) + ylab("count")
# }
# category_list = c("gender",      "car",               "reality",
#                   "child_num",   "income_type",       "edu_type",
#                   "family_type", "house_type",        "work_phone",
#                   "phone",       "email",             "occyp_type",
#                   "family_size", "fam_without_child", "iswork",
#                   "isna",        "credit")
# # for(i in 1:(length(category_list)-1)){
# #   print(train_plot(category_list[i]))
# # }
# train_plot("gender")
# train_plot("car")
# train_plot("reality")
# train_plot("child_num")
# train_plot("income_type")
# train_plot("edu_type")
# train_plot("family_type")
# train_plot("house_type")
# train_plot("work_phone")
# train_plot("phone")
# train_plot("email")
# train_plot("occyp_type")
# train_plot("family_size")
# train_plot("fam_without_child")
# train_plot("iswork")
# train_plot("isna")
# 
# 
# ### 연속형 변수 분포 확인
# ## 주요 연속형 변수의 boxplot
# par(mfrow = c(2,2))
# boxplot(tt$age, main="Age")
# boxplot(tt$scale_income_total, main="scaled Income Total")
# boxplot(tt$YEARS_EMPLOYED, main="Years Employed")
# boxplot(tt$begin_month, main="Begin Month")
# 
# ## boxplot 함수 정의
# box_nu <- function(str){
#   ggplot(train) +
#     geom_boxplot(aes(x = credit, y = .data[[str]], fill =credit),
#                  alpha = 0.2, outlier.color = 'red') +
#     geom_jitter(aes(x= credit, y= .data[[str]],col = credit),
#                 alpha = 0.02) +
#     theme_bw()}
# ## credit별 히스토그램 함수 정의
# hist_nu <- function(str,bw){
#   ggplot(train) +
#     geom_histogram(aes(x = .data[[str]], fill = credit ),
#                    binwidth = bw, alpha = 0.4) +
#     theme_bw() +
#     labs(fill = "credit")
# }
# ## 밀도그래프 함수 정의
# den_nu <- function(str){
#   ggplot(train) +
#     stat_density(mapping=aes(x= .data[[str]], fill = credit),
#                  position = "identity") +
#     facet_wrap(~credit, nrow=3)
# }
# 
# ## income_total, scale_income_per_year
# # 박스플롯
# box_nu("income_total")
# box_nu("scale_income_total")
# box_nu("scale_income_per_year")
# skew<-function(x){
#   sum3<-sum((x-mean(x))^3)
#   sum2<-sum((x-mean(x))^2)
#   skew<-(sqrt(length(x))*sum3)/(sum2^(3/2))
#   skew
# }
# skew(train$income_total) # 2.65912
# # 히스토그램
# hist_nu("income_total",50000)
# hist_nu("scale_income_total",0.45)
# hist_nu("scale_income_per_year",0.15)
# # 밀도함수
# den_nu("income_total")
# den_nu("scale_income_total")
# den_nu("scale_income_per_year")
# 
# ## DAYS_BIRTH, age
# # 박스플롯
# box_nu("age")
# box_nu("scale_DAYS_BIRTH")
# # 히스토그램
# hist_nu("age",1)
# hist_nu("scale_DAYS_BIRTH",0.1)
# # 밀도함수
# den_nu("age")
# den_nu("scale_DAYS_BIRTH")
# 
# ## DAYS_EMPLOYED, DAYS_UNEMPLOYED
# # 박스플롯
# box_nu("YEARS_EMPLOYED")
# box_nu("scale_DAYS_EMPLOYED")
# box_nu("YEARS_UNEMPLOYED")
# box_nu("scale_DAYS_UNEMPLOYED")
# # 히스토그램
# hist_nu("YEARS_EMPLOYED",1)
# hist_nu("scale_DAYS_EMPLOYED",0.1)
# hist_nu("YEARS_UNEMPLOYED",1)
# hist_nu("scale_DAYS_UNEMPLOYED",0.1)
# # 밀도함수
# den_nu("YEARS_EMPLOYED")
# den_nu("scale_DAYS_EMPLOYED")
# den_nu("YEARS_UNEMPLOYED")
# den_nu("scale_DAYS_UNEMPLOYED")
# 
# ## begin_month
# # 박스플롯
# box_nu("begin_month")
# box_nu("scale_begin_month")
# # 히스토그램
# hist_nu("begin_month",1)
# hist_nu("scale_begin_month",0.1)
# # 밀도함수
# den_nu("begin_month")
# den_nu("scale_begin_month")
# 
# 
# ### 주요 연속형 변수의 산점도
# pairs_list <- as.data.frame(cbind(train$scale_income_total,train$age,
#                                   train$begin_month,train$YEARS_EMPLOYED,
#                                   train$credit))
# colnames(pairs_list) <- c("Scaled Income Total", "Age",
#                           "Begin_Month",         "Years_Employed",
#                           "credit")
# pairs_list$credit <- as.factor(pairs_list$credit-1)
# ## 산점도
# ggpairs(data=pairs_list, aes(colour = credit, alpha = 0.4),
#         title="Numeric Variable & Credit")
# 
# ### 히트맵
# ## 히트맵 함수 정의
# par(mfrow=c(1,1))
# col <- colorRampPalette(c("#BB4444",
#                           "#EE9988",
#                           "#FFFFFF",
#                           "#77AADD",
#                           "#4477AA"))
# hit_plot <- function(hit_input, tit){
#   corrplot(hit_input,
#            method="color",
#            type="lower",
#            order="FPC",
#            addCoef.col = "black",
#            tl.col="black",
#            tl.srt = -0.001,
#            tl.cex = 1,
#            tl.offset = 0.35,
#            diag = T,
#            col=col(200),
#            main = paste("\n", tit))
# }
# 
# ## 범주형 변수의 Cramer'V 상관계수
# cramer_col <- cbind(train$gender,      train$car,               train$reality,
#                     train$child_num,   train$income_type,       train$edu_type,
#                     train$family_type, train$house_type,        train$work_phone,
#                     train$phone,       train$email,             train$occyp_type,
#                     train$family_size, train$fam_without_child, train$iswork,
#                     train$isna,        train$credit)
# cramer <- matrix(nrow=length(category_list),
#                  ncol=length(category_list))
# for(i in 1:nrow(cramer)){
#   for(j in 1:ncol(cramer)){
#     cramer_temp <- CramerV(cramer_col[,i],
#                            cramer_col[,j])
#     cramer[i, j] <- cramer_temp
#   }
# }
# colnames(cramer) <- category_list; rownames(cramer) <- category_list
# hit_plot(cramer, "Cramer's V")
# 
# ## 그룹이 2개인 명목형 변수의 Phi 상관계수
# phi_col <- cbind(train$gender,     train$car,   train$reality,
#                  train$work_phone, train$phone, train$email,
#                  train$iswork,     train$isna)
# phi_list <- c("gender",     "car",   "reality",
#               "work_phone", "phone", "email",
#               "iswork",     "isna")
# phi <- matrix(nrow=length(phi_list),
#               ncol=length(phi_list))
# for(i in 1:nrow(phi)){
#   for(j in 1:ncol(phi)){
#     phi_temp <- Phi(phi_col[,i],
#                     phi_col[,j])
#     phi[i, j] <- phi_temp
#   }
# }
# colnames(phi) <- phi_list; rownames(phi) <- phi_list
# hit_plot(phi, "Phi")
# 
# ## 순서형 변수의 스피어만 상관계수
# spearman_col <- cbind(as.numeric(train$child_num),
#                       as.numeric(train$family_size),
#                       as.numeric(train$fam_without_child),
#                       as.numeric(train$credit)
# )
# colnames(spearman_col) <- c("child_num",        "family_size",
#                             "fam_without_child","credit")
# hit_plot(cor(spearman_col, method="spearman"), "Spearman")
# 
# ## 연속형 변수의 피어슨 상관계수
# pearson_col <- cbind(train$income_total,          train$scale_income_total,
#                      train$scale_DAYS_EMPLOYED,   train$scale_DAYS_UNEMPLOYED,
#                      train$scale_income_per_year, train$scale_begin_month,
#                      train$scale_DAYS_BIRTH)
# colnames(pearson_col) <- c("income_total",          "scale_income_total",
#                            "scale_DAYS_EMPLOYED",   "scale_DAYS_UNEMPLOYED",
#                            "scale_income_per_year", "scale_begin_month",
#                            "scale_DAYS_BIRTH")
# hit_plot(cor(pearson_col, method="pearson"), "Pearson")


### 메모리 정리
rm(pairs_list, n_credit, pearson_col, spearman_col,
   category_list, i, j, box_nu, col, hist_nu, den_nu, hit_plot,
   train_plot, tt, raw_train, raw_test, cramer_temp, phi_list, skew,
   phi_col, phi, phi_temp, cramer_col, cramer, occ_gender)



################################# 변수 선별 ####################################
### 의미가 겹치는 변수 제거
train$DAYS_EMPLOYED    <- NULL; test$DAYS_EMPLOYED    <- NULL
train$DAYS_BIRTH       <- NULL; test$DAYS_BIRTH       <- NULL
train$DAYS_UNEMPLOYED  <- NULL; test$DAYS_UNEMPLOYED  <- NULL
train$begin_month      <- NULL; test$begin_month      <- NULL
train$age              <- NULL; test$age              <- NULL
train$YEARS_EMPLOYED   <- NULL; test$YEARS_EMPLOYED   <- NULL
train$YEARS_UNEMPLOYED <- NULL; test$YEARS_UNEMPLOYED <- NULL


# ## 다항 로지스틱 회귀
# # 다중공선을 보이는 변수 제거
# logistic_train <- train
# logistic_train$family_size           <- NULL
# logistic_train$scale_DAYS_UNEMPLOYED <- NULL
# logistic_train$scale_DAYS_EMPLOYED   <- NULL
# logistic_train$income_total          <- NULL
# logistic_train$iswork                <- NULL
# logistic_train$isna                  <- NULL
# colnames(logistic_train)
# ## 변수중요도
# logistic.fit <- multinom(credit~.,data=logistic_train)
# logistic.imp <- varImp(logistic.fit)
# logistic.imp$variable <- rownames(varImp(logistic.fit))
# ggplot(logistic.imp,
#        aes(x=Overall, y=reorder(variable, Overall))) + geom_col() +
#   theme_bw() + labs(title="Multinom Feature Importance",y="") +
#   theme(plot.title = element_text(hjust = 0.5))
# ## 로지스틱 회귀분석 변수선택 단계적방법
# # logistic_step <- step(logistic.fit, direction = 'both')
# 
# 
# ### 랜덤포레스트
# rf_train <- train
# rf_train$credit <- NULL
# rf.fit <- randomForest(rf_train, train$credit, importance = T)
# varImpPlot(rf.fit, main = "Random Forest Feature Importance")
# 
# 
# ### xgboost
# train.data <- train
# for(i in 1:ncol(train)){
#   train.data[,i] <- as.numeric(train[,i])
# }
# train_idx <- unlist(createDataPartition(train$credit, p=0.7))
# val.label <- train.data$credit[-train_idx]-1
# train.label <- train.data$credit[train_idx]-1
# train.data$credit <- NULL
# val.data <- as.matrix(train.data[-train_idx,])
# train.data <- as.matrix(train.data[train_idx,])
# xgb_train <- xgb.DMatrix(data=train.data,label=train.label)
# xgb_val <- xgb.DMatrix(data=val.data,label=val.label)
# xgb.fit<-xgb.train(
#   objective="multi:softprob",
#   eval_metric="mlogloss",
#   eta = 0.05,
#   num_class=3,
#   data=xgb_train,
#   nthreads=-1,
#   early_stopping_rounds=20,
#   watchlist=list(val1=xgb_train, val2=xgb_val),
#   verbose = T,
#   nrounds = 1000,
#   print_every_n = 50
# )
# xgb.imp <- xgb.importance(colnames(train.data), model = xgb.fit)
# xgb.imp %>%
#   mutate(Importance = ifelse(Gain > 0.04,"High","Low")) %>%
#   ggplot(aes(x=Gain, y=reorder(Feature, Gain), fill=Importance)) +
#   geom_bar(stat="identity") + theme_bw() +
#   labs(title="Xgboost Feature Importance", x="Gain",y="") +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   scale_fill_discrete(name="Feature Importance",
#                       breaks=c("High","Low"),
#                       labels=c("High","Low"))


### 3개의 Data Set 생성
label <- train$credit
## train1 (모든 변수를 고려, 23개)
train1 <- train; test1 <- test
train1$credit <- NULL
colnames(train1)

## train2 (중요하지 않은 변수 제거, 13개)
train2 <- train1                ; test2 <- test1
train2$isna              <- NULL; test2$isna              <- NULL
train2$iswork            <- NULL; test2$iswork            <- NULL
train2$fam_without_child <- NULL; test2$fam_without_child <- NULL
train2$email             <- NULL; test2$email             <- NULL
train2$family_size       <- NULL; test2$family_size       <- NULL
train2$gender            <- NULL; test2$gender            <- NULL
train2$house_type        <- NULL; test2$house_type        <- NULL
train2$reality           <- NULL; test2$reality           <- NULL
train2$work_phone        <- NULL; test2$work_phone        <- NULL
train2$phone             <- NULL; test2$phone             <- NULL
colnames(train2)

## train3 (다중공선성 제거, 가장 중요한 변수만 추출, 10개)
train3 <- train2                    ; test3 <- train2
train3$scale_DAYS_EMPLOYED   <- NULL; test3$scale_DAYS_EMPLOYED   <- NULL
train3$scale_DAYS_UNEMPLOYED <- NULL; test3$scale_DAYS_UNEMPLOYED <- NULL
train3$income_total          <- NULL; test3$income_total          <- NULL
colnames(train3)


### 메모리 정리
rm(logistic_step, logistic_train, logistic.fit, logistic.imp,
   rf_train, rf.fit, train.data, val.data,
   i, m, n, train_idx, train.label, val.label, xgb_train, xgb_val,
   xgb.fit, xgb.imp)



################################ 모델 생성 #####################################
### MultiLogloss 함수 정의
mlogloss <- function(t1, t2) {
  loss_sum <- 0
  for(i in 1:nrow(t2)){
    loss_temp <- 0
    for(j in 1:ncol(t2)){
      if(t1[i,j]==0){
        t1[i,j] <- 0.000001
      }
      loss_temp <- t2[i,j] * log(t1[i,j]) +loss_temp
    }
    loss_sum <- loss_sum +loss_temp
  }
  return(-loss_sum / nrow(t2))
}


### Kfold 교차검증 인덱스 생성
fold_iter <- 5
KFOLD <- createFolds(y=label, k=fold_iter)


### 서포트벡터머신
svm.data_temp <- train3
for(i in 1:ncol(svm.data_temp)) svm.data_temp[,i] <- as.numeric(svm.data_temp[,i])
svm.data <- as.data.frame(scale(svm.data_temp))
## 최적의 파라미터 찾기
svm.sigma = c(0.05, 0.1, 0.2, 0.25)
svm.cost = c(1, 2, 3, 4)
ksigma = length(svm.sigma)
kcost = length(svm.cost)
svm.logloss = list()
for(i in 1:(ksigma*kcost)){
  svm.index <- unlist(KFOLD[(i %% fold_iter)+1])
  svm.train <- svm.data[-svm.index,]
  svm.train$credit <- label[-svm.index]
  # svm.train <- upSample(svm.train, svm.train$credit)
  # svm.train$Class <- NULL
  svm.val.data <- svm.data[svm.index,]
  svm.val.label <- label[svm.index]
  svm.fit <- ksvm(credit ~ ., data = svm.train, type = "C-bsvc",
                  kernel = "rbfdot", 
                  kpar = list(sigma = svm.sigma[ksigma - (i %% ksigma)]),
                  C = svm.cost[((i-1) %/% ksigma)+1], 
                  prob.model = TRUE)
  svm.label.one <- one_hot(data.table(as.factor(svm.val.label)))
  svm.label.one <- as.data.frame(svm.label.one)
  svm.val.pred <- predict(svm.fit, svm.val.data, type= "prob")
  svm.val.pred <- as.data.frame(svm.val.pred)
  mll_temp <- 0
  mll_temp <- mlogloss(svm.val.pred, svm.label.one)
  svm.logloss <- append(svm.logloss, mll_temp)
  print(paste("SVM Fold",i," : ", mll_temp, sep=" "))
  if(min(unlist(svm.logloss)) >= mll_temp){
    best_sigma <- svm.sigma[ksigma - (i %% ksigma)]
    best_cost  <- svm.cost[((i-1) %/% ksigma)+1]
  }
}
svm.logloss
## 모델 훈련
svm.index <- unlist(KFOLD[fold_iter])
svm.train <- svm.data[-svm.index,]
svm.train$credit <- label[-svm.index]
# svm.train <- upSample(svm.train, svm.train$credit)
# svm.train$Class <- NULL
svm.val.data <- svm.data[svm.index,]
svm.val.label <- label[svm.index]
svm.fit <- ksvm(credit ~ ., data = svm.train, type = "C-bsvc",
                kernel = "rbfdot", kpar = list(sigma = best_sigma),
                C = best_cost, prob.model = TRUE)
summary(svm.fit)
## 예측
svm.pred <- predict(svm.fit, svm.val.data)
svm.table <- table(predict = svm.pred, real = svm.val.label)
svm.table
## 모델 성능 평가
# Confusion Matrix
svm.mat <- confusionMatrix(svm.pred, svm.val.label)
svm.mat
svm.accuracy <- (svm.table[1,1]+svm.table[2,2]+svm.table[3,3]) / sum(svm.table)
svm.precision <- mean(as.data.frame(svm.mat$byClass)['Precision'][,1])
svm.recall <- mean(as.data.frame(svm.mat$byClass)['Recall'][,1])
svm.f1 <- 2*(svm.precision*svm.recall / (svm.precision+svm.recall))
svm.FL <- svm.table[2,1]+svm.table[3,1]+svm.table[3,2]
svm.FH <- svm.table[1,2]+svm.table[1,3]+svm.table[2,3]
svm.Recovery_score <- svm.FL / (svm.FL + svm.FH)
# mlogloss
svm.label.one <- one_hot(data.table(as.factor(svm.val.label)))
svm.label.one <- as.data.frame(svm.label.one)
svm.val.pred <- predict(svm.fit, svm.val.data, type="prob")
svm.val.pred <- as.data.frame(svm.val.pred)
svm.mll <- mlogloss(svm.val.pred, svm.label.one)
svm.perform <- c(svm.accuracy, svm.precision, svm.recall, svm.f1, 1.5-svm.mll, svm.Recovery_score)
svm.perform


### 랜덤포레스트
rf.data <- train1
## 최적의 파라미터 찾기
rf.tree = c(200, 300, 400, 500)
rf.mtry = c(4, 5, 10, 20)
ktree = length(rf.tree)
kmtry = length(rf.mtry)
rf.logloss = list()
for(i in 1:(ktree*kmtry)){
  rf.index <- unlist(KFOLD[(i %% fold_iter)+1])
  rf.train <- rf.data[-rf.index,]
  rf.train$credit <- label[-rf.index]
  # rf.train <- upSample(rf.train, rf.train$credit)
  # rf.train$Class <- NULL
  rf.val.data <- rf.data[rf.index,]
  rf.val.label <- label[rf.index]
  rf.fit = randomForest(credit ~ . , data = rf.train,
                        ntree = rf.tree[ktree - (i %% ktree)],
                        mtry = rf.mtry[((i-1) %/% ktree)+1],
                        nodesize = 10)
  rf.label.one <- one_hot(data.table(as.factor(rf.val.label)))
  rf.label.one <- as.data.frame(rf.label.one)
  rf.val.pred <- predict(rf.fit, rf.val.data, type= "prob")
  rf.val.pred <- as.data.frame(rf.val.pred)
  mll_temp <- 0
  mll_temp <- mlogloss(rf.val.pred, rf.label.one)
  rf.logloss <- append(rf.logloss, mll_temp)
  print(paste("RF Fold",i," : ", mll_temp, sep=" "))
  if(min(unlist(rf.logloss)) >= mll_temp){
    best_ntree <- rf.tree[ktree - (i %% ktree)]
    best_mtry  <- rf.mtry[((i-1) %/% ktree)+1]
  }
}
rf.logloss
## 모델훈련
rf.index <- unlist(KFOLD[fold_iter])
rf.train <- rf.data[-rf.index,]
rf.train$credit <- label[-rf.index]
# rf.train <- upSample(rf.train, rf.train$credit)
# rf.train$Class <- NULL
rf.val.data <- rf.data[rf.index,]
rf.val.label <- label[rf.index]
rf.fit = randomForest(credit ~ . , data = rf.train,
                      ntree = best_ntree,
                      mtry = best_mtry,
                      nodesize = 10)
summary(rf.fit)
## 예측
rf.pred <- predict(rf.fit, rf.val.data)
rf.table <- table(predict = rf.pred, real = rf.val.label)
rf.table
## 모델 성능 평가
# Confusion Matrix
rf.mat <- confusionMatrix(rf.pred, rf.val.label)
rf.mat
rf.accuracy <- (rf.table[1,1]+rf.table[2,2]+rf.table[3,3]) / sum(rf.table)
rf.precision <- mean(as.data.frame(rf.mat$byClass)['Precision'][,1])
rf.recall <- mean(as.data.frame(rf.mat$byClass)['Recall'][,1])
rf.f1 <- 2*(rf.precision*rf.recall / (rf.precision+rf.recall))
rf.FL <- rf.table[2,1]+rf.table[3,1]+rf.table[3,2]
rf.FH <- rf.table[1,2]+rf.table[1,3]+rf.table[2,3]
rf.Recovery_score <- rf.FL / (rf.FL + rf.FH)
# mlogloss
rf.label.one <- one_hot(data.table(as.factor(rf.val.label)))
rf.label.one <- as.data.frame(rf.label.one)
rf.val.pred <- predict(rf.fit, rf.val.data, type= "prob")
rf.val.pred <- as.data.frame(rf.val.pred)
rf.mll <- mlogloss(rf.val.pred, rf.label.one)
rf.perform <- c(rf.accuracy, rf.precision, rf.recall, rf.f1, 1.5-rf.mll, rf.Recovery_score)
rf.perform


### Xgboost
xgb.data <- train1
for(i in 1:ncol(train1)) xgb.data[,i] <- as.numeric(xgb.data[,i])
## 최적의 파라미터 찾기
xgb.gamma = c(1, 2, 3, 4)
xgb.col = c(0.3, 0.5, 0.75, 1)
kgamma = length(xgb.gamma)
kcol = length(xgb.col)
xgb.logloss = list()
for(i in 1:(kgamma*kcol)) {
  params <- list(
    booster="gbtree",
    eta=0.08, 
    max_depth=7,
    gamma=xgb.gamma[kgamma - (i %% kgamma)],
    subsample=0.75,
    colsample_bytree=xgb.col[((i-1) %/% kgamma)+1],
    eval_metric="mlogloss", 
    num_class=3
  )
  xgb.index <- unlist(KFOLD[(i %% fold_iter)+1])
  xgb.train.data <- as.matrix(xgb.data[-xgb.index,])
  xgb.train.label <- as.integer(label[-xgb.index])-1
  xgb.val.data <- as.matrix(xgb.data[xgb.index,])
  xgb.val.label <- as.integer(label[xgb.index])-1
  xgb.train <- xgb.DMatrix(data=xgb.train.data,label=xgb.train.label)
  xgb.val <- xgb.DMatrix(data=xgb.val.data,label=xgb.val.label)
  xgb.fit<-xgb.train(
    params=params,
    objective="multi:softprob",
    data=xgb.train,
    nrounds=2000,
    nthreads=1,
    early_stopping_rounds=100,
    watchlist=list(val1=xgb.train,val2=xgb.val),
    verbose = F
  )
  xgb.label.one <- one_hot(data.table(as.factor(xgb.val.label)))
  xgb.label.one <- as.data.frame(xgb.label.one)
  xgb.val.pred <- predict(xgb.fit,xgb.val.data,reshape=T)
  xgb.val.pred <- as.data.frame(xgb.val.pred)
  mll_temp <- 0
  mll_temp <- mlogloss(xgb.val.pred, xgb.label.one)
  xgb.logloss <- append(xgb.logloss, mll_temp)
  print(paste("XGB Fold",i," : ", mll_temp, sep=" "))
  if(min(unlist(xgb.logloss)) >= mll_temp){
    best_gamma <- xgb.gamma[kgamma - (i %% kgamma)]
    best_col  <- xgb.col[((i-1) %/% kgamma)+1]
  }
}
xgb.logloss
## 모델훈련
params <- list(
  booster="gbtree",
  eta=0.08, 
  max_depth=7,
  gamma=best_gamma,
  subsample=0.75,
  colsample_bytree=best_col,
  eval_metric="mlogloss", 
  num_class=3
)
xgb.index <- unlist(KFOLD[fold_iter])
xgb.train.data <- as.matrix(xgb.data[-xgb.index,])
xgb.train.label <- as.integer(label[-xgb.index])-1
xgb.val.data <- as.matrix(xgb.data[xgb.index,])
xgb.val.label <- as.integer(label[xgb.index])-1
xgb.train <- xgb.DMatrix(data=xgb.train.data,label=xgb.train.label)
xgb.val <- xgb.DMatrix(data=xgb.val.data,label=xgb.val.label)
xgb.fit<-xgb.train(
  params=params,
  objective="multi:softmax",
  data=xgb.train,
  nrounds=2000,
  nthreads=1,
  early_stopping_rounds=100,
  watchlist=list(val1=xgb.train,val2=xgb.val),
  verbose = T,
  print_every_n = 100
)
summary(xgb.fit)
## 예측
xgb.pred <- predict(xgb.fit, xgb.val.data, type="class")
xgb.table <- table(predict = xgb.pred, real = xgb.val.label)
xgb.table
## 모델 성능 평가
# Confusion Matrix
xgb.mat <- confusionMatrix(as.factor(xgb.pred), as.factor(xgb.val.label))
xgb.mat
xgb.accuracy <- (xgb.table[1,1]+xgb.table[2,2]+xgb.table[3,3]) / sum(xgb.table)
xgb.precision <- mean(as.data.frame(xgb.mat$byClass)['Precision'][,1])
xgb.recall <- mean(as.data.frame(xgb.mat$byClass)['Recall'][,1])
xgb.f1 <- 2*(xgb.precision*xgb.recall / (xgb.precision+xgb.recall))
xgb.FL <- xgb.table[2,1]+xgb.table[3,1]+xgb.table[3,2]
xgb.FH <- xgb.table[1,2]+xgb.table[1,3]+xgb.table[2,3]
xgb.Recovery_score <- xgb.FL / (xgb.FL + xgb.FH)
# mlogloss
xgb.mll <- min(unlist(xgb.logloss))
xgb.perform <- c(xgb.accuracy, xgb.precision, xgb.recall, xgb.f1, 1.5-xgb.mll,xgb.Recovery_score)
xgb.perform


### ANN
ann.data_temp <- train1
for(i in 1:ncol(ann.data_temp)) ann.data_temp[,i] <- as.numeric(ann.data_temp[,i])
ann.data <- as.data.frame(scale(ann.data_temp))
## 최적의 파라미터 찾기
ann.size = c(8, 16, 24, 32)
ann.decay = c(0.0001, 0.0003, 0.0005, 0.001)
ksize = length(ann.size)
kdecay = length(ann.decay)
ann.logloss = list()
for(i in 1:(ksize*kdecay)) {
  ann.data$credit <- label
  ann.index <- unlist(KFOLD[(i %% fold_iter)+1])
  ann.train <- ann.data[-ann.index,]
  ann.train$credit <- label[-ann.index]
  ann.train <- upSample(ann.train, ann.train$credit)
  ann.train$Class <- NULL
  ann.val.data <- ann.data[ann.index,]
  ann.val.label <- label[ann.index]
  ann.fit <- nnet(credit ~ ., 
                  data = ann.train, 
                  size = ann.size[ksize - (i %% ksize)], 
                  decay = ann.decay[((i-1) %/% ksize)+1], 
                  maxit = 1000)
  ann.label.one <- one_hot(data.table(as.factor(ann.val.label)))
  ann.label.one <- as.data.frame(ann.label.one)
  ann.val.pred <- predict(ann.fit, ann.val.data, type= "raw")
  ann.val.pred <- as.data.frame(ann.val.pred)
  mll_temp <- 0
  mll_temp <- mlogloss(ann.val.pred, ann.label.one)
  ann.logloss <- append(ann.logloss, mll_temp)
  print(paste("ANN Fold",i," : ", mll_temp, sep=" "))
  if(min(unlist(ann.logloss)) >= mll_temp){
    best_size <- ann.size[ksize - (i %% ksize)]
    best_decay  <- ann.decay[((i-1) %/% ksize)+1]
  }
}
ann.logloss
## 모델 훈련
ann.data$credit <- label
ann.index <- unlist(KFOLD[fold_iter])
ann.train <- ann.data[-ann.index,]
ann.train$credit <- label[-ann.index]
ann.train <- upSample(ann.train, ann.train$credit)
ann.train$Class <- NULL
ann.val.data <- ann.data[ann.index,]
ann.val.label <- label[ann.index]
ann.fit <- nnet(credit ~ ., 
                data = ann.train, 
                size = best_size, 
                decay = best_decay, 
                maxit = 1000)
summary(ann.fit)
## 예측
ann.pred <- predict(ann.fit, ann.val.data, type = "class")
ann.table <- table(predict = ann.pred, real = ann.val.label)
ann.table
## 모델 성능 평가
# Confusion Matrix
ann.mat <- confusionMatrix(as.factor(ann.pred), as.factor(ann.val.label))
ann.mat
ann.accuracy <- (ann.table[1,1]+ann.table[2,2]+ann.table[3,3]) / sum(ann.table)
ann.precision <- mean(as.data.frame(ann.mat$byClass)['Precision'][,1])
ann.recall <- mean(as.data.frame(ann.mat$byClass)['Recall'][,1])
ann.f1 <- 2*(ann.precision*ann.recall / (ann.precision+ann.recall))
ann.FL <- ann.table[2,1]+ann.table[3,1]+ann.table[3,2]
ann.FH <- ann.table[1,2]+ann.table[1,3]+ann.table[2,3]
ann.Recovery_score <- ann.FL / (ann.FL + ann.FH)
# mlogloss
ann.label.one <- one_hot(data.table(as.factor(ann.val.label)))
ann.label.one <- as.data.frame(ann.label.one)
ann.val.pred <- predict(ann.fit, ann.val.data, type= "raw")
ann.val.pred <- as.data.frame(ann.val.pred)
ann.mll <- mlogloss(ann.val.pred, ann.label.one)
ann.perform <- c(ann.accuracy, ann.precision, ann.recall, ann.f1, 1.5-ann.mll, ann.Recovery_score)
ann.perform


## Catboost
cat.data <- train1
for(i in 1:ncol(train1)) cat.data[,i] <- as.numeric(train1[,i])
cat.index <- unlist(KFOLD[fold_iter])
cat.train.data <- as.matrix(cat.data[-cat.index,])
cat.train.label <- as.numeric(label[-cat.index])-1
cat.val.data <- as.matrix(cat.data[cat.index,])
cat.val.label <- as.numeric(label[cat.index])-1
cat.train <- catboost.load_pool(
  data = cat.train.data,
  label = cat.train.label,
  cat_features = 0:22)
cat.val <- catboost.load_pool(
  data = cat.val.data, 
  label = cat.val.label,
  cat_features = 0:22)
## 모델 훈련
params <- list(
  learning_rate = 0.1,
  iterations = 500,
  loss_function = "MultiClass",
  metric_period = 100)
cat.fit <- catboost.train(learn_pool=cat.train, test_pool=cat.val, params=params)
summary(cat.fit)
## 예측
cat.pred <- catboost.predict(cat.fit, cat.val, prediction_type = "Class")
cat.table <- table(cat.pred, cat.val.label)
cat.table
## 모델 성능 평가
# Confusion Matrix
cat.mat <- confusionMatrix(as.factor(cat.pred), as.factor(cat.val.label))
cat.mat
cat.accuracy <- (cat.table[1,1]+cat.table[2,2]+cat.table[3,3]) / sum(cat.table)
cat.precision <- mean(as.data.frame(cat.mat$byClass)['Precision'][,1])
cat.recall <- mean(as.data.frame(cat.mat$byClass)['Recall'][,1])
cat.f1 <- 2*(cat.precision*cat.recall / (cat.precision+cat.recall))
cat.FL <- cat.table[2,1]+cat.table[3,1]+cat.table[3,2]
cat.FH <- cat.table[1,2]+cat.table[1,3]+cat.table[2,3]
cat.Recovery_score <- cat.FL / (cat.FL + cat.FH)
# mlogloss
cat.label.one <- one_hot(data.table(as.factor(cat.val.label)))
cat.label.one <- as.data.frame(cat.label.one)
cat.val.pred <- catboost.predict(cat.fit, cat.val, prediction_type = "Probability")
cat.val.pred <- as.data.frame(cat.val.pred)
cat.mll <- mlogloss(cat.val.pred, cat.label.one)
cat.perform <- c(cat.accuracy, cat.precision, cat.recall, cat.f1, 1.5-cat.mll, cat.Recovery_score)
cat.perform

## 파라미터 튜닝 그래프
tune_svm <- data.frame(mlogloss = unlist(svm.logloss))
tune_rf  <- data.frame(mlogloss = unlist(rf.logloss))
tune_xgb <- data.frame(mlogloss = unlist(xgb.logloss))
tune_ann <- data.frame(mlogloss = unlist(ann.logloss))
for(i in 1:(ksigma*kcost)){
  tune_svm$sigma[i] <- svm.sigma[ksigma - (i %% ksigma)]
  tune_svm$cost[i]  <- svm.cost[((i-1) %/% ksigma)+1]
}
for(i in 1:(ktree*kmtry)){
  tune_rf$ntree[i] <- rf.tree[ktree - (i %% ktree)]
  tune_rf$mtry[i]  <- rf.mtry[((i-1) %/% ktree)+1]
}
for(i in 1:(kgamma*kcol)){
  tune_xgb$gamma[i]  <- xgb.gamma[kgamma - (i %% kgamma)]
  tune_xgb$subcol[i] <- xgb.col[((i-1) %/% kgamma)+1]
}
for(i in 1:(ksize*kdecay)){
  tune_ann$node_size[i] <- ann.size[ksize - (i %% ksize)]
  tune_ann$decay[i]     <- ann.decay[((i-1) %/% ksize)+1]
}
param_svm <- ggplot(tune_svm, aes(x=as.factor(sigma), y=as.factor(cost), 
                                  size=mlogloss, colour=mlogloss)) +
  geom_point(shape=18) +
  scale_color_continuous(low = "blue", high = "grey") +
  labs(title="SVM Parameter", x="sigma",y="cost") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 8, face = "bold"),
        legend.position="right") +
  scale_size(range = c(5,15)) +
  guides(color= guide_legend(), size=guide_legend())

param_rf <- ggplot(tune_rf, aes(x=as.factor(ntree), y=as.factor(mtry), 
                                size=mlogloss, colour=mlogloss)) +
  geom_point(shape=18) +
  scale_color_continuous(low = "purple", high = "grey") +
  labs(title="RF Parameter", x="ntree",y="mtry") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 8, face = "bold"),
        legend.position="right") +
  scale_size(range = c(5,15)) +
  guides(color= guide_legend(), size=guide_legend())

param_xgb <- ggplot(tune_xgb, aes(x=as.factor(gamma), y=as.factor(subcol), 
                                  size=mlogloss, colour=mlogloss)) +
  geom_point(shape=18) +
  scale_color_continuous(low = "red", high = "grey") +
  labs(title="XGB Parameter", x="gamma",y="subcol") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 8, face = "bold"),
        legend.position="right") +
  scale_size(range = c(5,15)) +
  guides(color= guide_legend(), size=guide_legend())

param_ann  <- ggplot(tune_ann, aes(x=as.factor(node_size), y=as.factor(decay), 
                                   size=mlogloss, colour=mlogloss)) +
  geom_point(shape=18) +
  scale_color_continuous(low = "green", high = "grey") +
  labs(title="ANN Parameter", x="node_size",y="decay") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 8, face = "bold"),
        legend.position="right") +
  scale_size(range = c(5,15)) +
  guides(color= guide_legend(), size=guide_legend())

grid.arrange(param_svm, param_rf, param_xgb, param_ann,
             nrow=2, ncol=2)



################################# 모델 비교 ####################################
## ROC 커브 생성 함수 정의
classes <- levels(label)
roc_color <- c("#F8766D","#00BA38","#619CFF")
roccurve <- function(val.pred,val.label, str){
  auc_list <- list()
  for (i in 1:3){
    true_values <- ifelse(val.label==classes[i],1,0)
    pred <- prediction(val.pred[,i],true_values)
    perf <- performance(pred, "tpr", "fpr")
    if (i==1){
      plot(perf,main=str, col=roc_color[i]) 
    }
    else{
      plot(perf,main=str, col=roc_color[i], add=TRUE) 
    }
    legend("right", c("credit 0", "credit 1", "credit 2"), 
           col=roc_color,
           pch = 1)
    auc.perf <- performance(pred, measure = "auc")
    auc_list <- append(auc_list, auc.perf@y.values)
  }
  print(mean(unlist(auc_list)))
}

## ROC 커브
par(mfrow=c(2,2))
roccurve(svm.val.pred, as.factor(svm.val.label), "SVM ROC Curve")
roccurve(rf.val.pred, as.factor(rf.val.label), "RF ROC Curve")
roccurve(xgb.val.pred, as.factor(xgb.val.label), "XGBoost ROC Curve")
roccurve(ann.val.pred, as.factor(ann.val.label), "ANN ROC Curve")
par(mfrow=c(1,1))
roccurve(cat.val.pred, as.factor(cat.val.label), "Catboost ROC Curve")


## 방사형 차트
colors_border=c( rgb(0.8,0.3,0.3,0.8), rgb(0.3,0.8,0.3,0.8) , 
                 rgb(0.3,0.3,0.8,0.8))
colors_in=c( rgb(0.8,0.3,0.3,0.3), rgb(0.3,0.8,0.3,0.2) , 
             rgb(0.3,0.3,0.8,0.2))
par(mfrow=c(1,1))
radarchartdata <- data.frame(Accuracy=c(0,0),
                             Precision=c(0,0),
                             Recall=c(0,0),
                             F1_score=c(0,0),
                             Mll_score=c(0,0),
                             Recovery_score=c(0,0))
radarchartdata[3,] <- svm.perform
radarchartdata[2,] <- xgb.perform
radarchartdata[1,] <- cat.perform

radarchartdata=rbind(rep(1,6) , rep(0.4,6) , radarchartdata)
radarchart( radarchartdata  , axistype=1 ,
            pcol=colors_border , pfcol=colors_in,
            plwd=4 , plty=1,
            cglcol="grey", cglty=1, axislabcol="grey",
            caxislabels=seq(0.4,1,0.15), cglwd=0.8,
            vlcex=0.8, title="Model Performance")
legend(x=0.9, y=0.75, 
       legend = c("Catboost", "XGboost", "SVM"), 
       bty = "n", pch=20 ,
       col=colors_border , text.col = "grey", cex=1, pt.cex=3)

svm.perform
rf.perform
xgb.perform
ann.perform
cat.perform


for(i in 1:ncol(test)) test[,i] <- as.numeric(test[,i])
test <- as.data.frame(scale(test))
final_pred <- predict(cat.fit, test, type= "probabilities")
write.csv(final_pred, file = "submission_final1.csv")