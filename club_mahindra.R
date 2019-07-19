
library("tidyverse")
library("ROCR")
library("car")
library("corrplot")
library("caret")
library("caTools")

setwd("C:/Users/SwaroopKM99/Documents/R/R_working_directory")

club = read.csv("C:/Users/SwaroopKM99/Documents/R/R_working_directory/Mahindra club/train.csv", header = T,
                sep = ",")
# To see the top 6 value of the file
head(club)

# to see no. of missing values in the file in total
sum(is.na(club))

# to see the dimension of the file, no. of rows and no. of columns
dim(club)

# it gives the structure of the file
str(club)

# to see the description of the file
summary(club)

# treating the missing value in variable seasin_holidayed_code & state_code_residnce 
getmode = function(v) {
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
result1 = getmode(club$season_holidayed_code)
print(result1)
club$season_holidayed_code[is.na(club$season_holidayed_code)] = result1

getmode = function(v) {
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
result2 = getmode(club$state_code_residence)
print(result2)
club$state_code_residence[is.na(club$state_code_residence)] = result2

# Creating dummy variables 

club$member_age_buckets_A = ifelse(club$member_age_buckets == "A", 1 ,0)
club$member_age_buckets_B = ifelse(club$member_age_buckets == "B", 1 ,0)
club$member_age_buckets_C = ifelse(club$member_age_buckets == "C", 1 ,0)
club$member_age_buckets_D = ifelse(club$member_age_buckets == "D", 1 ,0)
club$member_age_buckets_E = ifelse(club$member_age_buckets == "E", 1 ,0)
club$member_age_buckets_F = ifelse(club$member_age_buckets == "F", 1 ,0)
club$member_age_buckets_G = ifelse(club$member_age_buckets == "G", 1 ,0)
club$member_age_buckets_H = ifelse(club$member_age_buckets == "H", 1 ,0)
club$member_age_buckets_I = ifelse(club$member_age_buckets == "I", 1 ,0)
club$member_age_buckets_J = ifelse(club$member_age_buckets == "J", 1 ,0)

club$reservationstatusid_code_A = ifelse(club$reservationstatusid_code == "A", 1, 0)
club$reservationstatusid_code_B = ifelse(club$reservationstatusid_code == "B", 1, 0)
club$reservationstatusid_code_C = ifelse(club$reservationstatusid_code == "C", 1, 0)
club$reservationstatusid_code_D = ifelse(club$reservationstatusid_code == "D", 1, 0)

club$cluster_code_A = ifelse(club$cluster_code == "A", 1, 0)
club$cluster_code_B = ifelse(club$cluster_code == "B", 1, 0)
club$cluster_code_C = ifelse(club$cluster_code == "C", 1, 0)
club$cluster_code_D = ifelse(club$cluster_code == "D", 1, 0)
club$cluster_code_E = ifelse(club$cluster_code == "E", 1, 0)
club$cluster_code_F = ifelse(club$cluster_code == "F", 1, 0)

# Correlation between variable, first creating subset of numeric values

M = subset(club, select = -c(reservation_id,member_age_buckets,memberid,cluster_code,reservationstatusid_code,resort_id))
cr = cor(M)
corrplot(cr, type = "lower", method = "number")

# split the train data in training and testing

split = sample.split(club$amount_spent_per_room_night_scaled,SplitRatio = 0.7)
train_club = subset(club,split=="TRUE")
test_club = subset(club,split=="FALSE")
dim(train_club)
dim(test_club)

# As the file has three variables like reservation_id, memberid, resort_id which are factors so causing error in the model.. therefore i will remove the variables build the model and add it at the end

train = train_club
test = test_club

train$reservation_id = NULL
train$memberid = NULL
train$resort_id = NULL
train$member_age_buckets = NULL
train$cluster_code = NULL
train$reservationstatusid_code = NULL

colnames(train)
str(train)

# making a base model to understand the data and the model to be design

model = lm(amount_spent_per_room_night_scaled ~ . , data = train)
summary(model)

# creating model after removing the multicolinearity and doing forward stepwise regression

model1 = lm(amount_spent_per_room_night_scaled ~ booking_date , data = train)
summary(model1)

model2 = lm(amount_spent_per_room_night_scaled ~ booking_date+Actual_stay_booked , data = train)
summary(model2)

model3 = lm(amount_spent_per_room_night_scaled ~ booking_date+Actual_stay_booked+
            channel_code, data = train)
summary(model3)

model4 = lm(amount_spent_per_room_night_scaled ~ booking_date+Actual_stay_booked+
            channel_code+main_product_code, data = train)
summary(model4)

model5 = lm(amount_spent_per_room_night_scaled ~ booking_date+Actual_stay_booked+
            channel_code+main_product_code+numberofadults, data = train)
summary(model5)

model6 = lm(amount_spent_per_room_night_scaled ~ booking_date+Actual_stay_booked+
            channel_code+main_product_code+numberofadults+numberofchildren, data = train)
summary(model6)

model7 = lm(amount_spent_per_room_night_scaled ~ booking_date+Actual_stay_booked+
            channel_code+main_product_code+numberofadults+numberofchildren+persontravellingid,
            data = train)
summary(model7)

model8 = lm(amount_spent_per_room_night_scaled ~ booking_date+Actual_stay_booked+
            channel_code+main_product_code+numberofadults+numberofchildren+persontravellingid+
            resort_region_code,data = train)
summary(model8)

model9 = lm(amount_spent_per_room_night_scaled ~ booking_date+Actual_stay_booked+
            channel_code+main_product_code+numberofadults+numberofchildren+persontravellingid+
            resort_region_code+resort_type_code,data = train)
summary(model9)

model10 = lm(amount_spent_per_room_night_scaled ~ booking_date+Actual_stay_booked+
            channel_code+main_product_code+numberofadults+numberofchildren+persontravellingid+
            resort_region_code+resort_type_code+room_type_booked_code,data = train)
summary(model10)

model11 = lm(amount_spent_per_room_night_scaled ~ booking_date+Actual_stay_booked+
            channel_code+main_product_code+numberofadults+numberofchildren+persontravellingid+
            resort_region_code+resort_type_code+room_type_booked_code+roomnights,data = train)
summary(model11)

model12 = lm(amount_spent_per_room_night_scaled ~ booking_date+Actual_stay_booked+
            channel_code+main_product_code+numberofadults+numberofchildren+persontravellingid+
            resort_region_code+resort_type_code+room_type_booked_code+roomnights+
            season_holidayed_code,data = train)
summary(model12)

model13 = lm(amount_spent_per_room_night_scaled ~ booking_date+Actual_stay_booked+
               channel_code+main_product_code+numberofadults+numberofchildren+persontravellingid+
               resort_region_code+resort_type_code+room_type_booked_code+roomnights+
               season_holidayed_code+state_code_residence,data = train)
summary(model13)

model14 = lm(amount_spent_per_room_night_scaled ~ booking_date+Actual_stay_booked+
               channel_code+main_product_code+numberofadults+numberofchildren+persontravellingid+
               resort_region_code+resort_type_code+room_type_booked_code+roomnights+
               season_holidayed_code+state_code_residence+state_code_resort,data = train)
summary(model14)

model15 = lm(amount_spent_per_room_night_scaled ~ booking_date+Actual_stay_booked+
               channel_code+main_product_code+numberofadults+numberofchildren+persontravellingid+
               resort_region_code+resort_type_code+room_type_booked_code+roomnights+
               season_holidayed_code+state_code_residence+state_code_resort+
               total_pax,data = train)
summary(model15)

model16 = lm(amount_spent_per_room_night_scaled ~ booking_date+Actual_stay_booked+
               channel_code+main_product_code+numberofadults+numberofchildren+persontravellingid+
               resort_region_code+resort_type_code+room_type_booked_code+roomnights+
               season_holidayed_code+state_code_residence+state_code_resort+
               total_pax+booking_type_code,data = train)
summary(model16)

model17 = lm(amount_spent_per_room_night_scaled ~ booking_date+Actual_stay_booked+
               channel_code+main_product_code+numberofadults+numberofchildren+persontravellingid+
               resort_region_code+resort_type_code+room_type_booked_code+roomnights+
               season_holidayed_code+state_code_residence+state_code_resort+
               total_pax+booking_type_code+member_age_buckets_A,data = train)
summary(model17)

model18 = lm(amount_spent_per_room_night_scaled ~ booking_date+Actual_stay_booked+
               channel_code+main_product_code+numberofadults+numberofchildren+persontravellingid+
               resort_region_code+resort_type_code+room_type_booked_code+roomnights+
               season_holidayed_code+state_code_residence+state_code_resort+
               total_pax+booking_type_code+member_age_buckets_A+member_age_buckets_B,data = train)
summary(model18)

model19 = lm(amount_spent_per_room_night_scaled ~ booking_date+Actual_stay_booked+
               channel_code+main_product_code+numberofadults+numberofchildren+persontravellingid+
               resort_region_code+resort_type_code+room_type_booked_code+roomnights+
               season_holidayed_code+state_code_residence+state_code_resort+
               total_pax+booking_type_code+member_age_buckets_A+member_age_buckets_B+
               member_age_buckets_C,data = train)
summary(model19)

model20 = lm(amount_spent_per_room_night_scaled ~ booking_date+Actual_stay_booked+
               channel_code+main_product_code+numberofadults+numberofchildren+persontravellingid+
               resort_region_code+resort_type_code+room_type_booked_code+roomnights+
               season_holidayed_code+state_code_residence+state_code_resort+
               total_pax+booking_type_code+member_age_buckets_A+member_age_buckets_B+
               member_age_buckets_D,data = train)
summary(model20)

model21 = lm(amount_spent_per_room_night_scaled ~ booking_date+Actual_stay_booked+
               channel_code+main_product_code+numberofadults+numberofchildren+persontravellingid+
               resort_region_code+resort_type_code+room_type_booked_code+roomnights+
               season_holidayed_code+state_code_residence+state_code_resort+
               total_pax+booking_type_code+member_age_buckets_A+member_age_buckets_B+
               member_age_buckets_D+member_age_buckets_E,data = train)
summary(model21)

model22 = lm(amount_spent_per_room_night_scaled ~ booking_date+Actual_stay_booked+
               channel_code+main_product_code+numberofadults+numberofchildren+persontravellingid+
               resort_region_code+resort_type_code+room_type_booked_code+roomnights+
               season_holidayed_code+state_code_residence+state_code_resort+
               total_pax+booking_type_code+member_age_buckets_A+member_age_buckets_B+
               member_age_buckets_D+member_age_buckets_E+member_age_buckets_F,data = train)
summary(model22)

model23 = lm(amount_spent_per_room_night_scaled ~ booking_date+Actual_stay_booked+
               channel_code+main_product_code+numberofadults+numberofchildren+persontravellingid+
               resort_region_code+resort_type_code+room_type_booked_code+roomnights+
               season_holidayed_code+state_code_residence+state_code_resort+
               total_pax+booking_type_code+member_age_buckets_A+member_age_buckets_B+
               member_age_buckets_D+member_age_buckets_E+member_age_buckets_F+member_age_buckets_H
               ,data = train)
summary(model23)

model24 = lm(amount_spent_per_room_night_scaled ~ booking_date+Actual_stay_booked+
               channel_code+main_product_code+numberofadults+numberofchildren+persontravellingid+
               resort_region_code+resort_type_code+room_type_booked_code+roomnights+
               season_holidayed_code+state_code_residence+state_code_resort+
               total_pax+booking_type_code+member_age_buckets_A+member_age_buckets_B+
               member_age_buckets_D+member_age_buckets_E+member_age_buckets_F+member_age_buckets_H+
               member_age_buckets_I,data = train)
summary(model24)

model25 = lm(amount_spent_per_room_night_scaled ~ booking_date+Actual_stay_booked+
               channel_code+main_product_code+numberofadults+numberofchildren+persontravellingid+
               resort_region_code+resort_type_code+room_type_booked_code+roomnights+
               season_holidayed_code+state_code_residence+state_code_resort+
               total_pax+booking_type_code+member_age_buckets_A+member_age_buckets_B+
               member_age_buckets_D+member_age_buckets_E+member_age_buckets_F+member_age_buckets_H+
               member_age_buckets_I+reservationstatusid_code_A,data = train)
summary(model25)

model26 = lm(amount_spent_per_room_night_scaled ~ booking_date+Actual_stay_booked+
               channel_code+main_product_code+numberofadults+numberofchildren+persontravellingid+
               resort_region_code+resort_type_code+room_type_booked_code+roomnights+
               season_holidayed_code+state_code_residence+state_code_resort+
               total_pax+booking_type_code+member_age_buckets_A+member_age_buckets_B+
               member_age_buckets_D+member_age_buckets_E+member_age_buckets_F+member_age_buckets_H+
               member_age_buckets_I+reservationstatusid_code_B,data = train)
summary(model26)

model27 = lm(amount_spent_per_room_night_scaled ~ booking_date+Actual_stay_booked+
               channel_code+main_product_code+numberofadults+numberofchildren+persontravellingid+
               resort_region_code+resort_type_code+room_type_booked_code+roomnights+
               season_holidayed_code+state_code_residence+state_code_resort+
               total_pax+booking_type_code+member_age_buckets_A+member_age_buckets_B+
               member_age_buckets_D+member_age_buckets_E+member_age_buckets_F+member_age_buckets_H+
               member_age_buckets_I+reservationstatusid_code_B+cluster_code_A,data = train)
summary(model27)

model28 = lm(amount_spent_per_room_night_scaled ~ booking_date+Actual_stay_booked+
               channel_code+main_product_code+numberofadults+numberofchildren+persontravellingid+
               resort_region_code+resort_type_code+room_type_booked_code+roomnights+
               season_holidayed_code+state_code_residence+state_code_resort+
               total_pax+booking_type_code+member_age_buckets_A+member_age_buckets_B+
               member_age_buckets_D+member_age_buckets_E+member_age_buckets_F+member_age_buckets_H+
               member_age_buckets_I+reservationstatusid_code_B+cluster_code_A,data = train)
summary(model28)

model29 = lm(amount_spent_per_room_night_scaled ~ booking_date+Actual_stay_booked+
               channel_code+main_product_code+numberofadults+numberofchildren+persontravellingid+
               resort_region_code+resort_type_code+room_type_booked_code+roomnights+
               season_holidayed_code+state_code_residence+state_code_resort+
               total_pax+booking_type_code+member_age_buckets_A+member_age_buckets_B+
               member_age_buckets_D+member_age_buckets_E+member_age_buckets_F+member_age_buckets_H+
               member_age_buckets_I+reservationstatusid_code_B+cluster_code_A+cluster_code_B,data = train)
summary(model29)

model30 = lm(amount_spent_per_room_night_scaled ~ booking_date+Actual_stay_booked+
               channel_code+main_product_code+numberofadults+numberofchildren+persontravellingid+
               resort_region_code+resort_type_code+room_type_booked_code+roomnights+
               season_holidayed_code+state_code_residence+state_code_resort+
               total_pax+booking_type_code+member_age_buckets_A+member_age_buckets_B+
               member_age_buckets_D+member_age_buckets_E+member_age_buckets_F+member_age_buckets_H+
               member_age_buckets_I+reservationstatusid_code_B+cluster_code_A+cluster_code_B+
               cluster_code_C,data = train)
summary(model30)

model31 = lm(amount_spent_per_room_night_scaled ~ booking_date+Actual_stay_booked+
               channel_code+main_product_code+numberofadults+numberofchildren+persontravellingid+
               resort_region_code+resort_type_code+room_type_booked_code+roomnights+
               season_holidayed_code+state_code_residence+state_code_resort+
               total_pax+booking_type_code+member_age_buckets_A+member_age_buckets_B+
               member_age_buckets_D+member_age_buckets_E+member_age_buckets_F+member_age_buckets_H+
               member_age_buckets_I+reservationstatusid_code_B+cluster_code_A+cluster_code_B+
               cluster_code_C+cluster_code_D,data = train)
summary(model31)

model32 = lm(amount_spent_per_room_night_scaled ~ booking_date+Actual_stay_booked+
               channel_code+main_product_code+numberofadults+numberofchildren+persontravellingid+
               resort_region_code+resort_type_code+room_type_booked_code+roomnights+
               season_holidayed_code+state_code_residence+state_code_resort+
               total_pax+booking_type_code+member_age_buckets_A+member_age_buckets_B+
               member_age_buckets_D+member_age_buckets_E+member_age_buckets_F+member_age_buckets_H+
               member_age_buckets_I+reservationstatusid_code_B+cluster_code_A+cluster_code_B+
               cluster_code_C+cluster_code_D+cluster_code_E,data = train)
summary(model32)
