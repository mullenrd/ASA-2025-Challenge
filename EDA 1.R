library(arrow)
swing<-read_feather("/Users/reesemullen/Desktop/Statistical Practice/ASA-2025-Challenge/OneDrive_1_10-12-2024/statcast_pitch_swing_data_20240402_20240630.arrow")

swing$game_date<-as.Date(swing$game_date)
summary(swing$game_date)

swing1<-Filter(function(x)!all(is.na(x)), swing)
summary(swing1$bat_speed)
summary(swing1$swing_length)
