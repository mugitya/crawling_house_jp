
#====setting ====

##set working directory
#setwd("/Users/tomoo/Dropbox/2017SeikeiLec/ungra_seminar/analysis") 

##install packages
#for prepare 
library(readr)
#library(makedummies)

#for plot
library(scatterplot3d) 
library(ggplot2)
library(MASS) 
library(MuMIn) 
library(ggmap)
library(stargazer)


##reading csv file
original <- master1202                              #read.csv("master1123-E.csv", header=TRUE)

# eliminate row(s) if any data is missing
data <- original[complete.cases(original), ] 



# recreate dataset for model -11,, -18
str(data)
data2 <-  data[,c(-1,-2, -8, -9, -10,-11,-13, -14 , -15, -17)]


str(data2)                                            ##splitting data into three subsets

#residential data : data.resi
data.resi <- data2[data2$resi==1,] 
data.resi.v <- data[data$resi==1,] 


#separeate district b on tyu-ya jinkou ritu : df.und & df.up
df.und.100  <- subset(data2, tiku_edogawa ==1 | tiku_katushika == 1 | 
                            tiku_adachi == 1 | tiku_arakawa == 1 | tiku_kita == 1 | 
                            tiku_itabashi == 1 | tiku_nerima == 1 | tiku_nakano == 1 |
                            tiku_suginami == 1 | tiku_setagaya == 1| tiku_ota == 1 )

#remove other dummys
df.und.100 <-df.und.100[, -which (colnames(df.und.100) %in%
                         c("tiku_etou", "tiku_sumida", "tiku_taitou",
                           "tiku_bunkyou","tiku_toyoshima", "tiku_shinjuku",
                           "tiku_chiyoda", "tiku_chuou", "tiku_minato",
                           "tiku_shibuya", "tiku_meguro", "tiku_shinagawa"))]

str(df.und)

df.up.100 <- subset(data2, tiku_etou ==1 | tiku_sumida ==1 | tiku_taitou == 1 | 
                       tiku_bunkyou == 1 | tiku_toyoshima == 1 | tiku_shinjuku == 1 | 
                       tiku_chiyoda == 1 | tiku_chuou == 1 | tiku_minato == 1 |
                       tiku_shibuya == 1 | tiku_meguro == 1| tiku_shinagawa == 1 )

df.up.100     <-   df.up.100[, -which (colnames(df.up.100) %in%
                                     c("tiku_edogawa", "tiku_katushika",
                                       "tiku_adachi","tiku_arakawa", "tiku_kita", 
                                       "tiku_itabashi", "tiku_nerima", "tiku_nakano",
                                       "tiku_suginami", "tiku_setagaya", "tiku_ota" ))]

#remove resi dum
df.und <- df.und.100[ , -6]
df.up <- df.up.100[ , -6]


##create map : map
map <- get_googlemap(center = c(139.7670516,35.6811673), 
                     maptype = "terrain", source = "google",
                     zoom = 11, color = "bw")

#====define colors====

#create colors for plot : trand_blue & trans_red
trans_blue	<- rgb(red = 0.2, green = 0.2, blue = 1.0, alpha = 0.2)
trans_red	<- rgb(red = 1.0, green = 0.2, blue = 0.2, alpha = 0.2)

#change color based on price : col_price
col_price <- densCols(data.resi$price, colramp=colorRampPalette(c("red","yellow","blue")))




#==== About data names====

# num : number
# price : price ( en / 1 heihou M )
# space: tiseki
# distance : ekimadeno kyori (m)
# longi : keido
# lati : ido
# alti : hyoukou
# faratio : yousekiritu
# b2lratio : kenpei ristu
# shape : keijyou
# resi : residential dummy
# ind : industry dummy
# comm : commercial dummy
# burg : sinnnyuu 
# nunburg : hi shinnnyuu
# theft : burg+nunburg
# sales : syougyoutoukei( enn )
# floor : (youseki / kenpei) nannkai date


str(data)


#====regression model====

### About residential


##lm about df.und : model_und
str(df.und)
df.und.m <- df.und[ , c(-2, -4, -5,-7,-20)]
model_und <- lm(price~. , data=df.und.m)

df.und$und.gosa <- model_und$residuals
plot(df.und$und.gosa)

ggplot(data = df.und, aes(x = longi, y = lati, col = und.gosa)) +
  geom_point() +
  scale_colour_gradient(low="blue",high="yellow") +
  theme_bw() +
  theme(legend.position = "top") +
  ggtitle("Latitude vs. Longitude with und.Gosa")
dev.off()




##lm about df.up : model.up
str(df.up)
df.up.m <- df.up[ , c(-2,-3, -4, -5, -21)]
model_up <- lm(price~. , data=df.up.m)


df.up$up.gosa <- model_up$residuals
plot(df.up$up.gosa)

ggplot(data = df.up, aes(x = longi, y = lati, col = up.gosa)) +
  geom_point() +
  scale_colour_gradient(low="blue",high="yellow") +
  theme_bw() +
  theme(legend.position = "top") +
  ggtitle("Latitude vs. Longitude with up.Gosa")
dev.off()


#show summary
stargazer(model_und ,model_up, type = "text", single.row = TRUE)






#====plot ====
###--- About residential data


###plot about data :: kinitu deha nai
ggmap(map, extent = "device") +
  geom_density2d(data = data.resi, aes(x = longi, y = lati), size = 0.3) +
  stat_density2d(data = data.resi, 
                 aes(x = longi, y = lati, fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 16, geom = "polygon") +
  scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE) + 
  ggtitle("data no mitudo")

####plot map based on price color
ggmap(map) + geom_point(data=data.resi ,
                        aes(x=longi, y=lati), color=col_price, alpha = 0.8)

### log price 

data.resi.l<- transform(data.resi, lprice=log(price))

ggmap(map, darken = c(0.2, "white")) + 
  geom_point(data = data.resi.l, aes(x = longi, y = lati, colour = lprice))+ 
  ggtitle("log price") +
  xlab("ido") + ylab("keido")

### plot for floor and price
ggmap(map) +
geom_point(aes(x=longi, y=lati),data=data.resi,
           size=data.resi$floor,alpha=0.5, color=col_price) +
  ggtitle("floor and price")

###plot for space and price

#space/100 : space2 
space_df <- transform(data.resi, space2 = space/100)

ggmap(map) +
  geom_point(aes(x=longi, y=lati),data=space_df,
             size=space_df$space2 ,alpha=0.3,
             color=col_price) +
  ggtitle("space and price")

#distance and price
dis_df <- transform(data.resi.v, distance2 = distance/300)
col_price.dis <- densCols(dis_df$price, colramp=colorRampPalette(c("red","yellow","blue")))
ggmap(map) +
  geom_point(aes(x=longi, y=lati),data=dis_df,size=dis_df$distance2, alpha=0.8,
             color=col_price.dis) +
  ggtitle("distance and price")

#theft
th_df <- transform(data.resi.v, theft2 = log(theft))
col_price.th <- densCols(data.resi.v$theft, colramp=colorRampPalette(c("red","yellow","blue")))
ggmap(map) + geom_point(data=data.resi.v ,
                        aes(x=longi, y=lati), color=col_price.th, alpha = 0.8)+
  ggtitle("theft")


#theft and price
ggmap(map) +
  geom_point(aes(x=longi, y=lati),data=data.resi.v,size = th_df$theft2, alpha=0.5,
             color=col_price) +
  ggtitle("theft and price")




# scatter of  space & place
plot(data.resi$space, data.resi$price)



## 3D scatter plot : g3d
# x : space
# y : distance
# z : price
g3d <- scatterplot3d(data.resi$space,
                     data.resi$distance,
                     data.resi$price,
                     #color = trans_blue,
                     #pch = 16,
                     angle=130)

str(data.resi)

#hyoukou and price
scatterplot3d(data.resi.v$longi,
              data.resi.v$lati,
              data.resi.v$alti,
              color = col_price,
              pch = 20,
              angle=70, title(main = "hyoukou and price"))



# Altitude by Latitude-Longitude
  ggplot(data = data.resi.v, aes(x = longi, y = lati, col = alti)) +
  geom_point() +
  scale_colour_gradient(low="blue",high="yellow") +
  theme_bw() +
  theme(legend.position = "top") +
  ggtitle("Latitude vs. Longitude with Land Price")
dev.off()


# Land Price by Latitude-Longitude
ggplot(data = data.resi, aes(x = longi, y = lati, col = price)) +
  geom_point() +
  scale_colour_gradient(low="blue",high="yellow") +
  theme_bw() +
  theme(legend.position = "top") +
  ggtitle("Latitude vs. Longitude with Land Price")
dev.off()






#==== create dataset====

#万円になおす：sales2
#transform(data, sales2 <- data$sales/10000 )
#キロメートルに治す : dis_t_k
#transform(data, dis_t_k <- data$dis_t/1000)


##ダミー変数作成
#地区のみでデータフレーム作成
dat <- data.frame(tiku = data$tiku)
#ダミーを２３区すべてで作成
dum_tiku <- makedummies(dat,basal_level = TRUE)

str(dum_tiku)
#基準となるダミーを一つ削除する(世田谷削除)
dum_tiku2 <- dum_tiku[,-16]
str(dum_tiku2)

#dataからtiku 削除 : data2
head(data)
data2 <- data[,-2]
head(data2)
#データフレームにdum_tiku結合
data3 <- data.frame(data2, dum_tiku)
str(df)

new_df <- transform(df, floor=faratio/b2lratio)

str(new_df)

write.csv(new_df, "master1129.csv", quote=FALSE, row.names=TRUE)








#### create model No.2####
# model-3
model3 <- lm(price~space+distance+sales+theft, data=data.resi)
data.resi$gosa3 <- model3$residuals
plot(data.resi$gosa3)

ggplot(data = data.resi, aes(x = longi, y = lati, col = gosa3)) +
  geom_point() +
  scale_colour_gradient(low="blue",high="yellow") +
  theme_bw() +
  theme(legend.position = "top") +
  ggtitle("Latitude vs. Longitude with Gosa3")
dev.off()
summary(model3)

# model-4
model4 <- lm(price~space+distance+sales+theft+alti, data=data.resi)
data.resi$gosa4 <- model4$residuals
plot(data.resi$gosa4)

ggplot(data = data.resi, aes(x = longi, y = lati, col = gosa4)) +
  geom_point() +
  scale_colour_gradient(low="blue",high="yellow") +
  theme_bw() +
  theme(legend.position = "top") +
  ggtitle("Latitude vs. Longitude with Gosa4")
dev.off()
summary(model4)


# model-5
model5 <- lm(price~space+distance+sales+theft+alti+shape, data=data.resi)
data.resi$gosa5 <- model5$residuals
plot(data.resi$gosa5)

ggplot(data = data.resi, aes(x = longi, y = lati, col = gosa5)) +
  geom_point() +
  scale_colour_gradient(low="blue",high="yellow") +
  theme_bw() +
  theme(legend.position = "top") +
  ggtitle("Latitude vs. Longitude with Gosa5")
dev.off()
summary(model5)

## lm(y : price, x : space) : r_ps_model
r_ps_model <- lm(price~space, data=data.resi)

summary(r_ps_model)

## lm(y : price, x : space distance ) : r_psd_model
r_psd_model <- lm(price~space+distance, data=data.resi)

summary(r_psd_model)

# plot gosa of r_psd_model
data.resi$gosa1 <- r_psd_model$residuals
plot(data.resi$gosa1)
str(data)

# plot gosa of r_psds_model
data.resi$gosa2 <- r_psds_model$residuals
plot(data.resi$gosa2)


### use step-wize

# create full model without longi alti dummy : model_full 
model_full <- lm(price ~ space + distance 
                 + alti + faratio + 
                   b2lratio + shape + burg + nonburg + theft + 
                   sales, data.resi)
model

# hensuu gensyou
model_back <- step(model_full)
summary(model_back)

##step-wize back model : sw_model
sw_model <- lm(formula = price ~ space + distance + alti + faratio + b2lratio + 
                 nonburg + sales, data = data.resi)
AIC(sw_model) #---15968.67

summary(sw_model)

###print sw_model summery:sw
sw <- stargazer(sw_model,r_psd_model,type = "text", single.row = TRUE)
#----omit.table.layout = "sn"syouryaku statistic and note

sw


###--- About r_ps_model 

# line baced on r_ps_model
abline(r_ps_model, col="blue")
dev.off()


###---- About r_psd_model


g3d$plane3d(r_psd_model, col= "blue")

#plot for gosa of r_psd_model
ggplot(data = data.resi, aes(x = longi, y = lati, col = gosa1)) +
  geom_point() +
  scale_colour_gradient(low="blue",high="yellow") +
  theme_bw() +
  theme(legend.position = "top") +
  ggtitle("Latitude vs. Longitude with Gosa1")
dev.off()


###--- About r_psds_model

ggplot(data = data.resi, aes(x = longi, y = lati, col = gosa2)) +
  geom_point() +
  scale_colour_gradient(low="blue",high="yellow") +
  theme_bw() +
  theme(legend.position = "top") +
  ggtitle("Latitude vs. Longitude with Gosa2")
dev.off()




