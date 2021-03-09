# p値が0.05未満であれば優位になる


dat <- read.csv("srcs/stock2018q.csv")
dat

attach(dat)

# 問題1
plot(s,type="l")
sg=(s[5:236]-s[1:232])/s[1:232]*100
dev.new()
plot(sg,type="l")

# 問題2rrこれであってるっけ
rr = r[5:236]
dev.new()
plot(sg,rr,type="p")
yg=(y[5:236]-y[1:232])/y[1:232]*100
dev.new()
plot(sg,yg,type="p")

#問題3
summary(lm(sg~rr))
summary(lm(sg~yg))

# 問題4 所得の影響もあるため異なる
summary(lm(sg~yg+rr))

# 問題5
summary(lm(sg[1:116]~yg[1:116]+rr[1:116]))
summary(lm(sg[117:232]~yg[117:232]+rr[117:232]))

# 問題6
D = dummy[5:236]
summary(lm(sg~rr+yg+D+D*rr))
anova(lm(sg~rr+yg),lm(sg~rr+yg+D+D*rr))
((98141 - 78288) / 2 ) / (78288 / (232 - 5))

# 問題7
summary(lm(sg~yg+rr+D+D*yg))
summary(lm(sg~yg+rr))
anova(lm(sg~rr+yg),lm(sg~rr+yg+D+D*yg))
# 問題8

# 問題9 #あってるかわからん
# バブル期以前含める
summary(lm(sg[2:236]~yg[1:235]+rr[1:235]))

# これに代入する （2019年のデータはないのに？）
# 2018年のデータを入れれば2019年が出るわ
# sg = 0.8053yg - 1.0399rr + 7.1526
# 2019sg = 0.8053*(-0.06590079) - 1.0399* 0.907 + 7.1526
# 2018sg = 0.8053*(-0.29057102) - 1.0399* 0.916 + 7.1526
ans = 0.8053*(-0.06590079) - 1.0399* 0.907 + 7.1526
ans


#バブル期以前含めない
# 含めない方が良さげ
summary(lm(sg[117:232]~yg[116:231]+rr[116:231]))

# sg = 14.1356 + 2.5831yg - 6.2778rr
# 2019sg = 14.1356 + 14.1356*(-0.06590079) - 6.2778* 0.907
ans2  = 14.1 + 2.58*(-0.0659) - 6.28* 0.907

ans2



# 問題10
# yg ~ sg + rr 

# バブル期以前含めないやつ
summary(lm(yg[117:232]~sg[116:231]+rr[116:231]))
# yg = -1.632003 + 0.060479sg + 0.996180rr
# 2019yg = -1.632003 + 0.060479(-1.3092958) + 0.996180(0.907)
ans3 = -1.632003 + 0.060479*(-1.3092958) + 0.996180*(0.907)
ans3

# バブル期以前含めるやつ(含めた方が良さげ？？)
summary(lm(yg[2:232]~sg[1:231]+rr[1:231]))
# yg = -3.69030 + 0.07954sg + 1.93483rr
#  + (sg-1) + (rr-1)
# 2019yg = - 3.69 + (-1.31)* 0.0795 + 1.93* 0.907
ans4 = - 3.69 + (-1.31)* 0.0795 + 1.93* 0.907
ans4



# 問題1
dat2 <- read.csv("srcs/house_source_lec.csv")
dat2

attach(dat2)

RRent = rent*10 + mng/1000

plot(RRent,dis,type="p")
dev.new()
plot(RRent,space,type="p")
dev.new()
plot(RRent,year,type="p")

# 問題2
summary(lm(RRent~dis))
summary(lm(RRent~space))
summary(lm(RRent~year))

# 問題3
summary(lm(RRent~dis+space+year))

# 問題4
summary(lm(RRent~dis+space+year+bus))

# 問題5
# library(lm.beta)
# result <- lm(RRent~dis+space+year+bus)
# (lm.beta(result))
# 係数の割り算をする（11回）
ans4.5 = -0.702 / 1.42
ans4.5
ans5 = -13.5/-0.724
ans5

# あってるかわからん

# 問題6
# 値を代入するだけ
# RRent = 66.21113 -0.72356dis + 1.41699 space -0.70196year -13.47406bus
# RRent=rent*10+mng/1000 
# rent = ( 66.21113 -0.72356dis + 1.41699 space -0.70196year -13.47406bus - mmg/1000)/10


ans6 = (66.21113 -0.72356*5 + 1.41699*20 -0.70196*15 - 1000/1000)/10
# 単位が千円だから
ans6 = ans6 * 1000
ans6

ans6.5 = 66.2 - 0.724*5 + 1.42*20 - 0.702*15 - 13.5*0
ans6.5

# 80.45 = 1000*10 + mng / 1000
#mng = 80.45  * 1000 - 1000
#mng
80.45 * 1000 - 1000


# 問題7
result <- lm(RRent~dis+space+year+bus)
names(result)
result$residuals
summary(result$residuals)
# 残差のmaxは76番め、minは16番目
RRent[147]
RRent[76]
result$residuals[147]
result$residuals[76]

# 問題8
# 考察の過程が大事
# おそらく他の変数の要因が関係している（事故物件とかそういう感じ）
# 芸能人が住んでいたとか
# 簡単に割安を選ぶことはできない  

# 問題9
# 交差こうの使い道（）
# バスは0か1をとる
# 0、1の時はどのような式をとるのか
# gが有意であったら、出なかったら何が言えるのか  
summary(lm(RRent~dis+space+year+bus+bus*dis))
anova(lm(RRent~dis+space+year),lm(RRent~dis+space+year+bus+bus*dis))


# 課題３
# 一個追加してみましたではダメ
# どういった仮説、糸を持ってやったか
# リサーチクエスチョンともいう
# どんな問題に答えたくてそれをやったかを示すこと  
# データ読み込み
dat3 <- read.csv("srcs/house_source_lec.csv")
# データの確認
dat3
# 下準備
attach(dat3)
#  RRentの定義
RRent = rent*10 + mng/1000
# 重回帰分析
summary(lm(RRent~dis+space+year+bus+ struc + height ))

# RRent = 65.7 - 0.724dis + 1.38space -0.720year -14.2bus -3.47struc + 1.13height

# strucを除いた重回帰分析
summary(lm(RRent~dis+space+year+bus+ height))
# RRent = 64.0 - 0.756dis + 1.40space -0.711year -14.2bus + 1.40height





# ばらつきの大きさによっても違うので、標準化回帰で再び分析してみる
# パッケージからlm.betaをインストールしてくる




# 相対的な重要性は標準化しても、あんまり変わらなかった
