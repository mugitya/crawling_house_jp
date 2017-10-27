
# coding: utf-8

# In[1]:



#クローラー利用
from bs4 import BeautifulSoup
import requests
import pandas as pd
import time
#スクレイピング利用
import numpy as np
#データ整理利用
from pandas import Series, DataFrame


# In[2]:

#=====データ定義=====
shozai = []#所在及び地番 
jyukyo_h = []#住居表示  　NAあり
kakaku = []#価格
tiseki = []#地積
riyoukubun = []#利用区分
tyousa_bi = []#調査日
koutu = []#交通施設、距離
keijyou = []#形状（間口：奥行き）
ken_you = []#建ぺい率　容積率
#====２３区の辞書===
dict = {"世田谷区":13112,
        "江戸川区":13123,
        "葛飾区":13122,
        "足立区":13121,
        "荒川区":13118,
        "台東区":13106,
        "江東区":13108,
        "北区":13117,
        "豊島区":13116,
        "新宿区":13104,
        "千代田区":13101,
        "中央区":13102,
        "墨田区":13107,
        "文京区":13105,
        "板橋区":13119,
        "練馬区":13120,
        "中野区":13114,
        "渋谷区":13113,
        "港区":13103,
        "品川区":13109,
        "目黒区":13110,
        "杉並区":13115,
        "大田区":13111
}


# In[3]:

#テーブルすべてのデータを格納する関数：takeall(tables)
def takeall(tables):
    for x in range(0, 20):
        data = []
        rows =tables[x].find_all('div', {'class':'datalistline'})#はじめのテーブルから持ってくる
        len(rows)
        for tr in rows:
            cols = tr.find_all('div')
            for td in cols:
                text = td.find(text=True)
                #print(text)
                data.append(text)
        #data

        #データ格納
        tyousa_bi.append(data[3])#調査日
        shozai.append(data[5])#所在及び地番
        jyukyo_h.append(data[7])#住居表示  NAあり
        kakaku.append(data[9])#価格
        koutu.append(data[11])#交通施設、距離
        tiseki.append(data[13])#地積
        keijyou.append(data[15])#形状（間口：奥行き）
        ken_you.append(data[-5])#建ぺい率　容積率


# In[7]:

#クロールしてtakeallでデータを格納する関数:spider(地区ナンバー)
def spider(district_code):
    print("doing...") #実行中表示
    KeepGoing = True
    x = 1
    while KeepGoing:  ###Trueの限り
        try:#データ格納を試す
            result =requests.get('http://www.land.mlit.go.jp/landPrice/SearchServlet?MOD=2&TDK=&SKC='+str(district_code)+'&CHI=&YFR=2009&YTO=2015&YOU=0%2C5%2C9&PFR=&PTO=&PG='+str(x)+'&LATEST_YEAR=')
            c = result.content
            soup = BeautifulSoup(c, "lxml")
            summary = soup.find('div', {'class':'kekka2'})
            tables = summary.find_all('div', {'class':'datalist'})
            len(tables) #20件すべて取り出し完了
            takeall(tables)#データを格納する

            x = x+1 #次のページヘ
            time.sleep(1)#1秒待つ
            

        except:#例外発生の場合,KeepGoingをFalse
            final = x*20
            print(str(final)+"件取り出し完了")

            KeepGoing = False
            break


# In[8]:

###クロール
#検索条件：〔地域〕東京都２３区 〔対象〕地価公示・都道府県地価調査の両方 〔調査年〕平成21年～平成27年 〔用途区分〕住宅地 商業地 工業地  〔地価〕全て

#辞書の値に関して実行する
#for k,v in dict.items():
#    print(k)
#    spider(v)
spider(13112)#試しに世田谷区取り出し　実際は1368件　取り出しは1380　最後のページが１２件？


# In[9]:

##スクレイピング　　　　　！！！！！！！件数が違いすぎるのでスクレイプし直す必要ある
print("=========スクレイピング移行========")

#所在及び地番:shozai
shozai2 = ",".join(shozai).replace("\u3000\n\n\n\t\t\t\t\t\t\t","")
shozai = shozai2.split(',')
print("所在及び地番")
print(shozai[0])
print(str(len(shozai))+"件")

#住居表示:jyukyo_h
jyukyo_h2 = ",".join(jyukyo_h).replace("\xa0","NA")
jyukyo_h = jyukyo_h2.split(',')
print("住居表示")
print(jyukyo_h[0])
print(str(len(jyukyo_h))+"件")

#価格表示:kakaku
kakaku2 = ",".join(kakaku).replace("(円/m²)","")
kakaku3 = kakaku2.replace(",0","0")#1000のカンマ除外
kakaku = kakaku3.split(',')
print("価格表示")
print(kakaku[0])
print(str(len(kakaku))+"件")

#交通施設と距離　順繰りに入っている
koutu = ",".join(koutu).replace("m","")
re_koutu = koutu.replace(" 1,","1")#1000のカンマが引っかかるので除外
koutu2 = re_koutu.replace("、",",")
koutu3 = koutu2.replace(" ","")
koutu = koutu3.split(',')
print("交通施設と距離")
print(koutu[0])
print(str(len(koutu))+"件")

#地積
tiseki2 = ",".join(tiseki).replace("(m²)","")
tiseki = tiseki2.split(',')
print("地積")
print(tiseki[0])
print(str(len(tiseki))+"件")

#建ぺい率　容積率　順繰りにはいっている
ken_you = ",".join(ken_you).replace("\n\t\t\t\t\t\t\t\t\n\t\t\t\t\t\t\t\t" ,"t")
ken_you2 = ken_you.replace("t",",")
kenyou3 = ken_you2.replace("(%)","")
ken_you = kenyou3.split(',')
print("建ぺい率、容積率")
print(ken_you[0])
print(str(len(ken_you))+"件")


# In[ ]:



