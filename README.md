# Econometrics_Report

## 概要
このコードは、春学期の「計量経済学概論」の授業で最終課題として課されたレポートのためにコーディングしたものです。  

`srcs`ディレクトリの中に`stock2018q.csv`と``

`stock2018q.csv`のファイルは、1960年〜2018年の日本の株価と利子率、GDPに関する情報が格納されているもので、各項目は次の通りです。  

<hr>

- s  
日経平均株価

- r  
利子率

- y  
GDPの四半期データ

<hr>

`house_source_lec.csv`ファイルは、`https://sumaity.com/`から取得した日吉駅周辺の物件情報を格納したファイルで、各項目は次の通りです。  

<hr>

- add  
住所

- year  
建築から2020年までの経過年数

- struc  
木造かどうかかどうか(1なら木造)

- dis  
駅からの徒歩の距離

- bus  
駅までバスが必要か（1なら必要）

- rent  
賃貸料（万円）

- mng  
管理費（円）

- space  
広さ（平方メートル）

<hr>

これらのcsvファイルを元にグラフの描画や回帰分析などを行うというのが、最終課題の大まかな内容でした。  

`計量経済学レポート_21917983_長谷川優太.pdf`が自分の実際に提出したレポートファイルで、`軽量経済レポート.R`がレポートを作成するために使用したRのソースコードです。  

## 目的
初めてのRを使用したデータ分析であったので、Rをどのようにして使用するかなどを体感するというのが大きな目的の一つでした。  
内容は1年次に履修した「データ解析入門」の内容とかぶっている部分が多く、エクセルの分析ツールを使用してやっていたことをRに落とし込むという感覚でコーディングするという目的もありました。  

## 使い方
Rをインストールし、`軽量経済レポート.R`を実行するとコードが実行されます。  

## 開発環境
- MacBook Pro (15-inch, 2018)
- macOS Big Sur Version 11.2.1
- R version 3.6.1
