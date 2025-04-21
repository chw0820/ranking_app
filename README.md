# 問卷排序分析工具（Shiny App）

這是一個基於 R 語言與 Shiny 所製作的問卷排序分析工具，提供使用者進行單選題與複選題的排名計算與視覺化，方便用於問卷資料分析與研究論文撰寫。

## 功能介紹

- 支援上傳 CSV 格式的問卷數據
- 自動偵測並處理 Respondent 或 ID 等相關欄位
- 可指定分析的欄位範圍
- 提供 Wald 檢定與 GS 檢定方法
- 產生排名結果表格與圖表
- 支援結果下載（CSV 與 PNG）

## 線上體驗

👉 點擊以下連結體驗此應用程式：

🔗 [Shiny App 線上連結](https://hsingwen0820.shinyapps.io/rankingapp/)

> 若連結失效，請至 ShinyApps.io 搜尋相關部署

## 資料結構說明

上傳的資料需符合以下結構：

- CSV 格式
- 欄位可包含 Respondent 或 ID 等相關欄位（後台會自動排除）
- 題目可為單選或複選格式

範例如下：

| Respondent | Q1_Apple | Q1_Orange | Q1_Banana |
|------------|----------|-----------|-----------|
| 001        | 1        | 0         | 1         |
| 002        | 0        | 1         | 1         |
| 003        | 0        | 0         | 1         |
| 004        | 1        | 1         | 0         |
| 005        | 1        | 0         | 0         |

##  使用技術

- R
- Shiny
- RankResponse 套件
- ggplot2 / DT / shinythemes 等

##  安裝與執行（本機）

```r
# 安裝必要套件
install.packages(c("shiny", "shinythemes", "DT", "ggplot2", "RankResponse", "reshape2"))

# 執行 App
shiny::runApp("rankingApp")
```

##  作者

此應用程式由 **H.W,CHIEN** 製作，用於學術用途與問卷資料分析輔助。  
📫 聯絡方式：**xingwen0820@gmail.com**

##  License

本專案僅供學術與非商業用途使用，若需商業授權請聯絡作者。

## 範例數據來源

本章以 Kaggle 2022 年度調查（Kaggle Survey 2022）公開數據集為例，說明如何使用本研究開發的 Shiny 平台進行多選題問卷數據的統計分析。該調查針對全球資料科學家與機器學習從業者，涵蓋學習背景、職涯發展、技術與平台等主題。原始數據可從 [Kaggle 官方網站](https://www.kaggle.com/competitions/kaggle-survey-2022/data) 下載。
