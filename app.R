# 安裝套件
if (!require("shiny")) install.packages("shiny")
if (!require("shinyFiles")) install.packages("shinyFiles")
if (!require("RankResponse")) install.packages("RankResponse")
if (!require("DT")) install.packages("DT")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("shinythemes")) install.packages("shinythemes")
if (!require("reshape2")) install.packages("reshape2")


# 載入套件
library(shiny)
library(shinyFiles)
library(RankResponse)
library(DT)
library(ggplot2)
library(shinythemes)


# User Interface
ui <- fluidPage(
  theme = shinytheme("flatly"),  
  titlePanel("單選題/複選題排序"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "上傳數據文件（CSV）", accept = ".csv", multiple = TRUE), 
      numericInput("start_col", "選擇開始欄位（未選則預設為全部）", value = NULL, min = 1, step = 1),
      numericInput("end_col", "選擇結束欄位（未選則預設為全部）", value = NULL, min = 1, step = 1),
      sliderInput("alpha", "顯著性水準 α", min = 0.01, max = 0.10, value = 0.05, step = 0.01),
      actionButton("process", "計算排名", class = "btn-primary"),
      br(), br(),
      downloadButton("download_csv", "下載結果 (CSV)", class = "btn-success"),
      downloadButton("download_plot", "下載圖表 (PNG)", class = "btn-info")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("結果表格", DTOutput("resultsTable")),
        tabPanel("排名圖表", plotOutput("rankingPlot"))
      )
    )
  )
)


# server
server <- function(input, output) {
  results_data <- reactiveVal()  # 儲存計算結果
  
  observeEvent(input$process, {
    req(input$file)
    
    withProgress(message = "計算中...", value = 0.5, {
      incProgress(0.2, detail = "讀取數據中...")
      raw_data <- read.csv(input$file$datapath, header = TRUE)
      
      # 設定分析範圍
      start_col <- ifelse(is.na(input$start_col), 1, input$start_col)
      end_col <- ifelse(is.na(input$end_col), ncol(raw_data), input$end_col)
      
      validate(
        need(start_col > 0 && start_col <= ncol(raw_data), "開始欄位超出範圍"),
        need(end_col > 0 && end_col <= ncol(raw_data), "結束欄位超出範圍"),
        need(start_col <= end_col, "開始欄位不能大於結束欄位")
      )
      
      incProgress(0.4, detail = "分析數據中...")
      # 選擇指定的欄位範圍
      selected_data <- raw_data[,start_col:end_col]
      
      
      # 偵測並移除 ID 列（如果存在）
      if (any(grepl("id|respondent", colnames(selected_data), ignore.case = TRUE))) {
        cleaned_data <- selected_data[, !grepl("id|respondent", colnames(selected_data), ignore.case = TRUE)]
      } else {
        cleaned_data <- selected_data
      }
      
      # **步驟 1：檢查第一列是否為「題號」**
      first_col <- raw_data[,1]  # 取得第一列標題
      is_question_id <- all(grepl("^Q\\d+.*", first_col))  
      
      # **步驟 2：檢查第二列是否為「題目描述」**
      second_col <- as.character(raw_data[, 2])  # 取得第二列內容
      is_question_text <- mean(nchar(second_col) > 20, na.rm = TRUE) > 0.8 
      
      # **步驟 3：決定移除列數**
      
      if (is_question_id && is_question_text) {
        message("偵測到題號與題目描述，移除前兩列")
        processed_data <- raw_data[,-c(1,2) ]  # 移除前兩列
      } else {
        message("偵測到題號，移除第一列")
        processed_data <- raw_data[, -1]  # 只移除第一列
      }
      
      # 判斷是單選題還是複選題
      if (ncol(cleaned_data) == 1) {  # 單選題邏輯
        question_name <- colnames(cleaned_data)[1]
        single_choice_column <- cleaned_data[[1]]
        
        # 清理單選題數據
        single_choice_column <- trimws(single_choice_column)  
        single_choice_column <- single_choice_column[!is.na(single_choice_column) & single_choice_column != ""]  # 去除 NA 和空值
        validate(need(length(single_choice_column) > 0, "單選題數據為空，請檢查上傳文件"))
        
        # 提取唯一選項並生成二元矩陣
        unique_options <- unique(single_choice_column)
        validate(need(length(unique_options) > 1, "單選題選項數量不足，請提供至少兩個選項"))
        
        binary_data <- sapply(unique_options, function(option) {
          as.numeric(single_choice_column == option)
        })
        colnames(binary_data) <- unique_options
        responses <- as.matrix(binary_data)
        
      } else {  # 複選題邏輯
        binary_data <- apply(cleaned_data, 2, function(col) {
          if (is.character(col)) {
            return(as.numeric(trimws(col) != ""))
          } else {
            return(as.numeric(!is.na(col) & col != 0))
          }
        })
        
        option_names <- colnames(cleaned_data)
        responses <- as.matrix(binary_data)
      }
      
      alpha <- input$alpha
      rankings_list <- list()
      
      # 計算排名
      for (method in c("wald", "gs")) {
        for (ranktype in c(1, 2)) {
          if (method == "wald") {
            result <- rank.wald(data = responses, alpha = alpha, ranktype = ranktype)
          } else if (method == "gs") {
            result <- rank.gs(data = responses, alpha = alpha, ranktype = ranktype)
          }
          
          ranking_only <- gsub("(\\d+\\.\\d+),\\s*", "", paste(result, collapse = ", "))
          rankings <- strsplit(ranking_only, ",\\s*")[[1]]
          rankings_list[[paste(method, "Rule", ranktype, sep = "_")]] <- rankings
        }
      }
      
      incProgress(0.4, detail = "產生結果...")
      
      final_results <- do.call(rbind, lapply(names(rankings_list), function(name) {
        new_name <- switch(name,
                           "wald_Rule_1" = "Wald檢定、方法1",
                           "wald_Rule_2" = "Wald檢定、方法2",
                           "gs_Rule_1" = "G.S.檢定、方法1",
                           "gs_Rule_2" = "G.S.檢定、方法2",
                           name 
        )
        
        data.frame(
          選項 = new_name,
          t(as.data.frame(rankings_list[[name]])),
          row.names = NULL
        )
      }))
      
      if (exists("unique_options")) {
        colnames(final_results) <- c("選項", unique_options)
      } else {
        colnames(final_results) <- c("選項", option_names)
      }
      
      results_data(final_results)
      
      output$resultsTable <- renderDT({
        datatable(final_results, options = list(pageLength = 10, autoWidth = TRUE))
      })
      
      output$rankingPlot <- renderPlot({
        final_results_long <- reshape2::melt(final_results, id.vars = "選項", variable.name = "項目", value.name = "排名")
        
        final_results_long$排名 <- as.numeric(as.character(final_results_long$排名))
        final_results_long <- final_results_long[order(final_results_long$排名), ]
        
        ggplot(final_results_long, aes(x = 項目, y = 排名, fill = 選項)) +
          geom_bar(stat = "identity", position = "dodge",color=NA) +
          theme_minimal() + 
          scale_fill_brewer(palette = "Set2") +  
          labs(title = "排名結果", x = "選項", y = "排名順序", fill = "檢定規則") +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "lightgray", size = 0.3), 
            panel.grid.minor = element_blank(), 
            panel.background = element_rect(fill = "white", colour = "white"),
            panel.border = element_blank(),
            plot.title = element_text(hjust = 0.5) 
          ) +
          scale_y_continuous(breaks = seq(min(final_results_long$排名), max(final_results_long$排名), by = 1))
      })
    })
  })
  
  output$selected_range <- renderText({
    paste("目前分析範圍：第", input$start_col, "欄 至 第", input$end_col, "欄")
  })
  
  
  output$download_csv <- downloadHandler(
    filename = function() { paste("ranking_results_", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      final_results <- results_data()
      write.csv(final_results, file, row.names = FALSE)
    }
    
  )
  
  output$download_plot <- downloadHandler(
    filename = function() { paste("ranking_plot_", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      ggsave(file, plot = last_plot(), width = 8, height = 5, dpi = 300, bg = "white")
    }
  )
}


# 運作 app
shinyApp(ui = ui, server = server)
