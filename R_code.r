install.packages("stringi")
# tidyverse library
install.packages("tidyverse")
library("tidyverse")

install.packages("reshape2")
library(reshape2)

#Set working directory
setwd("C:/Users/Administrator/Desktop/R/PCA/Work")

#Read data
data < -read.table("dataset1.csv", header = T, sep = ",")
    head(data)

    #######standardization
    stand_data < -scale(data[, c(1:44)])
    summary(stand_data)
    sd(stand_data$X1st_credited) # 計算標準差
    boxplot(stand_data[, c(1:44)])
    #######try

    # 敘述統計量 sapply()將summary應用於資料集的每一個變數，返回一個包含所有變數摘要統計信息的列表或矩陣。每列代表一個變數，而每行包含該變數的摘要統計量。
    sapply(data, summary) #summary:最小值、第一四分位數、中位數、平均值、第三四分位數和最大值。
    boxplot(data[, c(1:44)])
    boxplot(data[, c(1)])
    #boxplot(data[, c(44)])

############factor
data$Application_mode_translate <- factor(data$Application_mode_translate, labels = c("1st_phase", "2nd phase", "Over_23_years_old", "Other"))
data$Course_translate <- factor(data$Course_translate, labels = c("Arts", "Sciences", "Other"))
data$Mother_qualification_translate <- factor(data$Mother_qualification_translate, labels = c("Secondary_education", "Higher_Education", "Administration", "Other"))
data$Father_qualification_translate <- factor(data$Father_qualification_translate, labels = c("Secondary_education", "Higher_Education", "Administration", "Other"))
data$Mother_occupation_translate <- factor(data$Mother_occupation_translate, labels = c("Scholars", "farmers", "Workers", "Businessmans", "Military", "Other"))
data$Father_occupation_translate <- factor(data$Father_occupation_translate, labels = c("Scholars", "farmers", "Workers", "Businessmans", "Military", "Other"))
data$Target <- factor(data$Target, labels = c("Dropout", "Graduate", "Enrolled"))
head(data)
str(data)
Mother_occupation.tab <- table(data$Mother_occupation_translate)
Mother_occupation.tab
Target.tab <- table(data$Target)
Target.tab
prop.table(Mother_occupation.tab)
round(prop.table(Mother_occupation.tab), 3)
barplot(Mother_occupation.tab)
prop.table(Target.tab)
round(prop.table(Target.tab), 3)
barplot(Target.tab)

twoway.tab <- table(data$Target, data$Mother_occupation_translate)
twoway.tab
cell.prop <- prop.table(twoway.tab, margin = NULL) 
round(cell.prop, 3)
cond_row_prop <- prop.table(twoway.tab, margin = 1) 
round(cond_row_prop, 3)
apply(cond_row_prop, 1, sum) # rows sum to 1
cond_col_prop <- prop.table(twoway.tab, margin = 2) 
round(cond_col_prop, 3)
apply(cond_col_prop, 2, sum) # cols sum to 1
barplot(twoway.tab, 
        beside = TRUE, 
        main = "Target by Mother_occupation",
        xlab = "Mother occupation type")
barplot(twoway.tab,
        beside = FALSE, 
        main = "Target by Mother_occupation",
        xlab = "Mother occupation type")

ggplot(data = data, aes(x = Mother_occupation_translate, fill = Target)) + 
  geom_bar()


    #correlation matrix 相關係數矩陣
    cor(data[, 1:ncol(data) - 1])
    #變數間彼此相關，所以要用PCA降維，把這些關聯性抓出來作為一個指標
    #而不是一次看那麼多變數，但光看數值閱讀上有困難

    #視覺化方式呈現 heatmap(熱圖) / ggplot2
    #前提：相關係數結果必須是tidy data
    #使用 melt / reshape2 package 將資料變成tidy data
    #melt()函數用於將相關性矩陣從寬格式轉換為長格式
    #相關性矩陣的每個單元格都被展開為一個觀察結果，其中包括兩個變數的名稱和它們之間的相關係數值

    head((melt(cor(data[, 1:ncol(data) - 1]))), 10) #var1 - var2 - correlation

    ggplot(melt(cor(data[, 1:ncol(data) - 1])), aes(Var1, Var2)) + #aes(Var1, Var2)指定了x軸和y軸變量
    geom_tile(aes(fill = value), colour = "white") +
    scale_fill_gradient2(low = "red", high = "blue", #?色漸變，?色最低值和最高值為?色和藍色
        mid = "white", midpoint = 0) + #中間值為0白色
    guides(fill = guide_legend(title = "Correlation")) + #添加圖例，解釋?色含意
    theme_bw() + #設置圖表主題為白底黑色字體
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())  # x軸的旋轉45度，水平對齊為1（right），垂直對齊?1（top)

        #########################刪掉第二學期資料做熱圖
    data1 < -read.table("dataset1_d.csv", header = T, sep = ",")
    cor(data1[, 1:ncol(data1) - 1])
    head((melt(cor(data1[, 1:ncol(data1) - 1]))), 10) #var1 - var2 - correlation
    ggplot(melt(cor(data1[, 1:40])), aes(Var1, Var2)) + #aes(Var1, Var2)指定了x軸和y軸變量
    geom_tile(aes(fill = value), colour = "white") +
    scale_fill_gradient2(low = "red", high = "blue", #?色漸變，?色最低值和最高值為?色和藍色
        mid = "white", midpoint = 0) + #中間值為0白色
    guides(fill = guide_legend(title = "Correlation")) + #添加圖例，解釋?色含意
    theme_bw() + #設置圖表主題為白底黑色字體
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())  # x軸的旋轉45度，水平對齊為1（right），垂直對齊?1（top)
        ########################

    #prcomp() 通?計算特徵值、向量，把原數據轉成新座標中，新坐標由一?主成分的正交變量構成
    pca.model < -prcomp(data[, 1:ncol(data) - 1], scale = T)   #做pca scale = T標準化
    #pca.model < -prcomp(data[, c(1:15, 21 : ncol(data) - 1)], scale = TRUE)

    names(pca.model)  #names()可以查看對象所有成分的名稱的字符向量，eg:"sdev" "rotation" "center" "scale" "x"
    summary(pca.model)
    pca.model

    #陡坡圖(Scree plot) - 凱莎原則
    plot(pca.model, type = "line", main = "Scree Plot")
    # 用藍線標示出特徵值 = 1的地方
    abline(h = 1, col = "blue") # Kaiser eigenvalue - greater - than - one rule


    #累積解釋比例畫成圖
    cum_prop = cumsum((pca.model$sdev) ^ 2 / sum((pca.model$sdev) ^ 2))
    plot(cum_prop, main = "Cum_Prop")
    abline(h = 0.8, col = "blue")

    # pca$rotation 主成分係數、權重
    pca.model$rotation[, 1:20]

    # 主成分分數：由主成分的權重與標準化的資料矩陣，可得每一個個體在各個主成分的分數
    top12_pca.data < -pca.model$x[, 1:20]
    top12_pca.data

    ####try 累積比例10個
    pca.model$rotation[, 1:20]
    top10_pca.data < -pca.model$x[, 1:20]
    top10_pca.data
    #####

    top3.pca.eigenvector < -pca.model$rotation[, 1:3]
    top3.pca.eigenvector

    first.pca < -top3.pca.eigenvector[, 1]   #  第一主成份
    second.pca < -top3.pca.eigenvector[, 2]  #  第二主成份
    third.pca < -top3.pca.eigenvector[, 3]   #  第三主成份


    # 第一主成份：由小到大排序原變數的係數
    first.pca[order(first.pca, decreasing = FALSE)]
    # 使用dotchart，繪製主成份負荷圖(創建點圖以顯示每個變量的係數)
    dotchart(first.pca[order(first.pca, decreasing = FALSE)], # 排序後的係數
        main = "Loading Plot for PC1", # 主標題
        xlab = "Variable Loadings", # x軸的標題
        col = "red")                                        # 顏色

    #二元圖 1:2->PC1 : PC2
    biplot(pca.model, choices = 1:2)



    #透過解釋變異 / 累積解釋比率圖來選取主成份：
    #var：該主成份解釋變異數的數值
    #prop：該主成份解釋變異數的比率 = PC 變異數 / 總變異
    #cum_prop：該主成份解釋變異數的累積比率

    #由於 pca.model 只能夠抓出每一個 PC 的標準差 pca.model$sdev，所以我們需要先建立以下表格，計算出上述各項數值。

    var.exp < -tibble(
        pc = paste0("PC_", formatC(1:18, width = 2, flag = "0")), #paste0()組合string、formatC()格式化成兩位元、flag = "0"指定在數字小於兩位數?補零
        var = pca.model$sdev ^ 2, #標準差平方
        prop = (pca.model$sdev) ^ 2 / sum((pca.model$sdev) ^ 2), #每個主成分的佔比權重[標準差平方 / 全部標準差平方]
        cum_prop = cumsum((pca.model$sdev) ^ 2 / sum((pca.model$sdev) ^ 2))) #累積比例

    head(var.exp, 20)

    #從上述分析，建議取前十個主成份即可。
    #察看每一個主成份的係數，以便解釋主成份內容。如果想要找出主成份的係數矩陣，可以透過 pca.model$rotation。
    head(pca.model$rotation, 20)

    #Heatmap(熱圖)—主成分權重(係數)
    ggplot(melt(pca.model$rotation[, 1:20]), aes(Var2, Var1)) +
    geom_tile(aes(fill = value), colour = "white") +
    scale_fill_gradient2(low = "firebrick4", high = "steelblue",
        mid = "white", midpoint = 0) +
    guides(fill = guide_legend(title = "Coefficient")) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())

    # Classification: PCA1 vs.PCA2
    table(data[, 1])
    plot(pca.model$x[1:1500, 1], pca.model$x[1:1500, 2],
        xlab = "PC 1", ylab = "PC 2",
        xlim = range(pca.model$x[, 1]), ylim = range(pca.model$x[, 2]), col = "red")
    points(pca.model$x[1501:3000, 1], pca.model$x[1501:3000, 2], col = "blue")
    points(pca.model$x[3001:4424, 1], pca.model$x[3001:4424, 2], col = "green")



    install.packages("nsprcomp")
    library(nsprcomp)

    #set.seed(5438)
    #nspca = nscumcomp(data[, 2:14], k = 65, nneg = T, scale. = T)
    #var.exp < -tibble(
        #  pc = paste0("PC_", formatC(1:13, width = 2, flag = "0")),
        #  var = nspca$sdev ^ 2,
        #  prop = (nspca$sdev) ^ 2 / sum((nspca$sdev) ^ 2),
        #  cum_prop = cumsum((nspca$sdev) ^ 2 / sum((nspca$sdev) ^ 2)))


