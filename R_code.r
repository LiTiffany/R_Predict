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
    sd(stand_data$X1st_credited) # �p��зǮt
    boxplot(stand_data[, c(1:44)])
    #######try

    # �ԭz�έp�q sapply()�Nsummary���Ω��ƶ����C�@���ܼơA��^�@�ӥ]�t�Ҧ��ܼƺK�n�έp�H�����C��ίx�}�C�C�C�N��@���ܼơA�ӨC��]�t���ܼƪ��K�n�έp�q�C
    sapply(data, summary) #summary:�̤p�ȡB�Ĥ@�|����ơB����ơB�����ȡB�ĤT�|����ƩM�̤j�ȡC
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


    #correlation matrix �����Y�Ưx�}
    cor(data[, 1:ncol(data) - 1])
    #�ܼƶ����������A�ҥH�n��PCA�����A��o�����p�ʧ�X�ӧ@���@�ӫ���
    #�Ӥ��O�@���ݨ���h�ܼơA�����ݼƭȾ\Ū�W���x��

    #��ı�Ƥ覡�e�{ heatmap(����) / ggplot2
    #�e���G�����Y�Ƶ��G�����Otidy data
    #�ϥ� melt / reshape2 package �N����ܦ�tidy data
    #melt()��ƥΩ�N�����ʯx�}�q�e�榡�ഫ�����榡
    #�����ʯx�}���C�ӳ椸�泣�Q�i�}���@���[��G�A�䤤�]�A����ܼƪ��W�٩M���̤����������Y�ƭ�

    head((melt(cor(data[, 1:ncol(data) - 1]))), 10) #var1 - var2 - correlation

    ggplot(melt(cor(data[, 1:ncol(data) - 1])), aes(Var1, Var2)) + #aes(Var1, Var2)���w�Fx�b�My�b�ܶq
    geom_tile(aes(fill = value), colour = "white") +
    scale_fill_gradient2(low = "red", high = "blue", #?�⺥�ܡA?��̧C�ȩM�̰��Ȭ�?��M�Ŧ�
        mid = "white", midpoint = 0) + #�����Ȭ�0�զ�
    guides(fill = guide_legend(title = "Correlation")) + #�K�[�ϨҡA����?��t�N
    theme_bw() + #�]�m�Ϫ�D�D���թ��¦�r��
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())  # x�b������45�סA���������1�]right�^�A�������?1�]top)

        #########################�R���ĤG�Ǵ���ư�����
    data1 < -read.table("dataset1_d.csv", header = T, sep = ",")
    cor(data1[, 1:ncol(data1) - 1])
    head((melt(cor(data1[, 1:ncol(data1) - 1]))), 10) #var1 - var2 - correlation
    ggplot(melt(cor(data1[, 1:40])), aes(Var1, Var2)) + #aes(Var1, Var2)���w�Fx�b�My�b�ܶq
    geom_tile(aes(fill = value), colour = "white") +
    scale_fill_gradient2(low = "red", high = "blue", #?�⺥�ܡA?��̧C�ȩM�̰��Ȭ�?��M�Ŧ�
        mid = "white", midpoint = 0) + #�����Ȭ�0�զ�
    guides(fill = guide_legend(title = "Correlation")) + #�K�[�ϨҡA����?��t�N
    theme_bw() + #�]�m�Ϫ�D�D���թ��¦�r��
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())  # x�b������45�סA���������1�]right�^�A�������?1�]top)
        ########################

    #prcomp() �q?�p��S�x�ȡB�V�q�A���ƾ��ন�s�y�Ф��A�s���ХѤ@?�D�����������ܶq�c��
    pca.model < -prcomp(data[, 1:ncol(data) - 1], scale = T)   #��pca scale = T�зǤ�
    #pca.model < -prcomp(data[, c(1:15, 21 : ncol(data) - 1)], scale = TRUE)

    names(pca.model)  #names()�i�H�d�ݹ�H�Ҧ��������W�٪��r�ŦV�q�Aeg:"sdev" "rotation" "center" "scale" "x"
    summary(pca.model)
    pca.model

    #�~�Y��(Scree plot) - �Ͳ��h
    plot(pca.model, type = "line", main = "Scree Plot")
    # ���Žu�ХܥX�S�x�� = 1���a��
    abline(h = 1, col = "blue") # Kaiser eigenvalue - greater - than - one rule


    #�ֿn������ҵe����
    cum_prop = cumsum((pca.model$sdev) ^ 2 / sum((pca.model$sdev) ^ 2))
    plot(cum_prop, main = "Cum_Prop")
    abline(h = 0.8, col = "blue")

    # pca$rotation �D�����Y�ơB�v��
    pca.model$rotation[, 1:20]

    # �D�������ơG�ѥD�������v���P�зǤƪ���Ưx�}�A�i�o�C�@�ӭ���b�U�ӥD����������
    top12_pca.data < -pca.model$x[, 1:20]
    top12_pca.data

    ####try �ֿn���10��
    pca.model$rotation[, 1:20]
    top10_pca.data < -pca.model$x[, 1:20]
    top10_pca.data
    #####

    top3.pca.eigenvector < -pca.model$rotation[, 1:3]
    top3.pca.eigenvector

    first.pca < -top3.pca.eigenvector[, 1]   #  �Ĥ@�D����
    second.pca < -top3.pca.eigenvector[, 2]  #  �ĤG�D����
    third.pca < -top3.pca.eigenvector[, 3]   #  �ĤT�D����


    # �Ĥ@�D�����G�Ѥp��j�Ƨǭ��ܼƪ��Y��
    first.pca[order(first.pca, decreasing = FALSE)]
    # �ϥ�dotchart�Aø�s�D�����t����(�Ы��I�ϥH��ܨC���ܶq���Y��)
    dotchart(first.pca[order(first.pca, decreasing = FALSE)], # �Ƨǫ᪺�Y��
        main = "Loading Plot for PC1", # �D���D
        xlab = "Variable Loadings", # x�b�����D
        col = "red")                                        # �C��

    #�G���� 1:2->PC1 : PC2
    biplot(pca.model, choices = 1:2)



    #�z�L�����ܲ� / �ֿn������v�Ϩӿ���D�����G
    #var�G�ӥD���������ܲ��ƪ��ƭ�
    #prop�G�ӥD���������ܲ��ƪ���v = PC �ܲ��� / �`�ܲ�
    #cum_prop�G�ӥD���������ܲ��ƪ��ֿn��v

    #�ѩ� pca.model �u�����X�C�@�� PC ���зǮt pca.model$sdev�A�ҥH�ڭ̻ݭn���إߥH�U���A�p��X�W�z�U���ƭȡC

    var.exp < -tibble(
        pc = paste0("PC_", formatC(1:18, width = 2, flag = "0")), #paste0()�զXstring�BformatC()�榡�Ʀ���줸�Bflag = "0"���w�b�Ʀr�p�����?�ɹs
        var = pca.model$sdev ^ 2, #�зǮt����
        prop = (pca.model$sdev) ^ 2 / sum((pca.model$sdev) ^ 2), #�C�ӥD�����������v��[�зǮt���� / �����зǮt����]
        cum_prop = cumsum((pca.model$sdev) ^ 2 / sum((pca.model$sdev) ^ 2))) #�ֿn���

    head(var.exp, 20)

    #�q�W�z���R�A��ĳ���e�Q�ӥD�����Y�i�C
    #��ݨC�@�ӥD�������Y�ơA�H�K�����D�������e�C�p�G�Q�n��X�D�������Y�Ưx�}�A�i�H�z�L pca.model$rotation�C
    head(pca.model$rotation, 20)

    #Heatmap(����)�X�D�����v��(�Y��)
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


