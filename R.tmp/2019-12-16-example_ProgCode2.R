library(hierarchicalFDR) ## from Schildknecht, Tabelow and Thorsten (2016) ## contains the original two-step procedure "hierasymptkappa()"
library(data.tree) ## data type for the hypotheses structure


example_ProgCode2 <- function() {
  ## load data from csv

  data <- read.csv(file = "~/DATA/LIN-Bremen/2019-11-27-Statistical Contrast Data (Bremen)/ProgCode2_3mm_AllVoxel_BottomUp_005.csv")
  # summary(data$vmp_stat)
  #     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
  # -5.63190  0.00000  0.00000  0.07035  0.00000 18.03707
  # sum(data$vmp_stat == 0, na.rm = TRUE)/length(data$vmp_stat) ## ~21%

  # data2 <- read.csv(file = "~/DATA/LIN-Bremen/2019-11-27-Statistical Contrast Data (Bremen)/ProgCode2_3mm_AllVoxel_BottomUpVsSyntax_005.csv")
  # summary(data2$vmp_stat) ## bad quality (?); are these data from stage two (?);
  # #     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
  # # -6.58589  0.00000  0.00000  0.02665  0.00000  5.51951
  # sum(data2$vmp_stat == 0, na.rm = TRUE)/length(data2$vmp_stat) ## ~97%

  data3 <- read.csv(file = "~/DATA/LIN-Bremen/2019-11-27-Statistical Contrast Data (Bremen)/ProgCode2_3mm_AllVoxel_ComprVsRest_005.csv")
  # summary(data3$vmp_stat)
  #      Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
  # -18.38141   0.00000   0.00000   0.33909   0.00751  40.43317
  # sum(data3$vmp_stat == 0, na.rm = TRUE)/length(data3$vmp_stat) ## ~56%

  # data4 <- read.csv(file = "~/DATA/LIN-Bremen/2019-11-27-Statistical Contrast Data (Bremen)/ProgCode2_3mm_AllVoxel_Syntax_005.csv")
  # summary(data4$vmp_stat) ## bad quality (?); are these data from stage two (?);
  # #     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
  # # -6.30304  0.00000  0.00000  0.04305  0.00000 12.54716
  # sum(data4$vmp_stat == 0, na.rm = TRUE)/length(data4$vmp_stat) ## ~97%

  # data5 <- read.csv(file = "~/DATA/LIN-Bremen/2019-11-27-Statistical Contrast Data (Bremen)/ProgCode2_3mm_AllVoxel_TopDown_005.csv")
  # summary(data5$vmp_stat) ## bad quality (?); are these data from stage two (?);
  # #      Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
  # # -8.968070  0.000000  0.000000  0.009346  0.000000 12.346010
  # sum(data5$vmp_stat == 0, na.rm = TRUE)/length(data5$vmp_stat) ## ~97%



  ## try BottomUp as an example

  n <- 11 ## there are 11 participants in the second study ProgCode2
  t_values <- data$vmp_stat
  # t_values <- data3$vmp_stat ## this gives similar results; the p-values are skewed;
  p_values <- 2 * (1 - pt(abs(t_values), df = n - 1))

  summary(p_values)
  ## output:
  #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
  # 0.0000  1.0000  1.0000  0.9797  1.0000  1.0000
  ## the p-values are highly skewed (mean=0.9797);

  mean(p_values <= 0.05) ## ~1%;




  ## create hierarchical hypotheses structure
  ## only an example

  super_families <- Node$new("H31")
  super_families$AddChild("H11")
  super_families$AddChild("H12")
  H21 <- super_families$AddChild("H21")
  super_families$AddChild("H16")
  H21$AddChild("H13")
  H21$AddChild("H14")
  H21$AddChild("H15")

  ROI_levels <- levels(data$ROI)
  for (i in 1:6) {
    H <- FindNode(super_families, paste0("H1", i))
    ROI <- ROI_levels[i]
    Set(list(H), ROI = ROI, p_values = list(p_values[which(data$ROI == ROI)]))
  }

  super_families.test(super_families)

  print(super_families, "ROI", "rejected") ## displays the level-1 to level-3 hypotheses hierarchy; the ROI are the level-1 hypotheses; the individual (voxel) hypotheses are not displayed;
  ## slightly modified output to better represent the hierarchy:
  #     levelName     ROI rejected
  # 1 H31                        0
  # 2  ¦------H11 ""             0
  # 3  ¦------H12 PC_BA21        0
  # 4  ¦--H21                    0
  # 5  ¦   ¦--H13 PC_BA40        0
  # 6  ¦   ¦--H14 PC_BA44        0
  # 7  ¦   °--H15 PC_BA47        0
  # 8  °------H16 PC_BA6         0

  super_families
}