library(hierarchicalFDR) ## from Schildknecht, Tabelow and Thorsten (2016) ## contains the original two-step procedure "hierasymptkappa()"
library(data.tree) ## data type for the hypotheses structure


example_ProgCode1 <- function() {
  ## load data from csv

  data <- read.csv(file = "~/DATA/LIN-Bremen/2019-11-27-Statistical Contrast Data (Bremen)/ProgCode_3mm_AllVoxel_ComprVsRest_005.csv")
  summary(data$vmp_stat)
  #     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
  # -11.1104  -0.6419   0.0000   0.6944   1.7008  13.7855


  # data2 <- read.csv(file = "~/DATA/LIN-Bremen/2019-11-27-Statistical Contrast Data (Bremen)/ProgCode_3mm_AllVoxel_ComprVsSyntax_001.csv")
  #
  # summary(data2$vmp_stat) ## bad quality (?)
  # ## output:
  # # Min.       1st Qu. Median Mean     3rd Qu. Max.      NA's
  # # -379315744 0       0      -1120841 0       377609920 11444
  #
  # sum(data2$vmp_stat == 0, na.rm = TRUE)/length(data2$vmp_stat) ## ~67%


  # data3 <- read.csv(file = "~/DATA/LIN-Bremen/2019-11-27-Statistical Contrast Data (Bremen)/ProgCode_1mm_AllVoxel_ComprVsSyntax_001.csv")
  #
  # summary(data3$vmp_stat) ## bad quality (?)
  # ## output:
  # # Min.       1st Qu. Median Mean    3rd Qu. Max.      NA's
  # # -379315744 0       0      -204071 0       377609952 322749
  #
  # sum(data3$vmp_stat == 0, na.rm = TRUE)/length(data3$vmp_stat) ## ~70%


  ## try ComprVsRest as an example

  # n <- 17 ## there are 17 participants in the first study ProgCode1
  n <- 16 ## we only have 16 nii-files in ProgCode1
  t_values <- data$vmp_stat
  p_values <- 2 * (1 - pt(abs(t_values), df = n - 1))

  summary(p_values)
  ## output:
  # Min.    1st Qu. Median  Mean    3rd Qu. Max.
  # 0.00000 0.02313 0.30585 0.41029 0.86468 1.00000

  mean(p_values <= 0.05) ## ~30%; a lot(?) of general activity;




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
  # levelName     ROI     rejected
  # 1 H31                        1
  # 2  ¦------H11 ""             1
  # 3  ¦------H12 PC_BA21        1
  # 4  ¦--H21                    1
  # 5  ¦   ¦--H13 PC_BA40        0
  # 6  ¦   ¦--H14 PC_BA44        1
  # 7  ¦   °--H15 PC_BA47        1
  # 8  °------H16 PC_BA6         0

  super_families
}