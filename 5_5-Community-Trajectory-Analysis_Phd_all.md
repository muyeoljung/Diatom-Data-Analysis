Community Trajectory Analysis (CTA)
================
Muyeol Jung

## Introduction

<https://cran.r-project.org/web/packages/vegclust/vignettes/CTA.html#simple-example>

For Govermnemt Diatom Data, the result of nMDS isn’t well-presented in
2D base or 3d base plots, so Community Trajectory Analysis is brought in
to geometrically show the direction and angle in dimensional plot.

## Data Process

Government Diatom Data 1. Load 2. Select : drop columns (HalfofYear,
Bio.Code.Name, Density) 3. Combine with Site Name and Create Stage by
year 4. Convert to wide format 5. Extract Enviornment from the data

``` r
# Load Government Data Set
Raw_GV_D <- read_csv("https://raw.githubusercontent.com/muyeoljung/Diatom-Data-Analysis/master/Gov_Diatom_Percentage.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Year = col_double(),
    ##   Half.Of.Year = col_double(),
    ##   Bio.Code = col_character(),
    ##   Bio.Code.Name = col_character(),
    ##   Species = col_character(),
    ##   Density = col_double(),
    ##   Percent = col_double()
    ## )

``` r
#In order to import data from Github, URL for data must be "directoy" for RAW


# Filter and Select
GOV_DS <- Raw_GV_D %>% select(1,2,3,5,7)

# Combine with Env by Bio Name column
GOV_D_Env <- read_csv("https://raw.githubusercontent.com/muyeoljung/Diatom-Data-Analysis/master/Studysites_20190725.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   River.Type = col_character(),
    ##   River.Name = col_character(),
    ##   New.Code.Name = col_character(),
    ##   Old.Code.Name = col_character(),
    ##   Loaction.Name = col_character(),
    ##   Lat = col_double(),
    ##   Lon = col_double(),
    ##   WQ.L.Name = col_character(),
    ##   WQ.L.Code = col_character(),
    ##   Bio.L.Name = col_character(),
    ##   Bio.L.Code = col_character(),
    ##   Fieldwork.Data = col_character(),
    ##   Water.Data = col_character(),
    ##   Bio.Data = col_character(),
    ##   Sector = col_character(),
    ##   Etc = col_character()
    ## )

``` r
GOV_D_Env <- GOV_D_Env %>% select(-8, -10, -12:-16)
colnames(GOV_D_Env)[9] <- "Bio.Code"
Comb_GOV_Data <- left_join(GOV_D_Env, GOV_DS, by="Bio.Code")

# Convert to wideformat, miscellaneous

Wide_GOV_Data <- Comb_GOV_Data %>% spread(key = Species, value = Percent) %>% select(-421)
#key = Column, value = in the table
#421 column is NA value only

Weir_Data <- Wide_GOV_Data %>% filter(River.Type=="Weir")
Wide_GOV_Data <- Wide_GOV_Data %>% filter(River.Type!="Weir") # != for filter obs not containing

a <- Wide_GOV_Data %>% select(12:420)
a[is.na(a)]<-0

Final_GOV_Data <- Wide_GOV_Data %>% select(1:11) %>% cbind(a)
rm(a)

# Change Half of Year to Spring/Autumn
Final_GOV_Data$Half.Of.Year <- as.character(Final_GOV_Data$Half.Of.Year)
Final_GOV_Data$Half.Of.Year <- factor(Final_GOV_Data$Half.Of.Year, levels = c("1", "2"),
                                      labels = c( "Spring", "Autumn" ))
  
Final_GOV_Data_M <- Final_GOV_Data %>% filter(River.Type=="M")
Final_GOV_Data_T <- Final_GOV_Data %>% filter(River.Type=="T")

#All_MY_D <- read_csv("https://raw.githubusercontent.com/muyeoljung/Diatom-Data-Analysis/master/Final_Diatom_List_forAnalysis_datainput.csv")
```

## Analysis and Result

1.  Determine dimensions
2.  Stree plot
3.  nMDS (performs Nonmetric Multidimensional scaling (metaMDS))
4.  Community Trajectory Analysis (CTA) using nMDS (You can choose
    different types of multidimensional scaling methods)
5.  Visualise them
6.  Statistical Analysis (figures)

### Determine dimensions

``` r
#Extract Env from all data (Final Gov Data Set)
Final_GOV_Env <- Final_GOV_Data %>% select(1:11)
Final_GOV_Species <- Final_GOV_Data %>% select(-1:-11)

# Determine the dimension
dimcheckMDS(Final_GOV_Species, distance="bray") #goeveg package
```

    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.433132 
    ## Run 1 stress 0.4885024 
    ## Run 2 stress 0.4949103 
    ## Run 3 stress 0.4883419 
    ## Run 4 stress 0.4822685 
    ## Run 5 stress 0.4917419 
    ## Run 6 stress 0.4936105 
    ## Run 7 stress 0.4998614 
    ## Run 8 stress 0.487818 
    ## Run 9 stress 0.4927849 
    ## Run 10 stress 0.5020316 
    ## Run 11 stress 0.498677 
    ## Run 12 stress 0.4887991 
    ## Run 13 stress 0.4910257 
    ## Run 14 stress 0.49918 
    ## Run 15 stress 0.4950504 
    ## Run 16 stress 0.4997058 
    ## Run 17 stress 0.4899717 
    ## Run 18 stress 0.4859133 
    ## Run 19 stress 0.49599 
    ## Run 20 stress 0.4948626 
    ## *** No convergence -- monoMDS stopping criteria:
    ##     19: stress ratio > sratmax
    ##      1: scale factor of the gradient < sfgrmin
    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.2914236 
    ## Run 1 stress 0.2912038 
    ## ... New best solution
    ## ... Procrustes: rmse 0.01938342  max resid 0.1055723 
    ## Run 2 stress 0.2952364 
    ## Run 3 stress 0.291008 
    ## ... New best solution
    ## ... Procrustes: rmse 0.008633016  max resid 0.1248221 
    ## Run 4 stress 0.2911858 
    ## ... Procrustes: rmse 0.01085474  max resid 0.1492336 
    ## Run 5 stress 0.2932677 
    ## Run 6 stress 0.2922082 
    ## Run 7 stress 0.2908057 
    ## ... New best solution
    ## ... Procrustes: rmse 0.01270241  max resid 0.1489727 
    ## Run 8 stress 0.2893096 
    ## ... New best solution
    ## ... Procrustes: rmse 0.009678734  max resid 0.1073613 
    ## Run 9 stress 0.2916431 
    ## Run 10 stress 0.292777 
    ## Run 11 stress 0.2898222 
    ## Run 12 stress 0.2913153 
    ## Run 13 stress 0.2915858 
    ## Run 14 stress 0.2911193 
    ## Run 15 stress 0.2917466 
    ## Run 16 stress 0.2922941 
    ## Run 17 stress 0.2944662 
    ## Run 18 stress 0.2922455 
    ## Run 19 stress 0.2914148 
    ## Run 20 stress 0.2901236 
    ## *** No convergence -- monoMDS stopping criteria:
    ##      5: no. of iterations >= maxit
    ##     15: stress ratio > sratmax
    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.2146054 
    ## Run 1 stress 0.2151185 
    ## Run 2 stress 0.2146801 
    ## ... Procrustes: rmse 0.003553965  max resid 0.07172093 
    ## Run 3 stress 0.2151238 
    ## Run 4 stress 0.2151799 
    ## Run 5 stress 0.214917 
    ## ... Procrustes: rmse 0.005698107  max resid 0.1306604 
    ## Run 6 stress 0.2163512 
    ## Run 7 stress 0.2151562 
    ## Run 8 stress 0.215171 
    ## Run 9 stress 0.2148183 
    ## ... Procrustes: rmse 0.005363134  max resid 0.128421 
    ## Run 10 stress 0.2145988 
    ## ... New best solution
    ## ... Procrustes: rmse 0.003249438  max resid 0.05089728 
    ## Run 11 stress 0.2158012 
    ## Run 12 stress 0.2147665 
    ## ... Procrustes: rmse 0.004850768  max resid 0.07481598 
    ## Run 13 stress 0.2147075 
    ## ... Procrustes: rmse 0.006617444  max resid 0.09672758 
    ## Run 14 stress 0.2149901 
    ## ... Procrustes: rmse 0.006050407  max resid 0.09009936 
    ## Run 15 stress 0.2155322 
    ## Run 16 stress 0.2149058 
    ## ... Procrustes: rmse 0.007574493  max resid 0.09763033 
    ## Run 17 stress 0.2145783 
    ## ... New best solution
    ## ... Procrustes: rmse 0.006052938  max resid 0.0944955 
    ## Run 18 stress 0.2158932 
    ## Run 19 stress 0.2149129 
    ## ... Procrustes: rmse 0.008774392  max resid 0.1333395 
    ## Run 20 stress 0.2147302 
    ## ... Procrustes: rmse 0.006447674  max resid 0.09156156 
    ## *** No convergence -- monoMDS stopping criteria:
    ##      8: no. of iterations >= maxit
    ##     12: stress ratio > sratmax
    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.1719948 
    ## Run 1 stress 0.1719578 
    ## ... New best solution
    ## ... Procrustes: rmse 0.001380964  max resid 0.02659609 
    ## Run 2 stress 0.1722584 
    ## ... Procrustes: rmse 0.005763241  max resid 0.09624652 
    ## Run 3 stress 0.1720494 
    ## ... Procrustes: rmse 0.004236849  max resid 0.07962047 
    ## Run 4 stress 0.1719171 
    ## ... New best solution
    ## ... Procrustes: rmse 0.001975009  max resid 0.02134131 
    ## Run 5 stress 0.1719364 
    ## ... Procrustes: rmse 0.001344319  max resid 0.02592632 
    ## Run 6 stress 0.1720438 
    ## ... Procrustes: rmse 0.002474079  max resid 0.05852101 
    ## Run 7 stress 0.1757901 
    ## Run 8 stress 0.172234 
    ## ... Procrustes: rmse 0.005741116  max resid 0.09439627 
    ## Run 9 stress 0.1722445 
    ## ... Procrustes: rmse 0.005480042  max resid 0.09425804 
    ## Run 10 stress 0.1720166 
    ## ... Procrustes: rmse 0.002390638  max resid 0.04936393 
    ## Run 11 stress 0.17196 
    ## ... Procrustes: rmse 0.001688929  max resid 0.02153864 
    ## Run 12 stress 0.1721276 
    ## ... Procrustes: rmse 0.005886194  max resid 0.1284031 
    ## Run 13 stress 0.1719348 
    ## ... Procrustes: rmse 0.001074377  max resid 0.01553434 
    ## Run 14 stress 0.1720233 
    ## ... Procrustes: rmse 0.003383231  max resid 0.05190014 
    ## Run 15 stress 0.1723599 
    ## ... Procrustes: rmse 0.006293055  max resid 0.09534565 
    ## Run 16 stress 0.1719417 
    ## ... Procrustes: rmse 0.00144237  max resid 0.01781362 
    ## Run 17 stress 0.1741208 
    ## Run 18 stress 0.1727364 
    ## Run 19 stress 0.1719739 
    ## ... Procrustes: rmse 0.002406854  max resid 0.04324251 
    ## Run 20 stress 0.1719794 
    ## ... Procrustes: rmse 0.00207116  max resid 0.03839624 
    ## *** No convergence -- monoMDS stopping criteria:
    ##     11: no. of iterations >= maxit
    ##      9: stress ratio > sratmax
    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.1457021 
    ## Run 1 stress 0.1460711 
    ## ... Procrustes: rmse 0.005250535  max resid 0.06194441 
    ## Run 2 stress 0.1467357 
    ## Run 3 stress 0.1458596 
    ## ... Procrustes: rmse 0.002880577  max resid 0.05073953 
    ## Run 4 stress 0.1461129 
    ## ... Procrustes: rmse 0.008513533  max resid 0.07654044 
    ## Run 5 stress 0.1457329 
    ## ... Procrustes: rmse 0.002003383  max resid 0.02563593 
    ## Run 6 stress 0.1457599 
    ## ... Procrustes: rmse 0.001904553  max resid 0.03772841 
    ## Run 7 stress 0.1509291 
    ## Run 8 stress 0.1469315 
    ## Run 9 stress 0.1460534 
    ## ... Procrustes: rmse 0.005441584  max resid 0.07071742 
    ## Run 10 stress 0.1462901 
    ## Run 11 stress 0.1457155 
    ## ... Procrustes: rmse 0.003407822  max resid 0.05922125 
    ## Run 12 stress 0.1464346 
    ## Run 13 stress 0.1458459 
    ## ... Procrustes: rmse 0.004353577  max resid 0.06351113 
    ## Run 14 stress 0.1457219 
    ## ... Procrustes: rmse 0.00374269  max resid 0.03681599 
    ## Run 15 stress 0.1458992 
    ## ... Procrustes: rmse 0.003959272  max resid 0.07615305 
    ## Run 16 stress 0.1457343 
    ## ... Procrustes: rmse 0.003060846  max resid 0.03951853 
    ## Run 17 stress 0.1464366 
    ## Run 18 stress 0.1460365 
    ## ... Procrustes: rmse 0.005817596  max resid 0.06053571 
    ## Run 19 stress 0.1458428 
    ## ... Procrustes: rmse 0.003902971  max resid 0.04315282 
    ## Run 20 stress 0.1461944 
    ## ... Procrustes: rmse 0.005545948  max resid 0.07027551 
    ## *** No convergence -- monoMDS stopping criteria:
    ##     18: no. of iterations >= maxit
    ##      2: stress ratio > sratmax
    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.1291016 
    ## Run 1 stress 0.1310823 
    ## Run 2 stress 0.1336814 
    ## Run 3 stress 0.1296033 
    ## Run 4 stress 0.1303055 
    ## Run 5 stress 0.1747033 
    ## Run 6 stress 0.1297804 
    ## Run 7 stress 0.1299251 
    ## Run 8 stress 0.1296314 
    ## Run 9 stress 0.12934 
    ## ... Procrustes: rmse 0.006901045  max resid 0.04746225 
    ## Run 10 stress 0.1297089 
    ## Run 11 stress 0.1300952 
    ## Run 12 stress 0.1314207 
    ## Run 13 stress 0.129871 
    ## Run 14 stress 0.1296516 
    ## Run 15 stress 0.1294379 
    ## ... Procrustes: rmse 0.007811981  max resid 0.07061638 
    ## Run 16 stress 0.1303127 
    ## Run 17 stress 0.1299886 
    ## Run 18 stress 0.1304226 
    ## Run 19 stress 0.1298619 
    ## Run 20 stress 0.1295792 
    ## ... Procrustes: rmse 0.007468687  max resid 0.05621489 
    ## *** No convergence -- monoMDS stopping criteria:
    ##     20: no. of iterations >= maxit

![](5_5-Community-Trajectory-Analysis_Phd_all_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

    ## [1] 0.4331320 0.2893096 0.2145783 0.1719171 0.1457021 0.1291016

Stress value is 0.215, just over the boundary at 3 dimension. As a
result, 3 dimensional space is selected for nMDS with extra care in
interpretation.

### Stress plot for Main stream and nMDS

``` r
#meta MDS: non Metrical multidimensional scaling
Final_ordination <- metaMDS(Final_GOV_Species, distance = "bray", k=3)
```

    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.2146054 
    ## Run 1 stress 0.215157 
    ## Run 2 stress 0.2151877 
    ## Run 3 stress 0.2160019 
    ## Run 4 stress 0.2148139 
    ## ... Procrustes: rmse 0.006214192  max resid 0.07732548 
    ## Run 5 stress 0.2148003 
    ## ... Procrustes: rmse 0.005600188  max resid 0.09201308 
    ## Run 6 stress 0.2151899 
    ## Run 7 stress 0.2146152 
    ## ... Procrustes: rmse 0.004204313  max resid 0.09098191 
    ## Run 8 stress 0.215042 
    ## ... Procrustes: rmse 0.0075605  max resid 0.09755343 
    ## Run 9 stress 0.2146537 
    ## ... Procrustes: rmse 0.004106165  max resid 0.09668192 
    ## Run 10 stress 0.2146506 
    ## ... Procrustes: rmse 0.006923105  max resid 0.1012497 
    ## Run 11 stress 0.2146984 
    ## ... Procrustes: rmse 0.005638913  max resid 0.0911802 
    ## Run 12 stress 0.222121 
    ## Run 13 stress 0.2156746 
    ## Run 14 stress 0.2158413 
    ## Run 15 stress 0.2149291 
    ## ... Procrustes: rmse 0.004328694  max resid 0.07449308 
    ## Run 16 stress 0.2146146 
    ## ... Procrustes: rmse 0.002289409  max resid 0.03807424 
    ## Run 17 stress 0.2149246 
    ## ... Procrustes: rmse 0.005973889  max resid 0.1305185 
    ## Run 18 stress 0.2149707 
    ## ... Procrustes: rmse 0.008897028  max resid 0.1334442 
    ## Run 19 stress 0.2148627 
    ## ... Procrustes: rmse 0.006049945  max resid 0.09091842 
    ## Run 20 stress 0.2153743 
    ## *** No convergence -- monoMDS stopping criteria:
    ##      5: no. of iterations >= maxit
    ##     15: stress ratio > sratmax

``` r
stressplot(Final_ordination)
```

![](5_5-Community-Trajectory-Analysis_Phd_all_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

What are the line

### Stress plot for Main / Tributaries and nMDS

``` r
# Mainstream Stress value
m <- Final_GOV_Data_M %>% select(-1:-11)
dimcheckMDS(m)
```

    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.4479103 
    ## Run 1 stress 0.4818712 
    ## Run 2 stress 0.4831653 
    ## Run 3 stress 0.4787711 
    ## Run 4 stress 0.4892884 
    ## Run 5 stress 0.4711173 
    ## Run 6 stress 0.4675753 
    ## Run 7 stress 0.4872697 
    ## Run 8 stress 0.4820719 
    ## Run 9 stress 0.4901416 
    ## Run 10 stress 0.4814379 
    ## Run 11 stress 0.472045 
    ## Run 12 stress 0.4874207 
    ## Run 13 stress 0.4858536 
    ## Run 14 stress 0.4875336 
    ## Run 15 stress 0.4863844 
    ## Run 16 stress 0.4827602 
    ## Run 17 stress 0.4875123 
    ## Run 18 stress 0.4865272 
    ## Run 19 stress 0.4907815 
    ## Run 20 stress 0.4769529 
    ## *** No convergence -- monoMDS stopping criteria:
    ##     20: stress ratio > sratmax
    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.2881016 
    ## Run 1 stress 0.296632 
    ## Run 2 stress 0.2917223 
    ## Run 3 stress 0.2929434 
    ## Run 4 stress 0.2931131 
    ## Run 5 stress 0.2890845 
    ## Run 6 stress 0.2936563 
    ## Run 7 stress 0.2969513 
    ## Run 8 stress 0.2912435 
    ## Run 9 stress 0.2893718 
    ## Run 10 stress 0.2913866 
    ## Run 11 stress 0.2945147 
    ## Run 12 stress 0.2932681 
    ## Run 13 stress 0.2930369 
    ## Run 14 stress 0.2929726 
    ## Run 15 stress 0.3015958 
    ## Run 16 stress 0.2918969 
    ## Run 17 stress 0.2977299 
    ## Run 18 stress 0.288328 
    ## ... Procrustes: rmse 0.01553035  max resid 0.1413815 
    ## Run 19 stress 0.2898179 
    ## Run 20 stress 0.2907877 
    ## *** No convergence -- monoMDS stopping criteria:
    ##      6: no. of iterations >= maxit
    ##     14: stress ratio > sratmax
    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.2100616 
    ## Run 1 stress 0.2100787 
    ## ... Procrustes: rmse 0.003638376  max resid 0.03691256 
    ## Run 2 stress 0.2101821 
    ## ... Procrustes: rmse 0.006120373  max resid 0.0853682 
    ## Run 3 stress 0.2175483 
    ## Run 4 stress 0.2101934 
    ## ... Procrustes: rmse 0.006200475  max resid 0.08625031 
    ## Run 5 stress 0.2100561 
    ## ... New best solution
    ## ... Procrustes: rmse 0.003131731  max resid 0.04210402 
    ## Run 6 stress 0.2101033 
    ## ... Procrustes: rmse 0.004774737  max resid 0.06695047 
    ## Run 7 stress 0.210093 
    ## ... Procrustes: rmse 0.00515087  max resid 0.05802771 
    ## Run 8 stress 0.2103558 
    ## ... Procrustes: rmse 0.01042572  max resid 0.092145 
    ## Run 9 stress 0.2100339 
    ## ... New best solution
    ## ... Procrustes: rmse 0.002556159  max resid 0.02478146 
    ## Run 10 stress 0.210063 
    ## ... Procrustes: rmse 0.005957996  max resid 0.07770037 
    ## Run 11 stress 0.2108185 
    ## Run 12 stress 0.2111485 
    ## Run 13 stress 0.2103393 
    ## ... Procrustes: rmse 0.01068637  max resid 0.09831056 
    ## Run 14 stress 0.210099 
    ## ... Procrustes: rmse 0.006373717  max resid 0.07546072 
    ## Run 15 stress 0.2101164 
    ## ... Procrustes: rmse 0.004670772  max resid 0.040052 
    ## Run 16 stress 0.2100822 
    ## ... Procrustes: rmse 0.004935057  max resid 0.07194106 
    ## Run 17 stress 0.2101031 
    ## ... Procrustes: rmse 0.005694485  max resid 0.07864961 
    ## Run 18 stress 0.210053 
    ## ... Procrustes: rmse 0.002723251  max resid 0.01792865 
    ## Run 19 stress 0.2101284 
    ## ... Procrustes: rmse 0.006700415  max resid 0.07100243 
    ## Run 20 stress 0.2100986 
    ## ... Procrustes: rmse 0.004753649  max resid 0.06369187 
    ## *** No convergence -- monoMDS stopping criteria:
    ##      6: no. of iterations >= maxit
    ##     14: stress ratio > sratmax
    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.1647919 
    ## Run 1 stress 0.1654189 
    ## Run 2 stress 0.1649173 
    ## ... Procrustes: rmse 0.005482953  max resid 0.06286673 
    ## Run 3 stress 0.1651194 
    ## ... Procrustes: rmse 0.006567385  max resid 0.0681494 
    ## Run 4 stress 0.1649153 
    ## ... Procrustes: rmse 0.005098579  max resid 0.07383223 
    ## Run 5 stress 0.1654116 
    ## Run 6 stress 0.1648111 
    ## ... Procrustes: rmse 0.002855076  max resid 0.03740027 
    ## Run 7 stress 0.1653983 
    ## Run 8 stress 0.1650282 
    ## ... Procrustes: rmse 0.005580697  max resid 0.07125246 
    ## Run 9 stress 0.1648183 
    ## ... Procrustes: rmse 0.002795585  max resid 0.03962306 
    ## Run 10 stress 0.1648258 
    ## ... Procrustes: rmse 0.003329298  max resid 0.02275359 
    ## Run 11 stress 0.1648104 
    ## ... Procrustes: rmse 0.00227943  max resid 0.02040008 
    ## Run 12 stress 0.165684 
    ## Run 13 stress 0.1657175 
    ## Run 14 stress 0.1650146 
    ## ... Procrustes: rmse 0.005219485  max resid 0.07222852 
    ## Run 15 stress 0.1648144 
    ## ... Procrustes: rmse 0.002671008  max resid 0.03021459 
    ## Run 16 stress 0.1650626 
    ## ... Procrustes: rmse 0.006163352  max resid 0.08185436 
    ## Run 17 stress 0.1649305 
    ## ... Procrustes: rmse 0.005388351  max resid 0.07848912 
    ## Run 18 stress 0.16484 
    ## ... Procrustes: rmse 0.003752767  max resid 0.04940323 
    ## Run 19 stress 0.165731 
    ## Run 20 stress 0.1654283 
    ## *** No convergence -- monoMDS stopping criteria:
    ##      3: no. of iterations >= maxit
    ##     17: stress ratio > sratmax
    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.1396927 
    ## Run 1 stress 0.1422168 
    ## Run 2 stress 0.1396571 
    ## ... New best solution
    ## ... Procrustes: rmse 0.004933222  max resid 0.04015183 
    ## Run 3 stress 0.1401957 
    ## Run 4 stress 0.1395364 
    ## ... New best solution
    ## ... Procrustes: rmse 0.009801454  max resid 0.1433701 
    ## Run 5 stress 0.140674 
    ## Run 6 stress 0.1421565 
    ## Run 7 stress 0.1396709 
    ## ... Procrustes: rmse 0.005443329  max resid 0.05485612 
    ## Run 8 stress 0.1397184 
    ## ... Procrustes: rmse 0.007859897  max resid 0.04605411 
    ## Run 9 stress 0.1396251 
    ## ... Procrustes: rmse 0.01261308  max resid 0.1259987 
    ## Run 10 stress 0.1396575 
    ## ... Procrustes: rmse 0.01384351  max resid 0.1374991 
    ## Run 11 stress 0.1414313 
    ## Run 12 stress 0.1397175 
    ## ... Procrustes: rmse 0.00942988  max resid 0.1455916 
    ## Run 13 stress 0.1398366 
    ## ... Procrustes: rmse 0.01061915  max resid 0.1179655 
    ## Run 14 stress 0.1395897 
    ## ... Procrustes: rmse 0.008841442  max resid 0.04712704 
    ## Run 15 stress 0.141254 
    ## Run 16 stress 0.1395471 
    ## ... Procrustes: rmse 0.01238127  max resid 0.1424299 
    ## Run 17 stress 0.1429316 
    ## Run 18 stress 0.1403071 
    ## Run 19 stress 0.1400116 
    ## ... Procrustes: rmse 0.01369381  max resid 0.1617771 
    ## Run 20 stress 0.1408947 
    ## *** No convergence -- monoMDS stopping criteria:
    ##     14: no. of iterations >= maxit
    ##      6: stress ratio > sratmax
    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.1222322 
    ## Run 1 stress 0.123078 
    ## Run 2 stress 0.1230542 
    ## Run 3 stress 0.1234329 
    ## Run 4 stress 0.1233271 
    ## Run 5 stress 0.1226048 
    ## ... Procrustes: rmse 0.01464112  max resid 0.07589281 
    ## Run 6 stress 0.1229364 
    ## Run 7 stress 0.1231567 
    ## Run 8 stress 0.1230912 
    ## Run 9 stress 0.1229655 
    ## Run 10 stress 0.1234462 
    ## Run 11 stress 0.1230506 
    ## Run 12 stress 0.1228271 
    ## Run 13 stress 0.1243617 
    ## Run 14 stress 0.1232002 
    ## Run 15 stress 0.1235319 
    ## Run 16 stress 0.1236987 
    ## Run 17 stress 0.123088 
    ## Run 18 stress 0.1225169 
    ## ... Procrustes: rmse 0.01187736  max resid 0.06212972 
    ## Run 19 stress 0.123221 
    ## Run 20 stress 0.1236403 
    ## *** No convergence -- monoMDS stopping criteria:
    ##     20: no. of iterations >= maxit

![](5_5-Community-Trajectory-Analysis_Phd_all_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

    ## [1] 0.4479103 0.2881016 0.2100339 0.1647919 0.1395364 0.1222322

``` r
m_ordination <- metaMDS(m, distance = "bray", k=3)
```

    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.2100616 
    ## Run 1 stress 0.2103562 
    ## ... Procrustes: rmse 0.007956162  max resid 0.1012348 
    ## Run 2 stress 0.2127094 
    ## Run 3 stress 0.2102288 
    ## ... Procrustes: rmse 0.00821452  max resid 0.101191 
    ## Run 4 stress 0.2100511 
    ## ... New best solution
    ## ... Procrustes: rmse 0.005076665  max resid 0.04574368 
    ## Run 5 stress 0.2100721 
    ## ... Procrustes: rmse 0.006111017  max resid 0.06123028 
    ## Run 6 stress 0.2100945 
    ## ... Procrustes: rmse 0.006221707  max resid 0.08241918 
    ## Run 7 stress 0.2100797 
    ## ... Procrustes: rmse 0.006450744  max resid 0.07446589 
    ## Run 8 stress 0.2100512 
    ## ... Procrustes: rmse 0.002265827  max resid 0.01981537 
    ## Run 9 stress 0.2100481 
    ## ... New best solution
    ## ... Procrustes: rmse 0.00348821  max resid 0.02902891 
    ## Run 10 stress 0.2100976 
    ## ... Procrustes: rmse 0.004544924  max resid 0.0609238 
    ## Run 11 stress 0.2103522 
    ## ... Procrustes: rmse 0.009186125  max resid 0.1117236 
    ## Run 12 stress 0.2132625 
    ## Run 13 stress 0.2100701 
    ## ... Procrustes: rmse 0.004764446  max resid 0.04200418 
    ## Run 14 stress 0.2103475 
    ## ... Procrustes: rmse 0.009816576  max resid 0.1013399 
    ## Run 15 stress 0.2100559 
    ## ... Procrustes: rmse 0.003543733  max resid 0.03026716 
    ## Run 16 stress 0.2102152 
    ## ... Procrustes: rmse 0.005051573  max resid 0.06571945 
    ## Run 17 stress 0.21022 
    ## ... Procrustes: rmse 0.008184811  max resid 0.1131328 
    ## Run 18 stress 0.210101 
    ## ... Procrustes: rmse 0.005091931  max resid 0.06942153 
    ## Run 19 stress 0.210126 
    ## ... Procrustes: rmse 0.003527742  max resid 0.02446172 
    ## Run 20 stress 0.2100654 
    ## ... Procrustes: rmse 0.00509374  max resid 0.05746086 
    ## *** No convergence -- monoMDS stopping criteria:
    ##      5: no. of iterations >= maxit
    ##     15: stress ratio > sratmax

``` r
stressplot(m_ordination)
```

![](5_5-Community-Trajectory-Analysis_Phd_all_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
# m (alone): stress value: 0.21 at 3 dimension

# Tributaries Stress value
t <- Final_GOV_Data_T %>% select(-1:-11)
dimcheckMDS(t)
```

    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.433967 
    ## Run 1 stress 0.4951689 
    ## Run 2 stress 0.4897489 
    ## Run 3 stress 0.4983281 
    ## Run 4 stress 0.5027723 
    ## Run 5 stress 0.5038923 
    ## Run 6 stress 0.497513 
    ## Run 7 stress 0.4986608 
    ## Run 8 stress 0.4916161 
    ## Run 9 stress 0.5031278 
    ## Run 10 stress 0.5029603 
    ## Run 11 stress 0.505045 
    ## Run 12 stress 0.5004177 
    ## Run 13 stress 0.4869891 
    ## Run 14 stress 0.4977277 
    ## Run 15 stress 0.5038102 
    ## Run 16 stress 0.4815528 
    ## Run 17 stress 0.494358 
    ## Run 18 stress 0.495237 
    ## Run 19 stress 0.482883 
    ## Run 20 stress 0.5033126 
    ## *** No convergence -- monoMDS stopping criteria:
    ##     19: stress ratio > sratmax
    ##      1: scale factor of the gradient < sfgrmin
    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.2840847 
    ## Run 1 stress 0.2863509 
    ## Run 2 stress 0.2870368 
    ## Run 3 stress 0.2839486 
    ## ... New best solution
    ## ... Procrustes: rmse 0.01071511  max resid 0.117471 
    ## Run 4 stress 0.2905426 
    ## Run 5 stress 0.2862262 
    ## Run 6 stress 0.2848907 
    ## Run 7 stress 0.2870373 
    ## Run 8 stress 0.2858939 
    ## Run 9 stress 0.2839912 
    ## ... Procrustes: rmse 0.01370438  max resid 0.1556768 
    ## Run 10 stress 0.2837024 
    ## ... New best solution
    ## ... Procrustes: rmse 0.01763395  max resid 0.156648 
    ## Run 11 stress 0.2842045 
    ## Run 12 stress 0.285472 
    ## Run 13 stress 0.2831733 
    ## ... New best solution
    ## ... Procrustes: rmse 0.0141739  max resid 0.1733232 
    ## Run 14 stress 0.2889594 
    ## Run 15 stress 0.2846167 
    ## Run 16 stress 0.2832541 
    ## ... Procrustes: rmse 0.01507419  max resid 0.1432372 
    ## Run 17 stress 0.287454 
    ## Run 18 stress 0.3082315 
    ## Run 19 stress 0.2838507 
    ## Run 20 stress 0.2832058 
    ## ... Procrustes: rmse 0.01440061  max resid 0.1418918 
    ## *** No convergence -- monoMDS stopping criteria:
    ##      3: no. of iterations >= maxit
    ##     17: stress ratio > sratmax
    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.2102841 
    ## Run 1 stress 0.2136145 
    ## Run 2 stress 0.2103667 
    ## ... Procrustes: rmse 0.003263281  max resid 0.03467674 
    ## Run 3 stress 0.2102921 
    ## ... Procrustes: rmse 0.001519856  max resid 0.01777968 
    ## Run 4 stress 0.2107498 
    ## ... Procrustes: rmse 0.009302528  max resid 0.09431957 
    ## Run 5 stress 0.2105163 
    ## ... Procrustes: rmse 0.007527809  max resid 0.1366379 
    ## Run 6 stress 0.2103847 
    ## ... Procrustes: rmse 0.004185144  max resid 0.06785547 
    ## Run 7 stress 0.2104187 
    ## ... Procrustes: rmse 0.004749827  max resid 0.0650601 
    ## Run 8 stress 0.2110721 
    ## Run 9 stress 0.2145605 
    ## Run 10 stress 0.2119066 
    ## Run 11 stress 0.2105292 
    ## ... Procrustes: rmse 0.006038473  max resid 0.05080086 
    ## Run 12 stress 0.2103591 
    ## ... Procrustes: rmse 0.005411354  max resid 0.09271949 
    ## Run 13 stress 0.2109124 
    ## Run 14 stress 0.2103536 
    ## ... Procrustes: rmse 0.004349665  max resid 0.07647048 
    ## Run 15 stress 0.210742 
    ## ... Procrustes: rmse 0.007611315  max resid 0.09299587 
    ## Run 16 stress 0.2105534 
    ## ... Procrustes: rmse 0.00798786  max resid 0.09741312 
    ## Run 17 stress 0.2108324 
    ## Run 18 stress 0.2114798 
    ## Run 19 stress 0.2103393 
    ## ... Procrustes: rmse 0.004913973  max resid 0.0733701 
    ## Run 20 stress 0.2118199 
    ## *** No convergence -- monoMDS stopping criteria:
    ##      7: no. of iterations >= maxit
    ##     13: stress ratio > sratmax
    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.1686183 
    ## Run 1 stress 0.1697099 
    ## Run 2 stress 0.1685055 
    ## ... New best solution
    ## ... Procrustes: rmse 0.005872908  max resid 0.1028527 
    ## Run 3 stress 0.1685131 
    ## ... Procrustes: rmse 0.001169969  max resid 0.01387929 
    ## Run 4 stress 0.1687568 
    ## ... Procrustes: rmse 0.009663381  max resid 0.1552058 
    ## Run 5 stress 0.1685228 
    ## ... Procrustes: rmse 0.001116964  max resid 0.01217455 
    ## Run 6 stress 0.1687836 
    ## ... Procrustes: rmse 0.009587415  max resid 0.1556782 
    ## Run 7 stress 0.168622 
    ## ... Procrustes: rmse 0.0058987  max resid 0.1035794 
    ## Run 8 stress 0.1689385 
    ## ... Procrustes: rmse 0.009624889  max resid 0.1528852 
    ## Run 9 stress 0.1686173 
    ## ... Procrustes: rmse 0.005713841  max resid 0.1027582 
    ## Run 10 stress 0.1696548 
    ## Run 11 stress 0.1688828 
    ## ... Procrustes: rmse 0.01008275  max resid 0.1534037 
    ## Run 12 stress 0.1685176 
    ## ... Procrustes: rmse 0.001272373  max resid 0.01474957 
    ## Run 13 stress 0.1686871 
    ## ... Procrustes: rmse 0.008663938  max resid 0.1556592 
    ## Run 14 stress 0.1687147 
    ## ... Procrustes: rmse 0.00868074  max resid 0.1556022 
    ## Run 15 stress 0.1685075 
    ## ... Procrustes: rmse 0.001112754  max resid 0.01046827 
    ## Run 16 stress 0.1685169 
    ## ... Procrustes: rmse 0.001110096  max resid 0.01590361 
    ## Run 17 stress 0.1686149 
    ## ... Procrustes: rmse 0.005916899  max resid 0.1042515 
    ## Run 18 stress 0.1687672 
    ## ... Procrustes: rmse 0.01004455  max resid 0.1555527 
    ## Run 19 stress 0.1731242 
    ## Run 20 stress 0.1685086 
    ## ... Procrustes: rmse 0.001292152  max resid 0.01928791 
    ## *** No convergence -- monoMDS stopping criteria:
    ##      8: no. of iterations >= maxit
    ##     12: stress ratio > sratmax
    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.1447748 
    ## Run 1 stress 0.1456159 
    ## Run 2 stress 0.1449231 
    ## ... Procrustes: rmse 0.02612968  max resid 0.1599972 
    ## Run 3 stress 0.1441223 
    ## ... New best solution
    ## ... Procrustes: rmse 0.0217241  max resid 0.1644539 
    ## Run 4 stress 0.1441154 
    ## ... New best solution
    ## ... Procrustes: rmse 0.01004778  max resid 0.1354274 
    ## Run 5 stress 0.1448055 
    ## Run 6 stress 0.1454079 
    ## Run 7 stress 0.1443533 
    ## ... Procrustes: rmse 0.009151459  max resid 0.1025566 
    ## Run 8 stress 0.1464554 
    ## Run 9 stress 0.1450998 
    ## Run 10 stress 0.1441724 
    ## ... Procrustes: rmse 0.01172151  max resid 0.1251914 
    ## Run 11 stress 0.1442503 
    ## ... Procrustes: rmse 0.009304569  max resid 0.1041193 
    ## Run 12 stress 0.1447837 
    ## Run 13 stress 0.144179 
    ## ... Procrustes: rmse 0.009840122  max resid 0.1307502 
    ## Run 14 stress 0.14539 
    ## Run 15 stress 0.1448055 
    ## Run 16 stress 0.1458409 
    ## Run 17 stress 0.1461479 
    ## Run 18 stress 0.144366 
    ## ... Procrustes: rmse 0.0122622  max resid 0.1667826 
    ## Run 19 stress 0.1447199 
    ## Run 20 stress 0.1450386 
    ## *** No convergence -- monoMDS stopping criteria:
    ##     17: no. of iterations >= maxit
    ##      3: stress ratio > sratmax
    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.1261577 
    ## Run 1 stress 0.1270746 
    ## Run 2 stress 0.1290113 
    ## Run 3 stress 0.1270681 
    ## Run 4 stress 0.1286544 
    ## Run 5 stress 0.1269888 
    ## Run 6 stress 0.1274129 
    ## Run 7 stress 0.1263256 
    ## ... Procrustes: rmse 0.008103905  max resid 0.09664264 
    ## Run 8 stress 0.1272791 
    ## Run 9 stress 0.1269465 
    ## Run 10 stress 0.126307 
    ## ... Procrustes: rmse 0.01068938  max resid 0.08034859 
    ## Run 11 stress 0.1281251 
    ## Run 12 stress 0.1262919 
    ## ... Procrustes: rmse 0.007932578  max resid 0.07795157 
    ## Run 13 stress 0.1280644 
    ## Run 14 stress 0.1268432 
    ## Run 15 stress 0.1265167 
    ## ... Procrustes: rmse 0.01158125  max resid 0.1097919 
    ## Run 16 stress 0.1270404 
    ## Run 17 stress 0.1278337 
    ## Run 18 stress 0.1267024 
    ## Run 19 stress 0.1261712 
    ## ... Procrustes: rmse 0.007371222  max resid 0.09880889 
    ## Run 20 stress 0.1262072 
    ## ... Procrustes: rmse 0.008425436  max resid 0.1007672 
    ## *** No convergence -- monoMDS stopping criteria:
    ##     20: no. of iterations >= maxit

![](5_5-Community-Trajectory-Analysis_Phd_all_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

    ## [1] 0.4339670 0.2831733 0.2102841 0.1685055 0.1441154 0.1261577

``` r
t_ordination <- metaMDS(t, distance = "bray", k=3)
```

    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.2102841 
    ## Run 1 stress 0.2105728 
    ## ... Procrustes: rmse 0.008015089  max resid 0.1361362 
    ## Run 2 stress 0.2123261 
    ## Run 3 stress 0.2118377 
    ## Run 4 stress 0.2105289 
    ## ... Procrustes: rmse 0.007780593  max resid 0.1368106 
    ## Run 5 stress 0.2105537 
    ## ... Procrustes: rmse 0.00703132  max resid 0.08128518 
    ## Run 6 stress 0.2112102 
    ## Run 7 stress 0.2134745 
    ## Run 8 stress 0.2115032 
    ## Run 9 stress 0.2111799 
    ## Run 10 stress 0.2105864 
    ## ... Procrustes: rmse 0.008975732  max resid 0.1364353 
    ## Run 11 stress 0.2103657 
    ## ... Procrustes: rmse 0.00486565  max resid 0.07818406 
    ## Run 12 stress 0.2104123 
    ## ... Procrustes: rmse 0.006720823  max resid 0.09887808 
    ## Run 13 stress 0.2104215 
    ## ... Procrustes: rmse 0.005358313  max resid 0.07437767 
    ## Run 14 stress 0.2106807 
    ## ... Procrustes: rmse 0.009590586  max resid 0.1022526 
    ## Run 15 stress 0.2107927 
    ## Run 16 stress 0.2111479 
    ## Run 17 stress 0.2103483 
    ## ... Procrustes: rmse 0.005530434  max resid 0.08133116 
    ## Run 18 stress 0.2104196 
    ## ... Procrustes: rmse 0.006413295  max resid 0.07939401 
    ## Run 19 stress 0.2106219 
    ## ... Procrustes: rmse 0.009525927  max resid 0.1357418 
    ## Run 20 stress 0.2127303 
    ## *** No convergence -- monoMDS stopping criteria:
    ##      5: no. of iterations >= maxit
    ##     15: stress ratio > sratmax

``` r
stressplot(t_ordination)
```

![](5_5-Community-Trajectory-Analysis_Phd_all_files/figure-gfm/unnamed-chunk-4-4.png)<!-- -->

``` r
# t (alone): stress value: 0.21 at 3 dimension
```

Diatom data set for Spring / Autumn Mainstream and Tributaries both have
0.21 stress value at 3 dimensions and at dimension 4, the value falls
within the boundary.

For this experiment, 3 dimension is selected\!

## Community Trajectory Analysis (CTA)

I created nMDS, one of multivariate analysis, ordination dimension
reduction

#### Final ordination: Gov Data

``` r
# Extract location of samples in dimensional space
Fn_Ordi_Point3d <- data.frame(X=Final_ordination$points[,1],
                                 Y=Final_ordination$points[,2],
                                 Z=Final_ordination$points[,3]
                                 )

Fn_Ordi_nMDS_Result <- cbind(Fn_Ordi_Point3d, Final_GOV_Env)

unique(Fn_Ordi_nMDS_Result$New.Code.Name)
```

    ##  [1] "M1"   "T1"   "M2"   "M3"   "T2"   "T3a"  "T3b"  "T4"   "M4"   "T5a" 
    ## [11] "T5b"  "M5"   "M6"   "T6a"  "T6b"  "M7"   "M8"   "M9"   "T7a"  "T7b" 
    ## [21] "M10"  "M11"  "M12"  "T8a"  "T8b"  "T9a"  "T9b"  "M13"  "T10a" "T10b"
    ## [31] "M14"  "T11a" "T11b" "M15"

``` r
Fn_Ordi_nMDS_Result$New.Code.Name <-factor(Fn_Ordi_nMDS_Result$New.Code.Name, levels = c(
  "M1", "M2","M3", "M4", "M5", "M6", "M7", "M8", "M9", "M10", "M11", "M12", "M13", "M14", "M15", "T1", "T2", "T3a", "T3b", "T4", "T5a", "T5b", "T6a", "T6b", "T7a", "T7b", "T8a", "T8b", "T9a", "T9b", "T10a", "T10b", "T11a", "T11b"))
```

#### Four square plots Spring/Autumn x main/tri

#### Main stream: all sites in one graph

``` r
Fn_nMDS_M <- Fn_Ordi_nMDS_Result %>% filter(River.Type =="M") 
Fn_nMDS_M_Site <- Fn_nMDS_M$New.Code.Name
Fn_nMDS_M_Year <- Fn_nMDS_M$Year
Fn_nMDS_M_Survey <- Fn_nMDS_M$Half.Of.Year

# Spring / Autumn (select)
## Spring
ggplot(data=subset(Fn_nMDS_M, Half.Of.Year=="Spring"), aes(X, Y, color=New.Code.Name))+
  geom_point()+
  geom_path()+
  theme_bw()+
  geom_hline(yintercept = 0, lty= 2, color="red")+geom_vline(xintercept = 0, lty= 2, color="red")+
  labs(title="nMDS results of Government Data Set",
       subtitle = "Spring Diatom Data Set in Mainstream", 
       caption = "Data Source: Government Data Set")
```

![](5_5-Community-Trajectory-Analysis_Phd_all_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
## Autumn
ggplot(data=subset(Fn_nMDS_M, Half.Of.Year=="Autumn"), aes(X, Y, color=New.Code.Name))+
  geom_point()+
  geom_path()+
  theme_bw()+
  geom_hline(yintercept = 0, lty= 2, color="red")+geom_vline(xintercept = 0, lty= 2, color="red")+
  labs(title="nMDS results of Government Data Set",
       subtitle = "Autumn Diatom Data Set in Mainstream", 
       caption = "Data Source: Government Data Set")
```

![](5_5-Community-Trajectory-Analysis_Phd_all_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
# Spring / Autumn (facet wrap)
Fn_nMDS_M %>%
  ggplot(aes(X, Y, color=Fn_nMDS_M_Site))+
  geom_point()+
  geom_path()+
  geom_text(label=Fn_nMDS_M_Year, size=3, nudge_y=0.1, check_overlap = T)+
  theme_bw()+
  geom_hline(yintercept = 0, lty= 2, color="red")+geom_vline(xintercept = 0, lty= 2, color="red")+
  labs(title="nMDS results of Government Data Set",
       subtitle = "Spring and Autumn Diatom Data Sets in Mainstream", 
       caption = "Data Source: Government Data Set")+
  facet_wrap(~Half.Of.Year)
```

![](5_5-Community-Trajectory-Analysis_Phd_all_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->

#### Main stream: each site in one graph, but one line in facet grid

``` r
# All facet grid: hard to read
ggplot(Fn_nMDS_M, aes(X, Y, colour=Z))+
  geom_point(size=1)+
  geom_path()+
  geom_text(label=Fn_nMDS_M_Year, size=2, check_overlap = T)+
  theme_bw()+
  geom_hline(yintercept = 0, lty= 2, color="red")+
  geom_vline(xintercept = 0, lty= 2, color="red")+
  labs(title = "Comparisons of nMDS results between Spring and Autumn Diatom Data Sets",
       caption = "Data Source: Government Data Set")+
  facet_grid(Half.Of.Year~New.Code.Name)
```

![](5_5-Community-Trajectory-Analysis_Phd_all_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

#### Main stream: facet\_grid 5 sites in one graph for better resolutions

``` r
# M1-M5 extraction
Fn_M1M5 <-Fn_nMDS_M %>% filter(New.Code.Name=="M1"|New.Code.Name=="M2"|New.Code.Name=="M3"|New.Code.Name=="M4"|New.Code.Name=="M5")
Fn_M1M5_Year <- Fn_M1M5$Year

# M6-M10 extraction
Fn_M6M10 <-Fn_nMDS_M %>% filter(New.Code.Name=="M6"|New.Code.Name=="M7"|New.Code.Name=="M8"|New.Code.Name=="M9"|New.Code.Name=="M10")
Fn_M6M10_Year <- Fn_M6M10$Year

# M11-M15 extraction
Fn_M11M15 <-Fn_nMDS_M %>% filter(New.Code.Name=="M11"|New.Code.Name=="M12"|New.Code.Name=="M13"|New.Code.Name=="M14"|New.Code.Name=="M15")
Fn_M11M15_Year <- Fn_M11M15$Year

# M1-M5 ggplot
ggplot(Fn_M1M5, aes(X, Y, colour=Z))+
  geom_point(size=1)+
  geom_path()+
  geom_text(label = Fn_M1M5_Year, size=2, nudge_y = 0.1, check_overlap = T)+
  theme_bw()+
  geom_hline(yintercept = 0, lty= 2, color="red")+
  geom_vline(xintercept = 0, lty= 2, color="red")+
  facet_grid(Half.Of.Year~New.Code.Name)+
  labs(title = "nMDS results: M1 - M5")
```

![](5_5-Community-Trajectory-Analysis_Phd_all_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
# M6-M10 ggplot
ggplot(Fn_M6M10, aes(X, Y, colour=Z))+
  geom_point(size=1)+
  geom_path()+
  geom_text(label = Fn_M6M10_Year, size=2, nudge_y = 0.1, check_overlap = T)+
  theme_bw()+
  geom_hline(yintercept = 0, lty= 2, color="red")+
  geom_vline(xintercept = 0, lty= 2, color="red")+
  facet_grid(Half.Of.Year~New.Code.Name)+
  labs(title = "nMDS results: M6 - M10")
```

![](5_5-Community-Trajectory-Analysis_Phd_all_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
# M11-M15 ggplot
ggplot(Fn_M11M15, aes(X, Y, colour=Z))+
  geom_point(size=1)+
  geom_path()+
  geom_text(label = Fn_M11M15_Year, size=2, nudge_y = 0.1, check_overlap = T)+
  theme_bw()+
  geom_hline(yintercept = 0, lty= 2, color="red")+
  geom_vline(xintercept = 0, lty= 2, color="red")+
  facet_grid(Half.Of.Year~New.Code.Name)+
  labs(title = "nMDS results: M11 - M15")
```

![](5_5-Community-Trajectory-Analysis_Phd_all_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

#### Main stream: each site in one graph using facet wrap

``` r
# M1-M15 Spring / Autumn + Facet_wrap

ggplot(data=subset(Fn_nMDS_M, Half.Of.Year=="Spring"), aes(X, Y, color=Z))+
  geom_point()+
  geom_path()+
  theme_bw()+
  geom_hline(yintercept = 0, lty= 2, color="red")+geom_vline(xintercept = 0, lty= 2, color="red")+
  facet_wrap(~New.Code.Name, ncol=5)
```

![](5_5-Community-Trajectory-Analysis_Phd_all_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
ggplot(data=subset(Fn_nMDS_M, Half.Of.Year=="Autumn"), aes(X, Y, color=Z))+
  geom_point()+
  geom_path()+
  theme_bw()+
  geom_hline(yintercept = 0, lty= 2, color="red")+geom_vline(xintercept = 0, lty= 2, color="red")+
  facet_wrap(~New.Code.Name, ncol=5)
```

![](5_5-Community-Trajectory-Analysis_Phd_all_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

``` r
## Comparison between Spring and Autumn Data in one facet wrap graph (3 dimensions omiited)
Com_plot <- ggplot(Fn_nMDS_M, aes(X, Y, color=Half.Of.Year))+
  geom_point()+
  geom_path()+
  theme_bw()+
  geom_hline(yintercept = 0, lty= 2, color="red")+geom_vline(xintercept = 0, lty= 2, color="red")+
  facet_wrap(~New.Code.Name, ncol=5)

Com_plot+ geom_text(data = Fn_nMDS_M, label=Fn_nMDS_M$Year, size=2, nudge_y = 0.2, check_overlap = T)+
  labs(title = "nMDS results of Spring and Autumn Data Set")
```

![](5_5-Community-Trajectory-Analysis_Phd_all_files/figure-gfm/unnamed-chunk-9-3.png)<!-- -->

``` r
## Selective labelling with ggplot2

ggplot(Fn_nMDS_M, aes(X, Y, colour=Z, label=Year))+
  geom_point(size=1)+
  geom_path()+
  geom_text(data=subset(Fn_nMDS_M, Year > 2017 | Year < 2010), size=3,nudge_y = 0.2, check_overlap = T)+
  theme_bw()+
  geom_hline(yintercept = 0, lty= 2, color="red")+
  geom_vline(xintercept = 0, lty= 2, color="red")+
  facet_wrap(~New.Code.Name, ncol=5)
```

![](5_5-Community-Trajectory-Analysis_Phd_all_files/figure-gfm/unnamed-chunk-9-4.png)<!-- -->
