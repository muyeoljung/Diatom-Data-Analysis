Dissimilarity Analysis of First 15 Diatom Samples in Nakdong River,
South Korea
================
Muyeol Jung
10/03/2020

# Introduction

Diatoms have been widely used in water qualty assessment because of
their traits of sensitivity to changes in environments and ubiquity in
water. Kelly and other researchers have adapted diatom to water quality
analysis, however, how replicable diatoms are remain unanswered.

This chapter is to attempt to compare daitom samples taken at the same
time from different rock samples and to examine their replicability in
random selection. For the research purpose, all 15 rock samples were
collected at three major different sites in Nakdong River Dec 2018,
encompassing upstream, midstream and downstream.

Methodologically, dissimilarity between samples is calculated and then
plotted using non-metric Multidimensional scaling.

# Data Process

## Import data set and process

Diatom Counts from 15 rock sample are imported and then processed in an
appropriate format of table with observations in rows and species in
columns.

``` r
# Import the data set and transpose
D15.Count <- read.csv("Final_Diatom_List.csv", stringsAsFactors = FALSE) %>% t()

# Convert matrix to dataframe
D15.Count <- data.frame(data.matrix(D15.Count))
summary(D15.Count)

# colnames
D15_colname <- D15.Count[1,]
D15_colname <- lapply(D15_colname, as.character)


D15.Count <- D15.Count[-1,]

## Convert factors to numeric
index <- sapply(D15.Count, is.factor)
D15.Count[index] <- lapply(D15.Count[index], function(x) as.numeric(as.character(x)))

colnames(D15.Count) <- D15_colname

D15.Count

### below works

D15.Count1 <- read.csv("Final_Diatom_List.csv") %>% t()
D15.Count1 <- data.frame(data.matrix(D15.Count1))

a1 <- D15.Count1[1,]
a1 <- lapply(a1, as.character)

D15.Count1 <- D15.Count1[-1,]
colnames(D15.Count1) <- a1

D15.Count1

index <- sapply(D15.Count1, is.factor)
D15.Count1[index] <- lapply(D15.Count1[index], function(x) as.numeric(as.character(x)))
```

## Import environment data set for the sample

Environmental data for the diatom results are also loaded. The data
includes Name, Site(Group), and Date and Site will be used here as
grouping factor.

Site; A upstream, B midstream, C downstream

``` r
D15.env <- readxl::read_xlsx("Diatom_Env.xlsx")
str(D15.env)
```

# Analysis and Results

## Dissimilarity

Calculate dissimilarities: vegdist (Dissimilarity Indicies for Community
Ecologists)

Command “Vegdist” will calculate distances (dissimilarity) between
observations (samples). The Bray-Curtis distance is used as the
Bray-Curtis dissimilarity is one of the most well-known ways of
quantifying the difference between samples. (It does not satisfy the
triangle inequality axiom, and hence is not a true distance)

Bray-Curtis distance is calculated as follows.

\[d_{j,k}= \sqrt{\frac{\sum _i|x_ij-x_ik|}{\sum _i(x_ij+x_ik)} }\] where
\(x_ij\), \(x_ik\) refer to the quantity on species (column) \(i\) and
site (rows) \(j\) and \(k\).

Bray-Curtis distance scale (multiply 100) is easy to understand (0 means
the samples are exactly the same, while 100 is the maximum difference
that can be observed between two samples).

Here, dissimilarities are calculated on raw counts (not relative
abundance).

``` r
D15.Count.dist <- vegdist(D15.Count, method = "bray")*100
D15.Count.dist
```

    ##          A1       A2       A3       A4       A5       B1       B2       B3
    ## A2 29.80421                                                               
    ## A3 37.43436 34.66205                                                      
    ## A4 30.70652 27.76489 33.11949                                             
    ## A5 38.62434 33.23331 36.44134 20.89762                                    
    ## B1 83.06138 84.47653 80.22599 83.68027 82.75584                           
    ## B2 74.35897 74.36709 70.60755 74.20781 74.23049 38.56655                  
    ## B3 81.37472 80.57922 78.54610 79.47908 79.64805 30.12939 35.21809         
    ## B4 75.15275 79.44359 69.87179 73.75631 74.35179 49.08486 43.44624 46.05678
    ## B5 84.15842 84.13361 79.00791 81.56863 81.52866 38.14126 51.23251 36.92417
    ## C1 82.32932 83.26996 78.88101 81.96023 80.66298 69.91006 64.03191 72.53685
    ## C2 80.61617 81.87092 76.89422 81.11413 79.76190 69.54157 62.16216 74.57502
    ## C3 78.01932 78.19905 72.74633 77.19745 76.52174 69.53069 56.78131 70.77877
    ## C4 82.74836 83.48868 77.47253 81.69279 79.81651 71.48936 63.60153 74.66125
    ## C5 82.59757 82.65896 76.53214 80.77183 80.35597 71.05263 65.33149 77.02504
    ##          B4       B5       C1       C2       C3       C4
    ## A2                                                      
    ## A3                                                      
    ## A4                                                      
    ## A5                                                      
    ## B1                                                      
    ## B2                                                      
    ## B3                                                      
    ## B4                                                      
    ## B5 53.88635                                             
    ## C1 69.33996 81.57216                                    
    ## C2 71.89409 80.81683 23.42704                           
    ## C3 67.15468 78.99650 25.25126 17.27053                  
    ## C4 71.92982 80.67855 29.00433 24.92564 26.02586         
    ## C5 74.83085 81.49291 30.48699 26.67946 27.15232 18.86121

``` r
# 0: the same, 100: different
```

## Analysis of Similarities(ANOSIM)

ANOSIM provides a way to test statistically whether there is a
significant difference between two or more groups of sampling units. It
operates directly on a dissimilarity matrix. (uses only the rank order
of dissimilarity values)

``` r
D15.anosim <- with(D15.env, anosim(D15.Count.dist, Site))
summary(D15.anosim)
```

    ## 
    ## Call:
    ## anosim(x = D15.Count.dist, grouping = Site) 
    ## Dissimilarity: bray 
    ## 
    ## ANOSIM statistic R:     1 
    ##       Significance: 0.001 
    ## 
    ## Permutation: free
    ## Number of permutations: 999
    ## 
    ## Upper quantiles of permutations (null model):
    ##   90%   95% 97.5%   99% 
    ## 0.160 0.221 0.275 0.364 
    ## 
    ## Dissimilarity ranks between and within classes:
    ##         0%   25%  50%   75% 100%  N
    ## Between 31 49.50 68.0 86.50  105 75
    ## A        3 12.75 16.5 19.50   25 10
    ## B       13 21.50 25.0 27.75   30 10
    ## C        1  4.25  6.5  8.75   14 10

``` r
plot(D15.anosim)
```

    ## Warning in bxp(list(stats = structure(c(31, 49.5, 68, 86.5, 105, 3, 12, :
    ## some notches went outside hinges ('box'): maybe set notch=FALSE

![](Git_First_D15_Analysis_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## MRPP

Multi Response Permutation Procedure and Mean Dissimilarity Matrix

mrpp for a similar function using original dissimiliarities instead of
their ranks.

``` r
D15.mrpp <- with(D15.env, mrpp(D15.Count, distance = "bray", Site))
D15.mrpp
```

    ## 
    ## Call:
    ## mrpp(dat = D15.Count, grouping = Site, distance = "bray") 
    ## 
    ## Dissimilarity index: bray 
    ## Weights for groups:  n 
    ## 
    ## Class means and counts:
    ## 
    ##       A      B      C     
    ## delta 0.3227 0.4227 0.2491
    ## n     5      5      5     
    ## 
    ## Chance corrected within-group agreement A: 0.4843 
    ## Based on observed delta 0.3315 and expected delta 0.6428 
    ## 
    ## Significance of delta: 0.001 
    ## Permutation: free
    ## Number of permutations: 999

``` r
## mean distance within/between groups
D15.meandis <- with(D15.env, meandist(D15.Count.dist, Site))
D15.meandis
```

    ##          A        B        C
    ## A 32.26881 78.58290 79.92720
    ## B 78.58290 42.26862 71.66864
    ## C 79.92720 71.66864 24.90846
    ## attr(,"class")
    ## [1] "meandist" "matrix"  
    ## attr(,"n")
    ## grouping
    ## A B C 
    ## 5 5 5

``` r
# summary finds the within-class, between-class and overall means of these dissimilarities.
summary(D15.meandis)
```

    ## 
    ## Mean distances:
    ##                 Average
    ## within groups  33.14863
    ## between groups 76.72625
    ## overall        64.27550
    ## 
    ## Summary statistics:
    ##                          Statistic
    ## MRPP A weights n         0.4842727
    ## MRPP A weights n-1       0.4842727
    ## MRPP A weights n(n-1)    0.4842727
    ## Classification strength 43.5776168

``` r
plot(D15.meandis)
```

![](Git_First_D15_Analysis_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## nMDS

Dimension reduction and ordination using nMDS (non-Metric
Multidimensional Scaling)

Non Metric as this is

``` r
D15.ordination <- metaMDS(D15.Count, distance = "bray", k=2)
```

    ## Square root transformation
    ## Wisconsin double standardization
    ## Run 0 stress 0.06360169 
    ## Run 1 stress 0.06359106 
    ## ... New best solution
    ## ... Procrustes: rmse 0.004014452  max resid 0.01117057 
    ## Run 2 stress 0.06366926 
    ## ... Procrustes: rmse 0.01494855  max resid 0.03958879 
    ## Run 3 stress 0.06359125 
    ## ... Procrustes: rmse 5.816849e-05  max resid 0.0001470027 
    ## ... Similar to previous best
    ## Run 4 stress 0.0635922 
    ## ... Procrustes: rmse 0.0002549486  max resid 0.0006289895 
    ## ... Similar to previous best
    ## Run 5 stress 0.06360313 
    ## ... Procrustes: rmse 0.004472188  max resid 0.01237232 
    ## Run 6 stress 0.06360225 
    ## ... Procrustes: rmse 0.004203467  max resid 0.01165722 
    ## Run 7 stress 0.06366913 
    ## ... Procrustes: rmse 0.01502028  max resid 0.03980167 
    ## Run 8 stress 0.06359042 
    ## ... New best solution
    ## ... Procrustes: rmse 0.0004262411  max resid 0.001124453 
    ## ... Similar to previous best
    ## Run 9 stress 0.06366914 
    ## ... Procrustes: rmse 0.01479758  max resid 0.03969625 
    ## Run 10 stress 0.06366916 
    ## ... Procrustes: rmse 0.01479629  max resid 0.03973675 
    ## Run 11 stress 0.06359155 
    ## ... Procrustes: rmse 0.0005933495  max resid 0.001556009 
    ## ... Similar to previous best
    ## Run 12 stress 0.06359211 
    ## ... Procrustes: rmse 0.0007482442  max resid 0.001937655 
    ## ... Similar to previous best
    ## Run 13 stress 0.06359092 
    ## ... Procrustes: rmse 0.0003520984  max resid 0.0009244102 
    ## ... Similar to previous best
    ## Run 14 stress 0.06366933 
    ## ... Procrustes: rmse 0.01478852  max resid 0.03983144 
    ## Run 15 stress 0.06366919 
    ## ... Procrustes: rmse 0.01479118  max resid 0.03974514 
    ## Run 16 stress 0.06359164 
    ## ... Procrustes: rmse 0.0006198373  max resid 0.001618849 
    ## ... Similar to previous best
    ## Run 17 stress 0.0636692 
    ## ... Procrustes: rmse 0.01479847  max resid 0.03971741 
    ## Run 18 stress 0.06359097 
    ## ... Procrustes: rmse 0.000385725  max resid 0.001020348 
    ## ... Similar to previous best
    ## Run 19 stress 0.06359355 
    ## ... Procrustes: rmse 0.001481359  max resid 0.004115052 
    ## ... Similar to previous best
    ## Run 20 stress 0.06366915 
    ## ... Procrustes: rmse 0.01480051  max resid 0.03972218 
    ## *** Solution reached

``` r
plot(D15.ordination, type="t", display = "sites")
with(D15.env, ordihull(D15.ordination, Site))
```

![](Git_First_D15_Analysis_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

# Conclusion

On Bray-Curtis dissimilarities, fives samples at each site are
clustered, whereas far away from samples from other sites.

Samples at site C are more closely clustered followed by site A, site B
is the least clustered.
