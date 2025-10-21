Test CBE State vs GHGI
================

To investigate issue
[\#1](https://github.com/cornerstone-data/USEEIO-State/issues/1), this
analysis compares the California CBE as presented and compares it to the
CA GHGI 2019 used in the model.

Download and read in the CA model for 2019 with GHGs.

``` r
#rmarkdown::render('../examples/DownloadandSaveModelLocally.Rmd', params =  list(states = c('CA'),model="v1.1-GHG",years=c(2019)))
model <- readRDS("../models/CAEEIOv1.1-GHG-19.rds")
```

## Validate the model

Make sure the model passes existing validation tests.

``` r
printValidationResults(model)
```

    ## [1] "Validate that commodity output can be recalculated (within 1%) with the model total requirements matrix (L) and demand vector (y) for US production"
    ## [1] "Number of sectors passing: 146"
    ## [1] "Number of sectors failing: 0"
    ## [1] "Sectors failing: "
    ## [1] "Validate that commodity output can be recalculated (within 1%) with model total domestic requirements matrix (L_d) and model demand (y) for US production"
    ## [1] "Number of sectors passing: 146"
    ## [1] "Number of sectors failing: 0"
    ## [1] "Sectors failing: "
    ## [1] "Validate that flow totals by commodity (E_c) can be recalculated (within 1%) using the model satellite matrix (B), market shares matrix (V_n), total requirements matrix (L), and demand vector (y) for US production"
    ## [1] "Number of flow totals by commodity passing: 2336"
    ## [1] "Number of flow totals by commodity failing: 0"
    ## [1] "Sectors with flow totals failing: "
    ## [1] "Validate that flow totals by commodity (E_c) can be recalculated (within 1%) using the model satellite matrix (B), market shares matrix (V_n), total domestic requirements matrix (L_d), and demand vector (y) for US production"
    ## [1] "Number of flow totals by commodity passing: 2336"
    ## [1] "Number of flow totals by commodity failing: 0"
    ## [1] "Sectors with flow totals failing: "
    ## [1] "Validate that commodity output are properly transformed to industry output via MarketShare"
    ## [1] "Number of flow totals by commodity passing: 146"
    ## [1] "Number of flow totals by commodity failing: 0"
    ## [1] "Sectors with flow totals failing: "
    ## [1] "Validate that commodity output equals to domestic use plus production demand"
    ## [1] "Number of flow totals by commodity passing: 146"
    ## [1] "Number of flow totals by commodity failing: 0"
    ## [1] "Sectors with flow totals failing: "
    ## 
    ## Checking that production demand vectors do not produce errors for 2-R models.
    ## Calculating direct results using Production Complete final demand...
    ## 2025-10-21 14:49:02.814801 INFO::Calculating Direct + Imported Perspective LCI and LCIA with external import factors...
    ## 2025-10-21 14:49:02.820974 INFO::Result calculation complete.
    ## 
    ## Calculating final results using Production Complete final demand...
    ## 
    ## 2025-10-21 14:49:02.825033 INFO::Calculating Final Perspective LCI and LCIA with external import factors...
    ## 2025-10-21 14:49:02.826344 INFO::Result calculation complete.
    ## 
    ## 
    ## Checking that consumption demand vectors do not produce errors for 2-R models.
    ## 
    ## Calculating direct results using Consumption Complete final demand...
    ## 2025-10-21 14:49:02.82677 INFO::Calculating Direct + Imported Perspective LCI and LCIA with external import factors...
    ## 2025-10-21 14:49:02.828757 INFO::Result calculation complete.
    ## 
    ## Calculating final results using Consumption Complete final demand...
    ## 2025-10-21 14:49:02.829123 INFO::Calculating Final Perspective LCI and LCIA with external import factors...
    ## 2025-10-21 14:49:02.830277 INFO::Result calculation complete.
    ## 
    ## Testing that final demand vector is equivalent between standard and coupled model approaches. I.e.: y = y_m + y_d.
    ## [1] TRUE
    ## 
    ## Testing that economic throughput is equivalement between standard and coupled model approaches for the given final demand vector.
    ## I.e.,: x = x_dm.
    ## [1] TRUE
    ## 
    ## Testing that LCI results are equivalent between standard and coupled model approaches (i.e., LCI = LCI_dm) when
    ## assuming model$M = model$M_m.
    ## [1] TRUE
    ## 
    ## Testing that LCIA results are equivalent between standard and coupled model approaches (i.e., LCIA = LCIA_dm) when
    ## assuming model$M = model$M_m.
    ## [1] TRUE
    ## 2025-10-21 14:49:02.835685 INFO::Calculating Final Perspective LCI and LCIA with external import factors...
    ## 2025-10-21 14:49:02.837308 INFO::Result calculation complete.
    ## 
    ## Testing that LCI emissions from households are equivalent to calculated result from Total Consumption.

    ## [1] TRUE

## Compare CBE from CA as done in report vs GHGI

First calculate the CBE as done in the report, using the final
perspective which is the default for calculateStateCBE.

``` r
cbe <- calculateStateCBE(model,perspective="FINAL")
```

    ## 2025-10-21 14:49:02.854201 INFO::Calculating Final Perspective LCI and LCIA with external import factors...
    ## 2025-10-21 14:49:02.855985 INFO::Result calculation complete.

``` r
cbe_fromCA <- subset(cbe, endsWith(rownames(cbe), soi)) 
```

Total California CBE in 2019 from CA calculated with the final
perspective is 5.3634661^{11} metric tons CO2e. The total from all
locations is 7.7465486^{11} metric tons CO2e.

``` r
ghgi <- getStateGHGI(model)
total_ghgi <- sum(ghgi[,"Greenhouse Gases"])
```

The total California GHGI from model GHGI data for 2019 is
4.4577087^{11} metric tons CO2e.

The result is that the CBE attributed to CA is greater than the GHGI for
CA.

## Change in Method - Use DIRECT Perspective

The DIRECT perspective calculation actually reveals emissions where they
occur, rather than attributing them to the final goods and services
consumed in the state. Recalculate the CBE using the direct perspective.

``` r
cbe <- calculateStateCBE(model,perspective="DIRECT")
```

    ## 2025-10-21 14:49:02.96618 INFO::Calculating Direct + Imported Perspective LCI and LCIA with external import factors...
    ## 2025-10-21 14:49:02.968307 INFO::Result calculation complete.

``` r
cbe_fromCA <- subset(cbe, endsWith(rownames(cbe), "CA")) 
```

Total California CBE from CA in 2019 calculated with the direct
perspective is 3.2626327^{11} metric tons CO2e. The total calculated
from all locations is 7.7465486^{11} metric tons CO2e.

To compare the CBE to the GHGI, the GHGI has to be translated into
commodity format We use the commmodity mix matrix to do this.

``` r
#Get the model list of industries in CA
CA_inds <- model$Industries$Code_Loc[endsWith(model$Industries$Code_Loc, "CA")]

#Index GHGI by CA industries
mghgi <- ghgi[match(CA_inds, rownames(ghgi)), , drop=FALSE]


print("Are the industry rows in the same order in GHGI and in CBE?")
```

    ## [1] "Are the industry rows in the same order in GHGI and in CBE?"

``` r
identical(rownames(mghgi),CA_inds)
```

    ## [1] TRUE

``` r
#mghgi<- as.matrix(ghgi)
#dim(mghgi)
C_m_CA <- subset(model$C_m, endsWith(rownames(model$C_m), "CA") , endsWith(colnames(model$C_m), "CA"))
#dim(C_m_CA)

#Left multiply C_m_CA by mghgi to get GHGI in commodity format
ghgi_c <- C_m_CA %*% mghgi

#Add household back in
ghgi_c <- rbind(ghgi_c,"F010/US-CA"=ghgi["F010/US-CA","Greenhouse Gases"])

#Create a table to compare CBE and GHGI by sector
compare <- merge(cbe_fromCA, ghgi_c, by=0)
colnames(compare) <- c("Sector", "CBE_fromCA", "GHGI")
compare$CBE_frac_of_GHGI <- compare$'CBE_fromCA'/compare$GHGI
rownames(compare) <- rownames(cbe_fromCA)

kable(compare, caption = "Comparison of CBE from CA using direct perspective vs GHGI")
```

|              | Sector       |   CBE_fromCA |         GHGI | CBE_frac_of_GHGI |
|:-------------|:-------------|-------------:|-------------:|-----------------:|
| 111CA/US-CA  | 111CA/US-CA  |  12326819960 |  37501690288 |        0.3287004 |
| 113FF/US-CA  | 113FF/US-CA  |     32443553 |    141607414 |        0.2291091 |
| 211/US-CA    | 211/US-CA    |   2833939255 |   8217910999 |        0.3448491 |
| 212/US-CA    | 212/US-CA    |    748320383 |   1173168485 |        0.6378627 |
| 213/US-CA    | 213/US-CA    |   1378794381 |   1496069734 |        0.9216110 |
| 22/US-CA     | 22/US-CA     |  29649496851 |  35387638280 |        0.8378490 |
| 23/US-CA     | 23/US-CA     |   9228896363 |   9524236519 |        0.9689907 |
| 321/US-CA    | 311FT/US-CA  |   4173816265 |   5619952423 |        0.7426782 |
| 327/US-CA    | 313TT/US-CA  |    154666355 |    311351286 |        0.4967584 |
| 331/US-CA    | 315AL/US-CA  |      7223903 |     31079102 |        0.2324360 |
| 332/US-CA    | 321/US-CA    |    514830875 |    678973907 |        0.7582484 |
| 333/US-CA    | 322/US-CA    |   2578827704 |   4565586527 |        0.5648404 |
| 334/US-CA    | 323/US-CA    |    129961748 |    260641653 |        0.4986223 |
| 335/US-CA    | 324/US-CA    |   5456785342 |  12336064298 |        0.4423441 |
| 3361MV/US-CA | 325/US-CA    |   5950993811 |  22241009730 |        0.2675685 |
| 3364OT/US-CA | 326/US-CA    |    441667170 |    820893159 |        0.5380325 |
| 337/US-CA    | 327/US-CA    |   5871905033 |   7959690299 |        0.7377052 |
| 339/US-CA    | 331/US-CA    |   2161074494 |   5808187564 |        0.3720738 |
| 311FT/US-CA  | 332/US-CA    |    491931278 |   1022015684 |        0.4813344 |
| 313TT/US-CA  | 333/US-CA    |    301573633 |    787304448 |        0.3830458 |
| 315AL/US-CA  | 334/US-CA    |     71118912 |    265026990 |        0.2683459 |
| 322/US-CA    | 335/US-CA    |    157523422 |    377190273 |        0.4176232 |
| 323/US-CA    | 3361MV/US-CA |    504095800 |    958586537 |        0.5258741 |
| 324/US-CA    | 3364OT/US-CA |    104796117 |    516652834 |        0.2028366 |
| 325/US-CA    | 337/US-CA    |    193842113 |    250276537 |        0.7745117 |
| 326/US-CA    | 339/US-CA    |    100809740 |    226719464 |        0.4446453 |
| 42/US-CA     | 42/US-CA     |    287031610 |   1410939219 |        0.2034330 |
| 441/US-CA    | 441/US-CA    |    184614220 |    171017359 |        1.0795057 |
| 445/US-CA    | 445/US-CA    |    148459225 |    228061794 |        0.6509605 |
| 452/US-CA    | 452/US-CA    |     85370739 |     96780618 |        0.8821057 |
| 4A0/US-CA    | 481/US-CA    |   9457715709 |  22591478917 |        0.4186408 |
| 481/US-CA    | 482/US-CA    |    833384485 |   1477894879 |        0.5638997 |
| 482/US-CA    | 483/US-CA    |   1671875436 |   2792132352 |        0.5987809 |
| 483/US-CA    | 484/US-CA    |  18096508861 |  28888414916 |        0.6264279 |
| 484/US-CA    | 485/US-CA    |    949814487 |   1968584980 |        0.4824859 |
| 485/US-CA    | 486/US-CA    |   2188038708 |   4035559667 |        0.5421897 |
| 486/US-CA    | 487OS/US-CA  |   1530358901 |   3178035246 |        0.4815425 |
| 487OS/US-CA  | 493/US-CA    |    455780097 |   1087960032 |        0.4189309 |
| 493/US-CA    | 4A0/US-CA    |    333427417 |    392758118 |        0.8489383 |
| 511/US-CA    | 511/US-CA    |     11796531 |     28492570 |        0.4140213 |
| 512/US-CA    | 512/US-CA    |      2422540 |     18300401 |        0.1323763 |
| 513/US-CA    | 513/US-CA    |    183644081 |    407129220 |        0.4510707 |
| 514/US-CA    | 514/US-CA    |     46626252 |    175397610 |        0.2658317 |
| 521CI/US-CA  | 521CI/US-CA  |     99545447 |    497306517 |        0.2001692 |
| 523/US-CA    | 523/US-CA    |    186886637 |    305916220 |        0.6109079 |
| 524/US-CA    | 524/US-CA    |     14373107 |     55827838 |        0.2574541 |
| 525/US-CA    | 525/US-CA    |   1353061592 |   1403106590 |        0.9643327 |
| HS/US-CA     | 532RL/US-CA  |    199510736 |    739156153 |        0.2699169 |
| ORE/US-CA    | 5411/US-CA   |      7955856 |     17737696 |        0.4485282 |
| 532RL/US-CA  | 5412OP/US-CA |    427144914 |   2401374769 |        0.1778752 |
| 5411/US-CA   | 5415/US-CA   |    373796648 |    637495056 |        0.5863522 |
| 5415/US-CA   | 55/US-CA     |     70467613 |    135873402 |        0.5186270 |
| 5412OP/US-CA | 561/US-CA    |    721373576 |   1970895645 |        0.3660131 |
| 55/US-CA     | 562/US-CA    |   4816575469 |  12121937253 |        0.3973437 |
| 561/US-CA    | 61/US-CA     |    708102872 |   2313831518 |        0.3060304 |
| 562/US-CA    | 621/US-CA    |   1344148903 |   1279511169 |        1.0505175 |
| 61/US-CA     | 622/US-CA    |   5015682868 |   3990778270 |        1.2568182 |
| 621/US-CA    | 623/US-CA    |    296010267 |    220990423 |        1.3394710 |
| 622/US-CA    | 624/US-CA    |    162457277 |    208131104 |        0.7805526 |
| 623/US-CA    | 711AS/US-CA  |     48948031 |     94640458 |        0.5171999 |
| 624/US-CA    | 713/US-CA    |    576072935 |   1296162554 |        0.4444450 |
| 711AS/US-CA  | 721/US-CA    |    309892079 |    372718183 |        0.8314381 |
| 713/US-CA    | 722/US-CA    |   1336367431 |   1511902984 |        0.8838976 |
| 721/US-CA    | 81/US-CA     |    877088146 |   1047937517 |        0.8369661 |
| 722/US-CA    | F010/US-CA   | 144237948162 | 144237948162 |        1.0000000 |
| 81/US-CA     | GFE/US-CA    |    105119962 |    286327814 |        0.3671315 |
| GFGD/US-CA   | GFGD/US-CA   |    456655342 |    868266739 |        0.5259390 |
| GFGN/US-CA   | GFGN/US-CA   |   7896798514 |   4328096263 |        1.8245432 |
| GFE/US-CA    | GSLE/US-CA   |   3677601333 |   2949183065 |        1.2469898 |
| GSLG/US-CA   | GSLG/US-CA   |  27214546212 |  27173196968 |        1.0015217 |
| GSLE/US-CA   | HS/US-CA     |   2928590037 |   4020645500 |        0.7283880 |
| Used/US-CA   | ORE/US-CA    |   1824742545 |   1645632622 |        1.1088396 |
| Other/US-CA  | Other/US-CA  |  -2573880437 |     44754830 |      -57.5106737 |
| F010/US-CA   | Used/US-CA   |   -483342726 |    164971748 |       -2.9298515 |

Comparison of CBE from CA using direct perspective vs GHGI

Show the commodit

|            | CBE_frac_of_GHGI |
|:-----------|-----------------:|
| 441/US-CA  |         1.079506 |
| 562/US-CA  |         1.050517 |
| 61/US-CA   |         1.256818 |
| 621/US-CA  |         1.339471 |
| 722/US-CA  |         1.000000 |
| GFGN/US-CA |         1.824543 |
| GFE/US-CA  |         1.246990 |
| GSLG/US-CA |         1.001522 |
| Used/US-CA |         1.108840 |

Commodities with CBE greater than GHGI

## Try it with domestic requirements only

    ## 2025-10-21 14:49:03.017326 INFO::Calculating Direct + Imported Perspective LCI and LCIA with external import factors...
    ## 2025-10-21 14:49:03.019755 INFO::Result calculation complete.

Total California CBE from CA in 2019 calculated with the direct
perspective with domestic requirements is 3.2626327^{11} metric tons
CO2e. The total calculated from all locations is 5.4185432^{11} metric
tons CO2e. This is the same as without domestic requirements.

| Sector       |   CBE_fromCA |         GHGI | CBE_frac_of_GHGI |
|:-------------|-------------:|-------------:|-----------------:|
| 111CA/US-CA  |  12326819960 |  37501690288 |        0.3287004 |
| 113FF/US-CA  |     32443553 |    141607414 |        0.2291091 |
| 211/US-CA    |   2833939255 |   8217910999 |        0.3448491 |
| 212/US-CA    |    748320383 |   1173168485 |        0.6378627 |
| 213/US-CA    |   1378794381 |   1496069734 |        0.9216110 |
| 22/US-CA     |  29649496851 |  35387638280 |        0.8378490 |
| 23/US-CA     |   9228896363 |   9524236519 |        0.9689907 |
| 311FT/US-CA  |   4173816265 |   5619952423 |        0.7426782 |
| 313TT/US-CA  |    154666355 |    311351286 |        0.4967584 |
| 315AL/US-CA  |      7223903 |     31079102 |        0.2324360 |
| 321/US-CA    |    514830875 |    678973907 |        0.7582484 |
| 322/US-CA    |   2578827704 |   4565586527 |        0.5648404 |
| 323/US-CA    |    129961748 |    260641653 |        0.4986223 |
| 324/US-CA    |   5456785342 |  12336064298 |        0.4423441 |
| 325/US-CA    |   5950993811 |  22241009730 |        0.2675685 |
| 326/US-CA    |    441667170 |    820893159 |        0.5380325 |
| 327/US-CA    |   5871905033 |   7959690299 |        0.7377052 |
| 331/US-CA    |   2161074494 |   5808187564 |        0.3720738 |
| 332/US-CA    |    491931278 |   1022015684 |        0.4813344 |
| 333/US-CA    |    301573633 |    787304448 |        0.3830458 |
| 334/US-CA    |     71118912 |    265026990 |        0.2683459 |
| 335/US-CA    |    157523422 |    377190273 |        0.4176232 |
| 3361MV/US-CA |    504095800 |    958586537 |        0.5258741 |
| 3364OT/US-CA |    104796117 |    516652834 |        0.2028366 |
| 337/US-CA    |    193842113 |    250276537 |        0.7745117 |
| 339/US-CA    |    100809740 |    226719464 |        0.4446453 |
| 42/US-CA     |    287031610 |   1410939219 |        0.2034330 |
| 441/US-CA    |    184614220 |    171017359 |        1.0795057 |
| 445/US-CA    |    148459225 |    228061794 |        0.6509605 |
| 452/US-CA    |     85370739 |     96780618 |        0.8821057 |
| 481/US-CA    |   9457715709 |  22591478917 |        0.4186408 |
| 482/US-CA    |    833384485 |   1477894879 |        0.5638997 |
| 483/US-CA    |   1671875436 |   2792132352 |        0.5987809 |
| 484/US-CA    |  18096508861 |  28888414916 |        0.6264279 |
| 485/US-CA    |    949814487 |   1968584980 |        0.4824859 |
| 486/US-CA    |   2188038708 |   4035559667 |        0.5421897 |
| 487OS/US-CA  |   1530358901 |   3178035246 |        0.4815425 |
| 493/US-CA    |    455780097 |   1087960032 |        0.4189309 |
| 4A0/US-CA    |    333427417 |    392758118 |        0.8489383 |
| 511/US-CA    |     11796531 |     28492570 |        0.4140213 |
| 512/US-CA    |      2422540 |     18300401 |        0.1323763 |
| 513/US-CA    |    183644081 |    407129220 |        0.4510707 |
| 514/US-CA    |     46626252 |    175397610 |        0.2658317 |
| 521CI/US-CA  |     99545447 |    497306517 |        0.2001692 |
| 523/US-CA    |    186886637 |    305916220 |        0.6109079 |
| 524/US-CA    |     14373107 |     55827838 |        0.2574541 |
| 525/US-CA    |   1353061592 |   1403106590 |        0.9643327 |
| 532RL/US-CA  |    199510736 |    739156153 |        0.2699169 |
| 5411/US-CA   |      7955856 |     17737696 |        0.4485282 |
| 5412OP/US-CA |    427144914 |   2401374769 |        0.1778752 |
| 5415/US-CA   |    373796648 |    637495056 |        0.5863522 |
| 55/US-CA     |     70467613 |    135873402 |        0.5186270 |
| 561/US-CA    |    721373576 |   1970895645 |        0.3660131 |
| 562/US-CA    |   4816575469 |  12121937253 |        0.3973437 |
| 61/US-CA     |    708102872 |   2313831518 |        0.3060304 |
| 621/US-CA    |   1344148903 |   1279511169 |        1.0505175 |
| 622/US-CA    |   5015682868 |   3990778270 |        1.2568182 |
| 623/US-CA    |    296010267 |    220990423 |        1.3394710 |
| 624/US-CA    |    162457277 |    208131104 |        0.7805526 |
| 711AS/US-CA  |     48948031 |     94640458 |        0.5171999 |
| 713/US-CA    |    576072935 |   1296162554 |        0.4444450 |
| 721/US-CA    |    309892079 |    372718183 |        0.8314381 |
| 722/US-CA    |   1336367431 |   1511902984 |        0.8838976 |
| 81/US-CA     |    877088146 |   1047937517 |        0.8369661 |
| F010/US-CA   | 144237948162 | 144237948162 |        1.0000000 |
| GFE/US-CA    |    105119962 |    286327814 |        0.3671315 |
| GFGD/US-CA   |    456655342 |    868266739 |        0.5259390 |
| GFGN/US-CA   |   7896798514 |   4328096263 |        1.8245432 |
| GSLE/US-CA   |   3677601333 |   2949183065 |        1.2469898 |
| GSLG/US-CA   |  27214546212 |  27173196968 |        1.0015217 |
| HS/US-CA     |   2928590037 |   4020645500 |        0.7283880 |
| ORE/US-CA    |   1824742545 |   1645632622 |        1.1088396 |
| Other/US-CA  |  -2573880437 |     44754830 |      -57.5106737 |
| Used/US-CA   |   -483342726 |    164971748 |       -2.9298515 |

Comparison of CBE from CA using direct perspective and domestic inputs
only vs GHGI

To take a clos We can also compare the total uses of CA commodities with
the total commodity output (q). The total uses should not be greater
than q.

|  | Production_Demand | Consumption_Demand | Intermediate_Uses | q | q_calc_consumption | cons_frac_of_q | q_calc_production | prod_frac_of_q |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|
| 111CA/US-CA | 15797938767 | 6283518421 | 31686115248 | 77165949762 | 37969633669 | 0.4920517 | 47484054016 | 0.6153498 |
| 113FF/US-CA | 1269786754 | 704205935 | 7114585785 | 17710185556 | 7818791721 | 0.4414856 | 8384372539 | 0.4734209 |
| 211/US-CA | 3105594191 | 0 | 7991156783 | 11096765050 | 7991156783 | 0.7201339 | 11096750975 | 0.9999987 |
| 212/US-CA | 497287179 | 2630800 | 3368488388 | 3892622171 | 3371119188 | 0.8660278 | 3865775567 | 0.9931032 |
| 213/US-CA | 3288120849 | 3271810387 | 456672139 | 3775020191 | 3728482526 | 0.9876722 | 3744792988 | 0.9919928 |
| 22/US-CA | 36607235091 | 36103617582 | 45596033222 | 83034615573 | 81699650804 | 0.9839228 | 82203268313 | 0.9899879 |
| 23/US-CA | 203559406368 | 203570304269 | 39836611142 | 243396017510 | 243406915411 | 1.0000448 | 243396017510 | 1.0000000 |
| 321/US-CA | 1032198499 | 669400199 | 6011903611 | 7121681018 | 6681303811 | 0.9381639 | 7044102110 | 0.9891067 |
| 327/US-CA | 1211079850 | 425626331 | 9258275359 | 10682278790 | 9683901690 | 0.9065389 | 10469355209 | 0.9800676 |
| 331/US-CA | 1684283427 | 17553448 | 6726836116 | 8502139076 | 6744389563 | 0.7932580 | 8411119542 | 0.9892945 |
| 332/US-CA | 5083540245 | 1402052577 | 22225072753 | 32392325731 | 23627125330 | 0.7294050 | 27308612998 | 0.8430581 |
| 333/US-CA | 20229567994 | 8248094942 | 6157034694 | 31543455899 | 14405129637 | 0.4566757 | 26386602688 | 0.8365159 |
| 334/US-CA | 42654996385 | 23144981929 | 11089052193 | 109667153697 | 34234034122 | 0.3121631 | 53744048578 | 0.4900651 |
| 335/US-CA | 6520814208 | 2607744253 | 3582795391 | 12206247941 | 6190539644 | 0.5071616 | 10103609599 | 0.8277408 |
| 3361MV/US-CA | 23611640814 | 13138488097 | 7533448035 | 32937754725 | 20671936132 | 0.6276061 | 31145088849 | 0.9455741 |
| 3364OT/US-CA | 17374777459 | 5781562172 | 4776426364 | 37570293489 | 10557988536 | 0.2810196 | 22151203823 | 0.5895936 |
| 337/US-CA | 3971903882 | 3424467936 | 2414587915 | 7096313160 | 5839055851 | 0.8228295 | 6386491798 | 0.8999732 |
| 339/US-CA | 16920776905 | 8414178510 | 4001996370 | 26669390188 | 12416174880 | 0.4655590 | 20922773276 | 0.7845239 |
| 311FT/US-CA | 62483533297 | 52013441488 | 33114707623 | 102689062788 | 85128149111 | 0.8289894 | 95598240919 | 0.9309486 |
| 313TT/US-CA | 1637394972 | 903382674 | 1487035728 | 3472830350 | 2390418402 | 0.6883199 | 3124430701 | 0.8996785 |
| 315AL/US-CA | 2438383293 | 1064653845 | 462417058 | 5829611041 | 1527070902 | 0.2619507 | 2900800351 | 0.4975976 |
| 322/US-CA | 1876250506 | 880131659 | 7584640926 | 9592826499 | 8464772586 | 0.8824065 | 9460891432 | 0.9862465 |
| 323/US-CA | 450236509 | 289922560 | 5960078927 | 6509566058 | 6250001487 | 0.9601257 | 6410315436 | 0.9847531 |
| 324/US-CA | 22841372359 | 16825730653 | 38674172756 | 102625229344 | 55499903408 | 0.5408017 | 61515545114 | 0.5994193 |
| 325/US-CA | 31507915323 | 18273142487 | 38459555846 | 148423153429 | 56732698333 | 0.3822362 | 69967471169 | 0.4714054 |
| 326/US-CA | 3576413616 | 1066783020 | 10428887252 | 14382626534 | 11495670272 | 0.7992748 | 14005300867 | 0.9737652 |
| 42/US-CA | 69976111460 | 32896447586 | 34334325944 | 279678757797 | 67230773529 | 0.2403857 | 104310437403 | 0.3729652 |
| 441/US-CA | 16352019356 | 19119120984 | 3557577264 | 19909596620 | 22676698248 | 1.1389833 | 19909596620 | 1.0000000 |
| 445/US-CA | 42917535388 | 27856899536 | 698774752 | 43616310140 | 28555674288 | 0.6547017 | 43616310140 | 1.0000000 |
| 452/US-CA | 33817847781 | 30081771492 | 1435138396 | 35252986177 | 31516909888 | 0.8940210 | 35252986177 | 1.0000000 |
| 4A0/US-CA | 107195511689 | 107195511689 | 17459197203 | 143612250477 | 124654708892 | 0.8679950 | 124654708892 | 0.8679950 |
| 481/US-CA | 17829967045 | 11482642235 | 5169915672 | 34493132910 | 16652557907 | 0.4827789 | 22999882716 | 0.6667960 |
| 482/US-CA | 1146524902 | 582731334 | 2243024309 | 3408907882 | 2825755643 | 0.8289328 | 3389549212 | 0.9943212 |
| 483/US-CA | 5533459179 | 3346542339 | 1103121149 | 6636793696 | 4449663487 | 0.6704538 | 6636580328 | 0.9999679 |
| 484/US-CA | 20300482669 | 15030165038 | 20373091232 | 44157164274 | 35403256270 | 0.8017557 | 40673573900 | 0.9211093 |
| 485/US-CA | 21629203276 | 8606303156 | 9671183714 | 31300386990 | 18277486870 | 0.5839380 | 31300386990 | 1.0000000 |
| 486/US-CA | 267279502 | 74554096 | 975949868 | 1244959288 | 1050503964 | 0.8438059 | 1243229370 | 0.9986105 |
| 487OS/US-CA | 7988004301 | 4185547672 | 32527305457 | 40515309758 | 36712853129 | 0.9061477 | 40515309758 | 1.0000000 |
| 493/US-CA | 32377256 | 9344438 | 14822307927 | 21340110672 | 14831652365 | 0.6950129 | 14854685183 | 0.6960922 |
| 511/US-CA | 34482752201 | 26577665042 | 5525207107 | 71554220962 | 32102872149 | 0.4486510 | 40007959308 | 0.5591279 |
| 512/US-CA | 15374718033 | 5515315514 | 4397210290 | 82498381386 | 9912525804 | 0.1201542 | 19771928322 | 0.2396644 |
| 513/US-CA | 36641336139 | 34895931160 | 34982957201 | 122761505773 | 69878888361 | 0.5692248 | 71624293340 | 0.5834426 |
| 514/US-CA | 17292102744 | 14554641361 | 29203678956 | 118956872480 | 43758320316 | 0.3678503 | 46495781700 | 0.3908625 |
| 521CI/US-CA | 15513818140 | 8280041192 | 14547384651 | 95767221779 | 22827425844 | 0.2383637 | 30061202792 | 0.3138987 |
| 523/US-CA | 34381765937 | 26591783040 | 31329280717 | 79251736145 | 57921063758 | 0.7308491 | 65711046655 | 0.8291433 |
| 524/US-CA | 12885473535 | 11489499707 | 14760968013 | 90909978067 | 26250467719 | 0.2887523 | 27646441548 | 0.3041079 |
| 525/US-CA | 15865550063 | 15865550063 | 1205104764 | 17370768831 | 17070654827 | 0.9827230 | 17070654827 | 0.9827230 |
| HS/US-CA | 425936456351 | 310247014388 | 0 | 425936456351 | 310247014388 | 0.7283880 | 425936456351 | 1.0000000 |
| ORE/US-CA | -52168372004 | 21361234449 | 168215926226 | 116047554222 | 189577160675 | 1.6336162 | 116047554222 | 1.0000000 |
| 532RL/US-CA | 16175562102 | 6606094125 | 18961855136 | 63908005952 | 25567949261 | 0.4000743 | 35137417238 | 0.5498124 |
| 5411/US-CA | 14532687230 | 12312234879 | 19184188478 | 52464903124 | 31496423357 | 0.6003332 | 33716875708 | 0.6426558 |
| 5415/US-CA | 63493880348 | 55493508913 | 28578921219 | 121973352530 | 84072430132 | 0.6892688 | 92072801566 | 0.7548600 |
| 5412OP/US-CA | 67891027829 | 35975221414 | 54437711857 | 429982019143 | 90412933270 | 0.2102714 | 122328739686 | 0.2844973 |
| 55/US-CA | 504972854 | 0 | 71408826336 | 74022176740 | 71408826336 | 0.9646950 | 71913799190 | 0.9715169 |
| 561/US-CA | 6315293621 | 5950485149 | 77667753753 | 147130490256 | 83618238902 | 0.5683271 | 83983047374 | 0.5708065 |
| 562/US-CA | 1945363869 | 1922384356 | 6017858951 | 16789771967 | 7940243307 | 0.4729215 | 7963222821 | 0.4742901 |
| 61/US-CA | 16759494299 | 16417987451 | 2075272042 | 59218493132 | 18493259493 | 0.3122886 | 18834766342 | 0.3180555 |
| 621/US-CA | 153160871148 | 160749034195 | 5353838787 | 158514709935 | 166102872982 | 1.0478704 | 158514709935 | 1.0000000 |
| 622/US-CA | 128452436725 | 161445761665 | 287968653 | 128740405378 | 161733730318 | 1.2562779 | 128740405378 | 1.0000000 |
| 623/US-CA | 27063018338 | 36399385392 | 535055644 | 27598073983 | 36934441036 | 1.3382978 | 27598073983 | 1.0000000 |
| 624/US-CA | 42365173406 | 33038569035 | 167739098 | 42532912503 | 33206308133 | 0.7807203 | 42532912503 | 1.0000000 |
| 711AS/US-CA | 14827053406 | 14489921264 | 17185540485 | 42305034279 | 31675461749 | 0.7487398 | 32012593891 | 0.7567089 |
| 713/US-CA | 12956268527 | 12956268527 | 238796484 | 29522729064 | 13195065012 | 0.4469460 | 13195065012 | 0.4469460 |
| 721/US-CA | 18727323929 | 18727323929 | 8650842437 | 27927982233 | 27378166366 | 0.9803131 | 27378166366 | 0.9803131 |
| 722/US-CA | 129911212967 | 121785369765 | 31613909006 | 161525121974 | 153399278771 | 0.9496930 | 161525121974 | 1.0000000 |
| 81/US-CA | 80889560651 | 80879677626 | 33115581442 | 121121855095 | 113995259068 | 0.9411618 | 114005142094 | 0.9412434 |
| GFGD/US-CA | 54982377326 | 28917376637 | 0 | 54982377326 | 28917376637 | 0.5259390 | 54982377326 | 1.0000000 |
| GFGN/US-CA | 40593234772 | 74064109606 | 0 | 40593234772 | 74064109606 | 1.8245432 | 40593234772 | 1.0000000 |
| GFE/US-CA | 436380879 | 357454573 | 3680472481 | 6477169224 | 4037927054 | 0.6234092 | 4116853360 | 0.6355945 |
| GSLG/US-CA | 299167433929 | 299622674712 | 0 | 299167433929 | 299622674712 | 1.0015217 | 299167433929 | 1.0000000 |
| GSLE/US-CA | 6229115967 | 10527894360 | 4967159494 | 11196275460 | 15495053853 | 1.3839472 | 11196275460 | 1.0000000 |
| Used/US-CA | -1823830207 | -4751754741 | 2810921241 | 987091034 | -1940833499 | -1.9662153 | 987091034 | 1.0000000 |
| Other/US-CA | 523137418 | -24113602588 | -103381659 | 419755759 | -24216984247 | -57.6930363 | 419755759 | 1.0000000 |

Comparison of Uses to Commodity output (q) for production and
consumption with domestic requirements only

Some of the commodities have uses greater than q. Show these.

|            | cons_frac_of_q |
|:-----------|---------------:|
| 23/US-CA   |       1.000045 |
| 441/US-CA  |       1.138983 |
| ORE/US-CA  |       1.633616 |
| 621/US-CA  |       1.047870 |
| 622/US-CA  |       1.256278 |
| 623/US-CA  |       1.338298 |
| GFGN/US-CA |       1.824543 |
| GSLG/US-CA |       1.001522 |
| GSLE/US-CA |       1.383947 |

Commodities with Uses greater than output (q)

These require further investigation.
