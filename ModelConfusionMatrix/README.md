# ModelConfusionMatrix
Script that tests the models for a specified land use by training it with half the data then doing predictions on the other half.

To run the program, open the R project and run the following commands:
````
source(paste(getwd(), '/confusion_matrix.r', sep=''))
results <- create_confusion_matrix(13) 
````
Note that the input parameter indicates the land use index, which defaults to 13 (Street tree)

The `create_confusion_matrix` function returns a list of variables as described in the table below.

Return variable | Description
------------ | -------------
confusion_matrix | The confustion matrix for each species that has at least 400 observations in the specified land use. Details of how to interpret the results can be found here: https://en.m.wikipedia.org/wiki/Confusion_matrix
predictions | The predicted species for every observed tree
counts | A table with the counts of predicted species for each species of observed tree
counts_summary | A table with just the positive and negative counts for each species

For example, run the following command to see the resulting confusion matrix
````
results$confusion_matrix

                          P     N   TP    TN    FP   FN          TPR       TNR        PPV       NPV       FNR          FPR       FDR          FOR       ACC          F1           MCC            BM           MK
Acer freemanii         2473 45941    6 45783   158 2467 0.0024262030 0.9965608 0.03658537 0.9488705 0.9975738 3.439194e-03 0.9634146 0.0511295337 0.9457801 0.004550626 -0.0038383730 -1.012991e-03 -0.014544168
Acer miyabei            380 48034    0 48034     0  380 0.0000000000 1.0000000        NaN 0.9921510 1.0000000 0.000000e+00       NaN 0.0078489693 0.9921510         NaN           NaN  0.000000e+00          NaN
Acer platanoides       4221 44193    3 44142    51 4218 0.0007107321 0.9988460 0.05555556 0.9127792 0.9992893 1.154029e-03 0.9444444 0.0872208437 0.9118230 0.001403509 -0.0037466149 -4.432969e-04 -0.031665288
Acer rubrum            2462 45952    2 45908    44 2460 0.0008123477 0.9990425 0.04347826 0.9491399 0.9991877 9.575209e-04 0.9565217 0.0508600728 0.9482794 0.001594896 -0.0010352011 -1.451732e-04 -0.007381812
Acer saccharinum       4607 43807 2269 39814  3993 2338 0.4925113957 0.9088502 0.36234430 0.9445341 0.5074886 9.114982e-02 0.6376557 0.0554659328 0.8692320 0.417517711  0.3509546776  4.013616e-01  0.306878366
Acer saccharum         1055 47359    0 47359     0 1055 0.0000000000 1.0000000        NaN 0.9782088 1.0000000 0.000000e+00       NaN 0.0217912174 0.9782088         NaN           NaN  0.000000e+00          NaN
Alnus spp.               24 48390    0 48390     0   24 0.0000000000 1.0000000        NaN 0.9995043 1.0000000 0.000000e+00       NaN 0.0004957244 0.9995043         NaN           NaN  0.000000e+00          NaN
Amelanchier spp.         93 48321    0 48321     0   93 0.0000000000 1.0000000        NaN 0.9980791 1.0000000 0.000000e+00       NaN 0.0019209320 0.9980791         NaN           NaN  0.000000e+00          NaN
Cercis canadensis        87 48327    0 48327     0   87 0.0000000000 1.0000000        NaN 0.9982030 1.0000000 0.000000e+00       NaN 0.0017970009 0.9982030         NaN           NaN  0.000000e+00          NaN
Fraxinus americana     2227 46187    0 46187     0 2227 0.0000000000 1.0000000        NaN 0.9540009 1.0000000 0.000000e+00       NaN 0.0459990912 0.9540009         NaN           NaN  0.000000e+00          NaN
Fraxinus pennsylvanica 6487 41927 3303 31736 10191 3184 0.5091721905 0.7569347 0.24477546 0.9088202 0.4908278 2.430653e-01 0.7552245 0.0911798396 0.7237369 0.330614083  0.2021703429  2.661069e-01  0.153595616
Gleditsia triacanthos  5677 42737  139 42231   506 5538 0.0244847631 0.9881601 0.21550388 0.8840671 0.9755152 1.183986e-02 0.7844961 0.1159329272 0.8751601 0.043973426  0.0354833091  1.264491e-02  0.099570949
Malus spp.             1157 47257    0 47257     0 1157 0.0000000000 1.0000000        NaN 0.9761020 1.0000000 0.000000e+00       NaN 0.0238980460 0.9761020         NaN           NaN  0.000000e+00          NaN
Other                  7899 40515 5473 18264 22251 2426 0.6928725155 0.4507960 0.19741019 0.8827453 0.3071275 5.492040e-01 0.8025898 0.1172547124 0.4902921 0.307273391  0.1073117796  1.436685e-01  0.080155474
Picea abies              95 48319    0 48318     1   95 0.0000000000 0.9999793 0.00000000 0.9980377 1.0000000 2.069579e-05 1.0000000 0.0019622829 0.9980171         NaN -0.0002015217 -2.069579e-05 -0.001962283
Picea pungens           317 48097    0 48097     0  317 0.0000000000 1.0000000        NaN 0.9934523 1.0000000 0.000000e+00       NaN 0.0065476928 0.9934523         NaN           NaN  0.000000e+00          NaN
Picea spp.              246 48168    0 48168     0  246 0.0000000000 1.0000000        NaN 0.9949188 1.0000000 0.000000e+00       NaN 0.0050811749 0.9949188         NaN           NaN  0.000000e+00          NaN
Pinus nigra             232 48182    0 48182     0  232 0.0000000000 1.0000000        NaN 0.9952080 1.0000000 0.000000e+00       NaN 0.0047920023 0.9952080         NaN           NaN  0.000000e+00          NaN
Pinus strobus            84 48330    0 48330     0   84 0.0000000000 1.0000000        NaN 0.9982650 1.0000000 0.000000e+00       NaN 0.0017350353 0.9982650         NaN           NaN  0.000000e+00          NaN
Pseudotsuga menziesii    18 48396    0 48396     0   18 0.0000000000 1.0000000        NaN 0.9996282 1.0000000 0.000000e+00       NaN 0.0003717933 0.9996282         NaN           NaN  0.000000e+00          NaN
Pyrus calleryana       2098 46316    0 46316     0 2098 0.0000000000 1.0000000        NaN 0.9566654 1.0000000 0.000000e+00       NaN 0.0433345726 0.9566654         NaN           NaN  0.000000e+00          NaN
Quercus alba            231 48183   17 48176     7  214 0.0735930736 0.9998547 0.70833333 0.9955776 0.9264069 1.452795e-04 0.2916667 0.0044224013 0.9954352 0.133333333  0.2273778908  7.344779e-02  0.703910932
Quercus bicolor         873 47541    0 47541     0  873 0.0000000000 1.0000000        NaN 0.9819680 1.0000000 0.000000e+00       NaN 0.0180319742 0.9819680         NaN           NaN  0.000000e+00          NaN
Quercus macrocarpa      524 47890    0 47890     0  524 0.0000000000 1.0000000        NaN 0.9891767 1.0000000 0.000000e+00       NaN 0.0108233156 0.9891767         NaN           NaN  0.000000e+00          NaN
Quercus rubra           671 47743    0 47743     0  671 0.0000000000 1.0000000        NaN 0.9861404 1.0000000 0.000000e+00       NaN 0.0138596274 0.9861404         NaN           NaN  0.000000e+00          NaN
Taxodium distichum      114 48300    0 48300     0  114 0.0000000000 1.0000000        NaN 0.9976453 1.0000000 0.000000e+00       NaN 0.0023546908 0.9976453         NaN           NaN  0.000000e+00          NaN
Thuja occidentalis       80 48334    0 48334     0   80 0.0000000000 1.0000000        NaN 0.9983476 1.0000000 0.000000e+00       NaN 0.0016524146 0.9983476         NaN           NaN  0.000000e+00          NaN
Tilia americana        1267 47147    0 47147     0 1267 0.0000000000 1.0000000        NaN 0.9738299 1.0000000 0.000000e+00       NaN 0.0261701161 0.9738299         NaN           NaN  0.000000e+00          NaN
Tilia cordata          1760 46654    0 46654     0 1760 0.0000000000 1.0000000        NaN 0.9636469 1.0000000 0.000000e+00       NaN 0.0363531210 0.9636469         NaN           NaN  0.000000e+00          NaN
Ulmus hybrid            955 47459    0 47459     0  955 0.0000000000 1.0000000        NaN 0.9802743 1.0000000 0.000000e+00       NaN 0.0197256992 0.9802743         NaN           NaN  0.000000e+00          NaN

````


# Future improvements
* Allow input data file to be passed into function
* Limit number of threads to the number supported by the processor
