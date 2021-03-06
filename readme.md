## Birkbeck MSc Applied Statistics R code

Some of the [R](https://www.r-project.org/) code I wrote for various courseworks and projects during my [MSc](http://www.bbk.ac.uk/study/2016/postgraduate/programmes/TMSSTAPP_C/) in Applied Statistics at Birkbeck between 2008 and 2010. Much of this was written for compilation into [LaTeX](https://www.latex-project.org/) and ultimately PDF, using [Sweave](https://www.statistik.lmu.de/~leisch/Sweave/).

[__Statistical Data Analysis__](StatisticalAnalysis/): A couple of courseworks from this first year topic that covered multivariate analysis and generalized linear models. The first coursework covers some [multivariate analysis](https://en.wikipedia.org/wiki/Multivariate_analysis): multivariate hypothesis testing, PCA and the like. The second is all about [generalized linear models](https://en.wikipedia.org/wiki/Generalized_linear_model).

[__Modern Statistical Methods__](ModernStatisticalMethods/): This second year course was all about (Monte Carlo) [simulation](https://en.wikipedia.org/wiki/Monte_Carlo_method), [kernel density estimation](https://en.wikipedia.org/wiki/Kernel_density_estimation), [exact tests](https://en.wikipedia.org/wiki/Exact_test), [bootstrap](https://en.wikipedia.org/wiki/Bootstrapping_(statistics))/[jackknife](https://en.wikipedia.org/wiki/Jackknife_resampling) resampling, and [Markov Chain Monte Carlo](https://en.wikipedia.org/wiki/Markov_chain_Monte_Carlo) methods.

[__Statistical Data Mining__](StatisticalDataMining/): A second year course about the statistical approach to machine learning. The task was to try fitting various supervised learning models to the Pima Indian diabetes [dataset](https://archive.ics.uci.edu/ml/datasets/Pima+Indians+Diabetes) (contained in the [`MASS`](https://cran.r-project.org/web/packages/MASS/index.html) package in R):
* [Linear](https://en.wikipedia.org/wiki/Linear_discriminant_analysis)/[quadratic](https://en.wikipedia.org/wiki/Quadratic_classifier#Quadratic_discriminant_analysis) discriminant analysis
* [Logistic regression](https://en.wikipedia.org/wiki/Logistic_regression)
* [Generalized additive model](https://en.wikipedia.org/wiki/Generalized_additive_model)
* [Classification tree](https://en.wikipedia.org/wiki/Decision_tree_learning)
* [_k_-nearest neighbours](https://en.wikipedia.org/wiki/K-nearest_neighbors_algorithm)
* [Neural network](https://en.wikipedia.org/wiki/Artificial_neural_network)

[__Final research project__](FinalResearchProject/): Final year research project where I investigated the empirical properties of range-based (OHLC) volatility estimators as compared with return-based and realized volatility estimators using a data set of 1 minute EURUSD OHLC prices. The paper that I synthesized from this research won the first place [departmental prize](http://bit.ly/1Pysys7) for research sponsored by [Winton Capital](https://www.wintoncapital.com/en/home).
