# R code for scoring psychological testing measures #

For most scales the default is `alpha=F` to return item scores. Pass `alpha=T` if you only want alpha values.

The term `df` is used to denote the input dataframe eg. for the rse the input value `df` could be represented by: `data.frame(matrix(sample.int(15,100, TRUE), ncol=10))` although this example is unlikely to produce valid values for scoring

The number of columns is assumed. For example: in the IPIP 50, it is assumed you matrix will have 50 columns where each column is a response to a question.


## Scales ##

* [Rosenberg Self-Esteem Scale](https://www.wwnorton.com/college/psych/psychsci/media/rosenberg.htm)
  * rsescore<-function(df,alpha=F) 

* IPIP 50 
  * ipip50score<-function(df,alpha=F)

* Narcissistic Personality Inventory 40 (NPI 40)
  * npi40score<-function(df,alpha=F)

* [Narcissistic Personality Inventory 16](http://www.columbia.edu/~da358/npi16/npi16.pdf)
  * npi16score<-function(npi,alpha=F)

* [Big Five Inventory (BFI-44)](https://fetzer.org/sites/default/files/images/stories/pdf/selfmeasures/Personality-BigFiveInventory.pdf)
  * bfi44score<-function(df,alpha=F)

* [Five Factor Model Rating form](http://www.uky.edu/~widiger/ffmrf.doc)
  * ffmrfscore<-function(csv,alpha=F)

* Ten Item Personality Inventory (TIPI)
  * tipiscore<-function(x)

* [Narcissistic Admiration and Rivalry Questionaire](http://www.persoc.net/persoc/uploads/Toolbox/NARQ_English.pdf)
  * narq<-function(x,alpha=F)

* [Short Dark Triad 3](http://www.midss.org/sites/default/files/d3.pdf)
  * sd3_score<-function(x)

## Functions to view data in APA format or calculate P and F from models ##

* Output correlation in APA style
  * apaCorr2<-function(df,round_digits=2)
    * example: apaCorr2(data.frame(sample.int(20,10,replace=T),sample.int(20,10,replace=T)))

* Shortcut function to calculate p-val from a linear model object
  * lmp <- function (modelobject) 
    * example: lmp(lm(csv$bfat~csv$exer))

* takes in linear model outputs: F(dfb,dfw)=fobt,p-val
  * prettyF<-function(x)
    * example: prettyF(lm(csv$bfat~csv$exer))


* get semi-partial correlation
  * partial_df<-function(x,cont)
    * #example call: semi_partial_df(res,"gender") //where the dataframe has a column `gender`
