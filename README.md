
# One/Two sample t-tests for Ames Housing data set,Iowa



# Exploration of all variables that are available for analysis.#
# %let statements define macro variables containing lists of dataset variables#

%let categorical=House_Style Overall_Qual Overall_Cond Year_Built 
         Fireplaces Mo_Sold Yr_Sold Garage_Type_2 Foundation_2 
         Heating_QC Masonry_Veneer Lot_Shape_2 Central_Air
         
%let interval=SalePrice Log_Price Gr_Liv_Area Basement_Area 
         Garage_Area Deck_Porch_Area Lot_Area Age_Sold Bedroom_AbvGr 
         Full_Bathroom Half_Bathroom Total_Bathroom ;
         
         
         
        
# PROC FREQ is used with categorical variables#
ods graphics;

proc freq data=STAT1.ameshousing3;
    tables &categorical / plots=freqplot ;
    format House_Style $House_Style.
           Overall_Qual Overall.
           Overall_Cond Overall.
           Heating_QC $Heating_QC.
           Central_Air $NoYes.
           Masonry_Veneer $NoYes.
           ;
    title "Categorical Variable Frequency Analysis";
run; 



# PROC UNIVARIATE provides summary statistics and plots for interval variables.#
# The ODS statement specifies that only the histogram be displayed.#
# The INSET statement requests summary statistics without having to print out tables.#

ods select histogram;
proc univariate data=STAT1.ameshousing3 noprint;
    var &interval;
    histogram &interval / normal kernel;
    inset n mean std / position=ne;
    title "Interval Variable Distribution Analysis";
run;


# To perform a 1-sample t-test using SAS#

ods graphics;

proc ttest data = STAT1.ameshousing3
           plots(shownull) =  interval
           H0 = 135000
     var = SalePrice;
     title "One Sample t-test testing whether mean SalePrice = $135000
     
# Two-Sample t-test Comparing Masonry Veneer, No vs. Yes"    
ods graphics;

proc ttest data=STAT1.ameshousing3 plots(shownull)=interval;
    class Masonry_Veneer;
    var SalePrice;
    format Masonry_Veneer $NoYes.;
    title "Two-Sample t-test Comparing Masonry Veneer, No vs. Yes";
run;

title;



# Used to to box plots to see the association between the categorical predictor variables and the continuous response variable*/

# PROC SGPLOT is used to explore relationships among categorical variables(X) and response variable(Y)*/

proc sgplot data=STAT1.ameshousing3;
    vbox SalePrice / category=Central_Air 
                     connect=mean;
    title "Sale Price Differences across Central Air";
run;


# PROC SGSCATTER is used to explore relationships among continuous variables#
# using scatter plots#
proc sgscatter data=STAT1.ameshousing3;
    plot SalePrice*Gr_Liv_Area / reg;
    title "Associations of Above Grade Living Area with Sale Price";
run;


# To plot multiple scatter plots in the same plot then use the code as below
%let interval=Gr_Liv_Area Basement_Area Garage_Area Deck_Porch_Area 
         Lot_Area Age_Sold Bedroom_AbvGr Total_Bathroom;

/*PROC SGSCATTER is used to explore relationships among continuous variables*/
/*using scatter plots*/
options nolabel;
proc sgscatter data=STAT1.ameshousing3;
    plot SalePrice*(&interval) / reg;
    title "Associations of Interval Variables with Sale Price";
run;



# To perform a one Way ANOVA model#

ods graphics;

proc glm data=STAT1.ameshousing3 plots=diagnostics;
    class Heating_QC;
    model SalePrice=Heating_QC; # dependent and independent variables
    means Heating_QC / hovtest=levene; #to use levene test to test for homogenity
    format Heating_QC $Heating_QC.;
    title "One-Way ANOVA with Heating Quality as Predictor";
run;
quit;

title;

# To do a pair-wise comparison to see which of means of the categories are not equal, we do a post-hoc analysis using Tukey & Dunnet plots and diffograms
ods graphics;

ods select lsmeans diff diffplot controlplot;
proc glm data=STAT1.ameshousing3 
         plots(only)=(diffplot(center) controlplot);
    class Heating_QC;
    model SalePrice=Heating_QC;
    lsmeans Heating_QC / pdiff=all 
                         adjust=tukey;
    lsmeans Heating_QC / pdiff=control('Average/Typical') 
                         adjust=dunnett;
    format Heating_QC $Heating_QC.;
    title "Post-Hoc Analysis of ANOVA - Heating Quality as Predictor";
run;
quit;

title;

# To check the correlation and the corresponding scatter plots
%let interval=Gr_Liv_Area Basement_Area Garage_Area Deck_Porch_Area 
         Lot_Area Age_Sold Bedroom_AbvGr Total_Bathroom;

ods graphics / reset=all imagemap;
proc corr data=STAT1.AmesHousing3 rank
          plots(only)=scatter(nvar=all ellipse=none);
   var &interval;
   with SalePrice;
   id PID;
   title "Correlations and Scatter Plots with SalePrice";
run;


# A code to run a Simple Linear Regression
ods graphics;

proc reg data=STAT1.ameshousing3;
    model SalePrice=Lot_Area;
    title "Simple Regression with Lot Area as Regressor";
run;
quit;

title;


# A code to run a Multiple Linear Regression

ods graphics on;

proc reg data=STAT1.ameshousing3 ;
    model SalePrice=Basement_Area Lot_Area;
    title "Model with Basement Area and Lot Area";
run;
quit;

/*st103d03.sas*/  /*Part B*/
proc glm data=STAT1.ameshousing3 
         plots(only)=(contourfit);
    model SalePrice=Basement_Area Lot_Area;
    store out=multiple;
    title "Model with Basement Area and Gross Living Area";
run;
quit;

/*st103d03.sas*/  /*Part C*/
proc plm restore=multiple plots=all;
    effectplot contour (y=Basement_Area x=Lot_Area);
    effectplot slicefit(x=Lot_Area sliceby=Basement_Area=250 to 1000 by 250);
run; 

title;


# Model Selection - Forward/Backward/Stepwise Regressions
%let interval=Gr_Liv_Area Basement_Area Garage_Area Deck_Porch_Area 
         Lot_Area Age_Sold Bedroom_AbvGr Total_Bathroom ;

/*st104d01.sas*/
ods graphics on; 
proc glmselect data=STAT1.ameshousing3 plots=all;
	STEPWISE: model SalePrice = &interval / selection=stepwise details=steps select=SL slstay=0.05 slentry=0.05;
	title "Stepwise Model Selection for SalePrice - SL 0.05";
run;


/*Optional Code that will execute forward and backward selection
  Each with slentry and slstay = 0.05.

proc glmselect data=STAT1.ameshousing3 plots=all;
	FORWARD: model SalePrice = &interval / selection=forward details=steps select=SL slentry=0.05;
	title "Forward Model Selection for SalePrice - SL 0.05";
run;

proc glmselect data=STAT1.ameshousing3 plots=all;
	BACKWARD: model SalePrice = &interval / selection=backward details=steps select=SL slstay=0.05;
	title "Backward Model Selection for SalePrice - SL 0.05";
run;
*/
# AIC,AICC,BIC,SBC - IN any of these cases the smaller the value the better the model

%let interval=Gr_Liv_Area Basement_Area Garage_Area Deck_Porch_Area 
         Lot_Area Age_Sold Bedroom_AbvGr Total_Bathroom ;

/*st104d02.sas*/
ods graphics on;
proc glmselect data=STAT1.ameshousing3 plots=all;
	STEPWISEAIC: model SalePrice = &interval / selection=stepwise details=steps select=AIC;
	title "Stepwise Model Selection for SalePrice - AIC";
run;

proc glmselect data=STAT1.ameshousing3 plots=all;
	STEPWISEBIC: model SalePrice = &interval / selection=stepwise details=steps select=BIC;
	title "Stepwise Model Selection for SalePrice - BIC";
run;

proc glmselect data=STAT1.ameshousing3 plots=all;
	STEPWISEAICC: model SalePrice = &interval / selection=stepwise details=steps select=AICC;
	title "Stepwise Model Selection for SalePrice - AICC";
run;

proc glmselect data=STAT1.ameshousing3 plots=all;
	STEPWISESBC: model SalePrice = &interval / selection=stepwise details=steps select=SBC;
	title "Stepwise Model Selection for SalePrice - SBC";
run;

