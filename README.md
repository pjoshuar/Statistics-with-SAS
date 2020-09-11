# Statistics-with-SAS
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



# To perform a one Way ANOVA model#

ods graphics;

proc glm data=STAT1.ameshousing3 plots=diagnostics;
    class Heating_QC;
    model SalePrice=Heating_QC;
    means Heating_QC / hovtest=levene;
    format Heating_QC $Heating_QC.;
    title "One-Way ANOVA with Heating Quality as Predictor";
run;
quit;

title;


# To plot 
proc sgplot data=STAT1.ameshousing3;
    vbox SalePrice / category=Central_Air 
                     connect=mean;
    title "Sale Price Differences across Central Air";
run;


