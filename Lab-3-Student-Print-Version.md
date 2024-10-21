Spatial Autocorrelation Tutorial Geography 418 Assignment 3
================
James Colalillo
October 20, 2024

## Introduction

In this tutorial we will be learning how to complete tests accompanying
spatial autocorrelation in R. We will also be creating a markdown file
of the code. Which will include an explanation of what is occurring in
each section.The tests being completed include a weighted neighbourhood
matrix, Global Morans I, and a Local spatial autocorrelation. This is an
important skill to have as it can be extremely helpful in analyzing
spatial data. It was found in a study by Xia et al that there was a
significant positive relationship between land use density and urban
vitality. Meaning socio-economic activity was far more prevalent in
densely populated areas. This is the tip of the iceberg for SAC (spatial
autocorellation) as we can look for trends between many different
variables to find geographic patterns(2020.) In this context it can help
in the planning and development of cities or re-imagining of pre
existing spaces depending on the clustering of other elements. ()

Libraries are directories that stores packages. R includes several
standard packages but others must be downloaded and installed into the
session depending on what the user is attempting to do. A package is a
collection of pre written code that includes functions and data in a
pre-determined format. Packages must be installed using the
install.packages function, the programmer must specify which package
they need. After installation you must call the package via the library
function. A Library may include multiple packages. We must also choose
then set a directory, A directory allows you to choose a filepath where
files for this project will be kept, as well as where anything generated
by the code will be saved. First choose the directory and assign it to
the variable dir using the dir command and then set that variable as the
directory with the setwd(dir) command.

``` r
options(repos = c(CRAN = "https://cloud.r-project.org/"))

#Install packages if not already installed:
#install.packages("knitr")
#install.packages("tmap")
#install.packages("spdep")
#install.packages("raster")
#install.packages("shinyjs")
#install.packages("e1071")
#install.packages("sf")

#Load in libraries:
library(tmaptools)
library(knitr)
library(tmap)
library(spdep)
library(raster)
library(shinyjs)
library(e1071)
library(sf)
dir <- "C:/Users/user/Documents/Geog418/418Assignment3"
setwd(dir)
```

Next we will need to bring in the data to R which we will use for the
project. This includes the census data in a csv (spreadsheet) file, a
shapefile which will contain the geography of census tracts and their
boundaries. Depending on where you are sourcing your data you will need
to download it and place it in your folder that you selected for your
directory above. To call the data attached to each tract we will use the
read.csv command and assign it to the variable csv. Next we will do the
same with the st_read command to read the features from our shapefile
including the census boundaries and assign it to the variable shp.

``` r
#From the working dir read in the csv
csv <- read.csv("ucgsJQnBVLvP_data.csv") 

#Data source is the working dir (where the layer is), layer is the name of the file (without .shp)
shp <- st_read("lda_000a16a_e.shp") 
```

    ## Reading layer `lda_000a16a_e' from data source 
    ##   `C:\Users\user\Documents\Geog418\418Assignment3\lda_000a16a_e.shp' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 56590 features and 22 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 3658201 ymin: 658873 xmax: 9019157 ymax: 6083005
    ## Projected CRS: PCS_Lambert_Conformal_Conic

Above you can see the information attached to the files we have called,
it includes lots of important information about the file. We will now
wnat to ‘clean’ our data to improve its ease of use. We need to build a
vector which assigns column names so we are able to know what data we
are referencing. This is done by the ‘c’ command which creates the
vector and we name the columns in the order of the data in the CSV. we
assign this information to the cols variable. Next we will need to apply
these column names to the csv via the colnames command. Now we will
remove unwated values by counting the number of ID characters in the GEO
UID column via the csv#len command. After counting we can remove the
ID’s with more characters than desired via the csv_clean command, this
is done by creating a subset which chooses all GEO UID’s with less than
the desired number of characters. Next we must merge our csv and
shapefile so that they align with each other via the merge command.
After we can select the region we are interested in, for this example I
have chosen to use the city of Saskatoon. Lastly we will create a rate,
for this example we will assess the percentage of the population with
french language knowledge. This can be achieved by dividing the
population with french language ability by the sample size and multiply
it by 100.

``` r
#New column names
cols <- c("GEO UID", "Province code", "Province name", "CD code",
        "CD name", "DA name", "Population", "Land area", 
        "Median total income", "Income Sample Size", "French Knowledge", 
        "Language Sample Size")

#Apply those names to dataframe
colnames(csv) <- cols

#Add column to count number of ID charactors
csv$len <- nchar(csv$`GEO UID`)

#Remove IDs with less than 8 numbers
csv_clean <- subset(csv, csv$len == 8)

#Merge spatial and aspatial data
census_DAs <- merge(shp, csv_clean, 
                    by.x = "DAUID", 
                    by.y = "GEO UID", 
                    all.x = TRUE)

#Subset for Saskatoon
Municp <- subset(census_DAs, census_DAs$CMANAME == "Saskatoon")

#Convert to rate
Municp$PercFrench <- (Municp$`French Knowledge`/Municp$`Language Sample Size`)*100
```

In order to ensure the accuracy of our data before we being our analyses
we must remove any values of ‘0’ or ‘NA’ as these could influence our
results. We need to remove any polygons that only contain the
aforementioned values. specifically in the median total income or
knowledge of french. This will be done via a simple command where we
name the data source (in this case municp) and then use the statement
which(!is.na(municp\$‘columnname’)) the dollar sign here is to separate
the source from the column in the call. We then assign this to a
variable for median total income it will be Income_noNA and for
percentfrench it will be French_noNA. Notice the names of our variables
so far generally are short form descriptions of what the variable holds.
This is good practice when working code to help us to remember what a
variable contains.

``` r
#Remove Income NA
Income_noNA <- Municp[which(!is.na(Municp$`Median total income`)),]
#Remove French NA
French_noNA <- Municp[which(!is.na(Municp$`PercFrench`)),]
```

Now we will calculate basic statistics for both variables. R can
complete these calculations easily using ‘mean’ to calculate a mean, sd
for standard deviation and skewness to look for a skew. in each line of
code we assign the results to an appropriately named variable and the
command again uses the dollar sign to split the source and column name,
we also include the na.rm = true command to ensure that any empty values
are removed. lastly we will create a table. First we need to create the
data frame to hold the table, then select titles for the columsn and
then for aesthetics we will round the values previously calculated. Then
we can produce the table via the kable command and gve it a title.

``` r
#Calculate descriptive stats for Income
meanIncome <- mean(Income_noNA$`Median total income`,na.rm = TRUE)
stdevIncome <- sd(Income_noNA$`Median total income`,na.rm = TRUE)
skewIncome <- skewness(Income_noNA$`Median total income`,na.rm = TRUE)

#Calculate descriptive stats for French
meanFrench <- mean(French_noNA$`PercFrench`,na.rm = TRUE)
stdevFrench <- sd(French_noNA$`PercFrench`,na.rm = TRUE)
skewFrench <- skewness(French_noNA$`PercFrench`,na.rm = TRUE)

head(Income_noNA)
```

    ## Simple feature collection with 6 features and 35 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 5209526 ymin: 1895493 xmax: 5211984 ymax: 1899304
    ## Projected CRS: PCS_Lambert_Conformal_Conic
    ##          DAUID PRUID       PRNAME CDUID          CDNAME CDTYPE  CCSUID
    ## 41716 47110021    47 Saskatchewan  4711 Division No. 11    CDR 4711066
    ## 41717 47110022    47 Saskatchewan  4711 Division No. 11    CDR 4711066
    ## 41718 47110027    47 Saskatchewan  4711 Division No. 11    CDR 4711066
    ## 41719 47110028    47 Saskatchewan  4711 Division No. 11    CDR 4711066
    ## 41720 47110029    47 Saskatchewan  4711 Division No. 11    CDR 4711066
    ## 41721 47110030    47 Saskatchewan  4711 Division No. 11    CDR 4711066
    ##         CCSNAME  CSDUID   CSDNAME CSDTYPE ERUID            ERNAME SACCODE
    ## 41716 Saskatoon 4711066 Saskatoon      CY  4730 Saskatoon--Biggar     725
    ## 41717 Saskatoon 4711066 Saskatoon      CY  4730 Saskatoon--Biggar     725
    ## 41718 Saskatoon 4711066 Saskatoon      CY  4730 Saskatoon--Biggar     725
    ## 41719 Saskatoon 4711066 Saskatoon      CY  4730 Saskatoon--Biggar     725
    ## 41720 Saskatoon 4711066 Saskatoon      CY  4730 Saskatoon--Biggar     725
    ## 41721 Saskatoon 4711066 Saskatoon      CY  4730 Saskatoon--Biggar     725
    ##       SACTYPE CMAUID CMAPUID   CMANAME CMATYPE      CTUID  CTNAME   ADAUID
    ## 41716       1    725   47725 Saskatoon       B 7250021.05 0021.05 47110602
    ## 41717       1    725   47725 Saskatoon       B 7250021.05 0021.05 47110602
    ## 41718       1    725   47725 Saskatoon       B 7250021.05 0021.05 47110602
    ## 41719       1    725   47725 Saskatoon       B 7250021.05 0021.05 47110602
    ## 41720       1    725   47725 Saskatoon       B 7250021.05 0021.05 47110602
    ## 41721       1    725   47725 Saskatoon       B 7250021.04 0021.04 47110602
    ##       Province code Province name CD code         CD name DA name Population
    ## 41716            47  Saskatchewan      11 Division No. 11      21        741
    ## 41717            47  Saskatchewan      11 Division No. 11      22        345
    ## 41718            47  Saskatchewan      11 Division No. 11      27        562
    ## 41719            47  Saskatchewan      11 Division No. 11      28        401
    ## 41720            47  Saskatchewan      11 Division No. 11      29        385
    ## 41721            47  Saskatchewan      11 Division No. 11      30        628
    ##       Land area Median total income Income Sample Size French Knowledge
    ## 41716      2.83               60672                595               65
    ## 41717      0.10               53376                270               10
    ## 41718      0.15               53568                420               40
    ## 41719      0.05               35392                255               20
    ## 41720      0.15               41600                305               30
    ## 41721      0.16               46635                500               55
    ##       Language Sample Size len                       geometry PercFrench
    ## 41716                  690   8 POLYGON ((5211542 1897758, ...   9.420290
    ## 41717                  330   8 POLYGON ((5211232 1896835, ...   3.030303
    ## 41718                  520   8 POLYGON ((5210021 1896913, ...   7.692308
    ## 41719                  325   8 POLYGON ((5210170 1896323, ...   6.153846
    ## 41720                  415   8 POLYGON ((5210137 1896253, ...   7.228916
    ## 41721                  605   8 POLYGON ((5210125 1896206, ...   9.090909

``` r
head(French_noNA)
```

    ## Simple feature collection with 6 features and 35 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 5209526 ymin: 1895493 xmax: 5211984 ymax: 1899304
    ## Projected CRS: PCS_Lambert_Conformal_Conic
    ##          DAUID PRUID       PRNAME CDUID          CDNAME CDTYPE  CCSUID
    ## 41716 47110021    47 Saskatchewan  4711 Division No. 11    CDR 4711066
    ## 41717 47110022    47 Saskatchewan  4711 Division No. 11    CDR 4711066
    ## 41718 47110027    47 Saskatchewan  4711 Division No. 11    CDR 4711066
    ## 41719 47110028    47 Saskatchewan  4711 Division No. 11    CDR 4711066
    ## 41720 47110029    47 Saskatchewan  4711 Division No. 11    CDR 4711066
    ## 41721 47110030    47 Saskatchewan  4711 Division No. 11    CDR 4711066
    ##         CCSNAME  CSDUID   CSDNAME CSDTYPE ERUID            ERNAME SACCODE
    ## 41716 Saskatoon 4711066 Saskatoon      CY  4730 Saskatoon--Biggar     725
    ## 41717 Saskatoon 4711066 Saskatoon      CY  4730 Saskatoon--Biggar     725
    ## 41718 Saskatoon 4711066 Saskatoon      CY  4730 Saskatoon--Biggar     725
    ## 41719 Saskatoon 4711066 Saskatoon      CY  4730 Saskatoon--Biggar     725
    ## 41720 Saskatoon 4711066 Saskatoon      CY  4730 Saskatoon--Biggar     725
    ## 41721 Saskatoon 4711066 Saskatoon      CY  4730 Saskatoon--Biggar     725
    ##       SACTYPE CMAUID CMAPUID   CMANAME CMATYPE      CTUID  CTNAME   ADAUID
    ## 41716       1    725   47725 Saskatoon       B 7250021.05 0021.05 47110602
    ## 41717       1    725   47725 Saskatoon       B 7250021.05 0021.05 47110602
    ## 41718       1    725   47725 Saskatoon       B 7250021.05 0021.05 47110602
    ## 41719       1    725   47725 Saskatoon       B 7250021.05 0021.05 47110602
    ## 41720       1    725   47725 Saskatoon       B 7250021.05 0021.05 47110602
    ## 41721       1    725   47725 Saskatoon       B 7250021.04 0021.04 47110602
    ##       Province code Province name CD code         CD name DA name Population
    ## 41716            47  Saskatchewan      11 Division No. 11      21        741
    ## 41717            47  Saskatchewan      11 Division No. 11      22        345
    ## 41718            47  Saskatchewan      11 Division No. 11      27        562
    ## 41719            47  Saskatchewan      11 Division No. 11      28        401
    ## 41720            47  Saskatchewan      11 Division No. 11      29        385
    ## 41721            47  Saskatchewan      11 Division No. 11      30        628
    ##       Land area Median total income Income Sample Size French Knowledge
    ## 41716      2.83               60672                595               65
    ## 41717      0.10               53376                270               10
    ## 41718      0.15               53568                420               40
    ## 41719      0.05               35392                255               20
    ## 41720      0.15               41600                305               30
    ## 41721      0.16               46635                500               55
    ##       Language Sample Size len                       geometry PercFrench
    ## 41716                  690   8 POLYGON ((5211542 1897758, ...   9.420290
    ## 41717                  330   8 POLYGON ((5211232 1896835, ...   3.030303
    ## 41718                  520   8 POLYGON ((5210021 1896913, ...   7.692308
    ## 41719                  325   8 POLYGON ((5210170 1896323, ...   6.153846
    ## 41720                  415   8 POLYGON ((5210137 1896253, ...   7.228916
    ## 41721                  605   8 POLYGON ((5210125 1896206, ...   9.090909

``` r
#Create dataframe for display in table
data <- data.frame(Variable = c("Income", "French Language"),
                   Mean = c(round(meanIncome,2), round(meanFrench,2)),
                   StandardDeviation = c(round(stdevIncome,2), round(stdevFrench,2)),
                   Skewness = c(round(skewIncome,2), round(skewFrench,2)))

#Produce table
kable(data, caption = paste("Descriptive statistics for Saskatoon ", 2016, " census variables"))
```

| Variable        |     Mean | StandardDeviation | Skewness |
|:----------------|---------:|------------------:|---------:|
| Income          | 40275.61 |           8856.61 |     0.04 |
| French Language |     6.39 |              4.03 |     0.68 |

Descriptive statistics for Saskatoon 2016 census variables

Now we are going to create maps using R to show the median total income
by census tract and percentage of surveyed population with french
language knowledge. We can choose different pallettes using the palette
explorer tool below which will open a menu of color options. Then we
assign the pallet via the tm_polygons command. Within tm_polygons we
will also set all the necessary features for the map including which
column we are calling for the maps data, its title, the data division
style and borders. Then we call the tm_layout command to choose a
position for the legend. Lastly we print the maps and arrange them with
the tmap_arrange command.

``` r
#Choose a pallete
#tmaptools::palette_explorer() #Tool for selecting pallettes


#Map median Income
map_Income <- tm_shape(Income_noNA) + 
  tm_polygons(col = "Median total income", 
              title = "Median total income", 
              style = "jenks", 
              palette = "Oranges", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("RIGHT", "TOP"))

#Map French Knowledge
map_French <- tm_shape(French_noNA) + 
  tm_polygons(col = "PercFrench", 
              title = "Percentage with \n French Knowledge", 
              style = "jenks", 
              palette = "Oranges", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("RIGHT", "TOP"))

#Print maps side by side
tmap_arrange(map_Income, map_French, ncol = 2, nrow = 1)
```

<figure>
<img src=StudyArea-1.png"
alt="Saskatoon census dissemination areas showing median total income (left) and percentage of respondants with knowledge of french (right)." />
<figcaption aria-hidden="true">Saskatoon census dissemination areas
showing median total income (left) and percentage of respondants with
knowledge of french (right).</figcaption>
</figure>

## Neighbourhood matrix

This portion of the project will create maps using neighbourhood matrix
weighting. A weighted neighbourhood matrix uses calculations to
represent the relationship between geographic areas such as the census
tracts in this example. It measures how connected each polygon or tract
is to each other by seeing if they influence each other. The key
components of a weighted matrix include the structure of the matrix, the
weight (binary or continous) and the type of weighting. The matrix
structure will define the grid being overlayed over the area being
studied. The weighting can be binary where a 1 could indicate
neighboring squares or a 0 if they are not, when continous a larger
number would indicate stronger relation. Lastly we need to decide which
type of weighting will be used, for the below example we will be
comparing rook and queen weighting. Rook weighting will consider all
census tracts that share a border to be neighbours. In queen weighting
any shared borders will still be considered neighbours but also any
adjacent corners.

The code below will create a list of neighbours from the list of
polygons via the poly2nb function. It creates the list by choosing
neighbours that share boundaries. By default this will be done via the
Queen weighting where a single point constitutes a neighbour, if
switched off via the queen = false command then it will default to the
rook weighting type.

``` r
#Income Neighbours - Queens weight
Income.nb <- poly2nb(Income_noNA)
# Use st_coordinates to get the coordinates
Income.net <- nb2lines(Income.nb, coords=st_coordinates(st_centroid(Income_noNA)))

#Income Neighbours - Rooks weight
Income.nb2 <- poly2nb(Income_noNA, queen = FALSE)
Income.net2 <- nb2lines(Income.nb2, coords=st_coordinates(st_centroid(Income_noNA)))
crs(Income.net2) <- crs(Income_noNA)

#French Neighbours - Queens weight
French.nb <- poly2nb(French_noNA)
French.net <- nb2lines(French.nb, coords=st_coordinates(st_centroid(French_noNA)))
crs(French.net) <- crs(French_noNA)

#French Neighbours - Rooks weight
French.nb2 <- poly2nb(French_noNA, queen = FALSE)
French.net2 <- nb2lines(French.nb2, coords=st_coordinates(st_centroid(French_noNA)))
crs(French.net2) <- crs(French_noNA)
```

This section of code takes the data we generated in the previous section
by the weight matrix and will transform it into a visual map. We call
the vairable for income in each matrix, and inside it set the parameters
for the map. the commands used are tm_shape which will call the
boundaries from the polygons being analyzed and borders of the map are
set using the tm_borders command. this will be added to the tm_shape of
the variable income.net (1 & 2) where our Neighbour weight data is
stored, the relationships are shown via lines whos parameters are set
with the tm_lines command. lastly the maps are printed via the
tmap_arrange which allows us to choose the placement of the maps based
on the order we call them in and selecting the number of columns and
rows. In the example below we have 3 columns and 1 row making them
horizontally displayed, if the user wanted a vertical display they could
do 3 rows and 1 column.

``` r
#Make queens map
IncomeQueen <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
              tm_shape(Income.net) + tm_lines(col='red')

#Make rooks map
IncomeRook <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
              tm_shape(Income.net2) + tm_lines(col='red', lwd = 2)

#Make combined map
IncomeBoth <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
               tm_shape(Income.net) + tm_lines(col='purple', lwd = 2) +
               tm_shape(Income.net2) + tm_lines(col='blue', lwd = 2)

#Print maps in a three pane figure
tmap_arrange(IncomeQueen, IncomeRook, IncomeBoth, ncol = 3, nrow = 1)
```

<figure>
<img
src="Lab-3-Student-Print-Version_files/figure-gfm/Neighboursmap-1.png"
alt="Saskatoon census dissemination areas showing median total income neighbours queens weight (left) rooks weight (middle) and the combination of the two (right)." />
<figcaption aria-hidden="true">Saskatoon census dissemination areas
showing median total income neighbours queens weight (left) rooks weight
(middle) and the combination of the two (right).</figcaption>
</figure>

Weight matrixes can occur in 3 different styles. In R these correspond
to the characters B, W, and C. They all use a different weighting
scheme. The B scheme is binary where a value of 1 is attributed to a
neighbouring polygon and all other polygons are attributed a 0. A W
matrix will give each neighbour an equal weight where they sum to a
value of 1 this is done in a local context. Lastly a C matrix gives all
neighbours across the global study area an equal weighting that sums to
1.

In the following code a nb2listw function is used which will take your
neighbours list previously created and attach weights based on weighting
scheme chosen in the previous paragraph. In this case we have chosen the
W style which is set with the command ‘Style = W’. We have also included
within the command the ‘zero.policy = TRUE’ this will fix the program by
assigning a value of 0 to any regions lacking a neighbour. Without a
value assigned to those regions the matrix would crash.

``` r
#Create Income weights matrix
Income.lw <- nb2listw(Income.nb, zero.policy = TRUE, style = "W")

#Create French weights matrix
French.lw <- nb2listw(French.nb, zero.policy = TRUE, style = "W")

subset_weights <- head(Income.lw[["weights"]])[c(1:3)]
print(subset_weights)
```

    ## [[1]]
    ## [1] 0.125 0.125 0.125 0.125 0.125 0.125 0.125 0.125
    ## 
    ## [[2]]
    ## [1] 0.2 0.2 0.2 0.2 0.2
    ## 
    ## [[3]]
    ## [1] 0.1666667 0.1666667 0.1666667 0.1666667 0.1666667 0.1666667

``` r
#head(Income.lw[["weights"]])[c(1:3)]
```

## Global Moran’s I

With the previous section completed the program now has a weighting type
and style chosen for our neighbours. We will now be taking that matrix
to calculate the Global Morans I statistic, this is a global test of
spatial autocorrelation. The following equation is how it is calculated.

$$
I = \frac{\sum_{i=1}^n\sum_{j=1}^nW_{i,j}(x_i - \bar{x})(x_j - \bar{x})}{(\sum_{i=1}^n\sum_{j=1}^nW_{i,j})\sum_{i=1}^n(x_i - \bar{x})^2}
$$

``` r
#Calculate Global Moran's I for Income
miIncome <- moran.test(Income_noNA$`Median total income`, Income.lw, zero.policy = TRUE)

#Extract Global Moran's I results for Income
mIIncome <- miIncome$estimate[[1]]
eIIncome <- miIncome$estimate[[2]]
varIncome <- miIncome$estimate[[3]]

#Calculate Global Moran's I for French
miFrench <- moran.test(French_noNA$PercFrench, French.lw, zero.policy = TRUE)

#Extract Global Moran's I results for French
mIFrench <- miFrench$estimate[[1]]
eIFrench <- miFrench$estimate[[2]]
varFrench <- miFrench$estimate[[3]]
```

The results of the above Global Morans I calculations for Income and
French are viewable in the ‘Environment Pane’ under the values section.
mIFrench and mIIncome indicate the morans I statistic and both values
are positive which suggests spatial clustering for both variables
meaning that median values are likely to be grouped together. The
expected values are low indicating our null hypothesis of no SAC to be
false. The variance calculate d for each is extremely low and shows our
observed values much further from our expected values.

``` r
#Function to calculate the range of global Moran's I
moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}

#Calculate the range for the Income variable
range <- moran.range(Income.lw)
minRange <- range[1]
maxRange <- range[2]
```

The range calculated via the range commands has a minimum value of
-0.9131 and a maximum of 1.061. a value of -1 or 1 indicates either
perfect negative SAC or perfect positive SAC. Because our max values are
so close to either there is the possibility of either within the data
step. This does not change our previous interpretation of the Morans I
statistic.

Lets finish the picture were painting by applying a Z test. Our null
hypothesis is that there is no SAC and that our calculated Morans I
value is not significantly different from the expected random value. The
alternative hypothesis is there is SAC and the Morans I value is
significantly different from our expected value. We will input an alpha
value of 0.05, to measure statistical significance we will see if our
score is above or below +/- 1.96, if this is the case then we can assume
statistical significance. a value over +1.96 indicates significant
positive SAC. While a value below -1.96 indicates significant negative
SAC.

We will conduct the Z test with code below.

``` r
#Calculate z-test for Income
zIncome <- (mIIncome - eIIncome) / (sqrt(varIncome))

#Calculate z-test for French
zFrench <- (mIFrench - eIFrench) / (sqrt(varFrench))
```

The zscores for both variable confirm that there is significant positive
SAC for both Income and French. The relationship is stronger with
Income.

## Local spatial autocorrelation

Explain local spatial autocorrelation Moving on from global SAC we will
not examine local spatial autocorrelation. This is a similar concept but
used at a local level that examines small clusters rather than the
entire study area. This allows us to find patterns that differ from the
global median.

Calculating Local Morans I is similar to the global calculation but
arranged in a different manner.

$$
I_i = \frac{x_i - \bar{x}}{S_i^2}\sum{_{j=1}^n}W_{i,j}(x_j - \bar{x})\space \space where \space \space S_i^2 = \frac{\sum_{i=1}^n (x_i - \bar{x})^2}{n-1} 
$$

Again, instead of typing out these calculations, we can use the
localmoran() function to deal with all of the messy calculations for us,
as long as we input our variable and weighting scheme.

We can use R to complete the calculations for us via the localmoran()
function. We will need to provide it with our variables and chosen
weighting style.

``` r
#Calculate LISA test for Income
lisa.testIncome <- localmoran(Income_noNA$`Median total income`, Income.lw)

#Extract LISA test results for Income
Income_noNA$Ii <- lisa.testIncome[,1]
Income_noNA$E.Ii<- lisa.testIncome[,2]
Income_noNA$Var.Ii<- lisa.testIncome[,3]
Income_noNA$Z.Ii<- lisa.testIncome[,4]
Income_noNA$P<- lisa.testIncome[,5]

#Calculate LISA test for Income
lisa.testFrench <- localmoran(French_noNA$PercFrench, French.lw)

#Extract LISA test results for Income
French_noNA$Ii <- lisa.testFrench [,1]
French_noNA$E.Ii<- lisa.testFrench [,2]
French_noNA$Var.Ii<- lisa.testFrench [,3]
French_noNA$Z.Ii<- lisa.testFrench [,4]
French_noNA$P<- lisa.testFrench [,5]
```

We now want to visualize the results for our interpretation using the
mapping function from earlier. We will simply call the new variables
instead and choose appropriate values for aesthetics.

``` r
#Map LISA Z-scores for Income
map_LISA_Income <- tm_shape(Income_noNA) + 
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(Income_noNA$Z.Ii, na.rm=TRUE), -1.96, 1.96, max(Income_noNA$Z.Ii, na.rm=TRUE)),
              palette = "-RdBu", n = 3) +
  tm_compass(position=c("left","top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_legend(position = c("right", "top"))

#Map LISA Z-scores for French
map_LISA_French <- tm_shape(French_noNA) + 
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(French_noNA$Z.Ii, na.rm=TRUE), -1.96, 1.96, max(French_noNA$Z.Ii, na.rm=TRUE)),
              palette = "-RdBu", n = 3) +
  tm_compass(position=c("left","top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_legend(position = c("right", "top"))

#Plot maps in a 2 pane figure
tmap_arrange(map_LISA_Income, map_LISA_French, ncol = 2, nrow = 1)
```

<figure>
<img
src="Lab-3-Student-Print-Version_files/figure-gfm/MappingLocalMoransI-1.png"
alt="Saskatooon census dissemination areas showing LISA z-scores for median total income (left) and percentage of respondants with knowledge of french (right)." />
<figcaption aria-hidden="true">Saskatooon census dissemination areas
showing LISA z-scores for median total income (left) and percentage of
respondants with knowledge of french (right).</figcaption>
</figure>

On the left map we can see the analysis for Income, the red regions
indicate strong significant positive autocorrelation, therefore we can
assume the south and east regions of the city has clustering of incomes
higher than the median. For french there is clustering to the North and
South of above average french speaking knowledge while to the east and
west there is significantly less knowledge than the median, curiously
there is a region of sigficantly less knowledge to the North tucked in
between regions of above average knowledge.

If we would like to take our SAC interpretation even further we can
graph the trends as well. This can be achieved by the following code. It
calls the variable then plots it while following the format outlined for
the x and y axis.

``` r
#Create Moran's I scatter plot for Income
moran.plot(Income_noNA$`Median total income`, Income.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Median Total Income ($)", 
           ylab="Spatially Lagged Median Total Income ($)", quiet=NULL)
```

<figure>
<img
src="Lab-3-Student-Print-Version_files/figure-gfm/MoransIScatter-1.png"
alt="Moran’s I scatter plot for median total income." />
<figcaption aria-hidden="true">Moran’s I scatter plot for median total
income.</figcaption>
</figure>

``` r
#Create Moran's I scatter plot for French
moran.plot(French_noNA$PercFrench, French.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Respondants with knowledge of French (%)", 
           ylab="Spatially Lagged knowledge of French (%)", quiet=NULL)
```

<figure>
<img
src="Lab-3-Student-Print-Version_files/figure-gfm/MoransIScatter2-1.png"
alt="Moran’s I scatter plot for percentage of respondants with knowledge of french." />
<figcaption aria-hidden="true">Moran’s I scatter plot for percentage of
respondants with knowledge of french.</figcaption>
</figure>

In these plots, the points with diamonds are considered statistically
significant, and the regression line shows the overall trend. For both
plots we can see that the trend shows Within both plots we can interpret
the following, points with diamonds surrounding them indicate
statistical significance. The regression line going on a diagonal
indicates the overall trend of the data. In both plots we can see a
positive trend indicating that areas that scored higher than the median
are grouped together. The trend is stronger in Income than french
knowledge. This means there is clustering occuring for both data sets.

## Summary

In this tutorial we have learned about ‘R’ basics including choosing &
loading libraries, setting directories and combining data from 2
different files. We have learned how to complete calculations within R
and creating deliverables including tables, maps and diagrams. We have
also learned about how to complete tests analyzing our data for spatial
autocorrelation. We have completed an example via our saskatoon example
examining income and french knowledge. We used our example data to map
our basic median values for the region. After we completed a weighted
neighbourhood matrix in both the Queen and Rook style to look for
influence from census tracts on their neighbours. We have also
calculated Global Morans I for both data types to observe trends of
positive spatial autocorrelation clustering across the whole study area.
We then changed the scope to local morans I to look for patterns that
may differ from the observed global trend. lastly we mapped and graphed
our findings. Our results from the example indicated strong positive SAC
in Income and moderate positive SAC for french knowledge in the city of
Saskatoon. I hope you have found this tutorial useful.

## References

ESRI. (n.d.). Spatial Autocorrelation (Global Moran’s I) (Spatial
Statistics)—ArcGIS Pro \| Documentation. Pro.arcgis.com.
<https://pro.arcgis.com/en/pro-app/latest/tool-reference/spatial-statistics/spatial-autocorrelation.htm>

Globals. (2024).Nku.edu.
<https://www.nku.edu/~longa/geomed/ppa/doc/globals/Globals.html>

Moraga, P. (n.d.). Chapter 7 Spatial neighborhood matrices \| Spatial
Statistics for Data Science: Theory and Practice with R. In
www.paulamoraga.com.
<https://www.paulamoraga.com/book-spatial/spatial-neighborhood-matrices.html>

Xia, C., Yeh, A. G.-O., & Zhang, A. (2020). Analyzing spatial
relationships between urban land use intensity and urban vitality at
street block level: A case study of five Chinese megacities. Landscape
and Urban Planning, 193, 103669.
<https://doi.org/10.1016/j.landurbplan.2019.103669>
