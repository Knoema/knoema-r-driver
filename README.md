Interface to the Knoema API
========

This is the official documentation for Knoema's R Package. The package can be used for obtaining data from the datasets from the site knoema.com.

# Installation

To install the [devtools](https://cran.r-project.org/package=devtools) package:

    install.packages("devtools")
    library("devtools")
    install_github("Knoema/knoema-r-driver")
    
# CRAN
To install the most recent package from CRAN type:

    install.packages("Knoema")
    library("Knoema")
    
Note: the CRAN version migth not reflect the latest changes made to this package. If you are interested in the latest changes, use the version from the github.  

# Authentication
By default, the package allows you to work only with public datasets from the site knoema.com and has a limit on the number of requests.
To make full use of the package we recommend you set parameters client.id and client.secret. You can get these parameters after registering on the site knoema.com, in the section "My profile - Apps - create new" (or use existing applications).
How to set these parameters will be shown below.

# Retrieving series from datasets
There is one method for retrieving series from datasets in R: the Knoema method. The method works with knoema datasets.

The following quick call can be used to retrieve a timeserie from dataset:

    library("Knoema")
    data = Knoema("IMFWEO2017Apr", list(country = "914", subject = "ngdp"))
   
where:

* "IMFWEO2017Apr" this is a public dataset, that available for all users by reference https://knoema.com/IMFWEO2017Apr.
* country and subject are dimensions names
* "914" is code of country *Albania*
* "ngdp" is code of subject *Gross domestic product, current prices (U.S. dollars)*

This example finds all data points for the dataset IMFWEO2017Apr with selection by country = *Albania* and subject =  *Gross domestic product, current prices (U.S. dollars)* and stores this series in a format ts. 

Please note that you need to identify all dimensions of the dataset, and for each dimension to indicate the selection. Otherwise, the method returns an error.

For multiple selection you can use the next example:
  
    data = Knoema("IMFWEO2017Apr", list("country" = "914;512;111", "subject" = "lp;ngdp"))
    
For case when the dimensions of dataset that have multi word names use the next example:

    data = Knoema("FDI_FLOW_CTRY", list("Reporting country" = "AUS",
                                                    "Partner country/territory" = "w0",
                                                    "Measurement principle" = "DI",
                                                    "Type of FDI" = "T_FA_F",
                                                    "Type of entity" = "ALL",
                                                    "Accounting entry" = "NET",
                                                    "Level of counterpart" = "IMC",
                                                    "Currency" = "USD"))   

In addition to the required using of the selections for dimensions, you can additionally specify the period and frequencies in the parameters. For more details, see the example below:

    data = Knoema("IMFWEO2017Apr",list (country = "914;512;111", subject = "lp;ngdp", frequency = "A", timerange = "2007-2017"))
    
The package supports such formats as "ts", "xts" and "zoo". By default type is equal "ts". How to use the type shown in the example below:

    data = Knoema("IMFWEO2017Apr",list (country = "914;512;111", subject = "lp;ngdp"), type = "zoo") 
    
In order to get access to private datasets please use parameters client.id and client.secret in a function:

    data = Knoema("MEI_BTS_COS_2015", list(location = "AT;AU", subject = "BSCI", measure = "blsa", frequency = "Q;M"), type = "xts", client.id = "some client id", client.secret = "some client secret")

Note: If the function returns an error 403(Forbidden), try using other parameters client.id and client.secret.
