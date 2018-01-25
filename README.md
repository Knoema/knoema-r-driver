Interface to the Knoema API
========

This is the official documentation for Knoema's R Package. The package can be used for obtaining data from the datasets from the site knoema.com.

# Installation

To install the [devtools](https://cran.r-project.org/package=devtools) package:

    install.packages("devtools")
    library("devtools")
    install_github("Knoema/knoema-r-driver")
    
# CRAN
To install the most recent package from CRAN:

    install.packages("Knoema")
    library("Knoema")
    
Note: the CRAN version migth not reflect the latest changes made to this package. If you are interested in the latest changes, use the version from the github.  

# Authentication
By default, the package allows you to work only with public datasets from the site knoema.com and has a limit on the number of requests.
To make full use of the package we recommend you use parameters client.id and client.secret. You can get these parameters after registering on the site knoema.com, in the section "My profile - Apps - create new" (or use existing applications). For a quick call you can use the link https://knoema.com/user/apps. 
If on this page you have some applications - open one of them or create a new one. You can see the parameters client id and client secret at the bottom of the page and then use them in the functions. How to use these parameters in the functions will be shown below.

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
  
    data = Knoema("IMFWEO2017Oct", list("country" = "914;512;111", "subject" = "lp;ngdp"))
    
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

    data = Knoema("IMFWEO2017Oct",list (country = "914;512;111", subject = "lp;ngdp", frequency = "A", timerange = "2007-2017"))
    
The package supports such formats as "ts", "xts" and "zoo", "DataFrame", "DataTable", "MetaDataFrame", "MetaDataTable". By default type is equal "ts". How to use the type shown in the example below:

    data = Knoema("IMFWEO2017Oct",list (country = "914;512;111", subject = "lp;ngdp"), type = "zoo") 
    
In order to get access to private datasets please use parameters client.id and client.secret in a function:

    data = Knoema("MEI_BTS_COS_2015", list(location = "AT;AU", subject = "BSCI", measure = "blsa", frequency = "Q;M"), type = "DataFrame", client.id = "some client id", client.secret = "some client secret")

# Searching by mnemonics

The search by mnemonics is implemented in knoema. Mnemonics is a unique identifier of the series. Different datasets can have the same series with the same mnemonics. In this case, in the search results there will be a series that was updated last. The same series can have several mnemonics at once, and you can search for any of them. 
An example of using the search for mnemonics::

    data = Knoema('dataset_id', mnemonics = 'mnemonic1;mnemonic2')

If you are downloading data by mnemonics without providing dataset id, you can use this example::

    data = Knoema(mnemonics = 'mnemonic1;mnemonic2')


# Possible errors in Knoema package and how to avoid them
1.  Error: "Client error: (403) Forbidden"
This error appears in the following cases:
1.1. when you use public user (without client.id and client.secret parameters set) and reached the limit of requests.
1.2  when you use client.id and client.secret parameters set, and reached the limit of requests.
1.3  when you use client.id and client.secret, but they are incorrect.

You can avoid these errors, using correct parameters client.id and client.secret

2.  Error: "dataset.id should be a string. Can't be NULL"
    Error: "dataset.id should be a string. Can't be double"
These errors appear when you use NULL or number in place of dataset's Id parameter.
Examples:

    Knoema(NULL)
    Knoema(123)

3.  Error: "The function does not support specifying mnemonics and selection in a single call"
This error appears when you use mnemonics and selection in one query.
Example::

    Knoema('IMFWEO2017Oct', selection = list(country ='912', subject='lp'), mnemonics = 'some_mnemonic')
    Knoema(selection = list(country = 'USA'), mnemonics = 'some_mnemonic')    

4. Error: "Dimension with id or name *some_name_of_dimension* is not found"
This error appears when you use name that doesn't correspond to any existing dimensions' names or ids.
Example:

    Knoema('IMFWEO2017Oct', list(dimension_not_exist='914', subject='lp')

5. Error: "Selection for dimension *dimension_name* is empty"
This error appears when you use empty selection for dimension or all specified elements don't exist.
Examples:

    Knoema('IMFWEO2017Oct', list(country ='', subject='lp'))
    Knoema('IMFWEO2017Oct', list('country'='914', 'subject'='nonexistent_element1; nonexistent_element2'))

6. Error: "The following frequencies are not correct: *list of frequencies*"
This error appears when you use frequencies that don't correspond to supported formats.
Example:

    Knoema("IMFWEO2017Oct", list(country = "914", subject = "LP", frequency = "A;nonexistent_frequency"))
    
We support only following abbreviations of frequencies - A, H, Q, M, W, D.

7. Error: "Requested dataset doesn't exist or you don't have access to it"
This error appears when you use dataset that doesn't exist or you don't have access rights to it.
Example:

    Knoema("IMFWEO2017Apr", list(country = "914", subject = "LP"))
    
This dataset doesn't exist. If your dataset exist, and you have access to it, check that you set client.id and client.secret parameters

8. Error: "Underlying data is very large. Can't create visualization"
This error appears when you use a big selection. Try to reduce the selection.

9. Error: "The specified host *incorect_host* doesn't exist"
This error can appear when you use host that doesn't exist.
Example:

     Knoema("IMFWEO2017Apr", list(country = "914", subject = "LP"), host='knoema_incorect.com')


