Interface to the Knoema API
========

This is the official documentation for Knoema's R Package. The package can be used for obtaining data from the datasets.

# Installation

There are two ways of installing the package:

1. Receiving it directly from the GitHub
2. Receiving it from CRAN

It is recommended to use GitHub version as it contains most up to date version. Also if you receive the error "400 Bad Request Error" when working with the package from CRAN that means that you need to switch to GitHub package. To do this run **remove.packages("Knoema")** and then use the instruction below to install the package directly from the GitHub.

# GitHub package
To install the latest version from the GitHub use the following code:

    install.packages("devtools")
    library("devtools")
    install_github("Knoema/knoema-r-driver")

> Note: you will need to install [devtools](https://cran.r-project.org/package=devtools) package first if it is not installed.

# CRAN package
To install the most recent package from CRAN:

    install.packages("Knoema")
    library("Knoema")
    
> Note: the CRAN version migth not reflect the latest changes made to this package. If you are interested in the latest changes, use the version from the GitHub.  

# Authentication
To make full use of the package you need to use parameters client.id and client.secret. You can get these parameters after registering on the site knoema.com, in the section _My Knoema - API Access - Apps_, or open the page by direct link: https://knoema.com/user/apps. 

If you have the application created already open it, you will see the fields _Client ID_ and _Client secret_. 
If you don't have the application, click _Create New_ button, add an application _Name_ (_Description_ and _Redirect UR_L are optional) and click _Save_, you will see the fileds _Client ID_ and _Client secret_ on the next screen. You can always get back to that page if you forget the parameters.

> Note: if you're an enterprise user you will need to create an app within your enterprise environment, not on the site knoema.com. You will also need to specify the _host_ parameter. For example if your enterprise solution is located here https://enterprise-portal.knoema.com the host should be _enterprise-portal.knoema.com_ and the app should be created directly on that portal (https://enterprise-portal.knoema.com/user/apps). You will only be able to access the data which is public on that portal or the data to which you have an access.

# Retrieving series from datasets
There is one method for retrieving series from datasets in R: the Knoema method.

The following quick call can be used to retrieve a timeserie from a dataset:

    library("Knoema")
    data = Knoema(
        "WBCC2020", 
        list("location" = "ASM", "indicator" = "AG.LND.AGRI.K2"), 
        client.id = "some client id", 
        client.secret = "some client secret",
        host = "knoema.com")

Where:

* "WBCC2020" this is a dataset ID. Dataset ID can be found from the URL, if you open any dataset you will see it right after the hostname: [https://knoema.com/__WBCC2020__/world-bank-climate-change](https://knoema.com/WBCC2020/world-bank-climate-change).
* location and indicator are dimensions names which can also be taken directly from the Dataset Viewer. Dimension names are not case-sensitive.
* "ASM" is an ID/Code of country *American Samoa* (can be found by clicking on _i_ icon near the country name). You can also use _the name_ of the element as a parameter if the code is not accessible (ex. _"location" = "American Samoa"_).
* "AG.LND.AGRI.K2" is ID/Code of indicator *Agricultural land (sq. km)* (can be found by clicking on _i_ icon near the indicator name). You can also use _the name_ of the element as a parameter.
* client.id and client.secret should contain the _Client ID_ and _Client secret_ of the application you created
* host is an optional parameter if you're working with Knoema.com site, but should be explicitly specified if you're an enterprise user and working with a solution located on a different link.

This example finds all data points for the dataset __WBCC2020__ with selection by location = *American Samoa* and subject =  *Agricultural land (sq. km)* and stores this series in a ts format. 

Please note that you need to identify all dimensions of the dataset, and for each dimension to indicate the selection. Otherwise, the method will return an error.

For multiple selection you can use the next example (use __;__ as a delimeter for dimension elements):

    data = Knoema(
        "WBCC2020", 
        list("Location" = "American Samoa;Australia", "Indicator" = "Agricultural land (sq. km);Forest area (sq. km)"), 
        client.id = "some client id", 
        client.secret = "some client secret",
        host = "knoema.com")

You can also specify the _time range_ and the _frequency_ in the parameters:

    data = Knoema(
        "WBCC2020", 
        list(
            "Location" = "American Samoa;Australia", 
            "Indicator" = "Agricultural land (sq. km);Forest area (sq. km)",
            "frequency" = "A",
            "timerange" = "2010-2015"), 
        client.id = "some client id", 
        client.secret = "some client secret",
        host = "knoema.com")
    
The package supports such formats as "ts", "xts" and "zoo", "DataFrame", "DataTable", "MetaDataFrame", "MetaDataTable". By default type is equal "ts". You can use _the type_ parameter to change it as needed:

    data = Knoema(
        "WBCC2020", 
        list(
            "Location" = "American Samoa;Australia", 
            "Indicator" = "Agricultural land (sq. km);Forest area (sq. km)",
            "frequency" = "A",
            "timerange" = "2010-2015"), 
        type = "zoo", 
        client.id = "some client id", 
        client.secret = "some client secret",
        host = "knoema.com")

# Searching by mnemonics

The search by mnemonics is implemented in knoema. Mnemonics is a unique identifier of the series. Different datasets can have the same series with the same mnemonics. In this case, in the search results there will be a series that was updated last. The same series can have several mnemonics at once, and you can search for any of them. 
An example of using the search for mnemonics:

    data = Knoema(
        "dataset_id", 
        mnemonics = "mnemonic1;mnemonic2", 
        client.id = "some client id", 
        client.secret = "some client secret",
        host = "knoema.com")

If you are downloading data by mnemonics without providing dataset id, you can use this example:

    data = Knoema(
        mnemonics = "mnemonic1;mnemonic2", 
        client.id = "some client id", 
        client.secret = "some client secret",
        host = "knoema.com")

> Note: not all datasets have support for mnemonics

# Possible errors in Knoema package and how to avoid them

1. Error: "Client error: (403) Forbidden"
This error appears in the following cases::
1.1. when you use public user (without client.id and client.secret parameters set) and reached the limit of requests.
1.2  when you use client.id and client.secret parameters set, and reached the limit of requests.
1.3  when you use client.id and client.secret, but they are incorrect.
1.4  when you use incorrect host

You can avoid these errors, using correct parameters of host, client.id and client.secret

2. Error: "dataset.id should be a string. Can't be NULL"
Error: "dataset.id should be a string. Can't be double"
These errors appear when you use NULL or number in place of dataset's Id parameter.
Examples:

        Knoema(NULL)
        Knoema(123)

3. Error: "Dimension with id or name *some_name_of_dimension* is not found"
This error appears when you use name that doesn't correspond to any existing dimensions' names or ids.
Example:

        Knoema('IMFWEO2018Apr', list(dimension_not_exist='914', subject='lp')

4. Error: "Selection for dimension *dimension_name* is empty"
This error appears when you use empty selection for dimension or all specified elements don't exist.
Examples:

        Knoema('IMFWEO2018Apr', list(country ='', subject='lp'))
        Knoema('IMFWEO2018Apr', list('country'='914', 'subject'='nonexistent_element1; nonexistent_element2'))

5. Error: "The following frequencies are not correct: *list of frequencies*"
This error appears when you use frequencies that don't correspond to supported formats.
Example:

        Knoema("IMFWEO2018Apr", list(country = "914", subject = "LP", frequency = "A;nonexistent_frequency"))
    
We support only following abbreviations of frequencies - A, H, Q, M, W, D.

6. Error: "Requested dataset doesn't exist or you don't have access to it"
This error appears when you use dataset that doesn't exist or you don't have access rights to it.
Example:

        Knoema("IMFWEO2018Apr", list(country = "914", subject = "LP"))
    
This dataset doesn't exist. If your dataset exist, and you have access to it, check that you set client.id and client.secret parameters

7. Error: "Underlying data is very large. Can't create visualization"
This error appears when you use a big selection. Try to reduce the selection.

8. Error: "The specified host *incorect_host* doesn't exist"
This error can appear when you use host that doesn't exist.
Example:

        Knoema("IMFWEO2018Apr", list(country = "914", subject = "LP"), host='knoema_incorect.com')

9.  Error: "The function does not support specifying mnemonics and selection in a single call"
This error appears when you use mnemonics and selection in one query.
Example:

        Knoema('IMFWEO2018Apr', selection = list(country ='912', subject='lp'), mnemonics = 'some_mnemonic')
        Knoema(selection = list(country = 'USA'), mnemonics = 'some_mnemonic')    
