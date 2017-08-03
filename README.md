Official R package for Knoema's API
========

This is the official documentation for Knoema's R Package. The package can be used for obtaining data from the datasets from the site knoema.com.

# Installation

To install the [devtools](https://cran.r-project.org/package=devtools) package:

    install.packages("devtools")
    library("devtools")
    install_github("Knoema/knoema-r-driver")

# Authentication
By default, the package allows you to work only with public datasets from the site knoema.com and has a limit on the number of requests.
To make full use of the package we recommend you set parameters app_id and app_secret. You can get these parameters after registering on the site knoema.com, in the section "My profile - Apps - create new" (or use existing applications).
How to set these parameters will be shown below.

# Retrieving series from datasets
There is one method for retrieving series from datasets in R: the knoema.get method. The method works with knoema datasets.

The following quick call can be used to retrieve a timeserie from dataset::

   library('knoema')
   data = knoema.get('IMFWEO2017Apr', list(country='914', subject='ngdp'))
   
where:

* 'IMFWEO2017Apr' this is a public dataset, that available for all users by reference https://knoema.com/IMFWEO2017Apr.
* country and subject are dimensions names
* '914' is code of country *Albania*
* 'ngdp' is code of subject *Gross domestic product, current prices (U.S. dollars)*

This example finds all data points for the dataset IMFWEO2017Apr with selection by country = *Albania* and subject =  *Gross domestic product, current prices (U.S. dollars)* and stores this series in a format ts. 

Please note that you need to identify all dimensions of the dataset, and for each dimension to indicate the selection. Otherwise, the method returns an error.

For multiple selection you can use the next example::
  
    data = knoema.get('IMFWEO2017Apr', list("country" = '914;512;111', "subject" = 'lp;ngdp'))
    
For case when the dimensions of dataset that have multi word names use the next example::

    data = knoema.get('FDI_FLOW_CTRY', list('Reporting country'='AUS',
                                                    'Partner country/territory'= 'w0',
                                                    'Measurement principle'= 'DI',
                                                    'Type of FDI'= 'T_FA_F',
                                                    'Type of entity'= 'ALL',
                                                    'Accounting entry'= 'NET',
                                                    'Level of counterpart'= 'IMC',
                                                    'Currency'= 'USD'))   

In addition to the required using of the selections for dimensions, you can additionally specify the period and frequencies in the parameters. For more details, see the example below::

    data = knoema.get("IMFWEO2017Apr",list (country='914;512;111', subject='lp;ngdp', frequency='A', timerange='2007-2017'))
    
The package supports such formats as "ts", "xts" and "zoo". By default type is equal "ts". How to use the type shown in the example below:

    data = knoema.get("IMFWEO2017Apr",list (country='914;512;111', subject='lp;ngdp'), type = "zoo") 
    
To take into account the parameters app_id and app_secret in a function, you must explicitly specify them:

    data = knoema.get('MEI_BTS_COS_2015', list(location='AT;AU', subject='BSCI', measure='blsa', frequency='Q;M'), type="xts", app_id='bHcV5UkOVyKcBw',app_secret="/0itYgLqnD0i49kmdBVSZ1qLjPU")

Note: If the function returns an error 403(Forbidden), try using other parameters app_id and app_secret.

# Convrting timeseries in different formats

  Function knoema.get always return list of timeseries. If you want to convert series in format ts,xts, zoo, you should first get the separate serie:
  
   data = knoema.get('IMFWEO2017Apr', list(country='albania;afghanistan;united states', subject='Gross domestic product, current prices (National currency);population (persons)'), "ts") 
   sname = 'A - United States - Gross domestic product, current prices (National currency)'
   time_ser = data[[sname]]
   zoo = as.zoo(time_ser)

