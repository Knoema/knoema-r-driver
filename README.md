Official R package for Knoema's API
========

This is the official documentation for Knoema's R Package. The package can be used for obtaining data from the datasets from the site knoema.com.

# Installation

To install the [devtools](https://cran.r-project.org/package=devtools) package:

    install.packages("devtools")
    library("devtools")
    install_github("ketiketi2507/knoematest") //NEED TO CHANGE WHEN WILL BE NOT TEST VERSION
    library("knoematest")
                
# Authentication
By default the package allows you to work only with public datasets from the site knoema.com.
If you want work with private datasets, you need to use the parameters app_id and app_secret.
You can get parameters app_id and app_secret after registering on the site knoema.com, in the section "My profile - Apps - create new" (or use existing applications).
create client like this: 
```r
client = ApiClient("knoema.com","some_app_id","some_app_secret")
```
app_id and app_secret should be set up together.
Also, you can use other hosts supported by knoema, just change the first parameter in the function.

# Retrieving series from datasets
There are one method for retrieving series from datasets in R: the knoema.get method. The method works with knoema datasets.

The following quick call can be used to retrieve a timeserie from dataset::

   library('knoematest')
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

In addition to the required using of the selections for dimensions, you can additionally specify the period and frequencies in the parameters. For more details, see the example below::

    data = knoema.get("IMFWEO2017Apr",list (country='914;512;111', subject='lp;ngdp', frequency='A', timerange='2007-2017'))
    
The package supports such formats as "ts", "xts" and "zoo". By default type is equal "ts". How to use the type shown in the example below:

    data = knoema.get("IMFWEO2017Apr",list (country='914;512;111', subject='lp;ngdp'), type = "zoo", client)   

