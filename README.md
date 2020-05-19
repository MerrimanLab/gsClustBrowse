# gsClustBrowse
Shiny app for browsing the exported intensities from Genome Studio

Contains an R Markdown for the setup of the SQLite database that is the data store for the app to query. 

There is also a R script in scripts that demonstrates how to access and query the database from within R


## Installing pre-requisites

### Mac OS

If you don't have homebrew install it by following the instructions at [https://brew.sh](https://brew.sh), it will let you follow the next steps.

#### Database Drivers

You need to install the unixodbc drivers for PostgreSQL. This can be done through homebrew `brew install psqlodbc`.

Then you need to fill in the driver details in this file `/usr/local/etc/odbcinst.ini` as such:
```
[PostgreSQL UNICODE]
Description=PostgreSQL ODBC driver (Unicode version)
Driver=/usr/local/Cellar/psqlodbc/12.01.0000/lib/psqlodbcw.so
Debug=0
CommLog=1
UsageCount=1
```

N.B. the version number might differ so check the file exists first using `ls /usr/local/Cellar/psqlodbc/12.01.0000/lib/psqlodbcw.so`


Reference websites:

https://www.labkey.org/Documentation/wiki-page.view?name=odbcmac

https://db.rstudio.com/getting-started/connect-to-database/

### R libraries

```
install.packages(shiny)
install.packages(DT)
install.packages(tidyverse)
install.packages(dbplyr)
install.packages(DBI)
install.packages(odbc)
install.packages(config)
```

### Create a config file

In the project root you will need a file called `config.yml` which is used to supply the database connection information.

It look like the following:
```
default:
  psql_ro:
    driver: 'PostgreSQL UNICODE'
    database: '<database_name>'
    server: '<server_address>'
    port: <port_number>
    user: '<db_username>'
    password: '<db_password>'
```

**It is important this file doesn't get added to the repo**


Ask Murray for the details