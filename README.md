Contains the R Markdown code for the Gender Equality Index 2020 publication at http://data.gov.scot/genderindex

Publication folder contains the files used for creating the publication:

- gender-equality-index.html (main gender index publication)
- *.Rmd (other Rmd files containing the various chapters)
- _site.yml (file that organised this and other html files on http://data.gov.scot/genderindex)
- pub_data.rData (datasets used within the publication)
- scripts used to produce charts etc.
- css style sheet
- javascript code for using bootstrap tooltips (bootstrap javascript code needed copying in here, so that it is loaded after the other libraries as there was a conflict with another package used)

Main folder contains:
- R scripts for analysing Gender Index data
- Gender Index 2020.xlsx (specifies structure of index, and variables included in each sub-domain)
- Gender Index Variables.xlsx (metadata for all Gender Index variables)

Notes:

R code for analysing the gender index indicators is included, but the raw data isn't. 

An Excel version of the gender index can be found on the publication website.

When building the website, the index.html (knitted from .Rmd) needed to be replace with the index.html file found within the publication folder.

**License**

You may use and re-use this software and associated documentation files free of charge in any format or medium, under the terms of the Open Government Licence v3.0.

You may obtain a copy of the Open Government Licence at http://www.nationalarchives.gov.uk/doc/open-government-licence/