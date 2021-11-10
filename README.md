# Chemistry-Derivatives
Wasting my weekends in R to procrastiate English essays. 
Using linear steps to do my chem work for me. 
All in R.

Instructions for finding the order using integrated rate laws:

Download R from https://cran.r-project.org/ <br>
Install the appropriate packages using this command (unless you already have them): <br>
`install.packages(c("tidyverse","here","ggpubr","ggmpisc","cowplot","googlesheets4"))` <br>

Navigate to this google sheet https://docs.google.com/spreadsheets/d/1a8l07lQ10MwJUHiL4wxex1g1ewfrx2GVZhUAc7FDFwg/edit?usp=sharing <br>
Add your data in here!

Clone the repo (or just download it since you won't be changing it) <br>
Type this into the command line: <br>
`cd [file_path_to_repo]` and replace [file_path_to_repo] with the path to where you cloned the repo <br>
Then, type this to run the script <br>
`Rscript google_sheet_orders.R` 

You should see the order printed in the console!
