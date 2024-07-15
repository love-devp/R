> install.packages("shiny")
WARNING: Rtools is required to build R packages but is not currently installed. Please download and install the appropriate version of Rtools before proceeding:

https://cran.rstudio.com/bin/windows/Rtools/
Installing package into ‘C:/Users/loved/AppData/Local/R/win-library/4.4’
(as ‘lib’ is unspecified)
trying URL 'https://cran.rstudio.com/bin/windows/contrib/4.4/shiny_1.8.1.1.zip'
Content type 'application/zip' length 4828781 bytes (4.6 MB)
downloaded 4.6 MB

package ‘shiny’ successfully unpacked and MD5 sums checked

The downloaded binary packages are in
	C:\Users\loved\AppData\Local\Temp\Rtmpu2mxRI\downloaded_packages
> library(shiny)
> ui <- fluidPage(
+     "Hello, world!"
+ )
> server <- function(input, output, session){
+ }
> shinyApp(ui, server)

Listening on http://127.0.0.1:3669