# Progress of each shiny app
1. Disease enrichment analysis → clusterprofiler-ui-shiny
	- *Successfully dockerized*

2. Disease enrichment analysis → Nasqar2
	- Successfully dockerized, can't execute because of the following error: 
		CondaError: Run 'conda init' before 'conda activate'
		: ambiguous redirectiny-server.sh: line 4: 1
		
3. Cell communication → InterCellar
	- *CLOSE* : Zach had already dealed with this. 
	
4. Pathway analysis → ROGUE
	- *Successfully dockerized*
	
5. Mapping to LINCs → sig2lead_LINCs
	- *Successfully dockerized*
	- Upgrading to sig2lead_v2.0...
	
6. Feature selection → Holomics
	- Successfully dockerized, can't execute because of the following error: 
		> shiny::runApp('/srv/shiny-server', port=8789, host='0.0.0.0')
		Loading required package: shiny
		Warning in warn_if_app_dir_is_package(appDir) :
		  Loading R/ subdirectory for Shiny application, but this directory appears to contain an R package. Sourcing files in R/ may cause unexpected behavior. See `?loadSupport` for more details.
		Error in dyn.load(file, DLLpath = DLLpath, ...) :
		  unable to load shared object '/usr/local/lib/R/site-library/cli/libs/cli.so':
		  /usr/lib/x86_64-linux-gnu/libc.so.6: version `GLIBC_2.32' not found (required by /usr/local/lib/R/site-library/cli/libs/cli.so)
		Calls: <Anonymous> ... <Anonymous> -> loadNamespace -> library.dynam -> dyn.load
		Execution halted