# Progress of each shiny app
1. Disease enrichment analysis → [clusterprofiler-ui-shiny](https://github.com/love-devp/R/tree/Internship/Week5/Day1/clusterprofiler-ui-shiny_Disease%20enrichment%20analysis)
	- Successfully dockerized, can't execute because of the following error:
		Error in library(clusterProfiler) :
		there is no package called ‘clusterProfiler’
		Calls: <Anonymous> ... sourceUTF8 -> eval -> eval -> ..stacktraceon.. -> library
Execution halted
2. Disease enrichment analysis → Nasqar2
	- Successfully dockerized, can't execute because of the following error: 
		CondaError: Run 'conda init' before 'conda activate'
		: ambiguous redirectiny-server.sh: line 4: 1
3. Cell communication → InterCellar
	- Successfully dockerized, can't execute because of the following error: 
		Error in loadNamespace(x) : there is no package called ‘InterCellar’ 
4. Pathway analysis → ROGUE
	- Making Dockerfile (on-going)
5. Mapping to LINCs → sig2lead_v2.0
	- Making Dockerfile (on-going)