# Progress of each shiny app
1. Disease enrichment analysis → clusterprofiler-ui-shiny
	- *Successfully dockerized*
	- Error that I faced and solution
		1. Error:
			Error in library(clusterProfiler) :
			there is no package called ‘clusterProfiler’
			Execution halted
		2. Solution:
			Update and Install "libglpk.so.40" into Ubuntu
			Add following code lines into Dockerfile
				```
				RUN apt-get update && apt-get install -y \
				libglpk40 \
				&& rm -rf /var/lib/apt/lists/*
				```
2. Disease enrichment analysis → Nasqar2
	- Successfully dockerized, can't execute because of the following error: 
		CondaError: Run 'conda init' before 'conda activate'
		: ambiguous redirectiny-server.sh: line 4: 1
3. Cell communication → InterCellar
	- Successfully dockerized, can't execute because of the following error: 
		Error in loadNamespace(x) : there is no package called ‘InterCellar’ 
4. Pathway analysis → ROGUE
	- *Successfully dockerized*
5. Mapping to LINCs → sig2lead_v2.0
	- Successfully dockerized, can't execute because of the following error:
		Warning: Error in library: there is no package called ‘ChemmineOB’
		52: <Anonymous>
		Error in library(ChemmineOB) : there is no package called ‘ChemmineOB’
		