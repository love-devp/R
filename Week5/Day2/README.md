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
	- *CLOSE* : Zach had already dealed with this. 
4. Pathway analysis → ROGUE
	- *Successfully dockerized*
5. Mapping to LINCs → sig2lead_v2.0
	- I found the docker image from docker hub, it perfectly works. But there is no Dockerfile that I can use as a resource.
	- I have been dealing with system dependencies and no packages errors. 
	- Making a new dockerfile currently	(on-going)
6. Feature selection → Shiny Cell
	- Making Dockerfile (on-going)