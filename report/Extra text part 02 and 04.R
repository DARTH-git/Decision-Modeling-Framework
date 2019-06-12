02 Decision model implementation
The Sick-Sicker model consists of four health states: healthy (H), two disease states sick (S1) and sicker (S2) and dead (D) (Figure 1). All individuals start in the healthy state. Over time, healthy individuals may develop the disease and can progress to S1. The disease has two levels; affected individuals initially become sick but can subsequently progress and become sicker, progress to S2. While Sick individuals can recover from the disease (return to state H), individuals in S2 cannot. From all states individuals can die. Healthy individuals have an age-, sex- and race- (ASR) specific mortality rate and individuals in S1 and S2 have an increased mortality rate compared to healthy individuals. The mortality rates of healthy individuals and the calibrated hazard rate ratios are used to  calculate the probabilities to when in S1 and S2. 

Two alternative strategies exist for this hypothetical disease: a no-treatment and a treatment strategy. Under the treatment strategy, individuals who become sick or progress and become sicker receive treatment and continue doing so until they recover or die. This treatment is not influencing their transition probabilities. Therefore, the 02_simulation-model_function.R file initiates only one age-specific transition probability matrices used for both the treatment and the no treatment strategy. The file 02_simulation-model.R runs this function computing the outcome 

this model structure function with the input data resulting in
the Markov trace of the Sick-Sicker model.


04 Calculate model outputs [or run model]
The generated inputs, decision model implementation data and calibrated parameters from the Sick-Sicker model are combined in this part. The 04_functions file creates a base-case parameter set used to inform the transition probability matrix for the CEA analysis from the calibrated parameters. The f.calculate_ce_out is the core part of the model and generates the CE outcome. 

Each health state in the model is associated with a specific utility and a cost. The cost of the treatment is additional to the cost of being sick or sicker for one year. The treatment improves quality of life for those individuals who are sick but has no effect on the quality of life of those who are sicker. 


The interventions differ from each other in costs and rewards. The utility of the health states decreased with severity of the state. 
The interventions differ from each other in cose. Treatment increases cost and is reducing qualo
Unless the transition probabilities, the cost and effectiveness are 

increased treatment costs and reduced quality of life.

Running the model to generate output is a very important part of the modeling process. This part of the process however, is code wise very simple. This file is informed by the generated inputs and decision model implementation. The function used is the function for an iterative loop for time, the total number of cycles the model needs to run, multiplies the transition probability matrix for the current cycle with the allocation of the cohort at that cycle. This multiplication calculated how the cohort is allocated among the health state at the next cycle. So this iterative process fills the Markov trace. Since the transition probabilities differ between the treatment strategy and the no treatment strategy within the iterative process this calculation is done for both the strategies.  



###04 Calculate model outcome
In this section we evaluate the model outcomes by combining the previous three sections. Each health state in the model is associated with a specific utility and a cost (Table \ref{tab:parameters}). We are asked to perform a cost-effectiveness analysis of a hypothetical treatment. This treatment is given to all patients in both Sick and Sicker and is associated with specific costs, see Table \ref{tab:parameters}. The cost of the treatment is additional to the cost of being sick or sicker for one year. The treatment improves QoL for those individuals who are sick but has no effect on the quality of life of those who are sicker [@Krijkamp2018]. 

Via the `04_calculate-outcomes_functions.R` basecase values of the calibrated parameters are replaced with calibrated values from the `03_nm-best-set.RData` 
data. This new set of parameters is used to run the model to produce a cohort trace for the CEA. The `04_calculate-outcomes_functions.R` files also generates the function `f.calculate_ce_out`, which is the core part of the model for the generation of cost-effectiveness results. Running this gives us the following cost and effectiveness outcomes. 



####05a Deterministic analysis of Decision analysis / cost-effectiveness analysis 
The deterministic sensitivity analysis of the Sick-Sicker model consists of a one-way sensitivity analysis (owsa) looking at the effect of the utility value used for Sicker (S2) on the QALY, the outcome of interest. In the `05a_deterministic-analysis_function.R` file we create the functions for a one-way sensitivity analysis `owsa.plot.det`, a two-way sensitivity analysis `twsa.plot.det` and the code to create tornado plots `TornadoPlot`. 


We are interested in the effect of the parameters `p.HS1`, `u.S2` and `c.Trt` on the QALYs. The results are summarixed in Figure \ref{fig:Tornado}.
![Tornado Plot \label{fig:Tornado}](../figs/Markov-SickSicker-Tornado.png)


