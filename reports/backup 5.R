
### 05: Analysis
The analysis component is where the model developed in components 1-4 is applied to answer the question(s) of interest given current information. Our framework separates the analysis in three subcomponents, subcomponents **05a** being the deterministic analysis, **05b** the uncertainty analysis and **05c** the value of information analysis. For the Sick-Sicker example, all three subcomponents will be used to answer the CE research question and to quantify the uncertainty of our decision. For procedures in the cost-effectiveness analyses, we rely on the R package dampack, which is available here: 
  <https://github.com/DARTH-git/dampack>. Instructions for installing dampack are described in Appendix A. provided in the framework *app0_packages-setup.R script*.

####05a Deterministic analysis
In this subcomponent we start with performing the basic CEA, followed by evaluating some of the parameter uncertainy using deterministic sensitivity analysis. The functions file of this subcomponent, `05a_deterministic-analysis_function.R` file contains the functions `f.generate_basecase_params()` and `f.calculate_ce_out`. These `f.generate_basecase_params` function is used to generate a base-case set of paramters needed for the CEA analysis. This included using the function `f.define_init_params` to generate the initial parameter values, as described before, but now the placeholder values are replaced by the calibrated values. 

```{r, eval = TRUE, echo = TRUE}
print(f.generate_basecase_params) # print the function 
```

The results for the `f.generate_basecase_params` function are stored in the variable` v.params.basecase`, containing all the basecase values of the parameters. These values are used as input for the `f.calculate_ce_out` function. The users can also specify the willingness-to-pay (WTP) value using the `n.wtp` argument of the function. This WTP value is used to compute a net monetary benefit (NMB) value. If the users does not specify the WTP, a default value of 100000 will be used by the function. 
```{r, eval = TRUE, echo = TRUE}
print(f.calculate_ce_out) # print the function 
```

The first part of the function runs the decision model using the previously described function `f.decision_model`. Since we set the argument `v.params` equal to `v.params.basecase`, we are now using the basecase parameters rather than the initial values. Running this code gives us a list of output for both the treatment, `l.model.out.trt`, and no treatement, `l.model.out.no_trt` strategy of the Sick-Sicker model. In the second part of the function we create multiple vectors for both the cost and effects of both strategies. These vectors in turn are mulitplied using matrix multiplication, indicated by `%*%` in R, with the Markov trace. This results in vectors of total costs (`v.tc`) and total effects (`v.tu`) per cycle. By multiplying these vectors with the vectors with the discount weights for costs (`v.dwc`) and effects (`v.dwe`) we get the total discounted mean costs (`tc.d_no_trt` and `tc.d_trt`) and QALYs (`tu.d_no_trt` and `tu.d_trt`) for both strategies. These values are used in the calculation the NMB. The total costs, total effectiveness and the NMB are combined in a matrix, called `m.ce`. This matrix is the result of the function. This compact matrix with the CE results can be used as an argumnet in the `calculate_icers` function from the DAMPACK package. 


We now have the results of our CEA and we would like to evaluate the uncertainity around this decision using one-way sensitivity analysis (OWSA). 
XXXXX


####05b Uncertainty analysis 
In this subsection, we evaluate decision uncertainy by evaluating the uncertainty of our parameter values using probabilistic sensitiviy analysis (PSA). Until now we used the parameter values as described in Table {tab:parameters}. However, the truth is that we are uncertain about the described values. Most of these input parameters are defined by a distribtion as described in Table \ref{tab:parameters PSA}. 

Table: \label{tab:parameters PSA} Description of parameters with their R name and value.

|           **Parameter**         |  **R name** |   **Distribtion**   |
  |:--------------------------------|:-----------:|:-------------:|
  | Annual transition probabilities |           |               |
  | - Disease onset (H to S1)       | `p.HS1`     |  `beta(30, 170)`        |
  | - Recovery (S1 to H)            | `p.S1H`     |  `beta(60, 60)`         |
  | Annual costs                    |             |               |
  | - Healthy individuals           | `c.H`       |  `gamma(shape = 100, scale = 20)`        |
  | - Sick individuals in S1        | `c.S1`      |  `gamma(shape = 177.8, scale = 22.5)`       |
  | - Sick individuals in S2        | `c.S2`      |  `gamma(shape = 225, scale = 66.7)`     |
  | - Additional costs of sick individuals treated in S1 or S2           | `c.Trt`     |  `gamma(shape = 73.5, scale = 163.3)`      |
  | Utility weights                 |             |               |
  | - Healthy individuals           | `u.H`       |`truncnorm(mean = 1, sd = 0.01, b = 1)`         |
  | - Sick individuals in S1        | `u.S1`      |`truncnorm(mean = 0.75, sd = 0.02, b = 1)`         |
  | - Sick individuals in S2        | `u.S2`      |`truncnorm(mean = 0.50, sd = 0.03, b = 1)`         |
  | Intervention effect             |             |               |
  | - Utility for treated individuals in S1 | `u.Trt` |`truncnorm(mean = 0.95, sd = 0.02, b = 1)`        |
  
  In a PSA we sample the input parameter values from these distributions and we than run the model for each sample. This means that we need to make multiple PSA data sets to run the model with. In the file *05b_uncertainty-analysis_functions.R* we created a single function, called `f.generate_psa_params`, that is able top generate a PSA dataset of CEA input parameters for each number of simulations we like to have in our PSA. We specify the number of PSA simulation via the `n.sim` argument. The function is also using a seed to allow reproducing the results of this stocastic proces. 

```{r, eval = TRUE, echo = TRUE}
print.function(f.generate_psa_params) # print the function 
```

The function returns a dataframe of PSA output, that can then be used to run the model for each sample. 












####05c Value of information

### References 









