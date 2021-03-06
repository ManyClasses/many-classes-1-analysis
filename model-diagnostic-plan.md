# Plan for ManyClasses model diagnostics

Bayesian models must be assessed to determine whether the posterior sample generated by the MCMC method is likely to be representative of the true posterior distribution of the model. Here we describe our plan to assess convergence and effective sample size, as well as possible remedies that we will employ if the models fail to converge or require too much computation to reach reasonable effective sample sizes when run with real data.

We are using JAGS 4.3.0 and the `runjags` R package for MCMC sampling, and our plan describes strategies as implemented in this software context.

## Convergence

We will run each model with multiple chains and assess convergence via the *potential scale reduction factor*, also known as the rhat statistic (Gelman and Rubin, 1992). This measure compares the variance between independent chains to the variance within chains. If the chains are all sampling the same region of the posterior distribution, the value should be very close to 1.0. We will monitor the *psrf* of all parameters in the model. If the *psrf* is above 1.1 for any parameter, we will take corrective steps (see below). 

We will also assess convergence via visualization of the chains. While *psrf* should catch any convergence issues, visualization is a simple way to check for any obvious problems. 

## Effective sample size

MCMC chains can suffer from autocorrelation. High autocorrelation means that the chains are not providing independent information about the posterior distribution at every sample. Effective sample size (ESS) is an estimate of the number of independent samples in the chains. We will monitor the *ESS* of all parameters in the model using the output from `runjags`. If the *ESS* is below 10,000 for any parameter, we will take corrective steps (see below).

## Corrective Steps

These are potential corrective steps we can take to fix issues with convergence and effective sample size.

Steps that present minimal risk of bias from increased researcher degrees of freedom:

1. Increase the sample size of the chains. More samples produces more accurate results at the cost of computational time.
2. Increase the burn-in. If the chains are not converging early in the sample we can increase burn-in. The only cost here is increased computational time.
3. Increase the number of chains, potentially massively. If the model has very high autocorrelation we can increase the number of chains and spread the computational load across multiple processors.
4. Introduce thinning to save disk space. If we need large numbers of chains we may introduce thinning into the model. While thinning removes some information relative to not thinning, it does improve the ESS per saved sample. If a large number of samples are needed to achieve the desired *ESS*, thinning will reduce the memory and disk storage demands of the models.

Steps that have some risk of bias from increased researcher degrees of freedom:

1. Adjusting priors. We have made an effort to specify mildly-informative priors that reflect only the expected scale of the data. If the priors are too vague or too specific, convergence problems can occur. If methods in the above section fail, we may tweak the priors. If we do tweak priors, we will document these changes and our justification, and include this document in the OSF repository.
2. Truncating estimated parameter distributions. Hierarchical models can suffer from implosive shrinkage, particularly with small samples (Kruschke, 2014). If this is the case, one possible solution (instead of, or in addition to tweaking the priors) is to restrict a parameter distribution's lower boundary to a value slightly greater than zero, using the "T(L,U)" syntax in JAGS.  If implemented, we will document these changes as above.
3. Reparameterizing the model. It's possible that alternative parameterizations of the model that preserve the same general inferential structure will be more efficient. This will involve changes to the priors as different parameters are introduced, and so we consider this a more drastic step than the above.

