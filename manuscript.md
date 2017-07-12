<!-- to render this file into a pdf: Rscript -e "rmarkdown::render('manuscript.Rmd')" -->
# Introduction

*to be done: step 4*

One of the issues in agroecology is to design innovative crop systems and identify arrangements of natural habitats in the landscape that allow both to control the abundances of weeds and to preserve their role in the ecosystem services.
It is also important to understand ecological process of landscape effects on population dynamics.

Discuss the overall theoretical knowledge of plant dispersal with an emphasis in short-lived plants and weed species.

Try and show the well-known role that dispersal plays in plant dynamics and what could be true/false for weed communities in a human environment.

In the above context, DynWeed was created to represent the dynamics of weed in a landscape scale, which makes necessary take dispersal into account. However, Modeling the dispersion is difficult because of the high variability of dispersion distance and the lack of empirical data. This is the context of the “technical question”. Discuss the technical part of dispersal (e.g. curves) and how it has been modeled.
*Relatively small differences in the shapes of tails can have large effects on rates of population spread (Clark 1998)*.

Here we ask the following questions: (i) what is the role of dispersal on weed dynamics? (ii) Does dispersal curve really matters at landscape-action? (iii) Could spatial arrangement of management decisions at landscape scale decrease weed spread and persistence in agro-ecosystems? If so, (iv) how can proportion of permanent meadows impact weed spread? Using a landscape-scale model, our hypothesis is that different curves to represent the dispersal process will change dispersal distance and hence the weed dynamics.
Furthermore, increasing the proportion permanent meadows in the agricultural landscape will reduce the speed of weed spread through the landscape.
These will provide mechanist insights into local weed management and into landscape spatial arrangement of management decisions, with suggestions to empirical research and decision makers.

# Methods

## Population dynamics model

We used a spatially explicit model of population dynamics for four different weed species in agricultural landscape [@Ricci2017].
Based in the population dynamics of weed species, the model has five parameters representing the natural process: the seed germination *(gr)* and mortality (*sm*) rate, the quantity of seed produced per plant (*sp*), the plant mortality rate (*pm*) and the environmental carrying capacity (*K*).
Let *P* be the density of plants at time *t* (@eq:p), this variable increases with viable seeds germinating at rate *(1 - sm)gr*, and with the number of survival plants *(1 - pm)* times the seed production (*S*).
The density of viable seeds (*S*; @eq:s) is a function of either germinated seeds *(1 - sm)* and the seed bank *(1 - gr)*, plus the proportion of dispersed seeds from other patches, which is calculated by different dispersal curves *D* (\*@tbl:eq).

$$ P_{t,x,y} = min\{K,(1 - sm_{t,x,y})gr\times(1 - pm_{t,x,y})S_{t-1,x,y}\} $$ {#eq:p}

$$ S_{t,x,y} = (1 - sm_{t,x,y})\times(1 - gr_{t,x,y})S_{t-1,x,y} + \iint\limits_{(x',y')} (P_{t,x',y'}sp_{t,x',y'})D(x - x', y - y') $$ {#eq:s}

## Dispersal curves and parameterization

We tested four kernel curves to represent the process *D* (@eq:s) of weed dispersion (\*@tbl:eq).
The Gaussian and exponential [@Austerlitz2001] distribution are frequently used to represent spatial dynamics and hence biological dispersal. However, due **DO**, these curves may not represent dispersal properly [@Clark1998].
As our model works in a landscape scale, long distance dispersal events are important to consider.
Weibull [@Tufto1997;@Weibull1951] et 2Dt [@Clark1999] curves were then implemented to try and represent the long distance dispersal by the leptokurtosis and long tail form.

Table: Kernel curves to calculate the probability of seed dispersal, the parameter assumptions and the distribution form. {#tbl:eq}

| Curve       |  Equation | Parameters | Distribution |
|:------------|----------:|:----------:|:------------:|
| Gausian     | $$ f(x) = \frac{1}{\theta\sqrt{2\pi}} e^{\frac{-x^2}{\theta y^2}} $$ | $\theta$ > 0 | ![](figure/gaussian.pdf){width=65%} |
| Weibull  | $$ f(x) = \frac{\epsilon - 1}{\epsilon\phi^\epsilon} \epsilon x^{\epsilon - 1} e^{\frac{\epsilon - 1}{\epsilon\phi^\epsilon}x^\epsilon} $$ | $\epsilon$ > 1   $\phi$ > 0 | ![](figure/weibull.pdf){width=65%} |
| Exponential | $$ f(x) = \frac{1}{2\pi\lambda^2} e^{\frac{-x}{y}} $$ | $\lambda$ > 0 | ![](figure/exponential.pdf){width=65%} |
| 2Dt | $$ f(x) = \frac{\beta - 1}{\alpha^2 \pi} 1 + (\frac{x^2}{y^2})^{-\beta} $$ | $\alpha$ > 0   $\beta$ > 1 | ![](figure/2dt.pdf){width=65%} |

Show here how I chose each value of parameter and where this values come from (curve fitted based in @Thomson2011)

## Dispersal calculation

The seed spread calculation was based...

## Simulation plan

Explain the variation of each simulation.

## Statistics analysis

Test [@Gaba2010]

# Results

*to be done: step 2*

The speed of seed spread over time shows a median of 262.2 meters.year$^{-1}$ (from 62.7 to 402.9) for all simulations. This speed is mostly influenced by land use (ω$^2_{mean}$ = 15.4%) and by the interaction of curve and proportion of land use (ω2mean = 10%; table 1). Both the proportion of land use and the curve alone has a moderate effect on the speed of spread (5.8% and 5.3%, respectively).
Among the landscape with a unique land use, the permanent meadows presented the slowest median dissemination speed (80 metersyear-1) compared with conventional (266.6) and organic (279). Increasing the proportion of permanent meadows to 50% in the landscape significantly decreased the dissemination speed, but there was no difference when increased to 3% (Figure 1).
The significantly but weak effect of the dispersion curve on dissemination speed (ω2mean = 5.3%), shows a general higher median dissemination speed for the 2Dt curve (282.3 metersyear-1) compared with Exponential (268.8), Gaussian (265.3) and Weibull (265.3; Figure 1).

# Discussion

*to be done: step 3*

# References
