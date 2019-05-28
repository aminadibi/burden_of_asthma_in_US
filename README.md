# Web app for Burden of Asthma in the US
This app provides an interactive interface to explore the predicted burden of asthma in the United States, both in terms of the cost and the effect on quality of life.
Please refer to the manuscript for more details on the structure of the model and the validation process.

## Background

### QALY
Quality-Adjusted Life Year - a unit of measuring quality of life in one year based on health.

- 1 QALY = 1 year in perfect health
- 0.5 QALY = 0.5 years in perfect health, or 1 year in 50% of perfect health

We calculated average QALY based on 5 studies as follows:

- Generic questionnaire assessing health-related quality of life
- N = 10589 total patients
- 5650 controlled asthma
- 4939 uncontrolled asthma

**per capita:** Both the cost per capita and QALYs lost per capita are generated using population predictions for 2019 - 2038.

### Direct Cost 

Direct medical costs due to uncontrolled asthma were estimated by the following parameters:

- Annual per-person excess medical costs of asthma (2015 USD) estimated in the paper by [Nurmagambetov et al](https://www.atsjournals.org/doi/pdf/10.1513/AnnalsATS.201703-259OC) to be a total of $3266:
  - $1830 for prescription medication
  - $640 for office visits
  - $529 for hospitalizations
  - $176 for hospital-based outpatient visits
  - $105 for emergency room visits
- Estimated annual prevalence of controlled and uncontrolled asthma
- Pooled odds ratio of healthcare utilization associated with uncontrolled vs controlled asthma

### Indirect Cost

To estimate the indirect costs of uncontrolled asthma, we used the following parameters:

- Comparison of percentage of time present/absent at work in controlled vs uncontrolled asthma (pooled estimate of standardized mean difference)
  - 3 studies, N = 9628 total patients
  - 5011 controlled asthma
  - 4617 uncontrolled asthma
  - overall estimate 12.70% work impairment
  - assuming 52 work weeks, an average loss in productivity of 6.6 weeks per year in uncontrolled asthma
- Average wages by age and sex as reported by the US Bureau of Labor and Statistics (2012)

## Viewing the App

The app can be viewed at either: 

https://ahill187.shinyapps.io/BurdenOfAsthma/

http://resp.core.ubc.ca/ipress/burdenofasthmainus

## Paper

This application is based off the paper [The projected economic and health burden of uncontrolled asthma in the United
States](https://www.biorxiv.org/content/biorxiv/early/2019/01/12/516740.full.pdf), written by Mohsen Yaghoubi, Amin Adibi, Abdollah Safari, J Mark FitzGerald, and Mohsen Sadatsafavi, for the Canadian Respiratory Research Network. Supplementary materials can be found here: https://www.biorxiv.org/content/biorxiv/early/2019/01/12/516740/DC1/embed/media-1.pdf?download=true.

