# Analytical pipeline for Omics analysis in REACT Long COVID biological cohort

## Primary outcomes for analysis

We are interested in signals both of current long COVID status and of 'ever' Long COVID. The table below shows the key variables that are available for these analyses.

| Outcome                                                       | Variable name | Data set                              | N                      |
|---------------------------------------------------------------|---------------|---------------------------------------|------------------------|
| Experienced one or more symptoms for 12 weeks or more         | `lc_binary`     | registration (registration_data)      | All registrants (13,454) |
| Not currently recovered from COVID-19 (baseline)              | `covrecov`      | health survey (health_survey$survey_t0) | 10,066                 |
| Not currently recovered from COVID-19 (follow-up)             | `covrecov`      | health survey (health_survey$survey_t1) | 2,075                  |
| Symptom-specific Long COVID status (baseline + folow-up)      | `sympttime_`    | health survey t0 and t1                 | 10,066 +2,075          |




* `lc_binary` gives us 'ever' Long COVID, an outcome comparable to the one used in _Persistent COVID-19 symptoms in a community study of 606,434 people in England_. Nat Commun 13, 1957 (2022). https://doi.org/10.1038/s41467-022-29521-z. It is derived from the symptom-specific data from the registration survey.
* `covrecov` gives us the response to the question 'Do you feel fully recovered from COVID-19?', asked in the health surveys completed around the time of participants' clinic visits. For a subset of 2,075 participants, two clinic visits were recorded, along with two health surveys
  -   from  combinations of these two variables (from three timepoints) we can infer several secondary outcomes, including:
    -   'Experienced Long COVID after infection but recovered by baseline' - lc_binary==1 & covrecov==1
    -   'Still experiencing Long COVID at baseline but recovered by follow-up' - lc_binary==1 & covrecov(t0)==0 & covrecov(t1)==1
* `sympttime_1` to 'sympttime_15' gives ratings of severity of these symptoms:
  -   a)	Breathlessness
  -   b)	Cough
  -   c)	Fatigue
  -   d)	Sleep quality
  -   e)	Pain
  
  Across three timepoints:

  -   Before you had COVID-19 [0-10]
  -   Since you had COVID-19 [0-10]
  -   Worst in the last 24 hours [0-10]
 
  We can engineer new continuous outcome variables for each of the 5 symptoms by taking the difference between pre-COVID-19 severity and post-COVID-19 severity.


