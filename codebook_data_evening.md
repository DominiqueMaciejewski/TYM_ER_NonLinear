Codebook created on 2026-05-04 at 2026-05-04 23:06:52.203271
================

A codebook contains documentation and metadata describing the contents,
structure, and layout of a data file.

## Dataset description

The data contains 4708 cases and 35 variables.

## Codebook

    ## Warning: 'xfun::attr()' is deprecated.
    ## Use 'xfun::attr2()' instead.
    ## See help("Deprecated")

    ## Warning: 'xfun::attr()' is deprecated.
    ## Use 'xfun::attr2()' instead.
    ## See help("Deprecated")

| name | type | n | missing | unique | mean | median | mode | mode_value | sd | v | min | max | range | skew | skew_2se | kurt | kurt_2se |
|:---|:---|---:|---:|---:|---:|---:|---:|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| participant.ID | integer | 4708 | 0.00 | 83 | 58172.39 | 61567 | 61567 |  | 28958.21 |  | 10076 | 99378 | 89302 | -0.18 | -2.55 | -1.40 | -9.78 |
| Dropped.out | factor | 4708 | 0.00 | 3 |  |  | 4595 | 0 |  | 0.05 |  |  |  |  |  |  |  |
| Toest_aspect1 | factor | 4708 | 0.00 | 3 |  |  | 2711 | Yes |  | 0.49 |  |  |  |  |  |  |  |
| Toest_aspect2 | factor | 4708 | 0.00 | 3 |  |  | 2772 | Yes |  | 0.48 |  |  |  |  |  |  |  |
| Toest_aspect3 | factor | 4708 | 0.00 | 3 |  |  | 2591 | Yes |  | 0.49 |  |  |  |  |  |  |  |
| max.day | integer | 4708 | 0.00 | 10 | 59.73 | 61 | 61 |  | 5.95 |  | 5 | 61 | 56 | -6.13 | -85.85 | 38.66 | 270.88 |
| max.week | integer | 4708 | 0.00 | 7 | 8.85 | 9 | 9 |  | 0.85 |  | 1 | 9 | 8 | -6.22 | -87.13 | 39.94 | 279.86 |
| duration_sec_start_stop | integer | 3143 | 0.33 | 666 | 455.58 | 144 | 144 |  | 3104.25 |  | 31 | 73833 | 73802 | 17.54 | 200.86 | 336.01 | 1924.15 |
| sent.beeps.evening | integer | 4708 | 0.00 | 11 | 59.65 | 61 | 61 |  | 6.11 |  | 1 | 61 | 60 | -6.10 | -85.46 | 38.65 | 270.78 |
| Appmal_noeven | factor | 4708 | 0.00 | 3 |  |  | 4651 | no |  | 0.02 |  |  |  |  |  |  |  |
| Appmal_negti | factor | 3143 | 0.33 | 2 |  |  | 3143 | no issue |  | 0.00 |  |  |  |  |  |  |  |
| Appmal_longexp | factor | 3143 | 0.33 | 3 |  |  | 3114 | no |  | 0.02 |  |  |  |  |  |  |  |
| filledin | integer | 4708 | 0.00 | 2 | 0.67 | 1 | 1 |  | 0.47 |  | 0 | 1 | 1 | -0.71 | -9.97 | -1.49 | -10.47 |
| day | integer | 4708 | 0.00 | 61 | 30.38 | 30 | 30 |  | 17.58 |  | 1 | 61 | 60 | 0.04 | 0.51 | -1.20 | -8.44 |
| week | integer | 4708 | 0.00 | 9 | 4.78 | 5 | 5 |  | 2.51 |  | 1 | 9 | 8 | 0.06 | 0.89 | -1.20 | -8.41 |
| beep.evening | integer | 4708 | 0.00 | 1 | 1.00 | 1 | 1 |  | 0.00 |  | 1 | 1 | 0 |  |  |  |  |
| obs.evening | integer | 4708 | 0.00 | 61 | 30.32 | 30 | 30 |  | 17.58 |  | 1 | 61 | 60 | 0.04 | 0.54 | -1.20 | -8.44 |
| comp.evening | integer | 4708 | 0.00 | 37 | 40.07 | 42 | 42 |  | 12.34 |  | 1 | 60 | 59 | -0.82 | -11.53 | 0.00 | -0.02 |
| n.em.ev | factor | 3143 | 0.33 | 3 |  |  | 2175 | 1 |  | 0.43 |  |  |  |  |  |  |  |
| n.em.int | integer | 3143 | 0.33 | 102 | 59.89 | 66 | 66 |  | 23.85 |  | 0 | 100 | 100 | -0.76 | -8.71 | -0.19 | -1.07 |
| n.em.cont | integer | 3143 | 0.33 | 102 | 48.68 | 44 | 44 |  | 26.45 |  | 0 | 100 | 100 | 0.15 | 1.66 | -1.07 | -6.10 |
| n.er.rel | integer | 3143 | 0.33 | 102 | 43.51 | 43 | 43 |  | 29.12 |  | 0 | 100 | 100 | -0.01 | -0.09 | -1.25 | -7.14 |
| n.er.eng | integer | 3143 | 0.33 | 102 | 45.83 | 50 | 50 |  | 28.40 |  | 0 | 100 | 100 | -0.08 | -0.92 | -1.17 | -6.72 |
| n.er.rum | integer | 3143 | 0.33 | 102 | 52.07 | 60 | 60 |  | 27.04 |  | 0 | 100 | 100 | -0.42 | -4.78 | -0.89 | -5.12 |
| n.er.reap | integer | 3143 | 0.33 | 100 | 31.78 | 27 | 27 |  | 25.42 |  | 0 | 100 | 100 | 0.59 | 6.70 | -0.66 | -3.78 |
| n.er.dis | integer | 3143 | 0.33 | 102 | 54.61 | 62 | 62 |  | 27.84 |  | 0 | 100 | 100 | -0.46 | -5.28 | -0.82 | -4.67 |
| n.er.sup | integer | 3143 | 0.33 | 102 | 54.66 | 61 | 61 |  | 27.01 |  | 0 | 100 | 100 | -0.45 | -5.18 | -0.78 | -4.47 |
| p.em.ev | factor | 3143 | 0.33 | 3 |  |  | 2451 | 1 |  | 0.34 |  |  |  |  |  |  |  |
| p.em.int | integer | 3143 | 0.33 | 101 | 66.30 | 70 | 70 |  | 20.03 |  | 0 | 100 | 100 | -0.94 | -10.79 | 0.89 | 5.09 |
| p.er.eng | integer | 3143 | 0.33 | 102 | 59.57 | 65 | 65 |  | 25.12 |  | 0 | 100 | 100 | -0.74 | -8.51 | -0.24 | -1.37 |
| p.er.sav | integer | 3143 | 0.33 | 102 | 58.69 | 65 | 65 |  | 25.05 |  | 0 | 100 | 100 | -0.66 | -7.61 | -0.38 | -2.16 |
| gender.dum | factor | 4708 | 0.00 | 3 |  |  | 4100 | 1 |  | 0.22 |  |  |  |  |  |  |  |
| Age_B | integer | 4564 | 0.03 | 18 | 22.29 | 21 | 21 |  | 6.04 |  | 18 | 53 | 35 | 3.05 | 42.06 | 10.42 | 71.88 |
| Student_B | numeric | 4564 | 0.03 | 3 | 0.92 | 1 | 1 |  | 0.27 |  | 0 | 1 | 1 | -3.10 | -42.71 | 7.59 | 52.34 |
| micro | integer | 4708 | 0.00 | 2 | 0.13 | 0 | 0 |  | 0.34 |  | 0 | 1 | 1 | 2.21 | 30.98 | 2.89 | 20.24 |

### Legend

- **Name**: Variable name
- **type**: Data type of the variable
- **missing**: Proportion of missing values for this variable
- **unique**: Number of unique values
- **mean**: Mean value
- **median**: Median value
- **mode**: Most common value (for categorical variables, this shows the
  frequency of the most common category)
- **mode_value**: For categorical variables, the value of the most
  common category
- **sd**: Standard deviation (measure of dispersion for numerical
  variables
- **v**: Agresti’s V (measure of dispersion for categorical variables)
- **min**: Minimum value
- **max**: Maximum value
- **range**: Range between minimum and maximum value
- **skew**: Skewness of the variable
- **skew_2se**: Skewness of the variable divided by 2\*SE of the
  skewness. If this is greater than abs(1), skewness is significant
- **kurt**: Kurtosis (peakedness) of the variable
- **kurt_2se**: Kurtosis of the variable divided by 2\*SE of the
  kurtosis. If this is greater than abs(1), kurtosis is significant.

This codebook was generated using the [Workflow for Open Reproducible
Code in Science (WORCS)](https://osf.io/zcvbs/)
