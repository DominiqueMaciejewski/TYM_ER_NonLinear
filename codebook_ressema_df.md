Codebook created on 2023-11-17 at 2023-11-17 17:42:47.327112
================

A codebook contains documentation and metadata describing the contents,
structure, and layout of a data file.

## Dataset description

The data contains 7903 cases and 11 variables.

## Codebook

| name    | type    |    n | missing | unique |  mean | median |   mode | mode_value |    sd |    v |  min |    max |  range |  skew | skew_2se |  kurt | kurt_2se |
|:--------|:--------|-----:|--------:|-------:|------:|-------:|-------:|-----------:|------:|-----:|-----:|-------:|-------:|------:|---------:|------:|---------:|
| sema_id | factor  | 7903 |    0.00 |    129 |       |        | 122.00 |    3934451 |       | 0.99 |      |        |        |       |          |       |          |
| int     | integer | 6101 |    0.23 |    102 | 39.93 |  35.00 |  35.00 |            | 27.28 |      | 0.00 | 100.00 | 100.00 |  0.26 |     4.08 | -1.12 |    -8.94 |
| ctrl    | integer | 6061 |    0.23 |    102 | 49.79 |  53.00 |  53.00 |            | 29.97 |      | 0.00 | 100.00 | 100.00 | -0.04 |    -0.61 | -1.23 |    -9.77 |
| relax2  | integer | 6024 |    0.24 |    102 | 32.96 |  25.00 |  25.00 |            | 30.82 |      | 0.00 | 100.00 | 100.00 |  0.55 |     8.69 | -1.06 |    -8.40 |
| exp1    | integer | 6036 |    0.24 |    102 | 41.13 |  36.00 |  36.00 |            | 30.24 |      | 0.00 | 100.00 | 100.00 |  0.24 |     3.79 | -1.18 |    -9.33 |
| rumi2   | integer | 6027 |    0.24 |    102 | 40.60 |  35.00 |  35.00 |            | 29.45 |      | 0.00 | 100.00 | 100.00 |  0.20 |     3.13 | -1.21 |    -9.58 |
| reap1   | integer | 6028 |    0.24 |    102 | 41.99 |  37.00 |  37.00 |            | 29.86 |      | 0.00 | 100.00 | 100.00 |  0.13 |     2.12 | -1.23 |    -9.74 |
| dist2   | integer | 6022 |    0.24 |    102 | 48.64 |  58.00 |  58.00 |            | 31.21 |      | 0.00 | 100.00 | 100.00 | -0.17 |    -2.65 | -1.30 |   -10.28 |
| sup1    | integer | 6029 |    0.24 |    102 | 37.13 |  29.00 |  29.00 |            | 30.38 |      | 0.00 | 100.00 | 100.00 |  0.42 |     6.58 | -1.12 |    -8.85 |
| PSS     | numeric | 7903 |    0.00 |     13 |  1.69 |   1.75 |   1.75 |            |  0.64 |      | 0.25 |   3.25 |   3.00 |  0.36 |     6.48 | -0.25 |    -2.30 |
| NA.     | numeric | 7903 |    0.00 |     49 |  3.77 |   3.91 |   3.91 |            |  1.20 |      | 1.27 |   6.91 |   5.64 |  0.10 |     1.84 | -0.40 |    -3.63 |

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
