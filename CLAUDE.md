# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Econometric research project analyzing household time-use and expenditure behavior using Chilean national survey data (ENUT - Encuesta Nacional de Uso de Tiempo). Implements the Jara-Diaz hybrid model for work-leisure-consumption tradeoffs.

## Commands

### Data Processing

```bash
# Full preprocessing pipeline (raw ENUT data -> cleaned datasets)
Rscript data_processing/data_processing.R

# Expenditure imputation from EPF survey
Rscript data_processing/expenditures.R

# Twin matrix generation for weekend imputation (requires Python)
python data_processing/gemelos_matriz.py
```

### Model Estimation

```bash
# Primary model: multiple time categories + expenditures (many parameters)
Rscript models/thphntne.R

# Simple model: 3 parameters (PH, theta_w, sigma)
Rscript models/thph1t1e.R

# Quadratic utility model
Rscript models/quadratic.R

# Structural equation model (lavaan)
Rscript models/SEM.R
```

## Architecture

### Data Flow

```
data/raw/          ->  data_processing/data_processing.R  ->  data/enut-i-{11,25}.{dta,csv}
data/raw/ + gastos ->  data_processing/expenditures.R      ->  data/enut-i-{11,25}G.{dta,csv}
enut-i-*G.csv      ->  models/*.R                          ->  output/
```

- `enut-i-11` / `enut-i-25`: Time allocation datasets with 11 or 25 activity categories
- `enut-i-*G`: Same datasets with imputed household expenditures appended
- `data/raw/` is gitignored (excluded from version control)

### Core Components

**`apollo_jaradiaz.R`** - Custom Apollo likelihood function implementing the Jara-Diaz theoretical framework. This is the heart of the model: given observed time and expenditure allocations, it computes the log-likelihood under a normal error structure. Parameters: preference weights (PH vector), wage-leisure tradeoff (theta_w), error terms (sigma), and correlations (rho).

**`utils.R`** - Shared data loading and helper functions used across all model scripts. Load this first in any new model script.

**`data_processing/processing_functions.R`** - Outlier removal (Vallejo method), time constraint enforcement (168 hours/week), wage rate computation (w = income / work_time), and activity aggregation.

**`data_processing/gemelos_matriz.py`** - Convex optimization (cvxpy) to construct a "twin matrix" mapping weekday respondents to weekend counterparts for imputation. Uses multiprocessing for parallelism.

### Model Structure

All model scripts (`models/*.R`) follow the same pattern:
1. Load data via `utils.R`
2. Define Apollo parameters and starting values
3. Set up the likelihood using `apollo_jaradiaz.R` or inline functions
4. Run multi-start optimization (100 random starting points)
5. Filter results by convergence quality (negative eigenvalues, convergence codes)
6. Save estimates to `output/`

**Key variables:**
- `Tw`: paid work time
- `Tfleisure`: leisure time
- `Tc`: committed time (sleep, personal care)
- `ta`: total available time (168 hours/week)
- `w`: wage rate (income / Tw)
- `Ef1`, `Ef2`: discretionary expenditures
- `Ec`: committed expenditures

### R Dependencies

`apollo`, `dplyr`, `tidyr`, `ggplot2`, `haven`, `lavaan`, `comprehenr`, `fitdistrplus`, `matrixcalc`, `randcorr`, `glue`, `purrr`, `lubridate`

### Python Dependencies

`polars`, `pandas`, `numpy`, `cvxpy`, `tqdm`
