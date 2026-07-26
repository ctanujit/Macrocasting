# Macrocasting: Macroeconomic Forecasting for the G7 countries under Uncertainty Shocks

[![Paper](https://img.shields.io/badge/arXiv-2510.23347-b31b1b.svg)](https://arxiv.org/abs/2510.23347)
[![Repository](https://img.shields.io/badge/GitHub-Macrocasting-black.svg)](https://github.com/ctanujit/Macrocasting)

This repository contains the code, notebooks, and supporting resources for the paper:

**“Macroeconomic Forecasting for the G7 countries under Uncertainty Shocks”**  
**Authors:** Shovon Sengupta, Sunny Kumar Singh, Tanujit Chakraborty

---

## Abstract

Accurate macroeconomic forecasting has become harder amid geopolitical disruptions, policy reversals, and volatile financial markets. Conventional vector autoregressions (VARs) overfit in high-dimensional settings, while threshold VARs struggle with time-varying interdependencies and complex parameter structures. This work addresses these limitations by extending the Sims–Zha Bayesian VAR with exogenous variables (**SZBVARx**) to incorporate domain-informed shrinkage and four newspaper-based uncertainty shocks—economic policy uncertainty, geopolitical risk, US equity market volatility, and US monetary policy uncertainty.

Using G7 data, the study analyzes spillovers from uncertainty shocks to five core variables—unemployment, real broad effective exchange rates, short-term rates, oil prices, and CPI inflation—combining wavelet coherence (time–frequency dynamics) with nonlinear local projections (state-dependent impulse responses). Out-of-sample results at 12- and 24-month horizons show that SZBVARx outperforms strong econometric and machine-learning benchmarks, with additional robustness confirmed via Murphy difference diagrams, multivariate Diebold–Mariano tests, and Giacomini–White predictability tests. Credible Bayesian prediction intervals further support scenario analysis and risk-aware decision-making.

---

## Repository Structure

- [`code/`](https://github.com/ctanujit/Macrocasting/tree/main/code): Main source modules for modeling, diagnostics, causality analysis, and evaluation.
- [`dataset/`](https://github.com/ctanujit/Macrocasting/tree/main/dataset): Country-wise datasets for G7 economies.
- [`Final_Output_File/`](https://github.com/ctanujit/Macrocasting/tree/main/Final_Output_File): Final exported outputs and artifacts.

### Code Modules

#### 1) Algorithms
- [`code/Algorithms/`](https://github.com/ctanujit/Macrocasting/tree/main/code/Algorithms)
- Proposed model (SZBVARx):
  - [`SZBVARx_G7_12M_24M_paper.R`](https://github.com/ctanujit/Macrocasting/blob/main/code/Algorithms/SZBVARx/SZBVARx_G7_12M_24M_paper.R)
  - [`SZBVARx_HP_tuning_G7_12M_24M_paper.R`](https://github.com/ctanujit/Macrocasting/blob/main/code/Algorithms/SZBVARx/SZBVARx_HP_tuning_G7_12M_24M_paper.R)
  - [`szbvarx_orchestrator_utils_G7_paper.R`](https://github.com/ctanujit/Macrocasting/blob/main/code/Algorithms/SZBVARx/szbvarx_orchestrator_utils_G7_paper.R)
  - [`PPI_Charts_G7_12M_24M_paper.R`](https://github.com/ctanujit/Macrocasting/blob/main/code/Algorithms/SZBVARx/PPI_Charts_G7_12M_24M_paper.R)
  - [`BPPI_Radar_Chart_24M_G7_paper.ipynb`](https://github.com/ctanujit/Macrocasting/blob/main/code/Algorithms/SZBVARx/BPPI_Radar_Chart_24M_G7_paper.ipynb)
- Baseline econometric models:
  - [`VAR_G7_12M_24M_paper.R`](https://github.com/ctanujit/Macrocasting/blob/main/code/Algorithms/VAR_G7_12M_24M_paper.R)
  - [`TVAR_G7_12M_24M_paper.R`](https://github.com/ctanujit/Macrocasting/blob/main/code/Algorithms/TVAR_G7_12M_24M_paper.R)
  - [`VES_G7_12M_24M_paper.R`](https://github.com/ctanujit/Macrocasting/blob/main/code/Algorithms/VES_G7_12M_24M_paper.R)
- Baseline machine/deep learning notebooks:
  - [`multivariate_XGBoost_G7_12M_24M_paper.ipynb`](https://github.com/ctanujit/Macrocasting/blob/main/code/Algorithms/multivariate_XGBoost_G7_12M_24M_paper.ipynb)
  - [`multivariate_CatBoost_G7_12M_24M_paper.ipynb`](https://github.com/ctanujit/Macrocasting/blob/main/code/Algorithms/multivariate_CatBoost_G7_12M_24M_paper.ipynb)
  - [`multivariate_LGBM_G7_12M_24M_paper.ipynb`](https://github.com/ctanujit/Macrocasting/blob/main/code/Algorithms/multivariate_LGBM_G7_12M_24M_paper.ipynb)
  - [`multivariate_BRNN_G7_12M_24M_paper.ipynb`](https://github.com/ctanujit/Macrocasting/blob/main/code/Algorithms/multivariate_BRNN_G7_12M_24M_paper.ipynb)
  - [`multivariate_NBEATSx_G7_12M_24M_paper.ipynb`](https://github.com/ctanujit/Macrocasting/blob/main/code/Algorithms/multivariate_NBEATSx_G7_12M_24M_paper.ipynb)
  - [`multivariate_NHiTS_G7_12M_24M_paper.ipynb`](https://github.com/ctanujit/Macrocasting/blob/main/code/Algorithms/multivariate_NHiTS_G7_12M_24M_paper.ipynb)
  - [`multivariate_TFTx_G7_12M_24M_paper.ipynb`](https://github.com/ctanujit/Macrocasting/blob/main/code/Algorithms/multivariate_TFTx_G7_12M_24M_paper.ipynb)
  - [`multivariate_TSMIXER_G7_12M_24M_paper.ipynb`](https://github.com/ctanujit/Macrocasting/blob/main/code/Algorithms/multivariate_TSMIXER_G7_12M_24M_paper.ipynb)
  - [`multivariate_TiDE_G7_12M_24M_paper.ipynb`](https://github.com/ctanujit/Macrocasting/blob/main/code/Algorithms/multivariate_TiDE_G7_12M_24M_paper.ipynb)
  - [`multivariate_DTS_XGB_G7_12M_24M_paper.ipynb`](https://github.com/ctanujit/Macrocasting/blob/main/code/Algorithms/multivariate_DTS_XGB_G7_12M_24M_paper.ipynb)
  - [`multivariate_DTS_CB_G7_12M_24M_paper.ipynb`](https://github.com/ctanujit/Macrocasting/blob/main/code/Algorithms/multivariate_DTS_CB_G7_12M_24M_paper.ipynb)
- Python dependency snapshot for notebook baselines:
  - [`requirements.txt`](https://github.com/ctanujit/Macrocasting/blob/main/code/Algorithms/requirements.txt)

#### 2) Data Analysis
- [`code/Data_Analysis/`](https://github.com/ctanujit/Macrocasting/tree/main/code/Data_Analysis)
- Key scripts:
  - [`global_characteristics_summary stats_mulvar_paper.R`](https://github.com/ctanujit/Macrocasting/blob/main/code/Data_Analysis/global_characteristics_summary%20stats_mulvar_paper.R)
  - [`Trend_chart_G7_paper.R`](https://github.com/ctanujit/Macrocasting/blob/main/code/Data_Analysis/Trend_chart_G7_paper.R)
  - [`ACF_PACF_G7_Charts_paper.R`](https://github.com/ctanujit/Macrocasting/blob/main/code/Data_Analysis/ACF_PACF_G7_Charts_paper.R)
  - [`OLS-CUSMUM_test_G7_for_SB_V2_paper.R`](https://github.com/ctanujit/Macrocasting/blob/main/code/Data_Analysis/OLS-CUSMUM_test_G7_for_SB_V2_paper.R)
  - [`Outlier_test_G7_mulvar_paper.R`](https://github.com/ctanujit/Macrocasting/blob/main/code/Data_Analysis/Outlier_test_G7_mulvar_paper.R)
  - [`Test_for_endogeneity_WH_test_G7_V2_paper.R`](https://github.com/ctanujit/Macrocasting/blob/main/code/Data_Analysis/Test_for_endogeneity_WH_test_G7_V2_paper.R)

#### 3) Causality Analysis
- [`code/Causality_Analysis/`](https://github.com/ctanujit/Macrocasting/tree/main/code/Causality_Analysis)
- Key scripts:
  - [`FDR_Corrected_WGC_G7_paper.R`](https://github.com/ctanujit/Macrocasting/blob/main/code/Causality_Analysis/FDR_Corrected_WGC_G7_paper.R)
  - [`WCA_FDR_correction_Plots_G7_paper.R`](https://github.com/ctanujit/Macrocasting/blob/main/code/Causality_Analysis/WCA_FDR_correction_Plots_G7_paper.R)
  - [`NL_IRF_LP_G7_Plots_Paper.R`](https://github.com/ctanujit/Macrocasting/blob/main/code/Causality_Analysis/NL_IRF_LP_G7_Plots_Paper.R)

#### 4) Performance Evaluation
- [`code/Performance_Evaluation/`](https://github.com/ctanujit/Macrocasting/tree/main/code/Performance_Evaluation)
- Main notebook:
  - [`Multivariate_G7_performance_evaluation_12M_24M_paper.ipynb`](https://github.com/ctanujit/Macrocasting/blob/main/code/Performance_Evaluation/Multivariate_G7_performance_evaluation_12M_24M_paper.ipynb)
- Supporting folders:
  - [`Forecasts_Datasets/`](https://github.com/ctanujit/Macrocasting/tree/main/code/Performance_Evaluation/Forecasts_Datasets)
  - [`Forecasts_Performance_Eval_results/`](https://github.com/ctanujit/Macrocasting/tree/main/code/Performance_Evaluation/Forecasts_Performance_Eval_results)

#### 5) Robustness Analysis
- [`code/Robustness_Analysis/`](https://github.com/ctanujit/Macrocasting/tree/main/code/Robustness_Analysis)
- Key scripts:
  - [`Murphydiag_diff_G7_12M_24M_paper.R`](https://github.com/ctanujit/Macrocasting/blob/main/code/Robustness_Analysis/Murphydiag_diff_G7_12M_24M_paper.R)
  - [`multivariate_DM_test_12M_24M_G7_paper.R`](https://github.com/ctanujit/Macrocasting/blob/main/code/Robustness_Analysis/multivariate_DM_test_12M_24M_G7_paper.R)
  - [`MCB_test_G7_Paper.R`](https://github.com/ctanujit/Macrocasting/blob/main/code/Robustness_Analysis/MCB_test_G7_Paper.R)
- Supporting folders:
  - [`Input_dataset/`](https://github.com/ctanujit/Macrocasting/tree/main/code/Robustness_Analysis/Input_dataset)
  - [`Results/`](https://github.com/ctanujit/Macrocasting/tree/main/code/Robustness_Analysis/Results)

---

## Data

Country-wise input data for G7 economies are organized under:

- [`dataset/canada`](https://github.com/ctanujit/Macrocasting/tree/main/dataset/canada)
- [`dataset/france`](https://github.com/ctanujit/Macrocasting/tree/main/dataset/france)
- [`dataset/germany`](https://github.com/ctanujit/Macrocasting/tree/main/dataset/germany)
- [`dataset/italy`](https://github.com/ctanujit/Macrocasting/tree/main/dataset/italy)
- [`dataset/japan`](https://github.com/ctanujit/Macrocasting/tree/main/dataset/japan)
- [`dataset/uk`](https://github.com/ctanujit/Macrocasting/tree/main/dataset/uk)
- [`dataset/usa`](https://github.com/ctanujit/Macrocasting/tree/main/dataset/usa)

---

## Reproducibility Guidance

To reproduce the key empirical workflow:

1. Run preprocessing and descriptive diagnostics under [`code/Data_Analysis`](https://github.com/ctanujit/Macrocasting/tree/main/code/Data_Analysis).
2. Estimate/tune the proposed model using scripts under [`code/Algorithms/SZBVARx`](https://github.com/ctanujit/Macrocasting/tree/main/code/Algorithms/SZBVARx).
3. Execute benchmark models in [`code/Algorithms`](https://github.com/ctanujit/Macrocasting/tree/main/code/Algorithms).
4. Run evaluation notebook in [`code/Performance_Evaluation`](https://github.com/ctanujit/Macrocasting/tree/main/code/Performance_Evaluation).
5. Run robustness/statistical testing modules in [`code/Robustness_Analysis`](https://github.com/ctanujit/Macrocasting/tree/main/code/Robustness_Analysis).
6. Run causality and nonlinear response scripts in [`code/Causality_Analysis`](https://github.com/ctanujit/Macrocasting/tree/main/code/Causality_Analysis).

---

## Citation

If you use this repository, please cite:

```bibtex
@article{sengupta2025macroeconomic,
  title   = {Macroeconomic Forecasting for the G7 countries under Uncertainty Shocks},
  author  = {Sengupta, Shovon and Singh, Sunny Kumar and Chakraborty, Tanujit},
  journal = {arXiv preprint arXiv:2510.23347},
  year    = {2025},
  url     = {https://arxiv.org/abs/2510.23347}
}
```

Paper link: [https://arxiv.org/abs/2510.23347](https://arxiv.org/abs/2510.23347)

---

## Notes

- Primary implementation is in **R** with supporting **Jupyter notebooks** for several machine/deep-learning benchmarks and evaluation workflows.
- This README intentionally maps all major scripts/notebooks with direct hyperlinks to make navigation and reproducibility easier.
