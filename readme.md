# Regression Analysis with Ridge, Lasso, and ElasticNet Models

This repository contains the project developed for the discipline **Applied Statistics II**, part of the Specialization in Applied Artificial Intelligence at the Federal University of Paraná (UFPR).

The project's objective is to apply, compare, and evaluate the performance of three regularized regression models (Ridge, Lasso, and ElasticNet) to predict the hourly wage of wives based on a set of socioeconomic variables.

## Team

* Francisco Viana  
* Gabriel Godinho
* Guilherme Bilibio
* José Augusto Viana
* Ricardo Monteiro
* Rodrigo Dittmar

## Professor

Prof. Dr. Arno P. Schmitz

## Analysis and Methodology

The study uses the `trabalhosalarios.RData` dataset. The dependent variable is `lwage` (the natural logarithm of the wife's hourly wage), and all other variables in the dataset were used as predictors.

The methodology followed these steps:
1.  **Preprocessing:** The data was split into training (80%) and test (20%) sets. Continuous variables were standardized (centered and scaled) to fit the requirements of the regularized models.
2.  **Model Training:**
    * **Ridge Regression:** Trained using `alpha = 0`.
    * **Lasso Regression:** Trained using `alpha = 1`.
    * **ElasticNet Regression:** Trained by searching for the best hyperparameters `alpha` and `lambda` via repeated cross-validation (`repeatedcv`).
3.  **Evaluation:** The models were evaluated on the test set using RMSE (Root Mean Square Error) and R² (Coefficient of Determination) as the primary metrics.

## Results

After training and evaluation, all three models showed very similar performance on the test set, with nearly identical RMSE and R² values.

### Comparative Model Table

| Model | RMSE | R² | Predicted Salary (US$) | Lower CI (95%) | Upper CI (95%) |
| :--- | :--- | :--- | :--- | :--- | :--- |
| **Ridge** | 0.989 | 0.259 | 8.61 | 8.42 | 8.80 |
| **Lasso** | 0.989 | 0.259 | 8.02 | 7.85 | 8.20 |
| **ElasticNet** | 0.989 | 0.259 | 8.78 | 8.59 | 8.97 |

### Prediction for a Specific Case

A prediction was made for a specific profile as requested in the assignment:

* `husage`: 40 years 
* `husunion`: 0 (Not in a union) 
* `husearns`: US$ 600 
* `huseduc`: 13 years 
* `husblck`: 1 (Black) 
* `hushisp`: 0 (Not Hispanic) 
* `hushrs`: 40 hours 
* `kidge6`: 1 (Has children over 6) 
* `age`: 38 years 
* `black`: 0 (Not Black) 
* `educ`: 13 years 
* `hispanic`: 1 (Hispanic) 
* `union`: 0 (Not in a union) 
* `exper`: 18 years 
* `kidlt6`: 1 (Has children under 6) 

The prediction results for this profile are included in the table above. The final predicted value and its confidence interval were obtained by applying the antilog (exponentiation) to the model's output, as the dependent variable `lwage` is in a logarithmic scale.

## Conclusion

The **Ridge model was chosen as the best** among the three evaluated models. The choice was based on the criterion of the **lowest Root Mean Square Error (RMSE)**, which was marginally the lowest for the Ridge model.

Additionally, the Ridge model shared the highest R² value (0.259) with the ElasticNet model, reinforcing its explanatory power. The confidence interval for the predicted hourly wage (US$ 8.42 to US$ 8.80) is relatively narrow, indicating good precision in the point estimate.

It is important to note that the performance differences between the three models were very small, suggesting that all are similarly competent for this specific dataset.
