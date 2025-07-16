# DS-stroke-prediction
## 🧠 Stroke Prediction System using Modified Random Forest & ADASYN

This project focuses on building a robust stroke prediction model from a real-world healthcare dataset that was initially noisy and imbalanced. The raw dataset contained issues such as missing values, duplicates, categorical inconsistencies, and outliers. A complete data cleaning pipeline was developed to address these problems — including imputation for missing values, trimming whitespace, encoding categorical variables, and handling outliers using Winsorization.

To tackle the severe class imbalance (fewer stroke cases), a **Custom ADASYN (Adaptive Synthetic Sampling)** algorithm was implemented. This algorithm intelligently generates synthetic minority samples to balance the dataset without causing uncontrolled growth in data size — making it suitable for resource-constrained environments.

After balancing, a **Modified Random Forest** model was trained. Feature selection was applied based on importance scores to reduce noise and improve performance. The model achieved high accuracy, precision, recall, and AUC scores on both training and test sets.

This end-to-end pipeline ensures high reliability for predicting stroke risks, and demonstrates how clean data, class balancing, and model optimization can work together to support impactful medical decisions.

📊 ROC Curve (Train vs Test)

- **Train AUC**: 1.00  
- **Test AUC**: 0.99
