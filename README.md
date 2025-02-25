# Análisis Predictivo de la Suscripción a Depósitos a Plazo Fijo en Marketing Bancario

### Jorge Antonio Gómez García

Este repositorio contiene un proyecto de análisis predictivo aplicado a campañas de marketing en el sector bancario. El objetivo es desarrollar y comparar modelos de regresión logística (con y sin técnicas de balanceo de clases) para predecir la probabilidad de que un cliente suscriba un depósito a plazo fijo. Dado que la variable objetivo presenta un marcado desbalance (solo el 11.7% de los clientes contratan el producto), se exploran estrategias como la optimización de hiperparámetros y el uso de técnicas de re-muestreo (SMOTE y undersampling) para mejorar el desempeño predictivo.

---

## Contenido del Repositorio

- **README.md**: Este archivo, con la descripción general del proyecto, instrucciones de uso y detalles sobre la estructura.
- **script_principal.Rmd**: Script principal en R que implementa el análisis, desde la preparación de datos y ajuste de modelos hasta la generación de gráficos y tablas.
- **plots_and_stats.R**: Funciones personalizadas para la generación de gráficos y cálculo de métricas estadísticas.
- **bank-full.csv**: Conjunto de datos original utilizado (campañas telefónicas de un banco portugués).
- **img/**: Carpeta que contiene los gráficos y figuras generadas (por ejemplo, curvas ROC, distribuciones de predicción y matrices de confusión).
- **Reporte.pdf**: Documento final con el análisis completo, resultados y conclusiones.

---

## Descripción del Proyecto

### Objetivo

Desarrollar un modelo predictivo que, mediante regresión logística, permita identificar a los clientes con mayor probabilidad de suscribir un depósito a plazo fijo en una campaña de marketing. Se hace especial énfasis en:

- El ajuste de hiperparámetros para mejorar la capacidad discriminativa.
- El manejo del desbalanceo de clases mediante técnicas como SMOTE combinado con undersampling.
- La evaluación del desempeño a través de múltiples métricas (exactitud, sensibilidad, especificidad, precisión, F1-Score, Kappa, Log-Loss y ROC-AUC).

### Metodología

Se han implementado y comparado tres enfoques principales:

- **Modelo M1 (Base)**: Regresión logística simple sin ajuste de hiperparámetros, aplicado a la muestra original desbalanceada. *Resultado*: Alta exactitud global pero con sensibilidad nula (no identifica casos positivos).
- **Modelo M2 (Ajustado)**: Regresión logística con optimización de hiperparámetros (regularización Lasso y Ridge) mediante validación cruzada, aplicado a la muestra original. *Resultado*: Mejora en la identificación de positivos, aunque aún presenta limitaciones en la sensibilidad y precisión.
- **Modelo M3 (Balanceado)**: Regresión logística con hiperparámetros óptimos aplicada a una muestra balanceada mediante SMOTE (para aumentar la clase minoritaria) y undersampling (para reducir la clase mayoritaria). *Resultado*: Mayor sensibilidad y capacidad discriminativa (ROC-AUC), aunque a costa de un incremento en falsos positivos.

### Resultados y Conclusiones

El análisis muestra que:

- **M1** resulta inadecuado para identificar la clase minoritaria.
- **M2** mejora la detección de clientes potenciales, pero aún presenta deficiencias en el equilibrio entre sensibilidad y precisión.
- **M3** ofrece el mejor desempeño en términos de identificación de clientes que contratan el depósito (mayor sensibilidad y un ROC-AUC superior), siendo la opción recomendada para campañas en contextos donde el costo de contactar falsos positivos es marginal frente a la pérdida de oportunidades.

Las conclusiones enfatizan la importancia de combinar métodos estadísticos clásicos con técnicas modernas de machine learning para abordar problemas de clasificación en entornos desbalanceados.

---

## Requisitos

- **R** (versión 4.x o superior) y **RStudio** (opcional, para facilitar la ejecución).
- Los siguientes paquetes de R (se pueden instalar con `install.packages()`):

  ```R
  install.packages(c('tidyverse', 'ggplot2', 'readr', 'tidymodels', 'glmnet', 'mlr3tuning', 'pROC', 'smotefamily', 'nnet', 'caret'))
  ```
