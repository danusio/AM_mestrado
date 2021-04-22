# Feature Selection

- `FSelector`: funções `oneR`, `information.gain` e `chi.squared`, sem parâmetros de *tuning*.
- `caret`: funções `preProcess` e `predict`.

# Treinamento

- Modelos fracos: 
   + `gbm`: `shrinkage` = 0.1, `interaction.depth` = 4, `n.minobsinnode` = 10, `n.trees` = 200.
   + `kknn`: `kmax` = 5, `distance` = 1, `kernel` = "optimal".
   + `brnn`: `neurons`= 9.

- Modelo combinado:
  + `cubist`: *tuning* automático.

Os modelos individuais foram treinados no *framework* da biblioteca `caret`, enquanto que o modelo combinado usou o da biblioteca `caretEnsemble`.
