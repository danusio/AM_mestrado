# AM_mestrado
Material para o projeto da disciplina de Aprendizado de Máquina - Mestrado em PO, ITA.

# DESCRIÇÃO DOS ARQUIVOS

- conv_fst_csv.R: lê o arquvo .fst com os dados de treino e escreve um arquivo .csv com esses dados.
- geracao_dados_treino.R: consolida os dados de treino de todas as ações a serem analisadas.
- datasets_treino.fst: arquivo .fst com os dados de treino.
- conj_min_cor.R: encontra o conjunto de *X* ações (10, no exemplo) com a menor correlação média.
- simul_completa.R: *script* para geração das métricas R² e MAE da previsão, além de armazenar as *features* selecionadas para as predições.
- analise_result.R: análise das saídas de simul_completa.R (*features* mais importantes e métricas de regressão).
- arima_pred.R: predição baseada no modelo ARIMA.
