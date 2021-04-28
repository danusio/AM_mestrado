# AM_mestrado
Material da disciplina de aprendizado de máquina - Mestrado em PO ITA.

# DESCRIÇÃO DOS ARQUIVOS

- conv_fst_csv.R: lê o arquvo .fst com os dados de treino e escreve um arquivo .csv com esses dados.
- geracao_dados_treino.R: consolida os dados de treino de todas as ações a serem analisadas.
- datasets_treino.fst: arquivo .fst com os dados de treino.
- conj_min_cor.R: encontra o conjunto de *X* ações (10, no exemplo) com a menor correlação média.
- simul_completa.R: *script* para geração das métricas R² e MAE da previsão. Nas linhas 169 e 353:

        `tickers[tick_sel[c(1,2)]]`
        `rownames(meas) <- tickers[tick_sel[c(1,2)]]`
  
  correspondentes ao laço `for` e à nomeação de linhas da matriz com as medidas de performance, substituir os índices `[c(1,2)]` pelos índices correspondentes às ações que serão analisadas.
