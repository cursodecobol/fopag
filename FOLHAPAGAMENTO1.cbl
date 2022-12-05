      ******************************************************************
      * Author: Curso de Cobol - www.cursodecobol.com.br
      * Date  : 07/02/2021
      * Purpose: Processar uma folha de pagamento
      * Tectonics: cobc
      * VSCode: Reabrit com code page ISO 8859-1
      *
      ******************************************************************
      * FOLHAPAGAMENTO1: Código inicial; montamos somente a estrutura
      *                  básica, incluindo leitura.
      *                  Fizemos alguns testes, detectamos situações de
      *                  erro, e de loop infinito.
      *                  - Erro no copybook afeta o programa.
      *                  - Esquecer de "ligar" a condição de fim provoca
      *                    situação de loop infinito.
      *                  - DISPLAY é o "debug" natural mais efetivo para
      *                    detecção de erros em tempo de construção.
      *                    (equivale ao System.out.println() do Java
      ******************************************************************
      * Comentários:
      * ------------
      * Este é um programa de computador escrito em COBOL
      * para fins estritamente didáticos.
      * Ele realiza o cálculo de uma folha de pagamento fictícia,
      * com base nas horas trabalhadas por cada funcionário.
      * Estes apontamentos chegam por meio de transmissão de arquivo,
      * cujo layout está definido no Copybook APONTAMENTOS.cpy.
      * Será fixada uma taxa adicional de 50% sobre horas extras.
      * ARQUIVOS DE SAIDA
      * - Registros processados com sucesso: gravados no arq. FOPAG.TXT
      * - Registros c/erro no processamento: gravados no arq. FOPAGR.TXT
      *
      * Roteiro:
      * 1) Somar vencimentos do mês:
      *    - salário registrado na CTPS
      *    - horas extras
      *    - salário família
      *    - adicionais como: insalubridade, noturno
      ******************************************************************
      * VRS001 - FEV/2021 - IMPLANTACAO
      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. FOLHAPAGAMENTO1.
       DATE-WRITTEN. 2021-02-07.
      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
      *-------------*
           DECIMAL-POINT IS COMMA.
      *----------------------------------------------------------------*
       INPUT-OUTPUT SECTION.
      *----------------------------------------------------------------*
       FILE-CONTROL.
      *-------------*
           SELECT APONTAMENTOS         ASSIGN TO "APONTAMENTOS.DAT"
                                       ORGANIZATION IS LINE SEQUENTIAL.
           SELECT RELATORIOFOLHA       ASSIGN TO "RELATORIOFOLHA.TXT"
                                       ORGANIZATION IS LINE SEQUENTIAL.
           SELECT RELATORIOREJ         ASSIGN TO "RELATORIOREJ.TXT"
                                       ORGANIZATION IS LINE SEQUENTIAL.
      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       FILE SECTION.
      *----------------------------------------------------------------*
       FD APONTAMENTOS.
       01 APONTAMENTO-FD.
       COPY APONTAMENTOS
       .
       
       FD RELATORIOFOLHA.
       01 RELATORIO-FOLHA-FD.
       COPY RELATORIOFOLHA
       .
       
       FD RELATORIOREJ.
       01 RELATORIOREJ-FD          PIC X(132).

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
       01 NUM-VERSAO               PIC 9(03) VALUE 1.

      *    Indicadores
       01 IND-FIM-APONTAMENTO      PIC X     VALUE 'N'.
          88  FIM-APONTAMENTO                VALUE 'S'.

      *    Data e hora correntes
       01 DATA-CORRENTE            PIC 9(8)    VALUE ZEROS.
       01 FILLER REDEFINES DATA-CORRENTE.
          03 DATA-CORRENTE-ANO     PIC 9(4).
          03 DATA-CORRENTE-MES     PIC 9(2).
          03 DATA-CORRENTE-DIA     PIC 9(2).
       01 HORA-CORRENTE            PIC X(8).
       01 FILLER REDEFINES HORA-CORRENTE.
          03 HORA-CORRENTE-HH      PIC 9(02).
          03 HORA-CORRENTE-MM      PIC 9(02).
          03 HORA-CORRENTE-SS      PIC 9(02).
          03 HORA-CORRENTE-CC      PIC 9(02).

      *    Contadores
       01 CNT-APONTAMENTOS         PIC 9(9)    VALUE ZEROS.

      ******************************************************************
       PROCEDURE                       DIVISION.
      ******************************************************************
       000000-ROTINA-PRINCIPAL.
           PERFORM 100000-INICIO

           PERFORM 200000-PROCESSA 
             UNTIL FIM-APONTAMENTO

           PERFORM 300000-FINALIZA

           GOBACK.
      *----------------------------------------------------------------*
       100000-INICIO                   SECTION.
      *----------------------------------------------------------------*
           DISPLAY '000 - INICIO PROGRAMA FOLHAPAGAMENTO V.' NUM-VERSAO
           ACCEPT DATA-CORRENTE FROM DATE YYYYMMDD
           ACCEPT HORA-CORRENTE FROM TIME
      *    Já move data e hora corrente para relatorio
           MOVE '01/01/0001' TO TX-DATA
           MOVE '00:00:00'   TO TX-HORA
           MOVE DATA-CORRENTE-DIA TO TX-DATA-DIA
           MOVE DATA-CORRENTE-MES TO TX-DATA-MES
           MOVE DATA-CORRENTE-ANO TO TX-DATA-ANO
           MOVE HORA-CORRENTE-HH  TO TX-HORA-HH
           MOVE HORA-CORRENTE-MM  TO TX-HORA-MM
           MOVE HORA-CORRENTE-SS  TO TX-HORA-SS

           OPEN INPUT  APONTAMENTOS
                OUTPUT RELATORIOFOLHA
                       RELATORIOREJ

      *    Ao abrir arquivo de dados, leia o primeiro registro
           PERFORM 500000-READ-APONTAMENTOS
           .
      *----------------------------------------------------------------*
       200000-PROCESSA                 SECTION.
      *----------------------------------------------------------------*
      *    Dica: Muitas vezes será útil exibir o nome da SECTION ou 
      *          parágrafo que está sendo executado.
           DISPLAY "20000-PROCESSA"

      * Dica: use o DISPLAY para exiibir alguns campos e checar se
      *       está tudo em ordem.
      *     DISPLAY "***************************************************"
           DISPLAY "NUM-MATRICULA          =" NR-MATRICULA
      *     DISPLAY "NOME-EMPREGADO         =" NOME-EMPREGADO
      *     DISPLAY "VLR-REMUNERACAO-HORA   =" VLR-REMUNERACAO-HORA
      *     DISPLAY "QTD-HORAS-NORMAIS      =" QTD-HORAS-NORMAIS
      *     DISPLAY "QTD-HORAS-EXTRAS-50PC  =" QTD-HORAS-EXTRAS-50PC
      *     DISPLAY "QTD-HORAS-EXTRAS-100PC =" QTD-HORAS-EXTRAS-100PC
      *     DISPLAY "QTD-DEPENDENTES-IRPF   =" QTD-DEPENDENTES-IRPF
      *     DISPLAY "QTD-FILHOS-SF          =" QTD-FILHOS-SF


      * Esquecer de ler o próximo registro TAMBÉM causa loop infinito...
           PERFORM 500000-READ-APONTAMENTOS
           .
      *----------------------------------------------------------------*
       300000-FINALIZA                 SECTION.
      *----------------------------------------------------------------*
           DISPLAY "FINALIZA"
      * Esquecer de fechar os arquivos não dá erro, mas dá mensagem de
      * alerta (warning)
           CLOSE APONTAMENTOS RELATORIOFOLHA RELATORIOREJ.

      *----------------------------------------------------------------*
       500000-READ-APONTAMENTOS        SECTION.
      *----------------------------------------------------------------*
           READ APONTAMENTOS INTO APONTAMENTO-FD
           AT END
              MOVE 'S' TO IND-FIM-APONTAMENTO
      *       Cuidado: se esquecer de ligar este flag vai entrar em loop
           NOT AT END
              ADD 1 TO CNT-APONTAMENTOS
           END-READ
           .

      ******************************************************************
       END PROGRAM FOLHAPAGAMENTO1.
      ******************************************************************
