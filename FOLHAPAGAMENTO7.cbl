      ******************************************************************
      * Author: Curso de Cobol - www.cursodecobol.com.br
      * Date  : 07/02/2021
      * Purpose: Processar uma folha de pagamento
      * Tectonics: 
      * c:\gc312vbi\bin\cobc -x <path\to>\FOLHAPAGAMENTO.cbl      \
      * -std=ibm -Wall -I<\path\to>copybooks \
      * -I<\path\to\>anothercopybooks        \
      * -tFOLHAPAGAMENTO.LST -v -g -Xref -ftsymbols
      ******************************************************************
      *    FOLHAPAGAMENTO7.cbl : Nesta versão vamos tratar arquivo de
      *                          entrada vazio
      ******************************************************************
      * Comentários:
      * ------------
      * Este é um programa de computador escrito em COBOL para fins 
      * estritamente didáticos.
      * Este programa realiza o cálculo de uma folha de pagamento 
      * fictícia, com base nas horas trabalhadas por cada 
      * funcionário.
      * Estes apontamentos podem chegar, por exemplo, por meio de 
      * transmissão de arquivos.
      * O layout deste arquivo está definido no Copybook 
      * APONTAMENTOS.cpy.
      * Para fins de praticidade de cálculos, as horas extras serão 
      * remuneradas com taxas fixas adicionais de 50% e/ou 100%. 
      * - Em um sistema de Folha de Pagamentos completo existem centenas
      *   de parâmetros e regras de cálculo muito complexas.
      *
      * ARQUIVO  DE ENTRADA                :  APONTAMENTOS.DAT
      *  - para teste de arquivo vazio     :  APONTAMENTOSvazio.DAT
      * ARQUIVOS DE SAIDA
      * - Registros processados com sucesso:  RELATORIOFOLHA.TXT
      * - Registros c/erro no processamento:  RELATORIOREJ.TXT
      *
      * Especificação:
      * 1) Somar vencimentos do mês:
      *    - salário registrado na CTPS
      *    - horas extras
      *    - salário família
      *
      * 2) Descontar o INSS do salário bruto
      *
      * 3) Deduzir dependentes legais do salário bruto
      *
      * 4) Encontrar e aplicar a alíquota do IR sobre a base de cálculo
      *
      * 5) Emitir relatório
      *
      * Observação: 
      * Existe um problema conhecido, que deverá ser tratado nas novas
      * versões do GnuCOBOL. Trata-se da acentuação de palavras.
      * Note que no arquivo de entrada retirei todos os acentos das 
      * palavras, exceto no nome 'Érica', para exemplificar que, no
      * GnuCOBOL, ele não está reconhecendo corretamente os caracteres
      * especiais.
      * Há previsão de implementação de instruções relacionadas a
      * 'COLLATING SEQUENCE', e 'NATIONAL'.
      ******************************************************************
      * VRS001 - FEV/2021 - IMPLANTACAO
      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. FOLHAPAGAMENTO7.
       DATE-WRITTEN. 2021-02-07.
      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************
       CONFIGURATION SECTION.
       Source-Computer. IBM-390.
       Object-Computer.
      *    Program Collating Sequence SPECIAL-SEQUENCE.
           PROGRAM COLLATING SEQUENCE  IS CCS. 

       SPECIAL-NAMES.
      *-------------*
           DECIMAL-POINT IS COMMA
           ALPHABET CCS FOR NATIONAL IS "A" THRU "Z".
           
      *    ALPHABET SPECIAL-SEQUENCE IS STANDARD-2.
      *----------------------------------------------------------------*
       INPUT-OUTPUT SECTION.
      *----------------------------------------------------------------*
       FILE-CONTROL.
      *-------------*
           SELECT APONTAMENTOS
                                       ASSIGN TO "APONTAMENTOS.DAT"
      *                                 ASSIGN TO "APONTAMENTOSVAZIO.DAT"
      *                                     ASSIGN TO "APONTAMENTOZ.TXT"
                                       ORGANIZATION IS LINE SEQUENTIAL
                                       FILE STATUS IS FS-APONTAMENTOS.
           SELECT RELATORIOFOLHA
                                       ASSIGN TO "RELATORIOFOLHA.TXT"
      *                                ASSIGN TO "RELATORIOFOLA.TXT"    Não deu erro!! Gerou o arquivo
      *           ASSIGN TO "C:\\ARQS\\RELATORIOFOLHA.TXT"              Deu erro FS=30
                                       ORGANIZATION IS LINE SEQUENTIAL
                                       FILE STATUS IS FS-RELATORIOFOLHA.
           SELECT RELATORIOREJ         ASSIGN TO "RELATORIOREJ.TXT"
                                       ORGANIZATION IS LINE SEQUENTIAL
                                       FILE STATUS IS FS-RELATORIOREJ.
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
           01 RELATORIO-FOLHA-FD       PIC X(200).
           .

           FD RELATORIOREJ.
           01 RELATORIOREJ-FD          PIC X(200) .

      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
           01 NUM-VERSAO               PIC 9(03) VALUE 7.

      *    Identifica conjunto de caracteres
           77 WS-LOCALE                PIC X(02) VALUE '00'.

      *    FILE-STATUS
           01 WS-FILE-STATUS           PIC 9(02) VALUE ZEROS.
           01 FS-APONTAMENTOS   REDEFINES WS-FILE-STATUS PIC 9(02).
           01 FS-RELATORIOFOLHA REDEFINES WS-FILE-STATUS PIC 9(02).
           01 FS-RELATORIOREJ   REDEFINES WS-FILE-STATUS PIC 9(02).
      *
           01 WS-NOME-ARQUIVO          PIC X(15) VALUE SPACES.
      *
           01 WS-LOCAL-ERRO            PIC 9(03) VALUE ZEROS.
           01 WS-MSG-ERRO          PIC X(70) VALUE SPACES.

      *    Indicadores
           01 IND-FIM-APONTAMENTO      PIC X     VALUE 'N'.
              88  FIM-APONTAMENTO                VALUE 'S'.
           01 IND-FIM-CALC-INSS        PIC X     VALUE 'N'.
              88  FIM-CALC-INSS                  VALUE 'S'.
           01 IND-FIM-CALC-IRRF        PIC X     VALUE 'N'.
              88  FIM-CALC-IRRF                  VALUE 'S'.

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
           01 CNT-LINHA                PIC 9(4)    COMP VALUE 99.
           01 CNT-PAGINA               PIC 9(4)    VALUE ZEROS.

      *    Constantes
      *    Quantidade de horas normais trabalhadas para calculo da CLT
           01 WS-QTD-HORAS-NORMAIS     PIC 9(3)V99 VALUE 220.


      *    Variáveis de cálculo da folha
      *    -----------------------------
      *    NOTA SOBRE ARREDONDAMENTOS:
      *    Para resolver problemas de arredondamento, é recomendado
      *    trabalhar com 3 ou mais casas decimais, até obter com
      *    maior precisão o resultado final.
      *
      *    O COBOL efetua um arredondamento automático, seguindo a
      *    regra matemática. 
      *    Veja por exemplo o que diz a Norma ABNT:
      *      "Quando o algarismo imediatamente seguinte ao último
      *      algarismo a ser conservado for 5 seguido de zeros,
      *      dever-se-á arredondar o algarismo a ser conservado para o
      *      algarismo par mais próximo. Consequentemente, o último a ser
      *      retirado, se for ímpar, aumentará uma unidade."
      *      - Regras de Arredondamento na Numeração Decimal
      *        Norma ABNT NBR 5891
      *    Veja um exemplo:
      *    a) com 2 casas decimais:
      *    - Iago tem um salário de 5,91 por hora;
      *       e Fez 9,5 Horas Extras com adicional de 50%, portanto:
      *            Hora Extra 50% = 5,91 * 1,5 = 8,86
      *            9,5 Horas Extras = R$ 84,17
      *    b) com 3 casas decimais:
      *       Hora Extra 50% = 5,910 * 1,500 = 8,865
      *       9,5 Horas Extras = R$ 84,217 --> arredondado: R4 84,22
      *    Neste exemplo específico, se for utilizado somente 2 casas
      *    decimais, o trabalhador sai perdendo 0,05 centavos, o que não 
      *    é bom para ele. Sistemas profissionais podem parametrizar esta 
      *    situação para atender políticas de Recursos Humanos das 
      *    empresas. 
      *    No setor financeiro é comum inserir cálculos intermediários 
      *    com mais casas decimais - 6 casas por exemplo - para cálculos
      *    mais apurados de juros, variação cambial, e apurações
      *    financeiras diversas.
      *
      * Parte 1) Apuração do Salário Bruto
           01 VLR-SALARIO-MENSAL       PIC 9(6)V999.
           01 VLR-HE-50PC              PIC 9(3)V999.
           01 VLR-PGTO-HE-50           PIC 9(6)V999.
           01 VLR-HE-100PC             PIC 9(3)V999.
           01 VLR-PGTO-HE-100          PIC 9(6)V999.
           01 VLR-SALARIO-FAMILIA      PIC 9(6)V999.
      * Este é o salário sobre o qual será calculado o desconto do
      * INSS.
           01 VLR-SALARIO-BRUTO        PIC 9(6)V999.
      *
      * Parte 2) Apuração do INSS a recolher
      * O valor total do INSS a recolher será armazenado aqui:
           01 VLR-TOTAL-INSS-RECOLHER   PIC 9(6)V999.
           01 ALIQUOTA-EFETIVA-INSS-REC   PIC 9(3)V9(6).

      * Esta é a tabela auxiliar do cálculo do INSS a recolher
           01 TABELA-APURACAO-INSS-RECOLHER.
              03 IDX-INSS-REC          PIC 9(4) COMP.
              03 TAB-INSS-REC-FAIXAS   OCCURS 4 TIMES.
                 05 VLR-BASE-CALCULO   PIC 9(6)V9(3).
                 05 VLR-INSS-RECOLHER  PIC 9(6)V9(3).
                 05 VLR-RESIDUAL-FAIXA PIC 9(6)V9(3).
              03 VLR-RESIDUAL          PIC 9(6)V9(3).
              03 SOMA-BASES-CALCULO-INSS PIC 9(6)V9(3).

      * Parte 3) Deduzir dependentes da Base de C?lculo
           01 VLR-DEDUCAO-DEPEND    PIC 9(6)V9(2).
           01 VLR-BASE-CALCULO-IR        PIC 9(6)V9(2).
           01 VLR-IRRF-RECOLHER          PIC 9(6)V9(2).
           01 IDX-FT                     PIC 9(4) COMP.

      * Parte 4) Apura Salário Líquido
           01 VLR-SALARIO-LIQUIDO        PIC 9(6)V99.
      *
      ******************************************************************
      * Área de Tabelas de Cálculos
      ******************************************************************
      * Tabelas INSS 2021 e Salário-Família 2021
      * Fontes consultadas:(08-02-2021):
      * https://www.contabilizei.com.br/contabilidade-online/desconto-inss/
      * https://www.contabilidadescalabrini.com.br/noticias/como-calcular-contribuicao-previdenciaria-inss-janeiro-2021/
           01 TABELA-INSS.
             02 IDX-INSS                    PIC 9(4) COMP VALUE 0.
             02 IDX-INSS-ANT                PIC 9(4) COMP VALUE 0.
             02 TAB-INSS-FAIXAS.
              03 INSS-FAIXA1.
                 05 SAL-CONTRIB-INSS-F1     PIC 9(6)V99  VALUE 1100,00.
                 05 ALIQUOTA-INSS-F1        PIC 9(2)V999 VALUE 0,075.
              03 INSS-FAIXA2.
                 05 SAL-CONTRIB-INSS-F2     PIC 9(6)V99  VALUE 2203,48.
                 05 ALIQUOTA-INSS-F2        PIC 9(2)V999 VALUE 0,090.
              03 INSS-FAIXA3.
                 05 SAL-CONTRIB-INSS-F3     PIC 9(6)V99  VALUE 3305,22.
                 05 ALIQUOTA-INSS-F3        PIC 9(2)V999 VALUE 0,120.
              03 INSS-FAIXA4.
                 05 SAL-CONTRIB-INSS-F4     PIC 9(6)V99  VALUE 6433,57.
                 05 ALIQUOTA-INSS-F4        PIC 9(2)V999 VALUE 0,140.
            02 TAB-INSS REDEFINES TAB-INSS-FAIXAS
               OCCURS 4 TIMES.
               03 INSS-FAIXA.
                  05 VLR-TETO-INSS-FAIXA    PIC 9(6)V99.
                  05 ALIQUOTA-INSS-FAIXA    PIC 9(2)V999.

      *    Tabela do Salário familia
           01 TAB-SALARIO-FAMILIA.
              03 VLR-SF-TETO           PIC 9(6)V99 VALUE 1503,25.
              03 VLR-SF-DEPENDENTE     PIC 9(6)V99 VALUE 52,17.


      *    Tabela IRPF
      * Fonte consultada: Receita Federal - Fevereiro/2021
      * https://receita.economia.gov.br/acesso-rapido/tributos/irpf-imposto-de-renda-pessoa-fisica-old
           01 TABELA-IRPF.
             02 IDX-IRPF               PIC 9(4) COMP VALUE 0.
             02 TAB-IRPF-FAIXAS.
              03 IRPF-FAIXA1.
                 05 VLR-TETO-FAIXA1    PIC 9(6)V99 VALUE 1903,98.
                 05 ALIQUOTA-FAIXA1    PIC 9(2)V999 VALUE ZEROS.
                 05 VLR-DEDUZIR-FAIXA1 PIC 9(6)V99 VALUE ZEROS.
              03 IRPF-FAIXA2.
                 05 VLR-TETO-FAIXA2    PIC 9(6)V99 VALUE 2826,65.
                 05 ALIQUOTA-FAIXA2    PIC 9(2)V999 VALUE 0,075.
                 05 VLR-DEDUZIR-FAIXA2 PIC 9(6)V99 VALUE 142,80.
              03 IRPF-FAIXA3.
                 05 VLR-TETO-FAIXA3    PIC 9(6)V99 VALUE 3751,05.
                 05 ALIQUOTA-FAIXA3    PIC 9(2)V999 VALUE 0,15.
                 05 VLR-DEDUZIR-FAIXA3 PIC 9(6)V99 VALUE 354,80.
              03 IRPF-FAIXA4.
                 05 VLR-TETO-FAIXA4    PIC 9(6)V99 VALUE 4664,68.
                 05 ALIQUOTA-FAIXA4    PIC 9(2)V999 VALUE 0,22.
                 05 VLR-DEDUZIR-FAIXA4 PIC 9(6)V99 VALUE 636,13.
              03 IRPF-FAIXA5.
                 05 VLR-TETO-FAIXA5    PIC 9(6)V99 VALUE 999999,99.
                 05 ALIQUOTA-FAIXA5    PIC 9(2)V999 VALUE 0,275.
                 05 VLR-DEDUZIR-FAIXA5 PIC 9(6)V99 VALUE 869,36.
            02 TAB-IRPF REDEFINES TAB-IRPF-FAIXAS
               OCCURS 5 TIMES.
               03 IRPF-FAIXA.
                  05 VLR-TETO-FAIXA    PIC 9(6)V99.
                  05 ALIQUOTA-FAIXA    PIC 9(2)V999.
                  05 VLR-DEDUZIR-FAIXA PIC 9(6)V99.
           01 VLR-DEDUZIR-DEPENDENTES  PIC 9(6)V99 VALUE 189,59.

       01 WS-FPGRIRRF PIC X(8) VALUE 'FPGRIRRF'.
       01 WS-FPGRIRRF-AREA.
            COPY FPGCIRRF. 
      ******************************************************************
      * Area de Relatorios
      ******************************************************************
           01 RELATORIO-FOLHA.
              COPY RELATORIOFOLHA.

      ******************************************************************
      * OBSERVAÇÃO - ANÁLISE
      * É muito importante usar uma planilha eletrônica
      * (Excel ou Libreoffice Calc) para um acompanhamento apurado
      * dos cálculos.
      * Normalmente o desenvolvedor/analista trabalha em conjunto com o
      * usuário/gestor do sistema para apurar estas questões. Em última
      * análise é o Gestor quem define estas regras. E a planilha
      * eletrônica é a melhor ferramenta para apução de cálculos.
      * (Pode parecer obvio, mas preciso registrar esta informação para
      * aqueles que estão estudando programação pela primeira vez ou 
      * ainda não possuem experiência em sistemas mais complexos.)
      ******************************************************************
       PROCEDURE                       DIVISION.
      ******************************************************************
       000000-ROTINA-PRINCIPAL.
           PERFORM 100000-INICIO

           PERFORM 200000-PROCESSA UNTIL FIM-APONTAMENTO

           PERFORM 300000-FINALIZA

      * Teste
            CALL WS-FPGRIRRF USING WS-FPGRIRRF-AREA
            
           GOBACK.
      *----------------------------------------------------------------*
       100000-INICIO                   SECTION.
      *----------------------------------------------------------------*
           DISPLAY '000 - INICIO PROGRAMA FOLHAPAGAMENTO V.' NUM-VERSAO
           ACCEPT DATA-CORRENTE FROM DATE YYYYMMDD
           ACCEPT HORA-CORRENTE FROM TIME
           DISPLAY '000 - DATA: ' DATA-CORRENTE ' HORA: ' HORA-CORRENTE
           DISPLAY '*-------------------------------------------------*'

      *    Move data e hora corrente para cabeçalho do relatório
           MOVE '01/01/0001' TO TX-DATA
           MOVE '00:00:00'   TO TX-HORA
           MOVE DATA-CORRENTE-DIA      TO TX-DATA-DIA
           MOVE DATA-CORRENTE-MES      TO TX-DATA-MES
           MOVE DATA-CORRENTE-ANO      TO TX-DATA-ANO
           MOVE HORA-CORRENTE-HH       TO TX-HORA-HH
           MOVE HORA-CORRENTE-MM       TO TX-HORA-MM
           MOVE HORA-CORRENTE-SS       TO TX-HORA-SS

           OPEN INPUT  APONTAMENTOS
           IF FS-APONTAMENTOS GREATER ZEROS
              MOVE 'APONTAMENTOS'      TO WS-NOME-ARQUIVO
              MOVE 001                 TO WS-LOCAL-ERRO
              PERFORM 999001-ERRO-I-O
           END-IF
           .

           OPEN OUTPUT RELATORIOFOLHA
           IF FS-RELATORIOFOLHA GREATER 0
              MOVE 'RELATORIOFOLHA'    TO WS-NOME-ARQUIVO
              MOVE 002                 TO WS-LOCAL-ERRO
              PERFORM 999001-ERRO-I-O
           END-IF
           .
           OPEN OUTPUT RELATORIOREJ
           IF FS-RELATORIOREJ GREATER 0
              MOVE 'RELATORIOREJ'    TO WS-NOME-ARQUIVO
              MOVE 002                 TO WS-LOCAL-ERRO
              PERFORM 999001-ERRO-I-O
           END-IF

      *    Após abrir o arquivo de entrada, leia o primeiro registro
           PERFORM 500000-READ-APONTAMENTOS
      *    Se arquivo de entrada vazio, imprima relatório com esta 
      *    informação (somente cabeçalho, e aviso)
           IF WS-FILE-STATUS EQUAL 10 
              PERFORM 200920-IMPRIME-RELATORIO-VAZIO
           END-IF 
           .
      *----------------------------------------------------------------*
       200000-PROCESSA                 SECTION.
      *----------------------------------------------------------------*
      * Inicializa variáveis de trabalho para cada folha processada
           PERFORM 200100-CALCULA-SALARIO-BRUTO
           PERFORM 200200-CALCULA-INSS
           PERFORM 200300-CALCULA-DEDUCOES-IRRF
           PERFORM 200400-CALCULA-IRRF
           PERFORM 200500-CALCULA-SALARIO-LIQUIDO
      *
           PERFORM 200900-IMPRIME-RELATORIO
      *
           PERFORM 500000-READ-APONTAMENTOS
           .
      *----------------------------------------------------------------*
       200100-CALCULA-SALARIO-BRUTO  SECTION.
      *----------------------------------------------------------------*
           MOVE ZEROS TO     VLR-SALARIO-MENSAL
                             VLR-HE-50PC  VLR-PGTO-HE-50
                             VLR-HE-100PC VLR-PGTO-HE-100
                             VLR-SALARIO-FAMILIA
                             VLR-SALARIO-BRUTO

           COMPUTE VLR-SALARIO-MENSAL = VLR-REMUNERACAO-HORA  *
                                        WS-QTD-HORAS-NORMAIS
      *
           COMPUTE  VLR-HE-50PC  = VLR-REMUNERACAO-HORA * 1,5
           COMPUTE  VLR-HE-100PC = VLR-REMUNERACAO-HORA * 2

           COMPUTE  VLR-PGTO-HE-50 = VLR-HE-50PC *
                                     QTD-HORAS-EXTRAS-50PC

           COMPUTE VLR-PGTO-HE-100 = VLR-HE-100PC *
                                     QTD-HORAS-EXTRAS-100PC

      * Cálculo Salário Família: Limite determinado por tabela
           IF VLR-SALARIO-MENSAL NOT GREATER VLR-SF-TETO
              COMPUTE VLR-SALARIO-FAMILIA = VLR-SF-DEPENDENTE *
                                            QTD-FILHOS-SF
           END-IF

      * Apuração do salário bruto (ou Total de Proventos)
           COMPUTE VLR-SALARIO-BRUTO = VLR-SALARIO-MENSAL +
                                       VLR-PGTO-HE-50 +
                                       VLR-PGTO-HE-100 +
                                       VLR-SALARIO-FAMILIA

           .
      *----------------------------------------------------------------*
       200200-CALCULA-INSS             SECTION.
      *----------------------------------------------------------------*
           MOVE 'N' TO IND-FIM-CALC-INSS
           MOVE 0   TO VLR-TOTAL-INSS-RECOLHER
                       SOMA-BASES-CALCULO-INSS
           MOVE VLR-SALARIO-BRUTO TO VLR-RESIDUAL
      * IMPORTANTE: Sempre inicializar tabelas auxiliares !!
           PERFORM VARYING IDX-INSS-REC FROM 1 BY 1
             UNTIL IDX-INSS-REC GREATER 4
             MOVE ZEROS TO
                     VLR-BASE-CALCULO(IDX-INSS-REC)
                     VLR-INSS-RECOLHER(IDX-INSS-REC)
                     VLR-RESIDUAL-FAIXA(IDX-INSS-REC)
           END-PERFORM
      *
      * Poderia utilizar apenas um indexador (IDX-INSS) para as 2
      * tabelas?
      * R: Poderia mas não é uma boa prática de programação.
      *    O correto é cada tabela ter seu próprio indexador, ainda que
      *    pela lógica as duas "andem" no mesmo ponteiro.
           PERFORM VARYING IDX-INSS FROM 1 BY 1
             UNTIL IDX-INSS GREATER 4
                OR FIM-CALC-INSS
             SET IDX-INSS-REC TO IDX-INSS
      * Base de Cálculo da Faixa
             EVALUATE IDX-INSS
                 WHEN 1
                      IF VLR-SALARIO-BRUTO >
                         VLR-TETO-INSS-FAIXA(IDX-INSS)
                         MOVE VLR-TETO-INSS-FAIXA(IDX-INSS)
                           TO VLR-BASE-CALCULO(IDX-INSS-REC)
                      ELSE
                         MOVE VLR-SALARIO-BRUTO
                           TO VLR-BASE-CALCULO(IDX-INSS-REC)
                      END-IF

                 WHEN OTHER
                      IF VLR-SALARIO-BRUTO >
                         VLR-TETO-INSS-FAIXA(IDX-INSS)
                         COMPUTE IDX-INSS-ANT = IDX-INSS - 1
                         COMPUTE VLR-BASE-CALCULO(IDX-INSS) =
                                VLR-TETO-INSS-FAIXA(IDX-INSS) -
                                VLR-TETO-INSS-FAIXA(IDX-INSS-ANT)
                      ELSE
                         MOVE VLR-RESIDUAL TO VLR-BASE-CALCULO(IDX-INSS)
                      END-IF
             END-EVALUATE

      * INSS a recolher da Faixa
             COMPUTE VLR-INSS-RECOLHER(IDX-INSS-REC) =
                     VLR-BASE-CALCULO(IDX-INSS-REC) *
                     ALIQUOTA-INSS-FAIXA(IDX-INSS)

      * Calcula saldo a tributar na próxima faixa
      *      COMPUTE VLR-RESIDUAL = VLR-RESIDUAL -
      *                              VLR-BASE-CALCULO(IDX-INSS-REC)
      * Instrução COBOL equivalente (COBOL "raiz");
             SUBTRACT VLR-BASE-CALCULO(IDX-INSS-REC) FROM VLR-RESIDUAL

      * Acumula INSS a recolher
             ADD VLR-INSS-RECOLHER(IDX-INSS-REC)
              TO VLR-TOTAL-INSS-RECOLHER

      * Dica de performance:
      * A instrução COMPUTE gasta mais instruções em linguagem C
      * do que a instrução ADD equivalente acima.
      * Isto acontece também no Mainframe, onde o programa COBOL
      * é precompilado para ASSEMBLER .
      *        COMPUTE VLR-TOTAL-INSS-RECOLHER = 
      *                VLR-TOTAL-INSS-RECOLHER +
      *                VLR-INSS-RECOLHER(IDX-INSS-REC)

      * Acumula bases de calculo
             ADD VLR-BASE-CALCULO(IDX-INSS-REC)
              TO SOMA-BASES-CALCULO-INSS
      * Armazena valor residual da faixa para fins de "debug" (ou LOG)
             MOVE VLR-RESIDUAL TO VLR-RESIDUAL-FAIXA(IDX-INSS-REC)

      * Os calculos se encerram quando n?o sobrar valor residual
             IF VLR-RESIDUAL EQUAL ZEROS
                SET FIM-CALC-INSS TO TRUE
             END-IF

           END-PERFORM

      * Calcula alíquota média INSS a recolher
           COMPUTE ALIQUOTA-EFETIVA-INSS-REC = VLR-TOTAL-INSS-RECOLHER
                                               / SOMA-BASES-CALCULO-INSS

           .
      *----------------------------------------------------------------*
       200300-CALCULA-DEDUCOES-IRRF    SECTION.
      *----------------------------------------------------------------*
           MOVE ZEROS TO VLR-BASE-CALCULO-IR
      *    a) Descontar o INSS a recolher
           COMPUTE VLR-BASE-CALCULO-IR = VLR-SALARIO-BRUTO -
                                         VLR-TOTAL-INSS-RECOLHER

      *    b) Deduções de dependentes
           COMPUTE VLR-DEDUCAO-DEPEND  = QTD-DEPENDENTES-IRPF *
                                         VLR-DEDUZIR-DEPENDENTES

           COMPUTE VLR-BASE-CALCULO-IR = VLR-BASE-CALCULO-IR -
                                         VLR-DEDUCAO-DEPEND

      *    c) Deduzir Pensao Alimenticia
           COMPUTE VLR-BASE-CALCULO-IR = VLR-BASE-CALCULO-IR -
                                       VLR-PENSAO-ALIMENTICIA

           .
      *----------------------------------------------------------------*
       200400-CALCULA-IRRF             SECTION.
      *----------------------------------------------------------------*
           MOVE 0   TO VLR-IRRF-RECOLHER
           MOVE 'N' TO IND-FIM-CALC-IRRF
      *
           PERFORM VARYING IDX-IRPF FROM 1 BY 1
             UNTIL IDX-IRPF GREATER 5
                OR FIM-CALC-IRRF
                EVALUATE IDX-IRPF
                    WHEN 1
                         IF VLR-BASE-CALCULO-IR <=
                            VLR-TETO-FAIXA(IDX-IRPF)
                            COMPUTE VLR-IRRF-RECOLHER =
                                    VLR-BASE-CALCULO-IR *
                                    ALIQUOTA-FAIXA (IDX-IRPF)
                            SET FIM-CALC-IRRF TO TRUE
                    WHEN OTHER
                         IF VLR-BASE-CALCULO-IR >
                            VLR-TETO-FAIXA(IDX-IRPF - 1)
                            AND
                            VLR-BASE-CALCULO-IR <=
                            VLR-TETO-FAIXA(IDX-IRPF)
                            COMPUTE VLR-IRRF-RECOLHER =
                                    VLR-BASE-CALCULO-IR *
                                    ALIQUOTA-FAIXA (IDX-IRPF)
                            SET FIM-CALC-IRRF TO TRUE
                END-EVALUATE
                SET IDX-FT TO IDX-IRPF
           END-PERFORM

      *    Valor a deduzir do imposto conforme tabela
           COMPUTE VLR-IRRF-RECOLHER = VLR-IRRF-RECOLHER -
                                       VLR-DEDUZIR-FAIXA(IDX-FT)


           .
      *----------------------------------------------------------------*
       200500-CALCULA-SALARIO-LIQUIDO  SECTION.
      *----------------------------------------------------------------*
           COMPUTE VLR-SALARIO-LIQUIDO = VLR-SALARIO-BRUTO -
                                         VLR-TOTAL-INSS-RECOLHER -
                                         VLR-IRRF-RECOLHER -
                                         VLR-PENSAO-ALIMENTICIA
           .

      *----------------------------------------------------------------*
       200900-IMPRIME-RELATORIO        SECTION.
      *----------------------------------------------------------------*
           IF CNT-LINHA > 50
              PERFORM 200910-IMPRIME-CABECALHO
           END-IF
           MOVE NR-MATRICULA           TO REL-NR-MATRICULA
           MOVE NOME-EMPREGADO         TO REL-NOME-EMPREGADO
      *     MOVE FUNCTION NATIONAL-OF(NOME-EMPREGADO,00819)  
      *                                 TO REL-NOME-EMPREGADO

           MOVE QTD-HORAS-NORMAIS      TO REL-QTD-HORAS-TRABALHADAS
           MOVE VLR-REMUNERACAO-HORA   TO REL-VLR-REMUNERACAO-HORA

           MOVE VLR-HE-50PC            TO REL-VLR-HE-50
           MOVE QTD-HORAS-EXTRAS-50PC  TO REL-QTD-HE-50
           MOVE VLR-PGTO-HE-50         TO REL-PGTO-HE-50

           MOVE VLR-HE-100PC           TO REL-VLR-HE-100
           MOVE QTD-HORAS-EXTRAS-100PC TO REL-QTD-HE-100
           MOVE VLR-PGTO-HE-100        TO REL-PGTO-HE-100

           MOVE VLR-SALARIO-FAMILIA    TO REL-VLR-SAL-FAMILIA

           MOVE VLR-SALARIO-BRUTO      TO REL-VLR-SALARIO-BRUTO

           MOVE VLR-IRRF-RECOLHER      TO REL-VLR-IRPF
           MOVE VLR-TOTAL-INSS-RECOLHER
                                       TO REL-VLR-INSS
           MOVE VLR-PENSAO-ALIMENTICIA TO REL-VLR-PENSAO-AL

           MOVE VLR-SALARIO-LIQUIDO    TO REL-VLR-SALARIO-LIQUIDO

           WRITE RELATORIO-FOLHA-FD FROM LINHA-DADOS
      *
           ADD 1 TO CNT-LINHA
           .
      *----------------------------------------------------------------*
       200910-IMPRIME-CABECALHO        SECTION.
      *----------------------------------------------------------------*
           ADD 1 TO CNT-PAGINA

      * ATENCAO :
      * TO-DO: Ler 'mes de apuração' do arquivo de entrada, ou
      *        via parametro (neste caso implementar LINKAGE).
           MOVE ' FEV/2021 ' TO TX-MES-APURACAO


           MOVE CNT-PAGINA TO REL-NUM-PAGINA

           WRITE RELATORIO-FOLHA-FD FROM CAB-LINHA-1
           WRITE RELATORIO-FOLHA-FD FROM CAB-LINHA-2
           WRITE RELATORIO-FOLHA-FD FROM CAB-LINHA-3
           WRITE RELATORIO-FOLHA-FD FROM LINHA-TITULOS
           MOVE 5 TO CNT-LINHA
           .
      *----------------------------------------------------------------*
       200920-IMPRIME-RELATORIO-VAZIO  SECTION.
      *----------------------------------------------------------------*
            PERFORM 200910-IMPRIME-CABECALHO
            MOVE SPACES TO LINHA-DADOS
            WRITE RELATORIO-FOLHA-FD FROM LINHA-DADOS
            WRITE RELATORIO-FOLHA-FD FROM LINHA-DADOS
            WRITE RELATORIO-FOLHA-FD FROM LINHA-DADOS
            STRING '     *** ARQUIVO DE APONTAMENTOS VAZIO. NÃO HOUVE '
                   'PROCESSAMENTO. ***'
            DELIMITED BY SIZE
            INTO LINHA-DADOS  
            WRITE RELATORIO-FOLHA-FD FROM LINHA-DADOS
            .
      *----------------------------------------------------------------*
       300000-FINALIZA                 SECTION.
      *----------------------------------------------------------------*
      * Esquecer de fechar os arquivos não dê erro, mas dê mensagem de
      * alerta (warning)
           CLOSE APONTAMENTOS RELATORIOFOLHA RELATORIOREJ

           DISPLAY '999 - TERMINO PROGRAMA FOLHAPAGAMENTO V.'
                   NUM-VERSAO
           ACCEPT DATA-CORRENTE FROM DATE YYYYMMDD
           ACCEPT HORA-CORRENTE FROM TIME
           DISPLAY '999 - DATA: ' DATA-CORRENTE ' HORA: ' HORA-CORRENTE
           DISPLAY '*-------------------------------------------------*'

           .

      *----------------------------------------------------------------*
       500000-READ-APONTAMENTOS        SECTION.
      *----------------------------------------------------------------*
           READ APONTAMENTOS INTO APONTAMENTO-FD
             AT  END
                 MOVE 'S' TO IND-FIM-APONTAMENTO
             NOT AT END
                 ADD 1 TO CNT-APONTAMENTOS
           END-READ
      *
           IF FS-APONTAMENTOS GREATER ZEROS AND NOT LESS 10
              DISPLAY '*------------------------------------------*'
              DISPLAY '777 ALERTA - READ APONTAMENTOS FS='
                      FS-APONTAMENTOS
              DISPLAY '777 QTD.LIDOS = ' CNT-APONTAMENTOS
              DISPLAY '*------------------------------------------*'
           ELSE
              IF FS-APONTAMENTOS GREATER 10
                 MOVE 'APONTAMENTOS'      TO WS-NOME-ARQUIVO
                 MOVE 004                 TO WS-LOCAL-ERRO
                 PERFORM 999001-ERRO-I-O
              END-IF
           END-IF

           .
      *
      ******************************************************************
       999000-ERROS                    SECTION.
      ******************************************************************
       999001-ERRO-I-O.
           COPY FSCODES REPLACING STATUS BY WS-FILE-STATUS
                                  MSG    BY WS-MSG-ERRO
           .
           DISPLAY '888 - ERRO I-O...: ' WS-NOME-ARQUIVO
           DISPLAY '888 - LOCAL......: ' WS-LOCAL-ERRO
           DISPLAY '888 - FILE STATUS: ' WS-FILE-STATUS
                                     ' ' WS-MSG-ERRO
           DISPLAY '888 - QTDE.LIDOS : ' CNT-APONTAMENTOS
           PERFORM 999999-ABEND.

       999999-ABEND.
           DISPLAY '888 - ERRO PROGRAMA FOLHAPAGAMENTO V.'
                   NUM-VERSAO
           ACCEPT DATA-CORRENTE FROM DATE YYYYMMDD
           ACCEPT HORA-CORRENTE FROM TIME
           DISPLAY '888 - DATA: ' DATA-CORRENTE ' HORA: ' HORA-CORRENTE
           DISPLAY '*-------------------------------------------------*'
      *
           STOP RUN.
      *
      ******************************************************************
       END PROGRAM FOLHAPAGAMENTO7.
      ******************************************************************
