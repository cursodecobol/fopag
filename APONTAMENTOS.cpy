      ****************************************************************
      * COPYBOOK: APONTAMENTOS.cpy
      * AUTOR   : www.cursodecobol.com.br
      * SISTEMA : FOPAG
      * OBJETIVO: Layout do arquivo APONTAMENTOS.dat
      * Tamanho do registro: 68 bytes
      * Origem dos dados: Sistema externo de Apontamentos (folha de
      *                   ponto).
      * - A Legisla��o CLT � complexa. Refor�o que este � um
      *   sistema did�tico para fins de ensino da linguagem COBOL.
      *   A Legisla��o mais atual pode n�o estar contemplada 
      *   neste sistema.
      ****************************************************************
      *01 FD-APONTAMENTO.                                               INI FIM TAM
          03 NR-MATRICULA              PIC X(06).                       001 006 6
          03 NOME-EMPREGADO            PIC X(30).                       007 036 30
          03 VLR-REMUNERACAO-HORA      PIC 9(3)V99.                     037 041 5
          03 QTD-HORAS-NORMAIS         PIC 9(3)V99.                     042 046 5
          03 QTD-HORAS-EXTRAS-50PC     PIC 9(3)V99.                     047 051 5
          03 QTD-HORAS-EXTRAS-100PC    PIC 9(3)V99.                     052 056 5
          03 QTD-DEPENDENTES-IRPF      PIC 9(2).                        057 058 2
          03 QTD-FILHOS-SF             PIC 9(2).                        059 060 2
          03 VLR-PENSAO-ALIMENTICIA    PIC 9(6)V99.                     061 068 8
      *    03 TESTE-VLR                 PIC 9V9(16).
      *    03 TESTE-VLR2                PIC V9(17).
      ****************************************************************
      * Descri��o dos campos
      * --------------------
      * NR-MATRICULA               N�mero da matricula do empregado
      * NOME-EMPREGADO             Nome completo do empregado
      * VLR-REMUNERACAO-HORA       Valor da remuneracao por hora, com
      *                            base em 220 horas. Divida o salario
      *                            mensal por 220 e informe neste campo.
      * QTD-HORAS-NORMAIS          Quantidade de horas trabalhadas no
      *                            periodo pelo empregado - DECIMAIS.
      *                            Obs: Para a CLT, a quantidade de
      *                            horas normais no m�s � 220. O
      *                            excedente s�o Horas Extras. Este
      *                            campo ser� informado, para permitir
      *                            futuramente o calculo de horas do
      *                            trabalhador pessoa jur�dica (PJ).
      * QTD-HORAS-EXTRAS-50PC      Quantidade de horas extras realizadas
      *                            com direito ao acr�scimo de 50%.
      * QTD-HORAS-EXTRAS-100PC     Quantidade de horas extras realizadas
      *                            com direito ao acr�scimo de 100%.
      * QTD-DEPENDENTES-IRPF       Quantidade de dependentes para fins
      *                            do Imposto de Renda
      * QTD-FILHOS-SF              Quantidade de filhos para fins de
      *                            c�lculo do Sal�rio-Fam�lia
      * VLR-PENSAO-ALIMENTICIA     Valor pago a t�tulo de Pens�o
      *                            Aliment�cia; � um Desconto, e tamb�m
      *                            � dedut�vel do IRRF.
      *                            Informar valor total em R$
      *
      * - IMPORTANTE: Informar Horas normais e/ou extras em formato
      *               DECIMAL.
      *               Exemplo: 8 horas e 23 minutos -> 8,38
      *                        9 horas e 40 minutos -> 9,67
      *               (Pesquise no Google por "convers�o hora decimal"
      *               ou "Como eu converto horas convencionais em um
      *               n�mero decimal?"
      ******************************************************************
