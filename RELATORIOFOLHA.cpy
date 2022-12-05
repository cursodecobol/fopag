      ****************************************************************
      * COPYBOOK: RELATORIOFOLHA.cpy
      * AUTOR   : www.cursodecobol.com.br
      * SISTEMA : FOPAG
      * OBJETIVO: Layout do relatório gerado após o processamento dos
      *           apontamentos recebidos no arquivo "APONTAMENTOS.dat"
      * Tamanho do registro: 132 bytes
      ****************************************************************
      *    01 RELATORIO-FOLHA.
            02 CABECALHO-FOLHA.
              03 CAB-LINHA-1.
                    07 TX-DATA PIC X(10).
                    07 FILLER REDEFINES TX-DATA.
                       09 TX-DATA-DIA PIC 9(02).
                       09 FILLER      PIC X.
                       09 TX-DATA-MES PIC 9(02).
                       09 FILLER      PIC X.
                       09 TX-DATA-ANO PIC 9(04).
                    07 FILLER  PIC X(30).
                    07 FILLER  PIC X(30) VALUE
                       'RELATORIO FOLHA PAGAMENTO'.
                    07 FILLER  PIC X(81).
                    07 FILLER  PIC X(05) VALUE 'PAG. '.
                    07 REL-NUM-PAGINA PIC ZZZ9.
      *
              03 CAB-LINHA-2.
                    07 TX-HORA PIC X(10).
                    07 FILLER REDEFINES TX-HORA.
                       09 TX-HORA-HH PIC 9(02).
                       09 FILLER PIC X.
                       09 TX-HORA-MM PIC 9(02).
                       09 FILLER PIC X.
                       09 TX-HORA-SS PIC 9(02).
                    07 FILLER  PIC X(30).
                    07 FILLER  PIC X(14) VALUE
                       'MES APURACAO: '.
                    07 TX-MES-APURACAO PIC X(15).
      *
              03 CAB-LINHA-3 PIC X(160) VALUE ALL '*'.
      *
              03 LINHA-TITULOS.
                 05 FILLER PIC X(06) VALUE 'MATRIC'.
                 05 FILLER PIC X.
                 05 FILLER PIC X(60) VALUE 'NOME EMPREGADO'.
                 05 FILLER PIC X.
                 05 FILLER PIC X(06) VALUE 'HR.NOR'.
                 05 FILLER PIC X.
                 05 FILLER PIC X(06) VALUE 'VLR.HR'.
                 05 FILLER PIC X.

                 05 FILLER PIC X(07) VALUE 'VL.HE50'.
                 05 FILLER PIC X.
                 05 FILLER PIC X(07) VALUE 'QT.HE50'.
                 05 FILLER PIC X.
                 05 FILLER PIC X(08) VALUE 'TOT.HE50'.
                 05 FILLER PIC X.

                 05 FILLER PIC X(07) VALUE 'VL.H100'.
                 05 FILLER PIC X.
                 05 FILLER PIC X(07) VALUE 'QT.H100'.
                 05 FILLER PIC X.
                 05 FILLER PIC X(08) VALUE 'TOT.H100'.
                 05 FILLER PIC X.

                 05 FILLER PIC X(07) VALUE 'SAL.FAM'.
                 05 FILLER PIC X.

                 05 FILLER PIC X(09) VALUE 'SAL.BRUTO'.
                 05 FILLER PIC XXXXXX.

                 05 FILLER PIC X(06) VALUE 'IRRF'.
                 05 FILLER PIC XX.
                 05 FILLER PIC X(04) VALUE 'INSS'.
                 05 FILLER PIC XXXX.
                 05 FILLER PIC X(06) VALUE 'PENSAO'.
                 05 FILLER PIC XX.

                 05 FILLER PIC X(11) VALUE 'SAL.LIQUIDO'.
      *
              03 LINHA-DADOS.
                 05 REL-NR-MATRICULA          PIC X(06).
                 05 FILLER                    PIC X(01).
      *          05 REL-NOME-EMPREGADO        PIC X(30).
                 05 REL-NOME-EMPREGADO        PIC N(30) USAGE NATIONAL.
                 05 FILLER                    PIC X.
                 05 REL-QTD-HORAS-TRABALHADAS PIC ZZ9,99.
                 05 FILLER                    PIC X.
                 05 REL-VLR-REMUNERACAO-HORA  PIC ZZ9,99.
                 05 FILLER                    PIC X(02).

                 05 REL-VLR-HE-50             PIC ZZ9,99.
                 05 FILLER                    PIC X(02).
                 05 REL-QTD-HE-50             PIC ZZ9,99.
                 05 FILLER                    PIC X.
                 05 REL-PGTO-HE-50            PIC Z.ZZ9,99.
                 05 FILLER                    PIC X(02).

                 05 REL-VLR-HE-100            PIC ZZ9,99.
                 05 FILLER                    PIC X(02).
                 05 REL-QTD-HE-100            PIC ZZ9,99.
                 05 FILLER                    PIC X.
                 05 REL-PGTO-HE-100           PIC Z.ZZ9,99.
                 05 FILLER                    PIC X.

                 05 REL-VLR-SAL-FAMILIA       PIC ZZZ9,99.
                 05 FILLER                    PIC X.

                 05 REL-VLR-SALARIO-BRUTO     PIC ZZ.ZZ9,99.
                 05 FILLER                    PIC X.

                 05 REL-VLR-IRPF              PIC ZZ.ZZ9,99.
                 05 FILLER                    PIC X.
                 05 REL-VLR-INSS              PIC ZZZ9,99.
                 05 FILLER                    PIC X.
                 05 REL-VLR-PENSAO-AL         PIC ZZ.ZZ9,99.
                 05 FILLER                    PIC XXXX.

                 05 REL-VLR-SALARIO-LIQUIDO   PIC ZZ.ZZ9,99.
      *
      ****************************************************************
      * FIM COPYBOOK RELATORIOFOLHA.cpy
      ****************************************************************
