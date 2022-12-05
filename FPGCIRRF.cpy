000000* SISTEMA: FOPAG
      * COPYBOOK: FPGCIRRF - Área de comunicação da Subrotina FPGRIRRF
      * VRS001 - SET/2021 - Implantação
      *
      * 01 IRRF-COMMAREA.
            03 IRRF-DADOS-ENTRADA.
               05 IRRF-VLR-BASE-CALCULO-IR  PIC 9(6)V9(2).
               05 IRRF-VLR-DEDUCAO-DEPEND   PIC 9(6)V9(2).
            03 IRRF-DADOS-RETORNO.
               05 IRRF-VLR-IRRF-RECOLHER    PIC 9(6)V9(2).
            03 IRRF-CONTROLE.
               05 IRRF-RETURN-CODE          PIC 99.
               05 IRRF-MENSAGEM-ERRO        PIC X(50).
000000* FIM COPYBOOK FPGCIRRF