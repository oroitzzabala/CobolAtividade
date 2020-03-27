      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. HBSI20AO.
       AUTHOR.     TADEU COSTA DE OLIVEIRA.
      *================================================================*
      *           A V A L I A C A O - H B S I S / A M B E V            *
      *----------------------------------------------------------------*
      *    PROGRAMA....: HBSI20AO                                      *
      *    PROGRAMADOR.: TADEU COSTA DE OLIVEIRA                       *
      *    DATA........: 25/03/2019                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....: PROGRAMA PARA CADASTRO DE CLIENTES            *                                                                          
      *----------------------------------------------------------------*
      *    BOOKS USADOS:                                               *
      *    HBSINXXX - DESCRICAO DO BOOK                                *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    HBSINXXX - DESCRICAO DO MODULO                              *
      *================================================================*
      *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*
      *                                                                *
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
      *
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.
      *
      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*
      *
       FILE-CONTROL.
      *
              SELECT ARQCLI01 ASSIGN   TO UT-S-ARQCLI01
                        ORGANIZATION   IS INDEXED
                         ACCESS MODE   IS DYNAMIC
                          RECORD KEY   IS FS-COD-CLI
                       ALTERNATE KEY   IS FS-CNPJ-CLI
                       ALTERNATE KEY   IS FS-RAZ-SOCI-CLI               
                                       WITH DUPLICATES
                           LOCK MODE   IS MANUAL
                         FILE STATUS   IS WRK-FS-ARQCLI01.
      *
              SELECT ARQIMPCL ASSIGN   TO UT-S-ARQIMPCL
                        ORGANIZATION   IS SEQUENTIAL
                         ACCESS MODE   IS SEQUENTIAL
                         FILE STATUS   IS WRK-FS-ARQIMPCL.
      
      *================================================================*
       DATA                            DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*
      *                                                                *
      *----------------------------------------------------------------*
      *    INPUT  : ARQUIVO ENTRADA - ARQUIVO DE CLIENTES              *
      *               ORG. DINAMICA - LRECL = 0083                     *
      *----------------------------------------------------------------*
      *
       FD ARQCLI01
          RECORD CONTAINS 83 CHARACTERS.
       01 FD-CLIENTE.
          05 FS-COD-CLI               PIC 9(007).
          05 FS-CNPJ-CLI              PIC 9(014).
          05 FS-RAZ-SOCI-CLI          PIC X(040).
          05 FS-LAT-CLI               PIC S9(003)V9(008).
          05 FS-LONG-CLI              PIC S9(003)V9(008).
      *
       FD ARQIMPCL
          RECORD CONTAINS 83 CHARACTERS.
       01 FD-IMP-CLIENTE.
          05 FS-IMP-COD-CLI            PIC 9(007).
          05 FS-IMP-CNPJ-CLI           PIC 9(014).
          05 FS-IMP-RAZ-SOCI-CLI       PIC X(040).
          05 FS-IMP-LAT-CLI            PIC S9(003)V9(008).
          05 FS-IMP-LONG-CLI           PIC S9(003)V9(008).
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)     VALUE
           ' HBSI20AO - INICIO DA AREA DE WORKING '.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       77  FILLER                      PIC X(050)      VALUE
           'AREA PARA VARIAVEIS AUXILIARES'.
      *----------------------------------------------------------------*
      *
       01  WRK-VAR-AUXILIARES.
           05 WRK-OPCAO                PIC X(002)      VALUE SPACES.
           05 WRK-SIM-NAO              PIC X(001)      VALUE SPACES.    
           05 WRK-COD-CLI-BUSC         PIC 9(007)      VALUE ZEROS.
           05 WRK-PROVA-CNPJ           PIC 9(014)      VALUE ZEROS.
           
       01  WRK-AREA-FS.
           05 WRK-FS-ARQCLI01          PIC X(002)      VALUE "00".
           05 WRK-FS-ARQIMPCL          PIC X(002)      VALUE "00".
      *
      *----------------------------------------------------------------*
       77 FILLER                       PIC  X(050)     VALUE
           'AREA PARA LAYOUT ENTRADA'.
      *----------------------------------------------------------------*
      *
       01  WRK-AREA-ARQCLI01.
           05 WRK-COD-CLI              PIC 9(007).
           05 WRK-CNPJ-CLI             PIC 9(014).
           05 WRK-RAZ-SOCI-CLI         PIC X(040).
           05 WRK-LAT-CLI              PIC S9(003)V9(008).
           05 WRK-LONG-CLI             PIC S9(003)V9(008).
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)     VALUE
           ' HBSI20AO - FIM DA AREA DE WORKING '.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       SCREEN SECTION.
      *----------------------------------------------------------------*
      *
       01  TELA-CLIENTE.
           05 VALUE "CADASTRO DE CLIENTES"             
                                       BLANK SCREEN      LINE  2 COL  2.
           05 VALUE "01 - INCLUIR"                       LINE  4 COL  2.
           05 VALUE "02 - ALTERAR"                       LINE  5 COL  2.
           05 VALUE "03 - EXCLUIR"                       LINE  6 COL  2.
           05 VALUE "04 - IMPORTAR"                      LINE  7 COL  2.
           05 VALUE "05 - RETORNAR AO MENU ANTERIOR"     LINE  8 COL  2.
           05 VALUE "DIGITE A OPCAO DESEJADA:"           LINE 10 COL  2.
           05 OPCAO                                      LINE 10 COL 27
                                       PIC X(002) TO WRK-OPCAO.
      *
       01  TELA-ADD-CLIENTE.
           05 VALUE "CADASTRO DE CLIENTES - INCLUSAO"
                                       BLANK SCREEN      LINE  2 COL  2.
           05 VALUE "CODIGO.......:"                     LINE  4 COL  2.
           05 CODIGO-CLI                                 LINE  4 COL 17
                                       PIC 9(007) TO WRK-COD-CLI.
           05 VALUE "CNPJ.........:"                     LINE  5 COL  2.
           05 CNPJ-CLI                                   LINE  5 COL 17
                                       PIC 9(014) TO WRK-CNPJ-CLI.
           05 VALUE "RAZAO SOCIAL.:"                     LINE  6 COL  2.
           05 RAZAO-CLI                                  LINE  6 COL 17
                                       PIC X(040) TO WRK-RAZ-SOCI-CLI.
           05 VALUE "LATITUDE.....:"                     LINE  7 COL  2.
           05 LATITUDE-CLI                               LINE  7 COL 17
                                       PIC S9(003)V9(008) TO
                                                            WRK-LAT-CLI.
           05 VALUE "LONGITUDE....:"                     LINE  8 COL  2.
           05 LONGITUDE-CLI                              LINE  8 COL 17
                                       PIC S9(003)V9(008) TO
                                                           WRK-LONG-CLI.
           05 VALUE "INCLUIR CLIENTE? (S/N):"            LINE 10 COL  2.
           05 CONFIRMA                                   LINE 10 COL 25
                                       PIC X TO WRK-SIM-NAO.
      *    
       01  TELA-ALT-CLIENTE.
           05 VALUE "CADASTRO DE CLIENTES - ALTERACAO"
                                       BLANK SCREEN      LINE  2 COL  2.
           05 VALUE "CODIGO.......:"                     LINE  4 COL  2.
           05 CODIGO-CLI                                 LINE  4 COL 17
                                       PIC 9(007) TO WRK-COD-CLI.
           05 VALUE "CNPJ.........:"                     LINE  5 COL  2.
           05 CNPJ-CLI                                   LINE  5 COL 17
                                       PIC 9(014) TO WRK-CNPJ-CLI.
           05 VALUE "RAZAO SOCIAL.:"                     LINE  6 COL  2.
           05 RAZAO-CLI                                  LINE  6 COL 17
                                       PIC X(040) TO WRK-RAZ-SOCI-CLI.
           05 VALUE "LATITUDE.....:"                     LINE  7 COL  2.
           05 LATITUDE-CLI                               LINE  7 COL 17
                                       PIC S9(003)V9(008) TO
                                                            WRK-LAT-CLI.
           05 VALUE "LONGITUDE....:"                     LINE  8 COL  2.
           05 LONGITUDE-CLI                              LINE  8 COL 17
                                       PIC S9(003)V9(008) TO
                                                           WRK-LONG-CLI.
           05 VALUE "INCLUIR CLIENTE? (S/N):"            LINE 10 COL  2.
           05 CONFIRMA                                   LINE 10 COL 25
                                       PIC X TO WRK-SIM-NAO.
      *    
       01  TELA-EXC-CLIENTE.
           05 VALUE "CADASTRO DE CLIENTES - EXCLUSAO"
                                       BLANK SCREEN      LINE  2 COL  2.
           05 VALUE "CODIGO.......:"                     LINE  4 COL  2.
           05 CODIGO-CLI                                 LINE  4 COL 17
                                       PIC 9(007) TO WRK-COD-CLI.
           05 VALUE "CNPJ.........:"                     LINE  5 COL  2.
           05 CNPJ-CLI                                   LINE  5 COL 17
                                       PIC 9(014) TO WRK-CNPJ-CLI.
           05 VALUE "RAZAO SOCIAL.:"                     LINE  6 COL  2.
           05 RAZAO-CLI                                  LINE  6 COL 17
                                       PIC X(040) TO WRK-RAZ-SOCI-CLI.
           05 VALUE "LATITUDE.....:"                     LINE  7 COL  2.
           05 LATITUDE-CLI                               LINE  7 COL 17
                                       PIC S9(003)V9(008) TO
                                                            WRK-LAT-CLI.
           05 VALUE "LONGITUDE....:"                     LINE  8 COL  2.
           05 LONGITUDE-CLI                              LINE  8 COL 17
                                       PIC S9(003)V9(008) TO
                                                           WRK-LONG-CLI.
           05 VALUE "INCLUIR CLIENTE? (S/N):"            LINE 10 COL  2.
           05 CONFIRMA                                   LINE 10 COL 25
                                       PIC X TO WRK-SIM-NAO.
      *    
       01  TELA-BUSCA-CLIENTE.
           05 VALUE "BUSCAR CLIENTE"   BLANK SCREEN      LINE  2 COL  2.
           05 VALUE "DIGITE CODIGO CLIENTE:"             LINE  6 COL  2.
           05 COD-BUSCA-CLI                              LINE  6 COL 24 
                                       PIC 9(007) TO WRK-COD-CLI-BUSC.
      *
       01  TELA-RESULT-BUSCA.
           05 VALUE "CLIENTE NAO ENCONTRADO"             
                                       BLANK SCREEN      LINE  6 COL  2.
           05 VALUE "REALIZAR NOVA BUSCA? (S/N):"        LINE  8 COL  2.
           05 BUSCA-NOVA                                 LINE  8 COL 29
                                       PIC X TO WRK-SIM-NAO.
      *
       01  TELA-CLI-IMPORTACAO.
      *
      *================================================================*
       PROCEDURE DIVISION.                               
      *================================================================*
      *
      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL DO PROGRAMA                                *
      *----------------------------------------------------------------*
       0000-PRINCIPAL                  SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 1000-INICIALIZAR
           PERFORM 2000-PROCESSAR
           PERFORM 3000-FINALIZAR
           .
      *
      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA INICIAL DO PROGRAMA                                  *
      *----------------------------------------------------------------*
       1000-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*
      *
           OPEN I-O ARQCLI01
           .
      *
      *----------------------------------------------------------------*
       1000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL DO PROGRAMA                         *
      *----------------------------------------------------------------*
       2000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 2010-MENU-CLIENTE.
      *
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA MOSTRAR MENU PRINCIPAL DO CLIENTE               *
      *----------------------------------------------------------------*
       2010-MENU-CLIENTE               SECTION.
      *----------------------------------------------------------------*
      *
           DISPLAY TELA-CLIENTE
           ACCEPT TELA-CLIENTE.
           EVALUATE WRK-OPCAO
              WHEN "01"
                  PERFORM 2100-MENU-INCLUI-CLI
              WHEN "02"
                  PERFORM 2200-MENU-ALTERA-CLI                              
              WHEN "03"
                  PERFORM 2300-EXCLUIR-CLI
              WHEN "04"
                  PERFORM 2400-IMPORTA-CLI
              WHEN "05"
                  PERFORM 3000-FINALIZAR
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       2010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA O MENU DE INCLUSAO DO CLIENTE                   *
      *----------------------------------------------------------------*
       2100-MENU-INCLUI-CLI            SECTION.
      *----------------------------------------------------------------*
      *
           DISPLAY TELA-ADD-CLIENTE
           ACCEPT TELA-ADD-CLIENTE
           EVALUATE FUNCTION UPPER-CASE(WRK-SIM-NAO)
               WHEN "S"
                   PERFORM 2110-INCLUIR-CLI
                   CLOSE ARQCLI01
                   PERFORM 2010-MENU-CLIENTE
               WHEN "N"
                   CLOSE ARQCLI01
                   PERFORM 2010-MENU-CLIENTE
               WHEN OTHER
                   CLOSE ARQCLI01
                   PERFORM 2010-MENU-CLIENTE
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA INCLUIR UM CLIENTE                              *
      *----------------------------------------------------------------*
       2110-INCLUIR-CLI                SECTION.
      *----------------------------------------------------------------*
      *
           MOVE WRK-AREA-ARQCLI01      TO FD-CLIENTE                    
           
           
           
           
           
           
           .
      *
      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------* 
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA O MENU DE ALTERACAO DO CLIENTE                  * 
      *----------------------------------------------------------------*
       2200-MENU-ALTERA-CLI            SECTION.
      *----------------------------------------------------------------*
      *
           OPEN I-O ARQCLI01
           DISPLAY TELA-BUSCA-CLIENTE
           ACCEPT TELA-BUSCA-CLIENTE
           MOVE WRK-COD-CLI            TO FS-COD-CLI
           READ ARQCLI01               RECORD INTO WRK-AREA-ARQCLI01
                  KEY IS               FS-COD-CLI
           IF WRK-FS-ARQCLI01 NOT EQUAL "00"
               DISPLAY TELA-RESULT-BUSCA
               ACCEPT TELA-RESULT-BUSCA
               EVALUATE FUNCTION UPPER-CASE(WRK-SIM-NAO)
                   WHEN "S"
                       CLOSE ARQCLI01
                       PERFORM 2200-MENU-ALTERA-CLI
                   WHEN "N"
                       CLOSE ARQCLI01
                       PERFORM 2010-MENU-CLIENTE
                   WHEN OTHER
                       CLOSE ARQCLI01
                       PERFORM 2010-MENU-CLIENTE
               END-EVALUATE
           ELSE
               DISPLAY TELA-ALT-CLIENTE
               ACCEPT TELA-ALT-CLIENTE
               EVALUATE FUNCTION UPPER-CASE(WRK-SIM-NAO)
                   WHEN "S"
                       PERFORM 2210-ALTERAR-CLI
                       CLOSE ARQCLI01
                       PERFORM 2010-MENU-CLIENTE
                   WHEN "N"
                       CLOSE ARQCLI01
                       PERFORM 2010-MENU-CLIENTE
                   WHEN OTHER
                       CLOSE ARQCLI01
                       PERFORM 2010-MENU-CLIENTE
               END-EVALUATE
           END-IF.
      *
      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA ALTERAR O CLIENTE                               *
      *----------------------------------------------------------------*
       2210-ALTERAR-CLI                SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE FD-CLIENTE
      *
           MOVE WRK-COD-CLI            TO FS-COD-CLI
           MOVE WRK-CNPJ-CLI           TO FS-CNPJ-CLI
           MOVE WRK-RAZ-SOCI-CLI       TO FS-RAZ-SOCI-CLI
           MOVE WRK-LAT-CLI            TO FS-LAT-CLI
           MOVE WRK-LONG-CLI           TO FS-LONG-CLI.
      *
      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA EXCLUIR UM CLIENTE                              *
      *----------------------------------------------------------------*
       2300-EXCLUIR-CLI                SECTION.
      *----------------------------------------------------------------*
      *
           .
      *
      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA IMPORTAR UM CLIENTE                             *
      *----------------------------------------------------------------*
       2400-IMPORTA-CLI                SECTION.
      *----------------------------------------------------------------*
      *
           .
      *
      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZAR PROCESSAMENTO                         *
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*
      *
           EXIT PROGRAM.

      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*