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
           05 WRK-ARQ-IMP                PIC X(020)      VALUE SPACES.
           
       01  WRK-AREA-FS.
           05 WRK-FS-ARQIMPCL          PIC X(002)      VALUE "00".
           05 WRK-FS-ARQCLI01          PIC X(002)      VALUE "00".
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
       01 LKS-PARM.
           05 LKS-NUMERO-I               PIC 9(014).
           05 LKS-NUMERO-F               PIC 9(014).
           05 LKS-TIPO-CALCULO           PIC X(003).
           05 LKS-ACAO                   PIC X(001).
           05 LKS-RETORNO                PIC 9(001).    
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
       01  TELA-CLIENTE-EXISTE.
           05 VALUE "CLIENTE JA EXISTE" BLANK SCREEN     LINE  6 COL 10.
           05 VALUE "REALIZAR NOVO CADASTRO? (S/N)"      LINE  8 COL  2.
           05 BUSCA-NOVA                                 LINE  8 COL 31
                                       PIC X TO WRK-SIM-NAO.
      *
       01  TELA-CLI-IMPORTACAO.
           05 VALUE "CADASTRO DE CLIENTES - IMPORTACAO"  
                                            BLANK SCREEN LINE  2 COL  2.
           05 VALUE "NOME DO ARQUIVO DE IMPORTACAO:"     LINE  4 COL  2.
           05 ARQ-IMPORTACAO                             LINE  4 COL 32
                                       PIC X(020) TO WRK-ARQ-IMP.       
           05 VALUE "REALIZAR IMPORTACAO? (S/N)"         LINE  8 COL  2.
           05 BUSCA-NOVA                                 LINE  8 COL 28
                                       PIC X TO WRK-SIM-NAO.
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
           PERFORM 2000-PROCESSAR
           PERFORM 3000-FINALIZAR
           .
      *
      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
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
                  PERFORM 2300-MENU-EXCLUI-CLI
              WHEN "04"
                  PERFORM 2400-MENU-IMPORTA-CLI
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
           OPEN I-O ARQCLI01
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
           MOVE WRK-CNPJ-CLI           TO LKS-NUMERO-I
           MOVE WRK-AREA-ARQCLI01      TO FD-CLIENTE
           MOVE 'CGC'                  TO LKS-TIPO-CALCULO
           MOVE 'V'                    TO LKS-ACAO
           MOVE ZEROS                  TO LKS-RETORNO
           MOVE ZEROS                  TO LKS-NUMERO-F
           CALL "HBSI30AO" USING LKS-PARM
      *    
           EVALUATE LKS-RETORNO
               WHEN 0
                   WRITE FD-CLIENTE
               WHEN 1
               WHEN 2
               WHEN 3
                   DISPLAY TELA-CLIENTE-EXISTE
                   ACCEPT TELA-CLIENTE-EXISTE
                   EVALUATE WRK-SIM-NAO
                       WHEN "S"
                           CLOSE ARQCLI01
                           PERFORM 2100-MENU-INCLUI-CLI
                       WHEN "N"
                           CLOSE ARQCLI01
                           PERFORM 3000-FINALIZAR
                       WHEN OTHER
                           CLOSE ARQCLI01
                           PERFORM 3000-FINALIZAR
                   END-EVALUATE
           END-EVALUATE.
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
       2300-MENU-EXCLUI-CLI            SECTION.
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
                       PERFORM 2300-MENU-EXCLUI-CLI
                   WHEN "N"
                       CLOSE ARQCLI01
                       PERFORM 2010-MENU-CLIENTE
                   WHEN OTHER
                       CLOSE ARQCLI01
                       PERFORM 2010-MENU-CLIENTE
               END-EVALUATE
           ELSE
               DISPLAY TELA-EXC-CLIENTE
               ACCEPT TELA-EXC-CLIENTE
               EVALUATE FUNCTION UPPER-CASE(WRK-SIM-NAO)
                   WHEN "S"
                       DELETE ARQCLI01 RECORD
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
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA IMPORTAR UM CLIENTE                             *
      *----------------------------------------------------------------*
       2400-MENU-IMPORTA-CLI           SECTION.
      *----------------------------------------------------------------*
      *
           DISPLAY TELA-CLI-IMPORTACAO
           ACCEPT TELA-CLI-IMPORTACAO
      *    
           EVALUATE FUNCTION UPPER-CASE(WRK-SIM-NAO)
               WHEN "S"
                   PERFORM 2410-IMPORTAR-CLI
                   PERFORM 2010-MENU-CLIENTE
               WHEN "N"
                   PERFORM 2010-MENU-CLIENTE
               WHEN OTHER
                   PERFORM 2010-MENU-CLIENTE
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA IMPORTAR UM CLIENTE                             *
      *----------------------------------------------------------------*
       2410-IMPORTAR-CLI               SECTION.
      *----------------------------------------------------------------*
      *
           OPEN INPUT ARQIMPCL
      *
           IF WRK-FS-ARQIMPCL EQUAL ZEROS
               OPEN I-O ARQCLI01
      *        
               PERFORM UNTIL WRK-FS-ARQIMPCL NOT EQUAL ZEROS
                   READ ARQIMPCL
                   IF WRK-FS-ARQIMPCL EQUAL ZEROS
                       MOVE FD-IMP-CLIENTE
                                       TO WRK-AREA-ARQCLI01
                       MOVE WRK-CNPJ-CLI           
                                       TO LKS-NUMERO-I
                       MOVE WRK-AREA-ARQCLI01      
                                       TO FD-CLIENTE
                       MOVE 'CGC'      TO LKS-TIPO-CALCULO
                       MOVE 'V'        TO LKS-ACAO
                       MOVE ZEROS      TO LKS-RETORNO
                       MOVE ZEROS      TO LKS-NUMERO-F
      *                
                       CALL "HBSI30AO" USING LKS-PARM                   
      *                
                       IF LKS-RETORNO EQUAL ZEROS
                           WRITE FD-CLIENTE
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *    
           CLOSE ARQCLI01
           CLOSE ARQIMPCL.
      *
      *----------------------------------------------------------------*
       2410-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA FINALIZAR PROCESSAMENTO                         *
      *----------------------------------------------------------------*
       3000-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*
      *
           EXIT PROGRAM.
      *
      *----------------------------------------------------------------*
       3000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*