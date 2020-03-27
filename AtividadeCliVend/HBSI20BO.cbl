      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. HBSI20BO.
       AUTHOR.     TADEU COSTA DE OLIVEIRA.
      *================================================================*
      *           A V A L I A C A O - H B S I S / A M B E V            *
      *----------------------------------------------------------------*
      *    PROGRAMA....: HBSI20BO                                      *
      *    PROGRAMADOR.: TADEU COSTA DE OLIVEIRA                       *
      *    DATA........: 25/03/2019                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....: PROGRAMA PARA CADASTRO DE VENDEDORES          *                                                                   
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
              SELECT ARQVEN01 ASSIGN   TO UT-S-ARQVEN01
                        ORGANIZATION   IS INDEXED
                         ACCESS MODE   IS DYNAMIC
                          RECORD KEY   IS FS-COD-VEN
                       ALTERNATE KEY   IS FS-CPF-VEN
                           LOCK MODE   IS MANUAL
                         FILE STATUS   IS WRK-FS-ARQVEN01.
      *
              SELECT ARQIMPVN ASSIGN   TO UT-S-ARQIMPVN
                        ORGANIZATION   IS SEQUENTIAL
                         ACCESS MODE   IS SEQUENTIAL
                         FILE STATUS   IS WRK-FS-ARQIMPVN.
      
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
       FD ARQVEN01
          RECORD CONTAINS 83 CHARACTERS.
       01 FD-VENDEDOR.
          05 FS-COD-VEN               PIC 9(007).
          05 FS-CPF-VEN               PIC 9(011).
          05 FS-NOME-VEN              PIC X(040).
          05 FS-LAT-VEN               PIC S9(003)V9(008).
          05 FS-LONG-VEN              PIC S9(003)V9(008).
      *
       FD ARQIMPVN
          RECORD CONTAINS 83 CHARACTERS.
       01 FD-IMP-VENDEDOR.
          05 FS-IMP-COD-VEN            PIC 9(007).
          05 FS-IMP-CPF-VEN            PIC 9(011).
          05 FS-IMP-NOME-VEN           PIC X(040).
          05 FS-IMP-LAT-VEN            PIC S9(003)V9(008).
          05 FS-IMP-LONG-VEN           PIC S9(003)V9(008).
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)     VALUE
           ' HBSI20BO - INICIO DA AREA DE WORKING '.
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
           05 WRK-COD-VND-BUSC         PIC 9(007)      VALUE ZEROS.
           05 WRK-ARQ-IMP              PIC X(020)      VALUE SPACES.
           
       01  WRK-AREA-FS.
           05 WRK-FS-ARQIMPVN          PIC X(002)      VALUE "00".
           05 WRK-FS-ARQVEN01          PIC X(002)      VALUE "00".
      *
      *----------------------------------------------------------------*
       77 FILLER                       PIC  X(050)     VALUE
           'AREA PARA LAYOUT ENTRADA'.
      *----------------------------------------------------------------*
      *
       01  WRK-AREA-ARQVEN01.
           05 WRK-COD-VEN              PIC 9(007).
           05 WRK-CPF-VEN              PIC 9(011).
           05 WRK-NOME-VEN             PIC X(040).
           05 WRK-LAT-VEN              PIC S9(003)V9(008).
           05 WRK-LONG-VEN             PIC S9(003)V9(008).
      *
       01 LKS-PARM.
           05 LKS-NUMERO-I               PIC 9(015).
           05 LKS-NUMERO-F               PIC 9(015).
           05 LKS-TIPO-CALCULO           PIC X(003).
           05 LKS-ACAO                   PIC X(001).
           05 LKS-RETORNO                PIC 9(001).    
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)     VALUE
           ' HBSI20BO - FIM DA AREA DE WORKING '.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
       SCREEN SECTION.
      *----------------------------------------------------------------*
      *
       01  TELA-VENDEDOR.
           05 VALUE "CADASTRO DE VENDEDORES"           
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
       01  TELA-ADD-VENDEDOR.
           05 VALUE "CADASTRO DE VENDEDORES - INCLUSAO"
                                       BLANK SCREEN      LINE  2 COL  2.
           05 VALUE "CODIGO.......:"                     LINE  4 COL  2.
           05 CODIGO-VEN                                 LINE  4 COL 17
                                       PIC 9(007) TO WRK-COD-VEN.       
           05 VALUE "CPF..........:"                     LINE  5 COL  2.
           05 CPF-VEN                                    LINE  5 COL 17
                                       PIC 9(011) TO WRK-CPF-VEN.       
           05 VALUE "NOME.........:"                     LINE  6 COL  2.
           05 NOME-VEN                                   LINE  6 COL 17
                                       PIC X(040) TO WRK-NOME-VEN.      
           05 VALUE "LATITUDE.....:"                     LINE  7 COL  2.
           05 LATITUDE-VEN                               LINE  7 COL 17
                                       PIC S9(003)V9(008) TO
                                                            WRK-LAT-VEN.
           05 VALUE "LONGITUDE....:"                     LINE  8 COL  2.
           05 LONGITUDE-VEN                              LINE  8 COL 17
                                       PIC S9(003)V9(008) TO
                                                           WRK-LONG-VEN.
           05 VALUE "INCLUIR VENDEDOR?(S/N):"            LINE 10 COL  2.
           05 CONFIRMA                                   LINE 10 COL 25
                                       PIC X TO WRK-SIM-NAO.
      *    
       01  TELA-ALT-VENDEDOR.
           05 VALUE "CADASTRO DE VENDEDOR - ALTERACAO"
                                       BLANK SCREEN      LINE  2 COL  2.
           05 VALUE "CODIGO.......:"                     LINE  4 COL  2.
           05 CODIGO-VEN                                 LINE  4 COL 17
                                       PIC 9(007) TO WRK-COD-VEN.       
           05 VALUE "CPF..........:"                     LINE  5 COL  2.
           05 CPF-VEN                                    LINE  5 COL 17
                                       PIC 9(011) TO WRK-CPF-VEN.       
           05 VALUE "NOME.........:"                     LINE  6 COL  2.
           05 NOME-VEN                                   LINE  6 COL 17
                                       PIC X(040) TO WRK-NOME-VEN.      
           05 VALUE "LATITUDE.....:"                     LINE  7 COL  2.
           05 LATITUDE-VEN                               LINE  7 COL 17
                                       PIC S9(003)V9(008) TO
                                                            WRK-LAT-VEN.
           05 VALUE "LONGITUDE....:"                     LINE  8 COL  2.
           05 LONGITUDE-VEN                              LINE  8 COL 17
                                       PIC S9(003)V9(008) TO
                                                           WRK-LONG-VEN.
           05 VALUE "INCLUIR VENDEDOR?(S/N):"            LINE 10 COL  2.
           05 CONFIRMA                                   LINE 10 COL 25
                                       PIC X TO WRK-SIM-NAO.
      *    
       01  TELA-EXC-VENDEDOR.
           05 VALUE "CADASTRO DE VENDEDOR - EXCLUSAO"
                                       BLANK SCREEN      LINE  2 COL  2.
           05 VALUE "CODIGO.......:"                     LINE  4 COL  2.
           05 CODIGO-VEN                                 LINE  4 COL 17
                                       PIC 9(007) TO WRK-COD-VEN.       
           05 VALUE "CPF..........:"                     LINE  5 COL  2.
           05 CPF-VEN                                    LINE  5 COL 17
                                       PIC 9(011) TO WRK-CPF-VEN.       
           05 VALUE "NOME.........:"                     LINE  6 COL  2.
           05 NOME-VEN                                   LINE  6 COL 17
                                       PIC X(040) TO WRK-NOME-VEN.      
           05 VALUE "LATITUDE.....:"                     LINE  7 COL  2.
           05 LATITUDE-VEN                               LINE  7 COL 17
                                       PIC S9(003)V9(008) TO
                                                            WRK-LAT-VEN.
           05 VALUE "LONGITUDE....:"                     LINE  8 COL  2.
           05 LONGITUDE-VEN                              LINE  8 COL 17
                                       PIC S9(003)V9(008) TO
                                                           WRK-LONG-VEN.
           05 VALUE "INCLUIR VENDEDOR?(S/N):"            LINE 10 COL  2.
           05 CONFIRMA                                   LINE 10 COL 25
                                       PIC X TO WRK-SIM-NAO.
      *    
       01  TELA-BUSCA-VENDEDOR.
           05 VALUE "BUSCAR VENDEDOR"  BLANK SCREEN      LINE  2 COL  2.
           05 VALUE "DIGITE CODIGO VENDEDOR:"            LINE  6 COL  2.
           05 COD-BUSCA-VEN                              LINE  6 COL 25 
                                       PIC 9(007) TO WRK-COD-VEN.       
      *
       01  TELA-RESULT-BUSCA.
           05 VALUE "VENDEDOR NAO ENCONTRADO"             
                                       BLANK SCREEN      LINE  6 COL  2.
           05 VALUE "REALIZAR NOVA BUSCA? (S/N):"        LINE  8 COL  2.
           05 BUSCA-NOVA                                 LINE  8 COL 29
                                       PIC X TO WRK-SIM-NAO.
      *
       01  TELA-VENDEDOR-EXISTE.
           05 VALUE "VENDEDOR JA EXISTE" BLANK SCREEN    LINE  6 COL 10.
           05 VALUE "REALIZAR NOVO CADASTRO? (S/N)"      LINE  8 COL  2.
           05 BUSCA-NOVA                                 LINE  8 COL 31
                                       PIC X TO WRK-SIM-NAO.
      *
       01  TELA-CLI-IMPORTACAO.
           05 VALUE "CADASTRO DE VENDEDOR - IMPORTACAO"  
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
           PERFORM 2010-MENU-VENDEDOR.
      *
      *----------------------------------------------------------------*
       2000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA MOSTRAR MENU PRINCIPAL DO VENDEDOR              *
      *----------------------------------------------------------------*
       2010-MENU-VENDEDOR              SECTION.
      *----------------------------------------------------------------*
      *
           DISPLAY TELA-VENDEDOR
           ACCEPT TELA-VENDEDOR.
           EVALUATE WRK-OPCAO
              WHEN "01"
                  PERFORM 2100-MENU-INCLUI-VEN
              WHEN "02"
                  PERFORM 2200-MENU-ALTERA-VEN                              
              WHEN "03"
                  PERFORM 2300-MENU-EXCLUI-VEN
              WHEN "04"
                  PERFORM 2400-MENU-IMPORTA-VEN
              WHEN "05"
                  PERFORM 3000-FINALIZAR
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       2010-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA O MENU DE INCLUSAO DO VENDEDOR                  *
      *----------------------------------------------------------------*
       2100-MENU-INCLUI-VEN            SECTION.
      *----------------------------------------------------------------*
      *
           OPEN I-O ARQVEN01
           DISPLAY TELA-ADD-VENDEDOR
           ACCEPT TELA-ADD-VENDEDOR
           EVALUATE FUNCTION UPPER-CASE(WRK-SIM-NAO)
               WHEN "S"
                   PERFORM 2110-INCLUIR-VEN
                   CLOSE ARQVEN01
                   PERFORM 2010-MENU-VENDEDOR
               WHEN "N"
                   CLOSE ARQVEN01
                   PERFORM 2010-MENU-VENDEDOR
               WHEN OTHER
                   CLOSE ARQVEN01
                   PERFORM 2010-MENU-VENDEDOR
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       2100-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA INCLUIR UM VENDEDOR                             *
      *----------------------------------------------------------------*
       2110-INCLUIR-VEN                SECTION.
      *----------------------------------------------------------------*
      *
           MOVE WRK-CPF-VEN            TO LKS-NUMERO-I
           MOVE WRK-AREA-ARQVEN01      TO FD-VENDEDOR
           MOVE 'CGC'                  TO LKS-TIPO-CALCULO
           MOVE 'V'                    TO LKS-ACAO
           MOVE ZEROS                  TO LKS-RETORNO
           MOVE ZEROS                  TO LKS-NUMERO-F
           CALL "HBSI30AO" USING LKS-PARM
      *    
           EVALUATE LKS-RETORNO
               WHEN 0
                   WRITE FD-VENDEDOR
               WHEN 1
               WHEN 2
               WHEN 3
                   DISPLAY TELA-VENDEDOR-EXISTE
                   ACCEPT TELA-VENDEDOR-EXISTE
                   EVALUATE WRK-SIM-NAO
                       WHEN "S"
                           CLOSE ARQVEN01
                           PERFORM 2100-MENU-INCLUI-VEN
                       WHEN "N"
                           CLOSE ARQVEN01
                           PERFORM 3000-FINALIZAR
                       WHEN OTHER
                           CLOSE ARQVEN01
                           PERFORM 3000-FINALIZAR
                   END-EVALUATE
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       2110-99-FIM.                    EXIT.
      *----------------------------------------------------------------* 
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA O MENU DE ALTERACAO DO VENDEDOR                 * 
      *----------------------------------------------------------------*
       2200-MENU-ALTERA-VEN            SECTION.
      *----------------------------------------------------------------*
      *
           OPEN I-O ARQVEN01
           DISPLAY TELA-BUSCA-VENDEDOR
           ACCEPT TELA-BUSCA-VENDEDOR
           MOVE WRK-COD-VEN            TO FS-COD-VEN
           READ ARQVEN01               RECORD INTO WRK-AREA-ARQVEN01    
                  KEY IS               FS-COD-VEN
           IF WRK-FS-ARQVEN01 NOT EQUAL "00"
               DISPLAY TELA-RESULT-BUSCA
               ACCEPT TELA-RESULT-BUSCA
               EVALUATE FUNCTION UPPER-CASE(WRK-SIM-NAO)
                   WHEN "S"
                       CLOSE ARQVEN01
                       PERFORM 2200-MENU-ALTERA-VEN
                   WHEN "N"
                       CLOSE ARQVEN01
                       PERFORM 2010-MENU-VENDEDOR
                   WHEN OTHER
                       CLOSE ARQVEN01
                       PERFORM 2010-MENU-VENDEDOR
               END-EVALUATE
           ELSE
               DISPLAY TELA-ALT-VENDEDOR
               ACCEPT TELA-ALT-VENDEDOR
               EVALUATE FUNCTION UPPER-CASE(WRK-SIM-NAO)
                   WHEN "S"
                       PERFORM 2210-ALTERAR-VEN
                       CLOSE ARQVEN01
                       PERFORM 2010-MENU-VENDEDOR
                   WHEN "N"
                       CLOSE ARQVEN01
                       PERFORM 2010-MENU-VENDEDOR
                   WHEN OTHER
                       CLOSE ARQVEN01
                       PERFORM 2010-MENU-VENDEDOR
               END-EVALUATE
           END-IF.
      *
      *----------------------------------------------------------------*
       2200-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA ALTERAR O VENDEDOR                              *
      *----------------------------------------------------------------*
       2210-ALTERAR-VEN                SECTION.
      *----------------------------------------------------------------*
      *
           INITIALIZE FD-VENDEDOR
      *
           MOVE WRK-COD-VEN            TO FS-COD-VEN
           MOVE WRK-CPF-VEN            TO FS-CPF-VEN
           MOVE WRK-NOME-VEN           TO FS-NOME-VEN
           MOVE WRK-LAT-VEN            TO FS-LAT-VEN
           MOVE WRK-LONG-VEN           TO FS-LONG-VEN.
      *
      *----------------------------------------------------------------*
       2210-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA EXCLUIR UM VENDEDOR                             *
      *----------------------------------------------------------------*
       2300-MENU-EXCLUI-VEN            SECTION.
      *----------------------------------------------------------------*
      *
           OPEN I-O ARQVEN01
           DISPLAY TELA-BUSCA-VENDEDOR
           ACCEPT TELA-BUSCA-VENDEDOR
           MOVE WRK-COD-VEN            TO FS-COD-VEN
           READ ARQVEN01               RECORD INTO WRK-AREA-ARQVEN01    
                  KEY IS               FS-COD-VEN
           IF WRK-FS-ARQVEN01 NOT EQUAL "00"
               DISPLAY TELA-RESULT-BUSCA
               ACCEPT TELA-RESULT-BUSCA
               EVALUATE FUNCTION UPPER-CASE(WRK-SIM-NAO)
                   WHEN "S"
                       CLOSE ARQVEN01
                       PERFORM 2300-MENU-EXCLUI-VEN
                   WHEN "N"
                       CLOSE ARQVEN01
                       PERFORM 2010-MENU-VENDEDOR
                   WHEN OTHER
                       CLOSE ARQVEN01
                       PERFORM 2010-MENU-VENDEDOR
               END-EVALUATE
           ELSE
               DISPLAY TELA-EXC-VENDEDOR
               ACCEPT TELA-EXC-VENDEDOR
               EVALUATE FUNCTION UPPER-CASE(WRK-SIM-NAO)
                   WHEN "S"
                       DELETE ARQVEN01 RECORD
                       CLOSE ARQVEN01
                       PERFORM 2010-MENU-VENDEDOR
                   WHEN "N"
                       CLOSE ARQVEN01
                       PERFORM 2010-MENU-VENDEDOR
                   WHEN OTHER
                       CLOSE ARQVEN01
                       PERFORM 2010-MENU-VENDEDOR
               END-EVALUATE
           END-IF.
      *
      *----------------------------------------------------------------*
       2300-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA IMPORTAR UM VENDEDOR                            *
      *----------------------------------------------------------------*
       2400-MENU-IMPORTA-VEN           SECTION.
      *----------------------------------------------------------------*
      *
           DISPLAY TELA-CLI-IMPORTACAO
           ACCEPT TELA-CLI-IMPORTACAO
      *    
           EVALUATE FUNCTION UPPER-CASE(WRK-SIM-NAO)
               WHEN "S"
                   PERFORM 2410-IMPORTAR-VEN
                   PERFORM 2010-MENU-VENDEDOR
               WHEN "N"
                   PERFORM 2010-MENU-VENDEDOR
               WHEN OTHER
                   PERFORM 2010-MENU-VENDEDOR
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
       2400-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    ROTINA PARA IMPORTAR UM VENDEDOR                            *
      *----------------------------------------------------------------*
       2410-IMPORTAR-VEN               SECTION.
      *----------------------------------------------------------------*
      *
           OPEN INPUT ARQIMPVN
      *
           IF WRK-FS-ARQIMPVN EQUAL ZEROS
               OPEN I-O ARQVEN01
      *        
               PERFORM UNTIL WRK-FS-ARQIMPVN NOT EQUAL ZEROS
                   READ ARQIMPVN
                   IF WRK-FS-ARQIMPVN EQUAL ZEROS
                       MOVE FD-IMP-VENDEDOR
                                       TO WRK-AREA-ARQVEN01
                       MOVE WRK-CPF-VEN           
                                       TO LKS-NUMERO-I
                       MOVE WRK-AREA-ARQVEN01      
                                       TO FD-VENDEDOR
                       MOVE 'CGC'      TO LKS-TIPO-CALCULO
                       MOVE 'V'        TO LKS-ACAO
                       MOVE ZEROS      TO LKS-RETORNO
                       MOVE ZEROS      TO LKS-NUMERO-F
      *                
                       CALL "HBSI30AO" USING LKS-PARM                   
      *                
                       IF LKS-RETORNO EQUAL ZEROS
                           WRITE FD-VENDEDOR
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *    
           CLOSE ARQVEN01
           CLOSE ARQIMPVN.
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