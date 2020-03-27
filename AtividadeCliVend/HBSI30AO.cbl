      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID.       HBSI30AO.
       AUTHOR.           TADEU COSTA DE OLIVEIRA.
      *================================================================*
      *           A V A L I A C A O - H B S I S / A M B E V            *
      *----------------------------------------------------------------*
      *    PROGRAMA....: HBSI30AO                                      *
      *    PROGRAMADOR.: TADEU COSTA DE OLIVEIRA                       *
      *    DATA........: 27/03/2019                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....: VERIFICA O DIGITO DO CPF CNPJ                 *                                                            
      *    COMO USAR...: LKS-NUMERO-I ....: NUMERO INFORMADO           *
      *                : LKS-NUMERO-F ....: NUMERO CALCULADO           *
      *                : LKS-TIPO-CALCULO : CPF, CGC                   *
      *                : LKS-ACAO ........: V - VERIFICA               *
      *----------------------------------------------------------------*
      *    BOOKS USADOS:                                               *
      *    HBSINXXX - DESCRICAO DO BOOK                                *
      *----------------------------------------------------------------*
      *    MODULOS.....:                                               *
      *    HBSINXXX - DESCRICAO DO MODULO                              *
      *================================================================*
      *
      *================================================================*
       ENVIRONMENT DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
      *
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.
      *
      *================================================================*
       DATA                            DIVISION.
      *================================================================*
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      *-----------------------------------------------------------------
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)     VALUE
           ' HBSI30AO - INICIO DA AREA DE WORKING '.
      *----------------------------------------------------------------*
      *
       01  WS-AUXILIARES.
           05 WSS-IND-N                  PIC 9(002)  VALUE ZEROES.
           05 WSS-IND-O                  PIC 9(002)  VALUE ZEROES.
           05 WSS-IND-P                  PIC 9(002)  VALUE ZEROES.
           05 WSS-SOMA                   PIC 9(008)  VALUE ZEROES.
           05 WSS-NUMERO                 PIC 9(015)  VALUE ZEROES.
           05 WSS-NUMERO-R               REDEFINES WSS-NUMERO.
              10  WSS-NUMERO-T           PIC 9(001)  OCCURS 15 TIMES.
           05 WSS-PESOS                  PIC X(028)  VALUE SPACES.
           05 WSS-PESOS-R                REDEFINES WSS-PESOS.
              10  WSS-PESOS-T            PIC 9(002)  OCCURS 14 TIMES.
           05 WSS-QUOCI                  PIC 9(008)  VALUE ZEROES.
           05 WSS-RESTO                  PIC 9(008)  VALUE ZEROES.
           05 WSS-PESOS-CPF              PIC X(028)  VALUE
                                   '0000000011100908070605040302'.
           05 WSS-PESOS-CGC              PIC X(028)  VALUE
                                   '0706050403020908070605040302'.
      *
      *----------------------------------------------------------------*
       01  FILLER                      PIC  X(050)     VALUE
           ' HBSI30AO - FIM DA AREA DE WORKING '.
      *----------------------------------------------------------------*
      *
      *-----------------------------------------------------------------
       LINKAGE SECTION.
      *-----------------------------------------------------------------

       01  LKS-PARAMETRO.
           05 COMPRIMENTO                PIC S9(04) COMP.
           05 LKS-NUMERO-I               PIC 9(015).
           05 FILLER                     PIC X(001).
           05 LKS-NUMERO-F               PIC 9(015).
           05 FILLER                     PIC X(001).
           05 LKS-TIPO-CALCULO           PIC X(003).
           05 FILLER                     PIC X(001).
           05 LKS-ACAO                   PIC X(001).
           05 LKS-RETORNO                PIC 9(001).
      *-----------------------------------------------------------------
      * LKS-NUMERO-F     = número retornado do programa
      * LKS-TIPO-CALCULO = CPF ou CGC ou PIS
      * LKS-ACAO         = C (calcula) V (verifica) 
      * LKS-RETORNO      = 0 - codigo verificado está correto
      *                  = 1 - LKS-TIPO-CALCULO está incorreto
      *                  = 2 - LKS-ACAO está incorreta
      *                  = 3 - código verificado está com erro	
      *-----------------------------------------------------------------
      *
      *================================================================*
       PROCEDURE DIVISION USING LKS-PARAMETRO.
      *================================================================*
      *
      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL DO PROGRAMA                                *
      *----------------------------------------------------------------*
       0000-PRINCIPAL                  SECTION.
      *----------------------------------------------------------------*
      *
           PERFORM 1000-INICIAL
           PERFORM 2000-PRINCIPAL
           PERFORM 5000-FINAL
           GOBACK.
      *
      *----------------------------------------------------------------*
       0000-99-FIM.                    EXIT.
      *----------------------------------------------------------------*
      *
      *-----------------------------------------------------------------
       1000-INICIAL                    SECTION.
      *-----------------------------------------------------------------
      *
           MOVE ZEROES TO LKS-RETORNO  
           EVALUATE TRUE
              WHEN LKS-ACAO = 'V'
                   EVALUATE LKS-TIPO-CALCULO
                      WHEN 'CPF'
                      WHEN 'CGC'
                         MOVE LKS-NUMERO-I TO WSS-NUMERO
                      WHEN OTHER
                         MOVE 1 TO LKS-RETORNO 
                         GOBACK
                   END-EVALUATE
              WHEN OTHER
                   MOVE 2 TO LKS-RETORNO 
                   GOBACK 
           END-EVALUATE.
      *
      *-----------------------------------------------------------------
       1000-99-FIM.                    EXIT.
      *-----------------------------------------------------------------
      *
      *-----------------------------------------------------------------
       2000-PRINCIPAL                  SECTION.
      *-----------------------------------------------------------------
      *
           EVALUATE LKS-TIPO-CALCULO
              WHEN 'CPF'
                    PERFORM 2100-CALCULO-CPF
              WHEN 'CGC'
                    PERFORM 2200-CALCULO-CGC
           END-EVALUATE.
      *
      *-----------------------------------------------------------------
       2000-99-FIM.                    EXIT.
      *-----------------------------------------------------------------
      *
      *-----------------------------------------------------------------
       2100-CALCULO-CPF                SECTION.
      *-----------------------------------------------------------------
      *
           MOVE WSS-PESOS-CPF TO WSS-PESOS
           MOVE 05            TO WSS-IND-N
           MOVE 06            TO WSS-IND-P
           MOVE 13            TO WSS-IND-O
           MOVE ZEROES        TO WSS-SOMA
           PERFORM 3000-CALC-DIGITO-1

           MOVE 05            TO WSS-IND-N
           MOVE 05            TO WSS-IND-P
           MOVE 14            TO WSS-IND-O
           MOVE ZEROES        TO WSS-SOMA
           PERFORM 4000-CALC-DIGITO-2.
      *
      *-----------------------------------------------------------------
       2100-99-FIM.                    EXIT.
      *-----------------------------------------------------------------
      *
      *-----------------------------------------------------------------
       2200-CALCULO-CGC                SECTION.
      *-----------------------------------------------------------------
      *
           MOVE WSS-PESOS-CGC TO WSS-PESOS
           MOVE 01            TO WSS-IND-N
           MOVE 02            TO WSS-IND-P
           MOVE 13            TO WSS-IND-O
           MOVE ZEROES        TO WSS-SOMA
           PERFORM 3000-CALC-DIGITO-1

           MOVE 01            TO WSS-IND-N
           MOVE 01            TO WSS-IND-P
           MOVE 14            TO WSS-IND-O
           MOVE ZEROES        TO WSS-SOMA
           PERFORM 4000-CALC-DIGITO-2.
      *
      *-----------------------------------------------------------------
       2200-99-FIM.                    EXIT.
      *-----------------------------------------------------------------
      *
      *-----------------------------------------------------------------
       3000-CALC-DIGITO-1              SECTION.
      *-----------------------------------------------------------------
      *
           MOVE ZEROES TO WSS-SOMA
           PERFORM UNTIL WSS-IND-N GREATER WSS-IND-O
                   COMPUTE WSS-SOMA = WSS-SOMA +
                                     (WSS-NUMERO-T (WSS-IND-N) *
                                      WSS-PESOS-T  (WSS-IND-P))
                   ADD 1 TO WSS-IND-N
                            WSS-IND-P
           END-PERFORM
           DIVIDE WSS-SOMA BY 11 GIVING WSS-QUOCI REMAINDER WSS-RESTO
           IF WSS-RESTO EQUAL 0 OR 1
              MOVE ZEROES TO WSS-NUMERO-T (14)
           ELSE
              SUBTRACT WSS-RESTO FROM 11 GIVING WSS-NUMERO-T (14)
           END-IF.
      *
      *-----------------------------------------------------------------
       3000-99-FIM.                    EXIT.
      *-----------------------------------------------------------------
      *
      *-----------------------------------------------------------------
       4000-CALC-DIGITO-2              SECTION.
      *-----------------------------------------------------------------
      *
           MOVE ZEROES TO WSS-SOMA
           PERFORM UNTIL WSS-IND-N GREATER WSS-IND-O
                   COMPUTE WSS-SOMA = WSS-SOMA +
                                     (WSS-NUMERO-T (WSS-IND-N) *
                                      WSS-PESOS-T  (WSS-IND-P))
                   ADD 1 TO WSS-IND-N
                            WSS-IND-P
           END-PERFORM
           DIVIDE WSS-SOMA BY 11 GIVING WSS-QUOCI REMAINDER WSS-RESTO
           IF WSS-RESTO EQUAL 0 OR 1
              MOVE ZEROES TO WSS-NUMERO-T (15)
           ELSE
              SUBTRACT WSS-RESTO FROM 11 GIVING WSS-NUMERO-T (15)
           END-IF.
      *
      *-----------------------------------------------------------------
       4000-99-FIM.                    EXIT.
      *-----------------------------------------------------------------
      *
      *-----------------------------------------------------------------
       5000-FINAL                      SECTION.
      *-----------------------------------------------------------------
      *
           MOVE WSS-NUMERO    TO LKS-NUMERO-F          
           IF  LKS-ACAO EQUAL 'V'                      
               IF LKS-NUMERO-I EQUAL LKS-NUMERO-F      
                  MOVE 0 TO LKS-RETORNO                
               ELSE                                    
                  MOVE 3 TO LKS-RETORNO                
               END-IF                                  
           ELSE                                        
               MOVE 0 TO LKS-RETORNO                   
           END-IF.                                      
      *
      *-----------------------------------------------------------------
       5000-99-FIM.                    EXIT.
      *-----------------------------------------------------------------