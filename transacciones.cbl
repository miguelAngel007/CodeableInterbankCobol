      ******************************************************************
      * AUTHOR MIGUEL ANGEL CHAVEZ DOMINGUEZ:
      * DATE:
      * PURPOSE:
      * TECTONICS: COBC
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CODEABLE-INTERBANK-TRANSACCIONES.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACCIONES ASSIGN TO '../data/transacciones.csv'
               ORGANIZATION IS LINE SEQUENTIAL
               STATUS STATUS-FILE.
       DATA DIVISION.
       FILE SECTION.
           FD TRANSACCIONES.
           01 TRANSACCION          PIC X(100).
       WORKING-STORAGE SECTION.

       01 STATUS-FILE          PIC XX.
       01 READKEY              PIC X.
       01 MENSAJE              PIC X(80).

       01 FIN                  PIC X(3) VALUE "NO".
         88 CONTINUAR          VALUE "NO".
         88 TERMINAR           VALUE "YES".

       01 TIPO                 PIC X(10).
       01 MONTO-TXT            PIC X(15).

       01 TRANSACCION-ID       PIC X(10).
       01 MONTO                PIC 9(7)V99 VALUE 0.

       01 MAX-TRX-ID           PIC X(10).
       01 MAX-TRX-MONTO        PIC 9(7)V99 VALUE 0.

       01 BALANCE-FINAL        PIC 9(7)V99 VALUE 0.

       01 TOTAL-CREDITO        PIC 9(7)V99 VALUE 0.
       01 TOTAL-DEBITO         PIC 9(7)V99 VALUE 0.

       01 CONTADOR-CREDITO     PIC 9(4) VALUE 0.
       01 CONTADOR-DEBITO      PIC 9(4) VALUE 0.

       01 ES-PRIMER-LINEA      PIC X VALUE 'S'.
       01 GUIONES              PIC X(80) VALUES ALL "-".

       01 COUNT-MESSAGE        PIC X(100).
       01 MAX-TRX-MESSAGE      PIC X(100).

       01 BALANCE-FINAL-FORMAT     PIC Z(7)9.99.
       01 MAX-TRX-MONTO-FORMAT     PIC Z(7)9.99.
       01 CONTADOR-CREDITO-FORMAT  PIC Z(4).
       01 CONTADOR-DEBITO-FORMAT   PIC Z(4).


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM OPEN-FILE.
           PERFORM PROCESS-FILE.
           PERFORM CLOSE-FILE.
           PERFORM PRINT-RESULTS.
           GO TO FINALIZE.

       OPEN-FILE.
           OPEN INPUT TRANSACCIONES.
           IF (STATUS-FILE > "07")
               STRING "ERROR AL ABRIR EL ARCHIVO " STATUS-FILE
                 DELIMITED BY SIZE INTO MENSAJE
               DISPLAY MENSAJE
               MOVE "YES" TO FIN.

       CLOSE-FILE.
           CLOSE TRANSACCIONES.

       PROCESS-FILE.

           PERFORM UNTIL TERMINAR
               READ TRANSACCIONES
                   AT END
                       SET TERMINAR TO TRUE
                   NOT AT END
                       PERFORM PROCESAR-LINEA
               END-READ
           END-PERFORM.

           SUBTRACT TOTAL-DEBITO FROM TOTAL-CREDITO
           GIVING BALANCE-FINAL.

           MOVE BALANCE-FINAL TO BALANCE-FINAL-FORMAT.
           MOVE MAX-TRX-MONTO TO MAX-TRX-MONTO-FORMAT.
           MOVE CONTADOR-CREDITO TO CONTADOR-CREDITO-FORMAT.
           MOVE CONTADOR-DEBITO TO CONTADOR-DEBITO-FORMAT.


       PROCESAR-LINEA.
           IF ES-PRIMER-LINEA = "S"
               MOVE "N" TO ES-PRIMER-LINEA
           ELSE
               UNSTRING TRANSACCION DELIMITED BY ","
                 INTO TRANSACCION-ID, TIPO, MONTO-TXT.

           MOVE FUNCTION NUMVAL (MONTO-TXT) TO MONTO. *> PARSE

           IF TIPO = "Crédito"
               ADD MONTO TO TOTAL-CREDITO
               ADD 1 TO CONTADOR-CREDITO
           ELSE
               IF TIPO = "Débito"
                   ADD MONTO TO TOTAL-DEBITO
                   ADD 1 TO CONTADOR-DEBITO
               END-IF
           END-IF.

           IF MONTO > MAX-TRX-MONTO
               MOVE TRANSACCION-ID TO MAX-TRX-ID
               MOVE MONTO TO MAX-TRX-MONTO
           END-IF.

       PRINT-RESULTS.
           DISPLAY "REPORTE DE TRANSACCIONES".
           DISPLAY GUIONES.
           DISPLAY "Balance Final: " BALANCE-FINAL-FORMAT.

           STRING "Transacción de Mayor Monto: Id " MAX-TRX-ID
             " - " MAX-TRX-MONTO-FORMAT
             DELIMITED BY SIZE INTO MAX-TRX-MESSAGE
             DISPLAY MAX-TRX-MESSAGE.

           STRING "Conteo de Transacciones: Crédito: "
             CONTADOR-CREDITO-FORMAT
             " Débito: " CONTADOR-DEBITO-FORMAT
           DELIMITED BY SIZE INTO COUNT-MESSAGE
           DISPLAY COUNT-MESSAGE.


           ACCEPT READKEY.

       FINALIZE.
           EXIT PROGRAM.

       END PROGRAM CODEABLE-INTERBANK-TRANSACCIONES.
