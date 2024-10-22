      ******************************************************************
      *                                                                *
      * LICENSED MATERIALS - PROPERTY OF IBM                           *
      *                                                                *
      * "RESTRICTED MATERIALS OF IBM"                                  *
      *                                                                *
      * (C) COPYRIGHT IBM CORP. 2021       ALL RIGHTS RESERVED         *
      *                                                                *
      * US GOVERNMENT USERS RESTRICTED RIGHTS - USE, DUPLICATION,      *
      * OR DISCLOSURE RESTRICTED BY GSA ADP SCHEDULE                   *
      * CONTRACT WITH IBM CORPORATION                                  *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GETAAVG.
      *****************************************************************
      *                                                               *
      *                                                               *
      *****************************************************************
      * THIS PROGRAM IS TO BE USED ONLY FOR IBM INTERNAL USE ONLY     *
      *****************************************************************
      *                                                               *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

      *01 WS-SWITCHES.
      *01 WS-CUSTOMER-EXISTS  PIC X(01)  VALUE 'N'.
      *      88 VALID-CUSTOMER              VALUE 'Y'.
      *      88 INVALID-CUSTOMER            VALUE 'N'.

       01 WS-WORK.
          05 WS-AVG-PREMIUM      PIC S9(09) COMP-5.
          05 WS-CUSTOMER-NUMBER  PIC S9(09) COMP-5.
          05 WS-E-SUMASSURED     PIC S9(09) COMP.
          05 WS-H-VALUE          PIC S9(09) COMP.
          05 WS-STATUS-CODE      PIC  X(02) .
          05 LGAC-NCS            PIC  X(02) VALUE 'ON'.
          05 DB2-BROKERID-INT    PIC S9(09) COMP.
          05 DB2-PAYMENT-INT     PIC S9(09) COMP.
          05 DB2-POLICYNUM-INT   PIC S9(09) COMP.
          05 WS-CUSTOMER-EXISTS  PIC  X(01) .

            EXEC SQL
             INCLUDE SQLCA
            END-EXEC.

            EXEC SQL
              INCLUDE DGENAPP
            END-EXEC.

            COPY LGPOLICY.

       LINKAGE SECTION.
        01 WS-IN-REC.
           05 IN-REQUEST-ID       PIC X(06).
           05 IN-CUST-NUMBER      PIC 9(10).
           05 IN-OVERPAID-FLAG    PIC X(01).
           05 IN-ACTION-CODE      PIC X(01).
           05 OUT-PREMIUM         PIC S9(09) COMP-5.
           05 OUT-STATUS-CODE     PIC X(02).


       PROCEDURE DIVISION USING WS-IN-REC.
       0001-MAIN.

           DISPLAY 'START OF PROGRAM GETAAVG'

           PERFORM 1000-INITIALIZATION
      *       THRU 1000-EXIT

           PERFORM 2000-CHECK-TYPE
      *       THRU 2000-EXIT

           MOVE WS-STATUS-CODE TO OUT-STATUS-CODE

           DISPLAY 'END OF PROGRAM GETAAVG'

           PERFORM 9000-END-PARA
           .
      *0001-MAIN-EXIT.
           EXIT.

       1000-INITIALIZATION.

      *    INITIALIZE WS-SWITCHES

           DISPLAY "CUSTOMER NUMBER IS " IN-CUST-NUMBER
           MOVE IN-CUST-NUMBER TO WS-CUSTOMER-NUMBER
           .
      *    PERFORM 1000-EXIT.
      *1000-EXIT.
           EXIT.

       2000-CHECK-TYPE.

           EVALUATE IN-REQUEST-ID
           WHEN '0AVCUS'
                PERFORM 3000-CUSTOMER-ACTION
      *            THRU 3000-EXIT

           WHEN '0AVMOT'
                PERFORM 3100-GET-AVG-MOT-PREMIUM
      *            THRU 3100-EXIT

           WHEN '0AVEND'
                PERFORM 3200-GET-ENDOWMENT
      *            THRU 3200-EXIT

           WHEN '0AVHOU'
                PERFORM 3300-GET-HOUSE
      *            THRU 3300-EXIT

           WHEN '0AVCOM'
                PERFORM 3400-GET-COMMERCIAL
      *            THRU 3400-EXIT

           WHEN OTHER
                MOVE '16' TO WS-STATUS-CODE
                DISPLAY 'INVALID REQUEST ID:' IN-REQUEST-ID
                PERFORM 9000-END-PARA

           END-EVALUATE.

      *    PERFORM 2000-EXIT.

      *2000-EXIT.
           EXIT.

       3000-CUSTOMER-ACTION.

           EVALUATE TRUE
           WHEN IN-ACTION-CODE = '1'
      * Call  to Insert dummy row in DB2 Customer table
                PERFORM ONBOARD-CUSTOMER

            WHEN IN-ACTION-CODE = '2'
      * Dummy policy insert
                PERFORM INSERT-CUSTOMER-POLICY

            WHEN IN-ACTION-CODE = '3'
      * Delete dummy customer
                PERFORM DELETE-CUSTOMER-POLICY

           END-EVALUATE.
      *    PERFORM 3000-EXIT.

      *3000-EXIT.
           EXIT.

      *----------------------------------------------------------------*
      * Logic add a customer in the DB                                 *
      *----------------------------------------------------------------*
       ONBOARD-CUSTOMER.
      *================================================================*
      * Insert row into Customer table based on customer number        *
      *================================================================*
           MOVE 'JOHN'          TO DB2-FIRSTNAME
           MOVE 'DOE'           TO DB2-LASTNAME
           MOVE '1950-01-01'    TO DB2-DATEOFBIRTH
           MOVE 'XXX'           TO DB2-HOUSENAME
           MOVE '1231'          TO DB2-HOUSENUMBER
           MOVE '32112'         TO DB2-POSTCODE
           MOVE '1111122222'    TO DB2-PHONE-MOBILE
           MOVE '1111122222'    TO DB2-PHONE-HOME
           MOVE 'johndoe@abc.com'
                                TO DB2-EMAIL-ADDRESS

           IF LGAC-NCS = 'ON'
             EXEC SQL
               INSERT INTO CUSTOMER
                         ( CUSTOMERNUMBER,
                           FIRSTNAME,
                           LASTNAME,
                           DATEOFBIRTH,
                           HOUSENAME,
                           HOUSENUMBER,
                           POSTCODE,
                           PHONEMOBILE,
                           PHONEHOME,
                           EMAILADDRESS )
                  VALUES ( DEFAULT,
                           :DB2-FIRSTNAME,
                           :DB2-LASTNAME,
                           :DB2-DATEOFBIRTH,
                           :DB2-HOUSENAME,
                           :DB2-HOUSENUMBER,
                           :DB2-POSTCODE,
                           :DB2-PHONE-MOBILE,
                           :DB2-PHONE-HOME,
                           :DB2-EMAIL-ADDRESS )
             END-EXEC
             EVALUATE SQLCODE
               WHEN 0
                  EXEC SQL
                      SET :WS-CUSTOMER-NUMBER = IDENTITY_VAL_LOCAL()
                  END-EXEC
                  MOVE WS-CUSTOMER-NUMBER TO IN-CUST-NUMBER
               WHEN 100
                  DISPLAY 'DUPLICATE CUSTOMER'
                  MOVE '80' TO WS-STATUS-CODE
                  PERFORM 9000-END-PARA
               WHEN OTHER
                  MOVE '90' TO WS-STATUS-CODE
                  PERFORM 9000-END-PARA
             END-EVALUATE
           ELSE
             EXEC SQL
               INSERT INTO CUSTOMER
                         ( CUSTOMERNUMBER,
                           FIRSTNAME,
                           LASTNAME,
                           DATEOFBIRTH,
                           HOUSENAME,
                           HOUSENUMBER,
                           POSTCODE,
                           PHONEMOBILE,
                           PHONEHOME,
                           EMAILADDRESS )
                  VALUES ( :WS-CUSTOMER-NUMBER,
                           :DB2-FIRSTNAME,
                           :DB2-LASTNAME,
                           :DB2-DATEOFBIRTH,
                           :DB2-HOUSENAME,
                           :DB2-HOUSENUMBER,
                           :DB2-POSTCODE,
                           :DB2-PHONE-MOBILE,
                           :DB2-PHONE-HOME,
                           :DB2-EMAIL-ADDRESS )
             END-EXEC
             EVALUATE SQLCODE
               WHEN 0
                  CONTINUE
               WHEN 100
                  DISPLAY 'DUPLICATE CUSTOMER'
                  MOVE '80' TO WS-STATUS-CODE
                  PERFORM 9000-END-PARA
               WHEN OTHER
                  MOVE '90' TO WS-STATUS-CODE
                  PERFORM 9000-END-PARA
             END-EVALUATE
           END-IF.

           EXIT.


      *================================================================*
       INSERT-CUSTOMER-POLICY.

           MOVE '1900-01-01'   TO DB2-ISSUEDATE
           MOVE '2999-01-01'   TO DB2-EXPIRYDATE
           MOVE 'M'            TO DB2-POLICYTYPE
           MOVE 0              TO DB2-BROKERID
           MOVE 'DUMMY'        TO DB2-BROKERSREF
           MOVE 0              TO DB2-PAYMENT

      *    Move numeric fields to integer format
           MOVE DB2-BROKERID TO DB2-BROKERID-INT
           MOVE DB2-PAYMENT TO DB2-PAYMENT-INT

           EXEC SQL
             INSERT INTO POLICY
                       ( POLICYNUMBER,
                         CUSTOMERNUMBER,
                         ISSUEDATE,
                         EXPIRYDATE,
                         POLICYTYPE,
                         LASTCHANGED,
                         BROKERID,
                         BROKERSREFERENCE,
                         PAYMENT           )
                VALUES ( DEFAULT,
                         :WS-CUSTOMER-NUMBER,
                         :DB2-ISSUEDATE,
                         :DB2-EXPIRYDATE,
                         :DB2-POLICYTYPE,
                         CURRENT TIMESTAMP,
                         :DB2-BROKERID-INT,
                         :DB2-BROKERSREF,
                         :DB2-PAYMENT-INT      )
           END-EXEC

           Evaluate SQLCODE

             When 0
               MOVE '00' TO WS-STATUS-CODE

             When -530
               MOVE '70' TO WS-STATUS-CODE
               PERFORM 9000-END-PARA

             When Other
               MOVE '90' TO WS-STATUS-CODE
               PERFORM 9000-END-PARA

           END-Evaluate.

      *    get value of assigned policy number
           EXEC SQL
             SET :DB2-POLICYNUM-INT = IDENTITY_VAL_LOCAL()
           END-EXEC

           MOVE DB2-POLICYNUM-INT TO DB2-POLICYNUMBER
           DISPLAY 'POLICY NUM'
           .
           EXIT.

      *================================================================*
      * Delete appropriate row from policy table                       *
      *  because of FOREIGN KEY definitions the delete should be       *
      *  propagated to the appropriate 'policy type' table             *
      *================================================================*
       DELETE-CUSTOMER-POLICY.


           EXEC SQL
             DELETE
               FROM POLICY
               WHERE CUSTOMERNUMBER = :WS-CUSTOMER-NUMBER
           END-EXEC

      *    Treat SQLCODE 0 and SQLCODE 100 (record not found) as
      *    successful - end result is record does not exist
           EVALUATE SQLCODE
              WHEN 0
              WHEN 100
               CONTINUE
              WHEN OTHER
               MOVE '90' TO WS-STATUS-CODE
               PERFORM 9000-END-PARA
           END-EVALUATE
           .

           EXIT.


       3100-GET-AVG-MOT-PREMIUM.

           PERFORM 310A-VALIDATE-CUSTOMER
      *       THRU 310A-EXIT

           IF WS-CUSTOMER-EXISTS = 'Y'

              EXEC SQL
                 SELECT AVG(MOT.PREMIUM)
                 INTO :WS-AVG-PREMIUM
                 FROM CUSTOMER CUST
                 INNER JOIN POLICY POL
                 ON POL.CUSTOMERNUMBER = CUST.CUSTOMERNUMBER
                 INNER JOIN MOTOR MOT
                 ON POL.POLICYNUMBER = MOT.POLICYNUMBER
                 WHERE POL.CUSTOMERNUMBER = :WS-CUSTOMER-NUMBER
                 GROUP BY POL.CUSTOMERNUMBER
              END-EXEC

              DISPLAY 'SQLCODE:' SQLCODE

              EVALUATE SQLCODE
               WHEN 0
                    MOVE '00' TO WS-STATUS-CODE
               WHEN 100
                    MOVE '02' TO WS-STATUS-CODE
                    PERFORM 9000-END-PARA
               WHEN OTHER
                    MOVE '16' TO WS-STATUS-CODE
                    PERFORM 9000-END-PARA
              END-EVALUATE
           ELSE
              MOVE '08' TO WS-STATUS-CODE
              DISPLAY 'INVALID CUSTOMER!!!'
              PERFORM 9000-END-PARA
           END-IF

           PERFORM 4000-CHECK-IF-OVERPAID
      *       THRU 4000-EXIT
      *    PERFORM 3100-EXIT 
      *    .
      *3100-EXIT.
           EXIT.

       310A-VALIDATE-CUSTOMER.

           EXEC SQL
                SELECT 'Y' INTO :WS-CUSTOMER-EXISTS
                FROM CUSTOMER
                WHERE CUSTOMERNUMBER = :WS-CUSTOMER-NUMBER
           END-EXEC

           EVALUATE WS-CUSTOMER-EXISTS 
            WHEN 'Y'
                DISPLAY 'VALID'
                CONTINUE 

            WHEN OTHER
                MOVE '16' TO WS-STATUS-CODE
                MOVE 'N'  TO WS-CUSTOMER-EXISTS
      *         PERFORM 9000-END-PARA
           END-EVALUATE
           .
      *    PERFORM 310A-EXIT.

      *310A-EXIT.
           EXIT.


       3200-GET-ENDOWMENT.

           EXEC SQL
                SELECT SUMASSURED
                INTO :WS-E-SUMASSURED
                FROM CUSTOMER CUST
                INNER JOIN POLICY POL
                ON POL.CUSTOMERNUMBER = CUST.CUSTOMERNUMBER
                INNER JOIN ENDOWMENT END1
                ON POL.POLICYNUMBER = END1.POLICYNUMBER
                WHERE POL.CUSTOMERNUMBER = :WS-CUSTOMER-NUMBER
           END-EXEC

           DISPLAY 'SQLCODE:' SQLCODE

           EVALUATE SQLCODE
           WHEN 0
                MOVE WS-E-SUMASSURED TO OUT-PREMIUM
                DISPLAY 'NO AVG PREMIUM FOR ENDOWMENT POLICY!'
                MOVE '01' TO WS-STATUS-CODE
           WHEN 100
                MOVE '02' TO WS-STATUS-CODE
           WHEN OTHER
                MOVE '16' TO WS-STATUS-CODE
                PERFORM 9000-END-PARA
           END-EVALUATE

           .
      *    PERFORM 3200-EXIT.

      *3200-EXIT.
           EXIT.


       3300-GET-HOUSE.

           EXEC SQL
                SELECT HOUS.VALUE
                INTO :WS-H-VALUE
                FROM CUSTOMER CUST
                INNER JOIN POLICY POL
                ON POL.CUSTOMERNUMBER = CUST.CUSTOMERNUMBER
                INNER JOIN HOUSE HOUS
                ON POL.POLICYNUMBER = HOUS.POLICYNUMBER
                WHERE POL.CUSTOMERNUMBER = :WS-CUSTOMER-NUMBER
                END-EXEC

           DISPLAY 'SQLCODE:' SQLCODE

           EVALUATE SQLCODE
           WHEN 0
                MOVE WS-H-VALUE TO OUT-PREMIUM
                DISPLAY 'NO AVG PREMIUM FOR HOUSING POLICY!'
                MOVE '01' TO WS-STATUS-CODE
           WHEN 100
                MOVE '02' TO WS-STATUS-CODE
           WHEN OTHER
                MOVE '16' TO WS-STATUS-CODE
                PERFORM 9000-END-PARA
           END-EVALUATE
      *    PERFORM 3300-EXIT 
           .
      *3300-EXIT.
           EXIT.



       3400-GET-COMMERCIAL.

           EXEC SQL
                SELECT CUSTOMER
                INTO :DB2-CUSTOMER
                FROM CUSTOMER CUST
                INNER JOIN POLICY POL
                ON POL.CUSTOMERNUMBER = CUST.CUSTOMERNUMBER
                INNER JOIN COMMERCIAL COMM
                ON POL.POLICYNUMBER = COMM.POLICYNUMBER
                WHERE POL.CUSTOMERNUMBER = :WS-CUSTOMER-NUMBER
                END-EXEC

           DISPLAY 'SQLCODE:' SQLCODE

           EVALUATE SQLCODE
           WHEN 0
                MOVE DB2-CUSTOMER TO OUT-PREMIUM
                DISPLAY 'NO AVG PREMIUM FOR COMMERCIAL POLICY!'
                MOVE '01' TO WS-STATUS-CODE
           WHEN 100
                MOVE '02' TO WS-STATUS-CODE
           WHEN OTHER
                MOVE '16' TO WS-STATUS-CODE
                PERFORM 9000-END-PARA
           END-EVALUATE
      *    PERFORM 3400-EXIT 
           .
      *3400-EXIT.
           EXIT.

       4000-CHECK-IF-OVERPAID.

      *    Missing numeric check
           EVALUATE IN-REQUEST-ID

             WHEN '0AVMOT'
               PERFORM 4100-CALC-RETURN-PREMIUM
      *           THRU 4100-EXIT

             WHEN OTHER
               CONTINUE

           END-EVALUATE
      *    PERFORM 4000-EXIT 
           .

      *4000-EXIT.
           EXIT.

       4100-CALC-RETURN-PREMIUM.

           IF IN-OVERPAID-FLAG = 'Y'
              COMPUTE OUT-PREMIUM = WS-AVG-PREMIUM * -1
           ELSE
              MOVE WS-AVG-PREMIUM TO OUT-PREMIUM
           END-IF
           DISPLAY 'PREMIUM GETAAVG:' OUT-PREMIUM
      *    PERFORM 4100-EXIT 
           .

      *4100-EXIT.
           EXIT.

       9000-END-PARA.

           DISPLAY 'STATUS CODE:' WS-STATUS-CODE

           GOBACK.

