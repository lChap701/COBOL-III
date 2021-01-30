       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CBLSC02C.
       AUTHOR.        LUCAS CHAPMAN.
       DATE-WRITTEN.  1/27/2021.
       DATE-COMPILED.
      ************************************************
      *                                              *
      * WRITES A SALES REPORT FOR PARKS WITH A MAJOR *
      * CONTROL BREAK ON EACH ATTENDANCE MONTH.      *
      *                                              *
      ************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT PARKS-REC
               ASSIGN TO SORTOUT
               ORGANIZATION IS SEQUENTIAL.

           SELECT PRTOUT
               ASSIGN TO PARKDATE
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  PARKS-REC
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 14 CHARACTERS
           DATA RECORD IS PARK-REC.

       01  PARK-REC.
           05  ATTENDANCE-DATE     PIC 9(8)    COMP-3.
           05  PARK                PIC 99.
           05  DISCOUNT-CODE       PIC X.
           05  ADULT-TICKETS       PIC 99.
           05  JUNIOR-TICKETS      PIC 99.
           05  SENIOR-TICKETS      PIC 99.

       FD  PRTOUT
           RECORDING MODE IS F
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS PRTLINE
           LINAGE IS 60 WITH FOOTING AT 56.

       01  PRTLINE                 PIC X(132).

       WORKING-STORAGE SECTION.
       01  WORK-AREA.
      * ADDS PAGE NUMBERS TO REPORTS *
           05  C-PCTR              PIC 99      VALUE 0.
      * CHECKS IF THE END OF THE INPUT FILE HAS BEEN REACHED *
           05  EOF                 PIC X       VALUE 'N'.
      * HOLD VARIABLES FOR CONTROL BREAK CHECKS *
           05  H-MM                PIC 99.
           05  H-YYYY              PIC 9(4).
      * USED TO BE ABLE BREAK DOWN, FORMAT, AND DISPLAY DATES *
           05  DATE-FORM.
               10  ATTENDANCE-YYYY PIC 9(4).
               10  ATTENDANCE-MM   PIC 99.
               10  ATTENDANCE-DD   PIC 99.
      * MAKES DATE-FORM NUMERIC *
           05  UNPACK-DATE REDEFINES DATE-FORM PIC 9(8).
      * CALCULATION VARIABLES *
           05  C-DISCOUNT          PIC 9V99    VALUE 0.
           05  C-AT-TCKTS-SALES    PIC 9(5)V99 VALUE 0.
           05  C-JR-TCKTS-SALES    PIC 9(5)V99 VALUE 0.
           05  C-SR-TCKTS-SALES    PIC 9(5)V99 VALUE 0.
           05  C-SUBTOTAL          PIC 9(6)V99 VALUE 0.
           05  C-DISCOUNT-AMT      PIC 9(5)V99 VALUE 0.
           05  C-TOTAL-COST        PIC 9(6)V99 VALUE 0.
      * MAJOR SUBTOTALS VARIABLE *
           05  C-MJ-TOTAL-SALES    PIC 9(6)V99 VALUE 0.
      * GRAND TOTALS VARIABLE *
           05  C-GT-TOTAL-SALES    PIC 9(7)V99 VALUE 0.

       01  CURRENT-DATE-AND-TIME.
           05 I-DATE.
              10  I-YYYY           PIC 9(4).
              10  I-MM             PIC 99.
              10  I-DD             PIC 99.
           05 I-TIME               PIC X(11).

      * PARK INFORMATION *
       01  PARKS-INFO.
           05  FILLER              PIC X(29)   VALUE '01ADVENTURE LAND'.
           05  FILLER              PIC X(15)   VALUE '100002000030000'.
           05  FILLER              PIC X(29)   VALUE
                                             '02THE GREATEST PARK EVER'.
           05  FILLER              PIC X(15)   VALUE '150002500099999'.
           05  FILLER              PIC X(29)   VALUE
                                        '03UNIVERSAL STUDIOS HOLLYWOOD'.
           05  FILLER              PIC X(15)   VALUE '150002500045010'.
           05  FILLER              PIC X(29)   VALUE
                                          '04UNIVERSAL STUDIOS ORLANDO'.
           05  FILLER              PIC X(15)   VALUE '125001450026789'.
           05  FILLER              PIC X(29)   VALUE '05DISNEY WORLD'.
           05  FILLER              PIC X(15)   VALUE '125001450026789'.
           05  FILLER              PIC X(29)   VALUE '06DISNEY LAND'.
           05  FILLER              PIC X(15)   VALUE '234563456745678'.
           05  FILLER              PIC X(29)   VALUE '07WORLDS OF FUN'.
           05  FILLER              PIC X(15)   VALUE '123452345634567'.
           05  FILLER              PIC X(29)   VALUE '08SIX FLAGS'.
           05  FILLER              PIC X(15)   VALUE '012341234523456'.
           05  FILLER              PIC X(29)   VALUE '09GLOBAL PARK'.
           05  FILLER              PIC X(15)   VALUE '543214321032100'.
           05  FILLER              PIC X(29)   VALUE
                                              '10WORLD WIDE PARK'.
           05  FILLER              PIC X(15)   VALUE '987658765454321'.
           05  FILLER              PIC X(29)   VALUE '11AMAZING PARK'.
           05  FILLER              PIC X(15)   VALUE '098769876587654'.
           05  FILLER              PIC X(29)   VALUE '12AMUSING PARK'.
           05  FILLER              PIC X(15)   VALUE '123214564578978'.
           05  FILLER              PIC X(29)   VALUE '13GRAND PARK'.
           05  FILLER              PIC X(15)   VALUE '321326546598798'.
           05  FILLER              PIC X(29)   VALUE '14ADVENTURE PARK'.
           05  FILLER              PIC X(15)   VALUE '111112222233333'.
           05  FILLER              PIC X(29)   VALUE '15AMUSEMENT LAND'.
           05  FILLER              PIC X(15)   VALUE '444445555566666'.

      * PARK TABLE *
       01  PARKS-TBL REDEFINES PARKS-INFO.
           05  T-PARKS             OCCURS 15.
               10  T-PARK-CODE     PIC 99.
               10  T-PARK-NAME     PIC X(27).
               10  T-PRICE         PIC 9(3)V99 OCCURS 3.

      ********************************************************
      *                 SALES REPORT LINES                   *
      ********************************************************
       01  COMPANY-TITLE.
           05  FILLER              PIC X(6)    VALUE 'DATE:'.
           05  O-MM                PIC 99.
           05  FILLER              PIC X       VALUE '/'.
           05  O-DD                PIC 99.
           05  FILLER              PIC X       VALUE '/'.
           05  O-YYYY              PIC 9(4).
           05  FILLER              PIC X(39)   VALUE ' '.
           05  FILLER              PIC X(23)   VALUE
                                              'CHAPMAN''S TICKET SALES'.
           05  FILLER              PIC X(46)   VALUE ' '.
           05  FILLER              PIC X(6)    VALUE 'PAGE:'.
           05  O-PCTR              PIC Z9.

       01  BLANK-LINE.
           05  FILLER              PIC X(132)  VALUE ' '.

       01  REPORT-TITLE.
           05  FILLER              PIC X(57)   VALUE ' '.
           05  FILLER              PIC X(17)   VALUE
                                              'DATE SALES REPORT'.

       01  COL-HDGS.
           05  FILLER              PIC X(6)    VALUE ' '.
           05  FILLER              PIC X(28)   VALUE 'PARK'.
           05  FILLER              PIC X(11)   VALUE 'DATE'.
           05  FILLER              PIC X(7)    VALUE 'ADULT'.
           05  FILLER              PIC X(8)    VALUE 'COST'.
           05  FILLER              PIC X(7)    VALUE 'JUNIOR'.
           05  FILLER              PIC X(8)    VALUE 'COST'.
           05  FILLER              PIC X(7)    VALUE 'SENIOR'.
           05  FILLER              PIC X(10)   VALUE 'COST'.
           05  FILLER              PIC X(12)   VALUE 'SUBTOTAL'.
           05  FILLER              PIC X(11)   VALUE 'DISCOUNT'.
           05  FILLER              PIC X(11)   VALUE 'TOTAL COST'.

       01  DETAIL-LINE.
           05  FILLER              PIC X(6)    VALUE ' '.
           05  O-PARK-NAME         PIC X(27).
           05  FILLER              PIC X       VALUE ' '.
           05  O-ATTENDANCE-MM     PIC 99.
           05  FILLER              PIC X       VALUE '/'.
           05  O-ATTENDANCE-DD     PIC 99.
           05  FILLER              PIC X       VALUE '/'.
           05  O-ATTENDANCE-YYYY   PIC 9(4).
           05  FILLER              PIC XXX     VALUE ' '.
      * OUTPUT PRICE TABLE *
           05  O-PRICE-TBL         OCCURS 3.
               10  O-TICKETS       PIC Z9.
               10  FILLER          PIC XXX     VALUE ' '.
               10  O-PRICE         PIC $$$9.99.
               10  FILLER          PIC XXX     VALUE ' '.
           05  O-SUBTOTAL          PIC $$$$,$$9.99.
           05  FILLER              PIC X       VALUE ' '.
           05  O-DISCOUNT-AMT      PIC $$$,$$9.99.
           05  FILLER              PIC X       VALUE ' '.
           05  O-TOTAL-COST        PIC $$$$,$$9.99.

      ********************************************************
      *                  MAJOR SUBTOTAL LINE                 *
      ********************************************************
       01  MAJORSUBTOTALS-LINE.
           05  FILLER              PIC X(6)    VALUE ' '.
           05  FILLER              PIC X(6)    VALUE 'DATE:'.
           05  O-MJ-MM             PIC 99.
           05  FILLER              PIC X       VALUE '/'.
           05  O-MJ-YYYY           PIC 9(4).
           05  FILLER              PIC X(4)    VALUE ' '.
           05  FILLER              PIC X(7)    VALUE 'TOTAL:'.
           05  O-MJ-TOTAL-SALES    PIC $$$$,$$9.99.

      ********************************************************
      *                  GRAND TOTAL LINE                    *
      ********************************************************
       01  GRANDTOTALS-LINE.
           05  FILLER              PIC X(6)    VALUE ' '.
           05  FILLER              PIC X(13)   VALUE 'GRAND TOTAL:'.
           05  O-GT-TOTAL-SALES    PIC $$,$$$,$$9.99.

       PROCEDURE DIVISION.
       L1-MAIN.
            PERFORM L2-INIT.
            PERFORM L2-MAINLINE
                UNTIL EOF = 'Y'.
            PERFORM L2-CLOSING.
            STOP RUN.

       L2-INIT.
      * OPENS FILES *
           OPEN INPUT PARKS-REC.
           OPEN OUTPUT PRTOUT.

      * GETS THE CURRENT DATE *
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.
           MOVE I-YYYY TO O-YYYY.
           MOVE I-MM TO O-MM.
           MOVE I-DD TO O-DD.

           PERFORM L9-HDGS.
           PERFORM L9-READ.

      * SETS THE HOLD VARIABLES *
           MOVE ATTENDANCE-MM TO H-MM.
           MOVE ATTENDANCE-YYYY TO H-YYYY.

       L2-MAINLINE.
      * CHECKS IF A CONTROL BREAK SHOULD OCCUR *
           IF H-MM NOT = ATTENDANCE-MM OR H-YYYY NOT = ATTENDANCE-YYYY
               PERFORM L9-MAJORSUBTOTALS.

           PERFORM L3-CALCS.
           PERFORM L3-OUTPUT.
           PERFORM L9-READ.

       L2-CLOSING.
           PERFORM L9-MAJORSUBTOTALS.

      * FORMATS GRAND TOTALS *
           MOVE C-GT-TOTAL-SALES TO O-GT-TOTAL-SALES.

      * PRINTS THE GRAND TOTALS LINE *
           WRITE PRTLINE FROM BLANK-LINE.
           WRITE PRTLINE FROM GRANDTOTALS-LINE.

      * CLOSES FILES *
           CLOSE PARKS-REC
                 PRTOUT.

       L3-CALCS.
      * DETERMINES THE DISCOUNT RATE *
           EVALUATE DISCOUNT-CODE
               WHEN 'A'
                   MOVE 0.15 TO C-DISCOUNT
               WHEN 'G'
                   MOVE 0.10 TO C-DISCOUNT
               WHEN 'W'
                   MOVE 0.08 TO C-DISCOUNT
               WHEN 'R'
                   MOVE 0.22 TO C-DISCOUNT
               WHEN OTHER
                   MOVE 0 TO C-DISCOUNT.

      * CALCULATES TICKET SALES *
           COMPUTE C-AT-TCKTS-SALES ROUNDED = T-PRICE(PARK 1)  *
               ADULT-TICKETS.
           COMPUTE C-JR-TCKTS-SALES ROUNDED = T-PRICE(PARK 2) *
               JUNIOR-TICKETS.
           COMPUTE C-SR-TCKTS-SALES ROUNDED = T-PRICE(PARK 3) *
               SENIOR-TICKETS.

      * CALCULATES THE SUBTOTAL *
           COMPUTE C-SUBTOTAL = C-AT-TCKTS-SALES + C-JR-TCKTS-SALES +
              C-SR-TCKTS-SALES.

      * CALCULATES THE DISCOUNT AMOUNT *
           COMPUTE C-DISCOUNT-AMT ROUNDED = C-DISCOUNT * C-SUBTOTAL.

      * CALCULATES THE TOTAL COST *
           SUBTRACT C-DISCOUNT-AMT FROM C-SUBTOTAL GIVING C-TOTAL-COST.

      * MAJOR SUBTOTALS CALCULATIONS *
           COMPUTE C-MJ-TOTAL-SALES = C-MJ-TOTAL-SALES + C-TOTAL-COST.

       L3-OUTPUT.
      * MOVES AND FORMATS DATA TO WRITE A SALES REPORT *
           MOVE T-PARK-NAME(PARK) TO O-PARK-NAME.
           MOVE ATTENDANCE-MM TO O-ATTENDANCE-MM.
           MOVE ATTENDANCE-DD TO O-ATTENDANCE-DD.
           MOVE ATTENDANCE-YYYY TO O-ATTENDANCE-YYYY.

      * GETS THE TICKET TYPE AND COST PER TICKET *
           MOVE ADULT-TICKETS TO O-TICKETS(1).
           MOVE T-PRICE(PARK 1) TO O-PRICE(1).
           MOVE JUNIOR-TICKETS TO O-TICKETS(2).
           MOVE T-PRICE(PARK 2) TO O-PRICE(2).
           MOVE SENIOR-TICKETS TO O-TICKETS(3).
           MOVE T-PRICE(PARK 3) TO O-PRICE(3).

           MOVE C-SUBTOTAL TO O-SUBTOTAL.
           MOVE C-DISCOUNT-AMT TO O-DISCOUNT-AMT.
           MOVE C-TOTAL-COST TO O-TOTAL-COST.

      * WRITES THE DETAIL LINE *
           WRITE PRTLINE FROM DETAIL-LINE
               AT EOP
                   PERFORM L9-HDGS.

       L9-HDGS.
      * GETS THE PAGE NUMBER *
           ADD 1 TO C-PCTR.
           MOVE C-PCTR TO O-PCTR.

      * WRITES TITLES AND HEADINGS *
           WRITE PRTLINE FROM COMPANY-TITLE
               AFTER ADVANCING PAGE.
           WRITE PRTLINE FROM REPORT-TITLE.
           WRITE PRTLINE FROM BLANK-LINE.
           WRITE PRTLINE FROM COL-HDGS.
           WRITE PRTLINE FROM BLANK-LINE.

       L9-READ.
           READ PARKS-REC
               AT END
                   MOVE 'Y' TO EOF.

      * UNPACKS ATTENDANCE DATES *
           MOVE ATTENDANCE-DATE TO UNPACK-DATE.

       L9-MAJORSUBTOTALS.
      * MOVES AND FORMATS MAJOR SUBTOTALS DATA *
           MOVE H-MM TO O-MJ-MM.
           MOVE H-YYYY TO O-MJ-YYYY.
           MOVE C-MJ-TOTAL-SALES TO O-MJ-TOTAL-SALES.

      * WRITES THE MAJOR SUBTOTALS LINE *
           WRITE PRTLINE FROM BLANK-LINE.
           WRITE PRTLINE FROM MAJORSUBTOTALS-LINE.
           WRITE PRTLINE FROM BLANK-LINE.

      * GRAND TOTAL CALCULATIONS *
           COMPUTE C-GT-TOTAL-SALES = C-GT-TOTAL-SALES +
               C-MJ-TOTAL-SALES.

      * CLEARS MAJOR SUBTOTALS DATA *
           MOVE 0 TO C-MJ-TOTAL-SALES.

      * RESETS THE HOLD VARIABLE *
           MOVE ATTENDANCE-MM TO H-MM.
           MOVE ATTENDANCE-YYYY TO H-YYYY.