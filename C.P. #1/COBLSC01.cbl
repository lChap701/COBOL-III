       IDENTIFICATION DIVISION.
       PROGRAM-ID.    COBLSC01.
       AUTHOR.        LUCAS CHAPMAN.
       DATE-WRITTEN.  12/6/2020.
       DATE-COMPILED.
      **************************************************
      *                                                *
      * WRITES A DETAIL AND SUMMARY REPORT FOR HUNTING *
      * LICENSES.                                      *
      *                                                *
      **************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT LICENSES-REC
               ASSIGN TO SORTOUT
               ORGANIZATION IS SEQUENTIAL.

           SELECT PRTOUT
               ASSIGN TO LICREPT
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  LICENSES-REC
           RECORDING MODE IS V
           LABEL RECORD IS STANDARD
           RECORD CONTAINS 32 TO 37 CHARACTERS
           DATA RECORDS ARE H-DETAIL-REC, H-LICENSE-REC.

       01  H-DETAIL-REC.
           05  FILLER              PIC X(10).
           05  H-CUST-BDAY         PIC 9(8)    COMP-3.
           05  FILLER              PIC X(14).
           05  H-FEE               PIC 9(3)V99 COMP-3.

       01  H-LICENSE-REC.
           05  R-TYPE              PIC X.
           05  FILLER              PIC X(36).

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
      * SUBSCRIPT FOR THE HEADER RECORDS TABLE *
           05  LICENSE-SUB         PIC 99      VALUE 0 COMP.
      * KEEPS TRACK OF THE REPORT THAT SHOULD BE PRINTED *
           05  REPORT-CHECK        PIC X       VALUE 'D'.
      * GRAND TOTAL VARIABLES *
           05  C-D-GT-SOLD-CTR     PIC 99      VALUE 0.
           05  C-GT-FEE            PIC 9(5)V99 VALUE 0.
           05  C-GT-RES-CTR        PIC 9(4)    VALUE 0.
           05  C-GT-NRES-CTR       PIC 9(4)    VALUE 0.
           05  C-S-GT-SOLD-CTR     PIC 9(4)    VALUE 0.
           05  C-GT-SALES          PIC 9(7)V99 VALUE 0.
      * USED TO BE ABLE TO FORMAT AND DISPLAY FEES *
           05  UNPACK-FEE          PIC 9(3)V99.
      * USED TO BE ABLE BREAK DOWN, FORMAT, AND DISPLAY BIRTHDAYS *
           05  UNPACK-BDAY-FORM.
               10  UNPACK-YYYY     PIC 9(4).
               10  UNPACK-MM       PIC 99.
               10  UNPACK-DD       PIC 99.
      * MAKES UNPACK-BDAY-FORM NUMERIC *
           05  UNPACK-BDAY REDEFINES UNPACK-BDAY-FORM  PIC 9(8).

       01  CURRENT-DATE-AND-TIME.
           05  I-DATE.
               10  I-YYYY          PIC 9(4).
               10  I-MM            PIC 99.
               10  I-DD            PIC 99.
           05  I-TIME              PIC X(11).

      * HEADER RECORD LAYOUT *
       01  LICENSE-REC.
           05  REC-TYPE            PIC X.
           05  HDR-REC.
               10  L-DESC          PIC X(34).
               10  L-TYPE          PIC 99.

      * DETAIL RECORD LAYOUT *
       01  DETAIL-REC.
           05  FILLER              PIC X.
           05  D-CUSTID            PIC X(9).
           05  D-CUST-BDAY         PIC 9(8)    COMP-3.
           05  D-CUST-SAFETY-NO    PIC X(10).
           05  D-L-TYPE            PIC 99.
           05  D-SEASON            PIC 9.
           05  D-RESIDENT          PIC X.
           05  D-FEE               PIC 9(3)V99 COMP-3.

      * HEADER RECORDS TABLE *
       01  HDR-TBL.
           05  T-HDR-REC           OCCURS 15.
               10  T-REC-TYPE      PIC X       VALUE ' '.
               10  T-DESC          PIC X(34)   VALUE ' '.
               10  T-TYPE          PIC 99      VALUE 0.
           05  T-SUMMARY           OCCURS 15.
               10  T-RES-CTR       PIC 99      VALUE 0.
               10  T-NRES-CTR      PIC 99      VALUE 0.
               10  T-SOLD-CTR      PIC 99      VALUE 0.
               10  T-ACC-SALES     PIC 9(5)V99 VALUE 0.

       01  COMPANY-TITLE.
           05  FILLER              PIC X(6)    VALUE 'DATE: '.
           05  O-MM                PIC 99.
           05  FILLER              PIC X       VALUE '/'.
           05  O-DD                PIC 99.
           05  FILLER              PIC X       VALUE '/'.
           05  O-YYYY              PIC 9(4).
           05  FILLER              PIC X(42)   VALUE ' '.
           05  FILLER              PIC X(16)   VALUE 'HUNTING LICENSES'.
           05  FILLER              PIC X(50)   VALUE ' '.
           05  FILLER              PIC X(6)    VALUE 'PAGE: '.
           05  O-PCTR              PIC Z9.

      *************************************************
      *             DETAIL REPORT LINES               *
      *************************************************
       01  D-REPORT-TITLE.
           05  FILLER              PIC X(55)   VALUE ' '.
           05  FILLER              PIC X(21)   VALUE
                                              'LUCAS''S DETAIL REPORT'.
           05  FILLER              PIC X(54)   VALUE ' '.

       01  D-REPORT-HDGS.
           05  FILLER              PIC X(7)    VALUE ' '.
           05  FILLER              PIC X(11)   VALUE 'CUSTOMER ID'.
           05  FILLER              PIC X(6)    VALUE ' '.
           05  FILLER              PIC X(8)    VALUE 'BIRTHDAY'.
           05  FILLER              PIC X(6)    VALUE ' '.
           05  FILLER              PIC X(10)   VALUE 'SAFTEY NO.'.
           05  FILLER              PIC X(15)   VALUE ' '.
           05  FILLER              PIC X(12)   VALUE 'LICENSE TYPE'.
           05  FILLER              PIC X(15)   VALUE ' '.
           05  FILLER              PIC X(6)    VALUE 'SEASON'.
           05  FILLER              PIC X(5)    VALUE ' '.
           05  FILLER              PIC X(13)   VALUE 'CUSTOMER TYPE'.
           05  FILLER              PIC X(7)    VALUE ' '.
           05  FILLER              PIC X(4)    VALUE 'FEES'.
           05  FILLER              PIC X(7)    VALUE ' '.

       01  BLANK-LINE.
           05  FILLER              PIC X(132)  VALUE ' '.

       01  D-DETAIL-LINE.
           05  FILLER              PIC X(8)    VALUE ' '.
           05  O-CUSTID            PIC X(9).
           05  FILLER              PIC X(6)    VALUE ' '.
           05  O-BDAY-MM           PIC 99.
           05  FILLER              PIC X       VALUE '/'.
           05  O-BDAY-DD           PIC 99.
           05  FILLER              PIC X       VALUE '/'.
           05  O-BDAY-YYYY         PIC 9(4).
           05  FILLER              PIC X(5)    VALUE ' '.
           05  O-CUST-SAFETY-NO    PIC X(10).
           05  FILLER              PIC X(4)    VALUE ' '.
           05  O-D-DESC            PIC X(34).
           05  FILLER              PIC X(7)    VALUE ' '.
           05  O-SEASON            PIC 9.
           05  FILLER              PIC X(7)    VALUE ' '.
           05  O-RESIDENT          PIC X(12)   VALUE ' '.
           05  FILLER              PIC X(8)    VALUE ' '.
           05  O-FEE               PIC $$$9.99.
           05  FILLER              PIC X(4)    VALUE ' '.

      *************************************************
      *                GRAND TOTALS LINES             *
      *************************************************
       01  D-GRAND-TOTALS-HDG.
           05  FILLER              PIC X(8)    VALUE ' '.
           05  FILLER              PIC X(13)   VALUE 'GRAND TOTALS:'.
           05  FILLER              PIC X(111)  VALUE ' '.

       01  D-GRAND-TOTALS-LINE.
           05  FILLER              PIC X(8)    VALUE ' '.
           05  FILLER              PIC X(15)   VALUE 'LICENSES SOLD: '.
           05  O-D-GT-SOLD-CTR     PIC Z9.
           05  FILLER              PIC X(4)    VALUE ' '.
           05  FILLER              PIC X(13)   VALUE 'TOTAL SALES: '.
           05  O-GT-FEE            PIC $$$,$$9.99.
           05  FILLER              PIC X(80)   VALUE ' '.

      *************************************************
      *              SUMMARY REPORT LINES             *
      *************************************************
       01  S-REPORT-TITLE.
           05  FILLER              PIC X(55)   VALUE ' '.
           05  FILLER              PIC X(22)   VALUE
                                              'LUCAS''S SUMMARY REPORT'.
           05  FILLER              PIC X(55)   VALUE ' '.

       01  S-REPORT-HDGS.
           05  FILLER              PIC X(14)   VALUE ' '.
           05  FILLER              PIC X(12)   VALUE 'LICENSE TYPE'.
           05  FILLER              PIC X(17)   VALUE ' '.
           05  FILLER              PIC X(18)   VALUE
                                               'RESIDENT PURCHASES'.
           05  FILLER              PIC X(6)    VALUE ' '.
           05  FILLER              PIC X(22)   VALUE
                                               'NON-RESIDENT PURCHASES'.
           05  FILLER              PIC X(6)    VALUE ' '.
           05  FILLER              PIC X(13)   VALUE 'LICENSES SOLD'.
           05  FILLER              PIC X(10)   VALUE ' '.
           05  FILLER              PIC X(11)   VALUE 'TOTAL SALES'.
           05  FILLER              PIC XXX     VALUE ' '.

       01  S-DETAIL-LINE.
           05  FILLER              PIC XXX     VALUE ' '.
           05  O-S-DESC            PIC X(34).
           05  FILLER              PIC X(14)   VALUE ' '.
           05  O-RES-CTR           PIC Z9.
           05  FILLER              PIC X(24)   VALUE ' '.
           05  O-NRES-CTR          PIC Z9.
           05  FILLER              PIC X(21)   VALUE ' '.
           05  O-SOLD-CTR          PIC Z9.
           05  FILLER              PIC X(17)   VALUE ' '.
           05  O-ACC-SALES         PIC $$$,$$9.99.
           05  FILLER              PIC XXX     VALUE ' '.

      *************************************************
      *               GRAND TOTALS LINE               *
      *************************************************
       01  S-GRAND-TOTALS-LINE.
           05  FILLER              PIC XXX     VALUE ' '.
           05  FILLER              PIC X(13)   VALUE 'GRAND TOTALS:'.
           05  FILLER              PIC X(26)   VALUE ' '.
           05  FILLER              PIC X(6)    VALUE 'SOLD: '.
           05  O-GT-RES-CTR        PIC Z,ZZ9.
           05  FILLER              PIC X(15)   VALUE ' '.
           05  FILLER              PIC X(6)    VALUE 'SOLD: '.
           05  O-GT-NRES-CTR       PIC Z,ZZ9.
           05  FILLER              PIC X(12)   VALUE ' '.
           05  FILLER              PIC X(6)    VALUE 'SOLD: '.
           05  O-S-GT-SOLD-CTR     PIC Z,ZZ9.
           05  FILLER              PIC X(7)    VALUE ' '.
           05  FILLER              PIC X(7)    VALUE 'SALES: '.
           05  O-GT-SALES          PIC $$,$$$,$$9.99.
           05  FILLER              PIC XXX     VALUE ' '.

       PROCEDURE DIVISION.
       L1-MAIN.
           PERFORM L2-INIT.
           PERFORM L2-MAINLINE
               UNTIL EOF = 'Y'.
           PERFORM L2-CLOSING.
           STOP RUN.

       L2-INIT.
      * OPENS FILES *
           OPEN INPUT LICENSES-REC.
           OPEN OUTPUT PRTOUT.

      * GETS THE CURRENT DATE *
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.
           MOVE I-YYYY TO O-YYYY.
           MOVE I-MM TO O-MM.
           MOVE I-DD TO O-DD.

      * SETS UP THE HEADER RECORDS TABLE *
           INITIALIZE HDR-TBL.

           PERFORM L9-HDGS.
           PERFORM L9-READ.

       L2-MAINLINE.
      * CHECKS WHAT ACTIONS SHOULD BE TAKEN BASED ON R-TYPE *
           IF R-TYPE = 'H'
               MOVE L-DESC TO T-DESC(L-TYPE)
			   MOVE L-TYPE TO T-TYPE(L-TYPE)
			   MOVE REC-TYPE TO T-REC-TYPE(L-TYPE)
           ELSE
               IF R-TYPE = 'D'
                   PERFORM L3-CALCS
                   PERFORM L3-OUTPUT.

           PERFORM L9-READ.

       L2-CLOSING.
      * MOVE AND FORMATS DATA TO FINISH PRINTING A DETAIL REPORT *
           MOVE C-GT-FEE TO O-GT-FEE.
           MOVE C-D-GT-SOLD-CTR TO O-D-GT-SOLD-CTR.

      * PRINTS THE GRAND TOTALS IN THE DETAIL REPORT *
           WRITE PRTLINE FROM BLANK-LINE
               AFTER ADVANCING 1 LINE.
           WRITE PRTLINE FROM BLANK-LINE
               AFTER ADVANCING 1 LINE.
           WRITE PRTLINE FROM D-GRAND-TOTALS-HDG
               AFTER ADVANCING 1 LINE.
           WRITE PRTLINE FROM BLANK-LINE
               AFTER ADVANCING 1 LINE.
           WRITE PRTLINE FROM D-GRAND-TOTALS-LINE
               AFTER ADVANCING 1 LINE.

      * DETERMINES THAT A SUMMARY REPORT WILL BE PRINTED *
           MOVE 'S' TO REPORT-CHECK.

      * RESETS PAGE COUNTER *
           MOVE 0 TO C-PCTR.

      * PRINTS SUMMARY REPORT HEADINGS AND TITLE LINE *
           PERFORM L9-HDGS.

      * CALCULATES GRAND TOTALS AND PRINTS DETAIL LINES IN THE SUMMARY *
      * REPORT USING THE LAST LICENSE TYPE AS A REFERENCE              *
           PERFORM L4-SUMMARY-REPORT
               VARYING LICENSE-SUB FROM 1 BY 1
                   UNTIL LICENSE-SUB > D-L-TYPE.

      * MOVES AND FORMATS DATA TO FINISH PRINTING A SUMMARY REPORT *
           MOVE C-GT-RES-CTR TO O-GT-RES-CTR.
           MOVE C-GT-NRES-CTR TO O-GT-NRES-CTR.
           MOVE C-S-GT-SOLD-CTR TO O-S-GT-SOLD-CTR.
           MOVE C-GT-SALES TO O-GT-SALES.

      * PRINTS THE GRAND TOTALS LINE IN THE SUMMARY REPORT *
           WRITE PRTLINE FROM BLANK-LINE
               AFTER ADVANCING 1 LINE.
           WRITE PRTLINE FROM BLANK-LINE
               AFTER ADVANCING 1 LINE.
           WRITE PRTLINE FROM S-GRAND-TOTALS-LINE
               AFTER ADVANCING 1 LINE.

      * CLOSES FILES *
           CLOSE LICENSES-REC
                 PRTOUT.

       L3-CALCS.
      * DETAIL REPORT CALCULATIONS *
           ADD 1 TO C-D-GT-SOLD-CTR.
           COMPUTE C-GT-FEE = C-GT-FEE + D-FEE.

      * SUMMARY REPORT CALCULATIONS *
      * ADDS TO A CTR FOR BOTH RESIDENTS AND NON-RESIDENTS *
           IF D-RESIDENT = 'R'
               ADD 1 TO T-RES-CTR(D-L-TYPE)
           ELSE
               IF D-RESIDENT = 'N'
                   ADD 1 TO T-NRES-CTR(D-L-TYPE).

           ADD 1 TO T-SOLD-CTR(D-L-TYPE).
           COMPUTE T-ACC-SALES(D-L-TYPE) = T-ACC-SALES(D-L-TYPE) +
               D-FEE.

       L3-OUTPUT.
      * MOVES AND FORMATS DATA TO PRINT A DETAIL REPORT *
           MOVE D-CUSTID TO O-CUSTID.

      * UNPACKS AND FORMATS BIRTHDAYS *
           MOVE D-CUST-BDAY TO UNPACK-BDAY.
           MOVE UNPACK-MM TO O-BDAY-MM.
           MOVE UNPACK-DD TO O-BDAY-DD.
           MOVE UNPACK-YYYY TO O-BDAY-YYYY.

           MOVE D-CUST-SAFETY-NO TO O-CUST-SAFETY-NO.
           MOVE T-DESC(D-L-TYPE) TO O-D-DESC.
           MOVE D-SEASON TO O-SEASON.

      * FINDS THE CORRECT FULL WORD TO USE *
           IF D-RESIDENT = 'R'
               MOVE 'RESIDENT' TO O-RESIDENT
           ELSE
               IF D-RESIDENT = 'N'
                   MOVE 'NON-RESIDENT' TO O-RESIDENT.

      * UNPACKS AND FORMATS FEES *
           MOVE D-FEE TO UNPACK-FEE.
           MOVE UNPACK-FEE TO O-FEE.

      * WRITES THE DETAIL LINES IN THE DETAIL REPORT *
           WRITE PRTLINE FROM D-DETAIL-LINE
               AFTER ADVANCING 1 LINE
                   AT EOP
                       PERFORM L9-HDGS.

       L4-SUMMARY-REPORT.
      * CALCULATES GRAND TOTALS *
           COMPUTE C-GT-RES-CTR = C-GT-RES-CTR + T-RES-CTR(LICENSE-SUB).
           COMPUTE C-GT-NRES-CTR = C-GT-NRES-CTR +
               T-NRES-CTR(LICENSE-SUB).
           COMPUTE C-S-GT-SOLD-CTR = C-S-GT-SOLD-CTR +
               T-SOLD-CTR(LICENSE-SUB).
           COMPUTE C-GT-SALES = C-GT-SALES + T-ACC-SALES(LICENSE-SUB).

      * MOVES AND FORMATS DATA TO PRINT A SUMMARY REPORT *
           MOVE T-DESC(LICENSE-SUB) TO O-S-DESC.
           MOVE T-RES-CTR(LICENSE-SUB) TO O-RES-CTR.
           MOVE T-NRES-CTR(LICENSE-SUB) TO O-NRES-CTR.
           MOVE T-SOLD-CTR(LICENSE-SUB) TO O-SOLD-CTR.
           MOVE T-ACC-SALES(LICENSE-SUB) TO O-ACC-SALES.

      * WRITES DETAIL LINES IN THE SUMMARY REPORT *
           WRITE PRTLINE FROM BLANK-LINE
               AFTER ADVANCING 1 LINE.
           WRITE PRTLINE FROM S-DETAIL-LINE
               AFTER ADVANCING 1 LINE
                   AT EOP
                       PERFORM L9-HDGS.

       L9-HDGS.
      * ADDS PAGE NUMBERS *
           ADD 1 TO C-PCTR.
           MOVE C-PCTR TO O-PCTR.

      * WRITES THE TITLE OF ALL REPORTS *
           WRITE PRTLINE FROM COMPANY-TITLE
               AFTER ADVANCING PAGE.

      * CHECKS WHICH REPORT IS BEING WORKED ON TO DETERMINE WHAT *
      * REPORT TITLE AND HEADINGS TO PRINT                       *
           IF REPORT-CHECK = 'D'
               WRITE PRTLINE FROM D-REPORT-TITLE
                   AFTER ADVANCING 1 LINE
               WRITE PRTLINE FROM BLANK-LINE
                   AFTER ADVANCING 1 LINE
               WRITE PRTLINE FROM D-REPORT-HDGS
                   AFTER ADVANCING 1 LINE
               WRITE PRTLINE FROM BLANK-LINE
                   AFTER ADVANCING 1 LINE
           ELSE
               IF REPORT-CHECK = 'S'
                   WRITE PRTLINE FROM S-REPORT-TITLE
                       AFTER ADVANCING 1 LINE
                   WRITE PRTLINE FROM BLANK-LINE
                       AFTER ADVANCING 1 LINE
                   WRITE PRTLINE FROM S-REPORT-HDGS
                       AFTER ADVANCING 1 LINE.

       L9-READ.
      * READ THE SRTHUNT.DAT FILE *
           READ LICENSES-REC
               AT END
                   MOVE 'Y' TO EOF.

      * MOVES RECORDS TO RECORD LAYOUTS BASED ON R-TYPE *
           IF R-TYPE = 'H'
               MOVE H-LICENSE-REC TO LICENSE-REC
           ELSE
               IF R-TYPE = 'D'
                   MOVE H-DETAIL-REC TO DETAIL-REC.