       IDENTIFICATION DIVISION.
       PROGRAM-ID.              COBLSC00.
       AUTHOR.                  LUCAS CHAPMAN.
       DATE-WRITTEN.            12/30/2020.
       DATE-COMPILED.

      *****************************************************************
      *                                                               *
      *  INSTRUCTOR       WILISON                                     *
      *  CASE PROBLEM     COBLSC00                                    *
      *                                                               *
      *  THIS PROGRAM WILL DEMONSTRATE A BASIC COBOL APPLICATION WITH *
      *  BOTH INPUT AND OUTPUT FILES USING THE MAINFRAME.             *
      *                                                               *
      *****************************************************************

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT GRADE-FILE
               ASSIGN TO GRADESIN
               ORGANIZATION IS SEQUENTIAL.

           SELECT PRTOUT
               ASSIGN TO GRADEOUT
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  GRADE-FILE
           LABEL RECORD IS STANDARD
           DATA RECORD IS STD-RECORD
           RECORDING MODE IS F
           RECORD CONTAINS 31 CHARACTERS.

       01  STD-RECORD.
           05  STD-COURSE-ID            PIC X(8).
           05  STD-ID                   PIC 9(7).
           05  STD-ASSIGNMENT           PIC X(10).
           05  STD-POINTS-POSS          PIC 999.
           05  STD-POINTS-EARNED        PIC 999.

       FD  PRTOUT
           LABEL RECORD IS OMITTED
           RECORD CONTAINS 132 CHARACTERS
           DATA RECORD IS PRTLINE
           RECORDING MODE IS F
           LINAGE IS 60 WITH FOOTING AT 56.

       01  PRTLINE                      PIC X(132).

       WORKING-STORAGE SECTION.
       01  WORK-AREAS.
           05  MORE-RECS                PIC XXX    VALUE 'YES'.
           05  PCTR                     PIC 99     VALUE 0.
           05  C-PERCENTAGE             PIC 9V99   VALUE 0.

       01  CURRENT-DATE-AND-TIME.
           05  THIS-DATE.
               10  CURRENT-YEAR         PIC X(4).
               10  CURRENT-MONTH        PIC XX.
               10  CURRENT-DAY          PIC XX.
           05  THIS-TIME                PIC X(11).

       01  TITLE-LINE.
           05  FILLER                   PIC X(6)   VALUE 'DATE:'.
           05  O-CURRENT-MONTH          PIC XX.
           05  FILLER                   PIC X      VALUE '/'.
           05  O-CURRENT-DAY            PIC XX.
           05  FILLER                   PIC X      VALUE '/'.
           05  O-CURRENT-YEAR           PIC X(4).
           05  FILLER                   PIC X(40)  VALUE ' '.
           05  FILLER                   PIC X(20)
               VALUE 'STUDENT GRADE REPORT'.
           05  FILLER                   PIC X(48)  VALUE ' '.
           05  FILLER                   PIC X(6)   VALUE 'PAGE:'.
           05  O-PCTR                   PIC Z9.

       01  HDG-LINE.
           05  FILLER                   PIC XX     VALUE ' '.
           05  FILLER                   PIC XX     VALUE 'ID'.
           05  FILLER                   PIC X(38)  VALUE ' '.
           05  FILLER                   PIC X(15)  VALUE 'ASSIGNMENT'.
           05  FILLER                   PIC X(35)  VALUE ' '.
           05  FILLER                   PIC X(15)  VALUE 'PERCENTAGE'.
           05  FILLER                   PIC X(25)  VALUE ' '.

       01  DETAIL-LINE.
           05  O-STD-ID                 PIC X(8).
           05  FILLER                   PIC X(35)  VALUE ' '.
           05  O-STD-ASSIGNMENT         PIC X(10).
           05  FILLER                   PIC X(40)  VALUE ' '.
           05  O-STD-PERCENTAGE         PIC Z.99.
           05  FILLER                   PIC X(35)  VALUE ' '.


       PROCEDURE DIVISION.

       0000-COBLSC00.
           PERFORM 1000-INIT.
           PERFORM 2000-MAINLINE
               UNTIL MORE-RECS = 'NO'.
           PERFORM 3000-EOJ.
           STOP RUN.

      *********************************************
      *     INITIALIZATION ROUTINE                *
      *********************************************

       1000-INIT.
           PERFORM 1100-DATE-ROUT.
           PERFORM 1200-OPEN-ROUT.
           PERFORM 9000-HDG-ROUT.
           PERFORM 9100-READ-GRADES.

       1100-DATE-ROUT.
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.
           MOVE CURRENT-MONTH TO O-CURRENT-MONTH.
           MOVE CURRENT-DAY TO O-CURRENT-DAY.
           MOVE CURRENT-YEAR TO O-CURRENT-YEAR.

       1200-OPEN-ROUT.
           OPEN INPUT GRADE-FILE.
           OPEN OUTPUT PRTOUT.

       2000-MAINLINE.
           PERFORM 2100-CALCS.
           PERFORM 2200-MOVE-DETAIL.
           PERFORM 9100-READ-GRADES.

       2100-CALCS.
           DIVIDE STD-POINTS-EARNED BY STD-POINTS-POSS
               GIVING C-PERCENTAGE.

       2200-MOVE-DETAIL.
           MOVE STD-ID TO O-STD-ID.
           MOVE STD-ASSIGNMENT TO O-STD-ASSIGNMENT.
           MOVE C-PERCENTAGE TO O-STD-PERCENTAGE.

           WRITE PRTLINE FROM DETAIL-LINE
               AFTER ADVANCING 2 LINES
                   AT EOP
                       PERFORM 9000-HDG-ROUT.

       3000-EOJ.
           CLOSE GRADE-FILE
                 PRTOUT.

       9000-HDG-ROUT.
           ADD 1 TO PCTR.
           MOVE PCTR TO O-PCTR.
           WRITE PRTLINE FROM TITLE-LINE
               AFTER ADVANCING PAGE.
           WRITE PRTLINE FROM HDG-LINE
               AFTER ADVANCING 2 LINES.

       9100-READ-GRADES.
           READ GRADE-FILE
               AT END
                   MOVE 'NO' TO MORE-RECS.