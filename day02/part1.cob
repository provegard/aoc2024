IDENTIFICATION DIVISION.
PROGRAM-ID. AOC202401P1.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
       SELECT INPUT-FILE ASSIGN TO 'input'
           ORGANIZATION IS LINE SEQUENTIAL.  

DATA DIVISION.
FILE SECTION.
FD INPUT-FILE.
01 INPUT-RECORD PIC X(100).

WORKING-STORAGE SECTION.
01 WS-EOF PIC A(1) VALUE "N".     *> for end-of-file detection
01 WS-NUMBERS.
       05 WS-NUMBER OCCURS 10 TIMES PIC 9(3).
01 WS-TEMP PIC X(80).             *> temp variable used when parsing a line
01 WS-IDX PIC 9(2).
01 WS-DIFF PIC S9(3).
01 WS-SIGNLASTDIFF PIC S9(3).
01 WS-ABSDIFF PIC S9(3).
01 WS-SIGNDIFF PIC S9(3).
01 WS-SAFE PIC A(1).
01 WS-SAFECOUNT PIC 9(4) VALUE 0.

PROCEDURE DIVISION.
       OPEN INPUT INPUT-FILE

       PERFORM UNTIL WS-EOF='Y'
           READ INPUT-FILE INTO INPUT-RECORD
               AT END MOVE "Y" TO WS-EOF
           END-READ

           IF WS-EOF NOT = "Y"
               MOVE "Y" TO WS-SAFE
               PERFORM PARSE-NUMBERS

               PERFORM VARYING WS-IDX FROM 2 BY 1 UNTIL WS-IDX > 10
                   IF WS-NUMBER(WS-IDX) NOT = 0
                       SUBTRACT WS-NUMBER(WS-IDX - 1) FROM WS-NUMBER(WS-IDX) GIVING WS-DIFF
                       COMPUTE WS-ABSDIFF = FUNCTION ABS(WS-DIFF)
                       COMPUTE WS-SIGNDIFF = FUNCTION SIGN(WS-DIFF)
                       
                       IF (WS-ABSDIFF < 1) OR (WS-ABSDIFF > 3)
                           *> Diff is too large
                           MOVE "N" TO WS-SAFE
                       END-IF
                       IF (WS-IDX > 2) AND (WS-SIGNDIFF <> WS-SIGNLASTDIFF)
                           *> Not all increasing or decreasing
                           MOVE "N" TO WS-SAFE
                       END-IF

                       MOVE WS-SIGNDIFF TO WS-SIGNLASTDIFF
                   END-IF
               END-PERFORM

               IF WS-SAFE = "Y"
                   ADD 1 TO WS-SAFECOUNT
               END-IF
           END-IF
       END-PERFORM

       DISPLAY "SAFE: " WS-SAFECOUNT

       CLOSE INPUT-FILE

       STOP RUN.

PARSE-NUMBERS.
       *> Clear the array first. There are no zero numbers in the input,
       *> so we can use 0 as an end-of-list indicator.
       PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 10
           MOVE 0 TO WS-NUMBER(WS-IDX)
       END-PERFORM
       MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TEMP
       UNSTRING WS-TEMP
           DELIMITED BY ALL SPACES
           INTO WS-NUMBER(1), WS-NUMBER(2), WS-NUMBER(3), WS-NUMBER(4), WS-NUMBER(5), WS-NUMBER(6), WS-NUMBER(7), WS-NUMBER(8), WS-NUMBER(9), WS-NUMBER(10).
