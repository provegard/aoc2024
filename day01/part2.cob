IDENTIFICATION DIVISION.
PROGRAM-ID. AOC202401P2.

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
01 WS-NUMBER1 PIC 9(10) VALUE 0.  *> stores the first number of each line
01 WS-NUMBER2 PIC 9(10) VALUE 0.  *> stores the second number of each line
01 WS-TEMP PIC X(80).             *> temp variable used when parsing a line
01 WS-FREQ.                       *> an array of 100000 numbers, used as a frequency table
       05 WS-FREQ-TABLE PIC 9(10) VALUE 0 OCCURS 100000 TIMES.

01 NUMBERS1.                      *> an array of 1000 numbers
       05 NUMBERS1-TABLE PIC 9(10) OCCURS 1000 TIMES.
01 NUMBERS2.                      *> an array of 1000 numbers
       05 NUMBERS2-TABLE PIC 9(10) OCCURS 1000 TIMES.
01 WS-INDEX PIC 9(10) VALUE 1.    *> 1-based index that effectively contains the size of the NUMBERSX arrays
01 I PIC 9(10).
01 WS-PRODUCT PIC S9(10).
01 WS-SCORE PIC 9(10) VALUE 0.
01 WS-F PIC 9(10) VALUE 0.         *> temp variable to hold the frequency of a number

PROCEDURE DIVISION.
       OPEN INPUT INPUT-FILE

       PERFORM UNTIL WS-EOF='Y'
           READ INPUT-FILE INTO INPUT-RECORD
               AT END MOVE "Y" TO WS-EOF
           END-READ

           IF WS-EOF NOT = "Y"
               PERFORM PARSE-NUMBERS
               MOVE WS-NUMBER1 TO NUMBERS1-TABLE(WS-INDEX)
               MOVE WS-NUMBER2 TO NUMBERS2-TABLE(WS-INDEX)
               ADD 1 TO WS-INDEX

               *> Update the frequency table as well.
               ADD 1 TO WS-FREQ-TABLE(WS-NUMBER2)
           END-IF
       END-PERFORM

       CLOSE INPUT-FILE

       PERFORM VARYING I FROM 1 BY 1 UNTIL I > WS-INDEX - 1
           *> Get the frequency of the 2nd number
           MOVE WS-FREQ-TABLE(NUMBERS1-TABLE(I)) TO WS-F
           MULTIPLY NUMBERS1-TABLE(I) BY WS-F GIVING WS-PRODUCT
           ADD WS-PRODUCT TO WS-SCORE
       END-PERFORM

       DISPLAY "Score:"
       DISPLAY WS-SCORE

       STOP RUN.

PARSE-NUMBERS.
       MOVE FUNCTION TRIM(INPUT-RECORD) TO WS-TEMP
       UNSTRING WS-TEMP
           DELIMITED BY ALL SPACES
           INTO WS-NUMBER1 WS-NUMBER2.
