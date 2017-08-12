REDIM strings(1000) AS STRING
DIM index AS LONG

PRINT "String Extractor"

DO
    INPUT "Source file: ", file$
    IF file$ = "" THEN SYSTEM
LOOP UNTIL _FILEEXISTS(file$)

PRINT "Extracting strings";

OPEN file$ FOR BINARY AS #1
DO
    IF EOF(1) THEN EXIT DO
    LINE INPUT #1, line$

    strAssign = 0
    DO
        strAssign = INSTR(strAssign + 1, line$, "= " + CHR$(34))
        IF strAssign = 0 THEN EXIT DO

        endStrAssign = INSTR(strAssign + 3, line$, CHR$(34))
        IF endStrAssign = 0 THEN _CONTINUE

        thisStr$ = MID$(line$, strAssign + 3, (endStrAssign - strAssign) - 3)
        strAssign = endStrAssign + 1

        IF LEN(thisStr$) THEN
            index = index + 1
            increaseArray strings(), index

            strings(index) = thisStr$

            PRINT ".";
        END IF
    LOOP

LOOP
CLOSE #1

IF index = 0 THEN END

u$ = STRING$(LEN(LTRIM$(STR$(index))), "#") + " "
s$ = "Total strings found:" + STR$(index)

DIM i AS LONG, y AS LONG
i = 1
j = 1

DO
    oldi = i

    WHILE _MOUSEINPUT
        i = i + _MOUSEWHEEL * 5
        IF i < 1 THEN i = 1
        IF i > index THEN i = index
    WEND

    IF oldi <> i THEN j = 1

    k& = _KEYHIT
    IF k& = 19200 THEN j = j + (j > 1)
    IF k& = 19712 THEN j = j + 1
    IF k& = 18432 THEN i = i + (i > 1)
    IF k& = 20480 THEN i = i - (i < index)

    CLS

    FOR y = i TO index
        IF y - i + 1 > _HEIGHT - 1 THEN EXIT FOR
        LOCATE y - i + 1, 1
        COLOR 8
        PRINT USING u$; y;
        COLOR 15
        PRINT MID$(strings(y), j, 80 - LEN(u$));
    NEXT

    COLOR 14
    LOCATE 25, 76 - LEN(s$)
    PRINT s$;

    COLOR 2
    IF i > 1 THEN LOCATE 1, 77: PRINT "(" + CHR$(24) + ")";
    IF i + 25 < index THEN LOCATE 25, 77: PRINT "(" + CHR$(25) + ")";

    _DISPLAY

LOOP

SUB increaseArray (array$(), upperBoundary AS LONG)

    IF upperBoundary > UBOUND(array$) THEN
        REDIM _PRESERVE array$(LBOUND(array$) TO upperBoundary)
    END IF

END SUB
