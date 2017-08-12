OPTION _EXPLICIT

CONST true = -1, false = NOT true

TYPE new_item
    selected AS _BYTE
    startPos AS _UNSIGNED LONG
    length AS _UNSIGNED LONG
END TYPE

REDIM SHARED strings(1000) AS STRING
REDIM SHARED item(1000) AS new_item
DIM index AS LONG, totalSelected AS LONG, file$, line$, strAssign AS LONG, endStrAssign AS LONG
DIM thisStr$, u$, s$
DIM i AS LONG, j AS LONG, oldi AS LONG, y AS LONG
DIM k AS LONG, startPos AS _UNSIGNED LONG

PRINT "String Extractor"

DO
    INPUT "Source file: ", file$
    IF file$ = "" THEN SYSTEM
LOOP UNTIL _FILEEXISTS(file$)

PRINT "Extracting strings";

OPEN file$ FOR BINARY AS #1
DO
    IF EOF(1) THEN EXIT DO
    startPos = SEEK(1)
    LINE INPUT #1, line$

    strAssign = 0
    DO
        strAssign = INSTR(strAssign + 1, line$, CHR$(34)) '"= " + CHR$(34))
        IF strAssign = 0 THEN EXIT DO

        endStrAssign = INSTR(strAssign + 1, line$, CHR$(34))

        IF endStrAssign = strAssign + 1 THEN strAssign = strAssign + 1: endStrAssign = 0

        IF endStrAssign = 0 THEN _CONTINUE

        thisStr$ = MID$(line$, strAssign + 1, (endStrAssign - strAssign) - 1)

        IF LEN(thisStr$) THEN
            index = index + 1
            increaseArrays index

            strings(index) = thisStr$
            item(index).startPos = startPos + strAssign + 1
            item(index).length = (endStrAssign - strAssign) - 1

            strAssign = endStrAssign + 1

            PRINT ".";
        END IF
    LOOP

LOOP
CLOSE #1

IF index = 0 THEN END

u$ = STRING$(LEN(LTRIM$(STR$(index))), "#") + " "

i = 1
j = 1

DO
    oldi = i

    WHILE _MOUSEINPUT
        i = i + _MOUSEWHEEL * 5
    WEND

    IF oldi <> i THEN j = 1

    k& = _KEYHIT
    IF k& = 19200 THEN j = j + (j > 1)
    IF k& = 19712 THEN j = j + 1
    IF k& = 18432 THEN i = i + (i > 1)
    IF k& = 20480 THEN i = i - (i < index)

    IF i + (_HEIGHT - 2) > index THEN i = index - (_HEIGHT - 2)
    IF i < 1 THEN i = 1

    CLS

    FOR y = i TO index
        IF y - i + 1 > _HEIGHT - 1 THEN EXIT FOR
        LOCATE y - i + 1, 1
        COLOR 8
        IF INT(_MOUSEY + .5) = y - i + 1 OR item(y).selected THEN COLOR 15
        PRINT USING u$; y;

        COLOR 7
        IF item(y).selected THEN COLOR 15, 6 ELSE IF INT(_MOUSEY + .5) = y - i + 1 THEN COLOR 15

        IF INT(_MOUSEY + .5) = y - i + 1 THEN
            'IF item(y).selected THEN COLOR 15, 1 ELSE COLOR 0, 7
            IF _MOUSEBUTTON(1) THEN
                WHILE _MOUSEBUTTON(1): WHILE _MOUSEINPUT: WEND: WEND
                IF INT(_MOUSEY + .5) = y - i + 1 THEN
                    item(y).selected = NOT item(y).selected
                    IF item(y).selected THEN totalSelected = totalSelected + 1 ELSE totalSelected = totalSelected - 1
                END IF
            END IF
        END IF

        PRINT MID$(strings(y), j, 80 - LEN(u$));
        IF item(y).selected THEN IF POS(1) < 80 THEN PRINT SPACE$(81 - POS(1));
        COLOR , 0
    NEXT

    COLOR 14
    _PRINTSTRING (1, 25), STRING$(80, 205)
    s$ = "Total strings selected:" + STR$(totalSelected) + "/" + LTRIM$(STR$(index))
    LOCATE 25, 76 - LEN(s$)
    PRINT s$;

    COLOR 2
    IF i > 1 THEN LOCATE 1, 77: PRINT "(" + CHR$(24) + ")";
    IF i + 25 < index THEN LOCATE 25, 77: PRINT "(" + CHR$(25) + ")";

    _DISPLAY
    _LIMIT 30
LOOP

SUB increaseArrays (upperBoundary AS LONG)

    IF upperBoundary > UBOUND(item) THEN
        REDIM _PRESERVE item(LBOUND(item) TO upperBoundary) AS new_item
        REDIM _PRESERVE strings(LBOUND(item) TO upperBoundary) AS STRING
    END IF

END SUB

