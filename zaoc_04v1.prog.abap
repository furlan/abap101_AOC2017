*&---------------------------------------------------------------------*
*& Report zaoc_04v1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zaoc_04v1.

TYPES phrases_type TYPE TABLE OF string.

CLASS solver DEFINITION.
  PUBLIC SECTION.
    METHODS solve IMPORTING phrases_input TYPE phrases_type
                  RETURNING VALUE(return) TYPE i.
  PRIVATE SECTION.
    METHODS check_unique_words IMPORTING phrase        TYPE string
                               RETURNING VALUE(return) TYPE i.
ENDCLASS.

CLASS solver IMPLEMENTATION.
  METHOD solve.
    FIELD-SYMBOLS <phrase> TYPE string.
    LOOP AT phrases_input ASSIGNING <phrase>.
      DATA(is_unique) = me->check_unique_words( <phrase> ).
      ADD is_unique TO return.
    ENDLOOP.
  ENDMETHOD.

  METHOD check_unique_words.
    SPLIT phrase AT ' ' INTO TABLE DATA(words).
    DATA(lines_count) = lines( words ).
    SORT words BY table_line ASCENDING.
    DELETE ADJACENT DUPLICATES FROM words.
    DATA(lines_count_delete) = lines( words ).

    IF lines_count = lines_count_delete.
      return = 1.
    ELSE.
      return = 0.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS test_solver DEFINITION FOR TESTING RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA testing_solve TYPE REF TO solver.
    METHODS setup.
    METHODS return_2 FOR TESTING.

ENDCLASS.

CLASS test_solver IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT me->testing_solve.
  ENDMETHOD.

  METHOD return_2.
    DATA input_table TYPE phrases_type.
    DATA word TYPE string.
    word = 'aa bb cc dd ee'.
    APPEND word TO input_table.
    word = 'aa bb cc dd aa'.
    APPEND word TO input_table.
    word = 'aa bb cc dd aaa'.
    APPEND word TO input_table.

    DATA(actual) = me->testing_solve->solve( input_table ).
    cl_abap_unit_assert=>assert_equals( act = actual exp = 2 ).

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  DATA raw_table TYPE TABLE OF string.
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename = '/Users/flavio/work/aoc2017/input_day_4.txt'
      filetype = 'ASC'
    TABLES
      data_tab = raw_table.

  DATA(use_solver) = NEW solver( ).
  DATA(count) = use_solver->solve( raw_table ).
  WRITE count.
