*&---------------------------------------------------------------------*
*& Report zaoc_04av1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zaoc_04av1.

TYPES phrases_type TYPE TABLE OF string.

CLASS solver DEFINITION.
  PUBLIC SECTION.
    METHODS solve IMPORTING phrases_input TYPE phrases_type
                  RETURNING VALUE(return) TYPE i.
  PRIVATE SECTION.
    METHODS has_not_anagram IMPORTING phrase        TYPE string
                            RETURNING VALUE(return) TYPE i.

    METHODS get_product IMPORTING word          TYPE string
                        RETURNING VALUE(return) TYPE int8.
ENDCLASS.

CLASS solver IMPLEMENTATION.
  METHOD solve.
    FIELD-SYMBOLS <phrase> TYPE string.
    LOOP AT phrases_input ASSIGNING <phrase>.
      DATA(anagram_free) = me->has_not_anagram( <phrase> ).
      WRITE: / anagram_free, <phrase>.
      ADD anagram_free TO return.
    ENDLOOP.
  ENDMETHOD.

  METHOD has_not_anagram.
    SPLIT phrase AT ' ' INTO TABLE DATA(words).
    SPLIT phrase AT ' ' INTO TABLE DATA(repeated).
    SORT repeated.
    DELETE ADJACENT DUPLICATES FROM repeated.
    CHECK lines( repeated ) EQ lines( words ).

    FIELD-SYMBOLS <word> TYPE string.
    DATA products TYPE TABLE OF int8.
    LOOP AT words ASSIGNING <word>.
      DATA(product) = me->get_product( <word> ).
      APPEND product TO products.
    ENDLOOP.
    SORT products.
    DATA(original_lines) = lines( products ).
    DELETE ADJACENT DUPLICATES FROM products.
    IF original_lines = lines( products ).
      return = 1.
    ENDIF.
  ENDMETHOD.

  METHOD get_product.
    DATA(repeat) = strlen( word ).
    DATA(offset) = 0.
    DATA(prime) = 0.
    return = 1.
    DO repeat TIMES.

      CASE word+offset(1).
        WHEN 'a'.
          prime = 2.
        WHEN 'b'.
          prime = 3.
        WHEN 'c'.
          prime = 5.
        WHEN 'd'.
          prime = 11.
        WHEN 'e'.
          prime = 13.
        WHEN 'f'.
          prime = 17.
        WHEN 'g'.
          prime = 19.
        WHEN 'h'.
          prime = 23.
        WHEN 'i'.
          prime = 29.
        WHEN 'j'.
          prime = 31.
        WHEN 'k'.
          prime = 37.
        WHEN 'l'.
          prime = 41.
        WHEN 'm'.
          prime = 43.
        WHEN 'n'.
          prime = 47.
        WHEN 'o'.
          prime = 53.
        WHEN 'p'.
          prime = 59.
        WHEN 'q'.
          prime = 61.
        WHEN 'r'.
          prime = 67.
        WHEN 's'.
          prime = 71.
        WHEN 't'.
          prime = 73.
        WHEN 'u'.
          prime = 79.
        WHEN 'v'.
          prime = 83.
        WHEN 'x'.
          prime = 89.
        WHEN 'y'.
          prime = 97.
        WHEN 'w'.
          prime = 101.
        WHEN 'z'.
          prime = 103.
        WHEN OTHERS.
          prime = 107.
      ENDCASE.

      return = return * prime.
      ADD 1 TO offset.

    ENDDO.

    EXIT.

  ENDMETHOD.
ENDCLASS.

CLASS test_solver DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION LONG.
  PRIVATE SECTION.
    DATA testing_solve TYPE REF TO solver.
    METHODS setup.
    METHODS return_3 FOR TESTING.
    METHODS return_1_5_letters FOR TESTING.
    METHODS return_0_5_letters FOR TESTING.

ENDCLASS.

CLASS test_solver IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT me->testing_solve.
  ENDMETHOD.

  METHOD return_3.
    DATA input_table TYPE phrases_type.
    DATA word TYPE string.
    word = 'abcde fghij'.
    APPEND word TO input_table.
    word = 'abcde xyz ecdab'.
    APPEND word TO input_table.
    word = 'a ab abc abd abf abj'.
    APPEND word TO input_table.
    word = 'iiii oiii ooii oooi oooo'.
    APPEND word TO input_table.
    word = 'oiii ioii iioi iiio'.
    APPEND word TO input_table.

    DATA(actual) = me->testing_solve->solve( input_table ).
    cl_abap_unit_assert=>assert_equals( act = actual exp = 3 ).

  ENDMETHOD.

  METHOD return_1_5_letters.
    DATA input_table TYPE phrases_type.
    DATA word TYPE string.
    word = 'iiii oiii ooii oooi oooo'.
    APPEND word TO input_table.
    DATA(actual) = me->testing_solve->solve( input_table ).
    cl_abap_unit_assert=>assert_equals( act = actual exp = 1 ).
  ENDMETHOD.

  METHOD return_0_5_letters.
    DATA input_table TYPE phrases_type.
    DATA word TYPE string.
    word = 'hkm yinhnkj kmh kwkw kayknck chur styjif yknakck'.
    APPEND word TO input_table.
    DATA(actual) = me->testing_solve->solve( input_table ).
    cl_abap_unit_assert=>assert_equals( act = actual exp = 0 ).
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
  WRITE / count.
*
