*&---------------------------------------------------------------------*
*& Report zaoc_02av1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zaoc_02av1.

TYPES BEGIN OF sheet_type.
TYPES lin TYPE i.
TYPES col TYPE i.
TYPES val TYPE i.
TYPES END OF sheet_type.
TYPES sheet_table TYPE TABLE OF sheet_type.

CLASS solver DEFINITION.
  PUBLIC SECTION.
    METHODS get_checksum IMPORTING spreadsheet   TYPE sheet_table
                         RETURNING VALUE(chksum) TYPE i.
ENDCLASS.

CLASS solver IMPLEMENTATION.
  METHOD get_checksum.

    FIELD-SYMBOLS <line> TYPE sheet_type.
    DATA(sheet_ordered) = spreadsheet[].
    SORT sheet_ordered BY lin DESCENDING val DESCENDING.
    DATA(line_n) = 1.

    DO 300 TIMES.
      line_n = sheet_ordered[ 1 ]-lin.
      LOOP AT sheet_ordered ASSIGNING <line> WHERE lin = line_n.
        AT FIRST.
          DATA(dividend) = <line>-val.
          CONTINUE.
        ENDAT.
        DATA(remainder) = dividend MOD <line>-val.
        IF remainder IS INITIAL.
          chksum = chksum + ( dividend / <line>-val ).
        ENDIF.
      ENDLOOP.
      DELETE sheet_ordered INDEX 1.
      IF lines( sheet_ordered ) = 0.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.
ENDCLASS.

CLASS test_solver DEFINITION FOR TESTING RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA solver_test TYPE REF TO solver.
    DATA input_sheet TYPE sheet_table.
    METHODS setup.
    METHODS must_return_9 FOR TESTING.
ENDCLASS.

CLASS test_solver IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT me->solver_test.
    FREE me->input_sheet.
  ENDMETHOD.

  METHOD must_return_9.
    DATA line TYPE sheet_type.
    line-lin = 1.
    line-val = 5.
    APPEND line TO me->input_sheet.
    line-val = 9.
    APPEND line TO me->input_sheet.
    line-val = 2.
    APPEND line TO me->input_sheet.
    line-val = 8.
    APPEND line TO me->input_sheet.

    line-lin = 2.
    line-val = 9.
    APPEND line TO me->input_sheet.
    line-val = 4.
    APPEND line TO me->input_sheet.
    line-val = 7.
    APPEND line TO me->input_sheet.
    line-val = 3.
    APPEND line TO me->input_sheet.

    line-lin = 3.
    line-val = 3.
    APPEND line TO me->input_sheet.
    line-val = 8.
    APPEND line TO me->input_sheet.
    line-val = 6.
    APPEND line TO me->input_sheet.
    line-val = 5.
    APPEND line TO me->input_sheet.

    DATA(chksum) = me->solver_test->get_checksum( me->input_sheet ).
    cl_abap_unit_assert=>assert_equals( exp = 9 act = chksum ).

  ENDMETHOD.
ENDCLASS.

DATA input_sheet TYPE sheet_table.
DATA line TYPE sheet_type.

START-OF-SELECTION.

  DATA raw_table TYPE TABLE OF string.
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename = '/Users/flavio/work/aoc2017/input_day_2_a.txt'
      filetype = 'ASC'
    TABLES
      data_tab = raw_table.

  LOOP AT raw_table INTO DATA(raw_line).
    line-lin = sy-tabix.
    SPLIT raw_line AT ' ' INTO TABLE DATA(values).
    LOOP AT values INTO DATA(value).
      CHECK value NE 0.
      line-val = value.
      APPEND line TO input_sheet.
    ENDLOOP.
  ENDLOOP.

  DATA(solver) = NEW solver( ).
  DATA(chksum) = solver->get_checksum( input_sheet ).
  WRITE chksum.
