*&---------------------------------------------------------------------*
*& Report zaoc_02v1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zaoc_02v1.

TYPES BEGIN OF sheet_type.
TYPES lin TYPE i.
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
    DATA(low) = 0.
    DATA(high) = 0.
    LOOP AT spreadsheet ASSIGNING <line>.
      AT NEW lin.
        chksum = chksum + ( high - low ).
        low = high = <line>-val.
      ENDAT.
      IF <line>-val LT low.
        low = <line>-val.
      ENDIF.
      IF <line>-val GT high.
        high = <line>-val.
      ENDIF.
      AT LAST.
        chksum = chksum + ( high - low ).
      ENDAT.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

CLASS test_solver DEFINITION FOR TESTING RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA solver_test TYPE REF TO solver.
    DATA input_sheet TYPE sheet_table.
    METHODS setup.
    METHODS must_return_18 FOR TESTING.
ENDCLASS.

CLASS test_solver IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT me->solver_test.
    FREE me->input_sheet.
  ENDMETHOD.

  METHOD must_return_18.
    DATA line TYPE sheet_type.
    line-lin = 1.
    line-val = 5.
    APPEND line TO me->input_sheet.
    line-val = 1.
    APPEND line TO me->input_sheet.
    line-val = 9.
    APPEND line TO me->input_sheet.
    line-val = 5.
    APPEND line TO me->input_sheet.

    line-lin = 2.
    line-val = 7.
    APPEND line TO me->input_sheet.
    line-val = 5.
    APPEND line TO me->input_sheet.
    line-val = 3.
    APPEND line TO me->input_sheet.


    line-lin = 3.
    line-val = 2.
    APPEND line TO me->input_sheet.
    line-val = 4.
    APPEND line TO me->input_sheet.
    line-val = 6.
    APPEND line TO me->input_sheet.
    line-val = 8.
    APPEND line TO me->input_sheet.

    DATA(chksum) = me->solver_test->get_checksum( me->input_sheet ).
    cl_abap_unit_assert=>assert_equals( exp = 18 act = chksum ).

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
