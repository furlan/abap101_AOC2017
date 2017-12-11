*&---------------------------------------------------------------------*
*& Report zaoc_03av1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zaoc_03av1.

TYPES BEGIN OF position_type.
TYPES lin TYPE i.
TYPES col TYPE i.
TYPES val TYPE i.
TYPES END OF position_type.
TYPES positions_type TYPE TABLE OF position_type.

CLASS solver DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS solve IMPORTING input_num     TYPE i
                  RETURNING VALUE(return) TYPE i.
    METHODS get_next_position RETURNING VALUE(return) TYPE position_type.
    METHODS get_value IMPORTING x             TYPE i
                                y             TYPE i
                      RETURNING VALUE(return) TYPE i.

  PRIVATE SECTION.
    DATA positions TYPE positions_type.
    DATA actual_search_pos_index TYPE i VALUE 1.
    DATA side_size TYPE i.
    DATA actual_pos TYPE position_type.
    METHODS get_next_adj_pos IMPORTING x             TYPE i
                                       y             TYPE i
                             RETURNING VALUE(return) TYPE position_type.
ENDCLASS.

CLASS solver IMPLEMENTATION.
  METHOD constructor.
    me->side_size = 3.
    me->actual_pos-val = 1.  "step
    me->actual_pos-lin = 0.
    me->actual_pos-col = 1.
    FIELD-SYMBOLS <position> TYPE position_type.
    APPEND INITIAL LINE TO me->positions ASSIGNING <position>.
    <position>-lin = 0.
    <position>-col = 0.
    <position>-val = 1.

    APPEND INITIAL LINE TO me->positions ASSIGNING <position>.
    <position>-lin = 0.
    <position>-col = 1.
    <position>-val = 1.
  ENDMETHOD.

  METHOD solve.

    DO. " exploratory - no ending!
      DATA(steps) = ( me->side_size * 4 ) - 4 - 1.
      DO steps TIMES.   " one lap
        DATA(position) = me->get_next_position( ).
        position-val = me->get_value( x = position-lin y = position-col ).
        IF position-val > input_num.
          return = position-val.
          EXIT.
        ENDIF.
        APPEND position TO me->positions.
      ENDDO.
      IF return IS NOT INITIAL.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD get_next_position.

    IF ( me->actual_pos-val < ( me->side_size - 1 ) ).
      ADD 1 TO me->actual_pos-lin.
    ELSEIF (  me->actual_pos-val >= ( me->side_size - 1 ) ) AND
           (  me->actual_pos-val < ( ( 2 * me->side_size ) - 2 ) ).
      SUBTRACT 1 FROM me->actual_pos-col.
    ELSEIF (  me->actual_pos-val >= ( ( 2 * me->side_size ) - 2 ) ) AND
           (  me->actual_pos-val < ( ( 3 * me->side_size ) - 3 ) ).
      SUBTRACT 1 FROM me->actual_pos-lin.
    ELSEIF (  me->actual_pos-val >= ( ( 3 * me->side_size ) - 3 ) ) AND
           (  me->actual_pos-val < ( ( 4 * me->side_size ) - 4 ) ).
      ADD 1 TO me->actual_pos-col.
    ELSE.
      ADD 1 TO me->actual_pos-col.
      ADD 2 TO me->side_size.
      me->actual_pos-val = 1.  "reset step
      return = me->actual_pos.
      EXIT.
    ENDIF.
    ADD 1 TO me->actual_pos-val.
    return = me->actual_pos.
  ENDMETHOD.

  METHOD get_value.
    DATA position TYPE position_type.
    DATA adj_pos TYPE position_type.

    DO.   " all adjacents
      adj_pos = me->get_next_adj_pos( x = x y = y ).
      IF adj_pos-val EQ -1.
        EXIT.
      ENDIF.
      READ TABLE me->positions WITH KEY lin = adj_pos-lin col = adj_pos-col INTO position.
      IF sy-subrc = 0.
        ADD position-val TO return.
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD get_next_adj_pos.

    CASE me->actual_search_pos_index.
      WHEN 0.
        return-lin = x.
        return-col = y.
        ADD 1 TO me->actual_search_pos_index.
      WHEN 1.
        return-lin = x.
        return-col = y + 1.
        ADD 1 TO me->actual_search_pos_index.
      WHEN 2.
        return-lin = x + 1.
        return-col = y + 1.
        ADD 1 TO me->actual_search_pos_index.
      WHEN 3.
        return-lin = x + 1.
        return-col = y.
        ADD 1 TO me->actual_search_pos_index.
      WHEN 4.
        return-lin = x + 1.
        return-col = y - 1.
        ADD 1 TO me->actual_search_pos_index.
      WHEN 5.
        return-lin = x - 1.
        return-col = y - 1.
        ADD 1 TO me->actual_search_pos_index.
      WHEN 6.
        return-lin = x.
        return-col = y - 1.
        ADD 1 TO me->actual_search_pos_index.
      WHEN 7.
        return-lin = x - 1.
        return-col = y.
        ADD 1 TO me->actual_search_pos_index.
      WHEN 8.
        return-lin = x - 1.
        return-col = y + 1.
        ADD 1 TO me->actual_search_pos_index.
      WHEN OTHERS.
        return-lin = return-col = return-val = -1.
        me->actual_search_pos_index = 1.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

CLASS test_solver DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    DATA solver_test TYPE REF TO solver.
    METHODS setup.
    METHODS must_return_2 FOR TESTING.
    METHODS must_return_26 FOR TESTING.
    METHODS must_return_133 FOR TESTING.
    Methods must_return_880 FOR TESTING.
ENDCLASS.

CLASS test_solver IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT me->solver_test.
  ENDMETHOD.

  METHOD must_return_2.
    DATA(next) = me->solver_test->solve( 1 ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = next ).
  ENDMETHOD.

  METHOD must_return_26.
    DATA(next) = me->solver_test->solve( 25 ).
    cl_abap_unit_assert=>assert_equals( exp = 26 act = next ).
  ENDMETHOD.

  METHOD must_return_133.
    DATA(next) = me->solver_test->solve( 122 ).
    cl_abap_unit_assert=>assert_equals( exp = 133 act = next ).
  ENDMETHOD.

  METHOD must_return_880.
    DATA(next) = me->solver_test->solve( 835 ).
    cl_abap_unit_assert=>assert_equals( exp = 880 act = next ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  DATA(solver) = NEW solver( ).
  DATA(next) = solver->solve( 368078 ).
  WRITE next.
