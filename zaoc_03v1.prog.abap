*&---------------------------------------------------------------------*
*& Report zaoc_03v1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zaoc_03v1.

CLASS solver DEFINITION.
  PUBLIC SECTION.
    METHODS get_checksum IMPORTING pos           TYPE i
                         RETURNING VALUE(return) TYPE i.
    METHODS get_next_odd IMPORTING pos           TYPE i
                         RETURNING VALUE(return) TYPE i.
ENDCLASS.

CLASS solver IMPLEMENTATION.
  METHOD get_checksum.
    IF pos = 1.
      return = 0.
      EXIT.
    ENDIF.
    DATA(base) = me->get_next_odd( pos ).
    DATA(a1) = base * base.
    DATA(an_prev) = a1.
    DATA(r) = 2.

    DO.
      DATA(an) = a1 + ( r - 1 ) * ( 1 - base ).
      IF an <= pos.
        DATA(x) = abs( ( ( an + an_prev ) / 2 ) - pos ).
        EXIT.
      ELSE.
        an_prev = an.
      ENDIF.
      ADD 1 TO r.
    ENDDO.

    return = x + ( ( base + 1 ) / 2 ) - 1.

  ENDMETHOD.

  METHOD get_next_odd.

    DATA(sqrt) = sqrt( pos ).
    DATA sqrt_int TYPE i.
    sqrt_int = floor( sqrt ).
    IF sqrt EQ sqrt_int.
      return = sqrt.
      EXIT.
    ENDIF.
    ADD 1 TO sqrt_int.
    DO 2 TIMES.
      DATA(rem) = sqrt_int MOD 2.
      IF rem NE 0.
        return = sqrt_int.
      ELSE.
        ADD 1 TO sqrt_int.
      ENDIF.
    ENDDO.

  ENDMETHOD.

ENDCLASS.

CLASS test_solver DEFINITION FOR TESTING RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA solver_test TYPE REF TO solver.
    METHODS setup.
    METHODS must_return_0 FOR TESTING.
    METHODS next_odd_should_be_7 FOR TESTING.
    METHODS next_odd_should_be_5 FOR TESTING.
    METHODS must_return_3 FOR TESTING.
    METHODS must_return_2 FOR TESTING.
    METHODS must_return_31 FOR TESTING.
ENDCLASS.

CLASS test_solver IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT me->solver_test.
  ENDMETHOD.

  METHOD must_return_0.
    DATA(chksum) = me->solver_test->get_checksum( 1 ).
    cl_abap_unit_assert=>assert_equals( exp = 0 act = chksum ).
  ENDMETHOD.

  METHOD next_odd_should_be_7.
    DATA(next_odd) = me->solver_test->get_next_odd( 28 ).
    cl_abap_unit_assert=>assert_equals( exp = 7 act = next_odd ).
  ENDMETHOD.

  METHOD next_odd_should_be_5.
    DATA(next_odd) = me->solver_test->get_next_odd( 25 ).
    cl_abap_unit_assert=>assert_equals( exp = 5 act = next_odd ).

    next_odd = me->solver_test->get_next_odd( 23 ).
    cl_abap_unit_assert=>assert_equals( exp = 5 act = next_odd ).

  ENDMETHOD.

  METHOD must_return_3.
    DATA(chksum) = me->solver_test->get_checksum( 12 ).
    cl_abap_unit_assert=>assert_equals( exp = 3 act = chksum ).
  ENDMETHOD.

  METHOD must_return_2.
    DATA(chksum) = me->solver_test->get_checksum( 23 ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = chksum ).
  ENDMETHOD.

  METHOD must_return_31.
    DATA(chksum) = me->solver_test->get_checksum( 1024 ).
    cl_abap_unit_assert=>assert_equals( exp = 31 act = chksum ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  DATA(solver) = NEW solver( ).
  DATA(chksum) = solver->get_checksum( 368078 ).
  WRITE chksum.
