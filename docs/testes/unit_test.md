# ABAP Unit

```abap
CLASS ltc_demo DEFINITION FOR TESTING.
  PRIVATE SECTION.
    METHODS test_sum FOR TESTING.
ENDCLASS.

CLASS ltc_demo IMPLEMENTATION.
  METHOD test_sum.
    cl_abap_unit_assert=>assert_equals( act = 2 + 2 exp = 4 ).
  ENDMETHOD.
ENDCLASS.
```
