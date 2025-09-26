CLASS zcl_shm_sflight DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  SHARED MEMORY ENABLED.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_sflight,
             carrid    TYPE sflight-carrid,
             connid    TYPE sflight-connid,
             fldate    TYPE sflight-fldate,
             price     TYPE sflight-price,
             currency  TYPE sflight-currency,
             planetype TYPE sflight-planetype,
             seatsmax  TYPE sflight-seatsmax,
             seatsocc  TYPE sflight-seatsocc,
           END OF ty_sflight.
    TYPES:
      tt_sflight TYPE STANDARD TABLE OF ty_sflight WITH EMPTY KEY.

    CLASS-DATA: gt_sflight TYPE tt_sflight.
    CLASS-METHODS:
      set_data IMPORTING it_data TYPE tt_sflight,
      get_data RETURNING VALUE(rt_data) TYPE tt_sflight,
      clear_data.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_shm_sflight IMPLEMENTATION.

  METHOD clear_data.
    CLEAR gt_sflight.
  ENDMETHOD.

  METHOD get_data.
    rt_data = gt_sflight.
  ENDMETHOD.

  METHOD set_data.
    IF it_data[] IS NOT INITIAL.
      APPEND LINES OF it_data TO gt_sflight.
      SORT gt_sflight BY carrid connid fldate.
      DELETE ADJACENT DUPLICATES FROM gt_sflight COMPARING ALL FIELDS.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
