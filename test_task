*&---------------------------------------------------------------------*
*& Report ZSELECTION
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

INCLUDE ZTOP.
INCLUDE ZK01.
INCLUDE ZF01.
INCLUDE ZF02_ALV.
INCLUDE ZE01.

INCLUDE ZPBO0110.
INCLUDE ZPAI0110.

INCLUDE ZPBO0120.
INCLUDE ZPAI0120.

INCLUDE ZPBO0130.
INCLUDE ZPAI0130.
*&---------------------------------------------------------------------*
*&  Include           ZTOP
*&---------------------------------------------------------------------*
REPORT ZSELECTION.

TABLES: ztabel1, ztabel2.
TABLES: zstructure, zstr_order.

DATA: control_area,
      ok_code LIKE sy-ucomm.

TYPES: BEGIN OF str_type.
  INCLUDE TYPE ztabel1.
  TYPES:
    key2 TYPE ZKEY2,
    namepos TYPE ztabel2-namepos,
END OF str_type.

TYPES: BEGIN OF main_info.
  TYPES:  mandt TYPE mandt,
          id TYPE zorid,
          author TYPE ztauthor,
          date TYPE datum,
          time TYPE time,
          curr TYPE currency,
END OF main_info.

DATA:
  itab TYPE TABLE OF str_type,
  wa_itab LIKE LINE OF itab.

DATA:
  orders TYPE TABLE OF main_info,
  wa_orders LIKE LINE OF orders.

DATA:
  it_ztabel1 TYPE TABLE OF ztabel1,
  wa_ztabel1 LIKE LINE OF it_ztabel1.

DATA:
  it_ztabel2 TYPE TABLE OF ztabel2,
  wa_ztabel2 LIKE LINE OF it_ztabel2.

DATA: gr_alv TYPE REF TO cl_salv_table,
      gr_error TYPE REF TO cx_salv_msg.

DATA so_alv TYPE REF TO cl_salv_table."cl_gui_alv_grid .
DATA gr_cont TYPE REF TO cl_gui_custom_container.


TYPES : BEGIN OF ty_f4pernr,
        orderid TYPE zorid,
        date TYPE datum,
        time TYPE time,
        END OF ty_f4pernr.

TYPES : BEGIN OF ty_nms_f4pernr,
        namepos TYPE znamepos,
        END OF ty_nms_f4pernr.

DATA: int_f4pernr TYPE STANDARD TABLE OF ty_f4pernr,
      names_f4pernr TYPE STANDARD TABLE OF ty_nms_f4pernr.
REFRESH : int_f4pernr, names_f4pernr.

SELECTION-SCREEN BEGIN OF BLOCK choice WITH FRAME TITLE text-001.
SELECT-OPTIONS:
    so_num FOR ztabel1-orderid,
    so_auth FOR ztabel1-author,
    so_date FOR ztabel1-datum,
    so_time FOR ztabel1-time,
    so_nmps FOR ztabel2-namepos.
SELECTION-SCREEN END OF BLOCK choice.

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click
        FOR EVENT double_click
          OF cl_salv_events_table
        IMPORTING
          row " type salv_de_row
          column, " type salv_de_column
    on_added_function
      FOR EVENT added_function
        OF cl_salv_events_table
      IMPORTING
        e_salv_function. " type salv_de_function

ENDCLASS.
*&---------------------------------------------------------------------*
*&  Include           ZK01
*&---------------------------------------------------------------------*

CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_double_click.
     CASE column.
       WHEN 'ID'.
          DATA wa_orders LIKE LINE OF orders.
          READ TABLE orders INDEX row INTO wa_orders.

          DATA id TYPE zorid.
          id = wa_orders-ID.
          SET PARAMETER ID 'MEM' FIELD id.
          CALL SCREEN 110
            STARTING AT 15 10
            ENDING AT 90 20.
      ENDCASE.
  ENDMETHOD.

  METHOD on_added_function.

    DATA: lr_columns TYPE REF TO cl_salv_columns_table,
          lr_column TYPE REF TO cl_salv_column_table.
    DATA: l_lvc_s_colo TYPE lvc_s_colo.

    DATA: lo_selections TYPE REF TO cl_salv_selections,
              lt_row TYPE salv_t_row,
              lwa_row LIKE LINE OF lt_row.
    DATA l_answer TYPE string.
    CLEAR l_answer.
    CASE e_salv_function.
      WHEN 'DELETE'.
        PERFORM block_ztabel2.
        IF sy-subrc EQ 0.
          CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
            EXPORTING
              textline1 = 'Are you sure?'
              titel = ''
            IMPORTING
              answer = l_answer.
          IF sy-subrc <> 0.
             MESSAGE i018(Error).
          ELSE.
           IF l_answer EQ 'J'.
             PERFORM delete_position USING so_alv
                                           lo_selections.
           ENDIF.
          ENDIF.
          PERFORM unblock_ztabel2.
        ELSE.
          MESSAGE i018(Blocked).
        ENDIF.
      WHEN 'CREATE'.
        PERFORM create_order USING gr_alv.
      WHEN 'DELETEID'.
        PERFORM block_ztabel1.
          IF sy-subrc EQ 0.
            PERFORM block_ztabel2.
              IF sy-subrc EQ 0.
                CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
                  EXPORTING
                    textline1 = 'Are you sure?'
                    titel = ''
                  IMPORTING
                    answer = l_answer.
                IF sy-subrc <> 0.
                  MESSAGE i018(Error).
                ELSE.
                  BREAK-POINT.
                IF l_answer EQ 'J'.
                  PERFORM delete_order USING gr_alv
                                         lo_selections.
                ENDIF.
                ENDIF.
                PERFORM unblock_ztabel2.
              ELSE.
                MESSAGE i018(Blocked).
              ENDIF.
            PERFORM unblock_ztabel1.
          ELSE.
            MESSAGE i018(Blocked).
          ENDIF.
     ENDCASE.
     COMMIT WORK AND WAIT.
   ENDMETHOD.
ENDCLASS. "lcl_event_handler IMPLEMENTATION
*&---------------------------------------------------------------------*
*&  Include           ZF01
*&---------------------------------------------------------------------*

FORM orderid_f4help.
  SELECT orderid datum time FROM ztabel1 INTO  TABLE int_f4pernr .

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ORDERID'
      dynpprog        = sy-repid    " Program name
      dynpnr          = sy-dynnr    " Screen number
      dynprofield     = 'so_num'   " F4 help need field
      value_org       = 'S'
    TABLES
      value_tab       = int_f4pernr " F4 help values
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

ENDFORM.

FORM nmps_f4help.
  SELECT namepos FROM ztabel2 INTO TABLE names_f4pernr
    WHERE ztabel2~orderid IN so_num.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'NAMEPOS'
      dynpprog        = sy-repid    " Program name
      dynpnr          = sy-dynnr    " Screen number
      dynprofield     = 'so_nmps'   " F4 help need field
      value_org       = 'S'
    TABLES
      value_tab       = names_f4pernr " F4 help values
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
ENDFORM.

FORM block_ztabel1.
  CALL FUNCTION 'ENQUEUE_EZ_ZTABEL1'
  EXPORTING
   MODE_ZTABEL1         = 'E'
   MANDT                = SY-MANDT
   X_ORDERID            = ' '
   _SCOPE               = '2'
   _WAIT                = ' '
   _COLLECT             = ' '
  EXCEPTIONS
   FOREIGN_LOCK         = 1
   SYSTEM_FAILURE       = 2
   OTHERS               = 3.
ENDFORM.

FORM unblock_ztabel1.
  CALL FUNCTION 'DEQUEUE_EZ_ZTABEL1'
    EXPORTING
     MODE_ZTABEL1       = 'E'
     MANDT              = SY-MANDT
     X_ORDERID          = ' '
     _SCOPE             = '3'
     _SYNCHRON          = ' '
     _COLLECT           = ' '.
ENDFORM.

FORM block_ztabel2.
  CALL FUNCTION 'ENQUEUE_EZ_ZTABEL2'
  EXPORTING
   MODE_ZTABEL2         = 'E'
   MANDT                = SY-MANDT
   X_ORDERID            = ' '
   _SCOPE               = '2'
   _WAIT                = ' '
   _COLLECT             = ' '
  EXCEPTIONS
   FOREIGN_LOCK         = 1
   SYSTEM_FAILURE       = 2
   OTHERS               = 3.
ENDFORM.

FORM unblock_ztabel2.
  CALL FUNCTION 'DEQUEUE_EZ_ZTABEL2'
    EXPORTING
     MODE_ZTABEL2       = 'E'
     MANDT              = SY-MANDT
     X_ORDERID          = ' '
     _SCOPE             = '3'
     _SYNCHRON          = ' '
     _COLLECT           = ' '.
ENDFORM.

FORM create_order
  USING p_alv TYPE REF TO cl_salv_table.

  PERFORM block_ztabel1.
  IF sy-subrc <> 0.
    MESSAGE i018(Blocked).
  ELSE.
    CALL SCREEN 130
      STARTING AT 15 10
      ENDING AT 90 20.

    PERFORM unblock_ztabel1.
  ENDIF.
  p_alv->refresh( ).
ENDFORM.

FORM create_position.
  PERFORM block_ztabel2.

  IF sy-subrc <> 0.
    MESSAGE i018(Blocked).
  ELSE.
    LEAVE TO SCREEN 120.
    PERFORM unblock_ztabel2.
  ENDIF.
ENDFORM.

FORM delete_position USING p_alv TYPE REF TO cl_salv_table
                           p_selections TYPE REF TO cl_salv_selections.
  DATA: lt_row TYPE salv_t_row,
        lwa_row LIKE LINE OF lt_row.
  p_selections = p_alv->get_selections( ).
       p_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

       IF p_selections IS BOUND.
         lt_row = p_selections->get_selected_rows( ).
         LOOP AT lt_row INTO lwa_row.
           READ TABLE itab INTO wa_itab INDEX lwa_row.
           IF sy-subrc EQ 0.

             CALL FUNCTION 'Z_DELETE_POSITION' IN UPDATE TASK
               EXPORTING orderid = wa_itab-orderid
                         key2 =  wa_itab-key2.

             IF sy-subrc EQ 0.
               DELETE itab INDEX lwa_row.
             ENDIF.
             COMMIT WORK.
             p_alv->refresh( ).
             PERFORM set_filter USING p_alv.
           ENDIF.
         ENDLOOP.
       ENDIF.
ENDFORM.

FORM delete_order USING p_alv TYPE REF TO cl_salv_table
                        p_selections TYPE REF TO cl_salv_selections.
  DATA: lt_row TYPE salv_t_row,
        lwa_row LIKE LINE OF lt_row.
  p_selections = p_alv->get_selections( ).
  p_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

  IF p_selections IS BOUND.
    lt_row = p_selections->get_selected_rows( ).
    LOOP AT lt_row INTO lwa_row.
      READ TABLE orders INTO wa_orders INDEX lwa_row.
      IF sy-subrc EQ 0.

        CALL FUNCTION 'Z_DELETE_ORDER' IN UPDATE TASK
          EXPORTING oderid = wa_orders-id.

        IF sy-subrc EQ 0.
          DELETE itab WHERE orderid = wa_orders-id.
          DELETE orders WHERE id = wa_orders-id.
          COMMIT WORK.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
  p_alv->refresh( ).
ENDFORM.
*&---------------------------------------------------------------------*
*&  Include           ZFALV
*&---------------------------------------------------------------------*
FORM set_events
  USING p_alv TYPE REF TO cl_salv_table.

  DATA: lo_event TYPE REF TO cl_salv_events_table.
  lo_event = p_alv->get_event( ).

  SET HANDLER:
    lcl_event_handler=>on_double_click FOR lo_event,
    lcl_event_handler=>on_added_function FOR lo_event.
ENDFORM.

FORM set_default_functions
  USING p_alv TYPE REF TO cl_salv_table.

  DATA: lo_functions  TYPE REF TO cl_salv_functions_list.

  lo_functions = p_alv->get_functions( ).
  lo_functions->set_default( ).
ENDFORM.

FORM add_delete_function
  USING p_alv TYPE REF TO cl_salv_table.
  DATA: lr_functions TYPE REF TO cl_salv_functions_list,
         l_text       TYPE string,
         l_icon       TYPE string.

   lr_functions = p_alv->get_functions( ).
   TRY.
     l_text = text-b01.
     l_icon = icon_delete.
     lr_functions->add_function(
       name     = 'DELETE'
       icon     = l_icon
       text     = l_text
       tooltip  = 'Delete'
       position = if_salv_c_function_position=>right_of_salv_functions ).
     catch cx_salv_wrong_call cx_salv_existing.
   ENDTRY.
ENDFORM.

FORM add_create_function
  USING p_alv TYPE REF TO cl_salv_table.
  DATA: lr_functions TYPE REF TO cl_salv_functions_list,
         l_text       TYPE string,
         l_icon       TYPE string.

   lr_functions = p_alv->get_functions( ).
   TRY.
     l_text = text-b01.
     l_icon = icon_create.
     lr_functions->add_function(
       name     = 'CREATE'
       icon     = l_icon
       text     = l_text
       tooltip  = 'Create'
       position = if_salv_c_function_position=>right_of_salv_functions ).
     catch cx_salv_wrong_call cx_salv_existing.
   ENDTRY.
ENDFORM.

FORM add_delete_function_for_id
  USING p_alv TYPE REF TO cl_salv_table.
  DATA: lr_functions TYPE REF TO cl_salv_functions_list,
         l_text       TYPE string,
         l_icon       TYPE string.

   lr_functions = p_alv->get_functions( ).
   TRY.
     l_text = text-b01.
     l_icon = icon_delete.
     lr_functions->add_function(
       name     = 'DELETEID'
       icon     = l_icon
       text     = l_text
       tooltip  = 'Delete'
       position = if_salv_c_function_position=>right_of_salv_functions ).
     catch cx_salv_wrong_call cx_salv_existing.
   ENDTRY.
ENDFORM.

FORM set_optimize
  USING p_alv TYPE REF TO cl_salv_table.

  p_alv->get_columns( )->set_optimize( ).
ENDFORM.

FORM set_sorts
  USING p_alv TYPE REF TO cl_salv_table.

  DATA: lo_sort TYPE REF TO cl_salv_sorts.
  lo_sort = p_alv->get_sorts( ).

  TRY.
    lo_sort->add_sort(
      EXPORTING
        columnname = 'ID'
        subtotal   = if_salv_c_bool_sap=>true
        ).
  CATCH: cx_salv_not_found,
         cx_salv_existing,
         cx_salv_data_error.
  ENDTRY.
ENDFORM.

FORM set_filter
   USING p_alv TYPE REF TO cl_salv_table.

   DATA: id TYPE salv_de_selopt_low,
         lo_filters TYPE REF TO cl_salv_filters.

   GET PARAMETER ID 'MEM' FIELD id.

   lo_filters = p_alv->get_filters( ).
   lo_filters->clear( ).

   TRY.
      lo_filters->add_filter(
        EXPORTING
          columnname = 'ORDERID'
          sign       = 'I'
          option     = 'EQ'
          low        = id
          ).
    CATCH: cx_salv_not_found,
           cx_salv_data_error,
           cx_salv_existing.
  ENDTRY.
ENDFORM.

FORM hide_client
  USING p_alv TYPE REF TO cl_salv_table.
  TRY.
    p_alv->get_columns( )->get_column( 'MANDT' )->set_visible( if_salv_c_bool_sap=>false ).
  CATCH cx_salv_not_found.
  ENDTRY.
ENDFORM.
*&---------------------------------------------------------------------*
*&  Include           ZE01
*&---------------------------------------------------------------------*
INITIALIZATION.
  AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_num-low.
    PERFORM orderid_f4help.

  AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_num-high.
     PERFORM orderid_f4help.

  AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_nmps-low.
    PERFORM nmps_f4help.

  AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_nmps-high.
    PERFORM nmps_f4help.

START-OF-SELECTION.

SELECT * FROM
  ztabel1 INNER JOIN ztabel2 ON ( ztabel1~orderid = ztabel2~orderid )
       INTO CORRESPONDING FIELDS OF TABLE itab
         WHERE ztabel1~orderid IN so_num
         AND author IN so_auth
         AND datum IN so_date
         AND time IN so_time
         AND ztabel2~namepos IN so_nmps.

PERFORM block_ztabel1.
PERFORM block_ztabel2.
END-OF-SELECTION.

LOOP AT itab INTO wa_itab.
  wa_orders-mandt = wa_itab-mandt.
  wa_orders-id = wa_itab-orderid.
  wa_orders-author = wa_itab-author.
  wa_orders-date = wa_itab-datum.
  wa_orders-time = wa_itab-time.
  wa_orders-curr = wa_itab-curr.

  COLLECT wa_orders INTO orders.
  CLEAR wa_orders.
ENDLOOP.

TRY.
  cl_salv_table=>factory(
    EXPORTING
      r_container = cl_gui_container=>screen0
    IMPORTING
      r_salv_table = gr_alv
    CHANGING
      t_table = orders
      ).
CATCH cx_salv_msg INTO gr_error.
ENDTRY.

PERFORM: set_events USING gr_alv,
         add_delete_function_for_id USING gr_alv,
         add_create_function USING gr_alv,
         set_optimize USING gr_alv,
         set_sorts USING gr_alv,
         hide_client USING gr_alv.

gr_alv->get_columns( )->get_column( 'CURR' )->set_visible( if_salv_c_bool_sap=>false ).
gr_alv->display( ).
WRITE ''.
*&---------------------------------------------------------------------*
*&  Include           ZPBO0110
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0110 OUTPUT.
  SET PF-STATUS 'S110'.
  SET TITLEBAR 'T110'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CREATE_CONTROL  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_control OUTPUT.
  IF gr_cont IS NOT BOUND.
    CREATE OBJECT gr_cont
      EXPORTING
        container_name = 'CONTROL_AREA'
      EXCEPTIONS
        OTHERS = 1.

    IF sy-subrc <> 0.
      MESSAGE a015(bc405).
    ENDIF.

    TRY.
        cl_salv_table=>factory(
      EXPORTING
        r_container = gr_cont
      IMPORTING
        r_salv_table = so_alv
      CHANGING
        t_table = itab
      ).
    CATCH cx_salv_msg INTO gr_error.
    ENDTRY.
  ELSE.
    so_alv->refresh( ).
  ENDIF.

  PERFORM:
    set_optimize USING so_alv,
    set_filter USING so_alv,
    set_events USING so_alv,
    add_delete_function USING so_alv,
    hide_client USING so_alv.

  so_alv->display( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CLEAR_OK_CODE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE clear_ok_code OUTPUT.
  CLEAR ok_code.
ENDMODULE.
*&---------------------------------------------------------------------*
*&  Include           ZPAI0110
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0110 INPUT.
  CASE ok_code.
    WHEN 'CANCEL'.
       CLEAR ok_code.
       LEAVE TO SCREEN 0.
    WHEN 'CREATE'.
      PERFORM create_position.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&  Include           ZPBO0120
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Module  STATUS_0120  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0120 OUTPUT.
  SET PF-STATUS 'S120'.
  SET TITLEBAR 'T120'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MOVE_TO_DYNP  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE move_to_dynp OUTPUT.
  CLEAR zstructure.
  MOVE sy-mandt TO zstructure-mandt.
  GET PARAMETER ID 'MEM' FIELD zstructure-orderid.
ENDMODULE.
*&---------------------------------------------------------------------*
*&  Include           ZPAI0120
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0120  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0120 INPUT.
CASE ok_code.
    WHEN 'CANCEL'.
      CLEAR ok_code.
       LEAVE SCREEN.
    WHEN 'CONTINUE'.
      MOVE-CORRESPONDING wa_itab TO wa_ztabel2.

      CALL FUNCTION 'Z_ADD_POSITION' IN UPDATE TASK
        EXPORTING wa_ztabel2 = wa_ztabel2.

      IF sy-subrc <> 0.
         MESSAGE a015(bc405).
      ELSE.
        DATA: id TYPE zorid.
        GET PARAMETER ID 'MEM' FIELD id.

        DATA: lwa LIKE LINE OF orders.
           LOOP AT orders INTO lwa
             WHERE id = id.
           ENDLOOP.

         wa_itab-orderid = lwa-id.
         wa_itab-author = lwa-author.
         wa_itab-datum = lwa-date.
         wa_itab-time = lwa-time.
         wa_itab-curr = lwa-curr.
         APPEND wa_itab TO itab.
         IF sy-subrc <> 0.
           MESSAGE a015(bc405).
         ENDIF.
         COMMIT WORK.
       ENDIF.
    COMMIT WORK AND WAIT.
    LEAVE SCREEN.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MOVE_FROM_DYNP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE move_from_dynp INPUT.
  MOVE-CORRESPONDING zstructure to wa_itab.
ENDMODULE.
*&---------------------------------------------------------------------*
*&  Include           ZPBO0130
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Module  STATUS_0130  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0130 OUTPUT.
  SET PF-STATUS 'S130'.
  SET TITLEBAR 'T130'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MOVE_TO_ZSTRORDER  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE move_to_zstrorder OUTPUT.
  MOVE sy-mandt TO zstr_order-mandt.
ENDMODULE.
*&---------------------------------------------------------------------*
*&  Include           ZPAI0130
*&---------------------------------------------------------------------*

MODULE user_command_0130 INPUT.
CASE ok_code.
    WHEN 'CANCEL'.
      CLEAR ok_code.
       LEAVE TO SCREEN 0.
    WHEN 'CONTINUE'.
      CALL FUNCTION 'ZADD_ORDER' IN UPDATE TASK
          EXPORTING wa_ztabel1 = wa_ztabel1.
      IF sy-subrc <> 0.
         MESSAGE a015(bc405).
      ELSE.
        MOVE-CORRESPONDING wa_ztabel1 TO wa_orders.
        wa_orders-id = wa_ztabel1-orderid.
        wa_orders-date = wa_ztabel1-datum.
        APPEND wa_orders TO orders.
        IF sy-subrc <> 0.
          MESSAGE a015(bc405).
        ENDIF.
        COMMIT WORK.
      ENDIF.
    LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MOVE_FROM_ZSTRORDER  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE move_from_zstrorder INPUT.
  MOVE-CORRESPONDING zstr_order TO wa_ztabel1.
ENDMODULE.


