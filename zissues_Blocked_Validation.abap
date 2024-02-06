*&---------------------------------------------------------------------*
*& Include          ZISSUE_BLOCKED_VALIDATION
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form Get_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data.
************************************************************************
**************Purchase Requisition Plant Level Data*********************
************************************************************************
  SELECT banfn, bnfpo,bsart,ekgrp,matnr,werks,lgort,matkl,badat,lpein
    INTO TABLE @gt_eban
    FROM eban
    WHERE werks = @p_plant.

************************************************************************
**************Purchase Requisition Items Level Data*********************
************************************************************************
  IF gt_eban[] IS NOT INITIAL.
    SELECT banfn,menge,sakto,kokrs
      INTO TABLE @DATA(lt_ebkn)
      FROM ebkn
      FOR ALL ENTRIES IN @gt_eban
      WHERE banfn EQ @gt_eban-banfn.
  ENDIF.

************************************************************************
**********************Material Related Data*****************************
************************************************************************

  IF gt_eban[] IS NOT INITIAL.
    SELECT matnr,mbrsh,matkl,meins
      INTO TABLE @gt_mara
      FROM mara
      FOR ALL ENTRIES IN @gt_eban
      WHERE matnr = @gt_eban-matnr.
  ENDIF.

************************************************************************
**********************Material Description Data*************************
************************************************************************

  IF gt_mara[] IS NOT INITIAL.
    SELECT matnr,maktx
      INTO TABLE @gt_makt
      FROM makt
      FOR ALL ENTRIES IN @gt_mara
      WHERE matnr = @gt_mara-matnr.
  ENDIF.

************************************************************************
**************************Material Plant Data***************************
************************************************************************

  IF gt_mara[] IS NOT INITIAL.
    SELECT matnr, pstat
      INTO TABLE @gt_marc
      FROM marc
      FOR ALL ENTRIES IN @gt_mara
      WHERE matnr = @gt_mara-matnr.
  ENDIF.


****Loooping on Purhcase Requisition table(EBAN).
  LOOP AT gt_eban INTO DATA(wa1).

    gwa_final_eban-banfn = wa1-banfn.
    gwa_final_eban-bnfpo = wa1-bnfpo.
    gwa_final_eban-bsart = wa1-bsart.
    gwa_final_eban-ekgrp = wa1-ekgrp.
    gwa_final_eban-lgort = wa1-lgort.
    gwa_final_eban-lpein = wa1-lpein.
    gwa_final_eban-matkl = wa1-matkl.
    gwa_final_eban-matnr = wa1-matnr.
    gwa_final_eban-werks = wa1-werks.
    gwa_final_eban-badat = wa1-badat.

****Reading the data from PO Account assignment by PR Number.
    READ TABLE lt_ebkn INTO DATA(wa2) WITH KEY banfn = wa1-banfn.

    IF sy-subrc = 0.
      gwa_final_eban-menge = wa2-menge.
      gwa_final_eban-sakto = wa2-sakto.
      gwa_final_eban-kokrs = wa2-kokrs.

****Clearing Read statement work area.
      CLEAR:wa2.
    ENDIF.

****appending to Final Internal table.
    APPEND gwa_final_eban TO gt_final_eban.

****Clearing Work areas.
    CLEAR:wa1,gwa_final_eban.
  ENDLOOP.

  IF sy-subrc EQ 0.
****Field Cataloge.
    DATA: it_fcat1 TYPE TABLE OF slis_fieldcat_alv,
          lv_pos1  TYPE i VALUE 0.

    ADD 1 TO lv_pos1.
    APPEND VALUE #( col_pos = lv_pos1 fieldname = 'BANFN' seltext_m = 'Purchase Requisition' key = 'X') TO it_fcat1.
    ADD 1 TO lv_pos1.
    APPEND VALUE #( col_pos = lv_pos1 fieldname = 'BNFPO' seltext_m = 'Purchase Requisition Item') TO it_fcat1.
    ADD 1 TO lv_pos1.
    APPEND VALUE #( col_pos = lv_pos1 fieldname = 'BSART' seltext_m = 'Document Type') TO it_fcat1.
    ADD 1 TO lv_pos1.
    APPEND VALUE #( col_pos = lv_pos1 fieldname = 'EKGRP' seltext_m = 'Purchase Group') TO it_fcat1.
    ADD 1 TO lv_pos1.
    APPEND VALUE #( col_pos = lv_pos1 fieldname = 'MATNR' seltext_m = 'Material Number') TO it_fcat1.
    ADD 1 TO lv_pos1.
    APPEND VALUE #( col_pos = lv_pos1 fieldname = 'WERKS' seltext_m = 'Plant') TO it_fcat1.
    ADD 1 TO lv_pos1.
    APPEND VALUE #( col_pos = lv_pos1 fieldname = 'LGORT' seltext_m = 'Storage Location') TO it_fcat1.
    ADD 1 TO lv_pos1.
    APPEND VALUE #( col_pos = lv_pos1 fieldname = 'MATKL' seltext_m = 'Material Group') TO it_fcat1.
    ADD 1 TO lv_pos1.
    APPEND VALUE #( col_pos = lv_pos1 fieldname = 'BADAT' seltext_m = 'Requisition date') TO it_fcat1.
    ADD 1 TO lv_pos1.
    APPEND VALUE #( col_pos = lv_pos1 fieldname = 'LPEIN' seltext_m = 'Delivery Date') TO it_fcat1.
    ADD 1 TO lv_pos1.
    APPEND VALUE #( col_pos = lv_pos1 fieldname = 'MENGE' seltext_m = 'Quantity') TO it_fcat1.
    ADD 1 TO lv_pos1.
    APPEND VALUE #( col_pos = lv_pos1 fieldname = 'SAKTO' seltext_m = 'G/L Account') TO it_fcat1.
    ADD 1 TO lv_pos1.
    APPEND VALUE #( col_pos = lv_pos1 fieldname = 'KOKRS' seltext_m = 'CO Area') TO it_fcat1.
    ADD 1 TO lv_pos1.
    APPEND VALUE #( col_pos = lv_pos1 fieldname = 'CHECK' seltext_m = 'Check' checkbox = 'X' edit = 'X') TO it_fcat1.

****Layout.
    DATA: wa_layout TYPE slis_layout_alv.
    wa_layout-zebra = 'X'.
    wa_layout-colwidth_optimize = 'X'.

****Calling ALV Function Module.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program       = sy-repid
        i_callback_pf_status_set = 'ZPF_STATUS'
        i_callback_user_command  = 'UCOMM'
        i_callback_top_of_page   = 'TOP_OF_PAGE'
        is_layout                = wa_layout
        it_fieldcat              = it_fcat1
      TABLES
        t_outtab                 = gt_final_eban
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.
  ENDIF.

ENDFORM.


************************************************************************
**************PF Status for Purchse Requisition Data********************
************************************************************************
FORM zpf_status USING rt_extab TYPE slis_t_extab.

  SET PF-STATUS 'ZPF_STATUS'.

ENDFORM.


************************************************************************
**************User Command for Purchse Requisition Data*****************
************************************************************************
FORM ucomm  USING action LIKE sy-ucomm
                            index TYPE slis_selfield.

*************************************************
****************Function code********************
*************************************************
  CASE action.

****User command upon choosing PR number.
    WHEN '&IC1'.
      READ TABLE gt_final_eban INTO DATA(wa3) INDEX index-tabindex.
      IF sy-subrc EQ 0.
        APPEND wa3 TO gt_final_eban_copy.
****Looping on Matrail data based on Materail number from PR.
        LOOP AT gt_mara INTO DATA(wa4) WHERE matnr = wa3-matnr.
          gwa_final_mat-matnr = wa4-matnr.
          gwa_final_mat-matkl = wa4-matkl.
          gwa_final_mat-mbrsh = wa4-mbrsh.
          gwa_final_mat-meins = wa4-meins.
          gwa_final_mat-mtart = wa4-mtart.

****Reading Materail data based on Materail Number.
          READ TABLE gt_makt INTO DATA(wa5) WITH KEY matnr = wa3-matnr.
          IF sy-subrc = 0.
            gwa_final_mat-maktx = wa5-maktx.
          ENDIF.

****Reading Materail description data based on Materail Number.
          READ TABLE gt_marc INTO DATA(wa6) WITH KEY matnr = wa3-matnr.
          IF sy-subrc = 0.
            gwa_final_mat-pstat = wa6-pstat.
          ENDIF.

****Append Materail data to final Internal table.
          APPEND gwa_final_mat TO gt_final_mat.

****Clearing work areas.
          CLEAR:wa4,wa5,wa6,gwa_final_mat.
        ENDLOOP.

****Clearing Work area from Index level.
        CLEAR:wa3.
      ENDIF.

      IF sy-subrc = 0.
****Initiating Block Report.
        CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_INIT'
          EXPORTING
            i_callback_program       = sy-repid
            i_callback_pf_status_set = 'ZPF_STATUS_BLK'
            i_callback_user_command  = 'DOWNLOAD'.

        IF sy-subrc = 0.

****Layout for Blocked Report.
          DATA: wa_blk_layout TYPE slis_layout_alv.
          wa_blk_layout-zebra = 'X'.
          wa_blk_layout-colwidth_optimize = 'X'.

****Field catalogue for Blocked Report.
          DATA:it_fcat2 TYPE TABLE OF slis_fieldcat_alv,
               lv_pos2  TYPE i VALUE 0.

          ADD 1 TO lv_pos2.
          APPEND VALUE #( col_pos = lv_pos2 fieldname = 'BANFN' seltext_m = 'Purchase Requisition' key = 'X') TO it_fcat2.
          ADD 1 TO lv_pos2.
          APPEND VALUE #( col_pos = lv_pos2 fieldname = 'BNFPO' seltext_m = 'Purchase Requisition Item') TO it_fcat2.
          ADD 1 TO lv_pos2.
          APPEND VALUE #( col_pos = lv_pos2 fieldname = 'BSART' seltext_m = 'Document Type') TO it_fcat2.
          ADD 1 TO lv_pos2.
          APPEND VALUE #( col_pos = lv_pos2 fieldname = 'EKGRP' seltext_m = 'Purchase Group') TO it_fcat2.
          ADD 1 TO lv_pos2.
          APPEND VALUE #( col_pos = lv_pos2 fieldname = 'MATNR' seltext_m = 'Material Number') TO it_fcat2.
          ADD 1 TO lv_pos2.
          APPEND VALUE #( col_pos = lv_pos2 fieldname = 'WERKS' seltext_m = 'Plant') TO it_fcat2.
          ADD 1 TO lv_pos2.
          APPEND VALUE #( col_pos = lv_pos2 fieldname = 'LGORT' seltext_m = 'Storage Location') TO it_fcat2.
          ADD 1 TO lv_pos2.
          APPEND VALUE #( col_pos = lv_pos2 fieldname = 'MATKL' seltext_m = 'Material Group') TO it_fcat2.
          ADD 1 TO lv_pos2.
          APPEND VALUE #( col_pos = lv_pos2 fieldname = 'BADAT' seltext_m = 'Requisition date') TO it_fcat2.
          ADD 1 TO lv_pos2.
          APPEND VALUE #( col_pos = lv_pos2 fieldname = 'LPEIN' seltext_m = 'Delivery Date') TO it_fcat2.
          ADD 1 TO lv_pos2.
          APPEND VALUE #( col_pos = lv_pos2 fieldname = 'MENGE' seltext_m = 'Quantity') TO it_fcat2.
          ADD 1 TO lv_pos2.
          APPEND VALUE #( col_pos = lv_pos2 fieldname = 'SAKTO' seltext_m = 'G/L Account') TO it_fcat2.
          ADD 1 TO lv_pos2.
          APPEND VALUE #( col_pos = lv_pos2 fieldname = 'KOKRS' seltext_m = 'CO Area') TO it_fcat2.


****Events for Blocked reports.
          DATA: lt_events     TYPE TABLE OF slis_alv_event.

          CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
*           EXPORTING
*             I_LIST_TYPE           = 0
            IMPORTING
              et_events       = lt_events
            EXCEPTIONS
              list_type_wrong = 1
              OTHERS          = 2.

          IF sy-subrc EQ 0.
            READ TABLE lt_events INTO DATA(wa7) INDEX 3.
            IF sy-subrc = 0.
              wa7-form = 'TOP_OF_PAGE_BLK'.
              MODIFY lt_events FROM wa7 INDEX 3.
            ENDIF.
            CLEAR: wa7.
          ENDIF.


****Append data to Blocked Report.
          CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_APPEND'
            EXPORTING
              is_layout                  = wa_blk_layout
              it_fieldcat                = it_fcat2
              i_tabname                  = 'GT_FINAL_EBAN_COPY'
              it_events                  = lt_events
            TABLES
              t_outtab                   = gt_final_eban_copy
            EXCEPTIONS
              program_error              = 1
              maximum_of_appends_reached = 2
              OTHERS                     = 3.

****field catalogue for Material block report.
          DATA:it_fcat3 TYPE TABLE OF slis_fieldcat_alv,
               lv_pos3  TYPE i VALUE 0.

          ADD 1 TO lv_pos3.
          APPEND VALUE #( col_pos = lv_pos3 fieldname = 'MATNR' seltext_m = 'Material Number' key = 'X') TO it_fcat3.
          ADD 1 TO lv_pos3.
          APPEND VALUE #( col_pos = lv_pos3 fieldname = 'MTART' seltext_m = 'Material Type') TO it_fcat3.
          ADD 1 TO lv_pos3.
          APPEND VALUE #( col_pos = lv_pos3 fieldname = 'MBRSH' seltext_m = 'Industry Sector') TO it_fcat3.
          ADD 1 TO lv_pos3.
          APPEND VALUE #( col_pos = lv_pos3 fieldname = 'MATKL' seltext_m = 'Material Group') TO it_fcat3.
          ADD 1 TO lv_pos3.
          APPEND VALUE #( col_pos = lv_pos3 fieldname = 'MEINS' seltext_m = 'BASIC UNIT') TO it_fcat3.
          ADD 1 TO lv_pos3.
          APPEND VALUE #( col_pos = lv_pos3 fieldname = 'MAKTX' seltext_m = 'Material Description') TO it_fcat3.
          ADD 1 TO lv_pos3.
          APPEND VALUE #( col_pos = lv_pos3 fieldname = 'PSTAT' seltext_m = 'Material Status') TO it_fcat3.

****Call function for Block report.
          CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_APPEND'
            EXPORTING
              is_layout                  = wa_blk_layout
              it_fieldcat                = it_fcat3
              i_tabname                  = 'GT_FINAL_MAT'
              it_events                  = lt_events
            TABLES
              t_outtab                   = gt_final_mat
            EXCEPTIONS
              program_error              = 1
              maximum_of_appends_reached = 2
              OTHERS                     = 3.

****Display the Data in Blocked Report Formate.
          IF sy-subrc EQ 0.
            CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_DISPLAY'
*             EXPORTING
*               I_INTERFACE_CHECK             = ' '
*               IS_PRINT                      =
*               I_SCREEN_START_COLUMN         = 0
*               I_SCREEN_START_LINE           = 0
*               I_SCREEN_END_COLUMN           = 0
*               I_SCREEN_END_LINE             = 0
*             IMPORTING
*               E_EXIT_CAUSED_BY_CALLER       =
*               ES_EXIT_CAUSED_BY_USER        =
*             EXCEPTIONS
*               PROGRAM_ERROR                 = 1
*               OTHERS                        = 2
              .

****Failure case in displaying blocked report.
            IF sy-subrc <> 0.
              MESSAGE 'Failed to display in blocked Report.' TYPE 'E'.
            ENDIF.

          ELSE.
            MESSAGE 'call function Block List Append has Failed.' TYPE 'E'.
          ENDIF.

        ENDIF.
      ELSE.
        MESSAGE 'call function Block List Initilization has Failed.' TYPE 'E'.

      ENDIF.

****User Command Upon clicking delete button in PF status.
    WHEN '&REM'.

****Object creation.
      DATA: r_grid TYPE REF TO cl_gui_alv_grid.

****FM which refreshes ALV report in output screen.
      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
        IMPORTING
          e_grid = r_grid.

      IF sy-subrc = 0.
****Collectively both Call method and above call function helps in refrshing the fianl output data.
        CALL METHOD r_grid->check_changed_data.
        IF sy-subrc EQ 0.
****Looping PR interal table to fetch records which are checked.
          LOOP AT gt_final_eban INTO DATA(wa8) WHERE check = 'X'.
            gwa_pr_delete-badat = wa8-badat.
            gwa_pr_delete-banfn = wa8-banfn.
            gwa_pr_delete-bnfpo = wa8-bnfpo.
            gwa_pr_delete-bsart = wa8-bsart.
            gwa_pr_delete-ekgrp = wa8-ekgrp.
            gwa_pr_delete-kokrs = wa8-kokrs.
            gwa_pr_delete-lgort = wa8-lgort.
            gwa_pr_delete-lpein = wa8-lpein.
            gwa_pr_delete-matkl = wa8-matkl.
            gwa_pr_delete-matnr = wa8-matnr.
            gwa_pr_delete-menge = wa8-menge.
            gwa_pr_delete-sakto = wa8-sakto.
            gwa_pr_delete-werks = wa8-werks.

****Appending to internal table which has to be deleted using PR_BAPI.
            APPEND gwa_pr_delete TO gt_pr_delete.

****Clearing Work area and Internal table.
            CLEAR: wa8,gwa_pr_delete.
          ENDLOOP.

          IF gt_pr_delete[] IS NOT INITIAL.

****Declarations for BAPI_REQUISITION_DELETE.
            DATA: lv_pr_number TYPE bapieban-preq_no,
                  lt_delete    TYPE TABLE OF bapieband,
                  lt_msg_log   TYPE TABLE OF bapireturn,
                  wa_delete    TYPE bapieband.

****Deleting Record by Record.
            LOOP AT gt_pr_delete INTO DATA(wa9).
              lv_pr_number = wa9-banfn.

****Mapping the values to BAPI fields.
              wa_delete-preq_item = wa9-bnfpo.
              wa_delete-delete_ind = 'X'.
              wa_delete-closed = 'X'.
              APPEND wa_delete TO lt_delete.

****call BAPI function for deleting the Puchase requisition.
              CALL FUNCTION 'BAPI_REQUISITION_DELETE'
                EXPORTING
                  number                      = lv_pr_number
                TABLES
                  requisition_items_to_delete = lt_delete
                  return                      = lt_msg_log.

****Reading Message log for error log/ success log.
              READ TABLE lt_msg_log INTO DATA(wa10) INDEX 1.
              IF sy-subrc = 0.

****Cheking Message type upon BAPI execution.
                CASE wa10-type.

****For Success and Information case.
                  WHEN 'S' OR 'I'.

****Transaction Commit.
                    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                      EXPORTING
                        wait = 'X'.

                    IF sy-subrc = 0.
                      DATA: lv_fm_name TYPE rs38l_fnam,
                            lwa_delete TYPE zstr_pr_infc,
                            it_delete  TYPE TABLE OF zstr_pr_infc,
                            lv_plant   TYPE ewerk.

                      LOOP AT gt_pr_delete INTO DATA(wa11).
                        lwa_delete-badat = wa11-badat.
                        lwa_delete-banfn = wa11-banfn.
                        lwa_delete-bnfpo = wa11-bnfpo.
                        lwa_delete-matkl = wa11-matkl.
                        lwa_delete-matnr = wa11-matnr.
                        lwa_delete-menge = wa11-menge.
                        lv_plant = wa11-werks.
                        APPEND lwa_delete TO it_delete.

                        CLEAR:wa11.
                      ENDLOOP.


***************************************************************************
****Calling Smart Form to display Purchase Requisition deletion records****
***************************************************************************

                      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
                        EXPORTING
                          formname           = 'ZSF_PRDELETION'
                        IMPORTING
                          fm_name            = lv_fm_name
                        EXCEPTIONS
                          no_form            = 1
                          no_function_module = 2
                          OTHERS             = 3.

                      IF sy-subrc EQ 0.

                        CALL FUNCTION lv_fm_name "'/1BCDWB/SF00001288'
                          EXPORTING
*                           ARCHIVE_INDEX    =
*                           ARCHIVE_INDEX_TAB          =
*                           ARCHIVE_PARAMETERS         =
*                           CONTROL_PARAMETERS         =
*                           MAIL_APPL_OBJ    =
*                           MAIL_RECIPIENT   =
*                           MAIL_SENDER      =
*                           OUTPUT_OPTIONS   =
*                           USER_SETTINGS    = 'X'
                            lv_plant         = lv_plant
*
                          TABLES
                            lt_pr_delete     = it_delete
                          EXCEPTIONS
                            formatting_error = 1
                            internal_error   = 2
                            send_error       = 3
                            user_canceled    = 4
                            OTHERS           = 5.

                        IF sy-subrc eq 0.
                          CLEAR:lv_plant.
                        ENDIF.

                      ELSE.
                        MESSAGE 'Failed to Capture function Module name and Capturing it into Variable.' TYPE 'E'.
* Implement suitable error handling here
                      ENDIF.
                    ENDIF.

****Error Message log upon unsuccessful exection or Deletion of PR's.
                  WHEN OTHERS.

****Layout For Message Log.
                    DATA: wa_msg_layout TYPE slis_layout_alv.
                    wa_msg_layout-zebra = 'X'.
                    wa_msg_layout-colwidth_optimize = 'X'.

****Field Catalogue For Message Log.
                    DATA:it_fcat4 TYPE TABLE OF slis_fieldcat_alv,
                         lv_pos4  TYPE i VALUE 0.

                    ADD 1 TO lv_pos4.
                    APPEND VALUE #( col_pos = lv_pos4 fieldname = 'TYPE' seltext_m = 'Message Type' key = 'X') TO it_fcat4.
                    ADD 1 TO lv_pos4.
                    APPEND VALUE #( col_pos = lv_pos4 fieldname = 'CODE' seltext_m = 'Message Code' key = 'X') TO it_fcat4.
                    ADD 1 TO lv_pos4.
                    APPEND VALUE #( col_pos = lv_pos4 fieldname = 'MESSAGE' seltext_m = 'Message Text' key = 'X') TO it_fcat4.
                    ADD 1 TO lv_pos4.
                    APPEND VALUE #( col_pos = lv_pos4 fieldname = 'LOG_NO' seltext_m = 'Log Number' key = 'X') TO it_fcat4.
                    ADD 1 TO lv_pos4.
                    APPEND VALUE #( col_pos = lv_pos4 fieldname = 'LOG_MSG_NO' seltext_m = 'Log Message Number' key = 'X') TO it_fcat4.

****Call Function ALV Report for message log.
                    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
                      EXPORTING
                        i_callback_program = sy-repid
                        i_grid_title       = 'Message Log For Purchase Requisition Deletion'
                        is_layout          = wa_msg_layout
                        it_fieldcat        = it_fcat4
                      TABLES
                        t_outtab           = lt_msg_log
                      EXCEPTIONS
                        program_error      = 1
                        OTHERS             = 2.

                ENDCASE.
                CLEAR: wa10.
              ENDIF.

              CLEAR: wa9.
            ENDLOOP.

          ENDIF.

        ENDIF.

      ENDIF.

  ENDCASE.

ENDFORM.


************************************************************************
**************PF Status for Material Data*******************************
************************************************************************
FORM zpf_status_blk USING rt_extab TYPE slis_t_extab.

  SET PF-STATUS 'ZPF_STATUS_BLK'.

ENDFORM.


************************************************************************
******************Top Of Page for PR Data Report************************
************************************************************************
FORM top_of_page.

****Data Declerations.
  DATA: lt_comlist TYPE TABLE OF slis_listheader,
        wa_comlist TYPE slis_listheader.

****Report Heading.
  wa_comlist-typ = 'H'.
  wa_comlist-info = TEXT-002.
  APPEND wa_comlist TO lt_comlist.
  CLEAR: wa_comlist.

****Username as heading.
  wa_comlist-typ = 'S'.
  wa_comlist-key = TEXT-003.
  wa_comlist-info = sy-uname.
  APPEND wa_comlist TO lt_comlist.
  CLEAR:wa_comlist.

****Date as Heading.
  wa_comlist-typ = 'S'.
  wa_comlist-key = TEXT-005.
  DATA(lv_year) = sy-datum+0(4).
  DATA(lv_month) = sy-datum+4(2).
  DATA(lv_day) = sy-datum+6(2).
  wa_comlist-info = |{ lv_day }| & |/| & |{ lv_month }| & |/| & |{ lv_year }|.
  APPEND wa_comlist TO lt_comlist.

  CLEAR: lv_year,lv_month,lv_day.

****Plant Name as Heading.
  wa_comlist-typ = 'S'.
  wa_comlist-key = TEXT-004.
  wa_comlist-info = p_plant.
  APPEND wa_comlist TO lt_comlist.
  CLEAR: wa_comlist.

****Call Function for Logo and Heading Part in report.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_comlist
      i_logo             = 'PLANT'.

ENDFORM.


************************************************************************
*****************Top of Page for Blocked Report Data********************
************************************************************************
FORM top_of_page_blk.
  DATA: lt_com TYPE TABLE OF slis_listheader,
        wa_com TYPE slis_listheader.

****Report Heading.
  wa_com-typ = 'H'.
  wa_com-info = TEXT-006.
  APPEND wa_com TO lt_com.
  CLEAR: wa_com.


  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_com.
*      i_logo             = 'PLANT'.
ENDFORM.

************************************************************************
********************User Command for Material Data**********************
************************************************************************
FORM download USING action_1 LIKE sy-ucomm
                            index_1 TYPE slis_selfield.
  IF action_1 EQ '&ARC'.

    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename                = 'Desktop\MM_Report.txt'
        filetype                = 'ASC'
      CHANGING
        data_tab                = gt_final_mat
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        not_supported_by_gui    = 22
        error_no_gui            = 23
        OTHERS                  = 24.

    IF sy-subrc <> 0.
      MESSAGE 'Unable to download report.' TYPE 'E'.
    ELSE.
      MESSAGE 'Materail Report Downloaded' TYPE 'S'.
    ENDIF.

  ENDIF.

ENDFORM.
