*&---------------------------------------------------------------------*
*& Report ZISSUE_BLOCKED
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zissue_blocked.

INCLUDE zissue_blocked_dd.
INCLUDE zissue_blocked_ss.
INCLUDE zissue_blocked_validation.

START-OF-SELECTION.
  PERFORM get_data.
