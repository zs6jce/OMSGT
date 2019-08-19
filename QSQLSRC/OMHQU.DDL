--  Generate SQL
--  Version:                   V7R3M0 160422
--  Generated on:              14-03-19 09:41:54
--  Relational Database:
--  Standards Option:          Db2 for i

CREATE TABLE DOMS03DDTA/OMHQU (
--  SQL150B   10   REUSEDLT(*NO) in table OMHQU in DOMS03DDTA ignored.
  QUHEDC CHAR(5) CCSID 37 NOT NULL DEFAULT '' ,
  QUQUE# DECIMAL(5, 0) NOT NULL DEFAULT 0 ,
  QUQUEI CHAR(1) CCSID 37 NOT NULL DEFAULT '' ,
  QUQGRC CHAR(10) CCSID 37 NOT NULL DEFAULT '' ,
  QUQUES CHAR(256) CCSID 37 NOT NULL DEFAULT '' )

  RCDFMT OMHQUR     ;

LABEL ON TABLE DOMS03DDTA/OMHQU
  IS 'OMS - HelpDesk Question' ;

LABEL ON COLUMN DOMS03DDTA/OMHQU
( QUHEDC IS 'HEDC' ,
  QUQUE# IS 'QUE#' ,
  QUQUEI IS 'QUEI' ,
  QUQGRC IS 'QGRC' ,
  QUQUES IS 'QUES' ) ;

LABEL ON COLUMN DOMS03DDTA/OMHQU
( QUHEDC TEXT IS 'Helpdesk code' ,
  QUQUE# TEXT IS 'Question number' ,
  QUQUEI TEXT IS 'Question indicator' ,
  QUQGRC TEXT IS 'Question group code' ,
  QUQUES TEXT IS 'Question' ) ;

