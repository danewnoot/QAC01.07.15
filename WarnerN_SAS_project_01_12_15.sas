/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Friday, January 09, 2015     TIME: 3:41:33 PM
PROJECT: WarnerN_SAS_project_01_12_15
PROJECT PATH: P:\QAC\qac200\students\nwarner\Assignments\WarnerN_SAS_project_01_12_15.egp
---------------------------------------- */

/* Library assignment for Local.NATDATA */
Libname NATDATA BASE 'P:\QAC\qac200\students\nwarner' ;
/* Library assignment for Local.NATDATA */
Libname NATDATA BASE 'P:\QAC\qac200\students\nwarner' ;


/* Conditionally delete set of tables or views, if they exists          */
/* If the member does not exist, then no action is performed   */
%macro _eg_conditional_dropds /parmbuff;
	
   	%local num;
   	%local stepneeded;
   	%local stepstarted;
   	%local dsname;
	%local name;

   	%let num=1;
	/* flags to determine whether a PROC SQL step is needed */
	/* or even started yet                                  */
	%let stepneeded=0;
	%let stepstarted=0;
   	%let dsname= %qscan(&syspbuff,&num,',()');
	%do %while(&dsname ne);	
		%let name = %sysfunc(left(&dsname));
		%if %qsysfunc(exist(&name)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;

			%end;
				drop table &name;
		%end;

		%if %sysfunc(exist(&name,view)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;
			%end;
				drop view &name;
		%end;
		%let num=%eval(&num+1);
      	%let dsname=%qscan(&syspbuff,&num,',()');
	%end;
	%if &stepstarted %then %do;
		quit;
	%end;
%mend _eg_conditional_dropds;


/* Build where clauses from stored process parameters */

%macro _eg_WhereParam( COLUMN, PARM, OPERATOR, TYPE=S, MATCHALL=_ALL_VALUES_, MATCHALL_CLAUSE=1, MAX= , IS_EXPLICIT=0);

  %local q1 q2 sq1 sq2;
  %local isEmpty;
  %local isEqual isNotEqual;
  %local isIn isNotIn;
  %local isString;
  %local isBetween;

  %let isEqual = ("%QUPCASE(&OPERATOR)" = "EQ" OR "&OPERATOR" = "=");
  %let isNotEqual = ("%QUPCASE(&OPERATOR)" = "NE" OR "&OPERATOR" = "<>");
  %let isIn = ("%QUPCASE(&OPERATOR)" = "IN");
  %let isNotIn = ("%QUPCASE(&OPERATOR)" = "NOT IN");
  %let isString = (%QUPCASE(&TYPE) eq S or %QUPCASE(&TYPE) eq STRING );
  %if &isString %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%");
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq D or %QUPCASE(&TYPE) eq DATE %then 
  %do;
    %let q1=%str(%");
    %let q2=%str(%"d);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq T or %QUPCASE(&TYPE) eq TIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"t);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq DT or %QUPCASE(&TYPE) eq DATETIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"dt);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else
  %do;
    %let q1=;
    %let q2=;
	%let sq1=;
    %let sq2=;
  %end;
  
  %if "&PARM" = "" %then %let PARM=&COLUMN;

  %let isBetween = ("%QUPCASE(&OPERATOR)"="BETWEEN" or "%QUPCASE(&OPERATOR)"="NOT BETWEEN");

  %if "&MAX" = "" %then %do;
    %let MAX = &parm._MAX;
    %if &isBetween %then %let PARM = &parm._MIN;
  %end;

  %if not %symexist(&PARM) or (&isBetween and not %symexist(&MAX)) %then %do;
    %if &IS_EXPLICIT=0 %then %do;
		not &MATCHALL_CLAUSE
	%end;
	%else %do;
	    not 1=1
	%end;
  %end;
  %else %if "%qupcase(&&&PARM)" = "%qupcase(&MATCHALL)" %then %do;
    %if &IS_EXPLICIT=0 %then %do;
	    &MATCHALL_CLAUSE
	%end;
	%else %do;
	    1=1
	%end;	
  %end;
  %else %if (not %symexist(&PARM._count)) or &isBetween %then %do;
    %let isEmpty = ("&&&PARM" = "");
    %if (&isEqual AND &isEmpty AND &isString) %then
       &COLUMN is null;
    %else %if (&isNotEqual AND &isEmpty AND &isString) %then
       &COLUMN is not null;
    %else %do;
	   %if &IS_EXPLICIT=0 %then %do;
           &COLUMN &OPERATOR %unquote(&q1)&&&PARM%unquote(&q2)
	   %end;
	   %else %do;
	       &COLUMN &OPERATOR %unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2))
	   %end;
       %if &isBetween %then 
          AND %unquote(&q1)&&&MAX%unquote(&q2);
    %end;
  %end;
  %else 
  %do;
	%local emptyList;
  	%let emptyList = %symexist(&PARM._count);
  	%if &emptyList %then %let emptyList = &&&PARM._count = 0;
	%if (&emptyList) %then
	%do;
		%if (&isNotin) %then
		   1;
		%else
			0;
	%end;
	%else %if (&&&PARM._count = 1) %then 
    %do;
      %let isEmpty = ("&&&PARM" = "");
      %if (&isIn AND &isEmpty AND &isString) %then
        &COLUMN is null;
      %else %if (&isNotin AND &isEmpty AND &isString) %then
        &COLUMN is not null;
      %else %do;
	    %if &IS_EXPLICIT=0 %then %do;
            &COLUMN &OPERATOR (%unquote(&q1)&&&PARM%unquote(&q2))
	    %end;
		%else %do;
		    &COLUMN &OPERATOR (%unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2)))
		%end;
	  %end;
    %end;
    %else 
    %do;
       %local addIsNull addIsNotNull addComma;
       %let addIsNull = %eval(0);
       %let addIsNotNull = %eval(0);
       %let addComma = %eval(0);
       (&COLUMN &OPERATOR ( 
       %do i=1 %to &&&PARM._count; 
          %let isEmpty = ("&&&PARM&i" = "");
          %if (&isString AND &isEmpty AND (&isIn OR &isNotIn)) %then
          %do;
             %if (&isIn) %then %let addIsNull = 1;
             %else %let addIsNotNull = 1;
          %end;
          %else
          %do;		     
            %if &addComma %then %do;,%end;
			%if &IS_EXPLICIT=0 %then %do;
                %unquote(&q1)&&&PARM&i%unquote(&q2) 
			%end;
			%else %do;
			    %unquote(%nrstr(&sq1))&&&PARM&i%unquote(%nrstr(&sq2)) 
			%end;
            %let addComma = %eval(1);
          %end;
       %end;) 
       %if &addIsNull %then OR &COLUMN is null;
       %else %if &addIsNotNull %then AND &COLUMN is not null;
       %do;)
       %end;
    %end;
  %end;
%mend;

/* save the current settings of XPIXELS and YPIXELS */
/* so that they can be restored later               */
%macro _sas_pushchartsize(new_xsize, new_ysize);
	%global _savedxpixels _savedypixels;
	options nonotes;
	proc sql noprint;
	select setting into :_savedxpixels
	from sashelp.vgopt
	where optname eq "XPIXELS";
	select setting into :_savedypixels
	from sashelp.vgopt
	where optname eq "YPIXELS";
	quit;
	options notes;
	GOPTIONS XPIXELS=&new_xsize YPIXELS=&new_ysize;
%mend;

/* restore the previous values for XPIXELS and YPIXELS */
%macro _sas_popchartsize;
	%if %symexist(_savedxpixels) %then %do;
		GOPTIONS XPIXELS=&_savedxpixels YPIXELS=&_savedypixels;
		%symdel _savedxpixels / nowarn;
		%symdel _savedypixels / nowarn;
	%end;
%mend;

/* ---------------------------------- */
/* MACRO: enterpriseguide             */
/* PURPOSE: define a macro variable   */
/*   that contains the file system    */
/*   path of the WORK library on the  */
/*   server.  Note that different     */
/*   logic is needed depending on the */
/*   server type.                     */
/* ---------------------------------- */
%macro enterpriseguide;
%global sasworklocation;
%local tempdsn unique_dsn path;

%if &sysscp=OS %then %do; /* MVS Server */
	%if %sysfunc(getoption(filesystem))=MVS %then %do;
        /* By default, physical file name will be considered a classic MVS data set. */
	    /* Construct dsn that will be unique for each concurrent session under a particular account: */
		filename egtemp '&egtemp' disp=(new,delete); /* create a temporary data set */
 		%let tempdsn=%sysfunc(pathname(egtemp)); /* get dsn */
		filename egtemp clear; /* get rid of data set - we only wanted its name */
		%let unique_dsn=".EGTEMP.%substr(&tempdsn, 1, 16).PDSE"; 
		filename egtmpdir &unique_dsn
			disp=(new,delete,delete) space=(cyl,(5,5,50))
			dsorg=po dsntype=library recfm=vb
			lrecl=8000 blksize=8004 ;
		options fileext=ignore ;
	%end; 
 	%else %do; 
        /* 
		By default, physical file name will be considered an HFS 
		(hierarchical file system) file. 
		*/
		%if "%sysfunc(getoption(filetempdir))"="" %then %do;
			filename egtmpdir '/tmp';
		%end;
		%else %do;
			filename egtmpdir "%sysfunc(getoption(filetempdir))";
		%end;
	%end; 
	%let path=%sysfunc(pathname(egtmpdir));
    %let sasworklocation=%sysfunc(quote(&path));  
%end; /* MVS Server */
%else %do;
	%let sasworklocation = "%sysfunc(getoption(work))/";
%end;
%if &sysscp=VMS_AXP %then %do; /* Alpha VMS server */
	%let sasworklocation = "%sysfunc(getoption(work))";                         
%end;
%if &sysscp=CMS %then %do; 
	%let path = %sysfunc(getoption(work));                         
	%let sasworklocation = "%substr(&path, %index(&path,%str( )))";
%end;
%mend enterpriseguide;

%enterpriseguide

ODS PROCTITLE;
OPTIONS DEV=ACTIVEX;
GOPTIONS XPIXELS=0 YPIXELS=0;
FILENAME EGSRX TEMP;
ODS tagsets.sasreport13(ID=EGSRX) FILE=EGSRX
    STYLE=HtmlBlue
    STYLESHEET=(URL="file:///C:/Program%20Files/SASHome/SASEnterpriseGuide/6.1/Styles/HtmlBlue.css")
    NOGTITLE
    NOGFOOTNOTE
    GPATH=&sasworklocation
    ENCODING=UTF8
    options(rolap="on")
;

/*   START OF NODE: Assign Project Library (NATDATA)   */
%LET _CLIENTTASKLABEL='Assign Project Library (NATDATA)';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\nwarner\Assignments\WarnerN_SAS_project_01_12_15.egp';
%LET _CLIENTPROJECTNAME='WarnerN_SAS_project_01_12_15.egp';

GOPTIONS ACCESSIBLE;
LIBNAME NATDATA BASE "P:\QAC\qac200\students\nwarner" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: VarriablesAdult01.07.15   */
%LET _CLIENTTASKLABEL='VarriablesAdult01.07.15';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\nwarner\Assignments\WarnerN_SAS_project_01_12_15.egp';
%LET _CLIENTPROJECTNAME='WarnerN_SAS_project_01_12_15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(NATDATA.'NatVarriablesAdults01.07.15'n);

PROC SQL;
   CREATE TABLE NATDATA.'NatVarriablesAdults01.07.15'n(label="NatVarriablesAdults01.07.15") AS 
   SELECT t1.AMNURS12, 
          t1.AMDRC12, 
          t1.AMTOTC12, 
          t1.ERTOT12, 
          t1.IPDIS12, 
          t1.OBASST12, 
          t1.OBDRV12, 
          t1.OPDRV12, 
          t1.AMASST12, 
          t1.AGE12X, 
          t1.ANGIDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.CANCERDX, 
          t1.REGION12, 
          t1.CHDDX, 
          t1.AFDC12, 
          t1.EICRDT12, 
          t1.SAQELIG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.MCDEV12, 
          t1.FAMINC12, 
          t1.SAQWT12F, 
          t1.INSURC12, 
          t1.MIDX, 
          t1.HIBPDX, 
          t1.INTVLANG, 
          t1.MARRY12X, 
          t1.DUPERSID, 
          t1.DIVDP12X, 
          t1.SSIP12X, 
          t1.TTLP12X, 
          t1.RACETHX, 
          t1.ADAPPT42, 
          t1.ADEXPL42, 
          t1.ADLIST42, 
          t1.ADTLHW42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADILWW42, 
          t1.ADRTWW42, 
          t1.ADFFRM42, 
          t1.ADRTCR42, 
          t1.ADSPEC42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADNSMK42, 
          t1.ADEGMC42, 
          t1.ADSPRF42, 
          t1.ADILCR42, 
          t1.ADNDCR42, 
          t1.ADDPRS42, 
          t1.ADINTR42, 
          t1.PHQ242, 
          t1.ADDRBP42, 
          t1.ADHOPE42, 
          t1.ADNERV42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADWRTH42, 
          t1.ADEFRT42, 
          t1.K6SUM42, 
          t1.ADCAPE42, 
          t1.ADDOWN42, 
          t1.ADNRGY42, 
          t1.ADSOCA42, 
          t1.ADMALS42, 
          t1.ADPALS42, 
          t1.ADPAIN42, 
          t1.ADMWLM42, 
          t1.ADPWLM42, 
          t1.ADOVER42, 
          t1.ADSMOK42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADINSA42, 
          t1.ADGENH42, 
          t1.ADINSB42, 
          t1.ADCLIM42, 
          t1.ADDAYA42, 
          t1.ADLANG42, 
          t1.ADRISK42, 
          t1.SFFLAG42, 
          t1.ADPRX42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.SEX, 
          t1.STRKDX, 
          t1.ERDEXP12, 
          t1.OBDEXP12, 
          t1.OBNEXP12, 
          t1.OPSEXP12, 
          t1.AMNEXP12, 
          t1.EDUYRDEG, 
          t1.EDRECODE
      FROM EC100006.meps_fullyr_2012 t1
      WHERE t1.AGE12X >= 18
      ORDER BY t1.DUPERSID;
QUIT;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Code For VarriablesAdult01_07_15   */
%LET SYSLAST=NATDATA.NATVARRIABLESADULTS01.07.15;
%LET _CLIENTTASKLABEL='Code For VarriablesAdult01_07_15';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\nwarner\Assignments\WarnerN_SAS_project_01_12_15.egp';
%LET _CLIENTPROJECTNAME='WarnerN_SAS_project_01_12_15.egp';
%LET _SASPROGRAMFILE='C:\Users\nwarner\Desktop\nwarner\SASProgramCode\WarnerN_SAS_projectcode\Code For VarriablesAdult01_07_15.sas';

GOPTIONS ACCESSIBLE;

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Wednesday, January 07, 2015 at 2:38:19 PM
   By task: VarriablesAdult01.07.15

   Input Data: Local:NATDATA.NATVARRIABLESADULTS01.07.15
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsForNATVARRIABLESADUL);
TITLE "Data set attributes for sub set data" ;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=NATDATA.'NATVARRIABLESADULTS01.07.15'n ;

RUN;





GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;


/*   START OF NODE: One-Way Frequencies for 2012 adult MEPS Subset   */
%LET _CLIENTTASKLABEL='One-Way Frequencies for 2012 adult MEPS Subset';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\nwarner\Assignments\WarnerN_SAS_project_01_12_15.egp';
%LET _CLIENTPROJECTNAME='WarnerN_SAS_project_01_12_15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 3:35:17 PM
   By task: One-Way Frequencies for 2012 adult MEPS Subset

   Input Data: Local:NATDATA.NATVARRIABLESADULTS01.07.15
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:NATDATA.NATVARRIABLESADULTS01.07.15
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.DIVDP12X, T.SSIP12X, T.TTLP12X, T.RACETHX, T.ADAPPT42, T.ADEXPL42, T.ADLIST42, T.ADTLHW42, T.ADINST42, T.ADEZUN42, T.ADRESP42, T.ADPRTM42, T.ADILWW42, T.ADRTWW42, T.ADFFRM42, T.ADRTCR42, T.ADSPEC42, T.ADFHLP42, T.ADHECR42
		     , T.ADNSMK42, T.ADEGMC42, T.ADSPRF42, T.ADILCR42, T.ADNDCR42, T.ADDPRS42, T.ADINTR42, T.PHQ242, T.ADDRBP42, T.ADHOPE42, T.ADNERV42, T.ADREST42, T.ADSAD42, T.ADWRTH42, T.ADEFRT42, T.K6SUM42, T.ADCAPE42, T.ADDOWN42, T.ADNRGY42
		     , T.ADSOCA42, T.ADMALS42, T.ADPALS42, T.ADPAIN42, T.ADMWLM42, T.ADPWLM42, T.ADOVER42, T.ADSMOK42, T.ADCMPD42, T.ADCMPM42, T.ADCMPY42, T.ADINSA42, T.ADGENH42, T.ADINSB42, T.ADCLIM42, T.ADDAYA42, T.ADLANG42, T.ADRISK42, T.SFFLAG42
		     , T.ADPRX42, T.MCS42, T.PCS42, T.SEX, T.STRKDX, T.ERDEXP12, T.OBDEXP12, T.OBNEXP12, T.OPSEXP12, T.AMNEXP12, T.EDUYRDEG, T.AMNURS12, T.AMDRC12, T.AMTOTC12, T.ERTOT12, T.IPDIS12, T.OBASST12, T.OBDRV12, T.OPDRV12, T.AMASST12
		     , T.AGE12X, T.ANGIDX, T.ARTHDX, T.ASTHDX, T.CANCERDX, T.REGION12, T.CHDDX, T.AFDC12, T.EICRDT12, T.SAQELIG, T.EMPST31, T.EMPST42, T.EMPST53, T.MCDEV12, T.FAMINC12, T.SAQWT12F, T.INSURC12, T.MIDX, T.HIBPDX, T.INTVLANG
		     , T.MARRY12X
	FROM NATDATA.'NATVARRIABLESADULTS01.07.15'n(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results for MEPS 2012 adults (scaled down to 99 varriables, inc DUPERSID)";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
FOOTNOTE3 "by Nat Warner! =)";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES DIVDP12X / MISSPRINT  SCORES=TABLE;
	TABLES SSIP12X / MISSPRINT  SCORES=TABLE;
	TABLES TTLP12X / MISSPRINT  SCORES=TABLE;
	TABLES RACETHX / MISSPRINT  SCORES=TABLE;
	TABLES ADAPPT42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEXPL42 / MISSPRINT  SCORES=TABLE;
	TABLES ADLIST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADTLHW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEZUN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRESP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPRTM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADILWW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRTWW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADFFRM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRTCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSPEC42 / MISSPRINT  SCORES=TABLE;
	TABLES ADFHLP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADHECR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNSMK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEGMC42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSPRF42 / MISSPRINT  SCORES=TABLE;
	TABLES ADILCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNDCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDPRS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINTR42 / MISSPRINT  SCORES=TABLE;
	TABLES PHQ242 / MISSPRINT  SCORES=TABLE;
	TABLES ADDRBP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADHOPE42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNERV42 / MISSPRINT  SCORES=TABLE;
	TABLES ADREST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSAD42 / MISSPRINT  SCORES=TABLE;
	TABLES ADWRTH42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEFRT42 / MISSPRINT  SCORES=TABLE;
	TABLES K6SUM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCAPE42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDOWN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNRGY42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSOCA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADMALS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPALS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPAIN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADMWLM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPWLM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADOVER42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSMOK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPD42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPY42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINSA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADGENH42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINSB42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCLIM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDAYA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADLANG42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRISK42 / MISSPRINT  SCORES=TABLE;
	TABLES SFFLAG42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPRX42 / MISSPRINT  SCORES=TABLE;
	TABLES MCS42 / MISSPRINT  SCORES=TABLE;
	TABLES PCS42 / MISSPRINT  SCORES=TABLE;
	TABLES SEX / MISSPRINT  SCORES=TABLE;
	TABLES STRKDX / MISSPRINT  SCORES=TABLE;
	TABLES ERDEXP12 / MISSPRINT  SCORES=TABLE;
	TABLES OBDEXP12 / MISSPRINT  SCORES=TABLE;
	TABLES OBNEXP12 / MISSPRINT  SCORES=TABLE;
	TABLES OPSEXP12 / MISSPRINT  SCORES=TABLE;
	TABLES AMNEXP12 / MISSPRINT  SCORES=TABLE;
	TABLES EDUYRDEG / MISSPRINT  SCORES=TABLE;
	TABLES AMNURS12 / MISSPRINT  SCORES=TABLE;
	TABLES AMDRC12 / MISSPRINT  SCORES=TABLE;
	TABLES AMTOTC12 / MISSPRINT  SCORES=TABLE;
	TABLES ERTOT12 / MISSPRINT  SCORES=TABLE;
	TABLES IPDIS12 / MISSPRINT  SCORES=TABLE;
	TABLES OBASST12 / MISSPRINT  SCORES=TABLE;
	TABLES OBDRV12 / MISSPRINT  SCORES=TABLE;
	TABLES OPDRV12 / MISSPRINT  SCORES=TABLE;
	TABLES AMASST12 / MISSPRINT  SCORES=TABLE;
	TABLES AGE12X / MISSPRINT  SCORES=TABLE;
	TABLES ANGIDX / MISSPRINT  SCORES=TABLE;
	TABLES ARTHDX / MISSPRINT  SCORES=TABLE;
	TABLES ASTHDX / MISSPRINT  SCORES=TABLE;
	TABLES CANCERDX / MISSPRINT  SCORES=TABLE;
	TABLES REGION12 / MISSPRINT  SCORES=TABLE;
	TABLES CHDDX / MISSPRINT  SCORES=TABLE;
	TABLES AFDC12 / MISSPRINT  SCORES=TABLE;
	TABLES EICRDT12 / MISSPRINT  SCORES=TABLE;
	TABLES SAQELIG / MISSPRINT  SCORES=TABLE;
	TABLES EMPST31 / MISSPRINT  SCORES=TABLE;
	TABLES EMPST42 / MISSPRINT  SCORES=TABLE;
	TABLES EMPST53 / MISSPRINT  SCORES=TABLE;
	TABLES MCDEV12 / MISSPRINT  SCORES=TABLE;
	TABLES FAMINC12 / MISSPRINT  SCORES=TABLE;
	TABLES SAQWT12F / MISSPRINT  SCORES=TABLE;
	TABLES INSURC12 / MISSPRINT  SCORES=TABLE;
	TABLES MIDX / MISSPRINT  SCORES=TABLE;
	TABLES HIBPDX / MISSPRINT  SCORES=TABLE;
	TABLES INTVLANG / MISSPRINT  SCORES=TABLE;
	TABLES MARRY12X / MISSPRINT  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: RecodeVarriables   */
%LET _CLIENTTASKLABEL='RecodeVarriables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\nwarner\Assignments\WarnerN_SAS_project_01_12_15.egp';
%LET _CLIENTPROJECTNAME='WarnerN_SAS_project_01_12_15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_NATVARRIABLES_MANAGED);

PROC SQL;
   CREATE TABLE WORK."QUERY_FOR_NATVARRIABLES_MANAGED"n AS 
   SELECT t1.AMNURS12, 
          t1.AMDRC12, 
          t1.AMTOTC12, 
          t1.ERTOT12, 
          t1.IPDIS12, 
          t1.OBASST12, 
          t1.OBDRV12, 
          t1.OPDRV12, 
          t1.AMASST12, 
          t1.AGE12X, 
          t1.ANGIDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.EDRECODE, 
          t1.CANCERDX, 
          t1.REGION12, 
          t1.CHDDX, 
          t1.AFDC12, 
          t1.EICRDT12, 
          t1.SAQELIG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.MCDEV12, 
          t1.FAMINC12, 
          t1.SAQWT12F, 
          t1.INSURC12, 
          t1.MIDX, 
          t1.HIBPDX, 
          t1.INTVLANG, 
          t1.MARRY12X, 
          t1.DUPERSID, 
          t1.DIVDP12X, 
          t1.SSIP12X, 
          t1.TTLP12X, 
          t1.RACETHX, 
          t1.ADAPPT42, 
          t1.ADEXPL42, 
          t1.ADLIST42, 
          t1.ADTLHW42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADILWW42, 
          t1.ADRTWW42, 
          t1.ADFFRM42, 
          t1.ADRTCR42, 
          t1.ADSPEC42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADNSMK42, 
          t1.ADEGMC42, 
          t1.ADSPRF42, 
          t1.ADILCR42, 
          t1.ADNDCR42, 
          t1.ADDPRS42, 
          t1.ADINTR42, 
          t1.PHQ242, 
          t1.ADDRBP42, 
          t1.ADHOPE42, 
          t1.ADNERV42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADWRTH42, 
          t1.ADEFRT42, 
          t1.K6SUM42, 
          t1.ADCAPE42, 
          t1.ADDOWN42, 
          t1.ADNRGY42, 
          t1.ADSOCA42, 
          t1.ADMALS42, 
          t1.ADPALS42, 
          t1.ADPAIN42, 
          t1.ADMWLM42, 
          t1.ADPWLM42, 
          t1.ADOVER42, 
          t1.ADSMOK42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADINSA42, 
          t1.ADGENH42, 
          t1.ADINSB42, 
          t1.ADCLIM42, 
          t1.ADDAYA42, 
          t1.ADLANG42, 
          t1.ADRISK42, 
          t1.SFFLAG42, 
          t1.ADPRX42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.SEX, 
          t1.STRKDX, 
          t1.ERDEXP12, 
          t1.OBDEXP12, 
          t1.OBNEXP12, 
          t1.OPSEXP12, 
          t1.AMNEXP12, 
          t1.EDUYRDEG, 
          /* DOWN/DEPR */
            (CASE 
               WHEN -1 = t1.ADDOWN42 THEN .
               WHEN -8 = t1.ADDOWN42 THEN .
               WHEN -9 = t1.ADDOWN42 THEN .
               ELSE t1.ADDOWN42
            END) LABEL="ADDOWN42 SAQ: down/depr last 4wks (recoded minus missing resp)" AS 'DOWN/DEPR'n, 
          /* CALM/PEACEFUL */
            (CASE 
               WHEN 1 = t1.ADCAPE42 THEN 5
               WHEN -1 = t1.ADCAPE42 THEN .
               WHEN 2 = t1.ADCAPE42 THEN 4
               WHEN 4 = t1.ADCAPE42 THEN 2
               WHEN 5 = t1.ADCAPE42 THEN 1
               WHEN -8 = t1.ADCAPE42 THEN .
               WHEN -9 = t1.ADCAPE42 THEN .
               ELSE t1.ADCAPE42
            END) LABEL="ADCAPE42 SAQ 4wks: calm/peace (recoded minus missing resp)" AS 'CALM/PEACEFUL'n, 
          /* ENERGY */
            (CASE 
               WHEN 1 = t1.ADNRGY42 THEN 5
               WHEN -1 = t1.ADNRGY42 THEN .
               WHEN 2 = t1.ADNRGY42 THEN 4
               WHEN 4 = t1.ADNRGY42 THEN 2
               WHEN 5 = t1.ADNRGY42 THEN 1
               WHEN -9 = t1.ADNRGY42 THEN .
               ELSE t1.ADNRGY42
            END) LABEL="ADNRGY42 SAQ 4wks lots of energy (recoded minus missing resp)" AS ENERGY, 
          /* HEALTH PROHIBITED SOC */
            (CASE 
               WHEN -1 = t1.ADSOCA42 THEN .
               WHEN -9 = t1.ADSOCA42 THEN .
               ELSE t1.ADSOCA42
            END) LABEL="ADSOCA42 SAQ 4wks health stopped social activities (recoded minus missing resp)" AS 
            'HEALTH PROHIBITED SOC'n, 
          /* PAIN LIMITS WORK */
            (CASE 
               WHEN 1 = t1.ADPAIN42 THEN 5
               WHEN -1 = t1.ADPAIN42 THEN .
               WHEN 2 = t1.ADPAIN42 THEN 4
               WHEN 4 = t1.ADPAIN42 THEN 2
               WHEN 5 = t1.ADPAIN42 THEN 1
               WHEN -9 = t1.ADPAIN42 THEN .
               ELSE t1.ADPAIN42
            END) LABEL="ADPAIN42 SAQ 4wks pain limits normal work (recoded minus missing resp)" AS 'PAIN LIMITS WORK'n, 
          /* MNT PROBS WORK LIMIT */
            (CASE 
               WHEN -1 = t1.ADMWLM42 THEN .
               WHEN -7 = t1.ADMWLM42 THEN .
               WHEN -8 = t1.ADMWLM42 THEN .
               WHEN -9 = t1.ADMWLM42 THEN .
               ELSE t1.ADMWLM42
            END) LABEL="ADMWLM42 mental problems limit work (recoded minus missing resp)" AS 'MNT PROBS WORK LIMIT'n, 
          /* HLTH LIMITS MOD ACT */
            (CASE 
               WHEN -1 = t1.ADDAYA42 THEN .
               WHEN -9 = t1.ADDAYA42 THEN .
               ELSE t1.ADDAYA42
            END) LABEL="ADDAYA42 SAQ: health limits moderate activities (recoded minus missing resp)" AS 
            'HLTH LIMITS MOD ACT'n, 
          /* HLTH LIMITS STAIRS */
            (CASE 
               WHEN -1 = t1.ADCLIM42 THEN .
               WHEN -8 = t1.ADCLIM42 THEN .
               WHEN -9 = t1.ADCLIM42 THEN .
               ELSE t1.ADCLIM42
            END) LABEL="ADCLIM42 SAQ: health limits ability to climb stairs (recoded minus missing resp)" AS 
            'HLTH LIMITS STAIRS'n, 
          /* GENERAL HLTH */
            (CASE 
               WHEN 1 = t1.ADGENH42 THEN 5
               WHEN -1 = t1.ADGENH42 THEN .
               WHEN 2 = t1.ADGENH42 THEN 4
               WHEN 4 = t1.ADGENH42 THEN 2
               WHEN 5 = t1.ADGENH42 THEN 1
               WHEN -8 = t1.ADGENH42 THEN .
               WHEN -9 = t1.ADGENH42 THEN .
               ELSE t1.ADGENH42
            END) LABEL="ADGENH42 SAQ: health in general (ex-poor) (recoded minus missing resp)" AS 'GENERAL HLTH'n, 
          /* ACCMP LESS BC PHY PROBS */
            (CASE 
               WHEN -1 = t1.ADPALS42 THEN .
               WHEN -9 = t1.ADPALS42 THEN .
               ELSE t1.ADPALS42
            END) LABEL="ADPALS42 SAQ: 4wks acomplished less because of physical problems (recoded minus missing resp)" 
            AS 'ACCMP LESS BC PHY PROBS'n, 
          /* ACOMP LESS BS MNT PROBS */
            (CASE 
               WHEN -1 = t1.ADMALS42 THEN .
               WHEN -9 = t1.ADMALS42 THEN .
               ELSE t1.ADMALS42
            END) LABEL="ADMALS42 SAQ: 4wks acomplished less because of mental problems (recoded minus missing resp)" AS 
            'ACOMP LESS BS MNT PROBS'n, 
          /* PHY PROBS LIMIT WORK */
            (CASE 
               WHEN -1 = t1.ADPWLM42 THEN .
               WHEN -9 = t1.ADPWLM42 THEN .
               ELSE t1.ADPWLM42
            END) LABEL=
            "ADPWLM42 SAQ: 4wks physical problems limit work (oh nooooooo!!!!!!!!!!) (recoded minus missing resp)" AS 
            'PHY PROBS LIMIT WORK'n, 
          /* MARITAL */
            (CASE 
               WHEN -7 = t1.MARRY12X THEN .
               WHEN -9 = t1.MARRY12X THEN .
            END) LABEL="MARRY12X marital status (recoded minus missing resp)" AS MARITAL, 
          /* EDU */
            (CASE 
               WHEN -1 = t1.EDUYRDEG THEN .
               WHEN -7 = t1.EDUYRDEG THEN .
               WHEN -8 = t1.EDUYRDEG THEN .
               WHEN -9 = t1.EDUYRDEG THEN .
               ELSE t1.EDUYRDEG
            END) LABEL="EDUYRDEG education years and degrees (recoded minus missing resp)" AS EDU, 
          /* EMPLY31 */
            (CASE 
               WHEN -1 = t1.EMPST31 THEN .
               WHEN -7 = t1.EMPST31 THEN .
               WHEN -8 = t1.EMPST31 THEN .
               WHEN -9 = t1.EMPST31 THEN .
               ELSE t1.EMPST31
            END) LABEL="EMPST31 employment status on 3/1/12 (recoded minus missing resp)" AS EMPLY31, 
          /* EMPLY42 */
            (CASE 
               WHEN -1 = t1.EMPST42 THEN .
               WHEN -7 = t1.EMPST42 THEN .
               WHEN -8 = t1.EMPST42 THEN .
               WHEN -9 = t1.EMPST42 THEN .
               ELSE t1.EMPST42
            END) LABEL="EMPST42 employment status on 4/2/12 (recoded minus missing resp)" AS EMPLY42, 
          /* EMPLY53 */
            (CASE 
               WHEN -7 = t1.EMPST53 THEN .
               WHEN -8 = t1.EMPST53 THEN .
               WHEN -9 = t1.EMPST53 THEN .
               ELSE t1.EMPST53
            END) LABEL="EMPST53 employment status on 5/3/12" AS EMPLY53, 
          /* CANCER */
            (CASE 
               WHEN -7 = t1.CANCERDX THEN .
               WHEN -8 = t1.CANCERDX THEN .
               WHEN -9 = t1.CANCERDX THEN .
               ELSE t1.CANCERDX
            END) LABEL="CANCERDX any cancer diagnosis (recoded minus missing resp)" AS CANCER, 
          /* ASTHMA */
            (CASE 
               WHEN -7 = t1.ASTHDX THEN .
               WHEN -8 = t1.ASTHDX THEN .
               WHEN -9 = t1.ASTHDX THEN .
               ELSE t1.ASTHDX
            END) LABEL="ASTHDX asthma diagnosis (recoded minus missing resp)" AS ASTHMA, 
          /* ARTHRITIS */
            (CASE 
               WHEN -7 = t1.ARTHDX THEN .
               WHEN -8 = t1.ARTHDX THEN .
               WHEN -9 = t1.ARTHDX THEN .
               ELSE t1.ARTHDX
            END) LABEL="ARTHDX arthritis diagnosis (recoded minus missing resp)" AS ARTHRITIS, 
          /* HIBLOODPRESS */
            (CASE 
               WHEN -7 = t1.HIBPDX THEN .
               WHEN -8 = t1.HIBPDX THEN .
               WHEN -9 = t1.HIBPDX THEN .
               ELSE t1.HIBPDX
            END) LABEL="HIBPDX high blood pressure diagnosis (recoded minus missing resp)" AS HIBLOODPRESS, 
          /* STROKE */
            (CASE 
               WHEN -7 = t1.STRKDX THEN .
               WHEN -8 = t1.STRKDX THEN .
               WHEN -9 = t1.STRKDX THEN .
               ELSE t1.STRKDX
            END) LABEL="STRKDX stroke diagnosis (recoded minus missing resp)" AS STROKE, 
          /* HEARTATTK */
            (CASE 
               WHEN -7 = t1.MIDX THEN .
               WHEN -8 = t1.MIDX THEN .
               WHEN -9 = t1.MIDX THEN .
               ELSE t1.MIDX
            END) LABEL="MIDX heart attack diagnosis (recoded minus missing resp)" AS HEARTATTK, 
          /* ANGINA */
            (CASE 
               WHEN -7 = t1.ANGIDX THEN .
               WHEN -8 = t1.ANGIDX THEN .
               WHEN -9 = t1.ANGIDX THEN .
               ELSE t1.ANGIDX
            END) LABEL="ANGIDX angina diagnosis (recoded minus missing resp)" AS ANGINA, 
          /* EITC */
            (CASE 
               WHEN -1 = t1.EICRDT12 THEN .
               WHEN -7 = t1.EICRDT12 THEN .
               WHEN -8 = t1.EICRDT12 THEN .
               WHEN -9 = t1.EICRDT12 THEN .
               ELSE t1.EICRDT12
            END) LABEL="EICRDT12 did/will recieve earned income tax credit (recoded minus missing resp)" AS EITC, 
          /* MEDVISITSFORCARE */
            (CASE 
               WHEN -1 = t1.ADAPPT42 THEN .
               WHEN -8 = t1.ADAPPT42 THEN .
               WHEN -9 = t1.ADAPPT42 THEN .
               ELSE t1.ADAPPT42
            END) LABEL="ADAPPT42 SAQ: 12mo # visits to medical office for care (recoded minus missing resp)" AS 
            MEDVISITSFORCARE, 
          /* EASEGETTINGCARE */
            (CASE 
               WHEN -1 = t1.ADEGMC42 THEN .
               WHEN -9 = t1.ADEGMC42 THEN .
               ELSE t1.ADEGMC42
            END) LABEL="ADEGMC42 SAQ: 12mo ease getting needed medical care (recoded minus missing resp)" AS 
            EASEGETTINGCARE, 
          /* HCARERATING */
            (CASE 
               WHEN -1 = t1.ADHECR42 THEN .
               WHEN -9 = t1.ADHECR42 THEN .
               ELSE t1.ADHECR42
            END) LABEL="ADHECR42 SAQ: 12mo rating of health care (recoded minus missing resp)" AS HCARERATING, 
          /* DRLISTENED */
            (CASE 
               WHEN -1 = t1.ADLIST42 THEN .
               WHEN -7 = t1.ADLIST42 THEN .
               WHEN -9 = t1.ADLIST42 THEN .
               ELSE t1.ADLIST42
            END) LABEL="ADLIST42 SAQ: 12mo doctor listened to you (recoded minus missing resp)" AS DRLISTENED, 
          /* CARENEEDED */
            (CASE 
               WHEN -1 = t1.ADNDCR42 THEN .
               WHEN -8 = t1.ADNDCR42 THEN .
               WHEN -9 = t1.ADNDCR42 THEN .
               ELSE t1.ADNDCR42
            END) LABEL="ADNDCR42 SAQ: 12mo needed any care, treatment, test (recoded minus missing resp)" AS CARENEEDED, 
          /* FREQALLEFFORT */
            (CASE 
               WHEN -1 = t1.ADEFRT42 THEN .
               WHEN -7 = t1.ADEFRT42 THEN .
               WHEN -8 = t1.ADEFRT42 THEN .
               WHEN -9 = t1.ADEFRT42 THEN .
               ELSE t1.ADEFRT42
            END) LABEL="ADEFRT42 SAQ: 30 days how often everything an effort" AS FREQALLEFFORT, 
          /* FEELINGS */
            (CASE 
               WHEN -1 = t1.K6SUM42 THEN .
               WHEN -9 = t1.K6SUM42 THEN .
               ELSE t1.K6SUM42
            END) LABEL="K6SUM42 SAQ: 30 days overall rating of feelings (recoded minus missing resp)" AS FEELINGS, 
          /* MENTALSUM */
            (CASE 
               WHEN -1 = t1.MCS42 THEN .
               WHEN -9 = t1.MCS42 THEN .
               ELSE t1.MCS42
            END) LABEL="MCS42 SAQ: mental component summary SF-12v2 (recoded minus missing resp)" AS MENTALSUM, 
          /* PHYSUM */
            (CASE 
               WHEN -1 = t1.PCS42 THEN .
               WHEN -9 = t1.PCS42 THEN .
               ELSE t1.PCS42
            END) LABEL="PCS42 SAQ: physical component summary SF-12v2 (recoded minus missing resp)" AS PHYSUM, 
          /* EDUC */
            (CASE 
               WHEN -7 = t1.EDRECODE THEN .
               WHEN -8 = t1.EDRECODE THEN .
               WHEN -9 = t1.EDRECODE THEN .
               ELSE t1.EDRECODE
            END) LABEL="EDRECODE recoded to remove useless varriables" AS EDUC
      FROM NATDATA.'NATVARRIABLESADULTS01.07.15'n t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis to confirm some recoded varriables   */
%LET _CLIENTTASKLABEL='Table Analysis to confirm some recoded varriables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\nwarner\Assignments\WarnerN_SAS_project_01_12_15.egp';
%LET _CLIENTPROJECTNAME='WarnerN_SAS_project_01_12_15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 3:35:30 PM
   By task: Table Analysis to confirm some recoded varriables

   Input Data: Local:WORK.QUERY_FOR_NATVARRIABLES_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_NATVARRIABLES_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.CANCERDX, T.EDUYRDEG, T.CANCER, T.EDU, T.EICRDT12, T.EITC, T.EASEGETTINGCARE, T.ADEGMC42, T.DRLISTENED, T.ADLIST42
	FROM WORK.QUERY_FOR_NATVARRIABLES_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES CANCERDX * CANCER /
		NOROW
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES EDUYRDEG * EDU /
		NOROW
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADEGMC42 * EASEGETTINGCARE /
		NOROW
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES EICRDT12 * EITC /
		NOROW
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADLIST42 * DRLISTENED /
		NOROW
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: AggSF-12V2   */
%LET _CLIENTTASKLABEL='AggSF-12V2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\nwarner\Assignments\WarnerN_SAS_project_01_12_15.egp';
%LET _CLIENTPROJECTNAME='WarnerN_SAS_project_01_12_15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.RECODEDVARRIABLES_AGV);

PROC SQL;
   CREATE TABLE WORK."RECODEDVARRIABLES_AGV"n AS 
   SELECT t1.AMNURS12, 
          t1.AMDRC12, 
          t1.AMTOTC12, 
          t1.ERTOT12, 
          t1.IPDIS12, 
          t1.OBASST12, 
          t1.OBDRV12, 
          t1.OPDRV12, 
          t1.AMASST12, 
          t1.EDRECODE, 
          t1.EDRECODE AS EDRECODE1, 
          t1.AGE12X, 
          t1.EDUC, 
          t1.ANGIDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.CANCERDX, 
          t1.REGION12, 
          t1.CHDDX, 
          t1.AFDC12, 
          t1.EICRDT12, 
          t1.SAQELIG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.MCDEV12, 
          t1.FAMINC12, 
          t1.SAQWT12F, 
          t1.INSURC12, 
          t1.MIDX, 
          t1.HIBPDX, 
          t1.INTVLANG, 
          t1.MARRY12X, 
          t1.DUPERSID, 
          t1.DIVDP12X, 
          t1.SSIP12X, 
          t1.TTLP12X, 
          t1.RACETHX, 
          t1.ADAPPT42, 
          t1.ADEXPL42, 
          t1.ADLIST42, 
          t1.ADTLHW42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADILWW42, 
          t1.ADRTWW42, 
          t1.ADFFRM42, 
          t1.ADRTCR42, 
          t1.ADSPEC42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADNSMK42, 
          t1.ADEGMC42, 
          t1.ADSPRF42, 
          t1.ADILCR42, 
          t1.ADNDCR42, 
          t1.ADDPRS42, 
          t1.ADINTR42, 
          t1.PHQ242, 
          t1.ADDRBP42, 
          t1.ADHOPE42, 
          t1.ADNERV42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADWRTH42, 
          t1.ADEFRT42, 
          t1.K6SUM42, 
          t1.ADCAPE42, 
          t1.ADDOWN42, 
          t1.ADNRGY42, 
          t1.ADSOCA42, 
          t1.ADMALS42, 
          t1.ADPALS42, 
          t1.ADPAIN42, 
          t1.ADMWLM42, 
          t1.ADPWLM42, 
          t1.ADOVER42, 
          t1.ADSMOK42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADINSA42, 
          t1.ADGENH42, 
          t1.ADINSB42, 
          t1.ADCLIM42, 
          t1.ADDAYA42, 
          t1.ADLANG42, 
          t1.ADRISK42, 
          t1.SFFLAG42, 
          t1.ADPRX42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.SEX, 
          t1.STRKDX, 
          t1.ERDEXP12, 
          t1.OBDEXP12, 
          t1.OBNEXP12, 
          t1.OPSEXP12, 
          t1.AMNEXP12, 
          t1.EDUYRDEG, 
          t1.'DOWN/DEPR'n, 
          t1.'CALM/PEACEFUL'n, 
          t1.ENERGY, 
          t1.'HEALTH PROHIBITED SOC'n, 
          t1.'PAIN LIMITS WORK'n, 
          t1.'MNT PROBS WORK LIMIT'n, 
          t1.'HLTH LIMITS MOD ACT'n, 
          t1.'HLTH LIMITS STAIRS'n, 
          t1.'GENERAL HLTH'n, 
          t1.'ACCMP LESS BC PHY PROBS'n, 
          t1.'ACOMP LESS BS MNT PROBS'n, 
          t1.'PHY PROBS LIMIT WORK'n, 
          t1.MARITAL, 
          t1.EDU, 
          t1.EMPLY31, 
          t1.EMPLY42, 
          t1.EMPLY53, 
          t1.CANCER, 
          t1.ASTHMA, 
          t1.ARTHRITIS, 
          t1.HIBLOODPRESS, 
          t1.STROKE, 
          t1.HEARTATTK, 
          t1.ANGINA, 
          t1.EITC, 
          t1.MEDVISITSFORCARE, 
          t1.EASEGETTINGCARE, 
          t1.HCARERATING, 
          t1.DRLISTENED, 
          t1.CARENEEDED, 
          t1.FREQALLEFFORT, 
          t1.FEELINGS, 
          t1.MENTALSUM, 
          t1.PHYSUM, 
          /* SUM_SF-12V2 */
            (SUM(t1.'GENERAL HLTH'n,t1.'ACCMP LESS BC PHY PROBS'n,t1.'ACOMP LESS BS MNT PROBS'n,t1.
            'PHY PROBS LIMIT WORK'n,t1.'HLTH LIMITS STAIRS'n,t1.'HLTH LIMITS MOD ACT'n,t1.'MNT PROBS WORK LIMIT'n,t1.
            'PAIN LIMITS WORK'n,t1.'HEALTH PROHIBITED SOC'n,t1.'CALM/PEACEFUL'n,t1.ENERGY,t1.'DOWN/DEPR'n)) LABEL=
            "Sum of recoded SF-12V2 varriables" AS 'SUM_SF-12V2'n
      FROM WORK.QUERY_FOR_NATVARRIABLES_MANAGED t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data to confirm integrity of SF-12V2 aggvar   */
%LET _CLIENTTASKLABEL='List Data to confirm integrity of SF-12V2 aggvar';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\nwarner\Assignments\WarnerN_SAS_project_01_12_15.egp';
%LET _CLIENTPROJECTNAME='WarnerN_SAS_project_01_12_15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 3:35:31 PM
   By task: List Data to confirm integrity of SF-12V2 aggvar

   Input Data: Local:WORK.RECODEDVARRIABLES_AGV
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.RECODEDVARRIABLES_AGV
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T."SUM_SF-12V2"n, T."DOWN/DEPR"n, T."CALM/PEACEFUL"n, T.ENERGY, T."HEALTH PROHIBITED SOC"n, T."PAIN LIMITS WORK"n, T."MNT PROBS WORK LIMIT"n, T."HLTH LIMITS MOD ACT"n, T."HLTH LIMITS STAIRS"n, T."GENERAL HLTH"n
		     , T."ACCMP LESS BC PHY PROBS"n, T."ACOMP LESS BS MNT PROBS"n, T."PHY PROBS LIMIT WORK"n
	FROM WORK.RECODEDVARRIABLES_AGV as T
;
QUIT;
TITLE;
TITLE1 "Agg SF-12V2 confirmation";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Nat Warner";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=100)
	OBS="Row number"
	LABEL
	;
	VAR "SUM_SF-12V2"n "DOWN/DEPR"n "CALM/PEACEFUL"n ENERGY "HEALTH PROHIBITED SOC"n "PAIN LIMITS WORK"n "MNT PROBS WORK LIMIT"n "HLTH LIMITS MOD ACT"n "HLTH LIMITS STAIRS"n "GENERAL HLTH"n "ACCMP LESS BC PHY PROBS"n "ACOMP LESS BS MNT PROBS"n
	  "PHY PROBS LIMIT WORK"n;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Summary Statistics  to determine how to create SF-12V2 categorical varriable   */
%LET _CLIENTTASKLABEL='Summary Statistics  to determine how to create SF-12V2 categorical varriable';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\nwarner\Assignments\WarnerN_SAS_project_01_12_15.egp';
%LET _CLIENTPROJECTNAME='WarnerN_SAS_project_01_12_15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 3:35:31 PM
   By task: Summary Statistics  to determine how to create SF-12V2 categorical varriable

   Input Data: Local:WORK.RECODEDVARRIABLES_AGV
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.RECODEDVARRIABLES_AGV
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T."SUM_SF-12V2"n
	FROM WORK.RECODEDVARRIABLES_AGV as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results for 2012 MEPS Adults SF-12V2";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Nat Warner";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MIN 
		MAX 
		MODE 
		N	
		Q1 
		MEDIAN 
		Q3	;
	VAR "SUM_SF-12V2"n;

RUN;
ODS GRAPHICS ON;
TITLE;
/*-----------------------------------------------------
 * Use PROC UNIVARIATE to generate the histograms.
 */

TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Histograms";
PROC UNIVARIATE DATA=WORK.SORTTempTableSorted	NOPRINT	;
	VAR "SUM_SF-12V2"n;

			HISTOGRAM ;

RUN; QUIT;
ODS GRAPHICS OFF;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis to determine how to create SF-12V2 categorical varriable   */
%LET _CLIENTTASKLABEL='Distribution Analysis to determine how to create SF-12V2 categorical varriable';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\nwarner\Assignments\WarnerN_SAS_project_01_12_15.egp';
%LET _CLIENTPROJECTNAME='WarnerN_SAS_project_01_12_15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 3:35:32 PM
   By task: Distribution Analysis to determine how to create SF-12V2 categorical varriable

   Input Data: Local:WORK.RECODEDVARRIABLES_AGV
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.RECODEDVARRIABLES_AGV
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T."SUM_SF-12V2"n
	FROM WORK.RECODEDVARRIABLES_AGV as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: SUM_SF-12V2";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR "SUM_SF-12V2"n;
	HISTOGRAM / 	CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	 
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder to create SF-12V2 categorical varriable   */
%LET _CLIENTTASKLABEL='Query Builder to create SF-12V2 categorical varriable';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\nwarner\Assignments\WarnerN_SAS_project_01_12_15.egp';
%LET _CLIENTPROJECTNAME='WarnerN_SAS_project_01_12_15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_RECODEDVARRIABLES_AGV);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_RECODEDVARRIABLES_AGV AS 
   SELECT /* SF-12V2_categorical */
            (CASE  
               WHEN t1.'SUM_SF-12V2'n >=52
               THEN 4
            WHEN t1.'SUM_SF-12V2'n >=48 and t1.'SUM_SF-12V2'n <52
               THEN 3
            WHEN t1.'SUM_SF-12V2'n >=41 and t1.'SUM_SF-12V2'n <48
               THEN 2
            WHEN t1.'SUM_SF-12V2'n <41
            THEN 1
            END) LABEL="SUM_SF-12V2 categorical by quartiles; 4 is best health and 1 is worst" AS 'SF-12V2_categorical'n, 
          t1.AMNURS12, 
          t1.AMDRC12, 
          t1.AMTOTC12, 
          t1.ERTOT12, 
          t1.IPDIS12, 
          t1.OBASST12, 
          t1.OBDRV12, 
          t1.OPDRV12, 
          t1.EDRECODE, 
          t1.EDUC AS EDUC1, 
          t1.AMASST12, 
          t1.EDUC, 
          t1.AGE12X, 
          t1.ANGIDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.CANCERDX, 
          t1.REGION12, 
          t1.CHDDX, 
          t1.AFDC12, 
          t1.EICRDT12, 
          t1.SAQELIG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.MCDEV12, 
          t1.FAMINC12, 
          t1.SAQWT12F, 
          t1.INSURC12, 
          t1.MIDX, 
          t1.HIBPDX, 
          t1.INTVLANG, 
          t1.MARRY12X, 
          t1.DUPERSID, 
          t1.DIVDP12X, 
          t1.SSIP12X, 
          t1.TTLP12X, 
          t1.RACETHX, 
          t1.ADAPPT42, 
          t1.ADEXPL42, 
          t1.ADLIST42, 
          t1.ADTLHW42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADILWW42, 
          t1.ADRTWW42, 
          t1.ADFFRM42, 
          t1.ADRTCR42, 
          t1.ADSPEC42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADNSMK42, 
          t1.ADEGMC42, 
          t1.ADSPRF42, 
          t1.ADILCR42, 
          t1.ADNDCR42, 
          t1.ADDPRS42, 
          t1.ADINTR42, 
          t1.PHQ242, 
          t1.ADDRBP42, 
          t1.ADHOPE42, 
          t1.ADNERV42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADWRTH42, 
          t1.ADEFRT42, 
          t1.K6SUM42, 
          t1.ADCAPE42, 
          t1.ADDOWN42, 
          t1.ADNRGY42, 
          t1.ADSOCA42, 
          t1.ADMALS42, 
          t1.ADPALS42, 
          t1.ADPAIN42, 
          t1.ADMWLM42, 
          t1.ADPWLM42, 
          t1.ADOVER42, 
          t1.ADSMOK42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADINSA42, 
          t1.ADGENH42, 
          t1.ADINSB42, 
          t1.ADCLIM42, 
          t1.ADDAYA42, 
          t1.ADLANG42, 
          t1.ADRISK42, 
          t1.SFFLAG42, 
          t1.ADPRX42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.SEX, 
          t1.STRKDX, 
          t1.ERDEXP12, 
          t1.OBDEXP12, 
          t1.OBNEXP12, 
          t1.OPSEXP12, 
          t1.AMNEXP12, 
          t1.EDUYRDEG, 
          t1.'DOWN/DEPR'n, 
          t1.'CALM/PEACEFUL'n, 
          t1.ENERGY, 
          t1.'HEALTH PROHIBITED SOC'n, 
          t1.'PAIN LIMITS WORK'n, 
          t1.'MNT PROBS WORK LIMIT'n, 
          t1.'HLTH LIMITS MOD ACT'n, 
          t1.'HLTH LIMITS STAIRS'n, 
          t1.'GENERAL HLTH'n, 
          t1.'ACCMP LESS BC PHY PROBS'n, 
          t1.'ACOMP LESS BS MNT PROBS'n, 
          t1.'PHY PROBS LIMIT WORK'n, 
          t1.MARITAL, 
          t1.EDU, 
          t1.EMPLY31, 
          t1.EMPLY42, 
          t1.EMPLY53, 
          t1.CANCER, 
          t1.ASTHMA, 
          t1.ARTHRITIS, 
          t1.HIBLOODPRESS, 
          t1.STROKE, 
          t1.HEARTATTK, 
          t1.ANGINA, 
          t1.EITC, 
          t1.MEDVISITSFORCARE, 
          t1.EASEGETTINGCARE, 
          t1.HCARERATING, 
          t1.DRLISTENED, 
          t1.CARENEEDED, 
          t1.FREQALLEFFORT, 
          t1.FEELINGS, 
          t1.MENTALSUM, 
          t1.PHYSUM, 
          t1.'SUM_SF-12V2'n
      FROM WORK.RECODEDVARRIABLES_AGV t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies comparing cat to originoal agg   */
%LET _CLIENTTASKLABEL='One-Way Frequencies comparing cat to originoal agg';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\nwarner\Assignments\WarnerN_SAS_project_01_12_15.egp';
%LET _CLIENTPROJECTNAME='WarnerN_SAS_project_01_12_15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 3:35:32 PM
   By task: One-Way Frequencies comparing cat to originoal agg

   Input Data: Local:WORK.QUERY_FOR_RECODEDVARRIABLES_AGV
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_RECODEDVARRIABLES_AGV
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T."SF-12V2_categorical"n, T."SUM_SF-12V2"n
	FROM WORK.QUERY_FOR_RECODEDVARRIABLES_AGV as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES "SF-12V2_categorical"n / MISSPRINT  SCORES=TABLE;
	TABLES "SUM_SF-12V2"n / MISSPRINT  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder to create AGG_PATIENTCONTACT   */
%LET _CLIENTTASKLABEL='Query Builder to create AGG_PATIENTCONTACT';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\nwarner\Assignments\WarnerN_SAS_project_01_12_15.egp';
%LET _CLIENTPROJECTNAME='WarnerN_SAS_project_01_12_15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_RECODEDVARRIABLES_0001);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_RECODEDVARRIABLES_0001 AS 
   SELECT t1.'SF-12V2_categorical'n, 
          t1.AMNURS12, 
          t1.AMDRC12, 
          t1.AMTOTC12, 
          t1.ERTOT12, 
          t1.IPDIS12, 
          t1.OBASST12, 
          t1.OBDRV12, 
          t1.EDRECODE AS EDRECODE1, 
          t1.EDRECODE, 
          t1.EDUC, 
          t1.OPDRV12, 
          t1.AMASST12, 
          t1.AGE12X, 
          t1.ANGIDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.CANCERDX, 
          t1.REGION12, 
          t1.CHDDX, 
          t1.AFDC12, 
          t1.EICRDT12, 
          t1.SAQELIG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.MCDEV12, 
          t1.FAMINC12, 
          t1.SAQWT12F, 
          t1.INSURC12, 
          t1.MIDX, 
          t1.HIBPDX, 
          t1.INTVLANG, 
          t1.MARRY12X, 
          t1.DUPERSID, 
          t1.DIVDP12X, 
          t1.SSIP12X, 
          t1.TTLP12X, 
          t1.RACETHX, 
          t1.ADAPPT42, 
          t1.ADEXPL42, 
          t1.ADLIST42, 
          t1.ADTLHW42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADILWW42, 
          t1.ADRTWW42, 
          t1.ADFFRM42, 
          t1.ADRTCR42, 
          t1.ADSPEC42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADNSMK42, 
          t1.ADEGMC42, 
          t1.ADSPRF42, 
          t1.ADILCR42, 
          t1.ADNDCR42, 
          t1.ADDPRS42, 
          t1.ADINTR42, 
          t1.PHQ242, 
          t1.ADDRBP42, 
          t1.ADHOPE42, 
          t1.ADNERV42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADWRTH42, 
          t1.ADEFRT42, 
          t1.K6SUM42, 
          t1.ADCAPE42, 
          t1.ADDOWN42, 
          t1.ADNRGY42, 
          t1.ADSOCA42, 
          t1.ADMALS42, 
          t1.ADPALS42, 
          t1.ADPAIN42, 
          t1.ADMWLM42, 
          t1.ADPWLM42, 
          t1.ADOVER42, 
          t1.ADSMOK42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADINSA42, 
          t1.ADGENH42, 
          t1.ADINSB42, 
          t1.ADCLIM42, 
          t1.ADDAYA42, 
          t1.ADLANG42, 
          t1.ADRISK42, 
          t1.SFFLAG42, 
          t1.ADPRX42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.SEX, 
          t1.STRKDX, 
          t1.ERDEXP12, 
          t1.OBDEXP12, 
          t1.OBNEXP12, 
          t1.OPSEXP12, 
          t1.AMNEXP12, 
          t1.EDUYRDEG, 
          t1.'DOWN/DEPR'n, 
          t1.'CALM/PEACEFUL'n, 
          t1.ENERGY, 
          t1.'HEALTH PROHIBITED SOC'n, 
          t1.'PAIN LIMITS WORK'n, 
          t1.'MNT PROBS WORK LIMIT'n, 
          t1.'HLTH LIMITS MOD ACT'n, 
          t1.'HLTH LIMITS STAIRS'n, 
          t1.'GENERAL HLTH'n, 
          t1.'ACCMP LESS BC PHY PROBS'n, 
          t1.'ACOMP LESS BS MNT PROBS'n, 
          t1.'PHY PROBS LIMIT WORK'n, 
          t1.MARITAL, 
          t1.EDU, 
          t1.EMPLY31, 
          t1.EMPLY42, 
          t1.EMPLY53, 
          t1.CANCER, 
          t1.ASTHMA, 
          t1.ARTHRITIS, 
          t1.HIBLOODPRESS, 
          t1.STROKE, 
          t1.HEARTATTK, 
          t1.ANGINA, 
          t1.EITC, 
          t1.MEDVISITSFORCARE, 
          t1.EASEGETTINGCARE, 
          t1.HCARERATING, 
          t1.DRLISTENED, 
          t1.CARENEEDED, 
          t1.FREQALLEFFORT, 
          t1.FEELINGS, 
          t1.MENTALSUM, 
          t1.PHYSUM, 
          t1.'SUM_SF-12V2'n, 
          /* AGG_PATIENTCONTACT */
            (SUM(t1.AMASST12,t1.IPDIS12,t1.OBASST12,t1.AMDRC12,t1.OBDRV12,t1.OPDRV12,t1.AMNURS12,t1.AMTOTC12)) LABEL=
            "Total patient contact w/ care provider" AS AGG_PATIENTCONTACT
      FROM WORK.QUERY_FOR_RECODEDVARRIABLES_AGV t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data to confirm new AGG_PATIENTCONTACT varriable   */
%LET _CLIENTTASKLABEL='List Data to confirm new AGG_PATIENTCONTACT varriable';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\nwarner\Assignments\WarnerN_SAS_project_01_12_15.egp';
%LET _CLIENTPROJECTNAME='WarnerN_SAS_project_01_12_15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 3:35:32 PM
   By task: List Data to confirm new AGG_PATIENTCONTACT varriable

   Input Data: Local:WORK.QUERY_FOR_RECODEDVARRIABLES_0001
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_RECODEDVARRIABLES_0001
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.AMNURS12, T.AMDRC12, T.AMTOTC12, T.IPDIS12, T.OBASST12, T.OBDRV12, T.OPDRV12, T.AMASST12, T.AGG_PATIENTCONTACT
	FROM WORK.QUERY_FOR_RECODEDVARRIABLES_0001 as T
;
QUIT;
TITLE;
TITLE1 "Report Listing Comparative: AGG_PATIENTCONTACT with parent varriables";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Nat Warner";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=100)
	OBS="Row number"
	LABEL
	;
	VAR AMNURS12 AMDRC12 AMTOTC12 IPDIS12 OBASST12 OBDRV12 OPDRV12 AMASST12 AGG_PATIENTCONTACT;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis to determine how to construct categorical patient contact varriable   */
%LET _CLIENTTASKLABEL='Distribution Analysis to determine how to construct categorical patient contact varriable';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\nwarner\Assignments\WarnerN_SAS_project_01_12_15.egp';
%LET _CLIENTPROJECTNAME='WarnerN_SAS_project_01_12_15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 3:35:33 PM
   By task: Distribution Analysis to determine how to construct categorical patient contact varriable

   Input Data: Local:WORK.QUERY_FOR_RECODEDVARRIABLES_0001
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_RECODEDVARRIABLES_0001
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.AGG_PATIENTCONTACT, T.MARRY12X, T.EDUC
	FROM WORK.QUERY_FOR_RECODEDVARRIABLES_0001 as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: AGG_PATIENTCONTACT";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Nat Warner";
	ODS EXCLUDE EXTREMEOBS MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
	FREQ
	MODES
;
	VAR AGG_PATIENTCONTACT MARRY12X EDUC;
	HISTOGRAM / 	CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	 
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder   */
%LET _CLIENTTASKLABEL='Query Builder';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\nwarner\Assignments\WarnerN_SAS_project_01_12_15.egp';
%LET _CLIENTPROJECTNAME='WarnerN_SAS_project_01_12_15.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_RECODEDVARRIABLES);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_RECODEDVARRIABLES AS 
   SELECT /* PATIENTCONTACT_CATEGORICAL */
            (CASE  
               WHEN t1.AGG_PATIENTCONTACT =0
               THEN 1
               WHEN t1.AGG_PATIENTCONTACT =1
               THEN 2
               WHEN t1.AGG_PATIENTCONTACT =2 or t1.AGG_PATIENTCONTACT =3
               THEN 3
               WHEN t1.AGG_PATIENTCONTACT =4 or t1.AGG_PATIENTCONTACT =5 or t1.AGG_PATIENTCONTACT =6 or 
            t1.AGG_PATIENTCONTACT =7
               THEN 4
               WHEN t1.AGG_PATIENTCONTACT >7 and t1.AGG_PATIENTCONTACT <=10
               THEN 5
               WHEN t1.AGG_PATIENTCONTACT >10 and t1.AGG_PATIENTCONTACT <=20
               THEN 6
               WHEN t1.AGG_PATIENTCONTACT >20 and t1.AGG_PATIENTCONTACT <=30
               THEN 7
               WHEN t1.AGG_PATIENTCONTACT >30
               THEN 8
            END) LABEL="categorical patient contact: 1=0, 2=1, 3=2/3, 4=4/5/6/7, 5=8-10, 6=11-20, 7=21-30, 8=30+" AS 
            PATIENTCONTACT_CATEGORICAL, 
          t1.'SF-12V2_categorical'n, 
          t1.AMNURS12, 
          t1.AMDRC12, 
          t1.AMTOTC12, 
          t1.ERTOT12, 
          t1.IPDIS12, 
          t1.EDUC, 
          t1.OBASST12, 
          t1.EDRECODE, 
          t1.OBDRV12, 
          t1.OPDRV12, 
          t1.AMASST12, 
          t1.AGE12X, 
          t1.ANGIDX, 
          t1.ARTHDX, 
          t1.ASTHDX, 
          t1.CANCERDX, 
          t1.REGION12, 
          t1.CHDDX, 
          t1.AFDC12, 
          t1.EICRDT12, 
          t1.SAQELIG, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.MCDEV12, 
          t1.FAMINC12, 
          t1.SAQWT12F, 
          t1.INSURC12, 
          t1.MIDX, 
          t1.HIBPDX, 
          t1.INTVLANG, 
          t1.MARRY12X, 
          t1.DUPERSID, 
          t1.DIVDP12X, 
          t1.SSIP12X, 
          t1.TTLP12X, 
          t1.RACETHX, 
          t1.ADAPPT42, 
          t1.ADEXPL42, 
          t1.ADLIST42, 
          t1.ADTLHW42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADILWW42, 
          t1.ADRTWW42, 
          t1.ADFFRM42, 
          t1.ADRTCR42, 
          t1.ADSPEC42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADNSMK42, 
          t1.ADEGMC42, 
          t1.ADSPRF42, 
          t1.ADILCR42, 
          t1.ADNDCR42, 
          t1.ADDPRS42, 
          t1.ADINTR42, 
          t1.PHQ242, 
          t1.ADDRBP42, 
          t1.ADHOPE42, 
          t1.ADNERV42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADWRTH42, 
          t1.ADEFRT42, 
          t1.K6SUM42, 
          t1.ADCAPE42, 
          t1.ADDOWN42, 
          t1.ADNRGY42, 
          t1.ADSOCA42, 
          t1.ADMALS42, 
          t1.ADPALS42, 
          t1.ADPAIN42, 
          t1.ADMWLM42, 
          t1.ADPWLM42, 
          t1.ADOVER42, 
          t1.ADSMOK42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADINSA42, 
          t1.ADGENH42, 
          t1.ADINSB42, 
          t1.ADCLIM42, 
          t1.ADDAYA42, 
          t1.ADLANG42, 
          t1.ADRISK42, 
          t1.SFFLAG42, 
          t1.ADPRX42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.SEX, 
          t1.STRKDX, 
          t1.ERDEXP12, 
          t1.OBDEXP12, 
          t1.OBNEXP12, 
          t1.OPSEXP12, 
          t1.AMNEXP12, 
          t1.EDUYRDEG, 
          t1.'DOWN/DEPR'n, 
          t1.'CALM/PEACEFUL'n, 
          t1.ENERGY, 
          t1.'HEALTH PROHIBITED SOC'n, 
          t1.'PAIN LIMITS WORK'n, 
          t1.'MNT PROBS WORK LIMIT'n, 
          t1.'HLTH LIMITS MOD ACT'n, 
          t1.'HLTH LIMITS STAIRS'n, 
          t1.'GENERAL HLTH'n, 
          t1.'ACCMP LESS BC PHY PROBS'n, 
          t1.'ACOMP LESS BS MNT PROBS'n, 
          t1.'PHY PROBS LIMIT WORK'n, 
          t1.MARITAL, 
          t1.EDU, 
          t1.EMPLY31, 
          t1.EMPLY42, 
          t1.EMPLY53, 
          t1.CANCER, 
          t1.ASTHMA, 
          t1.ARTHRITIS, 
          t1.HIBLOODPRESS, 
          t1.STROKE, 
          t1.HEARTATTK, 
          t1.ANGINA, 
          t1.EITC, 
          t1.MEDVISITSFORCARE, 
          t1.EASEGETTINGCARE, 
          t1.HCARERATING, 
          t1.DRLISTENED, 
          t1.CARENEEDED, 
          t1.FREQALLEFFORT, 
          t1.FEELINGS, 
          t1.MENTALSUM, 
          t1.PHYSUM, 
          t1.'SUM_SF-12V2'n, 
          t1.AGG_PATIENTCONTACT
      FROM WORK.QUERY_FOR_RECODEDVARRIABLES_0001 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies comparing cat to agg patient contact   */
%LET _CLIENTTASKLABEL='One-Way Frequencies comparing cat to agg patient contact';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\nwarner\Assignments\WarnerN_SAS_project_01_12_15.egp';
%LET _CLIENTPROJECTNAME='WarnerN_SAS_project_01_12_15.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 3:35:33 PM
   By task: One-Way Frequencies comparing cat to agg patient contact

   Input Data: Local:WORK.QUERY_FOR_RECODEDVARRIABLES
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_RECODEDVARRIABLES
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.PATIENTCONTACT_CATEGORICAL, T.AGG_PATIENTCONTACT
	FROM WORK.QUERY_FOR_RECODEDVARRIABLES as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results Comparing AGG to CAT varriables";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Nat Warner";
ODS GRAPHICS ON;
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES PATIENTCONTACT_CATEGORICAL / MISSPRINT  SCORES=TABLE plots(only)=freq;
	TABLES AGG_PATIENTCONTACT / MISSPRINT  SCORES=TABLE plots(only)=freq;
RUN;
ODS GRAPHICS OFF;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: OrigionalData01.07.15   */
%LET _CLIENTTASKLABEL='OrigionalData01.07.15';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\nwarner\Assignments\WarnerN_SAS_project_01_12_15.egp';
%LET _CLIENTPROJECTNAME='WarnerN_SAS_project_01_12_15.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\Data\MEPS";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 3:35:36 PM
   By task: OrigionalData01.07.15

   Input Data: P:\QAC\qac200\Data\MEPS\meps_fullyr_2012.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsFormeps_fullyr_2012);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=ECLIB000.meps_fullyr_2012 OUT=WORK.SUCOUT1;

RUN;

DATA WORK.CONTContentsFormeps_fullyr_2012(LABEL="Contents Details for meps_fullyr_2012");
   SET WORK.SUCOUT1;
RUN;

PROC DELETE DATA=WORK.SUCOUT1;
RUN;

%LET _LINESIZE=%SYSFUNC(GETOPTION(LINESIZE));

PROC SQL;
CREATE VIEW WORK.SCVIEW AS 
	SELECT DISTINCT memname LABEL="Table Name", 
			memlabel LABEL="Label", 
			memtype LABEL="Type", 
			crdate LABEL="Date Created", 
			modate LABEL="Date Modified", 
			nobs LABEL="Number of Obs.", 
			charset LABEL="Char. Set", 
			protect LABEL="Password Protected", 
			typemem LABEL="Data Set Type" FROM WORK.CONTContentsFormeps_fullyr_2012
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WORK.CONTContentsFormeps_fullyr_2012 OUT=WORK.CONTContentsFormeps_fullyr_2012;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.CONTContentsFormeps_fullyr_2012
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

PROC REPORT DATA=WORK.SCTABLE NOWINDOWS; 
   FORMAT TYPE _EG_VARTYPE.; 
   DEFINE LABEL / DISPLAY WIDTH=&_LINESIZE; 
   LABEL NAME="Name" LABEL="Label" TYPE="Type" LENGTH="Length" INFORMAT="Informat" FORMAT="Format"; 
   BY memname NOTSORTED;  
   COLUMN name varnum type format label length;  
 QUIT;  

PROC SQL;
	DROP TABLE WORK.SCTABLE;
	DROP VIEW WORK.SCVIEW;
QUIT;

PROC CATALOG CATALOG=WORK.FORMATS;
   DELETE _EG_VARTYPE / ENTRYTYPE=FORMAT;
RUN;
OPTIONS BYLINE;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
