/* ---------------------- FILE: STRUCTS.INC --------------------- */






/*
   Author: Ramesh Krishna  (Met Office - Central Computing).

	Last Modified: 19 Apr 1993.

	This file contains the structures that define the Program Library
	and the Directves file
*/



/*
	Define the basic sructure of the three parts of a PL line.
	i.e. The text, id name, id separator (a dot) and the id number.
*/

#define  PL_text_start           0     /* actually position 1  */
#define  PL_text_length         72

#define  PL_id_name_start       72     /* actually position 73  */
#define  PL_id_name_length      12

#define  PL_id_separator_start  84     /* actually position 85  */
#define  PL_id_separator_length  1
#define  PL_id_separator        ".\0"

#define  PL_id_number_start     85     /* actually position 86  */
#define  PL_id_number_length     8

#define  Master_char           '*'
#define  Master_char2          252


/* *** Define a complete PL line *** */
/*
	Warning: some routines use text as start of the structure.  Change
	those routines if text is not start of the structure.
	Routines: write_pl_line.
*/

typedef struct  {

	char  text[ PL_text_length ];
	char  id_name[ PL_id_name_length ];
	char  separator[ PL_id_separator_length ];
	char  id_number[ PL_id_number_length ];
	char  line_end[2];
	char  padding[maxlen];

}  PL_def;

#define total_pl_line_length  PL_text_length + PL_id_name_length     \
	+ PL_id_separator_length + PL_id_number_length + 2

#define max_ident_len  PL_id_name_length + PL_id_separator_length + \
        + PL_id_number_length




/* *** Define directive commands *** */

/* First the  *I, *B and *D  directives */

typedef struct  {

	char  *line[ 2 ];         /* 2 names are reqd for *D */
	char  *type;              /* name of the directive (I, B, D etc) */
	char  *file;              /* file that text is stored in */
	char  *PL_file;           /* the PL file that the line references */
	char  *deck_type;         /* (D)ECK or (C)OMDECK */
	char  *declare;           /* save name in DECLARE stmt */
	char  *ident;             /* save name in IDENT stmt */
	enum t_boolean executed;    /* set this if the directive is executed */

}  IBD_def;  /* Insert, Before, Delete */


typedef struct  {

	IBD_def  IBD[ max_directives ];
	char     *deck[max_decks];
	char     *comdeck[max_comdecks];
	char     *define[ max_directives ];
	char     *compile[ max_directives ];
	char     *call[ max_directives ];
	int      num_of_IBDs;
	int      num_of_decks;
	int      num_of_comdecks;
	int      num_of_defines;
	int      num_of_compiles;
	int      num_of_calls;
	char     ident[ max_name_len ];   /* for the current ID */
	char     master[ 3 ];             /* for the current master char */
	int      current_edit_line_num;

}  directive_def;





typedef struct  {

	char  *name;                       /* PL name in *(COM)DECK statement */
	char  *dependent[max_calls];       /* Name of (COM)DECK calling deck_name */
	char  dependent_type[max_calls][2];  /* type of dependents */
	char  type[2];                     /* Type (COMDECK or DECK).  */
	int   no_of_dependents;            /* Number or dependents  */

}	cdeck_def;




typedef struct  {

	char *edit_list[max_edits];
	int  no_of_edits;
	char *file_list[max_edits];
	int  no_of_files;

}   info_def;




typedef struct  {

	char  *directives_file;         /*  Directives file      */
	char  *PL;                      /*  Program Library      */
	char  *compile_file;            /*  -c option            */
	char  *source_file;             /*  -s option            */

	char  *verbose;                 /*  -m option  */

	char  *option[max_options];     /*  -o options           */
	int   num_of_options;

	char  mode[2];                  /*  full, quick, normal  */

	char  *suffix;
	int   split_output;             /* split output into decks
	                                   1 : do split
	                                   0 : don't split
	                                   Added 9/2/96 Paul Burton */

}  argument_def;
