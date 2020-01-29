/* -------------------- FILE: PROC_IBD.C ----------------------- */


#include <stdio.h>

#include "defns.h"
#include "structs.h"

char                  *proc_ibd();
char                  *rbsearch();
char                  *rbsearch2();
char                  *get_pl_line();
char                  *get_tmp_line();

extern FILE           *logfile;

extern enum t_boolean   seq_nos;
extern char           master[2];

enum type    { dirv_data, PL_data };

char  *get_id();


/*
   Author: Ramesh Krishna  (Met Office - Central Computing).

   Last Modified: 15 Jun 1993.

   While writing out the output file process the *I, *B and *D directives.
   This involves changing the input file. The deck being processed is
   curr_deck. directive_input is made to point to the file with the IBD
   data.
*/

char *proc_ibd( directive, info, curr_deck, curr_line, id_name,
                curr_edit_type, id2 )
directive_def  *directive;  /* set of directives from the directives file */
info_def       *info;       /* List of lines and files to be editted */
FILE           *curr_deck;  /* File pointer. DECK currently being processed */
char           *curr_line;  /* current text line */
char           *id_name;    /* identity name */
char           **curr_edit_type;  /* I, B or D */
char           **id2;              /* 2nd id in a DELETE stmt */
{
	char  line_id[maxlen];
	int   i;
	char  buffer[max_name_len];


	sscanf( &curr_line[PL_id_name_start], "%s", line_id );
	*id2 = NULL;
	*curr_edit_type = NULL;


#ifdef DEBUG
printf( "\nIn proc_ibd: PL_id_name_start = %d", PL_id_name_start ); fflush(stdout);
printf( "\nIn proc_ibd. Line: >%s<", curr_line ); fflush(stdout);
printf( "\nIn proc_ibd. id: >%s<", line_id ); fflush(stdout);
printf( "\nedit_list[0] = >%s<", info->edit_list[0] ); fflush(stdout);
#endif


	if ( rbsearch( info->edit_list, info->no_of_edits, line_id ) != NULL ) {

#ifdef DEBUG
		printf( "\nLine: %s\nwill be editted", curr_line );
#endif

		for ( i = 0; i <= directive->num_of_IBDs; i++ )  {

			if ( strcmp( line_id, directive->IBD[i].line[0] ) == 0 )  {

				*id2 = directive->IBD[i].line[1];

#ifdef DEBUG
				printf( "\nOPen file: %s", directive->IBD[i].file );
#endif

				*curr_edit_type = directive->IBD[i].type;

				strcpy( id_name, directive->IBD[i].ident );

				directive->IBD[i].executed = true;

#ifdef DEBUG
	fprintf( stderr, "\nLeaving proc_ibd. id2, curr_edit_type, id_name = >%s<, >%s<, >%s< ",
	                       id2, curr_edit_type );
#endif
				return( directive->IBD[i].file );
			}
		}
	}

#ifdef DEBUG
	fprintf( stderr, "\nLeaving proc_ibd. no edits\n ",
	                       id2, curr_edit_type );
#endif

	return(NULL);
}






int do_ibd( fname, edit_type, infile, outfile, id, curr_line,
            call_level, pl_name, directive, info, id_no, id_name )

char          *fname;         /* temporary file with insert text */
char          *edit_type;     /* I, B, D */
FILE          *infile;        /* Current input file */
FILE          *outfile;       /* Write output to this file */
char          *id;            /* value of 2nd line in a DELETE stmt */
char          *curr_line;     /* current line in the input file */
int           call_level;     /* Because we may call do_call */
char          *pl_name;       /* name of PL */
directive_def *directive;     /* directives structure */
info_def      *info;          /* sorted structure of directives */
int           *id_no;         /* current id_no */
char          *id_name;       /* identity name */
{
	char          buffer[maxlen];
	char          type[maxlen];
	char          curr_type[maxlen];
	enum t_boolean  insert_text;
	enum t_boolean  err=false; /* The =false bit added 16-10-95 in response 
to request by Paul Burton and Henry Ellingworth (Fujitsu) - M.C. */
	FILE          *input;
	int           retcode;

#ifdef DEBUG
	printf( "\nIN process_ibd. edit_type = \"%s\"", edit_type );
	printf( "\nIn process_ibd. id = >%s<", id );
#endif

	/* open file with insert text */


	if ( strcmp( edit_type, "DELETE" ) == 0 )  {

		if ( id == NULL )  {
			err = false;
		}
		else  {

			while ( get_pl_line( buffer, maxlen-1, infile ) != NULL )  {
#ifdef DEBUG
	fprintf( stderr, "\nIn proc_ibd. Deleting line:\n%s", buffer );
	fprintf( stderr, "\nid, get_id(buffer) = >%s<, >%s<", id, get_id(buffer) );
#endif
				if ( strcmp( id, get_id(buffer) ) == 0 )  {
					err = false;
					break;
				}
			}
		}
	}
		
	if ( err == true ) {
		printf( "\nError: 2nd line in DELETE stmt missing\n" );
		puts( curr_line );
		return( none );
	}


	nupopen( input, fname, "r", system_error );

	while ( get_tmp_line( buffer, maxlen, input ) != NULL )  {
		retcode = get_line_type( buffer, type, curr_type, master );
		if ( retcode != ignore )  {
			if ( strcmp( curr_type, "CALL" ) == 0  && retcode == PL_directve ) {
				if ( do_call( buffer, call_level, NULL, outfile,
				              pl_name, directive, info, id_no ) != 0 )
					return(user_error);
			}
			else  {
				(*id_no)++;
				write_pl_line( buffer, id_name, ".\0", *id_no, outfile,
				              true );
			}
		}
	}
					
	fclose( input );


	return(0);
}
