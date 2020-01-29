/* ------------------------- FILE: PROC_IF.C ------------------- */





#include "defns.h"
#include "structs.h"


char *rbsearch();
char *rbsearch2();

extern FILE *logfile;

/*
	Format of if line is:
	logical operator logical operator logical ...
*/

/*
   Author: Ramesh Krishna  (Met Office - Central Computing).

	Last Modified: 27 Dec 1992.

	The function  proc_if_def  processes the *IF DEF... statements and
	returns the following values.
	0 = expression is true.
	-1 = expression is false.
	user_error = error in *IF DEF... line.

	The routine requires the following input.
	ifdef = the *IF DEF... line   (*ELSEIF is treated similarly).
	defs  = array of defined names
	num_of_defs = number of members of above array
	def_width = length of members of the above array.


int proc_if_def( ifdef, defs, num_of_defs, def_width )

*/



int proc_if_def( ifdef, defs, num_of_defs )
char *ifdef, *defs[];
int num_of_defs;
{
	int length;
	int i, j;
	int numargs;
	int remove_trailing_spaces();

	char if_def[ maxlen ];
	char *ptr;
	char *getarg();
	char operator[ max_operators ][ max_operator_len ];
	char logical[ max_operators + 1 ][ max_name_len ];
	char substr[maxlen];
	enum t_boolean negate;
	int  loop;

	/* Make a copy of ifdef since it will be altered for manipulation */

	*if_def='h';     /* replace the master char, *, with an h */
	
        strcpy( &if_def[1], &ifdef[1] );

	/*  Take out the line id (after column where line text ends) */

	strcpy( &if_def[ PL_id_name_start ], "\0" );

	remove_trailing_spaces( if_def );


/*
	Reduce the if_def text to a simpler format, e.g.
	*IF DEF,A,OR,B,AND,C
	to
	A OR B AND C
	which is more convenient for analysis.
*/

#ifdef DEBUG
	printf( "\nIn proc_if_def. Cond = \"%s\"", if_def );
#endif

	subst( if_def, "DEF,\0", "\0" );
	subst( if_def, "DEFS,\0", "\0" );
	subst( if_def, ",AND,\0", " AND \0" );
	subst( if_def, ",OR,\0", " OR \0" );
/*
	subst( if_def, "ELSEIF\0", "\0" );
	subst( if_def, "ELSE IF\0", "\0" );
	subst( if_def, "IF\0", "\0" );

     The subst calls below replace the *first* occurrence of the 
     directive (reconised by the preceding 'h' which was changed from
     the master character,*, because it caused subst problems.
            -Marc Chippindale 24/8/94
*/

	subst( if_def, "hIF\0", "\0" );
	subst( if_def, "hELSEIF\0", "\0" );
	subst( if_def, "hELSE IF\0", "\0" );


#ifdef DEBUGIF
	printf( "\nnum_of_defs = %d", num_of_defs );
	printf( "\nIn proc_if_def. Reduced cond = \"%s\"", if_def );
#endif

/*
	ptr is used for moving along if_def and sorting the various
	operators and logicals into arrays.
*/
	ptr = if_def;

/*
	Get the first item in the list, which will be a logical.
	getarg reads the first item in the list (pointed to by ptr)
	and returns a pointer to the next item in the list.
*/
	numargs = 0;
	ptr = getarg( ptr, logical[numargs] );    /* get the first item */

	/*  Get the rest of the items.  */
	while( (int) strlen( ptr ) > 0 )  {

		numargs++;

		if ( numargs >= max_conds )
			prt_error( "Too Many IF DEFS in line:", ifdef, user_error );

		ptr = getarg( ptr, operator[numargs] );  
		if ( ptr == NULL )
			{ prt_error( "Error at line:", ifdef, user_error ); }

		ptr = getarg( ptr, logical[numargs] );
		if ( ptr == NULL )
			{ prt_error( "Error at line:", ifdef, user_error ); }
	}


/*  Syntax checking.  Make sure operators are OK  */


	for ( i = 1; i <= numargs; i++ )  {
		if ( strncmp( operator[i], "OR", 2 ) != 0  &&
			  strncmp( operator[i], "AND", 3 ) != 0 )  {
			printf( "\nSyntax error in line:\n%s", ifdef );
			puts( operator[i] );
			return( user_error );
		}
	}


/*
	For each logical check list in defs. If found then substitute with .t.
	else substitute with .f.
*/

#ifdef DEBUG
for( i = 0; i < num_of_defs; i++ )
	printf( "\ni,defs[i] = %d, >%s<", i, defs[i] );  fflush(stdout);

for( i = 0; i <= numargs; i++ )
	printf( "\ni,logical[i] = %d, >%s<", i, logical[i] );  fflush(stdout);
#endif


	for ( i = 0; i <= numargs; i++ )  {

#ifdef DEBUG
printf("\nQlogical .%s. ", logical[i] );
#endif

		if ( *logical[i] == '-' )  {
			negate = true;
			ptr = &logical[i][1];
		}
		else  {
			negate = false;
			ptr = logical[i];
		}

		if ( rbsearch2( defs, num_of_defs, ptr ) != NULL )
			strcpy( logical[i], ".t." );
		else
			strcpy( logical[i], ".f." );


		if ( negate == true )  {
			if ( strcmp( logical[i], ".t." ) == 0 )  
				strcpy( logical[i], ".f." );
			else
				strcpy( logical[i], ".t." );
		}


#ifdef DEBUG
printf(">%s<", logical[i] );
#endif

	}

/*
	Test each operator for value = AND.
	If yes:  Substitute .t. and .t. for .t. or .t.
	else  Substitute for  .f. or .f.
*/
/* 
   Iterate to allow false conditions to influence over AND statements
*/

	for ( loop = 0; loop <= numargs+1; loop++ )  {
		for ( i = 1; i <= numargs; i++ )  {
			if ( strncmp( operator[i], "AND", 3 ) == 0 )  {
				if ( strncmp( logical[i-1], ".f.", 3 ) == 0  ||
					  strncmp( logical[i], ".f.", 3 ) == 0  )  {
					strcpy( logical[i-1], ".f." );
					strcpy( logical[i], ".f." );
				}
			}
		}
	}



	for ( i = 1; i <= numargs; i++ )  {
		if ( strncmp( operator[i], "AND", 3 ) == 0 )  {
			strcpy( operator[i], "OR" );
			if ( strncmp( logical[i-1], ".f.", 3 ) == 0  ||
				  strncmp( logical[i], ".f.", 3 ) == 0  )  {
				strcpy( logical[i-1], ".f." );
				strcpy( logical[i], ".f." );
			}
		}
	}


/*
	Now all the operators are ORs. If any logical is .t. then the
	whole expression is true. So return 0.
*/

	for ( i = 0; i <= numargs; i++ )  {
#ifdef DEBUG
		printf( "\nAt end. i, log: %d %s", i, logical[i] );
#endif
		if ( strncmp( logical[i], ".t.", 3 ) == 0 )  return( 0 );
	}

/*
	We reach here if no logicals are = .t.
*/
	return( -1 );

}





/*


main()
{
	int i;
	int retcode;
	char ifdef[maxlen];
	char *defs[5];


	Malloc_and_copy( defs[0], "AA\0" );
	Malloc_and_copy( defs[1], "BB\0" ); 
	Malloc_and_copy( defs[2], "AA\0" );
	Malloc_and_copy( defs[3], "DDDD\0" );
	Malloc_and_copy( defs[4], "BBB\0" );

	strcpy( ifdef, "*IF DEF,A,OR,DEF,DDD,OR,DEF,BBB,AND,DEF,DDD" );

	rbsort( defs, 4 );

	for( i = 0; i < 5; i++ )  printf( "\n%d - %s", i, defs[i] );

	retcode = proc_if_def( ifdef, defs, 5 );

	if ( retcode > 0 )  exit( retcode );

	if ( retcode == 0 )
		printf( "\nExpression is true" );
	else
		printf( "\nExpression is false" );


	printf( "\nFinished in main\n" );

	return(0);
}


*/
