/* ------------------------ FILE: UTILS.C ----------------------- */



#include "defns.h"
#include "structs.h"

#include <time.h>

#ifdef MSDOS
#  include <process.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>


extern int  tmpfile_num;
extern char tmpdir[12];   /* Added 14-11-95  Marc Chippindale */


extern FILE  *logfile;

/*
   Author: Ramesh Krishna  (Met Office - Central Computing).

   Last Modified: 16 Dec 1992.

   Various routines which are useful for different tasks.
*/




/*
   The following routine gets various system variables that are defined
   by the IBM Dialog Manager on MVS machnies.  Useful for programs ported
   to UNIX computers.
*/



int get_system_variable( name, value )
char *name, *value;
{
   char *mygetdate();


   if ( strncmp( name, "ZUSER", 5 ) == 0    ||
        strncmp( name, "ZPREFIX", 7 ) == 0 )  {

#ifdef HP
      strcpy( value, getenv( "USER" ) );
#endif
#ifdef CRAY
      strcpy( value, getenv( "LOGNAME" ) );
#endif

      return(0);
   }

   if ( strncmp( name, "ZTEMPF", 6 ) == 0 )  {
      sprintf( value, "_F%u.tmp", getpid() );
      return(0);
   }

   if ( strncmp( name, "ZAPPLID", 7 ) == 0 )  {
      sprintf( value, "%u", getpid() );
      return(0);
   }

   if ( strncmp( name, "ZTIME", 5 ) == 0 )  {
      mygetdate( "time", value );
      return(0);
   }

   if ( strncmp( name, "ZJDATE", 6 ) == 0 )  {
      mygetdate( "date", value );
      return(0);
   }

   if ( strncmp( name, "ZDAY", 4 ) == 0 )  {
      mygetdate( "day", value );
      return(0);
   }

   if ( strncmp( name, "ZYEAR", 5 ) ==0 )  {
      mygetdate( "year", value );
      return(0);
   }

   printf( "\nUser error. name = >%s<\n", name );
   return( user_error );
}



/*
   The following routine is used by the above routine (get_system_variable)
   to get information about the current date.
*/


char *mygetdate( type, output )
char *type, *output;
{

   typedef struct {
      char day_name[4];
      char month[4];
      char day[3];
      char time[9];
      char year[4];
      char delimiter[1];
   } tt;

   char temp[10];
   char *ptr;


   time_t tme;
/* Changed by Anette Van der Wal 1/2/00 to get rid of warning message */
   static tt t;
   int pos;


   pos = 0;

   tme = time( NULL );

   strcpy( (char *)&t, ctime(&tme) );
/*   ptr = &t; */

   if ( strncmp( type, "month", 5 ) == 0 )  {
      strncpy( temp, t.month, 3 );
      pos = 2;

      if ( strncmp( temp, "Jan", 3 ) == 0 )  strcpy( output, " 1" );
      else if ( strncmp( temp, "Feb", 3 ) == 0 )  strcpy( output, " 2" );
      else if ( strncmp( temp, "Mar", 3 ) == 0 )  strcpy( output, " 3" );
      else if ( strncmp( temp, "Apr", 3 ) == 0 )  strcpy( output, " 4" );
      else if ( strncmp( temp, "May", 3 ) == 0 )  strcpy( output, " 5" );
      else if ( strncmp( temp, "Jun", 3 ) == 0 )  strcpy( output, " 6" );
      else if ( strncmp( temp, "Jul", 3 ) == 0 )  strcpy( output, " 7" );
      else if ( strncmp( temp, "Aug", 3 ) == 0 )  strcpy( output, " 8" );
      else if ( strncmp( temp, "Sep", 3 ) == 0 )  strcpy( output, " 9" );
      else if ( strncmp( temp, "Oct", 3 ) == 0 )  strcpy( output, "10" );
      else if ( strncmp( temp, "Nov", 3 ) == 0 )  strcpy( output, "11" );
      else if ( strncmp( temp, "Dec", 3 ) == 0 )  strcpy( output, "12" );
      else  {
         printf( "\nSystem error in routine testime\n" );
         exit(1);
      }
   }

   if ( strncmp( type, "date", 4 ) == 0 )  {
      strncpy( output, t.month, 6 );
      pos = 6;
   }

   if ( strncmp( type, "day", 3 ) == 0 )  {
      strncpy( output, t.day, 2 );
      pos = 2;
   }

   if ( strncmp( type, "day_name", 8 ) == 0 )  {
      strncpy( output, t.day_name, 3 );
      pos = 3;
   }

   if ( strncmp( type, "time", 4 ) == 0 )  {
      strncpy( output, t.time, 9 );
      pos = 5;
   }

   if ( strncmp( type, "year", 4 ) == 0 )  {
      strncpy( output, t.year, 4 );
      pos = 4;
   }

   if ( pos == 0 )  {
      printf( "\nSystem error in mygetdate\n" );
      exit( system_error );
   }

   strcpy( &output[pos], "\0" );

   return( (char *) &t );
}







/* Substitute string old for string new in character string string */
/* Two routines:  subst calls subst_2                              */

int subst( string, old, new )
char *string, *old, *new;
{
   while ( subst_2( string, old, new ) != 0 );   /* keep looping */
   return(0);
}


int subst_2( string, old, new )
char *string, *old, *new;
{
   char *Sb, *Sc;
   long int  Lb;
   char *temp;
   char *buffer;
   int length;


   if ( strlen( old ) == 0 )  {
      puts( "\nError zero length string in subst.\nAborting\n" );
      exit(1);
   }

   if ( ( Sb = strstr( string, old ) ) == NULL )  return(0);

   length = strlen( string );

/*
   if ( ( temp = ( char * ) malloc( length + 1 ) ) == NULL )  {
      puts( "\nUnable to allocate temporary space in subst.\nAborting\n" );
      exit( 1 );
   }
*/

	Malloc( temp, length+1, system_error );

   Lb = strlen( old );
   Sc = Sb + Lb;

   strcpy( Sb, "\0" );

   strcpy( temp, string );
   strcat( temp, new );
   strcat( temp, Sc );

   strcpy( string, temp );

   free( (void *) temp );

   return(1);
}







/*
   Author: Ramesh Krishna  (Met Office - Central Computing).

	Last Modified: 26 Dec 1992.

	Following program sorts an array using the qsort routine from the
	standard C library.

	Values required are:
	array_name
	number_of_elements
	size_of_elements

	The program uses the sort_function (defined below).
*/

rksort( array_name, number_of_elements, size_of_elements )
char *array_name;
int number_of_elements;
int size_of_elements;
{
	int sort_function();
	qsort( array_name, number_of_elements, size_of_elements, sort_function );
	return(0);
}

int sort_function( a, b )
char *a, *b;
{
	return( strcmp( a, b ) );
}




/*
   Author: Ramesh Krishna  (Met Office - Central Computing).

	Last Modified: 26 Dec 1992.

	Following program searches an array using the bsearch routine from the
	standard C library.

	Values required are:
	search_string
	array_name
	number_of_elements
	size_of_elements

	The program uses the sort_function (defined above).
*/

void *rksearch( search_string, array_name, number_of_elements,
					size_of_elements )
char *search_string;
char *array_name;
int number_of_elements;
int size_of_elements;
{
	int sort_function();
	void *retcode;

	retcode = bsearch( search_string, array_name, number_of_elements,
			 size_of_elements, sort_function );

	return( retcode );
}





int remove_trailing_spaces( string )
char *string;
{
	int i;
	int length;

	length = strlen( string );
	i = length - 1;

	while ( string[i] == ' '  &&  i > 0 )  i--;

	if ( i < length - 1 )  string[i+1] = 0;

	return(0);
}







/*
   Author: Ramesh Krishna  (Met Office - Central Computing).

	Last Modified: 27 Dec 1992.

	The following routine reads the first entry in argument_list
	into argument and returns the start of the next entry.
	If end of list is reached NULL is returned.
	Entries are separated by spaces.
*/


char *getarg( argument_list, argument )
char *argument_list, *argument;
{
	char *arg_list_ptr;

	arg_list_ptr = argument_list;

	if ( sscanf( arg_list_ptr, "%s", argument ) > 0 )  {
		arg_list_ptr = strstr( arg_list_ptr, argument );

		while ( strncmp( arg_list_ptr, " ", 1 ) != 0  &&
				  (int) strlen( arg_list_ptr ) > 0 )
			arg_list_ptr++;

		return( arg_list_ptr );
	}

	return( NULL );
}



/*
   Author: Ramesh Krishna  (Met Office - Central Computing).

   Last Modified: 27 Dec 1992.

	Check that directives are recognised.  If recognised make sure
	they are in the program's standard form.
	Return codes are self explanatory.
*/


int get_line_type( instring, type, curr_type, master )
char *instring, *type, *curr_type, *master;
{
	char temp[ maxlen ];
	enum t_boolean  error_status;

	if ( *instring !=  *master )  return( source_code );
	if ( sscanf( instring, "%s", temp ) == 0 )  return( source_code );

/* Make allowances for various comment types */

   if ( strncmp( "*/", instring, 2 ) == 0 )  return(ignore);
   if ( strncmp( "*~", instring, 2 ) == 0 )  return(ignore);
   if ( strncmp( "*-", instring, 2 ) == 0 )  return(ignore);
   if ( strncmp( "*CLL", instring, 4 ) == 0 )  return(ignore);
   if ( strncmp( "*CL", instring, 3 ) == 0 )  return(ignore);
/*  Ignore *VOCL - this is a Fujitsu directive on VPP700  - Marc Chippindale 13-2-97 */
   if ( strncmp( "*VOCL", instring, 5 ) == 0 )  return(source_code);
	if ( strncmp( "* ", instring, 2 ) == 0 )  return(ignore);

/*  Don't change current directive type for the following */

	strcpy( curr_type, &temp[1] );

	if ( strcmp( curr_type, "CALL"    ) == 0 )  return( PL_directve );
	if ( strcmp( curr_type, "IF"      ) == 0 )  return( PL_directve );
	if ( strcmp( curr_type, "ELSEIF"  ) == 0 )  return( PL_directve );
	if ( strcmp( curr_type, "ELSE"    ) == 0 )  return( PL_directve );
	if ( strcmp( curr_type, "ENDIF"   ) == 0 )  return( PL_directve );

	if ( strcmp( curr_type, "MASTER" ) == 0 ) {
		if ( sscanf( instring, "%*s%s", master ) != 1 )  {
			prt_error( "\nError. missing argument: \0", instring, user_error );
		}
		else
			return(ignore);
	}


	strcpy( type, &temp[1] );

	if ( strcmp( type, "DECK"    ) == 0 )  return( directve );
	if ( strcmp( type, "COMDECK" ) == 0 )  return( directve );
	if ( strcmp( type, "DEFINE"  ) == 0 )  return( directve );
	if ( strcmp( type, "DELETE"  ) == 0 )  return( directve );
	if ( strcmp( type, "INSERT"  ) == 0 )  return( directve );
	if ( strcmp( type, "BEFORE"  ) == 0 )  return( directve );
	if ( strcmp( type, "COMPILE" ) == 0 )  return( directve );
	if ( strcmp( type, "IDENT"   ) == 0 )  return( directve );
	if ( strcmp( type, "DECLARE" ) == 0 )  return( directve );

	if ( strcmp( type, "C" ) == 0 )  {
		strcpy( type, "COMPILE" );
		return( directve );
	}

	if ( strcmp( type, "I" ) == 0 )  {
		strcpy( type, "INSERT" );
		return( directve );
	}

	if ( strcmp( type, "D" ) == 0 )  {
		strcpy( type, "DELETE" );
		return( directve );
	}

	if ( strcmp( type, "B" ) == 0 )  {
		strcpy( type, "BEFORE" );
		return( directve );
	}

	if ( strcmp( type, "DEF" ) == 0  ||  strcmp( type, "DEFS" ) == 0 )  {
		strcpy( type, "DEFINE" );
		return( directve );
	}

	if ( strcmp( type, "DK" ) == 0 )  {
		strcpy( type, "DECK" );
		return( directve );
	}

	if ( strcmp( type, "CDK" ) == 0 )  {
		strcpy( type, "COMDECK" );
		return( directve );
	}

	if ( strcmp( type, "ID" ) == 0 )  {
		strcpy( type, "IDENT" );
		return( directve );
	}

	if ( strcmp( type, "DC" ) == 0 )  {
		strcpy( type, "DECLARE" );
		return( directve );
	}

/*
	Now deal with directives to be ignored.
*/
/*
	if ( strcmp( type, "DECLARE" ) == 0 )  {
		printf( "\nIgnoring: %s", instring );
		return( ignore );
	}
*/

	if ( strcmp( type, "MOVEDK" ) == 0 )  {
		printf( "\nIgnoring: %s", instring );
		return( ignore );
	}


/*	if ( *type == '/' )  return( ignore );   A comment */
/*	if ( strcmp( type, "/" ) == 0 )  return( ignore ); *



/*
	We reach here if type is an unrecognised directive.
*/
/*
	fprintf( logfile, "\n*** Warning: Unrecognised directive" );
	fprintf( logfile, "\n>>> %s\n", instring );

	printf( "\n*** Warning: Unrecognised directive" );
	printf( "\n>>> %s\n", instring );
*/

	prt_error( "Unrecognised directive\0", instring, user_error );

}






/*
   Author: Ramesh Krishna  (Met Office - Central Computing).

   Last Modified: 31 Dec 1992.

	Write string 'instring' to 'outstring' and right justify text in
	'outstring'.  'length' is the length of outstring.
*/

right_justify( outstring, instring, outlength )
char *outstring, *instring;
int outlength;
{
	int inlength;
	int length_diff;
	int i;

	inlength = strlen( instring );
	length_diff = outlength - inlength;


	if ( length_diff < 0 )
		prt_error( "System error in routine right_justify.  diff < 0",
		           instring, system_error );

	for ( i = 0; i < outlength; i++ )
		strncpy( &outstring[i], " ", 1 );

	strncpy( &outstring[ length_diff ], instring, inlength );

	return(0);
}






/*
   Author: Ramesh Krishna  (Met Office - Central Computing).

   Last Modified: 14 Jun 1992.


	Following routine writes a line to a file using the standard PL
	format given by structure PL_def (defined in file structs.inc).

	Input:
	pl_text0        (text making up the source code).
	pl_id_name      (name defined by directive IDENT)
	pl_id_separator (a dot at present)
	pl_id_number    (PL_id_number)
	fp              (output file handle)
	seq_nos         (whether seq numbers should be includede)

   If pl_id_name = NULL  then prog wont insert an id (which may or may not be
   contained in the input line.
*/

int write_pl_line( pl_text, pl_id_name, pl_id_separator, pl_id_number, fp, 
               seq_nos )
char  *pl_text; 
char  *pl_id_name; 
char  *pl_id_separator;
int   pl_id_number;
FILE  *fp;
enum  t_boolean seq_nos;
{
	PL_def  pl;
	int i;
	int total_length;
	int length;
	char temp[maxlen2];
	char *ptr;


#ifdef DEBUG
printf( "\nIn write_pl_line: pl_id_number = %d", pl_id_number );
printf( "\npl_text = >%s<", pl_text );
#endif


	if ( *pl_text == dummy_char || *pl_text == ignore_char )
		return(0);


	subst( pl_text, "\n", "\0" );


	ptr = pl.text;

   total_length = sizeof( pl );

	for ( i = 0; i < total_length; i++ )
		strncpy( ptr++, " ", 1 );


/* copy from PL directly (don't insert ids), or ignore ids */

	if ( pl_id_name == NULL  ||  seq_nos == false )  {

		strcpy( pl.text, pl_text );

		if ( seq_nos == false )  strcpy( pl.id_name, "\0" );


#ifdef NO_TRAILING_SPACES
		remove_trailing_spaces( pl.text );
#endif

		fprintf( fp, "%s\n", pl.text );


#ifdef DEBUG
		fprintf( stderr, "\nWWriting:\n%s", pl.text );
		fprintf( stderr, "\nxx" );
#endif

		return(0);
	}


	strcpy( temp, pl_text );
	remove_trailing_spaces( temp );
	length = strlen( temp );

	if ( length > PL_text_length ) {
		temp[PL_text_length] = '\0';    /* truncate long lines */
	}

	strncpy( pl.text, pl_text, length );


	length = strlen( pl_id_name );
	if ( length > PL_id_name_length )  {
		prt_error( "\nIDent too long: ", pl_text, user_error );
	}

	right_justify( pl.id_name, pl_id_name, PL_id_name_length );

	length = strlen( pl_id_separator );

	if ( length > PL_id_separator_length )  {
		prt_error( "ID separator too long, system error:",
		           pl_id_separator, system_error );
	}

	strncpy( pl.separator, pl_id_separator, length );


	sprintf( temp, "%d", pl_id_number );
	length = strlen( temp );

	if ( length > PL_id_number_length )  {
		prt_error( "\nID number too large, system error:",
		           temp, user_error );
	}

	strncpy( pl.id_number, temp, length );


	strcpy( pl.line_end, "\0" );

	ptr = pl.text;

/*
	printf( "\nlen(ptr) = %d", strlen(ptr) );
	printf( "\nptr = %s", ptr );
*/

	remove_trailing_spaces( ptr );

#ifdef DEBUG 
	fprintf( stderr, "\nwrite_pl_line: Writing line:\n%s", ptr );
#endif

	fprintf( fp, "%s\n", ptr );


	return(0);
}








/*
   Author: Ramesh Krishna  (Met Office - Central Computing).

   Last Modified: 20 Mar 1993.

   Input:
   pl_line  : Program library format line.
   id_start : Start of the pl id in the pl line.

   Output:
   id       : The extracted value of the line identity.
*/


int get_ident( pl_line, id_start, id )
char *pl_line; int id_start; char *id;
{
   void *ptr;

   strcpy( id, &pl_line[ id_start ] );

   return(0);
}






char *get_tempfile( prefix )
char *prefix;
{
   static char filename[max_name_len];
	char *ptr;
	int  proc_id;

/*
	Malloc( filename, strlen( template ) + 1, system_error );
*/

	if ( (int) strlen(prefix) > max_name_len - 5 )  {
		printf( "\ntemp file name too long for %s\n", prefix );
		return(NULL);
	}

	proc_id = getpid();

	sprintf( filename, "%s%d.tmp", prefix, proc_id );

   return( filename );
}








/*
   Author: Ramesh Krishna  (Met Office - Central Computing).

   Last Modified: 20 Mar 1993.

   This routine appends "string" to the end of file "filename"
   in "directory".

   Input:
   directory : directory containing the output file.
   filename  : output file name.
   string    : data to write to output file.
*/


int write_file( directory, filename, string )
char *directory, *filename, *string;
{
   char outfile[maxlen];
   FILE *fp;

#ifdef MSDOS
   sprintf( outfile, "%s\\%s", directory, filename );
#else
   sprintf( outfile, "%s/%s", directory, filename );
#endif


   nupopen( fp, outfile, "a", system_error );

   fputs( string, fp );

   fclose( fp );


#ifdef DEBUG
   printf( "\noutfile, string = %s, %s", outfile, string );
#endif


   return(0);
}









/* 
   Author: Ramesh Krishna  (Met Office - Central Computing).

   Last Modified: 26 Apr 1993.

	Binary search for string t in ordered string array s.
	n = number of elements in array s.

	Return pointer to string if found else return NULL.
	Array must have been a set of pointers initially, which were used
	in malloc statements to allocate storage for the data (eg *s[])
*/

char *rbsearch( s, n, t )
char *s[], *t;  int n;
{
	int a, b;
	int m;
	int oldm;
	int c;
	int i;


#ifdef DEBUG
	puts( "\n" );
/*
	for ( i = 0; i <= n-1; i++ )  {
		printf( "\n2.i, s[i] = %d %u", i, s[i] );
	}
*/

#endif

	a = 0;  b = n; m = -1;

	if ( b < a ) {
/*
		printf( "\nInternal error in routine rbsearch. Aborting" );
		exit( system_error );
*/
		return(NULL);
	}


	while (1)  {

		oldm = m;
		m = ( a + b ) / 2;
		c = (int) strcmp( t, s[m] );

		if ( c == 0 )  return( s[m] );
		if ( a == b )  return(NULL);
		if ( c < 0 )   b = ( a + b ) / 2;
		if ( c > 0 )   a = ( a + b ) / 2;

		if ( m == oldm )  {   /* do this because integers are rounded down */
			if ( m == n )  return(NULL);
			m++;
			c = (int) strcmp( t, s[m] );
			if ( c == 0 )  return( s[m] );
			return(NULL);
		}

	}

	return(NULL);
}








/* 
   Author: Ramesh Krishna  (Met Office - Central Computing).

   Last Modified: 26 Apr 1993.

	Binary search for string t in ordered string array s.
	n = number of elements in array s.

	Return pointer to string if found else return NULL.
	Array must have been a set of pointers initially, which were used
	in malloc statements to allocate storage for the data (eg *s[])

	This routine is similar to rbsearch but the comparison uses the 
	elements in the shorter string. Excess elements in the longer 
	string are ignored (after a trailing blank).
*/

char *rbsearch2( s, n, t )
char *s[], *t;  int n;
{
	int a, b;
	int m;
	int oldm;
	int c;
	int i;
	char temp[maxlen];


#ifdef DEBUGIF
puts("\nIn rbsearch2" );
if ( strcmp( t, "RECON" ) == 0 )  {
	printf("\nn = %d", n );
	printf( "\nt = >%s<", t );
	for ( i = 0; i <= n-1; i++ )  {
		printf( "\ni, s[i] = %d >%s<", i, s[i] );
	}
}
#endif


	a = 0;  b = n; m = -1;

	if ( b < a ) {
/*
		printf( "\nInternal error in routine rbsearch2. Aborting" );
		exit( system_error );
*/
		return(NULL);
	}


/*
   Special treatment if any character in the search string is a @. In this
   case search the arrays sequentially.
*/

	if ( strstr( t, "@" ) != NULL )  {

		for ( i = a; i <= b; i ++ )  {
			sscanf( s[i], "%s", temp );
			if ( strcmp( t, temp ) == 0 )  return( s[i] );
		}

		return(NULL);
	}


	while (1)  {

		oldm = m;
		m = ( a + b ) / 2;

		sscanf( s[m], "%s", temp );
		c = (int) strcmp( t, temp );

		if ( c == 0 )  { return( s[m] ); }
		if ( a == b )  { return(NULL); }
		if ( c < 0 )   b = ( a + b ) / 2;
		if ( c > 0 )   a = ( a + b ) / 2;

		if ( m == oldm )  {   /* do this because integers are rounded down */
			if ( m == n )  return(NULL);
			m++;
			sscanf( s[m], "%s", temp );
			c = (int) strcmp( t, temp );
			if ( c == 0 )  { return( s[m] ); }
			return(NULL);
		}

	}

	return(NULL);
}










/* 
   Author: Ramesh Krishna  (Met Office - Central Computing).

   Last Modified: 26 Apr 1993.

	Sort an array of string elements.  In this routine the elements are a 
	set of pointers to the data (possibly allocated using the malloc
	statement).

	text = elements to be sorted.
	max_elements = number_of_elements

*/


int rbsort( text, max_elements )
char *text[]; int max_elements;
{
	int i, j;
	int position;
	char *smallest;
	int num;
	char *temp;

	num = max_elements;


	for ( i = 0; i <= num - 1; i++ )  {
		position = i;
		smallest = text[i];

		for ( j = i + 1; j <= num; j++ )  {
			if ( strcmp( text[j], smallest ) < 0 )  {
				smallest = text[j];
				position = j;
			}
		}

		temp = text[i];
		text[i] = text[position];
		text[position] = temp;

	}

	return(0);
}	










/* 
   Author: Ramesh Krishna  (Met Office - Central Computing).

   Last Modified: 28 Apr 1993.

	For a sorted list of names remove repetitions.

	list           = array of sorted strings.
	num_of_members = number of elements.

	returns number of members after repetitions are removed.

*/

int remove_repetitions( list, num_of_members )
char *list[];
int  num_of_members;
{
	enum t_boolean  status_flag;
	int  no_of_members;
	int i, j;

   status_flag = true;
   no_of_members = num_of_members;

   for ( i = 0; i < no_of_members; i++ ) {    /* remove repetitions */
      if ( strcmp( list[i], list[i+1] ) == 0 )  {
         for ( j = i+1; j < no_of_members; j++ )  list[j] = list[j+1];
         i--;
         no_of_members--;
      }
   }

	return(no_of_members);
}








/* 
   Author: Ramesh Krishna  (Met Office - Central Computing).

   Last Modified: 29 Apr 1993.

	given a list of items (in a single row separated by spaces or commas ) 
	extract the items and put into a string array


	list  = single line of items (separated by spaces or commas)
	array = array of string pointers (malloc is used to allocate space)
	max_array_members = max allocated pointers (for error checking).
	                    (should be one greater than needed).

	function returns the number of items extracted.
*/

int process_list( list, array, max_array_members )
char *list, *array[];
int  max_array_members;
{
	int   no_of_members;
	char  *newlist;
	char  *ptr;
	int 	i;
	char  temp[maxlen];

	Malloc_and_copy( newlist, list );

	subst( newlist, ",\0", " \0" );
	ptr = newlist;

	no_of_members = -1;

	while (1)  {

		if ( ++no_of_members > max_array_members )  {
			printf( "\n*** Internal error in routine process_list" );
			printf( "\nno_of_members = %d, max_array_members = %d\n",
			        no_of_members, max_array_members );
			return( system_error );
		}

		if ( ( ptr = getarg( ptr, temp ) ) == NULL )  {
			no_of_members--;
			break;
		}
		Malloc_and_copy( array[ no_of_members ], temp );
	}


	free(newlist);

	return( no_of_members );
}





/* 
   Author: Ramesh Krishna  (Met Office - Central Computing).

   Last Modified: 27 jun 1993.


	Read a single line from the pl_file.  If a * is the first character
   change it to char 252 (i.e. change the master char).

*/


void *get_pl_line( buffer, max_len, pl_file )
char *buffer;
int  max_len;
FILE *pl_file;
{

	if ( fgets( buffer, max_len, pl_file )  ==  NULL )  return( NULL );

	return( buffer );
}







/*
   Author: Ramesh Krishna  (Met Office - Central Computing).

   Last Modified: 28 Jun 1993.


	Following routine writes a line to a file using the standard PL
	format given by structure PL_def (defined in file structs.h).

	Input:
	pl_text         (text making up the source code).
	pl_id_name      (name defined by directive IDENT)
	pl_id_separator (a dot at present)
	pl_id_number    (PL_id_number)
	fp              (output file handle)
	seq_nos         (whether seq numbers should be includede)
	master          (current_master character, in input)
*/

int write_tmp_line( pl_text, pl_id_name, pl_id_separator, pl_id_number, fp, 
               seq_nos, master )
char  *pl_text; 
char  *pl_id_name; 
char  *pl_id_separator;
int   pl_id_number;
FILE  *fp;
enum  t_boolean seq_nos;
char  *master;
{
	PL_def  pl;
	int i;
	int total_length;
	int length;
	char temp[maxlen2];
	char temp2[maxlen2];
	char *ptr;


#ifdef DEBUG
fprintf( stderr, "\nIn write_tmp_file: pl_id_number = %d", pl_id_number );
fprintf( stderr, "\nIn write_tmp_file: pl_id_name   = %s", pl_id_name );
fprintf( stderr, "\npl_text = >%s<", pl_text );
#endif


	subst( pl_text, "\n", "\0" );


/* copy from PL directly (don't insert ids), or ignore ids */

	if ( pl_id_name == NULL  ||  seq_nos == false )  {

		strcpy( pl.text, pl_text );

		if ( *pl.text == *master )  *pl.text = Master_char;

		if ( seq_nos == false )  strcpy( pl.id_name, "\0" );

		remove_trailing_spaces( pl.text );
		fprintf( fp, "%s\n", pl.text );

#ifdef DEBUG
		printf( "\nwrite_tmp_line(no_id) Writing:\n%s", pl.text );
#endif

		return(0);
	}


	ptr = pl.text;

   total_length = sizeof( pl );

	for ( i = 0; i < total_length; i++ )
		strncpy( ptr++, " ", 1 );


	strcpy( temp, pl_text );
	remove_trailing_spaces( temp );
	length = strlen( temp );
	if ( length > PL_text_length ) {

		strcpy( temp2, temp );
		temp[PL_text_length] = '\0';

		fprintf( stderr, "\n**Warning truncated line:\n%s\nto\n%s\n",
		         temp2, temp );
		fprintf( stderr, "(found in a modifications file)\n" );
#ifdef SAVE_LOG
		fprintf( logfile, "\n**Warning truncated line:\n%s\nto\n%s\n",
		         temp2, temp );
		fprintf( logfile, "(found in a modifications file)\n" );
#endif
	}

	strncpy( pl.text, pl_text, length );

	if ( *pl.text == *master )  *pl.text = Master_char;


	length = strlen( pl_id_name );

	if ( length > PL_id_name_length )  {

		prt_error( "\nIDentity too long: ", pl_id_name, user_error );
	}

	right_justify( pl.id_name, pl_id_name, PL_id_name_length );

	length = strlen( pl_id_separator );

	if ( length > PL_id_separator_length )  {
		prt_error( "ID separator too long, system error:",
		           pl_id_separator, system_error );
	}

	strncpy( pl.separator, pl_id_separator, length );


	sprintf( temp, "%d", pl_id_number );
	length = strlen( temp );

	if ( length > PL_id_number_length )  {
		prt_error( "ID number too large, system error:",
		           temp, user_error );
	}

	strncpy( pl.id_number, temp, length );


	strcpy( pl.line_end, "\0" );

	ptr = pl.text;

/*
	printf( "\nlen(ptr) = %d", strlen(ptr) );
	printf( "\nptr = %s", ptr );
*/

	remove_trailing_spaces( ptr );

#ifdef DEBUG
	fprintf( stderr, "\nwrite_tmp_line: Writing:\n%s\n", ptr );
#endif

	fprintf( fp, "%s\n", ptr );


	return(0);
}








/* 
   Author: Ramesh Krishna  (Met Office - Central Computing).

   Last Modified: 28 jun 1993.


	Read a single line from the temp file.

*/


void *get_tmp_line( buffer, max_len, tmp_file )
char *buffer;
FILE *tmp_file;
{

	if ( fgets( buffer, max_len, tmp_file )  ==  NULL )  return( NULL );

	return( buffer );
}








/* 
   Author: Ramesh Krishna  (Met Office - Central Computing).

   Last Modified: 05 jul 1993.


	Read a single line from the directives file.

*/


void *get_drv_line( buffer, max_len, tmp_file )
char *buffer;
FILE *tmp_file;
{

	if ( fgets( buffer, max_len, tmp_file )  ==  NULL )  return( NULL );

	return( buffer );
}








/* 
   Author: Ramesh Krishna  (Met Office - Central Computing).

   Last Modified: 05 jul 1993.


	Read a single line from the PL index file.

*/


void *get_idx_line( buffer, max_len, tmp_file )
char *buffer;
FILE *tmp_file;
{

	if ( fgets( buffer, max_len, tmp_file )  ==  NULL )  return( NULL );

	return( buffer );
}









/* 
   Author: Ramesh Krishna  (Met Office - Central Computing).

   Last Modified: 05 jul 1993.


	Read a single line from the PL xref file.

*/


void *get_xref_line( buffer, max_len, tmp_file )
char *buffer;
FILE *tmp_file;
{

	if ( fgets( buffer, max_len, tmp_file )  ==  NULL )  return( NULL );

	return( buffer );
}








/* 
   Author: Ramesh Krishna  (Met Office - Central Computing).

   Last Modified: 05 jul 1993.


	Read a single line from a file.

*/


void *fgetz( buffer, max_len, tmp_file )
char *buffer;
FILE *tmp_file;
{

	if ( fgets( buffer, max_len, tmp_file )  ==  NULL )  return( NULL );

	return( buffer );
}







/* 
   Author: Ramesh Krishna  (Met Office - Central Computing).

   Last Modified: 05 jul 1993.


	Generate a temporary file name in temporary directory defined in tmpdir
	defined in update and top of this file.

*/

char *tmpname()
{
	static  char name[maxlen];

	tmpfile_num++;

	if ( tmpfile_num > maxtempfiles )  {
		printf( "\nToo many temporary files (>500)\n" );
		return(NULL);
	}

	sprintf( name, "%s/%s.%d", tmpdir, tmpfilename, tmpfile_num );

	return( name );

}








/* 
   Author: Ramesh Krishna  (Met Office - Central Computing).

   Last Modified: 09 Jul 1993.

	Decide whether a suffix has to be attached.  If yes then validate
	the -a option (suffix specified).

	filename   =  file name of the compile file or source file.
	suffix     =  suffix to be attached, if any.
*/



process_suffix( filename, suffix )
char *filename, *suffix;
{
	int  length;
	char temp[maxlen];

	if ( suffix != NULL )
		printf( "\nfilename, suffix = %s, %s", filename, suffix );
	else
		printf( "\nfilename, suffix = %s, (null)", filename );


	
	if ( filename == NULL ) {
		printf( "\n**Error. Compile file (output) not specified\n" );
		return(user_error);
	}


	strcpy( temp, filename );

	if ( suffix != NULL )  {
		if ( strlen( suffix )  != 1 )  {
			prt_error( "\n**Error. Invalid -a option ", suffix, user_error );
		}
      sprintf( filename, "%s.%s", temp, suffix );
		return(0);
	}


	length = strlen( filename );

   if ( length > 2 )  {
      if ( strncmp( &filename[length-2], ".f", 2 ) != 0  &&
         strncmp( &filename[length-2], ".c", 2 ) != 0  &&
         strncmp( &filename[length-2], ".s", 2 ) != 0  )
         sprintf( filename, "%s.f", temp );
		}
   else  {
         sprintf( filename, "%s.f", temp );
   }

	return(0);
}






/*
   Author: Ramesh Krishna  (Met. Office - Central Computing).

   Last Modified: 12 Jul 1993.

	Routine delete_dir  deletes an empty directory.
*/




int delete_dir( dirname )
char *dirname;
{
	char  buffer[maxlen2];

	if ( rmdir( dirname ) != 0 )  return( system_error );

	if ( access( dirname, 00 )  == 0 )  {
		printf( "\nfailed to delete directory \"%s\"\n", dirname );
		return( user_error );
	}

	return(0);
}







/*
   Author: Ramesh Krishna  (Met. Office - Central Computing).

   Last Modified: 21 Jul 1993.

	Routine makedir deletes a directory together with all files in it.

	dirname = name of the directory to delete.
*/


makedir( dirname )
char *dirname;
{
	int  rc;

	if ( access( dirname, 06 ) != 0 )  {
		if ( mkdir( dirname, S_IRWXU ) != 0 )  return( user_error );
	}

	return(0);

}





/*
   Author: Ramesh Krishna  (Met. Office - Central Computing).

   Last Modified: 21 Jul 1993.

	Routine creates a file and puts a '_' in it. 

	filename = file which is created.
	data     = Initial contents of the file.
*/


mkfile( filename, data )
char *filename, *data;
{
	FILE *fp;

	nupopen( fp, filename, "w", user_error );

	fputs( data, fp );

	fclose( fp );

	return(0);
}







/*
   Author: Ramesh Krishna  (Met. Office - Central Computing).

   Last Modified: 21 Jul 1993.

	Routine cleanup is called whenever the program terminates normally.
	All temporary files (or directories) are deleted.
   The routine is called by atexit.

	filename = file which is created.
	data     = Initial contents of the file.
*/

atexit_t cleanup(void)
{
	char  buffer[maxlen2];

#ifndef DEBUG
	sprintf( buffer, "rm -r %s 2> /dev/null", tmpdir );
	system( buffer );
#endif

}












int write_master( pl_name )
char *pl_name;
{
   FILE *fp;
   char  master_file[maxlen];

   sprintf( master_file, "%s/master", pl_name );

   nupopen( fp, master_file, "w", system_error );

   fputc( Master_char, fp );

   fclose( fp );

}







int read_master( pl_name )
char *pl_name;
{
   FILE  *fp;
   char  master_file[maxlen];
	int   master_char;

   sprintf( master_file, "%s/master", pl_name );

   nupopen( fp, master_file, "r", system_error );

   master_char = fgetc( fp );

   fclose( fp );

	return( master_char );
}




