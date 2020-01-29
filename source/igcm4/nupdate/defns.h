/* ---------------- FILE: DEFNS.INC --------------------- */




#define RELEASE "0.4.9"

#define noMSDOS   /* Delete for Non-MSDOS systems */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define nodebug
#define notest
#define noinfo

/* necessary for the HP */
#ifdef HP
#define S_IRWXU 0000700  
#endif



#define maxlines        15000
#define max_pl_lines   850000 /* 27-11-97 increase by 100,000 -MA */
                              /* 04-12-98 increase by 300,000 JCT */
#define max_edits        1500
#define maxlen            150
#define maxlen2           300
#define maxlen3         20000
#define max_decks        3000 /* 25-6-96 Now more decks - MDC */
#define max_comdecks     3000 /* 17-8-98 Increased from 1500 - AV */
#define max_defs         1000
#define max_directives   2500
#define max_calls        1420
#define max_name_len      100
#define max_directive_len  30
#define max_operators      10
#define max_operator_len    5
#define max_filename_len  100
#define max_options        10       

#define in_bufsiz         512
#define out_bufsiz        512


#define max_conds           6

#define system_error        -1
#define user_error          -2
#define unknown_error       -3

#define directve            0
#define PL_directve         1
#define source_code         2
#define ignore              3

#define not_found         102

#define max_call_nest      30
#define max_if_nest        30

#define tmpfilename      "QQ"
#define maxtempfiles     2000

#define default_siffix    'f'



#ifdef ALT_STRCMP
#	define strcmp strrcmp
#	define strncmp strrncmp
#endif


/* Temporary work directory 

#define  tmpdir          "_up.tmp"
 
Replaced by: char tmpdir[12]; in all necessary files,  14-11-95  Marc Chippindale */

/* Changed by Anette Van der Wal 1/2/00 to get rid of warning message */ 
#define  atexit_t void

/* 
   If the PL name has not been entered assume it is not needed, but create 
   a directory and files index and xref for use in other parts of the prog.
   Their contents are not important.

-All this is done in update.c now as global variables. 15-11-95. Marc

#define  PL_tmp          "_up.tmp\0"
#define  index_tmp       "_up.tmp/index\0"
#define  xref_tmp        "_up.tmp/xref\0"
*/

/*
   If the directives file is not specified create a temporary file with
   dummy information inside.

-Done in update.c now. See above. 15-11-95. Marc Chippindale

#define  dirv_tmp        "_up.tmp/dirv.tmp\0"
*/

#define	dummy_char      '£'
#define	ignore_char     '~'



#define  nupopen( filehandle, filename, action, status )               \
         if ( ( filehandle = fopen( filename, action ) ) == NULL )   \
         {                                                           \
            printf( "\nUnabel to open file %s\n", filename );        \
				fflush( stdout );                                        \
            exit( status );                                          \
         }


#define  Malloc( ptr, length, status )                              \
         if ( ( ptr = (void *) malloc( length+1 ) ) == NULL )         \
         {                                                          \
            printf( "\nUnable to allocate memory" );                \
				fflush( stdout );                                       \
            exit( status );                                         \
         }


#define  Malloc_and_copy( ptr, srcptr )                                \
         if ( ( ptr = (void *) malloc( strlen(srcptr) + 2) ) == NULL ) \
         {                                                             \
            printf( "\nUnable to allocate memory" );                   \
				fflush( stdout );                                          \
            exit( system_error );                                      \
         }                                                             \
			else {                                                        \
				strcpy( ptr, srcptr );                                     \
			};
			


/*
FILE *PL_handle;
char Program_Library_name[max_name_len];

FILE *directives_handle;
char Directives_File_name[max_name_len];

FILE *compile_handle;
char compile_file_name[max_name_len];
*/

enum t_boolean { false, true, none };
enum types   { type1, type2, type3 };

#define  set_error()   \
         system( "echo 1 > _err.tmp" );


#define check_error()   \
   if ( ( error_handle = fopen( "_err.tmp", "r" ) ) != NULL )  {   \
      fclose( error_handle );                                      \
      unlink( "_err.tmp" );                                        \
      deltemp();                                                   \
      puts("\n*** Aborting due to Errors\n");                      \
		fflush( stdout );                                           \
      exit(3);                                                     \
   }


#define clear_error()   \
        unlink( "_err.tmp" );


#define alloc_buffer( handle, bufr, buftype, bufsize, name )      \
	if ( setvbuf( handle, bufr, buftype, bufsize ) != 0 ) {        \
		printf( "Failed to set up buffer for file %s", name );      \
		fflush( stdout );                                           \
		exit(2);                                                    \
	}



#ifdef SAVE_LOG

#define prt_error( message1, message2, err_num )  \
	{  																					\
		fputs( " ", stderr ); fputs( message1, stderr );               \
		fputs( message2, stderr ); fputs( " ", stderr );               \
		fputs( " ", logfile ); fputs( message1, logfile );             \
		fputs( message2, logfile ); fputs( " ", logfile );             \
		return( err_num );                                             \
	}

#else

#define prt_error( message1, message2, err_num )  \
	{  																					\
		fputs( " ", stderr ); fputs( message1, stderr );               \
		fputs( message2, stderr ); fputs( " ", stderr );               \
		return( err_num );                                             \
	}

#endif


#define  fputz( buffer, outfile )  \
	fprintf( outfile, "%s", buffer );




#ifdef MSDOS
#  define deckfile    "%s\\%s.dk"
#  define comdeckfile "%s\\%s.cdk"
#  define decklist    "%s\\deck.lst"
#  define comdecklist "%s\\comdeck.lst"
#  define indexfile   "%s\\index"
#  define xreffile    "%s\\xref"
#else
#  define deckfile    "%s/%s.dk"
#  define comdeckfile "%s/%s.cdk"
#  define decklist    "%s/deck.lst"
#  define comdecklist "%s/comdeck.lst"
#  define indexfile   "%s/index"
#  define xreffile    "%s/xref"
#endif

