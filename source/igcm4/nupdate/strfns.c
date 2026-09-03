#include <string.h>

int strrcmp( string1, string2 )
char *string1, *string2;
{
	if ( string1 == NULL ) return(-1);
	if ( string2 == NULL ) return(-2);

	return( strcmp( string1, string2 ) );
}



int strrncmp( string1, string2, length )
char *string1, *string2;
int  length;
{
	if ( string1 == NULL ) return(-1);
	if ( string2 == NULL ) return(-2);

	return( strncmp( string1, string2, length ) );
}

