
/* $Id: pvmfgsize.m4,v 1.2 1996/10/04 15:27:10 pvmsrc Exp $ */

#include "pvm3.h"
#include "pvm_consts.h"

void
FUNCTION(pvmfgsize) ARGS(`STRING_ARG(group), size')
STRING_ARG_DECL(group);
int *size;
{
	char tgroup[MAX_GRP_NAME + 1];

	/*
	 * Copy the group name to make sure there's
	 * a NUL at the end.
	 */
	if (ftocstr(tgroup, sizeof(tgroup), STRING_PTR(group), STRING_LEN(group))){
		*size = PvmBadParam;
		return;
	}

	*size = pvm_gsize(tgroup);
}

