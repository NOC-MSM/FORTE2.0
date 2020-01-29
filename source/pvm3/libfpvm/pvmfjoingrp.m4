
/* $Id: pvmfjoingrp.m4,v 1.2 1996/10/04 15:27:14 pvmsrc Exp $ */

#include "pvm3.h"
#include "pvm_consts.h"

void
FUNCTION(pvmfjoingroup) ARGS(`STRING_ARG(group), inum')
STRING_ARG_DECL(group);
int *inum;
{
	char tgroup[MAX_GRP_NAME + 1];

	/*
	 * Copy the group name to make sure there's
	 * a NUL at the end.
	 */
	if (ftocstr(tgroup, sizeof(tgroup), STRING_PTR(group), STRING_LEN(group))){
		*inum = PvmBadParam;
		return;
	}

	*inum = pvm_joingroup(tgroup);
}

