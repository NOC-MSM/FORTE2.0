C
C -- param_cou.h
C
	INTEGER jpmaxfld
	PARAMETER(jpmaxfld = 20)        ! Maximum number of fields exchanged
                                        ! between ocean and atmosphere
	INTEGER jpflda2o1
	PARAMETER(jpflda2o1 = 4)         ! Number of fields exchanged from
                                         ! atmosphere to ocean via flx.F
	INTEGER jpflda2o2
	PARAMETER(jpflda2o2 = 4)         ! Number of fields exchanged from
                                         ! atmosphere to ocean via tau.F
C
	INTEGER jpfldo2a
	PARAMETER(jpfldo2a = 2)          ! Number of fields exchanged from
                                         ! ocean to atmosphere
C
