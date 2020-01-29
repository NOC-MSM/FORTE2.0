C
C -- clim.h   18-08-95   Version 2.0   Author: Laurent Terray
C    ******
C@
C@  Contents : variables related to the CLIM library
C@  --------
C@ For complete definition, see the CLIM manual
C@
      INTEGER*4	CLIM_MaxMod,    CLIM_MaxPort,  CLIM_MaxSegments,
     *          CLIM_MaxTag,
     *          CLIM_MaxLink,
     *          CLIM_ParSize,
     *          CLIM_Clength
      INTEGER*4 CLIM_Void
C
      INTEGER*4 CLIM_In,	CLIM_Out,	CLIM_InOut
C
      INTEGER*4 CLIM_Strategy,  CLIM_Segments,  
     *          CLIM_Serial,    CLIM_Length,    CLIM_Orange,
     *          CLIM_Apple,     CLIM_Offset,
     *          CLIM_Box,	CLIM_SizeX,	CLIM_SizeY,
     *          CLIM_LdX
C
      INTEGER*4 CLIM_Integer,	CLIM_Real,	CLIM_Double
C
      INTEGER*4 CLIM_StopPvm,   CLIM_ContPvm
C
      INTEGER*4	CLIM_Ok
      INTEGER*4 CLIM_FastExit, 	CLIM_BadName, 	CLIM_BadPort,
     *          CLIM_BadType, 	CLIM_DoubleDef, CLIM_NotStep,
     *          CLIM_IncStep, 	CLIM_IncSize, 	CLIM_NotClim,
     *          CLIM_TimeOut,
     *          CLIM_Pvm, 	CLIM_FirstCall, CLIM_PbRoute,
     *          CLIM_Group, 	CLIM_BadTaskId, CLIM_NoTask,
     *          CLIM_InitBuff, 	CLIM_Pack, 	CLIM_Unpack,
     *          CLIM_Down, 	CLIM_PvmExit
C
C-----Parameter sizes
C
      PARAMETER ( CLIM_Void    = 0  )
      PARAMETER ( CLIM_MaxMod  = 8 )
      PARAMETER ( CLIM_MaxPort = 16 )
      PARAMETER ( CLIM_MaxSegments = 160 )
      PARAMETER ( CLIM_MaxLink = CLIM_MaxMod * CLIM_MaxPort )
      PARAMETER ( CLIM_ParSize = 2*CLIM_MaxSegments+2 )
      PARAMETER ( CLIM_MaxTag  = 16777215 )
      PARAMETER ( CLIM_Clength = 32 )
C
C-----Ports status
C
      PARAMETER ( CLIM_In      = 1 )
      PARAMETER ( CLIM_Out     = 0 )
      PARAMETER ( CLIM_InOut   = 2 )
C
C-----Parallel distribution
C
      PARAMETER ( CLIM_Strategy = 1 )
      PARAMETER ( CLIM_Segments = 2 )
      PARAMETER ( CLIM_Serial   = 0 )
      PARAMETER ( CLIM_Apple    = 1 )
      PARAMETER ( CLIM_Box      = 2 )
      PARAMETER ( CLIM_Orange   = 3 )
      PARAMETER ( CLIM_Offset   = 2 )
      PARAMETER ( CLIM_Length   = 3 )
      PARAMETER ( CLIM_SizeX    = 3 )
      PARAMETER ( CLIM_SizeY    = 4 )
      PARAMETER ( CLIM_LdX      = 5 )
C
C-----Datatypes
C
      PARAMETER ( CLIM_Integer = 1 )
      PARAMETER ( CLIM_Real    = 4 ) 
      PARAMETER ( CLIM_Double  = 8 )
C
C-----Quit parameters
C
      PARAMETER ( CLIM_ContPvm = 0 )
      PARAMETER ( CLIM_StopPvm = 1 )
C
C-----Error Codes
C
      PARAMETER ( CLIM_Ok	 = 0 )
      PARAMETER ( CLIM_FastExit  = -1 )
      PARAMETER ( CLIM_BadName   = -2 )
      PARAMETER ( CLIM_BadPort   = -3 )
      PARAMETER ( CLIM_BadType   = -4 )
      PARAMETER ( CLIM_DoubleDef = -5 )
      PARAMETER ( CLIM_NotStep   = -6 )
      PARAMETER ( CLIM_IncStep   = -7 )
      PARAMETER ( CLIM_IncSize   = -8 )
      PARAMETER ( CLIM_NotClim   = -9 )
      PARAMETER ( CLIM_TimeOut   = -10 )
      PARAMETER ( CLIM_Pvm       = -11 )
      PARAMETER ( CLIM_FirstCall = -12 )
      PARAMETER ( CLIM_PbRoute   = -13 )
      PARAMETER	( CLIM_Group     = -14 )
      PARAMETER ( CLIM_BadTaskId = -15 )
      PARAMETER ( CLIM_NoTask    = -16 )
      PARAMETER ( CLIM_InitBuff  = -17 )
      PARAMETER ( CLIM_Pack      = -18 )
      PARAMETER ( CLIM_Unpack    = -19 )
      PARAMETER ( CLIM_Down      = -20 )
      PARAMETER ( CLIM_PvmExit   = -21 )
C
C     --- end of clim.h
