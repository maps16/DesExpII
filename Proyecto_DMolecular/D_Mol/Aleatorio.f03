FUNCTION ZRAN(ISEED)
  implicit real*8 (a-h,o-z)
  common/semillas/iseed3,iseed2,iseed1
  mzran=iseed3-iseed1

  if(mzran.lt.0) mzran=mzran+2147483579
  iseed3=iseed2
  iseed2=iseed1
  iseed1=mzran
  iseed=ishft(3533*ishft(iseed,-16)+iand(iseed,125535),16)+3533*iand(iseed,65535)
  mzran=mzran+iseed
  zran=.5+.2328306d-9*mzran

  RETURN
END FUNCTION ZRAN

SUBROUTINE AZARG( ISEED,X )
  IMPLICIT DOUBLE PRECISION (A-H,O-Z)
  external zran
  common/semillas/iseed3,iseed2,iseed1

  pi=4.0*atan(1.0)
  R=zran(iseed)
  S=zran(iseed)
  X=SQRT(-2.0*LOG(R))*COS(2.0*PI*S)

  RETURN
END SUBROUTINE AZARG
