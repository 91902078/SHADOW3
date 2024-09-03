module mpmath
use, intrinsic :: ISO_C_BINDING    
use mpfung
    
public CROSS_MP, DOT_MP,NORM_MP
!private scalar_mp1, scalar_mp2,scalar_mp3
PRIVATE scalar_mp1,scalar_mp2,scalar_mp3
interface scalar_mp
  module procedure scalar_mp1
  module procedure scalar_mp2
  module procedure scalar_mp3
end interface

contains
!C
!C       scalar multiplication
!C ----
SUBROUTINE SCALAR_MP1( V1,ARG,V2)
    !IMPLICIT REAL(KIND=SKR)  (A-H,O-Z)
    !IMPLICIT INTEGER(KIND=SKI) (I-N)
    implicit none
    type(mp_real),dimension(3),intent(in)  :: v1
    type(mp_real),             intent(in)  :: arg
    type(mp_real),dimension(3),intent(out) :: v2
    !DIMENSION    V1(3),V2(3)
    V2(1) =   V1(1)*arg
    V2(2) =   V1(2)*ARG
    V2(3) =   V1(3)*arg

!C
!C If the numbers are *very* small, zero them out. This is not done
!C for VMS, since it seems to work out fine. Why mess with something
!C that already works.
!C

END SUBROUTINE SCALAR_MP1

!C
!C       scalar multiplication
!C ----
SUBROUTINE SCALAR_MP2( V1,ARG,V2)
    !IMPLICIT REAL(KIND=SKR)  (A-H,O-Z)
    !IMPLICIT INTEGER(KIND=SKI) (I-N)
    implicit none
    real(kind=8),dimension(3),intent(in)  :: v1
    REAL(KIND=8),             intent(in)  :: arg
    type(mp_real),dimension(3),intent(out) :: v2
    type(mp_real) MP_TEMP,MP_ARG
    !DIMENSION    V1(3),V2(3)
    MP_TEMP = MPREALD(V1(1))
    MP_ARG = MPREALD(arg)
    V2(1) =   MP_TEMP*MP_ARG
    MP_TEMP = MPREALD(V1(2))
    V2(2) =   MP_TEMP*MP_ARG
    MP_TEMP = MPREALD(V1(3))
    V2(3) =   MP_TEMP*MP_ARG

END SUBROUTINE SCALAR_MP2
!C
!C       scalar multiplication
!C ----
SUBROUTINE SCALAR_MP3( V1,ARG,V2)

    !IMPLICIT REAL(KIND=SKR)  (A-H,O-Z)
    !IMPLICIT INTEGER(KIND=SKI) (I-N)
    implicit none
    type(mp_real),dimension(3),intent(in)  :: v1
    type(mp_real),             intent(in)  :: arg
    REAL(KIND=8),dimension(3),intent(out) :: v2
    type(mp_real) MP_TEMP
    !DIMENSION    V1(3),V2(3)

    V2(1) =   V1(1)*ARG
    V2(2) =   V1(2)*ARG
    V2(3) =   V1(3)*ARG

!C
!C If the numbers are *very* small, zero them out. This is not done
!C for VMS, since it seems to work out fine. Why mess with something
!C that already works.
!C
    IF (ABS(V2(1)).LT.1.0D-31) V2(1) = 0.0D0
    IF (ABS(V2(2)).LT.1.0D-31) V2(2) = 0.0D0
    IF (ABS(V2(3)).LT.1.0D-31) V2(3) = 0.0D0

END SUBROUTINE SCALAR_MP3

! C
! C   	vector product :    vres = v1 x v2
! C ----
SUBROUTINE CROSS_MP (V1,V2,VRES,M_FLAG)
!    use mpfung
!    use, intrinsic :: ISO_C_BINDING 
    !IMPLICIT REAL(8)  (A-H,O-Z)
    !IMPLICIT INTEGER(8) (I-N)
    implicit none
    type(mp_real),dimension(3),intent(in)   :: v1,v2
    type(mp_real),dimension(3),intent(out)  :: vres
     integer(KIND=c_int),optional, intent (out)       :: M_FLAG
    !DIMENSION   V1(3),V2(3),VRES (3)
    type(mp_real) MP_TEMP, ttest
    VRES(1) =     V1(2)*V2(3) - V1(3)*V2(2)
    VRES(2) = - ( V1(1)*V2(3) - V1(3)*V2(1) )
    VRES(3) =     V1(1)*V2(2) - V1(2)*V2(1)

!C
!C If the numbers are *very* small, zero them out. This is not done
!C for VMS, since it seems to work out fine. Why mess with something
!C that already works.
!C
    MP_TEMP = MPREALD(1.0D-31)
    TTEST  =  VRES(1)*VRES(1) + VRES(2)*VRES(2) + VRES(3)*VRES(3)
    IF (TTEST.LT.MP_TEMP .AND. present (M_FLAG) ) THEN
        M_FLAG = 1
    ELSE
        IF ( present (M_FLAG) ) M_FLAG = 0
    END IF
    END SUBROUTINE CROSS_MP

! C
! C       scalar product 
! C ----
    SUBROUTINE DOT_MP (V1,V2,RES)
 !       use mpfung

        !IMPLICIT REAL(8)  (A-H,O-Z)
        !IMPLICIT INTEGER(8)  (I-N)
        !DIMENSION    V1(3),V2(3)
        implicit none
        type(mp_real),dimension(3),intent(in)  :: v1,v2
        type(mp_real),             intent(out) :: res
        type(mp_real) MP_TEMP

        RES = V1(1)*V2(1) + V1(2)*V2(2) + V1(3)*V2(3)

!C
!C If the numbers are *very* small, zero them out. This is not done
!C for VMS, since it seems to work out fine. Why mess with something
!C that already works.
!C
        MP_TEMP = MPREALD(1.0D-31)
        IF (ABS(RES).LT.MP_TEMP) RES = 0.0D0
      RETURN
    END SUBROUTINE DOT_MP

! C
! C       vector normalization 
! C ----

    SUBROUTINE NORM_MP (V1,V2)
!        use mpfung
        !IMPLICIT REAL(KIND=SKR)  (A-H,O-Z)
        !IMPLICIT INTEGER(KIND=SKI)  (I-N)
        !DIMENSION     V1(3),V2(3)
        implicit none
        type(mp_real),dimension(3),intent(in)   :: v1
        type(mp_real),dimension(3),intent(out)  :: v2

        type(mp_real)   :: rnorm, MP_TEMP

        RNORM = V1(1)**2 + V1(2)**2 + V1(3)**2
        RNORM = SQRT(RNORM)

!C
!C If the numbers are *very* small, zero them out. This is not done
!C for VMS, since it seems to work out fine. Why mess with something
!C that already works.
!C
        MP_TEMP = MPREALD(1.0D-31)
        IF (ABS(RNORM).LT.MP_TEMP) RNORM = MPREALD(0.0D0)

        IF (RNORM.NE.0.0D0) THEN
           RNORM = 1/RNORM
           V2(1) = V1(1)*RNORM
           V2(2) = V1(2)*RNORM
           V2(3) = V1(3)*RNORM
        END IF
        RETURN

        END SUBROUTINE NORM_MP

end module mpmath