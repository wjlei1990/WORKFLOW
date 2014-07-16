! Some parts are copied from FLEXWIN
! Ebru, Princeton, April 2013; Modified by JAS, Mar 2014
  module process_par
  
!===================================================================
  ! filter parameters for xapiir subroutine (filter type is BP)
  double precision, parameter :: TRBDNDW = 0.3
  double precision, parameter :: APARM = 30.0
  integer, parameter :: IORD = 4 
  integer, parameter :: PASSES = 2

  ! -------------------------------------------------------------
  ! array dimensions
  integer, parameter :: NDIM = 500000
  integer, parameter :: MAX_FILE_LENGTH = 200
  integer, parameter :: MAX_STATIONS = 2000

  ! FFT parameters
  real, parameter :: FORWARD_FFT = 1.0
  real, parameter :: REVERSE_FFT = -1.0
  
  ! -------------------------------------------------------------
  double precision, parameter :: eps = 0.01                
  ! -------------------------------------------------------------
  ! miscellaneous - do not modify!
  ! -------------------------------------------------------------

  ! mathematical constants
  double precision, parameter :: PI = 3.1415926535897
  double precision, parameter :: E  = 2.7182818284590
  double precision, parameter :: TWOPI = 2.0 * PI

  ! other constants
  complex*16, parameter :: CCI = cmplx(0.,1.)
  double precision, parameter :: LARGE_VAL = 1.0d8

  ! filter types
  integer, parameter :: HANNING = 1
  integer, parameter :: HAMMING = 2
  integer, parameter :: COSINE  = 3

end module process_par
