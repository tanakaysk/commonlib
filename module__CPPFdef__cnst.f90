module cnst
  ! type parameters

  implicit none
  public

  integer, parameter:: i1  = selected_int_kind( 2 )       ! 1-byte integer
  integer, parameter:: i2  = selected_int_kind( 4 )       ! 2-byte integer
  integer, parameter:: i4  = selected_int_kind( 8 )       ! 4-byte integer
  integer, parameter:: i8  = selected_int_kind( 16 )      ! 8-byte integer
  integer, parameter:: r4  = selected_real_kind( p = 6 )  ! single precision float
  integer, parameter:: r8  = selected_real_kind( p = 12 ) ! double precision float
  integer, parameter:: r16 = selected_real_kind( p = 24 ) ! quad precision float

end module cnst
