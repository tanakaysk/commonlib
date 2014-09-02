module nclib

  use netcdf
  use cnst, only: i4, r4, r8

  implicit none
  private
  public:: ncdimCOARDS, ncvarCOARDSi, ncvarCOARDSf, &
  &        nclib__COARDS_create, nclib__COARDS_putvar, &
  &        CFnclib__glbAttr, &
  &        CFnclib__dimensionAttribute, CFnclib__dimensionPut, &
  &        CFnclib__varAttr, CFnclib__varAttr_range, CFnclib__varPut, &
  &        nclib__errMssg, nclib__handle_error

  integer(i4),        parameter:: dim_types(3) = (/ NF90_INT, NF90_FLOAT, NF90_CHAR /)
  character(len = 5), parameter:: dim_ctype(3) = (/ 'int  ',  'float',    'chr  '   /)

  type ncdimCOARDS
    character(len = 30)  :: name      ! dimension name
    character(len = 99)  :: long_name ! detailed description
    character(len = 1)   :: drct      ! direction of the axis [x/y/z/t]
    character(len = 30)  :: units     ! unit of the dimension
    logical              :: rvs_axis  ! reversal axis (ex.: positive ocean depth)
    logical              :: geoaxis   ! geological coordinate
                                      !   (horizontal units are automatically set if TRUE)
    integer(i4)          :: len       ! dimension length
    real(r4), allocatable:: value(:)  ! value of this dimension
  end type ncdimCOARDS

  type ncvarCOARDSf
    character(len = 30)     :: name      ! dimension name
    character(len = 99)     :: long_name ! detailed description
    character(len = 30)     :: units     ! unit of the dimension
    integer(i4)             :: dims      ! number of dimensions
    integer(i4), allocatable:: shapes(:) ! array shape
    integer(i4), allocatable:: dimids(:) ! dimension IDs of each dimension
    real(r4),    allocatable:: var(:)    ! reshaped values of the variable
    real(r4)                :: undef     ! undefined value
  end type ncvarCOARDSf

  type ncvarCOARDSi
    character(len = 30)     :: name      ! dimension name
    character(len = 99)     :: long_name ! detailed description
    character(len = 30)     :: units     ! unit of the dimension
    integer(i4)             :: dims      ! number of dimensions
    integer(i4), allocatable:: shapes(:) ! array shape
    integer(i4), allocatable:: dimids(:) ! dimension IDs of each dimension
    integer(i4), allocatable:: var(:)    ! reshaped values of the variable
    integer(i4)             :: undef     ! undefined value
  end type ncvarCOARDSi

  interface nclib__COARDS_putvar
    module procedure nclib__COARDS_putvar_int
    module procedure nclib__COARDS_putvar_float
  end interface nclib__COARDS_putvar

  interface CFnclib__dimensionPut
    module procedure CF_dimensionPut_int
    module procedure CF_dimensionPut_float
    module procedure CF_dimensionPut_char
  end interface CFnclib__dimensionPut

  interface CFnclib__varAttr_range
    module procedure CF_varAttr_range_int
    module procedure CF_varAttr_range_float
  end interface CFnclib__varAttr_range

  interface CFnclib__varPut
    module procedure CF_varPut_float_1D
    module procedure CF_varPut_float_2D
    module procedure CF_varPut_float_3D
    module procedure CF_varPut_float_4D
  end interface CFnclib__varPut

  contains

    !===== COARDS netCDF file output =====
    subroutine nclib__COARDS_create( file_nc, title, ncdims, ncid, dimids, ierr )
      character(len = *),  intent(in) :: file_nc      ! name of output netCDF file
      character(len = *),  intent(in) :: title        ! title of the netCDF file
      type(ncdimCOARDS),   intent(in) :: ncdims(:)    ! netCDF dimension information
      integer(i4),         intent(out):: ncid         ! netCDF ID
      integer(i4),         intent(out):: dimids(:, :) ! dimension and corresponding variable IDs
      integer(i4),         intent(out):: ierr         ! I/O state flag

      integer(i4)        :: ndims ! number of dimensions
      integer(i4)        :: n     ! loop variables
      integer(i4)        :: nerr
      character(len = 30):: units ! unit of dimesnion
      character(len = 30):: name  ! dimension/variable name

      nerr = nf90_create( file_nc, NF90_NOCLOBBER, ncid )
      call nclib__handle_error( nerr, 'NF90_CREATE', ierr )
      if ( ierr /= 0 ) return

      ! define dimensions and dimension variables
      ndims = size( ncdims )
      do n = 1, ndims
        name = ncdims(n)%name

        nerr = nf90_def_dim( ncid, trim( name ), ncdims(n)%len, dimids(1, n) )
        call nclib__handle_error( nerr, 'NF90_DEF_DIM (' // trim( name ) // ')', ierr )
        if ( ierr /= 0 ) return
        nerr =  nf90_def_var( ncid, trim( name ), dim_types(2), dimids(1, n:n), dimids(2, n) )
        call nclib__handle_error( nerr, 'NF90_DEF_VAR (' // trim( name ) // ')', ierr )
        if ( ierr /= 0 ) return

        nerr = nf90_put_att( ncid, dimids(2, n), 'long_name', trim( ncdims(n)%long_name ) )
        call nclib__handle_error( nerr, 'NF90_PUT_ATT (' // trim( name ) // ':long_name)', ierr )
        if ( ierr /= 0 ) return

        units = trim( ncdims(n)%units )
        if ( ncdims(n)%drct == 'x' .and. ncdims(n)%geoaxis ) units = 'degrees_east'
        if ( ncdims(n)%drct == 'y' .and. ncdims(n)%geoaxis ) units = 'degrees_north'
        nerr = nf90_put_att( ncid, dimids(2, n), 'units', trim( units ) )
        call nclib__handle_error( nerr, 'NF90_PUT_ATT (' // trim( name ) // ':units)', ierr )
        if ( ierr /= 0 ) return

        if ( ncdims(n)%drct == 'z' ) then
          if ( ncdims(n)%rvs_axis ) then
            nerr = nf90_put_att( ncid, dimids(2, n), 'positive', 'down' )
          else
            nerr = nf90_put_att( ncid, dimids(2, n), 'positive', 'up' )
          endif
          call nclib__handle_error( nerr, 'NF90_PUT_ATT (' // trim( name ) // ':positive)', ierr )
          if ( ierr /= 0 ) return
        endif
      enddo

      ! put global attributes for COARDS
      nerr = nf90_put_att( ncid, NF90_GLOBAL, 'Conventions', 'COARDS' )
      call nclib__handle_error( nerr, 'NF90_PUT_ATT (Conventions)', ierr )
      if ( ierr /= 0 ) return
      nerr = nf90_put_att( ncid, NF90_GLOBAL, 'title', trim( title ) )
      call nclib__handle_error( nerr, 'NF90_PUT_ATT (title)', ierr )
      if ( ierr /= 0 ) return
      nerr = nf90_put_att( ncid, NF90_GLOBAL, 'source', 'MODULE nclib__COARDS_create' )
      call nclib__handle_error( nerr, 'NF90_PUT_ATT (source)', ierr )
      if ( ierr /= 0 ) return

      ! put dimension variables
      nerr = nf90_enddef( ncid )
      call nclib__handle_error( nerr, 'NF90_ENDDEF', ierr )
      if ( ierr /= 0 ) return

      do n = 1, ndims
        nerr = nf90_put_var( ncid, dimids(2, n), ncdims(n)%value )
        call nclib__handle_error( nerr, 'NF90_PUT_VAR (' // trim( ncdims(n)%name ) // ')', ierr )
        if ( ierr /= 0 ) return
      enddo

    end subroutine nclib__COARDS_create


    subroutine nclib__COARDS_putvar_int( ncid, ncvars, ierr )
      integer(i4),        intent(in) :: ncid      ! netCDF ID
      type(ncvarCOARDSi), intent(in) :: ncvars(:) ! variables
      integer(i4),        intent(out):: ierr      ! status flag

      integer(i4), allocatable:: varids(:) ! variable IDs
      integer(i4)             :: nvar      ! number of variables
      integer(i4)             :: n         ! loop variable
      integer(i4)             :: nerr
      integer(i4)             :: shape2(2) 
      integer(i4)             :: shape3(3) 
      integer(i4)             :: shape4(4) 
      character(len = 30)     :: name      ! variable name

      nerr = nf90_redef( ncid )
      call nclib__handle_error( nerr, 'NF90_REDEF', ierr )
      if ( ierr /= 0 ) return

      ! definition of variables
      nvar = size( ncvars )
      allocate(varids(nvar))
      do n = 1, nvar
        name = ncvars(n)%name
        call nclib__COARDS_defvar( ncid, trim( name ), trim( ncvars(n)%long_name ), &
        &                          trim( ncvars(n)%units ), 1, ncvars(n)%dimids, varids(n), ierr )
        if ( ierr /= 0 ) return
        nerr = nf90_put_att( ncid, varids(n), '_FillValue', ncvars(n)%undef )
        call nclib__handle_error( nerr, 'NF90_PUT_ATT (' // name // ':' // '_FillValue)', ierr )
        if ( ierr /= 0 ) return
      enddo

      ! put variable values
      nerr = nf90_enddef( ncid )
      call nclib__handle_error( nerr, 'NF90_ENDDEF', ierr )
      if ( ierr /= 0 ) return

      do n = 1, nvar
        name = ncvars(n)%name
        select case ( ncvars(n)%dims )
          case ( 2 )
            shape2 = ncvars(n)%shapes
            nerr = nf90_put_var( ncid, varids(n), reshape( ncvars(n)%var, shape2 ) )
          case ( 3 )
            shape3 = ncvars(n)%shapes
            nerr = nf90_put_var( ncid, varids(n), reshape( ncvars(n)%var, shape3 ) )
          case ( 4 )
            shape4 = ncvars(n)%shapes
            nerr = nf90_put_var( ncid, varids(n), reshape( ncvars(n)%var, shape4 ) )
          case default
            print '(a)', 'Not correspond to given variable dimansion number'
            ierr = 100
            return
        end select
        call nclib__handle_error( nerr, 'NF90_PUT_VAR (' // trim( name ) // ')', ierr )
      enddo

    end subroutine nclib__COARDS_putvar_int


    subroutine nclib__COARDS_putvar_float( ncid, ncvars, ierr )
      integer(i4),        intent(in) :: ncid      ! netCDF ID
      type(ncvarCOARDSf), intent(in) :: ncvars(:) ! variables
      integer(i4),        intent(out):: ierr      ! status flag

      integer(i4), allocatable:: varids(:) ! variable IDs
      integer(i4)             :: nvar      ! number of variables
      integer(i4)             :: n         ! loop variable
      integer(i4)             :: nerr
      integer(i4)             :: shape2(2) 
      integer(i4)             :: shape3(3) 
      integer(i4)             :: shape4(4) 
      character(len = 30)     :: name      ! variable name

      nerr = nf90_redef( ncid )
      call nclib__handle_error( nerr, 'NF90_REDEF', ierr )
      if ( ierr /= 0 ) return

      ! definition of variables
      nvar = size( ncvars )
      allocate(varids(nvar))
      do n = 1, nvar
        name = ncvars(n)%name
        call nclib__COARDS_defvar( ncid, trim( name ), trim( ncvars(n)%long_name ), &
        &                          trim( ncvars(n)%units ), 2, ncvars(n)%dimids, varids(n), ierr )
        if ( ierr /= 0 ) return
        nerr = nf90_put_att( ncid, varids(n), '_FillValue', ncvars(n)%undef )
        call nclib__handle_error( nerr, 'NF90_PUT_ATT (' // name // ':' // '_FillValue)', ierr )
        if ( ierr /= 0 ) return
      enddo

      ! put variable values
      nerr = nf90_enddef( ncid )
      call nclib__handle_error( nerr, 'NF90_ENDDEF', ierr )
      if ( ierr /= 0 ) return

      do n = 1, nvar
        name = ncvars(n)%name
        select case ( ncvars(n)%dims )
          case ( 2 )
            shape2 = ncvars(n)%shapes
            nerr = nf90_put_var( ncid, varids(n), reshape( ncvars(n)%var, shape2 ) )
          case ( 3 )
            shape3 = ncvars(n)%shapes
            nerr = nf90_put_var( ncid, varids(n), reshape( ncvars(n)%var, shape3 ) )
          case ( 4 )
            shape4 = ncvars(n)%shapes
            nerr = nf90_put_var( ncid, varids(n), reshape( ncvars(n)%var, shape4 ) )
          case default
            print '(a)', 'Not correspond to given variable dimansion number'
            ierr = 100
        end select
        call nclib__handle_error( nerr, 'NF90_PUT_VAR (' // trim( name ) // ')', ierr )
        if ( ierr /= 0 ) return
      enddo

    end subroutine nclib__COARDS_putvar_float


    subroutine nclib__COARDS_defvar( ncid, name, long_name, units, xtype, dimids, varid, ierr )
      integer(i4),        intent(in) :: ncid      ! netCDF ID
      integer(i4),        intent(in) :: xtype     ! type flag of dimension variables
      integer(i4),        intent(in) :: dimids(:) ! dimension IDs of the variable dimensions
      integer(i4),        intent(out):: varid     ! variable ID
      integer(i4),        intent(out):: ierr      ! status flag
      character(len = *), intent(in) :: name      ! variable name
      character(len = *), intent(in) :: long_name ! detailed variable name
      character(len = *), intent(in) :: units     ! unit of variable

      integer(i4):: nerr

      nerr = nf90_def_var( ncid, name, dim_types(xtype), dimids, varid )
      call nclib__handle_error( nerr, 'NF90_DEF_VAR (' // name // ')', ierr )
      if ( ierr /= 0 ) return

      nerr = nf90_put_att( ncid, varid, 'long_name', long_name )
      call nclib__handle_error( nerr, 'NF90_PUT_ATT (' // name // ':' // 'long_name)', ierr )
      if ( ierr /= 0 ) return
      nerr = nf90_put_att( ncid, varid, 'units', units )
      call nclib__handle_error( nerr, 'NF90_PUT_ATT (' // name // ':' // 'units)', ierr )
      if ( ierr /= 0 ) return
    end subroutine nclib__COARDS_defvar


    subroutine CFnclib__glbAttr( ncid, title, source )
      integer(i4),        intent(in):: ncid ! netCDF ID
      character(len = *), intent(in):: title
      character(len = *), intent(in):: source

      integer(i4):: nerr, ierr

      nerr = nf90_put_att( ncid, NF90_GLOBAL, 'Conventions', 'CF-1.6' )
      call nclib__errMssg( nerr, 'NF90_PUT_ATT (Conventions)', ierr )
      nerr = nf90_put_att( ncid, NF90_GLOBAL, 'title', title )
      call nclib__errMssg( nerr, 'NF90_PUT_ATT (title)', ierr )
      nerr = nf90_put_att( ncid, NF90_GLOBAL, 'source', source )
      call nclib__errMssg( nerr, 'NF90_PUT_ATT (source)', ierr )

    end subroutine CFnclib__glbAttr


    subroutine CFnclib__dimensionAttribute( ncid, dimName, dimLength, dimID, varID, dimKind, &
    &  standard_name, long_name, units, axis, positive, calendar, dateRef )
      integer(i4),                   intent(in) :: ncid          ! netCDF ID
      integer(i4),                   intent(out):: dimID         ! dimension ID
      integer(i4),                   intent(out):: varID         ! variable ID
      integer(i4),                   intent(in) :: dimLength     ! length of the dimension
      character(len = *),            intent(in) :: dimName       ! name of the dimension
      character(len = *),            intent(in) :: dimKind       ! kind of dimension variable
      character(len = *),  optional, intent(in) :: standard_name
      character(len = *),  optional, intent(in) :: long_name
      character(len = *),  optional, intent(in) :: units
      character(len = 1),            intent(in) :: axis
      character(len = *),  optional, intent(in) :: positive
      character(len = *),  optional, intent(in) :: calendar
      character(len = 20), optional, intent(in) :: dateRef       ! reference date of the time axis

      integer(i4)         :: nerr       ! return value of netCDF library functions
      integer(i4)         :: ierr
      character(len = 100):: units_time ! 'units' attribute for time dimension

      ! compliance checks
      if ( (present( long_name ) .eqv. .false.) .and. (present( standard_name ) .eqv. .false.) ) then
        print '(a)', 'CF convention compliance error (' // dimName // &
        &  '): neither "long_name" nor "standard_name" attribute is specified. '
        stop
      endif
      select case ( axis )
      case ( 'X', 'Y' )
      case ( 'Z' )
        if ( present( positive ) .eqv. .false. ) then
          print '(a)', 'CF convention compliance error (' // dimName // &
          & '): "positive" attribute is not specified for Z-axis. '
          stop
        endif
      case ( 'T' )
        if ( present( units ) .eqv. .false. ) then
          print '(a)', 'CF convention compliance error (' // dimName // &
          & '): "units" attribute is not specified for T-axis.'
          stop
        endif
        if ( present( dateRef ) ) then
          if ( dateRef(1:4) == '0000' .or. dateRef(5:6) == '00' .or. dateRef(7:8) == '00' ) then
            print '(a)', 'CF convention compliance error (' // dimName // &
            & '): invalid reference date for T-axis.'
            stop
          endif
        else
          print '(a)', 'CF convention compliance error (' // dimName // &
          & '): reference date is not specified for T-axis.'
          stop
        endif
      case default
        print '(a)', 'CF convention compliance error (' // dimName // '): invalid axis character.'
        stop
      end select

      ! get dimension and variable ID
      nerr = nf90_def_dim( ncid, dimName, dimLength, dimID )
      call nclib__errMssg( nerr, 'NF90_DEF_DIM (' // dimName // ')', ierr )
      if ( dimKind == 'integer' ) then
        nerr =  nf90_def_var( ncid, dimName, NF90_INT, dimID, varID )
      elseif ( dimKind == 'float' ) then
        nerr =  nf90_def_var( ncid, dimName, NF90_FLOAT, dimID, varID )
      elseif ( dimKind == 'character' ) then
        nerr =  nf90_def_var( ncid, dimName, NF90_CHAR, dimID, varID )
      else
        print '(a)', 'Error: (' // dimName // ') incorrect kind of dimension variable.'
        stop
      endif
      call nclib__errMssg( nerr, 'NF90_DEF_VAR (' // dimName // ')', ierr )

      ! attribute "standard_name" or/and "long_name"
      if ( present( standard_name ) ) then
        nerr = nf90_put_att( ncid, varID, 'standard_name', standard_name )
        call nclib__errMssg( nerr, 'NF90_PUT_ATT (' // dimName // ':standard_name)', ierr )
      endif
      if ( present( long_name ) ) then
        nerr = nf90_put_att( ncid, varID, 'long_name', long_name )
        call nclib__errMssg( nerr, 'NF90_PUT_ATT (' // dimName // ':long_name)', ierr )
      endif

      ! attribute "axis" and "positive"
      nerr = nf90_put_att( ncid, varID, 'axis', axis )
      call nclib__errMssg( nerr, 'NF90_PUT_ATT (' // dimName // ':axis)', ierr )
      if ( axis == 'Z' ) then
        nerr = nf90_put_att( ncid, varID, 'positive', positive )
        call nclib__errMssg( nerr, 'NF90_PUT_ATT (' // dimName // ':positive)', ierr )
      endif

      ! attribute "units" or "calendar"
      if ( axis == 'T' ) then
        if ( present( calendar ) ) then
          nerr = nf90_put_att( ncid, varID, 'calendar', calendar )
        else
          nerr = nf90_put_att( ncid, varID, 'calendar', 'gregorian' )
        endif
        call nclib__errMssg( nerr, 'NF90_PUT_ATT (' // dimName // ':calendar)', ierr )

        units_time = units // ' since ' // dateRef(1:4) // '-' // dateRef(5:6) // '-' // dateRef(7:8)
        if ( dateRef(9:20) /= '000000000000' ) then
          units_time = trim( units_time ) // ' ' // &
          &            dateRef(9:10) // ':' // dateRef(11:12) // ':' // dateRef(13:14)
          if ( dateRef(15:20) /= '000000' ) then
            units_time = trim( units_time ) // '.' // dateRef(15:20)
          endif
        endif
        nerr = nf90_put_att( ncid, varID, 'units', trim( units_time ) )
        call nclib__errMssg( nerr, 'NF90_PUT_ATT (' //  dimName // ':units)', ierr )
      else
        if ( present( units ) ) then
          nerr = nf90_put_att( ncid, varID, 'units', units )
          call nclib__errMssg( nerr, 'NF90_PUT_ATT (' // dimName // ':units)', ierr )
        else
          print '(a)', 'CF convention compliance warning (' // dimName // &
          & '): "units" attribute is not specified.'
        endif
      endif

    end subroutine CFnclib__dimensionAttribute


    subroutine CF_dimensionPut_int( ncid, varID, dimValue )
      integer(i4), intent(in):: ncid        ! netCDF ID
      integer(i4), intent(in):: varID       ! variable ID
      integer(i4), intent(in):: dimValue(:)

      integer(i4):: ierr, nerr

      nerr = nf90_put_var( ncid, varID, dimValue )
      call nclib__errMssg( nerr, 'NF90_PUT_VAR', ierr )
    end subroutine CF_dimensionPut_int


    subroutine CF_dimensionPut_float( ncid, varID, dimValue )
      integer(i4), intent(in):: ncid        ! netCDF ID
      integer(i4), intent(in):: varID       ! variable ID
      real(r4),    intent(in):: dimValue(:)

      integer(i4):: ierr, nerr

      nerr = nf90_put_var( ncid, varID, dimValue )
      call nclib__errMssg( nerr, 'NF90_PUT_VAR', ierr )
    end subroutine CF_dimensionPut_float


    subroutine CF_dimensionPut_char( ncid, varID, dimValue )
      integer(i4),        intent(in):: ncid        ! netCDF ID
      integer(i4),        intent(in):: varID       ! variable ID
      character(len = *), intent(in):: dimValue(:)

      integer(i4):: ierr, nerr

      nerr = nf90_put_var( ncid, varID, dimValue )
      call nclib__errMssg( nerr, 'NF90_PUT_VAR', ierr )
    end subroutine CF_dimensionPut_char


    subroutine CFnclib__varAttr( ncid, dimNameArray, dimIDarray, dimLenArray, &
    &  varName, ndim, varKind, dimList, standard_name, long_name, units, varID, vshape )
      integer(i4),                   intent(in) :: ncid            ! netCDF ID
      integer(i4),                   intent(in) :: dimIDArray(:)   ! list of dimension IDs
      integer(i4),                   intent(in) :: dimLenArray(:)  ! list of dimension length
      character(len = *),            intent(in) :: dimNameArray(:) ! list of dimension names
      integer(i4),                   intent(in) :: ndim            ! number of dimensions
      character(len = *),            intent(in) :: varName         ! name of the variable
      character(len = *),            intent(in) :: varKind         ! kind of variables
      character(len = *),            intent(in) :: dimList(:)      ! list of the variable dimension names
      character(len = *),  optional, intent(in) :: standard_name
      character(len = *),  optional, intent(in) :: long_name
      character(len = *),            intent(in) :: units
      integer(i4),                   intent(out):: varID           ! variable ID
      integer(i4),                   intent(out):: vshape(ndim)    ! shape of the variable

      integer(i4), allocatable:: dimIDList(:)
      integer(i4)             :: n, nc ! loop variables
      integer(i4)             :: nerr, ierr

      ! compliance checks
      if ( (present( long_name ) .eqv. .false.) .and. (present( standard_name ) .eqv. .false.) ) then
        print '(a)', 'CF convention compliance error (' // varName // &
        &  '): neither "long_name" nor "standard_name" attribute is specified. '
        stop
      endif

      ! specify dimension IDs
      allocate(dimIDList(ndim))
      dimIDList = -100
      do n = 1, ndim
        do nc = 1, size( dimNameArray )
          if ( trim( dimList(n) ) == trim( dimNameArray(nc) ) ) then
            vshape(n) = dimLenArray(nc)
            dimIDList(n) = dimIDArray(nc)
            exit
          endif
        enddo
        if ( dimIDList(n) == -100 ) then
          print '(a)', 'Error: dimension does not exist. ' // trim( dimList(n) )
          stop
        endif
      enddo

      ! get variable ID
      select case ( varKind )
      case ( 'integer' )
        nerr =  nf90_def_var( ncid, varName, NF90_INT, dimIDList, varID )
      case ( 'float' )
        nerr =  nf90_def_var( ncid, varName, NF90_FLOAT, dimIDList, varID )
      case ( 'character' )
        nerr =  nf90_def_var( ncid, varName, NF90_CHAR, dimIDList, varID )
      case default
        print '(a)', 'Error: (' // varName // ') incorrect kind of variable.'
        stop
      end select
      call nclib__errMssg( nerr, 'NF90_DEF_VAR (' // varName // ')', ierr )
      if ( ierr /= 0 ) stop

      ! attribute "standard_name" and/or "long_name"
      if ( present( standard_name ) ) then
        nerr = nf90_put_att( ncid, varID, 'standard_name', standard_name )
        call nclib__errMssg( nerr, 'NF90_PUT_ATT (' // varName // ':' // 'standard_name)', ierr )
      endif
      if ( present( long_name ) ) then
        nerr = nf90_put_att( ncid, varID, 'long_name', long_name )
        call nclib__errMssg( nerr, 'NF90_PUT_ATT (' // varName // ':' // 'long_name)', ierr )
      endif

      ! attribute "units"
      nerr = nf90_put_att( ncid, varID, 'units', units )
      call nclib__errMssg( nerr, 'NF90_PUT_ATT (' // varName // ':' // 'units)', ierr )

    end subroutine CFnclib__varAttr


    subroutine CF_varAttr_range_float( ncid, varID, FillValue, scale_factor, add_offset )
      integer(i4),        intent(in):: ncid  ! netCDF ID
      integer(i4),        intent(in):: varID ! variable ID
      real(r4),           intent(in):: FillValue
      real(r4), optional, intent(in):: scale_factor
      real(r4), optional, intent(in):: add_offset

      integer(i4):: ierr, nerr

      ! attribute "_FillValue", "scale_factor" and "add_offset"
      nerr = nf90_put_att( ncid, varID, '_FillValue', FillValue )
      call nclib__errMssg( nerr, 'NF90_PUT_ATT:_FillValue)', ierr )
      if ( present( scale_factor ) ) then
        if ( scale_factor /= 0.e0 ) then
          nerr = nf90_put_att( ncid, varID, 'scale_factor', scale_factor )
          call nclib__errMssg( nerr, 'NF90_PUT_ATT:scale_factor)', ierr )
        endif
      endif
      if ( present( add_offset ) ) then
        if ( add_offset /= 0.e0 ) then
          nerr = nf90_put_att( ncid, varID, 'add_offset', add_offset )
          call nclib__errMssg( nerr, 'NF90_PUT_ATT:add_offset)', ierr )
        endif
      endif
    end subroutine CF_varAttr_range_float


    subroutine CF_varAttr_range_int( ncid, varID, FillValue, scale_factor, add_offset )
      integer(i4),           intent(in):: ncid  ! netCDF ID
      integer(i4),           intent(in):: varID ! variable ID
      integer(i4),           intent(in):: FillValue
      integer(i4), optional, intent(in):: scale_factor
      integer(i4), optional, intent(in):: add_offset

      integer(i4):: ierr, nerr

      ! attribute "_FillValue", "scale_factor" and "add_offset"
      nerr = nf90_put_att( ncid, varID, '_FillValue', FillValue )
      call nclib__errMssg( nerr, 'NF90_PUT_ATT:_FillValue)', ierr )
      if ( present( scale_factor ) ) then
        if ( scale_factor /= 0.e0 ) then
          nerr = nf90_put_att( ncid, varID, 'scale_factor', scale_factor )
          call nclib__errMssg( nerr, 'NF90_PUT_ATT:scale_factor)', ierr )
        endif
      endif
      if ( present( add_offset ) ) then
        if ( add_offset /= 0.e0 ) then
          nerr = nf90_put_att( ncid, varID, 'add_offset', add_offset )
          call nclib__errMssg( nerr, 'NF90_PUT_ATT:add_offset)', ierr )
        endif
      endif
    end subroutine CF_varAttr_range_int


    subroutine CF_varPut_float_1D( ncid, varID, varName, variable )
      integer(i4),        intent(in):: ncid    ! netCDF ID
      integer(i4),        intent(in):: varID   ! variable ID
      character(len = *), intent(in):: varName ! name of the variable
      real(r4),           intent(in):: variable(:)

      integer(i4):: ierr, nerr

      nerr = nf90_put_var( ncid, varID, variable )
      call nclib__errMssg( nerr, 'NF90_PUT_VAR (' // varName // ')', ierr )
    end subroutine CF_varPut_float_1D


    subroutine CF_varPut_float_2D( ncid, varID, varName, variable )
      integer(i4),        intent(in):: ncid    ! netCDF ID
      integer(i4),        intent(in):: varID   ! variable ID
      character(len = *), intent(in):: varName ! name of the variable
      real(r4),           intent(in):: variable(:, :)

      integer(i4):: ierr, nerr

      nerr = nf90_put_var( ncid, varID, variable )
      call nclib__errMssg( nerr, 'NF90_PUT_VAR (' // varName // ')', ierr )
    end subroutine CF_varPut_float_2D


    subroutine CF_varPut_float_3D( ncid, varID, varName, variable )
      integer(i4),        intent(in):: ncid    ! netCDF ID
      integer(i4),        intent(in):: varID   ! variable ID
      character(len = *), intent(in):: varName ! name of the variable
      real(r4),           intent(in):: variable(:, :, :)

      integer(i4):: ierr, nerr

      nerr = nf90_put_var( ncid, varID, variable )
      call nclib__errMssg( nerr, 'NF90_PUT_VAR (' // varName // ')', ierr )
    end subroutine CF_varPut_float_3D


    subroutine CF_varPut_float_4D( ncid, varID, varName, variable )
      integer(i4),        intent(in):: ncid    ! netCDF ID
      integer(i4),        intent(in):: varID   ! variable ID
      character(len = *), intent(in):: varName ! name of the variable
      real(r4),           intent(in):: variable(:, :, :, :)

      integer(i4):: ierr, nerr

      nerr = nf90_put_var( ncid, varID, variable )
      call nclib__errMssg( nerr, 'NF90_PUT_VAR (' // varName // ')', ierr )
    end subroutine CF_varPut_float_4D


    subroutine nclib__errMssg( nerr, char_flg, ierr )
      integer(i4),        intent(in) :: nerr     ! NetCDF error handling 
      character(len = *), intent(in) :: char_flg ! indication flag
      integer(i4),        intent(out):: ierr     ! error handling variable

      ierr = 0
      if ( nerr == NF90_NOERR ) return
      ierr = 1
      print '(a)', 'Error ' // char_flg // ': ' // NF90_STRERROR( nerr )
    end subroutine nclib__errMssg


    subroutine nclib__handle_error( nerr, char_flg, ierr )
      integer(i4),        intent(in) :: nerr     ! NetCDF error handling 
      character(len = *), intent(in) :: char_flg ! indication flag
      integer(i4),        intent(out):: ierr     ! error handling variable

      call nclib__errMssg( nerr, char_flg, ierr )
    end subroutine nclib__handle_error

end module nclib
