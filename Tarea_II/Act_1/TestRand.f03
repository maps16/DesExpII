program test_random_number

  implicit none
  integer, allocatable :: seed(:),n(:)
  integer :: z
  Real, allocatable :: r(:)
  
  call random_seed(size=z)
  allocate(seed(z))
  allocate(n(z+1))
  allocate(r(z+1))
  call random_seed(put=n)
  call random_seed(get=seed)
  !Write(*,*) seed
  CALL RANDOM_NUMBER(r)
  !Write(*,*) "================================================================"
  Write(*,*) r
end program
