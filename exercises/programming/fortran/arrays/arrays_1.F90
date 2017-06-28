program arrays
  implicit none
  integer :: nx=5, ny=5

  real, allocatable :: A(:,:), B(:,:)
  real :: delta = 0.01
  
  !write (*,*) 'Give number of rows and columns for matrix A:'
  !read (*,*) nx, ny  

  call make_A(A,nx,ny)
  call laplacian(A,B,delta)
  call operations(A)
  call print(A,B)

contains
  subroutine make_a(A,nx,ny)
    implicit none
    real, allocatable, intent(out) :: A(:,:)
    integer, intent(in):: nx, ny

    allocate(A(nx,ny))
    A = 65.0

    A(:,1) = 20.0
    A(:,nx) = 70.0
    A(1,:) = 85.0
    A(ny,:) = 5.0
  end subroutine make_a

subroutine laplacian(A,B,delta)
  implicit none
  real, intent(in) :: A(:,:), delta
  real, allocatable, intent(out) :: B(:,:)
  integer :: i, j
  B = A

  do i = 2, ny-1
     do j = 2, nx-1
     	B(i,j) = -4*A(i,j) + A(i-1,j) + A(i+1,j) + A(i,j-1) + A(i,j+1)
     end do
  end do
  
  B = B/(delta**2)
end subroutine laplacian

subroutine operations(A)
  implicit none
  real, intent(in) :: A(:,:)
  real, allocatable :: C(:)
  real :: D(2), E
  logical :: F
  integer :: G, A1(2)

  A1 = shape(A)
  allocate(C(A1(1)))

  C = sum(A,2)
  D = maxloc(A)
  E = minval(A)
  F = all(A>=0)
  G = count(A>=0.5)

  write (*,*) C
  write (*,*) D
  write (*,*) E
  write (*,*) F
  write (*,*) G

end subroutine operations

subroutine print(A,B)
  implicit none
  real, intent(in) :: A(:,:), B(:,:)
  integer :: i

  write (*,*) 'A'
  do i = 1, nx
    write(*,*) A(i,:)
  end do

  write (*,*) 'B'
  do i = 1, nx
    write (*,*) B(i,:)
  end do
end subroutine print
  
end program arrays