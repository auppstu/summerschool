module laplacian_mod
  implicit none
  real, parameter :: dx = 0.01, dy = 0.01
  
contains
  
  subroutine initialize(field0)
	implicit none
    real, intent(inout) :: field0(:,:)
	integer :: nx=5, ny=5
	
    field0 = 65.0

    field0(:,1) = 20.0
    field0(:,ny) = 70.0
    field0(1,:) = 85.0
    field0(nx,:) = 5.0
  
  end subroutine initialize
	
  subroutine laplacian(curr, prev)
	implicit none
	real, intent(in) :: prev(:,:)
	real, intent(out) :: curr(:,:)
	integer :: i, j
	
	curr = prev

	do i = 2, nx-1
		do j = 2, ny-1
			curr(i,j) = (-2*prev(i,j) + prev(i-1,j) + prev(i+1,j))/dx**2  +&
			& (- 2*prev(i,j) + prev(i,j-1) + prev(i,j+1))/dy**2
		end do
	end do
  
  end subroutine laplacian

  subroutine write_field(array)
	implicit none
	real, intent(in) :: A(:,:)
	integer :: i

	do i = 1, nx
		write(*,*) A(i,:)
	end do
  end subroutine write_field(A)
	
end module laplacian_mod
