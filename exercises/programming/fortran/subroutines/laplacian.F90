module laplacian_mod
  implicit none
  real, parameter :: dx = 0.01, dy = 0.01
  
contains
  
  subroutine initialize(field0)
    
    real, intent(inout) :: field0(:,:)
    integer :: s(2)

    s=shape(field0)
	
    field0 = 65.0

    field0(:,1) = 20.0
    field0(:,s(2)) = 70.0
    field0(1,:) = 85.0
    field0(s(1),:) = 5.0
  
  end subroutine initialize
	
  subroutine laplacian(curr, prev)

	real, intent(in) :: prev(:,:)
	real, intent(out) :: curr(:,:)
	integer :: i, j, s(2)
	
	s=shape(prev)

	curr = 0.0

	do i = 2, s(1)-1
		do j = 2, s(2)-1
			curr(i,j) = (-2*prev(i,j) + prev(i-1,j) + prev(i+1,j))/dx**2  +&
			& (- 2*prev(i,j) + prev(i,j-1) + prev(i,j+1))/dy**2
		end do
	end do
  
  end subroutine laplacian

  subroutine write_field(array)
	
	real, intent(in) :: array(:,:)
	integer :: i, s(2)

	s=shape(array)

	do i = 1, s(1)
		write(*,*) array(i,:)
	end do
  end subroutine write_field
	
end module laplacian_mod
