program escala_vetor
  implicit none
  integer, parameter :: N = 100000000
  real(8) :: x(N), y(N), alpha
  integer :: i
  real(8) :: t1, t2

  alpha = 2.5
  do i = 1, N
     x(i) = i * 1.0d0
  end do

  call cpu_time(t1)
  do i = 1, N
     y(i) = alpha * x(i)
  end do
  call cpu_time(t2)

  print *, "Tempo (s):", t2 - t1
end program escala_vetor
