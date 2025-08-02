! arquivo: sum_timing_fortran.f90
program benchmark_sum_fortran
    implicit none

    integer, parameter :: valores_n(4) = [10**5,10**6, 10**7, 10**8]
    integer :: i, k
    integer :: n
    real(8) :: alpha = 2.5
    real(8), allocatable :: x(:)
    real(8) :: s
    real(8) :: t_inicio, t_fim

    print *, "--- Medindo Tempos (Soma de Produtos) em Fortran ---"
    print *, "----------------------------------------------------"

    do k = 1, size(valores_n)
        n = valores_n(k)
        allocate(x(n))
        s = 0.0d0  ! Inicializa a soma com precisão dupla

        ! Inicializa o vetor
        do i = 1, n
            x(i) = real(i, 8)
        end do

        ! Mede o tempo do cálculo
        call cpu_time(t_inicio)

        ! Loop principal para o cálculo da soma dos produtos
        do i = 1, n
            s = s + alpha * x(i)
        end do

        call cpu_time(t_fim)
        
        write(*, '(A, I0, A, F10.6, A)') "Para N = 1e", int(log10(real(n))), ": ", t_fim - t_inicio, " segundos"

        deallocate(x)
    end do

end program benchmark_sum_fortran