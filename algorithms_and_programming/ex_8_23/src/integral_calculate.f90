module Integral_calculate
  use Environment

  implicit none
  integer, parameter      :: N = 10
  real(R_), parameter     :: a = 0, b = 1, h = (b - a) / N

contains

pure subroutine Integral(p1, dp, q1, dq, P, Q, X, I)
    real(R_), intent(in)      :: p1, dp, q1, dq
    real(R_), intent(out)     :: I(:, :), P(:), Q(:), X(:)
    integer                   :: k, l

    X = [(a  + k*h,   k = 1, Size(X))]
    P = [(p1 + k*dp,  k = 0, Size(P) - 1)]
    Q = [(q1 + k*dq,  k = 0, Size(Q) - 1)]

    ! do l = 1, Size(Q)
    !    do k = 1, Size(P)
    !       I(k, l) = F_x(P(k), Q(l), X)
    !    end do
    ! end do

    do concurrent (l = 1:Size(Q), k = 1:Size(P))
          I(k, l) = F_x(P(k), Q(l), X)
    end do

    I = I * h
  end subroutine Integral

  pure function F_x(p, q, X)
    real(R_)                   p, q, X(:)
    real(R_)                   F_x
    real(R_), allocatable      :: F_x_temp(:)
    intent(in)   p, q, X
    
    allocate(F_x_temp(Size(X)))

    F_x_temp = Sin(p * X)**2 / SQRT(X**2 + q)
    F_x_temp(1) = X(1) / 2
    F_x_temp(N) = X(N) / 2
    
    F_x = Sum(F_x_temp)

  end function F_x

end module Integral_calculate


!    real(R_)                  :: temp
    ! temp = F_x(2.8, 5.0, X)
    ! print "(f6.2)", temp*h
    ! I = [(F_x(P(k), Q(l), X),   k = 1, Size(P))]


