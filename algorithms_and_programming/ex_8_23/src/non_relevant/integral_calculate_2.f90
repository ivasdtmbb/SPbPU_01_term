module Integral_calculate 
   use Environment

   implicit none
   integer, parameter      :: N = 10
   real(R_), parameter     :: a = 0, b = 1, h = (b - a) / N

contains

  pure subroutine Integral(p1, p2, dp, q1, q2, dq, X, P, Q)
    real(R_), intent(in)  :: p1, p2, dp, q1, q2, dq
    real(R_), intent(out) :: X(:), P(:), Q(:)
    integer               :: i

    X = [(a  + i*h,  i = 0, Size(X) - 1)]
    P = [(p1 + i*dp, i = 0, Size(P) - 1)]
    Q = [(q1 + i*dq, i = 0, Size(Q) - 1)]

    Integral = [(Sum(F_x(P(i), Q, X), i = 1, Size(P)))]
    Integral = Integral * h

  end subroutine Integral

  ! Чистая вектор-функция от скаляра p и массива X в регулярном стиле.
  pure function F_x(p, q, X)
    real(R_)    p, q, X(:)
    real(R_)    F_x(UBound(X, 1))
    intent(in)  p, q, X

    F_x = Sin(p * X)**2 / SQRT(X**2 + q)
    F_x(1) = F_x(1) / 2
    F_x(N) = F_x(N) / 2
  end function F_x

  ! real pure function F_x(p, q, X)
  !   integer                 :: N = Size(X)
  !   real(R_), allocatable   :: F(:)
  !   real(R_), intent(in)       p, q, X(:)

  !   allocate F(N)

  !   F = Sin(p * X)**2 / SQRT(X**2 + q)
  !   F(1) = F(1) / 2
  !   F(N) = F(N) / 2
  ! end function F_x

end module Integral_calculate 
