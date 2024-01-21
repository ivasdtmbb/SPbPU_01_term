module Integral_calculate
  use Environment

  implicit none
  integer, parameter      :: N = 10
  real(R_), parameter     :: a = 0, b = 1, h = (b - a) / N

contains

  pure subroutine Integral(p1, dp, q1, dq, P, Q, X, I)
    real(R_), intent(in)      :: p1, dp, q1, dq
    real(R_), intent(out)     :: I(:), P(:), Q(:), X(:)
    integer                   :: k, t

    X = [(a  + k*h,   k = 0, Size(X) - 1)]
    P = [(p1 + k*dp,  k = 0, Size(P) - 1)]
    Q = [(q1 + k*dq,  k = 0, Size(Q) - 1)]

    I = [(F_x(P(k), Q),   k = 1, Size(P))]
    I = I * h

  end subroutine Integral

  pure function F_x(p, q)
    real(R_)                :: p, q
    real(R_), allocatable   :: X(:)
    intent(in)   p, q
    
    X = [(a  + k*h,   k = 0, Size(X) - 1)]

    X = Sin(p * X)**2 / SQRT(X**2 + q)
    X(1) = X(1) / 2
    X(N) = X(N) / 2

    F_x = Sum(X)

  end function F_x
