module Context
	use Environment
	integer(I_), parameter	:: N = 9
  real(R_)	:: X(N), Y(N), B(N), C(N), D(N) ! X , Y - гранцицы разбиения функции, B, C, D - коэффициенты сплайна
end module Context