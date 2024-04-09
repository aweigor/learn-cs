
program var_17
   
   	use Environment
   	use Forsythe
		use Context

   	character(*), parameter  	:: output_file = "output.txt", fmt = "f9.6"
   	integer(I_)              	:: In = 0, Out = 0, NOFUN, I
   	real(R_)    							:: U, S ! U - текущий шаг разбиеяния, S - значение сплайна
		real(R_)    							:: L = 0.0, R = 1.6, RELERR = 0.1, ABSERR = 0.1, ERREST, FLAG, RESULT
		real(R_)									:: POINTS(500) ! Точки для графиков

		! Расчитаем значения функции в узловых точках
		do I = 1, N
			X(I) = 0.2 * (I - 1)
			Y(I) = FUN(X(I))
		end do

   	open (file="output.txt", encoding=E_, newunit=Out)
      write (Out, *) 'Граничные условия:'
      write (Out, '('//N//fmt//')') X(:)
			write (Out, '('//N//fmt//')') Y(:)
   	close (Out)

		! Вычисляем коэффициенты сплайна - векторы B,C,D
	 	call SPLINE(N,X,Y,B,C,D)

		! Сгенерируем точки Сплйна, Полинома, исходной функции для графика
		open (file="points_F.csv", encoding=E_, newunit=Out)
			write (Out, *) ',x,y'
      do I = 1, 500
				write (Out, '(i4, a, f5.2, a, f8.5)') I, ',', 1.6 / 500 * I, ',', FUN(1.6 / 500 * I)
			end do
   	close (Out)

		open (file="points_S.csv", encoding=E_, newunit=Out)
			write (Out, *) ',x,y'
      do I = 1, 500
				write (Out, '(i4, a, f5.2, a, f8.5)') I, ',', 1.6 / 500 * I, ',', SEVAL_EVAL(1.6 / 500 * I)
			end do
   	close (Out)

		open (file="points_L.csv", encoding=E_, newunit=Out)
			write (Out, *) ',x,y'
      do I = 1, 500
				write (Out, '(i4, a, f5.2, a, f8.5)') I, ',', 1.6 / 500 * I, ',', POLY(1.6 / 500 * I)
			end do
   	close (Out)



	 	open (file="output.txt", encoding=E_, newunit=Out, position="append", action="write")
			write (Out, *) 'Коэффициенты Сплайнa:'
      write (Out, '('//N//fmt//')') B(:)
			write (Out, '('//N//fmt//')') C(:)
			write (Out, '('//N//fmt//')') D(:)
    close (Out)

		! Вычисляем интеграл исходной функции
		call QUANC8(fun,L,R,ABSERR,RELERR,RESULT,ERREST,NOFUN,FLAG)

		open (file="output.txt", encoding=E_, newunit=Out, position="append", action="write")
			write (Out, *) 'Интеграл f(x)dx на интервале от 0 до 1.6 :'
			write (Out, *) 'ERREST :'
			write (Out, *) ERREST
			write (Out, *) 'FLAG :'
			write (Out, *) FLAG
			write (Out, *) 'RESULT :'
			write (Out, *) RESULT
    close (Out)

		! Вычисляем интеграл сплайна исходной функции
		call QUANC8(SEVAL_EVAL,L,R,ABSERR,RELERR,RESULT,ERREST,NOFUN,FLAG)

		open (file="output.txt", encoding=E_, newunit=Out, position="append", action="write")
			write (Out, *) 'Интеграл S(x)dx на интервале от 0 до 1.6 :'
			write (Out, *) 'ERREST :'
			write (Out, *) ERREST
			write (Out, *) 'FLAG :'
			write (Out, *) FLAG
			write (Out, *) 'RESULT :'
			write (Out, *) RESULT
    close (Out)

		! Вычисляем интеграл полинома Лагранжа исходной функции
		call QUANC8(POLY,L,R,ABSERR,RELERR,RESULT,ERREST,NOFUN,FLAG)
		
		open (file="output.txt", encoding=E_, newunit=Out, position="append", action="write")
			write (Out, *) 'Интеграл L(x)dx на интервале от 0 до 1.6 :'
			write (Out, *) 'ERREST :'
			write (Out, *) ERREST
			write (Out, *) 'FLAG :'
			write (Out, *) FLAG
			write (Out, *) 'RESULT :'
			write (Out, *) RESULT
    close (Out)

		contains
			real function FUN(X)
				real(R_)				:: X

				if (X .EQ. -1.0) FUN = 1
				if (X .NE. -1.0) FUN = cos(X) / (1 + X)

				return
			end function FUN

			! Вычисляет значение Полинома Лагранжа в расчетной точке
			! V - расчетная точка
			! N - степень
			! X, Y - массивы значений
			subroutine POLY_SUBROUTINE(N, X, Y, V, RES)
				integer(I_), intent(in)		:: N
				real(R_), intent(in)      :: X(8), Y(8), V
				real(R_), intent(out)     :: RES
				integer(I_)               :: In = 0, Out = 0, I, J
				real(R_)									:: W

				do I = 1, N
					W = 1.0
					do J = 1, N
						if (I .NE. J) then
							W = W * (V - X(J)) / (X(I) - X(J))
						end if
					end do
					RES = RES + Y(I) * W
				end do
			end subroutine POLY_SUBROUTINE


			! Вычисляет значение Полинома Лагранжа в расчетной точке
			! V - расчетная точка
			real function POLY(V)
				
				! N - степень
				! X, Y - массивы значений
				! Взяты из Context

				use Context
				use Environment

				real(R_), intent(in)      :: V
				integer(I_)               :: I, J
				real(R_)									:: W

				POLY = 0.0

				do I = 1, N
					W = 1.0
					do J = 1, N
						if (I .NE. J) then
							W = W * (V - X(J)) / (X(I) - X(J))
						end if
					end do
					POLY = POLY + Y(I) * W
				end do

				return

			end function POLY

			! Вычисляет значение сплайна в заданной точке V
			real function SEVAL_EVAL(V)

				! N, X, Y, B, C, D
				use Context
				use Forsythe
				use Environment 

				real(R_), intent(in)      :: V

				SEVAL_EVAL = SEVAL(N,V,X,Y,B,C,D)

				return

			end function SEVAL_EVAL
   
   
end program var_17