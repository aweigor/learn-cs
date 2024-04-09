program var_17

! Исследование связи числа обусловленности с нормой матрицы невязки
! Исользованы DECOMP, SOLVE библиотеки Forsythe
   
   use Environment
   use Forsythe
   
   implicit none

   
   character(*), parameter  :: output_file = "output.txt", fmt = "f6.1"
   integer(I_)              :: In = 0, Out = 0
   integer(I_), allocatable :: IPVT(:)
   real(R_), allocatable    :: A(:,:), A_COPY(:,:), A_INV(:,:), A_DECOMP(:,:), WORK(:)
   real(R_)                 :: P = 1.0, RN ! норма матрицы невязки
   integer                  :: M = 0, N = 0, i
   real(R_)                 :: COND,CONDP1

   open (file="./data/input.txt", newunit=In)
      read (In, '(2i2)') M, N
      allocate(A(M,N))
      allocate(A_COPY(M,N))
      allocate(A_INV(M,N))
      allocate(IPVT(N))
      allocate(A_DECOMP(M,N))
      allocate(WORK(N))
      call readMatrix("./data/matrix.txt",M,N,fmt,A)
   close (In)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, *) 'Расчетная матрица:'
      write (Out, '('//N//fmt//')') (A(i, :), i = 1, M)
   close (Out)

   do i=0, 6

      P = 1.0 / 10**i
      open (file=output_file, encoding=E_, newunit=Out, position="append", action="write")
         write (Out, '(/, a, f8.6)') 'P = ', P
      close (Out)

      A_COPY = A
      A_COPY(1,1) = A_COPY(1,1) + P
      A_DECOMP = A_COPY

      ! Разложение, получение числа обусловности
      call DECOMP(M,N,A_DECOMP,COND,IPVT,WORK)

      CONDP1=COND+1.0
      ! Матрица вырождена?
      if (CONDP1.EQ.COND) then
         ! Выведем сообщение, остановим выполнение
         open (file=output_file, encoding=E_, newunit=Out, position="append", action="write")
            write (Out, *) 'Матрица классифицируется как вырожденная'
         close (Out)
         stop
      end if

      ! Получим норму невязки, печатаем итог, и конец
      call ANORM(RN, M, N, RESIDUAL(M, N, A_COPY))

      open (file=output_file, encoding=E_, newunit=Out, position="append", action="write")
         write (Out, '(a, E12.5)') 'COND = ', COND
         write (Out, '(a, E12.5)') 'RN =', RN
         write (Out, '(a, E12.5)') 'COND / RN =', COND / RN
      close (Out)

   end do
   
   contains

      subroutine PRINT_MATRIX(M, N, A, msg, fmt)
         character(*), parameter    :: output_file = "output.txt"
         character(*), intent(in)   :: msg, fmt
         integer(I_), intent(in)    :: M, N
         real(R_), intent(in)       :: A(M,N)
         integer(I_)                :: i

         open (file=output_file, encoding=E_, newunit=Out, position="append", action="write")
            write (Out, *) msg
            write (Out, '('//N//fmt//')') (A(i, :), i = 1, M)
         close (Out)
      end

      
      subroutine ANORM(NORM, M, N, A)
      ! Первая норма от A

         integer(I_), intent(in)    :: M, N
         real(R_), intent(in)       :: A(M,N)
         integer(I_)                :: i, j
         real(R_)                   :: NORM, T

         NORM = 0.0
         do j=1, M
            T = 0.0
            do i = 1, N
               T = T + ABS(A(i,j))
               if (T.gt.NORM) then
                  NORM = T
               end if
            end do
         end do
      end

      
      function INVERT(M, N, A) result (A_INV)
      ! Ообратная матрица к А
      
         integer(I_), intent(in)    :: M, N
         real(R_), intent(in)       :: A(M,N)
         integer(I_)                :: IPVT(M), j
         real(R_)                   :: A_INV(M,N), A_COPY(M,N), X(M), WORK(M), COND, CONDP1
   
         do j=1, M
   
            A_COPY = A
   
            X(:j) = 0.0
            X(j:) = 0.0
            X(j) = 1.0
            
            call DECOMP(M,N,A_COPY,COND,IPVT,WORK)
   
            CONDP1=COND+1.0
            if (CONDP1.EQ.COND) then
               A_INV(:,:) = 0.0
               exit
            end if
   
            call SOLVE(M,N,A_COPY,X,IPVT)
   
            A_INV(:,j) = X(:)
   
         end do

         call PRINT_MATRIX(M, N, A_INV, "Обратная матрица: ", "(E12.4)")
   
      end function INVERT

      
      function RESIDUAL(M, N, A) result (R)
      ! Матрица невязки А

         integer(I_), intent(in)    :: M, N
         real(R_), intent(in)       :: A(M,N)
         real(R_)                   :: R(M,N), A_COPY(M,N), A_INV(M,N)
   
         A_COPY = A
         A_INV = INVERT(M,N,A_COPY)
   
         R = SUBSTRACT(M, N, IDENT(M,N), MATMUL(A_INV, A_COPY))

         call PRINT_MATRIX(M, N, R, "Матрица невязки: ", "f12.8")
   
      end function RESIDUAL
   
      function SUBSTRACT(M, N, A1, A2) result (SUB)
         integer(I_), intent(in)    :: M, N
         real(R_), intent(in)       :: A1(M,N), A2(M,N)
         real(R_)                   :: SUB(M,N)
   
         SUB(:,:) = A1(:,:) - A2(:,:)
   
      end function SUBSTRACT
      
      
      function IDENT(M,N) result (E)
      ! Получение единичной матрицы размерности M * N

         integer(I_), intent(in)    :: M, N
         integer(I_)                :: i, j
         real(R_)                   :: E(M,N)
   
         do i=1, M
            do j=1,N
               if (i.eq.j) then
                  E(i,j) = 1.0
               else
                  E(i,j) = 0.0
               end if
            end do
         end do
      end function IDENT
      
      ! A1 * A2
      function MULTIPLY(M, N, A1, A2) result (MUL)
         integer(I_), intent(in)    :: M, N
         real(R_), intent(in)       :: A1(M,N), A2(M,N)
         integer(I_)                :: i, j
         real(R_)                   :: MUL(M,N)
   
         do i=1, M
            do j=1,N
               MUL(i,j) = SUM(A1(i,:) * A2(:,j));
            end do
         end do
   
      end function MULTIPLY
      
end program var_17