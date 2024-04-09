module MatrixTools
   use Environment
   use Forsythe

   implicit none

contains

   ! сумма элементов матрицы
   function GRAND_SUM(M, N, matrix) result (sum)

      integer(I_), intent(in)    :: M, N
      real(R_), intent(in)       :: matrix(M,N)
      real(R_)                   :: sum
      integer                    :: i, j

      do i=1, M
         do j=1,N
            sum = sum + matrix(i,j)
         end do
      end do

   end function GRAND_SUM


   ! определить координаты максимального элемента матрицы
   function find_max(matrix, M, N ) result (matEl)

      integer(I_), intent(in)    :: M, N
      real(R_), intent(in)       :: matrix(M,N)
      integer(I_)                :: matEl(2)
      real(R_)                   :: max
      integer                    :: i, j

      max=matrix(1,1)
      do i=1, M
         do j=1,N
            if (matrix(i,j) > max) then 
               max = matrix(i,j)
               matEl(1)=i
               matEl(2)=j
            end if
         end do
      end do

   end function find_max

   ! определить координаты минимального элемента матрицы
   function find_min(matrix, M, N) result (matEl)

      integer(I_), intent(in)    :: M, N
      real(R_), intent(in)       :: matrix(M,N)
      integer(I_)                :: matEl(2)
      real(R_)                   :: min
      integer                    :: i, j

      min=matrix(1,1)
      do i=1, M
         do j=1,N
            if (matrix(i,j) < min) then 
               min = matrix(i,j)
               matEl(1)=i
               matEl(2)=j
            end if
         end do
      end do

   end function find_min

   function INVERT(M, N, A) result (A_INV)

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

   end function INVERT

   subroutine ANORM(NORM, M, N, A)
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

   function RESIDUAL(M, N, A) result (R)
      integer(I_), intent(in)    :: M, N
      real(R_), intent(in)       :: A(M,N)
      real(R_)                   :: R(M,N), A_COPY(M,N), A_INV(M,N)

      A_COPY = A
      A_INV = INVERT(M,N,A_COPY)

      R = SUBSTRACT(M, N, IDENT(M,N), MATMUL(A_INV, A_COPY))

   end function RESIDUAL

   function SUBSTRACT(M, N, A1, A2) result (SUB)
      integer(I_), intent(in)    :: M, N
      real(R_), intent(in)       :: A1(M,N), A2(M,N)
      real(R_)                   :: SUB(M,N)

      SUB(:,:) = A1(:,:) - A2(:,:)

   end function SUBSTRACT

   function IDENT(M,N) result (E)
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

   function MULTIPLY(M, N, A1, A2) result (MUL)
      integer(I_), intent(in)    :: M, N
      real(R_), intent(in)       :: A1(M,N), A2(M,N)
      integer(I_)                :: i, j
      real(R_)                   :: R(M,N), MUL(M,N)

      do i=1, M
         do j=1,N
            MUL(i,j) = SUM(A1(i,:) * A2(:,j));
         end do
      end do

   end function MULTIPLY

   function copy_matrix(A) result (A_COPY)
      
      real(R_), intent(in)       :: A
      real(R_), allocatable      :: A_COPY(:,:)

      A_COPY = A

   end function copy_matrix


end module MatrixTools 
