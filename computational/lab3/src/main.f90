program lab_3

   use Environment
   use Forsythe

   implicit none
   integer, parameter            :: N= 2
   character(*), parameter       :: output_file = "output.txt"
   integer                       :: Out = 0, steps, flag = 1, iwork(5), i, j
   real(R_)                      :: abserr = 0.0001, relerr = 0.0001, h_print = 0.0075
   real(R_)                      :: low = 0.0, high = 0.15, work(3 + 6 * N)
   real(R_)                      :: x1 = 3.0, x2 = -1.0, t_out = 0, h(N) = [0.0075, 0.0075], x_pair(N)

   open (file=output_file, encoding=E_, newunit=Out) 
      write (Out, "(a)") "RKF45 результаты: "
      write (Out, "(a)") " t | x(1) | x(2) |" 
      write (Out, "(a)") "----------------------------"
   close (Out)
   
   x_pair = [x1, x2]
   steps = nint((high - low) / h_print) + 1 

   open (file="test.txt", encoding=E_, newunit=Out, position="append") 
         write (Out, *) "TEST"
         write (Out, *) work(1)
          write (Out, *) "TEST"
         write (Out, *) work(2)
      close (Out)
   
   do i=1,steps
      call RKF45(f, N, x_pair, low, t_out, relerr, abserr, flag, work, iwork)
      open (file=output_file, encoding=E_, newunit=Out, position="append") 
         write (Out, "(f5.2, '|', 2(f10.7, '|'))") t_out, x_pair(1), x_pair(2)
     close (Out)
     t_out = t_out + h_print 
   end do
   do i=1,ubound(h, 1)
      open (file = output_file, encoding=E_, newunit=Out, position="append")
         write (Out, "(a, f5.3)") "Рунге-Кутты 2-й степени точности с шагом ", h(i) 
         write (Out, "(a)") " t | x(1) | x(2) |"
         write (Out, "(a)") "--------------------------------------"
      close (Out)
      
      low = 0
      t_out = low
      do j=1,steps
         x_pair = [x1, x2]
         call rk3(low, t_out, x_pair, h(i))

         open (file=output_file, encoding=E_, newunit=Out, position="append") 
            write (Out, "(f5.2, '|', 2(es15.7, '|'))") t_out, x_pair(1), x_pair(2)
         close (Out)
         
         t_out = t_out + h_print 
      end do
   end do
contains
   subroutine f(t, x, dxdy)
      real(R_), intent(in) :: t, x(2) 
      real(R_) :: dxdy(2)

       open (file="test.txt", encoding=E_, newunit=Out, position="append") 
         write (Out, *) "x_pair"
         write (Out, *) dxdy(1)
          write (Out, *) "x_pair"
         write (Out, *) dxdy(2)
      close (Out)
      
      dxdy(1) = -430.0 * x(1) - 12000.0 * x(2) + exp(-10 * t)
      dxdy(2) = x(1) + log(1.0 + 100 * t**2) 
   end subroutine f
   
   subroutine rk3(t, t_out, zn, h)
      real(R_)          :: tn, t, t_out, h, zn(:), k1(N), k2(N)
      real(R_)          :: fun(N), zn13(N), tn13
      integer           :: steps, i
      intent(in)        :: t, t_out, h 
      intent(out)       :: zn
      
      steps = nint((t_out - t) / h) 
      tn = t
      
      do i=1,steps
         call f(tn, zn, fun)
         k1 = h / 2 * fun
         ! z (n + 1/3)
         zn13 = zn + (h / 3) * fun;
         ! t (n + 1 / 3)
         tn13 = tn + (1/3 * h)
         call f(tn13, zn13, fun)
         k2 = h / 2 * fun
         ! z(n + 1)
         zn = zn + (-1 * k1 + 3 * k2)
         tn = tn + h
      end do
   end subroutine rk3
end program lab_3