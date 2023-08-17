module qube_error

   implicit none

   contains

   subroutine raise(mode,message)
      character,       intent(in) :: mode
      character(len=*),intent(in) :: message
      select case(mode)
         case('w')
            print'(''#warning!'',x,a)',message
         case('e')
            print'(''#error!'',x,a)',  message
         case default
            print'(''#unknown mode!'',x,a)', message
      end select
      call terminate(1)
   end subroutine raise

   subroutine terminate(signal)
      integer,intent(in) :: signal
      select case(signal)
         case(0)
            stop
         case(-1)
            error stop 'External termination of qube..'
         case default
            error stop
      end select
   end subroutine terminate


   subroutine wsigint
      print'(''Recieved sigint, terminating...'')'
      call terminate(-1)
   end subroutine wsigint


   subroutine wsigterm
      print'(''Recieved sigterm, terminating...'')'
      call terminate(-1)
   end subroutine wsigterm

end module qube_error