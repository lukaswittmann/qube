module qube_error

   implicit none

   contains

   !> This subroutine raises an error or warning message and terminates the program.
   ! Parameters:
   !   mode: character, intent(in) :: mode - specifies the type of message to be raised. 
   !          'w' for warning, 'e' for error, and any other character for unknown.
   !   message: character(len=*), intent(in) :: message - the message to be raised.
   ! Returns: None
   subroutine raise(mode,message)
      character,       intent(in) :: mode
      character(len=*),intent(in) :: message
      select case(mode)
         case('w')
            write(*,'(a)') '','Warning: '//message
         case('e')
            write(*,'(a)') '','Error: '//message
         case default
            write(*,'(a)') '','Unknown: '//message
      end select
      call terminate(1)
   end subroutine raise

   !> This subroutine terminates the program based on the input signal.
   ! If the signal is 0, the program stops normally.
   ! If the signal is -1, the program stops with an error message indicating external termination.
   ! If the signal is any other integer, the program stops with an error message.
   ! @param signal The integer signal to determine how the program should terminate.
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

   !> Handles the SIGINT signal
      subroutine wsigint
         write(*,'(a)') '','Recieved sigint, terminating...'
         call terminate(-1)
      end subroutine wsigint

   !> Handles the SIGTERM signal
   subroutine wsigterm
      write(*,'(a)') '','Recieved sigterm, terminating...'
      call terminate(-1)
   end subroutine wsigterm

end module qube_error