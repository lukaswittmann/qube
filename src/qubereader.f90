module qubeReader
   use mctc_env, only : wp
   implicit none

contains

   !* Reads the energy from a huge output file by searching for a keyword
   subroutine getOutputFileEnergy(energy,lineString,separatorString,filename)
      implicit none

      real(wp) :: energy
      integer :: iret, iostatus
      character(*) :: filename, lineString, separatorString
      character(80) :: lineStr

      ! Open the file
      open(unit=1, file=filename, iostat=iret, status='old')
      ! If the file doesn't exist, return
      if(iret.ne.0) return
      do
         ! If we've reached the end of the file, return
         if (iostatus /= 0) exit
         ! Read a line
         read(1,'(a)',  iostat=iostatus) lineStr
         ! If the line contains the keyword, extract the number
         if(index(lineStr, lineString) .ne. 0) then
            call extractNumber(lineStr, separatorString, energy)
            exit
         endif
      enddo
      close(1)
      
   end subroutine getOutputFileEnergy

   !* Reads the energy from a simple output file by searching for a keyword
   subroutine getSimpleEnergy(energy,filename)
      implicit none

      real(wp) :: energy
      integer :: iret, iostatus
      character(*) :: filename
      character(80) :: lineStr

      ! Open the file
      open(unit=1, file=filename, iostat=iret, status='old')
      ! If the file doesn't exist, return
      if(iret.ne.0) return
      ! Read a line
      read(1,'(a)',  iostat=iostatus) lineStr
      call extractNumber(lineStr, '', energy)
      close(1)
      
   end subroutine getSimpleEnergy

   !* Extracts a number from a string with optional keyword
   subroutine extractNumber(str, keyword, number)
      implicit none

      character(len=*), intent(in) :: str, keyword
      real(wp), intent(out) :: number
      integer :: strStart

      strStart = scan(str, keyword, back=.true.)
      read (str(1+strStart:),*) number

   end subroutine extractNumber


end module qubeReader