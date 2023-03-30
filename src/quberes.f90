
!* Module for reading the .res files and storing the data in a structure
module qubeRes
   use mctc_env, only : wp
	use stringifor_string_t
   implicit none

	!* Derived type for benchmark structure
	type benchmarkType
		! Name of the benchmark and the .res file
		character(len=:), allocatable :: name, resFilename

		! Path of the benchmark
		character(len=:), allocatable :: path

		! Number of folders and interactions of the benchmark
		integer :: numFolders, numInteractions

		! Folder and subfolder names
		character(len=:), allocatable :: subFolderName
		character(len=:), dimension(:,:), allocatable :: folderName

		! Matrix for the stoichiometry of each interaction
		integer, dimension(:,:), allocatable :: stoich

		! Matrix for the raw energy of each strucutre and the resulting relative energy
		real(wp), dimension(:,:), allocatable :: rawEnergy
		real(wp), dimension(:), allocatable :: relEnergy
		real(wp), dimension(:), allocatable :: refEnergy

		
		! Timing of the evaluation
		real(wp) :: timing

	end type benchmarkType


contains

   !* Reads the energy from a huge output file by searching for a keyword
   subroutine readBenchmarkResfile(benchmark)
      implicit none

      type(benchmarkType), intent(inout) :: benchmark
		character(len=200) :: ReadInLineStr
		integer :: iret, iostatus 
		character(len=100) :: dirStr, stoichStr, refEnergyStr
		character(len=:), dimension(:), allocatable :: workStr
		integer :: dim, line, i
		character(len=:), dimension(:), allocatable :: dirList, stoichList
		real(wp) :: refEnergy

		type(string) ::  lineStr
		type(string), allocatable :: substrings(:)

      ! Open the file
      open(unit=2, file=benchmark%path//benchmark%resFilename, iostat=iret, status='old')
      ! If the file doesn't exist, return
      if(iret.ne.0) return
      do line = 1, 10000000

      	! Read a line
         read(2,'(a)',  iostat=iostatus) ReadInLineStr

			! Convert to string
			lineStr = ReadInLineStr

			call lineStr%split(sep=' ', tokens=substrings )

			write(*,*) "======"
		   do i=1,size(substrings, dim=1)
   			write(*,*) substrings(i)
   		enddo

			dim = size(substrings, dim=1)

			write(*,*) dim
			
			! If we've reached the end of the file, return
      	if (iostatus /= 0) exit
      enddo
      close(2)


      
   end subroutine readBenchmarkResfile


	!* Replaces multiple spaces or tabs with a single space, creates new file
	subroutine makeSpaceDelimitedRes(benchmark)

		implicit none

		type(benchmarkType), intent(in) :: benchmark
		integer :: iret, iostatus 
		character(len=200) :: lineStr

		! Rename the original file
		call system("mv " // benchmark%path//benchmark%resFilename // " " &
			// benchmark%path//benchmark%resFilename // "-old")

		! Open the file
      open(unit=3, file=benchmark%path//benchmark%resFilename//"-old", &
			iostat=iret, status='old')
   
		! If the file doesn't exist, return
      if(iret.ne.0) return
      do
      	! Read a line
         read(3,'(a)',  iostat=iostatus) lineStr
      	! If the line contains the $tmer keywprd, extract the data
         if(index(lineStr, '$tmer') .ne. 0) then
				! Dont tell the FORTRAN gods ;)
				call system("echo " // lineStr // " | tr -s '[:blank:]' ' ' >> " // &
					benchmark%path//benchmark%resFilename)
         endif
			! If we've reached the end of the file, return
      	if (iostatus /= 0) exit
      enddo
      close(3)

		write (*,*) 'File converted!'
		write (*,*) 'File written to: ' // benchmark%path//benchmark%resFilename

	end subroutine makeSpaceDelimitedRes

	!* Counts the number of spaces in a string
	function countSpaces(input_str) result(count)
		implicit none
		character(len=*) :: input_str
		integer :: i, count

		count = 0

		do i = 1, len(input_str)
			if (input_str(i:i) == " ") then
					count = count + 1
			end if
		end do

	end function countSpaces



end module qubeRes