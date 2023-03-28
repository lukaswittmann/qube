
!* Module for reading the .res files and storing the data in a structure
module gpgRes
   use mctc_env, only : wp
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
		character(len=200) :: lineStr
		integer :: iret, iostatus 
		character(len=100) :: dirStr, stoichStr, refEnergyStr
		character(len=:), dimension(:), allocatable :: workStr

      ! Open the file
      open(unit=2, file=benchmark%path//benchmark%resFilename, iostat=iret, status='old')
      ! If the file doesn't exist, return
      if(iret.ne.0) return
      do

      	! Read a line
         read(2,'(a)',  iostat=iostatus) lineStr

      	! If the line contains the $tmer keywprd, extract the data
         if(index(lineStr, '$tmer') .ne. 0) then

				call readResLine(lineStr, dirStr, stoichStr, refEnergyStr)		

				! write(*,*) trim(lineStr)
				! write(*,*) ' '
				! write(*,*) trim(dirStr)
				! write(*,*) ' '
				! write(*,*) trim(stoichStr)
				! write(*,*) ' '
				! write(*,*) trim(refEnergyStr)
				write(*,*) '======================================================'

				! TODO Separate strings, that are separated by spaces or tabs

				! Separate the stoichiometry string
				call separateString(trim(stoichStr), workStr)
				write(*,*) workStr
         endif
			
			! If we've reached the end of the file, return
      	if (iostatus /= 0) exit
      enddo
      close(2)
      
   end subroutine readBenchmarkResfile


	!* Separates a string containing multiple tab or space delimted values
	subroutine separateString(strIn, strOut)
		implicit none

		character(len=*), intent(in) :: strIn
		character(len=:), dimension(:), allocatable, intent(out) :: strOut

	end subroutine separateString


	!* Reads a line from the .res file and extracts the data as strings
	subroutine readResLine(lineStr, dirStr, stoichStr, refEnergyStr)
		implicit none

		character(len=*), intent(inout) :: lineStr
		character(len=*), intent(out) :: dirStr, stoichStr, refEnergyStr

		! Indexing variables
		character(len=:), allocatable :: strStartSep, strXSep, strRefSep
		integer :: strStart, strRef, strX

		! Separators
      strStartSep = '$tmer'
		strXSep = 'x'
		strRefSep = 'w'		

		! Extract the indices
		strStart = scan(lineStr, strStartSep, back=.false.) + len(strStartSep)
		strX = scan(lineStr, strXSep, back=.false.)
		strRef = scan(lineStr, strRefSep, back=.false.)

		! Extract the strings
		dirStr = lineStr(strStart:strX-len(strXSep))
		stoichStr = lineStr(strX+len(strXSep):strRef-len(strRefSep)-1)
		refEnergyStr = lineStr(strRef+len(strRefSep):)

	end subroutine readResLine


   !* Extracts a string from a string with optional keyword
   subroutine extractString(strIn, keyword, strOut)
      implicit none

      character(len=*), intent(in) :: strIn, keyword
      character(len=*), intent(out) :: strOut
      integer :: strStart

      strStart = scan(strIn, keyword, back=.false.) + len(keyword)

		strOut = strIn(strStart:)

   end subroutine extractString

	!* Extracts an int from a string with optional keyword
   ! subroutine extractInt(str, keyword, number)
   !    implicit none

   ! end subroutine extractInt




end module gpgRes