program main

   use qube_error
   use qube_functions
   use qube_extract

   implicit none

   integer :: nmol,imet,i,nargs
   character(len=120),allocatable :: molname(:),btmp(:)
   character(len=120) :: atmp,mtmp
   real*8, allocatable :: stoechf(:)
   real*8, allocatable :: e      (:)
   real*8 :: fak1,eadd,emul,eref,de,fak2
   common /fit/ fak1,fak2
!  external wsigint,wsigterm
   call signal(2,wsigint)
   call signal(15,wsigterm)
   ! options printout
   if(command_argument_count()==0) call printopt
   nargs=command_argument_count()
   allocate(molname(nargs),btmp(nargs),stoechf(nargs),e(nargs))
   ! fit-param
   fak1=0
   fak2=0
   !     open(unit=1,file='~/.tmerparam')
   !     read(1,*)fak1,fak2
   !     close(1)
   stoechf=0.0d0
   e      =0.0d0
   eref   =0.0d0
   eadd   =0.0d0
   emul   =1.0d0
   molname=''
   ! get dir names and stoech-faktors
   call getreak(nmol,molname,stoechf)
   ! defaults for add and weighting
   imet=idint(stoechf(nmol+1))
   ! makes no sense for mvd calc.
   if(imet/=15)then
      if(abs(stoechf(nmol+2))>1.d-8)eref=      stoechf(nmol+2)
      if(stoechf(nmol+3)>1.d-8)eadd=      stoechf(nmol+3)
      if(stoechf(nmol+4)>1.d-8)emul=      stoechf(nmol+4)
   endif

   if(imet<0) error stop 'imet >= 0!'
   call gete(nmol,imet,molname,e)
   de=0
   do i=1,nmol
      de=de+e(i)*stoechf(i)
   enddo
   de=627.509541*de+eadd


   ! =======================================================================
   ! ======================== control printout here ========================
   ! =======================================================================
   write(*,'(8f22.12,3x,5a)') (e(i),i=1,5), de, (de-eref)*emul, eref,(trim(adjustl(molname(i))),i=1,5)
   ! energies
   ! resulting interaction energy de,
   ! resulting difference to reference 
   ! reference energy 
   ! names of structures(trim(adjustl(molname(i))),i=1,5)
   ! c     make the same print statement but dont use format instead use
   ! c     trim and adjustl
   !       write(*, *) trim(adjustl(molname(1))), trim(adjustl(molname(2))),
   !      . trim(adjustl(molname(3))), trim(adjustl(molname(4))),
   !      . trim(adjustl(molname(5))), de, (de-eref)*emul, eref,
   !      . (trim(adjustl(molname(i))),i=1,5)
end

