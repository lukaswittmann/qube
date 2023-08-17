module qube_functions 
   
   use qube_error
   use qube_extract

   implicit none

   contains

!* Select the right subroutine and execute it
subroutine gete(nmol,imet,molname,e)

   implicit none
   character(len=120) :: molname(*)
   real*8 :: e(*)
   real*8 :: e1(nmol)
   real*8 :: e2(nmol)
   real*8 :: edum,edum1,edum2,edum3
   integer :: nmol,imet
   integer :: i
   character(len=120) :: aa
   ! common /fit/ fak1,fak2
   real*8 :: fak1,fak2
   logical :: vdw,ex,cp,abc,zpe,etherm
   vdw=.false.
   cp=.false.
   abc=.false.  !vdw-three-body only
   zpe=.false.  !add crystal zpe from (..)/freq/crystal.out
   etherm=.false. !add crystal ethermal from (..)/freq/crystal.out

   if(imet>=2000)then
      imet=imet-2000
      zpe=.true.
      etherm=.true.
   endif
   if(imet>=1000)then
      imet=imet-1000
      zpe=.true.
   endif
   if(imet>=400)then
      imet=imet-400
      abc=.true.
   elseif(imet>=300)then
      imet=imet-300
      vdw=.true.
      cp=.true.
   elseif(imet>=200)then
      imet=imet-200
      cp=.true.
   elseif(imet>=100)then
      imet=imet-100
      vdw=.true.
   endif

   do i=1,nmol
      e(i) =0.0d0
      e1(i) =0.0d0
      e2(i) =0.0d0
      edum =0.0d0
      edum1=0.0d0
      edum2=0.0d0

      !* Basic extraction

      if(imet==13)then
      ! orca
         write(aa,'(a,''/orca.out'')')trim(molname(i))
         call get_en_orca(e(i),aa)
   
      ! gcp
      elseif(imet==30)then
         write(aa,'(a,''/.CPC'')')trim(molname(i))
         call get_en_d4(e(i),aa)
         
      ! d3
      elseif(imet==32) then
         write(aa,'(a,''/.EDISP'')')trim(molname(i))
         call get_en_d4(e(i),aa)
              
      ! total energy adf
      elseif(imet==52) then
         write(aa,'(a,''/adf.out'')')trim(molname(i))
         call get_en_adf(e(i),aa)

      ! total energy dft-c
      elseif(imet==53) then
         write(aa,'(a,''/.dftc'')')trim(molname(i))
         call get_en_d4(e(i),aa)

      ! nl energy from orca output
      elseif(imet==54) then
         write(aa,'(a,''/orca.out'')')trim(molname(i))
         call get_en_orca_nl(e(i),aa)

      ! ri-mp2 correlation energy from orca output
      elseif(imet==55) then
         write(aa,'(a,''/orca.out'')')trim(molname(i))
         call get_en_orca_rimp2c(e(i),aa)

      ! dft correlation energy from orca output
      elseif(imet==56) then
         write(aa,'(a,''/orca.out'')')trim(molname(i))
         call get_en_orca_dftc(e(i),aa)

      ! ccsd(t) correlation energy from orca output
      elseif(imet==57) then
         write(aa,'(a,''/orca.out'')')trim(molname(i))
         call get_en_orca_ccsdtc(e(i),aa)

      ! qchem final single point
      elseif(imet==58) then
         write(aa,'(a,''/qchem.out'')')trim(molname(i))
         call get_en_qchem(e(i),aa)

      ! qchem dft correlation
      elseif(imet==59) then
         write(aa,'(a,''/qchem.out'')')trim(molname(i))
         call get_en_qchem_dftc(e(i),aa)

      ! qchem ri-mos mp2 correlation
      elseif(imet==60) then
         write(aa,'(a,''/qchem.out'')')trim(molname(i))
         call get_en_qchem_mosmp2(e(i),aa)

      else 
         call raise('e', 'Unknown method')
      
      endif

      !* Additional corrections

      ! if(abc)then
      !    write(aa,'(a,''/dftd3.out'')')trim(molname(i))
      !    call getenabc(edum3,aa)
      !    e(i)=e(i)+edum3
      ! endif

      if(vdw)then
         write(aa,'(a,''/.edisp'')')trim(molname(i))
         call get_en_d4(edum,aa)
         e(i)=e(i)+edum
      endif

      if(cp) then
         write(aa,'(a,''/.cpc'')')trim(molname(i))
         call get_en_d4(edum2,aa)
         e(i)=e(i)+edum2
      endif


   enddo
end

!> This subroutine reads command line arguments and extracts molecular names and stoch. factors.
! The molecular names are stored in the input array molname and the factors are stored in the output array fac.
! If the number of molecular names is greater than the number of factors, the subroutine throws an error.
! @param nmol The number of molecular names.
! @param molname The array of molecular names.
! @param fac The array of factors.
subroutine getreak(nmol,molname,fac)
   character(len=120) :: molname(*)
   character(len=120),allocatable :: dir(:)
   character(len=10) tmp
   real*8 :: xx(20)
   real*8 :: fac(*)
   logical da
   integer i,j,n,nn,nfac,nmol,nargs
   nargs=command_argument_count()
   allocate(dir(nargs))
   dir = ''
   n=0
   do i=1,nargs
      call getarg(i,dir(i))
      if(dir(i)=='x'.and.n==0)then
         n=i
      endif
   enddo
   if(n==0)n=20
   nmol=0
   nfac=0
   do i=1,nargs
      if(i<n)then
         nmol=nmol+1
         molname(nmol)=dir(i)
      else
         call readl(dir(i),xx,nn)
         if(nn>0) then
            nfac=nfac+1
            fac(nfac)=xx(1)
         endif
      endif
   enddo
   if(nmol>nfac) error stop 'nmol > nfac'
end subroutine getreak

!> Prints the usage and available methods for the tmer2 program.
! The available methods are printed with their corresponding number.
subroutine printopt

   print'(a)','usage: '//'tmer2 <dirs> ... x <stfacts> ... <imethod> [refval]',''
   print'(a)','list of available <imethod>:',''

   print'(6xa)','52=adf energy (adf.out)',''

   print'(6xa)','53=dftc   (.dftc)'
   print'(6xa)','30=cpc    (.cpc)'
   print'(6xa)','32=d3/d4  (.edisp)',''

   print'(6xa)','13=orca                  (orca.out)'
   print'(6xa)','35=sc+nl                 (orca.out)'
   print'(6xa)','54=nl                    (orca.out)'
   print'(6xa)','55=ri-mp2 correlation    (orca.out)'
   print'(6xa)','56=dft correlation       (orca.out)'
   print'(6xa)','57=ccsd(t) correlation   (orca.out)',''

   print'(6xa)','58=total energy in the final basis set  (qchem.out)'
   print'(6xa)','59=dft correlation energy               (qchem.out)'
   print'(6xa)','60=total mos-mp2 correlation energy     (qchem.out)',''

   print'(6xa)','imethod+100  reads dispersion energy from .edisp'
   print'(6xa)','imethod+200  reads counterpoise correcture from .cpc'
   print'(6xa)','imethod+300  reads .edisp and .gcp',''

   call terminate(0)
end

end module qube_functions