module qube_extract

   use qube_error

   implicit none

   contains



!> Reads a string of numbers separated by spaces and stores them in an array.
! Arguments:
! a1: character string containing the numbers to be read.
! x: array to store the numbers.
! n: number of elements read.
subroutine readl(a1,x,n)
   implicit real*8 (a-h,o-z)
   integer :: n, i, j, ib, ie, is
   character*(*) a1
   dimension x(*)
   i=0
   is=1
   10     i=i+1
   x(i)=readaa(a1,is,ib,ie)
   if(ib>0 .and. ie>0) then
      is=ie
      goto 10
   endif
   n=i-1
end

!> This function reads a floating point number from a character string.
! The input string is parsed from the istart-th character to the end of the string.
! The function returns the floating point number read from the string.
! If the string does not contain a valid floating point number, the function returns 0.
! The function also returns the index of the last character read from the string (iend) and the index of the last character read after the exponent (iend2).
! The function is case-insensitive and can handle scientific notation.
! @param a The input character string.
! @param istart The index of the first character to read from the input string.
! @param iend The index of the last character read from the input string.
! @param iend2 The index of the last character read after the exponent.
! @return The floating point number read from the input string.
function readaa(a,istart,iend,iend2)
   
   implicit real*8 (a-h,o-z)

   integer :: istart, iend, iend2, i, j, ibl, idig, idot, ii, izero, m, n, ne, nd, nine, one, minus, c1, c2, nl, ib, ie

   real*8 readaa
   character*(*) a
   nine=ichar('9')
   izero=ichar('0')
   minus=ichar('-')
   idot=ichar('.')
   nd=ichar('d')
   ne=ichar('e')
   ibl=ichar(' ')
   iend=0
   iend2=0
   idig=0
   c1=0
   c2=0
   one=1.d0
   x = 1.d0
   nl=len(a)
   do j=istart,nl-1
      n=ichar(a(j:j))
      m=ichar(a(j+1:j+1))
      if(n<=nine.and.n>=izero .or.n==idot)goto 20
      if(n==minus.and.(m<=nine.and.m>=izero.or. m==idot)) goto 20
   enddo
   readaa=0.d0
   return
   20    continue
   iend=j
   do i=j,nl
      n=ichar(a(i:i))
      if(n<=nine.and.n>=izero) then
         idig=idig+1
         if (idig>10) goto 60
         c1=c1*10+n-izero
      elseif(n==minus.and.i==j) then
         one=-1.d0
      elseif(n==idot) then
         goto 40
      else
         goto 60
      endif
   enddo
   40        continue
   idig=0
   do ii=i+1,nl
      n=ichar(a(ii:ii))
      if(n<=nine.and.n>=izero) then
         idig=idig+1
         if (idig>10) goto 60
         c2=c2*10+n-izero
         x = x /10
      elseif(n==minus.and.ii==i) then
         x=-x
      else
         goto 60
      endif
   enddo
   !
   ! put the pieces together
   !
   60        continue
   readaa= one * ( c1 + c2 * x)
   do j=iend,nl
      n=ichar(a(j:j))
      iend2=j
      if(n==ibl)return
      if(n==nd .or. n==ne)goto 57
   enddo
   return
   57    c1=0.0d0
   one=1.0d0
   do i=j+1,nl
      n=ichar(a(i:i))
      iend2=i
      if(n==ibl)goto 70
      if(n<=nine.and.n>=izero) c1=c1*10.0d0+n-izero
      if(n==minus)one=-1.0d0
   enddo
   61        continue
   70         readaa=readaa*10**(one*c1)
end function readaa



!* gCP
subroutine getengcp(e,filen)
    implicit none
    real*8 xx(10),e
    integer nn,iret
    character*(*) filen
    character*120 a
    open(unit=10,file=filen,iostat=iret)
    if(iret/=0) return
    4               read(10,'(a)',end=20) a
    call readl(a,xx,nn)
    !      if(nn>=4) then
    !         e=xx(2)
    !      endif
    e=xx(1)
    goto 4
    20            continue
    close (10)
end

!* DFT-C
subroutine getendftc(e,filen)
    implicit none
    real*8 xx(10),e
    integer nn,iret
    character*(*) filen
    character*120 a
    open(unit=10,file=filen,iostat=iret)
    if(iret/=0) return
    4           read(10,'(a)',end=20) a
    call readl(a,xx,nn)
    !      if(nn>=4) then
    !         e=xx(2)
    !      endif
    e=xx(1)
    goto 4
    20         continue
    close (10)
end

!* D3, D4
subroutine getend3(e,filen)
    implicit none
    real*8 xx(10),e
    integer nn,iret
    character*(*) filen
    character*120 a
    open(unit=10,file=filen,iostat=iret)
    if(iret/=0) return
    4               read(10,'(a)',end=20) a
    call readl(a,xx,nn)
    !      if(nn>=4) then
    !         e=xx(2)
    !      endif
    e=xx(1)
    goto 4
    20            continue
    close (10)
end

!* ORCA final single point
subroutine getenorca(e,filen)
    implicit none
    real*8 xx(10),e
    integer nn,iret
    character*(*) filen
    character*120 a
    open(unit=10,file=filen,iostat=iret)
    if(iret/=0) return
    4       read(10,'(a)',end=20) a
    if(index(a,'final single')/=0)then
        !     if(index(a,'initial e(tot)')/=0)then
        call readl(a,xx,nn)
        e=xx(nn)
    endif
    goto 4
    20      continue
    close (10)
end

!* ADF final single point
subroutine geten_adf(e,filen)
    implicit none
    real*8 xx(10),e
    integer nn,iret
    character*(*) filen
    character*120 a
    open(unit=10,file=filen,iostat=iret)
    if(iret/=0) return
    4            read(10,'(a)',end=20) a
    if(index(a,'energy (hartree)')/=0)then
        call readl(a,xx,nn)
        e=xx(1)
    endif
    goto 4
    20         continue
    close (10)
end

!* ORCA NL energy
subroutine getenorcanl(e,filen)
    implicit none
    real*8 xx(10),e
    integer nn,iret
    character*(*) filen
    character*120 a
    open(unit=10,file=filen,iostat=iret)
    if(iret/=0) return
    4       read(10,'(a)',end=20) a
    if(index(a,'nl    energy')/=0)then
        !     if(index(a,'initial e(tot)')/=0)then
        call readl(a,xx,nn)
        e=xx(nn)
    endif
    goto 4
    20      continue
    close (10)
end

!* ORCA RI-MP2 correlation energy
subroutine getrimp2c(e,filen)
    implicit none
    real*8 xx(10),e
    integer nn,iret
    character*(*) filen
    character*120 a
    open(unit=10,file=filen,iostat=iret)
    if(iret/=0) return
    4       read(10,'(a)',end=20) a
    if(index(a,' ri-mp2 correlation energy:')/=0)then
        !     if(index(a,'initial e(tot)')/=0)then
        call readl(a,xx,nn)
        e=xx(nn)
    endif
    goto 4
    20      continue
    close (10)
end


subroutine getdftc(e,filen)
    implicit none
    real*8 xx(10),e
    integer nn,iret
    character*(*) filen
    character*120 a
    open(unit=10,file=filen,iostat=iret)
    if(iret/=0) return
    4       read(10,'(a)',end=20) a
    if(index(a,'e(c)               :')/=0)then
        !     if(index(a,'initial e(tot)')/=0)then
        call readl(a,xx,nn)
        e=xx(nn)
    endif
    goto 4
    20      continue
    close (10)
end

!* ORCA CCSD(T) correlation energy
subroutine getccsdtc(e,filen)
    implicit none
    real*8 xx(10),e
    integer nn,iret
    character*(*) filen
    character*120 a
    open(unit=10,file=filen,iostat=iret)
    if(iret/=0) return
    4       read(10,'(a)',end=20) a
    if(index(a,'final correlation energy')/=0)then
        !     if(index(a,'initial e(tot)')/=0)then
        call readl(a,xx,nn)
        e=xx(nn)
    endif
    goto 4
    20      continue
    close (10)
end

!* QCHEM final single point
subroutine getqchemfinalsinglepoint(e,filen)
    implicit none
    real*8 xx(10),e
    integer nn,iret
    character*(*) filen
    character*120 a
    open(unit=10,file=filen,iostat=iret)
    if(iret/=0) return
    4       read(10,'(a)',end=20) a
    if(index(a,'total energy in the final basis set =')/=0)then
        call readl(a,xx,nn)
        e=xx(nn)
    endif
    goto 4
    20      continue
    close (10)
end

!* QCHEM DFT correlation energy
subroutine getqchemdftcorr(e,filen)
    implicit none
    real*8 xx(10),e
    integer nn,iret
    character*(*) filen
    character*120 a
    open(unit=10,file=filen,iostat=iret)
    if(iret/=0) return
    4       read(10,'(a)',end=20) a
    if(index(a,'dft correlation      energy =')/=0)then
        call readl(a,xx,nn)
        e=xx(nn)
    endif
    goto 4
    20      continue
    close (10)
end

!* QCHEM MOS-MP2 correlation energy
subroutine getqchemmosmp2corr(e,filen)
    implicit none
    real*8 xx(10),e
    integer nn,iret
    character*(*) filen
    character*120 a
    open(unit=10,file=filen,iostat=iret)
    if(iret/=0) return
    4       read(10,'(a)',end=20) a
    if(index(a,'total mos-mp2 correlation energy        =')/=0)then
        call readl(a,xx,nn)
        e=xx(nn)
    endif
    goto 4
    20      continue
    close (10)
end

end module qube_extract