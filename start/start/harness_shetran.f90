PROGRAM harness_shetran
USE IFPORT
USE GETDIRQQ, ONLY : GET_DIR_AND_CATCH
USE DFLIB, ONLY : SPLITPATHQQ      
use xmlparse
IMPLICIT NONE

INTEGER(4)                :: res,i,length
CHARACTER(128), PARAMETER :: exename1='shetran-prepare.exe'
CHARACTER(128), PARAMETER :: exename2='shetran-prepare-snow.exe'
CHARACTER(128), PARAMETER :: exename3='shetran-prepare-sediment.exe'
CHARACTER(128), PARAMETER :: exename4='shetran.exe'
CHARACTER(128)            :: exenameused
CHARACTER(512)            :: filnam, exedir
CHARACTER(1024)           :: c
LOGICAL                   :: Lres
CHARACTER(3)              ::drive
CHARACTER(256)            ::path, ext, basedir
integer*4                 ::lengthpath 
CHARACTER(100)             ::MyName      

type(XML_PARSE)   :: info
character*200             ::xmlfilefull
character(len=200)                      :: tag
logical                                :: endtag
character(len=200), dimension(1:1,1:1)  :: attribs
integer                                :: no_attribs
character(len=200), dimension(1:1)     :: dummy
character(len=200), dimension(1:1)     :: projectfile
character(len=200), dimension(1:1)     :: catchmentname
character(len=200)                     :: catchmentname2
   integer                                :: no_data
logical :: file_exists1
logical :: file_exists2
logical :: file_exists3

INQUIRE(FILE=trim(exename1), EXIST=file_exists1)
INQUIRE(FILE=trim(exename2), EXIST=file_exists2)
INQUIRE(FILE=trim(exename3), EXIST=file_exists3)
if (file_exists1) then
    exenameused = exename1
!    write (*,*) 'Using ',trim(exename1)
elseif (file_exists2) then
    exenameused = exename2
!    write (*,*) 'Using ',trim(exename2)
elseif (file_exists3) then
    exenameused = exename3
!    write (*,*) 'Using ',trim(exename3)
else
write (*,*) 
write (*,*) 'No version of Shetran-prepare exists in this folder'
write (*,*) 
pause
stop
endif


!find directory where executables are
res = GETCWD(exedir)
!open the get_file dialog
CALL GET_DIR_AND_CATCH (filnam)


xmlfilefull = trim(filnam)
open(10,FILE=xmlfilefull,err=9999,status='old')

goto 9998

9999 write (*,*) 'Error openinig file ',xmlfilefull
close(10)
pause
stop 1

9998 close (10)
call xml_open( info, xmlfilefull, .true. )
call xml_options( info, report_details = .false. )
call xml_get( info, tag, endtag, attribs, no_attribs, projectfile, no_data )
if ( xml_error(info) ) then
       stop
endif
call xml_get( info, tag, endtag, attribs, no_attribs, dummy, no_data )
if ( xml_error(info) ) then
     stop
endif
call xml_get( info, tag, endtag, attribs, no_attribs, catchmentname, no_data )
if ( xml_error(info) ) then
    stop
endif
call xml_close( info )


Lres = CHANGEDIRQQ (TRIM(exedir))
WRITE(c,'(A)') TRIM(exenameused)//' "'//TRIM(filnam)//'"'
!WRITE(c,'(A)') TRIM(exename)//' -f "'//TRIM(filnam)//'"'

!print*, trim(c)

res=system(trim(c))

lengthpath = SPLITPATHQQ(filnam, drive, path, MyName, ext)
basedir= trim(drive)//trim(path)

WRITE(c,'(A)') TRIM(exename4)//' -f "'//trim(basedir)//'rundata_'//trim(catchmentname(1))//'.txt'//'"'
!print*, trim(c)

res=system(trim(c))

pause

END PROGRAM harness_shetran