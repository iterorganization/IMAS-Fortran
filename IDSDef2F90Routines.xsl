<?xml version="1.0" encoding="UTF-8"?>
<?modxslt-stylesheet type="text/xsl" media="fuffa, screen and $GET[stylesheet]" href="./%24GET%5Bstylesheet%5D" alternate="no" title="Translation using provided stylesheet" charset="ISO-8859-1" ?>
<?modxslt-stylesheet type="text/xsl" media="screen" alternate="no" title="Show raw source of the XML file" charset="ISO-8859-1" ?>
<xsl:stylesheet xmlns:yaslt="http://www.mod-xslt2.com/ns/2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xs="http://www.w3.org/2001/XMLSchema" version="2.0" extension-element-prefixes="yaslt" xmlns:fn="http://www.w3.org/2005/02/xpath-functions" xmlns:local="http://www.example.com/functions/local" exclude-result-prefixes="local xs">
<xsl:output method="text" version="1.0" encoding="UTF-8" indent="yes"/>	<!-- This XSL translates the list of IMAS IDSDefs to Fortran 90 GET/PUT Routines for IDSs -->

<xsl:param name="DD_GIT_DESCRIBE" as="xs:string" required="yes"/>
<xsl:param name="AL_GIT_DESCRIBE" as="xs:string" required="yes"/>

<xsl:function name="local:unique_name" as="xs:string">
  <!-- Provides pseudo-unique 16 characters reference to arbitrary long field name  -->
  <xsl:param name="FullName" as="xs:string"/>
<!--  <xsl:variable name="result" as="xs:string">-->
    <xsl:choose>
      <xsl:when test="string-length($FullName) &lt; 16">
        <xsl:value-of select="$FullName"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="concat(lower-case(substring($FullName,1,15)), sum(string-to-codepoints(lower-case(substring($FullName,16)))))"/>
      </xsl:otherwise>
    </xsl:choose>
<!--  </xsl:variable>
        <xsl:value-of select="$result"/>-->
</xsl:function>
<xsl:key name="unique-structures-by-type" match="field[@data_type='structure' or @data_type='struct_array']" 
         use="if (@structure_reference='self') then @name else @structure_reference" />
<xsl:key name="unique-structures-by-name" match="field[@data_type='structure' or @data_type='struct_array']" 
         use="@name" />
<xsl:key name="unique-structures-combined" match="field[@data_type='structure' or @data_type='struct_array']" 
         use="concat(if (@structure_reference='self') then @name else @structure_reference, '|', @name)" />
<xsl:key name="unique-structures-by-ids" match="field[@data_type='structure' or @data_type='struct_array']" 
         use="concat(if (@structure_reference='self') then @name else @structure_reference, '|', ancestor::IDS/@name)" />
<xsl:key name="utilities-fields" match="/IDSs/utilities/field" use="@name" />

<xsl:template match="/IDSs">
  <xsl:result-document href="ids_schemas.f90">
  module ids_schemas
  <xsl:for-each select="IDS">
  use ids_schemas_<xsl:value-of select="@name"/>
  </xsl:for-each>  
  end module
  </xsl:result-document>
  <xsl:result-document href="ids_routines.f90">
module ids_routines
use ids_schemas
use al_low_level_wrap
use utilities_copy_struct
use utilities_deallocate_struct
<!--use specific_validate_struct-->

<xsl:for-each select="IDS">
use <xsl:value-of select="@name"/>_put_struct
use <xsl:value-of select="@name"/>_put_slice_struct
use <xsl:value-of select="@name"/>_get_struct
use <xsl:value-of select="@name"/>_get_slice_struct
use <xsl:value-of select="@name"/>_delete
use <xsl:value-of select="@name"/>_copy_struct
use <xsl:value-of select="@name"/>_deallocate_struct
use <xsl:value-of select="@name"/>_validate_struct
</xsl:for-each>

#if defined(__INTEL_COMPILER)
use ifport, only : getpid  ! required for getpid() in ifort
#endif
#if defined(NAGFOR)
use f90_unix, only : getpid  ! required for getpid() in nagfor
#endif

contains

subroutine list_all_occurrences(idx, ids_name, node_path, node_content_list, occurrence_list)
  use al_low_level_wrap
  use ids_types
  implicit none
  integer(ids_int), intent(in)  :: idx
  character*(*), intent(in)  :: ids_name, node_path
  character(len=:), allocatable, intent(inout) :: node_content_list(:)
  integer, allocatable, intent(inout) :: occurrence_list(:)

  integer(ids_int), allocatable :: al_occurrences_list(:)
  character(len=:), allocatable :: replies(:)
  character(len=:), allocatable :: temp_replies(:)
  character(:), allocatable :: ids_full_name
  integer(ids_int) :: size, reply_dim
  character(STRMAXLEN) :: data
  integer :: status, i, n_max
  character(:), allocatable :: err_msg
  integer(ids_int) :: opctx

  call al_get_occurrences(idx, ids_name, al_occurrences_list, size, status, err_msg)

  if (allocated(node_content_list)) deallocate(node_content_list)
  if (allocated(occurrence_list)) deallocate(occurrence_list)

  if (status &lt; 0) then
    write(*,'(A,I0,A)') "IMAS:list_all_occurrences:Failed. Error calling al_get_occurrences for IDS name"//ids_name//"(idx=",idx,"):"//err_msg
    return
  end if

  if (size&gt;0) then
    allocate(character(len=1) :: node_content_list(size))
    allocate(occurrence_list(size))
    node_content_list(:) = " " 
  else 
    return
  end if

  do i = 1, size
    occurrence_list(i) = al_occurrences_list(i)
  end do
  if (LEN(node_path) .ne. 0) then 
      allocate(character(len=1) :: replies(size))
      n_max = 0
      do i = 1, size
            ids_full_name = ids_name
            if (al_occurrences_list(i)&gt;0) then
              ids_full_name=ids_full_name//"/"//trim(str(al_occurrences_list(i))) 
            end if

            call al_begin_global_action(idx, ids_full_name, READ_OP, opctx, status, err_msg) 
            if (status &lt; 0) then
              write(*,'(A)') "IMAS:list_all_occurrences:Failed. Error calling al_begin_global_action "//err_msg
              return
            end if

            call get_string(opCtx, node_path, "", data, reply_dim, status)
            if (status &lt; 0) then
              write(*,'(A)') "IMAS:list_all_occurrences:Failed. Error calling get_string "
              return
            end if

            call al_end_action(opctx, status, err_msg)
            if (status &lt; 0) then
              write(*,'(A)') "IMAS:list_all_occurrences:Failed. Error calling al_end_action: "//err_msg
              return
            end if

            if (reply_dim &gt; LEN(replies)) then
              !-- we use a temp array of string to realloc the replies (increasing the length)
              if(allocated(temp_replies)) deallocate(temp_replies)
              allocate(character(len=LEN(replies)) :: temp_replies(size))
              temp_replies = replies

              if(allocated(replies)) deallocate(replies)
              allocate(character(len=reply_dim) :: replies(size))
              replies(:) = temp_replies(:)
            end if

            replies(i) = data
        end do

        if(allocated(node_content_list)) deallocate(node_content_list)
        allocate(character(len=LEN(replies)) :: node_content_list(size))
        do i = 1, size
          node_content_list(i) = replies(i)
        end do
  else 
    do i = 1, size
      node_content_list(i) = "";
    end do
  end if 

end subroutine

subroutine ids_get_times(pulseCtx,path,time)
use al_low_level_wrap
use ids_types
implicit none

integer(ids_int) :: pulsectx, opctx, status
character*(*) :: path
real(ids_real), pointer :: time(:)
integer(ids_int) :: dim1

call al_begin_global_action(pulsectx, path, READ_OP, opctx, status) 
if (status.ne.0) then
   STOP 'Error in al_begin_global_action from ids_get_times'
end if

call get_vect1d_double(opctx, "time", "time", time, dim1, status)

call al_end_action(opctx, status)

end subroutine

! Turn an IDS into a bunch of bytes
subroutine ids_serialize(ids_in, buffer, protocol)
#if defined(_Linux)
#  define SERIALIZE_TEMPORARY_DIRECTORY '/dev/shm/'
#else
#  define SERIALIZE_TEMPORARY_DIRECTORY ''
#endif
  use, intrinsic :: ISO_C_BINDING, only: C_LOC
  class(IDS_base) :: ids_in ! no intent(in) because ids_put also does not have that
  integer(ids_int), intent(in), optional :: protocol
  character(len=1), dimension(:), allocatable :: buffer

  character(len=1), dimension(:), pointer :: pbuffer
  character(len=:), allocatable :: fname
  integer(ids_int) :: my_protocol
  integer(ids_int) :: pulsectx
  integer(ids_int) :: status
  integer(ids_int) :: unit
  integer(ids_int) :: file_size
  integer(ids_int) :: index
  integer(ids_int) :: dim1
  integer :: TMP_DIR_SIZE
  character(STRMAXLEN):: uri
  character(STRMAXLEN):: filename
  CHARACTER(len=255) :: BUFFER_IMAS_AL_SERIALIZER_TMP_DIR
  CHARACTER(len=:), ALLOCATABLE :: IMAS_AL_SERIALIZER_TMP_DIR
  
  my_protocol = DEFAULT_SERIALIZER_PROTOCOL
  if (present(protocol)) my_protocol = protocol

  ! Prepare data entry based on the chosen serializer protocol:
  if (my_protocol .eq. ASCII_SERIALIZER_PROTOCOL) then
    fname = generate_tmp_file()
    if (len_trim(fname) .eq. 0) then
      write(*,*) 'SERIALIZE: ERROR generating temporary file name'
      return
    end if

    index = SCAN(fname,'/', .TRUE.)
    filename=fname(index+1:)
    ! Write to file
    !call al_build_uri_from_legacy_parameters(ASCII_BACKEND, 0, 0, 'serialize', 'serialize', '3','-fullpath '//fname, uri, status)
    CALL get_environment_variable("IMAS_AL_SERIALIZER_TMP_DIR", BUFFER_IMAS_AL_SERIALIZER_TMP_DIR)
    TMP_DIR_SIZE = LEN_TRIM(BUFFER_IMAS_AL_SERIALIZER_TMP_DIR)
    if(TMP_DIR_SIZE > 0) then
      if(BUFFER_IMAS_AL_SERIALIZER_TMP_DIR(LEN_TRIM(BUFFER_IMAS_AL_SERIALIZER_TMP_DIR):LEN_TRIM(BUFFER_IMAS_AL_SERIALIZER_TMP_DIR)) /= "/") then
        BUFFER_IMAS_AL_SERIALIZER_TMP_DIR = TRIM(BUFFER_IMAS_AL_SERIALIZER_TMP_DIR) // "/"
      end if
      allocate(character(TMP_DIR_SIZE):: IMAS_AL_SERIALIZER_TMP_DIR)
      IMAS_AL_SERIALIZER_TMP_DIR = trim(BUFFER_IMAS_AL_SERIALIZER_TMP_DIR)
      uri = "imas:ascii?path=" // IMAS_AL_SERIALIZER_TMP_DIR // ";filename="//filename
    else
    uri = "imas:ascii?path=" // SERIALIZE_TEMPORARY_DIRECTORY // ";filename="//filename
    endif
    call al_begin_dataentry_action(uri, FORCE_CREATE_PULSE, pulsectx, status)
    if (status .ne. 0) then
      write(*,*) "SERIALIZE: ERROR opening ASCII backend - al_open_pulse"
      buffer = ''
      return
    end if
  else if (my_protocol .eq. FLEXBUFFERS_SERIALIZER_PROTOCOL) then
    call al_begin_dataentry_action("imas:flexbuffers?path=/", FORCE_CREATE_PULSE, pulsectx, status)
    if (status .ne. 0) then
      write(*,*) "SERIALIZE: ERROR opening Serialize backend - al_open_pulse"
      buffer = ''
      return
    end if
  else
    write(*,*) 'SERIALIZE: ERROR, unrecognized serialization protocol'
    return
  end if

  ! Put IDS to serialization backend
  ! I think if we implement an object-oriented ids_in->put the select type here becomes unnecessary
  select type (ids_in)
  <xsl:for-each select="IDS">
  class is (ids_<xsl:value-of select="@name"/>)
    call ids_put(pulsectx, '<xsl:value-of select="@name"/>', ids_in)
  </xsl:for-each>
  class default
    write(*,*) "SERIALIZE: ERROR selecting IDS type"
  end select
    
  ! Retrieve serialized IDS from the backend
  if (my_protocol .eq. ASCII_SERIALIZER_PROTOCOL) then
    ! Read from file
    unit = get_file_unit()
    open(unit=unit, file=fname, action='read', status='old', form='unformatted', access='stream')
    inquire(unit=unit, size=file_size)
    allocate(character(1) :: buffer(file_size + 1))
    buffer(1) = char(ASCII_SERIALIZER_PROTOCOL)
    read(unit) buffer(2:)
    close(unit, status='delete')
  else if (my_protocol .eq. FLEXBUFFERS_SERIALIZER_PROTOCOL) then
    call get_vect1D_char(pulsectx, "&lt;buffer&gt;", "", pbuffer, dim1, status)
    if (status .ne. 0) then
      write(*,*) "SERIALIZE: ERROR reading buffer from serialize backend"
      buffer = ''
    end if
    allocate(buffer(size(pbuffer)))
    buffer = pbuffer
    call c_free(C_LOC(pbuffer))
    nullify(pbuffer)
  end if

  ! Close and clean up the data entry
  call al_close_pulse(pulsectx, CLOSE_PULSE, status)
  if (status .ne. 0) then
    write(*,*) "SERIALIZE: ERROR closing backend - al_close_pulse"
    buffer = ''
  end if
  call al_end_action(pulsectx, status)
  if (status .ne. 0) then
    write(*,*) "SERIALIZE: ERROR closing backend - al_end_action"
    buffer = ''
  end if
end subroutine ids_serialize

! Turn a bunch of bytes into an IDS
subroutine ids_deserialize(buffer, ids_out)
#if defined(_Linux)
#  define SERIALIZE_TEMPORARY_DIRECTORY '/dev/shm/'
#else
#  define SERIALIZE_TEMPORARY_DIRECTORY ''
#endif
  class(IDS_base) :: ids_out ! it is up to you to pass the correct buffer and ids type
  character(len=1), dimension(:), allocatable, intent(in) :: buffer

  character(len=1), dimension(:), pointer :: pbuffer
  integer(ids_int) :: protocol
  character(len=:), allocatable :: fname
  integer(ids_int) :: pulsectx
  integer(ids_int) :: status
  integer(ids_int) :: unit
  integer(ids_int) :: file_size
  integer(ids_int) :: index
  integer :: TMP_DIR_SIZE
  character(STRMAXLEN):: uri
  character(STRMAXLEN):: filename
  CHARACTER(len=255) :: BUFFER_IMAS_AL_SERIALIZER_TMP_DIR
  CHARACTER(len=:), ALLOCATABLE :: IMAS_AL_SERIALIZER_TMP_DIR
  protocol = ichar(buffer(1))

  ! Prepare data entry based on the chosen serializer protocol:
  if (protocol .eq. ASCII_SERIALIZER_PROTOCOL) then
    fname = generate_tmp_file()
    
    if (len_trim(fname) .eq. 0) then
      write(*,*) 'SERIALIZE: ERROR generating temporary file name'
      return
    end if

    index = SCAN(fname,'/', .TRUE.)
    filename=fname(index+1:)
    ! Write to file
    unit = get_file_unit()
    open(unit=unit, file=fname, action='write', status='new', form='unformatted', access='stream')
    write(unit) buffer(2:)
    ! keep the file open, so we can delete it later in one go
    flush(unit)

    !call al_build_uri_from_legacy_parameters(ASCII_BACKEND, 0, 0, 'serialize', 'serialize', '3','-fullpath '//fname, uri, status)
    CALL get_environment_variable("IMAS_AL_SERIALIZER_TMP_DIR", BUFFER_IMAS_AL_SERIALIZER_TMP_DIR)
    TMP_DIR_SIZE = LEN_TRIM(BUFFER_IMAS_AL_SERIALIZER_TMP_DIR)
    if(TMP_DIR_SIZE > 0) then
      allocate(character(TMP_DIR_SIZE):: IMAS_AL_SERIALIZER_TMP_DIR)
      IMAS_AL_SERIALIZER_TMP_DIR = trim(BUFFER_IMAS_AL_SERIALIZER_TMP_DIR)
      uri = "imas:ascii?path=" // IMAS_AL_SERIALIZER_TMP_DIR // ";filename="//filename
    else
    uri = "imas:ascii?path=" // SERIALIZE_TEMPORARY_DIRECTORY // ";filename="//filename
    endif
    call al_begin_dataentry_action(uri, FORCE_CREATE_PULSE, pulsectx, status)
    if (status .ne. 0) then
      write(*,*) "SERIALIZE: ERROR opening ASCII backend - al_open_pulse"
      return
    end if
  else if (protocol .eq. FLEXBUFFERS_SERIALIZER_PROTOCOL) then
    call al_begin_dataentry_action("imas:flexbuffers?path=/", OPEN_PULSE, pulsectx, status)
    if (status .ne. 0) then
      write(*,*) "SERIALIZE: ERROR opening Serialize backend - al_open_pulse"
      return
    end if
    ! Write buffer to the backend
    allocate(pbuffer(size(buffer)))
    pbuffer = buffer
    call put_vect1D_char(pulsectx, "", "&lt;buffer&gt;", "", pbuffer, "", status)
    deallocate(pbuffer)
    if (status .ne. 0) then
      write(*,*) "SERIALIZE: ERROR writing buffer to Serialize backend"
      return
    end if
  else
    write(*,*) 'SERIALIZE: ERROR, unrecognized serialization protocol', protocol
    return
  end if

  ! Get IDS from the backend
  ! I think if we implement an object-oriented ids_in->put the select type here becomes unnecessary
  select type (ids_out)
  <xsl:for-each select="IDS">
  class is (ids_<xsl:value-of select="@name"/>)
    call ids_get(pulsectx, '<xsl:value-of select="@name"/>', ids_out)
  </xsl:for-each>
  class default
    write(*,*) "SERIALIZE: ERROR selecting IDS type"
  end select

  ! Close and clean up the data entry
  call al_close_pulse(pulsectx, CLOSE_PULSE, status)
  if (status .ne. 0) then
    write(*,*) "SERIALIZE: ERROR closing ASCII backend - al_close_pulse"
    call al_end_action(pulsectx, status)
    return
  end if
  call al_end_action(pulsectx, status)
  if (status .ne. 0) then
    write(*,*) "SERIALIZE: ERROR closing ASCII backend - al_end_action"
    return
  end if

  ! Serializer specific cleanup
  if (protocol .eq. ASCII_SERIALIZER_PROTOCOL) then
    ! delete file
    close(unit, status='delete')
  end if
end subroutine ids_deserialize

function generate_tmp_file() result(fname)
  character(len=:), allocatable :: fname
  ! Follow same approach as the Python standard library in generating a random temporary file
  character(len=*), parameter :: fs_safe_characters = 'abcdefghijklmnopqrstuvwxyz0123456789_'
  integer, parameter :: n = 8 ! number of random characters in the file
  integer, parameter :: MAX_TMP_FILES = 1000

  ! On Windows and Mac OSX, use the current working directory as temporary directory (since /dev/shm does not exist).
  ! On any recent Linux (2.6 or later according to Wikipedia [1]) the /dev/shm folder exists for shared memory.
  ! Since glibc assumes this to exist anyway [2], we will as well.
  ! [1] https://en.wikipedia.org/wiki/Shared_memory
  ! [2] https://www.kernel.org/doc/Documentation/filesystems/tmpfs.txt
#if defined(_Linux)
#  define SERIALIZE_TEMPORARY_DIRECTORY '/dev/shm/'
#else
#  define SERIALIZE_TEMPORARY_DIRECTORY ''
#endif

  real, dimension(n) :: rd
  integer :: string_base_length
  integer :: i, j, k
  integer :: unit ! Unit number to open file with
  integer :: iostat
  integer :: ipid
  integer :: TMP_DIR_SIZE
  character(10) :: cpid
  CHARACTER(len=255) :: BUFFER_IMAS_AL_SERIALIZER_TMP_DIR
  CHARACTER(len=:), ALLOCATABLE :: IMAS_AL_SERIALIZER_TMP_DIR

  ipid = getpid()
  ! Convert to characters, using I0 to left-justify without leading 0s
  write(cpid, '(I0)') ipid

  ! Setup the base of the filename
  CALL get_environment_variable("IMAS_AL_SERIALIZER_TMP_DIR", BUFFER_IMAS_AL_SERIALIZER_TMP_DIR)
  TMP_DIR_SIZE = LEN_TRIM(BUFFER_IMAS_AL_SERIALIZER_TMP_DIR)
  if(TMP_DIR_SIZE > 0) then
    if(BUFFER_IMAS_AL_SERIALIZER_TMP_DIR(LEN_TRIM(BUFFER_IMAS_AL_SERIALIZER_TMP_DIR):LEN_TRIM(BUFFER_IMAS_AL_SERIALIZER_TMP_DIR)) /= "/") then
      BUFFER_IMAS_AL_SERIALIZER_TMP_DIR = TRIM(BUFFER_IMAS_AL_SERIALIZER_TMP_DIR) // "/"
    end if
    allocate(character(TMP_DIR_SIZE):: IMAS_AL_SERIALIZER_TMP_DIR)
    IMAS_AL_SERIALIZER_TMP_DIR = trim(BUFFER_IMAS_AL_SERIALIZER_TMP_DIR)
    string_base_length = len(IMAS_AL_SERIALIZER_TMP_DIR) + len('al_serialize_') + len_trim(cpid) + 1
    fname = IMAS_AL_SERIALIZER_TMP_DIR // 'al_serialize_' // trim(cpid) // "_"  // repeat(' ', n) ! implicitly allocates to the right size
  else
  string_base_length = len(SERIALIZE_TEMPORARY_DIRECTORY) + len('al_serialize_') + len_trim(cpid) + 1
  fname = SERIALIZE_TEMPORARY_DIRECTORY // 'al_serialize_' // trim(cpid) // "_"  // repeat(' ', n) ! implicitly allocates to the right size
  endif
  ! get a free unit number
  unit = get_file_unit()

  do i=1,MAX_TMP_FILES
    call random_number(rd)
    do j=1,n
      k = ceiling(rd(j)*len(fs_safe_characters))
      fname(string_base_length + j:string_base_length + j) = fs_safe_characters(k:k)
    end do

    open(unit=unit, action='write', file=fname, status='new', iostat=iostat)
    if (iostat .gt. 0) cycle

    ! if we get here the file was opened successfully. Delete it, and return the filename found
    close(unit=unit, status='delete')
    return ! implies fname
  end do
  fname = ''
end function generate_tmp_file

function get_file_unit() result(unit)
  ! Get a free file unit description number without using Fortran 2008 newunit feature.
  integer :: unit, iostat
  logical :: opened

  do unit = 97,1,-1
    inquire (unit=unit, opened=opened, iostat=iostat)
    if (iostat .ne. 0) cycle
    if (.not. opened) exit
  end do
end function get_file_unit

end module
</xsl:result-document>

<xsl:apply-templates select="/IDSs/utilities" mode="deallocate_struct"/> 
<xsl:apply-templates select="IDS" mode="deallocate_struct"/> 

<xsl:apply-templates select="/IDSs/utilities" mode="copy_struct"/> 
<xsl:apply-templates select="IDS" mode="copy_struct"/> 

<xsl:apply-templates select="/IDSs/utilities" mode="put_struct"/> 
<xsl:apply-templates select="IDS" mode="put_struct"/> 

<xsl:apply-templates select="/IDSs/utilities" mode="put_slice_struct"/> 
<xsl:apply-templates select="IDS" mode="put_slice_struct"/> 

<xsl:apply-templates select="/IDSs/utilities" mode="get_struct"/> 
<xsl:apply-templates select="IDS" mode="get_struct"/> 

<xsl:apply-templates select="IDS" mode="get_slice_struct"/> 

<xsl:apply-templates select="IDS" mode="delete"/>

<xsl:apply-templates select="/IDSs/utilities" mode="VALIDATE_UTILITIES"/>
<xsl:apply-templates select="IDS" mode="validate_struct"/> 

</xsl:template>




<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<!-- IDS_DELETE MODULE, PER IDS                                              -->
<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<xsl:template match="IDS" mode="delete">
  <xsl:result-document href="{@name}_delete.f90">
module <xsl:value-of select="@name"/>_delete

! Declaration of the generic IDS DELETE routine
interface ids_delete
  module procedure ids_delete_<xsl:value-of select="local:unique_name(@name)"/> <!-- subroutine for the whole IDS -->
end interface 

contains

!!!!!! Routine to DELETE the IDS
subroutine ids_delete_<xsl:value-of select="local:unique_name(@name)"/>(pulsectx, IDSpath, IDS, retstatus)  <!-- systematic calls to the low level delete_data routine. The IDS input argument is added just for the interface to identify the relevant IDS type -->
  use ids_schemas_<xsl:value-of select="@name"/>
  use al_low_level_wrap
  use al_put_policy
  implicit none
  character*(*) :: IDSpath
  <!-- Optional, so every existing caller of the generic ids_delete keeps
       compiling unchanged across all 82 IDSs, and so this routine can keep the
       STOP it had for a caller that asked for no status. Same shape as
       ids_put's and ids_get's retstatus for the same reason. -->
  integer(ids_int), intent(out), optional :: retstatus
  integer(ids_int) :: pulsectx, opctx, status
  type(ids_<xsl:value-of select="@name"/>) :: IDS

  <!-- Before the traversal, so the count describes this delete. ids_put calls
       this routine and reads the count afterwards; see al_put_policy for why
       the delete phase resets its own counter rather than sharing ids_put's
       reset, which happens after the delete. -->
  call al_reset_refused_deletes()

  call al_begin_global_action(pulsectx, IDSpath, WRITE_OP, opctx, status)
  if (status.ne.0) then
     write(*,*) 'Error in al_begin_global_action (from ids_delete for IDS <xsl:value-of select="@name"/>)'
     <!-- Not a tolerable refusal even when it is one. An interposing layer
          returns the refusal code here for a malformed version stamp or a
          version-latch conflict, and tolerating it would run the traversal
          against an occurrence that was never opened, which is the site both
          policy modules name as barred. So the status is reported, not absorbed. It
          is reported rather than fatal only where the caller asked for it:
          this routine used to STOP unconditionally, which took the calling
          program down for a refusal ids_put could have returned. -->
     if (present(retstatus)) then
        retstatus = status
     else
        STOP 'Error in al_begin_global_action (from ids_delete for IDS <xsl:value-of select="@name"/>)'
     end if
     return
  end if

  <xsl:apply-templates select="field" mode="DELETE"/>

  call al_end_action(opctx,status)

  <!-- A refused delete leaves stored data the traversal asked to remove, so
       the occurrence is not what the caller asked for even though every call
       the layer accepted succeeded. PARTIAL_PUT says so: positive, so it can
       never be confused with a C-ABI status, and it still trips the
       `status.ne.0` test callers already write. Without this the refusal is
       recorded on stdout by al_note_refused_delete and nowhere a program can
       reach. -->
  if (present(retstatus)) then
     if (status == 0 .and. al_get_refused_delete_count() > 0) then
        retstatus = PARTIAL_PUT
     else
        retstatus = status
     end if
  end if

end subroutine ids_delete_<xsl:value-of select="local:unique_name(@name)"/>

<!-- One delete site, with the layer's refusal recorded instead of discarded.
     A subroutine rather than two statements per site because there are 7757 of
     them in the generated tree and the path literal would otherwise be emitted
     twice at each one. Named per IDS like the routine above, because
     ids_routines uses all 82 of these modules in one scope. -->
!!!!!! One field's delete, recording a refusal the layer declared
subroutine delete_field_<xsl:value-of select="local:unique_name(@name)"/>(opctx, path, status)
  use al_low_level_wrap
  use al_put_policy
  implicit none
  integer, intent(in) :: opctx
  character*(*), intent(in) :: path
  integer, intent(out) :: status

  call al_delete_data(opctx, path, status)
  <!-- Only a refusal is recorded. Every other non-zero status is discarded
       exactly as before: this traversal deletes all 7757 paths of the IDS
       whether or not the occurrence holds them, so an ordinary not-found is
       the normal case here and has always been ignored. Changing that is a
       separate question from making a refusal visible. -->
  if (is_external_refusal(status)) then
     call al_note_refused_delete(path, status)
  end if

end subroutine delete_field_<xsl:value-of select="local:unique_name(@name)"/>

end module <xsl:value-of select="@name"/>_delete
  </xsl:result-document>
</xsl:template>




<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<!-- IDS_DEALLOCATE MODULE, UTILITIES                                        -->
<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<xsl:template match="utilities" mode="deallocate_struct">
  <xsl:result-document href="utilities_deallocate_struct.f90">
module utilities_deallocate_struct

interface ids_deallocate_struct
  <xsl:for-each select="/IDSs/utilities/field[@data_type='structure' or @data_type='struct_array']">
  <xsl:variable name="this-type">
    <xsl:choose>
      <xsl:when test="@structure_reference='self'">
        <xsl:value-of select="@name"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="@structure_reference"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  module procedure ids_deallocate_struct_<xsl:value-of select="local:unique_name($this-type)"/>
  </xsl:for-each>
end interface

 contains

<xsl:for-each select="/IDSs/utilities//field[@data_type='structure' or @data_type='struct_array']">
  <xsl:variable name="this-name" select="@name"/>
  <xsl:variable name="this-type">
    <xsl:choose>
      <xsl:when test="@structure_reference='self'">
        <xsl:value-of select="@name"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="@structure_reference"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:if test="generate-id() = generate-id(key('unique-structures-by-type', $this-type)[1])">

subroutine ids_deallocate_struct_<xsl:value-of select="local:unique_name($this-type)"/>(struct_in,  c_data)
  use al_low_level_wrap
  use, intrinsic :: ISO_C_BINDING, only: C_LOC
  use ids_types
  use ids_utilities, only: ids_<xsl:value-of select="$this-type"/>
  implicit none

  integer(ids_int) :: i
  logical, intent(in) :: c_data
  type(ids_<xsl:value-of select="$this-type"/>) :: struct_in

  <xsl:apply-templates select="./field" mode="DEALLOCATE_FIELD">
    <xsl:with-param name="idxpath" select="''"/>
  </xsl:apply-templates>
end subroutine ids_deallocate_struct_<xsl:value-of select="local:unique_name($this-type)"/>

  </xsl:if>
</xsl:for-each>

end module    
  </xsl:result-document>
</xsl:template>



<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<!-- IDS_DEALLOCATE MODULE, PER IDS                                          -->
<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<xsl:template match="IDS" mode="deallocate_struct">
  <xsl:result-document href="{@name}_deallocate_struct.f90">
module <xsl:value-of select="@name"/>_deallocate_struct

use utilities_deallocate_struct

interface ids_deallocate
  module procedure ids_deallocate_struct_<xsl:value-of select="local:unique_name(@name)"/> <!-- subroutine for the whole IDS -->
end interface 

interface ids_deallocate_struct
  <xsl:for-each select=".//field[@data_type='structure' or @data_type='struct_array']">
    <xsl:variable name="this-name" select="@name"/>
    <xsl:variable name="this-type">
      <xsl:choose>
        <xsl:when test="@structure_reference='self'">
          <xsl:value-of select="@name"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="@structure_reference"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="this-ids" select="ancestor::IDS/@name"/>
    <xsl:if test="not (preceding::field[@structure_reference=$this-type and ancestor::IDS/@name=$this-ids] or /IDSs/utilities/field/@name=$this-type)">
  module procedure ids_deallocate_struct_<xsl:value-of select="local:unique_name($this-type)"/>
    </xsl:if>
  </xsl:for-each>
end interface

 contains 

<!-- subroutine for the whole IDS -->
subroutine ids_deallocate_struct_<xsl:value-of select="local:unique_name(@name)"/>(struct_in)
  use al_low_level_wrap
  use, intrinsic :: ISO_C_BINDING, only: C_LOC
  use ids_schemas_<xsl:value-of select="@name"/>
  implicit none

  integer(ids_int) :: i
  logical :: c_data
  type(ids_<xsl:value-of select="@name"/>) :: struct_in

  call is_c_data(struct_in, c_data)

  <xsl:apply-templates select="./field" mode="DEALLOCATE_FIELD">
    <xsl:with-param name="idxpath" select="''"/>
  </xsl:apply-templates>
  call set_c_data(struct_in, .FALSE.)
end subroutine ids_deallocate_struct_<xsl:value-of select="local:unique_name(@name)"/>

<xsl:for-each select=".//field[@data_type='structure' or @data_type='struct_array']">
  <xsl:variable name="this-name" select="@name"/>
  <xsl:variable name="this-type">
    <xsl:choose>
      <xsl:when test="@structure_reference='self'">
        <xsl:value-of select="@name"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="@structure_reference"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:variable name="this-ids" select="ancestor::IDS/@name"/>
  <xsl:if test="not (preceding::field[@structure_reference=$this-type and ancestor::IDS/@name=$this-ids] or /IDSs/utilities/field/@name=$this-type)">

subroutine ids_deallocate_struct_<xsl:value-of select="local:unique_name($this-type)"/>(struct_in,  c_data)
  use al_low_level_wrap
  use, intrinsic :: ISO_C_BINDING, only: C_LOC
  use ids_schemas_<xsl:value-of select="ancestor::IDS/@name"/>
  implicit none

  integer(ids_int) :: i
  logical, intent(in) :: c_data
  type(ids_<xsl:value-of select="$this-type"/>) :: struct_in

  <xsl:apply-templates select="./field" mode="DEALLOCATE_FIELD">
    <xsl:with-param name="idxpath" select="''"/>
  </xsl:apply-templates>
end subroutine ids_deallocate_struct_<xsl:value-of select="local:unique_name($this-type)"/>
  </xsl:if>
</xsl:for-each>

end module    
  </xsl:result-document>
</xsl:template>



<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<!-- IDS_COPY MODULE, UTILITIES                                              -->
<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<xsl:template match="utilities" mode="copy_struct">
  <xsl:result-document href="utilities_copy_struct.f90">
module utilities_copy_struct

interface ids_copy
  <xsl:for-each select="/IDSs/utilities/field[@data_type='structure' or @data_type='struct_array']">
  <xsl:variable name="this-type">
    <xsl:choose>
      <xsl:when test="@structure_reference='self'">
        <xsl:value-of select="@name"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="@structure_reference"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  module procedure ids_copy_struct_<xsl:value-of select="local:unique_name($this-type)"/>
  </xsl:for-each>
end interface    

 contains

<xsl:for-each select="/IDSs/utilities//field[@data_type='structure' or @data_type='struct_array']">
  <xsl:variable name="this-name" select="@name"/>
  <xsl:variable name="this-type">
    <xsl:choose>
      <xsl:when test="@structure_reference='self'">
        <xsl:value-of select="@name"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="@structure_reference"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:if test="not (preceding::field[@structure_reference=$this-type or @name=$this-type])">

subroutine ids_copy_struct_<xsl:value-of select="local:unique_name($this-type)"/>(struct_in,  struct_out)
  ! Copies all fields of struct_in to struct_out
  ! Assumes that struct_in is a single instance of a given structure
  use ids_types
  use ids_utilities, only: ids_<xsl:value-of select="$this-type"/>
  implicit none

  integer(ids_int) :: i

  type(ids_<xsl:value-of select="$this-type"/>) :: struct_in, struct_out

  <xsl:apply-templates select="./field" mode="COPY_FIELD">
    <xsl:with-param name="idxpath" select="''"/>
  </xsl:apply-templates>
  return
end subroutine ids_copy_struct_<xsl:value-of select="local:unique_name($this-type)"/>
  </xsl:if>
</xsl:for-each>

end module    
  </xsl:result-document>  
</xsl:template>



<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<!-- IDS_COPY MODULE, PER IDS                                                -->
<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<xsl:template match="IDS" mode="copy_struct">
  <xsl:result-document href="{@name}_copy_struct.f90">
module <xsl:value-of select="@name"/>_copy_struct

use utilities_copy_struct

interface ids_copy 
  module procedure ids_copy_struct_<xsl:value-of select="local:unique_name(@name)"/>
  <xsl:for-each select=".//field[@data_type='structure' or @data_type='struct_array']">
    <xsl:variable name="this-name" select="@name"/>
    <xsl:variable name="this-type">
      <xsl:choose>
        <xsl:when test="@structure_reference='self'">
          <xsl:value-of select="@name"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="@structure_reference"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <!--<xsl:variable name="this-type" select="@structure_reference"/>-->
    <xsl:variable name="this-ids" select="ancestor::IDS/@name"/>
    <xsl:if test="not (preceding::field[@structure_reference=$this-type and ancestor::IDS/@name=$this-ids] or /IDSs/utilities/field/@name=$this-type)">
  module procedure ids_copy_struct_<xsl:value-of select="local:unique_name($this-type)"/> 
    </xsl:if>
  </xsl:for-each>
end interface 

 contains

<!-- subroutine for the whole IDS -->
subroutine ids_copy_struct_<xsl:value-of select="local:unique_name(@name)"/>(struct_in, struct_out)
  ! Copies all fields of struct_in to struct_out
  ! Assumes that struct_in is a single instance of a given structure
  use ids_schemas_<xsl:value-of select="@name"/>
  implicit none

  integer(ids_int) :: i

  type(ids_<xsl:value-of select="@name"/>) :: struct_in, struct_out

  <xsl:apply-templates select="./field" mode="COPY_FIELD">
    <xsl:with-param name="idxpath" select="''"/>
  </xsl:apply-templates>
  return
end subroutine ids_copy_struct_<xsl:value-of select="local:unique_name(@name)"/>

<xsl:for-each select=".//field[@data_type='structure' or @data_type='struct_array']">
  <xsl:variable name="this-name" select="@name"/>
  <xsl:variable name="this-type">
    <xsl:choose>
      <xsl:when test="@structure_reference='self'">
        <xsl:value-of select="@name"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="@structure_reference"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:variable name="this-ids" select="ancestor::IDS/@name"/>
  <xsl:if test="not (preceding::field[@structure_reference=$this-type and ancestor::IDS/@name=$this-ids] or /IDSs/utilities/field/@name=$this-type)">

subroutine ids_copy_struct_<xsl:value-of select="local:unique_name($this-type)"/>(struct_in,  struct_out)
  ! Copies all fields of struct_in to struct_out
  ! Assumes that struct_in is a single instance of a given structure
  use ids_schemas_<xsl:value-of select="ancestor::IDS/@name"/>
  implicit none

  integer(ids_int) :: i

  type(ids_<xsl:value-of select="$this-type"/>) :: struct_in, struct_out

  <xsl:apply-templates select="./field" mode="COPY_FIELD">
    <xsl:with-param name="idxpath" select="''"/>
  </xsl:apply-templates>
  return
end subroutine ids_copy_struct_<xsl:value-of select="local:unique_name($this-type)"/>
  </xsl:if>
</xsl:for-each>

end module    
  </xsl:result-document>
</xsl:template>



<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<!-- IDS_PUT MODULE, UTILITIES                                               -->
<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<xsl:template match="utilities" mode="put_struct">
  <xsl:result-document href="utilities_put_struct.f90">
module utilities_put_struct

<!-- Used at module level, not inside one routine: both isErrorCritical and the
     failure arm of al_begin_arraystruct_action consult the write-side refusal
     policy, and the second is emitted into the put routines below rather than
     into that function. -->
use al_put_policy

interface ids_put_struct
  <xsl:for-each select="/IDSs/utilities/field[@data_type='structure' or @data_type='struct_array']">  
    <xsl:variable name="this-type">
      <xsl:choose>
        <xsl:when test="@structure_reference='self'">
          <xsl:value-of select="@name"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="@structure_reference"/>
        </xsl:otherwise>
      </xsl:choose>
      </xsl:variable>
      module procedure put_struct_ids_<xsl:value-of select="local:unique_name($this-type)"/>
  </xsl:for-each>
end interface

 contains

<xsl:call-template name="isCriticalFuncCtx">
  <xsl:with-param name="method" select="'put'"/>
</xsl:call-template>

<xsl:for-each select="/IDSs/utilities//field[@data_type='structure' or @data_type='struct_array']">
  <xsl:variable name="this-name" select="@name"/>
  <xsl:variable name="this-type">
    <xsl:choose>
      <xsl:when test="@structure_reference='self'">
        <xsl:value-of select="@name"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="@structure_reference"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:if test="not (preceding::field[@structure_reference=$this-type or @name=$this-type])">

subroutine put_struct_ids_<xsl:value-of select="local:unique_name($this-type)"/>(ctx, name, path, struct, timemode, timedparent, retstatus)
  use ids_types
  use ids_utilities, only: ids_<xsl:value-of select="$this-type"/>
  use al_low_level_wrap
  implicit none

  integer(ids_int), intent(in) :: ctx
  character*(*), intent(in) :: name, path
  type(ids_<xsl:value-of select="$this-type"/>), intent(inout) :: struct
  logical, intent(in) :: timedparent
  integer, intent(in) :: timemode
  integer(ids_int), intent(out) :: retstatus
  integer(ids_int) :: i, aoslen, aos_hli_len, lenstring, aosctx, lastdimsize
  integer :: status
  character(len=100000) :: longstring
  character(len=300) :: timepath

  <xsl:choose>
    <xsl:when test="$this-type='version_dd_al'">
      <xsl:apply-templates select="./field" mode="PUT_FIELD">
        <xsl:with-param name="structvar" select="'struct'"/>
        <xsl:with-param name="contextvar" select="'ctx'"/>
        <xsl:with-param name="timedparentexpr" select="'timedparent.or.'"/>
        <xsl:with-param name="provenance" select="'DD/AL/LANG'"/>
      </xsl:apply-templates>
    </xsl:when>
    <xsl:otherwise>
      <xsl:apply-templates select="./field" mode="PUT_FIELD">
        <xsl:with-param name="structvar" select="'struct'"/>
        <xsl:with-param name="contextvar" select="'ctx'"/>
        <xsl:with-param name="timedparentexpr" select="'timedparent.or.'"/>
      </xsl:apply-templates>
    </xsl:otherwise>
  </xsl:choose>
   retstatus = 0
end subroutine put_struct_ids_<xsl:value-of select="local:unique_name($this-type)"/>

  </xsl:if>
</xsl:for-each>

end module 
  </xsl:result-document>
</xsl:template>


<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<!-- IDS_PUT MODULE, PER IDS                                                 -->
<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<xsl:template match="IDS" mode="put_struct">
  <xsl:result-document href="{@name}_put.f90">
module <xsl:value-of select="@name"/>_put_struct

use utilities_put_struct

<!-- Used at module level, not inside one routine: both isErrorCritical and the
     failure arm of al_begin_arraystruct_action consult the write-side refusal
     policy, and the second is emitted into the put routines below rather than
     into that function. -->
use al_put_policy

interface ids_put
  module procedure put_struct_ids_<xsl:value-of select="local:unique_name(@name)"/> <!-- subroutine for the whole IDS -->
end interface

interface ids_put_struct
  <xsl:for-each select=".//field[@data_type='structure' or @data_type='struct_array']">
    <xsl:variable name="this-type">
      <xsl:choose>
        <xsl:when test="@structure_reference='self'">
          <xsl:value-of select="@name"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="@structure_reference"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="this-ids" select="ancestor::IDS/@name"/>
    <xsl:if test="not (preceding::field[@structure_reference=$this-type and ancestor::IDS/@name=$this-ids] or /IDSs/utilities/field/@name=$this-type)">
  module procedure put_struct_ids_<xsl:value-of select="local:unique_name($this-type)"/>
    </xsl:if>
  </xsl:for-each>
end interface

 contains 

<!-- <xsl:call-template name="isCriticalFuncCtx"/> done in utilities! -->

<!-- subroutine for the whole IDS -->
!!! Routines to PUT the full IDS !!!
subroutine put_struct_ids_<xsl:value-of select="local:unique_name(@name)"/>(pulsectx, name, IDS, retstatus, verbose)
  use ids_schemas_<xsl:value-of select="@name"/>
  use <xsl:value-of select="@name"/>_validate_struct
  use al_low_level_wrap
  use <xsl:value-of select="@name"/>_delete
  <!--<xsl:if test="@specific_validation_rules='yes'">
         use specific_validate_struct
  </xsl:if>-->
  implicit none

  integer(ids_int), optional, intent(out) :: retstatus 
  logical, optional, intent(in) :: verbose
  integer(ids_int) :: status = 0
  <!--<xsl:if test="@specific_validation_rules='yes'">
         integer(ids_int) :: validationstatus = 0
  </xsl:if>-->
  character*(*), intent(in) :: name
  integer(ids_int) :: pulsectx, opctx, aosctx
  type(ids_<xsl:value-of select="@name"/>) :: IDS
  ! internal variables declaration
  logical :: timedparent
  integer :: timemode
  integer(ids_int) :: aoslen, aos_hli_len, i, lenstring, lastdimsize
  character(len=100000) :: longstring
  character(len=300) :: timepath
  character(*), parameter :: path = ''
  integer(ids_int) :: validation_status
  integer(ids_int) :: deletestatus
  character(:), allocatable :: err_msg
  character(len=1) :: buffer

  ! Automatic validation of the data (if enabled)
  CALL get_environment_variable("IMAS_AL_DISABLE_VALIDATE", buffer)
  if (len_trim(buffer)==0 .or. buffer .eq. '0') then
    call ids_validate(IDS, validation_status, err_msg)
    if(validation_status == -1) then
      if (.not.present(verbose) .OR. (present(verbose) .AND. verbose)) then
        write(*,*) "Error during automatic validation before put of <xsl:value-of select="@name"/>"
        write(*,*) err_msg
      end if 
      if (present(retstatus)) retstatus = CONSISTENCY_ERR
      return
    end if
  end if 

  call IDS%check_name_<xsl:value-of select="@name"/>(name, status)
  if(status.ne.0) then
    write(*,*) 'Error in put_struct_ids_<xsl:value-of select="local:unique_name(@name)"/>: the IDS name ',name,' is invalid, it expects <xsl:value-of select="@name"/>'
    if(present(retstatus)) retstatus = status
    return
  end if 

  ! Systematic delete of the previous IDS, in case it existed
  <!-- Asked for a status, which this call could not report before. Two
       outcomes are now distinguishable: a refused delete, which leaves stored
       data behind and makes this put partial (folded into the derivation at
       the end of the routine, where the write refusals are), and a failure to
       open the occurrence at all, which is fatal to the operation. -->
  call ids_delete(pulsectx, name, IDS, deletestatus)
  if (deletestatus.lt.0) then
     <!-- The occurrence was never opened for the delete, so the previous
          contents are entirely intact and the traversal below would write
          into an occurrence still holding them. Return the real status rather
          than proceeding; before this call reported anything, ids_delete took
          the whole program down here instead. -->
     write(*,*) 'Error deleting the previous IDS <xsl:value-of select="@name"/> before put'
     if (present(retstatus)) then
        retstatus = deletestatus
     else
        STOP
     end if
     return
  end if

  if (IDS%ids_properties%homogeneous_time.EQ.IDS_TIME_MODE_UNKNOWN) then
     write(*,*) "Warning : <xsl:value-of select="@name"/> is found to be EMPTY (homogeneous_time undefined). PUT returns with no action."
     <!-- "No action" is not quite true once the delete above has run: an
          empty IDS is still put by clearing the occurrence, and a refused
          delete means it was not cleared. Report that rather than 0. -->
     if (present(retstatus)) retstatus = deletestatus
     return
  endif

<xsl:if test="@type='constant'">
  if (IDS%ids_properties%homogeneous_time.NE.IDS_TIME_MODE_INDEPENDENT ) then
 
    IDS%ids_properties%homogeneous_time = IDS_TIME_MODE_INDEPENDENT
    write(*,*) "AL warning: ids_properties/homogeneous_time has been set to IDS_TIME_MODE_INDEPENDENT for the constant IDS '", name, &amp;
    " Please check the program which has filled this IDS since this is the mandatory value for a constant IDS."
  endif
</xsl:if>

  timemode = IDS%ids_properties%homogeneous_time

  <!--<xsl:if test="@specific_validation_rules='yes'">
         call ids_validate(IDS, validationstatus)
  if (validationstatus.EQ.-1) then
     write(*,*) "PUT operation stopped"
     return
  endif
  </xsl:if>-->
  
  call al_begin_global_action(pulsectx, name, WRITE_OP, opctx, status) 
  if (status.ne.0) then
     write(*,*) 'Error in al_begin_global_action (from ids_put for IDS <xsl:value-of select="@name"/>)'
     <!-- The begin failed, so no context was created: there is nothing to end,
          and opctx holds no defined value. Return the real status, and return
          *here*: falling through ran the whole field block against a context
          that was never opened. -->
     if (present(retstatus)) then
        retstatus = status
     else
        STOP
     end if
     return
  end if

  timedparent=.false.
  <!-- The refused-write count describes one operation, not the process. -->
  call al_reset_refused_writes()

  <xsl:apply-templates select="./field" mode="PUT_FIELD">
    <xsl:with-param name="structvar" select="'IDS'"/>
    <xsl:with-param name="contextvar" select="'opctx'"/>
    <xsl:with-param name="timedparentexpr" select="''"/>
  </xsl:apply-templates>
  
  call al_write_plugins_metadata(opctx, status)
  if (status.ne.0) then
     write(*,*) 'Error in al_write_plugins_metadata (from ids_put for IDS <xsl:value-of select="@name"/>)'
     <!-- opctx was opened by this routine and is still open, so end it before
          returning. Return the real status, not opctx: that is a context id,
          not a status. Falling through ran the rest of the routine against a
          context whose operation had already failed. -->
     if (present(retstatus)) then
        retstatus = status
     else
        STOP
     end if
     call al_end_action(opctx, status)
     return
  end if
  call al_end_action(opctx, status)

  <!-- retstatus here is al_end_action's status, not the write's. If the
       interposing layer refused any path, the caller's value for it was dropped
       and the occurrence does not hold everything they supplied. Say so:
       PARTIAL_PUT is positive, so it can never be confused with a C-ABI status,
       and it still trips the `status.ne.0` test callers already write. Without
       this the tolerance of a refused write would be silent, which is the one
       outcome al_put_policy exists to prevent. -->
  <!-- Both phases of the operation count. A refused write dropped a value the
       caller supplied; a refused delete left one they did not. Either makes
       the occurrence something other than what they asked for, and CONTEXT.md
       derives a partial outcome from refusals during the operation the caller
       asked for, which is this whole routine, delete included. The delete
       count is still the one ids_delete recorded a few lines up: nothing
       between resets it, and al_reset_refused_writes above touches only the
       write counter. -->
  if (present(retstatus)) then
     if (status == 0 .and. (al_get_refused_write_count() > 0 .or. al_get_refused_delete_count() > 0)) then
        retstatus = PARTIAL_PUT
     else
        retstatus = status
     end if
  end if
end subroutine put_struct_ids_<xsl:value-of select="local:unique_name(@name)"/>

<xsl:for-each select=".//field[@data_type='structure' or @data_type='struct_array']">
  <xsl:variable name="this-type">
    <xsl:choose>
      <xsl:when test="@structure_reference='self'">
        <xsl:value-of select="@name"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="@structure_reference"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:variable name="this-ids" select="ancestor::IDS/@name"/>
  <xsl:if test="not (preceding::field[@structure_reference=$this-type and ancestor::IDS/@name=$this-ids] or /IDSs/utilities/field/@name=$this-type)">
subroutine put_struct_ids_<xsl:value-of select="local:unique_name($this-type)"/>(ctx, name, path, struct, timemode, timedparent, retstatus)
  use ids_schemas_<xsl:value-of select="ancestor::IDS/@name"/>
  use al_low_level_wrap
  implicit none

  integer(ids_int), intent(in) :: ctx
  character*(*), intent(in) :: name, path
  type(ids_<xsl:value-of select="$this-type"/>), intent(inout) :: struct      
  logical, intent(in) :: timedparent
  integer, intent(in) :: timemode 
  integer(ids_int), intent(out) :: retstatus
  integer(ids_int) :: i, aoslen, aos_hli_len, lenstring, aosctx, lastdimsize
  integer :: status
  character(len=100000) :: longstring
  character(len=300) :: timepath

  <xsl:apply-templates select="./field" mode="PUT_FIELD">
    <xsl:with-param name="structvar" select="'struct'"/>
    <xsl:with-param name="contextvar" select="'ctx'"/>
    <xsl:with-param name="timedparentexpr" select="'timedparent.or.'"/>
  </xsl:apply-templates>      
   retstatus = 0
end subroutine put_struct_ids_<xsl:value-of select="local:unique_name($this-type)"/>

  </xsl:if>
</xsl:for-each>

end module    
  </xsl:result-document>
</xsl:template>




<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<!-- IDS_PUT_SLICE MODULE, UTILITIES                                         -->
<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<xsl:template match="utilities" mode="put_slice_struct">
  <xsl:result-document href="utilities_put_slice_struct.f90">
module utilities_put_slice_struct

<!-- Used at module level, not inside one routine: both isErrorCritical and the
     failure arm of al_begin_arraystruct_action consult the write-side refusal
     policy, and the second is emitted into the put routines below rather than
     into that function. -->
use al_put_policy

interface ids_put_slice_struct
  <xsl:for-each select="/IDSs/utilities/field[@data_type='structure' or @data_type='struct_array']">  
    <xsl:variable name="this-type">
      <xsl:choose>
        <xsl:when test="@structure_reference='self'">
          <xsl:value-of select="@name"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="@structure_reference"/>
        </xsl:otherwise>
      </xsl:choose>
      </xsl:variable>
      module procedure put_slice_struct_ids_<xsl:value-of select="local:unique_name($this-type)"/>
  </xsl:for-each>
end interface

 contains

<xsl:call-template name="isCriticalFuncCtx">
  <xsl:with-param name="method" select="'put'"/>
</xsl:call-template>

<xsl:for-each select="/IDSs/utilities//field[@data_type='structure' or @data_type='struct_array']">
  <xsl:variable name="this-name" select="@name"/>
  <xsl:variable name="this-type">
    <xsl:choose>
      <xsl:when test="@structure_reference='self'">
        <xsl:value-of select="@name"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="@structure_reference"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:if test="not (preceding::field[@structure_reference=$this-type or @name=$this-type])">

subroutine put_slice_struct_ids_<xsl:value-of select="local:unique_name($this-type)"/>(ctx, name, path, struct, timemode, timedparent, retstatus)
  use ids_types
  use ids_utilities, only: ids_<xsl:value-of select="$this-type"/>
  use al_low_level_wrap
  implicit none

  integer(ids_int), intent(in) :: ctx
  character*(*), intent(in) :: name, path
  type(ids_<xsl:value-of select="$this-type"/>), intent(inout) :: struct
  logical, intent(in) :: timedparent
  integer, intent(in) :: timemode
  integer(ids_int), intent(out) :: retstatus
  integer(ids_int) :: i, aoslen, aos_hli_len, lenstring, aosctx, lastdimsize
  integer :: status
  character(len=100000) :: longstring
  character(len=300) :: timepath

  <xsl:apply-templates select="./field" mode="PUT_FIELD">
    <xsl:with-param name="structvar" select="'struct'"/>
    <xsl:with-param name="contextvar" select="'ctx'"/>
    <xsl:with-param name="timedparentexpr" select="'timedparent.or.'"/>
    <xsl:with-param name="slice" select="'yes'"/>
  </xsl:apply-templates>      
  retstatus = 0
end subroutine put_slice_struct_ids_<xsl:value-of select="local:unique_name($this-type)"/>

  </xsl:if>
</xsl:for-each>

end module 
  </xsl:result-document>
</xsl:template>


<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<!-- IDS_PUT_SLICE MODULE, PER IDS                                           -->
<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<xsl:template match="IDS" mode="put_slice_struct">
  <xsl:result-document href="{@name}_put_slice.f90">
module <xsl:value-of select="@name"/>_put_slice_struct

use utilities_put_slice_struct

<!-- Used at module level, not inside one routine: both isErrorCritical and the
     failure arm of al_begin_arraystruct_action consult the write-side refusal
     policy, and the second is emitted into the put routines below rather than
     into that function. -->
use al_put_policy

interface ids_put_slice
  module procedure put_slice_struct_ids_<xsl:value-of select="local:unique_name(@name)"/> <!-- subroutine for the whole IDS -->
end interface 

interface ids_put_slice_struct
  <xsl:for-each select=".//field[@data_type='structure' or @data_type='struct_array']">
    <xsl:variable name="this-type">
      <xsl:choose>
        <xsl:when test="@structure_reference='self'">
          <xsl:value-of select="@name"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="@structure_reference"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="this-ids" select="ancestor::IDS/@name"/>
    <xsl:if test="not (preceding::field[@structure_reference=$this-type and ancestor::IDS/@name=$this-ids] or /IDSs/utilities/field/@name=$this-type)">
  module procedure put_slice_struct_ids_<xsl:value-of select="local:unique_name($this-type)"/>
    </xsl:if>
  </xsl:for-each>
end interface

 contains 

<!-- subroutine for the whole IDS -->
!!! Routines to PUT_SLICE one time slice of an IDS !!!
subroutine put_slice_struct_ids_<xsl:value-of select="local:unique_name(@name)"/>(pulsectx, name, IDS, retstatus, verbose)
  use ids_schemas_<xsl:value-of select="@name"/>
  use <xsl:value-of select="@name"/>_validate_struct
  use al_low_level_wrap
  use <xsl:value-of select="@name"/>_put_struct
  <!--<xsl:if test="@specific_validation_rules='yes'">
         use specific_validate_struct
  </xsl:if>-->

  implicit none

  integer(ids_int), intent(out), optional :: retstatus
  logical, optional, intent(in) :: verbose
  integer(ids_int) :: status = 0
  <!--<xsl:if test="@specific_validation_rules='yes'">
         integer(ids_int) :: validationstatus = 0
  </xsl:if>-->
  character*(*) :: name
  integer(ids_int) :: pulsectx, opctx, aosctx
  type(ids_<xsl:value-of select="@name"/>) :: IDS
  ! internal variables declaration
  logical :: timedparent
  integer :: timemode, storedtimemode
  integer(ids_int) :: aoslen, aos_hli_len, i, lenstring, lastdimsize
  character(len=100000) :: longstring
  character(len=300) :: timepath
  character(*), parameter :: path = ''
  integer(ids_int) :: validation_status
  character(:), allocatable :: err_msg
  character(len=1) :: buffer

  ! Automatic validation of the data (if enabled)
  CALL get_environment_variable("IMAS_AL_DISABLE_VALIDATE", buffer)
  if (len_trim(buffer)==0 .or. buffer .eq. '0') then
    call ids_validate(IDS, validation_status, err_msg)
    if(validation_status == -1) then
      if (.not.present(verbose) .OR. (present(verbose) .AND. verbose)) then
        write(*,*) "Error during automatic validation before put_slice of <xsl:value-of select="@name"/>"
        write(*,*) err_msg
      end if 
      if (present(retstatus)) retstatus = CONSISTENCY_ERR
      return
    end if
  end if 

  call IDS%check_name_<xsl:value-of select="@name"/>(name, status)
  if(status.ne.0) then
    write(*,*) 'Error in put_slice_struct_ids_<xsl:value-of select="local:unique_name(@name)"/>'
    if(present(retstatus)) retstatus = status
    return
  end if 

  if (IDS%ids_properties%homogeneous_time.EQ.IDS_TIME_MODE_UNKNOWN) then
     write(*,*) "Warning : <xsl:value-of select="@name"/> is found to be EMPTY (homogeneous_time undefined). PUTSLICE returns with no action."
     if (present(retstatus)) retstatus = 0
     return
  endif

<xsl:if test="@type='constant'">
  if (IDS%ids_properties%homogeneous_time.NE.IDS_TIME_MODE_INDEPENDENT ) then
 
    IDS%ids_properties%homogeneous_time = IDS_TIME_MODE_INDEPENDENT
    write(*,*) "AL warning: ids_properties/homogeneous_time has been set to IDS_TIME_MODE_INDEPENDENT for the constant IDS '", name, &amp;
    " Please check the program which has filled this IDS since this is the mandatory value for a constant IDS."
  endif
</xsl:if>

  timemode = IDS%ids_properties%homogeneous_time

  if (timemode.EQ.IDS_TIME_MODE_INDEPENDENT) then
     write(*,*) "WARNING : homogeneous_time=2 mark an IDS <xsl:value-of select="@name"/> with static/constant data only. No static data stored with put_slice operation."
     if (present(retstatus)) retstatus = 0
     return
  endif

  storedtimemode = IDS_TIME_MODE_UNKNOWN
  call al_begin_global_action(pulsectx, name, READ_OP, opctx, status) 
  if (status.ne.0) then
     !! error when trying to get new ctx => stop!
     write(*,*) 'Error in al_begin_slice_action (from ids_put_slice for IDS <xsl:value-of select="@name"/>)'     
     if (present(retstatus)) then
        retstatus = status
     else
        STOP 
     end if
  else
     ! Get homogeneous_time to check consistency
     call get_int(opctx, "ids_properties/homogeneous_time",&amp;
     '', storedtimemode, status)
     <!-- Deliberately not isErrorCritical. That predicate now tolerates an
          interposing layer's refusal, and this site is not a per-field write:
          it is a *read*, and its result is what chooses between writing a slice
          and delegating to a full ids_put. A tolerated refusal here would leave
          storedtimemode at IDS_TIME_MODE_UNKNOWN, and ids_put opens by deleting
          the occurrence, so one refused metadata read would turn this
          put_slice into a delete-and-rewrite of the whole IDS. Routing it
          through the write policy would also record a refused *write* for a
          call that was writing nothing. al_put_policy carries the derived site
          list this exception belongs to. -->
     if (status.ne.0) then
        write(*,*) "ERROR! with field 'ids_properties/homogeneous_time'"
        if (present(retstatus)) then
           retstatus = status
           return
        else
           STOP
        endif
     endif
     call al_end_action(opctx, status)
  endif

  if (storedtimemode.eq.IDS_TIME_MODE_UNKNOWN) then
     call put_struct_ids_<xsl:value-of select="local:unique_name(@name)"/>(pulsectx, name, IDS, status)
     if (present(retstatus)) retstatus = status
     return
  else if (storedtimemode.ne.timemode) then
     write(*,'(a,i0,a,i0)') 'ERROR: IDS <xsl:value-of select="@name"/> homogeneous_time mode = ',timemode,&amp;
     ', differs from value already stored in database = ',storedtimemode
     if (present(retstatus)) then 
        retstatus = -1
        return
     else
        STOP
     endif
  endif

  call al_begin_slice_action(pulsectx, name, WRITE_OP, UNDEFINED_TIME, UNDEFINED_INTERP, opctx, status)
  if (status.ne.0) then
     !! error when trying to get new ctx => stop!
     write(*,*) 'Error in al_begin_slice_action (from ids_put_slice for IDS <xsl:value-of select="@name"/>)'     
     <!-- The begin failed, so no context was created: there is nothing to end,
          and opctx holds no defined value. Return the real status, and return
          *here*: falling through ran the whole field block against a context
          that was never opened. -->
     if (present(retstatus)) then
        retstatus = status
     else
        STOP
     end if
     return
  end if

  <!--<xsl:if test="@specific_validation_rules='yes'">
         call ids_validate(IDS,validationstatus)
  if (validationstatus.EQ.-1) return
  </xsl:if>-->

  timedparent=.false.
  <!-- The refused-write count describes one operation, not the process. -->
  call al_reset_refused_writes()

  <xsl:apply-templates select="./field" mode="PUT_FIELD">
    <xsl:with-param name="structvar" select="'IDS'"/>
    <xsl:with-param name="contextvar" select="'opctx'"/>
    <xsl:with-param name="timedparentexpr" select="''"/>
    <xsl:with-param name="slice" select="'yes'"/>
  </xsl:apply-templates>
  
  call al_write_plugins_metadata(opctx, status)
  if (status.ne.0) then
     write(*,*) 'Error in al_write_plugins_metadata (from ids_put_slice for IDS <xsl:value-of select="@name"/>)'
     <!-- opctx was opened by this routine and is still open, so end it before
          returning. Return the real status, not opctx: that is a context id,
          not a status. Falling through ran the rest of the routine against a
          context whose operation had already failed. -->
     if (present(retstatus)) then
        retstatus = status
     else
        STOP
     end if
     call al_end_action(opctx, status)
     return
  end if

  call al_end_action(opctx, status)

  <!-- retstatus here is al_end_action's status, not the write's. If the
       interposing layer refused any path, the caller's value for it was dropped
       and the occurrence does not hold everything they supplied. Say so:
       PARTIAL_PUT is positive, so it can never be confused with a C-ABI status,
       and it still trips the `status.ne.0` test callers already write. Without
       this the tolerance of a refused write would be silent, which is the one
       outcome al_put_policy exists to prevent. -->
  if (present(retstatus)) then
     if (status == 0 .and. al_get_refused_write_count() > 0) then
        retstatus = PARTIAL_PUT
     else
        retstatus = status
     end if
  end if
end subroutine put_slice_struct_ids_<xsl:value-of select="local:unique_name(@name)"/>

<xsl:for-each select=".//field[@data_type='structure' or @data_type='struct_array']">
  <xsl:variable name="this-type">
    <xsl:choose>
      <xsl:when test="@structure_reference='self'">
        <xsl:value-of select="@name"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="@structure_reference"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:variable name="this-ids" select="ancestor::IDS/@name"/>
  <xsl:if test="not (preceding::field[@structure_reference=$this-type and ancestor::IDS/@name=$this-ids] or /IDSs/utilities/field/@name=$this-type)">
subroutine put_slice_struct_ids_<xsl:value-of select="local:unique_name($this-type)"/>(ctx, name, path, struct, timemode, timedparent, retstatus)
  use ids_schemas_<xsl:value-of select="ancestor::IDS/@name"/>
  use al_low_level_wrap
  implicit none

  integer(ids_int), intent(in) :: ctx
  character*(*), intent(in) :: name, path
  type(ids_<xsl:value-of select="$this-type"/>), intent(inout) :: struct      
  logical, intent(in) :: timedparent
  integer, intent(in) :: timemode
  integer(ids_int), intent(out) :: retstatus
  integer(ids_int) :: i, aoslen, aos_hli_len, lenstring, aosctx, lastdimsize
  integer :: status
  character(len=100000) :: longstring
  character(len=300) :: timepath

  <xsl:apply-templates select="./field" mode="PUT_FIELD">
    <xsl:with-param name="structvar" select="'struct'"/>
    <xsl:with-param name="contextvar" select="'ctx'"/>
    <xsl:with-param name="timedparentexpr" select="'timedparent.or.'"/>
    <xsl:with-param name="slice" select="'yes'"/>
  </xsl:apply-templates>      

  retstatus = 0
end subroutine put_slice_struct_ids_<xsl:value-of select="local:unique_name($this-type)"/>

  </xsl:if>
</xsl:for-each>

end module    
  </xsl:result-document>
</xsl:template>





<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<!-- IDS_GET MODULE, UTILITIES                                               -->
<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<xsl:template match="utilities" mode="get_struct">
  <xsl:result-document href="utilities_get_struct.f90">
module utilities_get_struct

interface ids_get_struct
  <xsl:for-each select="/IDSs/utilities/field[@data_type='structure' or @data_type='struct_array']">  
    <xsl:variable name="this-type">
      <xsl:choose>
        <xsl:when test="@structure_reference='self'">
          <xsl:value-of select="@name"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="@structure_reference"/>
        </xsl:otherwise>
      </xsl:choose>
      </xsl:variable>
      module procedure get_struct_ids_<xsl:value-of select="local:unique_name($this-type)"/>
  </xsl:for-each>
end interface

 contains

<xsl:call-template name="isCriticalFuncCtx">
  <xsl:with-param name="method" select="'get'"/>
</xsl:call-template>

<xsl:for-each select="/IDSs/utilities//field[@data_type='structure' or @data_type='struct_array']">
  <xsl:variable name="this-name" select="@name"/>
  <xsl:variable name="this-type">
    <xsl:choose>
      <xsl:when test="@structure_reference='self'">
        <xsl:value-of select="@name"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="@structure_reference"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:if test="not (preceding::field[@structure_reference=$this-type or @name=$this-type])">

subroutine get_struct_ids_<xsl:value-of select="local:unique_name($this-type)"/>(ctx, path, struct, timemode, timedparent, retstatus)
  use ids_types
  use ids_utilities, only: ids_<xsl:value-of select="$this-type"/>
  use al_low_level_wrap
  implicit none

  integer(ids_int), intent(in) :: ctx
  character*(*), intent(in) :: path
  type(ids_<xsl:value-of select="$this-type"/>), intent(inout) :: struct
  logical, intent(in) :: timedparent
  integer, intent(in) :: timemode
  integer(ids_int), intent(out) :: retstatus
  integer(ids_int) :: i, aoslen, lenstring, aosctx
  integer(ids_int) :: size1, size2, size3, size4, size5, size6, size7
  integer :: status
  character(len=100000) :: longstring
  character(len=300) :: timepath

  <xsl:apply-templates select="./field" mode="GET_FIELD">
    <xsl:with-param name="structvar" select="'struct'"/>
    <xsl:with-param name="contextvar" select="'ctx'"/>
    <xsl:with-param name="timedparentexpr" select="'timedparent.or.'"/>
  </xsl:apply-templates>      
   retstatus = 0
end subroutine get_struct_ids_<xsl:value-of select="local:unique_name($this-type)"/>

  </xsl:if>
</xsl:for-each>

end module 
  </xsl:result-document>
</xsl:template>

<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<!-- IDS_VALIDATE MODULE, PER IDS                                                 -->
<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<xsl:template match="IDS" mode="validate_struct">
  <xsl:result-document href="{@name}_validate.f90">
<xsl:variable name="descendent-definitions">
  <xsl:apply-templates select="field[@data_type='structure' or @data_type='struct_array'][1]" mode="VALIDATE_FAMILY_DEFINITIONS">
  <xsl:with-param name="older-definitions" select="''"/>
  </xsl:apply-templates>
</xsl:variable>

module <xsl:value-of select="@name"/>_validate_struct

use utilities_validate_struct

interface ids_validate
  module procedure validate_struct_ids_<xsl:value-of select="local:unique_name(@name)"/> <!-- interface procedure for the whole IDS -->
  <xsl:for-each select=".//field[@data_type='structure' or @data_type='struct_array']"> <!-- interface procedure for all sub structures-->
  <xsl:variable name="this-type">
    <xsl:choose>
      <xsl:when test="@structure_reference='self'">
        <xsl:value-of select="@name"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="@structure_reference"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:if test="contains($descendent-definitions,concat('subroutine ids_validate_struct_',local:unique_name($this-type),'('))">
  <xsl:if test="not (preceding::field[@structure_reference=$this-type] or /IDSs/utilities/field/@name=$this-type)">
          module procedure ids_validate_struct_<xsl:value-of select="local:unique_name($this-type)"/>
   </xsl:if>
   </xsl:if>
  </xsl:for-each>
end interface 

contains

subroutine validate_struct_ids_<xsl:value-of select="local:unique_name(@name)"/>(ids, status, err_msg)
  use ids_schemas_<xsl:value-of select="@name"/>
  use al_low_level_wrap, only: IDS_TIME_MODE_HOMOGENEOUS, IDS_TIME_MODE_HETEROGENEOUS, IDS_TIME_MODE_INDEPENDENT
  implicit none
  type(ids_<xsl:value-of select="@name"/>), intent(in) :: ids
  integer(ids_int), intent(out), optional :: status
  character(:), allocatable, intent(out) :: err_msg

  character(len=:), allocatable :: ids_name

  integer(ids_int) :: array_size, i, itime, i1, i2, i3, i4, i5
  integer(ids_int) :: ids_time_mode
  integer(ids_int) :: ids_time_size
  logical :: check, error
  integer, allocatable :: shape_array(:)

  ids_name = "<xsl:value-of select="@name"/>"
  status = 0

  ids_time_mode = ids%ids_properties%homogeneous_time;
  if (ids_time_mode .ne. IDS_TIME_MODE_HOMOGENEOUS .and. \
      ids_time_mode .ne. IDS_TIME_MODE_HETEROGENEOUS .and. \
      ids_time_mode .ne. IDS_TIME_MODE_INDEPENDENT) then
        err_msg = "ids_properties.homogeneous_time wrong value"
        status = -1 
        return
  end if

  <xsl:if test="not(@type='constant')">
  if (ids_time_mode .eq. IDS_TIME_MODE_HOMOGENEOUS .and. .not. associated(ids%time)) then 
        err_msg = "the time array must be associated"
        status = -1 
        return
  else if (associated(ids%time)) then
    ids_time_size = size(ids%time)
  end if
  </xsl:if>
  
  
<!-- call ids_validate for each field-->
<xsl:apply-templates select="field" mode="VALIDATE_CHILD">
  <xsl:with-param name="descendent-definitions" select="$descendent-definitions"/>
  <xsl:with-param name="older-definitions" select="''"/>
  </xsl:apply-templates>
<!-- check the array shapes of the field of this ids-->
  <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_1D"> <xsl:with-param name="currpath" select="''"/> </xsl:apply-templates>
  <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_2D"> <xsl:with-param name="currpath" select="''"/> </xsl:apply-templates>
  <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_3D"> <xsl:with-param name="currpath" select="''"/> </xsl:apply-templates>
  <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_4D"> <xsl:with-param name="currpath" select="''"/> </xsl:apply-templates>
  <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_5D"> <xsl:with-param name="currpath" select="''"/> </xsl:apply-templates>
  <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_6D"> <xsl:with-param name="currpath" select="''"/> </xsl:apply-templates>	
end subroutine

<!-- subroutine definition for each field for all depth-->
<xsl:value-of select="$descendent-definitions"/>
end module    
  </xsl:result-document>
</xsl:template>

<xsl:template match="field" mode="VALIDATE_UTILITIES_CHILD_1D">
<xsl:apply-templates select=".//field[@data_type='struct_array' or @data_type='flt_1d_type' or @data_type='FLT_1D'
      or @data_type='int_1d_type' or @data_type='INT_1D'
      or @data_type='cpx_1d_type' or @data_type='CPX_1D']" mode="VALIDATE_UTILITIES_CHILD_SINGLE">
<xsl:with-param name="currpath" select="@name"/>
<xsl:with-param name="dimension" select="'0'"/>
</xsl:apply-templates>
</xsl:template>

<xsl:template match="field" mode="VALIDATE_UTILITIES_CHILD_2D">
<xsl:apply-templates select=".//field[@data_type='FLT_2D' or @data_type='INT_2D' or @data_type='CPX_2D']" mode="VALIDATE_UTILITIES_CHILD_SINGLE">
<xsl:with-param name="currpath" select="@name"/>
<xsl:with-param name="dimension" select="'0'"/>
</xsl:apply-templates>
<xsl:apply-templates select=".//field[@data_type='FLT_2D' or @data_type='INT_2D' or @data_type='CPX_2D']" mode="VALIDATE_UTILITIES_CHILD_SINGLE">
<xsl:with-param name="currpath" select="@name"/>
<xsl:with-param name="dimension" select="'1'"/>
</xsl:apply-templates>
</xsl:template>

<xsl:template match="field" mode="VALIDATE_UTILITIES_CHILD_3D">
<xsl:apply-templates select=".//field[@data_type='FLT_3D' or @data_type='INT_3D' or @data_type='CPX_3D']" mode="VALIDATE_UTILITIES_CHILD_SINGLE">
<xsl:with-param name="currpath" select="@name"/>
<xsl:with-param name="dimension" select="'0'"/>
</xsl:apply-templates>
<xsl:apply-templates select=".//field[@data_type='FLT_3D' or @data_type='INT_3D' or @data_type='CPX_3D']" mode="VALIDATE_UTILITIES_CHILD_SINGLE">
<xsl:with-param name="currpath" select="@name"/>
<xsl:with-param name="dimension" select="'1'"/>
</xsl:apply-templates>
<xsl:apply-templates select=".//field[@data_type='FLT_3D' or @data_type='INT_3D' or @data_type='CPX_3D']" mode="VALIDATE_UTILITIES_CHILD_SINGLE">
<xsl:with-param name="currpath" select="@name"/>
<xsl:with-param name="dimension" select="'2'"/>
</xsl:apply-templates>
</xsl:template>

<xsl:template match="field" mode="VALIDATE_UTILITIES_CHILD_4D">
<xsl:apply-templates select=".//field[@data_type='FLT_4D' or @data_type='INT_4D' or @data_type='CPX_4D']" mode="VALIDATE_UTILITIES_CHILD_SINGLE">
<xsl:with-param name="currpath" select="@name"/>
<xsl:with-param name="dimension" select="'0'"/>
</xsl:apply-templates>
<xsl:apply-templates select=".//field[@data_type='FLT_4D' or @data_type='INT_4D' or @data_type='CPX_4D']" mode="VALIDATE_UTILITIES_CHILD_SINGLE">
<xsl:with-param name="currpath" select="@name"/>
<xsl:with-param name="dimension" select="'1'"/>
</xsl:apply-templates>
<xsl:apply-templates select=".//field[@data_type='FLT_4D' or @data_type='INT_4D' or @data_type='CPX_4D']" mode="VALIDATE_UTILITIES_CHILD_SINGLE">
<xsl:with-param name="currpath" select="@name"/>
<xsl:with-param name="dimension" select="'2'"/>
</xsl:apply-templates>
<xsl:apply-templates select=".//field[@data_type='FLT_4D' or @data_type='INT_4D' or @data_type='CPX_4D']" mode="VALIDATE_UTILITIES_CHILD_SINGLE">
<xsl:with-param name="currpath" select="@name"/>
<xsl:with-param name="dimension" select="'3'"/>
</xsl:apply-templates>
</xsl:template>

<xsl:template match="field" mode="VALIDATE_UTILITIES_CHILD_5D">
<xsl:apply-templates select=".//field[@data_type='FLT_5D' or @data_type='INT_5D' or @data_type='CPX_5D']" mode="VALIDATE_UTILITIES_CHILD_SINGLE">
<xsl:with-param name="currpath" select="@name"/>
<xsl:with-param name="dimension" select="'0'"/>
</xsl:apply-templates>
<xsl:apply-templates select=".//field[@data_type='FLT_5D' or @data_type='INT_5D' or @data_type='CPX_5D']" mode="VALIDATE_UTILITIES_CHILD_SINGLE">
<xsl:with-param name="currpath" select="@name"/>
<xsl:with-param name="dimension" select="'1'"/>
</xsl:apply-templates>
<xsl:apply-templates select=".//field[@data_type='FLT_5D' or @data_type='INT_5D' or @data_type='CPX_5D']" mode="VALIDATE_UTILITIES_CHILD_SINGLE">
<xsl:with-param name="currpath" select="@name"/>
<xsl:with-param name="dimension" select="'2'"/>
</xsl:apply-templates>
<xsl:apply-templates select=".//field[@data_type='FLT_5D' or @data_type='INT_5D' or @data_type='CPX_5D']" mode="VALIDATE_UTILITIES_CHILD_SINGLE">
<xsl:with-param name="currpath" select="@name"/>
<xsl:with-param name="dimension" select="'3'"/>
</xsl:apply-templates>
<xsl:apply-templates select=".//field[@data_type='FLT_5D' or @data_type='INT_5D' or @data_type='CPX_5D']" mode="VALIDATE_UTILITIES_CHILD_SINGLE">
<xsl:with-param name="currpath" select="@name"/>
<xsl:with-param name="dimension" select="'4'"/>
</xsl:apply-templates>
</xsl:template>

<xsl:template match="field" mode="VALIDATE_UTILITIES_CHILD_6D">
<xsl:apply-templates select=".//field[@data_type='FLT_6D' or @data_type='INT_6D' or @data_type='CPX_6D']" mode="VALIDATE_UTILITIES_CHILD_SINGLE">
<xsl:with-param name="currpath" select="@name"/>
<xsl:with-param name="dimension" select="'0'"/>
</xsl:apply-templates>
<xsl:apply-templates select=".//field[@data_type='FLT_6D' or @data_type='INT_6D' or @data_type='CPX_6D']" mode="VALIDATE_UTILITIES_CHILD_SINGLE">
<xsl:with-param name="currpath" select="@name"/>
<xsl:with-param name="dimension" select="'1'"/>
</xsl:apply-templates>
<xsl:apply-templates select=".//field[@data_type='FLT_6D' or @data_type='INT_6D' or @data_type='CPX_6D']" mode="VALIDATE_UTILITIES_CHILD_SINGLE">
<xsl:with-param name="currpath" select="@name"/>
<xsl:with-param name="dimension" select="'2'"/>
</xsl:apply-templates>
<xsl:apply-templates select=".//field[@data_type='FLT_6D' or @data_type='INT_6D' or @data_type='CPX_6D']" mode="VALIDATE_UTILITIES_CHILD_SINGLE">
<xsl:with-param name="currpath" select="@name"/>
<xsl:with-param name="dimension" select="'3'"/>
</xsl:apply-templates>
<xsl:apply-templates select=".//field[@data_type='FLT_6D' or @data_type='INT_6D' or @data_type='CPX_6D']" mode="VALIDATE_UTILITIES_CHILD_SINGLE">
<xsl:with-param name="currpath" select="@name"/>
<xsl:with-param name="dimension" select="'4'"/>
</xsl:apply-templates>
<xsl:apply-templates select=".//field[@data_type='FLT_6D' or @data_type='INT_6D' or @data_type='CPX_6D']" mode="VALIDATE_UTILITIES_CHILD_SINGLE">
<xsl:with-param name="currpath" select="@name"/>
<xsl:with-param name="dimension" select="'5'"/>
</xsl:apply-templates>
</xsl:template>


<xsl:template match="field" mode="VALIDATE_UTILITIES_CHILD_SINGLE">
<xsl:param name="dimension"/>
<xsl:param name="currpath"/>
<xsl:variable name="coord">
  <xsl:apply-templates select="." mode="get_coordinate_string">
                <xsl:with-param name="dimension" select="$dimension"/>
        </xsl:apply-templates>
</xsl:variable >
<!-- check if as_parent is present in the coordinate path (assume at the end) if yes: check if the path does not refer to the field himself -->
<xsl:variable name="targetcoord">
<xsl:choose>
<xsl:when test="contains($coord,'as_parent')">
  <xsl:if test="not(ends-with(substring-before($coord,'/as_parent'),@name))">
    <xsl:value-of select="substring-before($coord,'/as_parent')"/>
  </xsl:if>
  <xsl:if test="ends-with(substring-before($coord,'/as_parent'),@name)">
    <xsl:value-of select="$coord"/>
  </xsl:if>
</xsl:when>
<xsl:otherwise>
  <xsl:value-of select="$coord"/>
</xsl:otherwise>
</xsl:choose>
</xsl:variable>
<!--  if as_parent is present define the target dimension, if not: targetdimension='1'-->
<xsl:variable name="targetdimension">
<xsl:choose>
<xsl:when test="contains($coord,'as_parent')">
  <xsl:if test="not(ends-with(substring-before($coord,'/as_parent'),@name))">
    <xsl:value-of select="number($dimension)+1"/>
  </xsl:if>
  <xsl:if test="ends-with(substring-before($coord,'/as_parent'),@name)">
    <xsl:value-of select="'1'"/>
  </xsl:if>
</xsl:when>
<xsl:otherwise>
  <xsl:apply-templates select="." mode="get_targetdim">
                <xsl:with-param name="dimension" select="$dimension"/>
        </xsl:apply-templates>
</xsl:otherwise>
</xsl:choose>
</xsl:variable>
<!-- ! get the common ancestors -->
<xsl:variable name="common_parent">
  <xsl:apply-templates select="." mode="get_common_parent">
                <xsl:with-param name="targetcoord" select="$targetcoord"/>
        </xsl:apply-templates>
</xsl:variable >
<xsl:choose>
      <xsl:when test="not(contains($targetcoord,' OR ')) and not(contains($targetcoord, '1...')) and $common_parent=$currpath">
            ! validation of <xsl:value-of select="@path"/> <xsl:value-of select="number($dimension)+1"/>
      <xsl:variable name="relativepath">
      <xsl:apply-templates select="." mode="get_path">
        <xsl:with-param name="parent" select="$common_parent"/>
      </xsl:apply-templates>
      </xsl:variable>
      <xsl:apply-templates select=".." mode="print_child_loops">
        <xsl:with-param name="parent" select="$common_parent"/>
      </xsl:apply-templates>
      if (associated(ids%<xsl:value-of select="$relativepath"/><xsl:value-of select="@name"/>)) then
      shape_array = SHAPE(ids%<xsl:value-of select="$relativepath"/><xsl:value-of select="@name"/>)
      array_size = size(ids%<xsl:value-of select="$relativepath"/><xsl:value-of select="@name"/>,<xsl:value-of select="number($dimension)+1"/>)
      if (array_size &gt; 0) then
      <xsl:if test="ends-with($targetcoord,'time')">
        if (ids_time_mode .eq. IDS_TIME_MODE_HOMOGENEOUS ) then
            if(array_size .ne. ids_time_size) then
              err_msg = "Element '"//ids_name//"/<xsl:value-of select="@path_doc"/>' has incorrect shape ("//trim(arrstr(shape_array))//"): its coordinate in dimension <xsl:value-of select="number($dimension) + 1"/> ('time') has size "//trim(str(ids_time_size))//"."
              status = -1 
              return
            endif
        endif
      <!-- <xsl:if test="field[@name='time' and @data_type='flt_1d_type' or @data_type='FLT_1D'
                 or @data_type='int_1d_type' or @data_type='INT_1D'
      or @data_type='cpx_1d_type' or @data_type='CPX_1D']"> -->
        if (ids_time_mode .eq. IDS_TIME_MODE_HETEROGENEOUS ) then
          if (.not.associated(ids%<xsl:value-of select="$targetcoord"/>)) then 
            err_msg = ids_name//"/<xsl:value-of select="$targetcoord"/> must be allocated."
            status = -1 
            return
          end if
          if(array_size .ne. size(ids%<xsl:value-of select="$targetcoord"/>,<xsl:value-of select="number($targetdimension)+1"/>)) then
            err_msg = "Element '"//ids_name//"/<xsl:value-of select="@path_doc"/>' has incorrect shape ("//trim(arrstr(shape_array))//"): its coordinate in dimension <xsl:value-of select="number($dimension) + 1"/> ('"//ids_name//"/<xsl:value-of select="$targetcoord"/>') has size "//trim(str(size(ids%<xsl:value-of select="$targetcoord"/>,<xsl:value-of select="number($targetdimension)+1"/>)))//"."
            status = -1 
            return
          endif
        endif
      <!-- </xsl:if> -->
      </xsl:if>
      <xsl:if test="not(ends-with($targetcoord,'time'))">
        if (.not.associated(ids%<xsl:value-of select="$targetcoord"/>)) then 
          err_msg = ids_name//"/<xsl:value-of select="$targetcoord"/> must be allocated."
          status = -1 
          return
        end if
        if(array_size .ne. size(ids%<xsl:value-of select="$targetcoord"/>,<xsl:value-of select="number($targetdimension)+1"/>)) then
          err_msg = "Element '"//ids_name//"/<xsl:value-of select="@path_doc"/>' has incorrect shape ("//trim(arrstr(shape_array))//"): its coordinate in dimension <xsl:value-of select="number($dimension) + 1"/> ('"//ids_name//"/<xsl:value-of select="$targetcoord"/>') has size "//trim(str(size(ids%<xsl:value-of select="$targetcoord"/>,<xsl:value-of select="number($targetdimension)+1"/>)))//"."
          status = -1 
          return
        endif
      </xsl:if>
      <xsl:text>
              end if
              end if 
      </xsl:text>
      <xsl:apply-templates select="." mode="print_end_child_loops">
        <xsl:with-param name="parent" select="$common_parent"/>
</xsl:apply-templates> 
      </xsl:when>
      <xsl:otherwise>
      <xsl:if test="$common_parent=$currpath">
            ! warning <xsl:value-of select="@path_doc"/> coordinates (<xsl:value-of select="number($dimension)+1"/>) consistency not verified (<xsl:value-of select="$targetcoord"/>)
      </xsl:if>
      </xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template match="field" mode="get_common_parent">
<xsl:param name="targetcoord"/>
<xsl:choose>
      <xsl:when test=".//field[@name=$targetcoord]">
        <xsl:value-of select="@name"/>
      </xsl:when>
      <xsl:otherwise>
      <xsl:if test="not(ancestor::IDSs/utilities/field/@name=@name)">
        <xsl:apply-templates select=".." mode="get_common_parent">
          <xsl:with-param name="targetcoord" select="$targetcoord"/>
        </xsl:apply-templates>
      </xsl:if>
      </xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template match="field" mode="get_path">
<xsl:param name="parent"/>
<xsl:choose>
      <xsl:when test="not(@name=$parent)">
        <xsl:if test=".[@data_type='structure']">
          <xsl:value-of select="concat(@name,'%')"/>
        </xsl:if>
         <xsl:if test=".[@data_type='struct_array']">
          <xsl:value-of select="concat(@path_doc,'%')"/>
        </xsl:if>
        <xsl:apply-templates select=".." mode="get_path">
          <xsl:with-param name="parent" select="$parent"/>
        </xsl:apply-templates>
      </xsl:when>
      <xsl:otherwise>
      </xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template match="field" mode="print_child_loops">
<xsl:param name="parent"/>
<xsl:choose>
      <xsl:when test="not(@name=$parent)">
         <xsl:if test=".[@data_type='struct_array']">
    if (associated(ids%<xsl:apply-templates select=".." mode="get_path">
        <xsl:with-param name="parent" select="$parent"/>
      </xsl:apply-templates><xsl:value-of select="@name"/>)) then
    do <xsl:value-of select="substring-after(substring-before(@path_doc,')'),'(')"/> = 1,size(ids%<xsl:apply-templates select=".." mode="get_path">
        <xsl:with-param name="parent" select="$parent"/>
      </xsl:apply-templates><xsl:value-of select="@name"/>,1)
        </xsl:if>
        <xsl:apply-templates select=".." mode="print_child_loops">
          <xsl:with-param name="parent" select="$parent"/>
        </xsl:apply-templates>
      </xsl:when>
      <xsl:otherwise>
      </xsl:otherwise>
</xsl:choose>
</xsl:template>


<xsl:template match="field" mode="print_end_child_loops">
<xsl:param name="parent"/>
<xsl:choose>
      <xsl:when test="not(@name=$parent)">
         <xsl:if test=".[@data_type='struct_array']">
    end do
    end if
        </xsl:if>
        <xsl:apply-templates select=".." mode="print_end_child_loops">
          <xsl:with-param name="parent" select="$parent"/>
        </xsl:apply-templates>
      </xsl:when>
      <xsl:otherwise>
      </xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template match="utilities" mode="VALIDATE_UTILITIES">
<xsl:result-document href="utilities_validate_struct.f90">
module utilities_validate_struct

use ids_types

interface ids_validate
  <xsl:for-each select=".//field[@data_type='structure' or @data_type='struct_array']">
  <xsl:variable name="this-type">
    <xsl:choose>
      <xsl:when test="@structure_reference='self'">
        <xsl:value-of select="@name"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="@structure_reference"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:variable name="procedure-content">
  <xsl:apply-templates select="." mode="VALIDATE_UTILITIES_DEFINITION"/>
  </xsl:variable>
  <xsl:if test="normalize-space($procedure-content)">
   <xsl:if test="generate-id() = generate-id(key('unique-structures-by-type', $this-type)[1])">
    module procedure ids_validate_struct_<xsl:value-of select="local:unique_name($this-type)"/>
  </xsl:if>
  </xsl:if>
  </xsl:for-each>
end interface 

contains

subroutine init_coordinate_validation(check,error,i) 
  implicit none 
  logical, intent(inout) :: check
  logical, intent(inout) :: error
  integer, intent(inout) :: i

  check = .TRUE.
  error = .TRUE.
  i = 0
end subroutine


subroutine  check_coordinate_allocation(check,i,stringmsg,err_msg,status)
  implicit none 
  logical, intent(inout) :: check
  integer, intent(in) :: i
  character(len=*), intent(in) :: stringmsg
  character(:), allocatable, intent(out) :: err_msg
  integer, intent(inout) :: status

  if (i.ne.1) then 
    check = .FALSE.
    end if   
  if (i.gt.1) then 
    check = .FALSE.
    err_msg = stringmsg
    status = -1 
  end if  
end subroutine


subroutine check_coordinate_size(array_size,coordsize,error,status, stringmsg, err_msg)
  implicit none
  integer, intent(in) :: array_size
  integer, intent(in) :: coordsize
  logical, intent(inout) :: error
  integer, intent(inout) :: status
  character(len=*), intent(in) :: stringmsg
  character(:), allocatable, intent(out) :: err_msg


  if (array_size .eq. coordsize) then
    error = .FALSE.
    status = 0
  else 
    if (coordsize.ne.0) then
      err_msg = stringmsg
      status = -1  
    end if  
  end if
end subroutine

<xsl:for-each select=".//field[@data_type='structure' or @data_type='struct_array']">
  <xsl:apply-templates select="." mode="VALIDATE_UTILITIES_DEFINITION"/>
</xsl:for-each>

character(len=20) function arrstr(karray)
!   "Convert an integer array to string."
  integer, intent(in) :: karray(:)
  write(arrstr,"(1000(I0,:,','))") karray
  arrstr = adjustl(arrstr)
end function arrstr

character(len=20) function str(k)
!   "Convert an integer to string."
    integer, intent(in) :: k
    write (str, *) k
    str = adjustl(str)
end function str

subroutine str_replace(str, old, new)
  implicit none
  character(:), allocatable, intent(inout) :: str
  character(*), intent(in) :: old
  character(*), intent(in) :: new
  
  character(:), allocatable :: temp
  
  integer :: i
  
  if(len(str)&lt;len(old)) return
  
  i = 1
  do while (i&lt;len(str)+1)
    if (i+len(old)&gt;len(str)) then 
    !nothing to do
    else
      if(str(i:i+len(old)-1)==old) then
        if (allocated(temp)) deallocate(temp)
          temp = str(1:i-1)//new//str(i+len(old):len(str))
          str  = temp
        i = i + len(new)
      end if
    end if
    i = i + 1
  end do
  
  end subroutine str_replace

end module 
</xsl:result-document>
</xsl:template>

<xsl:template match="field" mode="VALIDATE_UTILITIES_DEFINITION">
<xsl:variable name="this-name" select="@name"/>
  <xsl:variable name="this-type">
    <xsl:choose>
      <xsl:when test="@structure_reference='self'">
        <xsl:value-of select="@name"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="@structure_reference"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:variable name ="utilities_definition_content"> 
    <!-- call ids_validate for each field of this structure-->
    <xsl:apply-templates select="field" mode="VALIDATE_CHILD_UTILITIES"/>
    <!-- check the arrays-->
    <xsl:apply-templates select="." mode="VALIDATE_UTILITIES_CHILD_1D"/>
    <xsl:apply-templates select="." mode="VALIDATE_UTILITIES_CHILD_2D"/>
    <xsl:apply-templates select="." mode="VALIDATE_UTILITIES_CHILD_3D"/>
    <xsl:apply-templates select="." mode="VALIDATE_UTILITIES_CHILD_4D"/>
    <xsl:apply-templates select="." mode="VALIDATE_UTILITIES_CHILD_5D"/>
    <xsl:apply-templates select="." mode="VALIDATE_UTILITIES_CHILD_6D"/>
  </xsl:variable> 
  
  <xsl:if test="generate-id() = generate-id(key('unique-structures-by-type', $this-type)[1])">
  <xsl:if test="normalize-space($utilities_definition_content)">
  !----------------------------------------------------------------------- 
  !--- validation of <xsl:value-of select="$this-type"/>
  !-----------------------------------------------------------------------
  subroutine ids_validate_struct_<xsl:value-of select="local:unique_name($this-type)"/>(ids, ids_name, status, err_msg, ids_time_mode, ids_time_size)
    use ids_utilities, only: ids_<xsl:value-of select="$this-type"/>
    use al_low_level_wrap, only: IDS_TIME_MODE_HOMOGENEOUS, IDS_TIME_MODE_HETEROGENEOUS, IDS_TIME_MODE_INDEPENDENT
    implicit none
    type(ids_<xsl:value-of select="$this-type"/>), intent(in) :: ids
    character(len=*),  intent(in) :: ids_name
    integer(ids_int), intent(out), optional :: status
    character(:), allocatable, intent(out) :: err_msg
    integer(ids_int), intent(in) :: ids_time_mode
    integer(ids_int), intent(in) :: ids_time_size
    integer(ids_int) :: array_size, i, itime, i1, i2, i3, i4, i5
    logical :: check, error
    integer, allocatable :: shape_array(:)
    <xsl:value-of select="$utilities_definition_content" />
    status = 0
    return
  end subroutine
    </xsl:if>
  </xsl:if>
</xsl:template>

<xsl:template match="field" mode="VALIDATE_DESCENDENTS">
<xsl:param name="currpath"/>
<xsl:param name="containing"/>
  <xsl:variable name="descendant-validations">
 <xsl:if test="@data_type = 'struct_array'">
  <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_1D"> <xsl:with-param name="currpath" select="$currpath"/> <xsl:with-param name="containing" select="concat($containing,'/',@name,'(',substring-before(substring-after(@path_doc,concat('/',@name,'(')),')'),')')"/> </xsl:apply-templates>
  <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_2D"> <xsl:with-param name="currpath" select="$currpath"/> <xsl:with-param name="containing" select="concat($containing,'/',@name,'(',substring-before(substring-after(@path_doc,concat('/',@name,'(')),')'),')')"/> </xsl:apply-templates>
  <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_3D"> <xsl:with-param name="currpath" select="$currpath"/> <xsl:with-param name="containing" select="concat($containing,'/',@name,'(',substring-before(substring-after(@path_doc,concat('/',@name,'(')),')'),')')"/> </xsl:apply-templates>
  <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_4D"> <xsl:with-param name="currpath" select="$currpath"/> <xsl:with-param name="containing" select="concat($containing,'/',@name,'(',substring-before(substring-after(@path_doc,concat('/',@name,'(')),')'),')')"/> </xsl:apply-templates>
  <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_5D"> <xsl:with-param name="currpath" select="$currpath"/> <xsl:with-param name="containing" select="concat($containing,'/',@name,'(',substring-before(substring-after(@path_doc,concat('/',@name,'(')),')'),')')"/> </xsl:apply-templates>
  <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_6D"> <xsl:with-param name="currpath" select="$currpath"/> <xsl:with-param name="containing" select="concat($containing,'/',@name,'(',substring-before(substring-after(@path_doc,concat('/',@name,'(')),')'),')')"/> </xsl:apply-templates>
  </xsl:if> 
  <xsl:if test="@data_type = 'structure'">
  <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_1D"> <xsl:with-param name="currpath" select="$currpath"/> <xsl:with-param name="containing" select="concat($containing,'/',@name)"/> </xsl:apply-templates>
  <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_2D"> <xsl:with-param name="currpath" select="$currpath"/> <xsl:with-param name="containing" select="concat($containing,'/',@name)"/> </xsl:apply-templates>
  <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_3D"> <xsl:with-param name="currpath" select="$currpath"/> <xsl:with-param name="containing" select="concat($containing,'/',@name)"/> </xsl:apply-templates>
  <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_4D"> <xsl:with-param name="currpath" select="$currpath"/> <xsl:with-param name="containing" select="concat($containing,'/',@name)"/> </xsl:apply-templates>
  <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_5D"> <xsl:with-param name="currpath" select="$currpath"/> <xsl:with-param name="containing" select="concat($containing,'/',@name)"/> </xsl:apply-templates>
  <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_6D"> <xsl:with-param name="currpath" select="$currpath"/> <xsl:with-param name="containing" select="concat($containing,'/',@name)"/> </xsl:apply-templates>
  </xsl:if> 
  </xsl:variable>
  <xsl:if test="@data_type = 'structure'">
  <!-- !begin <xsl:value-of select="concat($containing,'/',@name)"/> -->
  <xsl:variable name="descendant-text"> 
  <xsl:apply-templates select="field[@data_type='struct_array' or @data_type='structure']" mode="VALIDATE_DESCENDENTS">
   <xsl:with-param name="currpath" select="$currpath"/>
   <xsl:with-param name="containing" select="concat($containing,'/',@name)"/>
  </xsl:apply-templates>
  </xsl:variable>
  <xsl:if test="normalize-space($descendant-text)"> <xsl:value-of select="$descendant-text"/> </xsl:if>
  <xsl:if test="normalize-space($descendant-validations)"> <xsl:value-of select="$descendant-validations"/>  </xsl:if>
  </xsl:if>
  <xsl:if test="@data_type = 'struct_array'">
  <!-- !begin <xsl:value-of select="concat($containing,'/',@name,'(',substring-before(substring-after(@path_doc,concat('/',@name,'(')),')'),')')"/> -->
  <xsl:variable name="descendant-text"> 
  <xsl:apply-templates select="field[@data_type='struct_array' or @data_type='structure']" mode="VALIDATE_DESCENDENTS">
   <xsl:with-param name="currpath" select="$currpath"/>
   <xsl:with-param name="containing" select="concat($containing,'/',@name,'(',substring-before(substring-after(@path_doc,concat('/',@name,'(')),')'),')')"/>
  </xsl:apply-templates>
  </xsl:variable>
  <xsl:if test="normalize-space($descendant-text) or normalize-space($descendant-validations)">
  if (associated(ids%<xsl:value-of select="replace(concat($containing,'%',@name),'/','%')"/>)) then
  do <xsl:value-of select="substring-before(substring-after(@path_doc,concat('/',@name,'(')),')')"/>= 1, size(ids%<xsl:value-of select="replace(concat($containing,'%',@name),'/','%')"/>)
  <xsl:value-of select="$descendant-text"/>
  <xsl:value-of select="$descendant-validations"/>
  if (status.eq.-1) call str_replace(err_msg,"<xsl:value-of select="substring-before(substring-after(@path_doc,concat('/',@name,'(')),')')"/>",trim(str(<xsl:value-of select="substring-before(substring-after(@path_doc,concat('/',@name,'(')),')')"/>)))
  end do
  end if
  </xsl:if>
  </xsl:if> 
</xsl:template>

<xsl:template match="IDS" mode="VALIDATE_2_DESCENDANT_1D">
<xsl:param name="currpath"/>
<xsl:apply-templates select="descendant-or-self::field[(@data_type='struct_array' or  @data_type='flt_1d_type' or @data_type='FLT_1D'
or @data_type='int_1d_type' or @data_type='INT_1D'
or @data_type='cpx_1d_type' or @data_type='CPX_1D') and (substring-before(@path_doc,concat(@name,'('))='/' or not(contains(@path_doc,('/'))))]" mode="VALIDATE_DESCENDANT_SINGLE">
<xsl:with-param name="currpath" select="$currpath"/>
<xsl:with-param name="dimension" select="'0'"/>
</xsl:apply-templates>
</xsl:template> 

<xsl:template match="IDS" mode="VALIDATE_2_DESCENDANT_2D">
<xsl:param name="currpath"/>
<xsl:apply-templates select="descendant-or-self::field[(@data_type='FLT_2D' or @data_type='INT_2D' or @data_type='CPX_2D') and (substring-before(@path_doc,concat(@name,'('))='/' or not(contains(@path_doc,('/'))))]" mode="VALIDATE_DESCENDANT_SINGLE_2D">
<xsl:with-param name="currpath" select="$currpath"/>
</xsl:apply-templates>
</xsl:template> 

<xsl:template match="IDS" mode="VALIDATE_2_DESCENDANT_3D">
<xsl:param name="currpath"/>
<xsl:apply-templates select="descendant-or-self::field[(@data_type='FLT_3D' or @data_type='INT_3D' or @data_type='CPX_3D') and (substring-before(@path_doc,concat(@name,'('))='/' or not(contains(@path_doc,('/'))))]" mode="VALIDATE_DESCENDANT_SINGLE_3D">
<xsl:with-param name="currpath" select="$currpath"/>
</xsl:apply-templates>
</xsl:template> 

<xsl:template match="IDS" mode="VALIDATE_2_DESCENDANT_4D">
<xsl:param name="currpath"/>
<xsl:apply-templates select="descendant-or-self::field[(@data_type='FLT_4D' or @data_type='INT_4D' or @data_type='CPX_4D') and (substring-before(@path_doc,concat(@name,'('))='/' or not(contains(@path_doc,('/'))))]" mode="VALIDATE_DESCENDANT_SINGLE_4D">
<xsl:with-param name="currpath" select="$currpath"/>
</xsl:apply-templates>
</xsl:template> 

<xsl:template match="IDS" mode="VALIDATE_2_DESCENDANT_5D">
<xsl:param name="currpath"/>
<xsl:apply-templates select="descendant-or-self::field[(@data_type='FLT_5D' or @data_type='INT_5D' or @data_type='CPX_5D') and (substring-before(@path_doc,concat(@name,'('))='/' or not(contains(@path_doc,('/'))))]" mode="VALIDATE_DESCENDANT_SINGLE_5D">
<xsl:with-param name="currpath" select="$currpath"/>
</xsl:apply-templates>
</xsl:template> 

<xsl:template match="IDS" mode="VALIDATE_2_DESCENDANT_6D">
<xsl:param name="currpath"/>
<xsl:apply-templates select="descendant-or-self::field[(@data_type='FLT_6D' or @data_type='INT_6D' or @data_type='CPX_6D') and (substring-before(@path_doc,concat(@name,'('))='/' or not(contains(@path_doc,('/'))))]" mode="VALIDATE_DESCENDANT_SINGLE_6D">
<xsl:with-param name="currpath" select="$currpath"/>
</xsl:apply-templates>
</xsl:template> 

<xsl:template match="field" mode="VALIDATE_2_DESCENDANT_1D">
<xsl:param name="currpath"/>
<xsl:param name="containing"/>
<xsl:apply-templates select="descendant-or-self::field[(@data_type='struct_array' or  @data_type='flt_1d_type' or @data_type='FLT_1D'
or @data_type='int_1d_type' or @data_type='INT_1D'
or @data_type='cpx_1d_type' or @data_type='CPX_1D') and contains(@path_doc,$containing) and substring-before(substring-after(@path_doc,$containing),concat(@name,'('))='/']" mode="VALIDATE_DESCENDANT_SINGLE">
<xsl:with-param name="currpath" select="$currpath"/>
<xsl:with-param name="dimension" select="'0'"/>
</xsl:apply-templates>
</xsl:template> 

<xsl:template match="field" mode="VALIDATE_2_DESCENDANT_2D">
<xsl:param name="currpath"/>
<xsl:param name="containing"/>
<xsl:apply-templates select="descendant-or-self::field[(@data_type='FLT_2D' or @data_type='INT_2D' or @data_type='CPX_2D') and contains(@path_doc,$containing) and substring-before(substring-after(@path_doc,$containing),concat(@name,'('))='/']" mode="VALIDATE_DESCENDANT_SINGLE_2D">
<xsl:with-param name="currpath" select="$currpath"/>
</xsl:apply-templates>
</xsl:template> 

<xsl:template match="field" mode="VALIDATE_2_DESCENDANT_3D">
<xsl:param name="currpath"/>
<xsl:param name="containing"/>
<xsl:apply-templates select="descendant-or-self::field[(@data_type='FLT_3D' or @data_type='INT_3D' or @data_type='CPX_3D') and contains(@path_doc,$containing) and substring-before(substring-after(@path_doc,$containing),concat(@name,'('))='/']" mode="VALIDATE_DESCENDANT_SINGLE_3D">
<xsl:with-param name="currpath" select="$currpath"/>
</xsl:apply-templates>
</xsl:template> 

<xsl:template match="field" mode="VALIDATE_2_DESCENDANT_4D">
<xsl:param name="currpath"/>
<xsl:param name="containing"/>
<xsl:apply-templates select="descendant-or-self::field[(@data_type='FLT_4D' or @data_type='INT_4D' or @data_type='CPX_4D') and contains(@path_doc,$containing) and substring-before(substring-after(@path_doc,$containing),concat(@name,'('))='/']" mode="VALIDATE_DESCENDANT_SINGLE_4D">
<xsl:with-param name="currpath" select="$currpath"/>
</xsl:apply-templates>
</xsl:template> 

<xsl:template match="field" mode="VALIDATE_2_DESCENDANT_5D">
<xsl:param name="currpath"/>
<xsl:param name="containing"/>
<xsl:apply-templates select="descendant-or-self::field[(@data_type='FLT_5D' or @data_type='INT_5D' or @data_type='CPX_5D') and contains(@path_doc,$containing) and substring-before(substring-after(@path_doc,$containing),concat(@name,'('))='/']" mode="VALIDATE_DESCENDANT_SINGLE_5D">
<xsl:with-param name="currpath" select="$currpath"/>
</xsl:apply-templates>
</xsl:template> 

<xsl:template match="field" mode="VALIDATE_2_DESCENDANT_6D">
<xsl:param name="currpath"/>
<xsl:param name="containing"/>
<xsl:apply-templates select="descendant-or-self::field[(@data_type='FLT_6D' or @data_type='INT_6D' or @data_type='CPX_6D') and contains(@path_doc,$containing) and substring-before(substring-after(@path_doc,$containing),concat(@name,'('))='/']" mode="VALIDATE_DESCENDANT_SINGLE_6D">
<xsl:with-param name="currpath" select="$currpath"/>
</xsl:apply-templates>
</xsl:template> 

<xsl:template match="field" mode="VALIDATE_CHILD_UTILITIES">
<xsl:variable name="this-type">
    <xsl:choose>
      <xsl:when test="@structure_reference='self'">
        <xsl:value-of select="@name"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="@structure_reference"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
<xsl:variable name="definition_content">
  <xsl:variable name="is_first_occurrence" select="generate-id() = generate-id(key('unique-structures-by-type', $this-type)[1])"/>
  <xsl:choose>
    <xsl:when test="not($is_first_occurrence)">
      <xsl:apply-templates select="key('unique-structures-by-type', $this-type)[1]" mode="VALIDATE_UTILITIES_DEFINITION"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:apply-templates select="." mode="VALIDATE_UTILITIES_DEFINITION"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:variable>
<xsl:choose>
<xsl:when test="@data_type='structure'"> 
 <xsl:if test="normalize-space($definition_content)">
  ! Validation of <xsl:value-of select = "@path"/>  ids_validate_struct_<xsl:value-of select="local:unique_name($this-type)"/>
  call ids_validate_struct_<xsl:value-of select="local:unique_name($this-type)"/>(ids%<xsl:value-of select = "@name"/>, ids_name//"/<xsl:value-of select = "@name"/>", status, err_msg, ids_time_mode, ids_time_size)
  if (status.eq.-1) return 
 </xsl:if>
</xsl:when>
<xsl:when test="@data_type='struct_array'">
  <xsl:if test="normalize-space($definition_content)">	
  if (associated(ids%<xsl:value-of select = "@name"/>)) then
  array_size = size(ids%<xsl:value-of select = "@name"/>)
  do i = 1, array_size
    ! Validation of <xsl:value-of select = "@path"/>   ids_validate_struct_<xsl:value-of select="local:unique_name($this-type)"/>
    call ids_validate_struct_<xsl:value-of select="local:unique_name($this-type)"/>(ids%<xsl:value-of select = "@name"/>(i), ids_name//"/<xsl:value-of select = "@name"/>", status, err_msg, ids_time_mode, ids_time_size)
    if (status.eq.-1) then
      call str_replace(err_msg,"<xsl:value-of select="substring-before(substring-after(@path_doc,concat(@name,'(')),')')"/>",trim(str(i)))
      return 
    end if
  end do
  end if 
  </xsl:if>
  </xsl:when>
  <xsl:when test="@data_type='struct_array' or @data_type='flt_1d_type' or @data_type='FLT_1D'
      or @data_type='int_1d_type' or @data_type='INT_1D'
      or @data_type='cpx_1d_type' or @data_type='CPX_1D'">

      <xsl:apply-templates select="." mode="VALIDATE_UTILITIES_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate1"/>
        <xsl:with-param name="dimension" select="'0'"/>
      </xsl:apply-templates>
  </xsl:when>
  <xsl:when test="@data_type='FLT_2D' or @data_type='INT_2D' or @data_type='CPX_2D'">

      <xsl:apply-templates select="." mode="VALIDATE_UTILITIES_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate1"/>
        <xsl:with-param name="dimension" select="'0'"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_UTILITIES_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate2"/>
        <xsl:with-param name="dimension" select="'1'"/>
      </xsl:apply-templates>
  </xsl:when>
  <xsl:when test="@data_type='FLT_3D' or @data_type='INT_3D' or @data_type='CPX_3D'">

      <xsl:apply-templates select="." mode="VALIDATE_UTILITIES_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate1"/>
        <xsl:with-param name="dimension" select="'0'"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_UTILITIES_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate2"/>
        <xsl:with-param name="dimension" select="'1'"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_UTILITIES_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate3"/>
        <xsl:with-param name="dimension" select="'2'"/>
      </xsl:apply-templates>
  </xsl:when>
  <xsl:when test="@data_type='FLT_4D' or @data_type='INT_4D' or @data_type='CPX_4D'">

      <xsl:apply-templates select="." mode="VALIDATE_UTILITIES_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate1"/>
        <xsl:with-param name="dimension" select="'0'"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_UTILITIES_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate2"/>
        <xsl:with-param name="dimension" select="'1'"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_UTILITIES_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate3"/>
        <xsl:with-param name="dimension" select="'2'"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_UTILITIES_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate4"/>
        <xsl:with-param name="dimension" select="'3'"/>
      </xsl:apply-templates>
  </xsl:when>
  <xsl:when test="@data_type='FLT_5D' or @data_type='INT_5D' or @data_type='CPX_5D'">

      <xsl:apply-templates select="." mode="VALIDATE_UTILITIES_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate1"/>
        <xsl:with-param name="dimension" select="'0'"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_UTILITIES_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate2"/>
        <xsl:with-param name="dimension" select="'1'"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_UTILITIES_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate3"/>
        <xsl:with-param name="dimension" select="'2'"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_UTILITIES_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate4"/>
        <xsl:with-param name="dimension" select="'3'"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_UTILITIES_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate5"/>
        <xsl:with-param name="dimension" select="'4'"/>
      </xsl:apply-templates>
  </xsl:when>
  <xsl:when test="@data_type='FLT_6D' or @data_type='INT_6D' or @data_type='CPX_6D'">

      <xsl:apply-templates select="." mode="VALIDATE_UTILITIES_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate1"/>
        <xsl:with-param name="dimension" select="'0'"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_UTILITIES_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate2"/>
        <xsl:with-param name="dimension" select="'1'"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_UTILITIES_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate3"/>
        <xsl:with-param name="dimension" select="'2'"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_UTILITIES_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate4"/>
        <xsl:with-param name="dimension" select="'3'"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_UTILITIES_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate5"/>
        <xsl:with-param name="dimension" select="'4'"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_UTILITIES_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate6"/>
        <xsl:with-param name="dimension" select="'5'"/>
      </xsl:apply-templates>
  </xsl:when>
</xsl:choose>
</xsl:template>

<!-- call validate routines for strucure and struct-array children -->
<xsl:template match="field" mode="VALIDATE_CHILD">
<xsl:param name="descendent-definitions"/>
<xsl:param name="older-definitions"/>
<xsl:variable name="this-type">
    <xsl:choose>
      <xsl:when test="@structure_reference='self'">
        <xsl:value-of select="@name"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="@structure_reference"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:variable name="text-uri" as="xs:string" select="unparsed-text('file:utilities_validate_struct.f90')"/>
  <xsl:variable name= "ids-validate-struct-definition-content">
   <xsl:if test="/IDSs/utilities/field/@name=$this-type and contains($text-uri,concat('subroutine ids_validate_struct_',local:unique_name($this-type),'('))">
   	print
   </xsl:if>
   <!--<xsl:if test="not(/IDSs/utilities/field/@name=$this-type) and contains($text-uri2,concat('subroutine ids_validate_struct_',local:unique_name($this-type),'('))">-->
   <xsl:if test="contains($older-definitions,concat('subroutine ids_validate_struct_',local:unique_name($this-type),'(')) or contains($descendent-definitions,concat('subroutine ids_validate_struct_',local:unique_name($this-type),'('))">
		   print
   </xsl:if>
   <!--</xsl:if>-->
  </xsl:variable>
<xsl:choose>
   <xsl:when test="@data_type='structure'">
<xsl:variable name = "validate_descendents_content">
        <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_1D"> <xsl:with-param name="currpath" select="normalize-space(../@path_doc)"/> <xsl:with-param name="containing" select="@name"/> </xsl:apply-templates>
  <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_2D"> <xsl:with-param name="currpath" select="normalize-space(../@path_doc)"/> <xsl:with-param name="containing" select="@name"/> </xsl:apply-templates>
  <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_3D"> <xsl:with-param name="currpath" select="normalize-space(../@path_doc)"/> <xsl:with-param name="containing" select="@name"/> </xsl:apply-templates>
  <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_4D"> <xsl:with-param name="currpath" select="normalize-space(../@path_doc)"/> <xsl:with-param name="containing" select="@name"/> </xsl:apply-templates>
  <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_5D"> <xsl:with-param name="currpath" select="normalize-space(../@path_doc)"/> <xsl:with-param name="containing" select="@name"/> </xsl:apply-templates>
  <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_6D"> <xsl:with-param name="currpath" select="normalize-space(../@path_doc)"/> <xsl:with-param name="containing" select="@name"/> </xsl:apply-templates>
  <xsl:apply-templates select="field[@data_type='struct_array' or @data_type='structure']" mode="VALIDATE_DESCENDENTS">
    <xsl:with-param name="currpath" select="normalize-space(../@path_doc)"/>
    <xsl:with-param name="containing" select="@name"/>
  </xsl:apply-templates>
  </xsl:variable>

  <xsl:if test="normalize-space($ids-validate-struct-definition-content)">
  ! Validation of <xsl:value-of select = "@path"/>
  call ids_validate_struct_<xsl:value-of select="local:unique_name($this-type)"/>(ids%<xsl:value-of select = "@name"/>, ids_name//"/<xsl:value-of select = "@name"/>", status, err_msg, ids_time_mode, ids_time_size)
  if (status.eq.-1) return
  </xsl:if>
  <xsl:if test="normalize-space($validate_descendents_content)">
  <xsl:value-of select="$validate_descendents_content"/>
  if (status.eq.-1) return
  </xsl:if>
</xsl:when>
<xsl:when test="@data_type='struct_array'">
<xsl:variable name="act_index">
    <xsl:choose>
      <xsl:when test="contains(@path_doc,'/')">
        <xsl:value-of select="substring-before(substring-after(@path_doc,concat('/',@name,'(')),')')"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="substring-before(substring-after(@path_doc,concat(@name,'(')),')')"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:variable name = "validate_descendents_content">
  <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_1D"> <xsl:with-param name="currpath" select="normalize-space(../@path_doc)"/> <xsl:with-param name="containing" select="concat(@name,'(',$act_index,')')"/> </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_2D"> <xsl:with-param name="currpath" select="normalize-space(../@path_doc)"/> <xsl:with-param name="containing" select="concat(@name,'(',$act_index,')')"/> </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_3D"> <xsl:with-param name="currpath" select="normalize-space(../@path_doc)"/> <xsl:with-param name="containing" select="concat(@name,'(',$act_index,')')"/> </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_4D"> <xsl:with-param name="currpath" select="normalize-space(../@path_doc)"/> <xsl:with-param name="containing" select="concat(@name,'(',$act_index,')')"/> </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_5D"> <xsl:with-param name="currpath" select="normalize-space(../@path_doc)"/> <xsl:with-param name="containing" select="concat(@name,'(',$act_index,')')"/> </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_6D"> <xsl:with-param name="currpath" select="normalize-space(../@path_doc)"/> <xsl:with-param name="containing" select="concat(@name,'(',$act_index,')')"/> </xsl:apply-templates>
      <xsl:apply-templates select="field[@data_type='struct_array' or @data_type='structure']" mode="VALIDATE_DESCENDENTS">
        <xsl:with-param name="currpath" select="normalize-space(../@path_doc)"/>
        <xsl:with-param name="containing" select="concat(@name,'(',$act_index,')')"/>
      </xsl:apply-templates>
  </xsl:variable>
  <xsl:if test="normalize-space($ids-validate-struct-definition-content) or normalize-space($validate_descendents_content)">
  if (associated(ids%<xsl:value-of select = "@name"/>)) then
  do <xsl:value-of select = "$act_index"/> = 1, size(ids%<xsl:value-of select = "@name"/>)
    ! Validation of <xsl:value-of select = "@path"/>
    <xsl:if test="normalize-space($ids-validate-struct-definition-content)">
    call ids_validate_struct_<xsl:value-of select="local:unique_name($this-type)"/>(ids%<xsl:value-of select = "@name"/>(<xsl:value-of select = "$act_index"/>), ids_name//"/<xsl:value-of select = "@name"/>", status, err_msg, ids_time_mode, ids_time_size)
    if (status.eq.-1) then
      call str_replace(err_msg,"<xsl:value-of select = "$act_index"/>",trim(str(<xsl:value-of select = "$act_index"/>)))
      return
    end if
    </xsl:if>
    <xsl:if test="normalize-space($validate_descendents_content)">
      <xsl:value-of select="$validate_descendents_content"/>
    if (status.eq.-1) then
      call str_replace(err_msg,"<xsl:value-of select = "$act_index"/>",trim(str(<xsl:value-of select = "$act_index"/>)))
      return
    end if  
    </xsl:if>
  end do
  end if 
  </xsl:if>
  </xsl:when>
  <xsl:when test="@data_type='struct_array' or @data_type='flt_1d_type' or @data_type='FLT_1D'
      or @data_type='int_1d_type' or @data_type='INT_1D'
      or @data_type='cpx_1d_type' or @data_type='CPX_1D'">
      <xsl:apply-templates select="." mode="VALIDATE_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate1"/>
        <xsl:with-param name="dimension" select="'0'"/>
      </xsl:apply-templates>
  </xsl:when>
  <xsl:when test="@data_type='FLT_2D' or @data_type='INT_2D' or @data_type='CPX_2D'">
      <xsl:apply-templates select="." mode="VALIDATE_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate1"/>
        <xsl:with-param name="dimension" select="'0'"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate2"/>
        <xsl:with-param name="dimension" select="'1'"/>
      </xsl:apply-templates>
  </xsl:when>
  <xsl:when test="@data_type='FLT_3D' or @data_type='INT_3D' or @data_type='CPX_3D'">

      <xsl:apply-templates select="." mode="VALIDATE_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate1"/>
        <xsl:with-param name="dimension" select="'0'"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate2"/>
        <xsl:with-param name="dimension" select="'1'"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate3"/>
        <xsl:with-param name="dimension" select="'2'"/>
      </xsl:apply-templates>
  </xsl:when>
  <xsl:when test="@data_type='FLT_4D' or @data_type='INT_4D' or @data_type='CPX_4D'">

      <xsl:apply-templates select="." mode="VALIDATE_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate1"/>
        <xsl:with-param name="dimension" select="'0'"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate2"/>
        <xsl:with-param name="dimension" select="'1'"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate3"/>
        <xsl:with-param name="dimension" select="'2'"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate4"/>
        <xsl:with-param name="dimension" select="'3'"/>
      </xsl:apply-templates>
  </xsl:when>
  <xsl:when test="@data_type='FLT_5D' or @data_type='INT_5D' or @data_type='CPX_5D'">

      <xsl:apply-templates select="." mode="VALIDATE_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate1"/>
        <xsl:with-param name="dimension" select="'0'"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate2"/>
        <xsl:with-param name="dimension" select="'1'"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate3"/>
        <xsl:with-param name="dimension" select="'2'"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate4"/>
        <xsl:with-param name="dimension" select="'3'"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate5"/>
        <xsl:with-param name="dimension" select="'4'"/>
      </xsl:apply-templates>
  </xsl:when>
  <xsl:when test="@data_type='FLT_6D' or @data_type='INT_6D' or @data_type='CPX_6D'">
      <xsl:apply-templates select="." mode="VALIDATE_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate1"/>
        <xsl:with-param name="dimension" select="'0'"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate2"/>
        <xsl:with-param name="dimension" select="'1'"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate3"/>
        <xsl:with-param name="dimension" select="'2'"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate4"/>
        <xsl:with-param name="dimension" select="'3'"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate5"/>
        <xsl:with-param name="dimension" select="'4'"/>
      </xsl:apply-templates>
      <xsl:apply-templates select="." mode="VALIDATE_FIXED_SIZE_COORDINATES">
        <xsl:with-param name="coord" select="@coordinate6"/>
        <xsl:with-param name="dimension" select="'5'"/>
      </xsl:apply-templates>
  </xsl:when>
</xsl:choose>
</xsl:template>

<xsl:template match="field" mode="VALIDATE_FIXED_SIZE_COORDINATES">
<xsl:param name="coord"/>
<xsl:param name="dimension"/>
<xsl:if test="not(contains($coord,' OR ')) and contains($coord, '1...') and not(contains($coord, '1...N')) and not(string(number(substring-after($coord,'1...')))='NaN')">
! validation of <xsl:value-of select="@path"/> dimension <xsl:value-of select="number($dimension)+1"/>
if (associated(ids%<xsl:value-of select = "@name"/>)) then
  array_size = size(ids%<xsl:value-of select = "@name"/>,<xsl:value-of select = "number($dimension)+1"/>)
  if (array_size .ne. <xsl:value-of select = "substring-after($coord,'1...')"/>) then
    status = -1
    err_msg = "Element '<xsl:value-of select="@path_doc"/>' has incorrect shape ("//trim(str(array_size))//"): dimension <xsl:value-of select="number($dimension) + 1"/> must have size <xsl:value-of select = "substring-after($coord,'1...')"/>."
    return 
  end if
end if
</xsl:if>
<xsl:if test="$coord='time'">
      ! validation of <xsl:value-of select="@path"/> dimension <xsl:value-of select="number($dimension)+1"/>   
      if (associated(ids%<xsl:value-of select="@name"/>)) then
      shape_array = SHAPE(ids%<xsl:value-of select="@name"/>)
      array_size = size(ids%<xsl:value-of select="@name"/>,<xsl:value-of select="number($dimension)+1"/>)
      if (ids_time_mode .eq. IDS_TIME_MODE_HOMOGENEOUS .or. ids_time_mode .eq. IDS_TIME_MODE_HETEROGENEOUS) then
          if(array_size .ne. ids_time_size) then
            err_msg = "Element '<xsl:value-of select="@path_doc"/>' has incorrect shape ("//trim(arrstr(shape_array))//"): its coordinate in dimension 1 ('time') has size "//trim(str(ids_time_size))//"."
            status = -1 
            return
          endif
	  endif
      endif
      </xsl:if>
</xsl:template>

<xsl:template match="field" mode="VALIDATE_UTILITIES_FIXED_SIZE_COORDINATES">
<xsl:param name="coord"/>
<xsl:param name="dimension"/>
<xsl:if test="not(contains($coord,' OR ')) and contains($coord, '1...') and not(contains($coord, '1...N')) and not(string(number(substring-after($coord,'1...')))='NaN')">
! validation of <xsl:value-of select="@path"/> dimension <xsl:value-of select="number($dimension)+1"/>
if (associated(ids%<xsl:value-of select = "@name"/>)) then
  array_size = size(ids%<xsl:value-of select = "@name"/>,<xsl:value-of select = "number($dimension)+1"/>)
  if (array_size .ne. <xsl:value-of select = "substring-after($coord,'1...')"/>) then
    status = -1
    err_msg = "Element '"//ids_name//"/<xsl:value-of select="@path_doc"/>' has incorrect shape ("//trim(str(array_size))//"): dimension <xsl:value-of select="number($dimension) + 1"/> must have size <xsl:value-of select = "substring-after($coord,'1...')"/>."
    return 
  end if
end if
</xsl:if>
<xsl:if test="$coord='time'">
      ! validation of <xsl:value-of select="@path"/> dimension <xsl:value-of select="number($dimension)+1"/>   
      if (associated(ids%<xsl:value-of select="@name"/>)) then
      shape_array = SHAPE(ids%<xsl:value-of select="@name"/>)
      array_size = size(ids%<xsl:value-of select="@name"/>,<xsl:value-of select="number($dimension)+1"/>)
      if (ids_time_mode .eq. IDS_TIME_MODE_HOMOGENEOUS ) then
          if(array_size .ne. ids_time_size) then
            err_msg = "Element '"//ids_name//"/<xsl:value-of select="@path_doc"/>' has incorrect shape ("//trim(arrstr(shape_array))//"): its coordinate in dimension 1 ('time') has size "//trim(str(ids_time_size))//"."
            status = -1 
            return
          endif
	  endif
      endif
      </xsl:if>
</xsl:template>

<xsl:template match="field" mode="VALIDATE_FAMILY_DEFINITIONS">
  <xsl:param name="older-definitions"/>
  <xsl:variable name="member-definition">
  <xsl:apply-templates select="." mode="VALIDATE_DEFINITIONS">
  <xsl:with-param name="older-definitions" select="$older-definitions"/>
  </xsl:apply-templates>
  </xsl:variable>
  <xsl:value-of select="$member-definition"/>
  <xsl:apply-templates select="following-sibling::field[@data_type='structure' or @data_type='struct_array'][1]" mode="VALIDATE_FAMILY_DEFINITIONS">
  <xsl:with-param name="older-definitions" select="concat($older-definitions,$member-definition)"/>
  </xsl:apply-templates>
</xsl:template>

<xsl:template match="field[@data_type='struct_array' or @data_type='structure']" mode="VALIDATE_DEFINITIONS">
<xsl:param name="older-definitions"/>
  <xsl:variable name="this-type">
    <xsl:choose>
      <xsl:when test="@structure_reference='self'">
        <xsl:value-of select="@name"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="@structure_reference"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:variable name="descendent-definitions">
  <xsl:apply-templates select="field[@data_type='structure' or @data_type='struct_array'][1]" mode="VALIDATE_FAMILY_DEFINITIONS">
  <xsl:with-param name="older-definitions" select="$older-definitions"/>
  </xsl:apply-templates>
  </xsl:variable>
  <xsl:variable name="ids-validate-struct-content">
    <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_1D"> <xsl:with-param name="currpath" select="normalize-space(@path_doc)"/> <xsl:with-param name="containing" select="@path_doc"/> </xsl:apply-templates>
    <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_2D"> <xsl:with-param name="currpath" select="normalize-space(@path_doc)"/> <xsl:with-param name="containing" select="@path_doc"/> </xsl:apply-templates>
    <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_3D"> <xsl:with-param name="currpath" select="normalize-space(@path_doc)"/> <xsl:with-param name="containing" select="@path_doc"/> </xsl:apply-templates>
    <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_4D"> <xsl:with-param name="currpath" select="normalize-space(@path_doc)"/> <xsl:with-param name="containing" select="@path_doc"/> </xsl:apply-templates>
    <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_5D"> <xsl:with-param name="currpath" select="normalize-space(@path_doc)"/> <xsl:with-param name="containing" select="@path_doc"/> </xsl:apply-templates>
    <xsl:apply-templates select="." mode="VALIDATE_2_DESCENDANT_6D"> <xsl:with-param name="currpath" select="normalize-space(@path_doc)"/> <xsl:with-param name="containing" select="@path_doc"/> </xsl:apply-templates>
  </xsl:variable>
  <xsl:variable name="validate-child-content">
  <xsl:apply-templates select="field" mode="VALIDATE_CHILD">
  <xsl:with-param name="descendent-definitions" select="$descendent-definitions"/>
  <xsl:with-param name="older-definitions" select="$older-definitions"/>
  </xsl:apply-templates>
  </xsl:variable>
  <xsl:if test="normalize-space(concat($ids-validate-struct-content,$validate-child-content))">
  <xsl:if test="not (preceding::field[@structure_reference=$this-type] or /IDSs/utilities/field/@name=$this-type)">
  !----------------------------------------------------------------------- 
  !--- validation of <xsl:value-of select="$this-type"/>
  !-----------------------------------------------------------------------
  subroutine ids_validate_struct_<xsl:value-of select="local:unique_name($this-type)"/>(ids, ids_name, status, err_msg, ids_time_mode, ids_time_size)
    use ids_schemas_<xsl:value-of select="ancestor::IDS/@name"/>
    use al_low_level_wrap, only: IDS_TIME_MODE_HOMOGENEOUS, IDS_TIME_MODE_HETEROGENEOUS, IDS_TIME_MODE_INDEPENDENT
    implicit none
    type(ids_<xsl:value-of select="$this-type"/>), intent(in) :: ids
    character(len=*),  intent(in) :: ids_name
    integer(ids_int), intent(out), optional :: status
    character(:), allocatable, intent(out) :: err_msg
    integer(ids_int), intent(in) :: ids_time_mode
    integer(ids_int), intent(in) :: ids_time_size
    integer(ids_int) :: array_size, i, itime, i1, i2, i3, i4, i5
    logical :: check, error
    integer, allocatable :: shape_array(:)

    <xsl:value-of select="$ids-validate-struct-content"/>
    <!-- call ids_validate for each field of this structure-->
    if (status .ne. 0) return
    <xsl:value-of select="$validate-child-content"/>
    <!-- check the array shapes of the field that have this structure as deeper common ancestor with the coordinateX reference field-->
  end subroutine
  </xsl:if>
  </xsl:if>
  <xsl:value-of select="$descendent-definitions"/>
</xsl:template>

<!-- return yes if some field exist like @path_doc equals to the parameter path_doc_to_check -->
<xsl:template match="field" mode="ISPRESENT_PATH_DOC">
<xsl:param name="path_doc_to_check"/>
        <xsl:if test="descendant-or-self::field[contains(@path_doc,$path_doc_to_check)]">
                <xsl:value-of select="'yes'"/>
        </xsl:if >
</xsl:template> 
<!-- get the target coordinate (if 'as_parent' or not) -->
<xsl:template match="field" mode="get_coordinate">
<xsl:param name="dimension"/>
<xsl:variable name="string_coord">
<xsl:apply-templates select="." mode="get_coordinate_string">
<xsl:with-param name="dimension" select="$dimension"/>
</xsl:apply-templates>
</xsl:variable>
  <xsl:choose>
                <xsl:when test="not($string_coord='1...N')">
      <xsl:value-of select="$string_coord"/>
                </xsl:when>
        </xsl:choose>
</xsl:template>


<xsl:template match="field" mode="get_coordinate_string">
<xsl:param name="dimension"/>
  <xsl:choose>
                <xsl:when test="$dimension='0'">
      <xsl:if test="@coordinate1='1...N' and @coordinate1_same_as">
                        <xsl:value-of select="@coordinate1_same_as"/>
      </xsl:if>
      <xsl:if test="not(@coordinate1_same_as)">
                        <xsl:value-of select="@coordinate1"/>
      </xsl:if>
                </xsl:when>
                <xsl:when test="$dimension='1'">
                        <xsl:if test="@coordinate2='1...N' and @coordinate2_same_as">
                        <xsl:value-of select="@coordinate2_same_as"/>
      </xsl:if>
      <xsl:if test="not(@coordinate2_same_as)">
                        <xsl:value-of select="@coordinate2"/>
      </xsl:if>
                </xsl:when>
                <xsl:when test="$dimension='2'">
                        <xsl:if test="@coordinate3='1...N' and @coordinate3_same_as">
                        <xsl:value-of select="@coordinate3_same_as"/>
      </xsl:if>
      <xsl:if test="not(@coordinate3_same_as)">
                        <xsl:value-of select="@coordinate3"/>
      </xsl:if>
                </xsl:when>
                <xsl:when test="$dimension='3'">
                        <xsl:if test="@coordinate4='1...N' and @coordinate4_same_as">
                        <xsl:value-of select="@coordinate4_same_as"/>
      </xsl:if>
      <xsl:if test="not(@coordinate4_same_as)">
                        <xsl:value-of select="@coordinate4"/>
      </xsl:if>
                </xsl:when>
                <xsl:when test="$dimension='4'">
                        <xsl:if test="@coordinate5='1...N' and @coordinate5_same_as">
                        <xsl:value-of select="@coordinate5_same_as"/>
      </xsl:if>
      <xsl:if test="not(@coordinate5_same_as)">
                        <xsl:value-of select="@coordinate5"/>
      </xsl:if>
                </xsl:when>
                <xsl:when test="$dimension='5'">
                        <xsl:if test="@coordinate6='1...N' and @coordinate6_same_as">
                        <xsl:value-of select="@coordinate6_same_as"/>
      </xsl:if>
      <xsl:if test="not(@coordinate6_same_as)">
                        <xsl:value-of select="@coordinate6"/>
      </xsl:if>
                </xsl:when>
        </xsl:choose>
</xsl:template>

<!-- get the target coordinate dimension-->
<xsl:template match="field" mode="get_targetdim">
<xsl:param name="dimension"/>
  <xsl:choose>
                <xsl:when test="$dimension='0'">
      <xsl:if test="@coordinate1='1...N' and @coordinate1_same_as">
                        <xsl:value-of select="'0'"/>
      </xsl:if>
      <xsl:if test="not(@coordinate1='1...N') and not(@coordinate1_same_as)">
                        <xsl:value-of select="'0'"/>
      </xsl:if>
                </xsl:when>
                <xsl:when test="$dimension='1'">
                        <xsl:if test="@coordinate2='1...N' and @coordinate2_same_as">
                        <xsl:value-of select="'1'"/>
      </xsl:if>
      <xsl:if test="not(@coordinate2='1...N') and not(@coordinate2_same_as)">
                        <xsl:value-of select="'0'"/>
      </xsl:if>
                </xsl:when>
                <xsl:when test="$dimension='2'">
                        <xsl:if test="@coordinate3='1...N' and @coordinate3_same_as">
                        <xsl:value-of select="'2'"/>
      </xsl:if>
      <xsl:if test="not(@coordinate3='1...N') and not(@coordinate3_same_as)">
                        <xsl:value-of select="'0'"/>
      </xsl:if>
                </xsl:when>
                <xsl:when test="$dimension='3'">
                        <xsl:if test="@coordinate4='1...N' and @coordinate4_same_as">
                        <xsl:value-of select="'3'"/>
      </xsl:if>
      <xsl:if test="not(@coordinate4='1...N') and not(@coordinate4_same_as)">
                        <xsl:value-of select="'0'"/>
      </xsl:if>
                </xsl:when>
                <xsl:when test="$dimension='4'">
                        <xsl:if test="@coordinate5='1...N' and @coordinate5_same_as">
                        <xsl:value-of select="'4'"/>
      </xsl:if>
      <xsl:if test="not(@coordinate5='1...N') and not(@coordinate5_same_as)">
                        <xsl:value-of select="'0'"/>
      </xsl:if>
                </xsl:when>
                <xsl:when test="$dimension='5'">
                        <xsl:if test="@coordinate6='1...N' and @coordinate6_same_as">
                        <xsl:value-of select="'5'"/>
      </xsl:if>
      <xsl:if test="not(@coordinate1='1...N') and not(@coordinate6_same_as)">
                        <xsl:value-of select="'0'"/>
      </xsl:if>
                </xsl:when>
        </xsl:choose>
</xsl:template>

<!-- write the check statements to check the <dimension> of the current field -->
<!-- <currpath> is the deeper comon ancestor of the reference coordinate and the current field -->
<xsl:template match="field" mode="VALIDATE_DESCENDANT_SINGLE">
        <xsl:param name="currpath"/>
        <xsl:param name="dimension"/>
  <!-- target field coordinate we want to check (specific and relative coordinate) -->
        <xsl:variable name="coord">
  <xsl:apply-templates select="." mode="get_coordinate">
                <xsl:with-param name="dimension" select="$dimension"/>
        </xsl:apply-templates>
        </xsl:variable >
  <!-- target field dimension we want to check -->
  <xsl:variable name="targetdim">
  <xsl:apply-templates select="." mode="get_targetdim">
                <xsl:with-param name="dimension" select="$dimension"/>
        </xsl:apply-templates>
        </xsl:variable >
  <!-- variable to check if the specified coordinate is present or not. 
         this variable is a safeguard that prevents wrong code generation-->
        <xsl:variable name="ispresent">
    <xsl:choose>
    <xsl:when test="$currpath='' and not($coord='') and not(contains($coord, '1...'))">
              <xsl:value-of select="'yes'"/>
    </xsl:when>
                <xsl:when test="contains($coord,'OR')">
                <xsl:apply-templates select="ancestor::field[@path_doc = $currpath]" mode="ISPRESENT_PATH_DOC">
                        <xsl:with-param name="path_doc_to_check" select="substring-before($coord,' OR')"/>
                </xsl:apply-templates>
                </xsl:when>
    <xsl:otherwise>
      <xsl:apply-templates select="ancestor::field[@path_doc = $currpath]" mode="ISPRESENT_PATH_DOC">
                        <xsl:with-param name="path_doc_to_check" select="$coord"/>
                  </xsl:apply-templates>
    </xsl:otherwise>
  </xsl:choose>
        </xsl:variable >
        <xsl:variable name="prefix" select="substring-before(@path_doc,concat('/',@name))"/>
  <!-- find the relative coordinate from the current field path and the target field path -->
        <xsl:variable name="relativecoord">
  <xsl:if test="not($currpath='')">
  <xsl:choose>
                <xsl:when test="contains($coord,'OR')">
                        <xsl:value-of select="substring-after(substring-before($coord,'OR'),concat($currpath,'/'))"/>
                </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="substring-after($coord,concat($currpath,'/'))"/>
    </xsl:otherwise>
  </xsl:choose>
  </xsl:if>
  <xsl:if test="$currpath=''">
  <xsl:choose>
                <xsl:when test="contains($coord,'OR')">
                        <xsl:value-of select="substring-before($coord,'OR')"/>
                </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="$coord"/>
    </xsl:otherwise>
  </xsl:choose>
  </xsl:if>
  </xsl:variable >
   <!-- find the relative coordinate from the current field path and the checked field -->
        <!-- <xsl:variable name="relativepath" select="substring-after(concat($prefix,'/',@name),concat($currpath,'/'))"/> -->
  <xsl:variable name="relativepath">
  <xsl:if test="not($currpath='')">
                        <xsl:value-of select="substring-after(concat($prefix,'/',@name),concat($currpath,'/'))"/>
  </xsl:if>
  <xsl:if test="$currpath=''">
      <xsl:value-of select="$prefix"/>
  </xsl:if>
  </xsl:variable >
        <xsl:variable name="child">
                <xsl:choose>
                          <xsl:when test="contains($relativepath,'/')">
                                <xsl:value-of select="substring-before($relativepath,'/')"/>
                        </xsl:when>
                        <xsl:otherwise>
                                <xsl:value-of select="$relativepath"/>
                        </xsl:otherwise>
        </xsl:choose>
        </xsl:variable>
  <!-- verify if the current field is the deeper common ancestor of the target coordinate field and the checked field -->
  <xsl:variable name="test">
                <xsl:choose>
      <!-- validation logic: the time coordinate are passed by argument of each validation routines so no need to check if at level of IDS -->
      <xsl:when test="($coord='time' or contains($coord,'IDS:')) and $currpath=''">
                                <xsl:value-of select="''"/>
                        </xsl:when>
                  <xsl:when test="contains($relativecoord,'/')">
                                <xsl:value-of select="$child=substring-before($relativecoord,'/')"/>
                        </xsl:when>
                        <xsl:otherwise>
                                <xsl:value-of select="$child=$relativecoord"/>
                        </xsl:otherwise>
        </xsl:choose>
       </xsl:variable>
       <xsl:variable name="root">
        <xsl:if test="not($currpath='')">
          <xsl:value-of select="concat($currpath,'/')"/>
        </xsl:if>
        <xsl:if test="$currpath=''">
          <xsl:value-of select="concat($currpath,'/')"/>
        </xsl:if>
        </xsl:variable>
        <xsl:variable name="onecoord">
        <xsl:if test="not(contains($coord,' OR'))">
           <xsl:value-of select="$coord"/>
        </xsl:if>
        <xsl:if test="contains($coord,' OR')">
          <xsl:value-of select="substring-before($coord,' OR')"/>
        </xsl:if>
        </xsl:variable>
        <xsl:variable name="is-index-dep">
        <xsl:if test="$root='/'">
          <xsl:if test="contains($onecoord,'(itime') and not(contains($onecoord,'(itime)/time'))">
          <xsl:value-of select="'yes'"/>
        </xsl:if>
        </xsl:if>
        <xsl:if test="not($root='/')">
          <xsl:if test="contains(substring-after($onecoord,$root),'(itime') and not(contains($onecoord,'(itime)/time'))">
          <xsl:value-of select="'yes'"/>
        </xsl:if>
        </xsl:if>
        </xsl:variable>

	<!-- missing IDS coordinate exception-->
	<xsl:if test="starts-with($coord,$currpath) and contains($ispresent,'yes') and not($is-index-dep='yes')">
          <xsl:if test="$test='false'">
            ! validation of <xsl:value-of select="@path"/> dimension <xsl:value-of select="number($dimension) + 1"/>
            <xsl:variable name="newpath">
            <xsl:if test="not($currpath='')">
              <xsl:value-of select="substring-after(@path,concat(ancestor::field[@path_doc = $currpath]/@path,'/'))"/>
            </xsl:if>
            <xsl:if test="$currpath=''">
              <xsl:value-of select="@path"/>
            </xsl:if>
            </xsl:variable>
              <xsl:apply-templates select="." mode="VALIDATE_PATH_SINGLE">
              <xsl:with-param name="newpath" select="$newpath"/>
              <xsl:with-param name="root" select="$root"/>
              <xsl:with-param name="string" select="''"/>
              <xsl:with-param name="dimension" select="$dimension"/>
              <xsl:with-param name="coord" select="$coord"/>
              <xsl:with-param name="targetdim" select="$targetdim"/>
            </xsl:apply-templates>
          </xsl:if>
        </xsl:if>
</xsl:template>

<xsl:template match="field" mode="VALIDATE_PATH_SINGLE">
        <xsl:param name="newpath"/>
        <xsl:param name="root"/>
        <xsl:param name="string"/>
        <xsl:param name="dimension"/>
        <xsl:param name="coord"/>
        <xsl:param name="targetdim"/>
        <xsl:if test="contains($newpath,'/')">
        <xsl:choose>
                <xsl:when test="ancestor::field[@name = substring-before($newpath,'/')]/@data_type='structure'">
                        <xsl:variable name="act_struct" select="ancestor::field[@name = substring-before($newpath,'/')]/@name" />
                        <xsl:apply-templates select="." mode="VALIDATE_PATH_SINGLE">
                        <xsl:with-param name="newpath" select="substring-after($newpath,'/')"/>
                        <xsl:with-param name="root" select="$root"/>
                        <xsl:with-param name="string" select="concat($string,$act_struct,'%')"/>
                        <xsl:with-param name="dimension" select="$dimension"/>
                        <xsl:with-param name="coord" select="$coord"/>
     <xsl:with-param name="targetdim" select="$targetdim"/>
                        </xsl:apply-templates>
                </xsl:when>
                <xsl:when test="ancestor::field[@name = substring-before($newpath,'/')]/@data_type='struct_array'">
                <xsl:variable name="act_struct" select="ancestor::field[@name = substring-before($newpath,'/')]/@name" />
                <xsl:variable name="act_index">
                <xsl:if test="contains(ancestor::field[@name = substring-before($newpath,'/')]/@path_doc,'/')">
                  <xsl:value-of select="substring-before(substring-after(ancestor::field[@name = substring-before($newpath,'/')]/@path_doc,concat('/',$act_struct,'(')),')')"/>
                </xsl:if>
                <xsl:if test="not(contains(ancestor::field[@name = substring-before($newpath,'/')]/@path_doc,'/'))">
                  <xsl:value-of select="substring-before(substring-after(ancestor::field[@name = substring-before($newpath,'/')]/@path_doc,concat($act_struct,'(')),')')"/>
                </xsl:if>
                </xsl:variable>
                <!-- !if (associated(ids%<xsl:value-of select="$string"/><xsl:value-of select="$act_struct"/>)) then
                         !do <xsl:value-of select="$act_index"/>=1, size(ids%<xsl:value-of select="$string"/><xsl:value-of select="$act_struct"/>,1) -->
                        <xsl:apply-templates select="." mode="VALIDATE_PATH_SINGLE">
                        <xsl:with-param name="newpath" select="substring-after($newpath,'/')"/>
                        <xsl:with-param name="root" select="$root"/>
                        <xsl:with-param name="string" select="concat($string,$act_struct,'(',$act_index,')%')"/>
                        <xsl:with-param name="dimension" select="$dimension"/>
                        <xsl:with-param name="coord" select="$coord"/>
      <xsl:with-param name="targetdim" select="$targetdim"/>
                </xsl:apply-templates>
              <!-- !  end do
                       !end if -->
                </xsl:when>
        </xsl:choose>
        </xsl:if>
        <xsl:variable name="istimeslice">
  <xsl:if test="contains($coord,' OR')">
  <xsl:if test="not($root='/')">
    <xsl:if test="contains(substring-before(substring-after($coord,$root),' OR'),'(itime)')">
      <xsl:if test="not(contains(concat($string,@name),'(itime)'))">
        <xsl:value-of select="'yes'"/>
      </xsl:if>
    </xsl:if>
  </xsl:if>
  <xsl:if test="$root='/'">
    <xsl:if test="contains(substring-before($coord,' OR'),'(itime)')">
      <xsl:if test="not(contains(concat($string,@name),'(itime)'))">
        <xsl:value-of select="'yes'"/>
      </xsl:if>
    </xsl:if>
  </xsl:if>
  </xsl:if>
  <xsl:if test="not($root='/')">
  <xsl:if test="not(contains($coord,' OR'))">
    <xsl:if test="contains(substring-after($coord,$root),'(itime)')">
      <xsl:if test="not(contains(concat($string,@name),'(itime)'))">
        <xsl:value-of select="'yes'"/>
      </xsl:if>
    </xsl:if>
  </xsl:if>
  </xsl:if>
  <xsl:if test="$root='/'">
  <xsl:if test="not(contains($coord,' OR'))">
    <xsl:if test="contains($coord,'(itime)')">
      <xsl:if test="not(contains(concat($string,@name),'(itime)'))">
        <xsl:value-of select="'yes'"/>
      </xsl:if>
    </xsl:if>
  </xsl:if>
  </xsl:if>
  </xsl:variable>
  <xsl:if test="not(contains($newpath,'/')) and not($istimeslice='yes')">
  if (status .eq. 0 .and. associated(ids%<xsl:value-of select="$string"/><xsl:value-of select="@name"/>)) then
  shape_array = SHAPE(ids%<xsl:value-of select="$string"/><xsl:value-of select="@name"/>)
  array_size = shape_array(<xsl:value-of select="number($dimension) + 1"/>)
  if (array_size &gt; 0) then
  <xsl:if test="contains($coord,' OR ')">
   <xsl:apply-templates select="." mode="check-target-indices"><xsl:with-param name="coord" select="$coord"/><xsl:with-param name="relativepathdoc" select="$root"/></xsl:apply-templates>
                <xsl:if test="@type='dynamic' and ends-with($coord,'/time')">
                if (ids_time_mode .eq. IDS_TIME_MODE_HETEROGENEOUS ) then
                </xsl:if>
      call init_coordinate_validation(check,error,i)
      <xsl:apply-templates select="." mode="possible-coordinates"><xsl:with-param name="coord" select="$coord"/><xsl:with-param name="relativepathdoc" select="$root"/><xsl:with-param name="self" select="concat($string,@name)"/></xsl:apply-templates>

      call check_coordinate_allocation(check,i,&amp;
       "Element <xsl:value-of select="@path_doc"/>  must have its coordinate in dimension <xsl:value-of select="number($dimension) + 1"/> (any of <xsl:value-of select="$coord"/>) filled.",&amp;
                                      err_msg,status)
                                      
      if(status == 0 .and. check) then
        <xsl:apply-templates select="." mode="check-possible-coordinates">
          <xsl:with-param name="coord" select="$coord"/>
          <xsl:with-param name="relativepathdoc" select="$root"/>
          <xsl:with-param name="dimension" select="$dimension"/>
          <xsl:with-param name="self" select="concat($string,@name)"/>
          <xsl:with-param  name="targetdim" select="$targetdim"/>
        </xsl:apply-templates> 
      endif

      <xsl:apply-templates select="." mode="check-specific-coordinates">
        <xsl:with-param name="coord" select="$coord"/>
        <xsl:with-param name="relativepathdoc" select="$root"/>
        <xsl:with-param name="dimension" select="$dimension"/>
        <xsl:with-param name="self" select="concat($string,@name)"/>
      </xsl:apply-templates>

      if (status == 0 .and. error) then 
        err_msg = "Element <xsl:value-of select="@path_doc"/>  must have its coordinate in dimension <xsl:value-of select="number($dimension) + 1"/> (any of <xsl:value-of select="$coord"/>) filled."
        status = -1 
      end if
    </xsl:if>
    <xsl:if test="not(contains($coord,' OR '))">
    <xsl:apply-templates select="." mode="check-target-indices"><xsl:with-param name="coord" select="$coord"/><xsl:with-param name="relativepathdoc" select="$root"/></xsl:apply-templates>
    <xsl:variable name="target">
    <xsl:if test="not($root='/')">
        <xsl:value-of select="replace(substring-after($coord,$root),'/','%')"/>
    </xsl:if>
    <xsl:if test="$root='/'">
        <xsl:value-of select="replace($coord,'/','%')"/>
    </xsl:if>
  </xsl:variable>
  <xsl:variable name="resolved_target">
    <xsl:apply-templates select="." mode="resolve_indices">
      <xsl:with-param name="target" select="$target"/>
      <xsl:with-param name="string-resolved" select="''"/>
    </xsl:apply-templates>
  </xsl:variable>
  <xsl:if test="@type='dynamic' and ends-with($coord,'/time')">
    if (ids_time_mode .eq. IDS_TIME_MODE_HETEROGENEOUS ) then
   </xsl:if>
     if (associated(ids%<xsl:value-of select="$resolved_target"/>)) then
    	if (size(ids%<xsl:value-of select="$resolved_target"/>,<xsl:value-of select="number($targetdim)+1"/>) .ne. array_size) then 
    	err_msg = "Element '<xsl:value-of select="@path_doc"/>' has incorrect shape ("//trim(arrstr(shape_array))//"): its coordinate in dimension <xsl:value-of select="number($dimension) + 1"/> ('<xsl:value-of select="$coord"/>') has size "//trim(str(size(ids%<xsl:value-of select="$resolved_target"/>,<xsl:value-of select="number($targetdim)+1"/>)))//"."
    	status = -1 
    	end if
    else 
    	err_msg = "Element <xsl:value-of select="@path_doc"/>  must have its coordinate in dimension <xsl:value-of select="number($dimension) + 1"/> (any of <xsl:value-of select="$coord"/>) filled."
        status = -1
    end if
    </xsl:if>
        <xsl:if test="@type='dynamic' and ends-with($coord,'/time')">
    endif
    if (ids_time_mode .eq. IDS_TIME_MODE_HOMOGENEOUS ) then
      if(array_size .ne. ids_time_size) then
        err_msg = "Element '<xsl:value-of select="@path_doc"/>' has incorrect shape ("//trim(arrstr(shape_array))//"): its coordinate in dimension <xsl:value-of select="number($dimension) + 1"/> ('time') has size "//trim(str(ids_time_size))//"."
        status = -1 
      endif
    endif
    if (ids_time_mode .eq. IDS_TIME_MODE_INDEPENDENT ) then
      if(array_size .ne. 0) then
        err_msg = "Element '<xsl:value-of select="@path_doc"/>' has incorrect shape ("//trim(arrstr(shape_array))//"): dimension <xsl:value-of select="number($dimension) + 1"/> must have size 0."
        status = -1 
      endif
    endif
                </xsl:if>
                endif
                endif
        </xsl:if> 
	<xsl:if test="not(contains($newpath,'/')) and $istimeslice='yes'">
  <xsl:if test="@type='dynamic' and ends-with($coord,'/time')">
  if (status .eq. 0 .and. associated(ids%<xsl:value-of select="$string"/><xsl:value-of select="@name"/>)) then
    shape_array = SHAPE(ids%<xsl:value-of select="$string"/><xsl:value-of select="@name"/>)
    array_size = size(ids%<xsl:value-of select="$string"/><xsl:value-of select="@name"/>,<xsl:value-of select="number($dimension) + 1"/>)
    if (ids_time_mode .eq. IDS_TIME_MODE_HOMOGENEOUS ) then
      if(array_size .ne. ids_time_size) then
        err_msg = "Element '<xsl:value-of select="@path_doc"/>' has incorrect shape ("//trim(arrstr(shape_array))//"): its coordinate in dimension <xsl:value-of select="number($dimension) + 1"/> ('time') has size "//trim(str(ids_time_size))//"."
        status = -1 
        return
      endif
    endif
    if (ids_time_mode .eq. IDS_TIME_MODE_HETEROGENEOUS ) then
      do itime =1, array_size
      if (.not. ids_is_valid(ids%<xsl:value-of select="@name"/>(itime)%time)) then 
        err_msg = "Time coordinate of '<xsl:value-of select="@path_doc"/>' ('<xsl:value-of select="@path_doc"/>/time') has empty values."
        status = -1 
        return
      end if
      end do 
    endif
  endif
  </xsl:if>
</xsl:if>


</xsl:template> 

<xsl:template match='field' mode="resolve_indices">
  <xsl:param name="target"/>
  <xsl:param name="string-resolved"/>
  <xsl:if test="contains($target,'(')">
    <xsl:variable name="indexstr">
      <xsl:apply-templates select="." mode="get_indices">
        <xsl:with-param name="target" select="$target"/>
      </xsl:apply-templates>
    </xsl:variable>
    <xsl:variable name="resolved_indexstr">
    <xsl:if test="matches($indexstr, '^[0-9]+$')">
      <xsl:value-of select="number($indexstr)"/>
    </xsl:if>
    <xsl:if test="matches($indexstr, '^itime|i[1-9]$')">
      <xsl:value-of select="$indexstr"/>
    </xsl:if>
    <xsl:if test="not(matches($indexstr, '^[0-9]+$') or matches($indexstr, '^itime|i[1-9]$'))">
      <xsl:value-of select="concat('ids%',$indexstr)"/>
    </xsl:if>
    </xsl:variable>
    <xsl:apply-templates select="." mode="resolve_indices">
        <xsl:with-param name="target" select="substring-after($target,concat($indexstr,')'))"/>
        <xsl:with-param name="string-resolved" select="concat($string-resolved,substring-before($target,concat($indexstr,')')), concat($resolved_indexstr,')'))"/>
    </xsl:apply-templates>
  </xsl:if>
  <xsl:if test="not(contains($target,'('))">
    <xsl:value-of select="concat($string-resolved, $target)"/>
  </xsl:if>
  </xsl:template>

  <!-- the get_indices function return the sub-string between parenthesis
               process(i1)/coordinate_index ===> i1
        struct(process(i1)/coordinate_index)/substruc ===> process(i1)/coordinate_index
  -->
  <xsl:template match='field' mode="get_indices">
  <xsl:param name="target"/>
  <xsl:variable name="partialindex" select="substring-before(substring-after($target,'('),')')"/>
  <xsl:if test="contains($partialindex,'(')">
    <xsl:value-of select="concat($partialindex,')',substring-before(substring-after($target,concat($partialindex,')')),')'))"/>
  </xsl:if>
  <xsl:if test="not(contains($partialindex,'('))">
    <xsl:value-of select="$partialindex"/>
  </xsl:if>
  </xsl:template>

  <xsl:template match='field' mode="check-target-indices">
      <xsl:param name="coord"/>
      <xsl:param name="relativepathdoc"/>
      <xsl:if test="contains($coord,' OR')">
        <xsl:variable name="target">
          <xsl:if test="not($relativepathdoc='/')">
            <xsl:value-of select="substring-before(substring-after($coord,$relativepathdoc),' OR')"/>
          </xsl:if>
          <xsl:if test="$relativepathdoc='/'">
              <xsl:value-of select="substring-before($coord,' OR')"/>
          </xsl:if>
        </xsl:variable>
        <xsl:apply-templates select="." mode="check_indices">
            <xsl:with-param name="target" select="$target"/>
            <xsl:with-param name="string-resolved" select="''"/>
            <xsl:with-param name="string-error" select="''"/>
        </xsl:apply-templates>
      <xsl:apply-templates select="." mode="check-target-indices">
        <xsl:with-param name="coord" select="substring-after($coord,' OR')"/>
        <xsl:with-param name="relativepathdoc" select="$relativepathdoc"/>
      </xsl:apply-templates>
      </xsl:if>
      <xsl:if test="not(contains($coord,' OR'))">
      <xsl:variable name="target">
        <xsl:if test="not($relativepathdoc='/')">
          <xsl:value-of select="substring-after($coord,$relativepathdoc)"/>
        </xsl:if>
        <xsl:if test="$relativepathdoc='/'">
            <xsl:value-of select="$coord"/>
        </xsl:if>
      </xsl:variable>
      <xsl:apply-templates select="." mode="check_indices">
            <xsl:with-param name="target" select="$target"/>
            <xsl:with-param name="string-resolved" select="''"/>
            <xsl:with-param name="string-error" select="''"/>
        </xsl:apply-templates>
      </xsl:if>
      </xsl:template>



  <xsl:template match='field' mode="check_indices">
      <xsl:param name="target"/>
      <xsl:param name="string-resolved"/>
      <xsl:param name="string-error"/>
      <xsl:if test="contains($target,'(')">
        <xsl:variable name="indexstr">
          <xsl:apply-templates select="." mode="get_indices">
            <xsl:with-param name="target" select="$target"/>
          </xsl:apply-templates>
        </xsl:variable>
        <xsl:variable name="resolved_indexstr">
        <xsl:if test="matches($indexstr, '^[0-9]+$')">
          <xsl:value-of select="$indexstr"/>
        </xsl:if>
        <xsl:if test="matches($indexstr, '^itime|i[1-9]$')">
          <xsl:value-of select="''"/>
        </xsl:if>
        <xsl:if test="not(matches($indexstr, '^[0-9]+$')) and not(matches($indexstr, '^itime|i[1-9]$'))">
          <xsl:value-of select="concat('ids/',$indexstr)"/>
        </xsl:if>
        </xsl:variable>
  <xsl:variable name="indexid_str">
    <xsl:if test="starts-with($target,'.')">  <xsl:value-of select="substring-before(substring-after($target,'.'),'(')"/>_id </xsl:if>
    <xsl:if test="not(starts-with($target,'.'))">  <xsl:value-of select="substring-before($target,'(')"/>_id </xsl:if>
  </xsl:variable>
   <xsl:if test="$resolved_indexstr != ''">
          <xsl:if test="not(matches($resolved_indexstr, '^[0-9]+$'))">
          if (<xsl:value-of select="replace($resolved_indexstr,'/','%')"/>==ids_int_invalid) then
            err_msg = "<xsl:value-of select="replace(replace($resolved_indexstr,'\(','(&quot;//trim(str('),'\)','))//&quot;)')"/> has empty values."
            status = -1
            return
          end if
          </xsl:if>
                if (size(<xsl:value-of select="replace(concat('ids/',$string-resolved,substring-before($target,'(')),'/','%')"/>)&lt;<xsl:value-of select="replace($resolved_indexstr,'/','%')"/>) then
            err_msg = "<xsl:value-of select="concat($string-error,substring-before($target,concat($indexstr,')')),'&quot;//trim(str(',replace($resolved_indexstr,'/','%'),'))//&quot;)' )"/> is not allocated. (Required for <xsl:value-of select="replace(replace(substring-before(@path_doc,'(:'),'\(i','(&quot;//trim(str(i'),'\)','))//&quot;)')"/>)"
            status = -1
            return
          end if
        </xsl:if>
        <xsl:apply-templates select="." mode="check_indices">
            <xsl:with-param name="target" select="substring-after($target,concat($indexstr,')'))"/>
            <xsl:with-param name="string-resolved" select="concat($string-resolved,substring-before($target,concat($indexstr,')')), concat($resolved_indexstr,')') )"/>
            <xsl:with-param name="string-error" select="concat($string-error,substring-before($target,concat($indexstr,')')),'&quot;//trim(str(',replace($resolved_indexstr,'/','%'),'))//&quot;)' )"/>
          </xsl:apply-templates>
      </xsl:if>
</xsl:template>

<xsl:template match='field' mode="possible-coordinates">
        <xsl:param name="coord"/>
        <xsl:param name="relativepathdoc"/>
  <xsl:param name="self"/>
        <xsl:if test="contains($coord,' OR')">
  <xsl:variable name="target">
      <xsl:if test="not($relativepathdoc='/')">
        <xsl:value-of select="replace(substring-before(substring-after($coord,$relativepathdoc),' OR'),'/','%')"/>
      </xsl:if>
      <xsl:if test="$relativepathdoc='/'">
          <xsl:value-of select="replace(substring-before($coord,' OR'),'/','%')"/>
      </xsl:if>
  </xsl:variable>
  <xsl:variable name="resolved_target">
    <xsl:apply-templates select="." mode="resolve_indices">
      <xsl:with-param name="target" select="$target"/>
      <xsl:with-param name="string-resolved" select="''"/>
    </xsl:apply-templates>
  </xsl:variable>
  <xsl:if test="not(contains(substring-before($coord,' OR'),'1...'))">
          if (associated(ids%<xsl:value-of select="$resolved_target"/>)) i = i + 1
  </xsl:if>
  <xsl:apply-templates select="." mode="possible-coordinates">
    <xsl:with-param name="coord" select="substring-after($coord,' OR')"/>
    <xsl:with-param name="relativepathdoc" select="$relativepathdoc"/>
    <xsl:with-param name="self" select="$self"/>
  </xsl:apply-templates>
  </xsl:if>
  <xsl:if test="not(contains($coord,' OR'))">
  <xsl:variable name="target">
    <xsl:if test="not($relativepathdoc='/')">
        <xsl:value-of select="replace(substring-after($coord,$relativepathdoc),'/','%')"/>
    </xsl:if>
    <xsl:if test="$relativepathdoc='/'">
        <xsl:value-of select="replace($coord,'/','%')"/>
    </xsl:if>
  </xsl:variable>
  <xsl:variable name="resolved_target">
    <xsl:apply-templates select="." mode="resolve_indices">
      <xsl:with-param name="target" select="$target"/>
      <xsl:with-param name="string-resolved" select="''"/>
    </xsl:apply-templates>
  </xsl:variable>
  <xsl:if test="not(contains($coord,'1...'))">
      if (associated(ids%<xsl:value-of select="$resolved_target"/>)) i = i + 1
  </xsl:if>
  </xsl:if>
</xsl:template>

<xsl:template match='field' mode="check-possible-coordinates">
        <xsl:param name="coord"/>
        <xsl:param name="relativepathdoc"/>
  <xsl:param name="dimension"/>
  <xsl:param name="self"/>
  <xsl:param name="targetdim"/>
        <xsl:if test="contains($coord,' OR')">
        <xsl:variable name="target">
          <xsl:if test="not($relativepathdoc='/')">
            <xsl:value-of select="replace(substring-before(substring-after($coord,$relativepathdoc),' OR'),'/','%')"/>
          </xsl:if>
          <xsl:if test="$relativepathdoc='/'">
            <xsl:if test="ancestor::IDS/@name='amns_data'">
          <xsl:value-of select="replace(concat(substring-before(substring-before($coord,' OR'),'process(i1)/coordinate_index'),'ids/process(i1)/coordinate_index', substring-after(substring-before($coord,' OR'),'process(i1)/coordinate_index')),'/','%')"/>
            </xsl:if>
            <xsl:if test="not(ancestor::IDS/@name='amns_data')">
              <xsl:value-of select="replace(substring-before($coord,' OR'),'/','%')"/>
            </xsl:if>
          </xsl:if>
        </xsl:variable>
        <xsl:if test="not(contains(substring-before($coord,' OR'),'1...'))">
        if (associated(ids%<xsl:value-of select="$target"/>)) then
          call check_coordinate_size(array_size,size(ids%<xsl:value-of select="$target"/>,<xsl:value-of select="number($targetdim)+1"/>),error,status,&amp;
          "Element '<xsl:value-of select="@path_doc"/>' has incorrect shape ("//trim(arrstr(shape_array))//"): its coordinate in dimension <xsl:value-of select="number($dimension) + 1"/> ('<xsl:value-of select="substring-before(substring-after($coord,$relativepathdoc),' OR')"/>') has size "//trim(str(size(ids%<xsl:value-of select="$target"/>,<xsl:value-of select="number($targetdim)+1"/>)))//".",&amp;
          err_msg)
        endif 
        </xsl:if>
  <xsl:apply-templates select="." mode="check-possible-coordinates">
    <xsl:with-param name="coord" select="substring-after($coord,' OR')"/>
    <xsl:with-param name="relativepathdoc" select="$relativepathdoc"/>
    <xsl:with-param  name="dimension" select="$dimension"/>
    <xsl:with-param  name="self" select="$self"/>
    <xsl:with-param  name="targetdim" select="$targetdim"/>
  </xsl:apply-templates>
  </xsl:if>
  <xsl:if test="not(contains($coord,' OR'))">
        <xsl:variable name="target">
          <xsl:if test="not($relativepathdoc='/')">
            <xsl:value-of select="replace(substring-after($coord,$relativepathdoc),'/','%')"/>
          </xsl:if>
          <xsl:if test="$relativepathdoc='/'">
            <xsl:if test="ancestor::IDS/@name='amns_data'">
          <xsl:value-of select="replace(concat(substring-before($coord,'process(i1)/coordinate_index'),'ids/process(i1)/coordinate_index', substring-after($coord,'process(i1)/coordinate_index')),'/','%')"/>
            </xsl:if>
            <xsl:if test="not(ancestor::IDS/@name='amns_data')">
              <xsl:value-of select="replace($coord,'/','%')"/>
            </xsl:if>
          </xsl:if>
        </xsl:variable>
        <xsl:if test="not(contains($coord,'1...'))">
        if (associated(ids%<xsl:value-of select="$target"/>)) then
          call check_coordinate_size(array_size,size(ids%<xsl:value-of select="$target"/>,<xsl:value-of select="number($targetdim)+1"/>),error,status,&amp;
          "Element '<xsl:value-of select="@path_doc"/>' has incorrect shape ("//trim(arrstr(shape_array))//"): its coordinate in dimension <xsl:value-of select="number($dimension) + 1"/> ('<xsl:value-of select="substring-after($coord,$relativepathdoc)"/>') has size "//trim(str(size(ids%<xsl:value-of select="$target"/>,<xsl:value-of select="number($targetdim)+1"/>)))//".",&amp;
          err_msg)
        endif 
        </xsl:if>
  </xsl:if>
</xsl:template>

<xsl:template match='field' mode="check-specific-coordinates">
        <xsl:param name="coord"/>
        <xsl:param name="relativepathdoc"/>
  <xsl:param name="dimension"/>
  <xsl:param name="self"/>
        <xsl:if test="contains($coord,' OR')">
        <xsl:variable name="target" select="replace(substring-before(substring-after($coord,$relativepathdoc),' OR'),'/','%')"/>
        <xsl:if test="contains(substring-before($coord,' OR'),'1...')">
        if (error .and. array_size .eq. <xsl:value-of select="substring-after($coord,'1...')"/>) then  
        error = .FALSE. 
        status = 0
        endif
        </xsl:if>
  <xsl:apply-templates select="." mode="check-specific-coordinates">
    <xsl:with-param name="coord" select="substring-after($coord,' OR')"/>
    <xsl:with-param name="relativepathdoc" select="$relativepathdoc"/>
    <xsl:with-param  name="dimension" select="$dimension"/>
    <xsl:with-param  name="self" select="$self"/>
  </xsl:apply-templates>
  </xsl:if>
  <xsl:if test="not(contains($coord,' OR'))">
        <xsl:variable name="target" select="replace(substring-after($coord,$relativepathdoc),'/','%')"/>
        <xsl:if test="contains($coord,'1...')">
        if (error .and. array_size .eq. <xsl:value-of select="substring-after($coord,'1...')"/>) then   
        error = .FALSE. 
        status = 0
        endif
        </xsl:if>
  </xsl:if>
</xsl:template>



<xsl:template match="field" mode="VALIDATE_DESCENDANT_SINGLE_2D">
        <xsl:param name="currpath"/>
        <xsl:apply-templates select="." mode="VALIDATE_DESCENDANT_SINGLE">
                <xsl:with-param name="currpath" select="$currpath"/>
                <xsl:with-param name="dimension" select="'0'"/>
        </xsl:apply-templates>
        <xsl:apply-templates select="." mode="VALIDATE_DESCENDANT_SINGLE">
                <xsl:with-param name="currpath" select="$currpath"/>
                <xsl:with-param name="dimension" select="'1'"/>
        </xsl:apply-templates>
</xsl:template> 

<xsl:template match="field" mode="VALIDATE_DESCENDANT_SINGLE_3D">
        <xsl:param name="currpath"/>
        <xsl:apply-templates select="." mode="VALIDATE_DESCENDANT_SINGLE">
                <xsl:with-param name="currpath" select="$currpath"/>
                <xsl:with-param name="dimension" select="'0'"/>
        </xsl:apply-templates>
        <xsl:apply-templates select="." mode="VALIDATE_DESCENDANT_SINGLE">
                <xsl:with-param name="currpath" select="$currpath"/>
                <xsl:with-param name="dimension" select="'1'"/>
        </xsl:apply-templates>
        <xsl:apply-templates select="." mode="VALIDATE_DESCENDANT_SINGLE">
                <xsl:with-param name="currpath" select="$currpath"/>
                <xsl:with-param name="dimension" select="'2'"/>
        </xsl:apply-templates>
</xsl:template> 

<xsl:template match="field" mode="VALIDATE_DESCENDANT_SINGLE_4D">
        <xsl:param name="currpath"/>
        <xsl:apply-templates select="." mode="VALIDATE_DESCENDANT_SINGLE">
                <xsl:with-param name="currpath" select="$currpath"/>
                <xsl:with-param name="dimension" select="'0'"/>
        </xsl:apply-templates>
        <xsl:apply-templates select="." mode="VALIDATE_DESCENDANT_SINGLE">
                <xsl:with-param name="currpath" select="$currpath"/>
                <xsl:with-param name="dimension" select="'1'"/>
        </xsl:apply-templates>
        <xsl:apply-templates select="." mode="VALIDATE_DESCENDANT_SINGLE">
                <xsl:with-param name="currpath" select="$currpath"/>
                <xsl:with-param name="dimension" select="'2'"/>
        </xsl:apply-templates>
        <xsl:apply-templates select="." mode="VALIDATE_DESCENDANT_SINGLE">
                <xsl:with-param name="currpath" select="$currpath"/>
                <xsl:with-param name="dimension" select="'3'"/>
        </xsl:apply-templates>
</xsl:template> 

<xsl:template match="field" mode="VALIDATE_DESCENDANT_SINGLE_5D">
        <xsl:param name="currpath"/>
        <xsl:apply-templates select="." mode="VALIDATE_DESCENDANT_SINGLE">
                <xsl:with-param name="currpath" select="$currpath"/>
                <xsl:with-param name="dimension" select="'0'"/>
        </xsl:apply-templates>
        <xsl:apply-templates select="." mode="VALIDATE_DESCENDANT_SINGLE">
                <xsl:with-param name="currpath" select="$currpath"/>
                <xsl:with-param name="dimension" select="'1'"/>
        </xsl:apply-templates>
        <xsl:apply-templates select="." mode="VALIDATE_DESCENDANT_SINGLE">
                <xsl:with-param name="currpath" select="$currpath"/>
                <xsl:with-param name="dimension" select="'2'"/>
        </xsl:apply-templates>
        <xsl:apply-templates select="." mode="VALIDATE_DESCENDANT_SINGLE">
                <xsl:with-param name="currpath" select="$currpath"/>
                <xsl:with-param name="dimension" select="'3'"/>
        </xsl:apply-templates>
        <xsl:apply-templates select="." mode="VALIDATE_DESCENDANT_SINGLE">
                <xsl:with-param name="currpath" select="$currpath"/>
                <xsl:with-param name="dimension" select="'4'"/>
        </xsl:apply-templates>
</xsl:template> 

<xsl:template match="field" mode="VALIDATE_DESCENDANT_SINGLE_6D">
        <xsl:param name="currpath"/>
        <xsl:apply-templates select="." mode="VALIDATE_DESCENDANT_SINGLE">
                <xsl:with-param name="currpath" select="$currpath"/>
                <xsl:with-param name="dimension" select="'0'"/>
        </xsl:apply-templates>
        <xsl:apply-templates select="." mode="VALIDATE_DESCENDANT_SINGLE">
                <xsl:with-param name="currpath" select="$currpath"/>
                <xsl:with-param name="dimension" select="'1'"/>
        </xsl:apply-templates>
        <xsl:apply-templates select="." mode="VALIDATE_DESCENDANT_SINGLE">
                <xsl:with-param name="currpath" select="$currpath"/>
                <xsl:with-param name="dimension" select="'2'"/>
        </xsl:apply-templates>
        <xsl:apply-templates select="." mode="VALIDATE_DESCENDANT_SINGLE">
                <xsl:with-param name="currpath" select="$currpath"/>
                <xsl:with-param name="dimension" select="'3'"/>
        </xsl:apply-templates>
        <xsl:apply-templates select="." mode="VALIDATE_DESCENDANT_SINGLE">
                <xsl:with-param name="currpath" select="$currpath"/>
                <xsl:with-param name="dimension" select="'4'"/>
        </xsl:apply-templates>
        <xsl:apply-templates select="." mode="VALIDATE_DESCENDANT_SINGLE">
                <xsl:with-param name="currpath" select="$currpath"/>
                <xsl:with-param name="dimension" select="'5'"/>
        </xsl:apply-templates>
</xsl:template> 

<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<!-- IDS_GET MODULE, PER IDS                                                 -->
<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<xsl:template match="IDS" mode="get_struct">
  <xsl:result-document href="{@name}_get.f90">
module <xsl:value-of select="@name"/>_get_struct

use utilities_get_struct

interface ids_get
  module procedure get_struct_ids_<xsl:value-of select="local:unique_name(@name)"/> <!-- subroutine for the whole IDS -->
end interface 

interface ids_get_sample
  module procedure get_sample_struct_ids_<xsl:value-of select="local:unique_name(@name)"/> <!-- subroutine for the whole IDS -->
end interface 

interface ids_get_struct
  <xsl:for-each select=".//field[@data_type='structure' or @data_type='struct_array']">
    <xsl:variable name="this-type">
      <xsl:choose>
        <xsl:when test="@structure_reference='self'">
          <xsl:value-of select="@name"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="@structure_reference"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="this-ids" select="ancestor::IDS/@name"/>
    <xsl:if test="not (preceding::field[@structure_reference=$this-type and ancestor::IDS/@name=$this-ids] or /IDSs/utilities/field/@name=$this-type)">
  module procedure get_struct_ids_<xsl:value-of select="local:unique_name($this-type)"/>
    </xsl:if>
  </xsl:for-each>
end interface

 contains 

<!-- <xsl:call-template name="isCriticalFuncCtx"/> done in utilities! -->

<!-- subroutine for the whole IDS -->
!!! Routines to GET the full IDS !!!
subroutine get_struct_ids_<xsl:value-of select="local:unique_name(@name)"/>(pulsectx, name, IDS, retstatus)
  use ids_schemas_<xsl:value-of select="@name"/>
  use al_low_level_wrap
  use al_get_policy
  implicit none

  integer(ids_int), intent(out), optional :: retstatus
  integer(ids_int) :: status = 0
  character*(*) :: name
  integer(ids_int) :: pulsectx, opctx, aosctx
  type(ids_<xsl:value-of select="@name"/>) :: IDS
  ! internal variables declaration
  logical :: timedparent
  integer(ids_int) :: aoslen, i, lenstring
  integer(ids_int) :: size1, size2, size3, size4, size5, size6, size7
  character(len=100000) :: longstring
  character(len=300) :: timepath
  character(*), parameter :: path = ''
  
  call IDS%check_name_<xsl:value-of select="@name"/>(name, status)
  if(status.ne.0) then
    write(*,*) 'Error in get_struct_ids_<xsl:value-of select="local:unique_name(@name)"/>'
    if(present(retstatus)) retstatus = status
    return
  end if 
  call al_begin_global_action(pulsectx, name, READ_OP, opctx, status) 
  if (status.ne.0) then
     !! error when trying to get new ctx => stop!
     write(*,*) 'Error in al_begin_global_action (from ids_get for IDS <xsl:value-of select="@name"/>)'
     <!-- The begin failed, so no context was created: there is nothing to end,
          and opctx holds no defined value. Return the real status, and return
          *here*: falling through ran the whole field block against a context
          that was never opened. -->
     if (present(retstatus)) then
        retstatus = status
     else
        STOP
     end if
     return
  end if
  
  call al_bind_readback_plugins(opctx, status)
  if (status.ne.0) then
     write(*,*) 'Error in al_bind_readback_plugins (from ids_get for IDS <xsl:value-of select="@name"/>)'
     <!-- opctx was opened by this routine and is still open, so end it before
          returning. Return the real status, not opctx: that is a context id,
          not a status. Falling through ran the rest of the routine against a
          context whose operation had already failed. -->
     if (present(retstatus)) then
        retstatus = status
     else
        STOP
     end if
     call al_end_action(opctx, status)
     return
  end if

  timedparent=.false.
  call set_c_data(IDS,.true.)
  <!-- The skip log describes one operation, not the process. -->
  call al_reset_skip_log()

  <xsl:apply-templates select="./field" mode="GET_FIELD">
    <xsl:with-param name="structvar" select="'IDS'"/>
    <xsl:with-param name="contextvar" select="'opctx'"/>
    <xsl:with-param name="timedparentexpr" select="''"/>
    <xsl:with-param name="root" select="'yes'"/>
  </xsl:apply-templates>

  call al_unbind_readback_plugins(opctx, status)
  if (status.ne.0) then
     write(*,*) 'Error in al_unbind_readback_plugins (from ids_get for IDS <xsl:value-of select="@name"/>)'
     <!-- opctx was opened by this routine and is still open, so end it before
          returning. Return the real status, not opctx: that is a context id,
          not a status. Falling through ran the rest of the routine against a
          context whose operation had already failed. -->
     if (present(retstatus)) then
        retstatus = status
     else
        STOP
     end if
     call al_end_action(opctx, status)
     return
  end if

  call al_end_action(opctx, status)

  <!-- retstatus here is al_end_action's status, not the read's. If the read left
       fields unset because a conversion layer refused them, say so: PARTIAL_READ
       is positive, so it can never be confused with a C-ABI status, and it still
       trips the `status.ne.0` test callers already write. al_get_policy has the
       list of paths. -->
  if (present(retstatus)) then
     if (status == 0 .and. al_get_skipped_count() > 0) then
        retstatus = PARTIAL_READ
     else
        retstatus = status
     end if
  end if
  return
end subroutine get_struct_ids_<xsl:value-of select="local:unique_name(@name)"/>


subroutine get_sample_struct_ids_<xsl:value-of select="local:unique_name(@name)"/>(pulsectx, name, IDS,  tmin, tmax, dtime, interp, retstatus)
use ids_schemas_<xsl:value-of select="@name"/>
  use al_low_level_wrap
  use al_get_policy
  implicit none
  integer(ids_int), intent(out), optional :: retstatus
  character*(*) :: name
  integer(ids_int) :: pulsectx, opctx, aosctx
  integer(ids_int), intent(in) :: interp
  real(ids_real), intent(in) ::  tmin, tmax
  real(ids_real) , intent(in), dimension(:) :: dtime
  type(ids_<xsl:value-of select="@name"/>) :: IDS
  !--- local variables ---
  integer(ids_int) :: status = 0
  integer :: storedtimemode = IDS_TIME_MODE_UNKNOWN
  real(ids_real), dimension(:), pointer :: cdtime
  logical :: timedparent
  integer(ids_int) :: aoslen, i, lenstring
  integer(ids_int) :: size1, size2, size3, size4, size5, size6, size7
  character(len=100000) :: longstring
  character(len=300) :: timepath
  character(*), parameter :: path = ''

  if (tmax &lt; tmin) then
    status = -1
    write(*,*) 'GET_SAMPLE: error, tmax should be greater or equals to tmin'
    if (present(retstatus)) retstatus = status
    return
  end if

  if ((interp .ne. 0) .and. (size(dtime) == 0)) then
    status = -1
    write(*,*) 'GET_SAMPLE: error, interpolation mode should be 0 with no resampling (size(dtime) == 0)'
    if (present(retstatus)) retstatus = status
    return
  end if

  if ((interp == 0) .and. (size(dtime) &gt;= 1)) then
    status = -1
    write(*,*) 'GET_SAMPLE: error, interpolation mode should be specified (non zero) with resampling (size(dtime) &gt;= 1)'
    if (present(retstatus)) retstatus = status
    return
  end if


  storedtimemode = IDS_TIME_MODE_UNKNOWN
  call al_begin_global_action(pulsectx, name, READ_OP, opctx, status) 
  if (status.ne.0) then
     !! error when trying to get new ctx => stop!
     write(*,*) 'Error in al_begin_slice_action (from get_sample for IDS <xsl:value-of select="@name"/>)'     
     if (present(retstatus)) then
      retstatus = status
     end if
     return
  endif
     ! Get homogeneous_time
     call get_int(opctx, "ids_properties/homogeneous_time",'', storedtimemode, status)
     if(isErrorCritical(status, opctx, "ids_properties/homogeneous_time")) then
        if (present(retstatus)) then
           retstatus = status
        endif
        call al_end_action(opctx, status)
        return
     endif
     <!-- Not dead code once a refusal can be tolerated: isErrorCritical may now
          return .FALSE. for a path the conversion layer refused. homogeneous_time
          is not an optional field (everything below depends on it), so this
          stays a hard stop, and it is why the band test must never be read as
          "tolerate everywhere". -->
     if (status.ne.0) then
        write(*,*) 'Error in get_int "ids_properties/homogeneous_time" (from get_sample for IDS <xsl:value-of select="@name"/>)'  
        if (present(retstatus)) then
           retstatus = status
        endif
        call al_end_action(opctx, status)
        return
     end if
  call al_end_action(opctx, status)

  if(storedtimemode == IDS_TIME_MODE_UNKNOWN) then
        write(*,*) 'GET_SAMPLE: error reading homogeneous time for <xsl:value-of select="@name"/> IDS'
        <!-- This returned without assigning retstatus at all, so the caller saw
             whatever it had passed in, normally 0, i.e. success. -->
        if (present(retstatus)) retstatus = UNKNOWN_ERR
        return
  end if

  allocate(cdtime(size(dtime)))
  cdtime = dtime(:)
  call al_begin_timerange_action(pulseCtx, name, READ_OP, tmin, tmax, cdtime, size(dtime), interp, opctx, status)
  deallocate(cdtime)
	if (status.ne.0) then
    write(*,*) 'GET_SAMPLE: error calling al_begin_timerange_action for <xsl:value-of select="@name"/> IDS'
    if (present(retstatus)) retstatus = status
		return
  end if

  call al_bind_readback_plugins(opctx, status)
  if (status.ne.0) then
     write(*,*) 'GET_SAMPLE: Error in al_bind_readback_plugins (from ids_get for IDS <xsl:value-of select="@name"/>)'
     <!-- opctx was opened by this routine and is still open, so end it before
          returning. Return the real status, not opctx: that is a context id,
          not a status. Falling through ran the rest of the routine against a
          context whose operation had already failed. -->
     if (present(retstatus)) then
        retstatus = status
     else
        STOP
     end if
     call al_end_action(opctx, status)
     return
  end if

  timedparent = .false.
 	call set_c_data(IDS,.true.)
  <!-- The skip log describes one operation, not the process. -->
  call al_reset_skip_log()

  <xsl:apply-templates select="./field" mode="GET_FIELD">
    <xsl:with-param name="structvar" select="'IDS'"/>
    <xsl:with-param name="contextvar" select="'opctx'"/>
    <xsl:with-param name="timedparentexpr" select="''"/>
    <xsl:with-param name="root" select="'yes'"/>
  </xsl:apply-templates>


	call al_unbind_readback_plugins(opctx, status)
  if (status.ne.0) then
     write(*,*) 'Error in al_unbind_readback_plugins (from ids_get for IDS <xsl:value-of select="@name"/>)'
     <!-- opctx was opened by this routine and is still open, so end it before
          returning. Return the real status, not opctx: that is a context id,
          not a status. Falling through ran the rest of the routine against a
          context whose operation had already failed. -->
     if (present(retstatus)) then
        retstatus = status
     else
        STOP
     end if
     call al_end_action(opctx, status)
     return
  end if

  call al_end_action(opctx, status)

  <!-- retstatus here is al_end_action's status, not the read's. If the read left
       fields unset because a conversion layer refused them, say so: PARTIAL_READ
       is positive, so it can never be confused with a C-ABI status, and it still
       trips the `status.ne.0` test callers already write. al_get_policy has the
       list of paths. -->
  if (present(retstatus)) then
     if (status == 0 .and. al_get_skipped_count() > 0) then
        retstatus = PARTIAL_READ
     else
        retstatus = status
     end if
  end if
  return


end subroutine get_sample_struct_ids_<xsl:value-of select="local:unique_name(@name)"/>

<xsl:for-each select=".//field[@data_type='structure' or @data_type='struct_array']">
  <xsl:variable name="this-type">
    <xsl:choose>
      <xsl:when test="@structure_reference='self'">
        <xsl:value-of select="@name"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="@structure_reference"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  <xsl:variable name="this-ids" select="ancestor::IDS/@name"/>
  <xsl:if test="not (preceding::field[@structure_reference=$this-type and ancestor::IDS/@name=$this-ids] or /IDSs/utilities/field/@name=$this-type)">
subroutine get_struct_ids_<xsl:value-of select="local:unique_name($this-type)"/>(ctx, path, struct, timemode, timedparent, retstatus)
  use ids_schemas_<xsl:value-of select="ancestor::IDS/@name"/>
  use al_low_level_wrap
  implicit none

  integer(ids_int), intent(in) :: ctx
  character*(*), intent(in) :: path
  type(ids_<xsl:value-of select="$this-type"/>), intent(inout) :: struct      
  logical, intent(in) :: timedparent
  integer, intent(in) :: timemode
  integer(ids_int), intent(out) :: retstatus
  integer(ids_int) :: i, aoslen, lenstring, aosctx
  integer(ids_int) :: size1, size2, size3, size4, size5, size6, size7
  integer :: status
  character(len=100000) :: longstring
  character(len=300) :: timepath

  <xsl:apply-templates select="./field" mode="GET_FIELD">
    <xsl:with-param name="structvar" select="'struct'"/>
    <xsl:with-param name="contextvar" select="'ctx'"/>
    <xsl:with-param name="timedparentexpr" select="'timedparent.or.'"/>
  </xsl:apply-templates>      
   retstatus = 0
end subroutine get_struct_ids_<xsl:value-of select="local:unique_name($this-type)"/>

  </xsl:if>
</xsl:for-each>

end module    
  </xsl:result-document>
</xsl:template>



<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<!-- IDS_GET_SLICE MODULE, PER IDS                                           -->
<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<xsl:template match="IDS" mode="get_slice_struct">
  <xsl:result-document href="{@name}_get_slice.f90">
module <xsl:value-of select="@name"/>_get_slice_struct

use utilities_get_struct
use <xsl:value-of select="@name"/>_get_struct

interface ids_get_slice
  module procedure get_slice_struct_ids_<xsl:value-of select="local:unique_name(@name)"/> <!-- subroutine for the whole IDS -->
end interface

 contains 

<!-- subroutine for the whole IDS -->
!!! Routines to GET one time slice of an IDS, with time interpolation !!!
subroutine get_slice_struct_ids_<xsl:value-of select="local:unique_name(@name)"/>(pulsectx, name, IDS, twant, interpol, retstatus)
  use ids_schemas_<xsl:value-of select="@name"/>
  use al_low_level_wrap
  use al_get_policy
  implicit none

  integer(ids_int), intent(out), optional :: retstatus
  integer(ids_int) :: status = 0
  character*(*) :: name
  real(ids_real), intent(in) :: twant
  integer(ids_int), intent(in) :: interpol
  integer(ids_int) :: pulsectx, opctx, aosctx
  type(ids_<xsl:value-of select="@name"/>) :: IDS

<xsl:choose>
  <xsl:when test="@type='constant'">
  ! for static IDSes only GET method is called
  CALL get_struct_ids_<xsl:value-of select="local:unique_name(@name)"/>(pulsectx, name, IDS, status)
  if(present(retstatus)) retstatus = status
    return
  </xsl:when>
  <xsl:otherwise>
  ! internal variables declaration
  logical :: timedparent
  integer(ids_int) :: aoslen, i, lenstring
  integer(ids_int) :: size1, size2, size3, size4, size5, size6, size7
  character(len=100000) :: longstring
  character(len=300) :: timepath
  character(*), parameter :: path = ''
  
  call IDS%check_name_<xsl:value-of select="@name"/>(name, status)
  if(status.ne.0) then
    write(*,*) 'Error in get_slice_struct_ids_<xsl:value-of select="local:unique_name(@name)"/>'
    if(present(retstatus)) retstatus = status
    return
  end if 
 
  call al_begin_slice_action(pulsectx, name, READ_OP, twant, interpol, opctx, status) 
  if (status.ne.0) then
     !! error when trying to get new ctx => stop!
     write(*,*) 'Error in al_begin_slice_action (from ids_get_slice for IDS <xsl:value-of select="@name"/>)'    
     <!-- The begin failed, so no context was created: there is nothing to end,
          and opctx holds no defined value. Return the real status, and return
          *here*: falling through ran the whole field block against a context
          that was never opened. -->
     if (present(retstatus)) then
        retstatus = status
     else
        STOP
     end if
     return
  end if
  
  call al_bind_readback_plugins(opctx, status)
  if (status.ne.0) then
     write(*,*) 'Error in al_bind_readback_plugins (from ids_get_slice for IDS <xsl:value-of select="@name"/>)'
     <!-- opctx was opened by this routine and is still open, so end it before
          returning. Return the real status, not opctx: that is a context id,
          not a status. Falling through ran the rest of the routine against a
          context whose operation had already failed. -->
     if (present(retstatus)) then
        retstatus = status
     else
        STOP
     end if
     call al_end_action(opctx, status)
     return
  end if

  timedparent=.false.
  call set_c_data(IDS,.true.)
  <!-- The skip log describes one operation, not the process. -->
  call al_reset_skip_log()

  <xsl:apply-templates select="./field" mode="GET_FIELD">
    <xsl:with-param name="structvar" select="'IDS'"/>
    <xsl:with-param name="contextvar" select="'opctx'"/>
    <xsl:with-param name="timedparentexpr" select="''"/>
    <xsl:with-param name="root" select="'yes'"/>
  </xsl:apply-templates>

  call al_unbind_readback_plugins(opctx, status)
  if (status.ne.0) then
     write(*,*) 'Error in al_unbind_readback_plugins (from ids_get_slice for IDS <xsl:value-of select="@name"/>)'
     <!-- opctx was opened by this routine and is still open, so end it before
          returning. Return the real status, not opctx: that is a context id,
          not a status. Falling through ran the rest of the routine against a
          context whose operation had already failed. -->
     if (present(retstatus)) then
        retstatus = status
     else
        STOP
     end if
     call al_end_action(opctx, status)
     return
  end if

  call al_end_action(opctx, status)

  <!-- retstatus here is al_end_action's status, not the read's. If the read left
       fields unset because a conversion layer refused them, say so: PARTIAL_READ
       is positive, so it can never be confused with a C-ABI status, and it still
       trips the `status.ne.0` test callers already write. al_get_policy has the
       list of paths. -->
  if (present(retstatus)) then
     if (status == 0 .and. al_get_skipped_count() > 0) then
        retstatus = PARTIAL_READ
     else
        retstatus = status
     end if
  end if
  return

  </xsl:otherwise>
</xsl:choose>
    
end subroutine get_slice_struct_ids_<xsl:value-of select="local:unique_name(@name)"/>

end module    
  </xsl:result-document>
</xsl:template>












<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<!--                                                                         -->
<!--                                TEMPLATES                                -->
<!--                                                                         -->
<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->




<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<!-- IDS_DELETE TEMPLATE                                                     -->
<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<xsl:template match="field" mode="DELETE">
  <xsl:param name="field_path"/>

  <xsl:variable name="updated_field_path">
    <xsl:choose>
      <xsl:when test="$field_path">
        <xsl:value-of select="concat(substring($field_path,1,string-length($field_path)-1),'/',@name,'&quot;')"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="concat('&quot;',@name,'&quot;')"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  
  <xsl:choose>
    <xsl:when test="@data_type='structure'">
      <xsl:apply-templates select="field" mode="DELETE">
        <xsl:with-param name ="field_path" select="$updated_field_path"/>
      </xsl:apply-templates>
    </xsl:when>
    <!-- Through the per-IDS wrapper rather than straight to al_delete_data, so
         a refusal the interposing layer declares here is recorded rather than
         dropped on the floor with the status. -->
    <xsl:otherwise>call delete_field_<xsl:value-of select="local:unique_name(ancestor::IDS/@name)"/>(opctx, <xsl:value-of select="$updated_field_path"/>, status)
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>





<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<!-- IDS_DEALLOCATE TEMPLATE                                                 -->
<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<xsl:template match="field" mode="DEALLOCATE_FIELD">
  <xsl:param name="idxpath"/>   
  <!-- build the complete path of the current field -->
  <xsl:param name="currentidxpath" select="concat($idxpath,'%',@name)"/>

  <xsl:choose>

    <!-- Case of a struct array -->
    <xsl:when test="@data_type='struct_array'">  
  ! deallocate <xsl:value-of select="$currentidxpath"/>
  if (associated(struct_in<xsl:value-of select="$currentidxpath"/>)) then
    do i = 1,size(struct_in<xsl:value-of select = "$currentidxpath"/>)
      call ids_deallocate_struct_<xsl:value-of select="local:unique_name(@structure_reference)"/>(struct_in<xsl:value-of select = "$currentidxpath"/>(i),c_data)
    enddo
    deallocate(struct_in<xsl:value-of select="$currentidxpath"/>)
  endif
    </xsl:when>

    <!-- Case of a struct array -->
    <xsl:when test="@data_type='structure'">  
  call ids_deallocate_struct_<xsl:value-of select="local:unique_name(@structure_reference)"/>(struct_in<xsl:value-of select = "$currentidxpath"/>,c_data)
    </xsl:when>

    <!-- Case of a string data, only Fortran string is there (C array was copied) -->
    <xsl:when test="@data_type='str_type' or @data_type='STR_0D' or @data_type='str_1d_type' or @data_type='STR_1D'"> 
  ! deallocate <xsl:value-of select="$currentidxpath"/>
  if (associated(struct_in<xsl:value-of select="$currentidxpath"/>)) then
    deallocate(struct_in<xsl:value-of select="$currentidxpath"/>)
  endif
    </xsl:when>

    <!-- Case of all other vector data -->
    <xsl:when test="@data_type='flt_1d_type' or @data_type='int_1d_type' or 
                    @data_type='FLT_1D' or @data_type='INT_1D' or @data_type='CPX_1D'">
  ! deallocate <xsl:value-of select="$currentidxpath"/>
  if (associated(struct_in<xsl:value-of select="$currentidxpath"/>)) then
    if (c_data) then
      call c_free(C_LOC(struct_in<xsl:value-of select="$currentidxpath"/>(1)))
    else
      deallocate(struct_in<xsl:value-of select="$currentidxpath"/>)
    endif
    nullify(struct_in<xsl:value-of select="$currentidxpath"/>)
  endif
    </xsl:when>

    <xsl:when test="@data_type='flt_2d_type' or @data_type='int_2d_type' or 
                    @data_type='FLT_2D' or @data_type='INT_2D' or @data_type='CPX_2D'">
  ! deallocate <xsl:value-of select="$currentidxpath"/>
  if (associated(struct_in<xsl:value-of select="$currentidxpath"/>)) then
    if (c_data) then
      call c_free(C_LOC(struct_in<xsl:value-of select="$currentidxpath"/>(1,1)))
    else
      deallocate(struct_in<xsl:value-of select="$currentidxpath"/>)
    endif
    nullify(struct_in<xsl:value-of select="$currentidxpath"/>)
  endif
    </xsl:when>

    <xsl:when test="@data_type='FLT_3D' or @data_type='INT_3D' or @data_type='CPX_3D'">
  ! deallocate <xsl:value-of select="$currentidxpath"/>
  if (associated(struct_in<xsl:value-of select="$currentidxpath"/>)) then
    if (c_data) then
      call c_free(C_LOC(struct_in<xsl:value-of select="$currentidxpath"/>(1,1,1)))
    else
      deallocate(struct_in<xsl:value-of select="$currentidxpath"/>)
    endif
    nullify(struct_in<xsl:value-of select="$currentidxpath"/>)
  endif
    </xsl:when>

    <xsl:when test="@data_type='FLT_4D' or @data_type='INT_4D' or @data_type='CPX_4D'">
  ! deallocate <xsl:value-of select="$currentidxpath"/>
  if (associated(struct_in<xsl:value-of select="$currentidxpath"/>)) then
    if (c_data) then
      call c_free(C_LOC(struct_in<xsl:value-of select="$currentidxpath"/>(1,1,1,1)))
    else
      deallocate(struct_in<xsl:value-of select="$currentidxpath"/>)
    endif
    nullify(struct_in<xsl:value-of select="$currentidxpath"/>)
  endif
    </xsl:when>

    <xsl:when test="@data_type='FLT_5D' or @data_type='INT_5D' or @data_type='CPX_5D'">
  ! deallocate <xsl:value-of select="$currentidxpath"/>
  if (associated(struct_in<xsl:value-of select="$currentidxpath"/>)) then
    if (c_data) then
      call c_free(C_LOC(struct_in<xsl:value-of select="$currentidxpath"/>(1,1,1,1,1)))
    else
      deallocate(struct_in<xsl:value-of select="$currentidxpath"/>)
    endif
    nullify(struct_in<xsl:value-of select="$currentidxpath"/>)
  endif
    </xsl:when>

    <xsl:when test="@data_type='FLT_6D' or @data_type='INT_6D' or @data_type='CPX_6D'">
  ! deallocate <xsl:value-of select="$currentidxpath"/>
  if (associated(struct_in<xsl:value-of select="$currentidxpath"/>)) then
    if (c_data) then
      call c_free(C_LOC(struct_in<xsl:value-of select="$currentidxpath"/>(1,1,1,1,1,1)))
    else
      deallocate(struct_in<xsl:value-of select="$currentidxpath"/>)
    endif
    nullify(struct_in<xsl:value-of select="$currentidxpath"/>)
  endif
    </xsl:when>

    <!-- Case of scalar data (just to differenciate with errors "otherwise") -->
    <xsl:when test="@data_type='int_type' or @data_type='INT_0D' or 
                    @data_type='flt_type' or @data_type='FLT_0D' or 
                    @data_type='CPX_0D'">
    </xsl:when>

    <!-- Error case: could throw error or generate code that can't compile? -->
    <xsl:otherwise>
      ! Deallocate <xsl:value-of select="$currentidxpath"/> : PROBLEM: UNIDENTIFIED TYPE !!!
      <xsl:message select="concat('PROBLEM : UNIDENTIFIED TYPE detected in DEALLOCATE routine for ',@path)" terminate="yes"/>
    </xsl:otherwise>
  </xsl:choose>

</xsl:template>




<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<!-- IDS_COPY TEMPLATE                                                       -->
<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<xsl:template match="field" mode="COPY_FIELD">
  <xsl:param name="idxpath"/>   <!-- passed current path -->
  <!-- build the complete path of the current field -->
  <xsl:param name="currentidxpath" select="concat($idxpath,'%',@name)"/>

  <xsl:choose>

    <!-- Case of a struct array -->
    <xsl:when test="@data_type='struct_array'">  
  ! Copy <xsl:value-of select="$currentidxpath"/>
  if (associated(struct_in<xsl:value-of select = "$currentidxpath"/>)) then  
    allocate(struct_out<xsl:value-of select="$currentidxpath"/>(size(struct_in<xsl:value-of select = "$currentidxpath"/>)))
    do i = 1,size(struct_in<xsl:value-of select = "$currentidxpath"/>)
      call ids_copy_struct_<xsl:value-of select="local:unique_name(@structure_reference)"/>(struct_in<xsl:value-of select = "$currentidxpath"/>(i), struct_out<xsl:value-of select = "$currentidxpath"/>(i))
    enddo
  endif
    </xsl:when>

    <!-- Case of a simple structure -->
    <xsl:when test="@data_type='structure'">  
  call ids_copy_struct_<xsl:value-of select="local:unique_name(@structure_reference)"/>(struct_in<xsl:value-of select = "$currentidxpath"/>, struct_out<xsl:value-of select = "$currentidxpath"/>)
    </xsl:when>

    <!-- 0D scalar, integer data -->
    <xsl:when test="@data_type='int_type' or @data_type='INT_0D'">
  ! Copy <xsl:value-of select="$currentidxpath"/>
  if (struct_in<xsl:value-of select="$currentidxpath"/>/=ids_int_invalid)  then
    struct_out<xsl:value-of select="$currentidxpath"/> = &amp;
    struct_in<xsl:value-of select="$currentidxpath"/>
  endif
    </xsl:when>

    <!-- 0D scalar, float data -->
    <xsl:when test="@data_type='flt_type' or @data_type='FLT_0D'">
  ! Copy <xsl:value-of select="$currentidxpath"/>
  if (struct_in<xsl:value-of select="$currentidxpath"/>.NE.ids_real_invalid) then
    struct_out<xsl:value-of select="$currentidxpath"/> = &amp;
    struct_in<xsl:value-of select="$currentidxpath"/>
  endif
    </xsl:when>

    <!-- 0D scalar, complex data -->
    <xsl:when test="@data_type='CPLX_0D'">
  ! Copy <xsl:value-of select="$currentidxpath"/>
  if (struct_in<xsl:value-of select="$currentidxpath"/>.NE.ids_complex_invalid) then
    struct_out<xsl:value-of select="$currentidxpath"/> = &amp;
    struct_in<xsl:value-of select="$currentidxpath"/>
  endif
    </xsl:when>

    <!-- 1D vector data -->
    <xsl:when test="@data_type='STR_0D' or @data_type='STR_1D' or 
                    @data_type='str_type' or @data_type='str_1d_type' or 
                    @data_type='FLT_1D' or @data_type='INT_1D' or @data_type='CPX_1D' or 
                    @data_type='flt_1d_type' or @data_type='int_1d_type' or @data_type='cpx_1d_type'">
  ! Copy <xsl:value-of select="$currentidxpath"/>
  if (associated(struct_in<xsl:value-of select="$currentidxpath"/>)) then
    allocate(struct_out<xsl:value-of select="$currentidxpath"/>&amp;
       (size(struct_in<xsl:value-of select="$currentidxpath"/>,1)))
    struct_out<xsl:value-of select="$currentidxpath"/> = &amp;
    struct_in<xsl:value-of select="$currentidxpath"/>
  endif
    </xsl:when>

    <!-- 2D vector data -->
    <xsl:when test="@data_type='int_2d_type' or @data_type='INT_2D' or 
                    @data_type='flt_2d_type' or @data_type='FLT_2D' or 
                    @data_type='cpx_2d_type' or @data_type='CPX_2D'">
  ! Copy <xsl:value-of select="$currentidxpath"/>
  if (associated(struct_in<xsl:value-of select="$currentidxpath"/>)) then
    allocate(struct_out<xsl:value-of select="$currentidxpath"/>&amp;
       (size(struct_in<xsl:value-of select="$currentidxpath"/>,1), &amp;
       size(struct_in<xsl:value-of select="$currentidxpath"/>,2)))
    struct_out<xsl:value-of select="$currentidxpath"/> = &amp;
    struct_in<xsl:value-of select="$currentidxpath"/>
  endif
    </xsl:when>

    <!-- 3D vector data -->
    <xsl:when test="@data_type='INT_3D' or @data_type='FLT_3D' or @data_type='CPX_3D'">
  ! Copy <xsl:value-of select="$currentidxpath"/>
  if (associated(struct_in<xsl:value-of select="$currentidxpath"/>)) then
    allocate(struct_out<xsl:value-of select="$currentidxpath"/>&amp;
       (size(struct_in<xsl:value-of select="$currentidxpath"/>,1), &amp;
       size(struct_in<xsl:value-of select="$currentidxpath"/>,2), &amp;
       size(struct_in<xsl:value-of select="$currentidxpath"/>,3)))
    struct_out<xsl:value-of select="$currentidxpath"/> = &amp;
    struct_in<xsl:value-of select="$currentidxpath"/>
  endif
    </xsl:when>

    <!-- 4D vector data -->
    <xsl:when test="@data_type='INT_4D' or @data_type='FLT_4D' or @data_type='CPX_4D'">
  ! Copy <xsl:value-of select="$currentidxpath"/>
  if (associated(struct_in<xsl:value-of select="$currentidxpath"/>)) then
    allocate(struct_out<xsl:value-of select="$currentidxpath"/>&amp;
       (size(struct_in<xsl:value-of select="$currentidxpath"/>,1), &amp;
       size(struct_in<xsl:value-of select="$currentidxpath"/>,2), &amp;
       size(struct_in<xsl:value-of select="$currentidxpath"/>,3), &amp;
       size(struct_in<xsl:value-of select="$currentidxpath"/>,4)))
    struct_out<xsl:value-of select="$currentidxpath"/> = &amp;
    struct_in<xsl:value-of select="$currentidxpath"/>
  endif
    </xsl:when>

    <!-- 5D vector data -->
    <xsl:when test="@data_type='INT_5D' or @data_type='FLT_5D' or @data_type='CPX_5D'">
  ! Copy <xsl:value-of select="$currentidxpath"/>
  if (associated(struct_in<xsl:value-of select="$currentidxpath"/>)) then
    allocate(struct_out<xsl:value-of select="$currentidxpath"/>&amp;
       (size(struct_in<xsl:value-of select="$currentidxpath"/>,1), &amp;
       size(struct_in<xsl:value-of select="$currentidxpath"/>,2), &amp;
       size(struct_in<xsl:value-of select="$currentidxpath"/>,3), &amp;
       size(struct_in<xsl:value-of select="$currentidxpath"/>,4), &amp;
       size(struct_in<xsl:value-of select="$currentidxpath"/>,5)))
    struct_out<xsl:value-of select="$currentidxpath"/> = &amp;
    struct_in<xsl:value-of select="$currentidxpath"/>
  endif
    </xsl:when>

    <!-- 6D vector data -->
    <xsl:when test="@data_type='INT_6D' or @data_type='FLT_6D' or @data_type='CPX_6D'">
  ! Copy <xsl:value-of select="$currentidxpath"/>
  if (associated(struct_in<xsl:value-of select="$currentidxpath"/>)) then
    allocate(struct_out<xsl:value-of select="$currentidxpath"/>&amp;
       (size(struct_in<xsl:value-of select="$currentidxpath"/>,1), &amp;
       size(struct_in<xsl:value-of select="$currentidxpath"/>,2), &amp;
       size(struct_in<xsl:value-of select="$currentidxpath"/>,3), &amp;
       size(struct_in<xsl:value-of select="$currentidxpath"/>,4), &amp;
       size(struct_in<xsl:value-of select="$currentidxpath"/>,5), &amp;
       size(struct_in<xsl:value-of select="$currentidxpath"/>,6)))
    struct_out<xsl:value-of select="$currentidxpath"/> = &amp;
    struct_in<xsl:value-of select="$currentidxpath"/>
  endif
    </xsl:when>

    <!-- Error case: could throw error or generate code that can't compile? -->
    <xsl:otherwise>
      ! Copy <xsl:value-of select="$currentidxpath"/> : PROBLEM: UNIDENTIFIED TYPE !!!
      <xsl:message select="concat('PROBLEM : UNIDENTIFIED TYPE detected in COPY routine for ',@path)" terminate="yes"/>
    </xsl:otherwise>
  </xsl:choose>

</xsl:template>



<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<!-- IDS_PUT TEMPLATE                                                        -->
<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<xsl:template match="field" mode="PUT_FIELD">
  <xsl:param name="structvar"/>
  <xsl:param name="contextvar"/>
  <xsl:param name="timedparentexpr"/>
  <xsl:param name="slice"/>
  <xsl:param name="provenance"/>

  <xsl:if test="$slice !='yes' or @type ='dynamic' or @data_type='structure' or (@data_type='struct_array' and .//field[@type='dynamic'])"> <!-- This skips the routine for non-timed fields when using this template in PUT_SLICE mode -->

    <xsl:variable name="fieldvar"><xsl:value-of select="$structvar"/>%<xsl:value-of select="@name"/></xsl:variable>
    <xsl:variable name="timedexpr">
      <xsl:choose>
        <xsl:when test="@type='dynamic'"><xsl:value-of select="$timedparentexpr"/>.true.</xsl:when>
        <xsl:otherwise><xsl:value-of select="$timedparentexpr"/>.false.</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="fieldpath">path//"<xsl:value-of select="@name"/>"</xsl:variable>
    <!--<xsl:choose>
                 <xsl:when test="$contextvar='aosctx' or $contextvar='opctx'">"<xsl:value-of select="@name"/>"</xsl:when>
        <xsl:otherwise>path//"<xsl:value-of select="@name"/>"</xsl:otherwise>
        </xsl:choose>-->

<!-- Detect type of the field -->
<xsl:choose>

  <!-- Array of structure -->
  <xsl:when test="@data_type='struct_array' and $contextvar!='aosctx'">
    <xsl:variable name="this-type">
      <xsl:choose>
        <xsl:when test="@structure_reference='self'">
          <xsl:value-of select="local:unique_name(@name)"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="local:unique_name(@structure_reference)"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    ! Put <xsl:value-of select="@name"/>
    if (<xsl:if test="@type='dynamic'">(timemode.NE.IDS_TIME_MODE_INDEPENDENT) .AND. </xsl:if>associated(<xsl:value-of select="$fieldvar"/>)) then
       aoslen = size(<xsl:value-of select="$fieldvar"/>)
       <xsl:choose>
         <xsl:when test="@type='dynamic'">
       if (timemode.EQ.IDS_TIME_MODE_HOMOGENEOUS) then
          timepath = "/time"
       else
          timepath = <xsl:value-of select="$fieldpath"/>//"/time"
       endif
         </xsl:when>
         <xsl:otherwise>
       timepath = ""
         </xsl:otherwise>
       </xsl:choose>
       aos_hli_len = aoslen
       call al_begin_arraystruct_action(<xsl:value-of select="$contextvar"/>, <xsl:value-of select="$fieldpath"/>, timepath, aoslen, aosctx, status)
       if (status.eq.0) then
          if ( (aos_hli_len.eq.0) .and. (aoslen.gt.0) ) then
             allocate(<xsl:value-of select="$fieldvar"/>(aoslen))
          endif
          do i = 1,aoslen
          <xsl:apply-templates select="." mode="PUT_FIELD">
            <xsl:with-param name="structvar" select="$structvar"/>
            <xsl:with-param name="contextvar" select="'aosctx'"/>
            <xsl:with-param name="timedparentexpr" select="'timedparent.or.'"/>
            <xsl:with-param name="slice" select="$slice"/>
          </xsl:apply-templates> 
             call al_iterate_over_arraystruct(aosctx, 1, status)
          enddo
          call al_end_action(aosctx, status)
       else if (is_external_refusal(status)) then
          ! The layer has refused to open this array of structures for writing:
          ! the stored dictionary has no slot for the path at all. Nothing below
          ! it can be written, and no per-field attempt underneath it could have
          ! succeeded, so this is one refusal covering a subtree rather than a
          ! failure to recover from. Tolerated for the same reason as a leaf
          ! (see al_put_policy, which also explains why this site needed its own
          ! answer: it is a sizing call, not a lookup). Recorded distinctly,
          ! because "one value was dropped" and "this whole branch is absent"
          ! must not read alike.
          !
          ! A failed al_begin_arraystruct_action creates no context, so there is
          ! nothing to end here. aoslen is stale -- it is written back only on
          ! success -- and is read only inside the branch above, so this must
          ! stay a fall-through and must never re-enter it. Reset status so the
          ! invariant is local to this arm.
          call al_note_refused_write_subtree(<xsl:value-of select="$fieldpath"/>, status)
          status = 0
       else
          write(*,*) "ERROR! with field "//<xsl:value-of select="$fieldpath"/><xsl:text>&#xa;</xsl:text>
          <!-- Same contract as the GET arm: a failed al_begin_arraystruct_action
               creates no context, so end only one this routine owns. At IDS level
               ($structvar='IDS') that is the opctx opened by al_begin_global_action
               here; inside a put_struct_* routine the enclosing context is a dummy
               argument and its owner ends it, so ending it here closed it twice.
               retstatus was never assigned at all, and callers pass their own status
               variable as this argument, so a failed put returned whatever that held
               (normally 0) and the caller carried on as if the write had succeeded. -->
          <xsl:if test="$structvar='IDS'">if (present(retstatus)) </xsl:if>retstatus = status<xsl:if test="$structvar='IDS'"><xsl:text>&#xa;</xsl:text>          call al_end_action(<xsl:value-of select="$contextvar"/>, status)</xsl:if>
          return
       endif
    endif
  </xsl:when>

  <!-- Structure -->
  <xsl:when test="@data_type='structure' or (@data_type='struct_array' and $contextvar='aosctx')">
    <xsl:variable name="this-type">
      <xsl:choose>
        <xsl:when test="@structure_reference='self'">
          <xsl:value-of select="local:unique_name(@name)"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="local:unique_name(@structure_reference)"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    ! Put <xsl:value-of select="@name"/>
    call put_<xsl:if test="$slice='yes'">slice_</xsl:if>struct_ids_<xsl:value-of select="$this-type"/>(<xsl:value-of select="$contextvar"/>, name, &amp;
    <xsl:choose>
      <xsl:when test="$contextvar='aosctx'">'', </xsl:when>
      <xsl:otherwise><xsl:value-of select="concat(substring($fieldpath,1,string-length($fieldpath)-1),'/&quot;')"/>, </xsl:otherwise>
    </xsl:choose>
    <xsl:value-of select="$fieldvar"/><xsl:if test="@data_type='struct_array'">(i)</xsl:if>, timemode, <xsl:value-of select="$timedexpr"/>, status)
    <xsl:call-template name="checkErrorCtx">
      <xsl:with-param name="method" select="'put'"/>
      <xsl:with-param name="ctx" select="$contextvar"/>
      <xsl:with-param name="path" select="$fieldpath"/>
      <xsl:with-param name="closectx" select="'yes'"/>
      <xsl:with-param name="structvar" select="$structvar"/>
    </xsl:call-template>
  </xsl:when>

  <!-- String data -->
  <xsl:when test="@data_type='str_type' or @data_type='STR_0D'">
    ! Put <xsl:value-of select="@name"/>
    <xsl:choose>
      <xsl:when test="$provenance='DD/AL/LANG'">
        <xsl:choose>
          <xsl:when test="@name='data_dictionary'">
       longstring = "<xsl:value-of select="$DD_GIT_DESCRIBE"/>"
          </xsl:when>
          <xsl:when test="@name='access_layer'">
       longstring = "<xsl:value-of select="$AL_GIT_DESCRIBE"/>"
          </xsl:when>
          <xsl:when test="@name='access_layer_language'">
       longstring = "fortran"
          </xsl:when>
          <xsl:otherwise>
          </xsl:otherwise>
        </xsl:choose>
          call put_string(<xsl:value-of select="$contextvar"/>, name, <xsl:value-of select="$fieldpath"/>,&amp;
          '', TRIM(longstring), '<xsl:value-of select="@lifecycle_status"/>', status)
       <xsl:call-template name="checkErrorCtx">
         <xsl:with-param name="method" select="'put'"/>
         <xsl:with-param name="ctx" select="$contextvar"/>
         <xsl:with-param name="path" select="$fieldpath"/>
         <xsl:with-param name="structvar" select="$structvar"/>
       </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
    if (associated(<xsl:value-of select="$fieldvar"/>)) then
       call pack_string(<xsl:value-of select="$fieldvar"/>, longstring, lenstring)
       call put_string(<xsl:value-of select="$contextvar"/>, name, <xsl:value-of select="$fieldpath"/>,&amp;
         '', longstring(1:lenstring), '<xsl:value-of select="@lifecycle_status"/>',  status)
       <xsl:call-template name="checkErrorCtx">
         <xsl:with-param name="method" select="'put'"/>
         <xsl:with-param name="ctx" select="$contextvar"/>
         <xsl:with-param name="path" select="$fieldpath"/>
         <xsl:with-param name="structvar" select="$structvar"/>
       </xsl:call-template>
    else
       call put_empty_string(<xsl:value-of select="$contextvar"/>, name, <xsl:value-of select="$fieldpath"/>,&amp;
         '', '<xsl:value-of select="@lifecycle_status"/>',  status)
       <xsl:call-template name="checkErrorCtx">
         <xsl:with-param name="method" select="'put'"/>
         <xsl:with-param name="ctx" select="$contextvar"/>
         <xsl:with-param name="path" select="$fieldpath"/>
         <xsl:with-param name="structvar" select="$structvar"/>
       </xsl:call-template>
         
    endif
      </xsl:otherwise>
    </xsl:choose>
  </xsl:when>

  <!-- 1D array of string data -->
  <xsl:when test="@data_type='str_1d_type' or @data_type='STR_1D'">
    ! Put <xsl:value-of select="@name"/>
    <xsl:call-template name="set_timepath2">
      <xsl:with-param name="slice" select="$slice"/>
      <xsl:with-param name="fieldpath" select="$fieldpath"/>
      <xsl:with-param name="fieldvar" select="$fieldvar"/>
    </xsl:call-template>
         call put_vect1d_string(<xsl:value-of select="$contextvar"/>, name, <xsl:value-of select="$fieldpath"/>,&amp;
         trim(timepath), <xsl:value-of select="$fieldvar"/>, 1, '<xsl:value-of select="@lifecycle_status"/>', status)
       <xsl:call-template name="checkErrorCtx">
         <xsl:with-param name="method" select="'put'"/>
         <xsl:with-param name="ctx" select="$contextvar"/>
         <xsl:with-param name="path" select="$fieldpath"/>
         <xsl:with-param name="structvar" select="$structvar"/>
       </xsl:call-template>
  </xsl:when>

  <!-- integer scalar data -->
  <xsl:when test="@data_type='int_type' or @data_type='INT_0D'">
    ! Put <xsl:value-of select="@name"/>
        call put_int(<xsl:value-of select="$contextvar"/>, name, <xsl:value-of select="$fieldpath"/>,&amp;
        '', <xsl:value-of select="$fieldvar"/>, <xsl:value-of select="$fieldvar"/>.NE.ids_int_invalid,&amp;
         '<xsl:value-of select="@lifecycle_status"/>', status)
          <xsl:call-template name="checkErrorCtx">
            <xsl:with-param name="method" select="'put'"/>
            <xsl:with-param name="ctx" select="$contextvar"/>
            <xsl:with-param name="path" select="$fieldpath"/>
            <xsl:with-param name="structvar" select="$structvar"/>
          </xsl:call-template>
  </xsl:when>

  <!-- float scalar data -->
  <xsl:when test="@data_type='flt_type' or @data_type='FLT_0D'">
    ! Put <xsl:value-of select="@name"/>
       call put_double(<xsl:value-of select="$contextvar"/>, name, <xsl:value-of select="$fieldpath"/>,&amp;
       '', <xsl:value-of select="$fieldvar"/>, <xsl:value-of select="$fieldvar"/>.NE.ids_real_invalid,&amp;
       '<xsl:value-of select="@lifecycle_status"/>', status)
          <xsl:call-template name="checkErrorCtx">
            <xsl:with-param name="method" select="'put'"/>
            <xsl:with-param name="ctx" select="$contextvar"/>
            <xsl:with-param name="path" select="$fieldpath"/>
            <xsl:with-param name="structvar" select="$structvar"/>
          </xsl:call-template>
  </xsl:when>

  <!-- complex scalar data -->
  <xsl:when test="@data_type='cpx_type' or @data_type='CPX_0D'">
    ! Put <xsl:value-of select="@name"/>
        call put_complex(<xsl:value-of select="$contextvar"/>, name, <xsl:value-of select="$fieldpath"/>,&amp;
          '', <xsl:value-of select="$fieldvar"/>, <xsl:value-of select="$fieldvar"/>.NE.ids_complex_invalid,&amp;
          '<xsl:value-of select="@lifecycle_status"/>', status)
          <xsl:call-template name="checkErrorCtx">
            <xsl:with-param name="method" select="'put'"/>
            <xsl:with-param name="ctx" select="$contextvar"/>
            <xsl:with-param name="path" select="$fieldpath"/>
            <xsl:with-param name="structvar" select="$structvar"/>
          </xsl:call-template>
  </xsl:when>

  <!-- float 1D vector data -->
  <xsl:when test="@data_type='flt_1d_type' or @data_type='FLT_1D'">
    ! Put <xsl:value-of select="@name"/>
    if (<xsl:if test="@type='dynamic'">(timemode.NE.IDS_TIME_MODE_INDEPENDENT) .AND. </xsl:if> .true. ) then
    <xsl:call-template name="set_timepath2">
      <xsl:with-param name="slice" select="$slice"/>
      <xsl:with-param name="fieldpath" select="$fieldpath"/>
      <xsl:with-param name="fieldvar" select="$fieldvar"/>
    </xsl:call-template>
       call put_vect1d_double(<xsl:value-of select="$contextvar"/>, name, <xsl:value-of select="$fieldpath"/>,&amp;
       trim(timepath), <xsl:value-of select="$fieldvar"/>, 1, '<xsl:value-of select="@lifecycle_status"/>', status)
          <xsl:call-template name="checkErrorCtx">
            <xsl:with-param name="method" select="'put'"/>
            <xsl:with-param name="ctx" select="$contextvar"/>
            <xsl:with-param name="path" select="$fieldpath"/>
            <xsl:with-param name="structvar" select="$structvar"/>
          </xsl:call-template>
    endif
  </xsl:when>

  <!-- complex 1D vector data -->
  <xsl:when test="@data_type='cpx_1d_type' or @data_type='CPX_1D'">
    ! Put <xsl:value-of select="@name"/>
    if (<xsl:if test="@type='dynamic'">(timemode.NE.IDS_TIME_MODE_INDEPENDENT) .AND. </xsl:if> .true. ) then
    <xsl:call-template name="set_timepath2">
      <xsl:with-param name="slice" select="$slice"/>
      <xsl:with-param name="fieldpath" select="$fieldpath"/>
      <xsl:with-param name="fieldvar" select="$fieldvar"/>
    </xsl:call-template>
       call put_vect1d_complex(<xsl:value-of select="$contextvar"/>, name, <xsl:value-of select="$fieldpath"/>,&amp;
       trim(timepath), <xsl:value-of select="$fieldvar"/>, 1, '<xsl:value-of select="@lifecycle_status"/>', status)
          <xsl:call-template name="checkErrorCtx">
            <xsl:with-param name="method" select="'put'"/>
            <xsl:with-param name="ctx" select="$contextvar"/>
            <xsl:with-param name="path" select="$fieldpath"/>
            <xsl:with-param name="structvar" select="$structvar"/>
          </xsl:call-template>
    endif
  </xsl:when>

  <!-- integer 1D vector data -->
  <xsl:when test="@data_type='int_1d_type' or @data_type='INT_1D'">
    ! Put <xsl:value-of select="@name"/>
    if (<xsl:if test="@type='dynamic'">(timemode.NE.IDS_TIME_MODE_INDEPENDENT) .AND. </xsl:if> .true. ) then
    <xsl:call-template name="set_timepath2">
      <xsl:with-param name="slice" select="$slice"/>
      <xsl:with-param name="fieldpath" select="$fieldpath"/>
      <xsl:with-param name="fieldvar" select="$fieldvar"/>
    </xsl:call-template>
       call put_vect1d_int(<xsl:value-of select="$contextvar"/>, name, <xsl:value-of select="$fieldpath"/>,&amp;
       trim(timepath), <xsl:value-of select="$fieldvar"/>, 1, '<xsl:value-of select="@lifecycle_status"/>', status)
          <xsl:call-template name="checkErrorCtx">
            <xsl:with-param name="method" select="'put'"/>
            <xsl:with-param name="ctx" select="$contextvar"/>
            <xsl:with-param name="path" select="$fieldpath"/>
            <xsl:with-param name="structvar" select="$structvar"/>
          </xsl:call-template>
    endif
  </xsl:when>

  <!-- float 2D vector data -->
  <xsl:when test="@data_type='FLT_2D'">
    ! Put <xsl:value-of select="@name"/>
    if (<xsl:if test="@type='dynamic'">(timemode.NE.IDS_TIME_MODE_INDEPENDENT) .AND. </xsl:if> .true. ) then
    <xsl:call-template name="set_timepath2">
      <xsl:with-param name="slice" select="$slice"/>
      <xsl:with-param name="fieldpath" select="$fieldpath"/>
      <xsl:with-param name="fieldvar" select="$fieldvar"/>
    </xsl:call-template>
        call put_vect2d_double(<xsl:value-of select="$contextvar"/>, name, <xsl:value-of select="$fieldpath"/>,&amp;
        trim(timepath), <xsl:value-of select="$fieldvar"/>,&amp;
      2, '<xsl:value-of select="@lifecycle_status"/>', status)
          <xsl:call-template name="checkErrorCtx">
            <xsl:with-param name="method" select="'put'"/>
            <xsl:with-param name="ctx" select="$contextvar"/>
            <xsl:with-param name="path" select="$fieldpath"/>
            <xsl:with-param name="structvar" select="$structvar"/>
          </xsl:call-template>
    endif
  </xsl:when>

  <!-- complex 2D vector data -->
  <xsl:when test="@data_type='cpx_2d_type' or @data_type='CPX_2D'">
    ! Put <xsl:value-of select="@name"/>
    if (<xsl:if test="@type='dynamic'">(timemode.NE.IDS_TIME_MODE_INDEPENDENT) .AND. </xsl:if> .true. ) then
    <xsl:call-template name="set_timepath2">
      <xsl:with-param name="slice" select="$slice"/>
      <xsl:with-param name="fieldpath" select="$fieldpath"/>
      <xsl:with-param name="fieldvar" select="$fieldvar"/>
    </xsl:call-template>
        call put_vect2d_complex(<xsl:value-of select="$contextvar"/>, name, <xsl:value-of select="$fieldpath"/>,&amp;
        trim(timepath), <xsl:value-of select="$fieldvar"/>,&amp;
      2, '<xsl:value-of select="@lifecycle_status"/>', status)
          <xsl:call-template name="checkErrorCtx">
            <xsl:with-param name="method" select="'put'"/>
            <xsl:with-param name="ctx" select="$contextvar"/>
            <xsl:with-param name="path" select="$fieldpath"/>
            <xsl:with-param name="structvar" select="$structvar"/>
          </xsl:call-template>
    endif
  </xsl:when>

  <!-- integer 2D vector data -->
  <xsl:when test="@data_type='INT_2D'">
    ! Put <xsl:value-of select="@name"/>
    if (<xsl:if test="@type='dynamic'">(timemode.NE.IDS_TIME_MODE_INDEPENDENT) .AND. </xsl:if> .true. ) then
    <xsl:call-template name="set_timepath2">
      <xsl:with-param name="slice" select="$slice"/>
      <xsl:with-param name="fieldpath" select="$fieldpath"/>
      <xsl:with-param name="fieldvar" select="$fieldvar"/>
    </xsl:call-template>
          call put_vect2d_int(<xsl:value-of select="$contextvar"/>, name, <xsl:value-of select="$fieldpath"/>,&amp;
          trim(timepath), <xsl:value-of select="$fieldvar"/>,&amp;
          2, '<xsl:value-of select="@lifecycle_status"/>', status)
          <xsl:call-template name="checkErrorCtx">
            <xsl:with-param name="method" select="'put'"/>
            <xsl:with-param name="ctx" select="$contextvar"/>
            <xsl:with-param name="path" select="$fieldpath"/>
            <xsl:with-param name="structvar" select="$structvar"/>
          </xsl:call-template>
    endif
  </xsl:when>

  <!-- float 3D vector data -->
  <xsl:when test="@data_type='FLT_3D'">
    ! Put <xsl:value-of select="@name"/>
    if (<xsl:if test="@type='dynamic'">(timemode.NE.IDS_TIME_MODE_INDEPENDENT) .AND. </xsl:if> .true. ) then
    <xsl:call-template name="set_timepath2">
      <xsl:with-param name="slice" select="$slice"/>
      <xsl:with-param name="fieldpath" select="$fieldpath"/>
      <xsl:with-param name="fieldvar" select="$fieldvar"/>
    </xsl:call-template>
          call put_vect3d_double(<xsl:value-of select="$contextvar"/>, name, <xsl:value-of select="$fieldpath"/>,&amp;
          trim(timepath), <xsl:value-of select="$fieldvar"/>, &amp;
          3, '<xsl:value-of select="@lifecycle_status"/>', status)
          <xsl:call-template name="checkErrorCtx">
            <xsl:with-param name="method" select="'put'"/>
            <xsl:with-param name="ctx" select="$contextvar"/>
            <xsl:with-param name="path" select="$fieldpath"/>
            <xsl:with-param name="structvar" select="$structvar"/>
          </xsl:call-template>
    endif
  </xsl:when>

  <!-- complex 3D vector data -->
  <xsl:when test="@data_type='CPX_3D'">
    ! Put <xsl:value-of select="@name"/>
    if (<xsl:if test="@type='dynamic'">(timemode.NE.IDS_TIME_MODE_INDEPENDENT) .AND. </xsl:if> .true. ) then
    <xsl:call-template name="set_timepath2">
      <xsl:with-param name="slice" select="$slice"/>
      <xsl:with-param name="fieldpath" select="$fieldpath"/>
      <xsl:with-param name="fieldvar" select="$fieldvar"/>
    </xsl:call-template>
          call put_vect3d_complex(<xsl:value-of select="$contextvar"/>, name, <xsl:value-of select="$fieldpath"/>,&amp;
          trim(timepath), <xsl:value-of select="$fieldvar"/>, &amp;
          3, '<xsl:value-of select="@lifecycle_status"/>', status)
          <xsl:call-template name="checkErrorCtx">
            <xsl:with-param name="method" select="'put'"/>
            <xsl:with-param name="ctx" select="$contextvar"/>
            <xsl:with-param name="path" select="$fieldpath"/>
            <xsl:with-param name="structvar" select="$structvar"/>
          </xsl:call-template>
    endif
  </xsl:when>

  <!-- integer 3D vector data -->
  <xsl:when test="@data_type='INT_3D'">
    ! Put <xsl:value-of select="@path"/>
    if (<xsl:if test="@type='dynamic'">(timemode.NE.IDS_TIME_MODE_INDEPENDENT) .AND. </xsl:if> .true. ) then
    <xsl:call-template name="set_timepath2">
      <xsl:with-param name="slice" select="$slice"/>
      <xsl:with-param name="fieldpath" select="$fieldpath"/>
      <xsl:with-param name="fieldvar" select="$fieldvar"/>
    </xsl:call-template>
          call put_vect3d_int(<xsl:value-of select="$contextvar"/>, name, <xsl:value-of select="$fieldpath"/>,&amp;
          trim(timepath), <xsl:value-of select="$fieldvar"/>,&amp;
          3, '<xsl:value-of select="@lifecycle_status"/>', status)
          <xsl:call-template name="checkErrorCtx">
            <xsl:with-param name="method" select="'put'"/>
            <xsl:with-param name="ctx" select="$contextvar"/>
            <xsl:with-param name="path" select="$fieldpath"/>
            <xsl:with-param name="structvar" select="$structvar"/>
          </xsl:call-template>
    endif
  </xsl:when>

  <!-- float 4D vector data -->
  <xsl:when test="@data_type='FLT_4D'">
    ! Put <xsl:value-of select="@name"/>
    if (<xsl:if test="@type='dynamic'">(timemode.NE.IDS_TIME_MODE_INDEPENDENT) .AND. </xsl:if> .true. ) then
    <xsl:call-template name="set_timepath2">
      <xsl:with-param name="slice" select="$slice"/>
      <xsl:with-param name="fieldpath" select="$fieldpath"/>
      <xsl:with-param name="fieldvar" select="$fieldvar"/>
    </xsl:call-template>
          call put_vect4d_double(<xsl:value-of select="$contextvar"/>, name, <xsl:value-of select="$fieldpath"/>,&amp;
          trim(timepath), <xsl:value-of select="$fieldvar"/>,&amp;
          4, '<xsl:value-of select="@lifecycle_status"/>', status)
          <xsl:call-template name="checkErrorCtx">
            <xsl:with-param name="method" select="'put'"/>
            <xsl:with-param name="ctx" select="$contextvar"/>
            <xsl:with-param name="path" select="$fieldpath"/>
            <xsl:with-param name="structvar" select="$structvar"/>
          </xsl:call-template>
    endif
  </xsl:when>

  <!-- complex 4D vector data -->
  <xsl:when test="@data_type='CPX_4D'">
    ! Put <xsl:value-of select="@name"/>
    if (<xsl:if test="@type='dynamic'">(timemode.NE.IDS_TIME_MODE_INDEPENDENT) .AND. </xsl:if> .true. ) then
    <xsl:call-template name="set_timepath2">
      <xsl:with-param name="slice" select="$slice"/>
      <xsl:with-param name="fieldpath" select="$fieldpath"/>
      <xsl:with-param name="fieldvar" select="$fieldvar"/>
    </xsl:call-template>
          call put_vect4d_complex(<xsl:value-of select="$contextvar"/>, name, <xsl:value-of select="$fieldpath"/>,&amp;
          trim(timepath), <xsl:value-of select="$fieldvar"/>,&amp;
      4, '<xsl:value-of select="@lifecycle_status"/>', status)
          <xsl:call-template name="checkErrorCtx">
            <xsl:with-param name="method" select="'put'"/>
            <xsl:with-param name="ctx" select="$contextvar"/>
            <xsl:with-param name="path" select="$fieldpath"/>
            <xsl:with-param name="structvar" select="$structvar"/>
          </xsl:call-template>
    endif
  </xsl:when>

  <!-- float 5D vector data -->
  <xsl:when test="@data_type='FLT_5D'">
    ! Put <xsl:value-of select="@name"/>
    if (<xsl:if test="@type='dynamic'">(timemode.NE.IDS_TIME_MODE_INDEPENDENT) .AND. </xsl:if> .true. ) then
    <xsl:call-template name="set_timepath2">
      <xsl:with-param name="slice" select="$slice"/>
      <xsl:with-param name="fieldpath" select="$fieldpath"/>
      <xsl:with-param name="fieldvar" select="$fieldvar"/>
    </xsl:call-template>
        call put_vect5d_double(<xsl:value-of select="$contextvar"/>, name, <xsl:value-of select="$fieldpath"/>,&amp;
        trim(timepath), <xsl:value-of select="$fieldvar"/>,&amp;
      5, '<xsl:value-of select="@lifecycle_status"/>', status)
          <xsl:call-template name="checkErrorCtx">
            <xsl:with-param name="method" select="'put'"/>
            <xsl:with-param name="ctx" select="$contextvar"/>
            <xsl:with-param name="path" select="$fieldpath"/>
            <xsl:with-param name="structvar" select="$structvar"/>
          </xsl:call-template>
    endif
  </xsl:when>

  <!-- complex 5D vector data -->
  <xsl:when test="@data_type='CPX_5D'">
    ! Put <xsl:value-of select="@name"/>
    if (<xsl:if test="@type='dynamic'">(timemode.NE.IDS_TIME_MODE_INDEPENDENT) .AND. </xsl:if> .true. ) then
    <xsl:call-template name="set_timepath2">
      <xsl:with-param name="slice" select="$slice"/>
      <xsl:with-param name="fieldpath" select="$fieldpath"/>
      <xsl:with-param name="fieldvar" select="$fieldvar"/>
    </xsl:call-template>
       call put_vect5d_complex(<xsl:value-of select="$contextvar"/>, name, <xsl:value-of select="$fieldpath"/>,&amp;
       trim(timepath), <xsl:value-of select="$fieldvar"/>,&amp;
      5, '<xsl:value-of select="@lifecycle_status"/>', status)
          <xsl:call-template name="checkErrorCtx">
            <xsl:with-param name="method" select="'put'"/>
            <xsl:with-param name="ctx" select="$contextvar"/>
            <xsl:with-param name="path" select="$fieldpath"/>
            <xsl:with-param name="structvar" select="$structvar"/>
          </xsl:call-template>
    endif
  </xsl:when>

  <!-- float 6D vector data -->
  <xsl:when test="@data_type='FLT_6D'">
    ! Put <xsl:value-of select="@name"/>
    if (<xsl:if test="@type='dynamic'">(timemode.NE.IDS_TIME_MODE_INDEPENDENT) .AND. </xsl:if> .true. ) then
    <xsl:call-template name="set_timepath2">
      <xsl:with-param name="slice" select="$slice"/>
      <xsl:with-param name="fieldpath" select="$fieldpath"/>
      <xsl:with-param name="fieldvar" select="$fieldvar"/>
    </xsl:call-template>
      call put_vect6d_double(<xsl:value-of select="$contextvar"/>, name, <xsl:value-of select="$fieldpath"/>,&amp;
      trim(timepath), <xsl:value-of select="$fieldvar"/>,&amp;
      6, '<xsl:value-of select="@lifecycle_status"/>', status)
          <xsl:call-template name="checkErrorCtx">
            <xsl:with-param name="method" select="'put'"/>
            <xsl:with-param name="ctx" select="$contextvar"/>
            <xsl:with-param name="path" select="$fieldpath"/>
            <xsl:with-param name="structvar" select="$structvar"/>
          </xsl:call-template>
    endif
  </xsl:when>

  <!-- complex 6D vector data -->
  <xsl:when test="@data_type='CPX_6D'">
    ! Put <xsl:value-of select="@name"/>
    if (<xsl:if test="@type='dynamic'">(timemode.NE.IDS_TIME_MODE_INDEPENDENT) .AND. </xsl:if> .true.) then
    <xsl:call-template name="set_timepath2">
      <xsl:with-param name="slice" select="$slice"/>
      <xsl:with-param name="fieldpath" select="$fieldpath"/>
      <xsl:with-param name="fieldvar" select="$fieldvar"/>
    </xsl:call-template>
      call put_vect6d_complex(<xsl:value-of select="$contextvar"/>, name, <xsl:value-of select="$fieldpath"/>,&amp;
      trim(timepath), <xsl:value-of select="$fieldvar"/>,&amp;
      6, '<xsl:value-of select="@lifecycle_status"/>', status)
          <xsl:call-template name="checkErrorCtx">
            <xsl:with-param name="method" select="'put'"/>
            <xsl:with-param name="ctx" select="$contextvar"/>
            <xsl:with-param name="path" select="$fieldpath"/>
            <xsl:with-param name="structvar" select="$structvar"/>
          </xsl:call-template>
    endif
  </xsl:when>
  <xsl:otherwise>
    ! Put <xsl:value-of select="@name"/> : PROBLEM : UNIDENTIFIED TYPE !!! 
    <xsl:message select="concat('PROBLEM : UNIDENTIFIED TYPE detected in PUT routine for ',@path)" terminate="yes"/>
  </xsl:otherwise>
</xsl:choose>
</xsl:if>

</xsl:template>



<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<!-- IDS_GET TEMPLATE                                                        -->
<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<xsl:template match="field" mode="GET_FIELD">
  <xsl:param name="structvar"/>
  <xsl:param name="contextvar"/>
  <xsl:param name="timedparentexpr"/>
  <xsl:param name="slice"/>
  <xsl:param name="root"/>

  <xsl:if test="$slice !='yes' or @type ='dynamic' or @data_type='structure' or (@data_type='struct_array' and .//field[@type='dynamic'])"> <!-- This skips the routine for non-timed fields when using this template in GET_SLICE mode -->

    <xsl:variable name="fieldvar"><xsl:value-of select="$structvar"/>%<xsl:value-of select="@name"/></xsl:variable>
    <xsl:variable name="timedexpr">
      <xsl:choose>
        <xsl:when test="@type='dynamic'"><xsl:value-of select="$timedparentexpr"/>.true.</xsl:when>
        <xsl:otherwise><xsl:value-of select="$timedparentexpr"/>.false.</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="fieldpath">path//"<xsl:value-of select="@name"/>"</xsl:variable>
    <!--<xsl:choose>
                 <xsl:when test="$contextvar='aosctx' or $contextvar='opctx'">"<xsl:value-of select="@name"/>"</xsl:when>
        <xsl:otherwise>path//"<xsl:value-of select="@name"/>"</xsl:otherwise>
        </xsl:choose>-->

<!-- Detect type of the field -->
<xsl:choose>

  <!-- Array of structure -->
  <xsl:when test="@data_type='struct_array' and $contextvar!='aosctx'">
    <xsl:variable name="this-type">
      <xsl:choose>
        <xsl:when test="@structure_reference='self'">
          <xsl:value-of select="local:unique_name(@name)"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="local:unique_name(@structure_reference)"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    ! Get <xsl:value-of select="@name"/>
    <xsl:choose>
      <xsl:when test="@type='dynamic'">
       if (<xsl:choose><xsl:when test="$root='yes'">IDS%ids_properties%homogeneous_time.NE.IDS_TIME_MODE_INDEPENDENT</xsl:when><xsl:otherwise>timemode.NE.IDS_TIME_MODE_INDEPENDENT</xsl:otherwise></xsl:choose>) then
          if (<xsl:choose><xsl:when test="$root='yes'">IDS%ids_properties%homogeneous_time.EQ.IDS_TIME_MODE_HOMOGENEOUS</xsl:when><xsl:otherwise>timemode.EQ.IDS_TIME_MODE_HOMOGENEOUS</xsl:otherwise></xsl:choose>) then
             timepath = "/time"
          else
             timepath = <xsl:value-of select="$fieldpath"/>//"/time"
          endif
      </xsl:when>
      <xsl:otherwise>
          timepath = ""
      </xsl:otherwise>
    </xsl:choose>
          call al_begin_arraystruct_action(<xsl:value-of select="$contextvar"/>, <xsl:value-of select="$fieldpath"/>, timepath, aoslen, aosctx, status)
          if (status.eq.0) then
             if (aoslen.gt.0) allocate(<xsl:value-of select="$fieldvar"/>(aoslen))
             do i = 1,aoslen
       <xsl:apply-templates select="." mode="GET_FIELD">
         <xsl:with-param name="structvar" select="$structvar"/>
         <xsl:with-param name="contextvar" select="'aosctx'"/>
         <xsl:with-param name="timedparentexpr" select="'timedparent.or.'"/>
         <xsl:with-param name="root" select="$root"/>
       </xsl:apply-templates> 
                call al_iterate_over_arraystruct(aosctx, 1, status)
                <!-- Advancing the cursor is not node-local: it is the loop itself,
                     not one field, so a failure here is fatal even under a
                     best-effort read. This used to be unreachable after a failed
                     element, because an element failure returned immediately. Now
                     that a refused element is tolerated, the loop really does run
                     to aoslen, and a wedged cursor would otherwise be iterated
                     against silently on every remaining turn. -->
                if (status.ne.0) then
                   write(*,*) "ERROR! cannot advance array of structures "//<xsl:value-of select="$fieldpath"/><xsl:text>&#xa;                   </xsl:text><xsl:if test="$structvar='IDS'">if (present(retstatus)) </xsl:if>retstatus = status
                   call al_end_action(aosctx, status)
                   return
                endif
             enddo
             call al_end_action(aosctx, status)
          else

             <!-- A failed al_begin_arraystruct_action creates no context: it leaves
                  aosctx unwritten, so returning it as a status returns garbage, and
                  there is no arraystruct context to end. Return the real status, and
                  end only a context this routine owns. At IDS level ($structvar='IDS')
                  that is the operation context opened by al_begin_global_action here,
                  so ending it is the correct cleanup before returning. Inside a
                  get_struct_* routine the enclosing context is a dummy argument owned
                  by the caller, which ends it itself on a non-zero status; ending it
                  here too closed the same context twice. -->
             if (isErrorCritical(status, <xsl:value-of select="$contextvar"/>, <xsl:value-of select="$fieldpath"/>)) then
                <xsl:if test="$structvar='IDS'">if (present(retstatus)) </xsl:if>retstatus = status<xsl:if test="$structvar='IDS'"><xsl:text>&#xa;</xsl:text>                call al_end_action(<xsl:value-of select="$contextvar"/>, status)</xsl:if>
                return
             endif
             <!-- Tolerated. The array is simply never allocated, which leaves it
                  disassociated: byte for byte what an absent array looks like.
                  aoslen is stale here (al_begin_arraystruct_action only writes it
                  back on success) but is read only inside the branch above, so
                  this must stay a fall-through and must never re-enter it.
                  Reset status so the invariant is local to this arm. -->
             status = 0
          endif
    <xsl:if test="@type='dynamic'">endif</xsl:if>
  </xsl:when>

  <!-- Structure -->
  <xsl:when test="@data_type='structure' or (@data_type='struct_array' and $contextvar='aosctx')">
    <xsl:variable name="this-type">
      <xsl:choose>
        <xsl:when test="@structure_reference='self'">
          <xsl:value-of select="local:unique_name(@name)"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="local:unique_name(@structure_reference)"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="closectx">
      <xsl:choose>
        <xsl:when test="@data_type='structure'">
          <xsl:value-of select="'no'"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="'yes'"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    ! Get <xsl:value-of select="@name"/>
    call get_struct_ids_<xsl:value-of select="$this-type"/>(<xsl:value-of select="$contextvar"/>, &amp;
    <xsl:choose>
      <xsl:when test="$contextvar='aosctx'">'', </xsl:when>
      <xsl:otherwise><xsl:value-of select="concat(substring($fieldpath,1,string-length($fieldpath)-1),'/&quot;')"/>, </xsl:otherwise>
    </xsl:choose>
    <xsl:value-of select="$fieldvar"/><xsl:if test="@data_type='struct_array'">(i)</xsl:if>, <xsl:choose><xsl:when test="$root='yes'">IDS%ids_properties%homogeneous_time</xsl:when><xsl:otherwise>timemode</xsl:otherwise></xsl:choose>, <xsl:value-of select="$timedexpr"/>, status)
    <xsl:call-template name="checkErrorCtx">
      <xsl:with-param name="method" select="'get'"/>
      <xsl:with-param name="ctx" select="$contextvar"/>
      <xsl:with-param name="path" select="$fieldpath"/>
      <xsl:with-param name="closectx" select="$closectx"/>
      <xsl:with-param name="structvar" select="$structvar"/>
    </xsl:call-template>
  </xsl:when>

  <!-- String data -->
  <xsl:when test="@data_type='str_type' or @data_type='STR_0D'">
    ! Get <xsl:value-of select="@name"/>
    longstring = ' '
    call get_string(<xsl:value-of select="$contextvar"/>, <xsl:value-of select="$fieldpath"/>,&amp;
          '', longstring, lenstring, status)
    if (status.EQ.0 .and. lenstring.gt.0) then
       call unpack_string(longstring, lenstring, <xsl:value-of select="$fieldvar"/>)
    else
      <xsl:call-template name="checkErrorCtx">
        <xsl:with-param name="method" select="'get'"/>
        <xsl:with-param name="ctx" select="$contextvar"/>
        <xsl:with-param name="path" select="$fieldpath"/>
        <xsl:with-param name="structvar" select="$structvar"/>
      </xsl:call-template>
    endif
  </xsl:when>

  <!-- 1D array of string data -->
  <xsl:when test="@data_type='str_1d_type' or @data_type='STR_1D'">
    ! Get <xsl:value-of select="@name"/>
    <xsl:call-template name="set_timepath">
      <xsl:with-param name="fieldpath" select="$fieldpath"/>
      <xsl:with-param name="fieldvar" select="$fieldvar"/>
      <xsl:with-param name="root" select="$root"/>
    </xsl:call-template>
    call get_vect1d_string(<xsl:value-of select="$contextvar"/>, <xsl:value-of select="$fieldpath"/>,&amp;
       trim(timepath), <xsl:value-of select="$fieldvar"/>, size1, status)
    <xsl:call-template name="checkErrorCtx">
      <xsl:with-param name="method" select="'get'"/>
      <xsl:with-param name="ctx" select="$contextvar"/>
      <xsl:with-param name="path" select="$fieldpath"/>
      <xsl:with-param name="structvar" select="$structvar"/>
      <xsl:with-param name="withtimepath" select="'yes'"/>
    </xsl:call-template>
  </xsl:when>

  <!-- integer scalar data -->
  <xsl:when test="@data_type='int_type' or @data_type='INT_0D'">
    ! Get <xsl:value-of select="@name"/>
    call get_int(<xsl:value-of select="$contextvar"/>, <xsl:value-of select="$fieldpath"/>,&amp;
          '', <xsl:value-of select="$fieldvar"/>, status)
    <xsl:call-template name="checkErrorCtx">
      <xsl:with-param name="method" select="'get'"/>
      <xsl:with-param name="ctx" select="$contextvar"/>
      <xsl:with-param name="path" select="$fieldpath"/>
      <xsl:with-param name="structvar" select="$structvar"/>
    </xsl:call-template>
  </xsl:when>

  <!-- float scalar data -->
  <xsl:when test="@data_type='flt_type' or @data_type='FLT_0D'">
    ! Get <xsl:value-of select="@name"/>
    call get_double(<xsl:value-of select="$contextvar"/>, <xsl:value-of select="$fieldpath"/>,&amp;
          '', <xsl:value-of select="$fieldvar"/>, status)
    <xsl:call-template name="checkErrorCtx">
      <xsl:with-param name="method" select="'get'"/>
      <xsl:with-param name="ctx" select="$contextvar"/>
      <xsl:with-param name="path" select="$fieldpath"/>
      <xsl:with-param name="structvar" select="$structvar"/>
    </xsl:call-template>
  </xsl:when>

  <!-- complex scalar data -->
  <xsl:when test="@data_type='cpx_type' or @data_type='CPX_0D'">
    ! Get <xsl:value-of select="@name"/>
    call get_complex(<xsl:value-of select="$contextvar"/>, <xsl:value-of select="$fieldpath"/>,&amp;
          '', <xsl:value-of select="$fieldvar"/>, status)
    <xsl:call-template name="checkErrorCtx">
      <xsl:with-param name="method" select="'get'"/>
      <xsl:with-param name="ctx" select="$contextvar"/>
      <xsl:with-param name="path" select="$fieldpath"/>
      <xsl:with-param name="structvar" select="$structvar"/>
    </xsl:call-template>
  </xsl:when>

  <!-- float 1D vector data -->
  <xsl:when test="@data_type='flt_1d_type' or @data_type='FLT_1D'">
    ! Get <xsl:value-of select="@name"/>
    <xsl:call-template name="set_timepath">
      <xsl:with-param name="fieldpath" select="$fieldpath"/>
      <xsl:with-param name="fieldvar" select="$fieldvar"/>
      <xsl:with-param name="root" select="$root"/>
    </xsl:call-template>
    call get_vect1d_double(<xsl:value-of select="$contextvar"/>, <xsl:value-of select="$fieldpath"/>,&amp;
          trim(timepath), <xsl:value-of select="$fieldvar"/>, size1, status)
    <xsl:call-template name="checkErrorCtx">
      <xsl:with-param name="method" select="'get'"/>
      <xsl:with-param name="ctx" select="$contextvar"/>
      <xsl:with-param name="path" select="$fieldpath"/>
      <xsl:with-param name="structvar" select="$structvar"/>
      <xsl:with-param name="withtimepath" select="'yes'"/>
    </xsl:call-template>
  </xsl:when>

  <!-- complex 1D vector data -->
  <xsl:when test="@data_type='cpx_1d_type' or @data_type='CPX_1D'">
    ! Get <xsl:value-of select="@name"/>
    <xsl:call-template name="set_timepath">
      <xsl:with-param name="fieldpath" select="$fieldpath"/>
      <xsl:with-param name="fieldvar" select="$fieldvar"/>
      <xsl:with-param name="root" select="$root"/>
    </xsl:call-template>
    call get_vect1d_complex(<xsl:value-of select="$contextvar"/>, <xsl:value-of select="$fieldpath"/>,&amp;
          trim(timepath), <xsl:value-of select="$fieldvar"/>, size1, status)
    <xsl:call-template name="checkErrorCtx">
      <xsl:with-param name="method" select="'get'"/>
      <xsl:with-param name="ctx" select="$contextvar"/>
      <xsl:with-param name="path" select="$fieldpath"/>
      <xsl:with-param name="structvar" select="$structvar"/>
      <xsl:with-param name="withtimepath" select="'yes'"/>
    </xsl:call-template>
  </xsl:when>

  <!-- integer 1D vector data -->
  <xsl:when test="@data_type='int_1d_type' or @data_type='INT_1D'">
    ! Get <xsl:value-of select="@name"/>
    <xsl:call-template name="set_timepath">
      <xsl:with-param name="fieldpath" select="$fieldpath"/>
      <xsl:with-param name="fieldvar" select="$fieldvar"/>
      <xsl:with-param name="root" select="$root"/>
    </xsl:call-template>
    call get_vect1d_int(<xsl:value-of select="$contextvar"/>, <xsl:value-of select="$fieldpath"/>,&amp;
          trim(timepath), <xsl:value-of select="$fieldvar"/>, size1, status)
    <xsl:call-template name="checkErrorCtx">
      <xsl:with-param name="method" select="'get'"/>
      <xsl:with-param name="ctx" select="$contextvar"/>
      <xsl:with-param name="path" select="$fieldpath"/>
      <xsl:with-param name="structvar" select="$structvar"/>
      <xsl:with-param name="withtimepath" select="'yes'"/>
    </xsl:call-template>
  </xsl:when>

  <!-- float 2D vector data -->
  <xsl:when test="@data_type='FLT_2D'">
    ! Get <xsl:value-of select="@name"/>
    <xsl:call-template name="set_timepath">
      <xsl:with-param name="fieldpath" select="$fieldpath"/>
      <xsl:with-param name="fieldvar" select="$fieldvar"/>
      <xsl:with-param name="root" select="$root"/>
    </xsl:call-template>
    call get_vect2d_double(<xsl:value-of select="$contextvar"/>, <xsl:value-of select="$fieldpath"/>,&amp;
          trim(timepath), <xsl:value-of select="$fieldvar"/>,&amp;
          size1, size2, status)
    <xsl:call-template name="checkErrorCtx">
      <xsl:with-param name="method" select="'get'"/>
      <xsl:with-param name="ctx" select="$contextvar"/>
      <xsl:with-param name="path" select="$fieldpath"/>
      <xsl:with-param name="structvar" select="$structvar"/>
      <xsl:with-param name="withtimepath" select="'yes'"/>
    </xsl:call-template>
  </xsl:when>

  <!-- complex 2D vector data -->
  <xsl:when test="@data_type='cpx_2d_type' or @data_type='CPX_2D'">
    ! Get <xsl:value-of select="@name"/>
    <xsl:call-template name="set_timepath">
      <xsl:with-param name="fieldpath" select="$fieldpath"/>
      <xsl:with-param name="fieldvar" select="$fieldvar"/>
      <xsl:with-param name="root" select="$root"/>
    </xsl:call-template>
    call get_vect2d_complex(<xsl:value-of select="$contextvar"/>, <xsl:value-of select="$fieldpath"/>,&amp;
          trim(timepath), <xsl:value-of select="$fieldvar"/>,&amp;
          size1, size2, status)
    <xsl:call-template name="checkErrorCtx">
      <xsl:with-param name="method" select="'get'"/>
      <xsl:with-param name="ctx" select="$contextvar"/>
      <xsl:with-param name="path" select="$fieldpath"/>
      <xsl:with-param name="structvar" select="$structvar"/>
      <xsl:with-param name="withtimepath" select="'yes'"/>
    </xsl:call-template>
  </xsl:when>

  <!-- integer 2D vector data -->
  <xsl:when test="@data_type='INT_2D'">
    ! Get <xsl:value-of select="@name"/>
    <xsl:call-template name="set_timepath">
      <xsl:with-param name="fieldpath" select="$fieldpath"/>
      <xsl:with-param name="fieldvar" select="$fieldvar"/>
      <xsl:with-param name="root" select="$root"/>
    </xsl:call-template>
    call get_vect2d_int(<xsl:value-of select="$contextvar"/>, <xsl:value-of select="$fieldpath"/>,&amp;
          trim(timepath), <xsl:value-of select="$fieldvar"/>,&amp;
          size1, size2, status)
    <xsl:call-template name="checkErrorCtx">
      <xsl:with-param name="method" select="'get'"/>
      <xsl:with-param name="ctx" select="$contextvar"/>
      <xsl:with-param name="path" select="$fieldpath"/>
      <xsl:with-param name="structvar" select="$structvar"/>
      <xsl:with-param name="withtimepath" select="'yes'"/>
    </xsl:call-template>
  </xsl:when>

  <!-- float 3D vector data -->
  <xsl:when test="@data_type='FLT_3D'">
    ! Get <xsl:value-of select="@name"/>
    <xsl:call-template name="set_timepath">
      <xsl:with-param name="fieldpath" select="$fieldpath"/>
      <xsl:with-param name="fieldvar" select="$fieldvar"/>
      <xsl:with-param name="root" select="$root"/>
    </xsl:call-template>
    call get_vect3d_double(<xsl:value-of select="$contextvar"/>, <xsl:value-of select="$fieldpath"/>,&amp;
          trim(timepath), <xsl:value-of select="$fieldvar"/>, &amp;
          size1, size2, size3, status)
    <xsl:call-template name="checkErrorCtx">
      <xsl:with-param name="method" select="'get'"/>
      <xsl:with-param name="ctx" select="$contextvar"/>
      <xsl:with-param name="path" select="$fieldpath"/>
      <xsl:with-param name="structvar" select="$structvar"/>
      <xsl:with-param name="withtimepath" select="'yes'"/>
    </xsl:call-template>
  </xsl:when>

  <!-- complex 3D vector data -->
  <xsl:when test="@data_type='CPX_3D'">
    ! Get <xsl:value-of select="@name"/>
    <xsl:call-template name="set_timepath">
      <xsl:with-param name="fieldpath" select="$fieldpath"/>
      <xsl:with-param name="fieldvar" select="$fieldvar"/>
      <xsl:with-param name="root" select="$root"/>
    </xsl:call-template>
    call get_vect3d_complex(<xsl:value-of select="$contextvar"/>, <xsl:value-of select="$fieldpath"/>,&amp;
          trim(timepath), <xsl:value-of select="$fieldvar"/>, &amp;
          size1, size2, size3, status)
    <xsl:call-template name="checkErrorCtx">
      <xsl:with-param name="method" select="'get'"/>
      <xsl:with-param name="ctx" select="$contextvar"/>
      <xsl:with-param name="path" select="$fieldpath"/>
      <xsl:with-param name="structvar" select="$structvar"/>
      <xsl:with-param name="withtimepath" select="'yes'"/>
    </xsl:call-template>
  </xsl:when>

  <!-- integer 3D vector data -->
  <xsl:when test="@data_type='INT_3D'">
    ! Get <xsl:value-of select="@path"/>
    <xsl:call-template name="set_timepath">
      <xsl:with-param name="fieldpath" select="$fieldpath"/>
      <xsl:with-param name="fieldvar" select="$fieldvar"/>
      <xsl:with-param name="root" select="$root"/>
    </xsl:call-template>
    call get_vect3d_int(<xsl:value-of select="$contextvar"/>, <xsl:value-of select="$fieldpath"/>,&amp;
          trim(timepath), <xsl:value-of select="$fieldvar"/>,&amp;
          size1, size2, size3, status)
    <xsl:call-template name="checkErrorCtx">
      <xsl:with-param name="method" select="'get'"/>
      <xsl:with-param name="ctx" select="$contextvar"/>
      <xsl:with-param name="path" select="$fieldpath"/>
      <xsl:with-param name="structvar" select="$structvar"/>
      <xsl:with-param name="withtimepath" select="'yes'"/>
    </xsl:call-template>
  </xsl:when>

  <!-- float 4D vector data -->
  <xsl:when test="@data_type='FLT_4D'">
    ! Get <xsl:value-of select="@name"/>
    <xsl:call-template name="set_timepath">
      <xsl:with-param name="fieldpath" select="$fieldpath"/>
      <xsl:with-param name="fieldvar" select="$fieldvar"/>
      <xsl:with-param name="root" select="$root"/>
    </xsl:call-template>
    call get_vect4d_double(<xsl:value-of select="$contextvar"/>, <xsl:value-of select="$fieldpath"/>,&amp;
          trim(timepath), <xsl:value-of select="$fieldvar"/>,&amp;
          size1, size2, size3, size4, status)
    <xsl:call-template name="checkErrorCtx">
      <xsl:with-param name="method" select="'get'"/>
      <xsl:with-param name="ctx" select="$contextvar"/>
      <xsl:with-param name="path" select="$fieldpath"/>
      <xsl:with-param name="structvar" select="$structvar"/>
      <xsl:with-param name="withtimepath" select="'yes'"/>
    </xsl:call-template>
  </xsl:when>

  <!-- complex 4D vector data -->
  <xsl:when test="@data_type='CPX_4D'">
    ! Get <xsl:value-of select="@name"/>
    <xsl:call-template name="set_timepath">
      <xsl:with-param name="fieldpath" select="$fieldpath"/>
      <xsl:with-param name="fieldvar" select="$fieldvar"/>
      <xsl:with-param name="root" select="$root"/>
    </xsl:call-template>
    call get_vect4d_complex(<xsl:value-of select="$contextvar"/>, <xsl:value-of select="$fieldpath"/>,&amp;
          trim(timepath), <xsl:value-of select="$fieldvar"/>,&amp;
          size1, size2, size3, size4, status)
    <xsl:call-template name="checkErrorCtx">
      <xsl:with-param name="method" select="'get'"/>
      <xsl:with-param name="ctx" select="$contextvar"/>
      <xsl:with-param name="path" select="$fieldpath"/>
      <xsl:with-param name="structvar" select="$structvar"/>
      <xsl:with-param name="withtimepath" select="'yes'"/>
    </xsl:call-template>
  </xsl:when>

  <!-- float 5D vector data -->
  <xsl:when test="@data_type='FLT_5D'">
    ! Get <xsl:value-of select="@name"/>
    <xsl:call-template name="set_timepath">
      <xsl:with-param name="fieldpath" select="$fieldpath"/>
      <xsl:with-param name="fieldvar" select="$fieldvar"/>
      <xsl:with-param name="root" select="$root"/>
    </xsl:call-template>
    call get_vect5d_double(<xsl:value-of select="$contextvar"/>, <xsl:value-of select="$fieldpath"/>,&amp;
          trim(timepath), <xsl:value-of select="$fieldvar"/>,&amp;
          size1, size2, size3, size4, size5, status)
    <xsl:call-template name="checkErrorCtx">
      <xsl:with-param name="method" select="'get'"/>
      <xsl:with-param name="ctx" select="$contextvar"/>
      <xsl:with-param name="path" select="$fieldpath"/>
      <xsl:with-param name="structvar" select="$structvar"/>
      <xsl:with-param name="withtimepath" select="'yes'"/>
    </xsl:call-template>
  </xsl:when>

  <!-- complex 5D vector data -->
  <xsl:when test="@data_type='CPX_5D'">
    ! Get <xsl:value-of select="@name"/>
    <xsl:call-template name="set_timepath">
      <xsl:with-param name="fieldpath" select="$fieldpath"/>
      <xsl:with-param name="fieldvar" select="$fieldvar"/>
      <xsl:with-param name="root" select="$root"/>
    </xsl:call-template>
    call get_vect5d_complex(<xsl:value-of select="$contextvar"/>, <xsl:value-of select="$fieldpath"/>,&amp;
          trim(timepath), <xsl:value-of select="$fieldvar"/>,&amp;
          size1, size2, size3, size4, size5, status)
    <xsl:call-template name="checkErrorCtx">
      <xsl:with-param name="method" select="'get'"/>
      <xsl:with-param name="ctx" select="$contextvar"/>
      <xsl:with-param name="path" select="$fieldpath"/>
      <xsl:with-param name="structvar" select="$structvar"/>
      <xsl:with-param name="withtimepath" select="'yes'"/>
    </xsl:call-template>
  </xsl:when>

  <!-- float 6D vector data -->
  <xsl:when test="@data_type='FLT_6D'">
    ! Get <xsl:value-of select="@name"/>
    <xsl:call-template name="set_timepath">
      <xsl:with-param name="fieldpath" select="$fieldpath"/>
      <xsl:with-param name="fieldvar" select="$fieldvar"/>
      <xsl:with-param name="root" select="$root"/>
    </xsl:call-template>
    call get_vect6d_double(<xsl:value-of select="$contextvar"/>, <xsl:value-of select="$fieldpath"/>,&amp;
          trim(timepath), <xsl:value-of select="$fieldvar"/>,&amp;
          size1, size2, size3, size4, size5, size6, status)
    <xsl:call-template name="checkErrorCtx">
      <xsl:with-param name="method" select="'get'"/>
      <xsl:with-param name="ctx" select="$contextvar"/>
      <xsl:with-param name="path" select="$fieldpath"/>
      <xsl:with-param name="structvar" select="$structvar"/>
      <xsl:with-param name="withtimepath" select="'yes'"/>
    </xsl:call-template>
  </xsl:when>

  <!-- complex 6D vector data -->
  <xsl:when test="@data_type='CPX_6D'">
    ! Get <xsl:value-of select="@name"/>
    <xsl:call-template name="set_timepath">
      <xsl:with-param name="fieldpath" select="$fieldpath"/>
      <xsl:with-param name="fieldvar" select="$fieldvar"/>
      <xsl:with-param name="root" select="$root"/>
    </xsl:call-template>
    call get_vect6d_complex(<xsl:value-of select="$contextvar"/>, <xsl:value-of select="$fieldpath"/>,&amp;
          trim(timepath), <xsl:value-of select="$fieldvar"/>,&amp;
          size1, size2, size3, size4, size5, size6, status)
    <xsl:call-template name="checkErrorCtx">
      <xsl:with-param name="method" select="'get'"/>
      <xsl:with-param name="ctx" select="$contextvar"/>
      <xsl:with-param name="path" select="$fieldpath"/>
      <xsl:with-param name="structvar" select="$structvar"/>
      <xsl:with-param name="withtimepath" select="'yes'"/>
    </xsl:call-template>
  </xsl:when>
  <xsl:otherwise>
    ! Get <xsl:value-of select="@name"/> : PROBLEM : UNIDENTIFIED TYPE !!! 
    <xsl:message select="concat('PROBLEM : UNIDENTIFIED TYPE detected in PUT routine for ',@path)" terminate="yes"/>
  </xsl:otherwise>
</xsl:choose>
</xsl:if>

</xsl:template>






<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->
<!-- MISC LOCAL TEMPLATES                                                    -->
<!--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-->

<xsl:template name ="checkErrorCtx">
  <xsl:param name="method"/> 
  <xsl:param name="ctx"/>
  <xsl:param name="path"/>
  <xsl:param name="closectx"/>
  <xsl:param name="structvar"/>
  <xsl:param name="withtimepath"/>
  <xsl:choose>
    <xsl:when test="$method='put'">
  if(isErrorCritical(status, <xsl:value-of select="$ctx"/>, <xsl:value-of select="$path"/>)) then
     <xsl:if test="$structvar='IDS'">if (present(retstatus)) </xsl:if>retstatus = status
     <xsl:if test="$closectx='yes'">call al_end_action(<xsl:value-of select="$ctx"/>, status)</xsl:if>
     return
  endif
    </xsl:when>
    <xsl:otherwise>
  if(isErrorCritical(status, <xsl:value-of select="$ctx"/>, <xsl:value-of select="$path"/>)) then
     <xsl:if test="$structvar='IDS'">if (present(retstatus)) </xsl:if>retstatus = status
     <xsl:if test="$closectx='yes'">call al_end_action(<xsl:value-of select="$ctx"/>, status)</xsl:if>
     return
  endif
  <xsl:if test="@type='dynamic'"><xsl:if test="$withtimepath='yes'">endif</xsl:if></xsl:if> <!-- closes the timemode.NE.IDS_TIME_MODE_INDEPENDENT test -->
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>



<xsl:template name="set_timepath">
  <xsl:param name="fieldpath"/>
  <xsl:param name="fieldvar"/>
  <xsl:param name="root"/>
  <xsl:choose>
    <xsl:when test="@type='dynamic'">
      if (<xsl:choose><xsl:when test="$root='yes'">IDS%ids_properties%homogeneous_time.NE.IDS_TIME_MODE_INDEPENDENT</xsl:when><xsl:otherwise>timemode.NE.IDS_TIME_MODE_INDEPENDENT</xsl:otherwise></xsl:choose>) then 
         if (timedparent) then
            timepath=""
         else
            if (<xsl:choose><xsl:when test="$root='yes'">IDS%ids_properties%homogeneous_time.EQ.IDS_TIME_MODE_HOMOGENEOUS</xsl:when><xsl:otherwise>timemode.EQ.IDS_TIME_MODE_HOMOGENEOUS</xsl:otherwise></xsl:choose>) then
               timepath="/time"
            else
               timepath=<xsl:if test="substring(@timebasepath,1,1)='\'">path//</xsl:if>"<xsl:value-of select="translate(@timebasepath,'\','')"/>"
            endif
         endif
    </xsl:when>
    <xsl:otherwise>
      timepath = ""
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>



<xsl:template name="set_timepath2">
  <xsl:param name="slice"/>
  <xsl:param name="fieldpath"/>
  <xsl:param name="fieldvar"/>
  <xsl:choose>
    <xsl:when test="@type='dynamic'">
      if (timedparent) then
         timepath=""
      else
         if (timemode.EQ.IDS_TIME_MODE_HOMOGENEOUS) then
            timepath="/time"
         else
            timepath=<xsl:if test="substring(@timebasepath,1,1)='\'">path//</xsl:if>"<xsl:value-of select="translate(@timebasepath,'\','')"/>"
         endif
      endif
    </xsl:when>
    <xsl:otherwise>
      timepath = ""
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>



<xsl:template name="isCriticalFuncCtx">
  <xsl:param name="method"/>
FUNCTION isErrorCritical(status, ctx, path) RESULT (exitRequest)
   use ids_types
   use al_low_level_wrap<xsl:if test="$method='get'">
   use al_get_policy</xsl:if>
   <!-- No `use al_put_policy` for method='put': the two modules this function is
        emitted into (utilities_put_struct, utilities_put_slice_struct) use it at
        module level, because the failure arm of al_begin_arraystruct_action needs
        it too and that code is not inside this function. -->
   implicit none

   integer(ids_int) :: status, ctx
   character*(*) :: path
   logical :: exitRequest

   exitRequest = .FALSE.

   if(status == 0) then
      exitRequest = .FALSE.
      return
<xsl:if test="$method='get'">   else if (is_external_refusal(status)) then
      ! An interposing DD-conversion layer has declared this path unservable in
      ! the dictionary this HLI speaks. That is not a failure of the read: the
      ! path does not exist here and no retry can produce it, so aborting would
      ! cost every remaining field for nothing. Leave the field unset, record
      ! what was skipped, and let the caller carry on.
      !
      ! Sound only because this is a per-field site. The same status is what the
      ! conversion layer returns from al_begin_global_action and the data-entry
      ! seams; tolerating it there would sail past an IDS that was never opened.
      call al_note_skipped_path(path, status)
      exitRequest = .FALSE.
      return
</xsl:if><xsl:if test="$method='put'">   else if (is_external_refusal(status)) then
      ! The interposing DD-conversion layer has declared this path unstorable in
      ! the dictionary the pulse is held under. Aborting would not undo the
      ! fields already written -- this traversal has no rollback -- so it buys a
      ! failure status at the cost of every field still to come. Tolerating
      ! instead leaves an occurrence complete except for paths the stored
      ! dictionary has no room for. See al_put_policy: this is a decision
      ! reasoned for writes, not the read-side policy reused.
      !
      ! Sound only because this is a per-field site. al_put_policy names the
      ! full site list, including the one put site that must NOT tolerate: the
      ! put_slice read of ids_properties/homogeneous_time, where a tolerated
      ! refusal would turn a slice write into a rewrite of the whole IDS.
      call al_note_refused_write(path, status)
      exitRequest = .FALSE.
      return
</xsl:if>   else
      exitRequest = .TRUE.
      write(*,*) "ERROR! with field '",path,"'"
      return
   endif
END FUNCTION isErrorCritical
</xsl:template>



</xsl:stylesheet>
