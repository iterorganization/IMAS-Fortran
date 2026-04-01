<?xml version="1.0" encoding="UTF-8"?>
<?modxslt-stylesheet type="text/xsl" media="fuffa, screen and $GET[stylesheet]" href="./%24GET%5Bstylesheet%5D" alternate="no" title="Translation using provided stylesheet" charset="ISO-8859-1" ?>
<?modxslt-stylesheet type="text/xsl" media="screen" alternate="no" title="Show raw source of the XML file" charset="ISO-8859-1" ?>

<xsl:stylesheet 
   xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="2.0"
   xmlns:xs="http://www.w3.org/2001/XMLSchema" 
   xmlns:my="http://localhost.localdomain/localns"
   exclude-result-prefixes="my xs">
   <xsl:import href="./identifiers.common.xsl"/>

<xsl:param name="name"/>
<xsl:param name="prefix"/>
   
<xsl:output method="text" version="1.0" encoding="UTF-8" indent="yes"/>


<!-- MAIN, FILE GENERATION -->
<xsl:template match="/constants">

  <!-- FORTRAN FILE -->
  <xsl:result-document href="{$prefix}{$name}.f90" method="text">
    <xsl:apply-templates select="header" mode="Fortran"/>

      <xsl:text>&#xA;&#xA;module al_</xsl:text><xsl:value-of select="$name"/><xsl:text>&#xA;</xsl:text>

    <xsl:apply-templates select="include[@name='Fortran']"/><xsl:text>&#xA;&#xA;</xsl:text>
    <xsl:text>  use ids_utilities&#xA;</xsl:text>
    <xsl:text>  implicit none&#xA;</xsl:text>
    <xsl:text>  private&#xA;</xsl:text>
    <xsl:text>  &#xA;</xsl:text>

    <xsl:if test="//constants[not(@create_mapping_function)]">
      <xsl:apply-templates select="*[name()!='header' and name()!='include' and @used_internally]" mode="Fortran"/>
    </xsl:if>

      <xsl:if test="//constants[@create_mapping_function]">
	<xsl:text>  public :: get_index&#xA;</xsl:text>
	<xsl:text>  public :: get_name&#xA;</xsl:text>
	<xsl:text>  public :: get_description&#xA;</xsl:text>
	<xsl:text>  private :: set_identifier_base&#xA;</xsl:text>
	<xsl:text>  private :: set_identifier_static&#xA;</xsl:text>
	<xsl:text>  private :: set_identifier_static_1d&#xA;</xsl:text>
	<xsl:text>  private :: set_identifier_dynamic_aos3&#xA;</xsl:text>
	<xsl:text>  private :: set_identifier_dynamic_aos3_1d&#xA;</xsl:text>
	<xsl:text>  &#xA;</xsl:text>
	<xsl:text>  ! Aliases for easier access&#xA;</xsl:text>
	<xsl:text>  public :: set_identifier&#xA;</xsl:text>
	<xsl:text>  &#xA;</xsl:text>
	<xsl:text>  ! Generic interface for set_identifier so it becomes overloaded &#xA;</xsl:text>
	<xsl:text>  interface set_identifier&#xA;</xsl:text>
	<xsl:text>    module procedure set_identifier_base&#xA;</xsl:text>
	<xsl:text>    module procedure set_identifier_static&#xA;</xsl:text>
	<xsl:text>    module procedure set_identifier_static_1d&#xA;</xsl:text>
	<xsl:text>    module procedure set_identifier_dynamic_aos3&#xA;</xsl:text>
	<xsl:text>    module procedure set_identifier_dynamic_aos3_1d&#xA;</xsl:text>
	<xsl:text>  end interface set_identifier&#xA;</xsl:text>
	<xsl:text>  &#xA;</xsl:text>

	<xsl:call-template name="translations_Fortran"/>
      </xsl:if>

	<xsl:text>&#xA;end module al_</xsl:text><xsl:value-of select="$name"/><xsl:text>&#xA;</xsl:text>

  </xsl:result-document>

  <!-- DOCBOOK FILE -->
  <xsl:result-document href="{$prefix}{$name}.xml" method="text">
    <xsl:call-template name="docbook"/>
  </xsl:result-document>

</xsl:template>

<!-- FORTRAN TEMPLATES -->

<xsl:template match="header" mode="Fortran">
  <xsl:call-template name="replace-string">
    <xsl:with-param name="text" select="concat('&#xA;',text())"/>
    <xsl:with-param name="replace" select="'&#xA;'"/>
    <xsl:with-param name="with" select="'&#xA;!> '"/>
  </xsl:call-template>
</xsl:template>
  
<xsl:template match="int" mode="Fortran">
  <xsl:text>  integer, parameter :: </xsl:text>
  <xsl:value-of select="@name"/>
  <xsl:text> = </xsl:text>
  <xsl:value-of select="."/>
  <xsl:text>&#xA;</xsl:text>
</xsl:template>

<xsl:template match="float" mode="Fortran">
  <xsl:text>  real(kind=c_double), parameter :: </xsl:text>
  <xsl:value-of select="@name"/>
  <xsl:text> = </xsl:text>
  <xsl:value-of select="."/>
  <xsl:text>_c_double&#xA;</xsl:text>
</xsl:template>

<xsl:template match="string" mode="Fortran">
  <xsl:text>  character(len=*), parameter :: </xsl:text>
  <xsl:value-of select="@name"/>
  <xsl:text> = "</xsl:text>
  <xsl:choose>
    <xsl:when test="string-length(.) > 132">
      <xsl:value-of select="substring(., 1, 132)"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="."/>
    </xsl:otherwise>
  </xsl:choose>
  <xsl:text>"&#xA;</xsl:text>
</xsl:template>

<xsl:template match="comment" mode="Fortran">
  <xsl:call-template name="replace-string">
    <xsl:with-param name="text" select="concat('&#xA;',text())"/>
    <xsl:with-param name="replace" select="'&#xA;'"/>
    <xsl:with-param name="with" select="'&#xA;  !> '"/>
  </xsl:call-template>
  <xsl:text>&#xA;</xsl:text>
</xsl:template>

<!-- Declare fortran variables -->
<xsl:template name="declareFortranIntegers">
    <xsl:for-each select="//constants/int[@name]">
            <xsl:text>    integer :: </xsl:text><xsl:value-of select="@name"/><xsl:text>&#xA;</xsl:text>
    </xsl:for-each>
    <xsl:for-each select="//constants/float[@name]">
      <xsl:text>    real(c_double) :: </xsl:text><xsl:value-of select="@name"/><xsl:text>&#xA;</xsl:text>
    </xsl:for-each>
    <xsl:text>    integer :: version&#xA;</xsl:text>
</xsl:template>

<!-- Assign Fortran variables -->
<xsl:template name="assignFortran">
  <xsl:for-each select="//constants/int[@name]">
    <xsl:text>    </xsl:text><xsl:value-of select="@name"/><xsl:text> = </xsl:text><xsl:value-of select="."/><xsl:text>, &amp;&#xA;</xsl:text>
  </xsl:for-each>
  <xsl:for-each select="//constants/float[@name]">
    <xsl:text>    </xsl:text><xsl:value-of select="@name"/><xsl:text> = </xsl:text><xsl:value-of select="."/><xsl:text>, &amp;&#xA;</xsl:text>
  </xsl:for-each>
  <xsl:text>    version=-999999999)&#xA;</xsl:text>
</xsl:template>

<!-- Translations between VALUE, NAME and DESCRIPTION -->
<xsl:template name="translations_Fortran">
    <xsl:text>&#xA;</xsl:text>

    <!-- Contains -->
    <xsl:text>contains&#xA;&#xA;</xsl:text>

    <!-- Private helper subroutine -->
    <xsl:text>  ! helper to get </xsl:text><xsl:value-of select="$name"/><xsl:text> data by name&#xA;</xsl:text>
    <xsl:text>  subroutine get_type_data_by_name(name, index_out, name_out, description_out)&#xA;</xsl:text>
    <xsl:text>    character(*), intent(in) :: name&#xA;</xsl:text>
    <xsl:text>    integer, intent(out) :: index_out&#xA;</xsl:text>
    <xsl:text>    character(len=ids_string_length), intent(out) :: name_out&#xA;</xsl:text>
    <xsl:text>    character(len=ids_string_length), intent(out) :: description_out&#xA;</xsl:text>
    <xsl:text>    character(len=:), allocatable :: key&#xA;</xsl:text>
    <xsl:text>    &#xA;</xsl:text>
    <xsl:text>    key = trim(name)&#xA;</xsl:text>
    <xsl:text>    index_out = ids_int_invalid&#xA;</xsl:text>
    <xsl:text>    name_out = ''&#xA;</xsl:text>
    <xsl:text>    description_out = ''&#xA;</xsl:text>
    <xsl:text>    &#xA;</xsl:text>
    <xsl:text>    select case (key)&#xA;</xsl:text>
    <xsl:for-each select="//constants/int[@name]">
      <xsl:text>      case ('</xsl:text><xsl:value-of select="@name"/><xsl:text>')&#xA;</xsl:text>
      <xsl:text>        index_out = </xsl:text><xsl:value-of select="."/><xsl:text>&#xA;</xsl:text>
      <xsl:text>        name_out = '</xsl:text><xsl:value-of select="@name"/><xsl:text>'&#xA;</xsl:text>
      <xsl:text>        description_out = '</xsl:text>
      <xsl:choose>
        <xsl:when test="string-length(@description) > 132">
          <xsl:value-of select="substring(@description, 1, 132)"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="@description"/>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:text>'&#xA;</xsl:text>
      <!-- If alias exists, add additional case for alias -->
      <xsl:if test="@alias">
        <xsl:text>      case ('</xsl:text><xsl:value-of select="@alias"/><xsl:text>')&#xA;</xsl:text>
        <xsl:text>        index_out = </xsl:text><xsl:value-of select="."/><xsl:text>&#xA;</xsl:text>
        <xsl:text>        name_out = '</xsl:text><xsl:value-of select="@name"/><xsl:text>'&#xA;</xsl:text>
        <xsl:text>        description_out = '</xsl:text>
        <xsl:choose>
          <xsl:when test="string-length(@description) > 132">
            <xsl:value-of select="substring(@description, 1, 132)"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="@description"/>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:text>'&#xA;</xsl:text>
      </xsl:if>
    </xsl:for-each>
    <xsl:text>      case default&#xA;</xsl:text>
    <xsl:text>        ! Raise error for unknown identifier&#xA;</xsl:text>
    <xsl:text>        ERROR STOP 'get_type_data_by_name: Unknown coordinate identifier: ' // trim(name)&#xA;</xsl:text>
    <xsl:text>    end select&#xA;</xsl:text>
    <xsl:text>  end subroutine get_type_data_by_name&#xA;&#xA;</xsl:text>

    <!-- Translation from NAME to VALUE -->
    <xsl:if test="int!='' and */@name!=''">
      <xsl:text>  ! Get </xsl:text><xsl:value-of select="$name"/><xsl:text> index from string name&#xA;</xsl:text>
      <xsl:text>  integer function get_index(NAME)&#xA;</xsl:text>
      <xsl:text>    character(*), intent(in) :: NAME&#xA;</xsl:text>
      <xsl:text>    character(len=:), allocatable :: key&#xA;</xsl:text>
      <xsl:text>    &#xA;</xsl:text>
      <xsl:text>    key = trim(NAME)&#xA;</xsl:text>
      <xsl:text>    &#xA;</xsl:text>
      <xsl:text>    select case (key)&#xA;</xsl:text>
      <xsl:for-each select="//constants/int[@name]">
        <xsl:text>      case ('</xsl:text><xsl:value-of select="@name"/><xsl:text>')&#xA;</xsl:text>
        <xsl:text>        get_index = </xsl:text><xsl:value-of select="."/><xsl:text>&#xA;</xsl:text>
        <!-- If alias exists, add additional case for alias -->
        <xsl:if test="@alias">
          <xsl:text>      case ('</xsl:text><xsl:value-of select="@alias"/><xsl:text>')&#xA;</xsl:text>
          <xsl:text>        get_index = </xsl:text><xsl:value-of select="."/><xsl:text>&#xA;</xsl:text>
        </xsl:if>
      </xsl:for-each>
      <xsl:text>      case default&#xA;</xsl:text>
            <xsl:text>        get_index = ids_int_invalid&#xA;</xsl:text>
      <xsl:text>    end select&#xA;</xsl:text>
      <xsl:text>  end function get_index&#xA;&#xA;</xsl:text>
    </xsl:if>

    <!-- Translation from VALUE to NAME -->
    <xsl:if test="int!=''">
      <xsl:text>  ! Get </xsl:text><xsl:value-of select="$name"/><xsl:text> name from index&#xA;</xsl:text>
      <xsl:text>  function get_name(IND) result(name)&#xA;</xsl:text>
      <xsl:text>    integer, intent(in) :: IND&#xA;</xsl:text>
      <xsl:text>    character(len=:), allocatable :: name&#xA;</xsl:text>
      <xsl:text>    &#xA;</xsl:text>
      <xsl:text>    name = ''&#xA;</xsl:text>
      <xsl:text>    select case (IND)&#xA;</xsl:text>
      <xsl:for-each select="//constants/int[@name]">
        <xsl:text>      case (</xsl:text><xsl:value-of select="."/><xsl:text>)&#xA;</xsl:text>
        <xsl:text>        name = '</xsl:text><xsl:value-of select="@name"/><xsl:text>'&#xA;</xsl:text>
      </xsl:for-each>
      <xsl:text>      case default&#xA;</xsl:text>
      <xsl:text>        name = ''&#xA;</xsl:text>
      <xsl:text>    end select&#xA;</xsl:text>
      <xsl:text>  end function get_name&#xA;&#xA;</xsl:text>
    </xsl:if>

    <!-- Translation from VALUE to DESCRIPTION -->
    <xsl:if test="int!=''">
      <xsl:text>  ! Get </xsl:text><xsl:value-of select="$name"/><xsl:text> description from index&#xA;</xsl:text>
      <xsl:text>  function get_description(IND) result(description)&#xA;</xsl:text>
      <xsl:text>    integer, intent(in) :: IND&#xA;</xsl:text>
      <xsl:text>    character(len=:), allocatable :: description&#xA;</xsl:text>
      <xsl:text>    &#xA;</xsl:text>
      <xsl:text>    description = ''&#xA;</xsl:text>
      <xsl:text>    select case (IND)&#xA;</xsl:text>
      <xsl:for-each select="//constants/int[@name]">
        <xsl:text>      case (</xsl:text><xsl:value-of select="."/><xsl:text>)&#xA;</xsl:text>
        <xsl:text>        description = '</xsl:text>
        <xsl:choose>
          <xsl:when test="string-length(@description) > 132">
            <xsl:value-of select="substring(@description, 1, 132)"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="@description"/>
          </xsl:otherwise>
        </xsl:choose>
        <xsl:text>'&#xA;</xsl:text>
      </xsl:for-each>
      <xsl:text>      case default&#xA;</xsl:text>
      <xsl:text>        description = ''&#xA;</xsl:text>
      <xsl:text>    end select&#xA;</xsl:text>
      <xsl:text>  end function get_description&#xA;&#xA;</xsl:text>
    </xsl:if>

    <!-- Set complete ids_identifier structure -->
    <xsl:if test="int!=''">
      <xsl:text>  ! Set complete ids_identifier structure&#xA;</xsl:text>
      <xsl:text>  subroutine set_identifier_base(identifier, name)&#xA;</xsl:text>
      <xsl:text>    type(ids_identifier), intent(out) :: identifier&#xA;</xsl:text>
      <xsl:text>    character(*), intent(in) :: name&#xA;</xsl:text>
      <xsl:text>    integer :: index_out&#xA;</xsl:text>
      <xsl:text>    character(len=ids_string_length) :: name_out, description_out&#xA;</xsl:text>
      <xsl:text>    &#xA;</xsl:text>
      <xsl:text>    call get_type_data_by_name(name, index_out, name_out, description_out)&#xA;</xsl:text>
      <xsl:text>    &#xA;</xsl:text>
      <xsl:text>    identifier%index = index_out&#xA;</xsl:text>
      <xsl:text>    allocate(identifier%name(1))&#xA;</xsl:text>
      <xsl:text>    allocate(identifier%description(1))&#xA;</xsl:text>
      <xsl:text>    identifier%name(1) = name_out&#xA;</xsl:text>
      <xsl:text>    identifier%description(1) = description_out&#xA;</xsl:text>
      <xsl:text>  end subroutine set_identifier_base&#xA;&#xA;</xsl:text>
    </xsl:if>

    <!-- Set complete ids_identifier_static structure -->
    <xsl:if test="int!=''">
      <xsl:text>  ! Set complete ids_identifier_static structure&#xA;</xsl:text>
      <xsl:text>  subroutine set_identifier_static(identifier, name)&#xA;</xsl:text>
      <xsl:text>    type(ids_identifier_static), intent(out) :: identifier&#xA;</xsl:text>
      <xsl:text>    character(*), intent(in) :: name&#xA;</xsl:text>
      <xsl:text>    integer :: index_out&#xA;</xsl:text>
      <xsl:text>    character(len=ids_string_length) :: name_out, description_out&#xA;</xsl:text>
      <xsl:text>    &#xA;</xsl:text>
      <xsl:text>    call get_type_data_by_name(name, index_out, name_out, description_out)&#xA;</xsl:text>
      <xsl:text>    &#xA;</xsl:text>
      <xsl:text>    identifier%index = index_out&#xA;</xsl:text>
      <xsl:text>    allocate(identifier%name(1))&#xA;</xsl:text>
      <xsl:text>    allocate(identifier%description(1))&#xA;</xsl:text>
      <xsl:text>    identifier%name(1) = name_out&#xA;</xsl:text>
      <xsl:text>    identifier%description(1) = description_out&#xA;</xsl:text>
      <xsl:text>  end subroutine set_identifier_static&#xA;&#xA;</xsl:text>
    </xsl:if>

    <!-- Set complete ids_identifier_static_1d structure -->
    <xsl:if test="int!=''">
      <xsl:text>  ! Set complete ids_identifier_static_1d structure&#xA;</xsl:text>
      <xsl:text>  subroutine set_identifier_static_1d(identifier, names)&#xA;</xsl:text>
      <xsl:text>    type(ids_identifier_static_1d), intent(out) :: identifier&#xA;</xsl:text>
      <xsl:text>    character(*), dimension(:), intent(in) :: names&#xA;</xsl:text>
      <xsl:text>    integer :: i, n_identifiers&#xA;</xsl:text>
      <xsl:text>    integer :: index_out&#xA;</xsl:text>
      <xsl:text>    character(len=ids_string_length) :: name_out, description_out&#xA;</xsl:text>
      <xsl:text>&#xA;</xsl:text>
      <xsl:text>    n_identifiers = size(names)&#xA;</xsl:text>
      <xsl:text>    &#xA;</xsl:text>
      <xsl:text>    allocate(identifier%indices(n_identifiers))&#xA;</xsl:text>
      <xsl:text>    allocate(identifier%names(n_identifiers))&#xA;</xsl:text>
      <xsl:text>    allocate(identifier%descriptions(n_identifiers))&#xA;</xsl:text>
      <xsl:text>&#xA;</xsl:text>
      <xsl:text>    ! Process each name in the array&#xA;</xsl:text>
      <xsl:text>    do i = 1, n_identifiers&#xA;</xsl:text>
      <xsl:text>      call get_type_data_by_name(trim(names(i)), index_out, name_out, description_out)&#xA;</xsl:text>
      <xsl:text>      &#xA;</xsl:text>
      <xsl:text>      identifier%indices(i) = index_out&#xA;</xsl:text>
      <xsl:text>      identifier%names(i) = name_out&#xA;</xsl:text>
      <xsl:text>      identifier%descriptions(i) = description_out&#xA;</xsl:text>
      <xsl:text>    end do&#xA;</xsl:text>
      <xsl:text>  end subroutine set_identifier_static_1d&#xA;&#xA;</xsl:text>
    </xsl:if>

    <!-- Set complete ids_identifier_dynamic_aos3 structure -->
    <xsl:if test="int!=''">
      <xsl:text>  ! Set complete ids_identifier_dynamic_aos3 structure&#xA;</xsl:text>
      <xsl:text>  subroutine set_identifier_dynamic_aos3(identifier, name)&#xA;</xsl:text>
      <xsl:text>    type(ids_identifier_dynamic_aos3), intent(out) :: identifier&#xA;</xsl:text>
      <xsl:text>    character(*), intent(in) :: name&#xA;</xsl:text>
      <xsl:text>    integer :: index_out&#xA;</xsl:text>
      <xsl:text>    character(len=ids_string_length) :: name_out, description_out&#xA;</xsl:text>
      <xsl:text>    &#xA;</xsl:text>
      <xsl:text>    call get_type_data_by_name(name, index_out, name_out, description_out)&#xA;</xsl:text>
      <xsl:text>    &#xA;</xsl:text>
      <xsl:text>    identifier%index = index_out&#xA;</xsl:text>
      <xsl:text>    allocate(identifier%name(1))&#xA;</xsl:text>
      <xsl:text>    allocate(identifier%description(1))&#xA;</xsl:text>
      <xsl:text>    identifier%name(1) = name_out&#xA;</xsl:text>
      <xsl:text>    identifier%description(1) = description_out&#xA;</xsl:text>
      <xsl:text>  end subroutine set_identifier_dynamic_aos3&#xA;&#xA;</xsl:text>
    </xsl:if>

    <!-- Set complete ids_identifier_dynamic_aos3_1d structure -->
    <xsl:if test="int!=''">
      <xsl:text>  ! Set complete ids_identifier_dynamic_aos3_1d structure&#xA;</xsl:text>
      <xsl:text>  subroutine set_identifier_dynamic_aos3_1d(identifier, names)&#xA;</xsl:text>
      <xsl:text>    type(ids_identifier_dynamic_aos3_1d), intent(out) :: identifier&#xA;</xsl:text>
      <xsl:text>    character(*), dimension(:), intent(in) :: names&#xA;</xsl:text>
      <xsl:text>    integer :: i, n_identifiers&#xA;</xsl:text>
      <xsl:text>    integer :: index_out&#xA;</xsl:text>
      <xsl:text>    character(len=ids_string_length) :: name_out, description_out&#xA;</xsl:text>
      <xsl:text>&#xA;</xsl:text>
      <xsl:text>    n_identifiers = size(names)&#xA;</xsl:text>
      <xsl:text>    &#xA;</xsl:text>
      <xsl:text>    allocate(identifier%indices(n_identifiers))&#xA;</xsl:text>
      <xsl:text>    allocate(identifier%names(n_identifiers))&#xA;</xsl:text>
      <xsl:text>    allocate(identifier%descriptions(n_identifiers))&#xA;</xsl:text>
      <xsl:text>&#xA;</xsl:text>
      <xsl:text>    ! Process each name in the array&#xA;</xsl:text>
      <xsl:text>    do i = 1, n_identifiers&#xA;</xsl:text>
      <xsl:text>      call get_type_data_by_name(trim(names(i)), index_out, name_out, description_out)&#xA;</xsl:text>
      <xsl:text>      &#xA;</xsl:text>
      <xsl:text>      identifier%indices(i) = index_out&#xA;</xsl:text>
      <xsl:text>      identifier%names(i) = name_out&#xA;</xsl:text>
      <xsl:text>      identifier%descriptions(i) = description_out&#xA;</xsl:text>
      <xsl:text>    end do&#xA;</xsl:text>
      <xsl:text>  end subroutine set_identifier_dynamic_aos3_1d&#xA;</xsl:text>
    </xsl:if>

</xsl:template>

</xsl:stylesheet>
