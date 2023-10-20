<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
xmlns:aml="http://schemas.microsoft.com/aml/2001/core"
xmlns:w="http://schemas.microsoft.com/office/word/2003/wordml"
xmlns:v="urn:schemas-microsoft-com:vml"
xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
>
<xsl:output method="xml" indent="no"/>

<xsl:template match="/">
<xsl:processing-instruction name="mso-application">
   <xsl:text>progid="Word.Document"</xsl:text>
  </xsl:processing-instruction>
<w:wordDocument>
  <w:fonts>
    <w:defaultFonts w:ascii="Arial" w:fareast="Arial"
                    w:h-ansi="Arial" w:cs="Arial"/>
  </w:fonts>
<w:body>


<w:sect>
<w:p>
<w:pPr>
<w:pStyle w:val="FrontPage"/>
<w:jc w:val="left"/>
<w:spacing w:line="480"/>
<w:sectPr>
<w:pgSz w:h="16840" w:w="11900" w:orient="portrait"/>
 <w:titlePg/>
</w:sectPr>
</w:pPr>
<w:r>
<w:rPr>
<w:b/>
<w:sz w:val="30"/>
</w:rPr>
<w:t>Document Title:<xsl:value-of select="Report/study"/></w:t>
<w:br/><w:t>Author:<xsl:value-of select="Report/author"/></w:t>
<w:br/><w:t>Date:<xsl:value-of select="Report/datestamp"/></w:t>
</w:r>
</w:p>
</w:sect>


<xsl:apply-templates select="Report/."/>

    <xsl:call-template name="PageOrientation">
        <xsl:with-param name="orientation">
        <xsl:value-of select="Report/*[last()]/heading/orientation"/>
        </xsl:with-param>
    </xsl:call-template>

</w:body>


</w:wordDocument>
</xsl:template>


<xsl:template match="MetaText" name="MetaText">
    <xsl:apply-templates select="heading">
      <xsl:with-param name="titletype" select="'Table'"/>
    </xsl:apply-templates>
    <xsl:apply-templates select="Program"/>
    <w:p>
    <w:pPr>
          <w:jc w:val="center"/>
    </w:pPr>
        <xsl:apply-templates select="text"/>
    </w:p>
     <w:p>
          <w:pPr>
          <w:jc w:val="left"/>
          <xsl:if test="not(position() = last())">
          <xsl:call-template name="PageOrientation">
          <xsl:with-param name="orientation">
          <xsl:value-of select="heading/orientation"/>
          </xsl:with-param>
          </xsl:call-template>
        </xsl:if>
        </w:pPr>
        <xsl:apply-templates select="footnote"/>
     </w:p>

</xsl:template>

<xsl:template match="text" name="text">
<xsl:if test="position() &gt;1">
<w:br/>
</xsl:if>
<w:r>
<w:t><xsl:call-template  name="insertBreaks"/></w:t></w:r>
</xsl:template>


<xsl:template match="MetaTable" name="MetaTable">
<w:sect>
    <xsl:apply-templates select="heading">
      <xsl:with-param name="titletype" select="'Table'"/>
    </xsl:apply-templates>
    <xsl:apply-templates select="Program"/>
    <w:tbl>
        <w:tblPr>
        <w:jc w:val="center"/>
        <w:tblW w:type="auto" w:w="4500" type="pct"/>
        <w:tblCellMar>
            <w:right w:w="100" w:type="dxa"/>
        </w:tblCellMar>
        <w:tblBorders>
            <w:top w:val="single" w:sz="10" w:space="0" w:color="000000"/>
            <w:bottom w:val="single" w:sz="10" w:space="0" w:color="000000"/>
        </w:tblBorders>
        </w:tblPr>
        <xsl:apply-templates select="table/thead/tr">
          <xsl:with-param name="fontsize">
            <xsl:choose>
              <xsl:when test="heading/fontsize=''">20</xsl:when>
              <xsl:otherwise><xsl:value-of select="heading/fontsize"/></xsl:otherwise>
            </xsl:choose>
          </xsl:with-param>
        </xsl:apply-templates>
        <xsl:apply-templates select="table/tbody/tr">
          <xsl:with-param name="fontsize">
            <xsl:choose>
              <xsl:when test="heading/fontsize=''">20</xsl:when>
              <xsl:otherwise><xsl:value-of select="heading/fontsize"/></xsl:otherwise>
            </xsl:choose>
          </xsl:with-param>
        </xsl:apply-templates>
    </w:tbl>

    <w:p>
        <w:pPr>
            <w:jc w:val="left"/>
            <xsl:if test="not(position() = last())">
                <xsl:call-template name="PageOrientation">
                    <xsl:with-param name="orientation">
                        <xsl:value-of select="heading/orientation"/>
                    </xsl:with-param>
                </xsl:call-template>
            </xsl:if>
        </w:pPr>
        <xsl:apply-templates select="footnote"/>
    </w:p>
</w:sect>
</xsl:template>

<!--Heading-->
<xsl:template match="heading" name="heading">
<xsl:param name="titletype"/>
<xsl:variable name="previous_section" select="../preceding-sibling::*[1]/heading/section"/>
<xsl:variable name="current_section"  select="section"/>
<xsl:if test = "not( $previous_section = $current_section)">
	<w:hdr>
    <w:p>
        <w:pPr>
            <w:pStyle w:val="Header"/>
            <w:jc w:val="left"/>
        </w:pPr>
    <w:r>
    <w:t>Tables Listing and Figures for<xsl:value-of select="/Report/study"/>| Section: <xsl:value-of select="section"/></w:t>
    </w:r></w:p>
    </w:hdr>

	<w:p>
		<w:pPr>
			<w:jc w:val="center"/>
			<w:outlineLvl w:val="1" />
		</w:pPr>
		<w:r>
			<w:t>
				Section: <xsl:value-of select="section"/>
			</w:t>
		</w:r>
	</w:p>
</xsl:if>
<w:p>
    <w:pPr>
        <w:jc w:val="center"/>
        <w:outlineLvl w:val="2" />
    </w:pPr>
    <!--Start Bookmark -->
    <aml:annotation  w:type="Word.Bookmark.Start" />
    <w:r>
        <w:t>
          <xsl:value-of select="$titletype"/>
        </w:t>
        <w:t xml:space="preserve"> </w:t>
        <!--Start Numbering table with field -->
        <w:fldChar w:fldCharType="begin" />
          <xsl:if test="$titletype='Table'">
            <w:instrText> SEQ Table \* ARABIC</w:instrText>
          </xsl:if>
          <xsl:if test="$titletype='Figure'">
            <w:instrText> SEQ Figure \* ARABIC</w:instrText>
          </xsl:if>
          <w:fldChar w:fldCharType="separate" w:fldLock="true"/>
          <w:t><xsl:value-of select="number"/></w:t>
          <w:t xml:space="preserve"> </w:t>
        <w:fldChar w:fldCharType="end" />
        <!--Numbering table with field ends-->
    </w:r>
    <aml:annotation  w:type="Word.Bookmark.End" />
    <!--Bookmark ends-->
    <w:r>
        <w:t><xsl:value-of select="title"/></w:t>
    </w:r>
</w:p>
<w:p>
	<w:pPr><w:jc w:val="center"/></w:pPr>
			<xsl:apply-templates select="subtitle"/>
      <xsl:apply-templates select="population"/>
</w:p>
</xsl:template>

<!--Population-->
<xsl:template match="population" name="population">
<xsl:if test = ". != '' ">
	<w:r>
		<w:t>Population: <xsl:value-of select ="."/></w:t>
		<w:br/>
	</w:r>
</xsl:if>
</xsl:template>

<!--Subtitle-->
<xsl:template match="subtitle" name="subtitle">
<xsl:if test = ". != '' ">
	<w:r>
		<w:t><xsl:value-of select ="."/></w:t>
		<w:br/>
	</w:r>
</xsl:if>
</xsl:template>

<!--Table header rows-->
<xsl:template match="thead/tr">
<xsl:param name="fontsize" select="$fontsize"/>
<w:tr>
  <w:tblPrEx>
		<w:tblBorders>
		  <w:insideH w:val="single" w:sz="5" w:space="0" w:color="000000"/>
		  <w:trHeight w:val="300" w:hRule="exact"/>
		</w:tblBorders>
	</w:tblPrEx>
	<w:trPr>
		<w:tblHeader/>
	</w:trPr>
  <xsl:apply-templates select="th" >
    <xsl:with-param name="fontsize" select="$fontsize"/>
  </xsl:apply-templates>
</w:tr>
</xsl:template>

<!--Table header cells-->
<xsl:template match="th"  name="th">
  <xsl:param name="fontsize" select="$fontsize"/>
  <w:tc>
    <w:p>
      <w:pPr>
        <w:jc w:val="center"/>
        <w:b />
      </w:pPr>
      <w:r>
        <w:rPr>
         <w:b  w:val="on"/>
         <!--Table font size to 9-->
         <w:sz w:val="{$fontsize}"/>
        </w:rPr>
				<w:t><xsl:call-template name="insertBreaks"/></w:t>
      </w:r>
    </w:p>
  </w:tc>
</xsl:template>

<!--Table body rows-->
<xsl:template match="tbody/tr">
<xsl:param name="fontsize" select="$fontsize"/>
<xsl:variable name="span">
   <xsl:choose>
     <xsl:when test="td[1]/@style and contains(td[1]/@style, 'span')">
        <!--<xsl:value-of select="td[1]/@COLSPAN"/> -->
        <xsl:value-of select="count(td)"/>
     </xsl:when>
     <xsl:otherwise>
       <xsl:value-of select="0"/>
     </xsl:otherwise>
   </xsl:choose>
</xsl:variable>

<xsl:variable name="align">
   <xsl:choose>
     <xsl:when test="td[1]/@style and contains(td[1]/@style, 'firstleft')">
        <!--<xsl:value-of select="td[1]/@COLSPAN"/> -->
        <xsl:value-of select="1"/>
     </xsl:when>
     <xsl:otherwise>
       <xsl:value-of select="0"/>
     </xsl:otherwise>
   </xsl:choose>
</xsl:variable>

<xsl:variable name="tab1">
   <xsl:choose>
     <xsl:when test="td[1]/@style">
        <xsl:value-of select="1"/>
     </xsl:when>
     <xsl:otherwise>
       <xsl:value-of select="0"/>
     </xsl:otherwise>
   </xsl:choose>
</xsl:variable>

<w:tr>
  <xsl:apply-templates select="td">
    <xsl:with-param name="RowPosition" select="position()"/>
    <xsl:with-param name="span" select="$span"/>
    <xsl:with-param name="tab1" select="$tab1"/>
    <xsl:with-param name="align" select="$align"/>
    <xsl:with-param name="fontsize" select="$fontsize"/>
  </xsl:apply-templates>
</w:tr>
</xsl:template>

<!--Table body cells-->
<xsl:template match="td">
<xsl:param name="RowPosition"/>
<xsl:param name="span"/>
<xsl:param name="tab1"/>
<xsl:param name="align"/>
<xsl:param name="fontsize" select="$fontsize"/>

<w:tc>
  <w:tcPr>
  <!--Merge cells-->
  <xsl:if test="$span != 0 and position() &lt;= $span">
    <xsl:choose>
      <xsl:when test="position()=1">
        <w:hmerge w:val="restart"/>
      </xsl:when>
      <xsl:when test="position() &lt; $span">
        <w:hmerge w:val="continue"/>
      </xsl:when>
      <xsl:otherwise>
        <w:hmerge/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:if>
  <!--Cell Color-->
  <xsl:if test="@style and contains(@style, 'bgcol')">
    <w:shd w:fill="#d3d3d3"/>
  </xsl:if>
  </w:tcPr>

  <w:p>
    <w:pPr>
      <!--Left align if first column-->
      <xsl:choose>
				<xsl:when test="$align=1 and position()!=1">
					<w:jc w:val="center"/>
				</xsl:when>
				<xsl:otherwise>
					<w:jc w:val="left"/>
				</xsl:otherwise>
			</xsl:choose>
      <!--Indent-->
      <xsl:if test="@style and contains(@style, 'indent')">
        <w:ind w:left="200" />
      </xsl:if>
		</w:pPr>
    <w:r>
    <w:rPr>
      <!--Table font size to 9-->
      <w:sz w:val="{$fontsize}"/>
      <!--Bold-->
      <xsl:if test="@style and contains(@style, 'bold')">
        <w:b  w:val="on"/>
      </xsl:if>
    </w:rPr>
    <w:t><xsl:call-template name="insertBreaks"/></w:t>
    </w:r>
  </w:p>
</w:tc>
</xsl:template>

<!--Cell text-->
<xsl:template match="text()" name="insertBreaks">
   <xsl:param name="pText" select="."/>
   <xsl:choose>
     <xsl:when test="not(contains($pText, '&#xA;'))">
       <xsl:value-of select="$pText"/>
     </xsl:when>
     <xsl:otherwise>
       <xsl:value-of select="substring-before($pText, '&#xA;')"/>
       <w:br/>
       <xsl:call-template name="insertBreaks">
         <xsl:with-param name="pText" select=
           "substring-after($pText, '&#xA;')"/>
       </xsl:call-template>
     </xsl:otherwise>
   </xsl:choose>
</xsl:template>

<!--Footnote-->
<xsl:template match="footnote" name="footnote">
<w:br/>
<w:pPr>
  <w:jc w:val="left"/>
</w:pPr>
<w:r>
<w:t>
<xsl:call-template name="insertBreaks" /><!--Convert line break-->
</w:t></w:r>
</xsl:template>

<!--Program-->
  <xsl:template match="Program" name="Program">
  <w:ftr>
    <w:p>
      <w:pPr>
        <w:pStyle w:val="Footer"/>
      </w:pPr>
      <w:r>
        <w:t>
			Cambridge CTU,<xsl:value-of select="/Report/author"/>-<xsl:value-of select="/Report/datestamp"/>- Page&#160;</w:t>
    </w:r>
    <w:r>
    <w:fldChar w:fldCharType="begin"/>
    </w:r>
    <w:r>
    <w:instrText xml:space="preserve"> PAGE </w:instrText>
    </w:r>
    <w:r>
    <w:fldChar w:fldCharType="separate"/>
    </w:r>
    <w:r>
    <w:fldChar w:fldCharType="end"/>
    </w:r>
        <w:r>
        <w:t>
          <w:br/>Program:<xsl:value-of select="."/>
        </w:t>
      </w:r>
    </w:p>
  </w:ftr>
  </xsl:template>

<!--Image-->
<xsl:template match="MetaFigure" name="MetaFigure">
<xsl:apply-templates select="heading">
  <xsl:with-param name="titletype" select="'Figure'"/>
</xsl:apply-templates>
<xsl:apply-templates select="Program"/>
<w:p>
	<w:pPr>
		<w:jc w:val="center"/>
	</w:pPr>
	<w:r>
	<w:pict>
		<v:shape>
			<xsl:attribute name="style">
				<xsl:if test="heading/orientation='landscape'">
					position:static;width:505pt;height:357pt
				</xsl:if>
				<xsl:if test="heading/orientation='portrait'">
					position:static;width:357pt;height:505pt
				</xsl:if>
			</xsl:attribute>
			<v:imagedata>
				<xsl:attribute name="src">
				    <xsl:value-of select="src"/>
				</xsl:attribute>
			</v:imagedata>
		</v:shape>
	</w:pict>
    </w:r>
</w:p>
<w:p>
    <w:pPr>
    <w:jc w:val="center"/>
    <xsl:if test="not(position() = last())">
        <xsl:call-template name="PageOrientation">
            <xsl:with-param name="orientation">
                <xsl:value-of select="heading/orientation"/>
            </xsl:with-param>
        </xsl:call-template>
    </xsl:if>
    </w:pPr>
    <xsl:apply-templates select="footnote"/>
</w:p>
</xsl:template>

<xsl:template name="PageOrientation">
<xsl:param name="orientation"/>
<xsl:variable name="lower_case_orientation">
	<xsl:value-of select="translate($orientation,'ABCDEFGHIJKLYMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')" />
</xsl:variable>

<w:sectPr>
    <xsl:choose>
        <xsl:when test="$lower_case_orientation='portrait'">
            <w:pgSz w:h="16840" w:w="11900" w:orient="portrait"/>
        </xsl:when>
        <xsl:otherwise>
            <w:pgSz w:w="16840" w:h="11900" w:orient="landscape"/>
        </xsl:otherwise>
    </xsl:choose>
</w:sectPr>
</xsl:template>


</xsl:stylesheet>
