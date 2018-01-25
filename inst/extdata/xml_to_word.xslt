<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
xmlns:w="http://schemas.microsoft.com/office/word/2003/wordml"
xmlns:v="urn:schemas-microsoft-com:vml"				
xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
>
<xsl:output method="xml" indent="yes"/>



<xsl:template match="/">
<xsl:processing-instruction name="mso-application">
   <xsl:text>progid="Word.Document"</xsl:text>
  </xsl:processing-instruction>
<w:wordDocument>
<w:body>


<w:sect>
<w:p>
<w:pPr>
<w:pStyle w:val="FrontPage"/>
<w:jc w:val="left"/>
<w:spacing w:line="480"/>
<w:sectPr>
<w:pgSz w:h="16840" w:w="11900" w:orient="portrait"/>
 w:titlePg/>
</w:sectPr>
</w:pPr>
<w:r>
<w:rPr>
<w:rFonts w:ascii="Arial" w:hAnsi="Arial" w:cs="Arial"/>
<w:b/>
<w:sz w:val="30"/>
</w:rPr>
<w:t>Document Title: <xsl:value-of select="Report/study"/></w:t>
<w:br/><w:t>Author: <xsl:value-of select="Report/author"/></w:t>
<w:br/><w:t>Date: <xsl:value-of select="Report/datestamp"/></w:t>
</w:r>
</w:p>
</w:sect>

<w:hdr>
<w:p>
<w:pPr>
<w:pStyle w:val="Header"/>
<w:jc w:val="center"/>
</w:pPr>
<w:r>
<w:rPr><w:rFonts w:ascii="Arial" w:hAnsi="Arial" w:cs="Arial"/></w:rPr>
<w:t>Tables Listing and Graphs for <xsl:value-of select="Report/study"/></w:t>
</w:r>
</w:p>
</w:hdr>



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
    <xsl:apply-templates select="heading"/>
     <xsl:apply-templates select="Program"/>   
    <w:p>
    <w:pPr>
          <w:jc w:val="center"/>
    </w:pPr>
        <xsl:apply-templates select="theText"/>
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

<xsl:template match="theText" name="theText">
<xsl:if test="position() &gt;1">
<w:br/>
</xsl:if>
<w:r>
<w:rPr><w:rFonts w:ascii="Arial" w:hAnsi="Arial" w:cs="Arial"/></w:rPr>
<w:t><xsl:call-template  name="insertBreaks"/></w:t></w:r>
</xsl:template>


<xsl:template match="MetaTable" name="MetaTable">
<w:sect>
<xsl:if test="position() &gt;1">
</xsl:if>

	<xsl:apply-templates select="heading"/>
  <xsl:apply-templates select="Program"/>


<w:tbl>
<w:tblPr>
<w:jc w:val="center"/>
<w:tblW w:type="auto" w:w="4500" type="pct"/>
<w:tblCellMar>
 <w:right w:w="100" w:type="dxa"/>
</w:tblCellMar>
<w:tblBorders>
<w:top w:val="single" w:sz="12" w:space="0" w:color="000000"/>
<w:bottom w:val="single" w:sz="12" w:space="0" w:color="000000"/>
</w:tblBorders>
</w:tblPr>
<xsl:apply-templates select="table/tr"/>
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

<xsl:template match="heading" name="heading">
	<xsl:variable name="previous_section" select="../preceding-sibling::*[1]/heading/section"/> 
	<xsl:variable name="current_section"  select="section"/> 
	<xsl:if test = "not( $previous_section = $current_section)">	
	<w:p>
		<w:pPr>
			<w:jc w:val="center"/>
			<w:outlineLvl w:val="1" /> 
		</w:pPr>

		<w:r>
		<w:rPr><w:rFonts w:ascii="Arial" w:hAnsi="Arial" w:cs="Arial"/></w:rPr>
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

		<w:r>
		<w:rPr><w:rFonts w:ascii="Arial" w:hAnsi="Arial" w:cs="Arial"/></w:rPr>
			<w:t>
				Title: <xsl:value-of select="title"/><w:br/>
				Population: <xsl:value-of select="population"/>
				</w:t>
		</w:r>
	</w:p>
	<w:p>
		<w:pPr>
			<w:jc w:val="center"/>
		</w:pPr>

		<w:r>
		<w:rPr><w:rFonts w:ascii="Arial" w:hAnsi="Arial" w:cs="Arial"/></w:rPr>
			<w:t>
				<xsl:apply-templates select="subtitle"/>
				Number: <xsl:value-of select="number"/><w:br/>
				
				
			</w:t>
		</w:r>
	</w:p>
	</xsl:template>
	
	
	
	
	<xsl:template match="subtitle" name="subtitle">
	Subtitle: <xsl:value-of select ="."/><w:br/>
	</xsl:template>
	

<xsl:template match="table/tr" name="tr">
		<w:tr>
		
		
	
		<xsl:choose>
		<xsl:when test="position()=1">
			<w:tblPrEx>
			<w:tblBorders>
			<w:insideH w:val="single" w:sz="12" w:space="0" w:color="000000"/>
			<w:trHeight w:val="300" w:hRule="exact"/>
			</w:tblBorders>
			</w:tblPrEx>
			<w:trPr>
				<w:tblHeader/>.
			</w:trPr>
			 
			
		</xsl:when>	
		<xsl:otherwise>
			
		</xsl:otherwise>
		</xsl:choose>

	
<xsl:apply-templates select="td">
	<xsl:with-param name="RowPosition" select="position()"/>
</xsl:apply-templates>
		 	
	
		</w:tr>
</xsl:template>


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

<xsl:template match="td"  name="td">

<xsl:param name="RowPosition"/>


				<w:tc><w:p>
			<w:pPr>
			<xsl:choose>
				<xsl:when test="$RowPosition=1">
					<w:jc w:val="center"/>
				</xsl:when>
				<xsl:otherwise>
					<w:jc w:val="right"/>
				</xsl:otherwise>

			</xsl:choose>
			</w:pPr>
					
					<w:r>
					<w:rPr>	
				    <w:rFonts w:ascii="Arial" w:hAnsi="Arial" w:cs="Arial"/>
					<xsl:choose>
					<xsl:when test="$RowPosition=1">
					<w:b w:val="true"/>
				</xsl:when>
				<xsl:otherwise>
					<w:b w:val="false"/>
				</xsl:otherwise>
				</xsl:choose>
					</w:rPr>	
					<w:t>
					<xsl:call-template name="insertBreaks"/>
					</w:t></w:r></w:p></w:tc>
				
</xsl:template>	


<xsl:template match="footnote" name="footnote">
<w:br/>
<w:r>
<w:rPr><w:rFonts w:ascii="Arial" w:hAnsi="Arial" w:cs="Arial"/></w:rPr>
<w:t><xsl:value-of select="."/></w:t></w:r>
</xsl:template>

  <xsl:template match="Program" name="Program">
  <w:ftr>
    <w:p>
      <w:pPr>
        <w:pStyle w:val="Footer"/>
      </w:pPr>
      <w:r>
        <w:rPr>
          <w:rFonts w:ascii="Arial" w:hAnsi="Arial" w:cs="Arial"/>
        </w:rPr>
        <w:t>
          Cambridge Clinical Trials Unit <xsl:value-of select="/Report/author"/> Page&#160; <w:fldSimple w:instr="PAGE"/>. <xsl:value-of select="/Report/datestamp"/>
        </w:t>
        <w:t>
          <w:br/>Program: <xsl:value-of select="."/> 
        </w:t>
      </w:r>
    </w:p>
  </w:ftr>
  </xsl:template>



  <xsl:template match="MetaFigure" name="MetaFigure">
 		<xsl:apply-templates select="heading"/>
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


















