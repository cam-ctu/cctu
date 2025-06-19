<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:o="urn:schemas-microsoft-com:office:office"
  xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"
  xmlns:v="urn:schemas-microsoft-com:vml"
  xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"
  xmlns:w10="urn:schemas-microsoft-com:office:word"
  xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing"
  xmlns:wps="http://schemas.microsoft.com/office/word/2010/wordprocessingShape"
  xmlns:wpg="http://schemas.microsoft.com/office/word/2010/wordprocessingGroup"
  xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"
  xmlns:wp14="http://schemas.microsoft.com/office/word/2010/wordprocessingDrawing"
  xmlns:w14="http://schemas.microsoft.com/office/word/2010/wordml"
  xmlns:w15="http://schemas.microsoft.com/office/word/2012/wordml" 
  mc:Ignorable="w14 wp14 w15"
>
  <xsl:output method="xml" indent="no" />

  <xsl:template match="/">
    <w:document>
      <w:body>
      
        <xsl:apply-templates select="Report/." />

        <xsl:call-template name="PageSection">
          <xsl:with-param name="orientation" select="Report/*[last()]/pagesection/orientation" />
          <xsl:with-param name="margin" select="Report/*[last()]/pagesection/margin" />
          <xsl:with-param name="headerid" select="Report/*[last()]/pagesection/headerid" />
          <xsl:with-param name="footerid" select="Report/*[last()]/pagesection/footerid" />
        </xsl:call-template>

      </w:body>

    </w:document>
  </xsl:template>

  <!-- Report front page -->
  <xsl:template match="frontpage" name="frontpage">
    <w:p>
      <w:pPr>
        <w:spacing w:line="480" w:lineRule="auto" />
        <w:jc w:val="left" />
        <w:sectPr>
          <w:pgSz w:h="16840" w:w="11900" w:orient="portrait" />
          <w:titlePg />
        </w:sectPr>
      </w:pPr>
      <w:r>
        <w:rPr>
          <w:b />
          <w:sz w:val="30" />
        </w:rPr>
        <w:t>Document Title:<xsl:value-of select="study" /></w:t>
        <w:br />
        <w:t>Author:<xsl:value-of select="author" /></w:t>
        <w:br />
        <w:t>Date:<xsl:value-of select="datestamp" /></w:t>
      </w:r>
    </w:p>
    <w:p>
      <w:r>
        <w:br w:type="page" />
      </w:r>
    </w:p>
  
  </xsl:template>

  <!-- Report plain text -->
  <xsl:template match="MetaText" name="MetaText">
    <xsl:apply-templates select="heading">
      <xsl:with-param name="titletype" select="'Table'" />
    </xsl:apply-templates>
    <w:p>
      <w:pPr>
        <w:jc w:val="center" />
      </w:pPr>
      <xsl:apply-templates select="text" />
    </w:p>

    <w:p>
      <xsl:apply-templates select="footnote" />
    </w:p>

    <w:p>
      <w:pPr>
        <xsl:if test="not(position() = last())">
          <xsl:call-template name="PageSection">
            <xsl:with-param name="orientation" select="pagesection/orientation" />
            <xsl:with-param name="margin" select="pagesection/margin" />
            <xsl:with-param name="headerid" select="pagesection/headerid" />
            <xsl:with-param name="footerid" select="pagesection/footerid" />
          </xsl:call-template>
        </xsl:if>
      </w:pPr>
    </w:p>

  </xsl:template>

  <xsl:template match="text" name="text">
    <xsl:if test="position() &gt;1">
      <w:br />
    </xsl:if>
    <w:r>
        <xsl:call-template name="insertBreaks" />
    </w:r>
  </xsl:template>

  <!-- Report table -->
  <xsl:template match="MetaTable" name="MetaTable">
    <xsl:apply-templates select="heading">
      <xsl:with-param name="titletype" select="'Table'" />
    </xsl:apply-templates>
    <w:tbl>
      <w:tblPr>
        <w:tblW w:w="4500" w:type="auto" />
        <w:jc w:val="center" />
        <w:tblBorders>
          <w:top w:val="single" w:sz="10" w:space="0" />
          <w:bottom w:val="single" w:sz="10" w:space="0" />
        </w:tblBorders>
        <w:tblCellMar>
          <w:right w:w="100" w:type="dxa" />
        </w:tblCellMar>
      </w:tblPr>
      <xsl:apply-templates select="table/thead/tr">
        <xsl:with-param name="fontsize">
          <xsl:choose>
            <xsl:when test="heading/fontsize=''">20</xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="heading/fontsize" />
            </xsl:otherwise>
          </xsl:choose>
        </xsl:with-param>
      </xsl:apply-templates>
      <xsl:apply-templates select="table/tbody/tr">
        <xsl:with-param name="fontsize">
          <xsl:choose>
            <xsl:when test="heading/fontsize=''">20</xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="heading/fontsize" />
            </xsl:otherwise>
          </xsl:choose>
        </xsl:with-param>
      </xsl:apply-templates>
    </w:tbl>

    <w:p>
      <xsl:apply-templates select="footnote" />
    </w:p>

    <w:p>
      <w:pPr>
        <xsl:if test="not(position() = last())">
          <xsl:call-template name="PageSection">
            <xsl:with-param name="orientation" select="pagesection/orientation" />
            <xsl:with-param name="margin" select="pagesection/margin" />
            <xsl:with-param name="headerid" select="pagesection/headerid" />
            <xsl:with-param name="footerid" select="pagesection/footerid" />
          </xsl:call-template>
        </xsl:if>
      </w:pPr>
    </w:p>
  </xsl:template>

  <!--Heading-->
  <xsl:template match="heading" name="heading">
    <xsl:param name="titletype" />
    <xsl:variable name="previous_section" select="../preceding-sibling::*[1]/heading/section" />
    <xsl:variable name="current_section" select="section" />
    <!-- Apply Page break only if this is not a new section -->
    <xsl:if test="../pagesection/orientation = ../preceding-sibling::*[1]/pagesection/orientation">
    <w:p>
      <w:r>
        <w:br w:type="page" />
      </w:r>
    </w:p>
    </xsl:if>

    <xsl:if test="not( $previous_section = $current_section)">
      <w:p>
        <w:pPr>
          <w:jc w:val="center" />
          <w:outlineLvl w:val="1" />
        </w:pPr>
        <w:r>
          <w:t> Section: <xsl:value-of select="section" />
          </w:t>
        </w:r>
      </w:p>
    </xsl:if>
    <w:p>
      <w:pPr>
        <w:jc w:val="center" />
        <w:outlineLvl w:val="2" />
      </w:pPr>
      <!--Start
      Bookmark -->
      <!--<aml:annotation
      w:type="Word.Bookmark.Start" /> -->
      <xsl:variable name="numberid" select="translate(number, '. ', '')" />
      <w:bookmarkStart w:id="{$numberid}" w:name="{$titletype}_{$numberid}" />
      <w:r>
        <w:t>
          <xsl:value-of select="$titletype" />
        </w:t>
        <w:t xml:space="preserve"> </w:t>
        <!--Start
        Numbering table with field -->
        <w:fldChar w:fldCharType="begin" w:fldLock="1" />
        <xsl:if test="$titletype='Table'">
          <w:instrText> SEQ Table \* ARABIC</w:instrText>
        </xsl:if>
        <xsl:if test="$titletype='Figure'">
          <w:instrText> SEQ Figure \* ARABIC</w:instrText>
        </xsl:if>
        <w:fldChar w:fldCharType="separate" />
        <w:t>
          <xsl:value-of select="number" />
        </w:t>
        <w:t xml:space="preserve"> </w:t>
        <w:fldChar w:fldCharType="end" />
        <!--Numbering
        table with field ends-->
      </w:r>
      <w:bookmarkEnd w:id="{$numberid}" />
      <!--<aml:annotation
      w:type="Word.Bookmark.End" />-->
      <!--Bookmark
      ends-->
      <w:r>
        <w:t>
          <xsl:value-of select="title" />
        </w:t>
      </w:r>
    </w:p>
    <w:p>
      <w:pPr>
        <w:jc w:val="center" />
      </w:pPr>
      <xsl:apply-templates select="subtitle" />
      <xsl:apply-templates select="population" />
    </w:p>
  </xsl:template>

  <!--Population-->
  <xsl:template match="population" name="population">
    <xsl:if test=". != '' ">
      <w:r>
        <w:t>Population: <xsl:value-of select="." /></w:t>
      </w:r>
    </xsl:if>
  </xsl:template>

  <!--Subtitle-->
  <xsl:template match="subtitle" name="subtitle">
    <xsl:if test=". != '' ">
      <w:r>
        <w:t>
          <xsl:value-of select="." />
        </w:t>
        <w:br />
      </w:r>
    </xsl:if>
  </xsl:template>

  <!--Table
  header rows-->
  <xsl:template match="thead/tr">
    <xsl:param name="fontsize" select="$fontsize" />
    <w:tr>
      <w:tblPrEx>
        <w:tblBorders>
          <w:insideH w:val="single" w:sz="5" w:space="0" />
        </w:tblBorders>
      </w:tblPrEx>
      <w:trPr>
        <w:tblHeader />
      </w:trPr>
      <xsl:apply-templates select="th">
        <xsl:with-param name="fontsize" select="$fontsize" />
      </xsl:apply-templates>
    </w:tr>
  </xsl:template>

  <!--Table
  header cells-->
  <xsl:template match="th" name="th">
    <xsl:param name="fontsize" select="$fontsize" />
    <w:tc>
      <w:p>
        <w:pPr>
          <w:jc w:val="center" />
        </w:pPr>
        <w:r>
          <w:rPr>
            <w:b />
            <!--Table
            font size to 9-->
            <w:sz w:val="{$fontsize}" />
          </w:rPr>
            <xsl:call-template name="insertBreaks" />
        </w:r>
      </w:p>
    </w:tc>
  </xsl:template>

  <!--Table
  body rows-->
  <xsl:template match="tbody/tr">
    <xsl:param name="fontsize" select="$fontsize" />
    <xsl:variable name="span">
      <xsl:choose>
        <xsl:when test="td[1]/@style and contains(td[1]/@style, 'span')">
          <!--<xsl:value-of
          select="td[1]/@COLSPAN"/> -->
          <xsl:value-of select="count(td)" />
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="0" />
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:variable name="align">
      <xsl:choose>
        <xsl:when test="td[1]/@style and contains(td[1]/@style, 'firstleft')">
          <!--<xsl:value-of
          select="td[1]/@COLSPAN"/> -->
          <xsl:value-of select="1" />
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="0" />
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <w:tr>
      <!-- Do not apply templates to span row and not first column.
       Thi is to avoid print extra cells after span -->
      <xsl:apply-templates select="td[not($span != 0 and position() != 1)]">
        <xsl:with-param name="span" select="$span" />
        <xsl:with-param name="align" select="$align" />
        <xsl:with-param name="fontsize" select="$fontsize" />
      </xsl:apply-templates>
    </w:tr>
  </xsl:template>

  <!--Table
  body cells-->
  <xsl:template match="td">
    <xsl:param name="span" />
    <xsl:param name="align" />
    <xsl:param name="fontsize" select="$fontsize" />

    <w:tc>
      <w:tcPr>
        <!--Merge
        cells-->
        <xsl:if test="$span != 0 and position() &lt;= $span">
          <w:gridSpan w:val="{$span}" />
        </xsl:if>
        <!--Cell
        Color-->
        <xsl:if test="@style and contains(@style, 'bgcol')">
          <w:shd w:val="clear" w:fill="d3d3d3" />
        </xsl:if>
      </w:tcPr>

      <w:p>
        <w:pPr>
          <!--Indent-->
          <xsl:if test="@style and contains(@style, 'indent')">
            <w:ind w:left="200" />
          </xsl:if>
          <!--Left
          align if first column-->
          <xsl:choose>
            <xsl:when test="$align=1 and position()!=1">
              <w:jc w:val="center" />
            </xsl:when>
            <xsl:otherwise>
              <w:jc w:val="left" />
            </xsl:otherwise>
          </xsl:choose>

        </w:pPr>
        <w:r>
          <w:rPr>
            <!--Bold-->
            <xsl:if test="@style and contains(@style, 'bold')">
              <w:b />
            </xsl:if>
            <!--Table
            font size to 9-->
            <w:sz w:val="{$fontsize}" />

          </w:rPr>
            <xsl:call-template name="insertBreaks" />
        </w:r>
      </w:p>
    </w:tc>
  </xsl:template>

  <!--Cell text-->
  <xsl:template match="text()" name="insertBreaks">
    <xsl:param name="pText" select="." />
    <xsl:choose>
      <xsl:when test="not(contains($pText, '&#xA;'))">
        <w:t><xsl:value-of select="$pText" /></w:t>
      </xsl:when>
      <xsl:otherwise>
        <w:t>
        <xsl:value-of select="substring-before($pText, '&#xA;')" />
      </w:t><w:br />
        <xsl:call-template name="insertBreaks">
            <xsl:with-param name="pText" select="substring-after($pText, '&#xA;')" />
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!--Footnote-->
  <xsl:template match="footnote" name="footnote">
    <w:r>
        <!--Convert line break-->
        <xsl:call-template name="insertBreaks" />
    </w:r>
    <!-- Page break
    <w:r>
      <w:br w:type="page" />
    </w:r>
  -->
  </xsl:template>

  <!--Image-->
  <xsl:template match="MetaFigure" name="MetaFigure">
    <xsl:apply-templates select="heading">
      <xsl:with-param name="titletype" select="'Figure'" />
    </xsl:apply-templates>
    <w:p>
      <w:pPr>
        <w:jc w:val="center" />
      </w:pPr>
      <w:r>
        <xsl:variable name="picrid">
          <xsl:value-of select="rid" />
        </xsl:variable>
        <xsl:variable name="height">
          <xsl:value-of select="height" />
        </xsl:variable>
        <xsl:variable name="width">
          <xsl:value-of select="width" />
        </xsl:variable>
        <w:drawing>
          <wp:inline distT="0" distB="0" distL="0" distR="0">
            <wp:extent cx="{$width}" cy="{$height}" />
            <wp:docPr id="{$picrid}" name="" descr="" />
            <wp:cNvGraphicFramePr>
              <a:graphicFrameLocks xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main"
                noChangeAspect="1" />
            </wp:cNvGraphicFramePr>
            <a:graphic xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main">
              <a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/picture">
                <pic:pic xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture">
                  <pic:nvPicPr>
                    <pic:cNvPr id="2" name="" />
                    <pic:cNvPicPr>
                      <a:picLocks noChangeAspect="1" noChangeArrowheads="1" />
                    </pic:cNvPicPr>
                  </pic:nvPicPr>
                  <pic:blipFill>
                    <a:blip cstate="hqprint" r:embed="rId{$picrid}" />
                    <a:stretch>
                      <a:fillRect />
                    </a:stretch>
                  </pic:blipFill>
                  <pic:spPr bwMode="auto">
                    <a:xfrm>
                      <a:off x="0" y="0" />
                      <a:ext cx="{$width}" cy="{$height}" />
                    </a:xfrm>
                    <a:prstGeom prst="rect">
                      <a:avLst />
                    </a:prstGeom>
                    <a:noFill />
                  </pic:spPr>
                </pic:pic>
              </a:graphicData>
            </a:graphic>
          </wp:inline>
        </w:drawing>
        <!-- 
        <w:pict >
          <v:shape style="width:{$width}pt;height:{$height}pt">
            <v:imagedata r:id="rId{$picrid}" o:title="" />
          </v:shape>
        </w:pict>
      -->
      </w:r>
    </w:p>

    <w:p>
      <xsl:apply-templates select="footnote" />
    </w:p>
    
    <w:p>
      <w:pPr>
        <xsl:if test="not(position() = last())">
          <xsl:call-template name="PageSection">
            <xsl:with-param name="orientation" select="pagesection/orientation" />
            <xsl:with-param name="margin" select="pagesection/margin" />
            <xsl:with-param name="headerid" select="pagesection/headerid" />
            <xsl:with-param name="footerid" select="pagesection/footerid" />
          </xsl:call-template>
        </xsl:if>
      </w:pPr>
    </w:p>
  </xsl:template>

  <xsl:template name="PageSection">
    <xsl:param name="orientation" />
    <xsl:param name="footerid" />
    <xsl:param name="headerid" />
    <xsl:param name="margin" />
    <xsl:variable name="lower_case_orientation">
      <xsl:value-of
        select="translate($orientation,'ABCDEFGHIJKLYMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')" />
    </xsl:variable>

    <w:sectPr>
      <xsl:if test="$headerid != ''">
        <w:headerReference w:type="default" r:id="{$headerid}" />
      </xsl:if>
      <xsl:if test="$footerid != ''">
        <w:footerReference w:type="default" r:id="{$footerid}" />
      </xsl:if>
      <w:type w:val="continuous" />
      <xsl:choose>
        <xsl:when test="$lower_case_orientation='portrait'">
          <w:pgSz w:h="16840" w:w="11900" w:orient="portrait" />
        </xsl:when>
        <xsl:otherwise>
          <w:pgSz w:w="16840" w:h="11900" w:orient="landscape" />
        </xsl:otherwise>
      </xsl:choose>
      <xsl:choose>
        <xsl:when test="$margin !='narrow'">
          <w:pgMar w:top="1440" w:right="1800" w:bottom="1440" w:left="1800" w:header="720" w:footer="720" w:gutter="0" />
        </xsl:when>
        <xsl:otherwise>
          <w:pgMar w:top="720" w:right="720" w:bottom="720" w:left="720" w:header="720" w:footer="720" w:gutter="0"/>
        </xsl:otherwise>  
      </xsl:choose>
      <w:cols w:space="720" />
    </w:sectPr>
  </xsl:template>

</xsl:stylesheet>