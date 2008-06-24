<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"    
    xmlns:s="http://www.w3.org/2003/05/soap-envelope"    
    xmlns:encoder="http://schemas.microsoft.com/robotics/2006/05/encoder.html"
    >

  <xsl:import href="/resources/dss/Microsoft.Dss.Runtime.Home.MasterPage.xslt" />

  <xsl:template match="/">
    <xsl:call-template name="MasterPage">
      <xsl:with-param name="serviceName">
        NXT Generic Encoder
      </xsl:with-param>
      <xsl:with-param name="description">
        Provides access to the LEGO® MINDSTORMS® NXT motor encoder.<br />
        (Uses the Generic Encoder contract).
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="s:Header">

  </xsl:template>

  <xsl:template match="encoder:EncoderState">
    <table width="100%">
      <tr>
        <th width="20%">Hardware Identifier:</th>
        <td width="80%">
          <xsl:value-of select="encoder:HardwareIdentifier"/>
        </td>
      </tr>
      <tr class="odd">
        <th>Ticks Per Revolution:</th>
        <td>
          <xsl:value-of select="encoder:TicksPerRevolution"/>
        </td>
      </tr>
      <tr>
        <th>Current Reading:</th>
        <td>
          <xsl:value-of select="encoder:CurrentReading"/>
        </td>
      </tr>
      <tr class="odd">
        <th>Current Angle:</th>
        <td>
          <xsl:value-of select="encoder:CurrentAngle"/>
        </td>
      </tr>
      <tr>
        <th>Ticks Since Reset:</th>
        <td>
          <xsl:value-of select="encoder:TicksSinceReset"/>
        </td>
      </tr>
      <tr class="odd">
        <th>Time Stamp:</th>
        <td>
          <xsl:value-of select="encoder:TimeStamp"/>
        </td>
      </tr>
    </table>
  </xsl:template>
  
</xsl:stylesheet>
