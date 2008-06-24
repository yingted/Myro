<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"    
    xmlns:s="http://www.w3.org/2003/05/soap-envelope"    
    xmlns:motor="http://schemas.microsoft.com/robotics/2006/05/motor.html"
    xmlns:physical="http://schemas.microsoft.com/robotics/2006/07/physicalmodel.html"
    >

  <xsl:import href="/resources/dss/Microsoft.Dss.Runtime.Home.MasterPage.xslt" />

  <xsl:template match="/">
    <xsl:call-template name="MasterPage">
      <xsl:with-param name="serviceName">
        NXT Generic Motor
      </xsl:with-param>
      <xsl:with-param name="description">
        Provides access to the LEGO® MINDSTORMS® NXT motor.<br />
        (Uses the Generic Motor contract.)
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="s:Header">

  </xsl:template>

  <xsl:template match="motor:MotorState">
    <table width="100%" border="0" cellpadding="5" cellspacing="5">
      <tr>
        <th width="20%">Motor Name:</th>
        <td width="80%">
          <xsl:value-of select="motor:Name"/>
        </td>
      </tr>
      <tr>
        <th>Identifier:</th>
        <td>
          <xsl:value-of select="motor:HardwareIdentifier"/>
        </td>
      </tr>
      <tr>
        <th>Wheel Speed:</th>
        <td>
          <xsl:value-of select="motor:CurrentPower"/>
        </td>
      </tr>
      <tr>
        <th>Radius:</th>
        <td>
          <xsl:value-of select="motor:PowerScalingFactor"/>
        </td>
      </tr>
      <tr>
        <th>Gear Ratio:</th>
        <td>
          <xsl:value-of select="motor:ReversePolarity"/>
        </td>
      </tr>
      <tr>
        <th>Current Power:</th>
        <td>
          <xsl:value-of select="motor:CurrentPower"/>
        </td>
      </tr>
      <tr>
        <th>Power Scaling Factor:</th>
        <td>
          <xsl:value-of select="motor:PowerScalingFactor"/>
        </td>
      </tr>
      <tr>
        <th>Reverse Polarity:</th>
        <td>
          <xsl:value-of select="motor:ReversePolarity"/>
        </td>
      </tr>
      <tr>
        <th>Position X:</th>
        <td>
          <xsl:value-of select="motor:Pose/physical:Position/physical:X"/>
        </td>
      </tr>
      <tr>
        <th>Position Y:</th>
        <td>
          <xsl:value-of select="motor:Pose/physical:Position/physical:Y"/>
        </td>
      </tr>
      <tr>
        <th>Position Z:</th>
        <td>
          <xsl:value-of select="motor:Pose/physical:Position/physical:Z"/>
        </td>
      </tr>
      <tr>
        <th>Orientation X:</th>
        <td>
          <xsl:value-of select="motor:Pose/physical:Orientation/physical:X"/>
        </td>
      </tr>
      <tr>
        <th>Orientation Y:</th>
        <td>
          <xsl:value-of select="motor:Pose/physical:Orientation/physical:Y"/>
        </td>
      </tr>
      <tr>
        <th>Orientation Z:</th>
        <td>
          <xsl:value-of select="motor:Pose/physical:Orientation/physical:Z"/>
        </td>
      </tr>
      <tr>
        <th>Orientation W:</th>
        <td>
          <xsl:value-of select="motor:Pose/physical:Orientation/physical:W"/>
        </td>
      </tr>
    </table>
  </xsl:template>
</xsl:stylesheet>
