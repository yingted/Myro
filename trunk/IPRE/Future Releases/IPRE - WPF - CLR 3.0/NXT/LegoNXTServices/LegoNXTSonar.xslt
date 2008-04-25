<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"    
    xmlns:s="http://www.w3.org/2003/05/soap-envelope"    
    xmlns:sonar="http://schemas.microsoft.com/robotics/2006/06/sonar.html"
    xmlns:physical="http://schemas.microsoft.com/robotics/2006/07/physicalmodel.html"
    >

  <xsl:import href="/resources/dss/Microsoft.Dss.Runtime.Home.MasterPage.xslt" />

  <xsl:template match="/">
    <xsl:call-template name="MasterPage">
      <xsl:with-param name="serviceName">
        Lego NXT Generic Sonar
      </xsl:with-param>
      <xsl:with-param name="description">
        Provides access to the LEGO® MINDSTORMS® NXT sonar sensor.<br />
        (Uses the Generic Sonar Sensor contract.)
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="s:Header">

  </xsl:template>

  <xsl:template match="sonar:SonarState">
    <table width="100%">
      <tr>
        <th width="20%">Hardware Identifier:</th>
        <td width="80%">
          <xsl:value-of select="sonar:HardwareIdentifier"/>
        </td>
      </tr>
      <tr class="odd">
        <th>Max Distance:</th>
        <td>
          <xsl:value-of select="sonar:MaxDistance"/>
        </td>
      </tr>
      <tr>
        <th>Distance Measurement:</th>
        <td>
          <xsl:value-of select="sonar:DistanceMeasurement"/>
        </td>
      </tr>
      <tr class="odd">
        <th>Angular Range:</th>
        <td>
          <xsl:value-of select="sonar:AngularRange"/>
        </td>
      </tr>
      <tr>
        <th>Angular Resolution:</th>
        <td>
          <xsl:value-of select="sonar:AngularResolution"/>
        </td>
      </tr>

      <xsl:apply-templates select="sonar:Pose/physical:Position"/>
      <xsl:apply-templates select="sonar:Pose/physical:Orientation"/>

    </table>
  </xsl:template>

  <xsl:template match="sonar:Pose/physical:Position">
    <tr class="odd">
      <th>
        X Position
      </th>
      <td>
        <xsl:value-of select="physical:X"/>
      </td>
    </tr>
    <tr>
      <th>
        Y Position
      </th>
      <td>
        <xsl:value-of select="physical:Y"/>
      </td>
    </tr>
    <tr class="odd">
      <th>
        Z Position
      </th>
      <td>
        <xsl:value-of select="physical:Z"/>
      </td>
    </tr>
  </xsl:template>

  <xsl:template match="sonar:Pose/physical:Orientation">
    <tr>
      <th>
        X Orientation
      </th>
      <td>
        <xsl:value-of select="physical:X"/>
      </td>
    </tr>
    <tr class="odd">
      <th>
        Y Orientation
      </th>
      <td>
        <xsl:value-of select="physical:Y"/>
      </td>
    </tr>
    <tr>
      <th>
        Z Orientation
      </th>
      <td>
        <xsl:value-of select="physical:Z"/>
      </td>
    </tr>
  </xsl:template>
  
</xsl:stylesheet>
