<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"    
    xmlns:s="http://www.w3.org/2003/05/soap-envelope"    
    xmlns:legonxtbattery="http://schemas.microsoft.com/2006/06/legonxtbattery.html"  
    xmlns:battery="http://schemas.microsoft.com/2006/06/legonxtbattery.html"
    >

  <xsl:import href="/resources/dss/Microsoft.Dss.Runtime.Home.MasterPage.xslt" />

  <xsl:template match="/">
    <xsl:call-template name="MasterPage">
      <xsl:with-param name="serviceName">
        NXT Generic Battery
      </xsl:with-param>
      <xsl:with-param name="description">
        Provides access to the LEGO® MINDSTORMS® NXT battery sensor.<br />
        (Uses the Generic Battery contract).
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="s:Header">

  </xsl:template>

  <xsl:template match="legonxtbattery:LegoNxtBatteryState">
    <table width="100%">
      <tr>
        <th width="20%">Max Battery Power:</th>
        <td width="80%">
          <xsl:value-of select="battery:MaxBatteryPower"/>
        </td>
      </tr>
      <tr class="odd">
        <th>Percent Battery Power:</th>
        <td>
          <xsl:value-of select="battery:PercentBatteryPower"/>
        </td>
      </tr>
      <tr>
        <th>Percent Critical Battery:</th>
        <td>
          <xsl:value-of select="battery:PercentCriticalBattery"/>
        </td>
      </tr>
      <tr class="odd">
        <th>Poll Delay Time:</th>
        <td>
          <xsl:value-of select="legonxtbattery:PollDelayTime"/>
        </td>
      </tr>
    </table>
  </xsl:template>
  
</xsl:stylesheet>
