<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"    
    xmlns:s="http://www.w3.org/2003/05/soap-envelope"    
    xmlns:backupbeep="http://schemas.microsoft.com/2006/06/legonxtbackupbeep.html"
    >

  <xsl:import href="/resources/dss/Microsoft.Dss.Runtime.Home.MasterPage.xslt" />

  <xsl:template match="/">
    <xsl:call-template name="MasterPage">
      <xsl:with-param name="serviceName">
        NXT Backup Beep
      </xsl:with-param>
      <xsl:with-param name="description">
        Monitors the LEGO® MINDSTORMS® NXT motors and requests the NXT brick to sound a backup beeping sound when the motors are in reverse.
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="s:Header">

  </xsl:template>

  <xsl:template match="backupbeep:LegoNxtBackupBeepState">
    <table width="100%">
      <tr>
        <th width="20%">Play Duration:</th>
        <td width="80%">
          <xsl:value-of select="backupbeep:PlayDuration"/>
        </td>
      </tr>
      <tr class="odd">
        <th>Pause Duration:</th>
        <td>
          <xsl:value-of select="backupbeep:PauseDuration"/>
        </td>
      </tr>
      <tr>
        <th>Frequency:</th>
        <td>
          <xsl:value-of select="backupbeep:Frequency"/>
        </td>
      </tr>
      <tr class="odd">
        <th>Motor 1 Port:</th>
        <td>
          <xsl:value-of select="backupbeep:Motor1Port"/>
        </td>
      </tr>
      <tr>
        <th>Motor 2 Port:</th>
        <td>
          <xsl:value-of select="backupbeep:Motor2Port"/>
        </td>
      </tr>
    </table>
  </xsl:template>
</xsl:stylesheet>
