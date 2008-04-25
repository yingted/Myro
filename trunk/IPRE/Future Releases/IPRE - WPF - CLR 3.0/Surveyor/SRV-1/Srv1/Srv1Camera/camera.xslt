<?xml version="1.0" encoding="utf-8" ?>
<!--
//  This file is part of the Microsoft Robotics SDK Code Samples.
// 
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: Srv1Camera.xslt $ $Revision$
-->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:wc="http://schemas.sharplogic.com/robotics/2006/11/srv1camera.html">

  <xsl:output method="html" version="4.01" encoding="utf-8" indent="yes" doctype-public="-//W3C//DTD XHTML 1.0 Transitional//EN"
      doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd" />
  <xsl:template match="/wc:Srv1CameraState">
    <html>
      <head>
        <title>Surveyor Camera Viewer</title>
        <link rel="stylesheet" type="text/css" href="/resources/dss/Microsoft.Dss.Common.DSS.css" />
        <script language="javascript" type="text/javascript">
          <![CDATA[
          <!--// hide from older browsers
    var refreshTime = 250;
    var targetUrl = "/camera/jpeg";
    var feedRunning = false;
    var frameCount;
    var startTime;
    
    function loadImage()
    {
        //fire the function every refreshRate
        if (feedRunning)
        {
            setTimeout("loadImage()", refreshTime);        
        }

        // The following is a workaround to refresh images in IE7. Works for jpg only.
        if ((document.all)&&(navigator.appVersion.indexOf("MSIE 7.")!=-1))
        {
          if (document.TargetImg.src.toLowerCase().lastIndexOf("/jpeg") > 0)
            targetUrl = "/camera/jpg";
          else
            targetUrl = "/camera/jpeg";
        }

        document.TargetImg.src = targetUrl;
        frameCount++;
        if (frameCount % 4 == 0)
        {
            var now = new Date();
            var interval = now.valueOf() - startTime.valueOf();
            window.status = "Frame Rate: " + (1000 * frameCount / interval).toFixed(1) + "fps"
        }
    }    
    
    function startFeed()
    {
        feedRunning = true;
        frameCount = 0;
        startTime = new Date();
        loadImage();
        
        document.all("btnStart").disabled = true;
        document.all("btnStop").disabled = false;
    }
    
    function stopFeed()
    {
        feedRunning = false;

        document.all("btnStart").disabled = false;
        document.all("btnStop").disabled = true;
        
        window.status = window.defaultStatus;
    }
    
    function setRefresh(value)
    {
        refreshTime = value;
        frameCount = 0;
        startTime = new Date();
    }
    
    function viewXML()
    {
    var frame = window.parent.dssExternal;
    if (frame){
      top.document.body.getElementsByTagName("frameset").item(2).cols="10,0,*,0";
      window.open(document.location.href + '/raw','dssExternal');
    } else {
      window.open(document.location.href + '/raw','_top');
    }
    return true;
    } 
    // -->
]]>
        </script>

      </head>
      <body>
        <table width="100%" border="0" cellpadding="5" cellspacing="5">
          <tr valign="middle">
            <td align="left" style="font-size:large; color:Black;">
              <div id="DSSServiceName">
                Surveyor SRV-1 Camera Viewer
              </div>
            </td>
            <td align="right">
              <a  style="font-size:large; color:Black; cursor:hand;">
                <xsl:attribute name="href">
                  <xsl:text disable-output-escaping="yes">javascript:viewXML();</xsl:text>
                </xsl:attribute>
                RAW XML <img id="DSSServiceXML" src="/resources/dss/Microsoft.Dss.Runtime.Home.Images.xml_button.gif" width="31" height="16" border="0" alt="Raw XML" />
              </a>
            </td>
          </tr>
          <tr>
            <td colspan="2" align="left">
              <div id="DSSServiceDescription">
                <strong>Description: </strong>
                <i>Surveyor SRV-1 Camera Viewer</i>
              </div>
              <hr style="width:90%; height:1px;" />
            </td>
          </tr>
        </table>
        <form action="" method="post">
          <table>
            <tr class="odd">
              <th colspan="2">
                SRV-1 Camera
              </th>
            </tr>
            <tr class="even">
              <td colspan="2" align="center">
                <img id="TargetImg" name="TargetImg" src="/camera/jpeg"
                    alt="SRV-1 Camera Image">
                  <xsl:attribute name="width">
                    <xsl:value-of select="wc:Selected/wc:Format/wc:Width"/>
                  </xsl:attribute>
                  <xsl:attribute name="height">
                    <xsl:value-of select="wc:Selected/wc:Format/wc:Height"/>
                  </xsl:attribute>
                </img>
              </td>
            </tr>
            <tr class="odd">
              <th>
                Refresh Interval
              </th>
              <td>
                <input type="Text" name="Interval" value="250" size="1" onchange="setRefresh(this.value)" />
              </td>
            </tr>
            <tr class="even">
              <th>
                Display Format
              </th>
              <td>
                <select name="Format" onchange='targetUrl = "/camera/" + this.value'>
                  <option value="jpeg" selected="selected">JPEG</option>
                  <option value="gif">GIF</option>
                  <option value="bmp">BMP</option>
                </select>
              </td>
            </tr>
            <tr class="odd">
              <th>Capture Format</th>
              <td>
                <xsl:apply-templates select="wc:Selected/wc:SupportedFormats">
                  <xsl:with-param name="current" select="wc:Selected/wc:Format"/>
                </xsl:apply-templates>
              </td>
            </tr>
            <tr class="even">
              <th>Camera</th>
              <td>
                <xsl:apply-templates select="wc:Cameras"/>
              </td>
            </tr>
            <tr class="odd">
              <th>
                Control
              </th>
              <td>
                <button id="btnStart" name="btnStart" onclick="startFeed()">
                  Start
                </button>
                <button id="btnStop" name="btnStop" onclick="stopFeed()" disabled="disabled">
                  Stop
                </button>
              </td>
            </tr>
          </table>
        </form>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="wc:Cameras">
    <select name="Camera">
      <xsl:apply-templates select="wc:CameraInstance">
        <xsl:with-param name="selected" select="/wc:SurveyorStateType/wc:Selected/wc:DevicePath"/>
        <xsl:sort select="wc:FriendlyName"/>
      </xsl:apply-templates>
    </select>
    <xsl:text> </xsl:text>
    <input name="ChangeCamera" type="Submit" value="Change"/>
  </xsl:template>

  <xsl:template match="wc:CameraInstance">
    <xsl:param name="selected"/>
    <option>
      <xsl:attribute name="value">
        <xsl:value-of select="wc:DevicePath"/>
      </xsl:attribute>
      <xsl:if test="$selected = wc:DevicePath">
        <xsl:attribute name="selected">
          <xsl:text>selected</xsl:text>
        </xsl:attribute>
      </xsl:if>
      <xsl:value-of select="wc:FriendlyName"/>
    </option>
  </xsl:template>

  <xsl:template match="wc:Selected/wc:SupportedFormats">
    <xsl:param name="current"/>
    <select name="CaptureFormat">
      <xsl:if test="count(wc:Format) = 0">
        <option value="0" selected="selected">
          <xsl:value-of select="$current/wc:Width"/>
          <xsl:text>x</xsl:text>
          <xsl:value-of select="$current/wc:Height"/>
          <xsl:if test="string-length($current/wc:Compression) > 0">
            <xsl:text> - </xsl:text>
            <xsl:value-of select="$current/wc:Compression"/>
          </xsl:if>
        </option>
      </xsl:if>
      <xsl:apply-templates select="wc:Format">
        <xsl:with-param name="current" select="$current"/>
      </xsl:apply-templates>
    </select>
    <xsl:text> </xsl:text>
    <input name="ChangeFormat" type="Submit" value="Change"/>
  </xsl:template>

  <xsl:template match="wc:Format">
    <xsl:param name="current"/>
    <option>
      <xsl:attribute name="value">
        <xsl:value-of select="position()"/>
      </xsl:attribute>
      <xsl:if test="$current/wc:Width = wc:Width and $current/wc:Height = wc:Height and $current/wc:Compression = wc:Compression">
        <xsl:attribute name="selected">selected</xsl:attribute>
      </xsl:if>
      <xsl:value-of select="wc:Width"/>
      <xsl:text>x</xsl:text>
      <xsl:value-of select="wc:Height"/>
      <xsl:if test="string-length(wc:Compression) > 0">
        <xsl:text> - </xsl:text>
        <xsl:value-of select="wc:Compression"/>
      </xsl:if>
    </option>
  </xsl:template>
</xsl:stylesheet>