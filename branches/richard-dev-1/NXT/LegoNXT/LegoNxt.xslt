<?xml version="1.0" encoding="utf-8"?>
<!--
//  This file is part of the Microsoft Robotics SDK Code Samples.
// 
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: MicrosoftGps.xslt $ $Revision$
-->
<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:nxt="http://schemas.microsoft.com/robotics/2006/05/legonxt.html"
    xmlns:fn="http://www.w3.org/2005/02/xpath-functions/"
    >

  <xsl:import href="/resources/dss/Microsoft.Dss.Runtime.Home.MasterPage.xslt" />

  <xsl:template match="/">
    <xsl:call-template name="MasterPage">
      <xsl:with-param name="serviceName">
        LEGO® NXT Brick
      </xsl:with-param>
      <xsl:with-param name="description">
        Provides access to the LEGO® MINDSTORMS® NXT Brick general service.
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="/nxt:LegoNxtState">
    <table style="border:0px;">
      <tr>
        <td align="top">

          <form method="POST" action="" name="ConfigurationForm">
            <input type="hidden" name="Action" value="LegoNxtConfig" />

            <table width="400px">

              <tr>
                <th colspan="2" class="Major">Configuration</th>
              </tr>

              <xsl:call-template name="nxt:SerialPortConfig"/>

              <tr>
                <td colspan="2">
                  <xsl:text>&#160;</xsl:text>
                </td>
              </tr>

              <tr>
                <td colspan="2">
                  <table width="100%">
                    <tr>
                      <th>Sensor Configuration</th>
                    </tr>
                    <xsl:variable name="SensorTypesList" select="nxt:BrickConfig/nxt:AvailableSensorTypes" />
                    <xsl:for-each select="nxt:BrickConfig/nxt:SensorPort/nxt:SensorConfig">
                      <xsl:call-template name="nxt:SensorConfigRow">
                        <xsl:with-param name="List" select="$SensorTypesList" />
                      </xsl:call-template>
                    </xsl:for-each>
                  </table>
                </td>
              </tr>

              <tr>
                <td colspan="2">
                  <xsl:text>&#160;</xsl:text>
                </td>
              </tr>

              <tr>
                <td colspan="2">
                  <table width="100%">
                    <tr>
                      <th>Motor Configuration</th>
                    </tr>
                    <xsl:variable name="MotorSensorTypesList" select="nxt:BrickConfig/nxt:AvailableMotorSensorTypes" />
                    <xsl:for-each select="nxt:BrickConfig/nxt:MotorPort/nxt:MotorConfig">
                      <xsl:call-template name="nxt:MotorConfigRow">
                        <xsl:with-param name="List" select="$MotorSensorTypesList" />
                      </xsl:call-template>
                    </xsl:for-each>
                  </table>
                </td>
              </tr>

              <tr>
                <td colspan="2">
                  <xsl:text>&#160;</xsl:text>
                </td>
              </tr>

              <tr>
                <td colspan="2">
                  <table width="100%">
                    <tr>
                      <th colspan="3">Button Configuration</th>
                    </tr>
                    <xsl:variable name="ButtonSensorTypesList" select="nxt:BrickConfig/nxt:AvailableButtonSensorTypes" />
                    <xsl:for-each select="nxt:BrickConfig/nxt:ButtonPort/nxt:string">
                      <xsl:call-template name="nxt:ButtonConfigRow">
                        <xsl:with-param name="List" select="$ButtonSensorTypesList" />
                      </xsl:call-template>
                    </xsl:for-each>
                  </table>
                </td>
              </tr>

              <tr>
                <td colspan="2">
                  <xsl:text>&#160;</xsl:text>
                </td>
              </tr>

              <tr>
                <td>
                  <input id="Button2" type="reset" value="Reset" name="buttonOk" title="Reset Configuration"/>
                </td>
                <td>
                  <input id="Button1" type="submit" value="Connect" name="buttonOk" title="Update and Connect"/>
                </td>
              </tr>

            </table>

          </form>
        </td>

        <td align="top" style="vertical-align:top; padding-left:20px;">



          <table width="200px">

            <tr>
              <th colspan="2" class="Major">Sensor Inputs</th>
            </tr>


            <tr>
              <td colspan="2">
                <xsl:text>&#160;</xsl:text>
              </td>
            </tr>

            <form method="POST" action="" name="PollForm">
              <input type="hidden" name="Action" value="LegoNxtSensorPoll" />

              <tr>
                <th colspan="2">Sensor Port</th>
              </tr>

              <tr>
                <td colspan="2">
                  <table width="100%">
                    <xsl:for-each select="nxt:SensorPort/nxt:int">
                      <tr>
                        <th class="Left">
                          <xsl:value-of select="position()"/>
                        </th>
                        <td>
                          <xsl:value-of select="."/>
                        </td>
                      </tr>
                    </xsl:for-each>
                  </table>
                </td>
              </tr>

            </form>

            <tr>
              <td colspan="2">
                <xsl:text>&#160;</xsl:text>
              </td>
            </tr>

            <form method="POST" action="" name="PollForm">
              <input type="hidden" name="Action" value="LegoNxtMotorPoll" />

              <tr>
                <th colspan="2">Motor Port</th>
              </tr>

              <tr>
                <td colspan="2">
                  <table width="100%">
                    <xsl:for-each select="nxt:MotorSensorPort/nxt:int">
                      <tr>
                        <th class="Left">
                          <xsl:value-of select="substring('ABCDEFG',position(),1)"/>
                        </th>
                        <td>
                          <xsl:value-of select="."/>
                        </td>
                      </tr>
                    </xsl:for-each>
                  </table>
                </td>
              </tr>

            </form>

            <tr>
              <td colspan="2">
                <xsl:text>&#160;</xsl:text>
              </td>
            </tr>

            <tr>
              <th colspan="2">Button Port</th>
            </tr>

            <tr>
              <td colspan="2">
                <table width="100%">
                  <tr>
                    <th class="Left" style="width:50px;">Right</th>
                    <td>
                      <xsl:value-of select="nxt:LegoButtons/nxt:Right"/>
                    </td>
                  </tr>
                  <tr>
                    <th class="Left" style="width:50px;">Left</th>
                    <td>
                      <xsl:value-of select="nxt:LegoButtons/nxt:Left"/>
                    </td>
                  </tr>
                  <tr>
                    <th class="Left" style="width:50px;">Enter</th>
                    <td>
                      <xsl:value-of select="nxt:LegoButtons/nxt:Enter"/>
                    </td>
                  </tr>
                  <tr>
                    <th class="Left" style="width:50px;">Cancel</th>
                    <td>
                      <xsl:value-of select="nxt:LegoButtons/nxt:Cancel"/>
                    </td>
                  </tr>
                </table>
              </td>
            </tr>

            <tr>
              <td colspan="2">
                <xsl:text>&#160;</xsl:text>
              </td>
            </tr>

            <tr>
              <th width="140px">Battery Voltage</th>
              <td>
                <xsl:value-of select="nxt:BatteryVoltage"/>
              </td>
            </tr>

          </table>



          <p>
            <xsl:text>&#160;</xsl:text>
          </p>

          <form method="POST" action="" name="SetOutputForm">
            <input type="hidden" name="Action" value="LegoNxtOutputs" />

            <table width="200px">
              <tr>
                <th colspan="2" class="Major">Motor Outputs</th>
              </tr>
              <tr>
                <td colspan="2">
                  <xsl:text>&#160;</xsl:text>
                </td>
              </tr>
              <tr>
                <th colspan="2">Motor Port</th>
              </tr>
              <tr>
                <td colspan="2">
                  <table width="100%">

                    <xsl:for-each select="nxt:MotorOutputPort/nxt:int">
                      <xsl:call-template name="nxt:MotorOutputRow"/>
                    </xsl:for-each>

                  </table>
                </td>

                <tr>
                  <td style="text-align:right;">
                    <input type="submit" value="All Stop" name="button_stop_all" title="Stop All Motors"/>

                    <input type="submit" value="Set All Motors" name="button_set_all" title="Set All Motors"/>
                  </td>
                </tr>

              </tr>
            </table>

          </form>
        </td>
      </tr>
    </table>
  </xsl:template>


  <xsl:template name="nxt:MotorOutputRow">
    <tr>
      <th class="Left">
        <xsl:value-of select="substring('ABCDEFG',position(),1)"/>
      </th>
      <td>
        <input type="text" size="10">
          <xsl:attribute name="name">
            <xsl:value-of select="'MotorOutput'"/>
            <xsl:value-of select="position()"/>
          </xsl:attribute>
          <xsl:attribute name="value">
            <xsl:value-of select="."/>
          </xsl:attribute>
        </input>
      </td>
      <td>
        <input type="submit" value=" Set ">
          <xsl:attribute name="name">
            <xsl:value-of select="'button_set_'"/>
            <xsl:value-of select="position()"/>
          </xsl:attribute>
          <xsl:attribute name="title">
            <xsl:value-of select="'Set motor '"/>
            <xsl:value-of select="substring('ABCDEFG',position(),1)"/>
          </xsl:attribute>
        </input>
      </td>
    </tr>
  </xsl:template>

  <xsl:template name="nxt:ButtonConfigRow">
    <xsl:param name="List" />
    <xsl:variable name="ThisRowVal" select="."/>
    <tr>
      <th class="Left" style="width:50px;">
        <xsl:value-of select="substring('RightLeft EnterExtraExtra',((position()-1)*5)+1,5)"/>
      </th>
      <td>Type</td>
      <td>
        <select size="1">
          <xsl:attribute name="name">
            <xsl:text>ButtonSensorType</xsl:text>
            <xsl:value-of select="position()"/>
          </xsl:attribute>
          <xsl:for-each select="$List/nxt:string">

            <xsl:element name="option">
              <xsl:if test="$ThisRowVal = .">
                <xsl:attribute name="selected">true</xsl:attribute>
              </xsl:if>
              <xsl:value-of select="."/>
            </xsl:element>
          </xsl:for-each>
        </select>
      </td>
    </tr>
  </xsl:template>


  <xsl:template name="nxt:MotorConfigRow">
    <xsl:param name="List" />
    <xsl:variable name="ThisRowVal" select="nxt:Type"/>
    <tr>
      <td>
        <table width="100%">
          <tr>
            <th rowspan="5" class="Left">
              <b>
                <xsl:value-of select="substring('ABCDEFG',position(),1)"/>
              </b>
            </th>
            <td>Type</td>
            <td>
              <select size="1">
                <xsl:attribute name="name">
                  <xsl:text>MotorSensorType</xsl:text>
                  <xsl:value-of select="position()"/>
                </xsl:attribute>
                <xsl:for-each select="$List/nxt:string">

                  <xsl:element name="option">
                    <xsl:if test="$ThisRowVal = .">
                      <xsl:attribute name="selected">true</xsl:attribute>
                    </xsl:if>
                    <xsl:value-of select="."/>
                  </xsl:element>
                </xsl:for-each>

              </select>
            </td>
          </tr>
          <tr>
            <td>Low Threshold</td>
            <td>
              <xsl:element name="input">
                <xsl:attribute name="type">text</xsl:attribute>
                <xsl:attribute name="name">
                  <xsl:text>MotorLowThresh</xsl:text>
                  <xsl:value-of select="position()"/>
                </xsl:attribute>
                <xsl:attribute name="size">20</xsl:attribute>
                <xsl:attribute name="value">
                  <xsl:value-of select="nxt:LowThresh"/>
                </xsl:attribute>
              </xsl:element>
            </td>
          </tr>
          <tr>
            <td>High Threshold</td>
            <td>
              <xsl:element name="input">
                <xsl:attribute name="type">text</xsl:attribute>
                <xsl:attribute name="name">
                  <xsl:text>MotorHighThresh</xsl:text>
                  <xsl:value-of select="position()"/>
                </xsl:attribute>
                <xsl:attribute name="size">20</xsl:attribute>
                <xsl:attribute name="value">
                  <xsl:value-of select="nxt:HighThresh"/>
                </xsl:attribute>
              </xsl:element>
            </td>
          </tr>
          <tr>
            <td>External Range</td>
            <td>
              <input id="MotorExternalRange" type="checkbox" title="External Range">
                <xsl:attribute name="name">
                  <xsl:text>MotorExternalRange</xsl:text>
                  <xsl:value-of select="position()"/>
                </xsl:attribute>
                <xsl:if test="nxt:ExternalRange = 'true'">
                  <xsl:attribute name="checked">CHECKED</xsl:attribute>
                  <xsl:attribute name="value">on</xsl:attribute>
                </xsl:if>
              </input>
            </td>
          </tr>
          <tr>
            <td>Ticks Per Revolution</td>
            <td>
              <xsl:element name="input">
                <xsl:attribute name="type">text</xsl:attribute>
                <xsl:attribute name="name">
                  <xsl:text>MotorTicksPerRev</xsl:text>
                  <xsl:value-of select="position()"/>
                </xsl:attribute>
                <xsl:attribute name="size">20</xsl:attribute>
                <xsl:attribute name="value">
                  <xsl:value-of select="nxt:TicksPerRevolution"/>
                </xsl:attribute>
              </xsl:element>
            </td>
          </tr>
        </table>
      </td>
    </tr>
  </xsl:template>
  
  <xsl:template name="nxt:SensorConfigRow">
    <xsl:param name="List" />
    <xsl:variable name="ThisRowVal" select="nxt:Type"/>
    <tr>
      <td>
        <table width="100%">
          <tr>
            <th rowspan="4" class="Left">
              <b>
                <xsl:value-of select="position()"/>
              </b>
            </th>
            <td>Type</td>
            <td>

              <select size="1">
                <xsl:attribute name="name">
                  <xsl:text>SensorType</xsl:text>
                  <xsl:value-of select="position()"/>
                </xsl:attribute>

                <xsl:for-each select="$List/nxt:string">

                  <xsl:element name="option">
                    <xsl:if test="$ThisRowVal = .">
                      <xsl:attribute name="selected">true</xsl:attribute>
                    </xsl:if>
                    <xsl:value-of select="."/>
                  </xsl:element>
                </xsl:for-each>

              </select>

            </td>
          </tr>



          <tr>

            <!-- To do: make thresholds hidden when input is 'Touch'
            <xsl:if test="$ThisRowVal = 'Touch'">
              <xsl:attribute name="style">
                <xsl:text>background:gray;</xsl:text>
              </xsl:attribute>
            </xsl:if>-->

            <td>Low Threshold</td>
            <td>
              <xsl:element name="input">
                <xsl:attribute name="type">text</xsl:attribute>
                <xsl:attribute name="name">
                  <xsl:text>SensorLowThresh</xsl:text>
                  <xsl:value-of select="position()"/>
                </xsl:attribute>
                <xsl:attribute name="size">20</xsl:attribute>
                <xsl:attribute name="value">
                  <xsl:value-of select="nxt:LowThresh"/>
                </xsl:attribute>
              </xsl:element>
            </td>
          </tr>
          <tr>
            <td>High Threshold</td>
            <td>
              <xsl:element name="input">
                <xsl:attribute name="type">text</xsl:attribute>
                <xsl:attribute name="name">
                  <xsl:text>SensorHighThresh</xsl:text>
                  <xsl:value-of select="position()"/>
                </xsl:attribute>
                <xsl:attribute name="size">20</xsl:attribute>
                <xsl:attribute name="value">
                  <xsl:value-of select="nxt:HighThresh"/>
                </xsl:attribute>
              </xsl:element>
            </td>
          </tr>
          <tr>
            <td>External Range</td>
            <td>
              <input id="SensorExternalRange" type="checkbox" title="External Range">
                <xsl:attribute name="name">
                  <xsl:text>SensorExternalRange</xsl:text>
                  <xsl:value-of select="position()"/>
                </xsl:attribute>
                <xsl:if test="nxt:ExternalRange = 'true'">
                  <xsl:attribute name="checked">CHECKED</xsl:attribute>
                  <xsl:attribute name="value">on</xsl:attribute>
                </xsl:if>
              </input>
            </td>
          </tr>
        </table>
      </td>
    </tr>
  </xsl:template>


  <xsl:template name="nxt:SerialPortConfig">
    <tr>
      <xsl:if test="nxt:ComPort = '0'">
        <xsl:attribute name="style">
          <xsl:text>background:#f67f7f;</xsl:text>
        </xsl:attribute>
      </xsl:if>
      <td size="10">COM Port</td>
      <td align="right" size="21">
        <input type="text" name="ComPort" size="20">
          <xsl:attribute name="value">
            <xsl:value-of select="nxt:ComPort"/>
          </xsl:attribute>
        </input>
      </td>
    </tr>
    <xsl:if test="nxt:BrickName">
      <tr>
        <td>Brick Name</td>
        <td align="right">
          <xsl:value-of select="nxt:BrickName"/>
        </td>
      </tr>
    </xsl:if>
    <tr>
      <td size="10">Status</td>
      <td size="21" align="right">
        <xsl:value-of select="nxt:Connected"/>
      </td>
    </tr>
    <xsl:if test="nxt:Connected = 'NotConnected'">
      <tr>
        <td style="color:red" align="center">NXT is not connected via Bluetooth. Please configure below.</td>
      </tr>
    </xsl:if>
  </xsl:template>



</xsl:stylesheet>
