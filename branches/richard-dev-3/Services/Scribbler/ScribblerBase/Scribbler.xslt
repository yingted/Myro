<?xml version="1.0" encoding="utf-8"?>
<!--
// Copyright (c) Microsoft Corporation.  All rights reserved.

//  This file is part of the IPRE Scribbler package
//  (http://www.roboteducation.org/)
// 
//  Ben Axelrod
//  11/10/06
-->
<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:scrb="http://www.roboteducation.org/schemas/2008/06/scribblerbase.html"
    xmlns:fn="http://www.w3.org/2005/02/xpath-functions/"
    >

  <xsl:output method="html" version="4.01" encoding="utf-8" indent="yes" 
              doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN"
              doctype-system="http://www.w3.org/TR/html4/loose.dtd" />

  <xsl:template match="/scrb:ScribblerState">
    <html>
      <head>
        <title>Scribbler Robot State</title>
        <script language="javascript">
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
        </script>
      
      <style type="text/css">
        Body
        {
	        font-family: Verdana;
	        font-size: smaller;
        	
	        background-image: url('/resources/scribbler/IPRE.ScribblerBase.ipre.png'); 
	        background-repeat: no-repeat;
	        background-position: 90% 90%; 
	        background-attachment: fixed;
        }

        Table
        {
	        border-right: black thin solid;
	        border-top: black thin solid;
	        border-left: black thin solid;
	        border-bottom: black thin solid;
        }

        TH
        {
	        text-transform: capitalize;
	        color: #951A06;
	        background-color: #CBCBCB;
	        font-variant: small-caps;
	        text-align: left;
	        font-weight: bold;
        }
     </style>
      
      </head>

      <body>
        <table width="100%" style="border:none">
          <tr>
            <td>
              <H1>
                Scribbler Robot: "<xsl:value-of select="scrb:RobotName"/>"
              </H1>
            </td>
            <td style="text-align:right">
              <a  style="font-size:large; color:Black; cursor:hand;">
                <xsl:attribute name="href">
                  <xsl:text disable-output-escaping="yes">javascript:viewXML();</xsl:text>
                </xsl:attribute>
                RAW XML <img id="DSSServiceXML" src="/resources/dss/Microsoft.Dss.Runtime.Home.Images.xml_button.gif" width="31" height="16" border="0" alt="Raw XML" />
              </a>
            </td>
          </tr>
        </table>
        
        <table width="550" style="border:none">
          <tr>
            <td>

              <form method="POST" action="" name="ConfigurationForm">
                <input type="hidden" name="Action" value="ScribblerConfig" />
                <table width="100%">
                  <tr>
                    <th colspan="2" scope="col">Configuration</th>
                  </tr>
                  <tr>
                    <td style="width:50%">Com Port</td>
                    <td>
                      <input type="text" name="ComPort" size="20">
                        <xsl:attribute name="value">
                          <xsl:value-of select="scrb:ComPort"/>
                        </xsl:attribute>
                      </input>
                    </td>
                  </tr>
                  <tr>
                    <td>Robot Name</td>
                    <td>
                      <input type="text" name="Name" size="20">
                        <xsl:attribute name="value">
                          <xsl:value-of select="scrb:RobotName"/>
                        </xsl:attribute>
                      </input>
                      &#160;&#160;&#160;<input id="Button1" type="submit" value="Change" name="buttonOk" title="Change robot's name"/>
                    </td>
                  </tr>
                  <tr>
                    <td>
                      Status:
                    </td>
                      <xsl:if test="scrb:Connected = 'false'">
                          <td style="color:red" >Not connected</td>
                      </xsl:if>
                    <xsl:if test="scrb:Connected = 'true'">
                      <td>Connected</td>
                    </xsl:if>
                  </tr>
                  <tr>
                    <td>&#160;</td>
                    <td>
                      <input id="Button2" type="reset" value="Reset" name="buttonOk" title="Reset form"/>&#160;&#160;&#160;
                      <input id="Button3" type="submit" value="Connect" name="buttonOk" title="Connect to robot"/>
                    </td>
                  </tr>
                </table>
              </form>
            </td>
          </tr>
          <tr>
            <td>
              <br />
              <form method="POST" action="" name="SensorForm">
                <input type="hidden" name="Action" value="ScribblerSensors" />
                <table width="100%">
                  <tr>
                    <th colspan="2" scope="col">Sensors</th>
                  </tr>
                  <tr>
                    <td style="width:50%">Left IR</td>
                    <td>
                      <xsl:value-of select="scrb:IRLeft"/>
                    </td>
                  </tr>
                  <tr>
                    <td>Right IR</td>
                    <td>
                      <xsl:value-of select="scrb:IRRight"/>
                    </td>
                  </tr>
                  <tr>
                    <td>Left Light</td>
                    <td>
                      <xsl:value-of select="scrb:LightLeft"/>
                    </td>
                  </tr>
                  <tr>
                    <td>Center Light</td>
                    <td>
                      <xsl:value-of select="scrb:LightCenter"/>
                    </td>
                  </tr>
                  <tr>
                    <td>Right Light</td>
                    <td>
                      <xsl:value-of select="scrb:LightRight"/>
                    </td>
                  </tr>
                  <tr>
                    <td>Left Line</td>
                    <td>
                      <xsl:value-of select="scrb:LineLeft"/>
                    </td>
                  </tr>
                  <tr>
                    <td>Right Line</td>
                    <td>
                      <xsl:value-of select="scrb:LineRight"/>
                    </td>
                  </tr>
                  <tr>
                    <td>Stall</td>
                    <td>
                      <xsl:value-of select="scrb:Stall"/>
                    </td>
                  </tr>
                  <tr>
                    <td>&#160;</td>
                    <td>
                      <input id="Button4" type="submit" value="Poll" name="buttonOk" title="Poll for lastest sensor readings"/>
                    </td>
                  </tr>
                </table>
              </form>
            </td>
          </tr>
          <tr>
            <td>
              <br />
              <form method="POST" action="" name="MotorForm">
                <input type="hidden" name="Action" value="ScribblerMotors" />
                <table width="100%">
                  <tr>
                    <th colspan="2" scope="col">Motors</th>
                  </tr>
                  <tr>
                    <td style="width:50%">Left Motor</td>
                    <td>
                      <input type="text" name="LeftMotor" size="20">
                        <xsl:attribute name="value">
                          <xsl:value-of select="scrb:MotorLeft"/>
                        </xsl:attribute>
                      </input>
                    </td>
                  </tr>
                  <tr>
                    <td>Right Motor</td>
                    <td>
                      <input type="text" name="RightMotor" size="20">
                        <xsl:attribute name="value">
                          <xsl:value-of select="scrb:MotorRight"/>
                        </xsl:attribute>
                      </input>
                    </td>
                  </tr>
                  <tr>
                    <td>&#160;</td>
                    <td>
                      <input id="Button5" type="reset" value="Reset" name="buttonOk" title="Reset form"/>&#160;&#160;&#160;
                      <input id="Button6" type="submit" value="Set" name="buttonOk" title="Set motor values"/>&#160;&#160;&#160;
                      <input id="Button65" type="submit" value="All Stop" name="buttonOk" title="Stop both motors"/>
                    </td>
                  </tr>
                </table>
              </form>
            </td>
          </tr>
          <tr>
            <td>
              <br />
              <form method="POST" action="" name="LEDForm">
                <input type="hidden" name="Action" value="ScribblerLEDs" />
                <table width="100%">
                  <tr>
                    <th colspan="2" scope="col">LEDs</th>
                  </tr>
                  <tr>
                    <td style="width:50%">Left LED</td>
                    <td>
                      <input id="LeftLED" type="checkbox" title="LeftLED" name="LeftLED">
                        <xsl:if test="scrb:LEDLeft = 'true'">
                          <xsl:attribute name="checked">CHECKED</xsl:attribute>
                          <xsl:attribute name="value">on</xsl:attribute>
                        </xsl:if>
                      </input>
                    </td>
                  </tr>
                  <tr>
                    <td>Center LED</td>
                    <td>
                      <input id="CenterLED" type="checkbox" title="CenterLED" name="CenterLED">
                        <xsl:if test="scrb:LEDCenter = 'true'">
                          <xsl:attribute name="checked">CHECKED</xsl:attribute>
                          <xsl:attribute name="value">on</xsl:attribute>
                        </xsl:if>
                      </input>
                    </td>
                  </tr>
                  <tr>
                    <td>Right LED</td>
                    <td>
                      <input id="RightLED" type="checkbox" title="RightLED" name="RightLED">
                        <xsl:if test="scrb:LEDRight = 'true'">
                          <xsl:attribute name="checked">CHECKED</xsl:attribute>
                          <xsl:attribute name="value">on</xsl:attribute>
                        </xsl:if>
                      </input>
                    </td>
                  </tr>
                  <tr>
                    <td>&#160;</td>
                    <td>
                      <input id="Button7" type="reset" value="Reset" name="buttonOk" title="Reset form"/>&#160;&#160;&#160;
                      <input id="Button8" type="submit" value="Set" name="buttonOk" title="Set LEDs"/>
                    </td>
                  </tr>
                </table>
              </form>
            </td>
          </tr>
          <tr>
            <td>
              <br />
              <form method="POST" action="" name="SpeakerForm">
                <input type="hidden" name="Action" value="ScribblerSpeaker" />
                <table width="100%">
                  <tr>
                    <th colspan="2" scope="col">Speaker</th>
                  </tr>
                  <tr>
                    <td style="width:50%">Tone 1</td>
                    <td>
                      <input type="text" name="Tone1" size="20" value="600"/> Hz
                    </td>
                  </tr>
                  <tr>
                    <td>Tone 2</td>
                    <td>
                      <input type="text" name="Tone2" size="20" value="0"/> Hz
                    </td>
                  </tr>
                  <tr>
                    <td>Duration</td>
                    <td>
                      <input type="text" name="Duration" size="20" value="500"/> ms
                    </td>
                  </tr>
                  <tr>
                    <td>&#160;</td>
                    <td>
                      <input id="Button9" type="reset" value="Reset" name="buttonOk" title="Reset form"/>&#160;&#160;&#160;
                      <input id="Button10" type="submit" value="Play" name="buttonOk" title="Play Tone"/>
                    </td>
                  </tr>
                </table>
              </form>
            </td>
          </tr>
        </table>
        <p>&#160;</p>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>