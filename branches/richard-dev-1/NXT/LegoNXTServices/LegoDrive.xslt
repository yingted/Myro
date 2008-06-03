<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"    
    xmlns:s="http://www.w3.org/2003/05/soap-envelope"    
    xmlns:drive="http://schemas.microsoft.com/robotics/2006/05/drive.html"
    xmlns:motor="http://schemas.microsoft.com/robotics/2006/05/motor.html"
    xmlns:encoder="http://schemas.microsoft.com/robotics/2006/05/encoder.html"
    xmlns:physical="http://schemas.microsoft.com/robotics/2006/07/physicalmodel.html"
    >

  <xsl:import href="/resources/dss/Microsoft.Dss.Runtime.Home.MasterPage.xslt" />

  <xsl:template match="/">
    <xsl:call-template name="MasterPage">
      <xsl:with-param name="serviceName">
        NXT Generic Drive
      </xsl:with-param>
      <xsl:with-param name="description">
        Controls the LEGO® MINDSTORMS® NXT motor configuration as a two-wheel differential drive.<br />
        (Uses the Generic Drive contract.)
      </xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="s:Header">

  </xsl:template>

  <xsl:template match="drive:DriveDifferentialTwoWheelState">
    <table width="100%">
      <tr>
        <th colspan="2">Lego Differential Drive</th>
      </tr>
      <tr>
        <td width="50%">
          <table width="100%">
            <tr>
              <th>Motor Name:</th>
              <td>
                <xsl:value-of select="drive:LeftWheel/motor:MotorState/motor:Name"/>
              </td>
            </tr>
            <tr>
              <th>Identifier:</th>
              <td>
                <xsl:value-of select="drive:LeftWheel/motor:MotorState/motor:Identifier"/>
              </td>
            </tr>
            <tr>
              <th>Wheel Speed:</th>
              <td>
                <xsl:value-of select="drive:LeftWheel/motor:WheelSpeed"/>
              </td>
            </tr>
            <tr>
              <th>Radius:</th>
              <td>
                <xsl:value-of select="drive:LeftWheel/motor:Radius"/>
              </td>
            </tr>
            <tr>
              <th>Gear Ratio:</th>
              <td>
                <xsl:value-of select="drive:LeftWheel/motor:GearRatio"/>
              </td>
            </tr>
            <tr>
              <th>Current Power:</th>
              <td>
                <xsl:value-of select="drive:LeftWheel/motor:MotorState/motor:CurrentPower"/>
              </td>
            </tr>
            <tr>
              <th>Power Scaling Factor:</th>
              <td>
                <xsl:value-of select="drive:LeftWheel/motor:MotorState/motor:PowerScalingFactor"/>
              </td>
            </tr>
            <tr>
              <th>Reverse Polarity:</th>
              <td>
                <xsl:value-of select="drive:LeftWheel/motor:MotorState/motor:ReversePolarity"/>
              </td>
            </tr>
            <tr>
              <th>Position X:</th>
              <td>
                <xsl:value-of select="drive:LeftWheel/motor:MotorState/motor:Pose/physical:Position/physical:X"/>
              </td>
            </tr>
            <tr>
              <th>Position Y:</th>
              <td>
                <xsl:value-of select="drive:LeftWheel/motor:MotorState/motor:Pose/physical:Position/physical:Y"/>
              </td>
            </tr>
            <tr>
              <th>Position Z:</th>
              <td>
                <xsl:value-of select="drive:LeftWheel/motor:MotorState/motor:Pose/physical:Position/physical:X"/>
              </td>
            </tr>
            <tr>
              <th>Orientation X:</th>
              <td>
                <xsl:value-of select="drive:LeftWheel/motor:MotorState/motor:Pose/physical:Orientation/physical:X"/>
              </td>
            </tr>
            <tr>
              <th>Orientation Y:</th>
              <td>
                <xsl:value-of select="drive:LeftWheel/motor:MotorState/motor:Pose/physical:Orientation/physical:Y"/>
              </td>
            </tr>
            <tr>
              <th>Orientation Z:</th>
              <td>
                <xsl:value-of select="drive:LeftWheel/motor:MotorState/motor:Pose/physical:Orientation/physical:X"/>
              </td>
            </tr>
            <tr>
              <th>Orientation W:</th>
              <td>
                <xsl:value-of select="drive:LeftWheel/motor:MotorState/motor:Pose/physical:Orientation/physical:W"/>
              </td>
            </tr>
          </table>
        </td>
        <td width="50%">
          <table width="100%">
            <tr>
              <th>Motor Name:</th>
              <td>
                <xsl:value-of select="drive:RightWheel/motor:MotorState/motor:Name"/>
              </td>
            </tr>
            <tr>
              <th>Identifier:</th>
              <td>
                <xsl:value-of select="drive:RightWheel/motor:MotorState/motor:Identifier"/>
              </td>
            </tr>
            <tr>
              <th>Wheel Speed:</th>
              <td>
                <xsl:value-of select="drive:RightWheel/motor:WheelSpeed"/>
              </td>
            </tr>
            <tr>
              <th>Radius:</th>
              <td>
                <xsl:value-of select="drive:RightWheel/motor:Radius"/>
              </td>
            </tr>
            <tr>
              <th>Gear Ratio:</th>
              <td>
                <xsl:value-of select="drive:RightWheel/motor:GearRatio"/>
              </td>
            </tr>
            <tr>
              <th>Current Power:</th>
              <td>
                <xsl:value-of select="drive:RightWheel/motor:MotorState/motor:CurrentPower"/>
              </td>
            </tr>
            <tr>
              <th>Power Scaling Factor:</th>
              <td>
                <xsl:value-of select="drive:RightWheel/motor:MotorState/motor:PowerScalingFactor"/>
              </td>
            </tr>
            <tr>
              <th>Reverse Polarity:</th>
              <td>
                <xsl:value-of select="drive:RightWheel/motor:MotorState/motor:ReversePolarity"/>
              </td>
            </tr>
            <tr>
              <th>Position X:</th>
              <td>
                <xsl:value-of select="drive:RightWheel/motor:MotorState/motor:Pose/physical:Position/physical:X"/>
              </td>
            </tr>
            <tr>
              <th>Position Y:</th>
              <td>
                <xsl:value-of select="drive:RightWheel/motor:MotorState/motor:Pose/physical:Position/physical:Y"/>
              </td>
            </tr>
            <tr>
              <th>Position Z:</th>
              <td>
                <xsl:value-of select="drive:RightWheel/motor:MotorState/motor:Pose/physical:Position/physical:X"/>
              </td>
            </tr>
            <tr>
              <th>Orientation X:</th>
              <td>
                <xsl:value-of select="drive:RightWheel/motor:MotorState/motor:Pose/physical:Orientation/physical:X"/>
              </td>
            </tr>
            <tr>
              <th>Orientation Y:</th>
              <td>
                <xsl:value-of select="drive:RightWheel/motor:MotorState/motor:Pose/physical:Orientation/physical:Y"/>
              </td>
            </tr>
            <tr>
              <th>Orientation Z:</th>
              <td>
                <xsl:value-of select="drive:RightWheel/motor:MotorState/motor:Pose/physical:Orientation/physical:Z"/>
              </td>
            </tr>
            <tr>
              <th>Orientation W:</th>
              <td>
                <xsl:value-of select="drive:RightWheel/motor:MotorState/motor:Pose/physical:Orientation/physical:W"/>
              </td>
            </tr>
          </table>
        </td>
      </tr>
    </table>

    <table width="100%">
      <tr>
        <th width="20%">Is Enabled:</th>
        <td width="80%">
          <xsl:value-of select="drive:IsEnabled"/>
        </td>
      </tr>
      <tr class="odd">
        <th>Distance Between Wheels:</th>
        <td>
          <xsl:value-of select="drive:DistanceBetweenWheels"/>
        </td>
      </tr>
    </table>
  </xsl:template>
</xsl:stylesheet>
