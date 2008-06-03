using System;
using System.Collections.Generic;
using System.Text;

namespace SharpLogic.Robotics.Surveyor.Srv1
{
    public enum ImageResolutions : byte
    {
        Resolution80x64,
        Resolution160x128,
        Resolution320x240,
    }

    /// <summary>
    /// Specifies the constant values based on the definition of communication protocol between host and SRV-1.
    /// Command values defined in this enum conform to Surveyor SRV-1 firmware version 101406.
    /// </summary>
    internal enum SerialCommands : byte
    {
        MotorControl = (byte)'M',
        RobotDriftLeft = (byte)'7',
        RobotDriveForward = (byte)'8',
        RobotDriftRight = (byte)'9',
        RobotDriveLeft = (byte)'4',
        RobotStop = (byte)'5',
        RobotDriveRight = (byte)'6',
        RobotBackLeft = (byte)'1',
        RobotDriveBack = (byte)'2',
        RobotBackRight = (byte)'3',
        RobotRotateLeft_20deg = (byte)'0',
        RobotRotateRight_20deg = (byte)'.',
        HighMotorSpeedRange = (byte)'+',
        LowMotorSpeedRange = (byte)'-',
        SetCaptureResolutionTo_80x64 = (byte)'a',
        SetCaptureResolutionTo_160x128 = (byte)'b',
        SetCaptureResolutionTo_320x240 = (byte)'c',
        /// <summary>
        /// Turn on "Failsafe Mode" (stop motors if no radio contact).
        /// </summary>
        FailSafeModeOn = (byte)'f',
        /// <summary>
        /// Turn off "Failsafe Mode".
        /// </summary>
        FailSafeModeOff = (byte)'F',
        /// <summary>
        /// Turn on "Wander Mode" in 160x128 resolution.
        /// </summary>
        WanderModeOn = (byte)'w',
        /// <summary>
        /// Turn off "Wander Mode".
        /// </summary>
        WanderModeOff = (byte)'W',
        /// <summary>
        /// Turn on "Wander Mode" without autonomous movement (useful for analyzing Scan data).
        /// </summary>
        WanderModeScanOn = (byte)'m',
        /// <summary>
        /// View raw "Pixel Column Vector" data in "Wander Mode".
        /// </summary>
        Scan = (byte)'S',
        GrabCompressedVideoFrame = (byte)'I',
        ReadFirmwareVersionInfo = (byte)'V',
        /// <summary>
        /// Send 0xFF bit pattern from each IR emitter in sequence.
        /// </summary>
        BounceInfraRed = (byte)'B',
        XmitIRBeaconModeOn = (byte)'x',
        XmitIRBeaconModeOff = (byte)'X',
        LocateBeaconModeOn = (byte)'l',
        LocateBeaconModeOff = (byte)'L',

        // Swarm Mode
        ReadRobotId = (byte)'r',
        RenewRobotId = (byte)'R',
        SwarmModeCommand = (byte)'@'
    }

}
