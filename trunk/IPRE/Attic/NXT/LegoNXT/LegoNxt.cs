//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
//
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: LegoNxt.cs $ $Revision$
//-----------------------------------------------------------------------

using Microsoft.Ccr.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.Core;
using Microsoft.Dss.Core.DsspHttp;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Dss.Core.DsspHttpUtilities;
using Microsoft.Dss.ServiceModel.DsspServiceBase;
using Microsoft.Robotics.Services.LegoNxt.Helper;
using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.IO.Ports;
using System.Net;
using W3C.Soap;

using permissions = System.Security.Permissions;
using svcbase = Microsoft.Dss.ServiceModel.DsspServiceBase;
using dssp = Microsoft.Dss.ServiceModel.Dssp;
using submgr = Microsoft.Dss.Services.SubscriptionManager;
using System.IO;


namespace Microsoft.Robotics.Services.LegoNxt
{
    [Contract(Contract.Identifier)]
    [DisplayName("LEGO® NXT Brick")]
    [Description("Provides access to the LEGO® MINDSTORMS® NXT Brick general service.")]
    public class LegoNxtService : svcbase.DsspServiceBase
    {
        private const string _legoStateFileName = "LegoNxt.Config.xml";
        private const string _transform = ServicePaths.Transforms + "/LegoNxt.xslt";

        private static int _keepAliveTime = 60000;

        private bool _sendConfig = false;
        private bool _serialPortOpened = false;
        private bool _ackReceived = false;

        [InitialStatePartner(Optional = true, ServiceUri = _legoStateFileName)]
        private LegoNxtState _state = null;

        // Subscription manager partner
        [Partner(dssp.Partners.SubscriptionManagerString, Contract = submgr.Contract.Identifier, CreationPolicy = PartnerCreationPolicy.CreateAlways)]
        submgr.SubscriptionManagerPort _subMgrPort = new submgr.SubscriptionManagerPort();

        private Port<DateTime> _keepAlivePort = new Port<DateTime>();
        DsspHttpUtilitiesPort _httpUtilities = new DsspHttpUtilitiesPort();
        private UpdateStatePort _internalStatePort = new UpdateStatePort();

        // Internal port for handling Lego commands which do not receive an acknowledgement.
        private Port<LegoCommand> _legoCommNoAckPort = new Port<LegoCommand>();
        private Port<SendLegoCommand> _legoCommWithAckPort = new Port<SendLegoCommand>();

        [ServicePort("/LegoNxt", AllowMultipleInstances = true)]
        private LegoNxtOperations _mainPort = new LegoNxtOperations();

        private LegoConnection _legoBlock;


        public LegoNxtService(dssp.DsspServiceCreationPort creationPort) : 
            base(creationPort)
        {
        }

        protected override void Start()
        {
            // Listen on the main port for requests and call the appropriate handler.
            Interleave mainInterleave = ActivateDsspOperationHandlers();

            // Set up an interleave for updating our internal state changes.
            Activate(new Interleave(
                new ExclusiveReceiverGroup(
                    Arbiter.ReceiveWithIterator<SensorNotification>(true, _internalStatePort, SensorNotificationHandler),
                    Arbiter.Receive<UpdateStatus>(true, _internalStatePort, UpdateStatusHandler),
                    Arbiter.Receive<UpdateResponse>(true, _internalStatePort, UpdateStateForLegoResponseHandler),
                    Arbiter.Receive<ReplaceState>(true, _internalStatePort, ReplaceStateHandler)
                ),
                new ConcurrentReceiverGroup()));

            //needed for HttpPost
            _httpUtilities = DsspHttpUtilitiesService.Create(Environment);

            // Insert ourselves into the directory so that others can find us
            DirectoryInsert();

            _legoBlock = new LegoConnection(this);
            InitializeState();

            if (ValidSerialPort(_state))
            {
                //open NXT port
                if (_legoBlock.Open(_state.ComPort, _state.BaudRate))
                {
                    _serialPortOpened = true;

                    // Listen for a single direct Serial port request
                    Activate(Arbiter.Receive(false, _legoCommNoAckPort, LegoCommNoAckHandler));

                    // Listen for a single Serial port request with an acknowledgement
                    Activate(Arbiter.ReceiveWithIterator<SendLegoCommand>(false, _legoCommWithAckPort, LegoCommWithAckHandler));

                    //send config file to brick and start program
                    Port<bool> donePort = new Port<bool>();
                    SpawnIterator<byte[], Port<bool>>(ParseConfig(_state.BrickConfig), donePort, ConfigureNXT);
                    Activate(Arbiter.Receive(false, donePort,
                        delegate(bool success)
                        {
                            if (success)
                            {
                                LogInfo("LEGO NXT Configuration succeeded: " + _state.Connected.ToString());
                            }
                            else
                            {
                                LogError("LEGO NXT Configuration failed");
                                OpenLegoServiceInBrowser();
                            }
                        }));
                }
            }

            if (!_serialPortOpened)
            {
                //send entire config file (error prone)
                _sendConfig = true;
                OpenLegoServiceInBrowser();
            }

        }

        #region Internal Logging

        /// <summary>
        /// Log an informational message to the console.
        /// </summary>
        /// <param name="message"></param>
        internal void LogConsoleInfo(string message)
        {
            LogInfo(LogGroups.Console, message);
        }

        /// <summary>
        /// Log a warning message to the console.
        /// </summary>
        /// <param name="message"></param>
        internal void LogConsoleWarning(string message)
        {
            LogWarning(LogGroups.Console, message);
        }

        /// <summary>
        /// Log an error message to the console.
        /// </summary>
        /// <param name="message"></param>
        internal void LogConsoleError(string message)
        {
            LogError(LogGroups.Console, message);
        }

        #endregion

        /// <summary>
        /// Open the LEGO NXT Service in a web browser.
        /// </summary>
        private void OpenLegoServiceInBrowser()
        {
            //start up IE to our state page so user can configure
            System.Diagnostics.Process process = new System.Diagnostics.Process();
            process.StartInfo.FileName = FindServiceAliasFromScheme(Uri.UriSchemeHttp);
            process.Start();
        }


        /// <summary>
        /// Update the current connection status
        /// <remarks>Internal Exclusive command</remarks>
        /// </summary>
        /// <param name="updateStatus"></param>
        private void UpdateStatusHandler(UpdateStatus updateStatus)
        {
            // Has the status changed?
            if (_state.Connected != updateStatus.Body.Status)
            {
                _state.Connected = updateStatus.Body.Status;

                //notify subscribers
                SendNotification<Configure>(_subMgrPort, _state);
                SendNotification<UpdateStatus>(_subMgrPort, new UpdateStatus(_state.Connected));
            }
            updateStatus.ResponsePort.Post(DefaultUpdateResponseType.Instance);
        }

        /// <summary>
        /// Replace the LEGO NXT Service configuration state
        /// </summary>
        /// <param name="replaceState"></param>
        private void ReplaceStateHandler(ReplaceState replaceState)
        {
            bool statusChanged = (_state.Connected != replaceState.Body.Connected);
            _state = replaceState.Body;
            replaceState.ResponsePort.Post(DefaultReplaceResponseType.Instance);
            if (statusChanged)
            {
                SendNotification<UpdateStatus>(_subMgrPort, new UpdateStatus(replaceState.Body.Connected));
            }
        }

        /// <summary>
        /// Send direct commands to the LegoNXT sequentially (Exclusive)
        /// without waiting for an ack.
        /// </summary>
        /// <param name="legoCommand"></param>
        private void LegoCommNoAckHandler(LegoCommand legoCommand)
        {
            _legoBlock.SendCommand(legoCommand);

            // Listen for the next command request
            Activate(Arbiter.Receive(false, _legoCommNoAckPort, LegoCommNoAckHandler));
        }

        /// <summary>
        /// Send a direct command to the LEGO NXT and wait for a response.
        /// </summary>
        /// <param name="ready"></param>
        /// <param name="legoCommand"></param>
        /// <returns></returns>
        private IEnumerator<ITask> LegoCommWithAckHandler(SendLegoCommand legoCommand)
        {
            // Send the command to the Lego
            LegoResponse validResponse = _legoBlock.SendCommand(legoCommand.Body);
            if (validResponse == null)
            {
                legoCommand.ResponsePort.Post(
                    Fault.FromCodeSubcodeReason(FaultCodes.Receiver,
                    DsspFaultCodes.OperationFailed,
                    "No response from LEGO command: " + legoCommand.Body.LegoCommandCode.ToString()));
            }
            else if (validResponse.GetType() == typeof(LegoResponseException))
            {
                // Pull exception text from response
                string errorMessage = "LEGO command: " + legoCommand.Body.LegoCommandCode.ToString() + " response generated an error: " + ((LegoResponseException)validResponse).ErrorMessage;
                legoCommand.ResponsePort.Post(
                    Fault.FromCodeSubcodeReason(FaultCodes.Receiver,
                    DsspFaultCodes.OperationFailed,
                    errorMessage));
            }
            else
            {
                // Check to see if we need to update state
                // based on the response received from LEGO.
                PortSet<DefaultUpdateResponseType, Fault> responsePort = UpdateCurrentState(validResponse);
                if (responsePort != null)
                {
                    yield return Arbiter.Choice(responsePort,
                        delegate(DefaultUpdateResponseType response) { },
                        delegate(Fault fault) 
                        {
                            LogError(LogGroups.Console, "Failed to update LEGO NXT service state", fault);
                        });
                }

                PostCommandProcessing(validResponse);

                legoCommand.ResponsePort.Post(TranslateLegoReturnStatus(validResponse));
            }

            // Ready to process another command
            Activate(Arbiter.ReceiveWithIterator<SendLegoCommand>(false, _legoCommWithAckPort, LegoCommWithAckHandler));
            yield break;
        }

        private void PostCommandProcessing(LegoResponse validResponse)
        {
            if (validResponse.LegoCommandCode == LegoHelper.LegoCommandCode.StopProgram)
            {
                if (validResponse.ErrorCode == LegoErrorCode.Success)
                {
                    // Stopping a program takes time.
                    // Pause for 1.5 seconds before sending the response.
                    System.Threading.Thread.Sleep(1500);
                }
            }
        }

        /// <summary>
        /// Check LEGO response to see if we need to update our state.
        /// Post all updates to an internal exclusive state handler.
        /// </summary>
        /// <param name="response"></param>
        private PortSet<DefaultUpdateResponseType, Fault> UpdateCurrentState(LegoResponse legoResponse)
        {
            // Update service state
            switch (legoResponse.LegoCommandCode)
            {
                case LegoHelper.LegoCommandCode.SetOutputState:
                case LegoHelper.LegoCommandCode.GetInputValues:
                case LegoHelper.LegoCommandCode.GetBatteryLevel:
                case LegoHelper.LegoCommandCode.ReadIOMap:
                case LegoHelper.LegoCommandCode.GetDeviceInfo:
                    UpdateResponse updateResponse = new UpdateResponse(legoResponse);
                    _internalStatePort.Post(updateResponse);
                    return updateResponse.ResponsePort;
            }
            return null;
        }

        /// <summary>
        /// Exclusive internal handler for updating state based on Lego Responses.
        /// </summary>
        /// <param name="legoResponse"></param>
        private void UpdateStateForLegoResponseHandler(UpdateResponse updateResponse)
        {
            LegoResponse legoResponse = updateResponse.Body;

            // Update service state
            switch (legoResponse.LegoCommandCode)
            {
                case LegoHelper.LegoCommandCode.SetOutputState:
                    UpdateMotors(new LegoSetOutputState(legoResponse.Data));
                    break;
                
                case LegoHelper.LegoCommandCode.GetInputValues:
                    UpdateSensors(new LegoResponseGetInputValues(legoResponse));
                    break;

                case LegoHelper.LegoCommandCode.ReadIOMap:
                    if (LegoResponseGetButtonState.IsValidButtonStateResponse(legoResponse))
                        UpdateButton(new LegoResponseGetButtonState(legoResponse));
                    break;
                
                case LegoHelper.LegoCommandCode.GetBatteryLevel:
                    UpdateBattery(new LegoResponseGetBatteryLevel(legoResponse));
                    break;
                
                case LegoHelper.LegoCommandCode.GetDeviceInfo:
                    UpdateDeviceInfo(new LegoResponseGetDeviceInfo(legoResponse));
                    break;

            }

            updateResponse.ResponsePort.Post(DefaultUpdateResponseType.Instance);
        }

        private void UpdateDeviceInfo(LegoResponseGetDeviceInfo legoResponseGetDeviceInfo)
        {
            _state.BrickName = legoResponseGetDeviceInfo.BrickName;
        }

        /// <summary>
        /// Send direct commands to the LegoNXT
        /// and returns a ResponsePort which will always contain
        /// a response.
        /// </summary>
        /// <param name="legoCommand"></param>
        private PortSet<LegoResponse, Fault> SendLegoDirectCommand(LegoCommand legoCommand)
        {
            SendLegoCommand sendLegoCommand = new SendLegoCommand(legoCommand);
            _legoCommWithAckPort.Post(sendLegoCommand);
            return sendLegoCommand.ResponsePort;
        }

        /// <summary>
        /// Initialize state
        /// </summary>
        private void InitializeState()
        {
            bool saveState = false;

            //initial state configuration
            if (_state == null)
            {
                _state = new LegoNxtState();
                _state.ComPort = 0;
                _state.BaudRate = 115200;

                //configure sensors
                _state.BrickConfig = new LegoBrickConfig();

                //sensor port 1 is a touch sensor
                //we will be notified on any change
                _state.BrickConfig.SensorPort[0].Type = SensorDefinition.SensorType.Touch;

                //sensor port 2 is a light sensor
                //we will be notified when the light reading is between 0 and 30
                _state.BrickConfig.SensorPort[1].Type = SensorDefinition.SensorType.LightOn;
                _state.BrickConfig.SensorPort[1].LowThresh = 0;
                _state.BrickConfig.SensorPort[1].HighThresh = 30;

                //sensor port 3 is a sound sensor
                //we will be notified when the sound reading is less than 20 or greater than 90
                _state.BrickConfig.SensorPort[2].Type = SensorDefinition.SensorType.Sound;
                _state.BrickConfig.SensorPort[2].LowThresh = 20;
                _state.BrickConfig.SensorPort[2].HighThresh = 90;
                _state.BrickConfig.SensorPort[2].ExternalRange = true;

                //sensor port 4 is a ultrasonic range finder sensor
                //we will be notified when the distance reading is in between 0 and 250 (the full range)
                _state.BrickConfig.SensorPort[3].Type = SensorDefinition.SensorType.Sonar;
                _state.BrickConfig.SensorPort[3].LowThresh = 0;
                _state.BrickConfig.SensorPort[3].HighThresh = 250;

                //motor ports A and C are configured as an encoders
                //the default "ticks per revolution" is used (6)
                //we will be notified every 60 degrees
                _state.BrickConfig.MotorPort[0].Type = "Encoder";
                _state.BrickConfig.MotorPort[2].Type = "Encoder";

                //motor port B is configured as an angle sensor
                //the low and high thresholds are default to 0, 
                //which causes us to be notified on any sensor change
                //NOTE: low and high thresholds can be set same as the other sensors above
                _state.BrickConfig.MotorPort[1].Type = "Angle";

                //listen for the center button
                _state.BrickConfig.ButtonPort[2] = "Button";

                saveState = true;
            }

            if (_state.BaudRate < 300)
            {
                _state.BaudRate = 115200;
                saveState = true;
            }

            if (_state.Connected != LegoConnectionStatus.NotConnected)
            {
                _state.Connected = LegoConnectionStatus.NotConnected;
                saveState = true;
            }

            if (saveState)
                SaveState(_state);

        }

        #region NXT Configuration

        /// <summary>
        /// Check the LEGO NXT for the specified file and
        /// optionally upload the file to the Brick.
        /// </summary>
        /// <param name="resourcePath"></param>
        /// <param name="fileName"></param>
        /// <param name="donePort"></param>
        /// <returns>posts (bool)success to donePort</returns>
        IEnumerator<ITask> LoadNxtFileFromResource(string resourcePath, string fileName, Port<bool> donePort)
        {
            int trial = 0;
            bool completed = false;
            bool success = false;
            bool fileOnBrick = false;
            PortSet<LegoResponse, Fault> responsePort = null;

            trial = 0;
            completed = false;
            while (trial < 5 && !completed)
            {
                responsePort = SendLegoDirectCommand(new LegoFindFirst(fileName));
                yield return Arbiter.Choice(
                    Arbiter.Receive<LegoResponse>(false, responsePort,
                        delegate(LegoResponse response)
                        {
                            if (response.ErrorCode == LegoErrorCode.Success)
                            {
                                completed = true;
                                fileOnBrick = true;
                            }
                            else if (response.ErrorCode == LegoErrorCode.FileNotFound)
                            {
                                completed = true;
                                fileOnBrick = false;
                            }
                            else
                                LogError(LogGroups.Console, "Error Validating " + fileName + " file on NXT: " + response.ErrorCode.ToString());
                        }),
                    Arbiter.Receive<Fault>(false, responsePort,
                        delegate(Fault fault)
                        {
                            LogError(LogGroups.Console, "Timed out validating " + fileName + " file on NXT.");
                        }));

                trial++;
            }

            if (!completed || fileOnBrick)
            {
                donePort.Post(fileOnBrick);
                yield break;
            }

            byte[] fileData = LoadFileResource(resourcePath + fileName);
            if (fileData != null)
            {
                int pgmLength = fileData.Length;
                int handle = -1;
                LogInfo(LogGroups.Console, "Downloading " + fileName + " to LEGO NXT.");

                completed = false;
                trial = 0;
                while (trial < 5 && !completed)
                {
                    LegoOpenWriteLinear openWrite = new LegoOpenWriteLinear(fileName, pgmLength);
                    responsePort = SendLegoDirectCommand(openWrite);
                    yield return Arbiter.Choice(
                        Arbiter.Receive<LegoResponse>(false, responsePort,
                            delegate(LegoResponse response)
                            {
                                LegoResponseOpenWriteLinear responseOpenWriteLinear = new LegoResponseOpenWriteLinear(response);
                                if (response.ErrorCode == LegoErrorCode.Success || response.ErrorCode == LegoErrorCode.FileExists)
                                {
                                    handle = responseOpenWriteLinear.Handle;
                                    completed = true;
                                }
                                else if (response.ErrorCode == LegoErrorCode.NoSpace)
                                {
                                    LogError(LogGroups.Console, "Out of space on LEGO NXT Brick.\nPlease remove one or more LEGO NXT programs on the NXT Brick\nthen restart this service.\n");
                                    trial = 999;
                                }
                                else
                                {
                                    LogError(LogGroups.Console, "Error preparing to upload " + fileName + " file to the LEGO NXT: " + response.ErrorCode.ToString());
                                }
                            }),
                        Arbiter.Receive<Fault>(false, responsePort,
                            delegate(Fault fault)
                            {
                                LogError(LogGroups.Console, "Timed out while uploading " + fileName + " file.");
                            }));

                    trial++;
                }

                if (!completed)
                {
                    donePort.Post(false);
                    yield break;
                }

                completed = false;
                trial = 0;
                while (trial < 5 && !completed)
                {


                    LegoWrite legoWrite = new LegoWrite(handle, fileData);
                    responsePort = SendLegoDirectCommand(legoWrite);
                    yield return Arbiter.Choice(
                        Arbiter.Receive<LegoResponse>(false, responsePort,
                            delegate(LegoResponse response)
                            {
                                LegoResponseWrite responseWrite = new LegoResponseWrite(response);
                                if (response.ErrorCode == LegoErrorCode.Success)
                                {
                                    if (pgmLength != responseWrite.BytesWritten)
                                        LogWarning(LogGroups.Console, "Warning: " + fileName + " file length on LEGO NXT does not match the PC.");
                                    completed = true;
                                }
                                else
                                {
                                    LogError(LogGroups.Console, "Error sending " + fileName + " file to the LEGO NXT: " + response.ErrorCode.ToString());
                                }
                            }),
                        Arbiter.Receive<Fault>(false, responsePort,
                            delegate(Fault fault)
                            {
                                LogError(LogGroups.Console, "Timed out sending " + fileName + " file to the LEGO NXT");
                            }));

                    trial++;
                }

                if (!completed)
                {
                    donePort.Post(false);
                    yield break;
                }


                // Now Close the Write Buffer.
                completed = false;

                LegoClose legoClose = new LegoClose(handle);
                responsePort = SendLegoDirectCommand(legoClose);
                yield return Arbiter.Choice(
                    Arbiter.Receive<LegoResponse>(false, responsePort,
                        delegate(LegoResponse response)
                        {
                            if (response.ErrorCode == LegoErrorCode.Success)
                            {
                                success = true;
                                completed = true;
                            }
                            else
                            {
                                LogError(LogGroups.Console, "Error in LEGO initialization step 6 while closing MSRS program upload on the LEGO NXT: " + response.ErrorCode.ToString());
                            }
                        }),
                    Arbiter.Receive<Fault>(false, responsePort,
                        delegate(Fault fault)
                        {
                            LogError(LogGroups.Console, "Timed out closing MSRS program upload during LEGO initialization step 6.");
                        }));

            }

            donePort.Post(success);
            yield break;
        }

        IEnumerator<ITask> ConfigureNXT(byte[] configureData, Port<bool> donePort)
        {
            string resourcePath = "Microsoft.Robotics.Services.LegoNxt.Resources.";
            string mainFile = "msrs006.rxe";
            string[] OldFilesList = { "main.rxe", "msrs.rxe" };
            string hourglassFile = "Timeglass.ric";
            string logoFile = "msrs.ric";

            string configFile = "config.txt";
            bool writesuccess = false;
            bool startsuccess = false;
            PortSet<LegoResponse, Fault> responsePort = null;

            int trial = 0;
            bool downloadProgram = false;
            bool completed = false;

            LogInfo("Connecting to NXT...");

            // Get the NXT Device Info
            yield return Arbiter.Choice(SendLegoDirectCommand(new LegoGetDeviceInfo()),
                delegate(LegoResponse response) 
                {
                    if (response.ErrorCode == LegoErrorCode.Success)
                    {
                        LogInfo(LogGroups.Console, string.Format("Connected to LEGO NXT: {0}\r\n", _state.BrickName));
                        completed = true;
                    }
                    else
                    {
                        LogError(LogGroups.Console, "Error getting LEGO Device Information " + response.ErrorCode.ToString());
                    }
                },
                delegate(Fault fault) 
                {
                    LogError(fault);
                });

            if (!completed)
            {
                LegoConnection.ShowLegoHelp(this);
                if (donePort != null) donePort.Post(false);
                yield break;
            }

            #region Stop any running program


            completed = false;
            // Stop any program that is running
            yield return Arbiter.Choice(StopLegoProgram(),
                delegate(LegoResponse response) 
                {
                    
                    if (response.ErrorCode != LegoErrorCode.NoActiveProgram
                        && response.ErrorCode != LegoErrorCode.Success)
                    {
                        completed = true;
                        LogError("Error stopping active LEGO program " + response.ErrorCode.ToString());
                    }
                },
                delegate(Fault fault) 
                {
                    LogError(LogGroups.Console, "Error stopping active LEGO program", fault);
                });

            #endregion

            #region See if the LEGO MSRS program exists

            trial = 0;
            completed = false;
            while (trial < 5 && !completed)
            {
                responsePort = SendLegoDirectCommand(new LegoFindFirst(mainFile));
                yield return Arbiter.Choice(
                    Arbiter.Receive<LegoResponse>(false, responsePort,
                        delegate(LegoResponse response)
                        {
                            if (response.ErrorCode == LegoErrorCode.Success)
                            {
                                completed = true;
                            }
                            else if (response.ErrorCode == LegoErrorCode.FileNotFound)
                            {
                                completed = true;
                                downloadProgram = true;
                            }
                            else
                                LogError(LogGroups.Console, response.ErrorCode.ToString());
                        }),
                    Arbiter.Receive<Fault>(false, responsePort, 
                        delegate(Fault fault)
                        {
                            LogError(LogGroups.Console, "Timed out during LEGO initialization step 1.\nIf you have not done so, please establish a Bluetooth\nconnection with the LEGO NXT and restart this service.\n");
                        }));

                trial++;
            }

            if (!completed)
            {
                if (donePort != null) donePort.Post(false);
                yield break;
            }

            #endregion

            #region Download MSRS program files to LEGO NXT
            if (downloadProgram)
            {
                Port<bool> doneRemoveFile = new Port<bool>();
                foreach (string removeFile in OldFilesList)
                {
                    SpawnIterator<string, Port<bool>>(removeFile, doneRemoveFile, RemoveOldFile);
                    yield return Arbiter.Receive<bool>(false, doneRemoveFile,
                        delegate(bool removed)
                        {
                        });
                }

                Port<bool> resourceDonePort = new Port<bool>();
                SpawnIterator<string, string, Port<bool>>(resourcePath, hourglassFile, resourceDonePort, LoadNxtFileFromResource);
                yield return Arbiter.Receive(false, resourceDonePort,
                    delegate(bool success)
                    {
                        completed = success;
                    });

                if (!completed)
                {
                    if (donePort != null) donePort.Post(false);
                    yield break;
                }

                SpawnIterator<string, string, Port<bool>>(resourcePath, logoFile, resourceDonePort, LoadNxtFileFromResource);
                yield return Arbiter.Receive(false, resourceDonePort,
                    delegate(bool success)
                    {
                        completed = success;
                    });

                if (!completed)
                {
                    if (donePort != null) donePort.Post(false);
                    yield break;
                }

                SpawnIterator<string, string, Port<bool>>(resourcePath, mainFile, resourceDonePort, LoadNxtFileFromResource);
                yield return Arbiter.Receive(false, resourceDonePort,
                    delegate(bool success)
                    {
                        completed = success;
                    });

                if (!completed)
                {
                    if (donePort != null) donePort.Post(false);
                    yield break;
                }

            }
            #endregion

            // -----------------------------------------------------------------------------------------------------------
            // Now the program exists.  See if it is started.
            // -----------------------------------------------------------------------------------------------------------

            #region See if the LEGO is running our MSRS program
            completed = false;
            bool stopProgram = false;

            LegoGetCurrentProgramName legoGetCurrentProgramName = new LegoGetCurrentProgramName();
            responsePort = SendLegoDirectCommand(legoGetCurrentProgramName);
            yield return Arbiter.Choice(
                Arbiter.Receive<LegoResponse>(false, responsePort,
                    delegate(LegoResponse response)
                    {
                        if (response.ErrorCode == LegoErrorCode.Success)
                        {
                            completed = true;
                            LegoResponseGetCurrentProgramName gcpnResponse = new LegoResponseGetCurrentProgramName(response);
                            if (gcpnResponse.FileName != mainFile)
                            {
                                if (!string.IsNullOrEmpty(gcpnResponse.FileName))
                                    LogError(LogGroups.Console, "The wrong program is running: " + gcpnResponse.FileName);

                                stopProgram = true;
                            }
                        }
                        else if (response.ErrorCode == LegoErrorCode.NoActiveProgram)
                        {
                            completed = true;
                        }
                        else
                        {
                            LogError(LogGroups.Console, "Error looking for a running program in LEGO initialization step 7: " + response.ErrorCode.ToString());
                        }
                    }),
                Arbiter.Receive<Fault>(false, responsePort,
                    delegate(Fault fault)
                    {
                        LogError(LogGroups.Console, "Timed out waiting for the MSRS program to start during LEGO initialization step 7.");
                    }));

            if (stopProgram)
            {
                yield return Arbiter.Choice(StopLegoProgram(),
                    delegate(LegoResponse response) { },
                    delegate(Fault fault) { });

                // Wait for the program to stop.
                yield return Arbiter.Receive(false, TimeoutPort(2000), delegate(DateTime t) { });
            }

            if (!completed)
            {
                if (donePort != null) donePort.Post(false);
                yield break;
            }

            #endregion

            // -----------------------------------------------------------------------------------------------------------
            // Do we need to start the program?
            // -----------------------------------------------------------------------------------------------------------

            #region Start the LEGO MSRS Program
            if (true)
            {
                LogInfo(LogGroups.Console, "Starting MSRS program on LEGO NXT...");
                completed = false;

                LegoStartProgram legoStartProgram = new LegoStartProgram(mainFile);
                legoStartProgram.RequireResponse = true;
                trial = 0;
                while (!completed && trial < 5)
                {
                    responsePort = SendLegoDirectCommand(legoStartProgram);
                    yield return Arbiter.Choice(
                        Arbiter.Receive<LegoResponse>(false, responsePort,
                            delegate(LegoResponse response)
                            {
                                if (response.ErrorCode == LegoErrorCode.Success)
                                {
                                    completed = true;
                                }

                            }),
                        Arbiter.Receive<Fault>(false, responsePort,
                            delegate(Fault fault)
                            {
                                LogError(LogGroups.Console, "Timed out starting MSRS program during LEGO initialization step 8.");
                            }));

                    trial++;
                }
                if (!completed)
                {
                    LogError(LogGroups.Console, "Error starting MSRS program during LEGO initialization step 8.");
                    if (donePort != null) donePort.Post(false);
                    yield break;
                }
                else
                {
                    // Wait 1 sec for program to fully start
                    yield return Arbiter.Receive(false, TimeoutPort(1000), delegate(DateTime t) { });
                }
            }
            #endregion

            // -----------------------------------------------------------------------------------------------------------

            #region Send Configuration File to the LEGO

            LogInfo(LogGroups.Console, "Sending Configuration file");
            if (_sendConfig)
            {
                trial = 0;
                while (trial < 5 && writesuccess == false)
                {
                    bool restart = false;
                    bool fileexists = false;
                    bool opensuccess = false;
                    int handle = 0;

                    //see if file exists
                    string cfile = @"C:\Microsoft Robotics Studio (1.5)\samples\Platforms\LEGO\NXT\Resources\" + configFile;
                    responsePort = SendLegoDirectCommand(new LegoFindFirst(cfile));
                    yield return Arbiter.Choice(
                        Arbiter.Receive<LegoResponse>(false, responsePort,
                            delegate(LegoResponse legoRsp)
                            {
                                LegoResponseFindFirst response = new LegoResponseFindFirst(legoRsp);
                                if (response.Status != 0)
                                {
                                    LogInfo(LogGroups.Console, "LEGO NXT FindFirst Status Error in LEGO initialization step 9: " + response.ErrorCode.ToString());
                                    LogError("LEGO NXT FindFirst Status Error in LEGO initialization step 9: " + response.ErrorCode.ToString());
                                    if (response.ErrorCode != LegoErrorCode.FileNotFound)
                                        restart = true;
                                }
                                else
                                {
                                    LogInfo(LogGroups.Console, "LEGO NXT \"" + response.FileName + "\" found");
                                    LogInfo("LEGO NXT \"" + response.FileName + "\" found");
                                    LogVerbose("Found");
                                    LogVerbose("  FileName: ");
                                    LogVerbose("  File size: " + response.FileSize);
                                    LogVerbose("  Handle: " + response.Handle);
                                    if (response.FileSize > 0)
                                    {
                                        handle = response.Handle;
                                        fileexists = true;
                                    }
                                    else
                                        restart = true;
                                }
                            }),
                        Arbiter.Receive<Fault>(false, responsePort,
                            delegate(Fault fault)
                            {
                                LogError(LogGroups.Console, "Timed out waiting for response from NXT during LEGO initialization step 9.");
                            })
                    );

                    //wait 10 ms
                    yield return Arbiter.Receive(false, TimeoutPort(10), delegate(DateTime t) { });

                    if (restart)
                    {
                        //wait 1 sec
                        yield return Arbiter.Receive(false, TimeoutPort(1000), delegate(DateTime t) { });
                        continue;
                    }

                    //close and delete file if nessisary
                    if (fileexists)
                    {
                        //wait 10 ms
                        yield return Arbiter.Receive(false, TimeoutPort(10), delegate(DateTime t) { });

                        //close
                        LegoClose close = new LegoClose(handle);
                        close.RequireResponse = true;
                        responsePort = SendLegoDirectCommand(close);
                        yield return Arbiter.Choice(
                            Arbiter.Receive<LegoResponse>(false, responsePort,
                                delegate(LegoResponse legoRsp)
                                {
                                    LegoResponseClose response = new LegoResponseClose(legoRsp);
                                    if (response.Status != 0)
                                    {
                                        LogError("LEGO NXT Error in initialization step 10: " + response.ErrorCode.ToString());
                                    }
                                    else
                                    {
                                        LogInfo("LEGO NXT Close Success");
                                        LogVerbose("LEGO NXT Close Success Handle: " + response.Handle);
                                    }
                                }),
                            Arbiter.Receive<Fault>(false, responsePort,
                                delegate(Fault fault)
                                {
                                    LogError(LogGroups.Console, "Timed out waiting for response from NXT during LEGO initialization step 10.");
                                }));

                        //wait 10 ms
                        yield return Arbiter.Receive(false, TimeoutPort(10), delegate(DateTime t) { });

                        //delete
                        LegoDelete delete = new LegoDelete(configFile);
                        delete.RequireResponse = true;
                        responsePort = SendLegoDirectCommand(delete);
                        yield return Arbiter.Choice(
                            Arbiter.Receive<LegoResponse>(false, responsePort,
                                delegate(LegoResponse legoRsp)
                                {
                                    LegoResponseDelete response = new LegoResponseDelete(legoRsp);
                                    if (response.Status != 0)
                                    {
                                        LogError("LEGO NXT Delete Error in initialization step 11: " + response.ErrorCode.ToString());
                                    }
                                    else
                                    {
                                        LogInfo("LEGO NXT Delete \"" + response.FileName + "\" Success");
                                    }
                                }),
                            Arbiter.Receive<Fault>(false, responsePort,
                                delegate(Fault fault)
                                {
                                    LogError(LogGroups.Console, "Timed out waiting for response from NXT during LEGO initialization step 11.");
                                }));
                    }

                    //wait 10 ms
                    yield return Arbiter.Receive(false, TimeoutPort(10), delegate(DateTime t) { });

                    //open write
                    responsePort = SendLegoDirectCommand(new LegoOpenWrite(configFile, configureData.Length));
                    yield return Arbiter.Choice(
                        Arbiter.Receive<LegoResponse>(false, responsePort,
                            delegate(LegoResponse legoRsp)
                            {
                                LegoResponseOpenWrite response = new LegoResponseOpenWrite(legoRsp);
                                if (response.Status != 0)
                                {
                                    LogError("LEGO NXT OpenWrite Error in initialization step 12: " + response.ErrorCode.ToString());
                                }
                                else
                                {
                                    LogInfo("LEGO NXT OpenWrite Success");
                                    LogVerbose("LEGO NXT OpenWrite  Handle: " + response.Handle);
                                    handle = response.Handle;
                                    opensuccess = true;
                                }
                            }),
                        Arbiter.Receive<Fault>(false, responsePort,
                            delegate(Fault fault)
                            {
                                LogError(LogGroups.Console, "Timed out waiting for response from NXT during LEGO initialization step 12.");
                            }));

                    if (opensuccess)
                    {
                        //wait 10 ms
                        yield return Arbiter.Receive(false, TimeoutPort(10), delegate(DateTime t) { });

                        //write data
                        LegoWrite write = new LegoWrite(handle, configureData);
                        write.RequireResponse = true;
                        responsePort = SendLegoDirectCommand(write);
                        yield return Arbiter.Choice(
                            Arbiter.Receive<LegoResponse>(false, responsePort,
                                delegate(LegoResponse legoRsp)
                                {
                                    LegoResponseWrite response = new LegoResponseWrite(legoRsp);
                                    if (response.Status != 0)
                                    {
                                        LogError("LEGO NXT Write Error in initialization step 13: " + response.ErrorCode.ToString());
                                    }
                                    else
                                    {
                                        LogInfo("LEGO NXT Write Success");
                                        LogVerbose("LEGO NXT Write Handle: " + response.Handle);
                                        LogVerbose("LEGO NXT Write Bytes: " + response.BytesWritten);
                                        if (response.BytesWritten == configureData.Length)
                                            writesuccess = true;
                                    }
                                }),
                            Arbiter.Receive<Fault>(false, responsePort,
                                delegate(Fault fault)
                                {
                                    LogError(LogGroups.Console, "Timed out waiting for response from NXT during LEGO initialization step 13.");
                                }));

                        //wait 10 ms
                        yield return Arbiter.Receive(false, TimeoutPort(10), delegate(DateTime t) { });

                        //close
                        LegoClose closeagain = new LegoClose(handle);
                        closeagain.RequireResponse = true;
                        responsePort = SendLegoDirectCommand(closeagain);
                        yield return Arbiter.Choice(
                            Arbiter.Receive<LegoResponse>(false, responsePort,
                                delegate(LegoResponse legoRsp)
                                {
                                    LegoResponseClose response = new LegoResponseClose(legoRsp);
                                    if (response.Status != 0)
                                    {
                                        LogError("LEGO NXT Close Error in initialization step 14: " + response.ErrorCode.ToString());
                                    }
                                    else
                                    {
                                        LogInfo("LEGO NXT Close Success");
                                        LogVerbose("LEGO NXT Close Handle: " + response.Handle);
                                    }
                                }),
                            Arbiter.Receive<Fault>(false, responsePort,
                                delegate(Fault fault)
                                {
                                    LogError(LogGroups.Console, "Timed out waiting for response from NXT during LEGO initialization step 14.");
                                }));
                    }

                    trial++;
                }
            }
            else
                writesuccess = true;

            #endregion

            #region Send "start" to LEGO msrs program.
            if (writesuccess)
            {

                //wait 10 ms
                yield return Arbiter.Receive(false, TimeoutPort(10), delegate(DateTime t) { }); ;

                //Send 'start' message to get NXT out of configuration loop
                LegoMessageWrite msgwrite = new LegoMessageWrite(0, "start");
                msgwrite.RequireResponse = true;
                responsePort = SendLegoDirectCommand(msgwrite);
                yield return Arbiter.Choice(
                    Arbiter.Receive<LegoResponse>(false, responsePort,
                        delegate(LegoResponse legoRsp)
                        {
                            if (legoRsp.Status != 0)
                            {
                                LogError(LogGroups.Console, "LEGO NXT MessageWrite Error in initialization step 15: " + legoRsp.ErrorCode.ToString() + "\n" +
                                         "Error sending start message to NXT \"msrs\" program\n" +
                                         "Please restart the LegoNxt service, resend the Configure message,\nor connect to the service using a web browser.");
                            }
                            else
                            {
                                LogVerbose("LEGO NXT MessageWrite Success");
                                startsuccess = true;
                            }
                        }),
                    Arbiter.Receive<Fault>(false, responsePort,
                        delegate(Fault fault)
                        {
                            LogError(LogGroups.Console, "Timed out waiting for response from NXT during LEGO initialization step 15.");
                        }));


            }
            else
            {
                LogError(LogGroups.Console, "Error writing configuration file to NXT.");
                LegoConnection.ShowLegoHelp(this);
            }

            LegoConnectionStatus newStatus = _state.Connected;
            if (startsuccess)
            {
                // If we are already connected, must have been configuring.
                if (_state.Connected == LegoConnectionStatus.Connected)
                {
                    LogInfo(LogGroups.Console, "LEGO NXT Configuration Complete.");
                }
                else
                {
                    //now we are connected
                    newStatus = LegoConnectionStatus.Connected;

                    //to keep lego alive while RS is running
                    _keepAliveTime = 250;
                    _keepAlivePort.Post(DateTime.Now);
                    Activate(Arbiter.Receive(true, _keepAlivePort, KeepAliveHandler));
                }

                // We are not in an exclusive block.  
                // Must post to the UpdateStatus handler to safely
                // update our connection status.
                if (newStatus != _state.Connected)
                {
                    yield return Arbiter.Choice(SendUpdateStatus(newStatus),
                        delegate(DefaultUpdateResponseType response) { },
                        delegate(Fault fault) { });
                }


                // Get the current battery level
                yield return Arbiter.Choice(SendLegoDirectCommand(new LegoGetBatteryLevel()),
                    delegate(LegoResponse response) { },
                    delegate(Fault fault) { });

            }


            #endregion

            //Send 'start' message to get NXT out of configuration loop
            LegoMessageWrite displayMsg = new LegoMessageWrite(1, "hello!");
            displayMsg.RequireResponse = true;
            responsePort = SendLegoDirectCommand(displayMsg);
            yield return Arbiter.Choice(
                Arbiter.Receive<LegoResponse>(false, responsePort,
                    delegate(LegoResponse legoRsp)
                    {
                        if (legoRsp.Status != 0)
                        {
                            LogError(LogGroups.Console, "LEGO NXT MessageWrite Error in initialization step 31: " + legoRsp.ErrorCode.ToString() + "\n" +
                                     "Error sending display text to NXT \"msrs\" program");
                        }
                        else
                        {
                            LogVerbose("LEGO NXT DisplayText Success");
                            startsuccess = true;
                        }
                    }),
                Arbiter.Receive<Fault>(false, responsePort,
                    delegate(Fault fault)
                    {
                        LogError(LogGroups.Console, "Timed out waiting for response from NXT during LEGO initialization step 31.");
                    }));

            // Finished
            if (donePort != null) donePort.Post(true);
            yield break;

        }

        /// <summary>
        /// Remove the specified file from the LEGO brick.
        /// </summary>
        /// <param name="removeFile"></param>
        /// <param name="donePort"></param>
        /// <returns></returns>
        private IEnumerator<ITask> RemoveOldFile(string removeFile, Port<bool> donePort)
        {
            bool fileExists = false;
            int trial = 0;
            bool completed = false;
            PortSet<LegoResponse, Fault> responsePort = new PortSet<LegoResponse, Fault>();

            while (trial < 5 && !completed)
            {
                responsePort = SendLegoDirectCommand(new LegoFindFirst(removeFile));
                yield return Arbiter.Choice(
                    Arbiter.Receive<LegoResponse>(false, responsePort,
                        delegate(LegoResponse response)
                        {
                            if (response.ErrorCode == LegoErrorCode.Success)
                            {
                                completed = true;
                                fileExists = true;
                            }
                            else if (response.ErrorCode == LegoErrorCode.FileNotFound)
                            {
                                completed = true;
                            }
                            else
                                LogError(LogGroups.Console, response.ErrorCode.ToString());
                        }),
                    Arbiter.Receive<Fault>(false, responsePort,
                        delegate(Fault fault)
                        {
                            LogError(LogGroups.Console, "Timed out during old file removal.");
                        }));

                trial++;
            }

            if (!completed)
            {
                if (donePort != null) donePort.Post(false);
                yield break;
            }

            if (fileExists)
            {
                trial = 0;
                completed = false;
                while (trial < 5 && !completed)
                {
                    responsePort = SendLegoDirectCommand(new LegoDelete(removeFile));
                    yield return Arbiter.Choice(
                        Arbiter.Receive<LegoResponse>(false, responsePort,
                            delegate(LegoResponse response)
                            {
                                if (response.ErrorCode == LegoErrorCode.Success)
                                {
                                    completed = true;
                                }
                                else if (response.ErrorCode == LegoErrorCode.FileNotFound)
                                {
                                    completed = true;
                                }
                                else
                                    LogError(LogGroups.Console, response.ErrorCode.ToString());
                            }),
                        Arbiter.Receive<Fault>(false, responsePort,
                            delegate(Fault fault)
                            {
                                LogError(LogGroups.Console, "Timed out during LEGO initialization step 3.");
                            }));

                    trial++;
                }


            }

            donePort.Post(fileExists && completed);

        }

        /// <summary>
        /// Update the current LEGO NXT connection status.
        /// </summary>
        /// <param name="status"></param>
        /// <returns></returns>
        private PortSet<DefaultUpdateResponseType, Fault> SendUpdateStatus(LegoConnectionStatus status)
        {
            UpdateStatus updateStatus = new UpdateStatus(status);
            _internalStatePort.Post(updateStatus);
            return updateStatus.ResponsePort;
        }


        private static byte[] LoadFileResource(string resourceName)
        {
            byte[] fileBytes = null;
            System.Reflection.Assembly currentAssembly = System.Reflection.Assembly.GetExecutingAssembly();
            if (currentAssembly != null)
            {
                Stream rxeStream = currentAssembly.GetManifestResourceStream(resourceName);
                if (rxeStream != null)
                {
                    int pgmLength = (int)rxeStream.Length;
                    if (pgmLength > 0)
                    {
                        fileBytes = new byte[pgmLength];
                        rxeStream.Read(fileBytes, 0, pgmLength);
                    }
                    rxeStream.Close();
                    rxeStream.Dispose();
                }
            }
            return fileBytes;
        }

        /// <summary>
        /// Stop any running Lego Program.
        /// <remarks>Don't wait for a response</remarks>
        /// </summary>
        private PortSet<LegoResponse, Fault> StopLegoProgram()
        {
            // Stop any program that is running
            return SendLegoDirectCommand(new LegoStopProgram());
        }

        /// <summary>
        /// Helper function to convert LegoBrickConfig to ascii data byte[]
        /// </summary>
        private static byte[] ParseConfig(LegoBrickConfig data)
        {
            //ascii character string to send
            //note absolute legal max for send in 142
            //  normal max should be under 100
            //  legal minimum is 72
            byte[] send = new byte[200];

            //current location in the send array
            int ix = 0;

            //first process sensor ports
            foreach (SensorConfig sensor in data.SensorPort)
            {
                string sensorTypeNumber = ((int)sensor.Type).ToString(System.Globalization.NumberFormatInfo.InvariantInfo);
                LegoHelper.SetStringToData(send, ix, sensorTypeNumber);
                ix += sensorTypeNumber.Length;
                send[ix++] = 0x0D; send[ix++] = 0x0A;

                if (sensor.Type == SensorDefinition.SensorType.Touch || sensor.Type == SensorDefinition.SensorType.Null)
                {
                    send[ix++] = 0x30;
                    send[ix++] = 0x0D; send[ix++] = 0x0A;
                    send[ix++] = 0x30;
                    send[ix++] = 0x0D; send[ix++] = 0x0A;
                }
                else
                {
                    int low;
                    int high;
                    if (sensor.ExternalRange)
                    {
                        low = sensor.HighThresh;
                        high = sensor.LowThresh;
                    }
                    else
                    {
                        low = sensor.LowThresh;
                        high = sensor.HighThresh;
                    }

                    LegoHelper.SetStringToData(send, ix, low.ToString(System.Globalization.NumberFormatInfo.InvariantInfo));
                    ix += low.ToString(System.Globalization.NumberFormatInfo.InvariantInfo).Length;
                    send[ix++] = 0x0D; send[ix++] = 0x0A;
                    LegoHelper.SetStringToData(send, ix, high.ToString(System.Globalization.NumberFormatInfo.InvariantInfo));
                    ix += high.ToString(System.Globalization.NumberFormatInfo.InvariantInfo).Length;
                    send[ix++] = 0x0D; send[ix++] = 0x0A;
                }
            }


            //next process motor ports
            foreach (MotorConfig motor in data.MotorPort)
            {
                SensorDefinition.SensorType type = SensorDefinition.GetSensorType(motor.Type);

                LegoHelper.SetStringToData(send, ix, ((int)type).ToString(System.Globalization.NumberFormatInfo.InvariantInfo));
                ix += ((int)type).ToString(System.Globalization.NumberFormatInfo.InvariantInfo).Length;
                send[ix++] = 0x0D; send[ix++] = 0x0A;

                if (type == SensorDefinition.SensorType.Null)
                {
                    send[ix++] = 0x30;
                    send[ix++] = 0x0D; send[ix++] = 0x0A;
                    send[ix++] = 0x30;
                    send[ix++] = 0x0D; send[ix++] = 0x0A;
                }
                else if (type == SensorDefinition.SensorType.Encoder)
                {
                    send[ix++] = 0x30;
                    send[ix++] = 0x0D; send[ix++] = 0x0A;

                    int ticks = motor.TicksPerRevolution;
                    LegoHelper.SetStringToData(send, ix, ticks.ToString(System.Globalization.NumberFormatInfo.InvariantInfo));
                    ix += ticks.ToString(System.Globalization.NumberFormatInfo.InvariantInfo).Length;
                    send[ix++] = 0x0D; send[ix++] = 0x0A;
                }
                else
                {
                    int low;
                    int high;
                    if (motor.ExternalRange)
                    {
                        low = motor.HighThresh;
                        high = motor.LowThresh;
                    }
                    else
                    {
                        low = motor.LowThresh;
                        high = motor.HighThresh;
                    }

                    LegoHelper.SetStringToData(send, ix, low.ToString(System.Globalization.NumberFormatInfo.InvariantInfo), low.ToString(System.Globalization.NumberFormatInfo.InvariantInfo).Length);
                    ix += low.ToString(System.Globalization.NumberFormatInfo.InvariantInfo).Length;
                    send[ix++] = 0x0D; send[ix++] = 0x0A;

                    LegoHelper.SetStringToData(send, ix, high.ToString(System.Globalization.NumberFormatInfo.InvariantInfo), high.ToString(System.Globalization.NumberFormatInfo.InvariantInfo).Length);
                    ix += high.ToString(System.Globalization.NumberFormatInfo.InvariantInfo).Length;
                    send[ix++] = 0x0D; send[ix++] = 0x0A;
                }
            }


            //lastly, process NXT buttons
            foreach (string button in data.ButtonPort)
            {
                SensorDefinition.SensorType btype = SensorDefinition.GetSensorType(button);
                LegoHelper.SetStringToData(send, ix, ((int)btype).ToString(System.Globalization.NumberFormatInfo.InvariantInfo));
                ix += ((int)btype).ToString(System.Globalization.NumberFormatInfo.InvariantInfo).Length;
                send[ix++] = 0x0D; send[ix++] = 0x0A;
            }


            //copy array over to fit exactly;
            byte[] rdata = new byte[ix];
            for (int i = 0; i < ix; i++)
                rdata[i] = send[i];

            return rdata;
        }
        
        #endregion

        #region Service Handlers

        #region Get / HttpGet / HttpPost
        /// <summary>
        /// standard Get handler returns state
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> GetHandler(Get get)
        {
            get.ResponsePort.Post(_state);
            yield break;
        }

        /// <summary>
        /// Http Get Handler.  Needed for XSLT transform
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public virtual IEnumerator<ITask> HttpGetHandler(HttpGet httpGet)
        {
            HttpListenerRequest request = httpGet.Body.Context.Request;
            HttpListenerResponse response = httpGet.Body.Context.Response;

            string path = request.Url.AbsolutePath;

            HttpResponseType rsp = new HttpResponseType(HttpStatusCode.OK, _state, _transform);
            httpGet.ResponsePort.Post(rsp);
            yield break;

        }

        /// <summary>
        /// Http Post Handler.  Handles http form inputs
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> HttpPostHandler(HttpPost httpPost)
        {
            // Use helper to read form data
            ReadFormData readForm = new ReadFormData(httpPost.Body.Context);
            _httpUtilities.Post(readForm);

            // Wait for result
            Activate(Arbiter.Choice(readForm.ResultPort,
                delegate(NameValueCollection parameters)
                {
                    if (!string.IsNullOrEmpty(parameters["Action"])
                        && parameters["Action"] == "LegoNxtConfig")
                    {
                        if (parameters["buttonOk"] == "Connect")
                        {
                            LegoNxtState config = new LegoNxtState();
                            config.BrickConfig = new LegoBrickConfig();

                            int port;
                            if (int.TryParse(parameters["ComPort"], out port) && port >= 0)
                            {
                                config.ComPort = port;
                            }

                            for (int i = 0; i < config.BrickConfig.SensorPort.Length; i++)
                            {
                                config.BrickConfig.SensorPort[i].Type = SensorDefinition.GetSensorType(parameters["SensorType" + (i + 1)]);
                                int low = 0;
                                int high = 0;
                                int.TryParse(parameters["SensorLowThresh" + (i + 1)], out low); ;
                                int.TryParse(parameters["SensorHighThresh" + (i + 1)], out high);
                                config.BrickConfig.SensorPort[i].LowThresh = low;
                                config.BrickConfig.SensorPort[i].HighThresh = high;
                                config.BrickConfig.SensorPort[i].ExternalRange = ((parameters["SensorExternalRange" + (i + 1)] ?? "off") == "on");
                            }

                            for (int i = 0; i < config.BrickConfig.MotorPort.Length; i++)
                            {
                                config.BrickConfig.MotorPort[i].Type = parameters["MotorSensorType" + (i + 1)];
                                int low = 0;
                                int high = 0;
                                int ticks = 6;
                                int.TryParse(parameters["MotorLowThresh" + (i + 1)], out low);
                                int.TryParse(parameters["MotorHighThresh" + (i + 1)], out high);
                                int.TryParse(parameters["MotorTicksPerRev" + (i + 1)], out ticks);
                                config.BrickConfig.MotorPort[i].LowThresh = low;
                                config.BrickConfig.MotorPort[i].HighThresh = high;
                                config.BrickConfig.MotorPort[i].TicksPerRevolution = ticks;
                                config.BrickConfig.MotorPort[i].ExternalRange = ((parameters["MotorExternalRange" + (i + 1)] ?? "off") == "on");
                            }

                            for (int i = 0; i < config.BrickConfig.ButtonPort.Length; i++)
                            {
                                config.BrickConfig.ButtonPort[i] = parameters["ButtonSensorType" + (i + 1)];
                            }


                            Configure replace = new Configure(config);
                            _mainPort.Post(replace);
                            Activate(
                                Arbiter.Choice(
                                    Arbiter.Receive<DefaultReplaceResponseType>(false, replace.ResponsePort,
                                        delegate(DefaultReplaceResponseType response)
                                        {
                                            HttpPostSuccess(httpPost);
                                        }),
                                    Arbiter.Receive<Fault>(false, replace.ResponsePort,
                                        delegate(Fault f)
                                        {
                                            HttpPostFailure(httpPost, f);
                                        })
                                )
                            );

                        }

                    }
                    else if (!string.IsNullOrEmpty(parameters["Action"])
                        && parameters["Action"] == "LegoNxtOutputs")
                    {
                        for (int i = 0; i < _state.MotorOutputPort.Length; i++)
                        {
                            if (parameters["button_set_" + (i + 1)] != null)
                            {
                                //set motor i
                                int set = 0;
                                int.TryParse(parameters["MotorOutput" + (i + 1)], out set);
                                _state.MotorOutputPort[i] = set;

                                LegoSetOutputState motorcmd = new LegoSetOutputState(i, set, LegoOutputMode.MotorOn, LegoRegulationMode.MotorSpeed, 0, LegoRunState.Running, 0);
                                SendLegoCommand sendcmd = new SendLegoCommand(motorcmd);
                                _mainPort.Post(sendcmd);
                            }
                        }

                        if (parameters["button_set_all"] != null)
                        {
                            //set all motors

                            for (int i = 0; i < _state.MotorOutputPort.Length; i++)
                            {
                                //set motor i
                                int set = 0;
                                int.TryParse(parameters["MotorOutput" + (i + 1)], out set);
                                _state.MotorOutputPort[i] = set;

                                LegoSetOutputState motorcmd = new LegoSetOutputState(i, set, LegoOutputMode.MotorOn, LegoRegulationMode.MotorSpeed, 0, LegoRunState.Running, 0);
                                SendLegoCommand sendcmd = new SendLegoCommand(motorcmd);
                                _mainPort.Post(sendcmd);
                            }
                        }
                        else if (parameters["button_stop_all"] != null)
                        {
                            //set all motors to 0

                            for (int i = 0; i < _state.MotorOutputPort.Length; i++)
                            {
                                _state.MotorOutputPort[i] = 0;
                            }

                            LegoSetOutputState motorcmd = new LegoSetOutputState(0xFF, 0, LegoOutputMode.MotorOn, LegoRegulationMode.MotorSpeed, 0, LegoRunState.Running, 0);
                            SendLegoCommand sendcmd = new SendLegoCommand(motorcmd);
                            _mainPort.Post(sendcmd);

                        }
                    }
                    else
                    {
                        HttpPostFailure(httpPost, Fault.FromCodeSubcode(FaultCodes.Sender, DsspFaultCodes.ActionNotSupported));
                    }
                },
                delegate(Exception Failure)
                {
                    LogError(Failure.Message);
                })
            );
            yield break;
        }

        /// <summary>
        /// Send Http Post Success Response
        /// </summary>
        private void HttpPostSuccess(HttpPost httpPost)
        {
            HttpResponseType rsp =
                new HttpResponseType(HttpStatusCode.OK, _state, _transform);
            httpPost.ResponsePort.Post(rsp);
        }

        /// <summary>
        /// Send Http Post Failure Response
        /// </summary>
        private static void HttpPostFailure(HttpPost httpPost, Fault fault)
        {
            HttpResponseType rsp =
                new HttpResponseType(HttpStatusCode.BadRequest,fault);
            httpPost.ResponsePort.Post(rsp);
        }
        #endregion

        #region Configuration
        /// <summary>
        /// Configure LEGO state.  Also tries to connect to NXT.
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> ConfigureHandler(Configure replace)
        {
            bool changeComPort = replace.Body.ComPort != _state.ComPort;
            SaveState(replace.Body);

            if (!ValidSerialPort(replace.Body))
            {
                // Send a connection fault
                replace.ResponsePort.Post(
                    Fault.FromCodeSubcodeReason(FaultCodes.Receiver,
                    DsspFaultCodes.OperationFailed,
                    "Unabled to configure Lego NXT")
                    );
                yield break;
            }

            // Since this handler is concurrent, 
            // call the exclusive state handler to update state.
            ReplaceState replaceState = new ReplaceState(replace.Body);
            _internalStatePort.Post(replaceState);
            yield return Arbiter.Choice(replaceState.ResponsePort,
                delegate(DefaultReplaceResponseType response) { },
                delegate(Fault fault)
                {
                    LogError(LogGroups.Console, "Failure to replace LegoNxt state during Configuration", fault);
                });

            //open NXT port
            if (changeComPort || !_serialPortOpened)
            {
                replace.Body.Connected = LegoConnectionStatus.NotConnected;
                if (_legoBlock.Open(_state.ComPort, _state.BaudRate))
                {
                    if (!_serialPortOpened)
                    {
                        _serialPortOpened = true;

                        // Listen for a single direct Serial port request
                        Activate(Arbiter.Receive(false, _legoCommNoAckPort, LegoCommNoAckHandler));

                        // Listen for a single Serial port request with an acknowledgement
                        Activate(Arbiter.ReceiveWithIterator<SendLegoCommand>(false, _legoCommWithAckPort, LegoCommWithAckHandler));
                    }
                }
                else
                {
                    replace.ResponsePort.Post(
                        Fault.FromCodeSubcodeReason(W3C.Soap.FaultCodes.Sender,
                        DsspFaultCodes.UnknownEntry,
                        "COM Port was not valid."));
                    yield break;
                }
            }

            // Send config file to brick and start program
            Port<bool> donePort = new Port<bool>();
            SpawnIterator<byte[], Port<bool>>(ParseConfig(_state.BrickConfig), donePort, ConfigureNXT);
            yield return Arbiter.Receive(false, donePort,
                delegate(bool success)
                {
                    if (success)
                    {
                        // Success
                        replace.ResponsePort.Post(dssp.DefaultReplaceResponseType.Instance);
                    }
                    else
                    {
                        // Send a connection fault
                        replace.ResponsePort.Post(
                            Fault.FromCodeSubcodeReason(FaultCodes.Receiver,
                            DsspFaultCodes.OperationFailed,
                            "Unabled to configure Lego NXT")
                            );
                    }
                });
            yield break;
        }
        #endregion

        #region Subscriptions

        /// <summary>
        /// General Subscription
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> SubscribeHandler(Subscribe subscribe)
        {
            base.SubscribeHelper(_subMgrPort, subscribe.Body, subscribe.ResponsePort);

            // Send only the status, not the full state.  
            SendNotification<UpdateStatus>(_subMgrPort, subscribe.Body.Subscriber, new UpdateStatus(_state.Connected));
            yield break;
        }

        /// <summary>
        /// Custom Subscription
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> SelectiveSubscribeHandler(SelectiveSubscribe subscribeRequest)
        {
            submgr.InsertSubscription ins = new submgr.InsertSubscription(
                new submgr.InsertSubscriptionMessage(
                subscribeRequest.Body.Subscriber,
                subscribeRequest.Body.Expiration,
                0));

            ins.Body.NotificationCount = subscribeRequest.Body.NotificationCount;

            List<submgr.QueryType> subscribeFilter = new List<submgr.QueryType>();

            if (subscribeRequest.Body.Sensors == null)
            {
                throw new ArgumentNullException("subscribeRequest.Body.Sensors");
            }


            //items in this loop are OR'ed together in the subscription
            foreach (SensorDefinition sensorDefinition in subscribeRequest.Body.Sensors)
            {
                string portNotification = sensorDefinition.Type.ToString().ToUpperInvariant() + sensorDefinition.Port.ToString(System.Globalization.NumberFormatInfo.InvariantInfo);

                LogInfo("Adding subscription for: " + portNotification);

                //you can achieve an AND behavior by adding a list of strings in the new QueryType
                subscribeFilter.Add(new submgr.QueryType(portNotification));
            }


            ins.Body.QueryList = subscribeFilter.ToArray();
            _subMgrPort.Post(ins);

            yield return Arbiter.Choice(ins.ResponsePort,
            delegate(dssp.SubscribeResponseType rsp)
            {
                subscribeRequest.ResponsePort.Post(rsp);
            },
            delegate(Fault fault)
            {
                subscribeRequest.ResponsePort.Post(fault);
            });
            yield break;

        }

        #endregion

        #region Keep Alive
        void KeepAliveHandler(DateTime signal)
        {
            Port<bool> donePort = new Port<bool>();
            SpawnIterator<Port<bool>>(donePort, ProcessKeepAlive);
            Activate(Arbiter.Receive(false, donePort,
                delegate(bool success)
                {
                    //re-call ourselves 
                    Activate(
                        Arbiter.Receive(false, TimeoutPort(_keepAliveTime),
                            delegate(DateTime time) { _keepAlivePort.Post(time); }
                        )
                    );

                }));
        }

        /// <summary>
        /// ProcessKeepAlive
        /// </summary>
        /// <returns></returns>
        IEnumerator<ITask> ProcessKeepAlive(Port<bool> donePort)
        {
            bool getAnotherMessage = true;
            while (getAnotherMessage)
            {
                LegoMessageRead msgRead = new LegoMessageRead(10, 0, true);
                PortSet<LegoResponse, Fault> responsePort = SendLegoDirectCommand(msgRead);
                yield return Arbiter.Choice(
                    Arbiter.Receive<LegoResponse>(false, responsePort,
                        delegate(LegoResponse response)
                        {
                            if (response.LegoCommandCode == LegoHelper.LegoCommandCode.MessageRead)
                            {

                                LegoResponseMessageRead rspMsgRead = new LegoResponseMessageRead(response);
                                if (rspMsgRead.Data != null && response.ErrorCode == LegoErrorCode.Success)
                                {
                                    //sensor update message from NXT
                                    int btLength = rspMsgRead.Data[2];
                                    SensorNotification sensorMsg = new SensorNotification(0x80, Helper.LegoHelper.LegoCommandCode.MessageWrite, 0);

                                    sensorMsg.Data = ByteSubArray(rspMsgRead.Data, 1, btLength + 2);
                                    _internalStatePort.Post(sensorMsg);
                                }
                                else
                                {
                                    getAnotherMessage = false;
                                    if (response.ErrorCode == LegoErrorCode.NoActiveProgram)
                                    {
                                        if (_state.Connected != LegoConnectionStatus.MSRSProgramTerminated)
                                        {
                                            // Update the state -- the LEGO program has been stopped.
                                            _internalStatePort.Post(new UpdateStatus(LegoConnectionStatus.MSRSProgramTerminated));
                                            LogWarning(LogGroups.Console, "The msrs program on the LEGO has been stopped");
                                        }
                                    }
                                }
                            }
                            else
                            {
                                // Serial port buffer is out of sync!
                                LogWarning("Serial port buffer is out of sync.");
                            }
                        }),
                    Arbiter.Receive<Fault>(false, responsePort,
                        delegate(Fault fault)
                        {
                            LogError(fault);
                            getAnotherMessage = false;
                        })
                );
            }

            donePort.Post(true);
            yield break;
        }
        #endregion

        #region Messages
        /// <summary>
        /// Handles incoming sensor updates from brick
        /// <remarks>EXCLUSIVE</remarks>
        /// </summary>
        private IEnumerator<ITask> SensorNotificationHandler(SensorNotification message)
        {
            if (!_ackReceived 
                && message.Data != null 
                && message.Data.Length == 6
                && message.Data[0] == 0 
                && message.Data[1] == 4
                && message.Data[2] == (byte)'a'
                && message.Data[3] == (byte)'c'
                && message.Data[4] == (byte)'k'
                && message.Data[5] == 0)
            {
                this._ackReceived = true;
                yield break;
            }

            SensorDefinition.SensorType sensorType;
            sensorType = SensorDefinition.GetNotificationType(message.Type);

            //update state
            switch (sensorType)
            {
                case SensorDefinition.SensorType.Touch:
                case SensorDefinition.SensorType.Sonar:
                case SensorDefinition.SensorType.LightOn:
                case SensorDefinition.SensorType.LightOff:
                case SensorDefinition.SensorType.Sound:
                    _state.SensorPort[message.Port - 1] = message.Value;
                    break;

                case SensorDefinition.SensorType.Angle:
                case SensorDefinition.SensorType.Encoder:
                    _state.MotorSensorPort[message.Port - 1] = message.Value;
                    break;

                case SensorDefinition.SensorType.Button:
                    _state.ButtonSensorPort[message.Port - 1] = message.Value;
                    if (_state.LegoButtons == null)
                        _state.LegoButtons = new LegoButtons();
                    switch(message.Port)
                    {
                        case 1:
                            _state.LegoButtons.Right = (message.Value != 0);
                            break;
                        case 2:
                            _state.LegoButtons.Left = (message.Value != 0);
                            break;
                        case 3:
                            _state.LegoButtons.Enter = (message.Value != 0);
                            break;
                    }
                    break;

                case SensorDefinition.SensorType.Motor:
                case SensorDefinition.SensorType.Null:
                default:
                    yield break;
            }

            // Send notifications out
            List<string> notify = new List<string>();

            string sensor = sensorType.ToString().ToUpperInvariant() + message.Port.ToString(System.Globalization.NumberFormatInfo.InvariantInfo);
            notify.Add(sensor);

            // notify general subscribers
            SendNotification<Configure>(_subMgrPort, _state);

            // notify selective subscribers
            submgr.Submit sub = new submgr.Submit(_state, dssp.DsspActions.ReplaceRequest, notify.ToArray());
            _subMgrPort.Post(sub);

            yield break;
        }
        #endregion

        #endregion

        #region Lego Command Handlers
        /// <summary>
        /// Main command to send messages to the NXT.  Updates state and notifies subscribers
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> SendLegoCommandHandler(SendLegoCommand submit)
        {
            PortSet<LegoResponse, Fault> responsePort = SendLegoDirectCommand(submit.Body);

            // Wait for a response
            Activate(Arbiter.Choice(
                Arbiter.Receive<LegoResponse>(false, responsePort,
                    delegate(LegoResponse legoResponse)
                    {
                        if (legoResponse.Status == (int)LegoHelper.LegoErrorCode.Success)
                        {
                            //respond success to sender
                            submit.ResponsePort.Post(legoResponse);
                        }
                        else
                        {
                            LogError("Lego Brick Communication Error: " + legoResponse.ErrorCode.ToString());
                            //respond fault to sender
                            submit.ResponsePort.Post(new Fault());
                        }
                    }),
                Arbiter.Receive<Fault>(false, responsePort, 
                    delegate(Fault fault)
                    {
                        LogError(fault);
                        submit.ResponsePort.Post(fault);
                    })));

            yield break;
        }

        /// <summary>
        /// Get the LEGO Battery Level
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> LegoGetBatteryLevelHandler(SendLegoGetBatteryLevel submit)
        {
            yield return Arbiter.Choice(SendLegoDirectCommand(submit.Body),
                delegate(LegoResponse response)
                {
                    submit.ResponsePort.Post((LegoResponseGetBatteryLevel)response);
                },
                delegate(Fault fault)
                {
                    submit.ResponsePort.Post(fault);
                });
            yield break;
        }

        /// <summary>
        /// LegoGetCurrentProgramName
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> LegoGetCurrentProgramNameHandler(SendLegoGetCurrentProgramName submit)
        {
            yield return Arbiter.Choice(SendLegoDirectCommand(submit.Body),
                delegate(LegoResponse response)
                {
                    submit.ResponsePort.Post((LegoResponseGetCurrentProgramName)response);
                },
                delegate(Fault fault)
                {
                    submit.ResponsePort.Post(fault);
                });
            yield break;
        }


        /// <summary>
        /// LegoPlaySoundFile
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> LegoPlaySoundFileHandler(SendLegoPlaySoundFile submit)
        {
            SendLegoCommand cmd = new SendLegoCommand(submit.Body);
            cmd.ResponsePort = submit.ResponsePort;
            _mainPort.Post(cmd);
            yield break;
        }

        /// <summary>
        /// LegoPlayTone
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> LegoPlayToneHandler(SendLegoPlayTone submit)
        {
            SendLegoCommand cmd = new SendLegoCommand(submit.Body);
            cmd.ResponsePort = submit.ResponsePort;
            _mainPort.Post(cmd);
            yield break;
        }

        /// <summary>
        /// LegoSetBrickName
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> LegoSetBrickNameHandler(SendLegoSetBrickName submit)
        {
            SendLegoCommand cmd = new SendLegoCommand(submit.Body);
            cmd.ResponsePort = submit.ResponsePort;
            _mainPort.Post(cmd);
            yield break;
        }

        /// <summary>
        /// Start a LEGO Program
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> LegoStartProgramHandler(SendLegoStartProgram submit)
        {
            SendLegoCommand cmd = new SendLegoCommand(submit.Body);
            cmd.ResponsePort = submit.ResponsePort;
            _mainPort.Post(cmd);
            yield break;
        }

        /// <summary>
        /// LegoStopProgram
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> LegoStopProgramHandler(SendLegoStopProgram submit)
        {
            SendLegoCommand cmd = new SendLegoCommand(submit.Body);
            cmd.ResponsePort = submit.ResponsePort;
            _mainPort.Post(cmd);
            yield break;
        }

        /// <summary>
        /// LegoStopSoundPlayback
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> LegoStopSoundPlaybackHandler(SendLegoStopSoundPlayback submit)
        {
            SendLegoCommand cmd = new SendLegoCommand(submit.Body);
            cmd.ResponsePort = submit.ResponsePort;
            _mainPort.Post(cmd);
            yield break;
        }

        #endregion

        #region State Management


        /// <summary>
        /// Is the serial port a number greater than zero?
        /// </summary>
        /// <param name="state"></param>
        /// <returns></returns>
        private bool ValidSerialPort(LegoNxtState state)
        {
            return state != null && state.ComPort > 0;
        }

        /// <summary>
        /// Update the Button state
        /// </summary>
        /// <param name="legoResponse"></param>
        private void UpdateButton(LegoResponseGetButtonState legoResponse)
        {
            if (_state.LegoButtons.Cancel != legoResponse.PressedCancel
                || _state.LegoButtons.Enter != legoResponse.PressedEnter
                || _state.LegoButtons.Left != legoResponse.PressedLeft
                || _state.LegoButtons.Right != legoResponse.PressedRight)
            {
                _state.LegoButtons.Cancel = legoResponse.PressedCancel;
                _state.LegoButtons.Enter = legoResponse.PressedEnter;
                _state.LegoButtons.Left = legoResponse.PressedLeft;
                _state.LegoButtons.Right = legoResponse.PressedRight;

                _state.ButtonSensorPort[0] = (_state.LegoButtons.Right ? 1 : 0);
                _state.ButtonSensorPort[1] = (_state.LegoButtons.Left ? 1 : 0);
                _state.ButtonSensorPort[2] = (_state.LegoButtons.Enter ? 1 : 0);

                // notify general subscribers
                SendNotification<Configure>(_subMgrPort, _state);

            }
        }


        private void UpdateBattery(LegoResponseGetBatteryLevel legoResponse)
        {
            //update state
            _state.BatteryVoltage = (double)legoResponse.Voltage / 1000;

            //"notify subscribers" & "notify general subscribers" are
            //not implemented because it is unclear when someone would be notified
            //LegoNxtBattery polls for battery messages.  other services should 
            //subscribe to LegoNxtBattery instead
        }

        private void UpdateSensors(LegoResponseGetInputValues sensorValue)
        {
            //update state
            bool changed = false;
            int temp = sensorValue.ScaledValue;
            if (temp != _state.SensorPort[sensorValue.InputPort])
            {
                changed = true;
                _state.SensorPort[sensorValue.InputPort] = temp;
            }
            
            if (changed)
            {
                //create notify list
                List<string> notify = new List<string>();

                SensorDefinition.SensorType sensorType = _state.BrickConfig.SensorPort[sensorValue.InputPort].Type;
                string name = sensorType.ToString().ToUpperInvariant();

                int port = sensorValue.InputPort + 1;

                notify.Add(name + port.ToString(System.Globalization.NumberFormatInfo.InvariantInfo));

                // notify general subscribers
                SendNotification<Configure>(_subMgrPort, _state);
            }
        }

        private void UpdateMotors(LegoSetOutputState motorCommand)
        {
            //create notify list
            List<string> notify = new List<string>();

            //update state
            if (motorCommand.OutputPort == 0xFF) //all motors
            {
                _state.MotorOutputPort[0] = motorCommand.PowerSetPoint;
                _state.MotorOutputPort[1] = motorCommand.PowerSetPoint;
                _state.MotorOutputPort[2] = motorCommand.PowerSetPoint;

                notify.Add("MOTOR1");
                notify.Add("MOTOR2");
                notify.Add("MOTOR3");
            }
            else //single motor
            {
                _state.MotorOutputPort[motorCommand.OutputPort] = motorCommand.PowerSetPoint;
                int port = motorCommand.OutputPort + 1;
                notify.Add("MOTOR" + port.ToString(System.Globalization.NumberFormatInfo.InvariantInfo));
            }

            // notify general subscribers
            SendNotification<Configure>(_subMgrPort, _state);

            // notify selective subscribers
            submgr.Submit sub = new submgr.Submit(_state, dssp.DsspActions.ReplaceRequest, notify.ToArray());
            _subMgrPort.Post(sub);
        }

        #endregion

        #region Return Status Translation

        /// <summary>
        /// Cast LegoResponse to the specific Response type.
        /// </summary>
        /// <param name="legoCommand"></param>
        /// <returns></returns>
        private static LegoResponse TranslateLegoReturnStatus(LegoResponse legoResponse)
        {
            LegoResponse returnStatus;
            switch (legoResponse.LegoCommandCode)
            {
                case Helper.LegoHelper.LegoCommandCode.BootCommand:
                    returnStatus = new LegoResponseBootCommand(legoResponse);
                    break;
                case Helper.LegoHelper.LegoCommandCode.Close:
                    returnStatus = new LegoResponseClose(legoResponse);
                    break;

                case Helper.LegoHelper.LegoCommandCode.Delete:
                    returnStatus = new LegoResponseDelete(legoResponse);
                    break;
                
                case Helper.LegoHelper.LegoCommandCode.FindFirst:
                    returnStatus = new LegoResponseFindFirst(legoResponse);
                    break;

                case Helper.LegoHelper.LegoCommandCode.GetBatteryLevel:
                    returnStatus = new LegoResponseGetBatteryLevel(legoResponse);
                    break;

                case Helper.LegoHelper.LegoCommandCode.GetCurrentProgramName:
                    returnStatus = new LegoResponseGetCurrentProgramName(legoResponse);
                    break;

                case Helper.LegoHelper.LegoCommandCode.GetDeviceInfo:
                    returnStatus = new LegoResponseGetDeviceInfo(legoResponse);
                    break;

                case Helper.LegoHelper.LegoCommandCode.GetFirmwareVersion:
                    returnStatus = new LegoResponseGetFirmwareVersion(legoResponse);
                    break;

                case Helper.LegoHelper.LegoCommandCode.GetInputValues:
                    returnStatus = new LegoResponseGetInputValues(legoResponse);
                    break;

                case Helper.LegoHelper.LegoCommandCode.GetOutputState:
                    returnStatus = new LegoResponseGetOutputState(legoResponse);
                    break;

                case Helper.LegoHelper.LegoCommandCode.KeepAlive:
                    returnStatus = new LegoResponseKeepAlive(legoResponse);
                    break;

                case Helper.LegoHelper.LegoCommandCode.LSGetStatus:
                    returnStatus = new LegoResponseLSGetStatus(legoResponse);
                    break;

                case Helper.LegoHelper.LegoCommandCode.LSRead:
                    returnStatus = new LegoResponseLSRead(legoResponse);
                    break;

                case Helper.LegoHelper.LegoCommandCode.MessageRead:
                    returnStatus = new LegoResponseMessageRead(legoResponse);
                    break;

                case Helper.LegoHelper.LegoCommandCode.OpenRead:
                    returnStatus = new LegoResponseOpenRead(legoResponse);
                    break;

                case Helper.LegoHelper.LegoCommandCode.OpenWrite:
                    returnStatus = new LegoResponseOpenWrite(legoResponse);
                    break;

                case Helper.LegoHelper.LegoCommandCode.OpenWriteData:
                    returnStatus = new LegoResponseOpenWriteData(legoResponse);
                    break;

                case Helper.LegoHelper.LegoCommandCode.OpenWriteLinear:
                    returnStatus = new LegoResponseOpenWriteLinear(legoResponse);
                    break;

                case Helper.LegoHelper.LegoCommandCode.Read:
                    returnStatus = new LegoResponseRead(legoResponse);
                    break;

                case Helper.LegoHelper.LegoCommandCode.Write:
                    returnStatus = new LegoResponseWrite(legoResponse);
                    break;

                // LegoGetButtonState uses ReadIOMap...
                case Helper.LegoHelper.LegoCommandCode.ReadIOMap:
                    returnStatus = new LegoResponseGetButtonState(legoResponse);
                    break;


                default:
                    returnStatus = legoResponse;
                    break;
            }

            return returnStatus;
        }
        #endregion

        private byte[] ByteSubArray(byte[] data, int startIx, int length)
        {
            byte[] subData = null;
            
            if (startIx < 0) 
                startIx = 0;
            
            if ((data == null)  || (startIx >= data.Length))
                return subData;
            
            if ((startIx + length) > data.Length)
                length = data.Length - startIx;

            subData = new byte[length];

            for (int ix = 0; ix < length; ix++)
                subData[ix] = data[startIx + ix];

            return subData;
        }
    }
}
