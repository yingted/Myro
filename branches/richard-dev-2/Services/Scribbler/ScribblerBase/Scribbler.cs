//------------------------------------------------------------------------------
// Scribbler.cs
//
//     
//      Ben Axelrod 08/28/2006
//
//------------------------------------------------------------------------------
using Microsoft.Ccr.Core;
using Microsoft.Dss.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.Core.DsspHttp;
using Microsoft.Dss.Core.DsspHttpUtilities;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Dss.ServiceModel.DsspServiceBase;
using System;
using System.IO;
using System.IO.Ports;
using System.Xml;
using System.Text;
using System.Net;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Security.Permissions;
using W3C.Soap;
using Myro.Utilities;

using submgr = Microsoft.Dss.Services.SubscriptionManager;
using dssp = Microsoft.Dss.ServiceModel.Dssp;


namespace Myro.Services.Scribbler.ScribblerBase
{

    [DisplayName("Scribbler Base")]
    [Description("The IPRE Scribbler Base Service")]
    [Contract(Contract.Identifier)]
    public class ScribblerService : DsspServiceBase
    {
        /// <summary>
        /// The saved state file name
        /// </summary>
        private const string _configFile = "Scribbler.State.xml";

        //NOTE: format: /resources/ServicePortName/Namespace.File
        //const string _transform = "/resources/scribbler/IPRE.ScribblerBase.Scribbler.xslt";

        [EmbeddedResource("Myro.Services.Scribbler.ScribblerBase.Scribbler.xslt")]
        string _transform = null;


        /// <summary>
        /// HttpGet helper
        /// </summary>
        private DsspHttpUtilitiesPort _httpUtilities = new DsspHttpUtilitiesPort();

        /// <summary>
        /// The current state
        /// </summary>
        [InitialStatePartner(Optional = true, ServiceUri = _configFile)]
        private ScribblerState _state = null;

        /// <summary>
        /// internal com port management
        /// </summary>
        private ScribblerCom _scribblerCom = new ScribblerCom();

        /// <summary>
        /// internal port for sending data to scribbler
        /// </summary>
        private Port<SendScribblerCommand> _scribblerComPort = new Port<SendScribblerCommand>();

        /// <summary>
        /// Main operations port
        /// </summary>
        [ServicePort("/scribbler", AllowMultipleInstances = false)]
        private ScribblerOperations _mainPort = new ScribblerOperations();

        /// <summary>
        /// Subscription manager partner
        /// </summary>
        [Partner(dssp.Partners.SubscriptionManagerString, Contract = submgr.Contract.Identifier, CreationPolicy = PartnerCreationPolicy.CreateAlways)]
        submgr.SubscriptionManagerPort subMgrPort = new submgr.SubscriptionManagerPort();

        //Timer to poll scribbler at minimum frequency
        private System.Timers.Timer PollTimer;
        private static int TimerDelay = 250;           //4 Hz

        /// <summary>
        /// Default Service Constructor
        /// </summary>
        public ScribblerService(DsspServiceCreationPort creationPort)
            : base(creationPort)
        {

        }

        /// <summary>
        /// Service Start
        /// </summary>
        protected override void Start()
        {
            if (_state == null)
            {
                //initialize state
                _state = new ScribblerState();
                _state.ComPort = 0;
                _state.RobotName = null;

                //motors initially stopped
                _state.MotorLeft = 100;
                _state.MotorRight = 100;

                SaveState(_state);
            }

            // display HTTP service Uri
            LogInfo(LogGroups.Console, "Service uri: ");

            //open Scribbler Communications port
            if (ConnectToScribbler())
            {
                // Listen for a single Serial port request with an acknowledgement
                Activate(Arbiter.ReceiveWithIterator<SendScribblerCommand>(false, _scribblerComPort, SendScribblerCommandHandler));

                PollTimer = new System.Timers.Timer();
                PollTimer.Interval = TimerDelay;
                PollTimer.AutoReset = true;
                PollTimer.Elapsed += new System.Timers.ElapsedEventHandler(PollTimer_Elapsed);
                PollTimer.Start();


                //play startup tone
                PlayToneBody startTone = new PlayToneBody(200, 1000, 2000);
                _mainPort.Post(new PlayTone(startTone));

                //debug
                //ScribblerCommand cmd = new ScribblerCommand((byte)ScribblerHelper.Commands.GET_INFO);
                //SendScribblerCommand sendcmd = new SendScribblerCommand(cmd);
                //_scribblerComPort.Post(sendcmd);


                //fix state
                _state.MotorLeft = 100;
                _state.MotorRight = 100;
                _state.LEDLeft = false;
                _state.LEDRight = false;
                _state.LEDCenter = false;
            }
            else
            {
                //no scribbler found. Open state page for manual settings.
                //OpenServiceInBrowser();
            }


            // Listen on the main port for requests and call the appropriate handler.
            Interleave mainInterleave = ActivateDsspOperationHandlers();

            //for HttpPost
            _httpUtilities = DsspHttpUtilitiesService.Create(Environment);

            // Publish the service to the local Node Directory
            DirectoryInsert();

            //add custom handlers to interleave
            mainInterleave.CombineWith(new Interleave(
                new TeardownReceiverGroup(),
                new ExclusiveReceiverGroup(
                    Arbiter.ReceiveWithIterator<SetMotors>(true, _mainPort, SetMotorHandler),
                    Arbiter.ReceiveWithIterator<SetLED>(true, _mainPort, SetLEDHandler),
                    Arbiter.ReceiveWithIterator<SetAllLEDs>(true, _mainPort, SetAllLEDsHandler),
                    Arbiter.ReceiveWithIterator<PlayTone>(true, _mainPort, PlayToneHandler),
                    Arbiter.ReceiveWithIterator<SetName>(true, _mainPort, SetNameHandler),
                    Arbiter.ReceiveWithIterator<ScribblerResponseMessage>(true, _mainPort, ScribblerResponseHandler)
                ),
                new ConcurrentReceiverGroup()
            ));

        }

        /// <summary>
        /// Opens service in a web browser.
        /// </summary>
        private void OpenServiceInBrowser()
        {
            //start up IE to state page so user can configure
            System.Diagnostics.Process process = new System.Diagnostics.Process();
            process.StartInfo.FileName = FindServiceAliasFromScheme(Uri.UriSchemeHttp);
            process.Start();
        }


        /// <summary>
        /// This will poll the scribbler at a minimum frequency
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void PollTimer_Elapsed(object sender, EventArgs e)
        {
            ScribblerCommand cmd = new ScribblerCommand((byte)ScribblerHelper.Commands.GET_ALL);
            SendScribblerCommand sendcmd = new SendScribblerCommand(cmd);
            _scribblerComPort.Post(sendcmd);
        }


        private IEnumerator<ITask> SetNameHandler(SetName command)
        {
            if (string.IsNullOrEmpty(command.Body.NewName))
            {
                LogError("New name is null");
                command.ResponsePort.Post(new Fault());
                yield break;
            }

            if (!_state.Connected)
            {
                command.ResponsePort.Post(new Fault());
                yield break;
            }

            string shortenedname;
            if (command.Body.NewName.Length > 8)
                shortenedname = command.Body.NewName.Substring(0, 8);
            else
                shortenedname = command.Body.NewName;

            _state.RobotName = shortenedname;

            ScribblerCommand cmd = new ScribblerCommand((byte)ScribblerHelper.Commands.SET_NAME, shortenedname);
            SendScribblerCommand sendcmd = new SendScribblerCommand(cmd);
            _scribblerComPort.Post(sendcmd);

            yield return Arbiter.Receive<ScribblerResponse>(false, sendcmd.ResponsePort,
                delegate(ScribblerResponse response)
                {
                    SaveState(_state);
                }
            );

            //reply to sender
            command.ResponsePort.Post(DefaultUpdateResponseType.Instance);

            yield break;
        }


        /// <summary>
        /// Send a command to the Scribbler and wait for a response.
        /// </summary>
        //[ServiceHandler(ServiceHandlerBehavior.Exclusive, PortFieldName = "_scribblerComPort")]
        private IEnumerator<ITask> SendScribblerCommandHandler(SendScribblerCommand command)
        {
            // Send command to robot and wait for echo and response
            ScribblerResponse validResponse = _scribblerCom.SendCommand(command.Body);
            if (validResponse == null)
            {
                LogError(LogGroups.Console, "Send Scribbler Command null response");
                command.ResponsePort.Post(new Fault());
            }
            else
            {
                //reset timer
                PollTimer.Enabled = false;
                PollTimer.Enabled = true;

                //Update our state with the scribbler's response
                //UpdateState(validResponse);
                _mainPort.Post(new ScribblerResponseMessage(validResponse));

                command.ResponsePort.Post(validResponse);
            }

            //Console.WriteLine("Finished command; " + command.Body.CommandType);
            // Ready to process another command
            Activate(Arbiter.ReceiveWithIterator<SendScribblerCommand>(false, _scribblerComPort, SendScribblerCommandHandler));
            yield break;
        }



        private bool ConnectToScribbler()
        {
            try
            {
                _state.Connected = false;

                //look for scribbler on last known Com port
                if (_state.ComPort > 0)
                {
                    _state.Connected = _scribblerCom.Open(_state.ComPort);
                }

                //scan all ports for the name of our Robot
                /*
                if (_state.Connected == false)
                {
                    _state.Connected = _scribblerCom.FindRobot(_state.RobotName);
                }
                */
            }
            catch (UnauthorizedAccessException ex)
            {
                LogError(ex);
            }
            catch (IOException ex)
            {
                LogError(ex);
            }
            catch (ArgumentOutOfRangeException ex)
            {
                LogError(ex);
            }
            catch (ArgumentException ex)
            {
                LogError(ex);
            }
            catch (InvalidOperationException ex)
            {
                LogError(ex);
            }

            if (!_state.Connected)
            {
                LogError(LogGroups.Console, "No Scribbler robot found.");
            }

            if (_state.Connected)
            {
                _state.RobotName = _scribblerCom.foundRobotName;
                _state.ComPort = _scribblerCom.openedComPort;
                LogInfo(LogGroups.Console, "Now connected to robot \"" + _state.RobotName + "\" on COM" + _state.ComPort);
                SaveState(_state);
            }

            return _state.Connected;
        }


        /// <summary>
        /// Handles incoming play tone requests
        /// </summary>
        //[ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> PlayToneHandler(PlayTone message)
        {
            if (!_state.Connected)
            {
                LogError("trying to play tone, but not connected");
                message.ResponsePort.Post(new Fault());
                yield break;
            }

            ScribblerCommand cmd = new ScribblerCommand((byte)ScribblerHelper.Commands.SET_SPEAKER_2,
                                                        message.Body.Duration,
                                                        message.Body.Frequency1,
                                                        message.Body.Frequency2);

            SendScribblerCommand sendcmd = new SendScribblerCommand(cmd);
            _scribblerComPort.Post(sendcmd);

            yield return Arbiter.Receive<ScribblerResponse>(false, sendcmd.ResponsePort,
                delegate(ScribblerResponse response)
                {
                    //reply to sender
                    message.ResponsePort.Post(DefaultUpdateResponseType.Instance);
                }
            );

            yield break;
        }

        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> SetLoudHandler(SetLoud message)
        {
            if (!_state.Connected)
            {
                LogError("trying to set loudness, but not connected");
                message.ResponsePort.Post(new Fault());
                yield break;
            }
            ScribblerCommand cmd = new ScribblerCommand((byte)
                (message.Body.IsLoud ? ScribblerHelper.Commands.SET_LOUD : ScribblerHelper.Commands.SET_QUIET));
            SendScribblerCommand sendcmd = new SendScribblerCommand(cmd);
            _scribblerComPort.Post(sendcmd);
            yield return Arbiter.Choice(sendcmd.ResponsePort,
                delegate(ScribblerResponse f) { message.ResponsePort.Post(DefaultUpdateResponseType.Instance); },
                delegate(Fault f) { message.ResponsePort.Post(f); });
            yield break;
        }


        /// <summary>
        /// Handles incoming SetLED requests
        /// </summary>
        private IEnumerator<ITask> SetLEDHandler(SetLED message)
        {
            if (!_state.Connected)
            {
                LogError("Trying to set LED, but not connected");
                message.ResponsePort.Post(new Fault());
                yield break;
            }

            ScribblerCommand cmd;

            switch (message.Body.LED)
            {
                case 0: //left LED
                    _state.LEDLeft = message.Body.State;
                    if (message.Body.State)
                        cmd = new ScribblerCommand((byte)ScribblerHelper.Commands.SET_LED_LEFT_ON);
                    else
                        cmd = new ScribblerCommand((byte)ScribblerHelper.Commands.SET_LED_LEFT_OFF);
                    break;
                case 1: //center LED
                    _state.LEDCenter = message.Body.State;
                    if (message.Body.State)
                        cmd = new ScribblerCommand((byte)ScribblerHelper.Commands.SET_LED_CENTER_ON);
                    else
                        cmd = new ScribblerCommand((byte)ScribblerHelper.Commands.SET_LED_CENTER_OFF);
                    break;
                case 2: //right LED
                    _state.LEDRight = message.Body.State;
                    if (message.Body.State)
                        cmd = new ScribblerCommand((byte)ScribblerHelper.Commands.SET_LED_RIGHT_ON);
                    else
                        cmd = new ScribblerCommand((byte)ScribblerHelper.Commands.SET_LED_RIGHT_OFF);
                    break;
                case 3: //all LEDs
                    _state.LEDLeft = message.Body.State;
                    _state.LEDCenter = message.Body.State;
                    _state.LEDRight = message.Body.State;
                    if (message.Body.State)
                        cmd = new ScribblerCommand((byte)ScribblerHelper.Commands.SET_LED_ALL_ON);
                    else
                        cmd = new ScribblerCommand((byte)ScribblerHelper.Commands.SET_LED_ALL_OFF);
                    break;
                default:
                    LogError("LED number set incorrect");
                    cmd = new ScribblerCommand();
                    break;
            }

            SendScribblerCommand sendcmd = new SendScribblerCommand(cmd);
            _scribblerComPort.Post(sendcmd);

            yield return Arbiter.Receive<ScribblerResponse>(false, sendcmd.ResponsePort,
                delegate(ScribblerResponse response)
                {

                }
            );

            //reply to sender
            message.ResponsePort.Post(DefaultUpdateResponseType.Instance);
            yield break;
        }

        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> SetLEDFrontHandler(SetLEDFront set)
        {
            if (!_state.Connected)
            {
                LogError("Trying to set LED, but not connected");
                set.ResponsePort.Post(new Fault());
                yield break;
            }

            ScribblerCommand cmd;
            if (set.Body.FrontLED == true)
                cmd = new ScribblerCommand((byte)ScribblerHelper.Commands.SET_DONGLE_LED_ON);
            else
                cmd = new ScribblerCommand((byte)ScribblerHelper.Commands.SET_DONGLE_LED_OFF);

            SendScribblerCommand sendcmd = new SendScribblerCommand(cmd);
            _scribblerComPort.Post(sendcmd);

            _state.LEDFront = set.Body.FrontLED;

            yield return Arbiter.Choice(sendcmd.ResponsePort,
                delegate(ScribblerResponse r) { set.ResponsePort.Post(DefaultUpdateResponseType.Instance); },
                delegate(Fault f) { set.ResponsePort.Post(f); });
            yield break;
        }


        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> SetLEDBackHandler(SetLEDBack set)
        {
            if (!_state.Connected)
            {
                LogError("Trying to set LED, but not connected");
                set.ResponsePort.Post(new Fault());
                yield break;
            }

            ScribblerCommand cmd = new ScribblerCommand((byte)ScribblerHelper.Commands.SET_DIMMER_LED, set.Body.BackLED);

            SendScribblerCommand sendcmd = new SendScribblerCommand(cmd);
            _scribblerComPort.Post(sendcmd);

            _state.LEDBack = set.Body.BackLED;

            yield return Arbiter.Choice(sendcmd.ResponsePort,
                delegate(ScribblerResponse r) { set.ResponsePort.Post(DefaultUpdateResponseType.Instance); },
                delegate(Fault f) { set.ResponsePort.Post(f); });
            yield break;
        }

        /// <summary>
        /// Handles incoming SetAllLEDs requests
        /// </summary>
        private IEnumerator<ITask> SetAllLEDsHandler(SetAllLEDs message)
        {
            //error check
            if (!_state.Connected)
            {
                message.ResponsePort.Post(new Fault());
                yield break;
            }

            //update state
            _state.LEDLeft = message.Body.LeftLED;
            _state.LEDCenter = message.Body.CenterLED;
            _state.LEDRight = message.Body.RightLED;

            //send command
            ScribblerCommand cmd = new ScribblerCommand((byte)ScribblerHelper.Commands.SET_LED_ALL,
                                                            _state.LEDLeft,
                                                            _state.LEDCenter,
                                                            _state.LEDRight);

            SendScribblerCommand sendcmd = new SendScribblerCommand(cmd);
            _scribblerComPort.Post(sendcmd);

            yield return Arbiter.Receive<ScribblerResponse>(false, sendcmd.ResponsePort,
                delegate(ScribblerResponse response)
                {

                }
            );

            //reply to sender
            message.ResponsePort.Post(DefaultUpdateResponseType.Instance);
            yield break;
        }


        /// <summary>
        /// Handles incoming set motor messages
        /// </summary>
        private IEnumerator<ITask> SetMotorHandler(SetMotors message)
        {
            if (!_state.Connected)
            {
                message.ResponsePort.Post(new Fault());
                yield break;
            }

            //debug
            if (message.Body.LeftSpeed < 0 || message.Body.LeftSpeed > 200 || message.Body.RightSpeed < 0 || message.Body.RightSpeed > 200)
            {
                LogError("Scribbler SetMotorHandler: target power set incorrect");
            }

            //update state
            _state.MotorLeft = message.Body.LeftSpeed;
            _state.MotorRight = message.Body.RightSpeed;

            //send command
            ScribblerCommand cmd = new ScribblerCommand((byte)ScribblerHelper.Commands.SET_MOTORS, (byte)_state.MotorRight, (byte)_state.MotorLeft);
            SendScribblerCommand sendcmd = new SendScribblerCommand(cmd);
            _scribblerComPort.Post(sendcmd);

            yield return Arbiter.Receive<ScribblerResponse>(false, sendcmd.ResponsePort,
                delegate(ScribblerResponse response)
                {
                    //initialize notify list
                    List<string> notify = new List<string>();
                    notify.Add("MOTORS");

                    // notify general subscribers
                    subMgrPort.Post(new submgr.Submit(_state, dssp.DsspActions.ReplaceRequest));

                    // notify selective subscribers
                    submgr.Submit sub = new submgr.Submit(_state, dssp.DsspActions.ReplaceRequest, notify.ToArray());
                    subMgrPort.Post(sub);

                    //reply to say we are done
                    message.ResponsePort.Post(DefaultUpdateResponseType.Instance);
                }
            );

            yield break;
        }

        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> GetObstacleHandler(GetObstacle get)
        {
            if (!_state.Connected)
            {
                get.ResponsePort.Post(new Fault() { Reason = new ReasonText[] { new ReasonText() { Value = "Not connected" } } });
                yield break;
            }

            ScribblerCommand cmd;
            switch (get.Body.Value)
            {
                case 0:
                    cmd = new ScribblerCommand((byte)ScribblerHelper.Commands.GET_DONGLE_L_IR);
                    break;
                case 1:
                    cmd = new ScribblerCommand((byte)ScribblerHelper.Commands.GET_DONGLE_C_IR);
                    break;
                case 2:
                    cmd = new ScribblerCommand((byte)ScribblerHelper.Commands.GET_DONGLE_R_IR);
                    break;
                default:
                    get.ResponsePort.Post(RSUtils.FaultOfException(
                        new ArgumentOutOfRangeException("DONGLE_IR", get.Body, "Dongle IR sensor must be 0, 1, or 2")));
                    yield break;
                    break;
            }

            SendScribblerCommand sendcmd = new SendScribblerCommand(cmd);
            _scribblerComPort.Post(sendcmd);
            yield return Arbiter.Choice(sendcmd.ResponsePort,
                delegate(ScribblerResponse r)
                {
                    get.ResponsePort.Post(new UInt16Body(ScribblerHelper.GetShort(r.Data, 0)));
                },
                delegate(Fault f)
                {
                    get.ResponsePort.Post(f);
                });
            yield break;
        }


        /// <summary>
        /// Update state after recieving return data from robot
        /// </summary>
        /// <param name="response">response from scribbler robot</param>
        private IEnumerator<ITask> ScribblerResponseHandler(ScribblerResponseMessage response) //private void UpdateState(ScribblerResponse response) //debug
        {
            //initialize notify list
            List<string> notify = new List<string>();

            switch ((ScribblerHelper.Commands)response.Body.CommandType)
            {
                case ScribblerHelper.Commands.GET_STATE:
                    ScribblerHelper.GetStatusDecomp parse_get_state = new ScribblerHelper.GetStatusDecomp(response.Body.Data[0], response.Body.Data[1]);
                    if (_state.Stall != parse_get_state.Stall)
                    {
                        notify.Add("STALL");
                        _state.Stall = parse_get_state.Stall;
                    }
                    if (_state.LineLeft != parse_get_state.LineLeft)
                    {
                        notify.Add("LINELEFT");
                        _state.LineLeft = parse_get_state.LineLeft;
                    }
                    if (_state.LineRight != parse_get_state.LineRight)
                    {
                        notify.Add("LINERIGHT");
                        _state.LineRight = parse_get_state.LineRight;
                    }
                    _state.LEDLeft = parse_get_state.LedLeft;
                    _state.LEDCenter = parse_get_state.LedCenter;
                    _state.LEDRight = parse_get_state.LedRight;
                    break;
                case ScribblerHelper.Commands.GET_IR_LEFT:
                    bool New_Get_Open_Left = response.Body.Data[0] == 1;             //NOTE: Not inverting logic here
                    if (_state.IRLeft != New_Get_Open_Left)
                    {
                        notify.Add("IRLEFT");
                        _state.IRLeft = New_Get_Open_Left;
                    }
                    break;
                case ScribblerHelper.Commands.GET_IR_RIGHT:
                    bool New_Get_Open_Right = response.Body.Data[0] == 1;             //NOTE: Not inverting logic here
                    if (_state.IRRight != New_Get_Open_Right)
                    {
                        notify.Add("IRRIGHT");
                        _state.IRRight = New_Get_Open_Right;
                    }
                    break;
                case ScribblerHelper.Commands.GET_STALL:
                    bool New_Get_Stall = response.Body.Data[0] == 1;
                    if (_state.Stall != New_Get_Stall)
                    {
                        notify.Add("STALL");
                        _state.Stall = New_Get_Stall;
                    }
                    break;
                case ScribblerHelper.Commands.GET_LIGHT_LEFT:
                    _state.LightLeft = BitConverter.ToUInt16(new byte[] { response.Body.Data[1], response.Body.Data[0] }, 0);
                    notify.Add("LIGHTLEFT");
                    break;
                case ScribblerHelper.Commands.GET_LIGHT_CENTER:
                    _state.LightCenter = BitConverter.ToUInt16(new byte[] { response.Body.Data[1], response.Body.Data[0] }, 0);
                    notify.Add("LIGHTCENTER");
                    break;
                case ScribblerHelper.Commands.GET_LIGHT_RIGHT:
                    _state.LightRight = BitConverter.ToUInt16(new byte[] { response.Body.Data[1], response.Body.Data[0] }, 0);
                    notify.Add("LIGHTRIGHT");
                    break;
                case ScribblerHelper.Commands.GET_LINE_RIGHT:
                    bool New_Get_Line_Right = response.Body.Data[0] == 1;
                    if (_state.LineRight != New_Get_Line_Right)
                    {
                        notify.Add("LINERIGHT");
                        _state.LineRight = New_Get_Line_Right;
                    }
                    break;
                case ScribblerHelper.Commands.GET_LINE_LEFT:
                    bool New_Get_Line_Left = response.Body.Data[0] == 1;
                    if (_state.LineLeft != New_Get_Line_Left)
                    {
                        notify.Add("LINELEFT");
                        _state.LineLeft = New_Get_Line_Left;
                    }
                    break;
                case ScribblerHelper.Commands.GET_NAME:
                    Encoding.ASCII.GetChars(response.Body.Data, 0, 8);
                    break;
                case ScribblerHelper.Commands.GET_LIGHT_ALL:
                    //TODO: this can be simplified if the scribbler sent low byte first
                    _state.LightLeft = BitConverter.ToUInt16(new byte[] { response.Body.Data[1], response.Body.Data[0] }, 0);
                    _state.LightCenter = BitConverter.ToUInt16(new byte[] { response.Body.Data[3], response.Body.Data[2] }, 0);
                    _state.LightRight = BitConverter.ToUInt16(new byte[] { response.Body.Data[5], response.Body.Data[4] }, 0);
                    _state.LightSensorsLastUpdated = DateTime.Now;
                    notify.Add("LIGHTLEFT");
                    notify.Add("LIGHTCENTER");
                    notify.Add("LIGHTRIGHT");
                    break;
                case ScribblerHelper.Commands.GET_IR_ALL:
                    bool newirleft = response.Body.Data[0] == 1;    //NOTE: Not inverting logic here
                    bool newirright = response.Body.Data[1] == 1;
                    if (_state.IRLeft != newirleft)
                    {
                        notify.Add("IRLEFT");
                        _state.IRLeft = newirleft;
                    }
                    if (_state.IRRight != newirright)
                    {
                        notify.Add("IRRIGHT");
                        _state.IRRight = newirright;
                    }
                    _state.LineSensorsLastUpdated = DateTime.Now;
                    _state.BumpersLastUpdated = DateTime.Now;
                    break;
                case ScribblerHelper.Commands.GET_LINE_ALL:
                    bool newlineleft = response.Body.Data[0] == 1;
                    bool newlineright = response.Body.Data[1] == 1;
                    if (_state.LineLeft != newlineleft)
                    {
                        notify.Add("LINELEFT");
                        _state.LineLeft = newlineleft;
                    }
                    if (_state.LineRight != newlineright)
                    {
                        notify.Add("LINERIGHT");
                        _state.LineRight = newlineright;
                    }
                    _state.LineSensorsLastUpdated = DateTime.Now;
                    break;
                case ScribblerHelper.Commands.GET_ALL:
                case ScribblerHelper.Commands.SET_MOTORS_OFF:
                case ScribblerHelper.Commands.SET_MOTORS:
                case ScribblerHelper.Commands.SET_LED_LEFT_ON:
                case ScribblerHelper.Commands.SET_LED_LEFT_OFF:
                case ScribblerHelper.Commands.SET_LED_CENTER_ON:
                case ScribblerHelper.Commands.SET_LED_CENTER_OFF:
                case ScribblerHelper.Commands.SET_LED_RIGHT_ON:
                case ScribblerHelper.Commands.SET_LED_RIGHT_OFF:
                case ScribblerHelper.Commands.SET_SPEAKER:
                case ScribblerHelper.Commands.SET_SPEAKER_2:
                case ScribblerHelper.Commands.SET_NAME:
                case ScribblerHelper.Commands.SET_LED_ALL_ON:
                case ScribblerHelper.Commands.SET_LED_ALL_OFF:
                case ScribblerHelper.Commands.SET_LED_ALL:
                case ScribblerHelper.Commands.SET_LOUD:
                case ScribblerHelper.Commands.SET_QUIET:
                case ScribblerHelper.Commands.SET_DATA:
                case ScribblerHelper.Commands.SET_ECHO_MODE:
                    bool New_Get_All_IR_Left = response.Body.Data[0] == 1;    //NOTE: Not inverting logic here
                    bool New_Get_All_IR_Right = response.Body.Data[1] == 1;
                    if (_state.IRLeft != New_Get_All_IR_Left)
                    {
                        notify.Add("IRLEFT");
                        _state.IRLeft = New_Get_All_IR_Left;
                    }
                    if (_state.IRRight != New_Get_All_IR_Right)
                    {
                        notify.Add("IRRIGHT");
                        _state.IRRight = New_Get_All_IR_Right;
                    }

                    _state.LightLeft = BitConverter.ToUInt16(new byte[] { response.Body.Data[3], response.Body.Data[2] }, 0);
                    _state.LightCenter = BitConverter.ToUInt16(new byte[] { response.Body.Data[5], response.Body.Data[4] }, 0);
                    _state.LightRight = BitConverter.ToUInt16(new byte[] { response.Body.Data[7], response.Body.Data[6] }, 0);
                    notify.Add("LIGHTLEFT");
                    notify.Add("LIGHTCENTER");
                    notify.Add("LIGHTRIGHT");

                    bool New_Get_All_Line_Left = response.Body.Data[8] == 1;
                    bool New_Get_All_Line_Right = response.Body.Data[9] == 1;
                    if (_state.LineLeft != New_Get_All_Line_Left)
                    {
                        notify.Add("LINELEFT");
                        _state.LineLeft = New_Get_All_Line_Left;
                    }
                    if (_state.LineRight != New_Get_All_Line_Right)
                    {
                        notify.Add("LINERIGHT");
                        _state.LineRight = New_Get_All_Line_Right;
                    }

                    bool newstall = response.Body.Data[10] == 1;
                    if (_state.Stall != newstall)
                    {
                        notify.Add("STALL");
                        _state.Stall = newstall;
                    }
                    _state.LineSensorsLastUpdated = DateTime.Now;
                    _state.BumpersLastUpdated = DateTime.Now;
                    _state.LightSensorsLastUpdated = DateTime.Now;
                    break;
                case ScribblerHelper.Commands.GET_ALL_BINARY:
                    ScribblerHelper.AllBinaryDecomp parse_all_binary = new ScribblerHelper.AllBinaryDecomp(response.Body.Data[0]);

                    if (_state.IRLeft != parse_all_binary.IRLeft)
                    {
                        notify.Add("IRLEFT");
                        _state.IRLeft = parse_all_binary.IRLeft;       //NOTE: Not inverting logic here
                    }
                    if (_state.IRRight != parse_all_binary.IRRight)
                    {
                        notify.Add("IRRIGHT");
                        _state.IRRight = parse_all_binary.IRRight;
                    }

                    if (_state.LineLeft != parse_all_binary.LineLeft)
                    {
                        notify.Add("LINELEFT");
                        _state.LineLeft = parse_all_binary.LineLeft;
                    }
                    if (_state.LineRight != parse_all_binary.LineRight)
                    {
                        notify.Add("LINERIGHT");
                        _state.LineRight = parse_all_binary.LineRight;
                    }

                    if (_state.Stall != parse_all_binary.Stall)
                    {
                        notify.Add("STALL");
                        _state.Stall = parse_all_binary.Stall;
                    }
                    _state.LineSensorsLastUpdated = DateTime.Now;
                    _state.BumpersLastUpdated = DateTime.Now;
                    break;
                case ScribblerHelper.Commands.GET_INFO:
                    Console.WriteLine(System.Text.Encoding.ASCII.GetChars(response.Body.Data));
                    break;
                case ScribblerHelper.Commands.GET_DATA:
                    foreach (byte b in response.Body.Data)
                        Console.Write(b);
                    Console.Write("\n");
                    break;
                case 0:
                    //Console.WriteLine("Got 0 command");
                    // Do nothing
                    break;
                default:
                    LogError(LogGroups.Console, "Update State command missmatch, got " + response.Body.CommandType);
                    break;
            }

            // notify general subscribers
            subMgrPort.Post(new submgr.Submit(_state, dssp.DsspActions.ReplaceRequest));

            // notify selective subscribers
            submgr.Submit sub = new submgr.Submit(_state, dssp.DsspActions.ReplaceRequest, notify.ToArray());
            subMgrPort.Post(sub);

            yield break;
        }



        /// <summary>
        /// Get Handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public virtual IEnumerator<ITask> GetHandler(Get get)
        {
            get.ResponsePort.Post(_state);
            yield break;
        }

        /// <summary>
        /// Http Get Handler for XSLT 
        /// </summary>
        /// <param name="httpGet"></param>
        /// <returns></returns>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> HttpGetHandler(HttpGet httpGet)
        {
            if (_transform == null)
                Console.WriteLine("TRANSFORM NULL");
            else
                Console.WriteLine("TRANSFORM NOT NULL");
            httpGet.ResponsePort.Post(new HttpResponseType(
                    System.Net.HttpStatusCode.OK, _state, _transform));
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
                          && parameters["Action"] == "ScribblerConfig")
                    {
                        if (parameters["buttonOk"] == "Change" && _state.Connected)
                        {
                            SetNameBody newname = new SetNameBody(parameters["Name"]);
                            SetName newnamemessage = new SetName(newname);
                            _mainPort.Post(newnamemessage);
                            Activate(
                                Arbiter.Choice(
                                    Arbiter.Receive<DefaultUpdateResponseType>(false, newnamemessage.ResponsePort,
                                        delegate(DefaultUpdateResponseType response)
                                        {
                                            HttpPostSuccess(httpPost);
                                        }),
                                    Arbiter.Receive<Fault>(false, newnamemessage.ResponsePort,
                                        delegate(Fault f)
                                        {
                                            HttpPostFailure(httpPost, f.Reason[0].Value);
                                        })
                                )
                            );

                        }
                        else if (parameters["buttonOk"] == "Connect" && _state.Connected)
                        {
                            //close down this connection to make a new connection below

                            PollTimer.Close();

                            System.Threading.Thread.Sleep(100);

                            _scribblerCom.Close();

                            _state.Connected = false;
                        }

                        if (parameters["buttonOk"] == "Connect" && !_state.Connected)
                        {
                            int port = 0;
                            int.TryParse(parameters["ComPort"], out port);
                            string name = parameters["Name"];
                            if (!string.IsNullOrEmpty(name) && name.Length > 8)
                                name = name.Substring(0, 8);

                            _state.ComPort = port;
                            _state.RobotName = name;

                            //open Scribbler Communications port
                            if (ConnectToScribbler())
                            {
                                // Listen for a single Serial port request with an acknowledgement
                                Activate(Arbiter.ReceiveWithIterator<SendScribblerCommand>(false, _scribblerComPort, SendScribblerCommandHandler));

                                PollTimer = new System.Timers.Timer();
                                PollTimer.Interval = TimerDelay;
                                PollTimer.AutoReset = true;
                                PollTimer.Elapsed += new System.Timers.ElapsedEventHandler(PollTimer_Elapsed);
                                PollTimer.Start();

                                //play startup tone
                                PlayToneBody startTone = new PlayToneBody(200, 1000, 2000);
                                PlayTone playToneMessage = new PlayTone(startTone);
                                _mainPort.Post(playToneMessage);

                                Activate(
                                    Arbiter.Choice(
                                        Arbiter.Receive<DefaultUpdateResponseType>(false, playToneMessage.ResponsePort,
                                            delegate(DefaultUpdateResponseType response)
                                            {
                                                HttpPostSuccess(httpPost);
                                            }),
                                        Arbiter.Receive<Fault>(false, playToneMessage.ResponsePort,
                                            delegate(Fault f)
                                            {
                                                HttpPostFailure(httpPost, f.Reason[0].Value);
                                            })
                                    )
                                );
                            }
                            else
                                HttpPostFailure(httpPost, "Connection to Scribbler failed");
                        }
                    }
                    else if (!string.IsNullOrEmpty(parameters["Action"])
                          && parameters["Action"] == "ScribblerSensors")
                    {
                        if (parameters["buttonOk"] == "Poll" && _state.Connected)
                        {
                            ScribblerCommand cmd = new ScribblerCommand((byte)ScribblerHelper.Commands.GET_ALL);
                            SendScribblerCommand sendcmd = new SendScribblerCommand(cmd);
                            _scribblerComPort.Post(sendcmd);
                            Activate(
                                Arbiter.Choice(
                                    Arbiter.Receive<ScribblerResponse>(false, sendcmd.ResponsePort,
                                        delegate(ScribblerResponse response)
                                        {
                                            HttpPostSuccess(httpPost);
                                        }),
                                    Arbiter.Receive<Fault>(false, sendcmd.ResponsePort,
                                        delegate(Fault f)
                                        {
                                            HttpPostFailure(httpPost, f.Reason[0].Value);
                                        })
                                )
                            );
                        }
                    }
                    else if (!string.IsNullOrEmpty(parameters["Action"])
                        && parameters["Action"] == "ScribblerMotors")
                    {
                        if (parameters["buttonOk"] == "Set" && _state.Connected)
                        {
                            int left = _state.MotorLeft;
                            int right = _state.MotorRight;
                            int.TryParse(parameters["LeftMotor"], out left);
                            int.TryParse(parameters["RightMotor"], out right);

                            SetMotorsBody setMotorsBody = new SetMotorsBody(left, right);
                            SetMotors setMotorsRequest = new SetMotors(setMotorsBody);

                            _mainPort.Post(setMotorsRequest);

                            Activate(
                                Arbiter.Choice(
                                    Arbiter.Receive<DefaultUpdateResponseType>(false, setMotorsRequest.ResponsePort,
                                        delegate(DefaultUpdateResponseType response)
                                        {
                                            HttpPostSuccess(httpPost);
                                        }),
                                    Arbiter.Receive<Fault>(false, setMotorsRequest.ResponsePort,
                                        delegate(Fault f)
                                        {
                                            HttpPostFailure(httpPost, f.Reason[0].Value);
                                        })
                                )
                            );
                        }
                        else if (parameters["buttonOk"] == "All Stop" && _state.Connected)
                        {
                            SetMotorsBody setMotorsBody = new SetMotorsBody(100, 100);
                            SetMotors setMotorsRequest = new SetMotors(setMotorsBody);

                            _mainPort.Post(setMotorsRequest);

                            Activate(
                                Arbiter.Choice(
                                    Arbiter.Receive<DefaultUpdateResponseType>(false, setMotorsRequest.ResponsePort,
                                        delegate(DefaultUpdateResponseType response)
                                        {
                                            HttpPostSuccess(httpPost);
                                        }),
                                    Arbiter.Receive<Fault>(false, setMotorsRequest.ResponsePort,
                                        delegate(Fault f)
                                        {
                                            HttpPostFailure(httpPost, f.Reason[0].Value);
                                        })
                                )
                            );
                        }
                    }
                    else if (!string.IsNullOrEmpty(parameters["Action"])
                        && parameters["Action"] == "ScribblerLEDs")
                    {
                        if (parameters["buttonOk"] == "Set" && _state.Connected)
                        {
                            bool left = ((parameters["LeftLED"] ?? "off") == "on");
                            bool center = ((parameters["CenterLED"] ?? "off") == "on");
                            bool right = ((parameters["RightLED"] ?? "off") == "on");

                            SetAllLedsBody leds = new SetAllLedsBody(left, center, right);
                            SetAllLEDs setAllLeds = new SetAllLEDs(leds);
                            _mainPort.Post(setAllLeds);

                            Activate(
                                Arbiter.Choice(
                                    Arbiter.Receive<DefaultUpdateResponseType>(false, setAllLeds.ResponsePort,
                                        delegate(DefaultUpdateResponseType response)
                                        {
                                            HttpPostSuccess(httpPost);
                                        }),
                                    Arbiter.Receive<Fault>(false, setAllLeds.ResponsePort,
                                        delegate(Fault f)
                                        {
                                            HttpPostFailure(httpPost, f.Reason[0].Value);
                                        })
                                )
                            );

                        }
                    }
                    else if (!string.IsNullOrEmpty(parameters["Action"])
                   && parameters["Action"] == "ScribblerSpeaker")
                    {
                        if (parameters["buttonOk"] == "Play" && _state.Connected)
                        {
                            int tone1 = 0;
                            int tone2 = 0;
                            int duration = 0;
                            int.TryParse(parameters["Tone1"], out tone1);
                            int.TryParse(parameters["Tone2"], out tone2);
                            int.TryParse(parameters["Duration"], out duration);

                            PlayToneBody playTone = new PlayToneBody(duration, tone1, tone2);
                            PlayTone playToneMessage = new PlayTone(playTone);
                            _mainPort.Post(playToneMessage);

                            Activate(
                                Arbiter.Choice(
                                    Arbiter.Receive<DefaultUpdateResponseType>(false, playToneMessage.ResponsePort,
                                        delegate(DefaultUpdateResponseType response)
                                        {
                                            HttpPostSuccess(httpPost);
                                        }),
                                    Arbiter.Receive<Fault>(false, playToneMessage.ResponsePort,
                                        delegate(Fault f)
                                        {
                                            HttpPostFailure(httpPost, f.Reason[0].Value);
                                        })
                                )
                            );
                        }
                    }
                    else
                    {
                        HttpPostFailure(httpPost, "Unknown Http Post");
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
        /// Send Http Post Success Response helper
        /// </summary>
        private void HttpPostSuccess(HttpPost httpPost)
        {
            HttpResponseType rsp =
                new HttpResponseType(HttpStatusCode.OK, _state, _transform);
            httpPost.ResponsePort.Post(rsp);
        }

        /// <summary>
        /// Send Http Post Failure Response helper
        /// </summary>
        private void HttpPostFailure(HttpPost httpPost, string faultMessage)
        {
            HttpResponseType rsp =
                new HttpResponseType(HttpStatusCode.ExpectationFailed, faultMessage);
            httpPost.ResponsePort.Post(rsp);
        }


        /// <summary>
        /// Replace Handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public virtual IEnumerator<ITask> ReplaceHandler(Replace replace)
        {
            _state = replace.Body;
            replace.ResponsePort.Post(DefaultReplaceResponseType.Instance);
            yield break;
        }


        // General Subscription
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> SubscribeHandler(Subscribe subscribe)
        {
            base.SubscribeHelper(subMgrPort, subscribe.Body, subscribe.ResponsePort);
            yield break;
        }

        // Custom Subscription
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> SelectiveSubscribeHandler(SelectiveSubscribe subRequest)
        {
            submgr.InsertSubscription selectiveSubscription = new submgr.InsertSubscription(
                new submgr.InsertSubscriptionMessage(
                    subRequest.Body.Subscriber,
                    subRequest.Body.Expiration,
                    0));

            selectiveSubscription.Body.NotificationCount = subRequest.Body.NotificationCount;

            List<submgr.QueryType> subscribeFilter = new List<submgr.QueryType>();

            //items in this loop are OR'ed together in the subscription
            foreach (string s in subRequest.Body.Sensors)
            {
                LogInfo("Adding subscription for: " + s.ToUpper());

                //you can achieve an AND behavior by adding a list of strings in the new QueryType
                subscribeFilter.Add(new submgr.QueryType(s.ToUpper()));
            }

            selectiveSubscription.Body.QueryList = subscribeFilter.ToArray();
            subMgrPort.Post(selectiveSubscription);

            yield return Arbiter.Choice(selectiveSubscription.ResponsePort,
                delegate(dssp.SubscribeResponseType response)
                {
                    subRequest.ResponsePort.Post(response);
                },
                delegate(Fault fault)
                {
                    subRequest.ResponsePort.Post(fault);
                });
            yield break;
        }


    }
}
