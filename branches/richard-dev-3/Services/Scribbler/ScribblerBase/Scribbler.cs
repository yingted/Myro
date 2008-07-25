// Copyright (c) Microsoft Corporation.  All rights reserved.

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
using System.Drawing;
using System.Drawing.Imaging;
using System.Threading;
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
        [ServicePort("/scribbler", AllowMultipleInstances = false,
            QueueDepthLimit = 10,
            QueuingPolicy = DsspOperationQueuingPolicy.DiscardWithFault)]
        private ScribblerOperations _mainPort = new ScribblerOperations();

        /// <summary>
        /// Subscription manager partner
        /// </summary>
        [Partner(dssp.Partners.SubscriptionManagerString, Contract = submgr.Contract.Identifier, CreationPolicy = PartnerCreationPolicy.CreateAlways)]
        submgr.SubscriptionManagerPort subMgrPort = new submgr.SubscriptionManagerPort();

        //Timer to poll scribbler at minimum frequency
        private System.Timers.Timer PollTimer;
        private static int TimerDelay = 250;           //4 Hz

        private byte[] cachedJpegHeaderColor = null;
        private byte[] cachedJpegHeaderGray = null;
        private ColorPalette grayscalePallette = new Bitmap(128, 192, PixelFormat.Format8bppIndexed).Palette;

        /// <summary>
        /// Default Service Constructor
        /// </summary>
        public ScribblerService(DsspServiceCreationPort creationPort)
            : base(creationPort)
        {
            for (int i = 0; i < grayscalePallette.Entries.Length; i++)
                grayscalePallette.Entries[i] = Color.FromArgb(i, i, i);
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

            //_mainPort.PostUnknownType(new Reconnect());

            //open Scribbler Communications port
            //if (ConnectToScribbler())
            //{
            // Listen for a single Serial port request with an acknowledgement

            //debug
            //ScribblerCommand cmd = new ScribblerCommand((byte)ScribblerHelper.Commands.GET_INFO);
            //SendScribblerCommand sendcmd = new SendScribblerCommand(cmd);
            //_scribblerComPort.Post(sendcmd);

            //}
            //else
            //{
            //no scribbler found. Open state page for manual settings.
            //OpenServiceInBrowser();
            //}

            Activate(Arbiter.ReceiveWithIterator<SendScribblerCommand>(false, _scribblerComPort, SendScribblerCommandHandler));

            // Listen on the main port for requests and call the appropriate handler.
            Interleave mainInterleave = ActivateDsspOperationHandlers();

            //for HttpPost
            _httpUtilities = DsspHttpUtilitiesService.Create(Environment);

            // Publish the service to the local Node Directory
            DirectoryInsert();

            //add custom handlers to interleave
            mainInterleave.CombineWith(new Interleave(
                new ExclusiveReceiverGroup(
                    Arbiter.ReceiveWithIteratorFromPortSet<SetMotors>(true, _mainPort, SetMotorHandler),
                    Arbiter.ReceiveWithIteratorFromPortSet<SetLED>(true, _mainPort, SetLEDHandler),
                    Arbiter.ReceiveWithIteratorFromPortSet<SetAllLEDs>(true, _mainPort, SetAllLEDsHandler),
                    Arbiter.ReceiveWithIteratorFromPortSet<PlayTone>(true, _mainPort, PlayToneHandler),
                    Arbiter.ReceiveWithIteratorFromPortSet<SetName>(true, _mainPort, SetNameHandler),
                    Arbiter.ReceiveWithIteratorFromPortSet<ScribblerResponseMessage>(true, _mainPort, ScribblerResponseHandler),
                    Arbiter.ReceiveWithIteratorFromPortSet<SetLoud>(true, _mainPort, SetLoudHandler),
                    Arbiter.ReceiveWithIteratorFromPortSet<SetLEDFront>(true, _mainPort, SetLEDFrontHandler),
                    Arbiter.ReceiveWithIteratorFromPortSet<SetLEDBack>(true, _mainPort, SetLEDBackHandler),
                    Arbiter.ReceiveWithIteratorFromPortSet<Reconnect>(true, _mainPort, ReconnectHandler)
                ),
                new ConcurrentReceiverGroup()));

            // These handlers do not use the state, so can run concurrently to those above.
            Activate(Arbiter.ReceiveWithIteratorFromPortSet<GetObstacle>(true, _mainPort, GetObstacleHandler));
            Activate(Arbiter.ReceiveWithIteratorFromPortSet<GetImage>(true, _mainPort, GetImageHandler));
            Activate(Arbiter.ReceiveWithIteratorFromPortSet<GetCamParam>(true, _mainPort, GetCamParamHandler));
            Activate(Arbiter.ReceiveWithIteratorFromPortSet<SetCamParam>(true, _mainPort, SetCamParamHandler));
            Activate(Arbiter.ReceiveWithIteratorFromPortSet<SendScribblerCommand>(true, _mainPort, SendScribblerCommandHandlerExternal));
            Activate(Arbiter.ReceiveWithIteratorFromPortSet<HttpPost>(true, _mainPort, HttpPostHandler));

            // These don't use the state either.
            // GetWindow has to set up the window, then retrieve it, so it must run one at a time.
            Activate(new Interleave(
                new ExclusiveReceiverGroup(
                    Arbiter.ReceiveWithIteratorFromPortSet<GetWindow>(true, _mainPort, GetWindowHandler)
                    ),
                new ConcurrentReceiverGroup()
            ));

        }

        private PortSet<ScribblerResponse, Fault> _scribblerComPortPost(ScribblerCommand cmd)
        {
            SendScribblerCommand sendcmd = new SendScribblerCommand(cmd);
            _scribblerComPort.Post(sendcmd);
            return sendcmd.ResponsePort;
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
            ScribblerCommand cmd = new ScribblerCommand(ScribblerHelper.Commands.GET_ALL);
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

            ScribblerCommand cmd = new ScribblerCommand(ScribblerHelper.Commands.SET_NAME, shortenedname);
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
            ScribblerResponse validResponse;
            try
            {
                validResponse = _scribblerCom.SendCommand(command.Body);
                if (validResponse == null)
                    throw new Exception("Send Scribbler Command null response");
            }
            catch (TimeoutException e)
            {
                Console.WriteLine("Serial port timeout");
                command.ResponsePort.Post(RSUtils.FaultOfException(e));
                validResponse = null;
            }
            catch (Exception e)
            {
                Console.WriteLine("SendCommand exception: " + e.ToString());
                command.ResponsePort.Post(RSUtils.FaultOfException(e));
                validResponse = null;
            }

            if (validResponse != null)
            {
                //reset timer
                PollTimer.Enabled = false;
                PollTimer.Enabled = true;

                //Update our state with the scribbler's response
                //UpdateState(validResponse);
                _mainPort.PostUnknownType(new ScribblerResponseMessage(validResponse));

                command.ResponsePort.Post(validResponse);
            }

            //Console.WriteLine("Finished command; " + command.Body.CommandType);
            // Ready to process another command
            Activate(Arbiter.ReceiveWithIterator<SendScribblerCommand>(false, _scribblerComPort, SendScribblerCommandHandler));
            yield break;
        }

        private IEnumerator<ITask> SendScribblerCommandHandlerExternal(SendScribblerCommand command)
        {
            _scribblerComPort.Post(command);
            yield break;
        }

        private IEnumerator<ITask> ReconnectHandler(Reconnect req)
        {
            //try
            //{
            _state.Connected = false;

            //look for scribbler on last known Com port
            if (_state.ComPort > 0)
            {
                var rPort = new Port<EmptyValue>();
                Exception ex = null;
                new Thread(new ThreadStart(delegate()
                {
                    try
                    {
                        _scribblerCom.Open(_state.ComPort);
                    }
                    catch (Exception e)
                    {
                        ex = e;
                    }
                    rPort.Post(new EmptyValue());
                })).Start();
                yield return Arbiter.Receive(false, rPort, delegate(EmptyValue v) { });
                if (ex != null)
                {
                    req.ResponsePort.Post(RSUtils.FaultOfException(ex));
                    yield break;
                }

                _state.RobotName = _scribblerCom.foundRobotName;
                _state.ComPort = _scribblerCom.openedComPort;
                LogInfo(LogGroups.Console, "Now connected to robot \"" + _state.RobotName + "\" on COM" + _state.ComPort);
                SaveState(_state);

                PollTimer = new System.Timers.Timer();
                PollTimer.Interval = TimerDelay;
                PollTimer.AutoReset = true;
                PollTimer.Elapsed += new System.Timers.ElapsedEventHandler(PollTimer_Elapsed);
                //PollTimer.Start();

                //fix state
                _state.MotorLeft = 100;
                _state.MotorRight = 100;
                _state.LEDLeft = false;
                _state.LEDRight = false;
                _state.LEDCenter = false;

                _state.Connected = true;

                //play startup tone
                LogInfo("playing startup tone...");
                PlayTone startTone = new PlayTone(new PlayToneBody(.2, 1000, 2000));
                _mainPort.PostUnknownType(startTone);
                //yield return Arbiter.Choice(startTone.ResponsePort,
                //    delegate(DefaultUpdateResponseType r) { },
                //    delegate(Fault f) { throw new Exception("Could not play startup tone"); });
                LogInfo("done playing");

                req.ResponsePort.Post(DefaultUpdateResponseType.Instance);

                LogInfo("reconnect sent success");
            }
            else
            {
                req.ResponsePort.Post(RSUtils.FaultOfException(new ScribblerBadCOMPortException()));
                LogInfo("reconnect sent failure");
            }

            yield break;
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

            ScribblerCommand cmd = new ScribblerCommand(ScribblerHelper.Commands.SET_SPEAKER_2,
                                                        (int)(message.Body.Duration * 1000.0),
                                                        message.Body.Frequency1,
                                                        message.Body.Frequency2);

            SendScribblerCommand sendcmd = new SendScribblerCommand(cmd);
            _scribblerComPort.Post(sendcmd);

            yield return Arbiter.Choice(sendcmd.ResponsePort,
                delegate(ScribblerResponse response)
                {
                    //reply to sender
                    message.ResponsePort.Post(DefaultUpdateResponseType.Instance);
                },
                delegate(Fault f)
                {
                    message.ResponsePort.Post(f);
                }
            );

            yield break;
        }

        public IEnumerator<ITask> SetLoudHandler(SetLoud message)
        {
            if (!_state.Connected)
            {
                LogError("trying to set loudness, but not connected");
                message.ResponsePort.Post(new Fault());
                yield break;
            }
            ScribblerCommand cmd = new ScribblerCommand(
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
                        cmd = new ScribblerCommand(ScribblerHelper.Commands.SET_LED_LEFT_ON);
                    else
                        cmd = new ScribblerCommand(ScribblerHelper.Commands.SET_LED_LEFT_OFF);
                    break;
                case 1: //center LED
                    _state.LEDCenter = message.Body.State;
                    if (message.Body.State)
                        cmd = new ScribblerCommand(ScribblerHelper.Commands.SET_LED_CENTER_ON);
                    else
                        cmd = new ScribblerCommand(ScribblerHelper.Commands.SET_LED_CENTER_OFF);
                    break;
                case 2: //right LED
                    _state.LEDRight = message.Body.State;
                    if (message.Body.State)
                        cmd = new ScribblerCommand(ScribblerHelper.Commands.SET_LED_RIGHT_ON);
                    else
                        cmd = new ScribblerCommand(ScribblerHelper.Commands.SET_LED_RIGHT_OFF);
                    break;
                case 3: //all LEDs
                    _state.LEDLeft = message.Body.State;
                    _state.LEDCenter = message.Body.State;
                    _state.LEDRight = message.Body.State;
                    if (message.Body.State)
                        cmd = new ScribblerCommand(ScribblerHelper.Commands.SET_LED_ALL_ON);
                    else
                        cmd = new ScribblerCommand(ScribblerHelper.Commands.SET_LED_ALL_OFF);
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
                cmd = new ScribblerCommand(ScribblerHelper.Commands.SET_DONGLE_LED_ON);
            else
                cmd = new ScribblerCommand(ScribblerHelper.Commands.SET_DONGLE_LED_OFF);

            SendScribblerCommand sendcmd = new SendScribblerCommand(cmd);
            _scribblerComPort.Post(sendcmd);

            _state.LEDFront = set.Body.FrontLED;

            yield return Arbiter.Choice(sendcmd.ResponsePort,
                delegate(ScribblerResponse r) { set.ResponsePort.Post(DefaultUpdateResponseType.Instance); },
                delegate(Fault f) { set.ResponsePort.Post(f); });
            yield break;
        }


        public IEnumerator<ITask> SetLEDBackHandler(SetLEDBack set)
        {
            if (!_state.Connected)
            {
                LogError("Trying to set LED, but not connected");
                set.ResponsePort.Post(new Fault());
                yield break;
            }

            ScribblerCommand cmd = new ScribblerCommand(ScribblerHelper.Commands.SET_DIMMER_LED, set.Body.BackLED);

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
            ScribblerCommand cmd = new ScribblerCommand(ScribblerHelper.Commands.SET_LED_ALL,
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
                //LogError("Scribbler SetMotorHandler: target power set incorrect");
                message.ResponsePort.Post(RSUtils.FaultOfException(new ArgumentOutOfRangeException("Motor speed", "Motor speed out of range")));
                yield break;
            }

            //update state
            _state.MotorLeft = message.Body.LeftSpeed;
            _state.MotorRight = message.Body.RightSpeed;

            //send command
            ScribblerCommand cmd = new ScribblerCommand(ScribblerHelper.Commands.SET_MOTORS, (byte)_state.MotorRight, (byte)_state.MotorLeft);
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
                    cmd = new ScribblerCommand(ScribblerHelper.Commands.GET_DONGLE_L_IR);
                    break;
                case 1:
                    cmd = new ScribblerCommand(ScribblerHelper.Commands.GET_DONGLE_C_IR);
                    break;
                case 2:
                    cmd = new ScribblerCommand(ScribblerHelper.Commands.GET_DONGLE_R_IR);
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
                    try
                    {
                        get.ResponsePort.Post(new UInt16Body(ScribblerHelper.GetShort(r.Data, 0)));
                    }
                    catch (Exception e)
                    {
                        get.ResponsePort.Post(RSUtils.FaultOfException(e));
                    }
                },
                delegate(Fault f)
                {
                    get.ResponsePort.Post(f);
                });
            yield break;
        }

        public IEnumerator<ITask> GetImageHandler(GetImage get)
        {
            if (!_state.Connected)
            {
                get.ResponsePort.Post(new Fault() { Reason = new ReasonText[] { new ReasonText() { Value = "Not connected" } } });
                yield break;
            }

            MyroImageType imageType = null;
            byte[] responseData = null;
            Fault fault = null;
            // Retrieve image data based on image type
            if (get.Body.ImageType.Equals(MyroImageType.Color.Guid))
            {
                imageType = MyroImageType.Color;
                yield return Arbiter.Choice(_scribblerComPortPost(
                    new ScribblerCommand(ScribblerHelper.Commands.GET_IMAGE)),
                    delegate(ScribblerResponse r) { responseData = r.Data; },
                    delegate(Fault f) { fault = f; });
            }
            else if (get.Body.ImageType.Equals(MyroImageType.Gray.Guid))
            {
                imageType = MyroImageType.Gray;
                var gw = new GetWindow(new GetWindowBody()
                {
                    Window = 0,
                    XLow = 1,
                    YLow = 0,
                    XHigh = 255,
                    YHigh = 191,
                    XStep = 2,
                    YStep = 1
                });
                _mainPort.PostUnknownType(gw);
                yield return Arbiter.Choice(gw.ResponsePort,
                    delegate(ImageResponse r)
                    {
                        if (r.Data.Length != 128 * 192)
                            fault = RSUtils.FaultOfException(new Exception("Invalid grayscale image from GetWindow"));
                        else
                        {
                            // Scale image up and copy bits
                            Bitmap bmp = new Bitmap(128, 192, PixelFormat.Format8bppIndexed);
                            bmp.Palette = grayscalePallette;
                            BitmapData bmpData = bmp.LockBits(new Rectangle(0, 0, 128, 192), ImageLockMode.WriteOnly, PixelFormat.Format8bppIndexed);
                            System.Runtime.InteropServices.Marshal.Copy(r.Data, 0, bmpData.Scan0, 128 * 192);
                            bmp.UnlockBits(bmpData);
                            Bitmap bmp2 = new Bitmap(256, 192, PixelFormat.Format24bppRgb);
                            Graphics g = Graphics.FromImage(bmp2);
                            g.InterpolationMode = System.Drawing.Drawing2D.InterpolationMode.Bilinear;
                            g.DrawImage(bmp, 0, 0, 256, 192);
                            BitmapData bmpData2 = bmp2.LockBits(new Rectangle(0, 0, 256, 192), ImageLockMode.ReadOnly, PixelFormat.Format24bppRgb);
                            responseData = new byte[256 * 192 * 3];
                            System.Runtime.InteropServices.Marshal.Copy(bmpData2.Scan0, responseData, 0, 256 * 192 * 3);
                            bmp2.UnlockBits(bmpData2);
                            bmp.Dispose();
                            bmp2.Dispose();
                            g.Dispose();
                        }
                    },
                    delegate(Fault f) { fault = f; });
            }
            else if (get.Body.ImageType.Equals(MyroImageType.JpegColor.Guid) ||
                get.Body.ImageType.Equals(MyroImageType.JpegColorFast.Guid))
            {
                imageType = MyroImageType.JpegColor;

                // Get Header
                if (cachedJpegHeaderColor == null)
                {
                    // NOTE: Hack here, in ScribblerComm.SendCommand, if the command is to
                    // get a JPEG header, it will get the header length from the first two
                    // bytes from the serial port.
                    yield return Arbiter.Choice(_scribblerComPortPost(new ScribblerCommand(
                        ScribblerHelper.Commands.GET_JPEG_COLOR_HEADER)),
                        delegate(ScribblerResponse r) { cachedJpegHeaderColor = r.Data; },
                        delegate(Fault f) { fault = f; });
                    //if (fault == null)
                    //    Console.WriteLine("JPEG: header is " + cachedJpegHeaderColor.Length + " bytes");
                }

                // Get scans
                byte[] scans = null;
                if (fault == null)
                {
                    byte reliable = get.Body.ImageType.Equals(MyroImageType.JpegColor.Guid) ? (byte)1 : (byte)0;
                    yield return Arbiter.Choice(_scribblerComPortPost(new ScribblerCommand(
                        ScribblerHelper.Commands.GET_JPEG_COLOR_SCAN, reliable) { EndMarker1 = 0xff, EndMarker2 = 0xd9 }),
                        delegate(ScribblerResponse r) { scans = r.Data; },
                        delegate(Fault f) { fault = f; });
                }
                //if (fault == null)
                //    Console.WriteLine("JPEG: scans is " + scans.Length + " bytes");

                // Decode JPEG
                if (fault == null)
                {
                    if (cachedJpegHeaderColor == null || scans == null)
                        fault = RSUtils.FaultOfException(new ScribblerDataException("Had null header or scans in GetImageHandler for jpeg"));
                    else
                    {
                        try
                        {
                            byte[] jpeg = new byte[cachedJpegHeaderColor.Length - 2 + scans.Length];
                            Array.Copy(cachedJpegHeaderColor, 2, jpeg, 0, cachedJpegHeaderColor.Length - 2);
                            Array.Copy(scans, 0, jpeg, cachedJpegHeaderColor.Length - 2, scans.Length);

                            // Create Bitmap from jpeg and scale (jpegs from Fluke are half-width)
                            Bitmap obmp = new Bitmap(new MemoryStream(jpeg));
                            Bitmap bmp = new Bitmap(imageType.Width, imageType.Height, PixelFormat.Format24bppRgb);
                            Graphics g = Graphics.FromImage(bmp);
                            g.InterpolationMode = System.Drawing.Drawing2D.InterpolationMode.Bilinear;
                            g.DrawImage(obmp, 0, 0, imageType.Width, imageType.Height);
                            obmp.Dispose();
                            g.Dispose();

                            if (bmp.Width != imageType.Width || bmp.Height != imageType.Height ||
                                    bmp.PixelFormat != PixelFormat.Format24bppRgb)
                                throw new ScribblerDataException("Bitmap from JPEG had wrong size or format");

                            // Copy frame
                            //Console.WriteLine("Copying frame...");
                            BitmapData bitmapData = bmp.LockBits(new Rectangle(0, 0, bmp.Width, bmp.Height),
                                System.Drawing.Imaging.ImageLockMode.ReadOnly, System.Drawing.Imaging.PixelFormat.Format24bppRgb);
                            responseData = new byte[imageType.Width * imageType.Height * 3];
                            System.Runtime.InteropServices.Marshal.Copy(
                                bitmapData.Scan0, responseData, 0, imageType.Width * imageType.Height * 3);
                            bmp.UnlockBits(bitmapData);
                            //Console.WriteLine("JPEG done");
                        }
                        catch (Exception e)
                        {
                            fault = RSUtils.FaultOfException(e);
                        }

                    }
                }
            }
            else if (get.Body.ImageType.Equals(MyroImageType.JpegGray.Guid) ||
                get.Body.ImageType.Equals(MyroImageType.JpegGrayFast.Guid))
            {
                imageType = MyroImageType.JpegGray;

                // Get Header
                if (cachedJpegHeaderGray == null)
                {
                    // NOTE: Hack here, in ScribblerComm.SendCommand, if the command is to
                    // get a JPEG header, it will get the header length from the first two
                    // bytes from the serial port.
                    yield return Arbiter.Choice(_scribblerComPortPost(new ScribblerCommand(
                        ScribblerHelper.Commands.GET_JPEG_GRAY_HEADER)),
                        delegate(ScribblerResponse r) { cachedJpegHeaderGray = r.Data; },
                        delegate(Fault f) { fault = f; });
                    //if (fault == null)
                    //    Console.WriteLine("JPEG: header is " + cachedJpegHeaderGray.Length + " bytes");
                }

                // Get scans
                byte[] scans = null;
                if (fault == null)
                {
                    byte reliable = get.Body.ImageType.Equals(MyroImageType.JpegGray.Guid) ? (byte)1 : (byte)0;
                    yield return Arbiter.Choice(_scribblerComPortPost(new ScribblerCommand(
                        ScribblerHelper.Commands.GET_JPEG_GRAY_SCAN, reliable) { EndMarker1 = 0xff, EndMarker2 = 0xd9 }),
                        delegate(ScribblerResponse r) { scans = r.Data; },
                        delegate(Fault f) { fault = f; });
                }
                //if (fault == null)
                //    Console.WriteLine("JPEG: scans is " + scans.Length + " bytes");

                // Decode JPEG
                if (fault == null)
                {
                    if (cachedJpegHeaderGray == null || scans == null)
                        fault = RSUtils.FaultOfException(new ScribblerDataException("Had null header or scans in GetImageHandler for jpeg"));
                    else
                    {
                        try
                        {
                            byte[] jpeg = new byte[cachedJpegHeaderGray.Length - 2 + scans.Length];
                            Array.Copy(cachedJpegHeaderGray, 2, jpeg, 0, cachedJpegHeaderGray.Length - 2);
                            Array.Copy(scans, 0, jpeg, cachedJpegHeaderGray.Length - 2, scans.Length);

                            // Create Bitmap from jpeg and scale (jpegs from Fluke are half-width)
                            Bitmap obmp = new Bitmap(new MemoryStream(jpeg));
                            Bitmap bmp = new Bitmap(imageType.Width, imageType.Height, PixelFormat.Format24bppRgb);
                            Graphics g = Graphics.FromImage(bmp);
                            g.InterpolationMode = System.Drawing.Drawing2D.InterpolationMode.Bilinear;
                            g.DrawImage(obmp, 0, 0, imageType.Width, imageType.Height);
                            obmp.Dispose();
                            g.Dispose();

                            if (bmp.Width != imageType.Width || bmp.Height != imageType.Height ||
                                    bmp.PixelFormat != PixelFormat.Format24bppRgb)
                                throw new ScribblerDataException("Bitmap from JPEG had wrong size or format");

                            // Copy frame
                            BitmapData bitmapData = bmp.LockBits(new Rectangle(0, 0, bmp.Width, bmp.Height),
                                System.Drawing.Imaging.ImageLockMode.ReadOnly, System.Drawing.Imaging.PixelFormat.Format24bppRgb);
                            responseData = new byte[imageType.Width * imageType.Height * 3];
                            System.Runtime.InteropServices.Marshal.Copy(
                                bitmapData.Scan0, responseData, 0, imageType.Width * imageType.Height * 3);
                            bmp.UnlockBits(bitmapData);
                        }
                        catch (Exception e)
                        {
                            fault = RSUtils.FaultOfException(e);
                        }

                    }
                }
            }
            else
            {
                fault = RSUtils.FaultOfException(
                        new ArgumentException("Invalid image type: " + get.Body.ImageType, "ImageType"));
            }

            // Send the image response
            if (fault == null)
            {
                if (responseData != null && imageType != null)
                    get.ResponsePort.Post(new ImageResponse()
                        {
                            Width = imageType.Width,
                            Height = imageType.Height,
                            Timestamp = DateTime.Now,
                            Data = responseData
                        });
                else
                    get.ResponsePort.Post(RSUtils.FaultOfException(new Exception("Internal error: in ScribblerBase.GetImageHandler")));
            }
            else
                get.ResponsePort.Post(fault);

            yield break;
        }

        public IEnumerator<ITask> GetWindowHandler(GetWindow get)
        {
            if (!_state.Connected)
            {
                get.ResponsePort.Post(new Fault() { Reason = new ReasonText[] { new ReasonText() { Value = "Not connected" } } });
                yield break;
            }

            Fault fault = null;
            yield return Arbiter.Choice(
                _scribblerComPortPost(new ScribblerCommand(
                    ScribblerHelper.Commands.SET_WINDOW,
                    new byte[] {
                        get.Body.Window,
                        get.Body.XLow,
                        get.Body.YLow,
                        get.Body.XHigh,
                        get.Body.YHigh,
                        get.Body.XStep,
                        get.Body.YStep },
                        false, 0)),
                    delegate(ScribblerResponse r) { },
                    delegate(Fault f) { fault = f; });

            int width = ((int)get.Body.XHigh - (int)get.Body.XLow) / get.Body.XStep + 1,
                height = ((int)get.Body.YHigh - (int)get.Body.YLow) / get.Body.YStep + 1,
                size = width * height;
            if (fault == null)
                yield return Arbiter.Choice(
                    _scribblerComPortPost(new ScribblerCommand(
                        ScribblerHelper.Commands.GET_WINDOW,
                        new byte[] { get.Body.Window },
                        false,
                        size)),
                    delegate(ScribblerResponse r)
                    {
                        get.ResponsePort.Post(new ImageResponse()
                        {
                            Width = width,
                            Height = height,
                            Timestamp = DateTime.Now,
                            Data = r.Data
                        });
                    },
                    delegate(Fault f) { fault = f; });

            if (fault != null)
                get.ResponsePort.Post(fault);

            yield break;

        }

        public IEnumerator<ITask> GetCamParamHandler(GetCamParam get)
        {
            if (!_state.Connected)
            {
                get.ResponsePort.Post(new Fault() { Reason = new ReasonText[] { new ReasonText() { Value = "Not connected" } } });
                yield break;
            }

            yield return Arbiter.Choice(
                _scribblerComPortPost(new ScribblerCommand(
                    ScribblerHelper.Commands.GET_CAM_PARAM,
                    get.Body.Value)),
                delegate(ScribblerResponse r) { get.ResponsePort.Post(new ByteBody(r.Data[0])); },
                delegate(Fault f) { get.ResponsePort.Post(f); });
            yield break;
        }

        public IEnumerator<ITask> SetCamParamHandler(SetCamParam set)
        {
            if (!_state.Connected)
            {
                set.ResponsePort.Post(new Fault() { Reason = new ReasonText[] { new ReasonText() { Value = "Not connected" } } });
                yield break;
            }

            yield return Arbiter.Choice(
                _scribblerComPortPost(new ScribblerCommand(
                    ScribblerHelper.Commands.SET_CAM_PARAM,
                    set.Body.Addr,
                    set.Body.Value)),
                delegate(ScribblerResponse r) { set.ResponsePort.Post(DefaultUpdateResponseType.Instance); },
                delegate(Fault f) { set.ResponsePort.Post(f); });
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
        //[ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> HttpPostHandler(HttpPost httpPost)
        {
            // Use helper to read form data
            ReadFormData readForm = new ReadFormData(httpPost);
            _httpUtilities.Post(readForm);

            // Read form data
            NameValueCollection parameters = null;
            yield return Arbiter.Choice(readForm.ResultPort,
                delegate(NameValueCollection p) { parameters = p; },
                delegate(Exception e) { throw new Exception("Error reading form data", e); });

            // Act on form data
            if (!string.IsNullOrEmpty(parameters["Action"])
                  && parameters["Action"] == "ScribblerConfig")
            {
                if (parameters["buttonOk"] == "Change" && _state.Connected)
                {
                    SetNameBody newname = new SetNameBody(parameters["Name"]);
                    SetName newnamemessage = new SetName(newname);
                    _mainPort.PostUnknownType(newnamemessage);
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

                    //HttpPostSuccess(httpPost);
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
                    LogInfo("connecting to scribbler...");
                    Reconnect rec = new Reconnect();
                    _mainPort.PostUnknownType(rec);
                    yield return Arbiter.Choice(rec.ResponsePort,
                        delegate(DefaultUpdateResponseType r)
                        {
                            LogInfo("connected, sending http reply");
                            HttpPostSuccess(httpPost);
                            LogInfo("http reply sent");
                        },
                        delegate(Fault f)
                        {
                            httpPost.ResponsePort.Post(f);
                        });
                }
            }
            else if (!string.IsNullOrEmpty(parameters["Action"])
                  && parameters["Action"] == "ScribblerSensors")
            {
                if (parameters["buttonOk"] == "Poll" && _state.Connected)
                {
                    ScribblerCommand cmd = new ScribblerCommand(ScribblerHelper.Commands.GET_ALL);
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

                    _mainPort.PostUnknownType(setMotorsRequest);

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

                    _mainPort.PostUnknownType(setMotorsRequest);

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
                    _mainPort.PostUnknownType(setAllLeds);

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
                    _mainPort.PostUnknownType(playToneMessage);

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
            //HttpResponseType rsp =
            //    new HttpResponseType(HttpStatusCode.OK, RSUtils.FaultOfException(new Exception(faultMessage)));
            httpPost.ResponsePort.Post(new Fault() { Reason = new ReasonText[] { new ReasonText() { Value = faultMessage } } });
        }


        /// <summary>
        /// Replace Handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public virtual IEnumerator<ITask> ReplaceHandler(Replace replace)
        {
            //if (_state.ComPort != replace.Body.ComPort)
            //{
            //    _state = replace.Body;
            //    var ePort = new Port<Exception>();
            //    Dispatcher.AddCausality(new Causality("connect to scribbler", ePort));
            //    Activate(ePort.Receive(delegate(Exception e) { replace.ResponsePort.Post(RSUtils.FaultOfException(e)); }));
            //    yield return new IterativeTask(ConnectToScribbler);
            //    replace.ResponsePort.Post(DefaultReplaceResponseType.Instance);
            //}
            //else
            //{
            _state = replace.Body;
            replace.ResponsePort.Post(DefaultReplaceResponseType.Instance);
            //}
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
