//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
// 
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: Srv1Camera.cs $ $Revision$
//
//  Portions modified Copyright (C) 2006 SharpLogic Software.
//-----------------------------------------------------------------------

using Microsoft.Ccr.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Dss.ServiceModel.DsspServiceBase;
using System;
using System.Collections.Generic;
using System.Security.Permissions;
using Microsoft.Dss.Core.DsspHttp;
using System.Drawing;
using System.IO;
using System.Drawing.Imaging;
using W3C.Soap;
using System.Net;
using System.Net.Mime;
using Microsoft.Dss.Core.DsspHttpUtilities;
using submgr = Microsoft.Dss.Services.SubscriptionManager;
using System.Collections.Specialized;
using Microsoft.Dss.Core;
using System.ComponentModel;
using srv1srv = SharpLogic.Robotics.Services.Surveyor.Srv1;

using vision = SharpLogic.Robotics.Services.Surveyor.Srv1.Vision;


namespace SharpLogic.Robotics.Services.Surveyor.Srv1.Camera
{
    
    [Contract(Contract.Identifier)]
    [ActivationSettings(ExecutionUnitsPerDispatcher=1,ShareDispatcher=false)]
    [DisplayName("Surveyor SRV-1 Camera")]
    [Description("Captures images from an attached camera.")]
    public class Srv1CameraService : DsspServiceBase
    {
        [InitialStatePartner(Optional = true, ServiceUri = "Srv1Camera.xml")]
        Srv1CameraState _state;

        [EmbeddedResource("SharpLogic.Robotics.Services.Surveyor.Srv1.Srv1Camera.camera.xslt")]
        string _transform = null;

        [ServicePort("/camera", AllowMultipleInstances = false)]
        Srv1CameraServiceOperations _mainPort = new Srv1CameraServiceOperations();
        Srv1CameraServiceOperations _fwdPort;

        FramePort _framePort = new FramePort();

        DsspHttpUtilitiesPort _utilitiesPort = new DsspHttpUtilitiesPort();
        DispatcherQueue _queue = null;
        SaveStreamPort _streamPort = null;

        [Partner("SubMgr", Contract = submgr.Contract.Identifier, CreationPolicy = PartnerCreationPolicy.CreateAlways)]
        submgr.SubscriptionManagerPort _submgrPort = new submgr.SubscriptionManagerPort();

        [Partner(srv1srv.Partners.Srv1, Contract = srv1srv.Contract.Identifier, CreationPolicy = PartnerCreationPolicy.UseExistingOrCreate)]
        srv1srv.Srv1Operations _controllerPort = new srv1srv.Srv1Operations();

        public Srv1CameraService(DsspServiceCreationPort creationPort) : 
                base(creationPort)
        {
        }
        
        protected override void Start()
        {
            Port<bool> successPort = new Port<bool>();
            SpawnIterator(successPort, StartIterator);
            Activate(Arbiter.Receive(false, successPort,
                delegate(bool success)
                {
                    if (!success)
                    {
                        LogError("Service failed to start. Shutting down.");
                        Shutdown();
                    }
                }
            ));
        }

        private IEnumerator<ITask> StartIterator(Port<bool> successPort)
        {
            bool success = false;

            InitController init = new InitController();
            _controllerPort.Post(init);
            yield return Arbiter.Choice(
                init.ResponsePort,
                delegate(DefaultSubmitResponseType response)
                {
                    // The controller initialized successfully.
                    success = true;
                },
                delegate(Fault fault) { }
            );

            if (!success)
            {
                successPort.Post(false);
                yield break;
            }

            // Activate service handlers.
            StartBehavior();
            successPort.Post(true);
            yield break;
        }

        private void StartBehavior()
        {
            _fwdPort = ServiceForwarder<Srv1CameraServiceOperations>(ServiceInfo.Service);
            _utilitiesPort = DsspHttpUtilitiesService.Create(Environment);

            if (_state == null)
            {
                _state = new Srv1CameraState();
            }
            // Listen on the main port for requests and call the appropriate handler.

            Activate(
                Arbiter.Interleave(
                    new TeardownReceiverGroup(
                        Arbiter.Receive<DsspDefaultDrop>(false, _mainPort, DropHandler)
                    ),
                    new ExclusiveReceiverGroup
                    (
                        Arbiter.ReceiveWithIterator<Replace>(true, _mainPort, ReplaceHandler),
                        Arbiter.ReceiveWithIterator<UpdateDevice>(true, _mainPort, UpdateDeviceHandler),
                        Arbiter.ReceiveWithIterator<UpdateFormat>(true, _mainPort, UpdateFormatHandler),
                        Arbiter.Receive(true, _framePort, FrameHandler),
                        Arbiter.ReceiveWithIterator<HttpPost>(true, _mainPort, HttpPostHandler)
                    ),
                    new ConcurrentReceiverGroup
                    (
                        Arbiter.ReceiveWithIterator<Get>(true, _mainPort, GetHandler),
                        Arbiter.ReceiveWithIterator<QueryFrame>(true, _mainPort, QueryFrameHandler),
                        Arbiter.ReceiveWithIterator<UpdateFrame>(true, _mainPort, UpdateFrameHandler),
                        Arbiter.ReceiveWithIterator<HttpGet>(true, _mainPort, HttpGetHandler),
                        Arbiter.ReceiveWithIterator<Subscribe>(true, _mainPort, SubscribeHandler),
                        Arbiter.Receive<DsspDefaultLookup>(true, _mainPort, DefaultLookupHandler)
                    ))
            );

            SpawnIterator(_state, GetInitialState);
        }

        Choice InitializeInternalService()
        {
            AllocateExecutionResource allocExecRes = new AllocateExecutionResource(0, "Srv1Camera");

            ResourceManagerPort.Post(allocExecRes);

            return Arbiter.Choice(
                allocExecRes.Result,
                delegate(ExecutionAllocationResult result)
                {
                    _queue = result.TaskQueue;
                },
                delegate(Exception e)
                {
                    LogError(e);
                }
            );
        }

        IEnumerator<ITask> GetInitialState(Srv1CameraState initialState)
        {
            Srv1CameraState state = new Srv1CameraState();
            state.CaptureFile = initialState.CaptureFile;
            state.Quality = initialState.Quality;

            yield return InitializeInternalService();

            foreach (object obj in new vision.CameraCollection())
            {
                using (vision.Camera camera = obj as vision.Camera)
                {
                    CameraInstance instance = new CameraInstance();
                    instance.FriendlyName = camera.Name;
                    instance.DevicePath = camera.Path;
                    instance.SupportedFormats = ConvertFormats(camera.Formats);

                    state.Cameras.Add(instance);
                }
            }

            state.Image = null;

            Replace replace = new Replace();
            replace.Body = state;
            _fwdPort.Post(replace);

            yield return Arbiter.Choice(
                replace.ResponsePort,
                delegate(DefaultReplaceResponseType success) { },
                delegate(Fault fault)
                {
                    LogError(null, "Unable to set camera list", fault);
                }
            );

            UpdateDeviceRequest request = new UpdateDeviceRequest();
            if (initialState.Selected != null)
            {
                request.Selected = initialState.Selected;
            }
            else if (state.Cameras.Count > 0)
            {
                request.Selected = state.Cameras[0];
            }
            else
            {
                yield break;
            }
            UpdateDevice update = new UpdateDevice();
            update.Body = request;
            _fwdPort.Post(update);

            yield return Arbiter.Choice(
                update.ResponsePort,
                delegate(DefaultUpdateResponseType success) { },
                delegate(Fault fault)
                {
                    LogError(null, "Unable to select camera", fault);
                }
            );

            if (initialState.Selected == null ||
                initialState.Selected.Format == null)
            {
                yield break;
            }

            UpdateFormat updateFormat = new UpdateFormat();
            updateFormat.Body = initialState.Selected.Format;

            _fwdPort.Post(updateFormat);

            yield return Arbiter.Choice(
                updateFormat.ResponsePort,
                delegate(DefaultUpdateResponseType success) { },
                delegate(Fault fault)
                {
                    LogError(null, "Unable to select format", fault);
                }
            );
        }

        private List<Format> ConvertFormats(vision.Format[] formats)
        {
            List<Format> converted = new List<Format>(formats.Length);

            foreach(vision.Format format in formats)
            {
                converted.Add(new Format(format));
            }
            return converted;
        }

        void DropHandler(DsspDefaultDrop drop)
        {
            if (_state.Selected != null &&
                _state.Selected.FrameGrabber != null)
            {
                _state.Selected.FrameGrabber.Dispose();
            }
            base.DefaultDropHandler(drop);
        }

        IEnumerator<ITask> GetHandler(Get get)
        {
            get.ResponsePort.Post(_state);
            yield break;
        }

        IEnumerator<ITask> ReplaceHandler(Replace replace)
        {
            _state = replace.Body;
            replace.ResponsePort.Post(DefaultReplaceResponseType.Instance);

            SendNotification(_submgrPort, replace);

            if (_streamPort != null)
            {
                _streamPort.Post(new Shutdown());
                _streamPort = null;
            }

            if (!string.IsNullOrEmpty(_state.CaptureFile))
            {
                _streamPort = SaveStream.Create(_state.CaptureFile, _state.Quality, _queue);
            }

            yield break;
        }

        IEnumerator<ITask> QueryFrameHandler(QueryFrame query)
        {
            if (_state.Image == null)
            {
                query.ResponsePort.Post(new QueryFrameResponse());
                yield break;
            }

            if (query.Body.Format == Guid.Empty)
            {
                // raw image requested;
                BitmapData raw = null;
                try
                {
                    raw = _state.Image.LockBits(new Rectangle(Point.Empty, _state.Size),
                        ImageLockMode.ReadOnly, PixelFormat.Format24bppRgb);

                    int size = raw.Height * raw.Stride;

                    QueryFrameResponse response = new QueryFrameResponse();

                    response.TimeStamp = _state.TimeStamp;
                    response.Frame = new byte[size];
                    response.Size = new Size(raw.Width, raw.Height);
                    response.Format = Guid.Empty;

                    System.Runtime.InteropServices.Marshal.Copy(raw.Scan0, response.Frame, 0, size);

                    query.ResponsePort.Post(response);
                }
                finally
                {
                    if (raw != null)
                    {
                        _state.Image.UnlockBits(raw);
                    }
                }
            }
            else
            {
                ImageFormat format = new ImageFormat(query.Body.Format);

                using (MemoryStream stream = new MemoryStream())
                {
                    Size size = query.Body.Size;
                    if (size == _state.Image.Size ||
                        size.Width == 0 ||
                        size.Height == 0 ||
                        size.Width >= _state.Image.Width ||
                        size.Height >= _state.Image.Height)
                    {
                        size = _state.Image.Size;
                        _state.Image.Save(stream, format);
                    }
                    else
                    {
                        using (Bitmap temp = new Bitmap(
                            _state.Image, query.Body.Size))
                        {
                            temp.Save(stream, format);
                        }
                    }


                    QueryFrameResponse response = new QueryFrameResponse();
                    response.TimeStamp = _state.TimeStamp;
                    response.Frame = new byte[(int)stream.Length];
                    response.Size = size;
                    response.Format = format.Guid;

                    stream.Position = 0;
                    stream.Read(response.Frame, 0, response.Frame.Length);

                    query.ResponsePort.Post(response);
                }
            }
            yield break;
        }

        IEnumerator<ITask> UpdateFrameHandler(UpdateFrame update)
        {
            update.ResponsePort.Post(DefaultUpdateResponseType.Instance);

            SendNotification(_submgrPort, update);
            yield break;
        }

        IEnumerator<ITask> UpdateDeviceHandler(UpdateDevice update)
        {
            UpdateDeviceInternal(update);
            yield break;
        }

        void UpdateDeviceInternal(UpdateDevice update)
        {
            CameraInstance selected = update.Body.Selected;
            bool updated = false;

            if (_state.Selected != null &&
                _state.Selected.FrameGrabber != null)
            {
                _state.Selected.FrameGrabber.StopCapture();
                _state.Selected.FrameGrabber.Dispose();

                _state.Selected = null;
            }

            foreach (object obj in new vision.CameraCollection())
            {
                using (vision.Camera camera = obj as vision.Camera)
                {
                    if ((!string.IsNullOrEmpty(selected.FriendlyName) && camera.Name == selected.FriendlyName) ||
                        (!string.IsNullOrEmpty(selected.DevicePath) && camera.Path == selected.DevicePath))
                    {
                        selected.FriendlyName = camera.Name;
                        selected.DevicePath = selected.DevicePath;
                        selected.FrameGrabber = vision.FrameGrabber.FromCamera(camera);
                        selected.SupportedFormats = ConvertFormats(selected.FrameGrabber.Formats);
                        selected.FrameGrabber.CaptureFrame += OnCaptureFrame;
                        selected.FrameGrabber.StartCapture();
                        selected.Format = new Format(selected.FrameGrabber.Format);

                        updated = true;
                        break;
                    }
                }
            }
            if (updated)
            {
                _state.Selected = selected;

                update.ResponsePort.Post(DefaultUpdateResponseType.Instance);

                SendNotification(_submgrPort, update);
            }
            else
            {
                update.ResponsePort.Post(
                    Fault.FromCodeSubcodeReason(FaultCodes.Receiver,
                    DsspFaultCodes.UnknownEntry,
                    "Camera not found"
                    )
                );
            }
        }

        IEnumerator<ITask> UpdateFormatHandler(UpdateFormat update)
        {
            UpdateFormatInternal(update);
            yield break;
        }

        void UpdateFormatInternal(UpdateFormat update)
        {
            Format format = update.Body;
            
			if (_state.Selected != null &&
				_state.Selected.FrameGrabber != null)
            {
				vision.FrameGrabber grabber = _state.Selected.FrameGrabber;

				grabber.StopCapture();
                grabber.Format = (vision.Format)format;
                grabber.StartCapture();
                _state.Selected.Format = new Format(grabber.Format);
                update.ResponsePort.Post(DefaultUpdateResponseType.Instance);
            }
            else
            {
                update.ResponsePort.Post(
                    Fault.FromCodeSubcodeReason(FaultCodes.Receiver,
                    DsspFaultCodes.OperationFailed,
                    "No camera is currently selected")
                    );
            }
        }

        void OnCaptureFrame(object sender, vision.CaptureEventArgs e)
        {
            Frame frame = new Frame();
            frame.Image = e.Frame;
            frame.TimeStamp = e.SampleTime;

            _framePort.Post(frame);
        }

        void FrameHandler(Frame frame)
        {
            Frame previous = new Frame();
            previous.Image = _state.Image;
            previous.TimeStamp = _state.TimeStamp;

            _state.Image = frame.Image;
            _state.TimeStamp = frame.TimeStamp;

            if (previous.Image != null)
            {
                if (_streamPort != null)
                {
                    _streamPort.Post(previous);
                }
                else
                {
                    previous.Image.Dispose();
                }
            }

            UpdateFrameRequest request = new UpdateFrameRequest();
            request.TimeStamp = frame.TimeStamp;

            UpdateFrame update = new UpdateFrame();
            update.Body = request;
            update.ResponsePort = null;

            _fwdPort.Post(update);
        }

        IEnumerator<ITask> HttpPostHandler(HttpPost post)
        {
            Fault fault = null;
            NameValueCollection collection = null;
            
            ReadFormData readForm = new ReadFormData(post.Body.Context);
            _utilitiesPort.Post(readForm);

            yield return Arbiter.Choice(
                readForm.ResultPort,
                delegate(NameValueCollection col)
                {
                    collection = col;
                },
                delegate(Exception e)
                {
                    fault = Fault.FromException(e);
                    LogError(null, "Error processing form data", fault);
                }
            );

            if (fault != null)
            {
                post.ResponsePort.Post(fault);
                yield break;
            }

            if (!string.IsNullOrEmpty(collection["ChangeCamera"]))
            {
                string device = string.Empty;
                try
                {
                    device = collection["Camera"];
                }
                catch (Exception e)
                {
                    fault = Fault.FromException(e);
                    LogError(null, "Error reading form data", fault);
                }

                if (fault != null)
                {
                    post.ResponsePort.Post(fault);
                    yield break;
                }

                UpdateDeviceRequest request = new UpdateDeviceRequest();
                request.Selected.DevicePath = device;

                UpdateDevice update = new UpdateDevice();
                update.Body = request;

                UpdateDeviceInternal(update);

                yield return Arbiter.Choice(
                    update.ResponsePort,
                    delegate(DefaultUpdateResponseType success) { },
                    delegate(Fault f)
                    {
                        fault = f;
                        LogError(null, "Unable to change camera", fault);
                    }
                );
            }
            else if (!string.IsNullOrEmpty(collection["ChangeFormat"]))
            {
                int formatIndex = 0;
                Format format = null;
                try
                {
                    formatIndex = int.Parse(collection["CaptureFormat"]);
                    format = _state.Selected.SupportedFormats[formatIndex - 1];
                }
                catch (Exception e)
                {
                    fault = Fault.FromException(e);
                    LogError(null, "Error parsing form data", fault);
                }

                if (fault != null)
                {
                    post.ResponsePort.Post(fault);
                    yield break;
                }

                UpdateFormat update = new UpdateFormat();
                update.Body = format;

                UpdateFormatInternal(update);

                yield return Arbiter.Choice(
                    update.ResponsePort,
                    delegate(DefaultUpdateResponseType success) { },
                    delegate(Fault f)
                    {
                        fault = f;
                        LogError(null, "Unable to change format", fault);
                    }
                );
            }

            if (fault != null)
            {
                post.ResponsePort.Post(fault);
                yield break;
            }

            post.ResponsePort.Post(new HttpResponseType(HttpStatusCode.OK, _state, _transform));
            yield break;
        }

        IEnumerator<ITask> HttpGetHandler(HttpGet get)
        {
            HttpListenerRequest request = get.Body.Context.Request;
            HttpListenerResponse response = get.Body.Context.Response;

            string path = request.Url.AbsolutePath;
            string type;
            ImageFormat format;

            switch (path)
            {
                case "/camera/jpeg":
                case "/camera/jpg":
                    type = MediaTypeNames.Image.Jpeg;
                    format = ImageFormat.Jpeg;
                    break;
                case "/camera/bmp":
                    type = "image/bmp";
                    format = ImageFormat.Bmp;
                    break;
                case "/camera/png":
                    type = "image/png";
                    format = ImageFormat.Png;
                    break;
                case "/camera/tif":
                case "/camera/tiff":
                    type = "image/tiff";
                    format = ImageFormat.Tiff;
                    break;
                case "/camera/gif":
                    type = MediaTypeNames.Image.Gif;
                    format = ImageFormat.Gif;
                    break;
                case "/camera/live":
                    get.ResponsePort.Post(new HttpResponseType(HttpStatusCode.OK,_state,_transform));
                    yield break;

                default:
                    get.ResponsePort.Post(new HttpResponseType(_state));
                    yield break;
            }

            if (_state.Image == null)
            {
                get.ResponsePort.Post(new HttpResponseType(HttpStatusCode.NotFound, _state, _transform));
                yield break;
            }

            using (MemoryStream stream = new MemoryStream())
            {
                _state.Image.Save(stream, format);
                stream.Position = 0;

                get.Body.Context.Response.AddHeader("Cache-Control", "No-cache");

                WriteResponseFromStream write = new WriteResponseFromStream(
                    get.Body.Context, stream, type
                );

                _utilitiesPort.Post(write);

                yield return Arbiter.Choice(
                    write.ResultPort,
                    delegate(Stream res)
                    {
                        stream.Close();
                    },
                    delegate(Exception e)
                    {
                        stream.Close();
                        LogError(e);
                    }
                );
            }
        }

        IEnumerator<ITask> SubscribeHandler(Subscribe subscribe)
        {
            yield return Arbiter.Choice(
                SubscribeHelper(_submgrPort, subscribe.Body, subscribe.ResponsePort),
                delegate(SuccessResult success)
                {
                    LogInfo("Subscriptin from: " + subscribe.Body.Subscriber);
                },
                delegate(Exception e)
                {
                    LogError(e);
                }
            );
        }
    }

    class Frame
    {
        private Bitmap _image;

        public Bitmap Image
        {
            get { return _image; }
            set { _image = value; }
        }

        private DateTime _timeStamp;

        public DateTime TimeStamp
        {
            get { return _timeStamp; }
            set { _timeStamp = value; }
        }
    }

    class FramePort : Port<Frame> { }
}
