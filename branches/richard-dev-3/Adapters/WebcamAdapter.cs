// Copyright (c) Microsoft Corporation.  All rights reserved.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Ccr.Core;
using Microsoft.Dss.Hosting;
using webcam = Microsoft.Robotics.Services.WebCam.Proxy;
//using webcam = Myro.Services.Scribbler.FlukeCam.Proxy;
using Myro.Utilities;

namespace Myro.Adapters
{
    public class WebcamAdapterFactory : IAdapterFactory
    {
        public List<string> supportedContracts = new List<string>()
        {
            //"http://www.roboteducation.org/schemas/2008/06/flukecam.html"
            webcam.Contract.Identifier,
        };

        public List<string> SupportedContracts { get { return supportedContracts; } }

        public IAdapter Create(ServiceInfoType service)
        {
            return new WebcamAdapter(service);
        }
    }

    public class WebcamAdapter : IAdapter
    {
        public ServiceInfoType ServiceInfo { get; private set; }

        webcam.WebCamOperations opPort;

        DispatcherQueue queue = new DispatcherQueue("WebcamAdapter", new Dispatcher(1, "WebcamAdapter"));

        public WebcamAdapter(ServiceInfoType serviceInfo)
        {
            ServiceInfo = serviceInfo;
            opPort = DssEnvironment.ServiceForwarder<webcam.WebCamOperations>(new Uri(serviceInfo.Service));
        }

        public void Dispose()
        {
            queue.Dispose();
        }

        public void QueryFrame(MyroImageType type, out int Width, out int Height, out byte[] Image)
        {
            var r = RSUtils.ReceiveSync(queue,
                opPort.QueryFrame(new webcam.QueryFrameRequest() { Format = type.Guid }),
                Params.DefaultRecieveTimeout);
            Width = r.Size.Width;
            Height = r.Size.Height;
            Image = r.Frame;
        }
    }
}
