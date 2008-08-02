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
    /// <summary>
    /// See documentation for IAdapterFactory in IAdapter.cs
    /// </summary>
    public class WebcamAdapterFactory : IAdapterFactory
    {
        private List<string> supportedContracts = new List<string>()
        {
            webcam.Contract.Identifier,
        };

        /// <summary>
        /// See documentation for IAdapterFactory in IAdapter.cs
        /// </summary>
        public List<string> SupportedContracts { get { return supportedContracts; } }

        /// <summary>
        /// See documentation for IAdapterFactory in IAdapter.cs
        /// </summary>
        public IAdapter Create(ServiceInfoType service)
        {
            return new WebcamAdapter(service);
        }
    }

    /// <summary>
    /// See adapter documentation in the Myro 3 developer manual.
    /// http://wiki.roboteducation.org/Myro_3.0_Developer_Manual
    /// </summary>
    public class WebcamAdapter : IAdapter
    {
        /// <summary>
        /// See adapter documentation in the Myro 3 developer manual.
        /// http://wiki.roboteducation.org/Myro_3.0_Developer_Manual
        /// </summary>
        public ServiceInfoType ServiceInfo { get; private set; }

        webcam.WebCamOperations opPort;

        DispatcherQueue queue = new DispatcherQueue("WebcamAdapter", new Dispatcher(1, "WebcamAdapter"));

        /// <summary>
        /// See adapter documentation in the Myro 3 developer manual.
        /// http://wiki.roboteducation.org/Myro_3.0_Developer_Manual
        /// </summary>
        public WebcamAdapter(ServiceInfoType serviceInfo)
        {
            ServiceInfo = serviceInfo;
            opPort = DssEnvironment.ServiceForwarder<webcam.WebCamOperations>(new Uri(serviceInfo.Service));
        }

        /// <summary>
        /// See adapter documentation in the Myro 3 developer manual.
        /// http://wiki.roboteducation.org/Myro_3.0_Developer_Manual
        /// </summary>
        public void Dispose()
        {
            queue.Dispose();
        }

        /// <summary>
        /// See adapter documentation in the Myro 3 developer manual.
        /// http://wiki.roboteducation.org/Myro_3.0_Developer_Manual
        /// </summary>
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
