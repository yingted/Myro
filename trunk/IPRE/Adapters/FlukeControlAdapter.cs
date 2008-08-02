// Copyright (c) Microsoft Corporation.  All rights reserved.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Ccr.Core;
using Microsoft.Dss.Hosting;
using Myro.Utilities;
using fluke = Myro.Services.Scribbler.FlukeControl.Proxy;
using b = Myro.Services.Scribbler.ScribblerBase;

namespace Myro.Adapters
{
    /// <summary>
    /// See documentation for IAdapterFactory in IAdapter.cs
    /// </summary>
    public class FlukeControlAdapterFactory : IAdapterFactory
    {
        private List<string> supportedContracts = new List<string>()
        {
            fluke.Contract.Identifier
        };
        /// <summary>
        /// See documentation for IAdapterFactory in IAdapter.cs
        /// </summary>
        public List<string> SupportedContracts
        {
            get { return supportedContracts; }
        }

        /// <summary>
        /// See documentation for IAdapterFactory in IAdapter.cs
        /// </summary>
        public IAdapter Create(ServiceInfoType service)
        {
            return new FlukeControlAdapter(service);
        }
    }

    /// <summary>
    /// See adapter documentation in the Myro 3 developer manual.
    /// http://wiki.roboteducation.org/Myro_3.0_Developer_Manual
    /// </summary>
    /// <param name="serviceRecord"></param>
    public class FlukeControlAdapter : IAdapter
    {
        DispatcherQueue queue = new DispatcherQueue("FlukeControlAdapter queue", new Dispatcher(1, "FlukeControlAdapter queue"));
        fluke.CamControlOperations opPort;

        /// <summary>
        /// See adapter documentation in the Myro 3 developer manual.
        /// http://wiki.roboteducation.org/Myro_3.0_Developer_Manual
        /// </summary>
        /// <param name="serviceRecord"></param>
        public Microsoft.Dss.ServiceModel.Dssp.ServiceInfoType ServiceInfo { get; internal set; }

        /// <summary>
        /// See adapter documentation in the Myro 3 developer manual.
        /// http://wiki.roboteducation.org/Myro_3.0_Developer_Manual
        /// </summary>
        /// <param name="serviceRecord"></param>
        public FlukeControlAdapter(ServiceInfoType service)
        {
            this.ServiceInfo = service;
            opPort = DssEnvironment.ServiceForwarder<fluke.CamControlOperations>(new Uri(service.Service));
        }

        /// <summary>
        /// See adapter documentation in the Myro 3 developer manual.
        /// http://wiki.roboteducation.org/Myro_3.0_Developer_Manual
        /// </summary>
        /// <param name="serviceRecord"></param>
        public void Dispose()
        {
            queue.Dispose();
        }

        /// <summary>
        /// "Darken" the camera, so that it only sees bright lights.  This
        /// turns off automatic exposure, gain, and white balance.
        /// </summary>
        /// <param name="level"></param>
        public void DarkenCamera(byte level)
        {
            RSUtils.ReceiveSync(queue, opPort.SetCamera(new fluke.CamControlState()
            {
                Darkness = level,
                Val1 = 0,
                Val2 = 0,
                Brightness = 0,
                Exposure = 0,
                AutoExposure = false,
                AutoGain = false,
                AutoWhiteBalance = false
            }), Params.DefaultRecieveTimeout);
        }

        /// <summary>
        /// Return the camera to automatic mode, auto exposure, gain, and WB.
        /// </summary>
        public void AutoCamera()
        {
            RSUtils.ReceiveSync(queue, opPort.SetCamera(new fluke.CamControlState()
            {
                Darkness = 0,
                Val1 = 0x80,
                Val2 = 0x80,
                Brightness = b.ScribblerHelper.CameraParams.CAM_BRT_DEFAULT,
                Exposure = b.ScribblerHelper.CameraParams.CAM_EXP_DEFAULT,
                AutoExposure = true,
                AutoGain = true,
                AutoWhiteBalance = true
            }), Params.DefaultRecieveTimeout);
        }

        /// <summary>
        /// Return the robot name.
        /// </summary>
        /// <returns></returns>
        public string GetName()
        {
            return RSUtils.ReceiveSync(queue, opPort.GetName(), Params.DefaultRecieveTimeout).Value;
        }

        /// <summary>
        /// Modify the robot name.
        /// </summary>
        /// <param name="name"></param>
        public void SetName(string name)
        {
            RSUtils.ReceiveSync(queue, opPort.SetName(name), Params.DefaultRecieveTimeout);
        }

        /// <summary>
        /// Set the fluke IR obstacle sensor power.
        /// </summary>
        /// <param name="power"></param>
        public void SetIRPower(byte power)
        {
            RSUtils.ReceiveSync(queue, opPort.SetIRPower(power), Params.DefaultRecieveTimeout);
        }
    }
}
