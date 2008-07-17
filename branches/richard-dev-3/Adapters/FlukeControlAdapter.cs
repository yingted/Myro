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
    public class FlukeControlAdapterFactory : IAdapterFactory
    {
        private List<string> supportedContracts = new List<string>()
        {
            fluke.Contract.Identifier
        };
        public List<string> SupportedContracts
        {
            get { return supportedContracts; }
        }

        public IAdapter Create(ServiceInfoType service)
        {
            return new FlukeControlAdapter(service);
        }
    }

    public class FlukeControlAdapter : IAdapter
    {
        DispatcherQueue queue = new DispatcherQueue("FlukeControlAdapter queue", new Dispatcher(1, "FlukeControlAdapter queue"));
        fluke.CamControlOperations opPort;

        public Microsoft.Dss.ServiceModel.Dssp.ServiceInfoType ServiceInfo { get; internal set; }

        public FlukeControlAdapter(ServiceInfoType service)
        {
            this.ServiceInfo = service;
            opPort = DssEnvironment.ServiceForwarder<fluke.CamControlOperations>(new Uri(service.Service));
        }

        public void Dispose()
        {
            queue.Dispose();
        }

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

        public string GetName()
        {
            return RSUtils.ReceiveSync(queue, opPort.GetName(), Params.DefaultRecieveTimeout).Value;
        }

        public void SetName(string name)
        {
            RSUtils.ReceiveSync(queue, opPort.SetName(name), Params.DefaultRecieveTimeout);
        }

        public void SetIRPower(byte power)
        {
            RSUtils.ReceiveSync(queue, opPort.SetIRPower(power), Params.DefaultRecieveTimeout);
        }
    }
}
