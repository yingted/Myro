using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Ccr.Core;
using Microsoft.Dss.Hosting;
using Myro.Utilities;
using camcontrol = Myro.Services.Scribbler.FlukeCamControl.Proxy;
using b = Myro.Services.Scribbler.ScribblerBase;

namespace Myro.Adapters
{
    public class CamControlAdapterFactory : IAdapterFactory
    {
        private List<string> supportedContracts = new List<string>()
        {
            camcontrol.Contract.Identifier
        };
        public List<string> SupportedContracts
        {
            get { return supportedContracts; }
        }

        public IAdapter Create(ServiceInfoType service)
        {
            return new CamControlAdapter(service);
        }
    }

    public class CamControlAdapter : IAdapter
    {
        DispatcherQueue queue = new DispatcherQueue("CamControlAdapter queue", new Dispatcher(1, "CamControlAdapter queue"));
        camcontrol.CamControlOperations opPort;

        public Microsoft.Dss.ServiceModel.Dssp.ServiceInfoType ServiceInfo { get; internal set; }

        public CamControlAdapter(ServiceInfoType service)
        {
            this.ServiceInfo = service;
            opPort = DssEnvironment.ServiceForwarder<camcontrol.CamControlOperations>(new Uri(service.Service));
        }

        public void Dispose()
        {
            queue.Dispose();
        }

        public void DarkenCamera(byte level)
        {
            RSUtils.ReceiveSync(queue, opPort.Replace(new camcontrol.CamControlState()
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
            RSUtils.ReceiveSync(queue, opPort.Replace(new camcontrol.CamControlState()
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
    }
}
