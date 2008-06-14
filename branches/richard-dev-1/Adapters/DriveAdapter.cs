using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Ccr.Core;
using Microsoft.Dss.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.Hosting;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Dss.ServiceModel.DsspServiceBase;
using System.Threading;
using W3C.Soap;
using Myro.Utilities;

using drive = Microsoft.Robotics.Services.Drive.Proxy;


namespace Myro.Adapters
{
    public class DriveAdapterFactory : IAdapterFactory
    {
        #region IAdapterFactory Members

        private List<string> supportedContracts = new List<string>() { drive.Contract.Identifier };
        public IList<string> SupportedContracts
        {
            get { return supportedContracts; }
        }

        public IAdapter Create(ServiceInfoType service)
        {
            return new DriveAdapter(service);
        }

        #endregion
    }

    public class DriveAdapter : IAdapter
    {
        public ServiceInfoType ServiceInfo { get; private set; }
        protected drive.DriveOperations drivePort;
        protected bool motorsOn;

        public DriveAdapter(ServiceInfoType serviceRecord)
        {
            ServiceInfo = serviceRecord;
            Initialize();
        }

        protected virtual void Initialize()
        {
            // Initialize the port and subscribe to the service
            motorsOn = false;
            drivePort = DssEnvironment.ServiceForwarder<drive.DriveOperations>(new Uri(ServiceInfo.Service));
            drive.DriveOperations driveNotificationPort = new drive.DriveOperations();
            drivePort.Subscribe(driveNotificationPort);

            // Set up notifications
            Arbiter.Activate(DssEnvironment.TaskQueue,
                Arbiter.Receive<drive.Update>(true, driveNotificationPort, NotifyDriveUpdate)
                );
        }

        public void SetMotors(float leftPower, float rightPower)
        {
            if (!motorsOn)
                EnableMotors();

            drive.SetDrivePowerRequest drivePowerReq = new drive.SetDrivePowerRequest();
            drivePowerReq.LeftWheelPower = leftPower;
            drivePowerReq.RightWheelPower = rightPower;
            drive.SetDrivePower setDrivePower = new drive.SetDrivePower(drivePowerReq);
            drivePort.Post(setDrivePower);

            ManualResetEvent signal = new ManualResetEvent(false);
            Arbiter.Activate(DssEnvironment.TaskQueue,
                Arbiter.Choice<DefaultUpdateResponseType, Fault>(
                    drivePort.SetDrivePower((double)leftPower, (double)rightPower),
                    delegate(DefaultUpdateResponseType state)
                    {
                        signal.Set();
                    },
                    delegate(Fault failure)
                    {
                        Console.WriteLine("*** Fault in SetMotors: ");
                        foreach (var r in failure.Reason)
                            Console.WriteLine("***    " + r.Value);
                        signal.Set();
                    }));
            signal.WaitOne();
        }

        protected void EnableMotors()
        {
            drive.EnableDriveRequest enableDriveMessage = new drive.EnableDriveRequest();
            enableDriveMessage.Enable = true;
            drivePort.EnableDrive(enableDriveMessage);
            Console.WriteLine("Enabling motors");
        }

        protected void NotifyDriveUpdate(drive.Update notification)
        {
            motorsOn = notification.Body.IsEnabled;
        }

        //public override bool Equals(Object obj)
        //{
        //    if (obj is String)
        //    {
        //        string truncUri = ServiceInfo.Service.Substring(ServiceInfo.Service.IndexOf('/', ServiceInfo.Service.IndexOf("//") + 2));
        //        return String.Equals(truncUri, obj);
        //    }
        //    else
        //        return String.Equals(this.ServiceInfo.Service, ((DriveAdapter)obj).ServiceInfo.Service);
        //}

        public void Stop()
        {
            RSUtils.RecieveSync<DefaultUpdateResponseType>(drivePort.AllStop(), Myro.Utilities.Params.defaultRecieveTimeout);
        }

        public drive.DriveDifferentialTwoWheelState Get()
        {
            return RSUtils.RecieveSync<drive.DriveDifferentialTwoWheelState>(drivePort.Get(), Myro.Utilities.Params.defaultRecieveTimeout);
        }

        public void Set(drive.DriveDifferentialTwoWheelState state)
        {
            throw new NotSupportedException("Setting the entire state is not supported on a drive.  Try the other forms of \"set\".");
        }
    }

}
