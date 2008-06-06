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

            bool done = false;
            Arbiter.Activate(DssEnvironment.TaskQueue,
                Arbiter.Receive<DefaultUpdateResponseType>(false,
                    setDrivePower.ResponsePort,
                    delegate(DefaultUpdateResponseType state)
                    {
                        done = true;
                    }
            ));

            while (!done) ;
        }

        protected void EnableMotors()
        {
            drive.EnableDriveRequest enableDriveMessage = new drive.EnableDriveRequest();
            enableDriveMessage.Enable = true;
            drivePort.EnableDrive(enableDriveMessage);
        }

        protected void NotifyDriveUpdate(drive.Update notification)
        {
            motorsOn = notification.Body.IsEnabled;
        }

        public override bool Equals(Object obj)
        {
            if (obj is String)
            {
                string truncUri = ServiceInfo.Service.Substring(ServiceInfo.Service.IndexOf('/', ServiceInfo.Service.IndexOf("//") + 2));
                return String.Equals(truncUri, obj);
            }
            else
                return String.Equals(this.ServiceInfo.Service, ((DriveAdapter)obj).ServiceInfo.Service);
        }

        public void Stop()
        {
            drive.AllStopRequest allStopReq = new drive.AllStopRequest();
            drive.AllStop allStop = new drive.AllStop(allStopReq);
            drivePort.Post(allStop);

            bool done = false;
            Arbiter.Activate(DssEnvironment.TaskQueue,
                Arbiter.Receive<DefaultUpdateResponseType>(false,
                    allStop.ResponsePort,
                    delegate(DefaultUpdateResponseType state)
                    {
                        done = true;
                    }
            ));

            while (!done) ;
        }

        public drive.DriveDifferentialTwoWheelState Get()
        {
            drive.DriveDifferentialTwoWheelState ret = null;
            Fault error = null;
            Signal signal = new Signal();
            Arbiter.Activate(DssEnvironment.TaskQueue,
                Arbiter.Choice<drive.DriveDifferentialTwoWheelState, Fault>(
                    drivePort.Get(),
                    delegate(drive.DriveDifferentialTwoWheelState state)
                    {
                        ret = state;
                        signal.Raise();
                    },
                    delegate(Fault failure)
                    {
                        error = failure;
                        signal.Raise();
                    }));
            signal.Wait();
            if (error != null)
                throw new AdapterOperationException(error);
            else
                return ret;
        }

        public void Set(drive.DriveDifferentialTwoWheelState state)
        {
            throw new NotSupportedException("Setting the entire state is not supported on a drive.  Try the other forms of \"set\".");
        }
    }

}
