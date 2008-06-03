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

using drive = Microsoft.Robotics.Services.Drive.Proxy;


namespace IPREFoundationClasses.Adapters
{
    public class DriveAdapter : MyroInterfaces.AbstractMovement, IAdapter
    {
        protected string serviceUri;
        protected drive.DriveOperations drivePort;
        protected bool motorsOn;

        public DriveAdapter(string serviceUri)
        {
            this.serviceUri = serviceUri;
            Initialize();
        }

        protected virtual void Initialize()
        {
            // Initialize the port and subscribe to the service
            motorsOn = false;
            drivePort = DssEnvironment.ServiceForwarder<drive.DriveOperations>(new Uri(serviceUri));
            drive.DriveOperations driveNotificationPort = new drive.DriveOperations();
            drivePort.Subscribe(driveNotificationPort);

            // Set up notifications
            Arbiter.Activate(DssEnvironment.TaskQueue,
                Arbiter.Receive<drive.Update>(true, driveNotificationPort, NotifyDriveUpdate)
                );
        }

        public string ServiceUri
        {
            get { return serviceUri; }
        }

        public AdapterTypes AdapterType
        {
            get { return AdapterTypes.DriveAdapter; }
        }

        public override void SetMotors(float leftPower, float rightPower)
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

        public override void SetMotorsFor(float leftPower, float rightPower, float seconds)
        {
            SetMotors(leftPower, rightPower);
            Thread.Sleep((int)(seconds * 1000));
            Stop();
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
                string truncUri = serviceUri.Substring(serviceUri.IndexOf('/', serviceUri.IndexOf("//") + 2));
                return String.Equals(truncUri, obj);
            }
            else
                return String.Equals(this.serviceUri, ((DriveAdapter)obj).serviceUri);
        }

        public override void Stop()
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
    }

}
