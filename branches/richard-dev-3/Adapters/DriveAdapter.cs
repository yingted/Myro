// Copyright (c) Microsoft Corporation.  All rights reserved.

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
        public List<string> SupportedContracts
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

        DispatcherQueue taskQueue = new DispatcherQueue("VectorAdapter", new Dispatcher(1, "VectorAdapter"));

        public DriveAdapter(ServiceInfoType serviceRecord)
        {
            ServiceInfo = serviceRecord;
            Initialize();
        }

        public void Dispose()
        {
            taskQueue.Dispose();
        }

        protected virtual void Initialize()
        {
            // Initialize the port and subscribe to the service to know when the motors are enabled
            motorsOn = false;
            drivePort = DssEnvironment.ServiceForwarder<drive.DriveOperations>(new Uri(ServiceInfo.Service));
            drive.DriveOperations driveNotificationPort = new drive.DriveOperations();
            RSUtils.ReceiveSync(taskQueue, drivePort.Subscribe(driveNotificationPort), Params.DefaultRecieveTimeout);

            // Set up notifications
            Arbiter.Activate(DssEnvironment.TaskQueue,
                Arbiter.Receive<drive.Update>(true, driveNotificationPort, NotifyDriveUpdate));

            // For some reason the first requests to the service are giving Faults, so get
            // it out of the way first
            try
            {
                SetMotors(0.0, 0.0);
            }
            catch (Exception)
            { }
        }

        public void SetMotors(double leftPower, double rightPower)
        {
            if (!motorsOn)
                EnableMotors();
            RSUtils.ReceiveSync(taskQueue, drivePort.SetDrivePower(leftPower, rightPower), Myro.Utilities.Params.DefaultRecieveTimeout);
        }

        protected void EnableMotors()
        {
            RSUtils.ReceiveSync(taskQueue, drivePort.EnableDrive(true), Params.DefaultRecieveTimeout);
        }

        protected void NotifyDriveUpdate(drive.Update notification)
        {
            motorsOn = notification.Body.IsEnabled;
        }

        public void Stop()
        {
            RSUtils.ReceiveSync<DefaultUpdateResponseType>(taskQueue, drivePort.AllStop(), Myro.Utilities.Params.DefaultRecieveTimeout);
        }

        public drive.DriveDifferentialTwoWheelState Get()
        {
            return RSUtils.ReceiveSync<drive.DriveDifferentialTwoWheelState>(taskQueue, drivePort.Get(), Myro.Utilities.Params.DefaultRecieveTimeout);
        }
    }

}
