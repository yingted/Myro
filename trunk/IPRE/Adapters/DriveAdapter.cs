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
    /// <summary>
    /// See documentation for IAdapterFactory in IAdapter.cs
    /// </summary>
    public class DriveAdapterFactory : IAdapterFactory
    {
        #region IAdapterFactory Members

        private List<string> supportedContracts = new List<string>() { drive.Contract.Identifier };
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
            return new DriveAdapter(service);
        }

        #endregion
    }

    /// <summary>
    /// The adapter for MSRDS drive contracts.
    /// </summary>
    public class DriveAdapter : IAdapter
    {
        /// <summary>
        /// See adapter documentation in the Myro 3 developer manual.
        /// http://wiki.roboteducation.org/Myro_3.0_Developer_Manual
        /// </summary>
        /// <param name="serviceRecord"></param>
        public ServiceInfoType ServiceInfo { get; private set; }
        protected drive.DriveOperations drivePort;
        protected bool motorsOn;

        DispatcherQueue taskQueue = new DispatcherQueue("VectorAdapter", new Dispatcher(1, "VectorAdapter"));

        /// <summary>
        /// See adapter documentation in the Myro 3 developer manual.
        /// http://wiki.roboteducation.org/Myro_3.0_Developer_Manual
        /// </summary>
        /// <param name="serviceRecord"></param>
        public DriveAdapter(ServiceInfoType serviceRecord)
        {
            ServiceInfo = serviceRecord;
            Initialize();
        }

        /// <summary>
        /// See adapter documentation in the Myro 3 developer manual.
        /// http://wiki.roboteducation.org/Myro_3.0_Developer_Manual
        /// </summary>
        /// <param name="serviceRecord"></param>
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
        }

        /// <summary>
        /// Set the motor power.
        /// </summary>
        /// <param name="leftPower"></param>
        /// <param name="rightPower"></param>
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

        /// <summary>
        /// Stop the motors (sets power to 0)
        /// </summary>
        public void Stop()
        {
            RSUtils.ReceiveSync<DefaultUpdateResponseType>(taskQueue, drivePort.AllStop(), Myro.Utilities.Params.DefaultRecieveTimeout);
        }

        /// <summary>
        /// Return the MSRDS DriveDifferentialTwoWheelState of the service.
        /// </summary>
        /// <returns></returns>
        public drive.DriveDifferentialTwoWheelState Get()
        {
            return RSUtils.ReceiveSync<drive.DriveDifferentialTwoWheelState>(taskQueue, drivePort.Get(), Myro.Utilities.Params.DefaultRecieveTimeout);
        }
    }

}
