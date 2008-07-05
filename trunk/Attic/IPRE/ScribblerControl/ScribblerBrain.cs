using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;

using MyroInterfaces;

using Microsoft.Ccr.Core;
using Microsoft.Dss.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.Hosting;
using W3C.Soap;
using Microsoft.Dss.ServiceModel.Dssp;

using drive = Microsoft.Robotics.Services.Drive.Proxy;
using directory = Microsoft.Dss.Services.Directory;

namespace ScribblerControl
{
    class ScribblerBrain : AbstractMovement
    {
        private drive.DriveOperations drivePort;
        private bool motorsOn;
        private AutoResetEvent autolock = new AutoResetEvent(false);
        private int attemptCount = 0;
        private Port<DateTime> _timerPort = new Port<DateTime>();

        public ScribblerBrain()
        {
            Initialize();
        }

        ~ScribblerBrain()
        {
            DssEnvironment.Shutdown();
        }

        private void Initialize()
        {
            // Initialize Dss using the Scribbler base manifest
            DssEnvironment.Initialize(50000, 50001,
                @"C:\\Microsoft Robotics Studio (1.5)\\samples\\IPRE\\Scribbler\\Scribbler\\Batch Files\\IPRE.Scribbler.standard.manifest.xml"
                );

            Arbiter.Receive<DateTime>(true, _timerPort, ConnectToScribbler);
            _timerPort.Post(DateTime.Now);

            autolock.WaitOne();
        }

        void ConnectToScribbler(DateTime dt)
        {
            Uri driveUri = null;

            // Look for any active generic drive contract service running
            Arbiter.Activate(DssEnvironment.TaskQueue,
                Arbiter.Choice(DssEnvironment.DirectoryQuery(drive.Contract.Identifier),
                    delegate(ServiceInfoType success)
                    {
                        driveUri = new Uri(success.Service);
                        Console.WriteLine("================\n" + driveUri);

                        // Initialize the port and subscribe to the service
                        motorsOn = false;
                        drivePort = DssEnvironment.ServiceForwarder<drive.DriveOperations>(driveUri);
                        drive.DriveOperations driveNotificationPort = new drive.DriveOperations();
                        drivePort.Subscribe(driveNotificationPort);

                        // Set up notifications
                        Arbiter.Activate(DssEnvironment.TaskQueue,
                            Arbiter.Receive<drive.Update>(true, driveNotificationPort, NotifyDriveUpdate)
                            );

                        autolock.Set();
                    },
                    delegate(W3C.Soap.Fault failure)
                    {
                        // Request failed. Sleep for 1 sec and look for 30 times.
                        if (++attemptCount >= 30)
                        {
                            DssEnvironment.LogError("Unable to find Drive Service, aborting - Press <Enter>");
                            Console.ReadLine();
                            DssEnvironment.Shutdown();
                        }
                        else
                        {
                            // Post a timer message that expires in 30 seconds
                            TimeSpan timeout = new TimeSpan(0, 0, 30);
                            DssEnvironment.TaskQueue.EnqueueTimer(timeout, _timerPort);
                        }
                    }
            ));

        }

        public override void SetMotors(float leftPower, float rightPower)
        {
            if (!motorsOn)
                EnableMotors();

            drive.SetDrivePowerRequest drivePowerReq = new drive.SetDrivePowerRequest();
            drivePowerReq.LeftWheelPower = leftPower;
            drivePowerReq.RightWheelPower = rightPower;
            drivePort.SetDrivePower(drivePowerReq);
        }

        public override void SetMotorsFor(float leftPower, float rightPower, float seconds)
        {
            SetMotors(leftPower, rightPower);
            Thread.Sleep((int)(seconds * 1000));
            Stop();
        }

        private void EnableMotors()
        {
            drive.EnableDriveRequest enableDriveMessage = new drive.EnableDriveRequest();
            enableDriveMessage.Enable = true;
            drivePort.EnableDrive(enableDriveMessage);
        }

        private void NotifyDriveUpdate(drive.Update notification)
        {
            motorsOn = notification.Body.IsEnabled;
        }
    }
}