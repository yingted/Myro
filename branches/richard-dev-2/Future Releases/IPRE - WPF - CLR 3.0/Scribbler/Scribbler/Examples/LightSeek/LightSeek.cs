//-----------------------------------------------------------------------
//  IPRE
//  
//-----------------------------------------------------------------------

using Microsoft.Ccr.Core;
using Microsoft.Dss.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Dss.ServiceModel.DsspServiceBase;
using System;
using System.Collections.Generic;
using System.Security.Permissions;
using System.ComponentModel;

using drive = Microsoft.Robotics.Services.Drive.Proxy;
using motor = Microsoft.Robotics.Services.Motor.Proxy;
using light = IPRE.ScribblerLightSensor.Proxy;


namespace IPRE.LightSeek
{
    [DisplayName("Light Seek")]
    [Description("Scribbler Light Seek Service")]
    [Contract(Contract.Identifier)]
    public class LightSeekService : DsspServiceBase
    {

        private LightSeekState _state = new LightSeekState();

        [Partner("Drive",
            Contract = drive.Contract.Identifier,
            CreationPolicy = PartnerCreationPolicy.UseExisting)]
        drive.DriveOperations _drivePort = new drive.DriveOperations();

        [Partner("Light",
            Contract = light.Contract.Identifier,
            CreationPolicy = PartnerCreationPolicy.UseExistingOrCreate)]
        light.ScribblerLightSensorOperations _lightPort = new light.ScribblerLightSensorOperations();

        [ServicePort("/LightSeek", AllowMultipleInstances=false)]
        private LightSeekOperations _mainPort = new LightSeekOperations();

        private Port<DateTime> _timerPort = new Port<DateTime>();
        private int frequency = 250; //how many milliseconds between light sensor readings

        public LightSeekService(DsspServiceCreationPort creationPort) : 
                base(creationPort)
        {

        }

        protected override void Start()
        {

            // Listen on the main port for requests and call the appropriate handler.
            ActivateDsspOperationHandlers();

            // Publish the service to the local Node Directory
            DirectoryInsert();

			// display HTTP service Uri
			LogInfo(LogGroups.Console, "Service uri: ");

            _timerPort.Post(DateTime.Now);
            Activate(Arbiter.Receive(true, _timerPort, TimerHandler));
        }


        void TimerHandler(DateTime signal)
        {
            SpawnIterator(CheckLight);

            //re-call ourselves
            Activate(
                Arbiter.Receive(false, TimeoutPort(frequency),
                    delegate(DateTime time)
                    {
                        _timerPort.Post(time);
                    }
                )
            );
        }


        IEnumerator<ITask> CheckLight()
        {
            int lefteye = 0;
            int centereye = 0;
            int righteye = 0;
            double leftpower, rightpower;

            //first get light readings
            yield return Arbiter.Receive<light.ScribblerLightSensorState>(false, _lightPort.Get(),
                delegate(light.ScribblerLightSensorState reading)
                {
                    lefteye = (int)reading.LeftSensor.RawMeasurement;
                    centereye = (int)reading.CenterSensor.RawMeasurement;
                    righteye = (int)reading.RightSensor.RawMeasurement;
                }
            );

            //do 'smarts'
            
            //not enough variation to turn
            if (Math.Max(Math.Max(lefteye, centereye), righteye) - Math.Min(Math.Min(lefteye, centereye), righteye) < 25)
            {
                leftpower = 0.9;
                rightpower = 0.9;
                _state.CurrentAction = "Driving Straight";
            }
            else if (Math.Min(Math.Min(lefteye, centereye), righteye) == lefteye) //bright light to left
            {
                leftpower = 0.3;
                rightpower = 1.0;
                _state.CurrentAction = "Turning Left";
            }
            else if (Math.Min(Math.Min(lefteye, centereye), righteye) == righteye) //bright light to right
            {
                leftpower = 1.0;
                rightpower = 0.3;
                _state.CurrentAction = "Turning Right";
            }
            else //bright light to center
            {
                leftpower = 0.9;
                rightpower = 0.9;
                _state.CurrentAction = "Driving Straight";
            }

            // send motor messages
            drive.SetDrivePowerRequest setPower = new drive.SetDrivePowerRequest(leftpower, rightpower);
            _drivePort.SetDrivePower(setPower);

            // done
            yield break;
        }


       
    }
}
