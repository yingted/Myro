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

namespace IPRE.DrawSquare
{
    [DisplayName("Draw Square")]
    [Description("Scribbler Draw Square Service")]
    [Contract(Contract.Identifier)]
    public class DrawSquareService : DsspServiceBase
    {

        private DrawSquareState _state = new DrawSquareState();

        [Partner("Drive",
            Contract = drive.Contract.Identifier,
            CreationPolicy = PartnerCreationPolicy.UseExisting)]
        drive.DriveOperations _drivePort = new drive.DriveOperations();


        [ServicePort("/DrawSquare", AllowMultipleInstances=false)]
        private DrawSquareOperations _mainPort = new DrawSquareOperations();

        public DrawSquareService(DsspServiceCreationPort creationPort) : 
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

            SpawnIterator(DrawIt);
        }


        IEnumerator<ITask> DrawIt()
        {
            //drawing parameters
            int DriveStraightTime = 1000;
            double DriveStraightPower = 1.0;

            int TurnTime = 750;
            double TurnPowerOut = 1.0;
            double TurnPowerIn = 0.0;



            //wait some time before starting
            _state.CurrentAction = "Waiting";
            Console.WriteLine(_state.CurrentAction);
            System.Threading.Thread.Sleep(1000);

            for (int i = 0; i < 4; i++)
            {
                // set motors straight
                _state.CurrentAction = "Driving Straight, edge " + i;
                Console.WriteLine(_state.CurrentAction);
                _drivePort.SetDrivePower(new drive.SetDrivePowerRequest(DriveStraightPower, DriveStraightPower));

                //wait straight time
                System.Threading.Thread.Sleep(DriveStraightTime);

                // set motors turning
                _state.CurrentAction = "Turning, corner " + i;
                Console.WriteLine(_state.CurrentAction);

                _drivePort.SetDrivePower(new drive.SetDrivePowerRequest(TurnPowerIn, TurnPowerOut));

                //wait turn time
                System.Threading.Thread.Sleep(TurnTime);
            }

            // done
            yield break;
        }

        
        /// <summary>
        /// Get Handler
        /// </summary>
        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public virtual IEnumerator<ITask> GetHandler(Get get)
        {
            SpawnIterator(DrawIt);

            get.ResponsePort.Post(_state);
            yield break;
        }


       
    }
}
