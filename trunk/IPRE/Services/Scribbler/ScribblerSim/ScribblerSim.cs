// Copyright (c) Microsoft Corporation.  All rights reserved.

//------------------------------------------------------------------------------
// ScribblerSim.cs
//
//
//------------------------------------------------------------------------------
using Microsoft.Ccr.Core;
using Microsoft.Dss.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Dss.ServiceModel.DsspServiceBase;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Security.Permissions;
using xml = System.Xml;
using System.Threading;

#region Simulation namespaces
using Microsoft.Robotics.Simulation;
using Microsoft.Robotics.Simulation.Engine;
using engineproxy = Microsoft.Robotics.Simulation.Engine.Proxy;
using Microsoft.Robotics.Simulation.Physics;

using drive = Microsoft.Robotics.Services.Simulation.Drive.Proxy;
using bumper = Microsoft.Robotics.Services.Simulation.Sensors.Bumper.Proxy;
using simwebcam = Microsoft.Robotics.Services.Simulation.Sensors.SimulatedWebcam.Proxy;

using Microsoft.Robotics.PhysicalModel;
using System.ComponentModel;
using Microsoft.Dss.Core.DsspHttp;
using System.Net;

#endregion

namespace Myro.Services.ScribblerSim
{
    
    [DisplayName("ScribblerSim")]
    [Description("The ScribblerSim Service")]
    [Contract(Contract.Identifier)]

    public class ScribblerSimService : DsspServiceBase
    {
        private ScribblerSimState _state = new ScribblerSimState();

        // partner attribute will cause simulation engine service to start
        [Partner("Engine",
            Contract = engineproxy.Contract.Identifier,
            CreationPolicy = PartnerCreationPolicy.UseExistingOrCreate)]
        private engineproxy.SimulationEnginePort _engineServicePort =
            new engineproxy.SimulationEnginePort();

        [ServicePort("/scribblersim", AllowMultipleInstances=false)]
        private ScribblerSimOperations _mainPort = new ScribblerSimOperations();
        /// <summary>
        /// Default Service Constructor
        /// </summary>
        
        public ScribblerSimService(DsspServiceCreationPort creationPort) : 
                base(creationPort)
        {

        }

        /// <summary>
        /// Service Start
        /// </summary>
        protected override void Start()
        {
            base.Start();

            

            _state.Cubes = 0;

            // orient sim camera view point
            SetupCamera();

            // Add objects (entities) in our simulated world
            PopulateWorld();
        }

        private void SetupCamera()
        {
            // Set up initial view
            CameraView view = new CameraView();
            view.EyePosition = new Vector3(0f, 6f, 4f);
            view.LookAtPoint = new Vector3(0f, -8f, -4.5f);
            SimulationEngine.GlobalInstancePort.Update(view);
        }

        private void PopulateWorld()
        {
            AddSky();
            AddGround();
            AddCube(new Vector3(1, 0.5f, -1));
            AddScribblerRobot(new Vector3(0, 0, -0.25f));
            AddArena();
        }

        

        void AddScribblerRobot(Vector3 position)
        {
            ScribblerRobot robotBaseEntity = CreateScribblerMotorBase(ref position);

            // Create bumper array entity and start simulated bumper service
            BumperArrayEntity bumperArray = CreateBumperArray();
            // insert as child of motor base
            robotBaseEntity.InsertEntity(bumperArray);

            // create Camera Entity ans start SimulatedWebcam service
            CameraEntity camera = CreateCamera();
            // insert as child of motor base
            robotBaseEntity.InsertEntity(camera);

            // Finaly insert the motor base and its two children 
            // to the simulation
            SimulationEngine.GlobalInstancePort.Insert(robotBaseEntity);
        }

        private ScribblerRobot CreateScribblerMotorBase(ref Vector3 position)
        {
            // use supplied entity that creates a motor base 
            // with 2 active wheels and one caster
            ScribblerRobot robotBaseEntity = new ScribblerRobot(position);

            // specify mesh. 
            robotBaseEntity.State.Assets.Mesh = "Scribbler.bos";

            // the name below must match manifest
            robotBaseEntity.State.Name = "ScribblerRobot";  //"\ScribblerRobot";

            // Start simulated motor service
            //drive.Contract.CreateService(ConstructorPort,
            //    Microsoft.Robotics.Simulation.Partners.CreateEntityPartner(
            //        "http://localhost/" + robotBaseEntity.State.Name)
            //);
            return robotBaseEntity;
        }

        private CameraEntity CreateCamera()
        {
            // low resolution, wide Field of View
            CameraEntity cam = new CameraEntity(320, 240, ((float)Math.PI * 0.4f));
            cam.State.Name = "ScribblerCamera";
            // just on top of the bot
            cam.State.Pose.Position = new Vector3(0.0f, 0.127f, 0.0f);
            // camera renders in an offline buffer at each frame
            // required for service
            cam.IsRealTimeCamera = true;

            // Start simulated webcam service
            //simwebcam.Contract.CreateService(
            //    ConstructorPort,
            //    Microsoft.Robotics.Simulation.Partners.CreateEntityPartner(
            //        "http://localhost/" + cam.State.Name)
            //);

            return cam;
        }


        private BumperArrayEntity CreateBumperArray()
        {
            // Create a bumper array entity with two bumpers
            BoxShape leftBumper = new BoxShape(
                new BoxShapeProperties("front left",
                    0.001f,
                    new Pose(new Vector3(-0.036f, 0.03f, -0.09f)),
                    new Vector3(0.07f, 0.03f, 0.06f)
                )
            );
            leftBumper.State.DiffuseColor = new Vector4(0.1f, 0.1f, 0.1f, 1.0f);

            BoxShape rightBumper = new BoxShape(
                new BoxShapeProperties("front right",
                    0.001f,
                    new Pose(new Vector3(0.036f, 0.03f, -0.09f)),
                    new Vector3(0.07f, 0.03f, 0.06f)
                )
            );
            rightBumper.State.DiffuseColor = new Vector4(0.1f, 0.1f, 0.1f, 1.0f);

            // The physics engine will issue contact notifications only
            // if we enable them per shape
            leftBumper.State.EnableContactNotifications = true;
            rightBumper.State.EnableContactNotifications = true;

            // put some force filtering so we only get notified for significant bumps
            //frontBumper.State.ContactFilter = new ContactNotificationFilter(1,1);
            //rearBumper.State.ContactFilter = new ContactNotificationFilter(1, 1);

            BumperArrayEntity
                bumperArray = new BumperArrayEntity(leftBumper, rightBumper);
            // entity name, must match manifest partner name
            bumperArray.State.Name = "ScribblerBumpers";

            // start simulated bumper service

            //bumper.Contract.CreateService(
            //    ConstructorPort,
            //    Microsoft.Robotics.Simulation.Partners.CreateEntityPartner(
            //    "http://localhost/" + bumperArray.State.Name));

            return bumperArray;
        }


        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public virtual IEnumerator<ITask> GetHandler(Get get)
        {
            Replace replace = new Replace();
            replace.Body = _state;
            replace.Body.Cubes++;
            _mainPort.Post(replace);

            get.ResponsePort.Post(_state);
            yield break;
        }

        [ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        public IEnumerator<ITask> ReplaceHandler(Replace replace)
        {
            AddCube(new Vector3(0, 3f, 0)); //just for fun, drop more cubes

            _state = replace.Body;
            replace.ResponsePort.Post(new DefaultReplaceResponseType());
            yield break;
        }

        #region Environment Entities

        void AddSky()
        {
            // Add a sky using a static texture. We will use the sky texture
            // to do per pixel lighting on each simulation visual entity
            SkyEntity sky = new SkyEntity("sky.dds", "sky_diff.dds");
            SimulationEngine.GlobalInstancePort.Insert(sky);
        }

        void AddGround()
        {
            HeightFieldShapeProperties hf = new HeightFieldShapeProperties("height field",
                64, // number of rows 
                100, // distance in meters, between rows
                64, // number of columns
                100, // distance in meters, between columns
                1, // scale factor to multiple height values 
                -1000); // vertical extent of the height field. Should be set to large negative values

            // create array with height samples
            hf.HeightSamples = new HeightFieldSample[hf.RowCount * hf.ColumnCount];
            for (int i = 0; i < hf.RowCount * hf.ColumnCount; i++)
            {
                hf.HeightSamples[i] = new HeightFieldSample();
                hf.HeightSamples[i].Height = (short)(Math.Sin(i * 0.01));
            }

            // create a material for the entire field. We could also specify material per sample.
            hf.Material = new MaterialProperties("ground", 0.8f, 0.5f, 0.8f);

            // insert ground entity in simulation and specify a texture
            SimulationEngine.GlobalInstancePort.Insert(new HeightFieldEntity(hf, "03RamieSc.dds"));
        }

        private void AddCube(Vector3 position)
        {
            BoxShapeProperties cBoxShape = null;
            SingleShapeEntity cBoxEntity = null;
            cBoxShape = new BoxShapeProperties(2.0f, new Pose(), new Vector3(0.5f, 0.5f, 0.5f));
            cBoxEntity = new SingleShapeEntity(new BoxShape(cBoxShape), position);
            cBoxEntity.State.Name = "cube:" + Guid.NewGuid().ToString();
            SimulationEngine.GlobalInstancePort.Insert(cBoxEntity);
        }

        private void AddArena()
        {
            BoxShapeProperties tBoxShape = null;
            SingleShapeEntity tBoxEntity = null;
            tBoxShape = new BoxShapeProperties(500f, new Pose(), new Vector3(5f, 0.25f, 0.25f));
            tBoxEntity = new SingleShapeEntity(new BoxShape(tBoxShape), new Vector3(0f, 0.5f, 2.63f));
            tBoxEntity.State.Name = "arena top";
            SimulationEngine.GlobalInstancePort.Insert(tBoxEntity);

            BoxShapeProperties lBoxShape = null;
            SingleShapeEntity lBoxEntity = null;
            lBoxShape = new BoxShapeProperties(500f, new Pose(), new Vector3(0.25f, 0.25f, 5f));
            lBoxEntity = new SingleShapeEntity(new BoxShape(lBoxShape), new Vector3(-2.63f, 0.5f, 0f));
            lBoxEntity.State.Name = "arena left";
            SimulationEngine.GlobalInstancePort.Insert(lBoxEntity);

            BoxShapeProperties rBoxShape = null;
            SingleShapeEntity rBoxEntity = null;
            rBoxShape = new BoxShapeProperties(500f, new Pose(), new Vector3(0.25f, 0.25f, 5f));
            rBoxEntity = new SingleShapeEntity(new BoxShape(rBoxShape), new Vector3(2.63f, 0.5f, 0f));
            rBoxEntity.State.Name = "arena right";
            SimulationEngine.GlobalInstancePort.Insert(rBoxEntity);

            BoxShapeProperties bBoxShape = null;
            SingleShapeEntity bBoxEntity = null;
            bBoxShape = new BoxShapeProperties(500f, new Pose(), new Vector3(5f, 0.25f, 0.25f));
            bBoxEntity = new SingleShapeEntity(new BoxShape(bBoxShape), new Vector3(0f, 0.5f, -2.63f));
            bBoxEntity.State.Name = "arena bottom";
            SimulationEngine.GlobalInstancePort.Insert(bBoxEntity);
        }

        #endregion
    }
}
