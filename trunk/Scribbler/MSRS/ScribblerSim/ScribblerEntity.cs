//------------------------------------------------------------------------------
// ScribblerEntity.cs
//
//     Defines the simulation scribbler entity
//
//------------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.Serialization;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.Core;
using Microsoft.Ccr.Core;
using Microsoft.Robotics.Simulation.Physics;
using Microsoft.Robotics.PhysicalModel;
using System.IO;


namespace Microsoft.Robotics.Simulation.Engine
{

    /// <summary>
    /// Scribbler variant of the motor base entity. It just specifies different physical properties in
    /// its custom constructor, otherwise uses the base class as is.
    /// </summary>
    [DataContract]
    public class ScribblerRobot : DifferentialDriveEntity
    {
        /// <summary>
        /// Default constructor, used for creating the entity from an XML description
        /// </summary>
        public ScribblerRobot() { }

        /// <summary>
        /// Custom constructor for building model from hardcoded values. Used to create entity programmatically
        /// </summary>
        /// <param name="initialPos"></param>
        public ScribblerRobot(Vector3 initialPos)
        {
            MASS = 0.68f; //kg (1.5 lbs)
            // the default settings approximate the Scribbler chassis
            CHASSIS_DIMENSIONS = new Vector3(0.1524f,  //meters wide (6 in)
                                             0.0508f,  //meters high (2 in)
                                             0.1905f); //meters long (7.5 in)
            
            FRONT_WHEEL_MASS = 0.01f;
            CHASSIS_CLEARANCE = 0.006f;     //0.25 in
            FRONT_WHEEL_RADIUS = 0.0381f;   //1.5 in
            CASTER_WHEEL_RADIUS = 0.0127f;  //0.5 in
            FRONT_WHEEL_WIDTH = 0.006f;     //0.25 in
            //CASTER_WHEEL_WIDTH = 0.008f; //not currently used
            FRONT_AXLE_DEPTH_OFFSET = 0; // distance of the axle from the center of robot //3.75 in

            base.State.Name = "ScribblerRobot";
            base.State.MassDensity.Mass = MASS;
            base.State.Pose.Position = initialPos;

            //this is because the robot is hevily back weighted
            base.State.MassDensity.CenterOfMass = new Pose(new Vector3(-1f, -1f, -1f));

            // reference point for all shapes is the projection of
            // the center of mass onto the ground plane 
            // (basically the spot under the center of mass, at Y = 0, or ground level)

            // NOTE: right/left is from the perspective of the robot, looking forward
            // NOTE: X = width of robot (right to left), Y = height, Z = length

            // chassis position
            BoxShapeProperties motorBaseDesc = new BoxShapeProperties("Scribbler Body", MASS,
                new Pose(new Vector3(
                0, // Chassis center is also the robot center, so use zero for the X axis offset
                CHASSIS_CLEARANCE + CHASSIS_DIMENSIONS.Y / 2, // chassis is off the ground and its center is DIM.Y/2 above the clearance
                0)), // any offset in the z/length axis
                CHASSIS_DIMENSIONS);

            motorBaseDesc.Material = new MaterialProperties("high friction", 0.0f, 1.0f, 20.0f);
            motorBaseDesc.Name = "Chassis";
            ChassisShape = new BoxShape(motorBaseDesc);

            // rear wheel is also called the caster
            CASTER_WHEEL_POSITION = new Vector3(0, // center of chassis widthwise
                CASTER_WHEEL_RADIUS, // distance from ground
                CHASSIS_DIMENSIONS.Z / 2 - CASTER_WHEEL_RADIUS / 2); // all the way at the back of the robot

            RIGHT_FRONT_WHEEL_POSITION = new Vector3(
                +CHASSIS_DIMENSIONS.X / 2 - FRONT_WHEEL_WIDTH / 2,// left of center
                FRONT_WHEEL_RADIUS,// distance from ground of axle
                FRONT_AXLE_DEPTH_OFFSET); // distance from center, on the z-axis

            LEFT_FRONT_WHEEL_POSITION = new Vector3(
                -CHASSIS_DIMENSIONS.X / 2 - FRONT_WHEEL_WIDTH / 2,// right of center
                FRONT_WHEEL_RADIUS,// distance from ground of axle
                FRONT_AXLE_DEPTH_OFFSET); // distance from center, on the z-axis

            MotorTorqueScaling = 10;


        }
    }


}
