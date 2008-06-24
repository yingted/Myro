using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using anarrProx = Microsoft.Robotics.Services.AnalogSensorArray.Proxy;
using anProx = Microsoft.Robotics.Services.AnalogSensor.Proxy;
using contact = Microsoft.Robotics.Services.ContactSensor.Proxy;
using vector = Myro.Services.Generic.Vector;

namespace Myro.Utilities
{
    public static class MSRDSUtils
    {
        //public static VectorState vectorOfBool(bool value, DateTime timestamp)
        //{
        //    VectorState state = new VectorState(
        //    double val = value ? 1.0 : 0.0;
        //    state.NormalizedMeasurement = val;
        //    state.RawMeasurement = val;
        //    state.RawMeasurementRange = 1.0;
        //    state.TimeStamp = timestamp;
        //    return state;
        //}

        public static vector.VectorState vectorOfBools(bool[] values, DateTime timestamp)
        {
            return new vector.VectorState(
                new List<double>(
                    from v in values
                    select (v ? 1.0 : 0.0)
                ), timestamp);
        }

        //public static anarrProx.AnalogSensorArrayState analogOfContact(contact.ContactSensorArrayState state)
        //{
        //    anarrProx.AnalogSensorArrayState ret = new anarrProx.AnalogSensorArrayState();
        //    ret.Sensors = new List<Microsoft.Robotics.Services.AnalogSensor.Proxy.AnalogSensorState>(state.Sensors.Count);
        //    foreach (contact.ContactSensor c in state.Sensors)
        //    {
        //        anProx.AnalogSensorState one = analogOfBool(c.Pressed, c.TimeStamp);
        //        one.HardwareIdentifier = c.HardwareIdentifier;
        //        one.Pose = c.Pose;
        //        ret.Sensors.Add(one);
        //    }
        //    return ret;
        //}

        //public static anProx.AnalogSensorState analogOfFloatOOR(double value, double range, double oneOverRange, DateTime timestamp)
        //{
        //    anProx.AnalogSensorState state = new anProx.AnalogSensorState();
        //    state.NormalizedMeasurement = value * oneOverRange;
        //    state.RawMeasurement = value;
        //    state.RawMeasurementRange = range;
        //    state.TimeStamp = timestamp;
        //    return state;
        //}

        //public static anProx.AnalogSensorState analogOfFloat(double value, double range, DateTime timestamp)
        //{
        //    return analogOfFloatOOR(value, range, 1.0 / range, timestamp);
        //}

        //public static anProx.AnalogSensorState analogOfFloat(double value, DateTime timestamp)
        //{
        //    return analogOfFloatOOR(value, 1.0, 1.0, timestamp);
        //}

        public static vector.VectorState vectorOfFloats(double[] values, DateTime timestamp)
        {
            return new vector.VectorState(new List<double>(values), timestamp);
        }

        //public static anarrProx.AnalogSensorArrayState analogOfFloats(double[] values, DateTime timestamp)
        //{
        //    return analogOfFloats(values, 1.0, timestamp);
        //}

    }
}
