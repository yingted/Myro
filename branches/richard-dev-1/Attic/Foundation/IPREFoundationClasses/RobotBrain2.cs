using System;
using System.Collections.Generic;
using System.Collections;
using System.Text;
using System.Threading;
using W3C.Soap;

using Microsoft.Ccr.Core;
using Microsoft.Dss.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.Hosting;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Dss.ServiceModel.DsspServiceBase;
using System.Xml;
using System.Xml.XPath;
using Microsoft.Dss.Core.DsspHttp;

using Myro.Adapters;

using drive = Microsoft.Robotics.Services.Drive.Proxy;
using directory = Microsoft.Dss.Services.Directory;
using analog = Microsoft.Robotics.Services.AnalogSensor.Proxy;
using contact = Microsoft.Robotics.Services.ContactSensor.Proxy;
using analogArray = Microsoft.Robotics.Services.AnalogSensorArray.Proxy;
using tts = Microsoft.Robotics.Technologies.Speech.TextToSpeech.Proxy;

namespace IPREFoundationClasses
{
    public partial class RobotBrain : MyroInterfaces.IMyroSensor, MyroInterfaces.IMyroMovement, MyroInterfaces.IMyroSound, MyroInterfaces.IMyroLED, MyroInterfaces.IMyroSpeech
    {
        protected DriveAdapter cacheDrive = null;               // done for performance purposes
        protected TTSAdapter cacheTTS = null;                   // done for performance purposes
        protected ToneGeneratorAdapter cacheTone = null;        // done for performance purposes
        protected LEDArrayAdapter cacheLED = null;              // done for performance purposes


        #region IMyroSensor implementation
        public float[] get(string sensorID) 
        {
            List<float> list = new List<float>();
            int index = 0;
            try
            {
                switch (sensorID.ToLower())
                {
                    case "light":
                        while (true)
                            list.Add(getLight(index++));
                    case "line":
                        while (true)
                            list.Add(getLine(index++));
                    case "ir":
                        while (true)
                            list.Add(getIR(index++));
                    case "stall":
                            list.Add(getStall());
                            break;
                    case "contact":
                        while(true)
                            list.Add(getContact(index++));
                    case "ultrasonic":
                        while (true)
                            list.Add(getSonar(index++));
                    case "sound":
                        while (true)
                            list.Add(getSound(index++));
                    default:
                        throw new NoInstanceException();
                }
            }
            catch (IndexOutOfRangeException)
            {
            }
            return list.ToArray();
        }
        
        public float get(string sensorID, int position) 
        {
            switch (sensorID.ToLower())
            {
                case "light":
                    return getLight(position);
                case "line":
                    return getLine(position);
                case "ir":
                    return getIR(position);
                case "stall":
                    return getStall();
                case "ultrasonic":
                    return getSonar(position);
                case "sound":
                    return getSound(position);
                default:
                    throw new NoInstanceException();
            }
        }
        
        public float getLight(int position) 
        { 
            ArrayList list = new ArrayList();
            if (!SensorsDictionary.TryGetValue(SensorTypes.LightSensor, out list))
                throw new NoInstanceException();

            if (list.Count <= position)
                throw new IndexOutOfRangeException();

            ListEntry le = (ListEntry)list[position];
            float value = Defines.INVALID_FLOAT;

            if (le.Adapter.AdapterType == AdapterTypes.AnalogSensorAdapter)
            {
                AnalogSensorAdapter adapter = (AnalogSensorAdapter)le.Adapter;
                value = adapter.get();
            }
            else if (le.Adapter.AdapterType == AdapterTypes.AnalogSensorArrayAdapter)
            {
                AnalogSensorArrayAdapter adapter = (AnalogSensorArrayAdapter)le.Adapter;
                value = adapter.get(le.BeginOffset);
            }

            if (value == Defines.INVALID_FLOAT)
                throw new InvalidValueException("Cannot get true value.");

            return value; 
        }

        public float getSound(int position)
        {
            ArrayList list = new ArrayList();
            if (!SensorsDictionary.TryGetValue(SensorTypes.SoundSensor, out list))
                throw new NoInstanceException();

            if (list.Count <= position)
                throw new IndexOutOfRangeException();

            ListEntry le = (ListEntry)list[position];
            float value = Defines.INVALID_FLOAT;

            if (le.Adapter.AdapterType == AdapterTypes.AnalogSensorAdapter)
            {
                AnalogSensorAdapter adapter = (AnalogSensorAdapter)le.Adapter;
                value = adapter.get();
            }
            else if (le.Adapter.AdapterType == AdapterTypes.AnalogSensorArrayAdapter)
            {
                AnalogSensorArrayAdapter adapter = (AnalogSensorArrayAdapter)le.Adapter;
                value = adapter.get(le.BeginOffset);
            }

            if (value == Defines.INVALID_FLOAT)
                throw new InvalidValueException("Cannot get true value.");

            return value;
        }

        public float getSonar(int position)
        {
            ArrayList list = new ArrayList();
            if (!SensorsDictionary.TryGetValue(SensorTypes.UltraSonicSonar, out list))
                throw new NoInstanceException();

            if (list.Count <= position)
                throw new IndexOutOfRangeException();

            ListEntry le = (ListEntry)list[position];
            float value = Defines.INVALID_FLOAT;

            SonarAdapter adapter = (SonarAdapter)le.Adapter;
            value = adapter.get();

            if (value == Defines.INVALID_FLOAT)
                throw new InvalidValueException("Cannot get true value.");

            return value;
        }

        public float getIR(int position) 
        {
            ArrayList list = new ArrayList();
            if (!SensorsDictionary.TryGetValue(SensorTypes.IRSensor, out list))
                throw new NoInstanceException();

            if (list.Count <= position)
                throw new IndexOutOfRangeException();

            ListEntry le = (ListEntry)list[position];
            float value = Defines.INVALID_FLOAT;

            if (le.Adapter.AdapterType == AdapterTypes.AnalogSensorAdapter)
            {
                AnalogSensorAdapter adapter = (AnalogSensorAdapter)le.Adapter;
                value = adapter.get();
            }
            else if (le.Adapter.AdapterType == AdapterTypes.AnalogSensorArrayAdapter)
            {
                AnalogSensorArrayAdapter adapter = (AnalogSensorArrayAdapter)le.Adapter;
                value = adapter.get(le.BeginOffset);
            }

            if (value == Defines.INVALID_FLOAT)
                throw new InvalidValueException("Cannot get true value.");

            return value; 
        }

        public float getLine(int position) 
        {
            ArrayList list = new ArrayList();
            if (!SensorsDictionary.TryGetValue(SensorTypes.LineSensor, out list))
                throw new NoInstanceException();

            if (list.Count <= position)
                throw new IndexOutOfRangeException();

            ListEntry le = (ListEntry)list[position];
            float value = Defines.INVALID_FLOAT;

            if (le.Adapter.AdapterType == AdapterTypes.ContactSensorArrayAdapter)
            {
                ContactSensorArrayAdapter adapter = (ContactSensorArrayAdapter)le.Adapter;
                    if(adapter.get(le.BeginOffset))
                        value = 1;
                    else
                        value = 0;
            }

            if (value == Defines.INVALID_FLOAT)
                throw new InvalidValueException("Cannot get true value.");

            return value;
        }

        public float getStall() 
        {
            ArrayList list = new ArrayList();
            if (!SensorsDictionary.TryGetValue(SensorTypes.StallSensor, out list))
                throw new NoInstanceException();

            if (list.Count == 0)
                throw new IndexOutOfRangeException();

            ListEntry le = (ListEntry)list[0];
            float value = Defines.INVALID_FLOAT;

            if (le.Adapter.AdapterType == AdapterTypes.ContactSensorArrayAdapter)
            {
                ContactSensorArrayAdapter adapter = (ContactSensorArrayAdapter)le.Adapter;
                if (adapter.get(le.BeginOffset))
                    value = 1;
                else
                    value = 0;
            }

            if (value == Defines.INVALID_FLOAT)
                throw new InvalidValueException("Cannot get true value.");

            return value;

        }

        public float getContact(int index)
        {
            ArrayList list = new ArrayList();
            if (!SensorsDictionary.TryGetValue(SensorTypes.ContactSensorArray, out list))
                throw new NoInstanceException();

            if (list.Count == 0 || list.Count < index + 1)
                throw new IndexOutOfRangeException();

            ListEntry le = (ListEntry)list[index];
            float value = Defines.INVALID_FLOAT;

            if (le.Adapter.AdapterType == AdapterTypes.ContactSensorArrayAdapter)
            {
                ContactSensorArrayAdapter adapter = (ContactSensorArrayAdapter)le.Adapter;
                if (adapter.get(le.BeginOffset))
                    value = 1;
                else
                    value = 0;
            }

            if (value == Defines.INVALID_FLOAT)
                throw new InvalidValueException("Cannot get true value.");

            return value;
        }

        public float[] getContact()
        {
            ArrayList list = new ArrayList();
            if (!SensorsDictionary.TryGetValue(SensorTypes.ContactSensorArray, out list))
                throw new NoInstanceException();

            float[] ret = new float[list.Count];
            for (int index = 0; index < list.Count; index++)
            {
                ListEntry le = (ListEntry)list[index];
                ret[index] = Defines.INVALID_FLOAT;

                if (le.Adapter.AdapterType == AdapterTypes.ContactSensorArrayAdapter)
                {
                    ContactSensorArrayAdapter adapter = (ContactSensorArrayAdapter)le.Adapter;
                    if (adapter.get(le.BeginOffset))
                        ret[index] = 1;
                    else
                        ret[index] = 0;
                }

                if (ret[index] == Defines.INVALID_FLOAT)
                    throw new InvalidValueException("Cannot get true value.");
            }

            return ret;
        }

        #endregion

        #region Movement implementation

        public void Move(float translate, float rotate)
        {
            cacheDrive.Move(translate, rotate);
        }

        public void Forward(float power)
        {
            cacheDrive.Forward(power);
        }

        public void ForwardFor(float power, float seconds)
        {
            cacheDrive.ForwardFor(power, seconds);
        }

        public void Backward(float power)
        {
            cacheDrive.Backward(power);
        }

        public void BackwardFor(float power, float seconds)
        {
            cacheDrive.BackwardFor(power, seconds);
        }

        public void Turn(string direction, float power)
        {
            cacheDrive.Turn(direction, power);
        }

        public void TurnFor(string direction, float power, float seconds)
        {
            cacheDrive.TurnFor(direction, power, seconds);
        }

        public void TurnLeft(float power)
        {
            cacheDrive.TurnLeft(power);
        }

        public void TurnLeftFor(float power, float seconds)
        {
            cacheDrive.TurnLeftFor(power, seconds);
        }

        public void TurnRight(float power)
        {
            cacheDrive.TurnRight(power);
        }

        public void TurnRightFor(float power, float seconds)
        {
            cacheDrive.TurnRightFor(power, seconds);
        }

        public void Stop()
        {
            cacheDrive.Stop();
        }

        public void SetMotors(float leftPower, float rightPower)
        {
            cacheDrive.SetMotors(leftPower, rightPower);
        }

        public void SetMotorsFor(float leftPower, float rightPower, float seconds)
        {
            cacheDrive.SetMotorsFor(leftPower, rightPower, seconds);
        }
        #endregion

        #region IMyroSound implemenatation
        public MyroInterfaces.IMyroSong ReadSong(string filename)
        {
            return cacheTone.ReadSong(filename);
        }

        public void PlaySong(MyroInterfaces.IMyroSong song)
        {
            cacheTone.PlaySong(song);
        }

        public MyroInterfaces.IMyroSong MakeSong(string text)
        {
            return cacheTone.MakeSong(text);
        }

        public void SaveSong(string text, string fileName)
        {
            throw new NotImplementedException();
        }

        public void beep(float duration, float frequency)
        {
            cacheTone.beep(duration, frequency);
        }

        public void beep(float duration, float frequency1, float frequency2)
        {
            cacheTone.beep(duration, frequency1,frequency2);
        }

        #endregion

        #region IMyroLED implementation
        public void setBinary(uint number)
        {
            throw new NotImplementedException();
        }

        public void setLED(string position, string state)
        {
            throw new NotImplementedException();
        }

        public virtual void setLED(string position, int state)
        {
            cacheLED.setLED(position, state);
        }
        #endregion

        #region IMyroSpeech implementation
        public virtual string GetVoice()
        {
            return cacheTTS.GetVoice();
        }

        public virtual void SetVoice(string voiceName)
        {
            cacheTTS.SetVoice(voiceName);
        }

        public virtual void Speak(string message, bool async)
        {
            cacheTTS.Speak(message, async);
        }

        public virtual void Speak(string message)
        {
            cacheTTS.Speak(message);
        }

        public virtual string[] GetVoices()
        {
            return cacheTTS.GetVoices();
        }

        public virtual void PlaySpeech(string fileName)        // REVIEW (jaredj): PlaySound maybe since if plays a WAV file
        {
            throw new NotImplementedException();
        }

        public virtual void SaveSpeech(string message, string fileName)
        {
            throw new NotImplementedException();
        }
        #endregion
    }
}
