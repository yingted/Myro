//-----------------------------------------------------------------------
//  This file is part of the Microsoft Robotics Studio Code Samples.
// 
//  Copyright (C) Microsoft Corporation.  All rights reserved.
//
//  $File: SaveStream.cs $ $Revision$
//
//  Portions modified Copyright (C) 2006 SharpLogic Software.
//-----------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Drawing;
using Microsoft.Ccr.Core;
using System.Drawing.Imaging;

namespace SharpLogic.Robotics.Services.Surveyor.Srv1.Camera
{
    class SaveStream : CcrServiceBase
    {
        BinaryWriter _writer;
        MemoryStream _memory;
        SaveStreamPort _port;
        ImageCodecInfo _codec;
        EncoderParameters _eps;

        SaveStream(string filename, double quality, DispatcherQueue queue)
            : base(queue)
        {
            _writer = new BinaryWriter(new FileStream(filename, FileMode.Append, FileAccess.Write));
            _memory = new MemoryStream();
            _port = new SaveStreamPort();

            foreach (ImageCodecInfo info in ImageCodecInfo.GetImageEncoders())
            {
                if (info.FormatID == ImageFormat.Jpeg.Guid)
                {
                    _codec = info;
                    _eps = new EncoderParameters();
                    _eps.Param[0] = new EncoderParameter(
                        System.Drawing.Imaging.Encoder.Quality, 
                        (long)(quality * 100)
                    );
                    break;
                }
            }

            ActivateHandler();
        }

        static public SaveStreamPort Create(string filename, double quality, DispatcherQueue queue)
        {
            SaveStream save = new SaveStream(filename, quality, queue);

            return save._port;
        }

        private void ActivateHandler()
        {
            Activate(
                Arbiter.Interleave(
                    new TeardownReceiverGroup(
                        Arbiter.Receive<Shutdown>(false, _port, ShutdownHandler)
                    ),
                    new ExclusiveReceiverGroup(
                        Arbiter.Receive<Frame>(true, _port, FrameHandler)
                    ),
                    new ConcurrentReceiverGroup()
                )
            );
        }

        void ShutdownHandler(Shutdown shutdown)
        {
            _writer.Flush();
            _writer.Close();
            _memory.Close();
        }

        void FrameHandler(Frame frame)
        {
            _writer.Write(frame.TimeStamp.ToBinary());

            _memory.Position = 0;
            frame.Image.Save(_memory, _codec, _eps);
            
            byte[] buffer = _memory.GetBuffer();
            int length = (int)_memory.Position;

            _writer.Write(length);
            _writer.Write(buffer, 0, length);
            _writer.Flush();

            frame.Image.Dispose();
        }
    }

    class SaveStreamPort : PortSet<Frame, Shutdown>
    {
    }

    class ReadStream : CcrServiceBase
    {
        DateTime _currFrame;
        SaveStreamPort _port = new SaveStreamPort();
        Port<DateTime> _timePort = new Port<DateTime>();
        BinaryReader _reader;
        bool _shutdown;

        ReadStream(string filename, DispatcherQueue queue)
            : base(queue)
        {
            _reader = new BinaryReader(new FileStream(filename, FileMode.Open, FileAccess.Read));

            _currFrame = DateTime.FromBinary(_reader.ReadInt64());

            _shutdown = false;
            ActivateHandler();
        }

        void ActivateHandler()
        {
            Activate(
                Arbiter.Interleave(
                    new TeardownReceiverGroup(
                        Arbiter.Receive<Shutdown>(false, _port, ShutdownHandler)
                    ),
                    new ExclusiveReceiverGroup(
                        Arbiter.Receive(true, _timePort, TimeHandler)
                    ),
                    new ConcurrentReceiverGroup()
                )
            );

            TimeHandler(DateTime.Now);
        }

        void ShutdownHandler(Shutdown shutdown)
        {
            _shutdown = true;
            _reader.Close();
        }

        void TimeHandler(DateTime signal)
        {
            if (_shutdown || _reader.BaseStream.Position >= _reader.BaseStream.Length)
            {
                return;
            }

            int length = _reader.ReadInt32();
            byte[] buffer = _reader.ReadBytes(length);
            using (MemoryStream stream = new MemoryStream(buffer, false))
            {
                Frame frame = new Frame();
                frame.Image = new Bitmap(stream);
                frame.TimeStamp = _currFrame;

                _port.Post(frame);
            }

            if (_reader.BaseStream.Position >= _reader.BaseStream.Length)
            {
                Activate(
                    Arbiter.Receive(false, TimeoutPort(2000),
                        delegate(DateTime t)
                        {
                            Frame frame = new Frame();
                            frame.Image = null;
                            frame.TimeStamp = _currFrame + TimeSpan.FromSeconds(2);
                            _port.Post(frame);
                        }
                    )
                );
                return;
            }

            DateTime lastFrame = _currFrame;
            _currFrame = DateTime.FromBinary(_reader.ReadInt64());

            TimeSpan offset = _currFrame - lastFrame;


            int ms = (int)Math.Round(offset.TotalMilliseconds);
            if (ms <= 0)
            {
                _timePort.Post(_currFrame);
            }
            else
            {
                ms = Math.Min(ms, 2000);

                Activate(
                    Arbiter.Receive(false, TimeoutPort(ms),
                        delegate(DateTime t)
                        {
                            _timePort.Post(t);
                        }
                    )
                );
            }
        }

        static public SaveStreamPort Create(string filename, DispatcherQueue queue)
        {
            ReadStream read = new ReadStream(filename, queue);

            return read._port;
        }
    }
}
