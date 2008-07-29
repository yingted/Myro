// Copyright (c) Microsoft Corporation.  All rights reserved.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using Microsoft.Ccr.Core;
using Microsoft.Dss.Core;
using Microsoft.Dss.Core.Attributes;
using Microsoft.Dss.ServiceModel.Dssp;
using Microsoft.Dss.ServiceModel.DsspServiceBase;
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Xml;
using System.Drawing;
using W3C.Soap;
using Myro.Utilities;
using System.Linq;

using brick = Myro.Services.Scribbler.ScribblerBase.Proxy;
using webcam = Microsoft.Robotics.Services.WebCam.Proxy;

namespace Myro.Services.Scribbler.FlukeCam
{
    public static class Contract
    {
        public const string Identifier = "http://www.roboteducation.org/schemas/2008/06/flukecam.html";
    }

    //[DataContract]
    //[DataMemberConstructor]
    //public class QueryFrameRequest
    //{
    //    [DataMember]
    //    public Guid Format;
    //}
    //[DataContract]
    //[DataMemberConstructor]
    //public class QueryFrameResponse
    //{
    //    [DataMember]
    //    public Guid Format;
    //    [DataMember]
    //    public System.Drawing.Size Size;
    //    [DataMember]
    //    public DateTime TimeStamp;
    //    [DataMember]
    //    public byte[] Frame;
    //}
    //public class QueryFrame : Get<QueryFrameRequest, PortSet<QueryFrameResponse,Fault>>
    //{
    //    public QueryFrame() : base() {}
    //    public QueryFrame(QueryFrameRequest b) : base(b) {}
    //}

    //public class WebCamOperations : PortSet<
    //    DsspDefaultLookup,
    //    DsspDefaultDrop,
    //    QueryFrame> { }

    /// <summary>
    /// The Fluke obstacle Service
    /// </summary>
    [DisplayName("Fluke Camera")]
    [Description("The Fluke Obstacle Detector")]
    [Contract(Contract.Identifier)]
    [AlternateContract(webcam.Contract.Identifier)] //implementing the generic contract
    class FlukeCamService : DsspServiceBase
    {
        /// <summary>
        /// Robot base partner
        /// </summary>
        [Partner("ScribblerBase", Contract = brick.Contract.Identifier,
            CreationPolicy = PartnerCreationPolicy.UseExistingOrCreate, Optional = false)]
        private brick.ScribblerOperations _scribblerPort = new brick.ScribblerOperations();

        [ServicePort("webcam", AllowMultipleInstances = false, QueueDepthLimit = 10,
            QueuingPolicy = DsspOperationQueuingPolicy.DiscardWithFault)]
        private webcam.WebCamOperations _mainPort = new webcam.WebCamOperations();

        [ServiceState()]
        webcam.WebCamState _state;

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="port"></param>
        public FlukeCamService(DsspServiceCreationPort port)
            : base(port) { }

        [ServiceHandler(ServiceHandlerBehavior.Concurrent)]
        public IEnumerator<ITask> QueryFrameHandler(webcam.QueryFrame req)
        {
            yield return Arbiter.Choice(_scribblerPort.GetImage(req.Body.Format),
                delegate(brick.ImageResponse r)
                {
                    try
                    {
                        var resp = new webcam.QueryFrameResponse()
                        {
                            Format = req.Body.Format,
                            Size = new System.Drawing.Size(r.Width, r.Height),
                            TimeStamp = r.Timestamp,
                        };
                        if (req.Body.Format.Equals(MyroImageType.Color.Guid))
                            resp.Frame = convertColorToRGB(r.Data, r.Width, r.Height);
                        else if (req.Body.Format.Equals(MyroImageType.Gray.Guid))
                            resp.Frame = r.Data;
                        else if (req.Body.Format.Equals(MyroImageType.JpegColor.Guid) ||
                                req.Body.Format.Equals(MyroImageType.JpegColorFast.Guid))
                            resp.Frame = r.Data;
                        else if (req.Body.Format.Equals(MyroImageType.JpegGray.Guid) ||
                                req.Body.Format.Equals(MyroImageType.JpegGrayFast.Guid))
                            resp.Frame = r.Data;
                        else
                            throw new Exception("FlukeCam does not know how to convert this image type");

                        req.ResponsePort.Post(resp);
                    }
                    catch (Exception e)
                    {
                        req.ResponsePort.Post(RSUtils.FaultOfException(e));
                    }
                },
                delegate(Fault f)
                {
                    req.ResponsePort.Post(f);
                });
        }

        //[ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        //public IEnumerator<ITask> UpdateFrameHandler(webcam.UpdateFrame req)
        //{
        //    req.ResponsePort.Post(RSUtils.FaultOfException(new NotSupportedException("Use QueryFrame with FlukeCam")));
        //    yield break;
        //}

        //[ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        //public IEnumerator<ITask> ReplaceHandler(webcam.Replace req)
        //{
        //    req.ResponsePort.Post(RSUtils.FaultOfException(new NotSupportedException("Use QueryFrame with FlukeCam")));
        //    yield break;
        //}

        //[ServiceHandler(ServiceHandlerBehavior.Exclusive)]
        //public IEnumerator<ITask> SubscribeHandler(webcam.Subscribe req)
        //{
        //    req.ResponsePort.Post(RSUtils.FaultOfException(new NotSupportedException("Use QueryFrame with FlukeCam")));
        //    yield break;
        //}

        private byte[] convertColorToRGB(byte[] color, int width, int height)
        {
            byte[] ret = new byte[width * height * 3];

            double y, u, v;
            int sourceLineOffset = 0;
            int destLineOffset = 0;
            for (int i = 0; i < height; i++)
            {
                // Grab the first y, u, and v values
                //v = color[sourceLineOffset];
                y = color[sourceLineOffset + 1];
                u = color[sourceLineOffset + 2] - 128.0;

                // Go in VYUV groups
                for (int j = 0; j < width; j += 4)
                {
                    // First pixel of group of 4 (v)
                    v = color[sourceLineOffset++] - 128.0;
                    ret[destLineOffset++] = (byte)Math.Max(Math.Min(y + 2.03211 * u, 255), 0);
                    ret[destLineOffset++] = (byte)Math.Max(Math.Min(y - 0.39466 * u - 0.58060 * v, 255), 0);
                    ret[destLineOffset++] = (byte)Math.Max(Math.Min(y + 1.13983 * v, 255), 0);

                    // First pixel of group of 4 (y)
                    y = color[sourceLineOffset++];
                    ret[destLineOffset++] = (byte)Math.Max(Math.Min(y + 2.03211 * u, 255), 0);
                    ret[destLineOffset++] = (byte)Math.Max(Math.Min(y - 0.39466 * u - 0.58060 * v, 255), 0);
                    ret[destLineOffset++] = (byte)Math.Max(Math.Min(y + 1.13983 * v, 255), 0);

                    // First pixel of group of 4 (u)
                    u = color[sourceLineOffset++] - 128.0;
                    ret[destLineOffset++] = (byte)Math.Max(Math.Min(y + 2.03211 * u, 255), 0);
                    ret[destLineOffset++] = (byte)Math.Max(Math.Min(y - 0.39466 * u - 0.58060 * v, 255), 0);
                    ret[destLineOffset++] = (byte)Math.Max(Math.Min(y + 1.13983 * v, 255), 0);

                    // First pixel of group of 4 (y)
                    y = color[sourceLineOffset++];
                    ret[destLineOffset++] = (byte)Math.Max(Math.Min(y + 2.03211 * u, 255), 0);
                    ret[destLineOffset++] = (byte)Math.Max(Math.Min(y - 0.39466 * u - 0.58060 * v, 255), 0);
                    ret[destLineOffset++] = (byte)Math.Max(Math.Min(y + 1.13983 * v, 255), 0);
                }
            }

            return ret;
        }

        private byte[] convertGrayToRGB(byte[] color, int width, int height)
        {
            byte[] ret = new byte[width * height * 3];

            int destOffset = 0;
            for (int i = 0; i < color.Length; i++)
            {
                ret[destOffset++] = color[i];
                ret[destOffset++] = color[i];
                ret[destOffset++] = color[i];
            }

            return ret;
        }

    }
}
