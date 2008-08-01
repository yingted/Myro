// Copyright (c) Microsoft Corporation.  All rights reserved.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using System.Threading;

using Microsoft.Ccr.Core;
using Microsoft.Dss.Core;
using W3C.Soap;
using Microsoft.Robotics.Simulation;
using Microsoft.Robotics.Simulation.Engine;
using engineproxy = Microsoft.Robotics.Simulation.Engine.Proxy;
using Microsoft.Robotics.Simulation.Physics;
using Microsoft.Robotics.PhysicalModel;
using xnaTypes = Microsoft.Xna.Framework;

namespace Myro.GUI.WPFControls
{
    /// <summary>
    /// Interaction logic for SimulatorDisplay.xaml
    /// </summary>
    public partial class SimulatorDisplay : UserControl
    {
        /// <summary>
        /// This event is raised when the simulator starts.  The GUI uses this
        /// to add the 'Simulator' tab.
        /// </summary>
        public event EventHandler SimulatorFound;

        SimulationEnginePort _notifyTarget = null;
        DispatcherQueue queue = null;
        DispatcherQueue throttledQueue = null;
        CameraInfo curCamera = null;
        class CameraInfo
        {
            public CameraEntity Camera;
            public Port<System.Drawing.Bitmap> Port;
        }

        Port<Size> sizeChangePort = new Port<Size>();
        bool shouldStay = true;

        enum DragType { LOOK, MOVE };
        DragType dragType;
        Vector2 mouseStart;

        // Maximum time between frames (increases GUI responsiveness
        // to limit this)
        int frameInterval = 50;
        DateTime lastFrameTime = DateTime.Now;

        /// <summary>
        /// Constructor.  Dispose must be called on this class.
        /// </summary>
        public SimulatorDisplay()
        {
            InitializeComponent();
        }

        /// <summary>
        /// Stop DispatcherQueues and threads.
        /// </summary>
        public void Dispose()
        {
            queue.Dispose();
            throttledQueue.Dispose();
            shouldStay = false;
        }

        /// <summary>
        /// Call to initialize the control and start threads.  The main GUI 
        /// </summary>
        public void Start()
        {
            queue = new DispatcherQueue("SimulatorDisplay", new Dispatcher());
            throttledQueue = new DispatcherQueue("SimulatorDisplayThrottled", new Dispatcher(1, "SimulatorDisplayThrottled"), TaskExecutionPolicy.ConstrainQueueDepthDiscardTasks, 2);

            // Start a thread that will wait one second, check for the SimulatorEngine,
            // and start another check thread if it is not found, or trigger the
            // SimulatorFound event if it is found.
            ParameterizedThreadStart checker = new ParameterizedThreadStart(
                delegate(object next)
                {
                    //Console.WriteLine("Checking for simulator...");
                    if (checkSimulator() == false)
                    {
                        Thread.Sleep(TimeSpan.FromSeconds(1));
                        new Thread((ParameterizedThreadStart)next).Start(next);
                    }
                });
            new Thread(checker).Start(checker);

            Arbiter.Activate(queue, Arbiter.Receive(false, sizeChangePort, SizeChangeHandler));
        }

        private bool checkSimulator()
        {
            if (_notifyTarget == null && SimulationEngine.GlobalInstancePort != null)
            {
                _notifyTarget = new SimulationEnginePort();
                addCameraWatch("MainCamera", true);
                addCameraWatch("ScribblerCamera", false);
                return true;
            }
            else
                return false;
        }

        private void addCameraWatch(string cameraName, bool selectIfFound)
        {
            Delegate subscribeForEntity = new ThreadStart(delegate()
            {
                try
                {
                    SimulationEngine.GlobalInstancePort.Subscribe(new EntitySubscribeRequestType() { Name = cameraName }, _notifyTarget);
                    Arbiter.Activate(queue, Arbiter.Receive<InsertSimulationEntity>(true, _notifyTarget,
                        delegate(InsertSimulationEntity ins) { AddCamera(ins.Body as CameraEntity, selectIfFound); }));
                }
                catch (Exception e)
                {
                    Dispatcher.Invoke(System.Windows.Threading.DispatcherPriority.Normal,
                        new ThreadStart(delegate() { GUIUtilities.ReportUnexpectedException(e); }));
                }
            });
            if (_notifyTarget != null && SimulationEngine.GlobalInstancePort != null)
            {
                Arbiter.Activate(queue, Arbiter.Choice(
                    SimulationEngine.GlobalInstancePort.Query(
                        new VisualEntity() { EntityState = new EntityState() { Name = cameraName } }),
                    delegate(QuerySimulationEntityResponseType r)
                    {
                        if (r.Entity != null)
                        {
                            var cam = r.Entity as CameraEntity;
                            if (cam != null)
                                AddCamera(cam, selectIfFound);
                            else
                                subscribeForEntity.DynamicInvoke();
                        }
                        else
                            subscribeForEntity.DynamicInvoke();
                    },
                    delegate(Fault f)
                    {
                        subscribeForEntity.DynamicInvoke();
                    }));
            }
        }

        private void AddCamera(CameraEntity camera, bool select)
        {
            if (camera != null)
            {
                // Store camera entity and subscribe to it to receive images
                CameraInfo ci = new CameraInfo() { Camera = camera };
                subscribeCamera(ci);

                // Add it to the camera list
                Dispatcher.BeginInvoke(System.Windows.Threading.DispatcherPriority.Normal,
                    new ThreadStart(delegate()
                    {
                        SimCamBox.Items.Add(new ComboBoxItem()
                        {
                            Content = (camera.EntityState != null ?
                                        camera.EntityState.Name :
                                        "Unnamed camera"),
                            Tag = ci
                        });
                        if (select)
                            SimCamBox.SelectedIndex = SimCamBox.Items.Count - 1;

                        // Trigger the SimulatorFound event if this is the first camera found
                        if (SimCamBox.Items.Count == 1)
                            SimulatorFound.Invoke(this, new EventArgs());
                    }));
            }
        }

        private void SizeChangeHandler(Size size)
        {
            // Flush size change messages
            while (sizeChangePort.Test() != null) { };

            // Resize the bitmap for displaying camera images
            try
            { initBitmap(); }
            catch (Exception e) { GUIUtilities.ReportUnexpectedException(e); }

            Thread.Sleep(50);

            // Activate new receiver
            try
            {
                // This try is here because this will throw an exception when DSS shuts down.
                Arbiter.Activate(queue, Arbiter.Receive(false, sizeChangePort, SizeChangeHandler));
            }
            catch (Exception) { }
        }

        private void initBitmap()
        {
            if (SimulatorImage != null && SimulatorContainer.ActualWidth > 0 && SimulatorContainer.ActualHeight > 0)
            {
                Dispatcher.Invoke(System.Windows.Threading.DispatcherPriority.Normal,
                    new ThreadStart(delegate()
                    {
                        SimulatorImage.Source = new WriteableBitmap(
                            (int)SimulatorContainer.ActualWidth,
                            (int)SimulatorContainer.ActualHeight,
                            96, 96, PixelFormats.Bgra32, null);
                    }));
            }
        }

        private void subscribeCamera(CameraInfo argCi)
        {
            // Subscribe to the simulator camera, using a handler
            // that will only update the GUI if this camera is
            // selected (because there is currently no way to 
            // unsubscribe from a camera.
            //if (ci.Camera.IsRealTimeCamera)
            //{
            CameraInfo ci = argCi;
            ci.Port = new Port<System.Drawing.Bitmap>();
            ci.Camera.Subscribe(ci.Port);
            Interleave interleave = new Interleave(new ExclusiveReceiverGroup(), new ConcurrentReceiverGroup());
            interleave.CombineWith(
                new Interleave(new ExclusiveReceiverGroup(
                    Arbiter.Receive(true, ci.Port,
                    delegate(System.Drawing.Bitmap inbmp)
                    {
                        if (interleave.PendingExclusiveCount <= 1 && curCamera == ci)
                            if (DateTime.Now.Subtract(lastFrameTime).Milliseconds >= frameInterval)
                            {
                                lastFrameTime = DateTime.Now;
                                updateImageDisplay(inbmp);
                            }
                    })),
                new ConcurrentReceiverGroup()));
            Arbiter.Activate(throttledQueue, interleave);
            //}
            //else
            //{
            //    // If it's not a real time camera, we have to start a loop
            //    // to query it
            //    new Thread(new ThreadStart(delegate()
            //    {
            //        var resultPort = new PortSet<System.Drawing.Bitmap, Exception>();
            //        while (shouldStay)
            //        {
            //            if (curCamera == ci)
            //            {
            //                // The throttledQueue has only 1 thread, so concurrent execution
            //                // will not happen here if the updateImageDisplay handler cannot
            //                // keep up with the rate at which we're querying frames.
            //                ci.Camera.CaptureScene(System.Drawing.Imaging.ImageFormat.Bmp, resultPort);
            //                Arbiter.Activate(throttledQueue, Arbiter.Choice(resultPort,
            //                    updateImageDisplay,
            //                    delegate(Exception e)
            //                    {
            //                        Console.WriteLine(e);
            //                    }));
            //            }
            //            Thread.Sleep(frameInterval);
            //        }
            //    })).Start();
            //}
        }

        private void updateImageDisplay(System.Drawing.Bitmap bmp)
        {
            Dispatcher.Invoke(System.Windows.Threading.DispatcherPriority.Normal,
                new ThreadStart(delegate()
                {
                    try
                    {
                        if (SimulatorImage.Source != null && bmp != null)
                        {
                            //Console.WriteLine("updating image");
                            WriteableBitmap bmpsrc = SimulatorImage.Source as WriteableBitmap;
                            if (bmpsrc != null && bmp.PixelFormat == System.Drawing.Imaging.PixelFormat.Format32bppArgb)
                            {
                                //Console.WriteLine("copying");
                                int srcxoffset = (int)((bmp.Width - bmpsrc.Width) / 2);
                                int srcyoffset = (int)((bmp.Height - bmpsrc.Height) / 2);
                                int dstxoffset = 0;
                                int dstyoffset = 0;
                                if (srcxoffset < 0)
                                {
                                    dstxoffset = -srcxoffset;
                                    srcxoffset = 0;
                                }
                                if (srcyoffset < 0)
                                {
                                    dstyoffset = -srcyoffset;
                                    srcyoffset = 0;
                                }
                                int wid = Math.Min((int)bmpsrc.Width, bmp.Width + srcxoffset);
                                int hei = Math.Min((int)bmpsrc.Height, bmp.Height + srcyoffset);
                                var bmpdata = bmp.LockBits(
                                    new System.Drawing.Rectangle(srcxoffset, srcyoffset, wid, hei),
                                    System.Drawing.Imaging.ImageLockMode.ReadOnly,
                                    bmp.PixelFormat);
                                bmpsrc.WritePixels(new Int32Rect(dstxoffset, dstyoffset, wid, hei),
                                    bmpdata.Scan0,
                                    bmpdata.Stride * bmpdata.Height,
                                    bmpdata.Stride);
                                bmp.UnlockBits(bmpdata);
                                bmp.Dispose();
                            }
                        }
                    }
                    catch (Exception)
                    { }
                }));
        }

        private void OnSimCamChange(object sender, SelectionChangedEventArgs e)
        {
            try
            {
                if (SimCamBox.SelectedItem != null)
                {
                    ComboBoxItem item = SimCamBox.SelectedItem as ComboBoxItem;
                    if (item != null && item.Tag != null && item.Tag is CameraInfo)
                    {
                        // Set the current camera, subscribe it if it has not yet
                        // been subscribed, and set it's frame update rate.
                        curCamera = (CameraInfo)item.Tag;

                        // Clear the display in case the new images are smaller
                        initBitmap();
                    }
                }
            }
            catch (Exception err)
            {
                GUIUtilities.ReportUnexpectedException(err);
            }
        }

        private void OnResize(object sender, SizeChangedEventArgs e)
        {
            try
            {
                sizeChangePort.Post(e.NewSize);
            }
            catch (Exception err)
            {
                GUIUtilities.ReportUnexpectedException(err);
            }
        }

        private void OnMouseMove(object sender, MouseEventArgs e)
        {
            try
            {
                if (curCamera != null && SimulatorContainer.IsMouseCaptured)
                {
                    var p = e.GetPosition(SimulatorContainer);
                    var drag = new Vector2((float)p.X - mouseStart.X, (float)p.Y - mouseStart.Y);
                    mouseStart = new Vector2((float)p.X, (float)p.Y);
                    xnaTypes.Vector3 view = curCamera.Camera.LookAt - curCamera.Camera.Location;
                    view.Normalize();
                    switch (dragType)
                    {
                        case DragType.LOOK:
                            xnaTypes.Vector3 up = new xnaTypes.Vector3(0, 1, 0);
                            float dot = xnaTypes.Vector3.Dot(view, up);
                            if (Math.Abs(dot) > 0.99)
                            {
                                up += new xnaTypes.Vector3(0.1f, 0, 0);
                                up.Normalize();
                            }
                            xnaTypes.Vector3 right = xnaTypes.Vector3.Cross(view, up);
                            view = xnaTypes.Vector3.Multiply(view, 10f);
                            view = xnaTypes.Vector3.Transform(view, xnaTypes.Matrix.CreateFromAxisAngle(up, (float)(-drag.X * Math.PI / 500)));
                            view = xnaTypes.Vector3.Transform(view, xnaTypes.Matrix.CreateFromAxisAngle(right, (float)(-drag.Y * Math.PI / 500)));

                            curCamera.Camera.LookAt = curCamera.Camera.Location + view;
                            break;
                        case DragType.MOVE:
                            var right2 = xnaTypes.Vector3.Cross(view, new xnaTypes.Vector3(0, 1, 0));
                            var up2 = xnaTypes.Vector3.Cross(right2, view);
                            right2 *= (drag.X * 0.05f);
                            up2 *= (-drag.Y * 0.05f);
                            var delta = right2 + up2;
                            curCamera.Camera.LookAt += delta;
                            curCamera.Camera.Location += delta;
                            break;
                    }
                }
            }
            catch (Exception err)
            {
                GUIUtilities.ReportUnexpectedException(err);
            }
        }

        private void OnMouseWheel(object sender, MouseWheelEventArgs e)
        {
            try
            {
                if (curCamera != null)
                {
                    xnaTypes.Vector3 view = curCamera.Camera.LookAt - curCamera.Camera.Location;
                    view.Normalize();
                    view = xnaTypes.Vector3.Multiply(view, e.Delta * 0.01f);
                    curCamera.Camera.LookAt += view;
                    curCamera.Camera.Location += view;
                }
            }
            catch (Exception err)
            {
                GUIUtilities.ReportUnexpectedException(err);
            }
        }

        private void OnMouseDown(object sender, MouseButtonEventArgs e)
        {
            try
            {
                if (!SimulatorContainer.IsMouseCaptured)
                {
                    if (e.ChangedButton == MouseButton.Left)
                        dragType = DragType.LOOK;
                    else if (e.ChangedButton == MouseButton.Right)
                        dragType = DragType.MOVE;
                    var p = e.GetPosition(SimulatorContainer);
                    mouseStart = new Vector2((float)p.X, (float)p.Y);
                    SimulatorContainer.CaptureMouse();
                }
            }
            catch (Exception err)
            {
                GUIUtilities.ReportUnexpectedException(err);
            }
        }

        private void OnMouseUp(object sender, MouseButtonEventArgs e)
        {
            try
            {
                SimulatorContainer.ReleaseMouseCapture();
            }
            catch (Exception err)
            {
                GUIUtilities.ReportUnexpectedException(err);
            }
        }

        private void SimulatorContainer_Loaded(object sender, RoutedEventArgs e)
        {
            try
            {
                initBitmap();
            }
            catch (Exception err)
            {
                GUIUtilities.ReportUnexpectedException(err);
            }
        }

        private void AddCamera_Button_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                AddContainer.Child = (UIElement)AddContainer.FindResource("textbox");
                AddContainer.Child.Focus();
            }
            catch (Exception err)
            {
                GUIUtilities.ReportUnexpectedException(err);
            }
        }

        private void TextBox_PreviewTextInput(object sender, TextCompositionEventArgs e)
        {
            try
            {
                var textbox = sender as TextBox;
                if (e.Text.EndsWith("\n") || e.Text.EndsWith("\r"))
                {
                    if (textbox.Text.Length > 0)
                        addCameraWatch(textbox.Text, true);
                    AddContainer.Child = (UIElement)AddContainer.FindResource("button");
                }
            }
            catch (Exception err)
            {
                GUIUtilities.ReportUnexpectedException(err);
            }
        }

        private void TextBox_PreviewKeyDown(object sender, KeyEventArgs e)
        {
            try
            {
                if (e.Key == Key.Escape)
                    AddContainer.Child = (UIElement)AddContainer.FindResource("button");
            }
            catch (Exception err)
            {
                GUIUtilities.ReportUnexpectedException(err);
            }
        }
    }
}
