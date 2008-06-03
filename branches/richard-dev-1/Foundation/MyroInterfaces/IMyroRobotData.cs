using System;
using System.Collections.Generic;
using System.Text;

namespace MyroInterfaces
{
    public interface IMyroRobotData
    {
        string Name { get; set; }
        float Volume { get; set; }
        string Info { get; }
    }
}
