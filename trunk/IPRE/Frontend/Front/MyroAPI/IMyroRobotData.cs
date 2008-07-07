using System;
using System.Collections.Generic;
using System.Text;

namespace Myro.API
{
    public interface IMyroRobotData
    {
        string Name { get; set; }
        float Volume { get; set; }
        string Info { get; }
    }
}
