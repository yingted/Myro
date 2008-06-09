using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.Dss.Core.Manifest;
using System.Xml.Serialization;

namespace XmlTest
{
    class Program
    {
        static void Main(string[] args)
        {
            ServiceRecordType rec = new ServiceRecordType() { Contract = "http://contracts.org/contract" };
            XmlSerializer ser = new XmlSerializer(typeof(ServiceRecordType));
            ser.Serialize(Console.Out, rec);
            Console.ReadLine();
        }
    }
}
