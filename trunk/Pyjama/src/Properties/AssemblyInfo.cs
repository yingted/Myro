using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// General Information about an assembly is controlled through the following 
// set of attributes. Change these attribute values to modify the information
// associated with an assembly.
[assembly: AssemblyTitle("Pyjama")]

#if (DEBUG)
[assembly: AssemblyDescription("Pyjama Application (Debug)")]
    [assembly: AssemblyConfiguration("Debug")]
#else
[assembly: AssemblyDescription("Python and Ruby IDE (Release)")]
    [assembly: AssemblyConfiguration("Release")]
#endif

[assembly: AssemblyCompany("RobotEducation.org")]
[assembly: AssemblyProduct("Pyjama Editor")]
[assembly: AssemblyCopyright("Copyright © 2009")]
[assembly: AssemblyTrademark("")]
[assembly: AssemblyCulture("")]

// Setting ComVisible to false makes the types in this assembly not visible 
// to COM components.  If you need to access a type in this assembly from 
// COM, set the ComVisible attribute to true on that type.
[assembly: ComVisible(false)]

// The following GUID is for the ID of the typelib if this project is exposed to COM
[assembly: Guid("53d6fbc4-26b2-4c21-b62b-5f3a25997d1d")]

// Version information for an assembly consists of the following four values:
//
//      Major Version
//      Minor Version 
//      Build Number
//      Revision
//
// You can specify all the values or you can default the Build and Revision Numbers 
// by using the '*' as shown below:
// [assembly: AssemblyVersion("1.0.*")]
[assembly: AssemblyVersion("1.0.*")]
