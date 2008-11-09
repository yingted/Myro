using System;

namespace IronEditor.UI.WinForms
{
    public static class MonoEnvironment
    {
        public static bool IsRunningOnMono()
        {
            //http://www.mono-project.com/Guide:_Porting_Winforms_Applications
            return Type.GetType("Mono.Runtime") != null;
        }
    }
}
