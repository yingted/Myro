using System;

namespace IronEditor.Engine
{
    internal class UnsupportedDynamicLanuage : Exception
    {
        public UnsupportedDynamicLanuage(string languageName) : base(languageName)
        {
            
        }
    }
}