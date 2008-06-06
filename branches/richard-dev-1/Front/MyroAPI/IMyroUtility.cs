using System;
using System.Collections.Generic;
using System.Text;

namespace Myro.API
{
    interface IMyroGuiUtility
    {
        string Ask(string prompt);
        string AskQuestion(string prompt); // REVIEW (jaredj): AskYesNo might be a more appropriate name
        System.IO.Directory PickAFolder();
        System.IO.File PickAFile();
    }

    interface IMyroUtility
    {
        void Wait(int seconds); // REVIEW (jaredj): Should we offer milliseconds as well?
        DateTime CurrentTime { get; }
        string FlipCoin();
        bool FlipCoinBool();
        float RandomNumber();
    }
}
