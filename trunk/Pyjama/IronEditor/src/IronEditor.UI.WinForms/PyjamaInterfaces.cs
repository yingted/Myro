/*********************************************************************
 *
 * Copyright (c) 2008 Douglas S. Blank
 *
 * This source code is subject to terms and conditions of the
 * Microsoft Public License. A copy of the license can be found in the
 * License.html file at the root of this distribution. If you cannot
 * locate the Microsoft Public License, please send an email to
 * dlr@microsoft.com. By using this source code in any fashion, you
 * are agreeing to be bound by the terms of the Microsoft Public
 * License.
 *
 * You must not remove this notice, or any other, from this software.
 *
 *********************************************************************/

namespace PyjamaInterfaces
{
    public interface IEditable
    {
        void Cut();
        void Copy();
        void Paste();
        void Delete();
    }
    
    public interface IDocument: IEditable
    {
        // Defines the Document interface
        string GetShortName();
        bool GetModified();
        void SetModified(bool value);
        void Save();
        void SaveAs(string value);
        void SetFilename(string value);
        string GetFilename();
        bool GetDirty();
        int GetPage();
        void SetPage(int page);
        int GetSize();
        void Print();
        //bool Untitled { get; }
    }

    public interface IShell
    {
        // Defines the Shell interface
        void ExecuteFile(string filename);
        void EvaluateExp(string expression);
        void Restart();
        void Quit();
        void Print();
        string command(string line);
    }
}

