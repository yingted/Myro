using Gtk;

namespace PyjamaInterfaces
{
    public interface IDocument
    {
	// Defines the Document interface
	string GetShortName();
	bool GetModified();
	void SetModified(bool value);
	void Save();
	void SaveAs(string value);
	void SetFilename(string value);
	string GetFilename();
	Widget GetView();
        bool GetDirty();
	int GetPage();
	void SetPage(int page);
	int GetSize();
    }

    public interface IShell
    {
	// Defines the Shell interface
	void ExecuteFile(string filename);
	void EvaluateExp(string expression);
	void Restart();
	void Quit();
	Widget GetView();
    }
}

