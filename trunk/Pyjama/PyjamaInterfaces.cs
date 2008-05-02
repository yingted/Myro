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
    }
}

