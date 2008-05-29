using System;
using Gtk;
using GtkSharp;
using Gnome;

class PrintText
{
	TextView tv;
	
	public PrintText(TextView tv)
	{
		this.tv = new TextView();
		this.tv.Buffer = tv.Buffer;
		Gtk.Window win = new Gtk.Window("Print Text");
		win.SetDefaultSize(400, 300);
		//win.DeleteEvent += new DeleteEventHandler(OnWinDelete);
		VBox vbox = new VBox(false, 0);
		win.Add(vbox);
		vbox.PackStart(this.tv, true, true, 0);
		Button print = new Button(Gtk.Stock.Print);
		print.Clicked += new EventHandler(OnPrintClicked);
		vbox.PackStart(print, false, true, 0);	
		win.ShowAll();
	}
	
	void MyPrint(Gnome.PrintContext gpc)
	{
		Gnome.Print.Beginpage(gpc, "demo");
		Gnome.Print.Moveto(gpc, 1, 700);
		Gnome.Print.Show(gpc, tv.Buffer.Text);
		Gnome.Print.Showpage(gpc);
	}
	
	void OnPrintClicked(object o, EventArgs args)
	{
		Gnome.PrintJob pj = new Gnome.PrintJob(PrintConfig.Default());
		PrintDialog dialog = new PrintDialog(pj, "Print Test", 0);
		int response = dialog.Run();
		Console.WriteLine("response: " + response);
		if (response == (int) PrintButtons.Cancel) {
			Console.WriteLine("Canceled");
			dialog.Hide();
			dialog.Dispose();
			return;
		}
		Gnome.PrintContext ctx = pj.Context;
		MyPrint (ctx); 
		pj.Close ();
		switch (response) {
		case (int) PrintButtons.Print: 
			pj.Print(); 
			break;
		case (int) PrintButtons.Preview:
			new Gnome.PrintJobPreview(pj, "Print Test").Show();
			break;
		}
		dialog.Hide();
		dialog.Dispose();
	}
}
