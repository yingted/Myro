## Makefile for building Pyjama with Mono
MONOPATH=/usr/local/bin

pyjama.exe: 
	MONO_PATH=../libs \
	$(MONOPATH)/gmcs \
	./ActiveCodeFile.cs \
	./ApplicationInformation.cs \
	./ApplicationOptions.cs \
	./BaseDialog.Designer.cs \
	./BaseDialog.cs \
	./BaseForm.Designer.cs \
	./BaseForm.cs \
	./CodeBlock.cs \
	./Controls/Console.Designer.cs \
	./Controls/Console.cs \
	./Controls/Document.Designer.cs \
	./Controls/Document.cs \
	./Controls/DocumentInput.cs \
	./Controls/DocumentManager.Designer.cs \
	./Controls/DocumentManager.cs \
	./Controls/DocumentPage.cs \
	./Controls/IDocument.cs \
	./Dialogs/AboutDialog.Designer.cs \
	./Dialogs/AboutDialog.cs \
	./Dialogs/OptionFrames/FontControl.Designer.cs \
	./Dialogs/OptionFrames/FontControl.cs \
	./Dialogs/OptionsDialog.Designer.cs \
	./Dialogs/OptionsDialog.cs \
	./IMainForm.cs \
	./LanguageSettings.cs \
	./MonoEnvironment.cs \
	./Program.cs \
	./Properties/AssemblyInfo.cs \
	./Properties/Resources.Designer.cs \
	./Properties/Settings.Designer.cs \
	./PyjamaForm.Designer.cs \
	./PyjamaForm.cs \
	./PyjamaFormController.cs \
	./PyjamaInterfaces.cs \
	./ShortcutKeyDispatcher.cs \
	./UserSettings.cs \
	-main:Pyjama.Program \
	-out:pyjama.exe \
	-r:Microsoft.VisualBasic.dll \
	-r:System.Windows.Forms \
	-r:System.Drawing \
	-r:Mono.Security \
	-r:IronTextBox.dll \
	-r:IronPython.dll \
	-r:IronPython.Modules.dll \
	-r:Microsoft.Scripting.Core.dll \
	-r:Microsoft.Scripting.dll \
	-res:res/Pyjama.FontControl.resources \
	-res:res/Pyjama.AboutDialog.resources \
	-res:res/Pyjama.OptionsDialog.resources \
	-res:res/Pyjama.DocumentPage.resources \
	-res:res/Pyjama.DocumentManager.resources \
	-res:res/Pyjama.Console.resources \
	-res:res/Pyjama.BaseDialog.resources \
	-res:res/Pyjama.BaseForm.resources \
	-res:res/Pyjama.PyjamaForm.resources \
	-res:res/Pyjama.Properties.Resources.resources \
	-res:Properties/Settings.settings

resx: 
	mkdir -p res
	resgen Dialogs/OptionFrames/FontControl.resx res/Pyjama.FontControl.resources
	resgen Dialogs/AboutDialog.resx res/Pyjama.AboutDialog.resources
	resgen Dialogs/OptionsDialog.resx res/Pyjama.OptionsDialog.resources
	resgen Controls/DocumentPage.resx res/Pyjama.DocumentPage.resources
	resgen Controls/DocumentManager.resx res/Pyjama.DocumentManager.resources
	resgen Controls/Console.resx res/Pyjama.Console.resources
	resgen BaseDialog.resx res/Pyjama.BaseDialog.resources
	resgen BaseForm.resx res/Pyjama.BaseForm.resources
	resgen PyjamaForm.resx res/Pyjama.PyjamaForm.resources
	cd Properties; MONO_IOMAP=all resgen Resources.resx ../res/Pyjama.Properties.Resources.resources

run: pyjama.exe
	XMODIFIERS= MONO_PATH=../libs $(MONOPATH)/mono pyjama.exe

clean:
	$(RM) res/Pyjama.BaseDialog.resources \
		res/Pyjama.BaseForm.resources \
		res/Pyjama.Console.resources \
		res/Pyjama.DocumentManager.resources \
		res/Pyjama.DocumentPage.resources \
		res/Pyjama.AboutDialog.resources \
		res/Pyjama.FontControl.resources \
		res/Pyjama.OptionsDialog.resources \
		res/Pyjama.Resources.resources \
		res/Pyjama.PyjamaForm.resources \
		pyjama.exe

