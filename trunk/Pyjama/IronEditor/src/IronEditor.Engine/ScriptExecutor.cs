using System;
using System.IO;
using Microsoft.Scripting;
using Microsoft.Scripting.Hosting;
using Microsoft.Scripting.Runtime;

namespace IronEditor.Engine
{
    public class ScriptExecutor
    {
        private ScriptEngine Engine;
        private TextWriter Writer;

        public ScriptExecutor(ScriptEngine engine, TextWriter writer)
        {
            Engine = engine;
            Writer = writer;
            TextWriterStream textStreamWriter = new TextWriterStream(Writer);
            Engine.Runtime.IO.SetOutput(textStreamWriter, writer);
            Engine.Runtime.IO.SetErrorOutput(textStreamWriter, writer);
            Console.SetOut(writer);
            Console.SetError(writer);
        }

        public void ExecuteStatement(string code)
        {
            ScriptScope scope = Engine.CreateScope();
            ScriptSource source = Engine.CreateScriptSourceFromString(code, SourceCodeKind.Statements);

            //Snippets.Shared.SaveSnippets = true;
            //Snippets.Shared.SnippetsDirectory = Directory.GetCurrentDirectory();
            //Snippets.Shared.SnippetsFileName = "SnippetsSaved";
            try
            {
                source.Execute(scope);
            }
            catch (SyntaxErrorException ex)
            {
                WriteSyntaxException(ex);
            }
            catch (UnboundNameException ex2)
            {
                WriteSyntaxException(ex2);
            }
            catch (Exception unhandled)
            {
                //Snippets.Shared.Dump();

                WriteUnhandledException(unhandled);
            }
        }

        private void WriteUnhandledException(Exception unhandled)
        {
            Writer.WriteLine("Unhandled Exception: " + unhandled.ToString() + " : " + unhandled.Message);
        }

        private void WriteSyntaxException(UnboundNameException exception)
        {
            Writer.WriteLine("Error: " + exception.Message);
        }

        private void WriteSyntaxException(SyntaxErrorException exception)
        {
            Writer.WriteLine("Error: " + exception.Message);
        }

        public void ExecuteFile(string filePath)
        {
            ScriptSource source = Engine.CreateScriptSourceFromFile(filePath);
            source.Execute();
        }
    }
}
