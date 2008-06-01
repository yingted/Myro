using System;
using System.IO;

namespace Scheme
{
    // Environment
    public class Interpreter
    {
        public static void Repl(string loadFile)
        {
            int start = Environment.TickCount;

            A_Program prog = null;
            for (int i = 0; i < 10; i++)
            {
                prog = new A_Program();
		// FIXME; don't load if flag
                prog.LoadEmbededInitScheme();
            }

            int end = Environment.TickCount;

            if (loadFile != null)
            {
                String init = File.OpenText(loadFile).ReadToEnd();
                prog.Eval(new StringReader(init));
            }
            else
            {
                while (true)
                {
                    StreamWriter str = null;
		    //str = new StreamWriter("transcript.ss", true);
                    try 
                    {
                        Console.WriteLine("(" + (end - start) + " ms)");
                        Console.Write("> ");
                    
                        String val = Console.ReadLine();

			if (str != null)
			    str.WriteLine(val);

                        start = Environment.TickCount;
                        object result = prog.Eval(new StringReader(val));
                        end = Environment.TickCount;
                        
                        Console.WriteLine(result);
                    } 
                    catch (Exception e) 
                    {
                        Console.WriteLine("Scheme Error: " + e.Message); // + e.Message); // .Message);
                        Console.WriteLine("Stacktrace: " + e.StackTrace);
                    }
		    if (str != null)
			str.Close();
                }
            }
        }

        static void Main(string[] args)
        {
            if (args.Length > 0)
                Repl(args[0]);
            else    
                Repl(null);
        }
    }

}