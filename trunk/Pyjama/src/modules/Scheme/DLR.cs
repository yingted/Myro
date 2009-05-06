using System;
using System.ComponentModel;
using System.Collections.Generic;
using System.Reflection;
using System.Text;

using Microsoft.Scripting;
using Microsoft.Scripting.Hosting;
using Microsoft.Scripting.Ast;
using Microsoft.Scripting.Actions;
using Microsoft.Scripting.Runtime;
using Microsoft.Scripting.Utils;

public class SchemeGlobalContext {
  Dictionary<string, object> members;
  
  public SchemeGlobalContext() {
	this.members = new Dictionary<string, object>();
  }
  
  public bool HasMember(string name) {
	return members.ContainsKey(name);
  }

  public virtual void Set(string name, object value)
  {
	if (members.ContainsKey(name))
	  members.Remove(name);
	members.Add(name, value);
  }
  
  public virtual object Get(string name)
  {
	if (!members.ContainsKey(name))
	  return null;
	return members[name];
  }
  
  public override string ToString()
  {
	bool first = true;
	StringBuilder res = new StringBuilder("{");
	foreach (KeyValuePair<string, object> member in members) {
	  if (!first)
		res.Append(", ");
	  res.Append(member.Key);
	  res.Append(": ");
	  if (member.Value == this)
		res.Append("this");
	  else
		res.Append(member.Value.ToString());
	  first = false;
	}
	res.Append("}");
	
	return res.ToString();
  }
}

public class SchemeContext {
  static SchemeGlobalContext @this = null;

  public static SchemeGlobalContext GetContext() {
	if (@this == null)
	{
	  @this = new SchemeGlobalContext();
	  @this.Set("this", @this);
	}
	return @this;
  }
  
  /// <summary>
  /// Load package in context.
  /// </summary>
  /// <param name="context">DLR context for call</param>
  /// <param name="name">library name</param>
  /// <returns>package</returns>
  public static object UseLibrary(CodeContext context, string name)
  {
	object value;
	if (context.LanguageContext.DomainManager.Globals.TryGetName(SymbolTable.StringToId(name), out value))
	  return value;
	return null;
  }
}

/*
class SchemeCommandLine : CommandLine {
  protected override string Logo {
    get {
	  return "Scheme Command line\nCopyright (c) Douglas Blank 2008\n\n";
	}
  }
  
  protected override string Prompt {
	get {
	  return "scheme> ";
	}
  }
}
*/

public class SchemeLanguageContext : LanguageContext  {

  static public readonly Guid SchemeGUID = new Guid("11111111-1111-1111-1111-111111111111");
  
  public SchemeLanguageContext(ScriptDomainManager manager) : base(manager) {
	//Binder = new SchemeBinder(new CodeContext(new Scope(this), this));
	//SchemeLibrary.Initialize();
  }
  
  public override LambdaExpression ParseSourceCode(CompilerContext context) {
	//Parser parser = new Parser(context.SourceUnit);
	//parser.Parse();
	
	// Return the generated AST
	//return parser.Result;
	return null;
  }

  public override Guid LanguageGuid {
	get {
	  return SchemeGUID;
	}
  }
  
  public override string DisplayName {
	get { return "Scheme"; }
  }
  
  public override Version LanguageVersion {
	get { return new Version(0, 1, 0, 0); }
  }
  
  public override bool TryLookupGlobal(CodeContext context, SymbolId name, out object value) {
	// Look in Scheme global context
	SchemeGlobalContext schemeContext = SchemeContext.GetContext();
	string memberName = SymbolTable.IdToString(name);
	if (schemeContext.HasMember(memberName))
	{
	  value = schemeContext.Get(memberName);
	  return true;
	}
	return base.TryLookupGlobal(context, name, out value);
  }
  
  public override ServiceType GetService<ServiceType>(params object[] args) {
	// Customized logo & prompt
	/*
	if (typeof(ServiceType) == typeof(CommandLine))
	{
	  return (ServiceType)(object)new SchemeCommandLine();
	}

	// Standard parser option
	else if (typeof(ServiceType) == typeof(OptionsParser))
	{
	  return (ServiceType)(object)new DefaultOptionsParser(this);
	}
	*/
    
	return base.GetService<ServiceType>(args);
  }
  
  public override string FormatException(Exception exception) {
	// Format syntax error
	if (exception is SyntaxErrorException)
	{
	  // Display line and column
	  SyntaxErrorException syntax = (SyntaxErrorException)exception;
	  StringBuilder msg = new StringBuilder();
	  msg.Append("(");
	  msg.Append(syntax.Line);
	  msg.Append(",");
	  msg.Append(syntax.Column);
	  msg.Append("): ");
	  msg.Append(syntax.Message);
	  return msg.ToString();
	}
	
	// Just display message
	else
	  return exception.Message;
  }
}

namespace n1 {
  public class MyType1 {
	public string Name { get; set; }
	public MyType1() {
	  Name = "MyType1";
	}

	public override string ToString() {
	  return Name;
	}
  }
}

public class SchemeHost {

// 	public static object Import(CodeContext context, string name) {
// 	  object value;
// 	  if (context.LanguageContext.DomainManager.Globals.TryGetName(SymbolTable.StringToId(name), out value)) {
// 		return value;
// 	  }
// 	  return null;
// 	}

  public static void Main(string[] args) {
	// Create runtime
	ScriptRuntime runtime = ScriptRuntime.Create();
	
	// Preload System assembly
	runtime.LoadAssembly(typeof(string).Assembly);
	// To make it soe that the language can create these objects:
	//runtime.LoadAssembly( Assembly.GetAssembly( typeof( n1.MyType1)));
	
	// Create a scope at global level
	ScriptScope globalEnv = runtime.CreateScope();
	
	// Load Scheme Engine
	ScriptEngine schemeEngine = runtime.GetEngine(typeof(SchemeLanguageContext));
	
	//schemeEngine.Runtime.Host.PlatformAdaptationLayer.LoadAssembly("System");
	//schemeEngine.Runtime.Host.PlatformAdaptationLayer.LoadAssemblyFromPath("External.dll");
	//env.Host.PlatformAdaptationLayer.LoadAssembly("System");

// 	object value;
// 	ScriptScope scope = schemeEngine.Runtime.CreateScope();
// 	scope.SetVariable("x", 1000);
// 	bool loaded = scope.TryGetVariable("System", out value);
// 	if (loaded)
// 	  Console.WriteLine(value);
// 	else
// 	  Console.WriteLine("error");


	ScriptRuntime env = ScriptRuntime.Create();
	Console.WriteLine(env.Globals);
	env.Globals.SetVariable("x", 42);
	env.Globals.SetVariable("y", "test");
	Console.WriteLine(String.Format("x = {0}", env.Globals.GetVariable("x")));
	Console.WriteLine(String.Format("y = {0}", env.Globals.GetVariable("y")));

	Assembly assembly = Assembly.LoadWithPartialName("System");
	env.Globals.SetVariable("System", assembly);
	Console.WriteLine(String.Format("System = {0}", env.Globals.GetVariable("System")));


  }

}