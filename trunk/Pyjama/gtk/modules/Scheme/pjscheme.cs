using System;
using Microsoft.VisualBasic.CompilerServices;

public class PJScheme:Scheme
{
   new public static object lit_exp (params object[]args)
   {
      return ((object) PJScheme.
	      cons ((object) symbol ("lit-exp"), (object) args));
   }

   new public static object var_exp (params object[]args)
   {
      return ((object) PJScheme.
	      cons ((object) symbol ("var-exp"), (object) args));
   }

   new public static object if_exp (params object[]args)
   {
      return ((object) PJScheme.
	      cons ((object) symbol ("if-exp"), (object) args));
   }

   new public static object assign_exp (params object[]args)
   {
      return ((object) PJScheme.
	      cons ((object) symbol ("assign-exp"), (object) args));
   }

   new public static object define_exp (params object[]args)
   {
      return ((object) PJScheme.
	      cons ((object) symbol ("define-exp"), (object) args));
   }

   new public static object define_syntax_exp (params object[]args)
   {
      return ((object) PJScheme.
	      cons ((object) symbol ("define-syntax-exp"), (object) args));
   }

   new public static object begin_exp (params object[]args)
   {
      return ((object) PJScheme.
	      cons ((object) symbol ("begin-exp"), (object) args));
   }

   new public static object lambda_exp (params object[]args)
   {
      return ((object) PJScheme.
	      cons ((object) symbol ("lambda-exp"), (object) args));
   }

   new public static object mu_lambda_exp (params object[]args)
   {
      return ((object) PJScheme.
	      cons ((object) symbol ("mu-lambda-exp"), (object) args));
   }

   new public static object app_exp (params object[]args)
   {
      return ((object) PJScheme.
	      cons ((object) symbol ("app-exp"), (object) args));
   }

   new public static object try_catch_exp (params object[]args)
   {
      return ((object) PJScheme.
	      cons ((object) symbol ("try-catch-exp"), (object) args));
   }

   new public static object try_finally_exp (params object[]args)
   {
      return ((object) PJScheme.
	      cons ((object) symbol ("try-finally-exp"), (object) args));
   }

   new public static object try_catch_finally_exp (params object[]args)
   {
      return ((object) PJScheme.
	      cons ((object) symbol ("try-catch-finally-exp"),
		    (object) args));
   }

   new public static object raise_exp (params object[]args)
   {
      return ((object) PJScheme.
	      cons ((object) symbol ("raise-exp"), (object) args));
   }

   new public static object dict_exp (params object[]args)
   {
      return ((object) PJScheme.
	      cons ((object) symbol ("dict-exp"), (object) args));
   }

   static Function pc = null;
   static object action_reg = symbol ("undefined");
   static object args_reg = symbol ("undefined");
   static object bindings_reg = symbol ("undefined");
   static object bodies_reg = symbol ("undefined");
   static object buffer_reg = symbol ("undefined");
   static object chars_reg = symbol ("undefined");
   static object clauses_reg = symbol ("undefined");
   static object components_reg = symbol ("undefined");
   static object datum_list_reg = symbol ("undefined");
   static object datum_reg = symbol ("undefined");
   static object env2_reg = symbol ("undefined");
   static object env_reg = symbol ("undefined");
   static object exception_reg = symbol ("undefined");
   static object exp_reg = symbol ("undefined");
   static object expected_terminator_reg = symbol ("undefined");
   static object exps_reg = symbol ("undefined");
   static object filename_reg = symbol ("undefined");
   static object filenames_reg = symbol ("undefined");
   static object final_reg = symbol ("undefined");
   static object handler_reg = symbol ("undefined");
   static object input_reg = symbol ("undefined");
   static object k2_reg = symbol ("undefined");
   static object k_reg = symbol ("undefined");
   static object keyword_reg = symbol ("undefined");
   static object list1_reg = symbol ("undefined");
   static object list2_reg = symbol ("undefined");
   static object lists_reg = symbol ("undefined");
   static object macro_reg = symbol ("undefined");
   static object p1_reg = symbol ("undefined");
   static object p2_reg = symbol ("undefined");
   static object pair1_reg = symbol ("undefined");
   static object pair2_reg = symbol ("undefined");
   static object pairs_reg = symbol ("undefined");
   static object path_reg = symbol ("undefined");
   static object pattern_reg = symbol ("undefined");
   static object proc_reg = symbol ("undefined");
   static object procs_reg = symbol ("undefined");
   static object s_reg = symbol ("undefined");
   static object sexp_reg = symbol ("undefined");
   static object token_type_reg = symbol ("undefined");
   static object tokens_reg = symbol ("undefined");
   static object value1_reg = symbol ("undefined");
   static object value2_reg = symbol ("undefined");
   static object value_reg = symbol ("undefined");
   static object var_reg = symbol ("undefined");
   static object variable_reg = symbol ("undefined");
   static object vars_reg = symbol ("undefined");
   static object temp_2 = symbol ("undefined");
   static object temp_1 = symbol ("undefined");
   new public static object make_cont (params object[]args)
   {
      return ((object) PJScheme.
	      cons ((object) symbol ("continuation"), (object) args));
   }

   new public static void apply_cont ()
   {
      {
	 object temp_1 = null;
	 temp_1 = PJScheme.cdr ((object) k_reg);
	 if (true_q
	     (PJScheme.
	      Eq ((object) PJScheme.car ((object) temp_1),
		  (object) symbol ("<cont-1>"))))
	   {
	      object token = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 2);
	      token = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value_reg = PJScheme.cons ((object) token, (object) value_reg);
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-2>"))))
	   {
	      final_reg = value_reg;
	      pc = null;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-3>"))))
	   {
	      object chars = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 2);
	      chars = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value2_reg = chars;
	      value1_reg =
		 PJScheme.append ((object) value_reg,
				  (object) PJScheme.
				  list ((object) read_line_count,
					(object) read_char_count));
	      k_reg = k;
	      pc = (Function) apply_cont2;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-4>"))))
	   {
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 2);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont2 ((object) symbol ("<cont2-3>"),
				      (object) handler, (object) k);
	      handler_reg = handler;
	      tokens_reg = value_reg;
	      pc = (Function) read_sexp;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-5>"))))
	   {
	      k_reg = init_cont;
	      handler_reg = init_handler;
	      tokens_reg = value_reg;
	      pc = (Function) print_unparsed_sexps;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-6>"))))
	   {
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value_reg = PJScheme.binding_value ((object) value_reg);
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-7>"))))
	   {
	      object variable = null;
	      object env = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 4);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 3);
	      env = PJScheme.list_ref ((object) temp_1, (object) 2);
	      variable = PJScheme.list_ref ((object) temp_1, (object) 1);
	      if (true_q (value_reg))
		{
		   k_reg = k;
		   handler_reg = handler;
		   env_reg = env;
		   path_reg = "";
		   components_reg = value_reg;
		   pc = (Function) lookup_variable_components;

		}
	      else if (true_q (PJScheme.dlr_env_contains ((object) variable)))
		{
		   value_reg = PJScheme.dlr_env_lookup ((object) variable);
		   k_reg = k;
		   pc = (Function) apply_cont;

		}
	      else
		{
		   exception_reg =
		      PJScheme.format ((object) "unbound variable ~a",
				       (object) variable);
		   handler_reg = handler;
		   pc = (Function) apply_handler;

		}
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-8>"))))
	   {
	      object components = null;
	      object path = null;
	      object var = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 5);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 4);
	      var = PJScheme.list_ref ((object) temp_1, (object) 3);
	      path = PJScheme.list_ref ((object) temp_1, (object) 2);
	      components = PJScheme.list_ref ((object) temp_1, (object) 1);
	      if (true_q
		  (PJScheme.
		   null_q ((object) PJScheme.cdr ((object) components))))
		{
		   k_reg = k;
		   pc = (Function) apply_cont;

		}
	      else
		{
		   object result = null;
		   object new_path = null;
		   new_path =
		      ((PJScheme.
			string_is__q ((object) path,
				      (object) "")) ? (PJScheme.
						       format ((object) "~a",
							       (object) var))
		       : (PJScheme.
			  format ((object) "~a.~a", (object) path,
				  (object) var)));
		   result = PJScheme.binding_value ((object) value_reg);
		   if (true_q
		       (PJScheme.
			not ((object) PJScheme.
			     environment_q ((object) result))))
		     {
			exception_reg =
			   PJScheme.format ((object) "~a is not a module",
					    (object) new_path);
			handler_reg = handler;
			pc = (Function) apply_handler;

		     }
		   else
		     {
			k_reg = k;
			handler_reg = handler;
			env_reg = result;
			path_reg = new_path;
			components_reg = PJScheme.cdr ((object) components);
			pc = (Function) lookup_variable_components;

		     }
		}
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-9>"))))
	   {
	      object datum = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 3);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 2);
	      datum = PJScheme.list_ref ((object) temp_1, (object) 1);
	      if (true_q (PJScheme.pattern_macro_q ((object) value_reg)))
		{
		   k_reg = k;
		   handler_reg = handler;
		   datum_reg = datum;
		   clauses_reg = PJScheme.macro_clauses ((object) value_reg);
		   pc = (Function) process_macro_clauses;

		}
	      else
		{
		   k_reg = k;
		   datum_reg = datum;
		   macro_reg = value_reg;
		   pc = (Function) apply_macro;

		}
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-10>"))))
	   {
	      object clauses = null;
	      object datum = null;
	      object right_pattern = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 5);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 4);
	      right_pattern = PJScheme.list_ref ((object) temp_1, (object) 3);
	      datum = PJScheme.list_ref ((object) temp_1, (object) 2);
	      clauses = PJScheme.list_ref ((object) temp_1, (object) 1);
	      if (true_q (value_reg))
		{
		   k_reg = k;
		   s_reg = value_reg;
		   pattern_reg = right_pattern;
		   pc = (Function) instantiate;

		}
	      else
		{
		   k_reg = k;
		   handler_reg = handler;
		   datum_reg = datum;
		   clauses_reg = PJScheme.cdr ((object) clauses);
		   pc = (Function) process_macro_clauses;

		}
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-11>"))))
	   {
	      object bindings = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 2);
	      bindings = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value_reg =
		 PJScheme.list ((object) symbol ("let"),
				(object) PJScheme.list ((object) PJScheme.
							car ((object)
							     bindings)),
				(object) value_reg);
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-12>"))))
	   {
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value_reg =
		 PJScheme.cons ((object) symbol ("cond"), (object) value_reg);
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-13>"))))
	   {
	      object clauses = null;
	      object var = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 3);
	      var = PJScheme.list_ref ((object) temp_1, (object) 2);
	      clauses = PJScheme.list_ref ((object) temp_1, (object) 1);
	      {
		 object clause = null;
		 clause = PJScheme.car ((object) clauses);
		 if (true_q
		     (PJScheme.
		      Eq ((object) PJScheme.car ((object) clause),
			  (object) symbol ("else"))))
		   {
		      value_reg =
			 PJScheme.cons ((object) clause, (object) value_reg);
		      k_reg = k;
		      pc = (Function) apply_cont;

		   }
		 else
		    if (true_q
			(PJScheme.
			 symbol_q ((object) PJScheme.car ((object) clause))))
		   {
		      value_reg =
			 PJScheme.cons ((object) PJScheme.
					cons ((object) PJScheme.
					      list ((object) symbol ("eq?"),
						    (object) var,
						    (object) PJScheme.
						    list ((object)
							  symbol ("quote"),
							  (object) PJScheme.
							  car ((object)
							       clause))),
					      (object) PJScheme.
					      cdr ((object) clause)),
					(object) value_reg);
		      k_reg = k;
		      pc = (Function) apply_cont;

		   }
		 else
		   {
		      value_reg =
			 PJScheme.cons ((object) PJScheme.
					cons ((object) PJScheme.
					      list ((object) symbol ("memq"),
						    (object) var,
						    (object) PJScheme.
						    list ((object)
							  symbol ("quote"),
							  (object) PJScheme.
							  car ((object)
							       clause))),
					      (object) PJScheme.
					      cdr ((object) clause)),
					(object) value_reg);
		      k_reg = k;
		      pc = (Function) apply_cont;

		   }
	      }
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-14>"))))
	   {
	      object v1 = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 2);
	      v1 = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value_reg = PJScheme.app_exp ((object) v1, (object) value_reg);
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-15>"))))
	   {
	      object datum = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 3);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 2);
	      datum = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-14>"),
				     (object) value_reg, (object) k);
	      handler_reg = handler;
	      datum_list_reg = PJScheme.cdr ((object) datum);
	      pc = (Function) parse_all;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-16>"))))
	   {
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value_reg = PJScheme.dict_exp ((object) value_reg);
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-17>"))))
	   {
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value_reg = PJScheme.raise_exp ((object) value_reg);
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-18>"))))
	   {
	      object cexps = null;
	      object datum = null;
	      object body = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 4);
	      body = PJScheme.list_ref ((object) temp_1, (object) 3);
	      datum = PJScheme.list_ref ((object) temp_1, (object) 2);
	      cexps = PJScheme.list_ref ((object) temp_1, (object) 1);
	      {
		 object cvar = null;
		 cvar =
		    PJScheme.catch_var ((object) PJScheme.
					caddr ((object) datum));
		 value_reg =
		    PJScheme.try_catch_finally_exp ((object) body,
						    (object) cvar,
						    (object) cexps,
						    (object) value_reg);
		 k_reg = k;
		 pc = (Function) apply_cont;
	      }
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-19>"))))
	   {
	      object datum = null;
	      object body = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 4);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 3);
	      body = PJScheme.list_ref ((object) temp_1, (object) 2);
	      datum = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-18>"),
				     (object) value_reg, (object) datum,
				     (object) body, (object) k);
	      handler_reg = handler;
	      datum_list_reg =
		 PJScheme.finally_exps ((object) PJScheme.
					cadddr ((object) datum));
	      pc = (Function) parse_all;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-20>"))))
	   {
	      object datum = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 3);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 2);
	      datum = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-19>"),
				     (object) datum, (object) value_reg,
				     (object) handler, (object) k);
	      handler_reg = handler;
	      datum_list_reg =
		 PJScheme.catch_exps ((object) PJScheme.
				      caddr ((object) datum));
	      pc = (Function) parse_all;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-21>"))))
	   {
	      object body = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 2);
	      body = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value_reg =
		 PJScheme.try_finally_exp ((object) body, (object) value_reg);
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-22>"))))
	   {
	      object datum = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 3);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 2);
	      datum = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-21>"),
				     (object) value_reg, (object) k);
	      handler_reg = handler;
	      datum_list_reg =
		 PJScheme.finally_exps ((object) PJScheme.
					caddr ((object) datum));
	      pc = (Function) parse_all;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-23>"))))
	   {
	      object datum = null;
	      object body = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 3);
	      body = PJScheme.list_ref ((object) temp_1, (object) 2);
	      datum = PJScheme.list_ref ((object) temp_1, (object) 1);
	      {
		 object cvar = null;
		 cvar =
		    PJScheme.catch_var ((object) PJScheme.
					caddr ((object) datum));
		 value_reg =
		    PJScheme.try_catch_exp ((object) body, (object) cvar,
					    (object) value_reg);
		 k_reg = k;
		 pc = (Function) apply_cont;
	      }
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-24>"))))
	   {
	      object datum = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 3);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 2);
	      datum = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-23>"),
				     (object) datum, (object) value_reg,
				     (object) k);
	      handler_reg = handler;
	      datum_list_reg =
		 PJScheme.catch_exps ((object) PJScheme.
				      caddr ((object) datum));
	      pc = (Function) parse_all;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-25>"))))
	   {
	      object datum = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 2);
	      datum = PJScheme.list_ref ((object) temp_1, (object) 1);
	      if (true_q
		  (PJScheme.list_q ((object) PJScheme.cadr ((object) datum))))
		{
		   value_reg =
		      PJScheme.lambda_exp ((object) PJScheme.
					   cadr ((object) datum),
					   (object) value_reg);
		   k_reg = k;
		   pc = (Function) apply_cont;

		}
	      else
		{
		   value_reg =
		      PJScheme.mu_lambda_exp ((object) PJScheme.
					      head ((object) PJScheme.
						    cadr ((object) datum)),
					      (object) PJScheme.
					      last ((object) PJScheme.
						    cadr ((object) datum)),
					      (object) value_reg);
		   k_reg = k;
		   pc = (Function) apply_cont;

		}
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-26>"))))
	   {
	      object datum = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 3);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 2);
	      datum = PJScheme.list_ref ((object) temp_1, (object) 1);
	      if (true_q (PJScheme.null_q ((object) value_reg)))
		{
		   exception_reg =
		      PJScheme.format ((object) "bad concrete syntax: ~a",
				       (object) datum);
		   handler_reg = handler;
		   pc = (Function) apply_handler;

		}
	      else
		 if (true_q
		     (PJScheme.
		      null_q ((object) PJScheme.cdr ((object) value_reg))))
		{
		   value_reg = PJScheme.car ((object) value_reg);
		   k_reg = k;
		   pc = (Function) apply_cont;

		}
	      else
		{
		   value_reg = PJScheme.begin_exp ((object) value_reg);
		   k_reg = k;
		   pc = (Function) apply_cont;

		}
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-27>"))))
	   {
	      object datum = null;
	      object body = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 3);
	      body = PJScheme.list_ref ((object) temp_1, (object) 2);
	      datum = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value_reg =
		 PJScheme.define_exp ((object) PJScheme.cadr ((object) datum),
				      (object) PJScheme.
				      list ((object) value_reg,
					    (object) body));
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-28>"))))
	   {
	      object datum = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 3);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 2);
	      datum = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-27>"),
				     (object) datum, (object) value_reg,
				     (object) k);
	      handler_reg = handler;
	      datum_reg = PJScheme.caddr ((object) datum);
	      pc = (Function) parse;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-29>"))))
	   {
	      object datum = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 2);
	      datum = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value_reg =
		 PJScheme.define_exp ((object) PJScheme.cadr ((object) datum),
				      (object) PJScheme.
				      list ((object) value_reg));
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-30>"))))
	   {
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 2);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg = k;
	      handler_reg = handler;
	      datum_reg = value_reg;
	      pc = (Function) parse;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-31>"))))
	   {
	      object datum = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 2);
	      datum = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value_reg =
		 PJScheme.assign_exp ((object) PJScheme.cadr ((object) datum),
				      (object) value_reg);
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-32>"))))
	   {
	      object v1 = null;
	      object v2 = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 3);
	      v2 = PJScheme.list_ref ((object) temp_1, (object) 2);
	      v1 = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value_reg =
		 PJScheme.if_exp ((object) v1, (object) v2,
				  (object) value_reg);
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-33>"))))
	   {
	      object datum = null;
	      object v1 = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 4);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 3);
	      v1 = PJScheme.list_ref ((object) temp_1, (object) 2);
	      datum = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-32>"),
				     (object) v1, (object) value_reg,
				     (object) k);
	      handler_reg = handler;
	      datum_reg = PJScheme.cadddr ((object) datum);
	      pc = (Function) parse;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-34>"))))
	   {
	      object datum = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 3);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 2);
	      datum = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-33>"),
				     (object) datum, (object) value_reg,
				     (object) handler, (object) k);
	      handler_reg = handler;
	      datum_reg = PJScheme.caddr ((object) datum);
	      pc = (Function) parse;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-35>"))))
	   {
	      object v1 = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 2);
	      v1 = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value_reg =
		 PJScheme.if_exp ((object) v1, (object) value_reg,
				  (object) PJScheme.lit_exp ((object) false));
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-36>"))))
	   {
	      object datum = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 3);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 2);
	      datum = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-35>"),
				     (object) value_reg, (object) k);
	      handler_reg = handler;
	      datum_reg = PJScheme.caddr ((object) datum);
	      pc = (Function) parse;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-37>"))))
	   {
	      object a = null;
	      object b = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 3);
	      b = PJScheme.list_ref ((object) temp_1, (object) 2);
	      a = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value_reg =
		 PJScheme.cons ((object) PJScheme.
				list ((object) a, (object) b),
				(object) value_reg);
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-38>"))))
	   {
	      object a = null;
	      object pairs = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 4);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 3);
	      pairs = PJScheme.list_ref ((object) temp_1, (object) 2);
	      a = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-37>"),
				     (object) a, (object) value_reg,
				     (object) k);
	      handler_reg = handler;
	      pairs_reg = PJScheme.cdr ((object) pairs);
	      pc = (Function) parse_pairs;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-39>"))))
	   {
	      object pairs = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 3);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 2);
	      pairs = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-38>"),
				     (object) value_reg, (object) pairs,
				     (object) handler, (object) k);
	      handler_reg = handler;
	      datum_reg = PJScheme.cadar ((object) pairs);
	      pc = (Function) parse;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-40>"))))
	   {
	      object a = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 2);
	      a = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value_reg = PJScheme.cons ((object) a, (object) value_reg);
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-41>"))))
	   {
	      object datum_list = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 3);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 2);
	      datum_list = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-40>"),
				     (object) value_reg, (object) k);
	      handler_reg = handler;
	      datum_list_reg = PJScheme.cdr ((object) datum_list);
	      pc = (Function) parse_all;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-42>"))))
	   {
	      object v1 = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 2);
	      v1 = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value_reg =
		 PJScheme.list ((object) symbol ("cons"), (object) v1,
				(object) value_reg);
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-43>"))))
	   {
	      object datum = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 3);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 2);
	      datum = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-42>"),
				     (object) value_reg, (object) k);
	      handler_reg = handler;
	      datum_reg = PJScheme.cdr ((object) datum);
	      pc = (Function) expand_quasiquote;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-44>"))))
	   {
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value_reg =
		 PJScheme.cons ((object) symbol ("list"), (object) value_reg);
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-45>"))))
	   {
	      object datum = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 2);
	      datum = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value_reg =
		 PJScheme.list ((object) symbol ("append"),
				(object) PJScheme.cadr ((object) PJScheme.
							car ((object) datum)),
				(object) value_reg);
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-46>"))))
	   {
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value_reg =
		 PJScheme.list ((object) symbol ("list->vector"),
				(object) value_reg);
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-47>"))))
	   {
	      object v1 = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 2);
	      v1 = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value_reg = PJScheme.cons ((object) v1, (object) value_reg);
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-48>"))))
	   {
	      object datum = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 3);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 2);
	      datum = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-47>"),
				     (object) value_reg, (object) k);
	      handler_reg = handler;
	      datum_reg = PJScheme.cdr ((object) datum);
	      pc = (Function) expand_quasiquote_list;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-49>"))))
	   {
	      k_reg = init_cont;
	      handler_reg = init_handler;
	      tokens_reg = value_reg;
	      pc = (Function) parse_sexps;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-50>"))))
	   {
	      object exp = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 2);
	      exp = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value_reg = PJScheme.cons ((object) exp, (object) value_reg);
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-51>"))))
	   {
	      object tokens_left = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 3);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 2);
	      tokens_left = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-50>"),
				     (object) value_reg, (object) k);
	      handler_reg = handler;
	      tokens_reg = tokens_left;
	      pc = (Function) parse_sexps;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-52>"))))
	   {
	      k_reg = REP_k;
	      handler_reg = REP_handler;
	      env_reg = toplevel_env;
	      exp_reg = value_reg;
	      pc = (Function) m;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-53>"))))
	   {
	      k_reg = PJScheme.make_cont2 ((object) symbol ("<cont2-20>"));
	      handler_reg = REP_handler;
	      input_reg = "(exit)";
	      pc = (Function) read_datum;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-54>"))))
	   {
	      k_reg = PJScheme.make_cont ((object) symbol ("<cont-53>"));
	      handler_reg = REP_handler;
	      env_reg = toplevel_env;
	      exp_reg = value_reg;
	      pc = (Function) m;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-55>"))))
	   {
	      k_reg = PJScheme.make_cont2 ((object) symbol ("<cont2-21>"));
	      handler_reg = REP_handler;
	      input_reg = "(test-all)";
	      pc = (Function) read_datum;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-56>"))))
	   {
	      k_reg = PJScheme.make_cont ((object) symbol ("<cont-55>"));
	      handler_reg = REP_handler;
	      env_reg = toplevel_env;
	      exp_reg = value_reg;
	      pc = (Function) m;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-57>"))))
	   {
	      if (true_q
		  (PJScheme.
		   not ((object) PJScheme.
			Eq ((object) value_reg, (object) symbol ("<void>")))))
		 PJScheme.pretty_print_prim ((object) value_reg);
	      if (true_q (config.NEED_NEWLINE))
		 PJScheme.newline ();
	      pc = (Function) read_eval_print;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-58>"))))
	   {
	      object args = null;
	      object env = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 4);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 3);
	      env = PJScheme.list_ref ((object) temp_1, (object) 2);
	      args = PJScheme.list_ref ((object) temp_1, (object) 1);
	      if (true_q (PJScheme.dlr_exp_q ((object) value_reg)))
		{
		   value_reg =
		      PJScheme.dlr_apply ((object) value_reg, (object) args);
		   k_reg = k;
		   pc = (Function) apply_cont;

		}
	      else
		{
		   k2_reg = k;
		   handler_reg = handler;
		   env2_reg = env;
		   args_reg = args;
		   proc_reg = value_reg;
		   pc = (Function) apply_proc;

		}
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-59>"))))
	   {
	      object rator = null;
	      object env = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 4);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 3);
	      env = PJScheme.list_ref ((object) temp_1, (object) 2);
	      rator = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-58>"),
				     (object) value_reg, (object) env,
				     (object) handler, (object) k);
	      handler_reg = handler;
	      env_reg = env;
	      exp_reg = rator;
	      pc = (Function) m;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-60>"))))
	   {
	      object handler = null;
	      handler = PJScheme.list_ref ((object) temp_1, (object) 1);
	      exception_reg = value_reg;
	      handler_reg = handler;
	      pc = (Function) apply_handler;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-61>"))))
	   {
	      object v = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 2);
	      v = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value_reg = v;
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-62>"))))
	   {
	      object fexps = null;
	      object env = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 4);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 3);
	      env = PJScheme.list_ref ((object) temp_1, (object) 2);
	      fexps = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-61>"),
				     (object) value_reg, (object) k);
	      handler_reg = handler;
	      env_reg = env;
	      exps_reg = fexps;
	      pc = (Function) eval_sequence;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-63>"))))
	   {
	      object clauses = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 2);
	      clauses = PJScheme.list_ref ((object) temp_1, (object) 1);
	      PJScheme.set_binding_value_b ((object) value_reg,
					    (object) PJScheme.
					    make_pattern_macro ((object)
								clauses));
	      value_reg = symbol ("<void>");
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-64>"))))
	   {
	      object docstring = null;
	      object rhs_value = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 3);
	      rhs_value = PJScheme.list_ref ((object) temp_1, (object) 2);
	      docstring = PJScheme.list_ref ((object) temp_1, (object) 1);
	      PJScheme.set_binding_docstring_b ((object) value_reg,
						(object) docstring);
	      PJScheme.set_binding_value_b ((object) value_reg,
					    (object) rhs_value);
	      value_reg = symbol ("<void>");
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-65>"))))
	   {
	      object rhs_value = null;
	      object var = null;
	      object env = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 5);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 4);
	      env = PJScheme.list_ref ((object) temp_1, (object) 3);
	      var = PJScheme.list_ref ((object) temp_1, (object) 2);
	      rhs_value = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-64>"),
				     (object) value_reg, (object) rhs_value,
				     (object) k);
	      handler_reg = handler;
	      env_reg = env;
	      var_reg = var;
	      pc = (Function) lookup_binding_in_first_frame;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-66>"))))
	   {
	      object rhs_exp = null;
	      object var = null;
	      object env = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 5);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 4);
	      env = PJScheme.list_ref ((object) temp_1, (object) 3);
	      var = PJScheme.list_ref ((object) temp_1, (object) 2);
	      rhs_exp = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-65>"),
				     (object) value_reg, (object) var,
				     (object) env, (object) handler,
				     (object) k);
	      handler_reg = handler;
	      env_reg = env;
	      exp_reg = PJScheme.car ((object) rhs_exp);
	      pc = (Function) m;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-67>"))))
	   {
	      object rhs_value = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 2);
	      rhs_value = PJScheme.list_ref ((object) temp_1, (object) 1);
	      PJScheme.set_binding_value_b ((object) value_reg,
					    (object) rhs_value);
	      value_reg = symbol ("<void>");
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-68>"))))
	   {
	      object var = null;
	      object env = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 4);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 3);
	      env = PJScheme.list_ref ((object) temp_1, (object) 2);
	      var = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-67>"),
				     (object) value_reg, (object) k);
	      handler_reg = handler;
	      env_reg = env;
	      var_reg = var;
	      pc = (Function) lookup_binding_in_first_frame;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-69>"))))
	   {
	      object var = null;
	      object env = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 4);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 3);
	      env = PJScheme.list_ref ((object) temp_1, (object) 2);
	      var = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-67>"),
				     (object) value_reg, (object) k);
	      handler_reg = handler;
	      env_reg = env;
	      variable_reg = var;
	      pc = (Function) lookup_binding;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-70>"))))
	   {
	      object else_exp = null;
	      object then_exp = null;
	      object env = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 5);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 4);
	      env = PJScheme.list_ref ((object) temp_1, (object) 3);
	      then_exp = PJScheme.list_ref ((object) temp_1, (object) 2);
	      else_exp = PJScheme.list_ref ((object) temp_1, (object) 1);
	      if (true_q (value_reg))
		{
		   k_reg = k;
		   handler_reg = handler;
		   env_reg = env;
		   exp_reg = then_exp;
		   pc = (Function) m;

		}
	      else
		{
		   k_reg = k;
		   handler_reg = handler;
		   env_reg = env;
		   exp_reg = else_exp;
		   pc = (Function) m;

		}
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-71>"))))
	   {
	      object e = null;
	      object handler = null;
	      handler = PJScheme.list_ref ((object) temp_1, (object) 2);
	      e = PJScheme.list_ref ((object) temp_1, (object) 1);
	      exception_reg = e;
	      handler_reg = handler;
	      pc = (Function) apply_handler;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-72>"))))
	   {
	      object exps = null;
	      object env = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 4);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 3);
	      env = PJScheme.list_ref ((object) temp_1, (object) 2);
	      exps = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-47>"),
				     (object) value_reg, (object) k);
	      handler_reg = handler;
	      env_reg = env;
	      exps_reg = PJScheme.cdr ((object) exps);
	      pc = (Function) m_star;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-73>"))))
	   {
	      object exps = null;
	      object env = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 4);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 3);
	      env = PJScheme.list_ref ((object) temp_1, (object) 2);
	      exps = PJScheme.list_ref ((object) temp_1, (object) 1);
	      if (true_q
		  (PJScheme.null_q ((object) PJScheme.cdr ((object) exps))))
		{
		   k_reg = k;
		   pc = (Function) apply_cont;

		}
	      else
		{
		   k_reg = k;
		   handler_reg = handler;
		   env_reg = env;
		   exps_reg = PJScheme.cdr ((object) exps);
		   pc = (Function) eval_sequence;

		}
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-74>"))))
	   {
	      object handler = null;
	      object k2 = null;
	      k2 = PJScheme.list_ref ((object) temp_1, (object) 2);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg = k2;
	      handler_reg = handler;
	      env_reg = toplevel_env;
	      exp_reg = value_reg;
	      pc = (Function) m;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-75>"))))
	   {
	      object list1 = null;
	      object proc = null;
	      object env = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 5);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 4);
	      env = PJScheme.list_ref ((object) temp_1, (object) 3);
	      proc = PJScheme.list_ref ((object) temp_1, (object) 2);
	      list1 = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-47>"),
				     (object) value_reg, (object) k);
	      handler_reg = handler;
	      env_reg = env;
	      list1_reg = PJScheme.cdr ((object) list1);
	      proc_reg = proc;
	      pc = (Function) map1;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-76>"))))
	   {
	      object list1 = null;
	      object proc = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 3);
	      proc = PJScheme.list_ref ((object) temp_1, (object) 2);
	      list1 = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value_reg =
		 PJScheme.cons ((object) PJScheme.
				dlr_apply ((object) proc,
					   (object) PJScheme.
					   list ((object) PJScheme.
						 car ((object) list1))),
				(object) value_reg);
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-77>"))))
	   {
	      object list1 = null;
	      object list2 = null;
	      object proc = null;
	      object env = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 6);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 5);
	      env = PJScheme.list_ref ((object) temp_1, (object) 4);
	      proc = PJScheme.list_ref ((object) temp_1, (object) 3);
	      list2 = PJScheme.list_ref ((object) temp_1, (object) 2);
	      list1 = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-47>"),
				     (object) value_reg, (object) k);
	      handler_reg = handler;
	      env_reg = env;
	      list2_reg = PJScheme.cdr ((object) list2);
	      list1_reg = PJScheme.cdr ((object) list1);
	      proc_reg = proc;
	      pc = (Function) map2;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-78>"))))
	   {
	      object list1 = null;
	      object list2 = null;
	      object proc = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 4);
	      proc = PJScheme.list_ref ((object) temp_1, (object) 3);
	      list2 = PJScheme.list_ref ((object) temp_1, (object) 2);
	      list1 = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value_reg =
		 PJScheme.cons ((object) PJScheme.
				dlr_apply ((object) proc,
					   (object) PJScheme.
					   list ((object) PJScheme.
						 car ((object) list1),
						 (object) PJScheme.
						 car ((object) list2))),
				(object) value_reg);
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-79>"))))
	   {
	      object lists = null;
	      object proc = null;
	      object env = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 5);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 4);
	      env = PJScheme.list_ref ((object) temp_1, (object) 3);
	      proc = PJScheme.list_ref ((object) temp_1, (object) 2);
	      lists = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-47>"),
				     (object) value_reg, (object) k);
	      handler_reg = handler;
	      env_reg = env;
	      lists_reg = map (cdr_proc, (object) lists);
	      proc_reg = proc;
	      pc = (Function) mapN;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-80>"))))
	   {
	      object lists = null;
	      object proc = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 3);
	      proc = PJScheme.list_ref ((object) temp_1, (object) 2);
	      lists = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value_reg =
		 PJScheme.cons ((object) PJScheme.
				dlr_apply ((object) proc,
					   (object) map (car_proc,
							 (object) lists)),
				(object) value_reg);
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-81>"))))
	   {
	      object arg_list = null;
	      object proc = null;
	      object env = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 5);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 4);
	      env = PJScheme.list_ref ((object) temp_1, (object) 3);
	      proc = PJScheme.list_ref ((object) temp_1, (object) 2);
	      arg_list = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg = k;
	      handler_reg = handler;
	      env_reg = env;
	      lists_reg = map (cdr_proc, (object) arg_list);
	      proc_reg = proc;
	      pc = (Function) for_each_prim;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-82>"))))
	   {
	      object args = null;
	      object sym = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 4);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 3);
	      sym = PJScheme.list_ref ((object) temp_1, (object) 2);
	      args = PJScheme.list_ref ((object) temp_1, (object) 1);
	      if (true_q
		  (PJScheme.null_q ((object) PJScheme.cdr ((object) args))))
		{
		   k_reg = k;
		   pc = (Function) apply_cont;

		}
	      else
		 if (true_q
		     (PJScheme.
		      not ((object) PJScheme.
			   environment_q ((object) value_reg))))
		{
		   exception_reg =
		      PJScheme.format ((object) "~a is not a module",
				       (object) sym);
		   handler_reg = handler;
		   pc = (Function) apply_handler;

		}
	      else
		{
		   k_reg = k;
		   handler_reg = handler;
		   env_reg = value_reg;
		   args_reg = PJScheme.cdr ((object) args);
		   pc = (Function) get_primitive;

		}
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-83>"))))
	   {
	      object filename = null;
	      object env = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 4);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 3);
	      env = PJScheme.list_ref ((object) temp_1, (object) 2);
	      filename = PJScheme.list_ref ((object) temp_1, (object) 1);
	      {
		 object module = null;
		 module =
		    PJScheme.extend ((object) env, (object) EmptyList,
				     (object) EmptyList);
		 PJScheme.set_binding_value_b ((object) value_reg,
					       (object) module);
		 k_reg = k;
		 handler_reg = handler;
		 env_reg = module;
		 filename_reg = filename;
		 pc = (Function) load_file;
	      }
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-84>"))))
	   {
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 1);
	      load_stack = PJScheme.cdr ((object) load_stack);
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-85>"))))
	   {
	      object env = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 3);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 2);
	      env = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-84>"),
				     (object) k);
	      handler_reg = handler;
	      env_reg = env;
	      tokens_reg = value_reg;
	      pc = (Function) load_loop;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-86>"))))
	   {
	      object tokens_left = null;
	      object env = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 4);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 3);
	      env = PJScheme.list_ref ((object) temp_1, (object) 2);
	      tokens_left = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg = k;
	      handler_reg = handler;
	      env_reg = env;
	      tokens_reg = tokens_left;
	      pc = (Function) load_loop;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-87>"))))
	   {
	      object tokens_left = null;
	      object env = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 4);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 3);
	      env = PJScheme.list_ref ((object) temp_1, (object) 2);
	      tokens_left = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-86>"),
				     (object) tokens_left, (object) env,
				     (object) handler, (object) k);
	      handler_reg = handler;
	      env_reg = env;
	      exp_reg = value_reg;
	      pc = (Function) m;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-88>"))))
	   {
	      object filenames = null;
	      object env = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 4);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 3);
	      env = PJScheme.list_ref ((object) temp_1, (object) 2);
	      filenames = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg = k;
	      handler_reg = handler;
	      env_reg = env;
	      filenames_reg = PJScheme.cdr ((object) filenames);
	      pc = (Function) load_files;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-89>"))))
	   {
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value_reg = PJScheme.binding_docstring ((object) value_reg);
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-90>"))))
	   {
	      k_reg = init_cont;
	      handler_reg = init_handler;
	      env_reg = toplevel_env;
	      exp_reg = value_reg;
	      pc = (Function) m;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-91>"))))
	   {
	      object pattern = null;
	      object var = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 3);
	      var = PJScheme.list_ref ((object) temp_1, (object) 2);
	      pattern = PJScheme.list_ref ((object) temp_1, (object) 1);
	      if (true_q (value_reg))
		{
		   value_reg = true;
		   k_reg = k;
		   pc = (Function) apply_cont;

		}
	      else
		{
		   k_reg = k;
		   pattern_reg = PJScheme.cdr ((object) pattern);
		   var_reg = var;
		   pc = (Function) occurs_q;

		}
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-92>"))))
	   {
	      object p1 = null;
	      object p2 = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 3);
	      p2 = PJScheme.list_ref ((object) temp_1, (object) 2);
	      p1 = PJScheme.list_ref ((object) temp_1, (object) 1);
	      if (true_q (value_reg))
		{
		   value_reg = false;
		   k_reg = k;
		   pc = (Function) apply_cont;

		}
	      else
		{
		   value_reg =
		      PJScheme.make_sub ((object) symbol ("unit"),
					 (object) p1, (object) p2);
		   k_reg = k;
		   pc = (Function) apply_cont;

		}
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-93>"))))
	   {
	      object s_car = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 2);
	      s_car = PJScheme.list_ref ((object) temp_1, (object) 1);
	      if (true_q (PJScheme.not ((object) value_reg)))
		{
		   value_reg = false;
		   k_reg = k;
		   pc = (Function) apply_cont;

		}
	      else
		{
		   value_reg =
		      PJScheme.make_sub ((object) symbol ("composite"),
					 (object) s_car, (object) value_reg);
		   k_reg = k;
		   pc = (Function) apply_cont;

		}
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-94>"))))
	   {
	      object new_cdr1 = null;
	      object s_car = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 3);
	      s_car = PJScheme.list_ref ((object) temp_1, (object) 2);
	      new_cdr1 = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-93>"),
				     (object) s_car, (object) k);
	      p2_reg = value_reg;
	      p1_reg = new_cdr1;
	      pc = (Function) unify_patterns;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-95>"))))
	   {
	      object pair2 = null;
	      object s_car = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 3);
	      s_car = PJScheme.list_ref ((object) temp_1, (object) 2);
	      pair2 = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-94>"),
				     (object) value_reg, (object) s_car,
				     (object) k);
	      s_reg = s_car;
	      pattern_reg = PJScheme.cdr ((object) pair2);
	      pc = (Function) instantiate;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-96>"))))
	   {
	      object pair1 = null;
	      object pair2 = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 3);
	      pair2 = PJScheme.list_ref ((object) temp_1, (object) 2);
	      pair1 = PJScheme.list_ref ((object) temp_1, (object) 1);
	      if (true_q (PJScheme.not ((object) value_reg)))
		{
		   value_reg = false;
		   k_reg = k;
		   pc = (Function) apply_cont;

		}
	      else
		{
		   k_reg =
		      PJScheme.make_cont ((object) symbol ("<cont-95>"),
					  (object) pair2, (object) value_reg,
					  (object) k);
		   s_reg = value_reg;
		   pattern_reg = PJScheme.cdr ((object) pair1);
		   pc = (Function) instantiate;

		}
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-97>"))))
	   {
	      object pattern = null;
	      object s = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 3);
	      s = PJScheme.list_ref ((object) temp_1, (object) 2);
	      pattern = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-40>"),
				     (object) value_reg, (object) k);
	      s_reg = s;
	      pattern_reg = PJScheme.cdr ((object) pattern);
	      pc = (Function) instantiate;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont-98>"))))
	   {
	      object s2 = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 2);
	      s2 = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg = k;
	      s_reg = s2;
	      pattern_reg = value_reg;
	      pc = (Function) instantiate;
	   }
	 else
	    throw new
	       Exception (format
			  (symbol ("apply-cont") + ": " +
			   "bad continuation: ~a", k_reg));
      }

   }

   new public static object make_cont2 (params object[]args)
   {
      return ((object) PJScheme.
	      cons ((object) symbol ("continuation2"), (object) args));
   }

   new public static void apply_cont2 ()
   {
      {
	 object temp_1 = null;
	 temp_1 = PJScheme.cdr ((object) k_reg);
	 if (true_q
	     (PJScheme.
	      Eq ((object) PJScheme.car ((object) temp_1),
		  (object) symbol ("<cont2-1>"))))
	   {
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 2);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 1);
	      if (true_q
		  (PJScheme.
		   token_type_q ((object) value1_reg,
				 (object) symbol ("end-marker"))))
		{
		   value_reg = PJScheme.list ((object) value1_reg);
		   k_reg = k;
		   pc = (Function) apply_cont;

		}
	      else
		{
		   k_reg =
		      PJScheme.make_cont ((object) symbol ("<cont-1>"),
					  (object) value1_reg, (object) k);
		   handler_reg = handler;
		   chars_reg = value2_reg;
		   pc = (Function) scan_input_loop;

		}
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont2-2>"))))
	   {
	      final_reg = value1_reg;
	      pc = null;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont2-3>"))))
	   {
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 2);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 1);
	      if (true_q
		  (PJScheme.
		   token_type_q ((object) PJScheme.
				 first ((object) value2_reg),
				 (object) symbol ("end-marker"))))
		{
		   k_reg = k;
		   pc = (Function) apply_cont2;

		}
	      else
		{
		   exception_reg =
		      PJScheme.
		      format ((object) "tokens left over at line: ~a col: ~a",
			      (object) PJScheme.
			      get_line_count ((object) PJScheme.
					      first ((object) value2_reg)),
			      (object) PJScheme.
			      get_char_count ((object) PJScheme.
					      first ((object) value2_reg)));
		   handler_reg = handler;
		   pc = (Function) apply_handler;

		}
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont2-4>"))))
	   {
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value1_reg = PJScheme.list_to_vector ((object) value1_reg);
	      k_reg = k;
	      pc = (Function) apply_cont2;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont2-5>"))))
	   {
	      object keyword = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 2);
	      keyword = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value1_reg =
		 PJScheme.list ((object) keyword, (object) value1_reg);
	      k_reg = k;
	      pc = (Function) apply_cont2;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont2-6>"))))
	   {
	      object sexp1 = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 2);
	      sexp1 = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value1_reg =
		 PJScheme.cons ((object) sexp1, (object) value1_reg);
	      k_reg = k;
	      pc = (Function) apply_cont2;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont2-7>"))))
	   {
	      object expected_terminator = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 3);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 2);
	      expected_terminator =
		 PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont2 ((object) symbol ("<cont2-6>"),
				      (object) value1_reg, (object) k);
	      handler_reg = handler;
	      expected_terminator_reg = expected_terminator;
	      tokens_reg = value2_reg;
	      pc = (Function) read_sexp_sequence;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont2-8>"))))
	   {
	      object expected_terminator = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 3);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 2);
	      expected_terminator =
		 PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg = k;
	      handler_reg = handler;
	      expected_terminator_reg = expected_terminator;
	      tokens_reg = value2_reg;
	      sexp_reg = value1_reg;
	      pc = (Function) close_sexp_sequence;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont2-9>"))))
	   {
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 2);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont2 ((object) symbol ("<cont2-6>"),
				      (object) value1_reg, (object) k);
	      handler_reg = handler;
	      tokens_reg = value2_reg;
	      pc = (Function) read_vector;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont2-10>"))))
	   {
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 2);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 1);
	      PJScheme.pretty_print ((object) value1_reg);
	      k_reg = k;
	      handler_reg = handler;
	      tokens_reg = value2_reg;
	      pc = (Function) print_unparsed_sexps;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont2-11>"))))
	   {
	      final_reg =
		 PJScheme.cons ((object) value1_reg, (object) value2_reg);
	      pc = null;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont2-12>"))))
	   {
	      object bodies = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 2);
	      bodies = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value_reg =
		 PJScheme.cons ((object) symbol ("let"),
				(object) PJScheme.cons ((object) value1_reg,
							(object) PJScheme.
							append ((object)
								value2_reg,
								(object)
								bodies)));
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont2-13>"))))
	   {
	      object procs = null;
	      object vars = null;
	      object k2 = null;
	      k2 = PJScheme.list_ref ((object) temp_1, (object) 3);
	      vars = PJScheme.list_ref ((object) temp_1, (object) 2);
	      procs = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value2_reg =
		 PJScheme.cons ((object) PJScheme.
				list ((object) symbol ("set!"),
				      (object) PJScheme.car ((object) vars),
				      (object) PJScheme.car ((object) procs)),
				(object) value2_reg);
	      value1_reg =
		 PJScheme.cons ((object) PJScheme.
				list ((object) PJScheme.car ((object) vars),
				      (object) PJScheme.
				      list ((object) symbol ("quote"),
					    (object) symbol ("undefined"))),
				(object) value1_reg);
	      k_reg = k2;
	      pc = (Function) apply_cont2;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont2-14>"))))
	   {
	      object exp = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 2);
	      exp = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value_reg =
		 PJScheme.list ((object) symbol ("let"),
				(object) PJScheme.cons ((object) PJScheme.
							list ((object)
							      symbol ("r"),
							      (object) exp),
							(object) value1_reg),
				(object) PJScheme.
				cons ((object) symbol ("cond"),
				      (object) value2_reg));
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont2-15>"))))
	   {
	      object clauses = null;
	      object var = null;
	      object k2 = null;
	      k2 = PJScheme.list_ref ((object) temp_1, (object) 3);
	      var = PJScheme.list_ref ((object) temp_1, (object) 2);
	      clauses = PJScheme.list_ref ((object) temp_1, (object) 1);
	      {
		 object clause = null;
		 clause = PJScheme.car ((object) clauses);
		 if (true_q
		     (PJScheme.
		      Eq ((object) PJScheme.car ((object) clause),
			  (object) symbol ("else"))))
		   {
		      value2_reg =
			 PJScheme.cons ((object) PJScheme.
					list ((object) symbol ("else"),
					      (object) PJScheme.
					      list ((object)
						    symbol ("else-code"))),
					(object) value2_reg);
		      value1_reg =
			 PJScheme.cons ((object) PJScheme.
					list ((object) symbol ("else-code"),
					      (object) PJScheme.
					      cons ((object)
						    symbol ("lambda"),
						    (object) PJScheme.
						    cons ((object) EmptyList,
							  (object) PJScheme.
							  cdr ((object)
							       clause)))),
					(object) value1_reg);
		      k_reg = k2;
		      pc = (Function) apply_cont2;

		   }
		 else
		    if (true_q
			(PJScheme.
			 symbol_q ((object) PJScheme.car ((object) clause))))
		   {
		      object name = null;
		      name = PJScheme.car ((object) clause);
		      value2_reg =
			 PJScheme.cons ((object) PJScheme.
					list ((object) PJScheme.
					      list ((object) symbol ("eq?"),
						    (object) var,
						    (object) PJScheme.
						    list ((object)
							  symbol ("quote"),
							  (object) PJScheme.
							  car ((object)
							       clause))),
					      (object) PJScheme.
					      list ((object) name)),
					(object) value2_reg);
		      value1_reg =
			 PJScheme.cons ((object) PJScheme.
					list ((object) name,
					      (object) PJScheme.
					      cons ((object)
						    symbol ("lambda"),
						    (object) PJScheme.
						    cons ((object) EmptyList,
							  (object) PJScheme.
							  cdr ((object)
							       clause)))),
					(object) value1_reg);
		      k_reg = k2;
		      pc = (Function) apply_cont2;
		   }
		 else
		   {
		      object name = null;
		      name = PJScheme.caar ((object) clause);
		      value2_reg =
			 PJScheme.cons ((object) PJScheme.
					list ((object) PJScheme.
					      list ((object) symbol ("memq"),
						    (object) var,
						    (object) PJScheme.
						    list ((object)
							  symbol ("quote"),
							  (object) PJScheme.
							  car ((object)
							       clause))),
					      (object) PJScheme.
					      list ((object) name)),
					(object) value2_reg);
		      value1_reg =
			 PJScheme.cons ((object) PJScheme.
					list ((object) name,
					      (object) PJScheme.
					      cons ((object)
						    symbol ("lambda"),
						    (object) PJScheme.
						    cons ((object) EmptyList,
							  (object) PJScheme.
							  cdr ((object)
							       clause)))),
					(object) value1_reg);
		      k_reg = k2;
		      pc = (Function) apply_cont2;
		   }
	      }
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont2-16>"))))
	   {
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value_reg =
		 PJScheme.list ((object) symbol ("let"), (object) value1_reg,
				(object) PJScheme.
				cons ((object) symbol ("cond"),
				      (object) value2_reg));
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont2-17>"))))
	   {
	      object clauses = null;
	      object var = null;
	      object k2 = null;
	      k2 = PJScheme.list_ref ((object) temp_1, (object) 3);
	      var = PJScheme.list_ref ((object) temp_1, (object) 2);
	      clauses = PJScheme.list_ref ((object) temp_1, (object) 1);
	      {
		 object clause = null;
		 clause = PJScheme.car ((object) clauses);
		 if (true_q
		     (PJScheme.
		      Eq ((object) PJScheme.car ((object) clause),
			  (object) symbol ("else"))))
		   {
		      value2_reg =
			 PJScheme.cons ((object) PJScheme.
					list ((object) symbol ("else"),
					      (object) PJScheme.
					      list ((object)
						    symbol ("else-code"))),
					(object) value2_reg);
		      value1_reg =
			 PJScheme.cons ((object) PJScheme.
					list ((object) symbol ("else-code"),
					      (object) PJScheme.
					      cons ((object)
						    symbol ("lambda"),
						    (object) PJScheme.
						    cons ((object) EmptyList,
							  (object) PJScheme.
							  cdr ((object)
							       clause)))),
					(object) value1_reg);
		      k_reg = k2;
		      pc = (Function) apply_cont2;

		   }
		 else
		    if (true_q
			(PJScheme.
			 symbol_q ((object) PJScheme.car ((object) clause))))
		   {
		      object name = null;
		      name = PJScheme.car ((object) clause);
		      value2_reg =
			 PJScheme.cons ((object) PJScheme.
					list ((object) PJScheme.
					      list ((object) symbol ("eq?"),
						    (object) PJScheme.
						    list ((object)
							  symbol ("car"),
							  (object) var),
						    (object) PJScheme.
						    list ((object)
							  symbol ("quote"),
							  (object) PJScheme.
							  car ((object)
							       clause))),
					      (object) PJScheme.
					      list ((object) symbol ("apply"),
						    (object) name,
						    (object) PJScheme.
						    list ((object)
							  symbol ("cdr"),
							  (object) var))),
					(object) value2_reg);
		      value1_reg =
			 PJScheme.cons ((object) PJScheme.
					list ((object) name,
					      (object) PJScheme.
					      cons ((object)
						    symbol ("lambda"),
						    (object) PJScheme.
						    cons ((object) PJScheme.
							  cadr ((object)
								clause),
							  (object) PJScheme.
							  cddr ((object)
								clause)))),
					(object) value1_reg);
		      k_reg = k2;
		      pc = (Function) apply_cont2;
		   }
		 else
		   {
		      object name = null;
		      name = PJScheme.caar ((object) clause);
		      value2_reg =
			 PJScheme.cons ((object) PJScheme.
					list ((object) PJScheme.
					      list ((object) symbol ("memq"),
						    (object) PJScheme.
						    list ((object)
							  symbol ("car"),
							  (object) var),
						    (object) PJScheme.
						    list ((object)
							  symbol ("quote"),
							  (object) PJScheme.
							  car ((object)
							       clause))),
					      (object) PJScheme.
					      list ((object) symbol ("apply"),
						    (object) name,
						    (object) PJScheme.
						    list ((object)
							  symbol ("cdr"),
							  (object) var))),
					(object) value2_reg);
		      value1_reg =
			 PJScheme.cons ((object) PJScheme.
					list ((object) name,
					      (object) PJScheme.
					      cons ((object)
						    symbol ("lambda"),
						    (object) PJScheme.
						    cons ((object) PJScheme.
							  cadr ((object)
								clause),
							  (object) PJScheme.
							  cddr ((object)
								clause)))),
					(object) value1_reg);
		      k_reg = k2;
		      pc = (Function) apply_cont2;
		   }
	      }
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont2-18>"))))
	   {
	      k_reg = init_cont;
	      handler_reg = init_handler;
	      datum_reg = value1_reg;
	      pc = (Function) parse;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont2-19>"))))
	   {
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 2);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-51>"),
				     (object) value2_reg, (object) handler,
				     (object) k);
	      handler_reg = handler;
	      datum_reg = value1_reg;
	      pc = (Function) parse;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont2-20>"))))
	   {
	      k_reg = PJScheme.make_cont ((object) symbol ("<cont-52>"));
	      handler_reg = REP_handler;
	      datum_reg = value1_reg;
	      pc = (Function) parse;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont2-21>"))))
	   {
	      k_reg = PJScheme.make_cont ((object) symbol ("<cont-54>"));
	      handler_reg = REP_handler;
	      datum_reg = value1_reg;
	      pc = (Function) parse;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont2-22>"))))
	   {
	      k_reg = PJScheme.make_cont ((object) symbol ("<cont-56>"));
	      handler_reg = REP_handler;
	      datum_reg = value1_reg;
	      pc = (Function) parse;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont2-23>"))))
	   {
	      object handler = null;
	      object k2 = null;
	      k2 = PJScheme.list_ref ((object) temp_1, (object) 2);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg = k2;
	      handler_reg = handler;
	      datum_reg = value1_reg;
	      pc = (Function) parse;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont2-24>"))))
	   {
	      object env = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 3);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 2);
	      env = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-87>"),
				     (object) value2_reg, (object) env,
				     (object) handler, (object) k);
	      handler_reg = handler;
	      datum_reg = value1_reg;
	      pc = (Function) parse;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<cont2-25>"))))
	   {
	      k_reg = PJScheme.make_cont ((object) symbol ("<cont-90>"));
	      handler_reg = init_handler;
	      datum_reg = value1_reg;
	      pc = (Function) parse;

	   }
	 else
	    throw new
	       Exception (format
			  (symbol ("apply-cont2") + ": " +
			   "bad continuation2: ~a", k_reg));
      }

   }

   new public static object make_handler (params object[]args)
   {
      return ((object) PJScheme.
	      cons ((object) symbol ("handler"), (object) args));
   }

   new public static void apply_handler ()
   {
      {
	 object temp_1 = null;
	 temp_1 = PJScheme.cdr ((object) handler_reg);
	 if (true_q
	     (PJScheme.
	      Eq ((object) PJScheme.car ((object) temp_1),
		  (object) symbol ("<handler-1>"))))
	   {
	      final_reg =
		 PJScheme.list ((object) symbol ("exception"),
				(object) exception_reg);
	      pc = null;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<handler-2>"))))
	   {
	      value_reg =
		 PJScheme.list ((object) symbol ("uncaught"),
				(object) symbol ("exception:"),
				(object) exception_reg);
	      k_reg = REP_k;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<handler-3>"))))
	   {
	      object cexps = null;
	      object cvar = null;
	      object env = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 5);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 4);
	      env = PJScheme.list_ref ((object) temp_1, (object) 3);
	      cvar = PJScheme.list_ref ((object) temp_1, (object) 2);
	      cexps = PJScheme.list_ref ((object) temp_1, (object) 1);
	      {
		 object new_env = null;
		 new_env =
		    PJScheme.extend ((object) env,
				     (object) PJScheme.list ((object) cvar),
				     (object) PJScheme.
				     list ((object) exception_reg));
		 k_reg = k;
		 handler_reg = handler;
		 env_reg = new_env;
		 exps_reg = cexps;
		 pc = (Function) eval_sequence;
	      }
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<handler-4>"))))
	   {
	      object fexps = null;
	      object env = null;
	      object handler = null;
	      handler = PJScheme.list_ref ((object) temp_1, (object) 3);
	      env = PJScheme.list_ref ((object) temp_1, (object) 2);
	      fexps = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-71>"),
				     (object) exception_reg,
				     (object) handler);
	      handler_reg = handler;
	      env_reg = env;
	      exps_reg = fexps;
	      pc = (Function) eval_sequence;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<handler-5>"))))
	   {
	      object cexps = null;
	      object cvar = null;
	      object fexps = null;
	      object env = null;
	      object handler = null;
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 6);
	      handler = PJScheme.list_ref ((object) temp_1, (object) 5);
	      env = PJScheme.list_ref ((object) temp_1, (object) 4);
	      fexps = PJScheme.list_ref ((object) temp_1, (object) 3);
	      cvar = PJScheme.list_ref ((object) temp_1, (object) 2);
	      cexps = PJScheme.list_ref ((object) temp_1, (object) 1);
	      {
		 object new_env = null;
		 new_env =
		    PJScheme.extend ((object) env,
				     (object) PJScheme.list ((object) cvar),
				     (object) PJScheme.
				     list ((object) exception_reg));
		 {
		    object catch_handler = null;
		    catch_handler =
		       PJScheme.try_finally_handler ((object) fexps,
						     (object) env,
						     (object) handler);
		    k_reg =
		       PJScheme.make_cont ((object) symbol ("<cont-62>"),
					   (object) fexps, (object) env,
					   (object) handler, (object) k);
		    handler_reg = catch_handler;
		    env_reg = new_env;
		    exps_reg = cexps;
		    pc = (Function) eval_sequence;
		 }
	      }
	   }
	 else
	    throw new
	       Exception (format
			  (symbol ("apply-handler") + ": " +
			   "bad handler: ~a", handler_reg));
      }

   }

   new public static void apply_proc ()
   {
      {
	 object temp_1 = null;
	 temp_1 = PJScheme.cdr ((object) proc_reg);
	 if (true_q
	     (PJScheme.
	      Eq ((object) PJScheme.car ((object) temp_1),
		  (object) symbol ("<proc-1>"))))
	   {
	      object formals = null;
	      object body = null;
	      object env = null;
	      env = PJScheme.list_ref ((object) temp_1, (object) 3);
	      body = PJScheme.list_ref ((object) temp_1, (object) 2);
	      formals = PJScheme.list_ref ((object) temp_1, (object) 1);
	      if (true_q
		  (PJScheme.
		   EqualSign ((object) PJScheme.length ((object) args_reg),
			      (object) PJScheme.length ((object) formals))))
		{
		   k_reg = k2_reg;
		   env_reg =
		      PJScheme.extend ((object) env, (object) formals,
				       (object) args_reg);
		   exp_reg = body;
		   pc = (Function) m;

		}
	      else
		{
		   exception_reg = "incorrect number of arguments";
		   pc = (Function) apply_handler;

		}
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-2>"))))
	   {
	      object formals = null;
	      object runt = null;
	      object body = null;
	      object env = null;
	      env = PJScheme.list_ref ((object) temp_1, (object) 4);
	      body = PJScheme.list_ref ((object) temp_1, (object) 3);
	      runt = PJScheme.list_ref ((object) temp_1, (object) 2);
	      formals = PJScheme.list_ref ((object) temp_1, (object) 1);
	      if (true_q
		  (PJScheme.
		   GreaterOrEqual ((object) PJScheme.
				   length ((object) args_reg),
				   (object) PJScheme.
				   length ((object) formals))))
		{
		   object new_env = null;
		   new_env =
		      PJScheme.extend ((object) env,
				       (object) PJScheme.cons ((object) runt,
							       (object)
							       formals),
				       (object) PJScheme.
				       cons ((object) PJScheme.
					     list_tail ((object) args_reg,
							(object) PJScheme.
							length ((object)
								formals)),
					     (object) PJScheme.
					     list_head ((object) args_reg,
							(object) PJScheme.
							length ((object)
								formals))));
		   k_reg = k2_reg;
		   env_reg = new_env;
		   exp_reg = body;
		   pc = (Function) m;
		}
	      else
		{
		   exception_reg = "not enough arguments given";
		   pc = (Function) apply_handler;

		}
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-3>"))))
	   {
	      k_reg = k2_reg;
	      env_reg = env2_reg;
	      var_reg = PJScheme.car ((object) args_reg);
	      pc = (Function) help_prim;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-4>"))))
	   {
	      value_reg =
		 PJScheme.make_vector_size ((object) PJScheme.
					    car ((object) args_reg));
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-5>"))))
	   {
	      value_reg = apply (vector_ref_proc, (object) args_reg);
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-6>"))))
	   {
	      value_reg =
		 PJScheme.vector_set_b ((object) PJScheme.
					car ((object) args_reg),
					(object) PJScheme.
					cadr ((object) args_reg),
					(object) PJScheme.
					caddr ((object) args_reg));
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-7>"))))
	   {
	      value_reg = PJScheme.make_vector ((object) args_reg);
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-8>"))))
	   {
	      apply (printf_prim_proc, (object) args_reg);
	      value_reg = symbol ("<void>");
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-9>"))))
	   {
	      value_reg =
		 PJScheme.not ((object) PJScheme.car ((object) args_reg));
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-10>"))))
	   {
	      value_reg =
		 PJScheme.using_prim ((object) args_reg, (object) env2_reg);
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-11>"))))
	   {
	      value_reg = env2_reg;
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-12>"))))
	   {
	      k_reg = k2_reg;
	      env_reg = env2_reg;
	      lists_reg = PJScheme.cdr ((object) args_reg);
	      proc_reg = PJScheme.car ((object) args_reg);
	      pc = (Function) for_each_prim;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-13>"))))
	   {
	      k_reg = k2_reg;
	      env_reg = env2_reg;
	      proc_reg = PJScheme.car ((object) args_reg);
	      args_reg = PJScheme.cdr ((object) args_reg);
	      pc = (Function) map_prim;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-14>"))))
	   {
	      value_reg = PJScheme.get_current_time ();
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-15>"))))
	   {
	      value_reg = PJScheme.dir ((object) args_reg, (object) env2_reg);
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-16>"))))
	   {
	      value_reg = apply (make_vector_proc, (object) args_reg);
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-17>"))))
	   {
	      value_reg = apply (append_proc, (object) args_reg);
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-18>"))))
	   {
	      value_reg = apply (reverse_proc, (object) args_reg);
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-19>"))))
	   {
	      k_reg = k2_reg;
	      env_reg = env2_reg;
	      proc_reg = PJScheme.car ((object) args_reg);
	      pc = (Function) call_cc_primitive;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-20>"))))
	   {
	      k_reg = k2_reg;
	      env_reg = env2_reg;
	      pc = (Function) get_primitive;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-21>"))))
	   {
	      k_reg = k2_reg;
	      env_reg = env2_reg;
	      pc = (Function) import_primitive;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-22>"))))
	   {
	      value_reg = apply (set_cdr_b_proc, (object) args_reg);
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-23>"))))
	   {
	      value_reg = apply (set_car_b_proc, (object) args_reg);
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-24>"))))
	   {
	      value_reg = apply (range_proc, (object) args_reg);
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-25>"))))
	   {
	      value_reg = apply (memq_proc, (object) args_reg);
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-26>"))))
	   {
	      value_reg = apply (Eq_proc, (object) args_reg);
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-27>"))))
	   {
	      value_reg = apply (Equal_proc, (object) args_reg);
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-28>"))))
	   {
	      value_reg = apply (EqualSign_proc, (object) args_reg);
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-29>"))))
	   {
	      value_reg = apply (GreaterThan_proc, (object) args_reg);
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-30>"))))
	   {
	      value_reg = apply (LessThan_proc, (object) args_reg);
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-31>"))))
	    if (true_q
		(PJScheme.
		 EqualSign ((object) PJScheme.length ((object) args_reg),
			    (object) 1)))
	       if (true_q
		   (PJScheme.
		    EqualSign ((object) PJScheme.car ((object) args_reg),
			       (object) 0)))
		 {
		    exception_reg = "division by zero";
		    pc = (Function) apply_handler;

		 }
	       else
		 {
		    value_reg = apply (Divide_proc, (object) args_reg);
		    k_reg = k2_reg;
		    pc = (Function) apply_cont;

		 }
	    else
	       if (true_q
		   (PJScheme.
		    GreaterOrEqual ((object) PJScheme.
				    length ((object) args_reg), (object) 2)))
	       if (true_q
		   (PJScheme.
		    EqualSign ((object) PJScheme.cadr ((object) args_reg),
			       (object) 0)))
		 {
		    exception_reg = "division by zero";
		    pc = (Function) apply_handler;

		 }
	       else
		 {
		    value_reg = apply (Divide_proc, (object) args_reg);
		    k_reg = k2_reg;
		    pc = (Function) apply_cont;

		 }
	    else
	      {
		 exception_reg = "not enough args to /";
		 pc = (Function) apply_handler;

	      }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-32>"))))
	   {
	      value_reg = apply (Multiply_proc, (object) args_reg);
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-33>"))))
	   {
	      value_reg = apply (Subtract_proc, (object) args_reg);
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-34>"))))
	   {
	      value_reg = apply (Add_proc, (object) args_reg);
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-35>"))))
	   {
	      value_reg = args_reg;
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-36>"))))
	   {
	      value_reg = apply (cdr_proc, (object) args_reg);
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-37>"))))
	   {
	      value_reg = apply (car_proc, (object) args_reg);
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-38>"))))
	   {
	      value_reg = apply (cons_proc, (object) args_reg);
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-39>"))))
	   {
	      value_reg = apply (null_q_proc, (object) args_reg);
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-40>"))))
	   {
	      load_stack = EmptyList;
	      k_reg = k2_reg;
	      env_reg = toplevel_env;
	      filename_reg = PJScheme.car ((object) args_reg);
	      pc = (Function) load_file;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-41>"))))
	   {
	      PJScheme.newline_prim ();
	      value_reg = symbol ("<void>");
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-42>"))))
	   {
	      apply (display_prim_proc, (object) args_reg);
	      value_reg = symbol ("<void>");
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-43>"))))
	   {
	      for_each (pretty_print_prim_proc, (object) args_reg);
	      value_reg = symbol ("<void>");
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-44>"))))
	   {
	      value_reg = apply (sqrt_proc, (object) args_reg);
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-45>"))))
	   {
	      object proc = null;
	      object proc_args = null;
	      proc_args = PJScheme.cadr ((object) args_reg);
	      proc = PJScheme.car ((object) args_reg);
	      args_reg = proc_args;
	      proc_reg = proc;
	      pc = (Function) apply_proc;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-46>"))))
	   {
	      k_reg =
		 PJScheme.make_cont2 ((object) symbol ("<cont2-23>"),
				      (object) handler_reg, (object) k2_reg);
	      input_reg = PJScheme.car ((object) args_reg);
	      pc = (Function) read_datum;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-47>"))))
	   {
	      k_reg = k2_reg;
	      datum_reg = PJScheme.car ((object) args_reg);
	      pc = (Function) parse;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-48>"))))
	   {
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-74>"),
				     (object) handler_reg, (object) k2_reg);
	      datum_reg = PJScheme.car ((object) args_reg);
	      pc = (Function) parse;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-49>"))))
	   {
	      macro_env = PJScheme.make_macro_env ();
	      toplevel_env = PJScheme.make_toplevel_env ();
	      load_stack = EmptyList;
	      final_reg =
		 PJScheme.list ((object) symbol ("exiting"),
				(object) symbol ("the"),
				(object) symbol ("interpreter"));
	      pc = null;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-50>"))))
	   {
	      object k = null;
	      k = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value_reg = PJScheme.car ((object) args_reg);
	      k_reg = k;
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<proc-51>"))))
	   {
	      object external_function_object = null;
	      external_function_object =
		 PJScheme.list_ref ((object) temp_1, (object) 1);
	      value_reg = apply (external_function_object, (object) args_reg);
	      k_reg = k2_reg;
	      pc = (Function) apply_cont;
	   }
	 else
	    throw new
	       Exception (format
			  (symbol ("apply-proc") + ": " + "bad procedure: ~a",
			   proc_reg));
      }

   }

   new public static object make_macro (params object[]args)
   {
      return ((object) PJScheme.
	      cons ((object) symbol ("macro-transformer"), (object) args));
   }

   new public static void apply_macro ()
   {
      {
	 object temp_1 = null;
	 temp_1 = PJScheme.cdr ((object) macro_reg);
	 if (true_q
	     (PJScheme.
	      Eq ((object) PJScheme.car ((object) temp_1),
		  (object) symbol ("<macro-1>"))))
	   {
	      object name = null;
	      object formals = null;
	      object bodies = null;
	      bodies = PJScheme.cddr ((object) datum_reg);
	      formals = PJScheme.cdadr ((object) datum_reg);
	      name = PJScheme.caadr ((object) datum_reg);
	      value_reg =
		 PJScheme.list ((object) symbol ("define"), (object) name,
				(object) PJScheme.
				cons ((object) symbol ("lambda"),
				      (object) PJScheme.
				      cons ((object) formals,
					    (object) bodies)));
	      pc = (Function) apply_cont;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<macro-2>"))))
	   {
	      object exps = null;
	      exps = PJScheme.cdr ((object) datum_reg);
	      if (true_q (PJScheme.null_q ((object) exps)))
		{
		   value_reg = true;
		   pc = (Function) apply_cont;

		}
	      else
		 if (true_q
		     (PJScheme.
		      null_q ((object) PJScheme.cdr ((object) exps))))
		{
		   value_reg = PJScheme.car ((object) exps);
		   pc = (Function) apply_cont;

		}
	      else
		{
		   value_reg =
		      PJScheme.list ((object) symbol ("if"),
				     (object) PJScheme.car ((object) exps),
				     (object) PJScheme.
				     cons ((object) symbol ("and"),
					   (object) PJScheme.
					   cdr ((object) exps)),
				     (object) false);
		   pc = (Function) apply_cont;

		}
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<macro-3>"))))
	   {
	      object exps = null;
	      exps = PJScheme.cdr ((object) datum_reg);
	      if (true_q (PJScheme.null_q ((object) exps)))
		{
		   value_reg = false;
		   pc = (Function) apply_cont;

		}
	      else
		 if (true_q
		     (PJScheme.
		      null_q ((object) PJScheme.cdr ((object) exps))))
		{
		   value_reg = PJScheme.car ((object) exps);
		   pc = (Function) apply_cont;

		}
	      else
		{
		   value_reg =
		      PJScheme.list ((object) symbol ("let"),
				     (object) PJScheme.
				     list ((object) PJScheme.
					   list ((object) symbol ("bool"),
						 (object) PJScheme.
						 car ((object) exps)),
					   (object) PJScheme.
					   list ((object)
						 symbol ("else-code"),
						 (object) PJScheme.
						 list ((object)
						       symbol ("lambda"),
						       (object) EmptyList,
						       (object) PJScheme.
						       cons ((object)
							     symbol ("or"),
							     (object)
							     PJScheme.
							     cdr ((object)
								  exps))))),
				     (object) PJScheme.
				     list ((object) symbol ("if"),
					   (object) symbol ("bool"),
					   (object) symbol ("bool"),
					   (object) PJScheme.
					   list ((object)
						 symbol ("else-code"))));
		   pc = (Function) apply_cont;

		}
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<macro-4>"))))
	   {
	      object clauses = null;
	      clauses = PJScheme.cdr ((object) datum_reg);
	      if (true_q (PJScheme.null_q ((object) clauses)))
		 throw new
		    Exception (format
			       (symbol ("cond-transformer") + ": " +
				"bad concrete syntax: ~a", datum_reg));
	      else
	      {
		 object first_clause = null;
		 object other_clauses = null;
		 other_clauses = PJScheme.cdr ((object) clauses);
		 first_clause = PJScheme.car ((object) clauses);
		 if (true_q
		     ((((bool) PJScheme.null_q ((object) first_clause))
		       || ((bool) PJScheme.
			   not ((object) PJScheme.
				list_q ((object) first_clause))))))
		    throw new
		       Exception (format
				  (symbol ("cond-transformer") + ": " +
				   "bad concrete syntax: ~a", datum_reg));
		 else
		 {
		    object test_exp = null;
		    object then_exps = null;
		    then_exps = PJScheme.cdr ((object) first_clause);
		    test_exp = PJScheme.car ((object) first_clause);
		    if (true_q
			(PJScheme.
			 Eq ((object) test_exp, (object) symbol ("else"))))
		       if (true_q (PJScheme.null_q ((object) then_exps)))
			  throw new
			     Exception (format
					(symbol ("cond-transformer") + ": " +
					 "bad concrete syntax: (~a)",
					 symbol ("else")));
		    else
		 if (true_q
		     (PJScheme.
		      null_q ((object) PJScheme.cdr ((object) then_exps))))
		   {
		      value_reg = PJScheme.car ((object) then_exps);
		      pc = (Function) apply_cont;

		   }
		 else
		   {
		      value_reg =
			 PJScheme.cons ((object) symbol ("begin"),
					(object) then_exps);
		      pc = (Function) apply_cont;

		   }
		 else
	      if (true_q (PJScheme.null_q ((object) then_exps)))
		 if (true_q (PJScheme.null_q ((object) other_clauses)))
		   {
		      value_reg =
			 PJScheme.list ((object) symbol ("let"),
					(object) PJScheme.
					list ((object) PJScheme.
					      list ((object) symbol ("bool"),
						    (object) test_exp)),
					(object) PJScheme.
					list ((object) symbol ("if"),
					      (object) symbol ("bool"),
					      (object) symbol ("bool")));
		      pc = (Function) apply_cont;

		   }
		 else
		   {
		      value_reg =
			 PJScheme.list ((object) symbol ("let"),
					(object) PJScheme.
					list ((object) PJScheme.
					      list ((object) symbol ("bool"),
						    (object) test_exp),
					      (object) PJScheme.
					      list ((object)
						    symbol ("else-code"),
						    (object) PJScheme.
						    list ((object)
							  symbol ("lambda"),
							  (object) EmptyList,
							  (object) PJScheme.
							  cons ((object)
								symbol
								("cond"),
								(object)
								other_clauses)))),
					(object) PJScheme.
					list ((object) symbol ("if"),
					      (object) symbol ("bool"),
					      (object) symbol ("bool"),
					      (object) PJScheme.
					      list ((object)
						    symbol ("else-code"))));
		      pc = (Function) apply_cont;

		   }
	      else if (true_q (PJScheme.null_q ((object) other_clauses)))
		 if (true_q
		     (PJScheme.
		      null_q ((object) PJScheme.cdr ((object) then_exps))))
		   {
		      value_reg =
			 PJScheme.list ((object) symbol ("if"),
					(object) test_exp,
					(object) PJScheme.
					car ((object) then_exps));
		      pc = (Function) apply_cont;

		   }
		 else
		   {
		      value_reg =
			 PJScheme.list ((object) symbol ("if"),
					(object) test_exp,
					(object) PJScheme.
					cons ((object) symbol ("begin"),
					      (object) then_exps));
		      pc = (Function) apply_cont;

		   }
	      else
		 if (true_q
		     (PJScheme.
		      null_q ((object) PJScheme.cdr ((object) then_exps))))
		{
		   value_reg =
		      PJScheme.list ((object) symbol ("if"),
				     (object) test_exp,
				     (object) PJScheme.
				     car ((object) then_exps),
				     (object) PJScheme.
				     cons ((object) symbol ("cond"),
					   (object) other_clauses));
		   pc = (Function) apply_cont;

		}
	      else
		{
		   value_reg =
		      PJScheme.list ((object) symbol ("if"),
				     (object) test_exp,
				     (object) PJScheme.
				     cons ((object) symbol ("begin"),
					   (object) then_exps),
				     (object) PJScheme.
				     cons ((object) symbol ("cond"),
					   (object) other_clauses));
		   pc = (Function) apply_cont;

		}
		 }
	      }
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<macro-5>"))))
	    if (true_q
		(PJScheme.
		 symbol_q ((object) PJScheme.cadr ((object) datum_reg))))
	      {
		 object name = null;
		 object bindings = null;
		 object vars = null;
		 object exps = null;
		 object bodies = null;
		 name = PJScheme.cadr ((object) datum_reg);
		 bindings = PJScheme.caddr ((object) datum_reg);
		 vars = map (car_proc, (object) bindings);
		 exps = map (cadr_proc, (object) bindings);
		 bodies = PJScheme.cdddr ((object) datum_reg);
		 value_reg =
		    PJScheme.list ((object) symbol ("letrec"),
				   (object) PJScheme.list ((object) PJScheme.
							   list ((object)
								 name,
								 (object)
								 PJScheme.
								 cons ((object) symbol ("lambda"), (object) PJScheme.cons ((object) vars, (object) bodies)))), (object) PJScheme.cons ((object) name, (object) exps));
		 pc = (Function) apply_cont;
	      }
	    else
	      {
		 object bindings = null;
		 object vars = null;
		 object exps = null;
		 object bodies = null;
		 bindings = PJScheme.cadr ((object) datum_reg);
		 vars = map (car_proc, (object) bindings);
		 exps = map (cadr_proc, (object) bindings);
		 bodies = PJScheme.cddr ((object) datum_reg);
		 value_reg =
		    PJScheme.cons ((object) PJScheme.
				   cons ((object) symbol ("lambda"),
					 (object) PJScheme.
					 cons ((object) vars,
					       (object) bodies)),
				   (object) exps);
		 pc = (Function) apply_cont;
	      }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<macro-6>"))))
	   {
	      object decls = null;
	      object vars = null;
	      object procs = null;
	      object bodies = null;
	      decls = PJScheme.cadr ((object) datum_reg);
	      vars = map (car_proc, (object) decls);
	      procs = map (cadr_proc, (object) decls);
	      bodies = PJScheme.cddr ((object) datum_reg);
	      k2_reg =
		 PJScheme.make_cont2 ((object) symbol ("<cont2-12>"),
				      (object) bodies, (object) k_reg);
	      procs_reg = procs;
	      vars_reg = vars;
	      pc = (Function) create_letrec_assignments;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<macro-7>"))))
	   {
	      object bindings = null;
	      object bodies = null;
	      bodies = PJScheme.cddr ((object) datum_reg);
	      bindings = PJScheme.cadr ((object) datum_reg);
	      bodies_reg = bodies;
	      bindings_reg = bindings;
	      pc = (Function) nest_let_star_bindings;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<macro-8>"))))
	   {
	      object exp = null;
	      object clauses = null;
	      clauses = PJScheme.cddr ((object) datum_reg);
	      exp = PJScheme.cadr ((object) datum_reg);
	      if (true_q (PJScheme.symbol_q ((object) exp)))
		{
		   k_reg =
		      PJScheme.make_cont ((object) symbol ("<cont-12>"),
					  (object) k_reg);
		   clauses_reg = clauses;
		   var_reg = exp;
		   pc = (Function) case_clauses_to_simple_cond_clauses;

		}
	      else
		{
		   k2_reg =
		      PJScheme.make_cont2 ((object) symbol ("<cont2-14>"),
					   (object) exp, (object) k_reg);
		   clauses_reg = clauses;
		   var_reg = symbol ("r");
		   pc = (Function) case_clauses_to_cond_clauses;

		}
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("<macro-9>"))))
	   {
	      object exp = null;
	      object clauses = null;
	      clauses = PJScheme.cddr ((object) datum_reg);
	      exp = PJScheme.cadr ((object) datum_reg);
	      if (true_q (PJScheme.symbol_q ((object) exp)))
		{
		   k2_reg =
		      PJScheme.make_cont2 ((object) symbol ("<cont2-16>"),
					   (object) k_reg);
		   clauses_reg = clauses;
		   var_reg = exp;
		   pc = (Function) record_case_clauses_to_cond_clauses;

		}
	      else
		{
		   k2_reg =
		      PJScheme.make_cont2 ((object) symbol ("<cont2-14>"),
					   (object) exp, (object) k_reg);
		   clauses_reg = clauses;
		   var_reg = symbol ("r");
		   pc = (Function) record_case_clauses_to_cond_clauses;

		}
	   }
	 else
	    throw new
	       Exception (format
			  (symbol ("apply-macro") + ": " +
			   "bad macro-transformer: ~a", macro_reg));
      }

   }

   new public static object First (object n)
   {
      return ((object) PJScheme.
	      string_ref ((object) chars_to_scan, (object) n));
   }

   new public static object remaining (object n)
   {
      return ((object) PJScheme.Add ((object) 1, (object) n));
   }

   new public static void scan_input ()
   {
      chars_to_scan =
	 PJScheme.string_append ((object) input_reg,
				 (object) PJScheme.
				 make_string ((object) NULL));
      chars_reg = 0;
      pc = (Function) scan_input_loop;

   }

   new public static void scan_input_loop ()
   {
      k_reg =
	 PJScheme.make_cont2 ((object) symbol ("<cont2-1>"),
			      (object) handler_reg, (object) k_reg);
      buffer_reg = EmptyList;
      action_reg =
	 PJScheme.list ((object) symbol ("goto"),
			(object) symbol ("start-state"));
      pc = (Function) apply_action;

   }

   new public static void scan_string (object input)
   {
      read_line_count = 1;
      read_char_count = 0;
      k_reg = init_cont;
      handler_reg = init_handler;
      input_reg = input;
      pc = (Function) scan_input;

   }

   new public static void scan_file (object filename)
   {
      read_line_count = 1;
      read_char_count = 0;
      k_reg = init_cont;
      handler_reg = init_handler;
      input_reg = PJScheme.read_content ((object) filename);
      pc = (Function) scan_input;

   }

   new public static void apply_action ()
   {
      if (true_q
	  (PJScheme.
	   Eq ((object) PJScheme.car ((object) action_reg),
	       (object) symbol ("shift"))))
	{
	   object next = null;
	   next = PJScheme.list_ref ((object) action_reg, (object) 1);
	   read_char_count =
	      PJScheme.Add ((object) read_char_count, (object) 1);
	   buffer_reg =
	      PJScheme.cons ((object) PJScheme.First ((object) chars_reg),
			     (object) buffer_reg);
	   chars_reg = PJScheme.remaining ((object) chars_reg);
	   action_reg = next;
	   pc = (Function) apply_action;
	}
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) PJScheme.car ((object) action_reg),
		  (object) symbol ("replace"))))
	{
	   object new_char = null;
	   object next = null;
	   next = PJScheme.list_ref ((object) action_reg, (object) 2);
	   new_char = PJScheme.list_ref ((object) action_reg, (object) 1);
	   chars_reg = PJScheme.remaining ((object) chars_reg);
	   buffer_reg =
	      PJScheme.cons ((object) new_char, (object) buffer_reg);
	   action_reg = next;
	   pc = (Function) apply_action;
	}
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) PJScheme.car ((object) action_reg),
		  (object) symbol ("drop-newline"))))
	{
	   object next = null;
	   next = PJScheme.list_ref ((object) action_reg, (object) 1);
	   read_line_count =
	      PJScheme.Add ((object) read_line_count, (object) 1);
	   read_char_count = 0;
	   chars_reg = PJScheme.remaining ((object) chars_reg);
	   action_reg = next;
	   pc = (Function) apply_action;
	}
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) PJScheme.car ((object) action_reg),
		  (object) symbol ("drop"))))
	{
	   object next = null;
	   next = PJScheme.list_ref ((object) action_reg, (object) 1);
	   read_char_count =
	      PJScheme.Add ((object) read_char_count, (object) 1);
	   chars_reg = PJScheme.remaining ((object) chars_reg);
	   action_reg = next;
	   pc = (Function) apply_action;
	}
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) PJScheme.car ((object) action_reg),
		  (object) symbol ("goto"))))
	{
	   object state = null;
	   state = PJScheme.list_ref ((object) action_reg, (object) 1);
	   {
	      object action = null;
	      action =
		 PJScheme.apply_state ((object) state,
				       (object) PJScheme.
				       First ((object) chars_reg));
	      if (true_q
		  (PJScheme.Eq ((object) action, (object) symbol ("error"))))
		 pc = (Function) scan_error;
	      else
		{
		   action_reg = action;
		   pc = (Function) apply_action;

		}
	   }
	}
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) PJScheme.car ((object) action_reg),
		  (object) symbol ("emit"))))
	{
	   object token_type = null;
	   token_type = PJScheme.list_ref ((object) action_reg, (object) 1);
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-3>"),
				  (object) chars_reg, (object) k_reg);
	   token_type_reg = token_type;
	   pc = (Function) convert_buffer_to_token;
	}
      else
	 throw new
	    Exception (format
		       (symbol ("apply-action") + ": " + "invalid action: ~a",
			action_reg));
   }

   new public static void scan_error ()
   {
      {
	 object c = null;
	 c = PJScheme.First ((object) chars_reg);
	 if (true_q (PJScheme.char_is__q ((object) c, (object) NULL)))
	   {
	      exception_reg =
		 PJScheme.
		 format ((object)
			 "unexpected end of input at line: ~a col: ~a",
			 (object) read_line_count, (object) read_char_count);
	      pc = (Function) apply_handler;

	   }
	 else
	   {
	      exception_reg =
		 PJScheme.
		 format ((object)
			 "unexpected character ~a encountered at line: ~a col: ~a",
			 (object) c, (object) read_line_count,
			 (object) read_char_count);
	      pc = (Function) apply_handler;

	   }
      }

   }

   new public static void convert_buffer_to_token ()
   {
      {
	 object buffer = null;
	 buffer = PJScheme.reverse ((object) buffer_reg);
	 if (true_q
	     (PJScheme.
	      Eq ((object) token_type_reg, (object) symbol ("integer"))))
	   {
	      value_reg =
		 PJScheme.list ((object) symbol ("integer"),
				(object) PJScheme.
				list_to_string ((object) buffer));
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) token_type_reg, (object) symbol ("decimal"))))
	   {
	      value_reg =
		 PJScheme.list ((object) symbol ("decimal"),
				(object) PJScheme.
				list_to_string ((object) buffer));
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) token_type_reg, (object) symbol ("rational"))))
	   {
	      value_reg =
		 PJScheme.list ((object) symbol ("rational"),
				(object) PJScheme.
				list_to_string ((object) buffer));
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) token_type_reg,
		     (object) symbol ("identifier"))))
	   {
	      value_reg =
		 PJScheme.list ((object) symbol ("identifier"),
				(object) PJScheme.
				string_to_symbol ((object) PJScheme.
						  list_to_string ((object)
								  buffer)));
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) token_type_reg, (object) symbol ("boolean"))))
	   {
	      value_reg =
		 PJScheme.list ((object) symbol ("boolean"),
				(object) (((bool) PJScheme.
					   char_is__q ((object) PJScheme.
						       car ((object) buffer),
						       (object) 't'))
					  || ((bool) PJScheme.
					      char_is__q ((object) PJScheme.
							  car ((object)
							       buffer),
							  (object) 'T'))));
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) token_type_reg, (object) symbol ("character"))))
	   {
	      value_reg =
		 PJScheme.list ((object) symbol ("character"),
				(object) PJScheme.car ((object) buffer));
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) token_type_reg,
		     (object) symbol ("named-character"))))
	   {
	      object name = null;
	      name = PJScheme.list_to_string ((object) buffer);
	      if (true_q
		  (PJScheme.string_is__q ((object) name, (object) "nul")))
		{
		   value_reg =
		      PJScheme.list ((object) symbol ("character"),
				     (object) NULL);
		   pc = (Function) apply_cont;

		}
	      else
		 if (true_q
		     (PJScheme.
		      string_is__q ((object) name, (object) "space")))
		{
		   value_reg =
		      PJScheme.list ((object) symbol ("character"),
				     (object) ' ');
		   pc = (Function) apply_cont;

		}
	      else
		 if (true_q
		     (PJScheme.string_is__q ((object) name, (object) "tab")))
		{
		   value_reg =
		      PJScheme.list ((object) symbol ("character"),
				     (object) '\t');
		   pc = (Function) apply_cont;

		}
	      else
		 if (true_q
		     (PJScheme.
		      string_is__q ((object) name, (object) "newline")))
		{
		   value_reg =
		      PJScheme.list ((object) symbol ("character"),
				     (object) NEWLINE);
		   pc = (Function) apply_cont;

		}
	      else
		 if (true_q
		     (PJScheme.
		      string_is__q ((object) name, (object) "linefeed")))
		{
		   value_reg =
		      PJScheme.list ((object) symbol ("character"),
				     (object) NEWLINE);
		   pc = (Function) apply_cont;

		}
	      else
		 if (true_q
		     (PJScheme.
		      string_is__q ((object) name, (object) "backspace")))
		{
		   value_reg =
		      PJScheme.list ((object) symbol ("character"),
				     (object) BACKSPACE);
		   pc = (Function) apply_cont;

		}
	      else
		 if (true_q
		     (PJScheme.
		      string_is__q ((object) name, (object) "return")))
		{
		   value_reg =
		      PJScheme.list ((object) symbol ("character"),
				     (object) '\r');
		   pc = (Function) apply_cont;

		}
	      else
		 if (true_q
		     (PJScheme.string_is__q ((object) name, (object) "page")))
		{
		   value_reg =
		      PJScheme.list ((object) symbol ("character"), (object) '');
		   pc = (Function) apply_cont;

		}
	      else
		{
		   exception_reg =
		      PJScheme.
		      format ((object)
			      "invalid character name '~a' at line: ~a col: ~a",
			      (object) name, (object) read_line_count,
			      (object) read_char_count);
		   pc = (Function) apply_handler;

		}
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) token_type_reg, (object) symbol ("string"))))
	   {
	      value_reg =
		 PJScheme.list ((object) symbol ("string"),
				(object) PJScheme.
				list_to_string ((object) buffer));
	      pc = (Function) apply_cont;

	   }
	 else
	   {
	      value_reg = PJScheme.list ((object) token_type_reg);
	      pc = (Function) apply_cont;

	   }
      }

   }

   new public static bool token_type_q (object token, object class_name)
   {
      return ((bool) PJScheme.
	      Eq ((object) PJScheme.car ((object) token),
		  (object) class_name));
   }

   new public static bool char_delimiter_q (object c)
   {
      return ((bool)
	      (((bool) PJScheme.char_whitespace_q ((object) c))
	       || ((bool) PJScheme.
		   char_is__q ((object) c, (object) SINGLEQUOTE))
	       || ((bool) PJScheme.char_is__q ((object) c, (object) '('))
	       || ((bool) PJScheme.char_is__q ((object) c, (object) '['))
	       || ((bool) PJScheme.char_is__q ((object) c, (object) ')'))
	       || ((bool) PJScheme.char_is__q ((object) c, (object) ']'))
	       || ((bool) PJScheme.
		   char_is__q ((object) c, (object) DOUBLEQUOTE))
	       || ((bool) PJScheme.char_is__q ((object) c, (object) ';'))
	       || ((bool) PJScheme.char_is__q ((object) c, (object) '#'))
	       || ((bool) PJScheme.char_is__q ((object) c, (object) NULL))));
   }

   new public static bool char_initial_q (object c)
   {
      return ((bool)
	      (((bool) PJScheme.char_alphabetic_q ((object) c))
	       || ((bool) PJScheme.char_is__q ((object) c, (object) '!'))
	       || ((bool) PJScheme.char_is__q ((object) c, (object) '$'))
	       || ((bool) PJScheme.char_is__q ((object) c, (object) '%'))
	       || ((bool) PJScheme.char_is__q ((object) c, (object) '&'))
	       || ((bool) PJScheme.char_is__q ((object) c, (object) '*'))
	       || ((bool) PJScheme.char_is__q ((object) c, (object) '/'))
	       || ((bool) PJScheme.char_is__q ((object) c, (object) ':'))
	       || ((bool) PJScheme.char_is__q ((object) c, (object) '<'))
	       || ((bool) PJScheme.char_is__q ((object) c, (object) '='))
	       || ((bool) PJScheme.char_is__q ((object) c, (object) '>'))
	       || ((bool) PJScheme.char_is__q ((object) c, (object) '?'))
	       || ((bool) PJScheme.char_is__q ((object) c, (object) '^'))
	       || ((bool) PJScheme.char_is__q ((object) c, (object) '_'))
	       || ((bool) PJScheme.char_is__q ((object) c, (object) TILDE))));
   }

   new public static bool char_special_subsequent_q (object c)
   {
      return ((bool)
	      (((bool) PJScheme.char_is__q ((object) c, (object) '+'))
	       || ((bool) PJScheme.char_is__q ((object) c, (object) '-'))
	       || ((bool) PJScheme.char_is__q ((object) c, (object) '@'))
	       || ((bool) PJScheme.char_is__q ((object) c, (object) '.'))));
   }

   new public static bool char_subsequent_q (object c)
   {
      return ((bool)
	      (((bool) PJScheme.char_initial_q ((object) c))
	       || ((bool) PJScheme.char_numeric_q ((object) c))
	       || ((bool) PJScheme.char_special_subsequent_q ((object) c))));
   }

   new public static bool char_sign_q (object c)
   {
      return ((bool)
	      (((bool) PJScheme.char_is__q ((object) c, (object) '+'))
	       || ((bool) PJScheme.char_is__q ((object) c, (object) '-'))));
   }

   new public static bool char_boolean_q (object c)
   {
      return ((bool)
	      (((bool) PJScheme.char_is__q ((object) c, (object) 't'))
	       || ((bool) PJScheme.char_is__q ((object) c, (object) 'T'))
	       || ((bool) PJScheme.char_is__q ((object) c, (object) 'f'))
	       || ((bool) PJScheme.char_is__q ((object) c, (object) 'F'))));
   }

   new public static object apply_state (object state, object c)
   {
      if (true_q
	  (PJScheme.Eq ((object) state, (object) symbol ("start-state"))))
	 if (true_q (PJScheme.char_is__q ((object) c, (object) NEWLINE)))
	    return ((object) PJScheme.
		    list ((object) symbol ("drop-newline"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol ("start-state"))));
	 else if (true_q (PJScheme.char_whitespace_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("drop"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol ("start-state"))));
	 else if (true_q (PJScheme.char_is__q ((object) c, (object) ';')))
	    return ((object) PJScheme.
		    list ((object) symbol ("drop"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol ("comment-state"))));
	 else if (true_q (PJScheme.char_is__q ((object) c, (object) '(')))
	    return ((object) PJScheme.
		    list ((object) symbol ("drop"),
			  (object) PJScheme.list ((object) symbol ("emit"),
						  (object)
						  symbol ("lparen"))));
	 else if (true_q (PJScheme.char_is__q ((object) c, (object) '[')))
	    return ((object) PJScheme.
		    list ((object) symbol ("drop"),
			  (object) PJScheme.list ((object) symbol ("emit"),
						  (object)
						  symbol ("lbracket"))));
	 else if (true_q (PJScheme.char_is__q ((object) c, (object) ')')))
	    return ((object) PJScheme.
		    list ((object) symbol ("drop"),
			  (object) PJScheme.list ((object) symbol ("emit"),
						  (object)
						  symbol ("rparen"))));
	 else if (true_q (PJScheme.char_is__q ((object) c, (object) ']')))
	    return ((object) PJScheme.
		    list ((object) symbol ("drop"),
			  (object) PJScheme.list ((object) symbol ("emit"),
						  (object)
						  symbol ("rbracket"))));
	 else
	    if (true_q
		(PJScheme.char_is__q ((object) c, (object) SINGLEQUOTE)))
	    return ((object) PJScheme.
		    list ((object) symbol ("drop"),
			  (object) PJScheme.list ((object) symbol ("emit"),
						  (object)
						  symbol ("apostrophe"))));
	 else
	    if (true_q (PJScheme.char_is__q ((object) c, (object) BACKQUOTE)))
	    return ((object) PJScheme.
		    list ((object) symbol ("drop"),
			  (object) PJScheme.list ((object) symbol ("emit"),
						  (object)
						  symbol ("backquote"))));
	 else if (true_q (PJScheme.char_is__q ((object) c, (object) ',')))
	    return ((object) PJScheme.
		    list ((object) symbol ("drop"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol ("comma-state"))));
	 else if (true_q (PJScheme.char_is__q ((object) c, (object) '#')))
	    return ((object) PJScheme.
		    list ((object) symbol ("drop"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("hash-prefix-state"))));
	 else
	    if (true_q
		(PJScheme.char_is__q ((object) c, (object) DOUBLEQUOTE)))
	    return ((object) PJScheme.
		    list ((object) symbol ("drop"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol ("string-state"))));
	 else if (true_q (PJScheme.char_initial_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("identifier-state"))));
	 else if (true_q (PJScheme.char_sign_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol ("signed-state"))));
	 else if (true_q (PJScheme.char_is__q ((object) c, (object) '.')))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("decimal-point-state"))));
	 else if (true_q (PJScheme.char_numeric_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("whole-number-state"))));
	 else if (true_q (PJScheme.char_is__q ((object) c, (object) NULL)))
	    return ((object) PJScheme.
		    list ((object) symbol ("drop"),
			  (object) PJScheme.list ((object) symbol ("emit"),
						  (object)
						  symbol ("end-marker"))));
	 else
	    return ((object) symbol ("error"));
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) state, (object) symbol ("comment-state"))))
	 if (true_q (PJScheme.char_is__q ((object) c, (object) NEWLINE)))
	    return ((object) PJScheme.
		    list ((object) symbol ("drop-newline"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol ("start-state"))));
	 else if (true_q (PJScheme.char_is__q ((object) c, (object) NULL)))
	    return ((object) PJScheme.
		    list ((object) symbol ("goto"),
			  (object) symbol ("start-state")));
	 else
	    return ((object) PJScheme.
		    list ((object) symbol ("drop"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol ("comment-state"))));
      else
	 if (true_q
	     (PJScheme.Eq ((object) state, (object) symbol ("comma-state"))))
	 if (true_q (PJScheme.char_is__q ((object) c, (object) '@')))
	    return ((object) PJScheme.
		    list ((object) symbol ("drop"),
			  (object) PJScheme.list ((object) symbol ("emit"),
						  (object)
						  symbol ("comma-at"))));
	 else
	    return ((object) PJScheme.
		    list ((object) symbol ("emit"),
			  (object) symbol ("comma")));
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) state, (object) symbol ("hash-prefix-state"))))
	 if (true_q (PJScheme.char_boolean_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("emit"),
						  (object)
						  symbol ("boolean"))));
	 else
	    if (true_q (PJScheme.char_is__q ((object) c, (object) BACKSLASH)))
	    return ((object) PJScheme.
		    list ((object) symbol ("drop"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("character-state"))));
	 else if (true_q (PJScheme.char_is__q ((object) c, (object) '(')))
	    return ((object) PJScheme.
		    list ((object) symbol ("drop"),
			  (object) PJScheme.list ((object) symbol ("emit"),
						  (object)
						  symbol ("lvector"))));
	 else
	    return ((object) symbol ("error"));
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) state, (object) symbol ("character-state"))))
	 if (true_q (PJScheme.char_alphabetic_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("alphabetic-character-state"))));
	 else
	    if (true_q
		(PJScheme.
		 not ((object) PJScheme.
		      char_is__q ((object) c, (object) NULL))))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("emit"),
						  (object)
						  symbol ("character"))));
	 else
	    return ((object) symbol ("error"));
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) state,
		  (object) symbol ("alphabetic-character-state"))))
	 if (true_q (PJScheme.char_alphabetic_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("named-character-state"))));
	 else
	    return ((object) PJScheme.
		    list ((object) symbol ("emit"),
			  (object) symbol ("character")));
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) state, (object) symbol ("named-character-state"))))
	 if (true_q (PJScheme.char_delimiter_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("emit"),
			  (object) symbol ("named-character")));
	 else
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("named-character-state"))));
      else
	 if (true_q
	     (PJScheme.Eq ((object) state, (object) symbol ("string-state"))))
	 if (true_q (PJScheme.char_is__q ((object) c, (object) DOUBLEQUOTE)))
	    return ((object) PJScheme.
		    list ((object) symbol ("drop"),
			  (object) PJScheme.list ((object) symbol ("emit"),
						  (object)
						  symbol ("string"))));
	 else
	    if (true_q (PJScheme.char_is__q ((object) c, (object) BACKSLASH)))
	    return ((object) PJScheme.
		    list ((object) symbol ("drop"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("string-escape-state"))));
	 else if (true_q (PJScheme.char_is__q ((object) c, (object) NULL)))
	    return ((object) symbol ("error"));
	 else
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol ("string-state"))));
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) state, (object) symbol ("string-escape-state"))))
	 if (true_q (PJScheme.char_is__q ((object) c, (object) DOUBLEQUOTE)))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol ("string-state"))));
	 else
	    if (true_q (PJScheme.char_is__q ((object) c, (object) BACKSLASH)))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol ("string-state"))));
	 else if (true_q (PJScheme.char_is__q ((object) c, (object) 'b')))
	    return ((object) PJScheme.
		    list ((object) symbol ("replace"), (object) "",
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol ("string-state"))));
	 else if (true_q (PJScheme.char_is__q ((object) c, (object) 'f')))
	    return ((object) PJScheme.
		    list ((object) symbol ("replace"), (object) "", (object) PJScheme.list ((object) symbol ("goto"), (object) symbol ("string-state"))));
	 else if (true_q (PJScheme.char_is__q ((object) c, (object) 'n')))
	    return ((object) PJScheme.
		    list ((object) symbol ("replace"), (object) "\n",
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol ("string-state"))));
	 else if (true_q (PJScheme.char_is__q ((object) c, (object) 't')))
	    return ((object) PJScheme.
		    list ((object) symbol ("replace"), (object) "	",
			  (object) PJScheme.
			  list ((object) symbol ("goto"),
				(object) symbol ("string-state"))));
	 else if (true_q (PJScheme.char_is__q ((object) c, (object) 'r')))
	    return ((object) PJScheme.
		    list ((object) symbol ("replace"), (object) "",
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol ("string-state"))));
	 else
	    return ((object) symbol ("error"));
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) state, (object) symbol ("identifier-state"))))
	 if (true_q (PJScheme.char_subsequent_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("identifier-state"))));
	 else if (true_q (PJScheme.char_delimiter_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("emit"),
			  (object) symbol ("identifier")));
	 else
	    return ((object) symbol ("error"));
      else
	 if (true_q
	     (PJScheme.Eq ((object) state, (object) symbol ("signed-state"))))
	 if (true_q (PJScheme.char_numeric_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("whole-number-state"))));
	 else if (true_q (PJScheme.char_is__q ((object) c, (object) '.')))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("signed-decimal-point-state"))));
	 else if (true_q (PJScheme.char_delimiter_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("emit"),
			  (object) symbol ("identifier")));
	 else if (true_q (PJScheme.char_subsequent_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("identifier-state"))));
	 else
	    return ((object) symbol ("error"));
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) state, (object) symbol ("decimal-point-state"))))
	 if (true_q (PJScheme.char_numeric_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("fractional-number-state"))));
	 else if (true_q (PJScheme.char_delimiter_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("emit"), (object) symbol ("dot")));
	 else if (true_q (PJScheme.char_subsequent_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("identifier-state"))));
	 else
	    return ((object) symbol ("error"));
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) state,
		  (object) symbol ("signed-decimal-point-state"))))
	 if (true_q (PJScheme.char_numeric_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("fractional-number-state"))));
	 else if (true_q (PJScheme.char_delimiter_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("emit"),
			  (object) symbol ("identifier")));
	 else if (true_q (PJScheme.char_subsequent_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("identifier-state"))));
	 else
	    return ((object) symbol ("error"));
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) state, (object) symbol ("whole-number-state"))))
	 if (true_q (PJScheme.char_numeric_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("whole-number-state"))));
	 else if (true_q (PJScheme.char_is__q ((object) c, (object) '.')))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("fractional-number-state"))));
	 else if (true_q (PJScheme.char_is__q ((object) c, (object) '/')))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("rational-number-state"))));
	 else
	    if (true_q
		((((bool) PJScheme.char_is__q ((object) c, (object) 'e'))
		  || ((bool) PJScheme.
		      char_is__q ((object) c, (object) 'E')))))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol ("suffix-state"))));
	 else if (true_q (PJScheme.char_delimiter_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("emit"),
			  (object) symbol ("integer")));
	 else if (true_q (PJScheme.char_subsequent_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("identifier-state"))));
	 else
	    return ((object) symbol ("error"));
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) state,
		  (object) symbol ("fractional-number-state"))))
	 if (true_q (PJScheme.char_numeric_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("fractional-number-state"))));
	 else
	    if (true_q
		((((bool) PJScheme.char_is__q ((object) c, (object) 'e'))
		  || ((bool) PJScheme.
		      char_is__q ((object) c, (object) 'E')))))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol ("suffix-state"))));
	 else if (true_q (PJScheme.char_delimiter_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("emit"),
			  (object) symbol ("decimal")));
	 else if (true_q (PJScheme.char_subsequent_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("identifier-state"))));
	 else
	    return ((object) symbol ("error"));
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) state, (object) symbol ("rational-number-state"))))
	 if (true_q (PJScheme.char_numeric_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("rational-number-state*"))));
	 else if (true_q (PJScheme.char_delimiter_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("emit"),
			  (object) symbol ("identifier")));
	 else if (true_q (PJScheme.char_subsequent_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("identifier-state"))));
	 else
	    return ((object) symbol ("error"));
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) state,
		  (object) symbol ("rational-number-state*"))))
	 if (true_q (PJScheme.char_numeric_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("rational-number-state*"))));
	 else if (true_q (PJScheme.char_delimiter_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("emit"),
			  (object) symbol ("rational")));
	 else if (true_q (PJScheme.char_subsequent_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("identifier-state"))));
	 else
	    return ((object) symbol ("error"));
      else
	 if (true_q
	     (PJScheme.Eq ((object) state, (object) symbol ("suffix-state"))))
	 if (true_q (PJScheme.char_sign_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("signed-exponent-state"))));
	 else if (true_q (PJScheme.char_numeric_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("exponent-state"))));
	 else if (true_q (PJScheme.char_delimiter_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("emit"),
			  (object) symbol ("identifier")));
	 else if (true_q (PJScheme.char_subsequent_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("identifier-state"))));
	 else
	    return ((object) symbol ("error"));
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) state, (object) symbol ("signed-exponent-state"))))
	 if (true_q (PJScheme.char_numeric_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("exponent-state"))));
	 else if (true_q (PJScheme.char_delimiter_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("emit"),
			  (object) symbol ("identifier")));
	 else if (true_q (PJScheme.char_subsequent_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("identifier-state"))));
	 else
	    return ((object) symbol ("error"));
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) state, (object) symbol ("exponent-state"))))
	 if (true_q (PJScheme.char_numeric_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("exponent-state"))));
	 else if (true_q (PJScheme.char_delimiter_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("emit"),
			  (object) symbol ("decimal")));
	 else if (true_q (PJScheme.char_subsequent_q ((object) c)))
	    return ((object) PJScheme.
		    list ((object) symbol ("shift"),
			  (object) PJScheme.list ((object) symbol ("goto"),
						  (object)
						  symbol
						  ("identifier-state"))));
	 else
	    return ((object) symbol ("error"));
      else
	 throw new
	    Exception (format
		       (symbol ("apply-state") + ": " + "invalid state: ~a",
			state));
   }

   new public static object first (object x)
   {
      return ((object) PJScheme.car ((object) x));
   }

   new public static object rest_of (object x)
   {
      return ((object) PJScheme.cdr ((object) x));
   }

   new public static void read_string (object input)
   {
      k_reg = init_cont2;
      handler_reg = init_handler;
      input_reg = input;
      pc = (Function) read_datum;

   }

   new public static void read_datum ()
   {
      read_char_count = 0;
      read_line_count = 1;
      k_reg =
	 PJScheme.make_cont ((object) symbol ("<cont-4>"),
			     (object) handler_reg, (object) k_reg);
      pc = (Function) scan_input;

   }

   new public static object get_line_count (object token)
   {
      return ((object) PJScheme.rac ((object) PJScheme.rdc ((object) token)));
   }

   new public static object get_char_count (object token)
   {
      return ((object) PJScheme.rac ((object) token));
   }

   new public static object rac (object lyst)
   {
      if (true_q (PJScheme.null_q ((object) PJScheme.cdr ((object) lyst))))
	 return ((object) PJScheme.car ((object) lyst));
      else
	 return ((object) PJScheme.
		 rac ((object) PJScheme.cdr ((object) lyst)));
   }

   new public static object rdc (object lyst)
   {
      if (true_q (PJScheme.null_q ((object) PJScheme.cdr ((object) lyst))))
	 return ((object) EmptyList);
      else
	 return ((object) PJScheme.
		 cons ((object) PJScheme.car ((object) lyst),
		       (object) PJScheme.rdc ((object) PJScheme.
					      cdr ((object) lyst))));
   }

   new public static void read_sexp ()
   {
      {
	 object temp_1 = null;
	 temp_1 = PJScheme.first ((object) tokens_reg);
	 if (true_q
	     (PJScheme.
	      Eq ((object) PJScheme.car ((object) temp_1),
		  (object) symbol ("integer"))))
	   {
	      object str = null;
	      str = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value2_reg = PJScheme.rest_of ((object) tokens_reg);
	      value1_reg = PJScheme.string_to_integer ((object) str);
	      pc = (Function) apply_cont2;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("decimal"))))
	   {
	      object str = null;
	      str = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value2_reg = PJScheme.rest_of ((object) tokens_reg);
	      value1_reg = PJScheme.string_to_decimal ((object) str);
	      pc = (Function) apply_cont2;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("rational"))))
	   {
	      object str = null;
	      str = PJScheme.list_ref ((object) temp_1, (object) 1);
	      {
		 object num = null;
		 num = PJScheme.string_to_rational ((object) str);
		 if (true_q (PJScheme.true_q ((object) num)))
		   {
		      value2_reg = PJScheme.rest_of ((object) tokens_reg);
		      value1_reg = num;
		      pc = (Function) apply_cont2;

		   }
		 else
		   {
		      exception_reg =
			 PJScheme.
			 format ((object)
				 "cannot represent ~a at line: ~a col: ~a",
				 (object) str,
				 (object) PJScheme.
				 get_line_count ((object) PJScheme.
						 first ((object) tokens_reg)),
				 (object) PJScheme.
				 get_char_count ((object) PJScheme.
						 first ((object)
							tokens_reg)));
		      pc = (Function) apply_handler;

		   }
	      }
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("boolean"))))
	   {
	      object boolean = null;
	      boolean = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value2_reg = PJScheme.rest_of ((object) tokens_reg);
	      value1_reg = boolean;
	      pc = (Function) apply_cont2;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("character"))))
	   {
	      object chr = null;
	      chr = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value2_reg = PJScheme.rest_of ((object) tokens_reg);
	      value1_reg = chr;
	      pc = (Function) apply_cont2;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("string"))))
	   {
	      object str = null;
	      str = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value2_reg = PJScheme.rest_of ((object) tokens_reg);
	      value1_reg = str;
	      pc = (Function) apply_cont2;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("identifier"))))
	   {
	      object id = null;
	      id = PJScheme.list_ref ((object) temp_1, (object) 1);
	      value2_reg = PJScheme.rest_of ((object) tokens_reg);
	      value1_reg = id;
	      pc = (Function) apply_cont2;
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("apostrophe"))))
	   {
	      keyword_reg = symbol ("quote");
	      pc = (Function) read_abbreviation;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("backquote"))))
	   {
	      keyword_reg = symbol ("quasiquote");
	      pc = (Function) read_abbreviation;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("comma"))))
	   {
	      keyword_reg = symbol ("unquote");
	      pc = (Function) read_abbreviation;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("comma-at"))))
	   {
	      keyword_reg = symbol ("unquote-splicing");
	      pc = (Function) read_abbreviation;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("lparen"))))
	   {
	      object tokens = null;
	      tokens = PJScheme.rest_of ((object) tokens_reg);
	      if (true_q
		  (PJScheme.
		   token_type_q ((object) PJScheme.first ((object) tokens),
				 (object) symbol ("dot"))))
		{
		   tokens_reg = tokens;
		   pc = (Function) read_error;

		}
	      else
		{
		   expected_terminator_reg = symbol ("rparen");
		   tokens_reg = tokens;
		   pc = (Function) read_sexp_sequence;

		}
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("lbracket"))))
	   {
	      object tokens = null;
	      tokens = PJScheme.rest_of ((object) tokens_reg);
	      if (true_q
		  (PJScheme.
		   token_type_q ((object) PJScheme.first ((object) tokens),
				 (object) symbol ("dot"))))
		{
		   tokens_reg = tokens;
		   pc = (Function) read_error;

		}
	      else
		{
		   expected_terminator_reg = symbol ("rbracket");
		   tokens_reg = tokens;
		   pc = (Function) read_sexp_sequence;

		}
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("lvector"))))
	   {
	      k_reg =
		 PJScheme.make_cont2 ((object) symbol ("<cont2-4>"),
				      (object) k_reg);
	      tokens_reg = PJScheme.rest_of ((object) tokens_reg);
	      pc = (Function) read_vector;

	   }
	 else
	    pc = (Function) read_error;
      }

   }

   new public static void read_abbreviation ()
   {
      k_reg =
	 PJScheme.make_cont2 ((object) symbol ("<cont2-5>"),
			      (object) keyword_reg, (object) k_reg);
      tokens_reg = PJScheme.rest_of ((object) tokens_reg);
      pc = (Function) read_sexp;

   }

   new public static void read_sexp_sequence ()
   {
      {
	 object temp_1 = null;
	 temp_1 = PJScheme.first ((object) tokens_reg);
	 if (true_q
	     (PJScheme.
	      memq ((object) PJScheme.car ((object) temp_1),
		    (object) PJScheme.list ((object) symbol ("rparen"),
					    (object) symbol ("rbracket")))))
	   {
	      sexp_reg = EmptyList;
	      pc = (Function) close_sexp_sequence;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("dot"))))
	   {
	      k_reg =
		 PJScheme.make_cont2 ((object) symbol ("<cont2-8>"),
				      (object) expected_terminator_reg,
				      (object) handler_reg, (object) k_reg);
	      tokens_reg = PJScheme.rest_of ((object) tokens_reg);
	      pc = (Function) read_sexp;

	   }
	 else
	   {
	      k_reg =
		 PJScheme.make_cont2 ((object) symbol ("<cont2-7>"),
				      (object) expected_terminator_reg,
				      (object) handler_reg, (object) k_reg);
	      pc = (Function) read_sexp;

	   }
      }

   }

   new public static void close_sexp_sequence ()
   {
      {
	 object temp_1 = null;
	 temp_1 = PJScheme.first ((object) tokens_reg);
	 if (true_q
	     (PJScheme.
	      memq ((object) PJScheme.car ((object) temp_1),
		    (object) PJScheme.list ((object) symbol ("rparen"),
					    (object) symbol ("rbracket")))))
	    if (true_q
		(PJScheme.
		 token_type_q ((object) PJScheme.first ((object) tokens_reg),
			       (object) expected_terminator_reg)))
	      {
		 value2_reg = PJScheme.rest_of ((object) tokens_reg);
		 value1_reg = sexp_reg;
		 pc = (Function) apply_cont2;

	      }
	    else
	       if (true_q
		   (PJScheme.
		    Eq ((object) expected_terminator_reg,
			(object) symbol ("rparen"))))
	      {
		 exception_reg =
		    PJScheme.
		    format ((object)
			    "parenthesized list terminated by bracket at line: ~a col: ~a",
			    (object) PJScheme.
			    get_line_count ((object) PJScheme.
					    first ((object) tokens_reg)),
			    (object) PJScheme.
			    get_char_count ((object) PJScheme.
					    first ((object) tokens_reg)));
		 pc = (Function) apply_handler;

	      }
	    else
	       if (true_q
		   (PJScheme.
		    Eq ((object) expected_terminator_reg,
			(object) symbol ("rbracket"))))
	      {
		 exception_reg =
		    PJScheme.
		    format ((object)
			    "bracketed list terminated by parenthesis at line: ~a col: ~a",
			    (object) PJScheme.
			    get_line_count ((object) PJScheme.
					    first ((object) tokens_reg)),
			    (object) PJScheme.
			    get_char_count ((object) PJScheme.
					    first ((object) tokens_reg)));
		 pc = (Function) apply_handler;

	      }
	    else
	       pc = (Function) read_error;
      }

   }

   new public static void read_vector ()
   {
      {
	 object temp_1 = null;
	 temp_1 = PJScheme.first ((object) tokens_reg);
	 if (true_q
	     (PJScheme.
	      Eq ((object) PJScheme.car ((object) temp_1),
		  (object) symbol ("rparen"))))
	   {
	      value2_reg = PJScheme.rest_of ((object) tokens_reg);
	      value1_reg = EmptyList;
	      pc = (Function) apply_cont2;

	   }
	 else
	   {
	      k_reg =
		 PJScheme.make_cont2 ((object) symbol ("<cont2-9>"),
				      (object) handler_reg, (object) k_reg);
	      pc = (Function) read_sexp;

	   }
      }

   }

   new public static void read_error ()
   {
      {
	 object token = null;
	 token = PJScheme.first ((object) tokens_reg);
	 if (true_q
	     (PJScheme.
	      token_type_q ((object) token, (object) symbol ("end-marker"))))
	   {
	      exception_reg =
		 PJScheme.
		 format ((object)
			 "unexpected end of input at line: ~a col: ~a",
			 (object) PJScheme.get_line_count ((object) token),
			 (object) PJScheme.get_char_count ((object) token));
	      pc = (Function) apply_handler;

	   }
	 else
	   {
	      exception_reg =
		 PJScheme.
		 format ((object)
			 "unexpected token ~a encountered at line: ~a col: ~a",
			 (object) PJScheme.car ((object) token),
			 (object) PJScheme.get_line_count ((object) token),
			 (object) PJScheme.get_char_count ((object) token));
	      pc = (Function) apply_handler;

	   }
      }

   }

   new public static void read_file (object filename)
   {
      k_reg = PJScheme.make_cont ((object) symbol ("<cont-5>"));
      handler_reg = init_handler;
      input_reg = PJScheme.read_content ((object) filename);
      pc = (Function) scan_input;

   }

   new public static void print_unparsed_sexps ()
   {
      if (true_q
	  (PJScheme.
	   token_type_q ((object) PJScheme.first ((object) tokens_reg),
			 (object) symbol ("end-marker"))))
	{
	   value_reg = symbol ("done");
	   pc = (Function) apply_cont;

	}
      else
	{
	   k_reg =
	      PJScheme.make_cont2 ((object) symbol ("<cont2-10>"),
				   (object) handler_reg, (object) k_reg);
	   pc = (Function) read_sexp;

	}

   }

   new public static void read_next_sexp (object tokens)
   {
      k_reg = PJScheme.make_cont2 ((object) symbol ("<cont2-11>"));
      handler_reg = init_handler;
      tokens_reg = tokens;
      pc = (Function) read_sexp;

   }

   new public static object binding_variable (object binding)
   {
      return ((object) PJScheme.car ((object) binding));
   }

   new public static object binding_docstring (object binding)
   {
      return ((object) PJScheme.cadr ((object) binding));
   }

   new public static object binding_value (object binding)
   {
      return ((object) PJScheme.caddr ((object) binding));
   }

   new public static void set_binding_docstring_b (object binding,
						   object docstring)
   {
      PJScheme.set_car_b ((object) PJScheme.cdr ((object) binding),
			  (object) docstring);
   }

   new public static void set_binding_value_b (object binding, object value)
   {
      PJScheme.set_car_b ((object) PJScheme.cddr ((object) binding),
			  (object) value);
   }

   new public static object make_frame (object variables, object values)
   {
      return ((object)
	      map (make_binding_proc, (object) variables, (object) values));
   }

   new public static object first_binding (object frame)
   {
      return ((object) PJScheme.car ((object) frame));
   }

   new public static object rest_of_bindings (object frame)
   {
      return ((object) PJScheme.cdr ((object) frame));
   }

   new public static bool empty_frame_q (object frame)
   {
      return ((bool) PJScheme.null_q ((object) frame));
   }

   new public static object search_frame (object frame, object variable)
   {
      if (true_q (PJScheme.empty_frame_q ((object) frame)))
	 return ((object) false);
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) PJScheme.
		  binding_variable ((object) PJScheme.
				    first_binding ((object) frame)),
		  (object) variable)))
	 return ((object) PJScheme.first_binding ((object) frame));
      else
	 return ((object) PJScheme.
		 search_frame ((object) PJScheme.
			       rest_of_bindings ((object) frame),
			       (object) variable));
   }

   new public static bool environment_q (object x)
   {
      return ((bool)
	      (((bool) PJScheme.pair_q ((object) x))
	       && ((bool) PJScheme.
		   Eq ((object) PJScheme.car ((object) x),
		       (object) symbol ("environment")))));
   }

   new public static object make_empty_environment ()
   {
      return ((object) PJScheme.
	      cons ((object) symbol ("environment"),
		    (object) PJScheme.list ((object) EmptyList)));
   }

   new public static object make_initial_environment (object vars,
						      object vals)
   {
      return ((object) PJScheme.
	      cons ((object) symbol ("environment"),
		    (object) PJScheme.list ((object) PJScheme.
					    make_frame ((object) vars,
							(object) vals))));
   }

   new public static object frames (object env)
   {
      return ((object) PJScheme.cdr ((object) env));
   }

   new public static object extend (object env, object variables,
				    object values)
   {
      return ((object) PJScheme.
	      cons ((object) symbol ("environment"),
		    (object) PJScheme.cons ((object) PJScheme.
					    make_frame ((object) variables,
							(object) values),
					    (object) PJScheme.
					    cdr ((object) env))));
   }

   new public static object search_env (object env, object variable)
   {
      return ((object) PJScheme.
	      search_frames ((object) PJScheme.cdr ((object) env),
			     (object) variable));
   }

   new public static object search_frames (object frames, object variable)
   {
      if (true_q (PJScheme.null_q ((object) frames)))
	 return ((object) false);
      else
	{
	   object binding = null;
	   binding =
	      PJScheme.search_frame ((object) PJScheme.car ((object) frames),
				     (object) variable);
	   if (true_q (binding))
	      return ((object) binding);
	   else
	      return ((object) PJScheme.
		      search_frames ((object) PJScheme.cdr ((object) frames),
				     (object) variable));
	}

   }

   new public static void lookup_value ()
   {
      k_reg =
	 PJScheme.make_cont ((object) symbol ("<cont-6>"), (object) k_reg);
      pc = (Function) lookup_binding;

   }

   new public static void lookup_binding ()
   {
      {
	 object binding = null;
	 binding =
	    PJScheme.search_env ((object) env_reg, (object) variable_reg);
	 if (true_q (binding))
	   {
	      value_reg = binding;
	      pc = (Function) apply_cont;

	   }
	 else
	   {
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-7>"),
				     (object) variable_reg, (object) env_reg,
				     (object) handler_reg, (object) k_reg);
	      pc = (Function) split_variable;

	   }
      }

   }

   new public static void lookup_binding_in_first_frame ()
   {
      {
	 object frame = null;
	 frame = PJScheme.first_frame ((object) env_reg);
	 {
	    object binding = null;
	    binding =
	       PJScheme.search_frame ((object) frame, (object) var_reg);
	    if (true_q (binding))
	      {
		 value_reg = binding;
		 pc = (Function) apply_cont;

	      }
	    else
	      {
		 object new_binding = null;
		 new_binding =
		    PJScheme.make_binding ((object) var_reg,
					   (object) symbol ("undefined"));
		 {
		    object new_frame = null;
		    new_frame =
		       PJScheme.cons ((object) new_binding, (object) frame);
		    PJScheme.set_first_frame_b ((object) env_reg,
						(object) new_frame);
		    value_reg = new_binding;
		    pc = (Function) apply_cont;
		 }
	      }
	 }
      }

   }

   new public static void lookup_variable_components ()
   {
      {
	 object var = null;
	 var = PJScheme.car ((object) components_reg);
	 k_reg =
	    PJScheme.make_cont ((object) symbol ("<cont-8>"),
				(object) components_reg, (object) path_reg,
				(object) var, (object) handler_reg,
				(object) k_reg);
	 var_reg = var;
	 pc = (Function) lookup_module_binding;
      }

   }

   new public static void lookup_module_binding ()
   {
      {
	 object binding = null;
	 binding = PJScheme.search_env ((object) env_reg, (object) var_reg);
	 if (true_q (binding))
	   {
	      value_reg = binding;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.string_is__q ((object) path_reg, (object) "")))
	   {
	      exception_reg =
		 PJScheme.format ((object) "unbound variable ~a",
				  (object) var_reg);
	      pc = (Function) apply_handler;

	   }
	 else
	   {
	      exception_reg =
		 PJScheme.format ((object) "unbound variable ~a in module ~a",
				  (object) var_reg, (object) path_reg);
	      pc = (Function) apply_handler;

	   }
      }

   }

   new public static void split_variable ()
   {
      {
	 object strings = null;
	 strings =
	    PJScheme.group ((object) PJScheme.
			    string_to_list ((object) PJScheme.
					    symbol_to_string ((object)
							      variable_reg)),
			    (object) '.');
	 if (true_q
	     ((((bool) PJScheme.member ((object) "", (object) strings))
	       || ((bool) PJScheme.
		   EqualSign ((object) PJScheme.length ((object) strings),
			      (object) 1)))))
	   {
	      value_reg = false;
	      pc = (Function) apply_cont;

	   }
	 else
	   {
	      value_reg = map (string_to_symbol_proc, (object) strings);
	      pc = (Function) apply_cont;

	   }
      }

   }

   new public static bool syntactic_sugar_q (object datum)
   {
      return ((bool)
	      (((bool) PJScheme.pair_q ((object) datum))
	       && ((bool) PJScheme.
		   symbol_q ((object) PJScheme.car ((object) datum)))
	       && ((bool) PJScheme.
		   true_q ((object) PJScheme.
			   search_env ((object) macro_env,
				       (object) PJScheme.
				       car ((object) datum))))));
   }

   new public static object make_pattern_macro (object clauses)
   {
      return ((object) PJScheme.
	      cons ((object) symbol ("pattern-macro"), (object) clauses));
   }

   new public static object macro_clauses (object macro)
   {
      return ((object) PJScheme.cdr ((object) macro));
   }

   new public static bool pattern_macro_q (object x)
   {
      return ((bool)
	      (((bool) PJScheme.pair_q ((object) x))
	       && ((bool) PJScheme.
		   Eq ((object) PJScheme.car ((object) x),
		       (object) symbol ("pattern-macro")))));
   }

   new public static void expand_once ()
   {
      k_reg =
	 PJScheme.make_cont ((object) symbol ("<cont-9>"), (object) datum_reg,
			     (object) handler_reg, (object) k_reg);
      env_reg = macro_env;
      variable_reg = PJScheme.car ((object) datum_reg);
      pc = (Function) lookup_value;

   }

   new public static void process_macro_clauses ()
   {
      if (true_q (PJScheme.null_q ((object) clauses_reg)))
	{
	   exception_reg =
	      PJScheme.format ((object) "no matching clause found for ~a",
			       (object) datum_reg);
	   pc = (Function) apply_handler;

	}
      else
	{
	   object left_pattern = null;
	   object right_pattern = null;
	   right_pattern = PJScheme.cadar ((object) clauses_reg);
	   left_pattern = PJScheme.caar ((object) clauses_reg);
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-10>"),
				  (object) clauses_reg, (object) datum_reg,
				  (object) right_pattern,
				  (object) handler_reg, (object) k_reg);
	   p2_reg = datum_reg;
	   p1_reg = left_pattern;
	   pc = (Function) unify_patterns;
	}

   }

   new public static void create_letrec_assignments ()
   {
      if (true_q (PJScheme.null_q ((object) vars_reg)))
	{
	   value2_reg = EmptyList;
	   value1_reg = EmptyList;
	   k_reg = k2_reg;
	   pc = (Function) apply_cont2;

	}
      else
	{
	   k2_reg =
	      PJScheme.make_cont2 ((object) symbol ("<cont2-13>"),
				   (object) procs_reg, (object) vars_reg,
				   (object) k2_reg);
	   procs_reg = PJScheme.cdr ((object) procs_reg);
	   vars_reg = PJScheme.cdr ((object) vars_reg);
	   pc = (Function) create_letrec_assignments;

	}

   }

   new public static void nest_let_star_bindings ()
   {
      if (true_q
	  ((((bool) PJScheme.null_q ((object) bindings_reg))
	    || ((bool) PJScheme.
		null_q ((object) PJScheme.cdr ((object) bindings_reg))))))
	{
	   value_reg =
	      PJScheme.cons ((object) symbol ("let"),
			     (object) PJScheme.cons ((object) bindings_reg,
						     (object) bodies_reg));
	   pc = (Function) apply_cont;

	}
      else
	{
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-11>"),
				  (object) bindings_reg, (object) k_reg);
	   bindings_reg = PJScheme.cdr ((object) bindings_reg);
	   pc = (Function) nest_let_star_bindings;

	}

   }

   new public static void case_clauses_to_simple_cond_clauses ()
   {
      if (true_q (PJScheme.null_q ((object) clauses_reg)))
	{
	   value_reg = EmptyList;
	   pc = (Function) apply_cont;

	}
      else
	{
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-13>"),
				  (object) clauses_reg, (object) var_reg,
				  (object) k_reg);
	   clauses_reg = PJScheme.cdr ((object) clauses_reg);
	   pc = (Function) case_clauses_to_simple_cond_clauses;

	}

   }

   new public static void case_clauses_to_cond_clauses ()
   {
      if (true_q (PJScheme.null_q ((object) clauses_reg)))
	{
	   value2_reg = EmptyList;
	   value1_reg = EmptyList;
	   k_reg = k2_reg;
	   pc = (Function) apply_cont2;

	}
      else
	{
	   k2_reg =
	      PJScheme.make_cont2 ((object) symbol ("<cont2-15>"),
				   (object) clauses_reg, (object) var_reg,
				   (object) k2_reg);
	   clauses_reg = PJScheme.cdr ((object) clauses_reg);
	   pc = (Function) case_clauses_to_cond_clauses;

	}

   }

   new public static void record_case_clauses_to_cond_clauses ()
   {
      if (true_q (PJScheme.null_q ((object) clauses_reg)))
	{
	   value2_reg = EmptyList;
	   value1_reg = EmptyList;
	   k_reg = k2_reg;
	   pc = (Function) apply_cont2;

	}
      else
	{
	   k2_reg =
	      PJScheme.make_cont2 ((object) symbol ("<cont2-17>"),
				   (object) clauses_reg, (object) var_reg,
				   (object) k2_reg);
	   clauses_reg = PJScheme.cdr ((object) clauses_reg);
	   pc = (Function) record_case_clauses_to_cond_clauses;

	}

   }

   new public static object make_macro_env ()
   {
      return ((object) PJScheme.
	      make_initial_environment ((object) PJScheme.
					list ((object) symbol ("and"),
					      (object) symbol ("or"),
					      (object) symbol ("cond"),
					      (object) symbol ("let"),
					      (object) symbol ("letrec"),
					      (object) symbol ("let*"),
					      (object) symbol ("case"),
					      (object)
					      symbol ("record-case")),
					(object) PJScheme.
					list ((object) and_transformer,
					      (object) or_transformer,
					      (object) cond_transformer,
					      (object) let_transformer,
					      (object) letrec_transformer,
					      (object) let_star_transformer,
					      (object) case_transformer,
					      (object)
					      record_case_transformer)));
   }

   new public static void parse_string (object make_string)
   {
      k_reg = PJScheme.make_cont2 ((object) symbol ("<cont2-18>"));
      handler_reg = init_handler;
      input_reg = make_string;
      pc = (Function) read_datum;

   }

   new public static void parse ()
   {
      if (true_q (PJScheme.literal_q ((object) datum_reg)))
	{
	   value_reg = PJScheme.lit_exp ((object) datum_reg);
	   pc = (Function) apply_cont;

	}
      else if (true_q (PJScheme.quote_q ((object) datum_reg)))
	{
	   value_reg =
	      PJScheme.lit_exp ((object) PJScheme.cadr ((object) datum_reg));
	   pc = (Function) apply_cont;

	}
      else if (true_q (PJScheme.quasiquote_q ((object) datum_reg)))
	{
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-30>"),
				  (object) handler_reg, (object) k_reg);
	   datum_reg = PJScheme.cadr ((object) datum_reg);
	   pc = (Function) expand_quasiquote;

	}
      else if (true_q (PJScheme.unquote_q ((object) datum_reg)))
	{
	   exception_reg =
	      PJScheme.format ((object) "misplaced ~a", (object) datum_reg);
	   pc = (Function) apply_handler;

	}
      else if (true_q (PJScheme.unquote_splicing_q ((object) datum_reg)))
	{
	   exception_reg =
	      PJScheme.format ((object) "misplaced ~a", (object) datum_reg);
	   pc = (Function) apply_handler;

	}
      else if (true_q (PJScheme.symbol_q ((object) datum_reg)))
	{
	   value_reg = PJScheme.var_exp ((object) datum_reg);
	   pc = (Function) apply_cont;

	}
      else if (true_q (PJScheme.syntactic_sugar_q ((object) datum_reg)))
	{
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-30>"),
				  (object) handler_reg, (object) k_reg);
	   pc = (Function) expand_once;

	}
      else if (true_q (PJScheme.if_then_q ((object) datum_reg)))
	{
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-36>"),
				  (object) datum_reg, (object) handler_reg,
				  (object) k_reg);
	   datum_reg = PJScheme.cadr ((object) datum_reg);
	   pc = (Function) parse;

	}
      else if (true_q (PJScheme.if_else_q ((object) datum_reg)))
	{
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-34>"),
				  (object) datum_reg, (object) handler_reg,
				  (object) k_reg);
	   datum_reg = PJScheme.cadr ((object) datum_reg);
	   pc = (Function) parse;

	}
      else if (true_q (PJScheme.assignment_q ((object) datum_reg)))
	{
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-31>"),
				  (object) datum_reg, (object) k_reg);
	   datum_reg = PJScheme.caddr ((object) datum_reg);
	   pc = (Function) parse;

	}
      else if (true_q (PJScheme.define_q ((object) datum_reg)))
	 if (true_q (PJScheme.mit_style_q ((object) datum_reg)))
	   {
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-30>"),
				     (object) handler_reg, (object) k_reg);
	      macro_reg = mit_define_transformer;
	      pc = (Function) apply_macro;

	   }
	 else
	    if (true_q
		(PJScheme.
		 EqualSign ((object) PJScheme.length ((object) datum_reg),
			    (object) 3)))
	   {
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-29>"),
				     (object) datum_reg, (object) k_reg);
	      datum_reg = PJScheme.caddr ((object) datum_reg);
	      pc = (Function) parse;

	   }
	 else
	   {
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-28>"),
				     (object) datum_reg, (object) handler_reg,
				     (object) k_reg);
	      datum_reg = PJScheme.cadddr ((object) datum_reg);
	      pc = (Function) parse;

	   }
      else if (true_q (PJScheme.define_syntax_q ((object) datum_reg)))
	{
	   value_reg =
	      PJScheme.define_syntax_exp ((object) PJScheme.
					  cadr ((object) datum_reg),
					  (object) PJScheme.
					  cddr ((object) datum_reg));
	   pc = (Function) apply_cont;

	}
      else if (true_q (PJScheme.begin_q ((object) datum_reg)))
	{
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-26>"),
				  (object) datum_reg, (object) handler_reg,
				  (object) k_reg);
	   datum_list_reg = PJScheme.cdr ((object) datum_reg);
	   pc = (Function) parse_all;

	}
      else if (true_q (PJScheme.lambda_q ((object) datum_reg)))
	{
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-25>"),
				  (object) datum_reg, (object) k_reg);
	   datum_reg =
	      PJScheme.cons ((object) symbol ("begin"),
			     (object) PJScheme.cddr ((object) datum_reg));
	   pc = (Function) parse;

	}
      else if (true_q (PJScheme.try_q ((object) datum_reg)))
	 if (true_q
	     (PJScheme.
	      EqualSign ((object) PJScheme.length ((object) datum_reg),
			 (object) 2)))
	   {
	      datum_reg = PJScheme.try_body ((object) datum_reg);
	      pc = (Function) parse;

	   }
	 else
	    if (true_q
		((((bool) PJScheme.
		   EqualSign ((object) PJScheme.length ((object) datum_reg),
			      (object) 3))
		  && ((bool) PJScheme.
		      catch_q ((object) PJScheme.
			       caddr ((object) datum_reg))))))
	   {
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-24>"),
				     (object) datum_reg, (object) handler_reg,
				     (object) k_reg);
	      datum_reg = PJScheme.try_body ((object) datum_reg);
	      pc = (Function) parse;

	   }
	 else
	    if (true_q
		((((bool) PJScheme.
		   EqualSign ((object) PJScheme.length ((object) datum_reg),
			      (object) 3))
		  && ((bool) PJScheme.
		      finally_q ((object) PJScheme.
				 caddr ((object) datum_reg))))))
	   {
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-22>"),
				     (object) datum_reg, (object) handler_reg,
				     (object) k_reg);
	      datum_reg = PJScheme.try_body ((object) datum_reg);
	      pc = (Function) parse;

	   }
	 else
	    if (true_q
		((((bool) PJScheme.
		   EqualSign ((object) PJScheme.length ((object) datum_reg),
			      (object) 4))
		  && ((bool) PJScheme.
		      catch_q ((object) PJScheme.caddr ((object) datum_reg)))
		  && ((bool) PJScheme.
		      finally_q ((object) PJScheme.
				 cadddr ((object) datum_reg))))))
	   {
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-20>"),
				     (object) datum_reg, (object) handler_reg,
				     (object) k_reg);
	      datum_reg = PJScheme.try_body ((object) datum_reg);
	      pc = (Function) parse;

	   }
	 else
	   {
	      exception_reg =
		 PJScheme.format ((object) "bad try syntax: ~a",
				  (object) datum_reg);
	      pc = (Function) apply_handler;

	   }
      else if (true_q (PJScheme.raise_q ((object) datum_reg)))
	{
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-17>"),
				  (object) k_reg);
	   datum_reg = PJScheme.cadr ((object) datum_reg);
	   pc = (Function) parse;

	}
      else if (true_q (PJScheme.dict_q ((object) datum_reg)))
	{
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-16>"),
				  (object) k_reg);
	   pairs_reg = PJScheme.cdr ((object) datum_reg);
	   pc = (Function) parse_pairs;

	}
      else if (true_q (PJScheme.application_q ((object) datum_reg)))
	{
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-15>"),
				  (object) datum_reg, (object) handler_reg,
				  (object) k_reg);
	   datum_reg = PJScheme.car ((object) datum_reg);
	   pc = (Function) parse;

	}
      else
	{
	   exception_reg =
	      PJScheme.format ((object) "bad concrete syntax: ~a",
			       (object) datum_reg);
	   pc = (Function) apply_handler;

	}

   }

   new public static void parse_pairs ()
   {
      if (true_q (PJScheme.null_q ((object) pairs_reg)))
	{
	   value_reg = EmptyList;
	   pc = (Function) apply_cont;

	}
      else
	{
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-39>"),
				  (object) pairs_reg, (object) handler_reg,
				  (object) k_reg);
	   datum_reg = PJScheme.caar ((object) pairs_reg);
	   pc = (Function) parse;

	}

   }

   new public static void parse_all ()
   {
      if (true_q (PJScheme.null_q ((object) datum_list_reg)))
	{
	   value_reg = EmptyList;
	   pc = (Function) apply_cont;

	}
      else
	{
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-41>"),
				  (object) datum_list_reg,
				  (object) handler_reg, (object) k_reg);
	   datum_reg = PJScheme.car ((object) datum_list_reg);
	   pc = (Function) parse;

	}

   }

   new public static void expand_quasiquote ()
   {
      if (true_q (PJScheme.vector_q ((object) datum_reg)))
	{
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-46>"),
				  (object) k_reg);
	   datum_reg = PJScheme.vector_to_list ((object) datum_reg);
	   pc = (Function) expand_quasiquote;

	}
      else
	 if (true_q
	     (PJScheme.not ((object) PJScheme.pair_q ((object) datum_reg))))
	{
	   value_reg =
	      PJScheme.list ((object) symbol ("quote"), (object) datum_reg);
	   pc = (Function) apply_cont;

	}
      else if (true_q (PJScheme.quasiquote_q ((object) datum_reg)))
	{
	   value_reg =
	      PJScheme.list ((object) symbol ("quote"), (object) datum_reg);
	   pc = (Function) apply_cont;

	}
      else if (true_q (PJScheme.unquote_q ((object) datum_reg)))
	{
	   value_reg = PJScheme.cadr ((object) datum_reg);
	   pc = (Function) apply_cont;

	}
      else
	 if (true_q
	     (PJScheme.
	      unquote_splicing_q ((object) PJScheme.
				  car ((object) datum_reg))))
	 if (true_q
	     (PJScheme.null_q ((object) PJScheme.cdr ((object) datum_reg))))
	   {
	      value_reg =
		 PJScheme.cadr ((object) PJScheme.car ((object) datum_reg));
	      pc = (Function) apply_cont;

	   }
	 else
	   {
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-45>"),
				     (object) datum_reg, (object) k_reg);
	      datum_reg = PJScheme.cdr ((object) datum_reg);
	      pc = (Function) expand_quasiquote;

	   }
      else if (true_q (PJScheme.quasiquote_list_q ((object) datum_reg)))
	{
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-44>"),
				  (object) k_reg);
	   pc = (Function) expand_quasiquote_list;

	}
      else
	{
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-43>"),
				  (object) datum_reg, (object) handler_reg,
				  (object) k_reg);
	   datum_reg = PJScheme.car ((object) datum_reg);
	   pc = (Function) expand_quasiquote;

	}

   }

   new public static void expand_quasiquote_list ()
   {
      if (true_q (PJScheme.null_q ((object) datum_reg)))
	{
	   value_reg = EmptyList;
	   pc = (Function) apply_cont;

	}
      else
	{
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-48>"),
				  (object) datum_reg, (object) handler_reg,
				  (object) k_reg);
	   datum_reg = PJScheme.car ((object) datum_reg);
	   pc = (Function) expand_quasiquote;

	}

   }

   new public static bool quasiquote_list_q (object datum)
   {
      return ((bool)
	      (((bool) PJScheme.null_q ((object) datum))
	       ||
	       ((bool)
		(((bool) PJScheme.pair_q ((object) datum))
		 && ((bool) PJScheme.
		     not ((object) PJScheme.quasiquote_q ((object) datum)))
		 && ((bool) PJScheme.
		     not ((object) PJScheme.unquote_q ((object) datum)))
		 && ((bool) PJScheme.
		     not ((object) PJScheme.
			  unquote_splicing_q ((object) datum)))
		 && ((bool) PJScheme.
		     not ((object) PJScheme.
			  quasiquote_q ((object) PJScheme.
					car ((object) datum))))
		 && ((bool) PJScheme.
		     not ((object) PJScheme.
			  unquote_splicing_q ((object) PJScheme.
					      car ((object) datum))))
		 && ((bool) PJScheme.
		     quasiquote_list_q ((object) PJScheme.
					cdr ((object) datum)))))));
   }

   new public static object head (object formals)
   {
      if (true_q (PJScheme.symbol_q ((object) formals)))
	 return ((object) EmptyList);
      else
	 if (true_q
	     (PJScheme.pair_q ((object) PJScheme.cdr ((object) formals))))
	 return ((object) PJScheme.
		 cons ((object) PJScheme.car ((object) formals),
		       (object) PJScheme.head ((object) PJScheme.
					       cdr ((object) formals))));
      else
	 return ((object) PJScheme.
		 list ((object) PJScheme.car ((object) formals)));
   }

   new public static object last (object formals)
   {
      if (true_q (PJScheme.symbol_q ((object) formals)))
	 return ((object) formals);
      else
	 if (true_q
	     (PJScheme.pair_q ((object) PJScheme.cdr ((object) formals))))
	 return ((object) PJScheme.
		 last ((object) PJScheme.cdr ((object) formals)));
      else
	 return ((object) PJScheme.cdr ((object) formals));
   }

   new public static bool mit_style_q (object datum)
   {
      return ((bool) PJScheme.
	      not ((object) PJScheme.
		   symbol_q ((object) PJScheme.cadr ((object) datum))));
   }

   new public static bool literal_q (object datum)
   {
      return ((bool)
	      (((bool) PJScheme.number_q ((object) datum))
	       || ((bool) PJScheme.boolean_q ((object) datum))
	       || ((bool) PJScheme.char_q ((object) datum))
	       || ((bool) PJScheme.string_q ((object) datum))
	       || ((bool) PJScheme.vector_q ((object) datum))));
   }

   new public static bool anything_q (object datum)
   {
      return ((bool) true);
   }

   new public static bool application_q (object datum)
   {
      return ((bool)
	      (((bool) PJScheme.list_q ((object) datum))
	       && ((bool) PJScheme.
		   not ((object) PJScheme.null_q ((object) datum)))
	       && ((bool) PJScheme.
		   not ((object) PJScheme.
			reserved_keyword_q ((object) PJScheme.
					    car ((object) datum))))));
   }

   new public static bool reserved_keyword_q (object x)
   {
      return ((bool)
	      (((bool) PJScheme.symbol_q ((object) x))
	       && ((bool) PJScheme.
		   memq ((object) x,
			 (object) PJScheme.list ((object) symbol ("quote"),
						 (object)
						 symbol ("quasiquote"),
						 (object) symbol ("lambda"),
						 (object) symbol ("if"),
						 (object) symbol ("set!"),
						 (object) symbol ("define"),
						 (object) symbol ("begin"),
						 (object) symbol ("cond"),
						 (object) symbol ("and"),
						 (object) symbol ("or"),
						 (object) symbol ("let"),
						 (object) symbol ("let*"),
						 (object) symbol ("letrec"),
						 (object) symbol ("case"),
						 (object)
						 symbol ("record-case"),
						 (object) symbol ("try"),
						 (object) symbol ("catch"),
						 (object) symbol ("finally"),
						 (object) symbol ("raise"),
						 (object)
						 symbol ("dict"))))));
   }

   new public static object try_body (object x)
   {
      return ((object) PJScheme.cadr ((object) x));
   }

   new public static object catch_var (object x)
   {
      return ((object) PJScheme.cadr ((object) x));
   }

   new public static object catch_exps (object x)
   {
      return ((object) PJScheme.cddr ((object) x));
   }

   new public static object finally_exps (object x)
   {
      return ((object) PJScheme.cdr ((object) x));
   }

   new public static void get_parsed_sexps (object filename)
   {
      k_reg = PJScheme.make_cont ((object) symbol ("<cont-49>"));
      handler_reg = init_handler;
      input_reg = PJScheme.read_content ((object) filename);
      pc = (Function) scan_input;

   }

   new public static void parse_sexps ()
   {
      if (true_q
	  (PJScheme.
	   token_type_q ((object) PJScheme.first ((object) tokens_reg),
			 (object) symbol ("end-marker"))))
	{
	   value_reg = EmptyList;
	   pc = (Function) apply_cont;

	}
      else
	{
	   k_reg =
	      PJScheme.make_cont2 ((object) symbol ("<cont2-19>"),
				   (object) handler_reg, (object) k_reg);
	   pc = (Function) read_sexp;

	}

   }

   new public static void start ()
   {
      pc = (Function) read_eval_print;

   }

   new public static void pretty_print_prim (object arg)
   {
      config.NEED_NEWLINE = false;
      PJScheme.pretty_print ((object) arg);
   }

   new public static void newline_prim ()
   {
      config.NEED_NEWLINE = false;
      PJScheme.newline ();
   }

   new public static void display_prim (object arg)
   {
      {
	 object s = null;
	 object len = null;
	 s = PJScheme.format ((object) "~s", (object) arg);
	 len = PJScheme.string_length ((object) s);
	 config.NEED_NEWLINE =
	    PJScheme.true_q ((object) PJScheme.
			     not ((object) PJScheme.
				  Equal ((object) PJScheme.
					 substring ((object) s,
						    (object) PJScheme.
						    Subtract ((object) len,
							      (object) 1),
						    (object) len),
					 (object) "\n")));
	 PJScheme.display ((object) s);
      }

   }

   new public static void read_eval_print ()
   {
      {
	 object input = null;
	 object input_string = null;
	 input = PJScheme.read_line ((object) "==> ");
	 input_string = PJScheme.format ((object) "~s", (object) input);
	 k_reg = PJScheme.make_cont2 ((object) symbol ("<cont2-20>"));
	 handler_reg = REP_handler;
	 input_reg = input_string;
	 pc = (Function) read_datum;
      }

   }

   new public static void m ()
   {
      if (true_q
	  (PJScheme.
	   Eq ((object) PJScheme.car ((object) exp_reg),
	       (object) symbol ("lit-exp"))))
	{
	   object datum = null;
	   datum = PJScheme.list_ref ((object) exp_reg, (object) 1);
	   value_reg = datum;
	   pc = (Function) apply_cont;
	}
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) PJScheme.car ((object) exp_reg),
		  (object) symbol ("var-exp"))))
	{
	   object id = null;
	   id = PJScheme.list_ref ((object) exp_reg, (object) 1);
	   variable_reg = id;
	   pc = (Function) lookup_value;
	}
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) PJScheme.car ((object) exp_reg),
		  (object) symbol ("if-exp"))))
	{
	   object test_exp = null;
	   object then_exp = null;
	   object else_exp = null;
	   else_exp = PJScheme.list_ref ((object) exp_reg, (object) 3);
	   then_exp = PJScheme.list_ref ((object) exp_reg, (object) 2);
	   test_exp = PJScheme.list_ref ((object) exp_reg, (object) 1);
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-70>"),
				  (object) else_exp, (object) then_exp,
				  (object) env_reg, (object) handler_reg,
				  (object) k_reg);
	   exp_reg = test_exp;
	   pc = (Function) m;
	}
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) PJScheme.car ((object) exp_reg),
		  (object) symbol ("assign-exp"))))
	{
	   object var = null;
	   object rhs_exp = null;
	   rhs_exp = PJScheme.list_ref ((object) exp_reg, (object) 2);
	   var = PJScheme.list_ref ((object) exp_reg, (object) 1);
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-69>"), (object) var,
				  (object) env_reg, (object) handler_reg,
				  (object) k_reg);
	   exp_reg = rhs_exp;
	   pc = (Function) m;
	}
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) PJScheme.car ((object) exp_reg),
		  (object) symbol ("define-exp"))))
	{
	   object var = null;
	   object rhs_exp = null;
	   rhs_exp = PJScheme.list_ref ((object) exp_reg, (object) 2);
	   var = PJScheme.list_ref ((object) exp_reg, (object) 1);
	   if (true_q
	       (PJScheme.
		EqualSign ((object) PJScheme.length ((object) rhs_exp),
			   (object) 1)))
	     {
		k_reg =
		   PJScheme.make_cont ((object) symbol ("<cont-68>"),
				       (object) var, (object) env_reg,
				       (object) handler_reg, (object) k_reg);
		exp_reg = PJScheme.car ((object) rhs_exp);
		pc = (Function) m;

	     }
	   else
	     {
		k_reg =
		   PJScheme.make_cont ((object) symbol ("<cont-66>"),
				       (object) rhs_exp, (object) var,
				       (object) env_reg, (object) handler_reg,
				       (object) k_reg);
		exp_reg = PJScheme.cadr ((object) rhs_exp);
		pc = (Function) m;

	     }
	}
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) PJScheme.car ((object) exp_reg),
		  (object) symbol ("define-syntax-exp"))))
	{
	   object keyword = null;
	   object clauses = null;
	   clauses = PJScheme.list_ref ((object) exp_reg, (object) 2);
	   keyword = PJScheme.list_ref ((object) exp_reg, (object) 1);
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-63>"),
				  (object) clauses, (object) k_reg);
	   env_reg = macro_env;
	   var_reg = keyword;
	   pc = (Function) lookup_binding_in_first_frame;
	}
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) PJScheme.car ((object) exp_reg),
		  (object) symbol ("begin-exp"))))
	{
	   object exps = null;
	   exps = PJScheme.list_ref ((object) exp_reg, (object) 1);
	   exps_reg = exps;
	   pc = (Function) eval_sequence;
	}
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) PJScheme.car ((object) exp_reg),
		  (object) symbol ("lambda-exp"))))
	{
	   object formals = null;
	   object body = null;
	   body = PJScheme.list_ref ((object) exp_reg, (object) 2);
	   formals = PJScheme.list_ref ((object) exp_reg, (object) 1);
	   value_reg =
	      PJScheme.closure ((object) formals, (object) body,
				(object) env_reg);
	   pc = (Function) apply_cont;
	}
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) PJScheme.car ((object) exp_reg),
		  (object) symbol ("mu-lambda-exp"))))
	{
	   object formals = null;
	   object runt = null;
	   object body = null;
	   body = PJScheme.list_ref ((object) exp_reg, (object) 3);
	   runt = PJScheme.list_ref ((object) exp_reg, (object) 2);
	   formals = PJScheme.list_ref ((object) exp_reg, (object) 1);
	   value_reg =
	      PJScheme.mu_closure ((object) formals, (object) runt,
				   (object) body, (object) env_reg);
	   pc = (Function) apply_cont;
	}
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) PJScheme.car ((object) exp_reg),
		  (object) symbol ("try-catch-exp"))))
	{
	   object body = null;
	   object cvar = null;
	   object cexps = null;
	   cexps = PJScheme.list_ref ((object) exp_reg, (object) 3);
	   cvar = PJScheme.list_ref ((object) exp_reg, (object) 2);
	   body = PJScheme.list_ref ((object) exp_reg, (object) 1);
	   {
	      object new_handler = null;
	      new_handler =
		 PJScheme.try_catch_handler ((object) cvar, (object) cexps,
					     (object) env_reg,
					     (object) handler_reg,
					     (object) k_reg);
	      handler_reg = new_handler;
	      exp_reg = body;
	      pc = (Function) m;
	   }
	}
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) PJScheme.car ((object) exp_reg),
		  (object) symbol ("try-finally-exp"))))
	{
	   object body = null;
	   object fexps = null;
	   fexps = PJScheme.list_ref ((object) exp_reg, (object) 2);
	   body = PJScheme.list_ref ((object) exp_reg, (object) 1);
	   {
	      object new_handler = null;
	      new_handler =
		 PJScheme.try_finally_handler ((object) fexps,
					       (object) env_reg,
					       (object) handler_reg);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-62>"),
				     (object) fexps, (object) env_reg,
				     (object) handler_reg, (object) k_reg);
	      handler_reg = new_handler;
	      exp_reg = body;
	      pc = (Function) m;
	   }
	}
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) PJScheme.car ((object) exp_reg),
		  (object) symbol ("try-catch-finally-exp"))))
	{
	   object body = null;
	   object cvar = null;
	   object cexps = null;
	   object fexps = null;
	   fexps = PJScheme.list_ref ((object) exp_reg, (object) 4);
	   cexps = PJScheme.list_ref ((object) exp_reg, (object) 3);
	   cvar = PJScheme.list_ref ((object) exp_reg, (object) 2);
	   body = PJScheme.list_ref ((object) exp_reg, (object) 1);
	   {
	      object new_handler = null;
	      new_handler =
		 PJScheme.try_catch_finally_handler ((object) cvar,
						     (object) cexps,
						     (object) fexps,
						     (object) env_reg,
						     (object) handler_reg,
						     (object) k_reg);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-62>"),
				     (object) fexps, (object) env_reg,
				     (object) handler_reg, (object) k_reg);
	      handler_reg = new_handler;
	      exp_reg = body;
	      pc = (Function) m;
	   }
	}
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) PJScheme.car ((object) exp_reg),
		  (object) symbol ("raise-exp"))))
	{
	   object exp = null;
	   exp = PJScheme.list_ref ((object) exp_reg, (object) 1);
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-60>"),
				  (object) handler_reg);
	   exp_reg = exp;
	   pc = (Function) m;
	}
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) PJScheme.car ((object) exp_reg),
		  (object) symbol ("dict-exp"))))
	{
	   object pairs = null;
	   pairs = PJScheme.list_ref ((object) exp_reg, (object) 1);
	   value_reg =
	      PJScheme.list ((object) symbol ("dict"), (object) pairs);
	   pc = (Function) apply_cont;
	}
      else
	 if (true_q
	     (PJScheme.
	      Eq ((object) PJScheme.car ((object) exp_reg),
		  (object) symbol ("app-exp"))))
	{
	   object rator = null;
	   object operands = null;
	   operands = PJScheme.list_ref ((object) exp_reg, (object) 2);
	   rator = PJScheme.list_ref ((object) exp_reg, (object) 1);
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-59>"),
				  (object) rator, (object) env_reg,
				  (object) handler_reg, (object) k_reg);
	   exps_reg = operands;
	   pc = (Function) m_star;
	}
      else
	 throw new
	    Exception (format
		       (symbol ("m") + ": " + "bad abstract syntax: ~a",
			exp_reg));
   }

   new public static object try_catch_handler (object cvar, object cexps,
					       object env, object handler,
					       object k)
   {
      return ((object) PJScheme.
	      make_handler ((object) symbol ("<handler-3>"), (object) cexps,
			    (object) cvar, (object) env, (object) handler,
			    (object) k));
   }

   new public static object try_finally_handler (object fexps, object env,
						 object handler)
   {
      return ((object) PJScheme.
	      make_handler ((object) symbol ("<handler-4>"), (object) fexps,
			    (object) env, (object) handler));
   }

   new public static object try_catch_finally_handler (object cvar,
						       object cexps,
						       object fexps,
						       object env,
						       object handler,
						       object k)
   {
      return ((object) PJScheme.
	      make_handler ((object) symbol ("<handler-5>"), (object) cexps,
			    (object) cvar, (object) fexps, (object) env,
			    (object) handler, (object) k));
   }

   new public static object closure (object formals, object body, object env)
   {
      return ((object) PJScheme.
	      make_proc ((object) symbol ("<proc-1>"), (object) formals,
			 (object) body, (object) env));
   }

   new public static object mu_closure (object formals, object runt,
					object body, object env)
   {
      return ((object) PJScheme.
	      make_proc ((object) symbol ("<proc-2>"), (object) formals,
			 (object) runt, (object) body, (object) env));
   }

   new public static void m_star ()
   {
      if (true_q (PJScheme.null_q ((object) exps_reg)))
	{
	   value_reg = EmptyList;
	   pc = (Function) apply_cont;

	}
      else
	{
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-72>"),
				  (object) exps_reg, (object) env_reg,
				  (object) handler_reg, (object) k_reg);
	   exp_reg = PJScheme.car ((object) exps_reg);
	   pc = (Function) m;

	}

   }

   new public static void eval_sequence ()
   {
      k_reg =
	 PJScheme.make_cont ((object) symbol ("<cont-73>"), (object) exps_reg,
			     (object) env_reg, (object) handler_reg,
			     (object) k_reg);
      exp_reg = PJScheme.car ((object) exps_reg);
      pc = (Function) m;

   }

   new public static object make_toplevel_env ()
   {
      return ((object) PJScheme.
	      make_initial_env_extended ((object) PJScheme.
					 make_initial_environment ((object)
								   PJScheme.
								   list ((object) symbol ("exit"), (object) symbol ("eval"), (object) symbol ("parse"), (object) symbol ("parse-string"), (object) symbol ("apply"), (object) symbol ("sqrt"), (object) symbol ("print"), (object) symbol ("display"), (object) symbol ("newline"), (object) symbol ("load"), (object) symbol ("null?"), (object) symbol ("cons"), (object) symbol ("car"), (object) symbol ("cdr"), (object) symbol ("list"), (object) symbol ("+"), (object) symbol ("-"), (object) symbol ("*"), (object) symbol ("/"), (object) symbol ("<"), (object) symbol (">"), (object) symbol ("="), (object) symbol ("equal?"), (object) symbol ("eq?"), (object) symbol ("memq"), (object) symbol ("range"), (object) symbol ("set-car!"), (object) symbol ("set-cdr!"), (object) symbol ("import"), (object) symbol ("get"), (object) symbol ("call-with-current-continuation"), (object) symbol ("call/cc"), (object) symbol ("reverse"), (object) symbol ("append"), (object) symbol ("list->vector"), (object) symbol ("dir"), (object) symbol ("current-time"), (object) symbol ("map"), (object) symbol ("for-each"), (object) symbol ("env"), (object) symbol ("using"), (object) symbol ("not"), (object) symbol ("printf"), (object) symbol ("vector"), (object) symbol ("vector-set!"), (object) symbol ("vector-ref"), (object) symbol ("make-vector"), (object) symbol ("help")), (object) PJScheme.list ((object) PJScheme.make_proc ((object) symbol ("<proc-49>")), (object) PJScheme.make_proc ((object) symbol ("<proc-48>")), (object) PJScheme.make_proc ((object) symbol ("<proc-47>")), (object) PJScheme.make_proc ((object) symbol ("<proc-46>")), (object) PJScheme.make_proc ((object) symbol ("<proc-45>")), (object) PJScheme.make_proc ((object) symbol ("<proc-44>")), (object) PJScheme.make_proc ((object) symbol ("<proc-43>")), (object) PJScheme.make_proc ((object) symbol ("<proc-42>")), (object) PJScheme.make_proc ((object) symbol ("<proc-41>")), (object) PJScheme.make_proc ((object) symbol ("<proc-40>")), (object) PJScheme.make_proc ((object) symbol ("<proc-39>")), (object) PJScheme.make_proc ((object) symbol ("<proc-38>")), (object) PJScheme.make_proc ((object) symbol ("<proc-37>")), (object) PJScheme.make_proc ((object) symbol ("<proc-36>")), (object) PJScheme.make_proc ((object) symbol ("<proc-35>")), (object) PJScheme.make_proc ((object) symbol ("<proc-34>")), (object) PJScheme.make_proc ((object) symbol ("<proc-33>")), (object) PJScheme.make_proc ((object) symbol ("<proc-32>")), (object) PJScheme.make_proc ((object) symbol ("<proc-31>")), (object) PJScheme.make_proc ((object) symbol ("<proc-30>")), (object) PJScheme.make_proc ((object) symbol ("<proc-29>")), (object) PJScheme.make_proc ((object) symbol ("<proc-28>")), (object) PJScheme.make_proc ((object) symbol ("<proc-27>")), (object) PJScheme.make_proc ((object) symbol ("<proc-26>")), (object) PJScheme.make_proc ((object) symbol ("<proc-25>")), (object) PJScheme.make_proc ((object) symbol ("<proc-24>")), (object) PJScheme.make_proc ((object) symbol ("<proc-23>")), (object) PJScheme.make_proc ((object) symbol ("<proc-22>")), (object) PJScheme.make_proc ((object) symbol ("<proc-21>")), (object) PJScheme.make_proc ((object) symbol ("<proc-20>")), (object) PJScheme.make_proc ((object) symbol ("<proc-19>")), (object) PJScheme.make_proc ((object) symbol ("<proc-19>")), (object) PJScheme.make_proc ((object) symbol ("<proc-18>")), (object) PJScheme.make_proc ((object) symbol ("<proc-17>")), (object) PJScheme.make_proc ((object) symbol ("<proc-16>")), (object) PJScheme.make_proc ((object) symbol ("<proc-15>")), (object) PJScheme.make_proc ((object) symbol ("<proc-14>")), (object) PJScheme.make_proc ((object) symbol ("<proc-13>")), (object) PJScheme.make_proc ((object) symbol ("<proc-12>")), (object) PJScheme.make_proc ((object) symbol ("<proc-11>")), (object) PJScheme.make_proc ((object) symbol ("<proc-10>")), (object) PJScheme.make_proc ((object) symbol ("<proc-9>")), (object) PJScheme.make_proc ((object) symbol ("<proc-8>")), (object) PJScheme.make_proc ((object) symbol ("<proc-7>")), (object) PJScheme.make_proc ((object) symbol ("<proc-6>")), (object) PJScheme.make_proc ((object) symbol ("<proc-5>")), (object) PJScheme.make_proc ((object) symbol ("<proc-4>")), (object) PJScheme.make_proc ((object) symbol ("<proc-3>"))))));
   }

   new public static void map_prim ()
   {
      {
	 object len = null;
	 object list_args = null;
	 list_args = PJScheme.listify ((object) args_reg);
	 len = PJScheme.length ((object) args_reg);
	 if (true_q (PJScheme.EqualSign ((object) len, (object) 1)))
	   {
	      list1_reg = PJScheme.car ((object) list_args);
	      pc = (Function) map1;

	   }
	 else if (true_q (PJScheme.EqualSign ((object) len, (object) 2)))
	   {
	      list2_reg = PJScheme.cadr ((object) list_args);
	      list1_reg = PJScheme.car ((object) list_args);
	      pc = (Function) map2;

	   }
	 else
	   {
	      lists_reg = list_args;
	      pc = (Function) mapN;

	   }
      }

   }

   new public static object listify (object arg_list)
   {
      if (true_q (PJScheme.null_q ((object) arg_list)))
	 return ((object) EmptyList);
      else
	 if (true_q
	     (PJScheme.list_q ((object) PJScheme.car ((object) arg_list))))
	 return ((object) PJScheme.
		 cons ((object) PJScheme.car ((object) arg_list),
		       (object) PJScheme.listify ((object) PJScheme.
						  cdr ((object) arg_list))));
      else
	 if (true_q
	     (PJScheme.vector_q ((object) PJScheme.car ((object) arg_list))))
	 return ((object) PJScheme.
		 cons ((object) PJScheme.
		       my_vector_to_list ((object) PJScheme.
					  car ((object) arg_list)),
		       (object) PJScheme.listify ((object) PJScheme.
						  cdr ((object) arg_list))));
      else
	 if (true_q
	     (PJScheme.string_q ((object) PJScheme.car ((object) arg_list))))
	 return ((object) PJScheme.
		 cons ((object) PJScheme.
		       string_to_list ((object) PJScheme.
				       car ((object) arg_list)),
		       (object) PJScheme.listify ((object) PJScheme.
						  cdr ((object) arg_list))));
      else
	 throw new
	    Exception (format
		       (symbol ("map") + ": " +
			"cannot use object type '~a' in map",
			PJScheme.get_type ((object) PJScheme.
					   car ((object) arg_list))));
   }

   new public static void map1 ()
   {
      if (true_q (PJScheme.null_q ((object) list1_reg)))
	{
	   value_reg = EmptyList;
	   pc = (Function) apply_cont;

	}
      else if (true_q (PJScheme.dlr_exp_q ((object) proc_reg)))
	{
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-76>"),
				  (object) list1_reg, (object) proc_reg,
				  (object) k_reg);
	   list1_reg = PJScheme.cdr ((object) list1_reg);
	   pc = (Function) map1;

	}
      else
	{
	   k2_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-75>"),
				  (object) list1_reg, (object) proc_reg,
				  (object) env_reg, (object) handler_reg,
				  (object) k_reg);
	   env2_reg = env_reg;
	   args_reg =
	      PJScheme.list ((object) PJScheme.car ((object) list1_reg));
	   pc = (Function) apply_proc;

	}

   }

   new public static void map2 ()
   {
      if (true_q (PJScheme.null_q ((object) list1_reg)))
	{
	   value_reg = EmptyList;
	   pc = (Function) apply_cont;

	}
      else if (true_q (PJScheme.dlr_exp_q ((object) proc_reg)))
	{
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-78>"),
				  (object) list1_reg, (object) list2_reg,
				  (object) proc_reg, (object) k_reg);
	   list2_reg = PJScheme.cdr ((object) list2_reg);
	   list1_reg = PJScheme.cdr ((object) list1_reg);
	   pc = (Function) map2;

	}
      else
	{
	   k2_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-77>"),
				  (object) list1_reg, (object) list2_reg,
				  (object) proc_reg, (object) env_reg,
				  (object) handler_reg, (object) k_reg);
	   env2_reg = env_reg;
	   args_reg =
	      PJScheme.list ((object) PJScheme.car ((object) list1_reg),
			     (object) PJScheme.car ((object) list2_reg));
	   pc = (Function) apply_proc;

	}

   }

   new public static void mapN ()
   {
      if (true_q
	  (PJScheme.null_q ((object) PJScheme.car ((object) lists_reg))))
	{
	   value_reg = EmptyList;
	   pc = (Function) apply_cont;

	}
      else if (true_q (PJScheme.dlr_exp_q ((object) proc_reg)))
	{
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-80>"),
				  (object) lists_reg, (object) proc_reg,
				  (object) k_reg);
	   lists_reg = map (cdr_proc, (object) lists_reg);
	   pc = (Function) mapN;

	}
      else
	{
	   k2_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-79>"),
				  (object) lists_reg, (object) proc_reg,
				  (object) env_reg, (object) handler_reg,
				  (object) k_reg);
	   env2_reg = env_reg;
	   args_reg = map (car_proc, (object) lists_reg);
	   pc = (Function) apply_proc;

	}

   }

   new public static void for_each_prim ()
   {
      {
	 object arg_list = null;
	 arg_list = PJScheme.listify ((object) lists_reg);
	 if (true_q
	     (PJScheme.null_q ((object) PJScheme.car ((object) arg_list))))
	   {
	      value_reg = symbol ("<void>");
	      pc = (Function) apply_cont;

	   }
	 else if (true_q (PJScheme.dlr_exp_q ((object) proc_reg)))
	   {
	      PJScheme.dlr_apply ((object) proc_reg,
				  (object) map (car_proc, (object) arg_list));
	      lists_reg = map (cdr_proc, (object) arg_list);
	      pc = (Function) for_each_prim;

	   }
	 else
	   {
	      k2_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-81>"),
				     (object) arg_list, (object) proc_reg,
				     (object) env_reg, (object) handler_reg,
				     (object) k_reg);
	      env2_reg = env_reg;
	      args_reg = map (car_proc, (object) arg_list);
	      pc = (Function) apply_proc;

	   }
      }

   }

   new public static void get_primitive ()
   {
      {
	 object sym = null;
	 sym = PJScheme.car ((object) args_reg);
	 k_reg =
	    PJScheme.make_cont ((object) symbol ("<cont-82>"),
				(object) args_reg, (object) sym,
				(object) handler_reg, (object) k_reg);
	 variable_reg = sym;
	 pc = (Function) lookup_value;
      }

   }

   new public static void import_primitive ()
   {
      {
	 object filename = null;
	 filename = PJScheme.car ((object) args_reg);
	 if (true_q
	     (PJScheme.null_q ((object) PJScheme.cdr ((object) args_reg))))
	   {
	      filename_reg = filename;
	      pc = (Function) load_file;

	   }
	 else
	   {
	      object module_name = null;
	      module_name = PJScheme.cadr ((object) args_reg);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-83>"),
				     (object) filename, (object) env_reg,
				     (object) handler_reg, (object) k_reg);
	      var_reg = module_name;
	      pc = (Function) lookup_binding_in_first_frame;
	   }
      }

   }

   new public static void call_cc_primitive ()
   {
      {
	 object fake_k = null;
	 fake_k =
	    PJScheme.make_proc ((object) symbol ("<proc-50>"),
				(object) k_reg);
	 if (true_q (PJScheme.dlr_exp_q ((object) proc_reg)))
	   {
	      value_reg =
		 PJScheme.dlr_apply ((object) proc_reg,
				     (object) PJScheme.
				     list ((object) fake_k));
	      pc = (Function) apply_cont;

	   }
	 else
	   {
	      k2_reg = k_reg;
	      env2_reg = env_reg;
	      args_reg = PJScheme.list ((object) fake_k);
	      pc = (Function) apply_proc;

	   }
      }

   }

   new public static object flatten (object lists)
   {
      if (true_q (PJScheme.null_q ((object) lists)))
	 return ((object) EmptyList);
      else
	 if (true_q
	     (PJScheme.list_q ((object) PJScheme.car ((object) lists))))
	 return ((object) PJScheme.
		 append ((object) PJScheme.
			 flatten ((object) PJScheme.car ((object) lists)),
			 (object) PJScheme.flatten ((object) PJScheme.
						    cdr ((object) lists))));
      else
	 return ((object) PJScheme.
		 cons ((object) PJScheme.car ((object) lists),
		       (object) PJScheme.flatten ((object) PJScheme.
						  cdr ((object) lists))));
   }

   new public static object dir (object args, object env)
   {
      return ((object) PJScheme.
	      sort ((Predicate2) symbolLessThan_q,
		    (object) ((PJScheme.null_q ((object) args)) ? (PJScheme.
								   flatten ((object) PJScheme.append ((object) map (get_variables_from_frame_proc, (object) PJScheme.frames ((object) macro_env)), (object) map (get_variables_from_frame_proc, (object) PJScheme.frames ((object) env))))) : (PJScheme.get_variables_from_frame ((object) PJScheme.car ((object) PJScheme.frames ((object) PJScheme.car ((object) args))))))));
   }

   new public static object get_variables_from_frame (object frame)
   {
      return ((object) map (binding_variable_proc, (object) frame));
   }

   new public static bool symbolLessThan_q (object a, object b)
   {
      {
	 object a_string = null;
	 object b_string = null;
	 b_string = PJScheme.symbol_to_string ((object) b);
	 a_string = PJScheme.symbol_to_string ((object) a);
	 return ((bool) PJScheme.
		 stringLessThan_q ((object) a_string, (object) b_string));
      }

   }

   new public static void load_file ()
   {
      if (true_q
	  (PJScheme.member ((object) filename_reg, (object) load_stack)))
	{
	   PJScheme.printf ((object) "skipping recursive load of ~a~%",
			    (object) filename_reg);
	   value_reg = symbol ("<void>");
	   pc = (Function) apply_cont;

	}
      else
	 if (true_q
	     (PJScheme.
	      not ((object) PJScheme.string_q ((object) filename_reg))))
	{
	   exception_reg =
	      PJScheme.format ((object) "filename is not a string: ~a",
			       (object) filename_reg);
	   pc = (Function) apply_handler;

	}
      else
	 if (true_q
	     (PJScheme.
	      not ((object) PJScheme.file_exists_q ((object) filename_reg))))
	{
	   exception_reg =
	      PJScheme.format ((object) "file does not exist: ~a",
			       (object) filename_reg);
	   pc = (Function) apply_handler;

	}
      else
	{
	   load_stack =
	      PJScheme.cons ((object) filename_reg, (object) load_stack);
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-85>"),
				  (object) env_reg, (object) handler_reg,
				  (object) k_reg);
	   input_reg = PJScheme.read_content ((object) filename_reg);
	   pc = (Function) scan_input;

	}

   }

   new public static void load_loop ()
   {
      if (true_q
	  (PJScheme.
	   token_type_q ((object) PJScheme.first ((object) tokens_reg),
			 (object) symbol ("end-marker"))))
	{
	   value_reg = symbol ("<void>");
	   pc = (Function) apply_cont;

	}
      else
	{
	   k_reg =
	      PJScheme.make_cont2 ((object) symbol ("<cont2-24>"),
				   (object) env_reg, (object) handler_reg,
				   (object) k_reg);
	   pc = (Function) read_sexp;

	}

   }

   new public static void load_files ()
   {
      if (true_q (PJScheme.null_q ((object) filenames_reg)))
	{
	   value_reg = symbol ("ok");
	   pc = (Function) apply_cont;

	}
      else
	{
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-88>"),
				  (object) filenames_reg, (object) env_reg,
				  (object) handler_reg, (object) k_reg);
	   filename_reg = PJScheme.car ((object) filenames_reg);
	   pc = (Function) load_file;

	}

   }

   new public static void help_prim ()
   {
      k_reg =
	 PJScheme.make_cont ((object) symbol ("<cont-89>"), (object) k_reg);
      variable_reg = var_reg;
      pc = (Function) lookup_binding;

   }

   new public static object make_external_proc (object
						external_function_object)
   {
      return ((object) PJScheme.
	      make_proc ((object) symbol ("<proc-51>"),
			 (object) external_function_object));
   }

   new public static void Main (string[]args)
   {
      PJScheme.printf ((object) "Pyjama Scheme (0.1)\n");
      PJScheme.printf ((object) "(c) 2009, IPRE\n");
      k_reg = REP_k;
      handler_reg = REP_handler;
      env_reg = toplevel_env;
      filenames_reg = PJScheme.list ((object) args);
      pc = (Function) load_files;
      PJScheme.trampoline ();
   }

   new public static object execute (string input_string)
   {
      k_reg = PJScheme.make_cont2 ((object) symbol ("<cont2-25>"));
      handler_reg = init_handler;
      input_reg = input_string;
      pc = (Function) read_datum;
      return ((object) PJScheme.trampoline ());
   }

   new public static bool pattern_q (object x)
   {
      return ((bool)
	      (((bool) PJScheme.null_q ((object) x))
	       || ((bool) PJScheme.number_q ((object) x))
	       || ((bool) PJScheme.boolean_q ((object) x))
	       || ((bool) PJScheme.symbol_q ((object) x))
	       ||
	       ((bool)
		(((bool) PJScheme.pair_q ((object) x))
		 && ((bool) PJScheme.
		     pattern_q ((object) PJScheme.car ((object) x)))
		 && ((bool) PJScheme.
		     pattern_q ((object) PJScheme.cdr ((object) x)))))));
   }

   new public static bool pattern_variable_q (object x)
   {
      return ((bool)
	      (((bool) PJScheme.symbol_q ((object) x))
	       && ((bool) PJScheme.
		   Equal ((object) "?",
			  (object) PJScheme.substring ((object) PJScheme.
						       symbol_to_string ((object) x), (object) 0, (object) 1)))));
   }

   new public static bool constant_q (object x)
   {
      return ((bool)
	      (((bool) PJScheme.
		not ((object) PJScheme.pattern_variable_q ((object) x)))
	       && ((bool) PJScheme.
		   not ((object) PJScheme.pair_q ((object) x)))));
   }

   new public static void occurs_q ()
   {
      if (true_q (PJScheme.constant_q ((object) pattern_reg)))
	{
	   value_reg = false;
	   pc = (Function) apply_cont;

	}
      else if (true_q (PJScheme.pattern_variable_q ((object) pattern_reg)))
	{
	   value_reg =
	      PJScheme.Equal ((object) var_reg, (object) pattern_reg);
	   pc = (Function) apply_cont;

	}
      else
	{
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-91>"),
				  (object) pattern_reg, (object) var_reg,
				  (object) k_reg);
	   pattern_reg = PJScheme.car ((object) pattern_reg);
	   pc = (Function) occurs_q;

	}

   }

   new public static void unify_patterns ()
   {
      if (true_q (PJScheme.pattern_variable_q ((object) p1_reg)))
	 if (true_q (PJScheme.pattern_variable_q ((object) p2_reg)))
	   {
	      value_reg =
		 PJScheme.make_sub ((object) symbol ("unit"), (object) p1_reg,
				    (object) p2_reg);
	      pc = (Function) apply_cont;

	   }
	 else
	   {
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-92>"),
				     (object) p1_reg, (object) p2_reg,
				     (object) k_reg);
	      pattern_reg = p2_reg;
	      var_reg = p1_reg;
	      pc = (Function) occurs_q;

	   }
      else if (true_q (PJScheme.pattern_variable_q ((object) p2_reg)))
	{
	   temp_1 = p2_reg;
	   temp_2 = p1_reg;
	   p1_reg = temp_1;
	   p2_reg = temp_2;
	   pc = (Function) unify_patterns;

	}
      else
	 if (true_q
	     ((((bool) PJScheme.constant_q ((object) p1_reg))
	       && ((bool) PJScheme.constant_q ((object) p2_reg))
	       && ((bool) PJScheme.
		   Equal ((object) p1_reg, (object) p2_reg)))))
	{
	   value_reg = PJScheme.make_sub ((object) symbol ("empty"));
	   pc = (Function) apply_cont;

	}
      else
	 if (true_q
	     ((((bool) PJScheme.pair_q ((object) p1_reg))
	       && ((bool) PJScheme.pair_q ((object) p2_reg)))))
	{
	   pair2_reg = p2_reg;
	   pair1_reg = p1_reg;
	   pc = (Function) unify_pairs;

	}
      else
	{
	   value_reg = false;
	   pc = (Function) apply_cont;

	}

   }

   new public static void unify_pairs ()
   {
      k_reg =
	 PJScheme.make_cont ((object) symbol ("<cont-96>"),
			     (object) pair1_reg, (object) pair2_reg,
			     (object) k_reg);
      p2_reg = PJScheme.car ((object) pair2_reg);
      p1_reg = PJScheme.car ((object) pair1_reg);
      pc = (Function) unify_patterns;

   }

   new public static void instantiate ()
   {
      if (true_q (PJScheme.constant_q ((object) pattern_reg)))
	{
	   value_reg = pattern_reg;
	   pc = (Function) apply_cont;

	}
      else if (true_q (PJScheme.pattern_variable_q ((object) pattern_reg)))
	{
	   var_reg = pattern_reg;
	   pc = (Function) apply_sub;

	}
      else if (true_q (PJScheme.pair_q ((object) pattern_reg)))
	{
	   k_reg =
	      PJScheme.make_cont ((object) symbol ("<cont-97>"),
				  (object) pattern_reg, (object) s_reg,
				  (object) k_reg);
	   pattern_reg = PJScheme.car ((object) pattern_reg);
	   pc = (Function) instantiate;

	}
      else
	 throw new
	    Exception (format
		       (symbol ("instantiate") + ": " + "bad pattern: ~a",
			pattern_reg));
   }

   new public static object make_sub (params object[]args)
   {
      return ((object) PJScheme.
	      cons ((object) symbol ("substitution"), (object) args));
   }

   new public static void apply_sub ()
   {
      {
	 object temp_1 = null;
	 temp_1 = PJScheme.cdr ((object) s_reg);
	 if (true_q
	     (PJScheme.
	      Eq ((object) PJScheme.car ((object) temp_1),
		  (object) symbol ("empty"))))
	   {
	      value_reg = var_reg;
	      pc = (Function) apply_cont;

	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("unit"))))
	   {
	      object new_var = null;
	      object new_pattern = null;
	      new_pattern = PJScheme.list_ref ((object) temp_1, (object) 2);
	      new_var = PJScheme.list_ref ((object) temp_1, (object) 1);
	      if (true_q
		  (PJScheme.Equal ((object) var_reg, (object) new_var)))
		{
		   value_reg = new_pattern;
		   pc = (Function) apply_cont;

		}
	      else
		{
		   value_reg = var_reg;
		   pc = (Function) apply_cont;

		}
	   }
	 else
	    if (true_q
		(PJScheme.
		 Eq ((object) PJScheme.car ((object) temp_1),
		     (object) symbol ("composite"))))
	   {
	      object s1 = null;
	      object s2 = null;
	      s2 = PJScheme.list_ref ((object) temp_1, (object) 2);
	      s1 = PJScheme.list_ref ((object) temp_1, (object) 1);
	      k_reg =
		 PJScheme.make_cont ((object) symbol ("<cont-98>"),
				     (object) s2, (object) k_reg);
	      s_reg = s1;
	      pc = (Function) apply_sub;
	   }
	 else
	    throw new
	       Exception (format
			  (symbol ("apply-sub") + ": " +
			   "bad substitution: ~a", s_reg));
      }

   }

   static object chars_to_scan = symbol ("undefined");
   static object read_line_count = 1;
   static object read_char_count = 0;
   static object init_cont =
      PJScheme.make_cont ((object) symbol ("<cont-2>"));
   static object init_cont2 =
      PJScheme.make_cont2 ((object) symbol ("<cont2-2>"));
   static object init_handler =
      PJScheme.make_handler ((object) symbol ("<handler-1>"));
   static object mit_define_transformer =
      PJScheme.make_macro ((object) symbol ("<macro-1>"));
   static object and_transformer =
      PJScheme.make_macro ((object) symbol ("<macro-2>"));
   static object or_transformer =
      PJScheme.make_macro ((object) symbol ("<macro-3>"));
   static object cond_transformer =
      PJScheme.make_macro ((object) symbol ("<macro-4>"));
   static object let_transformer =
      PJScheme.make_macro ((object) symbol ("<macro-5>"));
   static object letrec_transformer =
      PJScheme.make_macro ((object) symbol ("<macro-6>"));
   static object let_star_transformer =
      PJScheme.make_macro ((object) symbol ("<macro-7>"));
   static object case_transformer =
      PJScheme.make_macro ((object) symbol ("<macro-8>"));
   static object record_case_transformer =
      PJScheme.make_macro ((object) symbol ("<macro-9>"));
   static Func < object, bool > quote_q =
      PJScheme.tagged_list ((object) symbol ("quote"), (Predicate2) EqualSign,
			    (object) 2);
   static Func < object, bool > quasiquote_q =
      PJScheme.tagged_list ((object) symbol ("quasiquote"),
			    (Predicate2) EqualSign, (object) 2);
   static Func < object, bool > unquote_q =
      PJScheme.tagged_list ((object) symbol ("unquote"),
			    (Predicate2) EqualSign, (object) 2);
   static Func < object, bool > unquote_splicing_q =
      PJScheme.tagged_list ((object) symbol ("unquote-splicing"),
			    (Predicate2) EqualSign, (object) 2);
   static Func < object, bool > if_then_q =
      PJScheme.tagged_list ((object) symbol ("if"), (Predicate2) EqualSign,
			    (object) 3);
   static Func < object, bool > if_else_q =
      PJScheme.tagged_list ((object) symbol ("if"), (Predicate2) EqualSign,
			    (object) 4);
   static Func < object, bool > assignment_q =
      PJScheme.tagged_list ((object) symbol ("set!"), (Predicate2) EqualSign,
			    (object) 3);
   static Func < object, bool > define_q =
      PJScheme.tagged_list ((object) symbol ("define"),
			    (Predicate2) GreaterOrEqual, (object) 3);
   static Func < object, bool > define_syntax_q =
      PJScheme.tagged_list ((object) symbol ("define-syntax"),
			    (Predicate2) GreaterOrEqual, (object) 3);
   static Func < object, bool > begin_q =
      PJScheme.tagged_list ((object) symbol ("begin"),
			    (Predicate2) GreaterOrEqual, (object) 2);
   static Func < object, bool > lambda_q =
      PJScheme.tagged_list ((object) symbol ("lambda"),
			    (Predicate2) GreaterOrEqual, (object) 3);
   static Func < object, bool > raise_q =
      PJScheme.tagged_list ((object) symbol ("raise"), (Predicate2) EqualSign,
			    (object) 2);
   static Func < object, bool > dict_q =
      PJScheme.tagged_list ((object) symbol ("dict"),
			    (Predicate2) GreaterOrEqual, (object) 1);
   static Func < object, bool > try_q =
      PJScheme.tagged_list ((object) symbol ("try"),
			    (Predicate2) GreaterOrEqual, (object) 2);
   static Func < object, bool > catch_q =
      PJScheme.tagged_list ((object) symbol ("catch"),
			    (Predicate2) GreaterOrEqual, (object) 3);
   static Func < object, bool > finally_q =
      PJScheme.tagged_list ((object) symbol ("finally"),
			    (Predicate2) GreaterOrEqual, (object) 2);
   static object REP_k = PJScheme.make_cont ((object) symbol ("<cont-57>"));
   static object REP_handler =
      PJScheme.make_handler ((object) symbol ("<handler-2>"));
   static object load_stack = EmptyList;
   static object toplevel_env = PJScheme.make_toplevel_env ();
   static object macro_env = PJScheme.make_macro_env ();

   new public static object trampoline ()
   {
      while (pc != null)
	{
	   try
	   {
	      pc ();
	   }
	   catch (Exception e)
	   {
	      if (config.DEBUG > 0)
		{
		   exception_reg = e.ToString ();
		}
	      else
		{
		   string[]parts = get_parts (e.ToString (), NEWLINE_STRING);
		   exception_reg = format ("{0}", parts[0]);
		}
	      pc = (Function) apply_handler;
	   }
	}
      return (final_reg);
   }

   public static Proc get_variables_from_frame_proc =
      new Proc ("get-variables-from-frame",
		(Procedure1) get_variables_from_frame, 1, 1);
   public static Proc binding_variable_proc =
      new Proc ("binding-variable", (Procedure1) binding_variable, 1, 1);
}
