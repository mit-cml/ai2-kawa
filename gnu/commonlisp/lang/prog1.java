package gnu.commonlisp.lang;
import gnu.expr.*;
import kawa.lang.*;
import gnu.lists.*;

public class prog1 extends Syntax
{
  /** This is 1 from prog1 and 2 for prog2. */
  int index;

  public static final prog1 prog1 = new prog1("prog1", 1);
  public static final prog1 prog2 = new prog1("prog2", 2);

  public prog1 (String name, int index)
  {
    this.index = index;
    setName(name);
  }

  public Expression rewrite (Object obj, Translator tr)
  {
    int nexps = LList.length(obj);
    if (nexps < index)
      return tr.syntaxError("too few expressions in "+getName());
    if (index == 2)
      {
	Pair pair = (Pair) obj;
	return new BeginExp(tr.rewrite(pair.getCar()),
			    prog1.rewrite(pair.getCdr(), tr));
      }
    else
      {
	Expression[] inits = new Expression[1];
	LetExp let = new LetExp(inits);
	Expression[] body = new Expression[nexps];
	Pair pair = (Pair) obj;
	inits[0] = tr.rewrite(pair.getCar());
	obj = pair.getCdr();
	for (int i = 0;  i < nexps-1;  i++)
	  {
	    pair = (Pair) obj;
	    body[i] = tr.rewrite(pair.getCar());
	    obj = pair.getCdr();
	  }
	Declaration decl = let.addDeclaration((Object) null);
	body[nexps-1] = new ReferenceExp(decl);
	let.body = BeginExp.canonicalize(body);
	tr.mustCompileHere();
	return let;
      }
  }
}
