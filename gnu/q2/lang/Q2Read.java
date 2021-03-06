package gnu.q2.lang;
import gnu.kawa.lispexpr.*;
import gnu.expr.QuoteExp;
import gnu.text.*;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.expr.Keyword;
import gnu.kawa.xml.MakeAttribute;

/** A class to read Scheme forms (S-expressions). */

public class Q2Read extends LispReader
{
  void init()
  {
    ((InPort) port).readState = ' ';
  }

  public Q2Read(InPort port)
  {
    super(port);
    init();
  }
  
  public Q2Read(InPort port, SourceMessages messages)
  {
    super(port, messages);
    init();
  }

  /** Skip initial tabs and spaces.
   * @return indentation, encoded as @{code (numberOfTabs<<16)+numberOfSpaces}.
   */
  int skipIndentation ()
      throws java.io.IOException, SyntaxException
  {
    int numTabs = 0, numSpaces = 0;
    int ch = port.read();
    while (ch == '\t')
      {
	numTabs++;
	ch = port.read();
      }
    while (ch == ' ')
      {
	numSpaces++;
	ch = port.read();
      }
    if (ch < 0)
      return -1;
    port.unread();
    return (numTabs << 16) + numSpaces;
  }

  int curIndentation;

  /** Read a "command".
   * Assume curIndentation has been set.
   * After return, the read position is before the first non-WS character
   * of the next command (on the next line); curIndentation has been
   * updated to that of the initial whitespace of that line; and a
   * mark() has been set of the start of the line.
   * Exception: If PARSE_ONE_LINE, return position is *before* newline,
   * and mark is not set.
   */
  Object readIndentCommand ()
    throws java.io.IOException, SyntaxException
  {
    int startIndentation = curIndentation;
    LList rresult = LList.Empty;
    Object obj = LList.Empty;
    PairWithPosition pair = null, last = null;

    for (;;)
      {
	int ch = read();
	if (ch < 0)
	  break;
	if (ch == ' ' || ch == '\t')
	  continue;
	unread();
	if (ch == ')')
	  break;
        if (ch == '\r' || ch == '\n')
          {
	    if (singleLine())
	      break;
	    ch = read();
            port.mark(Integer.MAX_VALUE);
	    int subIndentation = skipIndentation(); // skipHorSpace.
            LList qresult = LList.Empty;
            curIndentation = subIndentation;
            for (;;)
              {
                if (curIndentation == -1)
                  break;
                if (subIndentation != curIndentation)
                  {
                    break;
                  }
                int comparedIndent = Q2.compareIndentation(subIndentation, startIndentation);
                if (comparedIndent == Integer.MIN_VALUE)
                  {
                    error('e', "cannot compare indentation - mix of tabs and spaces");
                    break;
                  }
                if (comparedIndent == -1 || comparedIndent == 1)
                  {
                    error('e', "indentation must differ by 2 or more");
                    break;
                  }
                if (comparedIndent <= 0)
                  {
                    // reset to start of line FIXME
                    break;
                  }
                // comparedIndent >= 2
                int line = port.getLineNumber();
                int column = port.getColumnNumber();
                Object val = readIndentCommand();
                qresult = makePair(val, qresult, line, column);
              }
            if (qresult != LList.Empty)
              {
                qresult = new Pair(kawa.standard.begin.begin,
                                   LList.reverseInPlace(qresult));
                rresult = new Pair(qresult, rresult);
              }
            break;
          }
        int line = port.getLineNumber();
        int column = port.getColumnNumber();

        Object val = readObject();

        rresult = makePair(val, rresult, line, column);
      }
    return LList.reverseInPlace(rresult);
  }

  boolean singleLine()
  {
    return interactive && nesting == 0;
  }

  public Object readCommand ()
      throws java.io.IOException, SyntaxException
  {
    int indent = skipIndentation();
    if (indent < 0)
      return Sequence.eofValue;
    curIndentation = indent;
    Object result = readIndentCommand();
    if (! interactive)
      port.reset();
    return result;
  }

  // RULE: Every newline (not preceded by backslash)
  //   is equivalent to ';'
  // RULE: If a line is followed by one or more lines that are
  //   indented more, add a '(BEGIN' at the end of this line, and
  //   and a ')' at the end of the last line indented more.
  // RULE: Forms separate ';' make a "block": Each form is
  //   either a declaration (visible throughout the block);
  //   or an expression.  Value of block is concatenation of
  //   values of expressions (producting multiple values).
  //   
  /* if parens:
     x + (a b
            c d
              e f) + y
    == x + (a b (c d (e f))) + y [OLD]
    == x + (a b (c d (; e f))) + y OR[*]
    == x + (a b; c d (; e f)) + y
    [*] New RULE[?]: Indentation relative to most recent lparen

    What about:
       x + (a b
        c d
        e f) + y
    == x + (a b (; c d (; e f))) + y OR
       ERROR
     */
    /*
      a b (d e
        f g h)
     */
    /*
      a b c
        d e
    */
    /* <body>
       [%x%]
          a b c
          e f g
            h i
          j k
      == [%x%] (CONCAT (a b c) (e f g (h i)) (j k))
      == [%x%] (; a b c; e f g (; h i); j k)

       [%x%] a b c
          e f g
      == ???
       [%x%] a b c (e f g)
      OR:
       [%x%] (CONCAT (a b c) (e f g))
      == [%x%] a b c (; e f g)

    f a b c
        d e
    == f a (b c) (d e) [probably not]
    == f a b c (; d e)

    if e1
    then
       a b
       c d
    else
       e f
       g h
    ==
      if e1 then (CONCAT (a b) (c d)) else (CONCAT (e f) (g h))

    f
      a b
      c d
    == f (CONCAT (a b) (c d))
    OR f (a b) (c d)
    == f (; a b; c d)
    DEPENDING ON f
    Even if former, what about explicit:
    f (a b) (c d)
    Same?
    Depends on whether f takes a <body> or an <arguments>
    */

  public Object readCommand (boolean forceList)
      throws java.io.IOException, SyntaxException
  {
    int line = port.getLineNumber();
    int startColumn = port.getColumnNumber();
    int lastColumn = startColumn;
    Object obj = LList.Empty;
    PairWithPosition pair = null, last = null;
    for (;;)
      {
	int ch = read();
	if (ch < 0)
	  break;
	if (ch == ' ' || ch == '\t')
	  continue;
	unread();
	if (ch == ')')
	  break;
	line = port.getLineNumber();
	int column = port.getColumnNumber();
	while (ch == '\r' || ch == '\n')
	  {
	    if (singleLine())
	      return obj;
	    ch = read();
	    skipIndentation(); // skipHorSpace.
	    column = port.getColumnNumber();
	    ch = peek();
	    if (column <= startColumn)
	      break;
	  }
	if (column <= startColumn && last != null)
	  break;
	Object next;
	if (column == lastColumn && last != null)
	  next = readCommand();
	else if (column < lastColumn && last != null)
	  {
	    PairWithPosition p = pair;
	    for (;;)
	      {
		Object n = p.getCdr();
		if (n == LList.Empty)
		  break;
		PairWithPosition np = (PairWithPosition) n;
		int pColumn = np.getColumnNumber()-1;
		if (pColumn >= column)
		  {
		    if (pColumn > column)
		      error('e', "some tokens on previous line indented more than current line");
		    n = np.getCdr();
		    if (n != LList.Empty)
		      {
			if (((PairWithPosition) n).getColumnNumber()-1==column)
			  {
			    p = (PairWithPosition) n;
			    continue;
			  }
			last = (PairWithPosition)
			  makePair(np, port.getLineNumber(), column);
			p.setCdrBackdoor(last);
		      }
		    break;
		  }
		p = np;
	      }
	    next = readCommand();
	  }
	else
	  next = readObject();
	if (next == Sequence.eofValue)
	  break;
	lastColumn = column;
	String filename = port.getName();
	PairWithPosition cur = PairWithPosition.make(next, LList.Empty,
						     filename, line+1, column+1);
	if (last == null)
	  {
	    pair = cur;
	    obj = cur;
	  }
	else if (last.getCar() instanceof Keyword)
	  {
	    Object name = new QuoteExp(((Keyword) last.getCar()).getName());
	    last.setCar(new PairWithPosition(last, MakeAttribute.makeAttribute,
                                              new PairWithPosition(last, name, cur)));
	    continue;
	  }
	else
	  last.setCdrBackdoor(cur);
	last = cur;
      }
    if (! forceList)
      {
	if (obj == last)
	  obj = last.getCar();
	else if (last == null)
	  obj = QuoteExp.voidExp;
      }
    return obj;
  }

  public static Object readObject(InPort port)
      throws java.io.IOException, SyntaxException
  {
    return (new Q2Read(port)).readObject();
  }

  /** Record '[' location for error messages. */ 
  String expressionStartFile;
  int expressionStartLine;
  int expressionStartColumn;

  void saveExpressionStartPosition()
  {
    expressionStartFile = port.getName();
    expressionStartLine = port.getLineNumber();
    expressionStartColumn = port.getColumnNumber();
  }
}

class Q2ReaderParens extends ReaderDispatchMisc
{
  public Object read (Lexer in, int ch, int count)
    throws java.io.IOException, SyntaxException
  {
    Q2Read reader = (Q2Read) in;
    char saveReadState = reader.pushNesting('(');
    try
      {
	Object result = reader.readCommand(true);

	LineBufferedReader port = reader.getPort();
	if (port.read() != ')')
	  reader.error("missing ')'");
	return result;
      }
    finally
      {
	reader.popNesting(saveReadState);
      }
  }

}
