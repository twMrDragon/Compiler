
package mini_python;

import java_cup.runtime.*;
import java.util.*;
import static mini_python.sym.*;

%%

%class Lexer
%unicode
%cup
%cupdebug
%line
%column
%yylexthrow Exception

/* The symbols produced by the lexical analyzer not just integers, but objects
   of type java_cup.runtime.Symbol. To create such an object, one invokes the
   function symbol(), defined below, and supplies an integer constant, which
   identifies a terminal symbol; if necessary, one also supplies a semantic
   value, of an appropriate type -- this must match the type declared for this
   terminal symbol in Parser.cup. */

/* See https://www2.in.tum.de/repos/cup/develop/src/java_cup/runtime/ */

/* Technical note: CUP seems to assume that the two integer parameters
   passed to the Symbol constructor are character counts for the left
   and right positions. Instead, we choose to provide line and column
   information. Accordingly, we will replace CUP's error reporting
   routine with our own. */

%{

    private Symbol symbol(int id)
    {
	return new Symbol(id, yyline, yycolumn);
    }

    private Symbol symbol(int id, Object value)
    {
	return new Symbol(id, yyline, yycolumn, value);
    }

    static Stack<Integer> indent = new Stack<Integer>();
    { indent.push(0); }

%}
LineTerminator     = \r | \n | \r\n
InputCharacter     = [^\r\n]
WhiteSpace         = [ \t\f]
String             = "\"" [^\"]* "\""

Comment            = "#" {InputCharacter}*

Identifier         = [:jletter:] [:jletterdigit:]*

Integer            = "0" | [1-9] [:digit:]*

%%

/* A specification of which regular expressions to recognize and what
   symbols to produce. */

<YYINITIAL> {

    "="
    { return symbol(EQUAL); }

    ":"
    { return symbol(COLON); }

    ","
    { return symbol(COMMA); }

    "("
    { return symbol(LP); }

    ")"
    { return symbol(RP); }

    "["
    { return symbol(LSQ); }

    "]"
    { return symbol(RSQ); }

    "+"
    { return symbol(PLUS); }

    "-"
    { return symbol(MINUS); }

    "*"
    { return symbol(TIMES); }

    "//"
    { return symbol(DIV); }

    "%"
    { return symbol(MOD); }

    "<"
    { return symbol(CMP, Binop.Blt); }

    "<="
    { return symbol(CMP, Binop.Ble); }

    ">"
    { return symbol(CMP, Binop.Bgt); }

    ">="
    { return symbol(CMP, Binop.Bge); }

    "=="
    { return symbol(CMP, Binop.Beq); }

    "!="
    { return symbol(CMP, Binop.Bneq); }

    "and"
    { return symbol(AND); }

    "or"
    { return symbol(OR); }

    "not"
    { return symbol(NOT); }

    "def"
    { return symbol(DEF); }

    "if"
    { return symbol(IF); }

    "else"
    { return symbol(ELSE); }

    "return"
    { return symbol(RETURN); }

    "print"
    { return symbol(PRINT); }

    "for"
    { return symbol(FOR); }

    "in"
    { return symbol(IN); }

    "None"
    { return symbol(CST, Constant.None); }

    "True"
    { return symbol(CST, new Cbool(true)); }

    "False"
    { return symbol(CST, new Cbool(false)); }

    {Identifier}
    { return symbol(IDENT,
                    new Ident(yytext().intern(),
                              new Location(yyline, yycolumn))); }
    // The call to intern() allows identifiers to be compared using == .

    {String}
    { String s = yytext();
      return symbol(CST, new Cstring(s.substring(1, s.length() - 1))); }

    {Integer}
    { return symbol(CST, new Cint(Long.parseLong(yytext()))); }

    ({WhiteSpace} | {Comment})+
    { /* ignore */ }

    {LineTerminator} (({WhiteSpace} | {Comment})* {LineTerminator})*
    {WhiteSpace}*
    { String s = yytext();
      int n = s.length() - 1 - s.lastIndexOf('\n');
      // System.out.println("n = " + n);
      if (indent.peek() < n) {
        indent.push(n);
        return symbol(INCINDENT);
      } else {
        int k = 0;
        while (indent.peek() > n) { indent.pop(); k++; }
        if (indent.peek() != n)
          throw new Exception(String.format(
            "%d:%d:\nerror: indentation error\n", yyline+1, yycolumn));
        return symbol(DECINDENT, k);
      }
    }

    .
    { throw new Exception (String.format (
        "%d:%d:\nerror: illegal character: '%s'\n", yyline+1, yycolumn, yytext()
      ));
    }

}


