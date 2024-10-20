package mini_python;

import java.io.IOException;
import java.io.Reader;
import java.util.LinkedList;
import java.util.Queue;

import java_cup.runtime.Symbol;

public class MyLexer extends Lexer {

  MyLexer(Reader in) {
    super(in);
  }

  Queue<Symbol> tokens = new LinkedList<Symbol>();

  @Override
  public Symbol next_token() throws IOException, Exception {
    if (tokens.isEmpty()) {
      Symbol token = super.next_token();
      // System.out.println("next_token => " + token);
      if (token.sym == sym.INCINDENT) {
        // System.out.println("INC");
        tokens.add(new Symbol(sym.NEWLINE));
        tokens.add(new Symbol(sym.BEGIN));
      } else if (token.sym == sym.DECINDENT) {
        tokens.add(new Symbol(sym.NEWLINE));
        // System.out.println("DEC " + (Integer)token.value);
        for (int i = 0; i < (Integer)token.value; i++)
          tokens.add(new Symbol(sym.END));
      } else
        tokens.add(token);
    }
    return tokens.poll();
  }

}
