package mini_python;
import java.util.LinkedList;

/* Abstract Syntax of Mini-Python */

/* Parsed trees.
   This is the output of the parser and the input of the type checker. */

class Location {
  final int line;
  final int column;

  Location(int line, int column) {
    this.line = line+1;
    this.column = column;
  }

  @Override
  public String toString() {
    return this.line + ":" + this.column + ":";
  }
}

class Ident {
  final String id;
  final Location loc;

  Ident(String id) {
    this.id = id;
    this.loc = null;
  }
  Ident(String id, Location loc) {
    this.id = id;
    this.loc = loc;
  }
}

/* unary and binary operators */

enum Unop { Uneg, Unot }

enum Binop {
  Badd , Bsub , Bmul , Bdiv , Bmod,
  Beq , Bneq , Blt , Ble , Bgt , Bge,
  Band , Bor
}

/* constants */

abstract class Constant {
  abstract void accept(Visitor v);
  static final Cnone None = new Cnone();
}

class Cnone extends Constant {
  @Override
  void accept(Visitor v) { v.visit(this); }
}
class Cbool extends Constant {
  final boolean b;
  Cbool(boolean b) {
    this.b = b;
  }
  @Override
  void accept(Visitor v) { v.visit(this); }
}
class Cstring extends Constant {
  final String s;
  Cstring(String s) {
    this.s = s;
  }
  @Override
  void accept(Visitor v) { v.visit(this); }
}
class Cint extends Constant {
  final long n;; // Python has arbitrary-precision integers; we simplify here
  Cint(long n) {
    this.n = n;
  }
  @Override
  void accept(Visitor v) { v.visit(this); }
}

/* expressions */

abstract class Expr {
  abstract void accept(Visitor v);
}
class Ecst extends Expr {
  final Constant c;
  Ecst(Constant c) {
    this.c = c;
  }
  @Override
  void accept(Visitor v) { v.visit(this); }
}
class Ebinop extends Expr {
  final Binop op;
  final Expr e1, e2;
  Ebinop(Binop op, Expr e1, Expr e2) {
    super();
    this.op = op;
    this.e1 = e1;
    this.e2 = e2;
  }
  @Override
  void accept(Visitor v) { v.visit(this); }
}
class Eunop extends Expr {
  final Unop op;
  final Expr e;
  Eunop(Unop op, Expr e) {
    super();
    this.op = op;
    this.e = e;
  }
  @Override
  void accept(Visitor v) { v.visit(this); }
}
class Eident extends Expr {
  final Ident x;

  Eident(Ident x) {
    super();
    this.x = x;
  }

  @Override
  void accept(Visitor v) { v.visit(this); }
}
class Eget extends Expr {
  final Expr e1, e2;

  Eget(Expr e1, Expr e2) {
    super();
    this.e1 = e1;
    this.e2 = e2;
  }

  @Override
  void accept(Visitor v) { v.visit(this); }
}
class Ecall extends Expr {
  final Ident f;
  final LinkedList<Expr> l;
  Ecall(Ident f, LinkedList<Expr> l) {
    super();
    this.f = f;
    this.l = l;
  }
  @Override
  void accept(Visitor v) { v.visit(this); }
}
class Elist extends Expr {
  final LinkedList<Expr> l;

  Elist(LinkedList<Expr> l) {
    super();
    this.l = l;
  }
  @Override
  void accept(Visitor v) { v.visit(this); }
}

/* statements */

abstract class Stmt {
  abstract void accept(Visitor v);
}
class Sif extends Stmt {
  final Expr e;
  final Stmt s1, s2;
  Sif(Expr e, Stmt s1, Stmt s2) {
    super();
    this.e = e;
    this.s1 = s1;
    this.s2 = s2;
  }
  @Override
  void accept(Visitor v) { v.visit(this); }
}
class Sreturn extends Stmt {
  final Expr e;

  Sreturn(Expr e) {
    super();
    this.e = e;
  }
  @Override
  void accept(Visitor v) { v.visit(this); }
}
class Sassign extends Stmt {
  final Ident x;
  final Expr e;
  Sassign(Ident x, Expr e) {
    super();
    this.x = x;
    this.e = e;
  }
  @Override
  void accept(Visitor v) { v.visit(this); }
}
class Sprint extends Stmt {
  final Expr e;

  Sprint(Expr e) {
    super();
    this.e = e;
  }
  @Override
  void accept(Visitor v) { v.visit(this); }
}
class Sblock extends Stmt {
  final LinkedList<Stmt> l;
  Sblock() {
    this.l = new LinkedList<Stmt>();
  }
  Sblock(LinkedList<Stmt> l) {
    super();
    this.l = l;
  }
  @Override
  void accept(Visitor v) { v.visit(this); }
}
class Sfor extends Stmt {
  final Ident x;
  final Expr e;
  final Stmt s;
  Sfor(Ident x, Expr e, Stmt s) {
    super();
    this.x = x;
    this.e = e;
    this.s = s;
  }
  @Override
  void accept(Visitor v) { v.visit(this); }
}
class Seval extends Stmt {
  final Expr e;

  Seval(Expr e) {
    super();
    this.e = e;
  }
  @Override
  void accept(Visitor v) { v.visit(this); }
}
class Sset extends Stmt {
  final Expr e1, e2, e3;
  Sset(Expr e1, Expr e2, Expr e3) {
    super();
    this.e1 = e1;
    this.e2 = e2;
    this.e3 = e3;
  }
  @Override
  void accept(Visitor v) { v.visit(this); }
}

/* function definition and file */

class Def {
  final Ident f;
  final LinkedList<Ident> l; // formal parameters
  final Stmt s;

  Def(Ident f, LinkedList<Ident> l, Stmt s) {
    super();
    this.f = f;
    this.l = l;
    this.s = s;
  }
}

class File {
  final LinkedList<Def> l;
  final Stmt s; // a block of global statements

  File(LinkedList<Def> l, Stmt s) {
    super();
    this.l = l;
    this.s = s;
  }
}

/* visitor for the parsed trees
   (feel free to modify it for your needs) */

interface Visitor {
  void visit(Cnone c);
  void visit(Cbool c);
  void visit(Cstring c);
  void visit(Cint c);
  void visit(Ecst e);
  void visit(Ebinop e);
  void visit(Eunop e);
  void visit(Eident e);
  void visit(Ecall e);
  void visit(Eget e);
  void visit(Elist e);
  void visit(Sif s);
  void visit(Sreturn s);
  void visit(Sassign s);
  void visit(Sprint s);
  void visit(Sblock s);
  void visit(Sfor s);
  void visit(Seval s);
  void visit(Sset s);
}

