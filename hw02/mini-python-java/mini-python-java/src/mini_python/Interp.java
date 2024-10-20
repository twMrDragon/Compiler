package mini_python;

import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;

// the following exception is used whenever you have to implement something
class Todo extends Error {
  private static final long serialVersionUID = 1L;

  Todo() {
    super("TODO");
  }
}

/*
 * Values of Mini-Python.
 * 
 * Two main differences wrt Python:
 * 
 * - We use here machine integers (Java type `long`) while Python
 * integers are arbitrary-precision integers (we could use Java
 * big integers but we opt for simplicity here).
 * 
 * - What Python calls a ``list'' is a resizeable array. In Mini-Python,
 * there is no way to modify the length, so a mere Java array can be used.
 */

abstract class Value implements Comparable<Value> {
  abstract boolean isFalse();

  boolean isTrue() {
    return !isFalse();
  }

  long asInt() {
    if (!(this instanceof Vint))
      throw new Error("integer expected");
    return ((Vint) this).n;
  }

  Vlist asList() {
    if (!(this instanceof Vlist))
      throw new Error("list expected");
    return (Vlist) this;
  }
}

class Vnone extends Value {
  @Override
  boolean isFalse() {
    return true;
  }

  @Override
  public String toString() {
    return "None";
  }

  @Override
  public int compareTo(Value o) {
    return o instanceof Vnone ? 0 : -1;
  }
}

class Vbool extends Value {
  final boolean b;

  Vbool(boolean b) {
    this.b = b;
  }

  @Override
  public String toString() {
    return this.b ? "True" : "False";
  }

  @Override
  boolean isFalse() {
    return this.b == false;
  }

  @Override
  public int compareTo(Value o) {
    if (o instanceof Vnone)
      return 1;
    if (o instanceof Vbool) {
      boolean ob = ((Vbool) o).b;
      return b ? (ob ? 0 : 1) : (ob ? -1 : 0);
    }
    return -1;
  }
}

class Vint extends Value {
  final long n;

  Vint(long n) {
    this.n = n;
  }

  @Override
  public String toString() {
    return "" + this.n;
  }

  @Override
  boolean isFalse() {
    return this.n == 0;
  }

  @Override
  public int compareTo(Value o) {
    if (o instanceof Vnone || o instanceof Vbool)
      return 1;
    if (o instanceof Vint) {
      long d = this.n - o.asInt();
      return d < 0 ? -1 : d > 0 ? 1 : 0;
    }
    return -1;
  }
}

class Vstring extends Value {
  final String s;

  Vstring(String s) {
    this.s = s;
  }

  @Override
  public String toString() {
    return this.s;
  }

  @Override
  boolean isFalse() {
    return this.s.isEmpty();
  }

  @Override
  public int compareTo(Value o) {
    if (o instanceof Vnone || o instanceof Vbool || o instanceof Vint)
      return 1;
    if (o instanceof Vstring)
      return this.s.compareTo(((Vstring) o).s);
    return -1;
  }
}

class Vlist extends Value {
  final Value[] l;

  Vlist(int n) {
    this.l = new Value[n];
  }

  Vlist(Value[] l1, Value[] l2) {
    this.l = new Value[l1.length + l2.length];
    System.arraycopy(l1, 0, l, 0, l1.length);
    System.arraycopy(l2, 0, l, l1.length, l2.length);
  }

  @Override
  public String toString() {
    StringBuffer b = new StringBuffer();
    b.append("[");
    for (int i = 0; i < this.l.length; i++) {
      if (i != 0)
        b.append(", ");
      b.append(this.l[i]);
    }
    b.append("]");
    return b.toString();
  }

  @Override
  boolean isFalse() {
    return this.l.length == 0;
  }

  @Override
  public int compareTo(Value o) {
    if (!(o instanceof Vlist))
      return -1;
    Value[] ol = ((Vlist) o).l;
    int n1 = this.l.length, n2 = ol.length;
    int i1 = 0, i2 = 0;
    for (; i1 < n1 && i2 < n2; i1++, i2++) {
      Value v1 = this.l[i1];
      Value v2 = ol[i2];
      int c = v1.compareTo(v2);
      if (c != 0)
        return c;
    }
    if (i1 < n1)
      return 1;
    if (i2 < n2)
      return -1;
    return 0;
  }
}

/*
 * The following exception is used to interpret Python's `return`.
 * 
 * Note: this is an unchecked exception, so that we don't have to
 * add `throws` declarations to the visitor methods.
 */
class Return extends RuntimeException {
  private static final long serialVersionUID = 1L;

  final Value v;

  Return(Value v) {
    this.v = v;
  }
}

/* The interpreter starts here */

class Interp implements Visitor {

  /*
   * The visitor methods do not return values (they have a `void` type).
   * 
   * So, to return values when evaluating a constant or an expression,
   * we use the following wrappers `evalConstant` and `evalExpr`.
   * They call `accept` and the visitor assigns the variable `value`.
   */
  Value value = null;

  Value evalConstant(Constant c) {
    assert value == null; // check for non-reentrance
    c.accept(this);
    Value v = value;
    value = null;
    return v;
  }

  Value evalExpr(Expr e) {
    assert value == null; // check for non-reentrance
    e.accept(this);
    Value v = value;
    value = null;
    return v;
  }

  // interpreting constants is immediate
  public void visit(Cnone c) {
    this.value = new Vnone();
  }

  public void visit(Cbool c) {
    this.value = new Vbool(c.b);
  }

  public void visit(Cstring c) {
    this.value = new Vstring(c.s);
  }

  public void visit(Cint c) {
    this.value = new Vint(c.n);
  }

  // local variables
  HashMap<String, Value> vars;

  Interp() {
    this.vars = new HashMap<String, Value>();
  }

  // functions definitions (functions are global, hence `static`)
  static HashMap<String, Def> functions = new HashMap<String, Def>();

  // binary operators
  static Value binop(Binop op, Value v1, Value v2) {
    switch (op) {
      case Bsub:
        return new Vint(v1.asInt() - v2.asInt());
      case Bmul:
        return new Vint(v1.asInt() * v2.asInt());
      case Bdiv:
        if (v2.asInt() == 0)
          throw new Error("division by zero");
        return new Vint(v1.asInt() / v2.asInt());
      case Bmod:
        if (v2.asInt() == 0)
          throw new Error("modulo by zero");
        return new Vint(v1.asInt() % v2.asInt());
      case Badd:
        if (v1 instanceof Vint && v2 instanceof Vint)
          return new Vint(v1.asInt() + v2.asInt());
        if (v1 instanceof Vstring && v2 instanceof Vstring)
          return new Vstring(v1.toString() + v2.toString());
        if (v1 instanceof Vlist && v2 instanceof Vlist)
          return new Vlist(v1.asList().l, v2.asList().l);
        break;
      case Beq:
        return new Vbool(v1.compareTo(v2) == 0);
      case Bneq:
        return new Vbool(v1.compareTo(v2) != 0);
      case Blt:
        return new Vbool(v1.compareTo(v2) < 0);
      case Ble:
        return new Vbool(v1.compareTo(v2) <= 0);
      case Bgt:
        return new Vbool(v1.compareTo(v2) > 0);
      case Bge:
        return new Vbool(v1.compareTo(v2) >= 0);
      default:
    }
    throw new Error("unsupported operand types");
  }

  // interpreting expressions

  @Override
  public void visit(Ecst e) {
    this.value = evalConstant(e.c);
  }

  @Override
  public void visit(Ebinop e) {
    Value v1 = evalExpr(e.e1);
    switch (e.op) {
      case Band:
        if (v1.isFalse())
          this.value = new Vbool(false);
        else
          this.value = new Vbool(evalExpr(e.e2).isTrue());
        break;
      case Bor:
        if (v1.isTrue())
          this.value = new Vbool(true);
        else
          this.value = new Vbool(evalExpr(e.e2).isTrue());
        break;
      default:
        this.value = binop(e.op, v1, evalExpr(e.e2));
    }
  }

  @Override
  public void visit(Eunop e) {
    Value v = evalExpr(e.e);
    switch (e.op) {
      case Unot:
        this.value = new Vbool(!v.isTrue());
        break;
      case Uneg:
        this.value = new Vint(-v.asInt());
        break;
    }
  }

  @Override
  public void visit(Eident id) {
    if (this.vars.containsKey(id.x.id))
      this.value = this.vars.get(id.x.id);
    else
      throw new Error("don't have this ident");
  }

  @Override
  public void visit(Ecall e) {
    switch (e.f.id) {
      case "len":
        if (e.l.size() != 1)
          throw new Error("wrong number of arguments");
        Value v = evalExpr(e.l.getFirst());
        if (v instanceof Vstring)
          this.value = new Vint(((Vstring) v).s.length());
        else if (v instanceof Vlist)
          this.value = new Vint(((Vlist) v).l.length);
        else
          throw new Error("unsupported operand type");
        break;
      case "list":
        if (e.l.size() != 1)
          throw new Error("wrong number of arguments");
        this.value = evalExpr(e.l.getFirst()).asList();
        break;
      case "range":
        if (e.l.size() != 1)
          throw new Error("wrong number of arguments");
        int n = (int) evalExpr(e.l.getFirst()).asInt();
        Vlist l = new Vlist(n);
        for (int i = 0; i < n; i++) {
          l.l[i] = new Vint(i);
        }
        this.value = l;
        break;
      default:
        if (!functions.containsKey(e.f.id))
          throw new Error("function not found: " + e.f.id);
        Def f = functions.get(e.f.id);
        if (f.l.size() != e.l.size())
          throw new Error("wrong number of arguments");
        Interp interp = new Interp();
        Iterator<Ident> idents = f.l.iterator();
        for (Expr expr : e.l) {
          interp.vars.put(idents.next().id, evalExpr(expr));
        }
        try {
          f.s.accept(interp);
          this.value = new Vnone();
        } catch (Return r) {
          this.value = r.v;
        }
    }
  }

  @Override
  public void visit(Elist e) {
    Vlist l = new Vlist(e.l.size());
    int index = 0;
    for (Expr expr : e.l) {
      l.l[index] = evalExpr(expr);
      index++;
    }
    this.value = l;
  }

  @Override
  public void visit(Eget e) {
    Vlist l = evalExpr(e.e1).asList();
    long index = evalExpr(e.e2).asInt();
    if (index < 0 || index >= l.l.length)
      throw new Error("index out of bounds");
    this.value = l.l[(int) index];
  }

  // interpreting statements

  @Override
  public void visit(Seval s) {
    s.e.accept(this);
  }

  @Override
  public void visit(Sprint s) {
    System.out.println(evalExpr(s.e).toString());
  }

  @Override
  public void visit(Sblock s) {
    for (Stmt st : s.l)
      st.accept(this);
  }

  @Override
  public void visit(Sif s) {
    if (evalExpr(s.e).isTrue())
      s.s1.accept(this);
    else if (s.s2 != null)
      s.s2.accept(this);
  }

  @Override
  public void visit(Sassign s) {
    this.vars.put(s.x.id, evalExpr(s.e));
  }

  @Override
  public void visit(Sreturn s) {
    throw new Return(evalExpr(s.e));
  }

  @Override
  public void visit(Sfor s) {
    Vlist l = evalExpr(s.e).asList();
    for (Value v : l.l) {
      this.vars.put(s.x.id, v);
      s.s.accept(this);
    }
  }

  @Override
  public void visit(Sset s) {
    Vlist l = evalExpr(s.e1).asList();
    long index = evalExpr(s.e2).asInt();
    if (index < 0 || index >= l.l.length)
      throw new Error("index out of bounds");
    l.l[(int) index] = evalExpr(s.e3);
  }
}
