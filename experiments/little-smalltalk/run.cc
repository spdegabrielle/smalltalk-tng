#include <cstring>
#include <cstdio>
#include <cstdlib>
#include <cstdint>
#include <ctime>

#include <string>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <vector>

#include <netinet/in.h>

using namespace std;

typedef void *obj;
typedef intptr_t smi;

static inline int isSmi(obj o) { return ((intptr_t) o) & 1; }
static inline obj mkSmi(intptr_t v) { return (obj) ((v << 1) | 1); }
static inline smi unSmi(obj o) { return ((smi) o) >> 1; }

static inline intptr_t rawCount(obj o) { return ((intptr_t *) o)[0]; }
static inline int isBytes(obj o) { return rawCount(o) & 1; }
static inline unsigned slotCount(obj o) { return rawCount(o) >> 1; }
static inline unsigned bytesCount(obj o) { return rawCount(o) >> 1; }
static inline obj slotAt(obj o, unsigned i) { return ((obj *) o)[i + 2]; }
static inline void slotAtPut(obj o, unsigned i, obj v) { ((obj *) o)[i + 2] = v; }
static inline uint8_t *bvBytes(obj o) { return (uint8_t *) &(((obj *) o)[2]); }
static inline string bvString(obj o) { return string((char *) bvBytes(o), bytesCount(o)); }

static inline intptr_t rawCount(unsigned count, int isBs) {
  return (intptr_t) count << 1 | (isBs & 1);
}

static inline void _fillWith(obj o, unsigned n, obj v) {
  for (unsigned i = 0; i < n; i++) {
    slotAtPut(o, i, v);
  }
}

static int32_t nextInt(istream &f) {
  int32_t i;
  f.read((char *) &i, sizeof(int32_t));
  return ntohl(i);
}

struct VM;

typedef obj (*primitive_handler_t)(unsigned, VM &, obj);

static obj unhandledPrimitive(unsigned, VM &, obj);

struct VM {
  obj *allocBase;
  obj *allocPtr;
  obj *allocLimit;

  obj _nil;
  obj _true;
  obj _false;
  obj _Array;
  obj _Block;
  obj _Context;
  obj _Integer;

  primitive_handler_t primitiveTable[256];

  inline obj objClass(obj o) {
    return isSmi(o) ? _Integer : ((obj *) o)[1];
  }

  string className(obj c) {
    if (c == _nil) return "(nil)";
    return bvString(slotAt(c, 0));
  }

  string objClassName(obj o) {
    return className(objClass(o));
  }

  VM() {
    unsigned heapSize = 1048576000;
    allocBase = allocPtr = new obj[heapSize];
    allocLimit = &allocPtr[heapSize];
    for (int i = 0; i < 256; i++) primitiveTable[i] = unhandledPrimitive;
  }

  void gc() {
    cerr << "GC! urk" << endl;
    exit(1);
  }

  obj allocObj(unsigned nSlots, obj klass) {
  retry:
    obj result = (obj) allocPtr;
    *allocPtr++ = (obj) rawCount(nSlots, 0);
    *allocPtr++ = klass;
    allocPtr += nSlots;
    if (allocPtr >= allocLimit) {
      allocPtr = (obj *) result;
      gc();
      goto retry;
    }
    return result;
  }

  obj allocBytes(unsigned nBytes, obj klass) {
  retry:
    obj result = (obj) allocPtr;
    *allocPtr++ = (obj) rawCount(nBytes, 1);
    *allocPtr++ = klass;
    nBytes = (nBytes + sizeof(obj) - 1) & ~(sizeof(obj) - 1);
    allocPtr = (obj *) (((uint8_t *) allocPtr) + nBytes);
    if (allocPtr >= allocLimit) {
      allocPtr = (obj *) result;
      gc();
      goto retry;
    }
    return result;
  }

  obj allocBytes(string const &s, obj klass) {
    obj o = allocBytes(s.length(), klass);
    memcpy(bvBytes(o), s.c_str(), s.length());
    return o;
  }

  obj allocRawArray(unsigned n) {
    return allocObj(n, _Array);
  }

  obj allocArray(unsigned n, obj v) {
    obj o = allocRawArray(n);
    _fillWith(o, n, v);
    return o;
  }

  obj allocArray(unsigned n) {
    return allocArray(n, _nil);
  }

  void print(ostream &f, obj o) {
    if (isSmi(o)) {
      f << unSmi(o);
      return;
    }

    string className = objClassName(o);
    int isString = className.compare("String") == 0;

    if (!isString) {
      f << className;
    }

    if (isBytes(o)) {
      f << (isString ? '\"' : '{');
      ios_base::fmtflags saved(f.flags());
      char fill = f.fill('0');
      f << hex;

      for (int i = 0; i < bytesCount(o); i++) {
        char c = bvBytes(o)[i];
        if (isString) {
          switch (c) {
            case '\"': f << "\\\""; break;
            case '\n': f << "\\n"; break;
            default: f << c; break;
          }
        } else {
          f << setw(2) << ((unsigned) c & 0xff);
        }
      }

      f.flags(saved);
      f.fill(fill);

      f << (isString ? '\"' : '}');
    } else {
    }
  }

  int loadImage(string const &filename) {
    ifstream f(filename);
    vector<obj> table;

    if (!f.is_open()) return 0;
    if (nextInt(f) != 1 || !f) return 0; // wrong image version

    while (true) {
      int32_t len = nextInt(f);
      if (!f) break;
      int32_t code = nextInt(f);
      int32_t classIndex = nextInt(f);
      int32_t oopCount = nextInt(f);

      switch (code) {
        case 0:
          if (len != 5) return 0; // weird SmallInt object length
          if (oopCount != 0) return 0; // SmallInt doesn't get to have slots
          table.push_back(mkSmi(nextInt(f)));
          break;

        case 1: {
          int32_t byteCount = len - oopCount - 4;
          obj bv = allocBytes(byteCount, mkSmi(classIndex));
          f.read((char *) bvBytes(bv), byteCount);
          table.push_back(bv);
          break;
        }

        case 2: {
          obj o = allocObj(oopCount, mkSmi(classIndex));
          for (int i = 0; i < oopCount; i++) {
            slotAtPut(o, i, mkSmi(nextInt(f)));
          }
          table.push_back(o);
          break;
        }

        default:
          return 0; // unhandled code
      }
    }

    for (vector<obj>::iterator it = table.begin(); it != table.end(); ++it) {
      obj o = *it;
      if (!isSmi(o)) {
        unsigned count = 1 + (isBytes(o) ? 0 : slotCount(o));
        for (unsigned i = 0; i < count; i++) {
          (((obj *) o)[i + 1]) = table[unSmi(((obj *) o)[i + 1])];
        }
      }
    }

    _nil = table[0];
    _true = table[1];
    _false = table[2];
    _Array = table[3];
    _Block = table[4];
    _Context = table[5];
    _Integer = table[6];

    return 1;
  }

  obj searchClassMethodDictionary(obj c, obj selector) {
    obj methods = slotAt(c, 2);
    unsigned selectorLen = bytesCount(selector);
    char const *selectorBytes = (char const *) bvBytes(selector);
    for (unsigned i = 0; i < slotCount(methods); i++) {
      obj m = slotAt(methods, i);
      obj mname = slotAt(m, 0);
      // cerr << "  " << selectorBytes << " =?= " << bvString(mname) << endl;
      if ((bytesCount(mname) == selectorLen) &&
          (memcmp(bvBytes(mname), selectorBytes, selectorLen) == 0)) {
        return m;
      }
    }
    return 0;
  }

  obj buildContext(obj prevCtx, obj args, obj method) {
    unsigned tempCount = (unsigned) unSmi(slotAt(method, 4));
    unsigned maxStack = (unsigned) unSmi(slotAt(method, 3));
    obj ctx = allocObj(7, _Context);
    slotAtPut(ctx, 0, method);
    slotAtPut(ctx, 1, args);
    slotAtPut(ctx, 2, allocArray(tempCount));
    slotAtPut(ctx, 3, allocArray(maxStack));
    slotAtPut(ctx, 4, mkSmi(0));
    slotAtPut(ctx, 5, mkSmi(0));
    slotAtPut(ctx, 6, prevCtx);
    return ctx;
  }

  obj __ctx, __method, __args, __temps, __stack, __prevCtx, __receiver, __literals;
  unsigned __ip, __stackTop;
  uint8_t *__bytecode;

  void push(obj v) {
    slotAtPut(__stack, __stackTop++, v);
  }

  obj pop() {
    return slotAt(__stack, --__stackTop);
  }

  obj peek() {
    return slotAt(__stack, __stackTop - 1);
  }

  uint8_t nextByte() {
    return __bytecode[__ip++];
  }

  obj popArray(uint8_t count) {
    obj a = allocRawArray(count);
    __stackTop -= count;
    for (int i = 0; i < count; i++) {
      slotAtPut(a, i, slotAt(__stack, __stackTop + i));
    }
    return a;
  }

  void loadContext(obj ctx);

  void loadContextAndPush(obj ctx, obj v) {
    loadContext(ctx);
    push(v);
  }

  void storeRegisters() {
    slotAtPut(__ctx, 4, mkSmi(__ip));
    slotAtPut(__ctx, 5, mkSmi(__stackTop));
  }

  obj lookupMethod(obj c, obj selector) {
    while (c != _nil) {
      // cerr << "Searching in " << className(c) << " for "; print(cerr, selector); cerr << endl;
      obj m = searchClassMethodDictionary(c, selector);
      if (m != 0) return m;
      c = slotAt(c, 1);
    }
    return 0;
  }

  void sendMessage(obj c, obj newArgs, obj selector) {
    storeRegisters();
    // cerr << "Sending " << bvString(selector) << " via " << className(c) << " to ";
    // print(cerr, slotAt(newArgs, 0));
    // cerr << endl;
    obj method = lookupMethod(c, selector);
    if (method == 0) {
      cerr << "DNU ";
      print(cerr, slotAt(newArgs, 0));
      cerr << ' ';
      print(cerr, selector);
      cerr << endl;
      exit(2);
    }
    loadContext(buildContext(__ctx, newArgs, method));
  }

  void interpret() {
    while (__ctx != _nil) {
      uint8_t opcode, arg;
      opcode = nextByte();
      arg = opcode & 0xf;
      opcode = opcode >> 4;
      if (opcode == 0) {
        opcode = arg;
        arg = nextByte();
      }

      // cout << (int) opcode << ' ' << (int) arg;
      // for (int i = 0; i < __stackTop; i++) {
      //   cout << ' ';
      //   print(cout, slotAt(__stack, i));
      // }
      // cout << endl;
      // cout << flush;

      switch (opcode) {
        case 1: push(slotAt(__receiver, arg)); continue;
        case 2: push(slotAt(__args, arg)); continue;
        case 3: push(slotAt(__temps, arg)); continue;
        case 4: push(slotAt(__literals, arg)); continue;
        case 5:
          if (arg < 10) {
            push(mkSmi(arg));
          } else {
            switch (arg) {
              case 10: push(_nil); break;
              case 11: push(_true); break;
              case 12: push(_false); break;
            }
          }
          continue;
        case 6: slotAtPut(__receiver, arg, peek()); continue;
        case 7: slotAtPut(__temps, arg, peek()); continue;
        case 8: push(popArray(arg)); continue;
        case 9: {
          obj newArgs = pop();
          sendMessage(objClass(slotAt(newArgs, 0)), newArgs, slotAt(__literals, arg));
          continue;
        }
        case 10:
          switch (arg) {
            case 0: push((pop() == _nil) ? _true : _false); continue;
            case 1: push((pop() != _nil) ? _true : _false); continue;
          }
        case 11: {
          obj j = pop();
          obj i = pop();
          if (isSmi(i) && isSmi(j)) {
            switch (arg) {
              case 0: push((unSmi(i) < unSmi(j)) ? _true : _false); continue;
              case 1: push((unSmi(i) <= unSmi(j)) ? _true : _false); continue;
              case 2: push(mkSmi(unSmi(i) + unSmi(j))); continue;
            }
          } else {
            obj newArgs = allocRawArray(2);
            slotAtPut(newArgs, 0, i);
            slotAtPut(newArgs, 1, j);
            obj selector;
            switch (arg) {
              case 0: selector = allocBytes("<", _nil); break;
              case 1: selector = allocBytes("<=", _nil); break;
              case 2: selector = allocBytes("+", _nil); break;
            }
            sendMessage(objClass(i), newArgs, selector);
            continue;
          }
        }
        case 12: {
          uint8_t target = nextByte();
          obj block = allocObj(10, _Block);
          slotAtPut(block, 0, __method);
          slotAtPut(block, 1, __args);
          slotAtPut(block, 2, __temps);
          slotAtPut(block, 3, __stack);
          slotAtPut(block, 4, mkSmi(__ip));
          slotAtPut(block, 5, mkSmi(0));
          slotAtPut(block, 6, __prevCtx);
          slotAtPut(block, 7, mkSmi(arg));
          slotAtPut(block, 8, __ctx);
          slotAtPut(block, 9, mkSmi(__ip));
          push(block);
          __ip = target;
          continue;
        }
        case 13: {
          uint8_t primNumber = nextByte();
          // cerr << "  primNumber = " << (int) primNumber << endl;
          switch (primNumber) {
            case 6: {
              loadContext(pop());
              continue;
            }
            case 8: {
              obj block = pop();
              unsigned argloc = unSmi(slotAt(block, 7));
              unsigned argcount = arg - 1;
              for (unsigned i = 0; i < argcount; i++) {
                slotAtPut(slotAt(block, 2),
                          argloc + i,
                          slotAt(__stack, __stackTop - argcount + i));
              }
              __stackTop = __stackTop - argcount;
              storeRegisters();
              obj blockCtx = allocObj(10, _Block);
              slotAtPut(blockCtx, 0, slotAt(block, 0));
              slotAtPut(blockCtx, 1, slotAt(block, 1));
              slotAtPut(blockCtx, 2, slotAt(block, 2));
              slotAtPut(blockCtx, 3, allocArray(slotCount(slotAt(block, 3))));
              slotAtPut(blockCtx, 4, slotAt(block, 9));
              slotAtPut(blockCtx, 5, mkSmi(0));
              slotAtPut(blockCtx, 6, slotAt(__ctx, 6));
              slotAtPut(blockCtx, 7, slotAt(block, 7));
              slotAtPut(blockCtx, 8, slotAt(block, 8));
              slotAtPut(blockCtx, 9, slotAt(block, 9));
              loadContext(blockCtx);
              continue;
            }
            case 34: return;
            case 35: push(__ctx); continue;
            default:
              push(primitiveTable[primNumber](primNumber, *this, popArray(arg)));
              continue;
          }
        }
        case 14: push(slotAt(objClass(__receiver), arg + 5)); continue;
        case 15:
          switch (arg) {
            case 1: loadContextAndPush(__prevCtx, __receiver); continue;
            case 2: loadContextAndPush(__prevCtx, pop()); continue;
            case 3: loadContextAndPush(slotAt(slotAt(__ctx, 8), 6), pop()); continue;
            case 4: push(peek()); continue;
            case 5: pop(); continue;
            case 6: __ip = nextByte(); continue;
            case 7: {
              unsigned target = nextByte();
              if (pop() == _true) __ip = target;
              continue;
            }
            case 8: {
              unsigned target = nextByte();
              if (pop() == _false) __ip = target;
              continue;
            }
            case 11: {
              obj selector = slotAt(__literals, nextByte());
              obj newArgs = pop();
              obj definingClass = slotAt(__method, 5);
              obj super = slotAt(definingClass, 1);
              sendMessage(super, newArgs, selector);
              continue;
            }
            default:
              cerr << "Unhandled opcode 15 arg " << (int) arg << endl;
              exit(1);
          }
        default:
          cerr << "Unhandled opcode " << (int) opcode << ", " << (int) arg << endl;
          exit(1);
      }
    }
  }

  void installPrimitive(unsigned primNumber, primitive_handler_t handler) {
    primitiveTable[primNumber] = handler;
  }
};

void VM::loadContext(obj ctx) {
  __ctx = ctx;
  if (ctx != _nil) {
    __method = slotAt(ctx, 0);
    __args = slotAt(ctx, 1);
    __temps = slotAt(ctx, 2);
    __stack = slotAt(ctx, 3);
    __ip = unSmi(slotAt(ctx, 4));
    __stackTop = unSmi(slotAt(ctx, 5));
    __prevCtx = slotAt(ctx, 6);

    __receiver = slotAt(__args, 0);

    __bytecode = bvBytes(slotAt(__method, 1));
    __literals = slotAt(__method, 2);
  }
}

static obj unhandledPrimitive(unsigned primNumber, VM &vm, obj args) {
  cerr << "Primitive " << primNumber << " is unhandled" << endl;
  exit(1);
}

static obj prim_eq(unsigned primNumber, VM &vm, obj args) {
  return (slotAt(args, 0) == slotAt(args, 1)) ? vm._true : vm._false;
}

static obj prim_getClass(unsigned primNumber, VM &vm, obj args) {
  return vm.objClass(slotAt(args, 0));
}

static obj prim_objectSize(unsigned primNumber, VM &vm, obj args) {
  obj o = slotAt(args, 0);
  if (isSmi(o)) return mkSmi(0);
  if (isBytes(o)) return mkSmi(bytesCount(o));
  return mkSmi(slotCount(o));
}

static obj prim_slotAtPut(unsigned primNumber, VM &vm, obj args) {
  slotAtPut(slotAt(args, 1), unSmi(slotAt(args, 2)) - 1, slotAt(args, 0));
  return slotAt(args, 1);
}

static obj prim_mkObject(unsigned primNumber, VM &vm, obj args) {
  obj r = vm.allocObj(unSmi(slotAt(args, 1)), slotAt(args, 0));
  _fillWith(r, slotCount(r), vm._nil);
  return r;
}

static obj prim_numericQuotient(unsigned primNumber, VM &vm, obj args) {
  return mkSmi(unSmi(slotAt(args, 0)) / unSmi(slotAt(args, 1)));
}

static obj prim_numericModulo(unsigned primNumber, VM &vm, obj args) {
  return mkSmi(unSmi(slotAt(args, 0)) % unSmi(slotAt(args, 1)));
}

static obj prim_numericEq(unsigned primNumber, VM &vm, obj args) {
  return (slotAt(args, 0) == slotAt(args, 1)) ? vm._true : vm._false;
}

static obj prim_numericSub(unsigned primNumber, VM &vm, obj args) {
  return mkSmi(unSmi(slotAt(args, 0)) - unSmi(slotAt(args, 1)));
}

static obj prim_numericMul(unsigned primNumber, VM &vm, obj args) {
  // TODO overflow checks
  return mkSmi(unSmi(slotAt(args, 0)) * unSmi(slotAt(args, 1)));
}

static obj prim_mkBytes(unsigned primNumber, VM &vm, obj args) {
  return vm.allocBytes(unSmi(slotAt(args, 1)), slotAt(args, 0));
}

static obj prim_bytesRef(unsigned primNumber, VM &vm, obj args) {
  return mkSmi(bvBytes(slotAt(args, 0))[unSmi(slotAt(args, 1)) - 1]);
}

static obj prim_bytesSet(unsigned primNumber, VM &vm, obj args) {
  bvBytes(slotAt(args, 1))[unSmi(slotAt(args, 2)) - 1] = unSmi(slotAt(args, 0));
  return slotAt(args, 1);
}

static obj prim_bytesAppend(unsigned primNumber, VM &vm, obj args) {
  obj b = slotAt(args, 0);
  obj a = slotAt(args, 1);
  obj r = vm.allocBytes(bytesCount(a) + bytesCount(b), vm.objClass(a));
  memcpy(bvBytes(r), bvBytes(a), bytesCount(a));
  memcpy(bvBytes(r) + bytesCount(a), bvBytes(b), bytesCount(b));
  return r;
}

static obj prim_bytesCmp(unsigned primNumber, VM &vm, obj args) {
  obj a = slotAt(args, 0);
  obj b = slotAt(args, 1);
  int alen = bytesCount(a);
  int blen = bytesCount(a);
  int minlen = alen < blen ? alen : blen;
  int result = memcmp(bvBytes(a), bvBytes(b), minlen);
  if (result == 0) {
    result = alen - blen;
  }
  if (result < 0) return mkSmi(-1);
  if (result > 0) return mkSmi(1);
  return mkSmi(0);
}

static obj prim_slotAt(unsigned primNumber, VM &vm, obj args) {
  return slotAt(slotAt(args, 0), unSmi(slotAt(args, 1)) - 1);
}

static obj prim_extendObj(unsigned primNumber, VM &vm, obj args) {
  obj v = slotAt(args, 0);
  obj o = slotAt(args, 1);
  obj r = vm.allocObj(slotCount(o) + 1, vm.objClass(o));
  for (int i = 0; i < slotCount(o); i++) slotAtPut(r, i, slotAt(o, i));
  slotAtPut(r, slotCount(o), v);
  return r;
}

static obj prim_createWindow(unsigned primNumber, VM &vm, obj args) {
  cerr << "Creating window ";
  vm.print(cerr, slotAt(args, 0));
  cerr << endl;
  return vm.allocObj(0, slotAt(args, 0));
}

static obj prim_showWindow(unsigned primNumber, VM &vm, obj args) {
  cerr << "Show/hide window " << slotAt(args, 0) << " " << (slotAt(args, 1) == vm._true) << endl;
  return slotAt(args, 1);
}

static obj prim_setContentPane(unsigned primNumber, VM &vm, obj args) {
  cerr << "Setting content pane " << slotAt(args, 0) << endl;
  return slotAt(args, 0);
}

static obj prim_setWindowSize(unsigned primNumber, VM &vm, obj args) {
  cout << slotAt(args, 0) << "prim_setWindowSize:"
       << " height " << unSmi(slotAt(args, 1))
       << " width " << unSmi(slotAt(args, 2)) << endl;
  return slotAt(args, 0);
}

static obj prim_setWindowMenu(unsigned primNumber, VM &vm, obj args) {
  cout << slotAt(args, 0) << "prim_setWindowMenu" << endl;
  return slotAt(args, 0);
}

static obj prim_setWindowTitle(unsigned primNumber, VM &vm, obj args) {
  cout << slotAt(args, 0) << "prim_setWindowTitle: " << bvString(slotAt(args, 1)) << endl;
  return slotAt(args, 0);
}

static obj prim_repaintWindow(unsigned primNumber, VM &vm, obj args) {
  cout << slotAt(args, 0) << "prim_repaintWindow" << endl;
  return slotAt(args, 0);
}

static obj prim_createButton(unsigned primNumber, VM &vm, obj args) {
  cerr << "Creating button ";
  vm.print(cerr, slotAt(args, 0));
  cerr << " with label " << bvString(slotAt(args, 1)) << endl;
  return vm.allocObj(0, slotAt(args, 0));
}

static obj prim_createTextArea(unsigned primNumber, VM &vm, obj args) {
  cerr << "Creating textarea ";
  vm.print(cerr, slotAt(args, 0));
  cerr << endl;
  return vm.allocObj(0, slotAt(args, 0));
}

static obj prim_updateTextArea(unsigned primNumber, VM &vm, obj args) {
  cout << slotAt(args, 0) << "prim_updateTextArea: " << bvString(slotAt(args, 1)) << endl;
  return slotAt(args, 1);
}

static obj prim_createMenu(unsigned primNumber, VM &vm, obj args) {
  cerr << "Creating menu ";
  vm.print(cerr, slotAt(args, 0));
  cerr << " with title " << bvString(slotAt(args, 1)) << endl;
  return vm.allocObj(0, slotAt(args, 0));
}

static obj prim_createMenuItem(unsigned primNumber, VM &vm, obj args) {
  cerr << "Creating menu item ";
  vm.print(cerr, slotAt(args, 0));
  cerr << " with title " << bvString(slotAt(args, 1)) << endl;
  return slotAt(args, 0);
}

static obj prim_closeWindowHandler(unsigned primNumber, VM &vm, obj args) {
  cerr << "Ignoring prim_closeWindowHandler" << endl;
  return slotAt(args, 0);
}

static struct timespec ts_start;
static obj prim_getMilliseconds(unsigned primNumber, VM &vm, obj args) {
  struct timespec ts_end;
  clock_gettime(CLOCK_MONOTONIC, &ts_end);
  intptr_t delta = (ts_end.tv_sec - ts_start.tv_sec) * 1000;
  delta += (ts_end.tv_nsec / 1000000) - (ts_start.tv_nsec / 1000000);
  return mkSmi(delta);
}

int main(int argc, char *argv[]) {
  VM vm;

  clock_gettime(CLOCK_MONOTONIC, &ts_start);

  vm.installPrimitive(1, prim_eq);
  vm.installPrimitive(2, prim_getClass);
  vm.installPrimitive(4, prim_objectSize);
  vm.installPrimitive(5, prim_slotAtPut);
  vm.installPrimitive(7, prim_mkObject);
  vm.installPrimitive(11, prim_numericQuotient);
  vm.installPrimitive(12, prim_numericModulo);
  vm.installPrimitive(14, prim_numericEq);
  vm.installPrimitive(15, prim_numericMul);
  vm.installPrimitive(16, prim_numericSub);
  vm.installPrimitive(20, prim_mkBytes);
  vm.installPrimitive(21, prim_bytesRef);
  vm.installPrimitive(22, prim_bytesSet);
  vm.installPrimitive(24, prim_bytesAppend);
  vm.installPrimitive(26, prim_bytesCmp);
  vm.installPrimitive(30, prim_slotAt);
  vm.installPrimitive(31, prim_extendObj);
  vm.installPrimitive(60, prim_createWindow);
  vm.installPrimitive(61, prim_showWindow);
  vm.installPrimitive(62, prim_setContentPane);
  vm.installPrimitive(63, prim_setWindowSize);
  vm.installPrimitive(64, prim_setWindowMenu);
  vm.installPrimitive(65, prim_setWindowTitle);
  vm.installPrimitive(66, prim_repaintWindow);
  vm.installPrimitive(71, prim_createButton);
  vm.installPrimitive(73, prim_createTextArea);
  vm.installPrimitive(82, prim_updateTextArea);
  vm.installPrimitive(90, prim_createMenu);
  vm.installPrimitive(91, prim_createMenuItem);
  vm.installPrimitive(118, prim_closeWindowHandler);
  vm.installPrimitive(119, prim_getMilliseconds);

  if (!vm.loadImage("SmallWorld/src/image")) {
    cerr << "Could not load image" << endl;
    return 1;
  }

  cout << "Loaded!" << endl;

  {
    string bootCode = "[SmallWorld startUp. \
Transcript show: 0 tinyBenchmarks.] value";
    // "SmallWorld startUp";
    obj _String = vm.objClass(slotAt(vm.objClass(vm._true), 0)); // class String
    obj code = vm.allocBytes(bootCode, _String);
    obj selector = vm.allocBytes("doIt", _String);
    obj args = vm.allocRawArray(1);
    slotAtPut(args, 0, code);
    obj doIt = vm.searchClassMethodDictionary(vm.objClass(code), selector);
    obj ctx = vm.buildContext(vm._nil, args, doIt);
    vm.loadContext(ctx);
    vm.interpret();
    cout << "Boot complete" << endl;
    cout << "Used: " << vm.allocPtr - vm.allocBase << endl;
    cout << flush;
  }

  return 0;
}
