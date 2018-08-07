#include <cstring>
#include <cstdio>
#include <cstdlib>
#include <cstdint>
#include <ctime>
#include <cassert>

#include <string>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <vector>
#include <deque>

#include <netinet/in.h>

using namespace std;

typedef void *obj;
typedef intptr_t smi;

static inline int isSmi(obj o) { return ((intptr_t) o) & 1; }
static inline obj mkSmi(intptr_t v) { return (obj) ((v << 1) | 1); }
static inline smi unSmi(obj o) { return ((smi) o) >> 1; }

static inline intptr_t objHeader(obj o) { return ((intptr_t *) o)[0]; }
static inline int isUnused(obj o) { return objHeader(o) == 0; }
static inline int isMarked(obj o) { return objHeader(o) & 1; }
static inline int isBytes(obj o) { return objHeader(o) & 2; }
static inline unsigned slotCount(obj o) { return (objHeader(o) >> 2) - 1; }
static inline unsigned bytesCount(obj o) { return (objHeader(o) >> 2) - 1; }
static inline void mark(obj o) { ((intptr_t *) o)[0] |= 1; }
static inline void unmark(obj o) { ((intptr_t *) o)[0] &= ~((intptr_t) 1); }

static inline obj *classAddr(obj o) { return &(((obj *) o)[1]); }
static inline obj *slotAddr(obj o, unsigned i) { return &(((obj *) o)[i + 2]); }
static inline obj slotAt(obj o, unsigned i) { return *slotAddr(o, i); }
static inline void slotAtPut(obj o, unsigned i, obj v) { *slotAddr(o, i) = v; }
static inline uint8_t *bvBytes(obj o) { return (uint8_t *) &(((obj *) o)[2]); }
static inline string bvString(obj o) { return string((char *) bvBytes(o), bytesCount(o)); }

static inline intptr_t objHeader(unsigned count, int isBs) {
  return (((intptr_t) count + 1) << 2) | (isBs ? 2 : 0);
}

static inline intptr_t freeHeader(unsigned blockWords) {
  assert(blockWords >= 2);
  return objHeader(blockWords - 2, 0);
}

static inline unsigned roundUpAllocation(unsigned nBytes) {
  return (nBytes + sizeof(obj) - 1) & ~(sizeof(obj) - 1);
}

static inline void _fillWith(obj o, unsigned n, obj v) {
  fill(slotAddr(o, 0), slotAddr(o, n), v);
}

static int32_t nextInt(istream &f) {
  int32_t i;
  f.read((char *) &i, sizeof(int32_t));
  return ntohl(i);
}

static long operator-(struct timespec const &a, struct timespec const &b) {
  long delta = (a.tv_sec - b.tv_sec) * 1000000;
  return delta + ((a.tv_nsec - b.tv_nsec) / 1000);
}

struct VM;

struct Root {
  VM *vm;
  obj *ptr;
  Root *next;

  Root(VM *vm, obj *ptr);
  ~Root();
};

typedef obj (*primitive_handler_t)(unsigned, VM &, obj);

static obj unhandledPrimitive(unsigned, VM &, obj);

struct VM {
  int gcEnabled;
  vector<obj> heap;
  obj *allocPtr;
  obj *allocLimit;

  obj _nil;
  obj _true;
  obj _false;
  obj _Array;
  obj _Block;
  obj _Context;
  obj _Integer;

  obj _activeCtx;
  obj _i;
  obj _j;

  obj __method;
  obj __args;
  obj __temps;
  obj __stack;
  obj __prevCtx;
  obj __receiver;
  obj __literals;
  unsigned __ip;
  unsigned __stackTop;
  uint8_t *__bytecode;

  Root *rootStack;

  primitive_handler_t primitiveTable[256];

  static unsigned const methodCacheCount = 2039;
  static unsigned const methodCacheLimit = 3 * methodCacheCount;
  obj methodCache[methodCacheLimit];

  struct timespec mutatorStart;

  inline obj objClass(obj o) {
    return isSmi(o) ? _Integer : *classAddr(o);
  }

  string className(obj c) {
    if (c == _nil) return "(nil)";
    return bvString(slotAt(c, 0));
  }

  string objClassName(obj o) {
    return className(objClass(o));
  }

  VM() {
    unsigned heapSize = 8 * 1048576;
    heap.resize(heapSize, 0);
    cerr << "Heap bounds: " << &heap[0] << "-" << &heap[heapSize] << endl;
    allocPtr = &heap[0];
    *allocPtr = (obj) freeHeader(heapSize);
    allocLimit = allocPtr + heapSize;
    gcEnabled = 0; // during image loading
    fill(&primitiveTable[0], &primitiveTable[256], unhandledPrimitive);
    rootStack = 0;
    new Root(this, &_nil);
    new Root(this, &_true);
    new Root(this, &_false);
    new Root(this, &_Array);
    new Root(this, &_Block);
    new Root(this, &_Context);
    new Root(this, &_Integer);
    new Root(this, &_activeCtx);
    new Root(this, &_i);
    new Root(this, &_j);
  }

  void scheduleMark(deque<obj *> &q, obj *p) {
    // cerr << "  scheduling mark of " << p << " (" << *p << ")" << endl;
    q.push_back(p);
  }

  void gc() {
    struct timespec gcStart;
    clock_gettime(CLOCK_MONOTONIC, &gcStart);

    unsigned liveCount = 0;
    unsigned scanCount = 0;
    unsigned byteCount = 0;

    deque<obj *> q;
    // cerr << "VM this = " << this << "-" << (this + 1) << endl;
    for (Root *r = rootStack; r != 0; r = r->next) {
      scheduleMark(q, r->ptr);
    }
    while (!q.empty()) {
      obj *op = q.front();
      q.pop_front();
      scanCount++;
      // cerr << "Scan of " << op;
      obj o = *op;
      // cerr << " (" << o << ")" << endl;
      if (isSmi(o)) continue;
      if (isMarked(o)) continue;
      // cerr << "Marking " << o << " which is "; print(cerr, o); cerr << endl;
      mark(o);
      liveCount++;
      scheduleMark(q, classAddr(o));
      if (!isBytes(o)) {
        for (int i = 0; i < slotCount(o); i++) {
          scheduleMark(q, slotAddr(o, i));
        }
      } else {
        byteCount += bytesCount(o);
      }
    }
    // {
    //   obj *p = &heap[0];
    //   cerr << "-------------- allocBase == " << &heap[0] << endl;
    //   while (p < allocLimit) {
    //     obj o = (obj) p;
    //     cerr << o << (isUnused(o) ? " (unused)" :
    //                   isMarked(o) ? " (marked) " :
    //                   " (      ) ");
    //     if (isUnused(o)) {
    //       p++;
    //     } else {
    //       if (isBytes(o)) {
    //         cerr << bytesCount(o) << " bytes";
    //       } else {
    //         cerr << slotCount(o) << " slots";
    //       }
    //       p += objSize(p);
    //       if (!isMarked(o)) {
    //         // TODO remove
    //         uint8_t *stomple = ((uint8_t *) o) + sizeof(obj);
    //         unsigned stompcount = ((uint8_t *) p) - stomple;
    //         cerr << " (stomping on " << stompcount << " bytes)";
    //         memset(stomple, 0x5a, stompcount);
    //       }
    //     }
    //     cerr << endl;
    //   }
    //   cerr << "-------------- end of sweep" << endl;
    // }
    allocPtr = &heap[0];

    struct timespec gcEnd;
    clock_gettime(CLOCK_MONOTONIC, &gcEnd);
    long gc_us = gcEnd - gcStart;
    long mutator_us = gcStart - mutatorStart;
    double mutator_percentage = (100.0 * mutator_us) / (mutator_us + gc_us);
    cerr << " complete in " << gc_us << "μs."
         << " Mutator ran for " << mutator_us << "μs (" << mutator_percentage << "%)."
         << " " << liveCount << " live objects; "
         << scanCount << " words scanned, "
         << byteCount << " bytes skipped."
         << endl;
    mutatorStart = gcEnd;
  }

  unsigned objSize(obj o) {
    if (isBytes(o)) {
      return (roundUpAllocation(bytesCount(o)) / sizeof(obj)) + 2;
    } else {
      return slotCount(o) + 2;
    }
  }

  unsigned chunkSizeAt(obj *p) {
    obj o = (obj) p;
    return
      isUnused(o) ? 1 :
      isMarked(o) ? 0 :
      objSize(o);
  }

  void findFree(unsigned nObj, obj *&candidate, unsigned &candidateSize) {
    candidate = allocPtr;
    // cerr << "Hunting for " << nObj << " words at " << candidate << endl;

  restart:
    obj *scan = candidate;
    candidateSize = 0;
    while ((candidateSize < nObj) && (scan < allocLimit)) {
      unsigned nextChunkSize = chunkSizeAt(scan);
      if (nextChunkSize == 0) { // marked block - not at allocLimit by loop condition
        unmark((obj) scan);
        candidate = scan + objSize(scan);
        goto restart;
      }
      candidateSize += nextChunkSize;
      // cerr << "  after nextChunkSize==" << nextChunkSize
      //      << " at " << scan << "-" << (scan + nextChunkSize)
      //      << ", candidateSize==" << candidateSize << endl;
      scan += nextChunkSize;
    }

    // cerr << "  Final candidateSize: " << candidateSize << endl;
    if (candidateSize < nObj) {
      candidate = 0;
    }
  }

  // void growHeap() {
  //   // BOGUS: resizing likely moves the base pointer, forcing rewrite
  //   // of all live pointers. We could in principle do this, but it'd
  //   // be more convenient to choose our moment so that, say, we have a
  //   // guarantee that only the pointers in the root set are live.

  //   unsigned oldSize = heap.size();
  //   unsigned newSize = oldSize * 2;
  //   cerr << "Growing heap from " << oldSize << " words to " << newSize << " words" << endl;
  //   heap.resize(newSize, 0);
  //   cerr << "Heap bounds: " << &heap[0] << "-" << &heap[newSize] << endl;
  //   allocPtr = &heap[oldSize];
  //   *allocPtr = (obj) freeHeader(newSize - oldSize);
  //   allocLimit = allocPtr + newSize;
  // }

  obj *allocBlock(unsigned nObj) {
    obj *candidate = 0;
    unsigned candidateSize = 0;
    findFree(nObj, candidate, candidateSize);

    if (candidate == 0) {
      if (gcEnabled) {
        cerr << "gc()...";
        storeRegisters();
        gc();
        findFree(nObj, candidate, candidateSize);
      }
      if (candidate == 0) {
        cerr << "Out of memory, heap resizing not yet implemented" << endl;
        exit(1);
      }
      // while (candidate == 0) {
      //   growHeap();
      //   findFree(nObj, candidate, candidateSize);
      // }
    }

    // cerr << "  candidate==" << candidate
    //      << ", candidateSize==" << candidateSize
    //      << ", nObj+2==" << (nObj + 2)
    //      << ", possibleLeftoverInclHeader==" << (candidateSize - nObj)
    //      << endl;
    assert(candidateSize >= nObj);

    if (candidateSize > (nObj + 2)) {
      allocPtr = candidate + nObj;
      *allocPtr = (obj) freeHeader(candidateSize - nObj);
    } else {
      fill(candidate + nObj, candidate + candidateSize, (obj) 0);
      allocPtr = candidate + candidateSize;
    }

    // if (allocPtr == allocLimit) {
    //   cerr << "Next allocation will GC!" << endl;
    // }

    return candidate;
  }

  obj allocObj(unsigned nSlots, obj klass) {
    obj *p = allocBlock(nSlots + 2);
    obj result = (obj) p;
    *p++ = (obj) objHeader(nSlots, 0);
    *p++ = klass;
    fill(p, p + nSlots, _nil);
    return result;
  }

  obj allocBytes(unsigned nBytes, obj klass) {
    obj *p = allocBlock((roundUpAllocation(nBytes) / sizeof(obj)) + 2);
    obj result = (obj) p;
    *p++ = (obj) objHeader(nBytes, 1);
    *p++ = klass;
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

    cerr << "Loading: starting fixup" << endl;

    for (vector<obj>::iterator it = table.begin(); it != table.end(); ++it) {
      obj o = *it;
      if (!isSmi(o)) {
        *classAddr(o) = table[unSmi(*classAddr(o))];
        if (!isBytes(o)) {
          for (unsigned i = 0; i < slotCount(o); i++) {
            *slotAddr(o, i) = table[unSmi(*slotAddr(o, i))];
          }
        }
      }
    }

    cerr << "Loading: fixup complete" << endl;

    _i = _j = _nil = table[0];
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

  void callMethod_i_j() {
    unsigned tempCount = (unsigned) unSmi(slotAt(_j, 4));
    unsigned maxStack = (unsigned) unSmi(slotAt(_j, 3));
    {
      obj ctx = allocObj(7, _Context);
      slotAtPut(ctx, 0, _j); // method
      slotAtPut(ctx, 1, _i); // args
      _i = ctx;
    }
    slotAtPut(_i, 2, allocArray(tempCount));
    slotAtPut(_i, 3, allocArray(maxStack));
    slotAtPut(_i, 4, mkSmi(0));
    slotAtPut(_i, 5, mkSmi(0));
    slotAtPut(_i, 6, _activeCtx);
    loadContext(_i);
    _i = _j = _nil;
  }

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

  void popArray_i(uint8_t count) {
    _i = allocRawArray(count);
    __stackTop -= count;
    copy(slotAddr(__stack, __stackTop), slotAddr(__stack, __stackTop + count), slotAddr(_i, 0));
  }

  void loadContext(obj ctx);

  void loadContextAndPush(obj ctx, obj v) {
    loadContext(ctx);
    push(v);
  }

  void storeRegisters() {
    slotAtPut(_activeCtx, 4, mkSmi(__ip));
    slotAtPut(_activeCtx, 5, mkSmi(__stackTop));
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

  void sendMessage_i_j(obj c) {
    storeRegisters();
    // cerr << "Sending " << bvString(_j) << " via " << className(c) << " to ";
    // print(cerr, slotAt(_i, 0));
    // cerr << endl;
    unsigned probe = ((((intptr_t) c) * 5 + ((intptr_t) _j)) % methodCacheCount) * 3;
    obj method;
    if ((methodCache[probe] == c) && (methodCache[probe + 1] == _j)) {
      method = methodCache[probe + 2];
    } else {
      method = lookupMethod(c, _j);
      methodCache[probe] = c;
      methodCache[probe + 1] = _j;
      methodCache[probe + 2] = method;
    }
    if (method == 0) {
      cerr << "DNU ";
      print(cerr, slotAt(_i, 0));
      cerr << ' ';
      print(cerr, _j);
      cerr << endl;
      exit(2);
    }
    _j = method;
    callMethod_i_j();
  }

  void interpret() {
    while (_activeCtx != _nil) {
      uint8_t opcode, arg;
      opcode = nextByte();
      arg = opcode & 0xf;
      opcode = opcode >> 4;
      if (opcode == 0) {
        opcode = arg;
        arg = nextByte();
      }

      // cerr << (int) opcode << ' ' << (int) arg;
      // for (int i = 0; i < __stackTop; i++) {
      //   cerr << ' ';
      //   print(cerr, slotAt(__stack, i));
      // }
      // cerr << endl;
      // cerr << flush;

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
        case 8: {
          popArray_i(arg);
          push(_i);
          _i = _nil;
          continue;
        }
        case 9: {
          _i = pop();
          _j = slotAt(__literals, arg);
          sendMessage_i_j(objClass(slotAt(_i, 0)));
          continue;
        }
        case 10:
          switch (arg) {
            case 0: push((pop() == _nil) ? _true : _false); continue;
            case 1: push((pop() != _nil) ? _true : _false); continue;
          }
        case 11: {
          _j = pop();
          _i = pop();
          if (isSmi(_i) && isSmi(_j)) {
            switch (arg) {
              case 0: push((unSmi(_i) < unSmi(_j)) ? _true : _false); break;
              case 1: push((unSmi(_i) <= unSmi(_j)) ? _true : _false); break;
              case 2: push(mkSmi(unSmi(_i) + unSmi(_j))); break;
            }
            _i = _j = _nil;
          } else {
            {
              obj newArgs = allocRawArray(2);
              slotAtPut(newArgs, 0, _i);
              slotAtPut(newArgs, 1, _j);
              _i = newArgs;
            }
            switch (arg) {
              case 0: _j = allocBytes("<", _nil); break;
              case 1: _j = allocBytes("<=", _nil); break;
              case 2: _j = allocBytes("+", _nil); break;
            }
            sendMessage_i_j(objClass(slotAt(_i, 0)));
          }
          continue;
        }
        case 12: {
          uint8_t target = nextByte();
          _i = allocObj(10, _Block);
          slotAtPut(_i, 0, __method);
          slotAtPut(_i, 1, __args);
          slotAtPut(_i, 2, __temps);
          slotAtPut(_i, 3, __stack);
          slotAtPut(_i, 4, mkSmi(__ip));
          slotAtPut(_i, 5, mkSmi(0));
          slotAtPut(_i, 6, __prevCtx);
          slotAtPut(_i, 7, mkSmi(arg));
          slotAtPut(_i, 8, _activeCtx);
          slotAtPut(_i, 9, mkSmi(__ip));
          push(_i);
          _i = _nil;
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
              _j = pop();
              unsigned argloc = unSmi(slotAt(_j, 7));
              unsigned argcount = arg - 1;
              copy(slotAddr(__stack, __stackTop - argcount),
                   slotAddr(__stack, __stackTop),
                   slotAddr(slotAt(_j, 2), argloc));
              __stackTop = __stackTop - argcount;
              storeRegisters();
              _i = allocObj(10, _Block);
              slotAtPut(_i, 0, slotAt(_j, 0));
              slotAtPut(_i, 1, slotAt(_j, 1));
              slotAtPut(_i, 2, slotAt(_j, 2));
              slotAtPut(_i, 3, allocArray(slotCount(slotAt(_j, 3))));
              slotAtPut(_i, 4, slotAt(_j, 9));
              slotAtPut(_i, 5, mkSmi(0));
              slotAtPut(_i, 6, slotAt(_activeCtx, 6));
              slotAtPut(_i, 7, slotAt(_j, 7));
              slotAtPut(_i, 8, slotAt(_j, 8));
              slotAtPut(_i, 9, slotAt(_j, 9));
              loadContext(_i);
              _i = _j = _nil;
              continue;
            }
            case 34: return;
            case 35: push(_activeCtx); continue;
            default:
              popArray_i(arg);
              push(primitiveTable[primNumber](primNumber, *this, _i));
              _i = _nil;
              continue;
          }
        }
        case 14: push(slotAt(objClass(__receiver), arg + 5)); continue;
        case 15:
          switch (arg) {
            case 1: loadContextAndPush(__prevCtx, __receiver); continue;
            case 2: loadContextAndPush(__prevCtx, pop()); continue;
            case 3: loadContextAndPush(slotAt(slotAt(_activeCtx, 8), 6), pop()); continue;
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
              _i = pop();
              _j = slotAt(__literals, nextByte());
              sendMessage_i_j(slotAt(slotAt(__method, 5), 1));
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
  _activeCtx = ctx;
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

Root::Root(VM *vm, obj *ptr) {
  this->vm = vm;
  this->ptr = ptr;
  this->next = vm->rootStack;
  vm->rootStack = this;
}

Root::~Root() {
  if (vm->rootStack != this) {
    cerr << "Root stack corruption" << endl;
    exit(1);
  }
  vm->rootStack = next;
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
  int blen = bytesCount(b);
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
  copy(slotAddr(o, 0), slotAddr(o, slotCount(o)), slotAddr(r, 0));
  slotAtPut(r, slotCount(o), v);
  return r;
}

static obj prim_createWindow(unsigned primNumber, VM &vm, obj args) {
  cout << "Creating window ";
  vm.print(cout, slotAt(args, 0));
  cout << endl;
  return vm.allocObj(0, slotAt(args, 0));
}

static obj prim_showWindow(unsigned primNumber, VM &vm, obj args) {
  cout << "Show/hide window " << slotAt(args, 0) << " " << (slotAt(args, 1) == vm._true) << endl;
  return slotAt(args, 1);
}

static obj prim_setContentPane(unsigned primNumber, VM &vm, obj args) {
  cout << "Setting content pane " << slotAt(args, 0) << endl;
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
  cout << "Creating button ";
  vm.print(cout, slotAt(args, 0));
  cout << " with label " << bvString(slotAt(args, 1)) << endl;
  return vm.allocObj(0, slotAt(args, 0));
}

static obj prim_createTextArea(unsigned primNumber, VM &vm, obj args) {
  cout << "Creating textarea ";
  vm.print(cout, slotAt(args, 0));
  cout << endl;
  return vm.allocObj(0, slotAt(args, 0));
}

static obj prim_updateTextArea(unsigned primNumber, VM &vm, obj args) {
  cout << slotAt(args, 0) << "prim_updateTextArea: " << bvString(slotAt(args, 1)) << endl;
  return slotAt(args, 1);
}

static obj prim_createMenu(unsigned primNumber, VM &vm, obj args) {
  cout << "Creating menu ";
  vm.print(cout, slotAt(args, 0));
  cout << " with title " << bvString(slotAt(args, 1)) << endl;
  return vm.allocObj(0, slotAt(args, 0));
}

static obj prim_createMenuItem(unsigned primNumber, VM &vm, obj args) {
  cout << "Creating menu item ";
  vm.print(cout, slotAt(args, 0));
  cout << " with title " << bvString(slotAt(args, 1)) << endl;
  return slotAt(args, 0);
}

static obj prim_closeWindowHandler(unsigned primNumber, VM &vm, obj args) {
  cout << "Ignoring prim_closeWindowHandler" << endl;
  return slotAt(args, 0);
}

static struct timespec ts_start;
static obj prim_getMilliseconds(unsigned primNumber, VM &vm, obj args) {
  struct timespec ts_end;
  clock_gettime(CLOCK_MONOTONIC, &ts_end);
  return mkSmi((ts_end - ts_start) / 1000);
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

  cerr << "Loaded!" << endl;

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
    vm._activeCtx = vm._nil;
    vm._i = args;
    vm._j = doIt;
    vm.callMethod_i_j();
    vm.gcEnabled = 1;
    clock_gettime(CLOCK_MONOTONIC, &vm.mutatorStart);
    cerr << "gcEnabled --> 1" << endl;
    vm.interpret();
    cerr << "Boot complete" << endl;
  }

  return 0;
}
